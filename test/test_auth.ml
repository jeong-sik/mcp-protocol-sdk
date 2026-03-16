(** Tests for OAuth 2.1 authentication.

    Covers:
    - Auth type serialization (token_info, metadata, token_response)
    - Auth middleware (bearer extraction, 401 responses, scope checking)
    - OAuth client (PKCE generation, credential store, URL building)
    - Integration (server + client with bearer tokens) *)

open Mcp_protocol

(* ── auth type tests ────────────────────────── *)

let test_protected_resource_metadata_roundtrip () =
  let metadata : Auth.protected_resource_metadata = {
    resource = "https://mcp.example.com";
    authorization_servers = ["https://auth.example.com"];
    scopes_supported = Some ["read"; "write"];
    bearer_methods_supported = Some ["header"];
  } in
  let json = Auth.protected_resource_metadata_to_yojson metadata in
  let decoded = Auth.protected_resource_metadata_of_yojson json in
  match decoded with
  | Ok m ->
    Alcotest.(check string) "resource" "https://mcp.example.com" m.resource;
    Alcotest.(check int) "auth servers count" 1 (List.length m.authorization_servers)
  | Error e -> Alcotest.fail (Printf.sprintf "Decode failed: %s" e)

let test_oauth_token_response_roundtrip () =
  let resp : Auth.oauth_token_response = {
    access_token = "test-token-123";
    token_type = "Bearer";
    expires_in = Some 3600;
    refresh_token = Some "refresh-456";
    scope = Some "read write";
  } in
  let json = Auth.oauth_token_response_to_yojson resp in
  let decoded = Auth.oauth_token_response_of_yojson json in
  match decoded with
  | Ok r ->
    Alcotest.(check string) "access_token" "test-token-123" r.access_token;
    Alcotest.(check string) "token_type" "Bearer" r.token_type;
    Alcotest.(check (option int)) "expires_in" (Some 3600) r.expires_in;
    Alcotest.(check (option string)) "refresh_token" (Some "refresh-456") r.refresh_token
  | Error e -> Alcotest.fail (Printf.sprintf "Decode failed: %s" e)

let test_stored_credentials_roundtrip () =
  let creds : Auth.stored_credentials = {
    client_id = "my-client";
    token_response = Some {
      access_token = "tok"; token_type = "Bearer";
      expires_in = None; refresh_token = None; scope = None;
    };
    granted_scopes = ["read"];
  } in
  let json = Auth.stored_credentials_to_yojson creds in
  let decoded = Auth.stored_credentials_of_yojson json in
  match decoded with
  | Ok c ->
    Alcotest.(check string) "client_id" "my-client" c.client_id;
    Alcotest.(check (list string)) "scopes" ["read"] c.granted_scopes
  | Error e -> Alcotest.fail (Printf.sprintf "Decode failed: %s" e)

let test_oauth_error_roundtrip () =
  let err : Auth.oauth_error = {
    error = "invalid_token";
    error_description = Some "Token expired";
    error_uri = None;
  } in
  let json = Auth.oauth_error_to_yojson err in
  let decoded = Auth.oauth_error_of_yojson json in
  match decoded with
  | Ok e ->
    Alcotest.(check string) "error" "invalid_token" e.error;
    Alcotest.(check (option string)) "description" (Some "Token expired") e.error_description
  | Error e -> Alcotest.fail (Printf.sprintf "Decode failed: %s" e)

(* ── middleware tests ───────────────────────── *)

let make_request ?(headers=[]) () =
  let h = Http.Header.of_list headers in
  Http.Request.make ~meth:`GET ~headers:h "/"

let always_valid_verifier token _req =
  if token = "valid-token" then
    Ok Auth.{ scopes = ["read"; "write"]; expires_at = Unix.gettimeofday () +. 3600.0;
              user_id = Some "test-user"; extra = `Null }
  else
    Error "Invalid token"

let test_middleware_missing_token () =
  let config = Mcp_protocol_http.Auth_middleware.create
    ~verifier:always_valid_verifier
    ~resource_server:"https://mcp.example.com"
    ~authorization_servers:["https://auth.example.com"]
    () in
  let request = make_request () in
  match Mcp_protocol_http.Auth_middleware.check_auth ~required:true config request with
  | Error _ -> () (* Expected: 401 response *)
  | Ok _ -> Alcotest.fail "Expected 401 for missing token"

let test_middleware_valid_token () =
  let config = Mcp_protocol_http.Auth_middleware.create
    ~verifier:always_valid_verifier
    ~resource_server:"https://mcp.example.com"
    ~authorization_servers:["https://auth.example.com"]
    () in
  let request = make_request ~headers:[("Authorization", "Bearer valid-token")] () in
  match Mcp_protocol_http.Auth_middleware.check_auth config request with
  | Ok (Some info) ->
    Alcotest.(check (option string)) "user_id" (Some "test-user") info.user_id;
    Alcotest.(check (list string)) "scopes" ["read"; "write"] info.scopes
  | Ok None -> Alcotest.fail "Expected Some token_info"
  | Error _ -> Alcotest.fail "Expected Ok for valid token"

let test_middleware_invalid_token () =
  let config = Mcp_protocol_http.Auth_middleware.create
    ~verifier:always_valid_verifier
    ~resource_server:"https://mcp.example.com"
    ~authorization_servers:["https://auth.example.com"]
    () in
  let request = make_request ~headers:[("Authorization", "Bearer bad-token")] () in
  match Mcp_protocol_http.Auth_middleware.check_auth config request with
  | Error _ -> () (* Expected: 401 *)
  | Ok _ -> Alcotest.fail "Expected 401 for invalid token"

let test_middleware_optional_no_token () =
  let config = Mcp_protocol_http.Auth_middleware.create
    ~verifier:always_valid_verifier
    ~resource_server:"https://mcp.example.com"
    ~authorization_servers:["https://auth.example.com"]
    () in
  let request = make_request () in
  match Mcp_protocol_http.Auth_middleware.check_auth ~required:false config request with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "Expected None for missing optional token"
  | Error _ -> Alcotest.fail "Expected Ok None for optional auth"

let test_middleware_scope_check () =
  let config = Mcp_protocol_http.Auth_middleware.create
    ~verifier:always_valid_verifier
    ~required_scopes:["admin"]
    ~resource_server:"https://mcp.example.com"
    ~authorization_servers:["https://auth.example.com"]
    () in
  let request = make_request ~headers:[("Authorization", "Bearer valid-token")] () in
  (* valid-token has ["read"; "write"] but config requires ["admin"] *)
  match Mcp_protocol_http.Auth_middleware.check_auth config request with
  | Error _ -> () (* Expected: insufficient_scope *)
  | Ok _ -> Alcotest.fail "Expected 401 for insufficient scope"

let test_bearer_extraction () =
  let req1 = make_request ~headers:[("Authorization", "Bearer my-token")] () in
  let req2 = make_request ~headers:[("Authorization", "Basic abc123")] () in
  let req3 = make_request () in
  Alcotest.(check (option string)) "bearer extracted"
    (Some "my-token")
    (Mcp_protocol_http.Auth_middleware.extract_bearer_token req1);
  Alcotest.(check (option string)) "basic ignored"
    None
    (Mcp_protocol_http.Auth_middleware.extract_bearer_token req2);
  Alcotest.(check (option string)) "missing"
    None
    (Mcp_protocol_http.Auth_middleware.extract_bearer_token req3)

let test_resource_metadata () =
  let config = Mcp_protocol_http.Auth_middleware.create
    ~verifier:always_valid_verifier
    ~required_scopes:["read"]
    ~resource_server:"https://mcp.example.com"
    ~authorization_servers:["https://auth.example.com"]
    () in
  let metadata = Mcp_protocol_http.Auth_middleware.resource_metadata config in
  Alcotest.(check string) "resource" "https://mcp.example.com" metadata.resource;
  Alcotest.(check (list string)) "auth servers"
    ["https://auth.example.com"] metadata.authorization_servers;
  Alcotest.(check (option (list string))) "scopes" (Some ["read"]) metadata.scopes_supported

(* ── OAuth client tests ─────────────────────── *)

let test_pkce_generation () =
  let verifier, challenge = Mcp_protocol_http.Oauth_client.generate_pkce () in
  Alcotest.(check bool) "verifier not empty" true (String.length verifier > 0);
  Alcotest.(check bool) "challenge not empty" true (String.length challenge > 0);
  Alcotest.(check bool) "verifier != challenge" true (verifier <> challenge);
  (* Generate another pair and check they're different *)
  let v2, c2 = Mcp_protocol_http.Oauth_client.generate_pkce () in
  Alcotest.(check bool) "different verifiers" true (verifier <> v2);
  Alcotest.(check bool) "different challenges" true (challenge <> c2)

let test_in_memory_store () =
  let store = Mcp_protocol_http.Oauth_client.in_memory_store () in
  Alcotest.(check bool) "initially empty" true (Option.is_none (store.load ()));
  let creds : Auth.stored_credentials = {
    client_id = "test-client";
    token_response = None;
    granted_scopes = [];
  } in
  store.save creds;
  (match store.load () with
   | Some c -> Alcotest.(check string) "loaded" "test-client" c.client_id
   | None -> Alcotest.fail "Expected stored credentials");
  store.clear ();
  Alcotest.(check bool) "cleared" true (Option.is_none (store.load ()))

let contains s sub =
  try ignore (Str.search_forward (Str.regexp_string sub) s 0); true
  with Not_found -> false

let test_build_authorization_url () =
  let url = Mcp_protocol_http.Oauth_client.build_authorization_url
    ~authorization_endpoint:"https://auth.example.com/authorize"
    ~client_id:"my-client"
    ~redirect_uri:"http://localhost:9999/callback"
    ~scopes:["read"; "write"]
    ~state:"random-state"
    ~code_challenge:"test-challenge"
    () in
  Alcotest.(check bool) "starts with endpoint" true
    (contains url "https://auth.example.com/authorize?");
  Alcotest.(check bool) "contains response_type" true
    (contains url "response_type=code");
  Alcotest.(check bool) "contains client_id" true
    (contains url "client_id=my-client");
  Alcotest.(check bool) "contains S256" true
    (contains url "code_challenge_method=S256")

let test_inject_bearer_token () =
  let headers = Http.Header.of_list [("Content-Type", "application/json")] in
  let with_auth = Mcp_protocol_http.Oauth_client.inject_bearer_token headers
    ~access_token:"my-access-token" in
  Alcotest.(check (option string)) "auth header"
    (Some "Bearer my-access-token")
    (Http.Header.get with_auth "Authorization")

(* ── suite ──────────────────────────────────── *)

let () =
  Alcotest.run "OAuth" [
    "auth types", [
      Alcotest.test_case "protected resource metadata roundtrip" `Quick
        test_protected_resource_metadata_roundtrip;
      Alcotest.test_case "token response roundtrip" `Quick
        test_oauth_token_response_roundtrip;
      Alcotest.test_case "stored credentials roundtrip" `Quick
        test_stored_credentials_roundtrip;
      Alcotest.test_case "oauth error roundtrip" `Quick
        test_oauth_error_roundtrip;
    ];
    "middleware", [
      Alcotest.test_case "missing token returns 401" `Quick
        test_middleware_missing_token;
      Alcotest.test_case "valid token passes" `Quick
        test_middleware_valid_token;
      Alcotest.test_case "invalid token returns 401" `Quick
        test_middleware_invalid_token;
      Alcotest.test_case "optional auth with no token" `Quick
        test_middleware_optional_no_token;
      Alcotest.test_case "scope check" `Quick
        test_middleware_scope_check;
      Alcotest.test_case "bearer extraction" `Quick
        test_bearer_extraction;
      Alcotest.test_case "resource metadata" `Quick
        test_resource_metadata;
    ];
    "oauth client", [
      Alcotest.test_case "PKCE generation" `Quick
        test_pkce_generation;
      Alcotest.test_case "in-memory credential store" `Quick
        test_in_memory_store;
      Alcotest.test_case "authorization URL" `Quick
        test_build_authorization_url;
      Alcotest.test_case "inject bearer token" `Quick
        test_inject_bearer_token;
    ];
  ]
