(** Tests for OAuth Discovery, Dynamic Client Registration, and TLS helpers. *)

open Mcp_protocol

(* ── discovery URL construction tests ───────── *)

let test_discover_url_construction () =
  (* We can't test actual HTTP calls without a server,
     but we test the well-known URL construction logic
     by verifying the metadata type roundtrip *)
  let metadata : Auth.authorization_server_metadata = {
    issuer = "https://auth.example.com";
    authorization_endpoint = "https://auth.example.com/authorize";
    token_endpoint = "https://auth.example.com/token";
    registration_endpoint = Some "https://auth.example.com/register";
    scopes_supported = Some ["read"; "write"; "admin"];
    response_types_supported = Some ["code"];
    code_challenge_methods_supported = Some ["S256"];
  } in
  let json = Auth.authorization_server_metadata_to_yojson metadata in
  match Auth.authorization_server_metadata_of_yojson json with
  | Ok m ->
    Alcotest.(check string) "issuer" "https://auth.example.com" m.issuer;
    Alcotest.(check string) "auth endpoint"
      "https://auth.example.com/authorize" m.authorization_endpoint;
    Alcotest.(check string) "token endpoint"
      "https://auth.example.com/token" m.token_endpoint;
    Alcotest.(check (option string)) "registration endpoint"
      (Some "https://auth.example.com/register") m.registration_endpoint;
    Alcotest.(check (option (list string))) "code challenge methods"
      (Some ["S256"]) m.code_challenge_methods_supported
  | Error e -> Alcotest.fail (Printf.sprintf "Roundtrip failed: %s" e)

let test_metadata_minimal () =
  (* Metadata with only required fields *)
  let json = `Assoc [
    ("issuer", `String "https://auth.example.com");
    ("authorization_endpoint", `String "https://auth.example.com/auth");
    ("token_endpoint", `String "https://auth.example.com/token");
  ] in
  match Auth.authorization_server_metadata_of_yojson json with
  | Ok m ->
    Alcotest.(check string) "issuer" "https://auth.example.com" m.issuer;
    Alcotest.(check (option string)) "registration" None m.registration_endpoint;
    Alcotest.(check (option (list string))) "scopes" None m.scopes_supported
  | Error e -> Alcotest.fail (Printf.sprintf "Parse failed: %s" e)

(* ── client registration request tests ──────── *)

let test_registration_request_construction () =
  let req : Mcp_protocol_http.Oauth_client.client_registration_request = {
    client_name = "my-mcp-client";
    redirect_uris = ["http://localhost:9999/callback"];
    grant_types = ["authorization_code"];
    response_types = ["code"];
    token_endpoint_auth_method = "none";
  } in
  Alcotest.(check string) "client_name" "my-mcp-client" req.client_name;
  Alcotest.(check (list string)) "grant_types" ["authorization_code"] req.grant_types;
  Alcotest.(check string) "auth_method" "none" req.token_endpoint_auth_method

(* ── TLS helpers tests ──────────────────────── *)

let test_tls_make_client () =
  (* Verify make_client doesn't crash (just constructs the client) *)
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let _client = Mcp_protocol_http.Tls_helpers.make_client net in
  () (* If we get here without exception, TLS init succeeded *)

let test_tls_https_authenticator () =
  (* Verify the authenticator can be constructed *)
  match Mcp_protocol_http.Tls_helpers.https_authenticator () with
  | Ok _f -> () (* System CA bundle found *)
  | Error (`Msg msg) ->
    (* On some CI environments, CA certs might not be available *)
    Printf.printf "Note: HTTPS authenticator unavailable: %s\n" msg

(* ── integration: full OAuth flow types ─────── *)

let test_full_flow_type_chain () =
  (* Verify the type chain from discovery -> registration -> PKCE -> exchange works *)
  let _metadata : Auth.authorization_server_metadata = {
    issuer = "https://auth.example.com";
    authorization_endpoint = "https://auth.example.com/authorize";
    token_endpoint = "https://auth.example.com/token";
    registration_endpoint = Some "https://auth.example.com/register";
    scopes_supported = Some ["mcp:read"];
    response_types_supported = Some ["code"];
    code_challenge_methods_supported = Some ["S256"];
  } in
  (* Generate PKCE *)
  let verifier, challenge = Mcp_protocol_http.Oauth_client.generate_pkce () in
  Alcotest.(check bool) "verifier not empty" true (String.length verifier > 0);
  (* Build authorization URL using discovered endpoints *)
  let url = Mcp_protocol_http.Oauth_client.build_authorization_url
    ~authorization_endpoint:_metadata.authorization_endpoint
    ~client_id:"test-client"
    ~redirect_uri:"http://localhost:9999/callback"
    ~scopes:["mcp:read"]
    ~state:"test-state"
    ~code_challenge:challenge
    ~resource:"https://mcp.example.com"
    () in
  Alcotest.(check bool) "url contains resource param" true
    (try ignore (Str.search_forward (Str.regexp_string "resource=") url 0); true
     with Not_found -> false);
  (* Credential store chain *)
  let store = Mcp_protocol_http.Oauth_client.in_memory_store () in
  store.save {
    client_id = "test-client";
    token_response = Some {
      access_token = "access-123";
      token_type = "Bearer";
      expires_in = Some 3600;
      refresh_token = Some "refresh-456";
      scope = Some "mcp:read";
    };
    granted_scopes = ["mcp:read"];
  };
  match store.load () with
  | Some creds ->
    Alcotest.(check string) "stored client_id" "test-client" creds.client_id;
    (match creds.token_response with
     | Some tr ->
       let headers = Mcp_protocol_http.Oauth_client.inject_bearer_token
         (Http.Header.init ()) ~access_token:tr.access_token in
       Alcotest.(check (option string)) "injected bearer"
         (Some "Bearer access-123")
         (Http.Header.get headers "Authorization");
       ignore verifier
     | None -> Alcotest.fail "Expected token_response")
  | None -> Alcotest.fail "Expected stored credentials"

(* ── suite ──────────────────────────────────── *)

let () =
  Alcotest.run "OAuth_discovery" [
    "discovery", [
      Alcotest.test_case "metadata roundtrip" `Quick test_discover_url_construction;
      Alcotest.test_case "minimal metadata" `Quick test_metadata_minimal;
    ];
    "registration", [
      Alcotest.test_case "request construction" `Quick test_registration_request_construction;
    ];
    "tls", [
      Alcotest.test_case "make_client" `Quick test_tls_make_client;
      Alcotest.test_case "https_authenticator" `Quick test_tls_https_authenticator;
    ];
    "integration", [
      Alcotest.test_case "full OAuth flow types" `Quick test_full_flow_type_chain;
    ];
  ]
