(** Unit tests for Oauth_client module.

    Tests pure functions (no network I/O): PKCE, authorization URL,
    credential store, state parameter, bearer token injection. *)

open Mcp_protocol
open Mcp_protocol_http

(* ── credential store ─────────────────────────── *)

let test_in_memory_store_empty () =
  let store = Oauth_client.in_memory_store () in
  Alcotest.(check (option pass)) "initially empty" None (store.load ())

let test_in_memory_store_save_load () =
  let store = Oauth_client.in_memory_store () in
  let creds : Auth.stored_credentials = {
    client_id = "test-client";
    token_response = None;
    granted_scopes = ["read"; "write"];
  } in
  store.save creds;
  match store.load () with
  | Some loaded ->
    Alcotest.(check string) "client_id" "test-client" loaded.client_id;
    Alcotest.(check (list string)) "scopes" ["read"; "write"] loaded.granted_scopes
  | None -> Alcotest.fail "expected credentials"

let test_in_memory_store_clear () =
  let store = Oauth_client.in_memory_store () in
  let creds : Auth.stored_credentials = {
    client_id = "c"; token_response = None; granted_scopes = [];
  } in
  store.save creds;
  store.clear ();
  Alcotest.(check (option pass)) "cleared" None (store.load ())

let test_in_memory_store_isolation () =
  let s1 = Oauth_client.in_memory_store () in
  let s2 = Oauth_client.in_memory_store () in
  let creds : Auth.stored_credentials = {
    client_id = "c1"; token_response = None; granted_scopes = [];
  } in
  s1.save creds;
  Alcotest.(check bool) "s2 is independent" true (s2.load () = None)

(* ── PKCE ─────────────────────────────────────── *)

let test_pkce_generates_pair () =
  let verifier, challenge = Oauth_client.generate_pkce () in
  Alcotest.(check bool) "verifier non-empty" true (String.length verifier > 0);
  Alcotest.(check bool) "challenge non-empty" true (String.length challenge > 0);
  Alcotest.(check bool) "verifier != challenge" true (verifier <> challenge)

let test_pkce_url_safe () =
  let verifier, challenge = Oauth_client.generate_pkce () in
  let is_url_safe s =
    String.to_seq s |> Seq.for_all (fun c ->
      (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
      (c >= '0' && c <= '9') || c = '-' || c = '_')
  in
  Alcotest.(check bool) "verifier url-safe" true (is_url_safe verifier);
  Alcotest.(check bool) "challenge url-safe" true (is_url_safe challenge)

let test_pkce_unique () =
  let v1, _ = Oauth_client.generate_pkce () in
  let v2, _ = Oauth_client.generate_pkce () in
  Alcotest.(check bool) "unique verifiers" true (v1 <> v2)

(* ── authorization URL ────────────────────────── *)

let test_build_auth_url_basic () =
  let url = Oauth_client.build_authorization_url
    ~authorization_endpoint:"https://auth.example.com/authorize"
    ~client_id:"my-client"
    ~redirect_uri:"http://localhost:3000/callback"
    ~scopes:["read"; "write"]
    ~state:"random-state"
    ~code_challenge:"abc123"
    () in
  Alcotest.(check bool) "starts with endpoint"
    true (String.length url > 0
          && String.sub url 0 (String.length "https://auth.example.com/authorize?")
             = "https://auth.example.com/authorize?");
  ignore url;
  (* Check key parameters are present *)
  let has s = try let _ = Str.search_forward (Str.regexp_string s) url 0 in true with Not_found -> false in
  Alcotest.(check bool) "has response_type" true (has "response_type=code");
  Alcotest.(check bool) "has client_id" true (has "client_id=my-client");
  Alcotest.(check bool) "has state" true (has "state=random-state");
  Alcotest.(check bool) "has code_challenge_method" true (has "code_challenge_method=S256")

let test_build_auth_url_with_resource () =
  let url = Oauth_client.build_authorization_url
    ~authorization_endpoint:"https://auth.example.com/authorize"
    ~client_id:"c"
    ~redirect_uri:"http://localhost/cb"
    ~scopes:["read"]
    ~state:"s"
    ~code_challenge:"ch"
    ~resource:"https://api.example.com"
    () in
  let has s = try let _ = Str.search_forward (Str.regexp_string s) url 0 in true with Not_found -> false in
  Alcotest.(check bool) "has resource param" true (has "resource=")

let test_build_auth_url_encodes_special_chars () =
  let url = Oauth_client.build_authorization_url
    ~authorization_endpoint:"https://auth.example.com/authorize"
    ~client_id:"client with spaces"
    ~redirect_uri:"http://localhost/cb?foo=bar"
    ~scopes:["scope one"; "scope&two"]
    ~state:"s"
    ~code_challenge:"ch"
    () in
  (* URI-encoded spaces and ampersands *)
  Alcotest.(check bool) "no raw spaces in query"
    false (try let _ = Str.search_forward (Str.regexp_string "client with spaces") url 0 in true with Not_found -> false)

(* ── state parameter ──────────────────────────── *)

let test_generate_state_format () =
  let state = Oauth_client.generate_state () in
  Alcotest.(check bool) "non-empty" true (String.length state > 0);
  let is_url_safe s =
    String.to_seq s |> Seq.for_all (fun c ->
      (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
      (c >= '0' && c <= '9') || c = '-' || c = '_')
  in
  Alcotest.(check bool) "url-safe chars" true (is_url_safe state)

let test_validate_state_exact_match () =
  Alcotest.(check bool) "exact match"
    true (Oauth_client.validate_state ~expected:"abc123" ~received:"abc123")

let test_validate_state_mismatch () =
  Alcotest.(check bool) "content mismatch"
    false (Oauth_client.validate_state ~expected:"abc" ~received:"xyz")

let test_validate_state_length_differ () =
  Alcotest.(check bool) "length differ"
    false (Oauth_client.validate_state ~expected:"short" ~received:"much_longer")

let test_validate_state_empty () =
  Alcotest.(check bool) "both empty"
    true (Oauth_client.validate_state ~expected:"" ~received:"")

let test_validate_state_one_empty () =
  Alcotest.(check bool) "expected empty"
    false (Oauth_client.validate_state ~expected:"" ~received:"x");
  Alcotest.(check bool) "received empty"
    false (Oauth_client.validate_state ~expected:"x" ~received:"")

(* ── bearer token injection ──────────────────── *)

let test_inject_bearer_token () =
  let headers = Http.Header.init () in
  let injected = Oauth_client.inject_bearer_token headers ~access_token:"tok123" in
  match Http.Header.get injected "Authorization" with
  | Some v -> Alcotest.(check string) "bearer" "Bearer tok123" v
  | None -> Alcotest.fail "missing Authorization header"

let test_inject_bearer_preserves_existing () =
  let headers = Http.Header.of_list [("Content-Type", "application/json")] in
  let injected = Oauth_client.inject_bearer_token headers ~access_token:"t" in
  Alcotest.(check (option string)) "content-type preserved"
    (Some "application/json") (Http.Header.get injected "Content-Type");
  Alcotest.(check (option string)) "auth added"
    (Some "Bearer t") (Http.Header.get injected "Authorization")

(* ── default_max_response_size ───────────────── *)

let test_default_max_response_size () =
  Alcotest.(check int) "1MB" (1024 * 1024) Oauth_client.default_max_response_size

(* ── Suite ────────────────────────────────────── *)

let () =
  Alcotest.run "Oauth_client" [
    "credential_store", [
      Alcotest.test_case "empty" `Quick test_in_memory_store_empty;
      Alcotest.test_case "save/load" `Quick test_in_memory_store_save_load;
      Alcotest.test_case "clear" `Quick test_in_memory_store_clear;
      Alcotest.test_case "isolation" `Quick test_in_memory_store_isolation;
    ];
    "pkce", [
      Alcotest.test_case "generates pair" `Quick test_pkce_generates_pair;
      Alcotest.test_case "url-safe" `Quick test_pkce_url_safe;
      Alcotest.test_case "unique" `Quick test_pkce_unique;
    ];
    "authorization_url", [
      Alcotest.test_case "basic" `Quick test_build_auth_url_basic;
      Alcotest.test_case "with resource" `Quick test_build_auth_url_with_resource;
      Alcotest.test_case "encodes special chars" `Quick test_build_auth_url_encodes_special_chars;
    ];
    "state_parameter", [
      Alcotest.test_case "format" `Quick test_generate_state_format;
      Alcotest.test_case "exact match" `Quick test_validate_state_exact_match;
      Alcotest.test_case "mismatch" `Quick test_validate_state_mismatch;
      Alcotest.test_case "length differ" `Quick test_validate_state_length_differ;
      Alcotest.test_case "both empty" `Quick test_validate_state_empty;
      Alcotest.test_case "one empty" `Quick test_validate_state_one_empty;
    ];
    "bearer_token", [
      Alcotest.test_case "inject" `Quick test_inject_bearer_token;
      Alcotest.test_case "preserves existing" `Quick test_inject_bearer_preserves_existing;
    ];
    "config", [
      Alcotest.test_case "max_response_size" `Quick test_default_max_response_size;
    ];
  ]
