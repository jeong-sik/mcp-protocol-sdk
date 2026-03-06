open Mcp_protocol

(* --- progress_token --- *)

let test_progress_token_string_roundtrip () =
  let tok = Mcp_result.String_token "abc" in
  let j = Mcp_result.progress_token_to_yojson tok in
  Alcotest.(check (result pass string)) "string token"
    (Ok tok) (Mcp_result.progress_token_of_yojson j)

let test_progress_token_int_roundtrip () =
  let tok = Mcp_result.Int_token 42 in
  let j = Mcp_result.progress_token_to_yojson tok in
  Alcotest.(check (result pass string)) "int token"
    (Ok tok) (Mcp_result.progress_token_of_yojson j)

let test_progress_token_invalid () =
  Alcotest.(check bool) "invalid"
    true (Result.is_error (Mcp_result.progress_token_of_yojson (`Bool true)))

(* --- progress --- *)

let json = Alcotest.testable
  (fun fmt j -> Format.pp_print_string fmt (Yojson.Safe.to_string j))
  (fun a b -> Yojson.Safe.equal a b)

let test_progress_to_yojson () =
  let p : Mcp_result.progress = {
    progress_token = String_token "tok-1";
    progress = 0.5;
    total = Some 1.0;
    message = Some "Halfway";
  } in
  let j = Mcp_result.progress_to_yojson p in
  let open Yojson.Safe.Util in
  Alcotest.(check (float 0.001)) "progress" 0.5 (j |> member "progress" |> to_float);
  Alcotest.(check (float 0.001)) "total" 1.0 (j |> member "total" |> to_float);
  Alcotest.(check string) "message" "Halfway" (j |> member "message" |> to_string)

let test_progress_minimal () =
  let p : Mcp_result.progress = {
    progress_token = Int_token 1;
    progress = 0.0;
    total = None;
    message = None;
  } in
  let j = Mcp_result.progress_to_yojson p in
  let open Yojson.Safe.Util in
  Alcotest.(check (float 0.001)) "progress" 0.0 (j |> member "progress" |> to_float);
  (* total and message should not be present *)
  Alcotest.(check json) "no total" `Null (member "total" j);
  Alcotest.(check json) "no message" `Null (member "message" j)

(* --- cancel_request --- *)

let test_cancel_request_to_yojson () =
  let c : Mcp_result.cancel_request = {
    request_id = String_id "req-1";
    reason = Some "User cancelled";
  } in
  let j = Mcp_result.cancel_request_to_yojson c in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "requestId" "req-1" (j |> member "requestId" |> to_string);
  Alcotest.(check string) "reason" "User cancelled" (j |> member "reason" |> to_string)

let test_cancel_request_no_reason () =
  let c : Mcp_result.cancel_request = {
    request_id = Int_id 42;
    reason = None;
  } in
  let j = Mcp_result.cancel_request_to_yojson c in
  let open Yojson.Safe.Util in
  Alcotest.(check int) "requestId" 42 (j |> member "requestId" |> to_int);
  Alcotest.(check json) "no reason" `Null (member "reason" j)

(* --- tagged messages (phantom types) --- *)

let test_make_params () =
  let p = Mcp_result.make_params "hello" in
  Alcotest.(check string) "data" "hello" p.data;
  Alcotest.(check bool) "no meta" true (Option.is_none (Mcp_result.get_meta p))

let test_make_result () =
  let r = Mcp_result.make_result 42 in
  Alcotest.(check int) "data" 42 r.data

let test_with_meta () =
  let p = Mcp_result.make_params "test" in
  let p' = Mcp_result.with_meta p (`Assoc [("key", `String "value")]) in
  Alcotest.(check bool) "has meta" true (Option.is_some (Mcp_result.get_meta p'));
  match Mcp_result.get_meta p' with
  | Some j ->
    let open Yojson.Safe.Util in
    Alcotest.(check string) "meta key" "value" (j |> member "key" |> to_string)
  | None -> Alcotest.fail "Expected meta"

(* --- request_id (Mcp_result's own, to be removed later) --- *)

let test_request_id_roundtrip () =
  let id = Mcp_result.String_id "test" in
  let j = Mcp_result.request_id_to_yojson id in
  Alcotest.(check (result pass string)) "string id"
    (Ok id) (Mcp_result.request_id_of_yojson j);
  let id2 = Mcp_result.Int_id 99 in
  let j2 = Mcp_result.request_id_to_yojson id2 in
  Alcotest.(check (result pass string)) "int id"
    (Ok id2) (Mcp_result.request_id_of_yojson j2)

let test_request_id_invalid () =
  Alcotest.(check bool) "invalid"
    true (Result.is_error (Mcp_result.request_id_of_yojson (`Bool false)))

(* --- Suite --- *)

let () =
  Alcotest.run "Mcp_result" [
    "progress_token", [
      Alcotest.test_case "string round-trip" `Quick test_progress_token_string_roundtrip;
      Alcotest.test_case "int round-trip" `Quick test_progress_token_int_roundtrip;
      Alcotest.test_case "invalid" `Quick test_progress_token_invalid;
    ];
    "progress", [
      Alcotest.test_case "full" `Quick test_progress_to_yojson;
      Alcotest.test_case "minimal" `Quick test_progress_minimal;
    ];
    "cancel_request", [
      Alcotest.test_case "with reason" `Quick test_cancel_request_to_yojson;
      Alcotest.test_case "no reason" `Quick test_cancel_request_no_reason;
    ];
    "tagged", [
      Alcotest.test_case "make_params" `Quick test_make_params;
      Alcotest.test_case "make_result" `Quick test_make_result;
      Alcotest.test_case "with_meta" `Quick test_with_meta;
    ];
    "request_id", [
      Alcotest.test_case "round-trip" `Quick test_request_id_roundtrip;
      Alcotest.test_case "invalid" `Quick test_request_id_invalid;
    ];
  ]
