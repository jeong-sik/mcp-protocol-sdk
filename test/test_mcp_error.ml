open Mcp_protocol

(* --- constructors --- *)

let test_protocol_without_data () =
  match Mcp_error.protocol ~code:(-32600) ~message:"Invalid" () with
  | Protocol { code; message; data } ->
    Alcotest.(check int) "code" (-32600) code;
    Alcotest.(check string) "message" "Invalid" message;
    Alcotest.(check bool) "data is None" true (Option.is_none data)
  | Sdk _ -> Alcotest.fail "Expected Protocol, got Sdk"

let test_protocol_with_data () =
  let extra = `String "extra" in
  match Mcp_error.protocol ~code:(-32600) ~message:"Invalid" ~data:extra () with
  | Protocol { code; message; data } ->
    Alcotest.(check int) "code" (-32600) code;
    Alcotest.(check string) "message" "Invalid" message;
    (match data with
     | Some d ->
       Alcotest.(check string) "data value"
         (Yojson.Safe.to_string extra) (Yojson.Safe.to_string d)
     | None -> Alcotest.fail "Expected data = Some")
  | Sdk _ -> Alcotest.fail "Expected Protocol, got Sdk"

let test_timeout () =
  match Mcp_error.timeout 30.0 with
  | Sdk (Request_timeout s) ->
    Alcotest.(check (float 0.001)) "seconds" 30.0 s
  | _ -> Alcotest.fail "Expected Sdk (Request_timeout _)"

let test_cancelled_no_reason () =
  match Mcp_error.cancelled () with
  | Sdk (Cancelled None) -> ()
  | _ -> Alcotest.fail "Expected Sdk (Cancelled None)"

let test_cancelled_with_reason () =
  match Mcp_error.cancelled ~reason:"user" () with
  | Sdk (Cancelled (Some r)) ->
    Alcotest.(check string) "reason" "user" r
  | _ -> Alcotest.fail "Expected Sdk (Cancelled (Some _))"

let test_connection_closed () =
  match Mcp_error.connection_closed "EOF" with
  | Sdk (Connection_closed r) ->
    Alcotest.(check string) "reason" "EOF" r
  | _ -> Alcotest.fail "Expected Sdk (Connection_closed _)"

let test_send_failed () =
  match Mcp_error.send_failed "broken" with
  | Sdk (Send_failed d) ->
    Alcotest.(check string) "detail" "broken" d
  | _ -> Alcotest.fail "Expected Sdk (Send_failed _)"

let test_not_connected () =
  match Mcp_error.not_connected with
  | Sdk Not_connected -> ()
  | _ -> Alcotest.fail "Expected Sdk Not_connected"

(* --- to_string --- *)

let contains ~sub s =
  let len_sub = String.length sub in
  let len_s = String.length s in
  if len_sub > len_s then false
  else
    let rec loop i =
      if i > len_s - len_sub then false
      else if String.sub s i len_sub = sub then true
      else loop (i + 1)
    in
    loop 0

let test_protocol_to_string () =
  let s = Mcp_error.to_string
    (Mcp_error.protocol ~code:(-32600) ~message:"Invalid Request" ()) in
  Alcotest.(check bool) "contains ProtocolError" true (contains ~sub:"ProtocolError" s);
  Alcotest.(check bool) "contains code" true (contains ~sub:"-32600" s)

let test_sdk_timeout_to_string () =
  let s = Mcp_error.to_string (Mcp_error.timeout 5.0) in
  Alcotest.(check bool) "contains timed out" true (contains ~sub:"timed out" s);
  Alcotest.(check bool) "contains seconds" true (contains ~sub:"5.0" s)

let test_sdk_not_connected_to_string () =
  let s = Mcp_error.to_string Mcp_error.not_connected in
  Alcotest.(check bool) "contains Not connected" true (contains ~sub:"Not connected" s)

let test_sdk_cancelled_to_string () =
  let s = Mcp_error.to_string (Mcp_error.cancelled ()) in
  Alcotest.(check bool) "contains cancelled" true (contains ~sub:"cancelled" s)

(* --- bridge --- *)

let test_to_string_result_ok () =
  let r = Mcp_error.to_string_result (Ok 42) in
  Alcotest.(check (result int string)) "Ok passthrough" (Ok 42) r

let test_to_string_result_error () =
  let err = Mcp_error.timeout 5.0 in
  let r = Mcp_error.to_string_result (Error err) in
  Alcotest.(check (result int string)) "Error to_string"
    (Error "SdkError: Request timed out after 5.0s") r

(* --- Suite --- *)

let () =
  Alcotest.run "Mcp_error" [
    "constructors", [
      Alcotest.test_case "protocol_without_data" `Quick test_protocol_without_data;
      Alcotest.test_case "protocol_with_data" `Quick test_protocol_with_data;
      Alcotest.test_case "timeout" `Quick test_timeout;
      Alcotest.test_case "cancelled_no_reason" `Quick test_cancelled_no_reason;
      Alcotest.test_case "cancelled_with_reason" `Quick test_cancelled_with_reason;
      Alcotest.test_case "connection_closed" `Quick test_connection_closed;
      Alcotest.test_case "send_failed" `Quick test_send_failed;
      Alcotest.test_case "not_connected" `Quick test_not_connected;
    ];
    "to_string", [
      Alcotest.test_case "protocol_to_string" `Quick test_protocol_to_string;
      Alcotest.test_case "sdk_timeout_to_string" `Quick test_sdk_timeout_to_string;
      Alcotest.test_case "sdk_not_connected_to_string" `Quick test_sdk_not_connected_to_string;
      Alcotest.test_case "sdk_cancelled_to_string" `Quick test_sdk_cancelled_to_string;
    ];
    "bridge", [
      Alcotest.test_case "to_string_result_ok" `Quick test_to_string_result_ok;
      Alcotest.test_case "to_string_result_error" `Quick test_to_string_result_error;
    ];
  ]
