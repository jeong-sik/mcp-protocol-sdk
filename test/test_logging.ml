open Mcp_protocol

(* --- log_level --- *)

let test_log_level_to_string () =
  Alcotest.(check string) "debug" "debug" (Logging.log_level_to_string Debug);
  Alcotest.(check string) "emergency" "emergency" (Logging.log_level_to_string Emergency)

let test_log_level_of_string () =
  Alcotest.(check (option pass)) "debug"
    (Some Logging.Debug) (Logging.log_level_of_string "debug");
  Alcotest.(check (option pass)) "unknown"
    None (Logging.log_level_of_string "unknown")

let test_log_level_roundtrip () =
  let levels = [Logging.Debug; Info; Notice; Warning; Error; Critical; Alert; Emergency] in
  List.iter (fun level ->
    let j = Logging.log_level_to_yojson level in
    Alcotest.(check (result pass string)) "roundtrip"
      (Ok level) (Logging.log_level_of_yojson j)
  ) levels

let test_log_level_of_yojson_invalid () =
  Alcotest.(check bool) "invalid string"
    true (Result.is_error (Logging.log_level_of_yojson (`String "bogus")));
  Alcotest.(check bool) "not a string"
    true (Result.is_error (Logging.log_level_of_yojson (`Int 42)))

(* --- severity comparison --- *)

let test_log_level_to_int () =
  Alcotest.(check bool) "emergency < debug"
    true (Logging.log_level_to_int Emergency < Logging.log_level_to_int Debug)

let test_compare_level () =
  Alcotest.(check bool) "emergency more severe than debug"
    true (Logging.compare_level Emergency Debug < 0);
  Alcotest.(check bool) "same level"
    true (Logging.compare_level Warning Warning = 0)

let test_should_log () =
  Alcotest.(check bool) "error passes warning filter"
    true (Logging.should_log ~min_level:Warning ~msg_level:Error);
  Alcotest.(check bool) "debug blocked by warning filter"
    false (Logging.should_log ~min_level:Warning ~msg_level:Debug);
  Alcotest.(check bool) "exact level passes"
    true (Logging.should_log ~min_level:Info ~msg_level:Info)

(* --- logging_message --- *)

let json = Alcotest.testable
  (fun fmt j -> Format.pp_print_string fmt (Yojson.Safe.to_string j))
  (fun a b -> Yojson.Safe.equal a b)

let test_logging_message_full () =
  let msg : Logging.logging_message = {
    level = Warning;
    logger = Some "mcp.server";
    data = `String "disk almost full";
  } in
  let j = Logging.logging_message_to_yojson msg in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "level" "warning" (j |> member "level" |> to_string);
  Alcotest.(check string) "logger" "mcp.server" (j |> member "logger" |> to_string);
  Alcotest.(check string) "data" "disk almost full" (j |> member "data" |> to_string)

let test_logging_message_minimal () =
  let msg : Logging.logging_message = {
    level = Debug;
    logger = None;
    data = `Assoc [("key", `Int 42)];
  } in
  let j = Logging.logging_message_to_yojson msg in
  let open Yojson.Safe.Util in
  Alcotest.(check json) "no logger" `Null (member "logger" j);
  Alcotest.(check int) "data.key" 42 (j |> member "data" |> member "key" |> to_int)

let test_logging_message_roundtrip () =
  let msg : Logging.logging_message = {
    level = Error;
    logger = Some "test";
    data = `String "oops";
  } in
  let j = Logging.logging_message_to_yojson msg in
  match Logging.logging_message_of_yojson j with
  | Ok msg' ->
    Alcotest.(check string) "level" "error" (Logging.log_level_to_string msg'.level);
    Alcotest.(check (option string)) "logger" (Some "test") msg'.logger;
    Alcotest.(check json) "data" (`String "oops") msg'.data
  | Error e -> Alcotest.fail e

(* --- set_level_params --- *)

let test_set_level_params_roundtrip () =
  let p : Logging.set_level_params = { level = Info } in
  let j = Logging.set_level_params_to_yojson p in
  match Logging.set_level_params_of_yojson j with
  | Ok p' ->
    Alcotest.(check string) "level" "info" (Logging.log_level_to_string p'.level)
  | Error e -> Alcotest.fail e

(* --- Suite --- *)

let () =
  Alcotest.run "Logging" [
    "log_level", [
      Alcotest.test_case "to_string" `Quick test_log_level_to_string;
      Alcotest.test_case "of_string" `Quick test_log_level_of_string;
      Alcotest.test_case "roundtrip" `Quick test_log_level_roundtrip;
      Alcotest.test_case "invalid" `Quick test_log_level_of_yojson_invalid;
    ];
    "severity", [
      Alcotest.test_case "to_int" `Quick test_log_level_to_int;
      Alcotest.test_case "compare" `Quick test_compare_level;
      Alcotest.test_case "should_log" `Quick test_should_log;
    ];
    "logging_message", [
      Alcotest.test_case "full" `Quick test_logging_message_full;
      Alcotest.test_case "minimal" `Quick test_logging_message_minimal;
      Alcotest.test_case "roundtrip" `Quick test_logging_message_roundtrip;
    ];
    "set_level_params", [
      Alcotest.test_case "roundtrip" `Quick test_set_level_params_roundtrip;
    ];
  ]
