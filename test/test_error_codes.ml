open Mcp_protocol

(* --- standard error codes --- *)

let test_standard_codes () =
  Alcotest.(check int) "parse_error" (-32700) Error_codes.parse_error;
  Alcotest.(check int) "invalid_request" (-32600) Error_codes.invalid_request;
  Alcotest.(check int) "method_not_found" (-32601) Error_codes.method_not_found;
  Alcotest.(check int) "invalid_params" (-32602) Error_codes.invalid_params;
  Alcotest.(check int) "internal_error" (-32603) Error_codes.internal_error

(* --- MCP-specific codes --- *)

let test_mcp_codes () =
  Alcotest.(check int) "connection_closed" (-32001) Error_codes.connection_closed;
  Alcotest.(check int) "request_timeout" (-32002) Error_codes.request_timeout;
  Alcotest.(check int) "resource_not_found" (-32003) Error_codes.resource_not_found;
  Alcotest.(check int) "tool_execution_error" (-32004) Error_codes.tool_execution_error;
  Alcotest.(check int) "prompt_not_found" (-32005) Error_codes.prompt_not_found;
  Alcotest.(check int) "url_elicitation_required" (-32006) Error_codes.url_elicitation_required;
  Alcotest.(check int) "stateless_mode_not_supported" (-32007) Error_codes.stateless_mode_not_supported

(* --- is_server_error --- *)

let test_is_server_error () =
  Alcotest.(check bool) "-32000 is server" true (Error_codes.is_server_error (-32000));
  Alcotest.(check bool) "-32050 is server" true (Error_codes.is_server_error (-32050));
  Alcotest.(check bool) "-32099 is server" true (Error_codes.is_server_error (-32099));
  Alcotest.(check bool) "-32100 is not" false (Error_codes.is_server_error (-32100));
  Alcotest.(check bool) "-31999 is not" false (Error_codes.is_server_error (-31999));
  Alcotest.(check bool) "0 is not" false (Error_codes.is_server_error 0);
  Alcotest.(check bool) "-32700 is not" false (Error_codes.is_server_error (-32700))

(* --- describe --- *)

let test_describe_standard () =
  Alcotest.(check string) "parse" "Parse error: Invalid JSON"
    (Error_codes.describe (-32700));
  Alcotest.(check string) "invalid request" "Invalid Request: Not a valid JSON-RPC request"
    (Error_codes.describe (-32600));
  Alcotest.(check string) "method not found" "Method not found"
    (Error_codes.describe (-32601));
  Alcotest.(check string) "invalid params" "Invalid params"
    (Error_codes.describe (-32602));
  Alcotest.(check string) "internal error" "Internal error"
    (Error_codes.describe (-32603))

let test_describe_mcp () =
  Alcotest.(check string) "connection closed" "Connection closed"
    (Error_codes.describe (-32001));
  Alcotest.(check string) "timeout" "Request timeout"
    (Error_codes.describe (-32002));
  Alcotest.(check string) "tool execution" "Tool execution error"
    (Error_codes.describe (-32004))

let test_describe_server_error () =
  let desc = Error_codes.describe (-32050) in
  Alcotest.(check bool) "contains code"
    true (String.length desc > 0)

let test_describe_unknown () =
  let desc = Error_codes.describe 999 in
  Alcotest.(check bool) "contains code"
    true (String.length desc > 0)

(* --- Suite --- *)

let () =
  Alcotest.run "Error_codes" [
    "standard_codes", [
      Alcotest.test_case "values" `Quick test_standard_codes;
    ];
    "mcp_codes", [
      Alcotest.test_case "values" `Quick test_mcp_codes;
    ];
    "is_server_error", [
      Alcotest.test_case "boundaries" `Quick test_is_server_error;
    ];
    "describe", [
      Alcotest.test_case "standard" `Quick test_describe_standard;
      Alcotest.test_case "mcp" `Quick test_describe_mcp;
      Alcotest.test_case "server error" `Quick test_describe_server_error;
      Alcotest.test_case "unknown" `Quick test_describe_unknown;
    ];
  ]
