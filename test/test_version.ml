open Mcp_protocol

(* --- supported_versions --- *)

let test_supported_versions_list () =
  Alcotest.(check int) "4 versions" 4 (List.length Version.supported_versions);
  Alcotest.(check bool) "contains 2024-11-05"
    true (List.mem "2024-11-05" Version.supported_versions);
  Alcotest.(check bool) "contains 2025-03-26"
    true (List.mem "2025-03-26" Version.supported_versions);
  Alcotest.(check bool) "contains 2025-06-18"
    true (List.mem "2025-06-18" Version.supported_versions);
  Alcotest.(check bool) "contains 2025-11-25"
    true (List.mem "2025-11-25" Version.supported_versions)

let test_latest () =
  Alcotest.(check string) "latest" "2025-11-25" Version.latest

let test_default () =
  Alcotest.(check string) "default" "2024-11-05" Version.default

(* --- is_supported --- *)

let test_is_supported () =
  Alcotest.(check bool) "known" true (Version.is_supported "2024-11-05");
  Alcotest.(check bool) "unknown" false (Version.is_supported "1999-01-01")

(* --- compare --- *)

let test_compare () =
  Alcotest.(check bool) "older < newer"
    true (Version.compare "2024-11-05" "2025-03-26" < 0);
  Alcotest.(check bool) "same = same"
    true (Version.compare "2024-11-05" "2024-11-05" = 0);
  Alcotest.(check bool) "newer > older"
    true (Version.compare "2025-11-25" "2024-11-05" > 0)

(* --- negotiate --- *)

let test_negotiate_exact_match () =
  Alcotest.(check (option string)) "exact match"
    (Some "2025-03-26")
    (Version.negotiate ~requested:"2025-03-26")

let test_negotiate_latest () =
  Alcotest.(check (option string)) "latest"
    (Some "2025-11-25")
    (Version.negotiate ~requested:"2025-11-25")

let test_negotiate_unknown_future () =
  (* A future version should negotiate down to latest supported *)
  Alcotest.(check (option string)) "future version"
    (Some "2025-11-25")
    (Version.negotiate ~requested:"2099-01-01")

let test_negotiate_too_old () =
  (* A version older than all supported should return None *)
  Alcotest.(check (option string)) "too old"
    None
    (Version.negotiate ~requested:"2020-01-01")

(* --- is_compatible --- *)

let test_is_compatible () =
  Alcotest.(check bool) "server newer"
    true (Version.is_compatible ~server_version:"2025-11-25" ~client_version:"2024-11-05");
  Alcotest.(check bool) "same version"
    true (Version.is_compatible ~server_version:"2025-03-26" ~client_version:"2025-03-26");
  Alcotest.(check bool) "server older"
    false (Version.is_compatible ~server_version:"2024-11-05" ~client_version:"2025-11-25")

(* --- features_of_version --- *)

let test_features_base () =
  let f = Version.features_of_version "2024-11-05" in
  Alcotest.(check bool) "has_tools" true f.has_tools;
  Alcotest.(check bool) "has_resources" true f.has_resources;
  Alcotest.(check bool) "has_prompts" true f.has_prompts;
  Alcotest.(check bool) "no sampling" false f.has_sampling;
  Alcotest.(check bool) "no elicitation" false f.has_elicitation;
  Alcotest.(check bool) "no streamable" false f.has_streamable_http;
  Alcotest.(check bool) "no structured_output" false f.has_structured_output;
  Alcotest.(check bool) "no resource_links" false f.has_resource_links;
  Alcotest.(check bool) "no tasks" false f.has_tasks;
  Alcotest.(check bool) "no icons" false f.has_icons;
  Alcotest.(check bool) "no extensions" false f.has_extensions

let test_features_2025_03_26 () =
  let f = Version.features_of_version "2025-03-26" in
  Alcotest.(check bool) "has_streamable" true f.has_streamable_http;
  Alcotest.(check bool) "no elicitation" false f.has_elicitation;
  Alcotest.(check bool) "no sampling" false f.has_sampling;
  Alcotest.(check bool) "no structured_output" false f.has_structured_output;
  Alcotest.(check bool) "no icons" false f.has_icons

let test_features_2025_06_18 () =
  let f = Version.features_of_version "2025-06-18" in
  Alcotest.(check bool) "has_streamable" true f.has_streamable_http;
  Alcotest.(check bool) "has_elicitation" true f.has_elicitation;
  Alcotest.(check bool) "has_structured_output" true f.has_structured_output;
  Alcotest.(check bool) "has_resource_links" true f.has_resource_links;
  Alcotest.(check bool) "no sampling" false f.has_sampling;
  Alcotest.(check bool) "no tasks" false f.has_tasks;
  Alcotest.(check bool) "no icons" false f.has_icons;
  Alcotest.(check bool) "no extensions" false f.has_extensions

let test_features_latest () =
  let f = Version.features_of_version "2025-11-25" in
  Alcotest.(check bool) "has_sampling" true f.has_sampling;
  Alcotest.(check bool) "has_elicitation" true f.has_elicitation;
  Alcotest.(check bool) "has_streamable" true f.has_streamable_http;
  Alcotest.(check bool) "has_structured_output" true f.has_structured_output;
  Alcotest.(check bool) "has_resource_links" true f.has_resource_links;
  Alcotest.(check bool) "has_tasks" true f.has_tasks;
  Alcotest.(check bool) "has_icons" true f.has_icons;
  Alcotest.(check bool) "has_extensions" true f.has_extensions

(* --- Suite --- *)

let () =
  Alcotest.run "Version" [
    "constants", [
      Alcotest.test_case "supported_versions" `Quick test_supported_versions_list;
      Alcotest.test_case "latest" `Quick test_latest;
      Alcotest.test_case "default" `Quick test_default;
    ];
    "is_supported", [
      Alcotest.test_case "known/unknown" `Quick test_is_supported;
    ];
    "compare", [
      Alcotest.test_case "ordering" `Quick test_compare;
    ];
    "negotiate", [
      Alcotest.test_case "exact match" `Quick test_negotiate_exact_match;
      Alcotest.test_case "latest" `Quick test_negotiate_latest;
      Alcotest.test_case "unknown future" `Quick test_negotiate_unknown_future;
      Alcotest.test_case "too old" `Quick test_negotiate_too_old;
    ];
    "is_compatible", [
      Alcotest.test_case "compatibility" `Quick test_is_compatible;
    ];
    "features", [
      Alcotest.test_case "base (2024-11-05)" `Quick test_features_base;
      Alcotest.test_case "2025-03-26" `Quick test_features_2025_03_26;
      Alcotest.test_case "2025-06-18" `Quick test_features_2025_06_18;
      Alcotest.test_case "latest" `Quick test_features_latest;
    ];
  ]
