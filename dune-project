(lang dune 3.0)

(name mcp_protocol)

(version 0.2.1)

(generate_opam_files true)

(source
 (github jeong-sik/mcp-protocol-sdk))

(license MIT)

(authors "jeong-sik")

(maintainers "yousleepwhen@gmail.com")

(package
 (name mcp_protocol)
 (synopsis "Model Context Protocol SDK for OCaml")
 (description
  "A pure OCaml implementation of the Model Context Protocol (MCP) specification.
Provides types, serialization, and utilities for building MCP clients and servers.")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.0))
  (yojson (>= 2.0))
  (ppx_deriving_yojson (>= 3.7))
  (uri (>= 4.0))
  (alcotest :with-test)))
