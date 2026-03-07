(lang dune 3.0)

(name mcp_protocol)

(version 0.7.0)

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

(package
 (name mcp_protocol_eio)
 (synopsis "Eio-based transport and server for MCP Protocol SDK")
 (description
  "Eio-based stdio transport for the Model Context Protocol.
Provides NDJSON over stdin/stdout transport and a minimal server framework.")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.0))
  (mcp_protocol (= :version))
  (eio (>= 1.0))
  (eio_main (>= 1.0))
  (yojson (>= 2.0))
  (alcotest :with-test)
  (eio :with-test)))
