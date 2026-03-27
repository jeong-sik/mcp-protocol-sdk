(lang dune 3.16)

(name mcp_protocol)

(version 0.16.0)

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
  (dune (>= 3.16))
  (yojson (>= 2.0))
  (ppx_deriving_yojson (>= 3.7))
  (ppx_deriving_jsonschema (>= 0.0.4))
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
  (dune (>= 3.16))
  (mcp_protocol (= :version))
  (eio (>= 1.0))
  (eio_main (>= 1.0))
  (yojson (>= 2.0))
  (alcotest :with-test)
  (eio :with-test)))

(package
 (name mcp_protocol_http)
 (synopsis "HTTP transport for MCP Protocol SDK (Streamable HTTP)")
 (description
  "HTTP transport for the Model Context Protocol using Streamable HTTP.
Provides SSE encoding/broadcasting, session management, and HTTP server/client.")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.16))
  (mcp_protocol (= :version))
  (mcp_protocol_eio (= :version))
  (eio (>= 1.0))
  (cohttp-eio (>= 6.0))
  (uri (>= 4.0))
  (base64 (>= 3.0))
  (digestif (>= 1.0))
  (mirage-crypto-rng (>= 1.0))
  (tls (>= 2.0))
  (tls-eio (>= 2.0))
  (ca-certs (>= 1.0))
  (yojson (>= 2.0))
  (alcotest :with-test)
  (eio_main :with-test)))
