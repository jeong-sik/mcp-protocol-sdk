(lang dune 3.16)

(name mcp_protocol)

(version 1.1.0)

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
  "A complete OCaml implementation of the Model Context Protocol (MCP) specification.
Provides types, serialization, Eio-based transports (stdio, HTTP), and server/client
frameworks for building MCP services.")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.16))
  (yojson (>= 2.0))
  (ppx_deriving_yojson (>= 3.7))
  (ppx_deriving_jsonschema (>= 0.0.4))
  (uri (>= 4.0))
  (eio (>= 1.0))
  (eio_main (>= 1.0))
  (cohttp-eio (>= 6.0))
  (base64 (>= 3.0))
  (digestif (>= 1.0))
  (mirage-crypto-rng (>= 1.0))
  (tls (>= 2.0))
  (tls-eio (>= 2.0))
  (ca-certs (>= 1.0))
  (alcotest :with-test)
  (eio_main :with-test)))
