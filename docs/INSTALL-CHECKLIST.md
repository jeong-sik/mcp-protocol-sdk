# Install Checklist

## Install
- [ ] `opam pin add mcp_protocol git+https://github.com/jeong-sik/mcp-protocol-sdk.git`

## Build from source
- [ ] `git clone https://github.com/jeong-sik/mcp-protocol-sdk.git`
- [ ] `cd mcp-protocol-sdk`
- [ ] `opam install . --deps-only`
- [ ] `dune build`

## Test
- [ ] `dune runtest`
