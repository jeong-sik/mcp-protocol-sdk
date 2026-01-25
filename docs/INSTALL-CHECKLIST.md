# Install Checklist

Use this after setup to verify the SDK builds in your environment.

## Install

- [ ] `opam pin add mcp_protocol git+https://github.com/jeong-sik/mcp-protocol-sdk.git`

## Build from source

- [ ] `git clone https://github.com/jeong-sik/mcp-protocol-sdk.git`
- [ ] `cd mcp-protocol-sdk`
- [ ] `opam install . --deps-only`
- [ ] `dune build`

## Post-install checks

```bash
dune build
```

Optional:

```bash
dune runtest
```
