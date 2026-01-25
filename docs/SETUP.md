# Setup

간단 설치/빌드/테스트 방법만 정리합니다.

## 설치

```bash
opam pin add mcp_protocol git+https://github.com/jeong-sik/mcp-protocol-sdk.git
```

또는 `dune-project`에 추가:

```lisp
(depends
 (mcp_protocol (>= 0.1.0)))
```

## 소스 빌드

```bash
git clone https://github.com/jeong-sik/mcp-protocol-sdk.git
cd mcp-protocol-sdk
opam install . --deps-only
dune build
```

## 테스트

```bash
dune runtest
```
