# MCP Config Template

This SDK does not run a server by itself. Use this template for an MCP server
that is built with this SDK.

Add the entry below to `~/.mcp.json`. If `mcpServers` already exists, merge it.

## HTTP

```json
{
  "mcpServers": {
    "your-server": {
      "type": "http",
      "url": "http://127.0.0.1:PORT/mcp"
    }
  }
}
```

## stdio (optional)

```json
{
  "mcpServers": {
    "your-server": {
      "command": "your-server",
      "args": ["--stdio"]
    }
  }
}
```
