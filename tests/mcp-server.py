#!/usr/bin/env python3
"""A stub MCP server over Streamable HTTP, for tests/mcp.exp.

Small on purpose, and awkward on purpose: it exercises the three things the
client has to get right and that a friendly server would hide.

  - it requires the configured header (401 without it), so a passing call
    proves :headers reached the wire;
  - it answers tools/list as an SSE stream rather than plain JSON, the other
    shape a Streamable HTTP server may pick;
  - it 404s the first tools/call, i.e. "I have forgotten that session", so a
    passing call proves the client re-initialized and retried.

tools/call echoes the keys it received, prefixed, so the test can assert on a
token that appears nowhere in what it typed: a key that survived is proof the
model's exact JSON was forwarded, not the lossy plist spelling of it.

Usage: mcp-server.py PORTFILE   (binds 127.0.0.1:0, writes the port chosen)
"""
import json
import sys
import uuid
from http.server import BaseHTTPRequestHandler, HTTPServer

TOKEN = "Bearer test-token"

TOOLS = [
    {
        "name": "echo_args",
        "description": "Echo the argument keys back.",
        "inputSchema": {
            "type": "object",
            # additionalProperties is exactly what evo's sexpr schema DSL
            # cannot express: it must reach the model verbatim.
            "properties": {
                "files": {"type": "object",
                          "additionalProperties": {"type": ["string", "null"]},
                          "description": "path -> source"},
            },
            "required": ["files"],
        },
    }
]


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"
    expired_once = False

    def log_message(self, *args):
        pass

    def _respond(self, payload, status=200, headers=None, sse=False):
        if payload is None:                      # a notification's 202
            body, ctype = b"", None
        elif sse:
            body = ("event: message\ndata: " + json.dumps(payload) + "\n\n").encode()
            ctype = "text/event-stream"
        else:
            body = json.dumps(payload).encode()
            ctype = "application/json"
        self.send_response(status)
        if ctype:
            self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(body)))
        for key, value in (headers or {}).items():
            self.send_header(key, value)
        self.end_headers()
        if body:
            self.wfile.write(body)

    def do_POST(self):
        length = int(self.headers.get("Content-Length", 0))
        message = json.loads(self.rfile.read(length).decode("utf-8"))
        method = message.get("method")

        if self.headers.get("Authorization") != TOKEN:
            self._respond({"error": "no credential"}, status=401)
            return

        if method == "initialize":
            self._respond({"jsonrpc": "2.0", "id": message["id"],
                           "result": {"protocolVersion": "2025-06-18",
                                      "capabilities": {"tools": {}},
                                      "serverInfo": {"name": "echo", "version": "0.1"},
                                      "instructions": "Echo server: call echo_args."}},
                          headers={"Mcp-Session-Id": uuid.uuid4().hex})
        elif method and method.startswith("notifications/"):
            self._respond(None, status=202)
        elif method == "tools/list":
            self._respond({"jsonrpc": "2.0", "id": message["id"],
                           "result": {"tools": TOOLS}}, sse=True)
        elif method == "tools/call":
            if not Handler.expired_once:
                Handler.expired_once = True
                self._respond({"error": "session not found"}, status=404)
                return
            files = (message["params"].get("arguments") or {}).get("files") or {}
            self._respond({"jsonrpc": "2.0", "id": message["id"],
                           "result": {"content": [
                               {"type": "text",
                                "text": "exact:" + "|".join(sorted(files))}]}})
        else:
            self._respond({"jsonrpc": "2.0", "id": message.get("id"),
                           "error": {"code": -32601, "message": "no such method"}})


if __name__ == "__main__":
    server = HTTPServer(("127.0.0.1", 0), Handler)
    with open(sys.argv[1], "w") as out:
        out.write(str(server.server_address[1]))
    server.serve_forever()
