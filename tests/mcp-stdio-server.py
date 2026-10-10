#!/usr/bin/env python3
"""A stub MCP server over stdio, for the stdio cases in tests/unit.lisp.

Awkward on purpose: each thing here is something a real stdio server does and
a friendly one would hide.

  - it logs to stderr at startup (the client must keep that off the terminal);
  - it prints a banner that is not JSON, and a notification, before EVERY
    answer, plus a stale answer to a request nobody is waiting on (the client
    must match answers by id, not take the next line);
  - before answering a tools/call it sends a `ping` REQUEST of its own and
    waits for the reply (the client must answer, or this hangs);
  - its tool results contain non-ASCII text (the pipes must be UTF-8);
  - `hang` never answers (the client's timeout must fire);
  - `pid` reports the process id (a restarted server has a new one);
  - `env` reports $STUB_ENV_PROBE (what the config's :env did to the child).

Usage: mcp-stdio-server.py   (speaks JSON-RPC, one object per line)
"""
import json
import os
import sys
import time


def send(obj):
    sys.stdout.write(json.dumps(obj, ensure_ascii=False) + "\n")
    sys.stdout.flush()


def chatter():
    sys.stdout.write("starting up... (this banner is not JSON)\n")
    send({"jsonrpc": "2.0", "method": "notifications/message",
          "params": {"level": "info", "data": "chatter"}})
    send({"jsonrpc": "2.0", "id": 99999, "result": {"stale": True}})


TOOLS = [
    {"name": "echo", "description": "Echo the arguments back.",
     "inputSchema": {"type": "object",
                     "properties": {"text": {"type": "string"}},
                     "required": ["text"]}},
    {"name": "hang", "description": "Never answers.",
     "inputSchema": {"type": "object", "properties": {}}},
    {"name": "pid", "description": "This server's process id.",
     "inputSchema": {"type": "object", "properties": {}}},
    {"name": "env", "description": "The value of STUB_ENV_PROBE.",
     "inputSchema": {"type": "object", "properties": {}}},
]


def text_result(rid, text):
    send({"jsonrpc": "2.0", "id": rid,
          "result": {"content": [{"type": "text", "text": text}]}})


def main():
    sys.stderr.write("stdio stub: stderr line, must not reach the terminal\n")
    sys.stderr.flush()
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        msg = json.loads(line)
        method = msg.get("method")
        rid = msg.get("id")
        if rid is None:                 # a notification: no answer
            continue
        chatter()
        if method == "initialize":
            send({"jsonrpc": "2.0", "id": rid, "result": {
                "protocolVersion": "2025-06-18",
                "capabilities": {"tools": {}},
                "serverInfo": {"name": "stdio-stub", "version": "0"},
                "instructions": "Stdio stub note."}})
        elif method == "tools/list":
            send({"jsonrpc": "2.0", "id": rid, "result": {"tools": TOOLS}})
        elif method == "tools/call":
            name = msg["params"]["name"]
            args = msg["params"].get("arguments") or {}
            if name == "hang":
                time.sleep(3600)
                continue
            # Ask the client something, and wait for its reply, before answering.
            send({"jsonrpc": "2.0", "id": "srv-1", "method": "ping"})
            reply = json.loads(sys.stdin.readline())
            if reply.get("id") != "srv-1" or "result" not in reply:
                send({"jsonrpc": "2.0", "id": rid,
                      "error": {"code": -32000, "message": "no pong"}})
                continue
            if name == "pid":
                text_result(rid, "pid:%d" % os.getpid())
            elif name == "env":
                text_result(rid, "env:" + os.environ.get("STUB_ENV_PROBE", "unset"))
            else:
                text_result(rid, "echo:" + args.get("text", ""))
        else:
            send({"jsonrpc": "2.0", "id": rid,
                  "error": {"code": -32601, "message": "Method not found"}})


if __name__ == "__main__":
    main()
