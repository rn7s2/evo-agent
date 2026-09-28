#!/usr/bin/env python3
"""stub-messages.py — a scripted Anthropic Messages endpoint for backend-free tests.

Speaks just enough of POST /v1/messages (streaming SSE) for evo's adapter,
and answers from the last user turn, so a test script decides what the
"model" does by what it sends:

  the summarizer's system prompt-> text "SUMMARY: ..."
  a tool result came back       -> text "tool done"
  "TOOL:<name>" in the text     -> one tool_use call to <name> with {}
  a goal continuation that
    mentions FINISH             -> update_goal {"status": "complete"}
  "SLOW" in the text            -> 60 text deltas, 0.1 s apart (6 s)
  text starting "CALL <tool> {json}"
                                -> one tool_use call to <tool> with that JSON
  a leading "DELAY<n> "         -> wait n seconds first (then the rest applies)
  anything else                 -> text "ok: <the first 40 chars>"

Every request is recorded; GET /_requests returns them as JSON (model,
tool names, effort, the last user text, and who asked: "coordinator",
"lane N" or "agent", read from the swarm prompt notes), so a test can check
what evo sent.

Usage: stub-messages.py PORT     (prints "stub listening PORT" when ready)
"""

import json
import sys
import threading
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

REQUESTS = []
LOCK = threading.Lock()


def block_text(block):
    if block.get("type") == "text":
        return block.get("text", "")
    if block.get("type") == "tool_result":
        content = block.get("content")
        if isinstance(content, list):
            return " ".join(block_text(b) for b in content)
        return str(content or "")
    return ""


def last_user(messages):
    """The last user message's text, whether it carries a tool result, and
    the text of its LAST block — what the stub acts on.  Consecutive user
    turns reach the model merged into one message (an interrupted turn in
    between is elided), and it is the newest of them that asks something."""
    for message in reversed(messages):
        if message.get("role") == "user":
            content = message.get("content")
            if isinstance(content, str):
                return content, False, content
            has_result = any(b.get("type") == "tool_result" for b in content)
            texts = [block_text(b) for b in content]
            return " ".join(texts), has_result, (texts[-1] if texts else "")
    return "", False, ""


def system_text(system):
    if isinstance(system, str):
        return system
    if isinstance(system, list):
        return " ".join(b.get("text", "") for b in system if isinstance(b, dict))
    return ""


class Handler(BaseHTTPRequestHandler):
    def log_message(self, fmt, *args):  # keep the test output readable
        pass

    def do_GET(self):
        if self.path != "/_requests":
            self.send_error(404)
            return
        with LOCK:
            body = json.dumps(REQUESTS).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def sse(self, event, data):
        self.wfile.write(f"event: {event}\ndata: {json.dumps(data)}\n\n".encode())
        self.wfile.flush()

    def do_POST(self):
        length = int(self.headers.get("Content-Length") or 0)
        request = json.loads(self.rfile.read(length) or b"{}")
        messages = request.get("messages", [])
        text, has_result, newest = last_user(messages)
        system = system_text(request.get("system"))
        role = "agent"
        if "## Swarm coordinator" in system:
            role = "coordinator"
        elif "## Swarm lane " in system:
            role = "lane " + system.split("## Swarm lane ", 1)[1].split()[0]
        record = {
            "role": role,
            "time": time.time(),
            "system_has_report_note": "`report` tool" in system,
            "model": request.get("model"),
            "tools": [t.get("name") for t in request.get("tools", [])],
            "effort": (request.get("output_config") or {}).get("effort"),
            "last_user": text,
            "summarizer": "summarizer" in system,
            "user_texts": [
                " ".join(block_text(b) for b in m["content"])
                if isinstance(m.get("content"), list) else str(m.get("content"))
                for m in messages if m.get("role") == "user"
            ],
        }
        with LOCK:
            REQUESTS.append(record)

        self.send_response(200)
        self.send_header("Content-Type", "text/event-stream")
        self.send_header("Cache-Control", "no-cache")
        self.send_header("Connection", "close")
        self.end_headers()
        try:
            self.respond(request.get("model"), newest, has_result, record)
        except (BrokenPipeError, ConnectionResetError):
            pass  # evo interrupted the request: exactly what a test wants

    def respond(self, model, text, has_result, record):
        self.sse("message_start", {
            "type": "message_start",
            "message": {"id": "msg_stub", "type": "message", "role": "assistant",
                        "model": model, "content": [],
                        "usage": {"input_tokens": 10, "output_tokens": 0}}})
        tool = None
        stripped = text.strip()
        if stripped.startswith("DELAY") and not has_result:
            head, _, rest = stripped.partition(" ")
            try:
                time.sleep(float(head[5:] or "1"))
            except ValueError:
                pass
            stripped = rest.strip()
            text = stripped
        if record["summarizer"]:
            # First: the transcript being summarized quotes every marker below.
            reply = "SUMMARY: the session so far, condensed by the stub."
        elif has_result:
            reply = "tool done"
        elif "You are idle but your goal is still active" in text and "FINISH" in text:
            tool = ("update_goal", {"status": "complete"})
        elif stripped.startswith("CALL "):
            name, _, rest = stripped[5:].partition(" ")
            args = {}
            if rest.strip():
                args, _ = json.JSONDecoder().raw_decode(rest.strip())
            tool = (name, args)
        elif "TOOL:" in text:
            name = text.split("TOOL:", 1)[1].split()[0]
            tool = (name, {})
        else:
            reply = None

        if tool:
            name, args = tool
            self.sse("content_block_start", {
                "type": "content_block_start", "index": 0,
                "content_block": {"type": "tool_use", "id": f"toolu_{len(REQUESTS)}",
                                  "name": name, "input": {}}})
            self.sse("content_block_delta", {
                "type": "content_block_delta", "index": 0,
                "delta": {"type": "input_json_delta", "partial_json": json.dumps(args)}})
            self.sse("content_block_stop", {"type": "content_block_stop", "index": 0})
            stop = "tool_use"
        else:
            self.sse("content_block_start", {
                "type": "content_block_start", "index": 0,
                "content_block": {"type": "text", "text": ""}})
            if reply is None and "SLOW" in text:
                for i in range(60):
                    self.sse("content_block_delta", {
                        "type": "content_block_delta", "index": 0,
                        "delta": {"type": "text_delta", "text": f"slow{i} "}})
                    time.sleep(0.1)
            else:
                reply = reply or ("ok: " + text[:40])
                self.sse("content_block_delta", {
                    "type": "content_block_delta", "index": 0,
                    "delta": {"type": "text_delta", "text": reply}})
            self.sse("content_block_stop", {"type": "content_block_stop", "index": 0})
            stop = "end_turn"
        self.sse("message_delta", {"type": "message_delta",
                                   "delta": {"stop_reason": stop},
                                   "usage": {"output_tokens": 5}})
        self.sse("message_stop", {"type": "message_stop"})


def main():
    port = int(sys.argv[1]) if len(sys.argv) > 1 else 0
    server = ThreadingHTTPServer(("127.0.0.1", port), Handler)
    server.daemon_threads = True
    print(f"stub listening {server.server_address[1]}", flush=True)
    server.serve_forever()


if __name__ == "__main__":
    main()
