# OpenAI Responses

`:openai-responses` is a bundled provider API alongside `:anthropic-messages`.
Any provider can select it; no extension or model-name routing is required.
The adapter targets GPT-5.6 and GPT-6 protocol features. Model availability,
context limits and supported reasoning efforts belong in your configuration.

```lisp
;; :openai is seeded with https://api.openai.com/v1 and OPENAI_API_KEY.
(evo:register-model "gpt-6-astra"
  :provider :openai :api :openai-responses
  :context-window 100000 :max-output 16384
  :effort '(:low :medium :high :xhigh :max))

;; Another Responses-compatible service; base URL includes its API version.
;; The adapter appends /responses.
(evo:register-provider :responses-service
  :base-url "https://example.com/v1" :api-key-env "RESPONSES_API_KEY")
```

The example uses a conservative application context budget; configure the
limits supported by your endpoint. Existing Messages registrations keep their
default API. There is no built-in model catalog or legacy model fallback.

## Request controls

The session thinking level maps to `reasoning.effort`, clamped to the model's
registered effort ladder. The adapter requests reasoning summaries with
`reasoning.summary: "auto"` by default. An explicit `reasoning` object replaces
that default (use `{}` for endpoints without summary support). `:responses-options` accepts a JSON object string
or a hash table with exact wire keys. A JSON string preserves schema property
names and the distinction between JSON null and false.

```lisp
(evo:register-model "gpt-5.6-sol"
  :provider :openai :api :openai-responses
  :context-window 100000 :max-output 16384 :effort t
  :responses-options
  "{\"reasoning\":{\"mode\":\"pro\",\"context\":\"all_turns\"},
    \"text\":{\"verbosity\":\"low\"},\"parallel_tool_calls\":true}")
```

Supported options: `reasoning`, `text` (including `text.format` JSON schema
structured outputs), `tool_choice`, `parallel_tool_calls`, `tools`, `include`,
`metadata`, `service_tier`, `prompt_cache_key`, `prompt_cache_retention`,
`safety_identifier`, `max_tool_calls`, and `truncation`. The registered session
effort overrides an effort in options. Additional wire tools are appended to
evo's function tools. Only configure tools that the adapter can handle below.
Unknown request options fail explicitly.

## Supported behavior

- Streaming text, refusals and reasoning summaries through evo's event hooks.
- System instructions; user, developer and assistant messages; text, image
  (base64 or URL), and file inputs (`:file-id`, `:file-url` or `:file-data` with
  `:filename`). Images retain the shared vision and request-budget handling.
- Function definitions, parallel calls, exact JSON arguments, tool results
  including images/files, and synthetic results for interrupted tool calls.
  Existing evo tools default to `strict: false`, preserving genuinely optional
  schema fields. Direct tool specs may use `:strict t` with a strict-compatible
  schema; explicit wire tools can also supply their own strict schemas.
- Complete output-item replay: encrypted reasoning, message phase, citations,
  annotations and function call IDs survive journal save/resume. Unified text
  and thinking fields drive the UI; `:responses-item-json` holds the wire item.
  Raw items are replayed only to the same model, API and provider. Foreign
  reasoning is discarded; ordinary text and function calls remain portable.
- Hosted web search, file search, code interpreter, image generation and MCP
  execution/listing items are retained for replay without local execution.
  Their raw results are available in the journal, not rendered as new UI types.
- Input, output, cached-input and reasoning-token usage. Cached tokens are
  subtracted from ordinary input; reasoning tokens are already included in
  output and are not counted twice.
- Completed, token-limited, filtered, failed and cancelled responses; malformed
  tool arguments; aborts; truncated streams; shared HTTP retry and watchdogs.
  Incomplete function calls never reach local execution.

The terminal response's `output` is authoritative, including when tool argument
chunks are interleaved. Text/summary deltas are emitted live; local tools run
only after the complete response has arrived. Unrecognized output item types
or terminal statuses fail loudly instead of silently dropping required work.

## Transport boundaries

Evo replays full local history with `store: false` and `stream: true`. It does
not use stored conversations, `previous_response_id`, background polling,
WebSocket steering, async local tool scheduling, free-form custom tools, or
client-executed computer/MCP approval flows. These require additional runtime
contracts beyond this synchronous provider adapter. They are not enabled by
passing arbitrary request options. JSON structured output is returned as text;
the caller decides how to consume it.

## Official protocol references

- [Responses migration and item model](https://developers.openai.com/api/docs/guides/migrate-to-responses)
- [Streaming events](https://developers.openai.com/api/docs/guides/streaming-responses)
- [Function calling and strict schemas](https://developers.openai.com/api/docs/guides/function-calling)
- [Reasoning controls and stateless replay](https://developers.openai.com/api/docs/guides/reasoning)
- [Structured outputs](https://developers.openai.com/api/docs/guides/structured-outputs)

Protocol fixtures live in `tests/responses.lisp` and run with `make test`.
