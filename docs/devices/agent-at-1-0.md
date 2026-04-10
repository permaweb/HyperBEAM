# Device: ~agent@1.0

## Overview

The [`~agent@1.0`](../resources/source-code/dev_agent.md) device implements a
**ReAct** (Reason–Act–Observe) agent loop on top of any OpenAI-compatible LLM
endpoint. It iteratively calls the LLM, executes tool calls, feeds results back
to the model, and repeats until the model returns a plain-text final answer or a
configurable iteration ceiling is hit.

## Core Concept: Iterative Reason-Act-Observe Loop

```
 User Prompt
      │
      ▼
┌──────────────┐    tool_calls     ┌───────────────────┐
│     LLM      │──────────────────▶│  Tool Executor    │
│(inference@   │◀──────────────────│  (built-in/custom)│
│  1.0)        │   tool results    └───────────────────┘
└──────┬───────┘
       │ routes via relay@1.0
       ▼
  local Python              remote provider
  inference server   OR     (OpenRouter, etc.)
  localhost:8080            https://openrouter.ai
```

All LLM calls are routed through [`~inference@1.0`](../resources/source-code/dev_inference.md),
which in turn uses [`~relay@1.0`](relay-at-1-0.md) to reach the backend. This
means local Python inference servers and remote OpenAI-compatible providers are
addressed identically from the agent's perspective — only the `agent-api-peer`
Opts key differs.

Each iteration either:

1. Receives `tool_calls` → executes them → appends results to message history →
   calls the LLM again; or
2. Receives a plain-text reply → returns it as `agent-answer`.

The entire conversation history is managed in-process via Erlang recursion;
nothing is persisted across `run` invocations unless an external tool does so.

## Key Functions (Keys)

### `run`

Start the ReAct loop.

**Request fields:**

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `agent-user-prompt` | binary | `<<"Hello">>` | The initial user message |
| `agent-model` | binary | `<<"gpt-4o-mini">>` | Model name passed to the LLM endpoint |
| `agent-max-iterations` | integer | `10` | Maximum tool-call rounds before force-stopping |
| `agent-api-peer` | binary | `<<"http://localhost:8080">>` | Base URL of the LLM API server |
| `agent-api-path` | binary | `<<"/v1/chat/completions">>` | Chat completions path |
| `agent-api-key` | binary | _(none)_ | Bearer token, added as `Authorization` header |
| `agent-llm-fun` | function | `default_call_llm/2` | Override the LLM call function |
| `agent-tool-fun` | function | `default_execute_tool/2` | Override the tool executor function |

**Response fields added to the base message:**

| Field | Type | Description |
|-------|------|-------------|
| `agent-answer` | binary | Final text answer returned by the model |
| `agent-iterations` | integer | Number of LLM calls made |
| `agent-error` | binary | Set only on error or `max_iterations_exceeded` |

**Example:**

```erlang
{ok, Result} = hb_ao:resolve(
    #{<<"device">> => <<"agent@1.0">>},
    #{<<"path">>             => <<"run">>,
      <<"agent-user-prompt">> => <<"What is the current Arweave block height?">>,
      <<"agent-model">>       => <<"openai/gpt-4o-mini">>,
      <<"agent-api-peer">>    => <<"https://openrouter.ai">>,
      <<"agent-api-path">>    => <<"/api/v1/chat/completions">>,
      <<"agent-api-key">>     => <<"sk-or-v1-...">>},
    #{}).
```

**HyperPATH:**

```
POST /~agent@1.0/run
```

with the fields above as message tags or body keys.

## Built-in Tools

The default tool executor dispatches on the `name` field of each LLM tool call.

### `http_request`

Make an HTTP request to any URL via [`~relay@1.0`](relay-at-1-0.md).

| Parameter | Required | Description |
|-----------|----------|-------------|
| `url` | ✓ | Full URL to request |
| `method` | | `GET` (default), `POST`, `PUT`, `DELETE` |
| `body` | | Request body for `POST`/`PUT` |
| `content_type` | | `Content-Type` header (default `text/plain`) |

### `lookup_data`

Retrieve a message or binary value from the local node cache via
[`~lookup@1.0`](../resources/source-code/dev_lookup.md).

| Parameter | Required | Description |
|-----------|----------|-------------|
| `id` | ✓ | Hash ID of the cached item |

### `search_messages`

Search the node's cache for messages matching a set of key-value pairs via
[`~query@1.0`](../resources/source-code/dev_query.md).

| Parameter | Required | Description |
|-----------|----------|-------------|
| `match` | ✓ | JSON object of key-value pairs to match |
| `return` | | `"paths"` (default), `"messages"`, or `"count"` |

### `get_arweave_tx`

Fetch an Arweave transaction header by its 43-character base64url ID via
[`~arweave@2.9-pre`](../resources/source-code/dev_arweave.md).

| Parameter | Required | Description |
|-----------|----------|-------------|
| `id` | ✓ | Arweave transaction ID |

### `bundle_item`

Submit a data item to be bundled and uploaded to Arweave via
[`~bundler@1.0`](../resources/source-code/dev_bundler.md).
Returns `<<"Message queued.">>` on success.

| Parameter | Required | Description |
|-----------|----------|-------------|
| `data` | ✓ | Data content to store |
| `content_type` | | `Content-Type` of the data (default `text/plain`) |

## Customising Tools

Both the LLM function and the tool executor can be replaced at call time by
passing Erlang fun references in `Opts`:

```erlang
MyToolFun = fun
    (#{name := <<"my_tool">>, args := Args}, _Opts) ->
        %% custom logic
        <<"result">>;
    (ToolInfo, Opts) ->
        %% fall through to the default executor
        dev_agent:default_execute_tool(ToolInfo, Opts)
end,

{ok, Result} = hb_ao:resolve(
    #{<<"device">> => <<"agent@1.0">>},
    #{<<"path">>             => <<"run">>,
      <<"agent-user-prompt">> => <<"Do something custom">>},
    #{<<"agent-tool-fun">> => MyToolFun,
      <<"agent-api-peer">>  => <<"https://openrouter.ai">>,
      <<"agent-api-path">>  => <<"/api/v1/chat/completions">>,
      <<"agent-api-key">>   => <<"sk-or-v1-...">>}).
```

## Configuration via Request Message

All `agent-*` fields are accepted both in `Opts` and in the `Req`/`Base`
message. This allows Lua scripts using `ao.resolve` to pass configuration
without modifying `Opts`:

```lua
local result = ao.resolve({
    path              = "/~agent@1.0/run",
    ["agent-user-prompt"] = "Summarise this process",
    ["agent-api-peer"]    = "https://openrouter.ai",
    ["agent-api-path"]    = "/api/v1/chat/completions",
    ["agent-api-key"]     = MY_KEY
})
```

## Error Handling

*   If the LLM call fails, `agent-error` is set to the relay error reason and
    `agent-answer` is set to `<<"Error calling LLM.">>`.
*   If the response cannot be JSON-decoded, `agent-error` describes the parse
    failure.
*   If `agent-max-iterations` is exceeded, `agent-error` is set to
    `<<"max_iterations_exceeded">>`.
*   Tool errors are returned as plain-text results to the LLM, which can then
    decide how to proceed.

## Tool Result Truncation

To avoid overflowing the LLM's context window, tool results exceeding 4000 bytes
are silently truncated and suffixed with `"\n... [truncated]"`.

[agent module](../resources/source-code/dev_agent.md)
