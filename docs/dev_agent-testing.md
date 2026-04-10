# dev_agent Testing Guide

## Overview

`dev_agent` is a ReAct (Reason-Act-Observe) agent device that calls an OpenAI-compatible LLM, parses tool calls, executes tools, and loops until a final answer is produced.

This document covers all testing approaches, from quick unit tests to full end-to-end Lua integration.

## Prerequisites

- HyperBEAM compiled: `rebar3 compile`
- An OpenAI-compatible API key (e.g., Novita AI)

## 1. Unit Tests (Mock, No API Required)

Run all 10 mock-based unit tests:

```bash
rebar3 eunit --module=dev_agent
```

These tests cover:
- Single-turn (no tools), single/multiple tool calls, max iterations
- Tool error handling, conversation history format, result truncation
- Unknown tool handling, Lua ao.resolve-style config passing

## 2. Lua Integration Tests (Real API)

These tests run Lua code that calls `ao.resolve` to invoke `dev_agent`, exactly as an AOS Lua process would.

### Run all Lua agent tests

```bash
LUA_TESTS="scripts/agent-test.lua" rebar3 as lua-test eunit --module=dev_lua_test
```

### Run a specific test

```bash
# Simple question (no tool call, ~2-3s)
LUA_TESTS="scripts/agent-test.lua:agent_simple_test" \
  rebar3 as lua-test eunit --module=dev_lua_test

# Question requiring tool call (HTTP GET to httpbin.org, ~5-8s)
LUA_TESTS="scripts/agent-test.lua:agent_resolve_test" \
  rebar3 as lua-test eunit --module=dev_lua_test
```

### Write your own Lua test

Create or edit `scripts/agent-test.lua`. Any function ending in `_test` is auto-discovered:

```lua
function my_custom_agent_test()
    local status, res = ao.resolve({
        path = "/~agent@1.0/run",
        ["agent-user-prompt"] = "Your question here",
        ["agent-model"] = "minimax/minimax-m2.7",
        ["agent-api-peer"] = "https://api.novita.ai",
        ["agent-api-path"] = "/openai/v1/chat/completions",
        ["agent-api-key"] = "YOUR_API_KEY"
    })
    -- res["agent-answer"]      = LLM's final answer (string)
    -- res["agent-iterations"]  = number of ReAct loop iterations
    -- res["agent-error"]       = error info (only if failed)
    return status, res
end
```

## 3. HTTP Endpoint Test (Full Node)

Start a HyperBEAM node and call the agent device directly via HTTP.

### Start the node

```bash
./scripts/e2e-test.sh start
# Wait for "HyperBEAM is ready!" message
```

### Call agent via curl

```bash
curl -s http://localhost:8734/~agent@1.0/run \
  -H "Content-Type: application/json" \
  -d '{
    "agent-user-prompt": "Use the http_request tool to GET https://httpbin.org/get and tell me the origin IP.",
    "agent-model": "minimax/minimax-m2.7",
    "agent-api-peer": "https://api.novita.ai",
    "agent-api-path": "/openai/v1/chat/completions",
    "agent-api-key": "YOUR_API_KEY",
    "agent-max-iterations": 5
  }' | jq .
```

### Stop the node

```bash
./scripts/e2e-test.sh stop
```

## 4. Lua ao.resolve from an AOS Process (Production Pattern)

In a real AOS Lua process, use `ao.resolve` inside a handler:

```lua
Handlers.add("Agent",
  { Action = "Agent-Run" },
  function(msg)
    local status, res = ao.resolve({
      path = "/~agent@1.0/run",
      ["agent-user-prompt"] = msg.Data,
      ["agent-model"] = "minimax/minimax-m2.7",
      ["agent-api-peer"] = "https://api.novita.ai",
      ["agent-api-path"] = "/openai/v1/chat/completions",
      ["agent-api-key"] = "sk_..."
    })
    if status == "ok" then
      ao.send({
        Target = msg.From,
        Data = res["agent-answer"]
      })
    else
      ao.send({
        Target = msg.From,
        Data = "Agent error: " .. tostring(res)
      })
    end
  end
)
```

## Configuration Reference

All configuration is passed as message fields (via Lua `ao.resolve` or HTTP body):

| Field | Required | Default | Description |
|-------|----------|---------|-------------|
| `agent-user-prompt` | Yes | `"Hello"` | The user's question/instruction |
| `agent-model` | No | `"gpt-4o-mini"` | Model ID for the LLM backend |
| `agent-api-peer` | No | `"http://localhost:8080"` | Base URL of the API (host:port only) |
| `agent-api-path` | No | `"/v1/chat/completions"` | API endpoint path |
| `agent-api-key` | No | _(none)_ | Bearer token for Authorization header |
| `agent-max-iterations` | No | `10` | Max ReAct loop iterations before force-stop |

## Troubleshooting

**404 from API**: Check that `agent-api-peer` contains only the host (e.g., `https://api.novita.ai`) and the full path is in `agent-api-path` (e.g., `/openai/v1/chat/completions`). The `peer` field is parsed by `gun` which only extracts host:port.

**Timeout**: The agent may need 10-30 seconds for multi-turn tool calls. For EUnit tests, use generator functions with `{timeout, 120, fun() -> ... end}`.

**Device sandbox**: When calling from Lua, ensure `agent@1.0` and `relay@1.0` are accessible. If the process has a `device-sandbox` setting, these devices must be included.
