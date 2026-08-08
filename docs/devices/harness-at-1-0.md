# Device: ~harness@1.0

## Overview

The [`~harness@1.0`](../../src/preloaded/agent/dev_harness.erl) device is the **generic agent runtime** for HyperBEAM. It implements the essay model **Agent = LLM + Harness + Tools + Instructions** — one harness, many agents.

Source: `src/preloaded/agent/dev_harness.erl` (`-implements(<<"harness@1.0">>)`), tests via `src/core/test/dev_llm_test.erl` + harness loop integration. Published via `forge` as `harness@1.0` (Spec `r09P4ZkjHMtGIDGRXv_43_CWs2PEwENlPvJwTcf1X-4`, Impl `2thbQP1hIWr48fo3jspVbm6gCMBoHGRh54fnCcXCZwM`, Signer `aa0b-vzWf7Sn4cFKI43P4MUcSgAdAjCH2V2IFTKZGfU`).

It orchestrates `llm@1.0` + `relay@1.0` (tools) + `hb_store` (FS/cache) + `query@1.0` + `lua@5.3a`. The `dan-feed` is just one test dataset (`collection=dan`).

## Core Concept: The Loop

Every turn the harness rebuilds the **context window** as defined in [Understanding the Agent Flow](https://chat.hyper.io/share/_4kT2xkDzGpPncXP5k6wx8zB5m4LKlnT):

1. **System** — `identity.md` + `soul.md` + `user.md` + `system` (combined as `role:system`)
2. **Tools** — `tools.json` → OpenAI `tools` spec
3. **History** — previous `inputs/outputs` from `hb_store` (`<collection>-harness-history`), truncated to `history_limit` (default 20, harness-managed)
4. **Current input** — `message|prompt|data` from any source (chat, schedule, Drive/Notion trigger, agent msg, API)

Then:

```
LLM → tool_calls|output → harness runs each tool via relay@1.0/call → append results → rebuild → LLM ... until no tool_calls → output + persist history
```

That loop *is* the agent. Not one call — a sequence.

## Key Functions

* **`handle` / `run` / `chat` / `execute`** — alias for the agentic loop.
    * **Inputs:** `message|prompt|data|input` (current), `history|messages` (explicit) or `collection`+`history_key` (load from `hb_store`), `tools` (OpenAI spec or `["fetch"]` shorthand via `skills@1.0`), `system|identity|soul|user|instructions` (combined to `role:system`, not persisted), `model` (default `qwen3.6`), `endpoint|llm-endpoint`, `history_limit|max_history` (default 20), `max_iterations` (default 10), `collection` (default `default`).
    * **Response:** `{ok, #{<<"output">>:=Binary, <<"history">>:=Messages, <<"messages">>:=Messages, <<"iterations">>:=N, <<"system">>:=SystemMsg, <<"raw">>:=LLMJson}}` + `hb_store:write` of `history` (without system) to `<collection>-harness-history`.
    * **Example:** `POST /~harness@1.0/handle {"message":"hello","tools":[...],"collection":"agent-tom","system":"You are Tom..."}`

* **`fetch` / `fetch_feed`** — `GET <url>` via `relay@1.0/call` (public; dan-feed default).

* **`parse`** — `body` (RSS XML) → `[{guid,title,link,description}]` (regex, no xmerl).

* **`store`** — `posts|items|body` → `hb_store:write` under `<collection>-<id>` + `<collection>-index`.

* **`ingest`** — `url|collection` → `fetch_via_relay` → `parse_feed` (or JSON fallback) → `store`.

* **`list` / `query`** — `collection-index` + per-item reads, `q` filter on `title|description|link`.

## AO / Lua Usage

```lua
-- Generic: any collection, any URL via relay + harness
ao.resolve({device="harness@1.0", path="ingest", url="https://hyperio-mc.github.io/dan-feed/feed.xml", collection="dan"})
ao.resolve({device="harness@1.0", path="query", q="space", collection="dan"})

-- Agentic loop: system = identity+soul+user, history managed, tools via relay
local ok, res = ao.resolve({
  device="harness@1.0", path="handle",
  message="Research water vs space datacenters",
  system="Identity: researcher\nPrinciples: boring ships",
  identity="...", soul="...", user="...",
  tools={{type="function", ["function"]={name="fetch", parameters={type="object", properties={["relay-path"]={type="string"}}}}}},
  collection="agent-researcher",
  model="qwen3.6", history_limit=20
})
print(res.output)
-- history persisted to hb_store, system not persisted
```

Via `skills@1.0` (recommended):

```lua
ao.resolve({device="skills@1.0", path="run", skill="research-write", agent_tools={"fetch"}, message="write essay", collection="agent-tom", identity=..., soul=..., user=...})
```

See `src/preloaded/agent/agent.lua` (`RunSkill`/`AgentPrompt`) and `examples/datacenter-essay/` for full Agent = LLM+Harness+Tools+Instructions wiring.

## Security

`relay@1.0` blocks private hosts; `llm@1.0` does not (it uses `hb_http` directly). The harness enforces `history_limit` to bound context.
