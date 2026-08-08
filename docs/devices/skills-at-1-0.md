# Device: ~skills@1.0

## Overview

The [`~skills@1.0`](../../src/preloaded/agent/dev_skills.erl) device implements **composable skills** from [What is an Agent?](https://chat.hyper.io/share/wE41XruWrXFj37EdGfFxn3sdXy0Wib9e): *Tools are atomic, Skills are composable* — any agent should be able to use any skill if it has the right tools.

Source: `src/preloaded/agent/dev_skills.erl` (`-implements(<<"skills@1.0">>)`). Published via `forge` as `skills@1.0` (Spec `isu2v3tmV3RmXrQhW6PJlsXDd3A8ktDAv871NjCutxg`, Impl `ov2XZ8-zQ0KrVR_Biz693uXmDyThYAMucHPOCagoM78`, Signer `aa0b-vzWf7Sn4cFKI43P4MUcSgAdAjCH2V2IFTKZGfU`).

```
Tool:  get_gmail_messages   (one capability, via relay/MCP)
Skill: summarize            (procedure: 1..N tools, generic, reusable)
```

The harness is the level playing field: it checks `requires_tools ⊆ agent tools.json` and injects only authorized tools.

## Skill Shape

Stored as JSON under `skill-<name>` + `skill-index` in `hb_store`:

```json
{
  "name": "summarize",
  "description": "Take docs, extract key points, produce tight brief",
  "requires_tools": ["gmail_read"],
  "instructions": "Take a set of documents...",
  "steps": [],
  "version": "1.0"
}
```

## Key Functions

* **`register`** — `name|skill|id + description + requires_tools|tools + instructions|procedure|steps + version` → `hb_store:write` + index `usort`.
    * Example: `POST /~skills@1.0/register {"name":"summarize","requires_tools":["gmail_read"],"instructions":"..."}`

* **`get`** — `name|skill|id` → skill JSON.

* **`list`** — → `{skills:[...], count}` from `skill-index`.

* **`check`** — `skill + agent_tools|tools` → `{can_run:bool, requires_tools, agent_tools, missing:[...]}`. Enforces matrix:
    ```
    Agent A tools=[gmail_read,gmail_send] → inbox-zero [gmail_read] → can_run:true
    Agent B tools=[calendar_read]        → inbox-zero [gmail_read] → can_run:false, missing=[gmail_read]
    ```

* **`run`** — `skill + agent_tools|tools + message|prompt + collection|agent + model|endpoint + identity|soul|user|system|history` → loads skill, `check`, then `tools_to_harness_specs(requires_tools)` → `harness@1.0/handle` with `system=skill.instructions (+ identity/soul/user)` + `history_limit` delegation, returns `{output, history, iterations, can_run:true}` or `{error, can_run:false, missing}`.
    * Composes essay flow: `research → summarize → write_essay` via single `message`.

* **`compose`** — `skill_a|a + skill_b|b + name|new_name` → loads A+B, `requires_tools=usort(A∪B)`, `instructions=A.instructions+"\n\nThen:\n"+B.instructions`, `steps=A.steps++B.steps`, `register` as new skill (e.g., `summarize+publish`).

## AO / Lua Usage

```lua
-- Register reusable skills (once)
ao.resolve({device="skills@1.0", path="register", name="summarize", requires_tools={"gmail_read"}, instructions="Take docs..."})
ao.resolve({device="skills@1.0", path="register", name="research", requires_tools={"fetch"}, instructions="Fetch Natick..."})

-- Check permission
local ok, c = ao.resolve({device="skills@1.0", path="check", skill="inbox-zero", tools={"gmail_read"}}) -- can_run:true

-- Run as agent tom (tools.json = ["gmail_read","gmail_send"])
ao.resolve({device="skills@1.0", path="run", skill="summarize", agent_tools={"gmail_read"}, message="morning brief", collection="agent-tom", identity="...", soul="...", user="..."})
-- → harness builds [System(identity+soul+user+instructions) + history↑limit + tools + current] → loop

-- Compose workflow
ao.resolve({device="skills@1.0", path="compose", skill_a="research", skill_b="summarize", name="research-summarize"})
ao.resolve({device="skills@1.0", path="run", skill="research-summarize", agent_tools={"fetch"}, message="water vs space datacenters", collection="agent-researcher"})
```

Via `src/preloaded/agent/agent.lua`:

```lua
Send({Target=proc, Action="RunSkill", Agent="researcher", Skill="research-write", Prompt="Research Natick..."})
-- agent.lua loads agents/researcher/identity.md etc and calls skills@1.0/run
```

See `examples/datacenter-essay/{skills.json,run.sh,run.lua,essay.out.md}` for full `research → write` flow.
