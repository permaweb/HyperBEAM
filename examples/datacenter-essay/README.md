# Example: Research & Write — Datacenters in Water and Space

Agent built from **LLM + Harness + Tools + Instructions** ([What is an Agent?](https://chat.hyper.io/share/wE41XruWrXFj37EdGfFxn3sdXy0Wib9e)).

Uses `llm@1.0` (engine), `harness@1.0` (loop + history + `hb_store` memory), `skills@1.0` (composable procedures), `relay@1.0` (tool).

## 1. Instructions — `agents/researcher/`

```
agents/researcher/identity.md — who the agent is
agents/researcher/user.md     — who it serves
agents/researcher/soul.md     — constraints/judgment
agents/researcher/tools.json  — explicit capabilities
agents/researcher/memory/*.md — durable memory (RAM vs files)
```

## 2. Skills — atomic tools → composable procedures

```
Tool (atomic):  fetch via relay@1.0, read file, query store
Skill (1..N tools): research → summarize → write_essay → publish
Any agent can run any skill if its tools.json satisfies requires_tools.
```

Matrix:
```
skill research       requires [fetch]
skill summarize      requires [fetch]
skill write_essay    requires [fetch]
skill publish        requires [drive_write] (optional)
Composed: research + summarize + write_essay  requires [fetch]
```

## 3. Run

```bash
# 1. Start HB node (with llm endpoint)
hb --store fs --port 8734

# 2. Register skills (once)
curl -X POST http://localhost:8734/~skills@1.0/register \
  -d '{"name":"research","requires_tools":["fetch"],"instructions":"..."}'

# 3. Run as agent researcher
aos researcher < src/preloaded/agent/agent.lua
Send({Target=researcher, Action="RunSkill", Agent="researcher", Skill="research-write", Prompt="Research datacenters in water and space, then write 800w essay"})
```

Output is `output` + `history` persisted to `agent-researcher` collection, and `memory` file.

See `run.sh`, `skills.json`, and `essay.instructions.md` below.
