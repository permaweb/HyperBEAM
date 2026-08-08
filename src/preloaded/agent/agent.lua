-- agent.lua — Generic AO Agent Process: LLM + Harness + Tools + Instructions + Memory
-- Essay: https://chat.hyper.io/share/wE41XruWrXFj37EdGfFxn3sdXy0Wib9e  — Agent = LLM + Harness + Tools + Instructions
-- This process composes: instructions (agents/<id>/*.md + tools.json) + memory (agents/<id>/memory/*.md) + harness@1.0/handle + skills@1.0
-- Usage: aos <PROC> < src/preloaded/agent/agent.lua
-- Then: Send({Target=PROC, Action="AgentPrompt", Prompt="...", Agent="tom", Skill="summarize"})
-- Or:   Send({Target=PROC, Action="RunSkill", Skill="summarize", Prompt="morning brief", Agent="tom"})

local json = require("json")

local function read_agent_file(agent, filename)
  -- Try HB store via relay? For now, read via harness store collection
  -- Agent files are plain text on disk in this repo (agents/tom/*), but in HyperBEAM they are in hb_store under agent:<id>:<file>
  -- We attempt ao.resolve on skills/harness store, fallback to empty
  local collection = "agent-"..agent
  local ok, res = pcall(function()
    return ao.resolve({device="harness@1.0", path="query", collection=collection, q=filename})
  end)
  if ok and res and res.results and #res.results>0 then
    return res.results[1].description or res.results[1].content or ""
  end
  return ""
end

local function load_instructions(agent)
  -- Essay flow: identity.md + soul.md + user.md are system instructions, tools.json is capabilities, memory/*.md is durable
  -- Try HB store first, then disk (for local dev), then defaults
  local function try_file(path)
    local f = io.open(path, "r")
    if f then local c = f:read("*a"); f:close(); return c end
    return nil
  end
  local base = "agents/"..agent
  local identity = try_file(base.."/identity.md") or read_agent_file(agent, "identity.md")
  if identity == "" then identity = "Identity: "..agent.." — hyper.io agent" end
  local user = try_file(base.."/user.md") or read_agent_file(agent, "user.md")
  if user == "" then user = "User: Tom" end
  local soul = try_file(base.."/soul.md") or read_agent_file(agent, "soul.md")
  if soul == "" then soul = "Principles: boring ships — prefer practical, auditable" end
  local tools = {"gmail_read"}
  local tools_raw = try_file(base.."/tools.json") or read_agent_file(agent, "tools.json")
  if tools_raw and tools_raw ~= "" then
    local ok, dec = pcall(json.decode, tools_raw)
    if ok and type(dec)=="table" then tools = dec end
  end
  return {identity=identity, user=user, soul=soul, tools=tools}
end

-- Main: Run a skill as an agent
Handlers.add("run-skill", Handlers.utils.hasMatchingTag("Action", "RunSkill"), function(msg)
  local agent = msg.Tags.Agent or msg.Tags.agent or "tom"
  local skill = msg.Tags.Skill or msg.Tags.skill or msg.Data or "summarize"
  local prompt = msg.Tags.Prompt or msg.Tags.prompt or msg.Data or "Do the skill"
  -- Load agent tools.json
  local tools_json = read_agent_file(agent, "tools.json")
  local agent_tools = {"gmail_read", "gmail_send"}
  if tools_json ~= "" then
    local ok, decoded = pcall(json.decode, tools_json)
    if ok and type(decoded)=="table" then agent_tools = decoded end
  else
    -- Try msg.Tags.Tools
    local t = msg.Tags.Tools or msg.Tags.tools
    if t then
      local ok, dec = pcall(json.decode, t)
      if ok then agent_tools = dec else agent_tools = {t} end
    end
  end
  print("Agent "..agent.." running skill "..skill.." with tools "..json.encode(agent_tools))
  local instr = load_instructions(agent)
  -- Essay flow: harness builds system = identity+ soul + user + skill instructions + history + tools + current
  -- Pass identity/soul/user explicitly so harness can build system window and enforce limit
  local ok, res = pcall(function()
    return ao.resolve({device="skills@1.0", path="run", skill=skill, agent_tools=agent_tools, message=prompt, collection="agent-"..agent, model="qwen3.6", identity=instr.identity, soul=instr.soul, user=instr.user, history_limit=20})
  end)
  if not ok then
    Send({Target=msg.From, Data="skills run failed: "..tostring(res)})
    return
  end
  if res.can_run == false or res["can_run"]==false then
    Send({Target=msg.From, Data="Agent "..agent.." cannot run skill "..skill..": missing "..json.encode(res.missing or res["missing"] or {})})
    return
  end
  local output = res.output or res.content or ""
  -- Persist memory: append to agents/<agent>/memory via harness store
  local mem_ok = pcall(function()
    return ao.resolve({device="harness@1.0", path="store", collection="agent-"..agent,
      posts={{guid="mem-"..tostring(math.random(1000000)), title="Memory: "..skill, description=output}}})
  end)
  Send({Target=msg.From, Data=json.encode({agent=agent, skill=skill, output=output, iterations=res.iterations or 1, missing=res.missing})})
end)

-- Generic agent prompt (no skill, just harness) — essay flow: system (identity/soul/user) + history + tools + current
Handlers.add("agent-prompt", Handlers.utils.hasMatchingTag("Action", "AgentPrompt"), function(msg)
  local agent = msg.Tags.Agent or "tom"
  local prompt = msg.Data or msg.Tags.Prompt or ""
  local ok, res = pcall(function()
    local instr = load_instructions(agent)
    -- Essay: harness rebuilds window each turn with system (identity/soul/user) + tools + history + current; it manages limit
    local tools = {
      {type="function", ["function"]={name="get_gmail_messages", description="Read Gmail", parameters={type="object", properties={q={type="string"}}}}},
      {type="function", ["function"]={name="get_calendar_events", description="Read Calendar", parameters={type="object", properties={timeMin={type="string"}}}}}
    }
    return ao.resolve({device="harness@1.0", path="handle", message=prompt, system=instr.identity.."\n"..instr.soul.."\n"..instr.user, identity=instr.identity, soul=instr.soul, user=instr.user, tools=tools, collection="agent-"..agent, model="qwen3.6", history_limit=20})
  end)
  if not ok then Send({Target=msg.From, Data="agent failed: "..tostring(res)}); return end
  Send({Target=msg.From, Data=json.encode({agent=agent, output=res.output or "", history=res.history})})
end)

-- Ingest agent instructions into store (helper)
Handlers.add("ingest-agent", Handlers.utils.hasMatchingTag("Action", "IngestAgent"), function(msg)
  local agent = msg.Tags.Agent or "tom"
  -- In real HyperBEAM, you'd POST agents/tom/*.md via relay; here just confirm
  local ok, res = pcall(function()
    return ao.resolve({device="skills@1.0", path="list"})
  end)
  Send({Target=msg.From, Data="Agent "..agent.." ready. Skills: "..json.encode(res and res.skills or {})})
end)

print("Agent loaded: RunSkill/AgentPrompt/IngestAgent — LLM+harness+skills+instructions+memory — essay model")
