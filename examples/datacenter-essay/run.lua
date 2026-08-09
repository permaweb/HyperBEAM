-- run.lua — same flow via Lua ao.resolve (for aOS)
-- Load in aos:  .load run.lua  or  ao.resolve directly
local json = require("json")

-- 1. register skills (run once)
local skills = json.decode(io.open("skills.json"):read("*a"))
for _, s in ipairs(skills) do
  local ok, res = ao.resolve({device="skills@1.0", path="register", name=s.name, description=s.description, requires_tools=s.requires_tools, instructions=s.instructions})
  print("registered", s.name, ok)
end

-- 2. run as agent researcher (identity/user/soul + tools.json + memory) via skills -> harness -> llm
local agent_tools = {"fetch"} -- from agents/researcher/tools.json
local ok, res = ao.resolve({
  device="skills@1.0", path="run",
  skill="research-write",
  agent_tools=agent_tools,
  message="Research water (Natick, Nautilus) and space (Kepler, Starcloud) datacenters, then write 800w essay with table and sources. Voice: boring, practical.",
  collection="agent-researcher",
  model="qwen3.6"
})
print(json.encode({output=(res.output or ""):sub(1,500), can_run=res.can_run, iterations=res.iterations}))

-- 3. compose example: any agent with same tools can reuse skill
-- local ok2, c = ao.resolve({device="skills@1.0", path="compose", skill_a="research", skill_b="write_essay", name="my-flow"})
