-- harness.lua — Generic AO agent harness for HyperBEAM
-- Demonstrates: llm@1.0 + relay@1.0 (tools) + store/cache FS + query@1.0 + lua bash
-- Generic: any URL/collection — dan-feed is just default test dataset
-- Load: aos <PROC> < src/preloaded/agent/harness.lua

-- 1. Generic ingest: fetch any URL via relay → store → index (dan is default)
Handlers.add("ingest", Handlers.utils.hasMatchingTag("Action", "Ingest"), function(msg)
  local url = msg.Tags.Url or msg.Data or "https://hyperio-mc.github.io/dan-feed/feed.xml"
  local collection = msg.Tags.Collection or "dan"
  print("Harness: fetching "..url.." via relay -> "..collection.." ...")
  local ok, res = pcall(function()
    return ao.resolve({device="harness@1.0", path="ingest", url=url, collection=collection})
  end)
  -- keep IngestDAN as alias for demo
end)
Handlers.add("ingest-dan", Handlers.utils.hasMatchingTag("Action", "IngestDAN"), function(msg)
  print("Harness: fetching dan-feed via harness@1.0 (dan collection)...")
  local ok, res = pcall(function()
    return ao.resolve({device="harness@1.0", path="ingest", url="https://hyperio-mc.github.io/dan-feed/feed.xml", collection="dan"})
  end)
  if not ok then
    print("ingest failed: "..tostring(res))
    Send({Target=msg.From, Data="ingest failed: "..tostring(res)})
    return
  end
  local ingested = res.ingested or res["ingested"] or 0
  print("Ingested "..tostring(ingested).." posts to dan/posts/*")
  -- Summarize via LLM (spark qwen3.6 default)
  local s_ok, s_res = pcall(function()
    return ao.resolve({device="llm@1.0", path="generate", prompt="Summarize these "..tostring(ingested).." Daily Agents News posts in 3 bullet points", model="qwen3.6"})
  end)
  local summary = ""
  if s_ok then summary = s_res.content or s_res.Content or "" end
  Send({Target=msg.From, Data="Ingested "..tostring(ingested).." posts. Summary: "..summary})
end)

-- 2. Generic query: any collection via harness (dan default), plus QueryDAN alias
Handlers.add("query", Handlers.utils.hasMatchingTag("Action", "Query"), function(msg)
  local q = msg.Tags.Query or msg.Data or ""
  local collection = msg.Tags.Collection or "dan"
  local ok, res = ao.resolve({device="harness@1.0", path="query", q=q, collection=collection})
end)
Handlers.add("query-dan", Handlers.utils.hasMatchingTag("Action", "QueryDAN"), function(msg)
  local q = msg.Tags.Query or msg.Data or ""
  local ok, res = ao.resolve({device="harness@1.0", path="query", q=q, collection="dan"})
  if not ok then Send({Target=msg.From, Data="query failed"}); return end
  local results = res.results or {}
  Send({Target=msg.From, Data=require("json").encode({count=res.count or #results, q=q, results=results})})
end)

Handlers.add("list", Handlers.utils.hasMatchingTag("Action", "List"), function(msg)
  local collection = msg.Tags.Collection or "dan"
  local ok, res = ao.resolve({device="harness@1.0", path="list", collection=collection})
end)
Handlers.add("list-dan", Handlers.utils.hasMatchingTag("Action", "ListDAN"), function(msg)
  local ok, res = ao.resolve({device="harness@1.0", path="list", collection="dan"})
  Send({Target=msg.From, Data=require("json").encode(res)})
end)

-- 3. Agent loop: harness@1.0/handle — message + history + tools -> llm loop -> tool dispatch -> store
-- Meets standard: takes message, last-turn history (via collection), and tools, loops until no tool_calls
Handlers.add("agent-run", Handlers.utils.hasMatchingTag("Action", "AgentRun"), function(msg)
  local prompt = msg.Data or msg.Tags.Prompt or "What should the agent do next?"
  local collection = msg.Tags.Collection or "agent"
  local tools = {
    {
      type = "function",
      ["function"] = {
        name = "fetch",
        description = "Fetch any URL via relay@1.0",
        parameters = {
          type = "object",
          properties = {
            ["relay-path"] = {type = "string", description = "URL to fetch"},
            ["relay-method"] = {type = "string", description = "HTTP method"},
            ["relay-body"] = {type = "string", description = "Optional body"}
          },
          required = {"relay-path"}
        }
      }
    }
  }
  -- Harness builds context (prompt + history from collection + tools), calls llm, dispatches tool_calls via relay, loops, stores history
  local ok, res = ao.resolve({device="harness@1.0", path="handle", message=prompt, tools=tools, collection=collection, model="qwen3.6"})
  if not ok then Send({Target=msg.From, Data="harness failed: "..tostring(res)}); return end
  local output = res.output or res.content or ""
  local iterations = res.iterations or 1
  print("Agent output ("..tostring(iterations).." iters): "..string.sub(output,1,300))
  -- Optionally store final output as post
  ao.resolve({device="harness@1.0", path="store", posts={{guid="agent-"..tostring(math.random(100000)), title="Agent run", link="", description=output}}, collection=collection})
  Send({Target=msg.From, Data="Output: "..string.sub(output,1,500).."\nIterations: "..tostring(iterations)})
end)

-- 4. Bash-like via lua (lua is the bash)
Handlers.add("bash", Handlers.utils.hasMatchingTag("Action", "Bash"), function(msg)
  local cmd = msg.Data or ""
  -- In HyperBEAM lua, os.execute is sandboxed, but you can use ao.resolve to subprocess via wasi if needed
  -- For demo, just echo via llm
  local ok, res = ao.resolve({device="llm@1.0", path="generate", prompt="Explain how to run in bash: "..cmd, model="qwen3.6"})
  Send({Target=msg.From, Data=res.content or ""})
end)

print("Harness loaded: Ingest/Query/List (generic, dan default) + IngestDAN/QueryDAN/ListDAN aliases, AgentRun, Bash — llm+relay+cache+query; relay used for fetch")
