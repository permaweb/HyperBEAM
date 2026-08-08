-- llm_sidecar.lua — AO sidecar for dev_llm@1.0
-- Load into your LLM_PID process: aos LLM_PID < llm_sidecar.lua
-- Supports: Chat, Generate, Embed (streaming for Chat)

LLM_MODEL = LLM_MODEL or "llama3.2"
EMBED_MODEL = EMBED_MODEL or "nomic-embed-text"

Handlers.add("llm-chat", Handlers.utils.hasMatchingTag("Action", "Chat"), function(msg)
  local prompt = msg.Tags.Prompt or msg.Data or ""
  local messages = msg.Tags.Messages
  local endpoint = msg.Tags.Endpoint
  local model = msg.Tags.Model or LLM_MODEL
  local stream = msg.Tags.Stream == "true"

  -- Delegate to device — device does hb_http to localhost
  local device = "llm@1.0"
  local res
  if stream then
    res = ao.resolve({ Device = device, Action = "chat", Prompt = prompt, Messages = messages, Model = model, Stream = "true", Endpoint = endpoint })
    local body = res.Body or res.Data or ""
    for chunk in string.gmatch(body, "data: ([^%n]+)") do
      if chunk ~= "[DONE]" then
        local ok, parsed = pcall(function() return require("json").decode(chunk) end)
        local content = ""
        if ok and parsed.choices and parsed.choices[1] and parsed.choices[1].delta then
          content = parsed.choices[1].delta.content or ""
        end
        if content ~= "" then
          Send({ Target = msg.From, Tags = { ["Stream-Chunk"] = "true" }, Data = content })
        end
      end
    end
    Send({ Target = msg.From, Tags = { ["Stream-Done"] = "true" }, Data = "done" })
  else
    res = ao.resolve({ Device = device, Action = "chat", Prompt = prompt, Messages = messages, Model = model, Endpoint = endpoint })
    Send({ Target = msg.From, Data = res and (res.Data or res.body or "") or "" })
  end
end)

Handlers.add("llm-generate", Handlers.utils.hasMatchingTag("Action", "Generate"), function(msg)
  local prompt = msg.Tags.Prompt or msg.Data or ""
  local model = msg.Tags.Model or LLM_MODEL
  local res = ao.resolve({ Device = "llm@1.0", Action = "generate", Prompt = prompt, Model = model })
  local content = res.content or res.Content or (res.raw and res.raw.choices and res.raw.choices[1].message.content) or ""
  Send({ Target = msg.From, Tags = { Content = content }, Data = content })
end)

Handlers.add("llm-embed", function(msg)
  return msg.Tags.Action == "Embed" or msg.Tags.Action == "Embeddings"
end, function(msg)
  local input = msg.Tags.Input or msg.Tags.Prompt or msg.Data or ""
  local model = msg.Tags.Model or EMBED_MODEL
  local endpoint = msg.Tags.Endpoint
  local res = ao.resolve({ Device = "llm@1.0", Action = "embed", Input = input, Model = model, ["embed-endpoint"] = endpoint })
  local embedding = res.embedding or {}
  local embeddings = res.embeddings or {}
  Send({
    Target = msg.From,
    Tags = { ["Embedding-Count"] = tostring(#embeddings) },
    Data = require("json").encode({ embedding = embedding, embeddings = embeddings, raw = res.raw })
  })
end)

print("LLM sidecar loaded — Actions: Chat, Generate, Embed (stream=true supported)")
