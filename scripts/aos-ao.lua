Handlers = Handlers or require('.handlers')

local oldaos = aos or {}

local utils = require('.utils')

local aos = {
    _version = "0.0.6",
    id = oldaos.id or "",
    _module = oldaos._module or "",
    authorities = oldaos.authorities or {},
    reference = oldaos.reference or 0,
    outbox = oldaos.outbox or
        {Output = {}, Messages = {}, Spawns = {}, Assignments = {}},
    nonExtractableTags = {
        'data-protocol', 'variant', 'from-process', 'from-module', 'type',
        'from', 'owner', 'anchor', 'target', 'data', 'tags', 'read-only'
    },
    nonForwardableTags = {
        'data-protocol', 'variant', 'from-process', 'from-module', 'type',
        'from', 'owner', 'anchor', 'target', 'tags', 'tagArray', 'hash-chain',
        'timestamp', 'nonce', 'slot', 'epoch', 'signature', 'forwarded-by',
        'pushed-for', 'read-only', 'cron', 'block-height', 'reference', 'id',
        'reply-to'
    },
    Nonce = nil
}

function aos.clearOutbox()
  aos.outbox = { Output = {}, Messages = {}, Spawns = {}, Assignments = {}}
end

local function getId(m)
  local id = ""
  utils.map(function (k)
    local c = m.commitments[k]
    if c.alg == "rsa-pss-sha512" or c.type == "rsa-pss-sha512" then
      id = k
    elseif (c.alg == "signed" or c.type == "signed") and c['commitment-device'] == "ans104" then
      id = k
    end
  end, utils.keys(m.commitments)
  )
  return id
end

local function splitOnComma(str)
  print(str)
  local curr = ""
  local parts = {}
  for i = 1, #str do
    local c = str:sub(i, i)
    if c == "," then
      table.insert(parts, curr)
      curr = ""
    else
      curr = curr .. c
    end
  end
  table.insert(parts, curr)
  return parts
end


function aos.init(env)
  if aos.id == "" then aos.id = getId(env.process) end

  -- if aos._module == "" then
  --   aos._module = env.Module.Id
  -- end
  -- TODO: need to deal with assignables
  if #aos.authorities < 1 then
      if type(env.process.authority) == 'string' then
        aos.authorities = {}
        for part in splitOnComma(env.process.authority) do
          if part ~= "" and part ~= nil and not utils.includes(part, aos.authorities) then
            table.insert(aos.authorities, part)
          end
        end
      else
        aos.authorities = env.process.authority
      end
  end

  aos.outbox = {Output = {}, Messages = {}, Spawns = {}, Assignments = {}}
  aos.env = env

end

function aos.send(msg)
  assert(type(msg) == 'table', 'msg should be a table')

  aos.reference = aos.reference + 1
  local referenceString = tostring(aos.reference)
  -- set kv
  msg.reference = referenceString

  -- clone message info and add to outbox
  table.insert(aos.outbox.Messages, utils.reduce(
    function (acc, key)
      acc[key] = msg[key]
      return acc
    end,
    {},
    utils.keys(msg)
  ))

  if msg.target then
    msg.onReply = function(...)
      local from, resolver
      if select("#", ...) == 2 then
        from = select(1, ...)
        resolver = select(2, ...)
      else
        from = msg.target
        resolver = select(1, ...)
      end
      Handlers.once({
        from = from,
        ["x-reference"] = referenceString
      }, resolver)
    end
  end
  return msg
end

function aos.spawn(module, msg)
  assert(type(module) == "string", "Module source id is required!")
  assert(type(msg) == "table", "Message must be a table.")

  aos.reference = aos.reference + 1

  local spawnRef = tostring(aos.reference)

  msg["reference"] = spawnRef

  -- clone message info and add to outbox
  table.insert(aos.outbox.Spawns, utils.reduce(
    function (acc, key)
      acc[key] = msg[key]
      return acc
    end,
    {},
    utils.keys(msg)
  ))

  msg.onReply = function(cb)
    Handlers.once({
      action = "Spawned",
      from = aos.id,
      ["x-reference"] = spawnRef
    }, cb)
  end

  return msg

end

-- registerHint
--
function aos.registerHint(msg)
  -- check if From-Process tag exists
  local fromProcess = nil
  local hint = nil
  local hintTTL = nil

  -- find From-Process tag
  if msg.Tags then
      for name, value in pairs(msg.Tags) do
          if name == "From-Process" then
              -- split by & to get process, hint, and ttl
              local parts = {}

              for part in string.gmatch(value, "[^&]+") do
                  table.insert(parts, part)
              end
              local hintParts = {}
              if parts[2] then
                  for item in string.gmatch(parts[2], "[^=]+") do
                      table.insert(hintParts, item)
                  end
              end
              local ttlParts = {}
              if parts[3] then
                  for item in string.gmatch(parts[3], "[^=]+") do
                      table.insert(ttlParts, item)
                  end
              end

              fromProcess = parts[1] or nil
              hint = hintParts[2] or nil
              hintTTL = ttlParts[2] or nil
              break
          end
      end
  end

  -- if we found a hint, store it in the registry
  if hint then
      if not aos._hints then
          aos._hints = {}
      end
      aos._hints[fromProcess] = {
          hint = hint,
          ttl = hintTTL
      }
  end
  -- enforce bounded registry of 1000 keys
  if aos._hints then
      local count = 0
      local oldest = nil
      local oldestKey = nil

      -- count keys and find oldest entry
      for k, v in pairs(aos._hints) do
          count = count + 1
          if not oldest or v.ttl < oldest then
              oldest = v.ttl
              oldestKey = k
          end
      end

      -- if over 1000 entries, remove oldest
      if count > 1000 and oldestKey then
          aos._hints[oldestKey] = nil
      end
  end
end

function aos.result(result)
  if aos.outbox.Error or result.Error then
    return { Error = result.Error or aos.outbox.Error }
  end
  return {
    Output = result.Output or aos.output.Output,
    Messages = aos.outbox.Messages,
    Spawns = aos.outbox.Spawns,
    Assignments = aos.outbox.Assignments
  }
end

-- set global Send and Spawn
Send = Send or aos.send
Spawn = Spawn or aos.spawn

return aos
