Handlers = Handlers or require('test.hyper-handlers')

local oldao = ao or {}

local utils = require('test.hyper-utils')

local ao = {
    _version = "0.0.6",
    id = oldao.id or "",
    _module = oldao._module or "",
    authorities = oldao.authorities or {},
    reference = oldao.reference or 0,
    outbox = oldao.outbox or
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

function ao.clearOutbox()
  ao.outbox = { Output = {}, Messages = {}, Spawns = {}, Assignments = {}}
end

local function getId(m)
  local id = ""
  utils.map(function (k)
    local c = m.commitments[k]
    if c.alg == "rsa-pss-sha512" then
      id = k
    elseif c.alg == "signed" and c['commitment-device'] == "ans104" then
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


function ao.init(env)
  if ao.id == "" then ao.id = getId(env.process) end

  -- if ao._module == "" then
  --   ao._module = env.Module.Id
  -- end
  -- TODO: need to deal with assignables
  if #ao.authorities < 1 then
      if type(env.process.authority) == 'string' then
        ao.authorities = {}
        for part in splitOnComma(env.process.authority) do
          if part ~= "" and part ~= nil and not utils.includes(part, ao.authorities) then
            table.insert(ao.authorities, part)
          end
        end
      else
        ao.authorities = env.process.authority
      end
  end

  ao.outbox = {Output = {}, Messages = {}, Spawns = {}, Assignments = {}}
  ao.env = env

end

function ao.send(msg)
  assert(type(msg) == 'table', 'msg should be a table')

  ao.reference = ao.reference + 1
  local referenceString = tostring(ao.reference)
  -- set kv
  msg.reference = referenceString

  -- clone message info and add to outbox
  table.insert(ao.outbox.Messages, utils.reduce(
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

function ao.spawn(module, msg)
  assert(type(module) == "string", "Module source id is required!")
  assert(type(msg) == "table", "Message must be a table.")

  ao.reference = ao.reference + 1

  local spawnRef = tostring(ao.reference)

  msg["reference"] = spawnRef

  -- clone message info and add to outbox
  table.insert(ao.outbox.Spawns, utils.reduce(
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
      from = ao.id,
      ["x-reference"] = spawnRef
    }, cb)
  end

  return msg

end

-- registerHint
--
function ao.registerHint(msg)
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
      if not ao._hints then
          ao._hints = {}
      end
      ao._hints[fromProcess] = {
          hint = hint,
          ttl = hintTTL
      }
  end
  -- enforce bounded registry of 1000 keys
  if ao._hints then
      local count = 0
      local oldest = nil
      local oldestKey = nil

      -- count keys and find oldest entry
      for k, v in pairs(ao._hints) do
          count = count + 1
          if not oldest or v.ttl < oldest then
              oldest = v.ttl
              oldestKey = k
          end
      end

      -- if over 1000 entries, remove oldest
      if count > 1000 and oldestKey then
          ao._hints[oldestKey] = nil
      end
  end
end

function ao.result(result)
  if ao.outbox.Error or result.Error then
    return { Error = result.Error or ao.outbox.Error }
  end
  return {
    Output = result.Output or ao.output.Output,
    Messages = ao.outbox.Messages,
    Spawns = ao.outbox.Spawns,
    Assignments = ao.outbox.Assignments
  }
end

-- set global Send and Spawn
Send = Send or ao.send
Spawn = Spawn or ao.spawn

return ao
