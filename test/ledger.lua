--[[
hyperOS simple ledger

simple library for managing account balances based on incoming "Credit-Notice" events. It responds to requests for balance checks ("Balance"), provides a list of all balances ("Balances"), and supports burning tokens ("Burn").  It also handles informational requests ("Info"). Designed for use within the hyperOS ecosystem, likely as part of a token/transaction processing system.

]]

Tokens = Tokens or {
  "0syT13r0s0tgPmIed95bJnuSqaD29HQNN8D3ElLSrsc",
  "JA2gH5W6SEB2Uj6ggkD9c0kIgKkFtxaLkBxihAbNNic"
}
Balances = Balances or {}
local state = require('.state')
local Utils = require('.utils')
local json = require('.json')

local utils = {
  add = function(a, b)
    a = type(a) == 'string' and tonumber(a) or a
    b = type(b) == 'string' and tonumber(b) or b
    return string.format("%i", (a + b))
  end,
  subtract = function(a, b)
    a = type(a) == 'string' and tonumber(a) or a
    b = type(b) == 'string' and tonumber(b) or b

    return string.format("%i", (a - b))
  end,
  toBalanceValue = function(a)
    a = type(a) == 'string' and tonumber(a) or a
    return string.format("%i", a)
  end,
  toNumber = function(a)
    return tonumber(a)
  end
}

-- applies credit if notice if from a token

local function applyCredit(req)
  local msg = req.body
  local from = state.getFrom(req)
  -- validate inputs
  assert(type(msg.sender) == 'string', 'Sender is required.')
  assert(type(msg.quantity) == 'string', 'Quantity is required.')
  assert(tonumber(msg.quantity) > tonumber(0), 'Quantity must be greater than zero.')
  assert(Utils.includes(from, Tokens), 'Credit must be from approved tokens.')
  Balances[msg.sender] = Balances[msg.sender] or "0"
  assert(tonumber(Balances[msg.sender]) >= 0, 'Balance must greater or equal to zero.')
  -- -- apply credits
  print('apply credits')
  Balances[msg.sender] = utils.add(msg.quantity, Balances[msg.sender])
  print('Credit has been applied.')
  print(msg.sender .. " - balance: " .. Balances[msg.sender])
  -- update cache with new balance
  Send({
     method="patch",
     balances = { [msg.sender] = Balances[msg.sender] }
  })
end

-- checks the balance for a given address
local function checkBalance(req)
  local msg = req.body
  local from = state.getFrom(req)
  if msg.recipient then
    Send({ target = from, data = Balances[msg.recipient] or "0" })
    return
  end
  Send({ target = from, data = Balances[from] })
end

local function isProcess(req)
  return req.body.type == "Process" and "continue"
end

local function initializeProcess(req)
  if req.body.token and type(req.body.token) == 'string' then
    table.insert(Tokens, req.body.token)
  end
end

local function sendBalances(req)
  Send({target = state.getFrom(req), data = json.encode(Balances), count = string.format('%i', #Utils.keys(Balances)) })
end

local function burn(req)
  local _from = state.getFrom(req)
  assert(_from == Owner, 'Sender must be process Owner')
  assert(type(req.body.address) == 'string', 'Address is required')
  assert(type(req.body.quantity) == 'string', 'Quantity is required')
  assert(utils.toNumber(req.body.quantity) > 0, 'Quantity should be greater than 0')
  assert(utils.toNumber(req.body.quantity) <= utils.toNumber(Balances[req.body.address]), 'Not enough balance to burn')
  Balances[req.body.quantity] = utils.subtract(Balances[req.body.address],req.body.quantity)
  req.reply({
    target = _from,
    action = "Burn-Notice",
    quantity = req.body.quantity,
    address = req.body.address,
    method = "patch",
    balances = {
      [req.body.address] = Balances[req.body.address]
    }
  })
  print('Burned ' .. req.body.quantity .. ' from ' .. req.body.address)
end

local function sendInfo(req)
  req.reply({ count = #Utils.keys(Balances), data = "Simple ledger process"})
end

Handlers.add("Init", isProcess, initializeProcess)
Handlers.add("Credit-Notice", applyCredit)
Handlers.add("Balance", checkBalance)
Handlers.add("Balances", sendBalances)
Handlers.add("Burn", burn)
Handlers.add("Info", sendInfo)

print('Loaded ledger blueprint.')
