local json = require('json')
local bint = require('.bint')(256)

-- Utility functions for bint operations
local utils = {
  add = function(a, b)
    return tostring(bint(a) + bint(b))
  end,
  subtract = function(a, b)
    return tostring(bint(a) - bint(b))
  end,
  toBalanceValue = function(a)
    return tostring(bint(a))
  end,
  isGreaterThanOrEqual = function(a, b)
    return bint(a) >= bint(b)
  end
}

-- Initialize State
Stakes = Stakes or {}  -- Stores all stakes: address -> {amount, stakeTime, cooldownStart, status}
TokenProcess = TokenProcess or "" -- Address of the token process
MinimumStake = MinimumStake or utils.toBalanceValue(10 * 10 ^ 12) -- 10 tokens minimum


-- Status constants
STATUS = {
  STAKED = "STAKED",
  IN_COOLDOWN = "IN_COOLDOWN",
}

-- Initialize stake data structure
local function initStake(address)
  if not Stakes[address] then
    Stakes[address] = {
      amount = "0",
      stakeTime = 0,
      lock_duration = 0,
      cooldownStart = 0,
      status = STATUS.STAKED,
      max_request_cost = '0',
      penalties = {
        token_per_failed_request = '0',
        max_penalties_per_epoch = '0',
      },
      slashing_criteria = {
        minimum_number_of_complainer = 0,
        types_of_nodes = {'sev-snp','tdx','jacked-in'}  -- How are these types of nodes defined?
      }
    }
  end
end

function isValidStake(Msg)
    if Msg.From == TokenProcess and Msg.Action == "Credit-Notice" and Msg["X-Action"] == "Stake" then
        return true
    else
        return false
    end
end

-- Handler for staking tokens
Handlers.add('stake',isValidStake, function(msg)
  assert(TokenProcess ~= "", "Token process not set")
  assert(type(msg.Tags.Quantity) == 'string', "Stake quantity required")
  
  if not utils.isGreaterThanOrEqual(msg.Tags.Quantity, MinimumStake) then
    Send({
      Target = TokenProcess,
      Action = "Transfer",
      Quantity = msg.Tags.Quantity,
      Recipient = msg.Sender,
      ['X-Error'] = "Stake amount below minimum"
    })
    return
  end

  -- Parse and validate penalties if provided
  local penalties = {
    token_per_failed_request = '0',
    max_penalties_per_epoch = '0'
  }
  

  assert(msg.Tags["X-TokenPerFailedRequest"] and type(msg.Tags["X-TokenPerFailedRequest"]) == 'string', "TokenPerFailedRequest must be a string")
  local penalty_amount = tonumber(msg.Tags["X-TokenPerFailedRequest"])
  assert(penalty_amount and penalty_amount >= 0, "TokenPerFailedRequest must be a non-negative number")
  penalties.token_per_failed_request = msg.Tags["X-TokenPerFailedRequest"]
  
  assert(msg.Tags["X-MaxPenaltiesPerEpoch"] and type(msg.Tags["X-MaxPenaltiesPerEpoch"]) == 'string', "MaxPenaltiesPerEpoch must be a string")
  local max_penalties = tonumber(msg.Tags["X-MaxPenaltiesPerEpoch"])
  assert(max_penalties and max_penalties >= 0, "MaxPenaltiesPerEpoch must be a non-negative number")
  penalties.max_penalties_per_epoch = msg.Tags["X-MaxPenaltiesPerEpoch"]

  
  -- Parse and validate slashing criteria if provided
  local slashing_criteria = {
    minimum_number_of_complainer = 0,
    types_of_nodes = {'sev-snp','tdx','jacked-in'}
  }
  
  assert(msg.Tags["X-MinComplainers"], "Minimum Complainers required for slashing criteria")
  local min_complainers = tonumber(msg.Tags["X-MinComplainers"])
  assert(min_complainers and min_complainers >= 0, "MinComplainers must be a non-negative number")
  slashing_criteria.minimum_number_of_complainer = min_complainers

  assert(msg.Tags["X-MaxRequestCost"] and type(msg.Tags["X-MaxRequestCost"]) == 'string', "Maximum Request Cost required and must be a string")
  
  assert(msg.Tags["X-LockDuration"], "Lock Duration required in milliseconds")


  initStake(msg.Sender)
  local amount = utils.add(Stakes[msg.Sender].amount or "0", msg.Tags.Quantity)
  
  -- Update stake information
  Stakes[msg.Sender] = {
    amount = amount,
    stakeTime = msg.Timestamp,
    cooldownStart = 0,
    status = STATUS.STAKED,
    max_request_cost = msg.Tags["X-MaxRequestCost"],
    lock_duration = msg.Tags["X-LockDuration"],
    penalties = penalties,
    slashing_criteria = slashing_criteria
  }
  
  Send({
      Target = msg.Sender,
      Action = "Stake-Success",
      Data = json.encode(Stakes[msg.Sender])
    })
end)

-- Handler for initiating unstake (starts cooldown)
Handlers.add('unstake', Handlers.utils.hasMatchingTag("Action", "Unstake"), function(msg)
  assert(Stakes[msg.From], "No active stake found")

  local staked = Stakes[msg.From]
  assert(staked.status == STATUS.STAKED, "Stake is already in cooldown")
  
  -- Start cooldown period
  Stakes[msg.From].cooldownStart = msg.Timestamp
  Stakes[msg.From].status = STATUS.IN_COOLDOWN
  
    msg.reply({
        Action = "Unstake-Initiated",
        Data = json.encode({
        amount = staked.amount,
        cooldownStart = staked.cooldownStart,
        withdrawalAvailable = utils.add(staked.cooldownStart, tostring(CooldownPeriod))
        })
    })
end)

-- Handler for withdrawing tokens after cooldown
Handlers.add('withdraw', Handlers.utils.hasMatchingTag("Action", "Withdraw"), function(msg)
  assert(Stakes[msg.From], "No stake record found")

  local staked = Stakes[msg.From]
  assert(staked.status == STATUS.IN_COOLDOWN, "unstake first to wirhdraw")
  assert(msg.Timestamp >= utils.add(staked.cooldownStart, tostring(CooldownPeriod)), "Cooldown period not completed")
  
  local stakeAmount = staked.amount

  local function safeTransfer(target, quantity)
      local success, result = pcall(function()
          return Send({
              Target = TokenProcess,
              Action = "Transfer",
              Recipient = target,
              Quantity = quantity,
              ["X-Withdraw"] = "true"
          }).receive().Tags
      end)
      
      if not success then
          -- Log error, notify user
          return {
              success = false,
              error = result
          }
      end
      
      return {
          success = true,
          result = result
      }
  end
  
  local sendStake = safeTransfer(msg.From, stakeAmount)

  if sendStake.success and sendStake.result.Action == "Debit-Notice" then
    -- Reset stake data
    Stakes[msg.From] = {
      amount = "0",
      stakeTime = 0,
      cooldownStart = 0,
      status = STATUS.STAKED
    }
    
    msg.reply({
      Action = "Withdraw-Success",
      Data = json.encode({
        amount = stakeAmount,
        timestamp = msg.Timestamp
      })
    })
    return
  end

  msg.reply({
    Action = "Withdraw-Failure",
    Data = "Some issue occured while tranferring of token"
  })

end)