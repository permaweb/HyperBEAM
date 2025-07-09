local json = require('json')
local bint = require('.bint')(256)

-- Add a message to the outbox of the given base.
local function send(base, message)
    table.insert(base.results.outbox, message)
    return base
end

-- Normalize a quantity value to ensure it is a proper integer.
-- Returns either the normalized integer value or nil and an error message.
local function normalize_int(value)
    local num
    -- Handle string conversion
    if type(value) == "string" then
        -- Check for decimal part (not allowed)
        if string.find(value, "%.") then
            return nil
        end
        -- Convert to number
        float = tonumber(value)
        if not float then
            return nil
        end
        num = math.floor(float)
    elseif type(value) == "number" then
        num = value
        -- Check if it's an integer
        if num ~= math.floor(num) then
            return nil
        end
    else
        -- Any other type is invalid.
        return nil
    end

    return num
end

-- Add a log message to the results of the given base.
local function log_result(base, status, message)
    ao.event("stake_log", { "Stake action log: ", {
        status = status,
        message = message
    } })
    base.results = base.results or {}
    base.results.status = status

    if base.results.log then
        table.insert(base.results.log, message)
    else
        base.results.log = { message }
    end

    return base
end

local function validate_stake_params(base,request)

    ao.event({ "Verifying the credit-notice is from token process.", {
        request = request,
        token = base.token or ""
    } })
    if base.token ~= request.from then
        ao.event({ "Credit Notice is not coming from trusted Token.", {
            token = base.token,
            from = request.from
        }})
        return "error", log_result(base, "error", {
            message = "Credit Notice is not coming from trusted Token."
        })
    end

    ao.event({"Checking if forwarded tag have stake action.",{
        request = request,
        action = request["x-action"] or ""
    }})
    local action = string.lower(request["x-action"] or request["X-Action"] or "")
    if action ~= "stake" then
        return "error", log_result(base, "error", {
            message = "Credit Notice is not a stake action.",
        })
    end

    -- TODO: check all the stake opts in here 

    return "ok",base
end

-- Ensure that the stake is initialized.
local function ensure_initialized(base, assignment)
    -- Ensure that the base has a `result' field before we try to register.
    base.results = base.results or {}
    base.results.outbox = {}
    base.results.status = "OK"

    -- If the stake is not being initialized, we can skip the rest of the
    -- function.
    if assignment.slot ~= 0 then
        return "ok", base
    end

    -- Ensure that the `stakes' map is initialized: present and empty.
    base.stakes = base.stakes or {}
    ao.event({ "Stakes before initialization: ", base.stakes })
    -- Ensure that the `unstaking' map is initialized: present and empty.
    base.unstaking = base.unstaking or {}
    ao.event({ "Unstaking before initialization: ", base.unstaking })
    -- base.unstakingIndex = base.unstakingIndex or {}

    base.token = assignment.body.token
    if not base.token then
        ao.event({ "Stake has no source token. Skipping registration." })
        return "ok", base
    end
    ao.event({ "Token for stake: ", base.token })

    -- TODO: can add the finalization here

    ao.event({ "Stake Process initialized.", { slot = assignment.slot } })
    return "ok", base
end

-- Validate the incoming request and ensure the stake is initialized.
local function validate_request(incoming_base, assignment)
    -- Ensure that the stake is initialized.
    local status, base = ensure_initialized(incoming_base, assignment)
    if status ~= "ok" then
        return "error", log_result(incoming_base, "error", {
            message = "Stake initialization failed.",
            assignment = assignment,
            status = status,
        })
    end

    return "ok", base, assignment.body
end

-- Status constants
STATUS = {
    STAKED = "STAKED",
    IN_COOLDOWN = "IN_COOLDOWN",
}

function stake(base,assignment)
    local status, request
    status,base, request = validate_request(base,assignment)
    if status ~= "ok" or not request then
        return "ok", base
    end

    local validate_status
    validate_status, base = validate_stake_params(base, request)
    if validate_status ~= "ok" then
        return "error", base 
    end

    -- Normalize the quantity value.
    local quantity = normalize_int(request.quantity)
    if not quantity then
        return log_result(base, "error", {
            message = "Invalid quantity value.",
            quantity = request.quantity
        })
    end

    -- Parse and validate penalties if provided
    local penalties = {
        token_per_failed_request = '0',
        max_penalties_per_epoch = '0'
    }

    -- Parse and validate slashing criteria if provided
    local slashing_criteria = {
        minimum_number_of_complainer = 0,
        types_of_nodes = { 'sev-snp', 'tdx', 'jacked-in' }
    }
    -- TODO: parse above values from forwarded tags

    -- Update stake information
    base.stakes[request.sender] = {
        quantity = quantity,
        stakeTime = assignment.timestamp,
        cooldownStart = 0,
        status = STATUS.STAKED,
        max_request_cost = request["X-MaxRequestCost"],
        lock_duration = request["X-LockDuration"],
        penalties = penalties,
        slashing_criteria = slashing_criteria
    }

    base = send(base, {
        target = request.sender,
        action = "stake-success",
        quantity = quantity
    })

    return log_result(base, "ok", {
        message = "Stake action processed successfully.",
        token = request.from,
        user = request.sender,
        quantity = quantity
    })

end

function unstake(base, assignment)
    local status, request
    status, base, request = validate_request(base, assignment)
    if status ~= "ok" or not request then
        return "ok", base
    end

    local from = ao.get("committers", request)
    if base.stakes[from] == nil then
        ao.event({ "Stake not found for the user" })
        return "error", log_result(base, "error", {
            message = "Stake not found for the user."
        })
    end
    local stake_info = base.stakes[from]
    -- Normalize the quantity value.
    local quantity = normalize_int(request.quantity)
    if not quantity then
        return log_result(base, "error", {
            message = "Invalid quantity value.",
        })
    end
    if stake_info.quantity < quantity then
        ao.event({ "Insufficient stake quantity for unstaking.", {
            user = from,
            requested = quantity,
            available = stake_info.quantity
        }})
        return "error", log_result(base, "error", {
            message = "Insufficient stake quantity for unstaking.",
        })
    end

    -- Staart cooldown period 
    quantity = stake_info.quantity - quantity
    base.stakes[from].quantity = quantity
    
    local _, msgId = ao.resolve(assignment, { path = "id", commitments = "all" })
    local release_at = assignment.timestamp + stake_info.lock_duration
    base.unstaking[from] = {
        [release_at] = {
            quantity = quantity,
            release_at = release_at,
            msgId = msgId
        }
    }
    
    -- NOTE: Unstaking Index is used to ustake all the stakes automatically
    -- at the end of the lock duration.

    -- base.unstaking[from] = {
    --     [msgId] = {
    --         quantity = quantity,
    --         release_at = release_at
    --     }
    -- }

    -- if not base.unstakingIndex[release_at] then
    --     base.unstakingIndex[release_at] = {}
    -- end
    -- table.insert(base.unstakingIndex[release_at], {
    --     user = from,
    --     quantity = quantity,
    --     msgId = msgId
    -- })

    ao.event({ "Unstaking initiated for user", {
        user = from,
        quantity = quantity,
        release_at = release_at
    }})

    base = send(base, {
        target = from,
        action = "unstake-initiated",
        quantity = quantity,
        release_at = release_at
    })
    return log_result(base, "ok", {
        message = "Unstake action processed successfully.",
        user = from,
        quantity = quantity,
        release_at = release_at
    })
end

function withdraw(base, assignment)
    local status, request
    status, base, request = validate_request(base, assignment)
    if status ~= "ok" or not request then
        return "ok", base
    end

    local timestamp = assignment.timestamp
    local from = ao.get("committers", request)
    if not base.unstaking[from] then
        ao.event({ "No unstaking record found for user", { user = from } })
        return "ok", base
    end

    local unstaking_info = base.unstaking[from]

    for ts,info in pairs(unstaking_info) do
        if ts <= timestamp then
            -- Withdraw the stake
            local quantity = info.quantity
            ao.event({ "Withdrawing stake for user", {
                user = from,
                quantity = quantity,
                release_at = ts
            }})
            base.unstaking[from][ts] = nil  -- Remove the unstaking record

            base = send(base, {
                target = base.token,
                action = "transfer",
                recipient = from,
                quantity = quantity
            })

            -- Send the withdrawal message
            base = send(base, {
                target = from,
                action = "withdraw-success",
                quantity = quantity,
                release_at = ts
            })

            return log_result(base, "ok", {
                message = "Withdraw action processed successfully.",
                user = from,
                quantity = quantity,
                release_at = ts
            })
        end
    end
end


--- Index function, called by the `~process@1.0` device for scheduled messages.
--- We route any `action' to the appropriate function based on the request path.
function compute(base, assignment)
    ao.event({ "compute called",
        { balance = base.balance, ledgers = base.ledgers } })

    assignment.body.action = string.lower(assignment.body.action or "")

    if assignment.body.action == "credit-notice" then
        return stake(base, assignment)
    elseif assignment.body.action == "unstake" then
        return unstake(base, assignment)
    elseif assignment.body.action == "withdraw" then
        return withdraw(base, assignment)
    else
        -- Handle unknown `action' values.
        _, base,_ = ensure_initialized(base, assignment)
        base.results = {
            status = "ok"
        }
        ao.event({ "Process initialized.", { slot = assignment.slot } })
        return "ok", base
    end
end
