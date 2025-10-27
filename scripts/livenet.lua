--- Livenet v0: Simple staking with cooldown and slashing
---
--- Core features:
--- 1. Stake: Lock tokens (prevents transfer)
--- 2. Unstake: Start cooldown timer (auto-withdraws after cooldown)
--- 3. Slash: Admin can reduce staked amounts

-- Global state
Stakes = Stakes or {}           -- address -> {amount, lock_duration, stake_time}
Unstaking = Unstaking or {}     -- address -> {timestamp -> [{amount, release_at, msgId}]}
TokenProcess = TokenProcess or ""
Admin = Admin or ""

-- Utility: Add message to outbox
local function send(base, message)
    table.insert(base.results.outbox, message)
    return base
end

-- Utility: Log result
local function log_result(base, status, message)
    ao.event("livenet_log", { status, message })
    base.results = base.results or {}
    base.results.status = status
    base.results.log = base.results.log or {}
    table.insert(base.results.log, message)
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
        num = tonumber(value)
        if not num then
            return nil
        end
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

-- Initialize results
local function init_results(base)
    base.results = base.results or {}
    base.results.outbox = {}
    base.results.status = "OK"
    return base
end

-- Check if request has stake action
local function is_stake_action(request)
    local action = string.lower(request["x-action"] or request["X-Action"] or "")
    return action == "stake", action
end

-- Get lock duration from request (default 24 hours)
local function get_lock_duration(request)
    local lock_duration = normalize_int(request["X-LockDuration"] or request["x-lock-duration"])
    return lock_duration or 86400000  -- Default: 24 hours in milliseconds
end

-- Stake: Lock tokens via Credit-Notice
function stake(base, assignment)
    base = init_results(base)
    local request = assignment.body

    -- Validate Credit-Notice from token process
    if TokenProcess ~= request.from then
        return "error", log_result(base, "error", {
            message = "Not from trusted token process",
            expected = TokenProcess,
            received = request.from
        })
    end

    -- Check stake action
    local is_stake, action = is_stake_action(request)
    if not is_stake then
        return "error", log_result(base, "error", {
            message = "Not a stake action",
            action = action
        })
    end

    -- Normalize quantity
    local quantity = normalize_int(request.quantity)
    if not quantity or quantity <= 0 then
        return "error", log_result(base, "error", {
            message = "Invalid quantity",
            quantity = request.quantity
        })
    end

    -- Get lock duration
    local lock_duration = get_lock_duration(request)

    -- Update stake
    local current = Stakes[request.sender] or { amount = 0, stake_time = assignment.timestamp }
    current.amount = current.amount + quantity
    current.lock_duration = lock_duration
    Stakes[request.sender] = current

    base = send(base, {
        target = request.sender,
        action = "Stake-Success",
        quantity = current.amount,
        ["lock-duration"] = lock_duration
    })

    return "ok", log_result(base, "ok", {
        message = "Stake successful",
        user = request.sender,
        quantity = quantity,
        total = current.amount
    })
end

-- Unstake: Start cooldown period
function unstake(base, assignment)
    base = init_results(base)
    local request = assignment.body
    
    -- Get sender
    local from = request.from
    -- Check stake exists
    if not Stakes[from] then
        return "error", log_result(base, "error", {
            message = "No stake found",
            user = from
        })
    end

    local stake = Stakes[from]

    -- Normalize quantity
    local quantity = normalize_int(request.quantity)
    if not quantity or quantity <= 0 then
        return "error", log_result(base, "error", {
            message = "Invalid quantity",
            quantity = request.quantity
        })
    end

    -- Check sufficient stake
    if stake.amount < quantity then
        return "error", log_result(base, "error", {
            message = "Insufficient stake",
            requested = quantity,
            available = stake.amount
        })
    end

    -- Reduce staked amount
    Stakes[from].amount = stake.amount - quantity

    -- Calculate release time
    local release_at = assignment.timestamp + stake.lock_duration

    -- Get message ID
    local _, msgId = ao.resolve(assignment, { path = "id", commitments = "all" })

    -- Add to unstaking queue
    Unstaking[from] = Unstaking[from] or {}
    Unstaking[from][release_at] = Unstaking[from][release_at] or {}
    table.insert(Unstaking[from][release_at], {
        amount = quantity,
        release_at = release_at,
        msgId = msgId
    })

    base = send(base, {
        target = from,
        action = "Unstake-Initiated",
        quantity = quantity,
        ["release-at"] = release_at,
        ["remaining-staked"] = Stakes[from].amount
    })

    return "ok", log_result(base, "ok", {
        message = "Unstake initiated",
        user = from,
        quantity = quantity,
        release_at = release_at
    })
end

-- Slash: Admin reduces staked amount
function slash(base, assignment)
    base = init_results(base)
    local request = assignment.body

    -- Check admin
    local from = ao.get("committers", request)
    if #from == 0 then
        return "error", log_result(base, "error", {
            message = "No committer found" 
        })
    end
    from = from[1]

    if from ~= Admin then
        return "error", log_result(base, "error", {
            message = "Only admin can slash",
            admin = Admin,
            caller = from
        })
    end

    -- Get target
    local target = request.target or request["target-user"]
    if not target then
        return "error", log_result(base, "error", {
            message = "No target specified"
        })
    end

    if not Stakes[target] then
        return "error", log_result(base, "error", {
            message = "Target has no stake",
            target = target
        })
    end

    -- Normalize amount
    local amount = normalize_int(request.quantity or request.amount)
    if not amount or amount <= 0 then
        return "error", log_result(base, "error", {
            message = "Invalid slash amount",
            quantity = request.quantity
        })
    end

    local original = Stakes[target].amount

    -- Slash
    if amount >= Stakes[target].amount then
        Stakes[target].amount = 0
        amount = original
    else
        Stakes[target].amount = Stakes[target].amount - amount
    end

    base = send(base, {
        target = target,
        action = "Stake-Slashed",
        amount = amount,
        remaining = Stakes[target].amount,
        reason = request.reason or "Admin penalty"
    })

    return "ok", log_result(base, "ok", {
        message = "Slash successful",
        target = target,
        slashed = amount,
        remaining = Stakes[target].amount
    })
end

-- Auto-finalize expired unstaking records
local function auto_finalize(base, assignment)
    local timestamp = assignment.timestamp
    local processed = 0

    for user, unstaking_records in pairs(Unstaking) do
        for release_time, entries in pairs(unstaking_records) do
            if release_time <= timestamp then
                local total = 0
                for _, entry in ipairs(entries) do
                    total = total + entry.amount
                    base = send(base, {
                        target = TokenProcess,
                        action = "Transfer",
                        recipient = user,
                        quantity = entry.amount
                    })
                    processed = processed + 1
                end

                if total > 0 then
                    base = send(base, {
                        target = user,
                        action = "Auto-Withdraw-Success",
                        quantity = total,
                        ["release-at"] = release_time
                    })
                end

                Unstaking[user][release_time] = nil
            end
        end
    end

    if processed > 0 then
        ao.event({
            "Auto-finalized unstakes",
            { count = processed, timestamp = timestamp }
        })
    end

    return base
end

-- Main compute function
function compute(base, assignment)
    base = init_results(base)

    -- Auto-finalize first
    base = auto_finalize(base, assignment)

    local action = string.lower(assignment.body.action or "")

    if action == "credit-notice" then
        return stake(base, assignment)
    elseif action == "unstake" then
        return unstake(base, assignment)
    elseif action == "slash" then
        return slash(base, assignment)
    else
        -- Initialization or unknown action
        if assignment.slot == 0 then
            TokenProcess = 
                base.token_process_id or base["token-process-id"] or TokenProcess
            Admin = base.authority or base.admin or Admin
            ao.event({ 
                "Livenet initialized", 
                { token = TokenProcess, admin = Admin }
            })
        end
        return "ok", base
    end
end
