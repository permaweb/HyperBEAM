--- Livenet v0: Non-fungible staking with cooldown and slashing
---
--- Core features:
--- 1. Stake: Lock tokens as individual non-fungible vaults
--- 2. Unstake: Start cooldown timer (auto-withdraws after cooldown)
--- 3. Slash: Admin can reduce staked amounts
---
--- Each stake is tracked separately with its own lock_duration
--- Unstaking uses FIFO (first-in-first-out) from oldest stakes

-- Constants
local DEFAULT_LOCK_DURATION = 86400000  -- 24 hours in milliseconds

-- Global state
Stakes = Stakes or {}           -- address -> array of {id, amount, lock_duration, stake_time}
Unstaking = Unstaking or {}     -- address -> {timestamp -> [{amount, release_at, msgId}]}
UnstakingByTime = UnstakingByTime or {}  -- timestamp -> [{user, amount, vault_id, msgId}]
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

-- Normalize a quantity value to ensure it is a proper integer
local function normalize_int(value)
    local num
    if type(value) == "string" then
        if string.find(value, "%.") then
            return nil
        end
        num = tonumber(value)
        if not num then
            return nil
        end
    elseif type(value) == "number" then
        num = value
        if num ~= math.floor(num) then
            return nil
        end
    else
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
    local lock_duration =
        normalize_int(
            request["X-LockDuration"] or request["x-lock-duration"]
        )
    return lock_duration or DEFAULT_LOCK_DURATION
end

-- Get total staked amount for a user
local function get_total_staked(address)
    if not Stakes[address] then
        return 0
    end

    local total = 0
    for _, vault in ipairs(Stakes[address]) do
        total = total + vault.amount
    end
    return total
end

-- Remove vaults using FIFO and return operations + remaining vaults
-- Returns: {operations = [...], new_vaults = [...]}
local function remove_vaults_fifo(vaults, amount)
    local remaining = amount
    local operations = {}
    local new_vaults = {}

    for _, vault in ipairs(vaults) do
        if remaining <= 0 then
            -- No more to remove, keep remaining vaults
            table.insert(new_vaults, vault)
        elseif vault.amount <= remaining then
            -- Take entire vault
            table.insert(operations, {
                vault_id = vault.id,
                amount = vault.amount,
                lock_duration = vault.lock_duration
            })
            remaining = remaining - vault.amount
            -- Don't add to new_vaults (effectively removed)
        else
            -- Take partial amount from vault
            table.insert(operations, {
                vault_id = vault.id,
                amount = remaining,
                lock_duration = vault.lock_duration
            })
            -- Add reduced vault to new_vaults
            table.insert(new_vaults, {
                id = vault.id,
                amount = vault.amount - remaining,
                lock_duration = vault.lock_duration,
                stake_time = vault.stake_time
            })
            remaining = 0
        end
    end

    return {
        operations = operations,
        new_vaults = new_vaults
    }
end

-- Add unstaking records to both user and time indexes
local function add_to_unstaking_indexes(user, operations, base_timestamp, msgId)
    Unstaking[user] = Unstaking[user] or {}

    for _, op in ipairs(operations) do
        local release_at = base_timestamp + op.lock_duration

        -- Store in user index (for queries)
        Unstaking[user][release_at] = Unstaking[user][release_at] or {}
        table.insert(Unstaking[user][release_at], {
            amount = op.amount,
            release_at = release_at,
            vault_id = op.vault_id,
            msgId = msgId
        })

        -- Store in time index (for efficient finalization)
        UnstakingByTime[release_at] = UnstakingByTime[release_at] or {}
        table.insert(UnstakingByTime[release_at], {
            user = user,
            amount = op.amount,
            vault_id = op.vault_id,
            msgId = msgId
        })
    end
end

-- Stake: Lock tokens via Credit-Notice as individual vault
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

    -- Get lock duration for this stake
    local lock_duration = get_lock_duration(request)

    -- Get stake ID (message ID)
    local _, stakeId = 
        ao.resolve(
            assignment, 
            { path = "id", commitments = "all" }
        )

    -- Initialize Stakes array for user if needed
    Stakes[request.sender] = Stakes[request.sender] or {}

    -- Add new stake vault (non-fungible)
    table.insert(Stakes[request.sender], {
        id = stakeId,
        amount = quantity,
        lock_duration = lock_duration,
        stake_time = assignment.timestamp
    })

    local total_staked = get_total_staked(request.sender)

    base = send(base, {
        target = request.sender,
        action = "Stake-Success",
        ["stake-id"] = stakeId,
        quantity = quantity,
        ["total-staked"] = total_staked,
        ["lock-duration"] = lock_duration
    })

    return "ok", log_result(base, "ok", {
        message = "Stake successful",
        user = request.sender,
        stake_id = stakeId,
        quantity = quantity,
        total_staked = total_staked
    })
end

-- Unstake: Start cooldown period (FIFO - pulls from oldest stakes first)
function unstake(base, assignment)
    base = init_results(base)
    local request = assignment.body

    -- Get sender
    local from = request.from

    -- Check stakes exist
    if not Stakes[from] or #Stakes[from] == 0 then
        return "error", log_result(base, "error", {
            message = "No stake found",
            user = from
        })
    end

    -- Normalize quantity to unstake
    local quantity = normalize_int(request.quantity)
    if not quantity or quantity <= 0 then
        return "error", log_result(base, "error", {
            message = "Invalid quantity",
            quantity = request.quantity
        })
    end

    -- Check sufficient stake
    local total_staked = get_total_staked(from)
    if total_staked < quantity then
        return "error", log_result(base, "error", {
            message = "Insufficient stake",
            requested = quantity,
            available = total_staked
        })
    end

    -- Remove vaults using FIFO - O(n) single pass
    local result = remove_vaults_fifo(Stakes[from], quantity)
    Stakes[from] = result.new_vaults

    -- Get message ID for unstake operation
    local _, unstakeMsgId =
        ao.resolve(
            assignment,
            { path = "id", commitments = "all" }
        )

    -- Add to dual indexes (user and time)
    add_to_unstaking_indexes(from, result.operations, assignment.timestamp, unstakeMsgId)

    base = send(base, {
        target = from,
        action = "Unstake-Initiated",
        quantity = quantity,
        ["unstake-operations"] = result.operations,
        ["remaining-staked"] = get_total_staked(from)
    })

    return "ok", log_result(base, "ok", {
        message = "Unstake initiated",
        user = from,
        quantity = quantity,
        operations = result.operations,
        remaining_staked = get_total_staked(from)
    })
end

-- Slash: Admin reduces staked amount (FIFO from oldest stakes)
function slash(base, assignment)
    base = init_results(base)
    local request = assignment.body

    -- Check admin
    local from = request.from

    if from ~= Admin then
        return "error", log_result(base, "error", {
            message = "Only admin can slash",
            admin = Admin,
            caller = from
        })
    end

    -- Get target user to slash
    local target = request.target or request["target-user"]
    if not target then
        return "error", log_result(base, "error", {
            message = "No target specified"
        })
    end

    if not Stakes[target] or #Stakes[target] == 0 then
        return "error", log_result(base, "error", {
            message = "Target has no stake",
            target = target
        })
    end

    -- Normalize slash amount
    local amount = normalize_int(request.quantity or request.amount)
    if not amount or amount <= 0 then
        return "error", log_result(base, "error", {
            message = "Invalid slash amount",
            quantity = request.quantity
        })
    end

    local original_total = get_total_staked(target)

    -- Remove vaults using FIFO - O(n) single pass
    local result = remove_vaults_fifo(Stakes[target], amount)
    Stakes[target] = result.new_vaults

    local actual_slashed = amount
    local new_total = get_total_staked(target)

    base = send(base, {
        target = target,
        action = "Stake-Slashed",
        amount = actual_slashed,
        remaining = new_total,
        ["slashed-vaults"] = result.operations,
        reason = request.reason or "Admin penalty"
    })

    return "ok", log_result(base, "ok", {
        message = "Slash successful",
        target = target,
        slashed = actual_slashed,
        remaining = new_total,
        original = original_total,
        vaults = result.operations
    })
end

-- Auto-finalize expired unstaking records
local function auto_finalize(base, assignment)
    local timestamp = assignment.timestamp
    local processed = 0

    -- Iterate only through timestamps that are ready
    for release_time, entries in pairs(UnstakingByTime) do
        if release_time <= timestamp then
            local user_totals = {}

            for _, entry in ipairs(entries) do
                local user = entry.user
                user_totals[user] = (user_totals[user] or 0) + entry.amount
                processed = processed + 1
            end
            for user, total in pairs(user_totals) do
                base = send(base, {
                    target = TokenProcess,
                    action = "Transfer",
                    recipient = user,
                    quantity = total
                })

                base = send(base, {
                    target = user,
                    action = "Auto-Withdraw-Success",
                    quantity = total,
                    ["release-at"] = release_time
                })
                if Unstaking[user] then
                    Unstaking[user][release_time] = nil
                end
            end
            UnstakingByTime[release_time] = nil
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
