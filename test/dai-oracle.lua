--- ## DAI Mint Oracle
---
--- An on-node oracle for the DAI bridge, executed with the `~lua@5.3a' device.
--- It replaces the previous split design, a genesis-wasm AOS process
--- (`dai-bal-aggregator.lua') driven by an off-chain poller
--- (`sync-mint-oracles.js'). On a `Sync' tick the process reads the bridge
--- deposit and withdrawal events and the DAI Savings Rate directly from Ethereum
--- via the `eth-client@1.0' device, and the DAI price via `relay@1.0', folds them
--- into its state, and dispatches `Set-Balances' batches to the Mint process.
---
--- The operations the AOS process exposed are kept as individual actions, so the
--- process can still be driven by pushed messages if needed:
---
---   Process-Events   apply a batch of decoded bridge events
---   Yield-Update     recompute the yield from a DSR value
---   Configure        set the DAI price (and optionally the mint process)
---   Send-Mint        page balances into Set-Balances batches for the Mint
---   Sync             perform all of the above by reading Ethereum itself
---   Info             read-only snapshot of the reconcilable state
---
--- Every mutating action is restricted to the process `authority', checked
--- against the committers of the incoming request (the `lua@5.3a' equivalent of
--- the AOS `msg.From == Authority' guard).
---
--- State (all fields on `base'):
---   balances        { eth_address = amount }         staked balance per depositor
---   arweave         { eth_address = arweave_id }      linked reward address
---   total_staked    integer                           sum of all balances
---   yield           { dsr = ray, bps = annual_bps }   savings rate and yield
---   price           { usd = string, at = seconds }    latest DAI/USD price
---   last_block      integer                           last bridge block applied
---   mint_nonce      integer                           Set-Balances batch counter

local json = require("json")

--- Configuration. Any value may be overridden by setting the matching field on
--- `base' at spawn time (for staging contracts or an alternate RPC endpoint).
local DEFAULTS = {
    ["rpc-url"] = "https://rpc.ankr.com/eth/74d2b128de93394f12cc690d47fd64995b86447646f83b6e5711fa10217eab9d",
    ["bridge-contract"] = "0x6A1B588B0684dACE1f53C5820111F400B3dbfeBf",
    ["dsr-contract"] = "0x197E90f9FAD81970bA7976f33CbD77088E5D7cf7",
    ["dsr-function"] = "dsr()",
    ["total-staked-function"] = "totalDepositedInPublicPools()",
    ["event-signature"] =
        "UserStaked(uint256,address,uint256,bytes32),UserWithdrawn(uint256,address,uint256,bytes32)",
    ["price-url"] = "https://api.coingecko.com/api/v3/simple/price?ids=dai&vs_currencies=usd&precision=5",
    ["price-field"] = "usd",
    ["deploy-block"] = 0,
    ["seed-total-staked"] = "0",
    ["confirmations"] = 18,
    ["chunk-blocks"] = 2000,
    ["page-size"] = 800,
    ["mint-process"] = "gu33ddMoAYLul3pxd5osxVnqJDOYRM4NnURotLuTC7k"
}

local SECONDS_PER_YEAR = 365 * 24 * 3600

--- Bridge event argument positions. The AOS process reads user, amount and
--- arweave address at these indices. NOTE: eth-client decodes indexed arguments
--- before non-indexed ones, so confirm this order against a real event before
--- relying on it.
local USER_INDEX, AMOUNT_INDEX, ARWEAVE_INDEX = 2, 3, 4

-- Read a configuration value, preferring an override set on `base'.
local function config(base, key)
    local override = base[key]
    if override ~= nil then return override end
    return DEFAULTS[key]
end

-- Add a message to the process outbox.
local function send(base, message)
    table.insert(base.results.outbox, message)
    return base
end

-- Record a status on the result and in the node event log.
local function log_result(base, status, message)
    ao.event("dai_oracle", { status = status, message = message })
    base.results.status = status
    base.results.output = { status = status, message = message }
    return base
end

-- True if the request was committed by one of the process authorities. This is
-- the `lua@5.3a' equivalent of the AOS `msg.From == Authority' check.
local function is_authority(base, assignment)
    local authorities = base.authority or {}
    if type(authorities) ~= "table" then authorities = { authorities } end
    local committers = ao.get("committers", assignment.body) or {}
    for _, committer in ipairs(committers) do
        for _, authority in ipairs(authorities) do
            if committer == authority then return true end
        end
    end
    return false
end

-- Parse a uint256 amount into a non-negative integer, or nil if it is not a
-- valid whole number (rejects negatives, decimals and non-numeric input).
local function parse_amount(value)
    if type(value) ~= "string" or value:match("^%d+$") == nil then
        return nil
    end
    return value
end

-- Decimal-string big-integer helpers. luerl represents Lua numbers as floats,
-- which lose precision above 2^53, so uint256 balances are kept as strings and
-- summed digit by digit.
local function strip0(s)
    s = s:gsub("^0+", "")
    if s == "" then return "0" end
    return s
end

local function uadd(a, b)
    a, b = strip0(a), strip0(b)
    local out, carry, i, j = {}, 0, #a, #b
    while i > 0 or j > 0 or carry > 0 do
        local da = (i > 0) and (a:byte(i) - 48) or 0
        local db = (j > 0) and (b:byte(j) - 48) or 0
        local s = da + db + carry
        out[#out + 1] = string.char(48 + (s % 10))
        carry = (s - (s % 10)) / 10
        i, j = i - 1, j - 1
    end
    return strip0(string.reverse(table.concat(out)))
end

-- a - b, clamped to "0" when b >= a.
local function usub(a, b)
    a, b = strip0(a), strip0(b)
    if #a < #b or (#a == #b and a <= b) then return "0" end
    local out, borrow, i, j = {}, 0, #a, #b
    while i > 0 do
        local da = a:byte(i) - 48 - borrow
        local db = (j > 0) and (b:byte(j) - 48) or 0
        if da < db then da = da + 10; borrow = 1 else borrow = 0 end
        out[#out + 1] = string.char(48 + (da - db))
        i, j = i - 1, j - 1
    end
    return strip0(string.reverse(table.concat(out)))
end

-- Encode 32 raw bytes (given as a hex string) as an Arweave address (base64url).
local BASE64URL = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
local function bytes32_to_arweave(hex)
    hex = hex:gsub("^0x", "")
    local bytes = {}
    for i = 1, #hex, 2 do
        bytes[#bytes + 1] = string.char(tonumber(hex:sub(i, i + 1), 16))
    end
    local raw = table.concat(bytes)
    local out = {}
    for i = 1, #raw, 3 do
        local b1, b2, b3 = raw:byte(i), raw:byte(i + 1), raw:byte(i + 2)
        out[#out + 1] = BASE64URL:sub(b1 // 4 + 1, b1 // 4 + 1)
        local n2 = (b1 % 4) * 16 + (b2 and b2 // 16 or 0)
        out[#out + 1] = BASE64URL:sub(n2 + 1, n2 + 1)
        if b2 then
            local n3 = (b2 % 16) * 4 + (b3 and b3 // 64 or 0)
            out[#out + 1] = BASE64URL:sub(n3 + 1, n3 + 1)
            if b3 then out[#out + 1] = BASE64URL:sub(b3 % 64 + 1, b3 % 64 + 1) end
        end
    end
    return table.concat(out)
end

--- Ethereum reads, each performed through the `eth-client@1.0' device with
--- `ao.resolve'. These are the only functions that touch the wire format.

-- Call a uint256 view function, returning its value as a decimal string.
local function eth_call_uint(base, contract, fn, block)
    local status, res = ao.resolve({
        device = "eth-client@1.0",
        path = "call",
        ["rpc-url"] = config(base, "rpc-url"),
        to = contract,
        ["function"] = fn,
        returns = "uint256",
        block = block or "latest"
    })
    if status ~= "ok" or not res or not res.body then return nil end
    return res.body.data
end

-- The current head block number.
local function eth_head_block(base)
    local status, res = ao.resolve({
        device = "eth-client@1.0",
        path = "rpc",
        ["rpc-url"] = config(base, "rpc-url"),
        ["rpc-method"] = "eth_blockNumber",
        ["rpc-params"] = "[]"
    })
    if status ~= "ok" or not res or not res.body then return nil end
    -- body is a 0x-prefixed hex string; base-16 tonumber rejects the 0x prefix
    local hex = res.body:gsub("^0x", "")
    return tonumber(hex, 16)
end

-- Fetch the bridge deposit and withdrawal events over a block range. Returns the
-- decoded event array, or nil on failure.
local function eth_events(base, from_block, to_block)
    local status, res = ao.resolve({
        device = "eth-client@1.0",
        path = "get-data",
        ["rpc-url"] = config(base, "rpc-url"),
        contract = config(base, "bridge-contract"),
        ["event-signature"] = config(base, "event-signature"),
        mode = "range",
        ["from-block"] = string.format("0x%x", from_block),
        ["to-block"] = string.format("0x%x", to_block)
    })
    if status ~= "ok" or not res or not res.body then return nil end
    local ok, events = pcall(json.decode, res.body.data)
    if not ok then return nil end
    return events
end

-- Fetch the DAI price in USD through relay@1.0, extracting the price field.
local function fetch_price(base)
    local status, res = ao.resolve({
        path = "/~relay@1.0/call?relay-path=" .. config(base, "price-url")
    })
    if status ~= "ok" or not res then return nil end
    local body = type(res) == "table" and (res.body or res.data) or res
    if type(body) ~= "string" then return nil end
    return body:match('"' .. config(base, "price-field") .. '"%s*:%s*([%d%.]+)')
end

--- Core state transitions, shared by the pushed-message actions and Sync.

-- Apply a batch of decoded bridge events to the balances. UserStaked adds to a
-- depositor's balance and records their Arweave address; UserWithdrawn subtracts,
-- clamped so a balance never goes negative. Returns the number applied.
local function apply_events(base, events)
    local applied = 0
    for _, event in ipairs(events) do
        local args = event.args
        local amount = args and parse_amount(args[AMOUNT_INDEX])
        if event.removed or not amount or not args[USER_INDEX] then
            -- skip reverted or malformed events
        elseif event.event == "UserStaked" and args[ARWEAVE_INDEX] then
            local user = string.lower(args[USER_INDEX])
            base.balances[user] = uadd(base.balances[user] or "0", amount)
            base.total_staked = uadd(base.total_staked, amount)
            base.arweave[user] = bytes32_to_arweave(args[ARWEAVE_INDEX])
            applied = applied + 1
        elseif event.event == "UserWithdrawn" then
            local user = string.lower(args[USER_INDEX])
            -- per-user balance clamps at zero; total_staked subtracts the full
            -- amount so it matches the bridge total even when seeded from a
            -- baseline without every depositor's balance.
            base.balances[user] = usub(base.balances[user] or "0", amount)
            base.total_staked = usub(base.total_staked, amount)
            applied = applied + 1
        end
    end
    return applied
end

-- Convert a DAI Savings Rate ray (1e27 fixed point, per-second compounding) into
-- an annualized yield in basis points.
local function dsr_to_bps(ray)
    local rate = tonumber(ray)
    if not rate or rate <= 0 then return nil end
    local apy = (rate / 1e27) ^ SECONDS_PER_YEAR - 1
    return math.floor(apy * 10000 + 0.5)
end

-- Page the current balances into CSV `Set-Balances' batches and queue them to the
-- Mint process. Returns the number of batches sent.
local function dispatch_mint(base)
    local rows = {}
    for user, balance in pairs(base.balances) do
        if base.arweave[user] then
            rows[#rows + 1] = table.concat({ user, balance, base.arweave[user] }, ",")
        end
    end
    if #rows == 0 then return 0 end

    local page_size = config(base, "page-size")
    local pages = {}
    for i = 1, #rows, page_size do
        pages[#pages + 1] = table.concat(rows, "\n", i, math.min(i + page_size - 1, #rows))
    end

    base.mint_nonce = base.mint_nonce + 1
    local price = base.price.usd and math.floor(tonumber(base.price.usd) * 10000 + 0.5) or 0
    for index, page in ipairs(pages) do
        send(base, {
            target = config(base, "mint-process"),
            action = "Set-Balances",
            format = "CSV",
            price = tostring(price),
            currency = "USD",
            yield = tostring(base.yield.bps),
            nonce = tostring(base.mint_nonce),
            index = tostring(index),
            total = tostring(#pages),
            data = page
        })
    end
    return #pages
end

--- Lifecycle and dispatch.

-- Ensure the state fields exist and reset the per-slot result.
local function ensure_initialized(base)
    base.results = base.results or {}
    base.results.outbox = {}
    base.results.status = "OK"
    base.balances = base.balances or {}
    base.arweave = base.arweave or {}
    base.total_staked = base.total_staked or config(base, "seed-total-staked")
    base.yield = base.yield or { dsr = "0", bps = 0 }
    base.price = base.price or { usd = nil, at = 0 }
    base.last_block = base.last_block or config(base, "deploy-block")
    base.mint_nonce = base.mint_nonce or 0
    return base
end

-- Guard used by every mutating action.
local function unauthorized(base, assignment)
    if is_authority(base, assignment) then return false end
    log_result(base, "error", "not authorized")
    return true
end

--- Actions.

function process_events(base, assignment)
    if unauthorized(base, assignment) then return "ok", base end
    local ok, events = pcall(json.decode, assignment.body.data or "[]")
    if not ok then return "ok", log_result(base, "error", "invalid events payload") end
    local applied = apply_events(base, events)
    return "ok", log_result(base, "ok", "applied " .. applied .. " events")
end

function yield_update(base, assignment)
    if unauthorized(base, assignment) then return "ok", base end
    local bps = dsr_to_bps(assignment.body.data)
    if not bps then return "ok", log_result(base, "error", "invalid dsr value") end
    base.yield = { dsr = assignment.body.data, bps = bps }
    return "ok", log_result(base, "ok", "yield " .. bps .. " bps")
end

function configure(base, assignment)
    if unauthorized(base, assignment) then return "ok", base end
    if assignment.body["mint-process"] then
        base["mint-process"] = assignment.body["mint-process"]
    end
    local usd = assignment.body.price or fetch_price(base)
    if usd then base.price = { usd = usd, at = os.time() } end
    return "ok", log_result(base, "ok", "configured")
end

function send_mint(base, assignment)
    if unauthorized(base, assignment) then return "ok", base end
    local batches = dispatch_mint(base)
    return "ok", log_result(base, "ok", "sent " .. batches .. " mint batches")
end

-- The on-node driver, and what removes the JS poller: advance the bridge sync by
-- one block chunk, and once caught up to the safe head, refresh the DSR-derived
-- yield and the DAI price. Driven repeatedly by cron, it catches up incrementally
-- and then stays at the head. Minting stays a separate action (`Send-Mint').
function sync(base, assignment)
    if unauthorized(base, assignment) then return "ok", base end

    local head = eth_head_block(base)
    if not head then return "ok", log_result(base, "error", "could not read head block") end
    local safe = head - config(base, "confirmations")

    local applied = 0
    local from = base.last_block + 1
    if from <= safe then
        local to = math.min(from + config(base, "chunk-blocks") - 1, safe)
        local events = eth_events(base, from, to)
        if events then applied = apply_events(base, events) end
        base.last_block = to
    end

    if base.last_block >= safe then
        local ray = eth_call_uint(base, config(base, "dsr-contract"), config(base, "dsr-function"), string.format("0x%x", safe))
        if ray then base.yield = { dsr = ray, bps = dsr_to_bps(ray) or base.yield.bps } end
        local usd = fetch_price(base)
        if usd then base.price = { usd = usd, at = os.time() } end
    end

    return "ok", log_result(base, "ok",
        "synced to " .. base.last_block .. " (+" .. applied .. " events)")
end

-- Read-only snapshot, in the same shape the reconciliation test and dashboards
-- consume.
function info(base, assignment)
    base.results.output = {
        ["total-staked"] = base.total_staked,
        ["yield-bps"] = tostring(base.yield.bps),
        ["yield-dsr"] = base.yield.dsr,
        ["last-block"] = base.last_block,
        ["dai-usd"] = base.price.usd or "0",
        ["mint-nonce"] = base.mint_nonce
    }
    return "ok", base
end

-- Entry point: dispatch on the request's action.
function compute(base, assignment)
    base = ensure_initialized(base)
    local action = string.lower(assignment.body.action or "")
    ao.event("dai_oracle", { "compute", { action = action, slot = assignment.slot } })

    if action == "sync" then
        return sync(base, assignment)
    elseif action == "process-events" then
        return process_events(base, assignment)
    elseif action == "yield-update" then
        return yield_update(base, assignment)
    elseif action == "configure" then
        return configure(base, assignment)
    elseif action == "send-mint" then
        return send_mint(base, assignment)
    elseif action == "info" then
        return info(base, assignment)
    else
        return "ok", base
    end
end
