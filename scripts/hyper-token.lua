--- ## HyperTokens: Networks of fungible, parallel ledgers.
--- # Version: 0.1.
--- 
--- An AO token standard implementation, with support for sub-ledger networks,
--- executed with the `~lua@5.3` device. This script supports both the base token
--- blueprint's 'active' keys, as well as the mainnet sub-ledger API.
--- 
--- Data access actions (e.g. `balance', `info', `total-supply') are not
--- implemented due to their redundancy. Instead, the full state of the process
--- is available via the AO-Core HTTP API, including all metadata and individual
--- account balances.
--- 
--- A full description of the hyper-token standard can be found in the
--- `token.md` file in this directory. The remainder of this module document
--- provides a breif overview of its design, and focuses on its implementation
--- details.
--- 
--- ## Design and Implementation
--- 
--- If running as a `root' token (indicated by the absence of a `token' field),
--- the `balance' field should be initialized to a table of balances for the
--- token during spawning. The `ledgers' field holds the state of a sub-ledger's
--- own balances with other ledgers. This field is always initialized to a message
--- of zero balances during the evaluation of the first assignment of the process.
--- When the token receives a `credit-notice' message, it will interpret it as a
--- deposit from the sending ledger and update its record of its own balance with
--- the sending ledger.
--- 
--- Atop the standard token transfer messages, the sub-ledger API allows for
--- `transfer' messages to specify a `route' field, which is a list of ledger
--- IDs that the transfer should be routed through in order to reach a
--- recipient on a different ledger. At each `hop' in the route, the recipient
--- ledger validates whether it trusts the sending ledger and whether it knows
--- how to route to the next hop. If the recipient ledger does not trust the
--- sending ledger, it will terminate the route with a `route-termination'
--- message. If the recipient ledger knows how to route to the next hop, it
--- will create a new `transfer' message to the next hop, with the first
--- ledger from the route removed and the remainder of the route and recipient
--- and quantity to transfer forwarded along with the message.
--- 
--- There are three security checks performed on incoming messages, above the
--- standard balance transfer checks:
--- 
--- 1. Assignments are evaluated against the `assess/assignment' message, if
---    present. If not, the assignment is evaluated against the process's own
---    scheduler address.
--- 
--- 2. If the message does not originate from an end-user (indicated by the 
---    presence of a `from-process' field), the message is evaluated against
---    the `assess/request' message, if present. If not, the message is 
---    evaluated against the `authority' field. The `authority' field may 
---    contain a list of addresses or messages that are considered to be
---    authorities.
--- 
--- 3. `credit-notice' messages that do not originate from a sub-ledger's
---    `token' are evaluated for parity of source code with the receiving
---     ledger. This is achieved by comparing the `from-base' field of the
---     credit-notice message with `process/id&commitments=none' on the receiving
---     ledger.

--- Utility functions:

-- Add a message to the outbox of the given base.
local function send(base, message)
    table.insert(base.results.outbox, message)
    return base
end

-- Add a log message to the results of the given base.
local function log_result(base, status, message)
    ao.event("token_log", {"Token action log: ", {
        status = status,
        message = message
    }})
    base.results = base.results or {}
    base.results.status = status
    
    if base.results.log then
        table.insert(base.results.log, message)
    else
        base.results.log = { message }
    end

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

-- Count the number of elements in `a' that are also in `b'.
function count_common(a, b)
    -- Normalize both arguments to tables.
    if type(a) ~= "table" then a = { a } end
    if type(b) ~= "table" then b = { b } end

    local count = 0
    for _, v in ipairs(a) do
        for _, w in ipairs(b) do
            if v == w then
                count = count + 1
            end
        end
    end

    return count
end

-- Normalize an argument to a table if it is not already a table.
local function normalize_table(value)
    if type(value) ~= "table" then
        value = { value }
    end
    return value
end

--- Security verification functions:

-- Enforce that a given list satisfies `hyper-token's grammar constraints.
-- This function is used to check that the `authority' and `scheduler' fields
-- satisfy the constraints specified by the `authority[-*]' and `scheduler[-*]'
-- fields. The supported grammar constraints are:
-- - `X`: A list of `X`s that are admissible to match in the subject.
-- - `X-match`: A count of the number of `X`s that must be present in the subject.
--              Default: Length of `X`.
-- - `X-required`: A list of `X`s that must be present in the subject.
--                Default: `{}`.
local function satisfies_list_constraints(subject, all, required, match)
    -- Normalize the fields to tables, aside from the match count.
    subject = normalize_table(subject)
    all = normalize_table(all)
    required = normalize_table(required or {})
    -- Normalize the match count.
    match = match or #all

    -- Check that the subject satisfies the grammar's constraints.
    -- 1. The subject must have at least `match' elements in common with `all'.
    -- 2. The subject must contain all elements in `required'.
    local count = count_common(subject, all)
    local required_count = count_common(required, subject)
    return (count >= match) and (required_count == #required)
end

-- Ensure that a message satisfies the grammar's constraints, or the assessment
-- message returns true, if present.
local function satisfies_constraints(message, assess, all, required, match)
    -- If the assessment message is present, run it against the message.
    if assess then
        ao.event({ "Running assessment message against request." },
            { assessment = assess, message = message })
        local status, result = ao.resolve(assess, message)
        if (status == "ok") and (result == true) then
            ao.event({ "Assessment of request passed." }, {
                message = message,
                status = status,
                result = result
            })
            return true
        else
            ao.event({ "Assessment of request failed.", {
                message = message,
                status = status,
                result = result
            }})
            return false
        end
    end

    -- If the assessment message is not present, check the signatures against
    -- the requirements list and specifiers.
    local satisfies_auth = satisfies_list_constraints(
        ao.get("committers", message),
        all,
        required,
        match
    )

    ao.event({ "Constraint satisfaction results", {
        result = satisfies_auth,
        message = message,
        all_admissible = all,
        required = required,
        required_count = match
    }})

    return satisfies_auth
end

-- Ensure that the `authority' field satisfies the `authority[-*]' constraints
-- (as supported by `satisfies_constraints') or that the assessment message
-- returns true.
local function is_trusted_compute(base, assignment)
    return satisfies_constraints(
        assignment.body,
        (base.assess or {})["authority"],
        base.authority,
        base["authority-required"],
        base["authority-match"]
    )
end

-- Ensure that the assignment is trusted. Either by running the assessment
-- process, or by checking the signature against the process's own scheduler
-- address and those it explicitly trusts.
local function is_trusted_assignment(base, assignment)
    return satisfies_constraints(
        assignment,
        (base.assess or {})["scheduler"],
        base.scheduler,
        base["scheduler-required"],
        base["scheduler-match"]
    )
end

-- Determine if the ledger indicated by `base` is the root ledger.
local function is_root(base)
    return base.token == nil
end

-- Ensure that a credit-notice from another ledger is admissible. It must either
-- be from our own root ledger, or from a sub-ledger that is precisely the same
-- as our own.
local function validate_new_peer_ledger(base, request)
    ao.event({ "Validating peer ledger: ", { request = request } })

    -- Check if the request is from the root ledger.
    if is_root(base) or (base.token == request.from) then
        ao.event({ "Peer is parent token. Accepting." }, {
            request = request
        })
        return true
    end

    -- Calculate the expected base ID from the process's own `process` message,
    -- modified to remove the `authority' and `scheduler' fields.
    -- This ensures that the process we are receiving the `credit-notice` from
    -- has the same structure as our own process.
    ao.event({ "Calculating expected `base` from self", { base = base } })
    local status, proc, expected
    status, proc = ao.resolve({"as", "message@1.0", base}, "process")
    -- Reset the `authority' and `scheduler' fields to nil, to ensure that the
    -- `base` message matches the structure created by `~push@1.0`.
    proc.authority = nil
    proc.scheduler = nil
    status, expected =
        ao.resolve(
            proc,
            { path = "id", commitments = "none" }
        )
    ao.event({ "Expected `from-base`", { status = status, expected = expected } })
    -- Check if the `from-base' field is present in the assignment.
    if not request["from-base"] then
        ao.event({ "`from-base` field not found in message", {
            request = request
        }})
        return false
    end

    -- Check if the `from-base' field matches the expected ID.
    local base_matches = request["from-base"] == expected

    if not base_matches then
        ao.event("debug_base", { "Peer registration messages do not match", {
            expected_base = expected,
            received_base = request["from-base"],
            process = proc,
            request = request
        }})
        return false
    end

    -- Check that the `from-authority' and `from-scheduler' fields match the
    -- expected values, to the degree specified by the `authority-match' and
    -- `scheduler-match' fields. Additionally, the `authority-required' and
    -- `scheduler-required' fields may be present in the base, the members of
    -- which must be present in the `from-authority' and `from-scheduler' fields
    -- respectively.
    local authority_matches = satisfies_list_constraints(
        request["from-authority"],
        base.authority,
        base["authority-required"],
        base["authority-match"]
    )
    local scheduler_matches = satisfies_list_constraints(
        request["from-scheduler"],
        base.scheduler,
        base["scheduler-required"],
        base["scheduler-match"]
    )
    if (not authority_matches) or (not scheduler_matches) then
        ao.event("debug_base", { "Peer security parameters do not match", {
            expected_authority = base.authority,
            received_authority = request["from-authority"],
            expected_scheduler = base.scheduler,
            received_scheduler = request["from-scheduler"],
            scheduler_matches = scheduler_matches,
            authority_matches = authority_matches,
            request = request
        }})
        return false
    end

    ao.event("Peer registration messages matches. Accepting.")

    return true
end

-- Register a new peer ledger, if the `from-base' field matches our own.
local function register_peer(base, request)
    -- Validate the registering ledger
    if not validate_new_peer_ledger(base, request) then
        base.results = {
            status = "error",
            error = "Ledger registration failed."
        }
        return "error", base
    end
    
    -- Add to known ledgers
    base.ledgers[request.from] = base.ledgers[request.from] or 0

    return "ok", base
end

-- Determine if a request is from a known ledger. Makes no assessment of whether
-- a request is otherwise trustworthy.
local function is_from_trusted_ledger(base, request)
    -- We always trust the root ledger.
    if request.from == base["token"] then
        return true, base
    end

    -- We trust any ledger that is already registered in the `ledgers' map.
    if base.ledgers and (base.ledgers[request.from] ~= nil) then
        return true, base
    end

    -- Validate whether the request is from a new peer ledger.
    local status
    status, base = register_peer(base, request)
    if status ~= "ok" then
        return false, base
    end

    return true, base
end

-- Ensure that the ledger is initialized.
local function ensure_initialized(base, assignment)
    -- Ensure that the base has a `result' field before we try to register.
    base.results = base.results or {}
    base.results.outbox = {}
    base.results.status = "OK"
    -- If the ledger is not being initialized, we can skip the rest of the
    -- function.
    if assignment.slot ~= 0 then
        return "ok", base
    end
    base.balance = base.balance or {}

    -- Ensure that the `ledgers' map is initialized: present and empty.
    base.ledgers = base.ledgers or {}
    ao.event({ "Ledgers before initialization: ", base.ledgers })

    for _, ledger in ipairs(base.ledgers) do
        base.ledgers[ledger] = 0
    end
    ao.event({ "Ledgers after initialization: ", base.ledgers })

    if not base.token then
        ao.event({ "Ledger has no source token. Skipping registration." })
        return "ok", base
    end

    ao.event({ "Registering self with known token ledgers: ", {
        ledgers = base.ledgers
    }})

    for _, ledger in ipairs(base.ledgers) do
        -- Insert the register result into the base.
        base.results = send(base, {
            action = "Register",
            target = ledger
        })
    end

    return "ok", base
end

-- Verify that an assignment has not been processed and that the request is
-- valid. If it is, update the `from' field to the address that signed the
-- request.
function validate_request(incoming_base, assignment)
    -- Ensure that the ledger is initialized.
    local status, base = ensure_initialized(incoming_base, assignment)
    if status ~= "ok" then
        return "error", log_result(incoming_base, "error", {
            message = "Ledger initialization failed.",
            assignment = assignment,
            status = status,
        })
    end

    -- First, ensure that the message has not already been processed.
    ao.event("Deduplicating message.", {
        ["history-length"] = #(base.dedup or {})
    })

    status, base =
        ao.resolve(
            incoming_base,
            {"as",
                "dedup@1.0",
                {
                    path = "compute",
                    ["subject-key"] = "body",
                    body = assignment.body
                }
            }
        )

    -- Set the device back to `process@1.0`.
    base.device = "process@1.0"
    if status ~= "ok" then
        return "error", log_result(base, "error", {
            message = "Deduplication failure.",
            assignment = assignment,
            status = status,
            incoming_base = incoming_base,
            resulting_base = base
        })
    end

    -- Next, ensure that the assignment is trusted.
    local trusted, details = is_trusted_assignment(base, assignment)
    if not trusted then
        return "error", log_result(base, "error", {
            message = "Assignment is not trusted.",
            details = details
        })
    end

    if assignment.body["from-process"] then
        -- If the request is proxied, we need to check that the source
        -- computation is trusted.
        trusted, details = is_trusted_compute(base, assignment)
        if not trusted then
            return "error", log_result(base, "error", {
                message = "Message computation is not trusted.",
                details = details
            })
        end
        assignment.body.from = assignment.body["from-process"]
        return "ok", base, assignment.body
    else
        -- If the request is not proxied, we set the `from' field to the address
        -- that signed the request.
        local committers = ao.get("committers", assignment.body)
        if #committers == 0 then
            return "error", log_result(base, "error", {
                message = "No request signers found."
            })
        end
        
        -- Only accept single-signed requests to avoid ambiguity
        if #committers > 1 then
            return "error", log_result(base, "error", {
                message = "Multiple signers detected, making sender ambiguous. " ..
                    "Only singly-signed requests are supported for end-user " ..
                    "requests (those that do not originate from another " ..
                    "computation)."
            })
        end
        
        assignment.body.from = committers[1]
        return "ok", base, assignment.body
    end
end

-- Ensure that the source has the required funds, then debit the source. Takes
-- an origin, which can be used to identify the reason for the debit in logging.
-- Returns error if the source balance is not viable, or `ok` and the updated
-- base state if the debit is successful. Does not credit any funds.
local function debit_balance(base, request)
    local source = request.from

    ao.event({ "Attempting to deduct balance.", {
        request = request,
        balances = base.balance or {}
    }})

    -- Ensure that the `source' and `quantity' fields are present in the request.
    if not source or not request.quantity then
        return "error", log_result(base, "error", {
            message = "Fund source or quantity not found in request.",
        })
    end

    -- Normalize the quantity value.
    request.quantity = normalize_int(request.quantity)
    if not request.quantity then
        ao.event({ "Invalid quantity value: ", { quantity = request.quantity } })
        base.results = {
            status = "error",
            error = "Invalid quantity value.",
            quantity = request.quantity
        }
        return "error", base
    end

    -- Ensure that the source has the required funds.
    -- Check 1: The source balance is present in the ledger.
    local source_balance = base.balance[source]

    if not source_balance then
        return "error", log_result(base, "error", {
            message = "Source balance not found.",
            from = source,
            quantity = request.quantity,
            ["is-root"] = is_root(base)
        })
    end

    -- Check 2: The source balance is a valid number.
    if type(source_balance) ~= "number" then
        return "error", log_result(base, "error", {
            message = "Source balance is not a number.",
            balance = source_balance
        })
    end

    -- Check 3: Ensure that the quantity to deduct is a non-negative number.
    if request.quantity < 0 then
        return "error", log_result(base, "error", {
            message = "Quantity to deduct is negative.",
            quantity = request.quantity
        })
    end

    -- Check 4: Ensure that the source has enough funds.
    if source_balance < request.quantity then
        return "error", log_result(base, "error", {
            message = "Insufficient funds.",
            from = source,
            quantity = request.quantity,
            balance = source_balance
        })
    end

    ao.event({ "Deducting funds:", { request = request } })
    base.balance[source] = source_balance - request.quantity
    ao.event({ "Balances after deduction:",
        { balances = base.balance, ledgers = base.ledgers } }
    )
    return "ok", base
end

-- Transfer the specified amount from the given account to the given account,
-- optionally routing to a different sub-ledger if necessary.
-- There are four differing types of transfer requests. They have the following
-- semantics:
-- Balance == owed to X. Credit == Owed to subject by X.

-- User on root -> User on sub-ledger:
-- Xfer in: Root = Dec User balance, Inc Sub-ledger balance
-- C-N in: Sub-ledger = Inc User balance

-- User on sub-ledgerA -> User on sub-ledgerB:
-- Xfer in: Sub-ledgerA = Dec User balance
-- C-N in: Sub-ledgerB = Inc User balance

-- User on sub-ledgerB -> User on sub-ledgerA:
-- Xfer in: Sub-ledgerB = Dec user balance
-- C-N in: Sub-ledgerA = Inc user balance

-- User on A->B->C:
-- Xfer in: A = Dec User balance
-- C-N in: B = 
-- C-N in: C = Inc User balance

-- User on sub-ledger -> User on root:
-- Xfer in: Sub-ledger = Dec User balance
-- C-N in: Root = Inc User balance, Dec Sub-ledger balance
function transfer(base, assignment)
    ao.event({ "Transfer request received", { assignment = assignment } })
    -- Verify the security of the request.
    local status, request
    status, base, request = validate_request(base, assignment)
    if status ~= "ok" or not request then
        return "ok", base
    end

    -- Ensure that the recipient is known.
    if not request.recipient then
        return log_result(base, "error", {
            message = "Transfer request has no recipient."
        })
    end

    -- Normalize the quantity value.
    local quantity = normalize_int(request.quantity)
    if not quantity then
        return log_result(base, "error", {
            message = "Invalid quantity value.",
            quantity = request.quantity
        })
    end

    -- Ensure that the source has the required funds. If they do, debit them.
    local debit_status
    debit_status, base = debit_balance(base, request)
    if debit_status ~= "ok" or base == nil then
        return "ok", base
    end

    if is_root(base) or not request.route then
        -- We are the root ledger, or the user is sending tokens directly to
        -- another user. We credit the recipient's balance, or the sub-ledger's
        -- balance if the request has a `route' key.
        local direct_recipient = request.route or request.recipient
        base.balance[direct_recipient] =
            (base.balance[direct_recipient] or 0) + quantity
        base = send(base, {
            action = "Credit-Notice",
            target = direct_recipient,
            recipient = request.recipient,
            quantity = quantity,
            sender = request.from
        })
        return log_result(base, "ok", {
            message = "Direct or root transfer processed successfully.",
            from_user = request.from,
            to = direct_recipient,
            explicit_recipient = request.recipient,
            quantity = quantity
        })
    end

    if request.route == base.token then
        -- The user is returning tokens to the root ledger, so we send a
        -- transfer to the root ledger.
        base = send(base, {
            action = "Transfer",
            target = base.token,
            recipient = request.recipient,
            quantity = request.quantity
        })
        return log_result(base, "ok", {
            message = "Ledger-root transfer processed successfully.",
            from_user = request.from,
            to_ledger = base.token,
            to_user = request.recipient,
            quantity = request.quantity
        })
    end

    -- We are not the root ledger, and the request has a `route` key.
    -- Subsequently, the target must be another ledger so we dispatch a
    -- credit-notice to the peer ledger. The peer will increment the balance of
    -- the recipient.
    base = send(base, {
        action = "Credit-Notice",
        target = request.route,
        recipient = request.recipient,
        quantity = quantity,
        sender = request.from
    })

    return log_result(base, "ok", {
        message = "Ledger-ledger transfer processed successfully.",
        from_user = request.from,
        to_ledger = request.route,
        to_user = request.recipient,
        quantity = quantity
    })
end

-- Process credit notices from other ledgers.
_G["credit-notice"] = function (base, assignment)
    ao.event({ "Credit-Notice received", { assignment = assignment } })

    -- Verify the security of the request.
    local status, request
    status, base, request = validate_request(base, assignment)
    if status ~= "ok" or not request then
        return "ok", base
    end

    if is_root(base) then
        -- The root ledger will not process credit notices.
        return log_result(base, "error", {
            message = "Credit-Notice to root ledger ignored."
        })
    end

    -- Ensure that the recipient is known.
    if not request.recipient then
        return log_result(base, "error", {
            message = "Credit-Notice request has no recipient."
        })
    end

    -- Normalize the quantity value.
    local quantity = normalize_int(request.quantity)
    if not quantity then
        return log_result(base, "error", {
            message = "Invalid quantity value.",
            quantity = request.quantity
        })
    end

    -- Ensure that the sender is a trusted ledger peer.
    local trusted
    trusted, base = is_from_trusted_ledger(base, request)
    if not trusted then
        return log_result(base, "error", {
            message = "Credit-Notice not from a trusted peer ledger."
        })
    end

    -- Credit the recipient's balance.
    base.balance[request.recipient] =
        (base.balance[request.recipient] or 0) + quantity
    
    return "ok", log_result(base, "ok", {
        message = "Credit-Notice processed successfully.",
        from_ledger = request.from,
        to_ledger = request.sender,
        to_user = request.recipient,
        quantity = quantity,
        balance = base.balance[request.recipient]
    })
end

-- Process registration requests from other ledgers.
function register(raw_base, assignment)
    ao.event({ "Register request received", { assignment = assignment } })
    
    local status, base, request = validate_request(raw_base, assignment)
    if (status ~= "ok") or (type(request) ~= "table") then
        return "ok", base
    end

    if base.ledgers[request.from] then
        ao.event({ "Ledger already registered. Ignoring registration request." })
        base.results = {
            message = "Ledger already registered."
        }
        return "ok", base
    end

    -- Validate the registering ledger
    status, base = register_peer(base, request)
    if status ~= "ok" then
        return status, base
    end
    
    -- Send a reciprocal registration request to the remote ledger.
    base = send(base, {
        target = request.from,
        action = "register"
    })
    
    return "ok", base
end

-- Register ourselves with a remote ledger, at the request of a user or another
-- ledger.
_G["register-remote"] = function (raw_base, assignment)
    -- Validate the request.
    local status, base, request = validate_request(raw_base, assignment)
    if (status ~= "ok") or (type(request) ~= "table") then
        return "ok", base
    end

    base = log_result(base, "ok", {
        message = "Register-Remote request received.",
        peer = request.peer
    })

    -- Send a registration request to the remote ledger. Our request is simply
    -- a `Register' message, as the recipient will be assessing our unsigned
    -- process ID in order to validate that we are an appropriate peer. This is
    -- added by our `push-device`, so no further action is required on our part.
    base = send(base, {
        target = request.peer,
        action = "register"
    })

    return "ok", base
end

--- Index function, called by the `~process@1.0` device for scheduled messages.
--- We route any `action' to the appropriate function based on the request path.
function compute(base, assignment)
    ao.event({ "compute called",
        { balance = base.balance, ledgers = base.ledgers } })

    assignment.body.action = string.lower(assignment.body.action or "")
    
    if assignment.body.action == "credit-notice" then
        return _G["credit-notice"](base, assignment)
    elseif assignment.body.action == "transfer" then
        return transfer(base, assignment)
    elseif assignment.body.action == "register" then
        return register(base, assignment)
    elseif assignment.body.action == "register-remote" then
        return _G["register-remote"](base, assignment)
    else
        -- Handle unknown `action' values.
        base.results = {
            status = "error",
            error = "Unknown action: " .. assignment.body.action,
            request = assignment
        }
        ao.event({ "Unknown action", { action = assignment.body.action } })
        return "ok", base
    end
end