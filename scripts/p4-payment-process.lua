--- A ledger that allows account balances to be debited and credited by a
--- specified address.

-- Check if the request is a valid debit/credit request by checking if one of
-- the committers is the operator.
local function is_valid_request(base, assignment)
    -- First, validate that the assignment is signed by the scheduler.
    local scheduler = base.scheduler
    local status, res = ao.resolve(assignment, "committers")
    ao.event({
        "assignment committers resp:",
        { status = status, res = res, scheduler = scheduler }
    })
    
    if status ~= "ok" then
        return false
    end

    local valid = false
    for _, committer in ipairs(res) do
        if committer == scheduler then
            valid = true
        end
    end

    if not valid then
        return false
    end

    -- Next, validate that the request is signed by the operator.
    local operator = base.operator
    status, res = ao.resolve(assignment.body, "committers")
    ao.event({
        "request committers resp:",
        { status = status, res = res, operator = operator }
    })

    if status ~= "ok" then
        return false
    end

    for _, committer in ipairs(res) do
        if committer == operator then
            return true
        end
    end

    return false
end

-- Debit the specified account by the given amount.
function debit(base, assignment)
    ao.event({ "process debit starting", { assignment = assignment } })
    if not is_valid_request(base, assignment) then
        base.result = { status = "error", error = "Operator signature required." }
        ao.event({ "debit error", base.result })
        return "ok", base
    end
    ao.event({ "process debit valid", { assignment = assignment } })
    base.balance = base.balance or {}
    base.balance[assignment.body.account] =
        (base.balance[assignment.body.account] or 0) - assignment.body.quantity
    
    ao.event({ "process debit success", { balances = base.balance } })
    return "ok", base
end

-- Credit the specified account by the given amount.
_G["credit-notice"] = function (base, assignment)
    ao.event({ "credit-notice", { assignment = assignment }, { balances = base.balance } })
    if not is_valid_request(base, assignment) then
        base.result = { status = "error", error = "Operator signature required." }
        return "ok", base
    end
    ao.event({ "is valid", { req = assignment.body } })
    base.balance = base.balance or {}
    base.balance[assignment.body.recipient] =
        (base.balance[assignment.body.recipient] or 0) + assignment.body.quantity
    ao.event({ "credit", { ["new balances"] = base.balance } })
    return "ok", base
end

--- Index function, called by the `~process@1.0` device for scheduled messages.
--- We route each to the appropriate function based on the request path.
function compute(base, assignment, opts)
    ao.event({ "compute", { assignment = assignment }, { balances = base.balance } })
    if assignment.body.path == "debit" then
        return debit(base, assignment.body)
    elseif assignment.body.path == "credit-notice" then
        return _G["credit-notice"](base, assignment.body)
    elseif assignment.body.path == "balance" then
        return balance(base, assignment.body)
    elseif assignment.slot == 0 then
        base.balance = base.balance or {}
        return "ok", base
    end
end
