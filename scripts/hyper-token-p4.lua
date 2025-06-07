--- An extension to the `hyper-token.lua` script, for execution with the
--- `lua@5.3a` device. This script adds the ability for an `admin' account to
--- charge a user's account. This is useful for allowing a node operator to
--- collect fees from users, if they are running in a trusted execution
--- environment.
--- 
--- This script must be added as after the `hyper-token.lua` script in the
--- `process-definition`s `script` field.

-- Process an `admin' charge request:
-- 1. Verify the sender's identity.
-- 2. Ensure that the quantity and account are present in the request.
-- 3. Debit the source account.
-- 4. Increment the balance of the recipient account.
function charge(base, assignment)
    ao.event("debug_charge", { "Charge received: ", { assignment = assignment } })
    local admin = base.admin
    local status, res, request = validate_request(base, assignment)
    if status ~= "ok" then
        return status, res
    end

    -- Verify that the request is signed by the admin.
    local committers = ao.get("committers", {"as", "message@1.0", assignment.body})
    ao.event("debug_charge", { "Validating request: ", {
        committers = committers,
        admin = admin
    } })
    if count_common(committers, admin) ~= 1 then
        return "error", base
    end

    -- Ensure that the quantity and account are present in the request.
    if not request.quantity or not request.account then
        ao.event({ "Failure: Quantity or account not found in request.",
            { request = request } })
        base.result = {
            status = "error",
            error = "Quantity or account not found in request."
        }
        return "ok", base
    end

    -- Debit the source. Note: We do not check the source balance here, because
    -- the node is capable of debiting the source at-will -- even it puts the
    -- source into debt. This is important because the node may estimate the
    -- cost of an execution at lower than its actual cost. Subsequently, the
    -- ledger should at least debit the source, even if the source may not
    -- deposit to restore this balance.
    ao.event({ "Debit request validated: ", { assignment = assignment } })
    base.balance = base.balance or {}
    base.balance[request.account] =
        (base.balance[request.account] or 0) - request.quantity

    -- Increment the balance of the recipient account.
    base.balance[request.recipient] =
        (base.balance[request.recipient] or 0) + request.quantity

    ao.event("debug_charge", { "Charge processed: ", { balances = base.balance } })
    return "ok", base
end
