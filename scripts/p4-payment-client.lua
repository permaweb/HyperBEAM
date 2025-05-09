--- A simple script that can be used as a `~p4@1.0` ledger device, marshalling
--- requests to a local process.

-- Find the user's balance in the current ledger state.
function balance(base, request)
    local status, res = ao.resolve({
        path =
            base["ledger-path"]
            .. "/now/balance/"
            .. request["target"]
    })
    ao.event({ "client received balance response", 
        { status = status, res = res, target = request["target"] } }
    )
    -- If the balance request fails (most likely because the user has no balance),
    -- return a balance of 0.
    if status ~= "ok" then
        return "ok", 0
    end

    -- We have successfully retrieved the balance, so return it.
    return "ok", res
end

-- Debit the user's balance in the current ledger state.
function debit(base, request)
    ao.event({ "client starting debit", { request = request, base = base } })
    local status, res = ao.resolve({
        path = "(" .. base["ledger-path"] .. ")/schedule",
        method = "POST",
        body = request
    })
    ao.event({ "client received schedule response", { status = status, res = res } })
    status, res = ao.resolve({
        path = base["ledger-path"] .. "/compute/balance/" .. request["account"],
        slot = res.slot
    })
    ao.event({ "confirmed balance", { status = status, res = res } })
    return "ok"
end

--- Poll an external ledger for credit events. If new credit noticess have been
--- sent by the external ledger, push them to the local ledger.
function poll(base, req)
    local status, local_last_credit = ao.resolve({
        path = base["ledger-path"] .. "/now/last-credit"
    })
    if status ~= "ok" then
        ao.event(
            { "error getting local last credit",
                { status = status, res = local_last_credit } }
        )
        return "error", base
    end

    local status, external_last_credit = ao.resolve({
        path = base["external-ledger"] .. "/now/last-credit"
    })
    if status ~= "ok" then
        ao.event({ "error getting external last credit",
            { status = status, res = external_last_credit } })
        return "error", base
    end

    ao.event({ "Retreived sync data. Last credit info:",
        {
            local_last_credit = local_last_credit,
            external_last_credit = external_last_credit }
        }
    )
    while local_last_credit < external_last_credit do
        status, res = ao.resolve({
            path = base["external-ledger"] .. "/push",
            slot = local_last_credit + 1
        })
        if status ~= "ok" then
            ao.event({ "error pushing slot", { status = status, res = res } })
            return "error", base
        end
        local_last_credit = local_last_credit + 1
    end

    return "ok", base
end