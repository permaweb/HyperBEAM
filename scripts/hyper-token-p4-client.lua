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
    -- If the balance request fails (most likely because the user has no balance),
    -- return a balance of 0.
    if status ~= "ok" then
        ao.event({"Returning 0 balance", {
            status = status,
            res = res,
            target = request["target"]
        }})
        return "ok", 0
    end

    ao.event({"Returning balance", {
        status = status,
        res = res,
        target = request["target"]
    }})
    -- We have successfully retrieved the balance, so return it.
    return "ok", res
end

-- Charge the user's balance in the current ledger state.
function charge(base, request)
    ao.event("debug_charge", {
        "client starting charge",
        { request = request, base = base }
    })
    local status, res = ao.resolve({
        path = "(" .. base["ledger-path"] .. ")/push",
        method = "POST",
        body = request
    })
    ao.event("debug_charge", {
        "client received charge response",
        { status = status, res = res }
    })
    return "ok", res
end