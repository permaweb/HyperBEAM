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

-- Charge the user's balance in the current ledger state.
function charge(base, request)
    ao.event("debug_charge", {
        "client starting charge",
        { request = request, base = base }
    })
    local status, res = ao.resolve_committed({
        path = base["ledger-path"] .. "/schedule",
        method = "POST",
        body = request
    }, {
        ["commitment-device"] = "httpsig@1.0",
        type = "signed",
        bundle = true,
        ["linkify-mode"] = false
    })
    ao.event("debug_charge", {
        "client received charge response",
        { status = status, res = res }
    })
    if status ~= "ok" then
        return status, res
    end
    local res_status = type(res) == "table" and rawget(res, "status") or nil
    if tonumber(res_status or 200) >= 400 then
        return "error", res
    end
    return "ok", res
end
