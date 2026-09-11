--- @module arweave-scheduler-test
--- State-transformation test module for the `~arweave-scheduler@1.0` device.
--- Messages scheduled as Arweave L1 transactions invoke these functions via
--- their `path` tag. The function names deliberately avoid the keys that
--- `~lua@5.3a` excludes from its default handler (`set`, `path`, ...) and
--- the keys present on the process message itself.

--- Executed for assignments without a `path` tag -- most notably slot 0,
--- the process message itself.
function compute(base, req, opts)
    return base
end

--- Overwrite the process state with the `value` tag of the message.
function setstate(base, req, opts)
    base.state = tonumber(req.body.value)
    return base
end

--- Add the `value` tag of the message to the process state.
function addstate(base, req, opts)
    base.state = (base.state or 0) + tonumber(req.body.value)
    return base
end

--- Report the process state in the results of the slot.
function querystate(base, req, opts)
    base.results = {
        output = {
            body = "state=" .. tostring(base.state)
        }
    }
    return base
end
