function compute(process, message, opts)
    -- Call the evm@1.0 device using ao.resolve
    local ok, response = ao.resolve({
        device = "evm@1.0",
        path = "get_state",
        chain_id = "9496"  -- Load's appchain
    })

    process.results = {
        output = tostring(response),
        outbox = {}
    }

    return process
end
