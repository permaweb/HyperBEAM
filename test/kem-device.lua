function compute(process, message, opts)
    -- Call the kem@1.0 device using ao.resolve
    local ok, response = ao.resolve({
        device = "kem@1.0",
        path = "execute_kernel",
        method = "POST",
        body = '{"kernel_id":"btSvNclyu2me_zGh4X9ULVRZqwze9l2DpkcVHcLw9Eg","input_data":[1,3,5,7],"output_size_hint":1}'
    })

    process.results = {
        output = tostring(response),
        outbox = {}
    }

    return process
end
