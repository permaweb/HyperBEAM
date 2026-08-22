local function decimal(value)
    local result = tostring(value):match("^0*(%d+)$")
    return result or "0"
end

local function better(left, right)
    if #left.balance ~= #right.balance then
        return #left.balance > #right.balance
    end
    if left.balance ~= right.balance then
        return left.balance > right.balance
    end
    return left.address < right.address
end

local function weaker(left, right)
    return better(right, left)
end

local function push(heap, holder, limit)
    if #heap < limit then
        heap[#heap + 1] = holder
        local index = #heap
        while index > 1 do
            local parent = math.floor(index / 2)
            if not weaker(heap[index], heap[parent]) then break end
            heap[index], heap[parent] = heap[parent], heap[index]
            index = parent
        end
        return
    end
    if not better(holder, heap[1]) then return end
    heap[1] = holder
    local index = 1
    while true do
        local child = index * 2
        if child > #heap then break end
        if child < #heap and weaker(heap[child + 1], heap[child]) then
            child = child + 1
        end
        if not weaker(heap[child], heap[index]) then break end
        heap[index], heap[child] = heap[child], heap[index]
        index = child
    end
end

local reserved = {
    ["ao-types"] = true,
    ["commitments"] = true,
    ["device"] = true,
    ["hashpath"] = true,
    ["priv"] = true,
}

local function walk(node, prefix, heap, limit)
    if node["node-value"] ~= nil then
        push(heap, { address = prefix, balance = decimal(node["node-value"]) }, limit)
    end
    for edge, child in pairs(node) do
        if not reserved[edge] and edge ~= "node-value" then
            local address = prefix .. edge
            if type(child) == "table" then
                walk(child, address, heap, limit)
            else
                push(heap, { address = address, balance = decimal(child) }, limit)
            end
        end
    end
end

local escapes = {
    ['"'] = '\\"',
    ['\\'] = '\\\\',
    ['\b'] = '\\b',
    ['\f'] = '\\f',
    ['\n'] = '\\n',
    ['\r'] = '\\r',
    ['\t'] = '\\t',
}

local function quote(value)
    return '"' .. tostring(value):gsub('[%z\1-\31\\"]', function(char)
        return escapes[char] or string.format('\\u%04x', string.byte(char))
    end) .. '"'
end

function top(_, req)
    local process = req["process-id"]
    local limit = math.floor(tonumber(req.top) or 100)
    if not process or limit < 1 then
        return "error", { status = 400, body = "process-id and a positive top are required" }
    end

    local status, balances = ao.resolve({
        path = "/" .. process .. "~process@1.0/compute/balances"
    })
    if status ~= "ok" then return status, balances end
    local heap = {}
    walk(balances, "", heap, limit)
    table.sort(heap, better)
    local rows = {}
    for index, holder in ipairs(heap) do
        rows[index] = '{"address":' .. quote(holder.address)
            .. ',"balance":' .. quote(holder.balance) .. '}'
    end
    return {
        status = 200,
        ["content-type"] = "application/json",
        body = '[' .. table.concat(rows, ',') .. ']'
    }
end
