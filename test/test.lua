--- @module test
--- Contains a collection of test functions for the Lua device.

--- @function AssocTable
--- @treturn table
--- @return a table with three key-value pairs. In Erlang, this will be
--- represented as `#{<<"a">> => 1, <<"b">> => 2, <<"c">> => 3}`.
function AssocTable()
    return {
        a = 1,
        b = 2,
        c = 3
    }
end

--- @function ListTable
--- @treturn table
--- @return a table with three elements. In Erlang, this will be
--- represented as `[1, 2, 3]`.
function ListTable()
    return {1, 2, 3}
end

--- @function handle
--- @tparam stringified AO process
--- @tparam stringified AO message
--- @return table An AO Process response, with the `ok` field set to `true`,
--- the `response` field set to a table with the `Output` field set to a string,
--- and the `messages` field set to an empty table.
function handle(process, message, opts)
    return {
        body = 42,
        messages = {}
    }
end

--- @function hello
--- @tparam table base
--- @tparam table request
--- @return table request with the `hello` field set to `"world"`.
function hello(base, req, opts)
    base.hello = req.name or "world"
    return base
end

--- @function preprocess
--- @tparam table base
--- @tparam table request
--- @return table an answer to every HTTP request with the words "i like turtles"
function preprocess(base, req, opts)
    return { { body = "i like turtles" } }
end