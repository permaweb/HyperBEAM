--- @module test
--- Contains a collection of test functions for the Lua device.

--- @function AssocTable
--- @treturn table
--- @return a table with three key-value pairs. In Erlang, this will be
--- represented as `#{<<"a">> => 1, <<"b">> => 2, <<"c">> => 3}`.
function assoctable()
    return {
        a = 1,
        b = 2,
        c = 3
    }
end

function error_response()
    return "error", "Very bad, but Lua caught it."
end

--- @function ListTable
--- @treturn table
--- @return a table with three elements. In Erlang, this will be
--- represented as `[1, 2, 3]`.
function ListTable()
    return {1, 2, 3}
end

function ao_resolve()
    local status, res =
        ao.resolve({
            path = "/hello",
            hello = "Hello, AO world!"
        })
    return res
end

function ao_relay()
    local status, res =
        ao.resolve({
            path = "/~relay@1.0/call?relay-path=http://localhost:10000/hello"
        })
    return res
end

--- @function compute
--- @tparam stringified AO process
--- @tparam stringified AO message
--- @return table An AO Process response, with the `ok` field set to `true`,
--- the `response` field set to a table with the `Output` field set to a string,
--- and the `messages` field set to an empty table.
function compute(process, message, opts)
    process.results = {
        output = {
            body = 42
        }
    }
    return process
end

--- @function json_result
--- @tparam table base
--- @tparam table request
--- @return table request with the `ok` field set to `true`, the `response`
--- field set to a table with the `Output` field set to `42`, and
--- the `messages` field set to an empty table.
function json_result(base, req, opts)
    return [[
        {
            "ok": true,
            "response": {"Output": {"data": 42}, "Messages": []}
        }
    ]]
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
function request(base, req, opts)
    return "ok", { body = { { body = "i like turtles" } } }
end

--- @function sandboxed_fail
--- @tparam table base
--- @tparam table request
--- @error fails when inside the sandbox
function sandboxed_fail()
    -- Do something that is not dangerous, but is sandboxed nonetheless.
    return os.getenv("PWD")
end

--- @function route_provider
--- @tparam table base
--- @tparam table request
--- @return table a static set of routes for testing purposes.
function route_provider(base, req, opts)
    return {
        {
            node = base.node
        }
    }
end

BaseRoutes = {
    {
        template = "test1",
        host = "http://localhost:10000",
        weight = 50
    },
    {
        template = "test2",
        strategy = "By-Weight",
        choose = 1,
        nodes = {
            {
                prefix = "http://localhost:10001/",
                weight = 50
            },
            {
                prefix = "http://localhost:10002/",
                weight = 50
            }
        }
    }
}

--- @function compute_routes
--- @tparam table base
--- @tparam table request
--- @return table the state of a process after adding a route.
function compute_routes(base, req, opts)
    base["known-routes"] = base["known-routes"] or BaseRoutes
    if req.body.path == "add-route" then
        table.insert(base["known-routes"], req.body)
        base.results = base.results or {}
        base.results.output = {
            status = 200,
            ["content-type"] = "text/plain",
            body = "Route added."
        }
    end
    return base
end