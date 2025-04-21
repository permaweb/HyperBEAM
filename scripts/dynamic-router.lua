--- A dynamic route generator in an AO `~process@1.0'.
--- This generator grants a routing table, found at `/now/routes', that is 
--- compatible with the `~router@1.0' interface. Subsequently, it can be
--- used for routing by HyperBEAM nodes' via setting the `route-provider'
--- node message key.
--- 
--- The configuration options are as follows:
--- /is-admissible = A message to call with the registration request's body. Should
---                return a boolean indicating whether the peer is admissible.
--- /sampling-rate = The frequency at which random sampling of registered nodes
---                should be performed, rather than scored routing. Default = 0.1.
--- /pricing-weight = The level to which pricing should be preferred relative to
---                performance in the scoring algorithm. Default = 1.
--- /performance-weight = The level to which performance should be preferred
---                relative to pricing in the scoring algorithm.
---                Default = 1.
--- /score-preference = The level to which the scoring algorithm influence routing
---                decisions amongst scored route generations. Default = 1,
---                yielding an exponential decay in preference for better
---                performing nodes. Default = 1.
--- /recalculate-every = The number of messages to process between recalculating
---                the routing table. Default = 1000.
local function ensure_defaults(state)
    state.routes = state.routes or {}
    state["is-admissible"] =
        state["is-admissible"] or {
            path = "/default",
            default = true
        }
    state["sampling-rate"] = state["sampling-rate"] or 0.1
    state["pricing-weight"] = state["pricing-weight"] or 1
    state["performance-weight"] = state["performance-weight"] or 1
    state["score-preference"] = state["score-preference"] or 1
    state["recalculate-every"] = state["recalculate-every"] or 1000
    state["averaging-period"] = state["averaging-period"] or 1000
    state.performance = state.performance or {}
    state["initial-performance"] = state["initial-performance"] or 30000
    return state
end

-- Find the current route message for a template.
local function current_route(routes, template, opts)
    local status, res =
        ao.resolve({
            path = "/~router@1.0/match",
            ["route-path"] = template, -- Only supports binary templates for now.
            routes = routes
        })
    if status == "ok" then
        return res
    else
        return { template = template, nodes = {} }
    end
end

-- Compute the decay for a given score, modulated by the score preference.
local function decay(state, score)
    return math.exp(-state["score-preference"] * score)
end

-- Compute the scores for all routes.
local function recalculate_scores(state, route, opts)
    -- Calculate the score per node.

    for _, node in ipairs(route.nodes) do
        -- The performance score for the node on the route should be scaled by
        -- the performance weight, moderated by the sampling rate. The sampling
        -- rate is used to ensure that new nodes (and improving nodes) are given
        -- a chance to be selected.
        local perf_score =
            decay(state, node.performance * state["performance-weight"]) *
            ((1 - node.price * state["sampling-rate"]) + state["sampling-rate"])
        -- The price score for the node on the route should be scaled by the
        -- pricing weight. It is not moderated by the sampling rate, as we want
        -- to ensure that the node is selected if it has a low price. New nodes
        -- can improve their likelihood of being selected by lowering their price.
        local price_score = decay(state, node.price * state["pricing-weight"])

        node.weight = perf_score + price_score
    end

    return route
end

local function add_route(state, route, opts)
    local routes = state.routes
    local path = route["route-path"] or route.path
    local price = route.price

    local r = current_route(routes, path, opts)
    table.insert(r.nodes, {
        node = route.node,
        price = price,
        performance = 0
    })
    table.insert(routes, r)
    return routes
end

-- Compute the new routes, with their weights, based on the current routes and
-- a new route.
local function recalculate_routes(state, opts)
    local routes = state.routes

    for _, r in ipairs(routes) do
        r = recalculate_scores(state, r, opts)
    end

    return routes
end

-- Register a new host to a route.
local function register(state, req, opts)
    local status, is_admissible = ao.resolve(state["is-admissible"])
    if status == "ok" and is_admissible ~= false then
        print "TRUSTED PEER"
        state.routes = add_route(state, req.route)
        state.routes = recalculate_routes(state, opts)
    else
        print "UNTRUSTED PEER"
    end

    return state
end

-- Update the performance of a host.
local function performance(state, req, opts)
    local duration = req.body.duration
    local host = req.body.host
    local change_factor = 1 / state["averaging-period"]

    -- Modify the node's existing performance score, weighted by the change
    -- factor, to give more weight to the existing performance score. Even node
    -- is given a poor performance score (30000ms) to start, then will slowly
    -- improve its performance score over time.
    state.performance[host] =
        ((state.performance[host] or state["initial-performance"]) *
            (1 - change_factor)) +
        (duration * change_factor)

    return state
end

-- Main handler for incoming scheduled messages.
function compute(state, assignment, opts)
    state = ensure_defaults(state)
    local req = assignment.body
    local path = req.path

    if path == "register" then
        state = register(state, req, opts)
    elseif path == "recalculate" then
        state = recalculate_routes(state, opts)
    elseif path == "performance" and req.body.method == "POST" then
        state = performance(state, req, opts)
    end

    return state
end

--- Tests

function register_route_test()
    local state = {}
    state = ensure_defaults(state)
    state.routes = {}

    -- Simulate a register call upon a default state.
    local req = {
        path = "register",
        route = {
            node = "host1",
            price = 0.5,
            template = "/test-key"
        }
    }
    state = compute(state, { body = req }, {})

    for key,value in pairs(state) do print(key,value) end
    for key,value in pairs(state.routes) do print(key,value) end
    -- We must now have exactly one route in state.routes.
    if #state.routes ~= 1 then
      error("Expected 1 route after register, got "..tostring(#state.routes))
    end

    -- Verify the node, price and default performance.
    local r = state.routes[1]
    ao.event("debug_router", { "route:", r })
    if r.nodes[1].node ~= "host1" then
      error("Expected node='host1', got "..tostring(r.nodes[1].node))
    end
    if r.nodes[1].price ~= 0.5 then
      error("Expected price=0.5, got "..tostring(r.nodes[1].price))
    end
    if r.nodes[1].performance ~= 0 then
      error("Expected performance=0, got "..tostring(r.nodes[1].performance))
    end

    return "ok"
  end
  
  -- Test 2: performance updates and weight recalculation
function performance_and_recalc_test_skip()
    -- start with one route already in state
    local init = { routes = { { node = "host1", price = 0.2, performance = 0 } } }
    local state = ensure_defaults(init)
  
    -- post a performance update
    local perf_req = {
      path = "performance",
      body = { host = "host1", duration = 100 }
    }
    state = compute(state, { body = perf_req }, {})
  
    -- compute expected new performance value
    local avg = state["averaging-period"]
    local init_perf = state["initial-performance"]
    local expected_perf = (init_perf * (1 - 1/avg)) + (100 * (1/avg))
    local actual_perf = state.performance.nodes["host1"]
    ao.event("debug_router", state.performance)
    if math.abs(actual_perf - expected_perf) > 1e-9 then
      error(("Performance mismatch: expected %.9f, got %.9f")
            :format(expected_perf, actual_perf))
    end
  
    -- now trigger a recalc
    state = compute(state, { body = { path = "recalculate" } }, {})
  
    -- manually compute the expected weight term
    local route = state.routes[1]
    local sp = state["score-preference"]
    local sr = state["sampling-rate"]
    local pw = state["pricing-weight"]
    local perf_term = math.exp(-sp * actual_perf) * ((1 - route.price * sr) + sr)
    local price_term = math.exp(-sp * (route.price * pw))
    local expected_weight = perf_term + price_term
    if math.abs(route.weight - expected_weight) > 1e-9 then
      error(("Weight mismatch: expected %.9f, got %.9f")
            :format(expected_weight, route.weight))
    end
  
    return "ok"
end