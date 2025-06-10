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
--- /performance-period = Alters the rate at which performance scores are modified
---                by new performance ratings. A lower period implies faster 
---                changes to the score.
--- /recalculate-every = The number of messages to process between recalculating
---                the routing table. Default = 1000.
local function ensure_defaults(state)
    state.routes = state.routes or {}
    state["is-admissible"] =
        state["is-admissible"] or {
			path = "/default",
            default = "true"
        }
    state["sampling-rate"] = state["sampling-rate"] or 0.1
    state["pricing-weight"] = state["pricing-weight"] or 1
    state["performance-weight"] = state["performance-weight"] or 1
    state["score-preference"] = state["score-preference"] or 1
    state["recalculate-every"] = state["recalculate-every"] or 1000
    state["performance-period"] = state["performance-period"] or 1000
    state["initial-performance"] = state["initial-performance"] or 30000
    return state
end

-- Find the current route message for a template.
local function current_route(routes, template, opts)
    -- Find the existing route that matches the template, if it exists.
    local status, res =
        ao.resolve({
            path = "/~router@1.0/match",
            ["route-path"] = template, -- Only supports binary templates for now.
            routes = routes
        })
    if status == "ok" then
        -- We found an existing route for this template. Return it as-is.
        return res
    else
        -- We haven't found a route for this template, so we need to create a new
        -- one. We set the reference to the next available index in the routes
        -- table.
        return {
            strategy = "By-Weight",
            template = template,
            nodes = {},
            reference = "routes/" .. tostring(#routes + 1)
        }
    end
end

-- Compute the decay for a given score, modulated by the score preference.
local function decay(state, score)
    return math.exp(-state["score-preference"] * score)
end

-- Calculate statistics for a given key across all nodes in a route.
local function calculate_stats(nodes, key)
    local stats = {
        count = 0,
        total = 0,
        max = 0,
        mean = 0,
        values = {}
    }

    for _, n in ipairs(nodes) do
        stats.count = stats.count + 1
        stats.total = stats.total + n[key]
        if n[key] > stats.max then
            stats.max = n[key]
        end
        if stats.min == nil or n[key] < stats.min then
            stats.min = n[key]
        end
        table.insert(stats.values, n[key])
    end

    stats.mean = stats.total / stats.count

    -- Add a function that returns the percentile of a node for the given key.
    table.sort(stats.values)
    stats.percentile =
        function(n)
            local n_key = n[key]
            for ix, v in ipairs(stats.values) do
                if n_key <= v then
                    return (ix-1) / stats.count
                end
            end
        end

    return stats
end

-- Compute the scores for all routes. Outputs a single weight value per node,
-- where a higher value indicates that the node should be picked more frequently.
-- Each of the 'scoring' factors, in their natural state, are worse if they are
-- higher. Higher price and slower response times are negative factors for nodes.
-- This function rectifies that, and scores each node relative to the performance
-- of each of their peers.
local function recalculate_scores(state, route, opts)
    -- TODO: Refactor such that this does not have `O(:facepalm:)` properties...

    -- Calculate stats for each relevant performance characteristic.
    local perf_stats = calculate_stats(route.nodes, "performance")
    local price_stats = calculate_stats(route.nodes, "price")

    -- Calculate the multipliers for performance and price from their weights.
    local total_weight = state["performance-weight"] + state["pricing-weight"]
    local perf_weight = state["performance-weight"] / total_weight
    local pricing_weight = state["pricing-weight"] / total_weight

    -- Calculate the score per node.
    for ix, node in ipairs(route.nodes) do
        -- The performance score for the node on the route should be scaled by
        -- moderated by the sampling rate. The sampling rate is used to ensure 
        -- that new/improving nodes (and improving nodes) are given a chance to
        -- be selected.
        local perf_percentile = perf_stats.percentile(node)
        local perf_score =
            (decay(state, perf_percentile) * (1 - state["sampling-rate"]))
                + state["sampling-rate"]
        -- The price score for the node on the route should be scaled by the
        -- pricing weight. It is not moderated by the sampling rate, as we want
        -- to ensure that the node is selected if it has a low price. New nodes
        -- can improve their likelihood of being selected by lowering their price.
        local price_percentile = price_stats.percentile(node)
        local price_score = decay(state, price_percentile)

        -- Calculate the final weight. In order to do this we:
        -- 1. Apply the factor weights to the calculated scores.
        -- 2. Sum them.
        node.weight =
            ((perf_score * perf_weight) + (price_score * pricing_weight))

        ao.event("debug_scores",
            {
                "calculated_score", {
                    node = ix,
                    prefix = node.prefix,
                    perf = node.performance,
                    perf_percentile = perf_percentile,
                    perf_weight = perf_weight,
                    perf_score = perf_score,
                    price = node.price,
                    price_percentile = price_percentile,
                    pricing_weight = pricing_weight,
                    price_score = price_score,
                    result = node.weight
                }
            }
        )
    end

    return route
end

local function add_node(state, req, opts)
    local route = current_route(state.routes, req.route.template, opts)
    local reference = route.reference .. "/nodes/" .. tostring(#route.nodes + 1)
    table.insert(route.nodes, {
        prefix = req.route.prefix,
        price = req.route.price,
        topup = req.route.topup,
        performance = state["initial-performance"],
        reference = reference,
        opts = { http_reference = reference }
    })

    local new_state = ao.set(state, route.reference, route)
    return new_state
end

-- Compute the new routes, with their weights, based on the current routes and
-- a new route.
function recalculate(state, _, opts)
    state = ensure_defaults(state)

    for _, r in ipairs(state.routes) do
        r = recalculate_scores(state, r, opts)
    end

    return "ok", state
end

-- Register a new host to a route.
function register(state, assignment, opts)
    state = ensure_defaults(state)
    local req = assignment.body
    req.path = state["is-admissible"].path or "is-admissible"
    local status, is_admissible = ao.resolve(state["is-admissible"], req)

    ao.event("is-admissible result:", { status, is_admissible })
    if status == "ok" and is_admissible == "true" then
        state = add_node(state, req)
        return recalculate(state, assignment, opts)
    else
        -- If the registration is untrusted signal the issue via and event and
        -- return the state unmodified
        ao.event("error", { "untrusted peer requested", req})
        return "ok", state
    end
end

-- Update the performance of a host by its reference.
function duration(state, assignment, opts)
    state = ensure_defaults(state)

    local req = assignment.body
    local reference = req.reference
    if reference == nil then
        ao.event("debug_dynrouter", 
            {
                "ignoring duration update for request without reference: ",
                req["request-path"]
            }
        )
        return state
    end
    ao.event("debug_dynrouter", {"applying_duration", req.reference})
    reference = reference .. "/performance"
    local duration = req.duration
    local change_factor = 1 / state["performance-period"]

    -- Get the performance of the route at `reference'
    local status, performance = ao.resolve(state, reference)

    -- Modify the node's existing performance score, weighted by the change
    -- factor, to give more weight to the existing performance score. Each node
    -- is given a poor performance score (30000ms) to start, then will slowly
    -- improve its performance score over time.
    performance =
        (performance * (1 - change_factor)) + (duration * change_factor)

    ao.event("debug_perf",
        {"Received performance", {
            reference = reference,
            performance = performance,
            update_duration = duration,
            change_factor = change_factor,
        }
    })
    
    state = ao.set(state, reference, performance)

    ao.event("debug_router",
        {
            "State after performance set",
            { state = state, performance = performance }
        }
    )
    return "ok", state
end

function compute(state, assignment, opts)
    if assignment.body.path == "register" then
        return register(state, assignment, opts)
    elseif assignment.body.path == "recalculate" then
        return recalculate(state, assignment, opts)
    elseif assignment.body.path == "performance" then
        return duration(state, assignment, opts)
    else
        -- If we have been called without a relevant path, simply ensure that
        -- the state is initialized and return it.
        state = ensure_defaults(state)
        return "ok", state
    end
end

--- Tests
function register_test()
    local state = {}
    -- Simulate a register call upon a default state.
    local req = {
        path = "register",
        route = {
            prefix = "host1",
            price = 5,
            template = "/test-key"
        }
    }
    _, state = register(state, { body = req }, {})

    -- We must now have exactly one route in state.routes.
    if #state.routes ~= 1 then
      error("Expected 1 route after register, got "..tostring(#state.routes))
    end
  
    -- Verify the node, price and default performance.
    local r = state.routes[1]
    ao.event("debug_router", { "route:", r })
    if r.nodes[1].prefix ~= "host1" then
        error("Expected node='host1', got "..tostring(r.nodes[1].node))
    end
    if r.nodes[1].price ~= 5 then
        error("Expected price=0.5, got "..tostring(r.nodes[1].price))
    end
    if r.nodes[1].performance ~= state["initial-performance"] then
        error("Expected performance=" .. 
            tostring(state["initial-performance"]) ..
            ", got " .. tostring(r.nodes[1].performance)
        )
    end

    -- Register another provider on the route.
    req = {
        path = "register",
        route = {
            prefix = "host2",
            price = 10,
            template = "/test-key"
        }
    }
    _, state = register(state, { body = req }, {})

    ao.event("debug_router", {"state after second registration", state})

    if #state.routes[1].nodes ~= 2 then
        error("Expected 2 nodes after second registration, got "
            .. tostring(#state.routes[1].nodes))
    end

    return "ok"
  end
  
  -- Test 2: performance updates and weight recalculation
function performance_test()
    -- Create a new state with a fast performance-period, giving rapid changes
    -- to the performance score of nodes.
    local state = {
        ["performance-period"] = 6
    }

    -- Add a node to a new route on the state
    local register_req = {
        path = "register",
        route = {
            prefix = "host1",
            price = 5,
            template = "/test-key"
        }
    }
    _, state = register(state, { body = register_req }, {})

    -- Modify the request and add another node.
    register_req.route.prefix = "host2"
    _, state = register(state, { body = register_req }, {})

    -- Get the references for the nodes on the route and validate it.
    local node1_ref = state.routes[1].nodes[1].reference
    local node2_ref = state.routes[1].nodes[2].reference

    if node1_ref ~= "routes/1/nodes/1" then
        error("Invalid reference. Received: " .. node1_ref)
    end
    if node2_ref ~= "routes/1/nodes/2" then
        error("Invalid reference. Received: " .. node2_ref)
    end

    -- Record the starting scores for the nodes
    local t0_node1_score = state.routes[1].nodes[1].weight
    local t0_node2_score = state.routes[1].nodes[1].weight
  
    if t0_node1_score ~= t0_node2_score then
        error("Initial node scores should be equal. Received: "
            .. tostring(t0_node1_score) .. " and " .. tostring(t0_node2_score))
    end

    -- Post 2 performance updates for the first node, improving its performance.
    local perf_req = {
        path = "duration",
        host = "host1",
        reference = node1_ref,
        duration = 200
    }
    _, state = duration(state, { body = perf_req }, {})
    _, state = duration(state, { body = perf_req }, {})
    -- Post a performance update for the second node, with very poor performance
    perf_req.reference = node2_ref
    perf_req.duration = 55500
    ao.event("debug_router", {"perf_req node 2", perf_req})
    _, state = duration(state, { body = perf_req }, {})

    ao.event("debug_router",
        {"state after performance updates", {
            state = state
        }}
    )
  
    -- now trigger a recalc
    _, state = recalculate(state, { body = { path = "recalculate" } }, {})

    ao.event("debug_router",
        {"Nodes after recalculation", state.routes[1].nodes}
    )

    -- Record the starting scores for the nodes
    local t1_node1_score = state.routes[1].nodes[1].weight
    local t1_node2_score = state.routes[1].nodes[2].weight

    ao.event("debug_router_scores", {
        t0_n1 = t0_node1_score,
        t1_n1 = t1_node1_score,
        t0_n2 = t0_node2_score,
        t1_n2 = t1_node2_score
    })

    if t1_node1_score ~= t0_node1_score then
        error("Node 1 sets the benchmark: It's score should stay the same.")
    end

    if t1_node2_score >= t0_node2_score then
        error("Node 2 score should have decreased!")
    end
    
    return "ok"
end