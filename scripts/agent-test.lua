--- @module agent-test
--- Tests for dev_agent called via ao.resolve (simulating Lua process usage).

--- @function agent_resolve_test
--- Call dev_agent via ao.resolve with real Novita AI API.
--- The agent should use the http_request tool to fetch data and return a result.
function agent_resolve_test()
    local status, res = ao.resolve({
        path = "/~agent@1.0/run",
        ["agent-user-prompt"] = "Use the http_request tool to GET https://httpbin.org/get and tell me what the origin IP address is.",
        ["agent-model"] = "minimax/minimax-m2.7",
        ["agent-api-peer"] = "https://api.novita.ai",
        ["agent-api-path"] = "/openai/v1/chat/completions",
        ["agent-api-key"] = "sk_VkH3T72Z7LsiDvc5oRvcCtuPciFNwKShHLOgs3LqGVI",
        ["agent-max-iterations"] = 5
    })
    -- Log the result for debugging
    ao.event("agent_test", {
        status = status,
        answer = res["agent-answer"],
        iterations = res["agent-iterations"]
    })
    return status, res
end

--- @function agent_simple_test
--- Call dev_agent with a simple question (no tool call needed).
function agent_simple_test()
    local status, res = ao.resolve({
        path = "/~agent@1.0/run",
        ["agent-user-prompt"] = "What is 2+2? Answer with just the number.",
        ["agent-model"] = "minimax/minimax-m2.7",
        ["agent-api-peer"] = "https://api.novita.ai",
        ["agent-api-path"] = "/openai/v1/chat/completions",
        ["agent-api-key"] = "sk_VkH3T72Z7LsiDvc5oRvcCtuPciFNwKShHLOgs3LqGVI",
        ["agent-max-iterations"] = 3
    })
    ao.event("agent_simple_test", {
        status = status,
        answer = res["agent-answer"],
        iterations = res["agent-iterations"]
    })
    return status, res
end
