--- A module that tests the `dev_lua_test' EUnit wrapper integration.

-- Return a simple result to the calling test suite.
function basic_test()
    return "ok"
end

-- Return a message to the calling test suite.
function return_message_test()
    return "ok", { key1 = "Value1", key2 = "Value2" }
end

-- This function returns an `{error, _}` tuple which -- if it were to be picked
-- up as a test by `dev_lua_tests' -- would cause the test suite to fail.
-- Subsequently, by virtue of the code not being executed, we gain confidence
-- that the test suite generator is differentiating between relevant and
-- irrelevant functions correctly. Its a curious mechanism, but it is useful.
function meta_test_ignored()
    return "error", "This should not be picked up as a test!"
end

-- Test that the test environment granted by the generator allows us to execute
-- calls with the `ao.resolve' function, outside of the sandbox. Currently,
-- `dev_lua_test' does not support sandboxing.
function sandbox_test()
    -- Simply return an AO call to the test suite. If the router device is not 
    -- available, this will cause the test to fail.
    local status, res = ao.resolve({ path = "/~router@1.0/routes/1/template" })
    return status, res
end