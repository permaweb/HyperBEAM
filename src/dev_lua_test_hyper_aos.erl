-module(dev_lua_test_hyper_aos).
-export([generate_lua_process/2, generate_test_message/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

invoke_aos_test() ->
    Opts = #{ priv_wallet => hb:wallet() },
    Process = generate_lua_process("test/hyper-aos.lua", Opts),
    {ok, _Proc} = hb_cache:write(Process, Opts),
    Message = generate_test_message(Process, Opts),
    {ok, _Assignment} = hb_ao:resolve(Process, Message, Opts#{ hashpath => ignore }),
    {ok, Results} = hb_ao:resolve(Process, <<"now/results/output">>, Opts),
    ?assertEqual(<<"1">>, hb_ao:get(<<"data">>, Results, #{})),
    ?assertEqual(<<"aos> ">>, hb_ao:get(<<"prompt">>, Results, #{})).

aos_authority_not_trusted_test() ->
    Opts = #{ priv_wallet => ar_wallet:new() },
    Process = generate_lua_process("test/hyper-aos.lua", Opts),
    ProcID = hb_message:id(Process, all),
    {ok, _} = hb_cache:write(Process, Opts),
    Message = hb_message:commit(
        #{
            <<"path">> => <<"schedule">>,
            <<"method">> => <<"POST">>,
            <<"body">> =>
                hb_message:commit(
                    #{
                        <<"target">> => ProcID,
                        <<"type">> => <<"Message">>,
                        <<"data">> => <<"1 + 1">>,
                        <<"random-seed">> => rand:uniform(1337),
                        <<"action">> => <<"Eval">>,
                        <<"from-process">> => <<"1234">>
                    },
                    Opts
                )
        },
        Opts
    ),
    ?event({message, Message}),
    {ok, _} = hb_ao:resolve(Process, Message, Opts#{ hashpath => ignore }),
    {ok, Results} = hb_ao:resolve(Process, <<"now/results/output/data">>, Opts),
    ?assertEqual(<<"Message is not trusted.">>, Results).

%% @doc Benchmark the performance of Lua executions.
aos_process_benchmark_test_() ->
    {timeout, 30, fun() ->
        BenchMsgs = 10,
        Opts = #{
            process_async_cache => false,
            hashpath => ignore,
            process_cache_frequency => 50
        },
        Process = generate_lua_process("test/hyper-aos.lua", Opts),
        Message = generate_test_message(Process, Opts),
        lists:foreach(
            fun(X) ->
                hb_ao:resolve(Process, Message, Opts),
                ?event(debug_lua, {scheduled, X})
            end,
            lists:seq(1, BenchMsgs)
        ),
        ?event(debug_lua, {executing, BenchMsgs}),
        BeforeExec = os:system_time(millisecond),
        {ok, _} = hb_ao:resolve(
            Process,
            <<"now">>,
            Opts
        ),
        AfterExec = os:system_time(millisecond),
        ?event(debug_lua, {execution_time, (AfterExec - BeforeExec) / BenchMsgs}),
        hb_util:eunit_print(
            "Computed ~p AOS process executions in ~ps (~.2f calls/s)",
            [
                BenchMsgs,
                (AfterExec - BeforeExec) / 1000,
                BenchMsgs / ((AfterExec - BeforeExec) / 1000)
            ]
        )
    end}.

%% @doc Test the hyper-aos stringify module.
hyper_aos_stringify_test() ->
    Wallet = hb:wallet(),
    Opts = #{ priv_wallet => Wallet },
    {ok, Stringify} = file:read_file("scripts/aos-stringify.lua"),
    Code = <<
"""
local stringify = require('.stringify')

function compute(base, req)
  local x = stringify.format({ hello = "World"})
  local y = stringify.format({ hello = "World"})
  base.results = tostring(x == y)
  return base 
end
"""
    >>,
    Process = generate_hyper_aos_modular_process([Stringify, Code], Wallet),
    Message = generate_test_message(Process, Opts, <<"">>),
    hb_cache:write(Process, Opts),
    hb_ao:resolve(Process, Message, Opts#{ hashpath => ignore }),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual(<<"true">>, Result).

%% @doc Test the hyper-aos json module.
hyper_aos_json_test() ->
    Wallet = hb:wallet(),
    Opts = #{ priv_wallet => Wallet },
    {ok, Json} = file:read_file("scripts/aos-json.lua"),
    Code = <<
"""
local json = require('.json')

function compute(base, req)
  local data = { hello = [[World]]}
  local json_data = json.encode(data)
  local new_data = json.decode(json_data)
  base.results = tostring(data.hello == new_data.hello)
  return base
end
"""
    >>,
    Process = generate_hyper_aos_modular_process([Json, Code], Wallet),
    Message = generate_test_message(Process, Opts, <<"">>),
    hb_cache:write(Process, Opts),
    hb_ao:resolve(Process, Message, Opts#{ hashpath => ignore }),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual(<<"true">>, Result).

%% @doc Test the hyper-aos handlers utils module matchesPattern function
hyper_aos_handlers_utils_has_matching_tag_test() ->
    Code = <<
"""
local handlers = {
  utils = require('.handlers-utils')
}

function compute(base, req)
  local hasMatchingTag = handlers.utils.hasMatchingTag('Action', 'Eval')
  base.results = { 
    tostring(hasMatchingTag({ Tags = { Action = 'Eval' } })),
    tostring(hasMatchingTag({ Tags = { Action = 'Foo' } })),
    tostring(hasMatchingTag({ Action = 'Eval', Tags = {} }))
  }
  return base
end
"""
>>, 
    {ok, Process, Opts} = 
        generate_hyper_aos_modular_handlers_utils_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"true">>, <<"false">>, <<"false">>], Result).

hyper_aos_handlers_utils_has_matching_tag_of_test() ->
    Code = <<
"""
local handlers = {
  utils = require('.handlers-utils')
}

function compute(base, req)
  local hasMatchingTagOf = 
    handlers.utils.hasMatchingTagOf('Action', { 'Eval', 'Foo' })
  base.results = { 
    tostring(hasMatchingTagOf({ Tags = { Action = 'Eval' } })),
    tostring(hasMatchingTagOf({ Action = 'Eval', Tags = {} })),
    tostring(hasMatchingTagOf({ Tags = { Data = 'None', Action = 'Foo' } })),
    tostring(hasMatchingTagOf({ Tags = { Action = 'Bar' } })),
    tostring(hasMatchingTagOf({ Tags = { Action = 'foo' } })) 
  }
  return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_handlers_utils_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([
        <<"true">>,
        <<"0">>,
        <<"true">>,
        <<"0">>,
        <<"0">>
    ], Result).

hyper_aos_handlers_utils_has_matching_data_test() ->
    Code = <<
"""
local handlers = {
    utils = require('.handlers-utils')
}

function compute(base, req)
    local hasMatchingData = handlers.utils.hasMatchingData('Foo')
    base.results = { 
        tostring(hasMatchingData({ Tags = { Action = 'Eval' }, Data = 'Foo' })),
        tostring(hasMatchingData({ Tags = { Data = 'Foo', Action = 'Foo' } })),
        tostring(hasMatchingData({ Tags = { Action = 'Bar' }, Data = 'Bar' })),
        tostring(hasMatchingData({ data = 'Foo' }))
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_handlers_utils_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"true">>, <<"false">>, <<"false">>, <<"false">>], Result).
    
hyper_aos_handlers_utils_reply_test() ->
    Code = <<
"""
local handlers = {
    utils = require('.handlers-utils')
}

function compute(base, req)
    base.results = {}
    local replyStr = handlers.utils.reply('Foo')
    local replyTable =
        handlers.utils.reply({ Tags = { Action = 'Foo' }, Data = 'Bar' })
    local msgReply = function(data)
        table.insert(base.results, data)
    end
    
    replyStr({ reply = msgReply })
    replyStr({ reply = msgReply, Tags = { Action = 'Eval' } })
    replyTable({ reply = msgReply })
    replyTable({ reply = msgReply, Tags = { Action = 'Eval' } })
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_handlers_utils_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([
        #{ <<"Data">> => <<"Foo">> },
        #{ <<"Data">> => <<"Foo">> },
        #{ 
            <<"Tags">> => #{ <<"Action">> => <<"Foo">> },
            <<"Data">> => <<"Bar">> 
        },
        #{ 
            <<"Tags">> => #{ <<"Action">> => <<"Foo">> },
            <<"Data">> => <<"Bar">> 
        }
    ], Result).

   
hyper_aos_handlers_utils_continue_test() ->
        Code = <<
    """
    local handlers = {
        utils = require('.handlers-utils')
    }


    function compute(base, req)
        local continue = handlers.utils.continue({ Action = 'Eval' })
        base.results = {
            tostring(continue({ Action = 'Eval' })),
            tostring(continue({ Action = 'Foo' }))
        }
        return base
    end
    """
    >>,
        {ok, Process, Opts} =
            generate_hyper_aos_modular_handlers_utils_process(Code),
        {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
        io:format("Result ~p~n", [Result]),
        ?assertEqual([<<"1">>, <<"false">>], Result).
       
%% @doc Test the hyper-aos handlers module.

%% @doc Test the hyper-aos handlers add function.
hyper_aos_handlers_add_test() ->
    Code = <<
"""
local handlers = require('.handlers')

function compute(base, req)
    -- Test adding a handler with 2 arguments
    handlers.add('test-handler-1', function(msg) end)
    
    -- Test adding a handler with 3 arguments
    handlers.add('test-handler-2', function(msg) return true end, function(msg) end)
    
    -- Test adding a handler with 4 arguments (including maxRuns)
    handlers.add('test-handler-3', function(msg) return true end, function(msg) end, 5)

    -- Update existing handler
    handlers.add('test-handler-1', function(msg) return true end, function(msg) end, 10)
    
    base.results = {
        tostring(#handlers.list),
        tostring(handlers.list[1].name),
        tostring(handlers.list[2].name),
        tostring(handlers.list[3].name),
        tostring(handlers.list[3].maxRuns),
        tostring(handlers.list[1].maxRuns)
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_handlers_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([
        <<"3">>,
        <<"test-handler-1">>,
        <<"test-handler-2">>,
        <<"test-handler-3">>,
        <<"5">>,
        <<"10">>
    ], Result).

% @doc Test the hyper-aos handlers append function.
hyper_aos_handlers_append_test() ->
    Code = <<
"""
local handlers = require('.handlers')

function compute(base, req)
    -- Add initial handler
    handlers.add('first-handler', function(msg) return true end, function(msg) end)
    
    -- Append a second handler
    handlers.append('second-handler', function(msg) return true end, function(msg) end)
    
    base.results = {
        tostring(#handlers.list),
        tostring(handlers.list[1].name),
        tostring(handlers.list[2].name)
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_handlers_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"2">>, <<"first-handler">>, <<"second-handler">>], Result).

%% @doc Test the hyper-aos handlers prepend function.
hyper_aos_handlers_prepend_test() ->
    Code = <<
"""
local handlers = require('.handlers')

function compute(base, req)
    -- Add initial handler
    handlers.add('first-handler', function(msg) return true end, function(msg) end)
    
    -- Prepend a handler (should be first in list)
    handlers.prepend('second-handler', function(msg) return true end, function(msg) end)
    
    base.results = {
        tostring(#handlers.list),
        tostring(handlers.list[1].name),
        tostring(handlers.list[2].name)
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_handlers_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"2">>, <<"second-handler">>, <<"first-handler">>], Result).

%% @doc Test the hyper-aos handlers remove function.
hyper_aos_handlers_remove_test() ->
    Code = <<
"""
local handlers = require('.handlers')

function compute(base, req)
    -- Add multiple handlers
    handlers.add('first-handler', function(msg) return true end, function(msg) end)
    handlers.add('second-handler', function(msg) return true end, function(msg) end)
    handlers.add('third-handler', function(msg) return true end, function(msg) end)
    
    local beforeRemove = handlers.list[2]
    
    -- Remove middle handler
    handlers.remove('second-handler')
    
    base.results = {
        tostring(beforeRemove.name),
        tostring(#handlers.list),
        tostring(handlers.list[1].name),
        tostring(handlers.list[2].name)
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_handlers_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([
        <<"second-handler">>,  
        <<"2">>,
        <<"first-handler">>,
        <<"third-handler">>
    ], Result).

%% @doc Test the hyper-aos handlers once function.
hyper_aos_handlers_once_test() ->
    Code = <<
"""
local handlers = require('.handlers')

function compute(base, req)
    -- Test once with named handler
    handlers.once(
        'once-handler', 
        function(msg) return msg.Tags['Action'] == 'Eval' end, 
        function(msg) table.insert(base.results, 'ran once-handler') end
    )
    
    -- Test once with generated name
    handlers.once(
        function(msg) return msg.Tags['Action'] == 'Eval2' end, 
        function(msg) end
    )
    
    base.results = {
        tostring(#handlers.list),
        tostring(handlers.list[1].name),
        tostring(handlers.list[1].maxRuns),
        tostring(handlers.list[2].name),
        tostring(handlers.list[2].maxRuns)
    }

    handlers.evaluate({ Tags = { Action = 'Eval' } }, {})
    table.insert(base.results, tostring(#handlers.list))
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_handlers_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([
        <<"2">>,
        <<"_once_0">>,
        <<"1">>,
        <<"once-handler">>,
        <<"1">>,
        <<"ran once-handler">>,
        <<"1">>
    ], Result).

%% @doc Test the hyper-aos handlers before function.
hyper_aos_handlers_before_test() ->
    Code = <<
"""
local handlers = require('.handlers')

function compute(base, req)
    -- Add initial handlers
    handlers.add('first-handler', function(msg) return true end, function(msg) end)
    handlers.add('third-handler', function(msg) return true end, function(msg) end)
    
    -- Insert handler before 'third-handler'
    handlers.before('third-handler').add('second-handler', function(msg) return true end, function(msg) end)
    
    base.results = {
        tostring(#handlers.list),
        tostring(handlers.list[1].name),
        tostring(handlers.list[2].name),
        tostring(handlers.list[3].name)
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_handlers_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([
        <<"3">>, 
        <<"first-handler">>,
        <<"second-handler">>,
        <<"third-handler">>
    ], Result).

%% @doc Test the hyper-aos handlers after function.
hyper_aos_handlers_after_test() ->
    Code = <<
"""
local handlers = require('.handlers')

function compute(base, req)
    -- Add initial handlers
    handlers.add('first-handler', function(msg) return true end, function(msg) end)
    handlers.add('third-handler', function(msg) return true end, function(msg) end)
    
    -- Insert handler after 'first-handler'
    handlers.after('first-handler').add('second-handler', function(msg) return true end, function(msg) end)
    
    base.results = {
        tostring(#handlers.list),
        tostring(handlers.list[1].name),
        tostring(handlers.list[2].name),
        tostring(handlers.list[3].name)
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_handlers_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"3">>, <<"first-handler">>, <<"second-handler">>, <<"third-handler">>], Result).

%% @doc Test the hyper-aos handlers evaluate function with pattern matching.
hyper_aos_handlers_evaluate_test() ->
    Code = <<
"""
local handlers = require('.handlers')

function compute(base, req)
    local callResults = {}
    
    -- Add default handler
    handlers.add('_default', function(msg) return true end, function(msg) end)

    -- Add handlers with different patterns
    handlers.add('action-eval', function(msg) 
        return msg.Tags and msg.Tags.Action == 'Eval' 
    end, function(msg) 
        table.insert(callResults, 'eval-called')
    end)
    
    handlers.add('action-test', function(msg) 
        return msg.Tags and msg.Tags.Action == 'Test'
    end, function(msg) 
        table.insert(callResults, 'test-called')
    end)
    
    -- Test with Eval action
    local msg1 = { Tags = { Action = 'Eval' } }
    handlers.evaluate(msg1, {})
    
    -- Test with Test action
    local msg2 = { Tags = { Action = 'Test' } }
    handlers.evaluate(msg2, {})
    
    -- Test with no matching action
    local msg3 = { Tags = { Action = 'Other' } }
    handlers.evaluate(msg3, {})
    
    base.results = callResults
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_handlers_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"eval-called">>, <<"test-called">>], Result).

%% @doc Test the hyper-aos handlers evaluate function with maxRuns limit.
hyper_aos_handlers_evaluate_maxruns_test() ->
    Code = <<
"""
local handlers = require('.handlers')

function compute(base, req)
    local callCount = 0
    
    -- Add default handler
    handlers.add('_default', function(msg) return true end, function(msg) end)

    -- Add handler with maxRuns = 2
    handlers.add('limited-handler', function(msg) return true end, function(msg) 
        callCount = callCount + 1
    end, 2)
    
    local msg = { Tags = { Action = 'Test' } }
    
    -- Call evaluate 3 times
    handlers.evaluate(msg, {})
    handlers.evaluate(msg, {})
    handlers.evaluate(msg, {})
    
    -- Check if handler was removed after maxRuns
    local handlerExists = false
    for _, h in ipairs(handlers.list) do
        if h.name == 'limited-handler' then
            handlerExists = true
            break
        end
    end
    
    base.results = {
        tostring(callCount),
        tostring(handlerExists)
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_handlers_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"2">>, <<"false">>], Result).

%% @doc Test the hyper-aos handlers generateResolver function.
hyper_aos_handlers_generate_resolver_test() ->
    Code = <<
"""
local handlers = require('.handlers')

function compute(base, req)
    -- Test with function resolver
    local funcResolver = handlers.generateResolver(function(msg) 
        return 'function-result' 
    end)
    
    -- Test with table resolver
    local tableResolver = handlers.generateResolver({
        ['pattern1'] = function(msg) return 'pattern1-result' end,
        ['pattern2'] = function(msg) return 'pattern2-result' end
    })
    
    
    local funcResult = funcResolver({})
    local tableResult1 = tableResolver({ action = 'pattern1' })
    print('tableResult1')
    print(tableResult1)

    local tableResult2 = tableResolver({ action = 'pattern2' })
    -- print('tableResult2')
    -- print(tableResult2)
    base.results = {
        tostring(funcResult),
        tostring(tableResult1),
        tostring(tableResult2)
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_handlers_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([
        <<"function-result">>,
        <<"pattern1-result">>,
        <<"pattern2-result">>
    ], Result).

%% @doc Test the hyper-aos utils module matchesPattern function
hyper_aos_utils_matches_pattern_test() ->
    Code = <<
"""
local utils = require('.utils')

function compute(base, req)
    local msg = { Tags = { Action = 'Eval' } }
    base.results = {
        tostring(utils.matchesPattern('_', 'Foo', msg)),
        tostring(utils.matchesPattern('Eval', 'Eval', msg)),
        tostring(utils.matchesPattern('Eval', 'Test', msg)),
        tostring(utils.matchesPattern('^E.*', 'Eval', msg)),
        tostring(utils.matchesPattern(function(value) return value == 'Eval' end, 'Eval', msg)),
        tostring(utils.matchesPattern({'Eval', 'Test'}, 'Eval', msg)),
        tostring(utils.matchesPattern({'Foo', 'Bar'}, 'Eval', msg))
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_utils_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([
        <<"true">>,
        <<"true">>,
        <<"false">>,
        <<"true">>,
        <<"true">>,
        <<"true">>,
        <<"false">>
    ], Result).

%% @doc Test the hyper-aos utils module matchesSpec function
hyper_aos_utils_matches_spec_test() ->
    Code = <<
"""
local utils = require('.utils')

function compute(base, req)
    local msg1 = { Tags = { Action = 'Eval' }, body = { Data = 'test' } }
    local msg2 = { action = 'Eval' }
    local msg3 = { body = { action = 'Test' } }
    
    base.results = {
        tostring(utils.matchesSpec(msg1, { Action = 'Test' })),
        tostring(utils.matchesSpec(msg1, { Action = 'Eval' })),
        tostring(utils.matchesSpec(
            msg1,
            function(m) return m.Tags.Action == 'Eval' end
        )),
        tostring(utils.matchesSpec(msg1, { Data = 'test' })),
        tostring(utils.matchesSpec(msg2, 'Eval')),
        tostring(utils.matchesSpec(msg3, 'Test')),
        tostring(utils.matchesSpec(
            msg1,
            function(m) return m.Tags.Action == 'Eval' end
        ))
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_utils_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([
        <<"false">>,
        <<"false">>,
        <<"true">>,
        <<"true">>,
        <<"true">>,
        <<"true">>,
        <<"true">>
    ], Result).

%% @doc Test the hyper-aos utils module curry function
hyper_aos_utils_curry_test() ->
    Code = <<
"""
local utils = require('.utils')

function compute(base, req)
    local add = function(a, b, c) return a + b + c end
    local curriedAdd = utils.curry(add, 3)
    
    local result1 = curriedAdd(1)(2)(3)
    local result2 = curriedAdd(1, 2, 3)
    local partialAdd = curriedAdd(10)
    local result3 = partialAdd(20, 30)
    
    base.results = {
        tostring(result1),
        tostring(result2), 
        tostring(result3)
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_utils_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"6">>, <<"6">>, <<"60">>], Result).

%% @doc Test the hyper-aos utils module concat function
hyper_aos_utils_concat_test() ->
    Code = <<
"""
local utils = require('.utils')

function compute(base, req)
    local arr1 = {1, 2, 3}
    local arr2 = {4, 5, 6}
    local result = utils.concat(arr1, arr2)
    
    base.results = {
        tostring(#result),
        tostring(result[1]),
        tostring(result[4]),
        tostring(result[6])
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_utils_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"6">>, <<"1">>, <<"4">>, <<"6">>], Result).

%% @doc Test the hyper-aos utils module reduce function
hyper_aos_utils_reduce_test() ->
    Code = <<
"""
local utils = require('.utils')

function compute(base, req)
    local arr = {1, 2, 3, 4, 5}
    local sum =
        utils.reduce(function(acc, val) return acc + val end, 0, arr)
    local product =
        utils.reduce(function(acc, val) return acc * val end, 1, arr)
    
    base.results = {
        tostring(sum),
        tostring(product)
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_utils_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"15">>, <<"120">>], Result).

%% @doc Test the hyper-aos utils module map function
hyper_aos_utils_map_test() ->
    Code = <<
"""
local utils = require('.utils')

function compute(base, req)
    local arr = {1, 2, 3}
    local doubled = utils.map(function(x) return x * 2 end, arr)
    local squares = utils.map(function(x) return x * x end, arr)
    
    base.results = {
        tostring(doubled[3]),
        tostring(squares[3])
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_utils_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"6">>, <<"9">>], Result).

%% @doc Test the hyper-aos utils module filter function
hyper_aos_utils_filter_test() ->
    Code = <<
"""
local utils = require('.utils')

function compute(base, req)
    local arr = {1, 2, 3, 4, 5, 6}
    local evens = utils.filter(function(x) return x % 2 == 0 end, arr)
    local greaterThan3 = utils.filter(function(x) return x > 3 end, arr)
    
    base.results = {
        tostring(#evens),
        tostring(evens[1]),
        tostring(evens[2]),
        tostring(#greaterThan3),
        tostring(greaterThan3[1])
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_utils_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"3">>, <<"2">>, <<"4">>, <<"3">>, <<"4">>], Result).

%% @doc Test the hyper-aos utils module find function
hyper_aos_utils_find_test() ->
    Code = <<
"""
local utils = require('.utils')

function compute(base, req)
    local arr = {1, 2, 3, 4, 5}
    local found = utils.find(function(x) return x > 3 end, arr)
    local notFound = utils.find(function(x) return x > 10 end, arr)
    
    base.results = {
        tostring(found or 'nil'),
        tostring(notFound or 'nil')
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_utils_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"4">>, <<"nil">>], Result).

%% @doc Test the hyper-aos utils module propEq function
hyper_aos_utils_prop_eq_test() ->
    Code = <<
"""
local utils = require('.utils')

function compute(base, req)
    local Tags = { Foo = 'Bar', Key = 'Value' }
    local checkFoo = utils.propEq('Foo', 'Bar', Tags)
    local checkKey = utils.propEq('Key', 'Bar', Tags)
    
    base.results = {
        tostring(checkFoo),
        tostring(checkKey)
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_utils_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"true">>, <<"false">>], Result).

%% @doc Test the hyper-aos utils module reverse function
hyper_aos_utils_reverse_test() ->
    Code = <<
"""
local utils = require('.utils')

function compute(base, req)
    local arr = {1, 2, 3, 4, 5}
    local reversed = utils.reverse(arr)
    
    base.results = {
        tostring(reversed[1]),
        tostring(reversed[2]),
        tostring(reversed[3]),
        tostring(reversed[4]),
        tostring(reversed[5])
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_utils_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"5">>, <<"4">>, <<"3">>, <<"2">>, <<"1">>], Result).

%% @doc Test the hyper-aos utils module compose function
hyper_aos_utils_compose_test() ->
    Code = <<
"""
local utils = require('.utils')

function compute(base, req)
    local add1 = function(x) return x + 1 end
    local mult2 = function(x) return x * 2 end
    local composed = utils.compose(add1, mult2)
    
    local result1 = composed(5)  -- (5 * 2) + 1 = 11
    local result2 = utils.compose(mult2, add1)(5)  -- (5 + 1) * 2 = 12
    
    base.results = {
        tostring(result1),
        tostring(result2)
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_utils_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"11">>, <<"12">>], Result).

%% @doc Test the hyper-aos utils module prop function
hyper_aos_utils_prop_test() ->
    Code = <<
"""
local utils = require('.utils')

function compute(base, req)
    local Tags = { Foo = 'Bar', Key = 'Value' }
    local getFoo = utils.prop('Foo')
    local getKey = utils.prop('Key')
    
    base.results = {
        tostring(getFoo(Tags)),
        tostring(getKey(Tags))
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_utils_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"Bar">>, <<"Value">>], Result).

%% @doc Test the hyper-aos utils module includes function
hyper_aos_utils_includes_test() ->
    Code = <<
"""
local utils = require('.utils')

function compute(base, req)
    local arr = {'apple', 'banana', 'cherry'}
    base.results = {
        tostring(utils.includes('banana')(arr)),
        tostring(utils.includes('grape')(arr)),
        tostring(utils.includes('apple', arr)),
        tostring(utils.includes(42, {1, 2, 3, 42, 5}))
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_utils_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"true">>, <<"false">>, <<"true">>, <<"true">>], Result).

%% @doc Test the hyper-aos utils module
hyper_aos_utils_keys_test() ->
    Code = <<
"""
local utils = require('.utils')

function compute(base, req)
    base.results = utils.keys({ Foo = 'Bar', Key = 'Value' })
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_utils_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual(lists:member(<<"Foo">>, Result), true),
    ?assertEqual(lists:member(<<"Key">>, Result), true),
    ?assertEqual(lists:member(<<"Value">>, Result), false).


%% @doc Test the hyper-aos utils module values function
hyper_aos_utils_values_test() ->
    Code = <<
"""
local utils = require('.utils')

function compute(base, req)
    base.results = utils.values({ Foo = 'Bar', Key = 'Value' })
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_utils_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual(lists:member(<<"Bar">>, Result), true),
    ?assertEqual(lists:member(<<"Value">>, Result), true),
    ?assertEqual(lists:member(<<"Foo">>, Result), false).

%% @doc Test the hyper-aos utils module Tab function
hyper_aos_utils_tab_test() ->
    Code = <<
"""
local utils = require('.utils')

function compute(base, req)
    local msg = {
        Tags = {
            { name = 'Action', value = 'Eval' },
            { name = 'Target', value = 'Process-123' },
            { name = 'From', value = 'User-456' }
        }
    }
    
    local tab = utils.Tab(msg)
    
    base.results = {
        tostring(tab.Action),
        tostring(tab.Target),
        tostring(tab.From)
    }
    return base
end
"""
>>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_utils_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"Eval">>, <<"Process-123">>, <<"User-456">>], Result).

%% @doc Test the hyper-aos ao module.

hyper_aos_ensure_owner_test() ->
    Wallet = hb:wallet(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Opts = #{ priv_wallet => Wallet },
    Process = generate_lua_process("test/hyper-aos.lua", Opts),
    {ok, _Proc} = hb_cache:write(Process, Opts),
    Code = """
      return Owner
    """,
    Message = generate_test_message(Process, Opts, Code),
    {ok, _Assignment} = hb_ao:resolve(Process, Message, Opts#{ hashpath => ignore }),
    {ok, Owner} = hb_ao:resolve(Process, <<"now/results/output/data">>, Opts),
    ?assertEqual(Address, Owner),
    ok.

hyper_aos_ensure_id_test() ->
    Wallet = hb:wallet(),
    Opts = #{ priv_wallet => Wallet },
    Process = generate_lua_process("test/hyper-aos.lua", Opts),
    {ok, _Proc} = hb_cache:write(Process, Opts),
    Code = """
    return aos.id
    """,
    Message = generate_test_message(Process, Opts, Code),
    {ok, _Assignment} = hb_ao:resolve(Process, Message, Opts#{ hashpath => ignore }),
    {ok, AosId} = hb_ao:resolve(Process, <<"now/results/output/data">>, Opts),
    {ok, Committers} = hb_ao:resolve(Process, <<"commitments">>, Opts),
    ProcessId = find_key_with_type_rsa_pss_sha512(Committers),
    ?assertEqual(AosId, ProcessId).

%% @doc Test the hyper-aos ao module clearOutbox function
hyper_aos_ao_clear_outbox_test() ->
    Code = <<
"""
local ao = require('.ao')

function compute(base, req)
    -- Add some data to outbox
    ao.outbox.Messages = {{target = 'test', data = 'hello'}}
    ao.outbox.Spawns = {{module = 'test'}}
    ao.outbox.Assignments = {{process = 'test'}}
    ao.outbox.Output = {data = 'output'}
    
    -- Clear outbox
    ao.clearOutbox()
    
    base.results = {
        tostring(#ao.outbox.Messages),
        tostring(#ao.outbox.Spawns), 
        tostring(#ao.outbox.Assignments),
        tostring(ao.outbox.Output.data or 'nil')
    }
    return base
end
"""
    >>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_ao_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"0">>, <<"0">>, <<"0">>, <<"nil">>], Result).

%% @doc Test the hyper-aos ao module send function
hyper_aos_ao_send_test() ->
    Code = <<
"""
local ao = require('.ao')

function compute(base, req)
    -- Test basic send
    local msg1 = ao.send({target = 'process1', data = 'hello'})
    local msg2 = ao.send({target = 'process2', data = 'world'})
    
    base.results = {
        tostring(#ao.outbox.Messages),
        tostring(ao.outbox.Messages[1].target),
        tostring(ao.outbox.Messages[1].data),
        tostring(ao.outbox.Messages[1].reference),
        tostring(ao.outbox.Messages[2].target),
        tostring(msg1.reference),
        tostring(msg2.reference),
        tostring(type(msg1.onReply))
    }
    return base
end
"""
    >>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_ao_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([
        <<"2">>,
        <<"process1">>,
        <<"hello">>,
        <<"1">>,
        <<"process2">>,
        <<"1">>,
        <<"2">>,
        <<"function">>
    ], Result).

%% @doc Test the hyper-aos ao module spawn function
hyper_aos_ao_spawn_test() ->
    Code = <<
"""
local ao = require('.ao')

function compute(base, req)
    -- Test spawn
    local spawn1 = ao.spawn('module1', {data = 'init data'})
    local spawn2 = ao.spawn('module2', {tags = {action = 'start'}})
    
    base.results = {
        tostring(#ao.outbox.Spawns),
        tostring(ao.outbox.Spawns[1].data),
        tostring(ao.outbox.Spawns[1].reference),
        tostring(ao.outbox.Spawns[2].tags.action),
        tostring(spawn1.reference),
        tostring(spawn2.reference),
        tostring(type(spawn1.onReply))
    }
    return base
end
"""
    >>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_ao_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([
        <<"2">>,
        <<"init data">>,
        <<"1">>,
        <<"start">>,
        <<"1">>,
        <<"2">>,
        <<"function">>
    ], Result).

%% @doc Test the hyper-aos ao module registerHint function
hyper_aos_ao_register_hint_test() ->
    Code = <<
"""
local ao = require('.ao')

function compute(base, req)
    -- Test registerHint with From-Process tag
    local msg1 = {
        Tags = {
            ['From-Process'] = 'process123&hint=hint1&ttl=3600'
        }
    }
    local msg2 = {
        Tags = {
            ['From-Process'] = 'process456&hint=hint2&ttl=7200'
        }
    }
    
    ao.registerHint(msg1)
    ao.registerHint(msg2)
    
    base.results = {
        tostring(ao._hints['process123'].hint),
        tostring(ao._hints['process123'].ttl),
        tostring(ao._hints['process456'].hint),
        tostring(ao._hints['process456'].ttl)
    }
    return base
end
"""
    >>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_ao_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([
        <<"hint1">>,
        <<"3600">>,
        <<"hint2">>,
        <<"7200">>
    ], Result).

%% @doc Test the hyper-aos ao module result function
hyper_aos_ao_result_test() ->
    Code = <<
"""
local ao = require('.ao')

function compute(base, req)
    -- Add some messages to outbox
    ao.send({target = 'test1', data = 'msg1'})
    ao.spawn('module1', {data = 'spawn1'})
    
    -- Test result with normal output
    local result1 = ao.result({Output = 'success'})
    
    -- Test result with error
    local result2 = ao.result({Error = 'error'})
    
    base.results = {
        tostring(result1.Output),
        tostring(#result1.Messages),
        tostring(#result1.Spawns),
        tostring(result2.Error)
    }
    return base
end
"""
    >>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_ao_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([
        <<"success">>,
        <<"1">>,
        <<"1">>,
        <<"error">>
    ], Result).

%% @doc Test the hyper-aos ao module initialization
hyper_aos_ao_init_test() ->
    Code = <<
"""
local ao = require('.ao')

function compute(base, req)
    -- Test ao object properties after init
    ao.init({
        process = {
            id = 'process123',
            authority = 'authority123,authority456',
            commitments = {
                ['key1'] = {
                    type = 'rsa-pss-sha512',
                    commitment = 'commitment123'
                },
                ['key2'] = {
                    type = 'hmac-sha256',
                    commitment = 'commitment456'
                }
            }
        }
    })
    base.results = {
        tostring(ao._version),
        tostring(type(ao.id)),
        tostring(ao.id),
        tostring(type(ao.authorities)),
        tostring(#ao.authorities),    
        tostring(ao.reference),
        tostring(type(ao.outbox)),
        tostring(type(ao.send)),
        tostring(type(ao.spawn))
    }
    return base
end
"""
    >>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_ao_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([
        <<"0.0.6">>,
        <<"string">>,
        <<"key1">>,
        <<"table">>,
        <<"2">>,
        <<"0">>,
        <<"table">>,
        <<"function">>,
        <<"function">>
    ], Result).
%%% @doc Test the hyper-aos assignment module

%% @doc Test the hyper-aos addAssignable function
hyper_aos_assignment_add_assignable_test() ->
    Code = <<
"""
local assignment = require('.assignment')
local ao = require('.ao')

function compute(base, req)
    -- Initialize assignment module
    assignment.init(ao)
    
    -- Test adding assignable with name and matchSpec
    ao.addAssignable('test-assignable', {Action = 'Test'})
    
    -- Test adding assignable with just matchSpec (no name)
    ao.addAssignable({Action = 'NoName'})
    
    -- Test updating existing assignable
    ao.addAssignable('test-assignable', {Action = 'Updated'})
    
    base.results = {
        tostring(#ao.assignables),
        tostring(ao.assignables[1].name),
        tostring(ao.assignables[1].pattern.Action),
        tostring(ao.assignables[2].name or 'nil'),
        tostring(ao.assignables[2].pattern.Action)
    }
    return base
end
"""
    >>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_assignment_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([
        <<"2">>,
        <<"test-assignable">>,
        <<"Updated">>,
        <<"nil">>,
        <<"NoName">>
    ], Result).

%% @doc Test the hyper-aos assignment module removeAssignable function
hyper_aos_assignment_remove_assignable_test() ->
    Code = <<
"""
local assignment = require('.assignment')
local ao = require('.ao')

function compute(base, req)
    -- Initialize assignment module
    assignment.init(ao)
    
    -- Add multiple assignables
    ao.addAssignable('first', {Action = 'First'})
    ao.addAssignable('second', {Action = 'Second'})
    ao.addAssignable('third', {Action = 'Third'})
    
    local beforeRemove = #ao.assignables
    
    -- Remove by name
    ao.removeAssignable('second')
    
    local afterNameRemove = #ao.assignables
    
    -- Remove by index
    ao.removeAssignable(1)
    
    base.results = {
        tostring(beforeRemove),
        tostring(afterNameRemove),
        tostring(#ao.assignables),
        tostring(ao.assignables[1].name),
        tostring(ao.assignables[1].pattern.Action)
    }
    return base
end
"""
    >>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_assignment_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([
        <<"3">>,
        <<"2">>,
        <<"1">>,
        <<"third">>,
        <<"Third">>
    ], Result).

%% @doc Test the hyper-aos assignment module isAssignment function
hyper_aos_assignment_is_assignment_test() ->
    Code = <<
"""
local assignment = require('.assignment')
local ao = require('.ao')

function compute(base, req)
    -- Initialize assignment module
    assignment.init(ao)
    
    -- Set ao.id for testing
    ao.id = 'process123'
    
    -- Test messages
    local msg1 = {Target = 'process123'}  -- Same as ao.id
    local msg2 = {Target = 'other-process'}  -- Different from ao.id
    local msg3 = {}  -- No target
    
    base.results = {
        tostring(ao.isAssignment(msg1)),
        tostring(ao.isAssignment(msg2)),
        tostring(ao.isAssignment(msg3))
    }
    return base
end
"""
    >>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_assignment_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([
        <<"false">>,
        <<"true">>,
        <<"true">>
    ], Result).

%% @doc Test the hyper-aos assignment module isAssignable function
hyper_aos_assignment_is_assignable_test() ->
    Code = <<
"""
local assignment = require('.assignment')
local ao = require('.ao')

function compute(base, req)
    -- Initialize assignment module
    assignment.init(ao)
    
    -- Add assignables
    ao.addAssignable('eval-assignable', {Action = 'Eval'})
    ao.addAssignable('data-assignable', {Data = 'test'})
    
    -- Test messages
    local msg1 = {Action = 'Eval'}  -- Should match first assignable
    local msg2 = {Data = 'test'}    -- Should match second assignable
    local msg3 = {Action = 'Other'} -- Should not match any
    local msg4 = {}                 -- Should not match any
    
    base.results = {
        tostring(ao.isAssignable(msg1)),
        tostring(ao.isAssignable(msg2)),
        tostring(ao.isAssignable(msg3)),
        tostring(ao.isAssignable(msg4))
    }
    return base
end
"""
    >>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_assignment_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([
        <<"true">>,
        <<"true">>,
        <<"false">>,
        <<"false">>
    ], Result).

%% @doc Test the hyper-aos assignment module with empty assignables
hyper_aos_assignment_empty_assignables_test() ->
    Code = <<
"""
local assignment = require('.assignment')
local ao = require('.ao')

function compute(base, req)
    -- Initialize assignment module
    assignment.init(ao)
    
    -- Test with empty assignables (default behavior)
    local msg = {Action = 'Test'}
    
    base.results = {
        tostring(#ao.assignables),
        tostring(ao.isAssignable(msg))
    }
    return base
end
"""
    >>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_assignment_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([
        <<"0">>,
        <<"false">>
    ], Result).

%% @doc Test the hyper-aos assignment module initialization
hyper_aos_assignment_init_test() ->
    Code = <<
"""
local assignment = require('.assignment')
local ao = require('.ao')

function compute(base, req)
    -- Test module properties
    base.results = {
        tostring(assignment._version),
        tostring(type(assignment.init))
    }
    
    -- Initialize and test aos properties
    assignment.init(ao)
    
    table.insert(base.results, tostring(type(ao.assignables)))
    table.insert(base.results, tostring(type(ao.addAssignable)))
    table.insert(base.results, tostring(type(ao.removeAssignable)))
    table.insert(base.results, tostring(type(ao.isAssignment)))
    table.insert(base.results, tostring(type(ao.isAssignable)))
    
    return base
end
"""
    >>,
    {ok, Process, Opts} =
        generate_hyper_aos_modular_assignment_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([
        <<"0.1.0">>,
        <<"function">>,
        <<"table">>,
        <<"function">>,
        <<"function">>,
        <<"function">>,
        <<"function">>
    ], Result).


% Test for aos-process.lua
hyper_aos_process_test() ->
    Code = <<
"""
local process  = require('.process')
local state    = require('.state')
local json     = require('.json')

local function make_req(action, data)
  return {
    ['block-timestamp'] = 0,
    body = {
      ['Content-Type'] = 'application/json',
      action           = action,
      data             = json.encode(data)
    },
    Tags = {}
  }
end

local base_ctx = {
  process = {
    authority   = 'owner',
    commitments = { key1 = { alg = 'rsa-pss-sha512', committer = 'owner' } }
  }
}

-- run process.handle in both untrusted and trusted branches
state.isTrusted = function(_) return false end
local untrusted = process.handle(make_req('Eval', '1+1'), base_ctx)

state.isTrusted = function(_) return true end
_G.Owner = 'owner'
local trusted   = process.handle(make_req('Ping', 'hello'), base_ctx)

function compute(base, _req)
  base.results = {
    tostring(Prompt()),                   
    tostring(process._version),           
    tostring(untrusted.Output.data),      
    tostring(trusted.Output.prompt),      
    tostring(type(trusted.Output.data)),  
    tostring(#trusted.Messages),          
    tostring(#trusted.Spawns),            
    tostring(type(trusted.Assignments)),  
    tostring(trusted.Outbox == nil),      
    tostring(type(untrusted.Output) == 'table'),
    tostring(type(trusted.Output) == 'table') 
  }
  return base
end
"""
    >>,
    {ok, Process, Opts} =generate_hyper_aos_modular_process_to_test_main_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual(
        [
            <<"aos> ">>,                      
            <<"2.0.7">>,                      
            <<"Message is not trusted.">>,    
            <<"aos> ">>,                      
            <<"string">>,                     
            <<"0">>, <<"0">>,                 
            <<"table">>,                      
            <<"true">>, 
            <<"true">>, 
            <<"true">>
        ],
        Result
    ).


%% aos-process: trusted Eval success
hyper_aos_process_eval_success_test() ->
    Code = <<
"""
local process = require('.process')
local state   = require('.state')

local function make_req(code)
  return {
    ['block-timestamp'] = 0,
    body = {
      ['Content-Type'] = 'text/plain',
      action           = 'Eval',
      data             = code,
      commitments      = { key1 = { alg = 'rsa-pss-sha512', committer = 'owner' } }
    },
    Tags = {}
  }
end

local base_ctx = {
  process = {
    authority   = 'owner',
    commitments = { key1 = { alg = 'rsa-pss-sha512', committer = 'owner' } }
  }
}

state.isTrusted = function(_) return true end
_G.Owner = 'owner'
local res = process.handle(make_req('return 2+2'), base_ctx)

function compute(base, _req)
  local str = tostring(res.Output.data)
  base.results = { str }
  return base
end
""" >>,
    {ok, Process, Opts} = generate_hyper_aos_modular_process_to_test_main_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?event(aos_process_eval_success_test_result, Result),
    ?assertEqual([<<"4">>], Result).

%% aos-process: Eval error path returns Error
hyper_aos_process_eval_error_test() ->
    Code = <<
"""
local process = require('.process')
local state   = require('.state')

local function make_req(code)
  return {
    ['block-timestamp'] = 0,
    body = {
      ['Content-Type'] = 'text/plain',
      action           = 'Eval',
      data             = code,
      commitments      = { key1 = { alg = 'rsa-pss-sha512', committer = 'owner' } }
    },
    Tags = {}
  }
end

local base_ctx = {
  process = {
    authority   = 'owner',
    commitments = { key1 = { alg = 'rsa-pss-sha512', committer = 'owner' } }
  }
}

state.isTrusted = function(_) return true end
_G.Owner = 'owner'
local res = process.handle(make_req('return invalid+'), base_ctx)

function compute(base, _req)
  base.results = { tostring(res.Error ~= nil) }
  return base
end
""" >>,
    {ok, Process, Opts} = generate_hyper_aos_modular_process_to_test_main_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"true">>], Result).


%% aos-string-ext.lua unit-test
hyper_aos_string_ext_gmatch_test() ->
    Code = <<
"""
local string_ext = require('.string-ext')
function compute(base, req)
    local s = "a,b,c"
    local t = {}
    for m in string.gmatch(s, "[^,]+") do
        table.insert(t, m)
    end
    base.results = {
        tostring(#t), -- 3 
        tostring(t[1]), -- a
        tostring(t[2]), -- b
        tostring(t[3]) -- c
    }
    return base
end
""" >>,
    {ok, Process, Opts} = generate_hyper_aos_modular_string_ext_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    %?event(aos_string_ext_gmatch_test_result, Result),
    ?assertEqual([<<"3">>, <<"a">>, <<"b">>, <<"c">>], Result).

hyper_aos_string_ext_captures_test() ->
    Code = <<
"""
local string_ext = require('.string-ext')
function compute(base, req)
    local out = {}
    for user, domain in string.gmatch(
        "user.name+tag@example-domain.org",
        "([^@]+)@([^@]+)"
    ) do
        table.insert(out, user)
        table.insert(out, domain)
    end
    base.results = {
        tostring(out[1]),
        tostring(out[2])
    }
    return base
end
""" >>,
    {ok, Process, Opts} = generate_hyper_aos_modular_string_ext_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    %?event(aos_string_ext_captures_test_result, Result),
    ?assertEqual([
        <<"user.name+tag">>, 
        <<"example-domain.org">>
    ], Result).

%% aos-state: insertInbox & reset test
hyper_aos_state_insert_reset_test() ->
    Code = <<
"""
local state = require('.state')
function compute(base, _req)
    Inbox = {}
    state.insertInbox({id=1})
    state.insertInbox({id=2})
    state.insertInbox({id=3})
    local count_before = #Inbox
    Inbox = state.reset(Inbox)
    local count_after = #Inbox
    base.results = { tostring(count_before), tostring(count_after) }
    return base
end
""" >>,
    {ok, Process, Opts} = generate_hyper_aos_modular_state_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"3">>, <<"0">>], Result).

%% aos-state: getFrom & isTrusted test
hyper_aos_state_getfrom_trusted_test() ->
    Code = <<
"""
local state = require('.state')
local aos   = require('.ao')
function compute(base, _req)
    aos.authorities = { 'alice' }
    local req = {
        body = {
            ['from-process'] = 'alice',
            commitments = {}
        }
    }
    local from    = state.getFrom(req)
    local trusted = state.isTrusted(req)
    base.results = { tostring(from), tostring(trusted) }
    return base
end
""" >>,
    {ok, Process, Opts} = generate_hyper_aos_modular_state_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"alice">>, <<"true">>], Result).
    
% Test helpers
%% @doc Generate a Lua process message.
generate_lua_process(File, Opts) ->
    NormOpts = Opts#{ priv_wallet => hb_opts:get(priv_wallet, hb:wallet(), Opts) },
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), NormOpts),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    {ok, Module} = file:read_file(File),
    hb_message:commit(
        #{
            <<"device">> => <<"process@1.0">>,
            <<"type">> => <<"Process">>,
            <<"scheduler-device">> => <<"scheduler@1.0">>,
            <<"execution-device">> => <<"lua@5.3a">>,
            <<"module">> => #{
                <<"content-type">> => <<"application/lua">>,
                <<"body">> => Module
            },
            <<"authority">> => [ 
                Address, 
                <<"E3FJ53E6xtAzcftBpaw2E1H4ZM9h6qy6xz9NXh5lhEQ">>
            ], 
            <<"scheduler-location">> =>
                hb_util:human_id(ar_wallet:to_address(Wallet)),
            <<"test-random-seed">> => rand:uniform(1337)
        },
        NormOpts
    ).

generate_test_message(Process, Opts) ->
    Code = """ 
Count = 0
function add() 
  Send({Target = 'Foo', Data = 'Bar' });
  Count = Count + 1 
end
add()
return Count
""",
    generate_test_message(Process, Opts, Code).
%% @doc Generate a test message for a Lua process.
generate_test_message(Process, Opts, Code) ->
    ProcID = hb_message:id(Process, all),
    NormOpts = Opts#{ priv_wallet => hb_opts:get(priv_wallet, hb:wallet(), Opts) },
    hb_message:commit(#{
            <<"path">> => <<"schedule">>,
            <<"method">> => <<"POST">>,
            <<"body">> =>
                hb_message:commit(
                    #{
                        <<"target">> => ProcID,
                        <<"type">> => <<"Message">>,
                        <<"body">> => #{
                            <<"content-type">> => <<"application/lua">>,
                            <<"body">> => hb_util:bin(Code) 
                        },
                        <<"random-seed">> => rand:uniform(1337),
                        <<"action">> => <<"Eval">>
                    },
                    NormOpts
                )
        },
        NormOpts
    ).

%% @doc Generate a stack message for the Lua process.
generate_stack(File) ->
    Wallet = hb:wallet(),
    {ok, Module} = file:read_file(File),
    Msg1 = #{
        <<"device">> => <<"Stack@1.0">>,
        <<"device-stack">> =>
            [
                <<"json-iface@1.0">>,
                <<"lua@5.3a">>,
                <<"multipass@1.0">>
            ],
        <<"function">> => <<"json_result">>,
        <<"passes">> => 2,
        <<"stack-keys">> => [<<"init">>, <<"compute">>],
        <<"module">> => Module,
        <<"process">> => 
            hb_message:commit(#{
                <<"type">> => <<"Process">>,
                <<"module">> => #{
                    <<"content-type">> => <<"application/lua">>,
                    <<"body">> => Module
                },
                <<"scheduler">> => hb:address(),
                <<"authority">> => hb:address()
            }, Wallet)
    },
    {ok, Msg2} = hb_ao:resolve(Msg1, <<"init">>, #{}),
    Msg2.

create_modules(Modules) ->
    Template = #{
        <<"content-type">> => <<"application/lua">>,
        <<"body">> => undefined
    },
    lists:map(fun(Binary) ->
        maps:map(fun(Key, _Value) ->
            case Key of
                <<"content-type">> -> <<"application/lua">>;
                <<"body">> -> Binary
            end
        end, Template)
    end, Modules).

generate_hyper_aos_modular_process(Codes, Wallet) ->
    hb_message:commit(
        #{
            <<"device">> => <<"process@1.0">>,
            <<"type">> => <<"Process">>,
            <<"method">> => <<"POST">>,
            <<"scheduler-device">> => <<"scheduler@1.0">>,
            <<"execution-device">> => <<"lua@5.3a">>,
            <<"module">> => create_modules(Codes),
            <<"authority">> => [ 
                hb:address(), 
                <<"E3FJ53E6xtAzcftBpaw2E1H4ZM9h6qy6xz9NXh5lhEQ">>
            ], 
            <<"scheduler-location">> => hb:address(),
            <<"test-random-seed">> => rand:uniform(1337)
            },
        Wallet
    ).
generate_hyper_aos_modular_handlers_utils_process(Code) -> 
    Wallet = hb:wallet(),
    Opts = #{ priv_wallet => Wallet },
    {ok, UtilsJson} = file:read_file("scripts/aos-utils.lua"),
    {ok, HandlersUtilsJson} = file:read_file("scripts/aos-handlers-utils.lua"),
    Process = generate_hyper_aos_modular_process(
        [UtilsJson, HandlersUtilsJson, Code],
        Wallet
    ),
    Message = generate_test_message(Process, Opts, <<"">>),
    hb_cache:write(Process, Opts),
    hb_ao:resolve(Process, Message, Opts#{ hashpath => ignore }),
    {ok, Process, Opts}.
generate_hyper_aos_modular_handlers_process(Code) -> 
    Wallet = hb:wallet(),
    Opts = #{ priv_wallet => Wallet },
    {ok, UtilsJson} = file:read_file("scripts/aos-utils.lua"),
    {ok, HandlersUtilsJson} = file:read_file("scripts/aos-handlers-utils.lua"),
    {ok, HandlersJson} = file:read_file("scripts/aos-handlers.lua"),
    Process = generate_hyper_aos_modular_process(
        [UtilsJson, HandlersUtilsJson, HandlersJson, Code],
        Wallet
    ),
    Message = generate_test_message(Process, Opts, <<"">>),
    hb_cache:write(Process, Opts),
    hb_ao:resolve(Process, Message, Opts#{ hashpath => ignore }),
    {ok, Process, Opts}.
generate_hyper_aos_modular_utils_process(Code) -> 
    Wallet = hb:wallet(),
    Opts = #{ priv_wallet => Wallet },
    {ok, UtilsJson} = file:read_file("scripts/aos-utils.lua"),
    Process = generate_hyper_aos_modular_process(
        [UtilsJson, Code],
        Wallet
    ),
    Message = generate_test_message(Process, Opts, <<"">>),
    hb_cache:write(Process, Opts),
    hb_ao:resolve(Process, Message, Opts#{ hashpath => ignore }),
    {ok, Process, Opts}.

generate_hyper_aos_modular_ao_process(Code) -> 
    Wallet = hb:wallet(),
    Opts = #{ priv_wallet => Wallet },
    {ok, StringExtJson} = file:read_file("scripts/aos-string-ext.lua"),
    {ok, UtilsJson} = file:read_file("scripts/aos-utils.lua"),
    {ok, HandlersUtilsJson} = file:read_file("scripts/aos-handlers-utils.lua"),
    {ok, HandlersJson} = file:read_file("scripts/aos-handlers.lua"),
    {ok, AoJson} = file:read_file("scripts/aos-ao.lua"),
    Process = generate_hyper_aos_modular_process(
        [StringExtJson, UtilsJson, HandlersUtilsJson, HandlersJson, AoJson, Code],
        Wallet
    ),
    Message = generate_test_message(Process, Opts, <<"">>),
    hb_cache:write(Process, Opts),
    hb_ao:resolve(Process, Message, Opts#{ hashpath => ignore }),
    {ok, Process, Opts}.

generate_hyper_aos_modular_assignment_process(Code) -> 
    Wallet = hb:wallet(),
    Opts = #{ priv_wallet => Wallet },
    {ok, UtilsJson} = file:read_file("scripts/aos-utils.lua"),
    {ok, HandlersUtilsJson} = file:read_file("scripts/aos-handlers-utils.lua"),
    {ok, HandlersJson} = file:read_file("scripts/aos-handlers.lua"),
    {ok, AoJson} = file:read_file("scripts/aos-ao.lua"),
    {ok, AssignmentJson} = file:read_file("scripts/aos-assignment.lua"),
    Process = generate_hyper_aos_modular_process(
        [UtilsJson, HandlersUtilsJson, HandlersJson, AoJson, AssignmentJson, Code],
        Wallet
    ),
    Message = generate_test_message(Process, Opts, <<"">>),
    hb_cache:write(Process, Opts),
    hb_ao:resolve(Process, Message, Opts#{ hashpath => ignore }),
    {ok, Process, Opts}.

%% Utilize aos-process.lua to generate a process that can be used to test the main process
generate_hyper_aos_modular_process_to_test_main_process(Code) ->
    Wallet = hb:wallet(),
    Opts = #{ priv_wallet => Wallet },
    {ok, StringExtJson} = file:read_file("scripts/aos-string-ext.lua"),
    {ok, JsonJson} = file:read_file("scripts/aos-json.lua"),
    {ok, StringifyJson} = file:read_file("scripts/aos-stringify.lua"),
    {ok, EvalJson} = file:read_file("scripts/aos-eval.lua"),
    {ok, AssignmentJson} = file:read_file("scripts/aos-assignment.lua"),
    {ok, StateJson} = file:read_file("scripts/aos-state.lua"),
    {ok, DumpJson} = file:read_file("scripts/aos-dump.lua"),
    {ok, DefaultJson} = file:read_file("scripts/aos-default.lua"),
    {ok, UtilsJson} = file:read_file("scripts/aos-utils.lua"),
    {ok, HandlersUtilsJson} = file:read_file("scripts/aos-handlers-utils.lua"),
    {ok, HandlersJson} = file:read_file("scripts/aos-handlers.lua"),
    {ok, ProcessJson} = file:read_file("scripts/aos-process.lua"),
    {ok, AoJson} = file:read_file("scripts/aos-ao.lua"),
    Process = generate_hyper_aos_modular_process(
        [
            JsonJson,          % .json     – foundation
            UtilsJson,         % .utils    – foundation
            HandlersUtilsJson, % .handlers-utils (needs .utils)
            HandlersJson,      % .handlers (needs .utils + .handlers-utils)
            AoJson,            % .ao       (needs .utils + .handlers)
            StringExtJson,     % .string-ext (optional utility)
            StringifyJson,     % .stringify (needs .utils)
            DumpJson,          % .dump
            DefaultJson,       % .default  (needs .json)
            EvalJson,          % .eval     (needs .stringify + .json)
            StateJson,         % .state    (needs .utils + .stringify + .ao)
            AssignmentJson,    % .assignment (needs .utils)
            ProcessJson,       % .process  (needs .ao + many of the above)
            Code               % test code
        ],
        Wallet
    ),
    Message = generate_test_message(Process, Opts, <<"">>),
    hb_cache:write(Process, Opts),
    hb_ao:resolve(Process, Message, Opts#{ hashpath => ignore }),
    {ok, Process, Opts}.

find_key_with_type_rsa_pss_sha512(Map) when is_map(Map) ->
    lists:foldl(
        fun({Key, #{<<"type">> := <<"rsa-pss-sha512">>}}, Acc) when Acc =:= undefined ->
                Key;
            (_, Acc) ->
                Acc
        end,
        undefined,
        maps:to_list(Map)
    ).


%% aos-string-ext process
generate_hyper_aos_modular_string_ext_process(Code) ->
    Wallet = hb:wallet(),
    Opts   = #{ priv_wallet => Wallet },
    {ok, StringExtJson} = file:read_file("scripts/aos-string-ext.lua"),
    Process = generate_hyper_aos_modular_process([
        StringExtJson,
        Code
    ], Wallet),
    Message = generate_test_message(Process, Opts, <<"">>),
    hb_cache:write(Process, Opts),
    hb_ao:resolve(Process, Message, Opts#{ hashpath => ignore }),
    {ok, Process, Opts}.


%% aos-state process
generate_hyper_aos_modular_state_process(Code) ->
    Wallet = hb:wallet(),
    Opts   = #{ priv_wallet => Wallet },
    {ok, JsonJson}        = file:read_file("scripts/aos-json.lua"),
    {ok, UtilsJson}       = file:read_file("scripts/aos-utils.lua"),
    {ok, StringifyJson}   = file:read_file("scripts/aos-stringify.lua"),
    {ok, HandlersUtils}   = file:read_file("scripts/aos-handlers-utils.lua"),
    {ok, HandlersJson}    = file:read_file("scripts/aos-handlers.lua"),
    {ok, AoJson}          = file:read_file("scripts/aos-ao.lua"),
    {ok, StateJson}       = file:read_file("scripts/aos-state.lua"),
    Process = generate_hyper_aos_modular_process([
        JsonJson,
        UtilsJson,
        StringifyJson,
        HandlersUtils,
        HandlersJson,
        AoJson,
        StateJson,
        Code
    ], Wallet),
    Message = generate_test_message(Process, Opts, <<"">>),
    hb_cache:write(Process, Opts),
    hb_ao:resolve(Process, Message, Opts#{ hashpath => ignore }),
    {ok, Process, Opts}.

%% aos-boot process generator
generate_hyper_aos_modular_boot_process(Code) ->
    Wallet = hb:wallet(),
    Opts = #{ priv_wallet => Wallet },
    {ok, JsonJson}        = file:read_file("scripts/aos-json.lua"),
    {ok, UtilsJson}       = file:read_file("scripts/aos-utils.lua"),
    {ok, StringifyJson}   = file:read_file("scripts/aos-stringify.lua"),
    {ok, HandlersUtils}   = file:read_file("scripts/aos-handlers-utils.lua"),
    {ok, HandlersJson}    = file:read_file("scripts/aos-handlers.lua"),
    {ok, AoJson}          = file:read_file("scripts/aos-ao.lua"),
    {ok, EvalJson}        = file:read_file("scripts/aos-eval.lua"),
    {ok, BootJson}        = file:read_file("scripts/aos-boot.lua"),
    Process = generate_hyper_aos_modular_process([
        JsonJson,
        UtilsJson,
        StringifyJson,
        HandlersUtils,
        HandlersJson,
        AoJson,
        EvalJson,
        BootJson,
        Code
    ], Wallet),
    Message = generate_test_message(Process, Opts, <<"">>),
    hb_cache:write(Process, Opts),
    hb_ao:resolve(Process, Message, Opts#{ hashpath => ignore }),
    {ok, Process, Opts}.

%% -------------------------------------------------------------------
%% aos-boot: No On-Boot tag
%% -------------------------------------------------------------------
hyper_aos_boot_no_onboot_test() ->
    Code = <<
"""
package.loaded['.eval'] = function(_)
  return function(arg)
    eval_calls = (eval_calls or 0) + 1
    last_arg = arg
  end
end

Inbox = {}
eval_calls = 0
last_arg = nil

local boot = require('.boot')
local aos  = require('.ao')
local handler = boot(aos)

function compute(base, _req)
  local msg = { Tags = {} }
  handler(msg)
  base.results = { tostring(#Inbox), tostring(eval_calls) }
  return base
end
""" >>,
    {ok, Process, Opts} = generate_hyper_aos_modular_boot_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"1">>, <<"0">>], Result).

%% -------------------------------------------------------------------
%% aos-boot: On-Boot == 'Data'
%% -------------------------------------------------------------------
hyper_aos_boot_onboot_data_test() ->
    Code = <<
"""
package.loaded['.eval'] = function(_)
  return function(arg)
    eval_calls = (eval_calls or 0) + 1
    last_arg = arg
  end
end

Inbox = {}
eval_calls = 0
last_arg = nil

local boot = require('.boot')
local aos  = require('.ao')
local handler = boot(aos)

function compute(base, _req)
  local msg = {
    Tags = { ['On-Boot'] = 'Data' },
    Data = 'return 2+2'
  }
  handler(msg)
  base.results = { tostring(eval_calls), tostring(last_arg.Data) }
  return base
end
""" >>,
    {ok, Process, Opts} = generate_hyper_aos_modular_boot_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"1">>, <<"return 2+2">>], Result).

%% -------------------------------------------------------------------
%% aos-boot: On-Boot == txid path
%% -------------------------------------------------------------------
hyper_aos_boot_onboot_txid_test() ->
    Code = <<
"""
package.loaded['.eval'] = function(_)
  return function(arg)
    eval_calls = (eval_calls or 0) + 1
    last_arg = arg
  end
end

-- stub io.open to return our fake file content
ios_open_orig = io.open
io.open = function(_path)
  return {
    seek = function(_, _) return 0 end,
    read = function(_, _) return 'return 6*7' end,
    close = function() end
  }
end

Inbox = {}
eval_calls = 0
last_arg = nil

local boot = require('.boot')
local aos  = require('.ao')
local handler = boot(aos)

function compute(base, _req)
  local msg = {
    Tags = { ['On-Boot'] = 'ABC' }
  }
  handler(msg)
  base.results = { tostring(eval_calls), tostring(last_arg.Data) }
  return base
end
""" >>,
    {ok, Process, Opts} = generate_hyper_aos_modular_boot_process(Code),
    {ok, Result} = hb_ao:resolve(Process, <<"now/results">>, Opts),
    ?assertEqual([<<"1">>, <<"return 6*7">>], Result).