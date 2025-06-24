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

% @doc Test the hyper-aos ao module.
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
    ?assertEqual(AosId, ProcessId),
    ok.

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

hyper_aos_json_test() ->
    Wallet = hb:wallet(),
    Opts = #{ priv_wallet => Wallet },
    {ok, Json} = file:read_file("scripts/aos-json.lua"),
    Code = <<
"""
local json = require('json')

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

%% @doc Test the hyper-aos handlers utils module.
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

%%% Test helpers
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