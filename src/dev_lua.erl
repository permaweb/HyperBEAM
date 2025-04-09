%%% @doc A device that calls a Lua script upon a request and returns the result.
-module(dev_lua).
-export([info/1, init/3, compute/3, snapshot/3, normalize/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
%%% The default Lua function to call, if not provided by the invoker.
-define(DEFAULT_LUA_FUNCTION, <<"handle">>).

%% @doc All keys that are not directly available in the base message are 
%% resolved by calling the Lua function in the script of the same name.
info(Base) ->
    #{
        default => fun handler/4,
        excludes => [<<"keys">>, <<"set">>] ++ maps:keys(Base)
    }.

%% @doc The handler of all non-message and non-device keys. We call the Lua
%% function in the script of the same name, passing the request as the 
%% parameter.
handler(Key, Base, Req, Opts) ->
    LuaReq =
        Req#{
            <<"function">> => Key,
            <<"parameters">> => [Base, Req, hb_private:reset(Opts)]
        },
    case compute(Base, LuaReq, Opts#{ hashpath => ignore }) of
        {ok, NewBase} ->
            ?event({handled, {key, Key}, {result, NewBase}}),
            Res =
                hb_ao:get(
                    <<"results/output">>,
                    {as, <<"message@1.0">>, NewBase},
                    Opts#{ hashpath => ignore }
                ),
            ?event({set_result, {key, Key}, {result, Res}}),
            {ok, Res};
        {error, Error} ->
            {error, Error}
    end.

%% @doc Initialize the device state, loading the script into memory if it is 
%% a reference.
init(Base, Req, Opts) ->
    ensure_initialized(Base, Req, Opts).

%% @doc Find the script in the base message, either by ID or by string.
find_script(Base, Opts) ->
    case hb_ao:get(<<"script-id">>, {as, <<"message@1.0">>, Base}, Opts) of
        not_found ->
            case hb_ao:get(<<"script">>, {as, <<"message@1.0">>, Base}, Opts) of
                not_found ->
                    {error, <<"missing-script-id-or-script">>};
                Script ->
                    {ok, Script}
            end;
        ScriptID ->
            case hb_cache:read(ScriptID, Opts) of
                {ok, Script} ->
                    {ok, Script};
                {error, Error} ->
                    {error, #{
                        <<"status">> => 404,
                        <<"body">> =>
                            <<"Lua script '", ScriptID/binary, "' not available.">>,
                        <<"cache-error">> => Error
                    }}
            end
    end.

%% @doc Initialize the Lua VM if it is not already initialized. Optionally takes
%% the script as a  Binary string. If not provided, the script will be loaded
%% from the base message.
ensure_initialized(Base, Req, Opts) ->
    case find_script(Base, Opts) of
        {ok, Script} ->
            ensure_initialized(Base, Req, Script, Opts);
        Error ->
            Error
    end.
ensure_initialized(Base, Req, Script, Opts) ->
    case hb_private:get(<<"state">>, Base, Opts) of
        not_found ->
            State0 = luerl:init(),
            {ok, _, State1} = luerl:do_dec(Script, State0),
            % Return the base message with the state added to it.
            {ok, hb_private:set(Base, <<"state">>, State1, Opts)};
        _ ->
            % The VM is already initialized, so return the base message.
            {ok, Base}
    end.

%% @doc Call the Lua script with the given arguments. This key is `multipass@1.0'
%% compatible: It will only execute the call on the first pass. On subsequent
%% passes, it will return the base message unchanged.
compute(Base, Req, RawOpts) ->
    Opts = RawOpts#{ hashpath => ignore },
    case hb_ao:get(<<"pass">>, {as, <<"message@1.0">>, Base}, 1, Opts) of
        1 -> do_compute(Base, Req, Opts);
        _ -> {ok, Base}
    end.
do_compute(RawBase, Req, Opts) ->
    {ok, Base} = ensure_initialized(RawBase, Req, Opts),
    % Get the state from the base message's private element.
    State = hb_private:get(<<"state">>, Base, Opts),
    % Get the Lua function to call from the base message.
    Function =
        hb_ao:get_first(
            [
                {Req, <<"body/function">>},
                {Req, <<"function">>},
                {{as, <<"message@1.0">>, Base}, <<"function">>}
            ],
            ?DEFAULT_LUA_FUNCTION,
            Opts#{ hashpath => ignore }
        ),
    Params =
        hb_ao:get_first(
            [
                {Req, <<"body/parameters">>},
                {Req, <<"parameters">>},
                {{as, <<"message@1.0">>, Base}, <<"parameters">>}
            ],
            [],
            Opts#{ hashpath => ignore }
        ),
    % Call the VM function with the given arguments.
    ?event({calling_lua_function, {function, Function}, {args, Params}, {req, Req}}),
    case luerl:call_function_dec([Function], encode(Params), State) of
        {ok, [LuaResult], NewState} ->
            ?event({lua_result, {result_before_decoding, {explicit, LuaResult}}}),
            Result = decode(LuaResult),
            BaseWithResults =
                Base#{
                    <<"results">> => #{
                        <<"type">> => <<"ok">>,
                        <<"output">> => Result
                    }
                },
            {ok, hb_private:set(BaseWithResults, <<"state">>, NewState, Opts)};
        {lua_error, Error} ->
            {error, #{
                <<"status">> => 500,
                <<"body">> =>
                    <<"Lua error: ", Error/binary>>
            }}
    end.

%% @doc Snapshot the Lua state from a live computation. Normalizes its `priv'
%% state element, then serializes the state to a binary.
snapshot(Base, _Req, Opts) ->
    case hb_private:get(<<"state">>, Base, Opts) of
        not_found ->
            {error, <<"Cannot snapshot Lua state: state not initialized.">>};
        State ->
            {ok,
                #{
                    <<"body">> =>
                        term_to_binary(luerl:externalize(State))
                }
            }
    end.

%% @doc Restore the Lua state from a snapshot, if it exists.
normalize(Base, _Req, RawOpts) ->
    Opts = RawOpts#{ hashpath => ignore },
    case hb_private:get(<<"state">>, Base, Opts) of
        not_found ->
            DeviceKey =
                case hb_ao:get(<<"device-key">>, {as, <<"message@1.0">>, Base}, Opts) of
                    not_found -> [];
                    Key -> [Key]
                end,
            ?event(
                {attempting_to_restore_lua_state,
                    {msg1, Base}, {device_key, DeviceKey}
                }
            ),
            SerializedState = 
                hb_ao:get(
                    [<<"snapshot">>] ++ DeviceKey ++ [<<"body">>],
                    {as, dev_message, Base},
                    Opts
                ),
            case SerializedState of
                not_found -> throw({error, no_lua_state_snapshot_found});
                State ->
                    ExternalizedState = binary_to_term(State),
                    InternalizedState = luerl:internalize(ExternalizedState),
                    {ok, hb_private:set(Base, <<"state">>, InternalizedState, Opts)}
            end;
        _ ->
            ?event(state_already_initialized),
            {ok, Base}
    end.

%% @doc Decode a Lua result into a HyperBEAM `structured@1.0' message.
decode(Map = [{_K, _V} | _]) when is_list(Map) ->
    maps:map(fun(_, V) -> decode(V) end, maps:from_list(Map));
decode(Other) ->
    Other.

%% @doc Encode a HyperBEAM `structured@1.0' message into a Lua result.
encode(Map) when is_map(Map) ->
    maps:to_list(maps:map(fun(_, V) -> encode(V) end, Map));
encode(List) when is_list(List) ->
    lists:map(fun encode/1, List);
encode(Other) ->
    Other.

%%% Tests
simple_invocation_test() ->
    {ok, Script} = file:read_file("test/test.lua"),
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"script">> => Script,
        <<"function">> => <<"AssocTable">>,
        <<"parameters">> => []
    },
    ?assertEqual(2, hb_ao:get(<<"compute/results/output/b">>, Base, #{})).

%% @doc Benchmark the performance of Lua executions.
direct_benchmark_test() ->
    BenchTime = 3,
    {ok, Script} = file:read_file("test/test.lua"),
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"script">> => Script,
        <<"function">> => <<"AssocTable">>,
        <<"parameters">> => []
    },
    {ok, Initialized} = hb_ao:resolve(Base, <<"init">>, #{}),
    Iterations = hb:benchmark(
        fun(X) ->
            {ok, _} = hb_ao:resolve(Initialized, <<"compute">>, #{}),
            ?event({iteration, X})
        end,
        BenchTime
    ),
    ?event({iterations, Iterations}),
    hb_util:eunit_print(
        "Computed ~p Lua executions in ~ps (~.2f calls/s)",
        [Iterations, BenchTime, Iterations / BenchTime]
    ),
    ?assert(Iterations > 10).

%% @doc Call AOS with an eval command.
invoke_aos_test_disabled() ->
    % Disabled: aos-2.0.4.lua is an update script, but does not initialize
    % the Lua environment state correctly.
    {ok, Script} = file:read_file("test/aos-2.0.4.lua"),
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"script">> => Script,
        <<"function">> => <<"handle">>,
        <<"parameters">> => [
            aos_process_binary(),
            aos_exec_binary(<<"return 42">>)
        ]
    },
    {ok, Initialized} = hb_ao:resolve(Base, <<"init">>, #{}),
    {ok, Results} = hb_ao:resolve(Initialized, <<"compute">>, #{}),
    ?event({results, Results}),
    ?assertEqual(42, hb_ao:get(<<"data">>, Results, #{})).

%% @doc Call a non-compute key on a Lua device message and ensure that the
%% function of the same name in the script is called.
invoke_non_compute_key_test() ->
    {ok, Script} = file:read_file("test/test.lua"),
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"script">> => Script,
        <<"test-value">> => 42
    },
    {ok, Result1} = hb_ao:resolve(Base, <<"hello">>, #{}),
    ?event({result1, Result1}),
    ?assertEqual(42, hb_ao:get(<<"test-value">>, Result1, #{})),
    ?assertEqual(<<"world">>, hb_ao:get(<<"hello">>, Result1, #{})),
    {ok, Result2} =
        hb_ao:resolve(
            Base,
            #{<<"path">> => <<"hello">>, <<"name">> => <<"Alice">>},
            #{}
        ),
    ?event({result2, Result2}),
    ?assertEqual(<<"Alice">>, hb_ao:get(<<"hello">>, Result2, #{})).

%% @doc Use a Lua script as a preprocessor on the HTTP server via `~meta@1.0'.
lua_http_preprocessor_test() ->
    {ok, Script} = file:read_file("test/test.lua"),
    Node = hb_http_server:start_node(
        #{
            preprocessor =>
                #{
                    <<"device">> => <<"lua@5.3a">>,
                    <<"script">> => Script
                }
        }),
    {ok, Res} = hb_http:get(Node, <<"/hello?hello=world">>, #{}),
    ?assertMatch(#{ <<"body">> := <<"i like turtles">> }, Res).

%% @doc Ensure that we can call a Lua process using the JSON interface.
lua_json_interface_test() ->
    {ok, Computed} = execute_aos_call(generate_stack("test/test.lua")),
    {ok, Results} = hb_ao:resolve(Computed, <<"results">>, #{}),
    ?assertEqual(42, hb_ao:get(<<"data">>, Results, #{})).

%% @doc Benchmark execution of a Lua stack with a JSON interface.
lua_json_interface_benchmark_test() ->
    BenchTime = 3,
    Initialized = generate_stack("test/test.lua"),
    Iterations = hb:benchmark(
        fun(X) ->
            execute_aos_call(Initialized),
            ?event({iteration, X})
        end,
        BenchTime
    ),
    ?event({iterations, Iterations}),
    hb_util:eunit_print(
        "Computed ~p Lua+JSON stack executions in ~ps (~.2f calls/s)",
        [Iterations, BenchTime, Iterations / BenchTime]
    ),
    ?assert(Iterations > 10).

%% @doc Call a process whose `execution-device' is set to `lua@5.3a'.
pure_lua_process_test() ->
    Process = generate_lua_process("test/test.lua"),
    {ok, _} = hb_cache:write(Process, #{}),
    Message = generate_test_message(Process),
    {ok, _} = hb_ao:resolve(Process, Message, #{ hashpath => ignore }),
    {ok, Results} = hb_ao:resolve(Process, <<"now">>, #{}),
    ?event({results, Results}),
    ?assertEqual(42, hb_ao:get(<<"results/output/body">>, Results, #{})).

pure_lua_process_benchmark_test() ->
    {timeout, 30, fun() ->
        BenchMsgs = 200,
        Process = generate_lua_process("test/test.lua"),
        Message = generate_test_message(Process),
        lists:foreach(
            fun(X) ->
                hb_ao:resolve(Process, Message, #{ hashpath => ignore }),
                ?event(debug_lua, {scheduled, X})
            end,
            lists:seq(1, BenchMsgs)
        ),
        ?event(debug_lua, {executing, BenchMsgs}),
        BeforeExec = os:system_time(millisecond),
        {ok, _} = hb_ao:resolve(Process, <<"now">>, #{ hashpath => ignore }),
        AfterExec = os:system_time(millisecond),
        ?event(debug_lua, {execution_time, (AfterExec - BeforeExec) / BenchMsgs}),
        hb_util:eunit_print(
            "Computed ~p pure Lua process executions in ~ps (~.2f calls/s)",
            [
                BenchMsgs,
                (AfterExec - BeforeExec) / 1000,
                BenchMsgs / ((AfterExec - BeforeExec) / 1000)
            ]
        )
    end}.
    % Iterations = hb:benchmark(
    %     fun(X) ->
    %         % Compute the latest result.
    %         ?event({iteration, X})
    %     end,
    %     BenchTime
    % ),
    % ?event({iterations, Iterations}),
    % hb_util:eunit_print(
    %     "Computed ~p pure Lua process executions in ~ps (~.2f calls/s)",
    %     [Iterations, BenchTime, Iterations / BenchTime]
    % ),
    % ?assert(Iterations > 10).

%%% Test helpers

%% @doc Generate a Lua process message.
generate_lua_process(File) ->
    Wallet = hb:wallet(),
    {ok, Script} = file:read_file(File),
    hb_message:commit(#{
        <<"device">> => <<"process@1.0">>,
        <<"type">> => <<"Process">>,
        <<"scheduler-device">> => <<"scheduler@1.0">>,
        <<"execution-device">> => <<"lua@5.3a">>,
        <<"script">> => Script,
        <<"scheduler-location">> =>
            hb_util:human_id(ar_wallet:to_address(Wallet)),
        <<"test-random-seed">> => rand:uniform(1337)
    }, Wallet).

%% @doc Generate a test message for a Lua process.
generate_test_message(Process) ->
    ProcID = hb_message:id(Process, all),
    Wallet = hb:wallet(),
    hb_message:commit(#{
            <<"path">> => <<"schedule">>,
            <<"method">> => <<"POST">>,
            <<"body">> =>
                hb_message:commit(
                    #{
                        <<"target">> => ProcID,
                        <<"type">> => <<"Message">>,
                        <<"body">> => <<"TEST MESSAGE">>,
                        <<"random-seed">> => rand:uniform(1337)
                    },
                    Wallet
                )
        },
        Wallet
    ).

%% @doc Generate a stack message for the Lua process.
generate_stack(File) ->
    Wallet = hb:wallet(),
    {ok, Script} = file:read_file(File),
    Msg1 = #{
        <<"device">> => <<"Stack@1.0">>,
        <<"device-stack">> =>
            [
                <<"json-iface@1.0">>,
                <<"lua@5.3a">>,
                <<"multipass@1.0">>
            ],
        <<"passes">> => 2,
        <<"stack-keys">> => [<<"init">>, <<"compute">>],
        <<"script">> => Script,
        <<"process">> => 
            hb_message:commit(#{
                <<"type">> => <<"Process">>,
                <<"script">> => Script,
                <<"scheduler">> => hb:address(),
                <<"authority">> => hb:address()
            }, Wallet)
    },
    {ok, Msg2} = hb_ao:resolve(Msg1, <<"init">>, #{}),
    Msg2.

execute_aos_call(Base) ->
    Req =
        hb_message:commit(#{
                <<"action">> => <<"Eval">>,
                <<"data">> => <<"return 2">>
            },
            hb:wallet()
        ),
    execute_aos_call(Base, Req).
execute_aos_call(Base, Req) ->
    hb_ao:resolve(Base,
        #{
            <<"path">> => <<"compute">>,
            <<"body">> => Req
        },
        #{}
    ).

aos_process_binary() ->
    <<
        "{\"Process\":{\"Id\":\"AOS\",\"Owner\":\"FOOBAR\",\"Tags\":",
        "[{\"name\":\"Name\",\"value\":\"Thomas\"}, ",
        "{\"name\":\"Authority\",\"value\":\"FOOBAR\"}]}}"
    >>.

aos_exec_binary(Command) ->
    <<
        "{\"From\":\"FOOBAR\",\"Block-Height\":\"1\",\"Target\":\"AOS\",",
        "\"Owner\":\"FOOBAR\",\"Id\":\"1\",\"Module\":\"W\",\"Tags\":[",
        "{\"name\":\"Action\",\"value\":\"Eval\"}],\"Data\":\"",
        Command/binary,
        "\"}"
    >>.