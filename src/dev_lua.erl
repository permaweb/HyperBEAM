%%% @doc A device that calls a Lua module upon a request and returns the result.
-module(dev_lua).
-export([info/1, init/3, snapshot/3, normalize/3, functions/3]).
%%% Public Utilities
-export([encode/2, decode/2]).
-export([pure_lua_process_benchmark/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The set of functions that will be sandboxed by default if `sandbox' is set 
%%% to only `true'. Setting `sandbox' to a map allows the invoker to specify
%%% which functions should be sandboxed and what to return instead. Providing
%%% a list instead of a map will result in all functions being sandboxed and
%%% returning `sandboxed'.
-define(DEFAULT_SANDBOX, [
    {['_G', io], <<"sandboxed">>},
    {['_G', file], <<"sandboxed">>},
    {['_G', os, execute], <<"sandboxed">>},
    {['_G', os, exit], <<"sandboxed">>},
    {['_G', os, getenv], <<"sandboxed">>},
    {['_G', os, remove], <<"sandboxed">>},
    {['_G', os, rename], <<"sandboxed">>},
    {['_G', os, tmpname], <<"sandboxed">>},
    {['_G', package], <<"sandboxed">>},
    {['_G', loadfile], <<"sandboxed">>},
    {['_G', require], <<"sandboxed">>},
    {['_G', dofile], <<"sandboxed">>},
    {['_G', load], <<"sandboxed">>},
    {['_G', loadfile], <<"sandboxed">>},
    {['_G', loadstring], <<"sandboxed">>}
]).

%% @doc All keys that are not directly available in the base message are 
%% resolved by calling the Lua function in the module of the same name.
%% Additionally, we exclude the `keys', `set', `encode' and `decode' functions
%% which are `message@1.0' core functions, and Lua public utility functions.
info(Base) ->
    #{
        default => fun compute/4,
        excludes =>
            [<<"keys">>, <<"set">>, <<"encode">>, <<"decode">>]
                ++ maps:keys(Base)
    }.

%% @doc Initialize the device state, loading the script into memory if it is 
%% a reference.
init(Base, Req, Opts) ->
    ensure_initialized(Base, Req, Opts).

%% @doc Initialize the Lua VM if it is not already initialized. Optionally takes
%% the script as a  Binary string. If not provided, the module will be loaded
%% from the base message.
ensure_initialized(Base, _Req, Opts) ->
    case hb_private:from_message(Base) of
        #{<<"state">> := _} -> 
            ?event(debug_lua, lua_state_already_initialized),
            {ok, Base};
        _ ->
            ?event(debug_lua, initializing_lua_state),
            case find_modules(Base, Opts) of
                {ok, Modules} ->
                    initialize(Base, Modules, Opts);
                Error ->
                    Error
            end
    end.

%% @doc Find the script in the base message, either by ID or by string.
find_modules(Base, Opts) ->
    case hb_ao:get(<<"module">>, {as, <<"message@1.0">>, Base}, Opts) of
        not_found ->
            {error, <<"no-modules-found">>};
        Module when is_binary(Module) ->
            find_modules(Base#{ <<"module">> => [Module] }, Opts);
        Module when is_map(Module) ->
            % If the module is a map, check its content type to see if it is 
            % a literal Lua module, or a map of modules with content types.
            case hb_ao:get(<<"content-type">>, Module, Opts) of
                CT when CT == <<"application/lua">> orelse CT == <<"text/x-lua">> ->
                    find_modules(Base#{ <<"module">> => [Module] }, Opts);
                _ ->
                    % If the script is not a literal Lua script, assume it is a
                    % map of scripts with content types, and recurse.
                    find_modules(Base#{ <<"module">> => maps:values(Module) }, Opts)
            end;
        Modules when is_list(Modules) ->
            % We have found a list of scripts, load them.
            load_modules(Modules, Opts)
    end.

%% @doc Load a list of modules for installation into the Lua VM.
load_modules(Modules, Opts) -> load_modules(Modules, Opts, []).
load_modules([], _Opts, Acc) ->
    {ok, lists:reverse(Acc)};
load_modules([ModuleID | Rest], Opts, Acc) when ?IS_ID(ModuleID) ->
    case hb_cache:read(ModuleID, Opts) of
        {ok, Module} when is_binary(Module) ->
            % The ID referred to a binary module item, so we add it to the list
            % as-is.
            load_modules(Rest, Opts, [{ModuleID, Module}|Acc]);
        {ok, ModuleMsg} when is_map(ModuleMsg) ->
            % We read a message from the store, so we recurse upon the output,
            % as if the module message had beeen given directly.
            load_modules([ModuleMsg|Rest], Opts, Acc);
        not_found ->
            {error, #{
                <<"status">> => 404,
                <<"body">> => <<"Lua module '", ModuleID/binary, "' not found.">>
            }}
    end;
load_modules([Module | Rest], Opts, Acc) when is_map(Module) ->
    % We have found a message with a Lua module inside. Search for the binary
    % of the program in the body and the data.
    ModuleBin =
        hb_ao:get_first(
            [
                {Module, <<"body">>},
                {Module, <<"data">>}
            ],
            Module,
            Opts
        ),
    case ModuleBin of
        not_found ->
            {error, #{
                <<"status">> => 404,
                <<"body">> =>
                    <<
                        """
                        Lua module not loadable. Lua modules must have a
                        `body' element set to a binary of the code to load.
                        """
                    >>,
                <<"module">> => Module
            }};
        ModuleBin ->
            % Get the `name' key from the script message if it exists, or 
            % return the module ID as the module name.
            Name =
                hb_ao:get_first(
                    [
                        {Module, <<"name">>},
                        {Module, <<"id">>}
                    ],
                    Module,
                    Opts
                ),
            % Load the module into the Lua state.
            load_modules(Rest, Opts, [{Name, ModuleBin}|Acc])
    end.

%% @doc Initialize a new Lua state with a given base message and module.
initialize(Base, Modules, Opts) ->
    State0 = luerl:init(),
    % Load each script into the Lua state.
    State1 =
        lists:foldl(
            fun({ModuleID, ModuleBin}, StateIn) ->
                {ok, _, StateOut} =
                    luerl:do_dec(
                        ModuleBin,
                        [
                            {name, hb_util:list(ModuleID)},
                            {file, hb_util:list(ModuleID)}
                        ],
                        StateIn
                    ),
                StateOut
            end,
            State0,
            Modules
        ),
    % Apply any sandboxing rules to the state.
    State2 =
        case hb_ao:get(<<"sandbox">>, {as, <<"message@1.0">>, Base}, false, Opts) of
            false -> State1;
            true -> sandbox(State1, ?DEFAULT_SANDBOX, Opts);
            Spec -> sandbox(State1, Spec, Opts)
        end,
    % Install the AO-Core Lua library into the state.
    {ok, State3} = dev_lua_lib:install(Base, State2, Opts),
    % Return the base message with the state added to it.
    {ok, hb_private:set(Base, <<"state">>, State3, Opts)}.

%%% @doc Return a list of all functions in the Lua environment.
functions(Base, _Req, Opts) ->
    case hb_private:get(<<"state">>, Base, Opts) of
        not_found ->
            {error, not_found};
        State ->
            {ok, [Res], _S2} =
                luerl:do_dec(
                    <<
                        """
                        local __tests = {}
                        for k, v in pairs(_G) do
                            if type(v) == "function" then
                                table.insert(__tests, k)
                            end
                        end
                        return __tests
                        """
                    >>,
                    State
                ),
            {ok, hb_util:message_to_ordered_list(decode(Res, Opts))}
    end.

%% @doc Sandbox (render inoperable) a set of Lua functions. Each function is
%% referred to as if it is a path in AO-Core, with its value being what to 
%% return to the caller. For example, 'os.exit' would be referred to as
%% referred to as `os/exit'. If preferred, a list rather than a map may be
%% provided, in which case the functions all return `sandboxed'.
sandbox(State, Map, Opts) when is_map(Map) ->
    sandbox(State, maps:to_list(Map), Opts);
sandbox(State, [], _Opts) ->
    State;
sandbox(State, [{Path, Value} | Rest], Opts) ->
    {ok, NextState} = luerl:set_table_keys_dec(Path, Value, State),
    sandbox(NextState, Rest, Opts);
sandbox(State, [Path | Rest], Opts) ->
    {ok, NextState} = luerl:set_table_keys_dec(Path, <<"sandboxed">>, State),
    sandbox(NextState, Rest, Opts).

%% @doc Call the Lua script with the given arguments.
compute(Key, RawBase, Req, Opts) ->
    ?event(debug_lua, compute_called),
    {ok, Base} = ensure_initialized(RawBase, Req, Opts),
    ?event(debug_lua, ensure_initialized_done),
    % Get the state from the base message's private element.
    OldPriv = #{ <<"state">> := State } = hb_private:from_message(Base),
    % TODO: looks like the script is injected in multiple places, does the 
    % script need to be passed?
    % Get the Lua function to call from the base message.
    Function =
        hb_ao:get_first(
            [
                {Req, <<"body/function">>},
                {Req, <<"function">>},
                {{as, <<"message@1.0">>, Base}, <<"function">>}
            ],
            Key,
            Opts#{ hashpath => ignore }
        ),
    ?event(debug_lua, function_found),
    Params =
        hb_ao:get_first(
            [
                {Req, <<"body/parameters">>},
                {Req, <<"parameters">>},
                {{as, <<"message@1.0">>, Base}, <<"parameters">>}
            ],
            [
                hb_private:reset(Base),
                Req,
                #{}
            ],
            Opts#{ hashpath => ignore }
        ),
    ?event(debug_lua, parameters_found),
    % Resolve all hyperstate links
    ResolvedParams = hb_cache:ensure_all_loaded(Params, Opts),
    % Call the VM function with the given arguments.
    ?event(lua,
        {calling_lua_func,
            {function, Function},
            {args, ResolvedParams},
            {req, Req}
        }
    ),
    process_response(
        try luerl:call_function_dec(
            [Function],
            encode(ResolvedParams, Opts),
            State
        )
        catch
            _:Reason:Stacktrace -> {error, Reason, Stacktrace}
        end,
        OldPriv,
		Opts
    ).

%% @doc Process a response to a Luerl invocation. Returns the typical AO-Core
%% HyperBEAM response format.
process_response({ok, [Result], NewState}, Priv, Opts) ->
    process_response({ok, [<<"ok">>, Result], NewState}, Priv, Opts);
process_response({ok, [Status, MsgResult], NewState}, Priv, Opts) ->
    % If the result is a HyperBEAM device return (`{Status, Msg}'), decode it 
    % and add the previous `priv' element back into the resulting message.
    case decode(MsgResult, Opts) of
        Msg when is_map(Msg) ->
            ?event(lua, {response, {status, Status}, {msg, Msg}}),
            {hb_util:atom(Status), Msg#{
                <<"priv">> => Priv#{
                    <<"state">> => NewState
                }
            }};
        NonMsgRes -> {hb_util:atom(Status), NonMsgRes}
    end;
process_response({lua_error, RawError, State}, _Priv, Opts) ->
    % An error occurred while calling the Lua function. Parse the stack trace
    % and return it.
    Error = try decode(luerl:decode(RawError, State), Opts) catch _:_ -> RawError end,
    StackTrace = decode_stacktrace(luerl:get_stacktrace(State), State, Opts),
    ?event(lua_error, {lua_error, Error, {stacktrace, StackTrace}}),
    {error, #{
        <<"status">> => 500,
        <<"body">> => Error,
        <<"trace">> => hb_ao:normalize_keys(StackTrace, Opts)
    }};
process_response({error, Reason, Trace}, _Priv, _Opts) ->
    % An Erlang error occurred while calling the Lua function. Return it.
    ?event(lua_error, {trace, Trace}),
    TraceBin = iolist_to_binary(hb_format:trace(Trace)),
    ?event(lua_error, {formatted, {string, TraceBin}}),
    ReasonBin = iolist_to_binary(io_lib:format("~p", [Reason])),
    {error, #{
        <<"status">> => 500,
        <<"body">> =>
            << "Erlang error while running Lua: ", ReasonBin/binary >>,
        <<"trace">> => TraceBin
    }}.

%% @doc Snapshot the Lua state from a live computation. Normalizes its `priv'
%% state element, then serializes the state to a binary.
snapshot(Base, _Req, Opts) ->
    case hb_private:get(<<"state">>, Base, Opts) of
        not_found ->
            {error, <<"Cannot snapshot Lua state: state not initialized.">>};
        State ->
            {ok, #{ <<"body">> => term_to_binary(luerl:externalize(State)) }}
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
            ?event(snapshot,
                {attempting_to_restore_lua_state,
                    {base, Base}, {device_key, DeviceKey}
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
                    ?event(snapshot, loaded_state_from_snapshot),
                    {ok, hb_private:set(Base, <<"state">>, InternalizedState, Opts)}
            end;
        _ ->
            ?event(snapshot, state_already_initialized),
            {ok, Base}
    end.

%% @doc Decode a Lua result into a HyperBEAM `structured@1.0' message.
decode(EncMsg, Opts) ->
    hb_message:normalize_commitments(do_decode(EncMsg, Opts), Opts, verify).
do_decode(EncMsg, _Opts) when is_list(EncMsg) andalso length(EncMsg) == 0 ->
    % The value is an empty table, so we assume it is a message rather than
    % a list.
    #{};
do_decode(EncMsg = [{_K, _V} | _], Opts) when is_list(EncMsg) ->
    do_decode(
        maps:map(
            fun(_, V) -> do_decode(V, Opts) end,
            maps:from_list(EncMsg)
        ),
        Opts
    );
do_decode(Msg, Opts) when is_map(Msg) ->
    % If the message is an ordered list encoded as a map, decode it to a list.
    case hb_util:is_ordered_list(Msg, Opts) of
        true ->
            lists:map(
                fun(V) -> do_decode(V, Opts) end,
                hb_util:message_to_ordered_list(Msg)
            );
        false ->
            Msg
    end;
do_decode(Other, _Opts) ->
    Other.

%% @doc Encode a HyperBEAM `structured@1.0' message into a Lua term.
encode(Map, Opts) ->
    hb_message:normalize_commitments(do_encode(Map, Opts), Opts).
do_encode(Map, Opts) when is_map(Map) ->
    hb_cache:ensure_all_loaded(
        case hb_util:is_ordered_list(Map, Opts) of
            true -> do_encode(hb_util:message_to_ordered_list(Map), Opts);
            false -> maps:to_list(maps:map(fun(_, V) -> do_encode(V, Opts) end, Map))
        end,
        Opts
    );
do_encode(List, Opts) when is_list(List) ->
    hb_cache:ensure_all_loaded(
        lists:map(fun(V) -> do_encode(V, Opts) end, List),
        Opts
    );
do_encode(Atom, _Opts) when is_atom(Atom) and (Atom /= false) and (Atom /= true)->
    hb_util:bin(Atom);
do_encode(Other, _Opts) ->
    Other.

%% @doc Parse a Lua stack trace into a list of messages.
decode_stacktrace(StackTrace, State0, Opts) ->
    decode_stacktrace(StackTrace, State0, [], Opts).
decode_stacktrace([], _State, Acc, _Opts) ->
    lists:reverse(Acc);
decode_stacktrace([{FuncBin, ParamRefs, FileInfo} | Rest], State0, Acc, Opts) ->
    %% Decode all the Lua table refs into Erlang terms
    DecodedParams = decode_params(ParamRefs, State0, Opts),
    %% Pull out the line number
    Line = proplists:get_value(line, FileInfo),
    File = proplists:get_value(file, FileInfo, undefined),
    ?event(debug_lua_stack, {stack_file, FileInfo}),
    %% Build our message‐map
    Entry = #{
        <<"function">>   => FuncBin,
        <<"parameters">> => hb_util:list_to_numbered_message(DecodedParams)
    },
    MaybeLine =
        if is_binary(File) andalso is_integer(Line) ->
            #{
                <<"line">> =>
                    iolist_to_binary(
                        io_lib:format("~s:~p", [File, Line])
                    )
            };
        is_integer(Line) ->
            #{ <<"line">> => Line };
        true ->
            #{}
        end,
    decode_stacktrace(Rest, State0, [maps:merge(Entry, MaybeLine)|Acc], Opts).

%% @doc Decode a list of Lua references, as found in a stack trace, into a
%% list of Erlang terms.
decode_params([], _State, _Opts) -> [];
decode_params([Tref|Rest], State, Opts) ->
    Decoded = decode(luerl:decode(Tref, State), Opts),
    [Decoded|decode_params(Rest, State, Opts)].

%%% Tests
simple_invocation_test() ->
    {ok, Script} = file:read_file("test/test.lua"),
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"module">> => #{
            <<"content-type">> => <<"application/lua">>,
            <<"body">> => Script
        },
        <<"parameters">> => []
    },
    ?assertEqual(2, hb_ao:get(<<"assoctable/b">>, Base, #{})).

post_invocation_message_validation_test() ->
    {ok, Script} = file:read_file("test/test.lua"),
    Opts = #{ priv_wallet => hb:wallet() },
    Base =
        hb_message:commit(
            #{
                <<"device">> => <<"lua@5.3a">>,
                <<"module">> => #{
                    <<"content-type">> => <<"application/lua">>,
                    <<"body">> => Script
                },
                <<"test-key">> => <<"test-value-1">>
            },
            Opts
        ),
    {ok, UnsignedID} = hb_cache:write(Base, Opts),
    ?event({base, {msg, Base}, {unsigned_id, UnsignedID}}),
    {ok, Res} = hb_ao:resolve(Base, <<"mutate_test_key">>, Opts),
    {ok, ResID} = hb_cache:write(Res, Opts),
    ?event({res_id, ResID}),
    {ok, ReadMsg} = hb_cache:read(UnsignedID, Opts),
    ?assertEqual(<<"test-value-1">>, hb_ao:get(<<"test-key">>, ReadMsg, Opts)),
    ?assert(length(hb_message:signers(Res, Opts)) == 0),
    ?assert(hb_message:verify(Res, all, Opts)).

load_modules_by_id_test_() ->
    {timeout, 30, fun load_modules_by_id/0}.
load_modules_by_id() ->
    % Start a node to ensure the HTTP services are available.
    _Node = hb_http_server:start_node(#{}),
    Module = <<"DosEHUAqhl_O5FH3vDqPlgGsG92Guxcm6nrwqnjsDKg">>,
    {ok, Acc} = load_modules([Module], #{}),
    [{_,Code}|_] = Acc,
    <<Prefix:8/binary, _/binary>> = Code,
    ?assertEqual(<<"function">>, Prefix).
    
multiple_modules_test() ->
    {ok, Module} = file:read_file("test/test.lua"),
    Module2 =
        <<
            """
            function test_second_script()
                return 4
            end
            """
        >>,
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"module">> => [
            #{
                <<"content-type">> => <<"application/lua">>,
                <<"body">> => Module
            },
            #{
                <<"content-type">> => <<"application/lua">>,
                <<"body">> => Module2
            }
        ],
        <<"parameters">> => []
    },
    ?assertEqual(2, hb_ao:get(<<"assoctable/b">>, Base, #{})),
    ?assertEqual(4, hb_ao:get(<<"test_second_script">>, Base, #{})).

error_response_test() ->
    {ok, Module} = file:read_file("test/test.lua"),
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"module">> => #{
            <<"content-type">> => <<"application/lua">>,
            <<"body">> => Module
        },
        <<"parameters">> => []
    },
    ?assertEqual(
        {error, <<"Very bad, but Lua caught it.">>},
        hb_ao:resolve(Base, <<"error_response">>, #{})
    ).

sandboxed_failure_test() ->
    {ok, Module} = file:read_file("test/test.lua"),
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"module">> => #{
            <<"content-type">> => <<"application/lua">>,
            <<"body">> => Module
        },
        <<"parameters">> => [],
        <<"sandbox">> => true
    },
    ?assertMatch({error, _}, hb_ao:resolve(Base, <<"sandboxed_fail">>, #{})).

%% @doc Run an AO-Core resolution from the Lua environment.
ao_core_sandbox_test() ->
    {ok, Module} = file:read_file("test/test.lua"),
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"module">> => #{
            <<"content-type">> => <<"application/lua">>,
            <<"body">> => Module
        },
        <<"parameters">> => [],
        <<"device-sandbox">> => [<<"message@1.0">>]
    },
    ?assertMatch({error, _}, hb_ao:resolve(Base, <<"ao_relay">>, #{})),
    ?assertMatch({ok, _}, hb_ao:resolve(Base, <<"ao_resolve">>, #{})).

%% @doc Run an AO-Core resolution from the Lua environment.
ao_core_resolution_from_lua_test() ->
    {ok, Module} = file:read_file("test/test.lua"),
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"module">> => #{
            <<"content-type">> => <<"application/lua">>,
            <<"body">> => Module
        },
        <<"parameters">> => []
    },
    {ok, Res} = hb_ao:resolve(Base, <<"ao_resolve">>, #{}),
    ?assertEqual(<<"Hello, AO world!">>, Res).

%% @doc Benchmark the performance of Lua executions.
direct_benchmark_test() ->
    BenchTime = 3,
    {ok, Module} = file:read_file("test/test.lua"),
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"module">> => #{
            <<"content-type">> => <<"application/lua">>,
            <<"body">> => Module
        },
        <<"parameters">> => []
    },
    Iterations = hb_test_utils:benchmark(
        fun(X) ->
            {ok, _} = hb_ao:resolve(Base, <<"assoctable">>, #{}),
            ?event({iteration, X})
        end,
        BenchTime
    ),
    ?event({iterations, Iterations}),
    hb_test_utils:benchmark_print(
        <<"Direct Lua:">>,
        <<"executions">>,
        Iterations,
        BenchTime
    ),
    ?assert(Iterations > 10).

%% @doc Call a non-compute key on a Lua device message and ensure that the
%% function of the same name in the script is called.
invoke_non_compute_key_test() ->
    {ok, Module} = file:read_file("test/test.lua"),
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"module">> => #{
            <<"content-type">> => <<"application/lua">>,
            <<"body">> => Module
        },
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

%% @doc Use a Lua module as a hook on the HTTP server via `~meta@1.0'.
lua_http_hook_test() ->
    {ok, Module} = file:read_file("test/test.lua"),
    Node = hb_http_server:start_node(
        #{
            priv_wallet => ar_wallet:new(),
            on => #{
                <<"request">> =>
                    #{
                        <<"device">> => <<"lua@5.3a">>,
                        <<"module">> => #{
                            <<"content-type">> => <<"application/lua">>,
                            <<"body">> => Module
                        }
                    }
            }
        }),
    {ok, Res} = hb_http:get(Node, <<"/hello?hello=world">>, #{}),
    ?assertMatch(#{ <<"body">> := <<"i like turtles">> }, Res).

%% @doc Call a process whose `execution-device' is set to `lua@5.3a'.
pure_lua_process_test() ->
    Process = generate_lua_process("test/test.lua", #{}),
    {ok, _} = hb_cache:write(Process, #{}),
    Message = generate_test_message(Process, #{}),
    {ok, _} = hb_ao:resolve(Process, Message, #{ hashpath => ignore }),
    {ok, Results} = hb_ao:resolve(Process, <<"now">>, #{}),
    ?assertEqual(42, hb_ao:get(<<"results/output/body">>, Results, #{})).

%% @doc Call a process whose `execution-device' is set to `lua@5.3a'.
pure_lua_restore_test() ->
    Opts = #{ process_cache_frequency => 1 },
    Process = generate_lua_process("test/test.lua", Opts),
    {ok, _} = hb_cache:write(Process, Opts),
    Message = generate_test_message(Process, Opts, #{ <<"path">> => <<"inc">>}),
    {ok, _} = hb_ao:resolve(Process, Message, Opts#{ hashpath => ignore }),
    {ok, Count1} = hb_ao:resolve(Process, <<"now/count">>, Opts),
    ?assertEqual(1, Count1),
    hb_ao:resolve(
        Process,
        generate_test_message(Process, #{}, #{ <<"path">> => <<"inc">>}),
        Opts
    ),
    {ok, Count2} = hb_ao:resolve(Process, <<"now/count">>, Opts),
    ?assertEqual(2, Count2).

pure_lua_process_benchmark_test_() ->
    {timeout,
        30,
        fun() ->
            pure_lua_process_benchmark(#{
                process_snapshot_slots => 50
            })
    end}.
pure_lua_process_benchmark(Opts) ->
    BenchMsgs = 50,
    hb:init(),
    Process = generate_lua_process("test/test.lua", Opts),
    {ok, _} = hb_cache:write(Process, Opts),
    Message = generate_test_message(Process, Opts),
    lists:foreach(
        fun(X) ->
            hb_ao:resolve(Process, Message, Opts#{ hashpath => ignore }),
            ?event(debug_lua, {scheduled, X})
        end,
        lists:seq(1, BenchMsgs)
    ),
    ?event(debug_lua, {executing, BenchMsgs}),
    BeforeExec = os:system_time(millisecond),
    {ok, _} = hb_ao:resolve(Process, <<"now">>, Opts),
    AfterExec = os:system_time(millisecond),
    hb_test_utils:benchmark_print(
        <<"Pure Lua process: Computed">>,
        <<"slots">>,
        BenchMsgs,
        (AfterExec - BeforeExec) / 1000
    ).

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
            process_async_cache => true,
            hashpath => ignore,
            process_snapshot_slots => 50
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
        hb_test_utils:benchmark_print(
            <<"HyperAOS process: Computed">>,
            <<"slots">>,
            BenchMsgs,
            (AfterExec - BeforeExec) / 1000
        )
    end}.

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

%% @doc Generate a test message for a Lua process.
generate_test_message(Process, Opts) ->
    generate_test_message(
        Process,
        Opts,
        <<""" 
        Count = 0
        function add() 
            Send({Target = 'Foo', Data = 'Bar' });
            Count = Count + 1 
        end
        add()
        return Count
        """>>
    ).
generate_test_message(Process, Opts, ToEval) when is_binary(ToEval) ->
    generate_test_message(
        Process,
        Opts,
        #{
            <<"action">> => <<"Eval">>,
            <<"body">> => #{
                <<"content-type">> => <<"application/lua">>,
                <<"body">> => hb_util:bin(ToEval) 
            }
        }
    );
generate_test_message(Process, Opts, MsgBase) ->
    ProcID = hb_message:id(Process, all),
    NormOpts = Opts#{ priv_wallet => hb_opts:get(priv_wallet, hb:wallet(), Opts) },
    hb_message:commit(#{
            <<"path">> => <<"schedule">>,
            <<"method">> => <<"POST">>,
            <<"body">> =>
                hb_message:commit(
                    MsgBase#{
                        <<"target">> => ProcID,
                        <<"type">> => <<"Message">>,
                        <<"random-seed">> => rand:uniform(1337)
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
    Base = #{
        <<"device">> => <<"stack@1.0">>,
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
    {ok, Req} = hb_ao:resolve(Base, <<"init">>, #{}),
    Req.

% execute_aos_call(Base) ->
%     Req =
%         hb_message:commit(#{
%                 <<"action">> => <<"Eval">>,
%                 <<"function">> => <<"json_result">>,
%                 <<"data">> => <<"return 2">>
%             },
%             hb:wallet()
%         ),
%     execute_aos_call(Base, Req).
% execute_aos_call(Base, Req) ->
%     hb_ao:resolve(Base,
%         #{
%             <<"path">> => <<"compute">>,
%             <<"body">> => Req
%         },
%         #{}
%     ).