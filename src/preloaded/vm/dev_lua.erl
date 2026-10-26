%%% @doc A device that calls a Lua module upon a request and returns the result.
-module(dev_lua).
-implements(<<"lua@5.3a">>).
-specification("../../../specs/lua@5.3a.md").
-export([device_info/1, init/3, snapshot/3, normalize/3, functions/3]).
%%% Public Utilities
-export([encode/2, decode/2]).
-include("include/hb.hrl").
-include_lib("luerl/include/luerl.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Utility macro to check if a binary is a Lua script content-type.
-define(IS_LUA_TYPE(CT), CT == <<"application/lua">> orelse CT == <<"text/x-lua">>).

%%% The set of functions that will be sandboxed by default. If the node message 
%%% parameter `lua-minimum-sandbox' is set to a different spec, it will be used.
%%% Messages may add to this sandbox spec, but they may not remove functions.
-define(DEFAULT_MIN_SANDBOX, [
    {['_G', loadfile], <<"sandboxed">>},
    {['_G', dofile], <<"sandboxed">>},
    {['_G', package, searchers], <<"sandboxed">>},  % disk module searcher
    {['_G', package, searchpath], <<"sandboxed">>}, % disk path prober
    {['_G', os, execute], <<"sandboxed">>},
    {['_G', os, exit], <<"sandboxed">>},
    {['_G', os, getenv], <<"sandboxed">>},
    {['_G', os, remove], <<"sandboxed">>},
    {['_G', os, rename], <<"sandboxed">>},
    {['_G', os, tmpname], <<"sandboxed">>}
]).

%%% The set of functions that will be sandboxed if the base sets
%%% `requires-determinism': their results depend on the node's random seed,
%%% clock and time zone.
-define(DETERMINISTIC_SANDBOX, [
    {['_G', math, random], <<"sandboxed">>},
    {['_G', os, clock], <<"sandboxed">>},
    {['_G', os, date], <<"sandboxed">>},
    {['_G', os, time], <<"sandboxed">>}
]).

%% @doc All keys that are not directly available in the base message are 
%% resolved by calling the Lua function in the module of the same name.
%% Additionally, we exclude the `keys', `set', `encode' and `decode' functions
%% which are `message@1.0' core functions, and Lua public utility functions.
device_info(Base) ->
    #{
        default => fun compute/4,
        excludes =>
            [
                <<"id">>,
                <<"commitments">>,
                <<"committers">>,
                <<"keys">>,
                <<"path">>,
                <<"set">>,
                <<"remove">>,
                <<"verify">>,
                <<"encode">>,
                <<"decode">>
            ] ++
            maps:keys(Base)
    }.

%% @doc Initialize the device state, loading the script into memory if it is 
%% a reference.
-spec init(
    #{
        module => _,
        'content-type' => binary(),
        body => _,
        sandbox => _,
        'requires-determinism' => _,
        _ => _
    },
    #{ _ => _ },
    #{ _ => _ }
) -> {ok, #{ _ => _ }} | {error, _}.
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

%% @doc Find the script(s) specified in the base message. If the `content-type'
%% key is set to `application/lua' or `text/x-lua', we assume that the `body' key
%% contains a Lua script. Additionally, if a `module' key may be present with
%% the following forms:
%% 1. A binary ID of a Lua module.
%% 2. A list of binary IDs of Lua modules.
%% 3. A message containing a series of named Lua modules.
find_modules(Base, Opts) ->
    MaybeBodyMod =
        case hb_ao:get(<<"content-type">>, {as, <<"message@1.0">>, Base}, Opts) of
            CT when ?IS_LUA_TYPE(CT) -> [Base];
            _ -> []
        end,
    ?event(
        debug_lua,
        {finding_modules, {base, Base}, {body_mod, MaybeBodyMod}},
        Opts
    ),
    case {hb_ao:get(<<"module">>, {as, <<"message@1.0">>, Base}, Opts), MaybeBodyMod} of
        {not_found, []} ->
            {error, <<"No Lua modules found when preparing environment for call.">>};
        {not_found, _} ->
            load_modules(MaybeBodyMod, Opts);
        {Module, _} when is_binary(Module)->
            find_modules(Base#{ <<"module">> => [Module] }, Opts);
        {Module, _} when is_map(Module) ->
            % If the module is a map, check its content type to see if it is 
            % a literal Lua module, or a map of modules with content types.
            case hb_ao:get(<<"content-type">>, Module, Opts) of
                LuaCT when ?IS_LUA_TYPE(LuaCT) ->
                    find_modules(
                        Base#{ <<"module">> => [Module] },
                        Opts
                    );
                _ ->
                    % If the script is not a literal Lua script binary, assume
                    % it is a map of scripts with content types, and recurse.
                    find_modules(
                        Base#{
                            <<"module">> => maps:values(Module)
                        },
                        Opts
                    )
            end;
        {Modules, _} when is_list(Modules) ->
            % We have found a list of scripts, load them.
            load_modules(MaybeBodyMod ++ Modules, Opts)
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
load_modules([Module|Rest], Opts, Acc) when is_map(Module) ->
    % We have found a message with a Lua module inside. Search for the binary
    % of the program in the body and the data.
    PlainModule = {as, <<"message@1.0">>, Module},
    ModuleBin =
        hb_ao:get_first(
            [
                {PlainModule, <<"body">>},
                {PlainModule, <<"data">>}
            ],
            not_found,
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
            ModuleRef =
                case hb_maps:find(<<"name">>, Module, Opts) of
                    {ok, Name} -> Name;
                    error -> hb_message:id(Module, all, Opts)
                end,
            % Load the module into the Lua state.
            load_modules(Rest, Opts, [{ModuleRef, ModuleBin}|Acc])
    end.

%% @doc Initialize a new Lua state with a given base message and module.
initialize(Base, Modules, Opts) ->
    % Apply the node's minimum sandbox and install the disk-free `require'
    % before loading any modules, such that code run while a module loads
    % cannot escape them. If the base sets `requires-determinism', also
    % sandbox the functions whose results depend on the node, with or without
    % the minimum sandbox. The message may add further restrictions via
    % `sandbox', but cannot lift these.
    State0 = create_state(Base, Opts),
    % Load each script into the Lua state.
    State1 =
        lists:foldl(
            fun({ModuleID, ModuleBin}, StateIn) ->
                ?event(
                    debug_lua,
                    {loading_module,
                        {module_id, ModuleID},
                        {module_bin, ModuleBin}
                    },
                    Opts
                ),
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
    % Apply any additional sandboxing requested by the message after module load.
    State2 =
        case hb_maps:get(<<"sandbox">>, Base, false, Opts) of
            false -> State1;
            true ->
                % Default sandbox has already been applied, so no-op.
                State1;
            Spec -> sandbox(State1, Spec, Opts)
        end,
    % Install the AO-Core Lua library into the state.
    {ok, State3} = dev_lua_lib:install(Base, State2, Opts),
    % Return the base message with the state added to it.
    {ok, hb_private:set(Base, <<"state">>, State3, Opts)}.

%% @doc Create a Lua state with no modules, in the node's minimum sandbox and
%% the sandbox that `requires-determinism' asks for.
create_state(Base, Opts) ->
    DeterministicSpec =
        case hb_util:bool(hb_maps:get(<<"requires-determinism">>, Base, false, Opts)) of
            true -> ?DETERMINISTIC_SANDBOX;
            false -> []
        end,
    case hb_opts:get(<<"lua-minimum-sandbox">>, ?DEFAULT_MIN_SANDBOX, Opts) of
        false -> sandbox(luerl:init(), DeterministicSpec, Opts);
        MinSpec ->
            dev_lua_require:install(
                sandbox(luerl:init(), MinSpec ++ DeterministicSpec, Opts),
                Opts
            )
    end.

%%% @doc Return a list of all functions in the Lua environment.
-spec functions(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, [_]} | {error, not_found}.
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

%% @doc Call the Lua script with the given arguments, returning the status
%% and result that the Lua function returns. A key with no function in the
%% script is not found.
-spec compute(
    binary(),
    #{ _ => _ },
    #{ _ => _ },
    #{ _ => _ }
) -> {atom(), _}.
compute(Key, RawBase, RawReq, Opts) ->
    ?event(debug_lua, compute_called),
    Req = 
        hb_cache:read_all_commitments(
            RawReq,
            Opts
        ),
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
            Opts#{ <<"hashpath">> => ignore }
        ),
    ?event(debug_lua, function_found),
    case luerl:get_table_keys_dec([Function], State) of
        {ok, nil, _} -> {error, not_found};
        _ -> call_function(Function, Base, Req, OldPriv, State, Opts)
    end.

%% @doc Call a function of the Lua state with the parameters that the request
%% or the base gives, or else with the base and the request.
call_function(Function, Base, Req, OldPriv, State, Opts) ->
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
            Opts#{ <<"hashpath">> => ignore }
        ),
    ?event(debug_lua, parameters_found),
    % Resolve all hyperstate links
    ResolvedParams = hb_cache:ensure_all_loaded(Params, Opts),
    % Call the VM function with the given arguments. The AO-Core library
    % functions read the node message from the private data of the Lua state.
    ?event(lua,
        {calling_lua_func,
            {function, Function},
            {args, ResolvedParams},
            {req, Req}
        }
    ),
    try
        process_response(
            luerl:call_function_dec(
                [Function],
                encode(ResolvedParams, Opts),
                luerl:put_private(<<"node-message">>, Opts, State)
            ),
            OldPriv,
            Opts
        )
    catch
        _:Reason:Stacktrace ->
            process_response({error, Reason, Stacktrace}, OldPriv, Opts)
    end.

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
            % Each call encodes its arguments and results as new tables in
            % the state. Luerl frees the tables that Lua cannot reach
            % only when it collects garbage, so collect after each call.
            {hb_util:atom(Status), Msg#{
                <<"priv">> => Priv#{
                    <<"state">> => luerl:gc(NewState)
                }
            }};
        NonMsgRes -> {hb_util:atom(Status), NonMsgRes}
    end;
process_response({ok, Results, _NewState}, _Priv, _Opts) ->
    % A Lua function returns a result, or a status and a result. Any other
    % number of values is an error of the script.
    {error,
        <<
            "Lua function returned ",
            (hb_util:bin(length(Results)))/binary,
            " values: it must return a result, or a status and a result."
        >>
    };
process_response({lua_error, RawError, State}, _Priv, Opts) ->
    % The script raised an error and did not catch it, with `error' or at
    % runtime. A script that throws is a failure, as a device that throws
    % is. Parse the stack trace and return it.
    Error = try decode(luerl:decode(RawError, State), Opts) catch _:_ -> RawError end,
    StackTrace = decode_stacktrace(luerl:get_stacktrace(State), State, Opts),
    ?event(lua_error, {lua_error, Error, {stacktrace, StackTrace}}),
    {failure, #{
        <<"status">> => 500,
        <<"body">> => Error,
        <<"trace">> => hb_ao:normalize_keys(StackTrace, Opts)
    }};
process_response({error, Reason, Trace}, _Priv, _Opts) ->
    % An Erlang exception of the Lua call, for example inside a library call
    % into the node, or of decoding its result. The node does not attest to
    % it, so it is a failure, not a result of the computation.
    ?event(lua_error, {trace, Trace}),
    TraceBin = iolist_to_binary(hb_format:trace(Trace)),
    ?event(lua_error, {formatted, {string, TraceBin}}),
    ReasonBin = iolist_to_binary(io_lib:format("~p", [Reason])),
    {failure, #{
        <<"status">> => 500,
        <<"body">> =>
            << "Erlang error while running Lua: ", ReasonBin/binary >>,
        <<"trace">> => TraceBin
    }}.

%% @doc Snapshot the Lua state from a live computation. Removes the node message
%% from the private data of the state and clears Luerl's `tag', then serializes
%% the state to a binary.
-spec snapshot(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ body := binary(), _ => _ }} | {error, binary()}.
snapshot(Base, _Req, Opts) ->
    case hb_private:get(<<"state">>, Base, Opts) of
        not_found ->
            {error, <<"Cannot snapshot Lua state: state not initialized.">>};
        RawState ->
            % Luerl sets the `tag' of a state to a reference made by the VM
            % that created it, and never reads it. Without it, equal states
            % give equal snapshots.
            State = luerl:delete_private(<<"node-message">>, RawState),
            Snapshot = luerl:externalize(State#luerl{ tag = undefined }),
            {ok, #{ <<"body">> => term_to_binary(Snapshot) }}
    end.

%% @doc Restore the Lua state from a snapshot, if it exists.
-spec normalize(
    #{ snapshot => #{ body => binary(), _ => _ }, _ => _ },
    #{ _ => _ },
    #{ _ => _ }
) -> {ok, #{ _ => _ }}.
normalize(Base, _Req, RawOpts) ->
    Opts = RawOpts#{ <<"hashpath">> => ignore },
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
                    {as, <<"message@1.0">>, Base},
                    Opts
                ),
            case SerializedState of
                not_found -> throw({error, no_lua_state_snapshot_found});
                State ->
                    ExternalizedState = decode_snapshot(State, Base, Opts),
                    InternalizedState = luerl:internalize(ExternalizedState),
                    ?event(snapshot, loaded_state_from_snapshot),
                    {ok, LibState} =
                        dev_lua_lib:reinstall(Base, InternalizedState, Opts),
                    {ok, hb_private:set(Base, <<"state">>, LibState, Opts)}
            end;
        _ ->
            ?event(snapshot, state_already_initialized),
            {ok, Base}
    end.

%% @doc Decode a snapshot, which may come from the caller, without creating
%% atoms. Each Erlang function in it that a new state for the base does not
%% contain becomes `nil'; restore sets the host library functions again.
decode_snapshot(Serialized, Base, Opts) ->
    % Collect the garbage of the new state first: sandboxing `package.searchers'
    % leaves the disk searchers in its table store.
    Allowed = sets:from_list(collect_mfas(luerl:gc(create_state(Base, Opts)))),
    strip_host_functions(binary_to_term(Serialized, [safe]), Allowed).

%% @doc Collect the Erlang functions in a term.
collect_mfas(#erl_mfa{} = MFA) -> [MFA];
collect_mfas([Head | Tail]) -> collect_mfas(Head) ++ collect_mfas(Tail);
collect_mfas(Term) when is_tuple(Term) -> collect_mfas(tuple_to_list(Term));
collect_mfas(Term) when is_map(Term) -> collect_mfas(maps:to_list(Term));
collect_mfas(_Term) -> [].

%% @doc Replace each Erlang function in a term that is not in `Allowed' with
%% `nil'. Luerl's wrappers around Lua functions become the Lua functions they
%% call.
strip_host_functions(#erl_func{ code = Fun }, Allowed) ->
    case unwrap_lua_function(Fun) of
        {ok, LuaFunction} -> strip_host_functions(LuaFunction, Allowed);
        error -> nil
    end;
strip_host_functions(#erl_mfa{} = MFA, Allowed) ->
    case sets:is_element(MFA, Allowed) of
        true -> MFA;
        false -> nil
    end;
strip_host_functions(Fun, _Allowed) when is_function(Fun) ->
    nil;
strip_host_functions([Head | Tail], Allowed) ->
    [strip_host_functions(Head, Allowed) | strip_host_functions(Tail, Allowed)];
strip_host_functions(Term, Allowed) when is_tuple(Term) ->
    list_to_tuple(strip_host_functions(tuple_to_list(Term), Allowed));
strip_host_functions(Term, Allowed) when is_map(Term) ->
    maps:from_list(strip_host_functions(maps:to_list(Term), Allowed));
strip_host_functions(Term, _Allowed) ->
    Term.

%% @doc Find the Lua function in one of Luerl's wrappers, which call it with
%% their arguments. Luerl makes them when it decodes a table that contains a Lua
%% function and encodes it again, as `dev_lua_lib:install/3' does with `ao'.
unwrap_lua_function(Fun) when is_function(Fun, 2) ->
    case [erlang:fun_info(Fun, Item) || Item <- [module, name, env]] of
        [{module, luerl}, {name, '-encode/2-fun-1-'}, {env, [Inner]}] ->
            unwrap_lua_function(Inner);
        [{module, luerl}, {name, '-decode_luafunc/3-fun-0-'}, {env, [Lua]}]
                when is_record(Lua, funref) ->
            {ok, Lua};
        _ ->
            error
    end;
unwrap_lua_function(_Term) ->
    error.

%% @doc Decode a Lua result into a HyperBEAM `structured@1.0' message, removing
%% the commitments that do not verify. No commitment is added, so a message
%% that passes through Lua unchanged keeps exactly the commitments it had.
%% Every key is a binary, as the cache writes it. A key that is a table, and a
%% value that is a function or userdata, which Luerl gives as a fun or a tuple,
%% are dropped, as a message cannot have them.
decode(EncMsg, Opts) ->
    hb_message:normalize_commitments(
        do_decode(EncMsg, Opts),
        Opts,
        #{ <<"verify">> => all, <<"add-unsigned">> => false }
    ).
do_decode(EncMsg, _Opts) when is_list(EncMsg) andalso length(EncMsg) == 0 ->
    % The value is an empty table, so we assume it is a message rather than
    % a list.
    #{};
do_decode(EncMsg = [{_K, _V} | _], Opts) when is_list(EncMsg) ->
    do_decode(
        maps:map(
            fun(Key, V) -> decode_value(Key, do_decode(V, Opts)) end,
            hb_ao:normalize_keys(
                maps:from_list(
                    [
                        {K, V}
                    ||
                        {K, V} <- EncMsg,
                        not is_list(K),
                        not is_function(V),
                        not is_tuple(V)
                    ]
                ),
                Opts
            )
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

%% @doc Decode a value of a message from Lua. Lua gives the Arweave committer
%% of a commitment as its address, so we convert it back to its URI.
decode_value(<<"commitments">>, Commitments) when is_map(Commitments) ->
    map_committers(fun hb_committer:uri/1, Commitments);
decode_value(_Key, Value) ->
    Value.

%% @doc Encode a HyperBEAM `structured@1.0' message into a Lua term. Arweave
%% committers are given to Lua as their addresses, because Lua code compares
%% them with the addresses that process definitions hold.
encode(Map, Opts) ->
    hb_message:normalize_commitments(do_encode(Map, Opts), Opts).
do_encode(Map, Opts) when is_map(Map) ->
    hb_cache:ensure_all_loaded(
        case hb_util:is_ordered_list(Map, Opts) of
            true -> do_encode(hb_util:message_to_ordered_list(Map), Opts);
            false ->
                maps:to_list(
                    maps:map(fun(K, V) -> encode_value(K, V, Opts) end, Map)
                )
        end,
        Opts
    );
do_encode(List, Opts) when is_list(List) ->
    hb_cache:ensure_all_loaded(
        lists:map(fun(V) -> do_encode(V, Opts) end, List),
        Opts
    );
% Luerl holds Lua's `nil' as the atom `nil'.
do_encode(nil, _Opts) -> nil;
do_encode(Atom, _Opts) when is_atom(Atom) and (Atom /= false) and (Atom /= true)->
    hb_util:bin(Atom);
do_encode(Other, _Opts) ->
    Other.

%% @doc Encode a value of a message for Lua. Lua sees only the signed
%% commitments of a message: each node makes its own unsigned commitments, so
%% a script that read them could compute differently on two nodes. The Arweave
%% committer of a commitment is given as its address.
encode_value(<<"commitments">>, Commitments, Opts) when is_map(Commitments) ->
    Signed =
        hb_maps:filter(
            fun(_ID, Commitment) ->
                hb_maps:is_key(<<"committer">>, Commitment, Opts)
            end,
            Commitments,
            Opts
        ),
    do_encode(map_committers(fun hb_util:committer_address/1, Signed), Opts);
encode_value(_Key, Value, Opts) ->
    do_encode(Value, Opts).

%% @doc Apply `Fun' to the committer of each commitment.
map_committers(Fun, Commitments) ->
    maps:map(
        fun(_ID, Commitment = #{ <<"committer">> := Committer }) ->
            Commitment#{ <<"committer">> => Fun(Committer) };
           (_ID, Commitment) ->
            Commitment
        end,
        Commitments
    ).

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
    ?assertEqual(2, hb_ao:get(<<"assoctable/b">>, Base, #{})),
    InlineBase = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"content-type">> => <<"application/lua">>,
        <<"data">> => Script,
        <<"parameters">> => []
    },
    ?assertEqual(2, hb_ao:get(<<"assoctable/b">>, InlineBase, #{})).

post_invocation_message_validation_test() ->
    {ok, Script} = file:read_file("test/test.lua"),
    Opts = #{ <<"priv-wallet">> => hb:wallet() },
    Base =
        hb_message:commit(
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
            Opts,
            #{ <<"type">> => <<"unsigned">> }
    ),
    {ok, UnsignedID} = hb_cache:write(Base, Opts),
    ?event({base, {msg, Base}, {unsigned_id, UnsignedID}}),
    {ok, Res} = hb_ao:resolve(Base, <<"mutate_test_key">>, Opts),
    {ok, ResID} = hb_cache:write(Res, Opts),
    ?event({res_id, ResID}),
    {ok, ReadMsg} = hb_cache:read(UnsignedID, Opts),
    ?assertEqual(<<"test-value-1">>, hb_ao:get(<<"test-key">>, ReadMsg, Opts)),
    ?assert(length(hb_message:signers(Res, Opts)) == 0),
    ?assert(hb_message:verify(Res, all, Opts)),
    % Lua sees the signed commitment of the base alone.
    ?assertEqual({ok, 1}, hb_ao:resolve(Base, <<"commitment_count">>, Opts)),
    % A `committer' key outside of a commitment is not converted.
    Bare = #{ <<"committer">> => <<"bare">> },
    ?assertEqual(Bare, decode(encode(Bare, Opts), Opts)).

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
    ),
    % A key with no normalized form is a bad argument, which `pcall' catches.
    ?assertEqual(
        {ok, [false, <<"bad argument table,table to 'set'">>]},
        hb_ao:resolve(Base, <<"ao_set_float_key">>, #{})
    ),
    % An exception inside the library call into the node -- here, reading a
    % message this node does not have -- depends on this node, so it is a
    % failure, not a result of the computation.
    ?assertMatch(
        {failure,
            #{
                <<"status">> := 500,
                <<"body">> := <<"Erlang error while running Lua: ", _/binary>>
            }
        },
        hb_ao:resolve(Base, <<"ao_get_as_missing">>, #{})
    ),
    % A result that has a float key cannot be decoded, so it is a failure.
    ?assertMatch(
        {failure, _},
        hb_ao:resolve(Base, <<"float_key_result">>, #{})
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
    ?assertMatch({failure, _}, hb_ao:resolve(Base, <<"sandboxed_fail">>, #{})).

default_sandboxed_failure_test() ->
    {ok, Module} = file:read_file("test/test.lua"),
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"module">> => #{
            <<"content-type">> => <<"application/lua">>,
            <<"body">> => Module
        },
        <<"parameters">> => []
    },
    ?assertMatch({failure, _}, hb_ao:resolve(Base, <<"sandboxed_fail">>, #{})).

minimum_sandbox_overrides_false_test() ->
    {ok, Module} = file:read_file("test/test.lua"),
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"module">> => #{
            <<"content-type">> => <<"application/lua">>,
            <<"body">> => Module
        },
        <<"parameters">> => [],
        <<"sandbox">> => false
    },
    ?assertMatch({failure, _}, hb_ao:resolve(Base, <<"sandboxed_fail">>, #{})).

lua_minimum_sandbox_can_be_disabled_test() ->
    {ok, Module} = file:read_file("test/test.lua"),
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"module">> => #{
            <<"content-type">> => <<"application/lua">>,
            <<"body">> => Module
        },
        <<"parameters">> => [],
        <<"sandbox">> => false
    },
    ?assertMatch(
        {ok, _},
        hb_ao:resolve(
            Base,
            <<"sandboxed_fail">>,
            #{ <<"lua-minimum-sandbox">> => false }
        )
    ).

module_load_is_sandboxed_by_default_test() ->
    Module =
        <<
            """
            local ok = pcall(os.getenv, "PWD")
            function load_sandboxed()
                if ok then
                    return "escaped"
                else
                    return "sandboxed"
                end
            end
            """
        >>,
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"module">> => #{
            <<"content-type">> => <<"application/lua">>,
            <<"body">> => Module
        },
        <<"parameters">> => []
    },
    ?assertEqual(
        {ok, <<"sandboxed">>},
        hb_ao:resolve(Base, <<"load_sandboxed">>, #{})
    ).

io_popen_is_sandboxed_by_default_test() ->
    Module =
        <<
            """
            function popen_fail()
                return io.popen("id")
            end
            """
        >>,
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"module">> => #{
            <<"content-type">> => <<"application/lua">>,
            <<"body">> => Module
        },
        <<"parameters">> => []
    },
    ?assertMatch({failure, _}, hb_ao:resolve(Base, <<"popen_fail">>, #{})).

require_cannot_load_from_disk_test() ->
    % Untrusted code cannot restore `require's filesystem search by
    % reassigning `package.path': `test/test.lua' exists on disk, but the
    % sandboxed `require' resolves only from `package.loaded'/`package.preload'.
    Module =
        <<
            """
            function disk_require()
                package.path = "./test/?.lua"
                local ok = pcall(require, "test")
                if ok then return "loaded-from-disk" else return "blocked" end
            end
            """
        >>,
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"module">> => #{
            <<"content-type">> => <<"application/lua">>,
            <<"body">> => Module
        },
        <<"parameters">> => []
    },
    % Positive control: with the sandbox disabled, the native `require' does
    % load `test/test.lua' from disk -- proving the vector is real and that
    % `blocked' under the default is meaningful, not an incidental failure.
    ?assertEqual(
        {ok, <<"loaded-from-disk">>},
        hb_ao:resolve(
            Base,
            <<"disk_require">>,
            #{ <<"lua-minimum-sandbox">> => false }
        )
    ),
    ?assertEqual(
        {ok, <<"blocked">>},
        hb_ao:resolve(Base, <<"disk_require">>, #{})
    ).

require_serves_preloaded_modules_test() ->
    % The sandboxed `require' still resolves modules registered in
    % `package.preload', as legitimate processes such as AOS rely upon.
    Module =
        <<
            """
            function preload_require()
                package.preload["mymod"] = function() return "from-preload" end
                return require("mymod")
            end
            """
        >>,
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"module">> => #{
            <<"content-type">> => <<"application/lua">>,
            <<"body">> => Module
        },
        <<"parameters">> => []
    },
    ?assertEqual(
        {ok, <<"from-preload">>},
        hb_ao:resolve(Base, <<"preload_require">>, #{})
    ).

package_disk_searcher_is_sandboxed_test() ->
    % The native filesystem searcher cannot be invoked directly either:
    % `package.searchers' is rendered inoperable, so no callable disk loader
    % remains in the `package' table.
    Module =
        <<
            """
            function searcher_type()
                return type(package.searchers)
            end
            """
        >>,
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"module">> => #{
            <<"content-type">> => <<"application/lua">>,
            <<"body">> => Module
        },
        <<"parameters">> => []
    },
    ?assertEqual(
        {ok, <<"string">>},
        hb_ao:resolve(Base, <<"searcher_type">>, #{})
    ).

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
    ?assertEqual(<<"Hello, AO world!">>, Res),
    % A key that `ao.set' sets to `nil' is removed, and `ao.get' gives `nil'.
    ?assertEqual({ok, <<"nil">>}, hb_ao:resolve(Base, <<"ao_set_nil">>, #{})).

%% @doc Benchmark the performance of Lua executions.
direct_benchmark_test() ->
    BenchTime = 0.25,
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
    ).

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
    ?assertEqual(<<"Alice">>, hb_ao:get(<<"hello">>, Result2, #{})),
    % A key with no function in the script is not found.
    ?assertEqual(
        {error, not_found},
        hb_ao:resolve(Base, <<"no_function">>, #{})
    ).

%% @doc Use a Lua module as a hook on the HTTP server via `~meta@1.0'.
lua_http_hook_test() ->
    {ok, Module} = file:read_file("test/test.lua"),
    Node = hb_http_server:start_node(
        #{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"on">> => #{
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
    {ok, _} = hb_ao:resolve(Process, Message, #{ <<"hashpath">> => ignore }),
    {ok, Results} = hb_ao:resolve(Process, <<"now">>, #{}),
    ?assertEqual(42, hb_ao:get(<<"results/output/body">>, Results, #{})).

%% @doc Call a process whose `execution-device' is set to `lua@5.3a'.
pure_lua_restore_test() ->
    Opts = #{ <<"process-cache-frequency">> => 1 },
    Process = generate_lua_process("test/test.lua", Opts),
    {ok, _} = hb_cache:write(Process, Opts),
    Message = generate_test_message(Process, Opts, #{ <<"path">> => <<"inc">>}),
    {ok, _} = hb_ao:resolve(Process, Message, Opts#{ <<"hashpath">> => ignore }),
    {ok, Count1} = hb_ao:resolve(Process, <<"now/count">>, Opts),
    ?assertEqual(1, Count1),
    hb_ao:resolve(
        Process,
        generate_test_message(Process, #{}, #{ <<"path">> => <<"inc">>}),
        Opts
    ),
    {ok, Count2} = hb_ao:resolve(Process, <<"now/count">>, Opts),
    ?assertEqual(2, Count2).

%% @doc Snapshots of a Lua state do not contain the wallet of the node, and
%% restore to a state whose AO-Core library functions still resolve.
snapshot_excludes_node_wallet_test() ->
    Wallet = #{ <<"priv">> := #{ <<"d">> := PrivKey } } = ar_wallet:new(),
    Opts = #{
        <<"priv-wallet">> => Wallet,
        <<"store">> => [hb_test_utils:test_store()]
    },
    {ok, Script} = file:read_file("test/test.lua"),
    {ok, ModuleID} =
        hb_cache:write(
            #{
                <<"content-type">> => <<"application/lua">>,
                <<"body">> => Script
            },
            Opts
        ),
    Node = hb_http_server:start_node(Opts),
    % Snapshot the state over HTTP after an invocation.
    {ok, Snapshot} =
        hb_http:get(
            Node,
            <<
                "/~lua@5.3a&module=", ModuleID/binary,
                "/init/hello~lua@5.3a/snapshot~lua@5.3a/body"
            >>,
            #{}
        ),
    ?assertEqual(nomatch, binary:match(Snapshot, PrivKey)),
    % Restore the state and call a function that uses the AO-Core library.
    {ok, Restored} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"lua@5.3a">>,
                <<"snapshot">> => #{ <<"body">> => Snapshot }
            },
            <<"normalize">>,
            Opts
        ),
    ?assertEqual(
        {ok, <<"Hello, AO world!">>},
        hb_ao:resolve(Restored, <<"ao_resolve">>, Opts)
    ).

pure_lua_process_benchmark_test_() ->
    {timeout,
        30,
        fun() ->
            pure_lua_process_benchmark(#{
                <<"process-snapshot-slots">> => 50
            })
    end}.
pure_lua_process_benchmark(Opts) ->
    BenchMsgs = 30,
    hb:init(),
    Process = generate_lua_process("test/test.lua", Opts),
    {ok, _} = hb_cache:write(Process, Opts),
    Message = generate_test_message(Process, Opts),
    lists:foreach(
        fun(X) ->
            hb_ao:resolve(Process, Message, Opts#{ <<"hashpath">> => ignore }),
            ?event(debug_lua, {scheduled, X})
        end,
        lists:seq(1, BenchMsgs)
    ),
    ?event(debug_lua, {executing, BenchMsgs}),
    BeforeExec = os:system_time(millisecond),
    {ok, _} = hb_ao:resolve(Process, <<"now">>, Opts),
    AfterExec = os:system_time(millisecond),
    ExecMs = AfterExec - BeforeExec,
    hb_test_utils:benchmark_print(
        <<"Pure Lua process: Computed">>,
        <<"slots">>,
        BenchMsgs,
        ExecMs / 1000
    ),
    ?assert(ExecMs =< 500).

invoke_aos_test() ->
    Opts = #{ <<"priv-wallet">> => hb:wallet() },
    Process = generate_lua_process("test/hyper-aos.lua", Opts),
    {ok, _Proc} = hb_cache:write(Process, Opts),
    Message = generate_test_message(Process, Opts),
    {ok, _Assignment} =
        hb_ao:resolve(Process, Message, Opts#{ <<"hashpath">> => ignore }),
    {ok, Results} = hb_ao:resolve(Process, <<"now/results/output">>, Opts),
    % The process gives its authority by address, and trusts its message.
    ?assertEqual(<<"1">>, hb_ao:get(<<"data">>, Results, #{})),
    ?assertEqual(<<"aos> ">>, hb_ao:get(<<"prompt">>, Results, #{})),
    % A process that does not set `requires-determinism' cannot call
    % `math.random'.
    RandomMessage = generate_test_message(Process, Opts, <<"math.random">>),
    {ok, _} =
        hb_ao:resolve(Process, RandomMessage, Opts#{ <<"hashpath">> => ignore }),
    ?assertEqual(
        {ok, <<"sandboxed">>},
        hb_ao:resolve(Process, <<"now/results/output/data">>, Opts)
    ).

aos_authority_not_trusted_test() ->
    Opts = #{ <<"priv-wallet">> => ar_wallet:new() },
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
    {ok, _} = hb_ao:resolve(Process, Message, Opts#{ <<"hashpath">> => ignore }),
    {ok, Results} = hb_ao:resolve(Process, <<"now/results/output/data">>, Opts),
    ?assertEqual(<<"Message is not trusted.">>, Results).

%% @doc Benchmark the performance of Lua executions.
aos_process_benchmark_test_() ->
    {timeout, 30, fun() ->
        BenchMsgs = 6,
        Opts = #{
            <<"hashpath">> => ignore,
            <<"process-snapshot-slots">> => 50
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
        ExecMs = AfterExec - BeforeExec,
        hb_test_utils:benchmark_print(
            <<"HyperAOS process: Computed">>,
            <<"slots">>,
            BenchMsgs,
            ExecMs / 1000
        ),
        ?assert(ExecMs =< 250)
    end}.

%%% Test helpers

%% @doc Generate a Lua process message.
generate_lua_process(File, Opts) ->
    NormOpts =
        Opts#{ <<"priv-wallet">> => hb_opts:get(priv_wallet, hb:wallet(), Opts) },
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
    NormOpts =
        Opts#{ <<"priv-wallet">> => hb_opts:get(priv_wallet, hb:wallet(), Opts) },
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
