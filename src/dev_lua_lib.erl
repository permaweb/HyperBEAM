%%% @doc A module for providing AO library functions to the Lua environment.
%%% This module contains the implementation of the functions, each by the name
%%% that should be used in the `ao' table in the Lua environment. Every export
%%% is imported into the Lua environment.
%%% 
%%% Each function adheres closely to the Luerl calling convention, adding the 
%%% appropriate node message as a third argument:
%%% 
%%%     fun(Args, State, NodeMsg) -> {ResultTerms, NewState}
%%% 
%%% As Lua allows for multiple return values, each function returns a list of
%%% terms to grant to the caller. Matching the tuple convention used by AO-Core,
%%% the first term is typically the status, and the second term is the result.
-module(dev_lua_lib).
%%% Library functions. Each exported function is _automatically_ added to the
%%% Lua environment, except for the `install/3' function, which is used to
%%% install the library in the first place.
-export([resolve/3, set/3, event/3, install/3]).
-ifdef(ENABLE_LUA_CACHE).
-export([cache_write/3, cache_read/3]).
-endif.
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").


%% @doc Install the library into the given Lua environment.
install(Base, State, Opts) ->
    % Calculate and set the new `preloaded_devices' option.
    AllDevs = hb_opts:get(preloaded_devices, Opts),
    DevSandboxDef =
        hb_ao:get(
            <<"device-sandbox">>,
            {as, <<"message@1.0">>, Base},
            false,
            Opts
        ),
    AdmissibleDevs =
        case DevSandboxDef of
            false -> AllDevs;
            DevNames ->
                lists:map(
                    fun(Name) ->
                        [Dev] =
                            lists:filter(
                                fun(X) ->
                                    hb_ao:get(<<"name">>, X, Opts) == Name
                                end,
                                AllDevs
                            ),
                        Dev
                    end,
                    hb_util:message_to_ordered_list(DevNames)
                )
        end,
    ?event({adding_ao_core_resolver, {device_sandbox, AdmissibleDevs}}),
    ExecOpts = Opts#{ preloaded_devices => AdmissibleDevs },
    % Initialize the AO-Core resolver.
    BaseAOTable =
        case luerl:get_table_keys_dec([ao], State) of
            {ok, nil, _} ->
                ?event(no_ao_table),
                #{};
            {ok, ExistingTable, _} ->
                ?event({existing_ao_table, ExistingTable}),
                dev_lua:decode(ExistingTable)
        end,
    ?event({base_ao_table, BaseAOTable}),
    {ok, State2} =
        luerl:set_table_keys_dec(
            [ao],
            dev_lua:encode(BaseAOTable),
            State
        ),
    {
        ok,
        lists:foldl(
            fun(FuncName, StateIn) ->
                {ok, StateOut} =
                    luerl:set_table_keys_dec(
                        [ao, FuncName],
                        fun(RawArgs, ImportState) ->
                            ?event(lua_import, {calling_import, {func, FuncName}}),
                            % Decode the arguments from the Lua environment.
                            Args =
                                lists:map(
                                    fun(Arg) ->
                                        dev_lua:decode(
                                            luerl:decode(Arg, ImportState)
                                        )
                                    end,
                                    RawArgs
                                ),
                            % Call the function with the decoded arguments.
                            {Res, ResState} =
                                ?MODULE:FuncName(Args, ImportState, ExecOpts),
                            % Encode the response for return to Lua
                            return(Res, ResState)
                        end,
                        StateIn
                    ),
                StateOut
            end,
            State2,
            [
                FuncName
            ||
                {FuncName, _} <- dev_lua_lib:module_info(exports),
                FuncName /= module_info,
                FuncName /= ?FUNCTION_NAME
            ]
        )
    }.

%% @doc Helper function for returning a result from a Lua function.
return(Result, ExecState) ->
    ?event(lua_import, {import_returning, {result, Result}}),
    TableEncoded = dev_lua:encode(Result),
    {ReturnParams, ResultingState} =
        lists:foldr(
            fun(LuaEncoded, {Params, StateIn}) ->
                {NewParam, NewState} = luerl:encode(LuaEncoded, StateIn),
                {[NewParam | Params], NewState}
            end,
            {[], ExecState},
            TableEncoded
        ),
    ?event({lua_encoded, ReturnParams}),
    {ReturnParams, ResultingState}.

%% @doc A wrapper function for performing AO-Core resolutions. Offers both the 
%% single-message (using `hb_singleton:from/1' to parse) and multiple-message
%% (using `hb_ao:resolve_many/2') variants.
resolve([SingletonMsg], ExecState, ExecOpts) ->
    ?event({ao_core_resolver, {msg, SingletonMsg}}),
    ParsedMsgs = hb_singleton:from(SingletonMsg),
    ?event({parsed_msgs_to_resolve, ParsedMsgs}),
    resolve({many, ParsedMsgs}, ExecState, ExecOpts);
resolve([Base, Path], ExecState, ExecOpts) when is_binary(Path) ->
    PathParts = hb_path:term_to_path_parts(Path, ExecOpts),
    resolve({many, [Base] ++ PathParts}, ExecState, ExecOpts);
resolve(Msgs, ExecState, ExecOpts) when is_list(Msgs) ->
    resolve({many, Msgs}, ExecState, ExecOpts);
resolve({many, Msgs}, ExecState, ExecOpts) ->
    MaybeAsMsgs = lists:map(fun convert_as/1, Msgs),
    try hb_ao:resolve_many(MaybeAsMsgs, ExecOpts) of
        {Status, Res} ->
            ?event({resolved_msgs, {status, Status}, {res, Res}, {exec_opts, ExecOpts}}),
            {[Status, Res], ExecState}
    catch
        Error ->
            ?event(lua_error, {ao_core_resolver_error, Error}),
            {[<<"error">>, Error], ExecState}
    end.

%% @doc Converts any `as' terms from Lua to their HyperBEAM equivalents.
convert_as([<<"as">>, Device, RawMsg]) ->
    {as, Device, RawMsg};
convert_as(Other) ->
    Other.

%% @doc Wrapper for `hb_ao''s `set' functionality.
set([Base, Key, Value], ExecState, ExecOpts) ->
    ?event({ao_core_set, {base, Base}, {key, Key}, {value, Value}}),
    NewRes = hb_ao:set(Base, Key, Value, ExecOpts),
    ?event({ao_core_set_result, {result, NewRes}}),
    {[NewRes], ExecState};
set([Base, NewValues], ExecState, ExecOpts) ->
    ?event({ao_core_set, {base, Base}, {new_values, NewValues}}),
    NewRes = hb_ao:set(Base, NewValues, ExecOpts),
    ?event({ao_core_set_result, {result, NewRes}}),
    {[NewRes], ExecState}.

%% @doc Allows Lua scripts to signal events using the HyperBEAM hosts internal
%% event system.
event([Event], ExecState, Opts) ->
    ?event({recalling_event, Event}),
    event([global, Event], ExecState, Opts);
event([Group, Event], State, Opts) when is_list(Event) ->
    event([Group, list_to_tuple(Event)], State, Opts);
event([Group, Event], ExecState, Opts) ->
    ?event(
        lua_event,
        {event,
            {group, Group},
            {event, Event}
        }
    ),
    ?event(Group, Event),
    {[<<"ok">>], ExecState}.


-ifdef(ENABLE_LUA_CACHE). 

%% Lua Cache Functions Wrappers

%% @doc Wrapper for hb_cache:write/2 (structured message version).
%% Expects Lua call: ao.cache_write(message_map, opts?)
%% Returns [ok, UncommittedID] | [error, #{reason => Reason}]
cache_write(Args, ExecState, ExecOpts) ->
    [MsgMapLua | RestArgs] = Args,
    MsgMapErlang = dev_lua:decode(MsgMapLua),
    LuaOpts = maybe_decode_opts(RestArgs),
    FinalOpts = maps:merge(ExecOpts, LuaOpts),
    ?event(debug_lua_lib_cache, {cache_write_call, {msg, MsgMapErlang}, {final_opts, FinalOpts}}),
    Result = hb_cache:write(MsgMapErlang, FinalOpts),
    ReturnList = case Result of
        {ok, ID} -> [ok, ID]; % Return [ok, ID]
        {error, Reason} -> [error, #{ <<"reason">> => Reason }] % Return [error, ReasonMap]
    end,
    ?event(debug_lua_lib_cache, {cache_write_return_list, {list, ReturnList}}),
    {ReturnList, ExecState}.

%% @doc Wrapper for hb_cache:read/2.
%% Expects Lua call: ao.cache_read(id_or_path_string, opts?)
%% Returns [ok, Value] | [not_found] | [error, #{reason => Reason}]
cache_read(Args, ExecState, ExecOpts) ->
    [IDOrPathBin | RestArgs] = Args,
    LuaOpts = maybe_decode_opts(RestArgs),
    FinalOpts = maps:merge(ExecOpts, LuaOpts),
    ?event(dev_lua_lib, {cache_read_call, {id_or_path, IDOrPathBin}, {final_opts, FinalOpts}}),
    Result = hb_cache:read(IDOrPathBin, FinalOpts),
    ReturnList = case Result of
        {ok, Value} -> [ok, Value]; % Return [ok, Value]
        not_found -> [not_found]; % Return [not_found]
        {error, Reason} -> [error, #{ <<"reason">> => Reason }] % Return [error, ReasonMap]
    end,
    ?event(dev_lua_lib, {cache_read_return_list, {list, ReturnList}}),
    {ReturnList, ExecState}.

%% @doc Helper to decode optional Opts map from Lua args list.
%% Expects Opts map to be the last argument if present.
maybe_decode_opts([]) -> #{};
maybe_decode_opts(ArgsList) ->
    LastArg = lists:last(ArgsList),
    case dev_lua:decode(LastArg) of
        OptsMap when is_map(OptsMap) -> OptsMap;
        _ -> #{} % Ignore if not a map
    end.


%% Test calling ao.cache_read from Lua via dev_lua
lua_ao_cache_write_read_test() ->
    ?event(lua_cache_test, {starting}),
    Opts = #{ store => [#{ 
		<<"store-module">> => hb_store_fs, 
		<<"prefix">> => <<"cache-TEST-writeread">> 
	}]},
    TestMap = #{ 
		<<"type">> => <<"test_data">>, 
		<<"value">> => rand:uniform(1000), 
		<<"nested">> => [1, <<"two">>] },
    {ok, Script} = file:read_file("scripts/lua-cache-test.lua"),
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"module">> => #{
            <<"content-type">> => <<"application/lua">>,
            <<"body">> => Script
        }
    },
    WriteRequest = #{
        <<"path">> => <<"write_test">>,  % Lua function name
        <<"parameters">> => [TestMap]     % Arguments for Lua function
    },
    ?event(lua_cache_test, {resolving_lua_write, WriteRequest}),
    {ok, WriteResolveResult} = hb_ao:resolve(Base, WriteRequest, Opts),
    ?event(lua_cache_test, {resolve_write_result, WriteResolveResult}),
	Status = hb_ao:get(<<"status">>, WriteResolveResult, Opts),
	?event(lua_cache_test, {status, Status}),
	Value = hb_ao:get(<<"value">>, WriteResolveResult, Opts),
	?event(lua_cache_test, {value, Value}),
	% Assert the extracted values
	?assertEqual(<<"ok">>, Status),
	?assert(is_binary(Value)), % Assert that the value (ID) is a binary
    % Create and Resolve Read Request (Found case)
    ReadRequestFound = #{
        <<"path">> => <<"read_test">>,
        <<"parameters">> => [Value] % Argument: the ID from write step
    },
    ?event(lua_cache_test, {resolving_lua_read_found, ReadRequestFound}),
    {ok, ReadResolveResultFound} = hb_ao:resolve(Base, ReadRequestFound, Opts),
    ?event(lua_cache_test, {resolve_read_result_found, ReadResolveResultFound}),
	Status2 = hb_ao:get(<<"status">>, ReadResolveResultFound, Opts),
	?event(lua_cache_test, {read_resolve_status, Status2}),
	RetrievedValueMap = maps:get(<<"value">>, ReadResolveResultFound),
	?event(lua_cache_test, {read_resolve_value, RetrievedValueMap}),
	% Assert the extracted values
	?assertEqual(<<"ok">>, Status),
	?assertEqual(TestMap, maps:remove(<<"priv">>, RetrievedValueMap)),
    ok.

-endif.