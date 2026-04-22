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
-export([get/3, resolve/3, set/3, event/3, install/3]).
-export([make_table_meta/2]).
-include("include/hb.hrl").

%%% The set of devices that must be included in the device sandbox for an
%%% execution that is able to perform AO-Core resolutions. Without the following
%%% devices, all resolutions will fail.
-define(MINIMAL_AO_CORE_DEVICES, [<<"structured@1.0">>]).

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
                    hb_util:message_to_ordered_list(
                        hb_util:unique(DevNames ++ ?MINIMAL_AO_CORE_DEVICES)
                    )
                )
        end,
    ?event({adding_ao_core_resolver, {device_sandbox, AdmissibleDevs}}),
    ExecOpts =
        Opts#{
            preloaded_devices => AdmissibleDevs,
            hashpath => ignore
        },
    % Initialize the AO-Core resolver.
    BaseAOTable =
        case luerl:get_table_keys_dec([ao], State) of
            {ok, nil, _} ->
                ?event(no_ao_table),
                #{};
            {ok, ExistingTable, _} ->
                ?event({existing_ao_table, ExistingTable}),
                dev_lua:decode(ExistingTable, Opts)
        end,
    ?event({base_ao_table, BaseAOTable}),
    {ok, State2} =
        luerl:set_table_keys_dec(
            [ao],
            dev_lua:encode(BaseAOTable, Opts),
            State
        ),
    State3 = 
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
                                            luerl:decode(Arg, ImportState),
                                            Opts
                                        )
                                    end,
                                    RawArgs
                                ),
                            % Call the function with the decoded arguments.
                            {Res, ResState} =
                                ?MODULE:FuncName(Args, ImportState, ExecOpts),
                            % Encode the response for return to Lua
                            return(Res, ResState, Opts)
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
        ),
    % Install a type-level metatable for all luerl tables so that any table
    % created in Lua routes reads through hb_ao:get and writes through hb_ao:set.
    % Per-table metatables (set by encode_table) take precedence.
    GetFun =
        fun([RawTable, Key], S) ->
            Table = dev_lua:decode(luerl:decode(RawTable, S), Opts),
            ?event(debug_lua_getindex, {global_get, {table, Table}, {key, Key}}),
            case hb_ao:get(Key, Table, Opts) of
                not_found -> 
                    ?event(
                        debug_lua_getindex,
                        {global_get_not_found, {key, Key}}
                    ),
                    {[nil], S};
                R ->
                    ?event(
                        debug_lua_getindex,
                        {global_get_found, {key, Key}, {result, R}}
                    ),
                    Encodable = if is_map(R) -> hb_private:reset(R); true -> R end,
                    {Encoded, S2} = dev_lua:encode_value(Encodable, S, Opts),
                    {[Encoded], S2}
            end
        end,
    SetFun =
        fun([RawTable, Key, Value], S) ->
            Table = dev_lua:decode(luerl:decode(RawTable, S), Opts),
            DecodedValue = dev_lua:decode(luerl:decode(Value, S), Opts),
            ?event(
                debug_lua_setindex,
                {global_set,{table, Table}, {key, Key}, {value, DecodedValue}}
            ),
            SetResult =
                hb_cache:ensure_all_loaded(
                    hb_ao:set(Table, #{ Key => DecodedValue }, Opts),
                    Opts
                ),
            ?event(debug_lua_setindex, {set_result, {set_result, SetResult}}),
            %% Clear all data keys from the table (preserving the metatable ref)
            %% then write SetResult entries using raw writes
            S1 =
                luerl_heap:upd_table(
                    RawTable,
                    fun({table, _A, _D, Meta}) ->
                        {table, array:new([{default, nil}]), ttdict:new(), Meta}
                    end,
                    S
                ),
            ?event(debug_lua_setindex, {updated_table, {updated_table, S1}}),
            S2 = 
                maps:fold(
                    fun(K, V, SAcc) ->
                        {EncodedV, SAcc2} = dev_lua:encode_value(V, SAcc, Opts),
                        luerl_heap:raw_set_table_key(RawTable, K, EncodedV, SAcc2)
                    end,
                    S1,
                    SetResult
                ),
            ?event(debug_lua_setindex, {done_set_result_encoded}),
            {[], S2}
        end,
    {GetRef, State4} = luerl:encode(GetFun, State3),
    {SetRef, State5} = luerl:encode(SetFun, State4),
    {MetaRef, State6} =
        luerl_heap:alloc_table(
            [{<<"__index">>, GetRef}, {<<"__newindex">>, SetRef}],
            State5
        ),
    %% We want to remove the custom metatable from lua-reserved util tables.
    {EmptyMeta, State7} = luerl_heap:alloc_table([], State6),
    GRef = luerl_heap:get_global(State7),
    State8 = luerl_heap:set_metatable(GRef, EmptyMeta, State7),
    StdTables = 
        [
            <<"ao">>, <<"string">>, <<"table">>, <<"math">>, <<"os">>,
            <<"io">>, <<"package">>, <<"coroutine">>, <<"state">>
        ],
    State9 = 
        lists:foldl(
            fun(Name, SAcc) ->
                case luerl_heap:raw_get_table_key(GRef, Name, SAcc) of
                    nil -> SAcc;
                    Tref when is_tuple(Tref), element(1, Tref) =:= tref ->
                        luerl_heap:set_metatable(Tref, EmptyMeta, SAcc);
                    _ -> SAcc
                end
            end,
            State8,
            StdTables
        ),
    {ok, luerl_heap:set_table_type_meta(MetaRef, State9)}.

%% @doc Build the __index/__newindex metatable for a luerl state without
%% allocating or populating the table itself. Used to inject hb_ao:get and
%% hb_ao:set into the Lua environment, for all gets and sets using the 
%% lua metatable (__index/__newindex).
make_table_meta(St0, Opts) ->
    GetFun =
        fun([RawTable, Key], S) ->
            Table = dev_lua:decode(luerl:decode(RawTable, S), Opts),
            case hb_ao:get(Key, Table, Opts) of
                not_found ->
                    {[nil], S};
                R ->
                    Encodable = if is_map(R) -> hb_private:reset(R); true -> R end,
                    {Encoded, S2} = dev_lua:encode_value(Encodable, S, Opts),
                    {[Encoded], S2}
            end
        end,
    SetFun =
        fun([RawTable, Key, RawValue], S) ->
            Table = dev_lua:decode(luerl:decode(RawTable, S), Opts),
            Value = dev_lua:decode(luerl:decode(RawValue, S), Opts),
            SetResult =
                hb_cache:ensure_all_loaded(
                    hb_ao:set(Table, #{ Key => Value }, Opts),
                    Opts
                ),
            %% Clear all data keys from the table (preserving the metatable ref)
            %% then write SetResult entries using raw writes to bypass __newindex.
            S1 =
                luerl_heap:upd_table(
                    RawTable,
                    fun({table, _A, _D, Meta}) ->
                        {table, array:new([{default, nil}]), ttdict:new(), Meta}
                    end,
                    S
                ),
            S2 =
                maps:fold(
                    fun(K, V, SAcc) ->
                        {EncodedV, SAcc2} = dev_lua:encode_value(V, SAcc, Opts),
                        luerl_heap:raw_set_table_key(RawTable, K, EncodedV, SAcc2)
                    end,
                    S1,
                    SetResult
                ),
            ?event(debug_lua, {set_result_encoded}),
            {[], S2}
        end,
    {GetFunRef, St1} = luerl:encode(GetFun, St0),
    {SetFunRef, St2} = luerl:encode(SetFun, St1),
    luerl_heap:alloc_table(
        [{<<"__index">>, GetFunRef}, {<<"__newindex">>, SetFunRef}],
        St2
    ).

%% @doc Helper function for returning a result from a Lua function.
return(Result, ExecState, Opts) ->
    ?event(lua_import, {import_returning, {result, Result}}),
    TableEncoded = dev_lua:encode(hb_cache:ensure_all_loaded(Result, Opts), Opts),
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
    ParsedMsgs = hb_singleton:from(SingletonMsg, ExecOpts),
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

%% @doc A wrapper for `hb_ao''s `get' functionality.
get([Key, Base], ExecState, ExecOpts) ->
    ?event({ao_core_get, {base, Base}, {key, Key}}),
    NewRes = hb_ao:get(convert_as(Key), convert_as(Base), ExecOpts),
    ?event({ao_core_get_result, {result, NewRes}}),
    {[NewRes], ExecState}.

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