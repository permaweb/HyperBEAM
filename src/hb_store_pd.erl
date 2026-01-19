-module(hb_store_pd).

%% Standard hb_store exports
-export([start/1, stop/1, reset/1, scope/0]).
-export([read/2, write/3, list/2, match/2]).
-export([type/2, resolve/2, path/2, add_path/3]).
-export([make_group/2, make_link/3]).
-export([flush/2, flush/3, buffer_size/1, keys/1]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Start the process dictionary store.
-spec start(map()) -> {ok, map()} | {error, term()}.
start(_Opts = #{ <<"name">> := DataPath }) ->
    ?event(store_pd, {starting, DataPath}),
    erlang:put(get_state_key(DataPath), #{}),
    {ok, #{}};
start(_) ->
    {error, {badarg, <<"name is required in store configuration">>}}.

%% @doc Stop the store by clearing all data.
-spec stop(map()) -> ok.
stop(Opts) ->
    reset(Opts).

%% @doc Reset the store by erasing all data from the process dictionary.
-spec reset(map()) -> ok.
reset(_Opts = #{<<"name">> := Name}) ->
    StateKey = get_state_key(Name),
    erlang:erase(StateKey),
    ok.

%% @doc Return the scope of this store.
%% Process dictionary storage is always in-memory and process-local.
-spec scope() -> in_memory.
scope() -> in_memory.

%% @doc Write a key-value pair to the process dictionary.
%% This operation is extremely fast as it only modifies local process state.
-spec write(map(), binary(), binary()) -> ok.
write(#{<<"name">> := Name}, Key, Value) ->
    StateKey = get_state_key(Name),
    ?event({ statekey, StateKey}),
    Data = case erlang:get(StateKey) of
        undefined -> #{};
        D -> D
    end,
    ?event({data, Data}),
    erlang:put(StateKey, Data#{Key => Value}),
    ok.

%% @doc Read a value from the process dictionary by key.
-spec read(map(), binary()) -> {ok, binary()} | not_found.
read(#{<<"name">> := Name}, Key) ->
    StateKey = get_state_key(Name),
    case erlang:get(StateKey) of
        undefined ->
            not_found;
        Data ->
            case maps:find(Key, Data) of
                {ok, Value} ->
                    ?event(store_pd, {read_hit, Name, Key}),
                    {ok, Value};
                error ->
                    ?event(store_pd, {read_miss, Name, Key}),
                    not_found
            end
    end.

%% @doc List all keys in the store.
-spec list(map(), binary()) -> {ok, [binary()]} | not_found.
list(#{<<"name">> := Name}, _Path) ->
    StateKey = get_state_key(Name),
    case erlang:get(StateKey) of
        undefined ->
            not_found;
        Data ->
            {ok, maps:keys(Data)}
    end.

%% @doc Match keys by pattern.
-spec match(map(), binary()) -> {ok, [binary()]} | not_found.
match(Opts, _Pattern) ->
    list(Opts, <<>>).

%% @doc Get the type of a value.
-spec type(map(), binary()) -> simple | not_found.
type(Opts, Key) ->
    case read(Opts, Key) of
        {ok, _Value} -> simple;
        not_found -> not_found
    end.

%% @doc Resolve a path by following links.
-spec resolve(map(), binary()) -> binary().
resolve(_Opts, Path) ->
    Path.

%% @doc Transform a path to the store's canonical form.
-spec path(map(), binary() | [binary()]) -> binary().
path(_Opts, Path) when is_binary(Path) ->
    Path;
path(_Opts, PathParts) when is_list(PathParts) ->
    hb_util:encode(PathParts).

%% @doc Add two path components together.
-spec add_path(map(), binary(), binary()) -> binary().
add_path(_Opts, Path1, Path2) ->
    <<Path1/binary, "/", Path2/binary>>.

%% @doc Create a group 
-spec make_group(map(), binary()) -> {error, not_implemented}.
make_group(_Opts, _Path) ->
    {error, not_implemented}.

%% @doc Create a symbolic link from one key to another.
-spec make_link(map(), binary(), binary()) -> {error, not_implemented}.
make_link(_Opts, _Existing, _New) ->
    {error, not_implemented}.
    
%% @doc Flush all data from the process dictionary to a target store.
-spec flush(map(), map()) -> {ok, map()}.
flush(PDStoreOpts, TargetStoreOpts) ->
    flush(PDStoreOpts, TargetStoreOpts, #{}).

-spec flush(map(), map(), map()) -> {ok, map()}.
flush(#{<<"name">> := Name}, TargetStoreOpts, FlushOpts) ->
    StateKey = get_state_key(Name),
    case erlang:get(StateKey) of
        undefined ->
            ?event(store_pd, {flush_empty, Name}),
            {ok, #{written => 0, failed => []}};
        Data ->
            ?event(store_pd, {flush_start, Name, {keys, map_size(Data)}}),
            %% Write each key-value pair to the target store
            Results = maps:fold(
                fun(Key, Value, Acc = #{written := W, failed := F}) ->
                    case hb_store:write(TargetStoreOpts, Key, Value) of
                        ok ->
                            Acc#{written => W + 1};
                        Error ->
                            ?event(error,
                                {store_pd_flush_failed,
                                    {key, Key},
                                    {error, Error}
                                }
                            ),
                            Acc#{failed => [{Key, Error} | F]}
                    end
                end,
                #{written => 0, failed => []},
                Data
            ),

            %% Determine whether to clear the buffer
            ShouldClear = should_clear_buffer(Results, FlushOpts),
            case ShouldClear of
                true ->
                    erlang:erase(StateKey),
                    ?event(store_pd, {flush_complete_cleared, Name, Results});
                false ->
                    ?event(store_pd, {flush_complete_kept, Name, Results})
            end,

            {ok, Results}
    end.

%% @doc Get the number of keys currently in the buffer.
-spec buffer_size(map()) -> non_neg_integer().
buffer_size(#{<<"name">> := Name}) ->
    StateKey = get_state_key(Name),
    case erlang:get(StateKey) of
        undefined -> 0;
        Data -> map_size(Data)
    end.

%% @doc Get all keys currently in the buffer.
-spec keys(map()) -> [binary()].
keys(#{<<"name">> := Name}) ->
    StateKey = get_state_key(Name),
    case erlang:get(StateKey) of
        undefined -> [];
        Data -> maps:keys(Data)
    end.

%% @doc Generate the process dictionary key for a given store name.
-spec get_state_key(binary()) -> {?MODULE, binary()}.
get_state_key(Name) ->
    {?MODULE, Name}.

%% @doc Determine if the buffer should be cleared after flush.
-spec should_clear_buffer(map(), map()) -> boolean().
should_clear_buffer(#{failed := []}, FlushOpts) ->
    %% No failures - respect clear_on_flush option (default true)
    maps:get(<<"clear-on-flush">>, FlushOpts, true);
should_clear_buffer(#{failed := [_|_]}, FlushOpts) ->
    %% Some failures - only clear if clear_on_error is true (default false)
    maps:get(<<"clear-on-error">>, FlushOpts, false).
    
%%% Tests

basic_write_read_test() ->
    Store = #{ <<"store-module">> => <<"hb_store_pd">>, <<"name">> => <<"test">> },
    {ok, _} = start(Store),
    ok = write(Store, <<"key1">>, <<"value1">>),
    ?event({ data, erlang:get()}),
    ?assertEqual({ok, <<"value1">>}, read(Store, <<"key1">>)),
    ?assertEqual(not_found, read(Store, <<"key2">>)),
    ok = reset(Store).

list_keys_test() ->
    Store = #{ <<"store-module">> => <<"hb_store_pd">>, <<"name">> => <<"test">> },
    {ok, _} = start(Store),
    ?event({lissts, list(Store, <<>>)}),
    ?assertEqual({ok, []}, list(Store, <<>>)),
    ok = write(Store, <<"key1">>, <<"value1">>),
    ok = write(Store, <<"key2">>, <<"value2">>),
    {ok, Keys} = list(Store, <<>>),
    ?assertEqual(2, length(Keys)),
    ?assert(lists:member(<<"key1">>, Keys)),
    ?assert(lists:member(<<"key2">>, Keys)),
    ok = reset(Store).

flush_test() ->
    PDStore = #{
        <<"store-module">> => <<"hb_store_pd">>, 
        <<"name">> => <<"buffer">>
    },
    {ok, _} = start(PDStore),
    TargetStore = hb_test_utils:test_store(hb_store_lmdb, <<"flush_test_db">>),
    hb_store:reset(TargetStore),
    ok = write(PDStore, <<"key1">>, <<"value1">>),
    ok = write(PDStore, <<"key2">>, <<"value2">>),
    ?assertEqual({ok, <<"value1">>}, read(PDStore, <<"key1">>)),
    ?assertEqual(not_found, hb_store:read(TargetStore, <<"key1">>)),
    {ok, Stats} = flush(PDStore, TargetStore),
    ?assertEqual(2, maps:get(written, Stats)),
    ?assertEqual([], maps:get(failed, Stats)),
    ?assertEqual(not_found, read(PDStore, <<"key1">>)),
    ?assertEqual({ok, <<"value1">>}, hb_store:read(TargetStore, <<"key1">>)),
    ?assertEqual({ok, <<"value2">>}, hb_store:read(TargetStore, <<"key2">>)).

buffer_size_test() ->
    Store = #{<<"store-module">> => <<"hb_store_pd">>, <<"name">> => <<"test">>},
    {ok, _} = start(Store),
    ?assertEqual(0, buffer_size(Store)),
    ok = write(Store, <<"key1">>, <<"value1">>),
    ?assertEqual(1, buffer_size(Store)),
    ok = write(Store, <<"key2">>, <<"value2">>),
    ?assertEqual(2, buffer_size(Store)),
    ok = reset(Store),
    ?assertEqual(0, buffer_size(Store)).

multiple_buffers_test() ->
    Buffer1 = #{
        <<"store-module">> => <<"hb_store_pd">>, 
        <<"name">> => <<"buffer1">>
    },
    Buffer2 = #{
        <<"store-module">> => <<"hb_store_pd">>, 
        <<"name">> => <<"buffer2">>
    },
    {ok, _} = start(Buffer1),
    {ok, _} = start(Buffer2),
    ok = write(Buffer1, <<"key1">>, <<"value1">>),
    ok = write(Buffer2, <<"key2">>, <<"value2">>),
    ?assertEqual({ok, <<"value1">>}, read(Buffer1, <<"key1">>)),
    ?assertEqual(not_found, read(Buffer1, <<"key2">>)),
    ?assertEqual({ok, <<"value2">>}, read(Buffer2, <<"key2">>)),
    ?assertEqual(not_found, read(Buffer2, <<"key1">>)),
    ok = reset(Buffer1),
    ok = reset(Buffer2).
