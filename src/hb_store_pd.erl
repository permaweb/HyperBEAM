-module(hb_store_pd).

%% Standard hb_store exports
-export([start/1, stop/1, reset/1, scope/0, scope/1]).
-export([read/2, write/3, list/2, match/2]).
-export([make_group/2, make_link/3, type/2]).
-export([path/2, add_path/3, resolve/2]).
-export([flush/2, flush/3, buffer_size/1, keys/1]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(MAX_REDIRECTS, 1000). 

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

%% @doc Return the scope of this store (local = only this process).
-spec scope() -> local.
scope() -> local.

%% @doc Return the scope of this store (ignores parameters).
-spec scope(term()) -> local.
scope(_) -> scope().

%% @doc Generate the process dictionary key for a given store name.
-spec get_state_key(binary()) -> {?MODULE, binary()}.
get_state_key(Name) ->
    {?MODULE, Name}.

%% @doc Get the data map from the process dictionary.
get_data(Name) ->
    StateKey = get_state_key(Name),
    case erlang:get(StateKey) of
        undefined -> #{};
        Data -> Data
    end.

%% @doc Put the data map into the process dictionary.
put_data(Name, Data) ->
    StateKey = get_state_key(Name),
    erlang:put(StateKey, Data).

%% @doc Convert path parts to a binary path.
to_path(PathParts) ->
    hb_util:bin(lists:join(<<"/">>, PathParts)).

%% @doc Check if a value is a symbolic link (format: "link:<target>").
%% Returns {true, Target} or false.
is_link(Value) when is_binary(Value) ->
    case binary:split(Value, <<":">>) of
        [<<"link">>, Target] -> {true, Target};
        _ -> false
    end;
is_link(_) ->
    false.

%% @doc Write a key-value pair to the store.
%% Accepts path as binary or list of segments.
-spec write(map(), binary() | list(), binary()) -> ok.
write(Opts, PathParts, Value) when is_list(PathParts) ->
    write(Opts, to_path(PathParts), Value);
write(#{<<"name">> := Name}, Key, Value) ->
    Data = get_data(Name),
    put_data(Name, Data#{Key => Value}),
    ok.

%% @doc Read a value directly without following any links.
%% This is the lowest-level read operation.
read_direct(#{<<"name">> := Name}, Key) ->
    Data = get_data(Name),
    case maps:find(Key, Data) of
        {ok, Value} -> {ok, Value};
        error -> not_found
    end.

%% @doc Read a value and follow links in the value itself (recursive).
%% Returns not_found for "group" marker values.
read_with_links(Opts, Path) ->
    case read_direct(Opts, Path) of
        {ok, <<"group">>} ->
            not_found;
        {ok, Value} ->
            case is_link(Value) of
                {true, Target} -> read_with_links(Opts, Target);
                false -> {ok, Value}
            end;
        not_found ->
            not_found
    end.

%% @doc Resolve links in a path by checking each segment (except the last).
resolve_path_links(Opts, PathParts) ->
    resolve_path_links(Opts, PathParts, 0).

resolve_path_links(_Opts, _Path, Depth) when Depth > ?MAX_REDIRECTS ->
    {error, too_many_redirects};
resolve_path_links(_Opts, [LastSegment], _Depth) ->
    {ok, [LastSegment]};
resolve_path_links(Opts, PathParts, Depth) ->
    resolve_segments(Opts, PathParts, [], Depth).

%% Walk through path segments, resolving links as we go.
resolve_segments(_Opts, [], ResolvedAcc, _Depth) ->
    {ok, lists:reverse(ResolvedAcc)};
resolve_segments(Opts, [Segment | Rest], ResolvedAcc, Depth) ->
    CurrentPath = to_path(lists:reverse([Segment | ResolvedAcc])),
    case read_direct(Opts, CurrentPath) of
        {ok, Value} ->
            case is_link(Value) of
                {true, Target} ->
                    TargetParts = binary:split(Target, <<"/">>, [global]),
                    resolve_path_links(Opts, TargetParts ++ Rest, Depth + 1);
                false ->
                    resolve_segments(Opts, Rest, [Segment | ResolvedAcc], Depth)
            end;
        not_found ->
            resolve_segments(Opts, Rest, [Segment | ResolvedAcc], Depth)
    end.

%% @doc Read a value with full link resolution (both in path and value).
-spec read(map(), binary() | list()) -> {ok, binary()} | not_found.
read(Opts, PathParts) when is_list(PathParts) ->
    read(Opts, to_path(PathParts));
read(Opts, Path) ->
    case read_with_links(Opts, Path) of
        {ok, Value} ->
            {ok, Value};
        not_found ->
            PathParts = binary:split(Path, <<"/">>, [global]),
            case resolve_path_links(Opts, PathParts) of
                {ok, ResolvedParts} ->
                    ResolvedPath = to_path(ResolvedParts),
                    read_with_links(Opts, ResolvedPath);
                {error, _} ->
                    not_found
            end
    end.

%% @doc List immediate children of a path (like "ls" in a directory).
-spec list(map(), binary()) -> {ok, [binary()]} | not_found.
list(Opts, Path) ->
    ResolvedPath = case read_direct(Opts, Path) of
        {ok, Value} ->
            case is_link(Value) of
                {true, Target} -> Target;
                false -> Path
            end;
        not_found ->
            Path
    end,
    Prefix = normalize_prefix(ResolvedPath),
    #{<<"name">> := Name} = Opts,
    Data = get_data(Name),
    Children = find_children(Prefix, maps:keys(Data)),

    case Children of
        [] ->
            case is_empty_group(Opts, Path) of
                true -> {ok, []};
                false -> not_found
            end;
        _ ->
            {ok, lists:usort(Children)}
    end.

%% Normalize a path to a prefix for matching (ensure trailing slash).
normalize_prefix(<<>>) -> <<>>;
normalize_prefix(<<"/">>) -> <<>>;
normalize_prefix(Path) ->
    case binary:last(Path) of
        $/ -> Path;
        _ -> <<Path/binary, "/">>
    end.

%% Find immediate children under a prefix.
find_children(Prefix, Keys) ->
    PrefixLen = byte_size(Prefix),
    lists:filtermap(
        fun(Key) ->
            extract_child_name(Key, Prefix, PrefixLen)
        end,
        Keys
    ).

%% Extract the immediate child name from a key, if it matches the prefix.
extract_child_name(Key, Prefix, PrefixLen) ->
    KeyLen = byte_size(Key),
    if
        PrefixLen =:= 0, KeyLen > 0 ->
            case binary:split(Key, <<"/">>) of
                [Child | _] -> {true, Child};
                _ -> false
            end;

        KeyLen > PrefixLen ->
            case Key of
                <<Prefix:PrefixLen/binary, Rest/binary>> ->
                    case binary:split(Rest, <<"/">>) of
                        [Child | _] when Child =/= <<>> -> {true, Child};
                        _ -> false
                    end;
                _ ->
                    false
            end;
        true ->
            false
    end.

%% Check if a path points to an empty group (group marker with no children).
is_empty_group(Opts, Path) ->
    case read_direct(Opts, Path) of
        {ok, <<"group">>} -> true;
        _ -> false
    end.

%% @doc Find all keys where child paths match the given patterns.

-spec match(map(), map() | list()) -> {ok, [binary()]} | not_found.
match(Opts, MatchMap) when is_map(MatchMap) ->
    match(Opts, maps:to_list(MatchMap));
match(#{<<"name">> := Name}, Patterns) ->
    LinkPatterns = [{Key, <<"link:", Path/binary>>} || {Key, Path} <- Patterns],

    Data = get_data(Name),
    Matches = [K || K <- maps:keys(Data), matches_all_patterns(K, LinkPatterns, Data)],

    case Matches of
        [] -> not_found;
        _ -> {ok, Matches}
    end.

%% Check if a key matches all the given patterns.
matches_all_patterns(Key, Patterns, Data) ->
    lists:all(
        fun({PatternKey, Expected}) ->
            FullKey = <<Key/binary, "/", PatternKey/binary>>,
            maps:get(FullKey, Data, undefined) =:= Expected
        end,
        Patterns
    ).

%% @doc Determine whether a key is a simple value or composite group.
%% Follows links to determine the type of the target.
-spec type(map(), binary()) -> composite | simple | not_found.
type(Opts, Key) ->
    case read_direct(Opts, Key) of
        {ok, <<"group">>} -> composite;
        {ok, Value} ->
            case is_link(Value) of
                {true, Target} -> type(Opts, Target);  
                false -> simple
            end;
        not_found ->
            not_found
    end.

%% @doc Resolve a path by following any symbolic links in the path segments.
%% Returns the fully resolved path as a binary.
-spec resolve(map(), binary() | list()) -> binary().
resolve(Opts, Path) when is_binary(Path) ->
    resolve(Opts, binary:split(Path, <<"/">>, [global]));
resolve(Opts, PathParts) when is_list(PathParts) ->
    case resolve_path_links(Opts, PathParts) of
        {ok, ResolvedParts} -> to_path(ResolvedParts);
        {error, _} -> to_path(PathParts)  
    end.

%% @doc Convert a path to the store's canonical form (binary).
-spec path(map(), binary() | [binary()]) -> binary().
path(_Opts, PathParts) when is_list(PathParts) ->
    to_path(PathParts);
path(_Opts, Path) when is_binary(Path) ->
    Path.

%% @doc Join two paths together.
-spec add_path(map(), binary() | list(), binary() | list()) -> binary().
add_path(_Opts, Path1, Path2) when is_list(Path1), is_list(Path2) ->
    to_path(Path1 ++ Path2);
add_path(Opts, Path1, Path2) when is_binary(Path1) ->
    Parts1 = binary:split(Path1, <<"/">>, [global]),
    Parts2 = ensure_path_parts(Path2),
    add_path(Opts, Parts1, Parts2);
add_path(Opts, Path1, Path2) when is_binary(Path2) ->
    Parts1 = ensure_path_parts(Path1),
    Parts2 = binary:split(Path2, <<"/">>, [global]),
    add_path(Opts, Parts1, Parts2).

ensure_path_parts(Path) when is_list(Path) -> Path;
ensure_path_parts(Path) when is_binary(Path) -> binary:split(Path, <<"/">>, [global]).

%% @doc Create a group marker (indicates a key can have children).
-spec make_group(map(), binary()) -> ok | {error, term()}.
make_group(Opts, GroupName) when is_binary(GroupName) ->
    write(Opts, GroupName, <<"group">>);
make_group(_, _) ->
    {error, {badarg, <<"GroupName must be a binary">>}}.

%% @doc Create a symbolic link from source to target.
%% Format: writes "link:<target>" at the source key.
-spec make_link(map(), binary() | list(), binary()) -> ok.
make_link(Opts, Target, LinkName) when is_list(Target) ->
    make_link(Opts, to_path(Target), LinkName);
make_link(Opts, Target, LinkName) ->
    TargetBin = hb_util:bin(Target),
    write(Opts, LinkName, <<"link:", TargetBin/binary>>).

%% @doc Flush all buffered data to a target store.
-spec flush(map(), map()) -> {ok, map()}.
flush(BufferOpts, TargetOpts) ->
    flush(BufferOpts, TargetOpts, #{}).

%% @doc Flush with options controlling clear behavior.
-spec flush(map(), map(), map()) -> {ok, map()}.
flush(#{<<"name">> := Name}, TargetOpts, FlushOpts) ->
    Data = get_data(Name),
    case map_size(Data) of
        0 ->
            ?event(store_pd, {flush_empty, Name}),
            {ok, #{written => 0, failed => []}};
        Size ->
            ?event(store_pd, {flush_start, Name, {keys, Size}}),

            Results = write_all_to_target(Data, TargetOpts),

            case should_clear_buffer(Results, FlushOpts) of
                true ->
                    erlang:erase(get_state_key(Name)),
                    ?event(store_pd, {flush_complete_cleared, Name, Results});
                false ->
                    ?event(store_pd, {flush_complete_kept, Name, Results})
            end,

            {ok, Results}
    end.

%% Write all key-value pairs to the target store.
write_all_to_target(Data, TargetOpts) ->
    maps:fold(
        fun(Key, Value, #{written := W, failed := F} = Acc) ->
            case hb_store:write(TargetOpts, Key, Value) of
                ok ->
                    Acc#{written => W + 1};
                Error ->
                    ?event(error, {store_pd_flush_failed, {key, Key}, {error, Error}}),
                    Acc#{failed => [{Key, Error} | F]}
            end
        end,
        #{written => 0, failed => []},
        Data
    ).

%% Determine if buffer should be cleared based on flush results.
should_clear_buffer(#{failed := []}, FlushOpts) ->
    maps:get(<<"clear-on-flush">>, FlushOpts, true);  
should_clear_buffer(#{failed := [_|_]}, FlushOpts) ->
    maps:get(<<"clear-on-error">>, FlushOpts, false).  

%% @doc Get the number of keys currently in the buffer.
-spec buffer_size(map()) -> non_neg_integer().
buffer_size(#{<<"name">> := Name}) ->
    map_size(get_data(Name)).

%% @doc Get all keys currently in the buffer.
-spec keys(map()) -> [binary()].
keys(#{<<"name">> := Name}) ->
    maps:keys(get_data(Name)).
    
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
    ?assertEqual(not_found, list(Store, <<>>)),
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
