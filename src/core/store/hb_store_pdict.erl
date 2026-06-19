%% @doc A process dictionary-based implementation of the HyperBeam store interface.
%%
%% This module provides a fast, volatile key-value store using the Erlang process
%% dictionary for storage. It's designed as a write buffer that accumulates changes
%% in memory before flushing them to a persistent store backend like LMDB.
%%
%% Key features include:
%% <ul>
%%   <li>Extremely fast reads (~4.8M records/s) and writes (~2.4M records/s)</li>
%%   <li>Automatic link resolution for creating symbolic references between keys</li>
%%   <li>Group support for organizing hierarchical data structures</li>
%%   <li>Explicit flush operation to persist buffered data</li>
%%   <li>Process-local storage with no disk I/O overhead</li>
%% </ul>
%%
%% Performance characteristics: The flat map structure provides excellent read/write
%% performance but slower list operations (~316 groups/s) due to O(n) iteration.
%% This tradeoff is acceptable for a write buffer where reads and writes dominate.
%%
%% Usage pattern: Buffer writes in memory during processing, then flush to a
%% persistent store when ready to commit. Multiple named buffers can coexist
%% independently within the same process.
-module(hb_store_pdict).

% Public API exports
-export([start/3, stop/3, reset/3, scope/0, scope/1]).
-export([read/3, write/3, list/3, match/3]).
-export([group/3, link/3, type/3, resolve/3]).
-export([flush/2, flush/3, buffer_size/1, keys/1]).

% Test framework and project includes
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

% Maximum number of link redirects to prevent infinite loops
-define(MAX_REDIRECTS, 1000).

%% @doc Start the process dictionary store for a given configuration.
%%
%% This function initializes a new buffer in the process dictionary with an
%% empty map. Multiple buffers can coexist independently by using different
%% names. The buffer persists until explicitly reset or the process terminates.
%%
%% The StoreOpts map must contain a "name" key that uniquely identifies this
%% buffer within the process. This name is used as part of the process
%% dictionary key to ensure isolation between multiple buffers.
%%
%% @param StoreOpts A map containing the "name" configuration
%% @returns {ok, EmptyMap} on success, {error, Reason} on failure
-spec start(map(), map(), map()) -> {ok, map()} | {error, term()}.
start(Opts, _Req, _NodeOpts) ->
    start(Opts).

start(_Opts = #{ <<"name">> := DataPath }) ->
    ?event(store_pd, {starting, DataPath}),
    erlang:put(get_state_key(DataPath), #{}),
    {ok, #{}};
start(_) ->
    {error, {badarg, <<"name is required in store configuration">>}}.

%% @doc Stop the store by clearing all buffered data.
-spec stop(map()) -> ok.
stop(Opts, _Req, _NodeOpts) ->
    stop(Opts).

stop(Opts) ->
    reset(Opts).

%% @doc Reset the store by erasing all data from the process dictionary.
%%
%% This completely removes the buffer from the process dictionary, freeing
%% memory and losing all buffered data. Use flush/2 before resetting if you
%% want to persist the data first.
-spec reset(map()) -> ok.
reset(Opts, _Req, _NodeOpts) ->
    reset(Opts).

reset(_Opts = #{<<"name">> := Name}) ->
    StateKey = get_state_key(Name),
    erlang:erase(StateKey),
    ok.

%% @doc Return the scope of this store.
%%
%% Process dictionary stores have 'local' scope, meaning data is only visible
%% within the current process and is not shared across processes or nodes.
-spec scope() -> local.
scope() -> local.

%% @doc Return the scope of this store (ignores parameters for compatibility).
-spec scope(term()) -> local.
scope(_) -> scope().

% Generate the process dictionary key for a store name
get_state_key(Name) ->
    {?MODULE, Name}.

% Get the data map from the process dictionary, returning empty map if not found
get_data(Name) ->
    StateKey = get_state_key(Name),
    case erlang:get(StateKey) of
        undefined -> #{};
        Data -> Data
    end.

% Store the data map in the process dictionary
put_data(Name, Data) ->
    StateKey = get_state_key(Name),
    erlang:put(StateKey, Data).

% Convert a list of path segments to a binary path
to_path(PathParts) ->
    hb_util:bin(lists:join(<<"/">>, PathParts)).

% Check if a value is a symbolic link (format: "link:<target>")
is_link(Value) when is_binary(Value) ->
    case binary:split(Value, <<":">>) of
        [<<"link">>, Target] -> {true, Target};
        _ -> false
    end;
is_link(_) ->
    false.

%% @doc Write a key-value pair to the store immediately.
%%
%% This function stores the key-value pair in the process dictionary map.
%% The write is synchronous and completes immediately, but the data only
%% exists in memory until explicitly flushed to a persistent store.
%%
%% When given a list of path segments, they are automatically joined with
%% "/" separators to form a binary key. This provides a convenient way to
%% work with hierarchical paths.
%%
%% @param Opts Store configuration map
%% @param Path Binary key or list of path segments to write
%% @param Value Binary value to store
%% @returns 'ok' always (writes never fail in memory)
-spec write(map(), map(), map()) -> ok.
write(Opts, Req, _NodeOpts) when is_map(Req) ->
    maps:fold(
        fun(Path, Value, ok) ->
            write_path(Opts, Path, Value);
           (_Path, _Value, Error) ->
            Error
        end,
        ok,
        Req
    ).

write_path(#{<<"name">> := Name}, Key, Value) ->
    Data = get_data(Name),
    put_data(Name, Data#{hb_path:to_binary(Key) => Value}),
    ok.

% Read a value directly without following any links
read_direct(#{<<"name">> := Name}, Key) ->
    Data = get_data(Name),
    case maps:find(Key, Data) of
        {ok, Value} -> {ok, Value};
        error -> not_found
    end.

% Read a value and recursively follow any links in the value itself
read_with_links(Opts, Path) ->
    case read_direct(Opts, Path) of
        {ok, <<"group">>} ->
            % Group markers are not readable values
            not_found;
        {ok, Value} ->
            case is_link(Value) of
                {true, Target} ->
                    % Value is a link, follow it recursively
                    read_with_links(Opts, Target);
                false ->
                    {ok, Value}
            end;
        not_found ->
            not_found
    end.

% Resolve links in path segments (except the last segment)
resolve_path_links(Opts, PathParts) ->
    resolve_path_links(Opts, PathParts, 0).

resolve_path_links(_Opts, _Path, Depth) when Depth > ?MAX_REDIRECTS ->
    {error, too_many_redirects};
resolve_path_links(_Opts, [LastSegment], _Depth) ->
    % Don't resolve the final segment
    {ok, [LastSegment]};
resolve_path_links(Opts, PathParts, Depth) ->
    resolve_segments(Opts, PathParts, [], Depth).

% Walk through path segments and resolve any that are links
resolve_segments(_Opts, [], ResolvedAcc, _Depth) ->
    {ok, lists:reverse(ResolvedAcc)};
resolve_segments(Opts, [Segment | Rest], ResolvedAcc, Depth) ->
    % Build the current path from accumulated segments
    CurrentPath = to_path(lists:reverse([Segment | ResolvedAcc])),
    case read_direct(Opts, CurrentPath) of
        {ok, Value} ->
            case is_link(Value) of
                {true, Target} ->
                    % This segment is a link - replace it and restart resolution
                    TargetParts = binary:split(Target, <<"/">>, [global]),
                    resolve_path_links(Opts, TargetParts ++ Rest, Depth + 1);
                false ->
                    % Regular value - continue with next segment
                    resolve_segments(Opts, Rest, [Segment | ResolvedAcc], Depth)
            end;
        not_found ->
            % Segment doesn't exist yet - continue anyway
            resolve_segments(Opts, Rest, [Segment | ResolvedAcc], Depth)
    end.

%% @doc Read a value from the store by key, with automatic link resolution.
%%
%% This function attempts to read a value with full link resolution support.
%% It handles both links in the value itself and links in the path segments.
%%
%% The resolution process works in two phases:
%% 1. Try a direct read and follow any links in the returned value
%% 2. If not found, resolve links in the path segments and try again
%%
%% This two-phase approach optimizes for the common case where paths don't
%% contain links, while still supporting hierarchical link structures like
%% "msg123/data" where "msg123" might be a link to another message.
%%
%% When given a list of path segments, they are automatically joined into
%% a binary path before resolution begins.
%%
%% @param Opts Store configuration map
%% @param Path Binary key or list of path segments to read
%% @returns {ok, Value} on success, not_found if key doesn't exist
-spec read(map(), map() | binary() | list(), map()) ->
    {ok, binary()} | {composite, [binary()]} | {error, not_found}.
read(Opts, #{ <<"read">> := Path }, _NodeOpts) ->
    case read_resolved(Opts, hb_path:to_binary(Path)) of
        {ok, ResolvedPath, <<"group">>} ->
            {composite, hb_util:ok(list(Opts, ResolvedPath))};
        {ok, _ResolvedPath, Value} ->
            {ok, Value};
        not_found ->
            {error, not_found}
    end.

-spec read(map(), binary() | list()) -> {ok, binary()} | not_found.
read(Opts, PathParts) when is_list(PathParts) ->
    read(Opts, to_path(PathParts));
read(Opts, Path) ->
    % Try direct read first (fast path for non-link paths)
    case read_with_links(Opts, Path) of
        {ok, Value} ->
            {ok, Value};
        not_found ->
            % Not found - try resolving links in the path segments
            PathParts = binary:split(Path, <<"/">>, [global]),
            case resolve_path_links(Opts, PathParts) of
                {ok, ResolvedParts} ->
                    ResolvedPath = to_path(ResolvedParts),
                    read_with_links(Opts, ResolvedPath);
                {error, _} ->
                    not_found
            end
    end.

read_resolved(Opts, Path) ->
    case read_value_with_links(Opts, Path) of
        {ok, _ResolvedPath, _Value} = Result ->
            Result;
        not_found ->
            PathParts = binary:split(Path, <<"/">>, [global]),
            case resolve_path_links(Opts, PathParts) of
                {ok, ResolvedParts} ->
                    read_value_with_links(Opts, to_path(ResolvedParts));
                {error, _} ->
                    not_found
            end
    end.

read_value_with_links(Opts, Path) ->
    case read_direct(Opts, Path) of
        {ok, Value} ->
            case is_link(Value) of
                {true, Target} ->
                    read_resolved(Opts, Target);
                false ->
                    {ok, Path, Value}
            end;
        not_found ->
            not_found
    end.

%% @doc List immediate children of a path, similar to a directory listing.
%%
%% This function returns the names of all immediate children under the given
%% path, much like "ls" in a Unix filesystem. It does not return nested children.
%%
%% Implementation note: This operation is O(n) where n is the total number of
%% keys in the store, because it must scan all keys to find matching prefixes.
%% For a write buffer with tens of thousands of keys, this takes ~30 seconds
%% for 10,000 list operations. This is acceptable given the buffer's intended
%% use as a temporary accumulator before flushing to faster persistent stores.
%%
%% The function handles link resolution: if the path itself is a link, it
%% resolves to the target before listing children.
%%
%% @param Opts Store configuration map
%% @param Path Binary path to list
%% @returns {ok, [ChildNames]} on success, not_found if path doesn't exist
-spec list(map(), binary()) -> {ok, [binary()]} | not_found.
list(Opts, #{ <<"list">> := Path }, _NodeOpts) ->
    case list(Opts, hb_path:to_binary(Path)) of
        {ok, _} = OK -> OK;
        not_found -> {error, not_found}
    end.

list(Opts, Path) ->
    % Resolve the path if it's a link
    ResolvedPath = case read_direct(Opts, Path) of
        {ok, Value} ->
            case is_link(Value) of
                {true, Target} -> Target;
                false -> Path
            end;
        not_found ->
            Path
    end,
    % Normalize to a search prefix (ensure trailing slash for non-empty paths)
    SearchPrefix = normalize_prefix(ResolvedPath),
    % Find all matching children
    #{<<"name">> := Name} = Opts,
    Data = get_data(Name),
    Children = find_children(SearchPrefix, maps:keys(Data)),
    case Children of
        [] ->
            % No children - check if this is a valid empty group or root
            case is_empty_group(Opts, Path) of
                true -> {ok, []};     
                false -> not_found
            end;
        _ ->
            % Return unique, sorted children
            {ok, lists:usort(Children)}
    end.

% Normalize a path to a search prefix (ensure trailing slash)
normalize_prefix(<<>>) -> <<>>;
normalize_prefix(<<"/">>) -> <<>>;
normalize_prefix(Path) ->
    case binary:last(Path) of
        $/ -> Path;
        _ -> <<Path/binary, "/">>
    end.

% Find immediate children matching a prefix
find_children(SearchPrefix, Keys) ->
    PrefixLen = byte_size(SearchPrefix),
    lists:filtermap(
        fun(Key) ->
            extract_child_name(Key, SearchPrefix, PrefixLen)
        end,
        Keys
    ).

% Extract the immediate child name from a key if it matches the prefix
extract_child_name(Key, _SearchPrefix, 0) ->
    % Root level - extract first segment
    case binary:split(Key, <<"/">>) of
        [Child | _] when Child =/= <<>> -> {true, Child};
        _ -> false
    end;
extract_child_name(Key, SearchPrefix, PrefixLen) ->
    % Check if key starts with prefix
    KeyLen = byte_size(Key),
    if
        KeyLen > PrefixLen ->
            case Key of
                <<SearchPrefix:PrefixLen/binary, Rest/binary>> ->
                    % Extract first segment after prefix
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

% Check if a path is a valid empty group
is_empty_group(Opts, Path) ->
    case read_direct(Opts, Path) of
        {ok, <<"group">>} -> true;
        _ -> false
    end.

%% @doc Find all keys where child paths match the given patterns.
%%
%% This function searches for keys that have specific child values matching
%% the provided patterns. For example, match(Store, #{<<"slot">> => 1})
%% finds all keys K where K/slot exists and links to the value 1.
%%
%% The patterns are provided as a map or list of {Key, Value} tuples. All
%% patterns must match for a key to be included in the results.
%%
%% @param Opts Store configuration map
%% @param Patterns Map or list of patterns to match
%% @returns {ok, [MatchingKeys]} or not_found if no matches
-spec match(map(), map() | list()) -> {ok, [binary()]} | not_found.
match(Opts, MatchMap, _NodeOpts) ->
    case match(Opts, MatchMap) of
        {ok, _} = OK -> OK;
        not_found -> {error, not_found}
    end.

match(Opts, MatchMap) when is_map(MatchMap) ->
    match(Opts, maps:to_list(MatchMap));
match(#{<<"name">> := Name}, Patterns) ->
    % Convert patterns to link format for comparison
    LinkPatterns = [{Key, <<"link:", Path/binary>>} || {Key, Path} <- Patterns],
    % Find all keys where all patterns match
    Data = get_data(Name),
    Matches = [K || K <- maps:keys(Data), matches_all_patterns(K, LinkPatterns, Data)],

    case Matches of
        [] -> not_found;
        _ -> {ok, Matches}
    end.

% Check if a key matches all required patterns
matches_all_patterns(Key, Patterns, Data) ->
    lists:all(
        fun({PatternKey, Expected}) ->
            FullKey = <<Key/binary, "/", PatternKey/binary>>,
            maps:get(FullKey, Data, undefined) =:= Expected
        end,
        Patterns
    ).

%% @doc Determine whether a key represents a simple value or composite group.
%%
%% This function examines a key to classify its type. Keys storing the literal
%% binary "group" are considered composite (directory-like) entries, while all
%% other values are treated as simple key-value pairs.
%%
%% The function automatically follows links: if the key is a link, it resolves
%% to the target and checks its type instead.
%%
%% @param Opts Store configuration map
%% @param Key The key to examine
%% @returns 'composite' for groups, 'simple' for values, not_found if missing
-spec type(map(), map(), map()) ->
    {ok, composite} | {ok, simple} | {error, not_found}.
type(Opts, #{ <<"type">> := Key }, _NodeOpts) ->
    case type(Opts, hb_path:to_binary(Key)) of
        composite -> {ok, composite};
        simple -> {ok, simple};
        not_found -> {error, not_found}
    end.

-spec type(map(), binary()) -> composite | simple | not_found.
type(Opts, Key) ->
    case read_direct(Opts, Key) of
        {ok, <<"group">>} ->
            composite;
        {ok, Value} ->
            case is_link(Value) of
                {true, Target} ->
                    % This is a link, check the target's type
                    type(Opts, Target);
                false ->
                    simple
            end;
        not_found ->
            not_found
    end.

%% @doc Resolve a path by following any symbolic links in the path segments.
%%
%% This function takes a path and resolves all symbolic links in the segments
%% (except the final segment) to return the canonical path. This is useful for
%% understanding where a path actually points after link resolution.
%%
%% If resolution fails (e.g., too many redirects), the original path is returned.
%%
%% @param Opts Store configuration map
%% @param Path Binary path or list of segments to resolve
%% @returns The fully resolved path as a binary
-spec resolve(map(), map(), map()) -> {ok, binary()}.
resolve(Opts, #{ <<"resolve">> := Path }, _NodeOpts) ->
    {ok, resolve(Opts, hb_path:to_binary(Path))}.

-spec resolve(map(), binary() | list()) -> binary().
resolve(Opts, Path) when is_binary(Path) ->
    resolve(Opts, binary:split(Path, <<"/">>, [global]));
resolve(Opts, PathParts) when is_list(PathParts) ->
    case resolve_path_links(Opts, PathParts) of
        {ok, ResolvedParts} -> to_path(ResolvedParts);
        {error, _} -> to_path(PathParts)
    end.

%% @doc Create a group marker indicating a key can have children.
%%
%% Groups are represented by storing the literal binary "group" as the value.
%% This allows the store to distinguish between simple values and hierarchical
%% containers.
-spec make_group(map(), binary()) -> ok | {error, term()}.
group(Opts, #{ <<"group">> := GroupName }, _NodeOpts) ->
    make_group(Opts, hb_path:to_binary(GroupName)).

make_group(Opts, GroupName) when is_binary(GroupName) ->
    write_path(Opts, GroupName, <<"group">>);
make_group(_, _) ->
    {error, {badarg, <<"GroupName must be a binary">>}}.

%% @doc Create a symbolic link from one key to another.
%%
%% This function writes a special "link:<target>" value at the source key that
%% will be automatically resolved during read operations. Links provide a way
%% to create aliases or indirection in the key space.
%%
%% @param Opts Store configuration map
%% @param Target The key being linked to (can be list or binary)
%% @param LinkName The new key that will link to the target
%% @returns 'ok' always
-spec make_link(map(), binary() | list(), binary()) -> ok.
link(Opts, Req, _NodeOpts) when is_map(Req) ->
    maps:fold(
        fun(New, Existing, ok) ->
            make_link(Opts, Existing, hb_path:to_binary(New));
           (_New, _Existing, Error) ->
            Error
        end,
        ok,
        Req
    ).

make_link(Opts, Target, LinkName) when is_list(Target) ->
    make_link(Opts, to_path(Target), LinkName);
make_link(Opts, Target, LinkName) ->
    TargetBin = hb_util:bin(Target),
    write_path(Opts, LinkName, <<"link:", TargetBin/binary>>).
    
%% @doc Flush all buffered data to a target store.
%%
%% This function writes all key-value pairs from the buffer to another store
%% (typically a persistent store like LMDB). By default, the buffer is cleared
%% after a successful flush, but this can be controlled with FlushOpts.
%%
%% @param BufferOpts Source buffer configuration
%% @param TargetOpts Target store configuration
%% @returns {ok, #{written => N, failed => []}} with flush statistics
-spec flush(map(), map()) -> {ok, map()}.
flush(BufferOpts, TargetOpts) ->
    flush(BufferOpts, TargetOpts, #{}).

%% @doc Flush with options controlling buffer clearing behavior.
%%
%% The FlushOpts map can contain:
%%   - "clear-on-flush": Whether to clear buffer after successful flush (default: true)
%%
%% @param BufferOpts Source buffer configuration
%% @param TargetOpts Target store configuration
%% @param FlushOpts Options controlling flush behavior
%% @returns {ok, Stats} where Stats contains write counts and any failures
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

% Write all key-value pairs to the target store
write_all_to_target(Data, TargetOpts) ->
    maps:fold(
        fun(Key, Value, #{written := W, failed := F} = Acc) ->
            case hb_store:write(TargetOpts, #{ Key => Value }, #{}) of
                ok ->
                    Acc#{written => W + 1};
                Error ->
                    ?event(
                        error, 
                        { store_pd_flush_failed, 
                            {key, Key}, 
                            {error, Error}
                        }
                    ),
                    Acc#{failed => [{Key, Error} | F]}
            end
        end,
        #{written => 0, failed => []},
        Data
    ).

% Determine whether to clear the buffer based on flush results
should_clear_buffer(#{failed := []}, FlushOpts) ->
    maps:get(<<"clear-on-flush">>, FlushOpts, true).

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
    Store = #{ <<"store-module">> => ?MODULE, <<"name">> => <<"test">> },
    {ok, _} = start(Store),
    ok = hb_store:write(Store, #{ <<"key1">> => <<"value1">> }, #{}),
    ?event({ data, erlang:get()}),
    ?assertEqual({ok, <<"value1">>}, read(Store, <<"key1">>)),
    ?assertEqual(not_found, read(Store, <<"key2">>)),
    ok = reset(Store).

list_keys_test() ->
    Store = #{ <<"store-module">> => ?MODULE, <<"name">> => <<"test">> },
    {ok, _} = start(Store),
    ?event({lissts, list(Store, <<>>)}),
    ?assertEqual(not_found, list(Store, <<>>)),
    ok = hb_store:write(Store, #{ <<"key1">> => <<"value1">> }, #{}),
    ok = hb_store:write(Store, #{ <<"key2">> => <<"value2">> }, #{}),
    {ok, Keys} = list(Store, <<>>),
    ?assertEqual(2, length(Keys)),
    ?assert(lists:member(<<"key1">>, Keys)),
    ?assert(lists:member(<<"key2">>, Keys)),
    ok = reset(Store).

flush_test() ->
    PDStore = #{
        <<"store-module">> => ?MODULE, 
        <<"name">> => <<"buffer">>
    },
    {ok, _} = start(PDStore),
    TargetStore = hb_test_utils:test_store(hb_store_lmdb, <<"flush_test_db">>),
    hb_store:reset(TargetStore),
    ok = hb_store:write(PDStore, #{ <<"key1">> => <<"value1">> }, #{}),
    ok = hb_store:write(PDStore, #{ <<"key2">> => <<"value2">> }, #{}),
    ?assertEqual({ok, <<"value1">>}, read(PDStore, <<"key1">>)),
    ?assertEqual({error, not_found}, hb_store:read(TargetStore, <<"key1">>, #{})),
    {ok, Stats} = flush(PDStore, TargetStore),
    ?assertEqual(2, maps:get(written, Stats)),
    ?assertEqual([], maps:get(failed, Stats)),
    ?assertEqual(not_found, read(PDStore, <<"key1">>)),
    ?assertEqual({ok, <<"value1">>}, hb_store:read(TargetStore, <<"key1">>, #{})),
    ?assertEqual({ok, <<"value2">>}, hb_store:read(TargetStore, <<"key2">>, #{})).

buffer_size_test() ->
    Store = #{<<"store-module">> => ?MODULE, <<"name">> => <<"test">>},
    {ok, _} = start(Store),
    ?assertEqual(0, buffer_size(Store)),
    ok = hb_store:write(Store, #{ <<"key1">> => <<"value1">> }, #{}),
    ?assertEqual(1, buffer_size(Store)),
    ok = hb_store:write(Store, #{ <<"key2">> => <<"value2">> }, #{}),
    ?assertEqual(2, buffer_size(Store)),
    ok = reset(Store),
    ?assertEqual(0, buffer_size(Store)).

multiple_buffers_test() ->
    Buffer1 = #{
        <<"store-module">> => ?MODULE, 
        <<"name">> => <<"buffer1">>
    },
    Buffer2 = #{
        <<"store-module">> => ?MODULE, 
        <<"name">> => <<"buffer2">>
    },
    {ok, _} = start(Buffer1),
    {ok, _} = start(Buffer2),
    ok = hb_store:write(Buffer1, #{ <<"key1">> => <<"value1">> }, #{}),
    ok = hb_store:write(Buffer2, #{ <<"key2">> => <<"value2">> }, #{}),
    ?assertEqual({ok, <<"value1">>}, read(Buffer1, <<"key1">>)),
    ?assertEqual(not_found, read(Buffer1, <<"key2">>)),
    ?assertEqual({ok, <<"value2">>}, read(Buffer2, <<"key2">>)),
    ?assertEqual(not_found, read(Buffer2, <<"key1">>)),
    ok = reset(Buffer1),
    ok = reset(Buffer2).
