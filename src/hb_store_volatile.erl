%%% @doc A lightweight in-memory HyperBEAM store backed by ETS. The store is
%%% volatile: It does not persist data to disk ever, and -- critically -- can
%%% be configured to expire all data periodically. This is useful for testing
%%% and as a short-term in-memory cache, not for instances where an `ok` from
%%% the `write` function should imply data persistence.
%%%
%%% This store keeps all data in-memory and does not flush to any persistent
%%% backend. It supports the core `hb_store` interface semantics used by
%%% `hb_store` and `hb_cache`: writes, reads, groups, links, type checks,
%%% path resolution, and resets.
-module(hb_store_volatile).
-export([start/1, stop/1, reset/1, scope/0, scope/1]).
-export([write/3, read/2, list/2, type/2, make_link/3, make_group/2, resolve/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ROOT_GROUP, <<"/">>).
-define(MAX_REDIRECTS, 32).

%% @doc Start the ETS-backed store and return the store instance message.
start(StoreOpts = #{ <<"name">> := Name }) ->
    ?event(cache_ets, {starting_ets_store, Name}),
    Parent = self(),
    spawn(
        fun() ->
            Table = ets:new(hb_store_volatile, [
                set,
                public,
                {read_concurrency, true},
                {write_concurrency, true}
            ]),
            Parent ! {ok, #{ <<"pid">> => self(), <<"ets-table">> => Table }},
            maybe_start_ttl_timer(StoreOpts, self()),
            owner_loop(StoreOpts)
        end
    ),
    receive
        {ok, InstanceMessage} ->
            {ok, InstanceMessage}
    end.

%% @doc Owner loop for the ETS store. Simply waits for a stop message and exits.
%% Until the store is stopped, the table will remain alive.
owner_loop(StoreOpts) ->
    receive
        {stop, From, Ref} ->
            From ! {ok, Ref},
            exit(normal);
        reset ->
            reset(StoreOpts),
            maybe_start_ttl_timer(StoreOpts, self()),
            owner_loop(StoreOpts);
        _ ->
            owner_loop(StoreOpts)
    end.

maybe_start_ttl_timer(StoreOpts, PID) ->
    case maps:get(<<"max-ttl">>, StoreOpts, infinity) of
        infinity -> skip;
        MaxTTL -> timer:send_after(hb_util:int(MaxTTL) * 1000, PID, reset)
    end.

%% @doc Stop the ETS owner process (which also drops the table).
stop(Opts) ->
    #{ <<"pid">> := Pid } = hb_store:find(Opts),
    Pid ! {stop, self(), Ref = make_ref()},
    receive
        {ok, Ref} -> ok
    after 5000 ->
        ok
    end.

%% @doc Scope for this store backend.
scope() -> local.
scope(_) -> scope().

%% @doc Remove all entries from the ETS table.
reset(Opts) ->
    #{ <<"ets-table">> := Table } = hb_store:find(Opts),
    ets:delete_all_objects(Table),
    ok.

%% @doc Write a value at the key path.
write(Opts, RawKey, Value) ->
    Key = hb_store:join(RawKey),
    #{ <<"ets-table">> := Table } = hb_store:find(Opts),
    ensure_parent_groups(Table, Key),
    ets:insert(Table, {Key, {raw, Value}}),
    ok.

%% @doc Read a value, following links when needed.
read(Opts, RawKey) ->
    read_resolved(Opts, resolve(Opts, RawKey), 0).

read_resolved(_Opts, _Key, Depth) when Depth > ?MAX_REDIRECTS ->
    not_found;
read_resolved(Opts, Key, Depth) ->
    case lookup_entry(Opts, Key) of
        {raw, Value} ->
            {ok, Value};
        {link, Link} ->
            read_resolved(Opts, hb_store:join(Link), Depth + 1);
        _ ->
            not_found
    end.

%% @doc Resolve links through a path segment-by-segment.
resolve(Opts, Key) ->
    resolve(Opts, <<>>, hb_path:term_to_path_parts(hb_store:join(Key), Opts), 0).

resolve(_Opts, CurrPath, [], _Depth) ->
    hb_store:join(CurrPath);
resolve(_Opts, CurrPath, _Rest, Depth) when Depth > ?MAX_REDIRECTS ->
    hb_store:join(CurrPath);
resolve(Opts, CurrPath, [Next | Rest], Depth) ->
    PathPart = join_path(CurrPath, Next),
    case lookup_entry(Opts, PathPart) of
        {link, Link} ->
            resolve(Opts, hb_store:join(Link), Rest, Depth + 1);
        _ ->
            resolve(Opts, PathPart, Rest, Depth)
    end.

%% @doc List child names under a group path.
list(Opts, <<"">>) ->
    list(Opts, ?ROOT_GROUP);
list(Opts, <<"/">>) ->
    list(Opts, ?ROOT_GROUP);
list(Opts, Path) ->
    ResolvedPath = resolve(Opts, Path),
    case lookup_entry(Opts, ResolvedPath) of
        {group, Set} ->
            {ok, sets:to_list(Set)};
        {link, Link} ->
            list(Opts, Link);
        {raw, Value} when is_map(Value) ->
            {ok, maps:keys(Value)};
        {raw, Value} when is_list(Value) ->
            {ok, Value};
        _ ->
            not_found
    end.

%% @doc Determine the item type at a path.
type(Opts, RawKey) ->
    Key = resolve(Opts, RawKey),
    case lookup_entry(Opts, Key) of
        {raw, _} ->
            simple;
        {group, _} ->
            composite;
        {link, Link} ->
            type(Opts, Link);
        _ ->
            not_found
    end.

%% @doc Ensure a group exists at the given path.
make_group(Opts, RawKey) ->
    Key = hb_store:join(RawKey),
    #{ <<"ets-table">> := Table } = hb_store:find(Opts),
    ensure_dir(Table, Key),
    ok.

%% @doc Create or replace a link from New to Existing.
make_link(_, Link, Link) ->
    ok;
make_link(Opts, RawExisting, RawNew) ->
    Existing = hb_store:join(RawExisting),
    New = hb_store:join(RawNew),
    #{ <<"ets-table">> := Table } = hb_store:find(Opts),
    ensure_parent_groups(Table, New),
    ets:insert(Table, {New, {link, Existing}}),
    ok.

join_path(<<>>, Next) ->
    hb_store:join(Next);
join_path(CurrPath, Next) ->
    hb_store:join([CurrPath, Next]).

lookup_entry(Opts, Key) when is_map(Opts) ->
    #{ <<"ets-table">> := Table } = hb_store:find(Opts),
    lookup_entry(Table, Key);
lookup_entry(Table, Key) ->
    case ets:lookup(Table, Key) of
        [] ->
            nil;
        [{_, Entry}] ->
            Entry
    end.

ensure_parent_groups(Table, Key) ->
    case filename:dirname(Key) of
        <<".">> ->
            add_group_child(Table, ?ROOT_GROUP, filename:basename(Key));
        ParentDir ->
            ensure_dir(Table, ParentDir),
            add_group_child(Table, ParentDir, filename:basename(Key))
    end.

ensure_dir(Table, Path) ->
    PathParts = hb_path:term_to_path_parts(Path),
    ensure_dir(Table, ?ROOT_GROUP, PathParts).

ensure_dir(_Table, _CurrentGroup, []) ->
    ok;
ensure_dir(Table, CurrentGroup, [Next | Rest]) ->
    add_group_child(Table, CurrentGroup, Next),
    NextGroup = next_group_path(CurrentGroup, Next),
    ensure_group(Table, NextGroup),
    ensure_dir(Table, NextGroup, Rest).

next_group_path(?ROOT_GROUP, Next) ->
    hb_store:join(Next);
next_group_path(CurrentGroup, Next) ->
    hb_store:join([CurrentGroup, Next]).

ensure_group(Table, GroupPath) ->
    case lookup_entry(Table, GroupPath) of
        {group, _} ->
            ok;
        _ ->
            ets:insert(Table, {GroupPath, {group, sets:new()}})
    end.

add_group_child(Table, GroupPath, Child) ->
    Set =
        case lookup_entry(Table, GroupPath) of
            {group, ExistingSet} ->
                ExistingSet;
            _ ->
                sets:new()
        end,
    ets:insert(Table, {GroupPath, {group, sets:add_element(Child, Set)}}),
    ok.

%%% Tests

max_ttl_test() ->
    StoreOpts =
        #{
            <<"store-module">> => ?MODULE,
            <<"name">> => <<"ets-max-ttl-test">>,
            <<"max-ttl">> => 1
        },
    hb_store:start(StoreOpts),
    hb_store:write(StoreOpts, <<"a">>, <<"b">>),
    ?assertEqual({ok, <<"b">>}, hb_store:read(StoreOpts, <<"a">>)),
    timer:sleep(1250),
    ?assertEqual(not_found, hb_store:read(StoreOpts, <<"a">>)),
    hb_store:stop(StoreOpts).