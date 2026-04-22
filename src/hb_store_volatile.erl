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
-export([start/1, start/3, stop/1, stop/3, reset/1, reset/3, scope/0, scope/1]).
-export([write/3, read/3, list/3, type/3, link/3, group/3, resolve/3]).
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
start(StoreOpts, _Req, _Opts) ->
    start(StoreOpts).

%% @doc Owner loop for the ETS store. Simply waits for a stop message and exits.
%% Until the store is stopped, the table will remain alive.
owner_loop(StoreOpts) ->
    receive
        {stop, From, Ref} ->
            From ! {ok, Ref},
            exit(normal);
        reset ->
            reset_store(StoreOpts),
            maybe_start_ttl_timer(StoreOpts, self()),
            owner_loop(StoreOpts);
        _ ->
            owner_loop(StoreOpts)
    end.

maybe_start_ttl_timer(StoreOpts, PID) ->
    case maps:get(<<"max-ttl-ms">>, StoreOpts, undefined) of
        undefined ->
            case maps:get(<<"max-ttl">>, StoreOpts, infinity) of
                infinity -> skip;
                MaxTTL ->
                    timer:send_after(hb_util:int(MaxTTL) * 1000, PID, reset)
            end;
        MaxTTLMs ->
            timer:send_after(hb_util:int(MaxTTLMs), PID, reset)
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
stop(Opts, _Req, _NodeOpts) ->
    stop(Opts).

%% @doc Scope for this store backend.
scope() -> local.
scope(_) -> scope().

%% @doc Remove all entries from the ETS table.
reset_store(Opts) ->
    #{ <<"ets-table">> := Table } = hb_store:find(Opts),
    ets:delete_all_objects(Table),
    ?event(store_volatile, {reset, {table, Table}}),
    ok.
reset(Opts) ->
    reset_store(Opts).
reset(Opts, _Req, _NodeOpts) ->
    reset_store(Opts).

%% @doc Write a value at the key path.
write(Opts, Req, _NodeOpts) when is_map(Req) ->
    maps:fold(
        fun(Key, Value, ok) ->
            write_path(Opts, Key, Value);
           (_Key, _Value, Error) ->
            Error
        end,
        ok,
        Req
    ).
write_path(Opts, RawKey, Value) ->
    Key = hb_path:to_binary(RawKey),
    #{ <<"ets-table">> := Table } = hb_store:find(Opts),
    ensure_parent_groups(Table, Key),
    ?event(store_volatile, {write, {key, Key}}),
    ets:insert(Table, {Key, {raw, Value}}),
    ok.

%% @doc Read a value, following links when needed.
read(Opts, #{ <<"read">> := RawKey }, _NodeOpts) ->
    read_path(Opts, RawKey).
read_path(Opts, RawKey) ->
    read_resolved(Opts, resolve_path(Opts, RawKey), 0).

read_resolved(_Opts, _Key, Depth) when Depth > ?MAX_REDIRECTS ->
    {error, not_found};
read_resolved(Opts, Key, Depth) ->
    case lookup_entry(Opts, Key) of
        {raw, Value} ->
            ?event(store_volatile, {hit, {key, Key}}),
            {ok, Value};
        {group, Set} ->
            ?event(store_volatile, {hit, {key, Key}}),
            {composite, sets:to_list(Set)};
        {link, Link} ->
            ?event(store_volatile, {hit, {key, Key}}),
            read_resolved(Opts, hb_path:to_binary(Link), Depth + 1);
        _ ->
            ?event(store_volatile, {miss, {key, Key}}),
            {error, not_found}
    end.

%% @doc Resolve links through a path segment-by-segment.
resolve(Opts, #{ <<"resolve">> := Key }, _NodeOpts) ->
    {ok, resolve_path(Opts, Key)}.
resolve_path(Opts, Key) ->
    resolve_path(Opts, <<>>, hb_path:term_to_path_parts(hb_path:to_binary(Key), Opts), 0).

resolve_path(_Opts, CurrPath, [], _Depth) ->
    hb_path:to_binary(CurrPath);
resolve_path(_Opts, CurrPath, _Rest, Depth) when Depth > ?MAX_REDIRECTS ->
    hb_path:to_binary(CurrPath);
resolve_path(Opts, CurrPath, [Next | Rest], Depth) ->
    PathPart = join_path(CurrPath, Next),
    case lookup_entry(Opts, PathPart) of
        {link, Link} ->
            resolve_path(Opts, hb_path:to_binary(Link), Rest, Depth + 1);
        _ ->
            resolve_path(Opts, PathPart, Rest, Depth)
    end.

%% @doc List child names under a group path.
list(Opts, #{ <<"list">> := Path }, _NodeOpts) ->
    list_path(Opts, Path).
list_path(Opts, <<"">>) ->
    list_path(Opts, ?ROOT_GROUP);
list_path(Opts, <<"/">>) ->
    list_path(Opts, ?ROOT_GROUP);
list_path(Opts, Path) ->
    ResolvedPath = resolve_path(Opts, Path),
    case lookup_entry(Opts, ResolvedPath) of
        {group, Set} ->
            {ok, sets:to_list(Set)};
        {link, Link} ->
            list_path(Opts, Link);
        {raw, Value} when is_map(Value) ->
            {ok, maps:keys(Value)};
        {raw, Value} when is_list(Value) ->
            {ok, Value};
        _ ->
            {error, not_found}
    end.

%% @doc Determine the item type at a path.
type(Opts, #{ <<"type">> := RawKey }, _NodeOpts) ->
    type_path(Opts, RawKey).
type_path(Opts, RawKey) ->
    Key = resolve_path(Opts, RawKey),
    case lookup_entry(Opts, Key) of
        {raw, _} ->
            {ok, simple};
        {group, _} ->
            {ok, composite};
        {link, Link} ->
            type_path(Opts, Link);
        _ ->
            {error, not_found}
    end.

%% @doc Ensure a group exists at the given path.
group(Opts, #{ <<"group">> := RawKey }, _NodeOpts) ->
    Key = hb_path:to_binary(RawKey),
    #{ <<"ets-table">> := Table } = hb_store:find(Opts),
    ensure_dir(Table, Key),
    ok.

%% @doc Create or replace a link from New to Existing.
link(Opts, Req, _NodeOpts) when is_map(Req) ->
    maps:fold(
        fun(LinkPath, ExistingPath, ok) ->
            link_path(Opts, LinkPath, ExistingPath);
           (_LinkPath, _ExistingPath, Error) ->
            Error
        end,
        ok,
        Req
    ).
link_path(_, LinkPath, LinkPath) ->
    ok;
link_path(Opts, RawNew, RawExisting) ->
    Existing = hb_path:to_binary(RawExisting),
    New = hb_path:to_binary(RawNew),
    #{ <<"ets-table">> := Table } = hb_store:find(Opts),
    ensure_parent_groups(Table, New),
    ets:insert(Table, {New, {link, Existing}}),
    ok.

join_path(<<>>, Next) ->
    hb_path:to_binary(Next);
join_path(CurrPath, Next) ->
    hb_path:to_binary([CurrPath, Next]).

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
    hb_path:to_binary(Next);
next_group_path(CurrentGroup, Next) ->
    hb_path:to_binary([CurrentGroup, Next]).

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
            <<"max-ttl-ms">> => 100
        },
    ok = hb_store:start(StoreOpts),
    ok = hb_store:write(StoreOpts, #{ <<"a">> => <<"b">> }, #{}),
    ?assertEqual({ok, <<"b">>}, hb_store:read(StoreOpts, <<"a">>, #{})),
    timer:sleep(200),
    ?assertEqual({error, not_found}, hb_store:read(StoreOpts, <<"a">>, #{})),
    ok = hb_store:write(StoreOpts, #{ <<"a">> => <<"c">> }, #{}),
    ?assertEqual({ok, <<"c">>}, hb_store:read(StoreOpts, <<"a">>, #{})),
    timer:sleep(200),
    ?assertEqual({error, not_found}, hb_store:read(StoreOpts, <<"a">>, #{})),
    ok = hb_store:stop(StoreOpts).
