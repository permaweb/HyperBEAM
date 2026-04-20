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
            ChildrenTable = ets:new(hb_store_volatile_children, [
                bag,
                public,
                {read_concurrency, true},
                {write_concurrency, true}
            ]),
            Parent ! {
                ok,
                #{
                    <<"pid">> => self(),
                    <<"ets-table">> => Table,
                    <<"ets-children-table">> => ChildrenTable
                }
            },
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

%% @doc Scope for this store backend.
scope() -> local.
scope(_) -> scope().

%% @doc Remove all entries from the ETS table.
reset(Opts) ->
    #{
        <<"ets-table">> := Table,
        <<"ets-children-table">> := ChildrenTable
    } = hb_store:find(Opts),
    ets:delete_all_objects(Table),
    ets:delete_all_objects(ChildrenTable),
    ?event(store_volatile, {reset, {table, Table}}),
    ok.

%% @doc Write a value at the key path.
write(Opts, RawKey, Value) ->
    Key = hb_store:join(RawKey),
    #{
        <<"ets-table">> := Table,
        <<"ets-children-table">> := ChildrenTable
    } = hb_store:find(Opts),
    ?event(store_volatile, {write, {key, Key}}),
    case lookup_entry(Table, Key) of
        nil ->
            ensure_parent_groups(Table, ChildrenTable, Key);
        {group, true} ->
            delete_group_children(Table, ChildrenTable, Key);
        _ ->
            ok
    end,
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
            ?event(store_volatile, {hit, {key, Key}}),
            {ok, Value};
        {link, Link} ->
            ?event(store_volatile, {hit, {key, Key}}),
            read_resolved(Opts, hb_store:join(Link), Depth + 1);
        _ ->
            ?event(store_volatile, {miss, {key, Key}}),
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
        {group, true} ->
            {ok, list_group_children(Opts, ResolvedPath)};
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
        {group, true} ->
            composite;
        {link, Link} ->
            type(Opts, Link);
        _ ->
            not_found
    end.

%% @doc Ensure a group exists at the given path.
make_group(Opts, RawKey) ->
    Key = hb_store:join(RawKey),
    #{
        <<"ets-table">> := Table,
        <<"ets-children-table">> := ChildrenTable
    } = hb_store:find(Opts),
    ensure_dir(Table, ChildrenTable, Key),
    ok.

%% @doc Create or replace a link from New to Existing.
make_link(_, Link, Link) ->
    ok;
make_link(Opts, RawExisting, RawNew) ->
    Existing = hb_store:join(RawExisting),
    New = hb_store:join(RawNew),
    #{
        <<"ets-table">> := Table,
        <<"ets-children-table">> := ChildrenTable
    } = hb_store:find(Opts),
    case lookup_entry(Table, New) of
        nil ->
            ensure_parent_groups(Table, ChildrenTable, New);
        {group, true} ->
            delete_group_children(Table, ChildrenTable, New);
        _ ->
            ok
    end,
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

list_group_children(Opts, GroupPath) ->
    #{ <<"ets-children-table">> := ChildrenTable } = hb_store:find(Opts),
    [Child || {_, Child} <- ets:lookup(ChildrenTable, GroupPath)].

delete_group_children(Table, ChildrenTable, GroupPath) ->
    lists:foreach(
        fun({_, Child}) ->
            delete_tree(
                Table,
                ChildrenTable,
                next_group_path(GroupPath, Child)
            )
        end,
        ets:lookup(ChildrenTable, GroupPath)
    ),
    ets:delete(ChildrenTable, GroupPath).

delete_tree(Table, ChildrenTable, Path) ->
    case lookup_entry(Table, Path) of
        {group, true} ->
            delete_group_children(Table, ChildrenTable, Path);
        _ ->
            ok
    end,
    ets:delete(Table, Path).

ensure_parent_groups(Table, ChildrenTable, Key) ->
    case filename:dirname(Key) of
        <<".">> ->
            add_group_child(Table, ChildrenTable, ?ROOT_GROUP, filename:basename(Key));
        ParentDir ->
            ensure_dir(Table, ChildrenTable, ParentDir),
            add_group_child(Table, ChildrenTable, ParentDir, filename:basename(Key))
    end.

ensure_dir(Table, ChildrenTable, Path) ->
    PathParts = hb_path:term_to_path_parts(Path),
    ensure_dir(Table, ChildrenTable, ?ROOT_GROUP, PathParts).

ensure_dir(_Table, _ChildrenTable, _CurrentGroup, []) ->
    ok;
ensure_dir(Table, ChildrenTable, CurrentGroup, [Next | Rest]) ->
    add_group_child(Table, ChildrenTable, CurrentGroup, Next),
    NextGroup = next_group_path(CurrentGroup, Next),
    ensure_group(Table, NextGroup),
    ensure_dir(Table, ChildrenTable, NextGroup, Rest).

next_group_path(?ROOT_GROUP, Next) ->
    hb_store:join(Next);
next_group_path(CurrentGroup, Next) ->
    hb_store:join([CurrentGroup, Next]).

ensure_group(Table, GroupPath) ->
    case lookup_entry(Table, GroupPath) of
        {group, true} ->
            ok;
        _ ->
            ets:insert(Table, {GroupPath, {group, true}})
    end.

add_group_child(Table, ChildrenTable, GroupPath, Child) ->
    ensure_group(Table, GroupPath),
    ets:insert(ChildrenTable, {GroupPath, Child}),
    ok.

%%% Tests

max_ttl_test() ->
    StoreOpts =
        #{
            <<"store-module">> => ?MODULE,
            <<"name">> => <<"ets-max-ttl-test">>,
            <<"max-ttl-ms">> => 100
        },
    hb_store:start(StoreOpts),
    hb_store:write(StoreOpts, <<"a">>, <<"b">>),
    ?assertEqual({ok, <<"b">>}, hb_store:read(StoreOpts, <<"a">>)),
    timer:sleep(200),
    ?assertEqual(not_found, hb_store:read(StoreOpts, <<"a">>)),
    hb_store:write(StoreOpts, <<"a">>, <<"c">>),
    ?assertEqual({ok, <<"c">>}, hb_store:read(StoreOpts, <<"a">>)),
    timer:sleep(200),
    ?assertEqual(not_found, hb_store:read(StoreOpts, <<"a">>)),
    hb_store:stop(StoreOpts).

list_test() ->
    StoreOpts = hb_test_utils:test_store(?MODULE, <<"ets-list-test">>),
    hb_store:reset(StoreOpts),
    ?assertEqual(not_found, hb_store:list(StoreOpts, <<"colors">>)),
    ok = hb_store:make_group(StoreOpts, <<"colors">>),
    ?assertEqual({ok, []}, hb_store:list(StoreOpts, <<"colors">>)),
    ok = hb_store:write(StoreOpts, <<"colors/red">>, <<"1">>),
    ok = hb_store:write(StoreOpts, <<"colors/blue">>, <<"2">>),
    ok = hb_store:write(StoreOpts, <<"colors/green">>, <<"3">>),
    ok = hb_store:write(StoreOpts, <<"colors/multi/foo">>, <<"4">>),
    ok = hb_store:write(StoreOpts, <<"colors/multi/bar">>, <<"5">>),
    ok = hb_store:write(StoreOpts, <<"colors/primary/red">>, <<"6">>),
    ok = hb_store:write(StoreOpts, <<"colors/nested/deep/value">>, <<"7">>),
    {ok, Colors} = hb_store:list(StoreOpts, <<"colors">>),
    ?assertEqual(
        [<<"blue">>, <<"green">>, <<"multi">>, <<"nested">>, <<"primary">>, <<"red">>],
        lists:sort(Colors)
    ),
    {ok, Multi} = hb_store:list(StoreOpts, <<"colors/multi">>),
    ?assertEqual([<<"bar">>, <<"foo">>], lists:sort(Multi)),
    {ok, Nested} = hb_store:list(StoreOpts, <<"colors/nested">>),
    ?assertEqual([<<"deep">>], Nested),
    ok = hb_store:stop(StoreOpts).

list_dedup_test() ->
    StoreOpts = hb_test_utils:test_store(?MODULE, <<"ets-list-dedup-test">>),
    hb_store:reset(StoreOpts),
    ok = hb_store:write(StoreOpts, <<"colors/red">>, <<"1">>),
    ok = hb_store:write(StoreOpts, <<"colors/red">>, <<"2">>),
    ok = hb_store:make_link(StoreOpts, <<"colors/red">>, <<"colors/alias">>),
    ok = hb_store:make_link(StoreOpts, <<"colors/red">>, <<"colors/alias">>),
    {ok, Colors} = hb_store:list(StoreOpts, <<"colors">>),
    ?assertEqual([<<"alias">>, <<"red">>], lists:sort(Colors)),
    ok = hb_store:stop(StoreOpts).

list_with_link_test() ->
    StoreOpts = hb_test_utils:test_store(?MODULE, <<"ets-list-link-test">>),
    hb_store:reset(StoreOpts),
    ok = hb_store:write(StoreOpts, <<"target/one">>, <<"1">>),
    ok = hb_store:write(StoreOpts, <<"target/two">>, <<"2">>),
    ok = hb_store:make_link(StoreOpts, <<"target">>, <<"shortcut">>),
    {ok, Shortcut} = hb_store:list(StoreOpts, <<"shortcut">>),
    ?assertEqual([<<"one">>, <<"two">>], lists:sort(Shortcut)),
    ok = hb_store:stop(StoreOpts).

overwrite_link_to_raw_test() ->
    StoreOpts = hb_test_utils:test_store(?MODULE, <<"ets-overwrite-link-test">>),
    hb_store:reset(StoreOpts),
    ok = hb_store:write(StoreOpts, <<"target/one">>, <<"1">>),
    ok = hb_store:make_link(StoreOpts, <<"target">>, <<"shortcut">>),
    ok = hb_store:write(StoreOpts, <<"shortcut">>, <<"replacement">>),
    ?assertEqual({ok, <<"replacement">>}, hb_store:read(StoreOpts, <<"shortcut">>)),
    ?assertEqual(not_found, hb_store:list(StoreOpts, <<"shortcut">>)),
    {ok, Target} = hb_store:list(StoreOpts, <<"target">>),
    ?assertEqual([<<"one">>], Target),
    ok = hb_store:stop(StoreOpts).

overwrite_group_to_raw_test() ->
    StoreOpts = hb_test_utils:test_store(?MODULE, <<"ets-overwrite-group-test">>),
    hb_store:reset(StoreOpts),
    ok = hb_store:make_group(StoreOpts, <<"colors">>),
    ok = hb_store:write(StoreOpts, <<"colors/red">>, <<"1">>),
    ok = hb_store:write(StoreOpts, <<"colors/blue">>, <<"2">>),
    ok = hb_store:write(StoreOpts, <<"colors">>, <<"replacement">>),
    ?assertEqual({ok, <<"replacement">>}, hb_store:read(StoreOpts, <<"colors">>)),
    ?assertEqual(not_found, hb_store:read(StoreOpts, <<"colors/red">>)),
    ?assertEqual(not_found, hb_store:read(StoreOpts, <<"colors/blue">>)),
    #{ <<"ets-children-table">> := CT } = hb_store:find(StoreOpts),
    ?assertEqual([], ets:lookup(CT, <<"colors">>)),
    ok = hb_store:make_group(StoreOpts, <<"colors">>),
    ?assertEqual({ok, []}, hb_store:list(StoreOpts, <<"colors">>)),
    ok = hb_store:stop(StoreOpts).

overwrite_group_to_link_test() ->
    StoreOpts = hb_test_utils:test_store(?MODULE, <<"ets-overwrite-g2l-test">>),
    hb_store:reset(StoreOpts),
    ok = hb_store:make_group(StoreOpts, <<"colors">>),
    ok = hb_store:write(StoreOpts, <<"colors/red">>, <<"1">>),
    ok = hb_store:write(StoreOpts, <<"colors/blue">>, <<"2">>),
    ok = hb_store:write(StoreOpts, <<"target/val">>, <<"42">>),
    ok = hb_store:make_link(StoreOpts, <<"target">>, <<"colors">>),
    ?assertEqual(not_found, hb_store:read(StoreOpts, <<"colors/red">>)),
    ?assertEqual(not_found, hb_store:read(StoreOpts, <<"colors/blue">>)),
    #{ <<"ets-children-table">> := CT } = hb_store:find(StoreOpts),
    ?assertEqual([], ets:lookup(CT, <<"colors">>)),
    {ok, Children} = hb_store:list(StoreOpts, <<"colors">>),
    ?assertEqual([<<"val">>], Children),
    ok = hb_store:stop(StoreOpts).
