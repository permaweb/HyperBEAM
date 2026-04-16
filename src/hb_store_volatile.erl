%%% @doc A lightweight in-memory HyperBEAM store backed by ETS.
%%%
%%% Two modes:
%%% - No TTL: single ETS table, data persists until stop/reset.
%%% - `max-ttl': Double-buffer table flip. Two ETS tables; writes go
%%%   to both. Reads check the "new" table first, fall back to "old"
%%%   with promote-on-read for raw/link entries. Every TTL/2 the old
%%%   table is wiped and roles flip. Active data survives via promote;
%%%   idle data expires atomically — no partial messages, no dangling
%%%   links. Groups are never promoted directly; they are recreated in
%%%   the new table as a side-effect of child promotes (via
%%%   ensure_parent_groups). `list' unions group sets from both tables
%%%   so children are always visible while either table holds them.
-module(hb_store_volatile).
-export([start/1, stop/1, reset/1, scope/0, scope/1]).
-export([write/3, read/2, list/2, type/2, make_link/3, make_group/2, resolve/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ROOT_GROUP, <<"/">>).
-define(MAX_REDIRECTS, 32).

%%%===================================================================
%%% Store lifecycle
%%%===================================================================

%% @doc Create a new public ETS table with concurrency opts.
new_table() ->
    ets:new(hb_store_volatile, [
        set, public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]).

%% @doc Start the ETS-backed store. With max-ttl, creates two tables
%% for double-buffer flip. Without, creates a single table.
start(StoreOpts = #{<<"name">> := Name}) ->
    ?event(cache_ets, {starting_ets_store, Name}),
    Parent = self(),
    spawn(
        fun() ->
            T1 = new_table(),
            case maps:get(<<"max-ttl">>, StoreOpts, infinity) of
                infinity ->
                    Parent ! {ok, #{
                        <<"pid">> => self(),
                        <<"ets-table">> => T1
                    }},
                    owner_loop(StoreOpts);
                _TTL ->
                    T2 = new_table(),
                    Flip = atomics:new(1, []),
                    atomics:put(Flip, 1, 0),
                    Parent ! {ok, #{
                        <<"pid">> => self(),
                        <<"ets-table">> => T1,
                        <<"ets-table-2">> => T2,
                        <<"ets-flip">> => Flip
                    }},
                    OwnerOpts = StoreOpts#{
                        <<"ets-table">> => T1,
                        <<"ets-table-2">> => T2,
                        <<"ets-flip">> => Flip
                    },
                    maybe_start_flip_timer(OwnerOpts, self()),
                    owner_loop(OwnerOpts)
            end
        end
    ),
    receive
        {ok, InstanceMessage} ->
            {ok, InstanceMessage}
    end.

%% @doc Owner loop. Handles stop, manual reset, and table flips.
owner_loop(OwnerOpts) ->
    receive
        {stop, From, Ref} ->
            From ! {ok, Ref},
            exit(normal);
        reset ->
            do_reset(OwnerOpts),
            owner_loop(OwnerOpts);
        table_flip ->
            do_flip(OwnerOpts),
            maybe_start_flip_timer(OwnerOpts, self()),
            owner_loop(OwnerOpts);
        _ ->
            owner_loop(OwnerOpts)
    end.

%% @doc Schedule the next table flip at TTL/2 interval.
maybe_start_flip_timer(OwnerOpts, PID) ->
    case maps:get(<<"max-ttl">>, OwnerOpts, infinity) of
        infinity -> skip;
        TTL ->
            TTLMs = hb_util:int(TTL) * 1000,
            Interval = max(500, TTLMs div 2),
            timer:send_after(Interval, PID, table_flip)
    end.

%% @doc Wipe the old table and flip roles.
do_flip(OwnerOpts) ->
    Flip = maps:get(<<"ets-flip">>, OwnerOpts),
    T1 = maps:get(<<"ets-table">>, OwnerOpts),
    T2 = maps:get(<<"ets-table-2">>, OwnerOpts),
    OldTable =
        case atomics:get(Flip, 1) of
            0 -> T2;
            1 -> T1
        end,
    ets:delete_all_objects(OldTable),
    OldVal = atomics:get(Flip, 1),
    atomics:put(Flip, 1, 1 - OldVal),
    ?event(store_volatile, {table_flip, {wiped, OldTable}}).

%% @doc Wipe all tables (used by manual reset).
do_reset(OwnerOpts) ->
    T1 = maps:get(<<"ets-table">>, OwnerOpts),
    ets:delete_all_objects(T1),
    case maps:get(<<"ets-table-2">>, OwnerOpts, undefined) of
        undefined -> ok;
        T2 -> ets:delete_all_objects(T2)
    end,
    ?event(store_volatile, {reset, OwnerOpts}).

%% @doc Stop the ETS owner process (drops both tables).
stop(Opts) ->
    #{<<"pid">> := Pid} = hb_store:find(Opts),
    Pid ! {stop, self(), Ref = make_ref()},
    receive
        {ok, Ref} -> ok
    after 5000 ->
        ok
    end.

scope() -> local.
scope(_) -> scope().

%% @doc Public reset — wipes all tables.
reset(Opts) ->
    Found = hb_store:find(Opts),
    T1 = maps:get(<<"ets-table">>, Found),
    ets:delete_all_objects(T1),
    case maps:get(<<"ets-table-2">>, Found, undefined) of
        undefined -> ok;
        T2 -> ets:delete_all_objects(T2)
    end,
    ?event(store_volatile, {reset, {table, T1}}),
    ok.

%%%===================================================================
%%% Table helpers
%%%===================================================================

%% @doc Get {NewTable, OldTable}. Returns {T, undefined} when no TTL.
get_tables(Opts) ->
    Found = hb_store:find(Opts),
    case maps:get(<<"ets-flip">>, Found, undefined) of
        undefined ->
            {maps:get(<<"ets-table">>, Found), undefined};
        Flip ->
            T1 = maps:get(<<"ets-table">>, Found),
            T2 = maps:get(<<"ets-table-2">>, Found),
            case atomics:get(Flip, 1) of
                0 -> {T1, T2};
                1 -> {T2, T1}
            end
    end.

%% @doc Extract the group set from a table, or empty set.
group_set(Table, Key) ->
    case ets:lookup(Table, Key) of
        [{_, {group, Set}}] -> Set;
        _ -> sets:new()
    end.

%%%===================================================================
%%% Write operations — always write to both tables
%%%===================================================================

%% @doc Write a value at the key path.
write(Opts, RawKey, Value) ->
    Key = hb_store:join(RawKey),
    {New, Old} = get_tables(Opts),
    ensure_parent_groups(New, Key),
    ?event(store_volatile, {write, {key, Key}}),
    ets:insert(New, {Key, {raw, Value}}),
    case Old of
        undefined -> ok;
        _ ->
            ensure_parent_groups(Old, Key),
            ets:insert(Old, {Key, {raw, Value}})
    end,
    ok.

%% @doc Create or replace a link from New to Existing.
make_link(_, Link, Link) ->
    ok;
make_link(Opts, RawExisting, RawNew) ->
    Existing = hb_store:join(RawExisting),
    NewPath = hb_store:join(RawNew),
    {NewT, OldT} = get_tables(Opts),
    ensure_parent_groups(NewT, NewPath),
    ets:insert(NewT, {NewPath, {link, Existing}}),
    case OldT of
        undefined -> ok;
        _ ->
            ensure_parent_groups(OldT, NewPath),
            ets:insert(OldT, {NewPath, {link, Existing}})
    end,
    ok.

%% @doc Ensure a group exists at the given path.
make_group(Opts, RawKey) ->
    Key = hb_store:join(RawKey),
    {New, Old} = get_tables(Opts),
    ensure_dir(New, Key),
    case Old of
        undefined -> ok;
        _ -> ensure_dir(Old, Key)
    end,
    ok.

%%%===================================================================
%%% Read operations — new first, fallback old with promote
%%%===================================================================

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
    resolve(
        Opts, <<>>,
        hb_path:term_to_path_parts(hb_store:join(Key), Opts),
        0
    ).

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

%% @doc Look up an entry. Single-table: direct lookup (original speed).
%% Double-buffer: checks new first, falls back to old with promote.
%% Groups are NOT promoted — they expire with their table.
lookup_entry(Opts, Key) when is_map(Opts) ->
    Found = hb_store:find(Opts),
    case Found of
        #{<<"ets-flip">> := Flip,
          <<"ets-table">> := T1,
          <<"ets-table-2">> := T2} ->
            {New, Old} =
                case atomics:get(Flip, 1) of
                    0 -> {T1, T2};
                    1 -> {T2, T1}
                end,
            case ets:lookup(New, Key) of
                [{_, Entry}] ->
                    Entry;
                [] ->
                    case ets:lookup(Old, Key) of
                        [{_, {group, _} = Entry}] ->
                            Entry;
                        [{_, Entry}] ->
                            ets:insert(New, {Key, Entry}),
                            ensure_parent_groups(New, Key),
                            Entry;
                        [] ->
                            nil
                    end
            end;
        #{<<"ets-table">> := Table} ->
            case ets:lookup(Table, Key) of
                [] -> nil;
                [{_, Entry}] -> Entry
            end
    end;
lookup_entry(Table, Key) ->
    case ets:lookup(Table, Key) of
        [] -> nil;
        [{_, Entry}] -> Entry
    end.

%%%===================================================================
%%% Query operations
%%%===================================================================

%% @doc List child names under a group path. Unions both tables.
list(Opts, Path) when Path =:= <<"">> orelse Path =:= <<"/">> ->
    list_resolved(Opts, ?ROOT_GROUP);
list(Opts, Path) ->
    list_resolved(Opts, resolve(Opts, Path)).

list_resolved(Opts, ResolvedPath) ->
    Found = hb_store:find(Opts),
    case Found of
        #{<<"ets-flip">> := Flip,
          <<"ets-table">> := T1,
          <<"ets-table-2">> := T2} ->
            {New, Old} =
                case atomics:get(Flip, 1) of
                    0 -> {T1, T2};
                    1 -> {T2, T1}
                end,
            Union = sets:union(
                group_set(New, ResolvedPath),
                group_set(Old, ResolvedPath)
            ),
            case sets:size(Union) > 0 of
                true ->
                    {ok, sets:to_list(Union)};
                false ->
                    list_fallback(Opts, ResolvedPath)
            end;
        #{<<"ets-table">> := Table} ->
            case lookup_entry(Table, ResolvedPath) of
                {group, Set} ->
                    {ok, sets:to_list(Set)};
                Other ->
                    list_from_entry(Opts, ResolvedPath, Other)
            end
    end.

list_fallback(Opts, ResolvedPath) ->
    case lookup_entry(Opts, ResolvedPath) of
        {link, Link} -> list(Opts, Link);
        Other -> list_from_entry(Opts, ResolvedPath, Other)
    end.

list_from_entry(_Opts, _Path, {raw, Value}) when is_map(Value) ->
    {ok, maps:keys(Value)};
list_from_entry(_Opts, _Path, {raw, Value}) when is_list(Value) ->
    {ok, Value};
list_from_entry(Opts, _Path, {link, Link}) ->
    list(Opts, Link);
list_from_entry(_Opts, _Path, _) ->
    not_found.

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

%%%===================================================================
%%% Path and group helpers
%%%===================================================================

join_path(<<>>, Next) ->
    hb_store:join(Next);
join_path(CurrPath, Next) ->
    hb_store:join([CurrPath, Next]).

ensure_parent_groups(Table, Key) ->
    case filename:dirname(Key) of
        <<".">> ->
            add_group_child(
                Table, ?ROOT_GROUP, filename:basename(Key));
        ParentDir ->
            ensure_dir(Table, ParentDir),
            add_group_child(
                Table, ParentDir, filename:basename(Key))
    end.

ensure_dir(Table, Path) ->
    PathParts = hb_path:term_to_path_parts(Path),
    do_ensure_dir(Table, ?ROOT_GROUP, PathParts).

do_ensure_dir(_Table, _CurrentGroup, []) ->
    ok;
do_ensure_dir(Table, CurrentGroup, [Next | Rest]) ->
    add_group_child(Table, CurrentGroup, Next),
    NextGroup = next_group_path(CurrentGroup, Next),
    ensure_group(Table, NextGroup),
    do_ensure_dir(Table, NextGroup, Rest).

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
    ets:insert(
        Table,
        {GroupPath, {group, sets:add_element(Child, Set)}}
    ),
    ok.

%%%===================================================================
%%% Tests
%%%===================================================================

%% @doc Idle data expires after TTL (two flip cycles).
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
    hb_store:write(StoreOpts, <<"a">>, <<"c">>),
    ?assertEqual({ok, <<"c">>}, hb_store:read(StoreOpts, <<"a">>)),
    timer:sleep(1250),
    ?assertEqual(not_found, hb_store:read(StoreOpts, <<"a">>)),
    hb_store:stop(StoreOpts).

%% @doc Demonstrates that a whole-table reset during a multi-step
%% cache write leaves dangling links.
ttl_corrupts_cache_structure_test() ->
    StoreOpts =
        #{
            <<"store-module">> => ?MODULE,
            <<"name">> => <<"ets-ttl-corrupt-test">>,
            <<"max-ttl">> => 1
        },
    hb_store:start(StoreOpts),
    hb_store:make_group(StoreOpts, <<"msg">>),
    hb_store:write(StoreOpts, <<"data/hash1">>, <<"value">>),
    hb_store:make_link(
        StoreOpts, <<"data/hash1">>, <<"msg/key1">>),
    ?assertEqual(
        {ok, <<"value">>},
        hb_store:read(StoreOpts, <<"msg/key1">>)
    ),
    timer:sleep(1250),
    hb_store:make_group(StoreOpts, <<"msg2">>),
    hb_store:make_link(
        StoreOpts, <<"data/hash1">>, <<"msg2/key1">>),
    ?assertEqual(
        not_found,
        hb_store:read(StoreOpts, <<"msg2/key1">>)
    ),
    ?assertEqual(
        not_found,
        hb_store:read(StoreOpts, <<"msg/key1">>)
    ),
    hb_store:stop(StoreOpts).

%% @doc Active reads promote data across flips. Write two keys,
%% read only one — it survives while the untouched key expires.
active_read_survives_test_() ->
    {timeout, 10, fun active_read_survives/0}.
active_read_survives() ->
    StoreOpts =
        #{
            <<"store-module">> => ?MODULE,
            <<"name">> => <<"ets-active-survives">>,
            <<"max-ttl">> => 1
        },
    hb_store:start(StoreOpts),
    hb_store:write(StoreOpts, <<"a">>, <<"val-a">>),
    hb_store:write(StoreOpts, <<"b">>, <<"val-b">>),
    lists:foreach(
        fun(_) ->
            timer:sleep(200),
            ?assertMatch(
                {ok, _}, hb_store:read(StoreOpts, <<"a">>))
        end,
        lists:seq(1, 7)
    ),
    ?assertEqual(not_found, hb_store:read(StoreOpts, <<"b">>)),
    ?assertEqual(
        {ok, <<"val-a">>},
        hb_store:read(StoreOpts, <<"a">>)
    ),
    {ok, Children} = hb_store:list(StoreOpts, <<"/">>),
    ?assert(lists:member(<<"a">>, Children)),
    ?assertNot(lists:member(<<"b">>, Children)),
    hb_store:stop(StoreOpts).

%% @doc Reading a deep child promotes it and recreates parent groups
%% via ensure_parent_groups on promote.
namespace_consistency_test_() ->
    {timeout, 10, fun namespace_consistency_test/0}.
namespace_consistency_test() ->
    StoreOpts =
        #{
            <<"store-module">> => ?MODULE,
            <<"name">> => <<"ets-namespace-test">>,
            <<"max-ttl">> => 1
        },
    hb_store:start(StoreOpts),
    hb_store:write(
        StoreOpts, [<<"a">>, <<"b">>, <<"c">>], <<"deep">>),
    lists:foreach(
        fun(_) ->
            timer:sleep(200),
            ?assertMatch(
                {ok, _},
                hb_store:read(
                    StoreOpts, [<<"a">>, <<"b">>, <<"c">>])
            )
        end,
        lists:seq(1, 7)
    ),
    ?assertEqual(composite, hb_store:type(StoreOpts, <<"a">>)),
    {ok, AChildren} = hb_store:list(StoreOpts, <<"a">>),
    ?assert(lists:member(<<"b">>, AChildren)),
    hb_store:stop(StoreOpts).

%% @doc hb_cache:read either returns a complete message or not_found
%% — never a group without children (no partial messages).
no_partial_message_test_() ->
    {timeout, 15, fun no_partial_message/0}.
no_partial_message() ->
    Store =
        #{
            <<"store-module">> => ?MODULE,
            <<"name">> => <<"ets-no-partial">>,
            <<"max-ttl">> => 1
        },
    hb_store:start(Store),
    Opts = #{store => [Store], priv_wallet => hb:wallet()},
    Msg =
        hb_message:commit(
            #{
                <<"data">> => <<"no-partial-test">>,
                <<"test-key">> => <<"test-value">>
            },
            Opts
        ),
    {ok, ID} = hb_cache:write(Msg, Opts),
    Errors = lists:foldl(
        fun(_, Acc) ->
            timer:sleep(200),
            case hb_cache:read(ID, Opts) of
                {ok, CachedMsg} ->
                    try
                        Resolved =
                            hb_cache:ensure_all_loaded(
                                CachedMsg, Opts
                            ),
                        case maps:is_key(<<"data">>, Resolved) of
                            true -> Acc;
                            false -> Acc + 1
                        end
                    catch _:_ ->
                        Acc + 1
                    end;
                not_found ->
                    Acc
            end
        end,
        0,
        lists:seq(1, 10)
    ),
    hb_store:stop(Store),
    ?assertEqual(0, Errors).

%% @doc Write a message, obtain lazy links via hb_cache:read, wait
%% for max-ttl to wipe the store, then resolve the lazy links.
%% The data behind the links must still be accessible.
ttl_wipe_lazy_link_test_() ->
    {timeout, 10, fun ttl_wipe_lazy_link/0}.
ttl_wipe_lazy_link() ->
    Store =
        #{
            <<"store-module">> => ?MODULE,
            <<"name">> => <<"ets-wipe-lazy-link">>,
            <<"max-ttl">> => 1
        },
    hb_store:start(Store),
    Opts = #{store => [Store], priv_wallet => hb:wallet()},
    Msg =
        hb_message:commit(
            #{
                <<"data">> => crypto:strong_rand_bytes(1024),
                <<"content-type">> =>
                    <<"application/octet-stream">>
            },
            Opts
        ),
    {ok, ID} = hb_cache:write(Msg, Opts),
    {ok, CachedMsg} = hb_cache:read(ID, Opts),
    %% Sleep 750ms, then load all fields.
    timer:sleep(750),
    hb_cache:ensure_all_loaded(CachedMsg, Opts),
    %% Sleep past the TTL boundary.
    timer:sleep(500),
    %% Resolve the lazy links again — data must still be accessible.
    {ok, Msg2} = hb_cache:read(ID, Opts),
    Resolved = hb_cache:ensure_all_loaded(Msg2, Opts),
    ?assert(maps:is_key(<<"data">>, Resolved)),
    hb_store:stop(Store).

%% @doc Full reads (ensure_all_loaded) promote all data, keeping the
%% message alive across flip cycles.
ensure_loaded_survives_test_() ->
    {timeout, 15, fun ensure_loaded_survives/0}.
ensure_loaded_survives() ->
    Store =
        #{
            <<"store-module">> => ?MODULE,
            <<"name">> => <<"ets-ensure-loaded-survives">>,
            <<"max-ttl">> => 1
        },
    hb_store:start(Store),
    Opts = #{store => [Store], priv_wallet => hb:wallet()},
    Msg =
        hb_message:commit(
            #{
                <<"data">> => crypto:strong_rand_bytes(1024),
                <<"content-type">> =>
                    <<"application/octet-stream">>
            },
            Opts
        ),
    {ok, ID} = hb_cache:write(Msg, Opts),
    Errors = lists:foldl(
        fun(_, Acc) ->
            timer:sleep(100),
            try
                case hb_cache:read(ID, Opts) of
                    {ok, CachedMsg} ->
                        Resolved =
                            hb_cache:ensure_all_loaded(
                                CachedMsg, Opts
                            ),
                        case maps:is_key(<<"data">>, Resolved) of
                            true -> Acc;
                            false -> Acc + 1
                        end;
                    not_found ->
                        Acc + 1
                end
            catch _:_ ->
                Acc + 1
            end
        end,
        0,
        lists:seq(1, 20)
    ),
    hb_store:stop(Store),
    ?assertEqual(0, Errors).

%% @doc Group with two children. Read child mid-TTL. After TTL,
%% active child survives (promoted), sibling is gone.
read_child_ttl_promotes_test() ->
    StoreOpts =
        #{
            <<"store-module">> => ?MODULE,
            <<"name">> => <<"ets-read-child-ttl">>,
            <<"max-ttl">> => 1
        },
    hb_store:start(StoreOpts),
    hb_store:write(
        StoreOpts, [<<"grp">>, <<"a">>], <<"val-a">>),
    hb_store:write(
        StoreOpts, [<<"grp">>, <<"b">>], <<"val-b">>),
    ?assertEqual(
        {ok, <<"val-a">>},
        hb_store:read(StoreOpts, [<<"grp">>, <<"a">>])
    ),
    ?assertEqual(
        {ok, <<"val-b">>},
        hb_store:read(StoreOpts, [<<"grp">>, <<"b">>])
    ),
    % Read a mid-TTL to promote it
    timer:sleep(500),
    ?assertEqual(
        {ok, <<"val-a">>},
        hb_store:read(StoreOpts, [<<"grp">>, <<"a">>])
    ),
    % Wait for full TTL expiry
    timer:sleep(750),
    % a survived (promoted), b is gone
    ?assertMatch(
        {ok, _},
        hb_store:read(StoreOpts, [<<"grp">>, <<"a">>])
    ),
    ?assertEqual(
        not_found,
        hb_store:read(StoreOpts, [<<"grp">>, <<"b">>])
    ),
    hb_store:stop(StoreOpts).

%% @doc Backward compat: store without TTL works as before.
no_ttl_backward_compat_test() ->
    StoreOpts =
        #{
            <<"store-module">> => ?MODULE,
            <<"name">> => <<"ets-no-ttl-compat">>
        },
    hb_store:start(StoreOpts),
    hb_store:write(StoreOpts, <<"x">>, <<"y">>),
    ?assertEqual(
        {ok, <<"y">>}, hb_store:read(StoreOpts, <<"x">>)),
    ?assertEqual(simple, hb_store:type(StoreOpts, <<"x">>)),
    {ok, Root} = hb_store:list(StoreOpts, <<"/">>),
    ?assert(lists:member(<<"x">>, Root)),
    hb_store:stop(StoreOpts).
