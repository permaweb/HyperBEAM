%%% @doc A writable sorted-set store backed by a single-key LMDB duplicate set.
%%%
%%% The container is the published-index format described in
%%% `docs/misc/published-arweave-indexes.md': an LMDB 1.0 environment with
%%% 64 KiB pages whose sole database is opened `dupsort|dupfixed' and holds
%%% exactly one key, `<<0>>'. Every item of the set is a duplicate value of
%%% that key, held in raw byte order by LMDB itself, so the whole set is the
%%% key's duplicate run and its leaves are the header-free `P_LEAF2' pages
%%% that published snapshots serve directly from the weave. `dupfixed' pins
%%% every item in one store to a single width — the first insert sets it.
%%%
%%% Items are opaque binaries: fixed-width layouts, hash prefixes and
%%% offsets belong to the protocol modules that produce and consume them.
%%% The store deliberately has no links, groups or path semantics — a
%%% sorted set has no hierarchy, and the format exists precisely so that
%%% reads are positioned seeks rather than walks. Since items may contain
%%% any byte (including `/'), callers must use the request-map forms of
%%% the `hb_store' verbs: the path forms normalize their argument.
%%%
%%% `write' inserts the keys of the request map as items (a set carries no
%%% values, so the map's values are ignored); `list' returns the ascending
%%% run of items sharing the `list' prefix, optionally bounded by an
%%% inclusive `from' cursor and a `limit'; `read' returns the first item
%%% matching a prefix, which is a membership test when given a full item.
%%% `append/2' writes a strictly-ascending batch in one transaction for
%%% bulk index builds.
-module(hb_store_lmdb_set).
-export([start/3, stop/3, reset/3, scope/0, scope/1]).
-export([read/3, write/3, list/3, type/3]).
-export([append/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% The single main-database key whose duplicate set carries every item.
-define(MAIN_KEY, <<0>>).
%% Configuration constants, matching `hb_store_lmdb''s defaults.
-define(DEFAULT_SIZE, 2 * 1024 * 1024 * 1024 * 1024).
-define(DEFAULT_PAGE_SIZE, 65536).

%% @doc Open (or create) the set's environment and its single database.
%% Read-only stores open the environment `MDB_RDONLY' and the database
%% without `create'. The duplicate flags are identical on every open:
%% a database keeps the mode it was created with.
start(Opts = #{ <<"name">> := DataDir }, _Req, _NodeOpts) ->
    DataDirPath = hb_util:list(DataDir),
    ok = filelib:ensure_dir(filename:join(DataDirPath, "data.mdb")),
    ReadOnly = maps:get(<<"read-only">>, Opts, false),
    EnvOpts =
        [
            {
                map_size,
                hb_util:int(maps:get(<<"capacity">>, Opts, ?DEFAULT_SIZE))
            },
            {
                page_size,
                hb_util:int(maps:get(<<"page-size">>, Opts, ?DEFAULT_PAGE_SIZE))
            },
            % Deliberately not `no_mem_init': with buffers initialized,
            % unused page regions hold zeros rather than heap garbage,
            % so an identical input builds a byte-identical file — the
            % property that lets a published snapshot be audited against
            % a local rebuild by hash.
            no_sync
        ] ++
        case ReadOnly of
            true -> [read_only, no_lock];
            false -> []
        end,
    {ok, Env} = elmdb:env_open(DataDirPath, EnvOpts),
    {ok, DB} =
        elmdb:db_open(
            Env,
            case ReadOnly of
                true -> [dupfixed];
                false -> [create, dupfixed]
            end
        ),
    {ok, #{ <<"env">> => Env, <<"db">> => DB }};
start(_Store, _Req, _NodeOpts) ->
    {error, {badarg, <<"StoreOpts must be a map">>}}.

%% @doc The set is a local store.
-spec scope() -> local.
scope() -> local.
scope(_) -> scope().

%% @doc Insert the keys of the request map as items. A set carries no
%% values, so the map's values are ignored. This is the same `Path =>
%% Value' map that `hb_cache:apply_write_ops/3' batches writes into, so
%% the store drops into a store list without special-casing there.
write(#{ <<"read-only">> := true }, _Req, _NodeOpts) ->
    {error, not_found};
write(Opts, Req, _NodeOpts) when is_map(Req) ->
    #{ <<"db">> := DB } = find_env(Opts),
    maps:fold(
        fun(Item, _Value, ok) -> put_item(DB, Item);
           (_Item, _Value, Error) -> Error
        end,
        ok,
        Req
    ).

%% @doc Add a single item to the set's duplicate run.
put_item(DB, Item) ->
    case elmdb:put(DB, ?MAIN_KEY, Item) of
        ok -> ok;
        {error, Type, Description} ->
            ?event(
                error,
                {lmdb_set_error,
                    {type, Type},
                    {description, Description}
                }
            ),
            {error, Type}
    end.

%% @doc Append a strictly-ascending batch of items in one transaction.
%% The direct bulk entry the index builder and merger write through:
%% items must arrive in ascending order and extend the set's tail, or
%% the whole batch is refused — `{error, validation_error}' for in-batch
%% disorder, `{error, key_exist}' for items at or below the current tail.
append(#{ <<"read-only">> := true }, _Items) ->
    {error, not_found};
append(Opts, Items) ->
    #{ <<"db">> := DB } = find_env(Opts),
    case elmdb:put_batch_append(DB, [{?MAIN_KEY, Item} || Item <- Items]) of
        ok -> ok;
        {error, Type, Description} ->
            ?event(
                error,
                {lmdb_set_error,
                    {type, Type},
                    {description, Description}
                }
            ),
            {error, Type}
    end.

%% @doc Read the first item in the set matching the given prefix. A full
%% item is its own prefix, so this doubles as a membership test; a proper
%% prefix is a point lookup whose collision detection belongs to the
%% caller (which recomputes ids from the bytes the item locates).
read(Opts, #{ <<"read">> := Prefix }, _NodeOpts) ->
    #{ <<"db">> := DB } = find_env(Opts),
    Selection = [{prefix, hb_util:bin(Prefix)}, {limit, 1}],
    case elmdb:read_dups(DB, ?MAIN_KEY, Selection) of
        {ok, [Item]} -> {ok, Item};
        {ok, []} -> {error, not_found};
        not_found -> {error, not_found};
        {error, Type, _Description} -> {error, Type}
    end.

%% @doc Return the ascending run of items sharing the `list' prefix. The
%% optional `from' key is an inclusive lower bound and `limit' caps the
%% result, together forming a stateless cursor over the set; an empty
%% prefix lists from the start. `{error, not_found}' means the set itself
%% is empty — so a store list falls through — while an in-range empty
%% selection is `{ok, []}'.
list(Opts, Req = #{ <<"list">> := Prefix }, _NodeOpts) ->
    #{ <<"db">> := DB } = find_env(Opts),
    Selection =
        [{prefix, hb_util:bin(Prefix)}] ++
        case maps:get(<<"from">>, Req, false) of
            false -> [];
            From -> [{from, hb_util:bin(From)}]
        end ++
        case maps:get(<<"limit">>, Req, false) of
            false -> [];
            Limit -> [{limit, hb_util:int(Limit)}]
        end,
    case elmdb:read_dups(DB, ?MAIN_KEY, Selection) of
        {ok, Items} -> {ok, Items};
        not_found -> {error, not_found};
        {error, Type, _Description} -> {error, Type}
    end.

%% @doc Every present item is a `simple' value: the set has no composite
%% entries.
type(Opts, #{ <<"type">> := Prefix }, NodeOpts) ->
    case read(Opts, #{ <<"read">> => Prefix }, NodeOpts) of
        {ok, _Item} -> {ok, simple};
        Error -> Error
    end.

%% @doc Retrieve or create the environment handle for the set.
find_env(Opts = #{ <<"db">> := _ }) -> Opts;
find_env(Opts) -> hb_store:find(Opts).

%% @doc Soft-close the environment by name; refs stay valid and reopen
%% lazily on next access.
stop(#{ <<"store-module">> := ?MODULE, <<"name">> := DataDir }, _Req, _Opts) ->
    catch elmdb:env_close_by_name(hb_util:list(DataDir)),
    ok;
stop(_InvalidStoreOpts, _Req, _Opts) ->
    ok.

%% @doc Completely delete the set's directory and all its contents.
reset(Opts, _Req, _NodeOpts) ->
    case maps:get(<<"name">>, Opts, undefined) of
        undefined ->
            ok;
        DataDir ->
            stop(Opts, #{}, #{}),
            os:cmd(binary_to_list(<< "rm -Rf ", DataDir/binary >>)),
            ok = filelib:ensure_dir(filename:join(hb_util:list(DataDir), "data.mdb")),
            ok
    end.

%%% Tests

%% 17-byte match-index-shaped test item: an 80-bit leading field and a
%% 56-bit trailing field, big-endian so byte order is numeric order.
item(H, O) -> <<H:80/big, O:56/big>>.

%% Drop the cached instance for a store so the next access reopens the
%% environment from the store options, as a fresh node boot would.
forget_instance(#{ <<"store-module">> := Mod, <<"name">> := Name }) ->
    StoreRef = {store, Mod, Name},
    erlang:erase(StoreRef),
    catch persistent_term:erase(StoreRef),
    ok.

test_items(S) ->
    ok = hb_store:write(
        S,
        #{
            item(1, 10) => <<>>,
            item(1, 20) => <<>>,
            item(1, 30) => <<>>,
            item(2, 5) => <<>>,
            item(2, 15) => <<>>
        },
        #{}
    ).

list_test() ->
    S = hb_test_utils:test_store(?MODULE),
    hb_store:reset(S, #{}, #{}),
    % An empty set reports not_found so that a store list falls through.
    ?assertEqual(
        {error, not_found},
        hb_store:list(S, #{ <<"list">> => <<>> }, #{})
    ),
    test_items(S),
    % A prefix selects its full ascending run; an empty prefix, the set.
    ?assertEqual(
        {ok, [item(1, 10), item(1, 20), item(1, 30)]},
        hb_store:list(S, #{ <<"list">> => <<1:80>> }, #{})
    ),
    ?assertEqual(
        {ok, [item(1, 10), item(1, 20), item(1, 30), item(2, 5), item(2, 15)]},
        hb_store:list(S, #{ <<"list">> => <<>> }, #{})
    ),
    % `from' is inclusive: at the start, middle and end of the run.
    ?assertEqual(
        {ok, [item(1, 10), item(1, 20), item(1, 30)]},
        hb_store:list(
            S,
            #{ <<"list">> => <<1:80>>, <<"from">> => item(1, 10) },
            #{}
        )
    ),
    ?assertEqual(
        {ok, [item(1, 20), item(1, 30)]},
        hb_store:list(
            S,
            #{ <<"list">> => <<1:80>>, <<"from">> => item(1, 15) },
            #{}
        )
    ),
    ?assertEqual(
        {ok, [item(1, 30)]},
        hb_store:list(
            S,
            #{ <<"list">> => <<1:80>>, <<"from">> => item(1, 30) },
            #{}
        )
    ),
    % Past the end of the run: present set, empty selection.
    ?assertEqual(
        {ok, []},
        hb_store:list(
            S,
            #{ <<"list">> => <<1:80>>, <<"from">> => item(1, 31) },
            #{}
        )
    ),
    % `limit' caps the page; with `from' it forms a stateless cursor.
    ?assertEqual(
        {ok, [item(1, 10), item(1, 20)]},
        hb_store:list(
            S,
            #{ <<"list">> => <<1:80>>, <<"limit">> => 2 },
            #{}
        )
    ),
    ?assertEqual(
        {ok, [item(1, 30), item(2, 5)]},
        hb_store:list(
            S,
            #{ <<"list">> => <<>>, <<"from">> => item(1, 21), <<"limit">> => 2 },
            #{}
        )
    ),
    ok = hb_store:stop(S).

read_membership_test() ->
    S = hb_test_utils:test_store(?MODULE),
    hb_store:reset(S, #{}, #{}),
    test_items(S),
    % A full item is a membership test; a proper prefix returns the first
    % item carrying it; an absent prefix is a miss.
    ?assertEqual(
        {ok, item(1, 20)},
        hb_store:read(S, #{ <<"read">> => item(1, 20) }, #{})
    ),
    ?assertEqual(
        {ok, item(2, 5)},
        hb_store:read(S, #{ <<"read">> => <<2:80>> }, #{})
    ),
    ?assertEqual(
        {error, not_found},
        hb_store:read(S, #{ <<"read">> => <<3:80>> }, #{})
    ),
    ?assertEqual(
        {ok, simple},
        hb_store:type(S, #{ <<"type">> => item(1, 10) }, #{})
    ),
    ?assertEqual(
        {error, not_found},
        hb_store:type(S, #{ <<"type">> => <<3:80>> }, #{})
    ),
    ok = hb_store:stop(S).

restart_persistence_test() ->
    S = hb_test_utils:test_store(?MODULE),
    hb_store:reset(S, #{}, #{}),
    test_items(S),
    ok = hb_store:stop(S),
    forget_instance(S),
    % A fresh instance over the same directory serves the same set.
    ?assertEqual(
        {ok, [item(1, 10), item(1, 20), item(1, 30), item(2, 5), item(2, 15)]},
        hb_store:list(S, #{ <<"list">> => <<>> }, #{})
    ),
    ok = hb_store:stop(S).

read_only_test() ->
    S = hb_test_utils:test_store(?MODULE),
    hb_store:reset(S, #{}, #{}),
    test_items(S),
    ok = hb_store:stop(S),
    forget_instance(S),
    RO = S#{ <<"read-only">> => true },
    ?assertEqual(
        {ok, item(1, 10)},
        hb_store:read(RO, #{ <<"read">> => <<1:80>> }, #{})
    ),
    % Writes and appends are refused so that a store list falls through.
    ?assertEqual(
        {error, not_found},
        hb_store:write(RO, #{ item(3, 1) => <<>> }, #{})
    ),
    ?assertEqual({error, not_found}, append(RO, [item(3, 1)])),
    ?assertEqual(
        {error, not_found},
        hb_store:read(RO, #{ <<"read">> => <<3:80>> }, #{})
    ),
    ok = hb_store:stop(RO).

reset_test() ->
    S = hb_test_utils:test_store(?MODULE),
    hb_store:reset(S, #{}, #{}),
    test_items(S),
    hb_store:reset(S, #{}, #{}),
    ?assertEqual(
        {error, not_found},
        hb_store:list(S, #{ <<"list">> => <<>> }, #{})
    ),
    ok = hb_store:write(S, #{ item(9, 9) => <<>> }, #{}),
    ?assertEqual(
        {ok, item(9, 9)},
        hb_store:read(S, #{ <<"read">> => item(9, 9) }, #{})
    ),
    ok = hb_store:stop(S).

two_store_fallthrough_test() ->
    S1 = hb_test_utils:test_store(?MODULE, <<"set-first">>),
    S2 = hb_test_utils:test_store(?MODULE, <<"set-second">>),
    hb_store:reset(S1, #{}, #{}),
    hb_store:reset(S2, #{}, #{}),
    ok = hb_store:write(S1, #{ item(1, 7) => <<>> }, #{}),
    ok = hb_store:write(S2, #{ item(1, 3) => <<>>, item(2, 4) => <<>> }, #{}),
    % A point read is served by the first store carrying the prefix; an
    % item only in the second store is reached by fallthrough.
    ?assertEqual(
        {ok, item(1, 7)},
        hb_store:read([S1, S2], #{ <<"read">> => <<1:80>> }, #{})
    ),
    ?assertEqual(
        {ok, item(2, 4)},
        hb_store:read([S1, S2], #{ <<"read">> => <<2:80>> }, #{})
    ),
    ?assertEqual(
        {error, not_found},
        hb_store:read([S1, S2], #{ <<"read">> => <<3:80>> }, #{})
    ),
    ok = hb_store:stop(S1),
    ok = hb_store:stop(S2).

append_test() ->
    S = hb_test_utils:test_store(?MODULE),
    hb_store:reset(S, #{}, #{}),
    Sorted = [item(1, O) || O <- lists:seq(1, 100)],
    ok = append(S, Sorted),
    ?assertEqual(
        {ok, [item(1, 98), item(1, 99), item(1, 100)]},
        hb_store:list(
            S,
            #{ <<"list">> => <<1:80>>, <<"from">> => item(1, 98) },
            #{}
        )
    ),
    % In-batch disorder and below-tail appends are refused whole.
    ?assertEqual(
        {error, validation_error},
        append(S, [item(2, 2), item(2, 1)])
    ),
    ?assertEqual({error, key_exist}, append(S, [item(1, 50)])),
    ?assertEqual(
        {error, not_found},
        hb_store:read(S, #{ <<"read">> => <<2:80>> }, #{})
    ),
    ok = hb_store:stop(S).
