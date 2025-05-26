%%% @doc A simple abstraction layer for AO key value store operations.
%%% 
%%% This interface allows us to swap out the underlying store implementation(s)
%%% as desired, without changing the API that `hb_cache` employs. Additionally,
%%% it enables node operators to customize their configuration to maximize
%%% performance, data availability, and other factors.
%%% 
%%% Stores can be represented in a node's configuration as either a single 
%%% message, or a (`structured@1.0') list of store messages. If a list of stores
%%% is provided, the node will cycle through each until a viable store is found
%%% to execute the given function.
%%% 
%%% A valid store must implement a _subset_ of the following functions:
%%% ```
%%%     start/1:      Initialize the store.
%%%     stop/1:       Stop any processes (etc.) that manage the store.
%%%     reset/1:      Restore the store to its original, empty state.
%%%     scope/0:      A tag describing the 'scope' of a stores search: `in_memory',
%%%                   `local', `remote', `arweave', etc. Used in order to allow
%%%                   node operators to prioritize their stores for search.
%%%     make_group/2: Create a new group of keys in the store with the given ID.
%%%     make_link/3:  Create a link (implying one key should redirect to another)
%%%                   from `existing` to `new` (in that order).
%%%     type/2:       Return whether the value found at the given key is a
%%%                   `composite' (group) type, or a `simple' direct binary.
%%%     read/2:       Read the data at the given location, returning a binary
%%%                   if it is a `simple' value, or a message if it is a complex
%%%                   term.
%%%     write/3:      Write the given `key` with the associated `value` (in that
%%%                   order) to the store.
%%%     list/2:       For `composite' type keys, return a list of its child keys.
%%%     path/2:       Optionally transform a list of path parts into the store's
%%%                   canonical form.
%%% '''
%%% Each function takes a `store' message first, containing an arbitrary set
%%% of its necessary configuration keys, as well as the `store-module' key which
%%% refers to the Erlang module that implements the store.
%%% 
%%% All functions must return `ok` or `{ok, Result}`, as appropriate. Other 
%%% results will lead to the store manager (this module) iterating to the next
%%% store message given by the user. If none of the given store messages are 
%%% able to execute a requested service, the store manager will return 
%%% `{error, no_viable_store}`.

-module(hb_store).
-export([behavior_info/1]).
-export([start/1, stop/1, reset/1]).
-export([filter/2, scope/2, sort/2]).
-export([type/2, read/2, write/3, list/2]).
-export([path/1, path/2, add_path/2, add_path/3, join/1]).
-export([make_group/2, make_link/3, resolve/2]).
-export([generate_test_suite/1, generate_test_suite/2, test_stores/0]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc The number of write and read operations to perform in the benchmark.
-define(STORE_BENCH_WRITE_OPS, 100_000).
-define(STORE_BENCH_READ_OPS, 100_000).

behavior_info(callbacks) ->
    [
        {start, 1}, {stop, 1}, {reset, 1}, {make_group, 2}, {make_link, 3},
        {type, 2}, {read, 2}, {write, 3},
        {list, 2}, {path, 2}, {add_path, 3}
    ].

-define(DEFAULT_SCOPE, local).

%%% Library wrapper implementations.

start(Modules) -> call_all(Modules, start, []).

stop(Modules) -> call_function(Modules, stop, []).

%% @doc Takes a store object and a filter function or match spec, returning a
%% new store object with only the modules that match the filter. The filter
%% function takes 2 arguments: the scope and the options. It calls the store's
%% scope function to get the scope of the module.
filter(Module, Filter) when not is_list(Module) ->
    filter([Module], Filter);
filter(Modules, Filter) ->
    lists:filter(
        fun(Store) ->
            try Filter(get_store_scope(Store), Store)
            catch _:_ -> false
            end
        end,
        Modules
    ).

%% @doc Limit the store scope to only a specific (set of) option(s).
%% Takes either an Opts message or store, and either a single scope or a list
%% of scopes.
scope(Opts, Scope) when is_map(Opts) ->
    case hb_opts:get(store, no_viable_store, Opts) of
        no_viable_store -> Opts;
        Store -> Opts#{ store => scope(Store, Scope) }
    end;
scope(Store, Scope) ->
    filter(
        Store,
        fun(StoreScope, _) ->
            StoreScope == Scope orelse
                (is_list(Scope) andalso lists:member(StoreScope, Scope))
        end
    ).

%% @doc Ask a store for its own scope. If it doesn't have one, return the
%% default scope (local).
get_store_scope(Store) ->
    case call_function(Store, scope, []) of
        no_viable_store -> ?DEFAULT_SCOPE;
        Scope -> Scope
    end.

%% @doc Order a store by a preference of its scopes. This is useful for making
%% sure that faster (or perhaps cheaper) stores are used first. If a list is
%% provided, it will be used as a preference order. If a map is provided,
%% scopes will be ordered by the scores in the map. Any unknown scopes will
%% default to a score of 0.
sort(Stores, PreferenceOrder) when is_list(PreferenceOrder) ->
    sort(
        Stores,
        hb_maps:from_list(
            [
                {Scope, -Index}
            ||
                {Scope, Index} <-
                    lists:zip(
                        PreferenceOrder,
                        lists:seq(1, length(PreferenceOrder))
                    )
            ]
        )
    );
sort(Stores, ScoreMap) ->
    lists:sort(
        fun(Store1, Store2) ->
            hb_maps:get(get_store_scope(Store1), ScoreMap, 0) >
                hb_maps:get(get_store_scope(Store2), ScoreMap, 0)
        end,
        Stores
    ).

%% @doc Join a list of path components together.
join(Path) -> hb_path:to_binary(Path).

%%% The store interface that modules should implement.

%% @doc Read a key from the store.
read(Modules, Key) -> call_function(Modules, read, [Key]).

%% @doc Write a key with a value to the store.
write(Modules, Key, Value) -> call_function(Modules, write, [Key, Value]).

%% @doc Make a group in the store. A group can be seen as a namespace or
%% 'directory' in a filesystem.
make_group(Modules, Path) -> call_function(Modules, make_group, [Path]).

%% @doc Make a link from one path to another in the store.
make_link(Modules, Existing, New) ->
    call_function(Modules, make_link, [Existing, New]).

%% @doc Delete all of the keys in a store. Should be used with extreme
%% caution. Lost data can lose money in many/most of hyperbeam's use cases.
reset(Modules) -> call_function(Modules, reset, []).

%% @doc Get the type of element of a given path in the store. This can be
%% a performance killer if the store is remote etc. Use only when necessary.
type(Modules, Path) -> call_function(Modules, type, [Path]).

%% @doc Create a path from a list of path components. If no store implements
%% the path function, we return the path with the 'default' transformation (id).
path(Path) -> join(Path).
path(_, Path) -> path(Path).

%% @doc Add two path components together. If no store implements the add_path
%% function, we concatenate the paths.
add_path(Path1, Path2) -> Path1 ++ Path2.
add_path(Store, Path1, Path2) ->
    case call_function(Store, add_path, [Path1, Path2]) of
        no_viable_store -> add_path(Path1, Path2);
        Result -> Result
    end.

%% @doc Follow links through the store to resolve a path to its ultimate target.
resolve(Modules, Path) -> call_function(Modules, resolve, [Path]).

%% @doc List the keys in a group in the store. Use only in debugging.
%% The hyperbeam model assumes that stores are built as efficient hash-based
%% structures, so this is likely to be very slow for most stores.
list(Modules, Path) -> call_function(Modules, list, [Path]).

%% @doc Call a function on the first store module that succeeds. Returns its
%% result, or no_viable_store if none of the stores succeed.
call_function(X, _Function, _Args) when not is_list(X) ->
    call_function([X], _Function, _Args);
call_function([], _Function, _Args) ->
    no_viable_store;
call_function([Store = #{<<"store-module">> := Mod} | Rest], Function, Args) ->
    try apply(Mod, Function, [Store | Args]) of
        not_found ->
            call_function(Rest, Function, Args);
        Result ->
            Result
    catch
        Class:Reason:Stacktrace ->
            ?event(warning, {store_call_failed, {Class, Reason, Stacktrace}}),
            call_function(Rest, Function, Args)
    end.

%% @doc Call a function on all modules in the store.
call_all(X, _Function, _Args) when not is_list(X) ->
    call_all([X], _Function, _Args);
call_all([], _Function, _Args) ->
    ok;
call_all([Store = #{<<"store-module">> := Mod} | Rest], Function, Args) ->
    try
        apply(Mod, Function, [Store | Args])
    catch
        Class:Reason:Stacktrace ->
            ?event(warning, {store_call_failed, {Class, Reason, Stacktrace}}),
            ok
    end,
    call_all(Rest, Function, Args).

%%% Test helpers

%% @doc Return a list of stores for testing. Additional individual functions are
%% used to generate store options for those whose drivers are not built by 
%% default into all HyperBEAM distributions.
test_stores() ->
    [
        #{
            <<"store-module">> => hb_store_lmdb,
            <<"prefix">> => <<"cache-TEST/lmdb">>,
            <<"max-size">> => 600 * 1024 * 1024
        },
        #{
            <<"store-module">> => hb_store_fs,
            <<"prefix">> => <<"cache-TEST/fs">>
        }
    ] ++ rocks_stores().

-ifdef(ENABLE_ROCKSDB).
rocks_stores() ->
    [
        #{
            <<"store-module">> => hb_store_rocksdb,
            <<"prefix">> => <<"cache-TEST/rocksdb">>
        }
    ].
-else.
rocks_stores() -> [].
-endif.

generate_test_suite(Suite) ->
    generate_test_suite(Suite, test_stores()).
generate_test_suite(Suite, Stores) ->
    hb:init(),
    lists:map(
        fun(Store = #{<<"store-module">> := Mod}) ->
            {foreach,
                fun() ->
                    hb_store:start(Store)
                end,
                fun(_) ->
                    hb_store:reset(Store),
                    hb_store:stop(Store)
                end,
                [
                    {
                        atom_to_list(Mod) ++ ": " ++ Desc,
                        {
                            timeout,
                            60,
                            fun() ->
                                TestResult = Test(Store),
                                TestResult
                            end
                        }
                    }
                ||
                    {Desc, Test} <- Suite
                ]
            }
        end,
        Stores
    ).

%%% Tests

%% @doc Test path resolution dynamics.
simple_path_resolution_test(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    ok = hb_store:write(Store, <<"test-file">>, <<"test-data">>),
    hb_store:make_link(Store, <<"test-file">>, <<"test-link">>),
    ?assertEqual({ok, <<"test-data">>}, hb_store:read(Store, <<"test-link">>)).

%% @doc Ensure that we can resolve links recursively.
resursive_path_resolution_test(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:write(Store, <<"test-file">>, <<"test-data">>),
    hb_store:make_link(Store, <<"test-file">>, <<"test-link">>),
    hb_store:make_link(Store, <<"test-link">>, <<"test-link2">>),
    ?assertEqual({ok, <<"test-data">>}, hb_store:read(Store, <<"test-link2">>)).

%% @doc Ensure that we can resolve links through a directory.
hierarchical_path_resolution_test(Opts) ->
    Store = Opts,
    hb_store:make_group(Store, <<"test-dir1">>),
    hb_store:write(Store, [<<"test-dir1">>, <<"test-file">>], <<"test-data">>),
    hb_store:make_link(Store, [<<"test-dir1">>], <<"test-link">>),
    ?assertEqual({ok, <<"test-data">>}, hb_store:read(Store, [<<"test-link">>, <<"test-file">>])).

store_suite_test_() ->
    hb_store:generate_test_suite([
        {"simple path resolution", fun simple_path_resolution_test/1},
        {"resursive path resolution", fun resursive_path_resolution_test/1},
        {"hierarchical path resolution", fun hierarchical_path_resolution_test/1}
    ]).

benchmark_suite_test_() ->
    hb_store:generate_test_suite([
        {"benchmark store", fun benchmark_store/1}
    ]).

%% @doc Benchmark a store. By default, we write 10,000 keys and read 10,000
%% keys. This can be altered by setting the `STORE_BENCH_WRITE_OPS' and
%% `STORE_BENCH_READ_OPS' macros.
benchmark_store(Store) ->
    benchmark_store(Store, ?STORE_BENCH_WRITE_OPS, ?STORE_BENCH_READ_OPS).
benchmark_store(Store, WriteOps, ReadOps) ->
    start(Store),
    timer:sleep(100),
    ?event(
        {benchmarking,
            {store, Store},
            {write_ops, WriteOps},
            {read_ops, ReadOps}
        }
    ),
    % Generate random data to write and the keys to read ahead of time.
    RandomData = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Keys =
        lists:map(
            fun(N) ->
                << "key-", (integer_to_binary(N))/binary >>
            end,
            lists:seq(1, WriteOps)
        ),
    {WriteTime, ok} =
        timer:tc(
            fun() ->
                lists:foreach(
                    fun(Key) -> ok = write(Store, Key, RandomData) end,
                    Keys
                )
            end
        ),
    % Calculate write rate.
    WriteRate = erlang:round(WriteOps / (WriteTime / 1000000)),
    hb_util:eunit_print(
        "Wrote ~s records in ~p ms (~s records/s)",
        [
            hb_util:human_int(WriteOps),
            WriteTime/1000,
            hb_util:human_int(WriteRate)
        ]
    ),
    % Generate keys to read ahead of time.
    ReadKeys =
        lists:map(
            fun(_) ->
                << "key-", (integer_to_binary(rand:uniform(ReadOps)))/binary >>
            end,
            lists:seq(1, ReadOps)
        ),
    % Time random reads.
    {ReadTime, ok} =
        timer:tc(
            fun() ->
                lists:foreach(fun(Key) -> read(Store, Key) end, ReadKeys)
            end
        ),
    % Calculate read rate.
    ReadRate = erlang:round(ReadOps / (ReadTime / 1000000)),
    hb_util:eunit_print(
        "Read ~s records in ~p ms (~s records/s)",
        [
            hb_util:human_int(ReadOps),
            ReadTime/1000,
            hb_util:human_int(ReadRate)
        ]
    ).