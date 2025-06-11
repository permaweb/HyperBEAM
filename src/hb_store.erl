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
%%% `not_found`.

-module(hb_store).
-export([behavior_info/1]).
-export([start/1, stop/1, reset/1]).
-export([filter/2, scope/2, sort/2]).
-export([type/2, read/2, write/3, list/2]).
-export([path/1, path/2, add_path/2, add_path/3, join/1]).
-export([make_group/2, make_link/3, resolve/2]).
-export([find/1]).
-export([generate_test_suite/1, generate_test_suite/2, test_stores/0]).
-include("include/hb.hrl").

-include_lib("eunit/include/eunit.hrl").

%% @doc The number of write and read operations to perform in the benchmark.
-define(STORE_BENCH_WRITE_OPS, 100_000).
-define(STORE_BENCH_READ_OPS,  100_000).
-define(BENCH_MSG_WRITE_OPS,   250).
-define(BENCH_MSG_READ_OPS,    250).
-define(BENCH_MSG_DATA_SIZE,   1024).


behavior_info(callbacks) ->
    [{start, 1},
     {stop, 1},
     {reset, 1},
     {make_group, 2},
     {make_link, 3},
     {type, 2},
     {read, 2},
     {write, 3},
     {list, 2},
     {path, 2},
     {add_path, 3}].


-define(DEFAULT_SCOPE, local).

%%% Store named terms registry functions.

-ifdef(STORE_EVENTS).
%% @doc Find or spawn a store instance by its store opts.


find(StoreOpts) ->
    {Time, Result} = timer:tc(fun() -> do_find(StoreOpts) end),
    hb_event:increment(<<"store_duration">>, <<"find">>, #{}, Time),
    hb_event:increment(<<"store">>, <<"find">>, #{}, 1),
    Result.


-else.


find(StoreOpts) ->
    do_find(StoreOpts).


-endif.


do_find(StoreOpts = #{<<"store-module">> := Mod}) ->
    Name = maps:get(<<"name">>, StoreOpts, Mod),
    LookupName = {store, Mod, Name},
    case get(LookupName) of
        undefined ->
            try persistent_term:get(LookupName) of
                Instance1 ->
                    EnsuredInstance = ensure_instance_alive(StoreOpts, Instance1),
                    put(LookupName, EnsuredInstance),
                    EnsuredInstance
            catch
                error:badarg -> spawn_instance(StoreOpts)
            end;
        InstanceMessage ->
            ensure_instance_alive(StoreOpts, InstanceMessage)
    end.


%% @doc Create a new instance of a store and return its term.
spawn_instance(StoreOpts = #{<<"store-module">> := Mod}) ->
    Name = maps:get(<<"name">>, StoreOpts, Mod),
    try Mod:start(StoreOpts) of
        ok -> ok;
        {ok, InstanceMessage} ->
            put({store, Mod, Name}, InstanceMessage),
            persistent_term:put({store, Mod, Name}, InstanceMessage),
            InstanceMessage;
        {error, Reason} ->
            ?event(error, {store_start_failed, {Mod, Name, Reason}}),
            throw({store_start_failed, {Mod, Name, Reason}})
    catch
        error:undef ->
            ok
    end.


%% @doc Handle a found instance message. If it contains a PID, we check if it
%% is alive. If it does not, we return it as is.
ensure_instance_alive(StoreOpts, InstanceMessage = #{<<"pid">> := Pid}) ->
    case is_process_alive(Pid) of
        true -> InstanceMessage;
        false -> spawn_instance(StoreOpts)
    end;
ensure_instance_alive(_, InstanceMessage) ->
    InstanceMessage.


%%% Library wrapper implementations.


%% @doc Ensure that a store, or list of stores, have all been started.
start(StoreOpts) when not is_list(StoreOpts) -> start([StoreOpts]);
start([]) -> ok;
start([StoreOpts | Rest]) ->
    find(StoreOpts),
    start(Rest).


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
              try
                  Filter(get_store_scope(Store), Store)
              catch
                  _:_ -> false
              end
      end,
      Modules).


%% @doc Limit the store scope to only a specific (set of) option(s).
%% Takes either an Opts message or store, and either a single scope or a list
%% of scopes.
scope(Opts, Scope) when is_map(Opts) ->
    case hb_opts:get(store, no_viable_store, Opts) of
        no_viable_store -> Opts;
        Store -> Opts#{store => scope(Store, Scope)}
    end;
scope(Store, Scope) ->
    filter(
      Store,
      fun(StoreScope, _) ->
              StoreScope == Scope orelse
              (is_list(Scope) andalso lists:member(StoreScope, Scope))
      end).


%% @doc Ask a store for its own scope. If it doesn't have one, return the
%% default scope (local).
get_store_scope(Store) ->
    case call_function(Store, scope, []) of
        not_found -> ?DEFAULT_SCOPE;
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
        [ {Scope, -Index}
          || {Scope, Index} <- lists:zip(
                                 PreferenceOrder,
                                 lists:seq(1, length(PreferenceOrder))) ]));
sort(Stores, ScoreMap) ->
    lists:sort(
      fun(Store1, Store2) ->
              hb_maps:get(get_store_scope(Store1), ScoreMap, 0) >
              hb_maps:get(get_store_scope(Store2), ScoreMap, 0)
      end,
      Stores).


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
        not_found -> add_path(Path1, Path2);
        Result -> Result
    end.


%% @doc Follow links through the store to resolve a path to its ultimate target.
resolve(Modules, Path) -> call_function(Modules, resolve, [Path]).


%% @doc List the keys in a group in the store. Use only in debugging.
%% The hyperbeam model assumes that stores are built as efficient hash-based
%% structures, so this is likely to be very slow for most stores.
list(Modules, Path) -> call_function(Modules, list, [Path]).


%% @doc Call a function on the first store module that succeeds. Returns its
%% result, or `not_found` if none of the stores succeed. If `TIME_CALLS` is set,
%% this function will also time the call and increment the appropriate event
%% counter.
-ifdef(STORE_EVENTS).


call_function(X, Function, Args) ->
    {Time, Result} = timer:tc(fun() -> do_call_function(X, Function, Args) end),
    hb_event:increment(<<"store_duration">>, hb_util:bin(Function), #{}, Time),
    hb_event:increment(<<"store">>, hb_util:bin(Function), #{}, 1),
    Result.


-else.


call_function(X, Function, Args) ->
    do_call_function(X, Function, Args).


-endif.


do_call_function(X, _Function, _Args) when not is_list(X) ->
    do_call_function([X], _Function, _Args);
do_call_function([], _Function, _Args) ->
    not_found;
do_call_function([Store = #{<<"store-module">> := Mod} | Rest], Function, Args) ->
    try apply(Mod, Function, [Store | Args]) of
        not_found ->
            do_call_function(Rest, Function, Args);
        Result ->
            Result
    catch
        Class:Reason:Stacktrace ->
            ?event(warning, {store_call_failed, {Class, Reason, Stacktrace}}),
            do_call_function(Rest, Function, Args)
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
     % #{
     %     <<"store-module">> => hb_store_fs,
     %     <<"name">> => <<"cache-TEST/fs">>
     % },
     #{
       <<"store-module">> => hb_store_lmdb,
       <<"name">> => <<"cache-TEST/lmdb">>
      },
     #{
       <<"store-module">> => hb_store_lru,
       <<"name">> => <<"cache-TEST/lru">>,
       <<"persistent-store">> => [#{
                                    <<"store-module">> => hb_store_fs,
                                    <<"name">> => <<"cache-TEST/lru">>
                                   }]
      }] ++ rocks_stores().


-ifdef(ENABLE_ROCKSDB).


rocks_stores() ->
    [#{
       <<"store-module">> => hb_store_rocksdb,
       <<"name">> => <<"cache-TEST/rocksdb">>
      }].


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
               [ {atom_to_list(Mod) ++ ": " ++ Desc,
                  {timeout,
                   60,
                   fun() ->
                           TestResult = Test(Store),
                           TestResult
                   end}}
                 || {Desc, Test} <- Suite ]}
      end,
      Stores).


%%% Tests


%% @doc Test path resolution dynamics.
simple_path_resolution_test(Store) ->
    ok = hb_store:write(Store, <<"test-file">>, <<"test-data">>),
    hb_store:make_link(Store, <<"test-file">>, <<"test-link">>),
    ?assertEqual({ok, <<"test-data">>}, hb_store:read(Store, <<"test-link">>)).


%% @doc Ensure that we can resolve links recursively.
resursive_path_resolution_test(Store) ->
    hb_store:write(Store, <<"test-file">>, <<"test-data">>),
    hb_store:make_link(Store, <<"test-file">>, <<"test-link">>),
    hb_store:make_link(Store, <<"test-link">>, <<"test-link2">>),
    ?assertEqual({ok, <<"test-data">>}, hb_store:read(Store, <<"test-link2">>)).


%% @doc Ensure that we can resolve links through a directory.
hierarchical_path_resolution_test(Store) ->
    hb_store:make_group(Store, <<"test-dir1">>),
    hb_store:write(Store, [<<"test-dir1">>, <<"test-file">>], <<"test-data">>),
    hb_store:make_link(Store, [<<"test-dir1">>], <<"test-link">>),
    ?assertEqual({ok, <<"test-data">>}, hb_store:read(Store, [<<"test-link">>, <<"test-file">>])).


store_suite_test_() ->
    generate_test_suite([{"simple path resolution", fun simple_path_resolution_test/1},
                         {"resursive path resolution", fun resursive_path_resolution_test/1},
                         {"hierarchical path resolution", fun hierarchical_path_resolution_test/1}]).


benchmark_suite_test_() ->
    generate_test_suite([{"benchmark key read write", fun benchmark_key_read_write/1},
                         {"benchmark message read write", fun benchmark_message_read_write/1}]).


%% @doc Benchmark a store. By default, we write 10,000 keys and read 10,000
%% keys. This can be altered by setting the `STORE_BENCH_WRITE_OPS' and
%% `STORE_BENCH_READ_OPS' macros.
benchmark_key_read_write(Store) ->
    benchmark_key_read_write(Store, ?STORE_BENCH_WRITE_OPS, ?STORE_BENCH_READ_OPS).


benchmark_key_read_write(Store, WriteOps, ReadOps) ->
    start(Store),
    timer:sleep(100),
    ?event(
      {benchmarking,
       {store, Store},
       {write_ops, WriteOps},
       {read_ops, ReadOps}}),
    % Generate random data to write and the keys to read ahead of time.
    RandomData = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Keys =
        lists:map(
          fun(N) ->
                  <<"key-", (integer_to_binary(N))/binary>>
          end,
          lists:seq(1, ReadOps)),
    {WriteTime, ok} =
        timer:tc(
          fun() ->
                  lists:foreach(
                    fun(Key) -> ok = write(Store, Key, RandomData) end,
                    Keys)
          end),
    % Calculate write rate.
    WriteRate = erlang:round(WriteOps / (WriteTime / 1000000)),
    hb_util:eunit_print(
      "Wrote ~s records in ~p ms (~s records/s)",
      [hb_util:human_int(WriteOps),
       WriteTime / 1000,
       hb_util:human_int(WriteRate)]),
    % Generate keys to read ahead of time.
    ReadKeys =
        lists:map(
          fun(_) ->
                  <<"key-", (integer_to_binary(rand:uniform(ReadOps)))/binary>>
          end,
          lists:seq(1, ReadOps)),
    % Time random reads.
    {ReadTime, NotFoundCount} =
        timer:tc(
          fun() ->
                  lists:foldl(
                    fun(Key, Count) ->
                            case read(Store, Key) of
                                {ok, _} -> Count;
                                _ -> Count + 1
                            end
                    end,
                    0,
                    ReadKeys)
          end),
    % Calculate read rate.
    ReadRate = erlang:round(ReadOps / (ReadTime / 1000000)),
    hb_util:eunit_print(
      "Read ~s records in ~p ms (~s records/s)",
      [hb_util:human_int(ReadOps),
       ReadTime / 1000,
       hb_util:human_int(ReadRate)]),
    ?assertEqual(0, NotFoundCount, "Written keys not found in store.").


benchmark_message_read_write(Store) ->
    benchmark_message_read_write(Store, ?BENCH_MSG_WRITE_OPS, ?BENCH_MSG_READ_OPS).


benchmark_message_read_write(Store, WriteOps, ReadOps) ->
    start(Store),
    Opts = #{store => Store, priv_wallet => hb:wallet()},
    TestDataSize = ?BENCH_MSG_DATA_SIZE * 8,  % in _bits_
    timer:sleep(100),
    ?event(
      {benchmarking,
       {store, Store},
       {write_ops, WriteOps},
       {read_ops, ReadOps}}),
    % Generate a random message to write and the keys to read ahead of time.
    Msgs =
        lists:map(
          fun(N) ->
                  #{
                    <<"process">> => hb_util:human_id(crypto:strong_rand_bytes(32)),
                    <<"slot">> => N,
                    <<"message">> =>
                        hb_message:commit(
                          #{
                            <<"body">> => <<"test", 0:TestDataSize, N:32>>
                           },
                          Opts)
                   }
          end,
          lists:seq(1, WriteOps)),
    hb_util:eunit_print(
      "Generated ~s messages (size ~s bits)",
      [hb_util:human_int(WriteOps),
       hb_util:human_int(TestDataSize)]),
    {WriteTime, MsgPairs} =
        timer:tc(
          fun() ->
                  lists:map(
                    fun(Msg) ->
                            {hb_util:ok(hb_cache:write(Msg, Opts)), Msg}
                    end,
                    Msgs)
          end),
    % Calculate write rate.
    WriteRate = erlang:round(WriteOps / (WriteTime / 1000000)),
    hb_util:eunit_print(
      "Wrote ~s records in ~p ms (~s records/s)",
      [hb_util:human_int(WriteOps),
       WriteTime / 1000,
       hb_util:human_int(WriteRate)]),
    % Generate keys to read ahead of time.
    ReadKeys =
        lists:map(
          fun(_) ->
                  lists:nth(rand:uniform(length(MsgPairs)), MsgPairs)
          end,
          lists:seq(1, ReadOps)),
    % Time random reads.
    {ReadTime, NotFoundCount} =
        timer:tc(
          fun() ->
                  lists:foldl(
                    fun({MsgID, Msg}, Count) ->
                            case hb_cache:read(MsgID, Opts) of
                                {ok, Msg1} ->
                                    case hb_cache:ensure_all_loaded(Msg1, Opts) of
                                        Msg -> Count;
                                        _ -> Count + 1
                                    end;
                                _ -> Count + 1
                            end
                    end,
                    0,
                    ReadKeys)
          end),
    % Calculate read rate.
    ReadRate = erlang:round(ReadOps / (ReadTime / 1000000)),
    hb_util:eunit_print(
      "Read ~s records in ~p ms (~s records/s)",
      [hb_util:human_int(ReadOps),
       ReadTime / 1000,
       hb_util:human_int(ReadRate)]),
    ?assertEqual(0, NotFoundCount, "Written keys not found in store.").
