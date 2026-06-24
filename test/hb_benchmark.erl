%%% @doc Consolidated benchmarks for HyperBEAM.
-module(hb_benchmark).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% AO-Core resolution benchmarks.
-define(AO_BENCHMARK_ITERATIONS, 1_000).

%% Store benchmarks.
-define(STORE_BENCH_WRITE_OPS, 100_000).
-define(STORE_BENCH_READ_OPS, 100_000).
-define(STORE_BENCH_LIST_KEYS, 100_000).
-define(STORE_BENCH_LIST_GROUP_SIZE, 10).
-define(STORE_BENCH_LIST_OPS, 20_000).
-define(BENCH_MSG_WRITE_OPS, 50_000).
-define(BENCH_MSG_READ_OPS, 50_000).
-define(BENCH_MSG_DATA_SIZE, 32).

%% Event benchmarks.
-define(EVENT_BENCHMARK_DURATION, 0.25).

%%% AO-Core benchmarks

%% @doc Run AO-Core resolution benchmarks against each core option profile.
ao_core_benchmark_test_() ->
    hb_test_utils:suite_with_opts(ao_benchmark_suite(), ao_benchmark_opts()).

ao_benchmark_suite() ->
    [
        {benchmark_simple, "simple resolution benchmark",
            fun benchmark_simple_test/1},
        {benchmark_multistep, "multistep resolution benchmark",
            fun benchmark_multistep_test/1},
        {benchmark_get, "get benchmark",
            fun benchmark_get_test/1},
        {benchmark_set, "single value set benchmark",
            fun benchmark_set_test/1},
        {benchmark_set_multiple, "set two keys benchmark",
            fun benchmark_set_multiple_test/1},
        {benchmark_set_multiple_deep, "set two keys deep benchmark",
            fun benchmark_set_multiple_deep_test/1}
    ].

ao_benchmark_opts() ->
    CachedExecStore = hb_test_utils:test_store(),
    [
        #{
            name => normal,
            desc => "Default opts",
            opts => #{ <<"store">> => hb_test_utils:test_store() },
            skip => []
        },
        #{
            name => without_hashpath,
            desc => "Default without hashpath",
            opts => #{
                <<"hashpath">> => ignore,
                <<"store">> => hb_test_utils:test_store()
            },
            skip => []
        },
        #{
            name => no_cache,
            desc => "No cache read or write",
            opts => #{
                <<"hashpath">> => ignore,
                <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
                <<"spawn-worker">> => false,
                <<"store">> => hb_test_utils:test_store()
            },
            skip => [load_as]
        },
        #{
            name => only_store,
            desc => "Store, don't read",
            opts => #{
                <<"hashpath">> => update,
                <<"cache-control">> => [<<"no-cache">>],
                <<"spawn-worker">> => false,
                <<"store">> => CachedExecStore
            },
            skip => [
                denormalized_device_name,
                deep_set_with_device,
                load_as
            ],
            reset => false
        },
        #{
            name => only_if_cached,
            desc => "Only read, don't exec",
            opts => #{
                <<"hashpath">> => ignore,
                <<"cache-control">> => [<<"only-if-cached">>],
                <<"spawn-worker">> => false,
                <<"store">> => CachedExecStore
            },
            skip => [
                resolve_id,
                start_as,
                start_as_with_parameters,
                as_path,
                multiple_as_subresolutions,
                key_from_id_device_with_args,
                get_with_denormalized_key,
                set_new_messages,
                resolve_from_multiple_keys,
                resolve_path_element,
                device_with_default_handler_function,
                device_with_handler_function,
                denormalized_device_name,
                get_with_device,
                get_as_with_device,
                set_with_device,
                device_exports,
                device_excludes,
                device_inheritance,
                deep_set_with_device,
                as,
                as_commitments,
                step_hook,
                paranoid_message_verification,
                paranoid_input_verification,
                paranoid_result_verification
            ]
        }
    ].

%% @doc Benchmark a single direct key resolution.
benchmark_simple_test(Opts) ->
    Time =
        hb_test_utils:benchmark_iterations(
            fun(I) -> hb_ao:resolve(#{ <<"a">> => I }, <<"a">>, Opts) end,
            ?AO_BENCHMARK_ITERATIONS
        ),
    hb_test_utils:benchmark_print(
        <<"Single-step resolutions:">>,
        ?AO_BENCHMARK_ITERATIONS,
        Time
    ).

%% @doc Benchmark a multistep path resolution.
benchmark_multistep_test(Opts) ->
    Time =
        hb_test_utils:benchmark_iterations(
            fun(I) ->
                hb_ao:resolve(
                    #{
                        <<"iteration">> => I,
                        <<"a">> => #{ <<"b">> => #{ <<"return">> => I } }
                    },
                    <<"a/b/return">>,
                    Opts
                )
            end,
            ?AO_BENCHMARK_ITERATIONS
        ),
    hb_test_utils:benchmark_print(
        <<"Multistep resolutions:">>,
        ?AO_BENCHMARK_ITERATIONS,
        Time
    ).

%% @doc Benchmark `hb_ao:get/3'.
benchmark_get_test(Opts) ->
    Time =
        hb_test_utils:benchmark_iterations(
            fun(I) ->
                hb_ao:get(
                    <<"a">>,
                    #{ <<"a">> => <<"1">>, <<"iteration">> => I },
                    Opts
                )
            end,
            ?AO_BENCHMARK_ITERATIONS
        ),
    hb_test_utils:benchmark_print(
        <<"Get operations:">>,
        ?AO_BENCHMARK_ITERATIONS,
        Time
    ).

%% @doc Benchmark setting one value in a message.
benchmark_set_test(Opts) ->
    Time =
        hb_test_utils:benchmark_iterations(
            fun(I) ->
                hb_ao:set(
                    #{ <<"a">> => <<"1">>, <<"iteration">> => I },
                    <<"a">>,
                    <<"2">>,
                    Opts
                )
            end,
            ?AO_BENCHMARK_ITERATIONS
        ),
    hb_test_utils:benchmark_print(
        <<"Single value set operations:">>,
        ?AO_BENCHMARK_ITERATIONS,
        Time
    ).

%% @doc Benchmark setting two top-level values in a message.
benchmark_set_multiple_test(Opts) ->
    Time =
        hb_test_utils:benchmark_iterations(
            fun(I) ->
                hb_ao:set(
                    #{ <<"a">> => <<"1">>, <<"iteration">> => I },
                    #{ <<"a">> => <<"1a">>, <<"b">> => <<"2">> },
                    Opts
                )
            end,
            ?AO_BENCHMARK_ITERATIONS
        ),
    hb_test_utils:benchmark_print(
        <<"Set two keys operations:">>,
        ?AO_BENCHMARK_ITERATIONS,
        Time
    ).

%% @doc Benchmark setting two nested values in a message.
benchmark_set_multiple_deep_test(Opts) ->
    Time =
        hb_test_utils:benchmark_iterations(
            fun(I) ->
                hb_ao:set(
                    #{ <<"a">> => #{ <<"b">> => <<"1">> } },
                    #{ <<"a">> => #{ <<"b">> => <<"2">>, <<"c">> => I } },
                    Opts
                )
            end,
            ?AO_BENCHMARK_ITERATIONS
        ),
    hb_test_utils:benchmark_print(
        <<"Set two keys operations:">>,
        ?AO_BENCHMARK_ITERATIONS,
        Time
    ).

%%% Store benchmarks

%% @doc Run the store benchmark suite against every configured test store.
store_benchmark_suite_test_() ->
    hb_store:generate_test_suite([
        {"benchmark key read write", fun benchmark_key_read_write/1},
        {"benchmark list", fun benchmark_list/1},
        {"benchmark flat message read write", fun benchmark_flat_message_read_write/1},
        {"benchmark nested message read write", fun benchmark_nested_message_read_write/1}
    ]).

%% @doc Benchmark random store key reads and writes.
benchmark_key_read_write(Store = #{ <<"benchmark-scale">> := Scale }) ->
    benchmark_key_read_write(
        Store,
        erlang:ceil(Scale * ?STORE_BENCH_WRITE_OPS),
        erlang:ceil(Scale * ?STORE_BENCH_READ_OPS)
    );
benchmark_key_read_write(Store) ->
    benchmark_key_read_write(Store, ?STORE_BENCH_WRITE_OPS, ?STORE_BENCH_READ_OPS).

benchmark_key_read_write(Store, WriteOps, ReadOps) ->
    hb_store:start(Store),
    timer:sleep(100),
    ?event(
        {benchmarking,
            {store, Store},
            {write_ops, WriteOps},
            {read_ops, ReadOps}
        }
    ),
    RandomData = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Keys =
        lists:map(
            fun(N) ->
                << "key-", (integer_to_binary(N))/binary >>
            end,
            lists:seq(1, ReadOps)
        ),
    WriteReqs =
        lists:map(
            fun(Key) ->
                store_write_req(Key, RandomData)
            end,
            Keys
        ),
    {WriteTime, ok} =
        timer:tc(
            fun() ->
                lists:foreach(
                    fun(Req) ->
                        ok = hb_store:write(Store, Req, #{})
                    end,
                    WriteReqs
                )
            end
        ),
    WriteRate = erlang:round(WriteOps / (WriteTime / 1000000)),
    hb_format:eunit_print(
        "Wrote ~s records in ~p ms (~s records/s)",
        [
            hb_util:human_int(WriteOps),
            WriteTime/1000,
            hb_util:human_int(WriteRate)
        ]
    ),
    ReadReqs =
        lists:map(
            fun(_) ->
                #{
                    <<"read">> =>
                        << "key-", (integer_to_binary(rand:uniform(ReadOps)))/binary >>
                }
            end,
            lists:seq(1, ReadOps)
        ),
    {ReadTime, NotFoundCount} =
        timer:tc(
            fun() ->
                lists:foldl(
                    fun(Req, Count) ->
                        case hb_store:read(Store, Req, #{}) of
                            {ok, _} -> Count;
                            _ -> Count + 1
                        end
                    end,
                    0,
                    ReadReqs
                )
            end
        ),
    ReadRate = erlang:round(ReadOps / (ReadTime / 1000000)),
    hb_format:eunit_print(
        "Read ~s records in ~p ms (~s records/s)",
        [
            hb_util:human_int(ReadOps),
            ReadTime/1000,
            hb_util:human_int(ReadRate)
        ]
    ),
    ?assertEqual(0, NotFoundCount, "Written keys not found in store.").

%% @doc Benchmark listing grouped store keys.
benchmark_list(Store = #{ <<"benchmark-scale">> := Scale }) ->
    benchmark_list(
        Store,
        erlang:ceil(Scale * ?STORE_BENCH_LIST_KEYS),
        erlang:ceil(Scale * ?STORE_BENCH_LIST_OPS),
        erlang:ceil(Scale * ?STORE_BENCH_LIST_GROUP_SIZE)
    );
benchmark_list(Store) ->
    benchmark_list(
        Store,
        ?STORE_BENCH_LIST_KEYS,
        ?STORE_BENCH_LIST_OPS,
        ?STORE_BENCH_LIST_GROUP_SIZE
    ).

benchmark_list(Store, WriteOps, ListOps, GroupSize) ->
    hb_store:start(Store),
    timer:sleep(100),
    ?event(
        {benchmarking,
            {store, Store},
            {keys, hb_util:human_int(WriteOps)},
            {groups, hb_util:human_int(WriteOps div GroupSize)},
            {lists, hb_util:human_int(ListOps)}
        }
    ),
    Groups =
        lists:map(
            fun(_) ->
                GroupID = hb_util:human_id(crypto:strong_rand_bytes(32)),
                {
                    GroupID,
                    lists:map(
                        fun(M) ->
                            {
                                <<"key-", (integer_to_binary(M))/binary >>,
                                <<"value-", (integer_to_binary(M))/binary >>
                            }
                        end,
                        lists:seq(1, GroupSize)
                    )
                }
            end,
            lists:seq(1, GroupCount = WriteOps div GroupSize)
        ),
    hb_format:eunit_print(
        "Generated ~s groups of ~s keys",
        [
            hb_util:human_int(GroupCount),
            hb_util:human_int(GroupSize)
        ]
    ),
    {WriteTime, _} =
        timer:tc(
            fun() ->
                lists:map(
                    fun({GroupID, KeyPairs}) ->
                        ok = hb_store:group(Store, GroupID, #{}),
                        lists:foreach(
                            fun({Key, Value}) ->
                                ok =
                                    hb_store:write(
                                        Store,
                                        store_write_req(
                                            <<GroupID/binary, "/", Key/binary>>,
                                            Value
                                        ),
                                        #{}
                                    )
                            end,
                            KeyPairs
                        )
                    end,
                    Groups
                ),
                {LastGroupID, _} = lists:last(Groups),
                hb_store:list(Store, LastGroupID, #{})
            end
        ),
    hb_test_utils:benchmark_print(
        <<"Wrote and flushed">>,
        <<"keys">>,
        WriteOps,
        WriteTime / 1_000_000
    ),
    ReadGroups =
        lists:map(
            fun(_) ->
                lists:nth(rand:uniform(GroupCount), Groups)
            end,
            lists:seq(1, ListOps)
        ),
    {ReadTime, NotFoundCount} =
        timer:tc(
            fun() ->
                lists:foldl(
                    fun({GroupID, GroupKeyValues}, Count) ->
                        ExpectedKeys =
                            [ KeyInGroup || {KeyInGroup, _} <- GroupKeyValues ],
                        case hb_store:list(Store, GroupID, #{}) of
                            {ok, ListedKeys} ->
                                Res =
                                    lists:all(
                                        fun({KeyInGroup, _ExpectedValue}) ->
                                            lists:member(KeyInGroup, ListedKeys)
                                        end,
                                        GroupKeyValues
                                    ),
                                case Res of
                                    true -> Count;
                                    _ ->
                                        ?event(
                                            {list_group_not_found,
                                                {group, GroupID},
                                                {received_keys, ListedKeys},
                                                {expected_keys, ExpectedKeys}
                                            }
                                        ),
                                        Count + 1
                                end;
                            _ ->
                                ?event(
                                    {list_group_not_found,
                                        {group, GroupID},
                                        {expected_keys, ExpectedKeys}
                                    }
                                ),
                                Count + 1
                        end
                    end,
                    0,
                    ReadGroups
                )
            end
        ),
    hb_test_utils:benchmark_print(
        <<"Listed">>,
        <<"groups">>,
        ListOps,
        ReadTime / 1_000_000
    ),
    ?assertEqual(0, NotFoundCount, "Groups listed in correctly.").

%% @doc Benchmark flat message cache reads and writes for a store.
benchmark_flat_message_read_write(Store) ->
    benchmark_message_read_write(Store, flat).

%% @doc Benchmark nested message cache reads and writes for a store.
benchmark_nested_message_read_write(Store) ->
    benchmark_message_read_write(Store, nested).

benchmark_message_read_write(Store = #{ <<"benchmark-scale">> := Scale }, Shape) ->
    benchmark_message_read_write(
        Store,
        erlang:ceil(Scale * ?BENCH_MSG_WRITE_OPS),
        erlang:ceil(Scale * ?BENCH_MSG_READ_OPS),
        Shape
    );
benchmark_message_read_write(Store, Shape) ->
    benchmark_message_read_write(
        Store,
        ?BENCH_MSG_WRITE_OPS,
        ?BENCH_MSG_READ_OPS,
        Shape
    ).

benchmark_message_read_write(Store, WriteOps, ReadOps, Shape) ->
    hb_store:start(Store),
    Opts = #{
        <<"store">> => Store,
        <<"priv-wallet">> => hb:wallet()
    },
    TestDataSize = ?BENCH_MSG_DATA_SIZE * 8,
    timer:sleep(100),
    ?event(
        {benchmarking,
            {store, Store},
            {shape, Shape},
            {write_ops, WriteOps},
            {read_ops, ReadOps}
        }
    ),
    {GenerateTime, Msgs} =
        timer:tc(
            fun() ->
                lists:map(
                    fun(N) ->
                        benchmark_message(Shape, N, TestDataSize)
                    end,
                    lists:seq(1, WriteOps)
                )
            end
        ),
    hb_test_utils:benchmark_print(
        <<"Generated">>,
        <<"messages">>,
        WriteOps,
        GenerateTime / 1_000_000
    ),
    {WriteTime, MsgPairs} =
        timer:tc(
            fun() ->
                lists:map(
                    fun(Msg) ->
                        {hb_util:ok(hb_cache:write(Msg, Opts)), Msg}
                    end,
                    Msgs
                )
            end
        ),
    hb_test_utils:benchmark_print(
        <<"Wrote">>,
        <<"messages">>,
        WriteOps,
        WriteTime / 1_000_000
    ),
    ReadKeys =
        lists:map(
            fun(_) ->
                lists:nth(rand:uniform(length(MsgPairs)), MsgPairs)
            end,
            lists:seq(1, ReadOps)
        ),
    {ReadTime, NotFoundCount} =
        timer:tc(
            fun() ->
                lists:foldl(
                    fun({MsgID, _Msg}, Count) ->
                        case hb_cache:read(MsgID, Opts) of
                            {ok, _CacheMsg} -> Count;
                            _ -> Count + 1
                        end
                    end,
                    0,
                    ReadKeys
                )
            end
        ),
    hb_test_utils:benchmark_print(
        <<"Read">>,
        <<"messages">>,
        ReadOps,
        ReadTime / 1_000_000
    ),
    ?assertEqual(0, NotFoundCount, "Written keys not found in store.").

benchmark_message(flat, N, TestDataSize) ->
    #{
        <<"process">> => <<0:TestDataSize, N:32>>,
        <<"slot">> => N
    };
benchmark_message(nested, N, TestDataSize) ->
    (benchmark_message(flat, N, TestDataSize))#{
        <<"message">> =>
            #{
                <<"body">> => <<"test", 0:TestDataSize, N:32>>
            }
    }.

store_write_req(Key, Value) ->
    #{ hb_path:to_binary(Key) => Value }.

%%% Scheduler benchmarks

%% @doc Benchmark the long-lived scheduler server when preloaded modules are
%% compiled into the active test profile.
scheduler_server_benchmark_test_() ->
    case code:ensure_loaded(dev_scheduler_server) of
        {module, dev_scheduler_server} ->
            {timeout, 30, fun scheduler_server_benchmark/0};
        _ ->
            []
    end.

scheduler_server_benchmark() ->
    BenchTime = 1,
    Wallet = ar_wallet:new(),
    Opts = #{ <<"priv-wallet">> => Wallet },
    SignedItem = hb_message:commit(
        #{ <<"data">> => <<"test">>, <<"random-key">> => rand:uniform(10000) },
        Opts
    ),
    ID = hb_message:id(SignedItem, all, Opts),
    dev_scheduler_registry:find(ID, SignedItem, Opts),
    ?event({benchmark_start, ?MODULE}),
    Iterations = hb_test_utils:benchmark(
        fun(X) ->
            MsgX = #{
                <<"path">> => <<"Schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> =>
                    #{
                        <<"type">> => <<"Message">>,
                        <<"test-val">> => X
                    }
            },
            dev_scheduler_server:schedule(ID, MsgX)
        end,
        BenchTime
    ),
    hb_format:eunit_print(
        "Scheduled ~p messages in ~p seconds (~.2f msg/s)",
        [Iterations, BenchTime, Iterations / BenchTime]
    ),
    ?assertMatch(
        #{ current := X } when X == Iterations - 1,
        dev_scheduler_server:info(dev_scheduler_registry:find(ID))
    ).

%% @doc Run local and HTTP scheduler benchmarks.
scheduler_benchmark_suite_test_parallel_() ->
    Bench = [
        {benchmark, "benchmark", fun scheduler_single_resolution/1},
        {multihttp_benchmark, "multihttp_benchmark", fun scheduler_many_clients/1}
    ],
    {serial, hb_test_utils:suite_with_opts(Bench, scheduler_benchmark_suite())}.

scheduler_single_resolution(Opts) ->
    scheduler_start(),
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    SignedOpts = Opts#{ <<"priv-wallet">> => Wallet },
    BenchTime =
        case hb_opts:get(scheduling_mode, local_confirmation, SignedOpts) of
            aggressive -> 1.0;
            _ -> 2.0
        end,
    Base = hb_message:commit(scheduler_test_process(SignedOpts), SignedOpts),
    ?event({benchmark_start, ?MODULE}),
    MsgToSchedule = hb_message:commit(#{
        <<"type">> => <<"Message">>,
        <<"test-key">> => <<"test-val">>
    }, SignedOpts),
    {ok, _} = hb_cache:write(MsgToSchedule, SignedOpts),
    Iterations = hb_test_utils:benchmark(
        fun(_) ->
            MsgX = #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> => MsgToSchedule
            },
            ?assertMatch({ok, _}, hb_ao:resolve(Base, MsgX, SignedOpts)),
            case hb_opts:get(scheduling_mode, local_confirmation, SignedOpts) of
                aggressive -> timer:sleep(50);
                _ -> ok
            end
        end,
        BenchTime
    ),
    ?event(benchmark, {scheduled, Iterations}),
    Res = #{
        <<"path">> => <<"slot">>,
        <<"method">> => <<"GET">>,
        <<"process">> => hb_util:human_id(hb_message:id(Base, all, SignedOpts))
    },
    ?assertMatch({ok, #{ <<"current">> := CurrentSlot }}
            when CurrentSlot == Iterations - 1,
        hb_ao:resolve(Base, Res, SignedOpts)),
    ?event(bench, {res, Iterations - 1}),
    hb_test_utils:benchmark_print(
        <<"Scheduled through AO-Core:">>,
        <<"messages">>,
        Iterations,
        BenchTime
    ),
    ?assert(Iterations > 3).

scheduler_many_clients(Opts) ->
    BenchTime = 0.25,
    Processes = hb_opts:get(workers, 25, Opts),
    {Node, HTTPOpts} = scheduler_http_init(Opts),
    PMsg = hb_message:commit(scheduler_test_process(HTTPOpts), HTTPOpts),
    Base = hb_message:commit(#{
        <<"path">> => <<"/~scheduler@1.0/schedule">>,
        <<"method">> => <<"POST">>,
        <<"process">> => PMsg,
        <<"body">> => hb_message:commit(#{ <<"inner">> => <<"test">> }, HTTPOpts)
    }, HTTPOpts),
    {ok, _} = hb_http:post(Node, Base, HTTPOpts),
    Iterations = hb_test_utils:benchmark(
        fun(X) ->
            {ok, _} = hb_http:post(Node, Base, HTTPOpts),
            ?event(bench, {iteration, X, self()})
        end,
        BenchTime,
        Processes
    ),
    ?event({iterations, Iterations}),
    hb_format:eunit_print(
        "Scheduled ~p messages with ~p workers through HTTP in ~ps (~.2f msg/s)",
        [Iterations, Processes, BenchTime, Iterations / BenchTime]
    ),
    {ok, Res} = scheduler_http_get_slot(Node, PMsg),
    ?event(bench, {res, Res}),
    ?assert(Iterations > 10).

scheduler_benchmark_suite() ->
    [
        #{
            name => fs,
            requires => [hb_store_fs],
            parallel => false,
            timeout => 30,
            opts => #{
                <<"store">> => hb_test_utils:test_store(hb_store_fs),
                <<"scheduling-mode">> => local_confirmation,
                <<"port">> => 0
            },
            desc => <<"FS store, local conf.">>
        },
        #{
            name => fs_aggressive,
            requires => [hb_store_fs],
            parallel => false,
            timeout => 30,
            opts => #{
                <<"store">> => hb_test_utils:test_store(hb_store_fs),
                <<"scheduling-mode">> => aggressive,
                <<"scheduler-default-commitment-spec">> => <<"httpsig@1.0">>,
                <<"port">> => 0
            },
            desc => <<"FS store, aggressive conf.">>
        },
        #{
            name => rocksdb,
            requires => [hb_store_rocksdb],
            parallel => false,
            timeout => 30,
            opts => #{
                <<"store">> => hb_test_utils:test_store(hb_store_rocksdb),
                <<"scheduling-mode">> => local_confirmation,
                <<"port">> => 0
            },
            desc => <<"RocksDB store, local conf.">>
        },
        #{
            name => rocksdb_aggressive,
            requires => [hb_store_rocksdb],
            parallel => false,
            timeout => 30,
            opts => #{
                <<"store">> => hb_test_utils:test_store(hb_store_rocksdb),
                <<"scheduling-mode">> => aggressive,
                <<"scheduler-default-commitment-spec">> => <<"httpsig@1.0">>,
                <<"port">> => 0
            },
            desc => <<"RocksDB store, aggressive conf.">>
        },
        #{
            name => rocksdb_extreme_aggressive_h3,
            requires => [http3],
            parallel => false,
            timeout => 30,
            opts => #{
                <<"store">> => hb_test_utils:test_store(hb_store_rocksdb),
                <<"scheduling-mode">> => aggressive,
                <<"scheduler-default-commitment-spec">> => <<"httpsig@1.0">>,
                <<"protocol">> => http3,
                <<"workers">> => 100
            },
            desc => <<"100xRocksDB store, aggressive conf, http/3.">>
        }
    ].

scheduler_test_process(#{ <<"priv-wallet">> := Wallet})  ->
    scheduler_test_process(hb_util:human_id(ar_wallet:to_address(Wallet)));
scheduler_test_process(Address) ->
    #{
        <<"device">> => <<"scheduler@1.0">>,
        <<"device-stack">> => [<<"cron@1.0">>, <<"wasm-64@1.0">>],
        <<"image">> => <<"wasm-image-id">>,
        <<"type">> => <<"Process">>,
        <<"scheduler-location">> => Address,
        <<"test-random-seed">> => rand:uniform(1337)
    }.

scheduler_http_init(Opts) ->
    scheduler_start(),
    Wallet = ar_wallet:new(),
    ExtendedOpts = Opts#{
        <<"priv-wallet">> => Wallet,
        <<"store">> => [
            hb_test_utils:test_store(),
            #{ <<"store-module">> => hb_store_gateway, <<"store">> => [] }
        ]
    },
    Node = hb_http_server:start_node(ExtendedOpts),
    {Node, ExtendedOpts}.

scheduler_http_get_slot(N, PMsg) ->
    ID = hb_message:id(PMsg, all),
    Wallet = hb:wallet(),
    {ok, _} = hb_http:get(N, hb_message:commit(#{
        <<"path">> => <<"/~scheduler@1.0/slot">>,
        <<"method">> => <<"GET">>,
        <<"target">> => ID
    }, #{ <<"priv-wallet">> => Wallet }), #{}).

scheduler_start() ->
    application:ensure_all_started(hb),
    <<I1:32/unsigned-integer, I2:32/unsigned-integer, I3:32/unsigned-integer>>
        = crypto:strong_rand_bytes(12),
    rand:seed(exsplus, {I1, I2, I3}),
    ok.

%%% JSON interface benchmarks

%% @doc Benchmark a minimal AOS stack evaluation through the JSON interface.
aos_stack_benchmark_test_() ->
    {timeout, 20, fun() ->
        BenchTime = 0.25,
        Opts = #{ <<"store">> => hb_test_utils:test_store() },
        RawWASMMsg = generate_aos_stack("test/aos-2-pure-xs.wasm", <<"WASM">>, Opts),
        Proc =
            hb_ao:get(
                <<"process">>,
                RawWASMMsg,
                Opts#{ <<"hashpath">> => ignore }
            ),
        ProcID = hb_ao:get(id, Proc, Opts),
        Msg = generate_aos_msg(ProcID, <<"return 1">>, Opts),
        {ok, Initialized} =
            hb_ao:resolve(
                RawWASMMsg,
                Msg,
                Opts
            ),
        Req = generate_aos_msg(ProcID, <<"return 1+1">>, Opts),
        Iterations =
            hb_test_utils:benchmark(
                fun() -> hb_ao:resolve(Initialized, Req, Opts) end,
                BenchTime
            ),
        hb_test_utils:benchmark_print(
            <<"(Minimal AOS stack:) Evaluated">>,
            <<"messages">>,
            Iterations,
            BenchTime
        ),
        ?assert(Iterations >= 1),
        ok
    end}.

normalize_benchmark_opts(Opts) ->
    Opts#{
        <<"priv-wallet">> => hb_opts:get(priv_wallet, hb:wallet(), Opts)
    }.

generate_aos_stack(File, _Mode, RawOpts) ->
    Opts = normalize_benchmark_opts(RawOpts),
    application:ensure_all_started(hb),
    Msg0 = cache_wasm_image(File, Opts),
    Image = hb_ao:get(<<"image">>, Msg0, Opts),
    Base = Msg0#{
        <<"device">> => <<"stack@1.0">>,
        <<"device-stack">> =>
            [
                <<"wasi@1.0">>,
                <<"json-iface@1.0">>,
                <<"wasm-64@1.0">>,
                <<"multipass@1.0">>
            ],
        <<"input-prefix">> => <<"process">>,
        <<"output-prefix">> => <<"wasm">>,
        <<"passes">> => 2,
        <<"stack-keys">> => [<<"init">>, <<"compute">>],
        <<"process">> =>
            hb_message:commit(#{
                <<"type">> => <<"Process">>,
                <<"image">> => Image,
                <<"scheduler">> => hb:address(),
                <<"authority">> => hb:address()
            }, Opts)
    },
    {ok, Req} = hb_ao:resolve(Base, <<"init">>, Opts),
    Req.

generate_aos_msg(ProcID, Code, RawOpts) ->
    Opts = normalize_benchmark_opts(RawOpts),
    hb_message:commit(#{
        <<"path">> => <<"compute">>,
        <<"body">>
            => hb_message:commit(#{
                <<"action">> => <<"Eval">>,
                <<"data">> => Code,
                <<"target">> => ProcID
            }, Opts),
        <<"block-height">> => 1
    }, Opts).

%%% WASM and stack benchmarks

%% @doc Benchmark direct calls into `hb_beamr'.
beamr_benchmark_test() ->
    BenchTime = 0.25,
    {ok, File} = file:read_file("test/test-64.wasm"),
    {ok, WASM, _ImportMap, _Exports} = hb_beamr:start(File),
    Iterations = hb_test_utils:benchmark(
        fun() ->
            {ok, [Result]} = hb_beamr:call(WASM, "fac", [5.0]),
            ?assertEqual(120.0, Result)
        end,
        BenchTime
    ),
    ?event(benchmark, {scheduled, Iterations}),
    ?assert(Iterations > 1000),
    hb_test_utils:benchmark_print(
        <<"Direct beamr: Executed">>,
        <<"calls">>,
        Iterations,
        BenchTime
    ),
    ok.

%% @doc Benchmark stack device resolution.
stack_benchmark_test() ->
    BenchTime = 0.3,
    Msg = #{
        <<"device">> => <<"stack@1.0">>,
        <<"device-stack">> =>
            #{
                <<"1">> => generate_append_device(<<"+D1">>),
                <<"2">> => generate_append_device(<<"+D2">>),
                <<"3">> => generate_append_device(<<"+D3">>),
                <<"4">> => generate_append_device(<<"+D4">>),
                <<"5">> => generate_append_device(<<"+D5">>)
            },
        <<"result">> => <<"INIT">>
    },
    Iterations =
        hb_test_utils:benchmark(
            fun() ->
                hb_ao:resolve(Msg,
                    #{
                        <<"path">> => <<"append">>,
                        <<"bin">> => <<"2">>
                    },
                    #{}
                ),
                {count, 5}
            end,
            BenchTime
        ),
    hb_test_utils:benchmark_print(
        <<"Stack:">>,
        <<"resolutions">>,
        Iterations,
        BenchTime
    ),
    ?assert(Iterations >= 10).

%% @doc Benchmark WASM executions through AO-Core.
wasm_benchmark_test() ->
    BenchTime = 0.25,
    application:ensure_all_started(hb),
    hb:init(),
    Msg0 = cache_wasm_image("test/test-64.wasm"),
    {ok, Base} = hb_ao:resolve(Msg0, <<"init">>, #{}),
    Req =
        hb_maps:merge(
            Base,
            #{
                <<"function">> => <<"fac">>,
                <<"parameters">> => [5.0]
            },
            #{}
        ),
    Iterations =
        hb_test_utils:benchmark(
            fun() ->
                hb_ao:resolve(Req, <<"compute">>, #{})
            end,
            BenchTime
        ),
    ?event(benchmark, {scheduled, Iterations}),
    hb_test_utils:benchmark_print(
        <<"Through AO-Core:">>,
        <<"resolutions">>,
        Iterations,
        BenchTime
    ),
    ?assert(Iterations > 5),
    ok.

cache_wasm_image(Image) ->
    cache_wasm_image(Image, #{}).

cache_wasm_image(Image, Opts) ->
    {ok, Bin} = file:read_file(Image),
    Msg = #{ <<"body">> => Bin },
    {ok, ID} = hb_cache:write(Msg, Opts),
    #{
        <<"device">> => <<"wasm-64@1.0">>,
        <<"image">> => ID
    }.

generate_append_device(Separator) ->
    #{
        append =>
            fun(M1 = #{ <<"pass">> := 3 }, _) ->
                {ok, M1};
               (M1 = #{ <<"result">> := Existing }, #{ <<"bin">> := New }) ->
                ?event({appending, {existing, Existing}, {new, New}}),
                {ok, M1#{ <<"result">> =>
                    << Existing/binary, Separator/binary, New/binary>>
                }}
            end
    }.

%%% Process worker benchmarks

%% @doc Benchmark a persistent WASM worker process.
process_simple_wasm_persistent_worker_benchmark_test_parallel() ->
    Opts = process_benchmark_opts(),
    BenchTime = 0.05,
    Base = process_wasm_process(<<"test/test-64.wasm">>, Opts),
    schedule_wasm_call(Base, <<"fac">>, [5.0], Opts),
    schedule_wasm_call(Base, <<"fac">>, [6.0], Opts),
    {ok, Initialized} =
        hb_ao:resolve(
            Base,
            #{ <<"path">> => <<"compute">>, <<"slot">> => 1 },
            Opts#{ <<"spawn-worker">> => true, <<"process-workers">> => true }
        ),
    Iterations = hb_test_utils:benchmark(
        fun(Iteration) ->
            schedule_wasm_call(
                Initialized,
                <<"fac">>,
                [5.0],
                Opts
            ),
            ?assertMatch(
                {ok, _},
                hb_ao:resolve(
                    Initialized,
                    #{ <<"path">> => <<"compute">>, <<"slot">> => Iteration + 1 },
                    Opts
                )
            )
        end,
        BenchTime
    ),
    ?event(benchmark, {scheduled, Iterations}),
    hb_format:eunit_print(
        "Scheduled and evaluated ~p simple wasm process messages in ~p s (~s msg/s)",
        [Iterations, BenchTime, hb_util:human_int(Iterations / BenchTime)]
    ),
    ?assert(Iterations >= 1),
    ok.

%% @doc Benchmark a persistent AOS worker process.
process_aos_persistent_worker_benchmark_test_parallel_() ->
    {timeout, 30, fun() ->
        BenchTime = 0.25,
        process_init(),
        Base = process_aos_process(),
        schedule_aos_call(Base, <<"X=1337">>),
        FirstSlotReq = #{
            <<"path">> => <<"compute">>,
            <<"slot">> => 0
        },
        ?assertMatch(
            {ok, _},
            hb_ao:resolve(Base, FirstSlotReq, #{ <<"spawn-worker">> => true })
        ),
        Iterations = hb_test_utils:benchmark(
            fun(Iteration) ->
                schedule_aos_call(
                    Base,
                    <<"return X + ", (integer_to_binary(Iteration))/binary>>
                ),
                ?assertMatch(
                    {ok, _},
                    hb_ao:resolve(
                        Base,
                        #{ <<"path">> => <<"compute">>, <<"slot">> => Iteration },
                        #{}
                    )
                )
            end,
            BenchTime
        ),
        ?event(benchmark, {scheduled, Iterations}),
        hb_format:eunit_print(
            "Scheduled and evaluated ~p AOS process messages in ~p s (~s msg/s)",
            [Iterations, BenchTime, hb_util:human_int(Iterations / BenchTime)]
        ),
        ?assert(Iterations >= 1),
        ok
    end}.

process_benchmark_opts() ->
    process_init(),
    #{
        <<"store">> => hb_test_utils:test_store(hb_store_lmdb),
        <<"priv-wallet">> => ar_wallet:new()
    }.

process_init() ->
    application:ensure_all_started(hb).

process_base_process(Opts) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    hb_message:commit(
        #{
            <<"device">> => <<"process@1.0">>,
            <<"scheduler-device">> => <<"scheduler@1.0">>,
            <<"scheduler-location">> => hb_opts:get(scheduler, Address, Opts),
            <<"type">> => <<"Process">>,
            <<"test-random-seed">> => rand:uniform(1337)
        },
        Opts#{ <<"priv-wallet">> => Wallet }
    ).

process_wasm_process(WASMImage, Opts) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    #{ <<"image">> := WASMImageID } = cache_wasm_image(WASMImage, Opts),
    hb_message:commit(
        hb_maps:merge(
            hb_message:uncommitted(process_base_process(Opts), Opts),
            #{
                <<"execution-device">> => <<"stack@1.0">>,
                <<"device-stack">> => [<<"wasm-64@1.0">>],
                <<"image">> => WASMImageID
            },
            Opts
        ),
        Opts#{ <<"priv-wallet">> => Wallet }
    ).

process_aos_process() ->
    process_aos_process(#{}).

process_aos_process(Opts) ->
    process_aos_process(Opts, [
        <<"wasi@1.0">>,
        <<"json-iface@1.0">>,
        <<"wasm-64@1.0">>,
        <<"multipass@1.0">>
    ]).

process_aos_process(Opts, Stack) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    WASMProc = process_wasm_process(<<"test/aos-2-pure-xs.wasm">>, Opts),
    hb_message:commit(
        hb_maps:merge(
            hb_message:uncommitted(WASMProc, Opts),
            #{
                <<"device-stack">> => Stack,
                <<"execution-device">> => <<"stack@1.0">>,
                <<"scheduler-device">> => <<"scheduler@1.0">>,
                <<"output-prefix">> => <<"wasm">>,
                <<"patch-from">> => <<"/results/outbox">>,
                <<"passes">> => 2,
                <<"stack-keys">> =>
                    [
                        <<"init">>,
                        <<"compute">>,
                        <<"snapshot">>,
                        <<"normalize">>
                    ],
                <<"scheduler">> =>
                    hb_opts:get(scheduler, Address, Opts),
                <<"authority">> =>
                    hb_opts:get(authority, Address, Opts)
            },
            Opts
        ),
        Opts#{ <<"priv-wallet">> => Wallet }
    ).

schedule_test_message(Base, Text, MsgBase, Opts) ->
    ?event(debug_test, {opts, Opts}),
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    UncommittedBase =
        hb_message:uncommitted(MsgBase, Opts#{ <<"priv-wallet">> => Wallet }),
    Req =
        hb_message:commit(
            #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> =>
                    hb_message:commit(
                        UncommittedBase#{
                            <<"type">> => <<"Message">>,
                            <<"test-label">> => Text
                        },
                        Opts#{ <<"priv-wallet">> => Wallet }
                    )
            },
            Opts#{ <<"priv-wallet">> => Wallet }
        ),
    {ok, _} = hb_ao:resolve(Base, Req, Opts#{ <<"priv-wallet">> => Wallet }).

schedule_aos_call(Base, Code) ->
    schedule_aos_call(Base, Code, #{}).

schedule_aos_call(Base, Code, Opts) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    ProcID = hb_message:id(Base, all),
    Req =
        hb_message:commit(
            #{
                <<"action">> => <<"Eval">>,
                <<"data">> => Code,
                <<"target">> => ProcID
            },
            Opts#{ <<"priv-wallet">> => Wallet }
        ),
    schedule_test_message(Base, <<"TEST MSG">>, Req, Opts).

schedule_wasm_call(Base, FuncName, Params, Opts) ->
    Wallet = hb:wallet(),
    Req =
        hb_message:commit(
            #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> =>
                    hb_message:commit(
                        #{
                            <<"type">> => <<"Message">>,
                            <<"function">> => FuncName,
                            <<"parameters">> => Params
                        },
                        Opts#{ <<"priv-wallet">> => Wallet }
                    )
            },
            Opts#{ <<"priv-wallet">> => Wallet }
        ),
    ?assertMatch({ok, _}, hb_ao:resolve(Base, Req, Opts)).

%%% Lua benchmarks

%% @doc Benchmark direct Lua device executions.
lua_direct_benchmark_test() ->
    BenchTime = 0.25,
    {ok, Module} = file:read_file("test/test.lua"),
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"module">> => #{
            <<"content-type">> => <<"application/lua">>,
            <<"body">> => Module
        },
        <<"parameters">> => []
    },
    Iterations = hb_test_utils:benchmark(
        fun(X) ->
            {ok, _} = hb_ao:resolve(Base, <<"assoctable">>, #{}),
            ?event({iteration, X})
        end,
        BenchTime
    ),
    ?event({iterations, Iterations}),
    hb_test_utils:benchmark_print(
        <<"Direct Lua:">>,
        <<"executions">>,
        Iterations,
        BenchTime
    ).

%% @doc Benchmark a process whose execution device is `lua@5.3a'.
lua_pure_process_benchmark_test_() ->
    {timeout,
        30,
        fun() ->
            pure_lua_process_benchmark(#{
                <<"process-snapshot-slots">> => 50
            })
    end}.

pure_lua_process_benchmark(Opts) ->
    BenchMsgs = 30,
    hb:init(),
    Process = generate_lua_process("test/test.lua", Opts),
    {ok, _} = hb_cache:write(Process, Opts),
    Message = generate_lua_test_message(Process, Opts),
    lists:foreach(
        fun(X) ->
            hb_ao:resolve(Process, Message, Opts#{ <<"hashpath">> => ignore }),
            ?event(debug_lua, {scheduled, X})
        end,
        lists:seq(1, BenchMsgs)
    ),
    ?event(debug_lua, {executing, BenchMsgs}),
    BeforeExec = os:system_time(millisecond),
    {ok, _} = hb_ao:resolve(Process, <<"now">>, Opts),
    AfterExec = os:system_time(millisecond),
    ExecMs = AfterExec - BeforeExec,
    hb_test_utils:benchmark_print(
        <<"Pure Lua process: Computed">>,
        <<"slots">>,
        BenchMsgs,
        ExecMs / 1000
    ),
    ?assert(ExecMs =< 500).

%% @doc Benchmark a HyperAOS Lua process.
lua_aos_process_benchmark_test_() ->
    {timeout, 30, fun() ->
        BenchMsgs = 6,
        Opts = #{
            <<"hashpath">> => ignore,
            <<"process-snapshot-slots">> => 50
        },
        Process = generate_lua_process("test/hyper-aos.lua", Opts),
        Message = generate_lua_test_message(Process, Opts),
        lists:foreach(
            fun(X) ->
                hb_ao:resolve(Process, Message, Opts),
                ?event(debug_lua, {scheduled, X})
            end,
            lists:seq(1, BenchMsgs)
        ),
        ?event(debug_lua, {executing, BenchMsgs}),
        BeforeExec = os:system_time(millisecond),
        {ok, _} = hb_ao:resolve(
            Process,
            <<"now">>,
            Opts
        ),
        AfterExec = os:system_time(millisecond),
        ExecMs = AfterExec - BeforeExec,
        hb_test_utils:benchmark_print(
            <<"HyperAOS process: Computed">>,
            <<"slots">>,
            BenchMsgs,
            ExecMs / 1000
        ),
        ?assert(ExecMs =< 250)
    end}.

generate_lua_process(File, Opts) ->
    NormOpts =
        Opts#{ <<"priv-wallet">> => hb_opts:get(priv_wallet, hb:wallet(), Opts) },
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), NormOpts),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    {ok, Module} = file:read_file(File),
    hb_message:commit(
        #{
            <<"device">> => <<"process@1.0">>,
            <<"type">> => <<"Process">>,
            <<"scheduler-device">> => <<"scheduler@1.0">>,
            <<"execution-device">> => <<"lua@5.3a">>,
            <<"module">> => #{
                <<"content-type">> => <<"application/lua">>,
                <<"body">> => Module
            },
            <<"authority">> => [
                Address,
                <<"E3FJ53E6xtAzcftBpaw2E1H4ZM9h6qy6xz9NXh5lhEQ">>
            ],
            <<"scheduler-location">> =>
                hb_util:human_id(ar_wallet:to_address(Wallet)),
            <<"test-random-seed">> => rand:uniform(1337)
        },
        NormOpts
    ).

generate_lua_test_message(Process, Opts) ->
    generate_lua_test_message(
        Process,
        Opts,
        <<"""
        Count = 0
        function add()
            Send({Target = 'Foo', Data = 'Bar' });
            Count = Count + 1
        end
        add()
        return Count
        """>>
    ).

generate_lua_test_message(Process, Opts, ToEval) when is_binary(ToEval) ->
    generate_lua_test_message(
        Process,
        Opts,
        #{
            <<"action">> => <<"Eval">>,
            <<"body">> => #{
                <<"content-type">> => <<"application/lua">>,
                <<"body">> => hb_util:bin(ToEval)
            }
        }
    );
generate_lua_test_message(Process, Opts, MsgBase) ->
    ProcID = hb_message:id(Process, all),
    NormOpts =
        Opts#{ <<"priv-wallet">> => hb_opts:get(priv_wallet, hb:wallet(), Opts) },
    hb_message:commit(#{
            <<"path">> => <<"schedule">>,
            <<"method">> => <<"POST">>,
            <<"body">> =>
                hb_message:commit(
                    MsgBase#{
                        <<"target">> => ProcID,
                        <<"type">> => <<"Message">>,
                        <<"random-seed">> => rand:uniform(1337)
                    },
                    NormOpts
                )
        },
        NormOpts
    ).

%%% Event benchmarks

%% @doc Benchmark full event logging.
event_benchmark_test() ->
    Iterations =
        hb_test_utils:benchmark(
            fun() ->
                hb_event:log(test_module, {test, 1})
            end,
            ?EVENT_BENCHMARK_DURATION
        ),
    hb_test_utils:benchmark_print(
        <<"Recorded">>,
        <<"events">>,
        Iterations,
        ?EVENT_BENCHMARK_DURATION
    ),
    ?assert(Iterations >= 1000),
    ok.

%% @doc Benchmark debug-print topic lookup.
event_print_lookup_benchmark_test() ->
    DefaultOpts = hb_opts:default_message_with_env(),
    Iterations =
        hb_test_utils:benchmark(
            fun() ->
                hb_event:should_print(print, test_module, DefaultOpts)
                    orelse hb_event:should_print(print, test_event, DefaultOpts)
            end,
            ?EVENT_BENCHMARK_DURATION
        ),
    hb_test_utils:benchmark_print(
        <<"Looked-up">>,
        <<"topics">>,
        Iterations,
        ?EVENT_BENCHMARK_DURATION
    ),
    ?assert(Iterations >= 1000),
    ok.

%% @doc Benchmark low-level event recording.
event_record_benchmark_test() ->
    Iterations =
        hb_test_utils:benchmark(
            fun() -> hb_event:record(test_module, {test, 1}, #{}) end,
            ?EVENT_BENCHMARK_DURATION
        ),
    hb_test_utils:benchmark_print(
        <<"Recorded">>,
        <<"events">>,
        Iterations,
        ?EVENT_BENCHMARK_DURATION
    ),
    ?assert(Iterations >= 1000),
    ok.

-ifdef(NO_EVENTS).
event_drain_rate_benchmark_test() -> ok.
-else.
%% @doc Benchmark event server drain throughput.
event_drain_rate_benchmark_test() ->
    NumKeys = 50,
    NumEvents = 100000,
    hb_event:log(warmup, {warmup, 0}),
    timer:sleep(100),
    EventPid = hb_name:lookup(hb_event),
    wait_drain(EventPid, 5000),
    erlang:suspend_process(EventPid),
    Keys =
        [
            {
                hb_util:bin([<<"corr-topic-">>, hb_util:int(K)]),
                hb_util:bin([<<"corr-event-">>, hb_util:int(K)])
            }
        ||
            K <- lists:seq(1, NumKeys)
        ],
    fill_mailbox(EventPid, NumEvents, Keys),
    erlang:resume_process(EventPid),
    {DrainTime, _} =
        timer:tc(
            fun() ->
                wait_drain(EventPid, 30000)
            end
        ),
    DrainRate = round(NumEvents / (max(1, DrainTime) / 1_000_000)),
    hb_test_utils:benchmark_print(
        <<"Drained">>,
        <<"events">>,
        DrainRate,
        1
    ),
    ?assert(DrainRate >= 10000),
    ok.

fill_mailbox(_Pid, 0, _Keys) -> ok;
fill_mailbox(Pid, N, Keys = [{Topic, Event}|_]) ->
    Pid ! {record, Topic, Event, 1},
    fill_mailbox(Pid, N - 1, hb_util:shuffle(Keys)).

wait_drain(Pid, Timeout) ->
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    wait_drain_loop(Pid, Deadline).

wait_drain_loop(Pid, Deadline) ->
    case erlang:process_info(Pid, message_queue_len) of
        {message_queue_len, 0} -> ok;
        {message_queue_len, _} ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true -> error(drain_timeout);
                false ->
                    timer:sleep(10),
                    wait_drain_loop(Pid, Deadline)
            end;
        undefined ->
            error(event_server_dead)
    end.
-endif.

%%% HTTP auth benchmarks

%% @doc Benchmark PBKDF2 key derivation for HTTP auth.
http_auth_pbkdf2_benchmark_test() ->
    Key = crypto:strong_rand_bytes(32),
    Iterations = 2 * 600_000,
    KeyLength = 32,
    Derivations =
        hb_test_utils:benchmark(
            fun() ->
                hb_crypto:pbkdf2(sha256, Key, <<"salt">>, Iterations, KeyLength)
            end,
            0.5
        ),
    hb_test_utils:benchmark_print(
        <<"Derived">>,
        <<"keys (1.2m iterations each)">>,
        Derivations
    ).
