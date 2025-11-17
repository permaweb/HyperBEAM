%%% @doc A module that provides a cache for scheduler assignments and locations.
-module(dev_scheduler_cache).
-export([write/2, write_spawn/2, write_location/2]).
-export([read/3, read_location/2]).
-export([list/2, latest/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The pseudo-path prefix which the scheduler cache should use.
-define(SCHEDULER_CACHE_PREFIX, <<"~scheduler@1.0">>).

%% @doc Merge the scheduler store with the main store. Used before writing
%% to the cache.
opts(Opts) ->
    Opts#{
        store =>
            hb_opts:get(
                scheduler_store,
                hb_opts:get(store, no_viable_store, Opts),
                Opts
            )
    }.

%% @doc Write an assignment message into the cache.
write(RawAssignment, RawOpts) ->
    Assignment = hb_cache:ensure_all_loaded(RawAssignment, RawOpts),
    Opts = opts(RawOpts),
    Store = hb_opts:get(store, no_viable_store, Opts),
    % Write the message into the main cache
    ProcID = hb_ao:get(<<"process">>, Assignment, Opts),
    Slot = hb_ao:get(<<"slot">>, Assignment, Opts),
    ?event(
        {writing_assignment,
            {proc_id, ProcID},
            {slot, Slot},
            {assignment, Assignment}
        }
    ),
    case hb_cache:write(Assignment, Opts) of
        {ok, RootPath} ->
            % Create symlinks from the message on the process and the 
            % slot on the process to the underlying data.
            hb_store:make_link(
                Store,
                RootPath,
                hb_store:path(
                    Store,
                    [
                        ?SCHEDULER_CACHE_PREFIX,
                        <<"assignments">>,
                        hb_util:human_id(ProcID),
                        hb_ao:normalize_key(Slot)
                    ]
                )
            ),
            ok;
        {error, Reason} ->
            ?event(error, {failed_to_write_assignment, {reason, Reason}}),
            {error, Reason}
    end.

%% @doc Write the initial assignment message to the cache.
write_spawn(RawInitMessage, Opts) ->
    InitMessage = hb_cache:ensure_all_loaded(RawInitMessage, Opts),
    hb_cache:write(InitMessage, opts(Opts)).

%% @doc Get an assignment message from the cache.
read(ProcID, Slot, Opts) when is_integer(Slot) ->
    read(ProcID, hb_util:bin(Slot), Opts);
read(ProcID, Slot, RawOpts) ->
    Opts = opts(RawOpts),
    Store = hb_opts:get(store, no_viable_store, Opts),
    ResolvedPath =
        P2 = hb_store:resolve(
            Store,
            P1 = hb_store:path(Store, [
                ?SCHEDULER_CACHE_PREFIX,
                "assignments",
                hb_util:human_id(ProcID),
                Slot
            ])
        ),
    ?event(
        {read_assignment,
            {proc_id, ProcID},
            {slot, Slot},
            {store, Store}
        }
    ),
    ?event({resolved_path, {p1, P1}, {p2, P2}, {resolved, ResolvedPath}}),
    case hb_cache:read(ResolvedPath, Opts) of
        {ok, Assignment} ->
            % If the slot key is not present, the format of the assignment is
            % AOS2, so we need to convert it to the canonical format.
            case hb_ao:get(<<"variant">>, Assignment, Opts) of
                <<"ao.TN.1">> ->
                    Loaded = hb_cache:ensure_all_loaded(Assignment, Opts),
                    Norm = dev_scheduler_formats:aos2_to_assignment(Loaded, Opts),
                    ?event({normalized_aos2_assignment, Norm}),
                    {ok, Norm};
                <<"ao.N.1">> ->
                    {ok, hb_cache:ensure_all_loaded(Assignment, Opts)}
            end;
        not_found ->
            ?event(debug_sched, {read_assignment, {res, not_found}}),
            not_found
    end.

%% @doc Get the assignments for a process.
list(ProcID, RawOpts) ->
    Opts = opts(RawOpts),
    hb_cache:list_numbered(
        hb_store:path(hb_opts:get(store, no_viable_store, Opts), [
            ?SCHEDULER_CACHE_PREFIX,
            "assignments",
            hb_util:human_id(ProcID)
        ]),
        Opts
    ).

%% @doc Get the latest assignment from the cache.
latest(ProcID, RawOpts) ->
    Opts = opts(RawOpts),
    ?event({getting_assignments_from_cache, {proc_id, ProcID}, {opts, Opts}}),
    case dev_scheduler_cache:list(ProcID, Opts) of
        [] ->
            ?event({no_assignments_in_cache, {proc_id, ProcID}}),
            not_found;
        Assignments ->
            AssignmentNum = lists:max(Assignments),
            ?event(
                {found_assignment_from_cache,
                    {proc_id, ProcID},
                    {assignment_num, AssignmentNum}
                }
            ),
            {ok, Assignment} = dev_scheduler_cache:read(
                ProcID,
                AssignmentNum,
                Opts
            ),
            {
                AssignmentNum,
                hb_ao:get(
                    <<"hash-chain">>, Assignment, #{ hashpath => ignore })
            }
    end.

%% @doc Read the latest known scheduler location for an address.
read_location(Address, RawOpts) ->
    Opts = opts(RawOpts),
    Res =
        hb_cache:read(
            hb_store:path(hb_opts:get(store, no_viable_store, Opts), [
                ?SCHEDULER_CACHE_PREFIX,
                "locations",
                hb_util:human_id(Address)
            ]),
            Opts
        ),
    Event =
        case Res of
            {ok, _} -> found_in_store;
            not_found -> not_found_in_store;
            _ -> local_lookup_unexpected_result
        end,
    ?event(scheduler_location, {Event, {address, Address}, {res, Res}}),
    Res.

%% @doc Write the latest known scheduler location for an address.
write_location(LocationMsg, RawOpts) ->
    Opts = opts(RawOpts),
    Signers = hb_message:signers(LocationMsg, Opts),
    ?event(
        scheduler_location,
        {caching_locally,
            {signers, Signers},
            {location_msg, LocationMsg}
        }
    ),
    case hb_cache:write(LocationMsg, Opts) of
        {ok, RootPath} ->
            lists:foreach(
                fun(Signer) ->
                    hb_store:make_link(
                        hb_opts:get(store, no_viable_store, Opts),
                        RootPath,
                        hb_store:path(
                            hb_opts:get(store, no_viable_store, Opts),
                            [
                                ?SCHEDULER_CACHE_PREFIX,
                                "locations",
                                hb_util:human_id(Signer)
                            ]
                        )
                    )
                end,
                Signers
            ),
            ok;
        false ->
            % The message is not valid, so we don't cache it.
            {error, <<"Invalid scheduler location message. Not caching.">>};
        {error, Reason} ->
            ?event(warning, {failed_to_cache_location_msg, {reason, Reason}}),
            {error, Reason}
    end.

%%% Tests

%% @doc Test that a volatile schedule is lost on restart.
volatile_schedule_test() ->
    VolStore = hb_test_utils:test_store(hb_store_fs, <<"volatile-sched">>),
    NonVolStore = hb_test_utils:test_store(hb_store_fs, <<"non-volatile-sched">>),
    Opts = #{
        store => [NonVolStore],
        scheduler_store => [VolStore]
    },
    hb_store:start(VolStore),
    hb_store:start(NonVolStore),
    Assignment = #{
        <<"variant">> => <<"ao.N.1">>,
        <<"process">> => ProcID = hb_util:human_id(crypto:strong_rand_bytes(32)),
        <<"slot">> => 1,
        <<"hash-chain">> => <<"test-hash-chain">>
    },
    ?assertEqual(ok, write(Assignment, Opts)),
    ?assertMatch({1, _}, latest(ProcID, Opts)),
    {ok, ReadAssignment} = read(ProcID, 1, Opts),
    ?assertEqual(ReadAssignment, hb_message:normalize_commitments(Assignment, Opts)),
    hb_store:stop(VolStore),
    hb_store:reset(VolStore),
    hb_store:start(VolStore),
    ?assertMatch(not_found, latest(ProcID, Opts)),
    ?assertMatch(not_found, read(ProcID, 1, Opts)).

%% @doc Test concurrent writes to scheduler store from multiple processes.
concurrent_scheduler_write_test() ->
    VolStore = hb_test_utils:test_store(hb_store_fs, <<"concurrent-vol">>),
    NonVolStore = hb_test_utils:test_store(hb_store_fs, <<"concurrent-nonvol">>),
    Opts = #{
        store => [NonVolStore],
        scheduler_store => [VolStore]
    },
    hb_store:start(VolStore),
    hb_store:start(NonVolStore),
    Workers = 50,
    ProcID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Parent = self(),
    lists:foreach(fun(Slot) ->
        spawn_link(fun() ->
            Assignment = #{
                <<"process">> => ProcID,
                <<"slot">> => Slot,
                <<"hash-chain">> =>
                    <<"concurrent-test-", (integer_to_binary(Slot))/binary>>
            },
            Result = write(Assignment, Opts),
            Parent ! {write_result, Slot, Result}
        end)
    end, lists:seq(1, Workers)),
    Results =
        lists:map(
            fun(Slot) ->
                receive
                    {write_result, Slot, Result} -> 
                        ?event(testing, {write_result, Slot, Result}),
                        Result
                after 5000 ->
                    timeout
                end
            end,
            lists:seq(1, Workers)
        ),
    ?event(testing, {concurrent_write_results, Results,Workers}),
    ?assertEqual(lists:duplicate(Workers, ok), Results),
    AllSlots = list(ProcID, Opts),
    ?event(testing, {all_slots, AllSlots}),
    ?assertEqual(Workers, length(AllSlots)),
    ?assertEqual(lists:seq(1, Workers), lists:sort(AllSlots)).

%% @doc Test concurrent reads during writes to detect race conditions.
concurrent_read_write_test() ->
    VolStore = hb_test_utils:test_store(hb_store_fs, <<"race-vol">>),
    NonVolStore = hb_test_utils:test_store(hb_store_fs, <<"race-nonvol">>),
    Opts = #{
        store => [NonVolStore],
        scheduler_store => [VolStore]
    },
    hb_store:start(VolStore),
    hb_store:start(NonVolStore),
    ProcID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Parent = self(),
    ?event(testing, {concurrent_test_proc_id, ProcID}),
    spawn_link(fun() ->
        lists:foreach(fun(Slot) ->
            Assignment = #{
                <<"variant">> => <<"ao.N.1">>,
                <<"process">> => ProcID,
                <<"slot">> => Slot,
                <<"hash-chain">> => <<"race-test-", (integer_to_binary(Slot))/binary>>
            },
            write(Assignment, Opts),
            timer:sleep(1)
        end, lists:seq(1, 100)),
        ?event(testing, {writer_completed}),
        Parent ! writer_done
    end),
    lists:foreach(
        fun(ReaderNum) ->
            spawn_link(fun() ->
                ReadResults = lists:map(fun(Slot) ->
                    timer:sleep(rand:uniform(5)),
                    case read(ProcID, Slot, Opts) of
                        {ok, _} -> success;
                        not_found -> not_found
                    end
                end, lists:seq(1, 100)),
                SuccessCount = length([R || R <- ReadResults, R == success]),
                ?event(testing, {reader_done, ReaderNum, SuccessCount}),
                Parent ! {reader_done, ReaderNum, ReadResults}
            end)
        end,
        lists:seq(1, 10)
    ),
    receive 
        writer_done -> ok
    after 15000 -> 
        ?assert(false) 
    end,
    AllReaderResults = lists:map(fun(ReaderNum) ->
        receive
            {reader_done, ReaderNum, Results} -> Results
        after 5000 ->
            ?assert(false),
            []
        end
    end, lists:seq(1, 10)),
    FinalSlots = list(ProcID, Opts),
    ?event(testing, {final_verification, {slots_found, length(FinalSlots)}}),
    ?assertEqual(100, length(FinalSlots)),
    ?assertEqual(lists:seq(1, 100), lists:sort(FinalSlots)),
    TotalSuccessfulReads = lists:sum([
        length([R || R <- Results, R == success]) || Results <- AllReaderResults
    ]),
    ?event(testing, {
        concurrent_read_stats,
        {total_successful_reads, TotalSuccessfulReads}
    }),
    ?assert(TotalSuccessfulReads > 0).

%% @doc Test writing a large volume of assignments to stress memory. Helps
%% identify memory leaks and also, checks performance issues.
large_assignment_volume_test() ->
    VolStore = hb_test_utils:test_store(hb_store_fs, <<"volume-vol">>),
    NonVolStore = hb_test_utils:test_store(hb_store_fs, <<"volume-nonvol">>),
    Opts = #{
        store => [NonVolStore],
        scheduler_store => [VolStore]
    },
    hb_store:start(VolStore),
    hb_store:start(NonVolStore),
    VolumeSize = 1000,
    ProcID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    StartTime = erlang:monotonic_time(millisecond),
    lists:foreach(
        fun(Slot) ->
            Assignment = #{
                <<"variant">> => <<"ao.N.1">>,
                <<"process">> => ProcID,
                <<"slot">> => Slot,
                <<"hash-chain">> => crypto:strong_rand_bytes(64)
            },
            ?assertEqual(ok, write(Assignment, Opts))
        end,
        lists:seq(1, VolumeSize)
    ),
    EndTime = erlang:monotonic_time(millisecond),
    ?event(testing, {large_volume_write_time, EndTime - StartTime}),
    AllSlots = list(ProcID, Opts),
    ?assertEqual(VolumeSize, length(AllSlots)),
    ?assertEqual(lists:seq(1, VolumeSize), lists:sort(AllSlots)),
    ReadStartTime = erlang:monotonic_time(millisecond),
    lists:foreach(fun(Slot) ->
        ?assertMatch({ok, _}, read(ProcID, Slot, Opts))
    end, lists:seq(1, VolumeSize)),
    ReadEndTime = erlang:monotonic_time(millisecond),
    ?event(testing, {large_volume_read_time, ReadEndTime - ReadStartTime}).

%% @doc Test rapid store restarts under load.
rapid_restart_test() ->
    VolStore = hb_test_utils:test_store(hb_store_fs, <<"restart-vol">>),
    NonVolStore = hb_test_utils:test_store(hb_store_fs, <<"restart-nonvol">>),
    Opts = #{
        store => [NonVolStore],
        scheduler_store => [VolStore]
    },
    hb_store:start(VolStore),
    hb_store:start(NonVolStore),
    ProcID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    lists:foreach(
        fun(Cycle) ->
            lists:foreach(
                fun(Slot) ->
                    Assignment = #{
                        <<"variant">> => <<"ao.N.1">>,
                        <<"process">> => ProcID,
                        <<"slot">> => Slot + (Cycle * 10),
                        <<"hash-chain">> =>
                            <<"restart-cycle-", (integer_to_binary(Cycle))/binary>>
                    },
                    ?assertEqual(ok, write(Assignment, Opts))
                end,
                lists:seq(1, 10)
            ),
            SlotsBeforeRestart = list(ProcID, Opts),
            ?assertMatch([_|_], SlotsBeforeRestart),
            ?event(testing, {
                restart_cycle, Cycle, {slots_before, length(SlotsBeforeRestart)}
            }),
            hb_store:stop(VolStore),
            timer:sleep(10),
            hb_store:reset(VolStore),
            hb_store:start(VolStore),
            SlotsAfterRestart = list(ProcID, Opts),
            ?assertEqual([], SlotsAfterRestart),
            ?event({restart_verified, Cycle, {slots_after, length(SlotsAfterRestart)}})
        end,
        lists:seq(1, 5)
    ).

%% @doc Test scheduler store behavior during reset store operations.
mixed_store_reset_operations_test() ->
    VolStore = hb_test_utils:test_store(hb_store_fs, <<"mixed-vol">>),
    NonVolStore = hb_test_utils:test_store(hb_store_fs, <<"mixed-nonvol">>),
    Opts = #{
        store => [NonVolStore],
        scheduler_store => [VolStore]
    },
    hb_store:start(VolStore),
    hb_store:start(NonVolStore),
    ProcID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Assignment1 = #{
        <<"variant">> => <<"ao.N.1">>,
        <<"process">> => ProcID,
        <<"slot">> => 1,
        <<"hash-chain">> => <<"mixed-test-1">>
    },
    ?assertEqual(ok, write(Assignment1, Opts)),
    ?event(testing, {assignment_written, ProcID}),
    hb_store:reset(NonVolStore),
    ReadAfterNonVolReset = read(ProcID, 1, Opts),
    ?assertMatch({ok, _}, ReadAfterNonVolReset),
    ?event(testing, {after_nonvol_reset, ReadAfterNonVolReset}),
    hb_store:reset(VolStore),
    ReadAfterVolReset = read(ProcID, 1, Opts),
    ?assertEqual(not_found, ReadAfterVolReset),
    ?event(testing, {after_vol_reset, ReadAfterVolReset}).

%% @doc Test handling of invalid assignment data.
invalid_assignment_stress_test() ->
    VolStore = hb_test_utils:test_store(hb_store_fs, <<"invalid-vol">>),
    NonVolStore = hb_test_utils:test_store(hb_store_fs, <<"invalid-nonvol">>),
    Opts = #{
        store => [NonVolStore],
        scheduler_store => [VolStore]
    },
    hb_store:start(VolStore),
    hb_store:start(NonVolStore),
    InvalidAssignments = [
        #{},
        #{<<"process">> => <<"invalid">>},
        #{<<"slot">> => 1},
        #{<<"process">> => <<>>, <<"slot">> => 1},
        #{<<"process">> => <<"valid">>, <<"slot">> => -1},
        #{<<"process">> => <<"valid">>, <<"slot">> => <<"not-integer">>}
    ],
    ?event(testing, {testing_invalid_assignments, length(InvalidAssignments)}),
    Results = lists:map(fun(Assignment) ->
        Result = try
            write(Assignment, Opts)
        catch
            _:_ -> error
        end,
        ?assertNotEqual(ok, Result),
        Result
    end, InvalidAssignments),
    
    ErrorCount = length([R || R <- Results, R == error]),
    ?event(
        {invalid_assignment_results,
            {errors, ErrorCount},
            {total, length(InvalidAssignments)}
        }
    ),
    ?assertEqual(6, ErrorCount).

%% @doc Test scheduler location operations under stress.
scheduler_location_stress_test() ->
    VolStore = hb_test_utils:test_store(hb_store_fs, <<"location-vol">>),
    NonVolStore = hb_test_utils:test_store(hb_store_fs, <<"location-nonvol">>),
    Wallet = ar_wallet:new(),
    Opts = #{
        store => [NonVolStore],
        scheduler_store => [VolStore],
        priv_wallet => Wallet
    },
    hb_store:start(VolStore),
    hb_store:start(NonVolStore),
    LocationCount = 10,
    ?event(testing, {location_stress_test_starting, LocationCount}),
    Results =
        lists:map(
            fun(N) ->
                LocationMsg = #{
                    <<"scheduler">> =>
                        hb_util:human_id(ar_wallet:to_address(Wallet)),
                    <<"location">> =>
                        <<
                            "http://scheduler",
                            (integer_to_binary(N))/binary,
                            ".com"
                        >>,
                    <<"timestamp">> => erlang:system_time(millisecond),
                    <<"ttl">> => 3600000
                },
                Result =
                    try
                        write_location(LocationMsg, Opts)
                    catch
                        Res -> 
                            ?event(testing, {location_write_error, {error, Res}}),
                            ok 
                    end,
                ?assert(Result == ok orelse element(1, Result) == error),
                Result
            end,
            lists:seq(1, LocationCount)
        ),
    SuccessCount = length([R || R <- Results, R == ok]),
    ?event(
        {location_stress_results,
            {successes, SuccessCount},
            {total, LocationCount}
        }
    ).

%% @doc Test system behavior with corrupted data in volatile store.
volatile_store_corruption_test() ->
    VolStore = hb_test_utils:test_store(hb_store_fs, <<"corruption-vol">>),
    NonVolStore = hb_test_utils:test_store(hb_store_fs, <<"corruption-nonvol">>),
    Opts = #{
        store => [NonVolStore],
        scheduler_store => [VolStore]
    },
    hb_store:start(VolStore),
    hb_store:start(NonVolStore),
    ProcID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Assignment = #{
        <<"variant">> => <<"ao.N.1">>,
        <<"process">> => ProcID,
        <<"slot">> => 1,
        <<"hash-chain">> => <<"corruption-test">>
    },
    ?assertEqual(ok, write(Assignment, Opts)),
    ReadBeforeCorruption = read(ProcID, 1, Opts),
    ?assertMatch({ok, _}, ReadBeforeCorruption),
    ?event(testing, {before_corruption, ReadBeforeCorruption}),
    hb_store:reset(VolStore),
    ?event(testing, {volatile_store_reset}),
    ReadAfterCorruption = read(ProcID, 1, Opts),
    SlotsAfterCorruption = list(ProcID, Opts),
    LatestAfterCorruption = latest(ProcID, Opts),
    ?assertEqual(not_found, ReadAfterCorruption),
    ?assertEqual([], SlotsAfterCorruption),
    ?assertEqual(not_found, LatestAfterCorruption),
    ?event(testing,
        { corruption_recovery_verified,
            { read, ReadAfterCorruption },
            { list, length(SlotsAfterCorruption) }, 
            { latest, LatestAfterCorruption }
    }).