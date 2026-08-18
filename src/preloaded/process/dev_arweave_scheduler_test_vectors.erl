%%% @doc Public integration tests for `~arweave-scheduler@1.0'.
%%%
%%% Every scheduler operation goes through `hb_ao'. Consensus fixtures use the
%%% same block-index device and publication boundary as a running
%%% `~arweave@2.9' node, with consensus and scheduler state held in distinct
%%% LMDB stores.
-module(dev_arweave_scheduler_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SCHEDULER, #{ <<"device">> => <<"arweave-scheduler@1.0">> }).
-define(TIP_PATH, <<"~arweave@2.9/tip">>).

%%% Tests.

%% @doc The last admitted height is exactly tip minus checkpoint depth, even
%% when the following validated block is already locally available.
settled_depth_is_exact_test() ->
    with_fixture(
        <<"settled-depth">>,
        #{},
        fun(Opts, _ConsensusStore, _SchedulerStore) ->
            TipHeight = ?CHECKPOINT_DEPTH + 2,
            StableHeight = TipHeight - ?CHECKPOINT_DEPTH,
            Hashes = test_hashes(<<"settled-depth">>, TipHeight),
            lists:foreach(
                fun(Height) ->
                    publish_block(
                        Hashes,
                        Height,
                        [],
                        [],
                        [<<"transactions">>],
                        Opts
                    )
                end,
                lists:seq(1, StableHeight + 1)
            ),
            TipHash = publish_tip(Hashes, TipHeight, Opts),
            {ok, State} = scheduler(#{ <<"path">> => <<"sync">> }, Opts),
            ?assertEqual(
                StableHeight,
                hb_util:int(hb_maps:get(<<"to">>, State, -1, Opts))
            ),
            ?assertEqual(
                TipHash,
                hb_maps:get(<<"tip-hash">>, State, not_found, Opts)
            ),
            ?assertEqual(
                StableHeight,
                global_to(scheduler_status(Opts), Opts)
            )
        end
    ).

%% @doc A restarted scheduler LMDB serves the same fully materialized spawn
%% and targeted transaction assignments, including both signed headers.
scheduler_restart_preserves_complete_schedule_test() ->
    with_fixture(
        <<"restart-schedule">>,
        #{},
        fun(Opts, _ConsensusStore, SchedulerStore) ->
            TipHeight = ?CHECKPOINT_DEPTH + 2,
            Hashes = test_hashes(<<"restart-schedule">>, TipHeight),
            Spawn = test_transaction(<<>>),
            ProcessID = transaction_id(Spawn),
            Targeted = test_transaction(hb_util:native_id(ProcessID)),
            publish_block(
                Hashes,
                1,
                [Spawn],
                [Spawn],
                [<<"transactions">>],
                Opts
            ),
            publish_block(
                Hashes,
                2,
                [Targeted],
                [Targeted],
                [<<"transactions">>],
                Opts
            ),
            publish_tip(Hashes, TipHeight, Opts),
            Before = scheduler_schedule(ProcessID, 1, Opts),
            assert_schedule(
                Before,
                ProcessID,
                [transaction_id(Spawn), transaction_id(Targeted)],
                Opts
            ),
            ok = hb_store:stop(SchedulerStore),
            ok = hb_store:start(SchedulerStore),
            After = scheduler_schedule(ProcessID, 1, Opts),
            assert_schedule(
                After,
                ProcessID,
                [transaction_id(Spawn), transaction_id(Targeted)],
                Opts
            ),
            ?assertEqual(
                TipHeight - ?CHECKPOINT_DEPTH,
                global_to(scheduler_status(Opts), Opts)
            )
        end
    ).

%% @doc A placement alias from another chain is rejected before process state
%% is written. Re-publishing the selected placement then succeeds immediately.
stale_placement_does_not_poison_process_test() ->
    with_fixture(
        <<"stale-placement">>,
        #{},
        fun(Opts, _ConsensusStore, _SchedulerStore) ->
            TipHeight = ?CHECKPOINT_DEPTH + 1,
            Hashes = test_hashes(<<"stale-placement">>, TipHeight),
            Spawn = test_transaction(<<>>),
            ProcessID = transaction_id(Spawn),
            publish_block(
                Hashes,
                1,
                [Spawn],
                [Spawn],
                [<<"transactions">>],
                Opts
            ),
            publish_tip(Hashes, TipHeight, Opts),
            {ok, Initial} = scheduler(#{ <<"path">> => <<"sync">> }, Opts),
            {ok, _} =
                publish_placements(
                    [
                        test_placement(
                            Spawn,
                            test_hash(<<"other-chain">>, 1),
                            1,
                            0
                        )
                    ],
                    Opts
                ),
            ?assertMatch(
                {error, #{ <<"message">> := <<"stale-placement">> }},
                scheduler_schedule_result(ProcessID, 0, Opts)
            ),
            ?assertEqual(Initial, scheduler_global(scheduler_status(Opts), Opts)),
            {ok, _} =
                publish_placements(
                    [test_placement(Spawn, maps:get(1, Hashes), 1, 0)],
                    Opts
                ),
            Schedule = scheduler_schedule(ProcessID, 0, Opts),
            assert_schedule(Schedule, ProcessID, [ProcessID], Opts),
            ?assertEqual(Initial, scheduler_global(scheduler_status(Opts), Opts))
        end
    ).

%% @doc A selected index entry with no validated block, and then with only an
%% archive header, both fail closed without moving the durable frontier.
missing_validated_or_archive_history_keeps_frontier_test() ->
    with_fixture(
        <<"missing-history">>,
        #{},
        fun(Opts, _ConsensusStore, _SchedulerStore) ->
            FirstTipHeight = ?CHECKPOINT_DEPTH + 1,
            Hashes = test_hashes(<<"missing-history">>, FirstTipHeight + 1),
            publish_block(
                Hashes,
                1,
                [],
                [],
                [<<"transactions">>],
                Opts
            ),
            publish_tip(Hashes, FirstTipHeight, Opts),
            {ok, Initial} = scheduler(#{ <<"path">> => <<"sync">> }, Opts),
            publish_tip(Hashes, FirstTipHeight + 1, Opts),
            assert_history_missing(scheduler(#{ <<"path">> => <<"sync">> }, Opts)),
            ?assertEqual(Initial, scheduler_global(scheduler_status(Opts), Opts)),
            publish_block(
                Hashes,
                2,
                [],
                [],
                [<<"identity">>],
                Opts
            ),
            assert_history_missing(scheduler(#{ <<"path">> => <<"sync">> }, Opts)),
            ?assertEqual(Initial, scheduler_global(scheduler_status(Opts), Opts))
        end
    ).

%% @doc A two-block batch whose final placement cannot load its committed
%% transaction publishes no frontier. Publishing that transaction makes the
%% identical public retry advance the whole batch.
partial_batch_retries_without_frontier_hole_test() ->
    with_fixture(
        <<"partial-batch">>,
        #{ <<"arweave-scheduler-block-batch">> => 2 },
        fun(Opts, _ConsensusStore, _SchedulerStore) ->
            FirstTipHeight = ?CHECKPOINT_DEPTH + 1,
            Hashes = test_hashes(<<"partial-batch">>, FirstTipHeight + 2),
            publish_block(
                Hashes,
                1,
                [],
                [],
                [<<"transactions">>],
                Opts
            ),
            publish_tip(Hashes, FirstTipHeight, Opts),
            {ok, Initial} = scheduler(#{ <<"path">> => <<"sync">> }, Opts),
            InSecond = test_transaction(crypto:strong_rand_bytes(32)),
            FirstInThird = test_transaction(crypto:strong_rand_bytes(32)),
            Missing = test_transaction(crypto:strong_rand_bytes(32)),
            publish_block(
                Hashes,
                2,
                [InSecond],
                [InSecond],
                [<<"transactions">>],
                Opts
            ),
            publish_block(
                Hashes,
                3,
                [FirstInThird, Missing],
                [FirstInThird],
                [<<"transactions">>],
                Opts
            ),
            publish_tip(Hashes, FirstTipHeight + 2, Opts),
            ?assertMatch(
                {error,
                    #{ <<"message">> := <<"invalid-placement-transaction">> }},
                scheduler(#{ <<"path">> => <<"sync">> }, Opts)
            ),
            ?assertEqual(Initial, scheduler_global(scheduler_status(Opts), Opts)),
            {ok, _} = hb_cache:write(transaction_message(Missing, Opts), Opts),
            {ok, Retried} = scheduler(#{ <<"path">> => <<"sync">> }, Opts),
            ?assertEqual(3, global_to(Retried, Opts)),
            ?assertEqual(Retried, scheduler_global(scheduler_status(Opts), Opts))
        end
    ).

%% @doc A frontier records the tip snapshot that selected it. Replacing the
%% public tip with a fork that rewrites the frontier height is rejected, and
%% the previously captured frontier remains the public scheduler status.
frontier_remains_bound_to_captured_tip_test() ->
    with_fixture(
        <<"captured-tip">>,
        #{},
        fun(Opts, _ConsensusStore, _SchedulerStore) ->
            TipHeight = ?CHECKPOINT_DEPTH + 2,
            Main = test_hashes(<<"captured-main">>, TipHeight),
            publish_block(
                Main,
                1,
                [],
                [],
                [<<"transactions">>],
                Opts
            ),
            publish_block(
                Main,
                2,
                [],
                [],
                [<<"transactions">>],
                Opts
            ),
            MainTip = publish_tip(Main, TipHeight, Opts),
            {ok, Captured} = scheduler(#{ <<"path">> => <<"sync">> }, Opts),
            ?assertEqual(
                MainTip,
                hb_maps:get(<<"tip-hash">>, Captured, not_found, Opts)
            ),
            Fork = maps:merge(Main, test_hashes(<<"captured-fork">>, TipHeight)),
            publish_block(
                Fork,
                2,
                [],
                [],
                [<<"transactions">>],
                Opts
            ),
            publish_tip(Fork, TipHeight, Opts),
            ?assertMatch(
                {error, #{ <<"message">> := <<"frontier-conflict">> }},
                scheduler(#{ <<"path">> => <<"sync">> }, Opts)
            ),
            ?assertEqual(Captured, scheduler_global(scheduler_status(Opts), Opts))
        end
    ).

%% @doc A durable process frontier is not trusted unless its owner and schema
%% agree with the process requested through the public scheduler.
malformed_process_frontier_is_rejected_test() ->
    with_fixture(
        <<"malformed-process-frontier">>,
        #{},
        fun(Opts, _ConsensusStore, _SchedulerStore) ->
            TipHeight = ?CHECKPOINT_DEPTH + 1,
            Hashes = test_hashes(<<"malformed-process-frontier">>, TipHeight),
            Spawn = test_transaction(<<>>),
            ProcessID = transaction_id(Spawn),
            publish_block(
                Hashes,
                1,
                [Spawn],
                [Spawn],
                [<<"transactions">>],
                Opts
            ),
            publish_tip(Hashes, TipHeight, Opts),
            {ok, _} = scheduler(#{ <<"path">> => <<"sync">> }, Opts),
            {ok, _} =
                lib_arweave_scheduler_cache:write_process(
                    ProcessID,
                    #{
                        <<"process">> => transaction_id(test_transaction(<<>>)),
                        <<"spawn-ordinate">> => <<"1-0">>,
                        <<"synced-to">> => 0,
                        <<"next-slot">> => 1
                    },
                    Opts
                ),
            ?assertMatch(
                {error, #{ <<"message">> := <<"invalid-process-frontier">> }},
                scheduler_schedule_result(ProcessID, 0, Opts)
            )
        end
    ).

%% @doc A process frontier exposes a dense interval. If one durable slot inside
%% it cannot be read, the public schedule fails instead of returning a prefix.
missing_dense_assignment_is_rejected_test() ->
    with_fixture(
        <<"missing-dense-assignment">>,
        #{},
        fun(Opts, _ConsensusStore, _SchedulerStore) ->
            TipHeight = ?CHECKPOINT_DEPTH + 2,
            Hashes = test_hashes(<<"missing-dense-assignment">>, TipHeight),
            Spawn = test_transaction(<<>>),
            ProcessID = transaction_id(Spawn),
            Targeted = test_transaction(hb_util:native_id(ProcessID)),
            publish_block(
                Hashes,
                1,
                [Spawn],
                [Spawn],
                [<<"transactions">>],
                Opts
            ),
            publish_block(
                Hashes,
                2,
                [Targeted],
                [Targeted],
                [<<"transactions">>],
                Opts
            ),
            publish_tip(Hashes, TipHeight, Opts),
            assert_schedule(
                scheduler_schedule(ProcessID, 1, Opts),
                ProcessID,
                [ProcessID, transaction_id(Targeted)],
                Opts
            ),
            SchedulerOpts = lib_arweave_scheduler_cache:opts(Opts),
            MissingID = hb_util:human_id(crypto:strong_rand_bytes(32)),
            ok =
                hb_store:link(
                    hb_opts:get(store, no_viable_store, SchedulerOpts),
                    #{ assignment_path(ProcessID, 1) => MissingID },
                    SchedulerOpts
                ),
            ?assertMatch(
                {error,
                    #{
                        <<"reason">> :=
                            <<"Scheduler assignment frontier is incomplete.">>,
                        <<"slot">> := 1
                    }},
                scheduler_schedule_result(ProcessID, 1, Opts)
            )
        end
    ).

%% @doc Unknown extension methods return a public 405 without creating atoms
%% or requiring scheduler state.
unknown_method_returns_405_test() ->
    ?assertMatch(
        {error, #{ <<"status">> := 405 }},
        hb_ao:resolve(
            ?SCHEDULER,
            #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"X-ARWEAVE-SCHEDULER-UNKNOWN">>
            },
            #{ <<"priv-wallet">> => hb:wallet() }
        )
    ).

%% @doc Status validates its store boundary and propagates malformed durable
%% global state instead of reporting an empty successful scheduler.
status_rejects_store_and_global_errors_test() ->
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-scheduler-store">> }},
        scheduler(#{ <<"path">> => <<"status">> }, #{})
    ),
    with_fixture(
        <<"invalid-global-frontier">>,
        #{},
        fun(Opts, _ConsensusStore, _SchedulerStore) ->
            {ok, _} =
                lib_arweave_scheduler_cache:publish_global(
                    #{ <<"from">> => 1, <<"to">> => <<"bad">> },
                    [],
                    Opts
                ),
            assert_invalid_global(
                scheduler(#{ <<"path">> => <<"sync">> }, Opts)
            ),
            assert_invalid_global(
                scheduler(#{ <<"path">> => <<"status">> }, Opts)
            ),
            {ok, _} =
                lib_arweave_scheduler_cache:publish_global(
                    #{
                        <<"from">> => 1,
                        <<"to">> => 1,
                        <<"block-hash">> => test_hash(<<"block">>, 1),
                        <<"tip-hash">> => test_hash(<<"tip">>, 1),
                        <<"tip-height">> => <<"bad">>
                    },
                    [],
                    Opts
                ),
            assert_invalid_global(
                scheduler(#{ <<"path">> => <<"sync">> }, Opts)
            ),
            assert_invalid_global(
                scheduler(#{ <<"path">> => <<"status">> }, Opts)
            )
        end
    ).

%% @doc A target alias written before its frontier cannot become an assignment
%% if the selected chain replaces that target's block before the retry.
orphaned_future_target_is_not_scheduled_test() ->
    with_fixture(
        <<"orphaned-future-target">>,
        #{},
        fun(Opts, _ConsensusStore, _SchedulerStore) ->
            FirstTipHeight = ?CHECKPOINT_DEPTH + 1,
            Hashes =
                test_hashes(
                    <<"orphaned-future-target">>,
                    FirstTipHeight + 1
                ),
            Spawn = test_transaction(<<>>),
            ProcessID = transaction_id(Spawn),
            publish_block(
                Hashes,
                1,
                [Spawn],
                [Spawn],
                [<<"transactions">>],
                Opts
            ),
            publish_tip(Hashes, FirstTipHeight, Opts),
            {ok, Initial} = scheduler(#{ <<"path">> => <<"sync">> }, Opts),
            Orphan = test_transaction(hb_util:native_id(ProcessID)),
            OrphanID = transaction_id(Orphan),
            OrphanHash = test_hash(<<"orphaned-block">>, 2),
            {ok, _} =
                lib_arweave_scheduler_cache:write_header(
                    OrphanID,
                    transaction_message(Orphan, Opts),
                    Opts
                ),
            {ok, _} =
                lib_arweave_scheduler_cache:publish_global(
                    Initial,
                    [{ProcessID, <<"2-0">>, OrphanHash, OrphanID}],
                    Opts
                ),
            publish_block(
                Hashes,
                2,
                [],
                [],
                [<<"transactions">>],
                Opts
            ),
            publish_tip(Hashes, FirstTipHeight + 1, Opts),
            Schedule = scheduler_schedule(ProcessID, 1, Opts),
            assert_schedule(Schedule, ProcessID, [ProcessID], Opts),
            ?assertEqual(2, global_to(scheduler_status(Opts), Opts))
        end
    ).

%% @doc A transaction may name an application object through an ordinary AO
%% link without making that object's contents part of scheduler persistence.
linked_transaction_tag_is_preserved_test() ->
    with_fixture(
        <<"linked-transaction-tag">>,
        #{},
        fun(Opts, _ConsensusStore, SchedulerStore) ->
            TipHeight = ?CHECKPOINT_DEPTH + 1,
            Hashes = test_hashes(<<"linked-transaction-tag">>, TipHeight),
            Spawn = test_transaction(<<>>),
            ProcessID = transaction_id(Spawn),
            MissingLink = hb_util:human_id(crypto:strong_rand_bytes(32)),
            TX =
                test_transaction(
                    hb_util:native_id(ProcessID),
                    [{<<"Device-Stack+link">>, MissingLink}]
                ),
            publish_block(
                Hashes,
                1,
                [Spawn, TX],
                [Spawn, TX],
                [<<"transactions">>],
                Opts
            ),
            publish_tip(Hashes, TipHeight, Opts),
            Schedule = scheduler_schedule(ProcessID, 1, Opts),
            assert_schedule(
                Schedule,
                ProcessID,
                [ProcessID, transaction_id(TX)],
                Opts
            ),
            [_SpawnAssignment, Assignment] =
                hb_util:message_to_ordered_list(
                    hb_maps:get(<<"assignments">>, Schedule, [], Opts),
                    Opts
                ),
            Body = hb_maps:get(<<"body">>, Assignment, not_found, Opts),
            ?assertMatch({link, _, _}, maps:get(<<"device-stack">>, Body)),
            ?assertEqual(
                {error, not_found},
                hb_cache:read(MissingLink, Opts#{ <<"store">> => [SchedulerStore] })
            )
        end
    ).

%%% Public calls and assertions.

%% @doc Resolve one public scheduler request without resolver result caching.
scheduler(Req, Opts) ->
    hb_ao:resolve(?SCHEDULER, Req, mutable_opts(Opts)).

%% @doc Return the public scheduler status record.
scheduler_status(Opts) ->
    {ok, Status} = scheduler(#{ <<"path">> => <<"status">> }, Opts),
    Status.

%% @doc Resolve a complete schedule and return its successful body.
scheduler_schedule(ProcessID, To, Opts) ->
    {ok, Schedule} = scheduler_schedule_result(ProcessID, To, Opts),
    Schedule.

%% @doc Resolve a process schedule through the public scheduler device.
scheduler_schedule_result(ProcessID, To, Opts) ->
    scheduler(
        #{
            <<"path">> => <<"schedule">>,
            <<"target">> => ProcessID,
            <<"from">> => 0,
            <<"to">> => To
        },
        Opts
    ).

%% @doc Read the durable global record from a public status response.
scheduler_global(Status, Opts) ->
    hb_maps:get(<<"sync">>, Status, not_found, Opts).

%% @doc Read the durable covered height from a state or status response.
global_to(State, Opts) ->
    Global =
        case hb_maps:get(<<"sync">>, State, not_found, Opts) of
            not_found -> State;
            Sync -> Sync
        end,
    hb_util:int(hb_maps:get(<<"to">>, Global, -1, Opts)).

%% @doc Assert every public assignment carries the expected complete header.
assert_schedule(Schedule, ProcessID, ExpectedTXIDs, Opts) ->
    ?assertEqual(
        ProcessID,
        hb_maps:get(<<"process">>, Schedule, not_found, Opts)
    ),
    Assignments =
        hb_util:message_to_ordered_list(
            hb_maps:get(<<"assignments">>, Schedule, [], Opts),
            Opts
        ),
    ?assertEqual(length(ExpectedTXIDs), length(Assignments)),
    ?assertEqual(
        ExpectedTXIDs,
        [
            hb_util:human_id(
                hb_message:id(
                    hb_maps:get(<<"body">>, Assignment, not_found, Opts),
                    signed,
                    Opts
                )
            )
        ||
            Assignment <- Assignments
        ]
    ).

%% @doc Assert the public error names absent transaction-complete history.
assert_history_missing(Result) ->
    ?assertMatch(
        {error, #{ <<"message">> := <<"history-not-materialized">> }},
        Result
    ).

%% @doc Assert public rejection of malformed durable scheduler state.
assert_invalid_global(Result) ->
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-global-frontier">> }},
        Result
    ).

%% @doc Disable resolver caching for every mutable public alias in a test.
mutable_opts(Opts) ->
    Opts#{
        <<"hashpath">> => ignore,
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>]
    }.

%%% Consensus fixtures.

%% @doc Run a test with independent real LMDB consensus and scheduler stores.
with_fixture(Tag, ExtraOpts, Test) ->
    ConsensusStore =
        hb_test_utils:test_store(
            hb_store_lmdb,
            <<"ar-scheduler-consensus-", Tag/binary>>
        ),
    SchedulerStore =
        hb_test_utils:test_store(
            hb_store_lmdb,
            <<"ar-scheduler-derived-", Tag/binary>>
        ),
    ok = hb_store:start(ConsensusStore),
    ok = hb_store:start(SchedulerStore),
    Opts =
        maps:merge(
            #{
                <<"store">> => [ConsensusStore],
                <<"scheduler-store">> => [SchedulerStore],
                <<"priv-wallet">> => hb:wallet(),
                <<"arweave-scheduler-from">> => 1,
                <<"arweave-scheduler-block-workers">> => 1,
                <<"arweave-scheduler-header-workers">> => 1
            },
            ExtraOpts
        ),
    try Test(Opts, ConsensusStore, SchedulerStore)
    after
        catch hb_store:stop(ConsensusStore),
        catch hb_store:stop(SchedulerStore)
    end.

%% @doc Return one deterministic, protocol-shaped block hash.
test_hash(Tag, Height) ->
    hb_util:encode(
        crypto:hash(
            sha384,
            <<Tag/binary, ":", (integer_to_binary(Height))/binary>>
        )
    ).

%% @doc Return the block hash selected at every height through a tip.
test_hashes(Tag, TipHeight) ->
    maps:from_list(
        [ {Height, test_hash(Tag, Height)} || Height <- lists:seq(0, TipHeight) ]
    ).

%% @doc Build a linked block-index state through its public AO interface.
test_index(Hashes, TipHeight, Opts) ->
    Entries =
        hb_util:list_to_numbered_message(
            [
                #{
                    <<"indep-hash">> => maps:get(Height, Hashes),
                    <<"weave-size">> => (Height + 1) * 262144,
                    <<"tx-root">> =>
                        hb_util:encode(
                            crypto:hash(
                                sha256,
                                maps:get(Height, Hashes)
                            )
                        )
                }
            ||
                Height <- lists:seq(0, TipHeight)
            ]
        ),
    {ok, Index} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-block-index@2.9">> },
            #{
                <<"path">> => <<"append">>,
                <<"entries">> => Entries,
                <<"start-height">> => 0
            },
            Opts
        ),
    Index.

%% @doc Publish the selected tip carrying the exact index the scheduler reads.
publish_tip(Hashes, TipHeight, Opts) ->
    Hash = maps:get(TipHeight, Hashes),
    Index = test_index(Hashes, TipHeight, Opts),
    {ok, _} =
        publish_consensus(
            #{
                <<"device">> => <<"arweave-block@2.9">>,
                <<"indep-hash">> => Hash,
                <<"height">> => TipHeight,
                <<"previous-block">> => maps:get(TipHeight - 1, Hashes),
                <<"block-index">> => Index,
                <<"txs">> => [],
                <<"transactions">> => [],
                <<"validation">> =>
                    #{ <<"checks">> => [<<"transactions">>] }
            },
            Hash,
            [],
            Opts
        ),
    ok = hb_cache:link(Hash, ?TIP_PATH, Opts),
    Hash.

%% @doc Publish one locally validated block and its requested transaction set.
publish_block(Hashes, Height, Records, PublishedRecords, Checks, Opts) ->
    Hash = maps:get(Height, Hashes),
    Placements =
        [
            test_placement(Record, Hash, Height, Position)
        ||
            {Position, Record} <-
                lists:zip(lists:seq(0, length(Records) - 1), Records)
        ],
    {ok, _} =
        publish_consensus(
            #{
                <<"device">> => <<"arweave-block@2.9">>,
                <<"indep-hash">> => Hash,
                <<"height">> => Height,
                <<"previous-block">> => maps:get(Height - 1, Hashes),
                <<"txs">> => [ transaction_id(TX) || TX <- Records ],
                <<"transactions">> => Placements,
                <<"validation">> => #{ <<"checks">> => Checks }
            },
            Hash,
            [ transaction_message(TX, Opts) || TX <- PublishedRecords ],
            Opts
        ),
    Hash.

%% @doc Publish a fixture with the same transaction, placement, block and
%% block-hash commit-marker order as consensus publication.
publish_consensus(Block, Hash, Transactions, Opts) ->
    lists:foreach(
        fun(Transaction) -> {ok, _} = hb_cache:write(Transaction, Opts) end,
        Transactions
    ),
    {ok, PlacementLinks} =
        publish_placements(
            hb_util:message_to_ordered_list(
                hb_maps:get(<<"transactions">>, Block, [], Opts),
                Opts
            ),
            Opts
        ),
    {ok, ID} = hb_cache:write(Block#{ <<"transactions">> => PlacementLinks }, Opts),
    ok = hb_cache:link(ID, Hash, Opts),
    {ok, _} = hb_cache:read(Hash, Opts),
    {ok, ID}.

%% @doc Publish placement messages and their mutable transaction aliases.
publish_placements(Placements, Opts) ->
    publish_placements(Placements, [], Opts).

publish_placements([], Links, _Opts) ->
    {ok, lists:reverse(Links)};
publish_placements([Placement | Rest], Links, Opts) ->
    TXID = hb_maps:get(<<"id">>, Placement, not_found, Opts),
    {ok, ID} = hb_cache:write(Placement, Opts),
    ok = hb_cache:link(ID, placement_path(TXID), Opts),
    {ok, _} = hb_cache:read(placement_path(TXID), Opts),
    publish_placements(
        Rest,
        [{link, ID, #{ <<"type">> => <<"link">>, <<"lazy">> => false }} | Links],
        Opts
    ).

%% @doc Return the canonical consensus placement alias for a transaction.
placement_path(TXID) ->
    hb_path:to_binary([<<"~arweave@2.9">>, <<"placements">>, TXID]).

%% @doc Return the scheduler's durable assignment slot alias.
assignment_path(ProcessID, Slot) ->
    hb_path:to_binary(
        [
            <<"~arweave-scheduler@1.0">>,
            <<"assignments">>,
            ProcessID,
            hb_ao:normalize_key(Slot)
        ]
    ).

%% @doc Return a real signed data-free Arweave transaction.
test_transaction(Target) ->
    test_transaction(Target, []).

%% @doc Return a real signed data-free transaction with the requested tags.
test_transaction(Target, Tags) ->
    ar_tx:sign(
        #tx{
            format = 2,
            anchor = crypto:strong_rand_bytes(32),
            target = Target,
            reward = 1,
            tags = Tags
        },
        hb:wallet()
    ).

%% @doc Return a transaction's canonical signed identifier.
transaction_id(TX) ->
    hb_util:human_id(TX#tx.id).

%% @doc Convert a transaction to the committed message consensus publishes.
transaction_message(TX, Opts) ->
    hb_message:convert(TX, <<"structured@1.0">>, <<"tx@1.0">>, Opts).

%% @doc Build the canonical placement a locally validated block publishes.
test_placement(TX, Hash, Height, Position) ->
    ID = transaction_id(TX),
    #{
        <<"id">> => ID,
        <<"block">> => Hash,
        <<"height">> => Height,
        <<"position">> => Position,
        <<"data-root">> => hb_util:encode(TX#tx.data_root),
        <<"data-size">> => TX#tx.data_size,
        <<"start-offset">> => 0,
        <<"transaction">> =>
            {link, ID, #{ <<"type">> => <<"link">>, <<"lazy">> => false }}
    }.
