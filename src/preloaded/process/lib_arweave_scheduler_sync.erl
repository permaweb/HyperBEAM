%%% @doc Derive `~arweave-scheduler@1.0' assignments from the locally
%%% validated Arweave chain.
%%%
%%% Consensus owns peer I/O, fork choice, validation, blocks, transactions and
%%% placements. This library captures one public `~arweave@2.9' tip and its
%%% block index, then writes only derived transaction targets and assignments
%%% to the scheduler store. A pass never combines data from different tips.
-module(lib_arweave_scheduler_sync).
-export([sync/1, process/2, read_global/1]).
-export([targets/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ARWEAVE, #{ <<"device">> => <<"arweave@2.9">> }).
-define(DEFAULT_FROM_HEIGHT, 1968888).
-define(DEFAULT_BLOCK_BATCH, 8).
-define(DEFAULT_BLOCK_WORKERS, 4).
-define(DEFAULT_HEADER_WORKERS, 32).
-define(PROCESS_IDLE_MS, 1000).

%% @doc Advance the shared target index to the stable height of one tip.
sync(Opts) ->
    maybe
        {ok, Store} ?= scheduler_store(Opts),
        exclusive(
            {?MODULE, sync, store_refs(Store)},
            fun() -> do_sync(Opts) end,
            ?PROCESS_IDLE_MS
        )
    end.

%% @doc Advance the global index and materialise one dense process schedule.
process(ProcessID, Opts) ->
    maybe
        {ok, HumanProcessID} ?= canonical_process_id(ProcessID),
        {ok, Store} ?= scheduler_store(Opts),
        {ok, Global} ?= sync(Opts),
        exclusive(
            {?MODULE, process, store_refs(Store), HumanProcessID},
            fun() -> materialize_process(HumanProcessID, Global, Opts) end,
            ?PROCESS_IDLE_MS
        )
    else
        error ->
            error_message(
                422,
                <<"invalid-process">>,
                <<"Arweave process ID must be a canonical transaction ID.">>
            );
        Error -> Error
    end.

%% @doc Read and validate the durable global frontier without advancing it.
read_global(Opts) ->
    maybe
        {ok, _Store} ?= scheduler_store(Opts),
        case lib_arweave_scheduler_cache:read_global(Opts) of
            {ok, State} -> initial_state({ok, State}, from_height(Opts));
            Other -> Other
        end
    end.

%% @doc Capture one chain view and extend only against that immutable view.
do_sync(Opts) ->
    From = from_height(Opts),
    maybe
        {ok, State} ?=
            initial_state(
                lib_arweave_scheduler_cache:read_global(Opts),
                From
            ),
        {ok, Snapshot} ?= snapshot(Opts),
        ok ?= validate_frontier(State, Snapshot, Opts),
        sync_blocks(State, Snapshot, Opts)
    end.

%% @doc Build the initial frontier or validate the configured rollout.
initial_state(not_found, From) ->
    {ok, #{ <<"from">> => From, <<"to">> => From - 1 }};
initial_state({error, not_found}, From) ->
    initial_state(not_found, From);
initial_state({ok, State}, From) ->
    case hb_util:safe_int(hb_maps:get(<<"from">>, State, -1, #{})) of
        {ok, From} -> validate_global_state(State, From);
        {ok, StoredFrom} ->
            error_message(
                409,
                <<"rollout-mismatch">>,
                <<"Configured scheduler rollout height does not match the ",
                    "stored synchronization record.">>,
                #{
                    <<"configured-from">> => From,
                    <<"stored-from">> => StoredFrom
                }
            );
        {error, invalid} -> invalid_global_state()
    end;
initial_state(Error, _From) ->
    Error.

%% @doc Require a complete, internally consistent durable global frontier.
validate_global_state(State, From) ->
    To = hb_util:safe_int(hb_maps:get(<<"to">>, State, -1, #{})),
    case To of
        {ok, InitialTo} when InitialTo =:= From - 1 ->
            {ok, State};
        {ok, StoredTo} when StoredTo >= From ->
            TipHeight =
                hb_util:safe_int(
                    hb_maps:get(<<"tip-height">>, State, -1, #{})
                ),
            BlockHash = hb_maps:get(<<"block-hash">>, State, not_found, #{}),
            TipHash = hb_maps:get(<<"tip-hash">>, State, not_found, #{}),
            case {
                TipHeight,
                valid_block_hash(BlockHash),
                valid_block_hash(TipHash)
            } of
                {{ok, Height}, true, true} when Height >= StoredTo ->
                    {ok, State};
                _ ->
                    invalid_global_state()
            end;
        _ ->
            invalid_global_state()
    end.

invalid_global_state() ->
    error_message(
        500,
        <<"invalid-global-frontier">>,
        <<"The stored scheduler global frontier is malformed.">>
    ).

%% @doc Read one selected tip and retain its index for the complete pass.
snapshot(Opts) ->
    ReadOpts = consensus_opts(Opts),
    maybe
        {ok, Tip} ?=
            hb_ao:resolve(?ARWEAVE, #{ <<"path">> => <<"tip">> }, ReadOpts),
        Height = hb_util:int(hb_maps:get(<<"height">>, Tip, -1, ReadOpts)),
        Hash = hb_maps:get(<<"indep-hash">>, Tip, not_found, ReadOpts),
        true ?= Height >= 0 andalso valid_block_hash(Hash),
        {ok, Index} ?=
            hb_ao:resolve(
                Tip,
                #{ <<"path">> => <<"block-index">> },
                ReadOpts
            ),
        Stable = stable_height(Height, Opts),
        {ok,
            #{
                tip => Tip,
                tip_hash => Hash,
                tip_height => Height,
                index => Index,
                stable_height => Stable
            }
        }
    else
        false ->
            error_message(
                500,
                <<"invalid-consensus-tip">>,
                <<"The locally selected Arweave tip is malformed.">>
            );
        Error -> Error
    end.

%% @doc Bound derived scheduling by the consensus protocol's finality depth.
stable_height(TipHeight, Opts) ->
    ProtocolStable = TipHeight - ?CHECKPOINT_DEPTH,
    case hb_opts:get(arweave_scheduler_max_height, undefined, Opts) of
        undefined -> ProtocolStable;
        Max -> min(ProtocolStable, hb_util:int(Max))
    end.

%% @doc Refuse to extend a frontier that is not on the captured selected chain.
validate_frontier(State, Snapshot, Opts) ->
    From = hb_util:int(hb_maps:get(<<"from">>, State, -1, Opts)),
    To = hb_util:int(hb_maps:get(<<"to">>, State, From - 1, Opts)),
    case To < From of
        true -> ok;
        false ->
            StoredHash = hb_maps:get(<<"block-hash">>, State, not_found, Opts),
            StoredTip = hb_maps:get(<<"tip-hash">>, State, not_found, Opts),
            StoredTipHeight =
                hb_maps:get(<<"tip-height">>, State, not_found, Opts),
            maybe
                true ?=
                    valid_block_hash(StoredHash) andalso
                        valid_block_hash(StoredTip) andalso
                        StoredTipHeight =/= not_found,
                true ?= To =< maps:get(stable_height, Snapshot),
                {ok, Expected} ?=
                    index_hash(maps:get(index, Snapshot), To, Opts),
                true ?= StoredHash =:= Expected,
                ok
            else
                false -> frontier_conflict(To, StoredHash)
            end
    end.

%% @doc Explain a stored frontier that cannot be continued safely.
frontier_conflict(Height, Hash) ->
    error_message(
        409,
        <<"frontier-conflict">>,
        <<"The stored scheduler frontier is not on the captured stable chain.">>,
        #{ <<"height">> => Height, <<"stored-hash">> => Hash }
    ).

%% @doc Extend in bounded batches, publishing the frontier after its targets.
sync_blocks(State, Snapshot, Opts) ->
    To = hb_util:int(hb_maps:get(<<"to">>, State, -1, Opts)),
    Stable = maps:get(stable_height, Snapshot),
    case To >= Stable of
        true -> {ok, State};
        false -> sync_batch(State, Snapshot, Opts, To, Stable)
    end.

%% @doc Load, derive and publish one complete batch.
sync_batch(State, Snapshot, Opts, To, Stable) ->
    BatchEnd = min(Stable, To + block_batch(Opts)),
    Heights = lists:seq(To + 1, BatchEnd),
    maybe
        {ok, Blocks} ?= load_blocks(Heights, Snapshot, Opts),
        {ok, Headers, Targets} ?= prepare_blocks(Blocks, Opts),
        ok ?= write_headers(Headers, Opts),
        Last = lists:last(Blocks),
        NewState =
            State#{
                <<"to">> => maps:get(height, Last),
                <<"block-hash">> => maps:get(hash, Last),
                <<"tip-hash">> => maps:get(tip_hash, Snapshot),
                <<"tip-height">> => maps:get(tip_height, Snapshot)
            },
        {ok, _} ?=
            lib_arweave_scheduler_cache:publish_global(
                NewState,
                Targets,
                Opts
            ),
        ?event(
            arweave_scheduler_short,
            {scheduler_indexed,
                {from, To + 1},
                {to, BatchEnd},
                {headers, length(Headers)},
                {targets, length(Targets)},
                {tip_height, maps:get(tip_height, Snapshot)}
            },
            Opts
        ),
        sync_blocks(NewState, Snapshot, Opts)
    end.

%% @doc Return the positive number of blocks admitted per publication batch.
block_batch(Opts) ->
    max(
        1,
        hb_util:int(
            hb_opts:get(
                arweave_scheduler_block_batch,
                ?DEFAULT_BLOCK_BATCH,
                Opts
            )
        )
    ).

%% @doc Read a bounded set of blocks by hashes from one captured index.
load_blocks(Heights, Snapshot, Opts) ->
    Results =
        hb_pmap:parallel_map(
            Heights,
            fun(Height) -> validated_at(Height, Snapshot, Opts) end,
            max(
                1,
                hb_util:int(
                    hb_opts:get(
                        arweave_scheduler_block_workers,
                        ?DEFAULT_BLOCK_WORKERS,
                        Opts
                    )
                )
            )
        ),
    collect(Results, []).

%% @doc Read exactly the block the captured index names at a height.
validated_at(Height, Snapshot, Opts) ->
    maybe
        {ok, Hash} ?= index_hash(maps:get(index, Snapshot), Height, Opts),
        case
            hb_ao:resolve(
                ?ARWEAVE,
                #{ <<"path">> => <<"validated">>, <<"block">> => Hash },
                consensus_opts(Opts)
            )
        of
            {ok, Block} -> checked_block(Height, Hash, Block, Opts);
            {error,
                #{
                    <<"status">> := 404,
                    <<"message">> := <<"not-validated">>
                }} ->
                history_missing(Height, Hash);
            Error -> Error
        end
    end.

%% @doc Resolve a height from a block index carried by the captured tip.
index_hash(Index, Height, Opts) ->
    maybe
        {ok, Entry} ?=
            hb_ao:resolve(
                Index,
                #{ <<"path">> => <<"at">>, <<"height">> => Height },
                consensus_opts(Opts)
            ),
        Hash = hb_maps:get(<<"indep-hash">>, Entry, not_found, Opts),
        true ?= valid_block_hash(Hash),
        {ok, Hash}
    else
        false ->
            error_message(
                500,
                <<"invalid-block-index">>,
                <<"The captured block index returned an invalid block hash.">>,
                #{ <<"height">> => Height }
            );
        Error -> Error
    end.

%% @doc Require a canonical, transaction-complete validated block.
checked_block(Height, Hash, Block, Opts) ->
    ActualHeight = hb_util:int(hb_maps:get(<<"height">>, Block, -1, Opts)),
    ActualHash = hb_maps:get(<<"indep-hash">>, Block, not_found, Opts),
    TXIDs =
        hb_util:message_to_ordered_list(
            hb_maps:get(<<"txs">>, Block, [], Opts),
            Opts
        ),
    Placements =
        hb_util:message_to_ordered_list(
            hb_maps:get(<<"transactions">>, Block, [], Opts),
            Opts
        ),
    case {
        ActualHeight =:= Height,
        ActualHash =:= Hash,
        lists:member(<<"transactions">>, validation_checks(Block, Opts)),
        length(TXIDs) =:= length(Placements)
    } of
        {true, true, true, true} ->
            {ok,
                #{
                    height => Height,
                    hash => Hash,
                    txids => TXIDs,
                    placements => Placements
                }
            };
        {false, _, _, _} ->
            invalid_block(Height, <<"Validated block height does not match.">>);
        {_, false, _, _} ->
            invalid_block(Height, <<"Validated block hash does not match.">>);
        {_, _, false, _} ->
            history_missing(Height, Hash);
        {_, _, _, false} ->
            history_missing(Height, Hash)
    end.

%% @doc Read the checks that established a validated block.
validation_checks(Block, Opts) ->
    hb_util:message_to_ordered_list(
        hb_maps:get(
            <<"checks">>,
            hb_maps:get(<<"validation">>, Block, #{}, Opts),
            [],
            Opts
        ),
        Opts
    ).

%% @doc Tell the operator which consensus history must be materialised.
history_missing(Height, Hash) ->
    error_message(
        424,
        <<"history-not-materialized">>,
        <<"The captured block is not transaction-complete in the local ",
            "consensus store. Materialize it through ~arweave@2.9/backfill.">>,
        #{ <<"height">> => Height, <<"block">> => Hash }
    ).

%% @doc Reject a locally named block whose canonical fields disagree.
invalid_block(Height, Detail) ->
    error_message(
        500,
        <<"invalid-validated-block">>,
        Detail,
        #{ <<"height">> => Height }
    ).

%% @doc Pair every transaction with its placement and prepare derived entries.
prepare_blocks(Blocks, Opts) ->
    Entries = lists:append([ block_entries(Block) || Block <- Blocks ]),
    Results =
        hb_pmap:parallel_map(
            Entries,
            fun(Entry) -> prepare_transaction(Entry, Opts) end,
            max(
                1,
                hb_util:int(
                    hb_opts:get(
                        arweave_scheduler_header_workers,
                        ?DEFAULT_HEADER_WORKERS,
                        Opts
                    )
                )
            )
        ),
    collect_prepared(Results, #{}, []).

%% @doc Preserve each transaction's exact block position.
block_entries(Block) ->
    block_entries(
        maps:get(height, Block),
        maps:get(hash, Block),
        maps:get(txids, Block),
        maps:get(placements, Block),
        0,
        []
    ).

block_entries(_Height, _Hash, [], [], _Position, Entries) ->
    lists:reverse(Entries);
block_entries(
        Height,
        Hash,
        [TXID | TXIDs],
        [Placement | Placements],
        Position,
        Entries
    ) ->
    block_entries(
        Height,
        Hash,
        TXIDs,
        Placements,
        Position + 1,
        [
            #{
                height => Height,
                hash => Hash,
                position => Position,
                txid => hb_util:human_id(TXID),
                placement => Placement
            }
        |
            Entries
        ]
    ).

%% @doc Admit one consensus placement and route only data-free transactions.
prepare_transaction(Entry, Opts) ->
    Height = maps:get(height, Entry),
    Hash = maps:get(hash, Entry),
    Position = maps:get(position, Entry),
    TXID = maps:get(txid, Entry),
    Placement = maps:get(placement, Entry),
    try
        PlacementID =
            hb_util:human_id(
                hb_maps:get(<<"id">>, Placement, not_found, Opts)
            ),
        PlacementHash = hb_maps:get(<<"block">>, Placement, not_found, Opts),
        PlacementHeight =
            hb_util:int(hb_maps:get(<<"height">>, Placement, -1, Opts)),
        PlacementPosition =
            hb_util:int(hb_maps:get(<<"position">>, Placement, -1, Opts)),
        DataSize =
            hb_util:int(hb_maps:get(<<"data-size">>, Placement, -1, Opts)),
        case {
            PlacementID =:= TXID,
            PlacementHash =:= Hash,
            PlacementHeight =:= Height,
            PlacementPosition =:= Position,
            DataSize >= 0
        } of
            {true, true, true, true, true} when DataSize > 0 ->
                {ok, none};
            {true, true, true, true, true} ->
                prepare_header(Entry, Placement, Opts);
            _ ->
                invalid_placement(Height, Position, TXID)
        end
    catch
        _:_ -> invalid_placement(Height, Position, TXID)
    end.

%% @doc Copy a validated header only when it contributes a schedule target.
prepare_header(Entry, Placement, Opts) ->
    TXID = maps:get(txid, Entry),
    case transaction(Placement, Opts) of
        {ok, Header, TX} ->
            case hb_util:human_id(TX#tx.id) of
                TXID ->
                    Addresses = targets(TX),
                    Ordinate =
                        ordinate(
                            maps:get(height, Entry),
                            maps:get(position, Entry)
                        ),
                    case Addresses of
                        [] -> {ok, none};
                        _ ->
                            {ok,
                                {TXID,
                                    Header,
                                    [
                                        {
                                            Address,
                                            Ordinate,
                                            maps:get(hash, Entry),
                                            TXID
                                        }
                                    ||
                                        Address <- Addresses
                                    ]
                                }
                            }
                    end;
                _ ->
                    invalid_placement(
                        maps:get(height, Entry),
                        maps:get(position, Entry),
                        TXID
                    )
            end;
        Error -> Error
    end.

%% @doc Resolve a committed placement transaction through the main store.
transaction(Placement, Opts) ->
    case maps:get(<<"transaction">>, Placement, not_found) of
        not_found ->
            invalid_placement_transaction();
        Raw ->
            try hb_cache:ensure_loaded(Raw, consensus_opts(Opts)) of
                Header -> {ok, Header, lib_arweave_tx:to_tx(Header, Opts)}
            catch
                throw:{necessary_message_not_found, _, _} ->
                    invalid_placement_transaction();
                throw:{could_not_read_lazy_link, _, _, _} ->
                    invalid_placement_transaction()
            end
    end.

%% @doc Report a committed placement whose transaction root is unavailable.
invalid_placement_transaction() ->
    error_message(
        500,
        <<"invalid-placement-transaction">>,
        <<"A committed placement does not resolve to its transaction.">>
    ).

%% @doc Reject a placement that disagrees with its block's canonical order.
invalid_placement(Height, Position, TXID) ->
    error_message(
        500,
        <<"invalid-placement">>,
        <<"A committed placement disagrees with its validated block.">>,
        #{
            <<"height">> => Height,
            <<"position">> => Position,
            <<"tx">> => TXID
        }
    ).

%% @doc Collect prepared headers by ID and all target aliases.
collect_prepared([], Headers, Targets) ->
    {ok, maps:to_list(Headers), lists:append(lists:reverse(Targets))};
collect_prepared([{ok, none} | Rest], Headers, Targets) ->
    collect_prepared(Rest, Headers, Targets);
collect_prepared(
        [{ok, {TXID, Header, Entries}} | Rest],
        Headers,
        Targets
    ) ->
    collect_prepared(Rest, Headers#{ TXID => Header }, [Entries | Targets]);
collect_prepared([Error | _], _Headers, _Targets) ->
    Error.

%% @doc Copy every required header before exposing any target or frontier.
write_headers(Headers, Opts) ->
    Results =
        hb_pmap:parallel_map(
            Headers,
            fun({TXID, Header}) ->
                lib_arweave_scheduler_cache:write_header(
                    TXID,
                    Header,
                    Opts
                )
            end,
            max(
                1,
                hb_util:int(
                    hb_opts:get(
                        arweave_scheduler_header_workers,
                        ?DEFAULT_HEADER_WORKERS,
                        Opts
                    )
                )
            )
        ),
    require_written(Results).

require_written([]) -> ok;
require_written([{ok, _} | Rest]) -> require_written(Rest);
require_written([Error | _]) -> Error.

%% @doc Materialise the entries relevant to one process into dense slots.
materialize_process(ProcessID, Global, Opts) ->
    maybe
        {ok, Index} ?= global_index(Global, Opts),
        {ok, State} ?= ensure_process(ProcessID, Global, Index, Opts),
        materialize_targets(ProcessID, State, Global, Index, Opts)
    end.

%% @doc Read a process frontier or initialise its spawn assignment.
ensure_process(ProcessID, Global, Index, Opts) ->
    case lib_arweave_scheduler_cache:read_process(ProcessID, Opts) of
        {ok, State} -> validate_process_state(ProcessID, State, Opts);
        not_found -> initialize_process(ProcessID, Global, Index, Opts);
        {error, not_found} -> initialize_process(ProcessID, Global, Index, Opts);
        Error -> Error
    end.

%% @doc Reject a durable process frontier whose schema or owner is corrupt.
validate_process_state(ProcessID, State, Opts) ->
    StoredProcess = hb_maps:get(<<"process">>, State, not_found, Opts),
    SpawnOrdinate = hb_maps:get(<<"spawn-ordinate">>, State, not_found, Opts),
    NextSlot = hb_util:safe_int(hb_maps:get(<<"next-slot">>, State, -1, Opts)),
    SyncedTo = hb_util:safe_int(hb_maps:get(<<"synced-to">>, State, -1, Opts)),
    case {
        canonical_process_id(StoredProcess),
        parse_ordinate(SpawnOrdinate),
        NextSlot,
        SyncedTo
    } of
        {{ok, ProcessID}, {ok, {SpawnHeight, _}}, {ok, Next}, {ok, Synced}}
                when Next >= 1, Synced >= SpawnHeight - 1 ->
            {ok, State};
        _ ->
            error_message(
                500,
                <<"invalid-process-frontier">>,
                <<"The stored scheduler process frontier is malformed.">>,
                #{ <<"process">> => ProcessID }
            )
    end.

%% @doc Validate the spawn against the global snapshot before writing slot 0.
initialize_process(ProcessID, Global, Index, Opts) ->
    maybe
        {ok, Height, Position, SpawnOrdinate, Header} ?=
            spawn_transaction(ProcessID, Global, Index, Opts),
        {ok, _} ?=
            lib_arweave_scheduler_cache:write_header(
                ProcessID,
                Header,
                Opts
            ),
        {ok, _} ?=
            write_assignment(
                ProcessID,
                0,
                Height,
                Position,
                ProcessID,
                Opts
            ),
        State =
            #{
                <<"process">> => ProcessID,
                <<"spawn-ordinate">> => SpawnOrdinate,
                <<"synced-to">> => Height - 1,
                <<"next-slot">> => 1
            },
        {ok, _} ?=
            lib_arweave_scheduler_cache:write_process(
                ProcessID,
                State,
                Opts
            ),
        {ok, State}
    end.

%% @doc Recover the exact index snapshot that published the global frontier.
global_index(Global, Opts) ->
    TipHash = hb_maps:get(<<"tip-hash">>, Global, not_found, Opts),
    To = hb_util:int(hb_maps:get(<<"to">>, Global, -1, Opts)),
    BlockHash = hb_maps:get(<<"block-hash">>, Global, not_found, Opts),
    maybe
        {ok, TipHeight} ?=
            hb_util:safe_int(
                hb_maps:get(<<"tip-height">>, Global, -1, Opts)
            ),
        true ?=
            valid_block_hash(TipHash) andalso
                valid_block_hash(BlockHash),
        {ok, Tip} ?=
            hb_ao:resolve(
                ?ARWEAVE,
                #{ <<"path">> => <<"validated">>, <<"block">> => TipHash },
                consensus_opts(Opts)
            ),
        {ok, ActualTipHeight} ?=
            hb_util:safe_int(hb_maps:get(<<"height">>, Tip, -1, Opts)),
        true ?=
            hb_maps:get(<<"indep-hash">>, Tip, not_found, Opts) =:= TipHash
                andalso
                ActualTipHeight =:= TipHeight,
        {ok, Index} ?=
            hb_ao:resolve(
                Tip,
                #{ <<"path">> => <<"block-index">> },
                consensus_opts(Opts)
            ),
        {ok, Expected} ?= index_hash(Index, To, Opts),
        true ?= Expected =:= BlockHash,
        {ok, Index}
    else
        false -> frontier_conflict(To, BlockHash);
        {error, invalid} -> frontier_conflict(To, BlockHash);
        Error -> Error
    end.

%% @doc Resolve and validate a process placement against the global snapshot.
spawn_transaction(ProcessID, Global, Index, Opts) ->
    maybe
        {ok, Placement} ?=
            hb_ao:resolve(
                ?ARWEAVE,
                #{ <<"path">> => <<"placement">>, <<"tx">> => ProcessID },
                consensus_opts(Opts)
            ),
        Height = hb_util:int(hb_maps:get(<<"height">>, Placement, -1, Opts)),
        Position =
            hb_util:int(hb_maps:get(<<"position">>, Placement, -1, Opts)),
        ok ?= require_covered_spawn(ProcessID, Height, Global, Opts),
        PlacedHash = hb_maps:get(<<"block">>, Placement, not_found, Opts),
        PlacedID =
            hb_util:human_id(
                hb_maps:get(<<"id">>, Placement, not_found, Opts)
            ),
        {ok, SelectedHash} ?= index_hash(Index, Height, Opts),
        true ?= PlacedID =:= ProcessID andalso PlacedHash =:= SelectedHash,
        {ok, Block} ?=
            hb_ao:resolve(
                ?ARWEAVE,
                #{
                    <<"path">> => <<"validated">>,
                    <<"block">> => SelectedHash
                },
                consensus_opts(Opts)
            ),
        {ok, Checked} ?= checked_block(Height, SelectedHash, Block, Opts),
        true ?= transaction_at(Checked, Position) =:= ProcessID,
        {ok, Header, TX} ?= transaction(Placement, Opts),
        true ?= hb_util:human_id(TX#tx.id) =:= ProcessID,
        {ok, Height, Position, ordinate(Height, Position), Header}
    else
        false ->
            error_message(
                409,
                <<"stale-placement">>,
                <<"The process placement is not on the scheduler snapshot.">>,
                #{ <<"process">> => ProcessID }
            );
        Error -> Error
    end.

%% @doc Return the canonical transaction ID at one zero-based block position.
transaction_at(Block, Position) when Position >= 0 ->
    TXIDs = maps:get(txids, Block),
    case Position < length(TXIDs) of
        true -> hb_util:human_id(lists:nth(Position + 1, TXIDs));
        false -> not_found
    end;
transaction_at(_Block, _Position) ->
    not_found.

%% @doc Require the spawn to fall inside the durable global interval.
require_covered_spawn(ProcessID, Height, Global, Opts) ->
    From = hb_util:int(hb_maps:get(<<"from">>, Global, -1, Opts)),
    To = hb_util:int(hb_maps:get(<<"to">>, Global, -1, Opts)),
    case {Height < From, Height > To} of
        {true, _} ->
            error_message(
                409,
                <<"process-before-rollout">>,
                <<"Process predates the Arweave scheduler rollout height.">>,
                #{ <<"spawn-height">> => Height, <<"indexed-from">> => From }
            );
        {_, true} ->
            error_message(
                425,
                <<"process-not-stable">>,
                <<"Process spawn block is not yet in the stable index.">>,
                #{
                    <<"process">> => ProcessID,
                    <<"spawn-height">> => Height,
                    <<"indexed-to">> => To
                }
            );
        _ -> ok
    end.

%% @doc Advance one process only through the global frontier it was given.
materialize_targets(ProcessID, State, Global, Index, Opts) ->
    SyncedTo = hb_util:int(hb_maps:get(<<"synced-to">>, State, -1, Opts)),
    GlobalTo = hb_util:int(hb_maps:get(<<"to">>, Global, -1, Opts)),
    case SyncedTo >= GlobalTo of
        true -> {ok, State};
        false ->
            maybe
                ok ?=
                    lib_arweave_scheduler_cache:ensure_target_root(
                        ProcessID,
                        Opts
                    ),
                {ok, Ordinates} ?=
                    lib_arweave_scheduler_cache:list_targets(
                        ProcessID,
                        Opts
                    ),
                Parsed =
                    lists:filtermap(
                        fun(Ordinate) ->
                            case parse_ordinate(Ordinate) of
                                {ok, ParsedPosition} ->
                                    {true, {ParsedPosition, Ordinate}};
                                error -> false
                            end
                        end,
                        Ordinates
                    ),
                Selected =
                    select_targets(lists:sort(Parsed), State, Global, Opts),
                write_process_assignments(
                    ProcessID,
                    Selected,
                    State,
                    Global,
                    Index,
                    Opts
                )
            end
    end.

%% @doc Select entries strictly after the spawn and previous process frontier.
select_targets(Targets, State, Global, Opts) ->
    SpawnOrdinate = hb_maps:get(<<"spawn-ordinate">>, State, <<>>, Opts),
    SyncedTo = hb_util:int(hb_maps:get(<<"synced-to">>, State, -1, Opts)),
    GlobalTo = hb_util:int(hb_maps:get(<<"to">>, Global, -1, Opts)),
    {ok, SpawnPosition} = parse_ordinate(SpawnOrdinate),
    [
        {Position, Ordinate}
    ||
        {Position = {Height, _}, Ordinate} <- Targets,
        Position > SpawnPosition,
        Height > SyncedTo,
        Height =< GlobalTo
    ].

%% @doc Publish assignments before the process frontier that exposes them.
write_process_assignments(ProcessID, Targets, State, Global, Index, Opts) ->
    NextSlot = hb_util:int(hb_maps:get(<<"next-slot">>, State, 0, Opts)),
    case write_process_assignments(ProcessID, Targets, NextSlot, Index, Opts) of
        {ok, NewNextSlot} ->
            NewState =
                State#{
                    <<"synced-to">> =>
                        hb_util:int(hb_maps:get(<<"to">>, Global, -1, Opts)),
                    <<"next-slot">> => NewNextSlot
                },
            case
                lib_arweave_scheduler_cache:write_process(
                    ProcessID,
                    NewState,
                    Opts
                )
            of
                {ok, _} -> {ok, NewState};
                Error -> Error
            end;
        Error -> Error
    end.

write_process_assignments(_ProcessID, [], Slot, _Index, _Opts) ->
    {ok, Slot};
write_process_assignments(
        ProcessID,
        [{{Height, Position}, Ordinate} | Rest],
        Slot,
        Index,
        Opts
    ) ->
    case
        lib_arweave_scheduler_cache:read_target(
            ProcessID,
            Ordinate,
            Opts
        )
    of
        {ok, BlockHash, TXID, _Header} ->
            maybe
                {ok, SelectedHash} ?= index_hash(Index, Height, Opts),
                case SelectedHash =:= BlockHash of
                    true ->
                        maybe
                            {ok, _} ?=
                                write_assignment(
                                    ProcessID,
                                    Slot,
                                    Height,
                                    Position,
                                    TXID,
                                    Opts
                                ),
                            write_process_assignments(
                                ProcessID,
                                Rest,
                                Slot + 1,
                                Index,
                                Opts
                            )
                        end;
                    false ->
                        write_process_assignments(
                            ProcessID,
                            Rest,
                            Slot,
                            Index,
                            Opts
                        )
                end
            end;
        not_found ->
            error_message(
                500,
                <<"missing-indexed-target">>,
                <<"An indexed scheduler target has no transaction header.">>,
                #{ <<"ordinate">> => Ordinate }
            );
        Error -> Error
    end.

%% @doc Write one canonical scheduler assignment.
write_assignment(ProcessID, Slot, Height, Position, TXID, Opts) ->
    Assignment =
        #{
            <<"path">> => <<"compute">>,
            <<"data-protocol">> => <<"ao">>,
            <<"variant">> => <<"ao.N.1">>,
            <<"process">> => ProcessID,
            <<"epoch">> => <<"0">>,
            <<"slot">> => Slot,
            <<"block-height">> => Height,
            <<"block-index">> => Position,
            <<"body">> =>
                {link,
                    TXID,
                    #{ <<"type">> => <<"link">>, <<"lazy">> => false }
                },
            <<"type">> => <<"Assignment">>
        },
    lib_arweave_scheduler_cache:write_assignment(Assignment, Opts).

%% @doc Return the native recipient and one well-formed `Assign-To' list.
targets(TX) ->
    Native =
        case TX#tx.target of
            Target when is_binary(Target), byte_size(Target) =:= 32 ->
                [hb_util:human_id(Target)];
            _ -> []
        end,
    AssignValues =
        [
            Value
        ||
            {Key, Value} <- TX#tx.tags,
            hb_util:to_lower(hb_ao:normalize_key(Key)) =:= <<"assign-to">>
        ],
    Assigned =
        case AssignValues of
            [Value] when is_binary(Value) ->
                lists:filtermap(
                    fun canonical_address/1,
                    binary:split(Value, <<",">>, [global])
                );
            _ -> []
        end,
    lists:usort(Native ++ Assigned).

%% @doc Return a canonical address after trimming ASCII whitespace.
canonical_address(Raw) ->
    Address = trim_ascii(Raw),
    try
        Native = hb_util:native_id(Address),
        case byte_size(Native) =:= 32 andalso hb_util:human_id(Native) =:= Address of
            true -> {true, Address};
            false -> false
        end
    catch
        _:_ -> false
    end.

%% @doc Validate and normalise an Arweave transaction identifier.
canonical_process_id(ProcessID) ->
    try canonical_address(hb_util:human_id(ProcessID)) of
        {true, HumanProcessID} -> {ok, HumanProcessID};
        false -> error
    catch
        _:_ -> error
    end.

trim_ascii(<<C, Rest/binary>>)
        when C =:= $\s; C =:= $\t; C =:= $\n; C =:= 11;
             C =:= 12; C =:= $\r ->
    trim_ascii(Rest);
trim_ascii(Bin) ->
    trim_ascii_right(Bin, byte_size(Bin)).

trim_ascii_right(_, 0) -> <<>>;
trim_ascii_right(Bin, Length) ->
    case binary:at(Bin, Length - 1) of
        C when C =:= $\s; C =:= $\t; C =:= $\n; C =:= 11;
               C =:= 12; C =:= $\r ->
            trim_ascii_right(Bin, Length - 1);
        _ -> binary:part(Bin, 0, Length)
    end.

%% @doc Encode a lossless layer-one block position.
ordinate(Height, Position) ->
    <<
        (integer_to_binary(Height))/binary,
        "-",
        (integer_to_binary(Position))/binary
    >>.

%% @doc Decode a non-negative layer-one block position.
parse_ordinate(Ordinate) when is_binary(Ordinate) ->
    case binary:split(Ordinate, <<"-">>, [global]) of
        [Height, Position] ->
            try
                Parsed =
                    {binary_to_integer(Height), binary_to_integer(Position)},
                case Parsed of
                    {H, P} when H >= 0, P >= 0 -> {ok, Parsed};
                    _ -> error
                end
            catch
                _:_ -> error
            end;
        _ -> error
    end;
parse_ordinate(_) ->
    error.

%% @doc Return the configured protocol rollout height.
from_height(Opts) ->
    hb_util:int(
        hb_opts:get(
            arweave_scheduler_from,
            ?DEFAULT_FROM_HEIGHT,
            Opts
        )
    ).

%% @doc Disable resolver result caching for mutable consensus aliases.
consensus_opts(Opts) ->
    Opts#{
        <<"hashpath">> => ignore,
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>]
    }.

%% @doc Return the mandatory scheduler store or a public configuration error.
scheduler_store(Opts) ->
    try
        SchedulerOpts = lib_arweave_scheduler_cache:opts(Opts),
        {ok, hb_opts:get(store, no_viable_store, SchedulerOpts)}
    catch
        _:_ ->
            error_message(
                500,
                <<"invalid-scheduler-store">>,
                <<"A distinct scheduler-store must be configured.">>
            )
    end.

%% @doc Return stable identities for runner serialisation.
store_refs(Stores) when is_list(Stores) ->
    lists:sort([ store_ref(Store) || Store <- Stores ]);
store_refs(Store) ->
    [store_ref(Store)].

store_ref(Store = #{ <<"store-module">> := Module }) ->
    {Module, hb_maps:get(<<"name">>, Store, Module)};
store_ref(Store) ->
    Store.

%% @doc Run one task at a time through a short-lived named worker.
exclusive(Name, Fun, IdleTimeout) ->
    Runner =
        hb_name:singleton(
            Name,
            fun() ->
                try runner(IdleTimeout)
                after hb_name:unregister(Name)
                end
            end
        ),
    Monitor = erlang:monitor(process, Runner),
    Runner ! {run, self(), Monitor, Fun},
    receive
        {ran, Monitor, Result} ->
            erlang:demonitor(Monitor, [flush]),
            Result;
        {'DOWN', Monitor, process, Runner, Reason}
                when Reason =:= normal; Reason =:= noproc ->
            exclusive(Name, Fun, IdleTimeout);
        {'DOWN', Monitor, process, Runner, Reason} ->
            exit({arweave_scheduler_runner_down, Reason})
    end.

runner(IdleTimeout) ->
    receive
        {run, Caller, Ref, Fun} ->
            Caller ! {ran, Ref, Fun()},
            runner(IdleTimeout)
    after IdleTimeout ->
        ok
    end.

%% @doc Collect successful results without hiding the first failure.
collect([], Results) ->
    {ok, lists:reverse(Results)};
collect([{ok, Result} | Rest], Results) ->
    collect(Rest, [Result | Results]);
collect([Error | _], _Results) ->
    Error.

valid_block_hash(Hash) ->
    is_binary(Hash) andalso byte_size(Hash) =:= 64.

%% @doc Construct a public AO error message.
error_message(Status, Message, Detail) ->
    {error,
        #{
            <<"status">> => Status,
            <<"message">> => Message,
            <<"detail">> => Detail
        }
    }.

error_message(Status, Message, Detail, Extra) ->
    {error,
        maps:merge(
            #{
                <<"status">> => Status,
                <<"message">> => Message,
                <<"detail">> => Detail
            },
            Extra
        )
    }.

%%% Tests

targets_test() ->
    Native = crypto:strong_rand_bytes(32),
    NativeAddress = hb_util:human_id(Native),
    A = hb_util:human_id(crypto:strong_rand_bytes(32)),
    B = hb_util:human_id(crypto:strong_rand_bytes(32)),
    TX =
        #tx{
            target = Native,
            tags =
                [
                    {<<"Assign-To">>,
                        <<" ", A/binary, ",", B/binary, ",", A/binary>>}
                ]
        },
    ?assertEqual(lists:usort([NativeAddress, A, B]), targets(TX)),
    ?assertEqual(
        [NativeAddress],
        targets(
            TX#tx{
                tags =
                    [
                        {<<"Assign-To">>, A},
                        {<<"assign-to">>, B}
                    ]
            }
        )
    ).

ordinate_test() ->
    ?assertEqual(<<"1966084-23">>, ordinate(1966084, 23)),
    ?assertEqual({ok, {1966084, 23}}, parse_ordinate(<<"1966084-23">>)),
    ?assertEqual(error, parse_ordinate(<<"1966084">>)),
    ?assertEqual(error, parse_ordinate(<<"-1-2">>)).
