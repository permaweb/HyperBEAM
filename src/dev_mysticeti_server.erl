%%% @doc Mysticeti-C consensus scheduler server.
%%%
%%% This server maintains a per-process block DAG, evaluates the Mysticeti-C
%%% decision sequence, and emits AO-Core assignments for user messages. The
%%% consensus logic follows the paper’s helper predicates and universal
%%% committer; the final message ordering step is AO-Core-specific.
%%%
%%% Algorithm sketch (paper references inline):
%%% 1. Block validity + DAG model (mysticeti-paper/sections/overview.tex).
%%%    - A block includes `{author, round, parents, body}` and a valid signature.
%%%    - Parents are distinct, from earlier rounds; the first parent is the
%%%      author’s most recent block (latest round < r).
%%%    - The parent list includes ≥ 2f+1 blocks from round r-1.
%%%    - Equivocations are retained; duplicate sequence numbers are removed
%%%      during delivery (mysticeti-paper/sections/security.tex, Integrity).
%%%
%%% 2. Support / vote predicates (mysticeti-paper/algorithms/consensus_utils.tex).
%%%    - `SupportedBlock` is a depth-first walk over parent lists: it returns
%%%      the first block encountered for (author, round).
%%%    - `IsVote` holds when a voting-round block supports the proposer block.
%%%
%%% 3. Direct decision per proposer slot (mysticeti-paper/algorithms/baseline_committer.tex
%%%    + consensus_utils.tex).
%%%    - A proposer slot is a pair (author, round). Waves (length = waveLength)
%%%      define proposer and decision rounds for each slot.
%%%    - For each round and proposer slot `l ∈ [0, num_proposers)`, select the
%%%      proposer via `PredefinedProposer(r + l + proposer_offset)`.
%%%    - `SupportedProposer` holds if ≥ 2f+1 *distinct authors* in the decision
%%%      round certify the proposer.
%%%    - `SkippedProposer` holds if ≥ 2f+1 *distinct authors* in the voting
%%%      round have no parent authored by the proposer.
%%%
%%% 4. Indirect decision (mysticeti-paper/algorithms/universal_committer.tex).
%%%    - If direct decision is undecided, look for later slots (anchors) with
%%%      round > decision round.
%%%    - If the first anchor is undecided, remain undecided.
%%%    - If the anchor is committed and has a certified link, commit; else skip.
%%%
%%% 5. Commit sequence + AO total order.
%%%    - Slots are ordered deterministically (round, slot) and committed until
%%%      the first undecided slot (mysticeti-paper/sections/consensus.tex).
%%%    - AO-Core then linearizes the committed proposer’s causal history,
%%%      removes duplicate (author, round) sequence numbers, and orders by
%%%      `(round, author, block_id)` to produce the total order of user messages.
%%%
%%% Notes:
%%% - Validators are derived from the process staker set; `f = floor((n-1)/3)`,
%%%   quorum is `2f+1`.
%%% - The committer iterates proposer slots (`num_proposers`) exactly as in the
%%%   universal committer’s inner loop.
%%% - Consensus parameters are read from the process `mysticeti` map (strict).
%%%
%%% Paper: "Mysticeti: Reaching the Limits of Latency with Uncertified DAGs"
%%% (Babel et al., arXiv:2310.14821).
-module(dev_mysticeti_server).
-export([start/3, schedule/2, ingest_block/2, info/1, stop/1]).
-include("include/hb.hrl").

%%% Default schedule timeout (ms)
-define(DEFAULT_TIMEOUT, 10000).

%% @doc Return the Opts map stored in server state.
state_opts(State) ->
    hb_maps:get(opts, State, #{}, #{}).

%% @doc Read a value from server state using hb_maps.
state_get(Key, State, Default) ->
    Opts = state_opts(State),
    hb_maps:get(Key, State, Default, Opts).

%% @doc Read a value from server state with default `undefined`.
state_get(Key, State) ->
    state_get(Key, State, undefined).

%% @doc Start a consensus scheduling server for a given process.
start(ProcID, Proc, Opts) ->
    ?event(mysticeti, {starting_server, {proc_id, ProcID}}),
    spawn_link(
        fun() ->
            RegKey = dev_mysticeti_registry:registry_key(ProcID, Opts),
            case hb_name:register(RegKey) of
                ok -> ok;
                error ->
                    throw({another_mysticeti_scheduler_is_already_registered,
                        {proc_id, ProcID},
                        {registry_key, RegKey}})
            end,
            % Ensure the process is cached for later reference.
            dev_scheduler_cache:write_spawn(Proc, Opts),
            {CurrentSlot, BaseStateHashpath} =
                case dev_scheduler_cache:latest(ProcID, Opts) of
                    not_found -> {-1, undefined};
                    {Slot, Base} -> {Slot, Base}
                end,
            State = init_state(ProcID, Proc, CurrentSlot, BaseStateHashpath, Opts),
            server(State)
        end
    ).

%% @doc Call the appropriate scheduling server to assign a message.
schedule(AOProcID, Message) when is_binary(AOProcID) ->
    schedule(dev_mysticeti_registry:find(AOProcID), Message);
schedule(ErlangProcID, Message) ->
    AbortTime = scheduler_time() + ?DEFAULT_TIMEOUT,
    ErlangProcID ! {schedule, Message, self(), AbortTime},
    receive
        {scheduled, Message, Result} ->
            Result
    after ?DEFAULT_TIMEOUT ->
        throw({mysticeti_scheduler_timeout, {proc_id, ErlangProcID}})
    end.

%% @doc Ingest a block from a peer into the server DAG.
ingest_block(ErlangProcID, Block) ->
    ErlangProcID ! {block, Block},
    ok.

%% @doc Get summary info from the scheduling server.
info(ProcID) ->
    ProcID ! {info, self()},
    receive {info, Info} -> Info end.

%% @doc Stop the scheduling server.
stop(ProcID) ->
    ProcID ! stop.

%% @doc Initialize the server state.
init_state(ProcID, Proc, CurrentSlot, BaseStateHashpath, Opts) ->
    Stakers = stakers(Proc, Opts),
    Validators = validators_from_stakers(Stakers),
    LocalAuthor = local_author(Validators, Opts),
    ok = ensure_local_author(Validators, LocalAuthor),
    F = (length(Validators) - 1) div 3,
    Quorum = (2 * F) + 1,
    WaveLength = wave_length(Proc, Opts),
    ProposerOffset = proposer_offset(Proc, Opts),
    NumProposers = num_proposers(Proc, Validators, Opts),
    Peers = peers(Proc, Opts),
    #{
        id => ProcID,
        opts => Opts,
        stakers => Stakers,
        validators => Validators,
        quorum => Quorum,
        wave_length => WaveLength,
        proposer_offset => ProposerOffset,
        num_proposers => NumProposers,
        local_author => LocalAuthor,
        peers => Peers,
        dag => #{},
        round_index => #{},
        author_rounds => #{},
        max_round => -1,
        ordered => #{},
        assigned => #{},
        decided_slots => #{},
        last_decided_round => -1,
        pending_blocks => #{},
        pending_msgs => [],
        current_slot => CurrentSlot,
        base_state_hashpath => BaseStateHashpath,
        wallets => commitment_wallets(Proc, Opts),
        commitment_spec => commitment_spec(Proc, Opts)
    }.

%% @doc Main server loop.
server(State) ->
    receive
        {schedule, Message, Reply, AbortTime} ->
            case scheduler_time() > AbortTime of
                true ->
                    server(State);
                false ->
                    {NextState, Result} = handle_schedule(State, Message),
                    Reply ! {scheduled, Message, Result},
                    server(NextState)
            end;
        {block, Block} ->
            server(handle_block(State, Block));
        {info, Reply} ->
            Reply ! {info, summarize(State)},
            server(State);
        stop ->
            ok
    end.

%% @doc Handle local schedule requests.
handle_schedule(State, Message) ->
    Opts = state_opts(State),
    {ok, OnlyCommitted} = hb_message:with_only_committed(Message, Opts),
    case make_block(State, OnlyCommitted) of
        {ok, Block} ->
            {State1, _} = add_block(State, Block),
            {State2, _} = drain_pending_blocks(State1),
            {State3, NewLocalBlocks} = drain_pending_messages(State2),
            {State4, _} = drain_pending_blocks(State3),
            {State5, NewAssignments} = try_commit(State4),
            broadcast_blocks(State5, [Block | NewLocalBlocks]),
            MessageId = hb_message:id(OnlyCommitted, all, Opts),
            Result =
                case lists:keyfind(MessageId, 1, NewAssignments) of
                    {MessageId, Assignment} -> {committed, Assignment};
                    false ->
                        {pending, #{
                            <<"block">> => hb_maps:get(<<"id">>, Block, undefined, Opts),
                            <<"author">> => hb_maps:get(<<"author">>, Block, undefined, Opts),
                            <<"round">> => hb_maps:get(<<"round">>, Block, undefined, Opts)
                        }}
                end,
            {State5, Result};
        {error, Reason} ->
            {State1, _Queued} = enqueue_pending_message(State, OnlyCommitted),
            {State1, {pending, #{ <<"reason">> => Reason }}}
    end.

%% @doc Handle an inbound block from a peer.
handle_block(State, Block) ->
    case validate_block(State, Block) of
        {ok, CanonBlock} ->
            {State1, _} = add_block(State, CanonBlock),
            {State2, _} = drain_pending_blocks(State1),
            {State3, NewLocalBlocks} = drain_pending_messages(State2),
            {State4, _} = drain_pending_blocks(State3),
            {State5, _} = try_commit(State4),
            broadcast_blocks(State5, NewLocalBlocks),
            State5;
        {error, missing_parents} ->
            {State1, _} = store_pending_block(State, Block),
            State1;
        {error, _Reason} ->
            State
    end.

%% @doc Construct a new block for the local author.
make_block(State, Payload) ->
    Round = next_round(State),
    case parent_set(State, Round) of
        {ok, Parents} ->
            Opts = state_opts(State),
            PayloadRes =
                case Payload of
                    Msg when is_map(Msg) ->
                        case hb_cache:write(Msg, Opts) of
                            {ok, _} -> ok;
                            {error, Reason} ->
                                {error, {payload_cache_failed, Reason}}
                        end;
                    _ ->
                        ok
                end,
            case PayloadRes of
                {error, _} = Err -> Err;
                ok ->
                    Block0 =
                        #{
                            <<"type">> => <<"MysticetiBlock">>,
                            <<"process">> => state_get(id, State),
                            <<"author">> => state_get(local_author, State),
                            <<"round">> => Round,
                            <<"parents">> => Parents,
                            <<"timestamp">> => scheduler_time(),
                            <<"body">> => Payload
                        },
                    Signed = hb_message:commit(Block0, Opts#{ <<"bundle">> => true }),
                    case ensure_payload_cached(Payload, Signed, Opts) of
                        {error, _} = Err -> Err;
                        ok ->
                            case ensure_block_id(Signed, Opts) of
                                {error, _} = Err -> Err;
                                Block ->
                                    BlockNoId = hb_maps:remove(<<"id">>, Block, Opts),
                                    case verify_block_signature(BlockNoId, Opts) of
                                        {ok, Signer} ->
                                            case hb_maps:get(<<"author">>, Block, undefined, Opts) of
                                                Signer -> {ok, Block};
                                                _ -> {error, signer_mismatch}
                                            end;
                                        {error, _} = Err -> Err
                                    end
                            end
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Ensure the block payload is resolvable from the local cache.
%% Required for HTTP bundling to succeed when broadcasting blocks.
ensure_payload_cached(Payload, Block, Opts) ->
    case hb_maps:get(<<"body">>, Block, undefined, Opts) of
        {link, LinkId, _} ->
            case Payload of
                Msg when is_map(Msg) ->
                    PayloadId = hb_message:id(Msg, all, Opts),
                    ensure_message_id_cached(PayloadId, Msg, Opts),
                    maybe_link_payload(PayloadId, LinkId, Opts),
                    case hb_cache:read(LinkId, Opts) of
                        {ok, _} -> ok;
                        _ -> {error, payload_cache_missing}
                    end;
                _ ->
                    case hb_cache:read(LinkId, Opts) of
                        {ok, _} -> ok;
                        _ -> {error, payload_cache_missing}
                    end
            end;
        _ ->
            ok
    end.

%% @doc Link a payload id to an expected link id when they differ.
maybe_link_payload(PayloadId, LinkId, Opts) ->
    case PayloadId =:= LinkId of
        true -> ok;
        false ->
            Store = hb_opts:get(store, no_viable_store, Opts),
            hb_store:make_link(Store, PayloadId, LinkId)
    end.

%% @doc Cache a message by id if it is not already present locally.
ensure_message_id_cached(MessageId, Message, Opts) ->
    case hb_cache:read(MessageId, Opts) of
        {ok, _} -> ok;
        _ ->
            _ = hb_cache:write(Message, Opts),
            ok
    end.

%% @doc Determine the next round for the local author.
next_round(State) ->
    Opts = state_opts(State),
    LocalAuthor = hb_maps:get(local_author, State, undefined, Opts),
    AuthorRounds = hb_maps:get(author_rounds, State, #{}, Opts),
    hb_maps:get(LocalAuthor, AuthorRounds, -1, Opts) + 1.

%% @doc Select parent blocks for a new round.
%% Block correctness: first parent is author's previous block (latest round < r)
%% and there must be at least 2f+1 distinct authors from the previous round
%% (mysticeti-paper/sections/overview.tex, sec:dag).
parent_set(_State, Round) when Round < 0 ->
    {error, invalid_round};
parent_set(_State, 0) ->
    {ok, []};
parent_set(State, Round) ->
    PrevRound = Round - 1,
    PrevRoundIds = round_block_ids(State, PrevRound),
    Quorum = state_get(quorum, State),
    case PrevRoundIds of
        [] -> {error, missing_prev_round};
        _ ->
            % Block correctness: require at least 2f+1 blocks from round r-1.
            case length(PrevRoundIds) < Quorum of
                true -> {error, not_enough_parents};
                false ->
                    LocalAuthor = state_get(local_author, State),
                    case latest_author_blocks(State, LocalAuthor, Round) of
                        [] ->
                            {error, missing_local_prev};
                        LocalPrevIds ->
                            LocalPrev = pick_block_id(LocalPrevIds),
                            OrderedPrev = lists:sort(PrevRoundIds),
                            Others = [Id || Id <- OrderedPrev, Id =/= LocalPrev],
                            {ok, [LocalPrev | Others]}
                    end
            end
    end.

%% @doc Return all block ids for a round.
round_block_ids(State, Round) ->
    Opts = state_opts(State),
    RoundIndex = hb_maps:get(round_index, State, #{}, Opts),
    case hb_maps:get(Round, RoundIndex, undefined, Opts) of
        undefined -> [];
        ByAuthor ->
            lists:sort(
                lists:usort(
                    lists:flatten(hb_maps:values(ByAuthor, Opts))
                )
            )
    end.

%% @doc Return block ids for an author in a round.
author_blocks_in_round(State, Author, Round) ->
    Opts = state_opts(State),
    RoundIndex = hb_maps:get(round_index, State, #{}, Opts),
    case hb_maps:get(Round, RoundIndex, undefined, Opts) of
        undefined -> [];
        ByAuthor ->
            lists:sort(hb_maps:get(Author, ByAuthor, [], Opts))
    end.

%% @doc Return the most recent blocks by author before a round.
latest_author_blocks(State, Author, Round) ->
    Opts = state_opts(State),
    RoundIndex = hb_maps:get(round_index, State, #{}, Opts),
    Rounds = hb_maps:keys(RoundIndex, Opts),
    PrevRounds =
        [R || R <- Rounds,
              R < Round,
              hb_maps:is_key(Author, hb_maps:get(R, RoundIndex, #{}, Opts), Opts)],
    case PrevRounds of
        [] -> [];
        _ ->
            PrevRound = lists:max(PrevRounds),
            lists:sort(
                hb_maps:get(
                    Author,
                    hb_maps:get(PrevRound, RoundIndex, #{}, Opts),
                    [],
                    Opts
                )
            )
    end.

%% @doc Pick a deterministic block id from a non-empty list.
pick_block_id([Id | _]) -> Id.

%% @doc Insert a block into the DAG.
add_block(State, Block) ->
    Opts = state_opts(State),
    Dag = hb_maps:get(dag, State, #{}, Opts),
    BlockId = hb_maps:get(<<"id">>, Block, undefined, Opts),
    case hb_maps:is_key(BlockId, Dag, Opts) of
        true -> {State, exists};
        false ->
            Round = hb_maps:get(<<"round">>, Block, undefined, Opts),
            Author = hb_maps:get(<<"author">>, Block, undefined, Opts),
            RoundIndex0 = hb_maps:get(round_index, State, #{}, Opts),
            ByAuthor0 = hb_maps:get(Round, RoundIndex0, #{}, Opts),
            Existing = hb_maps:get(Author, ByAuthor0, [], Opts),
            ByAuthor1 = ByAuthor0#{ Author => lists:usort([BlockId | Existing]) },
            RoundIndex1 = RoundIndex0#{ Round => ByAuthor1 },
            AuthorRounds0 = hb_maps:get(author_rounds, State, #{}, Opts),
            PrevRound = hb_maps:get(Author, AuthorRounds0, -1, Opts),
            AuthorRounds1 = AuthorRounds0#{ Author => max(PrevRound, Round) },
            MaxRound = max(hb_maps:get(max_round, State, -1, Opts), Round),
            State1 = State#{
                dag := Dag#{ BlockId => Block },
                round_index := RoundIndex1,
                author_rounds := AuthorRounds1,
                max_round := MaxRound
            },
            {State1, added}
    end.

%% @doc Ensure a block has a deterministic id (not part of the signed payload).
ensure_block_id(Block, Opts) ->
    BlockNoId = hb_maps:remove(<<"id">>, Block, Opts),
    BlockId = hb_message:id(BlockNoId, all, Opts),
    case hb_maps:get(<<"id">>, Block, undefined, Opts) of
        undefined -> BlockNoId#{ <<"id">> => BlockId };
        BlockId -> BlockNoId#{ <<"id">> => BlockId };
        _ -> {error, id_mismatch}
    end.

%% @doc Store a block awaiting missing parents.
store_pending_block(State, Block) ->
    Opts = state_opts(State),
    case ensure_block_id(Block, Opts) of
        {error, _} = Err -> {State, Err};
        Canon ->
            Pending0 = state_get(pending_blocks, State, #{}),
            BlockId = hb_maps:get(<<"id">>, Canon, undefined, Opts),
            {State#{ pending_blocks := Pending0#{ BlockId => Canon } }, BlockId}
    end.

%% @doc Attempt to promote pending blocks whose parents are now available.
drain_pending_blocks(State) ->
    drain_pending_blocks(State, 0).

%% @doc Promote any pending blocks whose parents are now available.
drain_pending_blocks(State, AddedTotal) ->
    Pending = state_get(pending_blocks, State, #{}),
    {State1, PendingLeft, AddedNow} =
        promote_pending_blocks(State, hb_maps:to_list(Pending, state_opts(State)), #{}, 0),
    State2 = State1#{ pending_blocks := PendingLeft },
    case AddedNow of
        0 -> {State2, AddedTotal};
        _ -> drain_pending_blocks(State2, AddedTotal + AddedNow)
    end.

%% @doc Internal helper to promote pending blocks.
promote_pending_blocks(State, [], PendingLeft, AddedNow) ->
    {State, PendingLeft, AddedNow};
promote_pending_blocks(State, [{Id, Block} | Rest], PendingLeft, AddedNow) ->
    case validate_block(State, Block) of
        {ok, CanonBlock} ->
            case add_block(State, CanonBlock) of
                {State1, added} ->
                    promote_pending_blocks(State1, Rest, PendingLeft, AddedNow + 1);
                {State1, _} ->
                    promote_pending_blocks(State1, Rest, PendingLeft, AddedNow)
            end;
        {error, missing_parents} ->
            promote_pending_blocks(State, Rest, PendingLeft#{ Id => Block }, AddedNow);
        {error, _} ->
            promote_pending_blocks(State, Rest, PendingLeft, AddedNow)
    end.

%% @doc Enqueue a pending payload when we cannot yet build a valid block.
enqueue_pending_message(State, Message) ->
    Pending0 = state_get(pending_msgs, State, []),
    {State#{ pending_msgs := Pending0 ++ [Message] }, length(Pending0) + 1}.

%% @doc Drain pending messages, producing local blocks when possible.
drain_pending_messages(State) ->
    drain_pending_messages(State, []).

drain_pending_messages(State, Acc) ->
    Pending = state_get(pending_msgs, State, []),
    case Pending of
        [] -> {State, lists:reverse(Acc)};
        [Message | Rest] ->
            case make_block(State, Message) of
                {ok, Block} ->
                    case add_block(State, Block) of
                        {State1, added} ->
                            drain_pending_messages(
                                State1#{ pending_msgs := Rest },
                                [Block | Acc]
                            );
                        {State1, _} ->
                            {State1#{ pending_msgs := [Message | Rest] }, lists:reverse(Acc)}
                    end;
                {error, _} ->
                    {State#{ pending_msgs := [Message | Rest] }, lists:reverse(Acc)}
            end
    end.

%% @doc Broadcast a list of blocks to peers.
broadcast_blocks(_State, []) ->
    ok;
broadcast_blocks(State, Blocks) ->
    lists:foreach(fun(Block) -> broadcast_block(State, Block) end, Blocks),
    ok.

%% @doc Attempt to decide slots in order, returning newly created assignments.
%% Mysticeti-C Algorithm 3: TryDecide (mysticeti-paper/algorithms/universal_committer.tex).
try_commit(State) ->
    MaxRound = state_get(max_round, State, -1),
    LastRound = state_get(last_decided_round, State, -1),
    case MaxRound =< LastRound of
        true -> {State, []};
        false ->
            Sequence = build_decision_sequence(State, MaxRound, LastRound + 1),
            Decided = decided_prefix(Sequence),
            {State1, Assignments} = apply_decisions(State, Decided, []),
            {advance_last_decided_round(State1), Assignments}
    end.

%% @doc Build the decision sequence for rounds [MinRound, MaxRound].
build_decision_sequence(State, MaxRound, MinRound) ->
    WaveLength = state_get(wave_length, State),
    NumProposers = state_get(num_proposers, State),
    lists:foldl(
        fun(Round, SeqAcc0) ->
            lists:foldl(
                fun(Slot, SeqAcc) ->
                    RoundOffset = round_offset(Round, WaveLength),
                    Decider = decider(WaveLength, RoundOffset, Slot),
                    Wave = wave_number(Round, Decider),
                    Decision0 = direct_decide(State, Decider, Wave),
                    Decision =
                        case Decision0 of
                            undecided ->
                                try_indirect_decide(State, Decider, Wave, SeqAcc);
                            _ -> Decision0
                        end,
                    [slot_status(Round, Slot, Wave, Decision) | SeqAcc]
                end,
                SeqAcc0,
                lists:seq(NumProposers - 1, 0, -1)
            )
        end,
        [],
        lists:seq(MaxRound, MinRound, -1)
    ).

%% @doc Take the decided prefix until the first undecided slot.
decided_prefix([]) -> [];
decided_prefix([Status | Rest]) ->
    case hb_maps:get(status, Status, undefined, #{}) of
        undecided -> [];
        _ -> [Status | decided_prefix(Rest)]
    end.

%% @doc Apply decisions in order, producing assignments for committed blocks.
apply_decisions(State, [], Acc) ->
    {State, lists:reverse(Acc)};
apply_decisions(State, [Status | Rest], Acc) ->
    Round = hb_maps:get(round, Status, undefined, #{}),
    Slot = hb_maps:get(slot, Status, undefined, #{}),
    Key = {Round, Slot},
    DecidedSlots0 = state_get(decided_slots, State, #{}),
    case hb_maps:get(Key, DecidedSlots0, undefined, state_opts(State)) of
        undefined ->
            case hb_maps:get(status, Status, undefined, #{}) of
                commit ->
                    ProposerBlock = hb_maps:get(block, Status, undefined, #{}),
                    {State1, NewAssignments} = commit_proposer(State, ProposerBlock),
                    DecidedSlots1 = DecidedSlots0#{ Key => commit },
                    apply_decisions(
                        State1#{ decided_slots := DecidedSlots1 },
                        Rest,
                        NewAssignments ++ Acc
                    );
                skip ->
                    DecidedSlots1 = DecidedSlots0#{ Key => skip },
                    apply_decisions(State#{ decided_slots := DecidedSlots1 }, Rest, Acc)
            end;
        _Existing ->
            apply_decisions(State, Rest, Acc)
    end.

%% @doc Advance the last fully decided round boundary.
advance_last_decided_round(State) ->
    NumProposers = state_get(num_proposers, State),
    DecidedSlots = state_get(decided_slots, State, #{}),
    LastRound = state_get(last_decided_round, State, -1),
    advance_last_decided_round(LastRound + 1, LastRound, NumProposers, DecidedSlots, State).

advance_last_decided_round(Round, Last, NumProposers, DecidedSlots, State) ->
    case all_slots_decided(Round, NumProposers, DecidedSlots) of
        true ->
            advance_last_decided_round(Round + 1, Round, NumProposers, DecidedSlots, State);
        false ->
            State#{ last_decided_round := Last }
    end.

all_slots_decided(_Round, NumProposers, _DecidedSlots) when NumProposers =< 0 ->
    false;
%% @doc True when all proposer slots in a round are decided.
all_slots_decided(Round, NumProposers, DecidedSlots) ->
    lists:all(
        fun(Slot) -> hb_maps:is_key({Round, Slot}, DecidedSlots, #{}) end,
        lists:seq(0, NumProposers - 1)
    ).

%% @doc Direct decision rule for a proposer slot.
%% Mysticeti-C Algorithm 2: TryDirectDecide (mysticeti-paper/algorithms/baseline_committer.tex).
direct_decide(State, Decider, Wave) ->
    case skipped_proposer(State, Decider, Wave) of
        true -> skip;
        false ->
            case supported_proposer(State, Decider, Wave) of
                undefined -> undecided;
                ProposerBlockId -> {commit, ProposerBlockId}
            end
    end.

%% @doc Check whether the proposer was skipped.
%% Mysticeti-C Algorithm 1: SkippedProposer (mysticeti-paper/algorithms/consensus_utils.tex).
skipped_proposer(State, Decider, Wave) ->
    ProposerRound = proposer_round(Wave, Decider),
    ProposerId = get_predefined_proposer(State, Decider, Wave),
    VotingRound = ProposerRound + 1,
    Quorum = state_get(quorum, State),
    Opts = state_opts(State),
    VotingBlocks = round_blocks(State, VotingRound),
    Skipped =
        count_distinct_authors(
            lists:filter(
                fun(Block) ->
                    no_parent_from_author(State, Block, ProposerId)
                end,
                VotingBlocks
            ),
            Opts
        ),
    Skipped >= Quorum.

%% @doc Check whether a proposer has enough support to commit.
%% Mysticeti-C Algorithm 1: SupportedProposer (mysticeti-paper/algorithms/consensus_utils.tex).
supported_proposer(State, Decider, Wave) ->
    ProposerBlocks = get_proposer_blocks(State, Decider, Wave),
    case ProposerBlocks of
        [] -> undefined;
        _ ->
            DecisionRound = decision_round(Wave, Decider),
            DecisionBlocks = round_blocks(State, DecisionRound),
            Quorum = state_get(quorum, State),
            Opts = state_opts(State),
            lists:foldl(
                fun(BlockId, Acc) ->
                    case Acc of
                        undefined ->
                            CertBlocks =
                                lists:filter(
                                    fun(Block) ->
                                        is_certificate(State, Block, BlockId)
                                    end,
                                    DecisionBlocks
                                ),
                            case count_distinct_authors(CertBlocks, Opts) >= Quorum of
                                true -> BlockId;
                                false -> undefined
                            end;
                        _ -> Acc
                    end
                end,
                undefined,
                ProposerBlocks
            )
    end.

%% @doc Determine if a block is a certificate for the proposer.
%% Mysticeti-C Algorithm 1: IsCert (mysticeti-paper/algorithms/consensus_utils.tex).
is_certificate(State, Block, ProposerBlockId) ->
    Quorum = state_get(quorum, State),
    Opts = state_opts(State),
    Parents = hb_maps:get(<<"parents">>, Block, [], Opts),
    Dag = state_get(dag, State, #{}),
    VoteAuthors =
        lists:foldl(
            fun(ParentId, Acc) ->
                case is_vote(State, ParentId, ProposerBlockId) of
                    true ->
                        case hb_maps:get(ParentId, Dag, undefined, Opts) of
                            undefined -> Acc;
                            Parent ->
                                Author = hb_maps:get(<<"author">>, Parent, undefined, Opts),
                                case Author of
                                    undefined -> Acc;
                                    _ -> hb_maps:put(Author, true, Acc, Opts)
                                end
                        end;
                    false ->
                        Acc
                end
            end,
            #{},
            Parents
        ),
    hb_maps:size(VoteAuthors, Opts) >= Quorum.

%% @doc A parent votes for a proposer if it supports that proposer block.
%% Mysticeti-C Algorithm 1: IsVote + SupportedBlock (mysticeti-paper/algorithms/consensus_utils.tex).
is_vote(State, VoteBlockId, ProposerBlockId) ->
    Dag = state_get(dag, State, #{}),
    case hb_maps:get(ProposerBlockId, Dag, undefined, state_opts(State)) of
        undefined -> false;
        ProposerBlock ->
            Opts = state_opts(State),
            Author = hb_maps:get(<<"author">>, ProposerBlock, undefined, Opts),
            Round = hb_maps:get(<<"round">>, ProposerBlock, undefined, Opts),
            supported_block(State, VoteBlockId, Author, Round) =:= ProposerBlockId
    end.

%% @doc Compute the supported block for (author, round) in b's ancestry.
%% Mysticeti-C Algorithm 1: SupportedBlock (mysticeti-paper/algorithms/consensus_utils.tex).
supported_block(State, BlockId, Author, Round) ->
    Dag = state_get(dag, State, #{}),
    case hb_maps:get(BlockId, Dag, undefined, state_opts(State)) of
        undefined -> undefined;
        Block ->
            Opts = state_opts(State),
            case Round >= hb_maps:get(<<"round">>, Block, -1, Opts) of
                true -> undefined;
                false ->
                    Parents = hb_maps:get(<<"parents">>, Block, [], Opts),
                    supported_block_parents(State, Parents, Author, Round)
            end
    end.

%% @doc Walk parents to find the first supported block for (author, round).
supported_block_parents(_State, [], _Author, _Round) -> undefined;
supported_block_parents(State, [ParentId | Rest], Author, Round) ->
    Dag = state_get(dag, State, #{}),
    case hb_maps:get(ParentId, Dag, undefined, state_opts(State)) of
        undefined ->
            supported_block_parents(State, Rest, Author, Round);
        Parent ->
            Opts = state_opts(State),
            case {hb_maps:get(<<"author">>, Parent, undefined, Opts),
                  hb_maps:get(<<"round">>, Parent, undefined, Opts)} of
                {Author, Round} -> ParentId;
                _ ->
                    case supported_block(State, ParentId, Author, Round) of
                        undefined -> supported_block_parents(State, Rest, Author, Round);
                        Res -> Res
                    end
            end
    end.

%% @doc Check if a block has a specific ancestor (Link predicate).
%% Mysticeti-C Algorithm 1: Link (mysticeti-paper/algorithms/consensus_utils.tex).
link(State, OldId, NewId) ->
    has_ancestor(State, NewId, OldId).

%% @doc Check if a block has a specific ancestor via parent traversal.
has_ancestor(State, StartId, TargetId) ->
    case StartId == TargetId of
        true -> true;
        false ->
            Dag = state_get(dag, State, #{}),
            Opts = state_opts(State),
            has_ancestor(Dag, [StartId], TargetId, #{}, Opts)
    end.

has_ancestor(_Dag, [], _TargetId, _Visited, _Opts) -> false;
has_ancestor(Dag, [Id | Rest], TargetId, Visited, Opts) ->
    case hb_maps:is_key(Id, Visited, Opts) of
        true -> has_ancestor(Dag, Rest, TargetId, Visited, Opts);
        false ->
            case hb_maps:get(Id, Dag, undefined, Opts) of
                undefined ->
                    has_ancestor(Dag, Rest, TargetId, Visited#{ Id => true}, Opts);
                Block ->
                    Parents = hb_maps:get(<<"parents">>, Block, [], Opts),
                    case lists:member(TargetId, Parents) of
                        true -> true;
                        false ->
                            has_ancestor(
                                Dag,
                                Parents ++ Rest,
                                TargetId,
                                Visited#{ Id => true },
                                Opts
                            )
                    end
            end
    end.

%% @doc Indirect decision rule for a proposer slot.
%% Mysticeti-C Algorithm 3: TryIndirectDecide (mysticeti-paper/algorithms/universal_committer.tex).
try_indirect_decide(State, Decider, Wave, Sequence) ->
    DecisionRound = decision_round(Wave, Decider),
    Anchors =
        lists:filter(
            fun(#{ round := R }) -> R > DecisionRound end,
            Sequence
        ),
    try_indirect_anchors(State, Decider, Wave, Anchors).

%% @doc Walk anchors for TryIndirectDecide, returning commit/skip/undecided.
try_indirect_anchors(_State, _Decider, _Wave, []) ->
    undecided;
try_indirect_anchors(State, Decider, Wave, [Anchor | Rest]) ->
    case hb_maps:get(status, Anchor, undefined, #{}) of
        undecided -> undecided;
        skip -> try_indirect_anchors(State, Decider, Wave, Rest);
        commit ->
            ProposerBlocks = get_proposer_blocks(State, Decider, Wave),
            case ProposerBlocks of
                [] -> undecided;
                _ ->
                    AnchorBlockId = hb_maps:get(block, Anchor, undefined, #{}),
                    case certified_link_any(State, Decider, AnchorBlockId, ProposerBlocks) of
                        undefined -> skip;
                        BlockId -> {commit, BlockId}
                    end
            end
    end.

%% @doc Return the first proposer block with a certified link to the anchor.
certified_link_any(_State, _Decider, _AnchorBlockId, []) ->
    undefined;
certified_link_any(State, Decider, AnchorBlockId, [BlockId | Rest]) ->
    case certified_link(State, Decider, AnchorBlockId, BlockId) of
        true -> BlockId;
        false -> certified_link_any(State, Decider, AnchorBlockId, Rest)
    end.

%% @doc Check if there is a certified link between the anchor and the proposer.
%% Mysticeti-C Algorithm 1: CertifiedLink (mysticeti-paper/algorithms/consensus_utils.tex).
certified_link(State, Decider, AnchorBlockId, ProposerBlockId) ->
    Dag = state_get(dag, State, #{}),
    Opts = state_opts(State),
    ProposerBlock = hb_maps:get(ProposerBlockId, Dag, undefined, Opts),
    Wave = wave_number(hb_maps:get(<<"round">>, ProposerBlock, undefined, Opts), Decider),
    DecisionRound = decision_round(Wave, Decider),
    DecisionBlocks = round_blocks(State, DecisionRound),
    lists:any(
        fun(Block) ->
            is_certificate(State, Block, ProposerBlockId) andalso
                link(State, hb_maps:get(<<"id">>, Block, undefined, Opts), AnchorBlockId)
        end,
        DecisionBlocks
    ).

%% @doc Commit a proposer block and produce assignments for new blocks.
%% Uses the committed past and a deterministic linearization that removes
%% duplicate sequence numbers (mysticeti-paper/sections/security.tex).
commit_proposer(State, ProposerBlockId) ->
    Past = collect_ancestors(State, ProposerBlockId),
    Opts = state_opts(State),
    Dag = state_get(dag, State, #{}),
    Ordered = state_get(ordered, State, #{}),
    NewBlockIds =
        lists:filter(
            fun(Id) ->
                case hb_maps:get(Id, Dag, undefined, Opts) of
                    undefined ->
                        false;
                    Block ->
                        Key =
                            {hb_maps:get(<<"author">>, Block, undefined, Opts),
                             hb_maps:get(<<"round">>, Block, undefined, Opts)},
                        not hb_maps:is_key(Key, Ordered, Opts)
                end
            end,
            Past
        ),
    OrderedBlocks = order_blocks(State, NewBlockIds),
    {State1, Assignments} = assign_blocks(State, OrderedBlocks, []),
    Ordered1 =
        lists:foldl(
            fun(Id, Acc) ->
                case hb_maps:get(Id, Dag, undefined, Opts) of
                    undefined -> Acc;
                    Block ->
                        Key =
                            {hb_maps:get(<<"author">>, Block, undefined, Opts),
                             hb_maps:get(<<"round">>, Block, undefined, Opts)},
                        hb_maps:put(Key, true, Acc, Opts)
                end
            end,
            Ordered,
            OrderedBlocks
        ),
    {State1#{ ordered := Ordered1 }, Assignments}.

%% @doc Collect all ancestors (including the block itself).
collect_ancestors(State, RootId) ->
    Dag = state_get(dag, State, #{}),
    Opts = state_opts(State),
    collect_ancestors(Dag, [RootId], #{}, [], Opts).

collect_ancestors(_Dag, [], _Visited, Acc, _Opts) -> Acc;
collect_ancestors(Dag, [Id | Rest], Visited, Acc, Opts) ->
    case hb_maps:is_key(Id, Visited, Opts) of
        true -> collect_ancestors(Dag, Rest, Visited, Acc, Opts);
        false ->
            case hb_maps:get(Id, Dag, undefined, Opts) of
                undefined ->
                    collect_ancestors(
                        Dag,
                        Rest,
                        hb_maps:put(Id, true, Visited, Opts),
                        Acc,
                        Opts
                    );
                Block ->
                    Parents = hb_maps:get(<<"parents">>, Block, [], Opts),
                    collect_ancestors(
                        Dag,
                        Parents ++ Rest,
                        hb_maps:put(Id, true, Visited, Opts),
                        [Id | Acc],
                        Opts
                    )
            end
    end.

%% @doc Order blocks deterministically by (round, author, id), after
%% removing equivocations for the same (author, round).
order_blocks(State, BlockIds) ->
    Opts = state_opts(State),
    Dag = state_get(dag, State, #{}),
    UniqueIds = dedupe_blocks_by_author_round(State, BlockIds),
    lists:sort(
        fun(A, B) ->
            BA = hb_maps:get(A, Dag, undefined, Opts),
            BB = hb_maps:get(B, Dag, undefined, Opts),
            {hb_maps:get(<<"round">>, BA, undefined, Opts),
             hb_maps:get(<<"author">>, BA, undefined, Opts),
             A}
                < {hb_maps:get(<<"round">>, BB, undefined, Opts),
                   hb_maps:get(<<"author">>, BB, undefined, Opts),
                   B}
        end,
        UniqueIds
    ).

%% @doc Choose a deterministic block id among equivocations.
pick_deterministic_block_id(IdA, IdB) when IdA =< IdB -> IdA;
pick_deterministic_block_id(_IdA, IdB) -> IdB.

%% @doc Remove multiple blocks for the same (author, round) by choosing the
%% smallest id, matching the paper’s “remove duplicate sequence numbers” rule.
dedupe_blocks_by_author_round(State, BlockIds) ->
    Opts = state_opts(State),
    Dag = state_get(dag, State, #{}),
    Chosen =
        lists:foldl(
            fun(Id, Acc) ->
                case hb_maps:get(Id, Dag, undefined, Opts) of
                    undefined -> Acc;
                    Block ->
                        Author = hb_maps:get(<<"author">>, Block, undefined, Opts),
                        Round = hb_maps:get(<<"round">>, Block, undefined, Opts),
                        Key = {Author, Round},
                        case hb_maps:get(Key, Acc, undefined, Opts) of
                            undefined ->
                                hb_maps:put(Key, Id, Acc, Opts);
                            Existing ->
                                hb_maps:put(
                                    Key,
                                    pick_deterministic_block_id(Id, Existing),
                                    Acc,
                                    Opts
                                )
                        end
                end
            end,
            #{},
            BlockIds
        ),
    hb_maps:values(Chosen, Opts).

%% @doc Create assignments for ordered blocks.
%% Only blocks with a payload (body) produce assignments.
assign_blocks(State, [], Acc) -> {State, lists:reverse(Acc)};
assign_blocks(State, [Id | Rest], Acc) ->
    Opts = state_opts(State),
    Dag = state_get(dag, State, #{}),
    case hb_maps:get(Id, Dag, undefined, Opts) of
        undefined ->
            assign_blocks(State, Rest, Acc);
        Block ->
            Payload = hb_maps:get(<<"body">>, Block, undefined, Opts),
            case Payload of
                undefined ->
                    assign_blocks(State, Rest, Acc);
                _ ->
                    MsgId = hb_message:id(Payload, all, Opts),
                    Assigned0 = state_get(assigned, State, #{}),
                    case hb_maps:is_key(MsgId, Assigned0, Opts) of
                        true ->
                            assign_blocks(State, Rest, Acc);
                        false ->
                            {Assignment, State1} = make_assignment(State, Payload),
                            assign_blocks(
                                State1#{
                                    assigned :=
                                        hb_maps:put(
                                            MsgId,
                                            true,
                                            state_get(assigned, State1, #{}),
                                            Opts
                                        )
                                },
                                Rest,
                                [{MsgId, Assignment} | Acc]
                            )
                    end
            end
    end.

%% @doc Generate an assignment for a payload message.
make_assignment(State, Message) ->
    Opts = state_opts(State),
    BaseStateHashpath = base_state(State),
    NextSlot = state_get(current_slot, State, -1) + 1,
    {Timestamp, Height, Hash} = ar_timestamp:get(),
    Path =
        case hb_path:from_message(request, Message, Opts) of
            undefined -> <<"compute">>;
            P -> hb_path:to_binary(P)
        end,
    Assignment0 =
        #{
            <<"path">> => Path,
            <<"data-protocol">> => <<"ao">>,
            <<"variant">> => <<"ao.N.1">>,
            <<"process">> => hb_util:id(state_get(id, State)),
            <<"epoch">> => <<"0">>,
            <<"slot">> => NextSlot,
            <<"block-height">> => Height,
            <<"block-hash">> => hb_util:human_id(Hash),
            <<"block-timestamp">> => Timestamp,
            <<"timestamp">> => scheduler_time(),
            <<"base-hashpath">> => BaseStateHashpath,
            <<"body">> => Message,
            <<"type">> => <<"Assignment">>
        },
    Assignment = commit_assignment(Assignment0, State),
    ok = dev_scheduler_cache:write(Assignment, Opts),
    NextHashpath = next_hashpath(BaseStateHashpath, Assignment, State),
    {
        Assignment,
        State#{
            current_slot := NextSlot,
            base_state_hashpath := NextHashpath
        }
    }.

%% @doc Commit to the assignment using all appropriate wallets.
commit_assignment(BaseAssignment, State) ->
    Wallets = state_get(wallets, State, []),
    Opts = state_opts(State),
    CommitmentSpec = state_get(commitment_spec, State),
    lists:foldr(
        fun(Wallet, Assignment) ->
            hb_message:commit(
                Assignment,
                Opts#{ priv_wallet => Wallet },
                CommitmentSpec
            )
        end,
        BaseAssignment,
        Wallets
    ).

%% @doc Return the base state hashpath.
base_state(S = #{ base_state_hashpath := undefined }) ->
    hb_util:id(state_get(id, S));
base_state(#{ base_state_hashpath := BaseStateHashpath }) ->
    BaseStateHashpath.

%% @doc Generate the next hashpath for a new assignment.
next_hashpath(BaseStateHashpath, NewAssignment, State) ->
    Opts = state_opts(State),
    HashpathAlg = hb_path:hashpath_alg(NewAssignment, Opts),
    hb_path:hashpath(
        BaseStateHashpath,
        hb_message:id(NewAssignment, all, Opts),
        HashpathAlg,
        Opts
    ).

%% @doc Build a DirectDecider descriptor (Algorithm 3).
decider(WaveLength, RoundOffset, SlotOffset) ->
    #{
        wave_length => WaveLength,
        round_offset => RoundOffset,
        slot_offset => SlotOffset
    }.

%% @doc RoundOffset for a round (Algorithm 3, line 8).
round_offset(Round, WaveLength) ->
    Round rem WaveLength.

%% @doc Convert an arbitrary round to its wave number (Algorithm 2).
wave_number(Round, #{ wave_length := WaveLength, round_offset := RoundOffset }) ->
    (Round - RoundOffset) div WaveLength.

%% @doc Determine the round for the proposer in a given wave (Algorithm 2).
proposer_round(Wave, #{ wave_length := WaveLength, round_offset := RoundOffset }) ->
    Wave * WaveLength + RoundOffset.

%% @doc Determine the decision round for a given wave (Algorithm 2).
decision_round(Wave, #{ wave_length := WaveLength, round_offset := RoundOffset }) ->
    Wave * WaveLength + WaveLength - 1 + RoundOffset.

%% @doc Select a proposer deterministically (round-robin).
%% Mysticeti-C Algorithm 2: PredefinedProposer (mysticeti-paper/algorithms/baseline_committer.tex).
predefined_proposer(State, Index) ->
    Validators = state_get(validators, State, []),
    BaseOffset = state_get(proposer_offset, State, 0),
    SlotIndex = (Index + BaseOffset) rem length(Validators),
    lists:nth(SlotIndex + 1, Validators).

%% @doc Get the predefined proposer for a slot (Algorithm 2).
get_predefined_proposer(State, Decider, Wave) ->
    ProposerRound = proposer_round(Wave, Decider),
    SlotOffset = hb_maps:get(slot_offset, Decider, 0, #{}),
    predefined_proposer(State, ProposerRound + SlotOffset).

%% @doc Lookup proposer blocks for a slot (Algorithm 1).
get_proposer_blocks(State, Decider, Wave) ->
    ProposerRound = proposer_round(Wave, Decider),
    ProposerId = get_predefined_proposer(State, Decider, Wave),
    lists:sort(block_by_author_round(State, ProposerId, ProposerRound)).

%% @doc Build a status record for a slot.
slot_status(Round, Slot, Wave, Decision) ->
    case Decision of
        undecided ->
            #{ round => Round, slot => Slot, wave => Wave, status => undecided };
        skip ->
            #{ round => Round, slot => Slot, wave => Wave, status => skip };
        {commit, BlockId} ->
            #{ round => Round, slot => Slot, wave => Wave, status => commit, block => BlockId }
    end.

%% @doc Lookup blocks by author and round.
block_by_author_round(State, Author, Round) ->
    author_blocks_in_round(State, Author, Round).

%% @doc Get all blocks for a round.
round_blocks(State, Round) ->
    Opts = state_opts(State),
    Dag = state_get(dag, State, #{}),
    lists:filtermap(
        fun(Id) ->
            case hb_maps:get(Id, Dag, undefined, Opts) of
                undefined -> false;
                Block -> {true, Block}
            end
        end,
        round_block_ids(State, Round)
    ).

%% @doc Count distinct authors in a list of blocks.
count_distinct_authors(Blocks, Opts) ->
    Authors =
        lists:foldl(
            fun(Block, Acc) ->
                Author = hb_maps:get(<<"author">>, Block, undefined, Opts),
                case Author of
                    undefined -> Acc;
                    _ -> hb_maps:put(Author, true, Acc, Opts)
                end
            end,
            #{},
            Blocks
        ),
    hb_maps:size(Authors, Opts).

%% @doc True if no parent is authored by the given id.
no_parent_from_author(State, Block, AuthorId) ->
    Opts = state_opts(State),
    Parents = hb_maps:get(<<"parents">>, Block, [], Opts),
    Dag = state_get(dag, State, #{}),
    lists:all(
        fun(ParentId) ->
            case hb_maps:get(ParentId, Dag, undefined, Opts) of
                undefined -> true;
                Parent ->
                    hb_maps:get(<<"author">>, Parent, undefined, Opts) =/= AuthorId
            end
        end,
        Parents
    ).

%% @doc Validate a received block.
%% Block correctness rules: mysticeti-paper/sections/overview.tex (sec:dag).
validate_block(State, Block0) ->
    Opts = state_opts(State),
    case ensure_block_id(Block0, Opts) of
        {error, _} = Err -> Err;
        Block ->
            BlockNoId = hb_maps:remove(<<"id">>, Block, Opts),
            Validators = state_get(validators, State, []),
            AuthorField = hb_maps:get(<<"author">>, Block, undefined, Opts),
            Round = hb_maps:get(<<"round">>, Block, undefined, Opts),
            Parents = hb_maps:get(<<"parents">>, Block, undefined, Opts),
            case verify_block_signature(BlockNoId, Opts) of
                {ok, Signer} ->
                    case AuthorField of
                        Signer ->
                            Author = Signer,
                            case {hb_maps:get(<<"process">>, Block, undefined, Opts) =:= state_get(id, State),
                                  hb_maps:get(<<"type">>, Block, undefined, Opts) =:= <<"MysticetiBlock">>,
                                  lists:member(Author, Validators),
                                  is_integer(Round),
                                  Round >= 0,
                                  is_list(Parents)} of
                                {false, _, _, _, _, _} -> {error, wrong_process};
                                {_, false, _, _, _, _} -> {error, wrong_type};
                                {_, _, false, _, _, _} -> {error, invalid_author};
                                {_, _, _, false, _, _} -> {error, invalid_round};
                                {_, _, _, _, false, _} -> {error, invalid_round};
                                {_, _, _, _, _, false} -> {error, invalid_parents};
                                _ ->
                                    validate_block_parents(State, Block)
                            end;
                        undefined ->
                            {error, missing_author};
                        _ ->
                            {error, signer_mismatch}
                    end;
                {error, _} = Err -> Err
            end
    end.

%% @doc Verify block signature and return the committer as the author.
%% Block correctness rules: mysticeti-paper/sections/overview.tex (sec:dag).
verify_block_signature(BlockNoId, Opts) ->
    try hb_message:verify(BlockNoId, all, Opts) of
        true ->
            Signers = hb_message:signers(BlockNoId, Opts),
            case Signers of
                [Signer] -> {ok, Signer};
                _ -> {error, signer_mismatch}
            end;
        false ->
            {error, invalid_signature}
    catch _:_ ->
        {error, invalid_signature}
    end.

validate_block_parents(State, Block) ->
    Opts = state_opts(State),
    Round = hb_maps:get(<<"round">>, Block, undefined, Opts),
    Parents = hb_maps:get(<<"parents">>, Block, [], Opts),
    Dag = state_get(dag, State, #{}),
    case length(Parents) =:= length(lists:usort(Parents)) of
        false -> {error, duplicate_parents};
        true ->
            ParentBlocks =
                lists:map(
                    fun(ParentId) -> {ParentId, hb_maps:get(ParentId, Dag, undefined, Opts)} end,
                    Parents
                ),
            case lists:any(fun({_, B}) -> B =:= undefined end, ParentBlocks) of
                true -> {error, missing_parents};
                false ->
                    case lists:any(
                        fun({_, B}) ->
                            hb_maps:get(<<"round">>, B, -1, Opts) >= Round
                        end,
                        ParentBlocks
                    ) of
                        true -> {error, invalid_parent_round};
                        false -> validate_block_parent_shape(State, Block, ParentBlocks)
                    end
            end
    end.

validate_block_parent_shape(State, Block, ParentBlocks) ->
    Opts = state_opts(State),
    Round = hb_maps:get(<<"round">>, Block, undefined, Opts),
    case Round of
        0 ->
            case hb_maps:get(<<"parents">>, Block, [], Opts) of
                [] -> {ok, Block};
                _ -> {error, genesis_parents_not_empty}
            end;
        _ ->
            Author = hb_maps:get(<<"author">>, Block, undefined, Opts),
            Quorum = state_get(quorum, State),
            Parents = hb_maps:get(<<"parents">>, Block, [], Opts),
            case Parents of
                [] ->
                    {error, missing_parents};
                [FirstParentId | _] ->
                    PrevIds = latest_author_blocks(State, Author, Round),
                    case lists:member(FirstParentId, PrevIds) of
                        false -> {error, invalid_prev_parent};
                        true ->
                            case lists:keyfind(FirstParentId, 1, ParentBlocks) of
                                {_, FirstParent} ->
                                    case hb_maps:get(<<"author">>, FirstParent, undefined, Opts) of
                                        Author ->
                                            % Block correctness: require 2f+1 distinct authors from round r-1.
                                            PrevRoundAuthors =
                                                lists:foldl(
                                                    fun({_, PB}, Acc) ->
                                                        case hb_maps:get(<<"round">>, PB, -1, Opts) of
                                                            R when R =:= Round - 1 ->
                                                                AuthorId =
                                                                    hb_maps:get(
                                                                        <<"author">>,
                                                                        PB,
                                                                        undefined,
                                                                        Opts
                                                                    ),
                                                                case AuthorId of
                                                                    undefined -> Acc;
                                                                    _ ->
                                                                        hb_maps:put(
                                                                            AuthorId,
                                                                            true,
                                                                            Acc,
                                                                            Opts
                                                                        )
                                                                end;
                                                            _ ->
                                                                Acc
                                                        end
                                                    end,
                                                    #{},
                                                    ParentBlocks
                                                ),
                                            case hb_maps:size(PrevRoundAuthors, Opts) >= Quorum of
                                                true -> {ok, Block};
                                                false -> {error, not_enough_prev_round_parents}
                                            end;
                                        _ ->
                                            {error, invalid_prev_parent}
                                    end;
                                false ->
                                    {error, invalid_prev_parent}
                            end
                    end
            end
    end.

%% @doc Summarize the server state for info.
summarize(State) ->
    Opts = state_opts(State),
    #{
        current => hb_maps:get(current_slot, State, -1, Opts),
        max_round => hb_maps:get(max_round, State, -1, Opts),
        stakers => hb_maps:get(stakers, State, [], Opts),
        validators => hb_maps:get(validators, State, [], Opts),
        quorum => hb_maps:get(quorum, State, 0, Opts),
        wave_length => hb_maps:get(wave_length, State, 0, Opts),
        num_proposers => hb_maps:get(num_proposers, State, 0, Opts),
        last_decided_round => hb_maps:get(last_decided_round, State, -1, Opts),
        local_author => hb_maps:get(local_author, State, undefined, Opts),
        peers => hb_maps:get(peers, State, [], Opts)
    }.

%% @doc Resolve a required Mysticeti config key (strict mode).
mysticeti_required(Proc, Key, Opts) ->
    case hb_ao:get(
        << "mysticeti/", Key/binary >>,
        Proc,
        not_found,
        Opts#{ hashpath => ignore }
    ) of
        not_found -> throw({missing_mysticeti_config, Key});
        Value -> Value
    end.

%% @doc Extract wave length (Algorithm 2) from the process config.
%% Mysticeti-C Algorithm 2: waveLength (mysticeti-paper/algorithms/baseline_committer.tex).
wave_length(Proc, Opts) ->
    WaveLength = hb_util:int(mysticeti_required(Proc, <<"wave-length">>, Opts)),
    case WaveLength >= 1 of
        true -> WaveLength;
        false -> throw({invalid_wave_length, WaveLength})
    end.

%% @doc Extract proposer offset (Algorithm 2) from the process config.
%% Mysticeti-C Algorithm 2: PredefinedProposer offset (mysticeti-paper/algorithms/baseline_committer.tex).
proposer_offset(Proc, Opts) ->
    Offset = hb_util:int(mysticeti_required(Proc, <<"proposer-offset">>, Opts)),
    case Offset >= 0 of
        true -> Offset;
        false -> throw({invalid_proposer_offset, Offset})
    end.

%% @doc Extract stakers (validator + stake) from `mysticeti/stakers`.
stakers(Proc, Opts) ->
    Raw = mysticeti_required(Proc, <<"stakers">>, Opts),
    normalize_stakers(hb_cache:ensure_all_loaded(Raw, Opts), Opts).

%% @doc Extract validator ids from normalized stakers.
validators_from_stakers(Stakers) ->
    [Id || #{ id := Id } <- Stakers].

%% @doc Normalize stakers into a list of #{id, stake} maps.
normalize_stakers(Stakers, Opts) when is_map(Stakers) ->
    normalize_stakers(
        [#{ id => Id, stake => Stake }
         || {Id, Stake} <- lists:sort(hb_maps:to_list(Stakers, Opts))],
        Opts
    );
normalize_stakers(Stakers, Opts) when is_list(Stakers) ->
    Loaded = hb_cache:ensure_loaded(Stakers, Opts),
    {_, Normalized} =
        lists:foldl(
            fun(Item, {Seen, Acc}) ->
                case normalize_staker(Item, Opts) of
                    {ok, #{ id := Id } = Staker} ->
                        case hb_maps:is_key(Id, Seen, Opts) of
                            true -> {Seen, Acc};
                            false ->
                                {hb_maps:put(Id, true, Seen, Opts), Acc ++ [Staker]}
                        end;
                    error ->
                        {Seen, Acc}
                end
            end,
            {#{}, []},
            Loaded
        ),
    Normalized;
normalize_stakers(Stakers, Opts) ->
    normalize_stakers([Stakers], Opts).

%% @doc Normalize a single staker entry into #{id, stake}.
normalize_staker(Map, Opts) when is_map(Map) ->
    Id0 = hb_maps:get(<<"id">>, Map, hb_maps:get(id, Map, undefined, Opts), Opts),
    case Id0 of
        undefined -> error;
        _ ->
            Stake0 = hb_maps:get(<<"stake">>, Map, hb_maps:get(stake, Map, 1, Opts), Opts),
            {ok, #{ id => hb_util:bin(Id0), stake => hb_util:int(Stake0) }}
    end;
normalize_staker({Id, Stake}, Opts) ->
    CleanId = hb_cache:ensure_loaded(Id, Opts),
    {ok, #{ id => hb_util:bin(CleanId), stake => hb_util:int(Stake) }};
normalize_staker(Id, Opts) ->
    CleanId = hb_cache:ensure_loaded(Id, Opts),
    {ok, #{ id => hb_util:bin(CleanId), stake => 1 }}.

%% @doc Resolve the local author identity from opts.
local_author(_Validators, Opts) ->
    hb_opts:get(mysticeti_author, hb:address(), Opts).

%% @doc Ensure the local author is in the validator set.
ensure_local_author(Validators, LocalAuthor) ->
    case lists:member(LocalAuthor, Validators) of
        true -> ok;
        false -> throw({local_author_not_in_staker_set, LocalAuthor})
    end.

%% @doc Determine the number of proposer slots per round.
num_proposers(Proc, Validators, Opts) ->
    Num = hb_util:int(mysticeti_required(Proc, <<"num-proposers">>, Opts)),
    case Num >= 1 andalso Num =< length(Validators) of
        true -> Num;
        false -> throw({invalid_num_proposers, Num})
    end.

%% @doc Extract peer nodes for gossip from `mysticeti/peers`.
peers(Proc, Opts) ->
    Raw = mysticeti_required(Proc, <<"peers">>, Opts),
    normalize_peers(Raw, Opts).

%% @doc Normalize peer inputs into a list of URLs.
normalize_peers(Peers, Opts) when is_list(Peers) ->
    Loaded = hb_cache:ensure_loaded(Peers, Opts),
    lists:usort(
        lists:filtermap(
            fun(Peer) -> resolve_peer(Peer, Opts) end,
            Loaded
        )
    );
normalize_peers(Peers, Opts) when is_binary(Peers) ->
    normalize_peers(dev_scheduler:parse_schedulers(Peers), Opts);
normalize_peers(Peers, Opts) when is_map(Peers) ->
    normalize_peers([Peers], Opts);
normalize_peers(_, _Opts) ->
    [].

%% @doc Resolve a peer descriptor to a URL or return false.
resolve_peer(Peer, Opts) when is_binary(Peer) ->
    case is_url(Peer) of
        true -> {true, Peer};
        false -> resolve_peer_location(Peer, Opts)
    end;
resolve_peer(Peer, Opts) when is_map(Peer) ->
    case hb_ao:get(<<"url">>, Peer, not_found, Opts) of
        not_found -> false;
        Url -> resolve_peer(Url, Opts)
    end;
resolve_peer(Peer, Opts) ->
    resolve_peer(hb_util:bin(Peer), Opts).

%% @doc Resolve a peer address via cached or gateway scheduler-location data.
resolve_peer_location(Address, Opts) ->
    case dev_scheduler_cache:read_location(Address, Opts) of
        {ok, Location} ->
            case hb_ao:get(<<"url">>, Location, not_found, Opts) of
                not_found -> false;
                Url -> resolve_peer(Url, Opts)
            end;
        not_found ->
            case hb_gateway_client:scheduler_location(Address, Opts) of
                {ok, Location} ->
                    _ = dev_scheduler_cache:write_location(Location, Opts),
                    case hb_ao:get(<<"url">>, Location, not_found, Opts) of
                        not_found -> false;
                        Url -> resolve_peer(Url, Opts)
                    end;
                {error, _} ->
                    false
            end
    end.

%% @doc True when a binary is an HTTP or HTTPS URL.
is_url(<<"http://", _/binary>>) -> true;
is_url(<<"https://", _/binary>>) -> true;
is_url(_) -> false.

%% @doc Broadcast a block to configured peers.
broadcast_block(State, Block) ->
    Peers = state_get(peers, State, []),
    Opts = state_opts(State),
    BlockId = hb_message:id(Block, all, Opts),
    ensure_message_id_cached(BlockId, Block, Opts),
    BlockLoaded = hb_cache:ensure_all_loaded(Block, Opts),
    Req0 =
        #{
            <<"path">> => <<"/~mystislopi@1.0-pre/block">>,
            <<"method">> => <<"POST">>,
            <<"body">> => BlockLoaded
        },
    Req = hb_message:commit(Req0, Opts),
    lists:foreach(
        fun(Node) ->
            spawn(fun() ->
                case catch hb_http:post(Node, Req, Opts) of
                    {ok, _} ->
                        ok;
                    Error ->
                        ?event(error,
                            {mysticeti_broadcast_failed,
                                {peer, Node},
                                {error, Error}
                            }
                        )
                end
            end)
        end,
        Peers
    ),
    ok.

%% @doc Determine the appropriate list of keys to use to commit assignments.
commitment_wallets(ProcMsg, Opts) ->
    SchedulerVal =
        hb_ao:get_first(
            [
                {ProcMsg, <<"scheduler">>},
                {ProcMsg, <<"scheduler-location">>}
            ],
            [],
            Opts
        ),
    lists:filtermap(
        fun(Scheduler) ->
            case hb_opts:as(Scheduler, Opts) of
                {ok, #{ priv_wallet := Wallet }} -> {true, Wallet};
                _ -> false
            end
        end,
        dev_scheduler:parse_schedulers(SchedulerVal)
    ).

%% @doc Returns the commitment specification to use for assignments.
commitment_spec(Proc, Opts) ->
    hb_ao:get(
        <<"scheduler-commitment-spec">>,
        {as, <<"message@1.0">>, Proc},
        hb_opts:get(
            scheduler_default_commitment_spec,
            <<"ans104@1.0">>,
            Opts
        ),
        Opts
    ).

%% @doc Return current time in milliseconds.
scheduler_time() ->
    erlang:system_time(millisecond).
