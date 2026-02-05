%%% @doc Mysticeti-style consensus scheduler server.
%%%
%%% This server maintains a per-process DAG of blocks and turns committed blocks
%%% into a total order of assignments. It implements both the direct and
%%% indirect decision rules from Mysticeti-C (Algorithms 1–3).
%%%
%%% Plain-English overview (aimed at readers familiar with Bitcoin):
%%% - Bitcoin builds a single chain by choosing the longest valid chain. Here we
%%%   allow multiple blocks per round (a DAG), but we still need a single,
%%%   deterministic order for process execution.
%%% - Time is divided into rounds. For each round, there is a deterministic
%%%   proposer (round-robin over validators).
%%% - A "wave" is a small fixed number of rounds (default 3):
%%%   proposer round, voting round, and decision round.
%%% - In the voting round, validators are expected to build blocks that reference
%%%   the proposer block as a parent. In the decision round, validators build
%%%   blocks whose parents include enough voting blocks. These decision blocks
%%%   act like a certificate.
%%% - If there are at least 2f+1 decision blocks that each reference at least
%%%   2f+1 voting blocks that (directly or indirectly) point to the proposer,
%%%   the proposer is committed. If at least 2f+1 voting blocks *omit* the
%%%   proposer as a parent, the proposer is skipped for that wave.
%%% - If the direct rule cannot decide a wave, the indirect rule uses a later
%%%   decided wave (an "anchor") to decide the earlier one. If the anchor is a
%%%   commit and there is a certified link between the anchor and the proposer,
%%%   the proposer is committed; otherwise it is skipped.
%%% - Once a proposer is committed, we take the committed past of that proposer,
%%%   order it deterministically, and emit assignments in that order. This
%%%   produces the single total order required by `process@1.0`.
%%%
%%% Implementation notes:
%%% - Validators are derived from the staker set in the process state; f =
%%%   floor((n-1)/3), quorum = 2f+1.
%%% - Blocks are signature-verified and validated against Mysticeti-C's
%%%   structure and parent rules (mysticeti-paper/sections/overview.tex,
%%%   "Block correctness").
%%% - The decision rules implement Algorithms 1–3 from the paper as written.
%%%
%%% Reference: "Mysticeti: Reaching the Limits of Latency with Uncertified DAGs"
%%% (Babel et al., arXiv:2310.14821).
-module(dev_mysticeti_server).
-export([start/3, schedule/2, ingest_block/2, info/1, stop/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Default schedule timeout (ms)
-define(DEFAULT_TIMEOUT, 10000).

%% @doc Start a consensus scheduling server for a given process.
start(ProcID, Proc, Opts) ->
    ?event(mysticeti, {starting_server, {proc_id, ProcID}}),
    spawn_link(
        fun() ->
            case hb_name:register({<<"mysticeti@1.0">>, ProcID}) of
                ok -> ok;
                error ->
                    throw({another_mysticeti_scheduler_is_already_registered,
                        {proc_id, ProcID}})
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
    Opts = maps:get(opts, State),
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
            Block0 =
                #{
                    <<"type">> => <<"MysticetiBlock">>,
                    <<"process">> => maps:get(id, State),
                    <<"author">> => maps:get(local_author, State),
                    <<"round">> => Round,
                    <<"parents">> => Parents,
                    <<"timestamp">> => scheduler_time(),
                    <<"body">> => Payload
                },
            Opts = maps:get(opts, State),
            Signed = hb_message:commit(Block0, Opts),
            case ensure_block_id(Signed, Opts) of
                {error, _} = Err -> Err;
                Block ->
                    BlockNoId = maps:remove(<<"id">>, Block),
                    case verify_block_signature(BlockNoId, Opts) of
                        {ok, Signer} ->
                            case hb_maps:get(<<"author">>, Block, undefined, Opts) of
                                Signer -> {ok, Block};
                                _ -> {error, signer_mismatch}
                            end;
                        {error, _} = Err -> Err
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Determine the next round for the local author.
next_round(State) ->
    LocalAuthor = maps:get(local_author, State),
    AuthorRounds = maps:get(author_rounds, State, #{}),
    maps:get(LocalAuthor, AuthorRounds, -1) + 1.

%% @doc Select parent blocks for a new round.
parent_set(_State, Round) when Round < 0 ->
    {error, invalid_round};
parent_set(_State, 0) ->
    {ok, []};
parent_set(State, Round) ->
    PrevRound = Round - 1,
    RoundIndex = maps:get(round_index, State, #{}),
    case maps:get(PrevRound, RoundIndex, undefined) of
        undefined -> {error, missing_prev_round};
        ByAuthor ->
            Quorum = maps:get(quorum, State),
            case maps:size(ByAuthor) < Quorum of
                true -> {error, not_enough_parents};
                false ->
                    LocalAuthor = maps:get(local_author, State),
                    case maps:get(LocalAuthor, ByAuthor, undefined) of
                        undefined -> {error, missing_local_prev};
                        LocalPrev ->
                            Ordered =
                                lists:sort(
                                    fun({A1, _}, {A2, _}) -> A1 =< A2 end,
                                    maps:to_list(ByAuthor)
                                ),
                            Others =
                                [Id || {Author, Id} <- Ordered, Author =/= LocalAuthor],
                            {ok, [LocalPrev | Others]}
                    end
            end
    end.

%% @doc Insert a block into the DAG.
add_block(State, Block) ->
    Dag = maps:get(dag, State),
    Opts = maps:get(opts, State),
    BlockId = hb_maps:get(<<"id">>, Block, undefined, Opts),
    case maps:is_key(BlockId, Dag) of
        true -> {State, exists};
        false ->
            Round = hb_maps:get(<<"round">>, Block, undefined, Opts),
            Author = hb_maps:get(<<"author">>, Block, undefined, Opts),
            RoundIndex0 = maps:get(round_index, State, #{}),
            ByAuthor0 = maps:get(Round, RoundIndex0, #{}),
            case maps:get(Author, ByAuthor0, undefined) of
                undefined ->
                    RoundIndex1 =
                        RoundIndex0#{
                            Round =>
                                ByAuthor0#{
                                    Author => BlockId
                                }
                        },
                    AuthorRounds0 = maps:get(author_rounds, State, #{}),
                    AuthorRounds1 = AuthorRounds0#{ Author => Round },
                    MaxRound = max(maps:get(max_round, State), Round),
                    State1 = State#{
                        dag := Dag#{ BlockId => Block },
                        round_index := RoundIndex1,
                        author_rounds := AuthorRounds1,
                        max_round := MaxRound
                    },
                    {State1, added};
                BlockId ->
                    {State, exists};
                _Other ->
                    {State, duplicate_slot}
            end
    end.

%% @doc Ensure a block has a deterministic id (not part of the signed payload).
ensure_block_id(Block, Opts) ->
    BlockNoId = maps:remove(<<"id">>, Block),
    BlockId = hb_message:id(BlockNoId, all, Opts),
    case hb_maps:get(<<"id">>, Block, undefined, Opts) of
        undefined -> BlockNoId#{ <<"id">> => BlockId };
        BlockId -> BlockNoId#{ <<"id">> => BlockId };
        _ -> {error, id_mismatch}
    end.

%% @doc Store a block awaiting missing parents.
store_pending_block(State, Block) ->
    Opts = maps:get(opts, State),
    case ensure_block_id(Block, Opts) of
        {error, _} = Err -> {State, Err};
        Canon ->
            Pending0 = maps:get(pending_blocks, State, #{}),
            BlockId = hb_maps:get(<<"id">>, Canon, undefined, Opts),
            {State#{ pending_blocks := Pending0#{ BlockId => Canon } }, BlockId}
    end.

%% @doc Attempt to promote pending blocks whose parents are now available.
drain_pending_blocks(State) ->
    drain_pending_blocks(State, 0).

drain_pending_blocks(State, AddedTotal) ->
    Pending = maps:get(pending_blocks, State, #{}),
    {State1, PendingLeft, AddedNow} =
        promote_pending_blocks(State, maps:to_list(Pending), #{} , 0),
    State2 = State1#{ pending_blocks := PendingLeft },
    case AddedNow of
        0 -> {State2, AddedTotal};
        _ -> drain_pending_blocks(State2, AddedTotal + AddedNow)
    end.

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
    Pending0 = maps:get(pending_msgs, State, []),
    {State#{ pending_msgs := Pending0 ++ [Message] }, length(Pending0) + 1}.

%% @doc Drain pending messages, producing local blocks when possible.
drain_pending_messages(State) ->
    drain_pending_messages(State, []).

drain_pending_messages(State, Acc) ->
    Pending = maps:get(pending_msgs, State, []),
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
    MaxRound = maps:get(max_round, State),
    LastRound = maps:get(last_decided_round, State),
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
    WaveLength = maps:get(wave_length, State),
    NumProposers = maps:get(num_proposers, State),
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
    case maps:get(status, Status) of
        undecided -> [];
        _ -> [Status | decided_prefix(Rest)]
    end.

%% @doc Apply decisions in order, producing assignments for committed blocks.
apply_decisions(State, [], Acc) ->
    {State, lists:reverse(Acc)};
apply_decisions(State, [Status | Rest], Acc) ->
    Round = maps:get(round, Status),
    Slot = maps:get(slot, Status),
    Key = {Round, Slot},
    DecidedSlots0 = maps:get(decided_slots, State, #{}),
    case maps:get(Key, DecidedSlots0, undefined) of
        undefined ->
            case maps:get(status, Status) of
                commit ->
                    ProposerBlock = maps:get(block, Status),
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
    NumProposers = maps:get(num_proposers, State),
    DecidedSlots = maps:get(decided_slots, State, #{}),
    LastRound = maps:get(last_decided_round, State),
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
all_slots_decided(Round, NumProposers, DecidedSlots) ->
    lists:all(
        fun(Slot) -> maps:is_key({Round, Slot}, DecidedSlots) end,
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
    Quorum = maps:get(quorum, State),
    VotingBlocks = round_blocks(State, VotingRound),
    Skipped =
        length(
            lists:filter(
                fun(Block) ->
                    no_parent_from_author(State, Block, ProposerId)
                end,
                VotingBlocks
            )
        ),
    Skipped >= Quorum.

%% @doc Check whether a proposer has enough support to commit.
%% Mysticeti-C Algorithm 1: SupportedProposer (mysticeti-paper/algorithms/consensus_utils.tex).
supported_proposer(State, Decider, Wave) ->
    ProposerBlockId = get_proposer_block(State, Decider, Wave),
    case ProposerBlockId of
        undefined -> undefined;
        _ ->
            DecisionRound = decision_round(Wave, Decider),
            DecisionBlocks = round_blocks(State, DecisionRound),
            Quorum = maps:get(quorum, State),
            Certs =
                lists:filter(
                    fun(Block) ->
                        is_certificate(State, Block, ProposerBlockId)
                    end,
                    DecisionBlocks
                ),
            case length(Certs) >= Quorum of
                true -> ProposerBlockId;
                false -> undefined
            end
    end.

%% @doc Determine if a block is a certificate for the proposer.
%% Mysticeti-C Algorithm 1: IsCert (mysticeti-paper/algorithms/consensus_utils.tex).
is_certificate(State, Block, ProposerBlockId) ->
    Quorum = maps:get(quorum, State),
    Opts = maps:get(opts, State),
    Parents = hb_maps:get(<<"parents">>, Block, [], Opts),
    VoteAuthors =
        lists:foldl(
            fun(ParentId, Acc) ->
                case is_vote(State, ParentId, ProposerBlockId) of
                    true ->
                        Author = block_author(State, ParentId),
                        case Author of
                            undefined -> Acc;
                            _ -> Acc#{ Author => true }
                        end;
                    false -> Acc
                end
            end,
            #{},
            Parents
        ),
    maps:size(VoteAuthors) >= Quorum.

%% @doc A parent votes for a proposer if it supports that proposer block.
%% Mysticeti-C Algorithm 1: IsVote + SupportedBlock (mysticeti-paper/algorithms/consensus_utils.tex).
is_vote(State, VoteBlockId, ProposerBlockId) ->
    case maps:get(ProposerBlockId, maps:get(dag, State), undefined) of
        undefined -> false;
        ProposerBlock ->
            Opts = maps:get(opts, State),
            Author = hb_maps:get(<<"author">>, ProposerBlock, undefined, Opts),
            Round = hb_maps:get(<<"round">>, ProposerBlock, undefined, Opts),
            supported_block(State, VoteBlockId, Author, Round) =:= ProposerBlockId
    end.

%% @doc Compute the supported block for (author, round) in b's ancestry.
%% Mysticeti-C Algorithm 1: SupportedBlock (mysticeti-paper/algorithms/consensus_utils.tex).
supported_block(State, BlockId, Author, Round) ->
    case maps:get(BlockId, maps:get(dag, State), undefined) of
        undefined -> undefined;
        Block ->
            Opts = maps:get(opts, State),
            case Round >= hb_maps:get(<<"round">>, Block, -1, Opts) of
                true -> undefined;
                false ->
                    Parents = hb_maps:get(<<"parents">>, Block, [], Opts),
                    supported_block_parents(State, Parents, Author, Round)
            end
    end.

supported_block_parents(_State, [], _Author, _Round) -> undefined;
supported_block_parents(State, [ParentId | Rest], Author, Round) ->
    case maps:get(ParentId, maps:get(dag, State), undefined) of
        undefined ->
            supported_block_parents(State, Rest, Author, Round);
        Parent ->
            Opts = maps:get(opts, State),
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

has_ancestor(State, StartId, TargetId) ->
    case StartId == TargetId of
        true -> true;
        false ->
            Dag = maps:get(dag, State),
            Opts = maps:get(opts, State),
            has_ancestor(Dag, [StartId], TargetId, #{}, Opts)
    end.

has_ancestor(_Dag, [], _TargetId, _Visited, _Opts) -> false;
has_ancestor(Dag, [Id | Rest], TargetId, Visited, Opts) ->
    case maps:is_key(Id, Visited) of
        true -> has_ancestor(Dag, Rest, TargetId, Visited, Opts);
        false ->
            case maps:get(Id, Dag, undefined) of
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

try_indirect_anchors(_State, _Decider, _Wave, []) ->
    undecided;
try_indirect_anchors(State, Decider, Wave, [Anchor | Rest]) ->
    case maps:get(status, Anchor) of
        undecided -> undecided;
        skip -> try_indirect_anchors(State, Decider, Wave, Rest);
        commit ->
            ProposerBlockId = get_proposer_block(State, Decider, Wave),
            case ProposerBlockId of
                undefined -> undecided;
                _ ->
                    AnchorBlockId = maps:get(block, Anchor),
                    case certified_link(State, Decider, AnchorBlockId, ProposerBlockId) of
                        true -> {commit, ProposerBlockId};
                        false -> skip
                    end
            end
    end.

%% @doc Check if there is a certified link between the anchor and the proposer.
%% Mysticeti-C Algorithm 1: CertifiedLink (mysticeti-paper/algorithms/consensus_utils.tex).
certified_link(State, Decider, AnchorBlockId, ProposerBlockId) ->
    ProposerBlock = maps:get(ProposerBlockId, maps:get(dag, State)),
    Opts = maps:get(opts, State),
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
%% Uses the committed past of the proposer and then a deterministic total
%% order to issue assignments (required by process@1.0).
commit_proposer(State, ProposerBlockId) ->
    Past = collect_ancestors(State, ProposerBlockId),
    Ordered = maps:get(ordered, State),
    NewBlockIds = [Id || Id <- Past, not maps:is_key(Id, Ordered)],
    OrderedBlocks = order_blocks(State, NewBlockIds),
    {State1, Assignments} = assign_blocks(State, OrderedBlocks, []),
    Ordered1 =
        lists:foldl(
            fun(Id, Acc) -> Acc#{ Id => true } end,
            Ordered,
            NewBlockIds
        ),
    {State1#{ ordered := Ordered1 }, Assignments}.

%% @doc Collect all ancestors (including the block itself).
collect_ancestors(State, RootId) ->
    Dag = maps:get(dag, State),
    Opts = maps:get(opts, State),
    collect_ancestors(Dag, [RootId], #{}, [], Opts).

collect_ancestors(_Dag, [], _Visited, Acc, _Opts) -> Acc;
collect_ancestors(Dag, [Id | Rest], Visited, Acc, Opts) ->
    case maps:is_key(Id, Visited) of
        true -> collect_ancestors(Dag, Rest, Visited, Acc, Opts);
        false ->
            case maps:get(Id, Dag, undefined) of
                undefined ->
                    collect_ancestors(Dag, Rest, Visited#{ Id => true }, Acc, Opts);
                Block ->
                    Parents = hb_maps:get(<<"parents">>, Block, [], Opts),
                    collect_ancestors(
                        Dag,
                        Parents ++ Rest,
                        Visited#{ Id => true },
                        [Id | Acc],
                        Opts
                    )
            end
    end.

%% @doc Order blocks deterministically by (round, author, id).
%% This provides a stable total order for process@1.0.
order_blocks(State, BlockIds) ->
    Dag = maps:get(dag, State),
    Opts = maps:get(opts, State),
    lists:sort(
        fun(A, B) ->
            BA = maps:get(A, Dag),
            BB = maps:get(B, Dag),
            {hb_maps:get(<<"round">>, BA, undefined, Opts),
             hb_maps:get(<<"author">>, BA, undefined, Opts),
             A}
                < {hb_maps:get(<<"round">>, BB, undefined, Opts),
                   hb_maps:get(<<"author">>, BB, undefined, Opts),
                   B}
        end,
        BlockIds
    ).

%% @doc Create assignments for ordered blocks.
%% Only blocks with a payload (body) produce assignments.
assign_blocks(State, [], Acc) -> {State, lists:reverse(Acc)};
assign_blocks(State, [Id | Rest], Acc) ->
    Dag = maps:get(dag, State),
    Block = maps:get(Id, Dag),
    Opts = maps:get(opts, State),
    Payload = hb_maps:get(<<"body">>, Block, undefined, Opts),
    case Payload of
        undefined ->
            assign_blocks(State, Rest, Acc);
        _ ->
            MsgId = hb_message:id(Payload, all, Opts),
            case maps:is_key(MsgId, maps:get(assigned, State)) of
                true ->
                    assign_blocks(State, Rest, Acc);
                false ->
                    {Assignment, State1} = make_assignment(State, Payload),
                    assign_blocks(
                        State1#{ assigned := (maps:get(assigned, State1))#{ MsgId => true }},
                        Rest,
                        [{MsgId, Assignment} | Acc]
                    )
            end
    end.

%% @doc Generate an assignment for a payload message.
make_assignment(State, Message) ->
    Opts = maps:get(opts, State),
    BaseStateHashpath = base_state(State),
    NextSlot = maps:get(current_slot, State) + 1,
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
            <<"process">> => hb_util:id(maps:get(id, State)),
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
    Wallets = maps:get(wallets, State),
    Opts = maps:get(opts, State),
    CommitmentSpec = maps:get(commitment_spec, State),
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
    hb_util:id(maps:get(id, S));
base_state(#{ base_state_hashpath := BaseStateHashpath }) ->
    BaseStateHashpath.

%% @doc Generate the next hashpath for a new assignment.
next_hashpath(BaseStateHashpath, NewAssignment, State) ->
    Opts = maps:get(opts, State),
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
    Validators = maps:get(validators, State),
    BaseOffset = maps:get(proposer_offset, State),
    SlotIndex = (Index + BaseOffset) rem length(Validators),
    lists:nth(SlotIndex + 1, Validators).

%% @doc Get the predefined proposer for a slot (Algorithm 2).
get_predefined_proposer(State, Decider, Wave) ->
    ProposerRound = proposer_round(Wave, Decider),
    SlotOffset = maps:get(slot_offset, Decider),
    predefined_proposer(State, ProposerRound + SlotOffset).

%% @doc Lookup the proposer block for a slot (Algorithm 1).
get_proposer_block(State, Decider, Wave) ->
    ProposerRound = proposer_round(Wave, Decider),
    ProposerId = get_predefined_proposer(State, Decider, Wave),
    block_by_author_round(State, ProposerId, ProposerRound).

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

%% @doc Lookup a block by author and round.
block_by_author_round(State, Author, Round) ->
    RoundIndex = maps:get(round_index, State, #{}),
    case maps:get(Round, RoundIndex, undefined) of
        undefined -> undefined;
        ByAuthor -> maps:get(Author, ByAuthor, undefined)
    end.

%% @doc Get all blocks for a round.
round_blocks(State, Round) ->
    RoundIndex = maps:get(round_index, State, #{}),
    Dag = maps:get(dag, State),
    case maps:get(Round, RoundIndex, undefined) of
        undefined -> [];
        ByAuthor ->
            lists:filtermap(
                fun(_Key) ->
                    Id = maps:get(_Key, ByAuthor),
                    case maps:get(Id, Dag, undefined) of
                        undefined -> false;
                        Block -> {true, Block}
                    end
                end,
                maps:keys(ByAuthor)
            )
    end.

%% @doc Get the author for a block id.
block_author(State, BlockId) ->
    case maps:get(BlockId, maps:get(dag, State), undefined) of
        undefined -> undefined;
        Block ->
            Opts = maps:get(opts, State),
            hb_maps:get(<<"author">>, Block, undefined, Opts)
    end.

%% @doc True if no parent is authored by the given id.
no_parent_from_author(State, Block, AuthorId) ->
    Opts = maps:get(opts, State),
    Parents = hb_maps:get(<<"parents">>, Block, [], Opts),
    Dag = maps:get(dag, State),
    lists:all(
        fun(ParentId) ->
            case maps:get(ParentId, Dag, undefined) of
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
    Opts = maps:get(opts, State),
    case ensure_block_id(Block0, Opts) of
        {error, _} = Err -> Err;
        Block ->
            BlockNoId = maps:remove(<<"id">>, Block),
            Validators = maps:get(validators, State),
            AuthorField = hb_maps:get(<<"author">>, Block, undefined, Opts),
            Round = hb_maps:get(<<"round">>, Block, undefined, Opts),
            Parents = hb_maps:get(<<"parents">>, Block, undefined, Opts),
            case verify_block_signature(BlockNoId, Opts) of
                {ok, Signer} ->
                    case AuthorField of
                        Signer ->
                            Author = Signer,
                            case {hb_maps:get(<<"process">>, Block, undefined, Opts) =:= maps:get(id, State),
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
    Opts = maps:get(opts, State),
    Round = hb_maps:get(<<"round">>, Block, undefined, Opts),
    Parents = hb_maps:get(<<"parents">>, Block, [], Opts),
    Dag = maps:get(dag, State),
    case length(Parents) =:= length(lists:usort(Parents)) of
        false -> {error, duplicate_parents};
        true ->
            ParentBlocks =
                lists:map(
                    fun(ParentId) -> {ParentId, maps:get(ParentId, Dag, undefined)} end,
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
    Opts = maps:get(opts, State),
    Round = hb_maps:get(<<"round">>, Block, undefined, Opts),
    case Round of
        0 ->
            case hb_maps:get(<<"parents">>, Block, [], Opts) of
                [] -> {ok, Block};
                _ -> {error, genesis_parents_not_empty}
            end;
        _ ->
            Author = hb_maps:get(<<"author">>, Block, undefined, Opts),
            Quorum = maps:get(quorum, State),
            [FirstParentId | _] = hb_maps:get(<<"parents">>, Block, [], Opts),
            PrevRound = Round - 1,
            PrevId = block_by_author_round(State, Author, PrevRound),
            case PrevId =:= FirstParentId of
                false -> {error, invalid_prev_parent};
                true ->
                    case lists:keyfind(FirstParentId, 1, ParentBlocks) of
                        {_, FirstParent} ->
                            case {hb_maps:get(<<"author">>, FirstParent, undefined, Opts),
                                  hb_maps:get(<<"round">>, FirstParent, undefined, Opts)} of
                                {Author, PrevRound} ->
                                    PrevRoundParents =
                                        [Id || {Id, PB} <- ParentBlocks,
                                            hb_maps:get(<<"round">>, PB, -1, Opts) =:= Round - 1],
                                    case length(PrevRoundParents) >= Quorum of
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
    end.

%% @doc Summarize the server state for info.
summarize(State) ->
    #{
        current => maps:get(current_slot, State),
        max_round => maps:get(max_round, State),
        stakers => maps:get(stakers, State, []),
        validators => maps:get(validators, State),
        quorum => maps:get(quorum, State),
        wave_length => maps:get(wave_length, State),
        num_proposers => maps:get(num_proposers, State),
        last_decided_round => maps:get(last_decided_round, State)
    }.

%% @doc Extract wave length (Algorithm 2) from the process config.
%% Mysticeti-C Algorithm 2: waveLength (mysticeti-paper/algorithms/baseline_committer.tex).
wave_length(Proc, Opts) ->
    Raw =
        hb_ao:get(
            <<"mysticeti/wave-length">>,
            Proc,
            hb_opts:get(mysticeti_wave_length, 3, Opts),
            Opts#{ hashpath => ignore }
        ),
    WaveLength = hb_util:int(Raw),
    case WaveLength >= 1 of
        true -> WaveLength;
        false -> throw({invalid_wave_length, WaveLength})
    end.

%% @doc Extract proposer offset (Algorithm 2) from the process config.
%% Mysticeti-C Algorithm 2: PredefinedProposer offset (mysticeti-paper/algorithms/baseline_committer.tex).
proposer_offset(Proc, Opts) ->
    Raw =
        hb_ao:get(
            <<"mysticeti/proposer-offset">>,
            Proc,
            hb_opts:get(mysticeti_proposer_offset, 0, Opts),
            Opts#{ hashpath => ignore }
        ),
    Offset = hb_util:int(Raw),
    case Offset >= 0 of
        true -> Offset;
        false -> throw({invalid_proposer_offset, Offset})
    end.

%% @doc Extract stakers (validator + stake) from process config or opts.
%% Preferred config: mysticeti/stakers or stakers. Fallback: mysticeti/validators.
stakers(Proc, Opts) ->
    Raw =
        hb_ao:get(
            <<"mysticeti/stakers">>,
            Proc,
            hb_ao:get(
                <<"stakers">>,
                Proc,
                hb_ao:get(
                    <<"mysticeti/validators">>,
                    Proc,
                    hb_ao:get(<<"validators">>, Proc, not_found, Opts#{ hashpath => ignore }),
                    Opts#{ hashpath => ignore }
                ),
                Opts#{ hashpath => ignore }
            ),
            Opts#{ hashpath => ignore }
        ),
    case Raw of
        not_found ->
            normalize_stakers(
                hb_opts:get(mysticeti_validators, [hb:address()], Opts),
                Opts
            );
        _ ->
            normalize_stakers(hb_cache:ensure_all_loaded(Raw, Opts), Opts)
    end.

validators_from_stakers(Stakers) ->
    [Id || #{ id := Id } <- Stakers].

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
                        case maps:is_key(Id, Seen) of
                            true -> {Seen, Acc};
                            false -> {Seen#{ Id => true }, Acc ++ [Staker]}
                        end;
                    error ->
                        {Seen, Acc}
                end
            end,
            {#{}, []},
            Loaded
        ),
    Normalized;
normalize_stakers(Stakers, Opts) when is_binary(Stakers) ->
    normalize_stakers(
        binary:split(
            binary:replace(Stakers, <<"\"">>, <<"">>, [global]),
            <<",">>,
            [global, trim_all]
        ),
        Opts
    );
normalize_stakers(Stakers, Opts) ->
    normalize_stakers([Stakers], Opts).

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

local_author(_Validators, Opts) ->
    hb_opts:get(mysticeti_author, hb:address(), Opts).

ensure_local_author(Validators, LocalAuthor) ->
    case lists:member(LocalAuthor, Validators) of
        true -> ok;
        false -> throw({local_author_not_in_staker_set, LocalAuthor})
    end.

num_proposers(Proc, Validators, Opts) ->
    Raw =
        hb_ao:get(
            <<"mysticeti/num-proposers">>,
            Proc,
            hb_opts:get(mysticeti_num_proposers, not_found, Opts),
            Opts#{ hashpath => ignore }
        ),
    Num =
        case Raw of
            not_found -> length(Validators);
            _ -> hb_util:int(Raw)
        end,
    case Num >= 1 andalso Num =< length(Validators) of
        true -> Num;
        false -> throw({invalid_num_proposers, Num})
    end.

%% @doc Extract peer nodes for gossip.
peers(Proc, Opts) ->
    Raw =
        hb_ao:get(
            <<"mysticeti/peers">>,
            Proc,
            hb_opts:get(mysticeti_peers, [], Opts),
            Opts#{ hashpath => ignore }
        ),
    normalize_peers(Raw).

normalize_peers(Peers) when is_list(Peers) ->
    lists:map(fun hb_util:bin/1, Peers);
normalize_peers(Peers) when is_binary(Peers) ->
    binary:split(
        binary:replace(Peers, <<"\"">>, <<"">>, [global]),
        <<",">>,
        [global, trim_all]
    );
normalize_peers(_) -> [].

%% @doc Broadcast a block to configured peers.
broadcast_block(State, Block) ->
    Peers = maps:get(peers, State, []),
    Opts = maps:get(opts, State),
    lists:foreach(
        fun(Node) ->
            spawn(fun() ->
                hb_http:post(Node, <<"/~mysticeti@1.0/block">>, Block, Opts)
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

%%% Tests

test_node_opts(Wallet, Author, Store) ->
    #{
        store => [Store],
        priv_wallet => Wallet,
        mysticeti_author => Author
    }.

test_block(ProcID, Author, Round, Parents, Payload, Opts) ->
    Block0 =
        #{
            <<"type">> => <<"MysticetiBlock">>,
            <<"process">> => ProcID,
            <<"author">> => Author,
            <<"round">> => Round,
            <<"parents">> => Parents,
            <<"timestamp">> => scheduler_time(),
            <<"body">> => Payload
        },
    Signed = hb_message:commit(Block0, Opts),
    ensure_block_id(Signed, Opts).

http_post_mysticeti_schedule(Node, ProcMsg, Msg, Opts) ->
    Base = hb_message:commit(#{
        <<"path">> => <<"/~mysticeti@1.0/schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> => Msg,
        <<"process">> => ProcMsg
    }, Opts),
    hb_http:post(Node, Base, Opts).

http_post_mysticeti_block(Node, Block, Opts) ->
    Base = hb_message:commit(#{
        <<"path">> => <<"/~mysticeti@1.0/block">>,
        <<"method">> => <<"POST">>,
        <<"body">> => Block
    }, Opts),
    hb_http:post(Node, Base, Opts).

http_get_mysticeti_schedule(Node, ProcID, From, To, Opts) ->
    hb_http:get(
        Node,
        <<"/~mysticeti@1.0/schedule&target=", ProcID/binary,
          "&from=", (integer_to_binary(From))/binary,
          "&to=", (integer_to_binary(To))/binary>>,
        Opts
    ).

mysticeti_test_process(Validators, Opts) ->
    mysticeti_test_process(Validators, #{}, Opts).

mysticeti_test_process(Validators, Overrides, Opts) ->
    Stakers = [#{ <<"id">> => V, <<"stake">> => 1 } || V <- Validators],
    Mysticeti0 = #{
        <<"validators">> => Validators,
        <<"stakers">> => Stakers,
        <<"wave-length">> => 3,
        <<"proposer-offset">> => 0,
        <<"num-proposers">> => length(Validators)
    },
    Mysticeti = hb_maps:merge(Mysticeti0, Overrides, Opts),
    hb_message:commit(
        #{
            <<"device">> => <<"process@1.0">>,
            <<"scheduler-device">> => <<"mysticeti@1.0">>,
            <<"scheduler-location">> => Validators,
            <<"mysticeti">> => Mysticeti,
            <<"type">> => <<"Process">>,
            <<"test-random-seed">> => rand:uniform(1000000)
        },
        Opts
    ).

test_message(ProcID, Body, Opts) ->
    hb_message:commit(
        #{
            <<"target">> => ProcID,
            <<"body">> => Body,
            <<"type">> => <<"Message">>
        },
        Opts
    ).

post_blocks(Node, Blocks, Opts) ->
    lists:foreach(
        fun(Block) ->
            {ok, _} = http_post_mysticeti_block(Node, Block, Opts)
        end,
        Blocks
    ).

fetch_assignments(Node, ProcID, From, To, Opts) ->
    {ok, Schedule} = http_get_mysticeti_schedule(Node, ProcID, From, To, Opts),
    hb_private:reset(
        hb_ao:get(<<"assignments">>, Schedule, Opts)
    ).

wait_for_assignments(Node, ProcID, From, To, Expected, Opts) ->
    _ = hb_util:wait_until(
        fun() ->
            case http_get_mysticeti_schedule(Node, ProcID, From, To, Opts) of
                {ok, Schedule} ->
                    Assignments =
                        hb_private:reset(
                            hb_ao:get(<<"assignments">>, Schedule, Opts)
                        ),
                    hb_maps:size(Assignments, Opts) >= Expected;
                _ -> false
            end
        end,
        5000
    ),
    fetch_assignments(Node, ProcID, From, To, Opts).

%% @doc Compute proposer/voting/decision rounds for a wave.
%% Mysticeti-C Algorithm 2: ProposerRound and DecisionRound.
wave_rounds(Wave, Proc, Opts) ->
    WaveLength = wave_length(Proc, Opts),
    Decider = decider(WaveLength, 0, 0),
    ProposerRound = proposer_round(Wave, Decider),
    VotingRound = ProposerRound + 1,
    DecisionRound = decision_round(Wave, Decider),
    {ProposerRound, VotingRound, DecisionRound}.

%% @doc Deterministic leader selection (round-robin).
%% Mysticeti-C Algorithm 2: PredefinedProposer.
proposer_for_round(Validators, Round, Proc, Opts) ->
    Offset = proposer_offset(Proc, Opts),
    Index = (Round + Offset) rem length(Validators),
    lists:nth(Index + 1, Validators).

%% @doc Build an author->block_id map from a list of blocks.
blocks_by_author(Blocks, Opts) ->
    maps:from_list(
        lists:map(
            fun(Block) ->
                {hb_maps:get(<<"author">>, Block, undefined, Opts),
                 hb_maps:get(<<"id">>, Block, undefined, Opts)}
            end,
            Blocks
        )
    ).

%% @doc Build parent list with the author's previous block first.
parents_for_author(PrevRoundByAuthor, Author) ->
    Ordered = lists:sort(maps:to_list(PrevRoundByAuthor)),
    OwnPrev = maps:get(Author, PrevRoundByAuthor),
    Others = [Id || {A, Id} <- Ordered, A =/= Author],
    [OwnPrev | Others].

%% @doc Generate a full round of valid blocks.
make_round_blocks(ProcID, Round, Authors, PrevRoundByAuthor, Payloads, OptsByAuthor) ->
    lists:map(
        fun(Author) ->
            Parents =
                case Round of
                    0 -> [];
                    _ -> parents_for_author(PrevRoundByAuthor, Author)
                end,
            Payload = maps:get(Author, Payloads, undefined),
            Opts = maps:get(Author, OptsByAuthor),
            test_block(ProcID, Author, Round, Parents, Payload, Opts)
        end,
        Authors
    ).

mysticeti_quorum_commit_test_() ->
    {timeout, 60, fun mysticeti_quorum_commit/0}.

%% @doc Validate direct commit (Algorithm 2: SupportedProposer + IsCert).
mysticeti_quorum_commit() ->
    W1 = ar_wallet:new(),
    W2 = ar_wallet:new(),
    W3 = ar_wallet:new(),
    W4 = ar_wallet:new(),
    A1 = hb_util:human_id(ar_wallet:to_address(W1)),
    A2 = hb_util:human_id(ar_wallet:to_address(W2)),
    A3 = hb_util:human_id(ar_wallet:to_address(W3)),
    A4 = hb_util:human_id(ar_wallet:to_address(W4)),
    Validators = [A1, A2, A3, A4],
    Store = hb_test_utils:test_store(),
    N1Opts = test_node_opts(W1, A1, Store),
    N2Opts = test_node_opts(W2, A2, Store),
    N3Opts = test_node_opts(W3, A3, Store),
    N4Opts = test_node_opts(W4, A4, Store),
    OptsByAuthor = #{
        A1 => N1Opts,
        A2 => N2Opts,
        A3 => N3Opts,
        A4 => N4Opts
    },
    N1 = hb_http_server:start_node(N1Opts),
    _N2 = hb_http_server:start_node(N2Opts),
    Proc = mysticeti_test_process(Validators, N1Opts),
    ProcID = hb_message:id(Proc, all, N1Opts),
    {ok, _} = hb_cache:write(Proc, N1Opts),
    {ok, _} = hb_cache:write(Proc, N2Opts),
    Pid = dev_mysticeti_registry:find(ProcID, Proc, N1Opts),
    #{ current := InitialSlot } = dev_mysticeti_server:info(Pid),
    Msg0 =
        hb_message:commit(
            #{
                <<"target">> => ProcID,
                <<"body">> => <<"m0">>,
                <<"type">> => <<"Message">>
            },
            N1Opts
        ),
    {ok, _} = hb_cache:write(Msg0, N1Opts),
    {ok, Res0} = http_post_mysticeti_schedule(N1, Proc, Msg0, N1Opts),
    ProposerBlockId =
        hb_ao:get(<<"pending/block">>, Res0, undefined, N1Opts),
    ?assert(is_binary(ProposerBlockId)),
    #{ current := AfterScheduleSlot } = dev_mysticeti_server:info(Pid),
    ?assertEqual(InitialSlot, AfterScheduleSlot),
    NextSlot = InitialSlot + 1,
    {ok, EmptySchedule} =
        http_get_mysticeti_schedule(N1, ProcID, NextSlot, NextSlot, N1Opts),
    Assignments0 =
        hb_private:reset(
            hb_ao:get(<<"assignments">>, EmptySchedule, N1Opts)
        ),
    ?assertEqual(0, hb_maps:size(Assignments0, N1Opts)),
    R0Others = make_round_blocks(
        ProcID,
        0,
        [A2, A3, A4],
        #{},
        #{},
        OptsByAuthor
    ),
    post_blocks(N1, R0Others, N2Opts),
    R0ByAuthor = (blocks_by_author(R0Others, N1Opts))#{ A1 => ProposerBlockId },
    VoteBlocks = make_round_blocks(
        ProcID,
        1,
        [A2, A3, A4],
        R0ByAuthor,
        #{},
        OptsByAuthor
    ),
    post_blocks(N1, VoteBlocks, N2Opts),
    VoteByAuthor = blocks_by_author(VoteBlocks, N1Opts),
    DecisionBlocks = make_round_blocks(
        ProcID,
        2,
        [A2, A3, A4],
        VoteByAuthor,
        #{},
        OptsByAuthor
    ),
    post_blocks(N1, DecisionBlocks, N2Opts),
    ?debug_wait(1000),
    {ok, Schedule} = http_get_mysticeti_schedule(N1, ProcID, 0, 1, N1Opts),
    Assignments = hb_ao:get(<<"assignments">>, Schedule, N1Opts),
    ?assert(hb_maps:size(Assignments, N1Opts) >= 1),
    [{Slot, Assignment} | _] = hb_util:to_sorted_list(Assignments, N1Opts),
    ?assertEqual(0, hb_util:int(Slot)),
    ?assertEqual(<<"m0">>, hb_ao:get(<<"body/body">>, Assignment, N1Opts)).

mysticeti_no_quorum_commit_test_() ->
    {timeout, 60, fun mysticeti_no_quorum_commit/0}.

%% @doc Ensure insufficient certificates do not commit (Algorithm 1: IsCert).
mysticeti_no_quorum_commit() ->
    W1 = ar_wallet:new(),
    W2 = ar_wallet:new(),
    W3 = ar_wallet:new(),
    W4 = ar_wallet:new(),
    A1 = hb_util:human_id(ar_wallet:to_address(W1)),
    A2 = hb_util:human_id(ar_wallet:to_address(W2)),
    A3 = hb_util:human_id(ar_wallet:to_address(W3)),
    A4 = hb_util:human_id(ar_wallet:to_address(W4)),
    Validators = [A1, A2, A3, A4],
    Store = hb_test_utils:test_store(),
    N1Opts = test_node_opts(W1, A1, Store),
    N2Opts = test_node_opts(W2, A2, Store),
    N3Opts = test_node_opts(W3, A3, Store),
    N4Opts = test_node_opts(W4, A4, Store),
    OptsByAuthor = #{
        A1 => N1Opts,
        A2 => N2Opts,
        A3 => N3Opts,
        A4 => N4Opts
    },
    Node = hb_http_server:start_node(N1Opts),
    Proc = mysticeti_test_process(Validators, N1Opts),
    ProcID = hb_message:id(Proc, all, N1Opts),
    {ok, _} = hb_cache:write(Proc, N1Opts),
    _ = dev_mysticeti_registry:find(ProcID, Proc, N1Opts),
    Msg = test_message(ProcID, <<"m0">>, N1Opts),
    {ok, _} = hb_cache:write(Msg, N1Opts),
    {ok, Res0} = http_post_mysticeti_schedule(Node, Proc, Msg, N1Opts),
    ProposerBlockId = hb_ao:get(<<"pending/block">>, Res0, undefined, N1Opts),
    R0Others = make_round_blocks(
        ProcID,
        0,
        [A2, A3, A4],
        #{},
        #{},
        OptsByAuthor
    ),
    post_blocks(Node, R0Others, N2Opts),
    R0ByAuthor = (blocks_by_author(R0Others, N1Opts))#{ A1 => ProposerBlockId },
    VoteBlocks = make_round_blocks(
        ProcID,
        1,
        [A2, A3, A4],
        R0ByAuthor,
        #{},
        OptsByAuthor
    ),
    post_blocks(Node, VoteBlocks, N2Opts),
    VoteByAuthor = blocks_by_author(VoteBlocks, N1Opts),
    DecisionBlocks = make_round_blocks(
        ProcID,
        2,
        [A2, A3],
        VoteByAuthor,
        #{},
        OptsByAuthor
    ),
    post_blocks(Node, DecisionBlocks, N2Opts),
    ?debug_wait(500),
    Assignments = fetch_assignments(Node, ProcID, 0, 0, N1Opts),
    ?assertEqual(0, hb_maps:size(Assignments, N1Opts)).

mysticeti_skip_wave_test_() ->
    {timeout, 60, fun mysticeti_skip_wave/0}.

%% @doc Validate SkippedProposer behavior (Algorithm 2: SkippedProposer).
mysticeti_skip_wave() ->
    W1 = ar_wallet:new(),
    W2 = ar_wallet:new(),
    W3 = ar_wallet:new(),
    W4 = ar_wallet:new(),
    A1 = hb_util:human_id(ar_wallet:to_address(W1)),
    A2 = hb_util:human_id(ar_wallet:to_address(W2)),
    A3 = hb_util:human_id(ar_wallet:to_address(W3)),
    A4 = hb_util:human_id(ar_wallet:to_address(W4)),
    Validators = [A1, A2, A3, A4],
    Store = hb_test_utils:test_store(),
    N1Opts = test_node_opts(W1, A1, Store),
    N2Opts = test_node_opts(W2, A2, Store),
    N3Opts = test_node_opts(W3, A3, Store),
    N4Opts = test_node_opts(W4, A4, Store),
    OptsByAuthor = #{
        A1 => N1Opts,
        A2 => N2Opts,
        A3 => N3Opts,
        A4 => N4Opts
    },
    Node = hb_http_server:start_node(N1Opts),
    Proc = mysticeti_test_process(Validators, N1Opts),
    ProcID = hb_message:id(Proc, all, N1Opts),
    {ok, _} = hb_cache:write(Proc, N1Opts),
    _ = dev_mysticeti_registry:find(ProcID, Proc, N1Opts),
    Msg = test_message(ProcID, <<"m-skip">>, N1Opts),
    {ok, _} = hb_cache:write(Msg, N1Opts),
    {ok, Res0} = http_post_mysticeti_schedule(Node, Proc, Msg, N1Opts),
    _ProposerBlockId = hb_ao:get(<<"pending/block">>, Res0, undefined, N1Opts),
    R0Others = make_round_blocks(
        ProcID,
        0,
        [A2, A3, A4],
        #{},
        #{},
        OptsByAuthor
    ),
    post_blocks(Node, R0Others, N2Opts),
    R0ByAuthorNoA1 = blocks_by_author(R0Others, N1Opts),
    VoteBlocks = make_round_blocks(
        ProcID,
        1,
        [A2, A3, A4],
        R0ByAuthorNoA1,
        #{},
        OptsByAuthor
    ),
    post_blocks(Node, VoteBlocks, N2Opts),
    ?debug_wait(500),
    Assignments = fetch_assignments(Node, ProcID, 0, 0, N1Opts),
    ?assertEqual(0, hb_maps:size(Assignments, N1Opts)).

mysticeti_two_wave_order_test_() ->
    {timeout, 60, fun mysticeti_two_wave_order/0}.

%% @doc Validate two-wave ordering and total order stability.
%% Mysticeti-C Algorithm 3: TryDecide ordering across waves
%% (mysticeti-paper/algorithms/universal_committer.tex).
mysticeti_two_wave_order() ->
    W1 = ar_wallet:new(),
    W2 = ar_wallet:new(),
    W3 = ar_wallet:new(),
    W4 = ar_wallet:new(),
    A1 = hb_util:human_id(ar_wallet:to_address(W1)),
    A2 = hb_util:human_id(ar_wallet:to_address(W2)),
    A3 = hb_util:human_id(ar_wallet:to_address(W3)),
    A4 = hb_util:human_id(ar_wallet:to_address(W4)),
    Validators = [A1, A2, A3, A4],
    Store = hb_test_utils:test_store(),
    N1Opts = test_node_opts(W1, A1, Store),
    N2Opts = test_node_opts(W2, A2, Store),
    N3Opts = test_node_opts(W3, A3, Store),
    N4Opts = test_node_opts(W4, A4, Store),
    OptsByAuthor = #{
        A1 => N1Opts,
        A2 => N2Opts,
        A3 => N3Opts,
        A4 => N4Opts
    },
    Node = hb_http_server:start_node(N1Opts),
    Proc = mysticeti_test_process(Validators, N1Opts),
    ProcID = hb_message:id(Proc, all, N1Opts),
    {ok, _} = hb_cache:write(Proc, N1Opts),
    _ = dev_mysticeti_registry:find(ProcID, Proc, N1Opts),
    Msg0 = test_message(ProcID, <<"m0">>, N1Opts),
    {ok, _} = hb_cache:write(Msg0, N1Opts),
    {ok, Res0} = http_post_mysticeti_schedule(Node, Proc, Msg0, N1Opts),
    ProposerBlock0 = hb_ao:get(<<"pending/block">>, Res0, undefined, N1Opts),
    R0Others = make_round_blocks(
        ProcID,
        0,
        [A2, A3, A4],
        #{},
        #{},
        OptsByAuthor
    ),
    post_blocks(Node, R0Others, N2Opts),
    R0ByAuthor = (blocks_by_author(R0Others, N1Opts))#{ A1 => ProposerBlock0 },
    VoteBlocks0 = make_round_blocks(
        ProcID,
        1,
        [A2, A3, A4],
        R0ByAuthor,
        #{},
        OptsByAuthor
    ),
    post_blocks(Node, VoteBlocks0, N2Opts),
    VoteByAuthor0 = blocks_by_author(VoteBlocks0, N1Opts),
    DecisionBlocks0 = make_round_blocks(
        ProcID,
        2,
        [A2, A3, A4],
        VoteByAuthor0,
        #{},
        OptsByAuthor
    ),
    post_blocks(Node, DecisionBlocks0, N2Opts),
    _ = wait_for_assignments(Node, ProcID, 0, 0, 1, N1Opts),
    DecisionByAuthor0 = blocks_by_author(DecisionBlocks0, N1Opts),
    {ProposerRound1, VotingRound1, DecisionRound1} = wave_rounds(1, Proc, N1Opts),
    Proposer1 = proposer_for_round(Validators, ProposerRound1, Proc, N1Opts),
    Msg1 = test_message(ProcID, <<"m1">>, maps:get(Proposer1, OptsByAuthor)),
    {ok, _} = hb_cache:write(Msg1, maps:get(Proposer1, OptsByAuthor)),
    Payloads1 = #{ Proposer1 => Msg1 },
    Round3Blocks = make_round_blocks(
        ProcID,
        ProposerRound1,
        [A2, A3, A4],
        DecisionByAuthor0,
        Payloads1,
        OptsByAuthor
    ),
    post_blocks(Node, Round3Blocks, N2Opts),
    Round3ByAuthor = blocks_by_author(Round3Blocks, N1Opts),
    VoteBlocks1 = make_round_blocks(
        ProcID,
        VotingRound1,
        [A2, A3, A4],
        Round3ByAuthor,
        #{},
        OptsByAuthor
    ),
    post_blocks(Node, VoteBlocks1, N2Opts),
    VoteByAuthor1 = blocks_by_author(VoteBlocks1, N1Opts),
    DecisionBlocks1 = make_round_blocks(
        ProcID,
        DecisionRound1,
        [A2, A3, A4],
        VoteByAuthor1,
        #{},
        OptsByAuthor
    ),
    post_blocks(Node, DecisionBlocks1, N2Opts),
    Assignments = wait_for_assignments(Node, ProcID, 0, 1, 2, N1Opts),
    Sorted = hb_util:to_sorted_list(Assignments, N1Opts),
    [{Slot0, A0}, {Slot1, A1Assignment} | _] = Sorted,
    ?assertEqual(0, hb_util:int(Slot0)),
    ?assertEqual(1, hb_util:int(Slot1)),
    ?assertEqual(<<"m0">>, hb_ao:get(<<"body/body">>, A0, N1Opts)),
    ?assertEqual(<<"m1">>, hb_ao:get(<<"body/body">>, A1Assignment, N1Opts)).

mysticeti_indirect_commit_test_() ->
    {timeout, 60, fun mysticeti_indirect_commit/0}.

%% @doc Validate TryIndirectDecide (Algorithm 3) via a later committed anchor.
%% Mysticeti-C Algorithm 3: TryIndirectDecide + CertifiedLink behavior
%% (mysticeti-paper/algorithms/universal_committer.tex).
mysticeti_indirect_commit() ->
    W1 = ar_wallet:new(),
    W2 = ar_wallet:new(),
    W3 = ar_wallet:new(),
    W4 = ar_wallet:new(),
    A1 = hb_util:human_id(ar_wallet:to_address(W1)),
    A2 = hb_util:human_id(ar_wallet:to_address(W2)),
    A3 = hb_util:human_id(ar_wallet:to_address(W3)),
    A4 = hb_util:human_id(ar_wallet:to_address(W4)),
    Validators = [A1, A2, A3, A4],
    Store = hb_test_utils:test_store(),
    N1Opts = test_node_opts(W1, A1, Store),
    N2Opts = test_node_opts(W2, A2, Store),
    N3Opts = test_node_opts(W3, A3, Store),
    N4Opts = test_node_opts(W4, A4, Store),
    OptsByAuthor = #{
        A1 => N1Opts,
        A2 => N2Opts,
        A3 => N3Opts,
        A4 => N4Opts
    },
    Node = hb_http_server:start_node(N1Opts),
    Proc = mysticeti_test_process(Validators, N1Opts),
    ProcID = hb_message:id(Proc, all, N1Opts),
    {ok, _} = hb_cache:write(Proc, N1Opts),
    _ = dev_mysticeti_registry:find(ProcID, Proc, N1Opts),
    Msg0 = test_message(ProcID, <<"m0">>, N1Opts),
    {ok, _} = hb_cache:write(Msg0, N1Opts),
    {ok, Res0} = http_post_mysticeti_schedule(Node, Proc, Msg0, N1Opts),
    ProposerBlock0 = hb_ao:get(<<"pending/block">>, Res0, undefined, N1Opts),
    R0Others = make_round_blocks(
        ProcID,
        0,
        [A2, A3, A4],
        #{},
        #{},
        OptsByAuthor
    ),
    post_blocks(Node, R0Others, N2Opts),
    R0ByAuthor = (blocks_by_author(R0Others, N1Opts))#{ A1 => ProposerBlock0 },
    VoteBlocksA2A3 = make_round_blocks(
        ProcID,
        1,
        [A2, A3],
        R0ByAuthor,
        #{},
        OptsByAuthor
    ),
    R0ByAuthorNoA1 = maps:remove(A1, R0ByAuthor),
    VoteBlockA4 =
        test_block(
            ProcID,
            A4,
            1,
            parents_for_author(R0ByAuthorNoA1, A4),
            undefined,
            maps:get(A4, OptsByAuthor)
        ),
    VoteBlocks0 = VoteBlocksA2A3 ++ [VoteBlockA4],
    post_blocks(Node, VoteBlocks0, N2Opts),
    VoteByAuthor0 = blocks_by_author(VoteBlocks0, N1Opts),
    DecisionBlocks0 = make_round_blocks(
        ProcID,
        2,
        [A2, A3, A4],
        VoteByAuthor0,
        #{},
        OptsByAuthor
    ),
    post_blocks(Node, DecisionBlocks0, N2Opts),
    ?debug_wait(500),
    Assignments0 = fetch_assignments(Node, ProcID, 0, 0, N1Opts),
    ?assertEqual(0, hb_maps:size(Assignments0, N1Opts)),
    DecisionByAuthor0 = blocks_by_author(DecisionBlocks0, N1Opts),
    {ProposerRound1, VotingRound1, DecisionRound1} = wave_rounds(1, Proc, N1Opts),
    Proposer1 = proposer_for_round(Validators, ProposerRound1, Proc, N1Opts),
    Msg1 = test_message(ProcID, <<"m1">>, maps:get(Proposer1, OptsByAuthor)),
    {ok, _} = hb_cache:write(Msg1, maps:get(Proposer1, OptsByAuthor)),
    Payloads1 = #{ Proposer1 => Msg1 },
    Round3Blocks = make_round_blocks(
        ProcID,
        ProposerRound1,
        [A2, A3, A4],
        DecisionByAuthor0,
        Payloads1,
        OptsByAuthor
    ),
    post_blocks(Node, Round3Blocks, N2Opts),
    Round3ByAuthor = blocks_by_author(Round3Blocks, N1Opts),
    VoteBlocks1 = make_round_blocks(
        ProcID,
        VotingRound1,
        [A2, A3, A4],
        Round3ByAuthor,
        #{},
        OptsByAuthor
    ),
    post_blocks(Node, VoteBlocks1, N2Opts),
    VoteByAuthor1 = blocks_by_author(VoteBlocks1, N1Opts),
    DecisionBlocks1 = make_round_blocks(
        ProcID,
        DecisionRound1,
        [A2, A3, A4],
        VoteByAuthor1,
        #{},
        OptsByAuthor
    ),
    post_blocks(Node, DecisionBlocks1, N2Opts),
    Assignments = wait_for_assignments(Node, ProcID, 0, 1, 2, N1Opts),
    Sorted = hb_util:to_sorted_list(Assignments, N1Opts),
    [{Slot0, A0}, {Slot1, A1Assignment} | _] = Sorted,
    ?assertEqual(0, hb_util:int(Slot0)),
    ?assertEqual(1, hb_util:int(Slot1)),
    ?assertEqual(<<"m0">>, hb_ao:get(<<"body/body">>, A0, N1Opts)),
    ?assertEqual(<<"m1">>, hb_ao:get(<<"body/body">>, A1Assignment, N1Opts)).

mysticeti_multi_proposer_commit_test_() ->
    {timeout, 60, fun mysticeti_multi_proposer_commit/0}.

%% @doc Validate multi-proposer slots per round (Algorithm 3 l-loop).
%% Mysticeti-C Algorithm 3: TryDecide over numOfProposers
%% (mysticeti-paper/algorithms/universal_committer.tex).
mysticeti_multi_proposer_commit() ->
    W1 = ar_wallet:new(),
    W2 = ar_wallet:new(),
    W3 = ar_wallet:new(),
    W4 = ar_wallet:new(),
    A1 = hb_util:human_id(ar_wallet:to_address(W1)),
    A2 = hb_util:human_id(ar_wallet:to_address(W2)),
    A3 = hb_util:human_id(ar_wallet:to_address(W3)),
    A4 = hb_util:human_id(ar_wallet:to_address(W4)),
    Validators = [A1, A2, A3, A4],
    Store = hb_test_utils:test_store(),
    N1Opts = test_node_opts(W1, A1, Store),
    N2Opts = test_node_opts(W2, A2, Store),
    N3Opts = test_node_opts(W3, A3, Store),
    N4Opts = test_node_opts(W4, A4, Store),
    OptsByAuthor = #{
        A1 => N1Opts,
        A2 => N2Opts,
        A3 => N3Opts,
        A4 => N4Opts
    },
    Node = hb_http_server:start_node(N1Opts),
    Proc = mysticeti_test_process(Validators, #{ <<"num-proposers">> => 2 }, N1Opts),
    ProcID = hb_message:id(Proc, all, N1Opts),
    {ok, _} = hb_cache:write(Proc, N1Opts),
    _ = dev_mysticeti_registry:find(ProcID, Proc, N1Opts),
    Msg0 = test_message(ProcID, <<"m0">>, N1Opts),
    {ok, _} = hb_cache:write(Msg0, N1Opts),
    {ok, Res0} = http_post_mysticeti_schedule(Node, Proc, Msg0, N1Opts),
    ProposerBlock0 = hb_ao:get(<<"pending/block">>, Res0, undefined, N1Opts),
    Msg1 = test_message(ProcID, <<"m1">>, N2Opts),
    {ok, _} = hb_cache:write(Msg1, N2Opts),
    ProposerBlock1 = test_block(ProcID, A2, 0, [], Msg1, N2Opts),
    R0Others = make_round_blocks(
        ProcID,
        0,
        [A3, A4],
        #{},
        #{},
        OptsByAuthor
    ),
    post_blocks(Node, [ProposerBlock1 | R0Others], N2Opts),
    R0ByAuthor =
        (blocks_by_author([ProposerBlock1 | R0Others], N1Opts))#{ A1 => ProposerBlock0 },
    VoteBlocks = make_round_blocks(
        ProcID,
        1,
        [A2, A3, A4],
        R0ByAuthor,
        #{},
        OptsByAuthor
    ),
    post_blocks(Node, VoteBlocks, N2Opts),
    VoteByAuthor = blocks_by_author(VoteBlocks, N1Opts),
    DecisionBlocks = make_round_blocks(
        ProcID,
        2,
        [A2, A3, A4],
        VoteByAuthor,
        #{},
        OptsByAuthor
    ),
    post_blocks(Node, DecisionBlocks, N2Opts),
    Assignments = wait_for_assignments(Node, ProcID, 0, 1, 2, N1Opts),
    Sorted = hb_util:to_sorted_list(Assignments, N1Opts),
    [{Slot0, A0}, {Slot1, A1Assignment} | _] = Sorted,
    ?assertEqual(0, hb_util:int(Slot0)),
    ?assertEqual(1, hb_util:int(Slot1)),
    ?assertEqual(<<"m0">>, hb_ao:get(<<"body/body">>, A0, N1Opts)),
    ?assertEqual(<<"m1">>, hb_ao:get(<<"body/body">>, A1Assignment, N1Opts)).

mysticeti_invalid_signature_rejected_test_() ->
    {timeout, 60, fun mysticeti_invalid_signature_rejected/0}.

%% @doc Reject blocks with invalid signatures (block correctness).
%% Mysticeti-C Block correctness: signature verification
%% (mysticeti-paper/sections/overview.tex).
mysticeti_invalid_signature_rejected() ->
    W1 = ar_wallet:new(),
    W2 = ar_wallet:new(),
    W3 = ar_wallet:new(),
    W4 = ar_wallet:new(),
    A1 = hb_util:human_id(ar_wallet:to_address(W1)),
    A2 = hb_util:human_id(ar_wallet:to_address(W2)),
    A3 = hb_util:human_id(ar_wallet:to_address(W3)),
    A4 = hb_util:human_id(ar_wallet:to_address(W4)),
    Validators = [A1, A2, A3, A4],
    Store = hb_test_utils:test_store(),
    N1Opts = test_node_opts(W1, A1, Store),
    N2Opts = test_node_opts(W2, A2, Store),
    N3Opts = test_node_opts(W3, A3, Store),
    N4Opts = test_node_opts(W4, A4, Store),
    OptsByAuthor = #{
        A1 => N1Opts,
        A2 => N2Opts,
        A3 => N3Opts,
        A4 => N4Opts
    },
    Node = hb_http_server:start_node(N1Opts),
    Proc = mysticeti_test_process(Validators, N1Opts),
    ProcID = hb_message:id(Proc, all, N1Opts),
    {ok, _} = hb_cache:write(Proc, N1Opts),
    _ = dev_mysticeti_registry:find(ProcID, Proc, N1Opts),
    Msg0 = test_message(ProcID, <<"m0">>, N1Opts),
    {ok, _} = hb_cache:write(Msg0, N1Opts),
    {ok, Res0} = http_post_mysticeti_schedule(Node, Proc, Msg0, N1Opts),
    ProposerBlock0 = hb_ao:get(<<"pending/block">>, Res0, undefined, N1Opts),
    R0Others = make_round_blocks(
        ProcID,
        0,
        [A2, A3, A4],
        #{},
        #{},
        OptsByAuthor
    ),
    post_blocks(Node, R0Others, N2Opts),
    R0ByAuthor = (blocks_by_author(R0Others, N1Opts))#{ A1 => ProposerBlock0 },
    VoteBlocksValid = make_round_blocks(
        ProcID,
        1,
        [A3, A4],
        R0ByAuthor,
        #{},
        OptsByAuthor
    ),
    InvalidVote =
        test_block(
            ProcID,
            A2,
            1,
            parents_for_author(R0ByAuthor, A2),
            undefined,
            N3Opts
        ),
    post_blocks(Node, VoteBlocksValid ++ [InvalidVote], N2Opts),
    VoteByAuthor0 = blocks_by_author(VoteBlocksValid, N1Opts),
    InvalidVoteId = hb_maps:get(<<"id">>, InvalidVote, undefined, N1Opts),
    VoteByAuthor = VoteByAuthor0#{ A2 => InvalidVoteId },
    DecisionBlocks = make_round_blocks(
        ProcID,
        2,
        [A2, A3, A4],
        VoteByAuthor,
        #{},
        OptsByAuthor
    ),
    post_blocks(Node, DecisionBlocks, N2Opts),
    ?debug_wait(500),
    Assignments = fetch_assignments(Node, ProcID, 0, 0, N1Opts),
    ?assertEqual(0, hb_maps:size(Assignments, N1Opts)).
