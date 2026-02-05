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
%%% - Validators are static for now; f = floor((n-1)/3), quorum = 2f+1.
%%% - Blocks are validated for author membership but are not signature-verified.
%%% - Indirect decision (anchor/certified link) is implemented.
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
    Validators0 = validators(Proc, Opts),
    LocalAuthor = local_author(Validators0, Opts),
    Validators = ensure_validator(Validators0, LocalAuthor),
    F = (length(Validators) - 1) div 3,
    Quorum = (2 * F) + 1,
    WaveLength = hb_util:int(hb_opts:get(mysticeti_wave_length, 3, Opts)),
    RoundOffset = hb_util:int(hb_opts:get(mysticeti_round_offset, 0, Opts)),
    ProposerOffset = hb_util:int(hb_opts:get(mysticeti_proposer_offset, 0, Opts)),
    Peers = peers(Proc, Opts),
    #{
        id => ProcID,
        opts => Opts,
        validators => Validators,
        quorum => Quorum,
        wave_length => WaveLength,
        round_offset => RoundOffset,
        proposer_offset => ProposerOffset,
        local_author => LocalAuthor,
        peers => Peers,
        dag => #{},
        round_index => #{},
        max_round => -1,
        ordered => #{},
        assigned => #{},
        last_decided_wave => -1,
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
    {Block, State1} = make_block(State, OnlyCommitted),
    {State2, NewAssignments} = add_block_and_commit(State1, Block),
    MessageId = hb_message:id(OnlyCommitted, all, Opts),
    Result =
        case lists:keyfind(MessageId, 1, NewAssignments) of
            {MessageId, Assignment} -> {committed, Assignment};
            false ->
                {pending, #{
                    <<"block">> => maps:get(<<"id">>, Block),
                    <<"author">> => maps:get(<<"author">>, Block),
                    <<"round">> => maps:get(<<"round">>, Block)
                }}
        end,
    broadcast_block(State2, Block),
    {State2, Result}.

%% @doc Handle an inbound block from a peer.
handle_block(State, Block) ->
    case validate_block(State, Block) of
        ok ->
            {State1, _} = add_block_and_commit(State, Block),
            State1;
        {error, _Reason} ->
            State
    end.

%% @doc Construct a new block for the local author.
make_block(State, Payload) ->
    Round = next_round(State),
    Parents = parent_set(State, Round),
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
    BlockId = hb_message:id(Block0, uncommitted, maps:get(opts, State)),
    Block = Block0#{ <<"id">> => BlockId },
    {Block, State#{ max_round := max(maps:get(max_round, State), Round) }}.

%% @doc Determine the next round for the local author.
next_round(State) ->
    maps:get(max_round, State, -1) + 1.

%% @doc Select parent blocks for a new round.
parent_set(_State, Round) when Round =< 0 ->
    [];
parent_set(State, Round) ->
    PrevRound = Round - 1,
    RoundIndex = maps:get(round_index, State, #{}),
    case maps:get(PrevRound, RoundIndex, undefined) of
        undefined -> [];
        ByAuthor -> maps:values(ByAuthor)
    end.

%% @doc Add a block to the DAG, then try to commit.
add_block_and_commit(State, Block) ->
    case add_block(State, Block) of
        {State1, added} ->
            try_commit(State1);
        {State1, _} ->
            {State1, []}
    end.

%% @doc Insert a block into the DAG.
add_block(State, Block) ->
    Dag = maps:get(dag, State),
    BlockId = maps:get(<<"id">>, Block),
    case maps:is_key(BlockId, Dag) of
        true -> {State, exists};
        false ->
            Round = maps:get(<<"round">>, Block),
            Author = maps:get(<<"author">>, Block),
            RoundIndex0 = maps:get(round_index, State, #{}),
            RoundIndex1 =
                RoundIndex0#{
                    Round =>
                        (maps:get(Round, RoundIndex0, #{}))#{
                            Author => BlockId
                        }
                },
            MaxRound = max(maps:get(max_round, State), Round),
            State1 = State#{
                dag := Dag#{ BlockId => Block },
                round_index := RoundIndex1,
                max_round := MaxRound
            },
            {State1, added}
    end.

%% @doc Attempt to commit waves in order, returning newly created assignments.
%% Implements Mysticeti-C Algorithm 3 (TryDecide), including the indirect rule.
try_commit(State) ->
    WaveLength = maps:get(wave_length, State),
    RoundOffset = maps:get(round_offset, State),
    MaxRound = maps:get(max_round, State),
    LastWave = maps:get(last_decided_wave, State),
    MaxWave = wave_number(MaxRound, WaveLength, RoundOffset),
    MinWave = LastWave + 1,
    case MaxWave < MinWave of
        true -> {State, []};
        false ->
            Sequence =
                build_decision_sequence(
                    State,
                    MaxWave,
                    MinWave,
                    WaveLength,
                    RoundOffset,
                    []
                ),
            apply_decisions(State, Sequence, [])
    end.

build_decision_sequence(_State, Wave, MinWave, _WL, _RO, Seq)
        when Wave < MinWave ->
    Seq;
build_decision_sequence(State, Wave, MinWave, WaveLength, RoundOffset, Seq) ->
    Status0 = direct_decide(State, Wave, WaveLength, RoundOffset),
    Status =
        case Status0 of
            undecided ->
                try_indirect_decide(State, Wave, Seq, WaveLength, RoundOffset);
            _ -> Status0
        end,
    ProposerRound = proposer_round(Wave, WaveLength, RoundOffset),
    build_decision_sequence(
        State,
        Wave - 1,
        MinWave,
        WaveLength,
        RoundOffset,
        [#{ wave => Wave, round => ProposerRound, status => Status } | Seq]
    ).

apply_decisions(State, [], Acc) ->
    {State, lists:reverse(Acc)};
apply_decisions(State, [#{ wave := Wave, status := Status } | Rest], Acc) ->
    case Status of
        undecided ->
            {State, lists:reverse(Acc)};
        {commit, ProposerBlock} ->
            {State1, NewAssignments} = commit_proposer(State, ProposerBlock),
            apply_decisions(
                State1#{ last_decided_wave := Wave },
                Rest,
                NewAssignments ++ Acc
            );
        {skip, _} ->
            apply_decisions(State#{ last_decided_wave := Wave }, Rest, Acc)
    end.

%% @doc Direct decision rule for a wave.
%% Mysticeti-C Algorithm 2 (DirectDecider): TryDirectDecide.
direct_decide(State, Wave, WaveLength, RoundOffset) ->
    ProposerRound = proposer_round(Wave, WaveLength, RoundOffset),
    VotingRound = ProposerRound + 1,
    DecisionRound = decision_round(Wave, WaveLength, RoundOffset),
    ProposerId = predefined_proposer(State, ProposerRound),
    case skipped_proposer(State, ProposerId, ProposerRound, VotingRound) of
        true -> {skip, Wave};
        false ->
            case block_by_author_round(State, ProposerId, ProposerRound) of
                undefined -> undecided;
                ProposerBlockId ->
                    case supported_proposer(
                        State,
                        ProposerBlockId,
                        DecisionRound
                    ) of
                        true -> {commit, ProposerBlockId};
                        false -> undecided
                    end
            end
    end.

%% @doc Check whether the proposer was skipped.
%% Mysticeti-C Algorithm 2 (DirectDecider): SkippedProposer.
skipped_proposer(State, ProposerId, ProposerRound, VotingRound) ->
    Quorum = maps:get(quorum, State),
    VotingBlocks = round_blocks(State, VotingRound),
    Skipped =
        length(
            lists:filter(
                fun(Block) ->
                    not has_parent_from_proposer(
                        State,
                        Block,
                        ProposerId,
                        ProposerRound
                    )
                end,
                VotingBlocks
            )
        ),
    Skipped >= Quorum.

%% @doc Check whether a proposer has enough support to commit.
%% Mysticeti-C Algorithm 2 (DirectDecider): SupportedProposer,
%% using Algorithm 1's IsCert predicate.
supported_proposer(State, ProposerBlockId, DecisionRound) ->
    Quorum = maps:get(quorum, State),
    DecisionBlocks = round_blocks(State, DecisionRound),
    CertBlocks =
        lists:filter(
            fun(Block) ->
                is_certificate(State, Block, ProposerBlockId)
            end,
            DecisionBlocks
        ),
    length(CertBlocks) >= Quorum.

%% @doc Determine if a block is a certificate for the proposer.
%% Mysticeti-C Algorithm 1: IsCert (>= 2f+1 votes in parents).
is_certificate(State, Block, ProposerBlockId) ->
    Quorum = maps:get(quorum, State),
    Parents = maps:get(<<"parents">>, Block, []),
    Votes =
        length(
            lists:filter(
                fun(ParentId) ->
                    is_vote(State, ParentId, ProposerBlockId)
                end,
                Parents
            )
        ),
    Votes >= Quorum.

%% @doc A parent votes for a proposer if it has the proposer in its ancestry.
%% Mysticeti-C Algorithm 1: IsVote, using Link/SupportedBlock via ancestry.
is_vote(State, ParentId, ProposerBlockId) ->
    has_ancestor(State, ParentId, ProposerBlockId).

%% @doc Check if a block has a specific ancestor.
%% Mysticeti-C Algorithm 1: Link predicate.
has_ancestor(State, StartId, TargetId) ->
    case StartId == TargetId of
        true -> true;
        false ->
            Dag = maps:get(dag, State),
            has_ancestor(Dag, [StartId], TargetId, #{})
    end.

has_ancestor(_Dag, [], _TargetId, _Visited) -> false;
has_ancestor(Dag, [Id | Rest], TargetId, Visited) ->
    case maps:is_key(Id, Visited) of
        true -> has_ancestor(Dag, Rest, TargetId, Visited);
        false ->
            case maps:get(Id, Dag, undefined) of
                undefined ->
                    has_ancestor(Dag, Rest, TargetId, Visited#{ Id => true});
                Block ->
                    Parents = maps:get(<<"parents">>, Block, []),
                    case lists:member(TargetId, Parents) of
                        true -> true;
                        false ->
                            has_ancestor(
                                Dag,
                                Parents ++ Rest,
                                TargetId,
                                Visited#{ Id => true }
                            )
                    end
            end
    end.

%% @doc Check whether a block has a direct parent from the proposer.
%% Used by SkippedProposer to detect proposer presence in voting round.
has_parent_from_proposer(State, Block, ProposerId, ProposerRound) ->
    Parents = maps:get(<<"parents">>, Block, []),
    lists:any(
        fun(ParentId) ->
            case maps:get(ParentId, maps:get(dag, State), undefined) of
                undefined -> false;
                Parent ->
                    maps:get(<<"author">>, Parent, undefined) =:= ProposerId
                        andalso maps:get(<<"round">>, Parent, undefined) =:= ProposerRound
            end
        end,
        Parents
    ).

%% @doc Indirect decision rule for a wave.
%% Mysticeti-C Algorithm 3: TryIndirectDecide.
try_indirect_decide(State, Wave, Sequence, WaveLength, RoundOffset) ->
    DecisionRound = decision_round(Wave, WaveLength, RoundOffset),
    ProposerRound = proposer_round(Wave, WaveLength, RoundOffset),
    ProposerId = predefined_proposer(State, ProposerRound),
    ProposerBlockId = block_by_author_round(State, ProposerId, ProposerRound),
    Anchors =
        lists:filter(
            fun(#{ round := R }) -> R > DecisionRound end,
            Sequence
        ),
    try_indirect_anchors(
        State,
        ProposerBlockId,
        DecisionRound,
        Wave,
        Anchors
    ).

try_indirect_anchors(_State, _ProposerBlockId, _DecisionRound, _Wave, []) ->
    undecided;
try_indirect_anchors(State, ProposerBlockId, DecisionRound, Wave,
        [#{ status := Status } | Rest]) ->
    case Status of
        undecided -> undecided;
        {skip, _} -> {skip, Wave};
        {commit, AnchorBlockId} ->
            case ProposerBlockId of
                undefined -> undecided;
                _ ->
                    case certified_link(
                        State,
                        AnchorBlockId,
                        ProposerBlockId,
                        DecisionRound
                    ) of
                        true -> {commit, ProposerBlockId};
                        false -> {skip, Wave}
                    end
            end;
        _ ->
            try_indirect_anchors(
                State,
                ProposerBlockId,
                DecisionRound,
                Wave,
                Rest
            )
    end.

%% @doc Check if there is a certified link between the anchor and the proposer.
%% Mysticeti-C Algorithm 1: CertifiedLink.
certified_link(State, AnchorBlockId, ProposerBlockId, DecisionRound) ->
    DecisionBlocks = round_blocks(State, DecisionRound),
    lists:any(
        fun(Block) ->
            is_certificate(State, Block, ProposerBlockId) andalso
                has_ancestor(
                    State,
                    AnchorBlockId,
                    maps:get(<<"id">>, Block)
                )
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
    collect_ancestors(Dag, [RootId], #{}, []).

collect_ancestors(_Dag, [], _Visited, Acc) -> Acc;
collect_ancestors(Dag, [Id | Rest], Visited, Acc) ->
    case maps:is_key(Id, Visited) of
        true -> collect_ancestors(Dag, Rest, Visited, Acc);
        false ->
            case maps:get(Id, Dag, undefined) of
                undefined ->
                    collect_ancestors(Dag, Rest, Visited#{ Id => true }, Acc);
                Block ->
                    Parents = maps:get(<<"parents">>, Block, []),
                    collect_ancestors(
                        Dag,
                        Parents ++ Rest,
                        Visited#{ Id => true },
                        [Id | Acc]
                    )
            end
    end.

%% @doc Order blocks deterministically by (round, author, id).
%% This provides a stable total order for process@1.0.
order_blocks(State, BlockIds) ->
    Dag = maps:get(dag, State),
    lists:sort(
        fun(A, B) ->
            BA = maps:get(A, Dag),
            BB = maps:get(B, Dag),
            {maps:get(<<"round">>, BA), maps:get(<<"author">>, BA), A}
                < {maps:get(<<"round">>, BB), maps:get(<<"author">>, BB), B}
        end,
        BlockIds
    ).

%% @doc Create assignments for ordered blocks.
%% Only blocks with a payload (body) produce assignments.
assign_blocks(State, [], Acc) -> {State, lists:reverse(Acc)};
assign_blocks(State, [Id | Rest], Acc) ->
    Dag = maps:get(dag, State),
    Block = maps:get(Id, Dag),
    Payload = maps:get(<<"body">>, Block, undefined),
    case Payload of
        undefined ->
            assign_blocks(State, Rest, Acc);
        _ ->
            Opts = maps:get(opts, State),
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

%% @doc Determine the round for the proposer in a given wave.
proposer_round(Wave, WaveLength, RoundOffset) ->
    Wave * WaveLength + RoundOffset.

%% @doc Determine the decision round for a given wave.
decision_round(Wave, WaveLength, RoundOffset) ->
    Wave * WaveLength + WaveLength - 1 + RoundOffset.

%% @doc Convert an arbitrary round to its wave number.
wave_number(Round, _WaveLength, RoundOffset) when Round < RoundOffset ->
    -1;
wave_number(Round, WaveLength, RoundOffset) ->
    (Round - RoundOffset) div WaveLength.

%% @doc Select a proposer deterministically (round-robin).
predefined_proposer(State, Round) ->
    Validators = maps:get(validators, State),
    ProposerOffset = maps:get(proposer_offset, State),
    Index = (Round + ProposerOffset) rem length(Validators),
    lists:nth(Index + 1, Validators).

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

%% @doc Validate a received block.
validate_block(State, Block) ->
    Validators = maps:get(validators, State),
    Author = maps:get(<<"author">>, Block, undefined),
    case lists:member(Author, Validators) of
        false -> {error, invalid_author};
        true ->
            case maps:get(<<"process">>, Block, undefined) =:= maps:get(id, State) of
                true -> ok;
                false -> {error, wrong_process}
            end
    end.

%% @doc Summarize the server state for info.
summarize(State) ->
    #{
        current => maps:get(current_slot, State),
        max_round => maps:get(max_round, State),
        validators => maps:get(validators, State),
        quorum => maps:get(quorum, State),
        wave_length => maps:get(wave_length, State),
        last_decided_wave => maps:get(last_decided_wave, State)
    }.

%% @doc Extract validators from process config or opts.
validators(Proc, Opts) ->
    Raw =
        hb_ao:get(
            <<"mysticeti/validators">>,
            Proc,
            hb_ao:get(<<"validators">>, Proc, not_found, Opts#{ hashpath => ignore }),
            Opts#{ hashpath => ignore }
        ),
    case Raw of
        not_found ->
            normalize_validators(hb_opts:get(mysticeti_validators, [hb:address()], Opts));
        _ ->
            normalize_validators(Raw)
    end.

normalize_validators(Validators) when is_list(Validators) ->
    hb_util:unique(
        lists:map(fun normalize_validator/1, Validators)
    );
normalize_validators(Validators) when is_binary(Validators) ->
    normalize_validators(
        binary:split(
            binary:replace(Validators, <<"\"">>, <<"">>, [global]),
            <<",">>,
            [global, trim_all]
        )
    );
normalize_validators(Validators) ->
    normalize_validators([Validators]).

normalize_validator(#{<<"id">> := Id}) -> hb_util:bin(Id);
normalize_validator(#{id := Id}) -> hb_util:bin(Id);
normalize_validator(Id) -> hb_util:bin(Id).

local_author(Validators, Opts) ->
    Local = hb_opts:get(mysticeti_author, hb:address(), Opts),
    case lists:member(Local, Validators) of
        true -> Local;
        false -> Local
    end.

ensure_validator(Validators, LocalAuthor) ->
    case lists:member(LocalAuthor, Validators) of
        true -> Validators;
        false -> [LocalAuthor | Validators]
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

test_node_opts(Wallet, Author) ->
    #{
        store => [hb_test_utils:test_store()],
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
    BlockId = hb_message:id(Block0, uncommitted, Opts),
    Block0#{ <<"id">> => BlockId }.

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
    hb_message:commit(
        #{
            <<"device">> => <<"process@1.0">>,
            <<"scheduler-device">> => <<"mysticeti@1.0">>,
            <<"scheduler-location">> => Validators,
            <<"mysticeti">> => #{ <<"validators">> => Validators },
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
wave_rounds(Wave, Opts) ->
    WaveLength = hb_util:int(hb_opts:get(mysticeti_wave_length, 3, Opts)),
    RoundOffset = hb_util:int(hb_opts:get(mysticeti_round_offset, 0, Opts)),
    ProposerRound = proposer_round(Wave, WaveLength, RoundOffset),
    VotingRound = ProposerRound + 1,
    DecisionRound = decision_round(Wave, WaveLength, RoundOffset),
    {ProposerRound, VotingRound, DecisionRound}.

%% @doc Deterministic leader selection (round-robin).
%% Mysticeti-C Algorithm 2: PredefinedProposer.
proposer_for_round(Validators, Round, Opts) ->
    Offset = hb_util:int(hb_opts:get(mysticeti_proposer_offset, 0, Opts)),
    Index = (Round + Offset) rem length(Validators),
    lists:nth(Index + 1, Validators).

%% @doc Generate voting-round blocks (no payload) that reference the proposer.
%% These blocks exist purely for IsVote/IsCert checks in Algorithm 1.
make_vote_blocks(ProcID, ParentId, Round, Authors, Opts) ->
    Blocks =
        lists:map(
            fun(Author) ->
                test_block(ProcID, Author, Round, [ParentId], undefined, Opts)
            end,
            Authors
        ),
    {Blocks, [maps:get(<<"id">>, B) || B <- Blocks]}.

%% @doc Generate decision-round blocks (no payload) to satisfy IsCert.
make_decision_blocks(ProcID, ParentIds, Round, Authors, Opts) ->
    lists:map(
        fun(Author) ->
            test_block(ProcID, Author, Round, ParentIds, undefined, Opts)
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
    N1Opts = test_node_opts(W1, A1),
    N2Opts = test_node_opts(W2, A2),
    N1 = hb_http_server:start_node(N1Opts),
    _N2 = hb_http_server:start_node(N2Opts),
    Proc =
        hb_message:commit(
            #{
                <<"device">> => <<"process@1.0">>,
                <<"scheduler-device">> => <<"mysticeti@1.0">>,
                <<"scheduler-location">> => Validators,
                <<"mysticeti">> => #{
                    <<"validators">> => Validators
                },
                <<"type">> => <<"Process">>
            },
            N1Opts
        ),
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
    V1 = test_block(
        ProcID,
        A2,
        1,
        [ProposerBlockId],
        hb_message:commit(#{ <<"body">> => <<"v1">> }, N1Opts),
        N1Opts
    ),
    V2 = test_block(
        ProcID,
        A3,
        1,
        [ProposerBlockId],
        hb_message:commit(#{ <<"body">> => <<"v2">> }, N1Opts),
        N1Opts
    ),
    V3 = test_block(
        ProcID,
        A4,
        1,
        [ProposerBlockId],
        hb_message:commit(#{ <<"body">> => <<"v3">> }, N1Opts),
        N1Opts
    ),
    V1Id = maps:get(<<"id">>, V1),
    V2Id = maps:get(<<"id">>, V2),
    V3Id = maps:get(<<"id">>, V3),
    D1 = test_block(
        ProcID,
        A2,
        2,
        [V1Id, V2Id, V3Id],
        hb_message:commit(#{ <<"body">> => <<"d1">> }, N1Opts),
        N1Opts
    ),
    D2 = test_block(
        ProcID,
        A3,
        2,
        [V1Id, V2Id, V3Id],
        hb_message:commit(#{ <<"body">> => <<"d2">> }, N1Opts),
        N1Opts
    ),
    D3 = test_block(
        ProcID,
        A4,
        2,
        [V1Id, V2Id, V3Id],
        hb_message:commit(#{ <<"body">> => <<"d3">> }, N1Opts),
        N1Opts
    ),
    {ok, _} = http_post_mysticeti_block(N1, V1, N2Opts),
    {ok, _} = http_post_mysticeti_block(N1, V2, N2Opts),
    {ok, _} = http_post_mysticeti_block(N1, V3, N2Opts),
    {ok, _} = http_post_mysticeti_block(N1, D1, N2Opts),
    {ok, _} = http_post_mysticeti_block(N1, D2, N2Opts),
    {ok, _} = http_post_mysticeti_block(N1, D3, N2Opts),
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
    Opts = test_node_opts(W1, A1),
    Node = hb_http_server:start_node(Opts),
    Proc = mysticeti_test_process(Validators, Opts),
    ProcID = hb_message:id(Proc, all, Opts),
    {ok, _} = hb_cache:write(Proc, Opts),
    _ = dev_mysticeti_registry:find(ProcID, Proc, Opts),
    Msg = test_message(ProcID, <<"m0">>, Opts),
    {ok, Res0} = http_post_mysticeti_schedule(Node, Proc, Msg, Opts),
    ProposerBlockId = hb_ao:get(<<"pending/block">>, Res0, undefined, Opts),
    {_, VotingRound, DecisionRound} = wave_rounds(0, Opts),
    {VoteBlocks, VoteIds} =
        make_vote_blocks(ProcID, ProposerBlockId, VotingRound, [A2, A3], Opts),
    DecisionBlocks =
        make_decision_blocks(ProcID, VoteIds, DecisionRound, [A2, A3], Opts),
    post_blocks(Node, VoteBlocks, Opts),
    post_blocks(Node, DecisionBlocks, Opts),
    ?debug_wait(500),
    Assignments = fetch_assignments(Node, ProcID, 0, 0, Opts),
    ?assertEqual(0, hb_maps:size(Assignments, Opts)).

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
    Opts = test_node_opts(W2, A2),
    Node = hb_http_server:start_node(Opts),
    Proc = mysticeti_test_process(Validators, Opts),
    ProcID = hb_message:id(Proc, all, Opts),
    {ok, _} = hb_cache:write(Proc, Opts),
    _ = dev_mysticeti_registry:find(ProcID, Proc, Opts),
    Msg = test_message(ProcID, <<"m-skip">>, Opts),
    {ok, Res0} = http_post_mysticeti_schedule(Node, Proc, Msg, Opts),
    LocalBlockId = hb_ao:get(<<"pending/block">>, Res0, undefined, Opts),
    {_, VotingRound, DecisionRound} = wave_rounds(0, Opts),
    {VoteBlocks, VoteIds} =
        make_vote_blocks(ProcID, LocalBlockId, VotingRound, [A2, A3, A4], Opts),
    DecisionBlocks =
        make_decision_blocks(ProcID, VoteIds, DecisionRound, [A2], Opts),
    post_blocks(Node, VoteBlocks, Opts),
    post_blocks(Node, DecisionBlocks, Opts),
    ?debug_wait(500),
    Assignments = fetch_assignments(Node, ProcID, 0, 0, Opts),
    ?assertEqual(0, hb_maps:size(Assignments, Opts)).

mysticeti_two_wave_order_test_() ->
    {timeout, 60, fun mysticeti_two_wave_order/0}.

%% @doc Validate two-wave ordering and total order stability.
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
    Opts = test_node_opts(W1, A1),
    Node = hb_http_server:start_node(Opts),
    Proc = mysticeti_test_process(Validators, Opts),
    ProcID = hb_message:id(Proc, all, Opts),
    {ok, _} = hb_cache:write(Proc, Opts),
    _ = dev_mysticeti_registry:find(ProcID, Proc, Opts),
    Msg0 = test_message(ProcID, <<"m0">>, Opts),
    {ok, Res0} = http_post_mysticeti_schedule(Node, Proc, Msg0, Opts),
    ProposerBlock0 = hb_ao:get(<<"pending/block">>, Res0, undefined, Opts),
    {_, VotingRound0, DecisionRound0} = wave_rounds(0, Opts),
    {VoteBlocks0, VoteIds0} =
        make_vote_blocks(ProcID, ProposerBlock0, VotingRound0, [A2, A3, A4], Opts),
    DecisionBlocks0 =
        make_decision_blocks(ProcID, VoteIds0, DecisionRound0, [A2, A3, A4], Opts),
    post_blocks(Node, VoteBlocks0, Opts),
    post_blocks(Node, DecisionBlocks0, Opts),
    _ = wait_for_assignments(Node, ProcID, 0, 0, 1, Opts),
    DecisionIds0 = [maps:get(<<"id">>, B) || B <- DecisionBlocks0],
    {ProposerRound1, VotingRound1, DecisionRound1} = wave_rounds(1, Opts),
    Proposer1 = proposer_for_round(Validators, ProposerRound1, Opts),
    Msg1 = hb_message:commit(#{ <<"body">> => <<"m1">> }, Opts),
    ProposerBlock1 =
        test_block(ProcID, Proposer1, ProposerRound1, DecisionIds0, Msg1, Opts),
    {VoteBlocks1, VoteIds1} =
        make_vote_blocks(ProcID, maps:get(<<"id">>, ProposerBlock1),
            VotingRound1, [A1, A2, A3], Opts),
    DecisionBlocks1 =
        make_decision_blocks(ProcID, VoteIds1, DecisionRound1, [A1, A2, A3], Opts),
    post_blocks(Node, [ProposerBlock1], Opts),
    post_blocks(Node, VoteBlocks1, Opts),
    post_blocks(Node, DecisionBlocks1, Opts),
    Assignments = wait_for_assignments(Node, ProcID, 0, 1, 2, Opts),
    Sorted = hb_util:to_sorted_list(Assignments, Opts),
    [{Slot0, A0}, {Slot1, A1Assignment} | _] = Sorted,
    ?assertEqual(0, hb_util:int(Slot0)),
    ?assertEqual(1, hb_util:int(Slot1)),
    ?assertEqual(<<"m0">>, hb_ao:get(<<"body/body">>, A0, Opts)),
    ?assertEqual(<<"m1">>, hb_ao:get(<<"body/body">>, A1Assignment, Opts)).

mysticeti_indirect_commit_test_() ->
    {timeout, 60, fun mysticeti_indirect_commit/0}.

%% @doc Validate TryIndirectDecide (Algorithm 3) via a later committed anchor.
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
    Opts = test_node_opts(W1, A1),
    Node = hb_http_server:start_node(Opts),
    Proc = mysticeti_test_process(Validators, Opts),
    ProcID = hb_message:id(Proc, all, Opts),
    {ok, _} = hb_cache:write(Proc, Opts),
    _ = dev_mysticeti_registry:find(ProcID, Proc, Opts),
    Msg0 = test_message(ProcID, <<"m0">>, Opts),
    {ok, Res0} = http_post_mysticeti_schedule(Node, Proc, Msg0, Opts),
    ProposerBlock0 = hb_ao:get(<<"pending/block">>, Res0, undefined, Opts),
    {_, VotingRound0, DecisionRound0} = wave_rounds(0, Opts),
    {VoteBlocks0, VoteIds0} =
        make_vote_blocks(ProcID, ProposerBlock0, VotingRound0, [A2, A3, A4], Opts),
    % Only one decision block -> direct decision cannot commit wave 0.
    [DecisionBlock0] =
        make_decision_blocks(ProcID, VoteIds0, DecisionRound0, [A2], Opts),
    DecisionId0 = maps:get(<<"id">>, DecisionBlock0),
    post_blocks(Node, VoteBlocks0, Opts),
    post_blocks(Node, [DecisionBlock0], Opts),
    {ProposerRound1, VotingRound1, DecisionRound1} = wave_rounds(1, Opts),
    Proposer1 = proposer_for_round(Validators, ProposerRound1, Opts),
    % Anchor block links to DecisionBlock0 so CertifiedLink can succeed.
    AnchorBlock =
        test_block(
            ProcID,
            Proposer1,
            ProposerRound1,
            [DecisionId0],
            undefined,
            Opts
        ),
    {VoteBlocks1, VoteIds1} =
        make_vote_blocks(ProcID, maps:get(<<"id">>, AnchorBlock),
            VotingRound1, [A1, A2, A3], Opts),
    DecisionBlocks1 =
        make_decision_blocks(ProcID, VoteIds1, DecisionRound1, [A1, A2, A3], Opts),
    post_blocks(Node, [AnchorBlock], Opts),
    post_blocks(Node, VoteBlocks1, Opts),
    post_blocks(Node, DecisionBlocks1, Opts),
    Assignments = wait_for_assignments(Node, ProcID, 0, 0, 1, Opts),
    [{Slot, Assignment} | _] = hb_util:to_sorted_list(Assignments, Opts),
    ?assertEqual(0, hb_util:int(Slot)),
    ?assertEqual(<<"m0">>, hb_ao:get(<<"body/body">>, Assignment, Opts)).
