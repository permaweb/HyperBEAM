%%% @doc Mysticeti-C test vectors and unit tests.
%%%
%%% These tests drive `process@1.0` HTTP (`/ID/schedule`) and build DAGs that
%%% exercise the paper's commit rules:
%%% - mysticeti-paper/algorithms/consensus_utils.tex (Alg. 1 predicates),
%%% - mysticeti-paper/algorithms/universal_committer.tex (Alg. 3 committer),
%%% - mysticeti-paper/sections/overview.tex (block correctness),
%%% - mysticeti-paper/sections/security.tex (integrity, duplicate sequence numbers).
-module(dev_mysticeti_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Helper functions

%% @doc Read a value from a test context map.
ctx_get(Key, Ctx, Default) ->
    hb_maps:get(Key, Ctx, Default, #{}).

%% @doc Read a value from a test context map with default `undefined`.
ctx_get(Key, Ctx) ->
    ctx_get(Key, Ctx, undefined).

%% @doc Return current time in milliseconds.
scheduler_time() ->
    erlang:system_time(millisecond).

%% @doc Ensure a block has a deterministic id (not part of the signed payload).
ensure_block_id(Block, Opts) ->
    BlockNoId = hb_maps:remove(<<"id">>, Block, Opts),
    BlockId = hb_message:id(BlockNoId, all, Opts),
    case hb_maps:get(<<"id">>, Block, undefined, Opts) of
        undefined -> BlockNoId#{ <<"id">> => BlockId };
        BlockId -> BlockNoId#{ <<"id">> => BlockId };
        _ -> {error, id_mismatch}
    end.

%% @doc Build node options for a test author.
test_node_opts(Wallet, Author, Store) ->
    #{
        store => [Store],
        priv_wallet => Wallet,
        mysticeti_author => Author
    }.

%% @doc Build a test author record with wallet, address, and opts.
test_author(Store) ->
    Wallet = ar_wallet:new(),
    Author = hb_util:human_id(ar_wallet:to_address(Wallet)),
    #{
        wallet => Wallet,
        author => Author,
        opts => test_node_opts(Wallet, Author, Store)
    }.

%% @doc Build a test context with N validators and optional config overrides.
test_context(Count, Overrides) ->
    Store = hb_test_utils:test_store(),
    Authors = [test_author(Store) || _ <- lists:seq(1, Count)],
    AuthorIds = [hb_maps:get(author, A, undefined, #{}) || A <- Authors],
    OptsByAuthor =
        hb_maps:from_list(
            [{hb_maps:get(author, A, undefined, #{}), hb_maps:get(opts, A, #{}, #{})}
             || A <- Authors]
        ),
    Primary = hd(Authors),
    PrimaryOpts = hb_maps:get(opts, Primary, #{}, #{}),
    Node = hb_http_server:start_node(PrimaryOpts),
    Proc = mysticeti_test_process(AuthorIds, Overrides, PrimaryOpts),
    ProcID = hb_util:human_id(hb_message:id(Proc, all, PrimaryOpts)),
    lists:foreach(
        fun(#{ opts := Opts }) -> {ok, _} = hb_cache:write(Proc, Opts) end,
        Authors
    ),
    Pid = dev_mysticeti_registry:find(ProcID, Proc, PrimaryOpts),
    #{
        node => Node,
        proc => Proc,
        proc_id => ProcID,
        pid => Pid,
        authors => AuthorIds,
        primary => hb_maps:get(author, Primary, undefined, #{}),
        primary_opts => PrimaryOpts,
        opts_by_author => OptsByAuthor
    }.

%% @doc Return the opts for a given author.
opts_for(Ctx, Author) ->
    hb_maps:get(Author, hb_maps:get(opts_by_author, Ctx, #{}, #{}), #{}, #{}).

%% @doc Return all authors except the primary.
other_authors(Ctx) ->
    lists:delete(
        hb_maps:get(primary, Ctx, undefined, #{}),
        hb_maps:get(authors, Ctx, [], #{})
    ).

%% @doc Create a signed Mysticeti block for tests.
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
    case ensure_block_id(Signed, Opts) of
        {error, _} = Err -> erlang:error(Err);
        Block -> Block
    end.

%% @doc Post /<process>/schedule with a signed message.
post_process_schedule(Node, ProcID, Msg, Opts) ->
    dev_mysticeti_test_utils:post_process_schedule(Node, ProcID, Msg, Opts).

%% @doc Build a process configured for Mysticeti tests with overrides.
mysticeti_test_process(Validators, Overrides, Opts) ->
    Stakers = [#{ <<"id">> => V, <<"stake">> => 1 } || V <- Validators],
    Mysticeti0 = #{
        <<"validators">> => Validators,
        <<"stakers">> => Stakers,
        <<"peers">> => [],
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

%% @doc Build a signed test message targeting a process.
test_message(ProcID, Body, Opts) ->
    hb_message:commit(
        #{
            <<"target">> => ProcID,
            <<"body">> => Body,
            <<"type">> => <<"Message">>
        },
        Opts
    ).

%% @doc Schedule a test body and return {Message, ProposerBlockId}.
schedule_body(Ctx, Body) ->
    Opts = ctx_get(primary_opts, Ctx, #{}),
    ProcID = ctx_get(proc_id, Ctx),
    Msg = test_message(ProcID, Body, Opts),
    {ok, _} = hb_cache:write(Msg, Opts),
    {ok, Res} = post_process_schedule(ctx_get(node, Ctx), ProcID, Msg, Opts),
    Pending = hb_maps:get(<<"pending">>, Res, #{}, Opts),
    BlockId = hb_maps:get(<<"block">>, Pending, undefined, Opts),
    {Msg, BlockId}.

%% @doc Post a list of blocks to the local node.
post_blocks(Pid, Blocks) ->
    lists:foreach(
        fun(Block) ->
            ok = dev_mysticeti_server:ingest_block(Pid, Block)
        end,
        Blocks
    ).

%% @doc Build an author->block_id map from a list of blocks.
blocks_by_author(Blocks, Opts) ->
    hb_maps:from_list(
        lists:map(
            fun(Block) ->
                {hb_maps:get(<<"author">>, Block, undefined, Opts),
                 hb_maps:get(<<"id">>, Block, undefined, Opts)}
            end,
            Blocks
        )
    ).

%% @doc Build parent list with the author's previous block first.
parents_for_author(PrevRoundByAuthor, Author, Opts) ->
    Ordered = lists:sort(hb_maps:to_list(PrevRoundByAuthor, Opts)),
    OwnPrev = hb_maps:get(Author, PrevRoundByAuthor, undefined, Opts),
    Others = [Id || {A, Id} <- Ordered, A =/= Author],
    [OwnPrev | Others].

%% @doc Generate a full round of valid blocks.
make_round_blocks(ProcID, Round, Authors, PrevRoundByAuthor, Payloads, OptsByAuthor) ->
    lists:map(
        fun(Author) ->
            Opts = hb_maps:get(Author, OptsByAuthor, #{}, #{}),
            Parents =
                case Round of
                    0 -> [];
                    _ -> parents_for_author(PrevRoundByAuthor, Author, Opts)
                end,
            Payload = hb_maps:get(Author, Payloads, undefined, Opts),
            test_block(ProcID, Author, Round, Parents, Payload, Opts)
        end,
        Authors
    ).

%% @doc Build and post a round of blocks, returning {Blocks, ByAuthor}.
post_round(Ctx, Round, Authors, PrevRoundByAuthor, Payloads) ->
    ProcID = ctx_get(proc_id, Ctx),
    OptsByAuthor = ctx_get(opts_by_author, Ctx, #{}),
    Blocks = make_round_blocks(ProcID, Round, Authors, PrevRoundByAuthor, Payloads, OptsByAuthor),
    post_blocks(ctx_get(pid, Ctx), Blocks),
    {Blocks, blocks_by_author(Blocks, ctx_get(primary_opts, Ctx, #{}))}.

%% @doc Fetch assignments for a slot range and normalize numeric keys.
fetch_assignments(Ctx, From, To) ->
    Opts = ctx_get(primary_opts, Ctx, #{}),
    ProcID = ctx_get(proc_id, Ctx),
    dev_mysticeti_test_utils:fetch_assignments_http(ctx_get(node, Ctx), ProcID, From, To, Opts).

%% @doc Wait until a minimum number of assignments is available.
wait_for_assignments(Ctx, From, To, Expected) ->
    Opts = ctx_get(primary_opts, Ctx, #{}),
    _ = hb_util:wait_until(
        fun() ->
            hb_maps:size(fetch_assignments(Ctx, From, To), Opts) >= Expected
        end,
        5000
    ),
    fetch_assignments(Ctx, From, To).

%% @doc Compute proposer/voting/decision rounds for a wave.
%% Mysticeti-C Algorithm 2: ProposerRound and DecisionRound.
wave_rounds(Wave, Proc, Opts) ->
    WaveLength = wave_length(Proc, Opts),
    ProposerRound = Wave * WaveLength,
    VotingRound = ProposerRound + 1,
    DecisionRound = ProposerRound + WaveLength - 1,
    {ProposerRound, VotingRound, DecisionRound}.

%% @doc Deterministic leader selection (round-robin).
%% Mysticeti-C Algorithm 2: PredefinedProposer.
proposer_for_round(Validators, Round, Proc, Opts) ->
    Offset = proposer_offset(Proc, Opts),
    Index = (Round + Offset) rem length(Validators),
    lists:nth(Index + 1, Validators).

%% @doc Extract wave length from the process config.
wave_length(Proc, Opts) ->
    hb_util:int(mysticeti_required(Proc, <<"wave-length">>, Opts)).

%% @doc Extract proposer offset from the process config.
proposer_offset(Proc, Opts) ->
    hb_util:int(mysticeti_required(Proc, <<"proposer-offset">>, Opts)).

%% @doc Read a required mysticeti config key.
mysticeti_required(Proc, Key, Opts) ->
    Mysticeti = hb_maps:get(<<"mysticeti">>, Proc, not_found, Opts),
    case Mysticeti of
        not_found -> erlang:error({missing_mysticeti_config, Key});
        _ ->
            case hb_maps:get(Key, Mysticeti, not_found, Opts) of
                not_found -> erlang:error({missing_mysticeti_config, Key});
                Value -> Value
            end
    end.

%% @doc Build vote parents with a specific proposer ordering.
%% Ensures the author's previous block is first.
vote_parents(OwnPrev, Preferred, Other, AllRound0) ->
    Base = [OwnPrev, Preferred, Other],
    Rest = [Id || Id <- AllRound0, not lists:member(Id, Base)],
    Base ++ Rest.

%%% Tests

%% @doc Validate direct commit (Algorithm 2: SupportedProposer + IsCert).
mysticeti_quorum_commit_test_() ->
    {timeout, 60, fun mysticeti_quorum_commit/0}.

%% @doc Validate direct commit (Algorithm 2: SupportedProposer + IsCert).
mysticeti_quorum_commit() ->
    Ctx = test_context(4, #{}),
    {_Msg0, ProposerBlock0} = schedule_body(Ctx, <<"m0">>),
    Primary = ctx_get(primary, Ctx),
    {_, R0ByAuthor} = post_round(Ctx, 0, other_authors(Ctx), #{}, #{}),
    {_, VoteByAuthor} =
        post_round(
            Ctx,
            1,
            other_authors(Ctx),
            R0ByAuthor#{ Primary => ProposerBlock0 },
            #{}
        ),
    _ = post_round(Ctx, 2, other_authors(Ctx), VoteByAuthor, #{}),
    Assignments = wait_for_assignments(Ctx, 0, 1, 1),
    Opts = ctx_get(primary_opts, Ctx, #{}),
    [{Slot, Assignment} | _] = hb_util:to_sorted_list(Assignments, Opts),
    ?assertEqual(0, hb_util:int(Slot)),
    ?assertEqual(<<"m0">>, dev_mysticeti_test_utils:assignment_body(Assignment, Opts)).

mysticeti_no_quorum_commit_test_() ->
    {timeout, 60, fun mysticeti_no_quorum_commit/0}.

%% @doc Ensure insufficient certificates do not commit (Algorithm 1: IsCert).
mysticeti_no_quorum_commit() ->
    Ctx = test_context(4, #{}),
    {_Msg0, ProposerBlock0} = schedule_body(Ctx, <<"m0">>),
    Primary = ctx_get(primary, Ctx),
    {_, R0ByAuthor} = post_round(Ctx, 0, other_authors(Ctx), #{}, #{}),
    {_, VoteByAuthor} =
        post_round(
            Ctx,
            1,
            other_authors(Ctx),
            R0ByAuthor#{ Primary => ProposerBlock0 },
            #{}
        ),
    DecisionAuthors = lists:sublist(other_authors(Ctx), 2),
    _ = post_round(Ctx, 2, DecisionAuthors, VoteByAuthor, #{}),
    Assignments = fetch_assignments(Ctx, 0, 0),
    ?assertEqual(0, hb_maps:size(Assignments, ctx_get(primary_opts, Ctx, #{}))).

%% @doc Test skip decision when proposer is not supported.
mysticeti_skip_wave_test_() ->
    {timeout, 60, fun mysticeti_skip_wave/0}.

%% @doc Validate SkippedProposer behavior (Algorithm 2: SkippedProposer).
mysticeti_skip_wave() ->
    Ctx = test_context(4, #{}),
    {_Msg0, _ProposerBlock0} = schedule_body(Ctx, <<"m-skip">>),
    {_, R0ByAuthorNoA1} = post_round(Ctx, 0, other_authors(Ctx), #{}, #{}),
    _ = post_round(Ctx, 1, other_authors(Ctx), R0ByAuthorNoA1, #{}),
    Assignments = fetch_assignments(Ctx, 0, 0),
    ?assertEqual(0, hb_maps:size(Assignments, ctx_get(primary_opts, Ctx, #{}))).

%% @doc Test ordering across two waves.
mysticeti_two_wave_order_test_() ->
    {timeout, 60, fun mysticeti_two_wave_order/0}.

%% @doc Validate two-wave ordering and total order stability.
%% Mysticeti-C Algorithm 3: TryDecide ordering across waves
%% (mysticeti-paper/algorithms/universal_committer.tex).
mysticeti_two_wave_order() ->
    Ctx = test_context(4, #{}),
    {_Msg0, ProposerBlock0} = schedule_body(Ctx, <<"m0">>),
    Primary = ctx_get(primary, Ctx),
    {_, R0ByAuthor} = post_round(Ctx, 0, other_authors(Ctx), #{}, #{}),
    {_, VoteByAuthor0} =
        post_round(
            Ctx,
            1,
            other_authors(Ctx),
            R0ByAuthor#{ Primary => ProposerBlock0 },
            #{}
        ),
    {_, DecisionByAuthor0} = post_round(Ctx, 2, other_authors(Ctx), VoteByAuthor0, #{}),
    _ = wait_for_assignments(Ctx, 0, 0, 1),
    Opts = ctx_get(primary_opts, Ctx, #{}),
    Proc = ctx_get(proc, Ctx),
    {ProposerRound1, VotingRound1, DecisionRound1} = wave_rounds(1, Proc, Opts),
    Proposer1 = proposer_for_round(ctx_get(authors, Ctx, []), ProposerRound1, Proc, Opts),
    ProposerOpts = opts_for(Ctx, Proposer1),
    Msg1 = test_message(ctx_get(proc_id, Ctx), <<"m1">>, ProposerOpts),
    {ok, _} = hb_cache:write(Msg1, ProposerOpts),
    Payloads1 = #{ Proposer1 => Msg1 },
    {_, Round3ByAuthor} =
        post_round(
            Ctx,
            ProposerRound1,
            other_authors(Ctx),
            DecisionByAuthor0,
            Payloads1
        ),
    {_, VoteByAuthor1} = post_round(Ctx, VotingRound1, other_authors(Ctx), Round3ByAuthor, #{}),
    _ = post_round(Ctx, DecisionRound1, other_authors(Ctx), VoteByAuthor1, #{}),
    Assignments = wait_for_assignments(Ctx, 0, 1, 2),
    Sorted = hb_util:to_sorted_list(Assignments, Opts),
    [{Slot0, A0}, {Slot1, A1Assignment} | _] = Sorted,
    ?assertEqual(0, hb_util:int(Slot0)),
    ?assertEqual(1, hb_util:int(Slot1)),
    ?assertEqual(<<"m0">>, dev_mysticeti_test_utils:assignment_body(A0, Opts)),
    ?assertEqual(<<"m1">>, dev_mysticeti_test_utils:assignment_body(A1Assignment, Opts)).

%% @doc Test indirect commit via a later anchor.
mysticeti_indirect_commit_test_() ->
    {timeout, 60, fun mysticeti_indirect_commit/0}.

%% @doc Validate TryIndirectDecide (Algorithm 3) via a later committed anchor.
%% Mysticeti-C Algorithm 3: TryIndirectDecide + CertifiedLink behavior
%% (mysticeti-paper/algorithms/universal_committer.tex).
mysticeti_indirect_commit() ->
    Ctx = test_context(4, #{}),
    {_Msg0, ProposerBlock0} = schedule_body(Ctx, <<"m0">>),
    [A2, A3, A4] = other_authors(Ctx),
    {_, R0ByAuthor} = post_round(Ctx, 0, [A2, A3, A4], #{}, #{}),
    VoteBlocksA2A3 =
        make_round_blocks(
            ctx_get(proc_id, Ctx),
            1,
            [A2, A3],
            R0ByAuthor#{ ctx_get(primary, Ctx) => ProposerBlock0 },
            #{},
            ctx_get(opts_by_author, Ctx, #{})
        ),
    R0ByAuthorNoA1 = hb_maps:remove(ctx_get(primary, Ctx), R0ByAuthor, #{}),
    VoteBlockA4 =
        test_block(
            ctx_get(proc_id, Ctx),
            A4,
            1,
            parents_for_author(R0ByAuthorNoA1, A4, opts_for(Ctx, A4)),
            undefined,
            opts_for(Ctx, A4)
        ),
    VoteBlocks0 = VoteBlocksA2A3 ++ [VoteBlockA4],
    post_blocks(ctx_get(pid, Ctx), VoteBlocks0),
    VoteByAuthor0 = blocks_by_author(VoteBlocks0, ctx_get(primary_opts, Ctx, #{})),
    {_, DecisionByAuthor0} = post_round(Ctx, 2, [A2, A3, A4], VoteByAuthor0, #{}),
    ?assertEqual(0, hb_maps:size(fetch_assignments(Ctx, 0, 0), ctx_get(primary_opts, Ctx, #{}))),
    Opts = ctx_get(primary_opts, Ctx, #{}),
    Proc = ctx_get(proc, Ctx),
    {ProposerRound1, VotingRound1, DecisionRound1} = wave_rounds(1, Proc, Opts),
    Proposer1 = proposer_for_round(ctx_get(authors, Ctx, []), ProposerRound1, Proc, Opts),
    ProposerOpts = opts_for(Ctx, Proposer1),
    Msg1 = test_message(ctx_get(proc_id, Ctx), <<"m1">>, ProposerOpts),
    {ok, _} = hb_cache:write(Msg1, ProposerOpts),
    Payloads1 = #{ Proposer1 => Msg1 },
    {_, Round3ByAuthor} =
        post_round(
            Ctx,
            ProposerRound1,
            [A2, A3, A4],
            DecisionByAuthor0,
            Payloads1
        ),
    {_, VoteByAuthor1} = post_round(Ctx, VotingRound1, [A2, A3, A4], Round3ByAuthor, #{}),
    _ = post_round(Ctx, DecisionRound1, [A2, A3, A4], VoteByAuthor1, #{}),
    Assignments = wait_for_assignments(Ctx, 0, 1, 2),
    Sorted = hb_util:to_sorted_list(Assignments, Opts),
    [{Slot0, A0}, {Slot1, A1Assignment} | _] = Sorted,
    ?assertEqual(0, hb_util:int(Slot0)),
    ?assertEqual(1, hb_util:int(Slot1)),
    ?assertEqual(<<"m0">>, dev_mysticeti_test_utils:assignment_body(A0, Opts)),
    ?assertEqual(<<"m1">>, dev_mysticeti_test_utils:assignment_body(A1Assignment, Opts)).

%% @doc Validate that parent order determines support in votes.
mysticeti_supported_block_dfs_test_() ->
    {timeout, 60, fun mysticeti_supported_block_dfs/0}.

%% @doc Changing parent order flips support and breaks direct commit.
mysticeti_supported_block_dfs() ->
    Ctx = test_context(4, #{}),
    {_Msg0, ProposerBlock0} = schedule_body(Ctx, <<"m0">>),
    Opts = ctx_get(primary_opts, Ctx, #{}),
    ProcID = ctx_get(proc_id, Ctx),
    Primary = ctx_get(primary, Ctx),
    [A2, A3, A4] = other_authors(Ctx),
    Msg0b = test_message(ProcID, <<"m0b">>, Opts),
    {ok, _} = hb_cache:write(Msg0b, Opts),
    EquivBlock = test_block(ProcID, Primary, 0, [], Msg0b, Opts),
    R0Others =
        make_round_blocks(
            ProcID,
            0,
            [A2, A3, A4],
            #{},
            #{},
            ctx_get(opts_by_author, Ctx, #{})
        ),
    post_blocks(ctx_get(pid, Ctx), [EquivBlock | R0Others]),
    R0ByAuthor0 = blocks_by_author(R0Others, Opts),
    R0ByAuthor = hb_maps:put(Primary, ProposerBlock0, R0ByAuthor0, Opts),
    AllRound0 = [ProposerBlock0, hb_maps:get(<<"id">>, EquivBlock, undefined, Opts)
                 | hb_maps:values(R0ByAuthor0, Opts)],
    EquivId = hb_maps:get(<<"id">>, EquivBlock, undefined, Opts),
    A2Prev = hb_maps:get(A2, R0ByAuthor, undefined, Opts),
    A3Prev = hb_maps:get(A3, R0ByAuthor, undefined, Opts),
    A4Prev = hb_maps:get(A4, R0ByAuthor, undefined, Opts),
    VoteA2 =
        test_block(
            ProcID,
            A2,
            1,
            vote_parents(A2Prev, ProposerBlock0, EquivId, AllRound0),
            undefined,
            opts_for(Ctx, A2)
        ),
    VoteA3 =
        test_block(
            ProcID,
            A3,
            1,
            vote_parents(A3Prev, ProposerBlock0, EquivId, AllRound0),
            undefined,
            opts_for(Ctx, A3)
        ),
    VoteA4 =
        test_block(
            ProcID,
            A4,
            1,
            vote_parents(A4Prev, EquivId, ProposerBlock0, AllRound0),
            undefined,
            opts_for(Ctx, A4)
        ),
    VoteBlocks = [VoteA2, VoteA3, VoteA4],
    post_blocks(ctx_get(pid, Ctx), VoteBlocks),
    VoteByAuthor = blocks_by_author(VoteBlocks, Opts),
    _ = post_round(Ctx, 2, [A2, A3, A4], VoteByAuthor, #{}),
    Assignments = fetch_assignments(Ctx, 0, 0),
    ?assertEqual(0, hb_maps:size(Assignments, Opts)).

%% @doc Ensure certificates count distinct authors, not blocks.
mysticeti_certificate_distinct_authors_test_() ->
    {timeout, 60, fun mysticeti_certificate_distinct_authors/0}.

%% @doc Equivocating votes from one author must not form a certificate.
mysticeti_certificate_distinct_authors() ->
    Ctx = test_context(4, #{}),
    {_Msg0, ProposerBlock0} = schedule_body(Ctx, <<"m0">>),
    Opts = ctx_get(primary_opts, Ctx, #{}),
    ProcID = ctx_get(proc_id, Ctx),
    Primary = ctx_get(primary, Ctx),
    [A2, A3, A4] = other_authors(Ctx),
    Msg0b = test_message(ProcID, <<"m0b">>, Opts),
    {ok, _} = hb_cache:write(Msg0b, Opts),
    EquivBlock = test_block(ProcID, Primary, 0, [], Msg0b, Opts),
    R0Others =
        make_round_blocks(
            ProcID,
            0,
            [A2, A3, A4],
            #{},
            #{},
            ctx_get(opts_by_author, Ctx, #{})
        ),
    post_blocks(ctx_get(pid, Ctx), [EquivBlock | R0Others]),
    R0ByAuthor0 = blocks_by_author(R0Others, Opts),
    R0ByAuthor = hb_maps:put(Primary, ProposerBlock0, R0ByAuthor0, Opts),
    AllRound0 = [ProposerBlock0, hb_maps:get(<<"id">>, EquivBlock, undefined, Opts)
                 | hb_maps:values(R0ByAuthor0, Opts)],
    EquivId = hb_maps:get(<<"id">>, EquivBlock, undefined, Opts),
    A2Prev = hb_maps:get(A2, R0ByAuthor, undefined, Opts),
    A3Prev = hb_maps:get(A3, R0ByAuthor, undefined, Opts),
    A4Prev = hb_maps:get(A4, R0ByAuthor, undefined, Opts),
    V1a =
        test_block(
            ProcID,
            A2,
            1,
            vote_parents(A2Prev, ProposerBlock0, EquivId, AllRound0),
            undefined,
            opts_for(Ctx, A2)
        ),
    V1b =
        test_block(
            ProcID,
            A2,
            1,
            vote_parents(A2Prev, ProposerBlock0, EquivId, AllRound0),
            undefined,
            opts_for(Ctx, A2)
        ),
    V1c =
        test_block(
            ProcID,
            A3,
            1,
            vote_parents(A3Prev, ProposerBlock0, EquivId, AllRound0),
            undefined,
            opts_for(Ctx, A3)
        ),
    V1d =
        test_block(
            ProcID,
            A4,
            1,
            vote_parents(A4Prev, EquivId, ProposerBlock0, AllRound0),
            undefined,
            opts_for(Ctx, A4)
        ),
    post_blocks(ctx_get(pid, Ctx), [V1a, V1b, V1c, V1d]),
    V1aId = hb_maps:get(<<"id">>, V1a, undefined, Opts),
    V1bId = hb_maps:get(<<"id">>, V1b, undefined, Opts),
    V1cId = hb_maps:get(<<"id">>, V1c, undefined, Opts),
    V1dId = hb_maps:get(<<"id">>, V1d, undefined, Opts),
    D2A2 =
        test_block(
            ProcID,
            A2,
            2,
            [V1aId, V1bId, V1cId, V1dId],
            undefined,
            opts_for(Ctx, A2)
        ),
    D2A3 =
        test_block(
            ProcID,
            A3,
            2,
            [V1cId, V1aId, V1bId, V1dId],
            undefined,
            opts_for(Ctx, A3)
        ),
    D2A4 =
        test_block(
            ProcID,
            A4,
            2,
            [V1dId, V1aId, V1bId, V1cId],
            undefined,
            opts_for(Ctx, A4)
        ),
    post_blocks(ctx_get(pid, Ctx), [D2A2, D2A3, D2A4]),
    Assignments = fetch_assignments(Ctx, 0, 0),
    ?assertEqual(0, hb_maps:size(Assignments, Opts)).

%% @doc Test commit behavior under equivocation.
mysticeti_equivocation_commit_test_() ->
    {timeout, 60, fun mysticeti_equivocation_commit/0}.

%% @doc Commit only the proposer block that gathers certificates.
%% Mysticeti-C Algorithm 1: SupportedProposer + IsCert under equivocation.
mysticeti_equivocation_commit() ->
    Ctx = test_context(4, #{}),
    {_Msg0, ProposerBlock0} = schedule_body(Ctx, <<"m0">>),
    Opts = ctx_get(primary_opts, Ctx, #{}),
    Msg0b = test_message(ctx_get(proc_id, Ctx), <<"m0b">>, Opts),
    {ok, _} = hb_cache:write(Msg0b, Opts),
    EquivBlock =
        test_block(
            ctx_get(proc_id, Ctx),
            ctx_get(primary, Ctx),
            0,
            [],
            Msg0b,
            Opts
        ),
    R0Others =
        make_round_blocks(
            ctx_get(proc_id, Ctx),
            0,
            other_authors(Ctx),
            #{},
            #{},
            ctx_get(opts_by_author, Ctx, #{})
        ),
    post_blocks(ctx_get(pid, Ctx), [EquivBlock | R0Others]),
    R0ByAuthor0 = blocks_by_author(R0Others, Opts),
    R0ByAuthor = hb_maps:put(ctx_get(primary, Ctx), ProposerBlock0, R0ByAuthor0, Opts),
    {_, VoteByAuthor} = post_round(Ctx, 1, other_authors(Ctx), R0ByAuthor, #{}),
    _ = post_round(Ctx, 2, other_authors(Ctx), VoteByAuthor, #{}),
    Assignments = wait_for_assignments(Ctx, 0, 0, 1),
    Bodies =
        [dev_mysticeti_test_utils:assignment_body(A, Opts)
         || {_Slot, A} <- hb_util:to_sorted_list(Assignments, Opts)],
    ?assertEqual([<<"m0">>], Bodies).

%% @doc Test skipping a round in the author's parent chain.
mysticeti_prev_block_skip_round_test_() ->
    {timeout, 60, fun mysticeti_prev_block_skip_round/0}.

%% @doc Allow first parent to be the author's previous block even if it is not
%% from the previous round (block correctness).
mysticeti_prev_block_skip_round() ->
    Ctx = test_context(4, #{}),
    {_, R0ByAuthor} = post_round(Ctx, 0, ctx_get(authors, Ctx, []), #{}, #{}),
    {_, R1ByAuthor} = post_round(Ctx, 1, other_authors(Ctx), R0ByAuthor, #{}),
    A1Prev =
        hb_maps:get(
            ctx_get(primary, Ctx),
            R0ByAuthor,
            undefined,
            ctx_get(primary_opts, Ctx, #{})
        ),
    Parents =
        [A1Prev
         | [Id || {_A, Id} <- hb_maps:to_list(R1ByAuthor, ctx_get(primary_opts, Ctx, #{}))]],
    A1Round2 =
        test_block(
            ctx_get(proc_id, Ctx),
            ctx_get(primary, Ctx),
            2,
            Parents,
            undefined,
            ctx_get(primary_opts, Ctx, #{})
        ),
    post_blocks(ctx_get(pid, Ctx), [A1Round2]),
    Info = dev_mysticeti_server:info(ctx_get(pid, Ctx)),
    ?assert(hb_maps:get(max_round, Info, -1, #{}) >= 2).

%% @doc Test commit with multiple proposers per round.
mysticeti_multi_proposer_commit_test_() ->
    {timeout, 60, fun mysticeti_multi_proposer_commit/0}.

%% @doc Validate multi-proposer slots per round (Algorithm 3 l-loop).
%% Mysticeti-C Algorithm 3: TryDecide over numOfProposers
%% (mysticeti-paper/algorithms/universal_committer.tex).
mysticeti_multi_proposer_commit() ->
    Ctx = test_context(4, #{ <<"num-proposers">> => 2 }),
    {_Msg0, ProposerBlock0} = schedule_body(Ctx, <<"m0">>),
    [A2, A3, A4] = other_authors(Ctx),
    Msg1 = test_message(ctx_get(proc_id, Ctx), <<"m1">>, opts_for(Ctx, A2)),
    {ok, _} = hb_cache:write(Msg1, opts_for(Ctx, A2)),
    ProposerBlock1 =
        test_block(ctx_get(proc_id, Ctx), A2, 0, [], Msg1, opts_for(Ctx, A2)),
    R0Blocks =
        make_round_blocks(
            ctx_get(proc_id, Ctx),
            0,
            [A3, A4],
            #{},
            #{},
            ctx_get(opts_by_author, Ctx, #{})
        ),
    post_blocks(ctx_get(pid, Ctx), [ProposerBlock1 | R0Blocks]),
    R0ByAuthor0 =
        blocks_by_author([ProposerBlock1 | R0Blocks], ctx_get(primary_opts, Ctx, #{})),
    R0ByAuthor =
        hb_maps:put(
            ctx_get(primary, Ctx),
            ProposerBlock0,
            R0ByAuthor0,
            ctx_get(primary_opts, Ctx, #{})
        ),
    {_, VoteByAuthor} = post_round(Ctx, 1, [A2, A3, A4], R0ByAuthor, #{}),
    _ = post_round(Ctx, 2, [A2, A3, A4], VoteByAuthor, #{}),
    Assignments = wait_for_assignments(Ctx, 0, 1, 2),
    Opts = ctx_get(primary_opts, Ctx, #{}),
    Sorted = hb_util:to_sorted_list(Assignments, Opts),
    [{Slot0, A0}, {Slot1, A1Assignment} | _] = Sorted,
    ?assertEqual(0, hb_util:int(Slot0)),
    ?assertEqual(1, hb_util:int(Slot1)),
    ?assertEqual(<<"m0">>, dev_mysticeti_test_utils:assignment_body(A0, Opts)),
    ?assertEqual(<<"m1">>, dev_mysticeti_test_utils:assignment_body(A1Assignment, Opts)).

%% @doc Test rejection of invalidly signed blocks.
mysticeti_invalid_signature_rejected_test_() ->
    {timeout, 60, fun mysticeti_invalid_signature_rejected/0}.

%% @doc Reject blocks with invalid signatures (block correctness).
%% Mysticeti-C Block correctness: signature verification
%% (mysticeti-paper/sections/overview.tex).
mysticeti_invalid_signature_rejected() ->
    Ctx = test_context(4, #{}),
    {_Msg0, ProposerBlock0} = schedule_body(Ctx, <<"m0">>),
    [A2, A3, A4] = other_authors(Ctx),
    {_, R0ByAuthor} = post_round(Ctx, 0, [A2, A3, A4], #{}, #{}),
    R0ByAuthor1 = R0ByAuthor#{ ctx_get(primary, Ctx) => ProposerBlock0 },
    {VoteBlocksValid, _} = post_round(Ctx, 1, [A3, A4], R0ByAuthor1, #{}),
    InvalidVote =
        test_block(
            ctx_get(proc_id, Ctx),
            A2,
            1,
            parents_for_author(R0ByAuthor1, A2, opts_for(Ctx, A2)),
            undefined,
            opts_for(Ctx, A3)
        ),
    post_blocks(ctx_get(pid, Ctx), [InvalidVote]),
    VoteByAuthor0 = blocks_by_author(VoteBlocksValid, ctx_get(primary_opts, Ctx, #{})),
    InvalidVoteId =
        hb_maps:get(<<"id">>, InvalidVote, undefined, ctx_get(primary_opts, Ctx, #{})),
    VoteByAuthor =
        hb_maps:put(A2, InvalidVoteId, VoteByAuthor0, ctx_get(primary_opts, Ctx, #{})),
    _ = post_round(Ctx, 2, [A2, A3, A4], VoteByAuthor, #{}),
    ?assertEqual(0, hb_maps:size(fetch_assignments(Ctx, 0, 0), ctx_get(primary_opts, Ctx, #{}))).
