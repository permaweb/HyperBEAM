%%% @doc Test vectors for checkpoint bootstrap and real-time Arweave sync.
-module(dev_arweave_sync_test_vectors).
-export([
    ensure_fixtures/0,
    live_account_transition/0,
    live_block_index/0,
    live_bootstrap/0,
    live_peers/0,
    live_sync/0
]).
-include("include/hb.hrl").
-include("include/ar_consensus.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(MAX_WALLET_LIST_PAGES, 1000).
-define(TIP_PATH, <<"~arweave@2.9/tip">>).
-define(ANCHOR_PATH, <<"~arweave@2.9/accounts-anchor">>).

%%% Tests.

%% @doc A real-shaped block hash for a readable test name.
%%
%% A request-supplied hash must be a real block hash before it can name a store
%% path, so the synthetic chain is keyed by hashes rather than by names like
%% `main-50'. Hashing the name keeps the tests readable while giving every
%% block an identifier of the shape production uses.
test_hash(Name) ->
    hb_util:encode(crypto:hash(sha384, Name)).

%% @doc A chain state as `bootstrap' and `sync' write them, reduced to the
%% fields fork choice reads. Written and indexed exactly as the real ones are,
%% so the tests exercise the real lookups.
test_state(Hash, Height, CDiff, Parent, Opts) ->
    {ok, _} =
        dev_arweave_sync:record_state(
            #{
                <<"block">> =>
                    #{
                        <<"indep-hash">> => test_hash(Hash),
                        <<"height">> => Height,
                        <<"cumulative-diff">> => CDiff,
                        <<"previous-block">> => Parent
                    }
            },
            test_hash(Hash),
            Opts
        ),
    test_hash(Hash).

%% @doc Write a run of blocks, each extending the last, and return the head.
test_branch(Prefix, Parent, From, To, CDiff, Step, Opts) ->
    lists:foldl(
        fun(Height, Previous) ->
            test_state(
                << Prefix/binary, (hb_util:bin(Height))/binary >>,
                Height,
                CDiff + ((Height - From) * Step),
                Previous,
                Opts
            )
        end,
        Parent,
        lists:seq(From, To)
    ).

%% @doc A chain of 100 blocks with the tip pointer set. Block 100's
%% `cumulative-diff' is 1990.
test_chain() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    Tip =
        test_branch(
            <<"main-">>,
            test_hash(<<"genesis">>),
            1,
            100,
            1000,
            10,
            Opts
        ),
    hb_cache:link(dev_arweave_sync:state_path(Tip), ?TIP_PATH, Opts),
    {Opts, Tip}.

%% @doc The height at which a branch is exactly as deep as fork choice can
%% still act on, and one deeper than that. The two are adjacent, so the pair of
%% tests below pins the boundary itself rather than merely sampling either side
%% of it. Upstream's `ar_block_cache:get_checkpoint_height/1' is
%% `TipHeight - ?CHECKPOINT_DEPTH + 1' and `is_valid_fork/4' refuses anything
%% below it, so the shallowest refused height is `TipHeight - ?CHECKPOINT_DEPTH'
%% -- one block above where a naive reading of the constant would put it.
test_eligible_fork_height() -> 100 - ?CHECKPOINT_DEPTH + 1.
test_ineligible_fork_height() -> 100 - ?CHECKPOINT_DEPTH.

%% @doc A branch that is strictly heavier and forks inside the window takes the
%% tip, and `tip' resolved as a device key reports the chain state it produced.
fork_choice_prefers_heavier_branch_test() ->
    {Opts, Tip} = test_chain(),
    Fork =
        test_branch(
            <<"fork-">>,
            test_hash(<<"main-90">>),
            91,
            100,
            1901,
            10,
            Opts
        ),
    ?assert(dev_arweave_sync:cumulative_diff(Fork, Opts) > dev_arweave_sync:cumulative_diff(Tip, Opts)),
    {ok, State} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{ <<"path">> => <<"tip">>, <<"candidates">> => [Fork] },
            Opts
        ),
    ?assertEqual(
        Fork,
        hb_maps:get(
            <<"indep-hash">>,
            hb_maps:get(<<"block">>, State, #{}, Opts),
            not_found,
            Opts
        )
    ).

%% @doc The mutation partner of the test above: the same branch, forking at the
%% same height, with a `cumulative-diff' equal to the incumbent's rather than
%% greater, does not take the tip. Equal keeps the incumbent -- first-seen --
%% and there is no secondary tie-break to rescue it.
fork_choice_keeps_incumbent_on_equal_diff_test() ->
    {Opts, Tip} = test_chain(),
    Fork =
        test_branch(
            <<"fork-">>,
            test_hash(<<"main-90">>),
            91,
            100,
            1900,
            10,
            Opts
        ),
    ?assertEqual(90, dev_arweave_sync:branch_point(Fork, Tip, 100, Opts)),
    ?assertEqual(dev_arweave_sync:cumulative_diff(Tip, Opts), dev_arweave_sync:cumulative_diff(Fork, Opts)),
    ?assertEqual(Tip, dev_arweave_sync:choose(Tip, [Fork], Opts)).

%% @doc A branch far heavier than the incumbent still loses when it forks
%% deeper than `?CHECKPOINT_DEPTH' below the tip. This is the only finality
%% rule there is. The branch really does fork one block below the boundary --
%% its lowest block names the incumbent block there as its parent -- and fork
%% choice still declines to find, or act on, a branch point that deep.
fork_choice_rejects_branch_below_checkpoint_depth_test() ->
    {Opts, Tip} = test_chain(),
    Height = test_ineligible_fork_height(),
    Parent = test_hash(<< "main-", (hb_util:bin(Height))/binary >>),
    Fork = test_branch(<<"deep-">>, Parent, Height + 1, 100, 5000, 10, Opts),
    ?assertEqual(
        Parent,
        dev_arweave_sync:parent(test_hash(<< "deep-", (hb_util:bin(Height + 1))/binary >>), Opts)
    ),
    ?assert(dev_arweave_sync:cumulative_diff(Fork, Opts) > dev_arweave_sync:cumulative_diff(Tip, Opts)),
    ?assertEqual([], dev_arweave_sync:branch_point(Fork, Tip, 100, Opts)),
    ?assertEqual(Tip, dev_arweave_sync:choose(Tip, [Fork], Opts)).

%% @doc `validated' serves the states this node produced, and nothing else.
%%
%% The last assertion is the one that gives the key meaning.
%% `~arweave@2.9/block' answers a cache miss by fetching from a gateway, so
%% "I got a block back" says
%% nothing about who checked it. This key has no fallback, and the test pins
%% that absence: a hash the node never validated must answer `not-validated'
%% rather than any block at all. The peer list is empty here, so a fallback
%% could not even succeed -- it would fail differently, and asserting the
%% specific message is what tells the two apart.
validated_serves_only_verified_blocks_test() ->
    {Opts, Tip} = test_chain(),
    Base = #{ <<"device">> => <<"arweave@2.9">> },
    Resolve =
        fun(Block) ->
            hb_ao:resolve(
                Base,
                #{ <<"path">> => <<"validated">>, <<"block">> => Block },
                Opts
            )
        end,
    Field =
        fun(Key, State) ->
            hb_maps:get(
                Key,
                hb_maps:get(<<"block">>, State, #{}, Opts),
                not_found,
                Opts
            )
        end,
    % A block this node validated resolves by its hash.
    {ok, State} = Resolve(test_hash(<<"main-50">>)),
    ?assertEqual(50, hb_util:int(Field(<<"height">>, State))),
    ?assertEqual(test_hash(<<"main-50">>), Field(<<"indep-hash">>, State)),
    % `current' agrees with `tip'.
    {ok, Current} = Resolve(<<"current">>),
    {ok, FromTip} = hb_ao:resolve(Base, #{ <<"path">> => <<"tip">> }, Opts),
    ?assertEqual(
        Field(<<"indep-hash">>, FromTip),
        Field(<<"indep-hash">>, Current)
    ),
    ?assertEqual(Tip, Field(<<"indep-hash">>, Current)),
    % A block this node never validated is refused, not fetched.
    ?assertEqual(
        <<"not-validated">>,
        case Resolve(test_hash(<<"never-seen-this-block">>)) of
            {error, Error} -> hb_maps:get(<<"message">>, Error, none, Opts);
            Other -> Other
        end
    ).

%% @doc The eligibility rule itself, asserted on the arithmetic rather than
%% through a chain. `eligible/2' is reached only with a branch point
%% `branch_point/4' was willing to return, and `branch_point/4' stops searching
%% at the very height `eligible/2' refuses -- so a test that goes through a
%% chain cannot tell the two apart, and neither would notice the rule moving by
%% a block. Both read `eligible_height/1', and this is what pins it.
fork_choice_eligibility_boundary_test() ->
    ?assertEqual(100 - ?CHECKPOINT_DEPTH + 1, dev_arweave_sync:eligible_height(100)),
    ?assert(dev_arweave_sync:eligible(100 - ?CHECKPOINT_DEPTH + 1, 100)),
    ?assertNot(dev_arweave_sync:eligible(100 - ?CHECKPOINT_DEPTH, 100)),
    ?assertNot(dev_arweave_sync:eligible([], 100)).

%% @doc The mutation partner: the same heavier branch forking one block
%% shallower -- at `eligible_height/1' exactly -- is eligible and wins.
%% Together with the test above this pins the boundary rather than merely
%% asserting that deep branches lose.
fork_choice_accepts_branch_at_checkpoint_depth_test() ->
    {Opts, Tip} = test_chain(),
    Height = test_eligible_fork_height(),
    Fork =
        test_branch(
            <<"edge-">>,
            test_hash(<< "main-", (hb_util:bin(Height))/binary >>),
            Height + 1,
            100,
            5000,
            10,
            Opts
        ),
    ?assertEqual(Height, dev_arweave_sync:branch_point(Fork, Tip, 100, Opts)),
    ?assertEqual(Fork, dev_arweave_sync:choose(Tip, [Fork], Opts)).

%% @doc A candidate that has run ahead of the tip is walked down to the tip's
%% height before the two are stepped together, so a node catching up still
%% finds its branch point. A step budget sized for the reorg window alone would
%% run out during the descent and reject every candidate a sync produces.
fork_choice_finds_branch_point_above_the_tip_test() ->
    {Opts, Tip} = test_chain(),
    Ahead = test_branch(<<"ahead-">>, Tip, 101, 150, 2000, 10, Opts),
    ?assertEqual(100, dev_arweave_sync:branch_point(Ahead, Tip, 100, Opts)),
    ?assertEqual(Ahead, dev_arweave_sync:choose(Tip, [Ahead], Opts)).

%% @doc Fork choice with no candidates is the identity on the stored tip, which
%% is what makes `tip' safe to resolve at any time.
fork_choice_without_candidates_keeps_tip_test() ->
    {Opts, Tip} = test_chain(),
    ?assertEqual(Tip, dev_arweave_sync:choose(Tip, [], Opts)).

%% @doc A candidate this node has no chain state for has no branch point, and
%% so cannot win however heavy it claims to be.
fork_choice_ignores_unknown_candidate_test() ->
    {Opts, Tip} = test_chain(),
    ?assertEqual([], dev_arweave_sync:branch_point(<<"unknown">>, Tip, 100, Opts)),
    ?assertEqual(Tip, dev_arweave_sync:choose(Tip, [<<"unknown">>], Opts)).

%% @doc A block takes the tip as soon as it is applied, not at the end of the
%% pass that applied it. This is what keeps a node closing a thirty-block gap
%% from being indistinguishable from a stalled one for twenty-five minutes.
adopt_moves_the_tip_per_block_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    _Chain =
        test_branch(
            <<"main-">>,
            test_hash(<<"genesis">>),
            1,
            100,
            1000,
            10,
            Opts
        ),
    hb_cache:link(dev_arweave_sync:state_path(test_hash(<<"main-99">>)), ?TIP_PATH, Opts),
    ?assertEqual({ok, test_hash(<<"main-99">>)}, dev_arweave_sync:incumbent(Opts)),
    ?assertEqual(ok, dev_arweave_sync:adopt(test_hash(<<"main-100">>), Opts)),
    ?assertEqual({ok, test_hash(<<"main-100">>)}, dev_arweave_sync:incumbent(Opts)).

%% @doc Adoption is still fork choice: a block does not take the tip merely by
%% having been applied. Its branch has to be heavier.
adopt_refuses_a_lighter_block_test() ->
    {Opts, Tip} = test_chain(),
    Lighter =
        test_branch(
            <<"light-">>,
            test_hash(<<"main-99">>),
            100,
            100,
            1000,
            0,
            Opts
        ),
    ?assert(dev_arweave_sync:cumulative_diff(Lighter, Opts) < dev_arweave_sync:cumulative_diff(Tip, Opts)),
    ?assertEqual(ok, dev_arweave_sync:adopt(Lighter, Opts)),
    ?assertEqual({ok, Tip}, dev_arweave_sync:incumbent(Opts)).

%% @doc The branch point between two branches is the last block they shared.
branch_point_is_the_last_shared_block_test() ->
    {Opts, Tip} = test_chain(),
    Fork =
        test_branch(
            <<"fork-">>,
            test_hash(<<"main-95">>),
            96,
            100,
            2000,
            10,
            Opts
        ),
    ?assertEqual(95, dev_arweave_sync:branch_point(Fork, Tip, 100, Opts)).

%% @doc The paging plan covers `[0, Height]' exactly: contiguous, ordered, no
%% gap and no overlap. A hole here would be a hole in the weave.
block_index_ranges_are_contiguous_test() ->
    Ranges = dev_arweave_sync:ranges(0, 12345, 5000),
    ?assertEqual([{0, 4999}, {5000, 9999}, {10000, 12345}], Ranges),
    ?assertEqual(
        12346,
        lists:sum([ (End - Start) + 1 || {Start, End} <- Ranges ])
    ).

%% @doc A range shorter than a page is one page, and an empty range is none.
block_index_ranges_edge_cases_test() ->
    ?assertEqual([{0, 0}], dev_arweave_sync:ranges(0, 0, 5000)),
    ?assertEqual([], dev_arweave_sync:ranges(1, 0, 5000)).

%% @doc Rotation spreads the first attempt without dropping any peer, so a page
%% whose preferred peer is down still falls back through all the others.
peer_rotation_preserves_every_peer_test() ->
    Peers = [<<"a">>, <<"b">>, <<"c">>],
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>], dev_arweave_sync:rotate(Peers, 0)),
    ?assertEqual([<<"b">>, <<"c">>, <<"a">>], dev_arweave_sync:rotate(Peers, 1)),
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>], dev_arweave_sync:rotate(Peers, 3)),
    ?assertEqual([], dev_arweave_sync:rotate([], 2)),
    lists:foreach(
        fun(N) ->
            ?assertEqual(lists:sort(Peers), lists:sort(dev_arweave_sync:rotate(Peers, N)))
        end,
        lists:seq(0, 7)
    ).

%% @doc The reward history decoder, on the shape of a real response. The
%% address is fixed-width and the two amounts are length-prefixed, so a misread
%% of either would shift every later entry -- which is why a body that is not a
%% whole number of entries is an error rather than a truncation.
reward_history_decodes_test() ->
    Addr = crypto:strong_rand_bytes(32),
    ?assertEqual(
        {ok, [{Addr, 159839725, 355263029606, 1}, {Addr, 7, 9, 1}]},
        dev_arweave_sync:decode_rewards(
            <<
                Addr/binary, 4:8, 159839725:32, 5:8, 355263029606:40, 1:24,
                Addr/binary, 1:8, 7:8, 1:8, 9:8, 1:24
            >>,
            []
        )
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-reward-history">> }},
        dev_arweave_sync:decode_rewards(<<0, 1, 2>>, [])
    ).

%% @doc The block-time history decoder, on the first three entries of a real
%% mainnet response.
block_time_history_decodes_test() ->
    ?assertEqual(
        {ok, [{123, 112, 2}, {87, 86, 2}, {116, 77, 2}]},
        dev_arweave_sync:decode_times(
            <<
                1:8, 123:8, 1:8, 112:8, 1:8, 2:8,
                1:8, 87:8, 1:8, 86:8, 1:8, 2:8,
                1:8, 116:8, 1:8, 77:8, 1:8, 2:8
            >>,
            []
        )
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-block-time-history">> }},
        dev_arweave_sync:decode_times(<<1, 1, 1>>, [])
    ).

%% @doc A `sync' pass over a range this node has already applied fetches no
%% block and moves no pointer, and a second pass returns exactly what the first
%% did. This is the idempotence property stated directly: the mock peer counts
%% the block requests that would prove otherwise.
sync_over_known_range_is_a_no_op_test() ->
    {Opts, Tip} = test_chain(),
    {ok, Peer, Handle} =
        hb_mock_server:start([
            {"/info", info,
                {200,
                    hb_json:encode(
                        #{ <<"height">> => 100, <<"current">> => Tip }
                    )
                }
            },
            {"/block2/hash/[...]", block, {404, <<>>}}
        ]),
    try
        PeerOpts = Opts#{ <<"arweave-untrusted-peers">> => [Peer] },
        First = dev_arweave_sync:do_sync(#{}, PeerOpts),
        Second = dev_arweave_sync:do_sync(#{}, PeerOpts),
        ?assertMatch(
            {ok, #{ <<"applied">> := 0, <<"indep-hash">> := Tip }},
            First
        ),
        ?assertEqual(First, Second),
        ?assertEqual([], hb_mock_server:get_requests(Handle, block))
    after
        hb_mock_server:stop(Handle)
    end.

%% @doc A node that has not bootstrapped has nothing to extend, and says so
%% rather than inventing a tip.
sync_without_bootstrap_is_an_error_test() ->
    ?assertMatch(
        {error, #{ <<"message">> := <<"not-bootstrapped">> }},
        dev_arweave_sync:do_sync(
            #{},
            #{
                <<"store">> => [hb_test_utils:test_store()],
                <<"arweave-untrusted-peers">> => [<<"http://localhost:1">>]
            }
        )
    ).

%% @doc A node with no peers configured is told so, rather than reaching for a
%% hardcoded host.
peers_default_to_none_test() ->
    ?assertMatch({error, #{ <<"message">> := <<"no-peers">> }}, dev_arweave_sync:peers(#{})).

%% @doc A checkpoint below the 2.9 fork is refused: its blocks carry proofs
%% this subsystem does not validate.
checkpoint_below_fork_is_refused_test() ->
    ?assertEqual(ok, dev_arweave_sync:above_fork(ar_fork:height_2_9())),
    ?assertMatch(
        {error, #{ <<"message">> := <<"checkpoint-below-fork">> }},
        dev_arweave_sync:above_fork(ar_fork:height_2_9() - 1)
    ).

%%% Live probes. These talk to mainnet, so they are not part of the suite and
%%% carry no `_test' suffix; run one by name with `rebar3 device test --devices
%%% dev_arweave --test all:<name>'. They exist because everything above this
%%% line is either pure or mocked, and a peer protocol is only ever really
%%% tested against a peer.

%% @doc The four mainnet peers these probes read from.
mainnet_peers() ->
    [
        <<"http://tip-1.arweave.xyz:1984">>,
        <<"http://tip-2.arweave.xyz:1984">>,
        <<"http://tip-3.arweave.xyz:1984">>,
        <<"http://tip-4.arweave.xyz:1984">>
    ].

%% @doc The probes share an ignored on-disk store so repeated runs reuse the
%% hydrated account tree and exercise restart/resume behavior.
mainnet_opts() ->
    #{
        <<"store">> =>
            [
                #{
                    <<"store-module">> => hb_store_fs,
                    <<"name">> => <<"_build/arweave-test-vectors">>
                }
            ],
        <<"arweave-untrusted-peers">> => mainnet_peers(),
        % A shared-ancestor bootstrap needs an explicit trusted list; these
        % probes name the same peers for both, as an operator with no
        % checkpoint hash would.
        <<"arweave-trusted-peers">> => mainnet_peers()
    }.

%% @doc Bootstrap only if this store has no tip yet.
ensure_bootstrapped(Opts) ->
    case dev_arweave_sync:tip(#{}, #{}, Opts) of
        {ok, State} ->
            ?debugFmt(
                "reusing chain state at height ~p",
                [
                    dev_arweave_sync:int(
                        <<"height">>,
                        hb_maps:get(<<"block">>, State, #{}, Opts),
                        Opts
                    )
                ]
            ),
            {ok, State};
        _ ->
            timed("bootstrap", fun() -> dev_arweave_sync:do_bootstrap(#{}, Opts) end)
    end.

%% @doc Hydrate the current account trie and consensus state into an ignored,
%% reusable HB store. Public peers prune old wallet lists, so live validation
%% vectors deliberately use a recent checkpoint rather than checked-in data.
ensure_fixtures() ->
    ensure_bootstrapped(mainnet_opts()).

%% @doc Everything `bootstrap' does except the full block index: peer tip
%% discovery, the shared-ancestor walk across all four peers, the checkpoint
%% block and the recomputation of its hash, both carried histories against
%% their committed hashes, and the account-anchor probe.
live_peers() ->
    Opts = mainnet_opts(),
    {ok, Peers} = dev_arweave_sync:peers(Opts),
    {ok, TipHeight} = dev_arweave_sync:network_height(Peers, Opts),
    ?debugFmt("network height: ~p", [TipHeight]),
    {ok, Hash} = dev_arweave_sync:shared_ancestor(Peers, TipHeight - 50, Opts),
    ?debugFmt("shared ancestor at ~p: ~s", [TipHeight - 50, Hash]),
    {ok, Block} = dev_arweave_sync:identified_block(Peers, Hash, Opts),
    ?debugFmt(
        "checkpoint block verified: height ~p, cumulative-diff ~p",
        [
            dev_arweave_sync:int(<<"height">>, Block, Opts),
            hb_maps:get(<<"cumulative-diff">>, Block, none, Opts)
        ]
    ),
    ?assertEqual(TipHeight - 50, dev_arweave_sync:int(<<"height">>, Block, Opts)),
    {ok, Rewards, Times} = dev_arweave_sync:histories(Peers, Block, Hash, Opts),
    {ok, RewardList} = dev_arweave_sync:decode_rewards(Rewards, []),
    {ok, TimeList} = dev_arweave_sync:decode_times(Times, []),
    ?debugFmt(
        "histories verified: ~p reward entries (~p bytes), "
        "~p block-time entries (~p bytes)",
        [
            length(RewardList), byte_size(Rewards),
            length(TimeList), byte_size(Times)
        ]
    ),
    ok = dev_arweave_sync:account_anchor(Peers, Block, Opts),
    {ok, Anchor} = hb_cache:read(?ANCHOR_PATH, Opts),
    ?debugFmt(
        "account anchor at height ~p, ~p blocks below the tip",
        [
            dev_arweave_sync:int(<<"height">>, Anchor, Opts),
            TipHeight - dev_arweave_sync:int(<<"height">>, Anchor, Opts)
        ]
    ),
    ?assert(dev_arweave_sync:int(<<"height">>, Anchor, Opts) >= TipHeight - 100).

%% @doc The full block index from genesis, assembled and proven against the
%% checkpoint block's `hash-list-merkle', with the assembled index spot-checked
%% for ordering: the entry the index reports at a height must be the block the
%% peers report at that height.
live_block_index() ->
    Opts = mainnet_opts(),
    {ok, Peers} = dev_arweave_sync:peers(Opts),
    {ok, TipHeight} = dev_arweave_sync:network_height(Peers, Opts),
    Height = TipHeight - 50,
    {ok, Hash} = dev_arweave_sync:height_hash(Peers, Height, Opts),
    {ok, Block} = dev_arweave_sync:identified_block(Peers, Hash, Opts),
    Started = os:system_time(millisecond),
    {ok, Index} = dev_arweave_sync:block_index(Peers, Block, Height, Opts),
    Length = hb_util:int(hb_maps:get(<<"length">>, Index, 0, Opts)),
    ?debugFmt(
        "block index: ~p entries assembled and verified in ~p ms",
        [Length, os:system_time(millisecond) - Started]
    ),
    ?assertEqual(Height + 1, Length),
    lists:foreach(
        fun(At) ->
            {ok, Expected} = dev_arweave_sync:height_hash(Peers, At, Opts),
            {ok, Entry} =
                hb_ao:resolve(
                    Index,
                    #{ <<"path">> => <<"at">>, <<"height">> => At },
                    Opts
                ),
            ?assertEqual(
                Expected,
                hb_maps:get(<<"indep-hash">>, Entry, none, Opts)
            )
        end,
        [0, 1, ar_fork:height_2_9(), Height - 1, Height]
    ).

%% @doc Bootstrap, then advance the chain by a single block and prove the
%% result: the tip moves on by exactly one, the new state's block names the old
%% tip as its parent, and a second pass over the same range applies nothing.
%% This is the smallest run that exercises `~arweave-block@2.9/apply', which is
%% where the whole cost of a sync lives.
live_sync() ->
    Opts = maps:put(<<"arweave-sync-batch">>, 3, mainnet_opts()),
    {ok, Peers} = dev_arweave_sync:peers(Opts),
    {ok, Before} = ensure_bootstrapped(Opts),
    Block = hb_maps:get(<<"block">>, Before, #{}, Opts),
    Start = dev_arweave_sync:int(<<"height">>, Block, Opts),
    Parent = hb_maps:get(<<"indep-hash">>, Block, none, Opts),
    % Report what the block being applied actually contains, so a run over an
    % empty block is not mistaken for one that exercised the transaction path.
    {ok, NextHash} = dev_arweave_sync:height_hash(Peers, Start + 1, Opts),
    {ok, Next} = dev_arweave_sync:peer_block(Peers, NextHash, Opts),
    ?debugFmt(
        "block ~p carries ~p transactions",
        [
            Start + 1,
            length(
                hb_util:message_to_ordered_list(
                    hb_maps:get(<<"txs">>, Next, [], Opts),
                    Opts
                )
            )
        ]
    ),
    {ok, First} =
        timed(
            "sync",
            fun() ->
                hb_ao:resolve(
                    #{ <<"device">> => <<"arweave@2.9">> },
                    #{ <<"path">> => <<"sync">> },
                    Opts
                )
            end
        ),
    ?debugFmt(
        "sync from ~p: applied ~p, now at height ~p",
        [
            Start,
            hb_maps:get(<<"applied">>, First, none, Opts),
            hb_maps:get(<<"height">>, First, none, Opts)
        ]
    ),
    Applied = hb_util:int(hb_maps:get(<<"applied">>, First, 0, Opts)),
    Tip = hb_maps:get(<<"indep-hash">>, First, none, Opts),
    ?assert(Applied >= 1),
    ?assertEqual(
        Start + Applied,
        hb_util:int(hb_maps:get(<<"height">>, First, 0, Opts))
    ),
    % Walking the new tip back by as many blocks as were applied must land on
    % the block the pass started from. Every state in between therefore exists
    % and names its parent, which is the only evidence that a multi-block pass
    % applied them in ancestor order rather than merely ending in the right
    % place -- a pass that applied them out of order could not have found each
    % block's parent state to apply it onto.
    ?assertEqual(
        Parent,
        lists:foldl(
            fun(_, Hash) -> dev_arweave_sync:parent(Hash, Opts) end,
            Tip,
            lists:seq(1, Applied)
        )
    ),
    % Re-running over the same range must apply nothing and leave the same tip.
    {ok, Second} =
        timed(
            "second sync",
            fun() -> dev_arweave_sync:do_sync(#{}, Opts#{ <<"arweave-sync-batch">> => 0 }) end
        ),
    ?assertEqual(0, hb_util:int(hb_maps:get(<<"applied">>, Second, 1, Opts))),
    ?assertEqual(Tip, hb_maps:get(<<"indep-hash">>, Second, none, Opts)).

%% @doc Apply exactly one block onto the stored tip and report the result
%% verbatim. Separated from `live_sync/0' because `sync' treats a block that
%% fails as that peer's problem and moves on, which is right for a running node
%% and useless for finding out why.
live_account_transition() ->
    Opts = mainnet_opts(),
    {ok, Peers} = dev_arweave_sync:peers(Opts),
    {ok, Tip} = ensure_fixtures(),
    State = previous_state(Tip, Opts),
    apply_transaction_block(Peers, State, Opts, 20).

%% @doc Reuse an already-synced store without waiting for the network to mine
%% another block. A fresh bootstrap has no parent state and simply starts from
%% its checkpoint, which is already below the tip.
previous_state(State, Opts) ->
    Block = hb_maps:get(<<"block">>, State, #{}, Opts),
    Parent = hb_maps:get(<<"previous-block">>, Block, not_found, Opts),
    case hb_cache:read(dev_arweave_sync:state_path(Parent), Opts) of
        {ok, Previous} -> Previous;
        _ -> State
    end.

apply_transaction_block(_Peers, _State, _Opts, 0) ->
    error(no_recent_transaction_block);
apply_transaction_block(Peers, State, Opts, Remaining) ->
    Block = hb_maps:get(<<"block">>, State, #{}, Opts),
    Height = dev_arweave_sync:int(<<"height">>, Block, Opts),
    {ok, NextHash} = dev_arweave_sync:height_hash(Peers, Height + 1, Opts),
    {ok, Next} = dev_arweave_sync:peer_block(Peers, NextHash, Opts),
    {ok, TXs} = dev_arweave_sync:transactions(Peers, Next, Opts),
    ?debugFmt(
        "applying ~p onto ~p, ~p transactions, accounts=~p",
        [
            Height + 1,
            Height,
            length(TXs),
            case hb_maps:get(<<"accounts">>, State, [], Opts) of
                [] -> absent;
                _ -> present
            end
        ]
    ),
    {ok, Applied} =
        timed(
            "apply",
            fun() ->
                hb_ao:resolve(
                    State#{ <<"device">> => <<"arweave-block@2.9">> },
                    #{
                        <<"path">> => <<"apply">>,
                        <<"next">> => Next,
                        <<"transactions">> => TXs
                    },
                    Opts
                )
            end
        ),
    case TXs of
        [] ->
            apply_transaction_block(Peers, Applied, Opts, Remaining - 1);
        _ ->
            ?assertEqual(
                hb_maps:get(<<"indep-hash">>, Block, not_found, Opts),
                hb_maps:get(<<"previous-block">>, Next, not_found, Opts)
            ),
            ?assertEqual(
                true,
                hb_util:atom(
                    hb_maps:get(
                        <<"accounts-checked">>,
                        hb_private:reset(Applied),
                        false,
                        Opts
                    )
                )
            ),
            Accounts =
                hb_cache:ensure_loaded(
                    hb_maps:get(<<"accounts">>, Applied, not_found, Opts),
                    Opts
                ),
            {ok, RootResult} = hb_ao:resolve(Accounts, <<"root">>, Opts),
            ?assertEqual(
                hb_maps:get(<<"wallet-list">>, Next, not_found, Opts),
                hb_maps:get(<<"root">>, RootResult, not_found, Opts)
            )
    end.

%% @doc Run a stage, reporting how long it took as it finishes rather than at
%% the end of the probe, so that a run which never finishes still says how far
%% it got.
timed(Name, Fun) ->
    Started = os:system_time(millisecond),
    Result = Fun(),
    ?debugFmt("~s: ~p ms", [Name, os:system_time(millisecond) - Started]),
    Result.

%% @doc A whole `bootstrap' against mainnet, resolved as a device key, and the
%% tip it leaves behind.
live_bootstrap() ->
    Opts = mainnet_opts(),
    Started = os:system_time(millisecond),
    {ok, State} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{ <<"path">> => <<"bootstrap">> },
            Opts
        ),
    Block = hb_maps:get(<<"block">>, State, #{}, Opts),
    ?debugFmt(
        "bootstrapped to height ~p in ~p ms",
        [dev_arweave_sync:int(<<"height">>, Block, Opts), os:system_time(millisecond) - Started]
    ),
    % The account tree must be attached to the state, not merely fetched. An
    % empty `accounts' link is what disables the account checks, so a bootstrap
    % that verified a tree and then dropped it would leave the node validating
    % everything except the property the tree exists for -- and would do it
    % silently.
    ?assertMatch(
        #{ <<"root">> := _ },
        hb_cache:ensure_loaded(
            hb_maps:get(<<"accounts">>, State, [], Opts),
            Opts
        )
    ),
    {ok, Tip} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{ <<"path">> => <<"tip">> },
            Opts
        ),
    ?assertEqual(
        hb_maps:get(<<"indep-hash">>, Block, none, Opts),
        hb_maps:get(
            <<"indep-hash">>,
            hb_maps:get(<<"block">>, Tip, #{}, Opts),
            none,
            Opts
        )
    ).

%% @doc `bootstrap' is reachable by anyone the node answers, so an unguarded one
%% is a repeatable trust reset: it re-asks the peers where the chain starts and
%% relinks the tip onto their answer, discarding whatever the node had validated
%% for itself. A node that already has a chain is refused whichever side of its
%% tip the new checkpoint falls on -- a checkpoint a few blocks ahead is still
%% blocks taken on trust that the node could have validated -- and only a
%% deliberate `force' overrides that.
bootstrap_refuses_on_an_established_chain_test() ->
    {Opts, _Tip} = test_chain(),
    Stored = dev_arweave_sync:stored_height(Opts),
    ?assertEqual(100, Stored),
    ?assertMatch({error, #{ <<"message">> := <<"already-bootstrapped">> }},
        dev_arweave_sync:unbootstrapped(false, Stored)),
    ?assertEqual(ok, dev_arweave_sync:unbootstrapped(true, Stored)),
    % A node with no chain of its own has nothing to protect, so the first
    % bootstrap is never the one that is refused.
    ?assertEqual(ok, dev_arweave_sync:unbootstrapped(false, [])).

%% @doc A request cannot shape a store path.
%%
%% `validated' and `tip' both turn a caller-supplied value into a path under
%% `~arweave@2.9/state/'. Nothing between here and the filesystem collapses
%% `..' -- `hb_path:to_binary/1' leaves it alone and `hb_store_fs' walks the
%% components -- so before this was guarded, `block=../../../hyperbeam-key.json'
%% read the operator's signing key off disk, unauthenticated, from a device key
%% whose whole purpose is to answer strangers.
%%
%% Asserted at both layers: the request boundary refuses the value, and
%% `state_path/1' refuses to build a path out of one even if a future caller
%% forgets to check.
request_cannot_shape_a_store_path_test() ->
    Traversals =
        [
            <<"../../../hyperbeam-key.json">>,
            % 64 characters, so it decodes to exactly 48 bytes and passes a
            % length check. `hb_util:decode/1' is the unchecked decoder and
            % validates no alphabet, so only checking the decoded size let this
            % through to the chokepoint, where it surfaced as an uncaught throw
            % instead of this device's own `invalid-block'.
            << "../../../hyperbeam-key.json",
                (binary:copy(<<"A">>, 37))/binary >>,
            <<"..">>,
            <<"a/b">>,
            <<"/etc/passwd">>,
            << 0 >>,
            % Well-formed base64url, but not 48 bytes: still not a block hash.
            hb_util:encode(<<"short">>)
        ],
    lists:foreach(
        fun(Bad) ->
            ?assertMatch(
                {error, #{ <<"message">> := <<"invalid-block">> }},
                dev_arweave_sync:validated_hash(#{ <<"block">> => Bad }, #{})
            ),
            ?assertMatch(
                {error, #{ <<"message">> := <<"invalid-block">> }},
                dev_arweave_sync:candidate_hashes([Bad])
            )
        end,
        Traversals
    ),
    % The chokepoint refuses a separator whatever the caller did.
    ?assertThrow({unsafe_state_path, _}, dev_arweave_sync:state_path(<<"../../secret">>)),
    ?assertThrow({unsafe_state_path, _}, dev_arweave_sync:state_path(<<"a/b">>)),
    % A real block hash is accepted at both layers.
    Good = hb_util:encode(crypto:strong_rand_bytes(48)),
    ?assertEqual({ok, Good}, dev_arweave_sync:validated_hash(#{ <<"block">> => Good }, #{})),
    ?assertEqual({ok, [Good]}, dev_arweave_sync:candidate_hashes([Good])),
    ?assertMatch(<<"~arweave@2.9/state/", _/binary>>, dev_arweave_sync:state_path(Good)).

%% @doc `force' is honoured from the node message and ignored from the request.
%%
%% `bootstrap' is reachable by anyone the node answers, and `force' discards a
%% chain this node validated for itself and re-anchors on whatever the peer set
%% says. Reading it from the request made the subsystem's one guarded trust
%% boundary unguarded again for any caller who could reach the port: the
%% guard would refuse the accidental second bootstrap and wave through the
%% deliberate one from a stranger.
%%
%% Pinned on `forced/2' rather than through `bootstrap/3' because the property
%% is about where the value is read, and a test that went through the device
%% would need a peer set to get far enough to observe it.
force_is_not_request_supplied_test() ->
    % A request asking for it, with nothing in the node message: refused.
    ?assertEqual(false, dev_arweave_sync:forced(#{ <<"force">> => true }, #{})),
    ?assertEqual(false, dev_arweave_sync:forced(#{ <<"force">> => <<"true">> }, #{})),
    % The node message asking for it: honoured, whichever way it is written.
    ?assertEqual(
        true,
        dev_arweave_sync:forced(#{}, #{ <<"arweave-force-bootstrap">> => true })
    ),
    ?assertEqual(
        true,
        dev_arweave_sync:forced(#{}, #{ <<"arweave-force-bootstrap">> => <<"true">> })
    ),
    % A request cannot override a node message that says no.
    ?assertEqual(
        false,
        dev_arweave_sync:forced(
            #{ <<"force">> => true },
            #{ <<"arweave-force-bootstrap">> => false }
        )
    ),
    % Absent everywhere: no.
    ?assertEqual(false, dev_arweave_sync:forced(#{}, #{})).

%% @doc A bootstrap that cannot anchor the account tree fails, rather than
%% producing a node that can never sync.
%%
%% This pins a liveness trap. An anchor recorded at checkpoint+k attaches only
%% to the block whose hash matches it, so the checkpoint state would keep
%% `accounts => []'; applying checkpoint+1 then reads a parent with no tree,
%% `~arweave-block@2.9/apply' refuses it with `accounts-not-checked', and the
%% block is skipped -- so the anchor block is never reached and the tree never
%% attaches. Searching upward on a failed anchor would therefore turn one
%% transient peer error during the wallet-list walk into a node that cannot
%% sync at all, recoverable only by setting `arweave-force-bootstrap' and
%% restarting. Failing the bootstrap outright is the safe answer.
anchor_failure_fails_the_bootstrap_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    % A peer that answers nothing stands for every way the walk can fail: a
    % pruned root, a 404 on a page, a timeout mid-walk.
    Peers = [<<"http://127.0.0.1:1">>],
    Block = #{
        <<"height">> => 100,
        <<"indep-hash">> => hb_util:encode(crypto:strong_rand_bytes(48)),
        <<"wallet-list">> => hb_util:encode(crypto:strong_rand_bytes(32))
    },
    ?assertMatch(
        {error, #{ <<"message">> := <<"no-account-anchor">> }},
        dev_arweave_sync:account_anchor(Peers, Block, Opts)
    ).

%% @doc Account bootstrap uses the block already selected and identified as the
%% checkpoint. It must not ask the peer which hash currently occupies the same
%% height, because another peer or fork could replace or veto that trust root.
account_anchor_is_bound_to_selected_checkpoint_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    Accounts =
        [
            {
                crypto:hash(sha256, <<"checkpoint-account">>),
                {1234, crypto:hash(sha256, <<"checkpoint-last-tx">>)}
            }
        ],
    Tree =
        lists:foldl(
            fun({Address, Account}, Acc) ->
                ar_patricia_tree:insert(Address, Account, Acc)
            end,
            ar_patricia_tree:new(),
            Accounts
        ),
    {RawRoot, _Memoised, _Updates} = ar_block:hash_wallet_list(Tree),
    Root = hb_util:encode(RawRoot),
    Hash = hb_util:encode(crypto:hash(sha384, <<"selected-checkpoint">>)),
    WalletPath = "/wallet_list/" ++ binary_to_list(Root),
    {ok, Peer, Handle} =
        hb_mock_server:start(
            [
                {
                    WalletPath,
                    wallet,
                    {
                        200,
                        term_to_binary(
                            #{ next_cursor => last, wallets => Accounts }
                        )
                    }
                },
                {"/block/height/:height", height, {500, <<"must not fetch">>}}
            ]
        ),
    try
        Block =
            #{
                <<"height">> => 100,
                <<"indep-hash">> => Hash,
                <<"wallet-list">> => Root
            },
        ?assertEqual(ok, dev_arweave_sync:account_anchor([Peer], Block, Opts)),
        {ok, Anchor} = hb_cache:read(?ANCHOR_PATH, Opts),
        ?assertEqual(Hash, hb_maps:get(<<"indep-hash">>, Anchor, none, Opts)),
        ?assertEqual([], hb_mock_server:get_requests(Handle, height))
    after
        hb_mock_server:stop(Handle)
    end.

%% @doc Only the trusted peers may decide where a shared-ancestor bootstrap
%% anchors. A node that names a checkpoint block trusts no peer at all, so it
%% needs no trusted list; a node that names neither has nothing to anchor on and
%% is told so rather than quietly asking whoever a previous bootstrap found.
trusted_peers_test() ->
    Trusted = [<<"http://trusted">>],
    Untrusted = [<<"http://untrusted">>],
    Both = #{
        <<"arweave-trusted-peers">> => Trusted,
        <<"arweave-untrusted-peers">> => Untrusted
    },
    Neither = #{ <<"arweave-untrusted-peers">> => Untrusted },
    ?assertEqual({ok, Trusted}, dev_arweave_sync:trusted_peers(#{}, Both)),
    ?assertMatch({error, #{ <<"message">> := <<"no-trusted-peers">> }},
        dev_arweave_sync:trusted_peers(#{}, Neither)),
    % A node that names a checkpoint in its **node message** needs no trusted
    % list: the hash is the trust root and no peer is asked where the chain
    % starts.
    ?assertEqual(
        {ok, Untrusted},
        dev_arweave_sync:trusted_peers(
            #{},
            Neither#{ <<"arweave-checkpoint-block">> => <<"anything">> }
        )
    ),
    % The same value in the *request* is ignored. `bootstrap' is
    % unauthenticated, so honouring a request-supplied checkpoint would let any
    % caller who can reach the port choose the single hash the entire chain is
    % anchored to.
    ?assertMatch({error, #{ <<"message">> := <<"no-trusted-peers">> }},
        dev_arweave_sync:trusted_peers(#{ <<"checkpoint-block">> => <<"anything">> }, Neither)).

%% @doc The wallet-list walk cannot be made to run for ever.
%%
%% The peer chooses the cursor, so only a bound here ends the walk when the peer
%% will not. This pins the terminating clause and the message it answers with;
%% pinning the decrement end to end would need a peer that serves
%% `/wallet_list' pages, and there is no stub for one.
wallet_list_is_bounded_test() ->
    ?assert(?MAX_WALLET_LIST_PAGES > 125),
    ?assertMatch(
        {error, #{ <<"message">> := <<"wallet-list-too-long">> }},
        dev_arweave_sync:wallet_list([], <<"root">>, <<"cursor">>, #{}, 0, #{})
    ),
    % A peer that has finished is answered before the bound is consulted, so an
    % honest short tree is not refused by an exhausted counter.
    ?assertEqual(
        {ok, #{}},
        dev_arweave_sync:wallet_list([], <<"root">>, <<"last">>, #{}, 0, #{})
    ).

%% @doc `block_hash/1' is exported so that `dev_arweave' can refuse on `block'
%% what this module refuses on `validated'. Both keys reach `hb_cache:read/2'
%% with a caller-supplied string, and a length test alone lets a traversal
%% through: 64 characters of `..' and `/' are 64 bytes.
block_hash_is_the_shared_definition_test() ->
    ?assertMatch({ok, _}, dev_arweave_sync:block_hash(binary:copy(<<"a">>, 64))),
    ?assertMatch({error, _}, dev_arweave_sync:block_hash(binary:copy(<<"a">>, 63))),
    ?assertMatch({error, _}, dev_arweave_sync:block_hash(<<"a/../b">>)),
    ?assertMatch({error, _}, dev_arweave_sync:block_hash(not_a_binary)),
    % 64 bytes, so the length gate admits it; no character is in the base64URL
    % alphabet, so the shared definition does not.
    Traversal = << (binary:copy(<<"../">>, 21))/binary, "x" >>,
    ?assertEqual(64, byte_size(Traversal)),
    ?assertMatch({error, #{ <<"message">> := <<"invalid-block">> }},
        dev_arweave_sync:block_hash(Traversal)).
