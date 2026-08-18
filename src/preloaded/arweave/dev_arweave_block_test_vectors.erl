%%% @doc Deterministic admission vectors for full Arweave block validation.
%%%
%%% A live, full account transition is provided by
%%% `dev_arweave_sync_test_vectors:live_account_transition/0'. It hydrates a
%%% recent checkpoint because public peers prune historical wallet lists.
-module(dev_arweave_block_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Mainnet blocks 1132210-1132214, the first five above the 2.6 fork, as
%%% `{Height, RewardAddr, Reward, Denomination, RewardHistoryHash}'. Below the
%%% 2.8 fork the committed hash covers the whole locked window, which at these
%%% heights is the whole history, so these are the only blocks on the chain
%%% whose `reward-history-hash' proves a representation of the history rather
%%% than of its newest element.
-define(REWARD_HISTORY_VECTOR, [
    {1132210, <<"SOtIrcaiJwVs4h3yqXKjOs0P9bNJG5F77y0-TilnQ2U">>,
        1465707980036, 1, <<"I190QrZCm05s9aIbt9j6bjFJn1BkNvZVMPEXJ7QFTtI">>},
    {1132211, <<"RTot9yL_q-RIsObxmHsZ4ApyVyADn5VxPJC7KefjNHM">>,
        1464501084774, 1, <<"NjA1DoZCYBzrowI9okx_yxt7tXFWD6XpVAtf2VjO81E">>},
    {1132212, <<"UW8wkuhbnjYkT1fqan6sGNiWT_kYULCdr7hqUhEWcGg">>,
        1466463708872, 1, <<"5gBXh5FEEj82filFyfv41h2LLeNo_b7YXfHj1kLdTvs">>},
    {1132213, <<"RTot9yL_q-RIsObxmHsZ4ApyVyADn5VxPJC7KefjNHM">>,
        1464450490014, 1, <<"1Y_BTWQEVZy-QmdrPIYk-9BhPhEYU6xJdzeWw5RUFvo">>},
    {1132214, <<"I03LhhAocpOu4FpH7f0mJ55CfK3cSnu1JJZtITQ4sA8">>,
        1464446627463, 1, <<"bpTKBba9IyNy-vAtj_gFoZRvjlop1h9XwkV44rY8pqQ">>}
]).

%%% The difficulty all five of those blocks were mined at, which is what
%%% `ar_difficulty:get_hash_rate_fixed_ratio/1' derives the hash rate their
%%% reward-history element carries from.
-define(REWARD_HISTORY_VECTOR_DIFF,
    115792084401224566807772651152730092358388734887252574358647557874619658061534
).

%%% Mainnet blocks 1275479-1275483: the 2.7 fork block's parent and the first
%%% four blocks the block-time history covers, as
%%% `{Height, Timestamp, GlobalStepNumber, BlockTimeHistoryHash}'. The parent
%%% carries no hash -- it is below the fork and contributes no element -- but
%%% its timestamp and step number are what the first element is measured
%%% against.
-define(BLOCK_TIME_HISTORY_VECTOR, [
    {1275479, 1696513946, 20992452, none},
    {1275480, 1696513952, 20992460,
        <<"VWw6aNPou6Nj6WImgaYNcYEKyc7SCBCTUwrwt_wxrK8">>},
    {1275481, 1696514212, 20992706,
        <<"L7q3syXwKAXNNisnFNocZkX-6CP-WXonxcA9Foc2olI">>},
    {1275482, 1696514242, 20992737,
        <<"Ryy0lEcLZtViBMuOuOHcuoOBXMK0aqU2USVqG_20ZBc">>},
    {1275483, 1696514280, 20992766,
        <<"zbbfsrF24QhcrbSewNnbLilvdODTPFUBbYo84OlTwbE">>}
]).

%% @doc A wide range carries the standard 10,800-step suffix without being
%% rejected merely because the parent is farther away.
wide_step_range_test() ->
    Output = <<0:256>>,
    Steps = lists:duplicate(10800, Output),
    Prev =
        #block{
            nonce_limiter_info =
                #nonce_limiter_info{
                    global_step_number = 1,
                    output = Output
                }
        },
    Next =
        #block{
            nonce_limiter_info =
                #nonce_limiter_info{
                    global_step_number = 10802,
                    prev_output = Output,
                    steps = Steps
                }
        },
    ?assertEqual(ok, lib_arweave_block:check_step_number(Next, Prev)),
    NextInfo = Next#block.nonce_limiter_info,
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-step-number">> }},
        lib_arweave_block:check_step_number(
            Next#block{
                nonce_limiter_info =
                    NextInfo#nonce_limiter_info{ steps = tl(Steps) }
            },
            Prev
        )
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-step-number">> }},
        lib_arweave_block:check_step_number(
            Next#block{
                nonce_limiter_info =
                    NextInfo#nonce_limiter_info{ steps = [Output | Steps] }
            },
            Prev
        )
    ).

%% @doc A VDF answer is accepted only when it carries the atom `true'.
vdf_fails_closed_test() ->
    Refused =
        fun(Answer) ->
            case lib_arweave_block:holds(
                maps:get(<<"valid">>, Answer, false) =:= true,
                <<"invalid-vdf-chain">>,
                <<"detail">>
            ) of
                ok -> accepted;
                {error, Error} -> maps:get(<<"message">>, Error)
            end
        end,
    ?assertEqual(accepted, Refused(#{ <<"valid">> => true })),
    ?assertEqual(
        <<"invalid-vdf-chain">>,
        Refused(#{ <<"valid">> => false })
    ),
    ?assertEqual(
        <<"invalid-vdf-chain">>,
        Refused(#{ <<"valid">> => <<"true">> })
    ),
    ?assertEqual(<<"invalid-vdf-chain">>, Refused(#{})).

%% @doc The reward history hash mainnet committed to, recomputed by the check
%% itself over a history read back out of the persistent list.
%%
%% Five blocks, so the check runs against histories of none, one, two, three and
%% four elements, and every one of them is walked in whole. The state each block
%% is checked against is the one the block before it produced, so what is proven
%% is the pair: what `next_reward_history/3' writes is what `reward_history/2'
%% reads back, and what it reads back is what mainnet signed.
reward_history_hash_matches_mainnet_test() ->
    Opts = test_opts(),
    lists:foldl(
        fun(Row, {Head, Previous}) ->
            Next = reward_history_block(Row),
            State = #{ <<"reward-history">> => Head },
            ?assertEqual(
                ok,
                lib_arweave_block:check_reward_history_hash(
                    Next,
                    #block{
                        height = Next#block.height - 1,
                        reward_history_hash = Previous,
                        reward_history = reward_history(State, Opts)
                    }
                )
            ),
            {
                lib_arweave_state:next_reward_history(State, Next, Opts),
                Next#block.reward_history_hash
            }
        end,
        {[], <<>>},
        ?REWARD_HISTORY_VECTOR
    ).

%% @doc The block-time history hash mainnet committed to, recomputed by the
%% check itself over a history read back out of the persistent list.
%%
%% The elements are not stated: they are measured from the blocks' own
%% timestamps and step numbers by the vendored rule, so what the list holds is
%% what mainnet hashed rather than a transcription of it.
block_time_history_hash_matches_mainnet_test() ->
    Opts = test_opts(),
    [Parent | Blocks] = ?BLOCK_TIME_HISTORY_VECTOR,
    lists:foldl(
        fun(Row, {Head, Previous}) ->
            Next = block_time_history_block(Row),
            State = #{ <<"block-time-history">> => Head },
            Prev =
                Previous#block{
                    block_time_history = block_time_history(State, Opts)
                },
            ?assertEqual(
                ok,
                lib_arweave_block:check_block_time_history_hash(Next, Prev)
            ),
            {
                lib_arweave_state:next_block_time_history(
                    State, Next, Prev, Opts),
                Next
            }
        end,
        {[], block_time_history_block(Parent)},
        Blocks
    ).

%% @doc The named profiles resolve to the checks they stand for, an explicit
%% list is put back into the order the checks run in, and both refusals hold:
%% a name this device does not know, and a set that omits a check another reads
%% from.
%%
%% Both refusals are the point of the key. A caller who misspells a check and is
%% quietly given a shorter set gets a block whose `validation/checks' is
%% accurate and whose validation is weaker than they asked for -- which is
%% exactly the failure selective verification exists to rule out.
selected_checks_test() ->
    {ok, Names} =
        lib_arweave_block:selected(#{ <<"profile">> => <<"full">> }, #{}),
    % No profile at all is full validation.
    ?assertEqual({ok, Names}, lib_arweave_block:selected(#{}, #{})),
    ?assertEqual(
        {ok, [<<"linkage">>, <<"identity">>]},
        lib_arweave_block:selected(
            #{ <<"verify">> => [<<"identity">>, <<"linkage">>] }, #{})
    ),
    % A query string cannot spell a list, so a comma-separated binary is one.
    ?assertEqual(
        {ok, [<<"linkage">>, <<"identity">>]},
        lib_arweave_block:selected(
            #{ <<"verify">> => <<"identity,linkage">> }, #{})
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"unknown-check">> }},
        select_error(#{ <<"verify">> => <<"identtiy">> })
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"unknown-profile">> }},
        select_error(#{ <<"profile">> => <<"quick">> })
    ),
    % `accounts' reads the transaction check's results and `poa' reads the
    % proof of work's, so neither may be asked for alone.
    ?assertMatch(
        {error, #{ <<"message">> := <<"incomplete-checks">> }},
        select_error(#{ <<"verify">> => <<"accounts">> })
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"incomplete-checks">> }},
        select_error(#{ <<"verify">> => <<"poa">> })
    ),
    ?assertMatch(
        {ok, _},
        lib_arweave_block:selected(
            #{ <<"verify">> => <<"transactions,accounts">> }, #{})
    ).

%% @doc A public block application passes component controls as AO requests,
%% leaving the carried account and block-index states canonical. The empty
%% transaction set still reads the reward addresses, applies the released
%% reward, checks the resulting wallet root and extends the branch-local index.
component_request_boundaries_test() ->
    Opts = test_opts(),
    Height = 1_500_010,
    LockedAddress = crypto:hash(sha256, <<"released-reward">>),
    RewardAddress = crypto:hash(sha256, <<"next-miner">>),
    Reward = {LockedAddress, 1, 7, 1},
    Prev =
        (test_header(Height, 262144, []))#block{
            timestamp = 1_700_000_000,
            diff = 1,
            reward_addr = RewardAddress,
            reward_pool = 1_000_000_000_000,
            reward_history = [Reward],
            nonce_limiter_info =
                #nonce_limiter_info{ global_step_number = 1 }
        },
    Next0 =
        (test_header(Height + 1, Prev#block.weave_size, []))#block{
            timestamp = Prev#block.timestamp + 120,
            diff = 1,
            previous_block = Prev#block.indep_hash,
            reward_addr = RewardAddress,
            nonce_limiter_info =
                #nonce_limiter_info{ global_step_number = 2 }
        },
    {ok,
        {
            EndowmentPool,
            MinerReward,
            DebtSupply,
            Latch,
            Multiplier,
            Updated
        }} = ar_node_utils:update_accounts(Next0, Prev, #{}),
    {WalletRoot, _Tree} =
        lib_arweave_accounts:root(
            lib_arweave_accounts:insert_all(
                maps:to_list(Updated),
                lib_arweave_accounts:new()
            )
        ),
    Next =
        Next0#block{
            reward_pool = EndowmentPool,
            reward = MinerReward,
            debt_supply = DebtSupply,
            kryder_plus_rate_multiplier_latch = Latch,
            kryder_plus_rate_multiplier = Multiplier,
            wallet_list = hb_util:decode(WalletRoot)
        },
    Accounts = empty_account_state(Opts),
    Index = block_index([index_entry(Prev)], Opts),
    RewardHistory =
        lib_arweave_history:append(
            <<"reward-history">>, Reward, Height, [], Opts),
    Base =
        (lib_arweave_block:from(Prev, Opts))#{
            <<"device">> => <<"arweave-block@2.9">>,
            <<"accounts">> => Accounts,
            <<"block-index">> => Index,
            <<"reward-history">> => RewardHistory
        },
    {ok, Applied} =
        hb_ao:resolve(
            Base,
            #{
                <<"path">> => <<"apply">>,
                <<"verify">> => <<"transactions,accounts">>,
                <<"next">> => lib_arweave_block:from(Next, Opts),
                <<"transactions">> => []
            },
            Opts
        ),
    ?assertEqual([<<"transactions">>, <<"accounts">>], checks(Applied, Opts)),
    AppliedAccounts = hb_maps:get(<<"accounts">>, Applied, not_found, Opts),
    ?assertEqual(
        WalletRoot,
        hb_maps:get(<<"root">>, AppliedAccounts, not_found, Opts)
    ),
    AppliedIndex = hb_maps:get(<<"block-index">>, Applied, not_found, Opts),
    ?assertEqual(
        2,
        hb_util:int(hb_maps:get(<<"length">>, AppliedIndex, 0, Opts))
    ),
    ?assertEqual(
        {ok, index_entry(Next)},
        without_priv(
            hb_ao:resolve(
                AppliedIndex,
                #{ <<"path">> => <<"at">>, <<"height">> => 1 },
                Opts
            ),
            Opts
        )
    ).

select_error(Selection) ->
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave-block@2.9">> },
        Selection#{ <<"path">> => <<"validate">> },
        #{ <<"store">> => [hb_test_utils:test_store()] }
    ).

%% @doc A header materialised against an authenticated block index establishes
%% its identity, its linkage, its index entry and its transactions -- and says
%% so, in the block it produces.
%%
%% The index is the whole of the trust here: the hash, weave size and
%% transaction root it records were committed to by a block this node validated,
%% so a peer that serves a different header, or the right header with a
%% different transaction set, is caught by one of the four.
materialize_against_index_test() ->
    Opts = test_opts(),
    {Block, Msg, TXs} = test_block(1_500_000, 3_000_000, Opts),
    Expected = index_entry(Block),
    Previous = #{ <<"indep-hash">> => hb_util:encode(Block#block.previous_block),
        <<"weave-size">> => 3_000_000, <<"tx-root">> => <<>> },
    {ok, Materialized} =
        materialize(Msg, Expected, Previous, <<"archive">>, TXs, Opts),
    ?assertEqual(
        [<<"linkage">>, <<"identity">>, <<"block-index">>, <<"transactions">>],
        checks(Materialized, Opts)
    ),
    % The header round-trips: what is stored re-encodes to the bytes the peer
    % served, so nothing about the block was lost in materialising it. The
    % AO-Core keys the materialisation adds are not block fields and do not
    % perturb the header, which is the property that lets a block message be
    % both the node's state and the block itself.
    ?assertEqual(
        ar_serialize:block_to_binary(lib_arweave_block:to(Msg, Opts)),
        ar_serialize:block_to_binary(
            lib_arweave_block:to(Materialized, Opts)
        )
    ),
    % The parent stays in the canonical scalar header field. It is resolved by
    % the block device when `previous' is requested, so this state remains
    % cache-portable even though an Arweave block hash is not an AO-Core ID.
    ?assertEqual(false, maps:is_key(<<"previous">>, Materialized)),
    ?assertEqual(
        hb_util:encode(Block#block.previous_block),
        maps:get(<<"previous-block">>, Materialized)
    ),
    % Every transaction is placed, at the offset the transaction root was built
    % over, in the block's own order.
    Placements = maps:get(<<"transactions">>, Materialized),
    ?assertEqual(length(TXs), length(Placements)),
    lists:foreach(
        fun({Position, {Placement, TX}}) ->
            Record = lib_arweave_tx:to_tx(TX, Opts),
            ?assertEqual(Position, maps:get(<<"position">>, Placement)),
            ?assertEqual(
                hb_util:encode(Block#block.indep_hash),
                maps:get(<<"block">>, Placement)
            ),
            ?assertEqual(1_500_000, maps:get(<<"height">>, Placement)),
            ?assertEqual(
                Record#tx.data_size,
                maps:get(<<"data-size">>, Placement)
            ),
            ?assertEqual(
                hb_util:encode(Record#tx.data_root),
                maps:get(<<"data-root">>, Placement)
            ),
            ?assert(maps:get(<<"start-offset">>, Placement) >= 3_000_000),
            ?assertMatch(
                {link, _ID, #{ <<"type">> := <<"link">> }},
                maps:get(<<"transaction">>, Placement)
            )
        end,
        lists:zip(
            lists:seq(0, length(TXs) - 1),
            lists:zip(Placements, TXs)
        )
    ),
    % The placements describe a partition of the block's own stretch of the
    % weave: every one of them lies inside it, and no two overlap. An index
    % built from placements that did would return one transaction's bytes for
    % another, which is the failure mode an offset index has.
    Ranges =
        lists:sort(
            [
                {
                    maps:get(<<"start-offset">>, Placement),
                    maps:get(<<"start-offset">>, Placement)
                        + maps:get(<<"data-size">>, Placement)
                }
            ||
                Placement <- Placements
            ]
        ),
    lists:foldl(
        fun({Start, End}, Floor) ->
            ?assert(Start >= Floor),
            ?assert(End =< Block#block.weave_size),
            End
        end,
        3_000_000,
        Ranges
    ),
    ok.

%% @doc A header-only materialisation establishes exactly one thing, fetches no
%% transaction, and places none.
materialize_headers_only_test() ->
    Opts = test_opts(),
    {Block, Msg, _TXs} = test_block(1_500_001, 3_000_000, Opts),
    {ok, Materialized} =
        materialize(Msg, index_entry(Block), #{}, <<"headers">>, [], Opts),
    ?assertEqual([<<"identity">>], checks(Materialized, Opts)),
    ?assertEqual([], maps:get(<<"transactions">>, Materialized)).

%% @doc A peer cannot substitute another block, another transaction set, or
%% another parent, however well-formed what it serves is.
materialize_refuses_a_substituted_block_test() ->
    Opts = test_opts(),
    {Block, Msg, TXs} = test_block(1_500_002, 3_000_000, Opts),
    Expected = index_entry(Block),
    Previous = #{ <<"indep-hash">> => hb_util:encode(Block#block.previous_block),
        <<"weave-size">> => 3_000_000, <<"tx-root">> => <<>> },
    % The index records a different block at this height.
    ?assertMatch(
        {error, #{ <<"message">> := <<"unexpected-block">> }},
        materialize(
            Msg,
            Expected#{
                <<"indep-hash">> =>
                    hb_util:encode(crypto:strong_rand_bytes(48))
            },
            Previous,
            <<"archive">>,
            TXs,
            Opts
        )
    ),
    % The index records a different block below this one.
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-previous-block">> }},
        materialize(
            Msg,
            Expected,
            Previous#{
                <<"indep-hash">> =>
                    hb_util:encode(crypto:strong_rand_bytes(48))
            },
            <<"archive">>,
            TXs,
            Opts
        )
    ),
    % The index records a different weave size or transaction root for it.
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-block-index-entry">> }},
        materialize(
            Msg,
            Expected#{ <<"weave-size">> => Block#block.weave_size + 1 },
            Previous,
            <<"archive">>,
            TXs,
            Opts
        )
    ),
    % The weave did not grow from where the index says it was.
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-weave-size">> }},
        materialize(
            Msg,
            Expected,
            Previous#{ <<"weave-size">> => 2_999_999 },
            <<"archive">>,
            TXs,
            Opts
        )
    ).

%% @doc A transaction body that is not the one its identifier names is refused
%% by the signature the identifier is the hash of.
materialize_refuses_a_forged_transaction_test() ->
    Opts = test_opts(),
    {Block, Msg, [TX | Rest]} = test_block(1_500_003, 3_000_000, Opts),
    Previous = #{ <<"indep-hash">> => hb_util:encode(Block#block.previous_block),
        <<"weave-size">> => 3_000_000, <<"tx-root">> => <<>> },
    % Keep the identifier, change the body. The transaction check recomputes
    % the identifier from a signature over the fields, so it does not agree.
    Forged = forged_field(TX, <<"field-quantity">>, <<"1">>),
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-tx-signature">> }},
        materialize(
            Msg,
            index_entry(Block),
            Previous,
            <<"archive">>,
            [Forged | Rest],
            Opts
        )
    ).

%% @doc A transaction with no real owner is refused by the transaction check,
%% and so never reaches the account lookup.
%%
%% An owner of 512 zero bytes is no RSA modulus, so the signature cannot
%% verify. It matters where that is caught: `balances/5' encodes each sender's
%% address to ask the account tree for it, and upstream answers `not_set' for
%% this owner. The transaction check runs first and `checks/0' makes the
%% account check depend on it, so the atom has nowhere to go.
materialize_refuses_a_transaction_with_no_owner_test() ->
    Opts = test_opts(),
    {Block, Msg, [TX | Rest]} = test_block(1_500_005, 3_000_000, Opts),
    Previous = #{ <<"indep-hash">> => hb_util:encode(Block#block.previous_block),
        <<"weave-size">> => 3_000_000, <<"tx-root">> => <<>> },
    Ownerless =
        forged_field(
            TX,
            <<"keyid">>,
            <<"publickey:", (hb_util:encode(?DEFAULT_OWNER))/binary>>
        ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-tx-signature">> }},
        materialize(
            Msg,
            index_entry(Block),
            Previous,
            <<"archive">>,
            [Ownerless | Rest],
            Opts
        )
    ).

%% @doc Materialising against an index refuses to be asked for a check it
%% cannot perform, rather than quietly omitting it.
materialize_refuses_state_dependent_checks_test() ->
    Opts = test_opts(),
    {_Block, Msg, _TXs} = test_block(1_500_004, 3_000_000, Opts),
    lists:foreach(
        fun(Check) ->
            ?assertMatch(
                {error, #{ <<"message">> := <<"unavailable-check">> }},
                materialize(
                    Msg, #{}, #{}, <<"full">>, [], Opts#{ verify => Check })
            )
        end,
        [<<"vdf">>, <<"fields">>, <<"reward-history">>]
    ).


%% @doc The transaction-anchor window is the chain itself, walked back from the
%% block being extended and stopped at the depth an anchor may reach.
%%
%% Nothing is carried: the window a block sees is read out of the blocks below
%% it, one link at a time, which is what makes a fifty-block window cost fifty
%% blocks of storage rather than fifty copies of fifty summaries. Two things
%% have to hold for that, and neither shows up as an error when it does not:
%% the walk has to read blocks from the name publication files them under, and
%% it has to stop at the oldest block this node holds rather than raise on the
%% link below it. A walk that quietly returned nothing would leave every
%% block-anchored transaction on the network unanchorable, and the only symptom
%% would be blocks being refused for the wrong reason.
anchor_window_walks_the_chain_test() ->
    Opts = test_opts(),
    Depth = ar_block:get_max_tx_anchor_depth(),
    Count = Depth + 5,
    Tip = test_chain(Count, Opts),
    ?assertEqual(
        [ chain_hash(Height) || Height <- window(Count, Depth) ],
        lib_arweave_state:block_anchors(Tip, Opts)
    ),
    ?assertEqual(
        [ chain_tx(Height) || Height <- window(Count, Depth) ],
        lib_arweave_state:recent_transactions(Tip, Opts)
    ),
    % The walk ends where the chain this node holds does, rather than raising
    % on the link below the oldest block.
    Shallow = test_chain(3, Opts),
    ?assertEqual(3, length(lib_arweave_state:block_anchors(Shallow, Opts))).

%%% Test helpers.

%% @doc A store of this vector's own, so that what one vector writes cannot be
%% read by another.
test_opts() ->
    #{ <<"store">> => [hb_test_utils:test_store()] }.

%% @doc Persist the canonical empty account state through its AO interface.
empty_account_state(Opts) ->
    hb_util:ok(
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-wallets@2.9">> },
            #{ <<"path">> => <<"finalize">> },
            Opts
        )
    ).

%% @doc Build a branch-local block index through semantic AO appends.
block_index(Entries, Opts) ->
    hb_util:ok(
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-block-index@2.9">> },
            #{
                <<"path">> => <<"append">>,
                <<"entries">> => hb_util:list_to_numbered_message(Entries)
            },
            Opts
        )
    ).

%% @doc Strip resolver-private metadata before comparing a public result.
without_priv({ok, Result}, Opts) ->
    {ok, hb_maps:without([<<"priv">>], Result, Opts)};
without_priv(Result, _Opts) ->
    Result.

%% @doc A run of blocks published under the names a walk reads them from, each
%% naming the one below it and carrying one transaction of its own.
test_chain(Count, Opts) ->
    lists:foldl(
        fun(Height, _Previous) ->
            Block =
                #{
                    <<"device">> => <<"arweave-block@2.9">>,
                    <<"indep-hash">> => chain_hash(Height),
                    <<"height">> => Height,
                    <<"previous-block">> => chain_hash(Height - 1),
                    <<"txs">> => [chain_tx(Height)]
                },
            {ok, ID} = hb_cache:write(Block, Opts),
            hb_cache:link(
                ID, lib_arweave_paths:block(chain_hash(Height)), Opts),
            Block
        end,
        [],
        lists:seq(1, Count)
    ).

%% @doc The heights a window of `Depth' blocks below `Count' covers, newest
%% first.
window(Count, Depth) ->
    lists:seq(Count, Count - Depth + 1, -1).

chain_hash(Height) ->
    hb_util:encode(crypto:hash(sha384, [<<"chain-">>, hb_util:bin(Height)])).

chain_tx(Height) ->
    hb_util:encode(crypto:hash(sha256, [<<"chain-tx-">>, hb_util:bin(Height)])).

%% @doc Resolve `materialize' against a header, naming the entries the caller
%% would have read out of an authenticated block index.
materialize(Msg, Expected, Previous, Profile, TXs, Opts) ->
    hb_ao:resolve(
        Msg#{ <<"device">> => <<"arweave-block@2.9">> },
        maps:merge(
            #{
                <<"path">> => <<"materialize">>,
                <<"profile">> => Profile,
                <<"expected">> => Expected,
                <<"previous-entry">> => Previous,
                <<"transactions">> => TXs
            },
            case maps:get(verify, Opts, none) of
                none -> #{};
                Check -> #{ <<"verify">> => Check }
            end
        ),
        maps:remove(verify, Opts)
    ).

%% @doc The checks a block records having been validated by.
checks(Block, Opts) ->
    hb_util:message_to_ordered_list(
        hb_maps:get(
            <<"checks">>,
            hb_maps:get(<<"validation">>, Block, #{}, Opts),
            [],
            Opts
        ),
        Opts
    ).

%% @doc The triplet an authenticated block index records for a block.
index_entry(Block) ->
    #{
        <<"indep-hash">> => hb_util:encode(Block#block.indep_hash),
        <<"weave-size">> => Block#block.weave_size,
        <<"tx-root">> => hb_util:encode(Block#block.tx_root)
    }.

%% @doc A block carrying three signed transactions, with the transaction root,
%% block size and weave size the protocol derives from them, and its own
%% `indep-hash' computed the way the protocol computes one.
%%
%% The signature is not a real one -- nothing checked against an index verifies
%% a block signature, because the preimage binds a cumulative difficulty no
%% index entry carries -- but the transactions are signed for real, because the
%% transaction check does verify those.
test_block(Height, PrevWeaveSize, Opts) ->
    Wallet = ar_wallet:new(),
    TXs =
        [
            ar_tx:sign(
                #tx{
                    format = 2,
                    anchor = crypto:strong_rand_bytes(32),
                    reward = 1_000_000_000_000,
                    data_size = Size,
                    data_root = crypto:strong_rand_bytes(32)
                },
                Wallet
            )
        ||
            Size <- [262144, 131072, 1048576]
        ],
    Block = test_header(Height, PrevWeaveSize, TXs),
    {
        Block,
        lib_arweave_block:from(
            Block#block{ txs = [ TX#tx.id || TX <- TXs ] },
            Opts
        ),
        [ lib_arweave_tx:from_tx(TX, Opts) || TX <- TXs ]
    }.

%% @doc Restate one of a transaction's fields, leaving the identifier the block
%% names it by untouched.
%%
%% A `tx@1.0' message keeps its fields in the commitment, so that is where a
%% forgery has to be made: a value written onto the message beside the
%% commitment is not part of the transaction and does not reach the record. The
%% commitment's key is the transaction identifier, so the forged body is still
%% offered as the transaction the block declared -- which is what makes the
%% signature the only thing left that can refuse it.
forged_field(TX, Field, Value) ->
    [{ID, Commitment}] = maps:to_list(maps:get(<<"commitments">>, TX)),
    TX#{ <<"commitments">> => #{ ID => Commitment#{ Field => Value } } }.

test_header(Height, PrevWeaveSize, TXs) ->
    Sized = ar_block:generate_size_tagged_list_from_txs(TXs, Height),
    Rooted =
        ar_block:generate_tx_tree(
            #block{
                height = Height,
                previous_block = crypto:strong_rand_bytes(48),
                nonce = 0,
                usd_to_ar_rate = {1, 1},
                scheduled_usd_to_ar_rate = {1, 1},
                % The fixed-width fields of the signed-hash preimage, which have
                % no record default wide enough to be encoded.
                chunk_hash = crypto:strong_rand_bytes(32),
                block_time_history_hash = crypto:strong_rand_bytes(32),
                reward_history_hash = crypto:strong_rand_bytes(32),
                packing_difficulty = 0,
                replica_format = 0,
                partition_number = 0,
                nonce_limiter_info =
                    #nonce_limiter_info{
                        output = crypto:strong_rand_bytes(32),
                        seed = crypto:strong_rand_bytes(48),
                        next_seed = crypto:strong_rand_bytes(48)
                    },
                signature = crypto:strong_rand_bytes(512),
                reward_addr = crypto:strong_rand_bytes(32),
                txs = TXs
            },
            [ {Root, Offset} || {{_TX, Root}, Offset} <- Sized ]
        ),
    Size =
        lists:foldl(
            fun(TX, Acc) -> Acc + ar_tx:get_weave_size_increase(TX, Height) end,
            0,
            TXs
        ),
    Header =
        Rooted#block{
            block_size = Size,
            weave_size = PrevWeaveSize + Size
        },
    Header#block{
        indep_hash =
            ar_block:indep_hash2(
                ar_block:generate_signed_hash(Header),
                Header#block.signature
            )
    }.

%% @doc Read the reward history a state carries, from the entries in the store.
reward_history(State, Opts) ->
    read(
        fun() ->
            lib_arweave_history:values(
                hb_maps:get(<<"reward-history">>, State, [], Opts),
                Opts
            )
        end
    ).

%% @doc Read the block-time history a state carries, from the entries in the
%% store.
block_time_history(State, Opts) ->
    read(
        fun() ->
            lib_arweave_history:values(
                hb_maps:get(<<"block-time-history">>, State, [], Opts),
                Opts
            )
        end
    ).

%% @doc Read a history in a process of its own, so that what answers is the
%% entries in the store rather than the window the process that wrote them
%% memoised. A node reads a chain state back in whichever process is applying
%% blocks, which is never the one that built the entries, so this is the read
%% these vectors are about.
read(Fun) ->
    Caller = self(),
    Ref = make_ref(),
    spawn(fun() -> Caller ! {Ref, Fun()} end),
    receive {Ref, Values} -> Values after 10000 -> error('read-timeout') end.

%% @doc The block a reward-history vector row stands for, carrying the fields
%% `ar_rewards:add_element/2' reads its element out of.
reward_history_block({Height, Address, Reward, Denomination, Hash}) ->
    #block{
        height = Height,
        diff = ?REWARD_HISTORY_VECTOR_DIFF,
        reward_addr = hb_util:decode(Address),
        reward = Reward,
        denomination = Denomination,
        reward_history_hash = hb_util:decode(Hash)
    }.

%% @doc The block a block-time-history vector row stands for, carrying the
%% fields `ar_block_time_history:update_history/2' measures its element from.
block_time_history_block({Height, Timestamp, Steps, none}) ->
    #block{
        height = Height,
        timestamp = Timestamp,
        nonce_limiter_info = #nonce_limiter_info{ global_step_number = Steps }
    };
block_time_history_block({Height, Timestamp, Steps, Hash}) ->
    Block = block_time_history_block({Height, Timestamp, Steps, none}),
    Block#block{ block_time_history_hash = hb_util:decode(Hash) }.
