%%% @doc Deterministic admission vectors for full Arweave block validation.
%%%
%%% A live, full account transition is provided by
%%% `dev_arweave_sync_test_vectors:live_account_transition/0'. It hydrates a
%%% recent checkpoint because public peers prune historical wallet lists.
-module(dev_arweave_block_test_vectors).
-include("include/hb.hrl").
-include("include/ar_consensus.hrl").
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

%%% The height a produced block is mined at. It is above the 2.9 fork and away
%%% from every schedule its parent drives: not a difficulty retarget height, not
%%% a price adjustment height and not a VDF difficulty retarget height, so what
%%% a solution produces is checked against the plain inheritance rules rather
%%% than against three retargets at once.
-define(MINING_HEIGHT, 1_700_000).

%%% The nonce limiter step the parent of a produced block was found at. It sits
%%% on an entropy reset line, so the next one is a full interval of 1,200 steps
%%% away and no search a vector performs can reach it.
-define(MINING_PARENT_STEP, 1_200_000).

%%% The step a reset-crossing vector's parent was found at instead: the last one
%%% below a reset line, so every step its child's search reaches is above one.
-define(MINING_RESET_PARENT_STEP, 1_201_199).

%%% The VDF difficulty the vectors' timeline runs at. Nothing in the nonce
%%% limiter reads `?VDF_DIFFICULTY' rather than the blocks' own field, and a
%%% step at mainnet's difficulty costs six seconds where this costs a fraction
%%% of a millisecond.
%%%
%%% It has to be more than a single iteration. The VDF NIF picks a fused SHA-2
%%% kernel per architecture and accepts it after a load-time self-test at one
%%% difficulty; the kernel accepted on x86 disagrees with the reference at a
%%% single iteration and agrees at every difficulty above it, so a vector run at
%%% one iteration verifies on ARM and refuses on x86 -- a property of the kernel
%%% rather than of anything these vectors check.
%%% `lib_arweave_vdf_timeline' documents the same disagreement for the
%%% computation kernels, and self-tests per difficulty because of it.
-define(MINING_VDF_DIFFICULTY, 2).

%%% The weave a produced block is mined from -- two chunks, in one transaction,
%%% in the block below -- and the leading nonces of each chunk a search tries.
%%% Every candidate nonce addresses one 8 KiB sub-chunk and packing one costs an
%%% 8 MiB RandomX entropy blob, so the search space is bounded by what it costs
%%% to build rather than by what the protocol permits.
-define(MINING_CHUNKS, 2).
-define(MINING_NONCES, 4).

%%% How far past its parent a search may advance the timeline. The odds per
%%% attempt are the protocol's own and they are not close to certain:
%%% `ar_difficulty:min_difficulty/1' floors the packing-difficulty-10 threshold
%%% at three quarters of the hash space, so a two-chunk solution passes one
%%% attempt in four, and the hundredfold proof-of-access multiplier a one-chunk
%%% solution pays leaves it about one in eighty. Four hundred steps of four
%%% nonces leaves the slower of the two searches failing about once in a
%%% billion runs.
-define(MINING_STEP_LIMIT, 400).

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

%% @doc `produce' builds a block this node's own `validate' accepts under the
%% `full' profile. This is the property the producer exists for: every field it
%% derives is the one the check that reads it recomputes.
%%
%% The solution is mined here rather than by the mining device, so that what is
%% established is `produce' alone. It is built through `~arweave-spora@2.9' --
%% `h0' over the parent's seed and the step's own nonce limiter output,
%% `recall-range' and `recall-byte' from that entropy, `pack-sub-chunk' for the
%% 8 KiB a nonce hashes, and `h1' over it -- and the search advances the nonce
%% limiter one step at a time, exactly as a miner does.
produce_yields_a_valid_block_test() ->
    Opts = mining_opts(),
    {Base, Prev} = mining_chain(Opts),
    {ok, Block} = produce(Base, Prev, mine(one_chunk, Prev, Opts), [], Opts),
    assert_valid(Base, Block, [], Opts),
    % A one-chunk solution proves one recall byte, and the block says so by
    % declaring neither a second one nor anything derived from a second chunk.
    ?assertEqual(false, maps:is_key(<<"recall-byte2">>, Block)),
    ?assertEqual(false, maps:is_key(<<"chunk2-hash">>, Block)),
    ?assertEqual(false, maps:is_key(<<"unpacked-chunk2-hash">>, Block)),
    ?assertEqual(?REPLICA_2_9_PACKING_DIFFICULTY,
        maps:get(<<"packing-difficulty">>, Block)),
    ?assertEqual(1, maps:get(<<"replica-format">>, Block)),
    ?assertEqual(?MINING_HEIGHT + 1, maps:get(<<"height">>, Block)).

%% @doc A solution found in both recall ranges produces a valid block carrying
%% the second recall byte, the second proof and the two hashes over its chunk.
%%
%% The search takes only the steps whose two ranges fall in different chunks of
%% the weave, so `poa2' is a second proof of a second chunk rather than the
%% first one restated -- which is what makes the second recall byte something
%% the checks can be wrong about.
produce_yields_a_valid_two_chunk_block_test() ->
    Opts = mining_opts(),
    {Base, Prev} = mining_chain(Opts),
    {ok, Block} = produce(Base, Prev, mine(two_chunk, Prev, Opts), [], Opts),
    assert_valid(Base, Block, [], Opts),
    Header = lib_arweave_block:to(Block, Opts),
    ?assert(is_integer(Header#block.recall_byte2)),
    ?assertNotEqual(Header#block.recall_byte, Header#block.recall_byte2),
    ?assertNotEqual((Header#block.poa2)#poa.chunk, <<>>),
    ?assertNotEqual(
        (Header#block.poa)#poa.chunk,
        (Header#block.poa2)#poa.chunk
    ),
    ?assertEqual(
        crypto:hash(sha256, (Header#block.poa2)#poa.chunk),
        Header#block.chunk2_hash
    ),
    ?assertEqual(
        crypto:hash(sha256, (Header#block.poa2)#poa.unpacked_chunk),
        Header#block.unpacked_chunk2_hash
    ).

%% @doc The signature is a real one over the block's own signed hash, the
%% identifier is the hash of the two together, and a block whose signed fields
%% were restated afterwards is refused.
%%
%% Both are asked of the device rather than computed here: `verify-signature'
%% and `id' are the keys a producer's own output is judged by, and a vector that
%% recomputed them itself would only prove that two copies of one expression
%% agree.
produce_signs_the_block_test() ->
    Opts = mining_opts(),
    {Base, Prev} = mining_chain(Opts),
    {ok, Block} = produce(Base, Prev, mine(one_chunk, Prev, Opts), [], Opts),
    ?assertEqual(
        {ok, #{ <<"valid">> => true }},
        without_priv(
            hb_ao:resolve(
                Block,
                #{
                    <<"path">> => <<"verify-signature">>,
                    <<"previous-cumulative-diff">> =>
                        Prev#block.cumulative_diff
                },
                Opts
            ),
            Opts
        )
    ),
    ?assertEqual(
        {ok, #{ <<"indep-hash">> => maps:get(<<"indep-hash">>, Block) }},
        without_priv(hb_ao:resolve(Block, <<"id">>, Opts), Opts)
    ),
    % The signature covers every consensus field, so restating one after the
    % fact leaves a block nothing can have signed.
    lists:foreach(
        fun({Key, Value}) ->
            ?assertMatch(
                {error, #{ <<"message">> := <<"invalid-signature">> }},
                validate(Base, Block#{ Key => Value }, [], Opts)
            )
        end,
        [
            {<<"hash-list-merkle">>, hb_util:encode(crypto:hash(sha384, <<>>))},
            {<<"reward-pool">>, maps:get(<<"reward-pool">>, Block) + 1},
            {<<"signature">>, hb_util:encode(crypto:strong_rand_bytes(512))}
        ]
    ).

%% @doc A block is the same block whether or not the node's own nonce limiter
%% has already run the steps it declares.
%%
%% `lib_arweave_vdf_timeline' is anchored on each validated block and computes
%% forward from it at close to real time, on a kernel it self-tests against
%% `ar_vdf:compute/3'. A producer that ignored it would recompute, on the slower
%% portable kernel, work the node had already done -- and at mainnet's VDF
%% difficulty that is the difference between a pass that keeps pace with the
%% timeline and one that cannot.
%%
%% Taking those steps has to be invisible, which is what this asserts: the
%% interval is held in full, and the block produced from it differs from the one
%% produced without it in exactly two fields. Those two are the signature and
%% the identifier taken over it -- RSA-PSS salts each signing, so no two
%% signings of one block agree. Every field the signature covers, the whole
%% nonce limiter info among them, is identical.
produce_consumes_the_nonce_limiter_test() ->
    Opts = mining_opts(),
    {Base, Prev} = mining_chain(Opts),
    Solution = mine(one_chunk, Prev, Opts),
    {ok, Computed} = produce(Base, Prev, Solution, [], Opts),
    Running = Opts#{ <<"arweave-vdf-timeline">> => true },
    Info = Prev#block.nonce_limiter_info,
    PrevStep = Info#nonce_limiter_info.global_step_number,
    Step = hb_util:int(hb_maps:get(<<"global-step-number">>, Solution, 0, Opts)),
    % Non-vacuous only if the timeline really is the source: without every step
    % of the interval it would fall back and the two blocks would be equal for
    % the wrong reason.
    ?assertEqual(
        Step - PrevStep,
        map_size(anchored_timeline(Info, PrevStep, Step, Running))
    ),
    {ok, FromTimeline} = produce(Base, Prev, Solution, [], Running),
    ?assertEqual(
        [<<"indep-hash">>, <<"signature">>],
        block_difference(Computed, FromTimeline, Opts)
    ),
    assert_valid(Base, FromTimeline, [], Running).

%% @doc The keys two block messages differ on, so that a failure names the
%% field rather than printing two headers.
block_difference(Left, Right, Opts) ->
    [
        Key
    ||
        Key <-
            lists:usort(
                maps:keys(hb_private:reset(Left))
                    ++ maps:keys(hb_private:reset(Right))
            ),
        hb_maps:get(Key, Left, not_found, Opts)
            =/= hb_maps:get(Key, Right, not_found, Opts)
    ].

%% @doc Anchor the node's nonce limiter on a block and wait for it to hold the
%% interval up to a step. It computes between messages, so what it holds is
%% whatever it has reached when asked.
anchored_timeline(Info, PrevStep, Step, Opts) ->
    lib_arweave_vdf_timeline:advance(
        Info#nonce_limiter_info.seed,
        Info#nonce_limiter_info.vdf_difficulty,
        PrevStep,
        Info#nonce_limiter_info.output,
        ar_nonce_limiter:get_entropy_reset_point(
            PrevStep, PrevStep + ?MINING_STEP_LIMIT),
        Opts
    ),
    anchored_timeline(Info, PrevStep, Step, Opts, 500).
anchored_timeline(_Info, _PrevStep, _Step, _Opts, 0) ->
    #{};
anchored_timeline(Info, PrevStep, Step, Opts, Tries) ->
    Held =
        lib_arweave_vdf_timeline:snapshot(
            PrevStep,
            Step,
            Info#nonce_limiter_info.seed,
            Info#nonce_limiter_info.vdf_difficulty,
            Opts
        ),
    case map_size(Held) == Step - PrevStep of
        true -> Held;
        false ->
            timer:sleep(10),
            anchored_timeline(Info, PrevStep, Step, Opts, Tries - 1)
    end.

%% @doc `~arweave-mining@2.9/start' mines continuously from one call: it
%% follows the node's own nonce limiter, searches each step as that step is
%% produced, and keeps going without being asked again.
%%
%% This is the claim a one-shot pass cannot make. `mine' searches a window from
%% the parent and returns; a session subscribes to the timeline and is driven by
%% it, so the steps it searches are the steps that exist, and it searches each
%% of them once. The vector watches the counters move: steps seen, searches
%% dispatched and completed, and how far behind the newest step the newest
%% searched one is.
mines_continuously_test_() ->
    {timeout, 300, fun test_mines_continuously/0}.

test_mines_continuously() ->
    Opts = (mining_opts())#{ <<"arweave-vdf-timeline">> => true },
    {Base, Prev} = mining_chain(Opts),
    Info = Prev#block.nonce_limiter_info,
    PrevStep = Info#nonce_limiter_info.global_step_number,
    % The session starts before the nonce limiter has an anchor, which is the
    % order a node comes up in: there is nothing to mine until a block has been
    % validated, and the miner waits rather than failing.
    {ok, Started} = started(Base, Opts),
    ?assertEqual(true, maps:get(<<"running">>, Started)),
    ?assertEqual(0, maps:get(<<"steps">>, Started)),
    % Anchoring the nonce limiter is what validating a block does. From here it
    % computes, and every step it computes is pushed at the session.
    anchored_timeline(Info, PrevStep, PrevStep + 2, Opts),
    % The session searches without being asked again.
    Searched = mined_until(fun(S) -> maps:get(<<"completed">>, S) > 0 end, Opts),
    ?assert(maps:get(<<"steps">>, Searched) > 0),
    ?assert(maps:get(<<"dispatched">>, Searched) > 0),
    ?assertEqual(
        maps:get(<<"dispatched">>, Searched),
        maps:get(<<"completed">>, Searched) + maps:get(<<"in-flight">>, Searched)
    ),
    ?assert(maps:get(<<"steps">>, Searched) > 1),
    % And it moves onto each new block without being asked. Every block this
    % session produces is validated, and validating a block re-anchors the
    % nonce limiter on it -- so the timeline the session is following moves
    % under it, repeatedly, and the session keeps searching across every one of
    % those moves. That is the jump-forward, exercised by the node's own blocks
    % rather than by a simulated one.
    Mined =
        mined_until(fun(S) -> maps:get(<<"blocks">>, S) > 1 end, Opts),
    ?assert(maps:get(<<"blocks">>, Mined) > 1),
    ?assertEqual(
        maps:get(<<"solutions">>, Mined),
        maps:get(<<"blocks">>, Mined)
    ),
    ?assert(maps:get(<<"steps">>, Mined) >= maps:get(<<"steps">>, Searched)),
    ?assertEqual(0, maps:get(<<"errors">>, Mined)),
    % The timeline it is following is the node's own, anchored on a block the
    % node validated rather than on anything the session asserted.
    ?assert(
        is_integer(
            maps:get(<<"anchored-at">>, maps:get(<<"timeline">>, Mined)))),
    % Stopping leaves the session alive and holding nothing.
    {ok, Stopped} = control(<<"stop">>, Opts),
    ?assertEqual(false, maps:get(<<"running">>, Stopped)),
    ?assertEqual(0, maps:get(<<"queued">>, Stopped)),
    {ok, Idle} = control(<<"status">>, Opts),
    ?assertEqual(false, maps:get(<<"running">>, Idle)).

%% @doc Start the session on a parent, over the vectors' own weave source.
%%
%% The source is built once. Building it packs chunks, which is thirty-two
%% RandomX runs each, so a poll loop that rebuilt it would be measuring the
%% harness rather than the miner.
started(Base, Opts) ->
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave-mining@2.9">> },
        #{
            <<"path">> => <<"start">>,
            <<"parent">> => Base,
            <<"max-nonces">> => ?MINING_NONCES,
            <<"weave">> => mining_source(Opts)
        },
        Opts
    ).

%% @doc Ask the running session something. `stop' and `status' read nothing
%% from the request, so nothing is built to ask them.
control(Path, Opts) ->
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave-mining@2.9">> },
        #{ <<"path">> => Path },
        Opts
    ).

%% @doc Poll the session until it reports what the caller is waiting for. The
%% timeline computes a step at a time and the session is driven by it, so what
%% is true is whatever it has reached when asked.
mined_until(Done, Opts) ->
    mined_until(Done, Opts, 600).
mined_until(_Done, Opts, 0) ->
    {ok, Status} = control(<<"status">>, Opts),
    error({'miner-made-no-progress', Status});
mined_until(Done, Opts, Tries) ->
    {ok, Status} = control(<<"status">>, Opts),
    case Done(Status) of
        true -> Status;
        false ->
            timer:sleep(50),
            mined_until(Done, Opts, Tries - 1)
    end.

%% @doc `~arweave-mining@2.9/mine` composes the search and the producer into
%% one pass, and the block it answers with is one this node's own validation
%% accepts under the `full' profile.
%%
%% Where the vectors above establish `produce' against a solution made by hand,
%% this establishes the whole path: the pass advances the nonce limiter itself,
%% searches the weave through an ordinary `chunk-proof' source, derives the
%% difficulty a solution must beat from the timestamp the block is mined at,
%% and checks what it built before answering with it. A wiring fault between
%% the two devices -- a search run against one step and a block built at
%% another, or against one timestamp and built at another -- is visible here
%% and nowhere else.
mine_yields_a_valid_block_test() ->
    Opts = mining_opts(),
    {Base, _Prev} = mining_chain(Opts),
    {ok, Result} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-mining@2.9">> },
            #{
                <<"path">> => <<"mine">>,
                <<"parent">> => Base,
                <<"steps">> => ?MINING_STEP_LIMIT,
                <<"max-nonces">> => ?MINING_NONCES,
                <<"weave">> => mining_source(Opts)
            },
            Opts
        ),
    ?assertEqual(true, hb_maps:get(<<"mined">>, Result, false, Opts)),
    Block = hb_maps:get(<<"block">>, Result, not_found, Opts),
    assert_valid(Base, Block, [], Opts),
    ?assertEqual(?MINING_HEIGHT + 1, hb_maps:get(<<"height">>, Block, 0, Opts)),
    ?assertEqual(
        hb_util:encode(
            ar_wallet:to_address(hb_opts:get(priv_wallet, [], Opts))),
        hb_maps:get(<<"reward-addr">>, Block, not_found, Opts)
    ).

%% @doc The block a pass answers with is the block it checked, whatever the
%% `arweave-mined-block' handler returns. The hook is where an operator
%% attaches an announcement, not where the block is decided: a pass that
%% answered with a handler's return value would let one substitute a message
%% nothing validated, under a result saying it was mined.
mine_answers_with_the_block_it_checked_test() ->
    Opts = mining_opts(),
    {Base, _Prev} = mining_chain(Opts),
    Substitute =
        #{
            <<"device">> =>
                #{
                    arweave_mined_block =>
                        fun(_Base, _Req, _Opts) ->
                            {ok, #{ <<"height">> => 1 }}
                        end
                }
        },
    {ok, Result} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-mining@2.9">> },
            #{
                <<"path">> => <<"mine">>,
                <<"parent">> => Base,
                <<"steps">> => ?MINING_STEP_LIMIT,
                <<"max-nonces">> => ?MINING_NONCES,
                <<"weave">> => mining_source(Opts)
            },
            Opts#{
                <<"on">> => #{ <<"arweave-mined-block">> => Substitute }
            }
        ),
    Block = hb_maps:get(<<"block">>, Result, not_found, Opts),
    ?assertEqual(?MINING_HEIGHT + 1, hb_maps:get(<<"height">>, Block, 0, Opts)),
    assert_valid(Base, Block, [], Opts).

%% @doc A pass over a weave this node does not hold is not a failure: it
%% answers that it mined nothing, which is what a miner missing the partition
%% it is searching concludes too.
%%
%% The pass is unbounded, which is the shape a caller who names no `max-nonces'
%% gets: every nonce of both ranges is walked and every one of them is a hole,
%% so the whole range is covered without a chunk being read or packed.
mine_without_a_weave_test() ->
    Opts = mining_opts(),
    {Base, _Prev} = mining_chain(Opts),
    ?assertMatch(
        {ok, #{ <<"mined">> := false }},
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-mining@2.9">> },
            #{
                <<"path">> => <<"mine">>,
                <<"parent">> => Base,
                <<"steps">> => 2,
                <<"weave">> => empty_mining_source()
            },
            Opts
        )
    ).

%% @doc A solution mined for an address this node holds no key for is refused,
%% before anything is derived from it.
%%
%% Nothing but the address is read, which is why the solution here carries
%% nothing else: a block is verified against the key its mining address is
%% derived from, so one naming another miner's address is one this node could
%% not sign whatever else it carried.
produce_refuses_a_foreign_solution_test() ->
    Opts = mining_opts(),
    {Base, Prev} = mining_chain(Opts),
    ?assertMatch(
        {error, #{ <<"message">> := <<"unowned-reward-addr">> }},
        produce(
            Base,
            Prev,
            #{
                <<"reward-addr">> =>
                    hb_util:encode(ar_wallet:to_address(ar_wallet:new()))
            },
            [],
            Opts
        )
    ).

%% @doc Each field the producer derives is the one the check that reads it
%% demands: restating any of them makes that check refuse the block, by its own
%% name.
%%
%% Each is put to the one check that owns it rather than to the whole profile,
%% because the signature covers all six: under `full' the identity check
%% reaches four of them before the check that owns them does and answers
%% `invalid-signature', which establishes that they were signed and nothing
%% about where they came from.
produce_derives_the_checked_fields_test() ->
    Opts = mining_opts(),
    {Base, Prev} = mining_chain(Opts),
    {ok, Block} = produce(Base, Prev, mine(one_chunk, Prev, Opts), [], Opts),
    lists:foreach(
        fun({Check, Key, Value, Message}) ->
            ?assertMatch(
                {error, #{ <<"message">> := Message }},
                validate(Base, Block#{ Key => Value }, [], Check, Opts)
            )
        end,
        [
            {<<"fields">>, <<"diff">>, Prev#block.diff + 1,
                <<"invalid-difficulty">>},
            % The parent's own cumulative difficulty, which the block declares
            % beside this one as `previous-cumulative-diff': one block's worth
            % of work short of what it must extend the parent by.
            {<<"fields">>, <<"cumulative-diff">>, Prev#block.cumulative_diff,
                <<"invalid-cumulative-diff">>},
            {<<"block-index">>, <<"hash-list-merkle">>,
                hb_util:encode(crypto:hash(sha384, <<>>)),
                <<"invalid-block-index-root">>},
            {<<"transactions">>, <<"tx-root">>,
                hb_util:encode(crypto:hash(sha256, <<>>)),
                <<"invalid-tx-root">>},
            {<<"transactions">>, <<"weave-size">>, Prev#block.weave_size + 1,
                <<"invalid-weave-size">>},
            {<<"transactions,accounts">>, <<"wallet-list">>,
                hb_util:encode(crypto:hash(sha384, <<>>)),
                <<"invalid-wallet-list-root">>}
        ]
    ).

%% @doc A produced block carries the transactions it was given: the transaction
%% root, the block size and the weave size it declares are the ones they lay
%% down, and the account transition it commits to has spent them.
%%
%% The transfer is between two represented accounts because the admission rules
%% require it: a transaction paying into an account that does not exist is
%% refused as an overspend, whatever the sender's balance.
produce_includes_transactions_test() ->
    Opts = mining_opts(),
    Sender = maps:get(<<"priv-wallet">>, Opts),
    Recipient = crypto:hash(sha256, <<"mining-recipient">>),
    {Base, Prev} =
        mining_chain(
            ?MINING_PARENT_STEP,
            funded_accounts(
                [
                    {ar_wallet:to_address(Sender), 1_000_000_000_000_000},
                    {Recipient, 1_000_000_000}
                ],
                Opts
            ),
            Opts
        ),
    TX = mining_transaction(Sender, Recipient, Prev),
    TXs = [lib_arweave_tx:from_tx(TX, Opts)],
    {ok, Block} =
        produce(Base, Prev, mine(one_chunk, Prev, Opts), TXs, Opts),
    assert_valid(Base, Block, TXs, Opts),
    ?assertEqual([hb_util:encode(TX#tx.id)], maps:get(<<"txs">>, Block)),
    ?assertEqual(?DATA_CHUNK_SIZE, maps:get(<<"block-size">>, Block)),
    ?assertEqual(
        Prev#block.weave_size + ?DATA_CHUNK_SIZE,
        maps:get(<<"weave-size">>, Block)
    ),
    ?assertEqual(
        hb_util:encode(
            ar_block:generate_tx_root_for_block([TX], Prev#block.height + 1)),
        maps:get(<<"tx-root">>, Block)
    ).

%% @doc A block whose nonce limiter interval crosses an entropy reset line
%% carries the rotated seed data, and the steps above the line are computed from
%% its own seed mixed into the output below it.
%%
%% No other vector reaches a reset line: the ordinary parent sits a full
%% interval below the next one. The mixing is the rule of the nonce limiter
%% with the least margin for error -- one hash, in Erlang, at one step -- and
%% here the producer performs it and `~arweave-vdf@2.9/verify-chain' takes its
%% own reset branch to establish that it performed it the same way.
produce_crosses_an_entropy_reset_test() ->
    Opts = mining_opts(),
    {Base, Prev} =
        mining_chain(
            ?MINING_RESET_PARENT_STEP, empty_account_state(Opts), Opts),
    {ok, Block} = produce(Base, Prev, mine(one_chunk, Prev, Opts), [], Opts),
    assert_valid(Base, Block, [], Opts),
    % All four seeds and bounds rotate forward together, and only here.
    PrevInfo = Prev#block.nonce_limiter_info,
    Info = (lib_arweave_block:to(Block, Opts))#block.nonce_limiter_info,
    ?assertEqual(
        {
            PrevInfo#nonce_limiter_info.next_seed,
            Prev#block.indep_hash,
            PrevInfo#nonce_limiter_info.next_partition_upper_bound,
            Prev#block.weave_size
        },
        {
            Info#nonce_limiter_info.seed,
            Info#nonce_limiter_info.next_seed,
            Info#nonce_limiter_info.partition_upper_bound,
            Info#nonce_limiter_info.next_partition_upper_bound
        }
    ).

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

%%% Block production helpers.

%% @doc A store and a mining key of this vector's own. The key is the node's
%% mining identity: it signs the block, and its address is what every sub-chunk
%% the search reads is packed for.
mining_opts() ->
    #{
        <<"store">> => [hb_test_utils:test_store()],
        <<"priv-wallet">> => ar_wallet:new()
    }.

%% @doc The chain state a produced block extends: the parent header, the
%% account tree it left behind, a block index covering the weave below it, and
%% the two histories its child extends.
mining_chain(Opts) ->
    mining_chain(?MINING_PARENT_STEP, empty_account_state(Opts), Opts).
mining_chain(StepNumber, Accounts, Opts) ->
    Prev = mining_parent(StepNumber),
    Base =
        (lib_arweave_block:from(Prev, Opts))#{
            <<"device">> => <<"arweave-block@2.9">>,
            <<"accounts">> => Accounts,
            <<"block-index">> => block_index([index_entry(Prev)], Opts),
            <<"reward-history">> =>
                lib_arweave_history:append(
                    <<"reward-history">>,
                    {Prev#block.reward_addr, 1, Prev#block.reward, 1},
                    Prev#block.height,
                    [],
                    Opts
                ),
            <<"block-time-history">> =>
                lib_arweave_history:append(
                    <<"block-time-history">>,
                    {120, 60, 1},
                    Prev#block.height,
                    [],
                    Opts
                )
        },
    {Base, Prev}.

%% @doc The block a produced block extends, found at the nonce limiter step
%% given -- which is what decides whether its child's search crosses an entropy
%% reset line.
%%
%% Its nonce limiter runs at the vectors' own VDF difficulty so that a search
%% can drive the timeline forward, and its partition upper bound is the whole
%% weave, which is a small fraction of one partition: 0 is therefore the only
%% partition number `check_partition_number/1' admits and the only one a search
%% may use.
mining_parent(StepNumber) ->
    #{ tx_root := TXRoot } = mining_weave(),
    Timestamp = os:system_time(second) - 120,
    #block{
        height = ?MINING_HEIGHT,
        previous_block = crypto:hash(sha384, <<"mining-grandparent">>),
        indep_hash = crypto:hash(sha384, <<"mining-parent">>),
        hash = crypto:hash(sha256, <<"mining-parent-solution">>),
        hash_preimage = crypto:hash(sha256, <<"mining-parent-preimage">>),
        previous_solution_hash =
            crypto:hash(sha256, <<"mining-grandparent-solution">>),
        timestamp = Timestamp,
        last_retarget = Timestamp - 1080,
        nonce = 0,
        diff = 1,
        cumulative_diff = 1,
        previous_cumulative_diff = 0,
        txs = [],
        tx_root = TXRoot,
        block_size = mining_weave_size(),
        weave_size = mining_weave_size(),
        wallet_list = crypto:hash(sha384, <<"mining-parent-wallets">>),
        reward_addr = crypto:hash(sha256, <<"mining-parent-miner">>),
        tags = [],
        reward = 1,
        reward_pool = 1_000_000_000_000,
        hash_list_merkle = crypto:hash(sha384, <<"mining-parent-index">>),
        usd_to_ar_rate = {1, 1},
        scheduled_usd_to_ar_rate = {1, 1},
        packing_2_5_threshold = 0,
        strict_data_split_threshold = ?STRICT_DATA_SPLIT_THRESHOLD,
        merkle_rebase_support_threshold = ?MERKLE_REBASE_SUPPORT_THRESHOLD,
        recall_byte = 0,
        partition_number = 0,
        nonce_limiter_info =
            #nonce_limiter_info{
                output = crypto:hash(sha256, <<"mining-parent-output">>),
                prev_output =
                    crypto:hash(sha256, <<"mining-grandparent-output">>),
                seed = crypto:hash(sha384, <<"mining-seed">>),
                next_seed = crypto:hash(sha384, <<"mining-next-seed">>),
                partition_upper_bound = mining_weave_size(),
                % Deliberately not the weave size, so that a range crossing a
                % reset line moves the bound the recall ranges are drawn from
                % to a value nothing else in the parent already holds.
                next_partition_upper_bound = ?DATA_CHUNK_SIZE,
                global_step_number = StepNumber,
                vdf_difficulty = ?MINING_VDF_DIFFICULTY,
                next_vdf_difficulty = ?MINING_VDF_DIFFICULTY
            },
        price_per_gib_minute = 1_000,
        scheduled_price_per_gib_minute = 1_000,
        reward_history_hash = crypto:hash(sha256, <<"mining-parent-rewards">>),
        block_time_history_hash =
            crypto:hash(sha256, <<"mining-parent-times">>),
        debt_supply = 0,
        kryder_plus_rate_multiplier = 1,
        kryder_plus_rate_multiplier_latch = 0,
        denomination = 1,
        redenomination_height = 0,
        chunk_hash = crypto:hash(sha256, <<"mining-parent-chunk">>),
        packing_difficulty = ?REPLICA_2_9_PACKING_DIFFICULTY,
        replica_format = 1,
        unpacked_chunk_hash = crypto:hash(sha256, <<"mining-parent-unpacked">>)
    }.

%% @doc The weave a produced block's solution is drawn from: one transaction of
%% two whole chunks, in the block below, with the Merkle trees a proof of
%% access walks. Every byte of it is fixed, so the parent header and the search
%% build the same weave without one being handed the other's.
mining_weave() ->
    Indexes = lists:seq(0, ?MINING_CHUNKS - 1),
    {DataRoot, DataTree} =
        ar_merkle:generate_tree(
            [
                {
                    ar_tx:generate_chunk_id(mining_chunk(Index)),
                    (Index + 1) * ?DATA_CHUNK_SIZE
                }
            ||
                Index <- Indexes
            ]
        ),
    {TXRoot, TXTree} =
        ar_merkle:generate_tree([{DataRoot, mining_weave_size()}]),
    #{
        data_root => DataRoot,
        data_tree => DataTree,
        tx_root => TXRoot,
        tx_tree => TXTree
    }.

%% @doc A weave source over the same chunks a produced block is mined from,
%% answering `chunk-proof' exactly as a peer answers `GET /chunk/<offset>'. An
%% offset past the end of the weave is a hole, which is what a node that holds
%% nothing there returns.
mining_source(Opts) ->
    % The chunks are packed once, here, and served by both keys. A pass reads a
    % range at every step it walks, and packing a chunk of them costs
    % thirty-two 8 MiB RandomX blobs -- so a source that packed on demand would
    % spend the whole vector doing it.
    Packed = mining_packed_chunks(Opts),
    #{
        <<"device">> =>
            #{
                range =>
                    fun(_Base, Req, Opts2) ->
                        mining_range(Packed, Req, Opts2)
                    end,
                chunk_proof =>
                    fun(_Base, Req, Opts2) ->
                        mining_chunk_proof(
                            Packed,
                            hb_util:int(
                                hb_maps:get(<<"offset">>, Req, 0, Opts2))
                        )
                    end
            }
    }.

%% @doc A weave source holding nothing at all. It still answers in the packing
%% it was asked for, because holding nothing is not the same thing as holding
%% the wrong thing.
empty_mining_source() ->
    #{
        <<"device">> =>
            #{
                range =>
                    fun(_Base, Req, Opts) ->
                        {ok,
                            #{
                                <<"packing">> =>
                                    hb_maps:get(
                                        <<"packing">>, Req, <<>>, Opts),
                                <<"chunks">> => []
                            }
                        }
                    end,
                chunk_proof => fun(_Base, _Req, _Opts) -> mining_hole() end
            }
    }.

%% @doc Every chunk of the weave, packed for the address the vectors mine to,
%% by the absolute end offset it sits at.
mining_packed_chunks(Opts) ->
    RewardAddr = ar_wallet:to_address(hb_opts:get(priv_wallet, [], Opts)),
    [
        {
            (Index + 1) * ?DATA_CHUNK_SIZE,
            mining_packed_chunk(Index, RewardAddr, Opts)
        }
    ||
        Index <- lists:seq(0, ?MINING_CHUNKS - 1)
    ].

%% @doc One whole chunk in the form a partition holds it: every sub-chunk of it
%% packed for the mining address at the offset it sits at.
mining_packed_chunk(Index, RewardAddr, Opts) ->
    <<
        <<
            (mining_packed_sub_chunk(Index, SubChunk, RewardAddr, Opts))/binary
        >>
    ||
        SubChunk <- lists:seq(0, ?COMPOSITE_PACKING_SUB_CHUNK_COUNT - 1)
    >>.

mining_packed_sub_chunk(Index, SubChunkIndex, RewardAddr, Opts) ->
    {ok, Packed} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-spora@2.9">>,
                <<"chunk">> => hb_util:encode(mining_chunk(Index)),
                <<"sub-chunk-index">> => SubChunkIndex,
                <<"absolute-end-offset">> =>
                    (Index + 1) * ?DATA_CHUNK_SIZE,
                <<"packing">> => mining_packing(RewardAddr)
            },
            <<"pack-sub-chunk">>,
            Opts
        ),
    hb_maps:get(<<"chunk">>, Packed, not_found, Opts).

%% @doc The packing the vectors' weave is held in.
mining_packing(RewardAddr) ->
    #{
        <<"format">> => <<"replica-2-9">>,
        <<"reward-addr">> => hb_util:encode(RewardAddr),
        <<"packing-difficulty">> => ?REPLICA_2_9_PACKING_DIFFICULTY
    }.

%% @doc Answer with the chunks of the weave lying inside a span.
mining_range(Packed, Req, Opts) ->
    Start = hb_util:int(hb_maps:get(<<"range-start">>, Req, 0, Opts)),
    Size =
        ar_block:get_recall_range_size(
            hb_util:int(
                hb_maps:get(<<"packing-difficulty">>, Req, 0, Opts))),
    {ok,
        #{
            <<"packing">> => hb_maps:get(<<"packing">>, Req, <<>>, Opts),
            <<"chunks">> =>
                hb_util:list_to_numbered_message(
                    [
                        #{
                            <<"absolute-end-offset">> => EndOffset,
                            <<"chunk">> => Chunk
                        }
                    ||
                        {EndOffset, Chunk} <- Packed,
                        EndOffset > Start,
                        EndOffset - ?DATA_CHUNK_SIZE < Start + Size
                    ]
                )
        }
    }.

mining_chunk_proof(_Packed, Offset)
        when Offset < 0; Offset >= ?MINING_CHUNKS * ?DATA_CHUNK_SIZE ->
    mining_hole();
mining_chunk_proof(Packed, Offset) ->
    #{
        data_root := DataRoot,
        data_tree := DataTree,
        tx_root := TXRoot,
        tx_tree := TXTree
    } = mining_weave(),
    Index = Offset div ?DATA_CHUNK_SIZE,
    EndOffset = (Index + 1) * ?DATA_CHUNK_SIZE,
    {EndOffset, Chunk} = lists:keyfind(EndOffset, 1, Packed),
    {ok,
        #{
            <<"chunk">> => hb_util:encode(Chunk),
            <<"unpacked-chunk">> => hb_util:encode(mining_chunk(Index)),
            <<"chunk-size">> => ?DATA_CHUNK_SIZE,
            <<"absolute-end-offset">> => EndOffset,
            <<"packing">> => <<"replica-2-9">>,
            <<"tx-path">> =>
                hb_util:encode(
                    ar_merkle:generate_path(TXRoot, Offset, TXTree)),
            <<"data-path">> =>
                hb_util:encode(
                    ar_merkle:generate_path(DataRoot, Offset, DataTree))
        }
    }.

%% @doc The answer a weave source gives for a byte it holds no chunk at.
mining_hole() ->
    {error,
        #{
            <<"status">> => 404,
            <<"message">> => <<"chunk-not-found">>,
            <<"detail">> => <<"This node holds no chunk at that offset.">>
        }
    }.

%% @doc The bytes of one chunk of that weave: a full 256 KiB, so no chunk of it
%% is padded and every sub-chunk of it is data.
mining_chunk(Index) ->
    binary:copy(
        crypto:hash(sha256, <<"mining-chunk-", Index:8>>),
        ?DATA_CHUNK_SIZE div 32
    ).

%% @doc The size of the weave a produced block extends.
mining_weave_size() ->
    ?MINING_CHUNKS * ?DATA_CHUNK_SIZE.

%% @doc Search the timeline for a solution of the kind asked for, advancing the
%% nonce limiter one step at a time and trying each candidate nonce at each
%% step, exactly as a miner does.
%%
%% The difficulty a solution must beat is derived here rather than taken from
%% the producer: at a height that is not a retarget height the child inherits
%% the parent's, and a search and a block that disagreed about it would be the
%% failure this whole vector exists to rule out.
mine(Kind, Prev, Opts) ->
    Info = Prev#block.nonce_limiter_info,
    Step = Info#nonce_limiter_info.global_step_number,
    % A vector's parent either sits a full interval below the next entropy
    % reset line, so no step a search reaches crosses one, or directly below
    % it, so every step does. Nothing in between, which is what lets the
    % rotation the crossing performs be stated once here rather than per step.
    Reset =
        ar_nonce_limiter:get_entropy_reset_point(
            Step, Step + ?MINING_STEP_LIMIT),
    Weave = mining_weave(),
    RewardAddr = ar_wallet:to_address(maps:get(<<"priv-wallet">>, Opts)),
    mine(
        Kind,
        #{
            step => Step + 1,
            output => Info#nonce_limiter_info.output,
            seed => Info#nonce_limiter_info.seed,
            reset => Reset,
            % The seed mixed at a reset line is the block's own, which is the
            % one the parent had scheduled.
            reset_seed => Info#nonce_limiter_info.next_seed,
            reward_addr => RewardAddr,
            partition_upper_bound =>
                rotated(
                    Reset,
                    Info#nonce_limiter_info.partition_upper_bound,
                    Info#nonce_limiter_info.next_partition_upper_bound
                ),
            diff_pair =>
                ar_difficulty:diff_pair(
                    #block{
                        diff = Prev#block.diff,
                        height = Prev#block.height + 1
                    }
                ),
            proofs => mining_proofs(Weave, RewardAddr, Opts)
        },
        Step + ?MINING_STEP_LIMIT,
        Opts
    ).

mine(_Kind, #{ step := Step }, Limit, _Opts) when Step > Limit ->
    error('no-solution-found');
mine(Kind, Session = #{ step := Step, output := Output }, Limit, Opts) ->
    {ok, Next, _Checkpoints} =
        ar_vdf:compute(
            Step,
            timeline_input(Step, Output, Session),
            ?MINING_VDF_DIFFICULTY
        ),
    Advanced = entropy(Session#{ output := Next }, Opts),
    case candidates(Kind, Advanced, lists:seq(0, ?MINING_NONCES - 1), Opts) of
        {ok, Solution} -> Solution;
        none -> mine(Kind, Advanced#{ step := Step + 1 }, Limit, Opts)
    end.

%% @doc The nonce limiter field a block's search draws on: the parent's own
%% below an entropy reset line, and the one the parent scheduled above it.
rotated(none, Held, _Scheduled) ->
    Held;
rotated(_Reset, _Held, Scheduled) ->
    Scheduled.

%% @doc The output a step is computed from: the one below it, with the block's
%% own seed mixed in at an entropy reset line. Both difficulties a vector's
%% parent carries are `?MINING_VDF_DIFFICULTY', so a line moves the entropy and
%% not the cost.
timeline_input(Reset, Output, #{ reset := Reset, reset_seed := Seed }) ->
    ar_nonce_limiter:mix_seed(Output, Seed);
timeline_input(_Step, Output, _Session) ->
    Output.

%% @doc Compute a step's mining entropy and the two recall ranges it selects,
%% through the device that owns both.
entropy(Session, Opts) ->
    #{
        output := Output,
        seed := Seed,
        reward_addr := RewardAddr,
        partition_upper_bound := UpperBound
    } = Session,
    {ok, Entropy} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-spora@2.9">>,
                <<"nonce-limiter-output">> => hb_util:encode(Output),
                <<"partition-number">> => 0,
                <<"seed">> => hb_util:encode(Seed),
                <<"reward-addr">> => hb_util:encode(RewardAddr),
                <<"packing-difficulty">> => ?REPLICA_2_9_PACKING_DIFFICULTY
            },
            <<"h0">>,
            Opts
        ),
    H0 = hb_maps:get(<<"h0">>, Entropy, <<>>, Opts),
    {ok, Ranges} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-spora@2.9">>,
                <<"h0">> => H0,
                <<"partition-number">> => 0,
                <<"partition-upper-bound">> => UpperBound
            },
            <<"recall-range">>,
            Opts
        ),
    Session#{
        h0 => H0,
        range1 => hb_util:int(hb_maps:get(<<"range1-start">>, Ranges, 0, Opts)),
        range2 => hb_util:int(hb_maps:get(<<"range2-start">>, Ranges, 0, Opts))
    }.

%% @doc Try each candidate nonce at one step, stopping at the first solution
%% the difficulty check accepts.
candidates(_Kind, _Session, [], _Opts) ->
    none;
candidates(Kind, Session, [Nonce | Nonces], Opts) ->
    case candidate(Kind, Session, Nonce, Opts) of
        {ok, Solution} -> {ok, Solution};
        none -> candidates(Kind, Session, Nonces, Opts)
    end.

%% @doc Hash one nonce's sub-chunk and answer with a solution if the result
%% beats the difficulty its solution type is held to. A one-chunk solution pays
%% the hundredfold proof-of-access multiplier, which is why the two are searched
%% for separately rather than one being taken whenever it turns up.
candidate(one_chunk, Session, Nonce, Opts) ->
    #{ h0 := H0, range1 := Range1, diff_pair := DiffPair } = Session,
    RecallByte = recall_byte(Range1, Nonce, Opts),
    PoA = recalled(RecallByte, Nonce, Session),
    {H1, Preimage} = solution_hash(<<"h1">>, h1_request(H0, Nonce, PoA), Opts),
    case
        ar_node_utils:h1_passes_diff_check(
            hb_util:decode(H1), DiffPair, ?REPLICA_2_9_PACKING_DIFFICULTY)
    of
        true ->
            {ok,
                solution(Session, Nonce, H1, Preimage,
                    #{ <<"recall-byte">> => RecallByte, <<"poa">> => PoA })};
        false ->
            none
    end;
candidate(two_chunk, Session, Nonce, Opts) ->
    #{ h0 := H0, range1 := Range1, range2 := Range2, diff_pair := DiffPair } =
        Session,
    RecallByte = recall_byte(Range1, Nonce, Opts),
    RecallByte2 = recall_byte(Range2, Nonce, Opts),
    case mining_chunk_index(RecallByte) == mining_chunk_index(RecallByte2) of
        true ->
            % Both ranges fall in the one chunk, so the second proof would be
            % the first one restated and would establish nothing.
            none;
        false ->
            PoA = recalled(RecallByte, Nonce, Session),
            PoA2 = recalled(RecallByte2, Nonce, Session),
            {H1, _Preimage1} =
                solution_hash(<<"h1">>, h1_request(H0, Nonce, PoA), Opts),
            {H2, Preimage2} =
                solution_hash(<<"h2">>,
                    #{
                        <<"h0">> => H0,
                        <<"h1">> => H1,
                        <<"chunk">> => maps:get(<<"chunk">>, PoA2)
                    },
                    Opts),
            case
                ar_node_utils:h2_passes_diff_check(
                    hb_util:decode(H2), DiffPair,
                    ?REPLICA_2_9_PACKING_DIFFICULTY)
            of
                true ->
                    {ok,
                        solution(Session, Nonce, H2, Preimage2,
                            #{
                                <<"recall-byte">> => RecallByte,
                                <<"poa">> => PoA,
                                <<"recall-byte2">> => RecallByte2,
                                <<"poa2">> => PoA2
                            })};
                false ->
                    none
            end
    end.

%% @doc The request `~arweave-spora@2.9/h1' takes over a nonce's packed
%% sub-chunk.
h1_request(H0, Nonce, PoA) ->
    #{
        <<"h0">> => H0,
        <<"nonce">> => Nonce,
        <<"chunk">> => maps:get(<<"chunk">>, PoA)
    }.

%% @doc The solution message a search answers with, in the shape
%% `~arweave-block@2.9/produce' takes.
solution(Session, Nonce, Hash, Preimage, Recalled) ->
    #{ step := Step, output := Output, reward_addr := RewardAddr } = Session,
    maps:merge(
        #{
            <<"solution-hash">> => Hash,
            <<"hash-preimage">> => Preimage,
            <<"nonce">> => Nonce,
            <<"partition-number">> => 0,
            <<"packing-difficulty">> => ?REPLICA_2_9_PACKING_DIFFICULTY,
            <<"replica-format">> => 1,
            <<"reward-addr">> => hb_util:encode(RewardAddr),
            <<"nonce-limiter-output">> => hb_util:encode(Output),
            <<"global-step-number">> => Step
        },
        Recalled
    ).

%% @doc Pack the sub-chunk every candidate nonce addresses and pair each with
%% the two Merkle paths that prove it. One pass rather than one per attempt:
%% packing an 8 KiB sub-chunk costs an 8 MiB RandomX entropy blob, and a search
%% revisits the same sub-chunks at every step.
mining_proofs(Weave, RewardAddr, Opts) ->
    maps:from_list(
        [
            {
                {ChunkIndex, SubChunkIndex},
                mining_proof(ChunkIndex, SubChunkIndex, Weave, RewardAddr, Opts)
            }
        ||
            ChunkIndex <- lists:seq(0, ?MINING_CHUNKS - 1),
            SubChunkIndex <- lists:seq(0, ?MINING_NONCES - 1)
        ]
    ).

%% @doc One proof of access: the paths from the transaction root to the chunk,
%% the 8 KiB of it packed for the mining address, and the whole unpacked chunk
%% the proof carries beside it.
mining_proof(ChunkIndex, SubChunkIndex, Weave, RewardAddr, Opts) ->
    #{
        data_root := DataRoot,
        data_tree := DataTree,
        tx_root := TXRoot,
        tx_tree := TXTree
    } = Weave,
    Offset = ChunkIndex * ?DATA_CHUNK_SIZE,
    {ok, Packed} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-spora@2.9">>,
                <<"chunk">> => hb_util:encode(mining_chunk(ChunkIndex)),
                <<"sub-chunk-index">> => SubChunkIndex,
                <<"absolute-end-offset">> => Offset + ?DATA_CHUNK_SIZE,
                <<"packing">> =>
                    #{
                        <<"format">> => <<"replica-2-9">>,
                        <<"reward-addr">> => hb_util:encode(RewardAddr),
                        <<"packing-difficulty">> =>
                            ?REPLICA_2_9_PACKING_DIFFICULTY
                    }
            },
            <<"pack-sub-chunk">>,
            Opts
        ),
    #{
        <<"tx-path">> =>
            hb_util:encode(ar_merkle:generate_path(TXRoot, Offset, TXTree)),
        <<"data-path">> =>
            hb_util:encode(ar_merkle:generate_path(DataRoot, Offset, DataTree)),
        <<"chunk">> =>
            hb_util:encode(hb_maps:get(<<"chunk">>, Packed, <<>>, Opts)),
        <<"unpacked-chunk">> =>
            hb_util:encode(
                hb_maps:get(<<"unpacked-chunk">>, Packed, <<>>, Opts))
    }.

%% @doc The proof of access for the sub-chunk a nonce addresses at a recall
%% byte.
recalled(RecallByte, Nonce, #{ proofs := Proofs }) ->
    maps:get(
        {
            mining_chunk_index(RecallByte),
            ar_block:get_sub_chunk_index(
                ?REPLICA_2_9_PACKING_DIFFICULTY, Nonce)
        },
        Proofs
    ).

%% @doc The chunk of the weave a recall byte falls in.
mining_chunk_index(RecallByte) ->
    RecallByte div ?DATA_CHUNK_SIZE.

%% @doc The byte a nonce recalls from a range, from the device that owns the
%% arithmetic the proof of access check repeats.
recall_byte(RangeStart, Nonce, Opts) ->
    {ok, Recalled} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-spora@2.9">>,
                <<"range-start">> => RangeStart,
                <<"nonce">> => Nonce,
                <<"packing-difficulty">> => ?REPLICA_2_9_PACKING_DIFFICULTY
            },
            <<"recall-byte">>,
            Opts
        ),
    hb_util:int(hb_maps:get(<<"recall-byte">>, Recalled, 0, Opts)).

%% @doc Compute one of the two solution hashes over a packed sub-chunk.
solution_hash(Key, Request, Opts) ->
    {ok, Result} =
        hb_ao:resolve(
            Request#{ <<"device">> => <<"arweave-spora@2.9">> },
            Key,
            Opts
        ),
    {
        hb_maps:get(<<"hash">>, Result, <<>>, Opts),
        hb_maps:get(<<"preimage">>, Result, <<>>, Opts)
    }.

%% @doc Resolve `produce' against the state the block extends, over the
%% transactions it includes and at the timestamp the search derived the
%% difficulty it beat from.
produce(Base, Prev, Solution, TXs, Opts) ->
    hb_ao:resolve(
        Base,
        #{
            <<"path">> => <<"produce">>,
            <<"solution">> => Solution,
            <<"transactions">> => TXs,
            <<"timestamp">> => Prev#block.timestamp + 120
        },
        Opts
    ).

%% @doc Resolve `validate' for a produced block and the transactions it carries,
%% under the whole profile or under the checks named.
validate(Base, Block, TXs, Opts) ->
    hb_ao:resolve(
        Base,
        #{
            <<"path">> => <<"validate">>,
            <<"next">> => Block,
            <<"transactions">> => TXs
        },
        Opts
    ).
validate(Base, Block, TXs, Verify, Opts) ->
    hb_ao:resolve(
        Base,
        #{
            <<"path">> => <<"validate">>,
            <<"verify">> => Verify,
            <<"next">> => Block,
            <<"transactions">> => TXs
        },
        Opts
    ).

%% @doc Assert that a block validates under every check the `full' profile
%% names. Equality with the whole set is the assertion rather than `valid'
%% alone: `ran/2' strips the account check from what it reports when the block
%% carries no tree, so a shorter list would mean the transition it declares was
%% never checked.
assert_valid(Base, Block, TXs, Opts) ->
    {ok, Full} = lib_arweave_block:selected(#{}, Opts),
    ?assertEqual(
        {ok, #{ <<"valid">> => true, <<"checks">> => Full }},
        without_priv(validate(Base, Block, TXs, Opts), Opts)
    ).

%% @doc An account tree holding the accounts a produced block's transactions
%% spend from and pay into.
funded_accounts(Credits, Opts) ->
    {ok, Inserted} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-wallets@2.9">> },
            #{
                <<"path">> => <<"insert">>,
                <<"accounts">> =>
                    maps:from_list(
                        [
                            {
                                hb_util:encode(Address),
                                lib_arweave_accounts:account_message(
                                    {Balance, <<>>})
                            }
                        ||
                            {Address, Balance} <- Credits
                        ]
                    )
            },
            Opts
        ),
    hb_util:ok(
        hb_ao:resolve(Inserted, #{ <<"path">> => <<"finalize">> }, Opts)).

%% @doc A signed transfer of one chunk's worth of data, anchored on the block
%% being extended and paying far above the fee the parent's storage price sets.
mining_transaction(Wallet, Recipient, Prev) ->
    ar_tx:sign(
        #tx{
            format = 2,
            anchor = Prev#block.indep_hash,
            target = Recipient,
            quantity = 1_000,
            reward = 1_000_000_000_000,
            data_size = ?DATA_CHUNK_SIZE,
            data_root = crypto:hash(sha256, <<"mining-transaction-data">>)
        },
        Wallet
    ).
