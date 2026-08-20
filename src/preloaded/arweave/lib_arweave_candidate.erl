%%% @doc The block a mining solution and the block it extends determine: the
%%% producer side of `~arweave-block@2.9'.
%%%
%%% `produce/3' is the inverse of `dev_arweave_block:apply/3'. Every field it
%%% fills is one a check recomputes, so the block it returns is one this node's
%%% own validation accepts under the `full' profile -- and a field derived here
%%% differently from the way it is checked there is a node that mines blocks it
%%% will not accept.
%%%
%%% That is why the derivations the two sides share live here rather than
%%% inside the checks that read them: `balances/5', `transition/3',
%%% `accounts/4', `block_size/1' and `prices/2' each have one implementation,
%%% called by the producer to fill a field and by the check to recompute it.
%%% Everything else the child is determined to carry is derived by a vendored
%%% function that already produces rather than checks.
%%%
%%% `parameters/4' and `nonce_limiter/3' are the seam a miner searches against.
%%% The difficulty a solution must meet follows from the block's timestamp
%%% through the retarget rule, and the entropy it is mined against follows from
%%% walking the nonce limiter forward, so a search run against either of them
%%% derived separately and a block built from these are two derivations of one
%%% consensus value. A miner takes both, searches, and hands the solution and
%%% the same timestamp back.
-module(lib_arweave_candidate).
-export([produce/3, parameters/4, nonce_limiter/3]).
-export([accounts/4, balances/5, block_size/1, prices/2, transition/3]).
-include("include/hb.hrl").
-include("include/ar_consensus.hrl").

%%% The only replication format legal at or above the 2.9 fork, and the packing
%%% difficulty it obliges. `ar_block:validate_replica_format/3' admits no other
%%% pair at this height.
-define(REPLICA_FORMAT, 1).

%% @doc Build the signed block a solution extends this chain state with.
%%
%% The solution names the entropy it was mined against and the two proofs it
%% was mined from; everything else follows from the block being extended, the
%% transactions the block carries and the timestamp it is mined at.
produce(State, Req, Opts) ->
    Prev = lib_arweave_state:previous_block(State, Opts),
    Accounts = lib_arweave_state:accounts(State, Opts),
    % The signing key is the node's own. A private key does not travel in an
    % AO-Core message, so it is read from the node's options and never from the
    % request.
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    maybe
        {ok, Solution} ?= solution(Req, Opts),
        ok ?= mining_address(Solution, Wallet, Opts),
        StepNumber = int(<<"global-step-number">>, Solution, Opts),
        {ok, Parameters} ?=
            parameters(Prev, StepNumber, requested_timestamp(Req, Opts), Opts),
        {ok, Info} ?= interval(Prev, StepNumber, Opts),
        ok ?=
            solution_output(
                Info#nonce_limiter_info.output,
                decode(<<"nonce-limiter-output">>, Solution, Opts)
            ),
        TXs = transactions(Req, Opts),
        Candidate =
            weave(
                Prev,
                candidate(Prev, Solution, Parameters, Info, TXs, Wallet, Opts)
            ),
        {ok, Endowed} ?= endowment(Accounts, Prev, Candidate, TXs, Opts),
        {ok, header(sign(histories(Endowed, Prev), Prev, Wallet), Opts)}
    end.

%% @doc The values a child of this block is determined to carry before a
%% solution exists: the height and timestamp it is mined at, the difficulty its
%% solution hash must beat, and the entropy the search draws its recall ranges
%% from.
%%
%% The timestamp answered is the one the block carries -- the caller's, raised
%% to the floor the parent's own timestamp and the network's clock tolerance
%% put it at -- because that is the value the retarget rule derived the
%% difficulty from.
%%
%% `seed' is the parent's, not the child's: `dev_arweave_block:solution_h0/3'
%% computes the mining entropy under the seed of the block being extended,
%% while the child's own seed is what the nonce limiter mixes at a reset line.
%% `partition-upper-bound' is the child's, since that is what selects the
%% recall ranges -- and it bounds the partition a solution may come from, which
%% on any weave below one partition leaves 0 as the only legal choice.
parameters(Prev, StepNumber, Requested, Opts) ->
    Height = Prev#block.height + 1,
    Timestamp =
        max(
            Requested,
            Prev#block.timestamp - ar_block:get_max_timestamp_deviation()
        ),
    {_PoA1Diff, Diff} =
        ar_retarget:maybe_retarget(
            Height,
            ar_difficulty:diff_pair(Prev),
            Timestamp,
            Prev#block.last_retarget,
            Prev#block.timestamp
        ),
    Info = Prev#block.nonce_limiter_info,
    maybe
        {ok, SeedData} ?= seed_data(Prev, StepNumber, Opts),
        {ok,
            #{
                <<"height">> => Height,
                <<"timestamp">> => Timestamp,
                <<"last-retarget">> => last_retarget(Height, Timestamp, Prev),
                <<"diff">> => Diff,
                <<"cumulative-diff">> =>
                    ar_difficulty:next_cumulative_diff(
                        Prev#block.cumulative_diff, Diff, Height),
                <<"partition-upper-bound">> =>
                    int(<<"partition-upper-bound">>, SeedData, Opts),
                <<"seed">> =>
                    hb_util:encode(Info#nonce_limiter_info.seed),
                <<"packing-difficulty">> => ?REPLICA_2_9_PACKING_DIFFICULTY,
                <<"replica-format">> => ?REPLICA_FORMAT
            }
        }
    end.

%% @doc The nonce limiter info a block mined at `StepNumber' declares, as the
%% message `~arweave-vdf@2.9' and the block both carry it.
%%
%% This is the one forward walk of the timeline. Its `steps' are the outputs of
%% every step from the parent's to this one, newest first, so a miner searching
%% a step takes its entropy from the head and the block it goes on to produce
%% carries this same computation rather than a second one.
nonce_limiter(Prev, StepNumber, Opts) ->
    maybe
        {ok, Info} ?= interval(Prev, StepNumber, Opts),
        {ok, lib_arweave_block:from_nonce_limiter(Info, Opts)}
    end.

%%% Derivations shared with the checks that recompute them.

%% @doc The bytes a block's transactions add to the weave.
block_size(Next) ->
    lists:foldl(
        fun(TX, Size) ->
            Size + ar_tx:get_weave_size_increase(TX, Next#block.height)
        end,
        0,
        Next#block.txs
    ).

%% @doc The two storage prices a block extending this one carries, in the
%% denomination it declares.
prices(Prev, Denomination) ->
    {Price, ScheduledPrice} = ar_pricing:recalculate_price_per_gib_minute(Prev),
    PrevDenomination = Prev#block.denomination,
    {
        ar_pricing:redenominate(Price, PrevDenomination, Denomination),
        ar_pricing:redenominate(ScheduledPrice, PrevDenomination, Denomination)
    }.

%% @doc Load the accounts a block's transition reads, in the vendored form.
%%
%% The set is the one upstream assembles: the mining address, every sender and
%% recipient of the block's transactions, the address whose locked reward this
%% block releases, and the address a double signing proof bans. An address the
%% transition would read but that is not fetched reads as absent rather than as
%% its real balance, so this set is part of the consensus rule rather than an
%% optimisation.
balances(Accounts, Next, Prev, TXs, Opts) ->
    Addresses =
        lists:usort(
            [
                hb_util:encode(Next#block.reward_addr),
                hb_util:encode(ar_rewards:get_oldest_locked_address(Prev))
            ]
            ++ [
                % The record's own field, not `ar_tx:get_owner_address/1'.
                % That function answers the atom `not_set' when the owner is
                % 512 zero bytes -- no RSA modulus, and so a transaction whose
                % signature cannot verify -- and the encoder would raise on it.
                % No such transaction reaches here: the transaction check
                % refuses it, it runs before this one, and `checks/0' names it
                % as one this check reads from, so a set that asks for this
                % without it is refused rather than run.
                hb_util:encode(TX#tx.owner_address)
            ||
                TX <- Next#block.txs
            ]
            ++ [
                hb_maps:get(<<"target">>, TX, <<>>, Opts)
            ||
                TX <- TXs
            ]
            ++ banned_addresses(Next)
        ),
    maybe
        {ok, Loaded} ?=
            hb_ao:resolve(
                Accounts,
                #{
                    <<"path">> => <<"get">>,
                    <<"addresses">> => Addresses
                },
                Opts
            ),
        % Every resolved result carries the resolver's own private section
        % alongside the device's keys. Folding over it unreset would read
        % `priv' as an address, decode it to three bytes of nothing and insert
        % a phantom account -- which the account tree would then hash into a
        % root no block ever signed.
        {ok,
            hb_maps:fold(
                fun(Address, Account, Balances) ->
                    Balances#{
                        hb_util:decode(Address) =>
                            lib_arweave_accounts:account(Account, Opts)
                    }
                end,
                #{},
                hb_private:reset(Loaded),
                Opts
            )
        }
    end.

%% @doc Run the account transition a block performs, and return both the five
%% endowment values it produces -- in the block's own denomination -- and the
%% accounts it changed.
%%
%% Every input is a field the block carries before its reward is known:
%% `ar_node_utils:update_accounts/3' reads the parent's pool and prices and the
%% child's mining address, weave size, height, timestamp and transactions, and
%% none of the five values it derives.
transition(Next, Prev, Balances) ->
    maybe
        {ok, Applied} ?= update_accounts(Next, Prev, Balances),
        % `ar_node_utils:update_accounts/3' takes its endowment arguments as
        % `{MinerReward, EndowmentPool, ...}' and returns them as
        % `{EndowmentPool, MinerReward, ...}'. The two are unequal in every
        % real block, so a transposition here is silent until the root fails.
        {EndowmentPool, MinerReward, DebtSupply, Latch, Multiplier, Updated} =
            Applied,
        Denomination = Prev#block.denomination,
        Denomination2 = Next#block.denomination,
        {ok,
            {
                ar_pricing:redenominate(
                    EndowmentPool, Denomination, Denomination2),
                ar_pricing:redenominate(
                    MinerReward, Denomination, Denomination2),
                ar_pricing:redenominate(
                    DebtSupply, Denomination, Denomination2),
                Latch,
                Multiplier
            },
            maps:filter(
                fun(Address, Account) ->
                    maps:get(Address, Balances, not_found) =/= Account
                end,
                Updated
            )
        }
    end.

%% @doc Insert the accounts a transition changed into the tree and return the
%% state that produces. `Expected' is the root the block signed, and `[]' asks
%% for the root the insertion reaches rather than an assertion about it -- which
%% is the difference between checking a block's `wallet-list' and producing one.
accounts(Accounts, Updated, Expected, Opts) ->
    hb_ao:resolve(
        Accounts,
        #{
            <<"path">> => <<"apply">>,
            <<"diff">> =>
                maps:fold(
                    fun(Address, Account, Diff) ->
                        Diff#{
                            hb_util:encode(Address) =>
                                lib_arweave_accounts:account_message(Account)
                        }
                    end,
                    #{},
                    Updated
                ),
            <<"expected-root">> => Expected
        },
        Opts
    ).

%%% Internal functions.

%% @doc Read the solution the block is built from.
solution(Req, Opts) ->
    case hb_maps:get(<<"solution">>, Req, not_found, Opts) of
        not_found ->
            {error,
                request_error(<<"missing-solution">>,
                    <<"The request must name the mining solution to build a "
                        "block from as `solution'.">>)};
        Solution ->
            {ok, hb_cache:ensure_loaded(Solution, Opts)}
    end.

%% @doc Refuse a solution mined for an address this node holds no key for. The
%% signature is verified against the key the block's mining address is derived
%% from, so a block signed by any other key is one nothing accepts.
mining_address(Solution, Wallet, Opts) ->
    mining_address(
        decode(<<"reward-addr">>, Solution, Opts),
        ar_wallet:to_address(Wallet)
    ).
mining_address(Address, Address) ->
    ok;
mining_address(_Address, _Own) ->
    {error,
        request_error(<<"unowned-reward-addr">>,
            <<"The solution names a mining address this node holds no key "
                "for, and a block must be signed by the key its mining "
                "address is derived from.">>)}.

%% @doc The timestamp the caller asks the block to be mined at, defaulting to
%% this node's clock. `parameters/4' raises it to the protocol's floor, since
%% that is where the difficulty is derived from it.
requested_timestamp(Req, Opts) ->
    hb_util:int(
        hb_maps:get(<<"timestamp">>, Req, os:system_time(second), Opts)).

%% @doc The transaction bodies the block includes, in the order it includes
%% them. They stay `tx@1.0' messages: the account transition reads a recipient
%% off the message rather than off the record, which does not carry one.
transactions(Req, Opts) ->
    hb_util:message_to_ordered_list(
        hb_maps:get(<<"transactions">>, Req, [], Opts),
        Opts
    ).

%% @doc The retarget timestamp: the block's own at a retarget height and the
%% parent's everywhere else.
last_retarget(Height, Timestamp, Prev) ->
    case ar_retarget:is_retarget_height(Height) of
        true -> Timestamp;
        false -> Prev#block.last_retarget
    end.

%% @doc The five rotating nonce limiter fields the parent and the block's own
%% step number determine, from the device that owns them.
seed_data(Prev, StepNumber, Opts) ->
    hb_ao:resolve(
        #{
            <<"device">> => <<"arweave-vdf@2.9">>,
            <<"step-number">> => StepNumber,
            <<"prev-nonce-limiter-info">> =>
                lib_arweave_block:from_nonce_limiter(
                    Prev#block.nonce_limiter_info, Opts),
            <<"prev-indep-hash">> => hb_util:encode(Prev#block.indep_hash),
            <<"prev-weave-size">> => Prev#block.weave_size
        },
        <<"seed-data">>,
        Opts
    ).

%% @doc Build the nonce limiter info a block mined at `StepNumber' declares,
%% running the VDF forward from the parent's output to that step.
%%
%% The head of `steps' is the entropy a solution found at this step is mined
%% against, so a miner searching a step draws it from here and the block it
%% produces carries this same computation rather than a second one.
interval(Prev, StepNumber, Opts) ->
    PrevInfo = Prev#block.nonce_limiter_info,
    maybe
        {ok, SeedData} ?= seed_data(Prev, StepNumber, Opts),
        Info =
            #nonce_limiter_info{
                global_step_number = StepNumber,
                seed = decode(<<"seed">>, SeedData, Opts),
                next_seed = decode(<<"next-seed">>, SeedData, Opts),
                partition_upper_bound =
                    int(<<"partition-upper-bound">>, SeedData, Opts),
                next_partition_upper_bound =
                    int(<<"next-partition-upper-bound">>, SeedData, Opts),
                vdf_difficulty = int(<<"vdf-difficulty">>, SeedData, Opts),
                next_vdf_difficulty =
                    ar_block:compute_next_vdf_difficulty(Prev)
            },
        Computed = vdf(Info, PrevInfo, Opts),
        [{Output, Buffer} | _] = Computed,
        {ok,
            Info#nonce_limiter_info{
                output = Output,
                prev_output = PrevInfo#nonce_limiter_info.output,
                steps =
                    lists:sublist(
                        [ Step || {Step, _Buffer} <- Computed ],
                        ?NONCE_LIMITER_MAX_CHECKPOINTS_COUNT
                    ),
                last_step_checkpoints =
                    [Output | ar_vdf:checkpoint_buffer_to_checkpoints(Buffer)]
            }
        }
    end.

%% @doc Refuse a solution mined against entropy the parent's timeline does not
%% reach at the step the solution names. Such a solution is one this node found
%% on another timeline, and every proof it carries would be checked against
%% recall ranges the block does not select.
solution_output(Output, Output) ->
    ok;
solution_output(_Output, _Claimed) ->
    {error,
        request_error(<<"invalid-solution-output">>,
            <<"The nonce limiter output the solution was mined against is not "
                "the one the parent's timeline reaches at its step "
                "number.">>)}.

%% @doc Every VDF step between the parent's and the block's, newest first, each
%% with the checkpoint buffer it was produced with.
%%
%% The inverse of `~arweave-vdf@2.9/verify-chain': a range crossing an entropy
%% reset line mixes the block's own seed into the input of the step at the
%% line, exactly once, and computes that step and every one above it at the
%% block's VDF difficulty rather than the parent's.
%%
%% The node's own nonce limiter has usually done this already. When it is
%% running it is anchored on the tip's validated output and computes forward
%% from there at close to real time, on a kernel it has self-tested against
%% `ar_vdf:compute/3' at the difficulty in use -- so the steps a block extending
%% that tip declares are steps it already holds. Taking them is not a shortcut
%% past any rule: the timeline offers a step only under the seed and difficulty
%% it computed it with, the nonce limiter is one chain shared by every block at
%% a step, and what is built from them goes through `verify-chain' before this
%% node will answer with it.
%%
%% Everything it does not hold is computed here, step by step, so a range it
%% stopped short of -- an epoch it has not been re-anchored past, a difficulty
%% that has since retargeted, a timeline that is not running at all -- costs
%% what it always did rather than failing.
vdf(Info, PrevInfo, Opts) ->
    StepNumber = Info#nonce_limiter_info.global_step_number,
    PrevStepNumber = PrevInfo#nonce_limiter_info.global_step_number,
    vdf(
        PrevStepNumber + 1,
        StepNumber,
        ar_nonce_limiter:get_entropy_reset_point(PrevStepNumber, StepNumber),
        Info,
        PrevInfo,
        PrevInfo#nonce_limiter_info.output,
        held(PrevStepNumber, StepNumber, Info, Opts),
        []
    ).

vdf(Step, StepNumber, _Reset, _Info, _PrevInfo, _Output, _Held, Computed)
        when Step > StepNumber ->
    Computed;
vdf(Step, StepNumber, Reset, Info, PrevInfo, Output, Held, Computed) ->
    Input =
        case Step of
            Reset ->
                ar_nonce_limiter:mix_seed(
                    Output, Info#nonce_limiter_info.seed);
            _ ->
                Output
        end,
    Difficulty =
        case Reset =/= none andalso Step >= Reset of
            true -> Info#nonce_limiter_info.vdf_difficulty;
            false -> PrevInfo#nonce_limiter_info.vdf_difficulty
        end,
    {Next, Buffer} =
        step(maps:get(Step, Held, not_found), Step, Input, Difficulty),
    vdf(
        Step + 1,
        StepNumber,
        Reset,
        Info,
        PrevInfo,
        Next,
        Held,
        [{Next, Buffer} | Computed]
    ).

%% @doc One step of the interval: the one the node's nonce limiter already ran,
%% or the one this computes now.
step(not_found, Step, Input, Difficulty) ->
    {ok, Output, Buffer} = ar_vdf:compute(Step, Input, Difficulty),
    {Output, Buffer};
step(Held, _Step, _Input, _Difficulty) ->
    Held.

%% @doc The steps of the interval the node's nonce limiter is already holding.
%% It answers with none unless it is running, and none for a seed or difficulty
%% it did not compute under.
held(PrevStepNumber, StepNumber, Info, Opts) ->
    case hb_opts:get(arweave_vdf_timeline, false, Opts) of
        true ->
            lib_arweave_vdf_timeline:snapshot(
                PrevStepNumber,
                StepNumber,
                Info#nonce_limiter_info.seed,
                Info#nonce_limiter_info.vdf_difficulty,
                Opts
            );
        _ ->
            #{}
    end.

%% @doc The block the solution and its parent determine, less the weave
%% arithmetic, the values the account transition produces, the two history
%% hashes and the signature.
candidate(Prev, Solution, Parameters, Info, TXs, Wallet, Opts) ->
    Height = int(<<"height">>, Parameters, Opts),
    Records = [ lib_arweave_tx:to_tx(TX, Opts) || TX <- TXs ],
    PoA = proof(hb_maps:get(<<"poa">>, Solution, not_found, Opts), Opts),
    PoA2 = proof(hb_maps:get(<<"poa2">>, Solution, not_found, Opts), Opts),
    RecallByte2 = recall_byte2(Solution, Opts),
    {Denomination, RedenominationHeight} = ar_pricing:may_be_redenominate(Prev),
    {Rate, ScheduledRate} = ar_pricing:recalculate_usd_to_ar_rate(Prev),
    {Price, ScheduledPrice} = prices(Prev, Denomination),
    lib_arweave_block:with_transactions(
        #block{
            nonce = int(<<"nonce">>, Solution, Opts),
            previous_block = Prev#block.indep_hash,
            timestamp = int(<<"timestamp">>, Parameters, Opts),
            last_retarget = int(<<"last-retarget">>, Parameters, Opts),
            diff = int(<<"diff">>, Parameters, Opts),
            height = Height,
            hash = decode(<<"solution-hash">>, Solution, Opts),
            tx_root = ar_block:generate_tx_root_for_block(Records, Height),
            reward_addr = decode(<<"reward-addr">>, Solution, Opts),
            tags = [],
            cumulative_diff = int(<<"cumulative-diff">>, Parameters, Opts),
            hash_list_merkle =
                ar_unbalanced_merkle:root(
                    Prev#block.hash_list_merkle,
                    {
                        Prev#block.indep_hash,
                        Prev#block.weave_size,
                        Prev#block.tx_root
                    },
                    fun ar_unbalanced_merkle:hash_block_index_entry/1
                ),
            poa = PoA,
            usd_to_ar_rate = Rate,
            scheduled_usd_to_ar_rate = ScheduledRate,
            packing_2_5_threshold =
                ar_block:get_packing_threshold(
                    Prev, int(<<"partition-upper-bound">>, Parameters, Opts)),
            strict_data_split_threshold =
                Prev#block.strict_data_split_threshold,
            hash_preimage = decode(<<"hash-preimage">>, Solution, Opts),
            recall_byte = int(<<"recall-byte">>, Solution, Opts),
            previous_solution_hash = Prev#block.hash,
            partition_number = int(<<"partition-number">>, Solution, Opts),
            nonce_limiter_info = Info,
            poa2 = PoA2,
            recall_byte2 = RecallByte2,
            reward_key =
                ar_block:get_reward_key(ar_wallet:to_pubkey(Wallet), Height),
            price_per_gib_minute = Price,
            scheduled_price_per_gib_minute = ScheduledPrice,
            denomination = Denomination,
            redenomination_height = RedenominationHeight,
            previous_cumulative_diff = Prev#block.cumulative_diff,
            merkle_rebase_support_threshold =
                Prev#block.merkle_rebase_support_threshold,
            chunk_hash = crypto:hash(sha256, PoA#poa.chunk),
            chunk2_hash = chunk_hash(RecallByte2, PoA2),
            packing_difficulty = int(<<"packing-difficulty">>, Solution, Opts),
            unpacked_chunk_hash = crypto:hash(sha256, PoA#poa.unpacked_chunk),
            unpacked_chunk2_hash = unpacked_chunk_hash(RecallByte2, PoA2),
            replica_format = int(<<"replica-format">>, Solution, Opts)
        },
        Records
    ).

%% @doc Convert one of a solution's proofs of access into its record form. A
%% solution found in a single recall range carries no second proof, which a
%% block spells as the empty proof rather than as an absent field.
proof(not_found, _Opts) ->
    #poa{};
proof(Proof, Opts) ->
    lib_arweave_block:to_poa(Proof, Opts).

%% @doc The second recall byte, which a solution found in a single recall range
%% does not have.
recall_byte2(Solution, Opts) ->
    case hb_maps:get(<<"recall-byte2">>, Solution, not_found, Opts) of
        not_found -> undefined;
        RecallByte2 -> hb_util:int(RecallByte2)
    end.

%% @doc The hash of a second packed sub-chunk, declared exactly when the second
%% recall byte is.
chunk_hash(undefined, _PoA2) ->
    undefined;
chunk_hash(_RecallByte2, PoA2) ->
    crypto:hash(sha256, PoA2#poa.chunk).

%% @doc The hash of a second unpacked chunk, declared exactly when the second
%% recall byte is.
unpacked_chunk_hash(undefined, _PoA2) ->
    undefined;
unpacked_chunk_hash(_RecallByte2, PoA2) ->
    crypto:hash(sha256, PoA2#poa.unpacked_chunk).

%% @doc Set the bytes the block's transactions add to the weave, and the size
%% the weave reaches by adding them.
weave(Prev, Next) ->
    BlockSize = block_size(Next),
    Next#block{
        block_size = BlockSize,
        weave_size = Prev#block.weave_size + BlockSize
    }.

%% @doc Fill the five values the account transition produces and the root of
%% the tree it leaves behind.
%%
%% A block extending one that carries no account tree could declare no
%% `wallet-list' at all, so this refuses rather than producing a block whose
%% strongest field is empty.
endowment([], _Prev, _Next, _TXs, _Opts) ->
    {error,
        request_error(<<"missing-accounts">>,
            <<"The block being extended carries no account tree, so the "
                "transition its child must declare cannot be computed.">>)};
endowment(Accounts, Prev, Next, TXs, Opts) ->
    maybe
        {ok, Balances} ?= balances(Accounts, Next, Prev, TXs, Opts),
        {ok, Endowment, Changed} ?= transition(Next, Prev, Balances),
        {EndowmentPool, MinerReward, DebtSupply, Latch, Multiplier} = Endowment,
        {ok, Applied} ?= accounts(Accounts, Changed, [], Opts),
        {ok,
            Next#block{
                wallet_list =
                    hb_util:decode(
                        hb_maps:get(<<"root">>, Applied, <<>>, Opts)),
                reward_pool = EndowmentPool,
                reward = MinerReward,
                debt_supply = DebtSupply,
                kryder_plus_rate_multiplier_latch = Latch,
                kryder_plus_rate_multiplier = Multiplier
            }
        }
    end.

%% @doc Commit to the two carried histories the block extends. Both read the
%% block's own contribution, so they are the last fields filled before it is
%% signed: the reward history covers the reward the transition produced, and
%% the block-time history covers the interval and step count it advanced by.
histories(Next, Prev) ->
    Next#block{
        reward_history_hash = lib_arweave_block:reward_history_hash(Next, Prev),
        block_time_history_hash =
            lib_arweave_block:block_time_history_hash(Next, Prev)
    }.

%% @doc Sign the block with the node's key and give it the identifier that
%% signature produces. The preimage binds the parent's cumulative difficulty,
%% which the block carries in no form the signature covers.
sign(Next, Prev, Wallet) ->
    SignedHash = ar_block:generate_signed_hash(Next),
    Signature =
        ar_wallet:sign(
            Wallet,
            ar_block:get_block_signature_preimage(
                Next#block.cumulative_diff,
                Prev#block.cumulative_diff,
                <<
                    (Next#block.previous_solution_hash)/binary,
                    SignedHash/binary
                >>,
                Next#block.height
            )
        ),
    Next#block{
        signature = Signature,
        indep_hash = ar_block:indep_hash2(SignedHash, Signature)
    }.

%% @doc Project the finished record onto the canonical block message. A block
%% header names its transactions by identifier, while the record carried their
%% bodies so that the transaction root and the weave arithmetic could read them.
header(Next, Opts) ->
    lib_arweave_block:from(
        Next#block{ txs = [ TX#tx.id || TX <- Next#block.txs ] },
        Opts
    ).

%% @doc Run the vendored account transition, mapping its rejections onto the
%% error convention.
update_accounts(Next, Prev, Balances) ->
    case ar_node_utils:update_accounts(Next, Prev, Balances) of
        {ok, Applied} ->
            {ok, Applied};
        {error, invalid_account_anchors} ->
            {error, consensus_error(<<"invalid-txs">>,
                <<"A transaction is anchored on an account state it may not "
                    "spend from.">>)};
        {error, mining_address_banned} ->
            {error, consensus_error(<<"invalid-mining-address">>,
                <<"The mining address is banned for double signing.">>)};
        {error, Reason} ->
            {error, consensus_error(<<"invalid-double-signing-proof">>,
                hb_util:bin(io_lib:format("~p", [Reason])))}
    end.

%% @doc Return the address a block's double signing proof bans, if it carries
%% one.
banned_addresses(#block{ double_signing_proof = undefined }) ->
    [];
banned_addresses(Next) ->
    [
        hb_util:encode(
            ar_wallet:to_address(
                ar_block:get_reward_key(
                    element(1, Next#block.double_signing_proof),
                    Next#block.height
                )
            )
        )
    ].

%% @doc Read a base64url field that has no meaningful default.
decode(Key, Message, Opts) ->
    hb_util:decode(field(Key, Message, Opts)).

%% @doc Read an integer field that has no meaningful default.
int(Key, Message, Opts) ->
    hb_util:int(field(Key, Message, Opts)).

%% @doc Read a field that has no meaningful default.
field(Key, Message, Opts) ->
    case hb_maps:get(Key, Message, not_found, Opts) of
        not_found -> throw({'missing-key', Key});
        Value -> Value
    end.

%% @doc Build the standard error bodies. A request this device cannot act on is
%% 400, while a rule the state itself breaks is 422.
request_error(Message, Detail) ->
    #{ <<"status">> => 400, <<"message">> => Message, <<"detail">> => Detail }.

consensus_error(Message, Detail) ->
    #{ <<"status">> => 422, <<"message">> => Message, <<"detail">> => Detail }.
