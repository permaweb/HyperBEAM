%%% @doc An AO-Core interface to Arweave's block rules: the codec between the
%%% two wire forms and the canonical block message, the three block hashing
%%% primitives, and the state transition itself.
%%%
%%% `apply/3' is the centrepiece. Its base is a chain-state message -- a block
%%% together with the state a block header does not express (see
%%% `lib_arweave_state') -- and its request names the block claiming to extend
%%% it. It returns the chain state that block produces, or the first check the
%%% block fails.
%%%
%%% The checks are the union of Arweave's five validation stages, run cheapest
%%% first rather than in stage order -- see `checks/5'.
%%% `ar_node_utils:validate/6' is only the fourth of those stages and performs
%%% no proof of work, no proof of access, no VDF and no RandomX; a port that
%%% implemented it alone would validate nothing cryptographic. The two dead
%%% upstream clauses -- `validate_block(difficulty, ...)' and
%%% `validate_block(block_field_sizes, ...)' -- are unreachable upstream and
%%% are not ported as live checks; difficulty is checked once, where the
%%% pre-validator checks it, and the field sizes are enforced structurally by
%%% `ar_serialize:binary_to_block/1'. The re-signed-solution shortcut is not
%%% ported either: it skips proof of work, proof of access and the VDF for a
%%% solution hash already in a cache this device does not keep.
%%%
%%% Every check reports its own error `message', so a rejection says which
%%% rule the block broke. That is what makes the mutation tests meaningful: a
%%% mutant that changes exactly one guarded field must produce exactly that
%%% check's error, and one that does not means the check is dead code.
%%%
%%% The checks are not wrapped in a catch-all. Upstream wraps its stage four in
%%% one because it runs inside a long-lived gen_server, but here it would turn
%%% a bug, or a sibling device that failed to resolve, into "invalid block" --
%%% precisely the failure this subsystem exists to rule out. Blocks reach these
%%% checks through `ar_serialize:binary_to_block/1', which enforces every field
%%% size structurally, and through the codec, which reports malformed base64url
%%% rather than decoding it to something else.
-module(dev_arweave_block).
-implements(<<"arweave-block@2.9">>).
-device_libraries([
    lib_arweave_block,
    lib_arweave_state,
    lib_arweave_tx,
    lib_arweave_accounts
]).
-compile({no_auto_import, [apply/3]}).
-export([info/1, apply/3, validate/3]).
-export([id/3, signed_hash/3, verify_signature/3]).
-export([from_binary/3, to_binary/3, from_json/3, to_json/3]).
-include("include/hb.hrl").
-include("include/ar_consensus.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Export only the block operations, leaving message manipulation to
%% `message@1.0'.
info(_Base) ->
    #{
        excludes => [<<"keys">>, <<"set">>, <<"set-path">>, <<"remove">>]
    }.

%% @doc Validate `next' as an extension of the chain state and return the chain
%% state it produces.
apply(Base, Req, Opts) ->
    maybe
        {ok, Prev, Next, TXs} ?= inputs(Base, Req, Opts),
        {ok, Accounts} ?= checks(Prev, Next, TXs, Base, Opts),
        ok ?= require_accounts(Accounts, Opts),
        {ok, NextMsg} ?= next_block(Req, Opts),
        transition(Base, Prev, Next, NextMsg, Accounts, Opts)
    end.

%% @doc Refuse to carry a chain state forward from a transition that ran with no
%% account tree, unless the node has asked for consensus-only validation.
%%
%% `validate/3' answers for a transition and names the mode it ran in, so a
%% caller inspecting one block can act on the answer. `apply/3' is different:
%% what it returns becomes the state the next block is checked against, and by
%% then the distinction is gone. A tree that failed to attach would otherwise
%% weaken every block after it, one `accounts-checked' line at a time, while the
%% chain kept advancing and the tip kept looking valid.
%%
%% `arweave-require-accounts' is the switch for the staged trust model: pre-2.9
%% work types arrive without a tree to spend from, and validating those is a
%% deliberate act, not a fallback something can slip into.
require_accounts([], Opts) ->
    case hb_util:atom(hb_opts:get(arweave_require_accounts, true, Opts)) of
        false ->
            ok;
        true ->
            {error, error_message(<<"accounts-not-checked">>,
                <<"The chain state carries no account tree, so the account and "
                    "transaction checks did not run. Set "
                    "`arweave-require-accounts' to false to build a chain "
                    "from consensus checks alone.">>)}
    end;
require_accounts(_Accounts, _Opts) ->
    ok.

%% @doc Run the same checks as `apply/3' without producing the next state, for
%% inspection and for testing a single transition in isolation.
%%
%% The result names which mode the block was validated in, because a chain
%% state carrying no account tree runs a strictly weaker set of checks and a
%% caller that cannot tell the two apart has no way to know it.
validate(Base, Req, Opts) ->
    maybe
        {ok, Prev, Next, TXs} ?= inputs(Base, Req, Opts),
        {ok, Accounts} ?= checks(Prev, Next, TXs, Base, Opts),
        {ok,
            #{
                <<"valid">> => true,
                <<"accounts-checked">> => accounts_checked(Accounts)
            }
        }
    end.

%% @doc Whether the account and transaction checks ran, which they do exactly
%% when the chain state carried an account tree for them to spend from. The
%% answer is read off what `check_accounts/5' produced rather than off the
%% state it was given, so it cannot drift from what actually happened.
accounts_checked([]) ->
    false;
accounts_checked(_Accounts) ->
    true.

%% @doc Recompute a block's identifier from the block itself. The header's own
%% `indep-hash' is not consulted, so the result may be compared against it.
id(Base, _Req, Opts) ->
    Block = lib_arweave_block:to(Base, Opts),
    {ok,
        #{
            <<"indep-hash">> =>
                hb_util:encode(
                    ar_block:indep_hash2(
                        ar_block:generate_signed_hash(Block),
                        Block#block.signature
                    )
                )
        }
    }.

%% @doc Return the hash the block producer signed.
signed_hash(Base, _Req, Opts) ->
    {ok,
        #{
            <<"signed-hash">> =>
                hb_util:encode(
                    ar_block:generate_signed_hash(
                        lib_arweave_block:to(Base, Opts)
                    )
                )
        }
    }.

%% @doc Verify a block's signature against its own reward key. The preimage
%% binds the previous block's cumulative difficulty, which the block does not
%% carry in a form the signature covers, so the caller supplies it as
%% `previous-cumulative-diff'.
verify_signature(Base, Req, Opts) ->
    Block = lib_arweave_block:to(Base, Opts),
    PrevCDiff =
        hb_util:int(
            get_first(
                <<"previous-cumulative-diff">>,
                Base,
                Req,
                Block#block.previous_cumulative_diff,
                Opts
            )
        ),
    {ok,
        #{
            <<"valid">> =>
                ar_block:verify_signature(
                    ar_block:generate_signed_hash(Block),
                    PrevCDiff,
                    Block
                )
        }
    }.

%% @doc Parse a block from Arweave's binary block format, given in `body'.
from_binary(Base, Req, Opts) ->
    case lib_arweave_block:from_binary(body(Base, Req, Opts), Opts) of
        {ok, Block} ->
            {ok, Block};
        {error, Reason} ->
            {error,
                error_message(<<"invalid-block-encoding">>,
                    hb_util:bin(io_lib:format("~p", [Reason])))}
    end.

%% @doc Serialize a block message into Arweave's binary block format.
to_binary(Base, _Req, Opts) ->
    {ok, #{ <<"body">> => lib_arweave_block:to_binary(Base, Opts) }}.

%% @doc Parse a block from Arweave's JSON block format, given in `body'.
from_json(Base, Req, Opts) ->
    {ok, lib_arweave_block:from_json(body(Base, Req, Opts), Opts)}.

%% @doc Serialize a block message into Arweave's JSON block format.
to_json(Base, _Req, Opts) ->
    {ok, #{ <<"body">> => lib_arweave_block:to_json(Base, Opts) }}.

%%% The ordered validation.

%% @doc Run every check the union of Arweave's five validation stages
%% performs, cheapest first. Each helper returns `ok' or an error, so the first
%% failure is the result of the block, and the last produces the account state
%% the block leaves behind -- both the subject of the final check and a
%% component of the next chain state.
%%
%% The set is exactly the reference's; the order is not. Upstream runs its
%% deterministic field checks after the VDF chain only because they live in a
%% different process, which a single function has no reason to imitate: it
%% would spend 1.8 billion SHA-256 invocations before noticing that a block's
%% weave size is one byte wrong. No check depends on another having run -- each
%% recomputes its expected value from the parent and the carried state -- so
%% the order decides which of several broken fields is named and how much work
%% a rejected block costs, and nothing else.
checks(Prev, Next, TXs, State, Opts) ->
    maybe
        ok ?= check_linkage(Next, Prev),
        ok ?= check_proof_sizes(Next),
        ok ?= check_chunk_hashes(Next),
        ok ?= check_unpacked_chunk_hashes(Next),
        ok ?= check_identity(Next, Prev),
        ok ?= check_timestamp(Next, Prev),
        ok ?= check_step_number(Next, Prev),
        ok ?= check_previous_solution_hash(Next, Prev),
        ok ?= check_last_retarget(Next, Prev),
        ok ?= check_difficulty(Next, Prev),
        ok ?= check_cumulative_diff(Next, Prev),
        ok ?= check_replica_format(Next),
        ok ?= check_weave_size(Next, Prev),
        ok ?= check_tx_root(Next),
        ok ?= check_block_index_root(Next, Prev),
        ok ?= check_packing_threshold(Next, Prev),
        ok ?= check_strict_data_split_threshold(Next, Prev),
        ok ?= check_merkle_rebase_threshold(Next, Prev),
        ok ?= check_usd_to_ar_rate(Next, Prev),
        ok ?= check_denomination(Next, Prev),
        ok ?= check_price_per_gib_minute(Next, Prev),
        ok ?= check_reward_history_hash(Next, Prev),
        ok ?= check_block_time_history_hash(Next, Prev),
        ok ?= check_next_vdf_difficulty(Next, Prev),
        ok ?= check_seed_data(Next, Prev, Opts),
        ok ?= check_partition_number(Next),
        ok ?= check_nonce(Next),
        {ok, H0, H1} ?= check_pow(Next, Prev, Opts),
        ok ?= check_poa(H0, H1, Next, Prev, State, Opts),
        ok ?= check_vdf(Next, Prev, Opts),
        ok ?= check_txs(Next, Prev, TXs, State, Opts),
        check_accounts(Next, Prev, TXs, State, Opts)
    end.

%% @doc The block extends the state's own block, one height further on, and
%% declares the cumulative difficulty it extends.
check_linkage(Next, Prev) ->
    maybe
        ok ?= equal(Next#block.previous_block, Prev#block.indep_hash,
            <<"invalid-previous-block">>,
            <<"The block does not name the chain state's block as its "
                "parent.">>),
        ok ?= equal(Next#block.height, Prev#block.height + 1,
            <<"invalid-height">>,
            <<"The height is not one greater than the parent's.">>),
        equal(Next#block.previous_cumulative_diff, Prev#block.cumulative_diff,
            <<"invalid-previous-cumulative-diff">>,
            <<"The declared previous cumulative difficulty is not the "
                "parent's.">>)
    end.

%% @doc Both proofs of access are within the tx-path, data-path and chunk size
%% limits.
check_proof_sizes(Next) ->
    holds(
        ar_block:validate_proof_size(Next#block.poa)
            andalso ar_block:validate_proof_size(Next#block.poa2),
        <<"invalid-proof-size">>,
        <<"A proof of access exceeds a field size limit.">>
    ).

%% @doc The packed chunks hash to the values the header declares. A block
%% solved from one chunk declares no second recall byte, and must then carry no
%% second proof at all.
check_chunk_hashes(Next = #block{ recall_byte2 = undefined }) ->
    maybe
        ok ?= check_chunk_hash(Next#block.poa, Next#block.chunk_hash),
        holds(Next#block.poa2 == #poa{}, <<"invalid-chunk-hash">>,
            <<"A block without a second recall byte carries no second proof.">>)
    end;
check_chunk_hashes(Next) ->
    maybe
        ok ?= check_chunk_hash(Next#block.poa, Next#block.chunk_hash),
        check_chunk_hash(Next#block.poa2, Next#block.chunk2_hash)
    end.

%% @doc One packed chunk hashes to its declared hash.
check_chunk_hash(PoA, Hash) ->
    equal(crypto:hash(sha256, PoA#poa.chunk), Hash,
        <<"invalid-chunk-hash">>,
        <<"A packed chunk does not hash to its declared value.">>).

%% @doc Above packing difficulty 0 the proofs carry the unpacked chunks too,
%% zero-padded to a full chunk, and the header declares their hashes. At
%% packing difficulty 0 it declares neither.
check_unpacked_chunk_hashes(Next = #block{ packing_difficulty = 0 }) ->
    holds(
        {Next#block.unpacked_chunk_hash, Next#block.unpacked_chunk2_hash}
            == {undefined, undefined},
        <<"invalid-unpacked-chunk-hash">>,
        <<"A block packed at difficulty 0 declares no unpacked chunk hash.">>
    );
check_unpacked_chunk_hashes(Next) ->
    maybe
        ok ?= check_unpacked_chunk_hash(
            Next#block.poa, Next#block.unpacked_chunk_hash),
        check_second_unpacked_chunk_hash(Next)
    end.

%% @doc The second unpacked chunk is declared exactly when the second recall
%% byte is.
check_second_unpacked_chunk_hash(Next = #block{ recall_byte2 = undefined }) ->
    holds(Next#block.unpacked_chunk2_hash == undefined,
        <<"invalid-unpacked-chunk-hash">>,
        <<"A block without a second recall byte declares no second unpacked "
            "chunk hash.">>);
check_second_unpacked_chunk_hash(Next) ->
    check_unpacked_chunk_hash(Next#block.poa2, Next#block.unpacked_chunk2_hash).

%% @doc One unpacked chunk is a full zero-padded chunk and hashes to its
%% declared hash.
check_unpacked_chunk_hash(PoA, Hash) ->
    holds(
        crypto:hash(sha256, PoA#poa.unpacked_chunk) == Hash
            andalso byte_size(PoA#poa.unpacked_chunk) == ?DATA_CHUNK_SIZE,
        <<"invalid-unpacked-chunk-hash">>,
        <<"An unpacked chunk is not a full chunk hashing to its declared "
            "value.">>
    ).

%% @doc The block is signed by the key its mining address is derived from, and
%% its identifier is the hash of that signed hash with the signature. Both read
%% the same signed hash, so it is computed once.
check_identity(Next, Prev) ->
    SignedHash = ar_block:generate_signed_hash(Next),
    maybe
        ok ?= holds(
            ar_block:verify_signature(
                SignedHash, Prev#block.cumulative_diff, Next),
            <<"invalid-signature">>,
            <<"The block signature does not verify against its reward key.">>),
        equal(
            ar_block:indep_hash2(SignedHash, Next#block.signature),
            Next#block.indep_hash,
            <<"invalid-indep-hash">>,
            <<"The block identifier is not the hash of its signed hash and "
                "signature.">>)
    end.

%% @doc The timestamp is within the network's clock tolerance of both the
%% parent's timestamp and the wall clock.
check_timestamp(Next, Prev) ->
    holds(ar_block:verify_timestamp(Next, Prev), <<"invalid-timestamp">>,
        <<"The timestamp is outside the tolerated deviation.">>).

%% @doc The block's VDF step is ahead of the parent's, no further ahead than a
%% block's step list may reach, carries exactly the steps between the two, and
%% anchors on the parent's output.
%%
%% The distance bound is measured against the *parent's* step number, where
%% upstream measures against the validating node's own VDF position
%% (`ar_block_pre_validator:pre_validate_nonce_limiter/2'). This node has no VDF
%% server, so it has no such position. Since a node's position is at least the
%% parent's, the bound here is the stricter of the two: a block more than
%% `?NONCE_LIMITER_MAX_CHECKPOINTS_COUNT' steps -- around three hours at the
%% current rate -- beyond its parent is refused where upstream might accept it.
%% Unreachable at a ~122 second block interval, but it is a tightening rather
%% than a port, and a tightening of a consensus rule should be visible.
check_step_number(Next, Prev) ->
    Info = Next#block.nonce_limiter_info,
    PrevInfo = Prev#block.nonce_limiter_info,
    StepNumber = ar_block:vdf_step_number(Next),
    PrevStepNumber = ar_block:vdf_step_number(Prev),
    Distance = StepNumber - PrevStepNumber,
    holds(
        ar_nonce_limiter:is_ahead_on_the_timeline(Info, PrevInfo)
            andalso Distance =< ?NONCE_LIMITER_MAX_CHECKPOINTS_COUNT
            andalso length(Info#nonce_limiter_info.steps)
                == min(?NONCE_LIMITER_MAX_CHECKPOINTS_COUNT, Distance)
            andalso Info#nonce_limiter_info.prev_output
                == PrevInfo#nonce_limiter_info.output,
        <<"invalid-step-number">>,
        <<"The VDF step number, step count or previous output does not follow "
            "the parent's.">>
    ).

%% @doc The block names the parent's solution hash.
check_previous_solution_hash(Next, Prev) ->
    equal(Next#block.previous_solution_hash, Prev#block.hash,
        <<"invalid-previous-solution-hash">>,
        <<"The declared previous solution hash is not the parent's.">>).

%% @doc The retarget timestamp is the block's own at a retarget height and the
%% parent's everywhere else.
check_last_retarget(Next, Prev) ->
    holds(ar_block:verify_last_retarget(Next, Prev),
        <<"invalid-last-retarget">>,
        <<"The last retarget timestamp does not follow the parent's.">>).

%% @doc The difficulty is the one the retarget rule derives from the parent.
check_difficulty(Next, Prev) ->
    holds(ar_retarget:validate_difficulty(Next, Prev),
        <<"invalid-difficulty">>,
        <<"The difficulty is not the one the retarget rule derives.">>).

%% @doc The cumulative difficulty is the parent's plus this block's work.
check_cumulative_diff(Next, Prev) ->
    holds(ar_block:verify_cumulative_diff(Next, Prev),
        <<"invalid-cumulative-diff">>,
        <<"The cumulative difficulty does not extend the parent's.">>).

%% @doc The packing difficulty and replication format are a combination legal
%% at this height.
check_replica_format(Next) ->
    holds(
        ar_block:validate_replica_format(
            Next#block.height,
            Next#block.packing_difficulty,
            Next#block.replica_format
        ),
        <<"invalid-replica-format">>,
        <<"The packing difficulty and replica format are not legal at this "
            "height.">>
    ).

%% @doc The five nonce limiter seed fields are the ones the parent and the
%% block's own step number determine. They rotate together, and only when the
%% step range crosses a reset line.
check_seed_data(Next, Prev, Opts) ->
    Info = Next#block.nonce_limiter_info,
    PrevInfo = Prev#block.nonce_limiter_info,
    maybe
        {ok, Expected} ?=
            hb_ao:resolve(
                #{
                    <<"device">> => <<"arweave-vdf@2.9">>,
                    <<"step-number">> => ar_block:vdf_step_number(Next),
                    <<"prev-nonce-limiter-info">> =>
                        lib_arweave_block:from_nonce_limiter(PrevInfo, Opts),
                    <<"prev-indep-hash">> =>
                        hb_util:encode(Prev#block.indep_hash),
                    <<"prev-weave-size">> => Prev#block.weave_size
                },
                <<"seed-data">>,
                Opts
            ),
        equal(
            {
                hb_util:encode(Info#nonce_limiter_info.seed),
                hb_util:encode(Info#nonce_limiter_info.next_seed),
                Info#nonce_limiter_info.partition_upper_bound,
                Info#nonce_limiter_info.next_partition_upper_bound,
                Info#nonce_limiter_info.vdf_difficulty
            },
            {
                hb_maps:get(<<"seed">>, Expected, <<>>, Opts),
                hb_maps:get(<<"next-seed">>, Expected, <<>>, Opts),
                hb_util:int(hb_maps:get(
                    <<"partition-upper-bound">>, Expected, 0, Opts)),
                hb_util:int(hb_maps:get(
                    <<"next-partition-upper-bound">>, Expected, 0, Opts)),
                hb_util:int(hb_maps:get(
                    <<"vdf-difficulty">>, Expected, 0, Opts))
            },
            <<"invalid-seed-data">>,
            <<"The nonce limiter seed data is not the one the parent "
                "determines.">>
        )
    end.

%% @doc The mining partition is within the weave as the nonce limiter bounds
%% it.
check_partition_number(Next) ->
    Info = Next#block.nonce_limiter_info,
    UpperBound = Info#nonce_limiter_info.partition_upper_bound,
    holds(
        Next#block.partition_number
            =< max(0, UpperBound div ar_block:partition_size() - 1),
        <<"invalid-partition-number">>,
        <<"The mining partition is beyond the weave's upper bound.">>
    ).

%% @doc The nonce is within the recall range at this packing difficulty.
check_nonce(Next) ->
    Max = ar_block:get_max_nonce(Next#block.packing_difficulty),
    holds(
        Next#block.nonce =< Max,
        <<"invalid-nonce">>,
        <<"The nonce is beyond the recall range.">>
    ).

%% @doc The solution hash is the one the two chunks produce, and it passes the
%% difficulty its solution type requires. A one-chunk solution pays the proof
%% of access multiplier, so the pair of difficulties is checked, not the
%% single header field.
%%
%% Upstream runs a cheaper form of this check first, as a denial of service
%% filter: it recomputes the solution hash from the declared preimage and tests
%% only the difficulty. That check is implied by this one whenever this one
%% passes, so it is not a separate rule here.
%%
%% Both hashes are returned, because the proof of access check is derived from
%% the same two: `h0' selects the recall ranges and costs the one RandomX hash
%% a block validation needs, and whether `h1' is the block's solution hash is
%% what decides whether there is a second proof to check at all.
check_pow(Next, Prev, Opts) ->
    maybe
        {ok, H0} ?= solution_h0(Next, Prev, Opts),
        {ok, H1, Preimage1} ?=
            solution_hash(<<"h1">>,
                #{
                    <<"h0">> => H0,
                    <<"nonce">> => Next#block.nonce,
                    <<"chunk">> => hb_util:encode((Next#block.poa)#poa.chunk)
                },
                Opts),
        DiffPair = ar_difficulty:diff_pair(Next),
        ok ?=
            check_pow(
                one_chunk(H1, Preimage1, Next, DiffPair),
                H0, H1, Next, DiffPair, Opts
            ),
        {ok, H0, H1}
    end.

%% @doc Whether the block is a one-chunk solution: its first solution hash is
%% the one it declares, meets the one-chunk difficulty and has the preimage it
%% declares, and it names no second recall byte and no second chunk.
%%
%% All five together, because the two branches are not alternatives a block may
%% pick between -- a block that fails any of them must be a two-chunk solution
%% and is held to the two-chunk rules below.
one_chunk(H1, Preimage1, Next, DiffPair) ->
    hb_util:decode(H1) == Next#block.hash
        andalso ar_node_utils:h1_passes_diff_check(
            hb_util:decode(H1), DiffPair, Next#block.packing_difficulty)
        andalso hb_util:decode(Preimage1) == Next#block.hash_preimage
        andalso Next#block.recall_byte2 == undefined
        andalso Next#block.chunk2_hash == undefined.

%% @doc A one-chunk solution needs no second hash. Otherwise the solution hash
%% must be the two-chunk hash, at the two-chunk difficulty, over the preimage
%% the block declares.
%%
%% The three name themselves apart rather than sharing one `invalid-pow'. They
%% are three separate rules -- the solution is this block's, the solution is
%% hard enough, the preimage is the one signed -- and only the second is the
%% difficulty enforcement that makes mining cost anything. Sharing a message
%% left it untestable: no mutant could distinguish its removal from the removal
%% of either neighbour, because a block mutated enough to reach this check at
%% all fails the first of the three.
check_pow(true, _H0, _H1, _Next, _DiffPair, _Opts) ->
    ok;
check_pow(false, H0, H1, Next, DiffPair, Opts) ->
    maybe
        {ok, H2, Preimage2} ?=
            solution_hash(<<"h2">>,
                #{
                    <<"h0">> => H0,
                    <<"h1">> => H1,
                    <<"chunk">> => hb_util:encode((Next#block.poa2)#poa.chunk)
                },
                Opts),
        ok ?= equal(hb_util:decode(H2), Next#block.hash,
            <<"invalid-solution-hash">>,
            <<"The solution hash is not the one the block's chunks produce.">>),
        ok ?= holds(
            ar_node_utils:h2_passes_diff_check(
                hb_util:decode(H2), DiffPair, Next#block.packing_difficulty),
            <<"insufficient-difficulty">>,
            <<"The solution hash does not meet the difficulty the block "
                "declares.">>),
        equal(hb_util:decode(Preimage2), Next#block.hash_preimage,
            <<"invalid-hash-preimage">>,
            <<"The hash preimage is not the one the solution hash was taken "
                "over.">>)
    end.

%% @doc Compute the entropy the mining nonces are drawn from. This is the one
%% RandomX hash a block validation needs.
solution_h0(Next, Prev, Opts) ->
    PrevInfo = Prev#block.nonce_limiter_info,
    Info = Next#block.nonce_limiter_info,
    maybe
        {ok, Result} ?=
            hb_ao:resolve(
                #{
                    <<"device">> => <<"arweave-spora@2.9">>,
                    <<"nonce-limiter-output">> =>
                        hb_util:encode(Info#nonce_limiter_info.output),
                    <<"partition-number">> => Next#block.partition_number,
                    <<"seed">> =>
                        hb_util:encode(PrevInfo#nonce_limiter_info.seed),
                    <<"reward-addr">> => hb_util:encode(Next#block.reward_addr),
                    <<"packing-difficulty">> => Next#block.packing_difficulty
                },
                <<"h0">>,
                Opts
            ),
        {ok, hb_maps:get(<<"h0">>, Result, <<>>, Opts)}
    end.

%% @doc Compute one of the two solution hashes over a packed chunk.
solution_hash(Key, Base, Opts) ->
    maybe
        {ok, Result} ?=
            hb_ao:resolve(
                Base#{ <<"device">> => <<"arweave-spora@2.9">> },
                Key,
                Opts
            ),
        {ok,
            hb_maps:get(<<"hash">>, Result, <<>>, Opts),
            hb_maps:get(<<"preimage">>, Result, <<>>, Opts)
        }
    end.

%% @doc Both recall bytes are recomputed from the mining entropy rather than
%% trusted, the block containing each is found in the weave, and the proof of
%% access for each validates against that block's transaction root. A
%% one-chunk solution proves only the first.
check_poa(H0, H1, Next, Prev, State, Opts) ->
    maybe
        {ok, Range1, Range2} ?= recall_ranges(Next, H0, Opts),
        RecallByte = ar_block:get_recall_byte(
            Range1, Next#block.nonce, Next#block.packing_difficulty),
        ok ?= check_proof(RecallByte, Next#block.recall_byte, Next#block.poa,
            Next, Prev, State, <<"invalid-poa">>, Opts),
        check_second_proof(
            hb_util:decode(H1) == Next#block.hash, Range2, Next, Prev, State,
            Opts)
    end.

%% @doc A solution whose hash is the one-chunk hash proves no second chunk.
check_second_proof(true, _Range2, _Next, _Prev, _State, _Opts) ->
    ok;
check_second_proof(false, Range2, Next, Prev, State, Opts) ->
    RecallByte2 = ar_block:get_recall_byte(
        Range2, Next#block.nonce, Next#block.packing_difficulty),
    check_proof(RecallByte2, Next#block.recall_byte2, Next#block.poa2, Next,
        Prev, State, <<"invalid-poa2">>, Opts).

%% @doc Check one proof of access: the recall byte the block declares is the
%% one the entropy determines, the byte lies within the weave, and the proof
%% resolves to the chunk the solution was mined from.
check_proof(RecallByte, Declared, PoA, Next, Prev, State, Message, Opts) ->
    maybe
        ok ?= equal(RecallByte, Declared, <<"invalid-recall-byte">>,
            <<"The declared recall byte is not the one the mining entropy "
                "determines.">>),
        {ok, BlockStart, BlockEnd, TXRoot} ?=
            block_bounds(RecallByte, Prev, State, Opts),
        {ok, Result} ?=
            hb_ao:resolve(
                #{
                    <<"device">> => <<"arweave-spora@2.9">>,
                    <<"block-start-offset">> => BlockStart,
                    <<"recall-offset">> => RecallByte,
                    <<"tx-root">> => hb_util:encode(TXRoot),
                    <<"block-size">> => BlockEnd - BlockStart,
                    <<"poa">> => lib_arweave_block:from_poa(PoA, Opts),
                    <<"packing">> => packing(Next),
                    <<"sub-chunk-index">> =>
                        ar_block:get_sub_chunk_index(
                            Next#block.packing_difficulty, Next#block.nonce)
                },
                <<"validate">>,
                Opts
            ),
        holds(hb_maps:get(<<"valid">>, Result, false, Opts) == true, Message,
            <<"The proof of access does not resolve to a chunk of the weave.">>)
    end.

%% @doc Return the block index to search, or say that there is none. A recall
%% byte may point anywhere in the weave, so unlike the account tree the index
%% has no mode in which the check it feeds can be skipped: without it a proof
%% of access cannot be checked at all, which is a caller error rather than an
%% invalid block.
index([]) ->
    {error,
        #{
            <<"status">> => 400,
            <<"message">> => <<"missing-block-index">>,
            <<"detail">> =>
                <<"The chain state must carry a block index to check a proof "
                    "of access against.">>
        }
    };
index(Index) ->
    {ok, Index}.

%% @doc Describe the packing a block's chunks are in, for the storage proof
%% device.
packing(Next) ->
    #{
        <<"format">> =>
            format(
                ar_block:get_packing(
                    Next#block.packing_difficulty,
                    Next#block.reward_addr,
                    Next#block.replica_format
                )
            ),
        <<"reward-addr">> => hb_util:encode(Next#block.reward_addr),
        <<"packing-difficulty">> => Next#block.packing_difficulty
    }.

%% @doc Name a packing format on the wire. The mapping is explicit rather than
%% derived, so an unhandled format is an error rather than a coerced atom.
format({spora_2_6, _Addr}) -> <<"spora-2-6">>;
format({composite, _Addr, _Difficulty}) -> <<"composite">>;
format({replica_2_9, _Addr}) -> <<"replica-2-9">>.

%% @doc Return the two recall range start offsets the mining entropy selects.
recall_ranges(Next, H0, Opts) ->
    Info = Next#block.nonce_limiter_info,
    maybe
        {ok, Result} ?=
            hb_ao:resolve(
                #{
                    <<"device">> => <<"arweave-spora@2.9">>,
                    <<"h0">> => H0,
                    <<"partition-number">> => Next#block.partition_number,
                    <<"partition-upper-bound">> =>
                        Info#nonce_limiter_info.partition_upper_bound
                },
                <<"recall-range">>,
                Opts
            ),
        {ok,
            hb_util:int(hb_maps:get(<<"range1-start">>, Result, 0, Opts)),
            hb_util:int(hb_maps:get(<<"range2-start">>, Result, 0, Opts))
        }
    end.

%% @doc Find the block of the weave a recall byte falls in, and its
%% transaction root.
%%
%% Upstream searches its recent-block cache before the index, because it keeps
%% one global index and resolves forks against that cache. Here the index is
%% itself part of the chain state and was extended by each block as it was
%% validated, so it already describes this branch and nothing else: the search
%% is the index lookup alone.
block_bounds(RecallByte, Prev, State, Opts) ->
    maybe
        ok ?= holds(RecallByte < Prev#block.weave_size,
            <<"invalid-recall-byte">>,
            <<"The recall byte is at or beyond the end of the weave.">>),
        {ok, Index} ?= index(lib_arweave_state:block_index(State, Opts)),
        {ok, Bounds} ?=
            hb_ao:resolve(
                Index#{
                    <<"device">> => <<"arweave-block-index@2.9">>,
                    <<"offset">> => RecallByte
                },
                <<"bounds">>,
                Opts
            ),
        {ok,
            hb_util:int(hb_maps:get(<<"block-start">>, Bounds, 0, Opts)),
            hb_util:int(hb_maps:get(<<"block-end">>, Bounds, 0, Opts)),
            hb_util:decode(hb_maps:get(<<"tx-root">>, Bounds, <<>>, Opts))
        }
    end.

%% @doc Every VDF step between the two blocks recomputes, anchored on the
%% parent's output rather than on anything the block asserts about itself.
%%
%% This is one call rather than two: the VDF device's `verify-chain' runs the
%% final step's 25 checkpoints -- the reference's separate stage two check --
%% before the chain itself, so calling `verify-step' as well would repeat a
%% second of work. The device reports which of the two failed, so the
%% distinction survives in the error rather than in the call order.
%% Matched as `{ok, Result}' and then asserted on, rather than by matching
%% `{ok, #{valid := true}}' directly. `maybe' without `else' returns the value
%% that failed to match, so a device answering `{ok, #{valid => false}}' -- or
%% `{ok, #{valid => <<"true">>}}' -- would become the return of this function,
%% then of `checks/5', where it binds `Accounts'. `require_accounts/2' would
%% take its non-empty clause and accept the block, and `accounts_checked/1'
%% would report `true'. Failing open on the most expensive check in the
%% subsystem, and reporting the result as fully validated.
check_vdf(Next, Prev, Opts) ->
    maybe
        {ok, Result} ?=
            hb_ao:resolve(
                #{
                    <<"device">> => <<"arweave-vdf@2.9">>,
                    <<"nonce-limiter-info">> =>
                        lib_arweave_block:from_nonce_limiter(
                            Next#block.nonce_limiter_info, Opts),
                    <<"prev-nonce-limiter-info">> =>
                        lib_arweave_block:from_nonce_limiter(
                            Prev#block.nonce_limiter_info, Opts),
                    <<"prev-indep-hash">> =>
                        hb_util:encode(Prev#block.indep_hash),
                    <<"prev-weave-size">> => Prev#block.weave_size
                },
                <<"verify-chain">>,
                Opts
            ),
        holds(hb_maps:get(<<"valid">>, Result, false, Opts) == true,
            <<"invalid-vdf-chain">>,
            <<"The nonce limiter device did not answer that the chain is "
                "valid.">>)
    end.

%% @doc The weave grows by exactly the padded size of the block's
%% transactions.
check_weave_size(Next, Prev) ->
    holds(ar_block:verify_weave_size(Next, Prev, Next#block.txs),
        <<"invalid-weave-size">>,
        <<"The weave size does not grow by the size of the block's "
            "transactions.">>).

%% @doc The 2.5 packing threshold moves at its scheduled rate.
check_packing_threshold(Next, Prev) ->
    Info = Next#block.nonce_limiter_info,
    Expected =
        ar_block:get_packing_threshold(
            Prev,
            Info#nonce_limiter_info.partition_upper_bound
        ),
    holds(
        Expected == undefined
            orelse Next#block.packing_2_5_threshold == Expected,
        <<"invalid-packing-threshold">>,
        <<"The 2.5 packing threshold did not move as scheduled.">>
    ).

%% @doc The strict data split threshold is fixed once set.
check_strict_data_split_threshold(Next, Prev) ->
    equal(Next#block.strict_data_split_threshold,
        Prev#block.strict_data_split_threshold,
        <<"invalid-strict-data-split-threshold">>,
        <<"The strict data split threshold is not the parent's.">>).

%% @doc The two conversion rates are the ones the parent determines.
check_usd_to_ar_rate(Next, Prev) ->
    equal(
        {Next#block.usd_to_ar_rate, Next#block.scheduled_usd_to_ar_rate},
        ar_pricing:recalculate_usd_to_ar_rate(Prev),
        <<"invalid-usd-to-ar-rate">>,
        <<"The USD to AR rates are not the ones the parent determines.">>
    ).

%% @doc The denomination and the height of the last redenomination are the ones
%% the parent determines.
check_denomination(Next, Prev) ->
    equal(
        {Next#block.denomination, Next#block.redenomination_height},
        ar_pricing:may_be_redenominate(Prev),
        <<"invalid-denomination">>,
        <<"The denomination is not the one the parent determines.">>
    ).

%% @doc The reward history hash chains this block's reward onto the parent's
%% hash. Since 2.8 the chain needs only the previous hash and the new element,
%% not the whole history.
check_reward_history_hash(Next, Prev) ->
    Element =
        {
            Next#block.reward_addr,
            ar_difficulty:get_hash_rate_fixed_ratio(Next),
            Next#block.reward,
            Next#block.denomination
        },
    equal(
        ar_rewards:reward_history_hash(
            Next#block.height,
            Prev#block.reward_history_hash,
            ar_rewards:trim_locked_rewards(
                Next#block.height,
                [Element | Prev#block.reward_history]
            )
        ),
        Next#block.reward_history_hash,
        <<"invalid-reward-history-hash">>,
        <<"The reward history hash does not chain onto the parent's.">>
    ).

%% @doc The block-time history hash covers the parent's history extended with
%% this block's interval, VDF interval and solution type.
check_block_time_history_hash(Next, Prev) ->
    equal(
        ar_block_time_history:hash(
            ar_block_time_history:update_history(Next, Prev)),
        Next#block.block_time_history_hash,
        <<"invalid-block-time-history-hash">>,
        <<"The block time history hash does not cover the parent's history "
            "extended with this block.">>
    ).

%% @doc The scheduled VDF difficulty is the one the parent's block-time history
%% retargets to.
check_next_vdf_difficulty(Next, Prev) ->
    Info = Next#block.nonce_limiter_info,
    equal(Info#nonce_limiter_info.next_vdf_difficulty,
        ar_block:compute_next_vdf_difficulty(Prev),
        <<"invalid-next-vdf-difficulty">>,
        <<"The scheduled VDF difficulty is not the one the retarget "
            "derives.">>).

%% @doc The two storage prices are the ones the parent determines, redenominated
%% if the block redenominates.
check_price_per_gib_minute(Next, Prev) ->
    {Price, ScheduledPrice} = ar_pricing:recalculate_price_per_gib_minute(Prev),
    Denomination = Next#block.denomination,
    PrevDenomination = Prev#block.denomination,
    equal(
        {
            Next#block.price_per_gib_minute,
            Next#block.scheduled_price_per_gib_minute
        },
        {
            ar_pricing:redenominate(Price, PrevDenomination, Denomination),
            ar_pricing:redenominate(
                ScheduledPrice, PrevDenomination, Denomination)
        },
        <<"invalid-price-per-gib-minute">>,
        <<"The storage prices are not the ones the parent determines.">>
    ).

%% @doc Every transaction is signed, affordable, anchored within the recent
%% blocks, not a replay of one already on the weave, and priced at least at
%% the required fee. The block's transaction count and data size are within
%% their limits.
%%
%% The rule is cumulative -- each transaction is applied to a running account
%% map before the next is checked -- so a block cannot spend the same balance
%% twice. That is why it is a block rule and not a fold over the
%% single-transaction primitive `~arweave-tx@2.9/verify'.
%%
%% Without an anchored account tree there are no balances to spend from, so it
%% runs in the same mode as the account checks.
check_txs(Next, Prev, TXs, State, Opts) ->
    check_txs(
        lib_arweave_state:accounts(State, Opts), Next, Prev, TXs, State, Opts).

check_txs([], _Next, _Prev, _TXs, _State, _Opts) ->
    ok;
check_txs(Accounts, Next, Prev, TXs, State, Opts) ->
    maybe
        {ok, Balances} ?= balances(Accounts, Next, Prev, TXs, Opts),
        Args =
            {
                Next#block.txs,
                ar_pricing:usd_to_ar_rate(Prev),
                Prev#block.price_per_gib_minute,
                Prev#block.kryder_plus_rate_multiplier,
                Prev#block.denomination,
                Next#block.height - 1,
                Prev#block.redenomination_height,
                Next#block.timestamp,
                Balances,
                anchors(lib_arweave_state:block_anchors(State, Opts)),
                replays(lib_arweave_state:recent_transactions(State, Opts))
            },
        holds(ar_tx_replay_pool:verify_block_txs(Args) == valid,
            <<"invalid-txs">>,
            <<"A transaction is unsigned, unaffordable, misanchored, replayed "
                "or underpriced.">>)
    end.

%% @doc Decode the block hashes a transaction may anchor against.
anchors(BlockAnchors) ->
    [ hb_util:decode(Anchor) || Anchor <- BlockAnchors ].

%% @doc Decode the identifiers a transaction may not replay, as the map
%% `ar_tx_replay_pool' looks them up in.
replays(RecentTransactions) ->
    maps:from_list([ {hb_util:decode(ID), ok} || ID <- RecentTransactions ]).

%% @doc The transaction root is the Merkle root over the data roots of the
%% block's transactions, in the offsets their padded sizes give them.
check_tx_root(Next) ->
    holds(ar_block:verify_tx_root(Next), <<"invalid-tx-root">>,
        <<"The transaction root is not the root over the block's "
            "transactions.">>).

%% @doc The block index root extends the parent's with the parent's own entry.
check_block_index_root(Next, Prev) ->
    holds(ar_block:verify_block_hash_list_merkle(Next, Prev),
        <<"invalid-block-index-root">>,
        <<"The block index root does not extend the parent's with the "
            "parent's entry.">>).

%% @doc The Merkle rebase threshold is fixed once set.
check_merkle_rebase_threshold(Next, Prev) ->
    equal(Next#block.merkle_rebase_support_threshold,
        Prev#block.merkle_rebase_support_threshold,
        <<"invalid-merkle-rebase-threshold">>,
        <<"The Merkle rebase support threshold is not the parent's.">>).

%% @doc The account transition the block declares is the one that applying its
%% transactions, its mining reward and its endowment movements produces, and
%% the resulting account tree hashes to the root the block signed.
%%
%% This is the strongest property the subsystem has: a transition wrong by one
%% Winston produces a different root, and mainnet is the oracle. It returns the
%% account state it produced, which is also a component of the next chain
%% state, so the transition is computed once rather than once per use.
%%
%% An absent account tree disables it and yields an absent account state.
%% `apply/3' refuses to carry such a state forward unless
%% `arweave-require-accounts' is explicitly false, so this clause is reachable
%% only for a caller that asked for consensus-only validation.
check_accounts(Next, Prev, TXs, State, Opts) ->
    check_accounts(
        lib_arweave_state:accounts(State, Opts), Next, Prev, TXs, State, Opts).

check_accounts([], _Next, _Prev, _TXs, _State, _Opts) ->
    {ok, []};
check_accounts(Accounts, Next, Prev, TXs, _State, Opts) ->
    maybe
        {ok, Balances} ?= balances(Accounts, Next, Prev, TXs, Opts),
        {ok, Applied} ?= update_accounts(Next, Prev, Balances),
        % `ar_node_utils:update_accounts/3' takes its endowment arguments as
        % `{MinerReward, EndowmentPool, ...}' and returns them as
        % `{EndowmentPool, MinerReward, ...}'. The two are unequal in every
        % real block, so a transposition here is silent until the root fails.
        {EndowmentPool, MinerReward, DebtSupply, Latch, Multiplier, Updated} =
            Applied,
        Denomination = Prev#block.denomination,
        Denomination2 = Next#block.denomination,
        ok ?= equal(Next#block.reward_pool,
            ar_pricing:redenominate(EndowmentPool, Denomination, Denomination2),
            <<"invalid-reward-pool">>,
            <<"The endowment pool is not the one the transition produces.">>),
        ok ?= equal(Next#block.reward,
            ar_pricing:redenominate(MinerReward, Denomination, Denomination2),
            <<"invalid-reward">>,
            <<"The mining reward is not the one the transition produces.">>),
        ok ?= equal(Next#block.debt_supply,
            ar_pricing:redenominate(DebtSupply, Denomination, Denomination2),
            <<"invalid-debt-supply">>,
            <<"The debt supply is not the one the transition produces.">>),
        ok ?= equal(
            {
                Next#block.kryder_plus_rate_multiplier_latch,
                Next#block.kryder_plus_rate_multiplier
            },
            {Latch, Multiplier},
            <<"invalid-kryder-multiplier">>,
            <<"The Kryder multiplier is not the one the transition "
                "produces.">>),
        apply_accounts(Accounts, Next, Updated, Opts)
    end.

%% @doc Run the vendored account transition, mapping its rejections onto the
%% error convention.
update_accounts(Next, Prev, Balances) ->
    case ar_node_utils:update_accounts(Next, Prev, Balances) of
        {ok, Applied} ->
            {ok, Applied};
        {error, invalid_account_anchors} ->
            {error, error_message(<<"invalid-txs">>,
                <<"A transaction is anchored on an account state it may not "
                    "spend from.">>)};
        {error, mining_address_banned} ->
            {error, error_message(<<"invalid-mining-address">>,
                <<"The mining address is banned for double signing.">>)};
        {error, Reason} ->
            {error, error_message(<<"invalid-double-signing-proof">>,
                hb_util:bin(io_lib:format("~p", [Reason])))}
    end.

%% @doc Insert the accounts the transition changed into the tree, and check the
%% resulting root against the one the block signed. The account tree device
%% owns both, and reports a mismatch as `invalid-wallet-list-root'.
apply_accounts(Accounts, Next, Updated, Opts) ->
    hb_ao:resolve(
        Accounts#{
            <<"device">> => <<"arweave-wallets@2.9">>,
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
            <<"expected-root">> => hb_util:encode(Next#block.wallet_list)
        },
        <<"apply">>,
        Opts
    ).

%% @doc Load the accounts the block's transition reads, in the vendored form.
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
                % That function answers `not_set' for an owner of 512 zero
                % bytes, which is a well-formed RSA owner rather than a
                % sentinel, and a peer chooses this field: transaction bodies
                % are fetched by id and their contents are not re-derived, so
                % a peer answering first with a zero owner reaches here. The
                % encoder would raise on the atom, and it would raise after the
                % proof of work, proof of access and VDF chain had been
                % computed. `lib_arweave_tx:to_tx/2' fills the field with
                % `ar_wallet:to_address/2' unconditionally, which is upstream's
                % value for every owner including this one.
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
                Accounts#{
                    <<"device">> => <<"arweave-wallets@2.9">>,
                    <<"addresses">> => Addresses
                },
                <<"get">>,
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

%%% Internal functions.

%% @doc Load the two blocks and the transaction bodies the checks work over.
%% The parent is read as a header: no check performed against it consults its
%% proofs, so neither of its chunks is loaded.
inputs(Base, Req, Opts) ->
    maybe
        {ok, NextMsg} ?= next_block(Req, Opts),
        Prev = lib_arweave_state:previous_block(Base, Opts),
        Next = lib_arweave_block:to(NextMsg, Opts),
        {ok, TXs} ?= transactions(NextMsg, Req, Opts),
        {ok,
            Prev,
            lib_arweave_block:with_transactions(
                Next,
                [ lib_arweave_tx:to_tx(TX, Opts) || TX <- TXs ]
            ),
            TXs
        }
    end.

%% @doc Read the block claiming to extend the chain state.
next_block(Req, Opts) ->
    case hb_maps:get(<<"next">>, Req, not_found, Opts) of
        not_found ->
            {error,
                #{
                    <<"status">> => 400,
                    <<"message">> => <<"missing-next-block">>,
                    <<"detail">> =>
                        <<"The request must name the block to apply as "
                            "`next'.">>
                }
            };
        NextMsg ->
            {ok, hb_cache:ensure_loaded(NextMsg, Opts)}
    end.

%% @doc Resolve the transaction bodies of the block being applied. A block
%% header carries only identifiers, while the weave size, transaction and
%% transaction root checks all need each transaction's data root and size, so
%% the request supplies the bodies. They must be exactly the block's
%% transactions, in the block's order. Supplying the wrong set is a caller
%% error rather than an invalid block, so it reports 400 rather than 422.
transactions(NextMsg, Req, Opts) ->
    IDs =
        hb_util:message_to_ordered_list(
            hb_maps:get(<<"txs">>, NextMsg, [], Opts), Opts),
    Supplied =
        hb_util:message_to_ordered_list(
            hb_maps:get(<<"transactions">>, Req, [], Opts), Opts),
    case [ hb_maps:get(<<"id">>, TX, <<>>, Opts) || TX <- Supplied ] of
        IDs ->
            {ok, Supplied};
        _ ->
            {error,
                #{
                    <<"status">> => 400,
                    <<"message">> => <<"missing-transactions">>,
                    <<"detail">> =>
                        <<"The request must supply the block's transaction "
                            "bodies, in the block's order.">>
                }
            }
    end.

%% @doc Build the chain state a validated block produces. Each component is
%% extended by the block: the histories gain its element, the recent blocks
%% gain its transactions, the index gains its entry, and the account state is
%% the one the account check already produced.
%% The block is carried forward as the message it arrived as, rather than
%% projected back out of the record: the record's transactions have been
%% replaced by resolved bodies, and a block message carries identifiers.
%%
%% The state also records which mode the block was validated in. Two states
%% that differ only in whether their block's transactions and account
%% transition were checked are otherwise indistinguishable, and a node whose
%% account checks are disabled must say so per block rather than look identical
%% to one whose are not.
transition(State, Prev, Next, NextMsg, Accounts, Opts) ->
    maybe
        {ok, Index} ?= next_block_index(State, Next, Opts),
        {ok,
            lib_arweave_state:next(State,
                #{
                    <<"block">> => NextMsg,
                    <<"block-index">> => Index,
                    <<"accounts">> => Accounts,
                    <<"accounts-checked">> => accounts_checked(Accounts),
                    <<"reward-history">> =>
                        lib_arweave_state:reward_history_message(
                            ar_rewards:add_element(
                                Next, Prev#block.reward_history)),
                    <<"block-time-history">> =>
                        lib_arweave_state:block_time_history_message(
                            ar_block_time_history:update_history(Next, Prev)),
                    <<"recent-blocks">> =>
                        lib_arweave_state:next_recent_blocks(
                            State, NextMsg, Opts)
                }
            )
        }
    end.

%% @doc Extend the block index with the validated block, leaving an absent
%% index absent.
next_block_index(State, Next, Opts) ->
    extend_block_index(lib_arweave_state:block_index(State, Opts), Next, Opts).

extend_block_index([], _Next, _Opts) ->
    {ok, []};
extend_block_index(Index, Next, Opts) ->
    hb_ao:resolve(
        Index#{
            <<"device">> => <<"arweave-block-index@2.9">>,
            <<"indep-hash">> => hb_util:encode(Next#block.indep_hash),
            <<"weave-size">> => Next#block.weave_size,
            <<"tx-root">> => hb_util:encode(Next#block.tx_root)
        },
        <<"append">>,
        Opts
    ).

%% @doc Read the wire document a codec key operates on.
body(Base, Req, Opts) ->
    get_first(<<"body">>, Base, Req, <<>>, Opts).

%% @doc Read a field from the request, falling back to the base message.
%%
%% These are data fields, not keys to resolve, so they are read with
%% `hb_maps:get/4' rather than `hb_ao:get/4'. Resolving a key against a message
%% that names this device dispatches back into the device, so reading a block's
%% own fields through the resolver would invoke this device's keys instead of
%% returning them.
get_first(Key, Base, Req, Default, Opts) ->
    case hb_maps:get(Key, Req, not_found, Opts) of
        not_found -> hb_maps:get(Key, Base, Default, Opts);
        Value -> Value
    end.

%% @doc Return `ok' when two values are equal, and the check's error otherwise.
equal(Value, Value, _Message, _Detail) -> ok;
equal(_Value, _Expected, Message, Detail) ->
    {error, error_message(Message, Detail)}.

%% @doc Return `ok' when a condition holds, and the check's error otherwise.
holds(true, _Message, _Detail) -> ok;
holds(false, Message, Detail) -> {error, error_message(Message, Detail)}.

%% @doc Build the standard error body.
error_message(Message, Detail) ->
    #{
        <<"status">> => 422,
        <<"message">> => Message,
        <<"detail">> => Detail
    }.

%%% Tests.
%%%
%%% Every vector here is a real mainnet post-2.9 block, taken from the network
%%% in both of its wire forms, and every mutant alters exactly one field of
%%% one of them.
%%%
%%% Two block pairs carry the state transition tests. `1,975,039 -> 1,975,040'
%%% is the field pair: the block-time history its parent commits to is frozen
%%% alongside it, so every deterministic field check is reachable. `1,974,870
%%% -> 1,974,871' is the transaction pair: all 26 transaction bodies of the
%%% child are frozen, so the weave size and the transaction root are computed
%%% over real transactions rather than over none.
%%%
%%% Mutating a block and expecting a check past the signature to fire needs
%%% care. `generate_signed_hash/1' covers nearly every header field, so a naive
%%% mutant is rejected as `invalid-signature' and proves only that the
%%% signature check works. Each mutant below therefore does one of three
%%% things: it mutates the parent, whose signature validating its child never
%%% verifies; it mutates a transaction body, which is not part of the header at
%%% all; or it mutates the child and re-signs it with a fresh key, which is
%%% sound for every check that runs before the reward history hash -- the first
%%% check to read the mining address the re-signing moves.

%% @doc The pair whose parent's block-time history is frozen.
-define(FIELD_PAIR, {1975039, 1975040}).

%% @doc The pair whose child's transaction bodies are frozen.
-define(TX_PAIR, {1974870, 1974871}).

%% @doc The pair whose child is a one-chunk solution: it declares one recall
%% byte, carries one proof of access, and its solution hash is the first of the
%% two mining hashes rather than the second.
%%
%% Roughly one mainnet block in thirty is solved from a single chunk, so a
%% corpus assembled by taking consecutive heights is all two-chunk blocks and
%% five clauses of this module never run against it. This pair is frozen for
%% that shape alone. It is near the tip because its parent's block-time history
%% has to be frozen with it, and peers serve that for the last fifty blocks
%% only -- an older one-chunk block stops at the block-time history check,
%% short of the proof of work the shape is interesting for.
-define(ONE_CHUNK_PAIR, {1975089, 1975090}).

%% @doc A pair whose VDF step range crosses an entropy reset line: the parent is
%% at step 111,614,366 and the child at 111,614,847, with the line at
%% 111,614,400. It is the only pair that can pin `check_vdf/3''s call site.
%%
%% At a reset, `ar_nonce_limiter:get_seed_data/2' takes its reset branch and
%% returns the parent's `next-vdf-difficulty', so the parent's own
%% `vdf-difficulty' is read by *no* check before `check_vdf/3' --
%% `check_seed_data/3' cannot see it, and `check_next_vdf_difficulty/2' only
%% recomputes it on a 720-boundary, which 1,975,308 is not. Mutating it
%% therefore reaches the chain verification and nothing else, which is what
%% makes the mutant able to tell whether `checks/5' calls it at all.
-define(RESET_PAIR, {1975307, 1975308}).

%% @doc The heights every frozen fixture block is at, read from the fixture
%% directory rather than listed, so that a block added there is covered by the
%% codec and hashing tests without anyone remembering to add it here.
fixtures() ->
    {ok, Names} = file:list_dir(<<"test/fixtures/arweave">>),
    lists:sort(
        [
            hb_util:int(Height)
        ||
            Name <- Names,
            {match, [Height]} <-
                [re:run(Name, "^block-([0-9]+)\\.bin$",
                    [{capture, all_but_first, binary}])]
        ]
    ).

%% @doc The options the tests resolve under.
test_opts() ->
    #{ <<"store">> => [hb_test_utils:test_store()] }.

%%% The codec.

%% @doc Re-serializing a parsed block reproduces the peer's bytes exactly, for
%% every fixture. This is the codec end to end: a field the parser drops,
%% mis-sizes or reorders shows up as a different binary.
binary_codec_test_() ->
    {timeout, 120, fun() ->
        lists:foreach(
            fun(Height) ->
                ?assertEqual(
                    fixture_binary(Height),
                    resolve_field(
                        fixture_message(Height), <<"to-binary">>, <<"body">>)
                )
            end,
            fixtures()
        )
    end}.

%% @doc The same block parsed from JSON and from the binary form re-serializes
%% to the same bytes. The two parsers share no code -- the binary one runs
%% through `ar_serialize', the JSON one is written against Arweave's encoder,
%% which has no parser to pair with -- so this is what catches a renamed field.
%% The comparison is made on the serialized form rather than on the two
%% messages, because a resolved result carries the resolver's own bookkeeping
%% alongside the block.
%%
%% `steps' is the field that matters: Arweave's JSON encoder emits it as
%% `checkpoints' and there is no JSON `steps' key at all, so a codec looking
%% for one yields an empty step list and the VDF chain then verifies over
%% nothing.
json_matches_binary_test_() ->
    {timeout, 120, fun() ->
        lists:foreach(
            fun(Height) ->
                ?assertEqual(
                    fixture_binary(Height),
                    resolve_field(
                        json_message(Height), <<"to-binary">>, <<"body">>)
                )
            end,
            fixtures()
        )
    end}.

%% @doc The JSON encoder reproduces the document the network served, key for
%% key, and its output parses back to the message it came from.
json_codec_test_() ->
    {timeout, 120, fun() ->
        lists:foreach(
            fun(Height) ->
                Encoded =
                    resolve_field(
                        fixture_message(Height), <<"to-json">>, <<"body">>),
                ?assertEqual(
                    hb_json:decode(fixture_json(Height)),
                    hb_json:decode(Encoded)
                ),
                ?assertEqual(
                    fixture_binary(Height),
                    resolve_field(
                        resolve(#{ <<"body">> => Encoded }, <<"from-json">>),
                        <<"to-binary">>,
                        <<"body">>
                    )
                )
            end,
            fixtures()
        )
    end}.

%% @doc Every block carries exactly the VDF steps between it and its parent,
%% newest first, and its output is the newest of them. A codec that read the
%% wrong JSON key would leave the list empty and every assertion here would
%% fail.
steps_test_() ->
    {timeout, 120, fun() ->
        lists:foreach(
            fun(Height) ->
                Info = nonce_limiter(fixture_message(Height)),
                PrevInfo = nonce_limiter(fixture_message(Height - 1)),
                Steps = maps:get(<<"steps">>, Info),
                ?assert(length(Steps) > 0),
                ?assertEqual(
                    maps:get(<<"global-step-number">>, Info)
                        - maps:get(<<"global-step-number">>, PrevInfo),
                    length(Steps)
                ),
                ?assertEqual(maps:get(<<"output">>, Info), hd(Steps)),
                ?assertEqual(
                    maps:get(<<"output">>, Info),
                    hd(maps:get(<<"last-step-checkpoints">>, Info))
                ),
                ?assertEqual(
                    maps:get(<<"output">>, PrevInfo),
                    maps:get(<<"prev-output">>, Info)
                )
            end,
            [
                Height
            ||
                Height <- fixtures(), lists:member(Height - 1, fixtures())
            ]
        )
    end}.

%%% Hashing.

%% @doc A block's identifier is the hash of its signed hash and its signature.
%% Recomputing it from the block reproduces the identifier the network knows
%% the block by, for every fixture.
indep_hash_test_() ->
    {timeout, 120, fun() ->
        lists:foreach(
            fun(Height) ->
                Block = fixture_message(Height),
                ?assertEqual(
                    maps:get(<<"indep-hash">>, Block),
                    resolve_field(Block, <<"id">>, <<"indep-hash">>)
                )
            end,
            fixtures()
        )
    end}.

%% @doc Every fixture's signature verifies against the reward key its mining
%% address is derived from.
verify_signature_test_() ->
    {timeout, 120, fun() ->
        lists:foreach(
            fun(Height) ->
                ?assertEqual(
                    true,
                    resolve_field(
                        fixture_message(Height), <<"verify-signature">>,
                        <<"valid">>)
                )
            end,
            fixtures()
        )
    end}.

%% @doc A block whose `wallet-list' has been altered neither verifies nor
%% hashes to its own identifier. The field is inside the signed hash, so this is
%% what stops a peer handing us a real block with a rewritten account root.
reject_altered_block_test() ->
    {_Prev, Next} = ?FIELD_PAIR,
    Altered =
        (fixture_message(Next))#{
            <<"wallet-list">> => hb_util:encode(crypto:strong_rand_bytes(48))
        },
    ?assertEqual(
        false,
        resolve_field(Altered, <<"verify-signature">>, <<"valid">>)
    ),
    ?assertNotEqual(
        maps:get(<<"indep-hash">>, fixture_message(Next)),
        resolve_field(Altered, <<"id">>, <<"indep-hash">>)
    ).

%%% The carried state.

%% @doc The two histories survive the wire encoding the chain state carries
%% them in. The block-time history frozen alongside the field pair is a real
%% one and re-encodes to the peer's bytes exactly.
history_round_trip_test() ->
    {Prev, _Next} = ?FIELD_PAIR,
    State = chain_state(Prev),
    History = lib_arweave_state:block_time_history(State, test_opts()),
    % A peer serves the history a block carries, which runs a consensus
    % window past the window the hash folds.
    ?assert(length(History) >= ar_block_time_history:history_length()),
    ?assertEqual(
        maps:get(<<"block-time-history">>, State),
        lib_arweave_state:block_time_history_message(History)
    ),
    Rewards = [{crypto:strong_rand_bytes(32), 17, 42, 1}],
    ?assertEqual(
        Rewards,
        lib_arweave_state:reward_history(
            #{
                <<"reward-history">> =>
                    lib_arweave_state:reward_history_message(Rewards)
            },
            test_opts()
        )
    ).

%% @doc The recent-block window a block leaves behind carries the block itself
%% at its head, and the anchors and transaction identifiers read back out of it
%% are the ones a transaction may anchor against and may not replay.
recent_blocks_test() ->
    {Prev, Next} = ?TX_PAIR,
    Block = fixture_message(Next),
    State =
        (chain_state(Prev))#{
            <<"recent-blocks">> =>
                lib_arweave_state:next_recent_blocks(
                    chain_state(Prev), Block, test_opts())
        },
    ?assertEqual(
        [maps:get(<<"indep-hash">>, Block)],
        lib_arweave_state:block_anchors(State, test_opts())
    ),
    ?assertEqual(
        maps:get(<<"txs">>, Block),
        lib_arweave_state:recent_transactions(State, test_opts())
    ),
    ?assertEqual(
        ar_block:get_max_tx_anchor_depth(),
        length(
            lists:foldl(
                fun(_N, Recent) ->
                    lib_arweave_state:next_recent_blocks(
                        #{ <<"recent-blocks">> => Recent }, Block, test_opts())
                end,
                [],
                lists:seq(1, ar_block:get_max_tx_anchor_depth() + 10)
            )
        )
    ).

%% @doc A parent is read as a header, with both proofs left empty. Every check
%% performed against a parent reads its fields and none reads its chunks, so
%% loading them would be two 256 KiB reads per block for nothing.
header_carries_no_proofs_test() ->
    {Prev, _Next} = ?FIELD_PAIR,
    Header = lib_arweave_state:previous_block(chain_state(Prev), test_opts()),
    ?assertEqual(#poa{}, Header#block.poa),
    ?assertEqual(#poa{}, Header#block.poa2),
    ?assertEqual(Prev, Header#block.height),
    ?assertEqual(
        hb_util:decode(maps:get(<<"indep-hash">>, fixture_message(Prev))),
        Header#block.indep_hash
    ).

%%% The state transition.

%% @doc A real consecutive mainnet pair validates in full. Every check runs:
%% structure, identity, linkage, time, difficulty, every deterministic field
%% check, the nonce limiter seed data, the partition and nonce bounds, the
%% proof of work, both proofs of access against the frozen slice of block
%% index, and the whole VDF step chain between the two blocks.
%%
%% This is the test the rest of the suite hangs off. A mutant is only evidence
%% that a check ran if the unmutated pair reaches that check, and until the
%% block index was frozen alongside the fixtures the pipeline stopped at
%% `missing-block-index' -- four checks short of the end, with proof of access,
%% the VDF chain, the transactions and the account transition all unreachable
%% and every mutant for them vacuous.
accepts_real_pair_test_() ->
    {timeout, 600, fun() ->
        ?assertEqual(valid, outcome(?FIELD_PAIR, fun unmutated/4))
    end}.

%% @doc The three rules the two-chunk proof of work enforces, each pinned by a
%% mutant that breaks that rule and nothing else.
%%
%% They are asserted against `check_pow/3' directly rather than through
%% `validate'. Every field the check reads is inside the block's signed hash,
%% so a mutant reaching the check through the pipeline has to be re-signed --
%% and re-signing moves the mining address, which is an input to the mining
%% entropy, so the solution hash changes whatever else the mutant altered and
%% the first of the three rules fires every time. Calling the check on records
%% is the only way to break one of the three at a time, and without it the
%% difficulty enforcement -- the rule that makes mining cost anything -- has no
%% test that would notice its removal.
pow_rules_test_() ->
    {timeout, 300, fun() ->
        {Prev, Next, Opts} = pow_subject(?FIELD_PAIR),
        ?assertMatch({ok, _H0, _H1}, check_pow(Next, Prev, Opts)),
        ?assertEqual(
            <<"invalid-solution-hash">>,
            pow_rejection(Next#block{ hash = hb_util:decode(hash(32)) },
                Prev, Opts)
        ),
        ?assertEqual(
            <<"insufficient-difficulty">>,
            pow_rejection(Next#block{ diff = ?MAX_DIFF - 1 }, Prev, Opts)
        ),
        ?assertEqual(
            <<"invalid-hash-preimage">>,
            pow_rejection(
                Next#block{ hash_preimage = hb_util:decode(hash(32)) },
                Prev, Opts
            )
        )
    end}.

%% @doc The parent and child records of a pair, and the options they resolve
%% under. No block index is needed: the proof of work reads the parent's seeds
%% and the child's own fields, and nothing from the weave.
pow_subject({PrevHeight, NextHeight}) ->
    Opts = test_opts(),
    {
        lib_arweave_state:previous_block(chain_state(PrevHeight), Opts),
        lib_arweave_block:to(fixture_message(NextHeight), Opts),
        Opts
    }.

%% @doc The `message' the proof of work check rejects a record pair with.
pow_rejection(Next, Prev, Opts) ->
    {error, Error} = check_pow(Next, Prev, Opts),
    hb_maps:get(<<"message">>, Error, not_found, Opts).

%% @doc Block validation runs the VDF chain, and reads what the chain device
%% answered rather than merely that it answered.
%%
%% Asserted on records for the same reason the proof of work is: every field
%% `check_vdf/3' reads is already read by a check that runs before it, so no
%% mutant can reach the VDF chain through `validate'.
vdf_rules_test_() ->
    {timeout, 300, fun() ->
        {Prev, Next, Opts} = pow_subject(?FIELD_PAIR),
        ?assertEqual(ok, check_vdf(Next, Prev, Opts)),
        PrevInfo = Prev#block.nonce_limiter_info,
        {error, Error} =
            check_vdf(
                Next,
                Prev#block{
                    nonce_limiter_info =
                        PrevInfo#nonce_limiter_info{
                            output = hb_util:decode(hash(32))
                        }
                },
                Opts
            ),
        ?assertEqual(
            <<"invalid-prev-output">>,
            hb_maps:get(<<"message">>, Error, not_found, Opts)
        )
    end}.

%% @doc A one-chunk mainnet pair validates in full, exercising the five clauses
%% the all-two-chunk corpus leaves dead: a block with no second recall byte
%% must carry no second proof, declare no second chunk hash and no second
%% unpacked chunk hash, take the one-chunk branch of the proof of work, and
%% prove no second chunk of access.
accepts_one_chunk_pair_test_() ->
    {timeout, 1800, fun() ->
        ?assertEqual(valid, outcome(?ONE_CHUNK_PAIR, fun unmutated/4))
    end}.

%% @doc The transaction pair reaches the block-time history check, which is the
%% first check its fixtures cannot satisfy: the history a block commits to is
%% carried state, and peers only serve it for the most recent hundred blocks.
%% Reaching it means the weave size was checked against the padded sizes of 26
%% real transactions and the transaction root against their data roots.
accepts_real_pair_with_transactions_test_() ->
    {timeout, 600, fun() ->
        ?assertEqual(
            <<"invalid-block-time-history-hash">>,
            outcome(?TX_PAIR, fun unmutated/4)
        )
    end}.

%% @doc The two clauses that stand in for an absent account tree select on the
%% chain state's tree and on nothing else, so a block cannot reach them by
%% carrying no transactions, and cannot escape them by carrying forty-three.
%%
%% Those two lines -- `check_txs([], ...) -> ok' and
%% `check_accounts([], ...) -> {ok, []}' -- are the shape of the failure this
%% subsystem exists to rule out: a node with its money checks switched off
%% accepts every block, and says so nowhere. They are asserted from both sides
%% here. The one-chunk pair is the subject because its child spends real
%% balances, so the difference between the two modes is the difference between
%% checking forty-three transactions and checking none.
%%
%% They are reached below `apply/3', which refuses the reduced mode outright
%% unless a node asks for it -- so a chain cannot be built this way, only a
%% single transition inspected. `require_accounts/2' owns that rule and
%% `reports_validation_mode_test_/0' pins it.
%%
%% Asserted on the checks directly rather than through `validate', as the proof
%% of work and the VDF chain are: what is under test is which clause matches,
%% and reaching it through the pipeline would spend a full proof of work per
%% assertion to learn the same thing.
absent_tree_clauses_test_() ->
    {timeout, 300, fun() ->
        Opts = test_opts(),
        {PrevHeight, NextHeight} = ?ONE_CHUNK_PAIR,
        State = pair_state({PrevHeight, NextHeight}, Opts),
        Bodies = transactions(NextHeight),
        Anchored = anchor(State, Bodies, Opts),
        Req =
            #{
                <<"next">> => fixture_message(NextHeight),
                <<"transactions">> => Bodies
            },
        {ok, Prev, Next, TXs} = inputs(State, Req, Opts),
        % The parent is read off the state, so the anchored one carries the
        % locked rewards the account transition releases as well as the tree.
        {ok, AnchoredPrev, _Next, _TXs} = inputs(Anchored, Req, Opts),
        ?assertNotEqual([], TXs),
        % Without a tree both stages answer their `[]' clause, and the block's
        % spending is not looked at.
        ?assertEqual([], lib_arweave_state:accounts(State, Opts)),
        ?assertEqual(ok, check_txs(Next, Prev, TXs, State, Opts)),
        ?assertEqual({ok, []}, check_accounts(Next, Prev, TXs, State, Opts)),
        % With one, neither clause can match. The transaction stage refuses the
        % block: its transactions anchor on recent blocks the state does not
        % carry. The account stage does not refuse it for that reason -- every
        % sender is funded, which is what its own anchor rule asks -- so the
        % two stages are told apart by which of them rejects, and a transaction
        % mutant cannot be answered by the account stage.
        ?assertNotEqual([], lib_arweave_state:accounts(Anchored, Opts)),
        ?assertEqual(
            <<"invalid-txs">>,
            rejection(check_txs(Next, AnchoredPrev, TXs, Anchored, Opts), Opts)
        ),
        ?assertEqual(
            <<"invalid-wallet-list-root">>,
            rejection(
                check_accounts(Next, AnchoredPrev, TXs, Anchored, Opts), Opts)
        )
    end}.

%% @doc The four scalar rules the account transition enforces -- the endowment
%% pool, the mining reward, the debt supply and the Kryder multiplier -- each
%% pinned by a mutant that breaks that rule and nothing else.
%%
%% Asserted against `check_accounts/6' on records for the reason
%% `pow_rules_test_/0' gives, and for one more. Every field here is inside the
%% child's signed hash, so a mutant reaching the check through the pipeline
%% would have to be re-signed, and re-signing moves the mining address, which
%% the proof of work reads two checks earlier. And nothing in the parent or the
%% carried state moves three of these expectations at all: the reward and the
%% pool are two halves of one conserved split, so `invalid-reward-pool' is
%% always named first; and below the v2 pricing height the debt supply and the
%% Kryder multiplier are the literals `0, 0, 1'. See `mutants/0'.
%%
%% The first assertion is what makes the other four evidence rather than
%% assumption: the unmutated pair runs the whole transition and is refused only
%% at the root, so each rule below is reached.
money_rules_test_() ->
    {timeout, 300, fun() ->
        Opts = test_opts(),
        {PrevHeight, NextHeight} = ?FIELD_PAIR,
        Bodies = transactions(NextHeight),
        State =
            anchor(pair_state({PrevHeight, NextHeight}, Opts), Bodies, Opts),
        {ok, Prev, Next, TXs} =
            inputs(
                State,
                #{
                    <<"next">> => fixture_message(NextHeight),
                    <<"transactions">> => Bodies
                },
                Opts
            ),
        Reject =
            fun(Block) ->
                rejection(check_accounts(Block, Prev, TXs, State, Opts), Opts)
            end,
        ?assertEqual(<<"invalid-wallet-list-root">>, Reject(Next)),
        ?assertEqual(
            <<"invalid-reward-pool">>,
            Reject(Next#block{ reward_pool = Next#block.reward_pool + 1 })
        ),
        ?assertEqual(
            <<"invalid-reward">>,
            Reject(Next#block{ reward = Next#block.reward + 1 })
        ),
        ?assertEqual(
            <<"invalid-debt-supply">>,
            Reject(Next#block{ debt_supply = 1 })
        ),
        ?assertEqual(
            <<"invalid-kryder-multiplier">>,
            Reject(Next#block{ kryder_plus_rate_multiplier = 2 })
        ),
        ?assertEqual(
            <<"invalid-kryder-multiplier">>,
            Reject(Next#block{ kryder_plus_rate_multiplier_latch = 1 })
        )
    end}.

%% @doc The three ways the vendored account transition refuses a block, each
%% mapped onto this module's error convention by `update_accounts/3'.
%%
%% None is reachable through `validate'. The anchor rule is masked by
%% `check_txs/6', which refuses the same block one check earlier and under the
%% same name; the mining address and the double signing proof are both inside
%% the child's signed hash. So they are asserted on records, for the reason
%% `pow_rules_test_/0' gives.
%%
%% The double signing proof is the one shape no fixture can supply. A proof is
%% published only when a miner signs two blocks at one height, which no block
%% in the corpus carries, so it is built here rather than mutated -- as two
%% identical signatures, the one malformation the transition names before it
%% verifies anything.
transition_rejections_test_() ->
    {timeout, 300, fun() ->
        Opts = test_opts(),
        {PrevHeight, NextHeight} = ?ONE_CHUNK_PAIR,
        Bodies = transactions(NextHeight),
        Funded =
            anchor(pair_state({PrevHeight, NextHeight}, Opts), Bodies, Opts),
        {ok, Prev, Next, TXs} =
            inputs(
                Funded,
                #{
                    <<"next">> => fixture_message(NextHeight),
                    <<"transactions">> => Bodies
                },
                Opts
            ),
        Reject =
            fun(Block, State) ->
                rejection(check_accounts(Block, Prev, TXs, State, Opts), Opts)
            end,
        % A tree that funds none of the senders. The transition has an anchor
        % rule of its own: an account that holds nothing and has never spent
        % may not be spent from.
        ?assertEqual(
            <<"invalid-txs">>,
            Reject(Next, Funded#{ <<"accounts">> => tree(#{}, Opts) })
        ),
        % A mining address banned for double signing collects no reward.
        ?assertEqual(
            <<"invalid-mining-address">>,
            Reject(
                Next,
                Funded#{
                    <<"accounts">> =>
                        tree(
                            #{
                                hb_util:encode(Next#block.reward_addr) =>
                                    banned()
                            },
                            Opts
                        )
                }
            )
        ),
        % A proof accusing a miner of signing the same thing twice accuses
        % nobody of anything.
        Signature = hb_util:decode(hash(64)),
        ?assertEqual(
            <<"invalid-double-signing-proof">>,
            Reject(
                Next#block{
                    double_signing_proof =
                        {
                            crypto:strong_rand_bytes(512),
                            Signature, 2, 1, <<>>,
                            Signature, 2, 1, <<>>
                        }
                },
                Funded
            )
        )
    end}.

%% @doc A body that is not a block is reported as a malformed encoding rather
%% than parsed into something else.
%%
%% `ar_serialize:binary_to_block/1' enforcing every field size structurally is
%% what lets the checks run without a catch-all around them, so its refusal has
%% to arrive as this module's error rather than as an exception.
rejects_corrupt_binary_test() ->
    {Prev, _Next} = ?FIELD_PAIR,
    Opts = test_opts(),
    Truncated = binary:part(fixture_binary(Prev), 0, 64),
    ?assertEqual(
        <<"invalid-block-encoding">>,
        rejection(
            hb_ao:resolve(
                base(#{ <<"body">> => Truncated }),
                <<"from-binary">>,
                Opts
            ),
            Opts
        )
    ).

%% @doc The transaction bodies a request supplies must be the block's own, in
%% the block's order.
%%
%% This reports 400 and reads like a caller-contract error, and it is a
%% consensus rule wearing one. A block header carries only identifiers, while
%% the weave size, the transaction root and the transaction checks all run over
%% the bodies the request hands in -- so a caller that could supply a different
%% set would have all three pass over transactions the block does not contain.
%%
%% Order is half the rule, and the half a set comparison would lose: the
%% transaction root is the Merkle root over the bodies at the offsets their
%% padded sizes give them, so a permutation of the same bodies is a different
%% weave.
requires_the_blocks_transactions_test_() ->
    {timeout, 300, fun() ->
        Opts = test_opts(),
        {PrevHeight, NextHeight} = ?ONE_CHUNK_PAIR,
        {_OtherPrev, OtherHeight} = ?TX_PAIR,
        State = pair_state({PrevHeight, NextHeight}, Opts),
        Reject =
            fun(Req) ->
                rejection(
                    hb_ao:resolve(
                        base(State),
                        Req#{ <<"path">> => <<"validate">> },
                        Opts
                    ),
                    Opts
                )
            end,
        % Another block's transactions.
        ?assertEqual(
            <<"missing-transactions">>,
            Reject(
                #{
                    <<"next">> => fixture_message(NextHeight),
                    <<"transactions">> => transactions(OtherHeight)
                }
            )
        ),
        % The block's own transactions, in an order it did not commit to.
        ?assertEqual(
            <<"missing-transactions">>,
            Reject(
                #{
                    <<"next">> => fixture_message(NextHeight),
                    <<"transactions">> =>
                        lists:reverse(transactions(NextHeight))
                }
            )
        ),
        % A request naming no block at all.
        ?assertEqual(<<"missing-next-block">>, Reject(#{}))
    end}.

%% @doc `check_vdf/3' must refuse anything that is not an explicit `valid =>
%% true', and must refuse it as an *error* rather than by returning it.
%%
%% This pins a fail-open. Matching `{ok, #{valid := true}}' inside a `maybe'
%% with no `else' would let a device answering `{ok, #{valid => false}}' become
%% the return value of `check_vdf/3', then of `checks/5', where `apply/3' binds
%% it as `Accounts'. Non-empty, so `require_accounts/2' would accept the block
%% and `accounts_checked/1' would report `true' -- a block applied with the VDF
%% chain unverified and the transaction and account stages never run, recorded
%% as fully validated.
%%
%% Asserted on the answers rather than through a fixture because no block-level
%% mutant can reach this call: every field `check_vdf/3' reads is read by a
%% check that runs earlier, and the child's whole nonce limiter info sits inside
%% the signed hash. That `checks/5' calls it at all is pinned separately, by
%% the reset-crossing fixture pair.
vdf_fails_closed_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    Refused =
        fun(Answer) ->
            case holds(hb_maps:get(<<"valid">>, Answer, false, Opts) == true,
                    <<"invalid-vdf-chain">>, <<"detail">>) of
                ok -> accepted;
                {error, Error} -> hb_maps:get(<<"message">>, Error, none, Opts)
            end
        end,
    ?assertEqual(accepted, Refused(#{ <<"valid">> => true })),
    ?assertEqual(<<"invalid-vdf-chain">>, Refused(#{ <<"valid">> => false })),
    % The string `true' is the shape a codec round-trip produces, and is not
    % the atom the check requires.
    ?assertEqual(
        <<"invalid-vdf-chain">>,
        Refused(#{ <<"valid">> => <<"true">> })
    ),
    ?assertEqual(<<"invalid-vdf-chain">>, Refused(#{})).

%% @doc The reset-crossing pair validates unmutated.
%%
%% Worth its own test rather than resting inside `mutation_test_': no pair whose
%% step range crosses an entropy reset had ever been through `checks/5' before
%% this fixture, so this is the first exercise of the reset branch of
%% `get_seed_data/2', of `check_chain/4''s below-line sub-range, and of the seed
%% rotation `solution_h0/3' feeds to h0. If this fails, the fixture has found a
%% live consensus divergence rather than a broken test.
accepts_reset_crossing_pair_test_() ->
    {timeout, 600, fun() ->
        ?assertEqual(valid, outcome(?RESET_PAIR, fun unmutated/4))
    end}.

%% @doc The `message' a check rejected its inputs with.
rejection({error, Error}, Opts) ->
    hb_maps:get(<<"message">>, Error, not_found, Opts).

%%% Mutation coverage. One mutant per check, each altering exactly the field
%%% that check guards and asserting that check's own error `message'. A mutant
%%% answering with any other message would mean the check never ran.

mutation_test_() ->
    {timeout, 1800, fun() ->
        lists:foreach(
            fun({Message, Pair, Mutate}) ->
                ?assertEqual(
                    {Message, Message},
                    {Message, outcome(Pair, Mutate)}
                )
            end,
            mutants()
        )
    end}.

%% @doc The mutants, in the order the checks they target run in.
mutants() ->
    [
        {<<"invalid-previous-block">>, ?FIELD_PAIR,
            parent(<<"indep-hash">>, hash(48))},
        {<<"invalid-height">>, ?FIELD_PAIR,
            parent(<<"height">>, 42)},
        {<<"invalid-previous-cumulative-diff">>, ?FIELD_PAIR,
            child(<<"previous-cumulative-diff">>, 1)},
        {<<"invalid-proof-size">>, ?FIELD_PAIR,
            child([<<"poa">>, <<"tx-path">>],
                hb_util:encode(<<0:(2177 * 8)>>))},
        {<<"invalid-chunk-hash">>, ?FIELD_PAIR,
            child(<<"chunk-hash">>, hash(32))},
        {<<"invalid-unpacked-chunk-hash">>, ?FIELD_PAIR,
            child(<<"unpacked-chunk-hash">>, hash(32))},
        {<<"invalid-signature">>, ?FIELD_PAIR,
            unsigned(<<"wallet-list">>, hash(48))},
        {<<"invalid-indep-hash">>, ?FIELD_PAIR,
            unsigned(<<"indep-hash">>, hash(48))},
        {<<"invalid-timestamp">>, ?FIELD_PAIR,
            parent(<<"timestamp">>, 4102444800)},
        {<<"invalid-step-number">>, ?FIELD_PAIR,
            parent([<<"nonce-limiter-info">>, <<"global-step-number">>], 1)},
        {<<"invalid-previous-solution-hash">>, ?FIELD_PAIR,
            parent(<<"hash">>, hash(32))},
        {<<"invalid-last-retarget">>, ?FIELD_PAIR,
            child(<<"last-retarget">>, 1)},
        {<<"invalid-difficulty">>, ?FIELD_PAIR,
            child(<<"diff">>, 12345)},
        {<<"invalid-cumulative-diff">>, ?FIELD_PAIR,
            child(<<"cumulative-diff">>, 12345)},
        {<<"invalid-replica-format">>, ?FIELD_PAIR,
            child(<<"replica-format">>, 0)},
        {<<"invalid-weave-size">>, ?TX_PAIR,
            transaction(<<"data-size">>, 4096)},
        {<<"invalid-tx-root">>, ?TX_PAIR,
            transaction(<<"data-root">>, hash(32))},
        {<<"invalid-block-index-root">>, ?FIELD_PAIR,
            parent(<<"hash-list-merkle">>, hash(48))},
        {<<"invalid-packing-threshold">>, ?FIELD_PAIR,
            parent(<<"packing-2-5-threshold">>, 1000000000000000000)},
        {<<"invalid-strict-data-split-threshold">>, ?FIELD_PAIR,
            parent(<<"strict-data-split-threshold">>, 1)},
        {<<"invalid-merkle-rebase-threshold">>, ?FIELD_PAIR,
            parent(<<"merkle-rebase-support-threshold">>, 1)},
        {<<"invalid-usd-to-ar-rate">>, ?FIELD_PAIR,
            child(<<"usd-to-ar-rate">>, [1, 11])},
        {<<"invalid-denomination">>, ?FIELD_PAIR,
            child(<<"denomination">>, 2)},
        {<<"invalid-price-per-gib-minute">>, ?FIELD_PAIR,
            parent(<<"price-per-gib-minute">>, 1)},
        {<<"invalid-reward-history-hash">>, ?FIELD_PAIR,
            parent(<<"reward-history-hash">>, hash(32))},
        {<<"invalid-block-time-history-hash">>, ?FIELD_PAIR,
            history(<<1, 1, 1, 1, 1, 1>>)},
        {<<"invalid-next-vdf-difficulty">>, ?FIELD_PAIR,
            parent([<<"nonce-limiter-info">>, <<"next-vdf-difficulty">>], 1)},
        {<<"invalid-seed-data">>, ?FIELD_PAIR,
            parent([<<"nonce-limiter-info">>, <<"next-seed">>], hash(48))},
        {<<"invalid-partition-number">>, ?FIELD_PAIR,
            child(<<"partition-number">>, 1000000)},
        {<<"invalid-nonce">>, ?FIELD_PAIR,
            child(<<"nonce">>, 320)},
        {<<"invalid-solution-hash">>, ?FIELD_PAIR,
            child(<<"hash-preimage">>, hash(32))},
        % The proof of work is the last check a re-signed block can reach. The
        % mining address is one of the solution hash's inputs, so re-signing
        % alone already breaks it, and no mutation of a child field can reach
        % past it. That is why this mutant asserts the solution hash and not
        % the preimage it alters: the preimage rule, and the difficulty rule
        % beside it, are pinned by `pow_rules_test_/0' instead, which calls
        % `check_pow/3' on records directly and so is not bound by the
        % signature.
        % The only mutant that reaches `check_vdf/3'. Every other field the VDF
        % chain verification reads is read by a check that runs before it, and
        % the child's whole nonce-limiter info sits inside the signed hash --
        % which is why this check had no mutant at all and its call site could
        % be deleted with the suite still green. At a reset line the parent's
        % `vdf-difficulty' is the exception: `get_seed_data/2' returns the
        % parent's `next-vdf-difficulty' instead, so nothing earlier reads it.
        {<<"invalid-vdf-chain">>, ?RESET_PAIR,
            parent([<<"nonce-limiter-info">>, <<"vdf-difficulty">>], 1)},
        {<<"invalid-tx-path">>, ?FIELD_PAIR,
            index_entry(<<"tx-root">>, hash(32))},
        % The three mutants below anchor an account tree, which is what takes
        % the two money stages out of the mode an absent tree puts them in.
        % They mutate the chain state rather than the child because both stages
        % run after the proof of work, which no re-signed block reaches.
        {<<"invalid-txs">>, ?ONE_CHUNK_PAIR,
            spending(fun unmutated/4)},
        {<<"invalid-reward-pool">>, ?FIELD_PAIR,
            spending(parent(<<"reward-pool">>, 1))},
        {<<"invalid-wallet-list-root">>, ?FIELD_PAIR,
            spending(fun unmutated/4)}
        % `invalid-reward', `invalid-debt-supply' and
        % `invalid-kryder-multiplier' have no mutant. All three compare a field
        % of the child against a value the parent and the carried state do not
        % determine, so no mutation of either can move the expectation without
        % moving something an earlier check already guards:
        %
        % The reward and the endowment pool are conserved. The transition splits
        % a fixed amount between them -- `{BaseReward + Take, Pool2 - Take}' --
        % so anything moving the reward moves the pool by the same amount, and
        % `invalid-reward-pool' is named first. The one input that moves them
        % independently is a transaction's fee, and altering a transaction body
        % is rejected by `check_txs/6' first, because the fee is signed.
        %
        % The debt supply and the Kryder multiplier are constants below fork
        % 2.9's v2 pricing height of 2,069,870, which no fetchable block has
        % reached: `ar_node_utils:update_accounts/3' takes the 2.5-era branch
        % and returns them as a literal `0, 0, 1'. Only the child's own declared
        % values can differ from that, and the child is signed.
        %
        % `money_rules_test_/0' pins all three on records instead, as
        % `pow_rules_test_/0' and `vdf_rules_test_/0' pin theirs.

        % `invalid-recall-byte' has no mutant either, and cannot be pinned that
        % way. It compares the recall byte the mining entropy computes against
        % the one the child declares, and both sides move together: the
        % declared byte is inside the signed hash, and the computed one is
        % derived from the mining address, which re-signing moves. So the only
        % mutant that could reach it is one the proof of work refuses first.
        % `invalid-mining-address' and `invalid-double-signing-proof' are the
        % same shape and are pinned on records by
        % `transition_rejections_test_/0'.

        % A block index that disagrees with the weave about a transaction root
        % is caught by the storage proof device rather than by `check_proof/8':
        % the device answers `{error, invalid-tx-path}', which propagates as
        % the result, and `invalid-poa' is reached only if the device were ever
        % to answer `{ok, #{valid => false}}'. It does not today, so
        % `invalid-poa' and `invalid-poa2' name a path nothing takes. Recorded
        % here rather than papered over with a mutant that cannot fire.

        % No mutant reaches the VDF chain. Every field `check_vdf/3' reads is
        % read by a check that runs before it -- the parent's output by
        % `check_step_number/2', the parent's seeds by `check_seed_data/3', and
        % the child's whole nonce limiter info sits inside the signed hash --
        % except across an entropy reset, where `get_seed_data/2' returns the
        % parent's `next-vdf-difficulty' and the parent's own `vdf-difficulty'
        % is read by nothing earlier. That is what `?RESET_PAIR' is for, and it
        % is the mutant directly above that pins the call site.
    ].

%%% Test helpers.

%% @doc Resolve a key against a base message on this device.
resolve(Base, Key) ->
    {ok, Result} = hb_ao:resolve(base(Base), Key, test_opts()),
    Result.

%% @doc Resolve a key and read one field of the result. A result carries the
%% resolver's own bookkeeping alongside the device's keys, so it is read field
%% by field rather than compared whole.
resolve_field(Base, Key, Field) ->
    hb_maps:get(Field, resolve(Base, Key), not_found, test_opts()).

%% @doc Name this device on a base message.
base(Base) ->
    Base#{ <<"device">> => <<"arweave-block@2.9">> }.

%% @doc Read a fixture block in Arweave's binary form.
fixture_binary(Height) ->
    {ok, Binary} = file:read_file(fixture_path(Height, <<".bin">>)),
    Binary.

%% @doc Read a fixture block in Arweave's JSON form.
fixture_json(Height) ->
    {ok, Body} = file:read_file(fixture_path(Height, <<".json">>)),
    Body.

fixture_path(Height, Extension) ->
    <<"test/fixtures/arweave/block-", (integer_to_binary(Height))/binary,
        Extension/binary>>.

%% @doc Parse a fixture from its binary form, through the device.
fixture_message(Height) ->
    resolve(#{ <<"body">> => fixture_binary(Height) }, <<"from-binary">>).

%% @doc Parse a fixture from its JSON form, through the device.
json_message(Height) ->
    resolve(#{ <<"body">> => fixture_json(Height) }, <<"from-json">>).

%% @doc Read a block's nonce limiter info out of its message.
nonce_limiter(Block) ->
    maps:get(<<"nonce-limiter-info">>, Block).

%% @doc A random value of the size a block field of that name carries, encoded
%% as the message carries it.
hash(Size) ->
    hb_util:encode(crypto:strong_rand_bytes(Size)).

%% @doc Build the chain state whose block is the fixture at `Height'.
%%
%% The block-time history is the one that block committed to, where it has been
%% frozen; peers serve it only for the last hundred or so blocks, so the older
%% fixtures have none. The reward history is empty, which every check that
%% consults it tolerates: the reward history hash has chained onto the previous
%% hash rather than folding the list since fork 2.8, and it reads only the
%% element the block itself contributes.
chain_state(Height) ->
    #{
        <<"block">> => fixture_message(Height),
        <<"block-time-history">> => block_time_history(Height),
        <<"reward-history">> => [],
        <<"block-index">> => [],
        <<"accounts">> => [],
        <<"recent-blocks">> => []
    }.

block_time_history(Height) ->
    Path =
        <<"test/fixtures/arweave/block-time-history-",
            (integer_to_binary(Height))/binary, ".bin">>,
    case file:read_file(Path) of
        {ok, Body} -> #{ <<"body">> => Body };
        {error, enoent} -> []
    end.

%% @doc The chain state a pair is validated against: the parent's, carrying the
%% slice of block index the child's recall bytes fall in. The index belongs to
%% the pair rather than to either block -- it is the parent's index, but which
%% part of it is needed is decided by where the child's mining entropy pointed.
pair_state({PrevHeight, NextHeight}, Opts) ->
    (chain_state(PrevHeight))#{
        <<"block-index">> => block_index(NextHeight, Opts)
    }.

%% @doc Build the block index a block's proofs of access are checked against,
%% from the `/block_index2' wire form frozen alongside it by
%% `scripts/fetch-arweave-block-index-fixtures.sh'.
%%
%% It is a slice, not the whole index. A recall byte may point anywhere in a
%% 389 TB weave and the real index is two million entries, so what is frozen is
%% the block that wrote each recall byte together with its immediate
%% predecessor -- which is where `bounds/3' reads the start of the range. The
%% heights inside the slice are therefore its own, not the weave's, which makes
%% `at/3' meaningless over it; `bounds/3' reads weave sizes alone and is exact.
%% Every entry is the real one a peer serves, so the transaction root the proof
%% of access is checked under is the weave's and not the block's own claim.
block_index(Height, Opts) ->
    Path =
        <<"test/fixtures/arweave/block-index-",
            (integer_to_binary(Height))/binary, ".bin">>,
    case file:read_file(Path) of
        {ok, Body} ->
            hb_util:ok(
                hb_ao:resolve(
                    #{ <<"device">> => <<"arweave-block-index@2.9">> },
                    #{
                        <<"path">> => <<"from-binary">>,
                        <<"body">> => Body
                    },
                    Opts
                )
            );
        {error, enoent} ->
            []
    end.

%% @doc Read the frozen transaction bodies of a block, in the block's own
%% order, as the transaction messages `apply/3' takes.
transactions(Height) ->
    [
        lib_arweave_tx:from_tx(
            lib_arweave_tx:from_json_struct(transaction_struct(Height, ID)),
            test_opts()
        )
    ||
        ID <- maps:get(<<"txs">>, fixture_message(Height))
    ].

transaction_struct(Height, ID) ->
    {ok, Body} =
        file:read_file(
            <<"test/fixtures/arweave/txs-", (integer_to_binary(Height))/binary,
                "/", ID/binary, ".json">>
        ),
    hb_json:decode(Body).

%% @doc Apply a mutation to a real block pair and return what validating it
%% answered: the `message' of the check that rejected it, or `valid' when every
%% check accepted it.
%%
%% One store serves the whole call. The block index the state carries is
%% written into it as runs, so building the index under a store the validation
%% then resolves against is not a convenience -- a second store would leave the
%% lookup reading a run that is not there.
outcome({_PrevHeight, NextHeight} = Pair, Mutate) ->
    Opts = test_opts(),
    {State, Next, TXs} =
        Mutate(
            pair_state(Pair, Opts),
            fixture_message(NextHeight),
            transactions(NextHeight),
            Opts
        ),
    case
        hb_ao:resolve(
            base(State),
            #{
                <<"path">> => <<"validate">>,
                <<"next">> => Next,
                <<"transactions">> => TXs
            },
            Opts
        )
    of
        {ok, _Result} -> valid;
        {error, Error} -> hb_maps:get(<<"message">>, Error, not_found, Opts)
    end.

%% @doc The identity mutation, for the tests that apply a real pair unaltered.
unmutated(State, Next, TXs, _Opts) ->
    {State, Next, TXs}.

%% @doc A mutant that alters one field of the parent block. Validating a child
%% never verifies its parent's signature, so no re-signing is needed.
parent(Path, Value) ->
    fun(State, Next, TXs, _Opts) ->
        {
            State#{
                <<"block">> => set(maps:get(<<"block">>, State), Path, Value)
            },
            Next,
            TXs
        }
    end.

%% @doc A mutant that alters one field of the child block and re-signs it, so
%% that the check under test fires rather than the signature check.
child(Path, Value) ->
    fun(State, Next, TXs, _Opts) ->
        {State, resign(State, set(Next, Path, Value)), TXs}
    end.

%% @doc A mutant that alters one field of the child block without re-signing,
%% for the two checks that guard the signature itself.
unsigned(Path, Value) ->
    fun(State, Next, TXs, _Opts) -> {State, set(Next, Path, Value), TXs} end.

%% @doc A mutant that alters one field of every transaction body supplied
%% alongside the block. Transaction bodies are not part of the header, so
%% nothing needs re-signing.
transaction(Key, Value) ->
    fun(State, Next, TXs, _Opts) ->
        {State, Next, [ TX#{ Key => Value } || TX <- TXs ]}
    end.

%% @doc A mutant that rewrites one field of every entry of the block index the
%% chain state carries. The index is fetched from a peer like everything else,
%% so this is the mutant for a peer that serves a plausible index describing a
%% weave that is not the one the block was mined against.
index_entry(Key, Value) ->
    fun(State, Next, TXs, Opts) ->
        Index = hb_maps:get(<<"block-index">>, State, [], Opts),
        {
            State#{
                <<"block-index">> =>
                    Index#{
                        <<"runs">> =>
                            hb_maps:map(
                                fun(_Number, Run) ->
                                    rewrite_run(Run, Key, Value, Opts)
                                end,
                                hb_maps:get(<<"runs">>, Index, #{}, Opts),
                                Opts
                            )
                    }
            },
            Next,
            TXs
        }
    end.

%% @doc Rewrite one field of every entry of a stored run, leaving its width and
%% every other field alone. The run is held in the store as the packed bytes
%% `dev_arweave_block_index' reads back, so the mutation is applied there and
%% the run written again.
rewrite_run(Run, <<"tx-root">>, Value, Opts) ->
    Body = hb_util:ok(hb_cache:read(Run, Opts)),
    TXRoot = hb_util:decode(Value),
    Padding = (32 - byte_size(TXRoot)) * 8,
    Rewritten =
        <<
            <<
                (binary:part(Entry, 0, 56))/binary,
                (byte_size(TXRoot)):8,
                TXRoot/binary,
                0:Padding
            >>
        ||
            << Entry:89/binary >> <= Body
        >>,
    hb_util:ok(
        hb_cache:write_binary(
            <<"~arweave-block-index@2.9/runs/mutated">>,
            Rewritten,
            Opts
        )
    ).

%% @doc Turn a mutant into one that also anchors an account tree, so that the
%% transaction and account stages run over balances instead of taking the
%% clauses that stand in for an absent tree.
%%
%% The tree is deliberately not the block's own. Reproducing that one needs the
%% whole mainnet wallet list -- three hundred thousand accounts, which peers
%% serve for about fifty blocks after the root they belong to -- and a mutation
%% test does not want it: what it wants is for the check to fire. Every
%% transition these mutants run therefore ends at a root the block never
%% signed, and each mutant is named by whichever check rejects it before the
%% root is reached.
spending(Mutate) ->
    fun(State, Next, TXs, Opts) ->
        {Mutated, Next2, TXs2} = Mutate(State, Next, TXs, Opts),
        {anchor(Mutated, TXs2, Opts), Next2, TXs2}
    end.

%% @doc Anchor a chain state to an account tree and to the locked reward its
%% transition releases.
%%
%% The reward history goes with the tree rather than into `chain_state/1'.
%% `ar_rewards:get_oldest_locked_address/1' is the last element of the locked
%% window, so an empty history has no oldest address and raises rather than
%% reporting; only the account stage reads it, and only the account stage needs
%% it. It is invisible to the checks that ran before: since fork 2.8 the reward
%% history hash folds the block's own element onto the parent's hash and reads
%% no further into the list.
anchor(State, TXs, Opts) ->
    State#{
        <<"accounts">> => accounts(TXs, Opts),
        <<"reward-history">> =>
            lib_arweave_state:reward_history_message(
                [{hb_util:decode(hash(32)), 17, 42, 1}])
    }.

%% @doc An account tree holding a funded account for every sender of the
%% block's transactions, built through the account tree device as a peer's
%% would be.
%%
%% The senders are funded because the account stage has an anchor rule of its
%% own -- `ar_node_utils' refuses a transaction from an account that holds
%% nothing and has never spent -- and it reports `invalid-txs' too. Funding
%% them satisfies that rule, so a transaction mutant can only be naming
%% `check_txs/6'. Without this, deleting `check_txs/6' from `checks/5'
%% altogether left the mutant green.
accounts(TXs, Opts) ->
    tree(
        maps:from_list(
            [
                {
                    hb_util:encode(
                        (lib_arweave_tx:to_tx(TX, Opts))#tx.owner_address),
                    funded()
                }
            ||
                TX <- TXs
            ]
        ),
        Opts
    ).

%% @doc An account tree holding the accounts a diff describes, built through
%% the account tree device as a peer's pages would be. One account is always
%% added, so that an empty diff still yields a tree rather than an empty state,
%% which the account and transaction stages would read as no tree at all.
tree(Diff, Opts) ->
    hb_util:ok(
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-wallets@2.9">> },
            #{
                <<"path">> => <<"apply">>,
                <<"diff">> => Diff#{ hash(32) => funded() }
            },
            Opts
        )
    ).

%% @doc An account with more than the block's transactions can spend.
funded() ->
    #{
        <<"balance">> => 1000000000000000,
        <<"last-tx">> => hb_util:encode(<<>>)
    }.

%% @doc An account the network has banned from mining, for double signing.
banned() ->
    (funded())#{
        <<"denomination">> => 1,
        <<"mining-permission">> => false
    }.

%% @doc A mutant that replaces the block-time history the chain state carries.
history(Body) ->
    fun(State, Next, TXs, _Opts) ->
        {State#{ <<"block-time-history">> => #{ <<"body">> => Body } },
            Next, TXs}
    end.

%% @doc Set a field of a block message, addressed by key or by a two-element
%% path into one of its nested messages.
set(Block, [Key, Inner], Value) ->
    Block#{ Key => (maps:get(Key, Block))#{ Inner => Value } };
set(Block, Key, Value) ->
    Block#{ Key => Value }.

%% @doc Re-sign a mutated block with a fresh key, so that its signature and
%% its identifier are consistent with the mutation and the check under test is
%% the one that fires.
%%
%% The mining address moves with the key, and two checks read it: the reward
%% history hash, which this recomputes so that the address change is invisible
%% to it, and the proof of work, which is the last check a re-signed block can
%% reach. Recomputing the hash here mirrors the check it is fixing up, which is
%% acceptable only because that check has a mutant of its own -- one that
%% alters the parent's hash and is not re-signed.
resign(State, Block) ->
    {Priv, {KeyType, Pub}} = ar_wallet:new(),
    Keyed =
        Block#{
            <<"reward-key">> => hb_util:encode(Pub),
            <<"reward-addr">> =>
                hb_util:encode(ar_wallet:to_address({KeyType, Pub}))
        },
    Signable =
        Keyed#{
            <<"reward-history-hash">> =>
                hb_util:encode(reward_history_hash(State, Keyed))
        },
    Record = lib_arweave_block:to(Signable, test_opts()),
    SignedHash = ar_block:generate_signed_hash(Record),
    Signature =
        ar_wallet:sign(
            Priv,
            ar_block:get_block_signature_preimage(
                Record#block.cumulative_diff,
                Record#block.previous_cumulative_diff,
                <<(Record#block.previous_solution_hash)/binary,
                    SignedHash/binary>>,
                Record#block.height
            )
        ),
    Signable#{
        <<"signature">> => hb_util:encode(Signature),
        <<"indep-hash">> =>
            hb_util:encode(ar_block:indep_hash2(SignedHash, Signature))
    }.

%% @doc The reward history hash a block with this mining address must declare.
reward_history_hash(State, Block) ->
    Prev = lib_arweave_state:previous_block(State, test_opts()),
    Next = lib_arweave_block:to(Block, test_opts()),
    ar_rewards:reward_history_hash(
        Next#block.height,
        Prev#block.reward_history_hash,
        ar_rewards:trim_locked_rewards(
            Next#block.height,
            [
                {
                    Next#block.reward_addr,
                    ar_difficulty:get_hash_rate_fixed_ratio(Next),
                    Next#block.reward,
                    Next#block.denomination
                }
            |
                Prev#block.reward_history
            ]
        )
    ).

%% @doc A block validated without an account tree says so, in the result and on
%% the chain state it produces.
%%
%% The fixtures carry no account tree -- one cannot be frozen, since peers stop
%% serving a wallet list about a hundred blocks below the tip -- so this pair
%% takes the reduced mode, and that is exactly the case the marker exists for.
%% Without the marker, two blocks validated in different modes are
%% indistinguishable: `validate' answers a byte-identical `#{valid => true}'
%% either way, and a node running with its account and transaction checks
%% disabled reads exactly like one running with them.
reports_validation_mode_test_() ->
    {timeout, 600, fun() ->
        Opts = test_opts(),
        {PrevHeight, NextHeight} = ?FIELD_PAIR,
        State = pair_state({PrevHeight, NextHeight}, Opts),
        ?assertEqual([], lib_arweave_state:accounts(State, Opts)),
        {ok, Result} =
            hb_ao:resolve(
                base(State),
                #{
                    <<"path">> => <<"validate">>,
                    <<"next">> => fixture_message(NextHeight),
                    <<"transactions">> => transactions(NextHeight)
                },
                Opts
            ),
        ?assertEqual(
            false,
            hb_util:atom(
                hb_maps:get(<<"accounts-checked">>, Result, missing, Opts))
        ),
        ApplyReq =
            #{
                <<"path">> => <<"apply">>,
                <<"next">> => fixture_message(NextHeight),
                <<"transactions">> => transactions(NextHeight)
            },
        % Naming the mode is enough for `validate', which answers about one
        % block. `apply' hands back the state the next block is checked
        % against, so the reduced mode has to be asked for rather than fallen
        % into: the same transition is refused outright at the default.
        ?assertEqual(
            <<"accounts-not-checked">>,
            rejection(hb_ao:resolve(base(State), ApplyReq, Opts), Opts)
        ),
        {ok, Applied} =
            hb_ao:resolve(
                base(State),
                ApplyReq,
                Opts#{ <<"arweave-require-accounts">> => false }
            ),
        ?assertEqual(
            false,
            hb_util:atom(
                hb_maps:get(
                    <<"accounts-checked">>,
                    hb_private:reset(Applied),
                    missing,
                    Opts
                )
            )
        )
    end}.

%% @doc A transaction whose owner is 512 zero bytes must not take the validator
%% down.
%%
%% That owner is a well-formed RSA field, not a sentinel, but
%% `ar_tx:get_owner_address/1' answers the atom `not_set' for it -- a HyperBEAM
%% divergence from upstream, which returns `sha256(Owner)' like any other. A
%% peer chooses this field: `dev_arweave_sync' fetches bodies by id and does not
%% re-derive their contents, and `transactions/3' compares only the declared
%% `id', so a body with the right id and a zero owner reaches `balances/5'. The
%% encoder raises on the atom, and it raises *after* the proof of work, proof of
%% access and VDF chain have been computed -- so one peer costs the node a pass
%% and does it again every cron tick.
%%
%% `balances/5' sits inside `check_txs/6', which no mutant driven through
%% `validate' can reach: the fixtures carry no account tree, so `check_txs/5'
%% takes its reduced-mode clause and the whole money path is skipped. Such a
%% mutant proves nothing here -- the block validates with the owner zeroed or
%% not, which was checked rather than assumed. The tree is synthesised instead,
%% as `money_rules_test_' does, so that the call site actually executes.
zero_owner_is_an_address_not_an_atom_test_() ->
    {timeout, 300, fun() ->
        Opts = test_opts(),
        % `?ONE_CHUNK_PAIR' rather than `?FIELD_PAIR': the latter's child
        % carries no transactions at all, so there would be no owner to zero.
        {PrevHeight, NextHeight} = ?ONE_CHUNK_PAIR,
        [First | Rest] = transactions(NextHeight),
        Zeroed =
            [ First#{ <<"owner">> => hb_util:encode(<<0:4096>>) } | Rest ],
        TX = lib_arweave_tx:to_tx(hd(Zeroed), Opts),
        % The vendored function answers the atom, and the atom is what the
        % encoder cannot take. Both halves are asserted so that a change to
        % either is loud.
        ?assertEqual(not_set, ar_tx:get_owner_address(TX)),
        ?assertError(badarg, hb_util:encode(ar_tx:get_owner_address(TX))),
        % The record's own field is upstream's value for the same owner.
        ?assertEqual(
            ar_wallet:to_address(<<0:4096>>, TX#tx.signature_type),
            TX#tx.owner_address
        ),
        % And the money path runs over it and answers rather than raising.
        % This is the assertion that pins the call site: with
        % `ar_tx:get_owner_address/1' back in `balances/5' this is `badarg'.
        State =
            anchor(pair_state({PrevHeight, NextHeight}, Opts), Zeroed, Opts),
        {ok, Prev, Next, TXs} =
            inputs(
                State,
                #{
                    <<"next">> => fixture_message(NextHeight),
                    <<"transactions">> => Zeroed
                },
                Opts
            ),
        ?assertMatch({error, _}, check_txs(Next, Prev, TXs, State, Opts))
    end}.
