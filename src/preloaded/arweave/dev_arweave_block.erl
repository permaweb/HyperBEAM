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
%%% Every check reports its own error `message', so a rejection identifies the
%%% consensus rule the block broke.
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
    lib_arweave_history,
    lib_arweave_tx,
    lib_arweave_accounts
]).
-compile({no_auto_import, [apply/3]}).
-export([info/1, apply/3, validate/3]).
-export([id/3, signed_hash/3, verify_signature/3]).
-export([from_binary/3, to_binary/3, from_json/3, to_json/3]).
-ifdef(TEST).
-export([
    holds/3,
    check_step_number/2,
    check_reward_history_hash/2,
    check_block_time_history_hash/2
]).
-endif.
-include("include/hb.hrl").
-include("include/ar_consensus.hrl").

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

%% @doc The block's VDF step is ahead of the parent's, carries the consensus
%% suffix of the steps between the two, and anchors on the parent's output.
check_step_number(Next, Prev) ->
    Info = Next#block.nonce_limiter_info,
    PrevInfo = Prev#block.nonce_limiter_info,
    StepNumber = ar_block:vdf_step_number(Next),
    PrevStepNumber = ar_block:vdf_step_number(Prev),
    Distance = StepNumber - PrevStepNumber,
    holds(
        ar_nonce_limiter:is_ahead_on_the_timeline(Info, PrevInfo)
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
                        lib_arweave_state:next_reward_history(
                            State, Next, Opts),
                    <<"block-time-history">> =>
                        lib_arweave_state:next_block_time_history(
                            State, Next, Prev, Opts),
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
