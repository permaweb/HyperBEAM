%%% @doc An AO-Core interface to Arweave's nonce limiter -- the verifiable
%%% delay function that paces block production, and the only thing standing
%%% between the weave and a miner with a faster clock.
%%%
%%% There are two verification keys and they are not interchangeable.
%%% `verify-chain' recomputes every VDF step between a block and its parent,
%%% anchored on the parent's own output, and is the only key that returns a
%%% `valid' claim. `verify-step' recomputes the 25 checkpoints of the final
%%% step only; its anchor comes from the block's own unverified `steps' list,
%%% so it proves that a second of VDF work happened somewhere and nothing
%%% about the chain. It therefore returns a `pre-filter' verdict carrying
%%% `sufficient' `false', never a `valid' one, and `verify-chain' recomputes
%%% the final step itself rather than trusting a prior `verify-step'. A caller
%%% branching on `valid' cannot be satisfied by the cheap key.
%%%
%%% Three facts about the protocol shape the code, and each is easy to get
%%% backwards:
%%%
%%% `steps' and `last-step-checkpoints' are newest-first, as they are on the
%%% wire; the NIF wants ascending. A validator that reverses one and not the
%%% other rejects every block -- or, on symmetric data, appears to work.
%%%
%%% The NIF's reset path is dead upstream: `verify_no_reset/6' always passes
%%% `ResetStepNumber' zero. At a reset line the entropy is mixed in Erlang,
%%% between two verification calls made at two different difficulties -- the
%%% parent's before the line and the block's after it. Driving the NIF's own
%%% reset branch would be a consensus divergence.
%%%
%%% The difficulty is read from the pair of blocks being checked, never from
%%% `?VDF_DIFFICULTY'. Mainnet has been away from that constant since the
%%% first retarget; a validator that reached for it would silently disagree
%%% with the network. `seed-data' is what pins it: a block's declared
%%% `vdf-difficulty' is one of the five fields that must match what its parent
%%% implies.
-module(dev_arweave_vdf).
-implements(<<"arweave-vdf@2.9">>).
-device_libraries([lib_arweave_vdf]).
-export([info/1, verify_chain/3, verify_step/3, seed_data/3]).
-export([reset_point/3, next_difficulty/3]).
-include("include/hb.hrl").
-include("include/ar_vdf.hrl").
-include("include/ar_consensus.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Export only the nonce limiter operations, leaving message manipulation
%% to `message@1.0'.
info(_Base) ->
    #{
        excludes => [<<"keys">>, <<"set">>, <<"set-path">>, <<"remove">>]
    }.

%% @doc Verify every VDF step a block claims to have advanced the timeline by,
%% against its parent. This is the trustless key: the chain is anchored on the
%% parent's output, so nothing the block asserts about itself can satisfy it.
%%
%% The cheap structural checks run first, then the final step's checkpoints,
%% then the chain itself -- `Delta' steps of `25 * vdf-difficulty' SHA-256
%% invocations each, which no amount of care makes fast.
verify_chain(Base, Req, Opts) ->
    Info = info_message(<<"nonce-limiter-info">>, Base, Req, Opts),
    PrevInfo = info_message(<<"prev-nonce-limiter-info">>, Base, Req, Opts),
    PrevIndepHash = required(<<"prev-indep-hash">>, Base, Req, Opts),
    PrevWeaveSize = hb_util:int(required(<<"prev-weave-size">>, Base, Req, Opts)),
    Threads = threads(Base, Req, Opts),
    maybe
        ok ?= check_step_range(Info, PrevInfo, Opts),
        ok ?= check_output(Info, Opts),
        ok ?= check_prev_output(Info, PrevInfo, Opts),
        ok ?= check_seed_data(Info, PrevInfo, PrevIndepHash, PrevWeaveSize, Opts),
        ok ?= check_last_step(Info, PrevInfo, Threads, Opts),
        ok ?= check_chain(Info, PrevInfo, Threads, Opts),
        {ok, #{ <<"valid">> => true }}
    end.

%% @doc Recompute the 25 checkpoints of a block's final step -- one second of
%% VDF work, against the several minutes `verify-chain' costs.
%%
%% This is a pre-filter and nothing more. Its anchor is the second entry of the
%% block's own `steps' list, which at this point is an unverified assertion, so
%% a block that tampered with its parent's output consistently passes here and
%% fails `verify-chain'. Hence the verdict shape: no `valid' key, and
%% `sufficient' stated as `false' rather than left to be inferred.
verify_step(Base, Req, Opts) ->
    Info = info_message(<<"nonce-limiter-info">>, Base, Req, Opts),
    PrevInfo = info_message(<<"prev-nonce-limiter-info">>, Base, Req, Opts),
    case lib_arweave_vdf:last_step_checkpoints(
            Info, PrevInfo, threads(Base, Req, Opts), Opts) of
        true ->
            {ok,
                #{
                    <<"pre-filter">> => <<"passed">>,
                    <<"sufficient">> => false
                }
            };
        false ->
            {error, error_message(<<"invalid-last-step-checkpoints">>,
                <<"The final step's checkpoints do not recompute.">>)}
    end.

%% @doc Return the seed data a block mined at `step-number' must declare, given
%% its parent's nonce limiter info, independent hash and weave size. All five
%% fields rotate together, and only when the step range crosses a reset line.
seed_data(Base, Req, Opts) ->
    StepNumber = hb_util:int(required(<<"step-number">>, Base, Req, Opts)),
    PrevInfo = info_message(<<"prev-nonce-limiter-info">>, Base, Req, Opts),
    PrevIndepHash = required(<<"prev-indep-hash">>, Base, Req, Opts),
    PrevWeaveSize = hb_util:int(required(<<"prev-weave-size">>, Base, Req, Opts)),
    case step_number(PrevInfo, Opts) of
        PrevStepNumber when PrevStepNumber >= StepNumber ->
            {error, error_message(<<"stale-step-number">>,
                <<"The step number is not ahead of the parent's.">>)};
        _ ->
            {ok,
                lib_arweave_vdf:seed_data(
                    StepNumber,
                    PrevInfo,
                    PrevIndepHash,
                    PrevWeaveSize,
                    Opts
                )
            }
    end.

%% @doc Return the entropy reset line the step range crosses, or `none'.
reset_point(Base, Req, Opts) ->
    PrevStepNumber =
        hb_util:int(required(<<"prev-step-number">>, Base, Req, Opts)),
    StepNumber = hb_util:int(required(<<"step-number">>, Base, Req, Opts)),
    ResetPoint =
        case entropy_reset_point(PrevStepNumber, StepNumber) of
            none -> <<"none">>;
            Line -> Line
        end,
    {ok, #{ <<"reset-point">> => ResetPoint }}.

%% @doc Compute the VDF difficulty scheduled by the block after the one
%% described. Every input is the *previous* block's: its `height', the two
%% difficulties from its nonce limiter info, and its `block-time-history'. The
%% retarget tests `height + 1', so a caller passing the new block's height
%% would shift the retarget by one block.
next_difficulty(Base, Req, Opts) ->
    Height = hb_util:int(required(<<"height">>, Base, Req, Opts)),
    VDFDifficulty = hb_util:int(required(<<"vdf-difficulty">>, Base, Req, Opts)),
    NextVDFDifficulty =
        hb_util:int(required(<<"next-vdf-difficulty">>, Base, Req, Opts)),
    History = get_first(<<"block-time-history">>, Base, Req, [], Opts),
    {ok,
        #{
            <<"next-vdf-difficulty">> =>
                lib_arweave_vdf:next_vdf_difficulty(
                    Height,
                    VDFDifficulty,
                    NextVDFDifficulty,
                    History,
                    Opts
                )
        }
    }.

%%% Internal functions.

%% @doc Check that the block advanced the timeline, that it did so by a gap we
%% can verify from block data alone, and that it carries exactly one step
%% output per step in the range. An empty or short list fails here: a step list
%% the chain verification never runs over would let the block through having
%% proven nothing.
%%
%% The three failures name themselves apart. A step number that does not
%% advance and a step list that does not cover the range are different faults
%% in different fields, and a block that does not advance the timeline fails
%% both at once -- so sharing one message would leave whichever check runs
%% second untestable, its removal invisible to any mutant.
check_step_range(Info, PrevInfo, Opts) ->
    Count = length(steps(Info, Opts)),
    case step_number(Info, Opts) - step_number(PrevInfo, Opts) of
        Delta when Delta =< 0 ->
            {error, error_message(<<"stale-step-number">>,
                <<"The step number is not ahead of the parent's.">>)};
        Delta when Delta > ?NONCE_LIMITER_MAX_CHECKPOINTS_COUNT ->
            {error, error_message(<<"step-gap-too-large">>,
                <<"The step gap cannot be verified from block data alone.">>)};
        Delta when Count =/= Delta ->
            {error, error_message(<<"invalid-step-count">>,
                <<"The step list does not cover the step range.">>)};
        _ ->
            ok
    end.

%% @doc Check that the block's `output' -- the entropy its solution was mined
%% against -- is the newest entry of its step list. The list is newest-first,
%% so an ascending list fails here rather than surviving to be verified as a
%% chain that runs the wrong way.
check_output(Info, Opts) ->
    Output = required(<<"output">>, Info, Opts),
    case steps(Info, Opts) of
        [Output | _] ->
            ok;
        _ ->
            {error, error_message(<<"invalid-output">>,
                <<"The output is not the newest entry of the step list.">>)}
    end.

%% @doc Check that the block names its parent's output as the point its own
%% steps continue from.
check_prev_output(Info, PrevInfo, Opts) ->
    PrevOutput = required(<<"output">>, PrevInfo, Opts),
    case required(<<"prev-output">>, Info, Opts) of
        PrevOutput ->
            ok;
        _ ->
            {error, error_message(<<"invalid-prev-output">>,
                <<"The block does not continue from its parent's output.">>)}
    end.

%% @doc Check the five seed fields against what the parent implies. This is
%% also what pins the difficulty the chain is then verified at: a block cannot
%% declare a `vdf-difficulty' its parent did not schedule.
check_seed_data(Info, PrevInfo, PrevIndepHash, PrevWeaveSize, Opts) ->
    Expected =
        lib_arweave_vdf:seed_data(
            step_number(Info, Opts),
            PrevInfo,
            PrevIndepHash,
            PrevWeaveSize,
            Opts
        ),
    case seed_data_claim(Info, Opts) of
        Expected ->
            ok;
        _ ->
            % Named apart from `~arweave-block@2.9''s `invalid-seed-data', which
            % checks the same property one stage earlier. Sharing a message made
            % the two checks mutually masking: a mutant that should have pinned
            % the block-level check was caught here instead when that check was
            % removed, and caught there when this device was never reached, so
            % deleting *either* left the suite green while deleting both
            % accepted the block.
            {error, error_message(<<"invalid-vdf-seed-data">>,
                <<"The seed data does not match the parent's.">>)}
    end.

%% @doc Recompute the final step's checkpoints. `verify-chain' does this itself
%% rather than accepting a prior `verify-step' result, so that no sequence of
%% calls can accept a block on the cheap key alone.
check_last_step(Info, PrevInfo, Threads, Opts) ->
    case lib_arweave_vdf:last_step_checkpoints(Info, PrevInfo, Threads, Opts) of
        true ->
            ok;
        false ->
            {error, error_message(<<"invalid-last-step-checkpoints">>,
                <<"The final step's checkpoints do not recompute.">>)}
    end.

%% @doc Recompute the whole step chain from the parent's output. When the range
%% crosses a reset line the steps below it are verified at the parent's
%% difficulty and those above it at the block's, with the entropy mixed between
%% the two calls -- the NIF is never asked to do the mixing.
check_chain(Info, PrevInfo, Threads, Opts) ->
    StepNumber = step_number(Info, Opts),
    PrevStepNumber = step_number(PrevInfo, Opts),
    PrevOutput = decode(<<"output">>, PrevInfo, Opts),
    Ascending =
        lists:reverse([ decode(Step) || Step <- steps(Info, Opts) ]),
    Result =
        case entropy_reset_point(PrevStepNumber, StepNumber) of
            none ->
                ar_nonce_limiter:verify_no_reset(
                    PrevStepNumber,
                    PrevOutput,
                    ?VDF_CHECKPOINT_COUNT_IN_STEP,
                    Ascending,
                    Threads,
                    difficulty(Info, Opts)
                );
            ResetPoint ->
                ar_nonce_limiter:verify(
                    PrevStepNumber,
                    PrevOutput,
                    ?VDF_CHECKPOINT_COUNT_IN_STEP,
                    Ascending,
                    ResetPoint,
                    crypto:hash(sha256, decode(<<"seed">>, Info, Opts)),
                    Threads,
                    difficulty(PrevInfo, Opts),
                    difficulty(Info, Opts)
                )
        end,
    case Result of
        {true, _Steps} ->
            ok;
        false ->
            {error, error_message(<<"invalid-vdf-chain">>,
                <<"The steps do not recompute from the parent's output.">>)}
    end.

%% @doc The five seed fields as the block declares them, shaped to compare
%% against `lib_arweave_vdf:seed_data/5'.
seed_data_claim(Info, Opts) ->
    #{
        <<"seed">> => required(<<"seed">>, Info, Opts),
        <<"next-seed">> => required(<<"next-seed">>, Info, Opts),
        <<"partition-upper-bound">> =>
            hb_util:int(required(<<"partition-upper-bound">>, Info, Opts)),
        <<"next-partition-upper-bound">> =>
            hb_util:int(required(<<"next-partition-upper-bound">>, Info, Opts)),
        <<"vdf-difficulty">> => difficulty(Info, Opts)
    }.

%% @doc Read a nonce limiter info, loading it if it arrived as a link. Every
%% field of an info is read by one check or another, so it is loaded whole.
info_message(Key, Base, Req, Opts) ->
    case required(Key, Base, Req, Opts) of
        Info when is_map(Info) -> Info;
        Link -> hb_cache:ensure_loaded(Link, Opts)
    end.

%% @doc Read an info's step list, newest-first and still encoded.
steps(Info, Opts) ->
    hb_util:message_to_ordered_list(
        hb_maps:get(<<"steps">>, Info, [], Opts),
        Opts
    ).

%% @doc Read an info's global step number.
step_number(Info, Opts) ->
    hb_util:int(required(<<"global-step-number">>, Info, Opts)).

%% @doc Read the VDF difficulty an info declares. Never defaulted: a block
%% verified at a difficulty it did not declare is a block verified against a
%% different network.
difficulty(Info, Opts) ->
    hb_util:int(required(<<"vdf-difficulty">>, Info, Opts)).

%% @doc Read a base64URL-encoded field of an info. Every info this device is
%% handed came out of a block header a peer served, so the checked decoder is
%% used and a field that is not base64URL is reported rather than silently
%% decoded to something else.
decode(Key, Info, Opts) ->
    decode(required(Key, Info, Opts)).

decode(Bin) ->
    case hb_util:safe_decode(Bin) of
        {ok, Decoded} -> Decoded;
        {error, _} -> throw({invalid_base64, Bin})
    end.

%% @doc The number of OS threads the NIF may verify with. There is no
%% parallelism inside a checkpoint -- that is the delay function's whole point
%% -- so the threads are spread across the steps of a chain, or across the 25
%% checkpoints of a single step.
threads(Base, Req, Opts) ->
    Default =
        hb_opts:get(
            arweave_vdf_threads,
            max(1, erlang:system_info(schedulers) div 2),
            Opts
        ),
    Threads = get_first(<<"arweave-vdf-threads">>, Base, Req, Default, Opts),
    max(1, hb_util:int(Threads)).

%% @doc Read a field from the request, falling back to the base message.
%%
%% These are data fields, not keys to resolve, so they are read with
%% `hb_maps:get/4' rather than `hb_ao:get/4'. Resolving a key against a message
%% that names this device dispatches back into the device, so a field that
%% shares a name with a key would be answered by the key.
get_first(Key, Base, Req, Default, Opts) ->
    case hb_maps:get(Key, Req, not_found, Opts) of
        not_found -> hb_maps:get(Key, Base, Default, Opts);
        Value -> Value
    end.

%% @doc Read a key that has no meaningful default.
required(Key, Base, Req, Opts) ->
    case get_first(Key, Base, Req, not_found, Opts) of
        not_found -> throw({missing_key, Key});
        Value -> Value
    end.

%% @doc Read a field of a nonce limiter info that has no meaningful default.
%% Two absent fields must never compare equal to one another, which is what a
%% shared default would make them.
required(Key, Info, Opts) ->
    case hb_maps:get(Key, Info, not_found, Opts) of
        not_found -> throw({missing_key, Key});
        Value -> Value
    end.

%% @doc The entropy reset line a step range crosses, or `none'.
entropy_reset_point(PrevStepNumber, StepNumber) ->
    ar_nonce_limiter:get_entropy_reset_point(PrevStepNumber, StepNumber).

%% @doc Build the standard error body.
error_message(Message, Detail) ->
    #{
        <<"status">> => 422,
        <<"message">> => Message,
        <<"detail">> => Detail
    }.

%%% Tests.
%%%
%%% Every vector here is a real mainnet post-2.9 block, and every mutant is one
%%% field of one of them. The chain tests are slow by construction -- verifying
%%% a step is a second of a reference CPU's work, and the point of the exercise
%%% is that no shortcut exists -- so they carry explicit timeouts.

%% @doc The options the tests resolve under.
test_opts() ->
    #{ <<"store">> => [hb_test_utils:test_store()] }.

%% @doc `verify-chain' accepts a real block pair. Block 1,974,876 advanced the
%% timeline by two steps, the shortest real chain in the fixtures.
verify_chain_test_() ->
    {timeout, 600, fun() ->
        ?assertEqual(
            true,
            resolve_field(chain_base(1974876), <<"verify-chain">>, <<"valid">>)
        )
    end}.

%% @doc `verify-chain' accepts a real block pair whose step range crosses an
%% entropy reset line. Block 1,974,850 crosses line 111,555,600 in the course
%% of its 270 steps: those below the line are verified at its parent's
%% difficulty and those above it at its own, with the mixed entropy carried
%% between the two calls in Erlang. The NIF's own reset branch stays dead.
verify_chain_across_reset_test_() ->
    {timeout, 3600, fun() ->
        ?assertEqual(
            true,
            resolve_field(
                with_all_threads(chain_base(1974850)),
                <<"verify-chain">>,
                <<"valid">>
            )
        )
    end}.

%% @doc `verify-step' recomputes a real block's final step, and reports it
%% without claiming validity: there is no `valid' key to branch on.
verify_step_test_() ->
    {timeout, 600, fun() ->
        {ok, Result} =
            hb_ao:resolve(chain_base(1974876), <<"verify-step">>, test_opts()),
        ?assertEqual(<<"passed">>, field(<<"pre-filter">>, Result)),
        ?assertEqual(false, field(<<"sufficient">>, Result)),
        ?assertEqual(not_found, field(<<"valid">>, Result))
    end}.

%% @doc The pre-filter does reject: a corrupted intra-step checkpoint fails it
%% just as it fails `verify-chain'. What the pre-filter cannot do is accept.
verify_step_rejects_test_() ->
    {timeout, 600, fun() ->
        Base = chain_base(1974876),
        [Newest, _Second | Rest] = checkpoints_of(Base),
        Corrupted =
            mutate(
                Base,
                <<"last-step-checkpoints">>,
                [Newest, random_hash() | Rest]
            ),
        {error, Error} =
            hb_ao:resolve(Corrupted, <<"verify-step">>, test_opts()),
        ?assertEqual(
            <<"invalid-last-step-checkpoints">>,
            field(<<"message">>, Error)
        )
    end}.

%% @doc The pre-filter is not a substitute for the chain, and here is the
%% block that proves it. Rewriting the parent's output -- and the block's claim
%% about it, so the two still agree -- leaves every check that reads only the
%% block itself satisfied, so `verify-step' passes. The chain is anchored on
%% the parent's output, so `verify-chain' rejects it.
verify_step_is_not_sufficient_test_() ->
    {timeout, 600, fun() ->
        Base = chain_base(1974876),
        Forged = hb_util:encode(crypto:strong_rand_bytes(32)),
        PrevInfo = maps:get(<<"prev-nonce-limiter-info">>, Base),
        Tampered =
            mutate(
                Base#{
                    <<"prev-nonce-limiter-info">> =>
                        PrevInfo#{ <<"output">> => Forged }
                },
                <<"prev-output">>,
                Forged
            ),
        ?assertEqual(
            <<"passed">>,
            resolve_field(Tampered, <<"verify-step">>, <<"pre-filter">>)
        ),
        assert_rejected(Tampered, <<"invalid-vdf-chain">>)
    end}.

%% @doc A step list that does not cover the whole range. Verifying the steps a
%% block does carry proves nothing about the ones it does not, so a short list
%% is an error rather than a smaller job.
reject_truncated_steps_test() ->
    Base = chain_base(1974876),
    [_Newest | Rest] = steps_of(Base),
    assert_rejected(mutate(Base, <<"steps">>, Rest), <<"invalid-step-count">>).

%% @doc An empty step list. The verification would have nothing to run over,
%% and a validator that let that through would accept every block.
reject_empty_steps_test() ->
    assert_rejected(
        mutate(chain_base(1974876), <<"steps">>, []),
        <<"invalid-step-count">>
    ).

%% @doc An ascending step list. The protocol's lists are newest-first; a
%% validator that took either order would verify some chains backwards.
reject_ascending_steps_test() ->
    Base = chain_base(1974876),
    assert_rejected(
        mutate(Base, <<"steps">>, lists:reverse(steps_of(Base))),
        <<"invalid-output">>
    ).

%% @doc A block that does not advance the timeline.
reject_stale_step_number_test() ->
    Base = chain_base(1974876),
    assert_rejected(
        mutate(Base, <<"global-step-number">>, prev_step_number(Base)),
        <<"stale-step-number">>
    ).

%% @doc A gap wider than the 10,800 steps a block may carry. It cannot be
%% verified from block data alone, whatever the block claims about it.
reject_wide_step_gap_test() ->
    Base = chain_base(1974876),
    assert_rejected(
        mutate(Base, <<"global-step-number">>, prev_step_number(Base) + 10801),
        <<"step-gap-too-large">>
    ).

%% @doc A block that does not continue from its parent's output.
reject_foreign_prev_output_test() ->
    Base = chain_base(1974876),
    assert_rejected(
        mutate(Base, <<"prev-output">>, random_hash()),
        <<"invalid-prev-output">>
    ).

%% @doc A block whose mined output is not the newest step it carries.
reject_foreign_output_test() ->
    Base = chain_base(1974876),
    assert_rejected(
        mutate(Base, <<"output">>, random_hash()),
        <<"invalid-output">>
    ).

%% @doc Each of the five seed fields is checked against what the parent
%% implies. The last mutant is `?VDF_DIFFICULTY' itself: mainnet left that
%% constant behind at its first retarget, so a device that reached for it
%% would verify every block against a difficulty no block declares.
reject_wrong_seed_data_test() ->
    Base = chain_base(1974876),
    lists:foreach(
        fun({Key, Value}) ->
            assert_rejected(
                mutate(Base, Key, Value),
                <<"invalid-vdf-seed-data">>
            )
        end,
        [
            {<<"seed">>, hb_util:encode(crypto:strong_rand_bytes(48))},
            {<<"next-seed">>, hb_util:encode(crypto:strong_rand_bytes(48))},
            {<<"partition-upper-bound">>, 1},
            {<<"next-partition-upper-bound">>, 1},
            {<<"vdf-difficulty">>, ?VDF_DIFFICULTY}
        ]
    ).

%% @doc A corrupted intra-step checkpoint. The 25 are one hash apart on the
%% chain, so a single wrong entry breaks the recomputation of two of them.
reject_corrupt_last_step_checkpoint_test_() ->
    {timeout, 600, fun() ->
        Base = chain_base(1974876),
        [Newest, _Second | Rest] = checkpoints_of(Base),
        assert_rejected(
            mutate(
                Base,
                <<"last-step-checkpoints">>,
                [Newest, random_hash() | Rest]
            ),
            <<"invalid-last-step-checkpoints">>
        )
    end}.

%% @doc A single corrupted step in the middle of the chain. Block 1,974,879
%% carries eighteen, so the mutation touches neither the newest step -- which
%% `output' pins -- nor the second, which the final step's checkpoints are
%% anchored on. Only the chain catches it.
reject_corrupt_step_test_() ->
    {timeout, 900, fun() ->
        Base = with_all_threads(chain_base(1974879)),
        {Newer, [_Corrupted | Older]} = lists:split(9, steps_of(Base)),
        assert_rejected(
            mutate(Base, <<"steps">>, Newer ++ [random_hash() | Older]),
            <<"invalid-vdf-chain">>
        )
    end}.

%% @doc Off a reset line the seed data is carried through from the parent
%% unchanged, and a real block declares exactly that.
seed_data_test() ->
    Base = chain_base(1974876),
    Info = maps:get(<<"nonce-limiter-info">>, Base),
    ?assertEqual(seed_data_of(Info), resolve_seed_data(Base, Info)).

%% @doc Across a reset line all five fields rotate together, and two of them
%% are drawn from the parent's header rather than its nonce limiter info:
%% `next-seed' becomes the parent's independent hash and
%% `next-partition-upper-bound' its weave size. Block 1,974,850 crosses line
%% 111,555,600 and declares exactly that.
seed_data_across_reset_test() ->
    Base = chain_base(1974850),
    Info = maps:get(<<"nonce-limiter-info">>, Base),
    Expected = seed_data_of(Info),
    ?assertEqual(
        maps:get(<<"prev-indep-hash">>, Base),
        maps:get(<<"next-seed">>, Expected)
    ),
    ?assertEqual(
        maps:get(<<"prev-weave-size">>, Base),
        maps:get(<<"next-partition-upper-bound">>, Expected)
    ),
    ?assertEqual(Expected, resolve_seed_data(Base, Info)).

%% @doc A step number behind its parent's has no seed data.
reject_stale_seed_data_step_number_test() ->
    Base = chain_base(1974876),
    {error, Error} =
        hb_ao:resolve(
            Base#{ <<"step-number">> => prev_step_number(Base) },
            <<"seed-data">>,
            test_opts()
        ),
    ?assertEqual(<<"stale-step-number">>, field(<<"message">>, Error)).

%% @doc The reset line a real step range crosses, and the absence of one.
%% Blocks 1,974,849 and 1,974,850 straddle line 111,555,600; 1,974,875 and
%% 1,974,876 sit between two lines.
reset_point_test() ->
    ?assertEqual(
        111555600,
        resolve_field(
            #{
                <<"device">> => <<"arweave-vdf@2.9">>,
                <<"prev-step-number">> => 111555523,
                <<"step-number">> => 111555793
            },
            <<"reset-point">>,
            <<"reset-point">>
        )
    ),
    ?assertEqual(
        <<"none">>,
        resolve_field(
            #{
                <<"device">> => <<"arweave-vdf@2.9">>,
                <<"prev-step-number">> => 111559070,
                <<"step-number">> => 111559072
            },
            <<"reset-point">>,
            <<"reset-point">>
        )
    ).

%% @doc Off a retarget height the scheduled difficulty is carried through.
%% Block 1,974,871 scheduled 1,111,546 and 1,974,872 is not a retarget, so
%% that is what 1,974,872 declares -- and does.
next_difficulty_test() ->
    ?assertEqual(
        1111546,
        resolve_field(
            #{
                <<"device">> => <<"arweave-vdf@2.9">>,
                <<"height">> => 1974871,
                <<"vdf-difficulty">> => 1111546,
                <<"next-vdf-difficulty">> => 1111546
            },
            <<"next-difficulty">>,
            <<"next-vdf-difficulty">>
        )
    ).

%% @doc At a retarget height the difficulty is recomputed from the parent's
%% block time history: the ratio of VDF time to block time over the 720
%% entries after the first 50, smoothed nine parts old to one part new.
%% Height 1,974,240 is a retarget and the network moved from 1,111,578 to
%% 1,111,546 there. The fixtures carry block headers, not histories, so the
%% history is synthesised to the ratio mainnet's implies; what is asserted is
%% that the arithmetic lands on the value the network chose. The entries
%% before and after the window are wildly off-ratio, so a device that failed
%% to cut them would miss by orders of magnitude.
next_difficulty_retarget_test() ->
    Window = [ history_element(100000, 99972) || _ <- lists:seq(1, 720) ],
    Excluded = [ history_element(100000, 1) || _ <- lists:seq(1, 50) ],
    ?assertEqual(
        1111546,
        resolve_field(
            #{
                <<"device">> => <<"arweave-vdf@2.9">>,
                <<"height">> => 1974239,
                <<"vdf-difficulty">> => 1111578,
                <<"next-vdf-difficulty">> => 1111578,
                <<"block-time-history">> => Excluded ++ Window ++ Excluded
            },
            <<"next-difficulty">>,
            <<"next-vdf-difficulty">>
        )
    ).

%% @doc A retarget height whose parent already scheduled a different
%% difficulty is not a retarget: the schedule is carried through untouched,
%% and the history is never read.
next_difficulty_scheduled_test() ->
    ?assertEqual(
        1111546,
        resolve_field(
            #{
                <<"device">> => <<"arweave-vdf@2.9">>,
                <<"height">> => 1974239,
                <<"vdf-difficulty">> => 1111578,
                <<"next-vdf-difficulty">> => 1111546
            },
            <<"next-difficulty">>,
            <<"next-vdf-difficulty">>
        )
    ).

%% @doc A nonce limiter info survives the round trip to a record and back,
%% including the order of both of its lists.
info_round_trip_test() ->
    Info = maps:get(<<"nonce-limiter-info">>, chain_base(1974871)),
    ?assertEqual(
        Info,
        lib_arweave_vdf:info_to_message(
            lib_arweave_vdf:message_to_info(Info, test_opts())
        )
    ).

%%% Test helpers.

%% @doc A `verify-chain' base for a fixture block and its parent.
chain_base(Height) ->
    Block = fixture(Height),
    PrevBlock = fixture(Height - 1),
    #{
        <<"device">> => <<"arweave-vdf@2.9">>,
        <<"nonce-limiter-info">> => fixture_info(Block),
        <<"prev-nonce-limiter-info">> => fixture_info(PrevBlock),
        <<"prev-indep-hash">> => maps:get(<<"indep_hash">>, PrevBlock),
        <<"prev-weave-size">> =>
            hb_util:int(maps:get(<<"weave_size">>, PrevBlock))
    }.

%% @doc Read a fixture block: real mainnet post-2.9 JSON, as the network's own
%% encoder emits it.
fixture(Height) ->
    {ok, Body} =
        file:read_file(
            <<"test/fixtures/arweave/block-",
                (integer_to_binary(Height))/binary, ".json">>
        ),
    hb_json:decode(Body).

%% @doc Project a fixture block's nonce limiter info into its message form.
%%
%% `~arweave-block@2.9' owns the block codec; until it lands this is the only
%% place the JSON spellings appear. Three of them are renames the JSON encoder
%% applies and the binary encoder does not, and the third is the trap: the
%% JSON `checkpoints' key holds the `steps' list, and there is no JSON `steps'
%% key at all. A codec that looked for one would hand `verify-chain' an empty
%% list.
fixture_info(Block) ->
    Info = maps:get(<<"nonce_limiter_info">>, Block),
    #{
        <<"output">> => maps:get(<<"output">>, Info),
        <<"prev-output">> => maps:get(<<"prev_output">>, Info),
        <<"seed">> => maps:get(<<"seed">>, Info),
        <<"next-seed">> => maps:get(<<"next_seed">>, Info),
        <<"partition-upper-bound">> =>
            hb_util:int(maps:get(<<"zone_upper_bound">>, Info)),
        <<"next-partition-upper-bound">> =>
            hb_util:int(maps:get(<<"next_zone_upper_bound">>, Info)),
        <<"global-step-number">> =>
            hb_util:int(maps:get(<<"global_step_number">>, Info)),
        <<"last-step-checkpoints">> =>
            maps:get(<<"last_step_checkpoints">>, Info),
        <<"steps">> => maps:get(<<"checkpoints">>, Info),
        <<"vdf-difficulty">> =>
            hb_util:int(maps:get(<<"vdf_difficulty">>, Info)),
        <<"next-vdf-difficulty">> =>
            hb_util:int(maps:get(<<"next_vdf_difficulty">>, Info))
    }.

%% @doc Replace one field of the block's nonce limiter info.
mutate(Base, Key, Value) ->
    Info = maps:get(<<"nonce-limiter-info">>, Base),
    Base#{ <<"nonce-limiter-info">> => Info#{ Key => Value } }.

%% @doc Resolve `verify-chain' and assert the error it rejects with. A mutant
%% that produced any other error, or none, would mean the check under test is
%% not the one doing the work.
assert_rejected(Base, Message) ->
    {error, Error} = hb_ao:resolve(Base, <<"verify-chain">>, test_opts()),
    ?assertEqual(Message, field(<<"message">>, Error)).

%% @doc Resolve a key and read one field of the result. The resolver attaches
%% its own `priv'/hashpath bookkeeping to every result, so assertions are made
%% per field rather than on the whole message.
resolve_field(Base, Key, Field) ->
    {ok, Result} = hb_ao:resolve(Base, Key, test_opts()),
    field(Field, Result).

%% @doc Read one field of a resolved result.
field(Key, Result) ->
    hb_maps:get(Key, Result, not_found, test_opts()).

%% @doc Resolve `seed-data' for a block's own step number, returning the five
%% fields it answers with.
resolve_seed_data(Base, Info) ->
    {ok, Result} =
        hb_ao:resolve(
            Base#{
                <<"step-number">> => maps:get(<<"global-step-number">>, Info)
            },
            <<"seed-data">>,
            test_opts()
        ),
    seed_data_of(Result).

%% @doc The block's step list, newest-first.
steps_of(Base) ->
    maps:get(<<"steps">>, maps:get(<<"nonce-limiter-info">>, Base)).

%% @doc The block's final-step checkpoints, newest-first.
checkpoints_of(Base) ->
    Info = maps:get(<<"nonce-limiter-info">>, Base),
    maps:get(<<"last-step-checkpoints">>, Info).

%% @doc The parent's global step number.
prev_step_number(Base) ->
    maps:get(
        <<"global-step-number">>,
        maps:get(<<"prev-nonce-limiter-info">>, Base)
    ).

%% @doc The five seed fields an info declares.
seed_data_of(Info) ->
    maps:with(
        [
            <<"seed">>,
            <<"next-seed">>,
            <<"partition-upper-bound">>,
            <<"next-partition-upper-bound">>,
            <<"vdf-difficulty">>
        ],
        Info
    ).

%% @doc Let the long chains use the whole machine, so the tests that must run
%% hundreds of steps are bounded by the hardware rather than by the default.
with_all_threads(Base) ->
    Base#{ <<"arweave-vdf-threads">> => erlang:system_info(schedulers) }.

%% @doc One block time history entry, in message form.
history_element(BlockInterval, VDFInterval) ->
    #{
        <<"block-interval">> => BlockInterval,
        <<"vdf-interval">> => VDFInterval,
        <<"chunk-count">> => 1
    }.

%% @doc An encoded hash belonging to nothing.
random_hash() ->
    hb_util:encode(crypto:strong_rand_bytes(32)).
