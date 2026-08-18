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
-device_libraries([lib_arweave_vdf, lib_arweave_vdf_timeline]).
-export([info/1, verify_chain/3, verify_step/3, seed_data/3]).
-export([reset_point/3, next_difficulty/3]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("include/hb.hrl").
-include("include/ar_vdf.hrl").
-include("include/ar_consensus.hrl").

%%% How far past a block the timeline is told it may run. It matches the bound
%%% the timeline keeps for itself, so the answer to "where does this epoch end"
%%% is `none' exactly when no reset falls inside the range it would compute.
-define(TIMELINE_HORIZON, 4096).

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
    Threads = lib_arweave_vdf:threads(Base, Req, Opts),
    maybe
        ok ?= lib_arweave_vdf:check_step_range(Info, PrevInfo, Opts),
        ok ?= check_output(Info, Opts),
        ok ?= check_prev_output(Info, PrevInfo, Opts),
        ok ?= check_seed_data(Info, PrevInfo, PrevIndepHash, PrevWeaveSize, Opts),
        Held = held(Info, Opts),
        ok ?= check_last_step(Info, PrevInfo, Held, Threads, Opts),
        ok ?= check_chain(Info, PrevInfo, Held, Threads, Opts),
        advance_timeline(Info, Opts),
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
            Info, PrevInfo, lib_arweave_vdf:threads(Base, Req, Opts), Opts) of
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

%% @doc Check the final step's checkpoints against the node's own computation,
%% falling back to recomputation when the timeline does not hold an exact match.
%% `verify-chain' never accepts a prior `verify-step' result.
check_last_step(Info, PrevInfo, Held, Threads, Opts) ->
    case held_last_step(Info, Held, Opts) orelse
            lib_arweave_vdf:last_step_checkpoints(
                Info, PrevInfo, Threads, Opts) of
        true ->
            ok;
        false ->
            {error, error_message(<<"invalid-last-step-checkpoints">>,
                <<"The final step's checkpoints do not recompute.">>)}
    end.

%% @doc Recompute the whole step chain from the parent's output. A standard
%% block carries at most 10,800 outputs. For a wider gap, first compute the
%% omitted prefix sequentially, then verify the signed suffix in parallel.
%% This is deliberately uncapped: the operator chose validation over a fast
%% refusal, and the VDF itself determines how long recovery takes.
%%
%% When the range crosses a reset line the steps below it use the parent's
%% difficulty and those from the line onwards use the block's. Entropy is mixed
%% exactly once, whether that line falls in the computed prefix or the supplied
%% suffix.
check_chain(Info, PrevInfo, Threads, Opts) ->
    check_chain(Info, PrevInfo, held(Info, Opts), Threads, Opts).

check_chain(Info, PrevInfo, Held, Threads, Opts) ->
    StepNumber = step_number(Info, Opts),
    PrevStepNumber = step_number(PrevInfo, Opts),
    PrevOutput = decode(<<"output">>, PrevInfo, Opts),
    Ascending =
        lists:reverse([ decode(Step) || Step <- steps(Info, Opts) ]),
    SuppliedStart = StepNumber - length(Ascending),
    {SuppliedStart, SuppliedPrevOutput} =
        compute_prefix(
            PrevStepNumber,
            SuppliedStart,
            PrevOutput,
            Info,
            PrevInfo,
            Opts
        ),
    Result =
        case entropy_reset_point(PrevStepNumber, StepNumber) of
            none ->
                verify_steps(
                    SuppliedStart,
                    SuppliedPrevOutput,
                    Ascending,
                    Info,
                    Held,
                    Threads,
                    Opts
                );
            ResetPoint when ResetPoint =< SuppliedStart ->
                verify_steps(
                    SuppliedStart,
                    SuppliedPrevOutput,
                    Ascending,
                    Info,
                    Held,
                    Threads,
                    Opts
                );
            ResetPoint ->
                ar_nonce_limiter:verify(
                    SuppliedStart,
                    SuppliedPrevOutput,
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

%% @doc Verify the supplied steps, taking as done the leading run of them this
%% node's own timeline had already computed.
%%
%% A stored step is a recomputation performed earlier from an anchor already
%% validated, so a supplied output that equals one is verified by exactly the
%% work the NIF would have repeated. The run has to be leading and unbroken:
%% each step anchors the next, so the first step the timeline does not hold, or
%% holds differently, ends it and everything from there is verified in full.
%%
%% With the timeline off, absent, or behind, nothing is held and this is the
%% call that was always made.
verify_steps(Start, PrevOutput, Ascending, Info, Held, Threads, Opts) ->
    {Confirmed, Anchor, Remaining} =
        confirmed_with(Start, PrevOutput, Ascending, Held, Opts),
    case Remaining of
        [] ->
            {true, []};
        _ ->
            ar_nonce_limiter:verify_no_reset(
                Start + Confirmed,
                Anchor,
                ?VDF_CHECKPOINT_COUNT_IN_STEP,
                Remaining,
                Threads,
                difficulty(Info, Opts)
            )
    end.

%% @doc Split the supplied steps into the leading run the timeline confirms and
%% the rest, returning the run's length and the output it ends on.
confirmed(Start, PrevOutput, Ascending, Info, Opts) ->
    confirmed_with(Start, PrevOutput, Ascending, held(Info, Opts), Opts).

confirmed_with(Start, PrevOutput, Ascending, Held, Opts) ->
    Result = matching(Start + 1, PrevOutput, Ascending, Held, 0),
    case hb_opts:get(arweave_vdf_timeline, false, Opts) of
        true ->
            report_timeline(Held, Result, Ascending, Opts),
            Result;
        _ ->
            Result
    end.

%% @doc Take one snapshot of the timeline work relevant to the block.
held(Info, Opts) ->
    case hb_opts:get(arweave_vdf_timeline, false, Opts) of
        true ->
            StepNumber = step_number(Info, Opts),
            lib_arweave_vdf_timeline:snapshot(
                StepNumber - length(steps(Info, Opts)),
                StepNumber,
                decode(<<"seed">>, Info, Opts),
                difficulty(Info, Opts),
                Opts
            );
        _ ->
            #{}
    end.

%% @doc Whether the timeline holds the exact final-step checkpoint claim.
held_last_step(Info, Held, Opts) ->
    StepNumber = step_number(Info, Opts),
    Output = decode(<<"output">>, Info, Opts),
    Claimed =
        [
            decode(Checkpoint)
        ||
            Checkpoint <-
                hb_util:message_to_ordered_list(
                    hb_maps:get(<<"last-step-checkpoints">>, Info, [], Opts),
                    Opts
                )
        ],
    case maps:get(StepNumber, Held, not_found) of
        {Output, CheckpointBuffer} when is_binary(CheckpointBuffer) ->
            Claimed ==
                [Output |
                    ar_vdf:checkpoint_buffer_to_checkpoints(CheckpointBuffer)];
        _ ->
            false
    end.

%% @doc Say what the timeline contributed to this block, on the topic the node
%% prints without being asked.
%%
%% An operator who has chosen to spend a core on the timeline needs to see
%% whether it is earning it, and the failure mode is silent by construction: a
%% timeline that answers nothing is indistinguishable, from the outside, from
%% one that is switched off. Reported per block, beside `block_applied'.
report_timeline(Held, {Confirmed, _Anchor, Remaining}, Ascending, Opts) ->
    ?event(arweave_sync_short,
        {vdf_timeline,
            {held, map_size(Held)},
            {confirmed, Confirmed},
            {recomputed, length(Remaining)},
            {steps, length(Ascending)}
        },
        Opts
    ).

matching(Step, Anchor, [Output | Rest] = Remaining, Held, Count) ->
    case maps:get(Step, Held, not_found) of
        {Output, _Checkpoints} ->
            matching(Step + 1, Output, Rest, Held, Count + 1);
        _ ->
            {Count, Anchor, Remaining}
    end;
matching(_Step, Anchor, Remaining, _Held, Count) ->
    {Count, Anchor, Remaining}.

%% @doc Re-anchor the timeline on a block this node has just validated in full,
%% and tell it where the epoch it may compute through ends.
%%
%% The seed and difficulty are the block's own. A retarget or a reset changes
%% them, and steps computed under the old pair are simply never offered again,
%% because `known/5' matches on both.
advance_timeline(Info, Opts) ->
    case hb_opts:get(arweave_vdf_timeline, false, Opts) of
        true ->
            StepNumber = step_number(Info, Opts),
            lib_arweave_vdf_timeline:advance(
                decode(<<"seed">>, Info, Opts),
                difficulty(Info, Opts),
                StepNumber,
                decode(<<"output">>, Info, Opts),
                entropy_reset_point(StepNumber, StepNumber + ?TIMELINE_HORIZON),
                Opts
            );
        _ ->
            ok
    end.

%% @doc Compute the prefix omitted by the header's bounded step list.
compute_prefix(StepNumber, StepNumber, Output, _Info, _PrevInfo, _Opts) ->
    {StepNumber, Output};
compute_prefix(StepNumber, EndStepNumber, Output, Info, PrevInfo, Opts) ->
    NextStepNumber = StepNumber + 1,
    ResetPoint = entropy_reset_point(step_number(PrevInfo, Opts), EndStepNumber),
    SeededOutput =
        case NextStepNumber of
            ResetPoint ->
                ar_nonce_limiter:mix_seed(
                    Output,
                    decode(<<"seed">>, Info, Opts)
                );
            _ ->
                Output
        end,
    Difficulty =
        case ResetPoint =/= none andalso NextStepNumber >= ResetPoint of
            true -> difficulty(Info, Opts);
            false -> difficulty(PrevInfo, Opts)
        end,
    {ok, NextOutput, _Checkpoints} =
        ar_vdf:compute(NextStepNumber, SeededOutput, Difficulty),
    compute_prefix(
        NextStepNumber,
        EndStepNumber,
        NextOutput,
        Info,
        PrevInfo,
        Opts
    ).

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
        {error, _} -> throw({'invalid-base64', Bin})
    end.

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
        not_found -> throw({'missing-key', Key});
        Value -> Value
    end.

%% @doc Read a field of a nonce limiter info that has no meaningful default.
%% Two absent fields must never compare equal to one another, which is what a
%% shared default would make them.
required(Key, Info, Opts) ->
    case hb_maps:get(Key, Info, not_found, Opts) of
        not_found -> throw({'missing-key', Key});
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

-ifdef(TEST).

%% @doc A full AO-Core verification reuses both kinds of timeline work, while
%% an invalid checkpoint claim still falls back to native verification and is
%% refused.
timeline_reuses_final_step_checkpoints_test() ->
    {Base, Info, Opts} = timeline_vector(),
    {Valid, ValidCalls} = traced_verify(Base, Opts),
    ?assertMatch({ok, #{ <<"valid">> := true }}, Valid),
    ?assertEqual(0, ValidCalls),
    Last = hb_maps:get(<<"last-step-checkpoints">>, Info, [], Opts),
    [Output, _Checkpoint | Rest] = Last,
    InvalidInfo =
        Info#{
            <<"last-step-checkpoints">> =>
                [Output, hb_util:encode(<<0:256>>) | Rest]
        },
    {Invalid, InvalidCalls} =
        traced_verify(Base#{ <<"nonce-limiter-info">> => InvalidInfo }, Opts),
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-last-step-checkpoints">> }},
        Invalid
    ),
    ?assertEqual(1, InvalidCalls),
    {Unheld, UnheldCalls} =
        traced_verify(Base, Opts#{ <<"arweave-vdf-timeline">> => false }),
    ?assertMatch({ok, #{ <<"valid">> := true }}, Unheld),
    ?assertEqual(2, UnheldCalls).

%% @doc A complete two-step VDF claim and the timeline that computed it.
timeline_vector() ->
    Seed = <<1:384>>,
    NextSeed = <<2:384>>,
    Initial = <<0:256>>,
    {ok, Output2, _Checkpoints2} = ar_vdf:compute(2, Initial, 1),
    {ok, Output3, Checkpoints3} = ar_vdf:compute(3, Output2, 1),
    Common =
        #{
            <<"seed">> => hb_util:encode(Seed),
            <<"next-seed">> => hb_util:encode(NextSeed),
            <<"partition-upper-bound">> => 100,
            <<"next-partition-upper-bound">> => 200,
            <<"vdf-difficulty">> => 1,
            <<"next-vdf-difficulty">> => 1
        },
    PrevInfo =
        Common#{
            <<"output">> => hb_util:encode(Initial),
            <<"prev-output">> => hb_util:encode(<<3:256>>),
            <<"global-step-number">> => 1,
            <<"last-step-checkpoints">> => [],
            <<"steps">> => []
        },
    Info =
        Common#{
            <<"output">> => hb_util:encode(Output3),
            <<"prev-output">> => hb_util:encode(Initial),
            <<"global-step-number">> => 3,
            <<"last-step-checkpoints">> =>
                [
                    hb_util:encode(Checkpoint)
                ||
                    Checkpoint <-
                        [Output3 |
                            ar_vdf:checkpoint_buffer_to_checkpoints(
                                Checkpoints3
                            )]
                ],
            <<"steps">> =>
                [hb_util:encode(Output3), hb_util:encode(Output2)]
        },
    Opts =
        #{
            <<"arweave-vdf-timeline">> => true,
            <<"arweave-max-vdf-workers">> => 1,
            <<"http-server">> =>
                <<"vdf-checkpoints-", (hb_util:bin(
                    erlang:unique_integer([positive])))/binary>>,
            <<"store">> => [hb_test_utils:test_store()]
        },
    ok = lib_arweave_vdf_timeline:advance(Seed, 1, 1, Initial, 4, Opts),
    #{ 3 := {Output3, Checkpoints3} } = await_timeline(Seed, Opts, 200),
    {
        #{
            <<"device">> => <<"arweave-vdf@2.9">>,
            <<"nonce-limiter-info">> => Info,
            <<"prev-nonce-limiter-info">> => PrevInfo,
            <<"prev-indep-hash">> => hb_util:encode(<<4:384>>),
            <<"prev-weave-size">> => 1000
        },
        Info,
        Opts
    }.

%% @doc Wait for the timeline to compute the two steps in the vector.
await_timeline(_Seed, _Opts, 0) ->
    error('timeline-did-not-compute');
await_timeline(Seed, Opts, Remaining) ->
    case lib_arweave_vdf_timeline:snapshot(1, 3, Seed, 1, Opts) of
        Held when map_size(Held) == 2 -> Held;
        _ ->
            timer:sleep(10),
            await_timeline(Seed, Opts, Remaining - 1)
    end.

%% @doc Resolve `verify-chain' in a traced process and count native VDF entries.
traced_verify(Base, Opts) ->
    {module, ar_nonce_limiter} = code:ensure_loaded(ar_nonce_limiter),
    Parent = self(),
    {Worker, Monitor} =
        spawn_monitor(
            fun() ->
                receive
                    run ->
                        Parent !
                            {verified, self(),
                                hb_ao:resolve(Base, <<"verify-chain">>, Opts)}
                end
            end
        ),
    1 = erlang:trace_pattern(
        {ar_nonce_limiter, verify_no_reset, 6}, true, [local]),
    1 = erlang:trace(Worker, true, [call]),
    Worker ! run,
    try collect_verify(Worker, Monitor, 0)
    after
        erlang:trace_pattern(
            {ar_nonce_limiter, verify_no_reset, 6}, false, [local])
    end.

collect_verify(Worker, Monitor, Calls) ->
    receive
        {trace, Worker, call,
                {ar_nonce_limiter, verify_no_reset, _Arguments}} ->
            collect_verify(Worker, Monitor, Calls + 1);
        {verified, Worker, Result} ->
            erlang:demonitor(Monitor, [flush]),
            {Result, Calls};
        {'DOWN', Monitor, process, Worker, Reason} ->
            error({'verify-worker-down', Reason})
    after 5000 ->
        error('verify-worker-timeout')
    end.

-endif.
