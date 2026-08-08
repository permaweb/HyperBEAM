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
-ifdef(TEST).
-export([check_step_range/3, check_chain/4, threads/3]).
-endif.
-include("include/hb.hrl").
-include("include/ar_vdf.hrl").
-include("include/ar_consensus.hrl").

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

%% @doc Check that the block advanced the timeline and carries the consensus
%% suffix of its step range. Arweave caps the header at 10,800 outputs even
%% when the distance from the parent is larger; `check_chain/4' computes the
%% missing prefix before it verifies that suffix.
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
        Delta when Count =/= min(?NONCE_LIMITER_MAX_CHECKPOINTS_COUNT, Delta) ->
            {error, error_message(<<"invalid-step-count">>,
                <<"The step list is not the consensus suffix of the range.">>)};
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
                ar_nonce_limiter:verify_no_reset(
                    SuppliedStart,
                    SuppliedPrevOutput,
                    ?VDF_CHECKPOINT_COUNT_IN_STEP,
                    Ascending,
                    Threads,
                    difficulty(Info, Opts)
                );
            ResetPoint when ResetPoint =< SuppliedStart ->
                ar_nonce_limiter:verify_no_reset(
                    SuppliedStart,
                    SuppliedPrevOutput,
                    ?VDF_CHECKPOINT_COUNT_IN_STEP,
                    Ascending,
                    Threads,
                    difficulty(Info, Opts)
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
        {error, _} -> throw({invalid_base64, Bin})
    end.

%% @doc The number of OS threads the NIF may verify with. There is no
%% parallelism inside a checkpoint -- that is the delay function's whole point
%% -- so the threads are spread across the steps of a chain, or across the 25
%% checkpoints of a single step. The request may choose fewer workers but can
%% never exceed the node operator's `arweave-max-vdf-workers' setting.
threads(Base, Req, Opts) ->
    Max =
        max(
            1,
            hb_util:int(
                hb_opts:get(
                    arweave_max_vdf_workers,
                    max(1, erlang:system_info(schedulers) div 2),
                    Opts
                )
            )
        ),
    Default = hb_opts:get(arweave_vdf_threads, Max, Opts),
    Threads = get_first(<<"arweave-vdf-threads">>, Base, Req, Default, Opts),
    min(Max, max(1, hb_util:int(Threads))).

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
