%%% @doc AO-Core device for Arweave SPoRA + nonce-limiter primitives.
-module(dev_arweave_spora).

-export([info/1, info/3, default/4]).
-export([
    compute/3,
    verify/3,
    seed_data/3,
    entropy_reset_point/3,
    maybe_add_entropy/3,
    poa/3,
    h1/3,
    h2/3,
    solution_hash/3,
    reset_frequency/3
]).

-include("include/hb.hrl").
-include("include/ar_vdf.hrl").
-include_lib("eunit/include/eunit.hrl").

info(_Opts) ->
    #{
        default => fun default/4
    }.

info(_Base, _Req, _Opts) ->
    {ok,
        #{
            <<"name">> => <<"arweave-spora@2.9">>,
            <<"description">> =>
                <<"SPoRA + nonce-limiter generation/verification device">>,
            <<"exports">> =>
                [
                    <<"compute">>,
                    <<"verify">>,
                    <<"seed-data">>,
                    <<"entropy-reset-point">>,
                    <<"maybe-add-entropy">>,
                    <<"poa">>,
                    <<"h1">>,
                    <<"h2">>,
                    <<"solution-hash">>,
                    <<"reset-frequency">>
                ]
        }
    }.

default(<<"set">>, Base, Req, Opts) ->
    dev_message:set(Base, Req, Opts);
default(<<"keys">>, Base, _Req, _Opts) ->
    dev_message:keys(Base);
default(<<"compute">>, Base, Req, Opts) ->
    compute(Base, Req, Opts);
default(<<"verify">>, Base, Req, Opts) ->
    verify(Base, Req, Opts);
default(<<"seed-data">>, Base, Req, Opts) ->
    seed_data(Base, Req, Opts);
default(<<"entropy-reset-point">>, Base, Req, Opts) ->
    entropy_reset_point(Base, Req, Opts);
default(<<"maybe-add-entropy">>, Base, Req, Opts) ->
    maybe_add_entropy(Base, Req, Opts);
default(<<"poa">>, Base, Req, Opts) ->
    poa(Base, Req, Opts);
default(<<"h1">>, Base, Req, Opts) ->
    h1(Base, Req, Opts);
default(<<"h2">>, Base, Req, Opts) ->
    h2(Base, Req, Opts);
default(<<"solution-hash">>, Base, Req, Opts) ->
    solution_hash(Base, Req, Opts);
default(<<"reset-frequency">>, Base, Req, Opts) ->
    reset_frequency(Base, Req, Opts);
default(_, Base, Req, Opts) ->
    compute(Base, Req, Opts).

compute(Base, Req, Opts) ->
    StepNumber = read_int([<<"step-number">>, <<"global-step-number">>], Base, Req, 1, Opts),
    PrevOutput = read_binary([<<"prev-output">>, <<"output">>], Base, Req, <<0:256>>, Opts),
    VDFDifficulty =
        read_int([<<"vdf-difficulty">>, <<"iteration-count">>], Base, Req, ?VDF_DIFFICULTY, Opts),
    {ok, Output, Checkpoints} = ar_nonce_limiter:compute(StepNumber, PrevOutput, VDFDifficulty),
    {
        ok,
        #{
            <<"step-number">> => StepNumber,
            <<"vdf-difficulty">> => VDFDifficulty,
            <<"output">> => hb_util:encode(Output),
            <<"checkpoints">> => lists:map(fun hb_util:encode/1, Checkpoints)
        }
    }.

verify(Base, Req, Opts) ->
    StartStepNumber =
        read_int(
            [<<"start-step-number">>, <<"step-number">>],
            Base,
            Req,
            1,
            Opts
        ),
    ProtocolStartStepNumber = max(0, StartStepNumber - 1),
    PrevOutput = read_binary([<<"prev-output">>, <<"output">>], Base, Req, <<0:256>>, Opts),
    NumCheckpointsBetweenHashes =
        read_int(
            [<<"checkpoints-between-hashes">>, <<"num-checkpoints-between-hashes">>],
            Base,
            Req,
            1,
            Opts
        ),
    Hashes = read_hashes(Base, Req, Opts),
    ThreadCount = read_int([<<"thread-count">>], Base, Req, 1, Opts),
    VDFDifficulty = read_int([<<"vdf-difficulty">>], Base, Req, ?VDF_DIFFICULTY, Opts),
    Result =
        case read_int_optional([<<"reset-step-number">>], Base, Req, Opts) of
            {ok, ResetStepNumber} ->
                ResetSeed = read_binary([<<"reset-seed">>], Base, Req, <<0:256>>, Opts),
                NextVDFDifficulty =
                    read_int(
                        [<<"next-vdf-difficulty">>],
                        Base,
                        Req,
                        VDFDifficulty,
                        Opts
                    ),
                ar_nonce_limiter:verify(
                    ProtocolStartStepNumber,
                    PrevOutput,
                    NumCheckpointsBetweenHashes,
                    Hashes,
                    ResetStepNumber,
                    ResetSeed,
                    ThreadCount,
                    VDFDifficulty,
                    NextVDFDifficulty
                );
            error ->
                ar_nonce_limiter:verify_no_reset(
                    ProtocolStartStepNumber,
                    PrevOutput,
                    NumCheckpointsBetweenHashes,
                    Hashes,
                    ThreadCount,
                    VDFDifficulty
                )
        end,
    case Result of
        false ->
            {ok, #{<<"valid">> => false}};
        {true, Steps} ->
            {
                ok,
                #{
                    <<"valid">> => true,
                    <<"steps">> => lists:map(fun hb_util:encode/1, Steps)
                }
            }
    end.

seed_data(Base, Req, Opts) ->
    StepNumber = read_int([<<"step-number">>], Base, Req, 1, Opts),
    PrevBlock = read_any([<<"previous-block">>, <<"prev-block">>], Base, Req, #{}, Opts),
    {
        Seed,
        NextSeed,
        PartitionUpperBound,
        NextPartitionUpperBound,
        VDFDifficulty
    } = ar_nonce_limiter:get_seed_data(StepNumber, PrevBlock),
    {
        ok,
        #{
            <<"seed">> => hb_util:safe_encode(Seed),
            <<"next-seed">> => hb_util:safe_encode(NextSeed),
            <<"partition-upper-bound">> => PartitionUpperBound,
            <<"next-partition-upper-bound">> => NextPartitionUpperBound,
            <<"vdf-difficulty">> => VDFDifficulty
        }
    }.

entropy_reset_point(Base, Req, Opts) ->
    PrevStep = read_int([<<"prev-step-number">>], Base, Req, 0, Opts),
    Step = read_int([<<"step-number">>], Base, Req, 0, Opts),
    case ar_nonce_limiter:get_entropy_reset_point(PrevStep, Step) of
        none -> {ok, <<"none">>};
        ResetPoint -> {ok, ResetPoint}
    end.

maybe_add_entropy(Base, Req, Opts) ->
    PrevOutput = read_binary([<<"prev-output">>], Base, Req, <<0:256>>, Opts),
    PrevStep = read_int([<<"prev-step-number">>], Base, Req, 0, Opts),
    Step = read_int([<<"step-number">>], Base, Req, 0, Opts),
    Seed = read_binary([<<"seed">>], Base, Req, <<0:384>>, Opts),
    {ok, hb_util:encode(ar_nonce_limiter:maybe_add_entropy(PrevOutput, PrevStep, Step, Seed))}.

reset_frequency(_Base, _Req, _Opts) ->
    {ok, ar_nonce_limiter:get_reset_frequency()}.

%% @doc Verify Arweave POA-style Merkle proofs for a chunk.
poa(Base, Req, Opts) ->
    Proof = read_any([<<"proof">>], Base, Req, Req, Opts),
    DataRoot = read_binary([<<"data-root">>, <<"data_root">>], Base, Proof, <<>>, Opts),
    DataPath = read_binary([<<"data-path">>, <<"data_path">>], Base, Proof, <<>>, Opts),
    DataSize = read_int([<<"data-size">>, <<"data_size">>], Base, Proof, 0, Opts),
    RecallByte = read_int([<<"recall-byte">>, <<"recall_byte">>], Base, Proof, 0, Opts),
    MaybeChunk = read_binary_optional([<<"chunk">>], Base, Proof, Opts),
    Ruleset = read_ruleset(Base, Proof, Opts),
    case ar_merkle:validate_path(DataRoot, RecallByte, DataSize, DataPath, Ruleset) of
        false ->
            {ok, #{<<"valid">> => false}};
        {Chunk, StartOffset, EndOffset} ->
            ChunkMatches =
                case MaybeChunk of
                    {ok, GivenChunk} ->
                        (GivenChunk =:= Chunk)
                            orelse (crypto:hash(sha256, GivenChunk) =:= Chunk);
                    error -> true
                end,
            {ok,
                #{
                    <<"valid">> => ChunkMatches,
                    <<"chunk">> => hb_util:encode(Chunk),
                    <<"chunk-start-offset">> => StartOffset,
                    <<"chunk-end-offset">> => EndOffset
                }
            }
    end.

h1(Base, Req, Opts) ->
    H0 = read_binary([<<"h0">>], Base, Req, <<0:256>>, Opts),
    Nonce = read_int([<<"nonce">>], Base, Req, 0, Opts),
    Chunk = read_binary([<<"chunk">>], Base, Req, <<>>, Opts),
    Preimage = crypto:hash(sha256, <<H0:32/binary, Nonce:64, Chunk/binary>>),
    {ok,
        #{
            <<"preimage">> => hb_util:encode(Preimage),
            <<"solution-hash">> => hb_util:encode(compute_solution_h(H0, Preimage))
        }
    }.

h2(Base, Req, Opts) ->
    H1 = read_binary([<<"h1">>], Base, Req, <<0:256>>, Opts),
    H0 = read_binary([<<"h0">>], Base, Req, <<0:256>>, Opts),
    Chunk = read_binary([<<"chunk">>], Base, Req, <<>>, Opts),
    Preimage = crypto:hash(sha256, <<H1:32/binary, Chunk/binary>>),
    {ok,
        #{
            <<"preimage">> => hb_util:encode(Preimage),
            <<"solution-hash">> => hb_util:encode(compute_solution_h(H0, Preimage))
        }
    }.

solution_hash(Base, Req, Opts) ->
    H0 = read_binary([<<"h0">>], Base, Req, <<0:256>>, Opts),
    Preimage = read_binary([<<"preimage">>], Base, Req, <<0:256>>, Opts),
    {ok, hb_util:encode(compute_solution_h(H0, Preimage))}.

compute_solution_h(H0, Preimage) ->
    crypto:hash(sha256, <<H0:32/binary, Preimage/binary>>).

read_hashes(Base, Req, Opts) ->
    Raw = read_any([<<"hashes">>, <<"checkpoints">>], Base, Req, [], Opts),
    lists:map(fun maybe_decode_binary/1, Raw).

read_int_optional(Keys, Base, Req, Opts) ->
    Raw = read_any(Keys, Base, Req, not_found, Opts),
    case Raw of
        not_found ->
            error;
        _ ->
            case hb_util:safe_int(Raw) of
                {ok, I} -> {ok, I};
                _ -> error
            end
    end.

read_binary_optional(Keys, Base, Req, Opts) ->
    Raw = read_any(Keys, Base, Req, not_found, Opts),
    case Raw of
        not_found -> error;
        _ -> {ok, maybe_decode_binary(Raw)}
    end.

read_ruleset(Base, Req, Opts) ->
    case read_any([<<"ruleset">>], Base, Req, <<"basic">>, Opts) of
        <<"strict-borders">> -> strict_borders_ruleset;
        <<"strict-data-split">> -> strict_data_split_ruleset;
        <<"offset-rebase">> -> offset_rebase_support_ruleset;
        _ -> basic_ruleset
    end.

read_int(Keys, Base, Req, Default, Opts) ->
    case hb_util:safe_int(read_any(Keys, Base, Req, Default, Opts)) of
        {ok, I} -> I;
        _ -> Default
    end.

read_binary(Keys, Base, Req, Default, Opts) ->
    maybe_decode_binary(read_any(Keys, Base, Req, Default, Opts)).

read_any(Keys, Base, Req, Default, Opts) ->
    Candidates = [{Req, Key} || Key <- Keys] ++ [{Base, Key} || Key <- Keys],
    hb_ao:get_first(Candidates, Default, Opts).

maybe_decode_binary(Bin) when is_binary(Bin) ->
    case hb_util:safe_decode(Bin) of
        {ok, Decoded} when byte_size(Decoded) > 0 -> Decoded;
        _ -> Bin
    end;
maybe_decode_binary(Other) ->
    Other.

%% ------------------------------------------------------------------
%% Tests (adapted from upstream nonce-limiter/VDF test vectors)
%% ------------------------------------------------------------------

compute_and_verify_test() ->
    PrevOutput = hb_util:decode(<<"f_z7RLug8etm3SrmRf-xPwXEL0ZQ_xHng2A5emRDQBw">>),
    {ok, ComputeRes} =
        compute(
            #{},
            #{
                <<"step-number">> => 2,
                <<"prev-output">> => hb_util:encode(PrevOutput),
                <<"vdf-difficulty">> => 2
            },
            #{}
        ),
    Hashes = lists:reverse(hb_maps:get(<<"checkpoints">>, ComputeRes, [], #{})),
    {ok, VerifyRes} =
        verify(
            #{},
            #{
                <<"start-step-number">> => 2,
                <<"prev-output">> => hb_util:encode(PrevOutput),
                <<"checkpoints">> => Hashes,
                <<"vdf-difficulty">> => 2
            },
            #{}
        ),
    ?assertEqual(true, hb_maps:get(<<"valid">>, VerifyRes, false, #{})).

entropy_reset_point_test() ->
    {ok, <<"none">>} =
        entropy_reset_point(
            #{},
            #{<<"prev-step-number">> => 1, <<"step-number">> => 100},
            #{}
        ),
    {ok, 1200} =
        entropy_reset_point(
            #{},
            #{<<"prev-step-number">> => 1199, <<"step-number">> => 1200},
            #{}
        ).

seed_data_reset_test() ->
    PrevBlock =
        #{
            <<"indep-hash">> => <<"new-seed">>,
            <<"weave-size">> => 5000,
            <<"nonce-limiter-info">> =>
                #{
                    <<"global-step-number">> => 1199,
                    <<"seed">> => <<"seed-a">>,
                    <<"next-seed">> => <<"seed-b">>,
                    <<"partition-upper-bound">> => 1000,
                    <<"next-partition-upper-bound">> => 2000,
                    <<"vdf-difficulty">> => 3,
                    <<"next-vdf-difficulty">> => 4
                }
        },
    {ok, SeedData} = seed_data(#{}, #{<<"step-number">> => 1200, <<"previous-block">> => PrevBlock}, #{}),
    ?assertEqual(
        <<"seed-b">>,
        hb_util:decode(hb_maps:get(<<"seed">>, SeedData, <<>>, #{}))
    ),
    ?assertEqual(4, hb_maps:get(<<"vdf-difficulty">>, SeedData, 0, #{})).

poa_validate_test() ->
    Chunk = crypto:strong_rand_bytes(128),
    SizedChunks = [{ar_tx:generate_chunk_id(Chunk), byte_size(Chunk)}],
    {Root, Tree} = ar_merkle:generate_tree(SizedChunks),
    Path = ar_merkle:generate_path(Root, 0, Tree),
    {ok, Result} =
        poa(
            #{},
            #{
                <<"data-root">> => hb_util:encode(Root),
                <<"data-path">> => hb_util:encode(Path),
                <<"data-size">> => byte_size(Chunk),
                <<"recall-byte">> => 0,
                <<"chunk">> => hb_util:encode(Chunk)
            },
            #{}
        ),
    ?assertEqual(true, hb_maps:get(<<"valid">>, Result, false, #{})).
