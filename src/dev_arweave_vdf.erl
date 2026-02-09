%%% @doc AO-Core device for Arweave VDF generation and verification.
-module(dev_arweave_vdf).

-export([info/1, info/3, default/4]).
-export([compute/3, verify/3, step_salt/3, checkpoints/3]).

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
            <<"name">> => <<"arweave-vdf@2.9.5">>,
            <<"description">> => <<"Arweave VDF compute/verify device">>,
            <<"exports">> =>
                [
                    <<"compute">>,
                    <<"verify">>,
                    <<"step-salt">>,
                    <<"checkpoints">>
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
default(<<"step-salt">>, Base, Req, Opts) ->
    step_salt(Base, Req, Opts);
default(<<"checkpoints">>, Base, Req, Opts) ->
    checkpoints(Base, Req, Opts);
default(_, Base, Req, Opts) ->
    compute(Base, Req, Opts).

compute(Base, Req, Opts) ->
    StepNumber = read_int([<<"step-number">>, <<"start-step-number">>], Base, Req, 1, Opts),
    PrevOutput =
        read_binary(
            [<<"prev-output">>, <<"output">>],
            Base,
            Req,
            <<0:256>>,
            Opts
        ),
    IterationCount =
        read_int(
            [<<"iteration-count">>, <<"vdf-difficulty">>],
            Base,
            Req,
            ?VDF_DIFFICULTY,
            Opts
        ),
    {ok, Output, Checkpoints} = ar_vdf:compute2(StepNumber, PrevOutput, IterationCount),
    {
        ok,
        #{
            <<"step-number">> => StepNumber,
            <<"iteration-count">> => IterationCount,
            <<"output">> => hb_util:encode(Output),
            <<"checkpoints">> => lists:map(fun hb_util:encode/1, Checkpoints)
        }
    }.

verify(Base, Req, Opts) ->
    PrevOutput =
        read_binary(
            [<<"prev-output">>, <<"output">>],
            Base,
            Req,
            <<0:256>>,
            Opts
        ),
    NumCheckpointsBetweenHashes =
        read_int(
            [<<"checkpoints-between-hashes">>, <<"num-checkpoints-between-hashes">>],
            Base,
            Req,
            1,
            Opts
        ),
    IterationCount =
        read_int(
            [<<"iteration-count">>, <<"vdf-difficulty">>],
            Base,
            Req,
            ?VDF_DIFFICULTY,
            Opts
        ),
    ThreadCount = read_int([<<"thread-count">>], Base, Req, 1, Opts),
    ResetSeed = read_binary([<<"reset-seed">>], Base, Req, <<0:256>>, Opts),
    Hashes = read_hashes(Base, Req, Opts),
    Result =
        case read_int_optional([<<"start-salt">>], Base, Req, Opts) of
            {ok, StartSalt} ->
                ResetSalt = read_int([<<"reset-salt">>], Base, Req, -1, Opts),
                ar_vdf:verify(
                    StartSalt,
                    PrevOutput,
                    NumCheckpointsBetweenHashes,
                    Hashes,
                    ResetSalt,
                    ResetSeed,
                    ThreadCount,
                    IterationCount
                );
            error ->
                StartStepNumber =
                    read_int(
                        [<<"start-step-number">>, <<"step-number">>],
                        Base,
                        Req,
                        1,
                        Opts
                    ),
                ProtocolStartStepNumber = max(0, StartStepNumber - 1),
                ResetStepNumber =
                    read_int([<<"reset-step-number">>], Base, Req, 0, Opts),
                ar_vdf:verify2(
                    ProtocolStartStepNumber,
                    PrevOutput,
                    NumCheckpointsBetweenHashes,
                    Hashes,
                    ResetStepNumber,
                    ResetSeed,
                    ThreadCount,
                    IterationCount
                )
        end,
    case Result of
        false ->
            {ok, #{<<"valid">> => false}};
        {true, StepsBinary} when is_binary(StepsBinary) ->
            StepHashes = ar_vdf:checkpoint_buffer_to_checkpoints(StepsBinary),
            {
                ok,
                #{
                    <<"valid">> => true,
                    <<"steps">> => lists:map(fun hb_util:encode/1, StepHashes)
                }
            };
        {true, Steps} when is_list(Steps) ->
            {
                ok,
                #{
                    <<"valid">> => true,
                    <<"steps">> => lists:map(fun hb_util:encode/1, Steps)
                }
            }
    end.

step_salt(Base, Req, Opts) ->
    StepNumber = read_int([<<"step-number">>], Base, Req, 1, Opts),
    {ok, ar_vdf:step_number_to_salt_number(StepNumber)}.

checkpoints(Base, Req, Opts) ->
    Buffer = read_binary([<<"buffer">>, <<"checkpoint-buffer">>], Base, Req, <<>>, Opts),
    {
        ok,
        lists:map(
            fun hb_util:encode/1,
            ar_vdf:checkpoint_buffer_to_checkpoints(Buffer)
        )
    }.

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
%% Tests (adapted from upstream `ar_vdf_tests`)
%% ------------------------------------------------------------------

compute_and_verify_roundtrip_test() ->
    PrevOutput = hb_util:decode(<<"f_z7RLug8etm3SrmRf-xPwXEL0ZQ_xHng2A5emRDQBw">>),
    {ok, ComputeRes} =
        compute(
            #{},
            #{
                <<"step-number">> => 2,
                <<"prev-output">> => hb_util:encode(PrevOutput),
                <<"iteration-count">> => 2
            },
            #{}
        ),
    Hashes = lists:reverse(hb_maps:get(<<"checkpoints">>, ComputeRes, [], #{})),
    {ok, VerifyRes} =
        verify(
            #{},
            #{
                <<"step-number">> => 2,
                <<"prev-output">> => hb_util:encode(PrevOutput),
                <<"checkpoints">> => Hashes,
                <<"iteration-count">> => 2
            },
            #{}
        ),
    ?assertEqual(true, hb_maps:get(<<"valid">>, VerifyRes, false, #{})).

step_salt_test() ->
    {ok, Salt} = step_salt(#{}, #{<<"step-number">> => 2}, #{}),
    ?assertEqual(26, Salt).

checkpoint_decode_test() ->
    PrevOutput = crypto:strong_rand_bytes(32),
    {ok, _Output, Buffer} = ar_vdf:compute(2, PrevOutput, 2),
    {ok, Checkpoints} =
        checkpoints(
            #{},
            #{<<"buffer">> => hb_util:encode(Buffer)},
            #{}
        ),
    ?assert(length(Checkpoints) > 0).
