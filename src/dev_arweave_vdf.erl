%%% @doc AO-Core device for Arweave VDF generation and verification.
-module(dev_arweave_vdf).

-export([info/1, info/3, default/4]).
-export([compute/3, verify/3, step_salt/3, checkpoints/3, session/3, previous_session/3, update/3]).

-include("include/hb.hrl").
-include("include/ar_vdf.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(VDF_PREFIX, <<"~arweave-vdf@2.9.5">>).

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
                    <<"checkpoints">>,
                    <<"session">>,
                    <<"previous-session">>,
                    <<"update">>
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
default(<<"session">>, Base, Req, Opts) ->
    session(Base, Req, Opts);
default(<<"previous-session">>, Base, Req, Opts) ->
    previous_session(Base, Req, Opts);
default(<<"previous_session">>, Base, Req, Opts) ->
    previous_session(Base, Req, Opts);
default(<<"update">>, Base, Req, Opts) ->
    update(Base, Req, Opts);
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
    Result =
        #{
            <<"step-number">> => StepNumber,
            <<"iteration-count">> => IterationCount,
            <<"output">> => hb_util:encode(Output),
            <<"checkpoints">> => lists:map(fun hb_util:encode/1, Checkpoints),
            <<"updated-at">> => os:system_time(second)
        },
    maybe_persist_session(Result, Base, Req, Opts),
    {ok, Result}.

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

session(_Base, _Req, Opts) ->
    read_state(session_path(Opts), Opts).

previous_session(_Base, _Req, Opts) ->
    read_state(previous_session_path(Opts), Opts).

update(Base, Req, Opts) ->
    case hb_maps:get(<<"method">>, Req, <<"GET">>, Opts) of
        <<"POST">> -> compute(Base, Req, Opts);
        _ -> session(Base, Req, Opts)
    end.

read_state(Path, Opts) ->
    case Path of
        undefined ->
            {error, not_found};
        _ ->
            case hb_cache:read(Path, Opts) of
                {ok, Session} -> {ok, hb_cache:ensure_all_loaded(Session, Opts)};
                _ -> {error, not_found}
            end
    end.

maybe_persist_session(Result, Base, Req, Opts) ->
    Persist = read_any([<<"persist-session">>], Base, Req, true, Opts),
    case Persist of
        false ->
            ok;
        <<"false">> ->
            ok;
        _ ->
            case session_path(Opts) of
                undefined ->
                    ok;
                SessionPath ->
                    case hb_cache:read(SessionPath, Opts) of
                        {ok, CurrentSession} ->
                            ok = write_state(previous_session_path(Opts), CurrentSession, Opts);
                        _ ->
                            ok
                    end,
                    ok = write_state(SessionPath, Result, Opts)
            end
    end.

write_state(Path, Value, Opts) ->
    case Path of
        undefined ->
            ok;
        _ ->
            {ok, ID} = hb_cache:write(Value, Opts),
            ok = hb_cache:link(ID, Path, Opts),
            ok
    end.

session_path(Opts) ->
    case hb_opts:get(store, no_viable_store, Opts) of
        no_viable_store ->
            undefined;
        not_found ->
            undefined;
        Store ->
            hb_store:path(Store, [?VDF_PREFIX, <<"state">>, <<"session">>])
    end.

previous_session_path(Opts) ->
    case hb_opts:get(store, no_viable_store, Opts) of
        no_viable_store ->
            undefined;
        not_found ->
            undefined;
        Store ->
            hb_store:path(Store, [?VDF_PREFIX, <<"state">>, <<"previous-session">>])
    end.

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

session_persistence_test() ->
    Opts = #{store => [hb_test_utils:test_store()]},
    PrevOutput = hb_util:decode(<<"f_z7RLug8etm3SrmRf-xPwXEL0ZQ_xHng2A5emRDQBw">>),
    {ok, _} =
        compute(
            #{},
            #{
                <<"step-number">> => 2,
                <<"prev-output">> => hb_util:encode(PrevOutput),
                <<"iteration-count">> => 2
            },
            Opts
        ),
    {ok, Session1} = session(#{}, #{}, Opts),
    ?assertEqual(2, hb_maps:get(<<"step-number">>, Session1, 0, #{})),
    {ok, _} =
        compute(
            #{},
            #{
                <<"step-number">> => 3,
                <<"prev-output">> => hb_maps:get(<<"output">>, Session1, <<>>, #{}),
                <<"iteration-count">> => 2
            },
            Opts
        ),
    {ok, Prev} = previous_session(#{}, #{}, Opts),
    ?assertEqual(2, hb_maps:get(<<"step-number">>, Prev, 0, #{})).
