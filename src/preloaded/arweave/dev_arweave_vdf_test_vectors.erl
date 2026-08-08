%%% @doc Deterministic test vectors for post-2.9 Arweave VDF admission rules.
-module(dev_arweave_vdf_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Caller input may lower the worker count but may not raise the
%% operator's ceiling.
worker_limit_test() ->
    Opts = #{ <<"arweave-max-vdf-workers">> => 4 },
    ?assertEqual(
        4,
        dev_arweave_vdf:threads(
            #{ <<"arweave-vdf-threads">> => 1000 },
            #{},
            Opts
        )
    ),
    ?assertEqual(
        2,
        dev_arweave_vdf:threads(
            #{},
            #{ <<"arweave-vdf-threads">> => 2 },
            Opts
        )
    ),
    ?assertEqual(
        1,
        dev_arweave_vdf:threads(
            #{ <<"arweave-vdf-threads">> => 0 },
            #{},
            Opts
        )
    ).

%% @doc A wide VDF range carries the standard 10,800-step suffix without being
%% rejected merely because the parent is farther away.
wide_step_range_test() ->
    Steps = lists:duplicate(10800, <<"step">>),
    ?assertEqual(
        ok,
        dev_arweave_vdf:check_step_range(
            #{
                <<"global-step-number">> => 10802,
                <<"steps">> => Steps
            },
            #{ <<"global-step-number">> => 1 },
            #{}
        )
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-step-count">> }},
        dev_arweave_vdf:check_step_range(
            #{
                <<"global-step-number">> => 10802,
                <<"steps">> => tl(Steps)
            },
            #{ <<"global-step-number">> => 1 },
            #{}
        )
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-step-count">> }},
        dev_arweave_vdf:check_step_range(
            #{
                <<"global-step-number">> => 10802,
                <<"steps">> => [<<"step">> | Steps]
            },
            #{ <<"global-step-number">> => 1 },
            #{}
        )
    ).

%% @doc The output omitted before a bounded header suffix is computed from the
%% parent before the supplied outputs are verified.
wide_step_chain_test() ->
    Initial = <<0:256>>,
    Seed = <<1:256>>,
    {ok, Output2, _} = ar_vdf:compute(2, Initial, 1),
    {ok, Output3, _} = ar_vdf:compute(3, Output2, 1),
    {ok, Output4, _} = ar_vdf:compute(4, Output3, 1),
    {ok, Output5, _} = ar_vdf:compute(5, Output4, 1),
    PrevInfo = #{
        <<"global-step-number">> => 1,
        <<"output">> => hb_util:encode(Initial),
        <<"vdf-difficulty">> => 1
    },
    Info = #{
        <<"global-step-number">> => 5,
        <<"steps">> => [hb_util:encode(Output5), hb_util:encode(Output4)],
        <<"seed">> => hb_util:encode(Seed),
        <<"vdf-difficulty">> => 1
    },
    ?assertEqual(ok, dev_arweave_vdf:check_chain(Info, PrevInfo, 1, #{})),
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-vdf-chain">> }},
        dev_arweave_vdf:check_chain(
            Info#{
                <<"steps">> =>
                    [hb_util:encode(<<0:256>>), hb_util:encode(Output4)]
            },
            PrevInfo,
            1,
            #{}
        )
    ).

%% @doc The omitted prefix may contain the entropy reset. It is mixed once and
%% switches to the block's difficulty before the supplied suffix is checked.
wide_step_chain_across_reset_test() ->
    Initial = <<0:256>>,
    Seed = <<1:256>>,
    {ok, Output1199, _} = ar_vdf:compute(1199, Initial, 1),
    {ok, Output1200, _} =
        ar_vdf:compute(
            1200,
            ar_nonce_limiter:mix_seed(Output1199, Seed),
            2
        ),
    {ok, Output1201, _} = ar_vdf:compute(1201, Output1200, 2),
    {ok, Output1202, _} = ar_vdf:compute(1202, Output1201, 2),
    PrevInfo = #{
        <<"global-step-number">> => 1198,
        <<"output">> => hb_util:encode(Initial),
        <<"vdf-difficulty">> => 1
    },
    Info = #{
        <<"global-step-number">> => 1202,
        <<"steps">> =>
            [hb_util:encode(Output1202), hb_util:encode(Output1201)],
        <<"seed">> => hb_util:encode(Seed),
        <<"vdf-difficulty">> => 2
    },
    ?assertEqual(ok, dev_arweave_vdf:check_chain(Info, PrevInfo, 1, #{})).

%% @doc Reset detection covers both a crossing range and an ordinary range.
reset_point_test() ->
    ?assertEqual(
        111555600,
        field(
            resolve(
                #{
                    <<"prev-step-number">> => 111555523,
                    <<"step-number">> => 111555793
                },
                <<"reset-point">>
            ),
            <<"reset-point">>
        )
    ),
    ?assertEqual(
        <<"none">>,
        field(
            resolve(
                #{
                    <<"prev-step-number">> => 111559070,
                    <<"step-number">> => 111559072
                },
                <<"reset-point">>
            ),
            <<"reset-point">>
        )
    ).

%% @doc A scheduled difficulty is carried through outside a retarget.
next_difficulty_test() ->
    ?assertEqual(
        1111546,
        field(
            resolve(
                #{
                    <<"height">> => 1974871,
                    <<"vdf-difficulty">> => 1111546,
                    <<"next-vdf-difficulty">> => 1111546
                },
                <<"next-difficulty">>
            ),
            <<"next-vdf-difficulty">>
        )
    ).

resolve(Base, Path) ->
    hb_ao:resolve(
        Base#{ <<"device">> => <<"arweave-vdf@2.9">> },
        Path,
        #{ <<"store">> => [hb_test_utils:test_store()] }
    ).

field({ok, Result}, Key) -> hb_maps:get(Key, Result, not_found, #{}).
