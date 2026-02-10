%%% @doc Integration-style tests for Arweave device decomposition.
-module(dev_arweave_device_split_tests).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

resolve_arweave(Path, Req, Opts) ->
    hb_ao:resolve(
        #{<<"device">> => dev_arweave},
        Req#{<<"path">> => Path},
        test_opts(Opts)
    ).

test_opts(Opts) ->
    case maps:is_key(store, Opts) of
        true -> Opts;
        false -> Opts#{store => [hb_test_utils:test_store()]}
    end.

delegates_to_vdf_test() ->
    PrevOutput = hb_util:decode(<<"f_z7RLug8etm3SrmRf-xPwXEL0ZQ_xHng2A5emRDQBw">>),
    {ok, Res} =
        resolve_arweave(
            <<"vdf">>,
            #{
                <<"action">> => <<"compute">>,
                <<"step-number">> => 2,
                <<"prev-output">> => hb_util:encode(PrevOutput),
                <<"iteration-count">> => 2
            },
            #{}
        ),
    ?assert(hb_maps:get(<<"output">>, Res, not_found, #{}) =/= not_found).

delegates_to_spora_test() ->
    {ok, 1200} =
        resolve_arweave(
            <<"spora">>,
            #{
                <<"action">> => <<"entropy-reset-point">>,
                <<"prev-step-number">> => 1199,
                <<"step-number">> => 1200
            },
            #{}
        ).

delegates_to_ledger_test() ->
    {ok, Res} =
        resolve_arweave(
            <<"ledger">>,
            #{
                <<"action">> => <<"validate-tx">>,
                <<"tx">> => #{}
            },
            #{}
        ),
    ?assertEqual(false, hb_maps:get(<<"valid">>, Res, true, #{})).

delegates_to_gossip_test() ->
    Opts = #{store => [hb_test_utils:test_store()]},
    {ok, #{<<"accepted">> := true}} =
        resolve_arweave(
            <<"gossip">>,
            #{
                <<"action">> => <<"tx">>,
                <<"method">> => <<"POST">>,
                <<"tx">> => #{<<"hello">> => <<"world">>}
            },
            Opts
        ),
    {ok, Listed} =
        resolve_arweave(
            <<"gossip">>,
            #{
                <<"action">> => <<"tx">>,
                <<"method">> => <<"GET">>
            },
            Opts
        ),
    ?assertEqual(1, hb_maps:get(<<"count">>, Listed, 0, #{})).

delegates_tx_pending_to_gossip_test() ->
    Opts = #{store => [hb_test_utils:test_store()]},
    {ok, _} =
        resolve_arweave(
            <<"gossip">>,
            #{
                <<"action">> => <<"tx">>,
                <<"method">> => <<"POST">>,
                <<"tx">> => #{<<"id">> => <<"pending-1">>}
            },
            Opts
        ),
    {ok, #{<<"txids">> := IDs}} =
        resolve_arweave(
            <<"tx">>,
            #{
                <<"method">> => <<"GET">>,
                <<"action">> => <<"pending">>
            },
            Opts
        ),
    ?assertEqual([<<"pending-1">>], IDs).

delegates_peers_alias_to_gossip_test() ->
    Opts = #{store => [hb_test_utils:test_store()]},
    {ok, #{<<"peers">> := [<<"http://peer-a">>]}} =
        resolve_arweave(
            <<"peers">>,
            #{
                <<"method">> => <<"POST">>,
                <<"peer">> => <<"http://peer-a">>
            },
            Opts
        ).

delegates_vdf2_alias_test() ->
    PrevOutput = hb_util:decode(<<"f_z7RLug8etm3SrmRf-xPwXEL0ZQ_xHng2A5emRDQBw">>),
    {ok, Res} =
        resolve_arweave(
            <<"vdf2">>,
            #{
                <<"action">> => <<"compute">>,
                <<"step-number">> => 2,
                <<"prev-output">> => hb_util:encode(PrevOutput),
                <<"iteration-count">> => 2
            },
            #{}
        ),
    ?assert(hb_maps:get(<<"output">>, Res, not_found, #{}) =/= not_found).

delegates_vdf_session_alias_test() ->
    Opts = #{store => [hb_test_utils:test_store()]},
    PrevOutput = hb_util:decode(<<"f_z7RLug8etm3SrmRf-xPwXEL0ZQ_xHng2A5emRDQBw">>),
    {ok, _} =
        resolve_arweave(
            <<"vdf">>,
            #{
                <<"action">> => <<"compute">>,
                <<"step-number">> => 2,
                <<"prev-output">> => hb_util:encode(PrevOutput),
                <<"iteration-count">> => 2
            },
            Opts
        ),
    {ok, Session} =
        resolve_arweave(
            <<"vdf2">>,
            #{
                <<"action">> => <<"session">>
            },
            Opts
        ),
    ?assertEqual(2, hb_maps:get(<<"step-number">>, Session, 0, #{})).
