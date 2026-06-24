%%% @doc Reproducible core test for the `tx@1.0' ECDSA (secp256k1) commitment
%%% (Phase 3 #9). `dev_tx' is a preloaded device excluded from `src_dirs', so its
%%% in-module tests cannot be run via `rebar3 eunit --module=dev_tx'; this core
%%% test exercises the same path through the public `hb_message' API, so it runs
%%% under standard `rebar3 eunit'. Offline: a real secp256k1 wallet, no network.
-module(hb_tx_ecdsa_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(ECDSA_TYPE, <<"ecdsa-secp256k1-sha256">>).

ecdsa_commitment_verifies_test() ->
    Wallet = ar_wallet:new_ecdsa(),
    Opts = #{ <<"priv-wallet">> => Wallet },
    Structured = #{ <<"tag1">> => <<"value1">> },
    Committed =
        hb_message:commit(
            Structured,
            Opts,
            #{ <<"device">> => <<"tx@1.0">>, <<"type">> => ?ECDSA_TYPE }
        ),
    ?assert(hb_message:verify(Committed, all, Opts)),
    {ok, _, Commitment} =
        hb_message:commitment(
            #{ <<"commitment-device">> => <<"tx@1.0">> },
            Committed,
            Opts
        ),
    ?assertEqual(
        ?ECDSA_TYPE,
        hb_maps:get(<<"type">>, Commitment, not_found, Opts)
    ).
