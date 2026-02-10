%%% @doc Tiny-network parity tests for AO Arweave ledger device.
%%% These are adapted from upstream ar_node_tests wallet/replay scenarios,
%%% executed only through AO-Core resolver calls.
-module(dev_arweave_tiny_network_tests).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

resolve_ledger(Path, Req, Opts) ->
    hb_ao:resolve(
        #{<<"device">> => dev_arweave_ledger},
        Req#{<<"path">> => Path},
        test_opts(Opts)
    ).

test_opts(Opts) ->
    case maps:is_key(store, Opts) of
        true -> Opts;
        false -> Opts#{store => [hb_test_utils:test_store()]}
    end.

addr_key(Address) when is_binary(Address), byte_size(Address) == 32 ->
    hb_util:encode(Address);
addr_key(Address) when is_binary(Address) ->
    Address;
addr_key(Address) ->
    hb_util:bin(Address).

make_signed_tx(SenderWallet, RecipientAddr, Quantity, Reward) ->
    TX0 =
        #tx{
            format = 2,
            target = RecipientAddr,
            quantity = Quantity,
            reward = Reward,
            data = <<>>,
            data_size = 0
        },
    ar_tx:sign(TX0, SenderWallet).

base_state(SenderWallet, Balance) ->
    SenderAddr = ar_wallet:to_address(SenderWallet),
    #{
        <<"height">> => 0,
        <<"balances">> => #{addr_key(SenderAddr) => Balance},
        <<"pending-reward">> => 0,
        <<"tx-history">> => []
    }.

wallet_transaction_tiny_network_test() ->
    SenderWallet = ar_wallet:new(),
    RecipientWallet = ar_wallet:new(),
    MinerWallet = ar_wallet:new(),
    RecipientAddr = ar_wallet:to_address(RecipientWallet),
    MinerAddr = addr_key(ar_wallet:to_address(MinerWallet)),
    InitialState = base_state(SenderWallet, 10000),
    TX = make_signed_tx(SenderWallet, RecipientAddr, 1000, 1),
    TXMsg = hb_message:convert(TX, <<"structured@1.0">>, <<"tx@1.0">>, #{}),

    {ok, Block1} =
        resolve_ledger(
            <<"generate-block">>,
            #{
                <<"state">> => InitialState,
                <<"txs">> => [TXMsg],
                <<"reward-addr">> => MinerAddr,
                <<"timestamp">> => 1000
            },
            #{}
        ),

    {ok, Validation} =
        resolve_ledger(
            <<"validate-block">>,
            #{<<"state">> => InitialState, <<"block">> => Block1},
            #{}
        ),
    ?assertEqual(true, hb_maps:get(<<"valid">>, Validation, false, #{})),

    {ok, State1} =
        resolve_ledger(
            <<"apply-block">>,
            #{<<"state">> => InitialState, <<"block">> => Block1},
            #{}
        ),

    SenderKey = addr_key(ar_wallet:to_address(SenderWallet)),
    RecipientKey = addr_key(RecipientAddr),
    ?assertEqual(8999, hb_maps:get(SenderKey, hb_maps:get(<<"balances">>, State1, #{}, #{}), 0, #{})),
    ?assertEqual(1000, hb_maps:get(RecipientKey, hb_maps:get(<<"balances">>, State1, #{}, #{}), 0, #{})),
    ?assertEqual(1, hb_maps:get(MinerAddr, hb_maps:get(<<"balances">>, State1, #{}, #{}), 0, #{})).

replay_attack_tiny_network_test() ->
    SenderWallet = ar_wallet:new(),
    RecipientWallet = ar_wallet:new(),
    MinerWallet = ar_wallet:new(),
    RecipientAddr = ar_wallet:to_address(RecipientWallet),
    MinerAddr = addr_key(ar_wallet:to_address(MinerWallet)),
    InitialState = base_state(SenderWallet, 10000),
    TX = make_signed_tx(SenderWallet, RecipientAddr, 1000, 1),
    TXMsg = hb_message:convert(TX, <<"structured@1.0">>, <<"tx@1.0">>, #{}),

    {ok, Block1} =
        resolve_ledger(
            <<"generate-block">>,
            #{
                <<"state">> => InitialState,
                <<"txs">> => [TXMsg],
                <<"reward-addr">> => MinerAddr,
                <<"timestamp">> => 1000
            },
            #{}
        ),

    {ok, State1} =
        resolve_ledger(
            <<"apply-block">>,
            #{<<"state">> => InitialState, <<"block">> => Block1},
            #{}
        ),

    {ok, ReplayValidation} =
        resolve_ledger(
            <<"validate-tx">>,
            #{<<"state">> => State1, <<"tx">> => TXMsg},
            #{}
        ),
    ?assertEqual(false, hb_maps:get(<<"valid">>, ReplayValidation, true, #{})),
    ?assertEqual(<<"tx_already_seen">>, hb_maps:get(<<"error">>, ReplayValidation, <<>>, #{})),

    {error, tx_already_seen} =
        resolve_ledger(
            <<"generate-block">>,
            #{
                <<"state">> => State1,
                <<"txs">> => [TXMsg],
                <<"reward-addr">> => MinerAddr,
                <<"timestamp">> => 1001
            },
            #{}
        ).

chain_progression_tiny_network_test() ->
    SenderWallet = ar_wallet:new(),
    RecipientWallet = ar_wallet:new(),
    MinerWallet = ar_wallet:new(),
    RecipientAddr = ar_wallet:to_address(RecipientWallet),
    MinerAddr = addr_key(ar_wallet:to_address(MinerWallet)),
    InitialState = base_state(SenderWallet, 5000),

    TX1 = make_signed_tx(SenderWallet, RecipientAddr, 400, 2),
    TX1Msg = hb_message:convert(TX1, <<"structured@1.0">>, <<"tx@1.0">>, #{}),
    {ok, Block1} =
        resolve_ledger(
            <<"generate-block">>,
            #{
                <<"state">> => InitialState,
                <<"txs">> => [TX1Msg],
                <<"reward-addr">> => MinerAddr,
                <<"timestamp">> => 1000
            },
            #{}
        ),
    {ok, State1} =
        resolve_ledger(
            <<"apply-block">>,
            #{<<"state">> => InitialState, <<"block">> => Block1},
            #{}
        ),

    TX2 = make_signed_tx(SenderWallet, RecipientAddr, 300, 2),
    TX2Msg = hb_message:convert(TX2, <<"structured@1.0">>, <<"tx@1.0">>, #{}),
    {ok, Block2} =
        resolve_ledger(
            <<"generate-block">>,
            #{
                <<"state">> => State1,
                <<"txs">> => [TX2Msg],
                <<"reward-addr">> => MinerAddr,
                <<"timestamp">> => 1001
            },
            #{}
        ),

    ?assertEqual(
        hb_maps:get(<<"last-block-hash">>, State1, <<>>, #{}),
        hb_maps:get(<<"previous-block">>, Block2, <<>>, #{})
    ),

    {ok, Validation2} =
        resolve_ledger(
            <<"validate-block">>,
            #{<<"state">> => State1, <<"block">> => Block2},
            #{}
        ),
    ?assertEqual(true, hb_maps:get(<<"valid">>, Validation2, false, #{})).
