%%% @doc Deterministic transaction admission vectors for Arweave 2.9.
-module(dev_arweave_tx_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TEST_HEIGHT, 1974871).
-define(TEST_REWARD, 1_000_000_000_000).

oversized_field_test() ->
    Opts = opts(),
    Wallet = ar_wallet:new(),
    Signed =
        fun(Anchor) ->
            sign(
                #tx{ format = 2, anchor = Anchor, reward = ?TEST_REWARD },
                Wallet,
                Opts
            )
        end,
    ?assertEqual(
        {ok, true},
        verify_result(Signed(crypto:strong_rand_bytes(32)), Opts)
    ),
    ?assertEqual(
        {error, <<"invalid-field-size">>},
        verify_result(Signed(crypto:strong_rand_bytes(64)), Opts)
    ).

invalid_target_length_test() ->
    Opts = opts(),
    Wallet = ar_wallet:new(),
    Anchor = crypto:strong_rand_bytes(32),
    Signed =
        fun(Target) ->
            sign(
                #tx{
                    format = 2,
                    anchor = Anchor,
                    target = Target,
                    reward = ?TEST_REWARD
                },
                Wallet,
                Opts
            )
        end,
    ?assertEqual(
        {ok, true},
        verify_result(Signed(crypto:strong_rand_bytes(32)), Opts)
    ),
    ?assertEqual(
        {error, <<"invalid-target-length">>},
        verify_result(Signed(crypto:strong_rand_bytes(33)), Opts)
    ).

self_targeted_test() ->
    Opts = opts(),
    Wallet = ar_wallet:new(),
    Anchor = crypto:strong_rand_bytes(32),
    Signed =
        fun(Target) ->
            sign(
                #tx{
                    format = 2,
                    anchor = Anchor,
                    target = Target,
                    reward = ?TEST_REWARD
                },
                Wallet,
                Opts
            )
        end,
    ?assertEqual(
        {ok, true},
        verify_result(Signed(crypto:strong_rand_bytes(32)), Opts)
    ),
    ?assertEqual(
        {error, <<"self-targeted-transaction">>},
        verify_result(Signed(ar_wallet:to_address(Wallet)), Opts)
    ).

negative_data_size_test() ->
    Opts = opts(),
    Wallet = ar_wallet:new(),
    Anchor = crypto:strong_rand_bytes(32),
    DataRoot = crypto:strong_rand_bytes(32),
    Signed =
        fun(DataSize) ->
            sign(
                #tx{
                    format = 2,
                    anchor = Anchor,
                    data_size = DataSize,
                    data_root = DataRoot,
                    reward = ?TEST_REWARD
                },
                Wallet,
                Opts
            )
        end,
    ?assertEqual({ok, true}, verify_result(Signed(262144), Opts)),
    ?assertEqual(
        {error, <<"negative-data-size">>},
        verify_result(Signed(-262144), Opts)
    ).

malleable_v1_test() ->
    Opts = opts(),
    Wallet = ar_wallet:new(),
    Anchor = crypto:strong_rand_bytes(32),
    Signed =
        fun(Target) ->
            sign(
                #tx{
                    format = 1,
                    anchor = Anchor,
                    target = Target,
                    quantity = 0,
                    reward = ?TEST_REWARD
                },
                Wallet,
                Opts
            )
        end,
    ?assertEqual({ok, true}, verify_result(Signed(<<>>), Opts)),
    ?assertEqual(
        {error, <<"malleable-transaction">>},
        verify_result(Signed(crypto:strong_rand_bytes(32)), Opts)
    ).

sign(TX, Wallet, Opts) ->
    lib_arweave_tx:from_tx(ar_tx:sign(TX, Wallet), Opts).

wallets(TX, Opts) ->
    Record = lib_arweave_tx:to_tx(TX, Opts),
    Addresses =
        case Record#tx.target of
            <<>> -> [Record#tx.owner_address];
            Target -> [Record#tx.owner_address, Target]
        end,
    maps:from_list(
        [
            {
                hb_util:encode(Address),
                #{
                    <<"balance">> => 1_000_000_000_000_000_000,
                    <<"last-tx">> => hb_maps:get(<<"last-tx">>, TX, <<>>, Opts),
                    <<"denomination">> => 1,
                    <<"mining-permission">> => true
                }
            }
         || Address <- Addresses
        ]
    ).

verify_result(TX, Opts) ->
    Req =
        #{
            <<"path">> => <<"verify">>,
            <<"height">> => ?TEST_HEIGHT,
            <<"wallets">> => wallets(TX, Opts),
            <<"price-per-gib-minute">> => 4897,
            <<"kryder-plus-rate-multiplier">> => 1,
            <<"block-denomination">> => 1
        },
    Base = TX#{ <<"device">> => <<"arweave-tx@2.9">> },
    case hb_ao:resolve(Base, Req, Opts) of
        {ok, Result} ->
            {ok, hb_maps:get(<<"valid">>, Result, not_found, Opts)};
        {error, Error} ->
            {error, hb_maps:get(<<"message">>, Error, not_found, Opts)}
    end.

opts() -> #{ <<"store">> => [hb_test_utils:test_store()] }.
