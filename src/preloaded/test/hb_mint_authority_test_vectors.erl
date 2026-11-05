-module(hb_mint_authority_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(PROCESS, <<"bEtA000000000000000000000000000000000000000">>).

opts() ->
    hb:init(),
    #{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"store">> =>
            [
                hb_test_utils:test_store()
            |
                hb_opts:get(store, [], hb_opts:default_message())
            ]
    }.

id(Seed) ->
    hb_util:human_id(crypto:hash(sha256, Seed)).

wallet() ->
    Wallet = ar_wallet:new(),
    {Wallet, hb_util:human_id(ar_wallet:to_address(Wallet))}.

base(Authority, Opts) ->
    base(Authority, #{}, Opts).
base(Authority, Extra, Opts) ->
    {ok, Base} =
        hb_ao:resolve(
            maps:merge(
                #{
                    <<"device">> => <<"token@1.0">>,
                    <<"scheduler">> => hb_util:human_id(hb_opts:address(Opts)),
                    <<"total-supply">> => 0,
                    <<"max-supply">> => 100,
                    <<"mint-device">> => <<"mint-authority@1.0">>,
                    <<"mint-authority">> => Authority
                },
                Extra
            ),
            #{ <<"path">> => <<"init">> },
            Opts
        ),
    Base.

signed(Wallet, Fields, Opts) ->
    hb_message:commit(
        Fields#{ <<"target">> => ?PROCESS },
        Opts#{ <<"priv-wallet">> => Wallet },
        #{
            <<"commitment-device">> => <<"tx@1.0">>,
            <<"field-target">> => ?PROCESS,
            <<"field-quantity">> => <<"0">>
        }
    ).

compute(State, Body, Opts) ->
    {ok, Next} =
        hb_ao:resolve(
            State,
            hb_message:commit(
                #{
                    <<"path">> => <<"compute">>,
                    <<"process">> => ?PROCESS,
                    <<"slot">> => 0,
                    <<"body">> => Body
                },
                Opts
            ),
            Opts
        ),
    Next.

mint(State, {Wallet, From}, Recipient, Quantity, Trigger, Opts) ->
    compute(
        State,
        signed(
            Wallet,
            #{
                <<"action">> => <<"Mint">>,
                <<"from">> => From,
                <<"recipient">> => Recipient,
                <<"quantity">> => Quantity,
                <<"mint-trigger">> => Trigger
            },
            Opts
        ),
        Opts
    ).

transfer(State, Signer, Recipient, Quantity, Opts) ->
    transfer(State, Signer, Recipient, Quantity, #{}, Opts).
transfer(State, {Wallet, From}, Recipient, Quantity, Extra, Opts) ->
    compute(
        State,
        signed(
            Wallet,
            Extra#{
                <<"action">> => <<"Transfer">>,
                <<"from">> => From,
                <<"recipient">> => Recipient,
                <<"quantity">> => Quantity
            },
            Opts
        ),
        Opts
    ).

balance(State, Account, Opts) ->
    {ok, Balance} =
        hb_ao:resolve(
            State,
            #{ <<"path">> => <<"balance">>, <<"balance">> => Account },
            Opts
        ),
    Balance.

supply(State, Opts) ->
    hb_ao:get(<<"total-supply">>, State, Opts).

reasons(State, Opts) ->
    [hb_ao:get(<<"reason">>, Msg, undefined, Opts) || Msg <- outbox(State, Opts)].

outbox(State, Opts) ->
    hb_util:message_to_ordered_list(
        hb_ao:get(<<"results/outbox">>, State, [], Opts),
        Opts
    ).

l1_mint(Wallet, NativeQuantity, Tags, Opts) ->
    TX =
        ar_tx:sign(
            #tx{
                format = 2,
                target = hb_util:native_id(?PROCESS),
                quantity = NativeQuantity,
                reward = 1,
                tags = [{<<"Action">>, <<"Mint">>} | Tags]
            },
            Wallet
        ),
    hb_message:convert(TX, <<"structured@1.0">>, <<"tx@1.0">>, Opts).

mint_once_per_trigger_test() ->
    Opts = opts(),
    {_, Authority} = Signer = wallet(),
    Alice = id(<<"Alice">>),
    Burn1 = id(<<"burn-1">>),
    Burn2 = id(<<"burn-2">>),
    Minted = mint(base(Authority, Opts), Signer, Alice, <<"5">>, Burn1, Opts),
    ?assertEqual(5, balance(Minted, Alice, Opts)),
    ?assertEqual(5, supply(Minted, Opts)),
    [Notice] = outbox(Minted, Opts),
    ?assertEqual(<<"Mint-Notice">>, hb_ao:get(<<"action">>, Notice, Opts)),
    ?assertEqual(Burn1, hb_ao:get(<<"mint-trigger">>, Notice, Opts)),
    ?assertEqual(5, hb_ao:get(<<"quantity">>, Notice, Opts)),
    Replayed = mint(Minted, Signer, Alice, <<"4">>, Burn1, Opts),
    ?assert(lists:member(<<"Mint trigger already used.">>, reasons(Replayed, Opts))),
    ?assertEqual(5, balance(Replayed, Alice, Opts)),
    Again = mint(Replayed, Signer, Alice, <<"3">>, Burn2, Opts),
    ?assertEqual(8, balance(Again, Alice, Opts)),
    ?assertEqual(8, supply(Again, Opts)).

minted_balance_is_transferable_test() ->
    Opts = opts(),
    {_, Authority} = Signer = wallet(),
    {_, Alice} = AliceSigner = wallet(),
    Bob = id(<<"Bob">>),
    Minted = mint(base(Authority, Opts), Signer, Alice, <<"5">>, id(<<"b">>), Opts),
    Moved = transfer(Minted, AliceSigner, Bob, <<"2">>, Opts),
    ?assertEqual(3, balance(Moved, Alice, Opts)),
    ?assertEqual(2, balance(Moved, Bob, Opts)),
    ?assertEqual(5, supply(Moved, Opts)).

authority_transfer_does_not_mint_test() ->
    Opts = opts(),
    {_, Authority} = Signer = wallet(),
    Bob = id(<<"Bob">>),
    Minted = mint(base(Authority, Opts), Signer, Authority, <<"5">>, id(<<"b">>), Opts),
    Moved =
        transfer(
            Minted,
            Signer,
            Bob,
            <<"2">>,
            #{ <<"mint-trigger">> => id(<<"t">>) },
            Opts
        ),
    ?assertEqual(3, balance(Moved, Authority, Opts)),
    ?assertEqual(2, balance(Moved, Bob, Opts)),
    ?assertEqual(5, supply(Moved, Opts)).

rejects_invalid_mints_test() ->
    Opts = opts(),
    {AuthorityWallet, Authority} = Signer = wallet(),
    {OtherWallet, _} = Other = wallet(),
    Alice = id(<<"Alice">>),
    Burn = id(<<"burn">>),
    Base = base(Authority, Opts),
    Unchanged =
        fun(State) ->
            ?assertEqual(0, balance(State, Alice, Opts)),
            ?assertEqual(0, supply(State, Opts))
        end,
    lists:foreach(
        fun({S, Recipient, Quantity, Trigger}) ->
            Unchanged(mint(Base, S, Recipient, Quantity, Trigger, Opts))
        end,
        [
            {Other, Alice, <<"5">>, Burn},
            {Signer, Alice, <<"0">>, Burn},
            {Signer, Alice, <<"-1">>, Burn},
            {Signer, Alice, <<"1.5">>, Burn},
            {Signer, Alice, <<"x">>, Burn},
            {Signer, Alice, <<"5">>, <<"not-an-id">>},
            {Signer, Alice, <<"5">>, binary:copy(<<"_">>, 43)},
            {Signer, <<"not-an-address">>, <<"5">>, Burn},
            {Signer, <<"uri-did-pkh-eip155-1-0x", 255>>, <<"5">>, Burn}
        ]
    ),
    Fields =
        #{
            <<"recipient">> => Alice,
            <<"quantity">> => <<"5">>,
            <<"mint-trigger">> => Burn
        },
    TwoSigners =
        hb_message:commit(
            hb_message:commit(
                Fields#{ <<"action">> => <<"Mint">> },
                Opts#{ <<"priv-wallet">> => AuthorityWallet }
            ),
            Opts#{ <<"priv-wallet">> => OtherWallet }
        ),
    Unchanged(compute(Base, TwoSigners, Opts)),
    Unchanged(compute(Base, signed(AuthorityWallet, Fields, Opts), Opts)).

max_supply_caps_minting_test() ->
    Opts = opts(),
    {_, Authority} = Signer = wallet(),
    Alice = id(<<"Alice">>),
    Base = base(Authority, Opts),
    Full = mint(Base, Signer, Alice, <<"100">>, id(<<"b1">>), Opts),
    ?assertEqual(100, supply(Full, Opts)),
    Over = mint(Full, Signer, Alice, <<"1">>, id(<<"b2">>), Opts),
    ?assert(lists:member(<<"Mint exceeds max supply.">>, reasons(Over, Opts))),
    ?assertEqual(100, supply(Over, Opts)),
    TooLarge = mint(Base, Signer, Alice, <<"101">>, id(<<"b3">>), Opts),
    ?assert(lists:member(<<"Mint exceeds max supply.">>, reasons(TooLarge, Opts))),
    ?assertEqual(0, supply(TooLarge, Opts)),
    lists:foreach(
        fun(State) ->
            Res = mint(State, Signer, Alice, <<"1">>, id(<<"b4">>), Opts),
            ?assert(lists:member(<<"Invalid max supply.">>, reasons(Res, Opts))),
            ?assertEqual(0, balance(Res, Alice, Opts))
        end,
        [
            base(Authority, #{ <<"max-supply">> => <<"x">> }, Opts),
            base(Authority, #{ <<"max-supply">> => -1 }, Opts),
            base(Authority, #{ <<"max-supply">> => #{} }, Opts),
            maps:remove(<<"max-supply">>, Base)
        ]
    ).

l1_mint_compute_test() ->
    Opts = opts(),
    {AuthorityWallet, Authority} = Signer = wallet(),
    {AttackerWallet, _} = wallet(),
    Alice = id(<<"Alice">>),
    Minted = mint(base(Authority, Opts), Signer, Alice, <<"5">>, id(<<"burn-1">>), Opts),
    ?assertEqual(5, balance(Minted, Alice, Opts)),
    Forged = mint(Minted, {AttackerWallet, Authority}, Alice, <<"5">>, id(<<"burn-2">>), Opts),
    ?assert(lists:member(<<"Minter is not the mint authority.">>, reasons(Forged, Opts))),
    ?assertEqual(5, balance(Forged, Alice, Opts)),
    ?assertEqual(5, supply(Forged, Opts)),
    WithAR =
        l1_mint(
            AuthorityWallet,
            1000,
            [
                {<<"Recipient">>, Alice},
                {<<"Quantity">>, <<"7">>},
                {<<"Mint-Trigger">>, id(<<"burn-3">>)}
            ],
            Opts
        ),
    AfterAR = compute(Forged, WithAR, Opts),
    ?assertEqual(12, balance(AfterAR, Alice, Opts)),
    ?assertEqual(12, supply(AfterAR, Opts)).
