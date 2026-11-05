-module(hb_mint_authority_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

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

token() ->
    {ok, Token} = hb_device_load:reference(<<"token@1.0">>, #{}),
    Token.

id(Seed) ->
    hb_util:human_id(crypto:hash(sha256, Seed)).

base(Authority, Opts) ->
    base(Authority, #{}, Opts).
base(Authority, Extra, Opts) ->
    hb_message:commit(
        maps:merge(
            #{
                <<"device">> => <<"token@1.0">>,
                <<"name">> => <<"Test Token">>,
                <<"ticker">> => <<"TEST">>,
                <<"denomination">> => 0,
                <<"total-supply">> => 0,
                <<"max-supply">> => 100,
                <<"mint-device">> => <<"mint-authority@1.0">>,
                <<"mint-authority">> => Authority
            },
            Extra
        ),
        Opts
    ).

mint(State, From, Recipient, Quantity, Trigger, Opts) ->
    (token()):handle_action(
        <<"mint">>,
        State,
        #{
            <<"body">> =>
                #{
                    <<"action">> => <<"Mint">>,
                    <<"from">> => From,
                    <<"recipient">> => Recipient,
                    <<"quantity">> => Quantity,
                    <<"mint-trigger">> => Trigger
                }
        },
        Opts
    ).

transfer(State, From, Recipient, Quantity, Opts) ->
    transfer(State, From, Recipient, Quantity, #{}, Opts).
transfer(State, From, Recipient, Quantity, Extra, Opts) ->
    (token()):handle_action(
        <<"transfer">>,
        State,
        #{
            <<"body">> =>
                Extra#{
                    <<"action">> => <<"Transfer">>,
                    <<"from">> => From,
                    <<"recipient">> => Recipient,
                    <<"quantity">> => Quantity
                }
        },
        Opts
    ).

balance(State, Account, Opts) ->
    {ok, Balance} = (token()):balance(State, #{ <<"balance">> => Account }, Opts),
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

-define(PROCESS, <<"bEtA000000000000000000000000000000000000000">>).

l1_base(Authority, Opts) ->
    {ok, Base} =
        (token()):init(
            #{
                <<"device">> => <<"token@1.0">>,
                <<"execution-device">> => <<"token@1.0">>,
                <<"scheduler-device">> => <<"arweave-scheduler@1.0">>,
                <<"scheduler">> => hb_util:human_id(hb_opts:address(Opts)),
                <<"mint-device">> => <<"mint-authority@1.0">>,
                <<"mint-authority">> => Authority,
                <<"total-supply">> => <<"0">>,
                <<"max-supply">> => <<"100">>,
                <<"denomination">> => <<"12">>
            },
            #{},
            Opts
        ),
    Base.

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

l1_apply(State, Body, Height, Opts) ->
    {ok, Updated} =
        (token()):compute(
            State,
            hb_message:commit(
                #{
                    <<"path">> => <<"compute">>,
                    <<"process">> => ?PROCESS,
                    <<"slot">> => Height,
                    <<"block-height">> => Height,
                    <<"body">> => Body
                },
                Opts
            ),
            Opts
        ),
    Updated.

mint_once_per_trigger_test() ->
    Opts = opts(),
    Authority = id(<<"authority">>),
    Alice = id(<<"Alice">>),
    Burn1 = id(<<"burn-1">>),
    Burn2 = id(<<"burn-2">>),
    Base = base(Authority, Opts),
    {ok, Minted} = mint(Base, Authority, Alice, <<"5">>, Burn1, Opts),
    ?assertEqual(5, balance(Minted, Alice, Opts)),
    ?assertEqual(5, supply(Minted, Opts)),
    [Notice] = outbox(Minted, Opts),
    ?assertEqual(<<"Mint-Notice">>, hb_ao:get(<<"action">>, Notice, Opts)),
    ?assertEqual(Burn1, hb_ao:get(<<"mint-trigger">>, Notice, Opts)),
    ?assertEqual(5, hb_ao:get(<<"quantity">>, Notice, Opts)),
    ?assertEqual(
        {error, <<"Mint trigger already used.">>},
        mint(Minted, Authority, Alice, 5, Burn1, Opts)
    ),
    {ok, Again} = mint(Minted, Authority, Alice, 3, Burn2, Opts),
    ?assertEqual(8, balance(Again, Alice, Opts)),
    ?assertEqual(8, supply(Again, Opts)).

minted_balance_is_transferable_test() ->
    Opts = opts(),
    Authority = id(<<"authority">>),
    Alice = id(<<"Alice">>),
    Bob = id(<<"Bob">>),
    {ok, Minted} = mint(base(Authority, Opts), Authority, Alice, 5, id(<<"b">>), Opts),
    {ok, Moved} = transfer(Minted, Alice, Bob, 2, Opts),
    ?assertEqual(3, balance(Moved, Alice, Opts)),
    ?assertEqual(2, balance(Moved, Bob, Opts)),
    ?assertEqual(5, supply(Moved, Opts)).

authority_transfer_does_not_mint_test() ->
    Opts = opts(),
    Authority = id(<<"authority">>),
    Bob = id(<<"Bob">>),
    {ok, Minted} =
        mint(base(Authority, Opts), Authority, Authority, 5, id(<<"b">>), Opts),
    {ok, Moved} =
        transfer(
            Minted,
            Authority,
            Bob,
            2,
            #{ <<"mint-trigger">> => id(<<"t">>) },
            Opts
        ),
    ?assertEqual(3, balance(Moved, Authority, Opts)),
    ?assertEqual(2, balance(Moved, Bob, Opts)),
    ?assertEqual(5, supply(Moved, Opts)).

rejects_invalid_mints_test() ->
    Opts = opts(),
    Authority = id(<<"authority">>),
    Other = id(<<"other">>),
    Alice = id(<<"Alice">>),
    Burn = id(<<"burn">>),
    Base = base(Authority, Opts),
    Rejected =
        [
            mint(Base, Other, Alice, 5, Burn, Opts),
            mint(Base, [Authority, Other], Alice, 5, Burn, Opts),
            mint(Base, Authority, Alice, 0, Burn, Opts),
            mint(Base, Authority, Alice, -1, Burn, Opts),
            mint(Base, Authority, Alice, <<"1.5">>, Burn, Opts),
            mint(Base, Authority, Alice, #{}, Burn, Opts),
            mint(Base, Authority, Alice, 5, <<"not-an-id">>, Opts),
            mint(Base, Authority, Alice, 5, binary:copy(<<"_">>, 43), Opts),
            mint(Base, Authority, <<"not-an-address">>, 5, Burn, Opts),
            mint(Base, Authority, <<"uri-did-pkh-eip155-1-0x", 255>>, 5, Burn, Opts)
        ],
    lists:foreach(fun(Res) -> ?assertMatch({error, _}, Res) end, Rejected),
    ?assertMatch(
        {error, _},
        (token()):handle_action(
            <<"mint">>,
            Base,
            #{
                <<"body">> =>
                    #{
                        <<"from">> => Authority,
                        <<"recipient">> => Alice,
                        <<"quantity">> => 5,
                        <<"mint-trigger">> => Burn
                    }
            },
            Opts
        )
    ).

max_supply_caps_minting_test() ->
    Opts = opts(),
    Authority = id(<<"authority">>),
    Alice = id(<<"Alice">>),
    Base = base(Authority, Opts),
    {ok, Full} = mint(Base, Authority, Alice, 100, id(<<"b1">>), Opts),
    ?assertEqual(100, supply(Full, Opts)),
    ?assertEqual(
        {error, <<"Mint exceeds max supply.">>},
        mint(Full, Authority, Alice, 1, id(<<"b2">>), Opts)
    ),
    ?assertEqual(
        {error, <<"Mint exceeds max supply.">>},
        mint(Base, Authority, Alice, 101, id(<<"b3">>), Opts)
    ),
    lists:foreach(
        fun(Extra) ->
            ?assertEqual(
                {error, <<"Invalid max supply.">>},
                mint(
                    base(Authority, Extra, Opts),
                    Authority,
                    Alice,
                    1,
                    id(<<"b4">>),
                    Opts
                )
            )
        end,
        [
            #{ <<"max-supply">> => <<"x">> },
            #{ <<"max-supply">> => -1 },
            #{ <<"max-supply">> => #{} }
        ]
    ),
    NoCap = maps:remove(<<"max-supply">>, hb_message:uncommitted(Base, Opts)),
    ?assertEqual(
        {error, <<"Invalid max supply.">>},
        mint(hb_message:commit(NoCap, Opts), Authority, Alice, 1, id(<<"b5">>), Opts)
    ).

l1_mint_compute_test() ->
    Opts = opts(),
    AuthorityWallet = ar_wallet:new(),
    Authority = hb_util:human_id(ar_wallet:to_address(AuthorityWallet)),
    Alice = id(<<"Alice">>),
    Burn1 = id(<<"burn-1">>),
    Base = l1_base(Authority, Opts),
    Tags =
        fun(Quantity, Trigger) ->
            [
                {<<"Recipient">>, Alice},
                {<<"Quantity">>, Quantity},
                {<<"Mint-Trigger">>, Trigger}
            ]
        end,
    Minted = l1_apply(Base, l1_mint(AuthorityWallet, 0, Tags(<<"5">>, Burn1), Opts), 1, Opts),
    ?assertEqual(5, balance(Minted, Alice, Opts)),
    ?assertEqual(5, supply(Minted, Opts)),
    Forged =
        l1_mint(
            ar_wallet:new(),
            0,
            [{<<"From">>, Authority} | Tags(<<"5">>, id(<<"burn-2">>))],
            Opts
        ),
    AfterForged = l1_apply(Minted, Forged, 2, Opts),
    ?assert(lists:member(<<"Minter is not the mint authority.">>, reasons(AfterForged, Opts))),
    ?assertEqual(5, balance(AfterForged, Alice, Opts)),
    ?assertEqual(5, supply(AfterForged, Opts)),
    Replay =
        l1_mint(
            AuthorityWallet,
            0,
            [{<<"From">>, Authority} | Tags(<<"6">>, Burn1)],
            Opts
        ),
    AfterReplay = l1_apply(AfterForged, Replay, 3, Opts),
    ?assert(lists:member(<<"Mint trigger already used.">>, reasons(AfterReplay, Opts))),
    ?assertEqual(5, balance(AfterReplay, Alice, Opts)),
    ?assertEqual(5, supply(AfterReplay, Opts)),
    WithAR = l1_mint(AuthorityWallet, 1000, Tags(<<"7">>, id(<<"burn-3">>)), Opts),
    AfterAR = l1_apply(AfterReplay, WithAR, 4, Opts),
    ?assertEqual(12, balance(AfterAR, Alice, Opts)),
    ?assertEqual(12, supply(AfterAR, Opts)).
