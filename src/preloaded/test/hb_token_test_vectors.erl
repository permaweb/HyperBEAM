%%% @doc Token-only tests for fixed-supply ownership ledgers.
-module(hb_token_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(PROCESS_OUTBOX_DEVICE, <<"process-outbox@1.0">>). 
-define(ASSET_PROCESS, <<"aSsEt00000000000000000000000000000000000000">>).

opts() ->
    hb:init(),
    #{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"store">> => [hb_test_utils:test_store() | default_stores()]
    }.

default_stores() ->
    hb_opts:get(store, [], hb_opts:default_message()).

%% @doc The module the preloaded `token@1.0' device runs as.
token() ->
    {ok, Token} = hb_device_load:reference(<<"token@1.0">>, #{}),
    Token.

%% @doc The module the preloaded `token@1.0' client library runs as.
token_lib() ->
    hb_util:atom(<<(hb_util:bin(token()))/binary, "__lib">>).

id(Bin) when is_binary(Bin) ->
    BitSize = byte_size(Bin) * 8,
    Suffix = <<0:(256 - BitSize)>>,
    hb_util:human_id(<<Bin/binary, Suffix/binary>>);
id(Other) ->
    hb_util:human_id(Other).

account_key(Account) ->
    hb_committer:uri(Account).

canonical_balances(Balances) ->
    maps:fold(
        fun(Account, Amount, Acc) ->
            Key = account_key(Account),
            Acc#{ Key => maps:get(Key, Acc, 0) + Amount }
        end,
        #{},
        Balances
    ).

token_state(Params, Opts) ->
    InitialBalances =
        canonical_balances(maps:get(initial_balances, Params, #{})),
    TotalSupply =
        maps:get(
            total_supply,
            Params,
            lists:sum(hb_maps:values(InitialBalances, Opts))
        ),
    {ok, Balances} =
        hb_ao:resolve(
            #{ <<"device">> => <<"trie@1.0">> },
            InitialBalances#{ <<"path">> => <<"set">> },
            Opts
        ),
    Extra = maps:get(extra, Params, #{}),
    Base =
        maps:merge(
            #{
                <<"device">> => <<"token@1.0">>,
                <<"name">> => <<"Test Token">>,
                <<"ticker">> => <<"TEST">>,
                <<"denomination">> => 0,
                <<"total-supply">> => TotalSupply,
                <<"balances">> => Balances
            },
            Extra
        ),
    hb_message:commit(Base, Opts).

balance(State, Account, Opts) ->
    Balances = hb_ao:get(<<"balances">>, State, Opts),
    case hb_ao:resolve(Balances, #{ <<"path">> => account_key(Account) }, Opts) of
        {ok, Amount} -> Amount;
        {error, not_found} -> 0
    end.

public_balance(State, Account, Opts) ->
    (token()):balance(State, #{ <<"balance">> => Account }, Opts).

outbox(State, Opts) ->
    hb_util:message_to_ordered_list(
        hb_ao:get(<<"results/outbox">>, State, [], Opts),
        Opts
    ).

process_outbox(Opts) ->
    {ok, Outbox} = hb_device_load:reference(?PROCESS_OUTBOX_DEVICE, Opts),
    Outbox.

outbox_subscribe(State, Req, Opts) ->
    (process_outbox(Opts)):subscribe(State, Req, Opts).

outbox_unsubscribe(State, Req, Opts) ->
    (process_outbox(Opts)):unsubscribe(State, Req, Opts).

outbox_subscribers(State, Action, Opts) ->
    outbox_subscribers(State, Action, <<"broadcast">>, Opts).
outbox_subscribers(State, Action, Target, Opts) ->
    {ok, Subscribers} =
        (process_outbox(Opts)):subscribers(
            State,
            #{ <<"action">> => Action, <<"target">> => Target },
            Opts
        ),
    Subscribers.

outbox_send(Message, State, Opts) ->
    {ok, Updated} =
        (process_outbox(Opts)):send(
            State,
            #{ <<"messages">> => Message },
            Opts
        ),
    Updated.

subscription_req(Action, default, Listener, Slot) ->
    #{
        <<"slot">> => Slot,
        <<"body">> =>
            #{
                <<"subscribe-action">> => Action,
                <<"from">> => Listener
            }
    };
subscription_req(Action, Target, Listener, Slot) ->
    #{
        <<"slot">> => Slot,
        <<"body">> =>
            #{
                <<"subscribe-action">> => Action,
                <<"subscribe-target">> => Target,
                <<"from">> => Listener
            }
    }.

has_message(Pairs, Msgs, Opts) ->
    lists:any(
        fun(Msg) ->
            lists:all(
                fun({Key, Value}) ->
                    hb_ao:get(Key, Msg, undefined, Opts) =:= Value
                end,
                Pairs
            )
        end,
        Msgs
    ).

transfer(State, From, To, Quantity, Opts) ->
    (token()):handle_action(
        <<"transfer">>,
        State,
        #{
            <<"body">> =>
                #{
                    <<"from">> => From,
                    <<"recipient">> => To,
                    <<"quantity">> => Quantity
                }
        },
        Opts
    ).

mint(State, From, Recipient, Quantity, Opts) ->
    (token()):handle_action(
        <<"mint">>,
        State,
        #{
            <<"body">> =>
                #{
                    <<"from">> => From,
                    <<"recipient">> => Recipient,
                    <<"quantity">> => Quantity
                }
        },
        Opts
    ).

set_field(State, From, Fields, Opts) ->
    (token()):handle_action(
        <<"set">>,
        State,
        #{ <<"body">> => Fields#{ <<"from">> => From } },
        Opts
    ).

signed_set_field(State, From, Fields, Opts) ->
    SignedBody =
        hb_message:commit(
            Fields#{ <<"action">> => <<"Set">>, <<"from">> => From },
            Opts
        ),
    (token()):handle_action(
        <<"set">>,
        State,
        #{ <<"body">> => SignedBody },
        Opts
    ).

balance_existing_account_test() ->
    Opts = opts(),
    Alice = id(<<"alice">>),
    Base =
        token_state(
            #{ initial_balances => #{ Alice => 7 } },
            Opts
        ),
    ?assertEqual({ok, 7}, public_balance(Base, Alice, Opts)).

mixed_case_initial_balance_uses_canonical_account_test() ->
    Opts = opts(),
    Alice = id(<<"Alice">>),
    Base =
        token_state(
            #{ initial_balances => #{ Alice => 7 } },
            Opts
        ),
    Balances = hb_ao:get(<<"balances">>, Base, Opts),
    ?assertNot(lists:member(Alice, hb_ao:keys(Balances, Opts))),
    ?assertEqual({error, not_found}, hb_ao:resolve(Balances, Alice, Opts)),
    ?assertEqual({ok, 7}, hb_ao:resolve(Balances, #{ <<"path">> => account_key(Alice) }, Opts)),
    ?assertEqual({ok, 7}, public_balance(Base, Alice, Opts)),
    ?assertEqual({ok, 7}, public_balance(Base, account_key(Alice), Opts)).

init_seeds_distinct_case_sensitive_accounts_test() ->
    Opts = opts(),
    Alice = <<"kRdpOYaT5pUUiNFDaUymqO1VcybZpAfNPnNdls-A134">>,
    Other = hb_util:to_lower(Alice),
    Base =
        #{
            <<"device">> => <<"token@1.0">>,
            <<"initial-holder">> => Alice,
            <<"total-supply">> => 7
        },
    {ok, Initialized} = hb_ao:resolve(Base, <<"init">>, Opts),
    Balances = hb_ao:get(<<"balances">>, Initialized, Opts),
    ?assertEqual([account_key(Alice)], hb_ao:keys(Balances, Opts)),
    ?assertEqual({ok, 7}, public_balance(Initialized, Alice, Opts)),
    ?assertEqual({ok, 0}, public_balance(Initialized, Other, Opts)),
    ?assertEqual(Alice, hb_ao:get(<<"initial-holder">>, Initialized, Opts)).

committer_balance_client_reads_literal_account_test() ->
    Opts = opts(),
    Alice = <<"kRdpOYaT5pUUiNFDaUymqO1VcybZpAfNPnNdls-A134">>,
    URI = account_key(Alice),
    State = #{ <<"now">> => #{ <<"balances">> => #{ URI => 7 } } },
    ?assertEqual(7, (token_lib()):balance(State, Alice, Opts)),
    ?assertEqual(7, (token_lib()):balance(State, URI, Opts)),
    ?assertEqual(0, (token_lib()):balance(State, hb_util:to_lower(Alice), Opts)).

committer_client_ledger_survives_json_test() ->
    Opts = opts(),
    Wallet = ar_wallet:new(),
    Owner = hb:address(Wallet),
    URI = hb_committer:uri(Wallet, Opts),
    Other = <<"krdpoyat5puuinfdauymqo1vcybzpafnpnndls-a134">>,
    OtherURI = hb_committer:uri(Other),
    Process = (token_lib()):ledger(#{
        <<"execution-device">> => <<"token@1.0">>,
        <<"balances">> => #{ Wallet => 3, URI => 4, Other => 2 },
        <<"total-supply">> => 9
    }, Opts),
    ?assert(hb_message:verify(Process, all, Opts)),
    Roundtrip = hb_message:convert(
        hb_message:convert(Process, <<"json@1.0">>, Opts),
        <<"structured@1.0">>, <<"json@1.0">>, Opts
    ),
    ?assert(hb_message:verify(Roundtrip, all, Opts)),
    Balances = (token_lib()):balances(initial, Roundtrip, Opts),
    ?assertEqual(7, hb_ao:get(URI, Balances, Opts)),
    ?assertEqual(2, hb_ao:get(OtherURI, Balances, Opts)),
    ?assertNot(lists:member(Owner, hb_ao:keys(Balances, Opts))),
    State = #{ <<"now">> => #{ <<"balances">> => Balances } },
    ?assertEqual(7, (token_lib()):balance(State, Wallet, Opts)),
    ?assertEqual(7, (token_lib()):balance(State, URI, Opts)),
    ?assertEqual(7, (token_lib()):balance(State, Owner, Opts)),
    ?assertEqual(2, (token_lib()):balance(State, Other, Opts)).

%% @doc A transfer's notices give the sender and the recipient as the URIs of
%% their accounts.
committer_client_transfer_gives_account_notices_test() ->
    Opts = opts(),
    Sender = ar_wallet:new(),
    Recipient = ar_wallet:new(),
    Process = (token_lib()):ledger(#{
        <<"execution-device">> => <<"token@1.0">>,
        <<"balances">> => #{ Sender => 7 }, <<"total-supply">> => 7
    }, Opts),
    {ok, Pushed} = (token_lib()):transfer(Process, Sender, Recipient, 3, Opts),
    Slot = hb_ao:get(<<"slot">>, Pushed, Opts),
    {ok, Computed} = hb_ao:resolve(Process, #{
        <<"path">> => <<"compute">>, <<"slot">> => Slot
    }, Opts),
    ?assertEqual(4, (token_lib()):balance(Computed, Sender, Opts)),
    ?assertEqual(3, (token_lib()):balance(Computed, Recipient, Opts)),
    ?assertEqual(4, (token_lib()):normalized_balance(Computed, Sender, Opts)),
    ?assertEqual(3, (token_lib()):normalized_balance(
        Computed, hb_committer:uri(Recipient, Opts), Opts
    )),
    Notices = outbox(Computed, Opts),
    SenderURI = hb_committer:uri(Sender, Opts),
    RecipientURI = hb_committer:uri(Recipient, Opts),
    ?assert(has_message([
        {<<"action">>, <<"Debit-Notice">>},
        {<<"target">>, SenderURI},
        {<<"recipient">>, RecipientURI}
    ], Notices, Opts)),
    ?assert(has_message([
        {<<"action">>, <<"Credit-Notice">>},
        {<<"target">>, RecipientURI},
        {<<"sender">>, SenderURI}
    ], Notices, Opts)).

balance_missing_account_returns_zero_test() ->
    Opts = opts(),
    Alice = id(<<"alice">>),
    Bob = id(<<"bob">>),
    Base =
        token_state(
            #{ initial_balances => #{ Alice => 7 } },
            Opts
        ),
    ?assertEqual({ok, 0}, public_balance(Base, Bob, Opts)).

balance_reserved_account_rejected_test() ->
    Opts = opts(),
    Base = token_state(#{}, Opts),
    ?assertEqual(
        {error, <<"Address is a reserved ao/custom key">>},
        public_balance(Base, <<"path">>, Opts)
    ).

uppercase_reserved_account_rejected_test() ->
    Opts = opts(),
    Base = token_state(#{}, Opts),
    ?assertEqual(
        {error, <<"Address is a reserved ao/custom key">>},
        public_balance(Base, <<"PATH">>, Opts)
    ),
    ?assertEqual(
        {error, <<"Address uses a reserved trie internal key.">>},
        public_balance(Base, <<"DEVICE">>, Opts)
    ).

basic_transfer_updates_balances_test() ->
    Opts = opts(),
    Alice = id(<<"alice">>),
    Bob = id(<<"bob">>),
    Base =
        token_state(
            #{
                initial_balances => #{
                    Alice => 100,
                    Bob => 10
                }
            },
            Opts
        ),
    {ok, Updated} = transfer(Base, Alice, Bob, 30, Opts),
    ?assertEqual(70, balance(Updated, Alice, Opts)),
    ?assertEqual(40, balance(Updated, Bob, Opts)),
    ?assertEqual(110, hb_ao:get(<<"total-supply">>, Updated, Opts)),
    Notices = outbox(Updated, Opts),
    ?assertEqual(2, length(Notices)),
    ?assertEqual(
        [<<"Credit-Notice">>, <<"Debit-Notice">>],
        lists:sort([hb_ao:get(<<"action">>, Notice, Opts) || Notice <- Notices])
    ).

mixed_case_transfer_updates_canonical_balances_test() ->
    Opts = opts(),
    Alice = id(<<"Alice">>),
    Bob = id(<<"Bob">>),
    Base =
        token_state(
            #{
                initial_balances => #{
                    Alice => 10,
                    Bob => 1
                }
            },
            Opts
        ),
    {ok, Updated} = transfer(Base, Alice, Bob, 3, Opts),
    ?assertEqual(7, balance(Updated, Alice, Opts)),
    ?assertEqual(4, balance(Updated, Bob, Opts)),
    ?assertEqual(7, balance(Updated, account_key(Alice), Opts)),
    ?assertEqual(4, balance(Updated, account_key(Bob), Opts)),
    Notices = outbox(Updated, Opts),
    [Debit] = [
        Notice
    ||
        Notice <- Notices,
        hb_ao:get(<<"action">>, Notice, Opts) =:= <<"Debit-Notice">>
    ],
    [Credit] = [
        Notice
    ||
        Notice <- Notices,
        hb_ao:get(<<"action">>, Notice, Opts) =:= <<"Credit-Notice">>
    ],
    ?assertEqual(account_key(Alice), hb_ao:get(<<"target">>, Debit, Opts)),
    ?assertEqual(account_key(Bob), hb_ao:get(<<"target">>, Credit, Opts)),
    ?assertEqual(account_key(Alice), hb_ao:get(<<"sender">>, Credit, Opts)).

outbox_default_broadcast_subscription_test() ->
    Opts = opts(),
    {ok, Subscribed} =
        outbox_subscribe(
            #{},
            subscription_req(<<"Ping">>, default, <<"listener-a">>, 11),
            Opts
        ),
    ?assertEqual(
        [<<"listener-a">>],
        outbox_subscribers(Subscribed, <<"Ping">>, Opts)
    ),
    Updated =
        outbox_send(
            #{ <<"action">> => <<"Ping">> },
            Subscribed,
            Opts
        ),
    Notices = outbox(Updated, Opts),
    ?assertEqual(2, length(Notices)),
    ?assert(has_message(
        [
            {<<"action">>, <<"notify">>},
            {<<"target">>, <<"listener-a">>},
            {<<"x-action">>, <<"Ping">>}
        ],
        Notices,
        Opts
    )).

outbox_targeted_subscriptions_do_not_overwrite_test() ->
    Opts = opts(),
    {ok, AliceSubscribed} =
        outbox_subscribe(
            #{},
            subscription_req(<<"Debit-Notice">>, <<"alice">>, <<"listener-a">>, 21),
            Opts
        ),
    {ok, BothSubscribed} =
        outbox_subscribe(
            AliceSubscribed,
            subscription_req(<<"Debit-Notice">>, <<"bob">>, <<"listener-b">>, 22),
            Opts
        ),
    ?assertEqual(
        [<<"listener-a">>],
        outbox_subscribers(
            BothSubscribed,
            <<"Debit-Notice">>,
            <<"alice">>,
            Opts
        )
    ),
    ?assertEqual(
        [<<"listener-b">>],
        outbox_subscribers(
            BothSubscribed,
            <<"Debit-Notice">>,
            <<"bob">>,
            Opts
        )
    ),
    Updated =
        outbox_send(
            #{
                <<"action">> => <<"Debit-Notice">>,
                <<"target">> => <<"alice">>,
                <<"quantity">> => 5
            },
            BothSubscribed,
            Opts
        ),
    Notices = outbox(Updated, Opts),
    ?assert(has_message(
        [
            {<<"action">>, <<"notify">>},
            {<<"target">>, <<"listener-a">>},
            {<<"x-target">>, <<"alice">>},
            {<<"x-quantity">>, 5}
        ],
        Notices,
        Opts
    )),
    ?assertNot(has_message(
        [
            {<<"action">>, <<"notify">>},
            {<<"target">>, <<"listener-b">>}
        ],
        Notices,
        Opts
    )).

outbox_unsubscribe_removes_listener_test() ->
    Opts = opts(),
    Req = subscription_req(<<"Debit-Notice">>, <<"alice">>, <<"listener-a">>, 31),
    {ok, Subscribed} = outbox_subscribe(#{}, Req, Opts),
    {ok, Unsubscribed} = outbox_unsubscribe(Subscribed, Req, Opts),
    ?assertEqual(
        [],
        outbox_subscribers(
            Unsubscribed,
            <<"Debit-Notice">>,
            <<"alice">>,
            Opts
        )
    ),
    Updated =
        outbox_send(
            #{
                <<"action">> => <<"Debit-Notice">>,
                <<"target">> => <<"alice">>
            },
            Unsubscribed,
            Opts
        ),
    ?assertEqual(1, length(outbox(Updated, Opts))).

fixed_supply_transfer_test() ->
    Opts = opts(),
    Alice = id(<<"alice">>),
    Bob = id(<<"bob">>),
    Base =
        token_state(
            #{
                total_supply => 1,
                initial_balances => #{ Alice => 1 }
            },
            Opts
        ),
    {ok, Updated} = transfer(Base, Alice, Bob, 1, Opts),
    ?assertEqual(0, balance(Updated, Alice, Opts)),
    ?assertEqual(1, balance(Updated, Bob, Opts)),
    ?assertEqual(1, hb_ao:get(<<"total-supply">>, Updated, Opts)).

fixed_supply_without_mint_device_cannot_mint_test() ->
    Opts = opts(),
    Owner = id(<<"owner">>),
    Minter = id(<<"minter">>),
    Base =
        token_state(
            #{
                total_supply => 1,
                initial_balances => #{ Owner => 1 }
            },
            Opts
        ),
    ?assertEqual(not_found, hb_ao:get(<<"mint-device">>, Base, Opts)),
    {ok, MintRejected} = mint(Base, Minter, Minter, 1, Opts),
    [Notice] = outbox(MintRejected, Opts),
    ?assertEqual(<<"mint-device not configured">>, hb_ao:get(<<"reason">>, Notice, Opts)),
    ?assertEqual(Minter, hb_ao:get(<<"target">>, Notice, Opts)),
    ?assertEqual(1, balance(MintRejected, Owner, Opts)),
    ?assertEqual(0, balance(MintRejected, Minter, Opts)),
    ?assertEqual(1, hb_ao:get(<<"total-supply">>, MintRejected, Opts)).

fixed_supply_name_token_flow_test() ->
    Opts = opts(),
    Owner = id(<<"owner">>),
    NewOwner = id(<<"new-owner">>),
    Admin = <<"pnp-admin">>,
    Base =
        token_state(
            #{
                total_supply => 1,
                initial_balances => #{ Owner => 1 },
                extra =>
                    #{
                        <<"set-authority">> => Admin,
                        <<"whitelisted-fields">> =>
                            [
                                <<"name">>,
                                <<"logo">>,
                                <<"asset-type">>
                            ]
                    }
            },
            Opts
        ),
    ?assertEqual(not_found, hb_ao:get(<<"mint-device">>, Base, Opts)),
    ?assertEqual(
        {error, <<"Too few acceptable committers present.">>},
        set_field(Base, Owner, #{ <<"name">> => <<"alice">> }, Opts)
    ),
    {ok, WithMetadata} =
        set_field(
            Base,
            Admin,
            #{
                <<"name">> => <<"pnp-name">>,
                <<"logo">> => <<"logo-tx-id">>,
                <<"asset-type">> => <<"pnp">>
            },
            Opts
        ),
    ?assertEqual(<<"pnp-name">>, hb_ao:get(<<"name">>, WithMetadata, Opts)),
    ?assertEqual(<<"logo-tx-id">>, hb_ao:get(<<"logo">>, WithMetadata, Opts)),
    ?assertEqual(<<"pnp">>, hb_ao:get(<<"asset-type">>, WithMetadata, Opts)),
    ?assertEqual(
        {error, <<"Attempted to set non-whitelisted fields.">>},
        set_field(WithMetadata, Admin, #{ <<"total-supply">> => 2 }, Opts)
    ),
    {ok, Transferred} = transfer(WithMetadata, Owner, NewOwner, 1, Opts),
    ?assertEqual(0, balance(Transferred, Owner, Opts)),
    ?assertEqual(1, balance(Transferred, NewOwner, Opts)),
    ?assertEqual(1, hb_ao:get(<<"total-supply">>, Transferred, Opts)),
    {ok, MintRejected} = mint(Transferred, Owner, Owner, 1, Opts),
    [Notice | _] = outbox(MintRejected, Opts),
    ?assertEqual(<<"mint-device not configured">>, hb_ao:get(<<"reason">>, Notice, Opts)),
    ?assertEqual(Owner, hb_ao:get(<<"target">>, Notice, Opts)),
    ?assertEqual(0, balance(MintRejected, Owner, Opts)),
    ?assertEqual(1, balance(MintRejected, NewOwner, Opts)),
    ?assertEqual(1, hb_ao:get(<<"total-supply">>, MintRejected, Opts)).

insufficient_balance_transfer_rejected_test() ->
    Opts = opts(),
    Alice = id(<<"alice">>),
    Bob = id(<<"bob">>),
    Base =
        token_state(
            #{ initial_balances => #{ Alice => 5 } },
            Opts
        ),
    {ok, Updated} = transfer(Base, Alice, Bob, 6, Opts),
    ?assertEqual(5, balance(Updated, Alice, Opts)),
    ?assertEqual(0, balance(Updated, Bob, Opts)),
    [Notice] = outbox(Updated, Opts),
    ?assertEqual(Alice, hb_ao:get(<<"target">>, Notice, Opts)),
    ?assertEqual(
        <<"Insufficient balance.">>,
        hb_ao:get(<<"reason">>, Notice, Opts)
    ).

negative_quantity_transfer_rejected_test() ->
    Opts = opts(),
    Alice = id(<<"alice">>),
    Bob = id(<<"bob">>),
    Base =
        token_state(
            #{ initial_balances => #{ Alice => 5 } },
            Opts
        ),
    {ok, Updated} = transfer(Base, Alice, Bob, -1, Opts),
    ?assertEqual(5, balance(Updated, Alice, Opts)),
    ?assertEqual(0, balance(Updated, Bob, Opts)),
    [Notice] = outbox(Updated, Opts),
    ?assertEqual(Alice, hb_ao:get(<<"target">>, Notice, Opts)),
    ?assertEqual(
        <<"Quantity must be a non-negative integer.">>,
        hb_ao:get(<<"reason">>, Notice, Opts)
    ).

self_transfer_keeps_balance_test() ->
    Opts = opts(),
    Alice = id(<<"alice">>),
    Base =
        token_state(
            #{ initial_balances => #{ Alice => 5 } },
            Opts
        ),
    {ok, Updated} = transfer(Base, Alice, Alice, 3, Opts),
    ?assertEqual(5, balance(Updated, Alice, Opts)),
    ?assertEqual(5, hb_ao:get(<<"total-supply">>, Updated, Opts)).

reserved_recipient_transfer_rejected_test() ->
    Opts = opts(),
    Alice = id(<<"alice">>),
    Base =
        token_state(
            #{
                total_supply => 1,
                initial_balances => #{ Alice => 1 }
            },
            Opts
        ),
    {ok, Updated} = transfer(Base, Alice, <<"device">>, 1, Opts),
    ?assertEqual(1, balance(Updated, Alice, Opts)),
    ?assertEqual(0, balance(Updated, <<"device">>, Opts)),
    ?assertEqual(1, hb_ao:get(<<"total-supply">>, Updated, Opts)).

reserved_ao_recipient_transfer_rejected_test() ->
    Opts = opts(),
    Alice = id(<<"alice">>),
    Base =
        token_state(
            #{
                total_supply => 1,
                initial_balances => #{ Alice => 1 }
            },
            Opts
        ),
    {ok, Updated} = transfer(Base, Alice, <<"path">>, 1, Opts),
    ?assertEqual(1, balance(Updated, Alice, Opts)),
    ?assertEqual(0, balance(Updated, <<"path">>, Opts)),
    ?assertEqual(1, hb_ao:get(<<"total-supply">>, Updated, Opts)).

non_whitelisted_set_field_rejected_test() ->
    Opts = opts(),
    Setter = id(<<"setter">>),
    Base =
        token_state(
            #{
                extra =>
                    #{
                        <<"set-authority">> => Setter,
                        <<"whitelisted-fields">> =>
                            [
                                <<"set-authority">>,
                                <<"set-authority-required">>,
                                <<"set-authority-match">>,
                                <<"name">>,
                                <<"ticker">>,
                                <<"denomination">>,
                                <<"logo">>
                            ]
                    }
            },
            Opts
        ),
    ?assertEqual(
        {error, <<"Attempted to set non-whitelisted fields.">>},
        set_field(Base, Setter, #{ <<"balances">> => #{ <<"fake">> => 1 } }, Opts)
    ).

default_whitelist_wildcard_allows_set_test() ->
    Opts = opts(),
    Setter = id(<<"setter">>),
    Base =
        token_state(
            #{
                extra => #{ <<"set-authority">> => Setter }
            },
            Opts
        ),
    {ok, Updated} =
        set_field(Base, Setter, #{ <<"logo">> => <<"logo-a">> }, Opts),
    ?assertEqual(<<"logo-a">>, hb_ao:get(<<"logo">>, Updated, Opts)).

signed_set_strips_commitments_before_whitelist_test() ->
    Opts = opts(),
    Setter = id(<<"setter">>),
    Base =
        token_state(
            #{
                extra =>
                    #{
                        <<"set-authority">> => Setter,
                        <<"whitelisted-fields">> => [<<"logo">>]
                    }
            },
            Opts
        ),
    {ok, Updated} =
        signed_set_field(Base, Setter, #{ <<"logo">> => <<"logo-signed">> }, Opts),
    ?assertEqual(<<"logo-signed">>, hb_ao:get(<<"logo">>, Updated, Opts)).

set_does_not_persist_control_keys_test() ->
    Opts = opts(),
    Setter = id(<<"setter">>),
    Base =
        token_state(
            #{
                extra => #{ <<"set-authority">> => Setter }
            },
            Opts
        ),
    ControlFields =
        #{
            <<"action">> => <<"Transfer">>,
            <<"path">> => <<"evil-path">>,
            <<"body">> => #{ <<"evil">> => true },
            <<"commitments">> => #{ <<"evil">> => true },
            <<"committers">> => [<<"evil">>],
            <<"ao-types">> => <<"evil=atom">>,
            <<"target">> => <<"evil-target">>,
            <<"type">> => <<"Message">>,
            <<"id">> => <<"evil-id">>,
            <<"timestamp">> => 1,
            <<"variant">> => <<"evil-variant">>,
            <<"data-protocol">> => <<"evil-protocol">>,
            <<"set-mode">> => <<"explicit">>,
            <<"priv">> => #{ <<"evil">> => true },
            <<"hashpath">> => <<"evil-hashpath">>
        },
    {ok, Updated} =
        set_field(
            Base,
            Setter,
            ControlFields#{ <<"logo">> => <<"logo-a">> },
            Opts
        ),
    ?assertEqual(<<"logo-a">>, hb_ao:get(<<"logo">>, Updated, Opts)),
    ?assertEqual(not_found, hb_maps:get(<<"action">>, Updated, not_found, Opts)),
    ?assertEqual(not_found, hb_maps:get(<<"path">>, Updated, not_found, Opts)),
    ?assertEqual(not_found, hb_maps:get(<<"body">>, Updated, not_found, Opts)),
    ?assertEqual([], hb_message:signers(Updated, Opts)),
    ?assert(hb_message:verify(Updated, all, Opts)),
    ?assertNot(hb_maps:is_key(<<"evil">>,
        hb_maps:get(<<"commitments">>, Updated, #{}, Opts), Opts)),
    ?assertEqual(not_found, hb_maps:get(<<"committers">>, Updated, not_found, Opts)),
    ?assertEqual(not_found, hb_maps:get(<<"ao-types">>, Updated, not_found, Opts)),
    ?assertEqual(not_found, hb_maps:get(<<"target">>, Updated, not_found, Opts)),
    ?assertEqual(not_found, hb_maps:get(<<"type">>, Updated, not_found, Opts)),
    ?assertEqual(not_found, hb_maps:get(<<"id">>, Updated, not_found, Opts)),
    ?assertEqual(not_found, hb_maps:get(<<"timestamp">>, Updated, not_found, Opts)),
    ?assertEqual(not_found, hb_maps:get(<<"variant">>, Updated, not_found, Opts)),
    ?assertEqual(
        not_found,
        hb_maps:get(<<"data-protocol">>, Updated, not_found, Opts)
    ),
    ?assertEqual(not_found, hb_maps:get(<<"set-mode">>, Updated, not_found, Opts)),
    ?assertNotEqual(
        #{ <<"evil">> => true },
        hb_maps:get(<<"priv">>, Updated, not_found, Opts)
    ),
    ?assertEqual(not_found, hb_maps:get(<<"hashpath">>, Updated, not_found, Opts)).

set_authority_dynamic_full_owner_default_test() ->
    Opts = opts(),
    Owner = id(<<"owner">>),
    Base =
        token_state(
            #{
                total_supply => 10,
                initial_balances => #{ Owner => 10 }
            },
            Opts
        ),
    {ok, Updated} =
        set_field(Base, Owner, #{ <<"logo">> => <<"logo-a">> }, Opts),
    ?assertEqual(<<"logo-a">>, hb_ao:get(<<"logo">>, Updated, Opts)).

set_authority_dynamic_tracks_current_full_owner_test() ->
    Opts = opts(),
    Owner = id(<<"owner">>),
    NewOwner = id(<<"new-owner">>),
    Base =
        token_state(
            #{
                total_supply => 1,
                initial_balances => #{ Owner => 1 }
            },
            Opts
        ),
    {ok, WithLogoA} =
        set_field(Base, Owner, #{ <<"logo">> => <<"logo-a">> }, Opts),
    {ok, Transferred} = transfer(WithLogoA, Owner, NewOwner, 1, Opts),
    ?assertEqual(
        {error, <<"Supply-threshold owner requirement not satisfied.">>},
        set_field(Transferred, Owner, #{ <<"logo">> => <<"logo-b">> }, Opts)
    ),
    {ok, WithLogoC} =
        set_field(Transferred, NewOwner, #{ <<"logo">> => <<"logo-c">> }, Opts),
    ?assertEqual(<<"logo-c">>, hb_ao:get(<<"logo">>, WithLogoC, Opts)).

set_authority_dynamic_split_supply_rejected_test() ->
    Opts = opts(),
    Alice = id(<<"alice">>),
    Bob = id(<<"bob">>),
    Base =
        token_state(
            #{
                total_supply => 10,
                initial_balances => #{ Alice => 5, Bob => 5 }
            },
            Opts
        ),
    ?assertEqual(
        {error, <<"Supply-threshold owner requirement not satisfied.">>},
        set_field(Base, Alice, #{ <<"logo">> => <<"logo-a">> }, Opts)
    ).

set_authority_dynamic_threshold_bps_allows_configured_owner_test() ->
    Opts = opts(),
    Alice = id(<<"alice">>),
    Bob = id(<<"bob">>),
    Base =
        token_state(
            #{
                total_supply => 10,
                initial_balances => #{ Alice => 5, Bob => 5 },
                extra =>
                    #{
                        <<"set-authority-template">> => <<"supply-threshold-owner">>,
                        <<"set-authority-threshold-bps">> => 5000
                    }
            },
            Opts
        ),
    {ok, Updated} =
        set_field(Base, Alice, #{ <<"logo">> => <<"logo-a">> }, Opts),
    ?assertEqual(<<"logo-a">>, hb_ao:get(<<"logo">>, Updated, Opts)).

set_authority_dynamic_zero_supply_rejected_test() ->
    Opts = opts(),
    Setter = id(<<"setter">>),
    Base = token_state(#{}, Opts),
    ?assertEqual(
        {error, <<"Total supply must be positive.">>},
        set_field(Base, Setter, #{ <<"logo">> => <<"logo-a">> }, Opts)
    ).

set_authority_static_uses_dev_security_test() ->
    Opts = opts(),
    Setter = id(<<"setter">>),
    Base =
        token_state(
            #{
                extra => #{ <<"set-authority">> => Setter }
            },
            Opts
        ),
    {ok, Updated} =
        set_field(Base, Setter, #{ <<"logo">> => <<"logo-a">> }, Opts),
    ?assertEqual(<<"logo-a">>, hb_ao:get(<<"logo">>, Updated, Opts)),
    ?assertEqual(
        {error, <<"Too few acceptable committers present.">>},
        set_field(Base, id(<<"other">>), #{ <<"logo">> => <<"logo-b">> }, Opts)
    ).

set_authority_required_uses_dev_security_test() ->
    Opts = opts(),
    Setter = id(<<"setter">>),
    Base =
        token_state(
            #{
                extra =>
                    #{
                        <<"set-authority">> => [Setter],
                        <<"set-authority-required">> => [Setter]
                    }
            },
            Opts
        ),
    {ok, Updated} =
        set_field(Base, Setter, #{ <<"logo">> => <<"logo-a">> }, Opts),
    ?assertEqual(<<"logo-a">>, hb_ao:get(<<"logo">>, Updated, Opts)).

set_authority_match_uses_dev_security_test() ->
    Opts = opts(),
    Setter = id(<<"setter">>),
    Base =
        token_state(
            #{
                extra =>
                    #{
                        <<"set-authority">> => [Setter],
                        <<"set-authority-match">> => 1
                    }
            },
            Opts
        ),
    {ok, Updated} =
        set_field(Base, Setter, #{ <<"logo">> => <<"logo-a">> }, Opts),
    ?assertEqual(<<"logo-a">>, hb_ao:get(<<"logo">>, Updated, Opts)).

%%% Arweave-native asset composition

party() ->
    Wallet = ar_wallet:new(),
    {Wallet, hb_util:human_id(ar_wallet:to_address(Wallet))}.

asset_tx(Wallet, Fields) ->
    hb_message:commit(
        Fields,
        #{ <<"priv-wallet">> => Wallet },
        #{ <<"commitment-device">> => <<"tx@1.0">> }
    ).

asset_apply(Base, Body, Height, Opts) ->
    {ok, Updated} =
        (token()):compute(
            Base,
            asset_assignment(Body, Height, Opts),
            Opts
        ),
    Updated.

asset_assignment(Body, Height, Opts) ->
    hb_message:commit(#{
        <<"path">> => <<"compute">>,
        <<"process">> => ?ASSET_PROCESS,
        <<"slot">> => Height,
        <<"block-height">> => Height,
        <<"body">> => Body
    }, Opts).

asset_base(Owner, Opts) ->
    {ok, Initialized} =
        (token()):init(
            #{
                <<"device">> => <<"token@1.0">>,
                <<"execution-device">> => <<"token@1.0">>,
                <<"swap-device">> => <<"arweave-swap@1.0">>,
                <<"scheduler-device">> => <<"arweave-scheduler@1.0">>,
                <<"scheduler">> => hb_opts:address(Opts),
                <<"scheduler-mode">> => <<"all">>,
                <<"initial-holder">> => Owner,
                <<"total-supply">> => 1,
                <<"denomination">> => 0
            },
            #{},
            Opts
        ),
    Initialized.

asset_balance(Base, Account, Opts) ->
    {ok, Amount} = (token()):balance(Base, #{ <<"balance">> => Account }, Opts),
    Amount.

asset_orders(Base, Opts) ->
    [
        Order
    ||
        Order <- hb_maps:values(
            hb_maps:get(<<"orders">>, Base, #{}, Opts),
            Opts
        ),
        is_map(Order),
        hb_maps:get(<<"order-id">>, Order, not_found, Opts) =/= not_found
    ].

asset_offer(Wallet, Asking) ->
    asset_tx(
        Wallet,
        #{
            <<"target">> => ?ASSET_PROCESS,
            <<"action">> => <<"make-offer">>,
            <<"offer-quantity">> => <<"1">>,
            <<"asking">> => hb_util:bin(Asking),
            <<"deposit">> => <<"0">>,
            <<"minimum-fee">> => <<"0">>,
            <<"deadline">> => <<"20">>
        }
    ).

asset_order_action(Wallet, Action, OrderID) ->
    asset_tx(
        Wallet,
        #{
            <<"target">> => ?ASSET_PROCESS,
            <<"action">> => Action,
            <<"order-id">> => OrderID
        }
    ).

asset_payment(Wallet, Seller, Asking, OrderID) ->
    asset_tx(
        Wallet,
        #{
            <<"target">> => Seller,
            <<"quantity">> => hb_util:bin(Asking),
            <<"order-id">> => OrderID
        }
    ).

asset_transfer(Wallet, Recipient) ->
    asset_tx(
        Wallet,
        #{
            <<"target">> => ?ASSET_PROCESS,
            <<"action">> => <<"transfer">>,
            <<"recipient">> => Recipient,
            <<"quantity">> => <<"1">>
        }
    ).

scalar_initial_holder_seeds_once_test() ->
    Opts = opts(),
    {_, Owner} = party(),
    {_, NewOwner} = party(),
    Seeded = asset_base(Owner, Opts),
    ?assertEqual(1, asset_balance(Seeded, Owner, Opts)),
    ?assertEqual(0, asset_balance(Seeded, NewOwner, Opts)),
    {ok, Moved} = transfer(Seeded, Owner, NewOwner, 1, Opts),
    {ok, Reinitialized} = (token()):init(Moved, #{}, Opts),
    ?assertEqual(0, asset_balance(Reinitialized, Owner, Opts)),
    ?assertEqual(1, asset_balance(Reinitialized, NewOwner, Opts)).

swap_asset_transfer_updates_live_owner_test() ->
    Opts = opts(),
    {OwnerWallet, Owner} = party(),
    {_, Recipient} = party(),
    Transferred =
        asset_apply(
            asset_base(Owner, Opts),
            asset_transfer(OwnerWallet, Recipient),
            100,
            Opts
        ),
    ?assertEqual(0, asset_balance(Transferred, Owner, Opts)),
    ?assertEqual(1, asset_balance(Transferred, Recipient, Opts)).

malformed_actions_do_not_block_transfer_test() ->
    Opts = opts(),
    {OwnerWallet, Owner} = party(),
    {_, Recipient} = party(),
    lists:foreach(
        fun(Swap) ->
            Seeded = asset_base(Owner, Opts),
            Base = case Swap of
                true -> Seeded;
                false ->
                    {ok, Standard} = hb_ao:resolve(
                        hb_maps:remove(<<"swap-device">>, Seeded, Opts),
                        <<"init">>,
                        Opts
                    ),
                    Standard
            end,
            Unchanged = lists:foldl(
                fun(Action, State) ->
                    Body = asset_tx(OwnerWallet, #{
                        <<"target">> => ?ASSET_PROCESS,
                        <<"action">> => Action
                    }),
                    {ok, Next} = hb_ao:resolve(
                        State, asset_assignment(Body, 100, Opts), Opts),
                    ?assertEqual(1, asset_balance(Next, Owner, Opts)),
                    ?assertEqual(0, asset_balance(Next, Recipient, Opts)),
                    Next
                end,
                Base,
                [3.14, #{}, [3.14]]
            ),
            {ok, Transferred} = hb_ao:resolve(
                Unchanged,
                asset_assignment(asset_transfer(OwnerWallet, Recipient), 101, Opts),
                Opts
            ),
            ?assertEqual(0, asset_balance(Transferred, Owner, Opts)),
            ?assertEqual(1, asset_balance(Transferred, Recipient, Opts))
        end,
        [true, false]
    ).

request_device_does_not_block_token_test() ->
    Opts = opts(),
    {OwnerWallet, Owner} = party(),
    {StrangerWallet, Stranger} = party(),
    {_, Recipient} = party(),
    lists:foreach(
        fun(Swap) ->
            Seeded = asset_base(Owner, Opts),
            Base = case Swap of
                true -> Seeded;
                false ->
                    {ok, Standard} = hb_ao:resolve(
                        hb_maps:remove(<<"swap-device">>, Seeded, Opts),
                        <<"init">>,
                        Opts
                    ),
                    Standard
            end,
            Unchanged = lists:foldl(
                fun(Device, State) ->
                    Body = asset_tx(StrangerWallet, #{
                        <<"target">> => ?ASSET_PROCESS,
                        <<"action">> => <<"transfer">>,
                        <<"recipient">> => Stranger,
                        <<"quantity">> => <<"1">>,
                        <<"device">> => Device
                    }),
                    Next = asset_apply(State, Body, 100, Opts),
                    ?assertEqual(1, asset_balance(Next, Owner, Opts)),
                    ?assertEqual(0, asset_balance(Next, Stranger, Opts)),
                    Next
                end,
                Base,
                [<<"nope@9.9">>, <<"trie@1.0">>]
            ),
            Transferred = asset_apply(
                Unchanged, asset_transfer(OwnerWallet, Recipient), 101, Opts
            ),
            ?assertEqual(0, asset_balance(Transferred, Owner, Opts)),
            ?assertEqual(1, asset_balance(Transferred, Recipient, Opts))
        end,
        [false, true]
    ).

request_fields_are_literal_test() ->
    Opts = opts(),
    {_, Owner} = party(),
    {_, Recipient} = party(),
    Base = asset_base(Owner, Opts),
    ?assertEqual({ok, 1}, (token()):balance(Base, #{
        <<"device">> => <<"nope@9.9">>, <<"balance">> => Owner
    }, Opts)),
    {ok, Transferred} = (token()):handle_action(<<"transfer">>, Base, #{
        <<"body">> => #{
            <<"device">> => <<"nope@9.9">>,
            <<"from">> => Owner,
            <<"recipient">> => Recipient,
            <<"quantity">> => <<"1">>
        }
    }, Opts),
    ?assertEqual(0, asset_balance(Transferred, Owner, Opts)),
    ?assertEqual(1, asset_balance(Transferred, Recipient, Opts)).

scalar_initial_holder_requires_valid_scalars_test() ->
    Opts = opts(),
    {_, Owner} = party(),
    lists:foreach(
        fun(Base) ->
            {ok, Initialized} = (token()):init(Base, #{}, Opts),
            ?assertEqual(
                not_found,
                hb_maps:get(<<"balances">>, Initialized, not_found, Opts)
            )
        end,
        [
            #{ <<"initial-holder">> => #{}, <<"total-supply">> => 1 },
            #{ <<"initial-holder">> => Owner, <<"total-supply">> => <<"one">> },
            #{ <<"initial-holder">> => Owner, <<"total-supply">> => -1 }
        ]
    ).

swap_offer_and_cancel_round_trip_test() ->
    Opts = opts(),
    {Seller, SellerAddr} = party(),
    Base = asset_base(SellerAddr, Opts),
    Offer = asset_offer(Seller, 500),
    ?assertMatch(
        {ok, _},
        hb_ao:resolve(
            Base#{ <<"device">> => <<"arweave-swap@1.0">> },
            asset_assignment(Offer, 100, Opts),
            Opts
        )
    ),
    Opened = asset_apply(Base, Offer, 100, Opts),
    [Order] = asset_orders(Opened, Opts),
    OrderID = hb_maps:get(<<"order-id">>, Order, Opts),
    ?assertEqual(0, asset_balance(Opened, SellerAddr, Opts)),
    ?assertEqual(<<"token@1.0">>, hb_maps:get(<<"device">>, Opened, Opts)),
    Cancelled =
        asset_apply(
            Opened,
            asset_order_action(Seller, <<"cancel-order">>, OrderID),
            101,
            Opts
        ),
    ?assertEqual([], asset_orders(Cancelled, Opts)),
    ?assertEqual(1, asset_balance(Cancelled, SellerAddr, Opts)).

swap_reserved_native_payment_settles_asset_test() ->
    Opts = opts(),
    {Seller, SellerAddr} = party(),
    {Buyer, BuyerAddr} = party(),
    Opened =
        asset_apply(
            asset_base(SellerAddr, Opts),
            asset_offer(Seller, 500),
            100,
            Opts
        ),
    [Order] = asset_orders(Opened, Opts),
    OrderID = hb_maps:get(<<"order-id">>, Order, Opts),
    Reserved =
        asset_apply(
            Opened,
            asset_order_action(Buyer, <<"register-interest">>, OrderID),
            110,
            Opts
        ),
    [Reservation] = asset_orders(Reserved, Opts),
    ?assertEqual(
        <<"reserved">>,
        hb_maps:get(<<"status">>, Reservation, Opts)
    ),
    ?assertEqual(account_key(BuyerAddr), hb_maps:get(<<"buyer">>, Reservation, Opts)),
    Settled =
        asset_apply(
            Reserved,
            asset_payment(Buyer, SellerAddr, 500, OrderID),
            111,
            Opts
        ),
    ?assertEqual([], asset_orders(Settled, Opts)),
    ?assertEqual(0, asset_balance(Settled, SellerAddr, Opts)),
    ?assertEqual(1, asset_balance(Settled, BuyerAddr, Opts)),
    ?assertEqual(<<"token@1.0">>, hb_maps:get(<<"device">>, Settled, Opts)).

unrelated_all_mode_traffic_is_ignored_test() ->
    Opts = opts(),
    {_, Owner} = party(),
    {Stranger, StrangerAddr} = party(),
    Base = asset_base(Owner, Opts),
    Result =
        asset_apply(
            Base,
            asset_tx(
                Stranger,
                #{
                    <<"target">> => StrangerAddr,
                    <<"action">> => <<"make-offer">>,
                    <<"offer-quantity">> => <<"1">>,
                    <<"asking">> => <<"500">>
                }
            ),
            100,
            Opts
        ),
    ?assertEqual(1, asset_balance(Result, Owner, Opts)),
    ?assertEqual([], asset_orders(Result, Opts)),
    ?assertEqual([], outbox(Result, Opts)).

malformed_address_rejected_without_throwing_test() ->
    lists:foreach(
        fun(Address) ->
            ?assertEqual(
                {error, <<"Address contains unsupported characters.">>},
                (token()):validate_address(Address, [])
            )
        end,
        [<<255>>, <<16#C3>>, <<16#80>>, <<16#C0, 16#AF>>]
    ),
    ?assertEqual(
        {error, <<"Address is too long.">>},
        (token()):validate_address(binary:copy(<<255>>, 129), [])
    ),
    ?assertEqual(
        {error, <<"Address must be a binary.">>},
        (token()):validate_address(42, [])
    ).

malformed_balance_and_mint_subjects_return_errors_test() ->
    lists:foreach(
        fun({Address, Error}) ->
            ?assertEqual(
                {error, Error},
                (token()):balance(#{}, #{ <<"balance">> => Address }, #{})
            ),
            ?assertEqual(
                {error, Error},
                (token()):mint(
                    #{},
                    #{ <<"body">> => #{ <<"subject">> => Address } },
                    #{}
                )
            )
        end,
        [
            {<<255>>, <<"Address contains unsupported characters.">>},
            {42, <<"Address must be a binary.">>}
        ]
    ).

malformed_recipients_do_not_block_followup_transfer_test() ->
    Opts = opts(),
    Alice = id(<<"alice">>),
    Bob = id(<<"bob">>),
    Base = token_state(#{ initial_balances => #{ Alice => 1 } }, Opts),
    Rejected = lists:foldl(
        fun({Recipient, Reason}, State) ->
            {ok, Next} = transfer(State, Alice, Recipient, 1, Opts),
            ?assertEqual(1, balance(Next, Alice, Opts)),
            ?assertEqual(0, balance(Next, Bob, Opts)),
            [Notice | _] = outbox(Next, Opts),
            ?assertEqual(Alice, hb_ao:get(<<"target">>, Notice, Opts)),
            ?assertEqual(Reason, hb_ao:get(<<"reason">>, Notice, Opts)),
            Next
        end,
        Base,
        [
            {<<255>>, <<"Address contains unsupported characters.">>},
            {42, <<"Address must be a binary.">>}
        ]
    ),
    {ok, Updated} = transfer(Rejected, Alice, Bob, 1, Opts),
    ?assertEqual(0, balance(Updated, Alice, Opts)),
    ?assertEqual(1, balance(Updated, Bob, Opts)).

l1_transfer_body(Wallet, Recipient, NativeQuantity, QuantityTags, Opts) ->
    Signed = ar_tx:sign(
        #tx{
            format = 2,
            target = hb_util:native_id(id(<<"target">>)),
            quantity = NativeQuantity,
            reward = 1,
            tags = [
                {<<"Action">>, <<"Transfer">>},
                {<<"Recipient">>, Recipient}
                | QuantityTags
            ]
        },
        Wallet
    ),
    hb_message:convert(Signed, <<"structured@1.0">>, <<"tx@1.0">>, Opts).

signed_l1_quantity_tag_overrides_native_quantity_test() ->
    Opts = opts(),
    {Wallet, Alice} = party(),
    Bob = id(<<"bob">>),
    Base = token_state(#{ initial_balances => #{ Alice => 1000 } }, Opts),
    Body = l1_transfer_body(Wallet, Bob, 1, [{<<"Quantity">>, <<"700">>}], Opts),
    ?assertEqual({ok, 1}, hb_util:safe_int(hb_maps:get(<<"quantity">>, Body, Opts))),
    {ok, Updated} = (token()):handle_action(
        <<"transfer">>, Base, #{ <<"body">> => Body#{ <<"from">> => Alice } }, Opts
    ),
    ?assertEqual(300, balance(Updated, Alice, Opts)),
    ?assertEqual(700, balance(Updated, Bob, Opts)),
    ?assertEqual(
        [700, 700],
        lists:sort([hb_ao:get(<<"quantity">>, Notice, Opts) || Notice <- outbox(Updated, Opts)])
    ).

signed_l1_missing_quantity_tag_rejected_test() ->
    Opts = opts(),
    {Wallet, Alice} = party(),
    Bob = id(<<"bob">>),
    Base = token_state(#{ initial_balances => #{ Alice => 1000 } }, Opts),
    Body = l1_transfer_body(Wallet, Bob, 1, [], Opts),
    ?assertEqual({ok, 1}, hb_util:safe_int(hb_maps:get(<<"quantity">>, Body, Opts))),
    {ok, Updated} = (token()):handle_action(
        <<"transfer">>, Base, #{ <<"body">> => Body#{ <<"from">> => Alice } }, Opts
    ),
    ?assertEqual(1000, balance(Updated, Alice, Opts)),
    ?assertEqual(0, balance(Updated, Bob, Opts)),
    [Notice] = outbox(Updated, Opts),
    ?assertEqual(<<"Quantity tag is required.">>, hb_ao:get(<<"reason">>, Notice, Opts)).

signed_l1_duplicate_quantity_tags_rejected_test() ->
    Opts = opts(),
    {Wallet, Alice} = party(),
    Bob = id(<<"bob">>),
    Base = token_state(#{ initial_balances => #{ Alice => 1000 } }, Opts),
    Body = l1_transfer_body(
        Wallet,
        Bob,
        1,
        [{<<"Quantity">>, <<"700">>}, {<<"quantity">>, <<"1">>}],
        Opts
    ),
    {ok, Updated} = (token()):handle_action(
        <<"transfer">>, Base, #{ <<"body">> => Body#{ <<"from">> => Alice } }, Opts
    ),
    ?assertEqual(1000, balance(Updated, Alice, Opts)),
    ?assertEqual(0, balance(Updated, Bob, Opts)),
    [Notice] = outbox(Updated, Opts),
    ?assertEqual(<<"Ambiguous Quantity tags.">>, hb_ao:get(<<"reason">>, Notice, Opts)).

signed_l1_quantity_survives_security_projection_test() ->
    Opts = opts(),
    {Wallet, Alice} = party(),
    Bob = id(<<"bob">>),
    Body = l1_transfer_body(Wallet, Bob, 1, [{<<"Quantity">>, <<"700">>}], Opts),
    SecureBody = (hb_message:uncommitted(Body, Opts))#{ <<"from">> => Alice },
    {ok, Corrected} = (token()):restore_signed_quantity(
        <<"transfer">>,
        #{ <<"body">> => Body },
        #{ <<"body">> => SecureBody },
        Opts
    ),
    CorrectedBody = hb_maps:get(<<"body">>, Corrected, Opts),
    ?assertEqual(<<"700">>, hb_maps:get(<<"quantity">>, CorrectedBody, Opts)),
    ?assertEqual(Alice, hb_maps:get(<<"from">>, CorrectedBody, Opts)),
    ?assertEqual(not_found, hb_message:commitment(
        #{ <<"commitment-device">> => <<"tx@1.0">> }, CorrectedBody, Opts
    )).

zero_l1_transfer_skips_mint_and_balance_writes_test() ->
    Opts = opts(),
    {Wallet, Sender} = party(),
    Recipient = id(<<"Bob">>),
    Base0 = token_state(#{ initial_balances => #{ Sender => 100 } }, Opts),
    Base = hb_ao:set(Base0, #{
        <<"process">> => Base0,
        <<"mint-device">> => #{
            <<"mint">> => fun(MintBase, _, MintOpts) ->
                {ok, hb_ao:set(MintBase, <<"mint-called">>, true, MintOpts)}
            end
        }
    }, Opts),
    Body = l1_transfer_body(
        Wallet, Recipient, 1, [{<<"Quantity">>, <<"0">>}], Opts
    ),
    {ok, Updated} = (token()):handle_action(
        <<"transfer">>, Base, #{ <<"body">> => Body#{ <<"from">> => Sender } }, Opts
    ),
    ?assertEqual(not_found, hb_ao:get(<<"mint-called">>, Updated, not_found, Opts)),
    ?assertEqual(
        hb_message:id(hb_ao:get(<<"balances">>, Base, Opts), all, Opts),
        hb_message:id(hb_ao:get(<<"balances">>, Updated, Opts), all, Opts)
    ),
    ?assertEqual(100, balance(Updated, Sender, Opts)),
    ?assertEqual(0, balance(Updated, Recipient, Opts)),
    Notices = outbox(Updated, Opts),
    ?assertEqual(2, length(Notices)),
    ?assert(has_message([
        {<<"action">>, <<"Debit-Notice">>},
        {<<"target">>, account_key(Sender)}, {<<"quantity">>, 0}
    ], Notices, Opts)),
    ?assert(has_message([
        {<<"action">>, <<"Credit-Notice">>},
        {<<"target">>, account_key(Recipient)}, {<<"quantity">>, 0}
    ], Notices, Opts)).

malformed_request_key_does_not_block_transfer_test() ->
    Opts = opts(),
    Alice = id(<<"alice">>),
    Bob = id(<<"bob">>),
    Base = token_state(#{ initial_balances => #{ Alice => 2 } }, Opts),
    {ok, Updated} = (token()):handle_action(
        <<"transfer">>,
        Base,
        #{
            <<"body">> =>
                #{
                    <<"from">> => Alice,
                    <<"recipient">> => Bob,
                    <<"quantity">> => 1,
                    <<16#C3, "x">> => <<"1">>
                }
        },
        Opts
    ),
    ?assertEqual(1, balance(Updated, Alice, Opts)),
    ?assertEqual(1, balance(Updated, Bob, Opts)).
