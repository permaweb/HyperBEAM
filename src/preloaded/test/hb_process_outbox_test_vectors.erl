%%% @doc Test vectors for the `process-outbox@1.0' preloaded package.
-module(hb_process_outbox_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

opts() ->
    hb:init(),
    #{
        <<"load-remote-devices">> => false,
        <<"priv-wallet">> => ar_wallet:new(),
        <<"store">> => [hb_test_utils:test_store()]
    }.

base() ->
    #{ <<"device">> => <<"process-outbox@1.0">> }.

call(Path, State, Req, Opts) ->
    hb_ao:resolve(State, Req#{ <<"path">> => Path }, Opts).

outbox(State, Opts) ->
    hb_util:message_to_ordered_list(
        hb_ao:get(<<"results/outbox">>, State, [], Opts),
        Opts
    ).

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

send_body_message_vector_test() ->
    Opts = opts(),
    Msg =
        #{
            <<"action">> => <<"Credit-Notice">>,
            <<"target">> => <<"alice">>,
            <<"quantity">> => 10
        },
    Req =
        #{
            <<"body">> => Msg,
            <<"x-trace">> => <<"trace-1">>
        },
    {ok, Updated} = call(<<"send">>, base(), Req, Opts),
    [Notice] = outbox(Updated, Opts),
    ?assertEqual(<<"Credit-Notice">>, hb_ao:get(<<"action">>, Notice, Opts)),
    ?assertEqual(<<"alice">>, hb_ao:get(<<"target">>, Notice, Opts)),
    ?assertEqual(10, hb_ao:get(<<"quantity">>, Notice, Opts)),
    ?assertEqual(<<"trace-1">>, hb_ao:get(<<"x-trace">>, Notice, Opts)).

batch_send_records_newest_first_vector_test() ->
    Opts = opts(),
    Req =
        #{
            <<"messages">> =>
                [
                    #{ <<"seq">> => 1 },
                    #{ <<"seq">> => 2 }
                ]
        },
    {ok, Updated} = call(<<"send">>, base(), Req, Opts),
    [Second, First] = outbox(Updated, Opts),
    ?assertEqual(2, hb_ao:get(<<"seq">>, Second, Opts)),
    ?assertEqual(1, hb_ao:get(<<"seq">>, First, Opts)).

subscribe_default_broadcast_vector_test() ->
    Opts = opts(),
    {ok, Subscribed} =
        call(
            <<"subscribe">>,
            base(),
            subscription_req(<<"Ping">>, default, <<"listener-a">>, 11),
            Opts
        ),
    {ok, Subscribers} =
        call(
            <<"subscribers">>,
            Subscribed,
            #{ <<"action">> => <<"Ping">> },
            Opts
        ),
    ?assertEqual([<<"listener-a">>], Subscribers),
    {ok, WithNotice} =
        call(
            <<"send">>,
            Subscribed,
            #{ <<"messages">> => #{ <<"action">> => <<"Ping">> } },
            Opts
        ),
    Notices = outbox(WithNotice, Opts),
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

targeted_subscription_vector_test() ->
    Opts = opts(),
    {ok, AliceSubscribed} =
        call(
            <<"subscribe">>,
            base(),
            subscription_req(<<"Debit-Notice">>, <<"alice">>, <<"listener-a">>, 21),
            Opts
        ),
    {ok, BothSubscribed} =
        call(
            <<"subscribe">>,
            AliceSubscribed,
            subscription_req(<<"Debit-Notice">>, <<"bob">>, <<"listener-b">>, 22),
            Opts
        ),
    {ok, Updated} =
        call(
            <<"send">>,
            BothSubscribed,
            #{
                <<"messages">> =>
                    #{
                        <<"action">> => <<"Debit-Notice">>,
                        <<"target">> => <<"alice">>,
                        <<"quantity">> => 5
                    }
            },
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

unsubscribe_removes_listener_vector_test() ->
    Opts = opts(),
    Req = subscription_req(<<"Debit-Notice">>, <<"alice">>, <<"listener-a">>, 31),
    {ok, Subscribed} = call(<<"subscribe">>, base(), Req, Opts),
    {ok, Unsubscribed} = call(<<"unsubscribe">>, Subscribed, Req, Opts),
    {ok, Subscribers} =
        call(
            <<"subscribers">>,
            Unsubscribed,
            #{
                <<"action">> => <<"Debit-Notice">>,
                <<"target">> => <<"alice">>
            },
            Opts
        ),
    ?assertEqual([], Subscribers),
    {ok, Updated} =
        call(
            <<"send">>,
            Unsubscribed,
            #{
                <<"messages">> =>
                    #{
                        <<"action">> => <<"Debit-Notice">>,
                        <<"target">> => <<"alice">>
                    }
            },
            Opts
        ),
    ?assertEqual(1, length(outbox(Updated, Opts))).

notify_only_vector_test() ->
    Opts = opts(),
    {ok, Subscribed} =
        call(
            <<"subscribe">>,
            base(),
            subscription_req(<<"Audit">>, <<"alice">>, <<"listener-a">>, 41),
            Opts
        ),
    {ok, Updated} =
        call(
            <<"notify">>,
            Subscribed,
            #{
                <<"messages">> =>
                    #{
                        <<"action">> => <<"Audit">>,
                        <<"target">> => <<"alice">>,
                        <<"quantity">> => 42
                    }
            },
            Opts
        ),
    [Notice] = outbox(Updated, Opts),
    ?assertEqual(<<"notify">>, hb_ao:get(<<"action">>, Notice, Opts)),
    ?assertEqual(<<"listener-a">>, hb_ao:get(<<"target">>, Notice, Opts)),
    ?assertEqual(<<"Audit">>, hb_ao:get(<<"x-action">>, Notice, Opts)),
    ?assertEqual(<<"alice">>, hb_ao:get(<<"x-target">>, Notice, Opts)),
    ?assertEqual(42, hb_ao:get(<<"x-quantity">>, Notice, Opts)).

malformed_subscribe_vector_test() ->
    Opts = opts(),
    ?assertEqual(
        {error, <<"No `subscribe-action` key to filter upon provided.">>},
        call(
            <<"subscribe">>,
            base(),
            #{ <<"body">> => #{ <<"from">> => <<"listener-a">> } },
            Opts
        )
    ),
    ?assertEqual(
        {error, <<"No security-normalized `from` key found in request.">>},
        call(
            <<"subscribe">>,
            base(),
            #{ <<"body">> => #{ <<"subscribe-action">> => <<"Ping">> } },
            Opts
        )
    ).

committer_host_accounts_vector_test() ->
    Opts = opts(),
    Target = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    Listener = hb_util:human_id(ar_wallet:to_address(maps:get(<<"priv-wallet">>, Opts))),
    HostTarget = hb_committer:uri(Target, Opts),
    HostListener = hb_committer:uri(Listener, Opts),
    Subscribed = lists:foldl(fun(Form, State) ->
        Request = hb_message:commit(subscription_req(<<"ping">>, Target, Form, 1), Opts),
        {ok, Next} = call(<<"subscribe">>, State, Request, Opts),
        Next
    end, base(), [Listener, <<"did:ar:", Listener/binary>>, HostListener]),
    lists:foreach(fun(Form) ->
        ?assertEqual({ok, [HostListener]}, call(<<"subscribers">>, Subscribed,
            #{ <<"action">> => <<"ping">>, <<"target">> => Form }, Opts))
    end, [Target, <<"did:ar:", Target/binary>>, HostTarget]),
    ?assertEqual({ok, []}, call(<<"subscribers">>, Subscribed,
        #{ <<"action">> => <<"ping">>, <<"target">> => Listener }, Opts)),
    {ok, Sent} = call(<<"send">>, Subscribed, #{ <<"messages">> => #{
        <<"action">> => <<"ping">>, <<"target">> => Target
    } }, Opts),
    ?assert(has_message([{<<"target">>, HostTarget}], outbox(Sent, Opts), Opts)),
    ?assert(has_message([{<<"target">>, HostListener}], outbox(Sent, Opts), Opts)),
    {ok, Unsubscribed} = call(<<"unsubscribe">>, Subscribed,
        subscription_req(<<"ping">>, HostTarget, HostListener, 1), Opts),
    ?assertEqual({ok, []}, call(<<"subscribers">>, Unsubscribed,
        #{ <<"action">> => <<"ping">>, <<"target">> => Target }, Opts)).

pkh_host_accounts_vector_test_() ->
    [{Normal, fun() ->
        Opts = opts(),
        Host = hb_committer:uri(Normal, Opts),
        ?assertMatch(<<"uri-", _/binary>>, Host),
        lists:foreach(fun(Form) ->
            ?assertEqual(Host, hb_committer:uri(Form, Opts)),
            {ok, State} = call(<<"subscribe">>, base(),
                subscription_req(<<"ping">>, Form, Form, 1), Opts),
            ?assertEqual({ok, [Host]}, call(<<"subscribers">>, State,
                #{ <<"action">> => <<"ping">>, <<"target">> => Host }, Opts)),
            {ok, Sent} = call(<<"send">>, State,
                #{ <<"messages">> => #{ <<"target">> => Form } }, Opts),
            ?assert(has_message([{<<"target">>, Host}], outbox(Sent, Opts), Opts))
        end, [Raw, Normal, Host])
    end} || {Raw, Normal} <- [
        {<<"0x52908400098527886E0F7030069857D2E4169EE7">>,
            <<"did:pkh:eip155:1:0x52908400098527886E0F7030069857D2E4169EE7">>},
        {<<"7S3P4HxJpyyigGzodYwHtCxZyUQe9JiBMHyRWXArAaKv">>,
            <<"did:pkh:solana:5eykt4UsFv8P8NJdTREpY1vzqKqZKvdp:7S3P4HxJpyyigGzodYwHtCxZyUQe9JiBMHyRWXArAaKv">>}
    ]].

malformed_subscription_identities_do_not_throw_vector_test() ->
    Opts = opts(),
    Listener = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    lists:foreach(fun({Action, Target, From}) ->
        ?assertMatch({ok, _}, call(<<"subscribe">>, base(),
            subscription_req(Action, Target, From, 1), Opts)),
        ?assertMatch({ok, _}, call(<<"unsubscribe">>, base(),
            subscription_req(Action, Target, From, 1), Opts))
    end, [
        {<<"ping">>, <<255>>, Listener},
        {<<"ping">>, Listener, <<255>>},
        {<<255>>, Listener, Listener}
    ]),
    {ok, State} = call(<<"subscribe">>, base(),
        subscription_req(<<"ping">>, Listener, Listener, 1), Opts),
    ?assertEqual({ok, [hb_committer:uri(Listener, Opts)]}, call(<<"subscribers">>, State,
        #{ <<"action">> => <<"ping">>, <<"target">> => Listener }, Opts)).

malformed_forwarded_key_does_not_throw_vector_test() ->
    Opts = opts(),
    Target = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    Res = call(<<"send">>, base(), #{
        <<"messages">> => #{ <<"action">> => <<"ping">>, <<"target">> => Target },
        <<"x-ok">> => <<"1">>,
        <<16#C3, "x">> => <<"2">>
    }, Opts),
    ?assertMatch({ok, _}, Res).
