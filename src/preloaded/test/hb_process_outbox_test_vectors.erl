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

%% @doc The address of a test account, and the host-form URI of its committer.
account(Name) -> hb_util:human_id(crypto:hash(sha256, Name)).
uri(Name) -> hb_committer:uri(account(Name)).

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
            subscription_req(
                <<"Ping">>, default, <<"did:ar:", (account(<<"listener-a">>))/binary>>, 11),
            Opts
        ),
    {ok, Subscribers} =
        call(
            <<"subscribers">>,
            Subscribed,
            #{ <<"action">> => <<"Ping">> },
            Opts
        ),
    ?assertEqual([uri(<<"listener-a">>)], Subscribers),
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
            {<<"target">>, uri(<<"listener-a">>)},
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
            subscription_req(<<"Debit-Notice">>, account(<<"alice">>), account(<<"listener-a">>), 21),
            Opts
        ),
    {ok, BothSubscribed} =
        call(
            <<"subscribe">>,
            AliceSubscribed,
            subscription_req(<<"Debit-Notice">>, account(<<"bob">>), account(<<"listener-b">>), 22),
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
                        <<"target">> => account(<<"alice">>),
                        <<"quantity">> => 5
                    }
            },
            Opts
        ),
    Notices = outbox(Updated, Opts),
    ?assert(has_message(
        [
            {<<"action">>, <<"notify">>},
            {<<"target">>, uri(<<"listener-a">>)},
            {<<"x-target">>, account(<<"alice">>)},
            {<<"x-quantity">>, 5}
        ],
        Notices,
        Opts
    )),
    ?assertNot(has_message(
        [
            {<<"action">>, <<"notify">>},
            {<<"target">>, uri(<<"listener-b">>)}
        ],
        Notices,
        Opts
    )).

unsubscribe_removes_listener_vector_test() ->
    Opts = opts(),
    Req = subscription_req(<<"Debit-Notice">>, account(<<"alice">>), account(<<"listener-a">>), 31),
    {ok, Subscribed} = call(<<"subscribe">>, base(), Req, Opts),
    {ok, Unsubscribed} = call(<<"unsubscribe">>, Subscribed, Req, Opts),
    {ok, Subscribers} =
        call(
            <<"subscribers">>,
            Unsubscribed,
            #{
                <<"action">> => <<"Debit-Notice">>,
                <<"target">> => account(<<"alice">>)
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
                        <<"target">> => account(<<"alice">>)
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
            subscription_req(<<"Audit">>, account(<<"alice">>), account(<<"listener-a">>), 41),
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
                        <<"target">> => account(<<"alice">>),
                        <<"quantity">> => 42
                    }
            },
            Opts
        ),
    [Notice] = outbox(Updated, Opts),
    ?assertEqual(<<"notify">>, hb_ao:get(<<"action">>, Notice, Opts)),
    ?assertEqual(uri(<<"listener-a">>), hb_ao:get(<<"target">>, Notice, Opts)),
    ?assertEqual(<<"Audit">>, hb_ao:get(<<"x-action">>, Notice, Opts)),
    ?assertEqual(account(<<"alice">>), hb_ao:get(<<"x-target">>, Notice, Opts)),
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
    ),
    ?assertEqual(
        {error, <<"Not the address or URI of a committer.">>},
        call(
            <<"subscribe">>,
            base(),
            subscription_req(<<"Ping">>, default, <<"listener-a">>, 1),
            Opts
        )
    ).
