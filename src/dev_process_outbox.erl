%%% @doc A small utility library for working with process outboxes.
-module(dev_process_outbox).
-include("include/hb.hrl").
-export([send/3, forwarded_keys/2, notify/3]).
-export([subscribe/3, unsubscribe/3, subscribers/3, subscribers/4]).
-export([send_subscription_request/4, send_subscription_request/5]).

%% @doc Add a message or list of messages to the process's outbox, notifying
%% subscribers to the action and target of the message, as appropriate.
%% Additionally, `x-` prefixed keys are forwarded in the outbound messages.
send(Msgs, Base, Opts) ->
    send(Msgs, Base, #{}, Opts).
send(Msg, Base, Req, Opts) when not is_list(Msg) ->
    send([Msg], Base, Req, Opts);
send(Msgs, Base, Req, Opts) ->
    ForwardedKeys = forwarded_keys(Req, Opts),
    lists:foldl(
        fun(XMsg, AccState) ->
            XMsgWithForwardedKeys = hb_ao:set(XMsg, ForwardedKeys, Opts),
            StateWithInitialSend = raw_send(XMsgWithForwardedKeys, AccState, Opts),
            notify(XMsgWithForwardedKeys, StateWithInitialSend, Opts)
        end,
        Base,
        Msgs
    ).

%% @doc Helper function to only add exactly one message to the process's outbox.
%% Does not notify subscribers.
raw_send(Msg, State, Opts) ->
    hb_ao:set(
        State,
        <<"results/outbox">>,
        [
            Msg
        |
            hb_util:message_to_ordered_list(
                hb_ao:get(<<"results/outbox">>, State, [], Opts),
                Opts
            )
        ],
        Opts
    ).

%% @doc Notify all subscribers to the action and target of the message. Does not
%% send a message to the `target' themselves (if set). If no `target' is provided, 
%% those that subscribed to the `broadcast' `subscribe-target' are notified.
notify(Msg, Base, Opts) ->
    maybe
        ?event(debug_subscriptions,
            {notifying_subscribers,
                {msg, Msg},
                {base, Base}
            },
            Opts
        ),
        {ok, Action} ?= hb_maps:find(<<"action">>, Msg, <<"action">>, Opts),
        Target = hb_maps:get(<<"target">>, Msg, <<"broadcast">>, Opts),
        Subscribers = subscribers(Base, Action, Target, Opts),
        ?event(debug_subscriptions,
            {notifying_subscribers,
                {action, Action},
                {target, Target},
                {subscribers, Subscribers}
            },
            Opts
        ),
        lists:foldl(
            fun(Listener, StateAcc) ->
                MsgWithNewTarget =
                    hb_ao:set(
                        forward_keys(Msg, Opts),
                        #{
                            <<"target">> => hb_ao:set(Msg, <<"target">>, Listener, Opts),
                            <<"action">> => <<"notify">>
                        },
                        Opts
                    ),
                ?event(debug_subscriptions,
                    {notifying_subscriber,
                        {listener, Listener},
                        {msg, MsgWithNewTarget}
                    },
                    Opts
                ),
                raw_send(
                    MsgWithNewTarget,
                    StateAcc,
                    Opts
                )
            end,
            Base,
            Subscribers
        )
    else
        {error, Missing} ->
            ?event(debug_subscriptions, {ignoring_message, {missing, Missing}}),
            Base
    end.

%% @doc Unsubscribe to a subject and target from a request.
unsubscribe(State, Req, Opts) ->
    manage_subscription(State, Req, unset, Opts).

%% @doc Subscribe to a subject and target, storing the slot of the request
%% (if available) or the message ID as the subscription info.
subscribe(State, Req, Opts) ->
    manage_subscription(
        State,
        Req,
        hb_ao:get(
            <<"slot">>,
            Req,
            hb_message:id(Req, signed, Opts),
            Opts
        ),
        Opts
    ).

%% @doc Helper function to manage subscriptions to a subject and target. If no
%% `subscribe-target' is provided, the `broadcast' target is used. Any message
%% sent with `notify/3' without a `target' will be sent to such subscribers.
manage_subscription(State, Req, SubscriptionInfo, Opts) ->
    maybe
        Msg = hb_ao:get(<<"body">>, Req, Opts),
        {ok, Action} ?=
            hb_maps:find(
                <<"subscribe-action">>,
                Msg,
                <<"No `subscribe-action' key to filter upon provided.">>,
                Opts
            ),
        {ok, Subject} ?=
            hb_maps:find(
                <<"subscribe-target">>,
                Msg,
                <<"broadcast">>,
                Opts
            ),
        {ok, Listener} ?=
            hb_maps:find(
                <<"from">>,
                Msg,
                <<"No security-normalized `from' key found in request.">>,
                Opts
            ),
        ProcessID = dev_process_lib:process_id(State, Opts),
        ?event(
            subscriptions_short,
            {set_subscription_info,
                {process_id, ProcessID},
                {action, Action},
                {subject, Subject},
                {listener, Listener},
                {info, SubscriptionInfo}
            },
            Opts
        ),
        NewState =
            hb_ao:set(
                State,
                #{
                    <<"subscribers">> => #{
                        Action => #{
                            Subject => #{
                                Listener => SubscriptionInfo
                            }
                        }
                    }
                },
                Opts
            ),
        ?event(
            debug_subscriptions,
            {setting_subscription,
                {process_id, ProcessID},
                {action, Action},
                {subject, Subject},
                {listener, Listener},
                {info, SubscriptionInfo},
                {request, Req},
                {new_state, NewState}
            },
            Opts
        ),
        {ok, NewState}
    else
        {error, Reason} ->
            ?event(
                debug_subscriptions,
                {error_setting_subscription,
                    {process_id, dev_process_lib:process_id(State, Opts)},
                    {state, State},
                    {request, Req},
                    {reason, Reason}
                },
                Opts
            ),
            {error, Reason}
    end.

%% @doc List all subscribers to a given subject and action.
subscribers(State, Action, Opts) ->
    subscribers(State, Action, <<"broadcast">>, Opts).
subscribers(State, Action, Target, Opts) ->
    hb_ao:get(
        <<
            "subscribers/",
            (hb_util:bin(Action))/binary,
            "/",
            (hb_util:bin(Target))/binary,
            "/keys">>,
        State,
        [],
        Opts
    ).

send_subscription_request(TargetProcess, Action, State, Opts) ->
    send_subscription_request(TargetProcess, Action, <<"broadcast">>, State, Opts).
send_subscription_request(TargetProcess, Action, Target, State, Opts) ->
    send(
        #{
            <<"target">> => TargetProcess,
            <<"action">> => <<"subscribe">>,
            <<"subscribe-action">> => Action,
            <<"subscribe-target">> => Target
        },
        State,
        Opts
    ).

%% @doc Extract the original keys from a forwarded request and return them
%% without their `x-' prefixes.
original_from_forwarded(Req, Opts) ->
    maps:from_list(
        lists:map(
            fun({<<"x-", Key/binary>>, Value}) -> {Key, Value} end,
            hb_maps:to_list(forwarded_keys(Req, Opts))
        )
    ).

%% @doc Extract keys with X- prefix for forwarding in notices
%% Follows AO token pattern: keys beginning with "X-" are forwarded.
forwarded_keys(Req, Opts) ->
    hb_maps:filter(
        fun(Key, _Value) ->
            case hb_util:to_lower(hb_util:bin(Key)) of
                <<"x-", _Rest/binary>> -> true;
                _ -> false
            end
        end,
        Req,
        Opts
    ).

%% @doc Uncommit a message and transform all keys into their `x-` forwarded form.
forward_keys(Msg, Opts) ->
    hb_maps:map(
        fun(Key, Value) ->
            {<<"x-", (hb_ao:normalize_key(Key))/binary>>, Value}
        end,
        hb_private:reset(hb_message:uncommitted(Msg, Opts)),
        Opts
    ).