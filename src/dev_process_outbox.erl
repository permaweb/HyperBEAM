%%% @doc A small utility library for working with process outboxes.
-module(dev_process_outbox).
-include("include/hb.hrl").
-export([send/3, forwarded_keys/2, notify/3]).
-export([subscribe/3, unsubscribe/3, subscribers/4]).

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
            {ok, Next} = notify(XMsgWithForwardedKeys, StateWithInitialSend, Opts),
            Next
        end,
        Base,
        Msgs
    ).

%% @doc Helper function to only add exactly one message to the process's outbox.
%% Does not notify subscribers.
raw_send(Msg, State, Opts) ->
    CurrentOutbox = hb_ao:get(<<"results/outbox">>, State, [], Opts),
    NewOutbox = hb_util:message_to_ordered_list(CurrentOutbox, Opts) ++ [Msg],
    hb_ao:set(State, <<"results/outbox">>, NewOutbox, Opts).

%% @doc Notify all subscribers to the action and target of the message. Does not
%% send a message to the `target' themselves (if set). If no `target' is provided, 
%% those that subscribed to the `broadcast' `subscribe-target' are notified.
notify(Msg, Base, Opts) ->
    maybe
        {ok, Action} ?= hb_maps:find(<<"action">>, Msg, <<"action">>, Opts),
        Target = hb_maps:get(<<"target">>, Msg, <<"broadcast">>, Opts),
        lists:foldl(
            fun(Listener, StateAcc) ->
                raw_send(
                    hb_ao:set(Msg, <<"target">>, Listener, Opts),
                    StateAcc,
                    Opts
                )
            end,
            Base,
            subscribers(Base, Action, Target, Opts)
        )
    else
        {error, Missing} ->
            ?event(debug_subscriptions, {ignoring_message, {missing, Missing}}),
            Base
    end.

%% @doc Unsubscribe to a subject and target from a request.
unsubscribe(State, Req, Opts) ->
    manage_subscription(State, Req, unset, Opts).

%% @doc Subscribe to a subject and target from a request.
subscribe(State, Req, Opts) ->
    manage_subscription(State, Req, hb_message:id(Req, signed, Opts), Opts).

%% @doc Helper function to manage subscriptions to a subject and target. If no
%% `subscribe-target' is provided, the `broadcast' target is used. Any message
%% sent with `notify/3' without a `target' will be sent to such subscribers.
manage_subscription(State, Req, SubscriptionInfo, Opts) ->
    maybe
        {ok, Action} ?=
            hb_maps:find(
                <<"subscribe-action">>,
                Req,
                <<"No `subscribed-action' key to filter upon provided.">>,
                Opts
            ),
        Subject =
            hb_maps:get(
                <<"subscribe-target">>,
                Req,
                <<"broadcast">>,
                Opts
            ),
        [Listener] ?= hb_message:signers(Req, Opts),
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
        )
    end.

%% @doc List all subscribers to a given subject and action.
subscribers(State, Action, Target, Opts) ->
    hb_ao:get(<<"subscribers/", Action, "/", Target, "/keys">>, State, [], Opts).

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