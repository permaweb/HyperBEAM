%%% @doc AO-Core process outbox and subscription helper device.
%%%
%%% `process-outbox@1.0' gives an AO process a small, deterministic outbox
%%% surface. It appends outbound messages to `results/outbox`, records listener
%%% subscriptions under action/target pairs, and can fan out notification
%%% messages to those listeners without requiring changes to HyperBEAM core
%%% process devices.
%%%
%%% The device keeps all state in the message it is resolving:
%%%
%%% ```
%%% results/outbox:
%%%     Newest-first list of outbound messages. `send' prepends each submitted
%%%     message, and subscriber notifications are also prepended as ordinary
%%%     outbox entries.
%%%
%%% subscribers:
%%%     Nested message of action -> target -> listener -> subscription-info.
%%%     A target is `broadcast', the default, or the host-form URI of a
%%%     committer, as a listener is. `subscription-info' is the request's
%%%     `slot' when present, otherwise the signed ID of the subscription
%%%     request.
%%% ```
%%%
%%% The public AO paths are:
%%%
%%% ```
%%% send:
%%%     messages?: Message | [Message]
%%%     body?:     Message | [Message]  Used when `messages' is absent.
%%%     x-*?:      Any request keys with the `x-' prefix are copied onto each
%%%                sent message before it is written.
%%%
%%%     Appends the message or messages to `results/outbox'. If a sent message
%%%     has an `action', subscribers for that action and its `target' receive
%%%     `notify' messages. If no target is set, `broadcast' is used.
%%%
%%% notify:
%%%     messages?: Message
%%%     body?:     Message              Used when `messages' is absent.
%%%
%%%     Emits subscriber notifications for the message without first appending
%%%     the original message. A message without `action' produces no notices.
%%%
%%% subscribe:
%%%     body:
%%%         subscribe-action: Action
%%%         subscribe-target?: Target   Defaults to `broadcast'.
%%%         from: Listener              Expected to be security-normalized by
%%%                                     the caller/package before subscription.
%%%     slot?: SubscriptionInfo
%%%
%%%     Registers `Listener' for `Action' and `Target'.
%%%
%%% unsubscribe:
%%%     body:
%%%         subscribe-action: Action
%%%         subscribe-target?: Target   Defaults to `broadcast'.
%%%         from: Listener
%%%
%%%     Removes `Listener' from the matching subscription bucket.
%%%
%%% subscribers:
%%%     action:  Action
%%%     target?: Target                 Defaults to `broadcast'.
%%%
%%%     Returns the list of listeners currently registered for the pair.
%%% ```
%%%
%%% Notification messages are written to `results/outbox' as:
%%%
%%% ```
%%% action: notify
%%% target: Listener
%%% x-*:    The uncommitted keys of the source message, normalized and prefixed
%%%         with `x-'. For example, `action = "Debit-Notice"' becomes
%%%         `x-action = "Debit-Notice"'.
%%% ```
%%%
%%% Missing `send' or `notify' payloads return
%%% `<<"Missing outbox request key.">>'. Missing subscription fields return the
%%% explicit error messages used by the package test vectors.
-module(dev_process_outbox).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-implements(<<"process-outbox@1.0">>).

%%% Public device API.
-export([device_info/0, send/3, subscribe/3, unsubscribe/3, notify/3, subscribers/3]).
%%% Erlang helper API for tests and package-local callers.
-export([forwarded_keys/2, original_from_forwarded/2]).

device_info() ->
    #{
        exports =>
            [
                <<"send">>,
                <<"subscribe">>,
                <<"unsubscribe">>,
                <<"notify">>,
                <<"subscribers">>
            ]
    }.

%% @doc Append one or more messages to `results/outbox`.
send(State, Req, Opts) ->
    maybe
        {ok, Msgs} ?= request_messages(Req, Opts),
        {ok, send_messages(Msgs, State, Req, Opts)}
    end.

%% @doc Notify subscribers for a message without first sending the message itself.
notify(State, Req, Opts) ->
    maybe
        {ok, Msg} ?= request_message(Req, Opts),
        {ok, notify_message(Msg, State, Opts)}
    end.

%% @doc Subscribe to a subject and target from a request body.
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

%% @doc Unsubscribe from a subject and target from a request body.
unsubscribe(State, Req, Opts) ->
    manage_subscription(State, Req, unset, Opts).

%% @doc List subscribers for `action` and optional `target`.
subscribers(State, Req, Opts) ->
    maybe
        {ok, Action} ?= request_value(<<"action">>, Req, Opts),
        Target = request_value(<<"target">>, Req, <<"broadcast">>, Opts),
        {ok, subscribers_for(State, Action, Target, Opts)}
    end.

request_messages(Req, Opts) ->
    case request_message(Req, Opts) of
        {ok, Msgs} when is_list(Msgs) -> {ok, Msgs};
        {ok, Msg} -> {ok, [Msg]};
        Error -> Error
    end.

request_message(Req, Opts) ->
    request_value(<<"messages">>, Req, Opts).

request_value(Key, Req, Opts) ->
    case hb_maps:find(Key, Req, Opts) of
        {ok, Value} ->
            {ok, Value};
        error ->
            case hb_maps:find(<<"body">>, Req, Opts) of
                {ok, Body} when Key =:= <<"messages">> ->
                    {ok, Body};
                {ok, Body} when is_map(Body) ->
                    find_or_error(Key, Body, <<"Missing outbox request key.">>, Opts);
                _ ->
                    {error, <<"Missing outbox request key.">>}
            end
    end.

request_value(Key, Req, Default, Opts) ->
    case request_value(Key, Req, Opts) of
        {ok, Value} -> Value;
        _ -> Default
    end.

%% @doc Add messages to a process outbox, notifying subscribers as appropriate.
send_messages(Msg, State, Req, Opts) when not is_list(Msg) ->
    send_messages([Msg], State, Req, Opts);
send_messages(Msgs, State, Req, Opts) ->
    ForwardedKeys = forwarded_keys(Req, Opts),
    lists:foldl(
        fun(Msg, AccState) ->
            MsgWithForwardedKeys = hb_ao:set(Msg, ForwardedKeys, Opts),
            StateWithInitialSend = raw_send(MsgWithForwardedKeys, AccState, Opts),
            notify_message(MsgWithForwardedKeys, StateWithInitialSend, Opts)
        end,
        State,
        Msgs
    ).

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

notify_message(Msg, State, Opts) ->
    maybe
        {ok, Action} ?= find_or_error(<<"action">>, Msg, <<"action">>, Opts),
        Target = hb_maps:get(<<"target">>, Msg, <<"broadcast">>, Opts),
        Subscribers = subscribers_for(State, Action, Target, Opts),
        lists:foldl(
            fun(Listener, StateAcc) ->
                raw_send(
                    hb_ao:set(
                        forward_keys(Msg, Opts),
                        #{
                            <<"target">> => Listener,
                            <<"action">> => <<"notify">>
                        },
                        Opts
                    ),
                    StateAcc,
                    Opts
                )
            end,
            State,
            Subscribers
        )
    else
        {error, _Missing} ->
            State
    end.

manage_subscription(State, Req, SubscriptionInfo, Opts) ->
    maybe
        Msg = hb_ao:get(<<"body">>, Req, Opts),
        {ok, Action} ?=
            find_or_error(
                <<"subscribe-action">>,
                Msg,
                <<"No `subscribe-action` key to filter upon provided.">>,
                Opts
            ),
        true ?= is_binary(Action)
            orelse {error, <<"`subscribe-action` must be a binary.">>},
        {ok, Target} ?=
            subscription_target(
                hb_maps:get(<<"subscribe-target">>, Msg, <<"broadcast">>, Opts)
            ),
        {ok, From} ?=
            find_or_error(
                <<"from">>,
                Msg,
                <<"No security-normalized `from` key found in request.">>,
                Opts
            ),
        {ok, Listener} ?= hb_committer:validated(From),
        NewState =
            set_subscription(
                State,
                Action,
                Target,
                Listener,
                SubscriptionInfo,
                Opts
            ),
        {ok, NewState}
    else
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Return the target of a subscription: `broadcast', or the host-form URI
%% of a committer.
subscription_target(<<"broadcast">>) -> {ok, <<"broadcast">>};
subscription_target(Target) -> hb_committer:validated(Target).

set_subscription(State, Action, Target, Listener, unset, Opts) ->
    ActionBin = hb_util:lowercase_key(hb_util:bin(Action)),
    Subscribers = subscription_map(State, Opts),
    ActionSubscribers = hb_maps:get(ActionBin, Subscribers, #{}, Opts),
    TargetSubscribers = hb_maps:get(Target, ActionSubscribers, #{}, Opts),
    NewTargetSubscribers = maps:remove(Listener, hb_private:reset(TargetSubscribers)),
    NewActionSubscribers =
        maybe_remove_empty(Target, NewTargetSubscribers, ActionSubscribers),
    NewSubscribers =
        maybe_remove_empty(ActionBin, NewActionSubscribers, Subscribers),
    write_subscribers(State, NewSubscribers, Opts);
set_subscription(State, Action, Target, Listener, SubscriptionInfo, Opts) ->
    ActionBin = hb_util:lowercase_key(hb_util:bin(Action)),
    Subscribers = subscription_map(State, Opts),
    ActionSubscribers = hb_maps:get(ActionBin, Subscribers, #{}, Opts),
    TargetSubscribers = hb_maps:get(Target, ActionSubscribers, #{}, Opts),
    NewTargetSubscribers =
        (hb_private:reset(TargetSubscribers))#{ Listener => SubscriptionInfo },
    NewActionSubscribers = ActionSubscribers#{ Target => NewTargetSubscribers },
    NewSubscribers = Subscribers#{ ActionBin => NewActionSubscribers },
    write_subscribers(State, NewSubscribers, Opts).

subscription_map(State, Opts) ->
    case hb_maps:get(<<"subscribers">>, State, #{}, Opts) of
        Map when is_map(Map) -> hb_private:reset(Map);
        _ -> #{}
    end.

maybe_remove_empty(Key, Value, Map) when map_size(Value) =:= 0 ->
    maps:remove(Key, hb_private:reset(Map));
maybe_remove_empty(Key, Value, Map) ->
    (hb_private:reset(Map))#{ Key => Value }.

write_subscribers(State, Subscribers, _Opts) when map_size(Subscribers) =:= 0 ->
    maps:remove(<<"subscribers">>, State);
write_subscribers(State, Subscribers, Opts) ->
    hb_ao:set(State, <<"subscribers">>, Subscribers, Opts).

subscribers_for(State, Action, Target, Opts) ->
    ActionBin = hb_util:lowercase_key(hb_util:bin(Action)),
    TargetBin = hb_committer:uri(Target),
    Subscribers = subscription_map(State, Opts),
    ActionSubscribers = hb_maps:get(ActionBin, Subscribers, #{}, Opts),
    TargetSubscribers = hb_maps:get(TargetBin, ActionSubscribers, #{}, Opts),
    case TargetSubscribers of
        Map when is_map(Map) ->
            hb_maps:keys(hb_private:reset(Map), Opts);
        List when is_list(List) ->
            List;
        _ ->
            hb_ao:get(
                <<
                    "subscribers/",
                    ActionBin/binary,
                    "/",
                    TargetBin/binary,
                    "/keys">>,
                State,
                [],
                Opts
            )
    end.

find_or_error(Key, Map, ErrorTerm, Opts) ->
    case hb_maps:find(Key, Map, Opts) of
        {ok, Value} -> {ok, Value};
        error -> {error, ErrorTerm}
    end.

%% @doc Extract the original keys from a forwarded request.
original_from_forwarded(Req, Opts) ->
    maps:from_list(
        lists:map(
            fun({<<"x-", Key/binary>>, Value}) -> {Key, Value} end,
            hb_maps:to_list(forwarded_keys(Req, Opts), Opts)
        )
    ).

%% @doc Extract request keys with the `x-` prefix for forwarding in notices.
forwarded_keys(Req, Opts) ->
    with_prefix([<<"x-">>], Req, Opts).

with_prefix(Prefixes, Map, Opts) when is_list(Prefixes) ->
    PrefixBins = [hb_util:to_lower(hb_util:bin(P)) || P <- Prefixes],
    hb_maps:filter(
        fun(Key, _Value) ->
            KeyBin = hb_util:to_lower(hb_util:bin(Key)),
            lists:any(
                fun(Prefix) ->
                    binary:match(KeyBin, Prefix) =:= {0, byte_size(Prefix)}
                end,
                PrefixBins
            )
        end,
        Map,
        Opts
    );
with_prefix(Prefix, Map, Opts) ->
    with_prefix([Prefix], Map, Opts).

forward_keys(Msg, Opts) ->
    hb_maps:from_list(
        [
            {<<"x-", (hb_ao:normalize_key(Key))/binary>>, Value}
        ||
            {Key, Value} <-
                hb_maps:to_list(
                    hb_private:reset(
                        hb_message:uncommitted(Msg, Opts)
                    ),
                    Opts
                )
        ]
    ).

send_appends_message_test() ->
    Req =
        #{
            <<"messages">> =>
                #{
                    <<"action">> => <<"notice">>,
                    <<"target">> => <<"alice">>
                },
            <<"x-trace">> => <<"1">>
        },
    {ok, State} = send(#{}, Req, #{}),
    [Notice] = hb_util:message_to_ordered_list(
        hb_ao:get(<<"results/outbox">>, State, [], #{}),
        #{}
    ),
    ?assertEqual(<<"notice">>, hb_ao:get(<<"action">>, Notice, #{})),
    ?assertEqual(<<"alice">>, hb_ao:get(<<"target">>, Notice, #{})),
    ?assertEqual(<<"1">>, hb_ao:get(<<"x-trace">>, Notice, #{})).

subscribe_send_unsubscribe_test() ->
    Alice = hb_util:human_id(crypto:hash(sha256, <<"alice">>)),
    Listener = hb_util:human_id(crypto:hash(sha256, <<"listener">>)),
    ListenerURI = hb_committer:uri(Listener),
    SubscribeReq =
        #{
            <<"slot">> => 7,
            <<"body">> =>
                #{
                    <<"subscribe-action">> => <<"Debit-Notice">>,
                    <<"subscribe-target">> => Alice,
                    <<"from">> => Listener
                }
        },
    {ok, Subscribed} = subscribe(#{}, SubscribeReq, #{}),
    ?assertEqual(
        [ListenerURI],
        subscribers_for(Subscribed, <<"Debit-Notice">>, Alice, #{})
    ),
    SendReq =
        #{
            <<"messages">> =>
                #{
                    <<"action">> => <<"Debit-Notice">>,
                    <<"target">> => Alice,
                    <<"quantity">> => 1
                }
        },
    {ok, WithNotices} = send(Subscribed, SendReq, #{}),
    Outbox =
        hb_util:message_to_ordered_list(
            hb_ao:get(<<"results/outbox">>, WithNotices, [], #{}),
            #{}
        ),
    ?assertEqual(2, length(Outbox)),
    ?assert(lists:any(
        fun(Msg) ->
            hb_ao:get(<<"action">>, Msg, #{}) =:= <<"notify">>
                andalso hb_ao:get(<<"target">>, Msg, #{}) =:= ListenerURI
        end,
        Outbox
    )),
    {ok, Unsubscribed} = unsubscribe(Subscribed, SubscribeReq, #{}),
    ?assertEqual(
        [],
        subscribers_for(Unsubscribed, <<"Debit-Notice">>, Alice, #{})
    ).
