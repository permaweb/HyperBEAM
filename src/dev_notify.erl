%%% @doc A device that provides real-time notifications for AO process events.
%%% It integrates with the existing event system (hb_event) and allows clients
%%% to subscribe to specific events using HTTP/3 streams.
-module(dev_notify).
-export([info/1, info/3, dispatch/3, register/3, unregister/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Device API information
info(_) ->
    #{
        exports => [info, dispatch, register, unregister],
        variant => <<"Notify/1.0">>
    }.

%% @doc HTTP info response providing information about this device
info(_Msg1, _Msg2, _Opts) ->
    InfoBody = #{
        <<"description">> => <<"Notification device for real-time event streaming">>,
        <<"version">> => <<"1.0">>,
        <<"paths">> => #{
            <<"info">> => <<"Get device info">>,
            <<"dispatch">> => <<"Dispatch an event to registered listeners">>,
            <<"register">> => <<"Register a new event listener">>,
            <<"unregister">> => <<"Unregister an event listener">>
        }
    },
    {ok, #{<<"status">> => 200, <<"body">> => InfoBody}}.

%% @doc Register a new event listener
register(StateMsg, InputMsg, Opts) ->
    ?event(notify, {register_listener, InputMsg}),
    case hb_ao:get(<<"pattern">>, InputMsg, Opts) of
        not_found ->
            {error, <<"No pattern specified for event registration">>};
        Pattern ->
            case hb_ao:get(<<"stream">>, InputMsg, Opts) of
                not_found ->
                    {error, <<"No stream specified for event registration">>};
                Stream ->
                    % Register the pattern and stream in the state
                    NewState = maps:put(
                        {pattern, Pattern},
                        Stream,
                        maps:get(<<"listeners">>, StateMsg, #{})
                    ),
                    {ok, StateMsg#{<<"listeners">> => NewState}}
            end
    end.

%% @doc Unregister an event listener
unregister(StateMsg, InputMsg, Opts) ->
    ?event(notify, {unregister_listener, InputMsg}),
    case hb_ao:get(<<"pattern">>, InputMsg, Opts) of
        not_found ->
            {error, <<"No pattern specified for event unregistration">>};
        Pattern ->
            % Remove the pattern from the state
            Listeners = maps:get(<<"listeners">>, StateMsg, #{}),
            NewState = maps:remove({pattern, Pattern}, Listeners),
            {ok, StateMsg#{<<"listeners">> => NewState}}
    end.

%% @doc Dispatch an event to registered listeners
dispatch(StateMsg, InputMsg, Opts) ->
    ?event(notify, {dispatch_event, InputMsg}),
    Listeners = maps:get(<<"listeners">>, StateMsg, #{}),
    
    % Spawn a worker process for non-blocking dispatch
    spawn(fun() ->
        dispatch_to_listeners(InputMsg, Listeners, Opts)
    end),
    
    {ok, <<"OK">>}.

%% @doc Internal function to dispatch events to matching listeners
dispatch_to_listeners(Event, Listeners, Opts) ->
    maps:fold(
        fun({pattern, Pattern}, Stream, _) ->
            case hb_message:match(Event, Pattern) of
                true ->
                    ?event(notify, {matched_pattern, Pattern, Event}),
                    % Use Cowboy's http/3 push to send the event
                    push_event(Stream, Event, Opts);
                false ->
                    ok
            end
        end,
        ok,
        Listeners
    ).

%% @doc Push an event to a stream using Cowboy's http/3 push
push_event(Stream, Event, Opts) ->
    try
        % Convert event to JSON
        EventJson = json:encode(Event),
        % Push the event to the stream
        cowboy_req:push(Stream, EventJson, Opts)
    catch
        Class:Reason ->
            ?event(notify, {push_error, {Class, Reason}}),
            ok
    end. 