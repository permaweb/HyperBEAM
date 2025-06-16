%%% @doc A device that provides real-time notifications for AO process events.
%%% It integrates with the existing event system (hb_event) and allows clients
%%% to subscribe to specific events using HTTP/3 streams.
-module(dev_notify).
-export([info/1, info/3, dispatch/3, register/3, unregister/3, stream/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Device API information
info(_) ->
    #{
        exports => [info, dispatch, register, unregister, stream],
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
            <<"unregister">> => <<"Unregister an event listener">>,
            <<"stream">> => <<"Start a streaming connection for real-time events">>
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

%% @doc Push an event to a stream using Cowboy's HTTP/3 streaming
push_event(StreamRef, Event, _Opts) ->
    try
        % Convert event to JSON
        EventJson = hb_util:encode(Event),
        % Stream the event data with nofin to keep connection alive
        cowboy_req:stream_body(EventJson, nofin, StreamRef),
        ?event(notify, {event_pushed, {stream, StreamRef}, {event_size, byte_size(EventJson)}})
    catch
        Class:Reason ->
            ?event(notify, {push_error, {Class, Reason}}),
            ok
    end.

%% @doc Start a streaming connection for real-time event notifications
stream(_StateMsg, InputMsg, Opts) ->
    ?event(notify, {start_stream, InputMsg}),
    
    % Extract pattern for filtering events
    Pattern = hb_ao:get(<<"pattern">>, InputMsg, <<"*">>, Opts),
    
    % Create streaming response headers
    Headers = #{
        <<"content-type">> => <<"application/json">>,
        <<"cache-control">> => <<"no-cache">>,
        <<"connection">> => <<"keep-alive">>,
        <<"access-control-allow-origin">> => <<"*">>
    },
    
    % Start the streaming response
    StreamingBody = create_streaming_body(Pattern, Opts),
    
    {ok, #{
        <<"status">> => 200,
        <<"headers">> => Headers,
        <<"body">> => StreamingBody,
        <<"streaming">> => true
    }}.

%% @doc Create a streaming body function that keeps the connection open
create_streaming_body(Pattern, Opts) ->
    fun(Req) ->
        % Initialize streaming response
        Req2 = cowboy_req:stream_reply(200, #{
            <<"content-type">> => <<"text/event-stream">>,
            <<"cache-control">> => <<"no-cache">>,
            <<"connection">> => <<"keep-alive">>,
            <<"access-control-allow-origin">> => <<"*">>
        }, Req),
        
        % Register this stream for notifications
        StreamId = make_ref(),
        register_stream(StreamId, Pattern, Req2, Opts),
        
        % Send initial connection message
        InitialMessage = hb_util:encode(#{
            <<"type">> => <<"connection">>,
            <<"message">> => <<"Connected to notification stream">>,
            <<"pattern">> => Pattern,
            <<"timestamp">> => erlang:system_time(millisecond)
        }),
        cowboy_req:stream_body([<<"data: ">>, InitialMessage, <<"\n\n">>], nofin, Req2),
        
        % Keep connection alive and handle cleanup
        stream_loop(StreamId, Req2, Opts),
        
        {ok, Req2}
    end.

%% @doc Register a streaming connection for receiving notifications
register_stream(StreamId, Pattern, Req, _Opts) ->
    % Store stream info in ETS table or process registry
    % For now, we'll use a simple approach with process dictionary
    Streams = get(notification_streams),
    NewStreams = case Streams of
        undefined -> #{StreamId => #{pattern => Pattern, req => Req}};
        _ -> Streams#{StreamId => #{pattern => Pattern, req => Req}}
    end,
    put(notification_streams, NewStreams),
    ?event(notify, {stream_registered, {stream_id, StreamId}, {pattern, Pattern}}).

%% @doc Keep the streaming connection alive and handle events
stream_loop(StreamId, Req, Opts) ->
    receive
        {notify_event, Event} ->
            % Send event to client
            EventData = hb_util:encode(Event),
            cowboy_req:stream_body([<<"data: ">>, EventData, <<"\n\n">>], nofin, Req),
            stream_loop(StreamId, Req, Opts);
        {close_stream, StreamId} ->
            % Close the stream
            unregister_stream(StreamId),
            cowboy_req:stream_body(<<"">>, fin, Req);
        _ ->
            stream_loop(StreamId, Req, Opts)
    after 30000 -> % 30 second keepalive
        % Send keepalive message
        KeepaliveMsg = hb_util:encode(#{
            <<"type">> => <<"keepalive">>,
            <<"timestamp">> => erlang:system_time(millisecond)
        }),
        cowboy_req:stream_body([<<"data: ">>, KeepaliveMsg, <<"\n\n">>], nofin, Req),
        stream_loop(StreamId, Req, Opts)
    end.

%% @doc Unregister a streaming connection
unregister_stream(StreamId) ->
    Streams = get(notification_streams),
    case Streams of
        undefined -> ok;
        _ -> 
            NewStreams = maps:remove(StreamId, Streams),
            put(notification_streams, NewStreams)
    end,
    ?event(notify, {stream_unregistered, {stream_id, StreamId}}). 