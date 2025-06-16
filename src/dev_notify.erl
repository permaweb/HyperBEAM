%%% @doc A device that provides real-time notifications for AO process events.
%%% It integrates with the existing event system (hb_event) and allows clients
%%% to subscribe to specific events using HTTP/3 streams.
%%% 
%%% Uses a long-running notification manager process to handle high-frequency
%%% message matching and dispatching efficiently.
-module(dev_notify).
-export([info/1, info/3, dispatch/3, register/3, unregister/3, stream/3]).
-export([start_notification_manager/0, stop_notification_manager/0]).
-export([notification_manager_loop/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(NOTIFICATION_MANAGER, hb_notification_manager).

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

%% @doc Dispatch an event to registered listeners via the notification manager
dispatch(_StateMsg, InputMsg, Opts) ->
    ?event(notify, {dispatch_event, InputMsg}),
    
    % Send event to the long-running notification manager process
    case whereis(?NOTIFICATION_MANAGER) of
        undefined ->
            ?event(notify, {manager_not_started, starting_manager}),
            start_notification_manager(),
            dispatch(_StateMsg, InputMsg, Opts);
        ManagerPid ->
            ManagerPid ! {dispatch_event, InputMsg, Opts},
            {ok, <<"OK">>}
    end.

%% Note: dispatch_to_listeners and push_event functions are now handled
%% by the notification manager process for better performance.

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
register_stream(StreamId, Pattern, _Req, _Opts) ->
    % Ensure notification manager is running
    start_notification_manager(),
    
    % Register with the notification manager
    case whereis(?NOTIFICATION_MANAGER) of
        undefined ->
            ?event(notify, {manager_not_available, {stream_id, StreamId}});
        ManagerPid ->
            StreamPid = self(),
            ManagerPid ! {register_listener, Pattern, StreamPid, StreamId},
            ?event(notify, {stream_registered_with_manager, {stream_id, StreamId}, {pattern, Pattern}})
    end.

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
    % Unregister with the notification manager
    case whereis(?NOTIFICATION_MANAGER) of
        undefined ->
            ?event(notify, {manager_not_available_for_unregister, {stream_id, StreamId}});
        ManagerPid ->
            ManagerPid ! {unregister_listener, StreamId},
            ?event(notify, {stream_unregistered_from_manager, {stream_id, StreamId}})
    end.

%% ============================================================================
%% Notification Manager Process
%% ============================================================================

%% @doc Start the long-running notification manager process
start_notification_manager() ->
    case whereis(?NOTIFICATION_MANAGER) of
        undefined ->
            % Initialize ETS table for fast listener lookups
            case ets:info(notification_listeners) of
                undefined ->
                    ets:new(notification_listeners, [named_table, public, bag, {read_concurrency, true}]);
                _ -> ok
            end,
            
            % Start the manager process
            ManagerPid = spawn_link(fun() -> 
                register(?NOTIFICATION_MANAGER, self()),
                ?event(notify, {notification_manager_started, self()}),
                notification_manager_loop(#{})
            end),
            {ok, ManagerPid};
        Pid ->
            {already_started, Pid}
    end.

%% @doc Stop the notification manager process
stop_notification_manager() ->
    case whereis(?NOTIFICATION_MANAGER) of
        undefined -> ok;
        Pid ->
            Pid ! stop,
            % Clean up ETS table
            case ets:info(notification_listeners) of
                undefined -> ok;
                _ -> ets:delete(notification_listeners)
            end,
            ok
    end.

%% @doc Main loop for the notification manager process
notification_manager_loop(State) ->
    receive
        {dispatch_event, Event, Opts} ->
            % Handle event dispatch in the main manager process
            % This keeps the work minimal to avoid blocking
            handle_event_dispatch(Event, Opts),
            notification_manager_loop(State);
            
        {register_listener, Pattern, StreamPid, Ref} ->
            % Register a new listener
            ets:insert(notification_listeners, {Pattern, StreamPid, Ref}),
            ?event(notify, {listener_registered, {pattern, Pattern}, {stream, StreamPid}, {ref, Ref}}),
            notification_manager_loop(State);
            
        {unregister_listener, Ref} ->
            % Remove listener by reference
            ets:match_delete(notification_listeners, {'_', '_', Ref}),
            ?event(notify, {listener_unregistered, {ref, Ref}}),
            notification_manager_loop(State);
            
        {get_stats} ->
            % Return statistics about registered listeners
            Stats = #{
                total_listeners => ets:info(notification_listeners, size),
                manager_pid => self(),
                state => State
            },
            ?event(notify, {manager_stats, Stats}),
            notification_manager_loop(State);
            
        stop ->
            ?event(notify, {notification_manager_stopping, self()}),
            ok;
            
        Msg ->
            ?event(notify, {unexpected_message, Msg}),
            notification_manager_loop(State)
    end.

%% @doc Handle event dispatch by matching against registered listeners
handle_event_dispatch(Event, Opts) ->
    % Get all registered listeners
    AllListeners = ets:tab2list(notification_listeners),
    
    % Spawn short-lived worker processes for actual dispatching
    % This keeps the manager process responsive
    lists:foreach(fun({Pattern, StreamPid, Ref}) ->
        spawn(fun() -> 
            try_dispatch_to_listener(Pattern, StreamPid, Ref, Event, Opts)
        end)
    end, AllListeners).

%% @doc Try to dispatch an event to a specific listener if pattern matches
try_dispatch_to_listener(Pattern, StreamPid, Ref, Event, _Opts) ->
    try
        case hb_message:match(Event, Pattern) of
            true ->
                ?event(notify, {matched_pattern, {pattern, Pattern}, {ref, Ref}}),
                % Send event to the stream process
                case is_process_alive(StreamPid) of
                    true ->
                        StreamPid ! {notify_event, Event},
                        ?event(notify, {event_sent, {stream, StreamPid}, {ref, Ref}});
                    false ->
                        % Clean up dead process
                        ets:match_delete(notification_listeners, {'_', StreamPid, '_'}),
                        ?event(notify, {dead_stream_cleaned, {stream, StreamPid}})
                end;
            false ->
                % Pattern didn't match, no action needed
                ok
        end
    catch
        Class:Reason ->
            ?event(notify, {dispatch_error, {class, Class}, {reason, Reason}, {pattern, Pattern}})
    end. 