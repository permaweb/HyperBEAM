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

%% @doc Register a new event listener with template/spec support
register(StateMsg, InputMsg, Opts) ->
    ?event(notify, {register_listener, InputMsg}),
    
    % Extract template specification (supports both map and regex templates)
    TemplateResult = case hb_ao:get(<<"template">>, InputMsg, Opts) of
        not_found ->
            % Fallback to legacy pattern field for compatibility
            case hb_ao:get(<<"pattern">>, InputMsg, Opts) of
                not_found -> {error, <<"No template or pattern specified">>};
                Pattern -> {ok, Pattern}
            end;
        TemplateSpec -> {ok, TemplateSpec}
    end,
    
    case TemplateResult of
        {error, Reason} -> {error, Reason};
        {ok, ExtractedTemplate} ->
            case hb_ao:get(<<"stream">>, InputMsg, Opts) of
                not_found ->
                    {error, <<"No stream specified for event registration">>};
                Stream ->
                    % Validate template specification
                    case validate_template(ExtractedTemplate, Opts) of
                        {ok, ValidatedTemplate} ->
                            % Register the template and stream in the state
                            NewState = maps:put(
                                {template, ValidatedTemplate},
                                Stream,
                                maps:get(<<"listeners">>, StateMsg, #{})
                            ),
                            {ok, StateMsg#{<<"listeners">> => NewState}};
                        {error, ValidationError} ->
                            {error, ValidationError}
                    end
            end
    end.

%% @doc Unregister an event listener (supports both template and legacy pattern)
unregister(StateMsg, InputMsg, Opts) ->
    ?event(notify, {unregister_listener, InputMsg}),
    
    % Extract template or pattern for removal
    TemplateResult = case hb_ao:get(<<"template">>, InputMsg, Opts) of
        not_found ->
            case hb_ao:get(<<"pattern">>, InputMsg, Opts) of
                not_found -> {error, <<"No template or pattern specified">>};
                Pattern -> {ok, Pattern}
            end;
        TemplateSpec -> {ok, TemplateSpec}
    end,
    
    case TemplateResult of
        {error, Reason} -> {error, Reason};
        {ok, ExtractedTemplate} ->
            % Remove the template from the state
            Listeners = maps:get(<<"listeners">>, StateMsg, #{}),
            NewState = maps:remove({template, ExtractedTemplate}, Listeners),
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
    
    % Extract template for filtering events (supports both map and regex templates)
    Template = case hb_ao:get(<<"template">>, InputMsg, Opts) of
        not_found ->
            % Fallback to pattern field for legacy compatibility
            hb_ao:get(<<"pattern">>, InputMsg, <<".*">>, Opts); % Default regex matches all
        TemplateSpec -> TemplateSpec
    end,
    
    % Create streaming response headers
    Headers = #{
        <<"content-type">> => <<"application/json">>,
        <<"cache-control">> => <<"no-cache">>,
        <<"connection">> => <<"keep-alive">>,
        <<"access-control-allow-origin">> => <<"*">>
    },
    
    % Start the streaming response
    StreamingBody = create_streaming_body(Template, Opts),
    
    {ok, #{
        <<"status">> => 200,
        <<"headers">> => Headers,
        <<"body">> => StreamingBody,
        <<"streaming">> => true
    }}.

%% @doc Create a streaming body function that keeps the connection open
create_streaming_body(Template, Opts) ->
    fun(Req) ->
        % Initialize streaming response
        Req2 = cowboy_req:stream_reply(200, #{
            <<"content-type">> => <<"text/event-stream">>,
            <<"cache-control">> => <<"no-cache">>,
            <<"connection">> => <<"keep-alive">>,
            <<"access-control-allow-origin">> => <<"*">>
        }, Req),
        
        % Validate template before registering
        case validate_template(Template, Opts) of
            {ok, ValidatedTemplate} ->
                % Register this stream for notifications
                StreamId = make_ref(),
                register_stream(StreamId, ValidatedTemplate, Req2, Opts),
                
                % Send initial connection message
                InitialMessage = hb_util:encode(#{
                    <<"type">> => <<"connection">>,
                    <<"message">> => <<"Connected to notification stream">>,
                    <<"template">> => ValidatedTemplate,
                    <<"timestamp">> => erlang:system_time(millisecond)
                }),
                cowboy_req:stream_body([<<"data: ">>, InitialMessage, <<"\n\n">>], nofin, Req2),
                
                % Keep connection alive and handle cleanup
                stream_loop(StreamId, Req2, Opts);
            {error, ValidationError} ->
                % Send error message and close stream
                ErrorMessage = hb_util:encode(#{
                    <<"type">> => <<"error">>,
                    <<"message">> => ValidationError,
                    <<"timestamp">> => erlang:system_time(millisecond)
                }),
                cowboy_req:stream_body([<<"data: ">>, ErrorMessage, <<"\n\n">>], fin, Req2)
        end,
        
        {ok, Req2}
    end.

%% @doc Register a streaming connection for receiving notifications
register_stream(StreamId, Template, _Req, _Opts) ->
    % Ensure notification manager is running
    start_notification_manager(),
    
    % Register with the notification manager
    case whereis(?NOTIFICATION_MANAGER) of
        undefined ->
            ?event(notify, {manager_not_available, {stream_id, StreamId}});
        ManagerPid ->
            StreamPid = self(),
            ManagerPid ! {register_listener, Template, StreamPid, StreamId},
            ?event(notify, {stream_registered_with_manager, {stream_id, StreamId}, {template, Template}})
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
%% Template Validation and Matching
%% ============================================================================

%% @doc Validate a template specification (map template or regex pattern)
validate_template(Template, _Opts) when is_map(Template) ->
    % Map templates are used for message structure matching
    % Validate that it's a proper map with at least one key
    case maps:size(Template) of
        0 -> {error, <<"Template cannot be empty">>};
        _ -> {ok, Template}
    end;
validate_template(Template, _Opts) when is_binary(Template) ->
    % Binary templates are treated as regex patterns for path matching
    try 
        % Test if the regex compiles properly
        case re:compile(Template) of
            {ok, _} -> {ok, Template};
            {error, Reason} -> {error, iolist_to_binary(io_lib:format("Invalid regex: ~p", [Reason]))}
        end
    catch
        _:_ -> {error, <<"Invalid regex template">>}
    end;
validate_template(Template, _Opts) ->
    {error, iolist_to_binary(io_lib:format("Template must be a map or binary, got: ~p", [Template]))}.

%% @doc Enhanced template matching using router's template system
template_matches(Event, Template, _Opts) when is_map(Template) ->
    % Use message structure matching (similar to dev_router:template_matches)
    case hb_message:match(Template, Event, primary) of
        {value_mismatch, _Key, _Val1, _Val2} -> false;
        true -> true;
        _Other -> false
    end;
template_matches(Event, Template, Opts) when is_binary(Template) ->
    % Use regex path matching
    EventPath = case hb_ao:get(<<"path">>, Event, Opts) of
        not_found -> <<"/">>;
        Path -> Path
    end,
    try 
        case re:run(EventPath, Template) of
            nomatch -> false;
            _ -> true
        end
    catch
        _:_ -> false
    end;
template_matches(_Event, _Template, _Opts) ->
    false.

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
            
        {register_listener, Template, StreamPid, Ref} ->
            % Register a new listener with template
            ets:insert(notification_listeners, {Template, StreamPid, Ref}),
            ?event(notify, {listener_registered, {template, Template}, {stream, StreamPid}, {ref, Ref}}),
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
    lists:foreach(fun({Template, StreamPid, Ref}) ->
        spawn(fun() -> 
            try_dispatch_to_listener(Template, StreamPid, Ref, Event, Opts)
        end)
    end, AllListeners).

%% @doc Try to dispatch an event to a specific listener if template matches
try_dispatch_to_listener(Template, StreamPid, Ref, Event, Opts) ->
    try
        case template_matches(Event, Template, Opts) of
            true ->
                ?event(notify, {matched_template, {template, Template}, {ref, Ref}}),
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
                % Template didn't match, no action needed
                ok
        end
    catch
        Class:Reason ->
            ?event(notify, {dispatch_error, {class, Class}, {reason, Reason}, {template, Template}})
    end.

%% ============================================================================
%% Tests
%% ============================================================================

%% @doc Test template validation
template_validation_test() ->
    % Valid map template
    ?assertEqual({ok, #{ <<"device">> => <<"test">> }}, 
                 validate_template(#{ <<"device">> => <<"test">> }, #{})),
    
    % Valid regex template  
    ?assertEqual({ok, <<"/.*/test">>}, 
                 validate_template(<<"/.*/test">>, #{})),
    
    % Invalid empty map
    ?assertMatch({error, _}, validate_template(#{}, #{})),
    
    % Invalid regex
    ?assertMatch({error, _}, validate_template(<<"[invalid">>, #{})),
    
    % Invalid type
    ?assertMatch({error, _}, validate_template(123, #{})).

%% @doc Test template matching
template_matching_test() ->
    % Map template matching
    MapTemplate = #{ <<"device">> => <<"process@1.0">> },
    
    Event1 = #{ <<"device">> => <<"process@1.0">>, <<"path">> => <<"/compute">> },
    ?assertEqual(true, template_matches(Event1, MapTemplate, #{})),
    
    Event2 = #{ <<"device">> => <<"message@1.0">>, <<"path">> => <<"/compute">> },
    ?assertEqual(false, template_matches(Event2, MapTemplate, #{})),
    
    % Regex template matching
    RegexTemplate = <<"/.*process.*/.*">>,
    
    Event3 = #{ <<"path">> => <<"/test/process@1.0/compute">> },
    ?assertEqual(true, template_matches(Event3, RegexTemplate, #{})),
    
    Event4 = #{ <<"path">> => <<"/test/message@1.0/compute">> },
    ?assertEqual(false, template_matches(Event4, RegexTemplate, #{})). 