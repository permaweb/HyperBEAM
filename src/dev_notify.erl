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
        not_found -> {error, <<"No template specified">>};
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
    
    % Extract template for removal
    TemplateResult = case hb_ao:get(<<"template">>, InputMsg, Opts) of
        not_found -> {error, <<"No template specified">>};
        TemplateSpec -> {ok, TemplateSpec}
    end,
    
    case TemplateResult of
        {error, Reason} -> {error, Reason};
        {ok, ExtractedTemplate} ->
            % Validate template to get the same format as register function
            case validate_template(ExtractedTemplate, Opts) of
                {ok, ValidatedTemplate} ->
                    % Find and remove the matching template from state
                    Listeners = maps:get(<<"listeners">>, StateMsg, #{}),
                    % We need to find the key that matches our template, accounting for message processing
                    MatchingKey = find_matching_template_key(ValidatedTemplate, Listeners),
                    case MatchingKey of
                        not_found ->
                            {error, <<"Template not found in listeners">>};
                        Key ->
                            NewState = maps:remove(Key, Listeners),
                            {ok, StateMsg#{<<"listeners">> => NewState}}
                    end;
                {error, ValidationError} ->
                    {error, ValidationError}
            end
    end.

%% @doc Find the template key that matches our target template
find_matching_template_key(TargetTemplate, Listeners) ->
    Keys = maps:keys(Listeners),
    find_matching_key(TargetTemplate, Keys).

find_matching_key(_TargetTemplate, []) ->
    not_found;
find_matching_key(TargetTemplate, [{template, StoredTemplate} | Rest]) ->
    case templates_match(TargetTemplate, StoredTemplate) of
        true -> {template, StoredTemplate};
        false -> find_matching_key(TargetTemplate, Rest)
    end;
find_matching_key(TargetTemplate, [_OtherKey | Rest]) ->
    find_matching_key(TargetTemplate, Rest).

%% @doc Check if two templates are equivalent
templates_match(Template1, Template2) when is_binary(Template1), is_binary(Template2) ->
    Template1 =:= Template2;
templates_match({compiled_regex, _CompiledRegex1, OriginalPattern1}, {compiled_regex, _CompiledRegex2, OriginalPattern2}) ->
    % Compare original patterns for compiled regex templates
    OriginalPattern1 =:= OriginalPattern2;
templates_match({compiled_regex, _CompiledRegex, OriginalPattern}, Template) when is_binary(Template) ->
    % Compare compiled regex with binary template
    OriginalPattern =:= Template;
templates_match(Template, {compiled_regex, _CompiledRegex, OriginalPattern}) when is_binary(Template) ->
    % Compare binary template with compiled regex
    Template =:= OriginalPattern;
templates_match(Template1, Template2) when is_map(Template1), is_map(Template2) ->
    % For map templates, we need to compare the core fields, ignoring metadata
    % Extract the essential template fields
    Essential1 = extract_essential_template(Template1),
    Essential2 = extract_essential_template(Template2),
    Essential1 =:= Essential2;
templates_match(_Template1, _Template2) ->
    false.

%% @doc Extract essential template fields, removing HyperBEAM metadata
extract_essential_template(Template) when is_map(Template) ->
    % Remove priv and other metadata keys, keep only the core template fields
    maps:without([<<"priv">>, <<"id">>, <<"unsigned_id">>, <<"hashpath">>], Template);
extract_essential_template(Template) ->
    Template.

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
    Template = hb_ao:get(<<"template">>, InputMsg, <<".*">>, Opts),
    
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
    % Compile the regex once for efficiency
    try 
        case re:compile(Template) of
            {ok, CompiledRegex} -> {ok, {compiled_regex, CompiledRegex, Template}};
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
template_matches(Event, {compiled_regex, CompiledRegex, _OriginalPattern}, Opts) ->
    % Use pre-compiled regex for efficient path matching
    EventPath = case hb_ao:get(<<"path">>, Event, Opts) of
        not_found -> <<"/">>;
        Path -> Path
    end,
    try 
        case re:run(EventPath, CompiledRegex) of
            nomatch -> false;
            _ -> true
        end
    catch
        _:_ -> false
    end;
template_matches(Event, Template, Opts) when is_binary(Template) ->
    % Fallback for legacy binary templates (compile on-the-fly)
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
                try register(?NOTIFICATION_MANAGER, self()) of
                    true ->
                        ?event(notify, {notification_manager_started, self()}),
                        notification_manager_loop(#{})
                catch
                    error:badarg ->
                        % Someone else registered already, exit quietly
                        exit(already_registered)
                end
            end),
            % Give the process a moment to register
            timer:sleep(10),
            % Check if it actually got registered
            case whereis(?NOTIFICATION_MANAGER) of
                ManagerPid -> {ok, ManagerPid};
                OtherPid when is_pid(OtherPid) -> {already_started, OtherPid};
                undefined -> {error, registration_failed}
            end;
        Pid ->
            {already_started, Pid}
    end.

%% @doc Stop the notification manager process
stop_notification_manager() ->
    case whereis(?NOTIFICATION_MANAGER) of
        undefined -> ok;
        Pid ->
            % First unregister to prevent new registrations
            try unregister(?NOTIFICATION_MANAGER) catch _:_ -> ok end,
            
            % Send stop signal
            Pid ! stop,
            
            % Wait for process to die with timeout
            Ref = monitor(process, Pid),
            receive
                {'DOWN', Ref, process, Pid, _Reason} -> ok
            after 100 ->
                exit(Pid, kill),
                receive {'DOWN', Ref, process, Pid, _} -> ok after 50 -> ok end
            end,
            
            % Clean up ETS table if it still exists
            case ets:info(notification_listeners) of
                undefined -> ok;
                _ -> 
                    try ets:delete(notification_listeners)
                    catch _:_ -> ok
                    end
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
            % Clean up ETS table before exiting
            case ets:info(notification_listeners) of
                undefined -> ok;
                _ -> 
                    try ets:delete(notification_listeners)
                    catch _:_ -> ok
                    end
            end,
            ok;
            
        Msg ->
            ?event(notify, {unexpected_message, Msg}),
            notification_manager_loop(State)
    end.

%% @doc Handle event dispatch by matching against registered listeners
handle_event_dispatch(Event, Opts) ->
    try
        % Get all registered listeners
        AllListeners = ets:tab2list(notification_listeners),
        
        % Spawn short-lived worker processes for actual dispatching
        % This keeps the manager process responsive
        lists:foreach(fun({Template, StreamPid, Ref}) ->
            spawn(fun() -> 
                try_dispatch_to_listener(Template, StreamPid, Ref, Event, Opts)
            end)
        end, AllListeners)
    catch
        _:_ ->
            % Table might not exist, ignore
            ok
    end.

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
    MapTemplate = #{ <<"device">> => <<"test">> },
    MapResult = validate_template(MapTemplate, #{}),
    ?event(debug, {map_template_validation, {input, MapTemplate}, {result, MapResult}}),
    ?assertEqual({ok, #{ <<"device">> => <<"test">> }}, MapResult),
    
    % Valid regex template  
    RegexTemplate = <<"/.*/test">>,
    RegexResult = validate_template(RegexTemplate, #{}),
    ?event(debug, {regex_template_validation, {input, RegexTemplate}, {result, RegexResult}}),
    ?assertMatch({ok, {compiled_regex, _, <<"/.*/test">>}}, RegexResult),
    
    % Invalid empty map
    EmptyResult = validate_template(#{}, #{}),
    ?event(debug, {empty_map_validation, {result, EmptyResult}}),
    ?assertMatch({error, _}, EmptyResult),
    
    % Invalid regex
    InvalidRegexResult = validate_template(<<"[invalid">>, #{}),
    ?event(debug, {invalid_regex_validation, {result, InvalidRegexResult}}),
    ?assertMatch({error, _}, InvalidRegexResult),
    
    % Invalid type
    InvalidTypeResult = validate_template(123, #{}),
    ?event(debug, {invalid_type_validation, {result, InvalidTypeResult}}),
    ?assertMatch({error, _}, InvalidTypeResult).

%% @doc Test template matching
template_matching_test() ->
    % Map template matching
    MapTemplate = #{ <<"device">> => <<"process@1.0">> },
    
    Event1 = #{ <<"device">> => <<"process@1.0">>, <<"path">> => <<"/compute">> },
    Match1 = template_matches(Event1, MapTemplate, #{}),
    ?event(debug, {map_template_match, {template, MapTemplate}, {event, Event1}, {result, Match1}}),
    ?assertEqual(true, Match1),
    
    Event2 = #{ <<"device">> => <<"message@1.0">>, <<"path">> => <<"/compute">> },
    Match2 = template_matches(Event2, MapTemplate, #{}),
    ?event(debug, {map_template_no_match, {template, MapTemplate}, {event, Event2}, {result, Match2}}),
    ?assertEqual(false, Match2),
    
    % Regex template matching
    RegexTemplate = <<"/.*process.*/.*">>,
    
    Event3 = #{ <<"path">> => <<"/test/process@1.0/compute">> },
    Match3 = template_matches(Event3, RegexTemplate, #{}),
    ?event(debug, {regex_template_match, {template, RegexTemplate}, {event, Event3}, {result, Match3}}),
    ?assertEqual(true, Match3),
    
    Event4 = #{ <<"path">> => <<"/test/message@1.0/compute">> },
    Match4 = template_matches(Event4, RegexTemplate, #{}),
    ?event(debug, {regex_template_no_match, {template, RegexTemplate}, {event, Event4}, {result, Match4}}),
    ?assertEqual(false, Match4).

%% @doc Test notification manager process lifecycle
notification_manager_test() ->
    % Ensure clean state
    InitialState = whereis(?NOTIFICATION_MANAGER),
    ?event(debug, {manager_initial_state, InitialState}),
    stop_notification_manager(),
    timer:sleep(50),
    
    % Start manager
    StartResult = start_notification_manager(),
    ?event(debug, {manager_start_result, StartResult}),
    {ok, ManagerPid} = StartResult,
    ?assert(is_process_alive(ManagerPid)),
    ?assertEqual(ManagerPid, whereis(?NOTIFICATION_MANAGER)),
    ?event(debug, {manager_started_successfully, {pid, ManagerPid}}),
    
    % Stop manager
    ?event(debug, {stopping_manager}),
    stop_notification_manager(),
    % Wait a bit for cleanup
    timer:sleep(100),
    FinalState = whereis(?NOTIFICATION_MANAGER),
    ?event(debug, {manager_final_state, FinalState}),
    ?assertEqual(undefined, FinalState).

%% @doc Test event dispatching with template matching
event_dispatch_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),
    
    start_notification_manager(),
    ManagerPid = whereis(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),
    
    % Create a test stream process
    TestPid = self(),
    StreamPid = spawn(fun() ->
        receive 
            {notify_event, Event} -> TestPid ! {received_event, Event}
        after 1000 -> TestPid ! timeout
        end
    end),
    
    % Register listener with map template
    MapTemplate = #{ <<"device">> => <<"process@1.0">> },
    Ref1 = make_ref(),
    ?event(debug, {registering_listener, {template, MapTemplate}, {stream_pid, StreamPid}, {ref, Ref1}}),
    ManagerPid ! {register_listener, MapTemplate, StreamPid, Ref1},
    
    % Create test events
    MatchingEvent = #{
        <<"device">> => <<"process@1.0">>,
        <<"action">> => <<"compute">>,
        <<"data">> => <<"test">>
    },
    
    NonMatchingEvent = #{
        <<"device">> => <<"message@1.0">>,
        <<"action">> => <<"compute">>
    },
    
    % Dispatch matching event
    ?event(debug, {dispatching_matching_event, MatchingEvent}),
    ManagerPid ! {dispatch_event, MatchingEvent, #{}},
    
    % Should receive the event
    receive
        {received_event, ReceivedEvent} ->
            ?event(debug, {received_matching_event, ReceivedEvent}),
            ?assertEqual(MatchingEvent, ReceivedEvent)
    after 500 ->
        ?event(debug, {timeout_on_matching_event}),
        ?assert(false, "Should have received matching event")
    end,
    
    % Dispatch non-matching event
    ?event(debug, {dispatching_non_matching_event, NonMatchingEvent}),
    ManagerPid ! {dispatch_event, NonMatchingEvent, #{}},
    
    % Should not receive anything (timeout expected)
    receive
        {received_event, UnexpectedEvent} ->
            ?event(debug, {unexpected_event_received, UnexpectedEvent}),
            ?assert(false, "Should not have received non-matching event")
    after 100 ->
        ?event(debug, {expected_timeout_on_non_matching_event}),
        ok % Expected timeout
    end,
    
    stop_notification_manager().

%% @doc Test integration with hb_persistent notification
integration_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),
    
    % Test the integration point with hb_persistent
    GroupName = <<"test-group">>,
    Msg2 = #{ <<"path">> => <<"/test">>, <<"data">> => <<"request">> },
    Msg3 = #{ <<"result">> => <<"success">>, <<"timestamp">> => erlang:system_time(millisecond) },
    Opts = #{ notify_device => <<"notify@1.0">> },
    
    % Start notification manager
    start_notification_manager(),
    ManagerPid = whereis(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),
    
    % This should call our dispatch function
    hb_persistent:dispatch_to_notify_device(GroupName, Msg2, Msg3, Opts),
    timer:sleep(50),
    
    % Manager should still be running (no crashes)
    ?assert(is_process_alive(ManagerPid)),
    
    stop_notification_manager().

%% @doc Test device registration and unregistration functions
device_registration_test() ->
    StateMsg = #{<<"listeners">> => #{}},
    ?event(debug, {initial_state, StateMsg}),
    
    % Test successful registration with map template
    InputMsg1 = #{
        <<"template">> => #{ <<"device">> => <<"test@1.0">> },
        <<"stream">> => <<"test-stream">>
    },
    ?event(debug, {registering_map_template, InputMsg1}),
    
    {ok, NewState1} = register(StateMsg, InputMsg1, #{}),
    Listeners1 = maps:get(<<"listeners">>, NewState1),
    ?event(debug, {after_map_registration, {listeners_count, maps:size(Listeners1)}, {keys, maps:keys(Listeners1)}}),
    ?assertEqual(1, maps:size(Listeners1)),
    
    % Test registration with regex template
    InputMsg2 = #{
        <<"template">> => <<"/.*test.*/.*">>,
        <<"stream">> => <<"test-stream-2">>
    },
    
    {ok, NewState2} = register(NewState1, InputMsg2, #{}),
    Listeners2 = maps:get(<<"listeners">>, NewState2),
    ?assertEqual(2, maps:size(Listeners2)),
    
    
    % Test unregistration - unregister the first template we added
    UnregMsg = #{
        <<"template">> => #{ <<"device">> => <<"test@1.0">> }
    },
    
    ?event(debug, {before_unregister, {listeners_count, maps:size(maps:get(<<"listeners">>, NewState2))}}),
    {ok, NewState3} = unregister(NewState2, UnregMsg, #{}),
    Listeners3 = maps:get(<<"listeners">>, NewState3),
    ?event(debug, {after_unregister, {listeners_count, maps:size(Listeners3)}, {keys, maps:keys(Listeners3)}}),
    ?assertEqual(1, maps:size(Listeners3)), % Should have 1 left (regex)
    
    % Verify the correct template was removed
    ?assertNot(maps:is_key({template, #{ <<"device">> => <<"test@1.0">> }}, Listeners3)),
    
    % Test error cases
    ?assertMatch({error, _}, register(StateMsg, #{}, #{})),
    ?assertMatch({error, _}, register(StateMsg, #{<<"template">> => #{}}, #{})),
    ?assertMatch({error, _}, unregister(StateMsg, #{}, #{})).

%% @doc Test listener registration and unregistration
listener_registration_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),
    
    % Start manager
    start_notification_manager(),
    ManagerPid = whereis(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),
    
    % Register a listener
    Template = #{ <<"device">> => <<"message@1.0">> },
    StreamPid = spawn(fun() -> receive _ -> ok end end),
    Ref = make_ref(),
    
    ManagerPid ! {register_listener, Template, StreamPid, Ref},
    timer:sleep(10), % Allow registration to process
    
    % Check listener is registered
    AllListeners = ets:tab2list(notification_listeners),
    ?assert(lists:member({Template, StreamPid, Ref}, AllListeners)),
    
    % Unregister listener
    ManagerPid ! {unregister_listener, Ref},
    timer:sleep(10), % Allow unregistration to process
    
    % Check listener is removed
    AllListeners2 = ets:tab2list(notification_listeners),
    ?assertNot(lists:member({Template, StreamPid, Ref}, AllListeners2)),
    
    stop_notification_manager().

%% @doc Test regex template matching in event dispatch
regex_dispatch_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),
    
    start_notification_manager(),
    ManagerPid = whereis(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),
    
    % Create test stream process
    TestPid = self(),
    StreamPid = spawn(fun() ->
        receive 
            {notify_event, Event} -> TestPid ! {received_event, Event}
        after 1000 -> TestPid ! timeout
        end
    end),
    
    % Register listener with regex template
    RegexTemplate = <<"/.*process.*/.*">>,
    Ref = make_ref(),
    ManagerPid ! {register_listener, RegexTemplate, StreamPid, Ref},
    
    % Test matching path
    MatchingEvent = #{ <<"path">> => <<"/test/process@1.0/compute">> },
    ManagerPid ! {dispatch_event, MatchingEvent, #{}},
    
    receive
        {received_event, ReceivedEvent} ->
            ?assertEqual(MatchingEvent, ReceivedEvent)
    after 500 ->
        ?assert(false, "Should have received matching event")
    end,
    
    % Test non-matching path
    NonMatchingEvent = #{ <<"path">> => <<"/test/message@1.0/compute">> },
    ManagerPid ! {dispatch_event, NonMatchingEvent, #{}},
    
    receive
        {received_event, _} ->
            ?assert(false, "Should not have received non-matching event")
    after 100 ->
        ok % Expected timeout
    end,
    
    stop_notification_manager().

%% @doc Test dispatch function
dispatch_function_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),
    
    % Start manager for testing
    start_notification_manager(),
    
    % Test dispatch with valid input
    InputMsg = #{
        <<"event">> => #{ <<"test">> => <<"data">> },
        <<"timestamp">> => erlang:system_time(millisecond)
    },
    
    Result = dispatch(#{}, InputMsg, #{}),
    ?assertEqual({ok, <<"OK">>}, Result),
    
    stop_notification_manager().

%% @doc Test stream function
stream_function_test() ->
    % Test stream creation with valid template
    InputMsg1 = #{
        <<"template">> => #{ <<"device">> => <<"test@1.0">> }
    },
    
    {ok, Result1} = stream(#{}, InputMsg1, #{}),
    ?assertEqual(200, maps:get(<<"status">>, Result1)),
    ?assertEqual(true, maps:get(<<"streaming">>, Result1)),
    ?assert(maps:is_key(<<"headers">>, Result1)),
    ?assert(maps:is_key(<<"body">>, Result1)),
    
    % Test with regex template
    InputMsg2 = #{
        <<"template">> => <<"/.*test.*/.*">>
    },
    
    {ok, Result2} = stream(#{}, InputMsg2, #{}),
    ?assertEqual(200, maps:get(<<"status">>, Result2)).
    

%% @doc Test dead process cleanup
dead_process_cleanup_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),
    
    start_notification_manager(),
    ManagerPid = whereis(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),
    
    % Create and register a process, then kill it
    DeadPid = spawn(fun() -> ok end),
    timer:sleep(10), % Ensure process dies
    ?assertNot(is_process_alive(DeadPid)),
    ?event(debug, {created_dead_process, {pid, DeadPid}, {alive, is_process_alive(DeadPid)}}),
    
    Template = #{ <<"device">> => <<"message@1.0">> },
    Ref = make_ref(),
    
    % Register the dead process
    ?event(debug, {registering_dead_process, {template, Template}, {dead_pid, DeadPid}}),
    ManagerPid ! {register_listener, Template, DeadPid, Ref},
    timer:sleep(10),
    
    % Verify it's in the table
    AllListeners = ets:tab2list(notification_listeners),
    ?event(debug, {listeners_before_cleanup, {count, length(AllListeners)}, {has_dead_process, lists:member({Template, DeadPid, Ref}, AllListeners)}}),
    ?assert(lists:member({Template, DeadPid, Ref}, AllListeners)),
    
    % Dispatch an event that matches
    Event = #{ <<"device">> => <<"message@1.0">> },
    ?event(debug, {dispatching_to_dead_process, Event}),
    ManagerPid ! {dispatch_event, Event, #{}},
    timer:sleep(50), % Allow cleanup to happen
    
    % Dead process should be cleaned up
    AllListeners2 = ets:tab2list(notification_listeners),
    ?event(debug, {listeners_after_cleanup, {count, length(AllListeners2)}, {dead_process_removed, not lists:member({Template, DeadPid, Ref}, AllListeners2)}}),
    ?assertNot(lists:member({Template, DeadPid, Ref}, AllListeners2)),
    
    stop_notification_manager().

%% @doc Test concurrent dispatching
concurrent_dispatch_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),
    
    start_notification_manager(),
    ManagerPid = whereis(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),
    
    % Create multiple test processes
    TestPid = self(),
    NumStreams = 5,
    
    StreamPids = lists:map(fun(N) ->
        spawn(fun() ->
            receive 
                {notify_event, Event} -> 
                    TestPid ! {received_event, N, Event}
            after 1000 -> 
                TestPid ! {timeout, N}
            end
        end)
    end, lists:seq(1, NumStreams)),
    
    % Register all with the same template
    Template = #{ <<"type">> => <<"broadcast">> },
    lists:foreach(fun({_N, Pid}) ->
        ManagerPid ! {register_listener, Template, Pid, make_ref()}
    end, lists:zip(lists:seq(1, NumStreams), StreamPids)),
    
    timer:sleep(20), % Allow registrations to process
    
    % Dispatch one event
    Event = #{ <<"type">> => <<"broadcast">>, <<"message">> => <<"hello">> },
    ManagerPid ! {dispatch_event, Event, #{}},
    
    % All streams should receive the event
    ReceivedCount = receive_events(NumStreams, 0),
    ?assertEqual(NumStreams, ReceivedCount),
    
    stop_notification_manager().

%% Helper function for concurrent test
receive_events(0, Count) -> Count;
receive_events(Remaining, Count) ->
    receive
        {received_event, _N, _Event} ->
            receive_events(Remaining - 1, Count + 1);
        {timeout, _N} ->
            receive_events(Remaining - 1, Count)
    after 500 ->
        Count
    end.

%% @doc Test template validation edge cases
template_validation_edge_cases_test() ->
    % Test deeply nested map template
    NestedTemplate = #{
        <<"device">> => <<"test@1.0">>,
        <<"data">> => #{
            <<"nested">> => #{
                <<"value">> => <<"deep">>
            }
        }
    },
    ?assertEqual({ok, NestedTemplate}, validate_template(NestedTemplate, #{})),
    
    % Test complex regex patterns
    ComplexRegex = <<"^/(test|prod)/process@[0-9]+\\.[0-9]+/.+$">>,
    ?assertMatch({ok, {compiled_regex, _, ComplexRegex}}, validate_template(ComplexRegex, #{})),
    
    % Test unicode in templates
    UnicodeTemplate = #{ <<"message">> => <<"Hello 世界">> },
    ?assertEqual({ok, UnicodeTemplate}, validate_template(UnicodeTemplate, #{})),
    
    % Test very long regex
    LongRegex = iolist_to_binary(lists:duplicate(1000, "a")),
    ?assertMatch({ok, {compiled_regex, _, LongRegex}}, validate_template(LongRegex, #{})).

%% @doc Test message matching edge cases
message_matching_edge_cases_test() ->
    % Test partial map matching
    Template = #{ <<"device">> => <<"message@1.0">> },
    Event = #{
        <<"device">> => <<"message@1.0">>,
        <<"extra">> => <<"field">>,
        <<"more">> => #{ <<"nested">> => <<"data">> }
    },
    ?assertEqual(true, template_matches(Event, Template, #{})),
    
    % Test case sensitivity
    CaseTemplate = #{ <<"Device">> => <<"Message@1.0">> },
    CaseEvent = #{ <<"device">> => <<"message@1.0">> },
    ?assertEqual(false, template_matches(CaseEvent, CaseTemplate, #{})),
    
    % Test missing path in regex matching
    RegexTemplate = <<"/test.*">>,
    EventWithoutPath = #{ <<"device">> => <<"message@1.0">> },
    ?assertEqual(false, template_matches(EventWithoutPath, RegexTemplate, #{})),
    
    % Test empty path
    EventWithEmptyPath = #{ <<"path">> => <<"">> },
    ?assertEqual(false, template_matches(EventWithEmptyPath, RegexTemplate, #{})).

%% @doc Test error handling and recovery
error_handling_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),
    
    start_notification_manager(),
    ManagerPid = whereis(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),
    
    % Test invalid template in dispatch (should not crash manager)
    InvalidTemplate = invalid_template_atom,
    TestPid = spawn(fun() -> receive _ -> ok end end),
    Ref = make_ref(),
    
    % Register invalid template (insert directly to bypass validation)
    ets:insert(notification_listeners, {InvalidTemplate, TestPid, Ref}),
    
    % Dispatch event - should handle error gracefully
    Event = #{ <<"test">> => <<"data">> },
    ManagerPid ! {dispatch_event, Event, #{}},
    timer:sleep(50),
    
    % Manager should still be alive
    ?assert(is_process_alive(ManagerPid)),
    
    stop_notification_manager().

%% @doc Performance test for high-frequency events
performance_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),
    
    start_notification_manager(),
    ManagerPid = whereis(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),
    
    % Register a listener
    Template = #{ <<"type">> => <<"perf-test">> },
    TestPid = spawn(fun() ->
        receive_loop(0)
    end),
    Ref = make_ref(),
    ManagerPid ! {register_listener, Template, TestPid, Ref},
    timer:sleep(10),
    
    % Send many events quickly
    NumEvents = 100,
    StartTime = erlang:system_time(microsecond),
    
    lists:foreach(fun(N) ->
        Event = #{
            <<"type">> => <<"perf-test">>,
            <<"sequence">> => N,
            <<"timestamp">> => erlang:system_time(millisecond)
        },
        ManagerPid ! {dispatch_event, Event, #{}}
    end, lists:seq(1, NumEvents)),
    
    EndTime = erlang:system_time(microsecond),
    Duration = EndTime - StartTime,
    
    ?event(notify, {performance_test, {events, NumEvents}, {duration_us, Duration}}),
    
    % Should handle 100 events quickly (< 100ms)
    ?assert(Duration < 100000, "Performance test took too long"),
    
    stop_notification_manager().

receive_loop(Count) ->
    receive
        {notify_event, _Event} ->
            receive_loop(Count + 1)
    after 10 ->
        exit({received_count, Count})
    end.

%% @doc Test duplicate listener registration bug
duplicate_listener_registration_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),
    
    start_notification_manager(),
    ManagerPid = whereis(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),
    
    % Create test stream process that counts events
    TestPid = self(),
    StreamPid = spawn(fun() ->
        duplicate_event_counter(0)
    end),
    
    % Register the same template and stream multiple times
    Template = #{ <<"device">> => <<"duplicate-test@1.0">> },
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    Ref3 = make_ref(),
    
    ?event(debug, {registering_duplicate_listeners, {template, Template}, {stream_pid, StreamPid}}),
    ManagerPid ! {register_listener, Template, StreamPid, Ref1},
    ManagerPid ! {register_listener, Template, StreamPid, Ref2},
    ManagerPid ! {register_listener, Template, StreamPid, Ref3},
    timer:sleep(20), % Allow registrations to process
    
    % Check how many entries are in the ETS table
    AllListeners = ets:tab2list(notification_listeners),
    MatchingListeners = [L || {T, P, _R} = L <- AllListeners, T =:= Template, P =:= StreamPid],
    ?event(debug, {duplicate_registrations_found, {count, length(MatchingListeners)}, {entries, MatchingListeners}}),
    
    % This test will FAIL with current implementation - showing the bug
    % The same {Template, StreamPid} should only be registered once
    ?assertEqual(1, length(MatchingListeners), "Same template/stream should only be registered once"),
    
    % Dispatch one event
    Event = #{ <<"device">> => <<"duplicate-test@1.0">>, <<"message">> => <<"test">> },
    ManagerPid ! {dispatch_event, Event, #{}},
    timer:sleep(50),
    
    % Stream should receive the event only once
    StreamPid ! {get_count, TestPid},
    receive
        {event_count, Count} ->
            ?event(debug, {event_count_received, Count}),
            ?assertEqual(1, Count, "Event should be received only once, not duplicated")
    after 500 ->
        ?assert(false, "Should have received event count")
    end,
    
    stop_notification_manager().

%% Helper process that counts duplicate events
duplicate_event_counter(Count) ->
    receive
        {notify_event, Event} ->
            ?event(debug, {duplicate_counter_received_event, {count, Count + 1}, {event, Event}}),
            duplicate_event_counter(Count + 1);
        {get_count, ReplyPid} ->
            ReplyPid ! {event_count, Count},
            duplicate_event_counter(Count)
    after 1000 ->
        exit({final_count, Count})
    end.

%% @doc Test multiple registration attempts with different scenarios
multiple_registration_scenarios_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),
    
    start_notification_manager(),
    ManagerPid = whereis(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),
    
    % Scenario 1: Same template, same stream, different refs
    Template1 = #{ <<"type">> => <<"scenario1">> },
    Stream1 = spawn(fun() -> receive _ -> ok end end),
    
    ManagerPid ! {register_listener, Template1, Stream1, make_ref()},
    ManagerPid ! {register_listener, Template1, Stream1, make_ref()},
    timer:sleep(10),
    
    Listeners1 = [L || {T, P, _R} = L <- ets:tab2list(notification_listeners), T =:= Template1, P =:= Stream1],
    ?event(debug, {scenario1_duplicates, {count, length(Listeners1)}}),
    % Same template/stream should only be registered once
    ?assertEqual(1, length(Listeners1), "Same template/stream should only be registered once"),
    
    % Scenario 2: Same template, different streams (should be allowed)
    Template2 = #{ <<"type">> => <<"scenario2">> },
    Stream2A = spawn(fun() -> receive _ -> ok end end),
    Stream2B = spawn(fun() -> receive _ -> ok end end),
    
    ManagerPid ! {register_listener, Template2, Stream2A, make_ref()},
    ManagerPid ! {register_listener, Template2, Stream2B, make_ref()},
    timer:sleep(10),
    
    Listeners2 = [L || {T, _P, _R} = L <- ets:tab2list(notification_listeners), T =:= Template2],
    ?event(debug, {scenario2_different_streams, {count, length(Listeners2)}}),
    ?assertEqual(2, length(Listeners2), "Different streams with same template should be allowed"),
    
    % Scenario 3: Different templates, same stream (should be allowed)
    Template3A = #{ <<"type">> => <<"scenario3a">> },
    Template3B = #{ <<"type">> => <<"scenario3b">> },
    Stream3 = spawn(fun() -> receive _ -> ok end end),
    
    ManagerPid ! {register_listener, Template3A, Stream3, make_ref()},
    ManagerPid ! {register_listener, Template3B, Stream3, make_ref()},
    timer:sleep(10),
    
    Listeners3 = [L || {_T, P, _R} = L <- ets:tab2list(notification_listeners), P =:= Stream3],
    ?event(debug, {scenario3_different_templates, {count, length(Listeners3)}}),
    ?assertEqual(2, length(Listeners3), "Same stream with different templates should be allowed"),
    
    stop_notification_manager().

%% @doc Test event duplication with multiple registrations
event_duplication_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),
    
    start_notification_manager(),
    ManagerPid = whereis(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),
    
    % Create counting process
    TestPid = self(),
    CounterPid = spawn(fun() ->
        event_duplication_counter(0, TestPid)
    end),
    
    % Register the same listener 3 times (showing the bug)
    Template = #{ <<"event">> => <<"duplication-test">> },
    ManagerPid ! {register_listener, Template, CounterPid, make_ref()},
    ManagerPid ! {register_listener, Template, CounterPid, make_ref()},
    ManagerPid ! {register_listener, Template, CounterPid, make_ref()},
    timer:sleep(20),
    
    % Dispatch one event
    Event = #{ <<"event">> => <<"duplication-test">>, <<"data">> => <<"single event">> },
    ?event(debug, {dispatching_single_event, Event}),
    ManagerPid ! {dispatch_event, Event, #{}},
    timer:sleep(50),
    
    % Check how many times the event was received
    CounterPid ! {report_count, TestPid},
    receive
        {duplicate_count, ReceivedCount} ->
            ?event(debug, {event_received_count, ReceivedCount}),
            % Event should be received only once, not duplicated
            ?assertEqual(1, ReceivedCount, "Event should be received only once, not duplicated")
    after 500 ->
        ?assert(false, "Should have received count report")
    end,
    
    stop_notification_manager().

%% Helper for event duplication test
event_duplication_counter(Count, TestPid) ->
    receive
        {notify_event, Event} ->
            ?event(debug, {duplication_counter_event, {count, Count + 1}, {event, Event}}),
            event_duplication_counter(Count + 1, TestPid);
        {report_count, ReplyPid} ->
            ReplyPid ! {duplicate_count, Count},
            event_duplication_counter(Count, TestPid)
    after 1000 ->
        TestPid ! {final_duplicate_count, Count}
    end. 