%%% @doc A device that provides real-time notifications for AO process events.
%%% It integrates with the existing event system (hb_event) and allows clients
%%% to subscribe to specific events using HTTP/3 streams. Registration happens
%%% automatically when clients establish streaming connections.
%%%
%%% Uses a long-running notification manager process to handle high-frequency
%%% message matching and dispatching efficiently.
-module(dev_notify).

-export([info/1, info/3, dispatch/3, stream/3, start_manager/3]).
-export([start_notification_manager/0, stop_notification_manager/0]).

-include("include/hb.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(NOTIFICATION_MANAGER, {dev_notify, notification_manager}).

%% @doc Device API information
info(_) ->
    #{exports => [info, dispatch, stream, start_manager], variant => <<"Notify/1.0">>}.

%% @doc HTTP info response providing information about this device
info(_Msg1, _Msg2, _Opts) ->
    InfoBody =
        #{<<"description">> => <<"Notification device for real-time event streaming">>,
          <<"version">> => <<"1.0">>,
          <<"paths">> =>
              #{<<"info">> => <<"Get device info">>,
                <<"dispatch">> => <<"Dispatch an event to registered listeners">>,
                <<"stream">> => <<"Start a streaming connection for real-time events">>,
                <<"start_manager">> => <<"Start notification manager via hook (internal)">>}},
    {ok, #{<<"status">> => 200, <<"body">> => InfoBody}}.

%% @doc Start notification manager if notify_device is configured (called via start hook)
start_manager(_StateMsg, HookMsg, _Opts) ->
    % Extract the body from hook message (contains node configuration)
    NodeConfig = maps:get(<<"body">>, HookMsg, #{}),

    % Check if notify device is configured
    case hb_opts:get(notify_device, undefined, NodeConfig) of
        undefined ->
            {ok, HookMsg}; % No notify device configured, return unchanged
        _NotifyDeviceSpec ->
            start_notification_manager(),
            
            % Register the on-notify hook dynamically
            CurrentHooks = hb_opts:get(on, #{}, NodeConfig),
            UpdatedHooks = CurrentHooks#{
                <<"on-notify">> => #{
                    <<"device">> => #{
                        <<"on-notify">> => fun dev_notify:dispatch/3
                    }
                }
            },
            UpdatedNodeConfig = NodeConfig#{on => UpdatedHooks},
            UpdatedHookMsg = HookMsg#{<<"body">> => UpdatedNodeConfig},
            
            {ok, UpdatedHookMsg}
    end.

%% @doc Dispatch an event to registered listeners via the notification manager
dispatch(_StateMsg, InputMsg, Opts) ->
    ?event(notify, {dispatch_event, InputMsg}),

    % Check if notify device is configured
    case hb_opts:get(notify_device, undefined, Opts) of
        undefined ->
            {ok, <<"No notify device configured">>}; % No notify device configured
        _NotifyDeviceSpec ->
            % Send event to the long-running notification manager process
            case hb_name:lookup(?NOTIFICATION_MANAGER) of
                undefined ->
                    % Manager not started, return error instead of trying to start
                    {error, <<"Notification manager not available">>};
                ManagerPid ->
                    % Send directly to manager process (minimal overhead)
                    ManagerPid ! {dispatch_event, InputMsg, Opts},
                    {ok, <<"OK">>}
            end
    end.

%% Note: dispatch_to_listeners and push_event functions are now handled
%% by the notification manager process for better performance.

%% @doc Start a streaming connection for real-time event notifications
stream(_StateMsg, InputMsg, Opts) ->
    ?event(notify, {start_stream, InputMsg}),

    % Extract template for filtering events (supports both map and regex templates)
    Template = hb_ao:get(<<"template">>, InputMsg, <<".*">>, Opts),

    % Create streaming response headers
    Headers =
        #{<<"content-type">> => <<"application/json">>,
          <<"cache-control">> => <<"no-cache">>,
          <<"connection">> => <<"keep-alive">>,
          <<"access-control-allow-origin">> => <<"*">>},

    % Start the streaming response
    StreamingBody = create_streaming_body(Template, Opts),

    {ok,
     #{<<"status">> => 200,
       <<"headers">> => Headers,
       <<"body">> => StreamingBody,
       <<"streaming">> => true}}.

%% @doc Create a streaming body function that keeps the connection open
create_streaming_body(Template, Opts) ->
    fun(Req) ->
       % Initialize streaming response
       Req2 =
           cowboy_req:stream_reply(200,
                                   #{<<"content-type">> => <<"text/event-stream">>,
                                     <<"cache-control">> => <<"no-cache">>,
                                     <<"connection">> => <<"keep-alive">>,
                                     <<"access-control-allow-origin">> => <<"*">>},
                                   Req),

       % Validate template before registering
       case validate_template(Template, Opts) of
           {ok, ValidatedTemplate} ->
               % Register this stream for notifications
               StreamId = make_ref(),
               register_stream(StreamId, ValidatedTemplate, Req2, Opts),

               % Send initial connection message
               InitialMessage =
                   hb_util:encode(#{<<"type">> => <<"connection">>,
                                    <<"message">> => <<"Connected to notification stream">>,
                                    <<"template">> => ValidatedTemplate,
                                    <<"timestamp">> => erlang:system_time(millisecond)}),
               cowboy_req:stream_body([<<"data: ">>, InitialMessage, <<"\n\n">>], nofin, Req2),

               % Keep connection alive and handle cleanup
               stream_loop(StreamId, Req2, Opts);
           {error, ValidationError} ->
               % Send error message and close stream
               ErrorMessage =
                   hb_util:encode(#{<<"type">> => <<"error">>,
                                    <<"message">> => ValidationError,
                                    <<"timestamp">> => erlang:system_time(millisecond)}),
               cowboy_req:stream_body([<<"data: ">>, ErrorMessage, <<"\n\n">>], fin, Req2)
       end,

       {ok, Req2}
    end.

%% @doc Register a streaming connection for receiving notifications
register_stream(StreamId, Template, _Req, _Opts) ->
    % Register with the notification manager (no longer automatically starts)
    case hb_name:lookup(?NOTIFICATION_MANAGER) of
        undefined ->
            ?event(notify, {manager_not_available, {stream_id, StreamId}});
        ManagerPid ->
            StreamPid = self(),
            ManagerPid ! {register_listener, Template, StreamPid, StreamId},
            ?event(notify,
                   {stream_registered_with_manager, {stream_id, StreamId}, {template, Template}})
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
        KeepaliveMsg =
            hb_util:encode(#{<<"type">> => <<"keepalive">>,
                             <<"timestamp">> => erlang:system_time(millisecond)}),
        cowboy_req:stream_body([<<"data: ">>, KeepaliveMsg, <<"\n\n">>], nofin, Req),
        stream_loop(StreamId, Req, Opts)
    end.

%% @doc Unregister a streaming connection
unregister_stream(StreamId) ->
    % Unregister with the notification manager
    case hb_name:lookup(?NOTIFICATION_MANAGER) of
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
        0 ->
            {error, <<"Template cannot be empty">>};
        _ ->
            {ok, Template}
    end;
validate_template(Template, _Opts) when is_binary(Template) ->
    % Binary templates are treated as regex patterns for path matching
    % Compile the regex once for efficiency
    try
        case re:compile(Template) of
            {ok, CompiledRegex} ->
                {ok, {compiled_regex, CompiledRegex, Template}};
            {error, Reason} ->
                {error, iolist_to_binary(io_lib:format("Invalid regex: ~p", [Reason]))}
        end
    catch
        _:_ ->
            {error, <<"Invalid regex template">>}
    end;
validate_template(Template, _Opts) ->
    {error,
     iolist_to_binary(io_lib:format("Template must be a map or binary, got: ~p", [Template]))}.

%% @doc Enhanced template matching using router's template system
template_matches(Event, Template, _Opts) when is_map(Template) ->
    % Use message structure matching (similar to dev_router:template_matches)
    case hb_message:match(Template, Event, primary) of
        {value_mismatch, _Key, _Val1, _Val2} ->
            false;
        true ->
            true;
        _Other ->
            false
    end;
template_matches(Event, {compiled_regex, CompiledRegex, _OriginalPattern}, Opts) ->
    % Use pre-compiled regex for efficient path matching
    EventPath =
        case hb_ao:get(<<"path">>, Event, Opts) of
            not_found ->
                <<"/">>;
            Path ->
                Path
        end,
    try
        case re:run(EventPath, CompiledRegex) of
            nomatch ->
                false;
            _ ->
                true
        end
    catch
        _:_ ->
            false
    end;
template_matches(Event, Template, Opts) when is_binary(Template) ->
    % Fallback for legacy binary templates (compile on-the-fly)
    EventPath =
        case hb_ao:get(<<"path">>, Event, Opts) of
            not_found ->
                <<"/">>;
            Path ->
                Path
        end,
    try
        case re:run(EventPath, Template) of
            nomatch ->
                false;
            _ ->
                true
        end
    catch
        _:_ ->
            false
    end;
template_matches(_Event, _Template, _Opts) ->
    false.

%% ============================================================================
%% Notification Manager Process
%% ============================================================================

%% @doc Start the long-running notification manager process
start_notification_manager() ->
    case hb_name:lookup(?NOTIFICATION_MANAGER) of
        undefined ->
            % Start the manager process
            ManagerPid =
                spawn_link(fun() ->
                              case hb_name:register(?NOTIFICATION_MANAGER) of
                                  ok ->
                                      % Initialize ETS table for fast listener lookups
                                      ets:new(notification_listeners,
                                              [named_table, public, set, {read_concurrency, true}]),
                                      ?event(notify, {notification_manager_started, self()}),
                                      notification_manager_loop(#{});
                                  error ->
                                      % Someone else registered already, exit quietly
                                      exit(already_registered)
                              end
                           end),
            % Give the process a moment to register
            timer:sleep(10),
            % Check if it actually got registered
            case hb_name:lookup(?NOTIFICATION_MANAGER) of
                ManagerPid ->
                    {ok, ManagerPid};
                OtherPid when is_pid(OtherPid) ->
                    {already_started, OtherPid};
                undefined ->
                    {error, registration_failed}
            end;
        Pid ->
            {already_started, Pid}
    end.

%% @doc Stop the notification manager process
stop_notification_manager() ->
    case hb_name:lookup(?NOTIFICATION_MANAGER) of
        undefined ->
            ok;
        Pid ->
            % Send stop signal
            Pid ! stop,
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
            % Register a new listener with template using new key structure
            % Key is {{Template, StreamPid}, Ref} to ensure deduplication by {Template, StreamPid}
            ets:insert(notification_listeners, {{Template, StreamPid}, Ref}),
            ?event(notify,
                   {listener_registered, {template, Template}, {stream, StreamPid}, {ref, Ref}}),
            notification_manager_loop(State);
        {unregister_listener, Ref} ->
            % Remove listener by reference using new key structure
            % Key structure is {{Template, StreamPid}, Ref}
            ets:match_delete(notification_listeners, {{'_', '_'}, Ref}),
            ?event(notify, {listener_unregistered, {ref, Ref}}),
            notification_manager_loop(State);
        {get_stats} ->
            % Return statistics about registered listeners
            Stats =
                #{total_listeners => ets:info(notification_listeners, size),
                  manager_pid => self(),
                  state => State},
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
    try
        % Use ets:foldl instead of tab2list to avoid copying entire table
        % This eliminates memory allocation overhead for large listener tables
        ets:foldl(fun({{Template, StreamPid}, Ref}, Acc) ->
                     % Spawn short-lived worker processes for actual dispatching
                     % This keeps the manager process responsive
                     spawn(fun() -> try_dispatch_to_listener(Template, StreamPid, Ref, Event, Opts)
                           end),
                     Acc
                  end,
                  ok,
                  notification_listeners)
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
                        % Clean up dead process using new key structure
                        ets:match_delete(notification_listeners, {{'_', StreamPid}, '_'}),
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
    MapTemplate = #{<<"device">> => <<"test">>},
    MapResult = validate_template(MapTemplate, #{}),
    ?event(debug, {map_template_validation, {input, MapTemplate}, {result, MapResult}}),
    ?assertEqual({ok, #{<<"device">> => <<"test">>}}, MapResult),

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
    MapTemplate = #{<<"device">> => <<"process@1.0">>},

    Event1 = #{<<"device">> => <<"process@1.0">>, <<"path">> => <<"/compute">>},
    Match1 = template_matches(Event1, MapTemplate, #{}),
    ?event(debug,
           {map_template_match, {template, MapTemplate}, {event, Event1}, {result, Match1}}),
    ?assertEqual(true, Match1),

    Event2 = #{<<"device">> => <<"message@1.0">>, <<"path">> => <<"/compute">>},
    Match2 = template_matches(Event2, MapTemplate, #{}),
    ?event(debug,
           {map_template_no_match, {template, MapTemplate}, {event, Event2}, {result, Match2}}),
    ?assertEqual(false, Match2),

    % Regex template matching
    RegexTemplate = <<"/.*process.*/.*">>,

    Event3 = #{<<"path">> => <<"/test/process@1.0/compute">>},
    Match3 = template_matches(Event3, RegexTemplate, #{}),
    ?event(debug,
           {regex_template_match, {template, RegexTemplate}, {event, Event3}, {result, Match3}}),
    ?assertEqual(true, Match3),

    Event4 = #{<<"path">> => <<"/test/message@1.0/compute">>},
    Match4 = template_matches(Event4, RegexTemplate, #{}),
    ?event(debug,
           {regex_template_no_match, {template, RegexTemplate}, {event, Event4}, {result, Match4}}),
    ?assertEqual(false, Match4).

%% @doc Test notification manager process lifecycle
notification_manager_test() ->
    % Ensure clean state
    InitialState = hb_name:lookup(?NOTIFICATION_MANAGER),
    ?event(debug, {manager_initial_state, InitialState}),
    stop_notification_manager(),
    timer:sleep(50),

    % Start manager
    StartResult = start_notification_manager(),
    ?event(debug, {manager_start_result, StartResult}),
    {ok, ManagerPid} = StartResult,
    ?assert(is_process_alive(ManagerPid)),
    ?assertEqual(ManagerPid, hb_name:lookup(?NOTIFICATION_MANAGER)),
    ?event(debug, {manager_started_successfully, {pid, ManagerPid}}),

    % Stop manager
    ?event(debug, {stopping_manager}),
    stop_notification_manager(),
    % Wait a bit for cleanup
    timer:sleep(100),
    FinalState = hb_name:lookup(?NOTIFICATION_MANAGER),
    ?event(debug, {manager_final_state, FinalState}),
    ?assertEqual(undefined, FinalState).

%% @doc Test event dispatching with template matching
event_dispatch_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),

    start_notification_manager(),
    ManagerPid = hb_name:lookup(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),

    % Create a test stream process
    TestPid = self(),
    StreamPid =
        spawn(fun() ->
                 receive
                     {notify_event, Event} -> TestPid ! {received_event, Event}
                 after 1000 -> TestPid ! timeout
                 end
              end),

    % Register listener with map template
    MapTemplate = #{<<"device">> => <<"process@1.0">>},
    Ref1 = make_ref(),
    ?event(debug,
           {registering_listener, {template, MapTemplate}, {stream_pid, StreamPid}, {ref, Ref1}}),
    ManagerPid ! {register_listener, MapTemplate, StreamPid, Ref1},

    % Create test events
    MatchingEvent =
        #{<<"device">> => <<"process@1.0">>,
          <<"action">> => <<"compute">>,
          <<"data">> => <<"test">>},

    NonMatchingEvent = #{<<"device">> => <<"message@1.0">>, <<"action">> => <<"compute">>},

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

%% @doc Test listener registration and unregistration
listener_registration_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),

    % Start manager
    start_notification_manager(),
    ManagerPid = hb_name:lookup(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),

    % Register a listener
    Template = #{<<"device">> => <<"message@1.0">>},
    StreamPid = spawn(fun() -> receive _ -> ok end end),
    Ref = make_ref(),

    ManagerPid ! {register_listener, Template, StreamPid, Ref},
    timer:sleep(10), % Allow registration to process

    % Check listener is registered
    AllListeners = ets:tab2list(notification_listeners),
    ?assert(lists:member({{Template, StreamPid}, Ref}, AllListeners)),

    % Unregister listener
    ManagerPid ! {unregister_listener, Ref},
    timer:sleep(10), % Allow unregistration to process

    % Check listener is removed
    AllListeners2 = ets:tab2list(notification_listeners),
    ?assertNot(lists:member({{Template, StreamPid}, Ref}, AllListeners2)),

    stop_notification_manager().

%% @doc Test regex template matching in event dispatch
regex_dispatch_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),

    start_notification_manager(),
    ManagerPid = hb_name:lookup(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),

    % Create test stream process
    TestPid = self(),
    StreamPid =
        spawn(fun() ->
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
    MatchingEvent = #{<<"path">> => <<"/test/process@1.0/compute">>},
    ManagerPid ! {dispatch_event, MatchingEvent, #{}},

    receive
        {received_event, ReceivedEvent} ->
            ?assertEqual(MatchingEvent, ReceivedEvent)
    after 500 ->
        ?assert(false, "Should have received matching event")
    end,

    % Test non-matching path
    NonMatchingEvent = #{<<"path">> => <<"/test/message@1.0/compute">>},
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

    % Test dispatch with valid input
    InputMsg =
        #{<<"event">> => #{<<"test">> => <<"data">>},
          <<"timestamp">> => erlang:system_time(millisecond)},

    % Test with no notify device configured (should return appropriate message)
    OptsWithoutNotify = #{notify_device => undefined},
    Result1 = dispatch(#{}, InputMsg, OptsWithoutNotify),
    ?assertEqual({ok, <<"No notify device configured">>}, Result1),

    % Test with notify device configured but no manager running (should return error)
    OptsWithNotify = #{notify_device => <<"notify@1.0">>},
    Result2 = dispatch(#{}, InputMsg, OptsWithNotify),
    ?assertEqual({error, <<"Notification manager not available">>}, Result2),

    % Start manager and test successful dispatch
    start_notification_manager(),
    Result3 = dispatch(#{}, InputMsg, OptsWithNotify),
    ?assertEqual({ok, <<"OK">>}, Result3),

    stop_notification_manager().

%% @doc Test stream function
stream_function_test() ->
    % Test stream creation with valid template
    InputMsg1 = #{<<"template">> => #{<<"device">> => <<"test@1.0">>}},

    {ok, Result1} = stream(#{}, InputMsg1, #{}),
    ?assertEqual(200, maps:get(<<"status">>, Result1)),
    ?assertEqual(true, maps:get(<<"streaming">>, Result1)),
    ?assert(maps:is_key(<<"headers">>, Result1)),
    ?assert(maps:is_key(<<"body">>, Result1)),

    % Test with regex template
    InputMsg2 = #{<<"template">> => <<"/.*test.*/.*">>},

    {ok, Result2} = stream(#{}, InputMsg2, #{}),
    ?assertEqual(200, maps:get(<<"status">>, Result2)).

%% @doc Test dead process cleanup
dead_process_cleanup_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),

    start_notification_manager(),
    ManagerPid = hb_name:lookup(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),

    % Create and register a process, then kill it
    DeadPid = spawn(fun() -> ok end),
    timer:sleep(10), % Ensure process dies
    ?assertNot(is_process_alive(DeadPid)),
    ?event(debug, {created_dead_process, {pid, DeadPid}, {alive, is_process_alive(DeadPid)}}),

    Template = #{<<"device">> => <<"message@1.0">>},
    Ref = make_ref(),

    % Register the dead process
    ?event(debug, {registering_dead_process, {template, Template}, {dead_pid, DeadPid}}),
    ManagerPid ! {register_listener, Template, DeadPid, Ref},
    timer:sleep(10),

    % Verify it's in the table
    AllListeners = ets:tab2list(notification_listeners),
    ?event(debug,
           {listeners_before_cleanup,
            {count, length(AllListeners)},
            {has_dead_process, lists:member({{Template, DeadPid}, Ref}, AllListeners)}}),
    ?assert(lists:member({{Template, DeadPid}, Ref}, AllListeners)),

    % Dispatch an event that matches
    Event = #{<<"device">> => <<"message@1.0">>},
    ?event(debug, {dispatching_to_dead_process, Event}),
    ManagerPid ! {dispatch_event, Event, #{}},
    timer:sleep(50), % Allow cleanup to happen

    % Dead process should be cleaned up
    AllListeners2 = ets:tab2list(notification_listeners),
    ?event(debug,
           {listeners_after_cleanup,
            {count, length(AllListeners2)},
            {dead_process_removed, not lists:member({{Template, DeadPid}, Ref}, AllListeners2)}}),
    ?assertNot(lists:member({{Template, DeadPid}, Ref}, AllListeners2)),

    stop_notification_manager().

%% @doc Test concurrent dispatching
concurrent_dispatch_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),

    start_notification_manager(),
    ManagerPid = hb_name:lookup(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),

    % Create multiple test processes
    TestPid = self(),
    NumStreams = 5,

    StreamPids =
        lists:map(fun(N) ->
                     spawn(fun() ->
                              receive
                                  {notify_event, Event} -> TestPid ! {received_event, N, Event}
                              after 1000 -> TestPid ! {timeout, N}
                              end
                           end)
                  end,
                  lists:seq(1, NumStreams)),

    % Register all with the same template
    Template = #{<<"type">> => <<"broadcast">>},
    lists:foreach(fun({_N, Pid}) ->
                     ManagerPid ! {register_listener, Template, Pid, make_ref()}
                  end,
                  lists:zip(
                      lists:seq(1, NumStreams), StreamPids)),

    timer:sleep(20), % Allow registrations to process

    % Dispatch one event
    Event = #{<<"type">> => <<"broadcast">>, <<"message">> => <<"hello">>},
    ManagerPid ! {dispatch_event, Event, #{}},

    % All streams should receive the event
    ReceivedCount = receive_events(NumStreams, 0),
    ?assertEqual(NumStreams, ReceivedCount),

    stop_notification_manager().

%% Helper function for concurrent test
receive_events(0, Count) ->
    Count;
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
    NestedTemplate =
        #{<<"device">> => <<"test@1.0">>,
          <<"data">> => #{<<"nested">> => #{<<"value">> => <<"deep">>}}},
    ?assertEqual({ok, NestedTemplate}, validate_template(NestedTemplate, #{})),

    % Test complex regex patterns
    ComplexRegex = <<"^/(test|prod)/process@[0-9]+\\.[0-9]+/.+$">>,
    ?assertMatch({ok, {compiled_regex, _, ComplexRegex}},
                 validate_template(ComplexRegex, #{})),

    % Test unicode in templates
    UnicodeTemplate = #{<<"message">> => <<"Hello 世界">>},
    ?assertEqual({ok, UnicodeTemplate}, validate_template(UnicodeTemplate, #{})),

    % Test very long regex
    LongRegex = iolist_to_binary(lists:duplicate(1000, "a")),
    ?assertMatch({ok, {compiled_regex, _, LongRegex}}, validate_template(LongRegex, #{})).

%% @doc Test message matching edge cases
message_matching_edge_cases_test() ->
    % Test partial map matching
    Template = #{<<"device">> => <<"message@1.0">>},
    Event =
        #{<<"device">> => <<"message@1.0">>,
          <<"extra">> => <<"field">>,
          <<"more">> => #{<<"nested">> => <<"data">>}},
    ?assertEqual(true, template_matches(Event, Template, #{})),

    % Test case sensitivity
    CaseTemplate = #{<<"Device">> => <<"Message@1.0">>},
    CaseEvent = #{<<"device">> => <<"message@1.0">>},
    ?assertEqual(false, template_matches(CaseEvent, CaseTemplate, #{})),

    % Test missing path in regex matching
    RegexTemplate = <<"/test.*">>,
    EventWithoutPath = #{<<"device">> => <<"message@1.0">>},
    ?assertEqual(false, template_matches(EventWithoutPath, RegexTemplate, #{})),

    % Test empty path
    EventWithEmptyPath = #{<<"path">> => <<"">>},
    ?assertEqual(false, template_matches(EventWithEmptyPath, RegexTemplate, #{})).

%% @doc Test error handling and recovery
error_handling_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),

    start_notification_manager(),
    ManagerPid = hb_name:lookup(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),

    % Test invalid template in dispatch (should not crash manager)
    InvalidTemplate = invalid_template_atom,
    TestPid = spawn(fun() -> receive _ -> ok end end),
    Ref = make_ref(),

    % Register invalid template (insert directly to bypass validation)
    ets:insert(notification_listeners, {{InvalidTemplate, TestPid}, Ref}),

    % Dispatch event - should handle error gracefully
    Event = #{<<"test">> => <<"data">>},
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
    ManagerPid = hb_name:lookup(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),

    % Register a listener
    Template = #{<<"type">> => <<"perf-test">>},
    TestPid = spawn(fun() -> receive_loop(0) end),
    Ref = make_ref(),
    ManagerPid ! {register_listener, Template, TestPid, Ref},
    timer:sleep(10),

    % Send many events quickly
    NumEvents = 100,
    StartTime = erlang:system_time(microsecond),

    lists:foreach(fun(N) ->
                     Event =
                         #{<<"type">> => <<"perf-test">>,
                           <<"sequence">> => N,
                           <<"timestamp">> => erlang:system_time(millisecond)},
                     ManagerPid ! {dispatch_event, Event, #{}}
                  end,
                  lists:seq(1, NumEvents)),

    EndTime = erlang:system_time(microsecond),
    Duration = EndTime - StartTime,

    ?event(notify, {performance_test, {events, NumEvents}, {duration_us, Duration}}),

    % Should handle 100 events quickly (< 100ms)
    ?assert(Duration < 100000, "Performance test took too long"),

    stop_notification_manager().

%% @doc Test duplicate listener registration bug
duplicate_listener_registration_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),

    start_notification_manager(),
    ManagerPid = hb_name:lookup(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),

    % Create test stream process that counts events
    TestPid = self(),
    StreamPid = spawn(fun() -> duplicate_event_counter(0) end),

    % Register the same template and stream multiple times
    Template = #{<<"device">> => <<"duplicate-test@1.0">>},
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    Ref3 = make_ref(),

    ?event(debug,
           {registering_duplicate_listeners, {template, Template}, {stream_pid, StreamPid}}),
    ManagerPid ! {register_listener, Template, StreamPid, Ref1},
    ManagerPid ! {register_listener, Template, StreamPid, Ref2},
    ManagerPid ! {register_listener, Template, StreamPid, Ref3},
    timer:sleep(20), % Allow registrations to process

    % Check how many entries are in the ETS table
    AllListeners = ets:tab2list(notification_listeners),
    MatchingListeners =
        [L || {{T, P}, _R} = L <- AllListeners, T =:= Template, P =:= StreamPid],
    ?event(debug,
           {duplicate_registrations_found,
            {count, length(MatchingListeners)},
            {entries, MatchingListeners}}),

    % This test will FAIL with current implementation - showing the bug
    % The same {Template, StreamPid} should only be registered once
    ?assertEqual(1,
                 length(MatchingListeners),
                 "Same template/stream should only be registered once"),

    % Dispatch one event
    Event = #{<<"device">> => <<"duplicate-test@1.0">>, <<"message">> => <<"test">>},
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
    ManagerPid = hb_name:lookup(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),

    % Scenario 1: Same template, same stream, different refs
    Template1 = #{<<"type">> => <<"scenario1">>},
    Stream1 = spawn(fun() -> receive _ -> ok end end),

    ManagerPid ! {register_listener, Template1, Stream1, make_ref()},
    ManagerPid ! {register_listener, Template1, Stream1, make_ref()},
    timer:sleep(10),

    Listeners1 =
        [L
         || {{T, P}, _R} = L <- ets:tab2list(notification_listeners),
            T =:= Template1,
            P =:= Stream1],
    ?event(debug, {scenario1_duplicates, {count, length(Listeners1)}}),
    % Same template/stream should only be registered once
    ?assertEqual(1,
                 length(Listeners1),
                 "Same template/stream should only be registered once"),

    % Scenario 2: Same template, different streams (should be allowed)
    Template2 = #{<<"type">> => <<"scenario2">>},
    Stream2A = spawn(fun() -> receive _ -> ok end end),
    Stream2B = spawn(fun() -> receive _ -> ok end end),

    ManagerPid ! {register_listener, Template2, Stream2A, make_ref()},
    ManagerPid ! {register_listener, Template2, Stream2B, make_ref()},
    timer:sleep(10),

    Listeners2 =
        [L || {{T, _P}, _R} = L <- ets:tab2list(notification_listeners), T =:= Template2],
    ?event(debug, {scenario2_different_streams, {count, length(Listeners2)}}),
    ?assertEqual(2,
                 length(Listeners2),
                 "Different streams with same template should be allowed"),

    % Scenario 3: Different templates, same stream (should be allowed)
    Template3A = #{<<"type">> => <<"scenario3a">>},
    Template3B = #{<<"type">> => <<"scenario3b">>},
    Stream3 = spawn(fun() -> receive _ -> ok end end),

    ManagerPid ! {register_listener, Template3A, Stream3, make_ref()},
    ManagerPid ! {register_listener, Template3B, Stream3, make_ref()},
    timer:sleep(10),

    Listeners3 =
        [L || {{_T, P}, _R} = L <- ets:tab2list(notification_listeners), P =:= Stream3],
    ?event(debug, {scenario3_different_templates, {count, length(Listeners3)}}),
    ?assertEqual(2,
                 length(Listeners3),
                 "Same stream with different templates should be allowed"),

    stop_notification_manager().

%% @doc Test event duplication with multiple registrations
event_duplication_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),

    start_notification_manager(),
    ManagerPid = hb_name:lookup(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),

    % Create counting process
    TestPid = self(),
    CounterPid = spawn(fun() -> event_duplication_counter(0, TestPid) end),

    % Register the same listener 3 times (showing the bug)
    Template = #{<<"event">> => <<"duplication-test">>},
    ManagerPid ! {register_listener, Template, CounterPid, make_ref()},
    ManagerPid ! {register_listener, Template, CounterPid, make_ref()},
    ManagerPid ! {register_listener, Template, CounterPid, make_ref()},
    timer:sleep(20),

    % Dispatch one event
    Event = #{<<"event">> => <<"duplication-test">>, <<"data">> => <<"single event">>},
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

%% @doc Test manual notification manager startup
notification_manager_manual_start_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(100),

    % Verify no manager is running
    ?assertEqual(undefined, hb_name:lookup(?NOTIFICATION_MANAGER)),

    % Start manager manually
    {ok, ManagerPid} = start_notification_manager(),

    % Verify manager started successfully
    ?assertNotEqual(undefined, ManagerPid),
    ?assert(is_process_alive(ManagerPid)),
    ?assertEqual(ManagerPid, hb_name:lookup(?NOTIFICATION_MANAGER)),

    ?event(debug, {manual_start_test_completed, {manager_pid, ManagerPid}}),

    stop_notification_manager().

%% @doc Test multiple start attempts return already_started
notification_manager_multiple_start_test() ->
    % Clean state
    stop_notification_manager(),
    timer:sleep(50),

    % Manually start manager first
    {ok, ManualPid} = start_notification_manager(),
    ?assert(is_process_alive(ManualPid)),

    % Try to start again (should return already_started)
    StartResult = start_notification_manager(),
    ?assertEqual({already_started, ManualPid}, StartResult),

    % Should still be the same process
    CurrentPid = hb_name:lookup(?NOTIFICATION_MANAGER),
    ?assertEqual(ManualPid, CurrentPid),
    ?assert(is_process_alive(CurrentPid)),

    stop_notification_manager().

%% @doc Test manual start handles conflicts gracefully
notification_manager_manual_start_conflict_test() ->
    % Clean state thoroughly
    stop_notification_manager(),
    timer:sleep(100),

    % Ensure no process is registered
    ?assertEqual(undefined, hb_name:lookup(?NOTIFICATION_MANAGER)),

    % Create a conflicting process with the same name
    TestPid = self(),
    ConflictPid =
        spawn(fun() ->
                 case hb_name:register(?NOTIFICATION_MANAGER) of
                     ok ->
                         TestPid ! {conflict_registered, self()},
                         receive stop -> ok after 10000 -> ok end;
                     error -> TestPid ! {conflict_failed, self()}
                 end
              end),

    % Wait for registration result
    receive
        {conflict_registered, ConflictPid} ->
            % Verify the conflict process is registered
            ?assertEqual(ConflictPid, hb_name:lookup(?NOTIFICATION_MANAGER)),

            % Try manual start (should return already_started)
            StartResult = start_notification_manager(),
            ?assertEqual({already_started, ConflictPid}, StartResult),

            % The original conflict process should still be there
            ?assertEqual(ConflictPid, hb_name:lookup(?NOTIFICATION_MANAGER)),

            % Clean up
            ConflictPid ! stop,
            timer:sleep(50);
        {conflict_failed, ConflictPid} ->
            % Name was already taken, just start normally
            {ok, _} = start_notification_manager()
    after 1000 ->
        ?assert(false, "Conflict process failed to register")
    end,

    % Ensure clean state
    stop_notification_manager(),

    % Now start should work
    {ok, _NewPid} = start_notification_manager(),
    stop_notification_manager().

%% @doc Test manager restart after stop/start cycle
notification_manager_manager_restart_test() ->
    % Test simulates stopping and restarting the manager
    stop_notification_manager(),
    timer:sleep(50),

    % Verify clean state
    ?assertEqual(undefined, hb_name:lookup(?NOTIFICATION_MANAGER)),

    % Start the manager manually
    {ok, FirstPid} = start_notification_manager(),
    timer:sleep(50),

    % Verify manager started
    ?assertEqual(FirstPid, hb_name:lookup(?NOTIFICATION_MANAGER)),
    ?assertNotEqual(undefined, FirstPid),
    ?assert(is_process_alive(FirstPid)),

    % Register a test listener to verify functionality
    TestTemplate = #{<<"test">> => <<"restart">>},
    TestStreamPid = spawn(fun() -> receive _ -> ok end end),
    TestRef = make_ref(),

    FirstPid ! {register_listener, TestTemplate, TestStreamPid, TestRef},
    timer:sleep(10),

    % Verify listener is registered
    Listeners = ets:tab2list(notification_listeners),
    ?assert(lists:member({{TestTemplate, TestStreamPid}, TestRef}, Listeners)),

    % Stop manager and restart
    stop_notification_manager(),
    timer:sleep(50),

    % Start again
    {ok, SecondPid} = start_notification_manager(),
    timer:sleep(50),

    % Verify new manager started (should be different PID)
    ?assertEqual(SecondPid, hb_name:lookup(?NOTIFICATION_MANAGER)),
    ?assertNotEqual(undefined, SecondPid),
    ?assert(is_process_alive(SecondPid)),
    ?assertNotEqual(FirstPid, SecondPid),

    % ETS table should be recreated (empty)
    NewListeners = ets:tab2list(notification_listeners),
    ?assertEqual([], NewListeners),

    stop_notification_manager().

%% @doc Test ETS table initialization and ownership
notification_manager_ets_initialization_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),

    % Start manager manually
    {ok, ManagerPid} = start_notification_manager(),
    timer:sleep(50),

    ?assertNotEqual(undefined, ManagerPid),
    ?assert(is_process_alive(ManagerPid)),

    % Verify ETS table exists and has correct properties
    TableInfo = ets:info(notification_listeners),
    ?assertNotEqual(undefined, TableInfo),

    % Verify table properties
    ?assertEqual(set, ets:info(notification_listeners, type)),
    ?assertEqual(true, ets:info(notification_listeners, named_table)),

    % Most importantly: verify the ETS table is owned by the manager process
    % This is the critical fix - table should be owned by manager, not caller
    ETSOwner = ets:info(notification_listeners, owner),
    ?assertEqual(ManagerPid, ETSOwner),

    % Test basic ETS operations work
    InitialSize = ets:info(notification_listeners, size),
    TestKey = {{test_template, test_pid}, test_ref},
    ets:insert(notification_listeners, TestKey),
    ?assertEqual(InitialSize + 1, ets:info(notification_listeners, size)),
    ?assert(ets:member(notification_listeners, element(1, TestKey))),

    % Clean up test data
    ets:delete(notification_listeners, element(1, TestKey)),
    ?assertEqual(InitialSize, ets:info(notification_listeners, size)).

%% @doc Test concurrent manager start attempts
notification_manager_concurrent_start_test() ->
    % Clean state
    stop_notification_manager(),
    timer:sleep(50),

    TestPid = self(),
    NumConcurrentCalls = 5,

    % Spawn multiple processes that all try to start the manager
    ConcurrentPids =
        lists:map(fun(N) ->
                     spawn(fun() ->
                              % Random small delay to increase chance of race conditions
                              timer:sleep(
                                  rand:uniform(50)),
                              Result = start_notification_manager(),
                              ManagerPid = hb_name:lookup(?NOTIFICATION_MANAGER),
                              TestPid ! {concurrent_result, N, Result, ManagerPid}
                           end)
                  end,
                  lists:seq(1, NumConcurrentCalls)),

    % Collect all results
    Results =
        lists:map(fun(_) ->
                     receive
                         {concurrent_result, N, Result, ManagerPid} -> {N, Result, ManagerPid}
                     after 5000 -> {timeout, timeout, undefined}
                     end
                  end,
                  ConcurrentPids),

    ?event(debug, {concurrent_start_results, Results}),

    % Should get one {ok, Pid} and rest {already_started, Pid}
    OkResults = [R || {_N, R, _Pid} <- Results, element(1, R) =:= ok],
    AlreadyStartedResults =
        [R || {_N, R, _Pid} <- Results, element(1, R) =:= already_started],

    % Should have exactly one ok result and rest already_started
    ?assertEqual(1, length(OkResults)),
    ?assertEqual(NumConcurrentCalls - 1, length(AlreadyStartedResults)),

    % All should see the same manager PID
    ManagerPids = [Pid || {_N, _Result, Pid} <- Results, Pid =/= undefined],
    case ManagerPids of
        [] ->
            ?assert(false, "No manager PIDs found");
        [FirstPid | Rest] ->
            % All should be the same PID
            lists:foreach(fun(Pid) -> ?assertEqual(FirstPid, Pid) end, Rest)
    end,

    stop_notification_manager().

%% @doc Test that the start_manager hook function works correctly
start_manager_hook_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),

    % Verify manager is not running
    ?assertEqual(undefined, hb_name:lookup(?NOTIFICATION_MANAGER)),

    % Test with notify_device undefined (should not start manager)
    HookMsg1 = #{<<"body">> => #{notify_device => undefined}},
    {ok, HookMsg1} = start_manager(#{}, HookMsg1, #{}),
    ?assertEqual(undefined, hb_name:lookup(?NOTIFICATION_MANAGER)),

    % Test with notify_device configured (should start manager)
    HookMsg2 = #{<<"body">> => #{notify_device => <<"notify@1.0">>}},
    {ok, UpdatedHookMsg} = start_manager(#{}, HookMsg2, #{}),
    ManagerPid = hb_name:lookup(?NOTIFICATION_MANAGER),
    ?assertNotEqual(undefined, ManagerPid),
    ?assert(is_process_alive(ManagerPid)),
    
    % Verify that the hook message was updated with on-notify registration
    UpdatedBody = maps:get(<<"body">>, UpdatedHookMsg),
    OnHooks = maps:get(on, UpdatedBody),
    ?assert(maps:is_key(<<"on-notify">>, OnHooks)),

    % Test idempotent behavior (calling again should not create new manager)
    {ok, _UpdatedHookMsg2} = start_manager(#{}, UpdatedHookMsg, #{}),
    ManagerPid2 = hb_name:lookup(?NOTIFICATION_MANAGER),
    ?assertEqual(ManagerPid, ManagerPid2), % Same PID

    stop_notification_manager().


%% @doc Benchmark test to measure handle_event_dispatch performance directly
handle_event_dispatch_benchmark_test() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),

    start_notification_manager(),
    ManagerPid = hb_name:lookup(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),

    % Register many listeners to create realistic load
    NumListeners = 10000,
    TestEvent =
        #{<<"service">> => <<"benchmark">>,
          <<"action">> => <<"test">>,
          <<"data">> => <<"benchmark_payload">>},

    % Create listeners with different templates for realistic scenario
    lists:foreach(fun(N) ->
                     Template =
                         case N rem 4 of
                             0 -> #{<<"service">> => <<"benchmark">>};  % Will match
                             1 -> #{<<"service">> => <<"other">>};      % Won't match
                             2 -> #{<<"action">> => <<"test">>};        % Will match
                             3 -> #{<<"type">> => <<"different">>}      % Won't match
                         end,
                     StreamPid = spawn(fun() -> receive stop -> ok after 10000 -> ok end end),
                     Ref = make_ref(),
                     ManagerPid ! {register_listener, Template, StreamPid, Ref}
                  end,
                  lists:seq(1, NumListeners)),

    timer:sleep(100), % Allow registrations to process

    % Verify listeners are registered
    ListenerCount = ets:info(notification_listeners, size),
    ?event(notify, {benchmark_setup, {listeners_registered, ListenerCount}}),
    ?assertEqual(NumListeners, ListenerCount),

    % Benchmark current handle_event_dispatch implementation
    NumIterations = 100,
    Times =
        lists:map(fun(_) ->
                     StartTime = erlang:system_time(microsecond),

                     % Call handle_event_dispatch directly (this is what we're optimizing)
                     handle_event_dispatch(TestEvent, #{}),

                     EndTime = erlang:system_time(microsecond),
                     EndTime - StartTime
                  end,
                  lists:seq(1, NumIterations)),

    % Calculate statistics
    TotalTime = lists:sum(Times),
    MinTime = lists:min(Times),
    MaxTime = lists:max(Times),
    AvgTime = TotalTime div NumIterations,

    % Calculate how many processes were spawned per call
    % (This will be listeners that match × 1 since we spawn one process per match)
    MatchingListeners = NumListeners div 2, % Roughly half should match our test event
    ProcessesPerCall = MatchingListeners,
    TotalProcessesSpawned = ProcessesPerCall * NumIterations,

    ?event(notify_benchmark,
           {handle_event_dispatch_benchmark,
            {listeners, NumListeners},
            {matching_listeners_estimate, MatchingListeners},
            {iterations, NumIterations},
            {total_time_us, TotalTime},
            {avg_time_us, AvgTime},
            {min_time_us, MinTime},
            {max_time_us, MaxTime},
            {processes_spawned_per_call, ProcessesPerCall},
            {total_processes_spawned, TotalProcessesSpawned},
            {calls_per_second, NumIterations * 1000000 div TotalTime}}),

    % Performance assertions
    ?assert(AvgTime > 0, "Benchmark should take measurable time"),
    ?assert(TotalProcessesSpawned > 0, "Should spawn processes for matching listeners"),

    % Log current performance baseline for comparison with foldl implementation
    EventsPerSecond = NumIterations * 1000000 div TotalTime,
    MicrosecondsPerEvent = AvgTime,

    ?event(notify_benchmark,
           {current_tab2list_baseline,
            {events_per_second, EventsPerSecond},
            {microseconds_per_event, MicrosecondsPerEvent},
            {microseconds_per_listener, AvgTime div NumListeners},
            {table_copying_overhead, "tab2list_copies_entire_table"}}),

    stop_notification_manager().

%% @doc Test handle_event_dispatch performance under different listener loads
scaling_benchmark_test() ->
    % Test how performance degrades as listener count increases
    ListenerCounts = [100, 500, 1000, 2000, 10000],
    Results =
        lists:map(fun(NumListeners) -> benchmark_with_listener_count(NumListeners) end,
                  ListenerCounts),

    ?event(notify_benchmark, {scaling_benchmark_results, Results}),

    % Performance should degrade linearly (or better) with listener count
    % If it degrades quadratically, that's a bad sign
    lists:foreach(fun({ListenerCount, AvgTimeUs}) ->
                     % Very rough check - average time should be reasonable
                     ReasonableTimeUs = ListenerCount * 10, % 10us per listener is reasonable
                     ?assert(AvgTimeUs < ReasonableTimeUs * 2,
                             io_lib:format("Performance too slow for ~p listeners: ~pus",
                                           [ListenerCount, AvgTimeUs]))
                  end,
                  Results).

%% Helper function for scaling benchmark
benchmark_with_listener_count(NumListeners) ->
    stop_notification_manager(),
    timer:sleep(50),

    start_notification_manager(),
    ManagerPid = hb_name:lookup(?NOTIFICATION_MANAGER),

    % Register listeners
    lists:foreach(fun(N) ->
                     Template =
                         #{<<"test">> => <<"scaling">>, <<"id">> => integer_to_binary(N rem 10)},
                     StreamPid = spawn(fun() -> receive stop -> ok after 5000 -> ok end end),
                     Ref = make_ref(),
                     ManagerPid ! {register_listener, Template, StreamPid, Ref}
                  end,
                  lists:seq(1, NumListeners)),

    timer:sleep(50),

    % Benchmark handle_event_dispatch
    TestEvent = #{<<"test">> => <<"scaling">>, <<"data">> => <<"test">>},
    NumIterations = 50,

    StartTime = erlang:system_time(microsecond),
    lists:foreach(fun(_) -> handle_event_dispatch(TestEvent, #{}) end,
                  lists:seq(1, NumIterations)),
    EndTime = erlang:system_time(microsecond),

    TotalTime = EndTime - StartTime,
    AvgTime = TotalTime div NumIterations,

    stop_notification_manager(),

    {NumListeners, AvgTime}.

%% @doc Test memory pressure with large listener counts
memory_pressure_test() ->
    % Only run this test if we have sufficient memory/time
    case erlang:system_info(schedulers) of
        N when N >= 4 ->
            memory_pressure_test_impl();
        _ ->
            ?event(notify_benchmark, {memory_pressure_test_skipped, insufficient_schedulers})
    end.

memory_pressure_test_impl() ->
    % Ensure clean state
    stop_notification_manager(),
    timer:sleep(50),

    start_notification_manager(),
    ManagerPid = hb_name:lookup(?NOTIFICATION_MANAGER),
    ?assert(is_process_alive(ManagerPid)),

    % Register many listeners
    NumListeners = 5000,  % Large number to stress memory

    ?event(notify_benchmark, {memory_test_start, {registering_listeners, NumListeners}}),

    lists:foreach(fun(N) ->
                     Template =
                         #{<<"service">> => <<"memory-test">>,
                           <<"instance">> => integer_to_binary(N rem 100), % Some variety
                           <<"id">> => integer_to_binary(N)},
                     StreamPid = spawn(fun() -> receive stop -> ok after 30000 -> ok end end),
                     Ref = make_ref(),
                     ManagerPid ! {register_listener, Template, StreamPid, Ref},

                     % Add small delay every 100 registrations to avoid overwhelming
                     case N rem 100 of
                         0 -> timer:sleep(1);
                         _ -> ok
                     end
                  end,
                  lists:seq(1, NumListeners)),

    timer:sleep(200), % Allow registrations to process

    % Get memory stats before test
    MemBefore = erlang:memory(total),
    ProcessCountBefore = erlang:system_info(process_count),

    % Send events that will trigger many process spawns
    NumEvents = 5,
    StartTime = erlang:system_time(microsecond),

    lists:foreach(fun(N) ->
                     Event =
                         #{<<"service">> => <<"memory-test">>,
                           <<"event_id">> => N,
                           <<"data">> => <<"large_event_payload_to_increase_memory_pressure">>},
                     ManagerPid ! {dispatch_event, Event, #{}}
                  end,
                  lists:seq(1, NumEvents)),

    % Wait for processing (increased to allow process spawns to complete)
    timer:sleep(1000),

    EndTime = erlang:system_time(microsecond),
    Duration = EndTime - StartTime,

    % Get memory stats after test
    MemAfter = erlang:memory(total),
    ProcessCountAfter = erlang:system_info(process_count),

    % Calculate metrics
    ExpectedProcessSpawns = NumListeners * NumEvents,
    MemoryIncrease = MemAfter - MemBefore,
    ProcessIncrease = ProcessCountAfter - ProcessCountBefore,

    ?event(notify_benchmark,
           {memory_pressure_results,
            {listeners, NumListeners},
            {events, NumEvents},
            {duration_us, Duration},
            {expected_process_spawns, ExpectedProcessSpawns},
            {memory_increase_bytes, MemoryIncrease},
            {process_count_increase, ProcessIncrease},
            {memory_per_spawn_bytes,
             case ExpectedProcessSpawns of
                 0 ->
                     0;
                 _ ->
                     MemoryIncrease div ExpectedProcessSpawns
             end}}),

    % Performance should not be terrible even with many listeners
    MaxReasonableTime = 5000000, % 5 seconds
    ?assert(Duration < MaxReasonableTime, "Memory pressure test took too long"),

    stop_notification_manager().

receive_loop(Count) ->
    receive
        {notify_event, _Event} ->
            receive_loop(Count + 1)
    after 10 ->
        exit({received_count, Count})
    end.
