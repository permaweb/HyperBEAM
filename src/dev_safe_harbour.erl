%%% @doc A `not-found' hook device that asynchronously imports missing IDs
%%% from a secondary store into a node-local safe harbour store.
-module(dev_safe_harbour).
-export([info/1, enqueue/3, not_found/3, status/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ENQUEUE_TIMEOUT, 1000).
-define(STATUS_TIMEOUT, 1000).
-define(DEFAULT_RETRY_MS, 30_000).
-define(SERVER_LOOKUP_RETRIES, 100).
-define(SERVER_LOOKUP_SLEEP_MS, 10).

%% @doc Device API information.
info(_) ->
    #{
        exports => [<<"enqueue">>, <<"not-found">>, <<"status">>]
    }.

%% @doc Queue any IDs referenced by a 404-producing request for import.
not_found(_Base, HookReq, Opts) ->
    case enabled(Opts) of
        false -> {ok, HookReq};
        true ->
            explainer_page(
                do_enqueue(missing_ids(HookReq, Opts), Opts),
                Opts
            )
    end.

%% @doc Manually queue one or more IDs for import.
enqueue(_Base, Req, Opts) ->
    case missing_ids(Req, Opts) of
        [] ->
            {error, <<"No IDs to import.">>};
        IDs ->
            case do_enqueue(IDs, Opts) of
                disabled ->
                    {error, <<"Safe harbour imports are not enabled on this node.">>};
                {ok, Queued} ->
                    {ok,
                        #{
                            <<"queued">> => length(Queued),
                            <<"body">> =>
                                <<
                                    "Queued ",
                                    (hb_util:bin(length(Queued)))/binary,
                                    " IDs for import."
                                >>
                        }
                    }
            end
    end.

%% @doc Return the current importer status for the node.
status(_Base, _Req, Opts) ->
    case enabled(Opts) of
        false ->
            {error,
                #{
                    <<"body">> =>
                        <<"Safe harbour imports are not enabled on this node.">>
                }
            };
        true ->
            {ok, PID} = ensure_server_started(Opts),
            PID ! {status, self()},
            receive
                {status, ServerStatus} -> {ok, ServerStatus}
            after ?STATUS_TIMEOUT ->
                {error, <<"Timed out waiting for safe harbour status.">>}
            end
    end.

%% @doc Queue IDs for import if the feature is enabled.
do_enqueue([], _Opts) ->
    {ok, []};
do_enqueue(IDs, Opts) ->
    case enabled(Opts) of
        false -> disabled;
        true -> send({enqueue, IDs}, Opts)
    end.

send(Command, Opts) ->
    {ok, PID} = ensure_server_started(Opts),
    PID ! {Ref = make_ref(), self(), Command},
    receive
        {reply, Ref, Result} ->
            Result
    after ?ENQUEUE_TIMEOUT ->
        {error, <<"Timed out waiting for `safe-harbour@1.0` server.">>}
    end.

%% @doc Ensure the singleton importer exists for the current node.
ensure_server_started(Opts) ->
    ServerID = server_id(Opts),
    case hb_name:lookup(ServerID) of
        PID when is_pid(PID) ->
            {ok, PID};
        undefined ->
            hb_name:spawn_register(
                ServerID,
                fun() ->
                    start_server(ServerID, relevant_opts(Opts))
                end
            )
    end.

%% @doc Wait for the singleton importer to appear in the name registry.
wait_for_server(ServerID, 0) ->
    {error, {safe_harbour_start_timeout, ServerID}};
wait_for_server(ServerID, AttemptsLeft) ->
    case hb_name:lookup(ServerID) of
        PID when is_pid(PID) ->
            {ok, PID};
        undefined ->
            timer:sleep(?SERVER_LOOKUP_SLEEP_MS),
            wait_for_server(ServerID, AttemptsLeft - 1)
    end.

%% @doc Register and start the singleton importer loop.
start_server(ServerID, Opts) ->
    case hb_name:register(ServerID, self()) of
        ok ->
            ?event(safe_harbour, {started, {server_id, ServerID}}),
            server_loop(
                #{
                    config => Opts,
                    queue => queue:new(),
                    queued => #{},
                    failed => #{},
                    imported => 0,
                    failures => 0,
                    inflight => undefined
                }
            );
        error ->
            ok
    end.

%% @doc Run the importer server loop, prioritizing control messages.
server_loop(State) ->
    receive
        Message ->
            server_loop(handle_message(Message, State))
    after 0 ->
        case next_id(State) of
            empty ->
                receive
                    Message ->
                        server_loop(handle_message(Message, State))
                end;
            {ok, ID, NextState} ->
                server_loop(import_id(ID, NextState))
        end
    end.

%% @doc Handle a control message sent to the importer.
handle_message({enqueue, From, IDs, Opts}, State) ->
    {Queued, NextState} = enqueue_ids(IDs, State#{ config => Opts }),
    From ! {enqueued, Queued},
    NextState;
handle_message({status, From}, State) ->
    From ! {status, status_message(State)},
    State;
handle_message(stop, State) ->
    State;
handle_message(_Other, State) ->
    State.

%% @doc Convert the current importer state into a message.
status_message(State) ->
    #{
        <<"enabled">> => true,
        <<"queue-depth">> => queue:len(maps:get(queue, State)),
        <<"in-flight">> => maps:get(inflight, State, <<>>),
        <<"imported">> => maps:get(imported, State, 0),
        <<"failures">> => maps:get(failures, State, 0),
        <<"failed-ids">> => maps:keys(maps:get(failed, State, #{}))
    }.

%% @doc Add new IDs to the queue, respecting deduplication and retry cooldowns.
enqueue_ids(IDs, State) ->
    lists:foldl(
        fun(ID, {QueuedNow, AccState}) ->
            case should_queue(ID, AccState) of
                true ->
                    Queue = queue:in(ID, maps:get(queue, AccState)),
                    Queued = maps:put(ID, true, maps:get(queued, AccState)),
                    {
                        QueuedNow ++ [ID],
                        AccState#{
                            queue => Queue,
                            queued => Queued
                        }
                    };
                false ->
                    {QueuedNow, AccState}
            end
        end,
        {[], State},
        normalize_ids(IDs)
    ).

%% @doc Determine whether an ID should be added to the queue.
should_queue(ID, State) ->
    not maps:is_key(ID, maps:get(queued, State))
        andalso maps:get(inflight, State, undefined) =/= ID
        andalso retry_ready(ID, State).

%% @doc Check whether the retry cooldown has elapsed for a failed ID.
retry_ready(ID, State) ->
    Failed = maps:get(failed, State, #{}),
    case maps:get(ID, Failed, undefined) of
        undefined ->
            true;
        LastFailure ->
            erlang:system_time(millisecond) - LastFailure >=
                retry_ms(maps:get(config, State))
    end.

%% @doc Pop the next queued ID, if one exists.
next_id(State) ->
    case queue:out(maps:get(queue, State)) of
        {empty, _} ->
            empty;
        {{value, ID}, Queue} ->
            {
                ok,
                ID,
                State#{
                    queue => Queue,
                    queued => maps:remove(ID, maps:get(queued, State)),
                    inflight => ID
                }
            }
    end.

%% @doc Import a single ID into the target store.
import_id(ID, State) ->
    Opts = maps:get(config, State),
    ?event(safe_harbour, {importing, {id, ID}}),
    try
        case target_has_id(ID, Opts) of
            true ->
                ?event(safe_harbour, {already_present, {id, ID}}),
                State#{ inflight => undefined };
            false ->
                case hb_cache:read(ID, read_opts(Opts)) of
                    {ok, Imported} ->
                        Loaded = hb_cache:ensure_all_loaded(Imported, read_opts(Opts)),
                        case hb_cache:write(Loaded, write_opts(Opts)) of
                            {ok, _Path} ->
                                case target_has_id(ID, Opts) of
                                    true ->
                                        ?event(safe_harbour, {imported, {id, ID}}),
                                        mark_success(ID, State);
                                    false ->
                                        mark_failure(ID, State)
                                end;
                            _ ->
                                mark_failure(ID, State)
                        end;
                    _ ->
                        mark_failure(ID, State)
                end
        end
    catch
        Type:Reason:Stacktrace ->
            ?event(
                warning,
                {safe_harbour_import_failed,
                    {id, ID},
                    {type, Type},
                    {reason, Reason},
                    {stacktrace, {trace, Stacktrace}}
                }
            ),
            mark_failure(ID, State)
    end.

%% @doc Record a successful import and clear any prior failure cooldown.
mark_success(ID, State) ->
    State#{
        inflight => undefined,
        imported => maps:get(imported, State, 0) + 1,
        failed => maps:remove(ID, maps:get(failed, State, #{}))
    }.

%% @doc Record a failed import attempt and start its retry cooldown.
mark_failure(ID, State) ->
    Failed =
        maps:put(
            ID,
            erlang:system_time(millisecond),
            maps:get(failed, State, #{})
        ),
    State#{
        inflight => undefined,
        failed => Failed,
        failures => maps:get(failures, State, 0) + 1
    }.

%% @doc Check whether the target store already has the requested ID.
target_has_id(ID, Opts) ->
    case hb_cache:read(ID, write_opts(Opts)) of
        {ok, _} -> true;
        _ -> false
    end.

%% @doc Extract queueable IDs from a not-found hook request.
missing_ids(HookReq, Opts) ->
    case hb_maps:find(<<"missing">>, HookReq, Opts) of
        {ok, ID} when ?IS_ID(ID) -> [ID];
        {ok, IDs} when is_list(IDs) -> [ID || ID <- IDs, ?IS_ID(ID)];
        error -> []
    end.

%% @doc Determine whether safe harbour imports are configured.
enabled(Opts) ->
    hb_opts:get(safe_harbour_source, false, Opts) =/= false.

%% @doc Return the configured import store, accepting both spellings.
import_store(Opts) ->
    hb_opts:get(
        safe_harbor_import,
        hb_opts:get(safe_harbour_import, [], Opts),
        Opts
    ).

%% @doc Return the configured target store, defaulting to the node store.
target_store(Opts) ->
    case hb_opts:get(
        safe_harbor_store,
        hb_opts:get(safe_harbour_store, false, Opts),
        Opts
    ) of
        false ->
            hb_opts:get(store, no_viable_store, Opts);
        Store ->
            Store
    end.

%% @doc Return the configured retry cooldown in milliseconds.
retry_ms(Opts) ->
    hb_opts:get(
        safe_harbor_retry_ms,
        hb_opts:get(safe_harbour_retry_ms, ?DEFAULT_RETRY_MS, Opts),
        Opts
    ).

%% @doc Calculate the singleton server ID for the current node.
server_id(Opts) ->
    NodeID =
        hb_opts:get(
            http_server,
            hb_util:human_id(
                ar_wallet:to_address(
                    hb_opts:get(priv_wallet, hb:wallet(), Opts)
                )
            ),
            Opts
        ),
    {?MODULE, NodeID}.

%%% Tests

%% @doc Build an isolated test environment for safe harbour imports.
setup_test_env(LegacyCount) ->
    PrimaryStore =
        hb_test_utils:test_store(
            hb_store_volatile,
            <<"safe-harbour-primary">>
        ),
    ImportStore =
        hb_test_utils:test_store(
            hb_store_volatile,
            <<"safe-harbour-import">>
        ),
    hb_store:reset([PrimaryStore, ImportStore]),
    hb_store:start([PrimaryStore, ImportStore]),
    Current = #{ <<"body">> => <<"current">> },
    {ok, CurrentID} = hb_cache:write(Current, #{ store => [PrimaryStore] }),
    Legacy =
        [
            begin
                Msg =
                    #{
                        <<"body">> =>
                            <<"legacy-", (integer_to_binary(N))/binary>>
                    },
                {ok, ID} = hb_cache:write(Msg, #{ store => [ImportStore] }),
                {ID, Msg}
            end
        ||
            N <- lists:seq(1, LegacyCount)
        ],
    Node =
        hb_http_server:start_node(
            #{
                store => [PrimaryStore],
                safe_harbor_import => [ImportStore],
                safe_harbor_store => [PrimaryStore]
            }
        ),
    #{
        node => Node,
        primary_store => [PrimaryStore],
        import_store => [ImportStore],
        current => {CurrentID, Current},
        legacy => Legacy
    }.

%% @doc Wait until all requested IDs are available in the given store.
wait_until_available(IDs, Store) ->
    hb_util:wait_until(
        fun() ->
            lists:all(
                fun(ID) ->
                    case hb_cache:read(ID, #{ store => Store }) of
                        {ok, _} -> true;
                        _ -> false
                    end
                end,
                IDs
            )
        end,
        5000
    ).

%% @doc Request a set of IDs concurrently over HTTP.
request_many(Node, IDs) ->
    Parent = self(),
    [
        spawn(
            fun() ->
                Parent ! {
                    requested,
                    ID,
                    hb_http:get(
                        Node,
                        #{ <<"path">> => <<"/", ID/binary>> },
                        #{ http_only_result => false }
                    )
                }
            end
        )
    ||
        ID <- IDs
    ],
    [
        receive
            {requested, ID, Result} ->
                {ID, Result}
        end
    ||
        _ <- IDs
    ].

%% @doc Verify that a missing item is imported after the initial 404 response.
single_import_after_404_test_() ->
    {timeout, 20, fun single_import_after_404/0}.
single_import_after_404() ->
    #{
        node := Node,
        primary_store := PrimaryStore,
        current := {CurrentID, _Current},
        legacy := [{LegacyID, LegacyMsg}]
    } = setup_test_env(1),
    ?assertMatch(
        {ok, #{ <<"status">> := 200 }},
        hb_http:get(
            Node,
            #{ <<"path">> => <<"/", CurrentID/binary>> },
            #{ http_only_result => false }
        )
    ),
    ?assertMatch(
        {error, #{ <<"status">> := 404 }},
        hb_http:get(
            Node,
            #{ <<"path">> => <<"/", LegacyID/binary>> },
            #{ http_only_result => false }
        )
    ),
    ?assert(wait_until_available([LegacyID], PrimaryStore)),
    ?assertMatch(
        {ok, #{ <<"status">> := 200, <<"body">> := <<"legacy-1">> }},
        hb_http:get(
            Node,
            #{ <<"path">> => <<"/", LegacyID/binary, "/body">> },
            #{ http_only_result => false }
        )
    ),
    {ok, StoredLegacy} = hb_cache:read(LegacyID, #{ store => PrimaryStore }),
    ?assertEqual(
        hb_ao:get(<<"body">>, LegacyMsg, not_found, #{}),
        hb_ao:get(<<"body">>, StoredLegacy, not_found, #{ store => PrimaryStore })
    ).

%% @doc Verify that many missing IDs can be queued and imported together.
many_imports_after_404_test_() ->
    {timeout, 20, fun many_imports_after_404/0}.
many_imports_after_404() ->
    #{
        node := Node,
        primary_store := PrimaryStore,
        legacy := Legacy
    } = setup_test_env(8),
    LegacyIDs = [ID || {ID, _} <- Legacy],
    InitialResponses = request_many(Node, LegacyIDs),
    lists:foreach(
        fun({_ID, Result}) ->
            ?assertMatch({error, #{ <<"status">> := 404 }}, Result)
        end,
        InitialResponses
    ),
    ?assert(wait_until_available(LegacyIDs, PrimaryStore)),
    lists:foreach(
        fun({ID, Msg}) ->
            ?assertMatch(
                {ok, #{ <<"status">> := 200, <<"body">> := _ }},
                hb_http:get(
                    Node,
                    #{ <<"path">> => <<"/", ID/binary, "/body">> },
                    #{ http_only_result => false }
                )
            ),
            {ok, Stored} = hb_cache:read(ID, #{ store => PrimaryStore }),
            ?assertEqual(
                hb_ao:get(<<"body">>, Msg, not_found, #{}),
                hb_ao:get(
                    <<"body">>,
                    Stored,
                    not_found,
                    #{ store => PrimaryStore }
                )
            )
        end,
        Legacy
    ).
