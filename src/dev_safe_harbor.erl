%%% @doc A `not-found' hook device that asynchronously imports missing IDs
%%% from a secondary store into a node-local safe harbour store.
-module(dev_safe_harbor).
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

%% @doc Explain the safe harbour import process to the user.
explainer_page(_Result, _Opts) ->
    {ok,
        #{
            <<"body">> =>
                <<
                    "Your ID was not found on this node directly, but it has been",
                    " scheduled for recovery from the node's configured safe harbour ",
                    "sources. ",
                    "You can check the status of the import process by calling",
                    "`~safe-harbor@1.0/status'."
                >>
        }
    }.

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
    return_server_reply(status, Opts).

%% @doc Queue IDs for import if the feature is enabled.
do_enqueue(IDs, Opts) ->
    case enabled(Opts) of
        false -> disabled;
        true -> return_server_reply({enqueue, IDs}, Opts)
    end.

%% @doc Generalized wrapper to allow 'server-side' execution of commands granting
%% a result that can be returned to the AO-Core caller.
return_server_reply(Command, Opts) ->
    case enabled(Opts) of
        false ->
            {error,
                #{
                    <<"body">> =>
                        <<"Safe harbour imports are not enabled on this node.">>
                }
            };
        true ->
            PID = ensure_server_started(Opts),
            PID ! {request, Ref = make_ref(), self(), Command},
            receive
                {response, Ref, Result} -> Result
            after ?ENQUEUE_TIMEOUT ->
                {error, <<"Timed out waiting for `safe-harbour@1.0` server.">>}
            end
    end.

%% @doc Ensure the singleton importer exists for the current node.
ensure_server_started(Opts) ->
    ServerID = server_id(Opts),
    hb_name:singleton(
        ServerID,
        fun() -> start_server(ServerID, Opts) end
    ).

%% @doc Register and start the singleton importer loop.
start_server(ServerID, Opts) ->
    ?event(safe_harbor, {started, {server_id, ServerID}}),
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
    ).

%% @doc Run the importer server loop, prioritizing control messages between
%% user calls.
server_loop(State) ->
    receive Msg -> server_loop(handle_request(Msg, State))
    after 0 ->
        case next_id(State) of
            empty -> receive Msg -> server_loop(handle_request(Msg, State)) end;
            {ok, ID, NextState} -> server_loop(import_id(ID, NextState))
        end
    end.

%% @doc A generic framework of requests and responses to the importer.
handle_request({request, Ref, From, Command}, State) ->
    case execute(Command, State) of
        {Status, Result} ->
            From ! {response, Ref, {Status, Result}},
            State;
        {Status, Result, NextState} ->
            From ! {response, Ref, {Status, Result}},
            NextState
    end.

%% @doc Execute a command against the importer state and return the result to
%% the caller.
execute({enqueue, IDs}, State) ->
    {Queued, NextState} = enqueue_ids(IDs, State),
    {
        ok,
        #{
            <<"enqueued">> => length(Queued),
            <<"queue-length">> => queue:len(maps:get(queue, NextState)),
            <<"body">> =>
                <<
                    "Queued ",
                    (hb_util:bin(length(Queued)))/binary,
                    " IDs for import."
                >>
        },
        NextState
    };
execute(status, State) ->
    {ok, state_to_message(State), State};
execute(stop, State) ->
    {ok, stopped, State};
execute(_Other, State) ->
    {error, unknown_command, State}.

%% @doc Convert the current importer state into a message.
state_to_message(State) ->
    Config = maps:get(config, State, #{}),
    #{
        <<"enabled">> => true,
        <<"bucket-reseed-enabled">> =>
            dev_safe_harbor_chunk_bucket:enabled(Config),
        <<"import-store-enabled">> => import_store_enabled(Config),
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
        lists:map(fun hb_util:human_id/1, IDs)
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
        undefined -> true;
        LastFailure ->
            erlang:system_time(millisecond) - LastFailure >=
                retry_ms(maps:get(config, State))
    end.

%% @doc Pop the next queued ID, if one exists.
next_id(State) ->
    case queue:out(maps:get(queue, State)) of
        {empty, _} -> empty;
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
    WriteOpts = #{ store => target_store(Opts) },
    ?event(safe_harbor, {importing, {id, ID}}),
    try
        case hb_cache:read(ID, WriteOpts) of
            {ok, _} ->
                ?event(safe_harbor, {id_already_present_locally, {id, ID}}),
                State#{ inflight => undefined };
            not_found ->
                case maybe_reseed_from_bucket(ID, Opts) of
                    {ok, Result} ->
                        ?event(
                            safe_harbor,
                            {id_reseeded_from_bucket,
                                {id, ID},
                                {root_tx_id, maps:get(root_tx_id, Result)},
                                {data_root, maps:get(data_root, Result)},
                                {proofs, maps:get(proofs, Result)},
                                {chunk_posts, maps:get(chunk_posts, Result)}
                            }
                        ),
                        mark_success(ID, State);
                    {skip, Stage, Reason} ->
                        maybe_import_from_store(
                            ID,
                            Stage,
                            Reason,
                            WriteOpts,
                            State
                        )
                end
        end
    catch
        Type:CatchReason:Stacktrace ->
            ?event(
                warning,
                {safe_harbor_import_failed,
                    {id, ID},
                    {type, Type},
                    {reason, CatchReason},
                    {stacktrace, {trace, Stacktrace}}
                }
            ),
            mark_failure(ID, State)
    end.

maybe_reseed_from_bucket(ID, Opts) ->
    case dev_safe_harbor_chunk_bucket:enabled(Opts) of
        false -> {skip, disabled, not_enabled};
        true -> dev_safe_harbor_chunk_bucket:reseed(ID, Opts)
    end.

maybe_import_from_store(ID, Stage, Reason, WriteOpts, State) ->
    Opts = maps:get(config, State),
    maybe_log_bucket_skip(ID, Stage, Reason),
    case import_store_enabled(Opts) of
        false ->
            mark_failure(ID, State);
        true ->
            case hb_cache:read(ID, ReadOpts = #{ store => import_store(Opts) }) of
                {ok, Imported} ->
                    Loaded = hb_cache:ensure_all_loaded(Imported, ReadOpts),
                    case hb_cache:write(Loaded, WriteOpts) of
                        {ok, _Path} ->
                            ?event(safe_harbor, {id_imported, {id, ID}}),
                            mark_success(ID, State);
                        _ ->
                            mark_failure(ID, State)
                    end;
                _ ->
                    mark_failure(ID, State)
            end
    end.

maybe_log_bucket_skip(_ID, disabled, not_enabled) ->
    ok;
maybe_log_bucket_skip(ID, Stage, Reason) ->
    ?event(
        safe_harbor,
        {bucket_reseed_skipped,
            {id, ID},
            {stage, Stage},
            {reason, Reason}
        }
    ).

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

%% @doc Extract queueable IDs from a not-found hook request.
missing_ids(HookReq, Opts) ->
    case hb_maps:find(<<"missing">>, HookReq, Opts) of
        {ok, ID} when ?IS_ID(ID) -> [ID];
        {ok, IDs} when is_list(IDs) -> [ID || ID <- IDs, ?IS_ID(ID)];
        error ->
            case hb_maps:find(<<"request">>, HookReq, Opts) of
                {ok, Request} -> missing_ids_from_request(Request, Opts);
                error -> []
            end
    end.

%% @doc Extract queueable IDs from the request path when no explicit `missing'
%% field is present.
missing_ids_from_request(Request, Opts) ->
    case hb_path:from_message(request, Request, Opts) of
        [ID | _] when ?IS_ID(ID) -> [hb_util:human_id(ID)];
        _ -> []
    end.

%% @doc Determine whether safe harbour imports are configured.
enabled(Opts) ->
    import_store_enabled(Opts)
        orelse dev_safe_harbor_chunk_bucket:enabled(Opts).

%% @doc Determine whether a fallback import store is configured.
import_store_enabled(Opts) ->
    hb_opts:get(safe_harbor_import, [], Opts) =/= [].

%% @doc Return the configured import store, accepting both spellings.
import_store(Opts) ->
    hb_opts:get(safe_harbor_import, [], Opts).

%% @doc Return the configured target store, defaulting to the node store.
target_store(Opts) ->
    case hb_opts:get(safe_harbor_store, false, Opts) of
        false -> hb_opts:get(store, no_viable_store, Opts);
        Store -> Store
    end.

%% @doc Return the configured retry cooldown in milliseconds.
retry_ms(Opts) ->
    hb_opts:get(
        safe_harbor_retry_ms,
        hb_opts:get(safe_harbor_retry_ms, ?DEFAULT_RETRY_MS, Opts),
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
    PrimaryStore = hb_test_utils:test_store(hb_store_volatile),
    ImportStore = hb_test_utils:test_store(hb_store_volatile),
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
                        #{}
                    )
                }
            end
        )
    ||
        ID <- IDs
    ],
    [ receive {requested, ID, Result} -> {ID, Result} end || _ <- IDs ].

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
            #{}
        )
    ),
    ?assertMatch(
        {error, #{ <<"status">> := 404 }},
        hb_http:get(
            Node,
            #{ <<"path">> => <<"/", LegacyID/binary>> },
            #{}
        )
    ),
    ?assert(wait_until_available([LegacyID], PrimaryStore)),
    ?assertMatch(
        {ok, <<"legacy-1">>},
        hb_http:get(
            Node,
            #{ <<"path">> => <<"/", LegacyID/binary, "/body">> },
            #{}
        )
    ),
    {ok, StoredLegacy} = hb_cache:read(LegacyID, #{ store => PrimaryStore }),
    ?assertEqual(
        hb_ao:get(<<"body">>, LegacyMsg, not_found, #{}),
        hb_ao:get(<<"body">>, StoredLegacy, not_found, #{ store => PrimaryStore })
    ).

enabled_with_bucket_config_test() ->
    ?assert(
        enabled(
            #{
                priv_safe_harbor_bucket_endpoint => <<"http://bucket">>,
                priv_safe_harbor_bucket_access_key => <<"access">>,
                priv_safe_harbor_bucket_secret_key => <<"secret">>
            }
        )
    ).

status_message_shows_bucket_reseed_test() ->
    Status =
        state_to_message(
            #{
                config =>
                    #{
                        priv_safe_harbor_bucket_endpoint => <<"http://bucket">>,
                        priv_safe_harbor_bucket_access_key => <<"access">>,
                        priv_safe_harbor_bucket_secret_key => <<"secret">>
                    },
                queue => queue:new(),
                failed => #{},
                imported => 0,
                failures => 0
            }
        ),
    ?assertEqual(true, maps:get(<<"bucket-reseed-enabled">>, Status)).

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
        fun({N, {ID, Msg}}) ->
            ExpectedBody = <<"legacy-", (hb_util:bin(N))/binary>>,
            ?assertMatch(
                {ok, ExpectedBody},
                hb_http:get(
                    Node,
                    #{ <<"path">> => <<"/", ID/binary, "/body">> },
                    #{}
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
        hb_util:number(Legacy)
    ).
