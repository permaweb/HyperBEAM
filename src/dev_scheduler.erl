%%% @doc A simple scheduler scheme for AO.
%%% This device expects a message of the form:
%%%     Process: `#{ id, Scheduler: #{ Authority } }'
%%% <pre>
%%% It exposes the following keys for scheduling:
%%%     `#{ method: GET, path: <<"/info">> }' ->
%%%         Returns information about the scheduler.
%%%     `#{ method: GET, path: <<"/slot">> }' -> `slot(Msg1, Msg2, Opts)'
%%%         Returns the current slot for a process.
%%%     `#{ method: GET, path: <<"/schedule">> }' -> `get_schedule(Msg1, Msg2, Opts)'
%%%         Returns the schedule for a process in a cursor-traversable format.
%%%    ` #{ method: POST, path: <<"/schedule">> }' -> `post_schedule(Msg1, Msg2, Opts)'
%%%         Schedules a new message for a process, or starts a new scheduler
%%%         for the given message.
%%% </pre>

-module(dev_scheduler).
%%% AO-Core API functions:
-export([info/0]).
%%% Local scheduling functions:
-export([schedule/3, router/4, location/3]).
%%% CU-flow functions:
-export([slot/3, status/3, next/3]).
-export([start/0, checkpoint/1]).
%%% Test helper exports:
-export([test_process/0]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
%%% The maximum number of assignments that we will query/return at a time.
-define(MAX_ASSIGNMENT_QUERY_LEN, 1000).
%%% The timeout for a lookahead worker.
-define(LOOKAHEAD_TIMEOUT, 200).

%% @doc Helper to ensure that the environment is started.
start() ->
    % We need the rocksdb backend to run for hb_cache module to work
    application:ensure_all_started(hb),
    <<I1:32/unsigned-integer, I2:32/unsigned-integer, I3:32/unsigned-integer>>
        = crypto:strong_rand_bytes(12),
    rand:seed(exsplus, {I1, I2, I3}),
    ok.

%% @doc This device uses a default_handler to route requests to the correct
%% function.
info() -> 
    #{
        exports =>
            [
                location,
                status,
                next,
                schedule,
                slot,
                init,
                checkpoint
            ],
        excludes => [set, keys],
        default => fun router/4
    }.

%% @doc The default handler for the scheduler device.
router(_, Msg1, Msg2, Opts) ->
    ?event({scheduler_router_called, {msg2, Msg2}, {opts, Opts}}),
    schedule(Msg1, Msg2, Opts).

%% @doc Load the schedule for a process into the cache, then return the next
%% assignment. Assumes that Msg1 is a `dev_process' or similar message, having
%% a `Current-Slot' key. It stores a local cache of the schedule in the
%% `priv/To-Process' key.
next(Msg1, Msg2, Opts) ->
    ?event(debug_next, {scheduler_next_called, {msg1, Msg1}, {msg2, Msg2}}),
    ?event(next, started_next),
    ?event(next_profiling, started_next),
    Schedule = message_cached_assignments(Msg1, Opts),
    LastProcessed =
        hb_util:int(
            hb_ao:get(
                <<"at-slot">>,
                Msg1,
                Opts#{ hashpath => ignore }
            )
        ),
    ?event(next_profiling, got_last_processed),
    ?event(debug_next, {in_message_cache, {schedule, Schedule}}),
    ?event(next, {last_processed, LastProcessed, {message_cache, length(Schedule)}}),
    % Get the assignments from the message cache, local cache, or fetch from
    % the SU. Returns an ordered list of assignments.
    {LookaheadWorker, [NextAssignment|Assignments]} =
        case Schedule of
            [_Next|_] -> {undefined, Schedule};
            _ ->
                ProcID = dev_process:process_id(Msg1, Msg2, Opts),
                case check_lookahead_and_local_cache(Msg1, ProcID, LastProcessed + 1, Opts) of
                    {ok, Worker, Assignment} ->
                        ?event(next_debug,
                            {in_cache,
                                {slot, LastProcessed + 1},
                                {assignment, Assignment}
                            }
                        ),
                        ?event(next_profiling, read_assignment),
                        {Worker, [Assignment]};
                    not_found ->
                        {ok, RecvdAssignments} =
                            hb_ao:resolve(
                                Msg1,
                                #{
                                    <<"method">> => <<"GET">>,
                                    <<"path">> => <<"schedule/assignments">>,
                                    <<"from">> => LastProcessed
                                },
                                Opts#{ scheduler_follow_redirects => true }
                            ),
                        % Convert the assignments to an ordered list of messages,
                        % after removing all keys before the last processed slot.
                        {undefined, hb_util:message_to_ordered_list(
                            maps:filter(
                                fun(<<"priv">>, _) -> false;
                                   (<<"commitments">>, _) -> false;
                                   (Slot, _) -> hb_util:int(Slot) > LastProcessed
                                end,
                                RecvdAssignments
                            )
                        )}
                end
        end,
    ?event(next_profiling, got_assignments),
    % Paranoia: Get the slot of the next assignment, to ensure that it is the
    % last processed slot + 1.
    NextAssignmentSlot =
        try hb_util:int(
            hb_ao:get(
                <<"slot">>,
                NextAssignment,
                Opts#{ hashpath => ignore }
            )
        )
        catch
            error:badarg -> invalid_slot
        end,
    ?event(next_profiling, found_next_assignment_slot),
    ?event(debug_next, {norm_assignments, Assignments}),
    ?event(next, {assignments_to_cache, length(Assignments)}),
    % Remove assignments that are below the last processed slot.
    ?event(debug_next,
        {calculating_next_from_assignments,
            {last_processed, LastProcessed},
            {next_slot_from_assignment, NextAssignmentSlot},
            {assignments_received, length(Assignments)}
        }),
    ExpectedSlot = LastProcessed + 1,
    case NextAssignmentSlot of
        ExpectedSlot ->
            ?event(next_profiling, setting_cache),
            ?event(next, {setting_cache, {assignments, length(Assignments)}}),
            NextState =
                hb_private:set(
                    Msg1,
                    #{ <<"scheduler@1.0">> => #{
                        <<"assignments">> => Assignments,
                        <<"lookahead-worker">> => LookaheadWorker
                    }},
                    Opts
                ),
            ?event(debug_next,
                {next_returning, {slot, NextAssignmentSlot}, {message, NextAssignment}}),
            ?event(next, {next_returning, {slot, NextAssignmentSlot}}),
			?event(next_profiling, returning),
            {ok, #{ <<"body">> => NextAssignment, <<"state">> => NextState }};
        _ ->
            {error,
                #{
                    <<"status">> => 503,
                    <<"body">> => <<"No assignment found for next slot.">>
                }
            }
    end.

%% @doc Non-device exported helper to get the cached assignments held in a
%% process.
message_cached_assignments(Msg, Opts) ->
    hb_private:get(
        <<"scheduler@1.0/assignments">>,
        Msg,
        [],
        Opts
    ).

%% @doc Spawn a new Erlang process to fetch the next assignments from the local
%% cache, if we have them available.
spawn_lookahead_worker(ProcID, Slot, Opts) ->
    Caller = self(),
    spawn(
        fun() ->
            ?event(next_lookahead,
                {looking_ahead,
                    {proc_id, ProcID},
                    {slot, Slot},
                    {caller, Caller}
                }
            ),
            case dev_scheduler_cache:read(ProcID, Slot, Opts) of
                {ok, Assignment} ->
                    Caller ! {assignment, ProcID, Slot, Assignment};
                not_found ->
                    fail
            end
        end
    ).

%% @doc Check if we have a result from a lookahead worker or from our local
%% cache. If we have a result in the local cache, we may also start a new
%% lookahead worker to fetch the next assignments if we have them locally, 
%% ahead of time. This can be enabled/disabled with the `scheduler_lookahead'
%% option.
check_lookahead_and_local_cache(Msg1, ProcID, TargetSlot, Opts) when is_map(Msg1) ->
    case hb_private:get(<<"scheduler@1.0/lookahead-worker">>, Msg1, Opts) of
        not_found ->
            check_lookahead_and_local_cache(undefined, ProcID, TargetSlot, Opts);
        LookaheadWorker ->
            check_lookahead_and_local_cache(LookaheadWorker, ProcID, TargetSlot, Opts)
    end;
check_lookahead_and_local_cache(Worker, ProcID, TargetSlot, Opts) when is_pid(Worker) ->
    receive
        {assignment, ProcID, OldSlot, _Assignment} when OldSlot < TargetSlot ->
            % The lookahead worker has found an assignment for a slot that is
            % before the target slot. We remove it from the cache and continue
            % searching.
            ?event(next_lookahead,
                {received_expired_assignment,
                    {slot, OldSlot},
                    {target_slot, TargetSlot}
                }
            ),
            check_lookahead_and_local_cache(Worker, ProcID, TargetSlot, Opts);
        {assignment, ProcID, TargetSlot, Assignment} ->
            % The lookahead worker has found an assignment for the target slot.
            % We return the assignment and stop searching. We will start a new
            % lookahead worker to fetch the next slot in the background again.
            NewWorker = spawn_lookahead_worker(ProcID, TargetSlot + 1, Opts),
            ?event(next_lookahead,
                {lookahead_worker_succeeded, {slot, TargetSlot}}
            ),
            {ok, NewWorker, Assignment}
    after ?LOOKAHEAD_TIMEOUT ->
        ?event(next_lookahead, {lookahead_worker_timed_out, {slot, TargetSlot}}),
        erlang:exit(Worker, timeout),
        check_lookahead_and_local_cache(undefined, ProcID, TargetSlot, Opts)
    end;
check_lookahead_and_local_cache(undefined, ProcID, TargetSlot, Opts) ->
    % The lookahead worker has not found an assignment for the target
    % slot yet, so we check our local cache.
    ?event(next_lookahead, {no_lookahead_worker, {slot, TargetSlot}}),
    case dev_scheduler_cache:read(ProcID, TargetSlot, Opts) of
        not_found -> not_found;
        {ok, Assignment} ->
            % We have an assignment in our local cache, so we return it.
            % Depending on the `scheduler_lookahead' option, we may also
            % start a new lookahead worker to fetch the next assignments
            % if we have them locally, ahead of time.
            Worker =
                case hb_opts:get(scheduler_lookahead, true, Opts) of
                    false -> unset;
                    true ->
                        % We found the assignment in our local cache, so
                        % optionally spawn a new Erlang process to fetch
                        % the next assignments if we have them locally, 
                        % ahead of time.
                        spawn_lookahead_worker(ProcID, TargetSlot + 1, Opts)
                end,
            {ok, Worker, Assignment}
    end.

%% @doc Returns information about the entire scheduler.
status(_M1, _M2, _Opts) ->
    ?event(getting_scheduler_status),
    Wallet = dev_scheduler_registry:get_wallet(),
    {ok,
        #{
            <<"address">> => hb_util:id(ar_wallet:to_address(Wallet)),
            <<"processes">> =>
                lists:map(
                    fun hb_util:id/1,
                    dev_scheduler_registry:get_processes()
                ),
            <<"cache-control">> => <<"no-store">>
        }
    }.

%% @doc Router for `record' requests. Expects either a `POST' or `GET' request.
location(Msg1, Msg2, Opts) ->
    case hb_ao:get(<<"method">>, Msg2, <<"GET">>, Opts) of
        <<"POST">> -> post_location(Msg1, Msg2, Opts);
        <<"GET">> -> get_location(Msg1, Msg2, Opts)
    end.

%% @doc Search for the location of the scheduler in the scheduler-location
%% cache. If an address is provided, we search for the location of that
%% specific scheduler. Otherwise, we return the location record for the current
%% node's scheduler, if it has been established.
get_location(_Msg1, Req, Opts) ->
    % Get the address of the scheduler from the request.
    Address =
        hb_ao:get(
            <<"address">>,
            Req,
            hb_util:human_id(ar_wallet:to_address(
                hb_opts:get(priv_wallet, hb:wallet(), Opts)
            )),
            Opts
        ),
    % Search for the location of the scheduler in the scheduler-location cache.
    case dev_scheduler_cache:read_location(Address, Opts) of
        not_found ->
            {ok,
                #{
                    <<"status">> => 404,
                    <<"body">> =>
                        <<"No location found for address: ", Address/binary>>
                }
            };
        {ok, Location} -> {ok, #{ <<"body">> => Location }}
    end.

%% @doc Generate a new scheduler location record and register it. We both send 
%% the new scheduler-location to the given registry, and return it to the caller.
post_location(Msg1, RawReq, Opts) ->
    % Ensure that the request is signed by the operator.
    Req =
        case hb_ao:get(<<"target">>, RawReq, not_found, Opts) of
            not_found -> RawReq;
            Target -> hb_ao:get(Target, RawReq, not_found, Opts)
        end,
    {ok, OnlyCommitted} = hb_message:with_only_committed(Req),
    ?event(scheduler_location,
        {scheduler_location_registration_request, OnlyCommitted}
    ),
    % Gather metadata for request validation.
    Signers = hb_message:signers(OnlyCommitted),
    Self =
        hb_util:human_id(
            ar_wallet:to_address(
                hb_opts:get(priv_wallet, hb:wallet(), Opts)
            )
        ),
    ExistingNonce = 
        case hb_gateway_client:scheduler_location(Self, Opts) of
            {ok, SchedulerLocation} ->
                hb_ao:get(<<"nonce">>, SchedulerLocation, 0, Opts);
            {error, _} -> -1
        end,
    NewNonce = hb_ao:get(<<"nonce">>, OnlyCommitted, ExistingNonce + 1, Opts),
    case {NewNonce > ExistingNonce, lists:member(Self, Signers)} of
        {false, _} ->
            % Invalid request: Known nonce is already higher than requested nonce
            % for the given operator.
            {ok,
                #{
                    <<"status">> => 400,
                    <<"body">> => <<"Known nonce higher than requested nonce.">>,
                    <<"requested-nonce">> => NewNonce,
                    <<"existing-nonce">> => ExistingNonce,
                    <<"signers">> => Signers
                }
            };
        {true, false} ->
            % Received request to store a new scheduler location from a peer
            % that is not the operator.
            case dev_scheduler_cache:write_location(OnlyCommitted, Opts) of
                ok ->
                    ?event(scheduler_location,
                        {cached_foreign_peer_location, OnlyCommitted}
                    ),
                    {ok, OnlyCommitted};
                {error, Reason} ->
                    {error,
                        #{
                            <<"status">> => 400,
                            <<"body">> =>
                                <<"Failed to store new scheduler location.">>,
                            <<"reason">> => Reason
                        }
                    }
            end;
        {true, true} ->
            % The operator has asked to replace the scheduler location. Get the
            % details and register the new location. Registration occurs in the
            % following steps:
            % 1. Generate a new scheduler location message.
            % 2. Sign the message.
            % 3. Upload the message to Arweave.
            % 4. Post the message to the peers specified in the
            %    `scheduler_location_notify_peers' option.
            TimeToLive =
                hb_ao:get_first(
                    [
                        {Msg1, <<"time-to-live">>},
                        {OnlyCommitted, <<"time-to-live">>}
                    ],
                    hb_opts:get(scheduler_location_ttl, 1000 * 60 * 60, Opts),
                    Opts
                ),
            URL =
                case hb_ao:get(<<"url">>, OnlyCommitted, Opts) of
                    not_found ->
                        Port = hb_util:bin(hb_opts:get(port, 8734, Opts)),
                        Host = hb_opts:get(host, <<"localhost">>, Opts),
                        Protocol = hb_opts:get(protocol, http1, Opts),
                        ProtoStr =
                            case Protocol of
                                http1 -> <<"http">>;
                                _ -> <<"https">>
                            end,
                        <<ProtoStr/binary, "://", Host/binary, ":", Port/binary>>;
                    GivenURL -> GivenURL
                end,
            % Construct the new scheduler location message.
            Codec =
                hb_ao:get_first(
                    [
                        {Msg1, <<"accept-codec">>},
                        {OnlyCommitted, <<"accept-codec">>}
                    ],
                    <<"httpsig@1.0">>,
                    Opts
                ),
            NewSchedulerLocation =
                #{
                    <<"data-protocol">> => <<"ao">>,
                    <<"variant">> => <<"ao.N.1">>,
                    <<"type">> => <<"scheduler-location">>,
                    <<"url">> => URL,
                    <<"nonce">> => NewNonce,
                    <<"time-to-live">> => TimeToLive,
                    <<"codec-device">> => Codec
                },
            Signed = hb_message:commit(NewSchedulerLocation, Opts, Codec),
            dev_scheduler_cache:write_location(Signed, Opts),
            ?event(scheduler_location,
                {uploading_signed_scheduler_location, Signed}
            ),
            {UploadStatus, _} = hb_client:upload(Signed, Opts),
            % Post the new scheduler location to the peers specified in the
            % `scheduler_location_notify_peers' option.
            Results =
                lists:map(
                    fun(Node) ->
                        PostRes = hb_http:post(
                            Node,
                            <<"/~scheduler@1.0/record">>,
                            Signed,
                            Opts
                        ),
                        ?event(scheduler_location,
                            {outbound_request, {res, PostRes}}
                        )
                    end,
                    hb_opts:get(scheduler_location_notify_peers, [], Opts)
                ),
            ?event(scheduler_location,
                {scheduler_location_registration_success,
                    {arweave_publication_status, UploadStatus},
                    {foreign_peers_notified, length(Results)}
                }
            ),
            {ok, Signed}
    end.

%% @doc A router for choosing between getting the existing schedule, or
%% scheduling a new message.
schedule(Msg1, Msg2, Opts) ->
    ?event({resolving_schedule_request, {msg2, Msg2}, {state_msg, Msg1}}),
    case hb_ao:get(<<"method">>, Msg2, <<"GET">>, Opts) of
        <<"POST">> -> post_schedule(Msg1, Msg2, Opts);
        <<"GET">> -> get_schedule(Msg1, Msg2, Opts)
    end.

%% @doc Schedules a new message on the SU. Searches Msg1 for the appropriate ID,
%% then uses the wallet address of the scheduler to determine if the message is
%% for this scheduler. If so, it schedules the message and returns the assignment.
post_schedule(Msg1, Msg2, Opts) ->
    ?event(scheduling_message),
    % Find the target message to schedule:
    ToSched = find_message_to_schedule(Msg1, Msg2, Opts),
    ?event({to_sched, ToSched}),
    % Find the ProcessID of the target message:
    % - If it is a Process, use the ID of the message.
    % - If not, use the target as the ProcessID.
    ProcID =
        case hb_ao:get(<<"type">>, ToSched, not_found, Opts) of
            <<"Process">> -> hb_message:id(ToSched, all);
            _ ->
                case hb_ao:get(<<"target">>, ToSched, not_found, Opts) of
                    not_found -> find_target_id(Msg1, Msg2, Opts);
                    Target -> Target
                end
        end,
    ?event({proc_id, ProcID}),
    % Filter all unsigned keys from the source message.
    case hb_message:with_only_committed(ToSched) of
        {ok, OnlyCommitted} ->
            ?event(
                {post_schedule,
                    {schedule_id, ProcID},
                    {message, ToSched}
                }
            ),
            % Find the relevant scheduler server for the given process and
            % message, start a new one if necessary, or return a redirect to the
            % correct remote scheduler.
            case find_server(ProcID, Msg1, ToSched, Opts) of
                {local, PID} ->
                    ?event({scheduling_message_locally, {proc_id, ProcID}, {pid, PID}}),
                    do_post_schedule(ProcID, PID, OnlyCommitted, Opts);
                {redirect, Redirect} ->
                    ?event({process_is_remote, {redirect, Redirect}}),
                    case hb_opts:get(scheduler_follow_redirects, true, Opts) of
                        true ->
                            ?event({proxying_to_remote_scheduler,
                                {redirect, Redirect},
                                {msg, OnlyCommitted}
                            }),
                            post_remote_schedule(ProcID, Redirect, OnlyCommitted, Opts);
                        false -> {ok, Redirect}
                    end;
                {error, Error} ->
                    ?event({error_finding_scheduler, {error, Error}}),
                    {error, Error}
            end;
        {error, Err} ->
            {error,
                #{
                    <<"status">> => 400,
                    <<"body">> => <<"Message invalid: ",
                        "Committed components cannot be validated.">>,
                    <<"reason">> => Err
                }
            }
    end.

%% @doc Post schedule the message. `Msg2' by this point has been refined to only
%% committed keys, and to only include the `target' message that is to be
%% scheduled.
do_post_schedule(ProcID, PID, Msg2, Opts) ->
    % Should we verify the message again before scheduling?
    Verified =
        case hb_opts:get(verify_assignments, true, Opts) of
            true ->
                ?event({verifying_message_before_scheduling, Msg2}),
                hb_message:verify(Msg2, signers);
            false -> true
        end,
    % Handle scheduling of the message if the message is valid.
    case {Verified, hb_ao:get(<<"type">>, Msg2, Opts)} of
        {false, _} ->
            {error,
                #{
                    <<"status">> => 400,
                    <<"body">> => <<"Message is not valid.">>,
                    <<"reason">> => <<"Given message does not correctly validate.">>
                }
            };
        {true, <<"Process">>} ->
            {ok, _} = hb_cache:write(Msg2, Opts),
            spawn(fun() -> hb_client:upload(Msg2, Opts) end),
            ?event(
                {registering_new_process,
                    {proc_id, ProcID},
                    {pid, PID},
                    {is_alive, is_process_alive(PID)}
                }
            ),
            {ok, dev_scheduler_server:schedule(PID, Msg2)};
        {true, _} ->
            % If Message2 is not a process, use the ID of Message1 as the PID
            {ok, dev_scheduler_server:schedule(PID, Msg2)}
    end.

%% @doc Locate the correct scheduling server for a given process.
find_server(ProcID, Msg1, Opts) ->
    find_server(ProcID, Msg1, undefined, Opts).
find_server(ProcID, Msg1, ToSched, Opts) ->
    case get_hint(ProcID, Opts) of
        {ok, Hint} ->
            ?event({found_hint_in_proc_id, Hint}),
            generate_redirect(ProcID, Hint, Opts);
        not_found ->
            ?event({no_hint_in_proc_id, ProcID}),
            case dev_scheduler_registry:find(ProcID, false, Opts) of
                PID when is_pid(PID) ->
                    ?event({found_pid_in_local_registry, PID}),
                    {local, PID};
                not_found ->
                    ?event({no_pid_in_local_registry, ProcID}),
                    % Find the process from the message.
                    Proc =
                        case hb_ao:get(<<"process">>, Msg1, not_found, Opts#{ hashpath => ignore }) of
                            not_found ->
                                case (ToSched =/= undefined) andalso (hb_message:id(ToSched, all) == ProcID) of
                                    true -> ToSched;
                                    false ->
                                        ?event(
                                            {reading_cache,
                                                {proc_id, ProcID},
                                                {store, hb_opts:get(store, Opts)}
                                            }
                                        ),
                                        case hb_message:id(Msg1, all) of
                                            ProcID -> Msg1;
                                            _ ->
                                                case hb_cache:read(ProcID, Opts) of
                                                    {ok, P} -> P;
                                                    not_found ->
                                                        throw({process_not_available, ProcID})
                                                end
                                        end
                                end;
                            P -> P
                        end,
                    ?event({found_process, {process, Proc}, {msg1, Msg1}}),
                    % Check if we are the scheduler for this process.
                    Address = hb_util:human_id(ar_wallet:to_address(
                        hb_opts:get(priv_wallet, hb:wallet(), Opts))),
                    ?event({local_address, Address}),
                    SchedLoc =
                        hb_ao:get_first(
                            [
                                {Proc, <<"scheduler">>},
                                {Proc, <<"scheduler-location">>}
                            ] ++
                            case ToSched of
                                undefined -> [];
                                _ -> [{ToSched, <<"scheduler-location">>}]
                            end,
                            not_found,
                            Opts#{ hashpath => ignore }
                        ),
                    ?event({sched_loc, SchedLoc}),
                    case SchedLoc of
                        not_found ->
                            {error, <<"No scheduler information provided.">>};
                        Address ->
                            % We are the scheduler. Start the server if it has not already
                            % been started.
                            {local, dev_scheduler_registry:find(ProcID, true, Opts)};
                        _ ->
                            % We are not the scheduler. Find it and return a redirect.
                            find_remote_scheduler(ProcID, SchedLoc, Opts)
                    end
            end
    end.

%% @doc If a hint is present in the string, return it. Else, return not_found.
get_hint(Str, Opts) when is_binary(Str) ->
    case hb_opts:get(scheduler_follow_hints, true, Opts) of
        true ->
            case binary:split(Str, <<"?">>, [global]) of
                [_, QS] ->
                    QueryMap = maps:from_list(uri_string:dissect_query(QS)),
                    case maps:get(<<"hint">>, QueryMap, not_found) of
                        not_found -> not_found;
                        Hint -> {ok, Hint}
                    end;
                _ -> not_found
            end;
        false -> not_found
    end;
get_hint(_Str, _Opts) -> not_found.

%% @doc Generate a redirect message to a scheduler.
generate_redirect(ProcID, SchedulerLocation, Opts) ->
    Variant = hb_ao:get(<<"variant">>, SchedulerLocation, <<"ao.N.1">>, Opts),
    ?event({generating_redirect, {proc_id, ProcID}, {variant, Variant}}),
    RedirectLocation =
        case is_binary(SchedulerLocation) of
            true -> SchedulerLocation;
            false ->
                hb_ao:get_first(
                    [
                        {SchedulerLocation, <<"url">>},
                        {SchedulerLocation, <<"location">>}
                    ],
                    <<"/">>,
                    Opts
                )
        end,
    {redirect,
        #{
            <<"status">> => 307,
            <<"location">> => RedirectLocation,
            <<"body">> =>
                <<"Redirecting to scheduler: ", RedirectLocation/binary>>,
            <<"variant">> => Variant
        }
    }.

%% @doc Take a process ID or target with a potential hint and return just the
%% process ID.
without_hint(Target) when ?IS_ID(Target) ->
    hb_util:human_id(Target);
without_hint(Target) ->
    case binary:split(Target, [<<"?">>, <<"&">>], [global]) of
        [ProcID] when ?IS_ID(ProcID) -> hb_util:human_id(ProcID);
        _ -> throw({invalid_operation_target, Target})
    end.

%% @doc Use the SchedulerLocation to the remote path and return a redirect.
find_remote_scheduler(ProcID, Scheduler, Opts) ->
    % Parse the scheduler location to see if it has a hint. If there is a hint,
    % we will use it to construct a redirect message.
    case get_hint(Scheduler, Opts) of
        {ok, Hint} ->
            % We have a hint. Construct a redirect message.
            generate_redirect(ProcID, Hint, Opts);
        not_found ->
            case dev_scheduler_cache:read_location(Scheduler, Opts) of
                {ok, SchedMsg} ->
                    % We have a cached scheduler location. Use it to construct a
                    % redirect message.
                    generate_redirect(ProcID, SchedMsg, Opts);
                not_found ->
                    % We have not yet cached the location for this address.
                    % Find it via the gateway.
                    case hb_gateway_client:scheduler_location(Scheduler, Opts) of
                        {ok, SchedMsg} ->
                            % We have found the location. Cache it and use it to
                            % construct a redirect message.
                            dev_scheduler_cache:write_location(
                                SchedMsg,
                                Opts
                            ),
                            generate_redirect(ProcID, SchedMsg, Opts);
                        {error, Res} ->
                            ?event({error_finding_scheduler, {error, Res}}),
                            {error, Res}
                    end
            end
    end.

%% @doc Returns information about the current slot for a process.
slot(M1, M2, Opts) ->
    ?event({getting_current_slot, {msg, M1}}),
    ProcID = find_target_id(M1, M2, Opts),
    case find_server(ProcID, M1, Opts) of
        {local, PID} ->
            ?event({getting_current_slot, {proc_id, ProcID}}),
            {Timestamp, Hash, Height} = ar_timestamp:get(),
            #{ current := CurrentSlot, wallet := Wallet } =
                dev_scheduler_server:info(PID),
            {ok, #{
                <<"process">> => ProcID,
                <<"current">> => CurrentSlot,
                <<"timestamp">> => Timestamp,
                <<"block-height">> => Height,
                <<"block-hash">> => Hash,
                <<"cache-control">> => <<"no-store">>,
                <<"wallet-address">> => hb_util:human_id(ar_wallet:to_address(Wallet))
            }};
        {redirect, Redirect} ->
            case hb_opts:get(scheduler_follow_redirects, true, Opts) of
                false -> {ok, Redirect};
                true -> remote_slot(ProcID, Redirect, Opts)
            end
    end.

%% @doc Get the current slot from a remote scheduler.
remote_slot(ProcID, Redirect, Opts) ->
    ?event({getting_remote_slot, {proc_id, ProcID}, {redirect, {explicit, Redirect}}}),
    Node = node_from_redirect(Redirect, Opts),
    ?event({getting_slot_from_node, {string, Node}}),
    remote_slot(
        hb_ao:get(<<"variant">>, Redirect, <<"ao.N.1">>, Opts),
        ProcID,
        Node,
        Opts
    ).

%% @doc Get the current slot from a remote scheduler, based on the variant of
%% the process's scheduler.
remote_slot(<<"ao.N.1">>, ProcID, Node, Opts) ->
    % The process is running on a mainnet AO-Core scheduler, so we can just
    % use the `/slot' endpoint to get the current slot.
    ?event({getting_slot_from_ao_core_remote,
        {path, {string, <<"/", ProcID/binary, "/slot">>}}}),
    hb_http:get(Node, <<ProcID/binary, "/slot">>, Opts);
remote_slot(<<"ao.TN.1">>, ProcID, Node, Opts) ->
    % The process is running on a testnet AO-Core scheduler, so we need to use
    % `/processes/procID/latest' to get the current slot.
    Path = << ProcID/binary, "/latest?proc-id=", ProcID/binary>>,
    ?event({getting_slot_from_ao_core_remote, {path, {string, Path}}}),
    case hb_http:get(Node, Path, Opts#{ http_client => httpc }) of
        {ok, Res} ->
            ?event({remote_slot_result, {res, Res}}),
            case hb_util:int(hb_ao:get(<<"status">>, Res, 200, Opts)) of
                200 ->
                    Body = hb_ao:get(<<"body">>, Res, Opts),
                    JSON = hb_json:decode(Body),
                    ?event({got_slot_response, {json, JSON}}),
                    % Convert the JSON object for the latest assignment into the
                    % standardized `~scheduler@1.0' format.
                    A =
                        dev_scheduler_formats:aos2_to_assignment(
                            JSON,
                            Opts
                        ),
                    ?event({got_slot_response, {assignment, A}}),
                    {ok, #{
                        <<"process">> => ProcID,
                        <<"current">> => maps:get(<<"slot">>, A),
                        <<"timestamp">> => maps:get(<<"timestamp">>, A),
                        <<"block-height">> => maps:get(<<"block-height">>, A),
                        <<"block-hash">> => hb_util:encode(<<0:256>>),
                        <<"cache-control">> => <<"no-store">>
                    }};
                307 ->
                    ?event({generating_new_redirect, {redirect, Res}}),
                    % Maintain the same variant, but generate the redirect using
                    % the new location.
                    NewRedirect =
                        generate_redirect(
                            ProcID,
                            Res#{ <<"variant">> => <<"ao.TN.1">> },
                            Opts
                        ),
                    ?event({recursing_on_new_redirect, {redirect, NewRedirect}}),
                    remote_slot(ProcID, NewRedirect, Opts);
                _ ->
                    {error, Res}
            end;
        {error, Res} ->
            ?event({remote_slot_error, {error, Res}}),
            {error, Res}
    end.

%% @doc Generate and return a schedule for a process, optionally between
%% two slots -- labelled as `from' and `to'. If the schedule is not local,
%% we redirect to the remote scheduler or proxy based on the node opts.
get_schedule(Msg1, Msg2, Opts) ->
    ProcID = find_target_id(Msg1, Msg2, Opts),
    From =
        case hb_ao:get(<<"from">>, Msg2, not_found, Opts) of
            not_found -> 0;
            X when X < 0 -> 0;
            FromRes -> hb_util:int(FromRes)
        end,
    To =
        case hb_ao:get(<<"to">>, Msg2, not_found, Opts) of
            not_found -> undefined;
            ToRes -> hb_util:int(ToRes)
        end,
    Format = hb_ao:get(<<"accept">>, Msg2, <<"application/http">>, Opts),
    ?event({parsed_get_schedule, {process, ProcID}, {from, From}, {to, To}, {format, Format}}),
    case find_server(ProcID, Msg1, Opts) of
        {local, _PID} ->
            generate_local_schedule(Format, ProcID, From, To, Opts);
        {redirect, Redirect} ->
            ?event({redirect_received, {redirect, Redirect}}),
            case hb_opts:get(scheduler_follow_redirects, true, Opts) of
                true ->
                    case get_remote_schedule(ProcID, From, To, Redirect, Opts) of
                        {ok, Res} ->
                            case uri_string:percent_decode(Format) of
                                <<"application/aos-2">> ->
                                    {ok, Formatted} = dev_scheduler_formats:assignments_to_aos2(
                                        ProcID,
                                        hb_ao:get(
                                            <<"assignments">>, Res, [], Opts),
                                        hb_util:atom(hb_ao:get(
                                            <<"continues">>, Res, false, Opts)),
                                        Opts
                                    ),
                                    ?event({formatted_assignments,
                                        {body,
                                            {string, hb_ao:get(<<"body">>, Formatted, Opts)}
                                        },
                                        {full, Formatted}}
                                    ),
                                    {ok, Formatted};
                                _ ->
                                    {ok, Res}
                            end;
                        {error, Res} ->
                            {error, Res}
                    end;
                false ->
                    {ok, Redirect}
            end
    end.

%% @doc Get a schedule from a remote scheduler, but first read all of the 
%% assignments from the local cache that we already know about.
get_remote_schedule(RawProcID, From, To, Redirect, Opts) ->
    % If we are responding to a legacy scheduler request we must add one to the
    % `from' slot to account for the fact that the legacy scheduler gives us
    % the slots _after_ the stated nonce.
    ProcID = without_hint(RawProcID),
    {FromLocalCache, _} = get_local_assignments(ProcID, From, To, Opts),
    ?event(debug_sched,
        {from_local_cache,
            {from, From},
            {to, To},
            {read, length(FromLocalCache)}
        }
    ),
    do_get_remote_schedule(
        ProcID,
        FromLocalCache,
        From + length(FromLocalCache),
        To,
        Redirect,
        Opts
    ).

%% @doc Get a schedule from a remote scheduler, unless we already have already
%% read all of the assignments from the local cache.
do_get_remote_schedule(ProcID, LocalAssignments, From, To, _, Opts)
        when (To =/= undefined) andalso (From >= To) ->
    % We already have all of the assignments from the local cache. Return them
    % as a bundle. We set the 'more' to `undefined' to indicate that there may
    % be more assignments to fetch, but we don't know for sure.
    Res = 
        dev_scheduler_formats:assignments_to_bundle(
            ProcID,
            LocalAssignments,
            undefined,
            Opts
        ),
    ?event(debug_sched,
        {returning_remote_schedule_from_only_cache,
            {length, length(LocalAssignments)},
            {from_after_local_cache, From},
            {original_to, To}
        }),
    Res;
do_get_remote_schedule(ProcID, LocalAssignments, From, To, Redirect, Opts) ->
    % We don't have all of the assignments from the local cache, so we need to
    % fetch the rest from the remote scheduler.
    Node = node_from_redirect(Redirect, Opts),
    Variant = hb_ao:get(<<"variant">>, Redirect, <<"ao.N.1">>, Opts),
    ?event(
        {getting_remote_schedule,
            {node, {string, Node}},
            {proc_id, {string, ProcID}},
            {from, From},
            {to, To}
        }
    ),
    MaybeNonce =
        if Variant == <<"ao.TN.1">> -> <<"-nonce">>;
        true -> <<>>
        end,
    FromBin =
        case From of
            undefined -> <<>>;
            From ->
                % The legacy scheduler gives us the slots _after_ the stated 
                % nonce. So we need to subtract one from the nonce to get the
                % correct slot.
                ModFrom =
                    if Variant == <<"ao.TN.1">> -> From - 1;
                    true -> From
                    end,
                <<
                    "&from",
                    MaybeNonce/binary,
                    "=",
                    (integer_to_binary(ModFrom))/binary
                >>
        end,
    ToParam =
        case To of
            undefined -> <<>>;
            To ->
                <<
                    "&to", MaybeNonce/binary, "=", (integer_to_binary(To))/binary
                >>
        end,
    Path =
        case Variant of
            <<"ao.N.1">> ->
                <<
                    ProcID/binary,
                    "/schedule?from=", FromBin/binary, ToParam
                >>;
            <<"ao.TN.1">> ->
                <<
                    ProcID/binary, "?proc-id=", ProcID/binary,
                    FromBin/binary, ToParam/binary,
                    "&limit=1000"
                >>
        end,
    ?event({getting_remote_schedule, {node, {string, Node}}, {path, {string, Path}}}),
    case hb_http:get(Node, Path, Opts#{ http_client => httpc, protocol => http2 }) of
        {ok, Res} ->
            case hb_util:int(hb_ao:get(<<"status">>, Res, 200, Opts)) of
                200 ->
                    {ok, NormSched} = 
                        case Variant of
                            <<"ao.N.1">> ->
                                {ok, Res};
                            <<"ao.TN.1">> ->
                                JSONRes =
                                    hb_json:decode(
                                        hb_ao:get(
                                            <<"body">>,
                                            Res,
                                            <<"">>,
                                            Opts#{ hashpath => ignore }
                                        )
                                    ),
                                Filtered = filter_json_assignments(JSONRes, To, From),
                                dev_scheduler_formats:aos2_to_assignments(
                                    ProcID,
                                    Filtered,
                                    Opts
                                )
                        end,
                    cache_remote_schedule(NormSched, Opts),
                    % Add existing local assignments we read to the remote schedule.
                    % In order to do this, we need to first convert the remote
                    % assignments to a list, maintaining the order of the keys.
                    RemoteAssignments =
                        hb_util:message_to_ordered_list(
                            hb_ao:normalize_keys(
                                hb_ao:get(
                                    <<"assignments">>,
                                    NormSched,
                                    Opts
                                )
                            )
                        ),
                    % Merge the local assignments with the remote assignments,
                    % and normalize the keys.
                    Merged =
                        dev_scheduler_formats:assignments_to_bundle(
                            ProcID,
                            MergedAssignments = LocalAssignments ++ RemoteAssignments,
                            hb_ao:get(<<"continues">>, NormSched, false, Opts),
                            Opts
                        ),
                    ?event(debug_sched,
                        {returning_remote_schedule,
                            {length, length(MergedAssignments)},
                            {from_local_cache, length(LocalAssignments)},
                            {from_remote_cache, length(RemoteAssignments)}
                        }
                    ),
                    Merged;
                307 ->
                    % NOTE: Shouldn't this be using the `Res' location key to
                    % regenerate the redirect and recurse on that, instead of
                    % just using the same redirect?
                    ?event({recursing_on_same_redirect, {redirect, Redirect}}),
                    do_get_remote_schedule(
                        ProcID,
                        LocalAssignments,
                        From,
                        To,
                        Redirect,
                        Opts
                    )
            end;
        {error, Res} ->
            ?event(push, {remote_schedule_result, {res, Res}}, Opts),
            {error, Res}
    end.

%% @doc Cache a schedule received from a remote scheduler.
cache_remote_schedule(Schedule, Opts) ->
    Cacher =
        fun() ->
            ?event(debug_sched, {caching_remote_schedule, {schedule, Schedule}}),
            Assignments =
                hb_ao:get(
                    <<"assignments">>,
                    Schedule,
                    Opts#{ hashpath => ignore }
                ),
            lists:foreach(
                fun(Assignment) ->
                    % We do not care about the result of the write because it is only
                    % an additional cache.
                    ?event(debug_sched,
                        {writing_assignment,
                            {assignment, maps:get(<<"slot">>, Assignment)}
                        }
                    ),
                    dev_scheduler_cache:write(Assignment, Opts)
                end,
                AssignmentList =
                    hb_util:message_to_ordered_list(
                        maps:without([<<"priv">>], hb_ao:normalize_keys(Assignments))
                    )
            ),
            ?event(debug_sched,
                {caching_remote_schedule, {assignments, length(AssignmentList)}}
            )
        end,
    case hb_opts:get(scheduler_async_remote_cache, true, Opts) of
        true -> spawn(Cacher);
        false -> Cacher()
    end.

%% @doc Get the node URL from a redirect.
node_from_redirect(Redirect, Opts) ->
    uri_string:recompose(
        (
            maps:remove(
                query,
                uri_string:parse(
                    hb_ao:get(<<"location">>, Redirect, Opts)
                )
            )
        )#{path => <<"/">>}
    ).

%% @doc Filter JSON assignment results from a remote legacy scheduler.
filter_json_assignments(JSONRes, To, From) ->
    Edges = maps:get(<<"edges">>, JSONRes, []),
    Filtered =
        lists:filter(
            fun(Edge) ->
                Node = maps:get(<<"node">>, Edge),
                Assignment = maps:get(<<"assignment">>, Node),
                Tags = maps:get(<<"tags">>, Assignment),
                Nonces = 
                    lists:filtermap(
                        fun(#{ <<"name">> := <<"Nonce">>, <<"value">> := Nonce }) ->
                            {true, hb_util:int(Nonce)};
                           (_) -> false
                    end,
                    Tags
                ),
                Nonce = hd(Nonces),
                ?event({filter, {nonce, Nonce}, {from, From}, {to, To}}),
                Nonce >= From andalso Nonce =< To
            end,
            Edges
        ),
    ?event({filtered, {length, length(Filtered)}, {edges, Filtered}}),
    JSONRes#{ <<"edges">> => Filtered }.

post_remote_schedule(RawProcID, Redirect, OnlyCommitted, Opts) ->
    RemoteOpts = Opts#{ http_client => httpc },
    ProcID = without_hint(RawProcID),
    Location = hb_ao:get(<<"location">>, Redirect, Opts),
    Parsed = uri_string:parse(Location),
    Node = uri_string:recompose((maps:remove(query, Parsed))#{path => <<"/">>}),
    Variant = hb_ao:get(<<"variant">>, Redirect, <<"ao.N.1">>, Opts),
    case Variant of
        <<"ao.N.1">> ->
            PostMsg = #{
                <<"path">> => << ProcID/binary, "/schedule">>,
                <<"body">> => OnlyCommitted,
                <<"method">> => <<"POST">>
            },
            hb_http:post(Node, PostMsg, RemoteOpts);
        <<"ao.TN.1">> ->
            post_legacy_schedule(ProcID, OnlyCommitted, Node, RemoteOpts)
    end.

post_legacy_schedule(ProcID, OnlyCommitted, Node, Opts) ->
    ?event({encoding_for_legacy_scheduler, {node, {string, Node}}}),
    Encoded =
        try
            Item =
                hb_message:convert(
                    OnlyCommitted,
                    <<"ans104@1.0">>,
                    Opts
                ),
            ?event(
                {encoded_for_legacy_scheduler,
                    {item, Item},
                    {exact, {explicit, Item}}
                }
            ),
            {ok, ar_bundles:serialize(Item)}
        catch
            _:_ ->
                {error,
                    #{
                        <<"status">> => 422,
                        <<"body">> =>
                            <<
                                "Failed to post schedule on ", Node/binary,
                                " for ", ProcID/binary, ". Try different encoding?"
                            >>
                    }
                }
        end,
    case Encoded of
        {error, EncodingErr} ->
            ?event({could_not_encode_for_legacy_scheduler, {error, EncodingErr}}),
            {error, #{
                <<"status">> => 422,
                <<"body">> =>
                    <<"Incorrect encoding. Scheduler has variant: ao.TN.1">>
                }
            };
        {ok, Body} ->
            ?event({encoded_for_legacy_scheduler, {encoded, Body}}),
            PostMsg = #{
                <<"path">> => P = <<"/?proc-id=", ProcID/binary>>,
                <<"body">> => Body,
                <<"method">> => <<"POST">>
            },
            ?event({posting_to_remote_legacy_scheduler,
                {node, {string, Node}},
                {path, {string, P}},
                {process_id, {string, ProcID}}
            }),
            LegacyOpts = Opts#{ protocol => http2 },
            case hb_http:post(Node, PostMsg, LegacyOpts) of
                {ok, PostRes} ->
                    ?event({remote_schedule_result, PostRes}),
                    JSONRes =
                        hb_json:decode(
                            hb_ao:get(<<"body">>, PostRes, Opts)
                        ),
                    % Legacy SUs return only the ID of the assignment, so we need
                    % to read and return it.
                    ID = maps:get(<<"id">>, JSONRes),
                    ?event({remote_schedule_result_id, ID, {json, JSONRes}}),
                    case hb_http:get(Node, << ID/binary, "?process-id=", ProcID/binary>>, LegacyOpts) of
                        {ok, AssignmentRes} ->
                            ?event({received_full_assignment, AssignmentRes}),
                            AssignmentJSON =
                                hb_json:decode(
                                    hb_ao:get(<<"body">>, AssignmentRes, Opts)
                                ),
                            Assignment =
                                dev_scheduler_formats:aos2_to_assignment(
                                    AssignmentJSON,
                                    Opts
                                ),
                            {ok, Assignment};
                        {error, PostErr} -> {error, PostErr}
                    end;
                {error, Resp = #{ <<"status">> := 404 }} ->
                    ?event({legacy_scheduler_not_found, {url, {string, P}}, {resp, Resp}}),
                    {error, Resp};
                {error, PostRes} ->
                    ?event({remote_schedule_proxy_error, {error, PostRes}}),
                    {error, PostRes}
            end
    end.

%%% Private methods

%% @doc Find the schedule ID from a given request. The precidence order for 
%% search is as follows:
%% [1. `ToSched/id' -- in the case of `POST schedule', handled locally]
%% 2. `Msg2/target'
%% 3. `Msg2/id' when `Msg2' has `type: Process'
%% 4. `Msg1/process/id'
%% 5. `Msg1/id' when `Msg1' has `type: Process'
%% 6. `Msg2/id'
find_target_id(Msg1, Msg2, Opts) ->
    TempOpts = Opts#{ hashpath => ignore },
    Res = case hb_ao:resolve(Msg2, <<"target">>, TempOpts) of
        {ok, Target} ->
            % ID found at Msg2/target
            Target;
        _ ->
            case hb_ao:resolve(Msg2, <<"type">>, TempOpts) of
                {ok, <<"Process">>} ->
                    % Msg2 is a Process, so the ID is at Msg2/id
                    hb_message:id(Msg2, all);
                _ ->
                    case hb_ao:resolve(Msg1, <<"process">>, TempOpts) of
                        {ok, Process} ->
                            % ID found at Msg1/process/id
                            hb_message:id(Process, all);
                        _ ->
                            % Does the message have a type of Process?
                            case hb_ao:get(<<"type">>, Msg1, TempOpts) of
                                <<"Process">> ->
                                    % Yes, so try Msg1/id
                                    hb_message:id(Msg1, all);
                                _ ->
                                    % No, so the ID is at Msg2/id
                                    hb_message:id(Msg2, all)
                            end
                end
            end
    end,
    ?event({found_id, {id, Res}, {msg1, Msg1}, {msg2, Msg2}}),
    Res.

%% @doc Search the given base and request message pair to find the message to
%% schedule. The precidence order for search is as follows:
%% 1. `Msg2/body'
%% 2. `Msg2'
find_message_to_schedule(_Msg1, Msg2, Opts) ->
    case hb_ao:resolve(Msg2, <<"body">>, Opts#{ hashpath => ignore }) of
        {ok, Body} ->
            Body;
        _ -> Msg2
    end.

%% @doc Generate a `GET /schedule' response for a process.
generate_local_schedule(Format, ProcID, From, To, Opts) ->
    ?event(
        {servicing_request_for_assignments,
            {proc_id, ProcID},
            {from, From},
            {to, To}
        }
    ),
    ?event(generating_schedule_from_local_server),
    {Assignments, More} = get_local_assignments(ProcID, From, To, Opts),
    ?event({got_assignments, length(Assignments), {more, More}}),
    % Determine and apply the formatting function to use for generation 
    % of the response, based on the `Accept' header.
    FormatterFun =
        case uri_string:percent_decode(Format) of
            <<"application/aos-2">> ->
                fun dev_scheduler_formats:assignments_to_aos2/4;
            _ ->
                fun dev_scheduler_formats:assignments_to_bundle/4
        end,
    Res = FormatterFun(ProcID, Assignments, More, Opts),
    ?event({assignments_bundle_outbound, {format, Format}, {res, Res}}),
    Res.

%% @doc Get the assignments for a process, and whether the request was truncated.
get_local_assignments(ProcID, From, undefined, Opts) ->
    case dev_scheduler_cache:latest(ProcID, Opts) of
        not_found ->
            % No assignments in cache.
            {[], false};
        {Slot, _} ->
            get_local_assignments(ProcID, From, Slot, Opts)
    end;
get_local_assignments(ProcID, From, RequestedTo, Opts) ->
    ?event({handling_req_to_get_assignments, ProcID, {from, From}, {to, RequestedTo}}),
    ComputedTo =
        case (RequestedTo - From) > ?MAX_ASSIGNMENT_QUERY_LEN of
            true -> From + ?MAX_ASSIGNMENT_QUERY_LEN;
            false -> RequestedTo
        end,
    {
        read_local_assignments(ProcID, From, ComputedTo, Opts),
        ComputedTo < RequestedTo
    }.

%% @doc Get the assignments for a process.
read_local_assignments(_ProcID, From, To, _Opts) when From > To ->
    [];
read_local_assignments(ProcID, CurrentSlot, To, Opts) ->
    case dev_scheduler_cache:read(ProcID, CurrentSlot, Opts) of
        not_found ->
            % No assignment found in cache.
            [];
        {ok, Assignment} ->
            [
                Assignment
                | read_local_assignments(
                    ProcID,
                    CurrentSlot + 1,
                    To,
                    Opts
                )
            ]
    end.

%% @doc Returns the current state of the scheduler.
checkpoint(State) -> {ok, State}.

%%% Tests

%% @doc Generate a _transformed_ process message, not as they are generated 
%% by users. See `dev_process' for examples of AO process messages.
test_process() -> test_process(hb:wallet()).
test_process(Wallet) when not is_binary(Wallet) ->
    test_process(hb_util:human_id(ar_wallet:to_address(Wallet)));
test_process(Address) ->
    #{
        <<"device">> => <<"scheduler@1.0">>,
        <<"device-stack">> => [<<"Cron@1.0">>, <<"WASM-64@1.0">>, <<"PODA@1.0">>],
        <<"image">> => <<"wasm-image-id">>,
        <<"type">> => <<"Process">>,
        <<"scheduler-location">> => Address,
        <<"test-random-seed">> => rand:uniform(1337)
    }.

status_test() ->
    start(),
    ?assertMatch(
        #{<<"processes">> := Processes,
            <<"address">> := Address}
            when is_list(Processes) and is_binary(Address),
        hb_ao:get(status, test_process())
    ).

register_new_process_test() ->
    start(),
    Msg1 = test_process(),
    ?event({test_registering_new_process, {msg, Msg1}}),
    ?assertMatch({ok, _},
        hb_ao:resolve(
            Msg1,
            #{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"schedule">>,
                <<"body">> => Msg1
            },
            #{}
        )
    ),
    ?event({status_response, Msg1}),
    Procs = hb_ao:get(<<"processes">>, hb_ao:get(status, Msg1)),
    ?event({procs, Procs}),
    ?assert(
        lists:member(
            hb_util:id(Msg1, all),
            hb_ao:get(<<"processes">>, hb_ao:get(status, Msg1))
        )
    ).

%% @doc Test that a scheduler location is registered on boot.
register_location_on_boot_test() ->
    NotifiedPeerWallet = ar_wallet:new(),
    RegisteringNodeWallet = ar_wallet:new(),
    start(),
    NotifiedPeer =
        hb_http_server:start_node(#{
            priv_wallet => NotifiedPeerWallet,
            store => [
                #{
                    <<"store-module">> => hb_store_fs,
                    <<"prefix">> => <<"cache-TEST/scheduler-location-notified">>
                }
            ]
        }),
    RegisteringNode = hb_http_server:start_node(
        #{
            priv_wallet => RegisteringNodeWallet,
            on =>
                #{
                    <<"start">> => #{
                        <<"device">> => <<"scheduler@1.0">>,
                        <<"path">> => <<"location">>,
                        <<"method">> => <<"POST">>,
                        <<"accept-codec">> => <<"ans104@1.0">>,
                        <<"hook">> =>#{
                            <<"result">> => <<"ignore">>,
                            <<"commit-request">> => true
                        }
                    }
                },
            scheduler_location_notify_peers => [NotifiedPeer]
        }
    ),
    {ok, CurrentLocation} =
        hb_http:get(
            RegisteringNode,
            <<"/~scheduler@1.0/location">>,
            #{
                <<"method">> => <<"GET">>,
                <<"address">> =>
                    hb_util:human_id(ar_wallet:to_address(RegisteringNodeWallet))
            }
        ),
    ?event({current_location, CurrentLocation}),
    ?assertMatch(
        #{ <<"url">> := Location, <<"nonce">> := 0 }
            when is_binary(Location),
        hb_ao:get(<<"body">>, CurrentLocation, #{})
    ).

schedule_message_and_get_slot_test() ->
    start(),
    Msg1 = test_process(),
    Msg2 = #{
        <<"path">> => <<"schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> =>
            hb_message:commit(#{
                <<"type">> => <<"Message">>,
                <<"test-key">> => <<"true">>
            }, hb:wallet())
    },
    ?assertMatch({ok, _}, hb_ao:resolve(Msg1, Msg2, #{})),
    ?assertMatch({ok, _}, hb_ao:resolve(Msg1, Msg2, #{})),
    Msg3 = #{
        <<"path">> => <<"slot">>,
        <<"method">> => <<"GET">>,
        <<"process">> => hb_util:id(Msg1)
    },
    ?event({pg, dev_scheduler_registry:get_processes()}),
    ?event({getting_schedule, {msg, Msg3}}),
    ?assertMatch({ok, #{ <<"current">> := CurrentSlot }}
            when CurrentSlot > 0,
        hb_ao:resolve(Msg1, Msg3, #{})).

redirect_to_hint_test() ->
    start(),
    RandAddr = hb_util:human_id(crypto:strong_rand_bytes(32)),
    TestLoc = <<"http://test.computer">>,
    Msg1 = test_process(<< RandAddr/binary, "?hint=", TestLoc/binary>>),
    Msg2 = #{
        <<"path">> => <<"schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> => Msg1
    },
    ?assertMatch(
        {ok, #{ <<"location">> := Location }} when is_binary(Location),
        hb_ao:resolve(
            Msg1,
            Msg2,
            #{
                scheduler_follow_hints => true,
                scheduler_follow_redirects => false
            }
        )
    ).

redirect_from_graphql_test() ->
    start(),
    Opts =
        #{ store =>
            [
                #{ <<"store-module">> => hb_store_fs, <<"prefix">> => <<"cache-mainnet">> },
                #{ <<"store-module">> => hb_store_gateway, <<"store">> => false }
            ]
        },
    {ok, Msg} = hb_cache:read(<<"0syT13r0s0tgPmIed95bJnuSqaD29HQNN8D3ElLSrsc">>, Opts),
    ?assertMatch(
        {ok, #{ <<"location">> := Location }} when is_binary(Location),
        hb_ao:resolve(
            Msg,
            #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> =>
                    hb_message:commit(#{
                        <<"type">> => <<"Message">>,
                        <<"target">> =>
                            <<"0syT13r0s0tgPmIed95bJnuSqaD29HQNN8D3ElLSrsc">>,
                        <<"test-key">> => <<"Test-Val">>
                    },
                    hb:wallet()
                )
            },
            #{
                scheduler_follow_redirects => false
            }
        )
    ).

get_local_schedule_test() ->
    start(),
    Msg1 = test_process(),
    Msg2 = #{
        <<"path">> => <<"schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> =>
            hb_message:commit(#{
                <<"type">> => <<"Message">>,
                <<"test-key">> => <<"Test-Val">>
            }, hb:wallet())
    },
    Msg3 = #{
        <<"path">> => <<"schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> =>
            hb_message:commit(#{
                <<"type">> => <<"Message">>,
                <<"test-key">> => <<"Test-Val-2">>
            }, hb:wallet())
    },
    ?assertMatch({ok, _}, hb_ao:resolve(Msg1, Msg2, #{})),
    ?assertMatch({ok, _}, hb_ao:resolve(Msg1, Msg3, #{})),
    ?assertMatch(
        {ok, _},
        hb_ao:resolve(Msg1, #{
            <<"method">> => <<"GET">>,
            <<"path">> => <<"schedule">>,
            <<"target">> => hb_util:id(Msg1)
        },
        #{})
    ).

%%% HTTP tests

http_init() -> http_init(#{}).
http_init(Opts) ->
    start(),
    Wallet = ar_wallet:new(),
    Node = hb_http_server:start_node(
        Opts#{
            priv_wallet => Wallet,
            store => [
                #{ <<"store-module">> => hb_store_fs, <<"prefix">> => <<"cache-mainnet">> },
                #{ <<"store-module">> => hb_store_gateway, <<"store">> => false }
            ]
        }),
    {Node, Wallet}.

register_scheduler_test() ->
    start(),
    {Node, Wallet} = http_init(),
    Msg1 = hb_message:commit(#{
        <<"path">> => <<"/~scheduler@1.0/location">>,
        <<"url">> => <<"https://hyperbeam-test-ignore.com">>,
        <<"method">> => <<"POST">>,
        <<"nonce">> => 1,
        <<"accept-codec">> => <<"ans104@1.0">>
    }, Wallet),
    {ok, Res} = hb_http:post(Node, Msg1, #{}),
    ?assertMatch(#{ <<"url">> := Location } when is_binary(Location), Res).

http_post_schedule_sign(Node, Msg, ProcessMsg, Wallet) ->
    Msg1 = hb_message:commit(#{
        <<"path">> => <<"/~scheduler@1.0/schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> =>
            hb_message:commit(
                Msg#{
                    <<"target">> =>
                        hb_util:human_id(hb_message:id(ProcessMsg, all)),
                    <<"type">> => <<"Message">>
                },
                Wallet
            )
    }, Wallet),
    hb_http:post(Node, Msg1, #{}).

http_get_slot(N, PMsg) ->
    ID = hb_message:id(PMsg, all),
    Wallet = hb:wallet(),
    {ok, _} = hb_http:get(N, hb_message:commit(#{
        <<"path">> => <<"/~scheduler@1.0/slot">>,
        <<"method">> => <<"GET">>,
        <<"target">> => ID
    }, Wallet), #{}).

http_get_schedule(N, PMsg, From, To) ->
    http_get_schedule(N, PMsg, From, To, <<"application/http">>).

http_get_schedule(N, PMsg, From, To, Format) ->
    ID = hb_message:id(PMsg, all),
    Wallet = hb:wallet(),
    {ok, _} = hb_http:get(N, hb_message:commit(#{
        <<"path">> => <<"/~scheduler@1.0/schedule">>,
        <<"method">> => <<"GET">>,
        <<"target">> => hb_util:human_id(ID),
        <<"from">> => From,
        <<"to">> => To,
        <<"accept">> => Format
    }, Wallet), #{}).

http_get_schedule_redirect_test() ->
    Opts =
        #{
            store =>
                [
                    #{ <<"store-module">> => hb_store_fs, <<"prefix">> => <<"cache-mainnet">> },
                    #{ <<"store-module">> => hb_store_gateway, <<"opts">> => #{} }
                ],
                scheduler_follow_redirects => false
        },
    {N, _Wallet} = http_init(Opts),
    start(),
    ProcID = <<"0syT13r0s0tgPmIed95bJnuSqaD29HQNN8D3ElLSrsc">>,
    Res = hb_http:get(N, <<"/", ProcID/binary, "/schedule">>, #{}),
    ?assertMatch({ok, #{ <<"location">> := Location }} when is_binary(Location), Res).

http_post_schedule_test() ->
    {N, W} = http_init(),
    PMsg = hb_message:commit(test_process(W), W),
    Msg1 = hb_message:commit(#{
        <<"path">> => <<"/~scheduler@1.0/schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> => PMsg
    }, W),
    {ok, _Res} = hb_http:post(N, Msg1, #{}),
    {ok, Res2} =
        http_post_schedule_sign(
            N,
            #{ <<"inner">> => <<"test-message">> },
            PMsg,
            W
        ),
    ?assertEqual(<<"test-message">>, hb_ao:get(<<"body/inner">>, Res2, #{})),
    ?assertMatch({ok, #{ <<"current">> := 1 }}, http_get_slot(N, PMsg)).

http_get_schedule_test_() ->
	{timeout, 20, fun() ->
		{Node, Wallet} = http_init(),
		PMsg = hb_message:commit(test_process(Wallet), Wallet),
		Msg1 = hb_message:commit(#{
			<<"path">> => <<"/~scheduler@1.0/schedule">>,
			<<"method">> => <<"POST">>,
			<<"body">> => PMsg
		}, Wallet),
		Msg2 = hb_message:commit(#{
			<<"path">> => <<"/~scheduler@1.0/schedule">>,
			<<"method">> => <<"POST">>,
			<<"body">> => PMsg
		}, Wallet),
		{ok, _} = hb_http:post(Node, Msg1, #{}),
		lists:foreach(
			fun(_) -> {ok, _} = hb_http:post(Node, Msg2, #{}) end,
			lists:seq(1, 10)
		),
		?assertMatch({ok, #{ <<"current">> := 10 }}, http_get_slot(Node, PMsg)),
		{ok, Schedule} = http_get_schedule(Node, PMsg, 0, 10),
		Assignments = hb_ao:get(<<"assignments">>, Schedule, #{}),
		?assertEqual(
			12, % +1 for the hashpath
			length(maps:values(Assignments))
		)
	end}.
    

http_get_legacy_schedule_test_() ->
    {timeout, 10, fun() ->
        Target = <<"CtOVB2dBtyN_vw3BdzCOrvcQvd9Y1oUGT-zLit8E3qM">>,
        {Node, _Wallet} = http_init(),
        Res = hb_http:get(Node, <<"/~scheduler@1.0/schedule&target=", Target/binary>>, #{}),
        ?assertMatch({ok, #{ <<"assignments">> := As }} when map_size(As) > 0, Res)
    end}.

http_get_legacy_slot_test_() ->
    {timeout, 10, fun() ->
        Target = <<"CtOVB2dBtyN_vw3BdzCOrvcQvd9Y1oUGT-zLit8E3qM">>,
        {Node, _Wallet} = http_init(),
        Res = hb_http:get(Node, <<"/~scheduler@1.0/slot&target=", Target/binary>>, #{}),
        ?assertMatch({ok, #{ <<"current">> := Slot }} when Slot > 0, Res)
    end}.

http_get_legacy_schedule_slot_range_test_() ->
    {timeout, 10, fun() ->
        Target = <<"zrhm4OpfW85UXfLznhdD-kQ7XijXM-s2fAboha0V5GY">>,
        {Node, _Wallet} = http_init(),
        Res = hb_http:get(Node, <<"/~scheduler@1.0/schedule&target=", Target/binary,
            "&from=0&to=10">>, #{}),
        ?event({res, Res}),
        ?assertMatch({ok, #{ <<"assignments">> := As }} when map_size(As) == 11, Res)
    end}.

http_get_legacy_schedule_as_aos2_test_() ->
    {timeout, 10, fun() ->
        Target = <<"CtOVB2dBtyN_vw3BdzCOrvcQvd9Y1oUGT-zLit8E3qM">>,
        {Node, _Wallet} = http_init(),
        {ok, Res} =
            hb_http:get(
                Node,
                #{
                    <<"path">> => <<"/~scheduler@1.0/schedule?target=", Target/binary>>,
                    <<"accept">> => <<"application/aos-2">>,
                    <<"method">> => <<"GET">>
                },
                #{}
            ),
        Decoded = hb_json:decode(hb_ao:get(<<"body">>, Res, #{})),
        ?assertMatch(#{ <<"edges">> := As } when length(As) > 0, Decoded)
    end}.

http_post_legacy_schedule_test_() ->
    {timeout, 10, fun() ->
        {Node, Wallet} = http_init(),
        Target = <<"zrhm4OpfW85UXfLznhdD-kQ7XijXM-s2fAboha0V5GY">>,
        Msg1 = hb_message:commit(#{
            <<"path">> => <<"/~scheduler@1.0/schedule">>,
            <<"method">> => <<"POST">>,
            <<"body">> =>
                hb_message:commit(
                    #{
                        <<"data-protocol">> => <<"ao">>,
                        <<"variant">> => <<"ao.TN.1">>,
                        <<"type">> => <<"Message">>,
                        <<"action">> => <<"ping">>,
                        <<"target">> => Target,
                        <<"test-from">> => hb_util:human_id(hb:address())
                    },
                    Wallet,
                    <<"ans104@1.0">>
                )
        }, Wallet),
        {Status, Res} = hb_http:post(Node, Msg1, #{}),
        ?event({status, Status}),
        ?event({res, Res}),
        ?assertMatch(
            {ok, #{ <<"slot">> := Slot }} when Slot > 0,
            {Status, Res}
        )
    end}.

http_get_json_schedule_test_() ->
	{timeout, 20, fun() ->
		{Node, Wallet} = http_init(),
		PMsg = hb_message:commit(test_process(Wallet), Wallet),
		Msg1 = hb_message:commit(#{
			<<"path">> => <<"/~scheduler@1.0/schedule">>,
			<<"method">> => <<"POST">>,
			<<"body">> => PMsg
		}, Wallet),
		{ok, _} = hb_http:post(Node, Msg1, #{}),
		Msg2 = hb_message:commit(#{
			<<"path">> => <<"/~scheduler@1.0/schedule">>,
			<<"method">> => <<"POST">>,
			<<"body">> =>
				hb_message:commit(
					#{
						<<"inner">> => <<"test">>,
						<<"target">> => hb_util:human_id(hb_message:id(PMsg, all))
					},
					Wallet
				)
			},
			Wallet
		),
		lists:foreach(
			fun(_) -> {ok, _} = hb_http:post(Node, Msg2, #{}) end,
			lists:seq(1, 10)
		),
		?assertMatch({ok, #{ <<"current">> := 10 }}, http_get_slot(Node, PMsg)),
		{ok, Schedule} = http_get_schedule(Node, PMsg, 0, 10, <<"application/aos-2">>),
		?event({schedule, Schedule}),
		JSON = hb_ao:get(<<"body">>, Schedule, #{}),
		Assignments = hb_json:decode(JSON),
		?assertEqual(
			11, % +1 for the hashpath
			length(maps:get(<<"edges">>, Assignments))
		)
	end}.

%%% Benchmarks

single_resolution(Opts) ->
    start(),
    BenchTime = 1,
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    Msg1 = test_process(Wallet),
    ?event({benchmark_start, ?MODULE}),
    MsgToSchedule = hb_message:commit(#{
        <<"type">> => <<"Message">>,
        <<"test-key">> => <<"test-val">>
    }, Wallet),
    Iterations = hb:benchmark(
        fun(_) ->
            MsgX = #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> => MsgToSchedule
            },
            ?assertMatch({ok, _}, hb_ao:resolve(Msg1, MsgX, Opts))
        end,
        BenchTime
    ),
    ?event(benchmark, {scheduled, Iterations}),
    Msg3 = #{
        <<"path">> => <<"slot">>,
        <<"method">> => <<"GET">>,
        <<"process">> => hb_util:human_id(hb_message:id(Msg1, all))
    },
    ?assertMatch({ok, #{ <<"current">> := CurrentSlot }}
            when CurrentSlot == Iterations - 1,
        hb_ao:resolve(Msg1, Msg3, Opts)),
    ?event(bench, {res, Iterations - 1}),
    hb_util:eunit_print(
        "Scheduled ~p messages through AO-Core in ~p seconds (~.2f msg/s)",
        [Iterations, BenchTime, Iterations / BenchTime]
    ),
    ?assert(Iterations > 3).

many_clients(Opts) ->
    BenchTime = 1,
    Processes = hb_opts:get(workers, 25, Opts),
    {Node, Wallet} = http_init(Opts),
    PMsg = hb_message:commit(test_process(Wallet), Wallet),
    Msg1 = hb_message:commit(#{
        <<"path">> => <<"/~scheduler@1.0/schedule">>,
        <<"method">> => <<"POST">>,
        <<"process">> => PMsg,
        <<"body">> => hb_message:commit(#{ <<"inner">> => <<"test">> }, Wallet)
    }, Wallet),
    {ok, _} = hb_http:post(Node, Msg1, Opts),
	    Iterations = hb:benchmark(
        fun(X) ->
            {ok, _} = hb_http:post(Node, Msg1, Opts),
            ?event(bench, {iteration, X, self()})
        end,
        BenchTime,
        Processes
    ),
    ?event({iterations, Iterations}),
    hb_util:eunit_print(
        "Scheduled ~p messages with ~p workers through HTTP in ~ps (~.2f msg/s)",
        [Iterations, Processes, BenchTime, Iterations / BenchTime]
    ),
    {ok, Res} = http_get_slot(Node, PMsg),
    ?event(bench, {res, Res}),
    ?assert(Iterations > 10).

benchmark_suite_test_() ->
	{timeout, 10, fun() -> 
		rand:seed(exsplus, erlang:timestamp()),
		Port = 30000 + rand:uniform(10000),
		Bench = [
			{benchmark, "benchmark", fun single_resolution/1},
			{multihttp_benchmark, "multihttp_benchmark", fun many_clients/1}
		],
		filelib:ensure_dir(
			binary_to_list(Base = <<"cache-TEST/run-">>)
		),
		hb_test_utils:suite_with_opts(Bench, benchmark_suite(Port, Base))
	end}.

benchmark_suite(Port, Base) ->
    PortBin = integer_to_binary(Port),
    [
        #{
            name => fs,
            requires => [hb_store_fs],
            opts => #{
                store => #{ <<"store-module">> => hb_store_fs, 
                    <<"prefix">> => <<Base/binary, PortBin/binary, "-A">>
                },
                scheduling_mode => local_confirmation,
                port => Port
            },
            desc => "FS store, local conf."
        },
        #{
            name => fs_aggressive,
            requires => [hb_store_fs],
            opts => #{
                store => #{ <<"store-module">> => hb_store_fs, 
                    <<"prefix">> => <<Base/binary, PortBin/binary, "-B">>
                },
                scheduling_mode => aggressive,
                port => Port + 1
            },
            desc => "FS store, aggressive conf."
        },
        #{
            name => rocksdb,
            requires => [hb_store_rocksdb],
            opts => #{
                store => #{ <<"store-module">> => hb_store_rocksdb, 
                    <<"prefix">> => <<Base/binary, PortBin/binary, "-C">>
                },
                scheduling_mode => local_confirmation,
                port => Port + 2
            },
            desc => "RocksDB store, local conf."
        },
        #{
            name => rocksdb_aggressive,
            requires => [hb_store_rocksdb],
            opts => #{
                store => #{ <<"store-module">> => hb_store_rocksdb, 
                    <<"prefix">> => <<Base/binary, PortBin/binary, "-D">>
                },
                scheduling_mode => aggressive,
                port => Port + 3
            },
            desc => "RocksDB store, aggressive conf."
        },
        #{
            name => rocksdb_extreme_aggressive_h3,
            requires => [http3],
            opts => #{
                store => #{ <<"store-module">> => hb_store_rocksdb, 
                    <<"prefix">> =>
                          <<
                              Base/binary,
                              "run-",
                              (integer_to_binary(Port+4))/binary
                          >>
                },
                scheduling_mode => aggressive,
                protocol => http3,
                workers => 100
            },
            desc => "100xRocksDB store, aggressive conf, http/3."
        }
    ].