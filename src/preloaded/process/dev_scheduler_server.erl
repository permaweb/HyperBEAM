%%% @doc A long-lived server that schedules messages for a process.
%%% It acts as a deliberate 'bottleneck' to prevent the server accidentally
%%% assigning multiple messages to the same slot.
-module(dev_scheduler_server).
-export([start/3, schedule/2, transfer/3, stop/1]).
-export([info/1]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% By default, we wait 10 seconds for a response from the scheduler before
%%% throwing an error on the client. If the scheduler is not able to sequence
%%% the message within this time, it will be discarded upon recipient by the
%%% server. This avoids situations in which the client did not receive 
%%% confirmation of the assignment, but the scheduler still processes it.
-define(DEFAULT_TIMEOUT, 10000).

%% @doc Start a scheduling server for a given computation. Once the server has
%% started it attempts to register on the message ID for the process definition.
%% If there is already a scheduler registered on the message ID, it will return
%% the existing PID and log a warning.
start(ProcID, Proc, Opts) ->
    ?event(scheduling, {starting_scheduling_server, {proc_id, ProcID}}),
    Ref = make_ref(),
    Caller = self(),
    spawn(
        fun() ->
            % Before we start, register the scheduler name.
            case hb_name:register({<<"scheduler@1.0">>, ProcID}) of
                ok -> ok;
                error ->
                    % Another scheduler is already registered on the process
                    % message ID, so we return the existing PID to the caller
                    % rather than our own.
                    ExistingPid = dev_scheduler_registry:find(ProcID, false, Opts),
                    ?event(
                        warning,
                        {another_scheduler_is_already_registered,
                            {process_message_id, ProcID},
                            {existing_pid, ExistingPid}
                        }
                    ),
                    Caller ! {ok, Ref, ExistingPid}
            end,
            % Write the process to the cache. We are the provider-of-last-resort
            % for this data.
            dev_scheduler_cache:write_spawn(Proc, Opts),
            case hb_opts:get(scheduling_mode, disabled, Opts) of
                disabled ->
                    throw({scheduling_disabled_on_node, {requested_for, ProcID}});
                _ -> ok
            end,
            HashpathAlg = hb_path:hashpath_alg(Proc, Opts),
            {CurrentSlot, BaseStateHashpath} =
                case dev_scheduler_cache:latest(ProcID, Opts) of
                    not_found ->
                        ?event({starting_new_schedule, {proc_id, ProcID}}),
                        {-1, undefined};
                    {Slot, Base} ->
                        {Slot, Base}
                end,
            Transfer = dev_scheduler_cache:read_transfer(ProcID, Opts),
            {Epoch, Transferred} =
                epoch_state(ProcID, CurrentSlot, Transfer, Opts),
            ?event(
                {scheduler_got_process_info,
                    {proc_id, ProcID},
                    {initial_slot, CurrentSlot},
                    {epoch, Epoch},
                    {base_state_hashpath, BaseStateHashpath}
                }
            ),
            case Transferred of
                false ->
                    Caller ! {ok, Ref, self()},
                    server(
                        #{
                            id => ProcID,
                            epoch => Epoch,
                            current => CurrentSlot,
                            base_state_hashpath => BaseStateHashpath,
                            hashpath_alg => HashpathAlg,
                            wallets => commitment_wallets(Proc, Transfer, Opts),
                            committment_spec => commitment_spec(Proc, Opts),
                            mode =>
                                hb_opts:get(
                                    scheduling_mode,
                                    remote_confirmation,
                                    Opts
                                ),
                            opts => Opts
                        }
                    );
                Next ->
                    % The schedule was terminated on this node by a transfer
                    % marker: release the process name and inform the caller
                    % of the successor.
                    hb_name:unregister({<<"scheduler@1.0">>, ProcID}),
                    ?event(scheduling,
                        {schedule_transferred,
                            {proc_id, ProcID},
                            {next, Next}
                        }
                    ),
                    Caller ! {ok, Ref, {transferred, Next}}
            end
        end
    ),
    receive
        {ok, Ref, ServerPID} -> ServerPID
    end.

%% @doc Derive the scheduling epoch of a process, and whether its schedule has
%% been transferred away from this node. A node holding a transfer marker
%% received from a prior scheduler serves the epoch after the marker's; a
%% schedule whose final assignment is a transfer marker is terminated on this
%% node in favor of the successor that the marker names.
epoch_state(ProcID, CurrentSlot, _Transfer, Opts) when CurrentSlot >= 0 ->
    {ok, Latest} = dev_scheduler_cache:read(ProcID, CurrentSlot, Opts),
    Epoch = hb_util:int(hb_ao:get(<<"epoch">>, Latest, 0, Opts)),
    case hb_ao:get(<<"type">>, Latest, not_found, Opts) of
        <<"Scheduler-Transfer">> ->
            {Epoch, hb_ao:get(<<"next-scheduler">>, Latest, Opts)};
        _ ->
            {Epoch, false}
    end;
epoch_state(_ProcID, _CurrentSlot, {ok, Marker}, Opts) ->
    {hb_util:int(hb_ao:get(<<"epoch">>, Marker, 0, Opts)) + 1, false};
epoch_state(_ProcID, _CurrentSlot, not_found, _Opts) ->
    {0, false}.

%% @doc Determine the appropriate list of keys to use to commit assignments for
%% a process. A schedule received through a transfer commits with the identity
%% that the marker names, rather than the process's own scheduler definition.
commitment_wallets(_ProcMsg, {ok, Marker}, Opts) ->
    Next = hb_ao:get(<<"next-scheduler">>, Marker, Opts),
    case hb_opts:as(Next, Opts) of
        {ok, AsOpts} -> [hb_opts:get(priv_wallet, hb:wallet(), AsOpts)];
        {error, not_found} -> []
    end;
commitment_wallets(ProcMsg, not_found, Opts) ->
    SchedulerVal =
        hb_ao:get_first(
            [
                {ProcMsg, <<"scheduler">>},
                {ProcMsg, <<"scheduler-location">>}
            ],
            [],
            Opts
        ),
    lists:filtermap(
        fun(Scheduler) ->
            case hb_opts:as(Scheduler, Opts) of
                {ok, SchedulerOpts} ->
                    case hb_opts:get(priv_wallet, not_found, SchedulerOpts) of
                        not_found -> false;
                        Wallet -> {true, Wallet}
                    end;
                _ ->
                    false
            end
        end,
        dev_scheduler:parse_schedulers(SchedulerVal)
    ).

%% @doc Returns the commitment specification which should be used to commit
%% assignments for a process.
commitment_spec(Proc, Opts) ->
    hb_ao:get(
        <<"scheduler-commitment-spec">>,
        {as, <<"message@1.0">>, Proc},
        hb_opts:get(
            scheduler_default_commitment_spec,
            <<"ans104@1.0">>,
            Opts
        ),
        Opts
    ).

%% @doc Call the appropriate scheduling server to assign a message.
schedule(AOProcID, Message) when is_binary(AOProcID) ->
    schedule(dev_scheduler_registry:find(AOProcID), Message);
schedule(ErlangProcID, Message) ->
    ?event(
        {scheduling_message,
            {proc_id, ErlangProcID},
            {message, Message},
            {is_alive, is_process_alive(ErlangProcID)}
        }
    ),
    AbortTime = scheduler_time() + ?DEFAULT_TIMEOUT,
    ErlangProcID ! {schedule, Message, self(), AbortTime},
    await_assignment(ErlangProcID, Message).

%% @doc Request that the scheduling server terminate the process's schedule
%% with a transfer marker naming the given successor. Returns the signed
%% marker: the final assignment of the epoch.
transfer(ErlangProcID, Message, Next) ->
    ?event(
        {transferring_schedule,
            {proc_id, ErlangProcID},
            {next, Next}
        }
    ),
    AbortTime = scheduler_time() + ?DEFAULT_TIMEOUT,
    ErlangProcID ! {transfer, Message, Next, self(), AbortTime},
    await_assignment(ErlangProcID, Message).

%% @doc Await the server's response to a scheduling request. Servers whose
%% schedule has been transferred away answer with the successor's address.
await_assignment(ErlangProcID, Message) ->
    receive
        {scheduled, Message, Assignment} ->
            Assignment;
        {transferred, Message, Next} ->
            {transferred, Next}
    after ?DEFAULT_TIMEOUT ->
        throw({scheduler_timeout, {proc_id, ErlangProcID}, {message, Message}})
    end.

%% @doc Get the current slot from the scheduling server.
info(ProcID) ->
    ?event({getting_info, {proc_id, ProcID}}),
    ProcID ! {info, self()},
    receive {info, Info} -> Info end.

stop(ProcID) ->
    ?event({stopping_scheduling_server, {proc_id, ProcID}}),
    ProcID ! stop.

%% @doc The main loop of the server. Simply waits for messages to assign and
%% returns the current slot.
server(State) ->
    receive
        {schedule, Message, Reply, AbortTime} ->
            case SchedTime = scheduler_time() > AbortTime of
                true ->
                    % Ignore scheduling requests if they are too old. The
                    % `abort-time' signals to us that the client has already
                    % given up on the request, so in order to maintain
                    % predictability we ignore it.
                    ?event(error,
                        {received_old_schedule_request,
                            {abort_time, AbortTime},
                            {sched_time, SchedTime}
                        }
                    ),
                    server(State);
                false ->
                    server(assign(State, Message, #{}, Reply))
            end;
        {transfer, Message, Next, Reply, AbortTime} ->
            case scheduler_time() > AbortTime of
                true ->
                    ?event(error,
                        {received_old_transfer_request,
                            {abort_time, AbortTime}
                        }
                    ),
                    server(State);
                false ->
                    % Terminate the schedule with a transfer marker as its
                    % final assignment, release the process name, and drain
                    % raced requests with the successor's address. The marker
                    % is written synchronously (regardless of scheduling mode):
                    % unregistering is irreversible, so the epoch's terminating
                    % assignment must be durable before we release the name.
                    Opts = maps:get(opts, State),
                    SyncState =
                        State#{
                            opts := Opts#{ <<"scheduling-mode">> => sync }
                        },
                    try do_assign(SyncState, Message, marker(Next), Reply) of
                        NewState ->
                            hb_name:unregister(
                                {<<"scheduler@1.0">>, maps:get(id, State)}
                            ),
                            transferred(NewState, Next)
                    catch
                        _Class:Reason:Stack ->
                            ?event(
                                {error_transferring,
                                    {reason, Reason},
                                    {trace, Stack}
                                }
                            ),
                            server(State)
                    end
            end;
        {info, Reply} ->
            Reply ! {info, State},
            server(State);
        stop ->
            ?event({stopping_scheduler_server, {proc_id, maps:get(id, State)}}),
            ok
    end.

%% @doc The assignment keys that turn the final assignment of an epoch into a
%% transfer marker.
marker(Next) ->
    #{
        <<"type">> => <<"Scheduler-Transfer">>,
        <<"next-scheduler">> => Next
    }.

%% @doc The post-transfer server loop: scheduling requests are answered with
%% the successor's address, such that requests racing the transfer receive a
%% redirect rather than a timeout. The process name is already released, so
%% no new requests are routed here.
transferred(State, Next) ->
    receive
        {schedule, Message, Reply, _AbortTime} ->
            Reply ! {transferred, Message, Next},
            transferred(State, Next);
        {transfer, Message, _OtherNext, Reply, _AbortTime} ->
            Reply ! {transferred, Message, Next},
            transferred(State, Next);
        {info, Reply} ->
            Reply ! {info, State},
            transferred(State, Next);
        stop -> ok
    end.

%% @doc Assign a message to the next slot.
assign(State, Message, Extra, ReplyPID) ->
    try
        do_assign(State, Message, Extra, ReplyPID)
    catch
        _Class:Reason:Stack ->
            ?event({error_scheduling, {reason, Reason}, {trace, Stack}}),
            State
    end.

%% @doc Generate and store the actual assignment message. `Extra' contains
%% assignment keys beyond the standard set, used to mark transfer markers.
do_assign(State, Message, Extra, ReplyPID) ->
    % Ensure that only committed keys from the message are included in the
    % assignment.
    {ok, OnlyAttested} =
        hb_message:with_only_committed(
            Message,
            Opts = maps:get(opts, State)
        ),
    % Generate parameters for the assignment message and commit to it.
    BaseStateHashpath = base_state(State),
    NextSlot = maps:get(current, State) + 1,
    {Timestamp, Height, Hash} = ar_timestamp:get(),
    Assignment =
        commit_assignment(
            maps:merge(
                #{
                    <<"path">> =>
                        case hb_path:from_message(request, Message, Opts) of
                            undefined -> <<"compute">>;
                            Path -> hb_path:to_binary(Path)
                        end,
                    <<"data-protocol">> => <<"ao">>,
                    <<"variant">> => <<"ao.N.1">>,
                    <<"process">> => hb_util:id(maps:get(id, State)),
                    <<"epoch">> => hb_util:bin(maps:get(epoch, State)),
                    <<"slot">> => NextSlot,
                    <<"block-height">> => Height,
                    <<"block-hash">> => hb_util:human_id(Hash),
                    <<"block-timestamp">> => Timestamp,
                    % Note: Local time on the SU, not Arweave
                    <<"timestamp">> => scheduler_time(),
                    <<"base-hashpath">> => BaseStateHashpath,
                    <<"body">> => OnlyAttested,
                    <<"type">> => <<"Assignment">>
                },
                Extra
            ),
            State
        ),
    DispatchFun =
        fun() ->
            AssignmentID = hb_message:id(Assignment, all),
            ?event(scheduling,
                {assigned,
                    {proc_id, maps:get(id, State)},
                    {slot, NextSlot},
                    {assignment, AssignmentID}
                }
            ),
            maybe_inform_recipient(
                aggressive,
                ReplyPID,
                Message,
                Assignment,
                State
            ),
            ?event(starting_message_write),
            ok = dev_scheduler_cache:write(Assignment, Opts),
            maybe_inform_recipient(
                local_confirmation,
                ReplyPID,
                Message,
                Assignment,
                State
            ),
            ?event(writes_complete),
            ?event(uploading_message),
            hb_client_remote:upload(Message, Opts),
            hb_client_remote:upload(Assignment, Opts),
            ?event(uploads_complete),
            maybe_inform_recipient(
                remote_confirmation,
                ReplyPID,
                Message,
                Assignment,
                State
            )
        end,
    case hb_opts:get(scheduling_mode, sync, Opts) of
        aggressive ->
            spawn(DispatchFun);
        Other ->
            ?event({scheduling_mode, Other}),
            DispatchFun()
    end,
    % Update the state with the next hashpath.
    State#{
        current := NextSlot,
        base_state_hashpath := next_hashpath(BaseStateHashpath, Assignment, State)
    }.

%% @doc Commit to the assignment using all of our appropriate wallets.
commit_assignment(BaseAssignment, State) ->
    Wallets = maps:get(wallets, State),
    Opts = maps:get(opts, State),
    CommittmentSpec = maps:get(committment_spec, State),
    lists:foldr(
        fun(Wallet, Assignment) ->
            hb_message:commit(
                Assignment,
                Opts#{ <<"priv-wallet">> => Wallet },
                CommittmentSpec
            )
        end,
        BaseAssignment,
        Wallets
    ).

%% @doc Potentially inform the caller that the assignment has been scheduled.
%% The main assignment loop calls this function repeatedly at different stages
%% of the assignment process. The scheduling mode determines which stages
%% trigger an update.
maybe_inform_recipient(Mode, ReplyPID, Message, Assignment, State) ->
    case maps:get(mode, State) of
        Mode -> ReplyPID ! {scheduled, Message, Assignment};
        _ -> ok
    end.

%% @doc Find the hashpath of the base state upon which a new assignment should
%% be applied.
base_state(S = #{ base_state_hashpath := undefined }) ->
    hb_util:id(maps:get(id, S));
base_state(#{ base_state_hashpath := BaseStateHashpath }) ->
    BaseStateHashpath.

%% @doc Generate the next hashpath for a new assignment.
next_hashpath(
        BaseStateHashpath,
        NewAssignment,
        #{ hashpath_alg := HashpathAlg, opts := Opts }
    ) ->
    hb_path:hashpath(
        BaseStateHashpath,
        hb_message:id(NewAssignment, all, Opts),
        HashpathAlg,
        Opts
    ).

%% @doc Return the current time in milliseconds.
scheduler_time() ->
    erlang:system_time(millisecond).

%%% Tests

%% @doc Test the basic functionality of the server.
new_proc_test() ->
    Wallet = ar_wallet:new(),
    SignedItem = hb_message:commit(
        #{ <<"data">> => <<"test">>, <<"random-key">> => rand:uniform(10000) },
        #{ <<"priv-wallet">> => Wallet }
    ),
    SignedItem2 = hb_message:commit(
        #{ <<"data">> => <<"test2">> },
        #{ <<"priv-wallet">> => Wallet }
    ),
    SignedItem3 = hb_message:commit(
        #{
            <<"data">> => <<"test2">>,
            <<"deep-key">> =>
                #{ <<"data">> => <<"test3">> }
        },
        #{ <<"priv-wallet">> => Wallet }
    ),
    dev_scheduler_registry:find(hb_message:id(SignedItem, all), SignedItem),
    schedule(ID = hb_message:id(SignedItem, all), SignedItem),
    schedule(ID, SignedItem2),
    schedule(ID, SignedItem3),
    ?assertMatch(
        #{ current := 2 },
        dev_scheduler_server:info(dev_scheduler_registry:find(ID))
    ).
    

benchmark_test() ->
    BenchTime = 1,
    Wallet = ar_wallet:new(),
    Opts = #{ <<"priv-wallet">> => Wallet },
    SignedItem = hb_message:commit(
        #{ <<"data">> => <<"test">>, <<"random-key">> => rand:uniform(10000) },
        Opts
    ),
    ID = hb_message:id(SignedItem, all, Opts),
    dev_scheduler_registry:find(ID, SignedItem, Opts),
    ?event({benchmark_start, ?MODULE}),
    Iterations = hb_test_utils:benchmark(
        fun(X) ->
            MsgX = #{
                <<"path">> => <<"Schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> =>
                    #{
                        <<"type">> => <<"Message">>,
                        <<"test-val">> => X
                    }
            },
            schedule(ID, MsgX)
        end,
        BenchTime
    ),
    hb_format:eunit_print(
        "Scheduled ~p messages in ~p seconds (~.2f msg/s)",
        [Iterations, BenchTime, Iterations / BenchTime]
    ),
    ?assertMatch(
        #{ current := X } when X == Iterations - 1,
        dev_scheduler_server:info(dev_scheduler_registry:find(ID))
    ),
    ?assert(Iterations > 30).
