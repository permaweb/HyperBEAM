%%% @doc A long-lived server that schedules messages for a process.
%%% It acts as a deliberate 'bottleneck' to prevent the server accidentally
%%% assigning multiple messages to the same slot.
-module(dev_scheduler_server).
-export([start/3, schedule/2, stop/1]).
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
            {Epoch, CurrentSlot, BaseStateHashpath} =
                case dev_scheduler_cache:latest_epoch(ProcID, Opts) of
                    not_found ->
                        ?event({starting_new_schedule, {proc_id, ProcID}}),
                        {0, -1, undefined};
                    {CachedEpoch, Slot, Base} ->
                        {CachedEpoch, Slot, Base}
                end,
            GlobalCurrent = global_current(ProcID, Epoch, CurrentSlot, Opts),
            ?event(
                {scheduler_got_process_info,
                    {proc_id, ProcID},
                    {initial_epoch, Epoch},
                    {initial_slot, CurrentSlot},
                    {initial_global_slot, GlobalCurrent},
                    {base_state_hashpath, BaseStateHashpath}
                }
            ),
            Caller ! {ok, Ref, self()},
            State =
                with_scheduler(
                    #{
                        id => ProcID,
                        current => CurrentSlot,
                        global_current => GlobalCurrent,
                        epoch => Epoch,
                        base_state_hashpath => BaseStateHashpath,
                        hashpath_alg => HashpathAlg,
                        wallets => commitment_wallets(Proc, Opts),
                        committment_spec => commitment_spec(Proc, Opts),
                        mode =>
                            hb_opts:get(
                                scheduling_mode,
                                remote_confirmation,
                                Opts
                            ),
                        opts => Opts
                    },
                    epoch_scheduler(ProcID, Epoch, Opts)
                ),
            server(
                maybe_cached_transfer(
                    State,
                    latest_transfer(ProcID, Epoch, CurrentSlot, Opts)
                )
            )
        end
    ),
    receive
        {ok, Ref, ServerPID} -> ServerPID
    end.

%% @doc Determine the appropriate list of keys to use to commit assignments for
%% a process.
commitment_wallets(ProcMsg, Opts) ->
    SchedulerVal =
        hb_ao:get_first(
            [
                {ProcMsg, <<"scheduler">>},
                {ProcMsg, <<"scheduler-location">>}
            ],
            [],
            Opts
        ),
    scheduler_wallets(SchedulerVal, Opts).

%% @doc Return the local wallets for a scheduler name/address list.
scheduler_wallets(SchedulerVal, Opts) ->
    lists:filtermap(
        fun(Scheduler) ->
            Identity = scheduler_identity(Scheduler),
            case hb_opts:as(Identity, Opts) of
                {ok, SchedulerOpts} ->
                    case hb_opts:get(priv_wallet, not_found, SchedulerOpts) of
                        not_found -> false;
                        Wallet ->
                            case hb_util:human_id(Wallet) of
                                Identity -> {true, Wallet};
                                _ -> false
                            end
                    end;
                _ ->
                    false
            end
        end,
        dev_scheduler:parse_schedulers(SchedulerVal)
    ).

%% @doc Return the scheduler identity without a routing hint.
scheduler_identity(Scheduler) when is_binary(Scheduler) ->
    hd(binary:split(Scheduler, <<"?">>));
scheduler_identity(Scheduler) ->
    Scheduler.

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

%% @doc Return the global process slot implied by an epoch-local slot.
global_current(_ProcID, 0, CurrentSlot, _Opts) ->
    CurrentSlot;
global_current(ProcID, Epoch, CurrentSlot, Opts) ->
    lists:sum(
        [
            case dev_scheduler_cache:latest(ProcID, PrevEpoch, Opts) of
                not_found -> 0;
                {Slot, _Base} -> Slot + 1
            end
        ||  PrevEpoch <- lists:seq(0, Epoch - 1)
        ]
    ) + CurrentSlot.

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
    receive
        {scheduled, Message, Assignment} ->
            Assignment;
        {schedule_error, Message, Error} ->
            {error, Error}
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
                    server(assign(State, Message, Reply))
            end;
        {info, Reply} ->
            Reply ! {info, State},
            server(State);
        stop ->
            ?event({stopping_scheduler_server, {proc_id, maps:get(id, State)}}),
            ok
    end.

%% @doc Assign a message to the next slot.
assign(State, Message, ReplyPID) ->
    try
        do_assign(State, Message, ReplyPID)
    catch
        _Class:Reason:Stack ->
            ?event({error_scheduling, {reason, Reason}, {trace, Stack}}),
            State
    end.

%% @doc Generate and store the actual assignment message.
do_assign(State = #{ transferred_to := NextScheduler }, Message, ReplyPID) ->
    ReplyPID !
        {schedule_error,
            Message,
            #{
                <<"status">> => 410,
                <<"body">> => <<"Scheduler has transferred.">>,
                <<"next-scheduler">> => NextScheduler
            }
        },
    State;
do_assign(State, Message, ReplyPID) ->
    % Ensure that only committed keys from the message are included in the
    % assignment.
    {ok, OnlyAttested} =
        hb_message:with_only_committed(
            Message,
            Opts = maps:get(opts, State)
        ),
    case scheduler_transfer(OnlyAttested, State) of
        {error, Error} ->
            ReplyPID ! {schedule_error, Message, Error},
            State;
        Transfer ->
            % Generate parameters for the assignment message and commit to it.
            BaseStateHashpath = base_state(State),
            NextSlot = maps:get(current, State) + 1,
            NextGlobalSlot = maps:get(global_current, State) + 1,
            {Timestamp, Height, Hash} = ar_timestamp:get(),
            Assignment =
                commit_assignment(
                    maybe_transfer_assignment(
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
                            <<"process-slot">> => NextGlobalSlot,
                            <<"block-height">> => Height,
                            <<"block-hash">> => hb_util:human_id(Hash),
                            <<"block-timestamp">> => Timestamp,
                            % Note: Local time on the SU, not Arweave
                            <<"timestamp">> => scheduler_time(),
                            <<"base-hashpath">> => BaseStateHashpath,
                            <<"body">> => OnlyAttested,
                            <<"type">> => <<"Assignment">>
                        },
                        Transfer,
                        State
                    ),
                    State
                ),
            DispatchFun =
                fun() ->
                    AssignmentID = hb_message:id(Assignment, all),
                    ?event(scheduling,
                        {assigned,
                            {proc_id, maps:get(id, State)},
                            {epoch, maps:get(epoch, State)},
                            {slot, NextSlot},
                            {global_slot, NextGlobalSlot},
                            {assignment, AssignmentID}
                        }
                    ),
                    case maybe_post_transfer(Assignment, Transfer, State) of
                        ok ->
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
                            ),
                            ok;
                        {error, Error} ->
                            ReplyPID ! {schedule_error, Message, Error},
                            {error, Error}
                    end
                end,
            DispatchResult =
                case {hb_opts:get(scheduling_mode, sync, Opts), Transfer} of
                    {aggressive, false} ->
                        spawn(DispatchFun),
                        ok;
                    {Other, _} ->
                        ?event({scheduling_mode, Other}),
                        DispatchFun()
                end,
            case DispatchResult of
                ok ->
                    % Update the state with the next hashpath.
                    maybe_transfer_state(
                        State#{
                            current := NextSlot,
                            global_current := NextGlobalSlot,
                            base_state_hashpath :=
                                next_hashpath(BaseStateHashpath, Assignment, State)
                        },
                        Transfer
                    );
                {error, _Error} ->
                    State
            end
    end.

%% @doc Post a terminal marker to a non-local next scheduler.
maybe_post_transfer(_Assignment, false, _State) ->
    ok;
maybe_post_transfer(Assignment, {transfer, NextScheduler, _Scheduler}, State) ->
    Opts = maps:get(opts, State),
    case scheduler_wallets(NextScheduler, Opts) of
        [] ->
            case
                dev_scheduler:post_transfer(
                    maps:get(id, State),
                    NextScheduler,
                    hb_cache:ensure_all_loaded(Assignment, Opts),
                    Opts
                )
            of
                {ok, Res} ->
                    case hb_util:int(hb_ao:get(<<"status">>, Res, 200, Opts)) of
                        Status when Status >= 200 andalso Status < 300 -> ok;
                        _ -> {error, Res}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            ok
    end.

%% @doc Return transfer information for a valid Scheduler-Transfer marker.
scheduler_transfer(Message, State) ->
    Opts = maps:get(opts, State),
    case transfer_message(Message, Opts) of
        {error, Error} ->
            {error, Error};
        {transfer, NextScheduler} ->
            case transfer_authorized(Message, State) of
                true -> {transfer, NextScheduler, current_scheduler(State)};
                false ->
                    {error,
                        #{
                            <<"status">> => 403,
                            <<"body">> =>
                                <<"Scheduler-Transfer is not authorized.">>
                        }
                    }
            end;
        false ->
            false
    end.

%% @doc Return transfer information from an assignment body.
transfer_message(Message, Opts) ->
    case
        lists:any(
            fun(Key) ->
                lists:member(
                    hb_util:to_lower(hb_ao:get(Key, Message, <<>>, Opts)),
                    [<<"scheduler-transfer">>, <<"request-scheduler-transfer">>]
                )
            end,
            [<<"action">>, <<"Action">>, <<"type">>, <<"Type">>]
        )
    of
        true ->
            case hb_ao:get_first(
                [
                    {Message, <<"next-scheduler">>},
                    {Message, <<"Next-Scheduler">>}
                ],
                not_found,
                Opts
            ) of
                not_found ->
                    {error,
                        #{
                            <<"status">> => 400,
                            <<"body">> =>
                                <<"Scheduler-Transfer missing next-scheduler.">>
                        }
                    };
                NextScheduler -> {transfer, NextScheduler}
            end;
        false ->
            false
    end.

%% @doc Return true if a transfer request is authorized by this node.
transfer_authorized(Message, State) ->
    Opts = maps:get(opts, State),
    (node_operator_signed(Message, Opts)
        andalso transfer_request_process_matches(Message, State))
        orelse trusted_process_push_signed(Message, State).

%% @doc Return true if a direct transfer request names this process.
transfer_request_process_matches(Message, State) ->
    Opts = maps:get(opts, State),
    case hb_ao:get(<<"process">>, Message, not_found, Opts) of
        not_found -> false;
        ProcID -> hb_util:id(ProcID) == hb_util:id(maps:get(id, State))
    end.

%% @doc Return true if the node operator signed a transfer request.
node_operator_signed(Message, Opts) ->
    case hb_opts:get(priv_wallet, not_found, Opts) of
        not_found ->
            false;
        Wallet ->
            lists:member(
                hb_util:human_id(Wallet),
                hb_message:signers(Message, Opts)
            )
    end.

%% @doc Return true if a trusted authority signed this process's push.
trusted_process_push_signed(Message, State) ->
    Opts = maps:get(opts, State),
    FromProcess = hb_ao:get(<<"from-process">>, Message, not_found, Opts),
    Authorities =
        lists:map(
            fun hb_util:human_id/1,
            hb_util:binary_to_strings(
                hb_opts:get(scheduler_transfer_authority, [], Opts)
            )
        ),
    (FromProcess =/= not_found)
        andalso (hb_util:id(FromProcess) == hb_util:id(maps:get(id, State)))
        andalso
            lists:any(
                fun(Signer) -> lists:member(Signer, Authorities) end,
                hb_message:signers(Message, Opts)
            )
        andalso process_push_requested_transfer(Message, State).

%% @doc Return true if the process outbox requested this transfer.
process_push_requested_transfer(Message, State) ->
    Opts = maps:get(opts, State),
    FromProcess = hb_ao:get(<<"from-process">>, Message, not_found, Opts),
    CurrentSlot = maps:get(global_current, State),
    case
        {
            hb_util:safe_int(hb_ao:get(<<"from-slot">>, Message, not_found, Opts)),
            hb_ao:get(<<"from-outbox">>, Message, not_found, Opts)
        }
    of
        {{ok, CurrentSlot}, OutboxKey} when OutboxKey =/= not_found ->
            case cached_process_result(FromProcess, CurrentSlot, Opts) of
                {ok, Result} ->
                    Outbox =
                        hb_util:lower_case_keys(
                            hb_ao:normalize_keys(
                                hb_private:reset(
                                    hb_ao:get(<<"outbox">>, Result, #{}, Opts)
                                )
                            ),
                            Opts
                        ),
                    case hb_ao:get(OutboxKey, Outbox, not_found, Opts) of
                        not_found ->
                            false;
                        OutboxMsg ->
                            case
                                {
                                    hb_ao:get(
                                        <<"target">>,
                                        OutboxMsg,
                                        not_found,
                                        Opts
                                    ),
                                    transfer_message(OutboxMsg, Opts),
                                    transfer_message(Message, Opts)
                                }
                            of
                                {
                                    Target,
                                    {transfer, NextScheduler},
                                    {transfer, NextScheduler}
                                } ->
                                    hb_util:id(Target) == hb_util:id(FromProcess);
                                _ ->
                                    false
                            end
                    end;
                _ ->
                    false
            end;
        _ ->
            false
    end.

%% @doc Read a cached process result without invoking process execution.
cached_process_result(ProcID, Slot, Opts) ->
    hb_cache:read(process_result_path(ProcID, Slot), Opts).

%% @doc Return the cache path for a process result slot.
process_result_path(ProcID, Slot) ->
    hb_path:to_binary([
        <<"computed">>,
        hb_util:human_id(ProcID),
        <<"slot">>,
        integer_to_binary(Slot)
    ]).

%% @doc Return the current scheduler assignment address.
current_scheduler(State) ->
    case maps:get(wallets, State) of
        [Wallet | _] -> hb_util:human_id(ar_wallet:to_address(Wallet));
        [] -> hb_util:human_id(hb_opts:get(priv_wallet, not_found, maps:get(opts, State)))
    end.

%% @doc Add mainnet transfer metadata to the terminal assignment.
maybe_transfer_assignment(Assignment, false, _State) ->
    Assignment;
maybe_transfer_assignment(
        Assignment,
        {transfer, NextScheduler, Scheduler},
        State
    ) ->
    WithTransfer = Assignment#{
        <<"scheduler">> => Scheduler,
        <<"next-scheduler">> => NextScheduler,
        <<"length">> => hb_ao:get(<<"slot">>, Assignment, maps:get(opts, State)) + 1
    },
    case previous_transfer(State) of
        false -> WithTransfer;
        Previous -> WithTransfer#{ <<"previous-transfer">> => Previous }
    end.

%% @doc Return the terminal marker that authorized the current epoch.
previous_transfer(#{ epoch := 0 }) ->
    false;
previous_transfer(#{ id := ProcID, epoch := Epoch, opts := Opts }) ->
    case dev_scheduler_cache:latest(ProcID, Epoch - 1, Opts) of
        not_found ->
            false;
        {Slot, _Base} ->
            case dev_scheduler_cache:read(ProcID, Epoch - 1, Slot, Opts) of
                {ok, Assignment} ->
                    case transfer_assignment(Assignment, Opts) of
                        false -> false;
                        _ -> hb_cache:ensure_all_loaded(Assignment, Opts)
                    end;
                not_found ->
                    false
            end
    end.

%% @doc Move local scheduling to the next epoch, or mark the process remote.
maybe_transfer_state(State, false) ->
    State;
maybe_transfer_state(State, {transfer, NextScheduler, _Scheduler}) ->
    transfer_state(
        State,
        NextScheduler,
        maps:get(epoch, State) + 1
    ).

%% @doc Return the scheduler that owns an already-cached epoch.
epoch_scheduler(_ProcID, 0, _Opts) ->
    false;
epoch_scheduler(ProcID, Epoch, Opts) ->
    case dev_scheduler_cache:latest(ProcID, Epoch - 1, Opts) of
        not_found ->
            false;
        {Slot, _Base} ->
            case latest_transfer(ProcID, Epoch - 1, Slot, Opts) of
                false -> false;
                {transfer, NextScheduler, _Slot, _Epoch} ->
                    {transfer, NextScheduler}
            end
    end.

%% @doc Restore transfer state from the latest cached terminal marker.
latest_transfer(_ProcID, _Epoch, Slot, _Opts) when Slot < 0 ->
    false;
latest_transfer(ProcID, Epoch, Slot, Opts) ->
    case dev_scheduler_cache:read(ProcID, Epoch, Slot, Opts) of
        {ok, Assignment} ->
            transfer_assignment(Assignment, Opts);
        not_found ->
            false
    end.

%% @doc Return transfer state encoded by a terminal assignment.
transfer_assignment(Assignment, Opts) ->
    case
        {
            transfer_message(hb_ao:get(<<"body">>, Assignment, #{}, Opts), Opts),
            hb_ao:get(<<"next-scheduler">>, Assignment, not_found, Opts),
            transfer_length_matches(Assignment, Opts)
        }
    of
        {{transfer, NextScheduler}, NextScheduler, true} ->
            {
                transfer,
                NextScheduler,
                hb_util:int(hb_ao:get(<<"slot">>, Assignment, Opts)),
                hb_util:int(
                    hb_ao:get(<<"epoch">>, Assignment, <<"0">>, Opts)
                )
            };
        _ ->
            false
    end.

%% @doc Return true if the marker length terminates the epoch at its slot.
transfer_length_matches(Assignment, Opts) ->
    try
        Slot = hb_util:int(hb_ao:get(<<"slot">>, Assignment, Opts)),
        Length = hb_util:int(hb_ao:get(<<"length">>, Assignment, Opts)),
        Slot >= 0 andalso Length == Slot + 1
    catch
        _:_ -> false
    end.

%% @doc Apply a cached terminal marker when restarting a scheduler.
maybe_cached_transfer(State, false) ->
    State;
maybe_cached_transfer(State, {transfer, NextScheduler, _Slot, Epoch}) ->
    transfer_state(State, NextScheduler, Epoch + 1).

%% @doc Move scheduling to an epoch whose first assignment is slot `0`.
transfer_state(State, NextScheduler, Epoch) ->
    with_scheduler(
        State#{ epoch := Epoch, current := -1 },
        NextScheduler
    ).

%% @doc Use a scheduler locally when possible, else mark it as remote.
with_scheduler(State, false) ->
    State;
with_scheduler(State, {transfer, NextScheduler}) ->
    with_scheduler(State, NextScheduler);
with_scheduler(State, NextScheduler) ->
    case scheduler_wallets(NextScheduler, maps:get(opts, State)) of
        [] ->
            State#{ transferred_to => NextScheduler };
        Wallets ->
            maps:remove(
                transferred_to,
                State#{ wallets := Wallets }
            )
    end.

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

%% @doc Build common scheduler-transfer test fixtures.
transfer_fixture(LocalOld, LocalNew) ->
    dev_scheduler_registry:start(),
    OldWallet = ar_wallet:new(),
    NewWallet = ar_wallet:new(),
    OperatorWallet = ar_wallet:new(),
    UserWallet = ar_wallet:new(),
    OldScheduler = hb_util:human_id(ar_wallet:to_address(OldWallet)),
    NewScheduler = hb_util:human_id(ar_wallet:to_address(NewWallet)),
    Opts =
        #{
            <<"priv-wallet">> => OperatorWallet,
            <<"store">> => hb_test_utils:test_store(),
            <<"scheduling-mode">> => local_confirmation,
            <<"identities">> =>
                maps:merge(
                    transfer_identity(LocalOld, OldScheduler, OldWallet),
                    transfer_identity(LocalNew, NewScheduler, NewWallet)
                )
        },
    Sign =
        fun(Wallet, Msg) ->
            hb_message:commit(Msg, Opts#{ <<"priv-wallet">> => Wallet })
        end,
    Proc =
        Sign(
            OldWallet,
            #{
                <<"device">> => <<"scheduler@1.0">>,
                <<"type">> => <<"Process">>,
                <<"scheduler-location">> => OldScheduler
            }
        ),
    #{
        old_wallet => OldWallet,
        new_wallet => NewWallet,
        operator_wallet => OperatorWallet,
        user_wallet => UserWallet,
        old_scheduler => OldScheduler,
        new_scheduler => NewScheduler,
        opts => Opts,
        sign => Sign,
        proc => Proc,
        proc_id => lib_process:process_id(Proc, #{}, Opts)
    }.

%% @doc Return a test identity map when the scheduler is local.
transfer_identity(true, Scheduler, Wallet) ->
    #{ Scheduler => #{ <<"priv-wallet">> => Wallet } };
transfer_identity(false, _Scheduler, _Wallet) ->
    #{}.

%% @doc Return the transfer request body for a transfer fixture.
transfer_body(#{
        proc_id := ProcID,
        old_scheduler := OldScheduler,
        new_scheduler := NewScheduler
    }) ->
    #{
        <<"type">> => <<"Scheduler-Transfer">>,
        <<"action">> => <<"Noop">>,
        <<"process">> => ProcID,
        <<"scheduler">> => OldScheduler,
        <<"next-scheduler">> => NewScheduler
    }.

%% @doc Sign a simple test message with the fixture user wallet.
transfer_test_message(#{ sign := Sign, user_wallet := UserWallet }, Body) ->
    Sign(UserWallet, #{ <<"type">> => <<"Message">>, <<"body">> => Body }).

%% @doc Resolve a test schedule POST.
resolve_schedule(Proc, Body, Opts) ->
    hb_ao:resolve(
        Proc,
        #{ <<"method">> => <<"POST">>, <<"path">> => <<"schedule">>, <<"body">> => Body },
        Opts
    ).

%% @doc Return a signed terminal assignment for remote handoff tests.
transfer_test_assignment(
        T = #{
            old_wallet := OldWallet,
            operator_wallet := OperatorWallet,
            proc_id := ProcID,
            old_scheduler := OldScheduler,
            new_scheduler := NewScheduler,
            sign := Sign
        }
    ) ->
    Epoch = maps:get(epoch, T, 0),
    Slot = maps:get(slot, T, 1),
    Base =
        case maps:get(previous_transfer, T, false) of
            false -> #{};
            Previous ->
                #{ <<"previous-transfer">> =>
                    hb_cache:ensure_all_loaded(Previous, maps:get(opts, T))
                }
        end,
    hb_cache:ensure_all_loaded(
        Sign(
            OldWallet,
            Base#{
                <<"data-protocol">> => <<"ao">>,
                <<"variant">> => <<"ao.N.1">>,
                <<"process">> => ProcID,
                <<"epoch">> => hb_util:bin(Epoch),
                <<"slot">> => Slot,
                <<"base-hashpath">> => ProcID,
                <<"body">> => Sign(OperatorWallet, transfer_body(T)),
                <<"type">> => <<"Assignment">>,
                <<"scheduler">> => OldScheduler,
                <<"next-scheduler">> => NewScheduler,
                <<"length">> => Slot + 1
            }
        ),
        maps:get(opts, T)
    ).

%% @doc Test that an authorized transfer terminates one epoch and starts another.
scheduler_transfer_test() ->
    T =
        #{
            opts := Opts,
            sign := Sign,
            proc := Proc,
            proc_id := ProcID,
            operator_wallet := OperatorWallet,
            user_wallet := UserWallet,
            old_scheduler := OldScheduler,
            new_scheduler := NewScheduler
        } =
            transfer_fixture(true, true),
    PID = dev_scheduler_registry:find(ProcID, Proc, Opts),
    Rejected = schedule(PID, Sign(UserWallet, transfer_body(T))),
    WrongProcess =
        schedule(
            PID,
            Sign(
                OperatorWallet,
                transfer_body(T#{
                    proc_id :=
                        hb_util:human_id(ar_wallet:to_address(ar_wallet:new()))
                })
            )
        ),
    Malformed =
        schedule(
            PID,
            Sign(
                OperatorWallet,
                maps:remove(<<"next-scheduler">>, transfer_body(T))
            )
        ),
    A0 = schedule(PID, transfer_test_message(T, <<"before">>)),
    AT = schedule(PID, Sign(OperatorWallet, transfer_body(T))),
    stop(PID),
    timer:sleep(10),
    PID2 = dev_scheduler_registry:find(ProcID, Proc, Opts),
    A1 = schedule(PID2, transfer_test_message(T, <<"after">>)),
    {ok, Epoch1Schedule} =
        hb_ao:resolve(
            Proc,
            #{
                <<"method">> => <<"GET">>,
                <<"path">> => <<"schedule">>,
                <<"epoch">> => 1,
                <<"from">> => <<"-1">>
            },
            Opts
        ),
    stop(PID2),
    timer:sleep(10),
    PID3 = dev_scheduler_registry:find(ProcID, Proc, Opts),
    A2 = schedule(PID3, transfer_test_message(T, <<"after restart">>)),
    ?assertMatch({error, #{ <<"status">> := 403 }}, Rejected),
    ?assertMatch({error, #{ <<"status">> := 403 }}, WrongProcess),
    ?assertMatch({error, #{ <<"status">> := 400 }}, Malformed),
    ?assertEqual(0, hb_ao:get(<<"slot">>, A0, Opts)),
    ?assertEqual(<<"0">>, hb_ao:get(<<"epoch">>, A0, Opts)),
    ?assert(lists:member(OldScheduler, hb_message:signers(A0, Opts))),
    ?assertEqual(0, hb_ao:get(<<"process-slot">>, A0, Opts)),
    ?assertEqual(1, hb_ao:get(<<"slot">>, AT, Opts)),
    ?assertEqual(<<"0">>, hb_ao:get(<<"epoch">>, AT, Opts)),
    ?assertEqual(1, hb_ao:get(<<"process-slot">>, AT, Opts)),
    ?assertEqual(2, hb_ao:get(<<"length">>, AT, Opts)),
    ?assertEqual(NewScheduler, hb_ao:get(<<"next-scheduler">>, AT, Opts)),
    ?assertEqual(false, transfer_assignment(
        AT#{ <<"next-scheduler">> => OldScheduler },
        Opts
    )),
    ?assertEqual(false, transfer_assignment(AT#{ <<"length">> => 3 }, Opts)),
    ?assert(lists:member(OldScheduler, hb_message:signers(AT, Opts))),
    ?assertEqual(0, hb_ao:get(<<"slot">>, A1, Opts)),
    ?assertEqual(<<"1">>, hb_ao:get(<<"epoch">>, A1, Opts)),
    ?assertEqual(2, hb_ao:get(<<"process-slot">>, A1, Opts)),
    ?assert(lists:member(NewScheduler, hb_message:signers(A1, Opts))),
    ?assertNot(lists:member(OldScheduler, hb_message:signers(A1, Opts))),
    ?assertEqual(0, hb_ao:get(<<"assignments/0/slot">>, Epoch1Schedule, Opts)),
    ?assertEqual(1, hb_ao:get(<<"slot">>, A2, Opts)),
    ?assertEqual(<<"1">>, hb_ao:get(<<"epoch">>, A2, Opts)),
    ?assertEqual(3, hb_ao:get(<<"process-slot">>, A2, Opts)),
    ?assert(lists:member(NewScheduler, hb_message:signers(A2, Opts))),
    ?assertNot(lists:member(OldScheduler, hb_message:signers(A2, Opts))),
    ?assertMatch(#{ current := 1, global_current := 3, epoch := 1 }, info(PID3)).

%% @doc Test process self-requested transfer via a trusted push signer.
process_requested_scheduler_transfer_test() ->
    T0 =
        #{
            opts := Opts0,
            proc := Proc,
            proc_id := ProcID,
            operator_wallet := OperatorWallet,
            new_scheduler := NewScheduler
        } =
            transfer_fixture(true, true),
    ThirdWallet = ar_wallet:new(),
    AuthorityWallet = ar_wallet:new(),
    ThirdScheduler = hb_util:human_id(ar_wallet:to_address(ThirdWallet)),
    Authority = hb_util:human_id(ar_wallet:to_address(AuthorityWallet)),
    Identities = maps:get(<<"identities">>, Opts0),
    Opts =
        Opts0#{
            <<"scheduler-transfer-authority">> => [Authority],
            <<"identities">> =>
                Identities#{
                    ThirdScheduler => #{ <<"priv-wallet">> => ThirdWallet },
                    Authority => #{ <<"priv-wallet">> => AuthorityWallet }
                }
        },
    Sign =
        fun(Wallet, Msg) ->
            hb_message:commit(Msg, Opts#{ <<"priv-wallet">> => Wallet })
        end,
    T = T0#{ opts := Opts, sign := Sign },
    PID = dev_scheduler_registry:find(ProcID, Proc, Opts),
    schedule(PID, Sign(OperatorWallet, transfer_body(T))),
    Req = schedule(PID, transfer_test_message(T, <<"request transfer">>)),
    ReqSlot = hb_ao:get(<<"process-slot">>, Req, Opts),
    OutboxKey = <<"1">>,
    OutboxMsg =
        #{
            <<"target">> => ProcID,
            <<"action">> => <<"Request-Scheduler-Transfer">>,
            <<"next-scheduler">> => ThirdScheduler
        },
    PushedTransfer =
        Sign(
            AuthorityWallet,
            OutboxMsg#{
                <<"data-protocol">> => <<"ao">>,
                <<"variant">> => <<"ao.N.1">>,
                <<"type">> => <<"Message">>,
                <<"from-process">> => ProcID,
                <<"from-slot">> => ReqSlot,
                <<"from-outbox">> => OutboxKey
            }
        ),
    ?assertMatch(
        {error, #{ <<"status">> := 403 }},
        schedule(PID, PushedTransfer)
    ),
    {ok, OutboxRoot} =
        hb_cache:write(
            hb_private:reset(#{ <<"outbox">> => #{ OutboxKey => OutboxMsg } }),
            Opts
        ),
    ok =
        hb_cache:link(
            OutboxRoot,
            process_result_path(ProcID, ReqSlot),
            Opts
        ),
    AT1 = schedule(PID, PushedTransfer),
    Replay = schedule(PID, PushedTransfer),
    AfterTransfer = schedule(PID, transfer_test_message(T, <<"after transfer">>)),
    AT1Body = hb_ao:get(<<"body">>, AT1, Opts),
    ?assertEqual(0, hb_ao:get(<<"slot">>, Req, Opts)),
    ?assertEqual(<<"1">>, hb_ao:get(<<"epoch">>, Req, Opts)),
    ?assertEqual(1, ReqSlot),
    ?assert(lists:member(NewScheduler, hb_message:signers(Req, Opts))),
    ?assertEqual(1, hb_ao:get(<<"slot">>, AT1, Opts)),
    ?assertEqual(<<"1">>, hb_ao:get(<<"epoch">>, AT1, Opts)),
    ?assertEqual(2, hb_ao:get(<<"process-slot">>, AT1, Opts)),
    ?assertEqual(ThirdScheduler, hb_ao:get(<<"next-scheduler">>, AT1, Opts)),
    ?assertEqual(ProcID, hb_ao:get(<<"from-process">>, AT1Body, Opts)),
    ?assertEqual(ReqSlot, hb_ao:get(<<"from-slot">>, AT1Body, Opts)),
    ?assertEqual(OutboxKey, hb_ao:get(<<"from-outbox">>, AT1Body, Opts)),
    ?assert(lists:member(Authority, hb_message:signers(AT1Body, Opts))),
    ?assertMatch({error, #{ <<"status">> := 403 }}, Replay),
    ?assertEqual(0, hb_ao:get(<<"slot">>, AfterTransfer, Opts)),
    ?assertEqual(<<"2">>, hb_ao:get(<<"epoch">>, AfterTransfer, Opts)),
    ?assertEqual(3, hb_ao:get(<<"process-slot">>, AfterTransfer, Opts)),
    ?assert(lists:member(ThirdScheduler, hb_message:signers(AfterTransfer, Opts))),
    stop(PID).

%% @doc Test that a failed remote handoff leaves the old scheduler active.
scheduler_remote_transfer_post_failure_test() ->
    T0 =
        #{
            opts := Opts,
            sign := Sign,
            proc := Proc,
            proc_id := ProcID,
            operator_wallet := OperatorWallet,
            old_scheduler := OldScheduler
        } =
            transfer_fixture(true, false),
    T =
        T0#{
            new_scheduler :=
                <<(maps:get(new_scheduler, T0))/binary,
                    "?hint=http://127.0.0.1:1">>
        },
    ?assertMatch(
        {error, #{ <<"status">> := 403 }},
        resolve_schedule(Proc, transfer_test_assignment(T), Opts)
    ),
    PID = dev_scheduler_registry:find(ProcID, Proc, Opts),
    A0 = schedule(PID, transfer_test_message(T, <<"before">>)),
    Failed = schedule(PID, Sign(OperatorWallet, transfer_body(T))),
    A1 = schedule(PID, transfer_test_message(T, <<"after">>)),
    ?assertEqual(0, hb_ao:get(<<"slot">>, A0, Opts)),
    ?assertMatch({error, _}, Failed),
    ?assertEqual(1, hb_ao:get(<<"slot">>, A1, Opts)),
    ?assertEqual(<<"0">>, hb_ao:get(<<"epoch">>, A1, Opts)),
    ?assert(lists:member(OldScheduler, hb_message:signers(A1, Opts))),
    ?assertMatch(#{ current := 1, global_current := 1, epoch := 0 }, info(PID)).

%% @doc Test that a transferred-away scheduler rejects direct PID scheduling.
transferred_server_rejects_direct_schedule_test() ->
    NextScheduler = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Message = #{ <<"nonce">> => crypto:strong_rand_bytes(32) },
    State = #{ transferred_to => NextScheduler },
    ?assertEqual(State, do_assign(State, Message, self())),
    receive
        {schedule_error,
            Message,
            #{ <<"status">> := 410, <<"next-scheduler">> := NextScheduler }} ->
            ok
    after 1000 ->
        error(no_schedule_error)
    end.

%% @doc Test that a remote terminal marker hands scheduling to this node.
scheduler_remote_transfer_handoff_test() ->
    T =
        #{
            opts := Opts,
            proc := Proc,
            proc_id := ProcID,
            new_wallet := NewWallet,
            new_scheduler := NewScheduler
        } =
            transfer_fixture(false, true),
    TransferAssignment = transfer_test_assignment(T),
    AttackerWallet = ar_wallet:new(),
    AttackerScheduler = hb_util:human_id(ar_wallet:to_address(AttackerWallet)),
    ForgedAssignment =
        transfer_test_assignment(
            T#{
                old_wallet := AttackerWallet,
                old_scheduler := AttackerScheduler
            }
        ),
    ?assertMatch(
        {error, #{ <<"status">> := 403 }},
        resolve_schedule(Proc, ForgedAssignment, Opts)
    ),
    ?assertMatch(
        {error, #{ <<"status">> := 403 }},
        resolve_schedule(Proc, TransferAssignment#{ <<"slot">> => 0 }, Opts)
    ),
    ?assertMatch(
        {error, #{ <<"status">> := 403 }},
        resolve_schedule(
            Proc,
            TransferAssignment#{ <<"slot">> => -1, <<"length">> => 0 },
            Opts
        )
    ),
    ?assertMatch(
        {error, #{ <<"status">> := 403 }},
        resolve_schedule(
            Proc,
            TransferAssignment,
            Opts#{
                <<"identities">> =>
                    #{ NewScheduler => #{ <<"priv-wallet">> => AttackerWallet } }
            }
        )
    ),
    {ok, AcceptedTransfer} = resolve_schedule(Proc, TransferAssignment, Opts),
    ?assertEqual(1, hb_ao:get(<<"slot">>, AcceptedTransfer, Opts)),
    ?assertEqual(<<"0">>, hb_ao:get(<<"epoch">>, AcceptedTransfer, Opts)),
    ?assertEqual(NewScheduler, hb_ao:get(<<"next-scheduler">>, AcceptedTransfer, Opts)),
    stop(dev_scheduler_registry:find(ProcID, Proc, Opts)),
    timer:sleep(10),
    {ok, A1} = resolve_schedule(Proc, transfer_test_message(T, <<"after">>), Opts),
    ?assertEqual(0, hb_ao:get(<<"slot">>, A1, Opts)),
    ?assertEqual(<<"1">>, hb_ao:get(<<"epoch">>, A1, Opts)),
    ?assert(lists:member(NewScheduler, hb_message:signers(A1, Opts))),
    PID = dev_scheduler_registry:find(ProcID, Proc, Opts),
    stop(PID),
    timer:sleep(10),
    {ok, A2} = resolve_schedule(Proc, transfer_test_message(T, <<"again">>), Opts),
    ?assertEqual(1, hb_ao:get(<<"slot">>, A2, Opts)),
    ?assertEqual(<<"1">>, hb_ao:get(<<"epoch">>, A2, Opts)),
    ?assert(lists:member(NewScheduler, hb_message:signers(A2, Opts))),
    NextWallet = ar_wallet:new(),
    NextScheduler = hb_util:human_id(ar_wallet:to_address(NextWallet)),
    FreshOpts =
        Opts#{
            <<"store">> => hb_test_utils:test_store(),
            <<"identities">> =>
                #{ NextScheduler => #{ <<"priv-wallet">> => NextWallet } }
        },
    Transfer2WithoutProof =
        transfer_test_assignment(
            T#{
                old_wallet := NewWallet,
                old_scheduler := NewScheduler,
                new_scheduler := NextScheduler,
                epoch => 1
            }
        ),
    ?assertMatch(
        {error, #{ <<"status">> := 403 }},
        resolve_schedule(Proc, Transfer2WithoutProof, FreshOpts)
    ),
    PoisonedPrev =
        transfer_test_assignment(
            T#{
                old_wallet := AttackerWallet,
                old_scheduler := AttackerScheduler,
                new_scheduler := NewScheduler
            }
        ),
    PoisonedOpts =
        FreshOpts#{ <<"store">> => hb_test_utils:test_store() },
    ok = dev_scheduler_cache:write(PoisonedPrev, PoisonedOpts),
    ?assertMatch(
        {error, #{ <<"status">> := 403 }},
        resolve_schedule(Proc, Transfer2WithoutProof, PoisonedOpts)
    ),
    MalformedProofTransfer =
        transfer_test_assignment(
            T#{
                old_wallet := NewWallet,
                old_scheduler := NewScheduler,
                new_scheduler := NextScheduler,
                epoch => 1,
                previous_transfer => TransferAssignment#{ <<"epoch">> => <<"bad">> }
            }
        ),
    ?assertMatch(
        {error, #{ <<"status">> := 403 }},
        resolve_schedule(Proc, MalformedProofTransfer, FreshOpts)
    ),
    Transfer2 =
        transfer_test_assignment(
            T#{
                old_wallet := NewWallet,
                old_scheduler := NewScheduler,
                new_scheduler := NextScheduler,
                epoch => 1,
                previous_transfer => TransferAssignment
            }
        ),
    {ok, AcceptedTransfer2} = resolve_schedule(Proc, Transfer2, FreshOpts),
    ?assertEqual(<<"1">>, hb_ao:get(<<"epoch">>, AcceptedTransfer2, FreshOpts)),
    ?assertEqual(NextScheduler, hb_ao:get(<<"next-scheduler">>, AcceptedTransfer2, FreshOpts)),
    {ok, A3} = resolve_schedule(Proc, transfer_test_message(T, <<"again">>), FreshOpts),
    ?assertEqual(0, hb_ao:get(<<"slot">>, A3, FreshOpts)),
    ?assertEqual(<<"2">>, hb_ao:get(<<"epoch">>, A3, FreshOpts)),
    ?assert(lists:member(NextScheduler, hb_message:signers(A3, FreshOpts))),
    ?assertMatch(
        #{ current := 0, global_current := 4, epoch := 2 },
        info(dev_scheduler_registry:find(ProcID, Proc, FreshOpts))
    ).

%% @doc Test that cached handoff proof does not persist junk embedded markers.
scheduler_remote_transfer_ignores_invalid_embedded_previous_test() ->
    T =
        #{
            opts := Opts,
            proc := Proc,
            proc_id := ProcID,
            new_wallet := NewWallet,
            new_scheduler := NewScheduler
        } =
            transfer_fixture(false, false),
    TransferAssignment = transfer_test_assignment(T),
    NextWallet = ar_wallet:new(),
    NextScheduler = hb_util:human_id(ar_wallet:to_address(NextWallet)),
    PoisonOpts =
        Opts#{
            <<"identities">> =>
                #{
                    NewScheduler => #{ <<"priv-wallet">> => NewWallet },
                    NextScheduler => #{ <<"priv-wallet">> => NextWallet }
                }
        },
    {ok, _AcceptedTransfer} = resolve_schedule(Proc, TransferAssignment, PoisonOpts),
    stop(dev_scheduler_registry:find(ProcID, Proc, PoisonOpts)),
    timer:sleep(10),
    JunkProcID = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    JunkTransfer = transfer_test_assignment(T#{ proc_id := JunkProcID }),
    PoisonedTransfer =
        transfer_test_assignment(
            T#{
                old_wallet := NewWallet,
                old_scheduler := NewScheduler,
                new_scheduler := NextScheduler,
                epoch => 1,
                previous_transfer => JunkTransfer
            }
        ),
    {ok, AcceptedTransfer} = resolve_schedule(Proc, PoisonedTransfer, PoisonOpts),
    ?assertEqual(<<"1">>, hb_ao:get(<<"epoch">>, AcceptedTransfer, PoisonOpts)),
    ?assertEqual(not_found, dev_scheduler_cache:latest(JunkProcID, 0, PoisonOpts)),
    stop(dev_scheduler_registry:find(ProcID, Proc, PoisonOpts)).


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
