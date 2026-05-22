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
            {CurrentSlot, BaseStateHashpath} =
                case dev_scheduler_cache:latest(ProcID, Opts) of
                    not_found ->
                        ?event({starting_new_schedule, {proc_id, ProcID}}),
                        {-1, undefined};
                    {Slot, Base} ->
                        {Slot, Base}
                end,
            ?event(
                {scheduler_got_process_info,
                    {proc_id, ProcID},
                    {initial_slot, CurrentSlot},
                    {base_state_hashpath, BaseStateHashpath}
                }
            ),
            Caller ! {ok, Ref, self()},
            server(
                #{
                    id => ProcID,
                    current => CurrentSlot,
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
                }
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
    receive
        {scheduled, Message, Assignment} ->
            Assignment;
        {schedule_failed, Message, Reason} ->
            throw({scheduler_error, {proc_id, ErlangProcID}, {reason, Reason}})
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
        Class:Reason:Stack ->
            ?event({error_scheduling, {reason, Reason}, {trace, Stack}}),
            ReplyPID ! {schedule_failed, Message, {Class, Reason, Stack}},
            State
    end.

%% @doc Generate and store the actual assignment message.
do_assign(State, Message, ReplyPID) ->
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
            #{
                <<"path">> =>
                    case hb_path:from_message(request, Message, Opts) of
                        undefined -> <<"compute">>;
                        Path -> hb_path:to_binary(Path)
                    end,
                <<"data-protocol">> => <<"ao">>,
                <<"variant">> => <<"ao.N.1">>,
                <<"process">> => hb_util:id(maps:get(id, State)),
                <<"epoch">> => <<"0">>,
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
            CommitmentSpec = maps:get(committment_spec, State),
            CommitmentDevice = commitment_device(CommitmentSpec),
            UploadOpts = upload_opts(State),
            ?event(
                {uploading_message,
                    {commitment_spec, CommitmentSpec},
                    {commitment_device, CommitmentDevice}
                }
            ),
            ok = upload_with_commitment(Message, UploadOpts, CommitmentSpec),
            ok = upload_assignment(Assignment, State, UploadOpts, CommitmentSpec),
            ?event(uploads_complete),
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
    lists:foldl(
        fun(Wallet, Acc) ->
            Signed =
                hb_message:commit(
                    BaseAssignment,
                    Opts#{ <<"priv-wallet">> => Wallet },
                    CommittmentSpec
                ),
            merge_commitments(Acc, Signed)
        end,
        BaseAssignment,
        Wallets
    ).

merge_commitments(Base, Signed) ->
    Signed#{
        <<"commitments">> =>
            maps:merge(
                maps:get(<<"commitments">>, Base, #{}),
                maps:get(<<"commitments">>, Signed, #{})
            )
    }.

%% @doc Ensure an upload target is committed with the scheduler's commitment
%% spec before asking the remote uploader to publish it using that spec.
ensure_committed(Msg, Opts, CommitmentSpec) ->
    Device = commitment_device(CommitmentSpec),
    case lists:member(Device, hb_message:commitment_devices(Msg, Opts)) of
        true -> Msg;
        false -> hb_message:commit(Msg, Opts, CommitmentSpec)
    end.

commitment_device(CommitmentSpec) when is_binary(CommitmentSpec) ->
    CommitmentSpec;
commitment_device(CommitmentSpec) ->
    maps:get(<<"commitment-device">>, CommitmentSpec).

upload_opts(#{ opts := Opts, wallets := [Wallet | _] }) ->
    case maps:is_key(<<"priv-wallet">>, Opts) of
        true -> Opts;
        false -> Opts#{ <<"priv-wallet">> => Wallet }
    end;
upload_opts(#{ opts := Opts }) ->
    Opts.

upload_with_commitment(Msg, Opts, CommitmentSpec) ->
    Device = commitment_device(CommitmentSpec),
    Committed = ensure_committed(Msg, Opts, CommitmentSpec),
    Results =
        lists:map(
            fun(UploadMsg) ->
                hb_client_remote:upload(UploadMsg, Opts, Device)
            end,
            upload_variants(Committed, Device, Opts)
        ),
    case lists:filter(fun upload_failed/1, Results) of
        [] -> ok;
        Errors -> {error, Errors}
    end.

upload_variants(Msg = #{ <<"commitments">> := Commitments }, Device, Opts) ->
    DeviceCommitments =
        maps:filter(
            fun(_ID, Commitment) ->
                hb_ao:get(<<"commitment-device">>, Commitment, Opts) =:= Device
            end,
            Commitments
        ),
    case maps:to_list(DeviceCommitments) of
        [] -> [Msg];
        [_] -> [Msg#{ <<"commitments">> => DeviceCommitments }];
        DeviceCommitmentsList ->
            [
                Msg#{ <<"commitments">> => #{ ID => Commitment } }
            ||
                {ID, Commitment} <- DeviceCommitmentsList
            ]
    end;
upload_variants(Msg, _Device, _Opts) ->
    [Msg].

upload_failed({ok, _}) -> false;
upload_failed(_) -> true.

upload_assignment(Assignment, #{ wallets := [] }, Opts, CommitmentSpec) ->
    upload_with_commitment(Assignment, Opts, CommitmentSpec);
upload_assignment(Assignment, #{ wallets := Wallets }, Opts, CommitmentSpec) ->
    Device = commitment_device(CommitmentSpec),
    BaseAssignment = hb_message:uncommitted(Assignment, Opts),
    Results =
        lists:map(
            fun(Wallet) ->
                hb_client_remote:upload(
                    hb_message:commit(
                        BaseAssignment,
                        Opts#{ <<"priv-wallet">> => Wallet },
                        CommitmentSpec
                    ),
                    Opts,
                    Device
                )
            end,
            Wallets
        ),
    case lists:filter(fun upload_failed/1, Results) of
        [] -> ok;
        Errors -> {error, Errors}
    end.

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
