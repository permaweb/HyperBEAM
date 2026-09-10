%%% @doc Creates and manages long-lived AO-Core resolution processes.
%%% These can be useful for situations where a message is large and expensive
%%% to serialize and deserialize, or when executions should be deliberately
%%% serialized to avoid parallel executions of the same computation. This 
%%% module is called during the core `hb_ao' execution process, so care
%%% must be taken to avoid recursive spawns/loops.
%%% 
%%% Built using the `pg' module, which is a distributed Erlang process group
%%% manager.

-module(hb_persistent).
-export([start_monitor/0, start_monitor/1, stop_monitor/1]).
-export([find_or_register/3, unregister_notify/4, await/4, notify/4]).
-export([group/3, start_worker/3, start_worker/2, forward_work/2]).
-export([default_grouper/3, default_worker/3, default_await/5]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Ensure that the `pg' module is started.
start() -> hb_name:start().

%% @doc Start a monitor that prints the current members of the group every
%% n seconds.
start_monitor() ->
    start_monitor(global).
start_monitor(Group) ->
	start_monitor(Group, #{}).
start_monitor(Group, Opts) ->
    start(),
    ?event({worker_monitor, {start_monitor, Group, hb_name:all()}}),
    spawn(fun() -> do_monitor(Group, #{}, Opts) end).

stop_monitor(PID) ->
    PID ! stop.

do_monitor(Group, Last, Opts) ->
    Groups = lists:map(fun({Name, _}) -> Name end, hb_name:all()),
    New =
        hb_maps:from_list(
            lists:map(
                fun(G) ->
                    Pid = hb_name:lookup(G),
                    {
                        G,
                        #{
                            pid => Pid,
                            messages =>
                                case Pid of
                                    undefined -> 0;
                                    _ ->
                                        length(
                                            element(2,
                                                erlang:process_info(Pid, messages)
                                            )
                                        )
                                end
                        }
                            
                    }
                end,
                case Group of
                    global -> Groups;
                    TargetGroup ->
                        case lists:member(TargetGroup, Groups) of
                            true -> [TargetGroup];
                            false -> []
                        end
                end
            )
        ),
    Delta =
        hb_maps:filter(
            fun(G, NewState) ->
                case hb_maps:get(G, Last, []) of
                    NewState -> false;
                    _ -> true
                end
            end,
            New,
			Opts
        ),
    case hb_maps:size(Delta, Opts) of
        0 -> ok;
        Deltas ->
            io:format(standard_error, "== Sitrep ==> ~p named processes. ~p changes. ~n",
                [hb_maps:size(New, Opts), Deltas]),
            hb_maps:map(
                fun(G, #{pid := P, messages := Msgs}) ->
                    io:format(standard_error, "[~p: ~p] #M: ~p~n", [G, P, Msgs])
                end,
                Delta,
				Opts
            ),
            io:format(standard_error, "~n", [])
    end,
    timer:sleep(1000),
    receive stop -> stopped
    after 0 -> do_monitor(Group, New, Opts)
    end.

%% @doc Register the process to lead an execution if none is found, otherwise
%% signal that we should await resolution. When `await-inprogress' is
%% explicitly disabled (the common case for internal/recursive resolves) we
%% skip the grouper dispatch entirely: the leader short-circuit below would
%% discard the computed group name anyway.
find_or_register(_Base, _Req, #{ <<"await-inprogress">> := false }) ->
    {leader, ungrouped_exec};
find_or_register(Base, Req, Opts) ->
    find_or_register(group(Base, Req, Opts), Base, Req, Opts).
find_or_register(ungrouped_exec, _Base, _Req, _Opts) ->
    {leader, ungrouped_exec};
find_or_register(GroupName, _Base, _Req, Opts) ->
    case hb_opts:get(await_inprogress, false, Opts) of
        false -> {leader, GroupName};
        _ ->
            Self = self(),
            case find_execution(GroupName, Opts) of
                {ok, Leader} when Leader =/= Self ->
                    ?event({found_leader, GroupName, {leader, Leader}}),
                    {wait, Leader};
                {ok, Leader} when Leader =:= Self ->
                    {infinite_recursion, GroupName};
                _ ->
                    ?event({register_resolver, {group, GroupName}}),
                    register_groupname(GroupName, Opts),
                    {leader, GroupName}
            end
    end.

%% @doc Unregister as the leader for an execution and notify waiting processes.
unregister_notify(ungrouped_exec, _Req, _Res, _Opts) -> ok;
unregister_notify(GroupName, Req, Res, Opts) ->
    unregister_groupname(GroupName, Opts),
    notify(GroupName, Req, Res, Opts).

%% @doc Find a group with the given name.
find_execution(Groupname, _Opts) ->
    start(),
    case hb_name:lookup(Groupname) of
        undefined -> not_found;
        Pid -> {ok, Pid}
    end.

%% @doc Calculate the group name for a Base and Req pair. Uses the Base's
%% `group' function if it is found in the `info', otherwise uses the default.
group(Base, Req, RawOpts) ->
    Opts = maps:remove(<<"cache-control-sources">>, RawOpts),
    Grouper =
        hb_maps:get(
            grouper,
            hb_device:info(Base, Opts),
            fun default_grouper/3,
            Opts
        ),
    apply(
        Grouper,
        hb_device:truncate_args(Grouper, [Base, Req, Opts])
    ).

%% @doc Register for performing an AO-Core resolution.
register_groupname(Groupname, _Opts) ->
    ?event({registering_as, Groupname}),
    hb_name:register(Groupname).

%% @doc Unregister for being the leader on an AO-Core resolution.
unregister(Base, Req, Opts) ->
    start(),
    unregister_groupname(group(Base, Req, Opts), Opts).
unregister_groupname(Groupname, _Opts) ->
    ?event({unregister_resolver, {explicit, Groupname}}),
    hb_name:unregister(Groupname).

%% @doc If there was already an Erlang process handling this execution,
%% we should register with them and wait for them to notify us of
%% completion.
await(Worker, Base, Req, Opts) ->
    CallbackOpts = maps:remove(<<"cache-control-sources">>, Opts),
    % Get the device's await function, if it exists.
    AwaitFun =
        hb_maps:get(
            await,
            hb_device:info(Base, CallbackOpts),
            fun default_await/5,
			CallbackOpts
        ),
    % Calculate the compute path that we will wait upon resolution of.
    % Register with the process.
    GroupName = group(Base, Req, CallbackOpts),
    % set monitor to a worker, so we know if it exits
    _Ref = erlang:monitor(process, Worker),
    Worker ! {resolve, self(), GroupName, Req, Opts},
    AwaitFun(Worker, GroupName, Base, Req, CallbackOpts).

%% @doc Default await function that waits for a resolution from a worker.
default_await(Worker, GroupName, Base, Req, Opts) ->
    % Wait for the result.
    receive
        {resolved, _, GroupName, Req, Res} ->
            worker_event(GroupName, {resolved_await, Res}, Base, Req, Opts),
            Res;
        {'DOWN', _R, process, Worker, Reason} ->
            ?event(
                {leader_died,
                    {group, GroupName},
                    {leader, Worker},
                    {reason, Reason},
                    {request, Req}
                }
            ),
            {error, leader_died}
    end.

%% @doc Check our inbox for processes that are waiting for the resolution
%% of this execution. Comes in two forms:
%% 1. Notify on group name alone.
%% 2. Notify on group name and Req.
notify(GroupName, Req, Res, Opts) ->
    case is_binary(GroupName) of
        true ->
            ?event({notifying_all, {group, GroupName}});
        false ->
            ok
    end,
    receive
        {resolve, Listener, GroupName, Req, _ListenerOpts} ->
            ?event({notifying_listener, {listener, Listener}, {group, GroupName}}),
            send_response(Listener, GroupName, Req, Res),
            notify(GroupName, Req, Res, Opts)
    after 0 ->
        ?event(finished_notify),
        ok
    end.

%% @doc Forward requests to a newly delegated execution process.
forward_work(NewPID, Opts) ->
    Gather =
        fun Gather() ->
            receive
                Req = {resolve, _, _, _, _} -> [Req | Gather()]
            after 0 -> []
            end
        end,
    ToForward = Gather(),
    lists:foreach(
        fun(Req) ->
            NewPID ! Req
        end,
        ToForward
    ),
    case length(ToForward) > 0 of
        true ->
            ?event({fwded, {reqs, length(ToForward)}, {pid, NewPID}}, Opts);
        false -> ok
    end,
    ok.

%% @doc Helper function that wraps responding with a new Res.
send_response(Listener, GroupName, Req, Res) ->
    ?event(worker,
        {send_response,
            {listener, Listener},
            {group, GroupName}
        }
    ),
    Listener ! {resolved, self(), GroupName, Req, Res}.

%% @doc Start a worker process that will hold a message in memory for
%% future executions.

start_worker(Msg, Opts) ->
    start_worker(group(Msg, undefined, Opts), Msg, Opts).
start_worker(_, NotMsg, _) when not is_map(NotMsg) -> not_started;
start_worker(GroupName, Msg, RawOpts) ->
    Opts = maps:remove(<<"cache-control-sources">>, RawOpts),
    start(),
    ?event(worker_spawns,
        {starting_worker, {group, GroupName}, {msg, Msg}, {opts, Opts}}
    ),
    WorkerPID = spawn(
        fun() ->
            % If the device's info contains a `worker' function we
            % use that instead of the default implementation.
            WorkerFun =
                hb_maps:get(
                    worker,
                    hb_device:info(Msg, Opts),
                    Def = fun default_worker/3,
					Opts
                ),
            ?event(worker,
                {new_worker,
                    {group, GroupName},
                    {default_server, WorkerFun == Def},
                    {default_group,
                        default_grouper(Msg, undefined, Opts) == GroupName
                    }
                }
            ),
            % Call the worker function, unsetting the option
            % to avoid recursive spawns.
            register_groupname(GroupName, Opts),
            apply(
                WorkerFun,
                hb_device:truncate_args(
                    WorkerFun,
                    [
                        GroupName,
                        Msg,
                        hb_maps:merge(Opts, #{
                            <<"is-worker">> => true,
                            <<"spawn-worker">> => false,
                            <<"allow-infinite">> => true
                        },
						Opts)
                    ]
                )
            )
        end
    ),
    WorkerPID.

%% @doc A server function for handling persistent executions. 
default_worker(GroupName, Base, Opts) ->
    Timeout = hb_opts:get(worker_timeout, 10000, Opts),
    worker_event(GroupName, default_worker_waiting_for_req, Base, undefined, Opts),
    receive
        {resolve, Listener, GroupName, Req, ListenerOpts} ->
            ?event(worker,
                {work_received,
                    {listener, Listener},
                    {group, GroupName}
                }
            ),
            % Restore only this listener's policy before the request is varied
            % again. Replies still identify the original envelope request.
            ReplayReq = replay_request(Req, ListenerOpts),
            Res =
                hb_ao:resolve(
                    Base,
                    ReplayReq,
                    maps:remove(
                        <<"cache-control-sources">>,
                        hb_maps:merge(ListenerOpts, Opts, Opts)
                    )
                ),
            send_response(Listener, GroupName, Req, Res),
            notify(GroupName, Req, Res, Opts),
            case hb_opts:get(static_worker, false, Opts) of
                true ->
                    % Reregister for the existing group name.
                    register_groupname(GroupName, Opts),
                    default_worker(GroupName, Base, Opts);
                false ->
                    % Register for the new (Base) group.
                    case Res of
                        {ok, Res} ->
                            NewGroupName = group(Res, undefined, Opts),
                            register_groupname(NewGroupName, Opts),
                            default_worker(NewGroupName, Res, Opts);
                        _ ->
                            % If the result is not ok, we should either ignore
                            % the error and stay on the existing group,
                            % or throw it.
                            case hb_opts:get(error_strategy, ignore, Opts) of
                                ignore ->
                                    register_groupname(GroupName, Opts),
                                    default_worker(GroupName, Base, Opts);
                                throw -> throw(Res)
                            end
                    end
            end
    after Timeout ->
        % We have hit the in-memory persistence timeout. Check whether the
        % device has shutdown procedures (for example, writing in-memory
        % state to the cache).
        unregister(Base, undefined, Opts)
    end.

%% @doc Restore projected request policy without retaining stale commitments.
replay_request(Req, #{ <<"cache-control-sources">> := {_, Policy} } = Opts) ->
    case maps:merge(Req, Policy) of
        Req -> Req;
        Restored -> hb_message:uncommitted(Restored, Opts)
    end;
replay_request(Req, _Opts) -> Req.

%% @doc Create a group name from a Base and Req pair as a tuple.
default_grouper(Base, Req, Opts) ->
    %?event({calculating_default_group_name, {base, Base}, {req, Req}}),
    % Use Erlang's `phash2' to hash the result of the Grouper function.
    % `phash2' is relatively fast and ensures that the group name is short for
    % storage in `pg'. In production we should only use a hash with a larger
    % output range to avoid collisions.
    ?no_prod("Using a hash for group names is not secure."),
    case hb_opts:get(await_inprogress, true, Opts) of
        true ->
            erlang:phash2(
                {
                    hb_maps:without([<<"priv">>], Base, Opts),
                    hb_maps:without([<<"priv">>], Req, Opts)
                }
            );
        _ -> ungrouped_exec
    end.

%% @doc Log an event with the worker process. If we used the default grouper
%% function, we should also include the Base and Req in the event. If we did not,
%% we assume that the group name expresses enough information to identify the
%% request.
worker_event(Group, Data, Base, Req, Opts) when is_integer(Group) ->
    ?event(worker, {worker_event, Group, Data, {base, Base}, {req, Req}}, Opts);
worker_event(Group, Data, _, _, Opts) ->
    ?event(worker, {worker_event, Group, Data}, Opts).

%%% Tests

test_device() -> test_device(#{}).
test_device(Base) ->
    #{
        info =>
            fun() ->
                hb_maps:merge(
                    #{
                        grouper =>
                            fun(M1, _M2, _Opts) ->
                                erlang:phash2(M1)
                            end
                    },
                    Base
                )
            end,
        slow_key =>
            fun(_, #{ <<"wait">> := Wait }) ->
                ?event({slow_key_wait_started, Wait}),
                receive after Wait ->
                    {ok,
                        #{
                            waited => Wait,
                            pid => self(),
                            random_bytes =>
                                hb_util:encode(crypto:strong_rand_bytes(4))
                        }
                    }
                end
            end,
        self =>
            fun(M1, #{ <<"wait">> := Wait }) ->
                ?event({self_waiting, {wait, Wait}}),
                receive after Wait ->
                    ?event({self_returning, M1, {wait, Wait}}),
                    {ok, M1}
                end
            end
    }.

spawn_test_client(Base, Req) ->
    spawn_test_client(Base, Req, #{}).
spawn_test_client(Base, Req, Opts) ->
    Ref = make_ref(),
    TestParent = self(),
    spawn_link(fun() ->
        ?event({new_concurrent_test_resolver, Ref, {executing, Req}}),
        Res = hb_ao:resolve(Base, Req, Opts),
        ?event({test_worker_got_result, Ref, {result, Res}}),
        TestParent ! {result, Ref, Res}
    end),
    Ref.

wait_for_test_result(Ref) ->
    receive {result, Ref, Res} -> Res end.

%% @doc Temporary cache policy is not exposed as device callback options.
cache_policy_callback_isolation_test() ->
    Parent = self(),
    Group = make_ref(),
    CheckOpts = fun(Opts) ->
        ?assertNot(maps:is_key(<<"cache-control-sources">>, Opts))
    end,
    Device = test_device(#{
        grouper => fun(_, _, Opts) -> CheckOpts(Opts), Group end,
        await => fun(_, _, _, _, Opts) -> CheckOpts(Opts), ok end,
        worker => fun(_, _, Opts) ->
            Parent ! {worker_policy, self(),
                maps:is_key(<<"cache-control-sources">>, Opts)}
        end
    }),
    Base = #{ <<"device">> => Device },
    Req = #{ <<"path">> => <<"self">> },
    Opts = #{ <<"cache-control-sources">> =>
        {#{}, #{ <<"cache-control">> => [<<"store">>] }} },
    ?assertEqual(Group, group(Base, Req, Opts)),
    {Awaiter, AwaitMonitor} = spawn_monitor(fun() ->
        Self = self(),
        ?assertEqual(ok, await(Self, Base, Req, Opts)),
        receive {resolve, Self, Group, Req, Opts} -> ok
        after 1000 -> error(missing_worker_envelope)
        end
    end),
    try
        receive {'DOWN', AwaitMonitor, process, Awaiter, Reason} ->
            ?assertEqual(normal, Reason)
        after 5000 -> error(await_callback_timeout)
        end
    after
        exit(Awaiter, kill),
        erlang:demonitor(AwaitMonitor, [flush])
    end,
    Worker = start_worker(Group, Base, Opts),
    WorkerMonitor = erlang:monitor(process, Worker),
    try
        receive {worker_policy, Worker, HasPolicy} -> ?assertNot(HasPolicy)
        after 5000 -> error(missing_worker_callback)
        end
    after
        exit(Worker, kill),
        receive {'DOWN', WorkerMonitor, process, Worker, _} -> ok
        after 1000 -> error(worker_stop_timeout)
        end,
        hb_name:unregister(Group)
    end.

%% @doc A default worker replays request storage policy, not just its projection.
projected_request_worker_cache_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store(),
        <<"attested-store">> => hb_test_utils:test_store(),
        <<"await-inprogress">> => true, <<"spawn-worker">> => false },
    Base = #{ <<"device">> => <<"test-device@1.0">>,
        <<"required">> => erlang:unique_integer([positive]),
        <<"deep">> => #{ <<"slot">> => 8 } },
    Req = #{ <<"path">> => <<"vary-projection">>,
        <<"deep-request">> => #{ <<"slot">> => 9 } },
    Group = group(Base, Req, Opts),
    Parent = self(),
    Hook = #{ <<"device">> => #{ <<"step">> =>
        fun(_, Event, _) ->
            case maps:get(<<"request">>, Event) of
                #{ <<"path">> := <<"vary-projection">> } ->
                    Parent ! {worker_executed, self()};
                _ -> ok
            end,
            {ok, Event}
        end } },
    {Worker, Monitor} = spawn_monitor(fun() ->
        hb_name:register(Group),
        Parent ! {worker_ready, self()},
        default_worker(Group, Base, Opts#{ <<"static-worker">> => true,
            <<"is-worker">> => true, <<"allow-infinite">> => true,
            <<"on">> => #{ <<"step">> => Hook } })
    end),
    try
        receive {worker_ready, Worker} -> ok
        after 5000 -> error(worker_start_timeout)
        end,
        {ok, _} = hb_ao:resolve(
            Base, Req#{ <<"cache-control">> => [<<"store">>] }, Opts
        ),
        receive {worker_executed, Worker} -> ok
        after 1000 -> error(request_did_not_execute_in_worker)
        end,
        {ok, Cached} = hb_ao:resolve(
            Base, Req#{ <<"cache-control">> => [<<"only-if-cached">>] },
            Opts#{ <<"await-inprogress">> => false }
        ),
        ?assertEqual(9, hb_ao:get(<<"request/deep-request/slot">>, Cached, Opts))
    after
        exit(Worker, kill),
        receive {'DOWN', Monitor, process, Worker, _} -> ok
        after 1000 -> error(worker_stop_timeout)
        end,
        hb_name:unregister(Group)
    end.

%% @doc Test merging and returning a value with a persistent worker.
deduplicated_execution_test() ->
    TestTime = 200,
    Base = #{ <<"device">> => test_device() },
    Req = #{ <<"path">> => <<"slow_key">>, <<"wait">> => TestTime },
    T0 = hb:now(),
    Ref1 = spawn_test_client(Base, Req),
    receive after 100 -> ok end,
    Ref2 = spawn_test_client(Base, Req),
    Res1 = wait_for_test_result(Ref1),
    Res2 = wait_for_test_result(Ref2),
    T1 = hb:now(),
    % Check the result is the same.
    ?assertEqual(Res1, Res2),
    % Check the time it took is less than the sum of the two test times.
    ?assert(T1 - T0 < (2*TestTime)).

%% @doc Test spawning a default persistent worker.
persistent_worker_test() ->
    TestTime = 200,
    Base = #{ <<"device">> => test_device() },
    link(start_worker(Base, #{ <<"static-worker">> => true })),
    receive after 10 -> ok end,
    Req = #{ <<"path">> => <<"slow_key">>, <<"wait">> => TestTime },
    Res = #{ <<"path">> => <<"slow_key">>, <<"wait">> => trunc(TestTime*1.1) },
    Msg4 = #{ <<"path">> => <<"slow_key">>, <<"wait">> => trunc(TestTime*1.2) },
    T0 = hb:now(),
    Ref1 = spawn_test_client(Base, Req),
    Ref2 = spawn_test_client(Base, Res),
    Ref3 = spawn_test_client(Base, Msg4),
    Res1 = wait_for_test_result(Ref1),
    Res2 = wait_for_test_result(Ref2),
    Res3 = wait_for_test_result(Ref3),
    T1 = hb:now(),
    ?assertNotEqual(Res1, Res2),
    ?assertNotEqual(Res2, Res3),
    ?assert(T1 - T0 >= (3*TestTime)).

spawn_after_execution_test() ->
    ?event(<<"">>),
    TestTime = 500,
    Base = #{ <<"device">> => test_device() },
    Req = #{ <<"path">> => <<"self">>, <<"wait">> => TestTime },
    Res = #{ <<"path">> => <<"slow_key">>, <<"wait">> => trunc(TestTime*1.1) },
    Msg4 = #{ <<"path">> => <<"slow_key">>, <<"wait">> => trunc(TestTime*1.2) },
    T0 = hb:now(),
    Ref1 =
        spawn_test_client(
            Base,
            Req,
            #{
                <<"spawn-worker">> => true,
                <<"static-worker">> => true,
                <<"hashpath">> => ignore
            }
        ),
    receive after 10 -> ok end,
    Ref2 = spawn_test_client(Base, Res),
    Ref3 = spawn_test_client(Base, Msg4),
    Res1 = wait_for_test_result(Ref1),
    Res2 = wait_for_test_result(Ref2),
    Res3 = wait_for_test_result(Ref3),
    T1 = hb:now(),
    ?assertNotEqual(Res1, Res2),
    ?assertNotEqual(Res2, Res3),
    ?assert(T1 - T0 >= (3*TestTime)).
