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
                    {G,
                     #{
                       pid => Pid,
                       messages =>
                           case Pid of
                               undefined -> 0;
                               _ ->
                                   length(
                                     element(2,
                                             erlang:process_info(Pid, messages)))
                           end
                      }}
            end,
            case Group of
                global -> Groups;
                TargetGroup ->
                    case lists:member(TargetGroup, Groups) of
                        true -> [TargetGroup];
                        false -> []
                    end
            end)),
    Delta =
        hb_maps:filter(
          fun(G, NewState) ->
                  case hb_maps:get(G, Last, []) of
                      NewState -> false;
                      _ -> true
                  end
          end,
          New,
          Opts),
    case hb_maps:size(Delta, Opts) of
        0 -> ok;
        Deltas ->
            io:format(standard_error,
                      "== Sitrep ==> ~p named processes. ~p changes. ~n",
                      [hb_maps:size(New, Opts), Deltas]),
            hb_maps:map(
              fun(G, #{pid := P, messages := Msgs}) ->
                      io:format(standard_error, "[~p: ~p] #M: ~p~n", [G, P, Msgs])
              end,
              Delta,
              Opts),
            io:format(standard_error, "~n", [])
    end,
    timer:sleep(1000),
    receive
        stop -> stopped
    after
        0 ->
            do_monitor(Group, New, Opts)
    end.


%% @doc Register the process to lead an execution if none is found, otherwise
%% signal that we should await resolution.
find_or_register(Msg1, Msg2, Opts) ->
    GroupName = group(Msg1, Msg2, Opts),
    find_or_register(GroupName, Msg1, Msg2, Opts).


find_or_register(ungrouped_exec, _Msg1, _Msg2, _Opts) ->
    {leader, ungrouped_exec};
find_or_register(GroupName, _Msg1, _Msg2, Opts) ->
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
unregister_notify(ungrouped_exec, _Msg2, _Msg3, _Opts) -> ok;
unregister_notify(GroupName, Msg2, Msg3, Opts) ->
    unregister_groupname(GroupName, Opts),
    notify(GroupName, Msg2, Msg3, Opts).


%% @doc Find a group with the given name.
find_execution(Groupname, _Opts) ->
    start(),
    case hb_name:lookup(Groupname) of
        undefined -> not_found;
        Pid -> {ok, Pid}
    end.


%% @doc Calculate the group name for a Msg1 and Msg2 pair. Uses the Msg1's
%% `group' function if it is found in the `info', otherwise uses the default.
group(Msg1, Msg2, Opts) ->
    Grouper =
        hb_maps:get(grouper, hb_ao:info(Msg1, Opts), fun default_grouper/3, Opts),
    apply(
      Grouper,
      hb_ao:truncate_args(Grouper, [Msg1, Msg2, Opts])).


%% @doc Register for performing an AO-Core resolution.
register_groupname(Groupname, _Opts) ->
    ?event({registering_as, Groupname}),
    hb_name:register(Groupname).


%% @doc Unregister for being the leader on an AO-Core resolution.
unregister(Msg1, Msg2, Opts) ->
    start(),
    unregister_groupname(group(Msg1, Msg2, Opts), Opts).


unregister_groupname(Groupname, _Opts) ->
    ?event({unregister_resolver, {explicit, Groupname}}),
    hb_name:unregister(Groupname).


%% @doc If there was already an Erlang process handling this execution,
%% we should register with them and wait for them to notify us of
%% completion.
await(Worker, Msg1, Msg2, Opts) ->
    % Get the device's await function, if it exists.
    AwaitFun =
        hb_maps:get(
          await,
          hb_ao:info(Msg1, Opts),
          fun default_await/5,
          Opts),
    % Calculate the compute path that we will wait upon resolution of.
    % Register with the process.
    GroupName = group(Msg1, Msg2, Opts),
    % set monitor to a worker, so we know if it exits
    _Ref = erlang:monitor(process, Worker),
    Worker ! {resolve, self(), GroupName, Msg2, Opts},
    AwaitFun(Worker, GroupName, Msg1, Msg2, Opts).


%% @doc Default await function that waits for a resolution from a worker.
default_await(Worker, GroupName, Msg1, Msg2, Opts) ->
    % Wait for the result.
    receive
        {resolved, _, GroupName, Msg2, Res} ->
            worker_event(GroupName, {resolved_await, Res}, Msg1, Msg2, Opts),
            Res;
        {'DOWN', _R, process, Worker, Reason} ->
            ?event(
              {leader_died,
               {group, GroupName},
               {leader, Worker},
               {reason, Reason},
               {request, Msg2}}),
            {error, leader_died}
    end.


%% @doc Check our inbox for processes that are waiting for the resolution
%% of this execution. Comes in two forms:
%% 1. Notify on group name alone.
%% 2. Notify on group name and Msg2.
notify(GroupName, Msg2, Msg3, Opts) ->
    case is_binary(GroupName) of
        true ->
            ?event({notifying_all, {group, GroupName}});
        false ->
            ok
    end,
    receive
        {resolve, Listener, GroupName, Msg2, _ListenerOpts} ->
            ?event({notifying_listener, {listener, Listener}, {group, GroupName}}),
            send_response(Listener, GroupName, Msg2, Msg3),
            notify(GroupName, Msg2, Msg3, Opts)
    after
        0 ->
            ?event(finished_notify),
            ok
    end.


%% @doc Forward requests to a newly delegated execution process.
forward_work(NewPID, Opts) ->
    Gather =
        fun Gather() ->
                receive
                    Req = {resolve, _, _, _, _} -> [Req | Gather()]
                after
                    0 ->
                        []
                end
        end,
    ToForward = Gather(),
    lists:foreach(
      fun(Req) ->
              NewPID ! Req
      end,
      ToForward),
    case length(ToForward) > 0 of
        true ->
            ?event({fwded, {reqs, length(ToForward)}, {pid, NewPID}}, Opts);
        false -> ok
    end,
    ok.


%% @doc Helper function that wraps responding with a new Msg3.
send_response(Listener, GroupName, Msg2, Msg3) ->
    ?event(worker,
           {send_response,
            {listener, Listener},
            {group, GroupName}}),
    Listener ! {resolved, self(), GroupName, Msg2, Msg3}.


%% @doc Start a worker process that will hold a message in memory for
%% future executions.


start_worker(Msg, Opts) ->
    start_worker(group(Msg, undefined, Opts), Msg, Opts).


start_worker(_, NotMsg, _) when not is_map(NotMsg) -> not_started;
start_worker(GroupName, Msg, Opts) ->
    start(),
    ?event(worker_spawns,
           {starting_worker, {group, GroupName}, {msg, Msg}, {opts, Opts}}),
    WorkerPID = spawn(
                  fun() ->
                          % If the device's info contains a `worker' function we
                          % use that instead of the default implementation.
                          WorkerFun =
                              hb_maps:get(
                                worker,
                                hb_ao:info(Msg, Opts),
                                Def = fun default_worker/3,
                                Opts),
                          ?event(worker,
                                 {new_worker,
                                  {group, GroupName},
                                  {default_server, WorkerFun == Def},
                                  {default_group,
                                   default_grouper(Msg, undefined, Opts) == GroupName}}),
                          % Call the worker function, unsetting the option
                          % to avoid recursive spawns.
                          register_groupname(GroupName, Opts),
                          apply(
                            WorkerFun,
                            hb_ao:truncate_args(
                              WorkerFun,
                              [GroupName,
                               Msg,
                               hb_maps:merge(Opts,
                                             #{
                                               is_worker => true,
                                               spawn_worker => false,
                                               allow_infinite => true
                                              },
                                             Opts)]))
                  end),
    WorkerPID.


%% @doc A server function for handling persistent executions.
default_worker(GroupName, Msg1, Opts) ->
    Timeout = hb_opts:get(worker_timeout, 10000, Opts),
    worker_event(GroupName, default_worker_waiting_for_req, Msg1, undefined, Opts),
    receive
        {resolve, Listener, GroupName, Msg2, ListenerOpts} ->
            ?event(worker,
                   {work_received,
                    {listener, Listener},
                    {group, GroupName}}),
            Res =
                hb_ao:resolve(
                  Msg1,
                  Msg2,
                  hb_maps:merge(ListenerOpts, Opts, Opts)),
            send_response(Listener, GroupName, Msg2, Res),
            notify(GroupName, Msg2, Res, Opts),
            case hb_opts:get(static_worker, false, Opts) of
                true ->
                    % Reregister for the existing group name.
                    register_groupname(GroupName, Opts),
                    default_worker(GroupName, Msg1, Opts);
                false ->
                    % Register for the new (Msg1) group.
                    case Res of
                        {ok, Msg3} ->
                            NewGroupName = group(Msg3, undefined, Opts),
                            register_groupname(NewGroupName, Opts),
                            default_worker(NewGroupName, Msg3, Opts);
                        _ ->
                            % If the result is not ok, we should either ignore
                            % the error and stay on the existing group,
                            % or throw it.
                            case hb_opts:get(error_strategy, ignore, Opts) of
                                ignore ->
                                    register_groupname(GroupName, Opts),
                                    default_worker(GroupName, Msg1, Opts);
                                throw -> throw(Res)
                            end
                    end
            end
    after
        Timeout ->
            % We have hit the in-memory persistence timeout. Check whether the
            % device has shutdown procedures (for example, writing in-memory
            % state to the cache).
            unregister(Msg1, undefined, Opts)
    end.


%% @doc Create a group name from a Msg1 and Msg2 pair as a tuple.
default_grouper(Msg1, Msg2, Opts) ->
    %?event({calculating_default_group_name, {msg1, Msg1}, {msg2, Msg2}}),
    % Use Erlang's `phash2' to hash the result of the Grouper function.
    % `phash2' is relatively fast and ensures that the group name is short for
    % storage in `pg'. In production we should only use a hash with a larger
    % output range to avoid collisions.
    ?no_prod("Using a hash for group names is not secure."),
    case hb_opts:get(await_inprogress, true, Opts) of
        true ->
            erlang:phash2(
              {hb_maps:without([<<"priv">>], Msg1, Opts),
               hb_maps:without([<<"priv">>], Msg2, Opts)});
        _ -> ungrouped_exec
    end.


%% @doc Log an event with the worker process. If we used the default grouper
%% function, we should also include the Msg1 and Msg2 in the event. If we did not,
%% we assume that the group name expresses enough information to identify the
%% request.
worker_event(Group, Data, Msg1, Msg2, Opts) when is_integer(Group) ->
    ?event(worker, {worker_event, Group, Data, {msg1, Msg1}, {msg2, Msg2}}, Opts);
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
                    Base)
          end,
      slow_key =>
          fun(_, #{<<"wait">> := Wait}) ->
                  ?event({slow_key_wait_started, Wait}),
                  receive
                  after
                      Wait ->
                          {ok,
                           #{
                             waited => Wait,
                             pid => self(),
                             random_bytes =>
                                 hb_util:encode(crypto:strong_rand_bytes(4))
                            }}
                  end
          end,
      self =>
          fun(M1, #{<<"wait">> := Wait}) ->
                  ?event({self_waiting, {wait, Wait}}),
                  receive
                  after
                      Wait ->
                          ?event({self_returning, M1, {wait, Wait}}),
                          {ok, M1}
                  end
          end
     }.


spawn_test_client(Msg1, Msg2) ->
    spawn_test_client(Msg1, Msg2, #{}).


spawn_test_client(Msg1, Msg2, Opts) ->
    Ref = make_ref(),
    TestParent = self(),
    spawn_link(fun() ->
                       ?event({new_concurrent_test_resolver, Ref, {executing, Msg2}}),
                       Res = hb_ao:resolve(Msg1, Msg2, Opts),
                       ?event({test_worker_got_result, Ref, {result, Res}}),
                       TestParent ! {result, Ref, Res}
               end),
    Ref.


wait_for_test_result(Ref) ->
    receive {result, Ref, Res} -> Res end.


%% @doc Test merging and returning a value with a persistent worker.
deduplicated_execution_test() ->
    TestTime = 200,
    Msg1 = #{<<"device">> => test_device()},
    Msg2 = #{<<"path">> => <<"slow_key">>, <<"wait">> => TestTime},
    T0 = hb:now(),
    Ref1 = spawn_test_client(Msg1, Msg2),
    receive after 100 -> ok end,
    Ref2 = spawn_test_client(Msg1, Msg2),
    Res1 = wait_for_test_result(Ref1),
    Res2 = wait_for_test_result(Ref2),
    T1 = hb:now(),
    % Check the result is the same.
    ?assertEqual(Res1, Res2),
    % Check the time it took is less than the sum of the two test times.
    ?assert(T1 - T0 < (2 * TestTime)).


%% @doc Test spawning a default persistent worker.
persistent_worker_test() ->
    TestTime = 200,
    Msg1 = #{<<"device">> => test_device()},
    link(start_worker(Msg1, #{static_worker => true})),
    receive after 10 -> ok end,
    Msg2 = #{<<"path">> => <<"slow_key">>, <<"wait">> => TestTime},
    Msg3 = #{<<"path">> => <<"slow_key">>, <<"wait">> => trunc(TestTime * 1.1)},
    Msg4 = #{<<"path">> => <<"slow_key">>, <<"wait">> => trunc(TestTime * 1.2)},
    T0 = hb:now(),
    Ref1 = spawn_test_client(Msg1, Msg2),
    Ref2 = spawn_test_client(Msg1, Msg3),
    Ref3 = spawn_test_client(Msg1, Msg4),
    Res1 = wait_for_test_result(Ref1),
    Res2 = wait_for_test_result(Ref2),
    Res3 = wait_for_test_result(Ref3),
    T1 = hb:now(),
    ?assertNotEqual(Res1, Res2),
    ?assertNotEqual(Res2, Res3),
    ?assert(T1 - T0 >= (3 * TestTime)).


spawn_after_execution_test() ->
    ?event(<<"">>),
    TestTime = 500,
    Msg1 = #{<<"device">> => test_device()},
    Msg2 = #{<<"path">> => <<"self">>, <<"wait">> => TestTime},
    Msg3 = #{<<"path">> => <<"slow_key">>, <<"wait">> => trunc(TestTime * 1.1)},
    Msg4 = #{<<"path">> => <<"slow_key">>, <<"wait">> => trunc(TestTime * 1.2)},
    T0 = hb:now(),
    Ref1 =
        spawn_test_client(
          Msg1,
          Msg2,
          #{
            spawn_worker => true,
            static_worker => true,
            hashpath => ignore
           }),
    receive after 10 -> ok end,
    Ref2 = spawn_test_client(Msg1, Msg3),
    Ref3 = spawn_test_client(Msg1, Msg4),
    Res1 = wait_for_test_result(Ref1),
    Res2 = wait_for_test_result(Ref2),
    Res3 = wait_for_test_result(Ref3),
    T1 = hb:now(),
    ?assertNotEqual(Res1, Res2),
    ?assertNotEqual(Res2, Res3),
    ?assert(T1 - T0 >= (3 * TestTime)).
