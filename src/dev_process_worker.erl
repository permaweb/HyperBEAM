%%% @doc A long-lived process worker that keeps state in memory between
%%% calls. Implements the interface of `hb_ao' to receive and respond
%%% to computation requests regarding a process as a singleton.
-module(dev_process_worker).
-export([server/3, stop/1, group/3, await/5, notify_compute/4]).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").


%% @doc Returns a group name for a request. The worker is responsible for all
%% computation work on the same process on a single node, so we use the
%% process ID as the group name.
group(Msg1, undefined, Opts) ->
    hb_persistent:default_grouper(Msg1, undefined, Opts);
group(Msg1, Msg2, Opts) ->
    case hb_opts:get(process_workers, false, Opts) of
        false ->
            hb_persistent:default_grouper(Msg1, Msg2, Opts);
        true ->
            case Msg2 of
                undefined ->
                    hb_persistent:default_grouper(Msg1, undefined, Opts);
                _ ->
                    case hb_path:matches(<<"compute">>, hb_path:hd(Msg2, Opts)) of
                        true ->
                            process_to_group_name(Msg1, Opts);
                        _ ->
                            hb_persistent:default_grouper(Msg1, Msg2, Opts)
                    end
            end
    end.


process_to_group_name(Msg1, Opts) ->
    Initialized = dev_process:ensure_process_key(Msg1, Opts),
    ProcMsg = hb_ao:get(<<"process">>, Initialized, Opts#{hashpath => ignore}),
    ID = hb_message:id(ProcMsg, all),
    ?event({process_to_group_name, {id, ID}, {msg1, Msg1}}),
    hb_util:human_id(ID).


%% @doc Spawn a new worker process. This is called after the end of the first
%% execution of `hb_ao:resolve/3', so the state we are given is the
%% already current.
server(GroupName, Msg1, Opts) ->
    ServerOpts = Opts#{
                   await_inprogress => false,
                   spawn_worker => false,
                   process_workers => false
                  },
    % The maximum amount of time the worker will wait for a request before
    % checking the cache for a snapshot. Default: 5 minutes.
    Timeout = hb_opts:get(process_worker_max_idle, 300_000, Opts),
    ?event(worker, {waiting_for_req, {group, GroupName}}),
    receive
        {resolve, Listener, GroupName, Msg2, ListenerOpts} ->
            TargetSlot = hb_ao:get(<<"slot">>, Msg2, Opts),
            ?event(worker,
                   {work_received,
                    {group, GroupName},
                    {slot, TargetSlot},
                    {listener, Listener}}),
            Res =
                hb_ao:resolve(
                  Msg1,
                  #{<<"path">> => <<"compute">>, <<"slot">> => TargetSlot},
                  hb_maps:merge(ListenerOpts, ServerOpts, Opts)),
            ?event(worker, {work_done, {group, GroupName}, {req, Msg2}, {res, Res}}),
            send_notification(Listener, GroupName, TargetSlot, Res),
            server(
              GroupName,
              case Res of
                  {ok, Msg3} -> Msg3;
                  _ -> Msg1
              end,
              Opts);
        stop ->
            ?event(worker, {stopping, {group, GroupName}, {msg1, Msg1}}),
            exit(normal)
    after
        Timeout ->
            % We have hit the in-memory persistence timeout. Generate a snapshot
            % of the current process state and ensure it is cached.
            hb_ao:resolve(
              Msg1,
              <<"snapshot">>,
              ServerOpts#{<<"cache-control">> => [<<"store">>]}),
            % Return the current process state.
            {ok, Msg1}
    end.


%% @doc Await a resolution from a worker executing the `process@1.0' device.
await(Worker, GroupName, Msg1, Msg2, Opts) ->
    case hb_path:matches(<<"compute">>, hb_path:hd(Msg2, Opts)) of
        false ->
            hb_persistent:default_await(Worker, GroupName, Msg1, Msg2, Opts);
        true ->
            TargetSlot = hb_ao:get(<<"slot">>, Msg2, any, Opts),
            ?event({awaiting_compute,
                    {worker, Worker},
                    {group, GroupName},
                    {target_slot, TargetSlot}}),
            receive
                {resolved, _, GroupName, {slot, RecvdSlot}, Res}
                  when RecvdSlot == TargetSlot orelse TargetSlot == any ->
                    ?event(compute_debug,
                           {notified_of_resolution,
                            {target, TargetSlot},
                            {group, GroupName}}),
                    Res;
                {resolved, _, GroupName, {slot, RecvdSlot}, _Res} ->
                    ?event(compute_debug,
                           {waiting_again,
                            {target, TargetSlot},
                            {recvd, RecvdSlot},
                            {worker, Worker},
                            {group, GroupName}}),
                    await(Worker, GroupName, Msg1, Msg2, Opts);
                {'DOWN', _R, process, Worker, _Reason} ->
                    ?event(compute_debug,
                           {leader_died,
                            {group, GroupName},
                            {leader, Worker},
                            {target, TargetSlot}}),
                    {error, leader_died}
            end
    end.


%% @doc Notify any waiters for a specific slot of the computed results.
notify_compute(GroupName, SlotToNotify, Msg3, Opts) ->
    notify_compute(GroupName, SlotToNotify, Msg3, Opts, 0).


notify_compute(GroupName, SlotToNotify, Msg3, Opts, Count) ->
    ?event({notifying_of_computed_slot, {group, GroupName}, {slot, SlotToNotify}}),
    receive
        {resolve, Listener, GroupName, #{<<"slot">> := SlotToNotify}, _ListenerOpts} ->
            send_notification(Listener, GroupName, SlotToNotify, Msg3),
            notify_compute(GroupName, SlotToNotify, Msg3, Opts, Count + 1);
        {resolve, Listener, GroupName, Msg, _ListenerOpts}
          when is_map(Msg) andalso not is_map_key(<<"slot">>, Msg) ->
            send_notification(Listener, GroupName, SlotToNotify, Msg3),
            notify_compute(GroupName, SlotToNotify, Msg3, Opts, Count + 1)
    after
        0 ->
            ?event(worker_short,
                   {finished_notifying,
                    {group, GroupName},
                    {slot, SlotToNotify},
                    {listeners, Count}})
    end.


send_notification(Listener, GroupName, SlotToNotify, Msg3) ->
    ?event({sending_notification, {group, GroupName}, {slot, SlotToNotify}}),
    Listener ! {resolved, self(), GroupName, {slot, SlotToNotify}, Msg3}.


%% @doc Stop a worker process.
stop(Worker) ->
    exit(Worker, normal).


%%% Tests


test_init() ->
    application:ensure_all_started(hb),
    ok.


info_test() ->
    test_init(),
    M1 = dev_process:test_wasm_process(<<"test/aos-2-pure-xs.wasm">>),
    Res = hb_ao:info(M1, #{}),
    ?assertEqual(fun dev_process_worker:group/3, hb_maps:get(grouper, Res, undefined, #{})).


grouper_test() ->
    test_init(),
    M1 = dev_process:test_aos_process(),
    M2 = #{<<"path">> => <<"compute">>, <<"v">> => 1},
    M3 = #{<<"path">> => <<"compute">>, <<"v">> => 2},
    M4 = #{<<"path">> => <<"not-compute">>, <<"v">> => 3},
    G1 = hb_persistent:group(M1, M2, #{process_workers => true}),
    G2 = hb_persistent:group(M1, M3, #{process_workers => true}),
    G3 = hb_persistent:group(M1, M4, #{process_workers => true}),
    ?event({group_samples, {g1, G1}, {g2, G2}, {g3, G3}}),
    ?assertEqual(G1, G2),
    ?assertNotEqual(G1, G3).
