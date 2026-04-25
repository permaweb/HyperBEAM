
%%% @doc A long-lived process worker that keeps state in memory between
%%% calls. Implements the interface of `hb_ao' to receive and respond 
%%% to computation requests regarding a process as a singleton.
-module(dev_process_worker).
-export([server/3, stop/1, group/3, await/5, notify_compute/4]).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Returns a group name for a request. The worker is responsible for all
%% computation work on the same process on a single node, so we use the
%% process ID as the group name -- but only when the request is one that
%% actually needs the worker to compute. `compute' requests whose result
%% is already available in the local cache return `ungrouped_exec' so
%% that `hb_persistent:find_or_register/3' short-circuits the wait and
%% the device's `compute/3' can serve the cached message immediately.
%% This keeps the dedup invariants for genuine compute work while
%% letting concurrent cache-hit reads run in parallel.
group(Base, undefined, Opts) ->
    hb_persistent:default_grouper(Base, undefined, Opts);
group(Base, Req, Opts) ->
    case hb_opts:get(process_workers, false, Opts) of
        false ->
            hb_persistent:default_grouper(Base, Req, Opts);
        true ->
            case Req of
                undefined ->
                    hb_persistent:default_grouper(Base, undefined, Opts);
                _ ->
                    case hb_path:matches(<<"compute">>, hb_path:hd(Req, Opts)) of
                        true ->
                            compute_group(Base, Req, Opts);
                        _ ->
                            hb_persistent:default_grouper(Base, Req, Opts)
                    end
            end
    end.

%% @doc Decide which group to enrol a `compute' request into. Cache-hit
%% reads bypass the per-process queue via `ungrouped_exec'; everything
%% else is serialised through the worker keyed on the process ID.
compute_group(Base, Req, Opts) ->
    ProcID = process_to_group_name(Base, Opts),
    case requested_compute_already_cached(ProcID, Req, Opts) of
        true ->
            ?event(worker,
                {compute_cache_hit_bypassing_queue,
                    {proc_id, ProcID},
                    {req, Req}
                },
                Opts
            ),
            ungrouped_exec;
        false ->
            ProcID
    end.

%% @doc Returns `true' iff `dev_process:compute/3' will resolve the
%% request from the local cache without spawning or awaiting work.
%% Mirrors the lookup rules in `dev_process:compute/3':
%%
%%   * Explicit `compute' or `slot' key -- cached iff that slot number
%%     is already written.
%%   * No slot -- the device falls into the cache-only branch of
%%     `now/3', so cached iff at least one slot exists for the process.
%%
%% Any error on the cache lookup falls back to `false', so the caller
%% defaults to the existing per-process group and behaviour is, at
%% worst, unchanged.
requested_compute_already_cached(ProcID, Req, Opts) ->
    try
        case dev_process_cache:latest_slot(ProcID, Opts) of
            {ok, _LatestCached} ->
                case requested_slot(Req, Opts) of
                    undefined -> true;
                    Slot ->
                        case dev_process_cache:read(ProcID, Slot, Opts) of
                            {ok, _} -> true;
                            _ -> false
                        end
                end;
            _ ->
                false
        end
    catch
        _:_ -> false
    end.

%% @doc Extract the requested slot number from a `compute' request, if
%% any. Mirrors `dev_process:compute/3''s lookup order: an explicit
%% `compute' key first, then `slot'. Returns `undefined' when neither
%% is present, or when the value cannot be parsed as a non-negative
%% integer.
requested_slot(Req, Opts) ->
    Raw =
        hb_ao:get_first(
            [
                {{as, <<"message@1.0">>, Req}, <<"compute">>},
                {{as, <<"message@1.0">>, Req}, <<"slot">>}
            ],
            Opts
        ),
    case Raw of
        not_found -> undefined;
        _ ->
            try hb_util:int(Raw) of
                Int when is_integer(Int), Int >= 0 -> Int;
                _ -> undefined
            catch
                _:_ -> undefined
            end
    end.

process_to_group_name(Base, Opts) ->
    Initialized = dev_process_lib:ensure_process_key(Base, Opts),
    ProcMsg =
        hb_ao:get(<<"process">>, Initialized, Opts#{ <<"hashpath">> => ignore }),
    ID = hb_message:id(ProcMsg, all),
    ?event({process_to_group_name, {id, ID}, {base, Base}}),
    hb_util:human_id(ID).

%% @doc Spawn a new worker process. This is called after the end of the first
%% execution of `hb_ao:resolve/3', so the state we are given is the
%% already current.
server(GroupName, Base, Opts) ->
    ServerOpts = Opts#{
        <<"await-inprogress">> => false,
        <<"spawn-worker">> => false,
        <<"process-workers">> => false
    },
    % The maximum amount of time the worker will wait for a request before
    % checking the cache for a snapshot. Default: 5 minutes.
    Timeout = hb_opts:get(process_worker_max_idle, 300_000, Opts),
    ?event(worker, {waiting_for_req, {group, GroupName}}),
    receive
        {resolve, Listener, GroupName, Req, ListenerOpts} ->
            TargetSlot = hb_ao:get(<<"slot">>, Req, Opts),
            ?event(worker,
                {work_received,
                    {group, GroupName},
                    {slot, TargetSlot},
                    {listener, Listener}
                }
            ),
            Res =
                hb_ao:resolve(
                    Base,
                    #{ <<"path">> => <<"compute">>, <<"slot">> => TargetSlot },
                    hb_maps:merge(ListenerOpts, ServerOpts, Opts)
                ),
            ?event(worker, {work_done, {group, GroupName}, {req, Req}, {res, Res}}),
            send_notification(Listener, GroupName, TargetSlot, Res),
            server(
                GroupName,
                case Res of
                    {ok, Res} -> Res;
                    _ -> Base
                end,
                Opts
            );
        stop ->
            ?event(worker, {stopping, {group, GroupName}, {base, Base}}),
            exit(normal)
    after Timeout ->
        % We have hit the in-memory persistence timeout. Generate a snapshot
        % of the current process state and ensure it is cached.
        hb_ao:resolve(
            Base,
            <<"snapshot">>,
            ServerOpts#{ <<"cache-control">> => [<<"store">>] }
        ),
        % Return the current process state.
        {ok, Base}
    end.

%% @doc Await a resolution from a worker executing the `process@1.0' device.
await(Worker, GroupName, Base, Req, Opts) ->
    case hb_path:matches(<<"compute">>, hb_path:hd(Req, Opts)) of
        false -> 
            hb_persistent:default_await(Worker, GroupName, Base, Req, Opts);
        true ->
            TargetSlot = hb_ao:get(<<"slot">>, Req, any, Opts),
            ?event({awaiting_compute, 
                {worker, Worker},
                {group, GroupName},
                {target_slot, TargetSlot}
            }),
            receive
                {resolved, _, GroupName, {slot, RecvdSlot}, Res}
                        when RecvdSlot == TargetSlot orelse TargetSlot == any ->
                    ?event(debug_compute, {notified_of_resolution,
                        {target, TargetSlot},
                        {group, GroupName}
                    }),
                    Res;
                {resolved, _, GroupName, {slot, RecvdSlot}, _Res} ->
                    ?event(debug_compute, {waiting_again,
                        {target, TargetSlot},
                        {recvd, RecvdSlot},
                        {worker, Worker},
                        {group, GroupName}
                    }),
                    await(Worker, GroupName, Base, Req, Opts);
                {'DOWN', _R, process, Worker, _Reason} ->
                    ?event(debug_compute,
                        {leader_died,
                            {group, GroupName},
                            {leader, Worker},
                            {target, TargetSlot}
                        }
                    ),
                    {error, leader_died}
            end
    end.

%% @doc Notify any waiters for a specific slot of the computed results.
notify_compute(GroupName, SlotToNotify, Res, Opts) ->
    notify_compute(GroupName, SlotToNotify, Res, Opts, 0).
notify_compute(GroupName, SlotToNotify, Res, Opts, Count) ->
    ?event({notifying_of_computed_slot, {group, GroupName}, {slot, SlotToNotify}}),
    receive
        {resolve, Listener, GroupName, #{ <<"slot">> := SlotToNotify }, _ListenerOpts} ->
            send_notification(Listener, GroupName, SlotToNotify, Res),
            notify_compute(GroupName, SlotToNotify, Res, Opts, Count + 1);
        {resolve, Listener, GroupName, Msg, _ListenerOpts}
                when is_map(Msg) andalso not is_map_key(<<"slot">>, Msg) ->
            send_notification(Listener, GroupName, SlotToNotify, Res),
            notify_compute(GroupName, SlotToNotify, Res, Opts, Count + 1)
    after 0 ->
        ?event(worker_short,
            {finished_notifying,
                {group, GroupName},
                {slot, SlotToNotify},
                {listeners, Count}
            }
        )
    end.

send_notification(Listener, GroupName, SlotToNotify, Res) ->
    ?event({sending_notification, {group, GroupName}, {slot, SlotToNotify}}),
    Listener ! {resolved, self(), GroupName, {slot, SlotToNotify}, Res}.

%% @doc Stop a worker process.
stop(Worker) ->
    exit(Worker, normal).

%%% Tests

test_init() ->
    application:ensure_all_started(hb),
    ok.

info_test() ->
    test_init(),
    M1 = dev_process_test_vectors:wasm_process(<<"test/aos-2-pure-xs.wasm">>),
    Res = hb_ao_device:info(M1, #{}),
    ?assertEqual(fun dev_process_worker:group/3, hb_maps:get(grouper, Res, undefined, #{})).

grouper_test() ->
    test_init(),
    M1 = dev_process_test_vectors:aos_process(),
    M2 = #{ <<"path">> => <<"compute">>, <<"v">> => 1 },
    M3 = #{ <<"path">> => <<"compute">>, <<"v">> => 2 },
    M4 = #{ <<"path">> => <<"not-compute">>, <<"v">> => 3 },
    G1 = hb_persistent:group(M1, M2, #{ <<"process-workers">> => true }),
    G2 = hb_persistent:group(M1, M3, #{ <<"process-workers">> => true }),
    G3 = hb_persistent:group(M1, M4, #{ <<"process-workers">> => true }),
    ?event({group_samples, {g1, G1}, {g2, G2}, {g3, G3}}),
    ?assertEqual(G1, G2),
    ?assertNotEqual(G1, G3).

%% @doc `compute' requests whose result is already in the local cache
%% should bypass the per-process worker queue (returning the
%% `ungrouped_exec' sentinel that `hb_persistent:find_or_register/3'
%% short-circuits). Requests that still need work, and requests for a
%% slot beyond what is cached, must continue to serialise through the
%% process group.
grouper_skips_when_slot_cached_test() ->
    test_init(),
    Opts =
        #{
            <<"store">> => hb_test_utils:test_store(hb_store_lmdb),
            <<"priv-wallet">> => ar_wallet:new()
        },
    M1 = dev_process_test_vectors:aos_process(Opts),
    POpts = Opts#{ <<"process-workers">> => true },
    %% With the cache empty, every compute request must group by
    %% process so that the worker can do the actual work.
    Uncached = #{ <<"path">> => <<"compute">>, <<"slot">> => 5 },
    ProcessGroup = hb_persistent:group(M1, Uncached, POpts),
    ?assertNotEqual(ungrouped_exec, ProcessGroup),
    %% Write slot 5 into the cache. The same request now has a result
    %% available and the grouper should step out of the queue.
    {ok, _} =
        dev_process_cache:write(
            ProcessGroup,
            5,
            #{ <<"hello">> => <<"cached">> },
            Opts
        ),
    ?assertEqual(
        ungrouped_exec,
        hb_persistent:group(M1, Uncached, POpts)
    ),
    %% Cache slots are not assumed to be gap-free. A lower slot that
    %% has not actually been written must still go through the worker.
    MissingLower = #{ <<"path">> => <<"compute">>, <<"slot">> => 4 },
    ?assertEqual(ProcessGroup, hb_persistent:group(M1, MissingLower, POpts)),
    %% A request for a slot beyond what we cached must still be
    %% serialised through the worker.
    Beyond = #{ <<"path">> => <<"compute">>, <<"slot">> => 999 },
    ?assertEqual(ProcessGroup, hb_persistent:group(M1, Beyond, POpts)),
    %% A `compute' request without a slot resolves via the cache-only
    %% branch of `now/3' once any slot exists, so it also bypasses the
    %% queue.
    NoSlot = #{ <<"path">> => <<"compute">> },
    ?assertEqual(
        ungrouped_exec,
        hb_persistent:group(M1, NoSlot, POpts)
    ).
