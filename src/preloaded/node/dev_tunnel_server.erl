%%% @doc Node-local broker for reverse HTTP registrations and client workers.
%%% Each request is dispatched once. Finite dispatched requests remain until
%%% their response or deadline, even if the caller disconnects. The broker
%%% never repeats a computation whose outcome it cannot know.
-module(dev_tunnel_server).

-export([register/3, call/5, response/4, connect/7, status/1, stop/1]).
-export([connected/2]).

-include("include/hb.hrl").

-define(PRESENCE_GRACE, 1_000).
-define(RESTART_DELAY, 1_000).

-record(waiting, {
    type, address, from, monitor, timer,
    id, request, mode = explicit
}).
-record(worker, {pool, stop_ref, monitor, retire_timer = undefined}).
-record(state, {
    name,
    waiting = [],
    dispatched = #{},
    pools = #{},
    workers = #{},
    recent = #{}
}).

%% @doc Wait for one request addressed to the registering node.
register(Key, Address, Timeout) ->
    case valid_timeout(Timeout) of
        true -> server_call(Key, {register, Address, Timeout});
        false -> invalid_control(<<"Invalid tunnel registration timeout.">>)
    end.

%% @doc Queue an explicit call, or a host call while its target is present.
call(Key, Address, Request, Timeout, Mode) ->
    case
        valid_timeout(Timeout) andalso
            (Mode =:= explicit orelse
                (Mode =:= host andalso Timeout =/= infinity))
    of
        true -> server_call(Key, {call, Address, Request, Timeout, Mode});
        false -> invalid_control(<<"Invalid tunnel request controls.">>)
    end.

%% @doc Deliver a response authenticated by the dispatched target's address.
response(Key, ID, Address, Response) ->
    server_call(Key, {response, ID, Address, Response}).

%% @doc Return whether a target has a waiting stream, a running request or a
%% renewing stream.
connected(Key, Address) ->
    server_call(Key, {connected, Address}).

%% @doc Resize an owned client pool without interrupting its current work.
%% A pool resized to no workers is removed once its workers retire.
connect(Key, Peer, Address, Workers, MaxWorkers, RetireTimeout, StartFun) ->
    case
        is_integer(Workers) andalso Workers >= 0 andalso
            (MaxWorkers =:= infinity orelse
                (is_integer(MaxWorkers) andalso MaxWorkers > 0)) andalso
            is_integer(RetireTimeout) andalso RetireTimeout > 0 andalso
            is_function(StartFun, 1)
    of
        true ->
            server_call(
                Key,
                {
                    connect,
                    Peer,
                    Address,
                    Workers,
                    MaxWorkers,
                    RetireTimeout,
                    StartFun
                }
            );
        false -> invalid_control(<<"Invalid tunnel client controls.">>)
    end.

%% @doc Return waiting, dispatched, and owned-worker counts for this node.
status(Key) ->
    server_call(Key, status).

%% @doc Stop this node's broker and all workers that it owns.
stop(Key) ->
    case hb_name:lookup({?MODULE, Key}) of
        undefined -> ok;
        PID -> gen_server:call(PID, stop, infinity)
    end.

%% @doc Use standard monitored RPC with a node-scoped receive-loop singleton.
server_call(Key, Request) ->
    Name = {?MODULE, Key},
    PID = hb_name:singleton(Name, fun() -> run(Name) end),
    gen_server:call(PID, Request, infinity).

%% @doc Own the registered name and linked workers for the broker's lifetime.
run(Name) ->
    process_flag(trap_exit, true),
    try loop(#state{ name = Name })
    after
        case hb_name:lookup(Name) of
            PID when PID =:= self() -> hb_name:unregister(Name);
            _ -> ok
        end
    end.

%% @doc Serialize admission, dispatch, replies, and resource cleanup.
loop(State) ->
    receive
        {'$gen_call', From, {connected, Address}} ->
            gen_server:reply(From, present(Address, State)),
            loop(State);
        {'$gen_call', From, {register, Address, Timeout}} ->
            Registration = new_waiting(register, Address, From, Timeout),
            loop(settle(Address, enqueue(Registration, State)));
        {'$gen_call', From, {call, Address, Request, Timeout, Mode}} ->
            case Mode =:= explicit orelse present(Address, State) of
                true ->
                    Call =
                        (new_waiting(call, Address, From, Timeout))#waiting{
                            id = hb_util:human_id(crypto:strong_rand_bytes(32)),
                            request = Request,
                            mode = Mode
                        },
                    loop(settle(Address, enqueue(Call, State)));
                false ->
                    gen_server:reply(From, unavailable(Address)),
                    loop(State)
            end;
        {'$gen_call', From, {response, ID, Address, Response}} ->
            {Reply, Next} = deliver_response(ID, Address, Response, State),
            gen_server:reply(From, Reply),
            loop(Next);
        {'$gen_call', From,
                {connect, Peer, Address, Count, Max, Retire, StartFun}} ->
            {Reply, Next} =
                resize_pool(
                    {Peer, Address},
                    Count,
                    Max,
                    Retire,
                    StartFun,
                    State
                ),
            gen_server:reply(From, Reply),
            loop(Next);
        {'$gen_call', From, status} ->
            gen_server:reply(From, {ok, status_message(State)}),
            loop(State);
        {'$gen_call', From, stop} ->
            shutdown(State),
            hb_name:unregister(State#state.name),
            gen_server:reply(From, ok);
        {timeout, _Timer, {waiting, Monitor}} ->
            loop(remove_waiting(Monitor, timeout, State));
        {timeout, Timer, {presence, Address}} ->
            Recent = State#state.recent,
            case maps:find(Address, Recent) of
                {ok, {_Until, Timer}} ->
                    loop(
                        release_offline(
                            Address,
                            State#state{ recent = maps:remove(Address, Recent) }
                        )
                    );
                _ -> loop(State)
            end;
        {timeout, Timer, {retire, PID}} ->
            case maps:find(PID, State#state.workers) of
                {ok, #worker{ retire_timer = Timer }} -> exit(PID, kill);
                _ -> ok
            end,
            loop(State);
        {'DOWN', Monitor, process, PID, Reason} ->
            case maps:find(PID, State#state.workers) of
                {ok, Worker = #worker{ monitor = Monitor }} ->
                    loop(worker_down(PID, Worker, Reason, State));
                _ -> loop(remove_waiting(Monitor, down, State))
            end;
        _ ->
            loop(State)
    end.

%% @doc Monitor an HTTP caller and start its independent wait deadline.
new_waiting(Type, Address, From = {PID, _Tag}, Timeout) ->
    Monitor = erlang:monitor(process, PID),
    #waiting{
        type = Type,
        address = Address,
        from = From,
        monitor = Monitor,
        timer = start_timer(Timeout, {waiting, Monitor})
    }.

%% @doc Preserve arrival order for both registrations and explicit callers.
enqueue(Waiting, State) ->
    State#state{ waiting = State#state.waiting ++ [Waiting] }.

%% @doc Match available streams, then release host calls for absent targets.
settle(Address, State) ->
    release_offline(Address, dispatch(Address, State)).

%% @doc Dispatch each queued request at most once to the oldest live stream.
dispatch(Address, State) ->
    case take_waiting(call, Address, State#state.waiting) of
        none -> State;
        {Call, Rest} ->
            case alive(Call) of
                false ->
                    cleanup(Call),
                    dispatch(Address, State#state{ waiting = Rest });
                true -> dispatch_registration(Address, Call, State)
            end
    end.

%% @doc Remove dead registrations before choosing the stream for a caller.
dispatch_registration(Address, Call, State) ->
    case take_waiting(register, Address, State#state.waiting) of
        none -> State;
        {Registration, Rest} ->
            case alive(Registration) of
                false ->
                    cleanup(Registration),
                    dispatch(Address, State#state{ waiting = Rest });
                true ->
                    cleanup(Registration),
                    ID = Call#waiting.id,
                    gen_server:reply(
                        Registration#waiting.from,
                        {ok, #{
                            <<"tunnel-request-id">> => ID,
                            <<"tunnel-target">> => Address,
                            <<"request">> => Call#waiting.request
                        }}
                    ),
                    dispatch(
                        Address,
                        State#state{
                            waiting = lists:delete(Call, Rest),
                            dispatched = (State#state.dispatched)#{
                                ID => Call#waiting{ request = undefined }
                            }
                        }
                    )
            end
    end.

%% @doc Take the oldest waiting entry of a type for an address.
take_waiting(Type, Address, Waiting) ->
    case
        lists:splitwith(
            fun(#waiting{ type = EntryType, address = EntryAddress }) ->
                Type =/= EntryType orelse Address =/= EntryAddress
            end,
            Waiting
        )
    of
        {_Before, []} -> none;
        {Before, [Entry | After]} -> {Entry, Before ++ After}
    end.

%% @doc Consume only a dispatched request authenticated by its target.
deliver_response(ID, Address, Response, State) ->
    case maps:find(ID, State#state.dispatched) of
        {ok, Call = #waiting{ address = Address }} ->
            cleanup(Call),
            Next =
                mark_recent(
                    Address,
                    State#state{
                        dispatched = maps:remove(ID, State#state.dispatched)
                    }
                ),
            case alive(Call) of
                true ->
                    gen_server:reply(Call#waiting.from, Response),
                    {{ok, accepted}, Next};
                false -> {unknown_request(ID), Next}
            end;
        {ok, _} ->
            {{error, #{
                <<"status">> => 403,
                <<"body">> =>
                    <<"Tunnel response signer does not match its address.">>
            }}, State};
        error ->
            {unknown_request(ID), State}
    end.

%% @doc Remove expired work or a disconnected caller that is still queued.
remove_waiting(Monitor, Reason, State) ->
    case take_monitor(Monitor, Reason, State) of
        none -> State;
        {Entry, Next} ->
            cleanup(Entry),
            case Reason of
                timeout -> gen_server:reply(Entry#waiting.from, expired(Entry));
                down -> ok
            end,
            settle(
                Entry#waiting.address,
                case {Reason, Entry#waiting.type} of
                    {timeout, register} ->
                        mark_recent(Entry#waiting.address, Next);
                    _ -> Next
                end
            )
    end.

%% @doc Take the queued entry or the dispatched request of a monitor. A
%% dispatched request with a finite wait stays until its response or its
%% deadline, even if its caller ends.
take_monitor(Monitor, Reason, State) ->
    case lists:keytake(Monitor, #waiting.monitor, State#state.waiting) of
        {value, Entry, Rest} -> {Entry, State#state{ waiting = Rest }};
        false ->
            case
                [
                    {ID, Entry}
                ||
                    {ID, Entry = #waiting{ monitor = Ref }} <-
                        maps:to_list(State#state.dispatched),
                    Ref =:= Monitor
                ]
            of
                [] -> none;
                [{_, #waiting{ timer = Timer }}]
                        when Reason =:= down, Timer =/= undefined -> none;
                [{ID, Entry}] ->
                    {Entry, State#state{
                        dispatched = maps:remove(ID, State#state.dispatched)
                    }}
            end
    end.

%% @doc Return whether an address has a live registration or a dispatched
%% request, or had a registration expire or a response delivered in the last
%% `PRESENCE_GRACE' ms.
present(Address, State) ->
    lists:any(
        fun(Entry = #waiting{ type = Type, address = Target }) ->
            Type =:= register andalso Address =:= Target andalso alive(Entry)
        end,
        State#state.waiting
    ) orelse
        lists:any(
            fun(#waiting{ address = Target }) -> Address =:= Target end,
            maps:values(State#state.dispatched)
        ) orelse
            case maps:find(Address, State#state.recent) of
                {ok, {Until, _Timer}} ->
                    erlang:monotonic_time(millisecond) < Until;
                error -> false
            end.

%% @doc Mark an address present for `PRESENCE_GRACE' ms, while one of its
%% workers registers again.
mark_recent(Address, State) ->
    case maps:find(Address, State#state.recent) of
        {ok, {_Until, OldTimer}} -> cancel_timer(OldTimer);
        error -> ok
    end,
    Timer = start_timer(?PRESENCE_GRACE, {presence, Address}),
    State#state{ recent = (State#state.recent)#{
        Address => {erlang:monotonic_time(millisecond) + ?PRESENCE_GRACE, Timer}
    }}.

%% @doc Release the queued host calls of an address that is not present.
%% Explicit calls stay queued.
release_offline(Address, State) ->
    case present(Address, State) of
        true -> State;
        false ->
            {Release, Keep} =
                lists:partition(
                    fun(#waiting{ type = Type, address = To, mode = Mode }) ->
                        Type =:= call andalso To =:= Address andalso
                            Mode =:= host
                    end,
                    State#state.waiting
                ),
            lists:foreach(
                fun(Entry) ->
                    cleanup(Entry),
                    gen_server:reply(Entry#waiting.from, unavailable(Address))
                end,
                Release
            ),
            State#state{ waiting = Keep }
    end.

%% @doc Resize in place, counting retiring workers against the node-wide limit.
resize_pool(Key = {Peer, Address}, Count, Max, Retire, StartFun, State) ->
    Active = active_workers(Key, State),
    Grow = erlang:max(0, Count - length(Active)),
    case Max =:= infinity orelse map_size(State#state.workers) + Grow =< Max of
        false ->
            {{error, #{
                <<"status">> => 429,
                <<"body">> => <<"Tunnel client worker limit exceeded.">>
            }}, State};
        true ->
            Pools =
                case Count of
                    0 -> maps:remove(Key, State#state.pools);
                    _ -> (State#state.pools)#{ Key => StartFun }
                end,
            WithPool = State#state{ pools = Pools },
            Shrunk =
                lists:foldl(
                    fun(PID, Acc) -> retire_worker(PID, Retire, Acc) end,
                    WithPool,
                    lists:sublist(Active, erlang:max(0, length(Active) - Count))
                ),
            Next =
                lists:foldl(
                    fun(_, Acc) -> start_worker(Key, StartFun, 0, Acc) end,
                    Shrunk,
                    lists:seq(1, Grow)
                ),
            {{ok, #{
                <<"status">> => 202,
                <<"body">> => <<"Tunnel client pool resized.">>,
                <<"workers">> => Count,
                <<"address">> => Address,
                <<"peer">> => Peer
            }}, Next}
    end.

%% @doc List workers still accepting future registrations for a pool.
active_workers(Key, State) ->
    [
        PID
        || {PID, #worker{ pool = Pool, retire_timer = Timer }} <-
            maps:to_list(State#state.workers),
           Pool =:= Key, Timer =:= undefined
    ].

%% @doc Link a worker to its owner and monitor its capacity independently.
start_worker(Key, StartFun, Delay, State) ->
    StopRef = make_ref(),
    {PID, Monitor} =
        spawn_opt(
            fun() ->
                timer:sleep(Delay),
                StartFun(StopRef)
            end,
            [link, monitor]
        ),
    State#state{
        workers = (State#state.workers)#{
            PID => #worker{ pool = Key, stop_ref = StopRef, monitor = Monitor }
        }
    }.

%% @doc Let an excess worker finish its current request within a bounded grace.
retire_worker(PID, Timeout, State) ->
    Worker = maps:get(PID, State#state.workers),
    PID ! {tunnel_client_stop, Worker#worker.stop_ref},
    State#state{
        workers = (State#state.workers)#{
            PID => Worker#worker{
                retire_timer = start_timer(Timeout, {retire, PID})
            }
        }
    }.

%% @doc Replace an active worker after death, but never restart retired workers.
worker_down(PID, Worker, Reason, State) ->
    Next = State#state{ workers = maps:remove(PID, State#state.workers) },
    case Worker#worker.retire_timer of
        undefined ->
            ?event(
                warning,
                {tunnel_client_worker_exit, Worker#worker.pool, Reason}
            ),
            start_worker(
                Worker#worker.pool,
                maps:get(Worker#worker.pool, State#state.pools),
                ?RESTART_DELAY,
                Next
            );
        Timer ->
            cancel_timer(Timer),
            Next
    end.

%% @doc Report queue and worker counts without retaining duplicate counters.
status_message(State) ->
    Retiring =
        length([
            ok
        ||
            #worker{ retire_timer = Timer } <- maps:values(State#state.workers),
            Timer =/= undefined
        ]),
    #{
        <<"registered">> => waiting_counts(register, State),
        <<"pending">> => waiting_counts(call, State),
        <<"open-registrations">> =>
            length([ok || #waiting{ type = register } <- State#state.waiting]),
        <<"open-requests">> =>
            map_size(State#state.dispatched) +
                length([ok || #waiting{ type = call } <- State#state.waiting]),
        <<"dispatched-requests">> => map_size(State#state.dispatched),
        <<"open-client-pools">> => map_size(State#state.pools),
        <<"open-client-workers">> => map_size(State#state.workers) - Retiring,
        <<"retiring-client-workers">> => Retiring
    }.

%% @doc Count the waiting entries of a type for each address.
waiting_counts(Type, State) ->
    lists:foldl(
        fun(#waiting{ type = EntryType, address = Address }, Counts)
                when EntryType =:= Type ->
                maps:update_with(Address, fun(N) -> N + 1 end, 1, Counts);
           (_, Counts) -> Counts
        end,
        #{},
        State#state.waiting
    ).

%% @doc Cancel one caller's timer and monitor at its terminal transition.
cleanup(#waiting{ timer = Timer, monitor = Monitor }) ->
    cancel_timer(Timer),
    erlang:demonitor(Monitor, [flush]).

%% @doc Inspect caller liveness before handing its stream to another process.
alive(#waiting{ from = {PID, _Tag} }) ->
    erlang:is_process_alive(PID).

%% @doc Start the timer of a finite wait. An infinite wait ends only with its
%% caller.
start_timer(infinity, _Message) -> undefined;
start_timer(Timeout, Message) -> erlang:start_timer(Timeout, self(), Message).

%% @doc Cancel a timer whose queued message is harmless after entry removal.
cancel_timer(undefined) -> ok;
cancel_timer(Timer) -> erlang:cancel_timer(Timer).

%% @doc Accept `infinity', or a number of milliseconds that is not negative
%% and that the runtime's timers take: at most `16#FFFFFFFF', about 49 days.
valid_timeout(infinity) -> true;
valid_timeout(Timeout) ->
    is_integer(Timeout) andalso Timeout >= 0 andalso Timeout =< 16#FFFFFFFF.

%% @doc Release all callers and terminate only broker-owned workers.
shutdown(State) ->
    maps:foreach(
        fun(PID, #worker{ monitor = Monitor, retire_timer = Timer }) ->
            cancel_timer(Timer),
            exit(PID, kill),
            erlang:demonitor(Monitor, [flush])
        end,
        State#state.workers
    ),
    maps:foreach(
        fun(_Address, {_Until, Timer}) -> cancel_timer(Timer) end,
        State#state.recent
    ),
    lists:foreach(
        fun(Entry) ->
            cleanup(Entry),
            Reply =
                case Entry#waiting.type of
                    register -> {ok, #{ <<"status">> => 204 }};
                    call -> {error, #{
                        <<"status">> => 503,
                        <<"reason">> => <<"tunnel-unavailable">>,
                        <<"body">> => <<"Tunnel broker is stopping.">>,
                        <<"tunnel-target">> => Entry#waiting.address,
                        <<"tunnel-request-id">> => Entry#waiting.id
                    }}
                end,
            gen_server:reply(Entry#waiting.from, Reply)
        end,
        State#state.waiting ++ maps:values(State#state.dispatched)
    ).

%% @doc Complete idle registration or report a caller's response deadline.
expired(#waiting{ type = register }) ->
    {ok, #{ <<"status">> => 204 }};
expired(#waiting{ address = Address, id = ID }) ->
    {error, #{
        <<"status">> => 504,
        <<"reason">> => <<"tunnel-timeout">>,
        <<"body">> => <<"Timed out waiting for tunneled response.">>,
        <<"tunnel-target">> => Address,
        <<"tunnel-request-id">> => ID
    }}.

%% @doc Report a host call for a target that is not connected.
unavailable(Address) ->
    {error, #{
        <<"status">> => 404,
        <<"reason">> => <<"tunnel-unavailable">>,
        <<"body">> => <<"Requested tunnel proxy is unavailable.">>,
        <<"tunnel-target">> => Address
    }}.

%% @doc Report a response to a request that no caller waits for.
unknown_request(ID) ->
    {error, #{
        <<"status">> => 404,
        <<"body">> => <<"Unknown tunnel request.">>,
        <<"tunnel-request-id">> => ID
    }}.

%% @doc Report controls that the broker refuses.
invalid_control(Body) ->
    {error, #{ <<"status">> => 400, <<"body">> => Body }}.
