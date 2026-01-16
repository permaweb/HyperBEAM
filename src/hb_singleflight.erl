-module(hb_singleflight).
-export([start/0, do/2, do/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(OWNERS_TABLE, hb_singleflight_owners).
-define(WAITERS_TABLE, hb_singleflight_waiters).
-define(DEFAULT_TIMEOUT, 300000).
-define(TABLE_HOLDER, hb_singleflight_table_holder).

start() ->
    ensure_tables_exist(),
    ok.

ensure_tables_exist() ->
    case tables_ready() of
        true -> ok;
        false -> start_table_holder()
    end.

tables_ready() ->
    ets:info(?OWNERS_TABLE) =/= undefined andalso
    ets:info(?WAITERS_TABLE) =/= undefined.

start_table_holder() ->
    Parent = self(),
    Ref = make_ref(),
    spawn(fun() -> table_holder_init(Parent, Ref) end),
    receive
        {table_holder_ready, Ref} -> ok;
        {table_holder_exists, Ref} ->
            wait_for_tables()
    after 5000 ->
        case tables_ready() of
            true -> ok;
            false -> error(table_holder_timeout)
        end
    end.

wait_for_tables() ->
    case tables_ready() of
        true -> ok;
        false ->
            timer:sleep(1),
            wait_for_tables()
    end.

table_holder_init(Parent, Ref) ->
    try
        create_tables(),
        register(?TABLE_HOLDER, self()),
        Parent ! {table_holder_ready, Ref},
        table_holder_loop()
    catch
        error:badarg ->
            Parent ! {table_holder_exists, Ref}
    end.

create_tables() ->
    ets:new(?OWNERS_TABLE, [
        named_table,
        public,
        set,
        {write_concurrency, true},
        {read_concurrency, true}
    ]),
    ets:new(?WAITERS_TABLE, [
        named_table,
        public,
        bag,
        {write_concurrency, true},
        {read_concurrency, true}
    ]).

table_holder_loop() ->
    receive
        stop -> ok
    end.

do(Key, Fun) ->
    Timeout = hb_opts:get(singleflight_timeout, ?DEFAULT_TIMEOUT),
    do(Key, Fun, Timeout).

do(Key, Fun, Timeout) ->
    start(),
    Self = self(),
    OwnershipRef = make_ref(),
    StartedAt = erlang:monotonic_time(millisecond),
    case ets:insert_new(?OWNERS_TABLE, {Key, Self, StartedAt, OwnershipRef}) of
        true ->
            execute_as_owner(Key, Fun, OwnershipRef);
        false ->
            maybe_become_owner_or_wait(Key, Fun, Self, Timeout)
    end.

execute_as_owner(Key, Fun, OwnershipRef) ->
    Result = try
        {ok, Fun()}
    catch
        Class:Reason:Stacktrace ->
            {error, Class, Reason, Stacktrace}
    end,
    notify_and_cleanup(Key, OwnershipRef, Result),
    unwrap_result(Result).

notify_and_cleanup(Key, OwnershipRef, Result) ->
    case ets:lookup(?OWNERS_TABLE, Key) of
        [{Key, _, _, OwnershipRef}] ->
            Waiters = ets:lookup(?WAITERS_TABLE, Key),
            MatchingWaiters = [W || W = {_, _, _, ORef} <- Waiters, ORef =:= OwnershipRef],
            lists:foreach(
                fun({_Key, WaiterPid, WaiterRef, _ORef}) ->
                    WaiterPid ! {singleflight_result, WaiterRef, OwnershipRef, Result}
                end,
                MatchingWaiters
            ),
            ets:delete(?OWNERS_TABLE, Key),
            ets:match_delete(?WAITERS_TABLE, {Key, '_', '_', OwnershipRef});
        _ ->
            ok
    end.

maybe_become_owner_or_wait(Key, Fun, Self, Timeout) ->
    case check_stale_owner(Key, Timeout) of
        {stale, _StaleOwnershipRef} ->
            ets:delete(?OWNERS_TABLE, Key),
            OwnershipRef = make_ref(),
            StartedAt = erlang:monotonic_time(millisecond),
            case ets:insert_new(?OWNERS_TABLE, {Key, Self, StartedAt, OwnershipRef}) of
                true ->
                    execute_as_owner(Key, Fun, OwnershipRef);
                false ->
                    wait_for_result(Key, Timeout)
            end;
        {active, OwnershipRef} ->
            wait_for_result_with_ref(Key, OwnershipRef, Timeout)
    end.

check_stale_owner(Key, Timeout) ->
    case ets:lookup(?OWNERS_TABLE, Key) of
        [] ->
            {stale, undefined};
        [{_Key, OwnerPid, StartedAt, OwnershipRef}] ->
            Now = erlang:monotonic_time(millisecond),
            Elapsed = Now - StartedAt,
            IsAlive = is_process_alive(OwnerPid),
            case IsAlive andalso Elapsed < Timeout of
                true -> {active, OwnershipRef};
                false -> {stale, OwnershipRef}
            end
    end.

wait_for_result(Key, Timeout) ->
    case ets:lookup(?OWNERS_TABLE, Key) of
        [{_Key, _Pid, _StartedAt, OwnershipRef}] ->
            wait_for_result_with_ref(Key, OwnershipRef, Timeout);
        [] ->
            {error, timeout}
    end.

wait_for_result_with_ref(Key, OwnershipRef, Timeout) ->
    WaiterRef = make_ref(),
    ets:insert(?WAITERS_TABLE, {Key, self(), WaiterRef, OwnershipRef}),
    receive
        {singleflight_result, WaiterRef, OwnershipRef, Result} ->
            unwrap_result(Result)
    after Timeout ->
        ets:delete_object(?WAITERS_TABLE, {Key, self(), WaiterRef, OwnershipRef}),
        {error, timeout}
    end.

unwrap_result({ok, Value}) ->
    Value;
unwrap_result({error, Class, Reason, Stacktrace}) ->
    erlang:raise(Class, Reason, Stacktrace).

%%% Tests

basic_execution_test() ->
    start(),
    Result = do(test_key_basic, fun() -> 42 end),
    ?assertEqual(42, Result).

concurrent_dedup_test() ->
    start(),
    Counter = ets:new(counter, [public, set]),
    ets:insert(Counter, {count, 0}),
    Key = {test_key_concurrent, make_ref()},
    ExpensiveFun = fun() ->
        ets:update_counter(Counter, count, 1),
        timer:sleep(100),
        expensive_result
    end,
    Self = self(),
    Pids = [spawn(fun() ->
        Result = do(Key, ExpensiveFun),
        Self ! {done, self(), Result}
    end) || _ <- lists:seq(1, 10)],
    Results = [receive {done, Pid, R} -> R after 5000 -> timeout end || Pid <- Pids],
    [{count, ExecutionCount}] = ets:lookup(Counter, count),
    ets:delete(Counter),
    ?assertEqual(1, ExecutionCount),
    ?assert(lists:all(fun(R) -> R =:= expensive_result end, Results)).

error_propagation_test() ->
    start(),
    Key = {test_key_error, make_ref()},
    Self = self(),
    Pids = [spawn(fun() ->
        Result = try
            do(Key, fun() -> error(test_error) end)
        catch
            error:test_error -> caught_error
        end,
        Self ! {done, self(), Result}
    end) || _ <- lists:seq(1, 5)],
    Results = [receive {done, Pid, R} -> R after 5000 -> timeout end || Pid <- Pids],
    ?assert(lists:all(fun(R) -> R =:= caught_error end, Results)).

timeout_test() ->
    start(),
    Key = {test_key_timeout, make_ref()},
    SlowFun = fun() ->
        timer:sleep(500),
        slow_result
    end,
    Self = self(),
    spawn(fun() ->
        Result = do(Key, SlowFun, 1000),
        Self ! {owner_done, Result}
    end),
    timer:sleep(50),
    WaiterResult = do(Key, SlowFun, 100),
    OwnerResult = receive {owner_done, R} -> R after 2000 -> timeout end,
    ?assertEqual({error, timeout}, WaiterResult),
    ?assertEqual(slow_result, OwnerResult).

stale_owner_takeover_test() ->
    start(),
    Key = {test_key_stale, make_ref()},
    OldRef = make_ref(),
    ets:insert(?OWNERS_TABLE, {Key, self(), erlang:monotonic_time(millisecond) - 60000, OldRef}),
    Result = do(Key, fun() -> took_over end, 1000),
    ?assertEqual(took_over, Result).

dead_owner_takeover_test() ->
    start(),
    Key = {test_key_dead, make_ref()},
    DeadPid = spawn(fun() -> ok end),
    timer:sleep(10),
    OldRef = make_ref(),
    ets:insert(?OWNERS_TABLE, {Key, DeadPid, erlang:monotonic_time(millisecond), OldRef}),
    Result = do(Key, fun() -> took_over_from_dead end, 1000),
    ?assertEqual(took_over_from_dead, Result).

old_owner_does_not_affect_new_owner_test() ->
    start(),
    Key = {test_key_isolation, make_ref()},
    OldOwnershipRef = make_ref(),
    NewOwnershipRef = make_ref(),
    ets:insert(?OWNERS_TABLE, {Key, self(), erlang:monotonic_time(millisecond), NewOwnershipRef}),
    ets:insert(?WAITERS_TABLE, {Key, self(), make_ref(), NewOwnershipRef}),
    notify_and_cleanup(Key, OldOwnershipRef, {ok, old_result}),
    ?assertMatch([{Key, _, _, NewOwnershipRef}], ets:lookup(?OWNERS_TABLE, Key)),
    ?assertMatch([{Key, _, _, NewOwnershipRef}], ets:lookup(?WAITERS_TABLE, Key)).
