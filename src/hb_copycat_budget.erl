%%% @doc Atomics-based byte budget pool for copycat memory throttling.
%%% Controls how many bytes of TX data can be held in memory simultaneously
%%% across all copycat workers. Uses persistent_term for constant-time access.
-module(hb_copycat_budget).
-export([ensure_started/1, reset/1, lease/1, release/1, get_budget/0, stats/0]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(PERSISTENT_KEY, hb_copycat_budget).
-define(IDX_LEASED, 1).
-define(IDX_PEAK, 2).
-define(IDX_BUDGET, 3).
-define(IDX_RETRIES, 4).
-define(RETRY_SLEEP_MS, 50).
-define(LEASE_LOOP_MAX_RETRIES, 100).

-define(INIT_LOCK, hb_copycat_budget_init).

ensure_started(Budget) when is_integer(Budget), Budget > 0 ->
    case persistent_term:get(?PERSISTENT_KEY, undefined) of
        undefined ->
            init_with_lock(Budget);
        _Ref ->
            ok
    end.

init_with_lock(Budget) ->
    try register(?INIT_LOCK, self()) of
        true ->
            try
                case persistent_term:get(?PERSISTENT_KEY, undefined) of
                    undefined ->
                        Ref = atomics:new(4, [{signed, false}]),
                        atomics:put(Ref, ?IDX_BUDGET, Budget),
                        persistent_term:put(?PERSISTENT_KEY, Ref);
                    _AlreadySet ->
                        ok
                end
            after
                unregister(?INIT_LOCK)
            end,
            ok
    catch
        error:badarg ->
            await_init(Budget)
    end.

await_init(Budget) ->
    case persistent_term:get(?PERSISTENT_KEY, undefined) of
        undefined ->
            case whereis(?INIT_LOCK) of
                undefined ->
                    init_with_lock(Budget);
                _Pid ->
                    timer:sleep(1),
                    await_init(Budget)
            end;
        _Ref ->
            ok
    end.

reset(Budget) when is_integer(Budget), Budget > 0 ->
    Ref = atomics:new(4, [{signed, false}]),
    atomics:put(Ref, ?IDX_BUDGET, Budget),
    persistent_term:put(?PERSISTENT_KEY, Ref),
    ok.

lease(Size) when is_integer(Size), Size > 0 ->
    Ref = persistent_term:get(?PERSISTENT_KEY),
    lease_loop(Ref, Size, 0).

lease_loop(Ref, Size, ?LEASE_LOOP_MAX_RETRIES) -> 
    ?event(error, 
        {lease_loop_max_retries_exhausted, 
            {ref, Ref},
            {size, Size},
            {max_retries, ?LEASE_LOOP_MAX_RETRIES}}),
    throw(exhausted_lease_loop_max_retires);
lease_loop(Ref, Size, Retries) ->
    Current = atomics:get(Ref, ?IDX_LEASED),
    Budget = atomics:get(Ref, ?IDX_BUDGET),
    case Current + Size > Budget of
        true ->
            atomics:add(Ref, ?IDX_RETRIES, 1),
            timer:sleep(?RETRY_SLEEP_MS),
            lease_loop(Ref, Size, Retries + 1);
        false ->
            case atomics:compare_exchange(Ref, ?IDX_LEASED, Current, Current + Size) of
                ok ->
                    update_peak(Ref, Current + Size),
                    ok;
                _Changed ->
                    lease_loop(Ref, Size, Retries + 1)
            end
    end.

release(Size) when is_integer(Size), Size > 0 ->
    Ref = persistent_term:get(?PERSISTENT_KEY),
    atomics:sub(Ref, ?IDX_LEASED, Size),
    ok.

get_budget() ->
    case persistent_term:get(?PERSISTENT_KEY, undefined) of
        undefined -> undefined;
        Ref -> atomics:get(Ref, ?IDX_BUDGET)
    end.

stats() ->
    case persistent_term:get(?PERSISTENT_KEY, undefined) of
        undefined ->
            not_started;
        Ref ->
            #{
                leased => atomics:get(Ref, ?IDX_LEASED),
                peak => atomics:get(Ref, ?IDX_PEAK),
                budget => atomics:get(Ref, ?IDX_BUDGET),
                retries => atomics:get(Ref, ?IDX_RETRIES)
            }
    end.

update_peak(Ref, NewLeased) ->
    Peak = atomics:get(Ref, ?IDX_PEAK),
    case NewLeased =< Peak of
        true -> ok;
        false ->
            case atomics:compare_exchange(Ref, ?IDX_PEAK, Peak, NewLeased) of
                ok -> ok;
                _Changed -> update_peak(Ref, NewLeased)
            end
    end.

%%% Tests

lease_release_cycle_test() ->
    reset(1000),
    ?assertEqual(1000, get_budget()),
    ok = lease(400),
    #{leased := 400, peak := 400, budget := 1000} = stats(),
    ok = lease(300),
    #{leased := 700, peak := 700} = stats(),
    ok = release(400),
    #{leased := 300, peak := 700} = stats(),
    ok = release(300),
    #{leased := 0, peak := 700} = stats(),
    reset_to_default(),
    ok.

blocks_when_over_budget_test() ->
    reset(100),
    ok = lease(100),
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn(fun() ->
        Parent ! {Ref, trying},
        ok = lease(50),
        Parent ! {Ref, got_lease}
    end),
    receive {Ref, trying} -> ok end,
    timer:sleep(120),
    receive
        {Ref, got_lease} -> error(should_have_blocked)
    after 0 -> ok
    end,
    release(60),
    receive
        {Ref, got_lease} -> ok
    after 500 ->
        exit(Pid, kill),
        error(lease_never_granted)
    end,
    release(50),
    #{leased := 40} = stats(),
    release(40),
    reset_to_default(),
    ok.

concurrent_leases_test() ->
    Budget = 1000,
    reset(Budget),
    Parent = self(),
    NumWorkers = 20,
    LeaseSize = 200,
    Pids = [spawn(fun() ->
        ok = lease(LeaseSize),
        timer:sleep(10),
        release(LeaseSize),
        Parent ! {done, self()}
    end) || _ <- lists:seq(1, NumWorkers)],
    lists:foreach(fun(Pid) ->
        receive {done, Pid} -> ok
        after 5000 -> error({timeout, Pid})
        end
    end, Pids),
    #{leased := 0, peak := Peak, budget := Budget} = stats(),
    ?assert(Peak =< Budget),
    ?assert(Peak > 0),
    reset_to_default(),
    ok.

reset_to_default() ->
    reset(hb_opts:get(<<"copycat-memory-budget">>, 6 * 1024 * 1024 * 1024, #{})).
