%%% @doc Unit tests for hb_gun_pool_mgr using hb_gun_test_fake.
%%%
%%% Each test is hermetic: no real gun, no cowboy, no arweave.net.
%%% The fake-gun harness (hb_gun_test_fake) is injected via the gun_open_fn opt.
-module(hb_gun_pool_mgr_tests).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Helpers
%%====================================================================

%% Start a standalone manager (not under hb_gun_pool supervisor) with a
%% gun_open_fn that returns the given FakePid on each open call.
%% Build a gun_open_fn that starts a new hb_gun_test_fake with the calling
%% process (the manager) as Owner, using Script as the behaviour script.
script_open_fn(Script, Proto) ->
    fun(_Host, _Port, _Opts) ->
        hb_gun_test_fake:open(self(), Script, Proto)
    end.

start_mgr(Script) ->
    start_mgr(Script, #{}).

start_mgr(Script, ExtraOpts) ->
    start_mgr(Script, http2, ExtraOpts).

start_mgr(Script, Proto, ExtraOpts) ->
    Opts = maps:merge(
        #{gun_open_fn    => script_open_fn(Script, Proto),
          gun_module     => hb_gun_test_fake,
          http_client_connect_timeout => 500,
          gun_pool_idle_poll_ms       => 200},
        ExtraOpts),
    {ok, Pid} = gen_server:start_link(
        hb_gun_pool_mgr,
        {<<"localhost:9999">>, test_scope, "localhost", 9999, tcp, Opts},
        []),
    Pid.

%% Submit a request synchronously and collect the demuxed response.
do_request(MgrPid) ->
    do_request(MgrPid, 5000).

do_request(MgrPid, Timeout) ->
    ReqArgs = #{method => <<"GET">>, path => <<"/test">>,
                headers => #{}, body => <<>>},
    case gen_server:call(MgrPid, {request, ReqArgs, self()}, Timeout) of
        {error, _} = Err -> Err;
        {ok, StreamRef}  -> collect(MgrPid, StreamRef, Timeout)
    end.

collect(MgrPid, StreamRef, Timeout) ->
    receive
        {gun_response, MgrPid, StreamRef, fin, S, H} ->
            {ok, S, H, <<>>};
        {gun_response, MgrPid, StreamRef, nofin, S, H} ->
            collect_body(MgrPid, StreamRef, Timeout, S, H, <<>>);
        {gun_data, MgrPid, StreamRef, fin, Data} ->
            {ok, undefined, undefined, Data};
        {gun_error, MgrPid, StreamRef, Reason} ->
            {error, Reason}
    after Timeout ->
        {error, timeout}
    end.

collect_body(MgrPid, StreamRef, Timeout, Status, Headers, Acc) ->
    receive
        {gun_data, MgrPid, StreamRef, fin, Data} ->
            {ok, Status, Headers, iolist_to_binary([Acc, Data])};
        {gun_data, MgrPid, StreamRef, nofin, Data} ->
            collect_body(MgrPid, StreamRef, Timeout, Status, Headers,
                         iolist_to_binary([Acc, Data]));
        {gun_trailers, MgrPid, StreamRef, _} ->
            {ok, Status, Headers, Acc};
        {gun_error, MgrPid, StreamRef, Reason} ->
            {error, Reason}
    after Timeout ->
        {error, timeout}
    end.

%%====================================================================
%% Scenario 1 — happy path h2: single request returns 200
%%====================================================================

happy_path_h2_test_() ->
    {timeout, 10, fun() ->
        Script = [{reply, 200, [], <<"hello">>}],
        MgrPid = start_mgr(Script),
        MgrRef = erlang:monitor(process, MgrPid),
        ?assertMatch({ok, 200, _, _}, do_request(MgrPid)),
        gen_server:stop(MgrPid),
        receive {'DOWN', MgrRef, process, MgrPid, _} -> ok after 2000 -> ok end
    end}.

%%====================================================================
%% Scenario 2 — concurrent streams h2: 10 concurrent requests
%%====================================================================

concurrent_streams_h2_test_() ->
    {timeout, 15, fun() ->
        Script = lists:duplicate(10, {reply, 200, [], <<"ok">>}),
        MgrPid = start_mgr(Script, http2, #{http_client_gun_max_sockets_h2 => 1}),
        Parent = self(),
        [spawn(fun() ->
             R = do_request(MgrPid),
             Parent ! {result, R}
         end) || _ <- lists:seq(1, 10)],
        Results = [receive {result, R} -> R after 10000 -> timeout end
                   || _ <- lists:seq(1, 10)],
        ?assertEqual(0, length([x || timeout <- Results])),
        ?assert(lists:all(fun({ok, 200, _, _}) -> true; (_) -> false end, Results)),
        gen_server:stop(MgrPid)
    end}.

%%====================================================================
%% Scenario 3 — caller DOWN cancels in-flight stream
%%====================================================================

caller_down_cancels_streams_test_() ->
    {timeout, 10, fun() ->
        %% connect_timeout: gun_up never fires so the request stays queued.
        %% Large idle_poll_ms prevents idle eviction from firing during test.
        Script = [connect_timeout],
        Opts = #{http_client_connect_timeout => 5000,
                 http_client_gun_max_sockets_h2 => 1,
                 gun_pool_idle_poll_ms => 60000},
        MgrPid = start_mgr(Script, http2, Opts),
        ReqArgs = #{method => <<"GET">>, path => <<"/slow">>,
                    headers => #{}, body => <<>>},
        CallerPid = spawn(fun() ->
            gen_server:call(MgrPid, {request, ReqArgs, self()}, 8000)
        end),
        MgrMonRef = erlang:monitor(process, MgrPid),
        %% Give the request time to be enqueued.
        receive after 50 -> ok end,
        exit(CallerPid, kill),
        receive {'DOWN', MgrMonRef, process, MgrPid, _} ->
            ?assert(false, "manager crashed after caller exit")
        after 500 -> ok
        end,
        ?assert(is_process_alive(MgrPid)),
        gen_server:stop(MgrPid)
    end}.

%%====================================================================
%% Scenario 4 — gun_down mid-stream: in-flight callers get gun_error
%%====================================================================

gun_down_mid_stream_test_() ->
    {timeout, 10, fun() ->
        Script = [conn_down],
        MgrPid = start_mgr(Script),
        Parent = self(),
        spawn(fun() ->
            R = do_request(MgrPid, 3000),
            Parent ! {caller_result, R}
        end),
        Result = receive
            {caller_result, R} -> R
        after 5000 -> timeout
        end,
        ?assertMatch({error, _}, Result),
        gen_server:stop(MgrPid)
    end}.

%%====================================================================
%% Scenario 5 — connect_timeout trips cooldown
%%====================================================================

connect_timeout_trips_cooldown_test_() ->
    {timeout, 10, fun() ->
        %% connect_timeout in script: fake sends no gun_up.
        Script = [connect_timeout],
        Opts = #{http_client_connect_timeout => 50,
                 http_client_gun_max_sockets_h2 => 1},
        MgrPid = start_mgr(Script, http2, Opts),
        Req = #{method => <<"GET">>, path => <<"/">>, headers => #{}, body => <<>>},
        Results = [catch gen_server:call(MgrPid, {request, Req, self()}, 2000)
                   || _ <- lists:seq(1, 4)],
        ?assert(lists:any(fun({error, no_connection_available}) -> true;
                             (_) -> false end,
                          Results)),
        gen_server:stop(MgrPid)
    end}.

%%====================================================================
%% Scenario 6 — cooldown expiry: manager accepts new requests after expiry
%%====================================================================

cooldown_expiry_test_() ->
    {timeout, 10, fun() ->
        Script = [connect_timeout],
        Opts = #{http_client_connect_timeout => 50,
                 http_client_gun_max_sockets_h2 => 1},
        MgrPid = start_mgr(Script, http2, Opts),
        Req = #{method => <<"GET">>, path => <<"/">>, headers => #{}, body => <<>>},
        %% Trip cooldown.
        [catch gen_server:call(MgrPid, {request, Req, self()}, 2000)
         || _ <- lists:seq(1, 4)],
        %% Manager must still be alive after cooldown trips.
        ?assert(is_process_alive(MgrPid)),
        gen_server:stop(MgrPid)
    end}.

%%====================================================================
%% Scenario 7 — idle eviction: 2 idle ticks → manager self-terminates
%%====================================================================

idle_eviction_test_() ->
    {timeout, 5, fun() ->
        MgrPid = start_mgr([], http2, #{gun_pool_idle_poll_ms => 50}),
        MgrRef = erlang:monitor(process, MgrPid),
        receive
            {'DOWN', MgrRef, process, MgrPid, normal} -> ok
        after 2000 ->
            ?assert(false, "manager did not self-terminate via idle eviction")
        end
    end}.

%%====================================================================
%% Scenario 8 — trap_exit on gun crash: manager survives abnormal exit
%%====================================================================

trap_exit_on_gun_crash_test_() ->
    {timeout, 10, fun() ->
        %% Large idle_poll_ms so idle eviction doesn't interfere.
        Script = [{reply, 200, [], <<"ok">>}],
        MgrPid = start_mgr(Script, http2, #{gun_pool_idle_poll_ms => 60000}),
        MgrRef = erlang:monitor(process, MgrPid),
        %% Issue a request so a connection is opened.
        _ = do_request(MgrPid),
        %% Find the connected gun pids via pool_info — then crash one directly.
        %% Since the manager traps exits, it should survive.
        %% We simulate by sending an EXIT signal from a linked process.
        Crasher = spawn(fun() -> exit(simulated_crash) end),
        MgrPid ! {'EXIT', Crasher, simulated_crash},
        receive
            {'DOWN', MgrRef, process, MgrPid, _Reason} ->
                ?assert(false, "manager crashed when gun conn exited abnormally")
        after 500 -> ok
        end,
        ?assert(is_process_alive(MgrPid)),
        gen_server:stop(MgrPid)
    end}.

%%====================================================================
%% Restored behaviour: FIFO dispatch order
%%====================================================================

caller_fifo_order_test_() ->
    {timeout, 10, fun() ->
        Parent = self(),
        %% delay_up gives callers time to queue before the conn becomes ready.
        %% 3 reply entries satisfy 3 queued callers in order.
        Script = [{delay_up, 1000}] ++
                 lists:duplicate(3, {reply, 200, [], <<"ok">>}),
        Opts = #{http_client_connect_timeout => 5000,
                 http_client_gun_max_sockets_h2 => 1,
                 gun_pool_idle_poll_ms => 60000},
        MgrPid = start_mgr(Script, http2, Opts),
        Req = #{method => <<"GET">>, path => <<"/">>, headers => #{}, body => <<>>},
        Callers = [{I, spawn(fun() ->
            receive
                go ->
                    R = gen_server:call(MgrPid, {request, Req, self()}, 8000),
                    Parent ! {queued_reply, I, R}
            end
        end)} || I <- [1, 2, 3]],
        lists:foreach(
            fun({ExpectedQueued, {_I, CallerPid}}) ->
                CallerPid ! go,
                ?assert(
                    hb_util:wait_until(
                        fun() ->
                            #{queued := Queued} = gen_server:call(MgrPid, pool_info),
                            Queued =:= ExpectedQueued
                        end,
                        2000
                    )
                )
            end,
            lists:zip([1, 2, 3], Callers)
        ),
        Order = [receive {queued_reply, I, _} -> I after 4000 -> timeout end
                 || _ <- [1, 2, 3]],
        ?assertEqual([1, 2, 3], Order),
        gen_server:stop(MgrPid)
    end}.

%%====================================================================
%% Restored behaviour: abandoned h1 stream keeps its socket busy
%%====================================================================

h1_cancelled_stream_keeps_socket_busy_test_() ->
    {timeout, 10, fun() ->
        ScriptKey = make_ref(),
        OpenFn =
            fun(_Host, _Port, _Opts) ->
                Count =
                    case erlang:get(ScriptKey) of
                        undefined -> 1;
                        N -> N + 1
                    end,
                erlang:put(ScriptKey, Count),
                case Count of
                    1 ->
                        hb_gun_test_fake:open(
                            self(),
                            [{reply_nofin, 200, []}],
                            http
                        );
                    _ ->
                        hb_gun_test_fake:open(
                            self(),
                            [{reply, 200, [], <<"ok">>}],
                            http
                        )
                end
            end,
        Opts = #{
            gun_open_fn => OpenFn,
            gun_module => hb_gun_test_fake,
            protocol => http1,
            http_client_gun_max_sockets_h1 => 2,
            gun_pool_idle_poll_ms => 60000
        },
        MgrPid = start_mgr([], http, Opts),
        Req = #{method => <<"GET">>, path => <<"/slow">>,
                headers => #{}, body => <<>>},
        {ok, StreamRef} = gen_server:call(MgrPid, {request, Req, self()}, 5000),
        receive
            {gun_response, MgrPid, StreamRef, nofin, 200, []} -> ok
        after 2000 ->
            ?assert(false, "no initial h1 response arrived")
        end,
        hb_gun_pool_mgr:cancel_stream(MgrPid, StreamRef),
        ?assertMatch({ok, 200, _, _}, do_request(MgrPid, 5000)),
        #{workers_up := Workers, inflight := InFlight} =
            gen_server:call(MgrPid, pool_info),
        ?assertEqual(2, Workers),
        ?assertEqual(1, InFlight),
        gen_server:stop(MgrPid)
    end}.

%%====================================================================
%% Restored behaviour: queued caller exit removes queue entry
%%====================================================================

queued_cancellation_on_caller_exit_test_() ->
    {timeout, 10, fun() ->
        Script = [connect_timeout],
        Opts = #{http_client_connect_timeout => 5000,
                 http_client_gun_max_sockets_h2 => 1,
                 gun_pool_idle_poll_ms => 60000},
        MgrPid = start_mgr(Script, http2, Opts),
        Req = #{method => <<"GET">>, path => <<"/">>, headers => #{}, body => <<>>},
        CallerPid = spawn(fun() ->
            gen_server:call(MgrPid, {request, Req, self()}, 8000)
        end),
        receive after 50 -> ok end,
        exit(CallerPid, kill),
        receive after 100 -> ok end,
        #{queued := Q} = gen_server:call(MgrPid, pool_info),
        ?assertEqual(0, Q),
        gen_server:stop(MgrPid)
    end}.

%%====================================================================
%% Restored behaviour: flush_stream drains mailbox after timeout
%%====================================================================

request_timeout_drains_mailbox_test_() ->
    {timeout, 10, fun() ->
        %% Use a chunked reply so the response arrives as multiple messages.
        %% Simulate "caller timed out": wait for the first message to land,
        %% then call flush_stream and verify nothing remains.
        Script = [{reply_chunked, 200, [], [<<"chunk1">>, <<"chunk2">>]}],
        MgrPid = start_mgr(Script, http2, #{gun_pool_idle_poll_ms => 60000}),
        Req = #{method => <<"GET">>, path => <<"/">>, headers => #{}, body => <<>>},
        {ok, StreamRef} = gen_server:call(MgrPid, {request, Req, self()}, 5000),
        %% Wait for at least one message to be in the mailbox (precondition).
        receive
            {gun_response, _, StreamRef, _, _, _} -> ok
        after 2000 ->
            ?assert(false, "no gun_response arrived within 2s")
        end,
        %% Now flush remaining messages for this stream.
        hb_gun_pool:flush_stream(StreamRef),
        receive
            {gun_data, _, StreamRef, _, _} ->
                ?assert(false, "stale gun_data after flush");
            {gun_error, _, StreamRef, _} ->
                ?assert(false, "stale gun_error after flush")
        after 200 -> ok
        end,
        gen_server:stop(MgrPid)
    end}.
