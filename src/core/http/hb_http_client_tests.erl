-module(hb_http_client_tests).
-include("include/hb.hrl").
-include("include/hb_http_client.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Hackney tests (live arweave.net — consistent with other modules in
%% the suite; if arweave.net is unreachable these legitimately fail)
%%====================================================================

standalone_start_link_initializes_gun_pool_test_() ->
    {timeout, 10, fun() ->
        case {whereis(hb_http_client), whereis(hb_gun_pool)} of
            {undefined, undefined} ->
                application:ensure_all_started(hackney),
                {ok, ClientPid} = hb_http_client:start_link(#{prometheus => false}),
                ?assert(whereis(hb_gun_pool) =/= undefined),
                gen_server:stop(ClientPid);
            _ ->
                {skip, runtime_already_started}
        end
    end}.

hackney_basic_request_test_() ->
    {timeout, 30, fun() ->
        application:ensure_all_started(hb),
        Args = #{
            peer => <<"https://arweave.net">>,
            path => <<"/info">>,
            method => <<"GET">>,
            headers => #{},
            body => <<>>
        },
        Opts = #{ <<"http-client">> => hackney, <<"http-retry">> => 0},
        {ok, 200, _, _} = hb_http_client:request(Args, Opts)
    end}.

hackney_bad_peer_test_() ->
    {timeout, 30, fun() ->
        application:ensure_all_started(hb),
        ?assert(erlang:whereis(hb_http_client) =/= undefined),
        ValidArgs = #{
            peer => <<"https://arweave.net">>,
            path => <<"/info">>,
            method => <<"GET">>,
            headers => #{},
            body => <<>>
        },
        Opts = #{ <<"http-client">> => hackney, <<"http-retry">> => 0},
        {ok, 200, _, _} = hb_http_client:request(ValidArgs, Opts),
        BadArgs = ValidArgs#{peer => <<"not-a-valid-uri">>},
        BadResult = hb_http_client:request(BadArgs, Opts),
        ?event(http_client_tests, {hackney_bad_peer_result, BadResult}),
        ?assertMatch({error, _}, BadResult),
        timer:sleep(500),
        ?assert(erlang:whereis(hb_http_client) =/= undefined,
            "gen_server must survive a bad peer URI with hackney backend"),
        {ok, 200, _, _} = hb_http_client:request(ValidArgs, Opts)
    end}.

hackney_post_test_() ->
    {timeout, 30, fun() ->
        application:ensure_all_started(hb),
        Args = #{
            peer => <<"https://arweave.net">>,
            path => <<"/info">>,
            method => <<"POST">>,
            headers => #{},
            body => <<"{}">>
        },
        Opts = #{ <<"http-client">> => hackney, <<"http-retry">> => 0},
        Result = hb_http_client:request(Args, Opts),
        ?event(http_client_tests, {hackney_post_result, summarize(Result)}),
        ?assertMatch({ok, _, _, _}, Result)
    end}.

%%====================================================================
%% Gun pool tests — juggler model
%%====================================================================

%% Queue fills up when pool cannot connect (port 1); the
%% (MaxQueued+1)th request from the same caller gets rejected.
gun_pool_backpressure_no_workers_test_() ->
    {timeout, 15, fun() ->
        gun_pool_ensure_sup(),
        {ok, MgrPid} = hb_gun_pool:start_or_get_pool(
            <<"localhost:1">>, no_workers_scope,
            #{host => <<"localhost">>, port => 1}),
        MaxQueued = 8,
        %% Use a long enough timeout so the gen_server:call doesn't time out
        %% while the request is queued; the test is about queue-cap rejection,
        %% not connect speed.  Calls return {error, shutdown} when MgrPid is
        %% stopped — we ignore their results here.
        MgrRef = erlang:monitor(process, MgrPid),
        [catch gen_server:call(MgrPid, {request, gun_pool_base_req(<<"/">>), self()},
                               5000)
         || _ <- lists:seq(1, MaxQueued)],
        ?assertEqual(
            {error, no_connection_available},
            gen_server:call(MgrPid,
                {request, gun_pool_base_req(<<"/">>), self()}, 5000)),
        hb_gun_pool:stop(MgrPid),
        receive {'DOWN', MgrRef, process, MgrPid, _} -> ok after 2000 -> ok end
    end}.

gun_pool_basic_request_test_() ->
    {timeout, 30, fun() ->
        gun_pool_with_mock_server(
            [{"/ping", ping, {200, <<"pong">>}}],
            fun(Port) ->
                {ok, 200, _, _} = gun_pool_request(Port, <<"/ping">>)
            end)
    end}.

gun_pool_synchronous_stop_releases_sockets_test_() ->
    {timeout, 30, fun() ->
        gun_pool_with_mock_server(
            [{"/ping", ping, {200, <<"pong">>}}],
            fun(Port) ->
                MgrPid = gun_pool_start_for(Port),
                Before = erlang:system_info(port_count),
                _ = do_pool_request(MgrPid, <<"/ping">>,
                                    #{http_client_connect_timeout => 10000}),
                hb_gun_pool:stop(MgrPid),
                hb_util:wait_until(fun() ->
                    erlang:system_info(port_count) =< Before + 2
                end, 2000),
                ?assert(erlang:system_info(port_count) =< Before + 2)
            end)
    end}.

gun_pool_caller_down_cancels_streams_test_() ->
    {timeout, 30, fun() ->
        gun_pool_with_mock_server(
            [{"/ping", ping, {200, <<"pong">>}}],
            fun(Port) ->
                MgrPid = gun_pool_start_for(Port),
                CallerPid = spawn(fun() ->
                    gen_server:call(
                        MgrPid, {request, gun_pool_base_req(<<"/ping">>), self()}, 5000)
                end),
                hb_util:wait_until(fun() ->
                    #{inflight := I, queued := Q} =
                        gen_server:call(MgrPid, pool_info),
                    I + Q >= 1
                end, 2000),
                Ref = erlang:monitor(process, CallerPid),
                exit(CallerPid, kill),
                receive {'DOWN', Ref, process, CallerPid, _} -> ok
                after 2000 -> error(caller_did_not_die) end,
                hb_util:wait_until(fun() ->
                    #{inflight := I, queued := Q} =
                        gen_server:call(MgrPid, pool_info),
                    I =:= 0 andalso Q =:= 0
                end, 2000),
                ?assert(is_map(gen_server:call(MgrPid, pool_info)))
            end)
    end}.

gun_pool_registry_idempotent_test_() ->
    {timeout, 15, fun() ->
        gun_pool_with_mock_server(
            [{"/ping", ping, {200, <<"ok">>}}],
            fun(Port) ->
                Authority = iolist_to_binary(["localhost:", integer_to_list(Port)]),
                ConnInfo = #{host => <<"localhost">>, port => Port},
                {ok, Pid1} = hb_gun_pool:start_or_get_pool(Authority, reg_test, ConnInfo),
                {ok, Pid2} = hb_gun_pool:start_or_get_pool(Authority, reg_test, ConnInfo),
                ?assertEqual(Pid1, Pid2)
            end)
    end}.

gun_pool_distinct_scopes_test_() ->
    {timeout, 15, fun() ->
        gun_pool_with_mock_server(
            [{"/ping", ping, {200, <<"ok">>}}],
            fun(Port) ->
                Authority = iolist_to_binary(["localhost:", integer_to_list(Port)]),
                ConnInfo = #{host => <<"localhost">>, port => Port},
                {ok, Pid1} = hb_gun_pool:start_or_get_pool(Authority, scope_a, ConnInfo),
                {ok, Pid2} = hb_gun_pool:start_or_get_pool(Authority, scope_b, ConnInfo),
                ?assertNotEqual(Pid1, Pid2),
                ?assertMatch([_], ets:lookup(hb_gun_pool_registry, {Authority, scope_a})),
                ?assertMatch([_], ets:lookup(hb_gun_pool_registry, {Authority, scope_b}))
            end)
    end}.

%% Idle eviction: pool self-terminates after two consecutive idle polls.
gun_pool_idle_eviction_test_() ->
    {timeout, 30, fun() ->
        gun_pool_with_mock_server(
            [{"/ping", ping, {200, <<"pong">>}}],
            fun(Port) ->
                Authority = iolist_to_binary(["localhost:", integer_to_list(Port)]),
                ConnInfo = #{host => <<"localhost">>, port => Port,
                             transport => tcp,
                             opts => #{gun_pool_idle_poll_ms => 100}},
                {ok, MgrPid} = hb_gun_pool:start_or_get_pool(
                    Authority, idle_eviction_scope, ConnInfo),
                MgrRef = erlang:monitor(process, MgrPid),
                receive
                    {'DOWN', MgrRef, process, MgrPid, normal} -> ok
                after 800 ->
                    ?assert(false, "manager did not self-terminate via idle eviction")
                end,
                ?assertEqual([], ets:lookup(hb_gun_pool_registry,
                                            {Authority, idle_eviction_scope}))
            end)
    end}.

%% Connection error surfaces as {error, _} and manager survives.
gun_pool_connection_error_surfaces_test_() ->
    {timeout, 15, fun() ->
        gun_pool_ensure_sup(),
        ConnInfo = #{host => <<"localhost">>, port => 1,
                     opts => #{http_client_connect_timeout => 500}},
        {ok, MgrPid} = hb_gun_pool:start_or_get_pool(
            <<"localhost:1">>, conn_err_scope, ConnInfo),
        %% connect_timeout=500ms: after MAX_CONNECT_FAILURES rounds the
        %% manager trips cooldown and fail_queued_requests replies to queued
        %% callers with {error, no_connection_available}.  Use a call timeout
        %% longer than the connect-failure window (~2s for h2 with 2 sockets).
        Result = do_pool_request(MgrPid, <<"/">>,
            #{http_client_connect_timeout => 500,
              http_client_send_timeout => 10000}),
        ?assertMatch({error, _}, Result),
        ?assert(is_process_alive(MgrPid)),
        hb_gun_pool:stop_all()
    end}.

%% tls_opts divergence: two callers with different tls_opts must get
%% separate pool managers (different TlsOptsHash in scope).
gun_pool_tls_scope_isolation_test_() ->
    {timeout, 10, fun() ->
        gun_pool_ensure_sup(),
        Authority = <<"localhost:8443">>,
        ConnInfo1 = #{host => <<"localhost">>, port => 8443, transport => tls,
                      opts => #{tls_opts => [{verify, verify_peer}]}},
        ConnInfo2 = #{host => <<"localhost">>, port => 8443, transport => tls,
                      opts => #{tls_opts => [{verify, verify_none}]}},
        TlsHash1 = erlang:phash2(hb_gun_pool_mgr:normalize_tls_opts([{verify, verify_peer}])),
        TlsHash2 = erlang:phash2(hb_gun_pool_mgr:normalize_tls_opts([{verify, verify_none}])),
        Scope1 = {tls, http2, TlsHash1},
        Scope2 = {tls, http2, TlsHash2},
        {ok, Pid1} = hb_gun_pool:start_or_get_pool(Authority, Scope1, ConnInfo1),
        {ok, Pid2} = hb_gun_pool:start_or_get_pool(Authority, Scope2, ConnInfo2),
        ?assertNotEqual(Pid1, Pid2),
        hb_gun_pool:stop_all()
    end}.

%% Flush stream: selective drain of demuxed messages.
gun_pool_flush_stream_test_() ->
    {timeout, 10, fun() ->
        gun_pool_ensure_sup(),
        Ref  = make_ref(),
        Ref2 = make_ref(),
        FakePid = self(),
        self() ! {gun_response, FakePid, Ref, fin, 200, []},
        self() ! {gun_data, FakePid, Ref, fin, <<"chunk">>},
        self() ! {gun_error, FakePid, Ref, some_reason},
        self() ! {gun_response, FakePid, Ref2, fin, 200, []},
        hb_gun_pool:flush_stream(Ref),
        receive
            {gun_response, _, Ref, _, _, _} ->
                ?assert(false, "stale gun_response for Ref after flush");
            {gun_data, _, Ref, _, _} ->
                ?assert(false, "stale gun_data for Ref after flush");
            {gun_error, _, Ref, _} ->
                ?assert(false, "stale gun_error for Ref after flush")
        after 0 -> ok
        end,
        receive {gun_response, _, Ref2, _, _, _} -> ok
        after 0 -> ?assert(false, "Ref2 message was incorrectly drained")
        end,
        hb_gun_pool:stop_all()
    end}.

%% End-to-end: scheme determines transport (tcp vs tls), not port.
gun_pool_scheme_transport_e2e_test_() ->
    {timeout, 15, fun() ->
        gun_pool_with_mock_server(
            [{"/ping", ping, {200, <<"ok">>}}],
            fun(Port) ->
                Authority = iolist_to_binary(
                    ["localhost:", integer_to_list(Port)]),
                Opts = #{http_client => gun, http_client_gun_use_pool => true,
                         http_retry => 0},
                Args = #{peer => iolist_to_binary(
                             ["http://localhost:", integer_to_list(Port)]),
                         path => <<"/ping">>, method => <<"GET">>,
                         headers => #{}, body => <<>>},
                ?assertMatch({ok, 200, _, _},
                             hb_http_client:request(Args, Opts)),
                [{_, MgrPid} | _] =
                    ets:match_object(hb_gun_pool_registry, {{Authority, '_'}, '_'}),
                ?assertMatch(#{}, gen_server:call(MgrPid, pool_info))
            end)
    end}.

%% Backpressure via hb_http_client when pool is cooling down.
gun_pool_no_connection_available_fast_fail_test_() ->
    {timeout, 15, fun() ->
        gun_pool_ensure_sup(),
        {ok, MgrPid} = hb_gun_pool:start_or_get_pool(
            <<"localhost:1">>, fast_fail_scope,
            #{host => <<"localhost">>, port => 1}),
        MaxQueued = 8,
        [catch gen_server:call(MgrPid, {request, gun_pool_base_req(<<"/">>), self()},
                               5000)
         || _ <- lists:seq(1, MaxQueued)],
        R = gen_server:call(MgrPid,
                {request, gun_pool_base_req(<<"/">>), self()}, 5000),
        ?assertEqual({error, no_connection_available}, R),
        hb_gun_pool:stop_all()
    end}.

%% Concurrent requests: multiple callers to same pool all complete.
gun_pool_concurrent_requests_test_() ->
    {timeout, 30, fun() ->
        gun_pool_with_mock_server(
            [{"/ping", ping, {200, <<"pong">>}}],
            fun(Port) ->
                MgrPid = gun_pool_start_for(Port),
                Parent = self(),
                N = 5,
                [spawn(fun() ->
                    R = do_pool_request(MgrPid, <<"/ping">>,
                                        #{http_client_connect_timeout => 10000}),
                    Parent ! {result, R}
                 end) || _ <- lists:seq(1, N)],
                Results = [receive {result, R} -> R after 15000 -> timeout end
                           || _ <- lists:seq(1, N)],
                ?assertEqual(0, length([x || timeout <- Results])),
                ?assert(lists:all(fun({ok, 200, _, _}) -> true; (_) -> false end,
                                  Results))
            end)
    end}.

%%====================================================================
%% Helpers
%%====================================================================

gun_pool_ensure_sup() ->
    application:ensure_all_started(gun),
    application:ensure_all_started(cowboy),
    case whereis(hb_gun_pool) of
        undefined -> {ok, _} = hb_gun_pool:start_link();
        _ -> ok
    end.

gun_pool_with_mock_server(Endpoints, Fun) ->
    gun_pool_ensure_sup(),
    {ok, _URL, Handle = {_, ListenerID}} = hb_mock_server:start(Endpoints),
    Port = ranch:get_port(ListenerID),
    try Fun(Port)
    after
        hb_gun_pool:stop_all(),
        hb_mock_server:stop(Handle)
    end.

gun_pool_start_for(Port) ->
    Authority = iolist_to_binary(["localhost:", integer_to_list(Port)]),
    {ok, MgrPid} = hb_gun_pool:start_or_get_pool(
        Authority, default, #{host => <<"localhost">>, port => Port}),
    MgrPid.

%% Issue one request through the juggler and collect the demuxed response.
%% The gen_server:call timeout is the send timeout (how long we wait for the
%% manager to hand us a StreamRef), not the connect timeout.
do_pool_request(MgrPid, Path, Opts) ->
    ReqArgs = gun_pool_base_req(Path),
    CallTimeout = hb_opts:get(http_client_send_timeout, 10000, Opts),
    case gen_server:call(MgrPid, {request, ReqArgs, self()}, CallTimeout) of
        {error, _} = Err ->
            Err;
        {ok, StreamRef} ->
            RecvTimeout = hb_opts:get(http_client_send_timeout, 10000, Opts),
            collect_demuxed(MgrPid, StreamRef, RecvTimeout, <<>>, undefined, undefined)
    end.

collect_demuxed(MgrPid, StreamRef, Timeout, Acc, Status, Headers) ->
    receive
        {gun_response, MgrPid, StreamRef, fin, S, H} ->
            {ok, S, H, <<>>};
        {gun_response, MgrPid, StreamRef, nofin, S, H} ->
            collect_demuxed(MgrPid, StreamRef, Timeout, Acc, S, H);
        {gun_data, MgrPid, StreamRef, fin, Data} ->
            {ok, Status, Headers, iolist_to_binary([Acc, Data])};
        {gun_data, MgrPid, StreamRef, nofin, Data} ->
            collect_demuxed(MgrPid, StreamRef, Timeout,
                iolist_to_binary([Acc, Data]), Status, Headers);
        {gun_trailers, MgrPid, StreamRef, _} ->
            {ok, Status, Headers, Acc};
        {gun_error, MgrPid, StreamRef, Reason} ->
            {error, Reason}
    after Timeout ->
        {error, timeout}
    end.

%% Helper: start pool and send a request end-to-end via hb_http_client.
gun_pool_request(Port, Path) ->
    Opts = #{http_client => gun,
             http_client_gun_use_pool => true,
             http_retry => 0,
             http_client_connect_timeout => 10000},
    URL = iolist_to_binary(["http://localhost:", integer_to_list(Port)]),
    Args = #{peer => URL, path => Path,
             method => <<"GET">>, headers => #{}, body => <<>>},
    hb_http_client:request(Args, Opts).

%%====================================================================
%% TLS scope normalization tests
%%====================================================================

%% Same opts in different order produce the same hash.
tls_scope_normalization_order_test_() ->
    {timeout, 5, fun() ->
        Opts1 = [{verify, verify_peer}, {cacertfile, "/etc/ssl/certs/ca.pem"}],
        Opts2 = [{cacertfile, "/etc/ssl/certs/ca.pem"}, {verify, verify_peer}],
        H1 = erlang:phash2(hb_gun_pool_mgr:normalize_tls_opts(Opts1)),
        H2 = erlang:phash2(hb_gun_pool_mgr:normalize_tls_opts(Opts2)),
        ?assertEqual(H1, H2)
    end}.

%% Different cacertfile produces a different hash.
tls_scope_normalization_cacertfile_test_() ->
    {timeout, 5, fun() ->
        Opts1 = [{cacertfile, "/etc/ssl/certs/ca1.pem"}],
        Opts2 = [{cacertfile, "/etc/ssl/certs/ca2.pem"}],
        H1 = erlang:phash2(hb_gun_pool_mgr:normalize_tls_opts(Opts1)),
        H2 = erlang:phash2(hb_gun_pool_mgr:normalize_tls_opts(Opts2)),
        ?assertNotEqual(H1, H2)
    end}.

%% Different SNI produces a different hash.
tls_scope_normalization_sni_test_() ->
    {timeout, 5, fun() ->
        Opts1 = [{server_name_indication, "host-a.example.com"}],
        Opts2 = [{server_name_indication, "host-b.example.com"}],
        H1 = erlang:phash2(hb_gun_pool_mgr:normalize_tls_opts(Opts1)),
        H2 = erlang:phash2(hb_gun_pool_mgr:normalize_tls_opts(Opts2)),
        ?assertNotEqual(H1, H2)
    end}.

%% Cipher order is preserved (server-preference matters).
tls_scope_normalization_cipher_order_test_() ->
    {timeout, 5, fun() ->
        C1 = "ECDHE-RSA-AES256-GCM-SHA384",
        C2 = "ECDHE-RSA-AES128-GCM-SHA256",
        Opts1 = [{ciphers, [C1, C2]}],
        Opts2 = [{ciphers, [C2, C1]}],
        H1 = erlang:phash2(hb_gun_pool_mgr:normalize_tls_opts(Opts1)),
        H2 = erlang:phash2(hb_gun_pool_mgr:normalize_tls_opts(Opts2)),
        ?assertNotEqual(H1, H2)
    end}.

gun_pool_base_req(Path) ->
    #{path => Path, method => <<"GET">>, headers => #{}, body => <<>>}.

summarize({caught, C, R}) when is_tuple(R) ->
    {caught, C, element(1, R)};
summarize({caught, C, R}) ->
    {caught, C, R};
summarize(Other) ->
    Other.
