-module(hb_http_client_tests).
-include("include/hb.hrl").
-include("include/hb_http_client.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc A peer's pool has the limits of the request that starts it, and keeps
%% them when its supervisor restarts it.
hackney_pool_restart_test_() ->
    {timeout, 30, fun() ->
        application:ensure_all_started(hb),
        Peer = hb_http_server:start_node(#{ <<"test-key">> => <<"pool-settings">> }),
        Server = hb_opts:get(http_server, undefined, hb_http_server:get_opts()),
        #{ port := Port } = uri_string:parse(Peer),
        Pool = {?HACKNEY_POOL, <<"http">>, "localhost", Port},
        try
            lists:foreach(
                fun({Max, Timeout}) ->
                    ok = hackney_pool:stop_pool(Pool),
                    pool_request(Peer, #{
                        <<"http-client-hackney-max-connections">> => Max,
                        <<"http-client-keepalive">> => Timeout
                    }),
                    ?assertEqual(Max, hackney_pool:max_connections(Pool)),
                    ?assertEqual(Timeout, hackney_pool:timeout(Pool)),
                    Old = hackney_pool:find_pool(Pool),
                    Ref = monitor(process, Old),
                    exit(Old, kill),
                    receive {'DOWN', Ref, process, Old, killed} -> ok
                    after 1000 -> error(pool_not_stopped)
                    end,
                    await_pool_restart(Pool, Old, 100),
                    ?assertEqual(Max, hackney_pool:max_connections(Pool)),
                    ?assertEqual(Timeout, hackney_pool:timeout(Pool)),
                    pool_request(Peer, #{}),
                    {ok, #{ start := {hackney_pool, start_link, [_, PoolOpts]} }} =
                        supervisor:get_childspec(hackney_sup, Pool),
                    ?assertEqual(Max, proplists:get_value(max_connections, PoolOpts)),
                    ?assertEqual(Timeout, proplists:get_value(timeout, PoolOpts)),
                    ?assertEqual(0, proplists:get_value(queue_count,
                        hackney_pool:get_stats(Pool)))
                end,
                [{600, 120000}, {37, 90000}, {600, 120000}, {100, 60000}]
            )
        after
            cowboy:stop_listener(Server)
        end
    end}.

%% @doc A peer whose requests take every connection of its pool does not stop
%% the requests to another peer.
hackney_pool_per_peer_test_() ->
    {timeout, 30, fun() ->
        application:ensure_all_started(hb),
        % Each request to this peer waits 2 seconds for its reply.
        Slow =
            hb_http_server:start_node(#{
                <<"priv-wallet">> => ar_wallet:new(),
                <<"on">> =>
                    #{
                        <<"request">> =>
                            #{
                                <<"device">> =>
                                    #{
                                        <<"request">> =>
                                            fun(_, HookReq, _) ->
                                                timer:sleep(2000),
                                                {ok, HookReq}
                                            end
                                    }
                            }
                    }
            }),
        Peer = hb_http_server:start_node(#{ <<"test-key">> => <<"pool-settings">> }),
        Test = self(),
        SlowRequests =
            [
                spawn(
                    fun() ->
                        Test !
                            {slow,
                                hb_http_client:request(
                                    #{
                                        peer => Slow,
                                        path => <<"/~meta@1.0/info">>,
                                        method => <<"GET">>,
                                        headers => #{},
                                        body => <<>>
                                    },
                                    #{ <<"http-client">> => hackney, <<"http-retry">> => 0 }
                                )
                            }
                    end
                )
            ||
                _ <- lists:seq(1, ?DEFAULT_HACKNEY_MAX_CONNECTIONS)
            ],
        timer:sleep(500),
        pool_request(Peer, #{ <<"http-client-hackney-checkout-timeout">> => 1000 }),
        [receive {slow, _} -> ok end || _ <- SlowRequests]
    end}.

%% @doc Read through the ordinary client and an independent HyperBEAM listener.
pool_request(Peer, Opts) ->
    ?assertMatch(
        {ok, 200, _, <<"pool-settings">>},
        hb_http_client:request(#{
            peer => Peer, path => <<"/~meta@1.0/info/test-key">>, method => <<"GET">>,
            headers => #{}, body => <<>>
        }, Opts#{ <<"http-client">> => hackney, <<"http-retry">> => 0 })
    ).

%% @doc Wait for a new registered pool, not the dead ETS entry.
await_pool_restart(_Pool, _Old, 0) -> error(pool_not_restarted);
await_pool_restart(Pool, Old, Attempts) ->
    case hackney_pool:find_pool(Pool) of
        New when is_pid(New), New =/= Old -> ok;
        _ ->
            timer:sleep(10),
            await_pool_restart(Pool, Old, Attempts - 1)
    end.

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

%% @doc A reply with a malformed chunk size is an error, and its connection is
%% closed rather than kept checked out of the pool.
hackney_bad_chunk_test() ->
    application:ensure_all_started(hb),
    {ok, Listener} =
        gen_tcp:listen(0, [binary, {active, false}, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(Listener),
    Test = self(),
    spawn_link(
        fun() ->
            {ok, Socket} = gen_tcp:accept(Listener),
            {ok, _} = gen_tcp:recv(Socket, 0),
            ok =
                gen_tcp:send(
                    Socket,
                    <<"HTTP/1.1 200 OK\r\ntransfer-encoding: chunked\r\n\r\nzz\r\n">>
                ),
            Test ! {peer, gen_tcp:recv(Socket, 0, 5000)}
        end
    ),
    ?assertMatch(
        {error, _},
        hb_http_client:request(
            #{
                peer => <<"http://127.0.0.1:", (hb_util:bin(Port))/binary>>,
                path => <<"/">>,
                method => <<"GET">>,
                headers => #{},
                body => <<>>
            },
            #{ <<"http-client">> => hackney, <<"http-retry">> => 0 }
        )
    ),
    receive {peer, Read} -> ?assertEqual({error, closed}, Read) end.

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

gun_final_frame_limit_test() ->
    {ok, Peer, Server} = hb_mock_server:start([
        {"/body", body, {200, <<"four">>}}
    ]),
    try
        ?assertEqual(
            {error, too_much_data},
            hb_http_client:request(#{
                peer => Peer, path => <<"/body">>, method => <<"GET">>,
                headers => #{}, body => <<>>, limit => 3
            }, #{ <<"http-client">> => gun, <<"protocol">> => http1 })
        )
    after
        hb_mock_server:stop(Server)
    end.

summarize({caught, C, R}) when is_tuple(R) ->
    {caught, C, element(1, R)};
summarize({caught, C, R}) ->
    {caught, C, R};
summarize(Other) ->
    Other.
