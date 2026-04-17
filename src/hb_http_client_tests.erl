-module(hb_http_client_tests).
-include("include/hb.hrl").
-include("include/hb_http_client.hrl").
-include_lib("eunit/include/eunit.hrl").

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
        Opts = #{http_client => hackney, http_retry => 0},
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
        Opts = #{http_client => hackney, http_retry => 0},
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
        Opts = #{http_client => hackney, http_retry => 0},
        Result = hb_http_client:request(Args, Opts),
        ?event(http_client_tests, {hackney_post_result, summarize(Result)}),
        ?assertMatch({ok, _, _, _}, Result)
    end}.

gun_pool_request_test_() ->
    {timeout, 60, fun() ->
        application:ensure_all_started(hb),
        {ok, Peer, Handle} = hb_mock_server:start(
            [{"/info", info, {200, <<"{\"ok\":true}">>}}]),
        Opts = pool_opts(#{http_client_gun_pool_size => 1}),
        GetArgs = basic_args(Peer),
        try
            [?assertMatch({ok, 200, _, _}, hb_http_client:request(GetArgs, Opts))
             || _ <- lists:seq(1, 3)],
            ?assertMatch({ok, _, _, _}, hb_http_client:request(
                GetArgs#{method => <<"POST">>, body => <<"{}">>}, Opts)),
            ?assertMatch({error, _}, hb_http_client:request(
                GetArgs#{peer => <<"not-a-valid-uri">>}, Opts))
        after
            hb_mock_server:stop(Handle)
        end
    end}.

gun_no_pool_parity_test_() ->
    {timeout, 60, fun() ->
        application:ensure_all_started(hb),
        {ok, Peer, Handle} = hb_mock_server:start(
            [{"/info", info, {200, <<"{\"ok\":true}">>}}]),
        Opts = pool_opts(#{http_client_gun_use_pool => false}),
        try
            ?assertMatch({ok, 200, _, _},
                hb_http_client:request(basic_args(Peer), Opts))
        after
            hb_mock_server:stop(Handle)
        end
    end}.

gun_pool_scope_differs_by_pool_size_test() ->
    Opts1 = pool_opts(#{http_client_gun_pool_size => 1}),
    Opts2 = pool_opts(#{http_client_gun_pool_size => 2}),
    ?assertNotEqual(
        hb_http_client:pool_scope(http1, tcp, Opts1),
        hb_http_client:pool_scope(http1, tcp, Opts2)).

gun_pool_host_override_test_() ->
    {timeout, 60, fun() ->
        application:ensure_all_started(hb),
        {ok, Peer, Handle} = hb_mock_server:start(
            [{"/x", info, {200, <<"{\"ok\":true}">>}}]),
        Opts = pool_opts(#{}),
        Args = (basic_args(Peer))#{path => <<"/x">>,
            headers => #{<<"host">> => <<"override.example">>}},
        try
            ?assertMatch({ok, 200, _, _}, hb_http_client:request(Args, Opts))
        after
            hb_mock_server:stop(Handle)
        end
    end}.

summarize({caught, C, R}) when is_tuple(R) ->
    {caught, C, element(1, R)};
summarize({caught, C, R}) ->
    {caught, C, R};
summarize(Other) ->
    Other.

pool_opts(Extra) ->
    maps:merge(
        #{http_client => gun, http_client_gun_use_pool => true,
          protocol => http1, http_retry => 0},
        Extra).

basic_args(Peer) ->
    #{peer => Peer, path => <<"/info">>, method => <<"GET">>,
      headers => #{}, body => <<>>}.

