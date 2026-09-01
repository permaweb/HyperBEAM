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
