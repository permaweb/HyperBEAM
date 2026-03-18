-module(hb_http_client_tests).
-include("include/hb.hrl").
-include("include/hb_http_client.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Regression test: create_new_connection used to reply to From immediately
%% AND store From in PendingRequests. gun_up would then reply again,
%% leaving orphan {Ref, {ok, PID}} messages in the caller's mailbox.
%% Fixed by storing an empty pending list when replying immediately.
orphan_message_leak_test_() ->
    {timeout, 60, fun() ->
        application:ensure_all_started(hb),
        flush_mailbox(),
        Peer = <<"https://arweave.net">>,
        Args = #{
            peer => Peer,
            path => <<"/info">>,
            method => <<"GET">>,
            headers => #{},
            body => <<>>
        },
        Opts = #{http_client => gun, http_retry => 0},
        {ok, 200, _, _} = hb_http_client:request(Args, Opts),
        timer:sleep(2000),
        Orphans = flush_mailbox(),
        ?event(http_client_tests, {orphaned_messages, {length, length(Orphans)}}),
        ?assertEqual(0, length(Orphans),
            "No orphan messages should be left in caller mailbox")
    end}.

unreachable_peer_hang_test_() ->
    {timeout, 30, fun() ->
        application:ensure_all_started(hb),
        Peer = <<"http://192.0.2.1:1984">>,
        Args = #{
            peer => Peer,
            path => <<"/info">>,
            method => <<"GET">>,
            headers => #{},
            body => <<>>
        },
        Opts = #{http_client => gun, http_retry => 0},
        T0 = erlang:monotonic_time(millisecond),
        Result = hb_http_client:request(Args, Opts),
        Elapsed = erlang:monotonic_time(millisecond) - T0,
        ?event(http_client_tests,
            {unreachable_peer_result, {result, Result}, {elapsed, Elapsed}}
        ),
        ?assertMatch({error, _}, Result),
        ?assert(Elapsed >= 4000 andalso Elapsed =< 15000,
            "Should block for ~5s connect_timeout, not infinity")
    end}.

bad_peer_survives_test_() ->
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
        Opts = #{http_client => gun, http_retry => 0},
        {ok, 200, _, _} = hb_http_client:request(ValidArgs, Opts),
        BadArgs = ValidArgs#{peer => <<"not-a-valid-uri">>},
        BadResult = hb_http_client:request(BadArgs, Opts),
        ?event(http_client_tests, {bad_peer_result, BadResult}),
        ?assertMatch({error, _}, BadResult),
        timer:sleep(500),
        ?assert(erlang:whereis(hb_http_client) =/= undefined,
            "gen_server must survive a bad peer URI"),
        {ok, 200, _, _} = hb_http_client:request(ValidArgs, Opts),
        ?event(http_client_tests, follow_up_request_to_valid_peer_succeeded)
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
        Opts = #{http_client => hackney, http_retry => 0},
        {ok, 200, _, _} = hb_http_client:request(Args, Opts)
    end}.

hackney_unreachable_peer_test_() ->
    {timeout, 30, fun() ->
        application:ensure_all_started(hb),
        Args = #{
            peer => <<"http://192.0.2.1:1984">>,
            path => <<"/info">>,
            method => <<"GET">>,
            headers => #{},
            body => <<>>
        },
        Opts = #{http_client => hackney, http_retry => 0},
        T0 = erlang:monotonic_time(millisecond),
        Result = hb_http_client:request(Args, Opts),
        Elapsed = erlang:monotonic_time(millisecond) - T0,
        ?event(http_client_tests,
            {hackney_unreachable_peer, {result, Result}, {elapsed, Elapsed}}
        ),
        ?assertMatch({error, _}, Result)
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

flush_mailbox() ->
    flush_mailbox([]).
flush_mailbox(Acc) ->
    receive
        Msg -> flush_mailbox([Msg | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.

summarize({caught, C, R}) when is_tuple(R) ->
    {caught, C, element(1, R)};
summarize({caught, C, R}) ->
    {caught, C, R};
summarize(Other) ->
    Other.
