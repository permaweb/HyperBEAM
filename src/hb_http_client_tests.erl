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
        ?debugFmt("Orphan messages after first request: ~p", [length(Orphans)]),
        ?assertEqual(0, length(Orphans),
            "No orphan messages should be left in caller mailbox")
    end}.

flush_mailbox() ->
    flush_mailbox([]).
flush_mailbox(Acc) ->
    receive
        Msg -> flush_mailbox([Msg | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.
