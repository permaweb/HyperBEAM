-module(su_request).
-export([get_result/2]).

%% get_result(ProcessId, Nonce)
get_result(ProcessId, Nonce) ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),

    %% Build su-router URL
    SuUrl = io_lib:format(
      "https://su-router.ao-testnet.xyz/~s?from-nonce=~p&limit=1",
      [ProcessId, Nonce - 1]),
    io:format("Getting message id at nonce ~p... ~s~n", [Nonce, SuUrl]),

    %% Fetch message id
    {ok, {{_, 200, _}, _Headers, Body1}} =
        httpc:request(get, {SuUrl, []}, [], []),
    Json1 = hb_json:decode(list_to_binary(Body1)),
    [FirstEdge | _] = maps:get(<<"edges">>, Json1),
    Node = maps:get(<<"node">>, FirstEdge),
    Message = maps:get(<<"message">>, Node),
    MessageId = maps:get(<<"id">>, Message),
    io:format("Got message ~p~n", [Message]),
    io:format("Got message id ~s~n", [MessageId]),

    %% Build CU testnet URL
    TestnetUrl = io_lib:format(
      "https://cu.ao-testnet.xyz/result/~s?process-id=~s",
      [MessageId, ProcessId]),
    io:format("Fetching testnet result... ~s~n", [TestnetUrl]),

    %% Fetch testnet result
    case httpc:request(get, {TestnetUrl, []}, [], []) of
        {ok, {{_, 200, _}, _Headers2, Body2}} ->
            TestnetResult = hb_json:decode(list_to_binary(Body2)),
            io:format("Fetched testnet result: ~n~P~n", [Body2, -1]),
            {ok, TestnetResult};
        Error ->
            io:format("Error fetching testnet result: ~p~n", [Error]),
            Error
    end.