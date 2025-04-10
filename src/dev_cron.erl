%%% A device that inserts new messages into the schedule to allow processes
%%% to passively 'call' themselves without user interaction.
-module(dev_cron).
-export([once/3, every/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

once(Msg, Msg2, Opts) ->
    {ok, Msg2}.

every(Msg, Msg2, Opts) ->
    {ok, Msg2}.

%% @doc Parse a time string into milliseconds.
parse_time(BinString) ->
    [AmountStr, UnitStr] = binary:split(BinString, <<"-">>),
    Amount = binary_to_integer(AmountStr),
    Unit = string:lowercase(binary_to_list(UnitStr)),
    case Unit of
        "millisecond" ++ _ -> Amount;
        "second" ++ _ -> Amount * 1000;
        "minute" ++ _ -> Amount * 60 * 1000;
        "hour" ++ _ -> Amount * 60 * 60 * 1000;
        "day" ++ _ -> Amount * 24 * 60 * 60 * 1000;
        _ -> throw({error, invalid_time_unit, UnitStr})
    end.

%%% Tests

once_executed_test() ->
    Node = hb_http_server:start_node(),
    PID = spawn(fun test_worker/0),
    ID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    hb_name:register(PID, {<<"test">>, ID}),
    hb_http:get(Node,
        <<"/~cron@1.0/once?test-id=", ID/binary,
            "&cron-path=/~test@1.0/update_state">>,
        #{}
    ),
    timer:sleep(1000),
    PID ! {get, self()},
    receive
        {state, State} ->
            ?assertMatch(#{ <<"test-id">> := ID }, State)
    after 1000 ->
        throw(no_response_from_worker)
    end.

test_worker() -> test_worker(undefined).
test_worker(State) ->
    receive
        {update, NewState} ->
            test_worker(NewState);
        {get, Pid} ->
            Pid ! {state, State},
            test_worker(State)
    end.