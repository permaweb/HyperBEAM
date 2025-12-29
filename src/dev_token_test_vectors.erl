%%% @doc Test vectors and benchmarks for the `~token@1.0` device.
-module(dev_token_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% Test Helpers: Generators.
%%% ----------------------------------------------------------------------------
balance(Process, Req, Opts) ->
    dev_token_lib:balance(Process, Req, Opts).

%% @doc Generate generic isolated node messages for testing.
test_opts() ->
    hb:init(),
    #{
        priv_wallet => ar_wallet:new(),
        store => [hb_test_utils:test_store()]
    }.

%% @doc Generate a random ID, or an 'ID' value of the correct length starting
%% with the given binary and padded with zeros.
id(Wallet) -> dev_process_lib:wallet_id(Wallet).

transfer_req(Addr, Qty, Params) ->
    Params#{
        <<"action">> => <<"transfer">>,
        <<"recipient">> => Addr,
        <<"quantity">> => Qty
    }.

%% @doc Create a request message.
schedule_request(Process, Body, Wallet, Opts) ->
    Signed =
        hb_message:commit(
            Body,
            Opts#{ priv_wallet => Wallet }
        ),
    Req =
        Signed#{
            <<"method">> => <<"POST">>,
            <<"path">> => <<"schedule">>
        },
    {ok, Res} = hb_ao:resolve(Process, Req, Opts),
    ?event(debug_test, {schedule_result, Res}, Opts),
    Res.

push_transfer(Process, Sender, Recipient, Qty, Opts) ->
    dev_token_lib:transfer(Process, Sender, Recipient, Qty, Opts).

%%% Test Cases.
%%% ----------------------------------------------------------------------------

simple_process_test() ->
    hb:init(),
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Process = 
        dev_token_lib:ledger(
            #{ <<"execution-device">> => <<"token@1.0">>, 
            <<"balances">> => #{ id(Alice) => 1_000_000_000 }}, 
            Opts
        ),
    push_transfer(Process, Alice, Bob, 1, Opts),
    State = dev_token_lib:now(Process, Opts),
    ?assertEqual(999_999_999, balance(State, Alice, Opts)),
    ?assertEqual(1, balance(State, Bob, Opts)),
    ?assertEqual(1_000_000_000, hb_ao:get(<<"total-supply">>, State, Opts)).

%%% Benchmark Tests
benchmark_transfers_process_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    % Benchmark N transfers
    Transfers = 100,
    Accounts = 1_000,
    % Setup: Alice has 1 billion tokens, the rest have 1 billion tokens each
    Base = 
        dev_token_lib:ledger(
            #{ 
                <<"execution-device">> => <<"token@1.0">>, 
                <<"balances">> => 
                    hb_maps:from_list(
                        [
                            {id(Alice), 1_000_000_000}
                        ] ++
                        [
                                {
                                    hb_util:human_id(
                                        crypto:strong_rand_bytes(32)
                                    ),
                                    1_000_000_000
                                }
                            ||
                                _ <- lists:seq(1, Accounts - 1)
                        ]
                    )
            }, 
            Opts
        ),
    Reqs =
        [
            schedule_request(
                Base,
                transfer_req(id(Bob), 1, #{ <<"transfer-number">> => I }),
                Alice,
                Opts
            )
            ||
                I <- lists:seq(1, Transfers)
        ],
    AOCoreStartTime = erlang:monotonic_time(millisecond),
    AOCoreInvokedState =
        lists:foldl(
            fun(Req, State) ->
                {ok, NewState} = hb_ao:resolve(State, Req, Opts),
                NewState#{ <<"results">> => #{} }
            end,
            Base,
            Reqs
        ),
    AOCoreEndTime = erlang:monotonic_time(millisecond),
    hb_test_utils:benchmark_print(
        <<"AOCore invoked transfers">>,
        <<"transfers">>,
        Transfers,
        (AOCoreEndTime - AOCoreStartTime) / 1000
    ),
    % Verify correctness
    ?assertEqual(
        1_000_000_000 - Transfers,
        balance(AOCoreInvokedState, id(Alice), Opts)
    ),
    ?assertEqual(
        Transfers,
        balance(AOCoreInvokedState, id(Bob), Opts)
    ).

benchmark_process_transfers_test_() ->
    {timeout, 180, fun benchmark_process_transfers/0}.
benchmark_process_transfers() ->
    hb:init(),
    % Benchmark N transfers
    Transfers = 100,
    Accounts = 3_000,
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    % Setup: Alice has 1 billion tokens, the rest have 1 billion tokens each
    Base =
        dev_token_lib:ledger(
            #{ 
                <<"execution-device">> => <<"token@1.0">>, 
                <<"balances">> => 
                    hb_maps:from_list(
                        [
                            {id(Alice), 1_000_000_000}
                        ] ++
                        [
                            {
                                hb_util:human_id(crypto:strong_rand_bytes(32)),
                                1_000_000_000
                            }
                            ||
                                _ <- lists:seq(1, Accounts - 1)
                        ]
                    )
            }, 
            Opts
        ),
    lists:foreach(
        fun(I) ->
            schedule_request(
                Base,
                transfer_req(id(Bob), 1, #{ <<"transfer-number">> => I }),
                Alice,
                Opts
            )
        end,
        lists:seq(1, Transfers)
    ),
    NowStartTime = erlang:monotonic_time(millisecond),
    State =dev_token_lib:now(Base, Opts),
    NowEndTime = erlang:monotonic_time(millisecond),
    hb_test_utils:benchmark_print(
        <<"Process transfers">>,
        <<"transfers">>,
        Transfers,
        (NowEndTime - NowStartTime) / 1000
    ),
    ?assertEqual(Transfers, balance(State, id(Bob), Opts)),
    ?assertEqual(1_000_000_000 - Transfers, balance(State, id(Alice), Opts)),
    ?assertEqual(
        1_000_000_000 * Accounts,
        hb_ao:get(<<"total-supply">>, State, Opts)
    ).