-module(load_revm_nif_test).
-export([test/0]).

test() ->
    io:format("~n__load_revm_nif__~n"),
    
    % Test hello function
    HelloResult = load_revm_nif:hello(),
    io:format("~nhello() result: ~p~n", [HelloResult]),

    % Test load_evm function
    LoadEvmResult = load_revm_nif:load_evm(),
    io:format("~nload_evm() result: ~p~n", [LoadEvmResult]),
    
    ok.
