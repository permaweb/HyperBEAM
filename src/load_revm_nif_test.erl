-module(load_revm_nif_test).
-export([test/0]).

test() ->
    io:format("~n__load_revm_nif__~n"),
    
    % Test hello function
    try
        HelloResult = load_revm_nif:hello(),
        io:format("~nhello() result: ~p~n", [HelloResult])
    catch
        Error:Reason:Stack ->
            io:format("Error calling hello(): ~p:~p~n~p~n", [Error, Reason, Stack])
    end,

    % Test eval_bytecode function
    try
        % Convert string to binary for the signed raw transaction
        SignedRawTx = list_to_binary("02f87782251881878447868c008447868c008307a12094c69b7ea1931e207bebe89fa32b10435aec234c40893635c9adc5dea0000080c080a07a207f6815ba6a9d92b49f205695514c93fbdb2abe37a1610389958a92f13aa4a0556295a23580307b7f0248784dcb9ec9c9c7aba9234703f902e0b37f227ab9f2"),
		CoutState = list_to_binary("native/load_revm_nif/appchains/1.json"),

        
        % Convert string to binary for the state JSON
        State = list_to_binary("{\"accounts\":{\"0x0000000000000000000000000000000000000000\":{\"nonce\":0,\"balance\":\"3100000000000001323000\",\"code\":null},\"0x197f818c1313dc58b32d88078ecdfb40ea822614\":{\"nonce\":135
			,\"balance\":\"994899999999999998676999\",\"code\":null},\"0xc69b7ea1931e207bebe89fa32b10435aec234c40\":{\"nonce\":0,\"balance\":\"2000000000000000000000\",\"code\":null}},\"storage\":{}}"),
        
        LoadEvmResult = load_revm_nif:eval_bytecode(SignedRawTx, State, CoutState),
        io:format("~neval_bytecode() result: ~p~n", [LoadEvmResult])
    catch
        Error2:Reason2:Stack2 ->
            io:format("Error calling eval_bytecode(): ~p:~p~n~p~n", [Error2, Reason2, Stack2])
    end,

% Test Retrieve State
try 
    ChainId = list_to_binary("9496"),
    StateResult = load_revm_nif:get_appchain_state(ChainId),
    % {ok, Decoded} = hb_json:decode(StateResult),
    io:format("~n 9496 appchain state: ~p~n", [StateResult])
catch
    error:Error3:Stack3 ->
        io:format("Error calling get_appchain_state(): ~p~n~p~n", [Error3, Stack3])
end,
    
	 ok.
 