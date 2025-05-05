-module(riscv_em_nif_test).
-export([test/0]).

test() ->
    io:format("~n__riscv_em_nif__~n"),
	ChainId = list_to_binary("1"),
    
    % Test hello function
    try
        HelloResult = riscv_em_nif:hello(),
        io:format("~nhello() result: ~p~n", [HelloResult])
    catch
        Error:Reason:Stack ->
            io:format("Error calling hello(): ~p:~p~n~p~n", [Error, Reason, Stack])
    end,

    % Test eval_riscv_bytecode function
    try
        % Mint riscv erc20
        SignedRawTx = list_to_binary("f8ad80850af16b16008435a4e90094f6a171f57acac30c292e223ea8adbb28abd3e14d80b84440c10f19000000000000000000000000000000000000000000000000000000000000000b0000000000000000000000000000000000000000000000056bc75e2d63100000824a53a0eab71453afe2dd526d44a9a8d722c050ac411002f658fade8b270f4e7e36b6d6a02d309512fa3abc8563ba0800dc0bd3d33ae2fe42cf349a51006a30f285ed755b"),
    
        LoadRiscvResult = riscv_em_nif:eval_riscv_bytecode(SignedRawTx, ChainId),
        io:format("~neval_riscv_bytecode() result: ~p~n", [LoadRiscvResult])
    catch
        Error2:Reason2:Stack2 ->
            io:format("Error calling eval_riscv_bytecode(): ~p:~p~n~p~n", [Error2, Reason2, Stack2])
    end,

% Test Retrieve State
try 
    StateResult = riscv_em_nif:get_appchain_state(ChainId),
    % {ok, Decoded} = hb_json:decode(StateResult),
    io:format("~n chaind_id 1 appchain state: ~p~n", [StateResult])
catch
    error:Error3:Stack3 ->
        io:format("Error calling get_appchain_state(): ~p~n~p~n", [Error3, Stack3])
end,
    
	 ok.
 