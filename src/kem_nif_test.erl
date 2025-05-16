-module(kem_nif_test).
-export([test/0]).

test() ->
    io:format("~n__kernel_em_nif__~n"),
    
    % Test hello function
    try
        HelloResult = kem_nif:hello(),
        io:format("~nhello() result: ~p~n", [HelloResult])
    catch
        Error:Reason:Stack ->
            io:format("Error calling hello(): ~p:~p~n~p~n", [Error, Reason, Stack])
    end,

    % Test adapter_info function
    try
        AdapterInfo = kem_nif:adapter_info(),
        io:format("~nadapter_info() result: ~p~n", [AdapterInfo])
    catch
        Error2:Reason2:Stack2 ->
            io:format("Error calling adapter_info(): ~p:~p~n~p~n", [Error2, Reason2, Stack2])
    end,

% Test execute_kernel function
try
	KernelId = <<"btSvNclyu2me_zGh4X9ULVRZqwze9l2DpkcVHcLw9Eg">>,
	InputData = <<1,3,5,7>>,  % Raw bytes as binary
	OutputSizeHint = 1,
    
    KernelResult = kem_nif:execute_kernel(KernelId, InputData, OutputSizeHint),
    io:format("~nexecute_kernel() result: ~p~n", [KernelResult])
catch
    Error3:Reason3:Stack3 ->
        io:format("Error calling execute_kernel(): ~p:~p~n~p~n", [Error3, Reason3, Stack3])
end,

    
    ok.
