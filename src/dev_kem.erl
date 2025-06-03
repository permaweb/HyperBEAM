%%% @doc A device that allow to compute over custom kernels (wgsl shaders) deployed on Arweave
%%% built on top wgpu, the kem@1.0 device is cross platform and highly flexible on GPU requirements
%%% allowing any hyperbeam node operator with a GPU to offer custom GPU compute.
%%% Also, given the flexibility of parallel computation within kernels (wgsl workgroups)
%%% this device offer parallel computation on hardware level, boosting ao's parallel compute to a lower level

-module(dev_kem).
-export([info/1, info/3, execute_kernel/3, execute_kernel_with_params/3, get_adapter_info/3, test_ao/0]).

info(_) ->
    #{
        <<"default">> => dev_message,
        handlers => #{
            <<"info">> => fun info/3,
            <<"execute_kernel">> => fun execute_kernel/3,
			<<"execute_kernel_with_params">> => fun execute_kernel_with_params/3,
            <<"get_adapter_info">> => fun get_adapter_info/3
        }
    }.

%% @doc return kernel device info
info(_Msg1, _Msg2, _Opts) ->
    InfoBody = #{
        <<"description">> => <<"Kernel device for interacting with kernel_em_nif">>,
        <<"version">> => <<"1.0">>,
        <<"paths">> => #{
            <<"info">> => <<"Get device info">>,
            <<"execute_kernel">> => <<"Execute kernel code">>,
            <<"get_adapter_info">> => <<"Get GPU adapter info">>,
			<<"execute_kernel_with_params">> => <<"Execute kernel code with params passed">>
        }
    },
    {ok, #{<<"status">> => 200, <<"body">> => InfoBody}}.

%% @doc execute input_data against the source code of a given kernel_id
execute_kernel(Msg1, _Msg2, Opts) ->
    try
        % decode the JSON body
        RawBody = hb_ao:get(<<"body">>, Msg1, not_found, Opts),
        io:format("~nRawBody: ~p~n", [RawBody]),
        
        Body = hb_json:decode(RawBody),
        io:format("~nDecoded Body: ~p~n", [Body]),
        
        % Now get parameters from decoded body
        KernelId = maps:get(<<"kernel_id">>, Body),
        RawInputData = maps:get(<<"input_data">>, Body),
        OutputSizeHint = maps:get(<<"output_size_hint">>, Body, 1),
        
        % Convert input array to binary
        InputData = list_to_binary(RawInputData),
        
        Result = kem_nif:execute_kernel(KernelId, InputData, OutputSizeHint),
        {ok, #{<<"status">> => 200, <<"body">> => Result}}
    catch
        error:Error:Stack ->
            io:format("~nError: ~p~n", [Error]),
            io:format("Stack: ~p~n", [Stack]),
            {error, #{
                <<"status">> => 500,
                <<"body">> => #{
                    <<"error">> => <<"Failed to execute kernel">>,
                    <<"details">> => Error
                }
            }}
    end.

%% @doc execute input_data and params against the source code of a given kernel_id
execute_kernel_with_params(Msg1, _Msg2, Opts) ->
    try
        % decode the JSON body
        RawBody = hb_ao:get(<<"body">>, Msg1, not_found, Opts),
        io:format("~nRawBody: ~p~n", [RawBody]),
        
        Body = hb_json:decode(RawBody),
        % io:format("~nDecoded Body: ~p~n", [Body]),
        
        % Now get parameters from decoded body
        KernelId = maps:get(<<"kernel_id">>, Body),
        RawInputData = maps:get(<<"input_data">>, Body),
        RawParams = maps:get(<<"params">>, Body),
        
        % Convert input array to binary
		InputData = list_to_binary(RawInputData),  % Keep image bytes as bytes
		Params = << <<X:32/little>> || X <- RawParams >>,
        
        Result = kem_nif:execute_kernel_with_params(KernelId, InputData, Params),
        {ok, #{<<"status">> => 200, <<"body">> => Result}}
    catch
        error:Error:Stack ->
            io:format("~nError: ~p~n", [Error]),
            io:format("Stack: ~p~n", [Stack]),
            {error, #{
                <<"status">> => 500,
                <<"body">> => #{
                    <<"error">> => <<"Failed to execute kernel">>,
                    <<"details">> => Error
                }
            }}
    end.


%% @doc get GPU adapter info
get_adapter_info(_Msg1, _Msg2, _Opts) ->
    try
        Result = kem_nif:adapter_info(),
        {ok, #{<<"status">> => 200, <<"body">> => Result}}
    catch
        error:Error ->
            {error, #{
                <<"status">> => 500,
                <<"body">> => #{
                    <<"error">> => <<"Failed to get adapter info">>,
                    <<"details">> => Error
                }
            }}
    end.

%  example call
% curl -X POST http://localhost:10001/~kem@1.0/execute_kernel   -H "Content-Type: application/json"   -d '{
%     "kernel_id": "btSvNclyu2me_zGh4X9ULVRZqwze9l2DpkcVHcLw9Eg",
%     "input_data": [1,3,5,7],
%     "output_size_hint": 1
%   }'

% in erlang shell call: dev_kem:test_ao().
test_ao() ->
    io:format("~n__test_kem_device__~n"),
    try
        % Get current wallet
        Wallet = hb:wallet(),
        % Get wallet address in human readable form  
        Address = hb_util:human_id(ar_wallet:to_address(Wallet)),

        % Create the process message
        {ok, Script} = file:read_file("test/kem-device.lua"),
        Process = hb_message:commit(#{
            <<"device">> => <<"process@1.0">>,
            <<"type">> => <<"Process">>,
            <<"scheduler-device">> => <<"scheduler@1.0">>,
            <<"execution-device">> => <<"lua@5.3a">>,
            <<"script">> => Script,
            <<"scheduler-location">> => Address,
            <<"authority">> => [Address],  % Add authority
            <<"test-random-seed">> => rand:uniform(1337)
        }, Wallet),

        % Cache the process
        {ok, _} = hb_cache:write(Process, #{}),

        % Get process ID
        ProcID = hb_message:id(Process, all),

        % Create schedule message  
        Message = hb_message:commit(#{
            <<"path">> => <<"schedule">>,
            <<"method">> => <<"POST">>,
            <<"body">> => hb_message:commit(#{
                <<"target">> => ProcID,
                <<"type">> => <<"Message">>,
                <<"action">> => <<"Eval">>
            }, Wallet)
        }, Wallet),

        % Schedule the message
        {ok, _} = hb_ao:resolve(Process, Message, #{}),

        % Get the results
        {ok, Results} = hb_ao:resolve(Process, <<"now">>, #{}),
        io:format("~nKEM test result: ~p~n", [Results])
    catch
        Error:Reason:Stack ->
            io:format("Error running KEM test: ~p:~p~n~p~n", [Error, Reason, Stack])
    end.
