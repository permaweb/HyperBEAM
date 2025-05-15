%%% @doc a device to interact with the EVM execution client (interpreted EVM) 
-module(dev_evm).
-export([info/1, info/3, get_state/3, test_hb_http/0, eval_tx/3, test_ao/0]).

info(_) ->
    #{
        <<"default">> => dev_message,
		handlers => #{
			<<"info">> => fun info/3,
			<<"get_state">> => fun get_state/3,
			<<"eval_tx">> => fun eval_tx/3
		}

    }.
%% @doc return evm device info
info(_Msg1, _Msg2, _Opts) ->
    InfoBody = #{
        <<"description">> => <<"EVM device for interacting with load_revm_nif">>,
        <<"version">> => <<"1.0">>,
		<<"paths">> => #{
			<<"info">> => <<"Get device info">>,
			<<"get_state">> => <<"Get appchain state">>,
			<<"eval_tx">> => <<"Evaluate a transaction">>
		}

    },
    {ok, #{<<"status">> => 200, <<"body">> => InfoBody}}.
%% @doc get the JSON-serialized EVM state for a given chain_id
get_state(Msg1, _Msg2, Opts) ->
    ChainId = case hb_ao:get(<<"chain_id">>, Msg1, not_found, Opts) of
        not_found -> <<"9496">>;  % default chain id (Load's appchain)
        Id -> Id
    end,
    try
        Result = load_revm_nif:get_appchain_state(ChainId),
        {ok, #{<<"status">> => 200, <<"body">> => Result}}
    catch
        error:Error ->
            {error, #{
                <<"status">> => 500,
                <<"body">> => #{
                    <<"error">> => <<"Failed to get appchain state">>,
                    <<"details">> => Error
                }
            }}
    end.

%% @doc evaluate a transaction on an appchain
eval_tx(Msg1, _Msg2, Opts) ->
    try
        % decode the JSON body
        RawBody = hb_ao:get(<<"body">>, Msg1, not_found, Opts),
        Body = hb_json:decode(RawBody),
		CoutState = list_to_binary("native/load_revm_nif/appchains/9496.json"),

        % Get parameters from decoded body
        SignedRawTx = maps:get(<<"signed_raw_tx">>, Body),
        ChainId = maps:get(<<"chain_id">>, Body, <<"9496">>),  % FOR NOW default to Load's appchain

        % % Get current state
        % State = load_revm_nif:get_appchain_state(ChainId),
		% io:format(State),try
    	State = load_revm_nif:get_appchain_state(ChainId),
		% io:format("~nCHAIN ID: : ~p~n", [ChainId]),
    	io:format("~n9496 appchain state: ~p~n", [State]),



        % Evaluate the transaction
        Result = load_revm_nif:eval_bytecode(SignedRawTx, State, CoutState),
        {ok, #{<<"status">> => 200, <<"body">> => Result}}
    catch
        error:Error ->
            {error, #{
                <<"status">> => 500,
                <<"body">> => #{
                    <<"error">> => <<"Failed to evaluate transaction">>,
                    <<"details">> => Error
                }
            }}
    end.


% in erlang shell call: dev_evm:test_hb_http().
test_hb_http() ->
    io:format("~n__test_evm_device__~n"),
    try
        % start a node with proper configuration
        Node = hb_http_server:start_node(#{
            priv_wallet => ar_wallet:new()
        }),

        % Create a message that uses the EVM device
        Base = #{
            <<"device">> => <<"evm@1.0">>,
            <<"path">> => <<"get_state">>,
            <<"chain_id">> => <<"9496">>
        },

        % Use hb_http:get to test the device through the node
        Result = hb_http:get(Node, <<"/~evm@1.0/get_state">>, #{}),
        io:format("~nEVM test result: ~p~n", [Result])
    catch
        Error:Reason:Stack ->
            io:format("Error running EVM test: ~p:~p~n~p~n", [Error, Reason, Stack])
    end.

% in erlang shell call: dev_evm:test_ao().
test_ao() ->
    io:format("~n__test_evm_device__~n"),
    try
        % Get current wallet
        Wallet = hb:wallet(),
        % Get wallet address in human readable form
        Address = hb_util:human_id(ar_wallet:to_address(Wallet)),

        % Create the process message
        {ok, Script} = file:read_file("test/evm-device.lua"),
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
        io:format("~nEVM test result: ~p~n", [Results])
    catch
        Error:Reason:Stack ->
            io:format("Error running EVM test: ~p:~p~n~p~n", [Error, Reason, Stack])
    end.