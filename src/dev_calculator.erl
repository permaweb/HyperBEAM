%%% @doc This module implements the calculator device, which performs calculations
%%% using a C++ backend.
-module(dev_calculator).
-export([calculate/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-hb_debug(print).

%% @doc Perform a calculation using the C++ backend.
calculate(M1, M2, Opts) ->
    try
        % Extract operation and operands from the message
        ?event("Start calculate"),
        Operation = hb_converge:get(<<"operation">>, M2, Opts),
        Operand1 = hb_converge:get(<<"operand1">>, M2, Opts),
        Operand2 = hb_converge:get(<<"operand2">>, M2, Opts),

        % Convert operation from binary to string
        OperationStr = binary_to_list(Operation),
        
        % Convert operands to float
        Op1 = erlang:float(Operand1),
        Op2 = erlang:float(Operand2),

        ?event({calculator_input, OperationStr, Op1, Op2}),

        % Perform calculation using NIF
        Result = dev_calculator_nif:calculate(OperationStr, Op1, Op2),

        ?event({calculator_result, Result}),

        % Return the result in the expected format
        {ok, #{
            <<"result">> => Result,
            <<"operation">> => Operation,
            <<"operand1">> => Op1,
            <<"operand2">> => Op2
        }}
    catch
        error:{badarg, _} ->
            ?event({calculator_error, badarg}),
            {error, <<"Invalid arguments for calculation">>};
        error:{not_found, Key} ->
            ?event({calculator_error, {not_found, Key}}),
            {error, <<"Missing required parameter: ", Key/binary>>};
        Error:Reason:Stack ->
            ?event({calculator_error, Error, Reason, Stack}),
            {error, <<"Calculation failed">>}
    end.

%% Tests
device_calculation_test() ->
	% Initialize test messages with device specification
	M1 = #{<<"device">> => <<"calculator@1.0">>},
	?event("Starting device calculation tests"),
	
	% Test addition through HyperBEAM resolve
	AddM2 = #{
		<<"path">> => <<"calculate">>,  % Specify the device function to call
		<<"operation">> => <<"add">>,
		<<"operand1">> => 10,
		<<"operand2">> => 5
	},
	?event({testing_addition_resolve, AddM2}),
	{ok, AddResult} = hb_converge:resolve(M1, AddM2, #{}),
	?assertEqual(15.0, maps:get(<<"result">>, AddResult)),

	% Test multiplication through HyperBEAM resolve
	MulM2 = #{
		<<"path">> => <<"calculate">>,
		<<"operation">> => <<"multiply">>,
		<<"operand1">> => 10,
		<<"operand2">> => 5
	},
	?event({testing_multiplication_resolve, MulM2}),
	{ok, MulResult} = hb_converge:resolve(M1, MulM2, #{}),
	?assertEqual(50.0, maps:get(<<"result">>, MulResult)).
