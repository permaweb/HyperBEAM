%%% @doc This module implements the calculator device, which performs calculations
%%% using a C++ backend.
-module(dev_calculator).
-export([calculate/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-hb_debug(print).

%% @doc Perform a calculation using the C++ backend.
calculate(_M1, M2, Opts) ->
    try
        % Extract operation and operands from the message
        ?event("Start calculate"),
        Parameters = [
            <<"operation">>,
            <<"operand1">>,
            <<"operand2">>
        ],
        [Operation, Operand1, Operand2] = lists:map(
            fun(Parameter) ->
                case hb_ao:get(Parameter, M2, Opts) of
                    not_found -> throw({not_found, Parameter});
                    Value -> Value
                end
            end,
            Parameters
        ),

        % Convert operation from binary to string
        OperationStr = binary_to_list(Operation),
        
        % Convert operands to floats
        [Op1, Op2] = lists:map(
            fun(Operand) ->
                case string:to_float(Operand) of
                    {error, no_float} ->
                        case string:to_integer(Operand) of
                            {error, _Reason} -> throw({badarg, Operand});
                            {Int, _} -> float(Int)
                        end;
                    {Float, _} -> Float
                end
            end,
            [Operand1, Operand2]
        ),

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
        throw:{badarg, Arg} ->
            ?event({calculator_error, badarg}),
            {error, <<"Not a number: ", Arg/binary>>};
        throw:{not_found, Key} ->
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
		<<"operand1">> => <<"10">>,
		<<"operand2">> => <<"5">>
	},
	?event({testing_addition_resolve, AddM2}),
	{ok, AddResult} = hb_converge:resolve(M1, AddM2, #{}),
	?assertEqual(15.0, maps:get(<<"result">>, AddResult)),

	% Test multiplication through HyperBEAM resolve
	MulM2 = #{
		<<"path">> => <<"calculate">>,
		<<"operation">> => <<"multiply">>,
		<<"operand1">> => <<"10">>,
		<<"operand2">> => <<"5">>
	},
	?event({testing_multiplication_resolve, MulM2}),
	{ok, MulResult} = hb_converge:resolve(M1, MulM2, #{}),
	?assertEqual(50.0, maps:get(<<"result">>, MulResult)).
