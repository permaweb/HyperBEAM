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
basic_calculation_test() ->
    % Initialize test messages
    M1 = #{<<"device">> => <<"calculator@1.0">>},
    ?event("Starting basic calculation test"),
    
    % Test addition
    AddM2 = #{
        <<"operation">> => <<"add">>,
        <<"operand1">> => 10,
        <<"operand2">> => 5
    },
    ?event({testing_addition, AddM2}),
    {ok, AddResult} = calculate(M1, AddM2, #{}),
    ?assertEqual(15.0, maps:get(<<"result">>, AddResult)),

    % Test multiplication
    MulM2 = #{
        <<"operation">> => <<"multiply">>,
        <<"operand1">> => 10,
        <<"operand2">> => 5
    },
    ?event({testing_multiplication, MulM2}),
    {ok, MulResult} = calculate(M1, MulM2, #{}),
    ?assertEqual(50.0, maps:get(<<"result">>, MulResult)),

    % Test division
    DivM2 = #{
        <<"operation">> => <<"divide">>,
        <<"operand1">> => 10,
        <<"operand2">> => 2
    },
    ?event({testing_division, DivM2}),
    {ok, DivResult} = calculate(M1, DivM2, #{}),
    ?assertEqual(5.0, maps:get(<<"result">>, DivResult)),

    % Test subtraction
    SubM2 = #{
        <<"operation">> => <<"subtract">>,
        <<"operand1">> => 10,
        <<"operand2">> => 3
    },
    ?event({testing_subtraction, SubM2}),
    {ok, SubResult} = calculate(M1, SubM2, #{}),
    ?assertEqual(7.0, maps:get(<<"result">>, SubResult)).