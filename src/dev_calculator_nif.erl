%%% @doc This module interfaces with the C++ backend for calculations.
-module(dev_calculator_nif).
-export([calculate/3]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-on_load(init/0).

-hb_debug(print).
-define(NOT_LOADED, not_loaded(?LINE)).
not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).



%% @doc Load the NIF library.
init() ->
    %% Load the NIF library from the priv directory
	PrivDir = code:priv_dir(hb),  % Get the priv directory of the hb application
	Path = filename:join(PrivDir, "dev_calculator"),  % Join path with the library name (without .so)
	case erlang:load_nif(Path, 0) of
		ok -> ok;
		{error, Reason} -> exit({load_failed, Reason})
	end.
%% @doc Perform a calculation using the C++ backend.
calculate(_Operation, _Operand1, _Operand2) ->
    not_loaded(?LINE).

calculate_test() ->
	?event("Alex test1"),
	Result = dev_calculator_nif:calculate("multiply", 10.0, 5.0),
	?assertEqual(50.0, Result).
