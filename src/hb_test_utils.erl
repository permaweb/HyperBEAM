-module(hb_test_utils).
-moduledoc """
Simple utilities for testing HyperBEAM.
""".
-export([suite_with_opts/2, run/4]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-doc """
Run each test in a suite with each set of options. Start and reset
the store(s) for each test. Expects suites to be a list of tuples with
the test name, description, and test function.
The list of `Opts' should contain maps with the `name' and `opts' keys.
Each element may also contain a `skip' key with a list of test names to skip.
They can also contain a `desc' key with a description of the options.
""".
suite_with_opts(Suite, OptsList) ->
    lists:filtermap(
        fun(OptSpec = #{ name := _Name, opts := Opts, desc := ODesc}) ->
            Store = hb_opts:get(store, hb_opts:get(store), Opts),
            Skip = maps:get(skip, OptSpec, []),
            case satisfies_requirements(OptSpec) of
                true ->
                    {true, {foreach,
                        fun() ->
                            ?event({starting, Store}),
                            hb_store:start(Store)
                        end,
                        fun(_) ->
                            %hb_store:reset(Store)
                            ok
                        end,
                        [
                            {ODesc ++ ": " ++ TestDesc, fun() -> Test(Opts) end}
                        ||
                            {TestAtom, TestDesc, Test} <- Suite, 
                                not lists:member(TestAtom, Skip)
                        ]
                    }};
                false -> false
            end
        end,
        OptsList
    ).

-doc """
Determine if the environment satisfies the given test requirements.
Requirements is a list of atoms, each corresponding to a module that must
return true if it exposes an `enabled/0' function.
""".
satisfies_requirements(Requirements) when is_map(Requirements) ->
    satisfies_requirements(maps:get(requires, Requirements, []));
satisfies_requirements(Requirements) ->
    lists:all(
        fun(Req) ->
            case hb_features:enabled(Req) of
                true -> true;
                false ->
                    case code:is_loaded(Req) of
                        false -> false;
                        {file, _} ->
                            case erlang:function_exported(Req, enabled, 0) of
                                true -> Req:enabled();
                                false -> true
                            end
                    end
            end
        end,
        Requirements
    ).

%% Run a single test with a given set of options.
run(Name, OptsName, Suite, OptsList) ->
    {_, _, Test} = lists:keyfind(Name, 1, Suite),
    [Opts|_] =
        [ O || #{ name := OName, opts := O } <- OptsList,
            OName == OptsName
        ],
    Test(Opts).