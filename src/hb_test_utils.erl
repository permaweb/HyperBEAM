%%% @doc Simple utilities for testing HyperBEAM.
-module(hb_test_utils).
-export([suite_with_opts/2, run/4, test_store/0, assert_throws/4]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Generate a new, unique test store as an isolated context for an execution.
test_store() ->
    TestDir =
        <<
            "cache-TEST/run-fs-",
            (integer_to_binary(erlang:system_time(millisecond)))/binary
        >>,
    filelib:ensure_dir(binary_to_list(TestDir)),
    #{ <<"store-module">> => hb_store_fs, <<"name">> => TestDir }.

%% @doc Run each test in a suite with each set of options. Start and reset
%% the store(s) for each test. Expects suites to be a list of tuples with
%% the test name, description, and test function.
%% The list of `Opts' should contain maps with the `name' and `opts' keys.
%% Each element may also contain a `skip' key with a list of test names to skip.
%% They can also contain a `desc' key with a description of the options.
suite_with_opts(Suite, OptsList) ->
    lists:filtermap(
        fun(OptSpec = #{ name := _Name, opts := Opts, desc := ODesc}) ->
            Store = hb_opts:get(store, hb_opts:get(store), Opts),
            Skip = hb_maps:get(skip, OptSpec, [], Opts),
            case satisfies_requirements(OptSpec) of
                true ->
                    {true, {foreach,
                        fun() ->
                            ?event({starting, Store}),
                            %Create and set a random server ID for the test process.
                            hb_http_server:set_proc_server_id(
                                hb_util:human_id(crypto:strong_rand_bytes(32))
                            ),
                            hb_store:reset(Store),
                            hb_store:start(Store)
                        end,
                        fun(_) ->
                            hb_store:reset(Store),
                            ok
                        end,
                        [
                            {
                                hb_util:list(ODesc)
                                    ++ ": "
                                    ++ hb_util:list(TestDesc),
                                fun() -> Test(Opts) end}
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

%% @doc Determine if the environment satisfies the given test requirements.
%% Requirements is a list of atoms, each corresponding to a module that must
%% return true if it exposes an `enabled/0' function.
satisfies_requirements(Requirements) when is_map(Requirements) ->
    satisfies_requirements(hb_maps:get(requires, Requirements, []));
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

%% @doc Assert that a function throws an expected exception. Needed to work around some
%% limitations in ?assertException (e.g. no way to attach an error message to the failure)
assert_throws(Fun, Args, ExpectedException, Label) ->
	Error = try 
		apply(Fun, Args),
		failed_to_throw
	catch
		error:ExpectedException -> expected_exception;
		ExpectedException -> expected_exception;
		error:Other -> {wrong_exception, Other};
		Other -> {wrong_exception, Other}
	end,
	?assertEqual(expected_exception, Error, Label).