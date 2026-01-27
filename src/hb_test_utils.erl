%%% @doc Simple utilities for testing HyperBEAM. Includes functions for 
%%% generating isolated (fresh) test stores, running suites of tests with
%%% differing options, as well as executing and reporting benchmarks.
-module(hb_test_utils).
-export([suite_with_opts/2, run/4, assert_throws/4]).
-export([test_store/0, test_store/1, test_store/2]).
-export([benchmark/1, benchmark/2, benchmark/3, benchmark_iterations/2]).
-export([benchmark_print/2, benchmark_print/3, benchmark_print/4]).
-export([compare_events/3, compare_events/4, compare_events/5]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The number of seconds to run a benchmark for when no time is specified.
-define(DEFAULT_BENCHMARK_TIME, 1).

%% @doc Generate a new, unique test store as an isolated context for an execution.
test_store() ->
    test_store(maps:get(<<"store-module">>, hd(hb_opts:get(store)))).
test_store(hb_store_preloaded) -> 
    #{
        <<"store-module">> => hb_store_preloaded,
        <<"arweave@2.9-pre">> => dev_arweave,
        <<"apply@1.0">> => dev_apply,
        <<"auth-hook@1.0">> => dev_auth_hook,
        <<"ans104@1.0">> => dev_codec_ans104,
        <<"bundler@1.0">> => dev_bundler,
        <<"compute@1.0">> => dev_cu,
        <<"cache@1.0">> => dev_cache,
        <<"cacheviz@1.0">> => dev_cacheviz,
        <<"cookie@1.0">> => dev_codec_cookie,
        <<"cron@1.0">> => dev_cron,
        <<"dedup@1.0">> => dev_dedup,
        <<"delegated-compute@1.0">> => dev_delegated_compute,
        <<"faff@1.0">> => dev_faff,
        <<"flat@1.0">> => dev_codec_flat,
        <<"genesis-wasm@1.0">> => dev_genesis_wasm,
        <<"greenzone@1.0">> => dev_green_zone,
        <<"httpsig@1.0">> => dev_codec_httpsig,
        <<"http-auth@1.0">> => dev_codec_http_auth,
        <<"hook@1.0">> => dev_hook,
        <<"hyperbuddy@1.0">> => dev_hyperbuddy,
        <<"copycat@1.0">> => dev_copycat,
        <<"json@1.0">> => dev_codec_json,
        <<"json-iface@1.0">> => dev_json_iface,
        <<"local-name@1.0">> => dev_local_name,
        <<"lookup@1.0">> => dev_lookup,
        <<"lua@5.3a">> => dev_lua,
        <<"manifest@1.0">> => dev_manifest,
        <<"message@1.0">> => dev_message,
        <<"meta@1.0">> => dev_meta,
        <<"monitor@1.0">> => dev_monitor,
        <<"multipass@1.0">> => dev_multipass,
        <<"name@1.0">> => dev_name,
        <<"node-process@1.0">> => dev_node_process,
        <<"p4@1.0">> => dev_p4,
        <<"patch@1.0">> => dev_patch,
        <<"poda@1.0">> => dev_poda,
        <<"process@1.0">> => dev_process,
        <<"profile@1.0">> => dev_profile,
        <<"push@1.0">> => dev_push,
        <<"query@1.0">> => dev_query,
        <<"relay@1.0">> => dev_relay,
        <<"router@1.0">> => dev_router,
        <<"scheduler@1.0">> => dev_scheduler,
        <<"simple-pay@1.0">> => dev_simple_pay,
        <<"snp@1.0">> => dev_snp,
        <<"stack@1.0">> => dev_stack,
        <<"structured@1.0">> => dev_codec_structured,
        <<"test-device@1.0">> => dev_test,
        <<"trie@1.0">> => dev_trie,
        <<"tx@1.0">> => dev_codec_tx,
        <<"volume@1.0">> => dev_volume,
        <<"secret@1.0">> => dev_secret,
        <<"wasi@1.0">> => dev_wasi,
        <<"wasm-64@1.0">> => dev_wasm,
        <<"whois@1.0">> => dev_whois
    };
test_store(Mod) ->
    test_store(Mod, <<"default">>).
test_store(Mod, Tag) ->
    TestDir =
        <<
            "cache-TEST"
        >>,
    % Wait a tiny interval to ensure that any further tests will get their own
    % directory.
    timer:sleep(1),
    filelib:ensure_dir(binary_to_list(TestDir)),
    #{ <<"store-module">> => Mod, <<"name">> => TestDir }.

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
                            % Create and set a random server ID for the test
                            % process.
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

%% @doc Find the options from a list of options by name.
opts_from_list(OptsName, OptsList) ->
    hd([ O || #{ name := OName, opts := O } <- OptsList, OName == OptsName ]).

%% Run a single test with a given set of options.
run(Name, OptsName, Suite, OptsList) ->
    {_, _, Test} = lists:keyfind(Name, 1, Suite),
    Test(opts_from_list(OptsName, OptsList)).

%% @doc Compares the events generated by executing a test/function with two 
%% different sets of options.
compare_events(Fun, Opts1, Opts2) ->
    hb_store:reset(hb_opts:get(store, hb_opts:get(store), Opts1)),
    hb_store:write(
        hb_opts:get(store, hb_opts:get(store), Opts1),
        <<"test">>,
        <<"test">>
    ),
    {EventsSample1, _Res2} = hb_event:diff(
        fun() ->
            Fun(Opts1)
        end
    ),
    hb_store:reset(hb_opts:get(store, hb_opts:get(store), Opts1)),
    hb_store:reset(hb_opts:get(store, hb_opts:get(store), Opts2)),
    {EventsSample2, _Res} = hb_event:diff(
        fun() ->
            Fun(Opts2)
        end
    ),
    hb_store:reset(hb_opts:get(store, hb_opts:get(store), Opts2)),
    EventsDiff = hb_message:diff(EventsSample1, EventsSample2, #{}),
    ?event(
        debug_perf,
        {events,
            {sample1, EventsSample1},
            {sample2, EventsSample2},
            {events_diff, EventsDiff}
        }
    ),
    EventsDiff.
compare_events(Fun, OptsName1, OptsName2, OptsList) ->
    compare_events(
        Fun,
        opts_from_list(OptsName1, OptsList),
        opts_from_list(OptsName2, OptsList)
    ).
compare_events(Name, OptsName1, OptsName2, Suite, OptsList) ->
    {_, _, Test} = lists:keyfind(Name, 1, Suite),
    compare_events(
        Test,
        opts_from_list(OptsName1, OptsList),
        opts_from_list(OptsName2, OptsList)
    ).

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

%% @doc Run a function as many times as possible in a given amount of time.
benchmark(Fun) ->
    benchmark(Fun, ?DEFAULT_BENCHMARK_TIME).
benchmark(Fun, TLen) ->
    T0 = erlang:system_time(millisecond),
    hb_util:until(
        fun() -> erlang:system_time(millisecond) - T0 > (TLen * 1000) end,
        Fun,
        0
    ).

%% @doc Return the amount of time required to execute N iterations of a function
%% as a fraction of a second.
benchmark_iterations(Fun, N) ->
    {Time, _} = timer:tc(
        fun() ->
            lists:foreach(
                fun(I) -> Fun(I) end,
                lists:seq(1, N)
            )
        end
    ),
    Time / 1_000_000.

%% @doc Run multiple instances of a function in parallel for a given amount of time.
benchmark(Fun, TLen, Procs) ->
    Parent = self(),
    receive _ -> worker_synchronized end,
    StartWorker =
        fun(_) ->
            Ref = make_ref(),
            spawn_link(fun() ->
                Count = benchmark(Fun, TLen),
                Parent ! {work_complete, Ref, Count}
            end),
            Ref
        end,
    CollectRes =
        fun(R) ->
            receive
                {work_complete, R, Count} ->
                    %?event(benchmark, {work_complete, R, Count}),
                    Count
            end
        end,
    Refs = lists:map(StartWorker, lists:seq(1, Procs)),
    lists:sum(lists:map(CollectRes, Refs)).

%% @doc Print benchmark results in a human-readable format that EUnit writes to
%% the console. Takes a `verb` as a string and an `iterations` count (returned
%% by the benchmark function), as well as optionally a `noun` to refer to the
%% objects in the benchmark, and a `time` in seconds. If `time' is not
%% provided, it defaults to the value of `?DEFAULT_BENCHMARK_TIME'.
benchmark_print(Verb, Iterations) ->
    benchmark_print(Verb, Iterations, ?DEFAULT_BENCHMARK_TIME).
benchmark_print(Verb, Iterations, Time) when is_integer(Iterations) ->
    hb_format:eunit_print(
        "~s ~s in ~s (~s/s)",
        [
            Verb,
            hb_util:human_int(Iterations),
            format_time(Time),
            hb_util:human_int(Iterations / Time)
        ]
    );
benchmark_print(Verb, Noun, Iterations) ->
    benchmark_print(Verb, Noun, Iterations, ?DEFAULT_BENCHMARK_TIME).
benchmark_print(Verb, Noun, Iterations, Time) ->
    hb_format:eunit_print(
        "~s ~s ~s in ~s (~s ~s/s)",
        [
            Verb,
            hb_util:human_int(Iterations),
            Noun,
            format_time(Time),
            hb_util:human_int(Iterations / Time),
            Noun
        ]
    ).

%% @doc Format a time in human-readable format. Takes arguments in seconds.
format_time(Time) when is_integer(Time) ->
    hb_util:human_int(Time) ++ "s";
format_time(Time) ->
    hb_util:human_int(Time * 1000) ++ "ms".
