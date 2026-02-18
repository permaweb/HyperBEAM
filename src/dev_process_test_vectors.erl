%%% @doc Test vectors for the `~process@1.0' and associated subsystems.
-module(dev_process_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
%%% Helpers used by other devices that utilize `~process@1.0'.
-export([init/0, aos_process/0, aos_process/1, test_process/0, wasm_process/1]).
-export([schedule_aos_call/2, schedule_aos_call/3]).

init() ->
    application:ensure_all_started(hb),
    ok.

%% @doc Generate a process message with a random number, and no executor.
base_process() ->
    base_process(#{}).
base_process(Opts) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    hb_message:commit(
        #{
            <<"device">> => <<"process@1.0">>,
            <<"scheduler-device">> => <<"scheduler@1.0">>,
            <<"scheduler-location">> => hb_opts:get(scheduler, Address, Opts),
            <<"type">> => <<"Process">>,
            <<"test-random-seed">> => rand:uniform(1337)
        },
        Opts#{ priv_wallet => Wallet }
    ).

wasm_process(WASMImage) ->
    wasm_process(WASMImage, #{}).
wasm_process(WASMImage, Opts) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    #{ <<"image">> := WASMImageID } = dev_wasm:cache_wasm_image(WASMImage, Opts),
    hb_message:commit(
        hb_maps:merge(
            hb_message:uncommitted(base_process(Opts), Opts),
            #{
                <<"execution-device">> => <<"stack@1.0">>,
                <<"device-stack">> => [<<"wasm-64@1.0">>],
                <<"image">> => WASMImageID
            },
			Opts
        ),
        Opts#{ priv_wallet => Wallet }
    ).

%% @doc Generate a process message with a random number, and the 
%% `dev_wasm' device for execution.
aos_process() ->
    aos_process(#{}).
aos_process(Opts) ->
    aos_process(Opts, [
        <<"wasi@1.0">>,
        <<"json-iface@1.0">>,
        <<"wasm-64@1.0">>,
        <<"multipass@1.0">>
    ]).
aos_process(Opts, Stack) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    WASMProc = wasm_process(<<"test/aos-2-pure-xs.wasm">>, Opts),
    hb_message:commit(
        hb_maps:merge(
            hb_message:uncommitted(WASMProc, Opts),
            #{
                <<"device-stack">> => Stack,
                <<"execution-device">> => <<"stack@1.0">>,
                <<"scheduler-device">> => <<"scheduler@1.0">>,
                <<"output-prefix">> => <<"wasm">>,
                <<"patch-from">> => <<"/results/outbox">>,
                <<"passes">> => 2,
                <<"stack-keys">> =>
                    [
                        <<"init">>,
                        <<"compute">>,
                        <<"snapshot">>,
                        <<"normalize">>
                    ],
                <<"scheduler">> =>
                    hb_opts:get(scheduler, Address, Opts),
                <<"authority">> =>
                    hb_opts:get(authority, Address, Opts)
            }, Opts),
        Opts#{ priv_wallet => Wallet }
    ).

%% @doc Generate a device that has a stack of two `dev_test's for 
%% execution. This should generate a message state has doubled 
%% `Already-Seen' elements for each assigned slot.
test_process() ->
    Wallet = hb:wallet(),
    hb_message:commit(
        hb_maps:merge(
            base_process(),
            #{
                <<"execution-device">> => <<"stack@1.0">>,
                <<"device-stack">> => [<<"test-device@1.0">>, <<"test-device@1.0">>]
            }, 
            #{}
        ),
        #{ priv_wallet => Wallet }
    ).

schedule_test_message(Base, Text, Opts) ->
    schedule_test_message(Base, Text, #{}, Opts).
schedule_test_message(Base, Text, MsgBase, Opts) ->
    Wallet = hb:wallet(),
    UncommittedBase = hb_message:uncommitted(MsgBase, Opts),
    Req =
        hb_message:commit(
            #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> =>
                    hb_message:commit(
                        UncommittedBase#{
                            <<"type">> => <<"Message">>,
                            <<"test-label">> => Text
                        },
                        Opts#{ priv_wallet => Wallet }
                    )
            },
			Opts#{ priv_wallet => Wallet }
        ),
    {ok, _} = hb_ao:resolve(Base, Req, Opts).

schedule_aos_call(Base, Code) ->
    schedule_aos_call(Base, Code, #{}).
schedule_aos_call(Base, Code, Opts) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    ProcID = hb_message:id(Base, all),
    Req =
        hb_message:commit(
            #{
                <<"action">> => <<"Eval">>,
                <<"data">> => Code,
                <<"target">> => ProcID
            },
            Opts#{ priv_wallet => Wallet }
        ),
    schedule_test_message(Base, <<"TEST MSG">>, Req, Opts).

schedule_wasm_call(Base, FuncName, Params) ->
    schedule_wasm_call(Base, FuncName, Params, #{}).
schedule_wasm_call(Base, FuncName, Params, Opts) ->
    Wallet = hb:wallet(),
    Req = 
        hb_message:commit(
            #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> =>
                    hb_message:commit(
                        #{
                            <<"type">> => <<"Message">>,
                            <<"function">> => FuncName,
                            <<"parameters">> => Params
                        },
                        Opts#{ priv_wallet => Wallet }
                    )
            },
            Opts#{ priv_wallet => Wallet }
        ),
    ?assertMatch({ok, _}, hb_ao:resolve(Base, Req, Opts)).

schedule_on_process_test_() ->
	{timeout, 30, fun()->
		init(),
		Base = aos_process(),
		schedule_test_message(Base, <<"TEST TEXT 1">>, #{}),
		schedule_test_message(Base, <<"TEST TEXT 2">>, #{}),
		?event(messages_scheduled),
		{ok, SchedulerRes} =
			hb_ao:resolve(Base, #{
				<<"method">> => <<"GET">>,
				<<"path">> => <<"schedule">>
			}, #{}),
		?assertMatch(
			<<"TEST TEXT 1">>,
			hb_ao:get(<<"assignments/0/body/test-label">>, SchedulerRes)
		),
		?assertMatch(
			<<"TEST TEXT 2">>,
			hb_ao:get(<<"assignments/1/body/test-label">>, SchedulerRes)
		)
	end}.

get_scheduler_slot_test() ->
    init(),
    Base = base_process(),
    schedule_test_message(Base, <<"TEST TEXT 1">>, #{}),
    schedule_test_message(Base, <<"TEST TEXT 2">>, #{}),
    Req = #{
        <<"path">> => <<"slot">>,
        <<"method">> => <<"GET">>
    },
    ?assertMatch(
        {ok, #{ <<"current">> := CurrentSlot }} when CurrentSlot > 0,
        hb_ao:resolve(Base, Req, #{})
    ).

recursive_path_resolution_test() ->
    init(),
    Base = base_process(),
    schedule_test_message(Base, <<"TEST TEXT 1">>, #{}),
    CurrentSlot =
        hb_ao:resolve(
            Base,
            #{ <<"path">> => <<"slot/current">> },
            #{ <<"hashpath">> => ignore }
        ),
    ?event({resolved_current_slot, CurrentSlot}),
    ?assertMatch(
        CurrentSlot when CurrentSlot > 0,
        CurrentSlot
    ),
    ok.

test_device_compute_test() ->
    init(),
    Base = test_process(),
    schedule_test_message(Base, <<"TEST TEXT 1">>, #{}),
    schedule_test_message(Base, <<"TEST TEXT 2">>, #{}),
    ?assertMatch(
        {ok, <<"TEST TEXT 2">>},
        hb_ao:resolve(
            Base,
            <<"schedule/assignments/1/body/test-label">>,
            #{}
        )
    ),
    Req = #{ <<"path">> => <<"compute">>, <<"slot">> => 1 },
    {ok, Res} = hb_ao:resolve(Base, Req, #{}),
    ?event({computed_message, {res, Res}}),
    ?assertEqual(1, hb_ao:get(<<"results/assignment-slot">>, Res, #{})),
    ?assertEqual([1,1,0,0], hb_ao:get(<<"already-seen">>, Res, #{})).

wasm_compute_test() ->
    init(),
    Base = wasm_process(<<"test/test-64.wasm">>),
    schedule_wasm_call(Base, <<"fac">>, [2.0]),
    schedule_wasm_call(Base, <<"fac">>, [3.0]),
    schedule_wasm_call(Base, <<"fac">>, [4.0]),
    schedule_wasm_call(Base, <<"fac">>, [5.0]),
    schedule_wasm_call(Base, <<"fac">>, [6.0]),
    {ok, _} = 
        hb_ao:resolve(
            Base,
            #{ <<"path">> => <<"compute">>, <<"slot">> => 3 },
            #{}
        ),
    {ok, _} = 
        hb_ao:resolve(
            Base,
            #{ <<"path">> => <<"compute">>, <<"slot">> => 3 },
            #{}
        ),
    {ok, _} = 
       hb_ao:resolve(
            Base,
            #{ <<"path">> => <<"compute">>, <<"slot">> => 1 },
            #{}
        ),
    {ok, _} = 
        hb_ao:resolve(
            Base,
            #{ <<"path">> => <<"compute">>, <<"slot">> => 2 },
            #{}
        ),
    {ok, _} = 
        hb_ao:resolve(
            Base,
            #{ <<"path">> => <<"compute">>, <<"slot">> => 4 },
            #{}
        ),
    ok.
    % ?assertEqual([24.0], hb_ao:get(<<"results/output">>, Slot2Res, #{})),
    % ?assertEqual([2.0], hb_ao:get(<<"results/output">>, Slot0Res, #{})),
    % ?assertEqual([6.0], hb_ao:get(<<"results/output">>, Slot1Res, #{})).

wasm_compute_from_id_test() ->
    init(),
    Opts = #{ cache_control => <<"always">> },
    Base = wasm_process(<<"test/test-64.wasm">>),
    schedule_wasm_call(Base, <<"fac">>, [5.0], Opts),
    BaseID = hb_message:id(Base, all),
    Req = #{ <<"path">> => <<"compute">>, <<"slot">> => 0 },
    {ok, Res} = hb_ao:resolve(BaseID, Req, Opts),
    ?event(process_compute, {computed_message, {res, Res}}),
    ?assertEqual([120.0], hb_ao:get(<<"results/output">>, Res, Opts)).

http_wasm_process_by_id_test() ->
    rand:seed(default),
    SchedWallet = ar_wallet:new(),
    Node = hb_http_server:start_node(Opts = #{
        port => 10000 + rand:uniform(10000),
        priv_wallet => SchedWallet,
        cache_control => <<"always">>,
        process_async_cache => false,
        store => #{
            <<"store-module">> => hb_store_fs,
            <<"name">> => <<"cache-mainnet">>
        }
    }),
    Wallet = ar_wallet:new(),
    Proc = wasm_process(<<"test/test-64.wasm">>, Opts),
    hb_cache:write(Proc, Opts),
    ProcID = hb_util:human_id(hb_message:id(Proc, all)),
    InitRes =
        hb_http:post(
            Node,
            << "/schedule" >>,
            Proc,
            #{}
        ),
    ?event({schedule_proc_res, InitRes}),
    ExecMsg =
        hb_message:commit(
            #{
                <<"target">> => ProcID,
                <<"type">> => <<"Message">>,
                <<"function">> => <<"fac">>,
                <<"parameters">> => [5.0]
            },
            #{ priv_wallet => Wallet }
        ),
    {ok, Res} = hb_http:post(Node, << ProcID/binary, "/schedule">>, ExecMsg, #{}),
    ?event({schedule_msg_res, {res, Res}}),
    {ok, Msg4} =
        hb_http:get(
            Node,
            #{
                <<"path">> => << ProcID/binary, "/compute">>,
                <<"slot">> => 1
            },
            #{}
        ),
    ?event({compute_msg_res, {msg4, Msg4}}),
    ?assertEqual([120.0], hb_ao:get(<<"results/output">>, Msg4, #{})).

aos_compute_test_() ->
    {timeout, 30, fun() ->
        init(),
        Base = aos_process(),
        schedule_aos_call(Base, <<"return 1+1">>),
        schedule_aos_call(Base, <<"return 2+2">>),
        Req = #{ <<"path">> => <<"compute">>, <<"slot">> => 0 },
        {ok, Res1} = hb_ao:resolve(Base, Req, #{}),
        {ok, Res2} = hb_ao:resolve(Res1, <<"results">>, #{}),
        ?event({computed_message, {res2, Res2}}),
        {ok, Data} = hb_ao:resolve(Res2, <<"data">>, #{}),
        ?event({computed_data, Data}),
        ?assertEqual(<<"2">>, Data),
        Msg4 = #{ <<"path">> => <<"compute">>, <<"slot">> => 1 },
        {ok, Res3} = hb_ao:resolve(Base, Msg4, #{}),
        ?assertEqual(<<"4">>, hb_ao:get(<<"results/data">>, Res3, #{})),
        {ok, Res3}
    end}.

aos_browsable_state_test_() ->
    {timeout, 30, fun() ->
        init(),
        Opts = #{ process_async_cache => false },
        Base = aos_process(Opts),
        schedule_aos_call(
            Base,
            <<"table.insert(ao.outbox.Messages, { target = ao.id, ",
                "action = \"State\", ",
                "data = { deep = 4, bool = true } })">>,
            Opts
        ),
        Req = #{ <<"path">> => <<"compute">>, <<"slot">> => 0 },
        {ok, Res} =
            hb_ao:resolve_many(
                [Base, Req, <<"results">>, <<"outbox">>, 1, <<"data">>, <<"deep">>],
                Opts#{ cache_control => <<"always">> }
            ),
        ID = hb_message:id(Base),
        ?event({computed_message, {id, {explicit, ID}}}),
        ?assertEqual(4, Res)
    end}.

aos_state_access_via_http_test_() ->
    {timeout, 60, fun() ->
        rand:seed(default),
        Wallet = ar_wallet:new(),
        Node = hb_http_server:start_node(Opts = #{
            port => 10000 + rand:uniform(10000),
            priv_wallet => Wallet,
            cache_control => <<"always">>,
            store => hb_test_utils:test_store(),
            force_signed_requests => true
        }),
        Proc = aos_process(Opts),
        ProcID = hb_util:human_id(hb_message:id(Proc, all)),
        {ok, _InitRes} = hb_http:post(Node, <<"/schedule">>, Proc, Opts),
        Req = 
            hb_message:commit(
                #{
                    <<"data-protocol">> => <<"ao">>,
                    <<"variant">> => <<"ao.N.1">>,
                    <<"type">> => <<"Message">>,
                    <<"action">> => <<"Eval">>,
                    <<"data">> =>
                        <<"table.insert(ao.outbox.Messages, { target = ao.id,",
                            " action = \"State\", data = { ",
                                "[\"content-type\"] = \"text/html\", ",
                                "[\"body\"] = \"<h1>Hello, world!</h1>\"",
                            "}})">>,
                    <<"target">> => ProcID
                },
                #{ priv_wallet => Wallet }
            ),
        {ok, Res} = hb_http:post(Node, << ProcID/binary, "/schedule">>, Req, Opts),
        ?event({schedule_msg_res, {res, Res}}),
        {ok, Msg4} =
            hb_http:get(
                Node,
                #{
                    <<"path">> => << ProcID/binary, "/compute/results/outbox/1/data" >>,
                    <<"slot">> => 1
                },
                Opts
            ),
        ?event({compute_msg_res, {msg4, Msg4}}),
        ?event(
            {try_yourself,
                {explicit,
                    <<
                        Node/binary,
                        "/",
                        ProcID/binary,
                        "/compute&slot=1/results/outbox/1/data"
                    >>
                }
            }
        ),
        ?assertMatch(#{ <<"body">> := <<"<h1>Hello, world!</h1>">> }, Msg4),
        ok
    end}.

aos_state_patch_test_() ->
    {timeout, 30, fun() ->
        Wallet = hb:wallet(),
        init(),
        BaseRaw = aos_process(#{}, [
            <<"wasi@1.0">>,
            <<"json-iface@1.0">>,
            <<"wasm-64@1.0">>,
            <<"patch@1.0">>,
            <<"multipass@1.0">>
        ]),
        {ok, Base} = hb_message:with_only_committed(BaseRaw, #{}),
        ProcID = hb_message:id(Base, all),
        InnerReq = 
            hb_message:commit(
                #{
                    <<"data-protocol">> => <<"ao">>,
                    <<"variant">> => <<"ao.N.1">>,
                    <<"target">> => ProcID,
                    <<"type">> => <<"Message">>,
                    <<"action">> => <<"Eval">>,
                    <<"data">> =>
                        <<
                            "table.insert(ao.outbox.Messages, "
                                "{ method = \"PATCH\", x = \"banana\" })"
                        >>
                },
                #{ priv_wallet => Wallet }
            ),
        Req = InnerReq#{
            <<"path">> => <<"schedule">>,
            <<"method">> => <<"POST">> 
        },
        {ok, _} = hb_ao:resolve(Base, Req, #{}),
        Res = #{ <<"path">> => <<"compute">>, <<"slot">> => 0 },
        {ok, Msg4} = hb_ao:resolve(Base, Res, #{}),
        ?event({computed_message, {res, Msg4}}),
        {ok, Data} = hb_ao:resolve(Msg4, <<"x">>, #{}),
        ?event({computed_data, Data}),
        ?assertEqual(<<"banana">>, Data)
    end}.

%% @doc Manually test state restoration without using the cache.
restore_test_() -> {timeout, 30, fun do_test_restore/0}.

do_test_restore() ->
    % Init the process and schedule 3 messages:
    % 1. Set variables in Lua.
    % 2. Return the variable.
    % Execute the first computation, then the second as a disconnected process.
    Opts = #{ process_cache_frequency => 1, process_async_cache => false },
    init(),
    Base = aos_process(Opts),
    schedule_aos_call(Base, <<"X = 42">>, Opts),
    schedule_aos_call(Base, <<"X = 1337">>, Opts),
    schedule_aos_call(Base, <<"return X">>, Opts),
    % Compute the first message.
    {ok, _} =
        hb_ao:resolve(
            Base,
            #{ <<"path">> => <<"compute">>, <<"slot">> => 1 },
            Opts
        ),
    {ok, ResultB} =
        hb_ao:resolve(
            Base,
            #{ <<"path">> => <<"compute">>, <<"slot">> => 2 },
            Opts
        ),
    ?event({result_b, ResultB}),
    ?assertEqual(<<"1337">>, hb_ao:get(<<"results/data">>, ResultB, #{})).

now_results_test_() ->
    {timeout, 30, fun() ->
        init(),
        Base = aos_process(),
        schedule_aos_call(Base, <<"return 1+1">>),
        schedule_aos_call(Base, <<"return 2+2">>),
        ?assertEqual({ok, <<"4">>}, hb_ao:resolve(Base, <<"now/results/data">>, #{}))
    end}.

prior_results_accessible_test_() ->
		{timeout, 30, fun() ->
			init(),
	        Opts = #{
	            process_async_cache => false
	        },
			Base = aos_process(Opts),
			schedule_aos_call(Base, <<"return 1+1">>, Opts),
			schedule_aos_call(Base, <<"return 2+2">>, Opts),
			?assertEqual(
	            {ok, <<"4">>},
	            hb_ao:resolve(Base, <<"now/results/data">>, Opts)
        ),
        {ok, Results} = 
            hb_ao:resolve(
                Base,
                #{ <<"path">> => <<"compute">>, <<"slot">> => 1 },
                Opts
            ),
		?assertMatch(
            #{ <<"results">> := #{ <<"data">> := <<"4">> } },
            hb_cache:ensure_all_loaded(Results, Opts)
		)
	end}.

persistent_process_test() ->
    {timeout, 30, fun() ->
        init(),
        Base = aos_process(),
        schedule_aos_call(Base, <<"X=1">>),
        schedule_aos_call(Base, <<"return 2">>),
        schedule_aos_call(Base, <<"return X">>),
        T0 = hb:now(),
        FirstSlotReq = #{
            <<"path">> => <<"compute">>,
            <<"slot">> => 0
        },
        ?assertMatch(
            {ok, _},
            hb_ao:resolve(Base, FirstSlotReq, #{ spawn_worker => true })
        ),
        T1 = hb:now(),
        ThirdSlotReq = #{
            <<"path">> => <<"compute">>,
            <<"slot">> => 2
        },
        Res = hb_ao:resolve(Base, ThirdSlotReq, #{}),
        ?event({computed_message, {res, Res}}),
        ?assertMatch(
            {ok, _},
            Res
        ),
        T2 = hb:now(),
        ?event(benchmark, {runtimes, {first_run, T1 - T0}, {second_run, T2 - T1}}),
        % The second resolve should be much faster than the first resolve, as the
        % process is already running.
        ?assert(T2 - T1 < ((T1 - T0)/2))
    end}.

simple_wasm_persistent_worker_benchmark_test() ->
    init(),
    BenchTime = 0.05,
    Base = wasm_process(<<"test/test-64.wasm">>),
    schedule_wasm_call(Base, <<"fac">>, [5.0]),
    schedule_wasm_call(Base, <<"fac">>, [6.0]),
    {ok, Initialized} = 
        hb_ao:resolve(
            Base,
            #{ <<"path">> => <<"compute">>, <<"slot">> => 1 },
            #{ spawn_worker => true, process_workers => true }
        ),
    Iterations = hb_test_utils:benchmark(
        fun(Iteration) ->
            schedule_wasm_call(
                Initialized,
                <<"fac">>,
                [5.0]
            ),
            ?assertMatch(
                {ok, _},
                hb_ao:resolve(
                    Initialized,
                    #{ <<"path">> => <<"compute">>, <<"slot">> => Iteration + 1 },
                    #{}
                )
            )
        end,
        BenchTime
    ),
    ?event(benchmark, {scheduled, Iterations}),
    hb_format:eunit_print(
        "Scheduled and evaluated ~p simple wasm process messages in ~p s (~s msg/s)",
        [Iterations, BenchTime, hb_util:human_int(Iterations / BenchTime)]
    ),
    ?assert(Iterations >= 1),
    ok.

aos_persistent_worker_benchmark_test_() ->
    {timeout, 30, fun() ->
        BenchTime = 0.25,
        init(),
        Base = aos_process(),
        schedule_aos_call(Base, <<"X=1337">>),
        FirstSlotReq = #{
            <<"path">> => <<"compute">>,
            <<"slot">> => 0
        },
        ?assertMatch(
            {ok, _},
            hb_ao:resolve(Base, FirstSlotReq, #{ spawn_worker => true })
        ),
        Iterations = hb_test_utils:benchmark(
            fun(Iteration) ->
                schedule_aos_call(
                    Base,
                    <<"return X + ", (integer_to_binary(Iteration))/binary>>
                ),
                ?assertMatch(
                    {ok, _},
                    hb_ao:resolve(
                        Base,
                        #{ <<"path">> => <<"compute">>, <<"slot">> => Iteration },
                        #{}
                    )
                )
            end,
            BenchTime
        ),
        ?event(benchmark, {scheduled, Iterations}),
        hb_format:eunit_print(
            "Scheduled and evaluated ~p AOS process messages in ~p s (~s msg/s)",
            [Iterations, BenchTime, hb_util:human_int(Iterations / BenchTime)]
        ),
        ?assert(Iterations >= 1),
        ok
    end}.
