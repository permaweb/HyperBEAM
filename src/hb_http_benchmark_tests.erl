-module(hb_http_benchmark_tests).
-include("include/hb.hrl").

-include_lib("eunit/include/eunit.hrl").

%% Allows to decrease or increase expected performance based on the current
%% machine specification. A smaller number implies more operations expected
%% to be performed.
%% 1: 50% performance of Macbook Pro M2 Max
-define(PERFORMANCE_DIVIDER, 1).

% unsigned_resolve_benchmark_test() ->
%     BenchTime = 1,
%     URL = hb_http_server:start_node(#{force_signed => false}),
%     Iterations = hb:benchmark(
%         fun() ->
%             hb_http:post(URL,
%                 #{
%                     <<"path">> => <<"key1">>,
%                     <<"key1">> => #{<<"key2">> => <<"value1">>}
%                 },
%                 #{}
%             )
%         end,
%         BenchTime
%     ),
%     hb_util:eunit_print(
%         "Resolved ~p messages through AO-Core via HTTP in ~p seconds (~.2f msg/s)",
%         [Iterations, BenchTime, Iterations / BenchTime]
%     ),
%     ?assert(Iterations > 400 / ?PERFORMANCE_DIVIDER).

% parallel_unsigned_resolve_benchmark_test() ->
%     BenchTime = 1,
%     BenchWorkers = 16,
%     URL = hb_http_server:start_node(#{force_signed => false}),
%     Iterations = hb:benchmark(
%         fun(_Count) ->
%             hb_http:post(
%                 URL,
%                 #{
%                     <<"path">> => <<"key1">>,
%                     <<"key1">> => #{<<"key2">> => <<"value1">>}
%                 },
%                 #{}
%             )
%         end,
%         BenchTime,
%         BenchWorkers
%     ),
%     hb_util:eunit_print(
%         "Resolved ~p messages via HTTP (~p workers) in ~p seconds (~.2f msg/s)",
%         [Iterations, BenchWorkers, BenchTime, Iterations / BenchTime]
%     ),
%     ?assert(Iterations > 1000 / ?PERFORMANCE_DIVIDER).

% wasm_compute_request(ImageFile, Func, Params) ->
%     {ok, Bin} = file:read_file(ImageFile),
%     #{
%         <<"path">> => <<"init/compute/results">>,
%         <<"device">> => <<"wasm-64@1.0">>,
%         <<"function">> => Func,
%         <<"parameters">> => Params,
%         <<"image">> => Bin
%     }.

% run_wasm_unsigned_benchmark_test() ->
%     BenchTime = 1,
%     URL = hb_http_server:start_node(#{force_signed => false}),
%     Msg = wasm_compute_request(<<"test/test-64.wasm">>, <<"fac">>, [10]),
%     Iterations = hb:benchmark(
%         fun(_) ->
%             case hb_http:post(URL, Msg, #{}) of
%                 {ok, _} -> 1;
%                 _ -> 0
%             end
%         end,
%         BenchTime
%     ),
%     hb_util:eunit_print(
%         "Resolved ~p WASM invocations via HTTP in ~p seconds (~.2f msg/s)",
%         [Iterations, BenchTime, Iterations / BenchTime]
%     ),
%     ?assert(Iterations > 100 / ?PERFORMANCE_DIVIDER).

% run_wasm_signed_benchmark_test_disabled() ->
%     BenchTime = 1,
%     URL = hb_http_server:start_node(#{force_signed => true}),
%     Msg = wasm_compute_request(<<"test/test-64.wasm">>, <<"fac">>, [10]),
%     Iterations = hb:benchmark(
%         fun(_) ->
%             case hb_http:post(URL, Msg, #{}) of
%                 {ok, _} -> 1;
%                 _ -> 0
%             end
%         end,
%         BenchTime
%     ),
%     hb_util:eunit_print(
%         "Resolved ~p WASM invocations via HTTP in ~p seconds (~.2f msg/s)",
%         [Iterations, BenchTime, Iterations / BenchTime]
%     ),
%     ?assert(Iterations > 50 / ?PERFORMANCE_DIVIDER).

% parallel_wasm_unsigned_benchmark_test_disabled() ->
%     BenchTime = 1,
%     BenchWorkers = 16,
%     URL = hb_http_server:start_node(#{force_signed => false}),
%     Msg = wasm_compute_request(<<"test/test-64.wasm">>, <<"fac">>, [10]),
%     Iterations = hb:benchmark(
%         fun(X) ->
%             ?event({post_start, X}),
%             case hb_http:post(URL, Msg, #{}) of
%                 {ok, _} ->
%                     1;
%                 _ -> 0
%             end
%         end,
%         BenchTime,
%         BenchWorkers
%     ),
%     hb_util:eunit_print(
%         "Resolved ~p WASM invocations via HTTP (~p workers) in ~p seconds (~.2f msg/s)",
%         [Iterations, BenchWorkers, BenchTime, Iterations / BenchTime]
%     ),
%     ?assert(Iterations > 200 / ?PERFORMANCE_DIVIDER).

% parallel_wasm_signed_benchmark_test_disabled() ->
%     BenchTime = 1,
%     BenchWorkers = 16,
%     URL = hb_http_server:start_node(#{force_signed => true}),
%     Msg = wasm_compute_request(<<"test/test-64.wasm">>, <<"fac">>, [10]),
%     Iterations = hb:benchmark(
%         fun(_) ->
%             case hb_http:post(URL, Msg, #{}) of
%                 {ok, _ResMsg} ->
%                     1;
%                 _ -> 0
%             end
%         end,
%         BenchTime,
%         BenchWorkers
%     ),
%     hb_util:eunit_print(
%         "Resolved ~p WASM invocations via HTTP (~p workers) in ~p seconds (~.2f msg/s)",
%         [Iterations, BenchWorkers, BenchTime, Iterations / BenchTime]
%     ),
%     ?assert(Iterations > 100 / ?PERFORMANCE_DIVIDER).

% % parallel_http_scheduling_benchmark_test() ->
% %     application:ensure_all_started(hb),
% %     URL = hb_http_server:start_node(#{force_signed => true}),
% %     BenchTime = 3,
% %     BenchWorkers = 16,
% %     Msg1 = dev_scheduler:test_process(),
% %     Proc = hb_ao:get(process, Msg1, #{ hashpath => ignore }),
% %     ProcID = hb_util:id(Proc),
% %     ?event({benchmark_start, ?MODULE}),
% %     Iterations = hb:benchmark(
% %         fun(X) ->
% %             MsgX = #{
% %                 <<"device">> => <<"Scheduler@1.0">>,
% %                 <<"path">> => <<"schedule">>,
% %                 <<"method">> => <<"POST">>,
% %                 <<"body">> =>
% %                     #{
% %                         <<"type">> => <<"body">>,
% %                         <<"test-val">> => X
% %                     }
% %             },
% %             Res = hb_http:post(URL, MsgX),
% %             ?event({post_result, Res}),
% %             case Res of
% %                 {ok, _} -> 1;
% %                 _ -> 0
% %             end
% %         end,
% %         BenchTime,
% %         BenchWorkers
% %     ),
% %     ?event(benchmark, {scheduled, Iterations}),
% %     Msg3 = #{
% %         <<"path">> => <<"slot">>,
% %         <<"method">> => <<"GET">>,
% %         <<"process">> => ProcID
% %     },
% %     Res = hb_http:post(URL, Msg3),
% %     ?event({slot_result, Res}),
% %     hb_util:eunit_print(
% %         "Scheduled ~p messages through AO-Core in ~p seconds (~.2f msg/s)",
% %         [Iterations, BenchTime, Iterations / BenchTime]
% %     ),
% %     ?assert(Iterations > 100).
