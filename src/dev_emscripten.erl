%%% @doc Implements the legacy/classic exception handling pattern of the 
%%% Emscripten WASM execution environment.
%%% 
%%% Emscripten has many subtly different ways of handling exceptions.
%%% For the avoidance of doubt, the pattern in question works as follows:
%%% 
%%% ```
%%% function invoke_vjj(index, a1, a2) {
%%%   var sp = stackSave();
%%%   try {
%%%     getWasmTableEntry(Number(index))(a1, a2);
%%%   } catch (e) {
%%%     stackRestore(sp);
%%%     if (e !== e + 0) throw e;
%%%     _setThrew(1, 0);
%%%   }
%%% }
%%% '''
%%% 
%%% Where '_vjj' represents the type spec of the function.
-module(dev_emscripten).
-export([info/1, init/3, '_emscripten_memcpy_js'/3, '__cxa_throw'/3, invoke_ii/3, invoke_vjj/3, invoke_viii/3, router/4, emscripten_date_now/3]).


-include("src/include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-hb_debug(print).

info(_) ->
    #{
        % default_handler => fun router/4,
        excludes => [keys, id, unsigned, hashpath]
    }.

%% @doc On-boot, initialize the virtual file system with:
%% - Empty stdio files
init(M1, _M2, Opts) ->
    ?event(init_emscripten),
    MsgWithLib =
        hb_ao:set(
            M1,
            #{
                <<"wasm/stdlib/env">> =>
                    #{ device => <<"emscripten@1.0">>}
            },
            Opts
        ),
    {ok, MsgWithLib}.

'_emscripten_memcpy_js'(Msg1, Msg2, Opts) ->
    ?event('_emscripten_memcpy_js'),
    State = hb_ao:get(<<"state">>, Msg1, #{ hashpath => ignore }),
    WASM = dev_wasm:instance(State, Msg2, Opts),
    [Dst, Src, Size] = hb_ao:get(args, Msg2, #{ hashpath => ignore }),
    ?event(debug, {memcpy, {dst, Dst}, {src, Src}, {size, Size}}),
    {ok, Data} = hb_wtime:mem_read(WASM, Src, Size),
    ok = hb_wtime:mem_write(WASM, Dst, Data),
    {ok, #{ <<"state">> => State, <<"results">> => [] }}.

'__cxa_throw'(_Msg1, _Msg2, _Opts) ->
    ?event('__cxa_throw'),
    {import_exception, <<"__cxa_throw">>}.

invoke_ii(Msg1, Msg2, Opts) ->
	?event(invoke_emscripten_ii),
	router(<<"invoke_ii">>, Msg1, Msg2, Opts).

invoke_vjj(Msg1, Msg2, Opts) ->
	?event(invoke_emscripten_vjj),
	router(<<"invoke_vjj">>, Msg1, Msg2, Opts).

invoke_viii(Msg1, Msg2, Opts) ->
	?event(invoke_emscripten_viii),
	router(<<"invoke_viii">>, Msg1, Msg2, Opts).

router(<<"invoke_", Sig/binary>>, Msg1, Msg2, Opts) ->
    ?event(invoke_emscripten),
    State = hb_ao:get(<<"state">>, Msg1, #{ hashpath => ignore }),
    WASM = dev_wasm:instance(State, Msg2, Opts),
    [Index|Args] = hb_ao:get(args, Msg2, #{ hashpath => ignore }),
    ?event(debug, {invoke, {invoke_signature, Sig}, {indirect_function_index, Index}, {args, Args}}),
    ?event(debug, invoke_emscripten_stack_get_current),
    {ok, [SP]} = dev_wasm:call(WASM, <<"emscripten_stack_get_current">>, []),
    ?event(debug, {invoke_stack_pointer, SP}),
    ImportResolver = hb_private:get(<<"wasm/import-resolver">>, State, Opts),
    try
        ?event(trying_indirect_call),
        Res = dev_wasm:call(WASM, Index, Args, ImportResolver, State, Opts),
        {ok, Result, StateMsg} = Res,
        ?event(debug, {try_indirect_call_succeeded, Result}),
        {ok, #{ <<"state">> => StateMsg, <<"results">> => Result }}
    catch
        _:Error ->
            ?event(debug, {invoke_try_error, Error}),
            ?event(debug, {calling_emscripten_stack_restore, {sp, SP}}),
            dev_wasm:call(WASM, <<"_emscripten_stack_restore">>, [SP], ImportResolver, State, Opts),
            ?event(debug, calling_set_threw),
            SetThrewRes = dev_wasm:call(WASM, <<"setThrew">>, [1, 0], ImportResolver, State, Opts),
            ?event(debug, {invoke_set_threw, {catch_result, SetThrewRes}}),
            % {ok, _, _} = SetThrewRes,
            ?event(debug, set_threw_done),
            {ok, SetThrewResult, SetThrewMsg} = SetThrewRes,
            {ok, #{ <<"state">> => SetThrewMsg, <<"results">> => SetThrewResult }}
    end.

% Return 0 (as a double)
emscripten_date_now(Msg1, _Msg2, _Opts) ->
    ?event(emscripten_date_now),
    State = hb_ao:get(<<"state">>, Msg1, #{ hashpath => ignore }),
    Result = 0.0,
    ?event(debug, {emscripten_date_now, {result, Result}}),
    {ok, #{ <<"state">> => State, <<"results">> => [Result] }}.

%%% Tests
init() ->
    application:ensure_all_started(hb).

generate_emscripten_stack(File, Func, Params) ->
    init(),
    Msg0 = dev_wasm:cache_wasm_image(File),
    Msg1 = Msg0#{
        <<"device">> => <<"stack@1.0">>,
        <<"device-stack">> => [<<"wasm-64@1.0">>, <<"wasi@1.0">>, <<"emscripten@1.0">>],
        <<"output-prefixes">> => [<<"wasm">>, <<"wasm">>],
        <<"stack-keys">> => [<<"init">>, <<"compute">>],
        <<"function">> => Func,
        <<"params">> => Params
    },
    {ok, Msg2} = hb_ao:resolve(Msg1, <<"init">>, #{}),
    Msg2.

%% @doc Ensure that an AOS Emscripten-style WASM AOT module can be invoked
%% with a function reference.
fib_test() ->
    Init = generate_emscripten_stack("test/fib.wasm", <<"handle">>, [0, 0]),
    Instance = hb_private:get(<<"wasm/instance">>, Init, #{}),
    Msg = <<"msg">>,
    Env = <<"env">>,
    {ok, Ptr1} = hb_wtime_io:malloc(Instance, byte_size(Msg)),
    ?assertNotEqual(0, Ptr1),
    hb_wtime:mem_write(Instance, Ptr1, Msg),
    {ok, Ptr2} = hb_wtime_io:malloc(Instance, byte_size(Env)),
    ?assertNotEqual(0, Ptr2),
    hb_wtime:mem_write(Instance, Ptr2, Env),
    Ready = Init#{ <<"parameters">> => [Ptr1, Ptr2] },
    {ok, StateRes} = hb_ao:resolve(Ready, <<"compute">>, #{}),
    [Ptr] = hb_ao:get(<<"results/wasm/output">>, StateRes),
    {ok, Output} = hb_wtime_io:read_string(Instance, Ptr),
    ?assertEqual(<<"Fibonacci index 10 is 55\n">>, Output).

%% @doc Ensure that an AOS Emscripten-style WASM AOT module can be invoked
%% with a function reference.
try_test() ->
    Init = generate_emscripten_stack("test/try.wasm", <<"handle">>, [0, 0]),
    Instance = hb_private:get(<<"wasm/instance">>, Init, #{}),
    Msg = <<"msg">>,
    % Note: set to <<"1">> to enable the try/catch path
    Env = <<"1">>,
    {ok, Ptr1} = hb_wtime_io:malloc(Instance, byte_size(Msg)),
    ?assertNotEqual(0, Ptr1),
    hb_wtime:mem_write(Instance, Ptr1, Msg),
    {ok, Ptr2} = hb_wtime_io:malloc(Instance, byte_size(Env)),
    ?assertNotEqual(0, Ptr2),
    hb_wtime:mem_write(Instance, Ptr2, Env),
    Ready = Init#{ <<"parameters">> => [Ptr1, Ptr2] },
    {ok, StateRes} = hb_ao:resolve(Ready, <<"compute">>, #{}),
    [Ptr] = hb_ao:get(<<"results/wasm/output">>, StateRes),
    {ok, Output} = hb_wtime_io:read_string(Instance, Ptr),
    ?assertEqual(<<"Initial">>, Output).

basic_aos_exec_test() ->
    Init = generate_emscripten_stack("test/aos-new.wasm", <<"handle">>, []),
    Msg = gen_test_aos_msg("return 1 + 1"),
    Env = gen_test_env(),
    Instance = hb_private:get(<<"wasm/instance">>, Init, #{}),
    {ok, Ptr1} = hb_wtime_io:malloc(Instance, byte_size(Msg)),
    ?assertNotEqual(0, Ptr1),
    hb_wtime:mem_write(Instance, Ptr1, Msg),
    {ok, Ptr2} = hb_wtime_io:malloc(Instance, byte_size(Env)),
    ?assertNotEqual(0, Ptr2),
    hb_wtime:mem_write(Instance, Ptr2, Env),
    % Read the strings to validate they are correctly passed
    {ok, MsgBin} = hb_wtime:mem_read(Instance, Ptr1, byte_size(Msg)),
    {ok, EnvBin} = hb_wtime:mem_read(Instance, Ptr2, byte_size(Env)),
    ?assertEqual(Env, EnvBin),
    ?assertEqual(Msg, MsgBin),
    Ready = Init#{ <<"parameters">> => [Ptr1, Ptr2] },
    {ok, StateRes} = hb_ao:resolve(Ready, <<"compute">>, #{}),
    [Ptr] = hb_ao:get(<<"results/wasm/output">>, StateRes),
    {ok, Output} = hb_wtime:mem_read_string(Instance, Ptr),
    ?event({got_output, Output}),
    #{ <<"response">> := #{ <<"Output">> := #{ <<"data">> := Data }} }
        = hb_json:decode(Output),
    ?assertEqual(<<"2">>, Data).

%%% Test Helpers
gen_test_env() ->
    <<"{\"Process\":{\"Id\":\"AOS\",\"Owner\":\"FOOBAR\",\"Tags\":[{\"name\":\"Name\",\"value\":\"Thomas\"}, {\"name\":\"Authority\",\"value\":\"FOOBAR\"}]}}\0">>.

gen_test_aos_msg(Command) ->
    <<"{\"From\":\"FOOBAR\",\"Block-Height\":\"1\",\"Target\":\"AOS\",\"Owner\":\"FOOBAR\",\"Id\":\"1\",\"Module\":\"W\",\"Tags\":[{\"name\":\"Action\",\"value\":\"Eval\"}],\"Data\":\"", (list_to_binary(Command))/binary, "\"}\0">>.
