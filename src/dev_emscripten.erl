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
-export([info/1, init/3
    ,'__cxa_throw'/3, '__cxa_rethrow'/3, '_emscripten_throw_longjmp'/3
    ,invoke_v/3, invoke_ii/3, invoke_jj/3, invoke_jjj/3, invoke_vjj/3, invoke_vjjj/3, invoke_vii/3, invoke_viii/3, invoke_iii/3, invoke_iiii/3, router/4
    ,'__cxa_find_matching_catch_3'/3, '__cxa_begin_catch'/3
    ,'_emscripten_memcpy_js'/3
    ,emscripten_date_now/3
    ,'__asyncjs__weavedrive_open'/3, '__asyncjs__weavedrive_read'/3, '__asyncjs__weavedrive_close'/3
]).

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

'__cxa_throw'(_Msg1, _Msg2, _Opts) ->
    Res = {import_exception, {<<"env">>, <<"__cxa_throw">>}},
    ?event(Res),
    {error, Res}.

'__cxa_rethrow'(_Msg1, _Msg2, _Opts) ->
    Res = {import_exception, {<<"env">>, <<"__cxa_throw">>}},
    ?event(Res),
    {error, Res}.

'_emscripten_throw_longjmp'(_Msg1, _Msg2, _Opts) ->
    Res = {import_exception, {<<"env">>, <<"_emscripten_throw_longjmp">>}},
    ?event(Res),
    {error, Res}.

invoke_v(Msg1, Msg2, Opts) ->
	?event(invoke_emscripten_v),
	router(<<"invoke_v">>, Msg1, Msg2, Opts).

invoke_ii(Msg1, Msg2, Opts) ->
	?event(invoke_emscripten_ii),
	router(<<"invoke_ii">>, Msg1, Msg2, Opts).

invoke_jj(Msg1, Msg2, Opts) ->
	?event(invoke_emscripten_jj),
	router(<<"invoke_jj">>, Msg1, Msg2, Opts).

invoke_jjj(Msg1, Msg2, Opts) ->
	?event(invoke_emscripten_jjj),
	router(<<"invoke_jjj">>, Msg1, Msg2, Opts).

invoke_vjjj(Msg1, Msg2, Opts) ->
	?event(invoke_emscripten_vjjj),
	router(<<"invoke_vjjj">>, Msg1, Msg2, Opts).

invoke_vjj(Msg1, Msg2, Opts) ->
	?event(invoke_emscripten_vjj),
	router(<<"invoke_vjj">>, Msg1, Msg2, Opts).

invoke_vii(Msg1, Msg2, Opts) ->
	?event(invoke_emscripten_vii),
	router(<<"invoke_vii">>, Msg1, Msg2, Opts).

invoke_viii(Msg1, Msg2, Opts) ->
	?event(invoke_emscripten_viii),
	router(<<"invoke_viii">>, Msg1, Msg2, Opts).

invoke_iii(Msg1, Msg2, Opts) ->
	?event(invoke_emscripten_iii),
	router(<<"invoke_iii">>, Msg1, Msg2, Opts).

invoke_iiii(Msg1, Msg2, Opts) ->
	?event(invoke_emscripten_iiii),
	router(<<"invoke_iiii">>, Msg1, Msg2, Opts).

router(<<"invoke_", Sig/binary>>, Msg1, Msg2, Opts) ->
    ?event(invoke_emscripten),
    State = hb_ao:get(<<"state">>, Msg1, #{ hashpath => ignore }),
    WASM = dev_wasm:instance(State, Msg2, Opts),
    [Index|ArgsRaw] = hb_ao:get(args, Msg2, #{ hashpath => ignore }),
    % If Args is a list, keep it, otherwise wrap it in one
    Args = case is_list(ArgsRaw) of
        true -> ArgsRaw;
        false -> [ArgsRaw]
    end,
    ?event(debug, {invoke, {invoke_signature, Sig}, {indirect_function_index, Index}, {args, Args}}),
    ?event(debug, invoke_emscripten_stack_get_current),
    {ok, [SP]} = dev_wasm:call(WASM, <<"emscripten_stack_get_current">>, []),
    ?event(debug, {invoke_stack_pointer, SP}),
    ?event({calling_import_resolver, {state, State}, {opts, Opts}}),
    ImportResolver = hb_private:get(<<"wasm/import-resolver">>, State, Opts),
    ?event({import_resolver, ImportResolver}),
    try
        ?event({trying_indirect_call, {index, Index}, {args, Args}}),
        Res = dev_wasm:call(WASM, Index, Args, ImportResolver, State, Opts),
        {ok, Result, _ResType, StateMsg} = Res,
        ?event(debug, {try_indirect_call_succeeded, Result}),
        {ok, #{ <<"state">> => StateMsg, <<"results">> => Result }}
    catch
        _:Error:Stack ->
            ?event(debug, {invoke_try_error, Error, Stack}),
            ?event(debug, cancelling_call),
            hb_wtime:call_cancel(WASM, <<"env">>, <<"_emscripten_throw_longjmp">>),
            ?event(debug, {calling_emscripten_stack_restore, {sp, SP}}),
            dev_wasm:call(WASM, <<"_emscripten_stack_restore">>, [SP], ImportResolver, State, Opts),
            ?event(debug, calling_set_threw),
            SetThrewRes = dev_wasm:call(WASM, <<"setThrew">>, [1, 0], ImportResolver, State, Opts),
            ?event(debug, {invoke_set_threw, {catch_result, SetThrewRes}}),
            {ok, _SetThrewResult, _SetThrewResType, SetThrewMsg} = SetThrewRes,
            % ?event(debug, set_threw_done),
            % {ok, SetThrewResult, SetThrewMsg} = SetThrewRes,
            % return empty result for invoke call
            {ok, #{ <<"state">> => SetThrewMsg, <<"results">> => [] }}
    end.

'__cxa_find_matching_catch_3'(Msg1, _Msg2, Opts) ->
    State = hb_ao:get(<<"state">>, Msg1, #{ hashpath => ignore }),
    ?event(debug, {cxa_find_matching_catch_3, {state, State}, {opts, Opts}}),
    {ok, #{ <<"state">> => State, <<"results">> => [1] }}.

'__cxa_begin_catch'(Msg1, _Msg2, Opts) ->
    State = hb_ao:get(<<"state">>, Msg1, #{ hashpath => ignore }),
    ?event(debug, {cxa_begin_catch, {state, State}, {opts, Opts}}),
    {ok, #{ <<"state">> => State, <<"results">> => [1] }}.

'_emscripten_memcpy_js'(Msg1, Msg2, Opts) ->
    ?event('_emscripten_memcpy_js'),
    State = hb_ao:get(<<"state">>, Msg1, #{ hashpath => ignore }),
    WASM = dev_wasm:instance(State, Msg2, Opts),
    [Dst, Src, Size] = hb_ao:get(args, Msg2, #{ hashpath => ignore }),
    ?event(debug, {memcpy, {dst, Dst}, {src, Src}, {size, Size}}),
    {ok, Data} = hb_wtime:mem_read(WASM, Src, Size),
    ok = hb_wtime:mem_write(WASM, Dst, Data),
    {ok, #{ <<"state">> => State, <<"results">> => [] }}.

% Return 0 (as a double)
emscripten_date_now(Msg1, _Msg2, _Opts) ->
    ?event(emscripten_date_now),
    State = hb_ao:get(<<"state">>, Msg1, #{ hashpath => ignore }),
    Result = 0.0,
    ?event(debug, {emscripten_date_now, {result, Result}}),
    {ok, #{ <<"state">> => State, <<"results">> => [Result] }}.

'__asyncjs__weavedrive_open'(Msg1, _Msg2, Opts) ->
    State = hb_ao:get(<<"state">>, Msg1, #{ hashpath => ignore }),
    ?event(debug, {weavedrive_open, {state, State}, {opts, Opts}}),
    {ok, #{ <<"state">> => State, <<"results">> => [0] }}.

'__asyncjs__weavedrive_read'(Msg1, _Msg2, Opts) ->
    State = hb_ao:get(<<"state">>, Msg1, #{ hashpath => ignore }),
    ?event(debug, {weavedrive_read, {state, State}, {opts, Opts}}),
    {ok, #{ <<"state">> => State, <<"results">> => [0] }}.

'__asyncjs__weavedrive_close'(Msg1, _Msg2, Opts) ->
    State = hb_ao:get(<<"state">>, Msg1, #{ hashpath => ignore }),
    ?event(debug, {weavedrive_close, {state, State}, {opts, Opts}}),
    {ok, #{ <<"state">> => State, <<"results">> => [0] }}.

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

% 0 - No catch cause triggered
% 1 - `throw` exception is caught
% 2 - `throw(int)` exception is caught
% 3 - `throw(runtime_error)` exception is caught
% 4 - native dereferencing an invalid pointer cannot be caught
try_test_case(Mod, N) -> 
    Init = generate_emscripten_stack(Mod, <<"handle">>, [0, 0]),
    Instance = hb_private:get(<<"wasm/instance">>, Init, #{}),
    Msg = <<"msg">>,
    % int to binary
    Env = list_to_binary(integer_to_list(N)),
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
    Output.

try_nothing_test() ->
    Output32 = try_test_case("test/try.wasm", 0),
    ?assertEqual(<<"Initial">>, Output32),
    Output64 = try_test_case("test/try-64.wasm", 0),
    ?assertEqual(<<"Initial">>, Output64).

% `throw` exception is caught
try_throw_test() ->
    Output32 = try_test_case("test/try.wasm", 1),
    ?assertEqual(<<"Catch">>, Output32),
    Output64 = try_test_case("test/try-64.wasm", 1),
    ?assertEqual(<<"Catch">>, Output64).

% `throw(int)` exception is caught
try_throw_int_test() ->
    % Output32 = try_test_case("test/try.wasm", 2),
    % ?assertEqual(<<"Catch">>, Output32),
    Output64 = try_test_case("test/try-64.wasm", 2),
    ?assertEqual(<<"Catch">>, Output64).

% `throw(runtime_error)` exception is caught
try_throw_runtime_error_test() ->
    Output32 = try_test_case("test/try.wasm", 3),
    ?assertEqual(<<"Catch">>, Output32),
    Output64 = try_test_case("test/try-64.wasm", 3),
    ?assertEqual(<<"Catch">>, Output64).

% native dereferencing an invalid pointer cannot be caught
try_throw_invalid_pointer_test() ->
    try
        try_test_case("test/try.wasm", 4)
    catch
        _:Error ->
            {badmatch, {error, {call_begin_failed, ErrorStrBin}}} = Error,
            ?event(debug, {invoke_try_error, ErrorStrBin})
    end,
    try
        try_test_case("test/try-64.wasm", 4)
    catch
        _:Error2 ->
            {badmatch, {error, {call_begin_failed, ErrorStrBin2}}} = Error2,
            ?event(debug, {invoke_try_error, ErrorStrBin2})
    end.

jmp_test() -> 
    Init = generate_emscripten_stack("test/jmp.wasm", <<"handle">>, [0, 0]),
    Instance = hb_private:get(<<"wasm/instance">>, Init, #{}),
    Msg = <<"msg">>,
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
    ?assertEqual(<<"last_level: 5, ret: 42, local_state: 7">>, Output).

%% @doc Ensure that an AOS Emscripten-style WASM AOT module can be invoked
%% with a function reference.
basic_aos_exec_test_() ->
    {timeout, 30, fun () ->
        Init = generate_emscripten_stack("test/aos-dock.wasm", <<"handle">>, []),
        Msg = gen_test_aos_msg("return 5 * 3"),
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
        {ok, Res} = hb_wtime_io:read_string(Instance, Ptr),
        % io:format("Output: ~p~n", [Output]),
        ?event({got_result, Res}),
        #{ <<"response">> := #{ <<"Output">> := #{ <<"data">> := Output } } }
            = hb_json:decode(Res),
        ?assertEqual(<<"15">>, Output)
    end}.

%%% Test Helpers
gen_test_env() ->
    <<"{\"Process\":{\"Id\":\"AOS\",\"Owner\":\"FB\",\"Tags\":[{\"name\":\"Name\",\"value\":\"Thomas\"}, {\"name\":\"Authority\",\"value\":\"FOOBAR\"}]}}\0">>.

gen_test_aos_msg(Command) ->
    <<"{\"From\":\"FB\",\"Block-Height\":\"1\",\"Target\":\"AOS\",\"Owner\":\"FB\",\"Id\":\"1\",\"Module\":\"W\",\"Tags\":[{\"name\":\"Action\",\"value\":\"Eval\"}],\"Data\":\"", (list_to_binary(Command))/binary, "\"}\0">>.
