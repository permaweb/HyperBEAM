
%%% @doc BEAMR: A WAMR wrapper for BEAM.
%%% 
%%% Beamr is a library that allows you to run WASM modules in BEAM, using the
%%% Webassembly Micro Runtime (WAMR) as its engine. Each WASM module is 
%%% executed using a Linked-In Driver (LID) that is loaded into BEAM. It is
%%% designed with a focus on supporting long-running WASM executions that 
%%% interact with Erlang functions and processes easily.
%%% 
%%% Because each WASM module runs as an independent async worker, if you plan
%%% to run many instances in parallel, you should be sure to configure the 
%%% BEAM to have enough async worker threads enabled (see `erl +A N' in the
%%% Erlang manuals).
%%% 
%%% The core API is simple:
%%% <pre>
%%%     start(WasmBinary) -> {ok, Port, Imports, Exports}
%%%         Where:
%%%             WasmBinary is the WASM binary to load.
%%%             Port is the port to the LID.
%%%             Imports is a list of tuples of the form {Module, Function,
%%%                 Args, Signature}.
%%%             Exports is a list of tuples of the form {Function, Args,
%%%                 Signature}.
%%%     stop(Port) -> ok
%%%     call(Port, FunctionName, Args) -> {ok, Result}
%%%         Where:
%%%             FunctionName is the name of the function to call.
%%%             Args is a list of Erlang terms (converted to WASM values by
%%%                 BEAMR) that match the signature of the function.
%%%             Result is a list of Erlang terms (converted from WASM values).
%%%     call(Port, FunName, Args[, Import, State, Opts]) -> {ok, Res, NewState}
%%%         Where:
%%%             ImportFun is a function that will be called upon each import.
%%%             ImportFun must have an arity of 2: Taking an arbitrary `state'
%%%             term, and a map containing the `port', `module', `func', `args',
%%%             `signature', and the `options' map of the import.
%%%             It must return a tuple of the form {ok, Response, NewState}.
%%%     serialize(Port) -> {ok, Mem}
%%%         Where:
%%%             Port is the port to the LID.
%%%             Mem is a binary representing the full WASM state.
%%%     deserialize(Port, Mem) -> ok
%%%         Where:
%%%             Port is the port to the LID.
%%%             Mem is a binary output of a previous `serialize/1' call.
%%% </pre>
%%% 
%%% BEAMR was designed for use in the HyperBEAM project, but is suitable for
%%% deployment in other Erlang applications that need to run WASM modules. PRs
%%% are welcome.
-module(hb_beamr).
%%% Control API:
-export([start/1, start/2, call/3, call/4, call/5, call/6, stop/1, wasm_send/2]).
%%% Utility API:
-export([serialize/1, deserialize/2, stub/3]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-hb_debug(print).

%% @doc Load the driver for the WASM executor.
load_driver() ->
    case erl_ddll:load(code:priv_dir(hb), ?MODULE) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, Error} -> {error, Error}
    end.

%% @doc Start a WASM executor context. Yields a port to the LID, and the
%% imports and exports of the WASM module. Optionally, specify a mode 
%% (wasm or aot) to indicate the type of WASM module being loaded.
start(WasmBinary) when is_binary(WasmBinary) ->
    start(WasmBinary, wasm).
start(WasmBinary, Mode) when is_binary(WasmBinary) ->
    ?event({loading_module, {bytes, byte_size(WasmBinary)}, Mode}),
    Self = self(),
    WASM = spawn(
        fun() ->
            ok = load_driver(),
            Port = open_port({spawn, "hb_beamr"}, []),
            Port ! {self(), {command, term_to_binary({init, WasmBinary, Mode})}},
            ?event({waiting_for_init_from, Port}),
            worker(Port, Self)
        end
    ),
    receive
        {execution_result} ->
            ?event({wasm_init_success}),
            {ok, WASM};
        {error, Error} ->
            ?event({wasm_init_error, Error}),
            stop(WASM),
            {error, Error}
    end.

%% @doc A worker process that is responsible for handling a WASM instance.
%% It wraps the WASM port, handling inputs and outputs from the WASM module.
%% The last sender to the port is always the recipient of its messages, so
%% be careful to ensure that there is only one active sender to the port at 
%% any time.
worker(Port, Listener) ->
    receive
        stop ->
            ?event({stop_invoked_for_beamr, self()}),
            case erlang:port_info(Port, id) of
                undefined ->
                    ok;
                _ ->
                    port_close(Port),
                    ok
            end,
            ok;
        {wasm_send, NewListener, Message} ->
            ?event({wasm_send, {listener, NewListener}, {message, Message}}),
            Port ! {self(), Message},
            worker(Port, NewListener);
        WASMResult ->
            ?event({wasm_result, {listener, Listener}, {result, WASMResult}}),
            Listener ! WASMResult,
            worker(Port, Listener)
    end.

wasm_send(WASM, Message) when is_pid(WASM) ->
    WASM ! {wasm_send, self(), Message},
    ok.

%% @doc Stop a WASM executor context.
stop(WASM) when is_pid(WASM) ->
    WASM ! stop,
    ok.

%% @doc Call a function in the WASM executor (see moduledoc for more details).
call(PID, FuncRef, Args) ->
    case call(PID, FuncRef, Args, fun stub/3) of 
        {ok, Res, _} ->
            {ok, Res};
        {error, Error, _} ->
            {error, Error}
    end.
call(PID, FuncRef, Args, ImportFun) ->
    call(PID, FuncRef, Args, ImportFun, #{}).
call(PID, FuncRef, Args, ImportFun, StateMsg) ->
    call(PID, FuncRef, Args, ImportFun, StateMsg, #{}).
call(PID, FuncRef, Args, ImportFun, StateMsg, Opts)
        when is_binary(FuncRef) ->
    call(PID, binary_to_list(FuncRef), Args, ImportFun, StateMsg, Opts);
call(WASM, FuncRef, Args, ImportFun, StateMsg, Opts) 
        when is_pid(WASM)
        andalso (is_list(FuncRef) or is_integer(FuncRef))
        andalso is_list(Args)
        andalso is_function(ImportFun)
        andalso is_map(Opts) ->
    case is_valid_arg_list(Args) of
        true ->
            ?event(
                {call_started,
                    WASM,
                    FuncRef,
                    Args,
                    ImportFun,
                    StateMsg,
                    Opts}),
            wasm_send(WASM,
                {command,
                    term_to_binary(
                        case is_integer(FuncRef) of
                            true -> {indirect_call, FuncRef, Args};
                            false -> {call, FuncRef, Args}
                        end
                    )
                }
            ),
            ?event({waiting_for_call_result, self(), WASM}),
            monitor_call(WASM, ImportFun, StateMsg, Opts);
        false ->
            {error, {invalid_args, Args}}
    end.

%% @doc Stub import function for the WASM executor.
stub(Msg1, _Msg2, _Opts) ->
    ?event(stub_stdlib_called),
    {ok, [0], Msg1}.

%% @doc Synchonously monitor the WASM executor for a call result and any
%% imports that need to be handled.
monitor_call(WASM, ImportFun, StateMsg, Opts) ->
    ?event(monitoring_call),
    receive
        {execution_result, Result} ->
            ?event({call_result, Result}),
            {ok, Result, StateMsg};
        {import, Module, Func, Args, Signature} ->
            ?event({import_called, Module, Func, Args, Signature}),
            try
                {ok, Res, StateMsg2} =
                    ImportFun(StateMsg,
                        #{
                            instance => WASM,
                            module => Module,
                            func => Func,
                            args => Args,
                            func_sig => Signature
                        },
                        Opts
                    ),
                ?event({import_ret, Module, Func, {args, Args}, {res, Res}}),
                dispatch_response(WASM, Res),
                monitor_call(WASM, ImportFun, StateMsg2, Opts)
            catch
                Err:Reason:Stack ->
                    % Signal the WASM executor to stop.
                    ?event({import_error, Err, Reason, Stack}),
                    stop(WASM),
                    % The driver is going to send us an error message, so we 
                    % need to clear it from the mailbox, even if we already 
                    % know that the import failed.
                    receive
                        {error, _} -> ok
                    %after 0 -> ok
                    end,
                    {error, Err, Reason, Stack, StateMsg}
            end;
        {error, Error} ->
            ?event({wasm_error, Error}),
            {error, Error, StateMsg};
        {error, Error, Extra} ->
            ?event({wasm_error, Error, Extra}),
            {error, Error, StateMsg}
    end.

%% @doc Check the type of an import response and dispatch it to a Beamr port.
dispatch_response(WASM, Term) when is_pid(WASM) ->
	case is_valid_arg_list(Term) of
		true ->
			wasm_send(WASM,
				{command, term_to_binary({import_response, Term})});
		false ->
			throw({error, {invalid_response, Term}})
	end;
dispatch_response(_WASM, Term) ->
	throw({error, {invalid_response, Term}}).

%% @doc Check that a list of arguments is valid for a WASM function call.
is_valid_arg_list(Args) when is_list(Args) ->
    lists:all(fun(Arg) -> is_integer(Arg) or is_float(Arg) end, Args);
is_valid_arg_list(_) ->
    false.

%% @doc Serialize the WASM state to a binary.
serialize(WASM) when is_pid(WASM) ->
    ?event(starting_serialize),
    {ok, Size} = hb_beamr_io:size(WASM),
    ?event({image_size, Size}),
    {ok, Mem} = hb_beamr_io:read(WASM, 0, Size),
    ?event({finished_serialize, byte_size(Mem)}),
    {ok, Mem}.

%% @doc Deserialize a WASM state from a binary.
deserialize(WASM, Bin) when is_pid(WASM) andalso is_binary(Bin) ->
    ?event(starting_deserialize),
    Res = hb_beamr_io:write(WASM, 0, Bin),
    ?event({finished_deserialize, Res}),
    ok.

%% Tests

driver_loads_test() ->
    ?assertEqual(ok, load_driver()).

%% @doc Test standalone `hb_beamr' correctly after loading a WASM module.
simple_wasm_test() ->
    {ok, File} = file:read_file("test/test.aot"),
    {ok, WASM, _Imports, _Exports} = start(File),
    {ok, [Result]} = call(WASM, "fac", [5.0]),
    ?assertEqual(120.0, Result).

-define(I32_MAX, 2147483647).
-define(U32_MAX, 4294967295).
-define(I64_MAX, 9223372036854775807).
-define(U64_MAX, 18446744073709551615).

send_int_test() ->
    {ok, File} = file:read_file("test/format.aot"),
    {ok, WASM} = start(File),
    % i32 - within i32 range ok
    I32Case0 = 0,
    ?event({i32_case0, I32Case0}),
    {ok, [I32StrPtr]} = call(WASM, "format_i32", [I32Case0]),
    {ok, I32Str0} = hb_beamr_io:read_string(WASM, I32StrPtr),
    ?assertEqual(<<"0">>, I32Str0),
    I32Case1 = ?I32_MAX,
    ?event({i32_case1, I32Case1}),
    {ok, [I32StrPtr]} = call(WASM, "format_i32", [I32Case1]),
    {ok, I32Str1} = hb_beamr_io:read_string(WASM, I32StrPtr),
    ?assertEqual(<<"2147483647">>, I32Str1),
    I32Case2 = -(?I32_MAX + 1),
    ?event({i32_case2, I32Case2}),
    {ok, [I32StrPtr]} = call(WASM, "format_i32", [I32Case2]),
    {ok, I32Str2} = hb_beamr_io:read_string(WASM, I32StrPtr),
    ?assertEqual(<<"-2147483648">>, I32Str2),
    % i32 - outside i32 range errors
    I32Case3 = -(?I32_MAX + 2),
    ?event({i32_case3, I32Case3}),
    {error, "Argument value out of range for wasm type"} = call(WASM, "format_i32", [I32Case3]),
    % u32 - within u32 range ok
    U32Case0 = 0,
    ?event({u32_case0, U32Case0}),
    {ok, [U32StrPtr]} = call(WASM, "format_u32", [U32Case0]),
    {ok, U32Str0} = hb_beamr_io:read_string(WASM, U32StrPtr),
    ?assertEqual(<<"0">>, U32Str0),
    U32Case1 = ?U32_MAX,
    ?event({u32_case1, U32Case1}),
    {ok, [U32StrPtr]} = call(WASM, "format_u32", [U32Case1]),
    {ok, U32Str1} = hb_beamr_io:read_string(WASM, U32StrPtr),
    ?assertEqual(<<"4294967295">>, U32Str1),
    % u32 - outside u32 range errors
    U32Case3 = (?U32_MAX + 1) + 255,
    ?event({u32_case3, U32Case3}),
    {error, "Argument value out of range for wasm type"} = call(WASM, "format_u32", [U32Case3]),
    % i64 - within i64 range ok
    I64Case0 = 0,
    ?event({i64_case0, I64Case0}),
    {ok, [I64StrPtr]} = call(WASM, "format_i64", [I64Case0]),
    {ok, I64Str0} = hb_beamr_io:read_string(WASM, I64StrPtr),
    ?assertEqual(<<"0">>, I64Str0),
    I64Case1 = ?I64_MAX,
    ?event({i64_case1, I64Case1}),
    {ok, [I64StrPtr]} = call(WASM, "format_i64", [I64Case1]),
    {ok, I64Str1} = hb_beamr_io:read_string(WASM, I64StrPtr),
    ?assertEqual(<<"9223372036854775807">>, I64Str1),
    I64Case2 = -(?I64_MAX + 1),
    ?event({i64_case2, I64Case2}),
    {ok, [I64StrPtr]} = call(WASM, "format_i64", [I64Case2]),
    {ok, I64Str2} = hb_beamr_io:read_string(WASM, I64StrPtr),
    ?assertEqual(<<"-9223372036854775808">>, I64Str2),
    % i64 - outside i64 range errors
    I64Case3 = -(?I64_MAX + 2),
    ?event({i64_case3, I64Case3}),
    {error, "Argument value out of range for wasm type"} = call(WASM, "format_i64", [I64Case3]),
    % u64 - within u64 range ok
    U64Case0 = 0,
    ?event({u64_case0, U64Case0}),
    {ok, [U64StrPtr]} = call(WASM, "format_u64", [U64Case0]),
    {ok, U64Str0} = hb_beamr_io:read_string(WASM, U64StrPtr),
    ?assertEqual(<<"0">>, U64Str0),
    U64Case1 = ?U64_MAX,
    ?event({u64_case1, U64Case1}),
    {ok, [U64StrPtr]} = call(WASM, "format_u64", [U64Case1]),
    {ok, U64Str1} = hb_beamr_io:read_string(WASM, U64StrPtr),
    ?assertEqual(<<"18446744073709551615">>, U64Str1),
    % u64 - outside u64 range errors
    U64Case3 = (?U64_MAX + 1) + 255,
    ?event({u64_case3, U64Case3}),
    {error, "Argument value out of range for wasm type"} = call(WASM, "format_u64", [U64Case3]),
    ok.

-define(F32_MAX,            340282346638528859811704183484516925440.000000).
-define(F32_MAX_STR_BIN, <<"340282346638528859811704183484516925440.000000">>).
-define(F64_MAX,            179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368.000000).
-define(F64_MAX_STR_BIN, <<"179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368.000000">>).

send_float_test() ->
    {ok, File} = file:read_file("test/format.aot"),
    {ok, WASM} = start(File),
    % f32 - within f32 range ok
    F32Case0 = 0.000000,
    ?event({f32_case0, F32Case0}),
    {ok, [F32StrPtr]} = call(WASM, "format_f32", [F32Case0]),
    {ok, F32Str} = hb_beamr_io:read_string(WASM, F32StrPtr),
    ?assertEqual(<<"0.000000">>, F32Str),
    F32Case1 = ?F32_MAX,
    ?event({f32_case1, F32Case1}),
    {ok, [F32StrPtr]} = call(WASM, "format_f32", [F32Case1]),
    {ok, F32Str2} = hb_beamr_io:read_string(WASM, F32StrPtr),
    ?assertEqual(?F32_MAX_STR_BIN, F32Str2),
    % f32 - outside f32 range goes to inf
    F32Case2 = ?F32_MAX * 10,
    ?event({f32_case2, F32Case2}),
    {ok, [F32StrPtr]} = call(WASM, "format_f32", [F32Case2]),
    {ok, F32Str3} = hb_beamr_io:read_string(WASM, F32StrPtr),
    ?assertEqual(<<"inf">>, F32Str3),
    % f64 - within f64 range ok
    F64Case0 = 0.0,
    ?event({f64_case0, F64Case0}),
    {ok, [F64StrPtr]} = call(WASM, "format_f64", [F64Case0]),
    {ok, F64Str} = hb_beamr_io:read_string(WASM, F64StrPtr),
    ?assertEqual(<<"0.000000">>, F64Str),
    F64Case1 = ?F64_MAX,
    ?event({f64_case1, F64Case1}),
    {ok, [F64StrPtr]} = call(WASM, "format_f64", [F64Case1]),
    {ok, F64Str2} = hb_beamr_io:read_string(WASM, F64StrPtr),
    ?assertEqual(?F64_MAX_STR_BIN, F64Str2),
    % f64 - outside f64 range cannot be represented in erlang
    try
        _ = ?F64_MAX * 10,
        ?assert(false)
    catch
        error:badarith ->
            ok
    end.

%% @doc Test standalone `hb_beamr' correctly after loading a WASM module.
multi_wasm_test() ->
    {ok, File} = file:read_file("test/test.aot"),
    {ok, WASM1} = start(File),
    {ok, WASM2} = start(File),
    {ok, [Result1]} = call(WASM1, "fac", [5.0]),
    ?assertEqual(120.0, Result1),
    {ok, [Result2]} = call(WASM2, "fac", [5.0]),
    ?assertEqual(120.0, Result2),
    {ok, [Result1B]} = call(WASM1, "fac", [5.0]),
    ?assertEqual(120.0, Result1B).

%% @doc Test that imported functions can be called from the WASM module.
imported_function_test() ->
    {ok, File} = file:read_file("test/pow_calculator.aot"),
    {ok, WASM} = start(File),
    {ok, [Result], _} =
        call(WASM, <<"pow">>, [2, 5],
            fun(Msg1, #{ args := [Arg1, Arg2] }, _Opts) ->
                {ok, [Arg1 * Arg2], Msg1}
            end),
    ?assertEqual(32, Result).

%% @doc Test that WASM Memory64 modules load and execute correctly.
wasm64_test() ->
    {ok, File} = file:read_file("test/test-64.aot"),
    {ok, WASM} = start(File),
    {ok, [Result]} = call(WASM, "fac", [5.0]),
    ?assertEqual(120.0, Result).

%% @doc Ensure that processes outside of the initial one can interact with
%% the WASM executor.
multiclient_test() ->
    Self = self(),
    ExecPID = spawn(fun() ->
        receive {wasm, WASM} ->
            {ok, [Result]} = call(WASM, "fac", [5.0]),
            Self ! {result, Result}
        end
    end),
    _StartPID = spawn(fun() ->
        {ok, File} = file:read_file("test/test.aot"),
        {ok, WASM} = start(File),
        ExecPID ! {wasm, WASM}
    end),
    receive
        {result, Result} ->
            ?assertEqual(120.0, Result)
    end.

benchmark_test() ->
    BenchTime = 1,
    {ok, File} = file:read_file("test/test-64.aot"),
    {ok, WASM} = start(File),
    Iterations = hb:benchmark(
        fun() ->
            {ok, [Result]} = call(WASM, "fac", [5.0]),
            ?assertEqual(120.0, Result)
        end,
        BenchTime
    ),
    ?event(benchmark, {scheduled, Iterations}),
    ?assert(Iterations > 1000),
    hb_util:eunit_print(
        "Executed ~s calls through Beamr in ~p seconds (~.2f call/s)",
        [hb_util:human_int(Iterations), BenchTime, Iterations / BenchTime]
    ),
    ok.