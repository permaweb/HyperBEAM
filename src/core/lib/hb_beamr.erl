
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
%%%     reset(WASM, WasmBinary, Mode, Mem) -> ok
%%%         Where:
%%%             WASM is the pid of the WASM worker.
%%%             WasmBinary is the WASM binary of the instance.
%%%             Mode is the type of the WASM module (`wasm' or `aot').
%%%             Mem is a binary output of a previous `serialize/1' call.
%%%         Loads a new instance of the image under the worker's pid, with
%%%         its memory taken from `Mem': the state a restore gives.
%%% </pre>
%%% 
%%% BEAMR was designed for use in the HyperBEAM project, but is suitable for
%%% deployment in other Erlang applications that need to run WASM modules. PRs
%%% are welcome.
-module(hb_beamr).
%%% Control API:
-export([start/1, start/2, call/3, call/4, call/5, call/6, stop/1, wasm_send/2]).
%%% Utility API:
-export([serialize/1, deserialize/2, reset/4, stub/3]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% An image is WASM if it starts with the WASM magic. An AOT image is machine
%% code for the host, so only the `aot' mode, which the node's options give,
%% loads one.
-define(IS_WASM(Bin),
    (byte_size(Bin) >= 4 andalso binary_part(Bin, 0, 4) =:= <<0, "asm">>)).

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
start(WasmBinary, Mode)
        when is_binary(WasmBinary)
        andalso Mode =/= aot
        andalso not ?IS_WASM(WasmBinary) ->
    {error, <<"Failed to load module: image is not WASM.">>};
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
        {execution_result, Imports, Exports} ->
            ?event(
                {wasm_init_success,
                    {imports, Imports},
                    {exports, Exports}}),
            {ok, WASM, Imports, Exports};
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
        {become, From, WasmBinary, Mode} ->
            % Load a new instance of the image in a new port, then serve it
            % under this worker's pid. The new instance's globals start at
            % their initial values; the caller writes the old memory in via
            % `deserialize/2' afterwards.
            NewPort = open_port({spawn, "hb_beamr"}, []),
            NewPort ! {self(), {command, term_to_binary({init, WasmBinary, Mode})}},
            receive
                {execution_result, _, _} ->
                    case erlang:port_info(Port, id) of
                        undefined -> ok;
                        _ -> port_close(Port)
                    end,
                    From ! {become, ok},
                    worker(NewPort, Listener);
                {error, Error} ->
                    port_close(NewPort),
                    From ! {become, {error, Error}},
                    worker(Port, Listener)
            end;
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
    {ok, Res, _} = call(PID, FuncRef, Args, fun stub/3),
    {ok, Res}.
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
            {error, <<"invalid-parameters">>, StateMsg}
    end.

%% @doc Stub import function for the WASM executor.
stub(Base, _Req, _Opts) ->
    ?event(stub_stdlib_called),
    {ok, [0], Base}.

%% @doc Synchonously monitor the WASM executor for a call result and any
%% imports that need to be handled. An import that raises stops the executor,
%% and its exception is raised to the caller.
monitor_call(WASM, ImportFun, StateMsg, Opts) ->
    receive
        {execution_result, Result} ->
            ?event({call_result, Result}),
            {ok, Result, StateMsg};
        {import, Module, Func, Args, Signature} ->
            ?event({import_called, Module, Func, Args, Signature}),
            try
                {ok, Res, StateReq} =
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
                StateReq
            of
                NextState -> monitor_call(WASM, ImportFun, NextState, Opts)
            catch
                Err:Reason:Stack ->
                    % The executor waits for the import's response: stop it.
                    % A stopped executor sends no further messages.
                    ?event({import_error, Err, Reason, Stack}),
                    stop(WASM),
                    erlang:raise(Err, Reason, Stack)
            end;
        {error, Error} ->
            ?event({wasm_error, Error}),
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

%% @doc Deserialize a WASM state from a binary. The instance's memory is first
%% grown to the size of the binary, so a state whose memory grew after the
%% instance started is written in full.
deserialize(WASM, Bin) when is_pid(WASM) andalso is_binary(Bin) ->
    ?event(starting_deserialize),
    {ok, Size} = hb_beamr_io:size(WASM),
    Pages = max(0, byte_size(Bin) - Size) div 65536,
    Res =
        maybe
            ok ?= hb_beamr_io:grow(WASM, Pages),
            hb_beamr_io:write(WASM, 0, Bin)
        end,
    ?event({finished_deserialize, Res}),
    Res.

%% @doc Replace the WASM instance of a worker with a new instance of the
%% image, loaded with the given memory: the state a restore from a snapshot
%% gives. The worker keeps its pid, so every holder of the instance
%% continues with a live, freshly initialized instance, with its globals at
%% their start values.
reset(_WASM, WasmBinary, Mode, _Memory)
        when is_binary(WasmBinary)
        andalso Mode =/= aot
        andalso not ?IS_WASM(WasmBinary) ->
    {error, <<"Failed to load module: image is not WASM.">>};
reset(WASM, WasmBinary, Mode, Memory)
        when is_pid(WASM)
        andalso is_binary(WasmBinary)
        andalso is_binary(Memory)
        andalso (Mode =:= wasm orelse Mode =:= aot) ->
    ?event({resetting_wasm_instance, WASM}),
    WASM ! {become, self(), WasmBinary, Mode},
    receive
        {become, ok} ->
            Res = deserialize(WASM, Memory),
            ?event({finished_reset, Res}),
            Res;
        {become, {error, Error}} ->
            ?event({reset_failed, Error}),
            {error, Error}
    end.

%% Tests

driver_loads_test() ->
    ?assertEqual(ok, load_driver()).

%% @doc Test standalone `hb_beamr' correctly after loading a WASM module.
simple_wasm_test() ->
    {ok, File} = file:read_file("test/test.wasm"),
    {ok, WASM, _Imports, _Exports} = start(File),
    {ok, [Result]} = call(WASM, "fac", [5.0]),
    ?assertEqual(120.0, Result),
    % A module with an export whose signature is longer than 256 bytes
    % starts: its signature does not overflow the buffer and end the VM.
    {ok, WideFile} = file:read_file("test/test-wide.wasm"),
    ?assertMatch({ok, _, _, _}, start(WideFile)),
    % An image that is not WASM does not start in the `wasm' mode.
    ?assertMatch({error, _}, start(<<"not a wasm image">>, wasm)).

%% @doc Test that imported functions can be called from the WASM module.
imported_function_test() ->
    {ok, File} = file:read_file("test/pow_calculator.wasm"),
    {ok, WASM, _Imports, _Exports} = start(File),
    {ok, [Result], _} =
        call(WASM, <<"pow">>, [2, 5],
            fun(Base, #{ args := [Arg1, Arg2] }, _Opts) ->
                {ok, [Arg1 * Arg2], Base}
            end),
    ?assertEqual(32, Result).

%% @doc Test that WASM Memory64 modules load and execute correctly.
wasm64_test() ->
    {ok, File} = file:read_file("test/test-64.wasm"),
    {ok, WASM, _ImportMap, _Exports} = start(File),
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
        {ok, File} = file:read_file("test/test.wasm"),
        {ok, WASM, _ImportMap, _Exports} = start(File),
        ExecPID ! {wasm, WASM}
    end),
    receive
        {result, Result} ->
            ?assertEqual(120.0, Result)
    end.

benchmark_test() ->
    BenchTime = 0.25,
    {ok, File} = file:read_file("test/test-64.wasm"),
    {ok, WASM, _ImportMap, _Exports} = start(File),
    Iterations = hb_test_utils:benchmark(
        fun() ->
            {ok, [Result]} = call(WASM, "fac", [5.0]),
            ?assertEqual(120.0, Result)
        end,
        BenchTime
    ),
    ?event(benchmark, {scheduled, Iterations}),
    ?assert(Iterations > 1000),
    hb_test_utils:benchmark_print(
        <<"Direct beamr: Executed">>,
        <<"calls">>,
        Iterations,
        BenchTime
    ),
    ok.

%% @doc A trapped call leaves the instance usable for the next call: a
%% process that skips the erroring slot keeps the same instance.
trap_keeps_instance_usable_test() ->
    {ok, File} = file:read_file("test/test-64.wasm"),
    {ok, WASM, _ImportMap, _Exports} = start(File),
    ?assertMatch(
        {error, _, #{}},
        call(WASM, "fac", [1000000000.0], fun stub/3, #{})
    ),
    ?assertEqual({ok, [120.0]}, call(WASM, "fac", [5.0])),
    % A reset instance goes on from a fresh instance of the image.
    {ok, Mem} = serialize(WASM),
    ok = reset(WASM, File, wasm, Mem),
    ?assertEqual({ok, [120.0]}, call(WASM, "fac", [5.0])).

%% @doc An imported function of no results is called, and the driver sizes
%% its import message to the terms it sends.
void_import_test() ->
    % A module that imports `env.f' of type () -> () and exports `run',
    % which calls it. The byte sequence is the module's sections: type,
    % import, function, export and code.
    Module =
        <<
            0, "asm", 1, 0, 0, 0,
            1, 4, 1, 16#60, 0, 0,
            2, 9, 1, 3, "env", 1, "f", 16#00, 0,
            3, 2, 1, 0,
            7, 7, 1, 3, "run", 16#00, 1,
            16#0A, 6, 1, 4, 0, 16#10, 0, 16#0B
        >>,
    {ok, WASM, _Imports, _Exports} = start(Module),
    ?assertEqual({ok, []}, call(WASM, "run", [])),
    stop(WASM).
