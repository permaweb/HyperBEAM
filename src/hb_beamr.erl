
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
%%% BEAM to have enough async worker threads enabled (see `erl +A N` in the
%%% Erlang manuals).
%%% 
%%% The core API is simple:
%%%     ```
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
%%%             ImportFun must have an arity of 2: Taking an arbitrary `state`
%%%             term, and a map containing the `port`, `module`, `func`, `args`,
%%%             `signature`, and the `options` map of the import.
%%%             It must return a tuple of the form {ok, NewState, Response}.
%%%     serialize(Port) -> {ok, Mem}
%%%         Where:
%%%             Port is the port to the LID.
%%%             Mem is a binary representing the full WASM state.
%%%     deserialize(Port, Mem) -> ok
%%%         Where:
%%%             Port is the port to the LID.
%%%             Mem is a binary output of a previous `serialize/1' call.'''
%%% 
%%% BEAMR was designed for use in the HyperBEAM project, but is suitable for
%%% deployment in other Erlang applications that need to run WASM modules. PRs
%%% are welcome.
-module(hb_beamr).
-export([start/1, call/3, call/4, call/5, call/6, stop/1]).
-export([serialize/1, deserialize/2, stub/3]).


-include("src/include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Load the driver for the WASM executor.
load_driver() ->
    case erl_ddll:load("./priv", ?MODULE) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, Error} -> {error, Error}
    end.

%% @doc Start a WASM executor context. Yields a port to the LID, and the
%% imports and exports of the WASM module.
start(WasmBinary) ->
    ok = load_driver(),
    Port = open_port({spawn, "hb_beamr"}, []),
    Port ! {self(), {command, term_to_binary({init, WasmBinary})}},
    ?event({waiting_for_init_from, Port}),
    receive
        {execution_result, Imports, Exports} ->
            ?event(
                {wasm_init_success,
                    {imports, length(Imports)},
                    {exports, length(Exports)}}),
            {ok, Port, Imports, Exports}
    end.

%% @doc Stop a WASM executor context.
stop(Port) ->
    ?event({stop_invoked_for_beamr, Port}),
    port_close(Port),
    ok.

%% @doc Call a function in the WASM executor (see moduledoc for more details).
call(Port, FunctionName, Args) ->
    {ok, Res, #{}} = call(Port, FunctionName, Args, fun stub/3),
    {ok, Res}.
call(Port, FunctionName, Args, ImportFun) ->
    call(Port, FunctionName, Args, ImportFun, #{}).
call(Port, FunctionName, Args, ImportFun, StateMsg) ->
    call(Port, FunctionName, Args, ImportFun, StateMsg, #{}).
call(Port, FunctionName, Args, ImportFun, StateMsg, Opts) ->
    ?event({call_started, Port, FunctionName, Args, ImportFun}),
    ?event({call, FunctionName, Args}),
    Port ! {self(), {command, term_to_binary({call, FunctionName, Args})}},
    ?event({waiting_for_call_result, self(), Port}),
    monitor_call(Port, ImportFun, StateMsg, Opts).

%% @doc Stub import function for the WASM executor.
stub(M1, _M2, _Opts) ->
    ?event(stub_stdlib_called),
    {ok, M1, [0]}.

%% @doc Synchonously monitor the WASM executor for a call result and any
%% imports that need to be handled.
monitor_call(Port, ImportFun, StateMsg, Opts) ->
    receive
        {execution_result, Result} ->
            ?event({call_result, Result}),
            {ok, Result, StateMsg};
        {import, Module, Func, Args, Signature} ->
            ?event({import_called, Module, Func, Args, Signature}),
            try
                {ok, Response, StateMsg2} =
                    ImportFun(StateMsg,
                        #{
                            port => Port,
                            module => Module,
                            func => Func,
                            args => Args,
                            signature => Signature,
                            options => Opts
                        }),
                ?event({import_returned, Module, Func, Args, Response}),
                Port !
                    {self(),
                        {
                            command,
                            term_to_binary({import_response, Response})
                        }},
                monitor_call(Port, ImportFun, StateMsg2, Opts)
            catch
                Err:Reason:Stack ->
                    ?event({import_error, Err, Reason, Stack}),
                    stop(Port),
                    {error, Err, Reason, Stack, StateMsg}
            end;
        {error, Error} ->
            ?event({wasm_error, Error}),
            {error, Error, StateMsg}
    end.

%% @doc Serialize the WASM state to a binary.
serialize(Port) ->
    ?event(starting_serialize),
    {ok, Size} = hb_beamr_io:size(Port),
    {ok, Mem} = hb_beamr_io:read(Port, 0, Size),
    ?event({finished_serialize, byte_size(Mem)}),
    {ok, Mem}.

%% @doc Deserialize a WASM state from a binary.
deserialize(Port, Bin) ->
    ?event(starting_deserialize),
    Res = hb_beamr_io:write(Port, 0, Bin),
    ?event({finished_deserialize, Res}),
    ok.

%% Tests

driver_loads_test() ->
    ?assertEqual(ok, load_driver()).

%% @doc Test standalone `hb_beamr' correctly after loading a WASM module.
simple_wasm_test() ->
    {ok, File} = file:read_file("test/test.wasm"),
    {ok, Port, _Imports, _Exports} = start(File),
    {ok, [Result]} = call(Port, "fac", [5.0]),
    ?assertEqual(120.0, Result).

%% @doc Test that imported functions can be called from the WASM module.
imported_function_test() ->
    {ok, File} = file:read_file("test/pow_calculator.wasm"),
    {ok, Port, _Imports, _Exports} = start(File),
    ImportFunc =
        fun(State, Request) ->
            ModName = maps:get(module, Request),
            FuncName = maps:get(func, Request),
            [Arg1, Arg2] = maps:get(args, Request),
            ?assertEqual("my_lib", ModName),
            ?assertEqual("mul", FuncName),
            {ok, [Arg1 * Arg2], State}
        end,
    {ok, [Result], _} = call(Port, "pow", [2, 5], ImportFunc),
    ?assertEqual(32, Result).

%% @doc Test that WASM Memory64 modules load and execute correctly.
wasm64_test() ->
    {ok, File} = file:read_file("test/test-64.wasm"),
    {ok, Port, _ImportMap, _Exports} = start(File),
    {ok, [Result]} = call(Port, "fac", [5.0]),
    ?assertEqual(120.0, Result).