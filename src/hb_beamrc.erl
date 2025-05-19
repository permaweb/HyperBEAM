%%% @doc BEAMRC: A WAMRC wrapper for BEAM.
%%% 
%%% Beamrc is a library that allows you to...
%%% 
-module(hb_beamrc).

-export([compile/1]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

% -hb_debug(print).

%% @doc Load the driver for the WASM compiler...
load_driver() ->
    case erl_ddll:load(code:priv_dir(hb), ?MODULE) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, Error} -> {error, Error}
    end.

%% @doc Start a WASM compiler context. Yields a port to the LID...
compile(WasmBinary) when is_binary(WasmBinary) ->   
    ?event({compiling_module, {bytes, byte_size(WasmBinary)}}),
    ok = load_driver(),
    Port = open_port({spawn, "hb_beamrc"}, []),
    Port ! {self(), {command, term_to_binary({compile, WasmBinary})}},
    ?event({waiting_for_compile_from, Port}),
    receive
        {compilation_result, Imports, Exports, AotWasm} ->
            ?event({compilation_result, 
                {imports, length(Imports)},
                {exports, length(Exports)},
                {bytes, byte_size(AotWasm)}}),
            {ok, Imports, Exports, AotWasm};
        {error, Error} ->
            ?event({compilation_error, Error}),
            {error, Error};
        Any -> ?event({any, Any})
    end.

% Tests
compile_binary_test() ->
    WasmPath = <<"test/test.wasm">>,
    WasmAotPath = <<"test/test.aot">>,
    {ok, WasmBinary} = file:read_file(WasmPath),
    {ok, _, _, AotWasmGenerated} = compile(WasmBinary),
    {ok, AotWasmFile} = file:read_file(WasmAotPath),
    print_binary_info("WasmBinary", WasmBinary),
    print_binary_info("AotWasmGenerated", AotWasmGenerated),
    print_binary_info("AotWasmFile", AotWasmFile),
    ?assertEqual(AotWasmGenerated, AotWasmFile).

compile_interface_test() ->
    WasmPath = <<"test/pow_calculator.wasm">>,
    {ok, WasmBinary} = file:read_file(WasmPath),
    {ok, Imports, Exports, _} = compile(WasmBinary),
    ?assertEqual(length(Imports), 4),
    ?assertEqual(lists:nth(1, Imports), {func, "my_lib", "mul", "(ii)i"}),
    ?assertEqual(length(Exports), 13),
    ?assertEqual(lists:nth(1, Exports), {memory, "memory", []}),
    ?assertEqual(lists:nth(2, Exports), {func, "pow", "(ii)i"}).

compile_invalid_test() ->
    WasmPath = <<"test/invalid.wasm">>,
    {ok, WasmBinary} = file:read_file(WasmPath),
    {error, _} = compile(WasmBinary).

compile_empty_test() ->
    WasmPath = <<"test/empty.wasm">>,
    {ok, WasmBinary} = file:read_file(WasmPath),
    {error, _} = compile(WasmBinary).

print_binary_info(Name, Bin) when is_binary(Bin) ->
    ?event({
        {name, Name},
        {size, byte_size(Bin)},
        {hash, erlang:md5(Bin)},
        {leading_bytes, binary:part(Bin, 0, 10)}
    }).
