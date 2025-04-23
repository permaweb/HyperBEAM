
%%% @doc BEAMRC: A WAMRC wrapper for BEAM.
%%% 
%%% Beamrc is a library that allows you to...
%%% 
-module(hb_beamrc).

-export([compile/1]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Load the driver for the WASM compiler...
load_driver() ->
    case erl_ddll:load(code:priv_dir(hb), ?MODULE) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, Error} -> {error, Error}
    end.

%% @doc Start a WASM compiler context. Yields a port to the LID...
compile(WasmBinary) when is_binary(WasmBinary) ->   
    ?event({loading_module, {bytes, byte_size(WasmBinary)}}),
    spawn(
        fun() ->
            ok = load_driver(),
            Port = open_port({spawn, "hb_beamrc"}, []),
            Port ! {self(), {command, term_to_binary({compile, WasmBinary})}},
            ?event({waiting_for_compile_from, Port})
        end
    ),
    receive
        {compile_success, AotModule} ->
            ?event(
                {compile_success,
                    {aot_module, AotModule}}),
            {ok, AotModule};
        {compile_error, Error} ->
            ?event({compile_error, Error}),
            {error, Error}
    end.

% Tests
compile_test() ->
    WasmPath = <<"test/test.wasm">>,
    WasmBinary = file:read_file(WasmPath),
    AotWasm = compile(WasmBinary),
    ?assertEqual(AotWasm, <<"test">>).
