-module(hb_beamr).
-export([start/1, call/3, call/4, call/5, stop/1, test/0]).
-export([serialize/1, deserialize/2, stub_stdlib/6]).
% Exports for testing from other modules.
-export([gen_test_aos_msg/1, gen_test_env/0, test_call/3, test_call/4]).

-include("src/include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

test() ->
    %aos64_standalone_wex_test(),
    %checkpoint_and_resume_test().
    %timed_calls_test().
    ok.

load_driver() ->
    case erl_ddll:load("./priv", ?MODULE) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, Error} -> {error, Error}
    end.

start(WasmBinary) ->
    ok = load_driver(),
    Port = open_port({spawn, "hb_beamr"}, []),
    Port ! {self(), {command, term_to_binary({init, WasmBinary})}},
    ?event({waiting_for_init_from, Port}),
    receive
        {execution_result, Imports, Exports} ->
            ?event({wasm_init_success, {imports, length(Imports)}, {exports, length(Exports)}}),
            {ok, Port, Imports, Exports}
    end.

stop(Port) ->
    ?event(stop_called_on_dev_wasm),
    port_close(Port),
    ok.

call(Port, FunctionName, Args) ->
    call(Port, FunctionName, Args, fun stub_stdlib/6).
call(Port, FunctionName, Args, Stdlib) ->
    {ResType, Res, _} = call(#{ stdout => [] }, Port, FunctionName, Args, Stdlib),
    {ResType, Res}.
call(S, Port, FunctionName, Args, ImportFunc) ->
    ?event({call_started, Port, FunctionName, Args, ImportFunc}),
    ?event({call, FunctionName, Args}),
    Port ! {self(), {command, term_to_binary({call, FunctionName, Args})}},
    ?event({waiting_for_call_result, self(), Port}),
    exec_call(S, ImportFunc, Port).

stub_stdlib(S, _Port, _Module, _Func, _Args, _Signature) ->
    ?event(stub_stdlib_called),
    {S, [0]}.

exec_call(S, ImportFunc, Port) ->
    receive
        {execution_result, Result} ->
            ?event({call_result, Result}),
            {ok, Result, S};
        {import, Module, Func, Args, Signature} ->
            ?event({import_called, Module, Func, Args, Signature}),
            try
                {S2, ErlRes} = ImportFunc(S, Port, Module, Func, Args, Signature),
                ?event({import_returned, Module, Func, Args, ErlRes}),
                Port ! {self(), {command, term_to_binary({import_response, ErlRes})}},
                exec_call(S2, ImportFunc, Port)
            catch
                Err:Reason:Stack ->
                    ?event({import_error, Err, Reason, Stack}),
                    stop(Port),
                    {error, Err, Reason, Stack, S}
            end;
        {error, Error} ->
            ?event({wasm_error, Error}),
            {error, Error, S}
    end.

serialize(Port) ->
    ?event(starting_serialize),
    {ok, Size} = hb_beamr_io:size(Port),
    {ok, Mem} = hb_beamr_io:read(Port, 0, Size),
    ?event({finished_serialize, byte_size(Mem)}),
    {ok, Mem}.

deserialize(Port, Bin) ->
    % TODO: Be careful of memory growth!
    ?event(starting_deserialize),
    Res = hb_beamr_io:write(Port, 0, Bin),
    ?event({finished_deserialize, Res}),
    ok.

%% Tests

nif_loads_test() ->
    ?MODULE:module_info().

simple_wasm_test() ->
    {ok, File} = file:read_file("test/test.wasm"),
    {ok, Port, _Imports, _Exports} = start(File),
    {ok, [Result]} = call(Port, "fac", [5.0]),
    ?assertEqual(120.0, Result).

generate_test_vfs() ->
    dev_vfs:init(#{}).

test_call(Port, Func, Args) ->
    {ok, VFSState0} = generate_test_vfs(),
    test_call(VFSState0, Port, Func, Args).
test_call(VFSState, Port, Func, Args) ->
    call(VFSState, Port, Func, Args, fun dev_json_iface:stdlib/6).

simple_wasm_calling_test() ->
    {ok, File} = file:read_file("test/test-calling.wasm"),
    {ok, Port, _Imports, _Exports} = start(File),
    {ok, [Result]} = call(Port, "main", [1,1]),
    ?assertEqual(1, Result),
    Arg0 = <<"Test string arg 000000000000000\0">>,
    Arg1 = <<"Test string arg 111111111111111\0">>,
    {ok, Ptr0} = hb_beamr_io:write_string(Port, Arg0),
    {ok, Ptr1} = hb_beamr_io:write_string(Port, Arg1),
    ?assertNotEqual(0, Ptr1),
    {ok, [], State} = test_call(Port, "print_args", [Ptr0, Ptr1]),
    Str = dev_vfs:stdout(State),
    ?assert(binary:match(Str, <<"Test string arg 00000000000000">>) /= nomatch),
    ?assert(binary:match(Str, <<"Test string arg 11111111111111">>) /= nomatch).

imported_functions_are_called_test() ->
    {ok, File} = file:read_file("test/pow_calculator.wasm"),
    {ok, Port, _Imports, _Exports} = start(File),
    Lib = fun(S, _Port, Module, Function, Args, _Signature) ->
        ?assertEqual("my_lib", Module),
        ?assertEqual("mul", Function),
        [Arg1, Arg2] = Args,
        {S, [Arg1 * Arg2]}
    end,
    {ok, [Result]} = call(Port, "pow", [2, 5], Lib),
    ?assertEqual(32, Result).

wasm64_test() ->
    ?event(simple_wasm64_test),
    {ok, File} = file:read_file("test/test-64.wasm"),
    {ok, Port, _ImportMap, _Exports} = start(File),
    {ok, [Result]} = call(Port, "fac", [5.0]),
    ?assertEqual(120.0, Result).

gen_test_env() ->
    <<"{\"Process\":{\"Id\":\"AOS\",\"Owner\":\"FOOBAR\",\"Tags\":["
        "{\"name\":\"Name\",\"value\":\"Thomas\"},"
        "{\"name\":\"Authority\",\"value\":\"FOOBAR\"}]}}\0">>.

gen_test_aos_msg(Command) ->
    <<"{\"From\":\"FOOBAR\",\"Block-Height\":\"1\",\"Target\":\"AOS\","
        "\"Owner\":\"FOOBAR\",\"Id\":\"1\",\"Module\":\"W\","
        "\"Tags\":[{\"name\":\"Action\",\"value\":\"Eval\"}],\"Data\":\"",
        (list_to_binary(Command))/binary, "\"}\0">>.

aos64_standalone_wex_test() ->
    Msg = gen_test_aos_msg("return 1+1"),
    Env = gen_test_env(),
    {ok, File} = file:read_file("test/aos-2-pure.wasm"),
    {ok, Port, _ImportMap, _Exports} = start(File),
    {ok, Ptr1} = hb_beamr_io:write_string(Port, Msg),
    {ok, Ptr2} = hb_beamr_io:write_string(Port, Env),
    % Read the strings to validate they are correctly passed
    {ok, MsgBin} = hb_beamr_io:read(Port, Ptr1, byte_size(Msg)),
    {ok, EnvBin} = hb_beamr_io:read(Port, Ptr2, byte_size(Env)),
    ?assertEqual(Env, EnvBin),
    ?assertEqual(Msg, MsgBin),
    {ok, [Ptr3], _} = test_call(Port, "handle", [Ptr1, Ptr2]),
    {ok, ResBin} = hb_beamr_io:read_string(Port, Ptr3),
    #{<<"ok">> := true, <<"response">> := Resp} = jiffy:decode(ResBin, [return_maps]),
    #{<<"Output">> := #{ <<"data">> := Data }} = Resp,
    ?assertEqual(<<"2">>, Data).

slow_test_() ->
    [ {timeout, 30, fun test_checkpoint_and_resume/0}].

test_checkpoint_and_resume() ->
    Env = gen_test_env(),
    Msg1 = gen_test_aos_msg("TestVar = 0"),
    Msg2 = gen_test_aos_msg("TestVar = 1"),
    Msg3 = gen_test_aos_msg("return TestVar"),
    {ok, File} = file:read_file("test/aos-2-pure.wasm"),
    {ok, Port1, _ImportMap, _Exports} = start(File),
    {ok, EnvPtr} = hb_beamr_io:write_string(Port1, Env),
    {ok, Msg1Ptr} = hb_beamr_io:write_string(Port1, Msg1),
    {ok, Msg2Ptr} = hb_beamr_io:write_string(Port1, Msg2),
    {ok, Msg3Ptr} = hb_beamr_io:write_string(Port1, Msg3),
    {ok, [_], S0} = test_call(Port1, "main", [0, 0]),
    {ok, [_], S1} = test_call(S0, Port1, "handle", [Msg1Ptr, EnvPtr]),
    {ok, MemCheckpoint} = serialize(Port1),
    {ok, [_], S2} = test_call(S1, Port1, "handle", [Msg2Ptr, EnvPtr]),
    {ok, [OutPtr1], S3} = test_call(S2, Port1, "handle", [Msg3Ptr, EnvPtr]),
    {ok, Port2, _, _} = start(File),
    deserialize(Port2, MemCheckpoint),
    {ok, [OutPtr2], _S4} = test_call(S3, Port2, "handle", [Msg3Ptr, EnvPtr]),
    Str1 = hb_beamr_io:read_string(Port1, OutPtr1),
    Str2 = hb_beamr_io:read_string(Port2, OutPtr2),
    ?assertNotEqual(Str1, Str2).