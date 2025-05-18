-module(hb_wtime).

-export([
    create/1,
    meta/1,
    call_begin/3,
    call_continue/4,
    mem_size/1,
    mem_read/3,
    mem_write/3
]).

-include("cargo.hrl").
-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).

-include("src/include/hb.hrl").
-hb_debug(print).

%%%===================================================================
%%% API
%%%===================================================================

create(_Bin) ->
    ?NOT_LOADED.

meta(_Context) ->
    ?NOT_LOADED.

call_begin(_Context, _Func, _Args) ->
    ?NOT_LOADED.

call_continue(_Context, _Module, _Field, _Results) ->
    ?NOT_LOADED.

mem_size(_Context) ->
    ?NOT_LOADED.

mem_read(_Context, _Offset, _Length) ->
    ?NOT_LOADED.

mem_write(_Context, _Offset, _Data) ->
    ?NOT_LOADED.

%%%===================================================================
%%% NIF
%%%===================================================================

init() ->
    ?load_nif_from_crate(hb_wtime, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

create_test() ->
    % Minimal valid WASM binary
    Bin = <<0, 97, 115, 109, 1, 0, 0, 0>>,
    {ok, _Inst} = create(Bin),
    ok.

create_error_test() ->
    % Invalid WASM binary
    Bin = <<0, 0, 0, 0, 0, 0, 0, 0>>,
    {error, Error} = create(Bin),
    ?event({invalid_wasm_error_raw, {explicit, Error}}),
    ok.

meta_test() ->
    Bin =
        <<
            "(module\n"
            "  (import \"env\" \"some_import_func\" (func $some_import_func (param i32) (result i32)))\n"
            "  (memory (export \"memory\") 1)\n"
            "  (func (export \"some_export_func\") (result f64)\n"
            "    f64.const 1.0\n"
            "  )\n"
            ")"
        >>,
    {ok, Inst} = create(Bin),
    {ok, Meta} = meta(Inst),
    {{imports, Imports}, {exports, Exports}} = Meta,
    ?assertEqual([
        {func, <<"env">>, <<"some_import_func">>, [i32], [i32]}
    ], Imports),
    ?assertEqual([
        {memory, <<"memory">>},
        {func, <<"some_export_func">>, [], [f64]}
    ], Exports),
    ok.

call_start_test() ->
    {ok, Bin} = file:read_file("test/test.wasm"),
    {ok, Inst} = create(Bin),
    {ok, complete, Res} = call_begin(Inst, <<"fac">>, [5.0]),
    ?assertEqual([120.0], Res),
    ok.

call_resume_test() ->
    {ok, Bin} = file:read_file("test/pow_calculator.wasm"),
    {ok, Inst} = create(Bin),
    Mod = <<"my_lib">>,
    Field = <<"mul">>,
    {ok, import, [Mod, Field, Res1]} = call_begin(Inst, <<"pow">>, [2, 2]),
    ?assertEqual([1, 2], Res1),
    {ok, import, [Mod, Field, [Res2A, Res2B]]} = call_continue(Inst, Mod, Field, [1 * 2]),
    {ok, complete, Res3} = call_continue(Inst, Mod, Field, [Res2A * Res2B]),
    ?assertEqual([4], Res3),
    ok.

nested_call_test() ->
    WAT =
        <<
            "(module\n"
            "  (import \"env\" \"host_A\" (func $host_A (result i32)))\n"
            "  (func (export \"outer_func\") (result i32)\n"
            "    call $host_A\n"
            "  )\n"
            "  (func (export \"inner_func\") (result i32)\n"
            "    i32.const 99\n"
            "  )\n"
            ")"
        >>,
    {ok, Inst} = create(WAT),
    % 1. Start outer_func, it should call host_A
    {ok, import, [EnvModuleA, FuncNameA, _ParamsA]} = call_begin(Inst, <<"outer_func">>, []),
    ?assertEqual(<<"env">>, EnvModuleA),
    ?assertEqual(<<"host_A">>, FuncNameA),
    % 2. While outer_func is awaiting host_A, call inner_func (nesting)
    %    call_begin should allow this because the FSM is in AwaitingHost for the outer call.
    {ok, complete, [InnerResult]} = call_begin(Inst, <<"inner_func">>, []),
    ?assertEqual(99, InnerResult),
    % 3. Provide response for host_A to resume outer_func
    HostAResponse = 42,
    {ok, complete, [OuterResult]} = call_continue(Inst, EnvModuleA, FuncNameA, [HostAResponse]),
    ?assertEqual(HostAResponse, OuterResult),
    ok.

mem_size_read_write_test() ->
    WAT = <<"(module (memory (export \"memory\") 1))">>,
    {ok, Inst} = create(WAT),
    % Memory size for 1 page
    {ok, Size} = mem_size(Inst),
    ?assertEqual(65536, Size), % 64KiB
    % Write data to memory
    InitialOffset = 5,
    WriteData = <<"Hello Wasmtime!\x00\x99">>,
    ?assertEqual(ok, mem_write(Inst, InitialOffset, WriteData)),
    % Read subset of data back from memory
    AdditionalOffset = 10,
    ReadOffset = InitialOffset + AdditionalOffset,
    ReadLength = size(WriteData) - AdditionalOffset,
    {ok, ReadData} = mem_read(Inst, ReadOffset, ReadLength),
    ?assertEqual(<<"time!\x00\x99">>, ReadData),
    ok.

mem_size_zero_test() ->
    WAT = <<"(module (memory (export \"memory\") 0))">>,
    {ok, Inst} = create(WAT),
    {ok, 0} = mem_size(Inst),
    ok.

mem_size_not_found_test() ->
    WAT = <<"(module)">>,
    {ok, Inst} = create(WAT),
    {ok, not_found} = mem_size(Inst),
    ok.

-endif.
