-module(hb_wtime).

-export([
    create/1,
    call_begin/3,
    call_continue/4,
    mem_size/1,
    mem_read/3,
    mem_write/3
]).

-include("cargo.hrl").
-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).

%%%===================================================================
%%% API
%%%===================================================================

create(_Bin) ->
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
    {ok, _Context} = create(Bin),
    ok.

create_error_test() ->
    % Invalid WASM binary
    Bin = <<0, 0, 0, 0, 0, 0, 0, 0>>,
    {error, _} = create(Bin),
    ok.

call_start_test() ->
    {ok, Bin} = file:read_file("test/test.wasm"),
    {ok, Inst} = create(Bin),
    {ok, complete, [120.0]} = call_begin(Inst, <<"fac">>, [5.0]),
    ok.

call_resume_test() ->
    {ok, Bin} = file:read_file("test/pow_calculator.wasm"),
    {ok, Inst} = create(Bin),
    Mod = <<"my_lib">>,
    Field = <<"mul">>,
    {ok, import, [Mod, Field, Res1]} = call_begin(Inst, <<"pow">>, [2, 2]),
    ?assertEqual([1, 2], Res1),
    {ok, import, [Mod, Field, [Res2A, Res2B]]} = call_continue(Inst, Mod, Field, [1 * 2]),
    {ok, complete, [4]} = call_continue(Inst, Mod, Field, [Res2A * Res2B]),
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

    {ok, Size} = mem_size(Inst),
    ?assertEqual(65536, Size), % 64KiB (1 page)

    Offset = 10,
    WriteData = <<"hello_wasmtime\x99">>,
    ?assertEqual(ok, mem_write(Inst, Offset, WriteData)),
    
    ReadLength = size(WriteData),
    {ok, ReadData} = mem_read(Inst, Offset, ReadLength),
    ?assertEqual(WriteData, ReadData),
    ok.

-endif.
