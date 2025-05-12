-module(hb_wtime).

-export([
    wtime_create_instance/1,
    wtime_call_start/3,
    wtime_call_resume/4
]).

-include("cargo.hrl").
-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).

%%%===================================================================
%%% API
%%%===================================================================

wtime_create_instance(_Bin) ->
    ?NOT_LOADED.

wtime_call_start(_Context, _Func, _Args) ->
    ?NOT_LOADED.

wtime_call_resume(_Context, _Module, _Field, _Results) ->
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

wtime_create_instance_test() ->
    % Minimal valid WASM binary
    Bin = <<0, 97, 115, 109, 1, 0, 0, 0>>,
    {ok, _Context} = wtime_create_instance(Bin),
    ok.

wtime_create_instance_error_test() ->
    % Invalid WASM binary
    Bin = <<0, 0, 0, 0, 0, 0, 0, 0>>,
    {error, _} = wtime_create_instance(Bin),
    ok.

wtime_call_start_test() ->
    {ok, Bin} = file:read_file("test/test.wasm"),
    {ok, Inst} = wtime_create_instance(Bin),
    {ok, complete, [120.0]} = wtime_call_start(Inst, <<"fac">>, [5.0]),
    ok.

wtime_call_resume_test() ->
    {ok, Bin} = file:read_file("test/pow_calculator.wasm"),
    {ok, Inst} = wtime_create_instance(Bin),
    Mod = <<"my_lib">>,
    Field = <<"mul">>,
    {ok, import, [Mod, Field, Res1]} = wtime_call_start(Inst, <<"pow">>, [2, 2]),
    ?assertEqual([1, 2], Res1),
    {ok, import, [Mod, Field, [Res2A, Res2B]]} = wtime_call_resume(Inst, Mod, Field, [1 * 2]),
    {ok, complete, [4]} = wtime_call_resume(Inst, Mod, Field, [Res2A * Res2B]),
    ok.

wtime_nested_call_test() ->
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
    {ok, Inst} = wtime_create_instance(WAT),
    % 1. Start outer_func, it should call host_A
    {ok, import, [EnvModuleA, FuncNameA, _ParamsA]} = wtime_call_start(Inst, <<"outer_func">>, []),
    ?assertEqual(<<"env">>, EnvModuleA),
    ?assertEqual(<<"host_A">>, FuncNameA),
    % 2. While outer_func is awaiting host_A, call inner_func (nesting)
    %    wtime_call_start should allow this because the FSM is in AwaitingHost for the outer call.
    {ok, complete, [InnerResult]} = wtime_call_start(Inst, <<"inner_func">>, []),
    ?assertEqual(99, InnerResult),
    % 3. Provide response for host_A to resume outer_func
    HostAResponse = 42,
    {ok, complete, [OuterResult]} = wtime_call_resume(Inst, EnvModuleA, FuncNameA, [HostAResponse]),
    ?assertEqual(HostAResponse, OuterResult),
    ok.

-endif.
