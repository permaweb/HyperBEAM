-module(hb_wtime).

-export([ add/2
        , my_map/0
        , my_maps/0
        , my_tuple/0
        , unit_enum_echo/1
        , tagged_enum_echo/1
        , untagged_enum_echo/1
        , xor_example/2
        , wtime_create_instance/1
        , wtime_call_start/3
        , wtime_call_resume/4
        ]).

-include("cargo.hrl").
-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).

%%%===================================================================
%%% API
%%%===================================================================

add(_A, _B) ->
    ?NOT_LOADED.

my_map() ->
    ?NOT_LOADED.

my_maps() ->
    ?NOT_LOADED.

my_tuple() ->
    ?NOT_LOADED.

unit_enum_echo(_Atom) ->
    ?NOT_LOADED.

tagged_enum_echo(_Tagged) ->
    ?NOT_LOADED.

untagged_enum_echo(_Untagged) ->
    ?NOT_LOADED.

xor_example(_BinX, _BinY) ->
    ?NOT_LOADED.

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

add_test() ->
    ?assertEqual(4, add(2, 2)).

my_map_test() ->
    ?assertEqual(#{lhs => 33, rhs => 21}, my_map()).

my_maps_test() ->
    ?assertEqual([#{lhs => 33, rhs => 21}, #{lhs => 33, rhs => 21}], my_maps()).

my_tuple_test() ->
    ?assertEqual({33, 21}, my_tuple()).

unit_enum_echo_test() ->
    ?assertEqual(foo_bar, unit_enum_echo(foo_bar)),
    ?assertEqual(baz, unit_enum_echo(baz)).

tagged_enum_echo_test() ->
    ?assertEqual(foo, tagged_enum_echo(foo)),
    ?assertEqual({bar, <<"string">>}, tagged_enum_echo({bar, <<"string">>})),
    ?assertEqual({baz,#{a => 1, b => 2}}, tagged_enum_echo({baz,#{a => 1, b => 2}})).

untagged_enum_echo_test() ->
    ?assertEqual(123, untagged_enum_echo(123)),
    ?assertEqual(<<"string">>, untagged_enum_echo(<<"string">>)).

xor_example_test() ->
    X = <<"\x4A\x4A\x4A\x4A\x4A\x4A\x4A\x4A">>,
    Y = <<"\x55\x55\x55\x55\x55\x55\x55\x55">>,
    ?assertEqual(<<"\x1F\x1F\x1F\x1F\x1F\x1F\x1F\x1F">>, xor_example(X, Y)).

wtime_create_instance_test() ->
    Bin = <<0,97,115,109,1,0,0,0>>,  % Minimal valid WASM binary
    {ok, _Context} = wtime_create_instance(Bin),
    ok.

wtime_create_instance_error_test() ->
    Bin = <<0,0,0,0,0,0,0,0>>,  % Invalid WASM binary
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
    WAT = <<"
        (module
          (import \"env\" \"host_A\" (func $host_A (result i32)))
          (func (export \"outer_func\") (result i32)
            call $host_A
          )
          (func (export \"inner_func\") (result i32)
            i32.const 99
          )
        )
    ">>,
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
