-module(hb_wtime).

-export([ add/2
        , my_map/0
        , my_maps/0
        , my_tuple/0
        , unit_enum_echo/1
        , tagged_enum_echo/1
        , untagged_enum_echo/1
        , xor_example/2
        , wtime_instance_init/1
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

wtime_instance_init(_Bin) ->
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


wtime_instance_init_test() ->
    Bin = <<0,97,115,109,1,0,0,0>>,  % Minimal valid WASM binary
    {ok, _Context} = wtime_instance_init(Bin),
    ok.

wtime_instance_init_error_test() ->
    Bin = <<0,0,0,0,0,0,0,0>>,  % Invalid WASM binary
    {error, _} = wtime_instance_init(Bin),
    ok.

-endif.
