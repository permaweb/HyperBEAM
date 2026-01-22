-module(dev_arith).
-export([init/3, inc/3, add/3]).
-include_lib("eunit/include/eunit.hrl").

-spec init(_, _, _) -> {ok, #{ x := integer() }}.
init(_, _, _Opts) -> {ok, #{ <<"x">> => 0 }}.

-spec inc(#{ x := integer() }, #{}, #{}) -> {ok, #{ x := integer() }}.
inc(#{<<"x">> := Value}, _, _Opts) ->  {ok, #{<<"x">> => Value + 1}}.

-spec add(#{ x := integer() }, #{ add := integer()}, _) -> {ok, #{ x := integer() }}.
add(#{<<"x">> := X}, #{ <<"add">> := Y }, _Opts) -> {ok, #{<<"x">> => X + Y}}.

-spec one(#{ 'init/inc/inc/inc/inc' := _ }, #{}, #{}) -> {ok, #{ x := integer() }}.
one(_, _, _Opts) -> {ok, #{ <<"x">> => 1 }}.

%%% Tests

basic_test() ->
    ?assertEqual(
        {ok, <<"0">>},
        hb_ao_micro:resolve(
            [
                #{ <<"device">> => <<"arith@1.0">> },
                <<"init">>,
                <<"inc">>,
                <<"x">>
            ],
            #{}
        )
    ).

lazy_test() ->
    ?assertEqual(
        {ok, <<"5">>},
        hb_ao_micro:resolve(
            [
                #{ <<"device">> => <<"arith@1.0">> },
                <<"five">>
            ],
            #{}
        )
    ).