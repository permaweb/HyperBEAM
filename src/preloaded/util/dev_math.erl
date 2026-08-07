%%% @doc Simple test device with math utilities. Not intended for production use.
-module(dev_math).
-include("include/hb.hrl").
-export([inc_x/3, dec_x/3, add_x/3, sum/3, with_sum/3]).

-spec inc_x(#{ x := integer() }, _, _) -> {ok, #{ '...' => base }}.
inc_x(#{ <<"x">> := X }, _, _) -> 
    ?prim_dbg({inc_x_called, X}),
    {ok, #{ <<"x">> => X + 1 }}.

-spec dec_x(#{ x := integer() }, _, _) -> {ok, #{ '...' => base }}.
dec_x(#{ <<"x">> := X }, _, _) -> {ok, #{ <<"x">> => X - 1 }}.

-spec add_x(#{ x := integer() }, #{ add := integer() }, _) ->
    {ok, #{ '...' => base }}.
add_x(#{ <<"x">> := X }, #{ <<"add">> := Add }, _) ->
    {ok, #{ <<"x">> => X + Add }}.

-spec sum(#{ x := integer(), y := integer() }, _, _) ->
    {ok, #{ '...' => base }}.
sum(#{ <<"x">> := X, <<"y">> := Y }, _, _) ->
    {ok, X + Y}.

-spec with_sum(#{ x := integer(), y := integer() }, _, _) ->
    {ok, #{ sum => integer(), '...' => base }}.
with_sum(#{ <<"x">> := X, <<"y">> := Y }, _, _) ->
    ?prim_dbg({doing_sum, {x, X}, {y, Y}}),
    {ok, #{ <<"sum">> => X + Y }}.