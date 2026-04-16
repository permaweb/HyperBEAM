%%% @doc A generic probabilistic gate device. When accessed as
%%% `~chance@1.0/N', the key `N' is parsed as an integer rate.
%%% A 1-in-N random check is performed:
%%%
%%%     {ok, Req}    -- check passed, hook chain continues
%%%     {error, _}   -- check failed, hook chain halts
%%%
%%% This device uses the 4-arity default handler pattern so that the
%%% rate is supplied via the URL path rather than a config key.
-module(dev_chance).
-export([info/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

info(_) -> #{ default => fun default/4 }.

default(Key, _Base, Req, _Opts) ->
    try binary_to_integer(Key) of
        Rate when Rate > 0 ->
            ?event({request, {rate, Rate}}),
            case rand:uniform(Rate) =:= 1 of
                true -> {ok, Req};
                false -> {error, <<"Filtered by chance gate.">>}
            end;
        _ ->
            {error, <<"chance@1.0 rate must be a positive integer.">>}
    catch
        error:badarg ->
            {error, <<"chance@1.0 rate must be a valid integer.">>}
    end.
