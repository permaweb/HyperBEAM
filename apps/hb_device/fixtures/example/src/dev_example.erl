%%% @doc Example root module for HyperBEAM device packaging.
-module(dev_example).
-export([info/1, ping/3]).

%% @doc Return static device metadata.
info(_) ->
    #{ <<"name">> => <<"example@1.0">> }.

%% @doc Echo a request body through helper modules.
ping(_Base, Req, _Opts) ->
    Body = maps:get(<<"body">>, Req, dev_example_state:default()),
    {ok, dev_example_codec:encode(Body)}.
