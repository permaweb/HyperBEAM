%%% @doc Example root module for HyperBEAM device packaging.
-module(dev_example).
-implements(<<"example@1.0">>).
-export([capture/3, ping/3]).

%% @doc Echo a request body through helper modules.
ping(_Base, Req, _Opts) ->
    Body = maps:get(<<"body">>, Req, dev_example_state:default()),
    {ok, dev_example_codec:encode(Body)}.

%% @doc Echo a request body through a captured helper function.
capture(_Base, Req, _Opts) ->
    Encode = fun dev_example_codec:encode/1,
    {ok, Encode(maps:get(<<"body">>, Req, dev_example_state:default()))}.
