%%% @doc A dummy module that we use for testing load-device-from-cache behaviors.

-module(dev_dummy).
-export([echo/3]).

echo(_M1, M2, _Opts) ->
    {ok, M2}.