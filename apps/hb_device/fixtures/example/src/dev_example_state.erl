%%% @doc Example helper module for HyperBEAM device packaging.
-module(dev_example_state).
-export([default/0]).

%% @doc Return the default response body.
default() ->
    <<"pong">>.
