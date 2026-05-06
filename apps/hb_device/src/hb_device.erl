%%% @doc Rebar3 plugin entrypoint for HyperBEAM device packaging.
-module(hb_device).
-export([init/1]).

%% @doc Register HyperBEAM device packaging providers.
init(State0) ->
    {ok, State1} = hb_device_prv_package:init(State0),
    {ok, State2} = hb_device_prv_verify:init(State1),
    {ok, State2}.
