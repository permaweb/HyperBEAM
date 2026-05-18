%%% @doc Rebar3 checkout entry-point for HyperBEAM Forge.
-module(plugin).
-export([init/1]).

%% @doc Register the Forge provider under Rebar's expected module name.
init(State) -> hb_forge_plugin:init(State).
