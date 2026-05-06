%%% @doc Example external HyperBEAM device state helper.
-module(dev_greeting_state).
-export([default_name/0]).

%% @doc Return the default greeting target.
default_name() ->
    <<"world">>.
