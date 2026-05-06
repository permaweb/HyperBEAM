%%% @doc Example external HyperBEAM device formatting helper.
-module(dev_greeting_text).
-export([format/1]).

%% @doc Format a greeting response.
format(Name) when is_binary(Name) ->
    <<"hello, ", Name/binary>>.
