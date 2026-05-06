%%% @doc Example helper module for HyperBEAM device packaging.
-module(dev_example_codec).
-export([encode/1]).

%% @doc Prefix a binary response body.
encode(Bin) when is_binary(Bin) ->
    <<"example:", Bin/binary>>.
