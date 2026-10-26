%%% @doc Functions for devices that commit with a shared secret.
-module(lib_secret).
-export([to_committer/1]).

%% @doc Build a committer address from a shared secret.
to_committer(Secret) ->
    hb_util:human_id(hb_crypto:sha256(Secret)).
