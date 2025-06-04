%%% @doc Get default store from hb_opts
-module(hb_store_default).

-export([get/0]).

%% @doc get first store from hb_opts default_message
get() ->
    % Get stores from hb_opts:default_message()
    [Store | _ ] = hb_ao:get(store, hb_opts:default_message(), #{}),
    Store.
