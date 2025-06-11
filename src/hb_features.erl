%%% @doc A module that exports a list of feature flags that the node supports
%%% using the `-ifdef' macro.
%%% As a consequence, this module acts as a proxy of information between the
%%% build system and the runtime execution environment.
-module(hb_features).
%%% Public API.
-export([all/0, enabled/1]).
%%% Individual feature flags.
-export([http3/0, rocksdb/0, test/0, genesis_wasm/0]).


%% @doc Returns a list of all feature flags that the node supports.
all() ->
    Features =
        lists:filtermap(
          fun({Name, _}) ->
                  case lists:member(Name, [all, enabled, module_info]) of
                      true -> false;
                      false -> {true, Name}
                  end
          end,
          ?MODULE:module_info(exports)),
    hb_maps:from_list(
      lists:map(
        fun(Name) ->
                {Name, ?MODULE:Name()}
        end,
        Features)).


%% @doc Returns true if the feature flag is enabled.
enabled(Feature) ->
    hb_maps:get(Feature, all(), false).


%%% Individual feature flags.
%%% These functions use the `-ifdef' macro to conditionally return a boolean
%%% value based on the presence of the `ENABLE_<FEATURE>' macro during
%%% compilation.

-ifdef(ENABLE_HTTP3).


http3() -> true.


-else.


http3() -> false.


-endif.

-ifdef(ENABLE_ROCKSDB).


rocksdb() -> true.


-else.


rocksdb() -> false.


-endif.

-ifdef(ENABLE_GENESIS_WASM).


genesis_wasm() -> true.


-else.


genesis_wasm() -> false.


-endif.

-ifdef(TEST).


test() -> true.


-else.


test() -> false.


-endif.
