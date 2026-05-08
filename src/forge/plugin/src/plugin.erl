%%% @doc Rebar3 plugin entry-point that registers the `device' provider
%%% namespace and its sub-commands.
%%%
%%% The HyperBEAM device tooling exposes one canonical namespace:
%%% `rebar3 device <command>'. Each sub-command lives in its own
%%% provider module (`plugin_prv_<cmd>') for clarity and to keep
%%% argument parsing local.
%%%
%%% Sub-commands:
%%% <ul>
%%%   <li>`package'  — generate `_hb_device_*' archives from `dev_*' sources</li>
%%%   <li>`verify'   — re-load packaged archives and check invariants</li>
%%%   <li>`preload'  — build a `preloaded-store' LMDB store</li>
%%%   <li>`test'     — run device EUnit tests against the preloaded store</li>
%%%   <li>`publish'  — package, sign, and upload to Arweave</li>
%%% </ul>
-module(plugin).
-export([init/1]).

init(State) ->
    Mods = [
        plugin_prv_package,
        plugin_prv_verify,
        plugin_prv_preload,
        plugin_prv_test,
        plugin_prv_publish
    ],
    % Initialize each module.
    lists:foldl(
        fun(Mod, {ok, Acc}) -> Mod:init(Acc) end,
        {ok, State},
        Mods
    ).
