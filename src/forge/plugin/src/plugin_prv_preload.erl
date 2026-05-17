%%% @doc `rebar3 device preload' — build a `preloaded-store' LMDB
%%% store for the discovered devices.
%%%
%%% Packages each device, signs its specification and implementation
%%% messages with the configured wallet, writes them to a fresh
%%% `hb_store_lmdb' store, and produces a signed `name@1.0' resolver
%%% message.
%%%
%%% On success the provider prints (and returns from `do/1') the path to
%%% the generated store and the index message ID. The corresponding
%%% `_build/hb_preloaded_index.hrl' header is regenerated so that the
%%% core default node configuration can pick up the index ID at
%%% compile-time.
-module(plugin_prv_preload).
-export([init/1, do/1, format_error/1, run/2]).

-define(PROVIDER, preload).

init(State) ->
    plugin_args:provider(
        State,
        ?PROVIDER,
        ?MODULE,
        "rebar3 device preload",
        "Generate a HyperBEAM preloaded-store.",
        "Package, sign and index devices into an LMDB preloaded-store."
    ).

do(State) ->
    Args = plugin_args:parse(State, "_build/preloaded-store"),
    case run(Args, default_node_opts()) of
        {ok, _Result} -> {ok, State};
        {error, Reason} -> {error, format_error(Reason)}
    end.

%% @doc Run the preload pipeline. Exposed so the rebar.config compile
%% hook (and tests) can invoke it without going through rebar3 state.
run(Args, NodeOpts) ->
    Dirs = maps:get(<<"device-src">>, Args),
    OutputDir = maps:get(<<"output-dir">>, Args),
    KeyPath = maps:get(<<"key">>, Args),
    Wallet = plugin_args:load_wallet(KeyPath),
    case plugin_args:default_preloaded_dirs(Dirs) of
        {ok, DefaultDirs} ->
            PackageOpts =
                (package_opts(Args, NodeOpts))#{
                    <<"bootstrap-device-src">> =>
                        plugin_args:bootstrap_preloaded_dirs(DefaultDirs)
                },
            Groups =
                hb_packager:scan(DefaultDirs, #{}) ++
                plugin_args:scan_devices(Args),
            {ok, Result} =
                hb_preload:build_groups(Groups, Wallet, OutputDir, PackageOpts),
            HeaderPath = header_path(OutputDir),
            ok =
                hb_preload:write_index_header(
                    maps:get(index, Result),
                    HeaderPath
                ),
            rebar_api:info(
                "device preload: store ~s, index ~s",
                [OutputDir, maps:get(index, Result)]
            ),
            {ok, Result};
        {error, _} = Error ->
            Error
    end.

default_node_opts() ->
    #{}.

%% @doc Add test compile flags when the caller is building a test store.
package_opts(Args, NodeOpts) ->
    case maps:get(<<"test">>, Args, false) of
        true -> NodeOpts#{ <<"test">> => true };
        _ -> NodeOpts
    end.


%% @doc Construct the path to the preloaded-store index header.
header_path(OutputDir) ->
    BuildDir = filename:dirname(binary_to_list(hb_util:bin(OutputDir))),
    list_to_binary(filename:join(BuildDir, "hb_preloaded_index.hrl")).

format_error(Reason) ->
    io_lib:format("device preload failed: ~p", [Reason]).
