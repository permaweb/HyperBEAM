%%% @doc `rebar3 device preload' - build a `preloaded-store' LMDB
%%% store for the discovered devices.
%%%
%%% Packages each device, signs its specification and implementation
%%% messages with the configured wallet, writes them to a fresh
%%% `hb_store_lmdb' store, and produces a signed `name@1.0'-compatible resolver
%%% message.
%%%
%%% On success the provider prints (and returns from `do/1') the path to
%%% the generated store and the index message ID.
-module(hb_forge_preload).
-export([init/1, do/1, format_error/1, run/2]).

-define(PROVIDER, preload).

%% @doc Register the `preload' provider with rebar3 and define its functionality.
init(State) ->
    hb_forge_args:provider(
        State,
        ?PROVIDER,
        ?MODULE,
        "rebar3 device preload",
        "Generate a HyperBEAM preloaded-store.",
        "Package, sign and index devices into an LMDB preloaded-store."
    ).

%% @doc Parse CLI args and build a signed preloaded-store.
do(State) ->
    hb_forge_args:run_provider(State, ?MODULE, fun do_run/1).

do_run(State) ->
    Args = hb_forge_args:parse(State, <<"_build/preloaded-store">>),
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
    Verbose = maps:get(<<"verbose">>, Args, false),
    Wallet = hb_forge_args:load_wallet(KeyPath),
    case hb_forge_args:default_preloaded_dirs(Dirs) of
        {ok, DefaultDirs} ->
            PackageOpts =
                (package_opts(Args, NodeOpts))#{
                    <<"bootstrap-device-src">> =>
                        hb_forge_args:bootstrap_preloaded_dirs(DefaultDirs)
                },
            Groups =
                hb_packager:scan(DefaultDirs, #{}) ++
                hb_forge_args:scan_devices(Args),
            {ok, Result} =
                hb_preload:build_groups(Groups, Wallet, OutputDir, PackageOpts),
            rebar_api:info(
                "Device preload complete: Store: ~s; Index: ~s.",
                [OutputDir, maps:get(index, Result)]
            ),
            case Verbose of
                true -> print_device_ids(Result);
                false -> ok
            end,
            {ok, Result};
        {error, _} = Error ->
            Error
    end.

%% @doc Return node opts supplied to the preload builder by default.
default_node_opts() ->
    #{}.

%% @doc Add test compile flags when the caller is building a test store.
package_opts(Args, NodeOpts) ->
    Base = case maps:get(<<"test">>, Args, false) of
        true -> NodeOpts#{ <<"test">> => true };
        _ -> NodeOpts
    end,
    Base#{
        <<"requires-system-architecture">> =>
            maps:get(<<"requires-system-architecture">>, Args, false)
    }.

%% @doc Render provider failures for rebar3.
format_error(Reason) ->
    io_lib:format("device preload failed: ~p", [Reason]).

print_device_ids(Result) ->
    Specs = maps:get(specs, Result),
    Impls = maps:get(impls, Result),
    Pkgs = maps:get(pkgs, Result),
    lists:foreach(
        fun({Pkg, ImplID}) ->
            Name = maps:get(device_name, Pkg),
            rebar_api:info(
                "Preloaded device: ~s; Specification ID: ~s; Implementation ID: ~s.",
                [Name, maps:get(Name, Specs), ImplID]
            )
        end,
        lists:zip(Pkgs, Impls)
    ).
