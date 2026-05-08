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
%%% kernel default node configuration can pick up the index ID at
%%% compile-time.
-module(plugin_prv_preload).
-export([init/1, do/1, format_error/1, run/2]).

-define(NAMESPACE, device).
-define(PROVIDER, preload).

init(State) ->
    % Create the provider.
    Provider =
        providers:create([
            {name, ?PROVIDER},
            {namespace, ?NAMESPACE},
            {module, ?MODULE},
            {bare, true},
            {deps, [{default, app_discovery}, {default, compile}]},
            {example, "rebar3 device preload"},
            {opts, plugin_args:opts()},
            {short_desc, "Generate a HyperBEAM preloaded-store."},
            {desc,
                "Package, sign and index the discovered devices into a "
                "LMDB-backed preloaded-store. Outputs the store path "
                "and the index message ID."
            }
        ]),
    {ok, rebar_state:add_provider(State, Provider)}.

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
    Roots = maps:get(<<"device-roots">>, Args, all),
    KeyPath = maps:get(<<"key">>, Args),
    Wallet = load_wallet(KeyPath),
    case default_preloaded_dirs(Dirs) of
        {ok, DefaultDirs} ->
            Groups =
                hb_packager:scan(DefaultDirs, #{}) ++
                hb_packager:scan(Dirs, #{ <<"device-roots">> => Roots }),
            Pkgs = [hb_packager:package(G, NodeOpts) || G <- Groups],
            {ok, Result} =
                hb_preload:build_dir(Pkgs, Wallet, OutputDir, NodeOpts),
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

load_wallet(undefined) ->
    hb:wallet();
load_wallet(Path) ->
    hb:wallet(binary_to_list(hb_util:bin(Path))).

%% @doc Return the HyperBEAM dependency's preloaded source directory.
default_preloaded_dirs(Dirs) ->
    DefaultDir = <<"_build/default/lib/hb/src/preloaded">>,
    case is_hb_checkout() orelse source_covers(DefaultDir, Dirs) of
        true ->
            {ok, []};
        false ->
            case filelib:is_dir(DefaultDir) of
                true -> {ok, [DefaultDir]};
                false -> {error, missing_hb_dependency_preloaded_devices}
            end
    end.

%% @doc Return true when the provider is running inside the HyperBEAM repo.
is_hb_checkout() ->
    filelib:is_file("src/kernel/hb_ao_device.erl").

%% @doc Return true when `Dirs' already includes `Dir'.
source_covers(Dir, Dirs) ->
    lists:any(fun(D) -> contains_dir(D, Dir) end, Dirs).

%% @doc Return true when `Parent' is equal to, or contains, `Child'.
contains_dir(Parent, Child) ->
    ParentPath = filename:absname(binary_to_list(hb_util:bin(Parent))),
    ChildPath = filename:absname(binary_to_list(hb_util:bin(Child))),
    ParentPath =:= ChildPath orelse
        lists:prefix(ParentPath ++ "/", ChildPath).

%% @doc Construct the path to the preloaded-store index header.
header_path(OutputDir) ->
    BuildDir = filename:dirname(binary_to_list(hb_util:bin(OutputDir))),
    list_to_binary(filename:join(BuildDir, "hb_preloaded_index.hrl")).

format_error(Reason) ->
    io_lib:format("device preload failed: ~p", [Reason]).
