%%% @doc `rebar3 device preload' — build a `preloaded-store' LMDB
%%% store for the discovered devices.
%%%
%%% Packages each device, signs its specification and implementation
%%% messages with the configured wallet, writes them to a fresh
%%% `hb_store_lmdb' store, and produces a signed `Device-Index' provider
%%% message.
%%%
%%% On success the provider prints (and returns from `do/1') the path to
%%% the generated store and the index message ID. The corresponding
%%% `_build/hb_preloaded_index.hrl' header is regenerated so that the
%%% kernel default node configuration can pick up the index ID at
%%% compile-time.
-module(rebar3_device_prv_preload).
-export([init/1, do/1, format_error/1, run/2]).

-define(NAMESPACE, device).
-define(PROVIDER, preload).

init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {namespace, ?NAMESPACE},
        {module, ?MODULE},
        {bare, true},
        {deps, [{default, app_discovery}, {default, compile}]},
        {example, "rebar3 device preload --output-dir _build/preloaded-store"},
        {opts, rebar3_device_args:opts()},
        {short_desc, "Generate a HyperBEAM preloaded-store."},
        {desc,
            "Package, sign and index the discovered devices into a "
            "LMDB-backed preloaded-store. Outputs the store path "
            "and the index message ID."
        }
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    Args = rebar3_device_args:parse(State),
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
    Groups = hb_packager:scan(Dirs,
        #{ <<"device-roots">> => Roots }
    ),
    Pkgs = [hb_packager:package(G, NodeOpts) || G <- Groups],
    {ok, Result} = hb_preload:build_dir(Pkgs, Wallet, OutputDir, NodeOpts),
    %% Always (re)generate the header so the kernel can compile.
    HeaderPath = header_path(OutputDir),
    ok = hb_preload:write_index_header(maps:get(index, Result), HeaderPath),
    rebar_api:info("device preload: store ~s, index ~s",
        [OutputDir, maps:get(index, Result)]),
    {ok, Result}.

default_node_opts() ->
    #{}.

load_wallet(undefined) ->
    hb:wallet();
load_wallet(Path) ->
    hb:wallet(binary_to_list(hb_util:bin(Path))).

header_path(OutputDir) ->
    BuildDir = filename:dirname(binary_to_list(hb_util:bin(OutputDir))),
    list_to_binary(filename:join(BuildDir, "hb_preloaded_index.hrl")).

format_error(Reason) ->
    io_lib:format("device preload failed: ~p", [Reason]).
