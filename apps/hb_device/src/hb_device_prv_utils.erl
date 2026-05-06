%%% @doc Shared option parsing for HyperBEAM device rebar3 providers.
-module(hb_device_prv_utils).
-export([opts/0, packager_opts/1, preload_opts/1]).

-define(DEFAULT_SRC_DIR, "src").
-define(DEFAULT_OUT_DIR, "_build/default/packaged-devices").
-define(DEFAULT_PRELOAD_STORE_DIR, "_build/default/preloaded-device-store").
-define(DEFAULT_PRELOAD_METADATA_FILE, "_build/default/preloaded-device-metadata.eterm").
-define(DEFAULT_KEY_FILE, "hyperbeam-key.json").

%% @doc Return CLI options accepted by the plugin providers.
opts() ->
    [
        {root, $r, "root", {string, undefined},
            "Package only the named root module, or comma-separated roots."},
        {src_dir, $s, "src-dir", {string, undefined},
            "Directory containing Erlang source files."},
        {out_dir, $o, "out-dir", {string, undefined},
            "Directory to write packaged device artifacts."},
        {store_dir, undefined, "store-dir", {string, undefined},
            "Filesystem store directory for preloaded devices."},
        {metadata_file, undefined, "metadata-file", {string, undefined},
            "Erlang term metadata file for the generated preload store."},
        {key, $k, "key", {string, undefined},
            "Wallet keyfile to sign preloaded device messages."}
    ].

%% @doc Convert rebar3 state into packager options.
packager_opts(State) ->
    Config = rebar_state:get(State, hb_device, []),
    {Cli, _Args} = rebar_state:command_parsed_args(State),
    #{
        src_dir => option(src_dir, Cli, Config, ?DEFAULT_SRC_DIR),
        out_dir => option(out_dir, Cli, Config, ?DEFAULT_OUT_DIR),
        roots => roots(Cli, Config)
    }.

%% @doc Convert rebar3 state into preload options.
preload_opts(State) ->
    Config = rebar_state:get(State, hb_device, []),
    {Cli, _Args} = rebar_state:command_parsed_args(State),
    (packager_opts(State))#{
        store_dir =>
            option(
                store_dir,
                Cli,
                Config,
                ?DEFAULT_PRELOAD_STORE_DIR
            ),
        metadata_file =>
            option(
                metadata_file,
                Cli,
                Config,
                ?DEFAULT_PRELOAD_METADATA_FILE
            ),
        key =>
            option(
                key,
                Cli,
                Config,
                ?DEFAULT_KEY_FILE
            )
    }.

%% @doc Return a CLI option, falling back through config and default values.
option(Key, Cli, Config, Default) ->
    case proplists:get_value(Key, Cli, undefined) of
        undefined -> proplists:get_value(Key, Config, Default);
        Value -> Value
    end.

%% @doc Return package roots requested by CLI or rebar config.
roots(Cli, Config) ->
    case option(root, Cli, Config, undefined) of
        undefined ->
            case proplists:get_value(roots, Config, all) of
                all -> all;
                Roots -> [normalize_root(Root) || Root <- Roots]
            end;
        Roots ->
            [normalize_root(Root) || Root <- string:tokens(Roots, ",")]
    end.

%% @doc Normalize a root module from config or CLI.
normalize_root(Root) when is_atom(Root) ->
    Root;
normalize_root(Root) when is_list(Root) ->
    list_to_atom(string:trim(Root)).
