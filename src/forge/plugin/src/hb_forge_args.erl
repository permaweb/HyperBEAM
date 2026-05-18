%%% @doc Shared argument parsing for the `rebar3 device' component providers.
%%%
%%% The provider namespace exposes a small, consistent flag set:
%%% <ul>
%%%   <li>`--device-src dir[,dir2]'  source roots to scan (default:
%%%        `src/preloaded' in HyperBEAM, `src' elsewhere)</li>
%%%   <li>`--output-dir dir'         where to write artifacts (default
%%%        depends on command)</li>
%%%   <li>`--key path'               path to a wallet keyfile</li>
%%%   <li>`--device-roots p[,p2]'   restrict to specific `dev_*' roots</li>
%%% </ul>
%%%
%%% Each provider re-uses {@link opts/0} for the rebar3 spec and
%%% {@link parse/2} to convert the parsed options into a normalised map.
-module(hb_forge_args).
-export([provider/6, opts/0, parse/2, scan_devices/1, package_opts/0]).
-export([set_preloaded_env/1, restore_preloaded_env/1, with_preloaded_env/2]).
-export([load_wallet/1, bootstrap_preloaded_dirs/0, bootstrap_preloaded_dirs/1]).
-export([default_preloaded_dirs/1]).
-define(PLUGIN_NAMESPACE, device).
-define(DEPS, [{default, app_discovery}, {default, compile}]).
-define(ENV_PRELOADED_STORE, <<"HB_PRELOADED_STORE">>).
-define(ENV_PRELOADED_DEVICES_INDEX, <<"HB_PRELOADED_DEVICES_INDEX">>).

%% @doc Register a `rebar3 device <provider>' command.
provider(State, Provider, Module, Example, ShortDesc, Desc) ->
    ProviderSpec =
        providers:create([
            {name, Provider},
            {namespace, ?PLUGIN_NAMESPACE},
            {module, Module},
            {bare, true},
            {deps, ?DEPS},
            {example, Example},
            {opts, opts()},
            {short_desc, ShortDesc},
            {desc, Desc}
        ]),
    {ok, rebar_state:add_provider(State, ProviderSpec)}.

%% @doc Return the shared command-line option spec for all forge providers.
opts() ->
    [
        {device_src, $s, "device-src", string,
            "Comma-separated list of source directories to scan."},
        {output_dir, $o, "output-dir", string,
            "Output directory for generated artifacts."},
        {key, $k, "key", string,
            "Path to wallet keyfile used for signing."},
        {device_roots, $r, "device-roots", string,
            "Comma-separated list of dev_* roots to operate upon."},
        {with_core, undefined, "with-core", {boolean, false},
            "Also run core HyperBEAM EUnit modules."}
    ].

%% @doc Convert parsed rebar command arguments into Forge's binary-keyed map.
parse(State, DefaultOutput) ->
    {Args, _Rest} = rebar_state:command_parsed_args(State),
    SrcRaw = proplists:get_value(device_src, Args, default_device_src()),
    OutRaw = proplists:get_value(output_dir, Args, DefaultOutput),
    KeyRaw = proplists:get_value(key, Args, undefined),
    RootsRaw = proplists:get_value(device_roots, Args, undefined),
    WithCore = proplists:get_value(with_core, Args, false),
    #{
        <<"device-src">> => split_list(SrcRaw),
        <<"output-dir">> => to_bin(OutRaw),
        <<"key">> => maybe_bin(KeyRaw),
        <<"with-core">> => WithCore,
        <<"device-roots">> =>
            case RootsRaw of
                undefined -> all;
                _ -> [to_bin(Root) || Root <- split_list(RootsRaw)]
            end
    }.

%% @doc Split a comma-separated provider option into trimmed binary values.
split_list(List) when is_list(List) ->
    split_list(hb_util:bin(List));
split_list(Bin) when is_binary(Bin) ->
    Parts = [string:trim(P) || P <- binary:split(Bin, <<",">>, [global])],
    [P || P <- Parts, P =/= <<>>].

%% @doc Normalize optional provider string values to binaries.
to_bin(undefined) -> undefined;
to_bin(V) -> hb_util:bin(V).

%% @doc Preserve `undefined' while normalizing present values to binaries.
maybe_bin(undefined) -> undefined;
maybe_bin(V) -> to_bin(V).

%% @doc Scan the selected device roots from parsed provider arguments.
scan_devices(Args) ->
    hb_packager:scan(
        maps:get(<<"device-src">>, Args),
        #{ <<"device-roots">> => maps:get(<<"device-roots">>, Args, all) }
    ).

%% @doc Common package options for provider commands.
package_opts() ->
    #{ <<"bootstrap-device-src">> => bootstrap_preloaded_dirs() }.

%% @doc Run `Fun' with `HB_PRELOADED_*' pointed at a preload result.
with_preloaded_env(Result, Fun) when is_function(Fun, 0) ->
    Env = set_preloaded_env(Result),
    try Fun()
    after restore_preloaded_env(Env)
    end.

%% @doc Point this VM at a generated preloaded-store.
set_preloaded_env(Result) ->
    StorePath = hb_util:bin(hb_maps:get(<<"name">>, maps:get(store, Result))),
    Index = hb_util:bin(maps:get(index, Result)),
    OldStore = getenv(?ENV_PRELOADED_STORE),
    OldIndex = getenv(?ENV_PRELOADED_DEVICES_INDEX),
    putenv(?ENV_PRELOADED_STORE, StorePath),
    putenv(?ENV_PRELOADED_DEVICES_INDEX, Index),
    erase_preloaded_env_cache(),
    {OldStore, OldIndex}.

%% @doc Restore the previous preloaded-store environment.
restore_preloaded_env({OldStore, OldIndex}) ->
    restore_env(?ENV_PRELOADED_STORE, OldStore),
    restore_env(?ENV_PRELOADED_DEVICES_INDEX, OldIndex),
    erase_preloaded_env_cache().

%% @doc Restore one environment variable captured by {@link set_preloaded_env/1}.
restore_env(Name, false) ->
    unsetenv(Name);
restore_env(Name, Value) ->
    putenv(Name, Value).

%% @doc Clear hb_opts' cached view of preloaded-store environment variables.
erase_preloaded_env_cache() ->
    erase({os_env, hb_util:list(?ENV_PRELOADED_STORE)}),
    erase({os_env, hb_util:list(?ENV_PRELOADED_DEVICES_INDEX)}),
    erase({processed_env, <<"preloaded-store">>}),
    erase({processed_env, <<"preloaded-devices-index">>}).

%% @doc Read an OS environment variable using HB binary naming internally.
getenv(Name) ->
    os:getenv(hb_util:list(Name)).

%% @doc Set an OS environment variable at the Erlang/OS string boundary.
putenv(Name, Value) ->
    os:putenv(hb_util:list(Name), hb_util:list(Value)).

%% @doc Unset an OS environment variable at the Erlang/OS string boundary.
unsetenv(Name) ->
    os:unsetenv(hb_util:list(Name)).

%% @doc Load the configured wallet, or the default wallet if omitted.
load_wallet(undefined) ->
    hb:wallet();
load_wallet(Path) ->
    hb:wallet(hb_util:list(Path)).

%% @doc Choose the default device source root for HB vs external checkouts.
default_device_src() ->
    case is_hb_checkout() of
        true -> <<"src/preloaded">>;
        false -> <<"src">>
    end.

bootstrap_preloaded_dirs() ->
    bootstrap_preloaded_dirs([]).

%% @doc Return HB's built-in preloaded source dir needed for bootstrap devices.
bootstrap_preloaded_dirs([]) ->
    case is_hb_checkout() of
        true -> [<<"src/preloaded">>];
        false -> [<<"_build/default/lib/hb/src/preloaded">>]
    end;
bootstrap_preloaded_dirs(Dirs) ->
    Dirs.

%% @doc Return dependency preloaded dirs needed outside the HB checkout.
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

%% @doc Return true when the current checkout is HyperBEAM itself.
is_hb_checkout() ->
    filelib:is_file(<<"src/core/hb_ao_device.erl">>).

%% @doc Return true when any configured source dir contains `Dir'.
source_covers(Dir, Dirs) ->
    lists:any(fun(D) -> contains_dir(D, Dir) end, Dirs).

%% @doc Return true when `Child' is `Parent' or sits below it.
contains_dir(Parent, Child) ->
    ParentPath = hb_util:bin(filename:absname(hb_util:list(Parent))),
    ChildPath = hb_util:bin(filename:absname(hb_util:list(Child))),
    ParentPrefix = <<ParentPath/binary, "/">>,
    ParentPath =:= ChildPath orelse
        path_prefix(ChildPath, ParentPrefix) =:= ParentPrefix.

%% @doc Return the same-length prefix of `Path' for directory containment.
path_prefix(Path, Prefix) ->
    binary:part(Path, 0, min(byte_size(Path), byte_size(Prefix))).
