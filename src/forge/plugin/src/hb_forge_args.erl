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
-export([provider/3, provider/6, opts/0, filter_opts/0, parse/2, scan_devices/1]).
-export([package_opts/0]).
-export([set_preloaded_env/1, restore_preloaded_env/1, with_preloaded_env/2]).
-export([load_wallet/1, bootstrap_preloaded_dirs/0, bootstrap_preloaded_dirs/1]).
-export([default_preloaded_dirs/1]).
-define(PLUGIN_NAMESPACE, device).
-define(DEPS, [{default, app_discovery}, {default, compile}]).
-define(ENV_PRELOADED_STORE, <<"HB_PRELOADED_STORE">>).
-define(ENV_PRELOADED_DEVICES_INDEX, <<"HB_PRELOADED_DEVICES_INDEX">>).

%% @doc Register a forge provider from a map of attributes. Required keys:
%% `name', `example', `short_desc', `desc'. Optional: `namespace' (defaults
%% to the `device' namespace) and `opts' (defaults to {@link opts/0}).
provider(State, Module, Spec) ->
    ProviderSpec =
        providers:create([
            {name, maps:get(name, Spec)},
            {namespace, maps:get(namespace, Spec, ?PLUGIN_NAMESPACE)},
            {module, Module},
            {bare, true},
            {deps, ?DEPS},
            {example, maps:get(example, Spec)},
            {opts, maps:get(opts, Spec, opts())},
            {short_desc, maps:get(short_desc, Spec)},
            {desc, maps:get(desc, Spec)}
        ]),
    {ok, rebar_state:add_provider(State, ProviderSpec)}.

%% @doc Register a `rebar3 device <provider>' command -- the common case:
%% the `device' namespace and the default {@link opts/0} flag set.
provider(State, Provider, Module, Example, ShortDesc, Desc) ->
    provider(State, Module, #{
        name => Provider,
        example => Example,
        short_desc => ShortDesc,
        desc => Desc
    }).

%% @doc Build-time options common to every provider.
build_opts() ->
    [
        {device_src, $s, "device-src", string,
            "Comma-separated list of source directories to scan."},
        {output_dir, $o, "output-dir", string,
            "Output directory for generated artifacts."},
        {key, $k, "key", string,
            "Path to wallet keyfile used for signing."}
    ].

%% @doc The default command-line option spec: build options plus the
%% device-selection flags used by the `rebar3 device' providers.
opts() ->
    build_opts() ++
    [
        {device_roots, $r, "device-roots", string,
            "Comma-separated list of dev_* roots to operate upon."},
        {with_core, undefined, "with-core", {boolean, false},
            "Also run core HyperBEAM EUnit modules."},
        {show_hash, undefined, "show-hash", {boolean, false},
            "Show generated device module hashes in EUnit output."}
    ].

%% @doc Option spec for the `eunit-one' provider: build options plus the
%% `--module'/`--test' filters for picking specific tests to run.
filter_opts() ->
    build_opts() ++
    [
        {module, $m, "module", string,
            "Comma-separated module names to run (default: all)."},
        {test, $t, "test", string,
            "Comma-separated test function names to run (default: all)."},
        {show_hash, undefined, "show-hash", {boolean, false},
            "Show generated device module hashes in EUnit output."}
    ].

%% @doc Convert parsed rebar command arguments into Forge's binary-keyed map.
parse(State, DefaultOutput) ->
    {Args, _Rest} = rebar_state:command_parsed_args(State),
    SrcRaw = proplists:get_value(device_src, Args, default_device_src()),
    OutRaw = proplists:get_value(output_dir, Args, DefaultOutput),
    KeyRaw = proplists:get_value(key, Args, undefined),
    RootsRaw = proplists:get_value(device_roots, Args, undefined),
    ModuleRaw = proplists:get_value(module, Args, undefined),
    TestRaw = proplists:get_value(test, Args, undefined),
    WithCore = proplists:get_value(with_core, Args, false),
    ShowHash = proplists:get_value(show_hash, Args, false),
    ModuleNames = parse_atom_list(ModuleRaw),
    TestNames = parse_atom_list(TestRaw),
    #{
        <<"device-src">> => split_list(SrcRaw),
        <<"output-dir">> => to_bin(OutRaw),
        <<"key">> => maybe_bin(KeyRaw),
        % A `--module'/`--test' filter implies `--with-core': the named
        % module may be a core test module, which is only compiled and made
        % available to EUnit under `--with-core'.
        <<"with-core">> =>
            WithCore orelse ModuleNames =/= all orelse TestNames =/= all,
        <<"show-hash">> => ShowHash,
        <<"device-roots">> =>
            case RootsRaw of
                undefined -> all;
                _ -> [to_bin(Root) || Root <- split_list(RootsRaw)]
            end,
        <<"module-names">> => ModuleNames,
        <<"test-names">> => TestNames
    }.

%% @doc Parse a comma-separated provider option into a list of atoms, or
%% `all' when the option was not given.
parse_atom_list(undefined) ->
    all;
parse_atom_list(Raw) ->
    [binary_to_atom(Name, utf8) || Name <- split_list(Raw)].

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
    filelib:is_file(<<"src/core/device/hb_device.erl">>).

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
