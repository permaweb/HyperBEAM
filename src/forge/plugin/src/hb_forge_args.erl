%%% @doc Shared argument parsing for the `rebar3 device' component providers.
%%%
%%% The provider namespace exposes a small, consistent flag set:
%%% <ul>
%%%   <li>`--device-src dir[,dir2]'  source roots to scan (default:
%%%        `src/preloaded' in HyperBEAM, `src' elsewhere)</li>
%%%   <li>`--output-dir dir'         where to write artifacts (default
%%%        depends on command)</li>
%%%   <li>`--key path'               path to a wallet keyfile</li>
%%%   <li>`--devices p[,p2]'        restrict to specific `dev_*' roots</li>
%%% </ul>
%%%
%%% Each provider re-uses {@link opts/0} for the rebar3 spec and
%%% {@link parse/2} to convert the parsed options into a normalised map.
-module(hb_forge_args).
-export([provider/6, opts/0, parse/2, scan_devices/1, package_opts/0, package_opts/1]).
-export([run_provider/3]).
-export([set_preloaded_env/1, restore_preloaded_env/1, with_preloaded_env/2]).
-export([load_wallet/1, bootstrap_preloaded_dirs/0, bootstrap_preloaded_dirs/1]).
-export([default_preloaded_dirs/1]).
-define(PLUGIN_NAMESPACE, device).
-define(DEPS, [{default, app_discovery}, {default, compile}]).
-define(ENV_PRELOADED_STORE, <<"HB_PRELOADED_STORE">>).

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
        {publish_codec, undefined, "publish-codec", string,
            "Commitment codec used when publishing."},
        {bundler, undefined, "bundler", string,
            "Bundler endpoint used when publishing."},
        {requires_system_architecture, undefined,
            "requires-system-architecture", {boolean, false},
            "Include the host system architecture in implementation metadata."},
        {devices, $d, "devices", string,
            "Comma-separated list of dev_* roots to operate upon."},
        {module, $m, "module", string,
            "Comma-separated module names to run."},
        {test, $t, "test", string,
            "Comma-separated tests to run, optionally Module:Func1+Func2."},
        {timeout, undefined, "timeout", string,
            "Per-test timeout, in seconds."},
        {timeout_multiplier, undefined, "timeout-multiplier", string,
            "Multiplier for EUnit timeouts."},
        {with_core, undefined, "with-core", {boolean, false},
            "Also run core HyperBEAM EUnit modules."},
        {show_hash, undefined, "show-hash", {boolean, false},
            "Show generated device module hashes in EUnit output."},
        {record, undefined, "record", string,
            "Write recorder@1.0 test flights; --record means errors, --record=all means every test."},
        {help, $h, "help", {boolean, false},
            "Show command help."},
        {dry_run, undefined, "dry-run", {boolean, false},
            "Sign packages and print their IDs without uploading to Arweave."},
        {verbose, undefined, "verbose", {boolean, false},
            "Print locally preloaded device IDs."}
    ].

%% @doc Convert parsed rebar command arguments into Forge's binary-keyed map.
parse(State, DefaultOutput) ->
    {Args, _Rest} = rebar_state:command_parsed_args(State),
    SrcRaw = proplists:get_value(device_src, Args, default_device_src()),
    OutRaw = proplists:get_value(output_dir, Args, DefaultOutput),
    KeyRaw = proplists:get_value(key, Args, undefined),
    BundlerRaw = proplists:get_value(bundler, Args, undefined),
    RootsRaw = proplists:get_value(devices, Args, undefined),
    ModuleRaw = proplists:get_value(module, Args, undefined),
    TestRaw = proplists:get_value(test, Args, undefined),
    TimeoutRaw = proplists:get_value(timeout, Args, undefined),
    TimeoutMultiplierRaw =
        proplists:get_value(timeout_multiplier, Args, undefined),
    RequiresSystemArchitecture =
        proplists:get_value(requires_system_architecture, Args, false),
    WithCore = proplists:get_value(with_core, Args, false),
    ShowHash = proplists:get_value(show_hash, Args, false),
    RecordRaw =
        case record_requested(rebar_state:command_args(State)) of
            true -> proplists:get_value(record, Args, "errors");
            false -> undefined
        end,
    DryRun = proplists:get_value(dry_run, Args, false),
    Verbose = proplists:get_value(verbose, Args, false),
    Bundler = maybe_bin(BundlerRaw),
    #{
        <<"device-src">> => split_list(SrcRaw),
        <<"output-dir">> => to_bin(OutRaw),
        <<"key">> => maybe_bin(KeyRaw),
        <<"publish-codec">> => to_bin(proplists:get_value(publish_codec, Args, "ans104@1.0")),
        <<"bundler">> => Bundler,
        <<"requires-system-architecture">> => RequiresSystemArchitecture,
        <<"with-core">> => WithCore,
        <<"show-hash">> => ShowHash,
        <<"record">> => parse_record_mode(RecordRaw),
        <<"module-names">> => parse_atom_list(ModuleRaw),
        <<"test-specs">> => parse_test_specs(TestRaw),
        <<"timeout">> => parse_number(TimeoutRaw),
        <<"timeout-multiplier">> => parse_number(TimeoutMultiplierRaw),
        <<"dry-run">> => DryRun,
        <<"verbose">> => Verbose,
        <<"device-roots">> =>
            case RootsRaw of
                undefined -> all;
                _ -> [to_bin(Root) || Root <- split_list(RootsRaw)]
            end
    }.

%% @doc Run a provider body after shared `rebar3 device' boundary handling.
run_provider(State, Module, Fun) when is_function(Fun, 1) ->
    {Args, _Rest} = rebar_state:command_parsed_args(State),
    case proplists:get_value(help, Args, false) of
        true ->
            Provider =
                providers:get_provider_by_module(
                    Module,
                    rebar_state:providers(State)
                ),
            providers:help(Provider),
            {ok, State};
        false ->
            try Fun(State)
            catch
                error:{device_compile_failed, _, _, _, _} = Reason ->
                    {error, hb_packager:format_error(Reason)}
            end
    end.

%% @doc Parse a comma-separated provider option into atoms, or `all'.
parse_atom_list(undefined) ->
    all;
parse_atom_list(Raw) ->
    [binary_to_atom(Name, utf8) || Name <- split_list(Raw)].

%% @doc Parse `--test' specs. Supports `Module:Fun1+Fun2' and bare funcs.
parse_test_specs(undefined) ->
    all;
parse_test_specs(Raw) ->
    lists:flatmap(fun parse_test_spec/1, split_list(Raw)).

parse_test_spec(Spec) ->
    case binary:split(Spec, <<":">>) of
        [Funs] ->
            [{all, parse_test_funs(Funs)}];
        [Mod, Funs] ->
            [{binary_to_atom(Mod, utf8), parse_test_funs(Funs)}]
    end.

parse_test_funs(Funs) ->
    [binary_to_atom(Fun, utf8)
        || Fun <- binary:split(Funs, <<"+">>, [global]),
           Fun =/= <<>>].

%% @doc Parse `--record=all|errors'.
parse_record_mode(undefined) ->
    none;
parse_record_mode(Raw) ->
    case string:lowercase(to_bin(Raw)) of
        <<"all">> -> all;
        <<"errors">> -> errors;
        Mode ->
            rebar_api:abort(
                "--record accepts errors or all; omit it to disable recording. Got ~s",
                [hb_util:list(Mode)]
            )
    end.

record_requested(Args) ->
    lists:any(fun record_arg/1, Args).

record_arg(Arg) ->
    Bin = hb_util:bin(Arg),
    Bin =:= <<"--record">>
        orelse binary:match(Bin, <<"--record=">>) =:= {0, 9}.

%% @doc Parse a numeric provider option, preserving `undefined'.
parse_number(undefined) ->
    undefined;
parse_number(Raw) ->
    Bin = to_bin(Raw),
    try binary_to_integer(Bin)
    catch
        error:badarg -> binary_to_float(Bin)
    end.

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

%% @doc Scan the selected devices from parsed provider arguments.
scan_devices(Args) ->
    hb_packager:scan(
        maps:get(<<"device-src">>, Args),
        #{ <<"device-roots">> => maps:get(<<"device-roots">>, Args, all) }
    ).

%% @doc Common package options for provider commands.
package_opts() ->
    package_opts(#{}).

package_opts(Args) ->
    Opts = #{
        <<"bootstrap-device-src">> => bootstrap_preloaded_dirs(),
        <<"requires-system-architecture">> =>
            maps:get(<<"requires-system-architecture">>, Args, false)
    },
    case maps:get(<<"bundler">>, Args, undefined) of
        undefined -> Opts;
        Bundler -> Opts#{ <<"bundler-ans104">> => Bundler }
    end.

%% @doc Run `Fun' with `HB_PRELOADED_STORE' pointed at a preload result.
with_preloaded_env(Result, Fun) when is_function(Fun, 0) ->
    Env = set_preloaded_env(Result),
    try Fun()
    after restore_preloaded_env(Env)
    end.

%% @doc Point this VM at a generated preloaded-store.
set_preloaded_env(Result) ->
    StorePath = hb_util:bin(hb_maps:get(<<"name">>, maps:get(store, Result))),
    OldStore = getenv(?ENV_PRELOADED_STORE),
    putenv(?ENV_PRELOADED_STORE, StorePath),
    erase_preloaded_env_cache(),
    OldStore.

%% @doc Restore the previous preloaded-store environment.
restore_preloaded_env(OldStore) ->
    restore_env(?ENV_PRELOADED_STORE, OldStore),
    erase_preloaded_env_cache().

%% @doc Restore one environment variable captured by {@link set_preloaded_env/1}.
restore_env(Name, false) ->
    unsetenv(Name);
restore_env(Name, Value) ->
    putenv(Name, Value).

%% @doc Clear hb_opts' cached view of preloaded-store environment variables.
erase_preloaded_env_cache() ->
    erase(default_message_with_env).

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
