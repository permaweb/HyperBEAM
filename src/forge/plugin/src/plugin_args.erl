%%% @doc Shared argument parsing for the `rebar3 device' providers.
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
-module(plugin_args).

-export([provider/6, opts/0, parse/2, scan_devices/1, package_opts/0]).
-export([load_wallet/1, bootstrap_preloaded_dirs/0, bootstrap_preloaded_dirs/1]).
-export([default_preloaded_dirs/1]).

-define(NAMESPACE, device).
-define(DEPS, [{default, app_discovery}, {default, compile}]).

%% @doc Register a `rebar3 device <provider>' command.
provider(State, Provider, Module, Example, ShortDesc, Desc) ->
    ProviderSpec =
        providers:create([
            {name, Provider},
            {namespace, ?NAMESPACE},
            {module, Module},
            {bare, true},
            {deps, ?DEPS},
            {example, Example},
            {opts, opts()},
            {short_desc, ShortDesc},
            {desc, Desc}
        ]),
    {ok, rebar_state:add_provider(State, ProviderSpec)}.

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

split_list(List) when is_list(List) ->
    [string:trim(P) || P <- string:split(List, ",", all), P =/= ""];
split_list(Bin) when is_binary(Bin) ->
    split_list(binary_to_list(Bin)).

to_bin(undefined) -> undefined;
to_bin(L) when is_list(L) -> list_to_binary(L);
to_bin(B) when is_binary(B) -> B.

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

%% @doc Load the configured wallet, or the default wallet if omitted.
load_wallet(undefined) ->
    hb:wallet();
load_wallet(Path) ->
    hb:wallet(binary_to_list(hb_util:bin(Path))).

default_device_src() ->
    case is_hb_checkout() of
        true -> "src/preloaded";
        false -> "src"
    end.

bootstrap_preloaded_dirs() ->
    bootstrap_preloaded_dirs([]).

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

is_hb_checkout() ->
    filelib:is_file("src/core/hb_ao_device.erl").

source_covers(Dir, Dirs) ->
    lists:any(fun(D) -> contains_dir(D, Dir) end, Dirs).

contains_dir(Parent, Child) ->
    ParentPath = filename:absname(binary_to_list(hb_util:bin(Parent))),
    ChildPath = filename:absname(binary_to_list(hb_util:bin(Child))),
    ParentPath =:= ChildPath orelse
        lists:prefix(ParentPath ++ "/", ChildPath).
