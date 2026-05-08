%%% @doc Shared argument parsing for the `rebar3 device' providers.
%%%
%%% The provider namespace exposes a small, consistent flag set:
%%% <ul>
%%%   <li>`--device-src dir[,dir2]'  source roots to scan (default: `src')</li>
%%%   <li>`--output-dir dir'         where to write artifacts (default
%%%        depends on command)</li>
%%%   <li>`--key path'               path to a wallet keyfile</li>
%%%   <li>`--device-roots p[,p2]'   restrict to specific `dev_*' roots</li>
%%% </ul>
%%%
%%% Each provider re-uses {@link opts/0} for the rebar3 spec and
%%% {@link parse/2} to convert the parsed options into a normalised map.
-module(plugin_args).

-export([opts/0, parse/2, bootstrap_preloaded_dirs/0, bootstrap_preloaded_dirs/1]).

opts() ->
    [
        {device_src, $s, "device-src", string,
            "Comma-separated list of source directories to scan."},
        {output_dir, $o, "output-dir", string,
            "Output directory for generated artifacts."},
        {key, $k, "key", string,
            "Path to wallet keyfile used for signing."},
        {device_roots, $r, "device-roots", string,
            "Comma-separated list of dev_* roots to operate upon."}
    ].

parse(State, DefaultOutput) ->
    {Args, _Rest} = rebar_state:command_parsed_args(State),
    SrcRaw = proplists:get_value(device_src, Args, "src"),
    OutRaw = proplists:get_value(output_dir, Args, DefaultOutput),
    KeyRaw = proplists:get_value(key, Args, undefined),
    RootsRaw = proplists:get_value(device_roots, Args, undefined),
    #{
        <<"device-src">> => split_list(SrcRaw),
        <<"output-dir">> => to_bin(OutRaw),
        <<"key">> => maybe_bin(KeyRaw),
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

bootstrap_preloaded_dirs() ->
    bootstrap_preloaded_dirs([]).

bootstrap_preloaded_dirs([]) ->
    case filelib:is_file("src/kernel/hb_ao_device.erl") of
        true -> [<<"src/preloaded">>];
        false -> [<<"_build/default/lib/hb/src/preloaded">>]
    end;
bootstrap_preloaded_dirs(Dirs) ->
    Dirs.
