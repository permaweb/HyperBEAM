%%% @doc `rebar3 device package' — generate packaged device BEAM archives.
%%%
%%% Walks one or more source directories for `dev_*.erl' files, groups
%%% root + helpers, and writes a generated BEAM archive per device into
%%% the configured output directory.
-module(plugin_prv_package).
-export([init/1, do/1, format_error/1]).

-define(PROVIDER, package).

init(State) ->
    plugin_args:provider(
        State,
        ?PROVIDER,
        ?MODULE,
        "rebar3 device package",
        "Generate packaged device BEAM archives.",
        "Scan dev_* Erlang sources and emit _hb_device_* BEAM archives."
    ).

do(State) ->
    Args = plugin_args:parse(State, "_build/device-packages"),
    case run_with_args(Args) of
        {ok, _Pkgs} -> {ok, State};
        {error, Reason} -> {error, format_error(Reason)}
    end.

run_with_args(Args) ->
    Output = maps:get(<<"output-dir">>, Args),
    OutputBin = hb_util:bin(Output),
    ok = filelib:ensure_dir(filename:join(binary_to_list(OutputBin), ".keep")),
    % Package each device group, and write to the output directory.
    Pkgs =
        lists:map(
            fun(Pkg) ->
                % Write the package to the output directory.
                write_pkg(OutputBin, Pkg),
                Pkg
            end,
            hb_packager:package_all(
                plugin_args:scan_devices(Args),
                plugin_args:package_opts()
            )
        ),
    rebar_api:info(
        "device package: emitted ~p archives to ~s",
        [length(Pkgs), Output]
    ),
    {ok, Pkgs}.

%% @doc Write a package to the output directory.
write_pkg(OutputBin, #{ module_name := Mod, archive := Archive }) ->
    ArchivePath =
        filename:join(
            binary_to_list(OutputBin),
            atom_to_list(Mod) ++ ".beam-archive.zip"
        ),
    ok = file:write_file(ArchivePath, Archive).

format_error({Type, Reason}) ->
    io_lib:format("device package failed: ~p — ~p", [Type, Reason]);
format_error(Reason) ->
    io_lib:format("device package failed: ~p", [Reason]).
