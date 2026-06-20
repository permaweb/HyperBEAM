%%% @doc `rebar3 device package' - generate packaged device BEAM archives.
%%%
%%% Walks one or more source directories for `dev_*.erl' files, groups
%%% root + helpers, and writes a generated BEAM archive per device into
%%% the configured output directory.
-module(hb_forge_package).
-export([init/1, do/1, format_error/1]).

-define(PROVIDER, package).

%% @doc Register the `package' provider with rebar3.
init(State) ->
    hb_forge_args:provider(
        State,
        ?PROVIDER,
        ?MODULE,
        "rebar3 device package",
        "Generate packaged device BEAM archives.",
        "Scan dev_* Erlang sources and emit _hb_device_* BEAM archives."
    ).

%% @doc Parse CLI args and emit generated package archives.
do(State) ->
    hb_forge_args:run_provider(State, ?MODULE, fun do_run/1).

do_run(State) ->
    Args = hb_forge_args:parse(State, <<"_build/device-packages">>),
    case run_with_args(Args) of
        {ok, _Pkgs} -> {ok, State};
        {error, Reason} -> {error, format_error(Reason)}
    end.

%% @doc Package selected devices and write each archive to the output dir.
run_with_args(Args) ->
    Output = maps:get(<<"output-dir">>, Args),
    OutputBin = hb_util:bin(Output),
    ok = filelib:ensure_dir(filename:join(hb_util:list(OutputBin), ".keep")),
    % Package each device group, and write to the output directory.
    Pkgs =
        hb_forge_seed:with_forge_bootstrap(
            hb_forge_args:package_opts(Args),
            fun(Opts) ->
                lists:map(
                    fun(Pkg) ->
                        % Write the package to the output directory.
                        write_pkg(OutputBin, Pkg),
                        Pkg
                    end,
                    hb_packager:package_all(
                        hb_forge_args:scan_devices(Args),
                        Opts
                    )
                )
            end
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
            hb_util:list(OutputBin),
            atom_to_list(Mod) ++ ".beam-archive.zip"
        ),
    ok = file:write_file(ArchivePath, Archive).

%% @doc Render provider failures for rebar3.
format_error({Type, Reason}) ->
    io_lib:format("device package failed: ~p - ~p", [Type, Reason]);
format_error(Reason) ->
    io_lib:format("device package failed: ~p", [Reason]).
