%%% @doc `rebar3 device package' — generate packaged device BEAM archives.
%%%
%%% Walks one or more source directories for `dev_*.erl' files, groups
%%% root + helpers, and writes a generated BEAM archive per device into
%%% the configured output directory.
-module(plugin_prv_package).
-export([init/1, do/1, format_error/1]).

-define(NAMESPACE, device).
-define(PROVIDER, package).
-define(DEPS, [{default, app_discovery}, {default, compile}]).

init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {namespace, ?NAMESPACE},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 device package"},
        {opts, opts()},
        {short_desc, "Generate packaged device BEAM archives."},
        {desc,
            "Scan dev_* Erlang sources, group root + helpers, and emit "
            "_hb_device_<name>_<hash> BEAM archives. Output goes to the "
            "configured --output-dir (default: _build/device-packages)."
        }
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    Args = plugin_args:parse(State, "_build/device-packages"),
    case run_with_args(Args) of
        {ok, _Pkgs} -> {ok, State};
        {error, Reason} -> {error, format_error(Reason)}
    end.

run_with_args(Args) ->
    Dirs = maps:get(<<"device-src">>, Args),
    Output = maps:get(<<"output-dir">>, Args),
    Roots = maps:get(<<"device-roots">>, Args, all),
    % Scan the source directory for all device groups.
    Groups = hb_packager:scan(Dirs, #{ <<"device-roots">> => Roots }),
    OutputBin = hb_util:bin(Output),
    ok = filelib:ensure_dir(filename:join(binary_to_list(OutputBin), ".keep")),
    % Package each device group, and write to the output directory.
    Pkgs =
        lists:map(
            fun(Group) ->
                % Package the device group.
                Pkg = hb_packager:package(Group, #{}),
                % Write the package to the output directory.
                write_pkg(OutputBin, Pkg),
                Pkg
            end,
            Groups
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

opts() -> plugin_args:opts().
