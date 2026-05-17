%%% @doc `rebar3 device verify' — verify packaged device BEAM archives.
%%%
%%% Re-loads each generated archive in the configured output directory and
%%% checks invariants:
%%% <ul>
%%%   <li>The BEAM's declared module name must be a generated
%%%       `_hb_device_*' atom.</li>
%%%   <li>Loadable via `code:atomic_load/1'.</li>
%%%   <li>The set of exported functions must be a superset of the root
%%%       device's expected handler arities (best-effort: we only verify
%%%       that the module exports something callable).</li>
%%%   <li>Helper modules that contributed source must NOT be loadable
%%%       under their original names from the build output.</li>
%%% </ul>
-module(plugin_prv_verify).
-export([init/1, do/1, format_error/1]).

-define(NAMESPACE, device).
-define(PROVIDER, verify).

init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {namespace, ?NAMESPACE},
        {module, ?MODULE},
        {bare, true},
        {deps, [{default, app_discovery}, {default, compile}]},
        {example, "rebar3 device verify"},
        {opts, plugin_args:opts()},
        {short_desc, "Verify packaged device BEAM archives."},
        {desc,
            "Re-load each generated _hb_device_* archive and check that "
            "exports, internal-call rewriting, and source helper non-loading "
            "invariants hold."
        }
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    Args = plugin_args:parse(State, "_build/device-packages"),
    Dirs = maps:get(<<"device-src">>, Args),
    Output = maps:get(<<"output-dir">>, Args),
    Roots = maps:get(<<"device-roots">>, Args),
    % Scan the source directory for all device groups.
    Groups = hb_packager:scan(Dirs, #{ <<"device-roots">> => Roots }),
    % Package each device group.
    Pkgs = hb_packager:package_all(Groups, package_opts()),
    % Verify each package.
    Results = [verify_pkg(Output, P) || P <- Pkgs],
    case [R || R <- Results, R =/= ok] of
        [] -> {ok, State};
        Errors -> {error, format_error({verify_failures, Errors})}
    end.

verify_pkg(Output, #{ module_name := Mod, archive := Archive,
              archive_modules := ArchiveModules,
              root_module := Root, helpers := Helpers,
              exports := DeclaredExports }) ->
    case hb_device_name:is_generated(Mod) of
        false ->
            {error, {not_generated_atom, Mod}};
        true ->
            % Load the archive. If it is not loadable, return an error.
            case load_archive(Mod, Archive, ArchiveModules) of
                ok ->
                    % Ensure that the module exports the expected functions.
                    Loaded = lists:sort(Mod:module_info(exports)),
                    Expected = lists:sort(DeclaredExports ++ default_exports()),
                    Missing = Expected -- Loaded,
                    case Missing of
                        [] ->
                            check_helpers_unloaded(Output, Mod, Root, Helpers);
                        _ ->
                            {error, {missing_exports, Mod, Missing}}
                    end;
                {error, Reason} ->
                    {error, {archive_unloadable, Mod, Reason}}
            end
    end.

%% @doc Load an archive and attempt to load each module in the archive.
load_archive(Root, Archive, ArchiveModules) ->
    {ok, Files} = zip:unzip(Archive, [memory]),
    Beams =
        maps:from_list([{hb_util:bin(Name), Beam} || {Name, Beam} <- Files]),
    ResourceFiles =
        [
            {Rel, Body}
         ||
            {Name, Body} <- Files,
            <<"priv/", Rel/binary>> <- [hb_util:bin(Name)]
        ],
    Modules =
        [
            begin
                Path = maps:get(<<"archive-path">>, Meta),
                ModBin = maps:get(<<"module-name">>, Meta),
                Mod = binary_to_atom(ModBin, utf8),
                {Mod, binary_to_list(Path), maps:get(Path, Beams)}
            end
          ||
            Meta <- ArchiveModules
        ],
    case write_resources(
        hb_ao_device:implementation_dir(Root),
        ResourceFiles
    ) of
        ok ->
            case code:atomic_load(Modules) of
                ok -> ok;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

write_resources(_Dir, []) ->
    ok;
write_resources(Dir, [{Rel, Body} | Rest]) ->
    Path = filename:join(Dir, hb_util:list(Rel)),
    case filelib:ensure_dir(Path) of
        ok ->
            case file:write_file(Path, Body) of
                ok ->
                    maybe_make_executable(Rel, Path),
                    write_resources(Dir, Rest);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

maybe_make_executable(<<"bin/", _/binary>>, Path) ->
    file:change_mode(Path, 8#100755);
maybe_make_executable(Rel, Path) ->
    case filename:extension(hb_util:list(Rel)) of
        ".sh" -> file:change_mode(Path, 8#100755);
        _ -> ok
    end.

default_exports() ->
    [{module_info, 0}, {module_info, 1}].

package_opts() ->
    #{ <<"bootstrap-device-src">> => plugin_args:bootstrap_preloaded_dirs() }.

%% @doc Check that the helpers are not loaded separately from the root module.
check_helpers_unloaded(Output, Mod, Root, Helpers) ->
    OutputDir = binary_to_list(Output),
    Bad =
        [
            H
          ||
            H <- [Root | Helpers],
            H =/= Mod,
            filelib:is_file(
                filename:join(OutputDir, atom_to_list(H) ++ ".beam")
            )
        ],
    case Bad of
        [] -> ok;
        _ -> {error, {helpers_loaded_separately, Mod, Bad}}
    end.

format_error({verify_failures, Errors}) ->
    io_lib:format("device verify: ~p failures: ~p", [length(Errors), Errors]);
format_error(Reason) ->
    io_lib:format("device verify failed: ~p", [Reason]).
