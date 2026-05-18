%%% @doc `rebar3 device verify' - verify packaged device BEAM archives.
%%%
%%% Re-loads each generated archive in the configured output directory and
%%% checks invariants:
%%% <ul>
%%%   <li>The BEAM's declared module name must be a generated
%%%       `_hb_device_*' atom.</li>
%%%   <li>Loadable into the code server with normal module loading.</li>
%%%   <li>The set of exported functions must be a superset of the root
%%%       device's expected handler arities (best-effort: we only verify
%%%       that the module exports something callable).</li>
%%%   <li>Helper modules that contributed source must NOT be loadable
%%%       under their original names from the build output.</li>
%%% </ul>
-module(hb_forge_verify).
-export([init/1, do/1, format_error/1]).

-define(PROVIDER, verify).

%% @doc Register the `verify' provider with rebar3.
init(State) ->
    hb_forge_args:provider(
        State,
        ?PROVIDER,
        ?MODULE,
        "rebar3 device verify",
        "Verify packaged device BEAM archives.",
        "Re-load generated _hb_device_* archives and check invariants."
    ).

%% @doc Package selected devices and verify the generated archive invariants.
do(State) ->
    Args = hb_forge_args:parse(State, <<"_build/device-packages">>),
    Output = maps:get(<<"output-dir">>, Args),
    Pkgs =
        hb_packager:package_all(
            hb_forge_args:scan_devices(Args),
            hb_forge_args:package_opts()
        ),
    % Verify each package.
    Results = [verify_pkg(Output, P) || P <- Pkgs],
    case [R || R <- Results, R =/= ok] of
        [] -> {ok, State};
        Errors -> {error, format_error({verify_failures, Errors})}
    end.

%% @doc Verify one generated package's module name, loadability and exports.
verify_pkg(Output, #{ module_name := Mod, archive := Archive,
              root_module := Root, helpers := Helpers,
              exports := DeclaredExports }) ->
    case hb_device_name:is_generated(Mod) of
        false ->
            {error, {not_generated_atom, Mod}};
        true ->
            % Load the archive. If it is not loadable, return an error.
            case load_archive(Mod, Archive) of
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
load_archive(Root, Archive) ->
    case hb_device_archive:load(
        atom_to_binary(Root, utf8),
        Archive,
        #{},
        #{}
    ) of
        {ok, Root} -> ok;
        {error, _} = Error -> Error
    end.

%% @doc Return BEAM exports present on every module.
default_exports() ->
    [{module_info, 0}, {module_info, 1}].

%% @doc Check that the helpers are not loaded separately from the root module.
check_helpers_unloaded(Output, Mod, Root, Helpers) ->
    OutputDir = hb_util:list(Output),
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

%% @doc Render provider failures for rebar3.
format_error({verify_failures, Errors}) ->
    io_lib:format("device verify: ~p failures: ~p", [length(Errors), Errors]);
format_error(Reason) ->
    io_lib:format("device verify failed: ~p", [Reason]).
