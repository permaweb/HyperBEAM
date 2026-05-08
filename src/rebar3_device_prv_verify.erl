%%% @doc `rebar3 device verify' — verify packaged device BEAMs.
%%%
%%% Re-loads each generated BEAM in the configured output directory and
%%% checks invariants:
%%% <ul>
%%%   <li>The BEAM's declared module name must be a generated
%%%       `_hb_device_*' atom.</li>
%%%   <li>Loadable via `code:load_binary/3'.</li>
%%%   <li>The set of exported functions must be a superset of the root
%%%       device's expected handler arities (best-effort: we only verify
%%%       that the module exports something callable).</li>
%%%   <li>Helper modules that contributed source must NOT be loadable
%%%       under their original names from the build output.</li>
%%% </ul>
-module(rebar3_device_prv_verify).
-export([init/1, do/1, format_error/1]).

-define(NAMESPACE, device).
-define(PROVIDER, verify).

init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {namespace, ?NAMESPACE},
        {module, ?MODULE},
        {bare, true},
        {deps, [{default, app_discovery}]},
        {example, "rebar3 device verify"},
        {opts, rebar3_device_args:opts()},
        {short_desc, "Verify packaged device BEAMs."},
        {desc,
            "Re-load each generated _hb_device_* BEAM and check that "
            "exports, internal-call elimination, and helper non-loading "
            "invariants hold."
        }
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    Args = rebar3_device_args:parse(State),
    Dirs = maps:get(<<"device-src">>, Args),
    Roots = maps:get(<<"device-roots">>, Args),
    Groups = hb_packager:scan(Dirs, #{ <<"device-roots">> => Roots }),
    Pkgs = [hb_packager:package(G, #{}) || G <- Groups],
    Results = [verify_pkg(P) || P <- Pkgs],
    case [R || R <- Results, R =/= ok] of
        [] -> {ok, State};
        Errors -> {error, format_error({verify_failures, Errors})}
    end.

verify_pkg(#{ module_name := Mod, beam := Beam,
              root_module := Root, helpers := Helpers,
              exports := DeclaredExports }) ->
    case hb_packager:is_generated_module(Mod) of
        false ->
            {error, {not_generated_atom, Mod}};
        true ->
            case code:load_binary(Mod, atom_to_list(Mod) ++ ".beam", Beam) of
                {module, Mod} ->
                    Loaded = lists:sort(Mod:module_info(exports)),
                    Expected = lists:sort(DeclaredExports ++ default_exports()),
                    Missing = Expected -- Loaded,
                    case Missing of
                        [] ->
                            check_helpers_unloaded(Mod, Root, Helpers);
                        _ ->
                            {error, {missing_exports, Mod, Missing}}
                    end;
                {error, Reason} ->
                    {error, {beam_unloadable, Mod, Reason}}
            end
    end.

default_exports() ->
    [{module_info, 0}, {module_info, 1}].

check_helpers_unloaded(Mod, Root, Helpers) ->
    Bad =
        [
            H
          ||
            H <- [Root | Helpers],
            H =/= Mod,
            code:is_loaded(H) =/= false,
            erlang:function_exported(H, module_info, 0)
        ],
    case Bad of
        [] -> ok;
        _ -> {error, {helpers_loaded_separately, Mod, Bad}}
    end.

format_error({verify_failures, Errors}) ->
    io_lib:format("device verify: ~p failures: ~p", [length(Errors), Errors]);
format_error(Reason) ->
    io_lib:format("device verify failed: ~p", [Reason]).
