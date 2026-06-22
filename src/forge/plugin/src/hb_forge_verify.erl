%%% @doc `rebar3 device verify' - verify packaged device BEAM archives.
%%%
%%% Packages every selected device through the real Forge path and loads
%%% each archive with the exact runtime loader
%%% ({@link hb_device_archive:load/4}). That loader is the contract: it
%%% only succeeds when every BEAM in the archive is a generated
%%% `_hb_device_*' module inside the root namespace and the whole set
%%% loads cleanly into the code server.
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
        "Package every device and load its archive via the runtime loader."
    ).

%% @doc Package selected devices and load each archive via the runtime loader.
do(State) ->
    hb_forge_args:run_provider(State, ?MODULE, fun verify/1).

verify(State) ->
    Args = hb_forge_args:parse(State, <<"_build/device-packages">>),
    Failures =
        hb_forge_seed:with_forge_bootstrap(
            hb_forge_args:package_opts(Args),
            fun(Opts) ->
                Pkgs =
                    hb_packager:package_all(
                        hb_forge_args:scan_devices(Args), Opts
                    ),
                [R || P <- Pkgs, (R = verify_pkg(P)) =/= ok]
            end
        ),
    case Failures of
        [] -> {ok, State};
        Errors -> {error, format_error({verify_failures, Errors})}
    end.

%% @doc Load one package's archive exactly as the runtime does. The
%% loader rejects anything whose BEAMs are not generated `_hb_device_*'
%% atoms in the root namespace, so a clean `{ok, Mod}' is the proof.
verify_pkg(#{ module_name := Mod, archive := Archive }) ->
    case
        hb_device_archive:load(atom_to_binary(Mod, utf8), Archive, #{}, #{})
    of
        {ok, Mod} -> ok;
        {ok, Other} -> {wrong_root_module, Mod, Other};
        {error, Reason} -> {archive_unloadable, Mod, Reason}
    end.

%% @doc Render provider failures for rebar3.
format_error({verify_failures, Errors}) ->
    io_lib:format("device verify: ~p failures: ~p", [length(Errors), Errors]);
format_error(Reason) ->
    io_lib:format("device verify failed: ~p", [Reason]).
