%%% @doc `rebar3 device local' - start a local shell with packaged devices.
%%%
%%% Builds a preloaded-store exactly like `device preload', points
%%% `HB_PRELOADED_STORE' and `HB_PRELOADED_DEVICES_INDEX' at that store for the
%%% duration of the shell, then delegates to Rebar's normal shell provider.
-module(hb_forge_local).
-export([init/1, do/1, format_error/1]).

-define(PROVIDER, local).

%% @doc Register the `local' provider with rebar3.
init(State) ->
    hb_forge_args:provider(
        State,
        ?PROVIDER,
        ?MODULE,
        "rebar3 device local",
        "Start a local node with a generated preloaded-store.",
        "Build a preloaded-store for the selected devices, then start shell."
    ).

%% @doc Build a preloaded-store and start a shell pointed at it.
do(State) ->
    case hb_forge_args:maybe_help(State, ?MODULE) of
        true -> {ok, State};
        false -> do_run(State)
    end.

do_run(State) ->
    Args = hb_forge_args:parse(State, <<"_build/device-local-store">>),
    case hb_forge_preload:run(Args, #{}) of
        {ok, Result} ->
            rebar_api:info(
                "device local: using store ~s, index ~s",
                [
                    hb_maps:get(<<"name">>, maps:get(store, Result)),
                    maps:get(index, Result)
                ]
            ),
            hb_forge_args:with_preloaded_env(
                Result,
                fun() -> rebar_prv_shell:do(with_hb_shell_app(State)) end
            );
        {error, Reason} ->
            {error, format_error(Reason)}
    end.

%% @doc Ensure external device repos start the HyperBEAM app in local shells.
with_hb_shell_app(State) ->
    ShellOpts = rebar_state:get(State, shell, []),
    rebar_state:set(State, shell, ensure_hb_app(ShellOpts)).

ensure_hb_app(ShellOpts) ->
    Apps = proplists:get_value(apps, ShellOpts, []),
    lists:keystore(apps, 1, ShellOpts, {apps, lists:usort([hb | Apps])}).

%% @doc Render provider failures for rebar3.
format_error(Reason) ->
    io_lib:format("device local failed: ~p", [Reason]).
