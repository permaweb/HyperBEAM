%%% @doc Rebar3 provider for building the local preloaded device store.
-module(hb_device_prv_preload).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(NAMESPACE, hb_device).
-define(PROVIDER, preload).

%% @doc Register the `rebar3 hb_device preload' provider.
init(State) ->
    Provider =
        providers:create(
            [
                {name, ?PROVIDER},
                {namespace, ?NAMESPACE},
                {module, ?MODULE},
                {bare, true},
                {deps, [{default, compile}]},
                {example, "rebar3 hb_device preload"},
                {opts, hb_device_prv_utils:opts()},
                {short_desc, "Build the HyperBEAM preloaded device store"},
                {desc, "Package, sign, and index local dev_* devices."}
            ]
        ),
    {ok, rebar_state:add_provider(State, Provider)}.

%% @doc Package and write preloaded device messages to a filesystem store.
do(State) ->
    ensure_project_paths(State),
    Opts = hb_device_prv_utils:preload_opts(State),
    try
        Metadata = hb_device_preloader:build(Opts),
        io:format(
            user,
            "Preloaded ~p device namespace(s) into ~s.~n",
            [
                length(maps:get(<<"devices">>, Metadata)),
                maps:get(<<"name">>, maps:get(<<"store">>, Metadata))
            ]
        ),
        {ok, State}
    catch
        Class:Reason:Stacktrace ->
            {error, {?MODULE, {Class, Reason, Stacktrace}}}
    end.

%% @doc Format provider errors for rebar3.
format_error({Class, Reason, _Stacktrace}) ->
    io_lib:format("~p:~p", [Class, Reason]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% @doc Add compiled project applications and dependencies to the plugin VM path.
ensure_project_paths(State) ->
    maybe_add_rebar_paths(State),
    code:add_paths(filelib:wildcard("_build/*/lib/*/ebin")).

maybe_add_rebar_paths(State) ->
    try
        code:add_paths(rebar_state:code_paths(State, all_deps)),
        code:add_paths(
            [
                rebar_app_info:ebin_dir(App)
            ||
                App <- rebar_state:project_apps(State)
            ]
        )
    catch
        error:undef -> ok
    end.
