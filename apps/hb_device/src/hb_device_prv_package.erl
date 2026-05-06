%%% @doc Rebar3 provider for packaging HyperBEAM device namespaces.
-module(hb_device_prv_package).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(NAMESPACE, hb_device).
-define(PROVIDER, package).

%% @doc Register the `rebar3 hb_device package' provider.
init(State) ->
    Provider =
        providers:create(
            [
                {name, ?PROVIDER},
                {namespace, ?NAMESPACE},
                {module, ?MODULE},
                {bare, true},
                {deps, []},
                {example, "rebar3 hb_device package"},
                {opts, hb_device_prv_utils:opts()},
                {short_desc, "Package multi-module HyperBEAM devices"},
                {desc, "Flatten dev_* Erlang namespaces into single BEAMs."}
            ]
        ),
    {ok, rebar_state:add_provider(State, Provider)}.

%% @doc Package configured HyperBEAM device namespaces.
do(State) ->
    Opts = hb_device_prv_utils:packager_opts(State),
    try
        hb_device_packager:package_devices(Opts),
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
