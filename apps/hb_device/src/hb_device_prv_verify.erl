%%% @doc Rebar3 provider for verifying packaged HyperBEAM device namespaces.
-module(hb_device_prv_verify).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(NAMESPACE, hb_device).
-define(PROVIDER, verify).

%% @doc Register the `rebar3 hb_device verify' provider.
init(State) ->
    Provider =
        providers:create(
            [
                {name, ?PROVIDER},
                {namespace, ?NAMESPACE},
                {module, ?MODULE},
                {bare, true},
                {deps, []},
                {example, "rebar3 hb_device verify"},
                {opts, hb_device_prv_utils:opts()},
                {short_desc, "Verify packaged HyperBEAM devices"},
                {desc, "Package devices and prove each generated BEAM loads."}
            ]
        ),
    {ok, rebar_state:add_provider(State, Provider)}.

%% @doc Package and load-check configured HyperBEAM device namespaces.
do(State) ->
    Opts = hb_device_prv_utils:packager_opts(State),
    try
        Results = hb_device_packager:verify(Opts),
        io:format(
            user,
            "Verified ~p packaged device namespace(s).~n",
            [length(Results)]
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
