%%% @doc `rebar3 device test' — package devices, build a preloaded-store,
%%% and run their generated EUnit suites through the resulting store.
%%%
%%% This is the developer's primary smoke-test loop: every device under
%%% the configured source directory is packaged, every device's spec/
%%% impl message is signed and indexed, and then its generated EUnit
%%% suites run against the just-built store.
-module(plugin_prv_test).
-export([init/1, do/1, format_error/1]).

-define(NAMESPACE, device).
-define(PROVIDER, test).

init(State) ->
    % Create the provider.
    Provider =
        providers:create([
            {name, ?PROVIDER},
            {namespace, ?NAMESPACE},
            {module, ?MODULE},
            {bare, true},
            {deps, [{default, app_discovery}, {default, compile}]},
            {example, "rebar3 device test"},
            {opts, plugin_args:opts()},
            {short_desc, "Run device EUnit against a fresh preloaded-store."},
            {desc,
                "Package + preload the discovered devices, then run "
                "the device root EUnit suites with the resulting "
                "store as the node's preloaded-store."
            }
        ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    Args = plugin_args:parse(State, "_build/device-test-store"),
    % Build a complete store from the configured source set so selected
    % device tests can resolve their dependencies.
    {ok, Result} =
        plugin_prv_preload:run(
            Args#{ <<"device-roots">> => all, <<"test">> => true },
            #{}
        ),
    Dirs = maps:get(<<"device-src">>, Args),
    Roots = maps:get(<<"device-roots">>, Args, all),
    % Scan the source directory for root device groups.
    Groups = hb_packager:scan(Dirs, #{ <<"device-roots">> => Roots }),
    % Re-package the selected groups with test exports to discover the
    % generated module names that the runtime will load from the store.
    Pkgs = [
        hb_packager:package(G, #{ <<"test">> => true })
     ||
        G <- Groups
    ],
    Modules = lists:usort(lists:append([
        maps:get(module_names, Pkg)
     ||
        Pkg <- Pkgs
    ])),
    Names = [maps:get(device_name, Pkg) || Pkg <- Pkgs],
    case Names of
        [] ->
            rebar_api:info("device test: nothing to test", []),
            {ok, State};
        _ ->
            Opts = test_opts(Result),
            case load_devices(Names, Opts) of
                ok ->
                    rebar_api:info(
                        "device test: running generated modules ~p",
                        [Modules]
                    ),
                    start_apps(),
                    case eunit:test(Modules, [verbose, {scale_timeouts, 10}]) of
                        ok -> {ok, State};
                        error -> {error, format_error(eunit_failed)};
                        Other -> {error, format_error({eunit_failed, Other})}
                    end;
                {error, Reason} ->
                    {error, format_error(Reason)}
            end
    end.

test_opts(Result) ->
    #{
        <<"preloaded-store">> => maps:get(store, Result),
        <<"preloaded-devices-index">> => maps:get(index, Result),
        <<"trusted-devices">> => maps:get(impls, Result),
        <<"device-store">> =>
            #{
                <<"store-module">> => hb_store_volatile,
                <<"name">> =>
                    iolist_to_binary([
                        <<"device-test-">>,
                        integer_to_binary(
                            erlang:unique_integer([positive])
                        )
                    ])
            }
    }.

load_devices([], _Opts) ->
    ok;
load_devices([Name | Names], Opts) ->
    case hb_ao_device:load(Name, Opts) of
        {ok, Mod} ->
            rebar_api:info("device test: loaded ~s as ~p", [Name, Mod]),
            load_devices(Names, Opts);
        {error, Reason} ->
            {error, {device_load_failed, Name, Reason}}
    end.

start_apps() ->
    lists:foreach(fun start_app/1, [hackney, prometheus, hb]).

start_app(App) ->
    case application:ensure_all_started(App) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({app_start_failed, App, Reason})
    end.

format_error(Reason) ->
    io_lib:format("device test failed: ~p", [Reason]).
