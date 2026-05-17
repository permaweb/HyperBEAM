%%% @doc `rebar3 device test' — package devices, build a preloaded-store,
%%% and run their generated EUnit suites through the resulting store.
%%%
%%% This is the developer's primary smoke-test loop: every device under
%%% the configured source directory is packaged, every device's spec/
%%% impl message is signed and indexed, and then its generated EUnit
%%% suites run against the just-built store.
-module(plugin_prv_test).
-export([init/1, do/1, format_error/1]).

-define(PROVIDER, test).

init(State) ->
    plugin_args:provider(
        State,
        ?PROVIDER,
        ?MODULE,
        "rebar3 device test",
        "Run device EUnit against a fresh preloaded-store.",
        "Package and preload devices, then run their generated EUnit suites."
    ).

do(State) ->
    Args = plugin_args:parse(State, "_build/device-test-store"),
    CoreTests = maybe_compile_core_test_modules(Args),
    % Build a complete store from the configured source set so selected
    % device tests can resolve their dependencies.
    {ok, Result} =
        plugin_prv_preload:run(
            Args#{ <<"device-roots">> => all, <<"test">> => true },
            #{}
        ),
    Roots = maps:get(<<"device-roots">>, Args, all),
    Groups = plugin_args:scan_devices(Args),
    % Use the exact packages written to the test preloaded-store.
    SelectedRoots = [maps:get(root, G) || G <- Groups],
    Pkgs =
        [
            Pkg
         ||
            Pkg <- maps:get(pkgs, Result),
            lists:member(maps:get(root_module, Pkg), SelectedRoots)
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
            with_preloaded_test_modules(
                Roots,
                fun(TestModules) ->
                    with_core_test_modules(
                        CoreTests,
                        fun(CoreModules) ->
                            test_modules(
                                State,
                                Names,
                                CoreModules,
                                Modules ++ TestModules,
                                Result
                            )
                        end
                    )
                end
            )
    end.

test_modules(State, Names, CoreModules, DeviceModules, Result) ->
    Tests = test_order(CoreModules, DeviceModules),
    rebar_api:info("device test: running EUnit modules ~p", [Tests]),
    Env = setup_device_tests(Names, Result),
    EUnitResult =
        try eunit:test(Tests, [verbose, {scale_timeouts, 10}])
        after restore_preloaded_env(Env)
        end,
    case EUnitResult of
        ok -> {ok, State};
        error -> {error, format_error(eunit_failed)};
        Other -> {error, format_error({eunit_failed, Other})}
    end.

test_order(CoreModules, DeviceModules) ->
    CoreFirst = [Mod || Mod <- CoreModules, Mod =/= hb_opts],
    CoreLast = [Mod || Mod <- CoreModules, Mod =:= hb_opts],
    CoreFirst ++ DeviceModules ++ CoreLast.

setup_device_tests(Names, Result) ->
    Env = set_preloaded_env(Result),
    try
        Opts = test_opts(Result),
        case load_devices(Names, Opts) of
            ok ->
                start_apps(),
                Env;
            {error, LoadError} ->
                erlang:error(LoadError)
        end
    catch Class:Error:Stacktrace ->
        restore_preloaded_env(Env),
        erlang:raise(Class, Error, Stacktrace)
    end.

set_preloaded_env(Result) ->
    StorePath = hb_util:bin(hb_maps:get(<<"name">>, maps:get(store, Result))),
    Index = hb_util:bin(maps:get(index, Result)),
    OldStore = os:getenv("HB_PRELOADED_STORE"),
    OldIndex = os:getenv("HB_PRELOADED_DEVICES_INDEX"),
    os:putenv("HB_PRELOADED_STORE", binary_to_list(StorePath)),
    os:putenv("HB_PRELOADED_DEVICES_INDEX", binary_to_list(Index)),
    erase_preloaded_env_cache(),
    {OldStore, OldIndex}.

restore_preloaded_env({OldStore, OldIndex}) ->
    restore_env("HB_PRELOADED_STORE", OldStore),
    restore_env("HB_PRELOADED_DEVICES_INDEX", OldIndex),
    erase_preloaded_env_cache().

restore_env(Name, false) ->
    os:unsetenv(Name);
restore_env(Name, Value) ->
    os:putenv(Name, Value).

erase_preloaded_env_cache() ->
    erase({os_env, "HB_PRELOADED_STORE"}),
    erase({os_env, "HB_PRELOADED_DEVICES_INDEX"}),
    erase({processed_env, <<"preloaded-store">>}),
    erase({processed_env, <<"preloaded-devices-index">>}).

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

with_preloaded_test_modules(Roots, Fun) when is_function(Fun, 1) ->
    {Ebin, Modules} = compile_preloaded_test_modules(),
    code:add_patha(Ebin),
    try Fun(test_modules_to_run(Modules, Roots))
    after
        code:del_path(Ebin),
        lists:foreach(fun purge_test_module/1, Modules),
        file:del_dir_r(filename:dirname(Ebin))
    end.

compile_preloaded_test_modules() ->
    compile_test_modules(
        "_build/device-test-fixtures",
        lists:sort(filelib:wildcard("src/preloaded/test/hb_*.erl")),
        preloaded_test_compile_failed
    ).

test_compile_opts(Ebin) ->
    [
        debug_info,
        {d, 'TEST'},
        {outdir, Ebin},
        {i, "src"},
        {i, "src/core"}
    ].

test_modules_to_run(_Modules, Roots) when Roots =/= all ->
    [];
test_modules_to_run(Modules, all) ->
    [
        Mod
     ||
        Mod <- Modules,
        not lists:suffix("_test_utils", atom_to_list(Mod))
    ].

purge_test_module(Mod) ->
    code:purge(Mod),
    code:delete(Mod),
    code:purge(Mod).

maybe_compile_core_test_modules(#{ <<"with-core">> := true }) ->
    compile_core_test_modules();
maybe_compile_core_test_modules(_Args) ->
    none.

with_core_test_modules(none, Fun) when is_function(Fun, 1) ->
    Fun([]);
with_core_test_modules({Ebin, Modules}, Fun) when is_function(Fun, 1) ->
    code:add_patha(Ebin),
    lists:foreach(
        fun(Mod) -> load_test_module(Mod, core_test_load_failed) end,
        Modules
    ),
    rebar_api:info(
        "device test: running core and packaged-device EUnit together",
        []
    ),
    try Fun(Modules)
    after
        code:del_path(Ebin),
        file:del_dir_r(filename:dirname(Ebin))
    end.

compile_core_test_modules() ->
    compile_test_modules(
        "_build/device-test-core",
        core_test_paths(),
        core_test_compile_failed
    ).

core_test_paths() ->
    Paths =
        filelib:wildcard("src/*.erl") ++
        filelib:wildcard("src/core/*.erl") ++
        filelib:wildcard("src/forge/*.erl"),
    First = "src/core/hb_test_parallel.erl",
    [First || lists:member(First, Paths)] ++ lists:sort(Paths -- [First]).

compile_test_modules(BuildDir, Paths, ErrorTag) ->
    Ebin = filename:join([BuildDir, "ebin"]),
    file:del_dir_r(filename:dirname(Ebin)),
    ok = filelib:ensure_dir(filename:join(Ebin, "x")),
    {Ebin, lists:usort([compile_test_module(Path, Ebin, ErrorTag)
        || Path <- Paths])}.

compile_test_module(Path, Ebin, ErrorTag) ->
    case compile:file(Path, test_compile_opts(Ebin)) of
        {ok, Mod} -> Mod;
        {ok, Mod, _} -> Mod;
        Error -> error({ErrorTag, Path, Error})
    end.

load_test_module(Mod, ErrorTag) ->
    code:purge(Mod),
    code:delete(Mod),
    case code:load_file(Mod) of
        {module, Mod} -> ok;
        {error, Reason} -> error({ErrorTag, Mod, Reason})
    end.

format_error(Reason) ->
    io_lib:format("device test failed: ~p", [Reason]).
