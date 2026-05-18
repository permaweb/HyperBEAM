%%% @doc `rebar3 device test' - package devices, build a preloaded-store,
%%% and run their generated EUnit suites through the resulting store.
%%%
%%% This is the developer's primary smoke-test loop: every device under
%%% the configured source directory is packaged, every device's spec/
%%% impl message is signed and indexed, and then its generated EUnit
%%% suites run against the just-built store.
-module(hb_forge_test).
-export([init/1, do/1, format_error/1]).

-define(PROVIDER, test).

%% @doc Register the `test' provider with rebar3.
init(State) ->
    hb_forge_args:provider(
        State,
        ?PROVIDER,
        ?MODULE,
        "rebar3 device test",
        "Run device EUnit against a fresh preloaded-store.",
        "Package and preload devices, then run their generated EUnit suites."
    ).

%% @doc Build a test preloaded-store and run selected package EUnit modules.
do(State) ->
    Args = hb_forge_args:parse(State, <<"_build/device-test-store">>),
    CoreTests = maybe_compile_core_test_modules(Args),
    % Build a complete store from the configured source set so selected
    % device tests can resolve their dependencies.
    {ok, Result} =
        hb_forge_preload:run(
            Args#{ <<"device-roots">> => all, <<"test">> => true },
            #{}
        ),
    Roots = maps:get(<<"device-roots">>, Args, all),
    Groups = hb_forge_args:scan_devices(Args),
    % Correlate the selected source groups with their packages by the
    % device name they implement, then take the generated modules to
    % EUnit from each archive itself (not packager internals).
    SelectedNames =
        lists:usort([hb_packager:group_device_name(G) || G <- Groups]),
    Pkgs =
        [
            Pkg
         ||
            Pkg <- maps:get(pkgs, Result),
            lists:member(maps:get(device_name, Pkg), SelectedNames)
        ],
    Modules = lists:usort(lists:append([archive_modules(Pkg) || Pkg <- Pkgs])),
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

%% @doc The generated module atoms inside a package's archive.
archive_modules(Pkg) ->
    {ok, Modules, _Resources} =
        hb_device_archive:contents(maps:get(archive, Pkg)),
    [Mod || {Mod, _File, _Beam} <- Modules].

%% @doc Run EUnit with the generated device-store environment installed.
test_modules(State, Names, CoreModules, DeviceModules, Result) ->
    Tests = test_order(CoreModules, DeviceModules),
    rebar_api:info("device test: running EUnit modules ~p", [Tests]),
    Env = setup_device_tests(Names, Result),
    EUnitResult =
        try eunit:test(Tests, [verbose, {scale_timeouts, 10}])
        after hb_forge_args:restore_preloaded_env(Env)
        end,
    case EUnitResult of
        ok -> {ok, State};
        error -> {error, format_error(eunit_failed)};
        Other -> {error, format_error({eunit_failed, Other})}
    end.

%% @doc Run core tests first, but defer `hb_opts' until env vars are set.
test_order(CoreModules, DeviceModules) ->
    CoreFirst = [Mod || Mod <- CoreModules, Mod =/= hb_opts],
    CoreLast = [Mod || Mod <- CoreModules, Mod =:= hb_opts],
    CoreFirst ++ DeviceModules ++ CoreLast.

%% @doc Load packaged devices and start apps needed by device test modules.
setup_device_tests(Names, Result) ->
    Env = hb_forge_args:set_preloaded_env(Result),
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
        hb_forge_args:restore_preloaded_env(Env),
        erlang:raise(Class, Error, Stacktrace)
    end.

%% @doc Build runtime opts that trust the just-written implementation IDs.
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

%% @doc Resolve each device name through the freshly-built preloaded-store.
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

%% @doc Start runtime apps needed by packaged-device integration tests.
start_apps() ->
    lists:foreach(fun start_app/1, [hackney, prometheus, hb]).

%% @doc Ensure one runtime application is available before EUnit starts.
start_app(App) ->
    case application:ensure_all_started(App) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({app_start_failed, App, Reason})
    end.

%% @doc Compile preloaded test-only modules for whole-library device tests.
with_preloaded_test_modules(Roots, Fun) when is_function(Fun, 1) ->
    {Ebin, Modules} = compile_preloaded_test_modules(),
    code:add_patha(hb_util:list(Ebin)),
    try Fun(test_modules_to_run(Modules, Roots))
    after
        code:del_path(hb_util:list(Ebin)),
        lists:foreach(fun purge_test_module/1, Modules),
        file:del_dir_r(filename:dirname(Ebin))
    end.

%% @doc Compile `src/preloaded/test' modules into an isolated ebin.
compile_preloaded_test_modules() ->
    compile_test_modules(
        <<"_build/device-test-fixtures">>,
        lists:sort(filelib:wildcard("src/preloaded/test/hb_*.erl")),
        preloaded_test_compile_failed
    ).

%% @doc Return compile options used for temporary test modules.
test_compile_opts(Ebin) ->
    [
        debug_info,
        {d, 'TEST'},
        {outdir, hb_util:list(Ebin)},
        {i, "src"},
        {i, "src/core"}
    ].

%% @doc Only run shared preloaded test vectors when testing the full library.
test_modules_to_run(_Modules, Roots) when Roots =/= all ->
    [];
test_modules_to_run(Modules, all) ->
    [
        Mod
     ||
        Mod <- Modules,
        not lists:suffix("_test_utils", atom_to_list(Mod))
    ].

%% @doc Remove a temporary test module from the code server.
purge_test_module(Mod) ->
    code:purge(Mod),
    code:delete(Mod),
    code:purge(Mod).

%% @doc Compile core tests only when the caller opts into `--with-core'.
maybe_compile_core_test_modules(#{ <<"with-core">> := true }) ->
    compile_core_test_modules();
maybe_compile_core_test_modules(_Args) ->
    none.

%% @doc Run `Fun' with core test modules available when requested.
with_core_test_modules(none, Fun) when is_function(Fun, 1) ->
    Fun([]);
with_core_test_modules({Ebin, Modules}, Fun) when is_function(Fun, 1) ->
    code:add_patha(hb_util:list(Ebin)),
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
        code:del_path(hb_util:list(Ebin)),
        file:del_dir_r(filename:dirname(Ebin))
    end.

%% @doc Compile core test modules into an isolated ebin.
compile_core_test_modules() ->
    compile_test_modules(
        <<"_build/device-test-core">>,
        core_test_paths(),
        core_test_compile_failed
    ).

%% @doc Return source paths that make up the core EUnit suite.
core_test_paths() ->
    Paths =
        filelib:wildcard("src/*.erl") ++
        filelib:wildcard("src/core/*.erl") ++
        filelib:wildcard("src/forge/*.erl"),
    First = "src/core/hb_test_parallel.erl",
    [First || lists:member(First, Paths)] ++ lists:sort(Paths -- [First]).

%% @doc Compile a group of test modules to a temporary ebin.
compile_test_modules(BuildDir, Paths, ErrorTag) ->
    Ebin = filename:join([BuildDir, "ebin"]),
    file:del_dir_r(filename:dirname(Ebin)),
    ok = filelib:ensure_dir(filename:join(Ebin, "x")),
    {
        Ebin,
        lists:usort(
            [
                compile_test_module(Path, Ebin, ErrorTag)
            ||
                Path <- Paths
            ]
        )
    }.

%% @doc Compile one test module and raise a tagged error on failure.
compile_test_module(Path, Ebin, ErrorTag) ->
    case compile:file(Path, test_compile_opts(Ebin)) of
        {ok, Mod} -> Mod;
        {ok, Mod, _} -> Mod;
        Error -> error({ErrorTag, Path, Error})
    end.

%% @doc Load a compiled temporary test module from the active code path.
load_test_module(Mod, ErrorTag) ->
    code:purge(Mod),
    code:delete(Mod),
    case code:load_file(Mod) of
        {module, Mod} -> ok;
        {error, Reason} -> error({ErrorTag, Mod, Reason})
    end.

%% @doc Render provider failures for rebar3.
format_error(Reason) ->
    io_lib:format("device test failed: ~p", [Reason]).
