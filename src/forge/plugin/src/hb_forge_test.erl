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
    ModuleLabels = device_module_labels(Pkgs),
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
                                ModuleLabels,
                                Args,
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

%% @doc Run EUnit with the generated preloaded-store environment installed.
test_modules(State, Names, CoreModules, DeviceModules, ModuleLabels, Args, Result) ->
    ShowHash = maps:get(<<"show-hash">>, Args, false),
    Env = setup_device_tests(Names, Result),
    EUnitResult =
        try
            Tests = test_order(
                CoreModules,
                device_tests(DeviceModules, ModuleLabels, ShowHash)
            ),
            rebar_api:info(
                "device test: running EUnit modules ~p",
                [test_names(CoreModules, DeviceModules, ModuleLabels, ShowHash)]
            ),
            eunit:test(Tests, [verbose, {scale_timeouts, 10}])
        after restore_test_env(Env)
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

%% @doc Convert generated device modules to readable EUnit descriptors.
device_tests(DeviceModules, _ModuleLabels, true) ->
    DeviceModules;
device_tests(DeviceModules, ModuleLabels, false) ->
    [
        case maps:find(Mod, ModuleLabels) of
            {ok, Label} -> readable_module_tests(Mod, Label);
            error -> Mod
        end
    ||
        Mod <- DeviceModules
    ].

%% @doc Names used in the provider log line before EUnit starts.
test_names(CoreModules, DeviceModules, _ModuleLabels, true) ->
    test_order(CoreModules, DeviceModules);
test_names(CoreModules, DeviceModules, ModuleLabels, false) ->
    test_order(
        CoreModules,
        [maps:get(Mod, ModuleLabels, Mod) || Mod <- DeviceModules]
    ).

%% @doc Map generated archive module atoms back to device-name labels.
device_module_labels(Pkgs) ->
    maps:from_list(lists:append([pkg_module_labels(Pkg) || Pkg <- Pkgs])).

pkg_module_labels(Pkg) ->
    Root = maps:get(module_name, Pkg),
    Device = maps:get(device_name, Pkg),
    [{Mod, module_label(Device, Root, Mod)} || Mod <- archive_modules(Pkg)].

module_label(Device, Root, Root) ->
    binary_to_atom(Device, utf8);
module_label(Device, Root, Mod) ->
    RootBin = atom_to_binary(Root, utf8),
    ModBin = atom_to_binary(Mod, utf8),
    Prefix = <<RootBin/binary, "__">>,
    PrefixSize = byte_size(Prefix),
    Tail =
        case ModBin of
            <<Prefix:PrefixSize/binary, Rest/binary>> -> Rest;
            _ -> ModBin
        end,
    binary_to_atom(<<Device/binary, " [", Tail/binary, "]">>, utf8).

%% @doc Return EUnit descriptors with readable source locations.
readable_module_tests(Mod, Label) ->
    Exports = Mod:module_info(exports),
    lists:foldr(fun(Export, Acc) -> readable_export(Export, Mod, Label, Acc) end, [], Exports).

readable_export({Fun, 0}, Mod, Label, Acc) ->
    Name = atom_to_list(Fun),
    case {lists:suffix("_test", Name), lists:suffix("_test_", Name)} of
        {true, _} ->
            [{{Label, Fun, 0}, {test, Mod, Fun}} | Acc];
        {_, true} ->
            [
                {generator,
                    fun() -> rewrite_test_term(apply(Mod, Fun, []), Mod, Label) end,
                    {Label, Fun, 0}}
             | Acc
            ];
        _ ->
            Acc
    end;
readable_export(_Export, _Mod, _Label, Acc) ->
    Acc.

rewrite_test_term({Line, Test}, Mod, Label) when is_integer(Line), Line >= 0 ->
    {Line, rewrite_test_term(Test, Mod, Label)};
rewrite_test_term({{Mod, Name, Arity}, Test}, Mod, Label) ->
    {{Label, Name, Arity}, rewrite_test_term(Test, Mod, Label)};
rewrite_test_term({test, Mod, Fun}, Mod, Label) ->
    {{Label, Fun, 0}, {test, Mod, Fun}};
rewrite_test_term({Mod, Fun}, Mod, Label) when is_atom(Fun) ->
    {{Label, Fun, 0}, {test, Mod, Fun}};
rewrite_test_term({generator, Mod, Fun}, Mod, Label) ->
    {
        generator,
        fun() -> rewrite_test_term(apply(Mod, Fun, []), Mod, Label) end,
        {Label, Fun, 0}
    };
rewrite_test_term({generator, Fun}, Mod, Label) when is_function(Fun, 0) ->
    {module, SourceMod} = erlang:fun_info(Fun, module),
    case SourceMod of
        Mod ->
            {name, Name} = erlang:fun_info(Fun, name),
            {arity, Arity} = erlang:fun_info(Fun, arity),
            {
                generator,
                fun() -> rewrite_test_term(Fun(), Mod, Label) end,
                {Label, Name, Arity}
            };
        _ ->
            {generator, Fun}
    end;
rewrite_test_term({generator, Fun, {Mod, Name, Arity}}, Mod, Label)
        when is_function(Fun, 0) ->
    {
        generator,
        fun() -> rewrite_test_term(Fun(), Mod, Label) end,
        {Label, Name, Arity}
    };
rewrite_test_term({Desc, Test}, Mod, Label)
        when is_list(Desc); is_binary(Desc) ->
    {Desc, rewrite_test_term(Test, Mod, Label)};
rewrite_test_term({timeout, N, Test}, Mod, Label) ->
    {timeout, N, rewrite_test_term(Test, Mod, Label)};
rewrite_test_term({inorder, Test}, Mod, Label) ->
    {inorder, rewrite_test_term(Test, Mod, Label)};
rewrite_test_term({inparallel, Test}, Mod, Label) ->
    {inparallel, rewrite_test_term(Test, Mod, Label)};
rewrite_test_term({inparallel, N, Test}, Mod, Label) ->
    {inparallel, N, rewrite_test_term(Test, Mod, Label)};
rewrite_test_term({spawn, Test}, Mod, Label) ->
    {spawn, rewrite_test_term(Test, Mod, Label)};
rewrite_test_term({spawn, Node, Test}, Mod, Label) ->
    {spawn, Node, rewrite_test_term(Test, Mod, Label)};
rewrite_test_term({setup, Setup, Test}, Mod, Label) ->
    {setup, Setup, rewrite_instantiator(Test, Mod, Label)};
rewrite_test_term({setup, Where, Setup, Test}, Mod, Label)
        when Where =:= local; Where =:= spawn; is_tuple(Where) ->
    {setup, Where, Setup, rewrite_instantiator(Test, Mod, Label)};
rewrite_test_term({setup, Setup, Cleanup, Test}, Mod, Label) ->
    {setup, Setup, Cleanup, rewrite_instantiator(Test, Mod, Label)};
rewrite_test_term({setup, Where, Setup, Cleanup, Test}, Mod, Label) ->
    {setup, Where, Setup, Cleanup, rewrite_instantiator(Test, Mod, Label)};
rewrite_test_term({foreach, Setup, Tests}, Mod, Label) ->
    {foreach, Setup, rewrite_test_term(Tests, Mod, Label)};
rewrite_test_term({foreach, Where, Setup, Tests}, Mod, Label)
        when Where =:= local; Where =:= spawn; is_tuple(Where) ->
    {foreach, Where, Setup, rewrite_test_term(Tests, Mod, Label)};
rewrite_test_term({foreach, Setup, Cleanup, Tests}, Mod, Label) ->
    {foreach, Setup, Cleanup, rewrite_test_term(Tests, Mod, Label)};
rewrite_test_term({foreach, Where, Setup, Cleanup, Tests}, Mod, Label) ->
    {foreach, Where, Setup, Cleanup, rewrite_test_term(Tests, Mod, Label)};
rewrite_test_term(Fun, Mod, Label) when is_function(Fun, 0) ->
    {module, SourceMod} = erlang:fun_info(Fun, module),
    case SourceMod of
        Mod ->
            {name, Name} = erlang:fun_info(Fun, name),
            {arity, Arity} = erlang:fun_info(Fun, arity),
            {{Label, Name, Arity}, Fun};
        _ ->
            Fun
    end;
rewrite_test_term(Tests, Mod, Label) when is_list(Tests) ->
    [rewrite_test_term(Test, Mod, Label) || Test <- Tests];
rewrite_test_term(Test, _Mod, _Label) ->
    Test.

rewrite_instantiator(Fun, Mod, Label) when is_function(Fun, 1) ->
    fun(Value) -> rewrite_test_term(Fun(Value), Mod, Label) end;
rewrite_instantiator(Test, Mod, Label) ->
    rewrite_test_term(Test, Mod, Label).

%% @doc Load packaged devices and start apps needed by device test modules.
setup_device_tests(Names, Result) ->
    Env = setup_test_env(Result),
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
        restore_test_env(Env),
        erlang:raise(Class, Error, Stacktrace)
    end.

%% @doc Point this VM at the generated store and use normal test print
%% defaults unless the caller explicitly asked for noisy events.
setup_test_env(Result) ->
    {hb_forge_args:set_preloaded_env(Result), set_test_print_env()}.

%% @doc Restore test-only environment changes.
restore_test_env({PreloadedEnv, PrintEnv}) ->
    hb_forge_args:restore_preloaded_env(PreloadedEnv),
    restore_test_print_env(PrintEnv).

%% @doc Use the same quiet event set as normal EUnit test builds.
set_test_print_env() ->
    case os:getenv("HB_PRINT") of
        false ->
            os:putenv(
                "HB_PRINT",
                "error,http_error,cron_error,hook_error"
            ),
            erase_print_env_cache(),
            false;
        Old -> Old
    end.

%% @doc Restore `HB_PRINT' after a device test run.
restore_test_print_env(false) ->
    os:unsetenv("HB_PRINT"),
    erase_print_env_cache();
restore_test_print_env(Old) ->
    os:putenv("HB_PRINT", Old),
    erase_print_env_cache().

%% @doc Clear hb_opts' cached view of `HB_PRINT'.
erase_print_env_cache() ->
    erase({os_env, "HB_PRINT"}),
    erase({processed_env, <<"debug-print">>}).

%% @doc Build runtime opts pointing at the freshly-built preloaded
%% store; its devices resolve through the high-trust preloaded path.
test_opts(Result) ->
    #{
        <<"preloaded-store">> => maps:get(store, Result),
        <<"preloaded-devices-index">> => maps:get(index, Result)
    }.

%% @doc Resolve each device name through the freshly-built preloaded-store.
load_devices([], _Opts) ->
    ok;
load_devices([Name | Names], Opts) ->
    case hb_device_load:reference(Name, Opts) of
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
    lists:foreach(
        fun(Mod) -> load_test_module(Mod, preloaded_test_load_failed) end,
        Modules
    ),
    try Fun(test_modules_to_run(Modules, Roots))
    after
        code:del_path(hb_util:list(Ebin)),
        lists:foreach(fun purge_test_module/1, Modules),
        file:del_dir_r(filename:dirname(Ebin))
    end.

%% @doc Compile `src/preloaded/test' modules into an isolated ebin.
compile_preloaded_test_modules() ->
    compile_test_modules(
        unique_build_dir("device-test-fixtures"),
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
        unique_build_dir("device-test-core"),
        core_test_paths(),
        core_test_compile_failed
    ).

%% @doc Return source paths that make up the core EUnit suite.
core_test_paths() ->
    Paths =
        filelib:wildcard("src/*.erl") ++
        filelib:wildcard("src/core/**/*.erl") ++
        filelib:wildcard("src/forge/*.erl"),
    First = "src/core/test/hb_test_parallel.erl",
    [First || lists:member(First, Paths)] ++ lists:sort(Paths -- [First]).

%% @doc Return a per-run temporary build directory.
unique_build_dir(Name) ->
    hb_util:bin(
        filename:join(
            [
                "_build",
                Name ++ "-" ++ os:getpid() ++ "-" ++
                    integer_to_list(erlang:unique_integer([positive]))
            ]
        )
    ).

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
