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
    case hb_forge_args:maybe_help(State, ?MODULE) of
        true -> {ok, State};
        false -> do_run(State)
    end.

do_run(State) ->
    Args = hb_forge_args:parse(State, <<"_build/device-test-store">>),
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
    SourceLabels = device_source_labels(Groups, Pkgs),
    CoreTests = maybe_compile_core_test_modules(Args, ModuleLabels, SourceLabels),
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
                                SourceLabels,
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
test_modules(
    State,
    Names,
    CoreModules,
    DeviceModules,
    ModuleLabels,
    SourceLabels,
    Args,
    Result
) ->
    ShowHash = maps:get(<<"show-hash">>, Args, false),
    ModuleNames = maps:get(<<"module-names">>, Args, all),
    TestSpecs = maps:get(<<"test-specs">>, Args, all),
    Env = setup_device_tests(Names, Result),
    EUnitResult =
        try
            {SelectedCoreModules, SelectedDeviceModules} =
                filter_modules(
                    CoreModules,
                    DeviceModules,
                    ModuleLabels,
                    SourceLabels,
                    module_filter(ModuleNames, TestSpecs)
                ),
            Tests = selected_tests(
                SelectedCoreModules,
                SelectedDeviceModules,
                ModuleLabels,
                SourceLabels,
                ShowHash,
                ModuleNames,
                TestSpecs
            ),
            RecordCtx = maybe_record_context(Args, Result),
            RecordTests = maybe_record_tests(Tests, RecordCtx),
            log_run(
                SelectedCoreModules,
                SelectedDeviceModules,
                ModuleLabels,
                ShowHash,
                ModuleNames,
                TestSpecs
            ),
            Res = eunit:test(timeout_tests(RecordTests, Args), eunit_opts(Args)),
            print_record_reports(RecordCtx),
            Res
        after restore_test_env(Env)
        end,
    case EUnitResult of
        ok -> {ok, State};
        error -> {error, format_error(eunit_failed)};
        Other -> {error, format_error({eunit_failed, Other})}
    end.

%% @doc Build the EUnit descriptor list, narrowed by `--module'/`--test'.
selected_tests(
    CoreModules,
    DeviceModules,
    ModuleLabels,
    SourceLabels,
    ShowHash,
    ModuleNames,
    TestSpecs
) ->
    case TestSpecs of
        all ->
            test_order(
                CoreModules,
                device_tests(DeviceModules, ModuleLabels, ShowHash)
            );
        _ ->
            Descriptors =
                [
                    named_test_descriptor(Mod, Fun, Label, Type)
                ||
                    Mod <- CoreModules ++ DeviceModules,
                    Fun0 <- matching_test_funs(
                        Mod, ModuleLabels, SourceLabels, TestSpecs
                    ),
                    {Fun, Type} <- resolve_test_fun(Mod, Fun0),
                    Label <- [test_label(Mod, ModuleLabels, ShowHash)]
                ],
            case Descriptors of
                [] ->
                    rebar_api:warn(
                        "device test: no test matched --module ~p --test ~p",
                        [ModuleNames, TestSpecs]
                    );
                _ ->
                    ok
            end,
            Descriptors
    end.

%% @doc Apply `--module', or module-qualified `--test' when no module was given.
module_filter(all, all) ->
    all;
module_filter(all, TestSpecs) ->
    case [Mod || {Mod, _Funs} <- TestSpecs, Mod =/= all] of
        [] -> all;
        Mods -> lists:usort(Mods)
    end;
module_filter(ModuleNames, _TestSpecs) ->
    ModuleNames.

%% @doc Narrow core and device module lists using generated/source/label names.
filter_modules(CoreModules, DeviceModules, _ModuleLabels, _SourceLabels, all) ->
    {CoreModules, DeviceModules};
filter_modules(CoreModules, DeviceModules, ModuleLabels, SourceLabels, Names) ->
    Keep =
        fun(Mod) ->
            lists:any(
                fun(Name) -> module_matches(Mod, Name, ModuleLabels, SourceLabels) end,
                Names
            )
        end,
    Filtered =
        {
            [Mod || Mod <- CoreModules, Keep(Mod)],
            [Mod || Mod <- DeviceModules, Keep(Mod)]
        },
    case Filtered of
        {[], []} ->
            rebar_api:warn(
                "device test: no module matched --module ~p", [Names]
            );
        _ ->
            ok
    end,
    Filtered.

matching_test_funs(Mod, ModuleLabels, SourceLabels, TestSpecs) ->
    lists:usort(
        lists:flatmap(
            fun
                ({all, Funs}) ->
                    Funs;
                ({Name, Funs}) ->
                    case module_matches(Mod, Name, ModuleLabels, SourceLabels) of
                        true -> Funs;
                        false -> []
                    end
            end,
            TestSpecs
        )
    ).

module_matches(Mod, Name, ModuleLabels, SourceLabels) ->
    Name =:= Mod orelse
        Name =:= maps:get(Mod, ModuleLabels, undefined) orelse
        Name =:= maps:get(Mod, SourceLabels, undefined).

%% @doc EUnit descriptor for an explicitly named test or generator.
named_test_descriptor(Mod, Fun, Label, generator) ->
    {
        generator,
        fun() -> rewrite_test_term(apply(Mod, Fun, []), Mod, Label) end,
        {Label, Fun, 0}
    };
named_test_descriptor(Mod, Fun, Label, test) ->
    {{Label, Fun, 0}, {test, Mod, Fun}}.

%% @doc Resolve a requested `-t' name, treating matching `_test_' as generator.
resolve_test_fun(Mod, Fun) ->
    code:ensure_loaded(Mod),
    case erlang:function_exported(Mod, Fun, 0) of
        true ->
            [
                {
                    Fun,
                    case lists:suffix("_test_", atom_to_list(Fun)) of
                        true -> generator;
                        false -> test
                    end
                }
            ];
        false ->
            Gen = binary_to_atom(<<(atom_to_binary(Fun, utf8))/binary, "_">>, utf8),
            case erlang:function_exported(Mod, Gen, 0) of
                true -> [{Gen, generator}];
                false -> []
            end
    end.

test_label(Mod, _ModuleLabels, true) ->
    Mod;
test_label(Mod, ModuleLabels, false) ->
    maps:get(Mod, ModuleLabels, Mod).

%% @doc Log what this run will execute before EUnit starts.
log_run(CoreModules, DeviceModules, ModuleLabels, ShowHash, all, all) ->
    rebar_api:info(
        "device test: running EUnit modules ~p",
        [test_names(CoreModules, DeviceModules, ModuleLabels, ShowHash)]
    );
log_run(CoreModules, DeviceModules, ModuleLabels, ShowHash, _ModuleNames, TestSpecs) ->
    rebar_api:info(
        "device test: running modules ~p tests ~p",
        [test_names(CoreModules, DeviceModules, ModuleLabels, ShowHash), TestSpecs]
    ).

%% @doc Apply a caller-supplied timeout to each top-level EUnit descriptor.
timeout_tests(Tests, #{ <<"timeout">> := undefined }) ->
    Tests;
timeout_tests(Tests, #{ <<"timeout">> := Timeout })
        when is_number(Timeout), Timeout >= 0 ->
    [{timeout, Timeout, Test} || Test <- Tests];
timeout_tests(_Tests, #{ <<"timeout">> := Timeout }) ->
    error({invalid_timeout, Timeout}).

%% @doc EUnit options, preserving the historical timeout multiplier by default.
eunit_opts(#{ <<"timeout-multiplier">> := Multiplier })
        when is_number(Multiplier), Multiplier >= 0 ->
    [verbose, {scale_timeouts, Multiplier}];
eunit_opts(#{ <<"timeout-multiplier">> := Multiplier })
        when Multiplier =/= undefined ->
    error({invalid_timeout_multiplier, Multiplier});
eunit_opts(#{ <<"timeout">> := Timeout }) when Timeout =/= undefined ->
    [verbose, {scale_timeouts, 1}];
eunit_opts(_Args) ->
    [verbose, {scale_timeouts, 10}].

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

%% @doc Map generated archive module atoms back to original source modules.
device_source_labels(Groups, Pkgs) ->
    maps:from_list(
        lists:flatmap(fun(Group) -> group_source_labels(Group, Pkgs) end, Groups)
    ).

group_source_labels(Group, Pkgs) ->
    Device = hb_packager:group_device_name(Group),
    case [Pkg || Pkg <- Pkgs, maps:get(device_name, Pkg) =:= Device] of
        [Pkg] ->
            RootMod = maps:get(module_name, Pkg),
            Root = maps:get(root, Group),
            Entries =
                [{Root, maps:get(root_file, Group)}] ++
                maps:get(helpers, Group, []) ++
                maps:get(libraries, Group, []),
            [
                {generated_module(RootMod, Root, Mod), Mod}
             ||
                {Mod, _Path} <- Entries
            ];
        [] ->
            []
    end.

generated_module(RootMod, Root, Root) ->
    RootMod;
generated_module(RootMod, Root, Mod) ->
    "dev_" ++ RootTail = atom_to_list(Root),
    RootPrefix = "dev_" ++ RootTail ++ "_",
    ModStr = atom_to_list(Mod),
    Tail =
        case {lists:prefix(RootPrefix, ModStr), lists:prefix("lib_", ModStr)} of
            {true, _} -> lists:nthtail(length(RootPrefix), ModStr);
            {_, true} -> lists:nthtail(length("lib_"), ModStr);
            _ -> ModStr
        end,
    binary_to_atom(
        <<(atom_to_binary(RootMod, utf8))/binary, "__",
            (hb_device_name:sanitize(Tail))/binary>>,
        utf8
    ).

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

%% @doc Build process-local recorder context when requested.
maybe_record_context(#{ <<"record">> := none }, _Result) ->
    none;
maybe_record_context(#{ <<"record">> := Report }, Result) ->
    RecorderMod = ensure_recorder_device(Result),
    rebar_api:info("device test: recording test flights for ~s", [
        case Report of
            all -> "every test";
            errors -> "failing tests"
        end
    ]),
    #{
        result => Result,
        recorder_mod => RecorderMod,
        report => Report,
        parent => self(),
        ref => make_ref()
    };
maybe_record_context(_Args, _Result) ->
    none.

%% @doc Optionally wrap leaf EUnit tests with process-local event recording.
maybe_record_tests(Tests, none) ->
    Tests;
maybe_record_tests(Tests, Ctx) ->
    event_test_term(Tests, Ctx).

%% @doc Print collected recorder links outside EUnit's captured output.
print_record_reports(none) ->
    ok;
print_record_reports(#{ ref := Ref }) ->
    case collect_event_reports(Ref, []) of
        [] ->
            ok;
        Reports ->
            rebar_api:info(
                "device test: recorder@1.0 test flights~n~s",
                [format_event_reports(lists:keysort(1, Reports))]
            )
    end.

collect_event_reports(Ref, Acc) ->
    receive
        {hb_forge_event_report, Ref, Seq, Status, Name, Path} ->
            collect_event_reports(Ref, [{Seq, Status, Name, Path} | Acc]);
        {hb_forge_event_report_error, Ref, Seq, Name, Error} ->
            collect_event_reports(Ref, [{Seq, error, Name, Error} | Acc])
    after 0 ->
        Acc
    end.

format_event_reports(Reports) ->
    lists:flatten(
        [
            io_lib:format(
                "  ~-7s ~s~n          ~s~n",
                [
                    event_report_status(Status),
                    event_report_name(Name),
                    event_report_link(Status, PathOrError)
                ]
            )
        ||
            {_Seq, Status, Name, PathOrError} <- Reports
        ]
    ).

event_report_status(Status) ->
    atom_to_list(Status).

event_report_link(error, Error) ->
    hb_util:list(hb_util:bin(io_lib:format("failed to write: ~0tp", [Error])));
event_report_link(_Status, Path) ->
    "file://" ++ Path.

event_report_name({Label, Fun, Arity})
        when is_atom(Label), is_atom(Fun), is_integer(Arity) ->
    lists:flatten(
        io_lib:format(
            "~s:~s/~B",
            [atom_to_list(Label), atom_to_list(Fun), Arity]
        )
    );
event_report_name({name, Name}) when is_atom(Name) ->
    atom_to_list(Name);
event_report_name(Name) ->
    hb_util:list(hb_util:bin(io_lib:format("~0tp", [Name]))).

ensure_recorder_device(Result) ->
    case hb_device_load:reference(<<"recorder@1.0">>, test_opts(Result)) of
        {ok, Mod} ->
            Mod;
        {error, Reason} ->
            erlang:error({recorder_device_unavailable, Reason})
    end.

event_test_term(Mod, Result) when is_atom(Mod) ->
    event_test_term(readable_module_tests(Mod, Mod), Result);
event_test_term({Line, Test}, Result) when is_integer(Line), Line >= 0 ->
    {Line, event_test_term(Test, Result)};
event_test_term({Name, {test, Mod, Fun}}, Result) ->
    {Name, event_test_fun(Name, fun() -> Mod:Fun() end, Result)};
event_test_term({test, Mod, Fun}, Result) ->
    Name = {Mod, Fun, 0},
    {Name, event_test_fun(Name, fun() -> Mod:Fun() end, Result)};
event_test_term({Mod, Fun}, Result) when is_atom(Mod), is_atom(Fun) ->
    Name = {Mod, Fun, 0},
    {Name, event_test_fun(Name, fun() -> Mod:Fun() end, Result)};
event_test_term({generator, Fun, Name}, Result) when is_function(Fun, 0) ->
    {
        generator,
        fun() ->
            event_run(
                Name,
                fun() -> event_test_term(Fun(), Result) end,
                record_errors_only(Result)
            )
        end,
        Name
    };
event_test_term({generator, Fun}, Result) when is_function(Fun, 0) ->
    {generator, fun() -> event_test_term(Fun(), Result) end};
event_test_term({timeout, N, Test}, Result) ->
    {timeout, N, event_test_term(Test, Result)};
event_test_term({inorder, Test}, Result) ->
    {inorder, event_test_term(Test, Result)};
event_test_term({inparallel, Test}, Result) ->
    {inparallel, event_test_term(Test, Result)};
event_test_term({inparallel, N, Test}, Result) ->
    {inparallel, N, event_test_term(Test, Result)};
event_test_term({spawn, Test}, Result) ->
    {spawn, event_test_term(Test, Result)};
event_test_term({spawn, Node, Test}, Result) ->
    {spawn, Node, event_test_term(Test, Result)};
event_test_term({setup, Setup, Test}, Result) ->
    {setup, Setup, event_test_instantiator(Test, Result)};
event_test_term({setup, Where, Setup, Test}, Result)
        when Where =:= local; Where =:= spawn; is_tuple(Where) ->
    {setup, Where, Setup, event_test_instantiator(Test, Result)};
event_test_term({setup, Setup, Cleanup, Test}, Result) ->
    {setup, Setup, Cleanup, event_test_instantiator(Test, Result)};
event_test_term({setup, Where, Setup, Cleanup, Test}, Result) ->
    {setup, Where, Setup, Cleanup, event_test_instantiator(Test, Result)};
event_test_term({foreach, Setup, Tests}, Result) ->
    {foreach, Setup, event_test_term(Tests, Result)};
event_test_term({foreach, Where, Setup, Tests}, Result)
        when Where =:= local; Where =:= spawn; is_tuple(Where) ->
    {foreach, Where, Setup, event_test_term(Tests, Result)};
event_test_term({foreach, Setup, Cleanup, Tests}, Result) ->
    {foreach, Setup, Cleanup, event_test_term(Tests, Result)};
event_test_term({foreach, Where, Setup, Cleanup, Tests}, Result) ->
    {foreach, Where, Setup, Cleanup, event_test_term(Tests, Result)};
event_test_term({Desc, Test}, Result) when is_list(Desc); is_binary(Desc) ->
    {Desc, event_test_term(Test, Result)};
event_test_term(Fun, Result) when is_function(Fun, 0) ->
    event_test_fun(erlang:fun_info(Fun, name), Fun, Result);
event_test_term(Tests, Result) when is_list(Tests) ->
    [event_test_term(Test, Result) || Test <- Tests];
event_test_term(Test, _Result) ->
    Test.

event_test_instantiator(Fun, Result) when is_function(Fun, 1) ->
    fun(Value) -> event_test_term(Fun(Value), Result) end;
event_test_instantiator(Test, Result) ->
    event_test_term(Test, Result).

event_test_fun(Name, Fun, Result) ->
    fun() -> event_run(Name, Fun, Result) end.

event_run(Name, Fun, Result) ->
    Env = setup_event_recording(Result),
    try
        Res = Fun(),
        maybe_write_event_success(Name, Result),
        Res
    catch Class:Reason:Stack ->
        safe_write_event_report(Name, failed, Result),
        erlang:raise(Class, Reason, Stack)
    after
        safe_clear_event_recording(Result),
        restore_event_recording_env(Env)
    end.

record_errors_only(none) ->
    none;
record_errors_only(Ctx) ->
    Ctx#{ report => errors }.

setup_event_recording(Result) ->
    OldEventOpts = erlang:get({hb_event, event_opts}),
    recorder_call(
        Result,
        take_off,
        #{
            <<"stack">> => true
        }
    ),
    erlang:put({hb_event, event_opts}, event_opts(Result)),
    OldEventOpts.

restore_event_recording_env(OldEventOpts) ->
    restore_process_value({hb_event, event_opts}, OldEventOpts).

restore_process_value(Key, undefined) ->
    erlang:erase(Key);
restore_process_value(Key, Value) ->
    erlang:put(Key, Value).

clear_event_recording(Result) ->
    apply(recorder_mod(Result), clear, []).

maybe_write_event_success(Name, #{ report := all } = Result) ->
    safe_write_event_report(Name, ok, Result);
maybe_write_event_success(_Name, _Result) ->
    ok.

write_event_report(Name, Status, Result) ->
    case recorder_call(
        Result,
        land,
        #{
            <<"format">> => <<"html">>
        }
    ) of
        {ok, #{ <<"body">> := Body }} ->
            Path = event_report_path(Name, Status),
            ok = file:write_file(Path, Body),
            send_event_report(Name, Status, Path, Result);
        Error ->
            send_event_report_error(Name, Error, Result)
    end.

safe_write_event_report(Name, Status, Result) ->
    try write_event_report(Name, Status, Result)
    catch Class:Reason ->
        send_event_report_error(Name, {Class, Reason}, Result)
    end.

send_event_report(Name, Status, Path, #{ parent := Parent, ref := Ref }) ->
    Parent !
        {
            hb_forge_event_report,
            Ref,
            erlang:unique_integer([monotonic, positive]),
            Status,
            Name,
            Path
        },
    ok.

send_event_report_error(Name, Error, #{ parent := Parent, ref := Ref }) ->
    Parent !
        {
            hb_forge_event_report_error,
            Ref,
            erlang:unique_integer([monotonic, positive]),
            Name,
            Error
        },
    ok.

safe_clear_event_recording(Result) ->
    try clear_event_recording(Result)
    catch _:_ -> ok
    end.

event_opts(Result) ->
    (event_plain_opts(Result))#{
        <<"on">> =>
            #{
                <<"event">> =>
                    #{
                        <<"device">> => <<"recorder@1.0">>,
                        <<"path">> => <<"maybe-append">>,
                        <<"hook/result">> => <<"ignore">>
                    }
            }
    }.

event_plain_opts(#{ result := Result, recorder_mod := RecorderMod }) ->
    (test_opts(Result))#{
        <<"forge-bootstrap">> => #{ <<"recorder@1.0">> => RecorderMod }
    }.

recorder_mod(#{ recorder_mod := RecorderMod }) ->
    RecorderMod.

recorder_call(Result, Fun, Req) ->
    apply(
        recorder_mod(Result),
        Fun,
        [
            #{ <<"device">> => <<"recorder@1.0">> },
            Req,
            event_plain_opts(Result)
        ]
    ).

event_report_path(Name, _Status) ->
    Filename =
        io_lib:format(
            "~s-~B.html",
            [
                hb_util:list(event_report_module_name(Name)),
                erlang:system_time(nanosecond)
            ]
        ),
    filename:join("/tmp", lists:flatten(Filename)).

event_report_module_name({Label, _Fun, _Arity}) ->
    event_report_module_name(Label);
event_report_module_name({name, Name}) ->
    event_report_module_name(Name);
event_report_module_name(Name) when is_atom(Name) ->
    safe_filename(atom_to_binary(Name, utf8));
event_report_module_name(Name) ->
    safe_filename(hb_util:bin(io_lib:format("~0tp", [Name]))).

safe_filename(Bin) ->
    << <<(safe_filename_char(Byte))>> || <<Byte>> <= Bin >>.

safe_filename_char(Byte)
        when Byte >= $a, Byte =< $z;
             Byte >= $A, Byte =< $Z;
             Byte >= $0, Byte =< $9;
             Byte =:= $-;
             Byte =:= $_ ->
    Byte;
safe_filename_char(_) ->
    $_.

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

%% @doc Compile core tests when requested, or when filters name core modules.
maybe_compile_core_test_modules(
    #{ <<"with-core">> := true },
    _ModuleLabels,
    _SourceLabels
) ->
    compile_core_test_modules();
maybe_compile_core_test_modules(Args, ModuleLabels, SourceLabels) ->
    case filter_needs_core(Args, ModuleLabels, SourceLabels) of
        true -> compile_core_test_modules();
        false -> none
    end.

filter_needs_core(Args, ModuleLabels, SourceLabels) ->
    ModuleNames = maps:get(<<"module-names">>, Args, all),
    TestSpecs = maps:get(<<"test-specs">>, Args, all),
    case module_filter(ModuleNames, TestSpecs) of
        all ->
            TestSpecs =/= all;
        Names ->
            lists:any(
                fun(Name) -> not device_module_name(Name, ModuleLabels, SourceLabels) end,
                Names
            )
    end.

device_module_name(Name, ModuleLabels, SourceLabels) ->
    lists:any(
        fun(Mod) -> module_matches(Mod, Name, ModuleLabels, SourceLabels) end,
        maps:keys(ModuleLabels) ++ maps:keys(SourceLabels)
    ).

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
