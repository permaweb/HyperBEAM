%%% @doc A tiny parse_transform plus runtime helper that lets EUnit modules
%%% opt in to parallel test execution by name.
%%%
%%% Any 0-arity function whose name ends in `_parallel_test' or
%%% `_parallel_test_' is treated as a parallel test: the transform
%%% renames it to an internal implementation name, auto-exports it,
%%% and -- when the module does not already define one -- injects an
%%% `all_parallel_test_/0' generator that runs all such functions in
%%% a single `{inparallel, ...}' EUnit batch.
%%%
%%% The internal rename (e.g. `foo_parallel_test' -> `foo_par_impl')
%%% prevents EUnit's own autoexport transform from treating the
%%% functions as individual sequential tests while the parallel
%%% generator still runs them all concurrently.
%%%
%%% Activation is by including `hb.hrl', which wires the transform in
%%% under `-ifdef(TEST)'. Example:
%%%
%%% ```
%%% -include("include/hb.hrl").
%%%
%%% foo_parallel_test() -> ?assertEqual(1, 1).
%%% bar_parallel_test_() -> {timeout, 30, fun() -> ?assert(true) end}.
%%% '''
%%%
%%% That is the whole contract. No manual exports, no hand-written
%%% generator, and nothing renamed by the developer.
-module(hb_test_parallel).
-export([parse_transform/2, all/1]).

-define(SIMPLE_SUFFIX, "_parallel_test").
-define(GENERATOR_SUFFIX, "_parallel_test_").
-define(IMPL_SUFFIX, "_par_impl").
-define(GEN_IMPL_SUFFIX, "_par_gen").
-define(GENERATOR_NAME, all_parallel_test_).

%% @doc Runtime helper invoked for REPL debugging. Returns an
%% `{inparallel, [...]}' EUnit test spec covering every renamed
%% implementation function exported by `Module'.
%%
%% Safe to call from a REPL (`hb_test_parallel:all(dev_name).') to
%% inspect what the generator will run.
all(Module) ->
    Funs =
        lists:sort(
            [
                F
            ||
                {F, 0} <- Module:module_info(exports),
                    is_impl_name(F)
            ]
        ),
    {inparallel,
        [
            {atom_to_list(F), fun Module:F/0}
        ||
            F <- Funs
        ]
    }.

%%% Compiler entry point.

%% @doc Invoked by the Erlang compiler when a module is compiled with
%% `-compile({parse_transform, hb_test_parallel}).'. Scans the module's
%% abstract forms, renames `_parallel_test[_]/0' functions to internal
%% impl names, exports the impl functions plus the generator, and
%% injects `all_parallel_test_/0' when the module does not supply its own.
parse_transform(Forms, _Options) ->
    {Matching, HasGenerator} = scan(Forms),
    case Matching of
        [] ->
            Forms;
        _ ->
            RenameMap = build_rename_map(Matching),
            Forms1 = rename_functions(Forms, RenameMap),
            Forms1b = update_existing_exports(Forms1, RenameMap),
            AlreadyExported = already_exported(Forms1b),
            ImplNames = [ImplName || {_, ImplName} <- maps:to_list(RenameMap)],
            ImplExports = [{N, 0} || N <- ImplNames, not sets:is_element(N, AlreadyExported)],
            GenExports =
                case HasGenerator of
                    true -> [];
                    false -> [{?GENERATOR_NAME, 0}]
                end,
            Forms2 = inject_exports(Forms1b, ImplExports ++ GenExports),
            case HasGenerator of
                true -> Forms2;
                false -> inject_generator(Forms2, RenameMap)
            end
    end.

%%% Internal helpers.

%% @doc Scan the forms once, returning the names of matching functions
%% and whether the user has already defined `all_parallel_test_/0'.
scan(Forms) ->
    lists:foldl(
        fun
            (
                {function, _Line, Name, 0, _Clauses},
                {Matching, HasGenerator}
            ) ->
                NowHasGenerator = HasGenerator orelse Name == ?GENERATOR_NAME,
                case is_parallel_test_name(Name) of
                    true -> {[Name | Matching], NowHasGenerator};
                    false -> {Matching, NowHasGenerator}
                end;
            (_Other, State) ->
                State
        end,
        {[], false},
        Forms
    ).

%% @doc True when `Name' ends in `_parallel_test' or `_parallel_test_'.
is_parallel_test_name(Name) ->
    Str = atom_to_list(Name),
    lists:suffix(?SIMPLE_SUFFIX, Str)
        orelse lists:suffix(?GENERATOR_SUFFIX, Str).

%% @doc True when `Name' is an internal impl name produced by this transform.
is_impl_name(Name) ->
    Str = atom_to_list(Name),
    lists:suffix(?IMPL_SUFFIX, Str) orelse lists:suffix(?GEN_IMPL_SUFFIX, Str).

%% @doc Build a map from original function name to its renamed impl name.
build_rename_map(Names) ->
    maps:from_list([{N, impl_name(N)} || N <- Names]).

%% @doc Derive the internal impl name by replacing the parallel-test
%% suffix with a non-`_test'-ending suffix so EUnit autoexport ignores it.
impl_name(Name) ->
    Str = atom_to_list(Name),
    case lists:suffix(?GENERATOR_SUFFIX, Str) of
        true ->
            Prefix = lists:sublist(Str, length(Str) - length(?GENERATOR_SUFFIX)),
            list_to_atom(Prefix ++ ?GEN_IMPL_SUFFIX);
        false ->
            Prefix = lists:sublist(Str, length(Str) - length(?SIMPLE_SUFFIX)),
            list_to_atom(Prefix ++ ?IMPL_SUFFIX)
    end.

%% @doc Return a set of function names already present in export attributes.
already_exported(Forms) ->
    sets:from_list(
        [
            Name
        ||
            {attribute, _, export, Exports} <- Forms,
                {Name, _Arity} <- Exports
        ]
    ).

%% @doc Update any existing `-export' attributes to reflect renamed functions.
%% This handles the case where `eunit_autoexport' runs before this transform
%% (because `eunit.hrl' is included before `hb.hrl') and has already added
%% export entries for the original `_parallel_test' names.
update_existing_exports(Forms, RenameMap) ->
    [update_exports_form(Form, RenameMap) || Form <- Forms].

update_exports_form({attribute, Line, export, Exports}, RenameMap) ->
    NewExports = [{maps:get(Name, RenameMap, Name), Arity} || {Name, Arity} <- Exports],
    {attribute, Line, export, NewExports};
update_exports_form(Form, _RenameMap) ->
    Form.

%% @doc Rename matching function definitions in the abstract forms.
rename_functions(Forms, RenameMap) ->
    [rename_form(Form, RenameMap) || Form <- Forms].

rename_form({function, Line, Name, 0, Clauses}, RenameMap) ->
    case maps:find(Name, RenameMap) of
        {ok, ImplName} -> {function, Line, ImplName, 0, Clauses};
        error -> {function, Line, Name, 0, Clauses}
    end;
rename_form(Form, _RenameMap) ->
    Form.

%% @doc Insert a single `-export([...])' attribute just before the first
%% function definition in `Forms'.
inject_exports(Forms, []) ->
    Forms;
inject_exports(Forms, Exports) ->
    inject_exports(Forms, Exports, []).

inject_exports(
    [Form = {function, Line, _, _, _} | Rest],
    Exports,
    Seen
) ->
    Attribute = {attribute, Line, export, Exports},
    lists:reverse(Seen) ++ [Attribute, Form | Rest];
inject_exports([Form | Rest], Exports, Seen) ->
    inject_exports(Rest, Exports, [Form | Seen]);
inject_exports([], _Exports, Seen) ->
    lists:reverse(Seen).

%% @doc Inject the `all_parallel_test_/0' generator with compile-time
%% local fun references to every renamed impl function.
inject_generator(Forms, RenameMap) ->
    {Before, [Eof]} = lists:split(length(Forms) - 1, Forms),
    Line =
        case Eof of
            {eof, L} -> L;
            _ -> 1
        end,
    Before ++ [generator_form(Line, RenameMap), Eof].

%% @doc Build the abstract form for `all_parallel_test_/0', which returns
%% `{inparallel, [{"OrigName", fun ImplName/0}, ...]}' with local funs.
generator_form(Line, RenameMap) ->
    TestList =
        lists:sort(
            [{atom_to_list(Orig), Impl} || {Orig, Impl} <- maps:to_list(RenameMap)]
        ),
    TestTuples = [make_test_tuple(Line, Label, Impl) || {Label, Impl} <- TestList],
    ListExpr = make_list(Line, TestTuples),
    InParallel = {tuple, Line, [{atom, Line, inparallel}, ListExpr]},
    Clause = {clause, Line, [], [], [InParallel]},
    {function, Line, ?GENERATOR_NAME, 0, [Clause]}.

make_test_tuple(Line, Label, Impl) ->
    {tuple, Line, [
        {string, Line, Label},
        {'fun', Line, {function, Impl, 0}}
    ]}.

make_list(Line, []) ->
    {nil, Line};
make_list(Line, [H | T]) ->
    {cons, Line, H, make_list(Line, T)}.
