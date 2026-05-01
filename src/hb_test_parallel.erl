%%% @doc A tiny parse_transform plus runtime helper that lets EUnit modules
%%% opt in to parallel test execution by name.
%%%
%%% Any 0-arity function whose name ends in `_parallel_test' or
%%% `_parallel_test_' is treated as a parallel test: the transform renames
%%% it to an internal name (e.g. `foo_par_impl'), auto-exports it, and --
%%% when the module does not already define one -- injects an
%%% `all_parallel_test_/0' generator that runs all such functions in a
%%% single `{inparallel, ...}' EUnit batch.
%%%
%%% The rename is necessary because `_parallel_test' ends in `_test', which
%%% causes EUnit's own autoexport transform to also discover these functions
%%% as individual sequential tests -- resulting in every test running twice.
%%% Renaming to a non-`_test' suffix prevents individual discovery while
%%% keeping the parallel batch intact.
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

%% @doc Runtime helper invoked by the injected `all_parallel_test_/0'
%% generator. Returns an `{inparallel, [...]}' EUnit test spec covering
%% every renamed implementation function exported by `Module'.
%%
%% Safe to call from a REPL (`hb_test_parallel:all(dev_name).') to inspect
%% what the generator will run, which is the primary debugging hook if a
%% test unexpectedly does or does not appear in the parallel batch.
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
            {original_name(F), fun Module:F/0}
        ||
            F <- Funs
        ]
    }.

%%% Compiler entry point.

%% @doc Invoked by the Erlang compiler when a module is compiled with
%% `-compile({parse_transform, hb_test_parallel}).'. Scans the module's
%% abstract forms, renames `_parallel_test[_]/0' functions to internal
%% impl names, exports them, and injects `all_parallel_test_/0' when the
%% module does not supply its own.
parse_transform(Forms, _Options) ->
    {Matching, HasGenerator} = scan(Forms),
    case Matching of
        [] ->
            %% No parallel tests in this module; leave the forms alone.
            Forms;
        _ ->
            RenameMap = maps:from_list([{F, impl_name(F)} || F <- Matching]),
            Forms1 = rename_functions(Forms, RenameMap),
            %% If `eunit_autoexport' ran first (i.e. `eunit.hrl' included
            %% before `hb.hrl'), it will have exported the original names.
            %% Update those entries to the new impl names so the compiler
            %% does not see a reference to a now-nonexistent function.
            Forms2 = update_existing_exports(Forms1, RenameMap),
            ImplNames = maps:values(RenameMap),
            %% Skip impl names already present to avoid "already exported"
            %% warnings when `update_existing_exports' already covered them.
            AlreadyExported = sets:from_list([N || {attribute, _, export, E} <- Forms2, {N, _} <- E]),
            Exports = exports_to_inject([F || F <- ImplNames, not sets:is_element(F, AlreadyExported)], HasGenerator),
            Forms3 = inject_exports(Forms2, Exports),
            case HasGenerator of
                true -> Forms3;
                false -> inject_generator(Forms3)
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

%% @doc Derive the internal impl name by replacing the `_parallel_test[_]'
%% suffix with a non-`_test' suffix so EUnit's autoexport transform does
%% not discover these functions as individual tests.
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

%% @doc Reconstruct the original `_parallel_test[_]' label from an impl name.
%% Used by `all/1' so that test output shows the human-readable original name.
original_name(ImplName) ->
    Str = atom_to_list(ImplName),
    case lists:suffix(?GEN_IMPL_SUFFIX, Str) of
        true ->
            Prefix = lists:sublist(Str, length(Str) - length(?GEN_IMPL_SUFFIX)),
            Prefix ++ ?GENERATOR_SUFFIX;
        false ->
            Prefix = lists:sublist(Str, length(Str) - length(?IMPL_SUFFIX)),
            Prefix ++ ?SIMPLE_SUFFIX
    end.

%% @doc Rename matching function definitions in the abstract forms.
rename_functions(Forms, RenameMap) ->
    [case Form of
        {function, Line, Name, 0, Clauses} ->
            {function, Line, maps:get(Name, RenameMap, Name), 0, Clauses};
        _ ->
            Form
    end || Form <- Forms].

%% @doc Update existing `-export' attributes to use renamed impl names.
update_existing_exports(Forms, RenameMap) ->
    [case Form of
        {attribute, Line, export, Exports} ->
            {attribute, Line, export, [{maps:get(N, RenameMap, N), A} || {N, A} <- Exports]};
        _ ->
            Form
    end || Form <- Forms].

%% @doc Build the list of `{Name, 0}' entries the transform needs to add
%% to the module's export table: every matching test, plus the generator
%% when the transform is going to inject one.
exports_to_inject(Matching, HasGenerator) ->
    BaseExports = [{F, 0} || F <- Matching],
    case HasGenerator of
        true -> BaseExports;
        false -> [{?GENERATOR_NAME, 0} | BaseExports]
    end.

%% @doc Insert a single `-export([...])' attribute just before the first
%% function definition in `Forms'. The position does not matter for
%% correctness, but sitting next to the function body makes the injected
%% attribute easy to find in compiler error messages.
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
    %% No function definitions in the module; nothing useful to inject
    %% against. Return the forms unchanged.
    lists:reverse(Seen).

%% @doc Inject the stub
%%
%% ```
%% all_parallel_test_() -> hb_test_parallel:all(?MODULE).
%% '''
%%
%% just before the module's `eof' marker. The body is a single remote
%% call; all of the discovery logic lives in `all/1' so that it stays
%% debuggable at runtime.
inject_generator(Forms) ->
    {Before, [Eof]} = lists:split(length(Forms) - 1, Forms),
    Line =
        case Eof of
            {eof, L} -> L;
            _ -> 1
        end,
    Before ++ [generator_form(Line, module_of(Forms)), Eof].

%% @doc Extract the module name from a list of abstract forms.
module_of(Forms) ->
    hd([M || {attribute, _, module, M} <- Forms]).

%% @doc Build the abstract form for
%% `all_parallel_test_() -> hb_test_parallel:all(Module).'.
generator_form(Line, Module) ->
    Call =
        {call, Line,
            {remote, Line,
                {atom, Line, ?MODULE},
                {atom, Line, all}
            },
            [{atom, Line, Module}]
        },
    Clause = {clause, Line, [], [], [Call]},
    {function, Line, ?GENERATOR_NAME, 0, [Clause]}.
