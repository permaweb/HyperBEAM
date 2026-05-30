%%% @doc A tiny parse_transform plus runtime helper that lets EUnit modules
%%% opt in to parallel test execution by name.
%%%
%%% Any 0-arity function whose name ends in `_test_parallel' or
%%% `_test_parallel_' is treated as a parallel test: the transform
%%% auto-exports it and -- when the module does not already define one --
%%% injects an `all_parallel_test_/0' generator that runs all such
%%% functions in a single `{inparallel, ...}' EUnit batch. A generator
%%% may return `{serial, Test}' to run after that batch when it uses
%%% shared global state that would make the benchmark/test meaningless.
%%%
%%% Because the `_test_parallel' suffix does not match EUnit's own
%%% `_test'/`_test_' auto-discovery, the original function names are not
%%% renamed. The name you write is the name that gets compiled, and every
%%% test runs exactly once.
%%%
%%% Activation is by including `hb.hrl', which wires the transform in
%%% under `-ifdef(TEST)'. Example:
%%%
%%% ```
%%% -include("include/hb.hrl").
%%%
%%% foo_test_parallel() -> ?assertEqual(1, 1).
%%% bar_test_parallel_() -> {timeout, 30, fun() -> ?assert(true) end}.
%%% '''
%%%
%%% That is the whole contract. No manual exports, no hand-written
%%% generator, and nothing renamed.
-module(hb_test_parallel).
-export([parse_transform/2, all/1]).

-define(SIMPLE_SUFFIX, "_test_parallel").
-define(GENERATOR_SUFFIX, "_test_parallel_").
-define(GENERATOR_NAME, all_parallel_test_).

%% @doc Runtime helper invoked by the injected `all_parallel_test_/0'
%% generator. Returns an `{inparallel, [...]}' EUnit test spec covering
%% every `_test_parallel[_]/0' function exported by `Module'.
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
                    is_parallel_test_name(F)
            ]
        ),
    {Parallel, Serial} =
        lists:foldr(
            fun(F, {ParallelAcc, SerialAcc}) ->
                Name = atom_to_list(F),
                case parallel_test(Module, F, Name) of
                    {serial, Test} -> {ParallelAcc, [Test | SerialAcc]};
                    Test -> {[Test | ParallelAcc], SerialAcc}
                end
            end,
            {[], []},
            Funs
        ),
    case {Parallel, Serial} of
        {_, []} -> {inparallel, Parallel};
        {[], _} -> {inorder, Serial};
        _ -> {inorder, [{inparallel, Parallel} | Serial]}
    end.

parallel_test(Module, F, Name) ->
    case lists:suffix(?GENERATOR_SUFFIX, Name) of
        true ->
            case Module:F() of
                {serial, Test} -> {serial, {Name, Test}};
                Test -> {Name, Test}
            end;
        false ->
            {Name, fun Module:F/0}
    end.

%%% Compiler entry point.

%% @doc Invoked by the Erlang compiler when a module is compiled with
%% `-compile({parse_transform, hb_test_parallel}).'. Scans the module's
%% abstract forms, adds any missing exports for `_test_parallel[_]/0'
%% functions, and injects `all_parallel_test_/0' when the module does
%% not supply its own.
parse_transform(Forms, _Options) ->
    {Matching, HasGenerator} = scan(Forms),
    case Matching of
        [] ->
            %% No parallel tests in this module; leave the forms alone.
            Forms;
        _ ->
            Exports = exports_to_inject(Matching, HasGenerator),
            Forms1 = inject_exports(Forms, Exports),
            case HasGenerator of
                true -> Forms1;
                false -> inject_generator(Forms1)
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

%% @doc True when `Name' ends in `_test_parallel' or `_test_parallel_'.
is_parallel_test_name(Name) ->
    Str = atom_to_list(Name),
    lists:suffix(?SIMPLE_SUFFIX, Str)
        orelse lists:suffix(?GENERATOR_SUFFIX, Str).

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
