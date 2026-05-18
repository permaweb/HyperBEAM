%%% @doc Compiler transform that namespaces a packaged device.
%%%
%%% A package is a closed set of source modules: one `dev_<name>' root,
%%% optional `dev_<name>_*' helpers, and optional `lib_*' libraries. The
%%% packager compiles every module with a shared rename map, so each source
%%% module becomes a generated `_hb_device_*' module and every literal
%%% reference inside the package points at the generated name.
%%%
%%% The transform operates on the compiler's preprocessed abstract forms,
%%% so includes, macros, `?MODULE', and source-level parse transforms are
%%% handled by Erlang itself. Runtime-computed module names cannot be
%%% rewritten; they fail normally because source `dev_*' modules are not
%%% runtime device implementations.
-module(hb_device_rename).
-export([parse_transform/2]).

%% @doc Rewrite one module's forms into its generated namespace.
parse_transform(Forms, Options) ->
    Renames = renames(Options),
    Rewritten = [rewrite_form(Form, Renames) || Form <- Forms],
    assert_resolved(Rewritten, Renames),
    Rewritten.

%% @doc Fetch the package rename map from the compile options.
renames(Options) ->
    case proplists:get_value(hb_device_renames, Options) of
        Map when is_map(Map) -> Map;
        Other -> erlang:error({hb_device_renames_missing, Other})
    end.

%% @doc Rename the module attribute and package module atoms in the forms.
rewrite_form({attribute, Anno, module, Mod}, Renames) ->
    {attribute, Anno, module, maps:get(Mod, Renames, Mod)};
rewrite_form(Form, Renames) ->
    substitute(Form, Renames).

%% @doc Replace every package module atom with its generated module atom.
substitute({atom, Anno, Name}, Renames) when is_map_key(Name, Renames) ->
    {atom, Anno, map_get(Name, Renames)};
substitute(Tuple, Renames) when is_tuple(Tuple) ->
    list_to_tuple(substitute(tuple_to_list(Tuple), Renames));
substitute([H | T], Renames) ->
    [substitute(H, Renames) | substitute(T, Renames)];
substitute(Other, _Renames) ->
    Other.

%% @doc Fail the package if any source package module atom survived.
assert_resolved(Forms, Renames) ->
    case residual(Forms, Renames, []) of
        [] -> ok;
        Bad -> erlang:error({unresolved_device_reference, lists:usort(Bad)})
    end.

%% @doc Collect source package module atoms that survived rewriting.
residual({atom, _, Name}, Renames, Acc) when is_map_key(Name, Renames) ->
    [Name | Acc];
residual(Tuple, Renames, Acc) when is_tuple(Tuple) ->
    residual(tuple_to_list(Tuple), Renames, Acc);
residual([H | T], Renames, Acc) ->
    residual(T, Renames, residual(H, Renames, Acc));
residual(_Other, _Renames, Acc) ->
    Acc.
