%%% @doc Vary the inputs of a device function by its spec or heads.
%%%
%%% A device function's Dialyzer spec describes the base and request messages
%%% it reads and the result it returns. AO-Core uses the spec to <em>vary</em>
%%% the messages before execution: the function receives the keys it declares,
%%% loaded and coerced to their declared types, and the execution's hashpath is
%%% derived from the varied messages alone. Every execution the spec deems
%%% equivalent thereby shares one cache entry, however its messages otherwise
%%% differ.
%%%
%%% Specs are read from a module's BEAM by `extract/1' and compiled into a
%%% <em>schema</em>: a map from each normalized key to its accepted argument
%%% and result schemas, in declaration order. `hb_device_load:schema/2'
%%% memoises them.
%%% Clauses and union members are tried in declaration order, including
%%% coercion. A clause selects the base, request and result schemas together.
%%% Functions sharing a normalized name describe the same AO-Core key; helper
%%% functions with different argument meanings need distinct names.
%%% Lists keep their sequence structure; their elements are coerced in order.
%%% Materialized links are carried through the alternatives of one variation.
%%% Only their source values are reused, never rejected coercions or projections.
%%%
%%% Where no spec applies, a uniquely named function's heads provide input
%%% schemas. Map, tuple and list patterns, literals and positive type guards
%%% describe the values to load and coerce. Bound whole messages keep their
%%% other fields; an anonymous `_' reads nothing. Each alternative must still
%%% select its own head and guard after Vary. Erlang checks the original guards,
%%% including their errors and short-circuit behavior; no body is evaluated.
%%% Unsupported forms conservatively retain the inputs. Heads imply no result
%%% overlay: use a spec to declare one.
%%%
%%% The type syntax means, for a message argument:
%%% <ul>
%%%   <li>`#{ key := type() }': `key' must be present; it is loaded if it is
%%%       a link and coerced to the type. `#{ key => type() }': as above,
%%%       but the key may be absent.</li>
%%%   <li>`#{ key := _ }' / `#{ key => _ }': `key' is kept exactly as given,
%%%       a link staying a link.</li>
%%%   <li>`#{ _ => _ }': every undeclared key is kept as given. Without it,
%%%       the message is <em>projected</em>: undeclared keys are removed
%%%       before execution and take no part in the hashpath.</li>
%%%   <li>`#{ _ := type() }': every undeclared key is coerced to the type.</li>
%%%   <li>`_' or `#{}': the function reads nothing from the argument, which
%%%       is projected to the implicit keys below.</li>
%%%   <li>`map()', `any()', `term()': the argument is passed through
%%%       untouched -- as is every value of a type the varier does not
%%%       understand (remote types, records, unbound type variables).</li>
%%% </ul>
%%%
%%% `when T :: integer() | binary()' chooses one member for every use of `T'
%%% in the clause. Bounds may refer to other variables and appear in nested
%%% types or local aliases. Each combination is an ordinary accepted schema,
%%% ordered by the bounds and their members as declared.
%%%
%%% `device' is always kept in the base and `path' in the request, so a
%%% projection cannot detach an execution from its device or key. Scalar
%%% types coerce through the `hb_util' converters; lists, tuples, unions,
%%% ranges and literals apply recursively. A value that cannot be coerced to
%%% its type throws `{invalid_type, Schema, Value}'; a required key that is
%%% absent throws `{required_key_missing, Key}'.
%%%
%%% A result spec may declare `#{ '...' := base }' (or `request'): the
%%% result is then a patch that `hb_ao' lays over the <em>unvaried</em>
%%% message, so a function that reads only a projection of its base can still
%%% return the whole of it, updated.
-module(hb_types).
-export([extract/1, vary/6]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% The built-in types that coerce and check as scalars, named as their
%% normalized kinds are.
-define(SCALARS,
    #{
        integer => true, non_neg_integer => true, pos_integer => true,
        neg_integer => true, float => true, number => true, binary => true,
        bitstring => true, atom => true, pid => true
    }
).

%%% --------------------------------------------------------------------
%%% Varying an execution
%%% --------------------------------------------------------------------

%% @doc Vary an execution's base and request by the schema of the function
%% that will execute `Key'. `AddKey' is the key if the function takes it as
%% its first argument (a `handler' or `default'), or `false'. Returns the
%% varied messages and the overlay the result spec declares, or `no_spec'.
vary(Key, Func, AddKey, Base, Req, Opts) ->
    case function_schema(Func, Key, Opts) of
        {ok, Schemas} ->
            vary_schemas(Schemas, AddKey, Base, Req, #{}, Opts);
        {error, _} ->
            no_spec
    end.

%% @doc Try complete schemas in order, retaining the selected result's overlay.
vary_schemas(Schemas, AddKey, Base, Req, Loaded, Opts) when is_list(Schemas) ->
    vary_schemas({none, Schemas}, AddKey, Base, Req, Loaded, Opts);
vary_schemas({Select, [Schema | Rest]}, AddKey, Base, Req, Loaded, Opts) ->
    try
        {BaseSchema, ReqSchema, ReturnSchema} =
            execution_schemas(Schema, AddKey),
        {VB, {_, Loaded1}} = apply_schema(implicit_base(BaseSchema), Base, Loaded, Opts),
        {VR, {_, Loaded2}} =
            apply_schema(
                implicit_request(ReqSchema),
                request_with_key(Req, AddKey),
                Loaded1,
                Opts
            ),
        VariedBase = reuse_loaded(VB, Loaded2),
        VariedReq = reuse_loaded(VR, Loaded2),
        case matches_head({Select, maps:get(<<"head">>, Schema, none)},
                AddKey, VariedBase, VariedReq, Opts) of
            true -> {ok, VariedBase, VariedReq, overlay(ReturnSchema)};
            false -> vary_schemas({Select, Rest}, AddKey, Base, Req, Loaded2, Opts)
        end
    catch
        throw:{'schema-mismatch', Reason, FailedLoads} ->
            case Rest of
                [] -> throw(Reason);
                _ -> vary_schemas({Select, Rest}, AddKey, Base, Req, FailedLoads, Opts)
            end
    end.

%% @doc Reuse materialized values without reading any other links.
reuse_loaded(Value, Loaded) when map_size(Loaded) =:= 0 -> Value;
reuse_loaded(Link, Loaded) when ?IS_LINK(Link) ->
    case maps:find(Link, Loaded) of
        {ok, Value} -> reuse_loaded(Value, Loaded);
        error -> Link
    end;
reuse_loaded(Value, Loaded) when is_map(Value) ->
    maps:map(fun(_, V) -> reuse_loaded(V, Loaded) end, Value);
reuse_loaded([H | T], Loaded) -> [reuse_loaded(H, Loaded) | reuse_loaded(T, Loaded)];
reuse_loaded(Value, Loaded) when is_tuple(Value) ->
    list_to_tuple(reuse_loaded(tuple_to_list(Value), Loaded));
reuse_loaded(Value, _) -> Value.

%% @doc Projection and coercion must leave the selected head and guards matching.
matches_head({Select, Index}, AddKey, Base, Req, Opts)
        when is_integer(Index) ->
    Args =
        case AddKey of
            false -> [Base, Req, Opts];
            _ -> [AddKey, Base, Req, Opts]
        end,
    Select(Args) =:= Index;
matches_head(_Schema, _AddKey, _Base, _Req, _Opts) -> true.

%% @doc Specs take precedence, by function name then key. Inferred heads
%% describe only their own function, not another handler of the same key.
function_schema(Func, Key, Opts) ->
    maybe
        true ?= is_function(Func) orelse {error, not_found},
        {module, Module} = erlang:fun_info(Func, module),
        {name, Name} = erlang:fun_info(Func, name),
        {ok, Schemas} ?= hb_device_load:schema(Module, Opts),
        NormName = normalize_name(Name),
        case Schemas of
            #{ NormName := Accepted } when is_list(Accepted) -> {ok, Accepted};
            #{ Key := Accepted } when is_list(Accepted) -> {ok, Accepted};
            #{ NormName := Accepted } -> {ok, Accepted};
            _ -> {error, not_found}
        end
    end.

%% @doc The base, request and result schemas of an execution. A function that
%% takes the key as its first argument reads the base and request from its
%% second and third; an omitted argument is treated as `_'.
execution_schemas(#{ <<"args">> := Args, <<"return">> := Return }, AddKey) ->
    Offset =
        case AddKey of
            false -> 0;
            _ -> 1
        end,
    {
        maybe_nth(1 + Offset, Args, wildcard_type()),
        maybe_nth(2 + Offset, Args, wildcard_type()),
        Return
    }.

%% @doc The `N'th element of a list, or the default if it is shorter.
maybe_nth(N, List, _Default) when N =< length(List) -> lists:nth(N, List);
maybe_nth(_N, _List, Default) -> Default.

%% @doc A `handler' or `default' function serves many keys, so the request
%% it is varied by records the key it was chosen for as its path.
request_with_key(Req, false) -> Req;
request_with_key(Req, Key) -> Req#{ <<"path">> => Key }.

%% @doc The base schema always admits the device, and the request schema
%% always requires the path: a projection keeps an execution attached to its
%% device and key.
implicit_base(Schema) ->
    implicit_key(top_level_schema(Schema), <<"device">>, optional).

implicit_request(Schema) ->
    implicit_key(top_level_schema(Schema), <<"path">>, required).

%% @doc A bare `_' argument declares that the function reads nothing from
%% the message: it is projected to the implicit keys alone.
top_level_schema(#{ <<"kind">> := <<"wildcard">> }) ->
    message_type(#{}, none);
top_level_schema(Schema) ->
    Schema.

%% @doc Add a key to a message schema, unless the schema declares it.
implicit_key(
    Schema = #{ <<"kind">> := <<"union">>, <<"members">> := Members },
    Key,
    Presence
) ->
    Schema#{
        <<"members">> =>
            [
                implicit_key(top_level_schema(Member), Key, Presence)
            ||
                Member <- Members
            ]
    };
implicit_key(
    Schema = #{ <<"kind">> := <<"message">>, <<"keys">> := Keys },
    Key,
    Presence
) when not is_map_key(Key, Keys) ->
    Schema#{
        <<"keys">> =>
            Keys#{
                Key => #{ <<"presence">> => Presence, <<"type">> => any_type() }
            }
    };
implicit_key(Schema, _Key, _Presence) ->
    Schema.

%% @doc The overlay a result spec declares: `base' or `request' when the
%% result message -- directly, or inside a tuple or union such as
%% `{ok, Result}' -- has a `...' key of that literal or alias type.
overlay(#{ <<"kind">> := <<"message">>, <<"keys">> := #{ <<"...">> := Field } }) ->
    overlay_marker(maps:get(<<"type">>, Field));
overlay(#{ <<"kind">> := <<"tuple">>, <<"items">> := Items }) ->
    first_overlay(Items);
overlay(#{ <<"kind">> := <<"union">>, <<"members">> := Members }) ->
    first_overlay(Members);
overlay(_Schema) ->
    none.

%% @doc The overlay of the first of the schemas that declares one.
first_overlay([]) ->
    none;
first_overlay([Schema | Rest]) ->
    case overlay(Schema) of
        none -> first_overlay(Rest);
        Overlay -> Overlay
    end.

%% @doc The overlay a `...' key's type names.
overlay_marker(#{ <<"kind">> := <<"literal">>, <<"value">> := Marker }) ->
    overlay_marker(Marker);
overlay_marker(#{ <<"kind">> := <<"alias">>, <<"name">> := Marker }) ->
    overlay_marker(Marker);
overlay_marker(base) -> base;
overlay_marker(request) -> request;
overlay_marker(<<"base">>) -> base;
overlay_marker(<<"request">>) -> request;
overlay_marker(_Type) -> none.

%%% --------------------------------------------------------------------
%%% Extracting schemas from a BEAM
%%% --------------------------------------------------------------------

%% @doc Extract the function schemas of a module from its BEAM: the given
%% bytes, or the object code of a module loaded from the code path. A module
%% compiled without `debug_info' has no abstract code, and so no schemas.
extract(Beam) when is_binary(Beam) ->
    case beam_lib:chunks(Beam, [abstract_code]) of
        {ok, {_Module, [{abstract_code, {_Version, Forms}}]}} ->
            TypeEnv = build_type_env(Forms),
            Specs =
                lists:foldl(
                    fun(Spec, Acc) -> put_spec(Spec, TypeEnv, Acc) end,
                    #{},
                    [ Attr || Attr = {attribute, _, spec, _} <- Forms ]
                ),
            Heads =
                maps:groups_from_list(
                    fun({function, _, Name, _, _}) -> normalize_name(Name) end,
                    [Form || Form = {function, _, _, _, _} <- Forms]
                ),
            {ok,
                maps:fold(
                    fun
                        (Name, [{function, _, _, _, Clauses}], Acc)
                                when not is_map_key(Name, Acc) ->
                            Acc#{ Name => head_schemas(Clauses) };
                        (_, _, Acc) -> Acc
                    end,
                    Specs,
                    Heads
                )};
        Other ->
            {error, {abstract_code_unavailable, Other}}
    end;
extract(Module) when is_atom(Module) ->
    case code:get_object_code(Module) of
        {Module, Beam, _Path} -> extract(Beam);
        error -> {error, {object_code_unavailable, Module}}
    end.

%% @doc A single unguarded head of distinct variables needs no dispatch check.
head_schemas([{clause, _, Args, [], _}] = Clauses) ->
    Names = [Name || {var, _, Name} <- Args, Name =/= '_'],
    case
        lists:all(fun({var, _, _}) -> true; (_) -> false end, Args)
            andalso length(Names) =:= length(lists:usort(Names))
    of
        true ->
            {none, [
                head_schema([head_type(Arg, #{}, false) || Arg <- Args], none),
                head_schema([any_type(), any_type(), any_type()], none)
            ]};
        false -> head_alternatives(Clauses)
    end;
head_schemas(Clauses) -> head_alternatives(Clauses).

%% @doc Infer inputs with a shared selector; unsupported syntax uses identity.
%% Store it once per function so ETS does not duplicate the clause AST.
head_alternatives(Clauses) ->
    Identity = head_schema([any_type(), any_type(), any_type()], none),
    try
        Indexed =
            lists:enumerate([
                {A, Args, [lists:uniq(G ++ Tests) || G <- Guards], Tests}
            ||
                {clause, A, Args, RawGuards, _} <- Clauses,
                Guards <- [case RawGuards of [] -> [[]]; _ -> RawGuards end],
                Tests <- lists:append([guard_branches(G) || G <- Guards])
            ]),
        Selectors =
            [
                {clause, A, [lists:foldr(
                    fun(P, Tail) -> {cons, A, P, Tail} end, {var, A, '_'}, Args
                )], Guards, [{integer, A, Index}]}
            || {Index, {A, Args, Guards, _}} <- Indexed
            ],
        {value, Select, _} =
            erl_eval:expr(
                {'fun', 0, {clauses, Selectors ++
                    [{clause, 0, [{var, 0, '_'}], [], [{integer, 0, 0}]}]}},
                erl_eval:new_bindings()
            ),
        {Select, [
            head_schema([
                head_dependencies(head_type(Arg, Env, false), N, Previous)
            || {N, Arg} <- lists:enumerate(Args)
            ], Index)
        ||
            {Index, {_, Args, _, Tests}} <- Indexed,
            Previous <- [lists:sublist(Indexed, Index - 1)],
            Hints <- [lists:append([guard_hints(Test) || Test <- Tests])],
            Env <- bindings([
                {Name, #{ <<"kind">> => <<"union">>,
                    <<"members">> => [Type || {N, Type} <- Hints, N =:= Name] }}
            || Name <- lists:uniq([N || {N, _} <- Hints])
            ], #{})
        ] ++ [Identity]}
    catch error:_ -> {none, [Identity]}
    end.

%% @doc Keep preceding heads' named fields without requiring or coercing them.
%% The selector still checks dependencies that cannot be inferred here.
head_dependencies(Schema, N, Previous) ->
    lists:foldl(
        fun(Key, Acc) -> implicit_key(Acc, Key, optional) end,
        top_level_schema(Schema),
        [Key || {_, {_, Args, Guards, _}} <- Previous,
            Key <- head_keys(lists:nth(N, Args), Guards)]
    ).

%% @doc Fields named by a pattern or read through its whole-message binding.
head_keys({map, _, Fields}, _Guards) ->
    [erl_parse:normalise(Key) || {map_field_exact, _, Key, _} <- Fields];
head_keys({match, _, Left, Right}, Guards) ->
    head_keys(Left, Guards) ++ head_keys(Right, Guards);
head_keys({var, _, Name}, Guards) -> guard_keys(Guards, Name);
head_keys(_, _) -> [].

%% @doc Literal guard reads retain the named field's value, including submessages.
guard_keys({call, A, {remote, _, {atom, _, erlang}, F}, Args}, Name) ->
    guard_keys({call, A, F, Args}, Name);
guard_keys({call, _, {atom, _, F}, [Key, {var, _, Name}]}, Name)
        when F =:= map_get; F =:= is_map_key ->
    try [erl_parse:normalise(Key)] catch error:_ -> [] end;
guard_keys(Term, Name) when is_tuple(Term) ->
    guard_keys(tuple_to_list(Term), Name);
guard_keys(Terms, Name) when is_list(Terms) ->
    lists:append([guard_keys(Term, Name) || Term <- Terms]);
guard_keys(_, _) -> [].

%% @doc An inferred clause describes inputs; result overlays require a spec.
head_schema(Args, Index) ->
    #{ <<"args">> => Args, <<"return">> => any_type(), <<"head">> => Index }.

%% @doc Ordered guard alternatives. Each also checks the original guard, whose
%% short-circuit errors cannot be replaced by ordinary boolean distribution.
guard_branches([]) -> [[]];
guard_branches([Test | Rest]) ->
    [A ++ B || A <- guard_branches(Test), B <- guard_branches(Rest)];
guard_branches({op, _, Op, A, B}) when Op =:= 'andalso'; Op =:= 'and' ->
    guard_branches([A, B]);
guard_branches({op, _, Op, A, B}) when Op =:= 'orelse'; Op =:= 'or' ->
    guard_branches(A) ++ guard_branches(B);
guard_branches(Test) -> [[Test]].

%% @doc Positive type tests provide coercion choices; the selector checks all guards.
guard_hints({op, _, '=:=', {var, _, Name}, Value}) ->
    try [{Name, literal_type(erl_parse:normalise(Value))}]
    catch error:_ -> []
    end;
guard_hints({op, A, '=:=', Value, Var = {var, _, _}}) ->
    guard_hints({op, A, '=:=', Var, Value});
guard_hints({call, A, {remote, _, {atom, _, erlang}, F}, Args}) ->
    guard_hints({call, A, F, Args});
guard_hints({call, _, {atom, _, Test}, [{var, _, Name}]}) ->
    case atom_to_binary(Test) of
        <<"is_", Type/binary>> ->
            Schema =
                case Type of
                    <<"map">> -> message_type(#{}, #{ <<"presence">> => optional });
                    <<"tuple">> -> #{ <<"kind">> => <<"tuple">> };
                    _ -> parse_type({type, 0, binary_to_atom(Type), []}, #{}, #{}, [])
                end,
            [{Name, Schema}];
        _ -> []
    end;
guard_hints(_) -> [].

%% @doc A bound whole value keeps its fields, including those inside nested patterns.
head_type({var, _, '_'}, _Env, _Keep) -> wildcard_type();
head_type({var, _, Name}, Env, _Keep) -> maps:get(Name, Env, any_type());
head_type({match, _, {var, _, Name}, {var, _, Other}}, Env, _Keep) ->
    maps:get(Name, Env, maps:get(Other, Env, any_type()));
head_type({match, _, {var, _, Name}, Pattern}, Env, Keep) ->
    head_type(Pattern, Env, Keep orelse Name =/= '_');
head_type({match, _, Pattern, {var, _, Name}}, Env, Keep) ->
    head_type(Pattern, Env, Keep orelse Name =/= '_');
head_type({map, _, Fields}, Env, Keep) ->
    Keys =
        maps:from_list([
            {erl_parse:normalise(Key), #{ <<"presence">> => required,
                <<"type">> => head_type(Value, Env, Keep) }}
        || {map_field_exact, _, Key, Value} <- Fields
        ]),
    message_type(Keys,
        case Keep of true -> #{ <<"presence">> => optional }; false -> none end);
head_type({tuple, _, Items}, Env, Keep) ->
    #{ <<"kind">> => <<"tuple">>,
        <<"items">> => [head_type(Item, Env, Keep) || Item <- Items] };
head_type({cons, _, Head, Tail}, Env, Keep) ->
    #{ <<"kind">> => <<"list">>, <<"head">> => head_type(Head, Env, Keep),
        <<"tail">> => head_type(Tail, Env, Keep) };
head_type(Pattern, _Env, _Keep) ->
    try literal_type(erl_parse:normalise(Pattern))
    catch error:_ ->
        case Pattern of
            {bin, _, _} -> scalar_type(<<"bitstring">>);
            _ -> any_type()
        end
    end.

%% @doc The module's own type declarations, by name and arity, for expansion when a
%% spec refers to them.
build_type_env(Forms) ->
    maps:from_list(
        [
            {{Name, length(Vars)},
                #{ vars => [ var_name(Var) || Var <- Vars ], ast => Ast }}
        ||
            {attribute, _, Tag, {Name, Ast, Vars}} <- Forms,
            Tag =:= type orelse Tag =:= opaque
        ]
    ).

%% @doc The name of a type's parameter.
var_name({var, _, Name}) -> Name;
var_name(Name) -> Name.

%% @doc Append a key's accepted schemas in the order the author declares them.
put_spec({attribute, _, spec, {{Name, _}, Clauses}}, TypeEnv, Schemas) ->
    NormName = normalize_name(Name),
    Schemas#{
        NormName =>
            maps:get(NormName, Schemas, []) ++
                [Schema || Clause <- Clauses,
                    Schema <- parse_fun_spec(Clause, TypeEnv)]
    };
put_spec(_Spec, _TypeEnv, Schemas) ->
    Schemas.

%% @doc The accepted argument and result schemas of a spec clause.
parse_fun_spec({type, _, bounded_fun, [FunSpec, Constraints]}, TypeEnv) ->
    Bounds =
        [
            {normalize_name(Name), parse_type(Type, TypeEnv, #{}, [])}
        ||
            {type, _, constraint, [{atom, _, is_subtype},
                [{var, _, Name}, Type]]} <- Constraints
        ],
    [substitute(Schema, Env) || Schema <- parse_fun_spec(FunSpec, TypeEnv),
        Env <- bindings(Bounds, #{})];
parse_fun_spec({type, _, 'fun', [{type, _, product, Args}, Return]}, TypeEnv) ->
    [#{
        <<"args">> => [ parse_type(Arg, TypeEnv, #{}, []) || Arg <- Args ],
        <<"return">> => parse_type(Return, TypeEnv, #{}, [])
    }];
parse_fun_spec(Other, _TypeEnv) ->
    [#{ <<"args">> => [unknown_type(Other)], <<"return">> => any_type() }].

%% @doc Choose each bound once for the whole clause, preserving member order.
bindings([], Env) -> [Env];
bindings(
    [{Name, #{ <<"kind">> := <<"union">>, <<"members">> := Members }} | Rest],
    Env
) ->
    lists:append([bindings(Rest, Env#{ Name => Member }) || Member <- Members]);
bindings([{Name, Type} | Rest], Env) ->
    bindings(Rest, Env#{ Name => Type }).

%% @doc Substitute after all bounds are chosen, so forward references agree.
%% Recursive references remain unbound, as do variables without a constraint.
substitute(#{ <<"kind">> := <<"variable">>, <<"name">> := Name } = Schema, Env) ->
    case maps:take(Name, Env) of
        {Type, Rest} -> substitute(Type, Rest);
        error -> Schema
    end;
substitute(Map, Env) when is_map(Map) ->
    maps:map(fun(_, Value) -> substitute(Value, Env) end, Map);
substitute(List, Env) when is_list(List) ->
    [substitute(Item, Env) || Item <- List];
substitute(Value, _Env) -> Value.

%% @doc Compile an abstract type into a schema. `TypeEnv' holds the module's
%% own types, `VarEnv' the schemas bound to the type variables of the one being
%% expanded, and `Seen' the types under expansion, so that a recursive type
%% becomes an alias rather than a loop.
parse_type({ann_type, _, [_Var, Type]}, TypeEnv, VarEnv, Seen) ->
    parse_type(Type, TypeEnv, VarEnv, Seen);
parse_type({var, _, '_'}, _TypeEnv, _VarEnv, _Seen) ->
    wildcard_type();
parse_type({var, _, Name}, _TypeEnv, VarEnv, _Seen) ->
    case maps:find(Name, VarEnv) of
        {ok, Bound} -> Bound;
        error -> variable_type(Name)
    end;
parse_type({user_type, _, Name, Args}, TypeEnv, VarEnv, Seen) ->
    TypeKey = {Name, length(Args)},
    case lists:member(TypeKey, Seen) orelse maps:find(TypeKey, TypeEnv) of
        {ok, #{ vars := Vars, ast := Ast }} ->
            parse_type(
                Ast,
                TypeEnv,
                maps:from_list(
                    lists:zip(
                        Vars,
                        [ parse_type(Arg, TypeEnv, VarEnv, Seen) || Arg <- Args ]
                    )
                ),
                [TypeKey | Seen]
            );
        _ ->
            alias_type(Name)
    end;
parse_type({remote_type, _, [{atom, _, Mod}, {atom, _, Name}, Args]},
        TypeEnv, VarEnv, Seen) ->
    #{
        <<"kind">> => <<"remote">>,
        <<"module">> => normalize_name(Mod),
        <<"name">> => normalize_name(Name),
        <<"args">> => [ parse_type(Arg, TypeEnv, VarEnv, Seen) || Arg <- Args ]
    };
parse_type({type, _, map, any}, _TypeEnv, _VarEnv, _Seen) ->
    any_type();
parse_type({type, _, map, Fields}, TypeEnv, VarEnv, Seen) ->
    {Keys, Wildcard} =
        lists:foldl(
            fun({type, _, Assoc, [KeyAst, ValueAst]}, {KeyAcc, WildAcc}) ->
                Field =
                    #{
                        <<"presence">> => field_presence(Assoc),
                        <<"type">> => parse_type(ValueAst, TypeEnv, VarEnv, Seen)
                    },
                case key_name(KeyAst, TypeEnv, VarEnv, Seen) of
                    <<"_">> -> {KeyAcc, Field};
                    Key -> {KeyAcc#{ Key => Field }, WildAcc}
                end
            end,
            {#{}, none},
            Fields
        ),
    message_type(Keys, Wildcard);
parse_type({type, _, ListType, Items}, TypeEnv, VarEnv, Seen)
        when ListType =:= list; ListType =:= nonempty_list ->
    #{
        <<"kind">> => <<"list">>,
        <<"item">> =>
            case Items of
                [] -> any_type();
                [Item] -> parse_type(Item, TypeEnv, VarEnv, Seen)
            end
    };
parse_type({type, _, tuple, any}, _TypeEnv, _VarEnv, _Seen) ->
    #{ <<"kind">> => <<"tuple">> };
parse_type({type, _, tuple, Items}, TypeEnv, VarEnv, Seen) ->
    #{
        <<"kind">> => <<"tuple">>,
        <<"items">> => [ parse_type(Item, TypeEnv, VarEnv, Seen) || Item <- Items ]
    };
parse_type({type, _, union, Members}, TypeEnv, VarEnv, Seen) ->
    #{
        <<"kind">> => <<"union">>,
        <<"members">> =>
            [ parse_type(Member, TypeEnv, VarEnv, Seen) || Member <- Members ]
    };
parse_type({type, _, range, [Min, Max]}, TypeEnv, VarEnv, Seen) ->
    #{
        <<"kind">> => <<"range">>,
        <<"min">> => literal_value(parse_type(Min, TypeEnv, VarEnv, Seen)),
        <<"max">> => literal_value(parse_type(Max, TypeEnv, VarEnv, Seen))
    };
parse_type({type, _, boolean, []}, _, _, _) -> boolean_type();
parse_type({type, _, any, []}, _, _, _) -> any_type();
parse_type({type, _, Scalar, _}, _, _, _) when is_map_key(Scalar, ?SCALARS) ->
    scalar_type(normalize_name(Scalar));
parse_type({atom, _, Atom}, _, _, _) -> literal_type(Atom);
parse_type({integer, _, Int}, _, _, _) -> literal_type(Int);
parse_type({char, _, Char}, _, _, _) -> literal_type(<<Char/utf8>>);
parse_type({string, _, String}, _, _, _) -> literal_type(hb_util:bin(String));
parse_type({nil, _}, _, _, _) -> literal_type([]);
parse_type(Other, _TypeEnv, _VarEnv, _Seen) -> unknown_type(Other).

%% @doc `:=' declares a required key, `=>' an optional one.
field_presence(map_field_exact) -> required;
field_presence(map_field_assoc) -> optional.

%% @doc The message key a map field's key type names. A literal key is the
%% key itself; `_' is the wildcard; any other type is named by its printed
%% form, so that it can never match a real key.
key_name({atom, _, Atom}, _TypeEnv, _VarEnv, _Seen) ->
    normalize_name(Atom);
key_name({string, _, String}, _TypeEnv, _VarEnv, _Seen) ->
    hb_util:bin(String);
key_name({var, _, '_'}, _TypeEnv, _VarEnv, _Seen) ->
    <<"_">>;
key_name(Other, TypeEnv, VarEnv, Seen) ->
    case parse_type(Other, TypeEnv, VarEnv, Seen) of
        #{ <<"kind">> := <<"literal">>, <<"value">> := Value } when is_binary(Value) ->
            Value;
        #{ <<"kind">> := <<"literal">>, <<"value">> := Value } ->
            hb_util:bin(io_lib:format("~tp", [Value]));
        _ ->
            hb_util:bin(io_lib:format("~tp", [Other]))
    end.

%%% --------------------------------------------------------------------
%%% Applying a schema to a value
%%% --------------------------------------------------------------------

%% @doc Vary a value by its schema: pass it through if the schema does not
%% constrain it, else load it if it is a link, then project, coerce or
%% check it as the schema's kind requires. Return the value and whether its
%% content changed, excluding link loading alone, with the materialized links.
%% Schema failures carry those same links to the next alternative.
apply_schema(
    Schema = #{ <<"kind">> := <<"union">>, <<"members">> := Members },
    Value,
    Loaded,
    Opts
) ->
    case apply_union(Members, Value, Loaded, Opts) of
        {ok, Result} -> Result;
        {error, FailedLoads} -> schema_error({invalid_type, Schema, Value}, FailedLoads)
    end;
apply_schema(#{ <<"kind">> := Kind }, Value, Loaded, _Opts)
        when Kind =:= <<"any">>;
             Kind =:= <<"wildcard">>;
             Kind =:= <<"remote">>;
             Kind =:= <<"alias">>;
             Kind =:= <<"variable">>;
             Kind =:= <<"unknown">> ->
    {Value, {false, Loaded}};
apply_schema(Schema, Link, Loaded, Opts) when ?IS_LINK(Link) ->
    Value =
        case maps:find(Link, Loaded) of
            {ok, V} -> V;
            error -> hb_cache:ensure_loaded(Link, Opts)
        end,
    apply_schema(Schema, Value, Loaded#{ Link => Value }, Opts);
apply_schema(Schema = #{ <<"kind">> := <<"message">> }, Value, Loaded, Opts)
        when not is_map(Value) ->
    case coerce_type(Schema, Value, Opts) of
        error -> schema_error({invalid_type, Schema, Value}, Loaded);
        Value -> schema_error({invalid_type, Schema, Value}, Loaded);
        Coerced ->
            {Varied, {_, NextLoaded}} = apply_schema(Schema, Coerced, Loaded, Opts),
            {Varied, {true, NextLoaded}}
    end;
apply_schema(
    #{ <<"kind">> := <<"message">>, <<"keys">> := Keys, <<"wildcard">> := Wildcard },
    Message,
    Loaded,
    Opts
) ->
    % The declared keys are varied onto the undeclared ones the wildcard
    % admits. A key kept as given is put back unchanged, so a message the
    % schema does not alter stays the same term.
    {Varied, {Changed, NextLoaded}} =
        maps:fold(
            fun(Key, Field, Acc) -> apply_key(Key, Field, Message, Acc, Opts) end,
            apply_wildcard(Wildcard, Keys, Message, Loaded, Opts),
            Keys
        ),
    case Changed orelse map_size(Varied) =/= map_size(Message) of
        true -> {hb_message:uncommitted(Varied, Opts), {true, NextLoaded}};
        false -> {Varied, {false, NextLoaded}}
    end;
apply_schema(
    Schema = #{ <<"kind">> := <<"list">>, <<"head">> := Head, <<"tail">> := Tail },
    Value,
    Loaded,
    Opts
) ->
    case Value of
        [H | T] ->
            {VH, {CH, LH}} = apply_schema(Head, H, Loaded, Opts),
            {VT, {CT, LT}} = apply_schema(Tail, T, LH, Opts),
            {[VH | VT], {CH orelse CT, LT}};
        _ -> schema_error({invalid_type, Schema, Value}, Loaded)
    end;
apply_schema(
    Schema = #{ <<"kind">> := <<"list">>, <<"item">> := ItemType },
    Value,
    Loaded,
    Opts
) ->
    case Value of
        List when is_list(List) ->
            apply_items(lists:duplicate(length(List), ItemType), List, Loaded, Opts);
        _ ->
            schema_error({invalid_type, Schema, Value}, Loaded)
    end;
apply_schema(
    Schema = #{ <<"kind">> := <<"tuple">>, <<"items">> := Items },
    Value,
    Loaded,
    Opts
) ->
    Values =
        case Value of
            Tuple when is_tuple(Tuple) -> tuple_to_list(Tuple);
            List when is_list(List) -> List;
            _ -> error
        end,
    case is_list(Values) andalso length(Values) =:= length(Items) of
        true ->
            {Varied, {Changed, NextLoaded}} = apply_items(Items, Values, Loaded, Opts),
            {list_to_tuple(Varied), {Changed orelse not is_tuple(Value), NextLoaded}};
        false ->
            schema_error({invalid_type, Schema, Value}, Loaded)
    end;
apply_schema(Type, Value, Loaded, Opts) ->
    % A scalar, literal or range: keep a value of the type, else coerce it.
    case check_type(Type, Value) of
        true ->
            {Value, {false, Loaded}};
        false ->
            Coerced = coerce_type(Type, Value, Opts),
            case Coerced =/= error andalso check_type(Type, Coerced) of
                true -> {Coerced, {true, Loaded}};
                false -> schema_error({invalid_type, Type, Value}, Loaded)
            end
    end.

%% @doc Vary a sequence, carrying materializations from one item to the next.
apply_items(Types, Values, Loaded, Opts) ->
    lists:mapfoldl(
        fun({Type, Item}, {Changed, Acc}) ->
            {Varied, {ItemChanged, Next}} = apply_schema(Type, Item, Acc, Opts),
            {Varied, {Changed orelse ItemChanged, Next}}
        end,
        {false, Loaded},
        lists:zip(Types, Values)
    ).

%% @doc The undeclared keys of a message, as its schema's wildcard admits
%% them: none for a projection, all of them as given for `_ => _', or each
%% coerced to the wildcard's type for `_ := type()'. Track coercions separately
%% from link loads.
apply_wildcard(none, _Keys, _Message, Loaded, _Opts) ->
    {#{}, {false, Loaded}};
apply_wildcard(#{ <<"presence">> := optional }, _Keys, Message, Loaded, _Opts) ->
    {Message, {false, Loaded}};
apply_wildcard(Field, Keys, Message, Loaded, Opts) ->
    maps:fold(
        fun(Key, _Value, Acc) -> apply_key(Key, Field, Message, Acc, Opts) end,
        {#{}, {false, Loaded}},
        maps:without(maps:keys(Keys), Message)
    ).

%% @doc Vary one declared key of a message onto the accumulated result.
apply_key(Key, Field, Message, {Acc, {Changed, Loaded}} = State, Opts) ->
    #{ <<"presence">> := Presence, <<"type">> := Type } = Field,
    case maps:find(Key, Message) of
        {ok, Value} ->
            {Coerced, {ChildChanged, NextLoaded}} = apply_schema(Type, Value, Loaded, Opts),
            {Acc#{ Key => Coerced }, {Changed orelse ChildChanged, NextLoaded}};
        error when Presence =:= required -> schema_error({required_key_missing, Key}, Loaded);
        error -> State
    end.

%% @doc Vary a value by the first of the members it can be coerced to.
apply_union([], _Value, Loaded, _Opts) ->
    {error, Loaded};
apply_union([Member | Rest], Value, Loaded, Opts) ->
    try {ok, apply_schema(Member, Value, Loaded, Opts)}
    catch
        throw:{'schema-mismatch', _Reason, FailedLoads} ->
            apply_union(Rest, Value, FailedLoads, Opts)
    end.

%% @doc A rejected schema retains only its materialized source values.
schema_error(Reason, Loaded) -> throw({'schema-mismatch', Reason, Loaded}).

%%% --------------------------------------------------------------------
%%% Coercing and checking values
%%% --------------------------------------------------------------------

%% @doc Coerce a value to a schema's type through the `hb_util' converters,
%% or `error' if it cannot be. Compound types coerce their elements in turn.
coerce_type(_Type, undefined, _Opts) -> error;
coerce_type(#{ <<"kind">> := <<"integer">> }, Value, _Opts) ->
    try_coerce(fun hb_util:int/1, Value);
coerce_type(#{ <<"kind">> := <<"non-neg-integer">> }, Value, _Opts) ->
    try_coerce(fun hb_util:int/1, Value);
coerce_type(#{ <<"kind">> := <<"pos-integer">> }, Value, _Opts) ->
    try_coerce(fun hb_util:int/1, Value);
coerce_type(#{ <<"kind">> := <<"neg-integer">> }, Value, _Opts) ->
    try_coerce(fun hb_util:int/1, Value);
coerce_type(#{ <<"kind">> := <<"range">> }, Value, _Opts) ->
    try_coerce(fun hb_util:int/1, Value);
coerce_type(#{ <<"kind">> := <<"float">> }, Value, _Opts) ->
    try_coerce(fun hb_util:float/1, Value);
coerce_type(#{ <<"kind">> := <<"number">> }, Value, _Opts) ->
    coerce_with([fun hb_util:int/1, fun hb_util:float/1], Value);
coerce_type(#{ <<"kind">> := <<"binary">> }, Value, _Opts) ->
    try_coerce(fun hb_util:bin/1, Value);
coerce_type(#{ <<"kind">> := <<"bitstring">> }, Value, _Opts) ->
    try_coerce(fun hb_util:bin/1, Value);
coerce_type(#{ <<"kind">> := <<"atom">> }, Value, _Opts) ->
    try_coerce(fun hb_util:atom/1, Value);
coerce_type(#{ <<"kind">> := <<"message">> }, Value, _Opts) ->
    try_coerce(fun hb_util:map/1, Value);
coerce_type(#{ <<"kind">> := <<"tuple">>, <<"items">> := Items }, Value, Opts)
        when is_tuple(Value) ->
    coerce_type(#{ <<"kind">> => <<"tuple">>, <<"items">> => Items }, tuple_to_list(Value), Opts);
coerce_type(#{ <<"kind">> := <<"tuple">>, <<"items">> := Items }, Value, Opts)
        when is_list(Value), length(Value) =:= length(Items) ->
    case coerce_sequence(lists:zip(Items, Value), Opts) of
        error -> error;
        Coerced -> list_to_tuple(Coerced)
    end;
coerce_type(#{ <<"kind">> := <<"list">>, <<"item">> := ItemType }, Value, Opts) ->
    case Value of
        List when is_list(List) ->
            coerce_sequence([ {ItemType, Item} || Item <- List ], Opts);
        _ ->
            error
    end;
coerce_type(#{ <<"kind">> := <<"union">>, <<"members">> := Members }, Value, Opts) ->
    coerce_with(
        [ fun(V) -> coerce_type(Member, V, Opts) end || Member <- Members ],
        Value
    );
coerce_type(#{ <<"kind">> := <<"literal">>, <<"value">> := Lit }, Value, _Opts) ->
    coerce_literal(Lit, Value);
coerce_type(_Type, _Value, _Opts) ->
    error.

%% @doc Apply a converter to a value, or `error' if it rejects it.
try_coerce(Fun, Value) ->
    try Fun(Value)
    catch _:_ -> error
    end.

%% @doc The result of the first converter that accepts a value.
coerce_with([], _Value) ->
    error;
coerce_with([Fun | Rest], Value) ->
    case try_coerce(Fun, Value) of
        error -> coerce_with(Rest, Value);
        Coerced -> Coerced
    end.

%% @doc Coerce each value of a sequence to its type, or `error' if any cannot
%% be.
coerce_sequence([], _Opts) ->
    [];
coerce_sequence([{Type, Value} | Rest], Opts) ->
    case coerce_type(Type, Value, Opts) of
        error ->
            error;
        Coerced ->
            case coerce_sequence(Rest, Opts) of
                error -> error;
                CoercedRest -> [Coerced | CoercedRest]
            end
    end.

%% @doc Coerce a value to a literal: the value must convert to the literal's
%% own type and then equal it.
coerce_literal(Expected, Value) when is_integer(Expected) ->
    coerce_exact(Expected, try_coerce(fun hb_util:int/1, Value));
coerce_literal(Expected, Value) when is_float(Expected) ->
    coerce_exact(Expected, try_coerce(fun hb_util:float/1, Value));
coerce_literal(Expected, Value) when is_binary(Expected) ->
    coerce_exact(Expected, try_coerce(fun hb_util:bin/1, Value));
coerce_literal(Expected, Value) when is_boolean(Expected) ->
    case is_boolean_coercible(Value) of
        true -> coerce_exact(Expected, try_coerce(fun hb_util:bool/1, Value));
        false -> error
    end;
coerce_literal(Expected, Value) when is_atom(Expected) ->
    coerce_exact(Expected, try_coerce(fun hb_util:atom/1, Value));
coerce_literal(Expected, Value) when is_list(Expected) ->
    coerce_exact(Expected, try_coerce(fun hb_util:list/1, Value));
coerce_literal(Expected, Expected) ->
    Expected;
coerce_literal(_Expected, _Value) ->
    error.

%% @doc A coerced value, if it is the literal expected.
coerce_exact(Expected, Expected) ->
    Expected;
coerce_exact(_Expected, _Value) ->
    error.

%% @doc The values `hb_util:bool/1' reads as a boolean.
is_boolean_coercible(Value) ->
    lists:member(Value, [true, false, 1, 0, <<"true">>, <<"false">>, <<"1">>, <<"0">>]).

%% @doc Whether a value is of a schema's type as it is. Types the varier does
%% not understand admit every value.
check_type(#{ <<"kind">> := <<"integer">> }, Value) -> is_integer(Value);
check_type(#{ <<"kind">> := <<"non-neg-integer">> }, Value) -> is_integer(Value) andalso Value >= 0;
check_type(#{ <<"kind">> := <<"pos-integer">> }, Value) -> is_integer(Value) andalso Value > 0;
check_type(#{ <<"kind">> := <<"neg-integer">> }, Value) -> is_integer(Value) andalso Value < 0;
check_type(#{ <<"kind">> := <<"float">> }, Value) -> is_float(Value);
check_type(#{ <<"kind">> := <<"number">> }, Value) -> is_number(Value);
check_type(#{ <<"kind">> := <<"binary">> }, Value) -> is_binary(Value);
check_type(#{ <<"kind">> := <<"bitstring">> }, Value) -> is_bitstring(Value);
check_type(#{ <<"kind">> := <<"atom">> }, Value) -> is_atom(Value);
check_type(#{ <<"kind">> := <<"pid">> }, Value) -> is_pid(Value);
check_type(#{ <<"kind">> := <<"message">>, <<"keys">> := Keys }, Value)
        when is_map(Value) ->
    lists:all(
        fun
            ({Key, #{ <<"presence">> := required }}) -> is_map_key(Key, Value);
            (_Field) -> true
        end,
        maps:to_list(Keys)
    );
check_type(#{ <<"kind">> := <<"message">> }, _Value) -> false;
check_type(#{ <<"kind">> := <<"tuple">>, <<"items">> := Items }, Value) ->
    is_tuple(Value)
        andalso tuple_size(Value) =:= length(Items)
        andalso lists:all(
            fun({Type, Item}) -> check_type(Type, Item) end,
            lists:zip(Items, tuple_to_list(Value))
        );
check_type(#{ <<"kind">> := <<"tuple">> }, Value) -> is_tuple(Value);
check_type(#{ <<"kind">> := <<"list">>, <<"item">> := ItemType }, Value) ->
    is_list(Value) andalso lists:all(fun(Item) -> check_type(ItemType, Item) end, Value);
check_type(#{ <<"kind">> := <<"union">>, <<"members">> := Members }, Value) ->
    lists:any(fun(Member) -> check_type(Member, Value) end, Members);
check_type(#{ <<"kind">> := <<"literal">>, <<"value">> := Expected }, Value) ->
    Value =:= Expected;
check_type(#{ <<"kind">> := <<"range">>, <<"min">> := Min, <<"max">> := Max }, V) ->
    is_integer(V) andalso V >= Min andalso V =< Max;
check_type(_Type, _Value) -> true.

%%% --------------------------------------------------------------------
%%% Schema constructors
%%% --------------------------------------------------------------------

%% @doc The normalized form of a function or key name: the dashed binary
%% that AO-Core keys are matched by.
normalize_name('_') -> <<"_">>;
normalize_name(Name) when is_atom(Name) -> hb_util:atom_to_dashed_binary(Name);
normalize_name(Name) -> hb_util:bin(Name).

%% @doc The value of a literal schema, such as a range's bound.
literal_value(#{ <<"kind">> := <<"literal">>, <<"value">> := Value }) -> Value;
literal_value(_Type) -> undefined.

message_type(Keys, Wildcard) ->
    #{
        <<"kind">> => <<"message">>,
        <<"keys">> => Keys,
        <<"wildcard">> => Wildcard
    }.

any_type() -> #{ <<"kind">> => <<"any">> }.
wildcard_type() -> #{ <<"kind">> => <<"wildcard">> }.
scalar_type(Name) -> #{ <<"kind">> => Name }.
literal_type(Value) -> #{ <<"kind">> => <<"literal">>, <<"value">> => Value }.
alias_type(Name) -> #{ <<"kind">> => <<"alias">>, <<"name">> => normalize_name(Name) }.
variable_type(Name) -> #{ <<"kind">> => <<"variable">>, <<"name">> => normalize_name(Name) }.
unknown_type(Ast) ->
    #{
        <<"kind">> => <<"unknown">>,
        <<"ast">> => hb_util:bin(io_lib:format("~tp", [Ast]))
    }.
boolean_type() ->
    #{
        <<"kind">> => <<"union">>,
        <<"members">> => [literal_type(true), literal_type(false)]
    }.

%%% Tests

%% @doc Apply one schema with a fresh materialization scope.
apply_schema(Schema, Value, Opts) ->
    try apply_schema(Schema, Value, #{}, Opts) of
        {Varied, {Changed, _Loaded}} -> {Varied, Changed}
    catch throw:{'schema-mismatch', Reason, _Loaded} -> throw(Reason)
    end.

%% @doc A message schema requiring one key of a type, and nothing else.
required(Key, Type) ->
    message_type(
        #{ Key => #{ <<"presence">> => required, <<"type">> => Type } },
        none
    ).

parse_empty_projection_test() ->
    ?assertEqual(wildcard_type(), parse_type({var, 1, '_'}, #{}, #{}, [])).

map_wildcards_test() ->
    Lazy =
        parse_type(
            {type, 1, map,
                [
                    {type, 1, map_field_exact, [{atom, 1, a}, {var, 1, '_'}]},
                    {type, 1, map_field_assoc, [{var, 1, '_'}, {var, 1, '_'}]}
                ]},
            #{},
            #{},
            []
        ),
    ?assertMatch(
        #{
            <<"kind">> := <<"message">>,
            <<"keys">> := #{ <<"a">> := _ },
            <<"wildcard">> := #{ <<"presence">> := optional }
        },
        Lazy
    ),
    Force =
        parse_type(
            {type, 1, map,
                [
                    {type, 1, map_field_exact, [{var, 1, '_'}, {var, 1, '_'}]}
                ]},
            #{},
            #{},
            []
        ),
    ?assertMatch(
        #{
            <<"kind">> := <<"message">>,
            <<"wildcard">> := #{ <<"presence">> := required }
        },
        Force
    ).

apply_empty_projection_test() ->
    {Varied, _} =
        apply_schema(
            implicit_base(wildcard_type()),
            #{ <<"device">> => <<"test@1.0">>, <<"extra">> => <<"drop">> },
            #{}
        ),
    ?assertEqual(#{ <<"device">> => <<"test@1.0">> }, Varied).

%% @doc A message that a schema does not alter is returned as the same term.
unaltered_message_is_identical_test() ->
    Message = #{ <<"device">> => <<"test@1.0">>, <<"a">> => 1, <<"b">> => <<"x">> },
    Schema =
        message_type(
            #{
                <<"a">> =>
                    #{
                        <<"presence">> => required,
                        <<"type">> => scalar_type(<<"integer">>)
                    }
            },
            #{ <<"presence">> => optional, <<"type">> => wildcard_type() }
        ),
    {Varied, _} = apply_schema(implicit_base(Schema), Message, #{}),
    ?assert(erts_debug:same(Message, Varied)).

%% @doc Projecting only a child invalidates both signatures, not just the child's.
nested_projection_drops_commitments_test() ->
    Wallet = ar_wallet:new(),
    Signer = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"priv-wallet">> => Wallet
    },
    Child = hb_message:commit(
        #{ <<"slot">> => 7, <<"noise">> => 8 }, Opts, <<"httpsig@1.0">>
    ),
    Signed = hb_message:commit(
        #{ <<"child">> => Child }, Opts, <<"httpsig@1.0">>
    ),
    ?assert(hb_message:verify(Signed, [Signer], Opts)),
    Keep = #{ <<"presence">> => optional, <<"type">> => wildcard_type() },
    ChildSchema = message_type(
        #{
            <<"slot">> => #{
                <<"presence">> => required, <<"type">> => scalar_type(<<"integer">>)
            },
            <<"commitments">> => Keep
        },
        none
    ),
    Schema = message_type(
        #{ <<"child">> => #{ <<"presence">> => required, <<"type">> => ChildSchema } },
        Keep
    ),
    {Varied, _} = apply_schema(Schema, Signed, Opts),
    ?assertEqual(#{ <<"child">> => #{ <<"slot">> => 7 } }, Varied).

%% @doc Each wildcard/projection case runs independently of the others.
wildcard_commitments_test_() ->
    [
        {atom_to_list(Operation), fun() -> wildcard_commitments(Operation) end}
    || Operation <- [unchanged, loaded, coerced, projected, optional_absent]
    ].

%% @doc Keep signatures only when the admitted message's content is unchanged.
wildcard_commitments(Operation) ->
    Wallet = ar_wallet:new(),
    Signer = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"priv-wallet">> => Wallet
    },
    Payload = #{ <<"slot">> => 7, <<"extra">> => 8 },
    Signed = hb_message:commit(Payload, Opts, <<"httpsig@1.0">>),
    SignedID = hb_message:id(Signed, [Signer], Opts),
    {ok, _} = hb_cache:write(Signed, Opts),
    {ok, Lazy} = hb_cache:read(SignedID, Opts),
    ?assertMatch({link, _, _}, maps:get(<<"slot">>, Lazy)),
    ?assert(hb_message:verify(Lazy, [Signer], Opts)),
    Projection = required(<<"commitments">>, wildcard_type()),
    Integer = #{
        <<"presence">> => required, <<"type">> => scalar_type(<<"integer">>)
    },
    {Schema, Input, Expected, Preserve} =
        case Operation of
            unchanged ->
                {Projection#{ <<"wildcard">> => Integer }, Signed, Payload, true};
            loaded ->
                {Projection#{ <<"wildcard">> => Integer }, Lazy, Payload, true};
            coerced ->
                {Projection#{ <<"wildcard">> => Integer#{
                    <<"type">> => scalar_type(<<"binary">>)
                } }, Lazy, #{ <<"slot">> => <<"7">>, <<"extra">> => <<"8">> }, false};
            projected ->
                {Projection#{ <<"keys">> =>
                    (maps:get(<<"keys">>, Projection))#{ <<"slot">> => Integer }
                }, Signed, #{ <<"slot">> => 7 }, false};
            optional_absent ->
                {message_type(
                    #{ <<"absent">> => Integer#{ <<"presence">> => optional } },
                    #{ <<"presence">> => optional, <<"type">> => wildcard_type() }
                ), Signed, Payload, true}
        end,
    {Varied, _} = apply_schema(Schema, Input, Opts),
    ?assertEqual(Expected, hb_message:uncommitted(Varied, Opts)),
    case Preserve of
        true ->
            ?assert(lists:member(Signer, hb_message:signers(Varied, Opts))),
            ?assertEqual(SignedID, hb_message:id(Varied, [Signer], Opts)),
            ?assert(hb_message:verify(Varied, [Signer], Opts));
        false -> ?assertNot(hb_maps:is_key(<<"commitments">>, Varied, Opts))
    end.

%% @doc List loading is identity-preserving; changing an element is not.
list_commitments_test_() ->
    [
        {Description, fun() -> list_commitments(Value, Nested, Preserve) end}
    || {Description, Value, Nested, Preserve} <- [
        {"loaded list", 7, false, true},
        {"coerced list element", <<"007">>, false, false},
        {"loaded child in list", 7, true, true},
        {"coerced child in list", <<"007">>, true, false}
    ]
    ].

%% @doc Exercise a cached list independently from nested message variation.
list_commitments(Value, Nested, Preserve) ->
    Wallet = ar_wallet:new(),
    Signer = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Opts = #{ <<"store">> => hb_test_utils:test_store(), <<"priv-wallet">> => Wallet },
    Item = case Nested of true -> #{ <<"slot">> => Value }; false -> Value end,
    Signed = hb_message:commit(#{ <<"items">> => [Item] }, Opts, <<"httpsig@1.0">>),
    SignedID = hb_message:id(Signed, [Signer], Opts),
    {ok, _} = hb_cache:write(Signed, Opts),
    {ok, Lazy} = hb_cache:read(SignedID, Opts),
    ?assertMatch({link, _, _}, maps:get(<<"items">>, Lazy)),
    ?assert(hb_message:verify(Lazy, [Signer], Opts)),
    Integer = scalar_type(<<"integer">>),
    Keep = #{ <<"presence">> => optional, <<"type">> => wildcard_type() },
    {ItemType, Expected} =
        case Nested of
            true ->
                [LazyItem] = hb_maps:get(<<"items">>, Lazy, not_found, Opts),
                ?assertMatch({link, _, _}, maps:get(<<"slot">>, LazyItem)),
                {(required(<<"slot">>, Integer))#{ <<"wildcard">> => Keep },
                    #{ <<"slot">> => 7 }};
            false -> {Integer, 7}
        end,
    Schema = (required(<<"items">>, #{
        <<"kind">> => <<"list">>, <<"item">> => ItemType
    }))#{ <<"wildcard">> => Keep },
    {Varied, _} = apply_schema(Schema, Lazy, Opts),
    ?assertEqual([Expected], maps:get(<<"items">>, Varied)),
    case Preserve of
        true ->
            % Prove that materializing the list did not change its signed content.
            ?assert(hb_message:verify(
                Signed#{ <<"items">> => maps:get(<<"items">>, Varied) }, [Signer], Opts
            )),
            ?assert(lists:member(Signer, hb_message:signers(Varied, Opts))),
            ?assertEqual(SignedID, hb_message:id(Varied, [Signer], Opts)),
            ?assert(hb_message:verify(Varied, [Signer], Opts));
        false -> ?assertNot(hb_maps:is_key(<<"commitments">>, Varied, Opts))
    end.

selected_links_are_materialized_without_loading_omitted_keys_test() ->
    Store = hb_test_utils:test_store(),
    Opts = #{ <<"store">> => Store },
    hb_store:reset(Store),
    {ok, SlotPath} = hb_cache:write(<<"7">>, Opts),
    Missing = {link, <<"data/not-present">>, #{}},
    Schema = required(<<"deep">>, required(<<"slot">>, scalar_type(<<"integer">>))),
    {Varied, _} =
        apply_schema(
            Schema,
            #{
                <<"deep">> =>
                    #{
                        <<"slot">> => {link, SlotPath, #{}},
                        <<"omitted">> => Missing
                    },
                <<"omitted">> => Missing
            },
            Opts
        ),
    ?assertEqual(#{ <<"deep">> => #{ <<"slot">> => 7 } }, Varied).

explicit_wildcard_preserves_lazy_links_test_() ->
    [
        {atom_to_list(Presence) ++ " " ++ binary_to_list(maps:get(<<"kind">>, Type)), fun() ->
            explicit_wildcard_preserves_lazy_links(Presence, Type)
        end}
    || Presence <- [required, optional], Type <- [wildcard_type(), any_type(),
        #{ <<"kind">> => <<"union">>,
            <<"members">> => [wildcard_type(), scalar_type(<<"integer">>)] }]
    ].

%% @doc Unconstrained fields must not attempt to load even a missing link.
explicit_wildcard_preserves_lazy_links(Presence, Type) ->
    Missing = {link, <<"data/not-present">>, #{}},
    Schema =
        message_type(
            #{
                <<"scheduler">> =>
                    #{
                        <<"presence">> => Presence,
                        <<"type">> => Type
                    }
            },
            none
        ),
    {Varied, _} = apply_schema(Schema, #{ <<"scheduler">> => Missing }, #{}),
    ?assertEqual(#{ <<"scheduler">> => Missing }, Varied).

optional_wildcard_preserves_links_and_sequences_materialize_test() ->
    Store = hb_test_utils:test_store(),
    Opts = #{ <<"store">> => Store },
    hb_store:reset(Store),
    {ok, ValuePath} = hb_cache:write(<<"8">>, Opts),
    Link = {link, ValuePath, #{}},
    WildcardSchema =
        message_type(
            #{},
            #{ <<"presence">> => optional, <<"type">> => wildcard_type() }
        ),
    {Varied, _} =
        apply_schema(
            WildcardSchema,
            #{ <<"extra">> => Link },
            Opts
        ),
    ?assertEqual(#{ <<"extra">> => Link }, Varied),
    Integer = scalar_type(<<"integer">>),
    {VariedList, _} =
        apply_schema(#{ <<"kind">> => <<"list">>, <<"item">> => Integer }, [Link], Opts),
    ?assertEqual([8], VariedList),
    {VariedTuple, _} =
        apply_schema(#{ <<"kind">> => <<"tuple">>, <<"items">> => [Integer] }, {Link}, Opts),
    ?assertEqual({8}, VariedTuple).

union_uses_declared_order_test() ->
    Binary = scalar_type(<<"binary">>),
    List = #{ <<"kind">> => <<"list">>, <<"item">> => Binary },
    Value = [<<"one">>, <<"two">>],
    {Varied, _} =
        apply_schema(
            #{ <<"kind">> => <<"union">>, <<"members">> => [Binary, List] },
            Value,
            #{}
        ),
    ?assertEqual(<<"onetwo">>, Varied).

union_passthrough_and_fallback_test() ->
    Store = hb_test_utils:test_store(),
    Opts = #{ <<"store">> => Store },
    hb_store:reset(Store),
    {ok, SlotPath} = hb_cache:write(<<"9">>, Opts),
    Integer = scalar_type(<<"integer">>),
    Binary = scalar_type(<<"binary">>),
    Union =
        fun(Members) ->
            #{ <<"kind">> => <<"union">>, <<"members">> => Members }
        end,
    {VariedMessage, _} =
        apply_schema(
            Union([unknown_type({record, tx}), required(<<"slot">>, Integer)]),
            #{ <<"slot">> => {link, SlotPath, #{}} },
            Opts
        ),
    ?assertEqual(#{ <<"slot">> => {link, SlotPath, #{}} }, VariedMessage),
    {VariedBinary, _} =
        apply_schema(Union([any_type(), wildcard_type(), Binary]), <<"value">>, #{}),
    ?assertEqual(<<"value">>, VariedBinary),
    {VariedSecond, _} =
        apply_schema(
            Union([required(<<"first">>, Integer), required(<<"second">>, Integer)]),
            #{ <<"second">> => 2 },
            #{}
        ),
    ?assertEqual(#{ <<"second">> => 2 }, VariedSecond),
    {VariedFallback, _} =
        apply_schema(
            Union([required(<<"value">>, Integer), required(<<"value">>, Binary)]),
            #{ <<"value">> => <<"text">> },
            #{}
        ),
    ?assertEqual(#{ <<"value">> => <<"text">> }, VariedFallback).
