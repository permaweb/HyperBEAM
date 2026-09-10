%%% @doc Vary the inputs of a device function by its `-spec'.
%%%
%%% A device function's Dialyzer spec describes the base and request messages
%%% it reads and the result it returns. AO-Core uses the spec to <em>vary</em>
%%% the messages before execution: the function receives the keys it declares,
%%% loaded and coerced to their declared types, and the execution's hashpath is
%%% derived from the varied messages alone. Every execution the spec deems
%%% equivalent thereby shares one cache entry, however its messages otherwise
%%% differ. A function without a spec executes upon its inputs as given.
%%%
%%% Specs are read from a module's BEAM by `extract/1' and compiled into a
%%% <em>schema</em>: a map from each spec'd function's normalized name and
%%% arity to the schemas of its arguments and result. `hb_device_load:schema/2'
%%% memoises them. The type syntax means, for a message argument:
%%%
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
%%%       understand (remote types, records, type variables).</li>
%%% </ul>
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
        {ok, Schema} ->
            {BaseSchema, ReqSchema, ReturnSchema} =
                execution_schemas(Schema, AddKey),
            {VariedBase, _} = apply_schema(implicit_base(BaseSchema), Base, Opts),
            {VariedReq, _} =
                apply_schema(
                    implicit_request(ReqSchema),
                    request_with_key(Req, AddKey),
                    Opts
                ),
            {ok, VariedBase, VariedReq, overlay(ReturnSchema)};
        {error, _} ->
            no_spec
    end.

%% @doc The schema of the function that will execute `Key': its spec, found
%% in its own module by its name and arity, or -- for a `handler' or
%% `default' that serves many keys -- by the key.
function_schema(Func, Key, Opts) ->
    maybe
        true ?= is_function(Func) orelse {error, not_found},
        {module, Module} = erlang:fun_info(Func, module),
        {name, Name} = erlang:fun_info(Func, name),
        {arity, Arity} = erlang:fun_info(Func, arity),
        {ok, Schemas} ?= hb_device_load:schema(Module, Opts),
        {error, not_found} ?= named_schema(normalize_name(Name), Arity, Schemas),
        named_schema(Key, Arity, Schemas)
    end.

%% @doc The schema of the function of the given normalized name and arity.
named_schema(Name, Arity, Schemas) ->
    case Schemas of
        #{ Name := #{ Arity := Schema } } -> {ok, Schema};
        _ -> {error, not_found}
    end.

%% @doc The base, request and result schemas of an execution. A function that
%% takes the key as its first argument reads the base and request from its
%% second and third; an argument the spec does not cover is not varied.
execution_schemas(#{ <<"args">> := Args, <<"return">> := Return }, AddKey) ->
    Offset =
        case AddKey of
            false -> 0;
            _ -> 1
        end,
    {
        maybe_nth(1 + Offset, Args, any_type()),
        maybe_nth(2 + Offset, Args, any_type()),
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
            {ok,
                lists:foldl(
                    fun(Spec, Acc) -> put_spec(Spec, TypeEnv, Acc) end,
                    #{},
                    [ Attr || Attr = {attribute, _, spec, _} <- Forms ]
                )};
        Other ->
            {error, {abstract_code_unavailable, Other}}
    end;
extract(Module) when is_atom(Module) ->
    case code:get_object_code(Module) of
        {Module, Beam, _Path} -> extract(Beam);
        error -> {error, {object_code_unavailable, Module}}
    end.

%% @doc The module's own type declarations, by name, for expansion when a
%% spec refers to them.
build_type_env(Forms) ->
    maps:from_list(
        [
            {Name, #{ vars => [ var_name(Var) || Var <- Vars ], ast => Ast }}
        ||
            {attribute, _, Tag, {Name, Ast, Vars}} <- Forms,
            Tag =:= type orelse Tag =:= opaque
        ]
    ).

%% @doc The name of a type's parameter.
var_name({var, _, Name}) -> Name;
var_name(Name) -> Name.

%% @doc Add a spec to the module schema under its function's normalized name
%% and arity. A spec with several clauses describes no single shape of input
%% and is not varied.
put_spec({attribute, _, spec, {{Name, Arity}, [Clause]}}, TypeEnv, Schemas) ->
    {Args, Return} = parse_fun_spec(Clause, TypeEnv),
    NormName = normalize_name(Name),
    Arities = maps:get(NormName, Schemas, #{}),
    Schemas#{
        NormName =>
            Arities#{
                Arity => #{ <<"args">> => Args, <<"return">> => Return }
            }
    };
put_spec(_Spec, _TypeEnv, Schemas) ->
    Schemas.

%% @doc The argument and result schemas of a spec clause, with any `when'
%% constraints dropped.
parse_fun_spec({type, _, bounded_fun, [FunSpec, _Constraints]}, TypeEnv) ->
    parse_fun_spec(FunSpec, TypeEnv);
parse_fun_spec({type, _, 'fun', [{type, _, product, Args}, Return]}, TypeEnv) ->
    {
        [ parse_type(Arg, TypeEnv, #{}, []) || Arg <- Args ],
        parse_type(Return, TypeEnv, #{}, [])
    };
parse_fun_spec(Other, _TypeEnv) ->
    {[unknown_type(Other)], any_type()}.

%% @doc Compile an abstract type into a schema. `TypeEnv' holds the module's
%% own types, `VarEnv' the bindings of the type variables of the one being
%% expanded, and `Seen' the types under expansion, so that a recursive type
%% becomes an alias rather than a loop.
parse_type({ann_type, _, [_Var, Type]}, TypeEnv, VarEnv, Seen) ->
    parse_type(Type, TypeEnv, VarEnv, Seen);
parse_type({var, _, '_'}, _TypeEnv, _VarEnv, _Seen) ->
    wildcard_type();
parse_type({var, _, Name}, TypeEnv, VarEnv, Seen) ->
    case maps:find(Name, VarEnv) of
        {ok, Bound} -> parse_type(Bound, TypeEnv, VarEnv, Seen);
        error -> variable_type(Name)
    end;
parse_type({user_type, _, Name, Args}, TypeEnv, VarEnv, Seen) ->
    case lists:member(Name, Seen) orelse maps:find(Name, TypeEnv) of
        {ok, #{ vars := Vars, ast := Ast }} ->
            parse_type(
                Ast,
                TypeEnv,
                maps:merge(VarEnv, maps:from_list(lists:zip(Vars, Args))),
                [Name | Seen]
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
parse_type({atom, _, Atom}, _, _, _) -> literal_type(hb_util:bin(Atom));
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
%% content changed, excluding link loading alone.
apply_schema(#{ <<"kind">> := <<"any">> }, Value, _Opts) ->
    {Value, false};
apply_schema(#{ <<"kind">> := <<"wildcard">> }, Value, _Opts) ->
    {Value, false};
apply_schema(#{ <<"kind">> := Kind }, Value, _Opts)
        when Kind =:= <<"remote">>;
             Kind =:= <<"alias">>;
             Kind =:= <<"variable">>;
             Kind =:= <<"unknown">> ->
    {Value, false};
apply_schema(Schema, Link, Opts) when ?IS_LINK(Link) ->
    apply_schema(Schema, hb_cache:ensure_loaded(Link, Opts), Opts);
apply_schema(Schema = #{ <<"kind">> := <<"message">> }, Value, Opts)
        when not is_map(Value) ->
    case coerce_type(Schema, Value, Opts) of
        error -> throw({invalid_type, Schema, Value});
        Value -> throw({invalid_type, Schema, Value});
        Coerced ->
            {Varied, _Changed} = apply_schema(Schema, Coerced, Opts),
            {Varied, true}
    end;
apply_schema(
    #{ <<"kind">> := <<"message">>, <<"keys">> := Keys, <<"wildcard">> := Wildcard },
    Message,
    Opts
) ->
    % The declared keys are varied onto the undeclared ones the wildcard
    % admits. A key kept as given is put back unchanged, so a message the
    % schema does not alter stays the same term.
    {Varied, Changed} =
        maps:fold(
            fun(Key, Field, Acc) -> apply_key(Key, Field, Message, Acc, Opts) end,
            apply_wildcard(Wildcard, Keys, Message, Opts),
            Keys
        ),
    case Changed orelse map_size(Varied) =/= map_size(Message) of
        true -> {hb_message:uncommitted(Varied, Opts), true};
        false -> {Varied, false}
    end;
apply_schema(
    Schema = #{ <<"kind">> := <<"list">>, <<"item">> := ItemType },
    Value,
    Opts
) ->
    case try_coerce(fun hb_util:list/1, Value) of
        List when is_list(List) ->
            lists:mapfoldl(
                fun(Item, Changed) ->
                    {Varied, ItemChanged} = apply_schema(ItemType, Item, Opts),
                    {Varied, Changed orelse ItemChanged}
                end,
                List =/= Value,
                List
            );
        _ ->
            throw({invalid_type, Schema, Value})
    end;
apply_schema(
    Schema = #{ <<"kind">> := <<"tuple">>, <<"items">> := Items },
    Value,
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
            {Varied, Changed} =
                lists:mapfoldl(
                    fun({Type, Item}, Acc) ->
                        {VariedItem, ItemChanged} = apply_schema(Type, Item, Opts),
                        {VariedItem, Acc orelse ItemChanged}
                    end,
                    not is_tuple(Value),
                    lists:zip(Items, Values)
                ),
            {list_to_tuple(Varied), Changed};
        false ->
            throw({invalid_type, Schema, Value})
    end;
apply_schema(
    Schema = #{ <<"kind">> := <<"union">>, <<"members">> := Members },
    Value,
    Opts
) ->
    case apply_union(Members, Value, Opts) of
        {ok, Result} -> Result;
        error -> throw({invalid_type, Schema, Value})
    end;
apply_schema(Type, Value, Opts) ->
    % A scalar, literal or range: keep a value of the type, else coerce it.
    case check_type(Type, Value) of
        true ->
            {Value, false};
        false ->
            Coerced = coerce_type(Type, Value, Opts),
            case Coerced =/= error andalso check_type(Type, Coerced) of
                true -> {Coerced, true};
                false -> throw({invalid_type, Type, Value})
            end
    end.

%% @doc The undeclared keys of a message, as its schema's wildcard admits
%% them: none for a projection, all of them as given for `_ => _', or each
%% coerced to the wildcard's type for `_ := type()'. Track coercions separately
%% from link loads.
apply_wildcard(none, _Keys, _Message, _Opts) ->
    {#{}, false};
apply_wildcard(#{ <<"presence">> := optional }, _Keys, Message, _Opts) ->
    {Message, false};
apply_wildcard(Field, Keys, Message, Opts) ->
    maps:fold(
        fun(Key, _Value, Acc) -> apply_key(Key, Field, Message, Acc, Opts) end,
        {#{}, false},
        maps:without(maps:keys(Keys), Message)
    ).

%% @doc Vary one declared key of a message onto the accumulated result.
apply_key(Key, Field, Message, {Acc, Changed} = State, Opts) ->
    #{ <<"presence">> := Presence, <<"type">> := Type } = Field,
    case maps:find(Key, Message) of
        {ok, RawValue} ->
            Value =
                case is_passthrough_schema(Type) of
                    true -> RawValue;
                    false -> hb_cache:ensure_loaded(RawValue, Opts)
                end,
            {Coerced, ChildChanged} = apply_schema(Type, Value, Opts),
            {Acc#{ Key => Coerced }, Changed orelse ChildChanged};
        error when Presence =:= required -> throw({required_key_missing, Key});
        error -> State
    end.

%% @doc Vary a value by the first member of a union that admits it as it is,
%% else by the first that it can be coerced to.
apply_union(Members, Value, Opts) ->
    case matching_union_member(Members, Value) of
        {ok, Member} ->
            try {ok, apply_schema(Member, Value, Opts)}
            catch
                throw:{invalid_type, _, _} ->
                    apply_coerced_union(Members, Value, Opts);
                throw:{required_key_missing, _} ->
                    apply_coerced_union(Members, Value, Opts)
            end;
        error -> apply_coerced_union(Members, Value, Opts)
    end.

%% @doc The first constraining member of a union that a value already
%% satisfies. Members that pass every value through never match, so that
%% they cannot shadow a constraining member.
matching_union_member([], _Value) ->
    error;
matching_union_member([Member | Rest], Value) ->
    case not is_passthrough_schema(Member) andalso check_type(Member, Value) of
        true -> {ok, Member};
        false -> matching_union_member(Rest, Value)
    end.

%% @doc Vary a value by the first member of a union it can be coerced to,
%% trying the constraining members before those that pass it through.
apply_coerced_union(Members, Value, Opts) ->
    {PassThrough, Constrained} =
        lists:partition(fun is_passthrough_schema/1, Members),
    apply_coerced_union_ordered(Constrained ++ PassThrough, Value, Opts).

%% @doc Vary a value by the first of the members it can be coerced to.
apply_coerced_union_ordered([], _Value, _Opts) ->
    error;
apply_coerced_union_ordered([Member | Rest], Value, Opts) ->
    try {ok, apply_schema(Member, Value, Opts)}
    catch
        throw:{invalid_type, _, _} ->
            apply_coerced_union_ordered(Rest, Value, Opts);
        throw:{required_key_missing, _} ->
            apply_coerced_union_ordered(Rest, Value, Opts)
    end.

%% @doc Whether a schema passes every value through unvaried.
is_passthrough_schema(#{ <<"kind">> := Kind }) ->
    lists:member(
        Kind,
        [<<"any">>, <<"wildcard">>, <<"remote">>, <<"alias">>,
            <<"variable">>, <<"unknown">>]
    );
is_passthrough_schema(_Schema) ->
    false.

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
    case try_coerce(fun hb_util:list/1, Value) of
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

%% @doc A message schema requiring one key of a type, and nothing else.
required(Key, Type) ->
    message_type(
        #{ Key => #{ <<"presence">> => required, <<"type">> => Type } },
        none
    ).

%% @doc Typed wildcards exclude declared keys and distinguish loading from coercion.
wildcard_changes_test_() ->
    [
        {binary_to_list(Type), fun() -> wildcard_changes(Type) end}
    ||
        Type <- [<<"integer">>, <<"binary">>]
    ].

%% @doc Only changing a wildcard's values sets the content-change flag.
wildcard_changes(Type) ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Message = #{ <<"slot">> => 7, <<"extra">> => 8 },
    {ok, ID} = hb_cache:write(Message, Opts),
    {ok, Lazy} = hb_cache:read(ID, Opts),
    ?assertMatch({link, _, _}, maps:get(<<"slot">>, Lazy)),
    Wildcard = #{ <<"presence">> => required, <<"type">> => scalar_type(Type) },
    {Expected, Changed} =
        case Type of
            <<"integer">> -> {Message, false};
            <<"binary">> -> {#{ <<"slot">> => <<"7">>, <<"extra">> => <<"8">> }, true}
        end,
    Schema = (required(<<"keep">>, wildcard_type()))#{ <<"wildcard">> => Wildcard },
    ?assertEqual(
        {Expected#{ <<"keep">> => <<"x">> }, Changed},
        apply_schema(Schema, Lazy#{ <<"keep">> => <<"x">> }, Opts)
    ).

%% @doc List elements propagate content changes, including nested messages.
list_changes_test_() ->
    Cases =
        [
            {"loaded list", 7, false, false},
            {"coerced list element", <<"007">>, false, true},
            {"loaded child in list", 7, true, false},
            {"coerced child in list", <<"007">>, true, true}
        ],
    [
        {Description, fun() -> list_changes(Value, Nested, Changed) end}
    ||
        {Description, Value, Nested, Changed} <- Cases
    ].

%% @doc Materialization alone must not set the enclosing message's change flag.
list_changes(Value, Nested, Changed) ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Item =
        case Nested of
            true -> #{ <<"slot">> => Value };
            false -> Value
        end,
    {ok, ID} = hb_cache:write(#{ <<"items">> => [Item] }, Opts),
    {ok, Lazy} = hb_cache:read(ID, Opts),
    ?assertMatch({link, _, _}, maps:get(<<"items">>, Lazy)),
    Integer = scalar_type(<<"integer">>),
    {ItemType, Expected} =
        case Nested of
            true ->
                [LazyItem] = hb_maps:get(<<"items">>, Lazy, not_found, Opts),
                ?assertMatch({link, _, _}, maps:get(<<"slot">>, LazyItem)),
                {required(<<"slot">>, Integer), #{ <<"slot">> => 7 }};
            false -> {Integer, 7}
        end,
    List = #{ <<"kind">> => <<"list">>, <<"item">> => ItemType },
    Schema = required(<<"items">>, List),
    {Varied, ActualChanged} = apply_schema(Schema, Lazy, Opts),
    ?assertEqual([Expected], maps:get(<<"items">>, Varied)),
    ?assertEqual(Changed, ActualChanged).

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
        {atom_to_list(Presence),
            {binary_to_list(maps:get(<<"kind">>, Type)),
                fun() ->
                    explicit_wildcard_preserves_lazy_links(Presence, Type)
                end}}
    ||
        Presence <- [required, optional], Type <- [wildcard_type(), any_type()]
    ].

%% @doc Unconstrained fields must not attempt to load even a missing link.
explicit_wildcard_preserves_lazy_links(Presence, Type) ->
    Missing = {link, <<"data/not-present">>, #{}},
    Field = #{ <<"presence">> => Presence, <<"type">> => Type },
    Schema = message_type(#{ <<"scheduler">> => Field }, none),
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
        apply_schema(WildcardSchema, #{ <<"extra">> => Link }, Opts),
    ?assertEqual(#{ <<"extra">> => Link }, Varied),
    Integer = scalar_type(<<"integer">>),
    {VariedList, _} =
        apply_schema(#{ <<"kind">> => <<"list">>, <<"item">> => Integer }, [Link], Opts),
    ?assertEqual([8], VariedList),
    {VariedTuple, _} =
        apply_schema(#{ <<"kind">> => <<"tuple">>, <<"items">> => [Integer] }, {Link}, Opts),
    ?assertEqual({8}, VariedTuple).

union_preserves_an_existing_member_type_test() ->
    Binary = scalar_type(<<"binary">>),
    List = #{ <<"kind">> => <<"list">>, <<"item">> => Binary },
    Value = [<<"one">>, <<"two">>],
    Schema = #{ <<"kind">> => <<"union">>, <<"members">> => [Binary, List] },
    {Varied, _} = apply_schema(Schema, Value, #{}),
    ?assertEqual(Value, Varied).

union_passthrough_members_do_not_swallow_constrained_members_test() ->
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
    ?assertEqual(#{ <<"slot">> => 9 }, VariedMessage),
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
