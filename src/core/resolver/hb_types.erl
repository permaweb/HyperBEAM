%%% @doc Extract Dialyzer-style device specs and vary AO-Core inputs.
-module(hb_types).
-export([extract/2, vary/7, beam_to_schema/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Apply the resolved function's base/request schemas to one execution.
vary(Device, Key, Func, AddKey, Base, Req, Opts) ->
    case function_schema(Device, Func, Key, Opts) of
        undefined ->
            no_spec;
        Schema ->
            {BaseSchema, ReqSchema, ReturnSchema} =
                execution_schemas(Schema, AddKey),
            ReqWithKey =
                case AddKey of
                    false -> Req;
                    _ -> Req#{ <<"path">> => Key }
                end,
            VariedBase = apply_schema(implicit_base(BaseSchema), Base, Opts),
            VariedReq =
                apply_schema(implicit_request(ReqSchema), ReqWithKey, Opts),
            {ok,
                VariedBase,
                VariedReq,
                overlay(ReturnSchema)}
    end.

%% @doc Extract the public function schemas from a device module.
extract(Device, _Opts) when is_map(Device) ->
    {error, {unsupported_device_type, Device}};
extract(Module, Opts) when is_atom(Module) ->
    case hb_opts:get(<<"caching-schema">>, false, Opts) of
        true ->
            {error, caching_schema};
        false ->
            case code:ensure_loaded(Module) of
                {module, Module} -> cached_extract(Module, Opts);
                {error, Reason} -> {error, {module_not_loaded, Module, Reason}}
            end
    end;
extract(Device, Opts) when is_binary(Device) ->
    case hb_device_load:reference(Device, Opts) of
        {ok, Module} -> extract(Module, Opts);
        Error -> Error
    end;
extract(Device, _Opts) ->
    {error, {unsupported_device_type, Device}}.

cached_extract(Module, Opts) ->
    Version = module_version(Module),
    CacheKey = {?MODULE, extract, Module},
    case erlang:get(CacheKey) of
        {Version, Schema} ->
            Schema;
        _ ->
            Schema =
                case hb_device_load:schema(Module, Opts) of
                    {ok, CachedSchema} -> {ok, CachedSchema};
                    _ -> do_extract(Module)
                end,
            erlang:put(CacheKey, {Version, Schema}),
            Schema
    end.

module_version(Module) ->
    try Module:module_info(md5)
    catch _:_ -> code:which(Module)
    end.

do_extract(Module) ->
    case module_beam(Module) of
        unavailable ->
            {error, {abstract_code_unavailable, Module, unavailable}};
        Beam -> beam_to_schema(Module, Beam)
    end.

%% @doc Extract a module's public function schemas from its BEAM bytes.
beam_to_schema(Module, Beam) ->
    case beam_lib:chunks(Beam, [abstract_code]) of
        {ok, {_, [{abstract_code, {_, Forms}}]}} ->
            TypeEnv = build_type_env(Forms),
            Specs = [ Attr || Attr = {attribute, _, spec, _} <- Forms ],
            {ok,
                #{
                    <<"keys">> =>
                        lists:foldl(
                            fun(Spec, Acc) ->
                                case spec_to_schema(Spec, TypeEnv) of
                                    false -> Acc;
                                    {Name, Schema} ->
                                        store_schema(Name, Schema, Acc)
                                end
                            end,
                            #{},
                            Specs
                        )
                }};
        Error ->
            {error, {abstract_code_unavailable, Module, Error}}
    end.

module_beam(Module) ->
    case code:get_object_code(Module) of
        {Module, Beam, _Filename} ->
            Beam;
        _ ->
            case code:which(Module) of
                Path when is_list(Path); is_binary(Path) -> Path;
                _ -> unavailable
            end
    end.

build_type_env(Forms) ->
    lists:foldl(
        fun
            ({attribute, _, Tag, {Name, Ast, Vars}}, Acc)
                    when Tag =:= type; Tag =:= opaque ->
                Acc#{
                    Name =>
                        #{
                            vars => [var_name(Var) || Var <- Vars],
                            ast => Ast
                        }
                };
            (_, Acc) ->
                Acc
        end,
        #{},
        Forms
    ).

spec_to_schema({attribute, _, spec, {{Name, Arity}, [Spec]}}, TypeEnv) ->
    {Args, Return} = parse_fun_spec(Spec, TypeEnv),
    {
        normalize_name(Name),
        #{
            <<"arity">> => Arity,
            <<"args">> => Args,
            <<"return">> => Return
        }
    };
spec_to_schema(_, _) ->
    false.

store_schema(Name, Schema, Schemas) ->
    case maps:get(Name, Schemas, undefined) of
        undefined ->
            Schemas#{ Name => Schema };
        Existing ->
            ExistingArity = maps:get(<<"arity">>, Existing),
            SchemaArity = maps:get(<<"arity">>, Schema),
            Overloads0 =
                maps:get(
                    <<"overloads">>,
                    Existing,
                    #{ ExistingArity => maps:without([<<"overloads">>], Existing) }
                ),
            Schemas#{
                Name =>
                    Schema#{
                        <<"overloads">> =>
                            Overloads0#{
                                SchemaArity =>
                                    maps:without([<<"overloads">>], Schema)
                            }
                    }
            }
    end.

function_schema(Device, Func, Key, Opts) ->
    case extract(Device, Opts) of
        {ok, #{ <<"keys">> := Schemas }} ->
            case function_schema(Func, Key, Schemas) of
                undefined -> function_module_schema(Device, Func, Key, Opts);
                Schema -> Schema
            end;
        {error, _Reason} ->
            function_module_schema(Device, Func, Key, Opts)
    end.

function_module_schema(Device, Func, Key, Opts) ->
    case erlang:fun_info(Func, module) of
        {module, Device} ->
            undefined;
        {module, Module} ->
            case extract(Module, Opts) of
                {ok, #{ <<"keys">> := Schemas }} ->
                    function_schema(Func, Key, Schemas);
                {error, _Reason} ->
                    undefined
            end;
        _ ->
            undefined
    end.

function_schema(Func, Key, Schemas) ->
    {arity, Arity} = erlang:fun_info(Func, arity),
    ByName =
        case erlang:fun_info(Func, name) of
            {name, Name} -> named_schema(Name, Arity, Schemas);
            _ -> undefined
        end,
    case ByName of
        undefined -> named_schema(Key, Arity, Schemas);
        Schema -> Schema
    end.

named_schema(Name, Arity, Schemas) ->
    case maps:get(normalize_name(Name), Schemas, undefined) of
        undefined ->
            undefined;
        #{ <<"overloads">> := Overloads } ->
            maps:get(Arity, Overloads, undefined);
        #{ <<"arity">> := Arity } = Schema ->
            Schema;
        _ ->
            undefined
    end.

execution_schemas(Schema, AddKey) ->
    Args = maps:get(<<"args">>, Schema, []),
    Offset =
        case AddKey of
            false -> 0;
            _ -> 1
        end,
    {
        maybe_nth(1 + Offset, Args, any_type()),
        maybe_nth(2 + Offset, Args, any_type()),
        maps:get(<<"return">>, Schema, any_type())
    }.

maybe_nth(N, List, Default) ->
    case catch lists:nth(N, List) of
        {'EXIT', _} -> Default;
        Value -> Value
    end.

implicit_base(Schema) ->
    implicit_key(top_level_schema(Schema), <<"device">>, optional).

implicit_request(Schema) ->
    implicit_key(top_level_schema(Schema), <<"path">>, required).

top_level_schema(#{ <<"kind">> := <<"wildcard">> }) ->
    message_type(#{}, none);
top_level_schema(Schema) ->
    Schema.

implicit_key(Schema = #{ <<"kind">> := <<"message">>, <<"keys">> := Keys }, Key, Presence) ->
    case maps:is_key(Key, Keys) of
        true ->
            Schema;
        false ->
            Schema#{
                <<"keys">> =>
                    Keys#{
                        Key =>
                            #{
                                <<"presence">> => Presence,
                                <<"type">> => any_type()
                            }
                    }
            }
    end;
implicit_key(Schema, _Key, _Presence) ->
    Schema.

overlay(ReturnSchema) ->
    case overlay_type(ReturnSchema) of
        base -> base;
        request -> request;
        _ -> none
    end.

overlay_type(#{ <<"kind">> := <<"message">>, <<"keys">> := Keys }) ->
    overlay_marker(
        maps:get(
            <<"type">>,
            maps:get(<<"...">>, Keys, #{}),
            #{}
        )
    );
overlay_type(#{ <<"kind">> := <<"tuple">>, <<"items">> := Items }) ->
    first_overlay(Items);
overlay_type(#{ <<"kind">> := <<"union">>, <<"members">> := Members }) ->
    first_overlay(Members);
overlay_type(_) ->
    none.

first_overlay([]) ->
    none;
first_overlay([Schema | Rest]) ->
    case overlay_type(Schema) of
        none -> first_overlay(Rest);
        Overlay -> Overlay
    end.

overlay_marker(#{ <<"kind">> := <<"literal">>, <<"value">> := <<"base">> }) -> base;
overlay_marker(#{ <<"kind">> := <<"literal">>, <<"value">> := <<"request">> }) -> request;
overlay_marker(#{ <<"kind">> := <<"alias">>, <<"name">> := <<"base">> }) -> base;
overlay_marker(#{ <<"kind">> := <<"alias">>, <<"name">> := <<"request">> }) -> request;
overlay_marker(_) -> none.

parse_fun_spec({type, _, bounded_fun, [FunSpec, _Constraints]}, TypeEnv) ->
    parse_fun_spec(FunSpec, TypeEnv);
parse_fun_spec({type, _, 'fun', [{type, _, product, Args}, Return]}, TypeEnv) ->
    {
        lists:map(fun(Arg) -> parse_type(Arg, TypeEnv, #{}, []) end, Args),
        parse_type(Return, TypeEnv, #{}, [])
    };
parse_fun_spec(Other, _TypeEnv) ->
    {[unknown_type(Other)], any_type()}.

parse_type({ann_type, _, [_Var, Type]}, TypeEnv, VarEnv, Seen) ->
    parse_type(Type, TypeEnv, VarEnv, Seen);
parse_type({var, _, '_'}, _TypeEnv, _VarEnv, _Seen) ->
    wildcard_type();
parse_type({var, _, Name}, TypeEnv, VarEnv, Seen) ->
    case maps:get(Name, VarEnv, undefined) of
        undefined -> variable_type(Name);
        Bound -> parse_type(Bound, TypeEnv, VarEnv, Seen)
    end;
parse_type({user_type, _, Name, Args}, TypeEnv, VarEnv, Seen) ->
    case lists:member(Name, Seen) of
        true ->
            alias_type(Name);
        false ->
            case maps:get(Name, TypeEnv, undefined) of
                undefined ->
                    alias_type(Name);
                #{ vars := Vars, ast := Ast } ->
                    parse_type(
                        Ast,
                        TypeEnv,
                        maps:merge(VarEnv, maps:from_list(lists:zip(Vars, Args))),
                        [Name | Seen]
                    )
            end
    end;
parse_type({remote_type, _, [{atom, _, Mod}, {atom, _, Name}, Args]},
        TypeEnv, VarEnv, Seen) ->
    #{
        <<"kind">> => <<"remote">>,
        <<"module">> => normalize_name(Mod),
        <<"name">> => normalize_name(Name),
        <<"args">> =>
            lists:map(fun(Arg) -> parse_type(Arg, TypeEnv, VarEnv, Seen) end, Args)
    };
parse_type({type, _, map, any}, _TypeEnv, _VarEnv, _Seen) ->
    any_type();
parse_type({type, _, map, Fields}, TypeEnv, VarEnv, Seen) ->
    {Keys, Wildcard} =
        lists:foldl(
            fun({type, _, Assoc, [KeyAst, ValueAst]}, {KeyAcc, WildAcc}) ->
                Key = key_name(KeyAst, TypeEnv, VarEnv, Seen),
                Field =
                    #{
                        <<"presence">> => field_presence(Assoc),
                        <<"type">> => parse_type(ValueAst, TypeEnv, VarEnv, Seen)
                    },
                case Key of
                    <<"_">> -> {KeyAcc, Field};
                    _ -> {KeyAcc#{ Key => Field }, WildAcc}
                end
            end,
            {#{}, none},
            Fields
        ),
    message_type(Keys, Wildcard);
parse_type({type, _, ListType, [Item]}, TypeEnv, VarEnv, Seen)
        when ListType =:= list; ListType =:= nonempty_list ->
    #{ <<"kind">> => <<"list">>, <<"item">> => parse_type(Item, TypeEnv, VarEnv, Seen) };
parse_type({type, _, ListType, []}, _TypeEnv, _VarEnv, _Seen)
        when ListType =:= list; ListType =:= nonempty_list ->
    #{ <<"kind">> => <<"list">>, <<"item">> => any_type() };
parse_type({type, _, ListType, Item}, TypeEnv, VarEnv, Seen)
        when ListType =:= list; ListType =:= nonempty_list ->
    #{ <<"kind">> => <<"list">>, <<"item">> => parse_type(Item, TypeEnv, VarEnv, Seen) };
parse_type({type, _, tuple, Items}, TypeEnv, VarEnv, Seen) ->
    #{ <<"kind">> => <<"tuple">>, <<"items">> =>
        lists:map(fun(Item) -> parse_type(Item, TypeEnv, VarEnv, Seen) end, Items) };
parse_type({type, _, union, Members}, TypeEnv, VarEnv, Seen) ->
    #{ <<"kind">> => <<"union">>, <<"members">> =>
        lists:map(fun(Member) -> parse_type(Member, TypeEnv, VarEnv, Seen) end, Members) };
parse_type({type, _, range, [Min, Max]}, TypeEnv, VarEnv, Seen) ->
    #{
        <<"kind">> => <<"range">>,
        <<"min">> => literal_value(parse_type(Min, TypeEnv, VarEnv, Seen)),
        <<"max">> => literal_value(parse_type(Max, TypeEnv, VarEnv, Seen))
    };
parse_type({type, _, integer, []}, _TypeEnv, _VarEnv, _Seen) -> scalar_type(<<"integer">>);
parse_type({type, _, non_neg_integer, []}, _TypeEnv, _VarEnv, _Seen) -> scalar_type(<<"non-neg-integer">>);
parse_type({type, _, pos_integer, []}, _TypeEnv, _VarEnv, _Seen) -> scalar_type(<<"pos-integer">>);
parse_type({type, _, neg_integer, []}, _TypeEnv, _VarEnv, _Seen) -> scalar_type(<<"neg-integer">>);
parse_type({type, _, float, []}, _TypeEnv, _VarEnv, _Seen) -> scalar_type(<<"float">>);
parse_type({type, _, number, []}, _TypeEnv, _VarEnv, _Seen) -> scalar_type(<<"number">>);
parse_type({type, _, binary, _}, _TypeEnv, _VarEnv, _Seen) -> scalar_type(<<"binary">>);
parse_type({type, _, bitstring, _}, _TypeEnv, _VarEnv, _Seen) -> scalar_type(<<"bitstring">>);
parse_type({type, _, boolean, []}, _TypeEnv, _VarEnv, _Seen) -> boolean_type();
parse_type({type, _, atom, []}, _TypeEnv, _VarEnv, _Seen) -> scalar_type(<<"atom">>);
parse_type({type, _, pid, []}, _TypeEnv, _VarEnv, _Seen) -> scalar_type(<<"pid">>);
parse_type({type, _, any, []}, _TypeEnv, _VarEnv, _Seen) -> any_type();
parse_type({atom, _, Atom}, _TypeEnv, _VarEnv, _Seen) -> literal_type(hb_util:bin(Atom));
parse_type({integer, _, Int}, _TypeEnv, _VarEnv, _Seen) -> literal_type(Int);
parse_type({char, _, Char}, _TypeEnv, _VarEnv, _Seen) -> literal_type(<<Char/utf8>>);
parse_type({string, _, String}, _TypeEnv, _VarEnv, _Seen) -> literal_type(hb_util:bin(String));
parse_type({nil, _}, _TypeEnv, _VarEnv, _Seen) -> literal_type([]);
parse_type(Other, _TypeEnv, _VarEnv, _Seen) ->
    unknown_type(Other).

field_presence(map_field_exact) -> required;
field_presence(map_field_assoc) -> optional;
field_presence(Other) -> normalize_name(Other).

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

apply_schema(#{ <<"kind">> := <<"any">> }, Value, _Opts) ->
    Value;
apply_schema(#{ <<"kind">> := <<"wildcard">> }, Value, _Opts) ->
    Value;
apply_schema(#{ <<"kind">> := Kind }, Value, _Opts)
        when Kind =:= <<"remote">>;
             Kind =:= <<"alias">>;
             Kind =:= <<"variable">>;
             Kind =:= <<"unknown">> ->
    Value;
apply_schema(Schema, Link, Opts) when ?IS_LINK(Link) ->
    apply_schema(Schema, hb_cache:ensure_loaded(Link, Opts), Opts);
apply_schema(Schema = #{ <<"kind">> := <<"message">> }, Value, Opts)
        when not is_map(Value) ->
    apply_coerced_schema(Schema, Value, Opts);
apply_schema(
    #{ <<"kind">> := <<"message">>, <<"keys">> := Keys, <<"wildcard">> := Wildcard },
    Message,
    Opts
) when is_map(Message) ->
    Explicit =
        maps:fold(
            fun(Key, #{ <<"presence">> := Presence, <<"type">> := Type }, Acc) ->
                case maps:find(Key, Message) of
                    {ok, Value} ->
                        Acc#{ Key => apply_schema(Type, Value, Opts) };
                    error when Presence =:= required ->
                        throw({required_key_missing, Key});
                    error ->
                        Acc
                end
            end,
            #{},
            Keys
        ),
    case Wildcard of
        none ->
            Explicit;
        #{ <<"presence">> := optional } ->
            maps:merge(
                maps:without(maps:keys(Keys), Message),
                Explicit
            );
        #{ <<"type">> := Type } ->
            Rest =
                maps:map(
                    fun(_, Value) -> apply_schema(Type, Value, Opts) end,
                    maps:without(maps:keys(Keys), Message)
                ),
            maps:merge(Rest, Explicit)
    end;
apply_schema(
    Schema = #{ <<"kind">> := <<"list">>, <<"item">> := ItemType },
    Value,
    Opts
) ->
    case try_coerce(fun hb_util:list/1, Value) of
        List when is_list(List) ->
            [apply_schema(ItemType, Item, Opts) || Item <- List];
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
            list_to_tuple(
                [
                    apply_schema(Type, Item, Opts)
                ||
                    {Type, Item} <- lists:zip(Items, Values)
                ]
            );
        false ->
            throw({invalid_type, Schema, Value})
    end;
apply_schema(
    Schema = #{ <<"kind">> := <<"union">>, <<"members">> := Members },
    Value,
    Opts
) ->
    case apply_union(Members, Value, Opts) of
        {ok, Varied} -> Varied;
        error -> throw({invalid_type, Schema, Value})
    end;
apply_schema(Type, Value, Opts) ->
    case check_type(Type, Value) of
        true ->
            Value;
        false ->
            case coerce_type(Type, Value, Opts) of
                error -> throw({invalid_type, Type, Value});
                Coerced ->
                    case check_type(Type, Coerced) of
                        true -> Coerced;
                        false -> throw({invalid_type, Type, Value})
                    end
            end
    end.

apply_coerced_schema(Schema, Value, Opts) ->
    case coerce_type(Schema, Value, Opts) of
        error -> throw({invalid_type, Schema, Value});
        Value -> throw({invalid_type, Schema, Value});
        Coerced -> apply_schema(Schema, Coerced, Opts)
    end.

apply_union([], _Value, _Opts) ->
    error;
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

matching_union_member([], _Value) ->
    error;
matching_union_member(
    [#{ <<"kind">> := Kind } | Rest],
    Value
) when Kind =:= <<"any">>;
       Kind =:= <<"wildcard">>;
       Kind =:= <<"remote">>;
       Kind =:= <<"alias">>;
       Kind =:= <<"variable">>;
       Kind =:= <<"unknown">> ->
    matching_union_member(Rest, Value);
matching_union_member([Member | Rest], Value) ->
    case check_type(Member, Value) of
        true -> {ok, Member};
        false -> matching_union_member(Rest, Value)
    end.

apply_coerced_union([], _Value, _Opts) ->
    error;
apply_coerced_union(Members, Value, Opts) ->
    {PassThrough, Constrained} =
        lists:partition(fun is_passthrough_schema/1, Members),
    apply_coerced_union_ordered(Constrained ++ PassThrough, Value, Opts).

apply_coerced_union_ordered([], _Value, _Opts) ->
    error;
apply_coerced_union_ordered([Member | Rest], Value, Opts) ->
    try apply_schema(Member, Value, Opts) of
        Varied -> {ok, Varied}
    catch
        throw:{invalid_type, _, _} ->
            apply_coerced_union_ordered(Rest, Value, Opts);
        throw:{required_key_missing, _} ->
            apply_coerced_union_ordered(Rest, Value, Opts)
    end.

is_passthrough_schema(#{ <<"kind">> := Kind }) ->
    lists:member(
        Kind,
        [<<"any">>, <<"wildcard">>, <<"remote">>, <<"alias">>,
            <<"variable">>, <<"unknown">>]
    );
is_passthrough_schema(_) ->
    false.

coerce_type(_, undefined, _Opts) -> error;
coerce_type(#{ <<"kind">> := <<"integer">> }, Value, _Opts) ->
    try_coerce(fun hb_util:int/1, Value);
coerce_type(#{ <<"kind">> := <<"non-neg-integer">> }, Value, _Opts) ->
    try_coerce(fun hb_util:int/1, Value);
coerce_type(#{ <<"kind">> := <<"pos-integer">> }, Value, _Opts) ->
    try_coerce(fun hb_util:int/1, Value);
coerce_type(#{ <<"kind">> := <<"neg-integer">> }, Value, _Opts) ->
    try_coerce(fun hb_util:int/1, Value);
coerce_type(#{ <<"kind">> := <<"float">> }, Value, _Opts) ->
    try_coerce(fun hb_util:float/1, Value);
coerce_type(#{ <<"kind">> := <<"number">> }, Value, _Opts) ->
    coerce_with([fun hb_util:int/1, fun hb_util:float/1], Value);
coerce_type(#{ <<"kind">> := <<"binary">> }, Value, _Opts) ->
    try_coerce(fun hb_util:bin/1, Value);
coerce_type(#{ <<"kind">> := <<"bitstring">> }, Value, _Opts) ->
    try_coerce(fun hb_util:bin/1, Value);
coerce_type(#{ <<"kind">> := <<"boolean">> }, Value, _Opts) ->
    case is_boolean_coercible(Value) of
        true -> try_coerce(fun hb_util:bool/1, Value);
        false -> error
    end;
coerce_type(#{ <<"kind">> := <<"atom">> }, Value, _Opts) ->
    try_coerce(fun hb_util:atom/1, Value);
coerce_type(#{ <<"kind">> := <<"message">> }, Value, _Opts) ->
    try_coerce(fun hb_util:map/1, Value);
coerce_type(#{ <<"kind">> := <<"tuple">>, <<"items">> := Items }, Value, Opts)
        when is_tuple(Value) ->
    coerce_type(#{ <<"kind">> => <<"tuple">>, <<"items">> => Items }, tuple_to_list(Value), Opts);
coerce_type(#{ <<"kind">> := <<"tuple">>, <<"items">> := Items }, Value, Opts)
        when is_list(Value) ->
    case length(Value) =:= length(Items) of
        false -> error;
        true ->
            case coerce_sequence(lists:zip(Items, Value), Opts) of
                error -> error;
                Coerced -> list_to_tuple(Coerced)
            end
    end;
coerce_type(#{ <<"kind">> := <<"list">>, <<"item">> := ItemType }, Value, Opts) ->
    case try_coerce(fun hb_util:list/1, Value) of
        error -> error;
        Coerced -> coerce_list(ItemType, Coerced, Opts)
    end;
coerce_type(#{ <<"kind">> := <<"union">>, <<"members">> := Members }, Value, Opts) ->
    coerce_union(Members, Value, Opts);
coerce_type(#{ <<"kind">> := <<"literal">>, <<"value">> := Expected }, Value, _Opts) ->
    coerce_literal(Expected, Value);
coerce_type(#{ <<"kind">> := <<"range">> }, Value, _Opts) ->
    try_coerce(fun hb_util:int/1, Value);
coerce_type(_, _, _Opts) ->
    error.

try_coerce(Fun, Value) ->
    try Fun(Value) of
        Coerced -> Coerced
    catch
        _:_ -> error
    end.

coerce_with([], _Value) ->
    error;
coerce_with([Fun | Rest], Value) ->
    case try_coerce(Fun, Value) of
        error -> coerce_with(Rest, Value);
        Coerced -> Coerced
    end.

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

coerce_list(ItemType, Value, Opts) when is_list(Value) ->
    coerce_sequence([{ItemType, Item} || Item <- Value], Opts);
coerce_list(_ItemType, _Value, _Opts) ->
    error.

coerce_union([], _Value, _Opts) ->
    error;
coerce_union([Member | Rest], Value, Opts) ->
    case coerce_type(Member, Value, Opts) of
        error -> coerce_union(Rest, Value, Opts);
        Coerced -> Coerced
    end.

coerce_literal(Expected, Value) when is_integer(Expected) ->
    coerce_exact(Expected, try_coerce(fun hb_util:int/1, Value));
coerce_literal(Expected, Value) when is_float(Expected) ->
    coerce_exact(Expected, try_coerce(fun hb_util:float/1, Value));
coerce_literal(Expected, Value) when is_binary(Expected) ->
    coerce_exact(Expected, try_coerce(fun hb_util:bin/1, Value));
coerce_literal(Expected, Value) when is_atom(Expected) ->
    case is_boolean(Expected) andalso is_boolean_coercible(Value) of
        true -> coerce_exact(Expected, try_coerce(fun hb_util:bool/1, Value));
        false -> coerce_exact(Expected, try_coerce(fun hb_util:atom/1, Value))
    end;
coerce_literal(Expected, Value) when is_list(Expected) ->
    case try_coerce(fun hb_util:list/1, Value) of
        error -> error;
        Coerced -> coerce_exact(Expected, Coerced)
    end;
coerce_literal(Expected, Value) when Value =:= Expected ->
    Value;
coerce_literal(_Expected, _Value) ->
    error.

coerce_exact(Expected, Expected) ->
    Expected;
coerce_exact(_Expected, _Value) ->
    error.

is_boolean_coercible(Value) ->
    lists:member(Value, [true, false, 1, 0, <<"true">>, <<"false">>, <<"1">>, <<"0">>]).

check_type(#{ <<"kind">> := <<"integer">> }, Value) -> is_integer(Value);
check_type(#{ <<"kind">> := <<"non-neg-integer">> }, Value) -> is_integer(Value) andalso Value >= 0;
check_type(#{ <<"kind">> := <<"pos-integer">> }, Value) -> is_integer(Value) andalso Value > 0;
check_type(#{ <<"kind">> := <<"neg-integer">> }, Value) -> is_integer(Value) andalso Value < 0;
check_type(#{ <<"kind">> := <<"float">> }, Value) -> is_float(Value);
check_type(#{ <<"kind">> := <<"number">> }, Value) -> is_number(Value);
check_type(#{ <<"kind">> := <<"binary">> }, Value) -> is_binary(Value);
check_type(#{ <<"kind">> := <<"bitstring">> }, Value) -> is_bitstring(Value);
check_type(#{ <<"kind">> := <<"boolean">> }, Value) -> is_boolean(Value);
check_type(#{ <<"kind">> := <<"atom">> }, Value) -> is_atom(Value);
check_type(#{ <<"kind">> := <<"pid">> }, Value) -> is_pid(Value);
check_type(#{ <<"kind">> := <<"message">>, <<"keys">> := Keys }, Value)
        when is_map(Value) ->
    maps:fold(
        fun
            (Key, #{ <<"presence">> := required }, true) -> maps:is_key(Key, Value);
            (_, _, Acc) -> Acc
        end,
        true,
        Keys
    );
check_type(#{ <<"kind">> := <<"message">> }, Value) -> is_map(Value);
check_type(#{ <<"kind">> := <<"tuple">>, <<"items">> := Items }, Value) ->
    is_tuple(Value)
        andalso tuple_size(Value) =:= length(Items)
        andalso lists:all(
            fun({Index, ItemType}) -> check_type(ItemType, element(Index, Value)) end,
            lists:zip(lists:seq(1, length(Items)), Items)
        );
check_type(#{ <<"kind">> := <<"list">>, <<"item">> := ItemType }, Value) ->
    is_list(Value) andalso lists:all(fun(Item) -> check_type(ItemType, Item) end, Value);
check_type(#{ <<"kind">> := <<"union">>, <<"members">> := Members }, Value) ->
    lists:any(fun(Member) -> check_type(Member, Value) end, Members);
check_type(#{ <<"kind">> := <<"literal">>, <<"value">> := Expected }, Value) ->
    Value =:= Expected;
check_type(#{ <<"kind">> := <<"range">>, <<"min">> := Min, <<"max">> := Max }, Value) ->
    is_integer(Value) andalso Value >= Min andalso Value =< Max;
check_type(#{ <<"kind">> := <<"remote">> }, _Value) -> true;
check_type(#{ <<"kind">> := <<"alias">> }, _Value) -> true;
check_type(#{ <<"kind">> := <<"variable">> }, _Value) -> true;
check_type(#{ <<"kind">> := <<"unknown">> }, _Value) -> true;
check_type(_, _) -> true.

normalize_name('_') -> <<"_">>;
normalize_name(Name) when is_atom(Name) -> hb_util:atom_to_dashed_binary(Name);
normalize_name(Name) -> hb_util:bin(Name).

literal_value(#{ <<"kind">> := <<"literal">>, <<"value">> := Value }) -> Value;
literal_value(_) -> undefined.

var_name({var, _, Name}) -> Name;
var_name(Name) -> Name.

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
unknown_type(Other) ->
    #{ <<"kind">> => <<"unknown">>, <<"ast">> => hb_util:bin(io_lib:format("~tp", [Other])) }.
boolean_type() ->
    #{
        <<"kind">> => <<"union">>,
        <<"members">> => [literal_type(true), literal_type(false)]
    }.

%%% Tests

parse_empty_projection_test() ->
    ?assertEqual(
        #{ <<"kind">> => <<"wildcard">> },
        parse_type({var, 1, '_'}, #{}, #{}, [])
    ).

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
    Schema = implicit_base(wildcard_type()),
    ?assertEqual(
        #{ <<"device">> => <<"test@1.0">> },
        apply_schema(
            Schema,
            #{ <<"device">> => <<"test@1.0">>, <<"extra">> => <<"drop">> },
            #{}
        )
    ).

selected_links_are_materialized_without_loading_omitted_keys_test() ->
    Store = hb_test_utils:test_store(),
    Opts = #{ <<"store">> => Store },
    hb_store:reset(Store),
    {ok, SlotPath} = hb_cache:write(<<"7">>, Opts),
    Missing = {link, <<"data/not-present">>, #{}},
    DeepSchema =
        message_type(
            #{
                <<"slot">> =>
                    #{ <<"presence">> => required, <<"type">> => scalar_type(<<"integer">>) }
            },
            none
        ),
    Schema =
        message_type(
            #{
                <<"deep">> =>
                    #{ <<"presence">> => required, <<"type">> => DeepSchema }
            },
            none
        ),
    ?assertEqual(
        #{ <<"deep">> => #{ <<"slot">> => 7 } },
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
        )
    ).

explicit_wildcard_preserves_lazy_links_test() ->
    Missing = {link, <<"data/not-present">>, #{}},
    Schema =
        message_type(
            #{
                <<"scheduler">> =>
                    #{
                        <<"presence">> => optional,
                        <<"type">> => wildcard_type()
                    }
            },
            none
        ),
    ?assertEqual(
        #{ <<"scheduler">> => Missing },
        apply_schema(Schema, #{ <<"scheduler">> => Missing }, #{})
    ).

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
    ?assertEqual(
        #{ <<"extra">> => Link },
        apply_schema(
            WildcardSchema,
            #{ <<"extra">> => Link },
            Opts
        )
    ),
    Integer = scalar_type(<<"integer">>),
    ?assertEqual(
        [8],
        apply_schema(#{ <<"kind">> => <<"list">>, <<"item">> => Integer }, [Link], Opts)
    ),
    ?assertEqual(
        {8},
        apply_schema(#{ <<"kind">> => <<"tuple">>, <<"items">> => [Integer] }, {Link}, Opts)
    ).

union_preserves_an_existing_member_type_test() ->
    Binary = scalar_type(<<"binary">>),
    List = #{ <<"kind">> => <<"list">>, <<"item">> => Binary },
    Value = [<<"one">>, <<"two">>],
    ?assertEqual(
        Value,
        apply_schema(
            #{ <<"kind">> => <<"union">>, <<"members">> => [Binary, List] },
            Value,
            #{}
        )
    ).

union_passthrough_members_do_not_swallow_constrained_members_test() ->
    Store = hb_test_utils:test_store(),
    Opts = #{ <<"store">> => Store },
    hb_store:reset(Store),
    {ok, SlotPath} = hb_cache:write(<<"9">>, Opts),
    Integer = scalar_type(<<"integer">>),
    Message =
        message_type(
            #{
                <<"slot">> =>
                    #{ <<"presence">> => required, <<"type">> => Integer }
            },
            none
        ),
    ?assertEqual(
        #{ <<"slot">> => 9 },
        apply_schema(
            #{
                <<"kind">> => <<"union">>,
                <<"members">> => [unknown_type({record, tx}), Message]
            },
            #{ <<"slot">> => {link, SlotPath, #{}} },
            Opts
        )
    ),
    Binary = scalar_type(<<"binary">>),
    ?assertEqual(
        <<"value">>,
        apply_schema(
            #{
                <<"kind">> => <<"union">>,
                <<"members">> => [any_type(), wildcard_type(), Binary]
            },
            <<"value">>,
            #{}
        )
    ),
    FirstMessage =
        message_type(
            #{
                <<"first">> =>
                    #{ <<"presence">> => required, <<"type">> => Integer }
            },
            none
        ),
    SecondMessage =
        message_type(
            #{
                <<"second">> =>
                    #{ <<"presence">> => required, <<"type">> => Integer }
            },
            none
        ),
    ?assertEqual(
        #{ <<"second">> => 2 },
        apply_schema(
            #{
                <<"kind">> => <<"union">>,
                <<"members">> => [FirstMessage, SecondMessage]
            },
            #{ <<"second">> => 2 },
            #{}
        )
    ),
    IntMessage =
        message_type(
            #{
                <<"value">> =>
                    #{ <<"presence">> => required, <<"type">> => Integer }
            },
            none
        ),
    BinaryMessage =
        message_type(
            #{
                <<"value">> =>
                    #{ <<"presence">> => required, <<"type">> => Binary }
            },
            none
        ),
    ?assertEqual(
        #{ <<"value">> => <<"text">> },
        apply_schema(
            #{
                <<"kind">> => <<"union">>,
                <<"members">> => [IntMessage, BinaryMessage]
            },
            #{ <<"value">> => <<"text">> },
            #{}
        )
    ).
