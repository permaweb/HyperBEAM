%%% @doc Extract Dialyzer-style type information from AO-Core devices and apply
%%% a static `vary` transform to base and request messages.
%%%
%%% Vary specs use `_' or `map()' for pass-through, `#{}' for no explicit
%%% keys, and `#{ _ => _ }' to pass through all keys while still applying
%%% explicitly declared key schemas.
-module(hb_types).
-export([extract/2, vary/5, vary/7, preserves_message_extension/6]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(EXTRACT_CACHE_TAG, {hb_types, extract, 2}).
-define(EXTRACT_CACHE_MISS, '$hb_types_extract_cache_miss').

%% @doc Apply a device's declared base/request schemas to the messages that will
%% participate in one AO-Core key execution. If no schema is provided, we return
%% the messages unchanged.
vary(Device, Key, Base, Request, Opts) ->
    case extract(Device, Opts) of
        {ok, #{ <<"keys">> := KeySchemas }} ->
            case maps:get(normalize_name(Key), KeySchemas, undefined) of
                undefined ->
                    {ok, Base, Request};
                Schema ->
                    ?event({apply_schema, {schema, Schema}, {base, Base}, {request, Request}}),
                    {ok,
                        apply_schema(
                            maps:get(<<"base">>, Schema, any_type()),
                            Base,
                            Opts
                        ),
                        apply_schema(
                            maps:get(<<"request">>, Schema, any_type()),
                            Request,
                            Opts
                        )
                    }
            end;
        {error, _Reason} ->
            {ok, Base, Request}
    end.

%% @doc Apply the schema for a resolved device function. This is the AO-Core
%% entrypoint: the resolver has already mapped a key to its Erlang function.
vary(Device, Key, Func, AddKey, Base, Request, Opts) ->
    case function_schema(Device, Func, Key, Opts) of
        undefined ->
            {ok, Base, Request, none};
        Schema ->
            {BaseSchema, ReqSchema, ReturnSchema} =
                execution_schemas(Schema, AddKey),
            Req =
                case AddKey of
                    false -> Request;
                    _ -> Request#{ <<"path">> => Key }
                end,
            {ok,
                apply_schema(implicit_base(BaseSchema), Base, Opts),
                apply_schema(implicit_request(ReqSchema), Req, Opts),
                overlay(ReturnSchema)
            }
    end.

%% @doc Return whether the resolved function's schema explicitly asks to
%% receive a message extension edge on the selected input side.
preserves_message_extension(Device, Key, Func, AddKey, Side, Opts) ->
    case function_schema(Device, Func, Key, Opts) of
        undefined ->
            false;
        Schema ->
            {BaseSchema, ReqSchema, _ReturnSchema} =
                execution_schemas(Schema, AddKey),
            SideSchema =
                case Side of
                    base -> implicit_base(BaseSchema);
                    request -> implicit_request(ReqSchema)
                end,
            schema_preserves_message_extension(SideSchema)
    end.

schema_preserves_message_extension(
    #{ <<"kind">> := <<"message">>, <<"all">> := true }
) ->
    true;
schema_preserves_message_extension(
    #{ <<"kind">> := <<"message">>, <<"keys">> := Keys }
) ->
    maps:is_key(<<"...">>, Keys)
        orelse maps:is_key(<<"...+link">>, Keys);
schema_preserves_message_extension(_Schema) ->
    false.

%% @doc Extract the public type schema for a device.
extract(Device, _Opts) when is_map(Device) ->
    {error, {unsupported_device_type, Device}};
extract(Module, Opts) when is_atom(Module) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            cached_extract(Module, Opts);
        {error, Reason} ->
            {error, {module_not_loaded, Module, Reason}}
    end;
extract(Device, Opts) when is_binary(Device) ->
    case hb_device_load:reference(Device, Opts) of
        {ok, Module} -> extract(Module, Opts);
        Error -> Error
    end;
extract(Device, _Opts) ->
    {error, {unsupported_device_type, Device}}.

cached_extract(Module, Opts) ->
    CacheKey = {?EXTRACT_CACHE_TAG, Module, Module:module_info(md5)},
    case persistent_term:get(CacheKey, ?EXTRACT_CACHE_MISS) of
        ?EXTRACT_CACHE_MISS ->
            Path = extract_cache_path(Module),
            Res =
                case read_cached_extract(Path, Opts) of
                    {ok, Cached} -> Cached;
                    miss ->
                        Extracted = do_extract(Module),
                        write_cached_extract(Path, Extracted, Opts),
                        Extracted
                end,
            persistent_term:put(CacheKey, Res),
            Res;
        Res ->
            Res
    end.

extract_cache_path(Module) ->
    ModuleBin = atom_to_binary(Module, utf8),
    BeamHash = module_beam_hash(Module),
    hb_path:to_binary([<<"ao-core">>, <<"device-", ModuleBin/binary>>, BeamHash]).

module_beam_hash(Module) ->
    case hb_device_archive:object_code(Module) of
        undefined ->
            case code:get_object_code(Module) of
                {Module, Beam, _Filename} ->
                    hb_util:encode(hb_crypto:sha256(Beam));
                _ ->
                    hb_util:encode(Module:module_info(md5))
            end;
        Beam ->
            hb_util:encode(hb_crypto:sha256(Beam))
    end.

read_cached_extract(Path, Opts) ->
    try hb_store:read(Path, hb_store:scope(Opts, local)) of
        {ok, Bin} ->
            case binary_to_term(Bin, [safe]) of
                {?EXTRACT_CACHE_TAG, Res} -> {ok, Res};
                _ -> miss
            end;
        _ ->
            miss
    catch _:_ ->
        miss
    end.

write_cached_extract(Path, Res = {ok, _}, Opts) ->
    try hb_store:write(#{ Path => term_to_binary({?EXTRACT_CACHE_TAG, Res}) },
            hb_store:scope(Opts, local)) of
        _ -> ok
    catch _:_ -> ok
    end;
write_cached_extract(_Path, _Res, _Opts) ->
    ok.

do_extract(Module) ->
    case beam_lib:chunks(module_beam(Module), [abstract_code]) of
        {ok, {_, [{abstract_code, {_, Forms}}]}} ->
            TypeEnv = build_type_env(Forms),
            Specs = [ Attr || Attr = {attribute, _, spec, _} <- Forms ],
            KeySchemas =
                lists:foldl(
                    fun(Spec, Acc) ->
                        case spec_to_schema(Spec, TypeEnv) of
                            false -> Acc;
                            {Key, Schema} -> store_schema(Key, Schema, Acc)
                        end
                    end,
                    #{},
                    Specs
                ),
            {ok,
                #{
                    <<"module">> => hb_util:bin(atom_to_binary(Module, utf8)),
                    <<"keys">> => KeySchemas,
                    <<"types">> => export_type_env(TypeEnv)
                }
            };
        Error ->
            {error, {abstract_code_unavailable, Module, Error}}
    end.

module_beam(Module) ->
    case hb_device_archive:object_code(Module) of
        undefined ->
            case code:get_object_code(Module) of
                {Module, Binary, _Filename} -> Binary;
                _ -> code:which(Module)
            end;
        Binary ->
            Binary
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

export_type_env(TypeEnv) ->
    maps:from_list(
        lists:map(
            fun({Name, #{ ast := Ast, vars := Vars }}) ->
                {
                    normalize_name(Name),
                    #{
                        <<"kind">> => <<"alias">>,
                        <<"name">> => normalize_name(Name),
                        <<"vars">> => [normalize_name(Var) || Var <- Vars],
                        <<"type">> => parse_type(Ast, TypeEnv, #{}, [Name])
                    }
                }
            end,
            maps:to_list(TypeEnv)
        )
    ).

spec_to_schema({attribute, _, spec, {{Name, Arity}, [Spec]}}, TypeEnv) ->
    {Args, Return} = parse_fun_spec(Spec, TypeEnv),
    {
        normalize_name(Name),
        #{
            <<"arity">> => Arity,
            <<"args">> => Args,
            <<"base">> => maybe_nth(1, Args, any_type()),
            <<"request">> => maybe_nth(2, Args, any_type()),
            <<"opts">> => maybe_nth(3, Args, any_type()),
            <<"return">> => Return
        }
    };
spec_to_schema(_, _) ->
    false.

maybe_nth(N, List, Default) ->
    case catch lists:nth(N, List) of
        {'EXIT', _} -> Default;
        Value -> Value
    end.

store_schema(Key, Schema, Acc) ->
    case maps:get(Key, Acc, undefined) of
        undefined ->
            Acc#{ Key => Schema };
        Existing ->
            ExistingArity = maps:get(<<"arity">>, Existing),
            SchemaArity = maps:get(<<"arity">>, Schema),
            Overloads0 =
                maps:get(
                    <<"overloads">>,
                    Existing,
                    #{ ExistingArity => maps:without([<<"overloads">>], Existing) }
                ),
            Acc#{
                Key =>
                    Schema#{
                        <<"overloads">> =>
                            Overloads0#{
                                SchemaArity => maps:without([<<"overloads">>], Schema)
                            }
                    }
            }
    end.

function_schema(Device, Func, Key, Opts) ->
    case extract(Device, Opts) of
        {ok, #{ <<"keys">> := KeySchemas }} ->
            case function_schema(Func, Key, KeySchemas) of
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
                {ok, #{ <<"keys">> := KeySchemas }} ->
                    function_schema(Func, Key, KeySchemas);
                {error, _Reason} ->
                    undefined
            end;
        _ ->
            undefined
    end.

function_schema(Func, Key, KeySchemas) ->
    {arity, Arity} = erlang:fun_info(Func, arity),
    FuncSchema =
        case erlang:fun_info(Func, name) of
            {name, Name} -> named_schema(Name, Arity, KeySchemas);
            _ -> undefined
        end,
    case FuncSchema of
        undefined -> named_schema(Key, Arity, KeySchemas);
        Schema -> Schema
    end.

named_schema(Name, Arity, KeySchemas) ->
    case maps:get(normalize_name(Name), KeySchemas, undefined) of
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

implicit_base(Schema) ->
    implicit_key(Schema, <<"device">>, optional).

implicit_request(Schema) ->
    implicit_key(Schema, <<"path">>, required).

implicit_key(Schema = #{ <<"kind">> := <<"message">>, <<"keys">> := Keys }, Key, Presence) ->
    case maps:is_key(Key, Keys) of
        true -> Schema;
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

overlay_type(#{ <<"kind">> := <<"message">> } = Schema) ->
    Wildcard =
        case maps:get(<<"wildcard">>, Schema, undefined) of
            #{ <<"type">> := WildcardType } -> overlay_marker(WildcardType);
            _ -> none
        end,
    case Wildcard of
        none ->
            overlay_marker(
                maps:get(
                    <<"type">>,
                    maps:get(<<"...">>, maps:get(<<"keys">>, Schema, #{}), #{}),
                    #{}
                )
            );
        Overlay -> Overlay
    end;
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
parse_fun_spec({type, _, 'fun', [{type, _, product, Args}, Ret]}, TypeEnv) ->
    {
        lists:map(fun(Arg) -> parse_type(Arg, TypeEnv, #{}, []) end, Args),
        parse_type(Ret, TypeEnv, #{}, [])
    };
parse_fun_spec(Other, _TypeEnv) ->
    {[unknown_type(Other)], any_type()}.

parse_type({ann_type, _, [_Var, Type]}, TypeEnv, VarEnv, Seen) ->
    parse_type(Type, TypeEnv, VarEnv, Seen);
parse_type({var, _, '_'}, _TypeEnv, _VarEnv, _Seen) ->
    any_type();
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
                    BoundEnv =
                        maps:merge(
                            VarEnv,
                            maps:from_list(lists:zip(Vars, Args))
                        ),
                    parse_type(Ast, TypeEnv, BoundEnv, [Name | Seen])
            end
    end;
parse_type({remote_type, _, [{atom, _, Mod}, {atom, _, Name}, Args]}, TypeEnv, VarEnv, Seen) ->
    #{
        <<"kind">> => <<"remote">>,
        <<"module">> => normalize_name(Mod),
        <<"name">> => normalize_name(Name),
        <<"args">> => lists:map(fun(Arg) -> parse_type(Arg, TypeEnv, VarEnv, Seen) end, Args)
    };
parse_type({type, _, map, any}, _TypeEnv, _VarEnv, _Seen) ->
    any_type();
parse_type({type, _, map, Fields}, TypeEnv, VarEnv, Seen) ->
    message_type(
        maps:from_list(
            lists:map(
                fun({type, _, Assoc, [KeyAst, ValueAst]}) ->
                    {
                        key_name(KeyAst, TypeEnv, VarEnv, Seen),
                        #{
                            <<"presence">> => field_presence(Assoc),
                            <<"type">> => parse_type(ValueAst, TypeEnv, VarEnv, Seen)
                        }
                    }
                end,
                Fields
            )
        )
    );
parse_type({type, _, ListType, [Item]}, TypeEnv, VarEnv, Seen)
        when ListType =:= list; ListType =:= nonempty_list ->
    #{
        <<"kind">> => <<"list">>,
        <<"item">> => parse_type(Item, TypeEnv, VarEnv, Seen)
    };
parse_type({type, _, ListType, []}, _TypeEnv, _VarEnv, _Seen)
        when ListType =:= list; ListType =:= nonempty_list ->
    #{
        <<"kind">> => <<"list">>,
        <<"item">> => any_type()
    };
parse_type({type, _, ListType, Item}, TypeEnv, VarEnv, Seen)
        when ListType =:= list; ListType =:= nonempty_list ->
    #{
        <<"kind">> => <<"list">>,
        <<"item">> => parse_type(Item, TypeEnv, VarEnv, Seen)
    };
parse_type({type, _, tuple, Items}, TypeEnv, VarEnv, Seen) ->
    #{
        <<"kind">> => <<"tuple">>,
        <<"items">> => lists:map(fun(Item) -> parse_type(Item, TypeEnv, VarEnv, Seen) end, Items)
    };
parse_type({type, _, union, Members}, TypeEnv, VarEnv, Seen) ->
    #{
        <<"kind">> => <<"union">>,
        <<"members">> =>
            lists:map(
                fun(Member) -> parse_type(Member, TypeEnv, VarEnv, Seen) end,
                Members
            )
    };
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
    ?event({parse_type_other, Other}),
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

apply_schema(#{ <<"kind">> := <<"message">>, <<"keys">> := Keys, <<"all">> := All }, Message, Opts)
        when is_map(Message) ->
    ?event(apply_schema, {message, {keys, Keys}, {all, All}, {message, Message}}),
    % Apply declared keys first so their coerced values take precedence.
    {Explicit, _Changed} =
        lists:foldl(
            fun({Key, #{ <<"presence">> := Presence, <<"type">> := Type }}, {Acc, Changed}) ->
                ?event({apply_schema_find, {key, Key}, {message, Message}, {presence, Presence}, {type, Type}}),
                % If we find the key in the message, apply the schema to the value.
                % If the key is not found and the field is required, throw an error.
                % If the key is not found and the field is optional, skip it.
                case hb_maps:find(Key, Message, Opts) of
                    {ok, Value} ->
                        Applied = apply_schema(Type, Value, Opts),
                        RawValue = maps:get(Key, Message, '$hb_types_missing'),
                        {
                            Acc#{ Key => Applied },
                            Changed orelse Applied =/= RawValue
                        };
                    error when Presence =:= required ->
                        throw({required_key_missing, Key});
                    error ->
                        {Acc, Changed}
                end 
            end,
            {#{}, false},
            maps:to_list(Keys)
        ),
    % If `all` is true, pass all keys through at the current layer. Explicit
    % schemas still validate/coerce their keys and take precedence.
    case All of
        true -> maps:merge(Message, Explicit);
        false ->
            Explicit
    end;
apply_schema(Type, Message, _Opts) ->
    ?event({apply_schema_check_type, {type, Type}, {message, Message}}),
    % If the type matches the message, return the message unchanged.
    % If the type does not match the message, coerce the message to the type.
    case check_type(Type, Message) of
        true -> Message;
        false ->
            case coerce_type(Type, Message) of
                error -> throw({invalid_type, Type, Message});
                Coerced -> Coerced
            end
    end.

%% @doc Coerce a value to a type. If the value is not coercible, return error.
%% Otherwise, return the coerced value.
coerce_type(_, undefined) -> error;
coerce_type(#{ <<"kind">> := <<"any">> }, Value) -> Value;
coerce_type(#{ <<"kind">> := <<"integer">> }, Value) ->
    try_coerce(fun hb_util:int/1, Value);
coerce_type(#{ <<"kind">> := <<"non-neg-integer">> }, Value) ->
    try_coerce(fun hb_util:int/1, Value);
coerce_type(#{ <<"kind">> := <<"pos-integer">> }, Value) ->
    try_coerce(fun hb_util:int/1, Value);
coerce_type(#{ <<"kind">> := <<"neg-integer">> }, Value) ->
    try_coerce(fun hb_util:int/1, Value);
coerce_type(#{ <<"kind">> := <<"float">> }, Value) ->
    try_coerce(fun hb_util:float/1, Value);
coerce_type(#{ <<"kind">> := <<"number">> }, Value) ->
    coerce_with([fun hb_util:int/1, fun hb_util:float/1], Value);
coerce_type(#{ <<"kind">> := <<"binary">> }, Value) ->
    try_coerce(fun hb_util:bin/1, Value);
coerce_type(#{ <<"kind">> := <<"bitstring">> }, Value) ->
    try_coerce(fun hb_util:bin/1, Value);
coerce_type(#{ <<"kind">> := <<"boolean">> }, Value) ->
    case is_boolean_coercible(Value) of
        true -> try_coerce(fun hb_util:bool/1, Value);
        false -> error
    end;
coerce_type(#{ <<"kind">> := <<"atom">> }, Value) ->
    try_coerce(fun hb_util:atom/1, Value);
coerce_type(#{ <<"kind">> := <<"pid">> }, _Value) ->
    error;
coerce_type(#{ <<"kind">> := <<"message">> }, Value) ->
    try_coerce(fun hb_util:map/1, Value);
coerce_type(#{ <<"kind">> := <<"tuple">>, <<"items">> := Items }, Value) when is_tuple(Value) ->
    coerce_type(#{ <<"kind">> => <<"tuple">>, <<"items">> => Items }, tuple_to_list(Value));
coerce_type(#{ <<"kind">> := <<"tuple">>, <<"items">> := Items }, Value) when is_list(Value) ->
    case length(Value) =:= length(Items) of
        false -> error;
        true ->
            case coerce_sequence(lists:zip(Items, Value)) of
                error -> error;
                Coerced -> list_to_tuple(Coerced)
            end
    end;
coerce_type(#{ <<"kind">> := <<"list">>, <<"item">> := ItemType }, Value) ->
    case try_coerce(fun hb_util:list/1, Value) of
        error -> error;
        Coerced -> coerce_list(ItemType, Coerced)
    end;
coerce_type(#{ <<"kind">> := <<"union">>, <<"members">> := Members }, Value) ->
    coerce_union(Members, Value);
coerce_type(#{ <<"kind">> := <<"literal">>, <<"value">> := Expected }, Value) ->
    coerce_literal(Expected, Value);
coerce_type(#{ <<"kind">> := <<"range">> }, Value) ->
    try_coerce(fun hb_util:int/1, Value);
coerce_type(_, _) -> error.

try_coerce(Fun, Value) ->
    try Fun(Value) of
        Coerced -> Coerced
    catch
        _:_ -> error
    end.

%% @doc Coerce a value with a list of functions.
%% This is useful for kind: number, which can be coerced to an integer or a float.
coerce_with([], _Value) ->
    error;
coerce_with([Fun | Rest], Value) ->
    case try_coerce(Fun, Value) of
        error -> coerce_with(Rest, Value);
        Coerced -> Coerced
    end.

%% @doc Coerce a sequence of values to a list of types.
%% This is useful for coercing a list to a tuple.
coerce_sequence([]) ->
    [];
coerce_sequence([{Type, Value} | Rest]) ->
    case coerce_type(Type, Value) of
        error -> error;
        Coerced ->
            case coerce_sequence(Rest) of
                error -> error;
                CoercedRest -> [Coerced | CoercedRest]
            end
    end.

coerce_list(ItemType, Value) when is_list(Value) ->
    coerce_sequence([{ItemType, Item} || Item <- Value]);
coerce_list(_ItemType, _Value) ->
    error.

coerce_union([], _Value) ->
    error;
coerce_union([Member | Rest], Value) ->
    case coerce_type(Member, Value) of
        error -> coerce_union(Rest, Value);
        Coerced -> Coerced
    end.

coerce_literal(Expected, Value) when is_integer(Expected) ->
    try_coerce(fun hb_util:int/1, Value);
coerce_literal(Expected, Value) when is_float(Expected) ->
    try_coerce(fun hb_util:float/1, Value);
coerce_literal(Expected, Value) when is_binary(Expected) ->
    try_coerce(fun hb_util:bin/1, Value);
coerce_literal(Expected, Value) when is_atom(Expected) ->
    case is_boolean(Expected) andalso is_boolean_coercible(Value) of
        true -> try_coerce(fun hb_util:bool/1, Value);
        false -> try_coerce(fun hb_util:atom/1, Value)
    end;
coerce_literal(Expected, Value) when is_list(Expected) ->
    case try_coerce(fun hb_util:list/1, Value) of
        error -> error;
        Coerced when length(Coerced) =:= length(Expected) -> Coerced;
        _ -> error
    end;
coerce_literal(Expected, Value) when is_map(Expected) ->
    try_coerce(fun hb_util:map/1, Value);
coerce_literal(Expected, Value) when Value =:= Expected ->
    Value;
coerce_literal(_Expected, _Value) ->
    error.

is_boolean_coercible(Value) -> 
    Coercible = [true, false, 1, 0, <<"true">>, <<"false">>, <<"1">>, <<"0">>],
    lists:member(Value, Coercible).

check_type(#{ <<"kind">> := <<"any">> }, _Value) -> true;
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
check_type(#{ <<"kind">> := <<"remote">> }, _Value) ->
    true;
check_type(#{ <<"kind">> := <<"alias">> }, _Value) ->
    true;
check_type(#{ <<"kind">> := <<"variable">> }, _Value) ->
    true;
check_type(_, _) ->
    true.

%% @doc Ensure that a name is a `dash-separated-binary` form, rather than
%% an atom, list, etc.
normalize_name('_') -> <<"_">>;
normalize_name(Name) when is_atom(Name) -> hb_util:atom_to_key(Name);
normalize_name(Name) -> hb_util:bin(Name).

%% @doc Extract the value from a literal form.
literal_value(#{ <<"kind">> := <<"literal">>, <<"value">> := Value }) -> Value.

%% @doc Extract the name from a variable form.
var_name({var, _, Name}) -> Name;
var_name(Name) -> Name.

any_type() -> #{ <<"kind">> => <<"any">> }.
scalar_type(Name) -> #{ <<"kind">> => Name }.
literal_type(Value) -> #{ <<"kind">> => <<"literal">>, <<"value">> => Value }.
alias_type(Name) -> #{ <<"kind">> => <<"alias">>, <<"name">> => normalize_name(Name) }.
variable_type(Name) -> #{ <<"kind">> => <<"variable">>, <<"name">> => normalize_name(Name) }.
message_type(AllKeys) ->
    Wildcard = maps:get(<<"_">>, AllKeys, undefined),
    ?event(apply_schema, {message_type, {all_keys, AllKeys}, {wildcard, Wildcard}}),
    % `#{ _ => _ }' passes unmatched keys through at the same layer. `#{}' and
    % other message specs only maintain explicitly declared keys.
    #{
        <<"kind">> => <<"message">>,
        <<"keys">> => maps:without([<<"_">>], AllKeys),
        <<"wildcard">> => Wildcard,
        <<"all">> =>
            case Wildcard of
                #{ <<"type">> := #{ <<"kind">> := <<"any">> } } -> true;
                #{ <<"type">> := #{ <<"value">> := <<"_">> } } -> true;
                _ -> false
            end
    }.
unknown_type(Other) -> #{ <<"kind">> => <<"unknown">>, <<"ast">> => hb_util:bin(io_lib:format("~tp", [Other])) }.
boolean_type() ->
    #{
        <<"kind">> => <<"union">>,
        <<"members">> => [literal_type(true), literal_type(false)]
    }.

%%% Tests

test_opts() ->
    #{
        store => [hb_test_utils:test_store()],
        priv_wallet => hb:wallet()
    }.

extract_test() ->
    Res = extract(<<"test-device@1.0">>, #{}),
    ?event({extraction_result, Res}),
    ?assertMatch(
        {ok, #{ <<"keys">> := #{}, <<"types">> := #{}}},
        Res
    ).

successful_vary_test() ->
    Opts = test_opts(),
    {ok, VariedBase, VariedReq} =
        vary(
            <<"test-device@1.0">>,
            <<"compute">>,
            #{ <<"unused">> => 1 },
            #{ <<"slot">> => 1 },
            Opts
        ),
    ?assertEqual(#{ <<"unused">> => 1 }, VariedBase),
    ?assertEqual(#{ <<"slot">> => 1 }, VariedReq),
    ?event(
        debug_types,
        {vary_result,
            {varied_base, {explicit, VariedBase}},
            {varied_req, {explicit, VariedReq}}
        }
    ).

function_vary_adds_implicit_keys_and_overlay_test() ->
    Opts = test_opts(),
    {ok, VariedBase, VariedReq, Overlay} =
        vary(
            <<"test-device@1.0">>,
            <<"varied">>,
            fun dev_test:varied/3,
            false,
            #{
                <<"device">> => <<"test-device@1.0">>,
                <<"x">> => <<"1">>,
                <<"extra">> => <<"base">>
            },
            #{ <<"path">> => <<"varied">>, <<"extra">> => <<"req">> },
            Opts
        ),
    ?assertEqual(
        #{ <<"device">> => <<"test-device@1.0">>, <<"x">> => 1 },
        VariedBase
    ),
    ?assertEqual(#{ <<"path">> => <<"varied">> }, VariedReq),
    ?assertEqual(base, Overlay).

vary_throw_required_key_missing_test() ->
    Opts = test_opts(),
    ?assertThrow(
        {required_key_missing, _},
        vary(<<"test-device@1.0">>, <<"compute">>, #{}, #{}, Opts)
    ).

vary_required_key_wrong_type_test() ->
    Opts = test_opts(),
    ?assertMatch(
        {
            ok,
            #{},
            #{ <<"slot">> := 1 }
        },
        vary(
            <<"test-device@1.0">>,
            <<"compute">>,
            #{},
            #{ <<"slot">> => <<"1">> },
            Opts
        )
    ).

vary_optional_key_wrong_type_test() ->
    Opts = test_opts(),
    ?assertMatch(
        {
            ok,
            #{ <<"already-seen">> := [1] },
            #{ <<"slot">> := 1 }
        },
        vary(
            <<"test-device@1.0">>,
            <<"compute">>,
            #{ <<"already-seen">> => [<<"1">>] },
            #{ <<"slot">> => <<"1">> },
            Opts
        )
    ).

successful_nested_vary_test() ->
    Opts = test_opts(),
    {ok, VariedBase, VariedReq} =
        vary(
            <<"test-device@1.0">>,
            <<"compute-nested">>,
            #{},
            #{ 
                <<"outer">> => 
                    #{ 
                        <<"slot">> => 1, 
                        <<"unused">> => 
                            #{ <<"unused-key">> => <<"unused-value">> }
                    }
            },
            Opts
        ),
    ?event(debug_types, {vary_result, {varied_base, VariedBase}, {varied_req, VariedReq}}),
    ?assertEqual(#{}, VariedBase),
    ?assertEqual(
        #{ <<"outer">> => #{ <<"slot">> => 1 }},
        VariedReq
    ).

vary_throw_nested_key_missing_test() ->
    Opts = test_opts(),
    ?assertThrow(
        {required_key_missing, _},
        vary(
            <<"test-device@1.0">>,
            <<"compute-nested">>,
            #{},
            #{ <<"outer">> => #{ <<"not-slot">> => 1 }},
            Opts
        )
    ).

vary_nested_key_wrong_type_test() ->
    Opts = test_opts(),
    ?assertMatch(
        {ok, #{}, #{ <<"outer">> := #{ <<"slot">> := 1 }}},
        vary(
            <<"test-device@1.0">>,
            <<"compute-nested">>,
            #{},
            #{ <<"outer">> => #{ <<"slot">> => <<"1">> }},
            Opts
        )
    ).

vary_coerces_required_key_from_binary_test() ->
    Opts = test_opts(),
    {ok, VariedBase, VariedReq} =
        vary(
            <<"test-device@1.0">>,
            <<"compute">>,
            #{},
            #{ <<"slot">> => <<"1">> },
            Opts
        ),
    ?assertEqual(#{}, VariedBase),
    ?assertEqual(#{ <<"slot">> => 1 }, VariedReq).

vary_coerces_required_key_from_list_test() ->
    Opts = test_opts(),
    {ok, VariedBase, VariedReq} =
        vary(
            <<"test-device@1.0">>,
            <<"compute">>,
            #{},
            #{ <<"slot">> => "1" },
            Opts
        ),
    ?assertEqual(#{}, VariedBase),
    ?assertEqual(#{ <<"slot">> => 1 }, VariedReq).

vary_throw_required_key_noncoercible_test() ->
    Opts = test_opts(),
    ?assertThrow(
        {invalid_type, _, _},
        vary(
            <<"test-device@1.0">>,
            <<"compute">>,
            #{},
            #{ <<"slot">> => <<"not-an-int">> },
            Opts
        )
    ).

vary_coerces_optional_base_key_test() ->
    Opts = test_opts(),
    {ok, VariedBase, VariedReq} =
        vary(
            <<"test-device@1.0">>,
            <<"compute">>,
            #{ <<"already-seen">> => [<<"2">>] },
            #{ <<"slot">> => <<"1">> },
            Opts
        ),
    ?assertEqual(#{ <<"already-seen">> => [2] }, VariedBase),
    ?assertEqual(#{ <<"slot">> => 1 }, VariedReq).

vary_throw_optional_base_key_noncoercible_test() ->
    Opts = test_opts(),
    ?assertThrow(
        {invalid_type, _, _},
        vary(
            <<"test-device@1.0">>,
            <<"compute">>,
            #{ <<"already-seen">> => [<<"not-an-int">>] },
            #{ <<"slot">> => 1 },
            Opts
        )
    ).

unschematized_key_returns_messages_unchanged_test() ->
    Opts = test_opts(),
    {ok, VariedBase, VariedReq} =
        vary(
            <<"test-device@1.0">>,
            <<"nonexistent-func">>,
            #{ <<"base">> => <<"value">> },
            #{ 
                <<"outer">> => 
                    #{ 
                        <<"slot">> => 1, 
                        <<"unused">> => 
                            #{ <<"unused-key">> => <<"unused-value">> }
                    }
            },
            Opts
        ),
    ?event(debug_types, {vary_result, {varied_base, VariedBase}, {varied_req, VariedReq}}),
    ?assertEqual(#{ <<"base">> => <<"value">> }, VariedBase),
    ?assertEqual(
        #{
            <<"outer">> =>
                #{
                    <<"slot">> => 1,
                    <<"unused">> =>
                        #{ <<"unused-key">> => <<"unused-value">> }
                }
        },
        VariedReq
    ).

unschematized_key_missing_req_is_unchanged_test() ->
    Opts = test_opts(),
    ?assertEqual(
        {ok, #{}, #{ <<"outer">> => #{ <<"not-slot">> => 1 }}},
        vary(
            <<"test-device@1.0">>,
            <<"nonexistent-func">>,
            #{},
            #{ <<"outer">> => #{ <<"not-slot">> => 1 }},
            Opts
        )
    ).

unschematized_key_wrong_type_is_unchanged_test() ->
    Opts = test_opts(),
    ?assertEqual(
        {ok, #{}, #{ <<"outer">> => #{ <<"slot">> => <<"1">> }}},
        vary(
            <<"test-device@1.0">>,
            <<"nonexistent-func">>,
            #{},
            #{ <<"outer">> => #{ <<"slot">> => <<"1">> }},
            Opts
        )
    ).

vary_on_nothing_passes_messages_through_test() ->
    Opts = test_opts(),
    Base =
        #{
            <<"device">> => <<"test-device@1.0">>,
            <<"a">> => <<"1">>,
            <<"nested">> => #{ <<"b">> => <<"2">> }
        },
    Req =
        #{
            <<"path">> => <<"pass-through">>,
            <<"slot">> => <<"1">>,
            <<"extra">> => #{ <<"c">> => <<"3">> }
        },
    ?assertEqual(
        {ok, Base, Req},
        vary(
            <<"test-device@1.0">>,
            <<"pass-through">>,
            Base,
            Req,
            Opts
        )
    ).

wildcard_message_preserves_extension_test() ->
    ?assertEqual(
        true,
        schema_preserves_message_extension(
            message_type(
                #{
                    <<"_">> =>
                        #{
                            <<"presence">> => optional,
                            <<"type">> => any_type()
                        }
                }
            )
        )
    ).

explicit_extension_preserves_extension_test() ->
    ?assertEqual(
        true,
        schema_preserves_message_extension(
            message_type(
                #{
                    <<"...">> =>
                        #{
                            <<"presence">> => optional,
                            <<"type">> => any_type()
                        }
                }
            )
        )
    ).

vary_on_all_test() -> 
    Opts = test_opts(),
    {ok, VariedBase, VariedReq} =
        vary(
            <<"test-device@1.0">>,
            <<"compute-all">>,
            #{ <<"a">> => <<"1">>, <<"b">> => 2 },
            #{ <<"slot">> => 1 },
            Opts
        ),
    ?event(debug_types, {vary_result, {varied_base, VariedBase}, {varied_req, VariedReq}}),
    ?assertEqual(
        #{
            <<"a">> => 1,
            <<"b">> => 2
        },
        VariedBase
    ),
    ?assertEqual(#{ <<"slot">> => 1 }, VariedReq).

vary_on_all_nested_test() -> 
    Opts = test_opts(),
    {ok, VariedBase, VariedReq} =
        vary(
            <<"test-device@1.0">>,
            <<"compute-all">>,
            #{
                <<"a">> => <<"1">>,
                <<"b">> => 2,
                <<"outer">> => #{
                    <<"c">> => <<"3">>,
                    <<"d">> => <<"4">>
                }
            },
            #{ <<"slot">> => 1 },
            Opts
        ),
    ?event(debug_types, {vary_result, {varied_base, VariedBase}, {varied_req, VariedReq}}),
    ?assertEqual(
        #{
            <<"a">> => 1,
            <<"b">> => 2,
            <<"outer">> => #{ <<"c">> => <<"3">>, <<"d">> => <<"4">> }
        },
        VariedBase
    ),
    ?assertEqual(
        #{ <<"c">> => <<"3">>, <<"d">> => <<"4">> },
        maps:get(<<"outer">>, VariedBase)
    ),
    ?assertEqual(#{ <<"slot">> => 1 }, VariedReq).

vary_on_all_preserves_extra_request_keys_test() ->
    Opts = test_opts(),
    {ok, VariedBase, VariedReq} =
        vary(
            <<"test-device@1.0">>,
            <<"compute-all">>,
            #{ <<"a">> => 1 },
            #{ <<"slot">> => 1, <<"extra">> => <<"x">> },
            Opts
        ),
    ?assertEqual(#{ <<"a">> => 1 }, VariedBase),
    ?assertEqual(
        #{
            <<"slot">> => 1,
            <<"extra">> => <<"x">>
        },
        VariedReq
    ),
    ?assertEqual(<<"x">>, maps:get(<<"extra">>, VariedReq)).

vary_on_all_preserves_nested_request_keys_test() ->
    Opts = test_opts(),
    {ok, _VariedBase, VariedReq} =
        vary(
            <<"test-device@1.0">>,
            <<"compute-all">>,
            #{},
            #{
                <<"slot">> => 1,
                <<"outer">> => #{ <<"c">> => 3, <<"d">> => 4 }
            },
            Opts
        ),
    ?assertEqual(
        #{
            <<"slot">> => 1,
            <<"outer">> => #{ <<"c">> => 3, <<"d">> => 4 }
        },
        VariedReq
    ),
    ?assertEqual(
        #{ <<"c">> => 3, <<"d">> => 4 },
        maps:get(<<"outer">>, VariedReq)
    ).

vary_on_all_removes_schematized_nested_keys_test() ->
    Opts = test_opts(),
    {ok, VariedBase, VariedReq} =
        vary(
            <<"test-device@1.0">>,
            <<"compute-all-nested">>,
            #{
                <<"nested">> => #{ <<"a">> => <<"1">>, <<"b">> => <<"2">> },
                <<"other">> => <<"3">>
            },
            #{
                <<"slot">> => 1,
                <<"nested">> => #{ <<"c">> => 3, <<"d">> => 4 }
            },
            Opts
        ),
    ?assertEqual(
        #{
            <<"nested">> => #{ <<"a">> => 1 },
            <<"other">> => <<"3">>
        },
        VariedBase
    ),
    ?assertEqual(<<"3">>, maps:get(<<"other">>, VariedBase)),
    ?assertEqual(
        #{
            <<"slot">> => 1,
            <<"nested">> => #{ <<"c">> => 3, <<"d">> => 4 }
        },
        VariedReq
    ),
    ?assertEqual(
        #{ <<"c">> => 3, <<"d">> => 4 },
        maps:get(<<"nested">>, VariedReq)
    ).
