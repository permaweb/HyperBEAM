%%% @doc Extract device specs and vary AO-Core execution inputs.
-module(hb_types).
-export([extract/2, vary/2, beam_to_schema/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Derive the resolved function's schema and use it to vary one execution.
%% Projection and dependency collection are one operation: every selected fact
%% is returned together with the hashpath of the membership that supplied it.
vary(
    Ctx =
        #{
            <<"base">> := Base,
            <<"request">> := Req,
            <<"priv">> := #{ <<"function">> := Func }
        },
    Opts
) ->
    case schema_from_function(Func, Opts) of
        undefined ->
            identity_vary(Ctx, Base, Req);
        Schema ->
            [BaseSchema, ReqSchema, ReturnSchema] =
                execution_schema(
                    Schema,
                    maps:get(<<"add-key">>, Ctx, false)
                ),
            {VariedBase, BaseDependencies} =
                project_schema(
                    implicit_base(BaseSchema),
                    Base,
                    undefined,
                    <<"base">>,
                    Opts
                ),
            {VariedReq, ReqDependencies} =
                project_schema(
                    implicit_request(ReqSchema),
                    Req,
                    undefined,
                    <<"request">>,
                    Opts
                ),
            varied_context(
                Ctx,
                VariedBase,
                VariedReq,
                merge_dependencies(BaseDependencies, ReqDependencies),
                overlay(ReturnSchema)
            )
    end;
vary(Ctx = #{ <<"base">> := Base, <<"request">> := Req }, _Opts) ->
    identity_vary(Ctx, Base, Req).

identity_vary(Ctx, Base, Req) ->
    varied_context(Ctx, Base, Req, #{}, none).

varied_context(Ctx, Base, Req, Dependencies, Normalizer) ->
    Varied =
        Ctx#{
            <<"varied-base">> => Base,
            <<"varied-request">> => Req,
            <<"normalizer">> => Normalizer
        },
    case map_size(Dependencies) of
        0 -> {ok, maps:remove(<<"dependencies">>, Varied)};
        _ -> {ok, Varied#{ <<"dependencies">> => Dependencies }}
    end.

%% @doc Extract a device module's function schemas. We first check the
%% `loaded-device-store` cache, then fall back to loading the module manually
%% if it isn't already available.
extract(Device, _Opts) when is_map(Device) ->
    {error, {unsupported_device_type, Device}};
extract(Module, Opts) when is_atom(Module) ->
    % If we are already caching a schema at the moment, skip the recursive cache
    % call and error early.
    case hb_opts:get(<<"caching-schema">>, false, Opts) of
        true ->
            {error, caching_schema};
        false ->
            case hb_device_load:schema(Module, Opts) of
                {ok, Schema} -> {ok, Schema};
                _ ->
                    case code:ensure_loaded(Module) of
                        {module, Module} -> beam_to_schema(Module);
                        {error, Reason} -> {error, {module_not_loaded, Module, Reason}}
                    end
            end
    end;
extract(Device, Opts) when is_binary(Device) ->
    case hb_device_load:reference(Device, Opts) of
        {ok, Module} -> extract(Module, Opts);
        Error -> Error
    end;
extract(Device, _Opts) ->
    {error, {unsupported_device_type, Device}}.

schema_from_function(Func, Opts) ->
    {module, Module} = erlang:fun_info(Func, module),
    case extract(Module, Opts) of
        {ok, #{ <<"keys">> := Schemas }} ->
            select_schema(Func, Schemas);
        {error, _Reason} ->
            undefined
    end.

select_schema(Func, Schemas) ->
    case erlang:fun_info(Func, name) of
        {name, Name} ->
            maps:get(normalize_name(Name), Schemas, undefined);
        _ ->
            undefined
    end.

execution_schema(Schema, AddKey) ->
    Args = maps:get(<<"args">>, Schema, []),
    Offset =
        case AddKey of
            false -> 0;
            _ -> 1
        end,
    [
        nth_or(1 + Offset, Args, any_type()),
        nth_or(2 + Offset, Args, any_type()),
        maps:get(<<"return">>, Schema, any_type())
    ].

nth_or(N, List, _Default) when length(List) >= N -> lists:nth(N, List);
nth_or(_N, _List, Default) -> Default.

implicit_base(Schema) ->
    implicit_key(top_level_schema(Schema), <<"device">>, optional).

implicit_request(Schema) ->
    implicit_key(top_level_schema(Schema), <<"path">>, optional).

top_level_schema(#{ <<"kind">> := <<"empty">> }) ->
    message_type({#{}, none});
top_level_schema(Schema) ->
    Schema.

implicit_key(Schema = #{ <<"kind">> := <<"message">>, <<"keys">> := Keys },
        Key,
        Presence) ->
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

beam_to_schema(Module) ->
    case module_beam(Module) of
        unavailable ->
            {error, {abstract_code_unavailable, Module, unavailable}};
        Beam -> beam_to_schema(Module, Beam)
    end.

beam_to_schema(Module, Beam) ->
    case beam_lib:chunks(Beam, [abstract_code]) of
        {ok, {_, [{abstract_code, {_, Forms}}]}} ->
            TypeEnv = build_type_env(Forms),
            {ok,
                #{
                    <<"keys">> =>
                        lists:foldl(
                            fun
                                ({attribute, _, spec, {{Name, _}, [Head | _]}}, Acc) ->
                                    Acc#{
                                        normalize_name(Name) =>
                                            fun_schema(Head, TypeEnv)
                                    };
                                (_, Acc) ->
                                    Acc
                            end,
                            #{},
                            Forms
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

fun_schema(Spec, TypeEnv) ->
    {Args, Return} = parse_fun_spec(Spec, TypeEnv),
    #{
        <<"args">> => Args,
        <<"return">> => Return
    }.

parse_fun_spec({type, _, bounded_fun, [FunSpec, _Constraints]}, TypeEnv) ->
    parse_fun_spec(FunSpec, TypeEnv);
parse_fun_spec({type, _, 'fun', [{type, _, product, Args}, Return]}, TypeEnv) ->
    {
        [parse_type(Arg, TypeEnv, #{}, []) || Arg <- Args],
        parse_type(Return, TypeEnv, #{}, [])
    };
parse_fun_spec(Other, _TypeEnv) ->
    {[unknown_type(Other)], any_type()}.

parse_type({ann_type, _, [_Var, Type]}, TypeEnv, VarEnv, Seen) ->
    parse_type(Type, TypeEnv, VarEnv, Seen);
parse_type({var, _, '_'}, _TypeEnv, _VarEnv, _Seen) ->
    empty_type();
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
        TypeEnv,
        VarEnv,
        Seen) ->
    #{
        <<"kind">> => <<"remote">>,
        <<"module">> => normalize_name(Mod),
        <<"name">> => normalize_name(Name),
        <<"args">> => [parse_type(Arg, TypeEnv, VarEnv, Seen) || Arg <- Args]
    };
parse_type({type, _, map, any}, _TypeEnv, _VarEnv, _Seen) ->
    any_type();
parse_type({type, _, map, Fields}, TypeEnv, VarEnv, Seen) ->
    message_type(parse_fields(Fields, TypeEnv, VarEnv, Seen));
parse_type({type, _, ListType, [Item]}, TypeEnv, VarEnv, Seen)
        when ListType =:= list; ListType =:= nonempty_list ->
    #{ <<"kind">> => <<"list">>, <<"item">> => parse_type(Item, TypeEnv, VarEnv, Seen) };
parse_type({type, _, ListType, []}, _TypeEnv, _VarEnv, _Seen)
        when ListType =:= list; ListType =:= nonempty_list ->
    #{ <<"kind">> => <<"list">>, <<"item">> => any_type() };
parse_type({type, _, tuple, Items}, TypeEnv, VarEnv, Seen) ->
    #{ <<"kind">> => <<"tuple">>, <<"items">> =>
        [parse_type(Item, TypeEnv, VarEnv, Seen) || Item <- Items] };
parse_type({type, _, union, Members}, TypeEnv, VarEnv, Seen) ->
    #{ <<"kind">> => <<"union">>, <<"members">> =>
        [parse_type(Member, TypeEnv, VarEnv, Seen) || Member <- Members] };
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

parse_fields(Fields, TypeEnv, VarEnv, Seen) ->
    lists:foldl(
        fun({type, _, Assoc, [KeyAst, ValueAst]}, {Keys, Wildcard}) ->
            Field =
                #{
                    <<"presence">> => field_presence(Assoc),
                    <<"type">> => parse_type(ValueAst, TypeEnv, VarEnv, Seen)
                },
            case key_name(KeyAst, TypeEnv, VarEnv, Seen) of
                <<"_">> -> {Keys, Field};
                Key -> {Keys#{ Key => Field }, Wildcard}
            end
        end,
        {#{}, none},
        Fields
    ).

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

%% @doc Apply a schema while collecting the source membership of every value
%% selected from a message. `Origin' is the hashpath of the containing value;
%% it is only present while recursively projecting an already-selected field.
project_schema(#{ <<"kind">> := <<"any">> }, Value, Origin, Location, _Opts) ->
    {Value, dependency(Origin, Location)};
project_schema(#{ <<"kind">> := <<"empty">> }, Value, Origin, Location, _Opts) ->
    {Value, dependency(Origin, Location)};
project_schema(Schema = #{ <<"kind">> := <<"message">> }, Value, Origin, Location, Opts)
        when not is_map(Value) ->
    project_schema(
        Schema,
        hb_cache:ensure_loaded(Value, Opts),
        Origin,
        Location,
        Opts
    );
project_schema(
    #{ <<"kind">> := <<"message">>, <<"keys">> := Keys, <<"wildcard">> := Wildcard },
    Message,
    Origin,
    Location,
    Opts
) when is_map(Message) ->
    {Explicit, ExplicitDependencies} =
        project_explicit(Keys, Message, Origin, Location, Opts),
    {WildcardValues, WildcardDependencies} =
        project_wildcard(Wildcard, Keys, Message, Origin, Location, Opts),
    Dependencies =
        merge_dependencies(ExplicitDependencies, WildcardDependencies),
    {
        maps:merge(WildcardValues, Explicit),
        case {map_size(Dependencies), Origin} of
            {0, undefined} -> #{};
            {0, _} -> dependency(Origin, Location);
            _ -> Dependencies
        end
    };
project_schema(Type, Value, Origin, Location, Opts) ->
    {apply_type(Type, Value, Opts), dependency(Origin, Location)}.

apply_type(Type, Value, Opts) ->
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

project_explicit(Keys, Message, ParentOrigin, ParentLocation, Opts) ->
    maps:fold(
        fun(Key, #{ <<"presence">> := Presence, <<"type">> := Type },
                {Values, Dependencies}) ->
            case read_schema_fact(Message, Key, Presence, ParentOrigin, Opts) of
                {ok, Value, Origin} ->
                    Location = dependency_location(ParentLocation, Key),
                    {Projected, FieldDependencies} =
                        project_schema(Type, Value, Origin, Location, Opts),
                    {
                        Values#{ Key => Projected },
                        merge_dependencies(Dependencies, FieldDependencies)
                    };
                {error, not_found} when Presence =:= required ->
                    throw({required_key_missing, Key});
                {error, not_found} ->
                    {Values, Dependencies};
                {error, Reason} ->
                    throw({dependency_resolution_failed, Key, Reason})
            end
        end,
        {#{}, #{}},
        Keys
    ).

project_wildcard(none, _Keys, _Message, _Origin, _Location, _Opts) ->
    {#{}, #{}};
project_wildcard(
    #{ <<"presence">> := Presence, <<"type">> := Type },
    Keys,
    Message,
    ParentOrigin,
    ParentLocation,
    Opts
) ->
    % Enumerate the value already being projected. Calling its `keys'
    % operation here would itself require varying and recursively re-enter this
    % walker.
    AvailableKeys =
        maps:keys(
            hb_private:reset(hb_message:uncommitted(Message, Opts))
        ),
    project_wildcard_keys(
        Type,
        Presence,
        hb_util:list_without(maps:keys(Keys), AvailableKeys),
        Message,
        ParentOrigin,
        ParentLocation,
        Opts
    ).

project_wildcard_keys(
    Type,
    Presence,
    Keys,
    Message,
    ParentOrigin,
    ParentLocation,
    Opts
) ->
    lists:foldl(
        fun(Key, {Values, Dependencies}) ->
            case read_schema_fact(Message, Key, Presence, ParentOrigin, Opts) of
                {ok, Value, Origin} ->
                    Location = dependency_location(ParentLocation, Key),
                    {Projected, FieldDependencies} =
                        project_schema(Type, Value, Origin, Location, Opts),
                    {
                        Values#{ Key => Projected },
                        merge_dependencies(Dependencies, FieldDependencies)
                    };
                {error, not_found} when Presence =:= optional ->
                    {Values, Dependencies};
                {error, Reason} ->
                    throw({dependency_resolution_failed, Key, Reason})
            end
        end,
        {#{}, #{}},
        Keys
    ).

%% Optional schema fields describe values that may already be present; they do
%% not authorize execution of a same-named device operation. Required fields do
%% authorize normal AO-Core resolution and record the resulting claim.
read_schema_fact(Message, Key, optional, ParentOrigin, Opts) ->
    case hb_device:id_or_direct_key(Message, Key, Opts) of
        {hit, _} -> read_fact(Message, Key, ParentOrigin, Opts);
        {ok, _Device} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end;
read_schema_fact(Message, Key, _Presence, ParentOrigin, Opts) ->
    read_fact(Message, Key, ParentOrigin, Opts).

%% Resolve one fact without discarding the execution context: the result and
%% the hashpath formatted from that same context must remain inseparable.
read_fact(Message, Key, ParentOrigin, Opts) ->
    InitialCtx =
        #{
            <<"base">> => Message,
            <<"request">> => #{ <<"path">> => Key },
            <<"opts">> => Opts
        },
    Ctx =
        case ParentOrigin of
            undefined -> InitialCtx;
            _ -> InitialCtx#{ <<"base-id">> => ParentOrigin }
        end,
    case hb_ao:do(Ctx) of
        {ok, ResolvedCtx = #{ <<"result">> := Value }} ->
            case maps:get(<<"status">>, ResolvedCtx, ok) of
                ok ->
                    {
                        ok,
                        Value,
                        fact_origin(ResolvedCtx, Opts)
                    };
                Status ->
                    {error, {Status, Value}}
            end;
        {error, #{ <<"reason">> := Reason }} ->
            {error, Reason};
        Other ->
            {error, {unexpected_fact_resolution, Other}}
    end.

fact_origin(Ctx, Opts) ->
    case hb_opts:get(<<"hashpath">>, enabled, Opts) of
        enabled -> hb_hashpath:format(Ctx, Opts);
        _ -> undefined
    end.

dependency(undefined, _Location) -> #{};
dependency(Origin, Location) ->
    #{ Origin => #{ Location => true } }.

dependency_location(Parent, Key) ->
    <<Parent/binary, "/", (hb_ao:normalize_key(Key))/binary>>.

merge_dependencies(Left, Right) ->
    maps:merge_with(
        fun(_Hashpath, LeftUses, RightUses) -> maps:merge(LeftUses, RightUses) end,
        Left,
        Right
    ).

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
    case lists:member(Value, [true, false, 1, 0, <<"true">>, <<"false">>, <<"1">>, <<"0">>]) of
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
        true -> coerce_tuple(Items, Value, Opts);
        false -> error
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

coerce_tuple(Items, Values, Opts) ->
    case coerce_sequence(lists:zip(Items, Values), Opts) of
        error -> error;
        Coerced -> list_to_tuple(Coerced)
    end.

coerce_list(ItemType, Value, Opts) when is_list(Value) ->
    coerce_sequence([{ItemType, Item} || Item <- Value], Opts);
coerce_list(_ItemType, _Value, _Opts) ->
    error.

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
    coerce_exact(Expected, try_coerce(fun hb_util:atom/1, Value));
coerce_literal(Expected, Value) when is_list(Expected) ->
    coerce_exact(Expected, try_coerce(fun hb_util:list/1, Value));
coerce_literal(Expected, Expected) ->
    Expected;
coerce_literal(_Expected, _Value) ->
    error.

coerce_exact(Expected, Expected) ->
    Expected;
coerce_exact(_Expected, _Value) ->
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
check_type(#{ <<"kind">> := <<"remote">> }, _Value) -> true;
check_type(#{ <<"kind">> := <<"alias">> }, _Value) -> true;
check_type(#{ <<"kind">> := <<"variable">> }, _Value) -> true;
check_type(#{ <<"kind">> := <<"unknown">> }, _Value) -> true;
check_type(_, _Value) -> true.

normalize_name('_') -> <<"_">>;
normalize_name(Name) when is_atom(Name) -> hb_util:atom_to_dashed_binary(Name);
normalize_name(Name) -> hb_util:bin(Name).

literal_value(#{ <<"kind">> := <<"literal">>, <<"value">> := Value }) -> Value;
literal_value(_) -> undefined.

var_name({var, _, Name}) -> Name;
var_name(Name) -> Name.

message_type({Keys, Wildcard}) ->
    #{
        <<"kind">> => <<"message">>,
        <<"keys">> => Keys,
        <<"wildcard">> => Wildcard
    }.

any_type() -> #{ <<"kind">> => <<"any">> }.
empty_type() -> #{ <<"kind">> => <<"empty">> }.
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

empty_projection_test() ->
    ?assertEqual(
        #{ <<"device">> => <<"test-device@1.0">> },
        projected(
            implicit_base(empty_type()),
            #{ <<"device">> => <<"test-device@1.0">>, <<"extra">> => <<"drop">> },
            #{}
        )
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

default_handler_uses_resolved_function_schema_test() ->
    Func = fun default_schema_fun/4,
    ?assertEqual(
        default_schema,
        select_schema(
            Func,
            #{
                <<"default-schema-fun">> => default_schema,
                <<"requested-key">> => requested_key_schema
            }
        )
    ),
    ?assertEqual(
        undefined,
        select_schema(
            Func,
            #{ <<"other-key">> => other_schema }
        )
    ).

default_schema_fun(_Key, _Base, _Req, _Opts) ->
    {ok, unused}.

extension_projection_test() ->
    Schema =
        message_type(
            {
                #{
                    <<"a">> => #{ <<"presence">> => required, <<"type">> => empty_type() },
                    <<"c">> => #{ <<"presence">> => optional, <<"type">> => empty_type() }
                },
                none
            }
        ),
    Msg =
        #{
            <<"b">> => 2,
            <<"...">> =>
                #{
                    <<"a">> => 1,
                    <<"b">> => 1
                }
        },
    ?assertEqual(#{ <<"a">> => 1 }, projected(Schema, Msg, #{})).

extension_wildcard_carry_test() ->
    Schema =
        message_type(
            {
                #{
                    <<"a">> => #{ <<"presence">> => optional, <<"type">> => empty_type() }
                },
                #{ <<"presence">> => optional, <<"type">> => empty_type() }
            }
        ),
    Parent = #{ <<"a">> => 1, <<"b">> => 1, <<"c">> => 1 },
    Msg = #{ <<"b">> => 2, <<"...">> => Parent },
    ?assertEqual(
        #{ <<"a">> => 1, <<"b">> => 2, <<"...">> => Parent },
        projected(Schema, Msg, #{})
    ).

projected(Schema, Value, Opts) ->
    {Projected, _Dependencies} =
        project_schema(Schema, Value, undefined, <<"value">>, Opts),
    Projected.
