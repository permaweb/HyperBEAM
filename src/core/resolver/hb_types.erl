%%% @doc Extract device specs and vary AO-Core execution inputs.
-module(hb_types).
-export([extract/2, vary/2, add_schema/2, beam_to_schema/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Add the schema for a resolution to an execution context, given a
%% resolved key, function, and execution module (e.g, from
%% `hb_device:add_resolver`).
add_schema(Ctx, Opts) ->
    case hb_opts:get(<<"caching-schema">>, false, Opts) of
        true -> {ok, Ctx};
        false -> do_add_schema(Ctx, Opts)
    end.

do_add_schema(
    Ctx =
        #{
            <<"key">> := Key,
            <<"resolver-device">> := Device,
            <<"priv">> :=
                #{
                    <<"function">> := Func,
                    <<"add-key">> := AddKey
                }
        },
    Opts) ->
    case schema_from_device(Device, Func, Key, Opts) of
        undefined ->
            ?event_debug(
                {schema_not_found,
                    {device, Device},
                    {func, Func},
                    {key, Key}
                }
            ),
            {ok, Ctx};
        Schema ->
            ?event_debug(
                {schema_found,
                    {device, Device},
                    {func, Func},
                    {key, Key},
                    {schema, Schema}
                }
            ),
            {ok, Ctx#{ <<"schema">> => execution_schema(Schema, AddKey) } }
    end;

do_add_schema(Ctx, _Opts) ->
    {ok, Ctx}.

%% @doc Apply the resolved function's base/request schemas to one execution.
vary(Ctx = #{ <<"base">> := Base, <<"request">> := Req }, _Opts)
        when not is_map_key(<<"schema">>, Ctx) ->
    case {hb_opts:get(<<"caching-schema">>, false, _Opts), hashpath_ignored(_Opts)} of
        {true, _} ->
            {ok,
                schema_declared_context(Ctx#{
                    <<"varied-base">> => Base,
                    <<"varied-request">> => Req,
                    <<"normalizer">> => none
                })
            };
        {_, true} ->
            {ok,
                schema_declared_context(Ctx#{
                    <<"varied-base">> => Base,
                    <<"varied-request">> => Req,
                    <<"normalizer">> => identity_normalizer(Req)
                })
            };
        {false, false} ->
            ProjectionOpts = schema_projection_opts(_Opts),
            WitnessBase = identity_witness(Base, ProjectionOpts),
            WitnessReq = identity_witness(Req, ProjectionOpts),
            {ok,
                maybe_add_dependencies(
                    schema_declared_context(Ctx#{
                        <<"varied-base">> => WitnessBase,
                        <<"varied-request">> => WitnessReq,
                        <<"normalizer">> => identity_normalizer(Req)
                    }),
                    fun() ->
                        #{
                            <<"base">> =>
                                identity_dependencies(Base, WitnessBase, ProjectionOpts),
                            <<"request">> =>
                                identity_dependencies(Req, WitnessReq, ProjectionOpts)
                        }
                    end,
                    _Opts
                )
            }
    end;
vary(Ctx = #{ <<"schema">> := [BaseSchema, ReqSchema, ReturnSchema] }, Opts) ->
    vary_with_schema(Ctx, BaseSchema, ReqSchema, any_type(), ReturnSchema, Opts);
vary(Ctx = #{ <<"schema">> := [BaseSchema, ReqSchema, OptsSchema, ReturnSchema] }, Opts) ->
    vary_with_schema(Ctx, BaseSchema, ReqSchema, OptsSchema, ReturnSchema, Opts).

vary_with_schema(
    Ctx = #{ <<"base">> := Base, <<"request">> := Req },
    BaseSchema,
    ReqSchema,
    OptsSchema,
    ReturnSchema,
    Opts
) ->
    ProjectionOpts = schema_projection_opts(Opts),
    BaseProjection = implicit_base(BaseSchema),
    ReqProjection = implicit_request(ReqSchema),
    {_, Env0} = apply_schema_env(OptsSchema, Opts, ProjectionOpts, #{}),
    case hashpath_ignored(Opts) of
        true ->
            {VariedBase, Env1} = apply_schema_env(BaseProjection, Base, ProjectionOpts, Env0),
            {VariedReq, _Env2} = apply_schema_env(ReqProjection, Req, ProjectionOpts, Env1),
            {ok,
                schema_declared_context(Ctx#{
                    <<"varied-base">> => VariedBase,
                    <<"varied-request">> => VariedReq,
                    <<"normalizer">> => overlay(ReturnSchema)
                })
            };
        false ->
            {VariedBase, BaseDeps, Env1} =
                apply_schema_with_dependencies(BaseProjection, Base, ProjectionOpts, Env0),
            {VariedReq, ReqDeps, _Env2} =
                apply_schema_with_dependencies(ReqProjection, Req, ProjectionOpts, Env1),
            {ok,
                schema_declared_context(Ctx#{
                    <<"varied-base">> => VariedBase,
                    <<"varied-request">> => VariedReq,
                    <<"dependencies">> =>
                        #{
                            <<"base">> => BaseDeps,
                            <<"request">> => ReqDeps
                        },
                    <<"normalizer">> => overlay(ReturnSchema)
                })
            }
    end.

schema_declared_context(Ctx) ->
    Ctx#{ <<"claim-level">> => <<"schema-declared">> }.

hashpath_ignored(Opts) ->
    hashpath_mode(Opts) =:= ignore.

hashpath_mode(Opts) when is_map(Opts) ->
    case maps:get(<<"hashpath">>, Opts, maps:get(hashpath, Opts, undefined)) of
        undefined -> hb_opts:get(<<"hashpath">>, enabled, Opts);
        Mode -> Mode
    end;
hashpath_mode(Opts) ->
    hb_opts:get(<<"hashpath">>, enabled, Opts).

maybe_add_dependencies(Ctx, DepsFun, Opts) ->
    case hashpath_ignored(Opts) of
        true -> Ctx;
        false -> Ctx#{ <<"dependencies">> => DepsFun() }
    end.

schema_projection_opts(Opts) ->
    Opts#{ <<"caching-schema">> => true }.

active_surface_opts(Opts) ->
    schema_projection_opts(
        Opts#{
            <<"hashpath">> => ignore,
            <<"spawn-worker">> => false
        }
    ).

identity_witness(Value, Opts) when is_map(Value) ->
    active_message_surface(Value, Opts);
identity_witness(Value, _Opts) ->
    Value.

identity_dependencies(Source, Witness, Opts) ->
    add_identity_unset_dependencies(
        Source,
        Source,
        [],
        dependency_tree(Source, [], Witness, Opts),
        Opts
    ).

add_identity_unset_dependencies(RootSource, Source, Path, Deps, Opts) when is_map(Source) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            case hb_private:is_private(Key) of
                true ->
                    Acc;
                false ->
                    FullPath = Path ++ [Key],
                    case Key of
                        <<"...">> ->
                            add_inherited_identity_unset_dependencies(
                                RootSource,
                                raw_unset_source(Value, Opts),
                                Path,
                                Acc,
                                direct_shadow_keys(Source, Opts),
                                Opts
                            );
                        _ ->
                            case unset_surface_value(Value, Opts) of
                                true ->
                                    add_identity_unset_dependency(Key, RootSource, FullPath, Acc, Opts);
                                false when is_map(Value) ->
                                    case maps:get(Key, Acc, undefined) of
                                        undefined ->
                                            Acc;
                                        ChildDeps ->
                                            Acc#{
                                                Key =>
                                                    add_identity_unset_dependencies(
                                                        RootSource,
                                                        Value,
                                                        FullPath,
                                                        ChildDeps,
                                                        Opts
                                                    )
                                            }
                                    end;
                                false ->
                                    Acc
                            end
                    end
            end
        end,
        Deps,
        hb_maps:to_list(hb_private:reset(Source), Opts)
    );
add_identity_unset_dependencies(_RootSource, _Source, _Path, Deps, _Opts) ->
    Deps.

add_inherited_identity_unset_dependencies(RootSource, Source, Path, Deps, Shadowed, Opts)
        when is_map(Source) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            case hb_private:is_private(Key) orelse lists:member(Key, Shadowed) of
                true ->
                    Acc;
                false ->
                    FullPath = Path ++ [Key],
                    case Key of
                        <<"...">> ->
                            add_inherited_identity_unset_dependencies(
                                RootSource,
                                raw_unset_source(Value, Opts),
                                Path,
                                Acc,
                                lists:usort(Shadowed ++ direct_shadow_keys(Source, Opts)),
                                Opts
                            );
                        _ ->
                            case unset_surface_value(Value, Opts) of
                                true ->
                                    add_identity_unset_dependency(Key, RootSource, FullPath, Acc, Opts);
                                false when is_map(Value) ->
                                    case maps:get(Key, Acc, undefined) of
                                        undefined ->
                                            Acc;
                                        ChildDeps ->
                                            Acc#{
                                                Key =>
                                                    add_identity_unset_dependencies(
                                                        RootSource,
                                                        Value,
                                                        FullPath,
                                                        ChildDeps,
                                                        Opts
                                                    )
                                            }
                                    end;
                                false ->
                                    Acc
                            end
                    end
            end
        end,
        Deps,
        hb_maps:to_list(hb_private:reset(Source), Opts)
    );
add_inherited_identity_unset_dependencies(_RootSource, _Source, _Path, Deps, _Shadowed, _Opts) ->
    Deps.

direct_shadow_keys(Source, Opts) when is_map(Source) ->
    [
        Key
    ||
        {Key, _Value} <- hb_maps:to_list(hb_private:reset(Source), Opts),
        Key =/= <<"...">>,
        not hb_private:is_private(Key)
    ];
direct_shadow_keys(_Source, _Opts) ->
    [].

add_identity_unset_dependency(Key, RootSource, FullPath, Deps, Opts) ->
    case maps:is_key(Key, Deps) of
        true ->
            Deps;
        false ->
            Deps#{
                Key =>
                    #{
                        <<"status">> => unset,
                        <<"origin">> => origin_hashpath(RootSource, FullPath, Opts),
                        <<"path">> => hb_path:to_binary(FullPath)
                    }
            }
    end.

identity_normalizer(#{ <<"path">> := <<"*">> }) ->
    base;
identity_normalizer(<<"*">>) ->
    base;
identity_normalizer(_Req) ->
    none.

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

schema_from_device(Device, Func, Key, Opts) ->
    case extract(Device, Opts) of
        {ok, #{ <<"keys">> := Schemas }} ->
            select_schema(Func, Key, Schemas);
        {error, _Reason} ->
            undefined
    end.

select_schema(Func, _Key, Schemas) ->
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
        nth_or(3 + Offset, Args, any_type()),
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
parse_type({remote_type, _, [{atom, _, hb_schema}, {atom, _, bind}, [Arg]]},
        TypeEnv,
        VarEnv,
        Seen) ->
    #{ <<"kind">> => <<"bind">>, <<"var">> => schema_arg_var(Arg, TypeEnv, VarEnv, Seen) };
parse_type({remote_type, _, [{atom, _, hb_schema}, {atom, _, int}, [Arg]]},
        TypeEnv,
        VarEnv,
        Seen) ->
    (scalar_type(<<"integer">>))#{
        <<"bind">> => schema_arg_var(Arg, TypeEnv, VarEnv, Seen)
    };
parse_type({remote_type, _, [{atom, _, hb_schema}, {atom, _, date}, [Unit, Bucket, Format]]},
        TypeEnv,
        VarEnv,
        Seen) ->
    #{
        <<"kind">> => <<"synthetic-date">>,
        <<"unit">> => schema_arg(Unit, TypeEnv, VarEnv, Seen),
        <<"bucket">> => schema_arg(Bucket, TypeEnv, VarEnv, Seen),
        <<"format">> => schema_arg(Format, TypeEnv, VarEnv, Seen)
    };
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

schema_arg_var(Arg, TypeEnv, VarEnv, Seen) ->
    case schema_arg(Arg, TypeEnv, VarEnv, Seen) of
        #{ <<"kind">> := <<"var-ref">>, <<"name">> := Name } -> Name;
        #{ <<"kind">> := <<"literal">>, <<"value">> := Value } -> hb_util:bin(Value);
        Other -> hb_util:bin(io_lib:format("~tp", [Other]))
    end.

schema_arg({var, _, Name}, TypeEnv, VarEnv, Seen) ->
    case maps:get(Name, VarEnv, undefined) of
        undefined -> var_ref_type(Name);
        Bound -> schema_arg(Bound, TypeEnv, VarEnv, Seen)
    end;
schema_arg({atom, _, Atom}, _TypeEnv, _VarEnv, _Seen) ->
    literal_type(hb_util:bin(Atom));
schema_arg({integer, _, Int}, _TypeEnv, _VarEnv, _Seen) ->
    literal_type(Int);
schema_arg({char, _, Char}, _TypeEnv, _VarEnv, _Seen) ->
    literal_type(<<Char/utf8>>);
schema_arg({string, _, String}, _TypeEnv, _VarEnv, _Seen) ->
    literal_type(hb_util:bin(String));
schema_arg(Other, TypeEnv, VarEnv, Seen) ->
    parse_type(Other, TypeEnv, VarEnv, Seen).

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

apply_schema(Schema, Value, Opts) ->
    {Projected, _Env} = apply_schema_env(Schema, Value, Opts, #{}),
    Projected.

apply_schema_env(#{ <<"kind">> := <<"any">> }, Value, _Opts, Env) ->
    {Value, Env};
apply_schema_env(#{ <<"kind">> := <<"empty">> }, Value, _Opts, Env) ->
    {Value, Env};
apply_schema_env(#{ <<"bind">> := Var } = Type, Value, Opts, Env) ->
    {Projected, Env1} = apply_schema_env(maps:remove(<<"bind">>, Type), Value, Opts, Env),
    {Projected, bind_schema_var(Var, Projected, Env1)};
apply_schema_env(#{ <<"kind">> := <<"bind">>, <<"var">> := Var }, Value, _Opts, Env) ->
    {Value, bind_schema_var(Var, Value, Env)};
apply_schema_env(Schema = #{ <<"kind">> := <<"synthetic-date">> }, Value, Opts, Env) ->
    {synthetic_date(Schema, Value, Opts, Env), Env};
apply_schema_env(Schema = #{ <<"kind">> := <<"message">> }, Value, Opts, Env)
        when not is_map(Value) ->
    apply_schema_env(Schema, hb_cache:ensure_loaded(Value, Opts), Opts, Env);
apply_schema_env(
    #{ <<"kind">> := <<"message">>, <<"keys">> := Keys, <<"wildcard">> := Wildcard },
    Message,
    Opts,
    Env0
) when is_map(Message) ->
    {Explicit, Env} = explicit_keys(Keys, Message, Opts, Env0),
    {maps:merge(wildcard_keys(Wildcard, Keys, Message, Opts), Explicit), Env};
apply_schema_env(Type, Value, Opts, Env) ->
    case check_type(Type, Value) of
        true ->
            {Value, Env};
        false ->
            case coerce_type(Type, Value, Opts) of
                error -> throw({invalid_type, Type, Value});
                Coerced ->
                    case check_type(Type, Coerced) of
                        true -> {Coerced, Env};
                        false -> throw({invalid_type, Type, Value})
                    end
            end
    end.

apply_schema_with_dependencies(Schema, Value, Opts, Env0) ->
    {Projected, Env} = apply_schema_env(Schema, Value, Opts, Env0),
    {
        Projected,
        add_dependency_observations(
            Schema,
            Value,
            Value,
            [],
            Projected,
            dependency_tree(Value, [], Projected, Opts),
            Opts
        ),
        Env
    }.

add_dependency_observations(
    #{ <<"kind">> := <<"message">>, <<"keys">> := Keys },
    RootSource,
    Source,
    Path,
    Projected,
    Deps,
    Opts
) when is_map(Projected) ->
    maps:fold(
        fun(Key, Field, Acc) ->
            add_key_dependency_observation(Key, Field, RootSource, Source, Path, Projected, Acc, Opts)
        end,
        Deps,
        Keys
    );
add_dependency_observations(_Schema, _RootSource, _Source, _Path, _Projected, Deps, _Opts) ->
    Deps.

add_key_dependency_observation(
    Key,
    #{ <<"presence">> := optional },
    RootSource,
    Source,
    Path,
    Projected,
    Deps,
    Opts
) when not is_map_key(Key, Projected) ->
    FullPath = Path ++ [Key],
    case raw_unset_at_path(Source, [Key], Opts) of
        true ->
            Deps#{
                Key =>
                    #{
                        <<"status">> => unset,
                        <<"origin">> => origin_hashpath(RootSource, FullPath, Opts),
                        <<"path">> => hb_path:to_binary(FullPath)
                    }
            };
        false ->
            Deps#{
                Key =>
                    #{
                        <<"status">> => not_found,
                        <<"origin">> => origin_hashpath(RootSource, FullPath, Opts),
                        <<"path">> => hb_path:to_binary(FullPath)
                    }
            }
    end;
add_key_dependency_observation(
    Key,
    #{ <<"type">> := Type },
    RootSource,
    Source,
    Path,
    Projected,
    Deps,
    Opts
) ->
    case maps:find(Key, Projected) of
        {ok, ProjectedValue} ->
            case source_child(Source, Key, Opts) of
                {ok, ChildSource} ->
                    Deps#{
                        Key =>
                            add_dependency_observations(
                                Type,
                                RootSource,
                                ChildSource,
                                Path ++ [Key],
                                ProjectedValue,
                                maps:get(Key, Deps, #{}),
                                Opts
                            )
                    };
                error ->
                    Deps
            end;
        error ->
            Deps
    end.

source_child(Source, Key, Opts) ->
    case hb_ao:raw(Source, #{ <<"path">> => Key }, Opts) of
        {ok, Value} -> {ok, Value};
        _ -> error
    end.

dependency_tree(Source, Path, Value, Opts) when is_map(Value) ->
    maps:from_list(
        [
            {Key, dependency_tree(Source, Path ++ [Key], Child, Opts)}
        ||
            {Key, Child} <- hb_maps:to_list(hb_private:reset(Value), Opts),
            Key =/= <<"...">>,
            not hb_private:is_private(Key)
        ]
    );
dependency_tree(Source, Path, Value, Opts)
        when Value =:= unset; Value =:= <<"unset">> ->
    #{
        <<"status">> => unset,
        <<"origin">> => origin_hashpath(Source, Path, Opts),
        <<"path">> => hb_path:to_binary(Path)
    };
dependency_tree(Source, Path, Value, Opts) ->
    positive_dependency_leaf(Source, Path, Value, Opts).

positive_dependency_leaf(Source, Path, Value, Opts) ->
    Origin = origin_hashpath(Source, Path, Opts),
    case observed_value_at_path(Source, Path, Opts) of
        {ok, Observed} ->
            case same_dependency_value(Value, Observed, Opts) of
                true ->
                    Origin;
                false ->
                    #{
                        <<"status">> => found,
                        <<"origin">> => Origin,
                        <<"observed">> => Observed,
                        <<"value">> => Value
                    }
            end;
        _ ->
            Origin
    end.

observed_value_at_path(Source, [], _Opts) ->
    {ok, Source};
observed_value_at_path(Source, [Key | Rest], Opts) ->
    case source_child(Source, Key, Opts) of
        {ok, Child} -> observed_value_at_path(Child, Rest, Opts);
        error -> {error, not_found}
    end.

same_dependency_value(Left, Right, Opts) ->
    hb_private:reset(hb_cache:ensure_all_loaded(Left, Opts))
        =:= hb_private:reset(hb_cache:ensure_all_loaded(Right, Opts)).

raw_unset_at_path(_Source, [], _Opts) ->
    false;
raw_unset_at_path(Source, [Key | Rest], Opts) ->
    case raw_unset_source(Source, Opts) of
        Message when is_map(Message) ->
            case maps:find(Key, Message) of
                {ok, Value0} ->
                    Value = hb_cache:ensure_loaded(Value0, Opts),
                    case {unset_literal(Value), Rest} of
                        {true, _} -> true;
                        {false, [_ | _]} -> raw_unset_at_path(Value, Rest, Opts);
                        {false, []} -> false
                    end;
                error ->
                    case maps:find(<<"...">>, Message) of
                        {ok, Ancestor} -> raw_unset_at_path(Ancestor, [Key | Rest], Opts);
                        error -> false
                    end
            end;
        _ ->
            false
    end.

raw_unset_source(Source, _Opts) when is_map(Source) ->
    Source;
raw_unset_source(Source, Opts) when ?IS_LINK(Source) ->
    hb_cache:ensure_loaded(Source, Opts);
raw_unset_source(Source, Opts) when is_binary(Source) ->
    case is_hashpath_reference(Source) of
        true ->
            case hb_hashpath:load(Source, Opts) of
                {ok, Loaded} -> Loaded;
                _ -> Source
            end;
        false ->
            case hb_cache:read(Source, Opts) of
                {ok, Loaded} -> Loaded;
                _ -> Source
            end
    end;
raw_unset_source(Source, _Opts) ->
    Source.

is_hashpath_reference(Value) when is_binary(Value) ->
    binary:match(Value, <<"/">>) =/= nomatch;
is_hashpath_reference(_Value) ->
    false.

unset_literal(unset) -> true;
unset_literal(<<"unset">>) -> true;
unset_literal(_) -> false.

origin_hashpath(Source, [], Opts) ->
    origin_ref(Source, Opts);
origin_hashpath(Source, Path, Opts) ->
    {Ref, OriginPath} = origin_location(Source, Path, Opts),
    origin_ref_path(Ref, OriginPath, Opts).

origin_location(Source, [], Opts) ->
    {origin_ref(Source, Opts), []};
origin_location(Source, Path = [Key | _], Opts) ->
    case origin_message(Source, Opts) of
        {ok, Message, Ref} ->
            case hb_maps:find(Key, Message, Opts) of
                {ok, Value} ->
                    origin_location_at(Value, tl(Path), Ref, [Key], Opts);
                error ->
                    case hb_maps:find(<<"...">>, Message, Opts) of
                        {ok, Ancestor} -> origin_location(Ancestor, Path, Opts);
                        error -> {Ref, Path}
                    end
            end;
        error ->
            {origin_ref(Source, Opts), Path}
    end.

origin_location_at(_Value, [], Ref, Prefix, _Opts) ->
    {Ref, Prefix};
origin_location_at(Value, Path = [Key | Rest], Ref, Prefix, Opts) ->
    case origin_message(Value, Opts) of
        {ok, Message, _ValueRef} ->
            case hb_maps:find(Key, Message, Opts) of
                {ok, Child} ->
                    origin_location_at(Child, Rest, Ref, Prefix ++ [Key], Opts);
                error ->
                    case hb_maps:find(<<"...">>, Message, Opts) of
                        {ok, Ancestor} -> origin_location(Ancestor, Path, Opts);
                        error -> {Ref, Prefix ++ Path}
                    end
            end;
        error ->
            {Ref, Prefix ++ Path}
    end.

origin_message(Source, _Opts) when is_map(Source) ->
    {ok, Source, origin_ref(Source, _Opts)};
origin_message(Source, Opts) when ?IS_LINK(Source) ->
    case hb_cache:ensure_loaded(Source, Opts) of
        Msg when is_map(Msg) -> {ok, Msg, origin_ref(Source, Opts)};
        _ -> error
    end;
origin_message(Source, Opts) when is_binary(Source) ->
    case is_hashpath_reference(Source) of
        true ->
            case hb_hashpath:load(Source, Opts) of
                {ok, Msg} when is_map(Msg) -> {ok, Msg, Source};
                _ -> error
            end;
        false ->
            case hb_cache:read(Source, Opts) of
                {ok, Msg} when is_map(Msg) -> {ok, Msg, Source};
                _ -> error
            end
    end;
origin_message(_Source, _Opts) ->
    error.

origin_ref(Source, _Opts) when is_map(Source) ->
    case hb_private:from_message(Source) of
        #{ <<"hashpath">> := HP } ->
            case origin_hashpath_matches_source(HP, Source, _Opts) of
                true -> HP;
                false -> hb_message:id(Source, all, schema_projection_opts(_Opts))
            end;
        _ -> hb_message:id(Source, all, schema_projection_opts(_Opts))
    end;
origin_ref(Source, _Opts) when is_binary(Source) ->
    Source;
origin_ref(Source, Opts) ->
    hb_message:id(Source, all, schema_projection_opts(Opts)).

origin_ref_path(Ref, [], _Opts) ->
    Ref;
origin_ref_path(Ref, Path, _Opts) ->
    PathBin = hb_path:to_binary(Path),
    <<Ref/binary, "/", PathBin/binary>>.

origin_hashpath_matches_source(Hashpath, Source, Opts) ->
    case hb_hashpath:load(Hashpath, Opts) of
        {ok, Loaded} -> same_active_message(Source, Loaded, Opts);
        _ -> false
    end.

same_active_message(Left, Right, Opts) when is_map(Left), is_map(Right) ->
    CompareOpts =
        schema_projection_opts(
            Opts#{
                <<"hashpath">> => ignore,
                <<"spawn-worker">> => false
            }
        ),
    case {deep_public_keys(Left, CompareOpts), deep_public_keys(Right, CompareOpts)} of
        {{ok, Keys}, {ok, Keys}} ->
            lists:all(
                fun(Key) -> same_resolved_key(Key, Left, Right, CompareOpts) end,
                Keys
            );
        _ ->
            hb_private:reset(hb_cache:ensure_all_loaded(Left, Opts))
                =:= hb_private:reset(hb_cache:ensure_all_loaded(Right, Opts))
    end;
same_active_message(Left, Right, Opts) ->
    hb_private:reset(hb_cache:ensure_all_loaded(Left, Opts))
        =:= hb_private:reset(hb_cache:ensure_all_loaded(Right, Opts)).

deep_public_keys(Msg, Opts) when is_list(Msg) ->
    case hb_ao:normalize_keys(Msg, Opts) of
        NormMsg when is_map(NormMsg) -> deep_public_keys(NormMsg, Opts);
        _ -> {error, badarg}
    end;
deep_public_keys(Msg, Opts) when is_map(Msg) ->
    {ok, lists:sort(active_public_keys(Msg, Opts))};
deep_public_keys(_Msg, _Opts) ->
    {error, not_found}.

active_public_keys(Msg, Opts) ->
    Pairs = hb_maps:to_list(Msg, Opts),
    MaskedKeys =
        [
            Key
        ||
            {Key, Value} <- Pairs,
            unset_surface_value(Value, Opts)
        ],
    Inherited =
        case hb_maps:find(<<"...">>, Msg, Opts) of
            {ok, Extension} ->
                case raw_unset_source(Extension, Opts) of
                    Ancestor when is_map(Ancestor) -> active_public_keys(Ancestor, Opts);
                    _ -> []
                end;
            error ->
                []
        end,
    Hidden = [<<"commitments">> | MaskedKeys],
    DirectKeys =
        [
            Key
        ||
            {Key, Value} <- Pairs,
            Key =/= <<"...">>,
            not hb_private:is_private(Key),
            not unset_surface_value(Value, Opts)
        ],
    InheritedPublic = [Key || Key <- Inherited, not lists:member(Key, Hidden)],
    lists:usort(DirectKeys ++ InheritedPublic).

same_resolved_key(Key, Left, Right, Opts) ->
    case {hb_ao:resolve(Left, Key, Opts), hb_ao:resolve(Right, Key, Opts)} of
        {{ok, LeftValue}, {ok, RightValue}} ->
            hb_private:reset(hb_cache:ensure_all_loaded(LeftValue, Opts))
                =:= hb_private:reset(hb_cache:ensure_all_loaded(RightValue, Opts));
        _ ->
            false
    end.

explicit_keys(Keys, Message, Opts, Env0) ->
    maps:fold(
        fun(Key, #{ <<"presence">> := Presence, <<"type">> := Type }, {Acc, Env}) ->
            case hb_ao:raw(Message, #{ <<"path">> => Key }, Opts) of
                {ok, Value} ->
                    {Projected, Env1} = apply_schema_env(Type, Value, Opts, Env),
                    {Acc#{ Key => Projected }, Env1};
                {error, not_found} ->
                    case {is_synthetic(Type), Presence} of
                        {true, _} ->
                            {Projected, Env1} = apply_schema_env(Type, undefined, Opts, Env),
                            {Acc#{ Key => Projected }, Env1};
                        {false, required} ->
                            throw({required_key_missing, Key});
                        {false, _} ->
                            {Acc, Env}
                    end
            end
        end,
        {#{}, Env0},
        Keys
    ).

is_synthetic(#{ <<"kind">> := <<"synthetic-date">> }) ->
    true;
is_synthetic(_Type) ->
    false.

bind_schema_var(<<"_">>, _Value, Env) ->
    Env;
bind_schema_var(Var, Value, Env) ->
    case maps:find(Var, Env) of
        {ok, Value} -> Env;
        {ok, Other} -> throw({schema_variable_mismatch, Var, Other, Value});
        error -> Env#{ Var => Value }
    end.

synthetic_date(Schema, Value, Opts, Env) ->
    Unit = normalize_date_unit(resolve_schema_arg(maps:get(<<"unit">>, Schema), Env)),
    Bucket = positive_date_bucket(resolve_schema_arg(maps:get(<<"bucket">>, Schema), Env)),
    Format = normalize_date_format(resolve_schema_arg(maps:get(<<"format">>, Schema), Env)),
    Seconds = date_source_seconds(Value, Opts),
    BucketSeconds = Bucket * date_unit_seconds(Unit),
    BucketedSeconds = (Seconds div BucketSeconds) * BucketSeconds,
    format_synthetic_date(Format, BucketedSeconds).

resolve_schema_arg(#{ <<"kind">> := <<"var-ref">>, <<"name">> := Name }, Env) ->
    case maps:find(Name, Env) of
        {ok, Value} -> Value;
        error -> throw({unbound_schema_variable, Name})
    end;
resolve_schema_arg(#{ <<"kind">> := <<"literal">>, <<"value">> := Value }, _Env) ->
    Value;
resolve_schema_arg(Value, _Env) ->
    Value.

date_source_seconds(undefined, Opts) ->
    case maps:get(<<"date-now">>, Opts, undefined) of
        undefined -> erlang:system_time(second);
        Now -> hb_util:int(Now)
    end;
date_source_seconds(Value, _Opts) when is_integer(Value) ->
    Value;
date_source_seconds(Value, _Opts) when is_binary(Value) ->
    hb_util:int(Value).

normalize_date_unit(Unit) when is_atom(Unit) ->
    normalize_date_unit(hb_util:bin(Unit));
normalize_date_unit(<<"second">>) -> second;
normalize_date_unit(<<"seconds">>) -> second;
normalize_date_unit(<<"minute">>) -> minute;
normalize_date_unit(<<"minutes">>) -> minute;
normalize_date_unit(<<"hour">>) -> hour;
normalize_date_unit(<<"hours">>) -> hour;
normalize_date_unit(<<"day">>) -> day;
normalize_date_unit(<<"days">>) -> day;
normalize_date_unit(Unit) ->
    throw({unsupported_date_unit, Unit}).

date_unit_seconds(second) -> 1;
date_unit_seconds(minute) -> 60;
date_unit_seconds(hour) -> 60 * 60;
date_unit_seconds(day) -> 24 * 60 * 60.

positive_date_bucket(Bucket0) ->
    case hb_util:int(Bucket0) of
        Bucket when Bucket > 0 -> Bucket;
        Bucket -> throw({invalid_date_bucket, Bucket})
    end.

normalize_date_format(Format) when is_atom(Format) ->
    normalize_date_format(hb_util:bin(Format));
normalize_date_format(<<"http">>) -> http;
normalize_date_format(<<"unix">>) -> unix;
normalize_date_format(Format) ->
    throw({unsupported_date_format, Format}).

format_synthetic_date(unix, Seconds) ->
    integer_to_binary(Seconds);
format_synthetic_date(http, Seconds) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:system_time_to_universal_time(Seconds, second),
    iolist_to_binary(
        io_lib:format(
            "~s, ~2..0B ~s ~4..0B ~2..0B:~2..0B:~2..0B GMT",
            [
                http_weekday(calendar:day_of_the_week(Year, Month, Day)),
                Day,
                http_month(Month),
                Year,
                Hour,
                Minute,
                Second
            ]
        )
    ).

http_weekday(1) -> "Mon";
http_weekday(2) -> "Tue";
http_weekday(3) -> "Wed";
http_weekday(4) -> "Thu";
http_weekday(5) -> "Fri";
http_weekday(6) -> "Sat";
http_weekday(7) -> "Sun".

http_month(1) -> "Jan";
http_month(2) -> "Feb";
http_month(3) -> "Mar";
http_month(4) -> "Apr";
http_month(5) -> "May";
http_month(6) -> "Jun";
http_month(7) -> "Jul";
http_month(8) -> "Aug";
http_month(9) -> "Sep";
http_month(10) -> "Oct";
http_month(11) -> "Nov";
http_month(12) -> "Dec".

wildcard_keys(none, _Keys, _Message, _Opts) ->
    #{};
wildcard_keys(#{ <<"presence">> := optional }, Keys, Message, Opts) ->
    maps:without(maps:keys(Keys), public_message_surface(Message, Opts));
wildcard_keys(#{ <<"presence">> := required, <<"type">> := Type }, Keys, Message, Opts) ->
    maps:map(
        fun(_Key, Value) ->
            apply_schema(Type, hb_cache:ensure_all_loaded(Value, Opts), Opts)
        end,
        maps:without(maps:keys(Keys), public_message_surface(Message, Opts))
    ).

public_message_surface(Message, Opts) ->
    active_message_surface(Message, Opts).

active_message_surface(Message, Opts) ->
    SurfaceOpts = active_surface_opts(Opts),
    case deep_public_keys(Message, SurfaceOpts) of
        {ok, Keys} ->
            maps:from_list(
                lists:filtermap(
                    fun(Key) ->
                        surface_key(Message, Key, SurfaceOpts)
                    end,
                    Keys
                )
            );
        _ ->
            direct_public_surface(Message, SurfaceOpts)
    end.

surface_key(Message, Key, Opts) ->
    try hb_ao:resolve(Message, Key, Opts) of
        {ok, Value} -> {true, {Key, surface_value(Value, Opts)}};
        _ -> false
    catch
        _:_ -> false
    end.

surface_value(Value, Opts) when is_map(Value) ->
    active_message_surface(Value, Opts);
surface_value(Value, _Opts) ->
    hb_private:reset(Value).

direct_public_surface(Message, Opts) ->
    maps:from_list(
        [
            {Key, surface_value(Value, Opts)}
        ||
            {Key, Value} <- hb_maps:to_list(hb_message:uncommitted(Message, Opts), Opts),
            Key =/= <<"...">>,
            not hb_private:is_private(Key),
            not unset_surface_value(Value, Opts)
        ]
    ).

unset_surface_value(Value, _Opts) when Value =:= unset; Value =:= <<"unset">> ->
    true;
unset_surface_value(Value, Opts) when ?IS_LINK(Value) ->
    try unset_literal(hb_cache:ensure_loaded(Value, Opts)) of
        IsUnset -> IsUnset
    catch
        _:_ -> false
    end;
unset_surface_value(_Value, _Opts) ->
    false.

overlay(ReturnSchema) ->
    case overlay_type(ReturnSchema) of
        base -> base;
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
overlay_marker(#{ <<"kind">> := <<"alias">>, <<"name">> := <<"base">> }) -> base;
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
var_ref_type(Name) -> #{ <<"kind">> => <<"var-ref">>, <<"name">> => normalize_name(Name) }.
unknown_type(Other) ->
    #{ <<"kind">> => <<"unknown">>, <<"ast">> => hb_util:bin(io_lib:format("~tp", [Other])) }.
boolean_type() ->
    #{
        <<"kind">> => <<"union">>,
        <<"members">> => [literal_type(true), literal_type(false)]
    }.

%%% Tests

-spec date_poc_schema_fun(
    map(),
    #{ date => hb_schema:date(Unit, Bucket, http) },
    #{
        process_now_bucket_size => hb_schema:int(Bucket),
        process_now_bucket_unit => hb_schema:bind(Unit)
    }
) -> map().
date_poc_schema_fun(Base, Req, _Opts) ->
    maps:merge(Base, Req).

date_schema_forms_can_bind_opts_and_synthesize_request_date_test() ->
    ?assertEqual(#{}, date_poc_schema_fun(#{}, #{}, #{})),
    {ok, #{ <<"keys">> := Schemas }} = beam_to_schema(?MODULE),
    Schema = maps:get(<<"date-poc-schema-fun">>, Schemas),
    [BaseSchema, ReqSchema, OptsSchema, ReturnSchema] = execution_schema(Schema, false),
    {ok, Ctx} =
        vary(
            #{
                <<"schema">> => [BaseSchema, ReqSchema, OptsSchema, ReturnSchema],
                <<"base">> => #{ <<"device">> => <<"message@1.0">> },
                <<"request">> => #{ <<"path">> => <<"current">> }
            },
            #{
                <<"hashpath">> => ignore,
                <<"date-now">> => 641,
                <<"process-now-bucket-size">> => 10,
                <<"process-now-bucket-unit">> => <<"minutes">>
            }
        ),
    ?assertEqual(
        <<"Thu, 01 Jan 1970 00:10:00 GMT">>,
        maps:get(<<"date">>, maps:get(<<"varied-request">>, Ctx))
    ).

empty_projection_test() ->
    ?assertEqual(
        #{ <<"device">> => <<"test-device@1.0">> },
        apply_schema(
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

required_wildcard_uses_public_surface_test() ->
    Schema =
        message_type(
            {
                #{
                    <<"a">> => #{ <<"presence">> => optional, <<"type">> => empty_type() }
                },
                #{ <<"presence">> => required, <<"type">> => empty_type() }
            }
        ),
    ?assertEqual(
        #{
            <<"a">> => 1,
            <<"b">> => 2,
            <<"c">> => #{ <<"visible">> => true }
        },
        apply_schema(
            Schema,
            #{
                <<"a">> => 1,
                <<"b">> => 2,
                <<"c">> => #{
                    <<"visible">> => true,
                    <<"priv">> => #{ <<"secret">> => true }
                },
                <<"priv">> => #{ <<"secret">> => true }
            },
            #{}
        )
    ).

wildcard_projection_omits_unset_masks_test() ->
    Schema =
        message_type(
            {
                #{},
                #{ <<"presence">> => optional, <<"type">> => empty_type() }
            }
        ),
    Msg = #{ <<"visible">> => <<"ok">>, <<"masked">> => <<"unset">> },
    ?assertEqual(#{ <<"visible">> => <<"ok">> }, apply_schema(Schema, Msg, #{})).

default_handler_uses_resolved_function_schema_test() ->
    Func = fun default_schema_fun/4,
    ?assertEqual(
        default_schema,
        select_schema(
            Func,
            <<"requested-key">>,
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
            <<"requested-key">>,
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
    ?assertEqual(#{ <<"a">> => 1 }, apply_schema(Schema, Msg, #{})).

request_overlay_marker_is_replacement_test() ->
    ReturnSchema =
        message_type(
            {
                #{
                    <<"...">> =>
                        #{
                            <<"presence">> => optional,
                            <<"type">> => literal_type(<<"request">>)
                        }
                },
                none
            }
        ),
    {ok, Ctx} =
        vary(
            #{
                <<"schema">> => [empty_type(), empty_type(), ReturnSchema],
                <<"base">> => #{},
                <<"request">> => #{ <<"path">> => <<"x">> }
            },
            #{}
        ),
    ?assertEqual(none, maps:get(<<"normalizer">>, Ctx)).

vary_generates_schema_dependency_tree_test() ->
    BaseSchema =
        message_type(
            {
                #{
                    <<"x">> => #{ <<"presence">> => required, <<"type">> => empty_type() },
                    <<"y">> => #{ <<"presence">> => required, <<"type">> => empty_type() }
                },
                none
            }
        ),
    ReqSchema =
        message_type(
            {
                #{
                    <<"add">> => #{ <<"presence">> => required, <<"type">> => empty_type() }
                },
                none
            }
        ),
    Base = #{ <<"device">> => <<"message@1.0">>, <<"x">> => 1, <<"y">> => 2 },
    Req = #{ <<"path">> => <<"add-x">>, <<"add">> => 3 },
    {ok, Ctx} =
        vary(
            #{
                <<"schema">> => [BaseSchema, ReqSchema, any_type()],
                <<"base">> => Base,
                <<"request">> => Req
            },
            #{}
        ),
    BaseID = hb_message:id(Base, all, #{}),
    ReqID = hb_message:id(Req, all, #{}),
    ?assertEqual(
        #{ <<"device">> => <<"message@1.0">>, <<"x">> => 1, <<"y">> => 2 },
        maps:get(<<"varied-base">>, Ctx)
    ),
	    ?assertEqual(#{ <<"path">> => <<"add-x">>, <<"add">> => 3 }, maps:get(<<"varied-request">>, Ctx)),
	    ?assertEqual(<<"schema-declared">>, maps:get(<<"claim-level">>, Ctx)),
	    ?assertEqual(
	        #{
	            <<"base">> => #{
                <<"device">> => <<BaseID/binary, "/device">>,
                <<"x">> => <<BaseID/binary, "/x">>,
                <<"y">> => <<BaseID/binary, "/y">>
            },
            <<"request">> => #{
                <<"path">> => <<ReqID/binary, "/path">>,
                <<"add">> => <<ReqID/binary, "/add">>
            }
        },
        maps:get(<<"dependencies">>, Ctx)
    ).

identity_vary_generates_dependency_tree_test() ->
    Base = #{ <<"device">> => <<"message@1.0">>, <<"x">> => 1 },
    Req = #{ <<"path">> => <<"x">> },
    {ok, Ctx} = vary(#{ <<"base">> => Base, <<"request">> => Req }, #{}),
    BaseID = hb_message:id(Base, all, #{}),
    ReqID = hb_message:id(Req, all, #{}),
	    ?assertEqual(Base, maps:get(<<"varied-base">>, Ctx)),
	    ?assertEqual(Req, maps:get(<<"varied-request">>, Ctx)),
	    ?assertEqual(<<"schema-declared">>, maps:get(<<"claim-level">>, Ctx)),
	    ?assertEqual(
	        #{
	            <<"base">> => #{
                <<"device">> => <<BaseID/binary, "/device">>,
                <<"x">> => <<BaseID/binary, "/x">>
            },
            <<"request">> => #{ <<"path">> => <<ReqID/binary, "/path">> }
        },
        maps:get(<<"dependencies">>, Ctx)
    ).

identity_vary_records_unset_as_dependency_not_witness_test() ->
    Base = #{
        <<"device">> => <<"message@1.0">>,
        <<"visible">> => <<"ok">>,
        <<"masked">> => <<"unset">>
    },
    Req = #{ <<"path">> => <<"keys">> },
    {ok, Ctx} = vary(#{ <<"base">> => Base, <<"request">> => Req }, #{}),
    BaseID = hb_message:id(Base, all, #{}),
    VariedBase = maps:get(<<"varied-base">>, Ctx),
    ?assertEqual(false, maps:is_key(<<"masked">>, VariedBase)),
    ?assertEqual({error, not_found}, hb_ao:resolve(VariedBase, <<"masked">>, #{ <<"hashpath">> => ignore })),
    ?assertEqual(
        #{
            <<"status">> => unset,
            <<"origin">> => <<BaseID/binary, "/masked">>,
            <<"path">> => <<"masked">>
        },
        maps:get(<<"masked">>, maps:get(<<"base">>, maps:get(<<"dependencies">>, Ctx)))
    ).

identity_vary_records_nested_unset_as_dependency_not_witness_test() ->
    Parent = #{ <<"kept">> => <<"yes">>, <<"masked">> => <<"unset">> },
    Base = #{
        <<"device">> => <<"message@1.0">>,
        <<"...">> => Parent
    },
    Req = #{ <<"path">> => <<"keys">> },
    {ok, Ctx} = vary(#{ <<"base">> => Base, <<"request">> => Req }, #{}),
    VariedBase = maps:get(<<"varied-base">>, Ctx),
    ?assertEqual(false, maps:is_key(<<"masked">>, VariedBase)),
    ?assertEqual({error, not_found}, hb_ao:resolve(VariedBase, <<"masked">>, #{ <<"hashpath">> => ignore })),
    ?assertEqual({ok, <<"yes">>}, hb_ao:resolve(VariedBase, <<"kept">>, #{ <<"hashpath">> => ignore })),
    ?assertMatch(
        #{ <<"status">> := unset },
        maps:get(
            <<"masked">>,
            maps:get(<<"base">>, maps:get(<<"dependencies">>, Ctx))
        )
    ).

identity_vary_direct_value_shadows_inherited_unset_dependency_test() ->
    Parent = #{ <<"k">> => <<"unset">> },
    Base = #{
        <<"device">> => <<"message@1.0">>,
        <<"k">> => 1,
        <<"...">> => Parent
    },
    Req = #{ <<"path">> => <<"k">> },
    {ok, Ctx} = vary(#{ <<"base">> => Base, <<"request">> => Req }, #{}),
    BaseID = hb_message:id(Base, all, #{}),
    ?assertEqual(1, maps:get(<<"k">>, maps:get(<<"varied-base">>, Ctx))),
    ?assertEqual(
        <<BaseID/binary, "/k">>,
        maps:get(<<"k">>, maps:get(<<"base">>, maps:get(<<"dependencies">>, Ctx)))
    ).

identity_vary_recursively_projects_nested_active_surfaces_test() ->
    Parent = #{ <<"a">> => 1, <<"b">> => 1 },
    Base = #{
        <<"device">> => <<"message@1.0">>,
        <<"nested">> => #{ <<"b">> => 2, <<"...">> => Parent }
    },
    Req = #{ <<"path">> => <<"nested">> },
    {ok, Ctx} = vary(#{ <<"base">> => Base, <<"request">> => Req }, #{}),
    BaseID = hb_message:id(Base, all, #{}),
    ParentID = hb_message:id(Parent, all, #{}),
    VariedNested = maps:get(<<"nested">>, maps:get(<<"varied-base">>, Ctx)),
    NestedDeps = maps:get(<<"nested">>, maps:get(<<"base">>, maps:get(<<"dependencies">>, Ctx))),
    ?assertEqual(#{ <<"a">> => 1, <<"b">> => 2 }, VariedNested),
    ?assertEqual(false, maps:is_key(<<"...">>, VariedNested)),
    ?assertEqual(<<ParentID/binary, "/a">>, maps:get(<<"a">>, NestedDeps)),
    ?assertEqual(<<BaseID/binary, "/nested/b">>, maps:get(<<"b">>, NestedDeps)).

identity_vary_direct_nested_value_shadows_inherited_nested_unset_test() ->
    Parent = #{ <<"nested">> => #{ <<"masked">> => <<"unset">> } },
    Base = #{
        <<"device">> => <<"message@1.0">>,
        <<"nested">> => #{ <<"kept">> => <<"yes">> },
        <<"...">> => Parent
    },
    Req = #{ <<"path">> => <<"nested">> },
    {ok, Ctx} = vary(#{ <<"base">> => Base, <<"request">> => Req }, #{}),
    NestedDeps = maps:get(<<"nested">>, maps:get(<<"base">>, maps:get(<<"dependencies">>, Ctx))),
    ?assertEqual(
        #{ <<"kept">> => <<"yes">> },
        maps:get(<<"nested">>, maps:get(<<"varied-base">>, Ctx))
    ),
    ?assertEqual(false, maps:is_key(<<"masked">>, NestedDeps)).

hashpath_ignore_skips_identity_dependencies_test() ->
    Base = #{ <<"lazy">> => {link, <<"missing-id">>, #{}} },
    Req = #{ <<"path">> => <<"keys">> },
    {ok, Ctx} =
        vary(
            #{ <<"base">> => Base, <<"request">> => Req },
            #{ <<"hashpath">> => ignore }
        ),
    ?assertEqual(Base, maps:get(<<"varied-base">>, Ctx)),
    ?assertEqual(Req, maps:get(<<"varied-request">>, Ctx)),
    ?assertEqual(false, maps:is_key(<<"dependencies">>, Ctx)).

atom_hashpath_ignore_skips_identity_dependencies_test() ->
    Base = #{ <<"lazy">> => {link, <<"missing-id">>, #{}} },
    Req = #{ <<"path">> => <<"keys">> },
    {ok, Ctx} = vary(#{ <<"base">> => Base, <<"request">> => Req }, #{ hashpath => ignore }),
    ?assertEqual(false, maps:is_key(<<"dependencies">>, Ctx)).

	vary_generates_optional_missing_dependency_test() ->
	    BaseSchema =
	        message_type(
	            {
                #{
                    <<"x">> => #{ <<"presence">> => required, <<"type">> => empty_type() },
                    <<"missing">> => #{ <<"presence">> => optional, <<"type">> => empty_type() }
                },
                none
            }
        ),
    ReqSchema = message_type({#{}, none}),
    Base = #{ <<"device">> => <<"message@1.0">>, <<"x">> => 1 },
    Req = #{ <<"path">> => <<"set">> },
    {ok, Ctx} =
        vary(
            #{
                <<"schema">> => [BaseSchema, ReqSchema, any_type()],
                <<"base">> => Base,
                <<"request">> => Req
	            },
	            #{}
	        ),
	    BaseID = hb_message:id(Base, all, #{}),
	    ?assertEqual(
	        #{
	            <<"status">> => not_found,
	            <<"origin">> => <<BaseID/binary, "/missing">>,
	            <<"path">> => <<"missing">>
	        },
	        maps:get(
	            <<"missing">>,
	            maps:get(<<"base">>, maps:get(<<"dependencies">>, Ctx))
	        )
	    ).

	vary_records_coerced_positive_dependency_leaf_test() ->
	    BaseSchema = message_type({#{}, none}),
	    ReqSchema =
	        message_type(
	            {
	                #{
	                    <<"quantity">> =>
	                        #{
	                            <<"presence">> => required,
	                            <<"type">> => scalar_type(<<"integer">>)
	                        }
	                },
	                none
	            }
	        ),
	    Req = #{ <<"path">> => <<"transfer">>, <<"quantity">> => <<"3">> },
	    {ok, Ctx} =
	        vary(
	            #{
	                <<"schema">> => [BaseSchema, ReqSchema, any_type()],
	                <<"base">> => #{},
	                <<"request">> => Req
	            },
	            #{}
	        ),
	    ReqID = hb_message:id(Req, all, #{}),
	    ?assertEqual(3, maps:get(<<"quantity">>, maps:get(<<"varied-request">>, Ctx))),
	    ?assertEqual(
	        #{
	            <<"status">> => found,
	            <<"origin">> => <<ReqID/binary, "/quantity">>,
	            <<"observed">> => <<"3">>,
	            <<"value">> => 3
	        },
	        maps:get(<<"quantity">>, maps:get(<<"request">>, maps:get(<<"dependencies">>, Ctx)))
	    ).

	vary_generates_optional_unset_dependency_test() ->
    BaseSchema =
        message_type(
            {
                #{
                    <<"x">> => #{ <<"presence">> => required, <<"type">> => empty_type() },
                    <<"masked">> => #{ <<"presence">> => optional, <<"type">> => empty_type() }
                },
                none
            }
        ),
    ReqSchema = message_type({#{}, none}),
    Base = #{
        <<"device">> => <<"message@1.0">>,
        <<"x">> => 1,
        <<"masked">> => <<"unset">>,
        <<"...">> => #{ <<"masked">> => <<"ancestor">> }
    },
    Req = #{ <<"path">> => <<"set">> },
    {ok, Ctx} =
        vary(
            #{
                <<"schema">> => [BaseSchema, ReqSchema, any_type()],
                <<"base">> => Base,
                <<"request">> => Req
            },
            #{}
        ),
    BaseID = hb_message:id(Base, all, #{}),
    ?assertEqual(false, maps:is_key(<<"masked">>, maps:get(<<"varied-base">>, Ctx))),
    ?assertEqual(
        #{
            <<"status">> => unset,
            <<"origin">> => <<BaseID/binary, "/masked">>,
            <<"path">> => <<"masked">>
        },
        maps:get(
            <<"masked">>,
            maps:get(<<"base">>, maps:get(<<"dependencies">>, Ctx))
        )
    ).

vary_generates_inherited_optional_unset_dependency_test() ->
    BaseSchema =
        message_type(
            {
                #{
                    <<"x">> => #{ <<"presence">> => required, <<"type">> => empty_type() },
                    <<"masked">> => #{ <<"presence">> => optional, <<"type">> => empty_type() }
                },
                none
            }
        ),
    ReqSchema = message_type({#{}, none}),
    Parent = #{ <<"masked">> => <<"unset">> },
    Base = #{
        <<"device">> => <<"message@1.0">>,
        <<"x">> => 1,
        <<"...">> => Parent
    },
    Req = #{ <<"path">> => <<"set">> },
    {ok, Ctx} =
        vary(
            #{
                <<"schema">> => [BaseSchema, ReqSchema, any_type()],
                <<"base">> => Base,
                <<"request">> => Req
            },
            #{}
        ),
    ParentID = hb_message:id(Parent, all, #{}),
    ?assertEqual(false, maps:is_key(<<"masked">>, maps:get(<<"varied-base">>, Ctx))),
    ?assertEqual(
        #{
            <<"status">> => unset,
            <<"origin">> => <<ParentID/binary, "/masked">>,
            <<"path">> => <<"masked">>
        },
        maps:get(
            <<"masked">>,
            maps:get(<<"base">>, maps:get(<<"dependencies">>, Ctx))
        )
    ).

vary_generates_hashpath_inherited_optional_unset_dependency_test() ->
    hb:init(),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    BaseSchema =
        message_type(
            {
                #{
                    <<"x">> => #{ <<"presence">> => required, <<"type">> => empty_type() },
                    <<"masked">> => #{ <<"presence">> => optional, <<"type">> => empty_type() }
                },
                none
            }
        ),
    ReqSchema = message_type({#{}, none}),
    Parent = #{ <<"masked">> => <<"unset">> },
    {ok, ParentID} = hb_cache:write(Parent, Opts),
    HashpathAncestor = <<"AncestorBase/AncestorReq.", ParentID/binary>>,
    Base = #{
        <<"device">> => <<"message@1.0">>,
        <<"x">> => 1,
        <<"...">> => HashpathAncestor
    },
    Req = #{ <<"path">> => <<"set">> },
    {ok, Ctx} =
        vary(
            #{
                <<"schema">> => [BaseSchema, ReqSchema, any_type()],
                <<"base">> => Base,
                <<"request">> => Req
            },
            Opts
        ),
    ?assertEqual(false, maps:is_key(<<"masked">>, maps:get(<<"varied-base">>, Ctx))),
    ?assertEqual(
        #{
            <<"status">> => unset,
            <<"origin">> => <<HashpathAncestor/binary, "/masked">>,
            <<"path">> => <<"masked">>
        },
        maps:get(
            <<"masked">>,
            maps:get(<<"base">>, maps:get(<<"dependencies">>, Ctx))
        )
    ).

vary_generates_nested_inherited_origin_test() ->
    BaseSchema =
        message_type(
            {
                #{
                    <<"balance">> =>
                        #{
                            <<"presence">> => required,
                            <<"type">> =>
                                message_type(
                                    {
                                        #{
                                            <<"x">> =>
                                                #{
                                                    <<"presence">> => required,
                                                    <<"type">> => empty_type()
                                                }
                                        },
                                        none
                                    }
                                )
                        }
                },
                none
            }
        ),
    ReqSchema = message_type({#{}, none}),
    BalanceParent = #{ <<"x">> => 1 },
    BalanceParentID = hb_message:id(BalanceParent, all, #{}),
    Base = #{ <<"balance">> => #{ <<"...">> => BalanceParent } },
    Req = #{ <<"path">> => <<"balance">> },
    {ok, Ctx} =
        vary(
            #{
                <<"schema">> => [BaseSchema, ReqSchema, any_type()],
                <<"base">> => Base,
                <<"request">> => Req
            },
            #{}
        ),
    ?assertEqual(#{ <<"balance">> => #{ <<"x">> => 1 } }, maps:get(<<"varied-base">>, Ctx)),
    ?assertEqual(
        <<BalanceParentID/binary, "/x">>,
        maps:get(
            <<"x">>,
            maps:get(<<"balance">>, maps:get(<<"base">>, maps:get(<<"dependencies">>, Ctx)))
        )
    ).

vary_rejects_stale_private_hashpath_origin_test() ->
    hb:init(),
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Base0 = #{ <<"x">> => 1 },
    OtherBase = #{ <<"x">> => 2 },
    BaseID = hb_message:id(Base0, all, Opts),
    {ok, OtherBaseID} = hb_cache:write(OtherBase, Opts),
    Base = hb_private:set_priv(Base0, #{ <<"hashpath">> => OtherBaseID }),
    Req = #{ <<"path">> => <<"x">> },
    {ok, Ctx} = vary(#{ <<"base">> => Base, <<"request">> => Req }, Opts),
    ?assertEqual(
        <<BaseID/binary, "/x">>,
        maps:get(<<"x">>, maps:get(<<"base">>, maps:get(<<"dependencies">>, Ctx)))
    ).

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
        #{ <<"a">> => 1, <<"b">> => 2, <<"c">> => 1 },
        apply_schema(Schema, Msg, #{})
    ),
    ?assertEqual(
        #{ <<"a">> => 1, <<"b">> => 2, <<"c">> => 1 },
        apply_schema(Schema, Msg#{ <<"priv">> => #{ <<"secret">> => true } }, #{})
    ),
    ?assertEqual(
        #{ <<"a">> => 1, <<"b">> => #{ <<"visible">> => true }, <<"c">> => 1 },
        apply_schema(
            Schema,
            Msg#{
                <<"b">> => #{ <<"visible">> => true, <<"priv">> => #{ <<"secret">> => true } },
                <<"priv">> => #{ <<"secret">> => true }
            },
            #{}
        )
    ).
