%%% @doc Extract Dialyzer-style type information from AO-Core devices and apply
%%% a static `vary` transform to base and request messages.
-module(hb_types).
-export([extract/2, vary/5]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

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

%% @doc Extract the public type schema for a device.
extract(Device, _Opts) when is_map(Device) ->
    {error, {unsupported_device_type, Device}};
extract(Module, _Opts) when is_atom(Module) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            do_extract(Module);
        {error, Reason} ->
            {error, {module_not_loaded, Module, Reason}}
    end;
extract(Device, Opts) when is_binary(Device) ->
    case hb_ao_device:load(Device, Opts) of
        {ok, Module} -> extract(Module, Opts);
        Error -> Error
    end;
extract(Device, _Opts) ->
    {error, {unsupported_device_type, Device}}.

do_extract(Module) ->
    Beam = code:which(Module),
    case beam_lib:chunks(Beam, [abstract_code]) of
        {ok, {_, [{abstract_code, {_, Forms}}]}} ->
            TypeEnv = build_type_env(Forms),
            Specs = [ Attr || Attr = {attribute, _, spec, _} <- Forms ],
            KeySchemas =
                lists:foldl(
                    fun(Spec, Acc) ->
                        case spec_to_schema(Spec, TypeEnv) of
                            false -> Acc;
                            {Key, Schema} -> Acc#{ Key => Schema }
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
    message_type(#{});
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
parse_type({type, _, list, [Item]}, TypeEnv, VarEnv, Seen) ->
    #{
        <<"kind">> => <<"list">>,
        <<"item">> => parse_type(Item, TypeEnv, VarEnv, Seen)
    };
parse_type({type, _, nonempty_list, [Item]}, TypeEnv, VarEnv, Seen) ->
    #{
        <<"kind">> => <<"list">>,
        <<"item">> => parse_type(Item, TypeEnv, VarEnv, Seen),
        <<"nonempty">> => true
    };
parse_type({type, _, maybe_improper_list, [Item, Tail]}, TypeEnv, VarEnv, Seen) ->
    #{
        <<"kind">> => <<"list">>,
        <<"item">> => parse_type(Item, TypeEnv, VarEnv, Seen),
        <<"tail">> => parse_type(Tail, TypeEnv, VarEnv, Seen),
        <<"improper">> => true
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
parse_type(Other, _TypeEnv, _VarEnv, _Seen) -> unknown_type(Other).

field_presence(map_field_exact) -> required;
field_presence(map_field_assoc) -> optional;
field_presence(Other) -> normalize_name(Other).

key_name({atom, _, Atom}, _TypeEnv, _VarEnv, _Seen) ->
    normalize_name(Atom);
key_name({string, _, String}, _TypeEnv, _VarEnv, _Seen) ->
    hb_util:bin(String);
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
    lists:foldl(
        fun({Key, #{ <<"presence">> := Presence, <<"type">> := Type }}, Acc) ->
            case hb_maps:find(Key, Message, Opts) of
                {ok, Value} ->
                    Acc#{ Key => project_value(Type, Value, Opts) };
                error when Presence =:= required ->
                    throw({required_key_missing, Key});
                error when Presence =:= optional ->
                    Acc#{ Key => project_value(Type, undefined, Opts) };
                error ->
                    Acc
            end
        end,
        #{},
        maps:to_list(Keys)
    );
apply_schema(Type, Message, _Opts) ->
    case check_type(Type, Message) of
        true -> Message;
        false -> throw({invalid_type, Type, Message})
    end.

project_value(#{ <<"kind">> := <<"message">> } = Type, Value, Opts) ->
    apply_schema(Type, Value, Opts);
project_value(Type, Value, _Opts) ->
    case check_type(Type, Value) of
        true -> Value;
        false -> throw({invalid_type, Type, Value})
    end.

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
    % If the `_` key is set to `_`, then we maintain the presence of all keys.
    % Otherwise, we only maintain the presence of the keys that are set.
    #{
        <<"kind">> => <<"message">>,
        <<"keys">> => maps:without(['_'], AllKeys),
        <<"all">> => maps:get('_', AllKeys, false) == '_'
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
    ?assertEqual(#{}, VariedBase),
    ?assertEqual(#{ <<"slot">> => 1 }, VariedReq),
    ?event(
        debug_types,
        {vary_result,
            {varied_base, {explicit, VariedBase}},
            {varied_req, {explicit, VariedReq}}
        }
    ).

vary_throw_required_key_missing_test() ->
    Opts = test_opts(),
    ?assertThrow(
        {required_key_missing, <<"/slot">>},
        vary(<<"test-device@1.0">>, <<"compute">>, #{}, #{}, Opts)
    ).

vary_throw_required_key_wrong_type_test() ->
    Opts = test_opts(),
    ?assertThrow(
        {
            invalid_type,
                {key, <<"/slot">>},
                {value, <<"1">>},
                {expected_type, integer}
        },
        vary(
            <<"test-device@1.0">>,
            <<"compute">>,
            #{},
            #{ <<"slot">> => <<"1">> },
            Opts
        )
    ).

vary_throw_optional_key_wrong_type_test() ->
    Opts = test_opts(),
    ?assertThrow(
        {
            invalid_type,
                {key, <<"/already-seen">>},
                {value, false},
                {expected_type, []}
        },
        vary(
            <<"test-device@1.0">>,
            <<"compute">>,
            #{},
            #{ <<"already-seen">> => false, <<"slot">> => 1 },
            Opts
        )
    ).

successful_nested_vary_test() ->
    Opts = test_opts(),
    {ok, VariedBase, VariedReq} =
        vary(
            <<"test-device@1.0">>,
            <<"compute_nested">>,
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
        {required_key_missing, <<"/outer/slot">>},
        vary(
            <<"test-device@1.0">>,
            <<"compute_nested">>,
            #{},
            #{ <<"outer">> => #{ <<"not-slot">> => 1 }},
            Opts
        )
    ).

vary_throw_nested_key_wrong_type_test() ->
    Opts = test_opts(),
    ?assertThrow(
        {invalid_type, 
            {key, <<"/outer/slot">>},
            {value, <<"1">>},
            {expected_type, integer}
        },
        vary(
            <<"test-device@1.0">>,
            <<"compute_nested">>,
            #{},
            #{ <<"outer">> => #{ <<"slot">> => <<"1">> }},
            Opts
        )
    ).

vary_on_all_test() -> 
    Opts = test_opts(),
    {ok, VariedBase, VariedReq} =
        vary(
            <<"test-device@1.0">>,
            <<"compute_all">>,
            #{ <<"a">> => 1, <<"b">> => 2 },
            #{ <<"slot">> => 1 },
            Opts
        ),
    ?event(debug_types, {vary_result, {varied_base, VariedBase}, {varied_req, VariedReq}}),
    ?assertEqual(#{ <<"a">> => 1, <<"b">> => 2 }, VariedBase),
    ?assertEqual(#{ <<"slot">> => 1 }, VariedReq).

vary_on_all_nested_test() -> 
    Opts = test_opts(),
    {ok, VariedBase, VariedReq} =
        vary(
            <<"test-device@1.0">>,
            <<"compute_all">>,
            #{ <<"a">> => 1, <<"b">> => 2, <<"outer">> => #{ <<"c">> => 3, <<"d">> => 4 } },
            #{ <<"slot">> => 1 },
            Opts
        ),
    ?event(debug_types, {vary_result, {varied_base, VariedBase}, {varied_req, VariedReq}}),
    ?assertEqual(
        #{ 
            <<"a">> => 1, 
            <<"b">> => 2, 
            <<"outer">> => #{ <<"c">> => 3, <<"d">> => 4 } 
        },
        VariedBase
    ),
    ?assertEqual(#{ <<"slot">> => 1 }, VariedReq).