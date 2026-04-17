%%% @doc Extract Dialyzer-style type information from AO-Core devices and apply
%%% a static `vary` transform to base and request messages.
-module(hb_types).
-export([extract/2, vary/5, format/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

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

%% @doc Apply a device's declared base/request schemas to the messages that will
%% participate in one AO-Core key execution.
vary(Device, Key, Base, Request, Opts) ->
    case extract(Device, Opts) of
        {ok, #{ <<"keys">> := KeySchemas }} ->
            case maps:get(normalize_name(Key), KeySchemas, undefined) of
                undefined ->
                    {ok, Base, Request};
                Schema ->
                    {ok,
                        apply_schema(maps:get(<<"base">>, Schema, any_type()), Base, Opts),
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

%% @doc Render a structured type into a compact human-readable string.
format(Type) when is_map(Type) ->
    hb_util:bin(do_format(Type));
format(Other) ->
    hb_util:bin(io_lib:format("~tp", [Other])).

do_extract(Module) ->
    Beam = code:which(Module),
    case beam_lib:chunks(Beam, [abstract_code]) of
        {ok, {_, [{abstract_code, {_, Forms}}]}} ->
            TypeEnv = build_type_env(Forms),
            Specs = [Attr || Attr = {attribute, _, spec, _} <- Forms],
            KeySchemas =
                lists:foldl(
                    fun(Spec, Acc) ->
                        case spec_to_schema(Spec, TypeEnv) of
                            false ->
                                Acc;
                            {Key, Schema} ->
                                merge_schema(Key, Schema, Acc)
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

merge_schema(Key, Schema = #{ <<"arity">> := Arity }, Acc) ->
    case maps:get(Key, Acc, undefined) of
        undefined ->
            Acc#{ Key => Schema };
        #{ <<"arity">> := ExistingArity } ->
            case schema_score(Arity) >= schema_score(ExistingArity) of
                true -> Acc#{ Key => Schema };
                false -> Acc
            end
    end.

schema_score(3) -> 40;
schema_score(2) -> 30;
schema_score(1) -> 20;
schema_score(0) -> 10;
schema_score(Arity) when is_integer(Arity) -> Arity;
schema_score(_) -> 0.

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

spec_to_schema({attribute, _, spec, {{Name, Arity}, [Spec | _]}}, TypeEnv) ->
    {Args, Return} = parse_fun_spec(Spec, TypeEnv),
    {normalize_name(Name),
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
    {lists:map(fun(Arg) -> parse_type(Arg, TypeEnv, #{}, []) end, Args),
        parse_type(Ret, TypeEnv, #{}, [])};
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
    map_type(#{});
parse_type({type, _, map, Fields}, TypeEnv, VarEnv, Seen) ->
    map_type(
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
    Members1 = lists:map(fun(Member) -> parse_type(Member, TypeEnv, VarEnv, Seen) end, Members),
    case is_boolean_union(Members1) of
        true -> boolean_type();
        false -> #{ <<"kind">> => <<"union">>, <<"members">> => Members1 }
    end;
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
parse_type({atom, _, Atom}, _TypeEnv, _VarEnv, _Seen) -> literal_type(normalize_atom_literal(Atom));
parse_type({integer, _, Int}, _TypeEnv, _VarEnv, _Seen) -> literal_type(Int);
parse_type({char, _, Char}, _TypeEnv, _VarEnv, _Seen) -> literal_type(<<Char/utf8>>);
parse_type({string, _, String}, _TypeEnv, _VarEnv, _Seen) -> literal_type(hb_util:bin(String));
parse_type({nil, _}, _TypeEnv, _VarEnv, _Seen) -> literal_type([]);
parse_type(Other, _TypeEnv, _VarEnv, _Seen) -> unknown_type(Other).

field_presence(map_field_exact) -> <<"required">>;
field_presence(map_field_assoc) -> <<"optional">>;
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

apply_schema(#{ <<"kind">> := <<"map">>, <<"keys">> := Keys }, Message, Opts)
        when is_map(Message) ->
    Selected =
        lists:foldl(
            fun({Key, #{ <<"presence">> := Presence, <<"type">> := Type }}, Acc) ->
                case hb_maps:find(Key, Message, Opts) of
                    {ok, Value} ->
                        Acc#{ Key => project_value(Type, Value, Opts) };
                    error when Presence =:= <<"required">> ->
                        throw({required_key_missing, Key});
                    error ->
                        Acc
                end
            end,
            #{},
            maps:to_list(Keys)
        ),
    hb_message:normalize_commitments(Selected, Opts, verify);
apply_schema(Type, Message, _Opts) ->
    case check_type(Type, Message) of
        true -> Message;
        false -> throw({invalid_type, Type, Message})
    end.

project_value(#{ <<"kind">> := <<"map">> } = Type, Value, Opts) ->
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
check_type(#{ <<"kind">> := <<"map">> }, Value) -> is_map(Value);
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

do_format(#{ <<"kind">> := <<"any">> }) ->
    <<"any()">>;
do_format(#{ <<"kind">> := <<"integer">> }) ->
    <<"integer()">>;
do_format(#{ <<"kind">> := <<"non-neg-integer">> }) ->
    <<"non_neg_integer()">>;
do_format(#{ <<"kind">> := <<"pos-integer">> }) ->
    <<"pos_integer()">>;
do_format(#{ <<"kind">> := <<"neg-integer">> }) ->
    <<"neg_integer()">>;
do_format(#{ <<"kind">> := <<"float">> }) ->
    <<"float()">>;
do_format(#{ <<"kind">> := <<"number">> }) ->
    <<"number()">>;
do_format(#{ <<"kind">> := <<"binary">> }) ->
    <<"binary()">>;
do_format(#{ <<"kind">> := <<"bitstring">> }) ->
    <<"bitstring()">>;
do_format(#{ <<"kind">> := <<"boolean">> }) ->
    <<"boolean()">>;
do_format(#{ <<"kind">> := <<"atom">> }) ->
    <<"atom()">>;
do_format(#{ <<"kind">> := <<"pid">> }) ->
    <<"pid()">>;
do_format(#{ <<"kind">> := <<"literal">>, <<"value">> := Value }) ->
    hb_util:bin(io_lib:format("~tp", [Value]));
do_format(#{ <<"kind">> := <<"alias">>, <<"name">> := Name }) ->
    <<Name/binary, "()">>;
do_format(#{ <<"kind">> := <<"variable">>, <<"name">> := Name }) ->
    <<Name/binary>>;
do_format(#{ <<"kind">> := <<"remote">>, <<"module">> := Mod, <<"name">> := Name, <<"args">> := [] }) ->
    <<Mod/binary, ":", Name/binary, "()">>;
do_format(#{ <<"kind">> := <<"remote">>, <<"module">> := Mod, <<"name">> := Name, <<"args">> := Args }) ->
    <<Mod/binary, ":", Name/binary, "(", (join_formatted(Args))/binary, ")">>;
do_format(#{ <<"kind">> := <<"list">>, <<"item">> := Item }) ->
    <<"[", (do_format(Item))/binary, "]">>;
do_format(#{ <<"kind">> := <<"tuple">>, <<"items">> := Items }) ->
    <<"{", (join_formatted(Items))/binary, "}">>;
do_format(#{ <<"kind">> := <<"union">>, <<"members">> := Members }) ->
    join_formatted(Members, <<" | ">>);
do_format(#{ <<"kind">> := <<"range">>, <<"min">> := Min, <<"max">> := Max }) ->
    hb_util:bin(io_lib:format("~p..~p", [Min, Max]));
do_format(#{ <<"kind">> := <<"map">>, <<"keys">> := Keys }) when map_size(Keys) =:= 0 ->
    <<"map()">>;
do_format(#{ <<"kind">> := <<"map">>, <<"keys">> := Keys }) ->
    Inner =
        join_binaries(
            lists:map(
                fun({Key, #{ <<"presence">> := Presence, <<"type">> := Type }}) ->
                    Sep =
                        case Presence of
                            <<"required">> -> <<" := ">>;
                            _ -> <<" => ">>
                        end,
                    <<Key/binary, Sep/binary, (do_format(Type))/binary>>
                end,
                maps:to_list(Keys)
            ),
            <<", ">>
        ),
    <<"#{", Inner/binary, "}">>;
do_format(Other) ->
    hb_util:bin(io_lib:format("~tp", [Other])).

join_formatted(Types) ->
    join_formatted(Types, <<", ">>).
join_formatted(Types, Separator) ->
    join_binaries(lists:map(fun do_format/1, Types), Separator).

join_binaries([], _Separator) -> <<>>;
join_binaries([Only], _Separator) -> Only;
join_binaries([Bin | Rest], Separator) ->
    <<Bin/binary, Separator/binary, (join_binaries(Rest, Separator))/binary>>.

normalize_name(Name) when is_binary(Name) ->
    binary:replace(hb_util:bin(Name), <<"_">>, <<"-">>, [global]);
normalize_name(Name) when is_atom(Name) ->
    normalize_name(atom_to_binary(Name, utf8));
normalize_name(Name) when is_list(Name) ->
    normalize_name(hb_util:bin(Name));
normalize_name(Name) when is_integer(Name) ->
    integer_to_binary(Name);
normalize_name(Name) ->
    hb_util:bin(io_lib:format("~tp", [Name])).

var_name({var, _, Name}) -> Name;
var_name(Name) -> Name.

normalize_atom_literal(true) -> true;
normalize_atom_literal(false) -> false;
normalize_atom_literal(undefined) -> <<"undefined">>;
normalize_atom_literal(Atom) -> normalize_name(Atom).

literal_value(#{ <<"kind">> := <<"literal">>, <<"value">> := Value }) -> Value;
literal_value(Other) -> do_format(Other).

is_boolean_union(Members) ->
    lists:sort(Members) =:=
        lists:sort([literal_type(true), literal_type(false)]).

any_type() -> #{ <<"kind">> => <<"any">> }.
boolean_type() -> #{ <<"kind">> => <<"boolean">> }.
scalar_type(Name) -> #{ <<"kind">> => Name }.
literal_type(Value) -> #{ <<"kind">> => <<"literal">>, <<"value">> => Value }.
alias_type(Name) -> #{ <<"kind">> => <<"alias">>, <<"name">> => normalize_name(Name) }.
variable_type(Name) -> #{ <<"kind">> => <<"variable">>, <<"name">> => normalize_name(Name) }.
map_type(Keys) -> #{ <<"kind">> => <<"map">>, <<"keys">> => Keys }.
unknown_type(Other) -> #{ <<"kind">> => <<"unknown">>, <<"ast">> => hb_util:bin(io_lib:format("~tp", [Other])) }.

%%% Tests

normalize_name_test() ->
    ?assertEqual(<<"reply-to">>, normalize_name(reply_to)).

format_map_type_test() ->
    Type =
        #{
            <<"kind">> => <<"map">>,
            <<"keys">> =>
                #{
                    <<"slot">> =>
                        #{
                            <<"presence">> => <<"required">>,
                            <<"type">> => #{ <<"kind">> => <<"integer">> }
                        }
                }
        },
    ?assertEqual(<<"#{slot := integer()}">>, format(Type)).

extract_loaded_module_test() ->
    ?assertMatch(
        {ok, #{ <<"module">> := <<"dev_meta">>, <<"keys">> := #{}, <<"types">> := #{} }},
        extract(dev_meta, #{})
    ).
