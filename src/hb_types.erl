%%% @doc Type extraction tooling for AO-Core devices in HyperBEAM.
-module(hb_types).
-export([extract/2, vary/5]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Apply device's type-spec information to a base and request prior to
%% AO-Core execution.
vary(Device, Key, Base, Request, Opts) ->
    {ok, #{ keys := KeySchemas, types := _Types }} = extract(Device, Opts),
    ?event(debug_types, {key_schemas, KeySchemas}),
    Schema = maps:get(hb_util:atom(Key), KeySchemas, undefined),
    ?event(debug_types, {schema, {key, Key}, {schema, Schema}}),
    case Schema of
        undefined -> {ok, Base, Request};
        #{ base := BaseSchema, request := RequestSchema } ->
            ?event(
                debug_types,
                {vary,
                    {base_schema, {explicit, BaseSchema}},
                    {request_schema, {explicit, RequestSchema}}
                }
            ),
            {
                ok,
                apply_schema(BaseSchema, Base, Opts),
                apply_schema(RequestSchema, Request, Opts)
            }
    end.

%% @doc Apply a schema to a message, throwing an error if a required key is
%% missing.
apply_schema(Schema, MessageID, Opts) ->
    hb_maps:filtermap(
        fun(Key, {IsRequired, Type}) ->
            IsNestedMessage = hb_link:is_link_key(Key),
            case hb_cache:read(<<MessageID/binary, "/", Key/binary>>, Opts) of
                {ok, Value} when IsNestedMessage ->
                    % The child is itself a message, so we recursively
                    % apply the schema to it.
                    {true, apply_schema(Type, Value, Opts)};
                {ok, Value} ->
                    case check_type(Type, Value) of
                        true -> {true, Value};
                        false ->
                            throw(
                                {invalid_type,
                                    {key, Key},
                                    {value, Value},
                                    {expected_type, Type}
                                }
                            )
                    end;
                not_found when IsRequired == optional ->
                    false;
                not_found when IsRequired == required ->
                    throw({required_key_missing, Key})
            end
        end,
        Schema,
        Opts
    ).

%% @doc Returns a message containing a pair of key name and key dependencies for
%% each key specified in a device.
extract(Device, Opts) when is_binary(Device) ->
    extract(hb_util:ok(hb_ao_device:load(Device, Opts)), Opts);
extract(Map, _Opts) when is_map(Map) ->
    {error, unsupported_device_type};
extract(Module, _Opts) when is_atom(Module) ->
    maybe
        BEAM = code:which(Module),
        false ?= BEAM == non_existing,
        {ok, {_, [{abstract_code, {_, Forms}}]}} ?=
            beam_lib:chunks(BEAM, [abstract_code]),
        SpecAttrs = [ Attr || Attr = {attribute, _, spec, _} <- Forms ],
        TypeAttrs =
            [
                Attr
            ||
                Attr = {attribute, _, Tag, _} <- Forms,
                Tag =:= type orelse Tag =:= opaque
            ],
        ?event({types, {explicit, TypeAttrs}}),
        ?event({specs, {explicit, SpecAttrs}}),
        Types = maps:from_list(lists:map(fun parse_type/1, TypeAttrs)),
        KeySchemas =
            maps:from_list(
                [spec_to_schema(Spec, Types) || Spec <- SpecAttrs]
            ),
        ?event({key_schemas, KeySchemas}),
        {ok, #{ keys => KeySchemas, types => Types }}
    end.

%% @doc Convert an Erlang function spec into a key schema.
%% TODO: Lookup types from the second argument if referenced in specs.
spec_to_schema({attribute, {_Line, _Col}, spec, {{Name, _}, [Spec|_]}}, _Types) ->
    ?event({spec_ast, Spec}),
    {Args, Ret} = parse_annotation(Spec),
    ?event({args, Args}),
    Base = parse_type(maybe_nth(1, Args)),
    ?event({base, {explicit, Base}}),
    Request = parse_type(maybe_nth(2, Args)),
    ?event({request, {explicit, Request}}),
    Opts = parse_type(maybe_nth(3, Args)),
    ?event({opts, {explicit, Opts}}),
    Schema = #{ base => Base, request => Request, opts => Opts, return => Ret },
    {Name, Schema}.

%% @doc Get the nth element of a list or return undefined if the list is too
%% short.
maybe_nth(_, []) -> undefined;
maybe_nth(1, [H|_]) -> H;
maybe_nth(N, [_|T]) -> maybe_nth(N-1, T).

%% @doc Extract AO-Core type specifications from a function annotation.
parse_annotation({type,_, 'fun', [{type, _, product, Args}, RetSpec]}) ->
    {BinStatus, Ret} = parse_type(RetSpec),
    {Args, {hb_util:atom(BinStatus), Ret}};
parse_annotation(Other) ->
    error({unexpected_spec_ast, Other}).

%% @doc Extract AO-Core type specifications from Erlang function spec ASTs.
parse_type({type, _, list, List}) ->
    lists:map(fun parse_type/1, List);
parse_type({type, _, tuple, TupleElements}) ->
    list_to_tuple(lists:map(fun parse_type/1, TupleElements));
parse_type({type, _, map, any}) -> #{};
parse_type({type, _, map, Fields}) ->
    #{
        (parse_type(K)) => {optional(Optional), parse_type(V)}
    ||
        {type, _, Optional, [K,V]} <- Fields
    };
parse_type({type, _, TypeName, []}) -> TypeName;
parse_type({atom, _, Atom}) -> hb_util:atom_to_key(Atom);
parse_type(Other) -> {unknown_type, Other}.

%% @doc Turn AST map associativity types into schema optional/required flags.
optional(map_field_assoc) -> optional;
optional(map_field_exact) -> required.

check_type([], Value) ->
    is_list(Value);
check_type(integer, Value) ->
    is_integer(Value);
check_type(any, _) -> true;
check_type(_, _) -> false.

%%% Tests

extract_test() ->
    Res = extract(<<"test-device@1.0">>, #{}),
    ?event({extraction_result, Res}),
    ?assertMatch(
        {ok, #{ keys := #{}, types := #{}}},
        Res
    ).
successful_vary_test() ->
    Base = #{},
    Req = #{ <<"slot">> => 1 },
    Opts = #{},
    Res = vary(<<"test-device@1.0">>, <<"compute">>, Base, Req, Opts),
    ?event({vary_result, Res}).

vary_throw_required_key_missing_test() ->
    Base = #{},
    Req = #{},
    Opts = #{},
    ?assertThrow(
        {required_key_missing, <<"slot">>},
        vary(<<"test-device@1.0">>, <<"compute">>, Base, Req, Opts)
    ).
vary_throw_required_key_wrong_type_test() ->
    Base = #{},
    Req = #{ <<"slot">> => <<"1">> },
    Opts = #{},
    ?assertThrow(
        {invalid_type, <<"slot">>, <<"1">>},
        vary(<<"test-device@1.0">>, <<"compute">>, Base, Req, Opts)
    ).

vary_throw_optional_key_wrong_type_test() ->
    Base = #{ <<"already-seen">> => false },
    Req = #{ <<"slot">> => 1 },
    Opts = #{},
    ?assertThrow(
        {invalid_type, <<"already-seen">>, false},
        vary(<<"test-device@1.0">>, <<"compute">>, Base, Req, Opts)
    ).

successful_nested_vary_test() ->
    Base = #{},
    Req = #{ <<"outer">> => #{ <<"slot">> => 1 }},
    Opts = #{},
    Res = vary(<<"test-device@1.0">>, <<"compute_nested">>, Base, Req, Opts),
    ?event({vary_result, Res}).

vary_throw_nested_key_missing_test() ->
    Base = #{},
    Req = #{ <<"outer">> => #{ <<"not-slot">> => 1 }},
    Opts = #{},
    ?assertThrow(
        {required_key_missing, <<"slot">>},
        vary(<<"test-device@1.0">>, <<"compute_nested">>, Base, Req, Opts)
    ).

vary_throw_nested_key_wrong_type_test() ->
    Base = #{},
    Req = #{ <<"outer">> => #{ <<"slot">> => <<"1">> }},
    Opts = #{},
    ?assertThrow(
        {invalid_type, <<"slot">>, <<"1">>},
        vary(<<"test-device@1.0">>, <<"compute_nested">>, Base, Req, Opts)
    ).