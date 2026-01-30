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
    Schema = 
        try 
            maps:get(hb_util:atom(Key), KeySchemas, undefined)
        catch _:_ -> undefined
        end,
    ?event(debug_types, {schema, {key, Key}, {schema, Schema}}),
    case Schema of
        undefined ->
            {
                ok,
                hb_util:ok(hb_cache_micro:read(Base, Opts)),
                if ?IS_ID(Request) ->
                    hb_util:ok(hb_cache_micro:read(Request, Opts));
                true -> Request
                end
            };
        #{ base := BaseSchema, request := RequestSchema } ->
            ?event(
                debug_types,
                {vary,
                    {base, {explicit, Base}},
                    {base_schema, {explicit, BaseSchema}},
                    {request, {explicit, Request}},
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
    apply_schema(Schema, MessageID, Opts, MessageID).
apply_schema(Schema, Message, Opts, PathRef) when is_map(Message) ->
    %%% TODO: This should be hb_ao_micro:get(<<"id">>, Message, Opts)
    %%% once implemented
    {ok, MessageID} = hb_cache_micro:write(Message, Opts),
    apply_schema(Schema, MessageID, Opts, PathRef);
apply_schema(any, MessageID, Opts, PathRef) ->
    ?event(debug_types, {applying_any_schema, {message_id, {explicit, MessageID}}}),
    Message = 
        hb_cache_micro:with_only_committed(
            hb_util:ok(hb_cache_micro:read(MessageID, Opts)),
            Opts
        ),
    ?event(debug_types, {applying_any_message, {message, Message}}),
    hb_maps:filtermap(
        fun(Key, _) ->
            CacheKey = <<MessageID/binary, "/", Key/binary>>,
            ?event(debug_types, {cache_read, {cache_key, CacheKey}}),
            case hb_ao_micro:resolve(CacheKey, Opts) of
                {ok, Value} when is_map(Value) ->
                    ?event(
                        debug_types,
                        {applying_schema_to_nested_message, {value, Value}},
                        Opts
                    ),
                    {
                        true,
                        apply_schema(
                            any,
                            Value,
                            Opts,
                            <<PathRef/binary, "/", Key/binary>>
                        )
                    };
                {ok, Value} ->
                    ?event(debug_types, {found_value, {value, Value}}),
                    {true, Value}
            end
        end,
        Message,
        Opts
    );
apply_schema(Schema, MessageID, Opts, PathRef) ->
    ?event(debug_types, {applying_schema, {schema, Schema}, {message_id, MessageID}, {ref, PathRef}}),
    hb_maps:filtermap(
        fun(Key, {IsRequired, Type}) ->
            CacheKey = <<MessageID/binary, "/", Key/binary>>,
            ?event(debug_types, {cache_read, {cache_key, CacheKey}}),
            case hb_ao_micro:resolve(CacheKey, Opts) of
                {ok, Value} when is_map(Value) ->
                    ?event(
                        debug_types,
                        {applying_schema_to_nested_message, {value, Value}},
                        Opts
                    ),
                    {
                        true,
                        apply_schema(
                            Type,
                            Value,
                            Opts,
                            <<PathRef/binary, "/", Key/binary>>
                        )
                    };
                {ok, Value} ->
                    ?event(debug_types, {found_value, {value, Value}}),
                    case check_type(Type, Value) of
                        true -> {true, Value};
                        false ->
                            ?event(debug_types, {throwing_invalid_type,
                                {key, Key},
                                {list, <<PathRef/binary, "/", Key/binary>>}
                            }),
                            throw(
                                {invalid_type,
                                    {key, <<PathRef/binary, "/", Key/binary>>},
                                    {value, Value},
                                    {expected_type, Type}
                                }
                            )
                    end;
                {error, not_found} when IsRequired == optional ->
                    false;
                {error, not_found} when IsRequired == required ->
                    throw(
                        {
                            required_key_missing, 
                            <<PathRef/binary, "/", Key/binary>>
                        }
                    )
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
test_opts() ->
    application:ensure_all_started(hb),
    #{
        store => 
            [
                hb_test_utils:test_store(hb_store_lmdb), 
                hb_test_utils:test_store(hb_store_preloaded)
            ],
        priv_wallet => hb:wallet()
    }.

extract_test() ->
    Res = extract(<<"test-device@1.0">>, #{}),
    ?event({extraction_result, Res}),
    ?assertMatch(
        {ok, #{ keys := #{}, types := #{}}},
        Res
    ).
successful_vary_test() ->
    Opts = test_opts(),
    {ok, BaseID} = hb_cache_micro:write(#{ <<"unused">> => 1 }, Opts),
    {ok, ReqID} = hb_cache_micro:write(#{ <<"slot">> => 1 }, Opts),
    {ok, VariedBase, VariedReq} =
        vary(<<"test-device@1.0">>, <<"compute">>, BaseID, ReqID, Opts),
    ?assertEqual(#{}, VariedBase),
    ?assertEqual(#{ <<"slot">> => 1 }, VariedReq),
    ?event(debug_types, {vary_result, {varied_base, {explicit, VariedBase}}, {varied_req, {explicit, VariedReq}}}).

vary_throw_required_key_missing_test() ->
    Opts = test_opts(),
    {ok, BaseID} = hb_cache_micro:write(#{}, Opts),
    {ok, ReqID} = hb_cache_micro:write(#{}, Opts),
    ExpectedErrorPath = <<BaseID/binary, "/slot">>,
    ?assertThrow(
        {required_key_missing, ExpectedErrorPath},
        vary(<<"test-device@1.0">>, <<"compute">>, BaseID, ReqID, Opts)
    ).
vary_throw_required_key_wrong_type_test() ->
    Opts = test_opts(),
    {ok, BaseID} = hb_cache_micro:write(#{}, Opts),
    {ok, ReqID} = hb_cache_micro:write(#{ <<"slot">> => <<"1">> }, Opts),
    ExpectedErrorPath = <<ReqID/binary, "/slot">>,
    ?assertThrow(
        {
            invalid_type,
                {key, ExpectedErrorPath},
                {value, <<"1">>},
                {expected_type, integer}
        },
        vary(<<"test-device@1.0">>, <<"compute">>, BaseID, ReqID, Opts)
    ).

vary_throw_optional_key_wrong_type_test() ->
    Opts = test_opts(),
    {ok, BaseID} = hb_cache_micro:write(#{ <<"already-seen">> => false }, Opts),
    {ok, ReqID} = hb_cache_micro:write(#{ <<"slot">> => 1 }, Opts),
    ExpectedErrorPath = <<BaseID/binary, "/already-seen">>,
    ?assertThrow(
        {
            invalid_type,
                {key, ExpectedErrorPath},
                {value, false},
                {expected_type, []}
        },
        vary(<<"test-device@1.0">>, <<"compute">>, BaseID, ReqID, Opts)
    ).

successful_nested_vary_test() ->
    Opts = test_opts(),
    {ok, BaseID} = hb_cache_micro:write(#{}, Opts),
    {ok, ReqID} = 
        hb_cache_micro:write(
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
    {ok, VariedBase, VariedReq} =
        vary(<<"test-device@1.0">>, <<"compute_nested">>, BaseID, ReqID, Opts),
    ?event(debug_types, {vary_result, {varied_base, VariedBase}, {varied_req, VariedReq}}),
    ?assertEqual(#{}, VariedBase),
    ?assertEqual(
        #{ <<"outer">> => #{ <<"slot">> => 1 }},
        VariedReq
    ).

vary_throw_nested_key_missing_test() ->
    Opts = test_opts(),
    {ok, BaseID} = hb_cache_micro:write(#{}, Opts),
    {ok, ReqID} = 
        hb_cache_micro:write(
            #{ <<"outer">> => #{ <<"not-slot">> => 1 }},
            Opts
        ),
    ExpectedErrorPath = <<ReqID/binary, "/outer/slot">>,
    ?assertThrow(
        {required_key_missing, ExpectedErrorPath},
        vary(<<"test-device@1.0">>, <<"compute_nested">>, BaseID, ReqID, Opts)
    ).

vary_throw_nested_key_wrong_type_test() ->
    Opts = test_opts(),
    {ok, BaseID} = hb_cache_micro:write(#{}, Opts),
    {ok, ReqID} = 
        hb_cache_micro:write(
            #{ <<"outer">> => #{ <<"slot">> => <<"1">> }},
            Opts
        ),
    ExpectedErrorPath = <<ReqID/binary, "/outer/slot">>,
    ?assertThrow(
        {invalid_type, 
            {key, ExpectedErrorPath},
            {value, <<"1">>},
            {expected_type, integer}
        },
        vary(<<"test-device@1.0">>, <<"compute_nested">>, BaseID, ReqID, Opts)
    ).

vary_on_all_test() -> 
    Opts = test_opts(),
    {ok, BaseID} = 
        hb_cache_micro:write(
            #{ <<"a">> => 1, <<"b">> => 2 },
            Opts
        ),
    {ok, ReqID} = hb_cache_micro:write(#{ <<"slot">> => 1 }, Opts),
    {ok, VariedBase, VariedReq} =
        vary(<<"test-device@1.0">>, <<"compute_all">>, BaseID, ReqID, Opts),
    ?event(debug_types, {vary_result, {varied_base, VariedBase}, {varied_req, VariedReq}}),
    ?assertEqual(#{ <<"a">> => 1, <<"b">> => 2 }, VariedBase),
    ?assertEqual(#{ <<"slot">> => 1 }, VariedReq).

vary_on_all_nested_test() -> 
    Opts = test_opts(),
    {ok, BaseID} = 
        hb_cache_micro:write(
            #{ 
                <<"a">> => 1, 
                <<"b">> => 2, 
                <<"outer">> => #{ <<"c">> => 3, <<"d">> => 4 } 
            },
            Opts
        ),
    {ok, ReqID} = hb_cache_micro:write(#{ <<"slot">> => 1 }, Opts),
    {ok, VariedBase, VariedReq} =
        vary(<<"test-device@1.0">>, <<"compute_all">>, BaseID, ReqID, Opts),
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