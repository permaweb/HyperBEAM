%%% @doc Type extraction tooling for AO-Core devices in HyperBEAM.
-module(hb_types).
-export([extract/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

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
        Types = maps:from_list(lists:map(fun typeinfo/1, TypeAttrs)),
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
    {Args, Ret} = fun_annotation_to_schema(Spec),
    ?event({args, Args}),
    Base = typeinfo(maybe_nth(1, Args)),
    ?event({base, {explicit, Base}}),
    Request = typeinfo(maybe_nth(2, Args)),
    ?event({request, {explicit, Request}}),
    Opts = typeinfo(maybe_nth(3, Args)),
    ?event({opts, {explicit, Opts}}),
    Schema = #{ base => Base, request => Request, opts => Opts, return => Ret },
    {Name, Schema}.

%% @doc Get the nth element of a list or return undefined if the list is too
%% short.
maybe_nth(_, []) -> undefined;
maybe_nth(1, [H|_]) -> H;
maybe_nth(N, [_|T]) -> maybe_nth(N-1, T).

%% @doc Extract AO-Core type specifications from a function annotation.
fun_annotation_to_schema({type,_, 'fun', [{type, _, product, Args}, RetSpec]}) ->
    {BinStatus, Ret} = typeinfo(RetSpec),
    {Args, {hb_util:atom(BinStatus), Ret}};
fun_annotation_to_schema(Other) ->
    error({unexpected_spec_ast, Other}).

%% @doc Extract AO-Core type specifications from Erlang function spec ASTs.
typeinfo({type, _, list, List}) ->
    lists:map(fun typeinfo/1, List);
typeinfo({type, _, tuple, TupleElements}) ->
    list_to_tuple(lists:map(fun typeinfo/1, TupleElements));
typeinfo({type, _, map, any}) -> #{};
typeinfo({type, _, map, Fields}) ->
    #{
        (typeinfo(K)) => {optional(Optional), typeinfo(V)}
    ||
        {type, _, Optional, [K,V]} <- Fields
    };
typeinfo({type, _, TypeName, []}) -> TypeName;
typeinfo({atom, _, Atom}) -> hb_util:atom_to_key(Atom);
typeinfo(Other) -> {unknown_type, Other}.

%% @doc Turn AST map associativity types into schema optional/required flags.
optional(map_field_assoc) -> optional;
optional(map_field_exact) -> required.

%%% Tests

extract_test() ->
    Res = extract(<<"test-device@1.0">>, #{}),
    ?event({extraction_result, Res}),
    ?assertMatch(
        {ok, #{ keys := #{}, types := #{}}},
        Res
    ).