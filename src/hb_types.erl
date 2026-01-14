%%% @doc Type extraction tooling for AO-Core devices in HyperBEAM.
-module(hb_types).
-export([extract/2, vary/5]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Apply device's type-spec information to a base and request prior to
%% AO-Core execution.
vary(Device, Key, Base, Request, Opts) ->
    {ok, #{ keys := KeySchemas, types := _Types }} = extract(Device, Opts),
    case maps:get(Key, KeySchemas, undefined) of
        undefined -> {ok, Base, Request};
        #{ base := BaseSchema, request := RequestSchema } ->
            {
                ok,
                apply_schema(BaseSchema, Base, Opts),
                apply_schema(RequestSchema, Request, Opts)
            }
    end.

%% @doc Apply a schema to a message, throwing an error if a required key is
%% missing.
apply_schema(Schema, Message, Opts) ->
    AllKeys = hb_maps:keys(Schema, Opts),
    RequiredKeys =
        hb_maps:keys(
            hb_maps:filter(
                fun({_, {Required, _}}) -> Required == required end,
                Schema,
                Opts
            ),
            Opts
        ),
    lists:foreach(
        fun(Key) ->
            case hb_maps:find(Key, Message, Opts) of
                {ok, _} -> ok;
                error -> throw({required_key_missing, Key})
            end
        end,
        RequiredKeys
    ),
    hb_message:normalize_commitments(
        hb_maps:with(
            Message,
            AllKeys,
            Opts
        ),
        Opts,
        verify
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

%%% Tests

extract_test() ->
    Res = extract(<<"test-device@1.0">>, #{}),
    ?event({extraction_result, Res}),
    ?assertMatch(
        {ok, #{ keys := #{}, types := #{}}},
        Res
    ).