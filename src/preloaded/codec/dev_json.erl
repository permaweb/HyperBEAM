%%% @doc A simple JSON codec for HyperBEAM's message format. Takes a
%%% message as TABM and returns an encoded JSON string representation.
%%% This codec utilizes the httpsig@1.0 codec for signing and verifying.
-module(dev_json).
-specification("../../../specs/json@1.0.md").
-device_libraries([lib_arweave_common]).
-export([to/3, to_hint/3, from/3, commit/3, verify/3, committed/3]).
-export([deserialize/3, serialize/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(MAX_SAFE_INTEGER, 9007199254740991).

%% @doc Apply the bundle state of the delegated HTTPSig commitment.
to_hint(Msg, Req, Opts) ->
    case lib_arweave_common:bundle_hint(<<"httpsig@1.0">>, Msg, Req, Opts) of
        not_found -> {ok, Req};
        Hint -> Hint
    end.

%% @doc Encode a message to a JSON string, using JSON-native typing. An integer
%% that a JSON number does not hold exactly is given as a string.
-spec to(
    binary() | number() | boolean() | null | #{ _ => _ },
    #{ bundle => boolean(), _ => _ },
    #{ _ => _ }
) -> {ok, binary()}.
to(Int, Req, Opts) when is_integer(Int), abs(Int) > ?MAX_SAFE_INTEGER ->
    to(integer_to_binary(Int), Req, Opts);
to(Msg, _Req, _Opts) when is_binary(Msg); is_number(Msg);
        Msg =:= true; Msg =:= false; Msg =:= null ->
    {ok, hb_util:bin(json:encode(Msg))};
to(Msg, Req, Opts) ->
    ConvOpts = Opts#{ <<"hashpath">> => ignore },
    HintedReq = hb_util:ok(to_hint(Msg, Req, Opts)),
    % The input to this function will be a TABM message, so we:
    % 1. Convert it to a structured message.
    % 2. Load any linked items if we are in `bundle' mode.
    % 3. Convert it back to a TABM message, this time preserving all types
    %    aside `atom's, binaries that are not UTF-8 text and integers that a
    %    JSON number does not hold exactly -- for which JSON has no native
    %    support.
    Restructured =
        hb_message:convert(
            hb_private:reset(Msg),
            <<"structured@1.0">>,
            tabm,
            ConvOpts
        ),
    Bundle =
        is_list(Restructured) orelse
            hb_maps:get(<<"bundle">>, HintedReq, false, Opts),
    Loaded =
        case Bundle of
            true -> hb_cache:ensure_all_loaded(Restructured, Opts);
            false -> Restructured
        end,
    JSONStructured =
        hb_message:convert(
            Loaded,
            tabm,
            #{
                <<"device">> => <<"structured@1.0">>,
                <<"encode-types">> => [<<"atom">>, <<"binary">>],
                <<"max-integer">> => ?MAX_SAFE_INTEGER,
                <<"bundle">> => Bundle
            },
            ConvOpts
        ),
    {ok, hb_json:encode(JSONStructured)}.

%% @doc Decode a JSON string to a message.
-spec from(
    binary() | #{ _ => _ },
    #{ _ => _ },
    #{ _ => _ }
) -> {ok, #{ _ => _ }}.
from(Map, _Req, _Opts) when is_map(Map) -> {ok, Map};
from(JSON, Req, Opts) ->
    ConvOpts = Opts#{ <<"hashpath">> => ignore },
    % The JSON string will be a partially-TABM encoded message: Rich number
    % and list types, but no `atom's. Subsequently, we convert it to a fully
    % structured message after decoding, then turn the result back into a TABM.
    % This is resource-intensive and could be improved, but ensures that the
    % results are fully normalized.
    Structured =
        hb_message:convert(
            json:decode(JSON),
            <<"structured@1.0">>,
            tabm,
            ConvOpts
        ),
    HintedReq = hb_util:ok(to_hint(Structured, Req, Opts)),
    ?event(debug_json, {structured, Structured}, Opts),
    % Re-encode the structured message back to TABM for the caller.
    TABM =
        hb_message:convert(
            Structured,
            tabm,
            HintedReq#{ <<"device">> => <<"structured@1.0">> },
            ConvOpts
        ),
    ?event(debug_json, {tabm, TABM}, Opts),
    {ok, TABM}.

%% @doc Route commitments through `httpsig@1.0'.
-spec commit(#{ _ => _ }, #{ _ => _ }, map()) -> term().
commit(Msg, Req, Opts) ->
    % `hb_message:commit/3' without its first line, which takes the node key:
    % a client can call this.
    {ok, Structured} =
        hb_ao:raw(<<"structured@1.0">>, <<"to">>, Msg, #{}, Opts),
    hb_ao:raw(
        <<"message@1.0">>,
        <<"commit">>,
        Structured,
        Req#{ <<"commitment-device">> => <<"httpsig@1.0">> },
        Opts
    ).

%% @doc Route verification through `httpsig@1.0'.
-spec verify(#{ _ => _ }, #{ _ => _ }, map()) -> term().
verify(Msg, Req, Opts) ->
    hb_ao:raw(<<"httpsig@1.0">>, <<"verify">>, Msg, Req, Opts).

-spec committed(binary() | #{ _ => _ }, #{ _ => _ }, #{ _ => _ }) -> [binary()].
committed(Msg, Req, Opts) when is_binary(Msg) ->
    committed(hb_util:ok(from(Msg, Req, Opts)), Req, Opts);
committed(Msg, _Req, Opts) ->
    hb_message:committed(Msg, all, Opts).

%% @doc Deserialize a JSON string to its message, a TABM with JSON's number and
%% list types, or decode the JSON string found at the given path of a message.
-spec deserialize(
    binary() | #{ _ => _ },
    #{ target => binary(), _ => _ },
    #{ _ => _ }
) ->
    {ok, #{ _ => _ }}
    | {error, #{ status := integer(), body := binary(), _ => _ }}.
deserialize(JSON, _Req, _Opts) when is_binary(JSON) ->
    {ok, json:decode(JSON)};
deserialize(Base, Req, Opts) ->
    Payload = 
        hb_ao:get(
            Target =
                hb_ao:get(
                    <<"target">>,
                    Req,
                    <<"body">>,
                    Opts
                ),
            Base,
            Opts
        ),
    case Payload of
        not_found -> {error, #{
            <<"status">> => 404,
            <<"body">> =>
                <<
                    "JSON payload not found in the base message.",
                    "Searched for: ", Target/binary
                >>
            }};
        _ ->
            from(Payload, Req, Opts)
    end.

%% @doc Serialize a message to a JSON string.
-spec serialize(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ 'content-type' := binary(), body := binary(), _ => _ }}.
serialize(Base, Msg, Opts) ->
    {ok,
        #{
            <<"content-type">> => <<"application/json">>,
            <<"body">> => hb_util:ok(to(Base, Msg, Opts))
        }
    }.

%%% Tests

%% @doc JSON reads preserve literals; unsupported chaining is a 400.
scalar_http_read_test() ->
    Node = hb_http_server:start_node(#{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"test-integer">> => 42,
        <<"test-name">> => <<"ASSET">>,
        <<"test-large">> => 9007199254740993
    }),
    lists:foreach(
        fun({Key, Expected}) ->
            Req = #{
                peer => Node,
                path => <<"/~meta@1.0/info/", Key/binary>>,
                method => <<"GET">>,
                headers => #{ <<"accept">> => <<"application/json">> },
                body => <<>>
            },
            {ok, 200, _, JSON} = hb_http_client:request(Req, #{}),
            ?assertEqual(Expected, maps:get(<<"body">>, json:decode(JSON))),
            lists:foreach(
                fun(Suffix) ->
                    ?assertMatch({ok, 400, _, _}, hb_http_client:request(
                        Req#{ path => <<"/~meta@1.0/info/", Key/binary,
                            Suffix/binary>> }, #{}
                    ))
                end,
                [<<"/~json@1.0/serialize">>, <<"/serialize~json@1.0">>]
            )
        end,
        [{<<"test-integer">>, 42}, {<<"test-name">>, <<"ASSET">>},
            {<<"test-large">>, <<"9007199254740993">>}]
    ).

large_integer_roundtrip_test() ->
    Big = ?MAX_SAFE_INTEGER + 2,
    Opts = #{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"store">> => hb_test_utils:test_store()
    },
    Msg = #{
        <<"balance">> => Big,
        <<"negative">> => -Big,
        <<"safe">> => ?MAX_SAFE_INTEGER,
        <<"nested">> => [Big, #{ <<"amount">> => Big, <<"state">> => ok }]
    },
    Signed = hb_message:commit(Msg, Opts, #{ <<"bundle">> => true }),
    JSON = hb_message:convert(Signed, <<"json@1.0">>, Opts),
    Parsed = json:decode(JSON),
    ?assertEqual(integer_to_binary(Big), maps:get(<<"balance">>, Parsed)),
    ?assertEqual(integer_to_binary(-Big), maps:get(<<"negative">>, Parsed)),
    ?assertEqual(?MAX_SAFE_INTEGER, maps:get(<<"safe">>, Parsed)),
    Decoded = hb_message:convert(
        JSON, <<"structured@1.0">>, <<"json@1.0">>, Opts
    ),
    ?assert(hb_message:verify(Decoded, all, Opts)),
    ?assert(hb_message:match(Signed, Decoded, strict, Opts)),
    {ok, #{ <<"body">> := Scalar }} = hb_ao:raw(
        <<"json@1.0">>, <<"serialize">>, Big, #{}, Opts
    ),
    ?assertEqual(integer_to_binary(Big), json:decode(Scalar)).

scalar_serialization_test() ->
    lists:foreach(
        fun(Value) ->
            {ok, Result} = hb_ao:raw(
                <<"json@1.0">>, <<"serialize">>, Value, #{}, #{}
            ),
            ?assertEqual(<<"application/json">>, maps:get(<<"content-type">>, Result)),
            ?assertEqual(Value, json:decode(maps:get(<<"body">>, Result)))
        end,
        [42, -1, 1.5, true, false, null, <<"ASSET">>]
    ).

decode_with_atom_test() ->
    JSON =
        <<"""
        [
            {
                "store-module": "hb_store_fs",
                "name": "cache-TEST/json-test-store",
                "ao-types": "atom=(\"store-module\")"
            }
        ]
        """>>,
    Msg = hb_message:convert(JSON, <<"structured@1.0">>, <<"json@1.0">>, #{}),
    ?assertMatch(
        [#{ <<"store-module">> := hb_store_fs }|_],
        hb_cache:ensure_all_loaded(Msg, #{})
    ).

deeply_nested_typed_keys_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    Msg = #{
        <<"message">> =>
            [
                #{
                    <<"deep-integer">> => 456,
                    <<"deep-atom">> => atom,
                    <<"deep-list">> => [1,2,3]
                }
            ]
    },
    Encoded =
        hb_message:convert(
            Msg,
            #{
                <<"device">> => <<"json@1.0">>,
                <<"bundle">> => true
            },
            Opts
        ),
    ?event(debug_json, {encoded, Encoded}, Opts),
    Decoded =
        hb_message:convert(
            Encoded,
            <<"structured@1.0">>,
            <<"json@1.0">>,
            Opts
        ),
    ?event(debug_json, {decoded, Decoded}, Opts),
    ?assert(hb_message:match(Msg, Decoded, strict, Opts)),
    % Unbundled lists with maps survive round-trip without dropping elements.
    ListMsg = [42, #{ <<"a">> => 1 }, 99],
    ListJSON = hb_message:convert(ListMsg, <<"json@1.0">>, Opts),
    ListDecoded =
        hb_message:convert(
            ListJSON,
            <<"structured@1.0">>,
            <<"json@1.0">>,
            Opts
        ),
    ?assertEqual(ListMsg, hb_cache:ensure_all_loaded(ListDecoded, Opts)),
    SingleListMsg = [#{ <<"a">> => 1 }],
    SingleListJSON = hb_message:convert(SingleListMsg, <<"json@1.0">>, Opts),
    SingleListDecoded =
        hb_message:convert(
            SingleListJSON,
            <<"structured@1.0">>,
            <<"json@1.0">>,
            Opts
        ),
    ?assertEqual(SingleListMsg, hb_cache:ensure_all_loaded(SingleListDecoded, Opts)).
