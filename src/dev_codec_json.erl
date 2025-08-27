%%% @doc A simple JSON codec for HyperBEAM's message format. Takes a
%%% message as TABM and returns an encoded JSON string representation.
%%% This codec utilizes the httpsig@1.0 codec for signing and verifying.
-module(dev_codec_json).
-export([to/3, from/3, commit/3, verify/3, committed/3, content_type/1]).
-export([deserialize/3, serialize/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Return the content type for the codec.
content_type(_) -> {ok, <<"application/json">>}.

%% @doc Encode a message to a JSON string, using JSON-native typing.
to(Msg, _Req, _Opts) when is_binary(Msg) ->
    {ok, hb_util:bin(json:encode(Msg))};
to(Msg, Req, Opts) ->
    % The input to this function will be a TABM message, so we:
    % 1. Convert it to a structured message.
    % 2. Load any linked items if we are in `bundle' mode.
    % 3. Convert it back to a TABM message, this time preserving all types
    %    aside `atom's -- for which JSON has no native support.
    Restructured =
        hb_message:convert(
            hb_private:reset(Msg),
            <<"structured@1.0">>,
            tabm,
            Opts
        ),
    Loaded =
        case hb_maps:get(<<"bundle">>, Req, false, Opts) of
            true ->
                hb_cache:ensure_all_loaded(Restructured, Opts);
            false ->
                Restructured
        end,
    {ok, JSONStructured} =
        dev_codec_structured:from(
            Loaded,
            Req#{ <<"encode-types">> => [<<"atom">>] },
            Opts
        ),
    {ok, hb_json:encode(JSONStructured)}.

%% @doc Decode a JSON string to a message.
from(Map, _Req, _Opts) when is_map(Map) -> {ok, Map};
from(JSON, _Req, Opts) ->
    % The JSON string will be a partially-TABM encoded message: Rich number
    % and list types, but no `atom's. Subsequently, we convert it to a fully
    % structured message after decoding, then turn the result back into a TABM.
    % This is resource-intensive and could be improved, but ensures that the
    % results are fully normalized.
    Decoded = json:decode(JSON),
    {ok, Structured} =
        dev_codec_structured:to(
            Decoded,
            #{},
            Opts
        ),
    {ok, TABM} = dev_codec_structured:from(Structured, #{}, Opts),
    {ok, TABM}.

commit(Msg, Req, Opts) -> dev_codec_httpsig:commit(Msg, Req, Opts).

verify(Msg, Req, Opts) -> dev_codec_httpsig:verify(Msg, Req, Opts).

committed(Msg, Req, Opts) when is_binary(Msg) ->
    committed(hb_util:ok(from(Msg, Req, Opts)), Req, Opts);
committed(Msg, _Req, Opts) ->
    hb_message:committed(Msg, all, Opts).

%% @doc Deserialize the JSON string found at the given path.
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
serialize(Base, Msg, Opts) ->
    {ok,
        #{
            <<"content-type">> => <<"application/json">>,
            <<"body">> => hb_util:ok(to(Base, Msg, Opts))
        }
    }.

%%% Tests

decode_with_atom_test() ->
    JSON =
        <<"""
        [
            {
                "store-module": "hb_store_fs",
                "name": "cache-TEST/json-test-store",
                "ao-types": "store-module=\"atom\""
            }
        ]
        """>>,
    Msg = hb_message:convert(JSON, <<"structured@1.0">>, <<"json@1.0">>, #{}),
    ?assertMatch(
        [#{ <<"store-module">> := hb_store_fs }|_],
        hb_cache:ensure_all_loaded(Msg, #{})
    ).