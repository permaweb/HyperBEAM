%%% @doc Encode and decode the `body' of a message as unpadded base64url
%%% (RFC 4648, section 5): the encoding of IDs and binaries across the
%%% Arweave data protocols.
-module(dev_base64url).
-export([decode/3, encode/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Replace the base message's base64url `body' with its decoded bytes.
decode(Base, _Req, Opts) ->
    with_body(Base, Opts, fun hb_util:decode/1).

%% @doc Replace the base message's binary `body' with its base64url encoding.
encode(Base, _Req, Opts) ->
    with_body(Base, Opts, fun hb_util:encode/1).

%% @doc Apply a codec function to the base message's body, in place.
with_body(Base, Opts, Codec) ->
    case hb_maps:find(<<"body">>, Base, Opts) of
        {ok, Body} when is_binary(Body) ->
            {ok, hb_ao:set(Base, #{ <<"body">> => Codec(Body) }, Opts)};
        _ ->
            {error, {'invalid-body', <<"No binary `body' key found.">>}}
    end.

%%% Tests

%% @doc Round-trip a random binary through encode and decode.
round_trip_test() ->
    Data = crypto:strong_rand_bytes(32),
    {ok, Encoded} =
        hb_ao:resolve(
            #{
                <<"path">> => <<"~base64url@1.0/encode/body">>,
                <<"body">> => Data
            },
            #{}
        ),
    ?assertEqual(hb_util:encode(Data), Encoded),
    {ok, Decoded} =
        hb_ao:resolve(
            #{
                <<"path">> => <<"~base64url@1.0/decode/body">>,
                <<"body">> => Encoded
            },
            #{}
        ),
    ?assertEqual(Data, Decoded).
