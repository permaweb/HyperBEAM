%%% @doc A simple JSON codec for HyperBEAM's message format. Takes a
%%% message as TABM and returns an encoded JSON string representation.
%%% This codec utilizes the httpsig@1.0 codec for signing and verifying.
-module(dev_codec_json).
-export([to/1, from/1, attest/3, verify/3, attested/1, content_type/1]).
-export([deserialize/3, serialize/3]).

%% @doc Return the content type for the codec.
content_type(_) -> {ok, <<"application/json">>}.

%% @doc Encode a message to a JSON string.
to(Msg) when is_binary(Msg) -> iolist_to_binary(json:encode(Msg));
to(Msg) -> iolist_to_binary(json:encode(hb_private:reset(Msg))).

%% @doc Decode a JSON string to a message.
from(Map) when is_map(Map) -> Map;
from(Json) -> json:decode(Json).

attest(Msg, Req, Opts) -> dev_codec_httpsig:attest(Msg, Req, Opts).

verify(Msg, Req, Opts) -> dev_codec_httpsig:verify(Msg, Req, Opts).

attested(Msg) when is_binary(Msg) -> attested(from(Msg));
attested(Msg) -> hb_message:attested(Msg).

%% @doc Deserialize the JSON string found at the given path.
deserialize(Base, Req, Opts) ->
    Payload = 
        hb_converge:get(
            Target =
                hb_converge:get(
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
            Decoded = from(Payload),
            {ok, Decoded}
    end.

%% @doc Serialize a message to a JSON string.
serialize(Base, _Msg, _Opts) ->
    {ok,
        #{
            <<"content-type">> => <<"application/json">>,
            <<"body">> => to(Base)
        }
    }.
