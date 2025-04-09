%%% @doc A simple JSON codec for HyperBEAM's message format. Takes a
%%% message as TABM and returns an encoded JSON string representation.
%%% This codec utilizes the httpsig@1.0 codec for signing and verifying.
-module(dev_codec_json).
-export([to/1, from/1, commit/3, verify/3, committed/1, content_type/1]).
-export([deserialize/3, serialize/3]).

%% @doc Return the content type for the codec.
content_type(_) -> {ok, <<"application/json">>}.

%% @doc Encode a message to a JSON string.
to(Msg) when is_binary(Msg) -> iolist_to_binary(json:encode(Msg));
to(Msg) -> iolist_to_binary(json:encode(hb_private:reset(Msg))).

%% @doc Decode a JSON string to a message.
from(Map) when is_map(Map) -> Map;
from(Json) -> json:decode(Json).

commit(Msg, Req, Opts) -> dev_codec_httpsig:commit(Msg, Req, Opts).

verify(Msg, Req, Opts) -> dev_codec_httpsig:verify(Msg, Req, Opts).

committed(Msg) when is_binary(Msg) -> committed(from(Msg));
committed(Msg) -> hb_message:committed(Msg).

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
