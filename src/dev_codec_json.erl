%%% @doc A simple JSON codec for HyperBEAM's message format. Takes a
%%% message as TABM and returns an encoded JSON string representation.
%%% This codec utilizes the httpsig@1.0 codec for signing and verifying.
-module(dev_codec_json).
-export([to/3, from/3, commit/3, verify/3, committed/3, content_type/1]).
-export([deserialize/3, serialize/3]).


%% @doc Return the content type for the codec.
content_type(_) -> {ok, <<"application/json">>}.


%% @doc Encode a message to a JSON string.
to(Msg, _Req, _Opts) when is_binary(Msg) ->
    {ok, hb_util:bin(json:encode(Msg))};
to(Msg, _Req, _Opts) ->
    {ok,
     hb_util:bin(
       json:encode(
         hb_private:reset(
           hb_cache:ensure_all_loaded(Msg))))}.


%% @doc Decode a JSON string to a message.
from(Map, _Req, _Opts) when is_map(Map) -> {ok, Map};
from(JSON, _Req, _Opts) -> {ok, json:decode(JSON)}.


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
                Opts),
          Base,
          Opts),
    case Payload of
        not_found ->
            {error, #{
                      <<"status">> => 404,
                      <<"body">> =>
                          <<"JSON payload not found in the base message.",
                            "Searched for: ", Target/binary>>
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
      }}.
