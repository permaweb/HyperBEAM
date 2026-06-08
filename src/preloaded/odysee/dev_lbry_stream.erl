%%% @doc Compatibility alias for the Odysee stream device.
-module(dev_lbry_stream).
-implements(<<"lbry-stream@1.0">>).
-export([info/1, stream/3, from_claim/3, playback/3, media/3]).

info(_Opts) ->
    #{ exports => [<<"stream">>, <<"from-claim">>, <<"playback">>, <<"media">>] }.

stream(Base, Req, Opts) ->
    hb_ao:raw(<<"odysee-stream@1.0">>, <<"stream">>, Base, Req, Opts).

from_claim(Base, Req, Opts) ->
    hb_ao:raw(<<"odysee-stream@1.0">>, <<"from-claim">>, Base, Req, Opts).

playback(Base, Req, Opts) ->
    hb_ao:raw(<<"odysee-stream@1.0">>, <<"playback">>, Base, Req, Opts).

media(Base, Req, Opts) ->
    hb_ao:raw(<<"odysee-stream@1.0">>, <<"media">>, Base, Req, Opts).
