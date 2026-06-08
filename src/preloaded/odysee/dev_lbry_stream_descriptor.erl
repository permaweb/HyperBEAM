%%% @doc Compatibility alias for the Odysee stream descriptor device.
-module(dev_lbry_stream_descriptor).
-implements(<<"lbry-stream-descriptor@1.0">>).
-export([info/1, decode/3, fetch/3, verify/3, reconstruct/3, media/3]).

info(_Opts) ->
    #{
        exports => [
            <<"decode">>,
            <<"fetch">>,
            <<"verify">>,
            <<"reconstruct">>,
            <<"media">>
        ]
    }.

decode(Base, Req, Opts) ->
    hb_ao:raw(<<"odysee-stream-descriptor@1.0">>, <<"decode">>, Base, Req, Opts).

fetch(Base, Req, Opts) ->
    hb_ao:raw(<<"odysee-stream-descriptor@1.0">>, <<"fetch">>, Base, Req, Opts).

verify(Base, Req, Opts) ->
    hb_ao:raw(<<"odysee-stream-descriptor@1.0">>, <<"verify">>, Base, Req, Opts).

reconstruct(Base, Req, Opts) ->
    hb_ao:raw(<<"odysee-stream-descriptor@1.0">>, <<"reconstruct">>, Base, Req, Opts).

media(Base, Req, Opts) ->
    hb_ao:raw(<<"odysee-stream-descriptor@1.0">>, <<"media">>, Base, Req, Opts).
