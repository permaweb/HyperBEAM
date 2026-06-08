%%% @doc Compatibility alias for the Odysee channel device.
-module(dev_lbry_channel).
-implements(<<"lbry-channel@1.0">>).
-export([info/1, channel/3, from_claim/3]).

info(_Opts) ->
    #{ exports => [<<"channel">>, <<"from-claim">>] }.

channel(Base, Req, Opts) ->
    hb_ao:raw(<<"odysee-channel@1.0">>, <<"channel">>, Base, Req, Opts).

from_claim(Base, Req, Opts) ->
    hb_ao:raw(<<"odysee-channel@1.0">>, <<"from-claim">>, Base, Req, Opts).
