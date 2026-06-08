%%% @doc Compatibility alias for the Odysee claim device.
-module(dev_lbry_claim).
-implements(<<"lbry-claim@1.0">>).
-export([info/1, resolve/3]).

info(_Opts) ->
    #{ exports => [<<"resolve">>] }.

resolve(Base, Req, Opts) ->
    hb_ao:raw(<<"odysee-claim@1.0">>, <<"resolve">>, Base, Req, Opts).
