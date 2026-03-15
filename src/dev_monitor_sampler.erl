-module(dev_monitor_sampler).
-export([info/1, should_sample/3]).
-include("include/hb.hrl").


%%% A lightweight device that gates HTTP monitor invocations via probabilistic
%%% sampling. When `sample-rate` is set to N in the monitor config, only
%%% ~1-in-N requests will be forwarded. If absent, every request is forwarded.

info(_Base) ->
    #{default => fun should_sample/3}.

should_sample(_Base, Req, Opts) ->
    ?event({req, Req}),
    case hb_maps:get(<<"sample-rate">>, Req, not_found, Opts) of
        not_found ->
            {ok, true};
        Rate when is_integer(Rate), Rate > 0 ->
            {ok, rand:uniform(Rate) =:= 1};
        _Other ->
            {ok, true}
    end.
