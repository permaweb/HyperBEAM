%%% @doc A device that triggers repass events until a certain counter has been
%%% reached. This is useful for certain types of stacks that need various
%%% execution passes to be completed in sequence across devices.
-module(dev_multipass).
-export([info/1]).
-include("include/hb.hrl").

-include_lib("eunit/include/eunit.hrl").


info(_M1) ->
    #{
      handler => fun handle/4
     }.


%% @doc Forward the keys function to the message device, handle all others
%% with deduplication. We only act on the first pass.
handle(<<"keys">>, M1, _M2, Opts) ->
    dev_message:keys(M1, Opts);
handle(<<"set">>, M1, M2, Opts) ->
    dev_message:set(M1, M2, Opts);
handle(_Key, M1, _M2, Opts) ->
    Passes = hb_ao:get(<<"passes">>, {as, dev_message, M1}, 1, Opts),
    Pass = hb_ao:get(<<"pass">>, {as, dev_message, M1}, 1, Opts),
    case Pass < Passes of
        true -> {pass, M1};
        false -> {ok, M1}
    end.


%%% Tests


basic_multipass_test() ->
    Msg1 =
        #{
          <<"device">> => <<"Multipass@1.0">>,
          <<"passes">> => 2,
          <<"pass">> => 1
         },
    Msg2 = Msg1#{<<"pass">> => 2},
    ?assertMatch({pass, _}, hb_ao:resolve(Msg1, <<"Compute">>, #{})),
    ?event(alive),
    ?assertMatch({ok, _}, hb_ao:resolve(Msg2, <<"Compute">>, #{})).
