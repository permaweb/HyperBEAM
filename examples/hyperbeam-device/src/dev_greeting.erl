%%% @doc Example external HyperBEAM device root module.
-module(dev_greeting).
-implements(<<"greeting@1.0">>).
-export([greet/3]).

%% @doc Build a greeting from request state.
greet(_Base, Req, _Opts) ->
    Name = maps:get(<<"name">>, Req, dev_greeting_state:default_name()),
    {ok, dev_greeting_text:format(Name)}.
