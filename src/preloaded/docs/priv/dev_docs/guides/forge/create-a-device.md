# Create A Device

A device root module is named `dev_<name>.erl`. The public device name is the module name converted to dash form plus a version, for example `dev_echo_lens` becomes `~echo-lens@1.0`.

Complete root module:

```erlang
-module(dev_echo_lens).
-export([info/1, echo/3, upper/3]).

info(_) ->
    #{ exports => [<<"echo">>, <<"upper">>] }.

echo(_Base, Req, Opts) ->
    {ok, hb_maps:get(<<"input">>, Req, <<>>, Opts)}.

upper(_Base, Req, Opts) ->
    Input = hb_maps:get(<<"input">>, Req, <<>>, Opts),
    Upper = string:uppercase(unicode:characters_to_list(Input)),
    {ok, unicode:characters_to_binary(Upper)}.
```

Add EUnit tests in the same file or a normal test module:

```erlang
-include_lib("eunit/include/eunit.hrl").

echo_test() ->
    ?assertEqual({ok, <<"hello">>}, echo(#{}, #{ <<"input">> => <<"hello">> }, #{})).

upper_test() ->
    ?assertEqual({ok, <<"HELLO">>}, upper(#{}, #{ <<"input">> => <<"hello">> }, #{})).
```

Package and run it with the commands in [Package, Verify, And Test](test-package-verify.md).
