%%% @doc Helper module used to prove multi-module device packaging.
-module(dev_test_example_mod).
-export([test_func/0]).

%% @doc Return the canonical `dev_test:test_func/1' response.
test_func() ->
    {ok, <<"GOOD FUNCTION">>}.
