-module(dev_test_trie).

-export([test/0]).

test() ->
    Trie = ao_trie:test(),
    Req = #{<<"3FzFTKKTXfwxl4iccvMqfe4-C0cA7RDxMaf9RS5neJc">> => <<"1000003">>,
       <<"5ns4NnP4LYFqcJP7YHGw6Q-dlVqNFKkvYf7PZ0_2m2U">> => <<"58997996987">>,
       <<"path">> => <<"set">>},
    {Trie, Req}.