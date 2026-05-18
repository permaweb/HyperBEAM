%%% @doc Test fixtures shared by name and manifest device tests.
-module(hb_name_test_utils).
-export([arns_opts/0, manifest_opts/0]).

%% @doc Return opts for an environment with the default ARNS name export.
arns_opts() ->
    JSONNames = <<"G_gb7SAgogHMtmqycwaHaC6uC-CZ3akACdFv5PUaEE8">>,
    Path = <<JSONNames/binary, "~json@1.0/deserialize&target=data">>,
    TempStore = hb_test_utils:test_store(),
    #{
        <<"store">> =>
            [
                TempStore,
                #{
                    <<"store-module">> => hb_store_gateway,
                    <<"local-store">> => [TempStore]
                }
            ],
        <<"name-resolvers">> => [Path],
        <<"on">> => #{
            <<"request">> => #{
                <<"device">> => <<"name@1.0">>
            }
        }
    }.

%% @doc Return opts with the test manifest fixture flow loaded.
manifest_opts() ->
    TempStore = hb_test_utils:test_store(),
    BaseOpts =
        #{
            <<"store">> =>
                [
                    TempStore,
                    #{<<"store-module">> => hb_store_gateway}
                ]
        },
    lists:foreach(
        fun(Ref) ->
            hb_test_utils:preload(
                BaseOpts,
                <<"test/arbundles.js/ans-104-manifest-", Ref/binary>>
            )
        end,
        [
            <<"42jky7O3rzKkMOfHBXgK-304YjulzEYqHc9qyjT3efA.bin">>,
            <<"index-Tqh6oIS2CLUaDY11YUENlvvHmDim1q16pMyXAeSKsFM.bin">>,
            <<"item-oLnQY-EgiYRg9XyO7yZ_mC0Ehy7TFR3UiDhFvxcohC4.bin">>
        ]
    ),
    BaseOpts#{
        <<"on">> =>
            #{
                <<"request">> =>
                    [#{<<"device">> => <<"manifest@1.0">>}]
            }
    }.
