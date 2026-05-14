%%% @doc Arweave device helper functions.
-module(dev_arweave_common).
-export([find_key/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Find a key potentially with a +link specifier.
find_key(Key, Map, Opts) ->
    case hb_maps:find(Key, Map, Opts) of
        {ok, Value} -> {Key, Value};
        error ->
            KeyLink = <<Key/binary, "+link">>,
            case hb_maps:find(KeyLink, Map, Opts) of
                {ok, Value} -> {KeyLink, Value};
                error -> error
            end
    end.

%%% Tests.

tagfind_test() ->
    Default = <<"default">>,
    ?assertEqual(
        <<"v1">>,
        ar_tx:tagfind(<<"Foo">>, [{<<"fOo">>, <<"v1">>}], Default)
    ),
    ?assertEqual(
        Default,
        ar_tx:tagfind(<<"Missing">>, [{<<"foo">>, <<"v">>}], Default)
    ).

type_test() ->
    assert_type(binary, []),
    assert_type(binary, [{<<"tag">>, <<"value">>}]),
    assert_type(list, [
        {<<"bundle-format">>, <<"binary">>},
        {<<"tag">>, <<"value">>},
        {<<"bundle-version">>, <<"2.0.0">>}
    ]),
    assert_type(map, [
        {<<"bundle-format">>, <<"binary">>},
        {<<"tag">>, <<"value">>},
        {<<"bundle-version">>, <<"2.0.0">>},
        {<<"bundle-map">>, <<"JmtD0fwFqJTK4P_XexVqBQdnDc0-C7FFIOge6GEOJE8">>}
    ]),
    % L1 TX with bundle tags, but data is not a valid bundle.
    ?assertEqual(
        binary,
        ar_tx:type(#tx{
            format = 1,
            tags = [
                {<<"bundle-format">>, <<"binary">>},
                {<<"bundle-version">>, <<"2.0.0">>}
            ],
            data = <<"not a bundle">>
        })
    ),
    ?assertEqual(
        binary,
        ar_tx:type(#tx{
            format = 2,
            tags = [
                {<<"bundle-format">>, <<"binary">>},
                {<<"bundle-version">>, <<"2.0.0">>}
            ],
            data = <<"not a bundle">>
        })
    ),
    ?assertEqual(
        binary,
        ar_tx:type(#tx{
            format = 1,
            tags = [
                {<<"bundle-format">>, <<"binary">>},
                {<<"bundle-version">>, <<"2.0.0">>}
            ],
            data = <<1:256/little, <<"not a bundle">>/binary>>
        })
    ),
    ?assertEqual(
        binary,
        ar_tx:type(#tx{
            format = 2,
            tags = [
                {<<"bundle-format">>, <<"binary">>},
                {<<"bundle-version">>, <<"2.0.0">>}
            ],
            data = <<1:256/little, <<"not a bundle">>/binary>>
        })
    ),
    % L1 TX with bundle tags, and non-binary data.
    ?assertEqual(
        list,
        ar_tx:type(#tx{
            format = 1,
            tags = [
                {<<"bundle-format">>, <<"binary">>},
                {<<"bundle-version">>, <<"2.0.0">>}
            ],
            data = []
        })
    ),
    ?assertEqual(
        list,
        ar_tx:type(#tx{
            format = 2,
            tags = [
                {<<"bundle-format">>, <<"binary">>},
                {<<"bundle-version">>, <<"2.0.0">>}
            ],
            data = []
        })
    ),
    ?assertEqual(
        map,
        ar_tx:type(#tx{
            format = 1,
            tags = [
                {<<"bundle-format">>, <<"binary">>},
                {<<"bundle-version">>, <<"2.0.0">>},
                {<<"bundle-map">>, <<"JmtD0fwFqJTK4P_XexVqBQdnDc0-C7FFIOge6GEOJE8">>}
            ],
            data = #{ <<"1">> => <<"value1">>, <<"2">> => <<"value2">> }
        })
    ),
    ?assertEqual(
        map,
        ar_tx:type(#tx{
            format = 2,
            tags = [
                {<<"bundle-format">>, <<"binary">>},
                {<<"bundle-version">>, <<"2.0.0">>},
                {<<"bundle-map">>, <<"JmtD0fwFqJTK4P_XexVqBQdnDc0-C7FFIOge6GEOJE8">>}
            ],
            data = #{ <<"1">> => <<"value1">>, <<"2">> => <<"value2">> }
        })
    ),
    ok.

assert_type(ExpectedType, Tags) ->
    ?assertEqual(ExpectedType, ar_tx:type(#tx{format = 1, tags = Tags})),
    ?assertEqual(ExpectedType, ar_tx:type(#tx{format = 2, tags = Tags})),
    ?assertEqual(ExpectedType, ar_tx:type(#tx{format = ans104, tags = Tags})).
