%%% @doc Allows Arweave message IDs to be used via their base32 encoding as
%%% subdomains on a HyperBEAM node.
-module(dev_b32_name).
-export([info/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

info(_Opts) ->
    #{
        default => fun get/4,
        excludes => [<<"keys">>, <<"set">>]
    }.

%% @doc Try to resolve 52char subdomain back to its original TX ID
get(Key, _, _HookMsg, _Opts) ->
    ?event({resolve_52char, {key, Key}}),
    case decode(Key) of
        error ->
            ?event({not_base32_id, {key, Key}}),
            {error, not_found};
        ID ->
            ?event({resolved_52char, {key, Key}, {id, ID}}),
            {ok, ID}
    end.

%% @doc If the key is a 52-character binary, attempt to decode it as base32.
%% Else, return `error`.
decode(Key) when byte_size(Key) == 52 ->
    try hb_util:human_id(base32:decode(Key))
    catch _:_ -> error
    end;
decode(_Key) -> error.

%% @doc Convert an ID into its base32 encoded string representation.
encode(ID) when ?IS_ID(ID) ->
    hb_util:bin(
        string:replace(
            string:to_lower(
                hb_util:list(base32:encode(hb_util:native_id(ID)))
            ),
            "=",
            "",
            all
        )
    ).

%%% Tests

%% @doc If the sudbdomain provided isn't a valid ARNS or a 52 char subdomain 
%% it should return 404 instead of HyperBEAM page.
invalid_arns_and_not_52char_host_resolution_gives_404_test() ->
    Opts = dev_name:test_arns_opts(),
    Node = hb_http_server:start_node(Opts),
    ?assertMatch(
        {error, #{<<"status">> := 404}},
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"/">>,
                <<"host">> => <<"non-existing-subdomain.localhost">>
            },
            Opts
        )
    ).

%% @doc Unit test for 52 char subdomain to TX ID logic
key_to_id_test() -> 
    Subdomain = <<"4nuojs5tw6xtfjbq47dqk6ak7n6tqyr3uxgemkq5z5vmunhxphya">>,
    ?assertEqual(
        <<"42jky7O3rzKkMOfHBXgK-304YjulzEYqHc9qyjT3efA">>,
        decode(Subdomain)
    ).

%% @doc Resolving a 52 char subdomain without a TXID in the path should work.
empty_path_manifest_test() ->
    TestPath = <<"/">>,
    Opts = load_manifest_opts(),
    %% Test to load manifest with only subdomain
    Subdomain = <<"4nuojs5tw6xtfjbq47dqk6ak7n6tqyr3uxgemkq5z5vmunhxphya">>,
    Node = hb_http_server:start_node(Opts),
    ?assertMatch(
        {ok,
            #{
                <<"status">> := 200,
                <<"commitments">> :=
                    #{<<"Tqh6oIS2CLUaDY11YUENlvvHmDim1q16pMyXAeSKsFM">> := _}
            }
        }, 
        hb_http:get(
            Node, 
            #{
                <<"path">> => TestPath,
                <<"host">> => <<Subdomain/binary, ".localhost">>
            },
            Opts
        )
    ).

%% @doc Loading assets from a manifest where only a 52 char subdomain is 
%% provided should work. 
resolve_52char_subdomain_asset_if_txid_not_present_test() ->
    TestPath = <<"/assets/ArticleBlock-Dtwjc54T.js">>,
    Opts = load_manifest_opts(),
    %% Test to load asset with only subdomain (no TX ID present).
    Subdomain = <<"4nuojs5tw6xtfjbq47dqk6ak7n6tqyr3uxgemkq5z5vmunhxphya">>,
    Node = hb_http_server:start_node(Opts),
    ?assertMatch(
        {ok,
            #{
                <<"status">> := 200,
                <<"commitments">> :=
                    #{<<"oLnQY-EgiYRg9XyO7yZ_mC0Ehy7TFR3UiDhFvxcohC4">> := _}
            }
        }, 
        hb_http:get(
            Node, 
            #{
                <<"path">> => TestPath,
                <<"host">> => <<Subdomain/binary, ".localhost">>
            },
            Opts
        )
    ).

%% @doc Loading assets from a manifest where a 52 char subdomain and TX ID 
%% is provided should work.
subdomain_matches_path_id_and_loads_asset_test() ->
    TestPath = <<"/42jky7O3rzKkMOfHBXgK-304YjulzEYqHc9qyjT3efA/assets/ArticleBlock-Dtwjc54T.js">>,
    Opts = load_manifest_opts(),
    %% Test to load asset with only subdomain (no TX ID present).
    Subdomain = <<"4nuojs5tw6xtfjbq47dqk6ak7n6tqyr3uxgemkq5z5vmunhxphya">>,
    Node = hb_http_server:start_node(Opts),
    ?assertMatch(
        {ok,
            #{
                <<"status">> := 200,
                <<"commitments">> :=
                    #{<<"oLnQY-EgiYRg9XyO7yZ_mC0Ehy7TFR3UiDhFvxcohC4">> := _}
            }
        }, 
        hb_http:get(
            Node, 
            #{
                <<"path">> => TestPath,
                <<"host">> => <<Subdomain/binary, ".localhost">>
            },
            Opts
        )
    ).

%% @doc Validate the behavior when a subdomain and primary path ID match. The
%% duplicated ID in the request message stream should be ignored.
subdomain_matches_path_id_test() ->
    #{ id1 := ID1, opts := Opts } = test_opts(),
    ?assertMatch(
        {ok, 1},
        hb_http:get(
            hb_http_server:start_node(Opts), 
            #{
                <<"path">> => <<ID1/binary, "/a">>,
                <<"host">> => subdomain(ID1, Opts)
            },
            Opts
        )
    ).

%% @doc Validate the behavior when a subdomain and primary path ID match. Both
%% IDs should be executed, the subdomain first then the path ID.
subdomain_does_not_match_path_id_test() ->
    #{ id1 := ID1, id2 := ID2, opts := Opts }
        = test_opts(),
    ?assertMatch(
        {error, not_found},
        hb_http:get(
            hb_http_server:start_node(Opts), 
            #{
                <<"path">> => <<ID1/binary, "/a">>,
                <<"host">> => subdomain(ID2, Opts)
            },
            Opts
        )
    ).

%% @doc When both 52 char subdomain and TX ID are provided and equal, ignore 
%% the TXID from the assets path. 
manifest_subdomain_matches_path_id_test() ->
    TestPath = <<"/42jky7O3rzKkMOfHBXgK-304YjulzEYqHc9qyjT3efA">>,
    Opts = load_manifest_opts(),
    Subdomain = <<"4nuojs5tw6xtfjbq47dqk6ak7n6tqyr3uxgemkq5z5vmunhxphya">>,
    Node = hb_http_server:start_node(Opts),
    ?assertMatch(
        {ok,
            #{
                <<"status">> := 200,
                <<"commitments">> :=
                    #{<<"Tqh6oIS2CLUaDY11YUENlvvHmDim1q16pMyXAeSKsFM">> := _}
            }
        }, 
        hb_http:get(
            Node, 
            #{
                <<"path">> => TestPath,
                <<"host">> => <<Subdomain/binary, ".localhost">>
            },
            Opts
        )
    ).

%% @doc When a valid 52 char subdomain TXID doesn't match the TX ID provided,
%% the subdomain TXID is loaded, and tries to access the assets path defined.
%% In this case, sinse no assets exists with this TX ID, it should load the 
%% index.
manifest_subdomain_does_not_match_path_id_test() ->
    TestPath = <<"/oLnQY-EgiYRg9XyO7yZ_mC0Ehy7TFR3UiDhFvxcohC4">>,
    Opts = load_manifest_opts(),
    Subdomain = <<"4nuojs5tw6xtfjbq47dqk6ak7n6tqyr3uxgemkq5z5vmunhxphya">>,
    Node = hb_http_server:start_node(Opts),
    ?assertMatch(
        {ok,
            #{
                <<"commitments">> :=
                    #{
                        <<"Tqh6oIS2CLUaDY11YUENlvvHmDim1q16pMyXAeSKsFM">> := _
                    }
            }
        }, 
        hb_http:get(
            Node, 
            #{
                <<"path">> => TestPath,
                <<"host">> => <<Subdomain/binary, ".localhost">>
            },
            Opts
        )
    ).

test_opts() ->
    Store = [hb_test_utils:test_store()],
    BaseOpts = #{ store => Store, priv_wallet => ar_wallet:new() },
    Msg1 =
        #{
            <<"a">> => 1,
            <<"b">> => 2,
            <<"nested">> => #{
                <<"z">> => 26
            }
        },
    Msg2 =
        #{
            <<"a">> => 2,
            <<"b">> => 4
        },
    MsgWithPath =
        #{
            <<"a">> => 3,
            <<"b">> => 6,
            <<"c">> => 9,
            <<"path">> => <<"nested">>
        },
    SignedMsg3 =
        hb_message:commit(
            #{ <<"a">> => 3, <<"b">> => 6, <<"c">> => 9 },
            BaseOpts
        ),
    {ok, UnsignedID1} = hb_cache:write(Msg1, BaseOpts),
    {ok, UnsignedID2} = hb_cache:write(Msg2, BaseOpts),
    {ok, UnsignedIDWithPath} = hb_cache:write(MsgWithPath, BaseOpts),
    {ok, _UnsignedID3} = hb_cache:write(SignedMsg3, BaseOpts),
    #{
        opts =>
            BaseOpts#{
                store => Store,
                name_resolvers => [#{ <<"device">> => <<"b32-name@1.0">> }],
                on =>
                    #{
                        <<"request">> => [#{<<"device">> => <<"name@1.0">>}]
                    }
            },
        id1 => UnsignedID1,
        id2 => UnsignedID2,
        id3 => SignedMsg3,
        id_with_path => UnsignedIDWithPath,
        messages => [Msg1, Msg2, SignedMsg3, MsgWithPath]
    }.

%% @doc Returns the subdomain for a given ID for testing purposes.
subdomain(ID, _Opts) when ?IS_ID(ID) ->
    <<(encode(ID))/binary, ".localhost">>;
subdomain(ID, Opts) ->
    subdomain(hb_message:id(ID, unsigned, Opts), Opts).

%% @doc Returns `Opts' with a test environment preloaded with manifest related
%% IDs.
load_manifest_opts() ->
    TempStore = hb_test_utils:test_store(),
    BaseOpts = #{ store => [TempStore] },
    %% Load TX data into the store
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
        name_resolvers => [#{ <<"device">> => <<"b32-name@1.0">> }],
        on =>
            #{
                <<"request">> =>
                    [
                        #{<<"device">> => <<"name@1.0">>},
                        #{<<"device">> => <<"manifest@1.0">>}
                    ]
            }
    }.
