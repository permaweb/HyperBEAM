%%% @doc The hyperbeam meta device, which gets and sets the node message
%%% (`info'), gives the version and build of the node (`build'), and checks
%%% whether a request is signed by the node operator (`is-operator').
-module(dev_meta).
-specification("../../../specs/meta@1.0.md").
-device_libraries([lib_meta, lib_process]).
-export([device_info/1, info/3, build/3, adopt_node_message/2]).
-export([is_operator/3]).
-export([is_operator/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Ensure that the helper function `adopt_node_message/2' is not exported.
device_info(_) -> #{ exports => [<<"info">>, <<"build">>, <<"is-operator">>] }.

%% @doc Utility function for determining if a request is from the `operator' of
%% the node. A pushed result is signed by the node, but it is not a request
%% from the operator.
is_operator(Request, NodeMsg) ->
    Operators = lib_meta:role(operator, NodeMsg),
    not lib_process:is_pushed(Request, NodeMsg) andalso
        lists:any(
            fun(Signer) -> lists:member(Signer, Operators) end,
            hb_message:signers(Request, NodeMsg)
        ).

%% @doc Return whether the request in the body is signed by the node operator.
is_operator(_Base, Req, NodeMsg) ->
    {ok, is_operator(hb_maps:get(<<"body">>, Req, Req, NodeMsg), NodeMsg)}.

%% @doc Emits the version number and commit hash of the HyperBEAM node source,
%% if available.
%%
%% We include the short hash separately, as the length of this hash may change in
%% the future, depending on the git version/config used to build the node.
%% Subsequently, rather than embedding the `git-short-hash-length', for the
%% avoidance of doubt, we include the short hash separately, as well as its long
%% hash.
-spec build(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) -> {ok, #{ _ => _ }}.
build(_, _, _NodeMsg) ->
    BuildInfo = build_info(),
    {ok,
        #{
            <<"node">> => <<"HyperBEAM">>,
            <<"version">> => ?HYPERBEAM_VERSION,
            <<"source">> => maps:get(<<"source">>, BuildInfo, <<"unknown">>),
            <<"source-short">> =>
                maps:get(<<"source-short">>, BuildInfo, <<"unknown">>),
            <<"build-time">> => maps:get(<<"build-time">>, BuildInfo, 0)
        }
    }.

build_info() ->
    case code:priv_dir(hb) of
        {error, _} ->
            #{};
        PrivDir ->
            case file:consult(filename:join(PrivDir, "hb_buildinfo")) of
                {ok, [Info]} when is_map(Info) -> Info;
                _ -> #{}
            end
    end.

%% @doc Get/set the node message. If the request is a `POST', we check that the
%% request is signed by the owner of the node. If not, we return the node message
%% as-is, aside all keys that are private (according to `hb_private').
-spec info(#{ _ => _ }, #{ _ => _ }, map()) -> term().
info(_, Request, NodeMsg) ->
    {ErlStatus, Res} = case hb_ao:get(<<"method">>, Request, NodeMsg) of
        <<"POST">> ->
            case is_permanent(NodeMsg) of
                true ->
                    {error,
                        #{
                            <<"body">> =>
                                <<"The node message of this machine is already "
                                    "permanent. It cannot be changed.">>
                        }
                    };
                false ->
                    update_node_message(Request, NodeMsg)
            end;
        _ ->
            ?event({get_config_req, Request, NodeMsg}),
            DynamicKeys = add_dynamic_keys(NodeMsg),
            {ok, filter_node_msg(DynamicKeys, NodeMsg)}
    end,
    {ErlStatus, Res#{ <<"cache-control">> => <<"no-store">> }}.

%% @doc Remove items from the node message that are not encodable into a
%% message.
filter_node_msg(Msg, NodeMsg) when is_map(Msg) ->
    hb_maps:map(
        fun(_, Value) -> filter_node_msg(Value, NodeMsg) end,
        hb_private:reset(Msg),
        NodeMsg
    );
filter_node_msg(Msg, NodeMsg) when is_list(Msg) ->
    lists:map(fun(Item) -> filter_node_msg(Item, NodeMsg) end, Msg);
filter_node_msg(Tuple, _NodeMsg) when is_tuple(Tuple) ->
    <<"Unencodable value.">>;
filter_node_msg(Other, _NodeMsg) ->
    Other.

%% @doc Add dynamic keys to the node message.
add_dynamic_keys(NodeMsg) ->
    UpdatedNodeMsg =
        case hb_opts:get(priv_wallet, no_viable_wallet, NodeMsg) of
            no_viable_wallet ->
                NodeMsg;
            Wallet ->
                %% Create a new map with address and merge it (overwriting existing)
                Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
                NodeMsg#{
                    <<"address">> => Address,
                    <<"committer">> => hb_opts:committer(NodeMsg)
                }
        end,
    add_identity_addresses(UpdatedNodeMsg).

add_identity_addresses(NodeMsg) ->
    NodeOpts = hb_http_server:with_node_committer(NodeMsg),
    Identities = hb_opts:get(identities, #{}, NodeOpts),
    NewIdentities = maps:map(fun(_, Identity) ->
        Identity#{
            <<"address">> => hb:address(
                hb_opts:get(priv_wallet, hb:wallet(), Identity)
            )
        }
    end, Identities),
    NodeMsg#{ <<"identities">> => NewIdentities }.

%% @doc Apply only the admin's signed node-message fields to the configuration.
update_node_message(Request, NodeMsg) ->
    case lib_meta:is_authorized(<<"node-message">>, admin, Request, NodeMsg) of
        {error, not_authorized} ->
            ?event({set_node_message_fail, Request}),
            {error, #{ <<"body">> => not_authorized }};
        {ok, Authorized} ->
            case adopt_node_message(Authorized, NodeMsg) of
                {ok, NewNodeMsg} ->
                    NewH = hb_opts:get(node_history, [], NewNodeMsg),
                    {ok,
                        #{
                            <<"body">> =>
                                iolist_to_binary(
                                    io_lib:format(
                                        "Node message updated. History: ~p"
                                            "updates.",
                                        [length(NewH)]
                                    )
                                ),
                            <<"history-length">> => length(NewH)
                        }
                    };
                {error, Reason} ->
                    ?event({set_node_message_fail, Request, Reason}),
                    {error, #{ <<"body">> => Reason }}
            end
    end.

%% @doc Attempt to adopt changes to a node message.
adopt_node_message(Request, NodeMsg) ->
    ?event({set_node_message_success, Request}),
    % Ensure that the node history is updated and the http-server ID is
    % not overridden.
    case is_permanent(NodeMsg) of
        true ->
            {error, <<"Node message is already permanent.">>};
        false ->
            % Merge the request into the server's node message.
            hb_http_server:update_opts(
                fun(ServerOpts) ->
                    hb_http_server:set_opts(Request, ServerOpts)
                end,
                NodeMsg
            )
    end.

is_permanent(NodeMsg) ->
    hb_util:bin(
        hb_ao:get(<<"initialized">>, NodeMsg, not_found, NodeMsg)
    ) =:= <<"permanent">>.

%%% Tests

%% @doc Test that we can get the node message.
config_test() ->
	StoreOpts = hb_test_utils:test_store(),
    Node =
        hb_http_server:start_node(
            Opts = #{ <<"test-config-item">> => <<"test">>, <<"store">> => StoreOpts }
        ),
    {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, Opts),
    ?assertEqual(<<"test">>, hb_ao:get(<<"test-config-item">>, Res, Opts)).

%% @doc Test that we can't get the node message if the requested key is private.
priv_inaccessible_test() ->
    Wallet = ar_wallet:new(),
    Node = hb_http_server:start_node(
        #{
            <<"test-config-item">> => <<"test">>,
            <<"priv-key">> => <<"BAD">>,
            <<"identities">> =>
                #{ <<"test">> => #{ <<"priv-wallet">> => Wallet } }
        }
    ),
    {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
    ?event({res, Res}),
    ?assertEqual(<<"test">>, hb_ao:get(<<"test-config-item">>, Res, #{})),
    ?assertEqual(not_found, hb_ao:get(<<"priv-key">>, Res, #{})),
    % An identity of the node is listed by its address, without its wallet.
    ?assertEqual(
        #{ <<"test">> => #{ <<"address">> => hb:address(Wallet) } },
        hb_maps:get(<<"identities">>, Res, not_found, #{})
    ).

%% @doc A request that carries a link to a message the node does not hold is
%% answered when its path does not read the link.
unread_request_link_test() ->
    Wallet = ar_wallet:new(),
    Node =
        hb_http_server:start_node(
            #{
                <<"priv-wallet">> => Wallet,
                <<"store">> => hb_test_utils:test_store()
            }
        ),
    Missing = hb_util:human_id(crypto:strong_rand_bytes(32)),
    ?assertEqual(
        {ok, hb_util:human_id(ar_wallet:to_address(Wallet))},
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"/~meta@1.0/info/address">>,
                <<"x">> => <<"ao:", Missing/binary>>
            },
            #{}
        )
    ),
    % The node's `committer' is the URI of the committer of its key.
    ?assertEqual(
        {ok, hb_test_utils:committer(Wallet)},
        hb_http:get(Node, <<"/~meta@1.0/info/committer">>, #{})
    ),
    % The node's wallet is the message of that committer, whose URI is in host
    % form.
    ?assertEqual(
        {ok, hb_test_utils:committer(Wallet)},
        hb_uri:to(
            Wallet,
            #{ <<"scheme">> => <<"did">>, <<"characters">> => <<"host">> },
            #{}
        )
    ).

%% @doc Test that we can't set the node message if the request is not signed by
%% the owner of the node.
unauthorized_set_node_msg_fails_test() ->
	StoreOpts = hb_test_utils:test_store(),
    Node =
        hb_http_server:start_node(
            Opts = #{ <<"store">> => StoreOpts, <<"priv-wallet">> => ar_wallet:new() }
        ),
    {error, _} =
        hb_http:post(
            Node,
            hb_message:commit(
                #{
                    <<"path">> => <<"/~meta@1.0/info">>,
                    <<"type">> => <<"node-message">>,
                    <<"evil-config-item">> => <<"BAD">>
                },
                Opts#{ <<"priv-wallet">> => ar_wallet:new() }
            ),
            #{}
        ),
    {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, Opts),
    ?assertEqual(not_found, hb_ao:get(<<"evil-config-item">>, Res, Opts)),
    ?assertEqual(0, length(hb_ao:get(<<"node-history">>, Res, [], Opts))),
    % A client's `~message@1.0/commit' is not signed with the node key, through
    % `httpsig@1.0' or a codec that commits through it.
    lists:foreach(
        fun(Device) ->
            ?assertNotMatch(
                {ok, _},
                hb_http:post(
                    Node,
                    #{
                        <<"path">> => <<"/~message@1.0/commit">>,
                        <<"commitment-device">> => Device,
                        <<"k">> => <<"v">>
                    },
                    #{}
                )
            )
        end,
        [<<"httpsig@1.0">>, <<"flat@1.0">>]
    ),
    % A client's own `secret-provider' in `~auth-hook@1.0' gets no node key.
    ?assertNotMatch(
        {ok, _},
        hb_http:post(
            Node,
            #{
                <<"path">> => <<"/~auth-hook@1.0/request">>,
                <<"secret-provider">> =>
                    #{
                        <<"device">> => <<"message@1.0">>,
                        <<"secret">> => <<"test">>,
                        <<"finalize-path">> => <<"commit">>
                    },
                <<"request">> => #{ <<"path">> => <<"/hello">> },
                <<"body">> => <<"x">>
            },
            #{}
        )
    ),
    % A request to another device does not change the node message.
    NodeMsg = hb_http_server:get_opts(),
    hb_http:get(Node, <<"/~test-device@1.0/postprocess?body=x">>, #{}),
    ?assertEqual(NodeMsg, hb_http_server:get_opts()).

%% @doc Test that we can set the node message if the request is signed by the
%% owner of the node.
authorized_set_node_msg_succeeds_test() ->
	StoreOpts = hb_test_utils:test_store(),
    Owner = ar_wallet:new(),
    Node = hb_http_server:start_node(
        Opts = #{
            <<"operator">> => hb_test_utils:committer(Owner),
            <<"test-config-item">> => <<"test">>,
			<<"store">> => StoreOpts
        }
    ),
    NodeMsg = hb_http_server:get_opts(),
    {ok, SetRes} =
        hb_http:post(
            Node,
            hb_message:commit(
                #{
                    <<"path">> => <<"/~meta@1.0/info">>,
                    <<"type">> => <<"node-message">>,
                    <<"test-config-item">> => <<"test2">>
                },
                Opts#{ <<"priv-wallet">> => Owner }
            ),
            Opts
        ),
    ?event({res, SetRes}),
    % Only the keys of the request and `node-history' change in the node
    % message.
    Changed =
        [<<"path">>, <<"type">>, <<"test-config-item">>, <<"node-history">>],
    ?assertEqual(
        maps:without(Changed, NodeMsg),
        maps:without(Changed, hb_http_server:get_opts())
    ),
    {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, Opts),
    ?event({res, Res}),
    ?assertEqual(<<"test2">>, hb_ao:get(<<"test-config-item">>, Res, Opts)),
    ?assertEqual(1, length(hb_ao:get(<<"node-history">>, Res, [], Opts))).

%% @doc Authorization requires a verified signature over the operation type,
%% and returns only the fields covered by the requested role.
typed_authorization_test() ->
    Owner = ar_wallet:new(),
    Other = ar_wallet:new(),
    Opts = #{ <<"operator">> => hb_test_utils:committer(Owner) },
    Fields = #{ <<"type">> => <<"node-message">>, <<"setting">> => <<"signed">> },
    Signed = hb_message:commit(Fields, #{ <<"priv-wallet">> => Owner }),
    UnsignedType = hb_message:commit(
        maps:remove(<<"type">>, Fields), #{ <<"priv-wallet">> => Owner }
    ),
    lists:foreach(
        fun(Msg) ->
            ?assertEqual({error, not_authorized},
                lib_meta:is_authorized(<<"node-message">>, admin, Msg, Opts))
        end,
        [
            Fields,
            hb_message:commit(Fields, #{ <<"priv-wallet">> => Other }),
            Signed#{ <<"setting">> => <<"forged">> },
            hb_message:commit(
                Fields#{ <<"type">> => <<"cache-write">> },
                #{ <<"priv-wallet">> => Owner }
            ),
            hb_message:commit(
                UnsignedType#{ <<"type">> => <<"node-message">> },
                #{ <<"priv-wallet">> => Other }
            ),
            hb_private:set(
                Fields, <<"http-request">>, Signed, Opts
            )
        ]
    ),
    Extended = hb_message:commit(
        Signed#{ <<"unsigned-setting">> => <<"ignore">> },
        #{ <<"priv-wallet">> => Other }
    ),
    {ok, Authorized} =
        lib_meta:is_authorized(<<"node-message">>, admin, Extended, Opts),
    ?assertEqual(Fields, hb_message:uncommitted(Authorized)),
    ?assert(hb_message:verify(Authorized, all, Opts)),
    AdminOpts = Opts#{ <<"admin">> => hb_test_utils:committer(Other) },
    ?assertEqual({error, not_authorized},
        lib_meta:is_authorized(<<"node-message">>, admin, Signed, AdminOpts)),
    ?assertMatch({ok, _},
        lib_meta:is_authorized(<<"node-message">>, admin,
            hb_message:commit(Fields, #{ <<"priv-wallet">> => Other }), AdminOpts)),
    ?assertMatch({ok, _},
        lib_meta:is_authorized(<<"node-message">>, operator, Signed, AdminOpts)),
    % An operator given as an address is the committer of the address.
    ?assertMatch({ok, _},
        lib_meta:is_authorized(<<"node-message">>, operator, Signed,
            #{ <<"operator">> => hb:address(Owner) })).

%% @doc A signed child ID authorizes its committed fields, not extensions.
nested_authorization_test() ->
    Wallet = ar_wallet:new(),
    Opts = #{ <<"priv-wallet">> => Wallet, <<"store">> => hb_test_utils:test_store() },
    Child = hb_message:commit(#{ <<"setting">> => <<"signed">> }, Opts),
    Msg = hb_message:commit(
        #{ <<"type">> => <<"node-message">>, <<"child">> => Child }, Opts
    ),
    Extended = Msg#{ <<"child">> => Child#{ <<"unsigned-setting">> => <<"ignore">> } },
    ?assert(hb_message:verify(Extended, all, Opts)),
    {ok, Authorized} =
        lib_meta:is_authorized(<<"node-message">>, admin, Extended, Opts),
    ?assertEqual(Child, hb_maps:get(<<"child">>, Authorized, Opts)),
    {ok, _} = hb_cache:write(Child, Opts),
    ChildID = hb_message:id(Child, all, Opts),
    Linked = Msg#{ <<"child">> => {link, <<"ao:", ChildID/binary>>, #{}} },
    {ok, Loaded} = lib_meta:is_authorized(<<"node-message">>, admin, Linked, Opts),
    LoadedChild = hb_maps:get(<<"child">>, Loaded, Opts),
    ?assertEqual(ChildID, hb_message:id(LoadedChild, all, Opts)),
    ?assertEqual(hb_message:uncommitted(Child), hb_message:uncommitted(LoadedChild)),
    {ok, _} = hb_cache:write(Msg, Opts),
    MsgID = hb_message:id(Msg, all, Opts),
    {ok, Cached} = lib_meta:is_authorized(<<"node-message">>, admin,
        {link, <<"ao:", MsgID/binary>>, #{}}, Opts),
    ?assertEqual(MsgID, hb_message:id(Cached, all, Opts)),
    ?assertEqual({error, not_authorized},
        lib_meta:is_authorized(<<"node-message">>, admin,
            Msg#{ <<"child">> => Child#{ <<"setting">> => <<"forged">> } }, Opts)).

%% @doc HTTP updates ignore unsigned fields and reject unsigned operation types.
node_message_signed_fields_test() ->
    Wallet = ar_wallet:new(),
    Opts = #{ <<"priv-wallet">> => Wallet },
    Node = hb_http_server:start_node(Opts),
    Fields = #{
        <<"path">> => <<"/~meta@1.0/info">>,
        <<"test-config-item">> => <<"signed">>
    },
    UnsignedType = (hb_message:commit(Fields, Opts))#{ <<"type">> => <<"node-message">> },
    ?assertMatch({error, #{ <<"status">> := 403 }},
        hb_http:post(Node, UnsignedType, Opts)),
    Signed = hb_message:commit(Fields#{ <<"type">> => <<"node-message">> }, Opts),
    ?assertMatch({ok, _},
        hb_http:post(Node, Signed#{ <<"unsigned-setting">> => <<"ignore">> }, Opts)),
    {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, Opts),
    ?assertEqual(<<"signed">>, hb_ao:get(<<"test-config-item">>, Res, Opts)),
    ?assertEqual(not_found, hb_ao:get(<<"unsigned-setting">>, Res, Opts)),
    ?assertEqual(1, length(hb_ao:get(<<"node-history">>, Res, Opts))).

%% @doc Optional signatures must still authenticate configuration writes.
invalid_optional_signature_cannot_update_node_test() ->
    Owner = ar_wallet:new(),
    Opts = #{
        <<"operator">> => hb_test_utils:committer(Owner),
        <<"priv-wallet">> => ar_wallet:new(),
        <<"force-signed-requests">> => false,
        <<"test-config-item">> => <<"original">>,
        <<"store">> => hb_test_utils:test_store()
    },
    Node = hb_http_server:start_node(Opts),
    Signed = hb_message:commit(
        #{
            <<"path">> => <<"/~meta@1.0/info">>,
            <<"type">> => <<"node-message">>,
            <<"test-config-item">> => <<"authorized">>
        },
        Opts#{ <<"priv-wallet">> => Owner }
    ),
    Forged = Signed#{ <<"test-config-item">> => <<"forged">> },
    ?assertNot(hb_message:verify(Forged, all, Opts)),
    ?assertMatch({error, #{ <<"status">> := 400 }},
        hb_http:post(Node, Forged, Opts)),
    {ok, Before} = hb_http:get(Node, <<"/~meta@1.0/info">>, Opts),
    ?assertEqual(<<"original">>, hb_ao:get(<<"test-config-item">>, Before, Opts)),
    ?assertMatch({ok, _}, hb_http:post(Node, Signed, Opts)),
    {ok, After} = hb_http:get(Node, <<"/~meta@1.0/info">>, Opts),
    ?assertEqual(<<"authorized">>, hb_ao:get(<<"test-config-item">>, After, Opts)).

%% @doc Test that an uninitialized node will not run computation.
uninitialized_node_test() ->
    Node = hb_http_server:start_node(#{ <<"initialized">> => false }),
    {error, Res} = hb_http:get(Node, <<"/key1?1.key1=value1">>, #{}),
    ?event({res, Res}),
    ?assertEqual(<<"Node must be initialized before use.">>, Res).

%% @doc Test that a permanent node message cannot be changed.
permanent_node_message_test() ->
	StoreOpts = hb_test_utils:test_store(),
    Owner = ar_wallet:new(),
    Node = hb_http_server:start_node(
        Opts =#{
            <<"operator">> => hb_test_utils:committer(Owner),
            <<"initialized">> => false,
            <<"test-config-item">> => <<"test">>,
			<<"store">> => StoreOpts
        }
    ),
    {ok, SetRes1} =
        hb_http:post(
            Node,
            hb_message:commit(
                #{
                    <<"path">> => <<"/~meta@1.0/info">>,
                    <<"type">> => <<"node-message">>,
                    <<"test-config-item">> => <<"test2">>,
                    <<"initialized">> => <<"permanent">>
                },
                Opts#{ <<"priv-wallet">> => Owner }
            ),
            Opts
        ),
    ?event({set_res, SetRes1}),
    {ok, Res} = hb_http:get(Node, #{ <<"path">> => <<"/~meta@1.0/info">> }, Opts),
    ?event({get_res, Res}),
    ?assertEqual(<<"test2">>, hb_ao:get(<<"test-config-item">>, Res, Opts)),
    {error, SetRes2} =
        hb_http:post(
            Node,
            hb_message:commit(
                #{
                    <<"path">> => <<"/~meta@1.0/info">>,
                    <<"type">> => <<"node-message">>,
                    <<"test-config-item">> => <<"bad-value">>
                },
                Opts#{ <<"priv-wallet">> => Owner }
            ),
            Opts
        ),
    ?event({set_res, SetRes2}),
    {ok, Res2} = hb_http:get(Node, #{ <<"path">> => <<"/~meta@1.0/info">> }, Opts),
    ?event({get_res, Res2}),
    ?assertEqual(<<"test2">>, hb_ao:get(<<"test-config-item">>, Res2, Opts)),
    ?assertEqual(1, length(hb_ao:get(<<"node-history">>, Res2, [], Opts))),
    % A node given the atom `permanent', as a config file gives it, refuses too.
    ?assertMatch(
        {error, _},
        hb_http:post(
            hb_http_server:start_node(
                Opts#{
                    <<"initialized">> => permanent,
                    <<"priv-wallet">> => ar_wallet:new()
                }
            ),
            hb_message:commit(
                #{
                    <<"path">> => <<"/~meta@1.0/info">>,
                    <<"type">> => <<"node-message">>,
                    <<"test-config-item">> => <<"bad-value">>
                },
                Opts#{ <<"priv-wallet">> => Owner }
            ),
            Opts
        )
    ).

%% @doc An unclaimed node cannot be remotely claimed or reconfigured.
unclaimed_node_rejects_mutation_test() ->
    Owner = ar_wallet:new(),
    Node = hb_http_server:start_node(
        Opts = #{
            <<"operator">> => unclaimed,
            <<"test-config-item">> => <<"test">>,
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => hb_test_utils:test_store()
        }
    ),
    Req = #{
        <<"path">> => <<"/~meta@1.0/info">>,
        <<"type">> => <<"node-message">>,
        <<"operator">> => hb_test_utils:committer(Owner),
        <<"test-config-item">> => <<"changed">>
    },
    lists:foreach(
        fun(Request) -> ?assertMatch({error, _}, hb_http:post(Node, Request, Opts)) end,
        [Req, hb_message:commit(Req, #{ <<"priv-wallet">> => Owner })]
    ),
    {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, Opts),
    ?assertEqual(<<"test">>, hb_ao:get(<<"test-config-item">>, Res, Opts)),
    ?assertEqual(0, length(hb_ao:get(<<"node-history">>, Res, [], Opts))).

%% Test that we can use a hook upon a request.
request_response_hooks_test() ->
    Parent = self(),
    Node = hb_http_server:start_node(
        #{
            <<"on">> =>
                #{
                    <<"request">> =>
                        #{
                            <<"device">> => #{
                                <<"request">> =>
                                    fun(_, #{ <<"body">> := Msgs }, _) ->
                                        Parent ! {hook, request},
                                        {ok, #{ <<"body">> => Msgs} }
                                    end
                            }
                        },
                    <<"response">> =>
                        #{
                            <<"device">> => #{
                                <<"response">> =>
                                    fun(_, #{ <<"body">> := Msgs }, _) ->
                                        Parent ! {hook, response},
                                        {ok, #{ <<"body">> => Msgs} }
                                    end
                            }
                        }
                }
        }),
    {ok, _} = hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
    % Receive both of the responses from the hooks, if possible.
    Res =
        receive
            {hook, request} ->
                receive {hook, response} -> true after 100 -> false end
            after 100 ->
                false
        end,
    ?assert(Res).

%% @doc Test that we can halt a request if the hook returns an error.
halt_request_test() ->
    Node = hb_http_server:start_node(
        #{
            <<"on">> =>
                #{
                    <<"request">> =>
                        #{
                            <<"device">> => #{
                                <<"request">> =>
                                    fun(_, _, _) ->
                                        {error, <<"Bad">>}
                                    end
                            }
                        }
                }
        }),
    {error, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
    ?assertEqual(<<"Bad">>, Res).

%% @doc Test that a hook can modify a request.
modify_request_test() ->
    Node = hb_http_server:start_node(
        #{
            <<"on">> =>
                #{
                    <<"request">> =>
                        #{
                            <<"device">> => #{
                                <<"request">> =>
                                    fun(_, #{ <<"body">> := [M|Ms] }, _) ->
                                        {
                                            ok,
                                            #{
                                                <<"body">> =>
                                                    [
                                                        M#{
                                                            <<"added">> =>
                                                                <<"value">>
                                                        }
                                                    |
                                                        Ms
                                                    ]
                                            }
                                        }
                                    end
                            }
                        }
                }
        }),
    {ok, Res} = hb_http:get(Node, <<"//added">>, #{}),
    ?assertEqual(<<"value">>, Res),
    % The hook raises on `GET /', whose `body' is empty: 500, with its message.
    ?assertMatch(
        {failure, <<"Handler for hook `request` raised", _/binary>>},
        hb_http:get(Node, <<"/">>, #{})
    ).

%% @doc Test that version information is available and returned correctly.
buildinfo_test() ->
    Node = hb_http_server:start_node(#{}),
    BuildInfo = build_info(),
    ?assertEqual(
        {ok, <<"HyperBEAM">>},
        hb_http:get(Node, <<"/~meta@1.0/build/node">>, #{})
    ),
    ?assertEqual(
        {ok, ?HYPERBEAM_VERSION},
        hb_http:get(Node, <<"/~meta@1.0/build/version">>, #{})
    ),
    ?assertEqual(
        {ok, maps:get(<<"source">>, BuildInfo)},
        hb_http:get(Node, <<"/~meta@1.0/build/source">>, #{})
    ),
    ?assertEqual(
        {ok, maps:get(<<"source-short">>, BuildInfo)},
        hb_http:get(Node, <<"/~meta@1.0/build/source-short">>, #{})
    ),
    ?assertEqual(
        {ok, maps:get(<<"build-time">>, BuildInfo)},
        hb_http:get(Node, <<"/~meta@1.0/build/build-time">>, #{})
    ).
