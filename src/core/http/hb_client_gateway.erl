%%% @doc Implementation of Arweave's GraphQL API to gain access to specific 
%%% items of data stored on the network.
%%% 
%%% This module must be used to get full HyperBEAM `structured@1.0' form messages
%%% from data items stored on the network, as Arweave gateways do not presently
%%% expose all necessary fields to retrieve this information outside of the
%%% GraphQL API. When gateways integrate serving in `httpsig@1.0' form, this
%%% module will be deprecated.
-module(hb_client_gateway).
%% Raw access primitives:
-export([query/2, query/3, query/4, query/5]).
-export([read/2, data/2, result_to_message/2, item_spec/0]).
%% Application-specific data access functions:
-export([device/3, location/2, location/3]).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Get a data item (including data and tags) by its ID, using the node's
%% GraphQL peers.
%% It uses the following GraphQL schema:
%% type Transaction {
%%   id: ID!
%%   anchor: String!
%%   signature: String!
%%   recipient: String!
%%   owner: Owner { address: String! key: String! }!
%%   fee: Amount!
%%   quantity: Amount!
%%   data: MetaData!
%%   tags: [Tag { name: String! value: String! }!]!
%% }
%% type Amount {
%%   winston: String!
%%   ar: String!
%% }
read(ID, Opts) ->
    {Query, Variables} = case maps:is_key(<<"subindex">>, Opts) of
      true -> 
        Tags = subindex_to_tags(maps:get(<<"subindex">>, Opts)),
        {
            <<
                "query($transactionIds: [ID!]!) { ",
                    "transactions(ids: $transactionIds,",
                    "tags: ", (Tags)/binary , ",",
                    "first: 1){ ",
                        "edges { ", (item_spec())/binary , " } ",
                    "} ",
                "} "
            >>,
            #{
                <<"transactionIds">> => [hb_util:human_id(ID)]
            }
        };
      false -> 
        {
            <<
                "query($transactionIds: [ID!]!) { ",
                    "transactions(ids: $transactionIds, first: 1){ ",
                        "edges { ", (item_spec())/binary , " } ",
                    "} ",
                "} "
            >>,
            #{
                <<"transactionIds">> => [hb_util:human_id(ID)]
            }
        }
    end,
    case query(Query, Variables, Opts) of
        {error, Reason} -> {error, Reason};
        {ok, GqlMsg} ->
            case hb_ao:get(<<"data/transactions/edges/1/node">>, GqlMsg, Opts) of
                not_found ->
                    ?event({read_not_found, {id, ID}, {gql_msg, GqlMsg}}),
                    {error, not_found};
                Item ->
                    ?event({read_found, {id, ID}, {item, Item}}),
                    result_to_message(ID, Item, Opts)
            end
    end.

%% @doc Gives the fields needed to construct an Arweave message.
item_spec() ->
    <<"""
        node {
            id
            bundledIn { id }
            anchor
            signature
            recipient
            owner { key }
            fee { winston }
            quantity { winston }
            tags { name value }
            data { size }
        }
        cursor
    """>>.

%% @doc Get the data associated with a transaction by its ID, using the node's
%% Arweave `gateway' peers. The item is expected to be available in its 
%% unmodified (by caches or other proxies) form at the following location:
%%      https://<gateway>/raw/<id>
%% where `<id>' is the base64-url-encoded transaction ID.
data(ID, Opts) ->
    Req = #{
        <<"multirequest-admissible-status">> => 200,
        <<"multirequest-responses">> => 1,
        <<"path">> => <<"/arweave/raw/", ID/binary>>,
        <<"method">> => <<"GET">>
    },
    case hb_http:request(Req, Opts) of
        {ok, Data} when is_binary(Data) -> {ok, Data};
        {ok, Res} ->
            Data =
                case hb_maps:find(<<"data">>, Res, Opts) of
                    {ok, D} -> D;
                    _ -> hb_maps:get(<<"body">>, Res, <<>>, Opts)
                end,
            ?event(gateway,
                {data,
                    {id, ID},
                    {response, Res},
                    {data, Data}
                }
            ),
            {ok, Data};
        Res ->
            ?event(gateway, {request_error, {id, ID}, {response, Res}}),
            {error, no_viable_gateway}
    end.

%% @doc Find the location of the scheduler based on its ID, through GraphQL.
%% The committer of an Arweave address is found by the address. The query asks
%% for the newest record with a `type' of `location', `Location' or
%% `Scheduler-Location', and with the given `variant', if any. An `ao.TN.1'
%% lookup asks only for `Location' and `Scheduler-Location', as HyperBEAM's
%% `location' records are `ao.N.1'. The gateway matches tag names in any case,
%% so `type' and `variant' also find `Type' and `Variant' tags. With
%% `scheduler-legacy-locations' set to `false', the query asks only for a
%% `type' of `location'.
location(Address, Opts) ->
    location(Address, undefined, Opts).
location(Committer, Variant, Opts) ->
    Address = hb_util:committer_address(Committer),
    Types =
        case {hb_opts:get(scheduler_legacy_locations, true, Opts), Variant} of
            {false, _} -> [<<"location">>];
            {true, <<"ao.TN.1">>} -> [<<"Location">>, <<"Scheduler-Location">>];
            {true, _} ->
                [<<"location">>, <<"Location">>, <<"Scheduler-Location">>]
        end,
    VariantTags =
        case Variant of
            undefined -> [];
            _ -> [#{ <<"name">> => <<"variant">>, <<"values">> => [Variant] }]
        end,
    Query =
        <<"query($Addresses: [String!]!, $Tags: [TagFilter!]!) { ",
                "transactions(",
                "owners: $Addresses, ",
                "tags: $Tags, ",
                "first: 1",
            "){ ",
                "edges { ",
                    (item_spec())/binary ,
                " } ",
            "} ",
        "}">>,
    Variables = #{
        <<"Addresses">> => [Address],
        <<"Tags">> =>
            [#{ <<"name">> => <<"type">>, <<"values">> => Types } | VariantTags]
    },
    case query(Query, Variables, Opts) of
        {error, Reason} ->
            ?event({scheduler_location, {query, Query}, {error, Reason}}),
            {error, Reason};
        {ok, GqlMsg} ->
            ?event({scheduler_location_req, {query, Query}, {response, GqlMsg}}),
            case hb_ao:get(<<"data/transactions/edges/1/node">>, GqlMsg, Opts) of
                not_found ->
                    ?event(scheduler_location,
                        {graphql_scheduler_location_not_found,
                            {address, Address}
                        }
                    ),
                    {error, not_found};
                Item = #{ <<"id">> := ID } ->
                    ?event(scheduler_location,
                        {found_via_graphql,
                            {address, Address},
                            {id, ID}
                        }
                    ),
                    result_to_message(ID, Item, Opts)
            end
    end.

%% @doc AO-Core devices are defined primarily by their specification IDs. To find
%% compatible device implementations we must query for messages with the
%% appropriate tags and signatures.
device(SpecID, TrustedSigners, Opts) ->
    Queries = device_queries(SpecID, TrustedSigners, Opts),
    case device_result(Queries, Opts) of
        {error, _} = Error ->
            Error;
        {ok, []} ->
            ?event(
                device_load,
                {no_viable_device_implementations, {device, SpecID}}
            ),
            {error, not_found};
        {ok, Items} ->
            ?event(
                device_load,
                {implementations_found_via_graphql,
                    {device, SpecID},
                    {implementations, length(Items)}
                }
            ),
            {
                ok,
                [
                    ID
                ||
                    #{ <<"node">> := #{ <<"id">> := ID } } <- Items
                ]
            }
    end.

device_result([], _Opts) ->
    {ok, []};
device_result([{Query, Variables} | Rest], Opts) ->
    case query(Query, Variables, Opts) of
        {error, Reason} ->
            ?event({device_read_failed, {query, Query}, {error, Reason}}),
            {error, Reason};
        {ok, GqlMsg} ->
            ?event({device_query_success, {query, Query}, {response, GqlMsg}}),
            case hb_ao:get(<<"data/transactions/edges">>, GqlMsg, Opts) of
                X when X =:= not_found orelse X =:= [] ->
                    device_result(Rest, Opts);
                Items ->
                    {ok, Items}
            end
    end.

device_queries(SpecID, TrustedSigners, Opts) ->
    SignerPolicies =
        [
            {Address, signer_valid_until_height(Signer, Opts)}
        ||
            Signer <- TrustedSigners,
            % GraphQL finds the items of the committer of an Arweave address
            % by the address.
            {ok, Address} <-
                [hb_committer:address(trusted_signer_address(Signer, Opts))]
        ],
    case lists:any(fun({_Signer, Height}) -> Height =/= undefined end, SignerPolicies) of
        false ->
            Signers = [Signer || {Signer, _Height} <- SignerPolicies],
            [device_query(SpecID, Signers, undefined)];
        true ->
            [
                device_query(SpecID, [Signer], Height)
            ||
                {Signer, Height} <- SignerPolicies
            ]
    end.

trusted_signer_address(Signer, Opts) when is_binary(Signer) ->
    hb_committer:uri(Signer, Opts);
trusted_signer_address(Signer, Opts) when is_map(Signer) ->
    hb_committer:uri(hb_maps:get(<<"address">>, Signer, undefined, Opts));
trusted_signer_address(_Signer, _Opts) ->
    undefined.

signer_valid_until_height(Signer, Opts) when is_map(Signer) ->
    case hb_maps:get(<<"valid-until-height">>, Signer, undefined, Opts) of
        undefined -> undefined;
        Height -> hb_util:int(Height)
    end;
signer_valid_until_height(_Signer, _Opts) ->
    undefined.

device_query(SpecID, TrustedSigners, ValidUntilHeight) ->
    BlockFilter =
        case ValidUntilHeight of
            undefined -> <<>>;
            _ -> <<"block: { max: $validUntilHeight }, ">>
        end,
    ValidUntilVar =
        case ValidUntilHeight of
            undefined -> <<>>;
            _ -> <<", $validUntilHeight: Int">>
        end,
    Query =
        <<"query($specid: [String!], $trusted: [String!]", ValidUntilVar/binary, ") { ",
                "transactions(",
                "owners: $trusted, ",
                "tags: { name: \"implements-device\" values: $specid }, ",
                BlockFilter/binary,
                "first: 1",
            "){ ",
                "edges { ",
                    (item_spec())/binary ,
                " } ",
            "} ",
        "}">>,
    Variables0 = #{ <<"trusted">> => TrustedSigners, <<"specid">> => [SpecID] },
    Variables =
        case ValidUntilHeight of
            undefined -> Variables0;
            _ -> Variables0#{ <<"validUntilHeight">> => ValidUntilHeight }
        end,
    {Query, Variables}.

%% @doc Run a GraphQL request encoded as a binary. The node message may contain 
%% a list of URLs to use, optionally as a tuple with an additional map of options
%% to use for the request.
query(Query, Opts) ->
    query(Query, undefined, Opts).
query(Query, Variables, Opts) ->
    query(Query, Variables, undefined, Opts).
query(Query, Variables, Node, Opts) ->
    query(Query, Variables, Node, undefined, Opts).
query(Query, Variables, Node, Operation, Opts) ->
    % Either use the given node if provided, or use the local machine's routes
    % to find the GraphQL endpoint.
    Path =
        case Node of
            undefined -> <<"/graphql">>;
            _ -> << Node/binary, "/graphql">>
        end,
    ?event(graphql,
        {request,
            {path, Path},
            {query, Query},
            {variables, Variables},
            {operation, Operation}
        }
    ),
    CombinedQuery =
        maps:filter(
            fun(_, V) -> V =/= undefined end,
            #{
                <<"query">> => Query,
                <<"variables">> => Variables,
                <<"operationName">> => Operation
            }
        ),
    % Find the routes for the GraphQL API.
    Res = hb_http:request(
        #{
            % Add options for the HTTP request, in case it is being made to
            % many nodes.
            <<"multirequest-responses">> => 1,
            <<"multirequest-admissible-status">> => 200,
            <<"multirequest-admissible">> =>
                #{
                    <<"device">> => <<"query@1.0">>,
                    <<"path">> => <<"has-results">>
                },
            % Main request fields
            <<"method">> => <<"POST">>,
            <<"path">> => <<"/graphql">>,
            <<"content-type">> => <<"application/json">>,
            <<"body">> => hb_json:encode(CombinedQuery)
        },
        Opts
    ),
    case Res of
        {ok, Msg} ->
            {ok, hb_json:decode(hb_ao:get(<<"body">>, Msg, <<>>, Opts))};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Takes a GraphQL item node, matches it with the appropriate data from a
%% gateway, then returns `{ok, ParsedMsg}'.
result_to_message(Item, Opts) ->
    case hb_maps:get(<<"id">>, Item, not_found, Opts) of
        ExpectedID when is_binary(ExpectedID) ->
            result_to_message(ExpectedID, Item, Opts);
        _ ->
            result_to_message(undefined, Item, Opts)
    end.
%% @doc Load an L1 transaction in its native form. The node is read through
%% `hb_ao:get/3', so its keys are in `structured@1.0' form. A node is an L1
%% transaction if its `bundledIn' is null or its fee is not zero. ANS-104
%% items pay no fee. Some gateways give an L1 transaction an empty
%% `bundledIn' ID.
result_to_message(ExpectedID, Item, Opts)
        when map_get(<<"bundledin">>, Item) =:= null;
             map_get(<<"winston">>, map_get(<<"fee">>, Item)) =/= <<"0">> ->
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{ <<"path">> => <<"tx">>, <<"tx">> => ExpectedID },
        Opts
    );
result_to_message(ExpectedID, Item, Opts) ->
    GQLOpts =
        Opts#{
            <<"hashpath">> => ignore,
            <<"cache-control">> => [<<"no-cache">>, <<"no-store">>]
        },
    % We have the headers, so we can get the data.
    Data =
        case hb_maps:get(<<"data">>, Item, not_found, GQLOpts) of
            #{ <<"size">> := Zero } when Zero =:= <<"0">> orelse Zero =:= 0 -> <<>>;
            BinData when is_binary(BinData) -> BinData;
            _ ->
                {ok, Bytes} = data(ExpectedID, Opts),
                Bytes
        end,
    DataSize = byte_size(Data),
    ?event(gateway, {data, {id, ExpectedID}, {data, Data}, {item, Item}}, Opts),
    % Convert the response to an ANS-104 message.
    Tags = hb_maps:get(<<"tags">>, Item, tags_not_found, GQLOpts),
	Signature =
        hb_util:decode(
            hb_maps:get(<<"signature">>, Item, not_found, GQLOpts)
        ),
	Owner =
        hb_util:decode(
            hb_util:deep_get(<<"owner/key">>, Item, GQLOpts)
        ),
	SignatureType =
        case {byte_size(Signature), byte_size(Owner)} of
            {64, _} -> {eddsa, ed25519};
            {65, 42} -> typed_ethereum;
            {65, _} -> ethereum;
            {512, _} -> {rsa, 65537};
            _ -> unsupported_tx_signature_type
        end,
    TX =
        ar_tx:reset_ids(#tx {
            format = ans104,
            anchor =
                decode_or_null(
                    hb_maps:get(<<"anchor">>, Item, not_found, GQLOpts)
                ),
            signature = Signature,
            signature_type = SignatureType,
            target =
                decode_or_null(
                    hb_ao:get_first(
                        [
                            {Item, <<"recipient">>},
                            {Item, <<"target">>}
                        ],
                        GQLOpts
                    )
                ),
            owner = Owner,
            tags =
                [
                    {normalize_graphql_tag_name(Name, Value), Value}
                ||
                    #{<<"name">> := Name, <<"value">> := Value} <- Tags
                ],
            data_size = DataSize,
            data = Data
        }),
    ?event({raw_ans104, TX}),
    ?event({ans104_form_response, TX}),
    TABM = hb_message:convert(TX, tabm, <<"ans104@1.0">>, Opts),
    ?event({decoded_tabm, TABM}),
    Structured = hb_message:convert(TABM, <<"structured@1.0">>, tabm, Opts),
    % Gateways serve only an item's data, and their indexes may leave out the
    % `anchor' or change the tags that its signature covers, so the item built
    % from the result must verify and match the requested ID. Otherwise, the
    % node serves it uncommitted only if it explicitly trusts the index.
    IDMatches =
        ExpectedID =:= undefined orelse
            hb_util:human_id(TX#tx.id) =:= hb_util:human_id(ExpectedID),
    case IDMatches andalso
        (try ar_bundles:verify_item(TX) catch _:_ -> false end)
    of
        true ->
            ?event({gql_verify_succeeded, Structured}),
            {ok, Structured};
        false ->
            case hb_opts:get(ans104_trust_gql, false, Opts) of
                true ->
                    ?event(warning,
                        {gql_verify_failed,
                            serving_uncommitted,
                            {id, ExpectedID}
                        }
                    ),
                    {ok, hb_message:uncommitted(Structured, Opts)};
                false ->
                    ?event(warning,
                        {gql_verify_failed, refusing_item, {id, ExpectedID}}
                    ),
                    {error, unverifiable_item}
            end
    end.

decode_or_null(Bin) when is_binary(Bin) ->
    hb_util:decode(Bin);
decode_or_null(_) ->
    <<>>.

%% @doc Some gateway GraphQL responses can expose TABM link tag names with
%% `+' decoded as a form-space, yielding `balances link' instead of the
%% canonical `balances+link'. Normalize only when the value has HyperBEAM's ID
%% shape, so ordinary tags whose names end in ` link' remain unchanged.
normalize_graphql_tag_name(Name, Value)
        when is_binary(Name), byte_size(Name) >= 5 ->
    Size = byte_size(Name),
    case {
        binary:part(Name, Size - 5, 5),
        ?IS_ID(Value)
    } of
        {<<" link">>, true} ->
            Prefix = binary:part(Name, 0, Size - 5),
            <<Prefix/binary, "+link">>;
        _ ->
            Name
    end;
normalize_graphql_tag_name(Name, _Value) ->
    Name.

%% @doc Takes a list of messages with `name' and `value' fields, and formats
%% them as a GraphQL `tags' argument.
subindex_to_tags(Subindex) ->
    Formatted =
        lists:map(
            fun(Spec) ->
                io_lib:format(
                    "{ name: \"~s\", values: [\"~s\"]}",
                    [
                        hb_ao:get(<<"name">>, Spec),
                        hb_ao:get(<<"value">>, Spec)
                    ]
                )
            end,
            hb_util:message_to_ordered_list(Subindex)
        ),
    ListInner =
        hb_util:bin(
            string:join([lists:flatten(E) || E <- Formatted], ", ")
        ),
    <<"[", ListInner/binary, "]">>.

%%% Tests
device_valid_until_height_query_test() ->
    SpecID = <<"spec">>,
    [Alice, Bob] = [hb_util:human_id(<<N:256>>) || N <- [1, 2]],
    AliceCommitter = hb_committer:uri(Alice),
    BobCommitter = hb_committer:uri(Bob),
    [{BaseQuery, BaseVars}] =
        device_queries(SpecID, [AliceCommitter, BobCommitter], #{}),
    ?assertEqual([Alice, Bob], maps:get(<<"trusted">>, BaseVars)),
    ?assertEqual(nomatch, binary:match(BaseQuery, <<"validUntilHeight">>)),
    [{AliceQuery, AliceVars}, {BobQuery, BobVars}] =
        device_queries(
            SpecID,
            [
                #{
                    <<"address">> => AliceCommitter,
                    <<"valid-until-height">> => 1543210
                },
                BobCommitter
            ],
            #{}
        ),
    ?assertEqual([Alice], maps:get(<<"trusted">>, AliceVars)),
    ?assertEqual(1543210, maps:get(<<"validUntilHeight">>, AliceVars)),
    ?assert(
        binary:match(AliceQuery, <<"block: { max: $validUntilHeight }">>)
            =/= nomatch
    ),
    ?assertEqual([Bob], maps:get(<<"trusted">>, BobVars)),
    ?assertEqual(nomatch, binary:match(BobQuery, <<"validUntilHeight">>)).

graphql_link_tag_name_normalization_test() ->
    ID = <<"5wE86A8Z3QRqYIQBDQY-rws-Lb-b2uaXO7yfdf4_NAg">>,
    ?assertEqual(
        <<"balances+link">>,
        normalize_graphql_tag_name(<<"balances link">>, ID)
    ),
    ?assertEqual(
        <<"26wkmnkmn3u99knn5dukjd0cgyotdiw6jj6eyc_ied4+link">>,
        normalize_graphql_tag_name(
            <<"26wkmnkmn3u99knn5dukjd0cgyotdiw6jj6eyc_ied4 link">>,
            ID
        )
    ),
    ?assertEqual(
        <<"balances link">>,
        normalize_graphql_tag_name(<<"balances link">>, <<"not-an-id">>)
    ),
    ?assertEqual(
        <<"plain link tag">>,
        normalize_graphql_tag_name(<<"plain link tag">>, ID)
    ),
    ?assertEqual(
        <<"already+link">>,
        normalize_graphql_tag_name(<<"already+link">>, ID)
    ).

ans104_no_data_item_test() ->
    % Start a random node so that all of the services come up.
    _Node = hb_http_server:start_node(#{}),
    {ok, Res} = read(<<"BOogk_XAI3bvNWnxNxwxmvOfglZt17o4MOVAdPNZ_ew">>, #{}),
    ?event(gateway, {get_ans104_test, Res}),
    ?event(gateway, {signer, hb_message:signers(Res, #{})}),
    ?assert(true).

%% @doc Test that we can get the scheduler location.
scheduler_location_test() ->
    % Start a random node so that all of the services come up.
    _Node = hb_http_server:start_node(#{}),
    % Look up the legacy scheduler with its variant, as a scheduler lookup does.
    {ok, Res} =
        location(
            <<"fcoN_xJeisVsPXA-trzVAuIiqO3ydLQxM-L4XbrQKzY">>,
            <<"ao.TN.1">>,
            #{}
        ),
    ?event(gateway, {get_scheduler_location_test, Res}),
    ?assertEqual(<<"Scheduler-Location">>, hb_ao:get(<<"Type">>, Res, #{})),
    ?event(gateway, {scheduler_location, {explicit, hb_ao:get(<<"url">>, Res, #{})}}),
    % Will need updating when Legacynet terminates.
    ?assertEqual(<<"https://su-router.ao-testnet.xyz">>, hb_ao:get(<<"url">>, Res, #{})).

%% @doc Test l1 message from graphql
l1_transaction_test() ->
    ID = <<"uJBApOt4ma3pTfY6Z4xmknz5vAasup4KcGX7FJ0Of8w">>,
    Node = hb_http_server:start_node(
        #{ <<"store">> => [#{ <<"store-module">> => hb_store_gateway }] }
    ),
    ClientOpts = #{ <<"store">> => [hb_test_utils:test_store(hb_store_volatile)] },
    {ok, Res} = hb_http:get(Node, ID, ClientOpts),
    ?event(gateway, {l1_transaction, Res}),
    Devices = hb_message:commitment_devices(Res, ClientOpts),
    ?assert(lists:member(<<"tx@1.0">>, Devices)),
    ?assertNot(lists:member(<<"ans104@1.0">>, Devices)),
    ?assert(hb_message:verify(Res, all, ClientOpts)),
    Data = maps:get(<<"data">>, Res),
    ?assertEqual(<<"Hello World">>, Data).

%% @doc Test l2 message from graphql. The gateway serves the item's anchor, so
%% the item verifies and is served with its owner's commitment.
l2_dataitem_test() ->
    _Node = hb_http_server:start_node(#{}),
    {ok, Res} = read(<<"oyo3_hCczcU7uYhfByFZ3h0ELfeMMzNacT-KpRoJK6g">>, #{}),
    ?event(gateway, {l2_dataitem, Res}),
    ?assert(hb_message:verify(Res, all, #{})),
    ?assertEqual(
        [hb_committer:uri(<<"vh-NTHVvlKZqRxc8LyyTNok65yQ55a_PJ1zWLb9G2JI">>)],
        hb_message:signers(Res, #{})
    ),
    Data = maps:get(<<"data">>, Res),
    ?assertEqual(<<"Hello World">>, Data).

%% @doc ed25519 L2 Transaction test. The gateway serves the item's anchor, so
%% the item verifies and is served with its owner's commitment.
l2_dataitem_ed25519_test() ->
    _Node = hb_http_server:start_node(#{}),
    ID = <<"AwrAs-HaBlc8xeI8sw6Wpbi7A0weQWeXYwW20CpX5oM">>,
    {ok, Res} = read(ID, #{}),
    ?event(gateway, {l2_dataitem, Res}),
    ?assert(hb_message:verify(Res, all, #{})),
    ?assertEqual(
        [hb_committer:uri(<<"ejhYD9Cw9VCsVik6yGLoclo3CLRvAITHTZamLY_6ro4">>)],
        hb_message:signers(Res, #{})
    ),
    %% Check Data
    Data = maps:get(<<"data">>, Res),
    ?assertEqual(<<"{\"displayName\":\"Test Hub\",\"description\":\"This is a test hub created in the test suite\",\"externalurl\":\"\",\"image\":\"\"}">>, Data).

%% @doc Test optimistic index
ao_dataitem_test() ->
    _Node = hb_http_server:start_node(#{}),
    {ok, Res} = read(
        <<"oyo3_hCczcU7uYhfByFZ3h0ELfeMMzNacT-KpRoJK6g">>,
        #{ <<"ans104-trust-gql">> => true }
    ),
    ?event(gateway, {l2_dataitem, Res}),
    Data = maps:get(<<"data">>, Res),
    ?assertEqual(<<"Hello World">>, Data).

%% @doc An index that serves an item's anchor lets the item verify against its
%% real signature, so it is served with its commitment. With the anchor left
%% out, the signature cannot be verified: the item is refused unless the node
%% explicitly trusts the index.
anchor_verification_test() ->
    {ok, Bin} = file:read_file(<<"test/arbundles.js/ans104-item-ed25519.bin">>),
    TX = ar_bundles:deserialize(Bin),
    Item =
        #{
            <<"id">> => hb_util:human_id(TX#tx.id),
            <<"anchor">> => hb_util:encode(TX#tx.anchor),
            <<"signature">> => hb_util:encode(TX#tx.signature),
            <<"owner">> => #{ <<"key">> => hb_util:encode(TX#tx.owner) },
            <<"tags">> =>
                [
                    #{ <<"name">> => Name, <<"value">> => Value }
                ||
                    {Name, Value} <- TX#tx.tags
                ],
            <<"data">> => TX#tx.data
        },
    {ok, Verified} = result_to_message(Item, #{}),
    ?assert(hb_message:verify(Verified, all, #{})),
    ?assertEqual(
        [hb_committer:uri(<<"ejhYD9Cw9VCsVik6yGLoclo3CLRvAITHTZamLY_6ro4">>)],
        hb_message:signers(Verified, #{})
    ),
    Unanchored = Item#{ <<"anchor">> => <<>> },
    ?assertEqual({error, unverifiable_item}, result_to_message(Unanchored, #{})),
    {ok, Uncommitted} = result_to_message(
        Unanchored, #{ <<"ans104-trust-gql">> => true }
    ),
    ?assertEqual([], hb_message:signers(Uncommitted, #{})),
    ?assertNot(maps:is_key(<<"commitments">>, Uncommitted)),
    ?assertEqual(TX#tx.data, maps:get(<<"data">>, Uncommitted)),
    ?assertEqual(
        {error, unverifiable_item},
        result_to_message(hb_util:human_id(<<0:256>>), Item, #{})
    ).
