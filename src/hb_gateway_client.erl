%%% @doc Implementation of Arweave's GraphQL API to gain access to specific 
%%% items of data stored on the network.
%%% 
%%% This module must be used to get full HyperBEAM `structured@1.0' form messages
%%% from data items stored on the network, as Arweave gateways do not presently
%%% expose all necessary fields to retrieve this information outside of the
%%% GraphQL API. When gateways integrate serving in `httpsig@1.0' form, this
%%% module will be deprecated.
-module(hb_gateway_client).
%% Raw access primitives:
-export([query/2, query/3, query/4, query/5]).
-export([read/2, data/2, result_to_message/2, item_spec/0]).
-export([assignments/2]).
%% Application-specific data access functions:
-export([location/2, assignments/2]).
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

%% @doc Gives the fields of a transaction that are needed to construct an
%% ANS-104 message.
item_spec() ->
    <<"""
        node {
            id
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
        <<"multirequest-accept-status">> => 200,
        <<"multirequest-responses">> => 1,
        <<"path">> => <<"/arweave/raw/", ID/binary>>,
        <<"method">> => <<"GET">>
    },
    case hb_http:request(Req, Opts) of
        {ok, Res} ->
            ?event(gateway,
                {data,
                    {id, ID},
                    {response, Res},
                    {body, hb_ao:get(<<"body">>, Res, <<>>, Opts)}
                }
            ),
            {ok, hb_ao:get(<<"body">>, Res, <<>>, Opts)};
        Res ->
            ?event(gateway, {request_error, {id, ID}, {response, Res}}),
            {error, no_viable_gateway}
    end.

%% @doc Find the location of the scheduler based on its ID, through GraphQL.
location(Address, Opts) ->
    Query =
        <<"query($Addresses: [String!]!) { ",
                "transactions(",
                "owners: $Addresses, ",
                "tags: { name: \"Type\" values: [\"Location\", \"Scheduler-Location\"] }, ",
                "first: 1",
            "){ ",
                "edges { ",
                    (item_spec())/binary ,
                " } ",
            "} ",
        "}">>,
    Variables = #{ <<"Addresses">> => [Address] },
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
assignments(ProcessId, Opts) ->
        fetch_all_assignments(ProcessId, undefined, [], Opts).

%% @doc Helper function to recursively fetch all assignment pages
fetch_all_assignments(ProcessId, Cursor, AccResults, Opts) ->
        Query =
            <<"query($Processes: [String!]!, $After: String) { ",
                "transactions(",
                    "tags: ["
                "{ name: \"type\" values: [\"Assignment\"] }, "
                "{ name: \"variant\" values: [\"ao.N.1\"] }, ",
                "{ name: \"process\" values: $Processes } ",
                "], "
                % "sort: HEIGHT_ASC, "
                "first: 100, "
                "after: $After "
                "){ ",
                    "pageInfo { hasNextPage } "
                    "edges { ",
                        (item_spec())/binary ,
                    " } ",
                "} ",
            "}">>,
        Variables = case Cursor of
            undefined -> #{ <<"Processes">> => [ProcessId] };
            _ -> #{ <<"Processes">> => [ProcessId], <<"After">> => Cursor }
        end,
        GqlRes = query(Query, Variables, Opts),
        case GqlRes of
            {error, Reason} ->
                ?event({process_assignments, {query, Query}, {error, Reason}}),
                {error, Reason};
            {ok, GqlMsg} ->
                ?event(j, {scheduler_location_req, {query, Query}, {response, GqlMsg}}),
                Edges =
                    hb_ao:get(
                        <<"data/transactions/edges">>,
                        GqlMsg,
                        [],
                        Opts
                    ),
                Count = length(Edges),
                case Count of
                    0 when AccResults =:= [] ->
                        {error, not_found};
                    _ ->

                        ?event(page, {edges, Edges}),
                        RawTags = [hb_ao:get(<<"node/tags">>, Edge, Opts) || Edge <- Edges],
                        ?event(page, {raw_tags, RawTags}),
                        Slots = [
                            hb_ao:get(<<"value">>, hd(lists:filter(fun(Tag) -> hb_ao:get(<<"name">>, Tag, Opts) == <<"slot">> end, Tag)), Opts)
                            ||
                            Tag <- RawTags
                        ],
                        ?event(page, {slots, {explicit, Slots}}),
                        Results =
                            lists:map(
                                fun(Edge) ->
                                    ?event(j, {edge_start, Edge}),
                                    Node = hb_ao:get(<<"node">>, Edge, Opts),
                                    Id = hb_ao:get(<<"id">>, Node, Opts),
                                    hb_util:ok(result_to_message(Id, Node, Opts))
                                end,
                                Edges
                            ),
                        AllResults = AccResults ++ Results,
                        % Check if there are more pages
                        HasNextPage = hb_ao:get(<<"data/transactions/pageInfo/hasNextPage">>, GqlMsg, false, Opts),
                        ?event(page, {do_next, {has_next_page, HasNextPage}}),
                        case HasNextPage of
                            true ->
                                % Get the cursor from the last edge
                                LastEdge = lists:last(Edges),
                                NextCursor = hb_ao:get(<<"cursor">>, LastEdge, Opts),
                                ?event(page, {fetching_next_page, {cursor, NextCursor}}),
                                fetch_all_assignments(ProcessId, NextCursor, AllResults, Opts);
                            _ ->
                                ?event(j,
                                    {found_via_graphql,
                                        {process_id, ProcessId},
                                        {total_results, length(AllResults)}
                                    },
                                    Opts
                                ),
                                {ok, hb_cache:ensure_all_loaded(AllResults, Opts)}
                        end
                end
        end.
        
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
result_to_message(ExpectedID, Item, Opts) ->
    GQLOpts =
        Opts#{
            hashpath => ignore,
            cache_control => [<<"no-cache">>, <<"no-store">>]
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
    RawTags = hb_maps:get(<<"tags">>, Item, tags_not_found, GQLOpts),
    TestTags = RawTags,
    Tags = 
        lists:map(
            fun(Tag) ->
                ?event(j, {mapping_raw_tags, {tag, Tag}}),
                Name = maps:get(<<"name">>, Tag),
                Value = maps:get(<<"value">>, Tag),
                case byte_size(Name) >= 5 andalso binary:part(Name, byte_size(Name) - 5, 5) =:= <<" link">> of 
                    true -> 
                        Start = binary:part(Name, 0, byte_size(Name) - 5),
                        LinkName = <<Start/binary, "+link">>, 
                        Ret = #{ <<"name">> => LinkName, <<"value">> => Value},
                        ?event(j, {tag_ret, Ret}),
                        Ret;
                    _ ->
                        #{ <<"name">> => Name, <<"value">> => Value}
                end
            end,
            RawTags
        ),
    ?event(j, {ans_104, {tags, Tags}, {test_tags, TestTags}}),
	Signature =
        hb_util:decode(
            hb_maps:get(<<"signature">>, Item, not_found, GQLOpts)
        ),
	SignatureType =
        case byte_size(Signature) of
            65 -> {ecdsa, 256};
            512 -> {rsa, 65537};
            _ -> unsupported_tx_signature_type
        end,
    TX =
        dev_arweave_common:reset_ids(#tx {
            format = ans104,
            anchor =
                normalize_null(hb_maps:get(<<"anchor">>, Item, not_found, GQLOpts)),
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
            owner =
                hb_util:decode(
                    hb_util:deep_get(<<"owner/key">>, Item, GQLOpts)
                ),
            tags =
                [
                    {Name, Value}
                ||
                    #{<<"name">> := Name, <<"value">> := Value} <- Tags
                ],
            data_size = DataSize,
            data = Data
        }),
    ?event({raw_ans104, TX}),
    ?event({ans104_form_response, TX}),
    TABM = hb_util:ok(dev_codec_ans104:from(TX, #{}, Opts)),
    ?event({decoded_tabm, TABM}),
    Structured = hb_util:ok(dev_codec_structured:to(TABM, #{}, Opts)),
    % Some graphql nodes do not grant the `anchor' or `last_tx' fields, so we
    % verify the data item and optionally add the explicit keys as committed
    % fields _if_ the node desires it.
    Embedded =
        case try ar_bundles:verify_item(TX) catch _:_ -> false end of
            true ->
                ?event({gql_verify_succeeded, Structured}),
                Structured;
            _ ->
                % The item does not verify on its own, but does the node choose
                % to trust the GraphQL API anyway?
                case hb_opts:get(ans104_trust_gql, false, Opts) of
                    false ->
                        ?event(
                            warning,
                            {gql_verify_failed, returning_unverifiable_tx}
                        ),
                        Structured;
                    true ->
                        % The node trusts the GraphQL API, so we add the explicit
                        % keys as committed fields.
                        ?event(warning,
                            {gql_verify_failed,
                                adding_trusted_fields,
                                {tags, Tags}
                            }
                        ),
                        Comms = hb_maps:get(<<"commitments">>, Structured, #{}, Opts),
                        AttName = hd(hb_maps:keys(Comms, Opts)),
                        Comm = hb_maps:get(AttName, Comms, not_found, Opts),
                        Structured#{
                            <<"commitments">> => #{
                                AttName =>
                                    Comm#{
                                        <<"trusted-keys">> =>
                                            hb_ao:normalize_keys(
                                                [
                                                    hb_ao:normalize_key(Name)
                                                ||
                                                    #{ <<"name">> := Name } <-
                                                        hb_maps:values(
                                                            hb_ao:normalize_keys(
                                                                Tags,
                                                                Opts
                                                            ),
                                                            Opts
                                                        )
                                                ],
												Opts
                                            )
                                    }
                            }
                        }
                end
        end,
    {ok, Embedded}.

normalize_null(null) -> <<>>;
normalize_null(not_found) -> <<>>;
normalize_null(Bin) when is_binary(Bin) -> Bin.

decode_id_or_null(Bin) when byte_size(Bin) > 0 ->
    hb_util:human_id(Bin);
decode_id_or_null(_) ->
    <<>>.

decode_or_null(Bin) when is_binary(Bin) ->
    hb_util:decode(Bin);
decode_or_null(_) ->
    <<>>.

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
    {ok, Res} =
        location(
            <<"fcoN_xJeisVsPXA-trzVAuIiqO3ydLQxM-L4XbrQKzY">>,
            #{}
        ),
    ?event(gateway, {get_scheduler_location_test, Res}),
    ?assertEqual(<<"Scheduler-Location">>, hb_ao:get(<<"Type">>, Res, #{})),
    ?event(gateway, {scheduler_location, {explicit, hb_ao:get(<<"url">>, Res, #{})}}),
    % Will need updating when Legacynet terminates.
    ?assertEqual(<<"https://su-router.ao-testnet.xyz">>, hb_ao:get(<<"url">>, Res, #{})).

%% @doc Test l1 message from graphql
l1_transaction_test() ->
    _Node = hb_http_server:start_node(#{}),
    {ok, Res} = read(<<"uJBApOt4ma3pTfY6Z4xmknz5vAasup4KcGX7FJ0Of8w">>, #{}),
    ?event(gateway, {l1_transaction, Res}),
    Data = maps:get(<<"data">>, Res),
    ?assertEqual(<<"Hello World">>, Data).

%% @doc Test l2 message from graphql
l2_dataitem_test() ->
    _Node = hb_http_server:start_node(#{}),
    {ok, Res} = read(<<"oyo3_hCczcU7uYhfByFZ3h0ELfeMMzNacT-KpRoJK6g">>, #{}),
    ?event(gateway, {l2_dataitem, Res}),
    Data = maps:get(<<"data">>, Res),
    ?assertEqual(<<"Hello World">>, Data).

%% @doc Test optimistic index
ao_dataitem_test() ->
    _Node = hb_http_server:start_node(#{}),
    {ok, Res} = read(<<"oyo3_hCczcU7uYhfByFZ3h0ELfeMMzNacT-KpRoJK6g">>, #{}),
    ?event(gateway, {l2_dataitem, Res}),
    Data = maps:get(<<"data">>, Res),
    ?assertEqual(<<"Hello World">>, Data).
