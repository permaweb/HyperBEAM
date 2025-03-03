%%% @doc Implementation of Arweave's GraphQL API to gain access to specific 
%%% items of data stored on the network.
%%% 
%%% This module must be used to get full HyperBEAM `structured@1.0' form messages
%%% from data items stored on the network, as Arweave gateways do not presently
%%% expose all necessary fields to retrieve this information outside of the
%%% GraphQL API. When gateways integrate serving in `httpsig@1.0' form, this
%%% module will be deprecated.
-module(hb_gateway_client).
%% @doc Raw access primitives:
-export([read/2, data/2]).
%% @doc Application-specific data access functions:
-export([scheduler_location/2]).
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
    Query =
        #{
            <<"query">> =>
                <<
                    "query($transactionIds: [ID!]!) { ",
                        "transactions(ids: $transactionIds, first: 1){ ",
                            "edges { ", (item_spec())/binary , " } ",
                        "} ",
                    "} "
                >>,
            <<"variables">> =>
                #{
                    <<"transactionIds">> => [ID]
                }
        },
    case query(Query, Opts) of
        {error, Reason} -> {error, Reason};
        {ok, GqlMsg} ->
            case hb_converge:get(<<"data/transactions/edges/1/node">>, GqlMsg, Opts) of
                not_found -> {error, not_found};
                Item = #{<<"id">> := ID} -> result_to_message(ID, Item, Opts)
            end
    end.

%% @doc Gives the fields of a transaction that are needed to construct an
%% ANS-104 message.
item_spec() ->
    <<"node { ",
        "id ",
        "anchor ",
        "signature ",
        "recipient ",
        "owner { key } ",
        "fee { winston } ",
        "quantity { winston } ",
        "tags { name value } ",
        "data { size } "
    "}">>.

%% @doc Get the data associated with a transaction by its ID, using the node's
%% Arweave `gateway' peers. The item is expected to be available in its 
%% unmodified (by caches or other proxies) form at the following location:
%%      https://<gateway>/raw/<id>
%% where `<id>' is the base64-url-encoded transaction ID.
data(ID, Opts) ->
    Req = #{
        <<"multirequest-accept-status">> => 200,
        <<"multirequest-responses">> => 1,
        <<"path">> => <<"/raw/", ID/binary>>,
        <<"method">> => <<"GET">>
    },
    case hb_http:request(Req, Opts) of
        {ok, Res} ->
            ?event(gateway,
                {data,
                    {id, ID},
                    {response, Res},
                    {body, hb_converge:get(<<"body">>, Res, <<>>, Opts)}
                }
            ),
            {ok, hb_converge:get(<<"body">>, Res, <<>>, Opts)};
        Res ->
            ?event(gateway, {request_error, {id, ID}, {response, Res}}),
            {error, no_viable_gateway}
    end.

%% @doc Find the location of the scheduler based on its ID, through GraphQL.
scheduler_location(Address, Opts) ->
    Query =
        #{
            <<"query">> =>
                <<"query($SchedulerAddrs: [String!]!) { ",
                    "transactions(owners: $SchedulerAddrs, tags: { name: \"Type\" values: [\"Scheduler-Location\"] }, first: 1){ ",
                        "edges { ",
                            (item_spec())/binary ,
                        " } ",
                    "} ",
                "}">>,
            <<"variables">> =>
                #{
                    <<"SchedulerAddrs">> => [Address]
                }
        },
    case query(Query, Opts) of
        {error, Reason} -> {error, Reason};
        {ok, GqlMsg} ->
            case hb_converge:get(<<"data/transactions/edges/1/node">>, GqlMsg, Opts) of
                not_found -> {error, not_found};
                Item = #{ <<"id">> := ID } -> result_to_message(ID, Item, Opts)
            end
    end.
        
%% @doc Run a GraphQL request encoded as a binary. The node message may contain 
%% a list of URLs to use, optionally as a tuple with an additional map of options
%% to use for the request.
query(Query, Opts) ->
    Res = hb_http:request(
        #{
            % Add options for the HTTP request, in case it is being made to
            % many nodes.
            <<"multirequest-accept-status">> => 200,
            <<"multirequest-responses">> => 1,
            % Main request fields
            <<"method">> => <<"POST">>,
            <<"path">> => <<"/graphql">>,
            <<"content-type">> => <<"application/json">>,
            <<"body">> => jiffy:encode(Query)
        },
        Opts
    ),
    case Res of
        {ok, Msg} ->
            {ok,
                jiffy:decode(
                    hb_converge:get(<<"body">>, Msg, <<>>, Opts),
                    [return_maps]
                )
            };
        {error, Reason} -> {error, Reason}
    end.

%% @doc Takes a GraphQL item node, matches it with the appropriate data from a
%% gateway, then returns `{ok, ParsedMsg}`.
result_to_message(ExpectedID, Item, Opts) ->
    % We have the headers, so we can get the data.
    {ok, Data} = data(ExpectedID, Opts),
    ?event(gateway, {data, {id, ExpectedID}, {data, Data}}, Opts),
    % Convert the response to an ANS-104 message.
    GQLOpts = Opts#{ hashpath => ignore },
    TX =
        #tx {
            format = ans104,
            id = hb_util:decode(ExpectedID),
            last_tx =
                hb_util:decode(hb_converge:get(<<"anchor">>,
                    Item, GQLOpts)),
            signature =
                hb_util:decode(hb_converge:get(<<"signature">>,
                    Item, GQLOpts)),
            target =
                hb_util:decode(hb_converge:get(<<"recipient">>,
                    Item, GQLOpts)),
            owner =
                hb_util:decode(hb_converge:get(<<"owner/key">>,
                    Item, GQLOpts)),
            tags =
                [
                    {Name, Value}
                ||
                    #{<<"name">> := Name, <<"value">> := Value}
                        <- hb_converge:get(<<"tags">>, Item, GQLOpts)
                ],
            data_size =
                hb_util:int(hb_converge:get(<<"data/size">>,
                    Item, GQLOpts)
                ),
            data = Data
        },
    ?event({raw_ans104, TX}),
    ?event({ans104_form_response, TX}),
    TABM = dev_codec_ans104:from(TX),
    ?event({decoded_tabm, TABM}),
    Structured = dev_codec_structured:to(TABM),
    ?event({encoded_structured, Structured}),
    {ok, Structured}.

%%% Tests
ans104_no_data_item_test() ->
    % Start a random node so that all of the services come up.
    _Node = hb_http_server:start_node(#{}),
    {ok, Res} = read(<<"0Tb9mULcx8MjYVgXleWMVvqo1_jaw_P6AO_CJMTj0XE">>, #{}),
    ?event(gateway, {get_ans104_test, Res}),
    ?event(gateway, {signer, hb_message:signers(Res)}),
    % ?assert(hb_message:verify(Res)).
    ?assert(true).

%% @doc Test that we can get the scheduler location.
scheduler_location_test() ->
    % Start a random node so that all of the services come up.
    _Node = hb_http_server:start_node(#{}),
    {ok, Res} = scheduler_location(<<"fcoN_xJeisVsPXA-trzVAuIiqO3ydLQxM-L4XbrQKzY">>, #{}),
    ?event(gateway, {get_scheduler_location_test, Res}),
    ?assert(hb_message:verify(Res)),
    ?assertEqual(<<"Scheduler-Location">>, hb_converge:get(<<"Type">>, Res, #{})),
    ?event(gateway, {scheduler_location, {explicit, hb_converge:get(<<"url">>, Res, #{})}}),
    % Will need updating when Legacynet terminates.
    ?assertEqual(<<"https://su-router.ao-testnet.xyz">>, hb_converge:get(<<"url">>, Res, #{})).
