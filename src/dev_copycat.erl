%%% @doc A device for orchestrating indexing of messages from foreign sources
%%% into a HyperBEAM node's caches.
%%% 
%%% Supported sources of messages are as follows:
%%% - A remote Arweave GraphQL endpoint.
%%% 
%%% This module is not production-ready. Do not use it in practice.
-module(dev_copycat).
-export([graphql/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(SUPPORTED_FILTERS,
    [
        <<"query">>, 
        <<"tag">>, 
        <<"owners">>, 
        <<"recipients">>, 
        <<"ids">>, 
        <<"all">>
    ]
).

%% @doc Takes a GraphQL query, optionally with a node address, and curses through
%% each of the messages returned by the query, indexing them into the node's
%% caches.
graphql(Base, Req, Opts) ->
    case parse_query(Base, Req, Opts) of
        {ok, Query} ->
            Node = maps:get(<<"node">>, Opts, undefined),
            OpName = hb_maps:get(<<"operationName">>, Req, undefined, Opts),
            Vars = hb_maps:get(<<"variables">>, Req, #{}, Opts),
            index_graphql(0, Query, Vars, Node, OpName, Opts);
        Other ->
            Other
    end.

%% @doc Index a GraphQL query into the node's caches.
index_graphql(Total, Query, Vars, Node, OpName, Opts) ->
    maybe
        ?event(
            {graphql_run_called,
                {query, {string, Query}},
                {operation, OpName},
                {variables, Vars}
            }
        ),
        {ok, RawRes} ?= hb_gateway_client:query(Query, Vars, Node, OpName, Opts),
        Res = hb_util:deep_get(<<"data/transactions">>, RawRes, #{}, Opts),
        NodeStructs = hb_util:deep_get(<<"edges">>, Res, [], Opts),
        ?event(indexer_short, {graphql_request_returned_items, length(NodeStructs)}),
        ?event(
            {graphql_indexing_responses,
                {query, {string, Query}},
                {variables, Vars},
                {result, Res}
            }
        ),
        ParsedMsgs =
            lists:filtermap(
                fun(NodeStruct) ->
                    Struct = hb_maps:get(<<"node">>, NodeStruct, not_found, Opts),
                    try
                        {ok, ParsedMsg} =
                            hb_gateway_client:result_to_message(
                                Struct,
                                Opts
                            ),
                        {true, ParsedMsg}
                    catch
                        error:Reason ->
                            ?event(
                                warning,
                                {indexer_graphql_parse_failed,
                                    {struct, NodeStruct},
                                    {reason, Reason}
                                }
                            ),
                            false
                    end
                end,
                NodeStructs
            ),
        ?event(indexer_short, {graphql_parsed_msgs, length(ParsedMsgs)}),
        WrittenMsgs =
            lists:filter(
                fun(ParsedMsg) ->
                    try
                        {ok, _} = hb_cache:write(ParsedMsg, Opts),
                        true
                    catch
                        error:Reason ->
                            ?event(
                                warning,
                                {indexer_graphql_write_failed,
                                    {reason, Reason},
                                    {msg, ParsedMsg}
                                }
                            ),
                            false
                    end
                end,
                ParsedMsgs
            ),
        NewTotal = Total + length(WrittenMsgs),
        ?event(indexer_short,
            {indexer_graphql_wrote,
                {total, NewTotal},
                {batch, length(WrittenMsgs)},
                {batch_failures, length(ParsedMsgs) - length(WrittenMsgs)}
            }
        ),
        HasNextPage = hb_util:deep_get(<<"pageInfo/hasNextPage">>, Res, false, Opts),
        case HasNextPage of
            true ->
                % Get the last cursor from the node structures and recurse.
                {ok, Cursor} =
                    hb_maps:find(
                        <<"cursor">>,
                        lists:last(NodeStructs),
                        Opts
                    ),
                index_graphql(
                    NewTotal,
                    Query,
                    Vars#{ <<"after">> => Cursor },
                    Node,
                    OpName,
                    Opts
                );
            false ->
                {ok, NewTotal}
        end
    else
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Find or create a GraphQL query from a given base and request. We expect
%% to find either a `query' field, a `tags' field, a `tag' and `value' field,
%% an `owner' field, or a `recipient' field. If none of these fields are found,
%% we return a query that will match all results known to an Arweave gateway.
parse_query(Base, Req, Opts) ->
    % Merge the keys of the base and request maps, and remove duplicates.
    Merged = hb_maps:merge(Base, Req, Opts),
    Keys = hb_maps:keys(Merged, Opts),
    SupportedKeys = ?SUPPORTED_FILTERS,
    ?event({finding_query, {supported, SupportedKeys}, {merged_req, Merged}}),
    case lists:filter(fun(K) -> lists:member(K, SupportedKeys) end, Keys) of
        [<<"query">>|_] ->
            % Find the query in either the `query' field or the `body'.
            case hb_maps:find(<<"query">>, Merged, Opts) of
                {ok, Bin} when is_binary(Bin) ->
                    {ok, Bin};
                _ ->
                    case hb_maps:find(<<"body">>, Merged, Opts) of
                        {ok, Bin} when is_binary(Bin) ->
                            {ok, Bin};
                        _ ->
                            {error,
                                #{
                                    <<"body">> => 
                                        <<"No query found in the request.">>
                                }
                            }
                    end
            end;
        [<<"tag">>|_] ->
            TagPairs = extract_tag_pairs(Base, Req, Opts),
            default_query(<<"tags">>, TagPairs, Opts);
        [FilterKey|_] ->
            % Handle array-type parameters (owners, recipients, ids)
            case FilterKey of
                K when K =:= <<"owners">>; K =:= <<"recipients">>; K =:= <<"ids">> ->
                    Values = extract_array_values(K, Merged, Opts),
                    default_query(K, Values, Opts);
                _ ->
                    default_query(FilterKey, Merged, Opts)
            end;
        [] ->
            {error,
                #{
                    <<"body">> =>
                        <<"No supported filter fields found. Supported filters: ",
                            (
                                lists:join(
                                    <<", ">>,
                                    lists:map(
                                        fun(K) -> <<"\"", (K)/binary, "\"">> end,
                                        SupportedKeys
                                    )
                                )
                            )/binary
                        >>
                }
            }
    end.

%% @doc Return a default query for a given filter type.
default_query(<<"tags">>, TagPairs, _Opts) ->
    TagsQuery = build_tags_query(TagPairs),
    {ok, <<"query($after: String) { ",
        "transactions(after: $after, tags: [", TagsQuery/binary, "]) { ",
        "edges { ", (hb_gateway_client:item_spec())/binary , " } ",
        "pageInfo { hasNextPage }",
    "} }">>};
default_query(<<"owners">>, Values, _Opts) when is_list(Values) ->
    ValuesArray = build_string_array(Values),
    {ok, <<"query($after: String) { ",
        "transactions(after: $after, owners: [", ValuesArray/binary, "]) { ",
        "edges { ", (hb_gateway_client:item_spec())/binary , " } ",
        "pageInfo { hasNextPage }",
    "} }">>};
default_query(<<"recipients">>, Values, _Opts) when is_list(Values) ->
    ValuesArray = build_string_array(Values),
    {ok, <<"query($after: String) { ",
        "transactions(after: $after, recipients: [", ValuesArray/binary, "]) { ",
        "edges { ", (hb_gateway_client:item_spec())/binary , " } ",
        "pageInfo { hasNextPage }",
    "} }">>};
default_query(<<"ids">>, Values, _Opts) when is_list(Values) ->
    ValuesArray = build_string_array(Values),
    {ok, <<"query($after: String) { ",
        "transactions(ids: [", ValuesArray/binary, "]) { ",
        "edges { ", (hb_gateway_client:item_spec())/binary , " } ",
        "pageInfo { hasNextPage }",
    "} }">>};
default_query(<<"all">>, _Merged, _Opts) ->
    {ok, <<"query($after: String) { ",
        "transactions(after: $after) { ",
        "edges { ", (hb_gateway_client:item_spec())/binary , " } ",
        "pageInfo { hasNextPage }",
    "} }">>}.

%% @doc Extract tag pairs from request parameters, handling multiple 
%% tag/value pairs. Supports both single pairs (tag=X&value=Y) and 
%% array syntax (tag=[X,Z]&value=[Y,W])
extract_tag_pairs(Base, Req, Opts) ->
    Merged = hb_maps:merge(Base, Req, Opts),
    TagResult = hb_maps:find(<<"tag">>, Merged, Opts),
    ValueResult = hb_maps:find(<<"value">>, Merged, Opts),
    case {TagResult, ValueResult} of
        {{ok, TagParam}, {ok, ValueParam}} 
        when is_binary(TagParam), is_binary(ValueParam) ->
            extract_from_parameters(TagParam, ValueParam);
        _ ->
            extract_with_defaults(Merged, Opts)
    end.

%% @doc Extract tag pairs from found tag and value parameters
extract_from_parameters(TagParam, ValueParam) ->
    IsTagArray = is_array_syntax(TagParam),
    IsValueArray = is_array_syntax(ValueParam),
    case {IsTagArray, IsValueArray} of
        {true, true} ->
            % Both are arrays: [tag1,tag2] & [val1,val2]
            Tags = parse_array_parameter(TagParam),
            Values = parse_array_parameter(ValueParam),
            TagValuePairs = pair_tags_values(Tags, Values),
            group_tag_pairs(TagValuePairs);
        {false, false} ->
            % Single tag/value pair: tag=X & value=Y
            [{TagParam, ValueParam}];
        _ ->
            % Mismatched formats, treat as single pair
            [{TagParam, ValueParam}]
    end.

%% @doc Extract tag pairs with default values when parameters not found
extract_with_defaults(Merged, Opts) ->
    Key = hb_maps:get(<<"tag">>, Merged, <<"">>, Opts),
    Value = hb_maps:get(<<"value">>, Merged, <<"">>, Opts),
    [{Key, Value}].

%% @doc Pair up tags and values, handling mismatched counts
pair_tags_values([], []) -> [];
pair_tags_values([Tag|Tags], [Value|Values]) -> 
    [{Tag, Value} | pair_tags_values(Tags, Values)];
pair_tags_values([Tag|Tags], []) -> 
    [{Tag, <<"">>} | pair_tags_values(Tags, [])];
pair_tags_values([], [Value|Values]) -> 
    [{<<"">>, Value} | pair_tags_values([], Values)].

%% @doc Group tag pairs by tag name, combining values for duplicate tags
%% Example: [{<<"type">>, <<"process">>}, {<<"type">>, <<"message">>}] 
%% -> [{<<"type">>, [<<"process">>, <<"message">>]}]
group_tag_pairs(TagValuePairs) ->
    % Group by tag name
    GroupedMap = lists:foldl(
        fun({Tag, Value}, Acc) ->
            maps:update_with(
                Tag, 
                fun(Existing) -> 
                    [Value | Existing] 
                end, 
                [Value], 
                Acc
            )
        end,
        #{},
        TagValuePairs
    ),
    % Convert back to list, reversing values to maintain order
    [{Tag, lists:reverse(Values)} || {Tag, Values} <- maps:to_list(GroupedMap)].

%% @doc Check if a parameter uses array syntax [item1,item2,...]
is_array_syntax(<<$[, _/binary>> = Param) ->
    byte_size(Param) > 2 andalso binary:last(Param) =:= $];
is_array_syntax(_) ->
    false.

%% @doc Parse array parameter "[item1,item2,item3]" into 
%% ["item1", "item2", "item3"]
parse_array_parameter(<<$[, Rest/binary>>) ->
    % Remove the closing bracket
    RestSize = byte_size(Rest),
    ContentWithoutBracket = binary:part(Rest, 0, RestSize - 1),
    % Split by comma and trim whitespace
    Items = binary:split(ContentWithoutBracket, <<",">>, [global]),
    [string:trim(Item) || Item <- Items].

%% @doc Build GraphQL tags query from tag pairs list
%% Handles both single values and lists of values per tag
%% Example: [{<<"type">>, <<"process">>}] -> 
%% <<"{"name": "type", "values": ["process"]}">>
build_tags_query(TagPairs) ->
    TagStrings = lists:map(fun build_single_tag_query/1, TagPairs),
    iolist_to_binary(lists:join(<<", ">>, TagStrings)).

%% @doc Build GraphQL query fragment for a single tag
build_single_tag_query({Key, Value}) when is_binary(Value) ->
    % Single value: {name: "tag", values: ["value"]}
    build_tag_with_values(Key, [Value]);
build_single_tag_query({Key, Values}) when is_list(Values) ->
    % Multiple values: {name: "tag", values: ["val1", "val2"]}
    build_tag_with_values(Key, Values).

%% @doc Build GraphQL tag object with quoted values array
build_tag_with_values(Key, Values) ->
    QuotedValues = [<<"\"", V/binary, "\"">> || V <- Values],
    ValuesArray = iolist_to_binary(lists:join(<<", ">>, QuotedValues)),
    <<"{name: \"", Key/binary, "\", values: [", ValuesArray/binary, "]}">>.

%% @doc Extract array values from parameters, supporting multiple input formats
%% Handles: "value", "val1,val2", "[val1,val2]"
extract_array_values(ParamKey, Merged, Opts) ->
    case hb_maps:get(ParamKey, Merged, <<>>, Opts) of
        <<>> -> [];
        Param when is_binary(Param) ->
            case is_array_syntax(Param) of
                true -> parse_array_parameter(Param);
                false -> parse_comma_separated(Param)
            end;
        _ -> []
    end.

%% @doc Parse comma-separated values "val1,val2,val3" into ["val1", "val2", "val3"]
parse_comma_separated(<<>>) -> [];
parse_comma_separated(Param) ->
    Items = binary:split(Param, <<",">>, [global]),
    [string:trim(Item) || Item <- Items].

%% @doc Build string array for GraphQL: ["val1", "val2"] -> "\"val1\", \"val2\""
build_string_array(Values) ->
    QuotedValues = [<<"\"", V/binary, "\"">> || V <- Values, V =/= <<>>],
    iolist_to_binary(lists:join(<<", ">>, QuotedValues)).

%%% Tests

%% @doc Basic test to test copycat device
basic_test() ->
    Store = hb_test_utils:test_store(hb_store_lmdb),
    Opts = #{ store => Store, priv_wallet => hb:wallet() },
    Node = hb_http_server:start_node(Opts),
    {ok, Res} =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"~copycat@1.0/graphql?tag=type&value=process">>
            },
            #{}
        ),
    ?event({basic_test_result, Res}),
    ok.

%% @doc Test multiple tag query functionality
multiple_tags_test() ->
    Store = hb_test_utils:test_store(hb_store_lmdb),
    Opts = #{ store => Store, priv_wallet => hb:wallet() },
    Node = hb_http_server:start_node(Opts),
    
    % Test query construction with multiple tag pairs
    TagPairs = [{<<"type">>, <<"process">>}, {<<"Data-Protocol">>, <<"ao">>}],
    QueryResult = build_tags_query(TagPairs),
    ?event({multiple_tags_query_result, QueryResult}),
    {ok, Res} =
        hb_http:get(
            Node,
            #{
                <<"path">> => 
                    <<"~copycat@1.0/graphql?tag=[type,type,Data-Protocol]"
                      "&value=[process,message,ao]">>
            },
            #{}
        ),
    ?event({multiple_tags_test_result, Res}),
    ok.

%% @doc Test owners query with comma-separated values
owners_query_test() ->
    Store = hb_test_utils:test_store(hb_store_lmdb),
    Opts = #{ store => Store, priv_wallet => hb:wallet() },
    Node = hb_http_server:start_node(Opts),
    
    {ok, Res} =
        hb_http:get(
            Node,
            #{
                <<"path">> => 
                    <<"~copycat@1.0/graphql?"
                    "owners=pxfKw58POM24dwBLmiah2K81UX-sM2CXOAm6AI13SvY"
                    ",VXj58O78wcrorcWV1Y5zT9vTmukV3_Xmb36iaJsztK0">>
            },
            #{}
        ),
    ?event({owners_test_result, Res}),
    ok.
