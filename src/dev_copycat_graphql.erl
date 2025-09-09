%%% @doc A `~copycat@1.0' engine that fetches data from a GraphQL endpoint for
%%% replication.
-module(dev_copycat_graphql).
-export([graphql/3]).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SUPPORTED_FILTERS,
    [
        <<"query">>, 
        <<"tag">>, 
        <<"tags">>,
        <<"owners">>, 
        <<"recipients">>, 
        <<"ids">>, 
        <<"all">>]
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
        ?event({graphql_request_returned_items, length(NodeStructs)}),
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
        ?event({graphql_parsed_msgs, length(ParsedMsgs)}),
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
        ?event(copycat_short,
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
    LoadedMerged = hb_cache:ensure_all_loaded(Merged, Opts),
    Keys = hb_maps:keys(LoadedMerged, Opts),
    SupportedKeys = ?SUPPORTED_FILTERS,
    MatchingKeys = 
        lists:filter(
            fun(K) -> lists:member(K, SupportedKeys) end, 
            Keys
        ),
    ?event(
        {finding_query,
            {supported, SupportedKeys}, 
            {merged_req, LoadedMerged}
        }
    ),
    case MatchingKeys of
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
            };
        [<<"query">>|_] ->
            % Handle query parameter - can be map or binary
            case hb_maps:find(<<"query">>, LoadedMerged, Opts) of
                {ok, QueryKeys} when is_map(QueryKeys) ->
                    build_combined_query(QueryKeys, Opts);
                {ok, Bin} when is_binary(Bin) ->
                    {ok, Bin};
                _ ->
                    case hb_maps:find(<<"body">>, LoadedMerged, Opts) of
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
            Key = hb_maps:get(<<"tag">>, LoadedMerged, <<>>, Opts),
            Value = hb_maps:get(<<"value">>, LoadedMerged, <<>>, Opts),
            TagsMap = case {Key, Value} of
                {<<>>, <<>>} -> #{};
                _ -> #{Key => Value}
            end,
            build_combined_query(#{<<"tags">> => TagsMap}, Opts);
        _ ->
            build_combined_query(LoadedMerged, Opts)
    end.

%% @doc Build GraphQL array from single value or list of values
build_graphql_array(Values) when is_list(Values) ->
    ValuesList = lists:map(fun hb_util:bin/1, Values),
    ValuesStr = hb_util:bin(lists:join(<<"\", \"">>, ValuesList)),
    <<"[\"", ValuesStr/binary, "\"]">>;
build_graphql_array(SingleValue) when is_binary(SingleValue) ->
    <<"[\"", SingleValue/binary, "\"]">>.

%% @doc Build combined GraphQL query supporting multiple filters
%% Handles: {"tags": {"type": "process"}, "owners": ["addr1"], "recipients": ["rec1"]}
build_combined_query(LoadedKeys, Opts) ->
    TagsPart = 
        build_tags_part(hb_maps:get(<<"tags">>, LoadedKeys, #{}, Opts)),
    OwnersPart = 
        build_filter_part(
            <<"owners">>, 
            hb_maps:get(<<"owners">>, LoadedKeys, [], Opts)
        ),
    RecipientsPart = 
        build_filter_part(
            <<"recipients">>, 
            hb_maps:get(<<"recipients">>, LoadedKeys, [], Opts)
        ),
    IdsPart = 
        build_filter_part(
            <<"ids">>, 
            hb_maps:get(<<"ids">>, LoadedKeys, [], Opts)
        ),
    %% Combine the filter criteria after preparing filters
    AllParts = TagsPart ++ OwnersPart ++ RecipientsPart ++ IdsPart,
    default_query(AllParts).

%% @doc Build tags part - special handling for map structure
build_tags_part(TagsMap) when map_size(TagsMap) =:= 0 -> [];
build_tags_part(TagsMap) when is_map(TagsMap) ->
    TagStrings = [
        <<"{name: \"", 
            (hb_util:bin(Key))/binary, 
            "\", values: ", 
            (build_graphql_array(Value))/binary, 
        "}">>
        || {Key, Value} <- maps:to_list(TagsMap)
    ],
    [<<"tags: [", (iolist_to_binary(lists:join(<<", ">>, TagStrings)))/binary, "]">>].

%% @doc Build filter part with empty check
build_filter_part(_FilterName, []) -> [];
build_filter_part(FilterName, Values) ->
    [<<FilterName/binary, ": ", (build_graphql_array(Values))/binary>>].

%% @doc Build final GraphQL query for empty vs non-empty
default_query([]) ->
    {ok, <<"query($after: String) { transactions(after: $after) { edges { ", 
            (hb_gateway_client:item_spec())/binary, 
        " } pageInfo { hasNextPage } } }">>};
default_query(Parts) ->
    CombinedFilters = iolist_to_binary(lists:join(<<", ">>, Parts)),
    {ok, <<"query($after: String) { transactions(after: $after, ", 
            CombinedFilters/binary, 
            ") { edges { ", (hb_gateway_client:item_spec())/binary, 
        " } pageInfo { hasNextPage } } }">>}.

%%% Tests
%% @doc Run node for testing
run_test_node() ->
    Store = hb_test_utils:test_store(hb_store_lmdb),
    Opts = #{ store => Store, priv_wallet => hb:wallet() },
    Node = hb_http_server:start_node(Opts),
    {Node ,Opts}. 
%% @doc Basic test to test copycat device
basic_test() ->
    {Node, _Opts} = run_test_node(),
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

query_test() ->
    Base = #{
        <<"query">> => #{
            <<"tags">> => #{
                <<"type">> => [<<"process">>,<<"assignment">>],
                <<"Data-Protocol">> => <<"ao">>
            },
            <<"owners">> => [<<"addr123">>],
            <<"recipients">> => [<<"rec1">>, <<"rec2">>],
            <<"ids">> => [<<"id1">>, <<"id2">>, <<"id3">>]
        }
    },
    Req = #{},
    Opts = #{},
    {ok, Query} = parse_query(Base, Req, Opts),
    ?event({query_test_result, {explicit, Query}}),
    ?assert(
        binary:matches(
            Query, 
            <<"{name: \"type\", values: [\"process\", \"assignment\"]}">>
        ) =/= []
    ),
    ?assert(
        binary:matches(
            Query, 
            <<"{name: \"Data-Protocol\", values: [\"ao\"]}">>
        ) =/= []
    ),
    ok.

%% @doc Test tag/value pair format
tag_value_test() ->
    Base = #{<<"tag">> => <<"type">>, <<"value">> => <<"process">>},
    {ok, Query} = parse_query(Base, #{}, #{}),
    ?event({tag_value_test, {query, Query}}),
    ?assert(
        binary:matches(
            Query,
            <<"{name: \"type\", values: [\"process\"]}">>
        ) =/= []
    ),
    ok.

%% @doc Test owners filter with single value
owners_filter_test() ->
    Base = #{<<"owners">> => <<"addr123">>},
    {ok, Query} = parse_query(Base, #{}, #{}),
    ?event({owners_filter_test, {query, Query}}),
    ?assert(
        binary:matches(
            Query,
            <<"owners: [\"addr123\"]">>
        ) =/= []
    ),
    ok.

%% @doc Test recipients filter with array values
recipients_filter_test() ->
    Base = #{<<"recipients">> => [<<"rec1">>, <<"rec2">>]},
    {ok, Query} = parse_query(Base, #{}, #{}),
    ?event({recipients_filter_test, {query, Query}}),
    ?assert(
        binary:matches(
            Query,
            <<"recipients: [\"rec1\", \"rec2\"]">>
        ) =/= []
    ),
    ok.

%% @doc Test ids filter
ids_filter_test() ->
    Base = #{<<"ids">> => [<<"id1">>, <<"id2">>, <<"id3">>]},
    {ok, Query} = parse_query(Base, #{}, #{}),
    ?event({ids_filter_test, {query, Query}}),
    ?assert(
        binary:matches(
            Query,
            <<"ids: [\"id1\", \"id2\", \"id3\"]">>
        ) =/= []
    ),
    ok.

%% @doc Test all filter type
all_filter_test() ->
    Base = #{<<"all">> => <<"true">>},
    {ok, Query} = parse_query(Base, #{}, #{}),
    ?event({all_filter_test, {query, Query}}),
    ?assert(
        binary:matches(
            Query,
            <<"transactions(after: $after)">>
        ) =/= []
    ),
    ok.

%% @doc Test combined multiple filters in one query
combined_filters_test() ->
    Base = #{
        <<"query">> => #{
            <<"tags">> => #{
                <<"type">> => [<<"process">>, <<"assignment">>],
                <<"Data-Protocol">> => <<"ao">>
            },
            <<"owners">> => <<"addr123">>,
            <<"recipients">> => [<<"rec1">>, <<"rec2">>],
            <<"ids">> => [<<"id1">>, <<"id2">>]
        }
    },
    {ok, Query} = parse_query(Base, #{}, #{}),
    ?event({combined_filters_test, {query, Query}}),
    % Should have tags
    ?assert(
        binary:matches(
            Query, 
            <<"{name: \"type\", values: [\"process\", \"assignment\"]}">>
        ) =/= []
    ),
    ?assert(
        binary:matches(
            Query, 
            <<"{name: \"Data-Protocol\", values: [\"ao\"]}">>
        ) =/= []
    ),
    % Should have owners
    ?assert(
        binary:matches(Query, <<"owners: [\"addr123\"]">>)
        =/= []
    ),
    % Should have recipients  
    ?assert(
        binary:matches(Query, <<"recipients: [\"rec1\", \"rec2\"]">>)
        =/= []
    ),
    % Should have ids
    ?assert(
        binary:matches(Query, <<"ids: [\"id1\", \"id2\"]">>)
        =/= []
    ),
    ok.

%% @doc Real world test with actual indexing
fetch_scheduler_location_test() ->
    {Node, _Opts} = run_test_node(),
    {ok, Res} =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"~copycat@1.0/graphql?tags+map=Type=Scheduler-Location">>
            },
            #{}
        ),
    ?event({graphql_indexing_completed, {response, Res}}),
    ?assert(is_tuple(Res)),
    {Status, Data} = Res,
    ?assertEqual(ok, Status),
    ?assert(is_integer(Data)),
    ?assert(Data > 0),
    ?event({schedulers_indexed, Data}),
    ok.
