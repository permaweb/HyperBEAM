-module(hb_arweave_fallback).
-export([read/2]).

read(ID, Opts) ->
    {Query, Variables} = case maps:is_key(<<"subindex">>, Opts) of
      true ->
        Tags = hb_gateway_client:subindex_to_tags(maps:get(<<"subindex">>, Opts)),
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
    case  hb_gateway_client:query(Query, Variables, Opts) of
        {error, Reason} ->
            {error, Reason};
        {ok, GqlMsg} ->
            case hb_ao:get(<<"data/transactions/edges/1/node/block/height">>, GqlMsg, Opts) of
                not_found ->
                    {error, not_found};
                Height ->
                    {ok, Height}
            end
    end.

item_spec() ->
    <<"""
        node {
            block { height }
        }
    """>>.

