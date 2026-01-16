%%% @doc A `~copycat@1.0' engine that fetches block data from an Arweave node for
%%% replication. This engine works in _reverse_ chronological order by default,
%%% fetching blocks from the latest known block towards the Genesis block. The
%%% node avoids retrieving blocks that are already present in the cache using
%%% `~arweave@2.9-pre''s built-in caching mechanism.
-module(dev_copycat_arweave).
-export([arweave/3]).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ARWEAVE_DEVICE, <<"~arweave@2.9-pre">>).
-define(ARWEAVE_INDEX_PATH, <<?ARWEAVE_DEVICE/binary, "/offset">>).

% GET /~cron@1.0/once&cron-path=~copycat@1.0/arweave

%% @doc Fetch blocks from an Arweave node between a given range, or from the
%% latest known block towards the Genesis block. If no range is provided, we
%% fetch blocks from the latest known block towards the Genesis block.
arweave(_Base, Request, Opts) ->
    {From, To} = parse_range(Request, Opts),
    fetch_blocks(Request, From, To, Opts).

%% @doc Parse the range from the request.
parse_range(Request, Opts) ->
    From =
        case hb_maps:find(<<"from">>, Request, Opts) of
            {ok, Height} -> Height;
            error ->
                {ok, LatestHeight} =
                    hb_ao:resolve(
                        <<?ARWEAVE_DEVICE/binary, "/current/height">>,
                        Opts
                    ),
                LatestHeight
        end,
    To = hb_maps:get(<<"to">>, Request, 0, Opts),
    {From, To}.

%% @doc Fetch blocks from an Arweave node between a given range.
fetch_blocks(Req, Current, Current, _Opts) ->
    ?event(copycat_arweave,
        {arweave_block_indexing_completed,
            {reached_target, Current},
            {initial_request, Req}
        }
    ),
    {ok, Current};
fetch_blocks(Req, Current, To, Opts) ->
    BlockRes =
        hb_ao:resolve(
            <<
                ?ARWEAVE_DEVICE/binary,
                "/block=",
                (hb_util:bin(Current))/binary
            >>,
            Opts
        ),
    process_block(BlockRes, Req, Current, To, Opts),
    fetch_blocks(Req, Current - 1, To, Opts).

%% @doc Process a block.
process_block(BlockRes, _Req, Current, To, Opts) ->
    case BlockRes of
        {ok, Block} ->
            % maybe_index_ids(Block, Opts),
            ?event(
                copycat_short,
                {arweave_block_cached,
                    {height, Current},
                    {target, To}
                }
            );
        {error, not_found} ->
            ?event(
                copycat_short,
                {arweave_block_not_found,
                    {height, Current},
                    {target, To}
                }
            )
    end.

%% @doc Index the IDs of all transactions in the block if configured to do so.
% maybe_index_ids(Block, Opts) ->
%     case hb_opts:get(arweave_index_ids, false, Opts) of
%         false -> ok;
%         true ->
%             IndexStore = hb_opts:get(arweave_index_store, no_store, Opts),
%             BlockOffset = hb_maps:get(<<"weave_size">>, Block, 0, Opts),
%             lists:foreach(
%                 fun(TXID) ->
%                     TX =
%                         hb_ao:get(
%                             <<
%                                 ?ARWEAVE_DEVICE/binary,
%                                 "/tx=",
%                                 (hb_util:bin(TXID))/binary
%                             >>,
%                             Opts
%                         ),
%                     TXOffset = hb_maps:get(<<"offset">>, TX, 0, Opts),
%                     case is_bundle_tx(TX, Opts) of
%                         false -> ok;
%                         true ->
%                             {ok, BundleIndex} = download_bundle_header(TXID, Opts),
%                             hb_maps:map(
%                                 fun(ItemID, #{ <<"offset">> := BundleOffset, <<"length">> := Length}) ->
%                                     Offset = hb_util:bin(BundleOffset + TXOffset + BlockOffset),
%                                     hb_store_arweave:write_offset(
%                                         IndexStore,
%                                         ItemID,
%                                         Offset,
%                                         Length
%                                     )
%                                 end,
%                                 BundleIndex,
%                                 Opts
%                             )
%                     end
%                 end,
%                 hb_maps:get(<<"txs">>, Block, #{}, Opts),
%                 Opts
%             )
%     end.
%     ok.


%%% Tests

