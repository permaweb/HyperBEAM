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
process_block(BlockRes, _Req, Current, To, _Opts) ->
    case BlockRes of
        {ok, _} ->
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