%%% @doc A `~copycat@1.0' engine that fetches block data (and optionally
%%% transactions + data) from an Arweave node for replication.
%%% 
%%% Defaults remain the same as before: reverse chronological indexing of block
%%% headers only. New options:
%%%   - `direction' (`asc' | `desc'): traverse blocks forward (chronological)
%%%     or backward (default).
%%%   - `include-txs' (boolean): fetch and cache all tx headers in each block.
%%%   - `include-data' (boolean): fetch tx data bodies as well (secondary; may
%%%     be heavy).
%%% 
%%% Hooks:
%%%   - `copycat/block' runs after a block is processed.
%%%   - `copycat/tx' runs after a tx is processed (header/data already cached).
%%% Handlers receive the processed item in `Req/body' plus metadata such as
%%% block height and tx id.
-module(dev_copycat_arweave).
-export([arweave/3]).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ARWEAVE_DEVICE, <<"~arweave@2.9-pre">>).

%% @doc Fetch blocks from an Arweave node between a given range.
arweave(_Base, Request, Opts) ->
    {From, To, Direction} = parse_range(Request, Opts),
    FetchOpts =
        #{
            include_txs =>
                parse_flag(
                    [<<"include-txs">>, <<"txs">>],
                    Request,
                    Opts,
                    copycat_include_txs
                ),
            include_data =>
                parse_flag(
                    [<<"include-data">>, <<"data">>],
                    Request,
                    Opts,
                    copycat_include_data
                ),
            direction => Direction
        },
    fetch_blocks(Request, From, To, FetchOpts, Opts).

%% @doc Parse the range from the request.
parse_range(Request, Opts) ->
    Direction = parse_direction(Request, Opts),
    LatestHeight =
        case hb_opts:get(copycat_current_height, undefined, Opts) of
            undefined ->
                case
                    hb_ao:resolve(
                        <<?ARWEAVE_DEVICE/binary, "/current/height">>,
                        Opts
                    )
                of
                    {ok, Height0} -> hb_util:int(Height0);
                    _ -> 0
                end;
            HeightOverride -> hb_util:int(HeightOverride)
        end,
    {From, To} =
        case Direction of
            asc ->
                {
                    hb_util:int(
                        hb_maps:get(
                            <<"from">>,
                            Request,
                            0,
                            Opts
                        )
                    ),
                    hb_util:int(
                        hb_maps:get(
                            <<"to">>,
                            Request,
                            LatestHeight,
                            Opts
                        )
                    )
                };
            desc ->
                {
                    case hb_maps:find(<<"from">>, Request, Opts) of
                        {ok, Height} -> hb_util:int(Height);
                        error -> LatestHeight
                    end,
                    hb_util:int(hb_maps:get(<<"to">>, Request, 0, Opts))
                }
        end,
    {From, To, Direction}.

%% @doc Fetch blocks from an Arweave node between a given range.
fetch_blocks(_Req, Current, Target, #{ direction := desc }, _Opts)
        when Current < Target ->
    {ok, Current};
fetch_blocks(Req, Current, To, FetchOpts = #{ direction := desc }, Opts) ->
    BlockRes =
        hb_ao:resolve(
            <<
                ?ARWEAVE_DEVICE/binary,
                "/block=",
                (hb_util:bin(Current))/binary
            >>,
            Opts
        ),
    process_block(BlockRes, Req, Current, To, FetchOpts, Opts),
    fetch_blocks(Req, Current - 1, To, FetchOpts, Opts);
fetch_blocks(_Req, Current, To, #{ direction := asc }, _Opts)
        when Current > To ->
    {ok, done};
fetch_blocks(Req, Current, To, FetchOpts = #{ direction := asc }, Opts)
        when Current =< To ->
    BlockRes =
        hb_ao:resolve(
            <<
                ?ARWEAVE_DEVICE/binary,
                "/block=",
                (hb_util:bin(Current))/binary
            >>,
            Opts
        ),
    process_block(BlockRes, Req, Current, To, FetchOpts, Opts),
    fetch_blocks(Req, Current + 1, To, FetchOpts, Opts).

%% @doc Process a block.
process_block(BlockRes, _Req, Current, To, FetchOpts, Opts) ->
    case BlockRes of
        {ok, _} ->
            ?event(
                copycat_short,
                {arweave_block_cached,
                    {height, Current},
                    {target, To}
                }
            ),
            maybe_hook_block(Current, BlockRes, Opts),
            maybe_fetch_block_txs(Current, BlockRes, FetchOpts, Opts);
        {error, not_found} ->
            ?event(
                copycat_short,
                {arweave_block_not_found,
                    {height, Current},
                    {target, To}
                }
            )
    end.

%% @doc Fetch all txs for a block if requested.
maybe_fetch_block_txs(_Height, _BlockRes, #{ include_txs := false }, _Opts) ->
    ok;
maybe_fetch_block_txs(Height, {ok, Block}, FetchOpts, Opts) ->
    Loaded = hb_cache:ensure_all_loaded(Block, Opts),
    TXIDs = hb_maps:get(<<"txs">>, Loaded, [], Opts),
    lists:foreach(
        fun(TXID) ->
            fetch_tx(TXID, Height, FetchOpts, Opts)
        end,
        TXIDs
    ).

%% @doc Retrieve, cache, and hook a tx.
fetch_tx(TXID, Height, FetchOpts, Opts) ->
    FetchFun =
        hb_opts:get(
            copycat_fetch_tx,
            fun default_fetch_tx/3,
            Opts
        ),
    case FetchFun(TXID, FetchOpts, Opts) of
        {ok, #{ tx_msg := TxMsg, tx_record := TxRec }} ->
            _ = hb_cache:write(TxMsg, Opts),
            maybe_hook_tx(TxMsg, TxRec, Height, Opts),
            ok;
        {ok, TxMsg} ->
            _ = hb_cache:write(TxMsg, Opts),
            maybe_hook_tx(TxMsg, undefined, Height, Opts),
            ok;
        Other ->
            ?event(
                warning,
                {failed_to_fetch_tx,
                    {tx, TXID},
                    {height, Height},
                    {reason, Other}
                }
            )
    end.

%% @doc Default tx fetcher: header-only unless include_data is true.
default_fetch_tx(TXID, #{ include_data := IncludeData }, Opts) ->
    TxPath = <<"/arweave/tx/", TXID/binary>>,
    case hb_http:request(#{ <<"path">> => TxPath, <<"method">> => <<"GET">>}, Opts) of
        {ok, #{ <<"body">> := Body }} ->
            TXHeader = ar_tx:json_struct_to_tx(hb_json:decode(Body)),
            FullTX =
                case IncludeData of
                    true -> maybe_add_data(TXHeader, TXID, Opts);
                    false -> TXHeader
                end,
            TxMsg =
                hb_message:convert(
                    FullTX,
                    <<"structured@1.0">>,
                    <<"tx@1.0">>,
                    Opts
                ),
            {ok, #{ tx_msg => TxMsg, tx_record => FullTX }};
        Error -> Error
    end.

maybe_add_data(TX, TXID, Opts) ->
    case hb_http:request(
        #{
            <<"path">> => <<"/arweave/raw/", TXID/binary>>,
            <<"method">> => <<"GET">>
        },
        Opts
    ) of
        {ok, #{ <<"body">> := Data }} ->
            TX#tx{
                data = Data,
                data_size = byte_size(Data),
                data_root = ar_tx:data_root(Data)
            };
        _ ->
            TX
    end.

%% @doc Emit the copycat/block hook.
maybe_hook_block(Height, {ok, Block}, Opts) ->
    _ = dev_hook:on(
        <<"copycat/block">>,
        #{ <<"body">> => Block, <<"height">> => Height },
        Opts
    ),
    ok.

%% @doc Emit the copycat/tx hook.
maybe_hook_tx(TxMsg, TxRec, Height, Opts) ->
    HookReq =
        #{
            <<"body">> => TxMsg,
            <<"tx-record">> => TxRec,
            <<"block-height">> => Height
        },
    _ = dev_hook:on(<<"copycat/tx">>, HookReq, Opts),
    ok.

%% @doc Parse a boolean-style flag from the request or opts.
parse_flag(Keys, Req, Opts, OptKey) ->
    Bool =
        lists:foldl(
            fun(Key, Acc) ->
                case Acc of
                    undefined ->
                        case hb_maps:find(Key, Req, Opts) of
                            {ok, Val} -> Val;
                            error -> undefined
                        end;
                    _ -> Acc
                end
            end,
            undefined,
            Keys
        ),
    case Bool of
        undefined ->
            hb_util:atom(hb_opts:get(OptKey, false, Opts));
        _ -> hb_util:atom(Bool)
    end.

parse_direction(Req, Opts) ->
    Raw = hb_maps:get(<<"direction">>, Req, <<"desc">>, Opts),
    case hb_util:to_lower(hb_util:bin(Raw)) of
        <<"asc">> -> asc;
        <<"forward">> -> asc;
        <<"chronological">> -> asc;
        _ -> desc
    end.

%%% Tests

copycat_tx_hook_integration_test() ->
    ProcID = hb_util:encode(crypto:strong_rand_bytes(32)),
    TxA = hb_util:encode(crypto:strong_rand_bytes(32)),
    TxB = hb_util:encode(crypto:strong_rand_bytes(32)),
    Store = hb_test_utils:test_store(),
    hb_store:start(Store),
    Block =
        #{
            <<"height">> => 1,
            <<"indep_hash">> => hb_util:encode(crypto:strong_rand_bytes(32)),
            <<"hash">> => hb_util:encode(crypto:strong_rand_bytes(32)),
            <<"txs">> => [TxA, TxB]
        },
    BaseOpts = #{
        store => [Store],
        scheduler_store => [Store],
        priv_wallet => hb:wallet()
    },
    {ok, _} = dev_arweave_block_cache:write(Block, BaseOpts),
    FetchFun =
        fun(TxId, _FetchOpts, _Opts) ->
            {ok,
                #{
                    <<"data-protocol">> => <<"arweave-scheduler">>,
                    <<"variant">> => <<"1.0">>,
                    <<"process">> => ProcID,
                    <<"type">> => <<"Message">>,
                    <<"id">> => TxId
                }
            }
        end,
    Opts =
        BaseOpts#{
            on =>
                #{
                    <<"copycat/tx">> =>
                        #{
                            <<"device">> => <<"arweave-scheduler@1.0">>,
                            <<"path">> => <<"on_tx">>
                        }
            },
            copycat_fetch_tx => FetchFun,
            copycat_current_height => 1
        },
    {ok, _} =
        arweave(
            #{},
            #{
                <<"from">> => 1,
                <<"to">> => 1,
                <<"include-txs">> => true,
                <<"direction">> => <<"asc">>
            },
            Opts
        ),
    Slots = dev_scheduler_cache:list(ProcID, Opts),
    ?assertEqual([0, 1], lists:sort(Slots)).
