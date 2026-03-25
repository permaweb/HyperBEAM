%%% @doc A store implementation that relays to an Arweave node, using an 
%%% intermediate cache of offsets as an ID->ArweaveLocation mapping.
-module(hb_store_arweave).
%%% Store API:
-export([scope/0, scope/1, type/2, read/2, start/1]).
%%% Unused Store API:
-export([resolve/2, write/3, make_link/3, make_group/2]).
%%% Indexing API:
-export([store_from_opts/1, write_offset/5, read_offset/2, read_chunks/3]).
%%% Lazy loading API:
-export([materialize/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PARTITION_SIZE, 3_600_000_000_000).

%% @doc Find the first Arweave store from the given node message. Searches first
%% for the `arweave_index_store' option, and if not found, searches the main
%% `store' list for the first Arweave store with an index.
store_from_opts(Opts) ->
    case hb_opts:get(arweave_index_store, no_store, Opts) of
        no_store -> first_arweave_store(hb_opts:get(store, [], Opts));
        IndexStoreOpts -> IndexStoreOpts
    end.

%% @doc Find the first Arweave store with an index from a list of stores.
first_arweave_store(NonList) when not is_list(NonList) ->
    first_arweave_store([NonList]);
first_arweave_store([]) -> no_store;
first_arweave_store(
    [Store = #{<<"store-module">> := ?MODULE, <<"index-store">> := _ } | _]
) -> Store;
first_arweave_store([_ | Rest]) -> first_arweave_store(Rest).

%% @doc Start the Arweave store, and the downstream associated index store.
start(#{<<"index-store">> := IndexStore}) ->
    init_prometheus(),
    hb_store:start(IndexStore).

%% @doc Although the index is local, loading an item via the index will make
%% requests to a remote node, so we define the scope as remote.
scope() -> remote.
scope(#{ <<"scope">> := Scope }) -> Scope;
scope(_) -> scope().

%% @doc Resolve a key path in the Arweave store, ignoring other paths.
resolve(_, ID) when ?IS_ID(ID) -> ID;
resolve(_, _) -> not_found.

%% @doc Unsupported.
write(_, _, _) -> not_found.

%% @doc Unsupported.
make_link(_, _, _) -> not_found.

%% @doc Unsupported.
make_group(_, _) -> not_found.

%% @doc Get the type of the data at the given key. We potentially cache the
%% result, so that we don't have to read the data from the GraphQL route
%% multiple times.
type(#{ <<"index-store">> := IndexStore }, ID) when ?IS_ID(ID) ->
    case hb_store:read(IndexStore, hb_store_arweave_offset:path(ID)) of
        {ok, _Offset} -> simple;
        _ -> not_found
    end;
type(_, _) -> not_found.

%% @doc Read the offset of the data at the given key.
read_offset(#{ <<"index-store">> := IndexStore }, ID) ->
    ReadRes =
        hb_prometheus:measure_and_report(
            fun() ->
                hb_store:read(IndexStore, hb_store_arweave_offset:path(ID))
            end,
            hb_store_arweave_index_check_duration_seconds
        ),
    case ReadRes of
        {ok, OffsetBinary} ->
            {Version, CodecName, StartOffset, Length} =
                hb_store_arweave_offset:decode(OffsetBinary),
            {ok, #{
                <<"version">> => Version,
                <<"codec-device">> => CodecName,
                <<"start-offset">> => StartOffset,
                <<"length">> => Length
            }};
        _ ->
            not_found
    end;
read_offset(_, _) -> not_found.

%% @doc Read the data at the given key. Returns a lazy
%% message with data-ref for Arweave-stored items. Call
%% materialize/2 when full data is needed.
read(StoreOpts, ID) when ?IS_ID(ID) ->
    case hb_store_remote_node:read_local_cache(StoreOpts, ID) of
        {ok, Message} -> {ok, Message};
        not_found -> do_read(StoreOpts, ID)
    end;
read(_, _) -> not_found.

%% @doc Read the data at the given key, reading the provided Arweave index store
%% as a source of offsets. After offsets have been found, the data is loaded
%% through the `~arweave@2.9` device -- either as an ANS-104 item or a TX.
do_read(StoreOpts, ID) ->
    case read_offset(StoreOpts, ID) of
        {ok,
            #{
                <<"version">> := Version,
                <<"codec-device">> := CodecName,
                <<"start-offset">> := StartOffset,
                <<"length">> := Length
            }} ->
            Loaded =
                case CodecName of
                    <<"ans104@1.0">> ->
                        load_item(StartOffset, Length, StoreOpts);
                    <<"tx@1.0">> ->
                        load_tx(ID, StartOffset, Length, StoreOpts)
                end,
            case Loaded of
                {ok, Message} ->
                    case maps:is_key(<<"data-ref">>, Message) of
                        true -> ok;
                        false ->
                            hb_store_remote_node:maybe_cache(
                                StoreOpts, Message)
                    end,
                    ?event(
                        arweave_offsets,
                        {read_ok,
                            {id, {explicit, ID}},
                            {format_version, Version},
                            {type, CodecName},
                            {start_offset, StartOffset},
                            {length, Length}
                        }
                    ),
                    record_partition_metric(StartOffset, ok),
                    Loaded;
                {error, Reason} ->
                    ?event(
                        arweave_offsets,
                        {read_chunks_not_found, 
                            {id, {explicit, ID}},
                            {format_version, Version},
                            {type, CodecName},
                            {start_offset, StartOffset},
                            {length, Length},
                            {reason, Reason}
                        }
                    ),
                    record_partition_metric(StartOffset, not_found),
                    if Reason =:= not_found -> not_found;
                    true -> {error, Reason}
                    end
            end;
        not_found ->
            ?event(
                arweave_offsets,
                {miss, {id, {explicit, ID}}}
            ),
            not_found
    end.

%% @doc Load an ANS-104 item header and return a message
%% with the parsed header metadata plus a data-ref for
%% lazy body loading. Fetches only the first chunk to
%% parse the ANS-104 header.
load_item(StartOffset, Length, Opts) ->
    hb_prometheus:measure_and_report(
        fun() ->
            HeaderSize = min(Length, ?DATA_CHUNK_SIZE),
            case read_chunks(StartOffset, HeaderSize, Opts) of
                {ok, HeaderData} ->
                    case ar_bundles:deserialize_header(HeaderData) of
                        {ok, _HdrSize, HeaderTX} ->
                            {ok, lazy_item(HeaderTX, StartOffset, Length, Opts)};
                        _ ->
                            {error, invalid_ans104_header}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
        end,
        hb_store_arweave_chunk_fetch_duration_seconds,
        [load_item]
    ).

%% @doc Convert a parsed ANS-104 header into a lazy message that preserves the
%% item metadata while deferring body reads to materialize/2.
lazy_item(HeaderTX, StartOffset, Length, Opts) ->
    BaseFields = [<<"anchor">>, <<"target">>],
    LazyHeader =
        HeaderTX#tx{
            id = dev_arweave_common:generate_id(HeaderTX, signed),
            data = ?DEFAULT_DATA,
            data_size = 0
        },
    Fields = dev_codec_ans104_from:fields(LazyHeader, <<>>, Opts),
    Tags = dev_codec_ans104_from:tags(LazyHeader, Opts),
    Data = #{},
    CommittedKeys =
        dev_codec_ans104_from:committed(
            BaseFields,
            LazyHeader,
            Fields,
            Tags,
            Data,
            Opts
        ),
    Base =
        dev_codec_ans104_from:base(
            CommittedKeys,
            Fields,
            Tags,
            Data,
            Opts
        ),
    Structured =
        dev_codec_ans104_from:with_commitments(
            BaseFields,
            LazyHeader,
            <<"ans104@1.0">>,
            dev_codec_ans104_from:fields(LazyHeader, ?FIELD_PREFIX, Opts),
            Tags,
            Base,
            CommittedKeys,
            Opts
        ),
    Structured#{
        <<"data-ref">> => #{
            <<"codec">> => <<"ans104@1.0">>,
            <<"offset">> => StartOffset,
            <<"length">> => Length
        }
    }.

%% @doc Return a lazy data-ref for a TX. The TX header
%% is read from the dedicated TX header cache when
%% available, falling back to `~arweave@2.9/tx`.
load_tx(ID, StartOffset, Length, Opts) ->
    {ok, Msg} =
        case dev_arweave_tx_cache:read(ID, Opts) of
            {ok, CachedTXHeader} ->
                {ok, CachedTXHeader};
            not_found ->
                hb_ao:resolve(
                    #{ <<"device">> => <<"arweave@2.9">> },
                    #{
                        <<"path">> => <<"tx">>,
                        <<"tx">> => ID,
                        <<"exclude-data">> => true
                    },
                    Opts
                )
        end,
    {ok, Msg#{
        <<"data-ref">> => #{
            <<"codec">> => <<"tx@1.0">>,
            <<"offset">> => StartOffset,
            <<"length">> => Length
        }
    }}.

%% @doc Read the chunks from the given start offset and length using the 
%% `~arweave@2.9` device.
read_chunks(StartOffset, Length, Opts) ->
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => StartOffset + 1,
            <<"length">> => Length
        },
        Opts
    ).

%% @doc Materialize a lazy message by fetching chunk data
%% and rebuilding the full message structure. Messages
%% without data-ref pass through unchanged.
materialize(Msg, Opts) when is_map(Msg) ->
    case maps:get(<<"data-ref">>, Msg, not_found) of
        not_found -> {ok, Msg};
        #{
            <<"codec">> := <<"ans104@1.0">>,
            <<"offset">> := Offset,
            <<"length">> := Length
        } ->
            case read_chunks(Offset, Length, Opts) of
                {ok, Data} ->
                    {ok,
                        hb_message:convert(
                            ar_bundles:deserialize(Data),
                            <<"structured@1.0">>,
                            <<"ans104@1.0">>,
                            Opts
                        )
                    };
                {error, Reason} -> {error, Reason}
            end;
        #{
            <<"codec">> := <<"tx@1.0">>,
            <<"offset">> := Offset,
            <<"length">> := Length
        } ->
            Header =
                hb_message:convert(
                    maps:remove(<<"data-ref">>, Msg),
                    <<"tx@1.0">>,
                    <<"structured@1.0">>,
                    Opts
                ),
            case Length of
                0 ->
                    {ok, hb_message:convert(
                        Header,
                        <<"structured@1.0">>,
                        <<"tx@1.0">>,
                        Opts)};
                _ ->
                    case read_chunks(Offset, Length, Opts) of
                        {ok, Data} ->
                            {ok,
                                hb_message:convert(
                                    Header#tx{data = Data},
                                    <<"structured@1.0">>,
                                    <<"tx@1.0">>,
                                    Opts
                                )
                            };
                        {error, Reason} -> {error, Reason}
                    end
            end
    end;
materialize(Msg, _Opts) -> {ok, Msg}.

%% @doc Write offset information to the index store.
write_offset(
        #{ <<"index-store">> := IndexStore },
        ID,
        CodecName,
        StartOffset,
        Length
    ) ->
    Value = hb_store_arweave_offset:encode(CodecName, StartOffset, Length),
    ?event(
        debug_store_arweave,
        {writing_offset, 
            {id, {explicit, ID}},
            {type, CodecName},
            {start_offset, StartOffset},
            {length, Length},
            {value, {explicit, Value}}
        }
    ),
    hb_store:write(IndexStore, hb_store_arweave_offset:path(ID), Value).

%% @doc Record the partition that data is found in when it is requested.
record_partition_metric(Offset, Result) when is_integer(Offset) ->
    spawn(fun() -> 
        hb_prometheus:inc(
            counter,
            hb_store_arweave_requests_partition,
            [Offset div ?PARTITION_SIZE, hb_util:bin(Result)],
            1
        )
    end).

%% @doc Initialize the Prometheus metrics for the Arweave store. Executed on
%% `start/1' of the store.
init_prometheus() ->
    hb_prometheus:declare(
        histogram,
        [
            {name, hb_store_arweave_index_check_duration_seconds},
            {buckets, [0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 2, 5, 10]},
            {help, "How much it takes to check the index"}
        ]
    ),
    hb_prometheus:declare(
        histogram,
        [
            {name, hb_store_arweave_chunk_fetch_duration_seconds},
            {buckets, [0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 30, 60]},
            {labels, [type]},
            {help, "How much it takes to check the index"}
        ]
    ),
    hb_prometheus:declare(
        counter,
        [
            {name, hb_store_arweave_requests_partition},
            {labels, [partition, result]},
            {help, "Partition where chunks are being requested"}
        ]
    ),
    % We also depend on the HTTP client, so we ensure its prometheus metrics are
    % initialized, too.
    hb_http_client:init_prometheus().

%%% Tests

write_read_tx_test() ->
    Store = [hb_test_utils:test_store()],
    Opts = #{ 
        <<"index-store">> => Store 
    },
    ID = <<"bndIwac23-s0K11TLC1N7z472sLGAkiOdhds87ZywoE">>,
    EndOffset = 363524457284025,
    Size = 8387,
    StartOffset = EndOffset - Size,
    ok = write_offset(Opts, ID, <<"tx@1.0">>, StartOffset, Size),
    {ok, LazyBundle} = read(Opts, ID),
    {ok, Bundle} = materialize(LazyBundle, Opts),
    ?assert(hb_message:verify(Bundle, all, #{})),
    {ok, Child} =
        hb_ao:resolve(
            Bundle,
            <<"1/2">>,
            #{}
        ),
    ?assert(hb_message:verify(Child, all, #{})),
    ExpectedChild = #{
        <<"data">> =>
            <<
                "{\"totalTickedRewardsDistributed\":0,\"distributedEpochIndexes\""
                ":[],\"newDemandFactors\":[],\"newEpochIndexes\":[],\""
                "tickedRewardDistributions\":[],\"newPruneGatewaysResults\""
                ":[{\"delegateStakeReturned\":0,\"stakeSlashed\":0,\""
                "gatewayStakeReturned\":0,\"delegateStakeWithdrawing\":0,\""
                "prunedGateways\":[],\"slashedGateways\":[],\""
                "gatewayStakeWithdrawing\":0}]}">>,
        <<"data-protocol">> => <<"ao">>,
        <<"from-module">> => <<"cbn0KKrBZH7hdNkNokuXLtGryrWM--PjSTBqIzw9Kkk">>,
        <<"from-process">> => <<"agYcCFJtrMG6cqMuZfskIkFTGvUPddICmtQSBIoPdiA">>,
        <<"anchor">> => <<"MDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAyODAxODg">>,
        <<"reference">> => <<"280188">>,
        <<"target">> => <<"1R5QEtX53Z_RRQJwzFWf40oXiPW2FibErT_h02pu8MU">>,
        <<"type">> => <<"Message">>,
        <<"variant">> => <<"ao.TN.1">>
    },
    ?assert(hb_message:match(ExpectedChild, Child, only_present)),
    ok.

%% @doc The L1 TX has bundle tags, but data is not a valid bundle.
write_read_fake_bundle_tx_test() ->
    Store = [hb_test_utils:test_store()],
    Opts = #{ 
        <<"index-store">> => Store 
    },
    ID = <<"cGNURX2IUt98VKVIeXSfYe6eulNwPEqijaQfvatzd_o">>,
    Size = 2,
    StartOffset = 155309918167286,
    ok = write_offset(Opts, ID, <<"tx@1.0">>, StartOffset, Size),
    {ok, LazyTX} = read(Opts, ID),
    {ok, TX} = materialize(LazyTX, Opts),
    ?assert(hb_message:verify(TX, all, #{})),
    ok.

load_tx_from_cache_test() ->
    Store = [hb_test_utils:test_store()],
    Opts = #{
        <<"index-store">> => Store,
        store => Store,
        priv_wallet => hb:wallet()
    },
    ok = start(Opts),
    Header = test_tx_header(Opts),
    TXID = hb_message:id(Header, signed, Opts),
    ok = dev_arweave_tx_cache:write(Header, Opts),
    ok = write_offset(Opts, TXID, <<"tx@1.0">>, 0, 0),
    {ok, LazyTX} = read(Opts, TXID),
    ?assertEqual(Header, maps:remove(<<"data-ref">>, LazyTX)),
    {ok, Materialized} = materialize(LazyTX, Opts),
    ?assertEqual(TXID, hb_message:id(Materialized, signed, Opts)),
    ?assertEqual(false, maps:is_key(<<"data">>, Materialized)).

lazy_item_manifest_header_test() ->
    Opts = #{},
    JSON =
        hb_json:encode(
            #{
                <<"paths">> => #{
                    <<"index.html">> => #{ <<"id">> => <<"some-id">> }
                },
                <<"index">> => #{ <<"path">> => <<"index.html">> }
            }
        ),
    SignedItem =
        ar_bundles:sign_item(
            ar_bundles:new_item(
                <<>>,
                <<>>,
                [
                    {
                        <<"content-type">>,
                        <<"application/x.arweave-manifest+json">>
                    }
                ],
                JSON
            ),
            hb:wallet()
        ),
    FullItem =
        hb_message:convert(
            SignedItem,
            <<"structured@1.0">>,
            <<"ans104@1.0">>,
            Opts
        ),
    LazyItem =
        lazy_item(
            SignedItem,
            100,
            byte_size(ar_bundles:serialize(SignedItem)),
            Opts
        ),
    ?assertEqual(
        <<"application/x.arweave-manifest+json">>,
        maps:get(<<"content-type">>, LazyItem)
    ),
    ?assertEqual(false, maps:is_key(<<"data">>, LazyItem)),
    ?assertEqual(
        hb_message:id(FullItem, signed, Opts),
        hb_message:id(LazyItem, signed, Opts)
    ),
    ?assertMatch(
        {ok,
            #{
                <<"body">> := [
                    {as, <<"manifest@1.0">>, _},
                    #{<<"path">> := <<"index">>}
                ]
            }
        },
        dev_manifest:request(#{}, #{ <<"body">> => [LazyItem] }, Opts)
    ).

test_tx_header(Opts) ->
    Msg =
        hb_message:commit(
            #{
                <<"content-type">> => <<"text/plain">>,
                <<"data">> => <<"test-data">>,
                <<"test-key">> => <<"test-value">>
            },
            Opts,
            #{ <<"commitment-device">> => <<"tx@1.0">> }
        ),
    TX = hb_message:convert(Msg, <<"tx@1.0">>, <<"structured@1.0">>, Opts),
    hb_message:convert(
        TX#tx{ data = <<>> },
        <<"structured@1.0">>,
        <<"tx@1.0">>,
        Opts
    ).
