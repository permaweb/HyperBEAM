%%% @doc A store implementation that relays to an Arweave node, using an 
%%% intermediate cache of offsets as an ID->ArweaveLocation mapping.
-module(hb_store_arweave).
%%% Store API:
-export([scope/0, scope/1, type/2, read/2, start/1]).
%%% Unused Store API:
-export([resolve/2, write/3, make_link/3, make_group/2]).
%%% Indexing API:
-export([store_from_opts/1, write_offset/5, read_offset/2, read_chunks/3]).
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

%% @doc Read the data at the given key, reading the `local-store' first if
%% available.
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
                        load_item(ID, StartOffset, Length, StoreOpts);
                    <<"tx@1.0">> ->
                        load_tx(ID, StartOffset, Length, StoreOpts)
                end,
            case Loaded of
                {ok, Message} ->
                    hb_store_remote_node:maybe_cache(StoreOpts, Message),
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

%% @doc Load an ANS-104 item from the given start offset and length.
%% Returns an `ok' tuple with the deserialized item, or an `error' tuple with
%% the reason. The `StartOffset` is the precise starting byte of the item _header_,
%% not the data segment. The `Length` covers the full size of the item, including
%% header. The `ExpectedID` is verified against the deserialized item's ID to
%% guard against stale offsets (e.g. after a reorg).
load_item(ExpectedID, StartOffset, Length, Opts) ->
    hb_prometheus:measure_and_report(
        fun() ->
            case read_chunks(StartOffset, Length, Opts) of
                {ok, SerializedItem} ->
                    Item = ar_bundles:deserialize(SerializedItem),
                    case hb_util:encode(Item#tx.id) of
                        ExpectedID ->
                            {
                                ok,
                                hb_message:convert(
                                    Item,
                                    <<"structured@1.0">>,
                                    <<"ans104@1.0">>,
                                    Opts
                                )
                            };
                        ActualID ->
                            {error, {id_mismatch, ExpectedID, ActualID}}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
        end,
        hb_store_arweave_chunk_fetch_duration_seconds,
        [load_item]
    ).

%% @doc Load a TX from the given start offset and length. The `StartOffset' is
%% the start of the first chunk of the data and runs for the length of the data
%% segment, ignoring header size. After reading chunks, verifies the data
%% matches the TX header's data_root to guard against stale offsets.
load_tx(ID, StartOffset, Length, Opts) ->
    hb_prometheus:measure_and_report(
        fun() ->
            {ok, StructuredTXHeader} = hb_ao:resolve(
                #{ <<"device">> => <<"arweave@2.9">> },
                #{ <<"path">> => <<"tx">>, <<"tx">> => ID, <<"exclude-data">> => true },
                Opts
            ),
            TXHeader =
                hb_message:convert(
                    StructuredTXHeader,
                    <<"tx@1.0">>,
                    <<"structured@1.0">>,
                    Opts
                ),
            case read_chunks(StartOffset, Length, Opts) of
                {ok, Data} ->
                    TXWithData = ar_tx:generate_chunk_tree(
                        TXHeader#tx{ data = Data }),
                    case TXWithData#tx.data_root =:= TXHeader#tx.data_root of
                        true ->
                            {
                                ok,
                                hb_message:convert(
                                    TXWithData,
                                    <<"structured@1.0">>,
                                    <<"tx@1.0">>,
                                    Opts
                                )
                            };
                        false ->
                            {error, {data_root_mismatch, ID}}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
        end,
        hb_store_arweave_chunk_fetch_duration_seconds,
        [load_tx]
    ).

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
    {ok, Bundle} = read(Opts, ID),
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

%% @doc Stale ANS-104 offset must return id_mismatch, not wrong data.
%% Writes an ans104 offset for a fake ID pointing to a known bundle TX's
%% data range. The deserialized item's ID won't match the fake ID.
stale_ans104_offset_returns_error_test() ->
    Store = [hb_test_utils:test_store()],
    Opts = #{<<"index-store">> => Store},
    FakeID = <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,
    RealEndOffset = 363524457284025,
    RealSize = 8387,
    RealStartOffset = RealEndOffset - RealSize,
    ok = write_offset(Opts, FakeID, <<"ans104@1.0">>, RealStartOffset, RealSize),
    Result = read(Opts, FakeID),
    ?assertMatch({error, {id_mismatch, _, _}}, Result).

%% @doc Stale TX offset must return data_root_mismatch, not wrong data.
%% Uses a real TX ID but points the offset at a different TX's data.
%% The header is fetched by ID (correct), but the chunk data won't match
%% the header's data_root.
stale_tx_offset_returns_error_test() ->
    Store = [hb_test_utils:test_store()],
    Opts = #{<<"index-store">> => Store},
    RealID = <<"bndIwac23-s0K11TLC1N7z472sLGAkiOdhds87ZywoE">>,
    WrongOffset = 155309918167286,
    WrongSize = 2,
    ok = write_offset(Opts, RealID, <<"tx@1.0">>, WrongOffset, WrongSize),
    Result = read(Opts, RealID),
    ?assertMatch({error, {data_root_mismatch, _}}, Result).

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
    {ok, TX} = read(Opts, ID),
    ?assert(hb_message:verify(TX, all, #{})),
    ok.