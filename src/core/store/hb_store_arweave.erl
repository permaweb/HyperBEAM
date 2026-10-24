%%% @doc A store implementation that relays to an Arweave node, using an 
%%% intermediate cache of offsets as an ID->ArweaveLocation mapping. Stores with
%%% `remote-index' set additionally source the offsets they lack from the nodes
%%% serving the `~arweave@2.9' routes, allowing a node to read data from Arweave
%%% before it has indexed the weave itself.
-module(hb_store_arweave).
%%% Store API:
-export([scope/0, scope/1, read/3, start/3]).
%%% Unused Store API:
-export([write/3]).
%%% Indexing API:
-export([store_from_opts/1, write_offset/5, read_chunks/3]).
-export([read_offset/3, read_index_offset/2]).
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
start(#{<<"index-store">> := IndexStore}, _Req, _Opts) ->
    init_prometheus(),
    hb_store:start(IndexStore).

%% @doc Although the index is local, loading an item via the index will make
%% requests to a remote node, so we define the scope as remote.
scope() -> remote.
scope(#{ <<"scope">> := Scope }) -> Scope;
scope(_) -> scope().

%% @doc Unsupported.
write(_, _, _) -> {error, not_found}.

%% @doc Read the offset of the data at the given key.
read_offset(StoreOpts, ID, Opts) ->
    case read_index_offset(StoreOpts, ID) of
        not_found -> read_remote_offset(StoreOpts, ID, Opts);
        Offset -> Offset
    end.

%% @doc Read the offset of the data at the given key from the local index alone.
read_index_offset(StoreOpts = #{ <<"index-store">> := _ }, ID) ->
    maybe
        % First check the full offset path from the `~arweave@2.9` device.
        not_found ?= read_offset_path(StoreOpts, <<"~arweave@2.9/offset=", ID/binary>>),
        % ...otherwise, check the raw binary path for the ID.
        read_offset_path(StoreOpts, hb_store_arweave_offset:path(ID))
    end;
read_index_offset(_, _) -> not_found.

%% @doc Read and parse offset results. Processes both a message response or a
%% version 1 binary encoded response.
read_offset_path(StoreOpts = #{ <<"index-store">> := IndexStore }, Path) ->
    ReadRes =
        hb_prometheus:measure_and_report(
            fun() ->
                hb_store:read(
                    IndexStore,
                    #{ <<"read">> => Path },
                    StoreOpts
                )
            end,
            hb_store_arweave_index_check_duration_seconds
        ),
    case ReadRes of
        {ok, Res} when is_map(Res) ->
            {ok, Res#{
                <<"version">> => maps:get(<<"version">>, Res, 2),
                <<"codec">> =>
                    maps:get(
                        <<"codec">>,
                        Res,
                        <<"ans104@1.0">>
                    )
            }};
        {ok, OffsetBinary} when is_binary(OffsetBinary) ->
            {Version, CodecName, StartOffset, Length} =
                hb_store_arweave_offset:decode(OffsetBinary),
            {ok, #{
                <<"version">> => Version,
                <<"codec">> => CodecName,
                <<"start">> => StartOffset,
                <<"length">> => Length
            }};
        _ ->
            not_found
    end.

%% @doc Find the offset of a message from the nodes serving the `~arweave@2.9'
%% routes. The response gives the start of the item alongside the lengths of its
%% header and data segments: an item with no header inside the weave's data tree
%% is a transaction, one with a header is an ANS-104 item, and the two lengths
%% sum to the span that the loader reads. Pending transactions report offsets
%% relative to themselves, so only confirmed items are found. The offset is not
%% added to the local index here: `do_read/3' adds it once a read at it succeeds.
read_remote_offset(
        #{ <<"index-store">> := _, <<"remote-index">> := true },
        ID,
        Opts
    ) ->
    Res =
        hb_http:request(
            #{
                <<"method">> => <<"HEAD">>,
                <<"path">> => <<"/~arweave@2.9/raw=", ID/binary>>
            },
            Opts
        ),
    ?event(arweave_offsets, {remote_index, {id, {explicit, ID}}, {result, Res}}),
    case Res of
        {ok,
            #{
                <<"offset">> := StartOffset,
                <<"header-length">> := HeaderLength,
                <<"data-length">> := DataLength
            }} when is_integer(StartOffset) ->
            CodecName =
                case HeaderLength of
                    0 -> <<"tx@1.0">>;
                    _ -> <<"ans104@1.0">>
                end,
            % Give the offset as the local index would give it once stored.
            {Version, CodecName, StartOffset, Length} =
                hb_store_arweave_offset:decode(
                    hb_store_arweave_offset:encode(
                        CodecName,
                        StartOffset,
                        HeaderLength + DataLength
                    )
                ),
            {ok, #{
                <<"version">> => Version,
                <<"codec">> => CodecName,
                <<"start">> => StartOffset,
                <<"length">> => Length
            }};
        _ ->
            not_found
    end;
read_remote_offset(_, _, _) -> not_found.

%% @doc Read the data at the given key, reading the `local-store' first if
%% available. The reads use the node's options, overridden by the options in
%% the store's message. `force-message' is removed, as with it the
%% `~arweave@2.9' reads return messages, not data, and `store-module', as in
%% `hb_store_remote_node:read/3'.
read(StoreOpts, #{ <<"read">> := ID }, NodeOpts) when ?IS_ID(ID) ->
    Opts =
        maps:without(
            [<<"store-module">>, <<"force-message">>],
            hb_util:deep_merge(NodeOpts, StoreOpts, NodeOpts)
        ),
    case hb_store_remote_node:read_local_cache(StoreOpts, ID, Opts) of
        {ok, Message} ->
            ?event(
                arweave_offsets,
                {local_store_hit, {id, {explicit, ID}}}
            ),
            {ok, Message};
        _ ->
            case do_read(StoreOpts, ID, Opts) of
                not_found -> {error, not_found};
                Result -> Result
            end
    end;
read(_StoreOpts, #{ <<"read">> := _ID }, _NodeOpts) ->
    {error, not_found}.

%% @doc Read the data at the given key, reading the provided Arweave index store
%% as a source of offsets. After offsets have been found, the data is loaded
%% through the `~arweave@2.9` device -- either as an ANS-104 item or a TX.
do_read(StoreOpts, ID, Opts) ->
    case read_offset(StoreOpts, ID, Opts) of
        {ok,
            #{
                <<"version">> := Version,
                <<"codec">> := CodecName,
                <<"start">> := StartOffset,
                <<"length">> := Length
            }} ->
            Loaded =
                case CodecName of
                    <<"ans104@1.0">> -> load_item(ID, StartOffset, Length, Opts);
                    <<"tx@1.0">> -> load_tx(ID, StartOffset, Length, Opts)
                end,
            case Loaded of
                {ok, Message} ->
                    keep_offset(StoreOpts, ID, CodecName, StartOffset, Length),
                    hb_store_remote_node:maybe_cache(StoreOpts, Message),
                    ?event(
                        arweave_offsets,
                        {read_ok,
                            {id, {string, ID}},
                            {format_version, Version},
                            {type, CodecName},
                            {start_offset, StartOffset},
                            {length, Length}
                        }
                    ),
                    record_partition_metric(StartOffset, ok, Opts),
                    Loaded;
                {error, Reason} ->
                    ?event(
                        arweave_offsets,
                        {read_chunks_not_found, 
                            {id, {string, ID}},
                            {format_version, Version},
                            {type, CodecName},
                            {start_offset, StartOffset},
                            {length, Length},
                            {reason, Reason}
                        }
                    ),
                    record_partition_metric(StartOffset, not_found, Opts),
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

%% @doc Add the offset that a read succeeded at to the local index, unless the
%% index holds it already. Offsets from the `remote-index' nodes enter the index
%% only here, so a read that fails at one leaves no entry, and the next read
%% asks the nodes again.
keep_offset(StoreOpts, ID, CodecName, StartOffset, Length) ->
    case read_index_offset(StoreOpts, ID) of
        not_found -> write_offset(StoreOpts, ID, CodecName, StartOffset, Length);
        _ -> ok
    end.

%% @doc Load an ANS-104 item from the given start offset and length.
%% Returns an `ok' tuple with the deserialized item, or an `error' tuple with
%% the reason. The `StartOffset` is the precise starting byte of the item _header_,
%% not the data segment. The `Length` covers the full size of the item, including
%% header. The `ExpectedID` is verified against the deserialized item's ID to
%% guard against stale offsets (e.g. after a reorg). The ID is the hash of the
%% signature alone, so the signature must verify too. Both checks run on the
%% bytes read, before the codec unbundles any items that they hold.
load_item(ExpectedID, StartOffset, Length, Opts) ->
    hb_prometheus:measure_and_report(
        fun() ->
            maybe
                {ok, SerializedItem} ?= read_chunks(StartOffset, Length, Opts),
                Item = ar_bundles:deserialize_item_wrapper(SerializedItem),
                ActualID = hb_util:encode(Item#tx.id),
                true ?=
                    ActualID =:= ExpectedID
                        orelse {error, {id_mismatch, ExpectedID, ActualID}},
                true ?=
                    ar_bundles:verify_item(Item)
                        orelse {error, unverifiable_item},
                {ok, hb_message:convert(
                    Item,
                    <<"structured@1.0">>,
                    <<"ans104@1.0">>,
                    Opts
                )}
            end
        end,
        hb_store_arweave_chunk_fetch_duration_seconds,
        [load_item]
    ).

%% @doc Load a TX from the given start offset and length. The `StartOffset' is
%% the start of the first chunk of the data and runs for the length of the data
%% segment, ignoring header size.
%% Pending TX offsets are fetched by ID.
load_tx(ID, relative, _Length, Opts) ->
    hb_prometheus:measure_and_report(
        fun() ->
            hb_ao:resolve(
                #{ <<"device">> => <<"arweave@2.9">> },
                #{
                    <<"path">> => <<"pending">>,
                    <<"pending">> => ID,
                    <<"exclude-data">> => false
                },
                Opts
            )
        end,
        hb_store_arweave_chunk_fetch_duration_seconds,
        [load_tx]
    );
load_tx(ID, StartOffset, Length, Opts) ->
    hb_prometheus:measure_and_report(
        fun() ->
            {ok, StructuredTXHeader} = hb_ao:resolve(
                #{ <<"device">> => <<"arweave@2.9">> },
                #{
                    <<"path">> => <<"tx">>,
                    <<"tx">> => ID,
                    <<"exclude-data">> => true
                },
                Opts
            ),
            TXHeader =
                hb_message:convert(
                    StructuredTXHeader,
                    <<"tx@1.0">>,
                    <<"structured@1.0">>,
                    Opts
                ),
            case Length of
                0 ->
                    {ok, hb_message:convert(
                        TXHeader,
                        <<"structured@1.0">>,
                        <<"tx@1.0">>,
                        Opts)};
                _ ->
                    maybe
                        {ok, Data} ?= read_chunks(StartOffset, Length, Opts),
                        TX = TXHeader#tx{data = Data},
                        % The data must be the data that the signature covers.
                        true ?= ar_tx:verify(TX) orelse {error, data_mismatch},
                        {ok, hb_message:convert(
                            TX,
                            <<"structured@1.0">>,
                            <<"tx@1.0">>,
                            Opts
                        )}
                    end
            end
        end,
        hb_store_arweave_chunk_fetch_duration_seconds,
        [load_tx]
    ).

%% @doc Read the chunks from the given start offset and length using the 
%% `~arweave@2.9` device.
read_chunks(#{ <<"relative">> := TXID, <<"offset">> := Offset }, Length, Opts) ->
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"pending">> => TXID,
            <<"offset">> => Offset,
            <<"length">> => Length
        },
        Opts
    );
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
        StoreOpts = #{ <<"index-store">> := IndexStore },
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
    hb_store:write(
        IndexStore,
        #{ hb_store_arweave_offset:path(ID) => Value },
        StoreOpts
    ).

%% @doc Record the partition that data is found in when it is requested.
record_partition_metric(Offset, Result, StoreOpts) when is_integer(Offset) ->
    case hb_opts:get(prometheus, not hb_features:test(), StoreOpts) of
        true ->
            spawn(fun() ->
                hb_prometheus:inc(
                    counter,
                    hb_store_arweave_requests_partition,
                    [Offset div ?PARTITION_SIZE, hb_util:bin(Result)],
                    1
                )
            end);
        false ->
            ok
    end;
record_partition_metric(_, _, _) -> ok.

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

slash_offset_key_read_test_parallel() ->
    IndexStore = [hb_test_utils:test_store(hb_store_lmdb)],
    Opts = #{
        <<"store">> => [#{
            <<"store-module">> => hb_store_arweave,
            <<"index-store">> => IndexStore
        }]
    },
    Store = store_from_opts(Opts),
    ID = <<"L2FiY2RlZmdoaWprbG0vL29wcXJzdHV2d3h5ejAxMi8">>,
    ?assertEqual(
        <<"/abcdefghijklm//opqrstuvwxyz012/">>,
        hb_store_arweave_offset:path(ID)
    ),
    ok = write_offset(Store, ID, <<"tx@1.0">>, 123, 456),
    ?assertMatch(
        {ok, #{ <<"start">> := 123, <<"length">> := 456 }},
        read_offset(Store, ID, Opts)
    ).

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
    {ok, Bundle} = read(Opts, #{ <<"read">> => ID }, Opts),
    ?assert(hb_message:verify(Bundle, all, #{})),
    % A read with the `force-message' of an HTTP request runs the node's
    % `http-client-receipt' hook.
    Self = self(),
    Receipt =
        fun(_, Req, _) ->
            Self ! receipt,
            {ok, Req}
        end,
    {ok, _} =
        read(
            Opts,
            #{ <<"read">> => ID },
            #{
                <<"store">> => [hb_test_utils:test_store()],
                <<"force-message">> => true,
                <<"priv-wallet">> => ar_wallet:new(),
                <<"on">> => #{
                    <<"http-client-receipt">> =>
                        #{ <<"device">> => #{ http_client_receipt => Receipt } }
                }
            }
        ),
    ?assertEqual(
        receipt,
        receive receipt -> receipt after 5000 -> 'missing-receipt' end
    ),
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

%% @doc Stale ANS-104 offset: fake ID pointing to a known bundle TX's
%% data range. The deserialized item's ID won't match the fake ID.
stale_ans104_offset_returns_error_test() ->
    Store = [hb_test_utils:test_store()],
    Opts = #{<<"index-store">> => Store},
    FakeID = <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,
    RealEndOffset = 363524457284025,
    RealSize = 8387,
    RealStartOffset = RealEndOffset - RealSize,
    ok = write_offset(Opts, FakeID, <<"ans104@1.0">>, RealStartOffset, RealSize),
    Result = read(Opts, #{ <<"read">> => FakeID }, Opts),
    ?assertMatch({error, {id_mismatch, _, _}}, Result).

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
    {ok, TX} = read(Opts, #{ <<"read">> => ID }, Opts),
    ?assert(hb_message:verify(TX, all, #{})),
    % A form of the ID with other spare bits in its last character raises.
    OtherForm = <<"cGNURX2IUt98VKVIeXSfYe6eulNwPEqijaQfvatzd_p">>,
    ?assertError(
        {badmatch, ID},
        read(Opts, #{ <<"read">> => OtherForm }, Opts)
    ),
    ok.

%% @doc A node with no offset index of its own finds the offsets of an Arweave
%% transaction and an ANS-104 item from its routes, serving the item over HTTP
%% and retaining each offset once a read at it succeeds.
remote_index_test() ->
    Local =
        #{
            <<"store-module">> => hb_store_arweave,
            <<"index-store">> => [hb_test_utils:test_store()]
        },
    TXID = <<"bndIwac23-s0K11TLC1N7z472sLGAkiOdhds87ZywoE">>,
    ItemID = <<"1R5QEtX53Z_RRQJwzFWf40oXiPW2FibErT_h02pu8MU">>,
    % The store sends its offset requests to a node that holds both offsets.
    Index = Local#{ <<"index-store">> => [hb_test_utils:test_store()] },
    ok = write_offset(Index, TXID, <<"tx@1.0">>, 363524457275638, 8387),
    ok = write_offset(Index, ItemID, <<"ans104@1.0">>, 195193803403756, 1364),
    IndexNode = hb_http_server:start_node(#{ <<"store">> => [Index] }),
    Route =
        #{
            <<"template">> => <<"^/~arweave@2.9/raw">>,
            <<"node">> => #{ <<"prefix">> => IndexNode }
        },
    Store =
        Local#{
            <<"remote-index">> => true,
            <<"routes">> => [Route | hb_opts:get(routes, [], #{})]
        },
    ?assertEqual(not_found, read_offset(Local, TXID, #{})),
    ?assertMatch(
        {ok, #{ <<"codec">> := <<"tx@1.0">>, <<"length">> := 8387 }},
        read_offset(Store, TXID, Store)
    ),
    % The offset is retained once a read at it succeeds, so the routes are
    % asked once per message.
    ?assertEqual(not_found, read_offset(Local, TXID, #{})),
    ?assertMatch({ok, _}, read(Store, #{ <<"read">> => TXID }, #{})),
    ?assertMatch({ok, #{ <<"length">> := 8387 }}, read_offset(Local, TXID, #{})),
    Node = hb_http_server:start_node(#{ <<"store">> => [Store] }),
    {ok, Msg} = hb_http:get(Node, <<"/", ItemID/binary>>, #{}),
    ?assert(hb_message:verify(Msg, all, #{})),
    ?assertEqual(ItemID, hb_message:id(Msg, signed, #{})),
    ?assertEqual(<<"1984">>, hb_ao:get(<<"data">>, Msg, #{})).
