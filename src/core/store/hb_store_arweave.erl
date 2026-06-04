%%% @doc A store implementation that relays to an Arweave node, using an 
%%% intermediate cache of offsets as an ID->ArweaveLocation mapping.
-module(hb_store_arweave).
%%% Store API:
-export([scope/0, scope/1, type/3, read/3, start/3]).
%%% Unused Store API:
-export([resolve/3, write/3, link/3, group/3]).
%%% Indexing API:
-export([store_from_opts/1, write_offset/6, write_parent/5, read_offset/3, read_parent/3, decode_parent_entries/1, read_chunks/3]).
-export([block_indexed_path/1, block_items_path/2]).
-export([read_block_item_counts/2, read_block_item_ids/2]).
-export([is_tx_indexed/2 ]).
-export([write_block_item_ids/4, read_block_marker_depth/2]).
-export([decode_item_ids/1, is_block_indexed/3, mark_block_indexed/3 ]).
-export([root_offset/2]).
-include("include/hb.hrl").
-include("core/include/hb_store_arweave.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PARTITION_SIZE, 3_600_000_000_000).

%% @doc Find the first Arweave store from the given node message. Searches first
%% for the `arweave_index_store' option, and if not found, searches the main
%% `store' list for the first Arweave store with an index.
store_from_opts(Opts) ->
    case hb_opts:get(<<"arweave-index-store">>, no_store, Opts) of
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
start(#{<<"index-store">> := IndexStore}, Req, Opts) ->
    init_prometheus(),
    hb_store:start(IndexStore, Req, Opts).

%% @doc Although the index is local, loading an item via the index will make
%% requests to a remote node, so we define the scope as remote.
scope() -> remote.
scope(#{ <<"scope">> := Scope }) -> Scope;
scope(_) -> scope().

%% @doc Resolve a key path in the Arweave store, ignoring other paths.
resolve(_Store, #{ <<"resolve">> := ID }, _NodeOpts) when ?IS_ID(ID) ->
    {ok, ID};
resolve(_Store, #{ <<"resolve">> := _ID }, _NodeOpts) ->
    {error, not_found}.

%% @doc Unsupported.
write(_, _, _) -> {error, not_found}.

%% @doc Unsupported.
link(_, _, _) -> {error, not_found}.

%% @doc Unsupported.
group(_, _, _) -> {error, not_found}.

%% @doc Get the type of the data at the given key. We potentially cache the
%% result, so that we don't have to read the data from the GraphQL route
%% multiple times.
type(#{ <<"index-store">> := IndexStore }, #{ <<"type">> := ID }, NodeOpts)
        when ?IS_ID(ID) ->
    case hb_store:read(IndexStore, hb_store_arweave_offset:path(ID), NodeOpts) of
        {ok, _Offset} ->
            {ok, simple};
        _ ->
            {error, not_found}
    end;
type(_Store, #{ <<"type">> := _ID }, _NodeOpts) ->
    {error, not_found}.

%% @doc Read the offset of the data at the given key.
read_offset(StoreOpts = #{ <<"index-store">> := IndexStore }, ID, Opts) ->
    ReadRes =
        hb_prometheus:measure_and_report(
            fun() ->
                hb_store:read(IndexStore, hb_store_arweave_offset:path(ID), StoreOpts)
            end,
            hb_store_arweave_index_check_duration_seconds
        ),
    case ReadRes of
        {ok, OffsetBinary} ->
            {CodecName, Offset, Length} =
                hb_store_arweave_offset:decode(OffsetBinary),
            {ok, #{
                <<"codec-device">> => CodecName,
                <<"offset">> => Offset,
                <<"length">> => Length
            }};
        _ ->
            not_found
    end;
read_offset(_, _, _) -> not_found.

%% @doc Read the parent entries for an item from the index store.
read_parent(#{ <<"index-store">> := IndexStore }, ID, Opts) ->
    NormalizedID = hb_util:native_id(ID),
    ParentPath = <<"parent/", NormalizedID/binary>>,
    case hb_store:read(IndexStore, ParentPath, Opts) of
        {ok, Bin} ->
            case decode_parent_entries(Bin) of
                {error, _} = Err -> Err;
                Entries -> {ok, Entries}
            end;
        _ ->
            not_found
    end;
read_parent(_, _, _) -> not_found.

decode_parent_entries(<<>>) -> [];
decode_parent_entries(<<0, Height:64/big-unsigned, Rest/binary>>) ->
    case decode_parent_entries(Rest) of
        {error, _} = Err -> Err;
        Tail -> [{Height, block} | Tail]
    end;
decode_parent_entries(<<1, ParentID:32/binary, Rest/binary>>) ->
    case decode_parent_entries(Rest) of
        {error, _} = Err -> Err;
        Tail -> [{ParentID, bundle} | Tail]
    end;
decode_parent_entries(_Corrupt) ->
    {error, corrupt_parent_data}.


%% @doc Return the store path for a parent index entry.
parent_path(ItemID) when byte_size(ItemID) =:= 32 ->
    <<"parent/", ItemID/binary>>.

%% @doc Encode a parent entry for storage.
encode_parent_entry(Height, block) when is_integer(Height) ->
    <<0, Height:64/big-unsigned>>;
encode_parent_entry(ParentID, bundle) when byte_size(ParentID) =:= 32 ->
    <<1, ParentID:32/binary>>.

%% Block Information Index

%% @doc Return the store path for a block completion marker.
block_indexed_path(Height) ->
    <<"block/", (hb_util:bin(Height))/binary, "/depth">>.

%% @doc Return the store path for a per-block item index at a given depth.
block_items_path(Height, Depth) ->
    <<"block/", (hb_util:bin(Height))/binary,
        "/items/", (hb_util:bin(Depth))/binary>>.

%% @doc Read the data at the given key, reading the `local-store' first if
%% available.
read(StoreOpts, #{ <<"read">> := ID }, _NodeOpts) when ?IS_ID(ID) ->
    case hb_store_remote_node:read_local_cache(StoreOpts, ID, StoreOpts) of
        {ok, Message} ->
            {ok, Message};
        _ ->
            case do_read(StoreOpts, ID, StoreOpts) of
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
                <<"codec-device">> := Codec,
                <<"offset">> := Offset,
                <<"length">> := Length
            }
        } ->
            Loaded =
                load_message(
                    Codec,
                    ID,
                    root_offset(Offset, StoreOpts),
                    Length,
                    StoreOpts
                ),
            case Loaded of
                {ok, Message} ->
                    hb_store_remote_node:maybe_cache(StoreOpts, Message),
                    ?event(
                        arweave_offsets,
                        {read_ok,
                            {id, {string, ID}},
                            {codec, Codec},
                            {offset, Offset},
                            {length, Length}
                        }
                    ),
                    record_partition_metric(Offset, ok, StoreOpts),
                    Loaded;
                {error, Reason} ->
                    ?event(
                        arweave_offsets,
                        {read_chunks_not_found, 
                            {id, {string, ID}},
                            {codec, Codec},
                            {offset, Offset},
                            {length, Length},
                            {reason, Reason}
                        }
                    ),
                    record_partition_metric(Offset, not_found, StoreOpts),
                    if Reason =:= not_found -> not_found;
                    true -> {error, Reason}
                    end
            end;
        not_found ->
            ?event(arweave_offsets, {miss, {id, {explicit, ID}}}),
            not_found
    end.

%% @doc Takes a `read_offset/2' result and returns it, normalized to the
%% outer-most root that is known: Either the mempool or a global byte offset.
root_offset(relative, _Store) -> relative;
root_offset(GlobalOffset, _Store) when is_integer(GlobalOffset) -> GlobalOffset;
root_offset(Offset, Store) -> root_offset(Offset, 0, Store).
root_offset(#{ <<"relative">> := P, <<"offset">> := Off }, Acc, Store) ->
    case read_offset(Store, P) of
        {ok, #{ <<"offset">> := Next = #{ <<"relative">> := _, <<"offset">> := _ } }} ->
            % We have another relative offset. Continue.
            root_offset(Next, Acc + Off, Store);
        {ok, #{ <<"offset">> := relative }} ->
            % We have reached an unconfirmed TX as the root of the relative offset
            % chain, so we return an offset against that.
            #{ <<"relative">> => P, <<"offset">> => Acc + Off };
        {ok, #{ <<"offset">> := GlobalOffset }} when is_integer(GlobalOffset) ->
            % We have reached a confirmed TX as the root of the relative offset
            % chain, so we return a global offset.
            GlobalOffset + Acc + Off;
        _ ->
            % The result was unknown, so we total accumulator and current offset
            % and return it with the `relative` key intact.
            #{ <<"relative">> => P, <<"offset">> => Acc + Off }
    end;
root_offset(Other, _, _) -> Other.

%% @doc Load a TX from Arweave. Supports either confirmed or pending TXs.
load_message(<<"tx@1.0">>, ID, Type, _Length, Opts) ->
    % Determine the correct path to hit to load the TX. Confirmed TXs require
    % `tx=ID`, while pending TXs require `pending=ID`.
    PathKeys =
        if Type =:= relative -> #{ <<"path">> => <<"pending">>, <<"pending">> => ID };
        true -> #{ <<"path">> => <<"tx">>, <<"tx">> => ID }
        end,
    hb_prometheus:measure_and_report(
        fun() ->
            hb_ao:resolve(
                #{ <<"device">> => <<"arweave@2.9">> },
                PathKeys#{ <<"exclude-data">> => false },
                Opts
            )
        end,
        hb_store_arweave_chunk_fetch_duration_seconds,
        [load_tx]
    );
%% @doc Load an ANS-104 item from the given start offset and length.
%% The `StartOffset` is the precise starting byte of the item _header_,
%% not the data segment. The `Length` covers the full size of the item, including
%% header. The `ExpectedID` is verified against the deserialized item's ID to
%% guard against stale offsets (e.g. after a reorg).
load_message(<<"ans104@1.0">>, ID, Offset, Length, Opts) ->
    hb_prometheus:measure_and_report(
        fun() ->
            case read_chunks(Offset, Length, Opts) of
                {ok, SerializedItem} ->
                    try
                        Item =
                            ar_bundles:deserialize(SerializedItem),
                        case hb_util:encode(Item#tx.id) of
                            ID ->
                                {ok, hb_message:convert(
                                    Item,
                                    <<"structured@1.0">>,
                                    <<"ans104@1.0">>,
                                    Opts
                                )};
                            ActualID ->
                                ?event(error, {load_item, {id_mismatch}}),
                                {error, {id_mismatch, ID, ActualID}}
                        end
                    catch _:Reason:Stacktrace ->
                        %% Due to malformed encoding, attempt to deserialize
                        %% can throw.
                        ?event(error, 
                            {load_item, 
                                {expected_id, ID}, 
                                {reason, Reason},
                                {stacktrace, Stacktrace}
                            }),
                        {error, Reason}
                    end;
                {error, Reason} ->
                    ?event(error, {load_item, Reason}),
                    {error, Reason}
            end
        end,
        hb_store_arweave_chunk_fetch_duration_seconds,
        [load_item]
    ).

%% @doc Read the chunks from the given start offset and length using the 
%% `~arweave@2.9` device.
read_chunks(Offset, Length, Opts) ->
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> =>
                % TODO: The rationale for this seems to be that Arweave offsets
                % start at the last byte of the previous chunk. It is unclear
                % whether it is wise to apply this offset here, or perhaps it
                % should be applied in the device key itself.
                if is_integer(Offset) -> Offset + 1;
                true -> Offset
                end,
            <<"length">> => Length
        },
        Opts
    ).

%% @doc Write a parent entry for an item to the index store.
write_parent(ItemID, ParentData, Type, Store, Opts) ->
    case 
        lists:member(
            ?SCOPE_PARENT, 
            hb_opts:get(<<"copycat-scope">>, [], Opts)
        ) of
        true ->
            Entry = encode_parent_entry(ParentData, Type),
            hb_store:write(Store, #{parent_path(ItemID) => Entry}, Opts);
        false ->
            ok 
    end.

%% @doc Write offset information to the index store.
write_offset(
        #{ <<"index-store">> := IndexStore },
        ID,
        CodecName,
        StartOffset,
        Length,
        Opts
    ) ->
    case 
        lists:member(
            ?SCOPE_OFFSET, 
            hb_opts:get(<<"copycat-scope">>, [], Opts)
        ) of
        true ->
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
                Opts
             );
        false ->
            ok
    end.

%% @doc Probe item entries upward from depth 1, applying TransformFun to each.
probe_block_items(Height, Opts, TransformFun) ->
    case store_from_opts(Opts) of
        no_store -> 
            erlang:display({no_store, Opts}),
            #{};
        #{ <<"index-store">> := Store } ->
            probe_block_items(Height, Store, 1, #{}, TransformFun, Opts)
    end.

probe_block_items(Height, Store, Depth, Acc, TransformFun, Opts) ->
    case hb_store:read(Store, block_items_path(Height, Depth), Opts) of
        {ok, Bin} ->
            Key = hb_util:bin(Depth),
            probe_block_items(
                Height, Store, Depth + 1,
                Acc#{Key => TransformFun(Bin)}, TransformFun, Opts);
        {error, not_found} ->
            Acc
    end.

count_ids(Bin) when byte_size(Bin) rem 32 =:= 0 ->
    byte_size(Bin) div 32;
count_ids(_) -> <<"corrupt">>.

decode_and_encode_ids(Bin) ->
    case decode_item_ids(Bin) of
        {error, _} -> <<"corrupt">>;
        List -> [hb_util:encode(ID) || ID <- List]
    end.

read_block_item_counts(Height, Opts) ->
    probe_block_items(Height, Opts, fun count_ids/1).

read_block_item_ids(Height, Opts) ->
    probe_block_items(Height, Opts, fun decode_and_encode_ids/1).

%% @doc Write per-depth item ID lists for a block.
%% Writes an entry for every depth from 1 through AchievedDepth (empty if
%% no items at that level), plus any partial depths beyond AchievedDepth
%% that were collected during indexing.
write_block_item_ids(Height, AchievedDepth, ItemIDs, Opts) ->
    Store = get_index_store(Opts),
    MaxStoredDepth = case maps:keys(ItemIDs) of
        [] -> AchievedDepth;
        Keys -> max(AchievedDepth, lists:max(Keys))
    end,
    Results = lists:map(
        fun(D) ->
            IDs = maps:get(D, ItemIDs, []),
            Bin = encode_item_ids(IDs),
            hb_store:write(
                Store,
                #{block_items_path(Height, D) => Bin},
                Opts
            )
        end,
        lists:seq(1, MaxStoredDepth)
    ),
    case lists:all(fun(R) -> R =:= ok end, Results) of
        true -> ok;
        false ->
            ?event(copycat_short,
                {block_item_ids_write_failed,
                    {height, Height}}),
            {error, item_ids_write_failed}
    end.

%% @doc Encode a list of 32-byte raw IDs into a single binary.
encode_item_ids(IDs) ->
    << <<ID:32/binary>> || ID <- IDs >>.

%% @doc Decode a binary of concatenated 32-byte IDs into a list.
%% Rejects binaries whose size is not a multiple of 32.
decode_item_ids(<<>>) -> [];
decode_item_ids(Bin) when byte_size(Bin) rem 32 =/= 0 ->
    {error, invalid_item_ids_binary};
decode_item_ids(Bin) ->
    decode_item_ids_acc(Bin, []).

decode_item_ids_acc(<<>>, Acc) -> lists:reverse(Acc);
decode_item_ids_acc(<<ID:32/binary, Rest/binary>>, Acc) ->
    decode_item_ids_acc(Rest, [ID | Acc]).

%% @doc Read the stored marker depth for a block, or undefined if none.
read_block_marker_depth(Height, Opts) ->
    case store_from_opts(Opts) of
        no_store -> undefined;
        #{ <<"index-store">> := Store } ->
            case hb_store:read(Store, block_indexed_path(Height), Opts) of
                {ok, Bin} ->
                    try binary_to_integer(Bin)
                    catch _:_ -> undefined
                    end;
                {error, not_found} -> undefined
            end
    end.

%% @doc Check if a block has been indexed at the given depth or deeper.
is_block_indexed(undefined, _TargetDepth, _Opts) ->
    false;
is_block_indexed(Height, TargetDepth, Opts) ->
    case read_block_marker_depth(Height, Opts) of
        undefined -> false;
        StoredDepth -> StoredDepth >= TargetDepth
    end.

%% @doc Write a block completion marker with the achieved depth.
mark_block_indexed(Height, Depth, Opts) ->
    Store = get_index_store(Opts),
    hb_store:write(
        Store,
        #{block_indexed_path(Height) => integer_to_binary(Depth)},
        Opts
    ).

%% @doc Check if a transaction ID is indexed in the arweave index store.
is_tx_indexed(TXID, Opts) ->
    Store = get_index_store(Opts),
    case hb_store:read(Store, hb_store_arweave_offset:path(TXID), Opts) of
        {ok, _} -> true;
        {error, not_found} -> false
    end.

get_index_store(Opts) ->
    case store_from_opts(Opts) of
        #{ <<"index-store">> := Store } -> Store;
        _ -> throw(no_index_store_available)
    end.

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

setup_test_store() ->
    IndexStore = [hb_test_utils:test_store()],
    ArweaveStore = 
        #{
            <<"store-module">> => hb_store_arweave,
            <<"index-store">> => IndexStore
         },
    Opts = #{<<"store">> => [ArweaveStore]},
    {IndexStore, ArweaveStore, Opts}.

write_read_tx_test() ->
    {_, ArweaveStoreOpts, Opts} = setup_test_store(),
    ID = <<"bndIwac23-s0K11TLC1N7z472sLGAkiOdhds87ZywoE">>,
    EndOffset = 363524457284025,
    Size = 8387,
    StartOffset = EndOffset - Size,
    ok = write_offset(ArweaveStoreOpts, ID, <<"tx@1.0">>, StartOffset, Size, Opts),
    {ok, Bundle} = read(ArweaveStoreOpts, #{ <<"read">> => ID }, Opts),
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

%% @doc Stale ANS-104 offset: fake ID pointing to a known bundle TX's
%% data range. The deserialized item's ID won't match the fake ID.
stale_ans104_offset_returns_error_test() ->
    {_, ArweaveStoreOpts, Opts} = setup_test_store(),
    FakeID = <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,
    RealEndOffset = 363524457284025,
    RealSize = 8387,
    RealStartOffset = RealEndOffset - RealSize,
    ok = write_offset(ArweaveStoreOpts, FakeID, <<"ans104@1.0">>, RealStartOffset, RealSize, Opts),
    Result = read(ArweaveStoreOpts, #{ <<"read">> => FakeID }, Opts),
    ?assertMatch({error, {id_mismatch, _, _}}, Result).

%% @doc The L1 TX has bundle tags, but data is not a valid bundle.
write_read_fake_bundle_tx_test() ->
    {_, ArweaveStoreOpts, Opts} = setup_test_store(),
    ID = <<"cGNURX2IUt98VKVIeXSfYe6eulNwPEqijaQfvatzd_o">>,
    Size = 2,
    StartOffset = 155309918167286,
    ok = write_offset(ArweaveStoreOpts, ID, <<"tx@1.0">>, StartOffset, Size, Opts),
    {ok, TX} = read(ArweaveStoreOpts, #{ <<"read">> => ID }, Opts),
    ?assert(hb_message:verify(TX, all, #{})),
    ok.

%% @doc Interior Arweave offset returns bytes that are not a valid ANS-104 item,
%% so ar_bundles:deserialize/1 throws. The catch in load_item/4 must convert
%% that throw into {error, _} rather than crashing.
load_item_deserialize_throws_test() ->
    {_, ArweaveStoreOpts, Opts} = setup_test_store(),
    FakeID = <<"BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB">>,
    %% Same interior offset used in dev_arweave bundle_header_garbage_guard test:
    %% the bytes at ProbeOffset are mid-TX application data, not an ANS-104 header.
    ProbeOffset = 376836336327208,
    Size = 4096,
    ok = write_offset(ArweaveStoreOpts, FakeID, <<"ans104@1.0">>, ProbeOffset - 1, Size, Opts),
    ?assertMatch({error, _}, read(ArweaveStoreOpts, #{ <<"read">> => FakeID }, Opts)).

root_offset_confirmed_parent_test() ->
    {_, ArweaveStoreOpts, Opts} = setup_test_store(),
    ParentID = <<"bndIwac23-s0K11TLC1N7z472sLGAkiOdhds87ZywoE">>,
    ok = write_offset(ArweaveStoreOpts, ParentID, <<"tx@1.0">>, 12345, 99, Opts),
    ?assertEqual(
        12352,
        root_offset(
            #{ <<"relative">> => ParentID, <<"offset">> => 7 },
            ArweaveStoreOpts
        )
    ).

corrupt_item_ids_read_test() ->
    {IndexStore, _StoreOpts, Opts} = setup_test_store(),
    Height = 99999999,
    ok = hb_store:write(IndexStore, #{block_indexed_path(Height) => <<"2">>}, Opts),
    ok = hb_store:write(IndexStore, #{block_items_path(Height, 1) => <<0:256>>}, Opts),
    ok = hb_store:write(IndexStore, #{block_items_path(Height, 2) => <<0:240>>}, Opts),
    Counts = read_block_item_counts(Height, Opts),
    erlang:display({counts, Counts}),
    ?assertEqual(1, maps:get(<<"1">>, Counts)),
    ?assertEqual(<<"corrupt">>, maps:get(<<"2">>, Counts)),
    IDs = read_block_item_ids(Height, Opts),
    ?assertEqual(1, length(maps:get(<<"1">>, IDs))),
    ?assertEqual(<<"corrupt">>, maps:get(<<"2">>, IDs)),
    ok.

parent_encode_decode_test() ->
    BlockEntry = encode_parent_entry(12345, block),
    ?assertEqual(<<0, 12345:64/big-unsigned>>, BlockEntry),
    BundleID = crypto:strong_rand_bytes(32),
    BundleEntry = encode_parent_entry(BundleID, bundle),
    ?assertEqual(<<1, BundleID:32/binary>>, BundleEntry),
    Combined = <<BlockEntry/binary, BundleEntry/binary>>,
    Decoded = decode_parent_entries(Combined),
    ?assertEqual([{12345, block}, {BundleID, bundle}], Decoded),
    ok.

parent_not_found_test() ->
    {_IndexStore, ArweaveStoreOpts, Opts} = setup_test_store(),
    UnknownID = crypto:strong_rand_bytes(32),
    ?assertEqual(
       not_found, 
       hb_store_arweave:read_parent(ArweaveStoreOpts, UnknownID, Opts),
       Opts
    ),
    ok.

decode_item_ids_validation_test() ->
    ?assertEqual([], decode_item_ids(<<>>)),
    GoodBin = <<0:256, 1:256>>,
    ?assertEqual(2, length(decode_item_ids(GoodBin))),
    BadBin = <<0:240>>,
    ?assertEqual({error, invalid_item_ids_binary}, decode_item_ids(BadBin)),
    ok.
