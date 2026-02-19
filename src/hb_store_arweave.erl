%%% @doc A store implementation that relays to an Arweave node, using an 
%%% intermediate cache of offsets as an ID->ArweaveLocation mapping.
-module(hb_store_arweave).
%%% Store API:
-export([scope/0, scope/1, type/2, read/2]).
-export([start/1, read_with_type/2, resolve/2]).
%%% Indexing API:
-export([write_offset/5]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Although the index is local, loading an item via the index will make
%% requests to a remote node, so we define the scope as remote.
scope() -> remote.
scope(#{ <<"scope">> := Scope }) -> Scope;
scope(_) -> scope().

resolve(_, Key) -> Key.

start(#{<<"index-store">> := IndexStore}) ->
    init_prometheus(),
    hb_store:start(IndexStore).

%% @doc Get the type of the data at the given key. We potentially cache the
%% result, so that we don't have to read the data from the GraphQL route
%% multiple times.
type(#{ <<"index-store">> := IndexStore }, ID) ->
    Type =
        case hb_store:read(IndexStore, hb_store_arweave_offset:path(ID)) of
            {ok, _Offset} -> simple;
            _ -> not_found
        end,
    ?event(store_arweave_debug,
        {type, {id, {explicit, ID}}, {type, Type}}),
    Type.

read(StoreOpts = #{ <<"index-store">> := IndexStore }, ID) ->
    {IndexDuration, IndexResponse} = timer:tc(
        fun () -> hb_store:read(IndexStore, hb_store_arweave_offset:path(ID)) end, 
        native
    ),
    record_index_check_metric(IndexDuration),
    case IndexResponse of
        {ok, OffsetBinary} ->
            {Version, CodecName, StartOffset, Length} =
                hb_store_arweave_offset:decode(OffsetBinary),
            Loaded =
                case CodecName of
                    <<"ans104@1.0">> ->
                        {LoadDuration, LoadedMsg} = timer:tc(fun () -> load_item(StartOffset, Length, StoreOpts) end, native),
                        record_chunk_fetch_metric(LoadDuration, load_bundle),
                        LoadedMsg;
                    <<"tx@1.0">> ->
                        {LoadDuration, LoadedMsg} = timer:tc(fun () -> load_tx(ID, StartOffset, Length, StoreOpts) end, native),
                        record_chunk_fetch_metric(LoadDuration, load_item),
                        LoadedMsg
                end,
            case Loaded of
                {ok, _Message} ->
                    ?event(
                        arweave_offsets,
                        {read_ok,
                            {id, {explicit, ID}},
                            {format_version, Version},
                            {type, CodecName},
                            {start_offset, StartOffset},
                            {length, Length}
                        }
                    );
                {error, Reason} ->
                    ?event(
                        arweave_offsets,
                        {read_error, 
                            {id, {explicit, ID}},
                            {format_version, Version},
                            {type, CodecName},
                            {start_offset, StartOffset},
                            {length, Length},
                            {reason, Reason}
                        }
                    )
            end,
            Loaded;
        not_found ->
            ?event(
                arweave_offsets,
                {miss, {id, {explicit, ID}}}
            )
    end.

record_index_check_metric(Duration) ->
    record_metric(hb_store_arweave_index_check_duration_seconds, [], Duration).

record_chunk_fetch_metric(Duration, Type) ->
    record_metric(hb_store_arweave_chunk_fetch_duration_seconds, [Type], Duration).

record_metric(Metric, Label, Duration) ->
    spawn(fun () -> 
        case application:get_application(prometheus) of
            undefined -> ok;
            _ ->
                prometheus_histogram:observe(Metric, Label, Duration)
        end
    end).

read_with_type(Opts, Key) when is_list(Key) ->
    read_with_type(Opts, hb_store:join(Key));
read_with_type(Opts, Key) ->
    ?event({read_with_type, {key, Key}}),
    case read(Opts, Key) of
        {ok, Value} -> {simple, Value};
        {error, not_found} -> not_found;
        {error, Error} ->
            ?event(store_error, {arweave_unexpected, {key, Key}, {error, Error}}),
            not_found;
        not_found -> not_found
    end.

load_item(StartOffset, Length, Opts) ->
    case read_chunks(StartOffset, Length, Opts) of
        {ok, SerializedItem} ->
            {
                ok,
                hb_message:convert(
                    ar_bundles:deserialize(SerializedItem, Opts),
                    <<"structured@1.0">>,
                    <<"ans104@1.0">>,
                    Opts
                )
            };
        {error, Reason} ->
            {error, Reason}
    end.

load_tx(ID, StartOffset, Length, Opts) ->
    {ok, StructuredTXHeader} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{ <<"path">> => <<"tx">>, <<"tx">> => ID, <<"exclude-data">> => true },
        Opts
    ),
    TXHeader = hb_message:convert(
        StructuredTXHeader,
        <<"tx@1.0">>,
        <<"structured@1.0">>,
        Opts),
    case read_chunks(StartOffset, Length, Opts) of
        {ok, SerializedItem} ->
            {
                ok,
                hb_message:convert(
                    TXHeader#tx{ data = SerializedItem },
                    <<"structured@1.0">>,
                    <<"tx@1.0">>,
                    Opts
                )
            };
        {error, Reason} ->
            {error, Reason}
    end.

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

write_offset(
        #{ <<"index-store">> := IndexStore },
        ID,
        CodecName,
        StartOffset,
        Length
    ) ->
    Value = hb_store_arweave_offset:encode(CodecName, StartOffset, Length),
    ?event(
        store_arweave_debug, 
        {writing_offset, 
            {id, {explicit, ID}},
            {type, CodecName},
            {start_offset, StartOffset},
            {length, Length},
            {value, {explicit, Value}}
        }
    ),
    hb_store:write(IndexStore, hb_store_arweave_offset:path(ID), Value).

init_prometheus() ->
    case application:get_application(prometheus) of
        undefined -> ok;
        _ ->
            try
                prometheus_histogram:declare([
                    {name, hb_store_arweave_index_check_duration_seconds},
                    {buckets, [0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1]},
                    {help, "How much it takes to check the index"}
                ]),
                prometheus_histogram:declare([
                    {name, hb_store_arweave_chunk_fetch_duration_seconds},
                    {buckets, [0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 30, 60]},
                    {labels, [type]},
                    {help, "How much it takes to check the index"}
                ]),

                ok
            catch
                error:mfa_already_exists -> ok;
                _:_ -> ok
            end
    end.

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
        <<"data">> => <<"{\"totalTickedRewardsDistributed\":0,\"distributedEpochIndexes\":[],\"newDemandFactors\":[],\"newEpochIndexes\":[],\"tickedRewardDistributions\":[],\"newPruneGatewaysResults\":[{\"delegateStakeReturned\":0,\"stakeSlashed\":0,\"gatewayStakeReturned\":0,\"delegateStakeWithdrawing\":0,\"prunedGateways\":[],\"slashedGateways\":[],\"gatewayStakeWithdrawing\":0}]}">>,
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
    {ok, TX} = read(Opts, ID),
    ?assert(hb_message:verify(TX, all, #{})),
    ok.