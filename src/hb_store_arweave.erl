%%% @doc A store implementation that relays to an Arweave node, using an 
%%% intermediate cache of offsets as an ID->ArweaveLocation mapping.
-module(hb_store_arweave).
%%% Store API:
-export([scope/0, scope/1, type/2, read/2]).
%%% Indexing API:
-export([write_offset/5, read_offset/2, read_chunks/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Although the index is local, loading an item via the index will make
%% requests to a remote node, so we define the scope as remote.
scope() -> remote.
scope(#{ <<"scope">> := Scope }) -> Scope;
scope(_) -> scope().

%% @doc Get the type of the data at the given key. We potentially cache the
%% result, so that we don't have to read the data from the GraphQL route
%% multiple times.
type(#{ <<"index-store">> := IndexStore }, ID) when ?IS_ID(ID) ->
    Type =
        case hb_store:read(IndexStore, hb_store_arweave_offset:path(ID)) of
            {ok, _Offset} -> simple;
            _ -> not_found
        end,
    ?event(store_arweave_debug,
        {type, {id, {explicit, ID}}, {type, Type}}),
    Type;
type(_, _) -> not_found.

%% @doc Read the offset of the data at the given key.
read_offset(#{ <<"index-store">> := IndexStore }, ID) when ?IS_ID(ID) ->
    case hb_store:read(IndexStore, hb_store_arweave_offset:path(ID)) of
        {ok, OffsetBinary} ->
            {Version, CodecName, StartOffset, Length} =
                hb_store_arweave_offset:decode(OffsetBinary),
            {ok, #{
                <<"version">> => Version,
                <<"codec-device">> => CodecName,
                <<"start-offset">> => StartOffset,
                <<"length">> => Length
            }};
        _ -> not_found
    end;
read_offset(_, _) -> not_found.

read(StoreOpts, ID) ->
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
            ),
            {error, not_found}
    end.

load_item(StartOffset, Length, Opts) ->
    case read_chunks(StartOffset, Length, Opts) of
        {ok, SerializedItem} ->
            {
                ok,
                hb_message:convert(
                    ar_bundles:deserialize(SerializedItem),
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