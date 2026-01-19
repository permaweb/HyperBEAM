%%% @doc A store implementation that relays to an Arweave node, using an 
%%% intermediate cache of offsets as an ID->ArweaveLocation mapping.
-module(hb_store_arweave).
%%% Store API:
-export([read/2]).
%%% Indexing API:
-export([write_offset/5]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ARWEAVE_INDEX_PATH, <<"~arweave@2.9-pre/offset">>).

read(StoreOpts = #{ <<"arweave-index-store">> := IndexStore }, ID) ->
    case hb_store:read(IndexStore, path(ID)) of
        {ok, Binary} ->
            [IsTX, Offset, Length] = binary:split(Binary, <<":">>, [global]),
            ?event(
                debug_test,
                {reading_offset, {is_tx, IsTX}, {offset, Offset}, {length, Length}}
            ),
            case hb_util:bool(IsTX) of
                true ->
                    load_bundle(ID, Offset, Length, StoreOpts);
                false ->
                    load_item(Offset, Length, StoreOpts)
            end;
        {error, not_found} ->
            {error, not_found}
    end.

load_item(Offset, Length, Opts) ->
    case read_chunks(Offset, Length, Opts) of
        {ok, SerializedItem} ->
            to_message(ar_bundles:deserialize(SerializedItem), Opts);
        {error, Reason} ->
            {error, Reason}
    end.

load_bundle(ID, Offset, Length, Opts) ->
    {ok, StructuredTXHeader} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9-pre">> },
        #{ <<"path">> => <<"tx">>, <<"tx">> => ID, <<"exclude-data">> => true },
        Opts
    ),
    TXHeader = hb_message:convert(
        StructuredTXHeader,
        <<"tx@1.0">>,
        <<"structured@1.0">>,
        Opts),
    case read_chunks(Offset, Length, Opts) of
        {ok, SerializedItem} ->
            to_message(
                ar_bundles:deserialize(TXHeader#tx{ data = SerializedItem }),
                Opts);
        {error, Reason} ->
            {error, Reason}
    end.

read_chunks(Offset, Length, Opts) ->
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9-pre">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => Offset,
            <<"length">> => Length
        },
        Opts
    ).

to_message(TX, Opts) ->
    {
        ok,
        hb_message:convert(
            TX,
            <<"structured@1.0">>,
            <<"tx@1.0">>,
            Opts
        )
    }.

write_offset(Store, ID, IsTX, Offset, Length) ->
    IsTxInt = hb_util:bool_int(IsTX),
    hb_store:write(
        Store,
        path(ID), 
        <<
            (hb_util:bin(IsTxInt))/binary,
            ":",
            (hb_util:bin(Offset))/binary,
            ":",
            (hb_util:bin(Length))/binary
        >>
    ).

path(ID) ->
    <<
        ?ARWEAVE_INDEX_PATH/binary,
        "/",
        (hb_util:bin(ID))/binary
    >>.


%%% Tests

write_read_tx_test() ->
    Store = [hb_test_utils:test_store()],
    ?event(debug_test, {store, Store}),
    Opts = #{ 
        <<"arweave-index-store">> => Store 
    },
    ID = <<"bndIwac23-s0K11TLC1N7z472sLGAkiOdhds87ZywoE">>,
    Offset = 363524457275639,
    Length = 8387,
    ok = write_offset(Store, ID, true, Offset, Length),
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
