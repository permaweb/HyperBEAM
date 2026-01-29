%%% @doc A store implementation that relays to an Arweave node, using an 
%%% intermediate cache of offsets as an ID->ArweaveLocation mapping.
-module(hb_store_arweave).
%%% Store API:
-export([type/2, read/2]).
%%% Indexing API:
-export([write_offset/5]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ARWEAVE_INDEX_PATH, <<"~arweave@2.9-pre/offset">>).

%% @doc Get the type of the data at the given key. We potentially cache the
%% result, so that we don't have to read the data from the GraphQL route
%% multiple times.
type(StoreOpts, Key) ->
    case read(StoreOpts, Key) of
        {error, not_found} -> not_found;
        {ok, _Data} ->
            % TODO:
            % - should this return composite for any index L1 bundles?
            % - if so, I guess we need to implement list/2?
            % - for now we don't index nested bundle children, but once we
            %   do we may nalso need to return composite for them.
            simple
    end.

read(StoreOpts = #{ <<"index-store">> := IndexStore }, ID) ->
    case hb_store:read(IndexStore, path(ID)) of
        {ok, Binary} ->
            [IsTX, StartOffset, Length] = binary:split(Binary, <<":">>, [global]),
            case hb_util:bool(IsTX) of
                true ->
                    load_bundle(ID,
                        hb_util:int(StartOffset), hb_util:int(Length), StoreOpts);
                false ->
                    load_item(
                        hb_util:int(StartOffset), hb_util:int(Length), StoreOpts)
            end;
        not_found ->
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

load_bundle(ID, StartOffset, Length, Opts) ->
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
        #{ <<"device">> => <<"arweave@2.9-pre">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => StartOffset + 1,
            <<"length">> => Length
        },
        Opts
    ).

write_offset(
        #{ <<"index-store">> := IndexStore }, ID, IsTX, StartOffset, Length) ->
    IsTxInt = hb_util:bool_int(IsTX),
    Value = <<
        (hb_util:bin(IsTxInt))/binary,
        ":",
        (hb_util:bin(StartOffset))/binary,
        ":",
        (hb_util:bin(Length))/binary
    >>,
    hb_store:write(IndexStore, path(ID), Value).

path(ID) ->
    <<
        ?ARWEAVE_INDEX_PATH/binary,
        "/",
        (hb_util:bin(ID))/binary
    >>.


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
    ok = write_offset(Opts, ID, true, StartOffset, Size),
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

%% XXX TODO: write/read for data item