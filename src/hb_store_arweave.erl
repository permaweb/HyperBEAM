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

read(StoreOpts = #{ <<"index-store">> := IndexStore }, ID) ->
    case hb_store:read(IndexStore, path(ID)) of
        {ok, Binary} ->
            % EndOffset and Size is recorded (rather than StartOffset and
            % Length) to preserve consistency with the Arweave API's
            % `/tx/<hash>/offset` endpoint.
            [IsTX, EndOffset, Size] = binary:split(Binary, <<":">>, [global]),
            ?event(
                debug_test,
                {reading_offset, 
                    {path, path(ID)},
                    {is_tx, IsTX},
                    {end_offset, EndOffset},
                    {size, Size}
                }
            ),
            case hb_util:bool(IsTX) of
                true ->
                    load_bundle(ID,
                        hb_util:int(EndOffset), hb_util:int(Size), StoreOpts);
                false ->
                    load_item(
                        hb_util:int(EndOffset), hb_util:int(Size), StoreOpts)
            end;
        not_found ->
            {error, not_found}
    end.

load_item(EndOffset, Size, Opts) ->
    case read_chunks(EndOffset, Size, Opts) of
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

load_bundle(ID, EndOffset, Size, Opts) ->
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
    case read_chunks(EndOffset, Size, Opts) of
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

read_chunks(EndOffset, Size, Opts) ->
    StartOffset = EndOffset - Size + 1,
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9-pre">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => StartOffset,
            <<"length">> => Size
        },
        Opts
    ).


%% @doc When recording an item or bundle's offset we use its EndOffset and
%% Size in rather than StartOffset and Length - this is for consistency with
%% the Arweave API's `/tx/<hash>/offset` endpoint which returns a global
%% end offset and size.
write_offset(#{ <<"index-store">> := IndexStore }, ID, IsTX, EndOffset, Size) ->
    IsTxInt = hb_util:bool_int(IsTX),
    Value = <<
        (hb_util:bin(IsTxInt))/binary,
        ":",
        (hb_util:bin(EndOffset))/binary,
        ":",
        (hb_util:bin(Size))/binary
    >>,
    ?event(debug_test, {{path, path(ID)}, {value, Value}}),
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
    ?event(debug_test, {store, Store}),
    Opts = #{ 
        <<"index-store">> => Store 
    },
    ID = <<"bndIwac23-s0K11TLC1N7z472sLGAkiOdhds87ZywoE">>,
    EndOffset = 363524457284025,
    Size = 8387,
    ok = write_offset(Opts, ID, true, EndOffset, Size),
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