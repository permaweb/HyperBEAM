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
            case IsTX of
                <<"1">> ->
                    load_bundle(ID, Offset, Length, StoreOpts);
                <<"0">> ->
                    load_item(Offset, Length, StoreOpts)
            end;
        {error, not_found} ->
            {error, not_found}
    end.

load_item(Offset, Length, Opts) ->
    case read_chunks(Offset, Length, Opts) of
        {ok, SerializedItem} ->
            ar_bundles:deserialize(SerializedItem);
        {error, Reason} ->
            {error, Reason}
    end.

load_bundle(ID, Offset, Length, Opts) ->

    case read_chunks(Offset, Length, Opts) of
        {ok, SerializedItem} ->
            ar_bundles:deserialize(SerializedItem);
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
    {ok, Item} = read(Opts, ID),
    ?event(debug_test, {item, Item}),
    ok.