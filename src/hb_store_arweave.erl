%%% @doc A store implementation that relays to an Arweave node, using an 
%%% intermediate cache of offsets as an ID->ArweaveLocation mapping.
-module(hb_store_arweave).
%%% Store API:
-export([read/2]).
%%% Indexing API:
-export([write_offset/4]).

-define(ARWEAVE_INDEX_PATH, <<"~arweave@2.9-pre/offset">>).

read(StoreOpts = #{ <<"arweave-index-store">> := IndexStore }, ID) ->
    Path =
        <<
            ?ARWEAVE_INDEX_PATH/binary,
            "/",
            (hb_util:bin(ID))/binary
        >>,
    case hb_store:read(IndexStore, Path) of
        {ok, Binary} ->
            [Offset, Length] = binary:split(Binary, <<":">>, [global]),
            SerializedItem =
                hb_ao:get(
                    <<
                        "~arweave@2.9-pre/chunk&offset=",
                        Offset/binary,
                        "&length=",
                        Length/binary
                    >>,
                    StoreOpts
                ),
            {ok, ar_bundles:deserialize(SerializedItem)};
        {error, not_found} ->
            {error, not_found}
    end.

write_offset(Store, ID, Offset, Length) ->
    hb_store:write(
        Store,
        <<
            ?ARWEAVE_INDEX_PATH/binary, "/",
            (hb_util:bin(ID))/binary>>, <<Offset/binary, ":", Length/binary
        >>
    ).