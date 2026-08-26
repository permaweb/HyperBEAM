%%% @doc The manifest of L1 transaction boundaries a scan consumes.
%%%
%%% The scanner needs, per L1 transaction in its range: the absolute padded
%%% weave offset its data begins at, the data's size, and -- when a source
%%% can say -- its txid and whether it is a bundle. Boundaries and sizes
%%% derive exactly from a local chunk index: every chunk row carries its end
%%% offset relative to its transaction, so the transaction's start is the
%%% difference and its size is the largest relative offset seen. Txids do
%%% not: an L1 txid is a hash over the signed transaction header, which is
%%% not in the weave data, so it can only be joined in from block metadata.
%%% A manifest without txids is complete except that top-level items get no
%%% `bundled-in' row; the scan itself never needs the network.
%%%
%%% The file is fixed-width and sorted by start offset:
%%%
%%% ```
%%% << "AIMF", 1, 0, 0, 0,
%%%    << Start:64, Size:64, Flags:8, TXID:32/binary >>* >>
%%% '''
%%%
%%% Flags: bit 0 -- txid present; bit 1 -- known bundle; bit 2 -- known not
%%% a bundle. An absent txid is 32 zero bytes; unknown bundlehood leaves the
%%% probe to the scanner's own structural check.
-module(lib_arweave_index_manifest).
-export([write/2, load/3, from_chunk_index/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The file's magic and version.
-define(MAGIC, << "AIMF", 1, 0, 0, 0 >>).

%%% The key prefix a chunk index files its rows under, per storage module.
-define(INDEX_PREFIX, <<"~arweave@2.9/storage">>).

%% @doc Write a manifest of transaction specs, sorting by start offset.
write(Path, Txs) ->
    Sorted =
        lists:sort(
            fun(A, B) ->
                maps:get(<<"start">>, A) =< maps:get(<<"start">>, B)
            end,
            Txs
        ),
    ok = filelib:ensure_path(filename:dirname(Path)),
    file:write_file(Path, [?MAGIC | [record(Tx) || Tx <- Sorted]]).

%% @doc The transactions of a manifest whose data intersects `[From, To)',
%% in ascending start order.
load(Path, From, To) ->
    maybe
        {ok, << Magic:8/binary, Records/binary >>} ?= file:read_file(Path),
        true ?= Magic == ?MAGIC orelse {error, <<"manifest-magic-invalid">>},
        {ok,
            [
                Spec
            ||
                << Start:64, Size:64, Flags:8, TXID:32/binary >> <= Records,
                Start + Size > From andalso Start < To,
                (Spec = spec(Start, Size, Flags, TXID)) /= invalid
            ]
        }
    else
        {ok, _Short} -> {error, <<"manifest-magic-invalid">>};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Derive boundary records from the chunk index a mining node keeps in
%% its store, for one storage module's rows, and write them as a manifest.
%% The index carries no txids, so none are recorded.
%%
%% Rows are read in bucket order, which is weave order: each names its
%% transaction's start (`AbsoluteEndOffset - RelativeOffset') and raises its
%% size high-water (`RelativeOffset'). The `TXRoot' runs the rows arrive in
%% are preserved as block groups for later metadata joins.
from_chunk_index(Path, StoreID, Opts) ->
    Store = hb_opts:get(<<"arweave-index-store">>, no_store, Opts),
    Prefix =
        <<
            (?INDEX_PREFIX)/binary,
            "/",
            (hb_util:bin(StoreID))/binary,
            "/chunks"
        >>,
    maybe
        {ok, Buckets} ?= hb_store:list(Store, Prefix, Opts),
        Txs = boundaries(lists:sort(Buckets), Prefix, Store, #{}, Opts),
        ok ?= write(Path, Txs),
        {ok, length(Txs)}
    end.

%%% Internal functions.

%% @doc One transaction's fixed-width record.
record(Spec) ->
    Start = maps:get(<<"start">>, Spec),
    Size = maps:get(<<"size">>, Spec),
    {IDFlag, TXID} =
        case maps:get(<<"id">>, Spec, undefined) of
            undefined -> {0, << 0:256 >>};
            ID -> {1, hb_util:native_id(ID)}
        end,
    BundleFlag =
        case maps:get(<<"bundle">>, Spec, undefined) of
            true -> 2;
            false -> 4;
            undefined -> 0
        end,
    << Start:64, Size:64, (IDFlag bor BundleFlag):8, TXID/binary >>.

%% @doc One record's spec map, or `invalid' for flag bits this version does
%% not write.
spec(Start, Size, Flags, TXID) when Flags =< 7 ->
    Base = #{ <<"start">> => Start, <<"size">> => Size },
    WithID =
        case Flags band 1 of
            0 -> Base;
            1 -> Base#{ <<"id">> => TXID }
        end,
    case Flags band 6 of
        0 -> WithID;
        2 -> WithID#{ <<"bundle">> => true };
        4 -> WithID#{ <<"bundle">> => false };
        _ -> invalid
    end;
spec(_Start, _Size, _Flags, _TXID) ->
    invalid.

%% @doc Fold the index's bucket groups into per-transaction boundaries. Each
%% bucket group holds the row of one chunk, keyed by absolute end offset.
boundaries([], _Prefix, _Store, Acc, _Opts) ->
    [
        #{ <<"start">> => Start, <<"size">> => Size }
    ||
        {Start, Size} <- lists:sort(maps:to_list(Acc))
    ];
boundaries([Bucket | Buckets], Prefix, Store, Acc, Opts) ->
    BucketPath = << Prefix/binary, "/", (hb_util:bin(Bucket))/binary >>,
    Acc2 =
        case hb_store:list(Store, BucketPath, Opts) of
            {ok, Ends} ->
                lists:foldl(
                    fun(End, Fold) ->
                        chunk_row(BucketPath, End, Store, Fold, Opts)
                    end,
                    Acc,
                    Ends
                );
            _ ->
                Acc
        end,
    boundaries(Buckets, Prefix, Store, Acc2, Opts).

%% @doc Raise one chunk row's transaction to the boundary map.
chunk_row(BucketPath, End, Store, Acc, Opts) ->
    Path = << BucketPath/binary, "/", (hb_util:bin(End))/binary >>,
    maybe
        {ok, Value} ?= hb_store:read(Store, Path, Opts),
        << 1:8, _ChunkSize:32, RelativeOffset:64, _/binary >> ?= Value,
        AbsoluteEnd = hb_util:int(End),
        Start = AbsoluteEnd - RelativeOffset,
        maps:update_with(
            Start,
            fun(Size) -> max(Size, RelativeOffset) end,
            RelativeOffset,
            Acc
        )
    else
        Other ->
            % A row the store lists but cannot produce, or one written by a
            % format this decoder does not know, contributes no boundary.
            ?event(warning,
                {chunk_index_row_skipped, {path, Path}, {result, Other}}),
            Acc
    end.

%%% Tests.

%% @doc Specs round-trip through the file, sorted and bounded by the load
%% range, with flags surviving.
round_trip_test() ->
    Path =
        filename:join(
            os:getenv("TMPDIR", "/tmp"),
            <<
                "hb-index-manifest-",
                (hb_util:encode(crypto:strong_rand_bytes(6)))/binary
            >>
        ),
    ID = crypto:hash(sha256, <<"an l1 txid">>),
    Txs =
        [
            #{ <<"start">> => 2097152, <<"size">> => 1000, <<"bundle">> => false },
            #{ <<"start">> => 0, <<"size">> => 262144, <<"id">> => ID,
                <<"bundle">> => true },
            #{ <<"start">> => 524288, <<"size">> => 300000 }
        ],
    ok = write(Path, Txs),
    ?assertEqual(
        {ok,
            [
                #{ <<"start">> => 0, <<"size">> => 262144, <<"id">> => ID,
                    <<"bundle">> => true },
                #{ <<"start">> => 524288, <<"size">> => 300000 }
            ]
        },
        load(Path, 0, 1000000)
    ),
    ?assertEqual(
        {ok, [#{ <<"start">> => 2097152, <<"size">> => 1000,
            <<"bundle">> => false }]},
        load(Path, 2097153, 2097154)
    ),
    ?assertEqual({ok, []}, load(Path, 3000000, 4000000)),
    ok = file:write_file(Path, <<"not a manifest">>),
    ?assertEqual({error, <<"manifest-magic-invalid">>}, load(Path, 0, 1)).
