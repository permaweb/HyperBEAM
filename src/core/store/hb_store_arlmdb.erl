%%% @doc A read-only store that serves keys from a published Arweave index:
%%% a transaction whose data is an LMDB 1.0 container of fixed-width rows, and
%%% whose tags make the index self-describing. The store fetches the container
%%% in place with byte-range requests -- it never downloads the file.
%%%
%%% The root transaction's tags parameterize every lookup:
%%% ```
%%%     device:            `lmdb@1.0', the container format.
%%%     prefix:            Keys that the index serves. All other keys are
%%%                        immediate misses, such that the store manager falls
%%%                        through to the next store in the list.
%%%     normalize-key:     A relative AO-Core path, resolved with the key's
%%%                        remainder (after the prefix) as the request body.
%%%                        The result is the row bits to seek to.
%%%     normalize-result:  A relative AO-Core path, resolved with the matching
%%%                        row as the request body. The result is the message
%%%                        that the read returns.
%%% '''
%%% The container is an LMDB 1.0 file (`MDB_DATA_VERSION' 3, 64 KiB pages,
%%% little-endian). Each page opens with a 24 byte header: the page number and
%%% the LMDB internal transaction as 64 bit ints, then `pad', `flags',
%%% `lower', and `upper' as 16 bit integers. The meta pages are pages 0 and 1; the one with
%%% the higher transaction ID wins. The main database must be `MDB_DUPSORT bor
%%% MDB_DUPFIXED' with a single-leaf root holding one `F_SUBDATA' node keyed
%%% `<<0>>', whose data is the sub-database record: its `pad' is the row width
%%% in bytes. The sub-database's branch pages hold full rows as node keys, and
%%% its leaves are `P_LEAF2': no nodes and no slot array -- row `I' is the
%%% `pad' bytes at page offset `24 + I * pad', with `lower bsr 1' rows in the
%%% page, ascending strictly.
%%%
%%% Fetched chunks are retained in the stores named by the `chunk-store' key
%%% of the store message, defaulting to a volatile store that expires every
%%% five minutes; `[]' retains nothing. Published containers are
%%% defragmented -- the first chunk carries the meta pages, main root and
%%% sub-root, branch pages cluster four to a chunk, and leaves follow in key
%%% order -- so the chunks a read retains answer most of the reads that
%%% follow it.
%%%
%%% Every container shape outside this contract is refused with a distinct
%%% error, never treated as a miss, and a failed byte-range fetch surfaces as
%%% `{error, {unavailable, ...}}': a proven miss requires a successfully read
%%% leaf.
-module(hb_store_arlmdb).
-export([start/3, stop/3, scope/0, scope/1]).
-export([read/3, resolve/3, type/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The byte-level constants of the LMDB 1.0 container.
-define(PAGE_SIZE, 65536).
%% An Arweave chunk holds four pages, and a published container's data begins
%% on a chunk boundary, so every page fetch lies within one chunk of the
%% container.
-define(CHUNK_SIZE, (4 * ?PAGE_SIZE)).
%% The default chunk store expires its whole table on this cadence.
-define(CHUNK_TTL_MS, 300000).
-define(PAGE_HDR, 24).
-define(MDB_MAGIC, 16#BEEFC0DE).
-define(MDB_VERSION, 3).
-define(MAIN_DB_FLAGS, 16#14). % MDB_DUPSORT bor MDB_DUPFIXED
-define(P_BRANCH, 16#01).
-define(P_LEAF, 16#02).
-define(P_LEAF2, 16#20).
-define(F_SUBDATA, 16#02).

%% @doc Resolve the root transaction's weave location and tags, validate the
%% container's meta page, and return the store instance. The instance holds
%% only the location and tags; fetched chunks are retained by the chunk
%% store alone.
start(StoreOpts = #{ <<"root">> := Root }, _Req, _Opts) ->
    maybe
        {ok, Start, Size} ?= read_location(Root, StoreOpts),
        {ok, _Meta} ?= read_meta(Start, Size, StoreOpts),
        {ok, Tags} ?= read_tags(Root, StoreOpts),
        ?event(store_arlmdb,
            {started, {root, Root}, {start, Start}, {size, Size}}
        ),
        {ok, #{ <<"start">> => Start, <<"size">> => Size, <<"tags">> => Tags }}
    end.

%% @doc The instance holds no processes: nothing to stop.
stop(_StoreOpts, _Req, _NodeOpts) -> ok.

%% @doc Reads are served by remote byte-range fetches.
scope() -> remote.
scope(#{ <<"scope">> := Scope }) -> Scope;
scope(_) -> scope().

%% @doc Index keys carry no links: resolution is the identity.
resolve(_StoreOpts, #{ <<"resolve">> := Key }, _NodeOpts) -> {ok, Key}.

%% @doc A key is `simple' when the index holds a row for it.
type(StoreOpts, #{ <<"type">> := Key }, NodeOpts) ->
    case read(StoreOpts, #{ <<"read">> => Key }, NodeOpts) of
        {ok, _} -> {ok, simple};
        Error -> Error
    end.

%% @doc Serve a read for a key under the index's prefix. All other keys are
%% immediate misses, so that the store manager falls through cleanly.
read(StoreOpts, #{ <<"read">> := Key }, _NodeOpts) ->
    #{ <<"start">> := Start, <<"size">> := Size, <<"tags">> := Tags } =
        hb_store:find(StoreOpts),
    Prefix = maps:get(<<"prefix">>, Tags),
    PrefixSize = byte_size(Prefix),
    case Key of
        <<Prefix:PrefixSize/binary, Suffix/binary>> ->
            lookup(Suffix, Start, Size, Tags, StoreOpts);
        _ ->
            {error, not_found}
    end.

%% @doc Look up one key: normalize its remainder to the seek bits, descend to
%% the first row at-or-after them, and normalize the row to the result.
lookup(Suffix, Start, Size, Tags, Opts) ->
    maybe
        {ok, Seek} ?=
            normalize(maps:get(<<"normalize-key">>, Tags), Suffix, Opts),
        true ?= is_bitstring(Seek) orelse {error, {'invalid-seek', Seek}},
        {ok, Meta} ?= read_meta(Start, Size, Opts),
        {ok, SubDB} ?= read_main_db(Meta, Opts),
        {ok, Row} ?= seek(Seek, SubDB, Meta, Opts),
        ?event(store_arlmdb, {row_found, {suffix, Suffix}, {row, Row}}),
        normalize(maps:get(<<"normalize-result">>, Tags), Row, Opts)
    end.

%% @doc Execute one of the index's tag paths as a relative AO-Core path, with
%% the given binary as the body of the base message alone.
normalize(Path, Body, Opts) ->
    hb_ao:resolve(#{ <<"path">> => Path, <<"0.body">> => Body }, Opts).

%% @doc Find the absolute weave offset and size of the root's data. The
%% gateway reports the offset of the final byte, 1-indexed.
read_location(Root, Opts) ->
    Res =
        hb_http:request(
            #{
                <<"method">> => <<"GET">>,
                <<"path">> => <<"/arweave/tx/", Root/binary, "/offset">>
            },
            Opts
        ),
    case Res of
        {ok, #{ <<"body">> := Body }} ->
            Info = hb_json:decode(Body),
            End = hb_util:int(maps:get(<<"offset">>, Info)),
            Size = hb_util:int(maps:get(<<"size">>, Info)),
            {ok, End - Size, Size};
        Error ->
            {error, {unavailable, {offset, Root, Error}}}
    end.

%% @doc Read and decode the root transaction's tags. Names and values arrive
%% base64url-encoded in the gateway's JSON form.
read_tags(Root, Opts) ->
    Res =
        hb_http:request(
            #{
                <<"method">> => <<"GET">>,
                <<"path">> => <<"/arweave/tx/", Root/binary>>
            },
            Opts
        ),
    case Res of
        {ok, #{ <<"body">> := Body }} ->
            required_tags(
                maps:from_list(
                    [
                        {hb_util:decode(Name), hb_util:decode(Value)}
                    ||
                        #{ <<"name">> := Name, <<"value">> := Value } <-
                            maps:get(<<"tags">>, hb_json:decode(Body), [])
                    ]
                )
            );
        Error ->
            {error, {unavailable, {tags, Root, Error}}}
    end.

%% @doc Require the tags that parameterize the index's lookups.
required_tags(
    Tags = #{
        <<"device">> := <<"lmdb@1.0">>,
        <<"prefix">> := _,
        <<"normalize-key">> := _,
        <<"normalize-result">> := _
    }
) ->
    {ok, Tags};
required_tags(#{ <<"device">> := Device }) when Device =/= <<"lmdb@1.0">> ->
    {error, {'unsupported-container-device', Device}};
required_tags(Tags) ->
    {error, {'missing-tags', maps:keys(Tags)}}.

%% @doc Read both meta pages and validate the container, returning the meta
%% written by the most recent transaction. The returned meta also carries the
%% container's weave location, parameterizing every page fetch beneath it.
read_meta(Start, Size, Opts) ->
    maybe
        true ?=
            Size >= 2 * ?PAGE_SIZE
                orelse {error, {'invalid-container-size', Size}},
        {ok, <<Page0:?PAGE_SIZE/binary, Page1:?PAGE_SIZE/binary>>} ?=
            fetch(Start, Size, 0, 2 * ?PAGE_SIZE, Opts),
        {ok, Meta0} ?= parse_meta(Page0),
        {ok, Meta1} ?= parse_meta(Page1),
        validate_meta(
            (newest_meta(Meta0, Meta1))#{ start => Start, size => Size }
        )
    end.

%% @doc Parse a meta page: the `MDB_meta' record follows the page header.
parse_meta(
    <<
        _:?PAGE_HDR/binary,
        ?MDB_MAGIC:32/little, ?MDB_VERSION:32/little,
        _Address:64/little, _MapSize:64/little,
        DB0:48/binary, DB1:48/binary,
        LastPage:64/little, TxnID:64/little,
        _/binary
    >>
) ->
    {ok, #{
        db0 => parse_db(DB0),
        db1 => parse_db(DB1),
        last_page => LastPage,
        txn => TxnID
    }};
parse_meta(
    <<_:?PAGE_HDR/binary, Magic:32/little, Version:32/little, _/binary>>
) ->
    {error, {'invalid-meta', {magic, Magic}, {version, Version}}}.

%% @doc Parse a 48 byte `MDB_db' record.
parse_db(
    <<
        Pad:32/little, Flags:16/little, Depth:16/little,
        _Branch:64/little, _Leaf:64/little, _Overflow:64/little,
        Entries:64/little, Root:64/little
    >>
) ->
    #{ pad => Pad, flags => Flags, depth => Depth,
       entries => Entries, root => Root }.

%% @doc Choose the meta page written by the most recent transaction.
newest_meta(Meta0 = #{ txn := Txn0 }, #{ txn := Txn1 }) when Txn0 >= Txn1 ->
    Meta0;
newest_meta(_Meta0, Meta1) ->
    Meta1.

%% @doc Enforce the container invariants that this store implements.
validate_meta(Meta = #{ db0 := DB0, db1 := DB1 }) ->
    #{ pad := PageSize } = DB0,
    #{ flags := MainFlags } = DB1,
    #{ last_page := LastPage, size := Size } = Meta,
    maybe
        true ?=
            PageSize =:= ?PAGE_SIZE
                orelse {error, {'invalid-page-size', PageSize}},
        true ?=
            MainFlags =:= ?MAIN_DB_FLAGS
                orelse {error, {'invalid-main-flags', MainFlags}},
        true ?=
            (LastPage + 1) * ?PAGE_SIZE =< Size
                orelse {error, {'invalid-last-page', LastPage}},
        {ok, Meta}
    end.

%% @doc Read the main database's root: a single-node leaf whose `F_SUBDATA'
%% data is the record of the sub-database holding the index rows.
read_main_db(Meta = #{ db1 := #{ root := Root, depth := Depth } }, Opts) ->
    maybe
        true ?= Depth =:= 1 orelse {error, {'invalid-main-depth', Depth}},
        {ok, Page} ?= read_page(Root, Meta, Opts),
        {ok, 1} ?= main_leaf_nodes(Page),
        sub_db(Page, node(Page, 0))
    end.

%% @doc Require the main root to be an ordinary leaf with a single node.
main_leaf_nodes(Page) ->
    case parse_page(Page) of
        {?P_LEAF, 1} -> {ok, 1};
        {?P_LEAF, Count} -> {error, {'invalid-main-entries', Count}};
        {Flags, _} -> {error, {'invalid-main-page-flags', Flags}}
    end.

%% @doc Require the main node to be the `<<0>>'-keyed sub-database reference
%% and return the parsed sub-database record.
sub_db(Page, Node = #{ key := Key, flags := Flags }) ->
    maybe
        true ?= Key =:= <<0>> orelse {error, {'invalid-main-key', Key}},
        true ?=
            Flags band ?F_SUBDATA =/= 0
                orelse {error, {'invalid-main-node-flags', Flags}},
        {ok, DB} ?= sub_db_record(node_data(Page, Node)),
        {ok, parse_db(DB)}
    end.

%% @doc Require a full 48 byte sub-database record.
sub_db_record(<<DB:48/binary, _/binary>>) -> {ok, DB};
sub_db_record(Short) -> {error, {'invalid-sub-db', byte_size(Short)}}.

%% @doc Descend the sub-database to the first row at-or-after the seek bits
%% and require it to begin with them.
seek(Seek, #{ pad := Pad, root := Root, depth := Depth }, Meta, Opts) ->
    SeekSize = bit_size(Seek),
    RowBits = Pad * 8,
    maybe
        true ?=
            Pad > 0 andalso ?PAGE_HDR + Pad =< ?PAGE_SIZE
                orelse {error, {'invalid-row-width', Pad}},
        true ?=
            SeekSize =< RowBits
                orelse {error, {'invalid-seek-size', SeekSize}},
        Target = <<Seek/bitstring, 0:(RowBits - SeekSize)>>,
        {ok, Row} ?= descend(Root, Depth, Target, Pad, none, Meta, Opts),
        true ?= byte_size(Row) =:= Pad orelse {error, {'invalid-row', Row}},
        confirm(Row, Seek)
    end.

%% @doc Require the found row to begin with the seek bits: a row at-or-after
%% the target that diverges within them is a proven miss.
confirm(Row, Seek) ->
    SeekSize = bit_size(Seek),
    case Row of
        <<Lead:SeekSize/bitstring, _/bitstring>> when Lead =:= Seek ->
            {ok, Row};
        _ ->
            {error, not_found}
    end.

%% @doc Take one step of the descent. Branch pages recurse into the last child
%% whose key is at-or-under the target, with the first node standing for
%% negative infinity. The nearest next-node key seen on the way down is the
%% successor row for targets that fall after every row in the leaf reached.
descend(PgNo, Depth, Target, Pad, Next, Meta, Opts) ->
    maybe
        true ?= Depth > 0 orelse {error, {'invalid-depth', PgNo}},
        {ok, Page} ?= read_page(PgNo, Meta, Opts),
        step(parse_page(Page), Page, Depth, Target, Pad, Next, Meta, Opts)
    end.

%% @doc Recurse through a branch, search a fixed-width leaf, and refuse every
%% other page kind.
step({?P_BRANCH, Count}, Page, Depth, Target, Pad, Next, Meta, Opts) ->
    Slot = branch_slot(Page, 1, Count, Target, 0),
    NewNext =
        if Slot + 1 < Count -> maps:get(key, node(Page, Slot + 1));
        true -> Next
        end,
    descend(child(node(Page, Slot)), Depth - 1, Target, Pad, NewNext, Meta, Opts);
step({Flags, Count}, Page, _Depth, Target, Pad, Next, _Meta, _Opts)
        when Flags =:= (?P_LEAF bor ?P_LEAF2) ->
    maybe
        true ?=
            ?PAGE_HDR + (Count * Pad) =< ?PAGE_SIZE
                orelse {error, {'invalid-leaf-count', Count}},
        case leaf_slot(Page, 0, Count, Pad, Target) of
            Slot when Slot < Count -> {ok, row(Page, Slot, Pad)};
            _ when Next =:= none -> {error, not_found};
            _ -> {ok, Next}
        end
    end;
step({Flags, _Count}, _Page, _Depth, _Target, _Pad, _Next, _Meta, _Opts) ->
    {error, {'invalid-page-flags', Flags}}.

%% @doc Find the last branch slot whose key is at-or-under the target. Keys
%% ascend, so the scan stops at the first key over it.
branch_slot(_Page, Slot, Count, _Target, Best) when Slot >= Count -> Best;
branch_slot(Page, Slot, Count, Target, Best) ->
    case maps:get(key, node(Page, Slot)) =< Target of
        true -> branch_slot(Page, Slot + 1, Count, Target, Slot);
        false -> Best
    end.

%% @doc Binary-search a fixed-width leaf for the first row at-or-after the
%% target, returning its index (or the row count if every row is under it).
leaf_slot(_Page, Low, High, _Pad, _Target) when Low >= High -> Low;
leaf_slot(Page, Low, High, Pad, Target) ->
    Mid = (Low + High) div 2,
    case row(Page, Mid, Pad) < Target of
        true -> leaf_slot(Page, Mid + 1, High, Pad, Target);
        false -> leaf_slot(Page, Low, Mid, Pad, Target)
    end.

%% @doc The row at the given index of a fixed-width leaf.
row(Page, Slot, Pad) ->
    Offset = ?PAGE_HDR + (Slot * Pad),
    <<_:Offset/binary, Row:Pad/binary, _/binary>> = Page,
    Row.

%% @doc Split a node page's header into its flags and slot count.
parse_page(
    <<
        _PgNo:64/little, _Txn:64/little, _Pad:16/little,
        Flags:16/little, Lower:16/little, _Upper:16/little,
        _/binary
    >>
) ->
    {Flags, Lower bsr 1}.

%% @doc Read the node at the given slot. Slot offsets are relative to the end
%% of the page header.
node(Page, Slot) ->
    SlotOffset = ?PAGE_HDR + (Slot * 2),
    <<_:SlotOffset/binary, NodeOffset:16/little, _/binary>> = Page,
    Offset = ?PAGE_HDR + NodeOffset,
    <<
        _:Offset/binary,
        Lo:16/little, Hi:16/little, Flags:16/little, KSize:16/little,
        Key:KSize/binary,
        _/binary
    >> = Page,
    #{ lo => Lo, hi => Hi, flags => Flags, ksize => KSize,
       key => Key, offset => Offset }.

%% @doc A leaf node's data: after the key, rounded up to even length.
node_data(Page, #{ offset := Offset, ksize := KSize }) ->
    DataOffset = Offset + 8 + KSize + (KSize band 1),
    <<_:DataOffset/binary, Data/binary>> = Page,
    Data.

%% @doc A branch node's child page number.
child(#{ lo := Lo, hi := Hi, flags := Flags }) ->
    Lo bor (Hi bsl 16) bor (Flags bsl 32).

%% @doc Fetch a page by number, refusing page numbers outside the container.
read_page(PgNo, #{ last_page := LastPage, start := Start, size := Size }, Opts) ->
    maybe
        true ?=
            PgNo =< LastPage andalso (PgNo + 1) * ?PAGE_SIZE =< Size
                orelse {error, {'invalid-page-number', PgNo}},
        fetch(Start, Size, PgNo * ?PAGE_SIZE, ?PAGE_SIZE, Opts)
    end.

%% @doc Fetch a byte range of the container, sliced from the chunk that holds
%% it. Pages align within the container's chunks, so a range never spans two.
%% Failed fetches are unavailability, never misses.
fetch(Start, Size, Offset, Length, Opts) ->
    Chunk = Offset div ?CHUNK_SIZE,
    Within = Offset - (Chunk * ?CHUNK_SIZE),
    maybe
        true ?=
            Within + Length =< ?CHUNK_SIZE
                orelse {error, {'invalid-fetch-span', Offset, Length}},
        {ok, Bytes} ?= read_chunk(Start, Size, Chunk, Opts),
        true ?=
            Within + Length =< byte_size(Bytes)
                orelse {error, {unavailable, {short_read, byte_size(Bytes)}}},
        {ok, binary:part(Bytes, Within, Length)}
    end.

%% @doc One whole chunk of the container, from the chunk store when it is
%% held, and from the weave -- retained for the next read -- when it is not.
%% The defragmented layout clusters the tree: the first chunk carries the
%% meta pages, main root and sub-root, and a chunk holding one branch page
%% holds its neighbours, so held chunks answer most of every descent.
read_chunk(Start, Size, Chunk, Opts) ->
    Stores = chunk_store(Opts),
    Key =
        <<
            (hb_util:bin(Start))/binary, "/chunk=", (hb_util:bin(Chunk))/binary
        >>,
    case hb_store:read(Stores, Key, Opts) of
        {ok, Bytes} -> {ok, Bytes};
        _ -> fill_chunk(Stores, Key, Start, Size, Chunk, Opts)
    end.

%% @doc Fetch a chunk from the weave and retain it. Retention is best-effort:
%% a store that refuses the write costs the next read a fetch, nothing more.
fill_chunk(Stores, Key, Start, Size, Chunk, Opts) ->
    ChunkStart = Chunk * ?CHUNK_SIZE,
    Length = min(?CHUNK_SIZE, Size - ChunkStart),
    case hb_store_arweave:read_chunks(Start + ChunkStart, Length, Opts) of
        {ok, Bytes} when byte_size(Bytes) =:= Length ->
            case hb_store:write(Stores, #{ Key => Bytes }, Opts) of
                ok -> ok;
                Refused -> ?event(store_arlmdb, {chunk_not_retained, Refused})
            end,
            {ok, Bytes};
        {ok, Bytes} -> {error, {unavailable, {short_read, byte_size(Bytes)}}};
        Error -> {error, {unavailable, Error}}
    end.

%% @doc The stores retaining fetched chunks, from the `chunk-store' key of
%% the store message. The default is a volatile store named for the
%% container, expiring wholesale every five minutes; `[]' retains nothing.
chunk_store(#{ <<"chunk-store">> := Stores }) when is_list(Stores) -> Stores;
chunk_store(#{ <<"chunk-store">> := Store }) -> [Store];
chunk_store(StoreOpts = #{ <<"root">> := Root }) ->
    Name = maps:get(<<"name">>, StoreOpts, Root),
    [
        #{
            <<"store-module">> => hb_store_volatile,
            <<"name">> => <<Name/binary, "-chunks">>,
            <<"max-ttl-ms">> => ?CHUNK_TTL_MS
        }
    ].

%%% Tests

%% The live offset index: maps `~arweave@2.9/offset=<id>' keys to the weave
%% locations of ANS-104 data items.
-define(LIVE_INDEX, <<"7vg2832WFsisEcBr1oBQ8ldc4EGOkjQdwW46hDvJsOs">>).
%% A mined LMDB container that predates the published-index container format.
-define(OLD_CONTAINER, <<"b159UDeD87YEFujWBMM8bISZ8DL8Wm1jLa-Bs_LQGAw">>).

%% @doc A store message for the live index. The instance registry is keyed by
%% name, so the root doubles as the name and instances persist across tests.
test_store() ->
    #{
        <<"store-module">> => hb_store_arlmdb,
        <<"name">> => ?LIVE_INDEX,
        <<"root">> => ?LIVE_INDEX
    }.

%% @doc The live index resolves indexed data item IDs to their weave
%% locations through the full store API.
read_indexed_offset_test() ->
    Store = test_store(),
    ok = hb_store:start([Store]),
    ?assertMatch(
        {ok, #{ <<"start">> := 381852134215637, <<"length">> := 3947 }},
        hb_store:read(
            [Store],
            <<"~arweave@2.9/offset=AAAAhyV8_NwududSxuraAj7DLWiZHDTqVKWrZglpNok">>,
            #{}
        )
    ),
    ?assertMatch(
        {ok, #{ <<"start">> := 381680833668862, <<"length">> := 1356 }},
        hb_store:read(
            [Store],
            <<"~arweave@2.9/offset=1QAAJqd60JFNvY3lBfIS5CFPjXteQSHMTp8cuvBJuHA">>,
            #{}
        )
    ).

%% @doc The index holds data items alone: an L1 transaction ID is a proven
%% miss, found by reading the leaf where its row would sit.
unindexed_key_test() ->
    Store = test_store(),
    ok = hb_store:start([Store]),
    ?assertEqual(
        {error, not_found},
        hb_store:read(
            [Store],
            <<"~arweave@2.9/offset=", (?OLD_CONTAINER)/binary>>,
            #{}
        )
    ).

%% @doc Keys outside the index's prefix are served by the other stores in the
%% list, and index keys fall through the stores ahead of it.
fallthrough_test() ->
    Local = hb_test_utils:test_store(hb_store_fs),
    Stores = [Local, test_store()],
    ok = hb_store:start(Stores),
    ok = hb_store:write(Stores, #{ <<"local-key">> => <<"local-value">> }, #{}),
    ?assertEqual(
        {ok, <<"local-value">>},
        hb_store:read(Stores, <<"local-key">>, #{})
    ),
    ?assertMatch(
        {ok, #{ <<"start">> := 381852134215637, <<"length">> := 3947 }},
        hb_store:read(
            Stores,
            <<"~arweave@2.9/offset=AAAAhyV8_NwududSxuraAj7DLWiZHDTqVKWrZglpNok">>,
            #{}
        )
    ).

%% @doc `hb_store_arweave' finds the offsets of items that its key-value index
%% lacks from a published index in the same `index-store' list.
read_offset_fallback_test() ->
    ArweaveStore =
        #{
            <<"store-module">> => hb_store_arweave,
            <<"index-store">> => [test_store()]
        },
    ok = hb_store:start([ArweaveStore]),
    ?assertMatch(
        {ok, #{
            <<"codec-device">> := <<"ans104@1.0">>,
            <<"version">> := 2,
            <<"start">> := 381852134215637,
            <<"length">> := 3947
        }},
        hb_store_arweave:read_offset(
            ArweaveStore,
            <<"AAAAhyV8_NwududSxuraAj7DLWiZHDTqVKWrZglpNok">>,
            #{}
        )
    ).

%% @doc A mined LMDB file that is not in the container format this store
%% implements is refused loudly at start, never silently skipped.
invalid_container_test() ->
    Store =
        #{
            <<"store-module">> => hb_store_arlmdb,
            <<"name">> => ?OLD_CONTAINER,
            <<"root">> => ?OLD_CONTAINER
        },
    ?assertMatch(
        {error, {'invalid-main-flags', 0}},
        hb_store:start([Store])
    ).

%% @doc Retained chunks serve repeated reads without the weave: a key looked
%% up once answers again through a store whose routes are gone, while an
%% indexed key whose leaf chunk was never fetched cannot. The second key's
%% unavailability reaches the store manager, which reports an exhausted
%% store list as its terminal miss.
chunk_retention_test_() ->
    {timeout, 120, fun chunk_retention/0}.
chunk_retention() ->
    Store = (test_store())#{ <<"name">> => <<?LIVE_INDEX/binary, "-cached">> },
    Key =
        <<"~arweave@2.9/offset="
            "AAAAhyV8_NwududSxuraAj7DLWiZHDTqVKWrZglpNok">>,
    {ok, First} = hb_store:read([Store], Key, #{}),
    Unrouted = Store#{ <<"routes">> => [] },
    ?assertEqual({ok, First}, hb_store:read([Unrouted], Key, #{})),
    Fresh =
        <<"~arweave@2.9/offset="
            "KgADUJYkEY0dbUKTI3aDZy2c_nb4WLh7VDh2ZHrb1yY">>,
    ?assertMatch({error, _}, hb_store:read([Unrouted], Fresh, #{})),
    ?assertMatch(
        {ok, #{ <<"start">> := 381838173656091 }},
        hb_store:read([Store], Fresh, #{})
    ).
