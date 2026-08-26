%%% @doc A reader for the on-disk pages of an LMDB 1.0 database.
%%%
%%% The module is pure: it is given the bytes of a page and returns the
%%% structures that they encode. Callers supply those bytes -- from a file, or
%%% from a range of the Arweave weave -- and drive the descent themselves.
%%%
%%% The layout implemented here is that of LMDB 1.0.0, read from
%%% `libraries/liblmdb' of `openldap/openldap' at
%%% `bac0ccfc4fbe17867349357987557bfdf1b680df', for a 64-bit little-endian
%%% target built without `MDB_RPAGE_CACHE' or `MDB_VL32'. `MDB_DATA_VERSION' is
%%% 3, the page header is 24 bytes, and `PAGEBASE' equals `PAGEHDRSZ', so the
%%% offsets in a page's pointer array are relative to the end of its header.
%%% The `elmdb' dependency vendors LMDB 0.9, whose files carry
%%% `MDB_DATA_VERSION' 1 and a shorter page header; `meta/1' refuses them
%%% rather than misreading them.
%%%
%%% Two databases are readable. The plain, single-value kind that a HyperBEAM
%%% store writes, and the sorted-set container that a published index uses: a
%%% main database flagged `MDB_DUPSORT|MDB_DUPFIXED' whose entries each carry a
%%% set of fixed-width duplicates. A small set sits inside its leaf node as a
%%% `P_SUBP' sub-page; a large one is promoted to a sub-database -- the node
%%% then holds that database's `MDB_db' under `F_SUBDATA' -- whose leaves are
%%% `P_LEAF2': bare items packed after the header, with no node structure at
%%% all. `item/3' and `item_seek/3' read those, as `node/2' and `seek/2' read
%%% pages that carry nodes. Every other database flag changes the layout or the
%%% comparator in ways this module has no reader for, so `meta/1' refuses it.
-module(hb_lmdb_page).
-export([meta/1, page/1, search/2, seek/2, node/2, num_keys/1]).
-export([item/3, item_seek/3]).
%% The fixtures that the tests below read are also published to Arweave, where
%% `hb_store_arlmdb' reads the same bytes. Its tests take the contents to expect
%% from here, so that both readers are held to one description of them.
-export([fixture_entries/0, dup_fixture_items/0]).
-include_lib("eunit/include/eunit.hrl").

%% Sizes, in bytes, of the fixed-length structures of the format.
-define(PAGE_HEADER_SIZE, 24).      % PAGEHDRSZ, and PAGEBASE with it
-define(NODE_HEADER_SIZE, 8).       % NODESIZE
-define(OVERFLOW_REF_SIZE, 24).     % sizeof(MDB_ovpage)
-define(DB_SIZE, 48).               % sizeof(MDB_db)

%% Constants that identify a file this module can read.
-define(MAGIC, 16#BEEFC0DE).
-define(DATA_VERSION, 3).
-define(MIN_PAGE_SIZE, 256).
-define(MAX_PAGE_SIZE, 65536).

%% Page flags. Values outside `?PAGE_FLAG_MASK' are in-memory bookkeeping and
%% never reach a committed file.
-define(P_BRANCH, 16#01).
-define(P_LEAF, 16#02).
-define(P_OVERFLOW, 16#04).
-define(P_LEAF2, 16#20).
-define(P_SUBP, 16#40).
-define(PAGE_FLAG_MASK, 16#6F).

%% Node flags. `F_BIGDATA' replaces a leaf's value with a reference to the
%% overflow pages that hold it. `F_DUPDATA' replaces it with the key's
%% duplicate set, held as a sub-page inside the node -- or, once too large for
%% one, as a sub-database whose `MDB_db' the node carries under `F_SUBDATA'.
-define(F_BIGDATA, 16#01).
-define(F_SUBDATA, 16#02).
-define(F_DUPDATA, 16#04).

%% Database flags. `MDB_DUPSORT' and `MDB_DUPFIXED' together are the sorted-set
%% container, and are refused apart: either alone gives a leaf layout this
%% module has no reader for. `MDB_REVERSEKEY', `MDB_INTEGERKEY',
%% `MDB_INTEGERDUP' and `MDB_REVERSEDUP' alter the key comparator, and a
%% database using one cannot be searched with byte order.
-define(MDB_DUPSORT, 16#04).
-define(MDB_DUPFIXED, 16#10).
-define(DUPLICATE_DB_FLAGS, (?MDB_DUPSORT bor ?MDB_DUPFIXED)).
-define(UNSUPPORTED_DB_FLAGS, 16#6A).

%% @doc Parse a meta page, returning the parameters of the snapshot it names.
%% Pages 0 and 1 are both meta pages and alternate on commit, so a caller reads
%% both and takes the one with the higher `txnid': the page 0 of a freshly
%% written file describes an empty database. A `root' of `16#FFFFFFFFFFFFFFFF'
%% means the main database holds nothing.
meta(
        <<
            _Header:?PAGE_HEADER_SIZE/binary,
            Magic:32/little,
            Version:32/little,
            _Address:64/little,
            _MapSize:64/little,
            % mm_dbs[0], the free database. Its `pad' and `flags' fields hold
            % the page size and the environment's own persistent flags.
            PageSize:32/little,
            _EnvironmentFlags:16/little,
            _FreeDepth:16/little,
            _FreeStatistics:32/binary,
            _FreeRoot:64/little,
            % mm_dbs[1], the main database.
            MainDb:?DB_SIZE/binary,
            LastPage:64/little,
            TxnID:64/little,
            _/binary
        >>
    ) ->
    Main = #{ <<"flags">> := MainFlags } = db(MainDb),
    validate_meta(
        Magic,
        Version,
        PageSize,
        MainFlags,
        Main#{
            <<"page-size">> => PageSize,
            <<"last-page">> => LastPage,
            <<"txnid">> => TxnID
        }
    );
meta(Bin) when is_binary(Bin) ->
    {error, truncated_meta_page}.

%% @doc Parse an `MDB_db', the 48 bytes that describe one database: its flags,
%% its depth and entry count, and the page its tree is rooted at. The meta page
%% carries one for the main database, and a leaf node flagged `F_SUBDATA'
%% carries one for the sub-database holding its key's duplicates -- there,
%% `pad' is the width of the items on the sub-database's fixed-width pages.
db(<<
        Pad:32/little,
        Flags:16/little,
        Depth:16/little,
        _BranchPages:64/little,
        _LeafPages:64/little,
        _OverflowPages:64/little,
        Entries:64/little,
        Root:64/little
    >>) ->
    #{
        <<"pad">> => Pad,
        <<"flags">> => Flags,
        <<"depth">> => Depth,
        <<"entries">> => Entries,
        <<"root">> => Root
    }.

%% @doc Refuse every meta page that this module cannot read: one belonging to
%% another format entirely, one written by a version whose layout differs, one
%% claiming a page size LMDB could not have used, and one whose main database
%% is neither the plain single-value kind nor the sorted-set container.
validate_meta(Magic, _Version, _PageSize, _Flags, _Meta) when Magic =/= ?MAGIC ->
    {error, not_lmdb};
validate_meta(_Magic, Version, _PageSize, _Flags, _Meta)
        when Version =/= ?DATA_VERSION ->
    {error, {unsupported_data_version, Version}};
validate_meta(_Magic, _Version, PageSize, _Flags, _Meta) when
        PageSize < ?MIN_PAGE_SIZE;
        PageSize > ?MAX_PAGE_SIZE;
        PageSize band (PageSize - 1) =/= 0 ->
    {error, {invalid_page_size, PageSize}};
validate_meta(_Magic, _Version, _PageSize, Flags, _Meta)
        when Flags band ?UNSUPPORTED_DB_FLAGS =/= 0;
            Flags band ?DUPLICATE_DB_FLAGS == ?MDB_DUPSORT;
            Flags band ?DUPLICATE_DB_FLAGS == ?MDB_DUPFIXED ->
    {error, {unsupported_database_flags, Flags}};
validate_meta(_Magic, _Version, _PageSize, _Flags, Meta) ->
    {ok, Meta}.

%% @doc Parse a page header, returning the kind of page it is: a `branch' or
%% `leaf' page reports the number of nodes it holds as `keys', a `leaf2' page
%% reports its fixed-width items as `keys' and their width as `pad', and an
%% `overflow' page reports the number of pages its value spans as `pages'.
page(Page = <<
            _PageNumber:64/little,
            _TxnID:64/little,
            Pad:16/little,
            Flags:16/little,
            Bounds:4/binary,
            _/binary
        >>) ->
    classify(Flags, Pad, Bounds, byte_size(Page) - ?PAGE_HEADER_SIZE);
page(Bin) when is_binary(Bin) ->
    {error, truncated_page}.

%% The four bytes that follow the flags are the bounds of a node page's free
%% space, or the page count of an overflow page. `lower', `upper' and the
%% pointer array are all relative to the end of the header, so the bounds are
%% checked against the page size less that header.
classify(Flags, _Pad, _Bounds, _Limit)
        when Flags band (bnot ?PAGE_FLAG_MASK) =/= 0 ->
    {error, {invalid_page_flags, Flags}};
% A fixed-width page holds bare items with no node structure: `P_LEAF|P_LEAF2'
% on a full page of a duplicate sub-database, with `P_SUBP' added on the
% sub-page held inside a leaf node. The header's `pad' field is the width of
% every item, so a page whose items would not all fit inside it is refused.
classify(Flags, Pad, <<Lower:16/little, Upper:16/little>>, Limit)
        when Flags band ?P_LEAF2 =/= 0,
            Flags band (?P_BRANCH bor ?P_LEAF bor ?P_OVERFLOW) == ?P_LEAF ->
    case
        Pad >= 1 andalso Lower band 1 == 0 andalso Lower =< Upper
            andalso Upper =< Limit andalso (Lower bsr 1) * Pad =< Limit
    of
        true ->
            {ok, #{
                <<"type">> => leaf2,
                <<"keys">> => Lower bsr 1,
                <<"pad">> => Pad
            }};
        false ->
            {error, {invalid_fixed_width_bounds, Pad, Lower, Upper}}
    end;
classify(Flags, _Pad, _Bounds, _Limit)
        when Flags band (?P_LEAF2 bor ?P_SUBP) =/= 0 ->
    {error, {unsupported_page_layout, Flags}};
classify(Flags, _Pad, <<Pages:32/little>>, _Limit)
        when Flags band ?P_OVERFLOW =/= 0 ->
    {ok, #{ <<"type">> => overflow, <<"pages">> => Pages }};
classify(Flags, _Pad, <<Lower:16/little, Upper:16/little>>, Limit)
        when Flags band (?P_BRANCH bor ?P_LEAF) =/= 0 ->
    case Lower band 1 == 0 andalso Lower =< Upper andalso Upper =< Limit of
        true ->
            Type = case Flags band ?P_BRANCH of 0 -> leaf; _ -> branch end,
            {ok, #{ <<"type">> => Type, <<"keys">> => Lower bsr 1 }};
        false ->
            {error, {invalid_free_space_bounds, Lower, Upper}}
    end;
classify(Flags, _Pad, _Bounds, _Limit) ->
    {error, {unsupported_page_type, Flags}}.

%% @doc Return the number of nodes held by a branch or leaf page.
num_keys(Page) ->
    case node_page(Page) of
        {ok, _Type, Keys} -> {ok, Keys};
        {error, _} = Error -> Error
    end.

%% Parse a page that must carry nodes, refusing every other kind: a `leaf2'
%% page counts its items as `keys' but has no node structure to read them by.
node_page(Page) ->
    case page(Page) of
        {ok, #{ <<"type">> := Type, <<"keys">> := Keys }}
                when Type == branch; Type == leaf ->
            {ok, Type, Keys};
        {ok, #{ <<"type">> := Type }} ->
            {error, {not_a_node_page, Type}};
        {error, _} = Error ->
            Error
    end.

%% @doc Return the node at the given index as its key and the thing that it
%% refers to: the page number of a child, a value held inline, or the page
%% number and size of a value held on overflow pages.
node(Page, Index) ->
    maybe
        {ok, Type, Keys} ?= node_page(Page),
        true ?= Index >= 0 andalso Index < Keys orelse {error, no_such_node},
        {ok, Start} ?= node_offset(Page, Index),
        <<
            Low:16/little,
            High:16/little,
            NodeFlags:16/little,
            KeySize:16/little
        >> = binary:part(Page, Start, ?NODE_HEADER_SIZE),
        KeyStart = Start + ?NODE_HEADER_SIZE,
        DataStart = KeyStart + even(KeySize),
        true ?= DataStart =< byte_size(Page) orelse {error, node_overruns_page},
        Key = binary:part(Page, KeyStart, KeySize),
        {ok, Reference} ?=
            reference(Page, Type, DataStart, NodeFlags, Low bor (High bsl 16)),
        {ok, Key, Reference}
    end.

%% @doc Return the offset of a node within its page. The pointer array follows
%% the header, and each of its entries is relative to the end of that header.
node_offset(Page, Index) ->
    case binary:part(Page, ?PAGE_HEADER_SIZE + (Index * 2), 2) of
        <<Offset:16/little>> when
                Offset + ?PAGE_HEADER_SIZE + ?NODE_HEADER_SIZE
                    =< byte_size(Page) ->
            {ok, Offset + ?PAGE_HEADER_SIZE};
        _ ->
            {error, node_overruns_page}
    end.

%% @doc Return what a node points at. A branch node carries its child's page
%% number packed across the three header words that a leaf uses for its value
%% size and flags. A leaf's value sits inline unless the node is flagged:
%% `F_BIGDATA' replaces it with an `MDB_ovpage' naming the overflow page the
%% value starts on, the size staying the true size of the value; `F_SUBDATA'
%% with the `MDB_db' of the sub-database holding the key's duplicates; and
%% `F_DUPDATA' alone with the sub-page holding them, a fixed-width page
%% embedded whole in the node's data.
reference(_Page, branch, _DataStart, NodeFlags, Packed) ->
    {ok, {branch, Packed bor (NodeFlags bsl 32)}};
reference(Page, leaf, DataStart, NodeFlags, Size)
        when NodeFlags band ?F_SUBDATA =/= 0 ->
    case Size == ?DB_SIZE andalso DataStart + Size =< byte_size(Page) of
        true -> {ok, {leaf, {subdb, db(binary:part(Page, DataStart, Size))}}};
        false -> {error, {invalid_subdatabase_node, Size}}
    end;
reference(Page, leaf, DataStart, NodeFlags, Size)
        when NodeFlags band ?F_DUPDATA =/= 0 ->
    case
        Size >= ?PAGE_HEADER_SIZE andalso DataStart + Size =< byte_size(Page)
    of
        true -> {ok, {leaf, {subpage, binary:part(Page, DataStart, Size)}}};
        false -> {error, value_overruns_page}
    end;
reference(Page, leaf, DataStart, NodeFlags, Size)
        when NodeFlags band ?F_BIGDATA == 0 ->
    case DataStart + Size =< byte_size(Page) of
        true -> {ok, {leaf, binary:part(Page, DataStart, Size)}};
        false -> {error, value_overruns_page}
    end;
reference(Page, leaf, DataStart, _NodeFlags, Size) ->
    case DataStart + ?OVERFLOW_REF_SIZE =< byte_size(Page) of
        true ->
            <<PageNumber:64/little, _/binary>> =
                binary:part(Page, DataStart, ?OVERFLOW_REF_SIZE),
            {ok, {leaf, {overflow, PageNumber, Size}}};
        false ->
            {error, value_overruns_page}
    end.

%% @doc Round a size up to a multiple of two, as LMDB does when placing a
%% node's value after its key.
even(Size) -> (Size + 1) band -2.

%% @doc Find the index of the first node whose key is greater than or equal to
%% the given key, or the node count if the page holds no such node. A scan over
%% a range of keys starts here and walks forwards.
seek(Page, Key) ->
    case node_page(Page) of
        {ok, _Type, Keys} -> lower_bound(Page, Key, 0, Keys - 1);
        {error, _} = Error -> Error
    end.

%% @doc Look a key up in a single page. On a branch page this returns the child
%% to descend into and the index of the node naming it, which is what a scan
%% climbs back to when it exhausts a leaf; on a leaf page it returns the value,
%% or `not_found' if the key is absent from the page and therefore from the
%% database.
search(Page, Key) ->
    case node_page(Page) of
        {ok, branch, Keys} -> search_branch(Page, Key, Keys);
        {ok, leaf, Keys} -> search_leaf(Page, Key, Keys);
        {error, _} = Error -> Error
    end.

%% The first node of a branch page is its leftmost child, whose key is
%% implicitly negative infinity and is stored empty. The search therefore runs
%% over the nodes after it and, where it lands on a key strictly greater than
%% the one sought, steps back to the child that covers it.
search_branch(_Page, _Key, 0) ->
    {error, empty_branch_page};
search_branch(Page, Key, Keys) ->
    case lower_bound(Page, Key, 1, Keys - 1) of
        {error, _} = Error ->
            Error;
        Index when Index >= Keys ->
            child(Page, Keys - 1);
        Index ->
            case node(Page, Index) of
                {ok, Key, {branch, Child}} -> {branch, Index, Child};
                {ok, _OtherKey, _} -> child(Page, Index - 1);
                {error, _} = Error -> Error
            end
    end.

%% @doc Return the child named by the node at the given index of a branch page.
child(Page, Index) ->
    case node(Page, Index) of
        {ok, _Key, {branch, Child}} -> {branch, Index, Child};
        {ok, _Key, _} -> {error, invalid_branch_page};
        {error, _} = Error -> Error
    end.

%% A leaf holds the key itself or nothing does, so an inexact landing is a miss
%% rather than a step to a neighbour.
search_leaf(Page, Key, Keys) ->
    case lower_bound(Page, Key, 0, Keys - 1) of
        {error, _} = Error ->
            Error;
        Index when Index >= Keys ->
            not_found;
        Index ->
            case node(Page, Index) of
                {ok, Key, {leaf, Value}} -> {leaf, Value};
                {ok, _OtherKey, _} -> not_found;
                {error, _} = Error -> Error
            end
    end.

%% Binary search for the first key in `[Low, High]' that is greater than or
%% equal to `Key', returning `High + 1' when there is none. Erlang orders
%% binaries exactly as LMDB's default comparator does: byte-wise over the
%% shorter of the two, then by length.
lower_bound(_Page, _Key, Low, High) when Low > High ->
    Low;
lower_bound(Page, Key, Low, High) ->
    Middle = (Low + High) div 2,
    case node(Page, Middle) of
        {ok, NodeKey, _} when NodeKey < Key ->
            lower_bound(Page, Key, Middle + 1, High);
        {ok, _NodeKey, _} ->
            lower_bound(Page, Key, Low, Middle - 1);
        {error, _} = Error ->
            Error
    end.

%% @doc Return the fixed-width item at the given index of a `leaf2' page. Such
%% a page packs bare items back to back after its header, so an item is
%% addressed by index and width alone. The width the caller took from the
%% sub-database's `MDB_db' must agree with the one the page's header carries:
%% a file whose two widths differ is describing two different layouts at once,
%% and is refused rather than read by either.
item(Page, Index, Pad) ->
    maybe
        {ok, Keys} ?= fixed_width_page(Page, Pad),
        true ?= Index >= 0 andalso Index < Keys orelse {error, no_such_item},
        {ok, binary:part(Page, ?PAGE_HEADER_SIZE + (Index * Pad), Pad)}
    end.

%% Parse a page that must hold fixed-width items of the given width, refusing
%% every other kind of page, and the right kind whose own width disagrees.
fixed_width_page(Page, Pad) ->
    case page(Page) of
        {ok, #{ <<"type">> := leaf2, <<"pad">> := Pad, <<"keys">> := Keys }} ->
            {ok, Keys};
        {ok, #{ <<"type">> := leaf2, <<"pad">> := Other }} ->
            {error, {item_width_mismatch, Pad, Other}};
        {ok, #{ <<"type">> := Type }} ->
            {error, {not_a_fixed_width_page, Type}};
        {error, _} = Error ->
            Error
    end.

%% @doc Find the index of the first item that is greater than or equal to the
%% given key, or the item count if the page holds no such item. The key may be
%% shorter than the items: a prefix sorts before every item that begins with
%% it, so a scan over one prefix's run starts here and walks forwards.
item_seek(Page, Key, Pad) ->
    case fixed_width_page(Page, Pad) of
        {ok, Keys} -> item_lower_bound(Page, Key, Pad, 0, Keys - 1);
        {error, _} = Error -> Error
    end.

%% Binary search for the first item in `[Low, High]' that is greater than or
%% equal to `Key', mirroring `lower_bound/4' over items instead of nodes.
item_lower_bound(_Page, _Key, _Pad, Low, High) when Low > High ->
    Low;
item_lower_bound(Page, Key, Pad, Low, High) ->
    Middle = (Low + High) div 2,
    case item(Page, Middle, Pad) of
        {ok, Item} when Item < Key ->
            item_lower_bound(Page, Key, Pad, Middle + 1, High);
        {ok, _Item} ->
            item_lower_bound(Page, Key, Pad, Low, Middle - 1);
        {error, _} = Error ->
            Error
    end.

%%% Tests

%% The committed fixture is an LMDB 1.0 database written by the reference
%% `liblmdb' with `MDB_APPEND': 512-byte pages, 1,273 entries, and a tree three
%% levels deep, so a lookup crosses two branch pages before it reaches a leaf.
%% Its contents are a deterministic function of the two counts below, letting
%% the tests reconstruct every key and value they expect to find.
-define(FIXTURE, "test/lmdb-1.0.mdb").
-define(FIXTURE_KEYS, 1200).
-define(FIXTURE_MATCHES, 64).
-define(FIXTURE_BLOB_SIZE, 8192).

%% Three sorted-set containers, written by the reference `liblmdb' with
%% `MDB_APPENDDUP': one key `<<0>>' whose duplicates are 17-byte items, each a
%% 10-byte group hash followed by a 7-byte ascending tail. The first holds
%% 5,000 items on 512-byte pages, promoting the set to a sub-database three
%% levels deep; the second holds six, few enough to stay a sub-page inside the
%% main leaf node; the third holds 50,000 on the 64 KiB pages a published
%% index uses, filling its `P_LEAF2' leaves to 3,853 items each.
-define(DUPFIXED_FIXTURE, "test/lmdb-1.0-dupfixed.mdb").
-define(SUBPAGE_FIXTURE, "test/lmdb-1.0-subpage.mdb").
-define(DUPFIXED_64K_FIXTURE, "test/lmdb-1.0-dupfixed-64k.mdb").
-define(DUP_FIXTURE_PAD, 17).
-define(DUP_FIXTURE_GROUPS, 200).
-define(DUP_FIXTURE_MEMBERS, 25).
-define(SUBPAGE_FIXTURE_GROUPS, 2).
-define(SUBPAGE_FIXTURE_MEMBERS, 3).
-define(DUPFIXED_64K_GROUPS, 2000).
-define(DUPFIXED_64K_MEMBERS, 25).

%% @doc The key-value pairs that the fixture was built from, in no particular
%% order.
fixture_entries() ->
    [
        {<<"kv">>, <<"group">>},
        {<<"kv/sub">>, <<"group">>},
        {<<"kv/sub/a">>, <<"sub-a">>},
        {<<"kv/sub/b">>, <<"sub-b">>},
        {<<"alias">>, <<"group">>},
        {<<"alias/latest">>, <<"link:kv/000042">>},
        {<<"big">>, <<"group">>},
        {<<"big/blob">>, fixture_blob()},
        {<<"~match@1.0&type=Message">>, <<"group">>}
    ] ++
    [
        {fixture_key(I), <<"v-", (integer_to_binary(I))/binary>>}
    ||
        I <- lists:seq(0, ?FIXTURE_KEYS - 1)
    ] ++
    [
        {<<"~match@1.0&type=Message/", (fixture_id(J))/binary>>, <<>>}
    ||
        J <- lists:seq(0, ?FIXTURE_MATCHES - 1)
    ].

fixture_key(I) ->
    iolist_to_binary(io_lib:format("kv/~6..0b", [I])).

fixture_blob() ->
    << <<($A + (I rem 26))>> || I <- lists:seq(0, ?FIXTURE_BLOB_SIZE - 1) >>.

fixture_id(J) ->
    hb_util:encode(<< <<((J * K) rem 251)>> || K <- lists:seq(0, 31) >>).

%% @doc The items that the promoted duplicate-set fixture was built from, in
%% ascending order. The group hashes are generated with splitmix64 so that
%% they spread over the whole key space, as hashed predicates do.
dup_fixture_items() ->
    dup_fixture_items(?DUP_FIXTURE_GROUPS, ?DUP_FIXTURE_MEMBERS).
dup_fixture_items(Groups, Members) ->
    lists:usort(
        [
            <<(dup_fixture_hash(G))/binary, (M * 4096 + G):56>>
        ||
            G <- lists:seq(0, Groups - 1),
            M <- lists:seq(0, Members - 1)
        ]
    ).

dup_fixture_hash(G) ->
    <<(splitmix(2 * G)):64, (splitmix(2 * G + 1) bsr 48):16>>.

splitmix(X) ->
    Mask = 16#FFFFFFFFFFFFFFFF,
    A = (X + 16#9E3779B97F4A7C15) band Mask,
    B = ((A bxor (A bsr 30)) * 16#BF58476D1CE4E5B9) band Mask,
    C = ((B bxor (B bsr 27)) * 16#94D049BB133111EB) band Mask,
    C bxor (C bsr 31).

fixture() ->
    fixture(?FIXTURE).
fixture(Path) ->
    {ok, Bin} = file:read_file(Path),
    Bin.

%% @doc Take the meta page of the snapshot the file most recently committed.
%% The first 256 bytes are enough to learn the page size, which is what says
%% where the second meta page begins.
fixture_meta(File) ->
    {ok, First = #{ <<"page-size">> := PageSize, <<"txnid">> := FirstTxnID }} =
        meta(binary:part(File, 0, 256)),
    {ok, Second = #{ <<"txnid">> := SecondTxnID }} =
        meta(fixture_page(File, PageSize, 1)),
    case SecondTxnID > FirstTxnID of
        true -> Second;
        false -> First
    end.

fixture_page(File, PageSize, Number) ->
    binary:part(File, Number * PageSize, PageSize).

%% @doc Walk the tree from its root to the leaf that would hold the key,
%% returning the value found there, `not_found', or the leaf's bytes and the
%% index that a scan would start from when `seek' is asked for instead.
fixture_lookup(File, Meta, Key) ->
    fixture_lookup(File, Meta, Key, search).
fixture_lookup(
        File,
        #{ <<"page-size">> := PageSize, <<"root">> := Root, <<"depth">> := Depth },
        Key,
        Operation
    ) ->
    fixture_descend(File, PageSize, Root, Key, Operation, Depth).
fixture_descend(_File, _PageSize, _Number, _Key, _Operation, 0) ->
    {error, too_deep};
fixture_descend(File, PageSize, Number, Key, Operation, Remaining) ->
    Page = fixture_page(File, PageSize, Number),
    case search(Page, Key) of
        {branch, _Index, Child} ->
            fixture_descend(File, PageSize, Child, Key, Operation, Remaining - 1);
        Result when Operation == search ->
            Result;
        _ ->
            {Page, seek(Page, Key)}
    end.

%% @doc Read a value that was pushed onto overflow pages. Only the first of the
%% run carries a header, so the value is the contiguous bytes that follow it.
fixture_overflow(File, PageSize, Number, Size) ->
    Page = fixture_page(File, PageSize, Number),
    {ok, #{ <<"type">> := overflow, <<"pages">> := Pages }} = page(Page),
    ?assert(Pages >= 1),
    binary:part(File, (Number * PageSize) + 24, Size).

%% @doc The fixture's meta pages describe the database that built it, and the
%% pair of them alternates: page 0 still carries the empty snapshot that
%% `mdb_env_open' wrote, so a reader that took it would report no data at all.
meta_test() ->
    File = fixture(),
    {ok, #{ <<"page-size">> := PageSize, <<"txnid">> := ZeroTxnID }} =
        meta(binary:part(File, 0, 256)),
    ?assertEqual(512, PageSize),
    {ok, #{ <<"txnid">> := OneTxnID }} = meta(fixture_page(File, PageSize, 1)),
    ?assert(OneTxnID > ZeroTxnID),
    ?assertMatch(
        #{ <<"depth">> := 3, <<"entries">> := 1273 },
        fixture_meta(File)
    ),
    #{ <<"root">> := Root, <<"last-page">> := LastPage } = fixture_meta(File),
    ?assert(Root =< LastPage),
    ?assertEqual(byte_size(File), (LastPage + 1) * PageSize).

%% @doc Every key the fixture was built from resolves to the value it was built
%% with, and no key that it was not built from resolves at all.
lookup_test() ->
    File = fixture(),
    Meta = #{ <<"page-size">> := PageSize } = fixture_meta(File),
    lists:foreach(
        fun({Key, <<Value/binary>>}) when byte_size(Value) < ?FIXTURE_BLOB_SIZE ->
            ?assertEqual({leaf, Value}, fixture_lookup(File, Meta, Key));
           ({Key, Value}) ->
            % The blob is too large to sit inside a 512-byte page, so the leaf
            % names the overflow pages holding it instead.
            {leaf, {overflow, Number, Size}} = fixture_lookup(File, Meta, Key),
            ?assertEqual(byte_size(Value), Size),
            ?assertEqual(Value, fixture_overflow(File, PageSize, Number, Size))
        end,
        fixture_entries()
    ),
    lists:foreach(
        fun(Key) -> ?assertEqual(not_found, fixture_lookup(File, Meta, Key)) end,
        [
            <<>>,
            <<"kt">>,
            <<"kv/">>,
            <<"kv/000000 ">>,
            <<"kv/00000">>,
            <<"kv/001200">>,
            <<"kv/999999">>,
            <<"zzz">>,
            <<"~match@1.0&type=Message/">>
        ]
    ).

%% @doc `seek' lands on the first key at or after the one it is given, which is
%% where a scan over a group's children starts. The fixture's match-index rows
%% share a leaf with nothing else, so the walk from that index yields the
%% group's members in order.
seek_test() ->
    File = fixture(),
    Meta = fixture_meta(File),
    Prefix = <<"~match@1.0&type=Message/">>,
    {Page, Index} = fixture_lookup(File, Meta, Prefix, seek),
    {ok, Keys} = num_keys(Page),
    ?assert(Index < Keys),
    Found =
        [
            Key
        ||
            I <- lists:seq(Index, Keys - 1),
            {ok, Key, _} <- [node(Page, I)]
        ],
    Expected =
        lists:sort([<<Prefix/binary, (fixture_id(J))/binary>>
            || J <- lists:seq(0, ?FIXTURE_MATCHES - 1)]),
    ?assertEqual(lists:sublist(Expected, length(Found)), Found),
    % A key beyond everything the page holds seeks past its last node.
    ?assertEqual({ok, Keys}, {ok, seek(Page, <<255>>)}).

%% @doc Take a container fixture's duplicate set. The main tree of each is one
%% leaf holding the single key `<<0>>', whose node carries the set.
dup_fixture_set(File) ->
    Meta = #{ <<"page-size">> := PageSize, <<"root">> := Root } =
        fixture_meta(File),
    {leaf, Set} = search(fixture_page(File, PageSize, Root), <<0>>),
    {Meta, Set}.

%% @doc Collect a sub-database's leaves in order, descending its branch pages
%% depth-first. The branches are ordinary node pages; only the leaves differ.
dup_fixture_leaves(File, PageSize, Number) ->
    Page = fixture_page(File, PageSize, Number),
    case page(Page) of
        {ok, #{ <<"type">> := branch, <<"keys">> := Keys }} ->
            lists:append(
                [
                    dup_fixture_leaves(File, PageSize, Child)
                ||
                    I <- lists:seq(0, Keys - 1),
                    {ok, _Key, {branch, Child}} <- [node(Page, I)]
                ]
            );
        {ok, #{ <<"type">> := leaf2 }} ->
            [Page]
    end.

%% @doc Read every item that a fixed-width page holds, in order.
dup_fixture_leaf_items(Leaf) ->
    {ok, #{ <<"keys">> := Keys, <<"pad">> := Pad }} = page(Leaf),
    [
        Item
    ||
        I <- lists:seq(0, Keys - 1),
        {ok, Item} <- [item(Leaf, I, Pad)]
    ].

%% @doc The container fixtures' meta pages each name an LMDB 1.0 database of
%% the built page size, whose main database carries both duplicate flags, one
%% level of tree, and every item of the set as an entry.
dup_meta_test() ->
    lists:foreach(
        fun({Path, PageSize, Groups, Members}) ->
            Meta = fixture_meta(fixture(Path)),
            ?assertMatch(#{ <<"flags">> := 16#14, <<"depth">> := 1 }, Meta),
            ?assertEqual(PageSize, maps:get(<<"page-size">>, Meta)),
            ?assertEqual(
                length(dup_fixture_items(Groups, Members)),
                maps:get(<<"entries">>, Meta)
            )
        end,
        [
            {?DUPFIXED_FIXTURE, 512,
                ?DUP_FIXTURE_GROUPS, ?DUP_FIXTURE_MEMBERS},
            {?SUBPAGE_FIXTURE, 512,
                ?SUBPAGE_FIXTURE_GROUPS, ?SUBPAGE_FIXTURE_MEMBERS},
            {?DUPFIXED_64K_FIXTURE, 65536,
                ?DUPFIXED_64K_GROUPS, ?DUPFIXED_64K_MEMBERS}
        ]
    ).

%% @doc The promoted form: the single main-database entry is an `F_SUBDATA'
%% node whose data is the sub-database's `MDB_db'. Every leaf below its root
%% is fixed-width; each but the last is full, holding
%% `(page size - header) div pad' items; and the items of all of them, taken
%% in leaf order, are exactly the ones the fixture was built from -- strictly
%% ascending, with no duplicates, since the expectation itself is a sorted
%% set.
dup_subdatabase_test() ->
    lists:foreach(
        fun({Path, PageSize, Groups, Members}) ->
            File = fixture(Path),
            {Meta, {subdb, Db}} = dup_fixture_set(File),
            Expected = dup_fixture_items(Groups, Members),
            ?assertMatch(
                #{ <<"pad">> := ?DUP_FIXTURE_PAD, <<"flags">> := 16#10 },
                Db
            ),
            ?assertEqual(length(Expected), maps:get(<<"entries">>, Db)),
            ?assert(maps:get(<<"depth">>, Db) >= 2),
            ?assert(maps:get(<<"root">>, Db) =< maps:get(<<"last-page">>, Meta)),
            Leaves =
                dup_fixture_leaves(File, PageSize, maps:get(<<"root">>, Db)),
            Full = (PageSize - ?PAGE_HEADER_SIZE) div ?DUP_FIXTURE_PAD,
            {Filled, [_Last]} = lists:split(length(Leaves) - 1, Leaves),
            lists:foreach(
                fun(Leaf) ->
                    ?assertMatch({ok, #{ <<"keys">> := Full }}, page(Leaf))
                end,
                Filled
            ),
            ?assertEqual(
                Expected,
                lists:append(
                    [dup_fixture_leaf_items(Leaf) || Leaf <- Leaves]
                )
            )
        end,
        [
            {?DUPFIXED_FIXTURE, 512,
                ?DUP_FIXTURE_GROUPS, ?DUP_FIXTURE_MEMBERS},
            {?DUPFIXED_64K_FIXTURE, 65536,
                ?DUPFIXED_64K_GROUPS, ?DUPFIXED_64K_MEMBERS}
        ]
    ).

%% @doc The sub-page form: a set small enough stays inside its leaf node as a
%% fixed-width sub-page, marked `F_DUPDATA' without `F_SUBDATA', and reads
%% with the same item calls as a full page.
dup_subpage_test() ->
    File = fixture(?SUBPAGE_FIXTURE),
    {_Meta, {subpage, Sub}} = dup_fixture_set(File),
    ?assertMatch(
        {ok, #{ <<"type">> := leaf2, <<"pad">> := ?DUP_FIXTURE_PAD }},
        page(Sub)
    ),
    ?assertEqual(
        dup_fixture_items(?SUBPAGE_FIXTURE_GROUPS, ?SUBPAGE_FIXTURE_MEMBERS),
        dup_fixture_leaf_items(Sub)
    ).

%% @doc `item_seek' finds the first item at or after its key: an exact key
%% finds its own index, a group's hash the start of that group's run, and a
%% key past everything the item count. The node calls stay off fixed-width
%% pages, which have no node structure for them to read.
dup_item_seek_test() ->
    File = fixture(?DUPFIXED_FIXTURE),
    {#{ <<"page-size">> := PageSize }, {subdb, Db}} = dup_fixture_set(File),
    [Leaf | _] = dup_fixture_leaves(File, PageSize, maps:get(<<"root">>, Db)),
    Items = dup_fixture_leaf_items(Leaf),
    Third = lists:nth(3, Items),
    ?assertEqual(2, item_seek(Leaf, Third, ?DUP_FIXTURE_PAD)),
    ?assertEqual(0, item_seek(Leaf, <<>>, ?DUP_FIXTURE_PAD)),
    % A group hash is a prefix of its items, so it lands on the first of them.
    Hash = binary:part(lists:last(Items), 0, 10),
    First = item_seek(Leaf, Hash, ?DUP_FIXTURE_PAD),
    ?assertMatch(<<Hash:10/binary, _/binary>>, lists:nth(First + 1, Items)),
    ?assert(First == 0 orelse
        binary:part(lists:nth(First, Items), 0, 10) =/= Hash),
    {ok, #{ <<"keys">> := Keys }} = page(Leaf),
    ?assertEqual(
        Keys,
        item_seek(Leaf, binary:copy(<<255>>, ?DUP_FIXTURE_PAD),
            ?DUP_FIXTURE_PAD)
    ),
    ?assertEqual({error, no_such_item}, item(Leaf, Keys, ?DUP_FIXTURE_PAD)),
    ?assertEqual({error, no_such_item}, item(Leaf, -1, ?DUP_FIXTURE_PAD)),
    ?assertEqual({error, {item_width_mismatch, 21, 17}}, item(Leaf, 0, 21)),
    SubRoot = fixture_page(File, PageSize, maps:get(<<"root">>, Db)),
    ?assertEqual(
        {error, {not_a_fixed_width_page, branch}},
        item(SubRoot, 0, ?DUP_FIXTURE_PAD)
    ),
    ?assertEqual({error, {not_a_node_page, leaf2}}, node(Leaf, 0)),
    ?assertEqual({error, {not_a_node_page, leaf2}}, seek(Leaf, <<>>)),
    ?assertEqual({error, {not_a_node_page, leaf2}}, num_keys(Leaf)).

%% @doc A container whose own bookkeeping does not agree with its bytes is
%% refused wherever the disagreement lies: an item width of zero or one wider
%% than the page, an `F_SUBDATA' node sized as anything but an `MDB_db', and a
%% sub-page shorter than a page header.
refuses_malformed_container_test() ->
    File = fixture(?DUPFIXED_FIXTURE),
    {Meta = #{ <<"page-size">> := PageSize }, {subdb, Db}} =
        dup_fixture_set(File),
    [Leaf | _] = dup_fixture_leaves(File, PageSize, maps:get(<<"root">>, Db)),
    ?assertMatch(
        {error, {invalid_fixed_width_bounds, 0, _, _}},
        page(overwrite(Leaf, 16, <<0:16/little>>))
    ),
    ?assertMatch(
        {error, {invalid_fixed_width_bounds, 1024, _, _}},
        page(overwrite(Leaf, 16, <<1024:16/little>>))
    ),
    ?assertMatch(
        {error, {invalid_fixed_width_bounds, _, _, _}},
        page(overwrite(Leaf, 20, <<3:16/little>>))
    ),
    % The main leaf's node claims a sub-database description of 47 bytes.
    MainRoot = fixture_page(File, PageSize, maps:get(<<"root">>, Meta)),
    {ok, Offset} = node_offset(MainRoot, 0),
    ?assertEqual(
        {error, {invalid_subdatabase_node, 47}},
        node(overwrite(MainRoot, Offset, <<47:16/little>>), 0)
    ),
    % A sub-page must at least hold its own header.
    SubFile = fixture(?SUBPAGE_FIXTURE),
    {#{ <<"page-size">> := SubPageSize, <<"root">> := SubRoot }, {subpage, _}} =
        dup_fixture_set(SubFile),
    SubLeaf = fixture_page(SubFile, SubPageSize, SubRoot),
    {ok, SubOffset} = node_offset(SubLeaf, 0),
    ?assertEqual(
        {error, value_overruns_page},
        node(overwrite(SubLeaf, SubOffset, <<10:16/little>>), 0)
    ).

%% @doc Input that is not an LMDB 1.0 database is refused rather than
%% interpreted, including a database written by the 0.9 series that the local
%% store uses.
refuses_foreign_input_test() ->
    ?assertEqual({error, truncated_meta_page}, meta(<<>>)),
    ?assertEqual({error, truncated_meta_page}, meta(<<0:1024>>)),
    ?assertEqual({error, not_lmdb}, meta(<<0:2048>>)),
    ?assertEqual({error, not_lmdb}, meta(crypto:strong_rand_bytes(4096))),
    File = fixture(),
    Meta = binary:part(File, 0, 256),
    ?assertEqual(
        {error, {unsupported_data_version, 1}},
        meta(overwrite(Meta, 28, <<1:32/little>>))
    ),
    ?assertEqual(
        {error, {invalid_page_size, 1000}},
        meta(overwrite(Meta, 48, <<1000:32/little>>))
    ),
    % A comparator-changing flag is refused, and so is either duplicate flag
    % alone; the pair together is the sorted-set container and is admitted.
    ?assertEqual(
        {error, {unsupported_database_flags, 2}},
        meta(overwrite(Meta, 100, <<2:16/little>>))
    ),
    ?assertEqual(
        {error, {unsupported_database_flags, 4}},
        meta(overwrite(Meta, 100, <<4:16/little>>))
    ),
    ?assertEqual(
        {error, {unsupported_database_flags, 16#10}},
        meta(overwrite(Meta, 100, <<16#10:16/little>>))
    ),
    ?assertMatch(
        {ok, #{ <<"flags">> := 16#14 }},
        meta(overwrite(Meta, 100, <<16#14:16/little>>))
    ).

%% @doc A page whose own bookkeeping does not agree with its size is refused,
%% and so is a node that claims to reach past the end of the page it sits on.
%% Neither may be interpreted, because the offsets they carry would otherwise
%% size a read.
refuses_malformed_page_test() ->
    File = fixture(),
    Meta = #{ <<"page-size">> := PageSize, <<"root">> := Root } = fixture_meta(File),
    Page = fixture_page(File, PageSize, Root),
    ?assertMatch({ok, #{ <<"type">> := branch }}, page(Page)),
    ?assertEqual({error, truncated_page}, page(<<0:64>>)),
    ?assertMatch(
        {error, {invalid_page_flags, _}},
        page(overwrite(Page, 18, <<16#8000:16/little>>))
    ),
    % Flagging the branch page `P_LEAF|P_LEAF2' makes it claim a fixed-width
    % layout whose item width -- its `pad' field -- is zero, and a sub-page
    % flag without the fixed-width one is a layout with no reader here.
    ?assertMatch(
        {error, {invalid_fixed_width_bounds, 0, _, _}},
        page(overwrite(Page, 18, <<16#22:16/little>>))
    ),
    ?assertMatch(
        {error, {unsupported_page_layout, _}},
        page(overwrite(Page, 18, <<16#42:16/little>>))
    ),
    ?assertMatch(
        {error, {invalid_free_space_bounds, _, _}},
        page(overwrite(Page, 20, <<(PageSize * 2):16/little>>))
    ),
    % The pointer array is the only thing that says where a node begins.
    ?assertEqual(
        {error, node_overruns_page},
        node(overwrite(Page, 24, <<(PageSize - 4):16/little>>), 0)
    ),
    % A leaf node that claims a value longer than its page can hold.
    {Leaf, _} = fixture_lookup(File, Meta, <<"kv/000000">>, seek),
    {ok, Offset} = node_offset(Leaf, 0),
    ?assertEqual(
        {error, value_overruns_page},
        node(overwrite(Leaf, Offset, <<16#FFFF:16/little>>), 0)
    ).

%% @doc Replace the bytes at an offset of a page, leaving its length unchanged.
overwrite(Page, Offset, Replacement) ->
    Size = byte_size(Replacement),
    <<Head:Offset/binary, _:Size/binary, Tail/binary>> = Page,
    <<Head/binary, Replacement/binary, Tail/binary>>.
