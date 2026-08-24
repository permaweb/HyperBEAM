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
%%% Only the plain, single-value database that a HyperBEAM store writes is
%%% supported. `MDB_DUPSORT' and its relatives change both the layout of a leaf
%%% page and the key comparator, so `meta/1' refuses a main database carrying
%%% any of their flags.
-module(hb_lmdb_page).
-export([meta/1, page/1, search/2, seek/2, node/2, num_keys/1]).
%% The fixture that the tests below read is also published to Arweave, where
%% `hb_store_arlmdb' reads the same bytes. Its tests take the contents to expect
%% from here, so that both readers are held to one description of them.
-export([fixture_entries/0]).
-include_lib("eunit/include/eunit.hrl").

%% Sizes, in bytes, of the fixed-length structures of the format.
-define(PAGE_HEADER_SIZE, 24).      % PAGEHDRSZ, and PAGEBASE with it
-define(NODE_HEADER_SIZE, 8).       % NODESIZE
-define(OVERFLOW_REF_SIZE, 24).     % sizeof(MDB_ovpage)

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
%% overflow pages that hold it.
-define(F_BIGDATA, 16#01).

%% Database flags that alter the leaf layout or the key comparator:
%% `MDB_REVERSEKEY', `MDB_DUPSORT', `MDB_INTEGERKEY', `MDB_DUPFIXED',
%% `MDB_INTEGERDUP' and `MDB_REVERSEDUP'.
-define(UNSUPPORTED_DB_FLAGS, 16#7E).

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
            _MainPad:32/little,
            MainFlags:16/little,
            Depth:16/little,
            _BranchPages:64/little,
            _LeafPages:64/little,
            _OverflowPages:64/little,
            Entries:64/little,
            Root:64/little,
            LastPage:64/little,
            TxnID:64/little,
            _/binary
        >>
    ) ->
    validate_meta(
        Magic,
        Version,
        PageSize,
        MainFlags,
        #{
            <<"page-size">> => PageSize,
            <<"root">> => Root,
            <<"depth">> => Depth,
            <<"entries">> => Entries,
            <<"last-page">> => LastPage,
            <<"txnid">> => TxnID
        }
    );
meta(Bin) when is_binary(Bin) ->
    {error, truncated_meta_page}.

%% @doc Refuse every meta page that this module cannot read: one belonging to
%% another format entirely, one written by a version whose layout differs, one
%% claiming a page size LMDB could not have used, and one whose main database
%% is not the plain single-value kind.
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
        when Flags band ?UNSUPPORTED_DB_FLAGS =/= 0 ->
    {error, {unsupported_database_flags, Flags}};
validate_meta(_Magic, _Version, _PageSize, _Flags, Meta) ->
    {ok, Meta}.

%% @doc Parse a page header, returning the kind of page it is: a `branch' or
%% `leaf' page reports the number of nodes it holds as `keys', and an
%% `overflow' page reports the number of pages its value spans as `pages'.
page(Page = <<
            _PageNumber:64/little,
            _TxnID:64/little,
            _Pad:16/little,
            Flags:16/little,
            Bounds:4/binary,
            _/binary
        >>) ->
    classify(Flags, Bounds, byte_size(Page) - ?PAGE_HEADER_SIZE);
page(Bin) when is_binary(Bin) ->
    {error, truncated_page}.

%% The four bytes that follow the flags are the bounds of a node page's free
%% space, or the page count of an overflow page. `lower', `upper' and the
%% pointer array are all relative to the end of the header, so the bounds are
%% checked against the page size less that header.
classify(Flags, _Bounds, _Limit) when Flags band (bnot ?PAGE_FLAG_MASK) =/= 0 ->
    {error, {invalid_page_flags, Flags}};
classify(Flags, _Bounds, _Limit) when Flags band (?P_LEAF2 bor ?P_SUBP) =/= 0 ->
    {error, {unsupported_page_layout, Flags}};
classify(Flags, <<Pages:32/little>>, _Limit) when Flags band ?P_OVERFLOW =/= 0 ->
    {ok, #{ <<"type">> => overflow, <<"pages">> => Pages }};
classify(Flags, <<Lower:16/little, Upper:16/little>>, Limit)
        when Flags band (?P_BRANCH bor ?P_LEAF) =/= 0 ->
    case Lower band 1 == 0 andalso Lower =< Upper andalso Upper =< Limit of
        true ->
            Type = case Flags band ?P_BRANCH of 0 -> leaf; _ -> branch end,
            {ok, #{ <<"type">> => Type, <<"keys">> => Lower bsr 1 }};
        false ->
            {error, {invalid_free_space_bounds, Lower, Upper}}
    end;
classify(Flags, _Bounds, _Limit) ->
    {error, {unsupported_page_type, Flags}}.

%% @doc Return the number of nodes held by a branch or leaf page.
num_keys(Page) ->
    case node_page(Page) of
        {ok, _Type, Keys} -> {ok, Keys};
        {error, _} = Error -> Error
    end.

%% Parse a page that must carry nodes, refusing every other kind.
node_page(Page) ->
    case page(Page) of
        {ok, #{ <<"type">> := Type, <<"keys">> := Keys }} -> {ok, Type, Keys};
        {ok, #{ <<"type">> := Type }} -> {error, {not_a_node_page, Type}};
        {error, _} = Error -> Error
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
%% size and flags. A leaf's value sits inline unless the node is flagged
%% `F_BIGDATA', in which case the node instead holds an `MDB_ovpage' naming the
%% page the value starts on; the size stays the true size of the value.
reference(_Page, branch, _DataStart, NodeFlags, Packed) ->
    {ok, {branch, Packed bor (NodeFlags bsl 32)}};
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

fixture() ->
    {ok, Bin} = file:read_file(?FIXTURE),
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
    ?assertEqual(
        {error, {unsupported_database_flags, 4}},
        meta(overwrite(Meta, 100, <<4:16/little>>))
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
    ?assertMatch(
        {error, {unsupported_page_layout, _}},
        page(overwrite(Page, 18, <<16#22:16/little>>))
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
