%%% @doc A read-only store over an LMDB 1.0 file published on Arweave: the
%%% data of a transaction, read in place with byte-range requests. The file
%%% is never downloaded and never opened with LMDB.
%%%
%%% The store knows the file's layout alone, and reads it from the file.
%%% A main database in LMDB's default mode -- keys with values -- holds
%%% paths as `hb_store_lmdb' writes them: a value is a `group' marker, a
%%% `link:' to another path, or the value itself. Such a file is served as
%%% that store serves its own, read-only: reads follow links and answer a
%%% group with its children's values, lists answer with a group's children,
%%% and `resolve' follows the links in a path. A main database in DUP mode
%%% (`MDB_DUPSORT bor MDB_DUPFIXED') -- fixed-width keys, each holding a
%%% sorted set of fixed-width duplicate values -- takes a request that is
%%% a key, or a key followed by value bits:
%%% ```
%%%     read:   The first value under the key that begins with the value
%%%             bits, or a proven miss.
%%%     list:   The values under the key, from the one `from' gives, in
%%%             the `direction' requested, up to the `limit'. A batch is
%%%             the rest of the first leaf read, and reads into the next
%%%             leaf only while the first holds nothing past the value the
%%%             list starts from. A request shorter than the key width
%%%             lists the keys beginning with it instead.
%%%     type:   `composite' for a key, `simple' for a key and value bits
%%%             that a read finds.
%%% '''
%%% A file holding a single key serves that key's values, and its requests
%%% leave the key out: they are value bits. A store definition with
%%% `return-row' asks for rows instead: a request is any leading bits of a
%%% row -- its key followed by its value -- and reads and lists answer with
%%% whole rows, a list's `from' continuing the request's bits. That serves
%%% an artifact whose builder let a field straddle the key boundary, as the
%%% match index below did.
%%%
%%% No artifact's geometry lives here. The keys a file serves, the bits a
%%% key seeks and the message a value answers with belong to the store
%%% definition, through the store manager's pipeline -- `prefix', `to-key',
%%% `from-key' and `from-value', described in `hb_store' -- so a definition
%%% alone serves any file of either mode. The tests read three. A published
%%% `hb_store_lmdb' layout needs nothing but its root:
%%% ```
%%%     #{
%%%         <<"store-module">> => hb_store_arlmdb,
%%%         <<"root">> => <<"aOLp1k7Rbatt0FRq6LBCJtvjLrKBGNfOo5qZs66gGuw">>
%%%     }
%%% '''
%%% An offset index maps ANS-104 IDs to weave ranges: one key, 160-bit
%%% values of an ID's leading 77 bits, `start' and `length':
%%% ```
%%%     #{
%%%         <<"store-module">> => hb_store_arlmdb,
%%%         <<"root">> => <<"7vg2832WFsisEcBr1oBQ8ldc4EGOkjQdwW46hDvJsOs">>,
%%%         <<"prefix">> => <<"~arweave@2.9/offset=">>,
%%%         <<"to-key">> => <<"~base64url@1.0/decode/~bits@1.0/take=77">>,
%%%         <<"from-value">> =>
%%%             <<"~bits@1.0/from=_:77,start:49+integer,length:34+integer">>
%%%     }
%%% '''
%%% A match index maps predicates to weave offsets: 128-bit rows of two
%%% truncated hashes and a 49-bit offset, whose high bit ends the 80-bit
%%% key, so a predicate's rows span two keys and its definition asks for
%%% rows. Its device hashes a predicate into row bits and reads a row
%%% back as a member:
%%% ```
%%%     #{
%%%         <<"store-module">> => hb_store_arlmdb,
%%%         <<"root">> => <<"oWRzBr3KHhULAL-s5ULeXac1mb_WQOX5uFBRea16iRI">>,
%%%         <<"return-row">> => true,
%%%         <<"prefix">> => <<"~match@1.0/">>,
%%%         <<"to-key">> =>
%%%             <<"~match@1.0/row&key-hash-size=39&value-hash-size=40",
%%%                 "&offset-size=49">>,
%%%         <<"from-key">> =>
%%%             <<"~match@1.0/member&key-hash-size=39&value-hash-size=40",
%%%                 "&offset-size=49">>
%%%     }
%%% '''
%%% The file is LMDB 1.0 (`MDB_DATA_VERSION' 3, little-endian), of pages
%%% between 256 bytes and 64 KiB, the size the meta page records. Each page
%%% opens with a 24 byte header: the page number and the LMDB internal
%%% transaction as 64 bit ints, then `pad', `flags', `lower', and `upper' as
%%% 16 bit integers. The node pointer array follows the header, and its
%%% entries -- like `lower' and `upper' -- are relative to the end of the
%%% header. The meta pages are pages 0 and 1; the one with the higher
%%% transaction ID wins. The main database is in the default mode or in DUP
%%% mode. Its branch pages hold keys as node keys, the first node of each
%%% standing for the least key. In the default mode a leaf node holds a
%%% value in the node's data, or `F_BIGDATA' with the value on overflow
%%% pages from the page the data numbers. In DUP mode each leaf node holds
%%% a key's duplicate set in one of three forms: a single item as the
%%% node's data; an `F_DUPDATA' sub-page, a whole fixed-width page image in
%%% the node's data; or an `F_SUBDATA' sub-database, whose 48 byte record
%%% the node holds and whose tree the store descends -- branch pages whose
%%% keys are items, and `P_LEAF2' leaves holding no nodes and no slot array,
%%% where item `I' is the `pad' bytes at page offset `24 + I * pad' and
%%% `lower bsr 1' items fill the page, ascending strictly.
%%%
%%% Fetched chunks are retained in the stores under the `chunk-store' key
%%% of the store definition, defaulting to a volatile store that expires
%%% every five minutes; `[]' retains nothing. Published files are
%%% defragmented -- the first chunk holds the meta pages and main root,
%%% branch pages cluster, and leaves follow in key order -- so the chunks a
%%% read retains answer most of the reads that follow it.
%%%
%%% Every layout outside this contract is refused with a distinct error,
%%% never treated as a miss, and a failed byte-range fetch surfaces as
%%% `{error, {unavailable, ...}}': a proven miss requires a successfully
%%% read leaf.
-module(hb_store_arlmdb).
-export([start/3, stop/3, scope/0, scope/1]).
-export([read/3, list/3, resolve/3, type/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The byte-level constants of the LMDB 1.0 file.
%% An Arweave chunk holds a whole number of pages of every admitted size, and
%% a published file's data begins on a chunk boundary, so every page
%% fetch lies within one chunk of the file.
-define(CHUNK_SIZE, ?DATA_CHUNK_SIZE).
%% The default chunk store expires its whole table on this cadence.
-define(CHUNK_TTL_MS, 300000).
-define(PAGE_HDR, 24).
-define(NODE_HDR, 8).
-define(DB_SIZE, 48).
-define(MIN_PAGE_SIZE, 256).
-define(MAX_PAGE_SIZE, 65536).
-define(MDB_MAGIC, 16#BEEFC0DE).
-define(MDB_VERSION, 3).
-define(MAIN_DB_FLAGS, 16#14). % MDB_DUPSORT bor MDB_DUPFIXED
-define(P_BRANCH, 16#01).
-define(P_LEAF, 16#02).
-define(P_LEAF2, 16#20).
-define(P_SUBP, 16#40).
-define(F_BIGDATA, 16#01). % A value on overflow pages.
-define(F_DUPDATA, 16#04). % A sub-page in the node's data.
-define(F_DUPTREE, 16#06). % F_DUPDATA bor F_SUBDATA: a sub-database record.

%% @doc Resolve the root's weave location, validate the file's meta page
%% and main tree, and return the store instance: the location, the main
%% tree and the layout. Fetched chunks are retained by the chunk
%% store alone.
start(StoreOpts = #{ <<"root">> := Root }, _Req, _Opts) ->
    maybe
        {ok, Start, Size} ?= read_location(Root, StoreOpts),
        {ok, Meta} ?= read_meta(Start, Size, StoreOpts),
        {ok, Layout} ?= read_layout(Meta, StoreOpts),
        ?event(store_arlmdb,
            {started, {root, Root}, {meta, Meta}, {layout, Layout}}
        ),
        {ok, maps:merge(Meta, Layout)}
    end.

%% @doc The instance holds no processes: nothing to stop.
stop(_StoreOpts, _Req, _NodeOpts) -> ok.

%% @doc Reads are served by remote byte-range fetches.
scope() -> remote.
scope(#{ <<"scope">> := Scope }) -> Scope;
scope(_) -> scope().

%% @doc A path with its links followed; a key of a sorted set holds no
%% links, so it resolves to itself.
resolve(StoreOpts, #{ <<"resolve">> := Key }, _NodeOpts) ->
    case hb_store:find(StoreOpts) of
        Meta = #{ <<"mode">> := default } ->
            path_resolve(hb_path:to_binary(Key), Meta, StoreOpts);
        _ ->
            {ok, Key}
    end.

%% @doc The type of a path or of a key: `composite' for a group, a prefix
%% of keys or a key a sorted set holds, and `simple' for a value.
type(StoreOpts, #{ <<"type">> := Key }, NodeOpts) ->
    Meta = hb_store:find(StoreOpts),
    case Meta of
        #{ <<"mode">> := default } ->
            path_type(hb_path:to_binary(Key), Meta, StoreOpts);
        _ ->
            set_type(StoreOpts, Key, Meta, NodeOpts)
    end.

%% @doc `composite' for a prefix of keys or a key a sorted set holds, and
%% `simple' for a key and value bits that a read finds.
set_type(StoreOpts, Bits, Meta, NodeOpts) ->
    case classify(Bits, Meta, StoreOpts) of
        {keys, _Prefix} ->
            {ok, composite};
        {key, Key} ->
            maybe
                {ok, _Set} ?= set_of(Key, Meta, StoreOpts),
                {ok, composite}
            end;
        Error = {error, _} ->
            Error;
        _ ->
            maybe
                {ok, _Found} ?=
                    read(StoreOpts, #{ <<"read">> => Bits }, NodeOpts),
                {ok, simple}
            end
    end.

%% @doc A path's value, or the values under a key.
read(StoreOpts, #{ <<"read">> := Key }, _NodeOpts) ->
    case hb_store:find(StoreOpts) of
        Meta = #{ <<"mode">> := default } ->
            path_read(hb_path:to_binary(Key), Meta, StoreOpts);
        Meta ->
            set_read(StoreOpts, Key, Meta)
    end.

%% @doc The first value under a key that begins with its value bits, or a
%% proven miss. A key alone reads its first value.
set_read(StoreOpts, Bits, Meta) ->
    First =
        case classify(Bits, Meta, StoreOpts) of
            {key, Key} -> values(Key, <<>>, none, 1, asc, Meta, StoreOpts);
            {value, Key, Prefix} ->
                values(Key, Prefix, none, 1, asc, Meta, StoreOpts);
            {rows, Prefix} -> rows(Prefix, none, 1, asc, Meta, StoreOpts);
            {keys, _Prefix} -> {error, {'invalid-key-size', bit_size(Bits)}};
            Error -> Error
        end,
    case First of
        {ok, [Found]} -> {ok, Found};
        {ok, []} -> {error, not_found};
        Failure -> Failure
    end.

%% @doc A group's children, or the values under a key, bounded by the
%% request.
list(StoreOpts, Req = #{ <<"list">> := Key }, _NodeOpts) ->
    case hb_store:find(StoreOpts) of
        Meta = #{ <<"mode">> := default } ->
            path_list(hb_path:to_binary(Key), Req, Meta, StoreOpts);
        Meta ->
            set_list(StoreOpts, Key, Req, Meta)
    end.

%% @doc The values under a key -- those beginning with its value bits when
%% it holds any -- or, for bits shorter than a key, the keys beginning
%% with them; from the request's `from', in its direction, up to its limit.
set_list(StoreOpts, Bits, Req, Meta) ->
    #{ <<"from">> := From, <<"limit">> := Limit, <<"direction">> := Dir } =
        hb_store_utils:list_request_bounds(Req),
    case classify(Bits, Meta, StoreOpts) of
        {keys, Prefix} ->
            keys(Prefix, From, Limit, Dir, Meta, StoreOpts);
        {key, Key} ->
            values(Key, <<>>, From, Limit, Dir, Meta, StoreOpts);
        {value, Key, Prefix} ->
            values(Key, Prefix, From, Limit, Dir, Meta, StoreOpts);
        {rows, Prefix} ->
            rows(Prefix, From, Limit, Dir, Meta, StoreOpts);
        Error ->
            Error
    end.

%% @doc What a request's bits are, by the file's widths: a prefix of keys
%% when shorter than a key; a key; or a key and value bits -- the value
%% bits alone where the file holds one key. Under `return-row' the bits
%% are the leading bits of rows. Bits longer than a row, and anything but
%% bits, are refused.
classify(Bits, Meta, StoreOpts) when is_bitstring(Bits) ->
    #{ <<"key-size">> := KeySize, <<"value-size">> := ValueSize } = Meta,
    KeyBits = KeySize * 8,
    ReturnRow = hb_util:atom(maps:get(<<"return-row">>, StoreOpts, false)),
    case bit_size(Bits) of
        Size when Size > KeyBits + (ValueSize * 8) ->
            {error, {'invalid-key-size', Size}};
        _ when ReturnRow ->
            {rows, Bits};
        Size when Size < KeyBits ->
            {keys, Bits};
        KeyBits ->
            {key, Bits};
        _ ->
            <<Key:KeyBits/bitstring, Prefix/bitstring>> = Bits,
            {value, Key, Prefix}
    end;
classify(Bits, _Meta, _StoreOpts) ->
    {error, {'invalid-key', Bits}}.

%%% Paths, as `hb_store_lmdb' writes them.

%% @doc A path's value: its own when it holds one, its target's when it
%% holds a link, and its children's values when it holds a group or has
%% children with no marker of its own. A path the file lacks is read
%% again with the links in it followed, and is otherwise a miss.
path_read(Path, Meta, Opts) ->
    maybe
        {ok, Rows} ?= rows(Path, Meta, Opts),
        case path_result(Path, Rows, Meta, Opts) of
            {error, not_found} -> path_miss(Path, Meta, Opts);
            Result -> Result
        end
    end.

%% @doc Classify the marker row of a path's scan, which sorts ahead of the
%% path's descendants: a link chases its target, a group answers with its
%% immediate children, any other value answers itself, and a path with
%% descendants but no marker is a group without a marker.
path_result(Path, [{Path, <<"link:", Link/binary>>} | _], Meta, Opts)
        when byte_size(Link) > 0 ->
    path_read(Link, Meta, Opts);
path_result(Path, [{Path, <<"group">>} | Rows], _Meta, _Opts) ->
    Prefix = hb_store_utils:child_prefix(Path),
    {composite, hb_store_utils:immediate_children(Prefix, Rows)};
path_result(Path, [{Path, Value} | _], _Meta, _Opts) ->
    {ok, Value};
path_result(Path, Rows, _Meta, _Opts) ->
    Prefix = hb_store_utils:child_prefix(Path),
    Children = hb_store_utils:immediate_children(Prefix, Rows),
    case {hb_store_utils:is_data_path(Path), Children} of
        {false, [_ | _]} -> {composite, Children};
        _ -> {error, not_found}
    end.

%% @doc A path absent from the scan, read again through the links in it.
path_miss(Path, Meta, Opts) ->
    through_links(Path, fun path_read/3, Meta, Opts).

%% @doc A path taken again by `Then' with the links within it followed,
%% when they lead elsewhere, and a miss otherwise. Content-addressed `data'
%% keys hold no links.
through_links(Path, Then, Meta, Opts) ->
    Parts = binary:split(Path, <<"/">>, [global, trim_all]),
    Links =
        case hb_store_utils:is_data_path(Path) of
            true ->
                {ok, Parts};
            false ->
                hb_store_utils:resolve_path_links(reader(Meta, Opts), Parts)
        end,
    case Links of
        {ok, Resolved} ->
            case hb_store_utils:to_path(Resolved) of
                Path -> {error, not_found};
                Target -> Then(Target, Meta, Opts)
            end;
        {error, _} ->
            {error, not_found}
    end.

%% @doc A group's children, bounded by the request: the path's links are
%% followed, and it must hold a group.
path_list(Path, Req, Meta, Opts) ->
    case resolved(Path, Meta, Opts) of
        {ok, Group, <<"group">>} ->
            maybe
                {ok, Children} ?= children(Group, Meta, Opts),
                {ok, hb_store_utils:apply_list_bounds(Children, Req)}
            end;
        {ok, _Path, _Value} ->
            {error, not_found};
        Error ->
            Error
    end.

%% @doc `composite' for a path that holds a group, and `simple' for one
%% that holds a value, its links followed.
path_type(Path, Meta, Opts) ->
    case resolved(Path, Meta, Opts) of
        {ok, _Resolved, <<"group">>} -> {ok, composite};
        {ok, _Resolved, _Value} -> {ok, simple};
        Error -> Error
    end.

%% @doc The path with the links in it followed, or the path itself when
%% they cannot be.
path_resolve(Path, Meta, Opts) ->
    Parts = binary:split(Path, <<"/">>, [global]),
    case hb_store_utils:resolve_path_links(reader(Meta, Opts), Parts) of
        {ok, Resolved} -> {ok, hb_store_utils:to_path(Resolved)};
        {error, _} -> {ok, Path}
    end.

%% @doc The value a path holds and the path holding it, with a link the
%% path holds followed, and the links within the path followed when it
%% holds nothing.
resolved(Path, Meta, Opts) ->
    case value(Path, Meta, Opts) of
        {ok, Value} ->
            case hb_store_utils:is_link(Value) of
                {true, Link} -> resolved(Link, Meta, Opts);
                false -> {ok, Path, Value}
            end;
        {error, not_found} ->
            through_links(Path, fun resolved/3, Meta, Opts);
        Error ->
            Error
    end.

%% @doc A reader of values for the link resolver, which sees only hits
%% and misses.
reader(Meta, Opts) ->
    fun(Key) ->
        case value(Key, Meta, Opts) of
            {ok, Value} -> {ok, Value};
            _ -> not_found
        end
    end.

%% @doc The value a key holds, or a proven miss.
value(Key, Meta, Opts) ->
    #{ <<"main-root">> := Root, <<"main-depth">> := Depth } = Meta,
    maybe
        {ok, {Leaf, Slot, Count, _Bound}} ?=
            descend(Root, Depth, Key, nodes, asc, none, Meta, Opts),
        true ?= Slot < Count orelse {error, not_found},
        Node = node(Leaf, Slot),
        true ?= maps:get(key, Node) =:= Key orelse {error, not_found},
        node_value(Leaf, Node, Meta, Opts)
    end.

%% @doc Every row whose key begins with the prefix, in key order.
rows(Prefix, Meta, Opts) ->
    Collect =
        fun(Leaf, Node = #{ key := Key }, Rows) ->
            case begins_with(Key, Prefix) of
                true ->
                    maybe
                        {ok, Value} ?= node_value(Leaf, Node, Meta, Opts),
                        {ok, [{Key, Value} | Rows]}
                    end;
                false ->
                    {stop, Rows}
            end
        end,
    maybe
        {ok, Rows} ?= fold_nodes(Prefix, Collect, [], Meta, Opts),
        {ok, lists:reverse(Rows)}
    end.

%% @doc A group's children: each key beneath it gives one, its next path
%% component, and a component with descendants of its own is seen once,
%% the read seeking past its subtree.
children(Path, Meta, Opts) ->
    Prefix = hb_store_utils:child_prefix(Path),
    Size = byte_size(Prefix),
    Collect =
        fun(_Leaf, #{ key := Key }, Children) ->
            case Key of
                <<Prefix:Size/binary, Rest/binary>> when Rest =/= <<>> ->
                    case binary:split(Rest, <<"/">>) of
                        [Child] ->
                            {ok, [Child | Children]};
                        [Child, _Below] ->
                            Past = <<Prefix/binary, Child/binary, ($/ + 1)>>,
                            {seek, Past, [Child | Children]}
                    end;
                _ ->
                    {stop, Children}
            end
        end,
    maybe
        {ok, Children} ?= fold_nodes(Prefix, Collect, [], Meta, Opts),
        {ok, lists:usort(Children)}
    end.

%% @doc Fold a function over the main tree's nodes from the target key
%% onward in key order, until it stops or no key remains. It may seek
%% ahead to another key. Crossing a leaf re-descends at the boundary key
%% the descent passed down.
fold_nodes(Target, Fun, Acc, Meta, Opts) ->
    #{ <<"main-root">> := Root, <<"main-depth">> := Depth } = Meta,
    maybe
        {ok, {Leaf, Slot, Count, Bound}} ?=
            descend(Root, Depth, Target, nodes, asc, none, Meta, Opts),
        fold_slots(Leaf, Slot, Count, Bound, Fun, Acc, Meta, Opts)
    end.

%% @doc Fold over one leaf's nodes from a slot onward, and past its end
%% over the leaf beyond it.
fold_slots(_Leaf, Slot, Count, Bound, Fun, Acc, Meta, Opts)
        when Slot >= Count ->
    case beyond(asc, Bound) of
        none -> {ok, Acc};
        Next -> fold_nodes(Next, Fun, Acc, Meta, Opts)
    end;
fold_slots(Leaf, Slot, Count, Bound, Fun, Acc, Meta, Opts) ->
    case Fun(Leaf, node(Leaf, Slot), Acc) of
        {ok, Folded} ->
            fold_slots(Leaf, Slot + 1, Count, Bound, Fun, Folded, Meta, Opts);
        {seek, Target, Folded} ->
            fold_nodes(Target, Fun, Folded, Meta, Opts);
        {stop, Folded} ->
            {ok, Folded};
        Error ->
            Error
    end.

%% @doc A default-mode leaf node's value: the node's data, or the bytes on the
%% overflow pages the data numbers, of the size the node records.
node_value(Leaf, Node = #{ flags := 0 }, _Meta, _Opts) ->
    node_data(Leaf, Node);
node_value(Leaf, Node = #{ flags := ?F_BIGDATA }, Meta, Opts) ->
    #{ lo := Lo, hi := Hi, offset := Offset, ksize := KSize } = Node,
    DataOffset = Offset + ?NODE_HDR + KSize + (KSize band 1),
    case Leaf of
        <<_:DataOffset/binary, PgNo:64/little, _/binary>> ->
            overflow(PgNo, Lo bor (Hi bsl 16), Meta, Opts);
        _ ->
            {error, {'invalid-node-size', DataOffset}}
    end;
node_value(_Leaf, #{ flags := Flags }, _Meta, _Opts) ->
    {error, {'invalid-node-flags', Flags}}.

%% @doc The value on an overflow chain: contiguous pages from the one
%% numbered, its header first, every page within the file.
overflow(PgNo, Size, Meta, Opts) ->
    #{ <<"page-size">> := PageSize, <<"last-page">> := LastPage } = Meta,
    #{ <<"start">> := Start, <<"size">> := Total } = Meta,
    Pages = (?PAGE_HDR + Size + PageSize - 1) div PageSize,
    maybe
        true ?=
            PgNo + Pages - 1 =< LastPage
                orelse {error, {'invalid-page-number', PgNo}},
        fetch_span(
            Start, Total, (PgNo * PageSize) + ?PAGE_HDR, Size, Opts, []
        )
    end.

%% @doc A byte range of the file that may span chunks, fetched chunk
%% by chunk.
fetch_span(_Start, _Total, _Offset, 0, _Opts, Parts) ->
    {ok, iolist_to_binary(lists:reverse(Parts))};
fetch_span(Start, Total, Offset, Length, Opts, Parts) ->
    Within = Offset rem ?CHUNK_SIZE,
    Take = min(Length, ?CHUNK_SIZE - Within),
    maybe
        {ok, Part} ?= fetch(Start, Total, Offset, Take, Opts),
        fetch_span(
            Start, Total, Offset + Take, Length - Take, Opts, [Part | Parts]
        )
    end.

%%% Reading the main tree.

%% @doc The keys beginning with the prefix, from the row `from' gives --
%% the prefix's own end when `from' is absent -- in the direction requested,
%% up to the limit, across the main tree's leaves.
keys(Prefix, From, Limit, Dir, Meta, Opts) ->
    #{ <<"key-size">> := KeySize } = Meta,
    maybe
        {ok, Target} ?= target(From, Prefix, Dir, KeySize * 8),
        {ok, Keys} ?= main(Target, Prefix, Limit, Dir, Meta, Opts, []),
        {ok, lists:reverse(Keys)}
    end.

%% @doc Collect the main keys beginning with the prefix from the target,
%% in the direction requested, up to the limit, onto a reversed
%% accumulator. Crossing a leaf re-descends at the boundary key the descent
%% passed down.
main(Target, Prefix, Limit, Dir, Meta, Opts, Keys) ->
    #{ <<"main-root">> := Root, <<"main-depth">> := Depth } = Meta,
    maybe
        {ok, {Leaf, Slot, Count, Bound}} ?=
            descend(Root, Depth, Target, nodes, Dir, none, Meta, Opts),
        {ok, {Taken, Collected}} ?=
            gather(Leaf, Slot, Count, Prefix, Dir, Limit, Meta, Keys),
        Left = spent(Collected, Target, Taken),
        Next = beyond(Dir, Bound),
        More =
            Left =/= 0 andalso Next =/= none
                andalso begins_with(Next, Prefix),
        case More of
            true -> main(Next, Prefix, Left, Dir, Meta, Opts, Taken);
            false -> {ok, Taken}
        end
    end.

%% @doc The keys of one main leaf from a slot onward in the direction
%% requested that begin with the prefix, with the limit that remains. A key
%% that diverges from the prefix ends the read: keys are sorted.
gather(_Leaf, Slot, Count, _Prefix, _Dir, Limit, _Meta, Keys)
        when Slot < 0; Slot >= Count; Limit =:= 0 ->
    {ok, {Keys, Limit}};
gather(Leaf, Slot, Count, Prefix, Dir, Limit, Meta, Keys) ->
    maybe
        {ok, Key} ?= main_key(node(Leaf, Slot), Meta),
        case begins_with(Key, Prefix) of
            true ->
                gather(
                    Leaf, Slot + stride(Dir), Count, Prefix, Dir,
                    spend(Limit), Meta, [Key | Keys]
                );
            false ->
                {ok, {Keys, 0}}
        end
    end.

%% @doc A main node's key, required to be of the file's key width: nothing
%% where the file holds one key, which requests leave out.
main_key(_Node, #{ <<"key-size">> := 0 }) ->
    {ok, <<>>};
main_key(#{ key := Key }, #{ <<"key-size">> := KeySize })
        when byte_size(Key) =:= KeySize ->
    {ok, Key};
main_key(#{ ksize := KSize }, _Meta) ->
    {error, {'invalid-key-size', KSize}}.

%% @doc A key's duplicate set: the sole node's where the file holds one
%% key, and the node the main tree holds for the key otherwise. A key the
%% tree lacks is not found.
set_of(_Key, Meta = #{ <<"key-size">> := 0 }, Opts) ->
    maybe
        {ok, {Page, 0, 1, none}} ?= single_leaf(Meta, Opts),
        dup_set(Page, node(Page, 0), Meta)
    end;
set_of(Key, Meta, Opts) ->
    #{ <<"main-root">> := Root, <<"main-depth">> := Depth } = Meta,
    maybe
        {ok, {Leaf, Slot, Count, _Bound}} ?=
            descend(Root, Depth, Key, nodes, asc, none, Meta, Opts),
        true ?= Slot < Count orelse {error, not_found},
        Node = node(Leaf, Slot),
        true ?= maps:get(key, Node) =:= Key orelse {error, not_found},
        dup_set(Leaf, Node, Meta)
    end.

%%% Reading rows.

%% @doc The rows beginning with the prefix, from the row the prefix
%% followed by the `from' bits gives -- the prefix's own end when `from' is
%% absent -- in the direction requested, up to the limit. The main keys
%% beginning with the prefix's key part are visited in that direction from
%% the target's key, and of each the values beginning with its value part
%% are collected: from the target's value for the target's key, and whole
%% for every key after it.
rows(Prefix, From, Limit, Dir, Meta, Opts) ->
    #{ <<"key-size">> := KeySize, <<"value-size">> := ValueSize } = Meta,
    RowBits = (KeySize + ValueSize) * 8,
    maybe
        {ok, Seek} ?= seek(Prefix, From),
        true ?=
            bit_size(Seek) =< RowBits
                orelse {error, {'invalid-key-size', bit_size(Seek)}},
        <<TargetKey:KeySize/binary, TargetValue:ValueSize/binary>> =
            pad(Dir, Seek, RowBits),
        {KeyPrefix, ValuePrefix} = split_prefix(Prefix, KeySize * 8),
        Range =
            #{
                dir => Dir,
                key_prefix => KeyPrefix,
                value_prefix => ValuePrefix,
                target => {TargetKey, TargetValue}
            },
        {ok, Start} ?= main_leaf(TargetKey, Dir, Meta, Opts),
        {ok, Rows} ?= fold_keys(Start, Range, Limit, Meta, Opts, []),
        {ok, lists:reverse(Rows)}
    end.

%% @doc The row sought: the prefix, continued by the `from' bits.
seek(Prefix, none) -> {ok, Prefix};
seek(Prefix, From) when is_bitstring(From) ->
    {ok, <<Prefix/bitstring, From/bitstring>>};
seek(_Prefix, From) -> {error, {'invalid-from', From}}.

%% @doc The main leaf holding the target key's node, with the slot to start
%% from: the sole node of a one-key file's root, and the leaf the descent
%% reaches otherwise.
main_leaf(_Target, _Dir, Meta = #{ <<"key-size">> := 0 }, Opts) ->
    single_leaf(Meta, Opts);
main_leaf(Target, Dir, Meta, Opts) ->
    #{ <<"main-root">> := Root, <<"main-depth">> := Depth } = Meta,
    descend(Root, Depth, Target, nodes, Dir, none, Meta, Opts).

%% @doc The one node of a one-key file's root.
single_leaf(Meta = #{ <<"main-root">> := Root }, Opts) ->
    maybe
        {ok, Page} ?= read_page(Root, Meta, Opts),
        {ok, 1} ?= single_root(parse_page(Page)),
        {ok, {Page, 0, 1, none}}
    end.

%% @doc Fold over the main keys from a leaf and slot: each key beginning
%% with the key prefix, in the direction requested, until the limit is
%% spent or no key remains. Crossing a main leaf re-descends at the
%% boundary key passed down.
fold_keys({_Leaf, Slot, Count, Bound}, Range, Limit, Meta, Opts, Rows)
        when Slot < 0; Slot >= Count ->
    #{ dir := Dir, key_prefix := KeyPrefix } = Range,
    Next = beyond(Dir, Bound),
    case Next =/= none andalso begins_with(Next, KeyPrefix) of
        true ->
            maybe
                {ok, Start} ?= main_leaf(Next, Dir, Meta, Opts),
                fold_keys(Start, Range, Limit, Meta, Opts, Rows)
            end;
        false ->
            {ok, Rows}
    end;
fold_keys({Leaf, Slot, Count, Bound}, Range, Limit, Meta, Opts, Rows) ->
    #{ dir := Dir, key_prefix := KeyPrefix, value_prefix := ValuePrefix } =
        Range,
    #{ target := {TargetKey, TargetValue} } = Range,
    Node = node(Leaf, Slot),
    maybe
        {ok, Key} ?= main_key(Node, Meta),
        % A key past the prefix ends the read: keys are sorted.
        true ?= begins_with(Key, KeyPrefix) orelse {ok, Rows},
        {ok, Set} ?= dup_set(Leaf, Node, Meta),
        Target =
            case Key of
                TargetKey -> TargetValue;
                _ -> pad(Dir, <<>>, maps:get(<<"value-size">>, Meta) * 8)
            end,
        {ok, Values, Left} ?=
            scan(Set, Target, ValuePrefix, Limit, Dir, Meta, Opts, []),
        Taken = [ <<Key/binary, Value/binary>> || Value <- Values ] ++ Rows,
        case Left of
            0 ->
                {ok, Taken};
            _ ->
                Next = {Leaf, Slot + stride(Dir), Count, Bound},
                fold_keys(Next, Range, Left, Meta, Opts, Taken)
        end
    end.

%% @doc Split prefix bits into the part within a row's key and the part
%% within its value: a prefix within the key's width is key bits alone.
split_prefix(Prefix, KeyBits) ->
    case Prefix of
        <<KeyPrefix:KeyBits/bitstring, ValuePrefix/bitstring>> ->
            {KeyPrefix, ValuePrefix};
        _ ->
            {Prefix, <<>>}
    end.

%%% Reading a duplicate set.

%% @doc The values of a key's set beginning with the prefix, from the row
%% `from' gives -- the prefix's own end when `from' is absent -- in the
%% direction requested, up to the limit.
values(Key, Prefix, From, Limit, Dir, Meta, Opts) ->
    #{ <<"value-size">> := Pad } = Meta,
    maybe
        {ok, Set} ?= set_of(Key, Meta, Opts),
        {ok, Target} ?= target(From, Prefix, Dir, Pad * 8),
        {ok, Values, _Left} ?=
            scan(Set, Target, Prefix, Limit, Dir, Meta, Opts, []),
        {ok, lists:reverse(Values)}
    end.

%% @doc Collect a set's values beginning with the prefix from the target,
%% in the direction requested, up to the limit, onto a reversed
%% accumulator, with the limit left. Crossing
%% a leaf re-seeks at the boundary the descent passes down: the pages
%% above the leaves are retained, so a crossing costs one leaf fetch.
scan(Set, Target, Prefix, Limit, Dir, Meta, Opts, Values) ->
    #{ <<"value-size">> := Pad } = Meta,
    maybe
        {ok, {Leaf, Slot, Count, Bound}} ?=
            position(Set, Target, Dir, Pad, Meta, Opts),
        {Taken, Collected} =
            collect(Leaf, Slot, Count, Pad, Prefix, Dir, Limit, Values),
        Left = spent(Collected, Target, Taken),
        Next = beyond(Dir, Bound),
        More =
            Left =/= 0 andalso Next =/= none
                andalso begins_with(Next, Prefix),
        case More of
            true -> scan(Set, Next, Prefix, Left, Dir, Meta, Opts, Taken);
            false -> {ok, Taken, Left}
        end
    end.

%% @doc The values of one fixed-width leaf from a slot onward in the
%% direction requested that begin with the prefix, with the limit that
%% remains. A value that diverges from the prefix ends the read: values are
%% sorted.
collect(_Leaf, Slot, Count, _Pad, _Prefix, _Dir, Limit, Values)
        when Slot < 0; Slot >= Count; Limit =:= 0 ->
    {Values, Limit};
collect(Leaf, Slot, Count, Pad, Prefix, Dir, Limit, Values) ->
    Value = row(Leaf, Slot, Pad),
    case begins_with(Value, Prefix) of
        true ->
            collect(
                Leaf, Slot + stride(Dir), Count, Pad, Prefix, Dir,
                spend(Limit), [Value | Values]
            );
        false ->
            {Values, 0}
    end.

%% @doc The limit left once a leaf is collected: a batch is spent by the
%% leaf that yields a row past the one the list starts from, as the caller
%% learns nothing from one that does not.
spent(batch, Start, [Row | _]) when Row =/= Start -> 0;
spent(Left, _Start, _Rows) -> Left.

%% @doc The limit left after taking a row: `all' and a batch are never
%% spent by one.
spend(Limit) when is_atom(Limit) -> Limit;
spend(Limit) -> Limit - 1.

%% @doc The row a list starts from, padded to the row width with the
%% direction's extreme: the `from' bits when given, the prefix otherwise.
target(none, Prefix, Dir, Width) ->
    {ok, pad(Dir, Prefix, Width)};
target(From, _Prefix, Dir, Width)
        when is_bitstring(From), bit_size(From) =< Width ->
    {ok, pad(Dir, From, Width)};
target(From, _Prefix, _Dir, _Width) ->
    {error, {'invalid-from', From}}.

%% @doc Pad bits to a width with the direction's extreme: zeros ascending,
%% the least row beginning with the bits, and ones descending, the
%% greatest.
pad(asc, Bits, Width) -> <<Bits/bitstring, 0:(Width - bit_size(Bits))>>;
pad(desc, Bits, Width) -> <<Bits/bitstring, -1:(Width - bit_size(Bits))>>.

%% @doc Position on a duplicate set's values around the target: the
%% fixed-width leaf holding them, the starting slot in it, its value count,
%% and the boundary key passed down for the leaf beyond it -- `none' for a
%% set held in one leaf. A single value is read as a one-value leaf, and a
%% sub-page as the leaf its image is.
position({item, Item}, Target, Dir, Pad, _Meta, _Opts) ->
    Leaf = <<0:(?PAGE_HDR * 8), Item/binary>>,
    {ok, {Leaf, leaf_slot(Dir, Leaf, Pad, 1, Target), 1, none}};
position({page, Image}, Target, Dir, Pad, _Meta, _Opts) ->
    {_Flags, Count} = parse_page(Image),
    {ok, {Image, leaf_slot(Dir, Image, Pad, Count, Target), Count, none}};
position({tree, Tree}, Target, Dir, Pad, Meta, Opts) ->
    #{ root := Root, depth := Depth } = Tree,
    descend(Root, Depth, Target, Pad, Dir, none, Meta, Opts).

%%% Locating and validating the file.

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

%% @doc Read both meta pages and validate the file, returning the
%% instance the meta written by the most recent transaction describes. The
%% page size is learned from the first meta page, which is what says where
%% the second begins.
read_meta(Start, Size, Opts) ->
    maybe
        {ok, Head} ?=
            fetch(Start, Size, 0, min(Size, 2 * ?MAX_PAGE_SIZE), Opts),
        {ok, Meta0 = #{ page_size := PageSize }} ?= parse_meta(Head),
        true ?=
            valid_page_size(PageSize)
                orelse {error, {'invalid-page-size', PageSize}},
        true ?=
            Size >= 2 * PageSize
                orelse {error, {'invalid-file-size', Size}},
        {ok, Meta1} ?= parse_meta(binary:part(Head, PageSize, PageSize)),
        instance(newest_meta(Meta0, Meta1), Start, Size)
    end.

%% @doc Parse a meta page: the `MDB_meta' record follows the page header,
%% and the free database's `pad' holds the page size.
parse_meta(
    <<
        _:?PAGE_HDR/binary,
        ?MDB_MAGIC:32/little, ?MDB_VERSION:32/little,
        _Address:64/little, _MapSize:64/little,
        PageSize:32/little, _:(?DB_SIZE - 4)/binary,
        Main:?DB_SIZE/binary,
        LastPage:64/little, TxnID:64/little,
        _/binary
    >>
) ->
    {ok, #{
        page_size => PageSize,
        main => parse_db(Main),
        last_page => LastPage,
        txn => TxnID
    }};
parse_meta(
    <<_:?PAGE_HDR/binary, Magic:32/little, Version:32/little, _/binary>>
) ->
    {error, {'invalid-meta', {magic, Magic}, {version, Version}}};
parse_meta(Short) ->
    {error, {'invalid-meta', {size, byte_size(Short)}}}.

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

%% @doc Whether a page size is one LMDB can have used.
valid_page_size(PageSize) ->
    PageSize >= ?MIN_PAGE_SIZE andalso PageSize =< ?MAX_PAGE_SIZE
        andalso PageSize band (PageSize - 1) =:= 0.

%% @doc Choose the meta page written by the most recent transaction.
newest_meta(Meta0 = #{ txn := Txn0 }, #{ txn := Txn1 }) when Txn0 >= Txn1 ->
    Meta0;
newest_meta(_Meta0, Meta1) ->
    Meta1.

%% @doc The store instance a meta page describes, once the file
%% invariants this store implements hold: its weave location, page size,
%% page count, and the root and depth of its main tree.
instance(#{ page_size := PageSize, main := Main, last_page := LastPage },
        Start, Size) ->
    #{ flags := Flags, depth := Depth, root := Root } = Main,
    maybe
        true ?=
            Flags =:= 0 orelse Flags =:= ?MAIN_DB_FLAGS
                orelse {error, {'invalid-main-flags', Flags}},
        true ?=
            (LastPage + 1) * PageSize =< Size
                orelse {error, {'invalid-last-page', LastPage}},
        true ?=
            Depth > 0 andalso Root =< LastPage
                orelse {error, {'invalid-main-depth', Depth}},
        {ok,
            #{
                <<"start">> => Start,
                <<"size">> => Size,
                <<"page-size">> => PageSize,
                <<"last-page">> => LastPage,
                <<"main-flags">> => Flags,
                <<"main-root">> => Root,
                <<"main-depth">> => Depth
            }}
    end.

%% @doc Learn the file's layout from its main tree. A main database in the
%% default mode holds paths. In DUP mode, a root that is one leaf holding
%% one key is a one-key file, whose requests leave the key out: its key
%% width is zero. Any other root's key width is that of the first node of
%% its leftmost leaf. The value width is that of the first node's duplicate
%% set in either case, and every node read thereafter must match both.
read_layout(#{ <<"main-flags">> := 0 }, _Opts) ->
    {ok, #{ <<"mode">> => default }};
read_layout(Meta, Opts) ->
    #{ <<"main-root">> := Root, <<"main-depth">> := Depth } = Meta,
    #{ <<"page-size">> := PageSize } = Meta,
    maybe
        {ok, Page} ?= read_page(Root, Meta, Opts),
        {ok, Leaf, Node} ?= first_node(Page, Depth, Meta, Opts),
        {ok, Set} ?= dup_set(Leaf, Node),
        Width = set_width(Set),
        true ?=
            Width > 0 andalso ?PAGE_HDR + Width =< PageSize
                orelse {error, {'invalid-row-width', Width}},
        {ok, KeySize} ?= key_size(parse_page(Page), Node),
        {ok, #{
            <<"mode">> => dup,
            <<"key-size">> => KeySize,
            <<"value-size">> => Width
        }}
    end.

%% @doc The first node of the leftmost leaf beneath a page, with its leaf.
first_node(Page, Depth, Meta, Opts) ->
    case parse_page(Page) of
        {?P_BRANCH, Count} when Count > 0, Depth > 1 ->
            maybe
                {ok, Child} ?= read_page(child(node(Page, 0)), Meta, Opts),
                first_node(Child, Depth - 1, Meta, Opts)
            end;
        {?P_LEAF, Count} when Count > 0 ->
            {ok, Page, node(Page, 0)};
        {Flags, Count} ->
            {error, {'invalid-main-page', {flags, Flags}, {count, Count}}}
    end.

%% @doc The width of the file's keys: zero for the sole node of a one-leaf
%% root, which requests leave out, and the first node's own key width
%% otherwise.
key_size({?P_LEAF, 1}, _Node) -> {ok, 0};
key_size(_Root, #{ ksize := KSize }) when KSize > 0 -> {ok, KSize};
key_size(_Root, #{ ksize := KSize }) -> {error, {'invalid-key-size', KSize}}.

%% @doc Require a one-key file's root to remain an ordinary leaf with a
%% single node.
single_root({?P_LEAF, 1}) -> {ok, 1};
single_root({?P_LEAF, Count}) -> {error, {'invalid-main-entries', Count}};
single_root({Flags, _}) -> {error, {'invalid-main-page-flags', Flags}}.

%% @doc A main node's duplicate set, in the form the node holds it, requiring
%% its item width to be the file's.
dup_set(Leaf, Node, #{ <<"value-size">> := Width }) ->
    maybe
        {ok, Set} ?= dup_set(Leaf, Node),
        true ?=
            set_width(Set) =:= Width
                orelse {error, {'invalid-row-width', set_width(Set)}},
        {ok, Set}
    end.

%% @doc A main node's duplicate set: the single item in its data, the
%% fixed-width sub-page image in its data, or the record of the promoted
%% sub-database in its data, by the node's flags.
dup_set(Leaf, Node = #{ flags := Flags }) ->
    maybe
        {ok, Data} ?= node_data(Leaf, Node),
        case Flags of
            0 -> {ok, {item, Data}};
            ?F_DUPDATA -> sub_page(Data);
            ?F_DUPTREE -> sub_tree(Data);
            _ -> {error, {'invalid-node-flags', Flags}}
        end
    end.

%% @doc A sub-page: a whole fixed-width page image in a node's data, whose
%% header holds the item width as `pad'. Its item count is checked against
%% its own length, as a leaf's is against the page size.
sub_page(
    Image =
        <<
            _:16/binary, Pad:16/little, Flags:16/little,
            Lower:16/little, _Upper:16/little, _/binary
        >>
) ->
    Count = Lower bsr 1,
    maybe
        true ?=
            Flags =:= (?P_LEAF bor ?P_LEAF2 bor ?P_SUBP)
                orelse {error, {'invalid-page-flags', Flags}},
        true ?=
            Pad > 0 andalso ?PAGE_HDR + (Count * Pad) =< byte_size(Image)
                orelse {error, {'invalid-leaf-count', Count}},
        {ok, {page, Image}}
    end;
sub_page(Short) ->
    {error, {'invalid-sub-page', byte_size(Short)}}.

%% @doc A promoted sub-database: the 48 byte `MDB_db' record in a node's
%% data, whose `pad' is the item width.
sub_tree(<<DB:?DB_SIZE/binary>>) -> {ok, {tree, parse_db(DB)}};
sub_tree(Data) -> {error, {'invalid-sub-db', byte_size(Data)}}.

%% @doc The item width a duplicate set records.
set_width({item, Item}) -> byte_size(Item);
set_width({page, <<_:16/binary, Pad:16/little, _/binary>>}) -> Pad;
set_width({tree, #{ pad := Pad }}) -> Pad.

%% @doc Whether bits begin with the given prefix bits.
begins_with(Bits, Prefix) ->
    PrefixSize = bit_size(Prefix),
    case Bits of
        <<Lead:PrefixSize/bitstring, _/bitstring>> -> Lead =:= Prefix;
        _ -> false
    end.

%% @doc The step between slots in the direction requested.
stride(asc) -> 1;
stride(desc) -> -1.

%% @doc The target that re-descends onto the leaf beyond a boundary: the
%% boundary key itself ascending, and the greatest key under it descending;
%% `none' where no boundary was carried, or under the least key.
beyond(_Dir, none) -> none;
beyond(asc, Bound) -> Bound;
beyond(desc, Bound) ->
    Bits = bit_size(Bound),
    case Bound of
        <<0:Bits>> -> none;
        <<Int:Bits>> -> <<(Int - 1):Bits>>
    end.

%%% Descending a tree.

%% @doc Take one step of the descent to the leaf that would hold the target.
%% Branch pages recurse into the last child whose key is at-or-under it, with
%% the first node standing for the least key. The `Leaf' argument gives the
%% leaf form expected: the item width of a fixed-width tree, or `nodes' for
%% the main tree.
descend(PgNo, Depth, Target, Leaf, Dir, Bound, Meta, Opts) ->
    maybe
        true ?= Depth > 0 orelse {error, {'invalid-depth', PgNo}},
        {ok, Page} ?= read_page(PgNo, Meta, Opts),
        step(
            parse_page(Page), Page, Depth, Target, Leaf, Dir, Bound, Meta,
            Opts
        )
    end.

%% @doc Recurse through a branch, reach a leaf of the expected form, and
%% refuse every other page kind. The result is the leaf, the starting slot
%% in it, its slot count, and the boundary key passed down.
step({?P_BRANCH, Count}, Page, Depth, Target, Leaf, Dir, Bound, Meta, Opts) ->
    Slot = branch_slot(Page, 1, Count, Target, 0),
    NewBound = bound(Dir, Page, Slot, Count, Bound),
    descend(
        child(node(Page, Slot)), Depth - 1, Target, Leaf, Dir, NewBound,
        Meta, Opts
    );
step({Flags, Count}, Page, _Depth, Target, Pad, Dir, Bound, _Meta, _Opts)
        when Flags =:= (?P_LEAF bor ?P_LEAF2), is_integer(Pad) ->
    maybe
        true ?=
            ?PAGE_HDR + (Count * Pad) =< byte_size(Page)
                orelse {error, {'invalid-leaf-count', Count}},
        {ok, {Page, leaf_slot(Dir, Page, Pad, Count, Target), Count, Bound}}
    end;
step({?P_LEAF, Count}, Page, _Depth, Target, nodes, Dir, Bound, _Meta, _Opts) ->
    {ok, {Page, node_slot(Dir, Page, Count, Target), Count, Bound}};
step({Flags, _}, _Page, _Depth, _Target, _Leaf, _Dir, _Bound, _Meta, _Opts) ->
    {error, {'invalid-page-flags', Flags}}.

%% @doc The boundary key carried past a branch. Ascending, the key of the
%% node after the one taken -- the least key beyond the taken subtree --
%% when there is one; descending, the key of the node taken -- at-or-under
%% every key in its subtree, and over every key before it -- unless it is
%% the first. Otherwise the boundary carried from above stands.
bound(asc, Page, Slot, Count, _Bound) when Slot + 1 < Count ->
    maps:get(key, node(Page, Slot + 1));
bound(desc, Page, Slot, _Count, _Bound) when Slot > 0 ->
    maps:get(key, node(Page, Slot));
bound(_Dir, _Page, _Slot, _Count, Bound) ->
    Bound.

%% @doc Find the last branch slot whose key is at-or-under the target. Keys
%% ascend, so the scan stops at the first key over it.
branch_slot(_Page, Slot, Count, _Target, Best) when Slot >= Count -> Best;
branch_slot(Page, Slot, Count, Target, Best) ->
    case maps:get(key, node(Page, Slot)) =< Target of
        true -> branch_slot(Page, Slot + 1, Count, Target, Slot);
        false -> Best
    end.

%% @doc The starting slot in a fixed-width leaf, searched over its
%% rows.
leaf_slot(Dir, Page, Pad, Count, Target) ->
    start_slot(Dir, fun(Slot) -> row(Page, Slot, Pad) end, Count, Target).

%% @doc The starting slot in a node leaf, searched over its keys.
node_slot(Dir, Page, Count, Target) ->
    start_slot(
        Dir,
        fun(Slot) -> maps:get(key, node(Page, Slot)) end,
        Count,
        Target
    ).

%% @doc Binary-search a leaf for the starting slot, reading its keys
%% through the given function. Ascending, the first slot at-or-over the
%% target, or the count when every key is under it; descending, the last
%% slot at-or-under the target, or -1 when every key is over it.
start_slot(asc, Read, Count, Target) ->
    first_over(Read, 0, Count, Target, asc);
start_slot(desc, Read, Count, Target) ->
    first_over(Read, 0, Count, Target, desc) - 1.

%% @doc The first slot whose key is at-or-over the target ascending, and
%% strictly over it descending; the high bound when there is none.
first_over(_Read, Low, High, _Target, _Dir) when Low >= High -> Low;
first_over(Read, Low, High, Target, Dir) ->
    Mid = (Low + High) div 2,
    Key = Read(Mid),
    case Key < Target orelse (Dir =:= desc andalso Key =:= Target) of
        true -> first_over(Read, Mid + 1, High, Target, Dir);
        false -> first_over(Read, Low, Mid, Target, Dir)
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

%% @doc A leaf node's data: the bytes after its key, rounded up to even
%% length, of the size its header records.
node_data(Page, #{ offset := Offset, ksize := KSize, lo := Lo, hi := Hi }) ->
    DataOffset = Offset + ?NODE_HDR + KSize + (KSize band 1),
    Size = Lo bor (Hi bsl 16),
    case Page of
        <<_:DataOffset/binary, Data:Size/binary, _/binary>> -> {ok, Data};
        _ -> {error, {'invalid-node-size', Size}}
    end.

%% @doc A branch node's child page number.
child(#{ lo := Lo, hi := Hi, flags := Flags }) ->
    Lo bor (Hi bsl 16) bor (Flags bsl 32).

%%% Fetching the file.

%% @doc Fetch a page by number, refusing page numbers outside the file.
read_page(PgNo, Meta, Opts) ->
    #{ <<"page-size">> := PageSize, <<"last-page">> := LastPage } = Meta,
    #{ <<"start">> := Start, <<"size">> := Size } = Meta,
    maybe
        true ?=
            PgNo =< LastPage andalso (PgNo + 1) * PageSize =< Size
                orelse {error, {'invalid-page-number', PgNo}},
        fetch(Start, Size, PgNo * PageSize, PageSize, Opts)
    end.

%% @doc Fetch a byte range of the file, sliced from the chunk that holds
%% it. Pages align within the file's chunks, so a range never spans two.
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

%% @doc One whole chunk of the file, from the chunk store when it is
%% held, and from the weave -- retained for the next read -- when it is not.
read_chunk(Start, Size, Chunk, Opts) ->
    Stores = chunk_store(Opts),
    % The key is the `~arweave@2.9' device's own address for the chunk: the
    % 1-based absolute weave offset of its first byte, so every store and
    % device retaining weave chunks shares one namespace.
    Key =
        <<
            "~arweave@2.9/chunk=",
            (hb_util:bin(Start + (Chunk * ?CHUNK_SIZE) + 1))/binary
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
%% the store definition. The default is a volatile store named after the
%% root, expiring wholesale every five minutes; `[]' retains nothing.
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

%% A live offset index: `~arweave@2.9/offset=<id>' keys to the weave
%% locations of ANS-104 data items. One key, whose values are 160 bits: the
%% leading 77 bits of an item's ID, its start offset and its length.
-define(OFFSET_INDEX, <<"7vg2832WFsisEcBr1oBQ8ldc4EGOkjQdwW46hDvJsOs">>).
%% A mined LMDB file that predates the published index format.
-define(OLD_CONTAINER, <<"b159UDeD87YEFujWBMM8bISZ8DL8Wm1jLa-Bs_LQGAw">>).
%% A published `hb_store_lmdb' layout: an LMDB 1.0 file of 4 KiB pages
%% holding groups, links, a value on overflow pages and a child beside its
%% own subtree,
%% posted for these tests.
-define(PATHS_INDEX, <<"aOLp1k7Rbatt0FRq6LBCJtvjLrKBGNfOo5qZs66gGuw">>).
%% A mined transaction whose data is not an LMDB file.
-define(NOT_LMDB, <<"ptBC0UwDmrUTBQX3MqZ1lB57ex20ygwzkjjCrQjIx3o">>).
%% A published `~match@1.0' index: 10-byte keys -- a predicate's 39-bit name
%% hash, 40-bit value hash and a pad bit -- each holding a duplicate set of
%% 6-byte offsets.
-define(MATCH_INDEX, <<"oWRzBr3KHhULAL-s5ULeXac1mb_WQOX5uFBRea16iRI">>).

%% @doc A store definition for the offset index: an offset key's ID is
%% decoded and cut to its leading 77 bits, and a value's trailing fields
%% are read out as the offset message. The instance registry looks stores
%% up by name, so the root doubles as the name and instances persist across
%% tests.
test_store() ->
    #{
        <<"store-module">> => hb_store_arlmdb,
        <<"name">> => ?OFFSET_INDEX,
        <<"root">> => ?OFFSET_INDEX,
        <<"prefix">> => <<"~arweave@2.9/offset=">>,
        <<"to-key">> => <<"~base64url@1.0/decode/~bits@1.0/take=77">>,
        <<"from-value">> =>
            <<"~bits@1.0/from=_:77,start:49+integer,length:34+integer">>
    }.

%% @doc The offset index resolves indexed data item IDs to their weave
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

%% @doc A mined transaction that is not an LMDB file is refused loudly
%% at start, never silently skipped.
invalid_container_test() ->
    Store =
        #{
            <<"store-module">> => hb_store_arlmdb,
            <<"name">> => ?NOT_LMDB,
            <<"root">> => ?NOT_LMDB
        },
    {error, Reason} = hb_store:start([Store]),
    ?assertEqual('invalid-meta', element(1, Reason)).

%% @doc Retained chunks serve repeated reads without the weave: a key looked
%% up once answers again through a store whose routes are gone, while an
%% indexed key whose leaf chunk was never fetched cannot. The second key's
%% unavailability reaches the store manager, which reports an exhausted
%% store list as its terminal miss.
chunk_retention_test_() ->
    {timeout, 120, fun chunk_retention/0}.
chunk_retention() ->
    Store =
        (test_store())#{ <<"name">> => <<?OFFSET_INDEX/binary, "-cached">> },
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

%% @doc The offset index holds one key, so its requests are value bits. A
%% list serves the values beginning with a key's bits: a full ID's is its
%% read, and keys outside the index's prefix fall through. Mounted raw, the
%% values resume inclusively from a value in either direction and answer
%% past the last with the empty list rather than a miss; a read of the bits
%% is the value itself; and the values list from the file's own start.
list_values_test_() ->
    {timeout, 120, fun list_values/0}.
list_values() ->
    Store = test_store(),
    ID = <<"AAAAhyV8_NwududSxuraAj7DLWiZHDTqVKWrZglpNok">>,
    Key = <<"~arweave@2.9/offset=", ID/binary>>,
    <<Prefix:77/bitstring, _/bitstring>> = hb_util:native_id(ID),
    Row = <<Prefix/bitstring, 381852134215637:49, 3947:34>>,
    ?assertEqual({ok, [Row]}, hb_store:list([Store], Key, #{})),
    ?assertEqual(
        {error, not_found},
        hb_store:list([Store], <<"other/key">>, #{})
    ),
    Raw = [maps:without([<<"prefix">>, <<"to-key">>, <<"from-value">>], Store)],
    List =
        fun(Req) -> hb_store:list(Raw, Req#{ <<"list">> => Prefix }, #{}) end,
    ?assertEqual({ok, [Row]}, List(#{ <<"limit">> => 5 })),
    ?assertEqual({ok, [Row]}, List(#{ <<"from">> => Row })),
    ?assertEqual(
        {ok, [Row]},
        List(#{ <<"from">> => Row, <<"direction">> => desc })
    ),
    ?assertEqual(
        {ok, []},
        List(#{ <<"from">> => <<Prefix/bitstring, 381852134215638:49, 0:34>> })
    ),
    ?assertEqual({ok, Row}, hb_store:read(Raw, #{ <<"read">> => Prefix }, #{})),
    ?assertEqual(
        {ok, simple},
        hb_store:type(Raw, #{ <<"type">> => Prefix }, #{})
    ),
    {ok, Head} =
        hb_store:list(Raw, #{ <<"list">> => <<>>, <<"limit">> => 3 }, #{}),
    ?assertEqual(3, length(Head)),
    ?assertEqual(lists:sort(Head), Head).

%% @doc The match index mounted raw, under a fresh name and with a
%% retained-chunk store of its own, so that its chunks are the test's own.
published_store() ->
    #{
        <<"store-module">> => hb_store_arlmdb,
        <<"name">> =>
            <<
                "arlmdb-published-",
                (hb_util:encode(crypto:strong_rand_bytes(4)))/binary
            >>,
        <<"root">> => ?MATCH_INDEX,
        <<"chunk-store">> => hb_test_utils:test_store(hb_store_volatile)
    }.

%% @doc The 79 bits of a predicate's rows: the leading bits of the SHA-256
%% of its name under `~match@1.0/' and of its value.
predicate(Name, Value) ->
    <<KeyHash:39/bitstring, _/bitstring>> =
        crypto:hash(sha256, <<"~match@1.0/", Name/binary>>),
    <<ValueHash:40/bitstring, _/bitstring>> = crypto:hash(sha256, Value),
    <<KeyHash/bitstring, ValueHash/bitstring>>.

%% @doc The match index as rows, as its definition asks: a
%% predicate's rows list whole and sorted under its 79 bits, mirror
%% descending, resume inclusively from an offset in either direction and
%% stop at a limit or as a batch; a large predicate's rows span leaves; a
%% read finds a predicate's first row; and a predicate without rows lists
%% none.
published_rows_test_() ->
    {timeout, 600, fun published_rows/0}.
published_rows() ->
    Store = [(published_store())#{ <<"return-row">> => true }],
    ok = hb_store:start(Store),
    Small = predicate(<<"battleid">>, <<"b2">>),
    List =
        fun(Prefix, Req) ->
            {ok, Rows} =
                hb_store:list(Store, Req#{ <<"list">> => Prefix }, #{}),
            Rows
        end,
    Rows = List(Small, #{}),
    ?assertEqual(20, length(Rows)),
    ?assertEqual(lists:sort(Rows), Rows),
    ?assertEqual(
        lists:reverse(Rows),
        List(Small, #{ <<"direction">> => desc })
    ),
    {Before, [Middle | After]} = lists:split(9, Rows),
    <<Small:79/bitstring, Offset:49>> = Middle,
    ?assertEqual(
        [Middle | After],
        List(Small, #{ <<"from">> => <<Offset:49>> })
    ),
    ?assertEqual(
        [Middle | lists:reverse(Before)],
        List(Small, #{ <<"from">> => <<Offset:49>>, <<"direction">> => desc })
    ),
    ?assertEqual(
        lists:sublist(After, 3),
        List(Small, #{ <<"from">> => <<(Offset + 1):49>>, <<"limit">> => 3 })
    ),
    ?assertEqual(
        After,
        List(
            Small,
            #{ <<"from">> => <<(Offset + 1):49>>, <<"limit">> => batch }
        )
    ),
    ?assertEqual(
        {ok, hd(Rows)},
        hb_store:read(Store, #{ <<"read">> => Small }, #{})
    ),
    ?assertEqual(
        {ok, simple},
        hb_store:type(Store, #{ <<"type">> => Small }, #{})
    ),
    Large = predicate(<<"action">>, <<"Battle.Info">>),
    Page = List(Large, #{ <<"limit">> => 600 }),
    ?assertEqual(600, length(Page)),
    ?assertEqual(lists:sort(Page), Page),
    Last = List(Large, #{ <<"limit">> => 600, <<"direction">> => desc }),
    ?assertEqual(lists:reverse(lists:sort(Last)), Last),
    ?assert(hd(Last) > lists:last(Page)),
    % A batch from a row within a leaf is the rest of that leaf: rows past
    % the row, at most a leaf's worth, from one fetch.
    <<Large:79/bitstring, Within:49>> = lists:nth(300, Page),
    Batch = List(Large, #{ <<"from">> => <<Within:49>>, <<"limit">> => batch }),
    ?assertEqual(lists:nth(300, Page), hd(Batch)),
    ?assertEqual(lists:sort(Batch), Batch),
    ?assert(length(Batch) > 1 andalso length(Batch) =< 700),
    ?assertEqual(
        [],
        List(predicate(<<"battleid">>, <<"no-such-battle">>), #{})
    ).

%% @doc The match index as keys and values: the keys list from the file's
%% start and under a name's hash; a predicate's
%% key holds its rows' offsets as 48-bit values, listed whole, from a
%% value, and to a limit; a key reads its first value and a key with a
%% value that value; a key is composite and a value simple; and a key the
%% index lacks is not found.
published_keys_test_() ->
    {timeout, 600, fun published_keys/0}.
published_keys() ->
    Store = [published_store()],
    ok = hb_store:start(Store),
    List =
        fun(Bits, Req) ->
            {ok, Rows} = hb_store:list(Store, Req#{ <<"list">> => Bits }, #{}),
            Rows
        end,
    Keys = List(<<>>, #{ <<"limit">> => 5 }),
    ?assertEqual(5, length(Keys)),
    ?assertEqual(lists:sort(Keys), Keys),
    ?assertEqual(10, byte_size(hd(Keys))),
    Small = predicate(<<"battleid">>, <<"b2">>),
    % The rows' 49-bit offsets exceed 2^48, so their high bit ends the key.
    Key = <<Small/bitstring, 1:1>>,
    ?assertEqual([Key], List(Small, #{})),
    <<NameHash:39/bitstring, _/bitstring>> = Small,
    ?assertEqual(
        [Key],
        List(NameHash, #{ <<"from">> => Key, <<"limit">> => 1 })
    ),
    Values = List(Key, #{}),
    ?assertEqual(20, length(Values)),
    ?assertEqual(lists:sort(Values), Values),
    ?assertEqual(6, byte_size(hd(Values))),
    {_Before, [Middle | After]} = lists:split(9, Values),
    ?assertEqual([Middle | After], List(Key, #{ <<"from">> => Middle })),
    ?assertEqual(
        lists:sublist(After, 3),
        List(Key, #{ <<"from">> => Middle, <<"limit">> => 4 }) -- [Middle]
    ),
    % A key with value bits lists the values beginning with them: a whole
    % value is itself alone.
    ?assertEqual([Middle], List(<<Key/bitstring, Middle/binary>>, #{})),
    <<Lead:24/bitstring, _/bitstring>> = Middle,
    Led = List(<<Key/bitstring, Lead/bitstring>>, #{}),
    ?assert(lists:member(Middle, Led)),
    ?assert(lists:all(fun(V) -> begins_with(V, Lead) end, Led)),
    ?assertEqual(
        {ok, hd(Values)},
        hb_store:read(Store, #{ <<"read">> => Key }, #{})
    ),
    ?assertEqual(
        {ok, Middle},
        hb_store:read(
            Store,
            #{ <<"read">> => <<Key/bitstring, Middle/binary>> },
            #{}
        )
    ),
    ?assertEqual(
        {ok, composite},
        hb_store:type(Store, #{ <<"type">> => Key }, #{})
    ),
    ?assertEqual(
        {ok, simple},
        hb_store:type(
            Store,
            #{ <<"type">> => <<Key/bitstring, Middle/binary>> },
            #{}
        )
    ),
    ?assertEqual(
        {error, not_found},
        hb_store:list(Store, #{ <<"list">> => <<Small/bitstring, 0:1>> }, #{})
    ).

%% @doc The published `hb_store_lmdb' layout, under a fresh name and with
%% a retained-chunk store of its own.
paths_store() ->
    #{
        <<"store-module">> => hb_store_arlmdb,
        <<"name">> =>
            <<
                "arlmdb-paths-",
                (hb_util:encode(crypto:strong_rand_bytes(4)))/binary
            >>,
        <<"root">> => ?PATHS_INDEX,
        <<"chunk-store">> => hb_test_utils:test_store(hb_store_volatile)
    }.

%% @doc A published `hb_store_lmdb' layout is served as that store serves
%% its own: a value reads as itself, a link as its target, a link within a
%% path as the target's child, a large value from its overflow pages, and a
%% group as its children's values; a group lists its children, each once
%% and bounded on request, while a value lists none; types and resolution
%% follow links; and an absent path is a miss.
published_paths_test_() ->
    {timeout, 600, fun published_paths/0}.
published_paths() ->
    Store = [paths_store()],
    ok = hb_store:start(Store),
    Read = fun(Path) -> hb_store:read(Store, Path, #{}) end,
    ?assertEqual({ok, <<"value-b">>}, Read(<<"a/b">>)),
    ?assertEqual({ok, <<"value-b">>}, Read(<<"a/link">>)),
    ?assertEqual({ok, <<"value-b">>}, Read(<<"l/b">>)),
    ?assertEqual({ok, <<"deep">>}, Read(<<"l/c/d">>)),
    Big = binary:copy(<<"0123456789abcdef">>, 6400),
    ?assertEqual({ok, Big}, Read(<<"a/big">>)),
    ?assertEqual({ok, <<"raw">>}, Read(<<"data/k">>)),
    ?assertEqual({error, not_found}, Read(<<"nope">>)),
    {composite, Children} = Read(<<"a">>),
    ?assertEqual(
        [<<"b">>, <<"big">>, <<"c">>, <<"link">>],
        lists:sort([ Name || {Name, _Value} <- Children ])
    ),
    ?assertEqual(<<"group">>, proplists:get_value(<<"c">>, Children)),
    List = fun(Req) -> hb_store:list(Store, Req, #{}) end,
    ?assertEqual({ok, [<<"b">>, <<"b-x">>]}, List(<<"z">>)),
    % Only a group lists, as in `hb_store_lmdb': the root holds no marker.
    ?assertEqual({error, not_found}, List(#{ <<"list">> => <<>> })),
    ?assertEqual(
        {ok, [<<"c">>, <<"link">>]},
        List(#{
            <<"list">> => <<"a">>,
            <<"from">> => <<"c">>,
            <<"limit">> => 2
        })
    ),
    ?assertEqual(
        {ok, [<<"c">>, <<"big">>]},
        List(#{
            <<"list">> => <<"a">>,
            <<"from">> => <<"c">>,
            <<"limit">> => 2,
            <<"direction">> => desc
        })
    ),
    ?assertEqual({error, not_found}, List(<<"a/b">>)),
    ?assertEqual({ok, composite}, hb_store:type(Store, <<"a">>, #{})),
    ?assertEqual({ok, simple}, hb_store:type(Store, <<"a/b">>, #{})),
    ?assertEqual({ok, composite}, hb_store:type(Store, <<"l">>, #{})),
    ?assertEqual({ok, <<"a/c/d">>}, hb_store:resolve(Store, <<"l/c/d">>, #{})).
