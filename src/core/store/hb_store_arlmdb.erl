%%% @doc A store that reads an LMDB database out of the Arweave weave.
%%%
%%% `hb_store_arweave' needs a map from data-item ID to the range of the weave
%%% that holds it, and every node builds that map for itself by scanning the
%%% chain. That map is a sorted key-value store, and Arweave can hold sorted
%%% key-value stores: this module reads one where it lies, so a node can answer
%%% from a published index with no index of its own and no warm-up.
%%%
%%% The database is an ordinary LMDB 1.0 file, named by a single locator -- an
%%% Arweave ID, or an explicit `Offset:Length' pair naming a range of the weave.
%%% A lookup walks the B+tree, fetching one page per level through
%%% `hb_store_arweave:read_chunks/3', so it costs the tree's depth plus one
%%% ranged read and touches nothing else: on 64 KiB pages a database of ten
%%% million keys is three levels deep, and four ranged reads answer a lookup.
%%%
%%% `list/3' is the callback the query layer rests on. `hb_cache' writes a
%%% reverse index at `~match@1.0&<key>=<value>/<id>', `dev_match' finds messages
%%% by listing those groups, and `dev_query' sits on top of that -- so a
%%% published database is queryable, not merely readable by ID. `match/3' is
%%% the unindexed fallback, a scan of the whole store, and is not a path this
%%% store serves.
%%%
%%% The store is read-only: every mutating callback returns `{error, not_found}'
%%% so that `hb_store' falls through to the next store in the node's list.
%%%
%%% Configuration:
%%% ```
%%%     root        The locator: an Arweave ID, or `<Offset>:<Length>'.
%%%     max-depth   Bound on the descent. Defaults to the tree's own depth.
%%%     max-value   Refuse values larger than this. Defaults to 16 MiB.
%%% '''
-module(hb_store_arlmdb).
%%% Store API:
-export([start/3, stop/3, scope/0, scope/1, type/3, read/3, resolve/3, list/3]).
%%% Unused Store API:
-export([write/3, link/3, group/3, match/3, reset/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Both meta pages are taken in one ranged read, so that a lookup costs the
%% depth of the tree plus one. Their span is fixed by LMDB's largest page size.
-define(META_SPAN, 2 * 65536).
%% The header that precedes a value on the overflow pages holding it.
-define(PAGE_HEADER_SIZE, 24).
-define(DEFAULT_MAX_VALUE, 16 * 1024 * 1024).
-define(MAX_LINKS, 1000).

%% The pages a store holds after reading them, unless it is given a store of
%% its own or `[]'. A published database is immutable, so the only reason to
%% let a page go is to bound what the node holds: the default resets every ten
%% minutes, and every lookup after a reset pays the top of the tree once more.
-define(DEFAULT_PAGE_STORE,
    [
        #{
            <<"store-module">> => hb_store_volatile,
            <<"name">> => <<"arlmdb-pages">>,
            <<"max-ttl">> => 600
        }
    ]).

%% @doc Start the store by resolving its locator: the range of the weave that
%% holds the database. The data is immutable, so the range is resolved once and
%% carried in the store's instance message for the life of the node.
start(#{ <<"root">> := Root }, _Req, Opts) ->
    case locate(Root, Opts) of
        {ok, Locator} -> {ok, Locator#{ <<"root">> => Root }};
        {error, _} = Error -> Error
    end;
start(_Store, _Req, _Opts) ->
    {error, {badarg, <<"An `arlmdb' store must be given a `root'.">>}}.

%% @doc Nothing is held open on the store's behalf, so there is nothing to stop.
stop(_Store, _Req, _Opts) -> ok.

%% @doc Reads are served from a range of the weave, so the store is remote even
%% though nothing about it is stateful.
scope() -> remote.
scope(#{ <<"scope">> := Scope }) -> Scope;
scope(_) -> scope().

%% @doc Unsupported: the database is a published artefact and cannot be added
%% to in place.
write(_, _, _) -> {error, not_found}.

%% @doc Unsupported.
reset(_, _, _) -> {error, not_found}.

%% @doc Unsupported.
link(_, _, _) -> {error, not_found}.

%% @doc Unsupported.
group(_, _, _) -> {error, not_found}.

%% @doc Unsupported. Messages carrying a given key-value pair are found through
%% the reverse index that `dev_match' lists, not by scanning the store.
match(_, _, _) -> {error, not_found}.

%% @doc Read the value at a key, following link markers. A `group' marker makes
%% the entry composite, and its immediate children come back with it, each
%% carrying the value it holds.
read(Store, #{ <<"read">> := Path }, Opts) ->
    maybe
        {ok, Located, Meta} ?= open(Store, Opts),
        read_entry(Located, Meta, key(Path), Opts)
    end.

%% An element of a sorted set is a key and nothing else, so a member reads back
%% as an empty binary rather than as itself.
read_entry(Store, Meta = #{ <<"width">> := _ }, Key, Opts) ->
    member(Store, Meta, Key, Opts);
read_entry(Store, Meta, Key, Opts) ->
    maybe
        {ok, Resolved, Value} ?= resolved(Store, Meta, Key, Opts),
        case Value of
            <<"group">> -> composite(Store, Meta, Resolved, Opts);
            _ -> {ok, Value}
        end
    end.

%% @doc Classify the entry at a key as a group or a direct value.
type(Store, #{ <<"type">> := Path }, Opts) ->
    maybe
        {ok, Located, Meta} ?= open(Store, Opts),
        type_entry(Located, Meta, key(Path), Opts)
    end.

%% Every element of a sorted set is a direct value; a set holds no groups.
type_entry(Store, Meta = #{ <<"width">> := _ }, Key, Opts) ->
    maybe
        {ok, _Value} ?= member(Store, Meta, Key, Opts),
        {ok, simple}
    end;
type_entry(Store, Meta, Key, Opts) ->
    maybe
        {ok, _Resolved, Value} ?= resolved(Store, Meta, Key, Opts),
        case Value of
            <<"group">> -> {ok, composite};
            _ -> {ok, simple}
        end
    end.

%% @doc Follow the links in a path's intermediate segments, returning the path
%% that the entry itself sits at.
resolve(Store, #{ <<"resolve">> := Path }, Opts) ->
    maybe
        {ok, Located, Meta} ?= open(Store, Opts),
        resolve_entry(Located, Meta, key(Path), Opts)
    end.

%% A sorted set holds no link markers, so a path resolves to itself.
resolve_entry(_Store, #{ <<"width">> := _ }, Key, _Opts) ->
    {ok, Key};
resolve_entry(Store, Meta, Key, Opts) ->
    maybe
        {ok, Parts} ?= resolve_links(Store, Meta, split(Key, [global]), Opts),
        {ok, join(Parts)}
    end.

%% @doc List the immediate children of a group. A path that does not carry a
%% `group' marker is not a group, even where keys sit below it: the marker is
%% what `hb_cache' writes, and a listing that ignored it would disagree with the
%% same database read locally.
list(Store, Req = #{ <<"list">> := Path }, Opts) ->
    maybe
        {ok, Located, Meta} ?= open(Store, Opts),
        list_entry(Located, Meta, key(Path), Req, Opts)
    end.

%% The children of a sorted set's prefix are the elements that carry it, named
%% by the bytes after it. The walk seeks to `from' and stops at `limit', so a
%% page of a large set costs the page rather than the set. A group of a
%% key-value database has to be read before it can be bounded, so its bounds
%% are applied to the children it yields.
list_entry(Store, Meta = #{ <<"width">> := _ }, Prefix, Req, Opts) ->
    elements(Store, Meta, Prefix, Req, Opts);
list_entry(Store, Meta, Key, Req, Opts) ->
    maybe
        {ok, Resolved, Value} ?= resolved(Store, Meta, Key, Opts),
        case Value of
            <<"group">> ->
                hb_store:bound_children(
                    Req,
                    names(children(Store, Meta, Resolved, Opts))
                );
            _ ->
                {error, not_found}
        end
    end.

%%% Opening the database.

%% @doc Attach the resolved locator to the store options and take the meta page
%% of the snapshot the database last committed.
open(Store, Opts) ->
    % A store can be reached without having been started explicitly, so the
    % metrics that the reads below record are declared here rather than there.
    init_prometheus(),
    maybe
        {ok, Located} ?= ensure_locator(Store, Opts),
        {ok, Meta} ?= meta(Located, Opts),
        {ok, Resolved} ?= elements_of(Located, Meta, Opts),
        {ok, Located, Resolved}
    end.

%% @doc Describe the database that holds the entries a caller asks about.
%%
%% A `MDB_DUPSORT' database is a sorted set: one key, whose duplicates are the
%% whole set, each the same width, in a database of its own that the key's leaf
%% node names. Rewriting the meta to describe that database lets one descent
%% read both shapes, and `width' -- the size of one element -- is what tells
%% them apart. Reaching it costs a read of the main database's root, which is
%% one page and never changes.
elements_of(_Store, Meta = #{ <<"depth">> := 0 }, _Opts) ->
    {ok, Meta};
elements_of(Store, Meta = #{ <<"root">> := Root }, Opts) ->
    case hb_lmdb_page:duplicates(Meta) of
        true ->
            maybe
                {ok, Page} ?= page(Store, Meta, Root, Opts),
                set_meta(Meta, hb_lmdb_page:node(Page, 0))
            end;
        false ->
            {ok, Meta}
    end.

%% The single leaf node of a sorted set's main database names the elements: a
%% database of its own once the set outgrows a page, and a page held inside the
%% node itself while it has not. A page carries the width of its items where a
%% database carries it as `pad'.
set_meta(Meta, {ok, _Key, {leaf, {subpage, Page}}}) ->
    case hb_lmdb_page:page(Page) of
        {ok, #{ <<"type">> := leaf2, <<"width">> := Width }} ->
            {ok, Meta#{ <<"subpage">> => Page, <<"width">> => Width }};
        {ok, #{ <<"type">> := Type }} ->
            {error, {not_a_leaf2_page, Type}};
        {error, _} = Error ->
            Error
    end;
set_meta(Meta, {ok, _Key, {leaf, {database, Set}}}) ->
    {ok,
        Meta#{
            <<"root">> => maps:get(<<"root">>, Set),
            <<"depth">> => maps:get(<<"depth">>, Set),
            <<"entries">> => maps:get(<<"entries">>, Set),
            <<"width">> => maps:get(<<"pad">>, Set)
        }
    };
set_meta(_Meta, {ok, _Key, _Reference}) ->
    {error, not_a_duplicate_set};
set_meta(_Meta, {error, _} = Error) ->
    Error.

%% @doc Resolve the locator, if it is not already attached. `hb_store' holds
%% the resolution in the store's instance message, which is keyed by the store's
%% name rather than by its root -- so an instance naming a different root, as
%% two published databases sharing the default name would produce, is resolved
%% afresh rather than trusted.
ensure_locator(Store = #{ <<"size">> := _, <<"start">> := _ }, _Opts) ->
    {ok, Store};
ensure_locator(Store = #{ <<"size">> := _, <<"relative">> := _ }, _Opts) ->
    {ok, Store};
ensure_locator(Store = #{ <<"root">> := Root }, Opts) ->
    case instance(Store, Root) of
        {ok, Locator} ->
            {ok, maps:merge(Store, Locator)};
        {error, not_found} ->
            case locate(Root, Opts) of
                {ok, Locator} -> {ok, maps:merge(Store, Locator)};
                {error, _} = Error -> Error
            end;
        {error, _} = Error ->
            Error
    end;
ensure_locator(_Store, _Opts) ->
    {error, {badarg, <<"An `arlmdb' store must be given a `root'.">>}}.

%% @doc Take the store's instance message, if `hb_store' holds one that names
%% the same root. Starting the store is what resolves the locator, so a failure
%% to start is a failure to reach Arweave and is surfaced as such: it must not
%% read as a missing key, or the node would move silently to its next store.
instance(Store, Root) ->
    try hb_store:find(Store) of
        Locator = #{ <<"root">> := Root, <<"size">> := _ } -> {ok, Locator};
        _ -> {error, not_found}
    catch
        throw:{store_start_failed, {_Module, _Name, Reason}} -> {error, Reason}
    end.

%% @doc Resolve a locator to the range of the weave that holds the database.
%% An explicit `Offset:Length' pair names the range directly. An ID is looked
%% for as a confirmed transaction, then as a transaction still in the mempool --
%% whose bytes are addressed relative to itself, having no place in the weave
%% yet -- and finally as a data item inside a bundle.
locate(Root, Opts) when is_binary(Root) ->
    case binary:split(Root, <<":">>) of
        [Offset, Length] ->
            explicit(Offset, Length);
        [ID] when ?IS_ID(ID) ->
            case locate_transaction(ID, Opts) of
                {error, not_found} -> locate_pending(ID, Opts);
                Result -> Result
            end;
        _ ->
            {error, {badarg, <<"An `arlmdb' root must be an ID or a range.">>}}
    end;
locate(_Root, _Opts) ->
    {error, {badarg, <<"An `arlmdb' root must be a binary.">>}}.

%% Ranges are given as absolute weave offsets, matching the `~arweave@2.9'
%% device's own `Offset-Length' references.
explicit(Offset, Length) ->
    try {hb_util:int(Offset), hb_util:int(Length)} of
        {Start, Size} when Start >= 0, Size > 0 ->
            {ok, #{ <<"start">> => Start, <<"size">> => Size }};
        _ ->
            {error, {badarg, <<"An `arlmdb' range must be positive.">>}}
    catch _:_ ->
        {error, {badarg, <<"An `arlmdb' range must be `Offset:Length'.">>}}
    end.

%% The transaction offset route reports the offset of a transaction's *final*
%% byte, so its data begins that many bytes back.
locate_transaction(ID, Opts) ->
    Request =
        #{
            <<"path">> => <<"/arweave/tx/", ID/binary, "/offset">>,
            <<"method">> => <<"GET">>
        },
    try hb_http:request(Request, Opts) of
        {ok, #{ <<"body">> := Body }} ->
            case hb_json:decode(Body) of
                #{ <<"offset">> := End, <<"size">> := Size } ->
                    transaction_range(hb_util:int(End), hb_util:int(Size));
                _ ->
                    {error, not_found}
            end;
        _ ->
            {error, not_found}
    catch _:_ ->
        {error, not_found}
    end.

transaction_range(_End, 0) ->
    {error, {badarg, <<"An `arlmdb' transaction must carry data.">>}};
transaction_range(End, Size) ->
    {ok, #{ <<"start">> => End - Size, <<"size">> => Size }}.

%% A transaction that has not been mined has no place in the weave's data tree
%% yet, so its bytes are addressed relative to the transaction itself.
locate_pending(ID, Opts) ->
    Request =
        #{
            <<"path">> => <<"pending">>,
            <<"pending">> => ID,
            <<"exclude-data">> => true
        },
    case hb_ao:resolve(#{ <<"device">> => <<"arweave@2.9">> }, Request, Opts) of
        {ok, Pending} ->
            case hb_ao:get(<<"data_size">>, Pending, not_found, Opts) of
                not_found -> locate_item(ID, Opts);
                Size -> {ok, #{
                    <<"relative">> => ID,
                    <<"size">> => hb_util:int(Size)
                }}
            end;
        _ ->
            locate_item(ID, Opts)
    end.

%% A data item inside a bundle carries a header of its own inside the weave, so
%% the range that holds its data starts past that header.
locate_item(ID, Opts) ->
    Request =
        #{
            <<"method">> => <<"HEAD">>,
            <<"path">> => <<"/~arweave@2.9/raw=", ID/binary>>
        },
    case hb_http:request(Request, Opts) of
        {ok, #{ <<"data-offset">> := Start, <<"content-length">> := Size }}
                when is_integer(Start), is_integer(Size), Size > 0 ->
            {ok, #{ <<"start">> => Start, <<"size">> => Size }};
        _ ->
            {error, {unresolvable_root, ID}}
    end.

%%% Reading the database.

%% @doc Take the meta page of the snapshot the database last committed. Pages 0
%% and 1 are both meta pages and alternate on every commit, so both are read --
%% in one range, since a page can be no larger than 64 KiB -- and the one
%% carrying the later transaction wins. Reading page 0 alone would report a
%% freshly written database as empty.
meta(Store = #{ <<"size">> := Size }, Opts) ->
    maybe
        {ok, Bin} ?= read_range(Store, 0, min(?META_SPAN, Size), Opts, meta),
        {ok, First = #{ <<"page-size">> := PageSize }} ?= hb_lmdb_page:meta(Bin),
        true ?= 2 * PageSize =< byte_size(Bin) orelse {error, truncated_database},
        {ok, Second} ?= hb_lmdb_page:meta(binary:part(Bin, PageSize, PageSize)),
        {ok, latest(First, Second)}
    end.

latest(First = #{ <<"txnid">> := FirstID }, Second = #{ <<"txnid">> := SecondID }) ->
    case SecondID > FirstID of
        true -> Second;
        false -> First
    end.

%% @doc Read a page of the database by its number, refusing one that falls
%% outside the database or past the last page its meta page admits to.
page(Store, #{ <<"page-size">> := PageSize, <<"last-page">> := Last }, Number, Opts)
        when is_integer(Number), Number >= 0, Number =< Last ->
    read_range(Store, Number * PageSize, PageSize, Opts, page);
page(_Store, _Meta, Number, _Opts) ->
    {error, {no_such_page, Number}}.

%% @doc Read a value that LMDB pushed onto overflow pages. Only the first page
%% of the run carries a header, so the value is the bytes that follow it, and
%% the header is taken in the same read as the value it introduces rather than
%% costing a second.
overflow(Store, Meta = #{ <<"page-size">> := PageSize }, Number, Size, Opts) ->
    #{ <<"last-page">> := Last } = Meta,
    maybe
        true ?= Size =< max_value(Store) orelse {error, {value_too_large, Size}},
        true ?= Number =< Last orelse {error, {no_such_page, Number}},
        {ok, Bin} ?=
            read_range(
                Store,
                Number * PageSize,
                ?PAGE_HEADER_SIZE + Size,
                Opts,
                overflow
            ),
        {ok, #{ <<"type">> := overflow }} ?= hb_lmdb_page:page(Bin),
        {ok, binary:part(Bin, ?PAGE_HEADER_SIZE, Size)}
    end.

%% @doc Read a range of the database. Every offset in the file is untrusted, so
%% a range that does not sit inside the database is refused before it can size a
%% request. A transaction still in the mempool is read relative to itself.
read_range(#{ <<"size">> := Size }, Offset, Length, _Opts, _Kind)
        when not is_integer(Offset);
            not is_integer(Length);
            Offset < 0;
            Length =< 0;
            Offset + Length > Size ->
    {error, {out_of_bounds, Offset, Length}};
read_range(Store, Offset, Length, Opts, Kind) ->
    held(
        Store,
        page_store(Store),
        page_key(Store, Offset, Length),
        Offset,
        Length,
        Opts,
        Kind
    ).

%% @doc Take a range from the page store where it holds it already.
%%
%% A published database never changes, so a page of it is the same bytes for as
%% long as anything is willing to hold them. Every lookup crosses the meta
%% pages and the top of the tree, so holding those turns each lookup after the
%% first into a read of the leaf and little else. The store is volatile and
%% resets on a timer by default; `[]' turns it off, and `hb_store' reads that
%% as no viable store without a branch of its own.
held(Store, [], _Key, Offset, Length, Opts, Kind) ->
    fetch(Store, Offset, Length, Opts, Kind);
held(Store, _PageStore, no_key, Offset, Length, Opts, Kind) ->
    fetch(Store, Offset, Length, Opts, Kind);
held(Store, PageStore, Key, Offset, Length, Opts, Kind) ->
    case hb_store:read(PageStore, Key, Opts) of
        {ok, Bin} ->
            hb_prometheus:inc(
                counter, hb_store_arlmdb_page_hits, [hb_util:bin(Kind)]),
            {ok, Bin};
        _ ->
            keep(PageStore, Key, fetch(Store, Offset, Length, Opts, Kind), Opts)
    end.

keep(PageStore, Key, {ok, Bin}, Opts) ->
    hb_store:write(PageStore, #{ Key => Bin }, Opts),
    {ok, Bin};
keep(_PageStore, _Key, Result, _Opts) ->
    Result.

fetch(Store, Offset, Length, Opts, Kind) ->
    hb_prometheus:inc(counter, hb_store_arlmdb_reads, [hb_util:bin(Kind)]),
    hb_prometheus:measure_and_report(
        fun() -> read_chunks(Store, Offset, Length, Opts) end,
        hb_store_arlmdb_read_duration_seconds,
        [hb_util:bin(Kind)]
    ).

%% @doc The store that holds the pages already read.
page_store(#{ <<"page-store">> := PageStore }) -> PageStore;
page_store(_Store) -> ?DEFAULT_PAGE_STORE.

%% @doc The page store's key for a range of the weave.
%%
%% A range is named by where it begins in the weave and how far it runs. Two
%% ranges can begin at the same place -- the pair of meta pages and the first
%% page of the database both begin at its start -- and are not the same bytes,
%% so the length is part of the name. A weave offset names one range of one
%% database, so two stores may share a page store without colliding.
%%
%% A database still in the mempool has no place in the weave yet, and is still
%% being written, so its pages are not held.
page_key(#{ <<"start">> := Start }, Offset, Length) ->
    <<
        "~arweave@2.9/offset=",
        (hb_util:bin(Start + Offset))/binary,
        "/length=",
        (hb_util:bin(Length))/binary
    >>;
page_key(_Store, _Offset, _Length) ->
    no_key.

read_chunks(#{ <<"start">> := Start }, Offset, Length, Opts) ->
    hb_store_arweave:read_chunks(Start + Offset, Length, Opts);
read_chunks(#{ <<"relative">> := ID }, Offset, Length, Opts) ->
    hb_store_arweave:read_chunks(
        #{ <<"relative">> => ID, <<"offset">> => Offset },
        Length,
        Opts
    ).

%%% Walking the tree.

%% @doc Find the value stored at a key. The descent is bounded by the depth the
%% meta page records: a database read out of the weave is untrusted input, so a
%% cycle in its page pointers must terminate.
lookup(_Store, #{ <<"depth">> := 0 }, _Key, _Opts) ->
    {error, not_found};
lookup(Store, Meta = #{ <<"root">> := Root }, Key, Opts) ->
    descend(Store, Meta, Root, Key, max_depth(Store, Meta), Opts).

descend(_Store, _Meta, _Number, _Key, 0, _Opts) ->
    {error, descent_too_deep};
descend(Store, Meta, Number, Key, Remaining, Opts) ->
    maybe
        {ok, Page} ?= page(Store, Meta, Number, Opts),
        found(Store, Meta, Page, Key, Remaining, Opts)
    end.

found(Store, Meta, Page, Key, Remaining, Opts) ->
    case hb_lmdb_page:search(Page, Key) of
        {branch, _Index, Child} ->
            descend(Store, Meta, Child, Key, Remaining - 1, Opts);
        {leaf, _} = Leaf ->
            value(Store, Meta, Leaf, Opts);
        not_found ->
            {error, not_found};
        {error, _} = Error ->
            Error
    end.

%% @doc Take the value that a leaf node holds, reading the overflow pages it
%% names where the value was too large to sit on the leaf itself.
value(Store, _Meta, {leaf, Value}, _Opts) when is_binary(Value) ->
    case byte_size(Value) =< max_value(Store) of
        true -> {ok, Value};
        false -> {error, {value_too_large, byte_size(Value)}}
    end;
value(Store, Meta, {leaf, {overflow, Number, Size}}, Opts) ->
    overflow(Store, Meta, Number, Size, Opts).

%% @doc Read a key, following the value links that lead from it and returning
%% the key that the entry holding the data sits at.
resolved(Store, Meta, Key, Opts) ->
    resolved(Store, Meta, Key, Opts, 0).
resolved(_Store, _Meta, _Key, _Opts, Links) when Links > ?MAX_LINKS ->
    {error, too_many_links};
resolved(Store, Meta, Key, Opts, Links) ->
    case lookup(Store, Meta, Key, Opts) of
        {ok, <<"link:", Target/binary>>} when byte_size(Target) > 0 ->
            resolved(Store, Meta, Target, Opts, Links + 1);
        {ok, Value} ->
            {ok, Key, Value};
        {error, not_found} ->
            missing(Store, Meta, Key, Opts, Links);
        {error, _} = Error ->
            Error
    end.

%% A key that is absent may still be reachable through a link in one of its
%% path segments. Content-addressed `data' keys never carry one, so a miss on
%% one of those is final and costs no further reads.
missing(Store, Meta, Key, Opts, Links) ->
    case is_data_path(Key) of
        true ->
            {error, not_found};
        false ->
            case resolve_links(Store, Meta, split(Key, [global, trim_all]), Opts) of
                {ok, Parts} ->
                    case join(Parts) of
                        Key -> {error, not_found};
                        Resolved -> resolved(Store, Meta, Resolved, Opts, Links + 1)
                    end;
                {error, _} = Error ->
                    Error
            end
    end.

%% @doc Rewrite a path so that any of its segments that is itself a link is
%% replaced by the link's target. The final segment names the entry rather than
%% a step towards it, so it is left alone.
resolve_links(Store, Meta, Path, Opts) ->
    resolve_links(Store, Meta, Path, Opts, 0).
resolve_links(_Store, _Meta, _Path, _Opts, Links) when Links > ?MAX_LINKS ->
    {error, too_many_links};
resolve_links(_Store, _Meta, [Last], _Opts, _Links) ->
    {ok, [Last]};
resolve_links(Store, Meta, Path, Opts, Links) ->
    resolve_links(Store, Meta, Path, [], Opts, Links).
resolve_links(_Store, _Meta, [], Resolved, _Opts, _Links) ->
    {ok, lists:reverse(Resolved)};
resolve_links(_Store, _Meta, Path = [<<"data">> | _], [], _Opts, _Links) ->
    {ok, Path};
resolve_links(Store, Meta, [Head | Tail], Resolved, Opts, Links) ->
    Prefix = join(lists:reverse([Head | Resolved])),
    case lookup(Store, Meta, Prefix, Opts) of
        {ok, <<"link:", Target/binary>>} when byte_size(Target) > 0 ->
            resolve_links(
                Store,
                Meta,
                split(Target, [global]) ++ Tail,
                Opts,
                Links + 1
            );
        {ok, _Value} ->
            resolve_links(Store, Meta, Tail, [Head | Resolved], Opts, Links);
        {error, not_found} ->
            resolve_links(Store, Meta, Tail, [Head | Resolved], Opts, Links);
        {error, _} = Error ->
            Error
    end.

%%% Scanning a range of the tree.

%% @doc Read a group as a composite, carrying its immediate children with it.
composite(Store, Meta, Key, Opts) ->
    case children(Store, Meta, Key, Opts) of
        {ok, Children} -> {composite, Children};
        {error, _} = Error -> Error
    end.

%% @doc Collect a group's immediate children -- the keys directly below `Key/'
%% -- with the value that each of them holds. A key that still carries a `/'
%% below the prefix names a grandchild, reached through a subgroup whose own
%% marker is an immediate child in its own right. The values come from the
%% leaves that the scan crosses anyway, sparing the caller a read for each.
children(Store, Meta, Key, Opts) ->
    Prefix = child_prefix(Key),
    maybe
        {ok, Stack, Leaf, Keys, Index} ?= seek(Store, Meta, Prefix, Opts),
        scan(Store, Meta, Stack, Leaf, Keys, Index, Prefix, [], Opts)
    end.

%% Walk forwards from the first key at or after the prefix. LMDB leaves carry
%% no pointer to their neighbour, so an exhausted leaf sends the walk back up
%% the branch pages it descended through, to take the next child from there.
scan(Store, Meta, Stack, _Leaf, Keys, Index, Prefix, Found, Opts)
        when Index >= Keys ->
    case adjacent_leaf(Store, Meta, Stack, 1, Opts) of
        {ok, NextStack, NextLeaf, NextKeys, NextIndex} ->
            scan(
                Store, Meta, NextStack, NextLeaf, NextKeys, NextIndex,
                Prefix, Found, Opts
            );
        {error, no_more_leaves} ->
            {ok, lists:reverse(Found)};
        {error, _} = Error ->
            Error
    end;
scan(Store, Meta, Stack, Leaf, Keys, Index, Prefix, Found, Opts) ->
    Size = byte_size(Prefix),
    case hb_lmdb_page:node(Leaf, Index) of
        {ok, <<Prefix:Size/binary, Name/binary>>, Reference} when Name =/= <<>> ->
            case child(Store, Meta, Name, Reference, Found, Opts) of
                {ok, Next} ->
                    scan(
                        Store, Meta, Stack, Leaf, Keys, Index + 1, Prefix,
                        Next, Opts
                    );
                {error, _} = Error ->
                    Error
            end;
        {ok, _Key, _Reference} ->
            % The scan has passed the last key carrying the prefix.
            {ok, lists:reverse(Found)};
        {error, _} = Error ->
            Error
    end.

child(Store, Meta, Name, Reference, Found, Opts) ->
    case binary:match(Name, <<"/">>) of
        nomatch ->
            case value(Store, Meta, Reference, Opts) of
                {ok, Value} -> {ok, [{Name, Value} | Found]};
                {error, _} = Error -> Error
            end;
        _ ->
            {ok, Found}
    end.

%%% Reading a sorted set.

%% @doc Test whether a sorted set holds an element. Every element is the same
%% width, so a key of any other length is not one.
member(Store, Meta = #{ <<"width">> := Width }, Key, Opts)
        when byte_size(Key) == Width ->
    maybe
        {ok, _Stack, Leaf, Items, Index} ?= seek(Store, Meta, Key, Opts),
        at_index(Leaf, Items, Index, Width, Key)
    end;
member(_Store, _Meta, _Key, _Opts) ->
    {error, not_found}.

%% Report whether the element a seek landed on is the one sought. A seek that
%% ran off the end of its leaf landed on no element at all.
at_index(Leaf, Items, Index, Width, Key) when Index < Items ->
    case hb_lmdb_page:item(Leaf, Index, Width) of
        {ok, Key} -> {ok, <<>>};
        {ok, _Other} -> {error, not_found};
        {error, _} = Error -> Error
    end;
at_index(_Leaf, _Items, _Index, _Width, _Key) ->
    {error, not_found}.

%% @doc Walk the elements of a sorted set that begin with a prefix, naming each
%% by the bytes that follow it.
%%
%% The walk starts at the prefix extended by `from' -- the first element at or
%% after it going forward, the last at or before it going backward -- and stops
%% at `limit'. Both are seeks rather than scans, so the page a caller asks for
%% costs what the page holds and nothing more, and page fifty costs what page
%% one does.
elements(Store, Meta, Prefix, Req, Opts) ->
    Start = <<Prefix/binary, (maps:get(<<"from">>, Req, <<>>))/binary>>,
    Limit =
        case maps:get(<<"limit">>, Req, unbounded) of
            unbounded -> unbounded;
            Bound -> hb_util:int(Bound)
        end,
    Step =
        case hb_util:atom(maps:get(<<"direction">>, Req, forward)) of
            backward -> -1;
            forward -> 1
        end,
    maybe
        {ok, Stack, Leaf, Items, Index} ?= start(Store, Meta, Start, Step, Opts),
        walk(Store, Meta, Stack, Leaf, Items, Index, Prefix, Step, Limit, [], Opts)
    end.

%% Position the walk on the element it starts from. Going backward that is the
%% last element before the first one that sorts after everything carrying the
%% starting bytes, which is the element at or before them.
start(Store, Meta, Start, 1, Opts) ->
    seek(Store, Meta, Start, Opts);
start(Store, Meta, Start, -1, Opts) ->
    case increment(Start) of
        no_successor ->
            last(Store, Meta, Opts);
        Bound ->
            maybe
                {ok, Stack, Leaf, Items, Index} ?= seek(Store, Meta, Bound, Opts),
                {ok, Stack, Leaf, Items, Index - 1}
            end
    end.

%% Position on the last element of the set.
last(_Store, #{ <<"subpage">> := Page }, _Opts) ->
    case hb_lmdb_page:num_keys(Page) of
        {ok, Items} -> {ok, [], Page, Items, Items - 1};
        {error, _} = Error -> Error
    end;
last(Store, Meta = #{ <<"root">> := Root }, Opts) ->
    outermost(Store, Meta, Root, [], -1, max_depth(Store, Meta), Opts).

%% @doc The smallest binary that sorts after every binary starting with the
%% given one, or `no_successor' where there is none: the bytes are empty, or
%% every one of them is `16#ff'.
increment(<<>>) ->
    no_successor;
increment(Bytes) ->
    Head = binary:part(Bytes, 0, byte_size(Bytes) - 1),
    case binary:last(Bytes) of
        16#ff -> increment(Head);
        Last -> <<Head/binary, (Last + 1)>>
    end.

%% Walk from one element to the next, taking the leaf on either side when the
%% index leaves the one in hand. A leaf carries no pointer to its neighbour, so
%% the branch pages the descent came through are what the walk climbs.
walk(_Store, _Meta, _Stack, _Leaf, _Items, _Index, _Prefix, _Step, 0, Found, _Opts) ->
    {ok, lists:reverse(Found)};
walk(Store, Meta, Stack, _Leaf, Items, Index, Prefix, Step, Limit, Found, Opts)
        when Index >= Items; Index < 0 ->
    case adjacent_leaf(Store, Meta, Stack, Step, Opts) of
        {ok, NextStack, NextLeaf, NextItems, NextIndex} ->
            walk(
                Store, Meta, NextStack, NextLeaf, NextItems, NextIndex, Prefix,
                Step, Limit, Found, Opts
            );
        {error, no_more_leaves} ->
            {ok, lists:reverse(Found)};
        {error, _} = Error ->
            Error
    end;
walk(
        Store, Meta = #{ <<"width">> := Width }, Stack, Leaf, Items, Index,
        Prefix, Step, Limit, Found, Opts
    ) ->
    Size = byte_size(Prefix),
    case hb_lmdb_page:item(Leaf, Index, Width) of
        {ok, <<Prefix:Size/binary, Name/binary>>} ->
            walk(
                Store, Meta, Stack, Leaf, Items, Index + Step, Prefix, Step,
                remaining(Limit), [Name | Found], Opts
            );
        {ok, _Other} ->
            % The walk has passed the last element carrying the prefix.
            {ok, lists:reverse(Found)};
        {error, _} = Error ->
            Error
    end.

remaining(unbounded) -> unbounded;
remaining(Limit) -> Limit - 1.

%% @doc Reduce a group's children to their names alone, which is what `list/3'
%% answers with; a composite read carries the values along with them.
names({ok, Children}) -> {ok, [Name || {Name, _Value} <- Children]};
names({error, _} = Error) -> Error.

%% @doc Descend to the leaf that holds the first key at or after the given one,
%% keeping the branch pages walked through and the child of each that was taken.
%% That stack is what lets the scan move on to the next leaf.
seek(_Store, #{ <<"subpage">> := Page, <<"width">> := Width }, Key, _Opts) ->
    % A set held inside one page has no tree above it to descend.
    cursor([], Page, hb_lmdb_page:seek_item(Page, Key, Width));
seek(_Store, #{ <<"depth">> := 0 }, _Key, _Opts) ->
    {error, not_found};
seek(Store, Meta = #{ <<"root">> := Root }, Key, Opts) ->
    seek(Store, Meta, Root, Key, [], max_depth(Store, Meta), Opts).
seek(_Store, _Meta, _Number, _Key, _Stack, 0, _Opts) ->
    {error, descent_too_deep};
seek(Store, Meta, Number, Key, Stack, Remaining, Opts) ->
    maybe
        {ok, Page} ?= page(Store, Meta, Number, Opts),
        seek_page(Store, Meta, Page, Key, Stack, Remaining, Opts)
    end.

seek_page(
        Store, Meta = #{ <<"width">> := Width }, Page, Key, Stack, Remaining,
        Opts
    ) ->
    case hb_lmdb_page:page(Page) of
        {ok, #{ <<"type">> := branch }} ->
            branch_step(Store, Meta, Page, Key, Stack, Remaining, Opts);
        {ok, #{ <<"type">> := leaf2, <<"keys">> := Items }} ->
            cursor(Stack, Page, Items, hb_lmdb_page:seek_item(Page, Key, Width));
        {ok, #{ <<"type">> := Type }} ->
            {error, {not_a_leaf2_page, Type}};
        {error, _} = Error ->
            Error
    end;
seek_page(Store, Meta, Page, Key, Stack, Remaining, Opts) ->
    case hb_lmdb_page:search(Page, Key) of
        {branch, Index, Child} ->
            seek(
                Store, Meta, Child, Key, [{Page, Index} | Stack],
                Remaining - 1, Opts
            );
        {error, _} = Error ->
            Error;
        _ ->
            cursor(Stack, Page, hb_lmdb_page:seek(Page, Key))
    end.

%% Take the child of a branch page that covers the key, keeping the index taken
%% so that a walk can climb back and take the next one.
branch_step(Store, Meta, Page, Key, Stack, Remaining, Opts) ->
    case hb_lmdb_page:search(Page, Key) of
        {branch, Index, Child} ->
            seek(
                Store, Meta, Child, Key, [{Page, Index} | Stack],
                Remaining - 1, Opts
            );
        {error, _} = Error ->
            Error
    end.

%% @doc Take the leaf on one side of the one a walk has exhausted: climb to the
%% nearest branch page with a child left on that side, step to it, and descend
%% to the leaf a walk travelling that way enters first.
%%
%% LMDB leaves carry no pointer to their neighbours, so the branch pages the
%% descent came through are what a walk climbs.
adjacent_leaf(_Store, _Meta, [], _Step, _Opts) ->
    {error, no_more_leaves};
adjacent_leaf(Store, Meta, [{Page, Index} | Above], Step, Opts) ->
    case sibling(Page, Index, Step) of
        exhausted ->
            adjacent_leaf(Store, Meta, Above, Step, Opts);
        {ok, Next} ->
            take_child(Store, Meta, Page, Next, Above, Step, Opts);
        {error, _} = Error ->
            Error
    end.

%% The child of a branch page on the far side of the one a walk came through,
%% or `exhausted' where it has none left on that side.
sibling(_Page, Index, -1) when Index > 0 ->
    {ok, Index - 1};
sibling(_Page, _Index, -1) ->
    exhausted;
sibling(Page, Index, 1) ->
    case hb_lmdb_page:num_keys(Page) of
        {ok, Keys} when Index + 1 < Keys -> {ok, Index + 1};
        {ok, _Keys} -> exhausted;
        {error, _} = Error -> Error
    end.

take_child(Store, Meta, Page, Index, Above, Step, Opts) ->
    case hb_lmdb_page:node(Page, Index) of
        {ok, _Key, {branch, Child}} ->
            outermost(
                Store, Meta, Child, [{Page, Index} | Above], Step,
                max_depth(Store, Meta), Opts
            );
        {ok, _Key, _Reference} ->
            {error, invalid_branch_page};
        {error, _} = Error ->
            Error
    end.

%% @doc Descend to the leaf a walk travelling in the given direction enters
%% first below a page, positioned on the entry it enters that leaf at: the
%% leftmost of each going forward, the rightmost going backward.
outermost(_Store, _Meta, _Number, _Stack, _Step, 0, _Opts) ->
    {error, descent_too_deep};
outermost(Store, Meta, Number, Stack, Step, Remaining, Opts) ->
    maybe
        {ok, Page} ?= page(Store, Meta, Number, Opts),
        {ok, Parsed} ?= hb_lmdb_page:page(Page),
        outermost_page(Store, Meta, Page, Parsed, Stack, Step, Remaining, Opts)
    end.

outermost_page(
        Store, Meta, Page, #{ <<"type">> := branch, <<"keys">> := Keys },
        Stack, Step, Remaining, Opts
    ) ->
    Index = entered_at(Keys, Step),
    case hb_lmdb_page:node(Page, Index) of
        {ok, _Key, {branch, Child}} ->
            outermost(
                Store, Meta, Child, [{Page, Index} | Stack], Step,
                Remaining - 1, Opts
            );
        {ok, _Key, _Reference} ->
            {error, invalid_branch_page};
        {error, _} = Error ->
            Error
    end;
outermost_page(
        _Store, _Meta, Page, #{ <<"type">> := Type, <<"keys">> := Keys },
        Stack, Step, _Remaining, _Opts
    ) when Type == leaf; Type == leaf2 ->
    {ok, Stack, Page, Keys, entered_at(Keys, Step)};
outermost_page(
        _Store, _Meta, _Page, #{ <<"type">> := Type }, _Stack, _Step, _R, _Opts
    ) ->
    {error, {not_a_node_page, Type}}.

%% The entry of a page that a walk travelling in the given direction reaches
%% first.
entered_at(_Keys, 1) -> 0;
entered_at(Keys, -1) -> Keys - 1.

%% @doc Pair a leaf with the index a scan of it starts from.
cursor(_Stack, _Page, {error, _} = Error) ->
    Error;
cursor(Stack, Page, Index) ->
    case hb_lmdb_page:num_keys(Page) of
        {ok, Keys} -> {ok, Stack, Page, Keys, Index};
        {error, _} = Error -> Error
    end.
cursor(_Stack, _Page, _Keys, {error, _} = Error) ->
    Error;
cursor(Stack, Page, Keys, Index) ->
    {ok, Stack, Page, Keys, Index}.

%%% Helpers.

max_depth(Store, #{ <<"depth">> := Depth }) ->
    hb_util:int(maps:get(<<"max-depth">>, Store, Depth)).

max_value(Store) ->
    hb_util:int(maps:get(<<"max-value">>, Store, ?DEFAULT_MAX_VALUE)).

key(Path) when is_binary(Path) -> Path;
key(Path) -> hb_path:to_binary(Path).

split(Path, Options) -> binary:split(Path, <<"/">>, Options).

join(Parts) -> hb_util:bin(lists:join(<<"/">>, Parts)).

is_data_path(<<"data">>) -> true;
is_data_path(<<"data/", _/binary>>) -> true;
is_data_path(_) -> false.

child_prefix(<<>>) -> <<>>;
child_prefix(<<"/">>) -> <<>>;
child_prefix(Path) ->
    case binary:last(Path) of
        $/ -> Path;
        _ -> <<Path/binary, "/">>
    end.

%% @doc Initialize the Prometheus metrics for the store. A lookup's cost is the
%% number of ranged reads it makes, so those are counted by the kind of page
%% they fetched.
init_prometheus() ->
    hb_prometheus:declare(
        counter,
        [
            {name, hb_store_arlmdb_reads},
            {labels, [kind]},
            {help, "Ranged reads of a published LMDB database"}
        ]
    ),
    hb_prometheus:declare(
        counter,
        [
            {name, hb_store_arlmdb_page_hits},
            {labels, [kind]},
            {help, "Ranges served from the page store rather than the weave"}
        ]
    ),
    hb_prometheus:declare(
        histogram,
        [
            {name, hb_store_arlmdb_read_duration_seconds},
            {buckets, [0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 30]},
            {labels, [kind]},
            {help, "How long a ranged read of a published LMDB database takes"}
        ]
    ),
    hb_http_client:init_prometheus().

%%% Tests

%% A confirmed Arweave transaction carrying an ANS-104 bundle, and the range of
%% the weave that holds it. Used to exercise locator resolution, and to point a
%% store at bytes that are emphatically not an LMDB database.
-define(BUNDLE_TX, <<"bndIwac23-s0K11TLC1N7z472sLGAkiOdhds87ZywoE">>).
-define(BUNDLE_START, 363524457275638).
-define(BUNDLE_SIZE, 8387).

%% The fixture that `hb_lmdb_page' reads from disk, published to Arweave: the
%% same bytes, so the store must answer with what the local reader finds.
%% 52,736 bytes on 512-byte pages, three levels deep.
-define(PUBLISHED_FIXTURE, <<"OLuXZuP3L1Sjj1ysJeYqf_782tWUmky0UYY3cBuYre4">>).

%% An Arweave offset index of ten million entries, on 64 KiB pages and three
%% levels deep: 540,934,144 bytes, with a `~match@1.0' group over every
%% hundred-thousandth of its keys.
-define(PUBLISHED_INDEX, <<"b159UDeD87YEFujWBMM8bISZ8DL8Wm1jLa-Bs_LQGAw">>).
-define(INDEX_ENTRIES, 10_000_000).

%% The cache that twenty thousand `cache_message/1' writes produced, carrying
%% the `~match@1.0' reverse index that `hb_cache' wrote alongside them.
-define(PUBLISHED_CACHE, <<"oaIcZfEszYz84F_8Q1RRkJ-UlTREJBSGlGw3rQ-KOR8">>).
-define(CACHE_MESSAGES, 20_000).

store(Name, Root) ->
    #{
        <<"store-module">> => ?MODULE,
        <<"name">> => Name,
        <<"root">> => Root
    }.

%% @doc An ID names the range of the weave that holds the transaction's data.
%% The offset route reports the offset of a transaction's final byte, so the
%% data begins its own size back from there.
locates_transaction_test() ->
    Store = store(<<"arlmdb-locate">>, ?BUNDLE_TX),
    ok = hb_store:start(Store, #{}),
    ?assertMatch(
        #{ <<"start">> := ?BUNDLE_START, <<"size">> := ?BUNDLE_SIZE },
        hb_store:find(Store)
    ).

%% @doc A range of the weave that is not an LMDB database is refused rather
%% than interpreted. The refusal must not read as a missing key: a store that
%% answered `not_found' would send the node quietly on to its next store.
refuses_foreign_range_test() ->
    Range =
        <<
            (integer_to_binary(?BUNDLE_START))/binary,
            ":",
            (integer_to_binary(?BUNDLE_SIZE))/binary
        >>,
    Store = store(<<"arlmdb-foreign">>, Range),
    ?assertEqual(
        {error, not_lmdb},
        hb_store:read([Store], <<"anything">>, #{})
    ),
    ?assertEqual(
        {error, not_lmdb},
        hb_store:list([Store], <<"anything">>, #{})
    ).

%% @doc A root that names neither an ID nor a range is refused before any
%% request is made for it.
refuses_malformed_root_test() ->
    lists:foreach(
        fun(Root) ->
            ?assertMatch(
                {error, {badarg, _}},
                start(store(<<"arlmdb-malformed">>, Root), #{}, #{})
            )
        end,
        [<<"not-a-root">>, <<"12:">>, <<"abc:def">>, <<"-1:10">>, <<"10:0">>]
    ),
    ?assertMatch({error, {badarg, _}}, start(#{}, #{}, #{})).

%% @doc Two databases that share a store name each resolve their own locator:
%% the instance message that `hb_store' holds names the root it came from, and
%% one naming a different root is resolved afresh rather than trusted.
distinct_roots_test() ->
    First = store(<<"arlmdb-shared-name">>, <<"100:200">>),
    Second = First#{ <<"root">> => <<"300:400">> },
    ok = hb_store:start(First, #{}),
    ?assertMatch(
        {ok, #{ <<"start">> := 100, <<"size">> := 200 }},
        ensure_locator(First, #{})
    ),
    ?assertMatch(
        {ok, #{ <<"start">> := 300, <<"size">> := 400 }},
        ensure_locator(Second, #{})
    ).

%% @doc Every offset the database carries is untrusted, so a range that does not
%% sit inside it is refused before it can size a request for the weave.
refuses_out_of_bounds_range_test() ->
    Located = #{ <<"start">> => 0, <<"size">> => 1024 },
    lists:foreach(
        fun({Offset, Length}) ->
            ?assertMatch(
                {error, {out_of_bounds, _, _}},
                read_range(Located, Offset, Length, #{}, page)
            )
        end,
        [{0, 1025}, {1024, 1}, {-1, 8}, {0, 0}, {512, 513}, {not_an_offset, 8}]
    ).

%% @doc The published copy of `hb_lmdb_page''s fixture answers with the contents
%% it was built from. Listing a group asserts its whole key set in one scan --
%% the walk crosses every leaf the group spans -- while reading proves the
%% values, including one pushed onto overflow pages and one reached by link.
published_fixture_test_() ->
    {timeout, 300, fun published_fixture/0}.
published_fixture() ->
    Store = [store(<<"arlmdb-fixture">>, ?PUBLISHED_FIXTURE)],
    Entries = maps:from_list(hb_lmdb_page:fixture_entries()),
    Expected =
        lists:sort(
            [
                Child
            ||
                <<"kv/", Child/binary>> <- maps:keys(Entries),
                binary:match(Child, <<"/">>) == nomatch
            ]
        ),
    ?assertEqual({ok, Expected}, hb_store:list(Store, <<"kv">>, #{})),
    ?assertEqual({ok, [<<"a">>, <<"b">>]}, hb_store:list(Store, <<"kv/sub">>, #{})),
    ?assertEqual({ok, [<<"latest">>]}, hb_store:list(Store, <<"alias">>, #{})),
    ?assertEqual({ok, [<<"blob">>]}, hb_store:list(Store, <<"big">>, #{})),
    % Reading a group yields the same children as listing it, with the values
    % that the scan crossed on its way through the leaves.
    {composite, Children} = hb_store:read(Store, <<"kv">>, #{}),
    ?assertEqual(Expected, [Name || {Name, _Value} <- Children]),
    lists:foreach(
        fun({Name, Value}) ->
            ?assertEqual(
                maps:get(<<"kv/", Name/binary>>, Entries),
                Value
            )
        end,
        Children
    ),
    % Values, including one on overflow pages and one behind a link.
    lists:foreach(
        fun(Key) ->
            ?assertEqual(
                {ok, maps:get(Key, Entries)},
                hb_store:read(Store, Key, #{})
            )
        end,
        [
            <<"kv/000000">>,
            <<"kv/000042">>,
            <<"kv/001199">>,
            <<"kv/sub/a">>,
            <<"big/blob">>
        ]
    ),
    ?assertEqual(
        {ok, maps:get(<<"kv/000042">>, Entries)},
        hb_store:read(Store, <<"alias/latest">>, #{})
    ),
    ?assertEqual(
        {ok, <<"kv/000042">>},
        hb_store:resolve(Store, <<"alias/latest">>, #{})
    ),
    ?assertEqual({ok, composite}, hb_store:type(Store, <<"kv">>, #{})),
    ?assertEqual({ok, simple}, hb_store:type(Store, <<"kv/000042">>, #{})),
    lists:foreach(
        fun(Key) ->
            ?assertEqual({error, not_found}, hb_store:read(Store, Key, #{})),
            ?assertEqual({error, not_found}, hb_store:type(Store, Key, #{})),
            ?assertEqual({error, not_found}, hb_store:list(Store, Key, #{}))
        end,
        [<<"kv/001200">>, <<"kv/999999">>, <<"zzz">>, <<"alias/missing">>]
    ),
    % A simple value is not a group, however many keys sort below it.
    ?assertEqual({error, not_found}, hb_store:list(Store, <<"kv/000042">>, #{})).

%% @doc The published fixture's `~match@1.0' group lists exactly the ids that
%% were written under it, which is the path `dev_match' takes.
published_fixture_match_group_test_() ->
    {timeout, 120, fun published_fixture_match_group/0}.
published_fixture_match_group() ->
    Store = [store(<<"arlmdb-fixture-match">>, ?PUBLISHED_FIXTURE)],
    Group = <<"~match@1.0&type=Message">>,
    Prefix = <<Group/binary, "/">>,
    Size = byte_size(Prefix),
    Expected =
        lists:sort(
            lists:filtermap(
                fun({Key, _Value}) ->
                    case Key of
                        <<Prefix:Size/binary, ID/binary>> -> {true, ID};
                        _ -> false
                    end
                end,
                hb_lmdb_page:fixture_entries()
            )
        ),
    ?assert(length(Expected) > 0),
    ?assertEqual({ok, Expected}, hb_store:list(Store, Group, #{})).

%% @doc A store carrying nothing but a locator answers from a published index of
%% ten million entries, with no index of its own and no warm-up.
published_index_test_() ->
    {timeout, 300, fun published_index/0}.
published_index() ->
    Store = [store(<<"arlmdb-index">>, ?PUBLISHED_INDEX)],
    {ok, _Located, Meta} = open(hd(Store), #{}),
    ?assertMatch(#{ <<"page-size">> := 65536 }, Meta),
    ?assert(maps:get(<<"entries">>, Meta) >= ?INDEX_ENTRIES),
    lists:foreach(
        fun(I) ->
            ?assertEqual(
                {ok, index_value(I)},
                hb_store:read(Store, index_key(I), #{})
            )
        end,
        [0, 1, 4242, 1_000_000, 5_000_000, ?INDEX_ENTRIES - 1]
    ),
    lists:foreach(
        fun(I) ->
            ?assertEqual(
                {error, not_found},
                hb_store:read(Store, index_key(I), #{})
            )
        end,
        [?INDEX_ENTRIES, ?INDEX_ENTRIES + 1, 12_345_678]
    ),
    % Every hundred-thousandth key also appears under the match group.
    {ok, Listed} = hb_store:list(Store, <<"~match@1.0&type=Message">>, #{}),
    ?assertEqual(?INDEX_ENTRIES div 100_000, length(Listed)),
    ?assert(lists:member(hb_util:encode(index_key(0)), Listed)).

%% @doc A cold lookup costs the depth of the tree plus one ranged read: one for
%% the meta pages, and one for each level it descends. Counted rather than
%% asserted, with the page store turned off so that the count is of the weave.
published_index_reads_test_() ->
    {timeout, 300, fun published_index_reads/0}.
published_index_reads() ->
    Store =
        (store(<<"arlmdb-index-reads">>, ?PUBLISHED_INDEX))#{
            <<"page-store">> => []
        },
    {ok, Located, #{ <<"depth">> := Depth }} = open(Store, #{}),
    ?assert(Depth > 1),
    lists:foreach(
        fun(I) ->
            Before = ranged_reads(),
            ?assertEqual(
                {ok, index_value(I)},
                hb_store:read([Located], index_key(I), #{})
            ),
            ?assertEqual(Depth + 1, ranged_reads() - Before)
        end,
        [0, 4242, ?INDEX_ENTRIES - 1]
    ).

%% @doc Every lookup crosses the meta pages and the top of the tree, and a
%% published database never changes, so a store that holds the pages it has
%% read pays for those levels once rather than once per lookup. The second
%% lookup of a key costs nothing at all, and a lookup of a different key costs
%% only the levels the two do not share.
published_index_page_store_test_() ->
    {timeout, 300, fun published_index_page_store/0}.
published_index_page_store() ->
    Store =
        (store(<<"arlmdb-index-pages">>, ?PUBLISHED_INDEX))#{
            <<"page-store">> =>
                [
                    #{
                        <<"store-module">> => hb_store_volatile,
                        <<"name">> =>
                            <<"arlmdb-pages-",
                                (hb_util:bin(erlang:unique_integer([positive])))/binary>>
                    }
                ]
        },
    {ok, Located, #{ <<"depth">> := Depth }} = open(Store, #{}),
    Read =
        fun(I) ->
            Before = ranged_reads(),
            ?assertEqual(
                {ok, index_value(I)},
                hb_store:read([Located], index_key(I), #{})
            ),
            ranged_reads() - Before
        end,
    % The first lookup pays for the whole descent, including the read that
    % `open/2' has already made of the meta pages.
    ?assert(Read(0) =< Depth + 1),
    ?assertEqual(0, Read(0)),
    % A different key shares the meta pages and the root, and pays only for
    % the levels below the point at which the two descents part.
    ?assert(Read(?INDEX_ENTRIES - 1) =< Depth - 1),
    ?assertEqual(0, Read(?INDEX_ENTRIES - 1)).

%% @doc Count the ranged reads that the store has made of the weave.
ranged_reads() ->
    lists:sum(
        [
            case catch prometheus_counter:value(hb_store_arlmdb_reads, [Kind]) of
                Value when is_number(Value) -> Value;
                _ -> 0
            end
        ||
            Kind <- [<<"meta">>, <<"page">>, <<"overflow">>]
        ]
    ).

%% @doc `hb_cache:match' finds the same messages through the published cache as
%% through a local one built from the same writes. The published database was
%% produced from a HyperBEAM store, so it carries the reverse index that
%% `hb_cache' writes at `~match@1.0&<key>=<value>/<id>' unchanged, and
%% `dev_match' reaches it through `list/3' alone.
published_cache_match_test_() ->
    {timeout, 600, fun published_cache_match/0}.
published_cache_match() ->
    Published = [store(<<"arlmdb-cache">>, ?PUBLISHED_CACHE)],
    PublishedOpts =
        #{ <<"store">> => Published, <<"match-index">> => Published },
    Local = [hb_test_utils:test_store(hb_store_lmdb)],
    LocalOpts = #{ <<"store">> => Local, <<"match-index">> => Local },
    % `index' is unique to each message, so both stores must find exactly the
    % one message that carries it, and agree on which it is.
    Written =
        [
            begin
                {ok, ID} = hb_cache:write(cache_message(I), LocalOpts),
                {I, ID}
            end
        ||
            I <- lists:seq(1, 200)
        ],
    lists:foreach(
        fun({I, ID}) ->
            Spec = #{ <<"index">> => integer_to_binary(I) },
            ?assertEqual({ok, [ID]}, hb_cache:match(Spec, LocalOpts)),
            ?assertEqual({ok, [ID]}, hb_cache:match(Spec, PublishedOpts))
        end,
        [lists:nth(1, Written), lists:nth(97, Written), lists:nth(200, Written)]
    ),
    % `cohort' is shared by an eighth of the messages, so the published cache
    % returns every one of them.
    {ok, Cohort} =
        hb_cache:match(
            #{ <<"type">> => <<"Message">>, <<"cohort">> => <<"3">> },
            PublishedOpts
        ),
    ?assertEqual(?CACHE_MESSAGES div 8, length(Cohort)),
    lists:foreach(
        fun({_I, ID}) -> ?assert(lists:member(ID, Cohort)) end,
        [{I, ID} || {I, ID} <- Written, I rem 8 == 3]
    ),
    % A key-value pair that no message carries matches nothing.
    ?assertEqual(
        {error, not_found},
        hb_cache:match(#{ <<"index">> => <<"999999">> }, PublishedOpts)
    ).

%% @doc The messages that the published cache was built from.
cache_message(I) ->
    #{
        <<"type">> => <<"Message">>,
        <<"cohort">> => integer_to_binary(I rem 8),
        <<"index">> => integer_to_binary(I),
        <<"data">> => <<"payload-", (integer_to_binary(I))/binary>>
    }.

%% @doc Entry `I' of the published index: a 32-byte data-item ID mapped to the
%% `hb_store_arweave_offset' encoding of the range that holds it. The ids are
%% generated with splitmix64 so that they spread over the whole key space, as
%% real ones do.
index_key(I) ->
    << <<(splitmix(4 * I + J)):64>> || J <- lists:seq(0, 3) >>.

index_value(I) ->
    hb_store_arweave_offset:encode(
        <<"tx@1.0">>,
        I * 4096,
        65536 + (I rem 1000)
    ).

splitmix(X) ->
    Mask = 16#FFFFFFFFFFFFFFFF,
    A = (X + 16#9E3779B97F4A7C15) band Mask,
    B = ((A bxor (A bsr 30)) * 16#BF58476D1CE4E5B9) band Mask,
    C = ((B bxor (B bsr 27)) * 16#94D049BB133111EB) band Mask,
    C bxor (C bsr 31).

%% @doc A message is found by one of its key-value pairs and then read back, out
%% of a database that lives on Arweave. This is the whole of the module's
%% purpose in one line: `hb_cache:match/2' reaches the reverse index through
%% `dev_match' and `list/3', and the message that the id it returns names is
%% read through the same store.
published_cache_read_test_() ->
    {timeout, 300, fun published_cache_read/0}.
published_cache_read() ->
    Store = [store(<<"arlmdb-cache-read">>, ?PUBLISHED_CACHE)],
    Opts = #{ <<"store">> => Store, <<"match-index">> => Store },
    {ok, [ID]} = hb_cache:match(#{ <<"index">> => <<"77">> }, Opts),
    {ok, Message} = hb_cache:read(ID, Opts),
    ?assert(
        hb_message:match(
            cache_message(77),
            hb_cache:ensure_all_loaded(Message, Opts),
            only_present
        )
    ).

%% @doc The published index stands in as `hb_store_arweave''s source of offsets,
%% which is what a published index is for: a node reading data items out of the
%% weave through a map from id to range that it never built itself.
published_index_as_offsets_test_() ->
    {timeout, 300, fun published_index_as_offsets/0}.
published_index_as_offsets() ->
    Arweave =
        #{
            <<"store-module">> => hb_store_arweave,
            <<"index-store">> => [store(<<"arlmdb-offsets">>, ?PUBLISHED_INDEX)]
        },
    lists:foreach(
        fun(I) ->
            ?assertEqual(
                {ok,
                    #{
                        <<"version">> => 1,
                        <<"codec-device">> => <<"tx@1.0">>,
                        <<"start-offset">> => I * 4096,
                        <<"length">> => 65536 + (I rem 1000)
                    }
                },
                hb_store_arweave:read_index_offset(
                    Arweave,
                    hb_util:encode(index_key(I))
                )
            )
        end,
        [0, 1, 4242, 5_000_000, ?INDEX_ENTRIES - 1]
    ).

%% The published copy of `hb_lmdb_page''s duplicate-set fixture: one main key
%% whose 3,000 duplicates are fifteen-byte rows of an eight-byte hash and a
%% seven-byte offset, the shape `~match@1.0' writes.
-define(PUBLISHED_SET, <<"mvtlyM3yZl_M2ZlCGUcUaABFqutCMRN0i_Pd9CfFeTs">>).
-define(SET_FIXTURE, "test/lmdb-1.0-dupfixed.mdb").

%% @doc A published sorted set answers by membership and by prefix.
%%
%% The store reads the database's flags rather than being told what it is: a
%% duplicate set is a database of its own, named by the single leaf node of the
%% main database, and its leaves carry fixed-width elements in place of nodes.
published_set_test_() ->
    {timeout, 300, fun published_set/0}.
published_set() ->
    Store = store(<<"arlmdb-set">>, ?PUBLISHED_SET),
    {ok, Located, Meta} = open(Store, #{}),
    ?assertMatch(#{ <<"width">> := 15, <<"entries">> := 3000 }, Meta),
    ?assertEqual(3, maps:get(<<"depth">>, Meta)),
    % An element reads back as an empty binary, and one the set does not hold
    % is absent rather than empty.
    ?assertEqual({ok, <<>>}, hb_store:read([Located], set_row(0, 0), #{})),
    ?assertEqual(
        {error, not_found},
        hb_store:read([Located], set_row(0, 100), #{})
    ),
    ?assertEqual(
        {error, not_found},
        hb_store:read([Located], <<"not-fifteen-bytes">>, #{})
    ),
    ?assertEqual({ok, simple}, hb_store:type([Located], set_row(0, 0), #{})),
    % A prefix is a group of the elements carrying it, named by what follows.
    ?assertEqual(
        {ok, [<<Offset:56>> || Offset <- lists:seq(0, 99)]},
        hb_store:list([Located], set_prefix(7), #{})
    ),
    lists:foreach(
        fun({Bounds, Expected}) ->
            ?assertEqual(
                {ok, Expected},
                hb_store:list(
                    [Located],
                    Bounds#{ <<"list">> => set_prefix(7) },
                    #{}
                )
            )
        end,
        [
            {#{ <<"limit">> => 3 }, [<<Offset:56>> || Offset <- [0, 1, 2]]},
            {
                #{ <<"from">> => <<40:56>>, <<"limit">> => 3 },
                [<<Offset:56>> || Offset <- [40, 41, 42]]
            },
            {
                #{
                    <<"from">> => <<40:56>>,
                    <<"limit">> => 3,
                    <<"direction">> => backward
                },
                [<<Offset:56>> || Offset <- [40, 39, 38]]
            },
            {
                #{ <<"limit">> => 2, <<"direction">> => backward },
                [<<Offset:56>> || Offset <- [99, 98]]
            },
            {#{ <<"from">> => <<100:56>> }, []}
        ]
    ),
    % A prefix no element carries has no elements.
    ?assertEqual({ok, []}, hb_store:list([Located], set_prefix(30), #{})).

%% @doc The same bytes, read from disk through `hb_store_lmdb' and from the
%% weave through `hb_store_arlmdb', answer identically.
%%
%% This is what publishing an ordinary LMDB file rather than a bespoke format
%% buys: the published index and a local copy of it are one artefact, and a
%% node chooses between downloading it and reading it where it lies without
%% anything standing between the two.
published_set_equivalence_test_() ->
    {timeout, 300, fun published_set_equivalence/0}.
published_set_equivalence() ->
    Directory =
        filename:join(
            <<"cache-TEST">>,
            <<"arlmdb-equivalence-",
                (hb_util:bin(erlang:unique_integer([positive])))/binary>>
        ),
    ok = filelib:ensure_dir(filename:join(Directory, <<"data.mdb">>)),
    {ok, Bytes} = file:read_file(?SET_FIXTURE),
    ok = file:write_file(filename:join(Directory, <<"data.mdb">>), Bytes),
    Local =
        #{
            <<"store-module">> => hb_store_lmdb,
            <<"name">> => Directory,
            <<"sorted-set">> => true
        },
    {ok, Remote, _Meta} = open(store(<<"arlmdb-equivalence">>, ?PUBLISHED_SET), #{}),
    Requests =
        [
            #{ <<"read">> => set_row(3, 7) },
            #{ <<"read">> => set_row(3, 100) },
            #{ <<"type">> => set_row(3, 7) },
            #{ <<"list">> => set_prefix(3) },
            #{ <<"list">> => set_prefix(3), <<"limit">> => 4 },
            #{
                <<"list">> => set_prefix(3),
                <<"from">> => <<60:56>>,
                <<"limit">> => 4
            },
            #{
                <<"list">> => set_prefix(3),
                <<"limit">> => 4,
                <<"direction">> => backward
            }
        ],
    Answers =
        lists:map(
            fun(Request) ->
                Callback = callback_of(Request),
                Answer =
                    apply(
                        hb_store, hb_util:atom(Callback), [[Local], Request, #{}]),
                ?assertEqual(
                    {Request, Answer},
                    {Request,
                        apply(
                            hb_store,
                            hb_util:atom(Callback),
                            [[Remote], Request, #{}]
                        )}
                ),
                Answer
            end,
            Requests
        ),
    % The answers agreeing is only worth something if they are the right ones.
    ?assertEqual(
        [
            {ok, <<>>},
            {error, not_found},
            {ok, simple},
            {ok, [<<Offset:56>> || Offset <- lists:seq(0, 99)]},
            {ok, [<<Offset:56>> || Offset <- [0, 1, 2, 3]]},
            {ok, [<<Offset:56>> || Offset <- [60, 61, 62, 63]]},
            {ok, [<<Offset:56>> || Offset <- [99, 98, 97, 96]]}
        ],
        Answers
    ),
    file:del_dir_r(Directory).

%% The key a request carries its path under names the callback it is for.
callback_of(#{ <<"read">> := _ }) -> <<"read">>;
callback_of(#{ <<"type">> := _ }) -> <<"type">>;
callback_of(#{ <<"list">> := _ }) -> <<"list">>.

%% The fixture's rows, as `hb_lmdb_page:fixture_dup_rows/0' builds them.
set_row(Hash, Offset) -> <<Hash:64, Offset:56>>.
set_prefix(Hash) -> <<Hash:64>>.

%% A published index built by `~copycat@1.0' in `full' mode over block
%% 1,889,322: 1,223 rows, small enough that its duplicate set is held inside
%% the node that names it rather than promoted to a database of its own. Six of
%% its rows are the items tagged `App-Name: ArDrive-App' in that block, and one
%% more is the sentinel that stands for an item whose position was not known.
-define(PUBLISHED_BLOCK_INDEX,
    <<"z6yYEGs4XrHxqcElbSSJUqJ6xs5eM0C63QdXLYsCybA">>).
-define(BLOCK_INDEX_PREDICATE, <<"~match@1.0/p_JoMqZ0uG8">>).
-define(BLOCK_INDEX_OFFSETS,
    [
        0,
        386310990766550,
        386310990767812,
        386310990769443,
        386310990770705,
        386310990772336,
        386310990773598
    ]).

%% @doc A published index answers through the store definition of the design:
%% the rows are fifteen raw bytes, and the paths that reach them are
%% `~match@1.0/<hash>/<offset>'.
published_block_index_test_() ->
    {timeout, 300, fun published_block_index/0}.
published_block_index() ->
    Store =
        (store(<<"arlmdb-block-index">>, ?PUBLISHED_BLOCK_INDEX))#{
            <<"prefix">> => <<"~match@1.0/">>,
            <<"path-normalization">> =>
                [<<"decode-base64url">>, <<"decode-int-56">>],
            <<"strip-slashes">> => true
        },
    Offsets = [hb_util:bin(Offset) || Offset <- ?BLOCK_INDEX_OFFSETS],
    ?assertEqual(
        {ok, Offsets},
        hb_store:list([Store], ?BLOCK_INDEX_PREDICATE, #{})
    ),
    % The bounds seek into the set rather than reading up to their start.
    ?assertEqual(
        {ok, lists:sublist(Offsets, 2)},
        hb_store:list(
            [Store],
            #{ <<"list">> => ?BLOCK_INDEX_PREDICATE, <<"limit">> => 2 },
            #{}
        )
    ),
    ?assertEqual(
        {ok, lists:sublist(Offsets, 4, 2)},
        hb_store:list(
            [Store],
            #{
                <<"list">> => ?BLOCK_INDEX_PREDICATE,
                <<"from">> => lists:nth(4, Offsets),
                <<"limit">> => 2
            },
            #{}
        )
    ),
    ?assertEqual(
        {ok, lists:reverse(lists:nthtail(5, Offsets))},
        hb_store:list(
            [Store],
            #{
                <<"list">> => ?BLOCK_INDEX_PREDICATE,
                <<"limit">> => 2,
                <<"direction">> => backward
            },
            #{}
        )
    ),
    % A row of the set reads back as an empty binary, and one it does not hold
    % is absent.
    ?assertEqual(
        {ok, <<>>},
        hb_store:read(
            [Store],
            <<?BLOCK_INDEX_PREDICATE/binary, "/",
                (lists:nth(2, Offsets))/binary>>,
            #{}
        )
    ),
    ?assertEqual(
        {error, not_found},
        hb_store:read(
            [Store],
            <<?BLOCK_INDEX_PREDICATE/binary, "/1">>,
            #{}
        )
    ),
    % A predicate the index does not hold has no rows.
    ?assertEqual(
        {ok, []},
        hb_store:list([Store], <<"~match@1.0/AAAAAAAAAAA">>, #{})
    ).

%% @doc A page taken from the middle of a published set costs no more ranged
%% reads of the weave than one taken from its start.
%%
%% `dev_match' counts what it asks of a store; this counts what the store does
%% to answer, which is where a scan up to the cursor would show itself.
published_block_index_reads_test_() ->
    {timeout, 300, fun published_block_index_reads/0}.
published_block_index_reads() ->
    Store =
        (store(<<"arlmdb-block-index-reads">>, ?PUBLISHED_BLOCK_INDEX))#{
            <<"prefix">> => <<"~match@1.0/">>,
            <<"path-normalization">> =>
                [<<"decode-base64url">>, <<"decode-int-56">>],
            <<"strip-slashes">> => true,
            % The page store would answer every read after the first, which is
            % the opposite of what this measures.
            <<"page-store">> => []
        },
    Offsets = [hb_util:bin(Offset) || Offset <- ?BLOCK_INDEX_OFFSETS],
    Page =
        fun(Bounds) ->
            Before = ranged_reads(),
            {ok, Found} =
                hb_store:list(
                    [Store],
                    Bounds#{ <<"list">> => ?BLOCK_INDEX_PREDICATE },
                    #{}
                ),
            {Found, ranged_reads() - Before}
        end,
    {First, FirstReads} = Page(#{ <<"limit">> => 2 }),
    ?assertEqual(lists:sublist(Offsets, 2), First),
    ?assert(FirstReads > 0),
    {Later, LaterReads} =
        Page(#{ <<"from">> => lists:nth(6, Offsets), <<"limit">> => 2 }),
    ?assertEqual(lists:sublist(Offsets, 6, 2), Later),
    ?assertEqual(FirstReads, LaterReads).
