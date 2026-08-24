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
        {ok, Resolved, Value} ?= resolved(Located, Meta, key(Path), Opts),
        case Value of
            <<"group">> -> composite(Located, Meta, Resolved, Opts);
            _ -> {ok, Value}
        end
    end.

%% @doc Classify the entry at a key as a group or a direct value.
type(Store, #{ <<"type">> := Path }, Opts) ->
    maybe
        {ok, Located, Meta} ?= open(Store, Opts),
        {ok, _Resolved, Value} ?= resolved(Located, Meta, key(Path), Opts),
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
        {ok, Parts} ?=
            resolve_links(Located, Meta, split(key(Path), [global]), Opts),
        {ok, join(Parts)}
    end.

%% @doc List the immediate children of a group. A path that does not carry a
%% `group' marker is not a group, even where keys sit below it: the marker is
%% what `hb_cache' writes, and a listing that ignored it would disagree with the
%% same database read locally.
list(Store, #{ <<"list">> := Path }, Opts) ->
    maybe
        {ok, Located, Meta} ?= open(Store, Opts),
        {ok, Resolved, Value} ?= resolved(Located, Meta, key(Path), Opts),
        case Value of
            <<"group">> -> names(children(Located, Meta, Resolved, Opts));
            _ -> {error, not_found}
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
        {ok, Located, Meta}
    end.

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
    hb_prometheus:inc(counter, hb_store_arlmdb_reads, [hb_util:bin(Kind)]),
    hb_prometheus:measure_and_report(
        fun() -> read_chunks(Store, Offset, Length, Opts) end,
        hb_store_arlmdb_read_duration_seconds,
        [hb_util:bin(Kind)]
    ).

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
    case next_leaf(Store, Meta, Stack, Opts) of
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

%% @doc Reduce a group's children to their names alone, which is what `list/3'
%% answers with; a composite read carries the values along with them.
names({ok, Children}) -> {ok, [Name || {Name, _Value} <- Children]};
names({error, _} = Error) -> Error.

%% @doc Descend to the leaf that holds the first key at or after the given one,
%% keeping the branch pages walked through and the child of each that was taken.
%% That stack is what lets the scan move on to the next leaf.
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

%% @doc Take the next leaf after the one a scan has exhausted: climb to the
%% nearest branch page with a child left to take, step to it, and descend to the
%% leftmost leaf below.
next_leaf(_Store, _Meta, [], _Opts) ->
    {error, no_more_leaves};
next_leaf(Store, Meta, [{Page, Index} | Above], Opts) ->
    case hb_lmdb_page:num_keys(Page) of
        {ok, Keys} when Index + 1 < Keys ->
            take_child(Store, Meta, Page, Index + 1, Above, Opts);
        {ok, _Keys} ->
            next_leaf(Store, Meta, Above, Opts);
        {error, _} = Error ->
            Error
    end.

take_child(Store, Meta, Page, Index, Above, Opts) ->
    case hb_lmdb_page:node(Page, Index) of
        {ok, _Key, {branch, Child}} ->
            leftmost(
                Store, Meta, Child, [{Page, Index} | Above],
                max_depth(Store, Meta), Opts
            );
        {ok, _Key, _Reference} ->
            {error, invalid_branch_page};
        {error, _} = Error ->
            Error
    end.

leftmost(_Store, _Meta, _Number, _Stack, 0, _Opts) ->
    {error, descent_too_deep};
leftmost(Store, Meta, Number, Stack, Remaining, Opts) ->
    maybe
        {ok, Page} ?= page(Store, Meta, Number, Opts),
        {ok, Parsed} ?= hb_lmdb_page:page(Page),
        leftmost_page(Store, Meta, Page, Parsed, Stack, Remaining, Opts)
    end.

leftmost_page(Store, Meta, Page, #{ <<"type">> := branch }, Stack, Remaining, Opts) ->
    case hb_lmdb_page:node(Page, 0) of
        {ok, _Key, {branch, Child}} ->
            leftmost(
                Store, Meta, Child, [{Page, 0} | Stack], Remaining - 1, Opts
            );
        {ok, _Key, _Reference} ->
            {error, invalid_branch_page};
        {error, _} = Error ->
            Error
    end;
leftmost_page(
        _Store, _Meta, Page, #{ <<"type">> := leaf, <<"keys">> := Keys },
        Stack, _Remaining, _Opts
    ) ->
    {ok, Stack, Page, Keys, 0};
leftmost_page(_Store, _Meta, _Page, #{ <<"type">> := Type }, _Stack, _R, _Opts) ->
    {error, {not_a_node_page, Type}}.

%% @doc Pair a leaf with the index a scan of it starts from.
cursor(_Stack, _Page, {error, _} = Error) ->
    Error;
cursor(Stack, Page, Index) ->
    case hb_lmdb_page:num_keys(Page) of
        {ok, Keys} -> {ok, Stack, Page, Keys, Index};
        {error, _} = Error -> Error
    end.

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

%% @doc A lookup costs the depth of the tree plus one ranged read: one for the
%% meta page, and one for each level it descends. Counted rather than asserted.
published_index_reads_test_() ->
    {timeout, 300, fun published_index_reads/0}.
published_index_reads() ->
    Store = store(<<"arlmdb-index-reads">>, ?PUBLISHED_INDEX),
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
