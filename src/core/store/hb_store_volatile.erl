%%% @doc A lightweight in-memory HyperBEAM store backed by a single ETS
%%% `ordered_set`. The store is volatile: it does not persist data to disk
%%% ever, and -- critically -- can be configured to expire all data
%%% periodically. This is useful for testing and as a short-term in-memory
%%% cache, not for instances where an `ok` from the `write` function should
%%% imply data persistence.
%%%
%%% Each entry is stored as `{Path, {raw, Bin} | {link, Target} | group}`.
%%% Group membership is discovered by a lexicographic range scan over the
%%% `Path ++ "/"` prefix, mirroring the LMDB and FS store layouts. An
%%% explicit `group` marker is inserted at every ancestor path so that
%%% `type/3` stays a single lookup.
-module(hb_store_volatile).
-export([start/3, stop/3, reset/3, scope/0, scope/1]).
-export([write/3, read/3, list/3, type/3, link/3, group/3, resolve/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ROOT_GROUP, <<"/">>).
-define(MAX_REDIRECTS, 32).

%% @doc Start the ETS-backed store and return the store instance message.
start(StoreOpts = #{ <<"name">> := Name }, _Req, _Opts) ->
    ?event(store_volatile, {starting_ets_store, Name}),
    Parent = self(),
    spawn(
        fun() ->
            Table = ets:new(hb_store_volatile, [
                ordered_set,
                public,
                {read_concurrency, true},
                {write_concurrency, true}
            ]),
            Parent ! {ok, #{ <<"pid">> => self(), <<"ets-table">> => Table }},
            maybe_start_ttl_timer(StoreOpts, self()),
            owner_loop(StoreOpts)
        end
    ),
    receive
        {ok, InstanceMessage} ->
            {ok, InstanceMessage}
    after 5000 ->
        {error, start_timeout}
    end.

%% @doc Owner loop for the ETS store. Supervised long-lived server: blocks
%% indefinitely on stop/reset/other messages; the table stays alive until
%% the owner exits.
owner_loop(StoreOpts) ->
    receive
        {stop, From, Ref} ->
            From ! {ok, Ref},
            exit(normal);
        reset ->
            reset_store(StoreOpts),
            maybe_start_ttl_timer(StoreOpts, self()),
            owner_loop(StoreOpts);
        _ ->
            owner_loop(StoreOpts)
    end.

maybe_start_ttl_timer(StoreOpts, PID) ->
    case maps:get(<<"max-ttl-ms">>, StoreOpts, undefined) of
        undefined ->
            case maps:get(<<"max-ttl">>, StoreOpts, infinity) of
                infinity -> skip;
                MaxTTL ->
                    timer:send_after(hb_util:int(MaxTTL) * 1000, PID, reset)
            end;
        MaxTTLMs ->
            timer:send_after(hb_util:int(MaxTTLMs), PID, reset)
    end.

%% @doc Stop the ETS owner process (which also drops the table).
stop(Opts, _Req, _NodeOpts) ->
    #{ <<"pid">> := Pid } = hb_store:find(Opts),
    Pid ! {stop, self(), Ref = make_ref()},
    receive
        {ok, Ref} -> ok
    after 5000 ->
        ok
    end.

%% @doc Scope for this store backend.
scope() -> local.
scope(_) -> scope().

%% @doc Remove all entries from the ETS table.
reset(Opts, _Req, _NodeOpts) ->
    reset_store(Opts).

reset_store(Opts) ->
    Table = table(Opts),
    ets:delete_all_objects(Table),
    ?event(store_volatile, {reset, {table, Table}}),
    ok.

%% @doc Write one or more entries. Request maps are folded into individual
%% writes. Any raw/link entry at an ancestor path is converted to a group
%% marker (raw/link entries have no descendants, so no subtree purge is
%% needed). If the target key previously held a group, its descendants are
%% deleted first.
write(Opts, Req, _NodeOpts) when is_map(Req) ->
    maps:fold(
        fun(Path, Value, ok) ->
            put_entry(Opts, Path, {raw, Value});
           (_Path, _Value, Error) ->
            Error
        end,
        ok,
        Req
    ).

%% @doc Read a value, following links when needed. Group paths return
%% `{composite, Children}` with the immediate child names.
read(Opts, #{ <<"read">> := RawKey }, _NodeOpts) ->
    read_resolved(Opts, resolve_path(Opts, RawKey), 0).

read_resolved(_Opts, _Key, Depth) when Depth > ?MAX_REDIRECTS ->
    {error, not_found};
read_resolved(Opts, Key, Depth) ->
    case lookup_entry(Opts, Key) of
        {raw, Value} ->
            ?event(store_volatile, {hit, {key, Key}}),
            {ok, Value};
        group ->
            ?event(store_volatile, {hit, {key, Key}}),
            {composite, immediate_children(Opts, Key)};
        {link, Link} ->
            ?event(store_volatile, {hit, {key, Key}}),
            read_resolved(Opts, hb_path:to_binary(Link), Depth + 1);
        _ ->
            ?event(store_volatile, {miss, {key, Key}}),
            {error, not_found}
    end.

%% @doc Resolve a path segment-by-segment, following any links encountered
%% at intermediate positions.
resolve(Opts, #{ <<"resolve">> := Key }, _NodeOpts) ->
    {ok, resolve_path(Opts, Key)}.

resolve_path(Opts, Key) ->
    resolve_path(
        Opts,
        <<>>,
        hb_path:term_to_path_parts(hb_path:to_binary(Key), Opts),
        0
    ).

resolve_path(_Opts, CurrPath, [], _Depth) ->
    hb_path:to_binary(CurrPath);
resolve_path(_Opts, CurrPath, _Rest, Depth) when Depth > ?MAX_REDIRECTS ->
    hb_path:to_binary(CurrPath);
resolve_path(Opts, CurrPath, [Next | Rest], Depth) ->
    PathPart = join_path(CurrPath, Next),
    case lookup_entry(Opts, PathPart) of
        {link, Link} ->
            resolve_path(Opts, hb_path:to_binary(Link), Rest, Depth + 1);
        _ ->
            resolve_path(Opts, PathPart, Rest, Depth)
    end.

%% @doc List immediate child names under a group path.
list(Opts, #{ <<"list">> := RawPath }, _NodeOpts) ->
    list_path(Opts, hb_path:to_binary(RawPath)).

list_path(Opts, <<"">>) ->
    list_path(Opts, ?ROOT_GROUP);
list_path(Opts, Path) ->
    case lookup_entry(Opts, Path) of
        group ->
            {ok, immediate_children(Opts, Path)};
        {link, Link} ->
            list_path(Opts, hb_path:to_binary(Link));
        nil when Path =:= ?ROOT_GROUP ->
            %% Empty store at root — no entries here; return `not_found`
            %% rather than `{ok, []}` so a chained store can serve the
            %% request.
            {error, not_found};
        nil ->
            %% Not at this exact path; try resolving intermediate-segment
            %% links. If resolution yields the same path, it's truly absent.
            case resolve_path(Opts, Path) of
                Path -> {error, not_found};
                Resolved -> list_path(Opts, Resolved)
            end;
        _ ->
            {error, not_found}
    end.

%% @doc Determine the item type at a path.
type(Opts, #{ <<"type">> := RawKey }, _NodeOpts) ->
    type_path(Opts, RawKey).

type_path(Opts, RawKey) ->
    Key = resolve_path(Opts, RawKey),
    case lookup_entry(Opts, Key) of
        {raw, _} -> {ok, simple};
        group -> {ok, composite};
        {link, Link} -> type_path(Opts, hb_path:to_binary(Link));
        _ -> {error, not_found}
    end.

%% @doc Ensure a group exists at the given path. Idempotent on existing
%% groups; converts a raw/link entry to an empty group.
group(Opts, #{ <<"group">> := RawKey }, _NodeOpts) ->
    Key = hb_path:to_binary(RawKey),
    Table = table(Opts),
    case lookup_entry(Table, Key) of
        group -> ok;
        _ -> put_entry(Opts, Key, group)
    end,
    ok.

%% @doc Create or replace one or more links. Request maps are folded; each
%% `New => Existing` pair installs a link at New targeting Existing.
link(Opts, Req, _NodeOpts) when is_map(Req) ->
    maps:fold(
        fun(New, Existing, ok) ->
            link_path(Opts, New, Existing);
           (_New, _Existing, Error) ->
            Error
        end,
        ok,
        Req
    ).

link_path(_Opts, Same, Same) ->
    ok;
link_path(Opts, RawNew, RawExisting) ->
    put_entry(Opts, RawNew, {link, hb_path:to_binary(RawExisting)}).

%% @doc Install an entry at Key, purging any prior group subtree and
%% ensuring ancestor group markers exist.
put_entry(Opts, RawKey, Entry) ->
    Key = hb_path:to_binary(RawKey),
    Table = table(Opts),
    maybe_delete_subtree(Table, Key),
    ensure_parent_groups(Table, Key),
    ?event(store_volatile, {put, {key, Key}}),
    ets:insert(Table, {Key, Entry}),
    ok.

table(Opts) ->
    #{ <<"ets-table">> := Table } = hb_store:find(Opts),
    Table.

join_path(<<>>, Next) ->
    hb_path:to_binary(Next);
join_path(CurrPath, Next) ->
    hb_path:to_binary([CurrPath, Next]).

lookup_entry(Opts, Key) when is_map(Opts) ->
    lookup_entry(table(Opts), Key);
lookup_entry(Table, Key) ->
    case ets:lookup(Table, Key) of
        [] -> nil;
        [{_, Entry}] -> Entry
    end.

%% @doc Walk ancestor paths from root to parent, inserting a `group` marker
%% via unconditional `ets:insert/2` at each. A raw or link entry sitting at
%% an intermediate path is converted to a group — those entry types have no
%% descendants, so no subtree purge is required.
ensure_parent_groups(Table, Key) ->
    case filename:dirname(Key) of
        <<".">> ->
            ets:insert(Table, {?ROOT_GROUP, group});
        ParentDir ->
            ensure_dir(Table, ParentDir)
    end.

ensure_dir(Table, Path) ->
    ets:insert(Table, {?ROOT_GROUP, group}),
    ensure_dir(Table, ?ROOT_GROUP, hb_path:term_to_path_parts(Path)).

ensure_dir(_Table, _CurrentGroup, []) ->
    ok;
ensure_dir(Table, CurrentGroup, [Next | Rest]) ->
    NextGroup = next_group_path(CurrentGroup, Next),
    ets:insert(Table, {NextGroup, group}),
    ensure_dir(Table, NextGroup, Rest).

next_group_path(?ROOT_GROUP, Next) ->
    hb_path:to_binary(Next);
next_group_path(CurrentGroup, Next) ->
    hb_path:to_binary([CurrentGroup, Next]).

%% @doc If `Key` currently holds a `group` marker, delete all descendants
%% (entries whose path starts with `<<Key/binary, $/>>`).
maybe_delete_subtree(Table, Key) ->
    case lookup_entry(Table, Key) of
        group ->
            Prefix = <<Key/binary, $/>>,
            PLen = byte_size(Prefix),
            ets:select_delete(Table, [
                {{'$1', '_'},
                 [{'=:=', {binary_part, '$1', 0, PLen}, {const, Prefix}}],
                 [true]}
            ]);
        _ ->
            ok
    end.

%% @doc Return immediate child names under GroupPath. Walks the ordered_set
%% with `ets:next/2`, jumping past each child's own subtree after it has
%% been recorded (see `advance_past_child/4`). Cost is O(immediate
%% children), not O(descendants). For root listings the scan must start
%% from the true beginning of the table — `ets:next(Table, ?ROOT_GROUP)`
%% would skip any top-level key whose binary sort order is less than
%% `<<"/">>` (for example base64url paths starting with `-`) — so the
%% root marker row itself is skipped explicitly inside the loop instead.
immediate_children(Opts, GroupPath) ->
    Table = table(Opts),
    Seen =
        case GroupPath of
            ?ROOT_GROUP ->
                collect_immediate_children(
                    Table, <<>>, ets:first(Table), #{});
            _ ->
                Prefix = <<GroupPath/binary, $/>>,
                collect_immediate_children(
                    Table, Prefix, ets:next(Table, Prefix), #{})
        end,
    maps:keys(Seen).

collect_immediate_children(_Table, _Prefix, '$end_of_table', Seen) ->
    Seen;
collect_immediate_children(Table, <<>> = Prefix, ?ROOT_GROUP, Seen) ->
    collect_immediate_children(Table, Prefix, ets:next(Table, ?ROOT_GROUP), Seen);
collect_immediate_children(Table, Prefix, Key, Seen) ->
    PLen = byte_size(Prefix),
    case Key of
        <<Prefix:PLen/binary, Rest/binary>> ->
            [Name | _] = binary:split(Rest, <<"/">>),
            case is_map_key(Name, Seen) of
                true ->
                    %% Re-entered a subtree we already recorded (possible
                    %% when a sibling like `<<Name, X>>` with X < $/ is
                    %% interleaved in ordered_set iteration). Jump past
                    %% Name's subtree in one step.
                    NextKey = skip_subtree(Table, Prefix, Name),
                    collect_immediate_children(Table, Prefix, NextKey, Seen);
                false ->
                    NextKey = advance_past_child(Table, Prefix, Name, Key),
                    collect_immediate_children(
                        Table, Prefix, NextKey, Seen#{Name => true})
            end;
        _ ->
            Seen
    end.

%% @doc Advance to the next sibling of Name under Prefix. Plain `ets:next`
%% handles the shallow case at one-op cost; if it lands inside Name's own
%% subtree, jump past it via `skip_subtree/3`.
advance_past_child(Table, Prefix, Name, Key) ->
    case ets:next(Table, Key) of
        '$end_of_table' ->
            '$end_of_table';
        NextKey ->
            SubtreePrefix = <<Prefix/binary, Name/binary, $/>>,
            SPLen = byte_size(SubtreePrefix),
            case NextKey of
                <<SubtreePrefix:SPLen/binary, _/binary>> ->
                    skip_subtree(Table, Prefix, Name);
                _ ->
                    NextKey
            end
    end.

%% @doc Jump past every key under `<<Prefix, Name, $/>>`. The lex successor
%% of that prefix is `<<Prefix, Name, $0>>` (since $0 = $/ + 1); an explicit
%% `ets:member` probe preserves a literal sibling named `<<Name/binary, "0">>`
%% if one exists (possible with hex-TXID-shaped keys).
skip_subtree(Table, Prefix, Name) ->
    SkipPast = <<Prefix/binary, Name/binary, $0>>,
    case ets:member(Table, SkipPast) of
        true -> SkipPast;
        false -> ets:next(Table, SkipPast)
    end.

%%% Tests

max_ttl_test() ->
    StoreOpts =
        #{
            <<"store-module">> => ?MODULE,
            <<"name">> => <<"ets-max-ttl-test">>,
            <<"max-ttl-ms">> => 100
        },
    ok = hb_store:start(StoreOpts),
    ok = hb_store:write(StoreOpts, #{ <<"a">> => <<"b">> }, #{}),
    ?assertEqual({ok, <<"b">>}, hb_store:read(StoreOpts, <<"a">>, #{})),
    timer:sleep(200),
    ?assertEqual({error, not_found}, hb_store:read(StoreOpts, <<"a">>, #{})),
    ok = hb_store:write(StoreOpts, #{ <<"a">> => <<"c">> }, #{}),
    ?assertEqual({ok, <<"c">>}, hb_store:read(StoreOpts, <<"a">>, #{})),
    timer:sleep(200),
    ?assertEqual({error, not_found}, hb_store:read(StoreOpts, <<"a">>, #{})),
    ok = hb_store:stop(StoreOpts).

empty_root_reports_not_found_test() ->
    %% A fresh store with no writes must report `{error, not_found}` on
    %% `list(<<"/">>)` so that, when used first in a store chain, the
    %% dispatcher falls through to the next store instead of stopping on
    %% a spurious `{ok, []}`.
    S = hb_test_utils:test_store(?MODULE, <<"empty-root-test">>),
    hb_store:start(S),
    ?assertEqual({error, not_found}, hb_store:list(S, <<"/">>, #{})),
    ok = hb_store:stop(S).

list_root_test() ->
    S = hb_test_utils:test_store(?MODULE, <<"list-root-test">>),
    hb_store:start(S),
    ok = hb_store:write(S, #{ <<"alpha">> => <<"1">>,
                              <<"beta/child">> => <<"2">> }, #{}),
    {ok, Keys} = hb_store:list(S, <<"/">>, #{}),
    ?assertNot(lists:member(<<>>, Keys)),
    ?assertEqual([<<"alpha">>, <<"beta">>], lists:sort(Keys)),
    ok = hb_store:stop(S).

list_root_includes_pre_slash_keys_test() ->
    %% Top-level keys whose first byte sorts before "/" (47) — e.g. "-" (45),
    %% common in base64url/TXID paths — must appear in the root listing.
    S = hb_test_utils:test_store(?MODULE, <<"list-root-dash-test">>),
    hb_store:start(S),
    ok = hb_store:write(S, #{ <<"-a">> => <<"1">>,
                              <<"alpha">> => <<"2">> }, #{}),
    {ok, Keys} = hb_store:list(S, <<"/">>, #{}),
    ?assertEqual([<<"-a">>, <<"alpha">>], lists:sort(Keys)),
    ok = hb_store:stop(S).

list_test() ->
    S = hb_test_utils:test_store(?MODULE, <<"list-test">>),
    hb_store:start(S),
    ok = hb_store:write(S,
        #{ <<"colors/red">> => <<"1">>,
           <<"colors/blue">> => <<"2">>,
           <<"colors/multi/foo">> => <<"3">> }, #{}),
    {ok, Children} = hb_store:list(S, <<"colors">>, #{}),
    ?assertEqual(
        [<<"blue">>, <<"multi">>, <<"red">>], lists:sort(Children)),
    {ok, Deep} = hb_store:list(S, <<"colors/multi">>, #{}),
    ?assertEqual([<<"foo">>], Deep),
    ok = hb_store:stop(S).

list_dedup_test() ->
    S = hb_test_utils:test_store(?MODULE, <<"list-dedup-test">>),
    hb_store:start(S),
    ok = hb_store:link(S,
        #{ <<"a/link">> => <<"target1">> }, #{}),
    ok = hb_store:link(S,
        #{ <<"a/link">> => <<"target2">> }, #{}),
    {ok, Children} = hb_store:list(S, <<"a">>, #{}),
    ?assertEqual([<<"link">>], lists:usort(Children)),
    ok = hb_store:stop(S).

list_with_link_test() ->
    S = hb_test_utils:test_store(?MODULE, <<"list-with-link-test">>),
    hb_store:start(S),
    ok = hb_store:write(S, #{ <<"real/child">> => <<"v">> }, #{}),
    ok = hb_store:link(S, #{ <<"alias">> => <<"real">> }, #{}),
    {ok, Children} = hb_store:list(S, <<"alias">>, #{}),
    ?assertEqual([<<"child">>], Children),
    ok = hb_store:stop(S).

overwrite_link_to_raw_test() ->
    S = hb_test_utils:test_store(?MODULE, <<"overwrite-link-to-raw-test">>),
    hb_store:start(S),
    ok = hb_store:link(S, #{ <<"p/x">> => <<"target">> }, #{}),
    ok = hb_store:write(S, #{ <<"p/x">> => <<"val">> }, #{}),
    ?assertEqual({ok, <<"val">>}, hb_store:read(S, <<"p/x">>, #{})),
    ok = hb_store:stop(S).

overwrite_group_to_raw_test() ->
    S = hb_test_utils:test_store(?MODULE, <<"overwrite-group-to-raw-test">>),
    hb_store:start(S),
    ok = hb_store:write(S, #{ <<"g/child">> => <<"v">> }, #{}),
    ?assertEqual({ok, composite}, hb_store:type(S, <<"g">>, #{})),
    ok = hb_store:write(S, #{ <<"g">> => <<"raw">> }, #{}),
    ?assertEqual({ok, simple}, hb_store:type(S, <<"g">>, #{})),
    ?assertEqual({error, not_found}, hb_store:read(S, <<"g/child">>, #{})),
    ok = hb_store:group(S, <<"g">>, #{}),
    {ok, Empty} = hb_store:list(S, <<"g">>, #{}),
    ?assertEqual([], Empty),
    ok = hb_store:stop(S).

overwrite_group_to_link_test() ->
    S = hb_test_utils:test_store(?MODULE, <<"overwrite-group-to-link-test">>),
    hb_store:start(S),
    ok = hb_store:write(S, #{ <<"g/child">> => <<"v">> }, #{}),
    ok = hb_store:write(S, #{ <<"other/x">> => <<"w">> }, #{}),
    ok = hb_store:link(S, #{ <<"g">> => <<"other">> }, #{}),
    ?assertEqual({error, not_found}, hb_store:read(S, <<"g/child">>, #{})),
    {ok, Children} = hb_store:list(S, <<"g">>, #{}),
    ?assertEqual([<<"x">>], Children),
    ok = hb_store:stop(S).

implicit_group_conversion_test() ->
    S = hb_test_utils:test_store(?MODULE, <<"implicit-group-conversion-test">>),
    hb_store:start(S),
    ok = hb_store:write(S, #{ <<"a">> => <<"raw_val">> }, #{}),
    ?assertEqual({ok, simple}, hb_store:type(S, <<"a">>, #{})),
    ok = hb_store:write(S, #{ <<"a/b">> => <<"child_val">> }, #{}),
    ?assertEqual({ok, composite}, hb_store:type(S, <<"a">>, #{})),
    {ok, Children} = hb_store:list(S, <<"a">>, #{}),
    ?assertEqual([<<"b">>], Children),
    ok = hb_store:stop(S).

list_deep_subtree_test() ->
    S = hb_test_utils:test_store(?MODULE, <<"list-deep-test">>),
    hb_store:start(S),
    Writes = maps:from_list(
        [
            {<<"root/heavy/d", (integer_to_binary(I))/binary>>, <<"v">>}
        ||
            I <- lists:seq(1, 200)
        ]
    ),
    ok = hb_store:write(S, Writes#{ <<"root/other">> => <<"v">> }, #{}),
    {ok, Children} = hb_store:list(S, <<"root">>, #{}),
    ?assertEqual([<<"heavy">>, <<"other">>], lists:sort(Children)),
    ok = hb_store:stop(S).

sibling_with_zero_suffix_test() ->
    S = hb_test_utils:test_store(?MODULE, <<"sibling-zero-test">>),
    hb_store:start(S),
    ok = hb_store:write(S,
        #{ <<"p/a">> => <<"1">>,
           <<"p/a0">> => <<"2">>,
           <<"p/a/leaf">> => <<"3">> }, #{}),
    {ok, Children} = hb_store:list(S, <<"p">>, #{}),
    ?assertEqual([<<"a">>, <<"a0">>], lists:sort(Children)),
    ok = hb_store:stop(S).

list_large_flat_group_test() ->
    %% Regression for the quadratic-dedup issue: listing a flat group with
    %% many children must be linear in the child count, not quadratic.
    S = hb_test_utils:test_store(?MODULE, <<"list-large-flat-test">>),
    hb_store:start(S),
    N = 5000,
    Writes = maps:from_list(
        [{<<"grp/k", (integer_to_binary(I))/binary>>, <<"v">>}
         || I <- lists:seq(1, N)]
    ),
    ok = hb_store:write(S, Writes, #{}),
    {_, {ok, Children}} = timer:tc(
        fun() -> hb_store:list(S, <<"grp">>, #{}) end),
    ?assertEqual(N, length(Children)),
    ?assertEqual(N, length(lists:usort(Children))),
    ok = hb_store:stop(S).

prefixed_sibling_no_duplicate_test() ->
    %% Child "a" has a subtree ("a/leaf") and also a sibling "a-" whose
    %% first differentiating byte sorts before "/" (45 < 47). ordered_set
    %% iteration visits p/a, then p/a-, then p/a/leaf — re-entering a's
    %% subtree after a sibling. list/3 must not report "a" twice.
    S = hb_test_utils:test_store(?MODULE, <<"prefixed-sibling-test">>),
    hb_store:start(S),
    ok = hb_store:write(S,
        #{ <<"p/a/leaf">> => <<"1">>,
           <<"p/a-">> => <<"2">> }, #{}),
    {ok, Children} = hb_store:list(S, <<"p">>, #{}),
    ?assertEqual([<<"a">>, <<"a-">>], lists:sort(Children)),
    ok = hb_store:stop(S).
