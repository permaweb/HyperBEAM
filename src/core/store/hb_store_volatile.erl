%%% @doc A lightweight in-memory HyperBEAM store backed by a single ETS
%%% `ordered_set`. The store is volatile: it does not persist data to disk
%%% ever, and -- critically -- can be configured to expire each entry
%%% after a fixed TTL. This is useful for testing and as a short-term in-memory
%%% cache, not for instances where an `ok` from the `write` function should
%%% imply data persistence.
%%%
%%% Each entry is stored as `{Path, {raw, Bin} | group, Expiry}`. Group
%%% membership is discovered by a lexicographic range scan over the
%%% `Path ++ "/"` prefix, mirroring the LMDB and FS store layouts. An explicit
%%% `group` marker is inserted at every ancestor path, so that a single lookup
%%% tells whether a path has keys below it.
%%%
%%% `Expiry` is the monotonic time in milliseconds at which the entry expires:
%%% the time of its write plus the store's `max-ttl-ms` (or `max-ttl`, in
%%% seconds), or `infinity` if the store has neither. Reads and lists treat an
%%% expired entry as missing, and the owner process deletes expired entries
%%% `?SWEEPS_PER_TTL` times in each TTL. A write gives its ancestor group
%%% markers its own expiry, so a group expires with the last entry written
%%% below it.
-module(hb_store_volatile).
-export([start/3, stop/3, reset/3, scope/0, scope/1]).
-export([write/3, read/3, list/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ROOT_GROUP, <<"/">>).
%% The number of sweeps for expired entries in each TTL.
-define(SWEEPS_PER_TTL, 10).

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
%% indefinitely on stop/sweep/other messages; the table stays alive until
%% the owner exits.
owner_loop(StoreOpts) ->
    receive
        {stop, From, Ref} ->
            From ! {ok, Ref},
            exit(normal);
        sweep ->
            sweep_store(StoreOpts),
            maybe_start_ttl_timer(StoreOpts, self()),
            owner_loop(StoreOpts);
        _ ->
            owner_loop(StoreOpts)
    end.

maybe_start_ttl_timer(StoreOpts, PID) ->
    case max_ttl_ms(StoreOpts) of
        infinity -> skip;
        MaxTTLMs ->
            timer:send_after(MaxTTLMs div ?SWEEPS_PER_TTL, PID, sweep)
    end.

%% @doc The store's TTL in milliseconds, from `max-ttl-ms` or `max-ttl` (in
%% seconds), or `infinity` if it has neither.
max_ttl_ms(StoreOpts) ->
    case maps:get(<<"max-ttl-ms">>, StoreOpts, undefined) of
        undefined ->
            case maps:get(<<"max-ttl">>, StoreOpts, infinity) of
                infinity -> infinity;
                MaxTTL -> hb_util:int(MaxTTL) * 1000
            end;
        MaxTTLMs ->
            hb_util:int(MaxTTLMs)
    end.

%% @doc The expiry of an entry written now: the current monotonic time plus
%% the store's TTL, or `infinity` if it has none.
expiry(StoreOpts) ->
    case max_ttl_ms(StoreOpts) of
        infinity -> infinity;
        MaxTTLMs -> erlang:monotonic_time(millisecond) + MaxTTLMs
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

%% @doc Delete every entry whose expiry has passed.
sweep_store(Opts) ->
    Table = table(Opts),
    Now = erlang:monotonic_time(millisecond),
    Swept =
        ets:select_delete(
            Table,
            [{{'_', '_', '$1'}, [{'=<', '$1', Now}], [true]}]
        ),
    ?event(store_volatile, {swept, {table, Table}, {entries, Swept}}),
    ok.

%% @doc Write one or more entries, and a `group` marker once at each of
%% their ancestor paths. The rows of a request share one expiry. A raw entry
%% at an ancestor path is converted to a group marker (raw entries have no
%% descendants, so no subtree purge is needed). If a key written is a group,
%% its descendants are deleted first. Rows are inserted one at a time: a
%% multi-object `ets:insert/2` locks the whole of a `write_concurrency`
%% ordered_set, stalling every reader.
write(Opts, Req, _NodeOpts) when is_map(Req) ->
    Table = table(Opts),
    Expiry = expiry(Opts),
    Entries =
        maps:from_list(
            [
                {hb_path:to_binary(Path), {raw, Value}}
            ||
                {Path, Value} <- maps:to_list(Req)
            ]
        ),
    lists:foreach(
        fun(Key) ->
            maybe_delete_subtree(Table, Key),
            ?event(store_volatile, {put, {key, Key}})
        end,
        maps:keys(Entries)
    ),
    Groups = lists:foldl(fun parent_groups/2, #{}, maps:keys(Entries)),
    lists:foreach(
        fun({Key, Entry}) -> ets:insert(Table, {Key, Entry, Expiry}) end,
        maps:to_list(maps:merge(Entries, Groups))
    ).

%% @doc Read a value. Group paths return `{composite, Children}`: their
%% immediate children, each with its value if it has one.
read(Opts, #{ <<"read">> := RawKey }, _NodeOpts) ->
    Key = hb_path:to_binary(RawKey),
    case lookup_entry(Opts, Key) of
        {raw, Value} ->
            ?event(store_volatile, {hit, {key, Key}}),
            {ok, Value};
        group ->
            ?event(store_volatile, {hit, {key, Key}}),
            Children = immediate_children(Opts, Key),
            {composite, [ child(Name, Entry) || {Name, Entry} <- Children ]};
        _ ->
            ?event(store_volatile, {miss, {key, Key}}),
            {error, not_found}
    end.

%% @doc List immediate child names under a group path. An ascending list
%% walks from its `from' child and ends at its limit.
list(Opts, Req = #{ <<"list">> := RawPath }, _NodeOpts) ->
    list_path(Opts, hb_path:to_binary(RawPath), Req).

list_path(Opts, <<"">>, Req) ->
    list_path(Opts, ?ROOT_GROUP, Req);
list_path(Opts, Path, Req) ->
    case lookup_entry(Opts, Path) of
        group ->
            Children =
                case hb_store_utils:list_request_bounds(Req, Opts) of
                    #{
                        <<"from">> := From,
                        <<"limit">> := Limit,
                        <<"direction">> := asc
                    } when is_binary(From) ->
                        immediate_children(Opts, Path, From, Limit);
                    _ -> immediate_children(Opts, Path)
                end,
            Names = [ Name || {Name, _} <- Children ],
            {ok, hb_store_utils:apply_list_bounds(Names, Req, Opts)};
        _ ->
            % A path with a value has no children to list. An empty
            % store returns `not_found` for the root too, so that a chained
            % store can serve the request.
            {error, not_found}
    end.

%% @doc Return a listed child with its value if it has one, or its name if
%% it is a group.
child(Name, {raw, Value}) -> {Name, Value};
child(Name, group) -> Name.

table(Opts) ->
    #{ <<"ets-table">> := Table } = hb_store:find(Opts),
    Table.

%% @doc Return the entry at Key, or `nil` if it is missing or expired.
lookup_entry(Opts, Key) when is_map(Opts) ->
    lookup_entry(table(Opts), Key);
lookup_entry(Table, Key) ->
    Now = erlang:monotonic_time(millisecond),
    case ets:lookup(Table, Key) of
        [{_, Entry, Expiry}] when Expiry > Now -> Entry;
        _ -> nil
    end.

%% @doc Add a `group` marker for the root and for each ancestor path of Key,
%% from root to parent, to Groups. A parent that Groups has already has its
%% own ancestors there too.
parent_groups(Key, Groups) ->
    case filename:dirname(Key) of
        <<".">> ->
            Groups#{ ?ROOT_GROUP => group };
        ParentDir when is_map_key(ParentDir, Groups) ->
            Groups;
        ParentDir ->
            parent_groups(
                ?ROOT_GROUP,
                hb_path:term_to_path_parts(ParentDir),
                Groups#{ ?ROOT_GROUP => group }
            )
    end.

parent_groups(_CurrentGroup, [], Groups) ->
    Groups;
parent_groups(CurrentGroup, [Next | Rest], Groups) ->
    NextGroup = next_group_path(CurrentGroup, Next),
    parent_groups(NextGroup, Rest, Groups#{ NextGroup => group }).

next_group_path(?ROOT_GROUP, Next) ->
    hb_path:to_binary(Next);
next_group_path(CurrentGroup, Next) ->
    hb_path:to_binary([CurrentGroup, Next]).

%% @doc If `Key` currently has a `group` marker, delete all descendants
%% (entries whose path starts with `<<Key/binary, $/>>`).
maybe_delete_subtree(Table, Key) ->
    case lookup_entry(Table, Key) of
        group ->
            Prefix = <<Key/binary, $/>>,
            PLen = byte_size(Prefix),
            ets:select_delete(Table, [
                {{'$1', '_', '_'},
                 [{'=:=', {binary_part, '$1', 0, PLen}, {const, Prefix}}],
                 [true]}
            ]);
        _ ->
            ok
    end.

%% @doc Return the immediate children under GroupPath, each as
%% `{Name, Entry}`. Walks the ordered_set with `ets:next_lookup/2`, which
%% gives each key with its row, jumping past each child's own subtree after
%% it has been recorded (see `advance_past_child/4`). Cost is O(immediate
%% children), not O(descendants). For root listings the scan must start
%% from the true beginning of the table — `ets:next(Table, ?ROOT_GROUP)`
%% would skip any top-level key whose binary sort order is less than
%% `<<"/">>` (for example base64url paths starting with `-`) — so the
%% root marker row itself is skipped explicitly inside the loop instead.
%%
%% With a `From' child and a `Limit', the walk starts at the first key at
%% or after `From' and ends after `Limit' names, so a seek costs O(Limit).
%% Every child has a row of its own, before any row below it, so the names
%% are recorded in ascending order. A name before `From', whose subtree the
%% walk may start inside, is skipped like a recorded one.
%%
%% Each child is recorded with the entry of its first row in the walk (see
%% `child_entry/2'), and only live ones are, so an expired child does not
%% count toward `Limit'. The walk passes over an expired child with its
%% subtree: a group marker expires with the last entry written below it, so
%% the subtree of an expired child has expired too.
immediate_children(Opts, GroupPath) ->
    immediate_children(Opts, GroupPath, <<>>, all).
immediate_children(Opts, GroupPath, From, Limit) ->
    Table = table(Opts),
    Seen =
        case GroupPath of
            ?ROOT_GROUP ->
                collect_immediate_children(
                    Table, <<>>, ets:first_lookup(Table), #{}, {From, Limit});
            _ ->
                Prefix = <<GroupPath/binary, $/>>,
                First = lookup_from(Table, <<Prefix/binary, From/binary>>),
                collect_immediate_children(
                    Table, Prefix, First, #{}, {From, Limit})
        end,
    maps:to_list(Seen).

collect_immediate_children(_Table, _Prefix, _Next, Seen, {_From, Limit})
        when map_size(Seen) =:= Limit ->
    Seen;
collect_immediate_children(_Table, _Prefix, '$end_of_table', Seen, _Bounds) ->
    Seen;
collect_immediate_children(Table, <<>> = Prefix, {?ROOT_GROUP, _}, Seen, Bounds) ->
    collect_immediate_children(
        Table, Prefix, ets:next_lookup(Table, ?ROOT_GROUP), Seen, Bounds);
collect_immediate_children(Table, Prefix, {Key, [Row]}, Seen, Bounds = {From, _}) ->
    PLen = byte_size(Prefix),
    case Key of
        <<Prefix:PLen/binary, Rest/binary>> ->
            [Name | Below] = binary:split(Rest, <<"/">>),
            case is_map_key(Name, Seen) orelse Name < From of
                true ->
                    %% Re-entered a subtree we already recorded (possible
                    %% when a sibling like `<<Name, X>>` with X < $/ is
                    %% interleaved in ordered_set iteration), or entered
                    %% the subtree of a name before `From'. Jump past
                    %% Name's subtree in one step.
                    Next = skip_subtree(Table, Prefix, Name),
                    collect_immediate_children(
                        Table, Prefix, Next, Seen, Bounds);
                false ->
                    Next = advance_past_child(Table, Prefix, Name, Key),
                    NextSeen =
                        case child_entry(Row, Below) of
                            nil -> Seen;
                            Entry -> Seen#{Name => Entry}
                        end,
                    collect_immediate_children(
                        Table, Prefix, Next, NextSeen, Bounds)
            end;
        _ ->
            Seen
    end.

%% @doc The entry that the first row of a child in the walk gives it: the
%% row's own entry, `group` if the row is below the child, or `nil` if the
%% row has expired.
child_entry(Row, Below) ->
    child_entry(Row, Below, erlang:monotonic_time(millisecond)).
child_entry({_, _, Expiry}, _Below, Now) when Expiry =< Now -> nil;
child_entry({_, Entry, _}, [], _Now) -> Entry;
child_entry(_Row, _Below, _Now) -> group.

%% @doc Advance to the next sibling of Name under Prefix. Plain
%% `ets:next_lookup` handles the shallow case at one-op cost; if it lands
%% inside Name's own subtree, jump past it via `skip_subtree/3`.
advance_past_child(Table, Prefix, Name, Key) ->
    case ets:next_lookup(Table, Key) of
        '$end_of_table' ->
            '$end_of_table';
        Next = {NextKey, _} ->
            SubtreePrefix = <<Prefix/binary, Name/binary, $/>>,
            SPLen = byte_size(SubtreePrefix),
            case NextKey of
                <<SubtreePrefix:SPLen/binary, _/binary>> ->
                    skip_subtree(Table, Prefix, Name);
                _ ->
                    Next
            end
    end.

%% @doc Jump past every key under `<<Prefix, Name, $/>>`. The lex successor
%% of that prefix is `<<Prefix, Name, $0>>` (since $0 = $/ + 1); an explicit
%% lookup probe (see `lookup_from/2`) preserves a literal sibling named
%% `<<Name/binary, "0">>` if one exists (possible with hex-TXID-shaped keys).
skip_subtree(Table, Prefix, Name) ->
    lookup_from(Table, <<Prefix/binary, Name/binary, $0>>).

%% @doc The first key at or after Key, with its row.
lookup_from(Table, Key) ->
    case ets:lookup(Table, Key) of
        [] -> ets:next_lookup(Table, Key);
        Rows -> {Key, Rows}
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

entry_expiry_test() ->
    % Each entry expires a TTL after its own write: an entry written late in
    % the store's first TTL is still read after it, and one written at its
    % start is not.
    S = (hb_test_utils:test_store(?MODULE, <<"entry-expiry-test">>))#{
        <<"max-ttl">> => 1
    },
    hb_store:start(S),
    ok = hb_store:write(S, #{ <<"early">> => <<"1">> }, #{}),
    timer:sleep(600),
    ok = hb_store:write(S, #{ <<"late">> => <<"2">> }, #{}),
    timer:sleep(600),
    ?assertEqual({error, not_found}, hb_store:read(S, <<"early">>, #{})),
    ?assertEqual({ok, <<"2">>}, hb_store:read(S, <<"late">>, #{})),
    timer:sleep(600),
    ?assertEqual({error, not_found}, hb_store:read(S, <<"late">>, #{})),
    ok = hb_store:stop(S).

sweep_test() ->
    % The owner process deletes expired entries from the table, and keeps
    % those that have not expired.
    S = (hb_test_utils:test_store(?MODULE, <<"sweep-test">>))#{
        <<"max-ttl-ms">> => 1000
    },
    hb_store:start(S),
    Table = table(S),
    Old = maps:from_list(
        [
            {<<"old/", (integer_to_binary(I))/binary>>, <<"v">>}
        ||
            I <- lists:seq(1, 100)
        ]
    ),
    ok = hb_store:write(S, Old, #{}),
    timer:sleep(600),
    ok = hb_store:write(S, #{ <<"new">> => <<"v">> }, #{}),
    % The old entries, the `old` and root markers, and the new entry.
    ?assertEqual(103, ets:info(Table, size)),
    timer:sleep(750),
    % The root marker, given the new entry's expiry, and the new entry.
    ?assertEqual(2, ets:info(Table, size)),
    ok = hb_store:stop(S).

group_expiry_test() ->
    % A group lasts as long as the last entry written below it, and reads and
    % lists of it give only the children that have not expired.
    S = (hb_test_utils:test_store(?MODULE, <<"group-expiry-test">>))#{
        <<"max-ttl-ms">> => 1000
    },
    hb_store:start(S),
    ok = hb_store:write(S,
        #{ <<"g/a">> => <<"1">>,
           <<"g/b/leaf">> => <<"2">> }, #{}),
    timer:sleep(600),
    ok = hb_store:write(S,
        #{ <<"g/b-">> => <<"3">>,
           <<"g/c">> => <<"4">> }, #{}),
    timer:sleep(600),
    {ok, Children} = hb_store:list(S, <<"g">>, #{}),
    ?assertEqual([<<"b-">>, <<"c">>], lists:sort(Children)),
    {composite, Composite} = hb_store:read(S, <<"g">>, #{}),
    ?assertEqual(
        [{<<"b-">>, <<"3">>}, {<<"c">>, <<"4">>}],
        lists:sort(Composite)
    ),
    ?assertEqual({error, not_found}, hb_store:list(S, <<"g/b">>, #{})),
    ?assertEqual({ok, [<<"g">>]}, hb_store:list(S, <<"/">>, #{})),
    timer:sleep(600),
    ?assertEqual({error, not_found}, hb_store:read(S, <<"g">>, #{})),
    ?assertEqual({error, not_found}, hb_store:list(S, <<"/">>, #{})),
    ok = hb_store:stop(S).

list_from_limit_expiry_test() ->
    % An ascending list with `from' and `limit' passes over the children that
    % have expired, swept or not: they do not count toward the limit.
    S = (hb_test_utils:test_store(?MODULE, <<"list-from-expiry-test">>))#{
        <<"max-ttl-ms">> => 3600000
    },
    hb_store:start(S),
    ok = hb_store:write(S,
        #{ <<"p/a">> => <<"1">>,
           <<"p/b">> => <<"2">>,
           <<"p/c">> => <<"3">>,
           <<"p/d">> => <<"4">> }, #{}),
    % Expire the first two children in place, as a sweep finds them before
    % it deletes them.
    Past = erlang:monotonic_time(millisecond) - 1,
    true = ets:update_element(table(S), <<"p/a">>, {3, Past}),
    true = ets:update_element(table(S), <<"p/b">>, {3, Past}),
    List =
        fun(From, Limit) ->
            hb_store:list(
                S,
                #{
                    <<"list">> => <<"p">>,
                    <<"from">> => From,
                    <<"limit">> => Limit
                },
                #{}
            )
        end,
    ?assertEqual({ok, [<<"c">>, <<"d">>]}, List(<<"a">>, 2)),
    ?assertEqual({ok, [<<"c">>]}, List(<<"b">>, 1)),
    ?assertEqual({error, not_found}, hb_store:read(S, <<"p/a">>, #{})),
    ok = hb_store:stop(S).

no_max_ttl_test() ->
    % Without a TTL every entry, group marker included, never expires.
    S = hb_test_utils:test_store(?MODULE, <<"no-max-ttl-test">>),
    hb_store:start(S),
    ok = hb_store:write(S, #{ <<"a">> => <<"1">>, <<"g/b">> => <<"2">> }, #{}),
    timer:sleep(300),
    ?assertEqual({ok, <<"1">>}, hb_store:read(S, <<"a">>, #{})),
    ?assertEqual({ok, [<<"b">>]}, hb_store:list(S, <<"g">>, #{})),
    ?assertEqual(
        [infinity, infinity, infinity, infinity],
        [ Expiry || {_, _, Expiry} <- ets:tab2list(table(S)) ]
    ),
    ok = hb_store:stop(S).

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
    ok = hb_store:write(S, #{ <<"a/key">> => <<"value1">> }, #{}),
    ok = hb_store:write(S, #{ <<"a/key">> => <<"value2">> }, #{}),
    {ok, Children} = hb_store:list(S, <<"a">>, #{}),
    ?assertEqual([<<"key">>], Children),
    ok = hb_store:stop(S).

overwrite_group_to_raw_test() ->
    S = hb_test_utils:test_store(?MODULE, <<"overwrite-group-to-raw-test">>),
    hb_store:start(S),
    ok = hb_store:write(S, #{ <<"g/child">> => <<"v">> }, #{}),
    ok = hb_store:write(S, #{ <<"g">> => <<"raw">> }, #{}),
    ?assertEqual({ok, <<"raw">>}, hb_store:read(S, <<"g">>, #{})),
    ?assertEqual({error, not_found}, hb_store:read(S, <<"g/child">>, #{})),
    ok = hb_store:stop(S).

implicit_group_conversion_test() ->
    S = hb_test_utils:test_store(?MODULE, <<"implicit-group-conversion-test">>),
    hb_store:start(S),
    ok = hb_store:write(S, #{ <<"a">> => <<"raw_val">> }, #{}),
    ?assertEqual({ok, <<"raw_val">>}, hb_store:read(S, <<"a">>, #{})),
    ok = hb_store:write(S, #{ <<"a/b">> => <<"child_val">> }, #{}),
    ?assertEqual(
        {composite, [{<<"b">>, <<"child_val">>}]},
        hb_store:read(S, <<"a">>, #{})
    ),
    {ok, Children} = hb_store:list(S, <<"a">>, #{}),
    ?assertEqual([<<"b">>], Children),
    ok = hb_store:stop(S).

same_request_group_test() ->
    % A key written in the same request as keys below it is a group, as it
    % is when they are written one after the other.
    S = hb_test_utils:test_store(?MODULE, <<"same-request-group-test">>),
    hb_store:start(S),
    ok = hb_store:write(S,
        #{ <<"a">> => <<"raw_val">>,
           <<"a/b">> => <<"child_val">>,
           <<"a/c/d">> => <<"deep_val">> }, #{}),
    {composite, Composite} = hb_store:read(S, <<"a">>, #{}),
    ?assertEqual([{<<"b">>, <<"child_val">>}, <<"c">>], lists:sort(Composite)),
    ?assertEqual({ok, [<<"d">>]}, hb_store:list(S, <<"a/c">>, #{})),
    ?assertEqual({ok, [<<"a">>]}, hb_store:list(S, <<"/">>, #{})),
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

list_from_limit_test() ->
    %% An ascending list with `from' and `limit' gives the first children at
    %% or after `from', including when the walk starts inside the subtree of
    %% a child before it (`a.' sorts between `a-' and `a/leaf').
    S = hb_test_utils:test_store(?MODULE, <<"list-from-limit-test">>),
    hb_store:start(S),
    ok = hb_store:write(S,
        #{ <<"p/a/leaf">> => <<"1">>,
           <<"p/a-">> => <<"2">>,
           <<"p/b">> => <<"3">>,
           <<"p/c/leaf">> => <<"4">> }, #{}),
    List =
        fun(From, Limit) ->
            hb_store:list(
                S,
                #{
                    <<"list">> => <<"p">>,
                    <<"from">> => From,
                    <<"limit">> => Limit
                },
                #{}
            )
        end,
    ?assertEqual({ok, [<<"a">>, <<"a-">>]}, List(<<"a">>, 2)),
    ?assertEqual({ok, [<<"b">>]}, List(<<"a.">>, 1)),
    ?assertEqual({ok, [<<"b">>, <<"c">>]}, List(<<"b">>, 5)),
    ?assertEqual({ok, []}, List(<<"d">>, 1)),
    ok = hb_store:stop(S).
