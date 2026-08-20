%%% @doc Vectors for the persistent linked list the carried histories are held
%%% in: what a read costs, what a branch shares, and where a history ends.
%%%
%%% The values the two committed hashes are reproduced from are mainnet's own
%%% and live with the checks that read them, in
%%% `dev_arweave_block_test_vectors'.
-module(dev_arweave_history_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc A read of the newest few entries follows one link per entry it returns.
%%
%% An eight-entry history is built across two stores: its newest three entries
%% in one and its oldest five in the other. Read back from the first alone, a
%% walk of three answers and a walk of four raises on the entry it needs, so
%% what this vector measures is how far the walk went rather than what it
%% returned: a read of the whole chain would raise for three as well.
take_follows_one_link_per_entry_test() ->
    Deep = test_opts(),
    Newest = test_opts(),
    Both = #{ <<"store">> => stores(Newest) ++ stores(Deep) },
    Tail = block_time_history(lists:nthtail(3, intervals(8)), Deep),
    Head =
        lists:foldl(
            fun(Value, Onto) -> append_block_time(Value, Onto, Both) end,
            Tail,
            lists:reverse(lists:sublist(intervals(8), 3))
        ),
    ?assertEqual(
        lists:sublist(intervals(8), 3),
        lib_arweave_history:values(Head, 3, Newest)
    ),
    ?assertThrow(
        {necessary_message_not_found, _, _},
        lib_arweave_history:values(Head, 4, Newest)
    ).

%% @doc Two branches sharing a prefix share the entries below the fork, and
%% building the second rewrites none of them.
%%
%% Sharing is identity: the entries the two branches walk below the fork are the
%% same messages under the same identifiers, which is what makes switching
%% between the branches free.
%%
%% Identity alone would not catch a rebuild, because a rebuilt entry is
%% content-addressed to the same identifier as the one it replaced. What catches
%% it is where the writes land: the prefix is built in a store of its own and
%% the branches in a second one in front of it, so what the second store holds
%% is what building the branches wrote. A branch read back from it alone reaches
%% its own entry and nothing below the fork, which a rebuilt tail would not.
structural_sharing_survives_a_reorg_test() ->
    Prefix = test_opts(),
    Branches = test_opts(),
    Both = #{ <<"store">> => stores(Branches) ++ stores(Prefix) },
    Fork = block_time_history(intervals(4), Prefix),
    Branch1 = append_block_time({91, 91, 1}, Fork, Both),
    Branch2 = append_block_time({92, 92, 1}, Fork, Both),
    Shared = ids(lib_arweave_history:entries(Fork, 4, Prefix), Prefix),
    ?assertEqual(
        Shared,
        tl(ids(lib_arweave_history:entries(Branch1, 5, Both), Both))
    ),
    ?assertEqual(
        Shared,
        tl(ids(lib_arweave_history:entries(Branch2, 5, Both), Both))
    ),
    ?assertEqual([{91, 91, 1} | intervals(4)],
        lib_arweave_history:values(Branch1, Both)),
    ?assertEqual([{92, 92, 1} | intervals(4)],
        lib_arweave_history:values(Branch2, Both)),
    ?assertEqual([{91, 91, 1}],
        lib_arweave_history:values(Branch1, 1, Branches)),
    ?assertThrow(
        {necessary_message_not_found, _, _},
        lib_arweave_history:values(Branch1, 2, Branches)
    ).

%% @doc A history is trimmed to the length the consensus rules read back, and
%% the values below it are never written.
%%
%% The wire form carries more entries than the cap, as the one a peer serves
%% does. What the list holds is the cap exactly, in order, and a walk asking for
%% more than the cap ends at it rather than reaching an entry beyond it.
trims_at_the_cap_test() ->
    Opts = test_opts(),
    Cap = lib_arweave_history:cap(<<"block-time-history">>, 1275480),
    Values = intervals(Cap + 5),
    {ok, Head} =
        lib_arweave_history:from_binary(
            <<"block-time-history">>,
            ar_serialize:block_time_history_to_binary(Values),
            1275480,
            Opts
        ),
    ?assertEqual(Cap, lib_arweave_history:length(Head, Opts)),
    ?assertEqual(lists:sublist(Values, Cap),
        lib_arweave_history:values(Head, Opts)),
    ?assertEqual(Cap, length(lib_arweave_history:entries(Head, Cap + 5, Opts))),
    Appended = append_block_time({7, 7, 1}, Head, Opts),
    ?assertEqual(Cap, lib_arweave_history:length(Appended, Opts)),
    ?assertEqual(
        [{7, 7, 1} | lists:sublist(Values, Cap - 1)],
        lib_arweave_history:values(Appended, Opts)
    ).

%% @doc A history nothing carries is empty rather than an error, and a history
%% of one entry ends its walk at that entry.
empty_and_short_histories_test() ->
    Opts = test_opts(),
    ?assertEqual(0, lib_arweave_history:length([], Opts)),
    ?assertEqual([], lib_arweave_history:entries([], 5, Opts)),
    ?assertEqual([], lib_arweave_history:values([], Opts)),
    Head = block_time_history(intervals(1), Opts),
    ?assertEqual(1, lib_arweave_history:length(Head, Opts)),
    ?assertEqual(intervals(1), lib_arweave_history:values(Head, 9, Opts)),
    ?assertEqual(1, length(lib_arweave_history:entries(Head, 9, Opts))).

%% @doc The memoised window answers a head that has been through the cache.
%%
%% The head is written and read back, so what is asked for is the message the
%% cache produced rather than the one the window was recorded against. The read
%% then runs against a store holding that one entry alone: a walk would fail on
%% the second, so an answer at all is the window being used.
window_survives_a_cache_round_trip_test() ->
    Opts = test_opts(),
    Head = block_time_history(intervals(5), Opts),
    {ok, ID} = hb_cache:write(Head, Opts),
    {ok, Reread} = hb_cache:read(ID, Opts),
    Partial = test_opts(),
    {ok, _} = hb_cache:write(Head, Partial),
    ?assertThrow(
        {necessary_message_not_found, _, _},
        lib_arweave_history:values(Reread, 5, Partial)
    ),
    ?assertEqual(intervals(5), lib_arweave_history:values(Reread, Partial)).

%% @doc The device reads a history back over AO-Core, and reports how long it is
%% without being asked to materialise it.
take_key_returns_the_newest_entries_test() ->
    Opts = test_opts(),
    Head = block_time_history(intervals(6), Opts),
    {ok, Taken} =
        hb_ao:resolve(
            Head,
            #{ <<"path">> => <<"take">>, <<"count">> => 2 },
            Opts
        ),
    ?assertEqual(6, hb_util:int(hb_maps:get(<<"length">>, Taken, 0, Opts))),
    Entries = hb_maps:get(<<"entries">>, Taken, [], Opts),
    ?assertEqual(2, length(Entries)),
    ?assertEqual(
        [1, 2],
        [
            hb_util:int(hb_maps:get(<<"block-interval">>, Entry, 0, Opts))
        ||
            Entry <- Entries
        ]
    ).

%% @doc The device builds and extends a history over AO-Core alone, and renders
%% one back into the exact bytes a peer serves.
%%
%% The round trip is what makes the representation checkable without a block:
%% bytes in through `from-binary', one element on through `push', the whole
%% history back out through `to-binary'. What comes back is the wire form the
%% vendored decoders read, so the persistent list and the format Arweave
%% publishes are the same history rather than two descriptions of one.
push_and_to_binary_round_trip_test() ->
    Opts = test_opts(),
    Wire = ar_serialize:block_time_history_to_binary(intervals(4)),
    Seed = #{ <<"device">> => <<"arweave-history@2.9">>,
        <<"kind">> => <<"block-time-history">>, <<"height">> => 1275480 },
    {ok, Head} =
        hb_ao:resolve(
            Seed,
            #{ <<"path">> => <<"from-binary">>, <<"body">> => Wire },
            Opts
        ),
    ?assertEqual(4, hb_util:int(hb_maps:get(<<"length">>, Head, 0, Opts))),
    ?assertEqual(Wire, to_binary(Head, Opts)),
    % One element on, and the wire form grows by exactly that element.
    {ok, Pushed} =
        hb_ao:resolve(
            Head,
            #{
                <<"path">> => <<"push">>,
                <<"height">> => 1275481,
                <<"block-interval">> => 99,
                <<"vdf-interval">> => 98,
                <<"chunk-count">> => 2
            },
            Opts
        ),
    ?assertEqual(5, hb_util:int(hb_maps:get(<<"length">>, Pushed, 0, Opts))),
    ?assertEqual(
        [{99, 98, 2} | intervals(4)],
        lib_arweave_history:values(Pushed, Opts)
    ),
    ?assertEqual(
        ar_serialize:block_time_history_to_binary([{99, 98, 2} | intervals(4)]),
        to_binary(Pushed, Opts)
    ),
    % A history that does not exist yet is pushed onto directly.
    {ok, First} =
        hb_ao:resolve(
            Seed,
            #{
                <<"path">> => <<"push">>,
                <<"block-interval">> => 7,
                <<"vdf-interval">> => 7,
                <<"chunk-count">> => 1
            },
            Opts
        ),
    ?assertEqual(1, hb_util:int(hb_maps:get(<<"length">>, First, 0, Opts))),
    % A kind this device does not hold is refused rather than guessed at.
    ?assertMatch(
        {error, #{ <<"message">> := <<"unknown-history-kind">> }},
        hb_ao:resolve(
            Seed#{ <<"kind">> => <<"weather-history">> },
            #{ <<"path">> => <<"push">>, <<"block-interval">> => 1 },
            Opts
        )
    ).

%% @doc The same four keys hold for the reward history, whose entries carry
%% four fields of their own rather than three.
%%
%% The two kinds share every line of the representation and differ only in what
%% an element is, so what this vector establishes is that the difference really
%% is confined to `fields/2' and `value/3': the wire form a peer serves round
%% trips, and a pushed element reads back as the tuple `ar_rewards' works on.
reward_history_push_round_trip_test() ->
    Opts = test_opts(),
    Address = crypto:strong_rand_bytes(32),
    Values = [ {Address, N * 7, N * 1000, 1} || N <- lists:seq(1, 3) ],
    Wire = ar_serialize:reward_history_to_binary(Values),
    Seed =
        #{
            <<"device">> => <<"arweave-history@2.9">>,
            <<"kind">> => <<"reward-history">>,
            <<"height">> => 1500000
        },
    {ok, Head} =
        hb_ao:resolve(
            Seed,
            #{ <<"path">> => <<"from-binary">>, <<"body">> => Wire },
            Opts
        ),
    ?assertEqual(Values, lib_arweave_history:values(Head, Opts)),
    ?assertEqual(Wire, to_binary(Head, Opts)),
    Pushed = crypto:strong_rand_bytes(32),
    {ok, Extended} =
        hb_ao:resolve(
            Head,
            #{
                <<"path">> => <<"push">>,
                <<"height">> => 1500001,
                <<"address">> => hb_util:encode(Pushed),
                <<"hash-rate">> => 99,
                <<"reward">> => 12345,
                <<"denomination">> => 1
            },
            Opts
        ),
    ?assertEqual(
        [{Pushed, 99, 12345, 1} | Values],
        lib_arweave_history:values(Extended, Opts)
    ),
    ?assertEqual(
        ar_serialize:reward_history_to_binary(
            [{Pushed, 99, 12345, 1} | Values]),
        to_binary(Extended, Opts)
    ).

%% @doc The device refuses a body that is not a whole number of entries.
rejects_corrupt_history_binary_test() ->
    Opts = test_opts(),
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-block-time-history">> }},
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-history@2.9">>,
                <<"kind">> => <<"block-time-history">>,
                <<"height">> => 1275480
            },
            #{ <<"path">> => <<"from-binary">>, <<"body">> => <<1:8, 5:8>> },
            Opts
        )
    ).

%%% Test helpers.

%% @doc A store of this vector's own, so that what one vector writes cannot be
%% read by another.
test_opts() ->
    #{ <<"store">> => [hb_test_utils:test_store()] }.

%% @doc The stores a set of options names, so that two of them can be put in
%% front of each other: writes land in the first, reads fall through to both.
stores(Opts) ->
    hb_opts:get(store, [], Opts).

%% @doc The identifiers a run of entries is known by. Two branches share a
%% prefix exactly when these are equal over it.
ids(Entries, Opts) ->
    [ hb_message:id(Entry, all, Opts) || Entry <- Entries ].

%% @doc Distinguishable block-time values, newest first, so that the order a
%% history is read back in is checkable rather than merely plausible.
intervals(Count) ->
    [ {Interval, Interval, 1} || Interval <- lists:seq(1, Count) ].

%% @doc Build a block-time history from a newest-first list of values.
block_time_history(Values, Opts) ->
    lists:foldl(
        fun(Value, Head) -> append_block_time(Value, Head, Opts) end,
        [],
        lists:reverse(Values)
    ).

%% @doc Render a history through the device's own codec key.
to_binary(Head, Opts) ->
    {ok, Result} = hb_ao:resolve(Head, <<"to-binary">>, Opts),
    hb_maps:get(<<"body">>, Result, not_found, Opts).

%% @doc Add one block-time value to a history, at a height above the 2.7 fork.
append_block_time(Value, Head, Opts) ->
    lib_arweave_history:append(
        <<"block-time-history">>, Value, 1275480, Head, Opts).
