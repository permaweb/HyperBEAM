%%% @doc Vectors for the storage-module sync record.
%%%
%%% Every test that touches a store runs against a real LMDB database in a
%%% directory of its own under the system temporary directory, reached the way
%%% a bounded pass reaches it: by naming a storage module and letting
%%% `lib_arweave_storage:store/2' place its index beside the chunk files.
%%%
%%% The query vectors are checked against one hand-written table of intervals,
%%% `query_records/0'. Their expected answers are computed by hand from the
%%% half-open convention -- left bound excluded, right bound included -- and
%%% match upstream `ar_sync_record:get_next_synced_interval/5',
%%% `get_next_unsynced_interval/5', `get_interval/3' and `is_recorded/4', whose
%%% behaviour is in `ar_ets_intervals'.
-module(lib_arweave_sync_record_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc A record set saved to a module's store loads back holding exactly what
%% it held, across plain ids, packing-tagged ids and a record emptied by a
%% delete.
save_load_round_trip_test() ->
    Opts = opts(<<"round-trip">>),
    Module = module(),
    Records =
        lib_arweave_sync_record:delete(
            lib_arweave_sync_record:add(
                lib_arweave_sync_record:add(
                    lib_arweave_sync_record:add(
                        lib_arweave_sync_record:add(
                            #{},
                            ar_data_sync,
                            {replica_2_9, address()},
                            262144,
                            0
                        ),
                        ar_data_sync,
                        {spora_2_6, address()},
                        786432,
                        524288
                    ),
                    ar_chunk_storage,
                    786432,
                    0
                ),
                ar_data_sync_footprints,
                {composite, address(), 2},
                100,
                50
            ),
            invalid_chunks,
            10,
            5
        ),
    ok = lib_arweave_sync_record:save(Module, Records, Opts),
    {ok, Loaded} = lib_arweave_sync_record:load(Module, Opts),
    assert_records(Records, Loaded),
    ?assertEqual(
        [
            ar_chunk_storage,
            ar_data_sync,
            ar_data_sync_footprints,
            invalid_chunks,
            {ar_data_sync, {replica_2_9, address()}},
            {ar_data_sync, {spora_2_6, address()}},
            {ar_data_sync_footprints, {composite, address(), 2}}
        ],
        lib_arweave_sync_record:ids(Loaded)
    ).

%% @doc `add/5' records the range against the plain id as well as the
%% packing-tagged one, so a reader asking what is synced and a reader asking
%% what is synced in a packing it can mine from both find it.
add_packing_writes_both_records_test() ->
    Opts = opts(<<"add-packing">>),
    Module = module(),
    Packing = {replica_2_9, address()},
    Records =
        lib_arweave_sync_record:add(#{}, ar_data_sync, Packing, 262144, 0),
    ?assertEqual(
        [ar_data_sync, {ar_data_sync, Packing}],
        lib_arweave_sync_record:ids(Records)
    ),
    ?assert(lib_arweave_sync_record:is_recorded(Records, ar_data_sync, 1)),
    ?assert(
        lib_arweave_sync_record:is_recorded(Records, ar_data_sync, Packing, 1)
    ),
    ?assertNot(
        lib_arweave_sync_record:is_recorded(
            Records,
            ar_data_sync,
            unpacked,
            1
        )
    ),
    ok = lib_arweave_sync_record:save(Module, Records, Opts),
    {ok, Loaded} = lib_arweave_sync_record:load(Module, Opts),
    assert_records(Records, Loaded),
    ?assert(lib_arweave_sync_record:is_recorded(Loaded, ar_data_sync, 262144)),
    ?assert(
        lib_arweave_sync_record:is_recorded(
            Loaded,
            ar_data_sync,
            Packing,
            262144
        )
    ).

%% @doc `delete/4' takes the range out of every packing of the id, because the
%% bytes are gone whatever they were packed as.
delete_clears_packing_records_test() ->
    Opts = opts(<<"delete">>),
    Module = module(),
    Records =
        lib_arweave_sync_record:delete(
            lib_arweave_sync_record:add(
                lib_arweave_sync_record:add(
                    lib_arweave_sync_record:add(
                        #{},
                        ar_data_sync,
                        {replica_2_9, address()},
                        30,
                        0
                    ),
                    ar_data_sync,
                    unpacked,
                    30,
                    0
                ),
                ar_chunk_storage,
                30,
                0
            ),
            ar_data_sync,
            20,
            10
        ),
    ok = lib_arweave_sync_record:save(Module, Records, Opts),
    {ok, Loaded} = lib_arweave_sync_record:load(Module, Opts),
    assert_records(Records, Loaded),
    % The plain record and both packings lost the middle ten bytes.
    lists:foreach(
        fun(Id) ->
            ?assertEqual([{10, 0}, {30, 20}], listed(Loaded, Id))
        end,
        [
            ar_data_sync,
            {ar_data_sync, {replica_2_9, address()}},
            {ar_data_sync, unpacked}
        ]
    ),
    % A different id keeps them: a delete names one record family.
    ?assertEqual([{30, 0}], listed(Loaded, ar_chunk_storage)),
    ?assertEqual(20, lib_arweave_sync_record:size(Loaded, ar_data_sync)),
    ?assertNot(lib_arweave_sync_record:is_recorded(Loaded, ar_data_sync, 15)),
    ?assert(lib_arweave_sync_record:is_recorded(Loaded, ar_chunk_storage, 15)).

%% @doc `cut/3' drops everything above the offset from every packing of the id,
%% keeping the part of a straddling interval that is below it.
cut_truncates_every_packing_test() ->
    Opts = opts(<<"cut">>),
    Module = module(),
    Records =
        lib_arweave_sync_record:cut(
            lib_arweave_sync_record:add(
                lib_arweave_sync_record:add(
                    lib_arweave_sync_record:add(
                        lib_arweave_sync_record:add(
                            #{},
                            ar_data_sync,
                            {replica_2_9, address()},
                            20,
                            10
                        ),
                        ar_data_sync,
                        {replica_2_9, address()},
                        40,
                        30
                    ),
                    ar_data_sync,
                    unpacked,
                    40,
                    30
                ),
                ar_chunk_storage,
                40,
                30
            ),
            ar_data_sync,
            15
        ),
    ok = lib_arweave_sync_record:save(Module, Records, Opts),
    {ok, Loaded} = lib_arweave_sync_record:load(Module, Opts),
    assert_records(Records, Loaded),
    ?assertEqual([{15, 10}], listed(Loaded, ar_data_sync)),
    ?assertEqual(
        [{15, 10}],
        listed(Loaded, {ar_data_sync, {replica_2_9, address()}})
    ),
    ?assertEqual([], listed(Loaded, {ar_data_sync, unpacked})),
    % Another id is untouched, and a cut above everything recorded is a no-op.
    ?assertEqual([{40, 30}], listed(Loaded, ar_chunk_storage)),
    ?assertEqual(
        [{40, 30}],
        listed(
            lib_arweave_sync_record:cut(Loaded, ar_chunk_storage, 100),
            ar_chunk_storage
        )
    ).

%% @doc The query surface against a hand-computed table, at both ends of every
%% interval and with an unbounded right bound.
query_boundaries_test() ->
    Records = query_records(),
    Recorded = recorded_vectors(),
    ?assertEqual(
        Recorded,
        [
            {
                Offset,
                lib_arweave_sync_record:is_recorded(
                    Records,
                    ar_data_sync,
                    Offset
                )
            }
        ||
            {Offset, _Expected} <- Recorded
        ]
    ),
    Intervals = interval_vectors(),
    ?assertEqual(
        Intervals,
        [
            {
                Offset,
                lib_arweave_sync_record:interval(Records, ar_data_sync, Offset)
            }
        ||
            {Offset, _Expected} <- Intervals
        ]
    ),
    Synced = next_synced_vectors(),
    ?assertEqual(
        Synced,
        [
            {
                {Offset, Bound},
                lib_arweave_sync_record:next_synced(
                    Records,
                    ar_data_sync,
                    Offset,
                    Bound
                )
            }
        ||
            {{Offset, Bound}, _Expected} <- Synced
        ]
    ),
    Unsynced = next_unsynced_vectors(),
    ?assertEqual(
        Unsynced,
        [
            {
                {Offset, Bound},
                lib_arweave_sync_record:next_unsynced(
                    Records,
                    ar_data_sync,
                    Offset,
                    Bound
                )
            }
        ||
            {{Offset, Bound}, _Expected} <- Unsynced
        ]
    ).

%% @doc An id with nothing recorded answers every query as an empty record
%% rather than as a failure: a gap covering the whole span asked about, no
%% synced interval, and no bytes.
unrecorded_id_answers_empty_test() ->
    Records = query_records(),
    ?assertEqual(
        not_found,
        lib_arweave_sync_record:next_synced(
            Records,
            invalid_chunks,
            0,
            infinity
        )
    ),
    ?assertEqual(
        {100, 5},
        lib_arweave_sync_record:next_unsynced(Records, invalid_chunks, 5, 100)
    ),
    ?assertEqual(
        not_found,
        lib_arweave_sync_record:interval(Records, invalid_chunks, 5)
    ),
    ?assertNot(
        lib_arweave_sync_record:is_recorded(Records, invalid_chunks, 5)
    ),
    ?assertNot(
        lib_arweave_sync_record:is_recorded(
            Records,
            ar_data_sync,
            {replica_2_9, address()},
            5
        )
    ),
    ?assertEqual(0, lib_arweave_sync_record:count(Records, invalid_chunks)),
    ?assertEqual(0, lib_arweave_sync_record:size(Records, invalid_chunks)),
    ?assertEqual(
        ar_intervals:new(),
        lib_arweave_sync_record:intervals(Records, invalid_chunks)
    ).

%% @doc Adjacent and overlapping ranges become one interval, and stay one
%% through a save and a load.
interval_compaction_survives_round_trip_test() ->
    Opts = opts(<<"compaction">>),
    Module = module(),
    % (2, 0) meets (5, 2) at 2, and (12, 9) runs into (10, 7).
    Records =
        lists:foldl(
            fun({End, Start}, Acc) ->
                lib_arweave_sync_record:add(Acc, ar_chunk_storage, End, Start)
            end,
            #{},
            [{5, 2}, {2, 0}, {10, 7}, {12, 9}]
        ),
    ?assertEqual([{5, 0}, {12, 7}], listed(Records, ar_chunk_storage)),
    ?assertEqual(2, lib_arweave_sync_record:count(Records, ar_chunk_storage)),
    ok = lib_arweave_sync_record:save(Module, Records, Opts),
    {ok, Loaded} = lib_arweave_sync_record:load(Module, Opts),
    ?assertEqual([{5, 0}, {12, 7}], listed(Loaded, ar_chunk_storage)),
    ?assertEqual(2, lib_arweave_sync_record:count(Loaded, ar_chunk_storage)),
    ?assertEqual(10, lib_arweave_sync_record:size(Loaded, ar_chunk_storage)),
    % A range already held changes nothing, before or after the round trip.
    ?assertEqual(
        [{5, 0}, {12, 7}],
        listed(
            lib_arweave_sync_record:add(Loaded, ar_chunk_storage, 4, 1),
            ar_chunk_storage
        )
    ).

%% @doc A record of ten thousand intervals round-trips, and its stored value is
%% exactly sixteen bytes an interval.
large_record_round_trip_test() ->
    Opts = opts(<<"large">>),
    Module = module(),
    % Two bytes recorded then two skipped, so nothing merges.
    Intervals = [{(4 * N) + 3, (4 * N) + 1} || N <- lists:seq(0, 9999)],
    Records =
        lists:foldl(
            fun({End, Start}, Acc) ->
                lib_arweave_sync_record:add(Acc, invalid_chunks, End, Start)
            end,
            #{},
            Intervals
        ),
    ?assertEqual(10000, lib_arweave_sync_record:count(Records, invalid_chunks)),
    ok = lib_arweave_sync_record:save(Module, Records, Opts),
    ?assertEqual(160000, byte_size(stored(Module, invalid_chunks, Opts))),
    {ok, Loaded} = lib_arweave_sync_record:load(Module, Opts),
    ?assertEqual(Intervals, listed(Loaded, invalid_chunks)),
    ?assertEqual(20000, lib_arweave_sync_record:size(Loaded, invalid_chunks)),
    ?assertEqual(
        {39999, 39997},
        lib_arweave_sync_record:interval(Loaded, invalid_chunks, 39998)
    ).

%% @doc A module whose store has never been written to holds no records, which
%% is `{ok, #{}}' rather than an error.
unwritten_module_loads_empty_test() ->
    Opts = opts(<<"unwritten">>),
    Module = module(),
    ?assertEqual({ok, #{}}, lib_arweave_sync_record:load(Module, Opts)),
    ?assertEqual([], lib_arweave_sync_record:ids(#{})),
    % Saving nothing to it leaves it empty and still loadable.
    ok = lib_arweave_sync_record:save(Module, #{}, Opts),
    ?assertEqual({ok, #{}}, lib_arweave_sync_record:load(Module, Opts)).

%% @doc Every id shape round-trips through its label, and the labels are the
%% spelling upstream `ar_serialize:encode_packing(Packing, true)' gives -- the
%% literals below are read from that function.
label_round_trip_test() ->
    Encoded = hb_util:encode(address()),
    Ids =
        [
            ar_data_sync,
            ar_chunk_storage,
            ar_chunk_storage_replica_2_9_1_unpacked,
            ar_chunk_storage_replica_2_9_5_entropy,
            ar_chunk_storage_replica_2_9_1_entropy,
            ar_data_sync_footprints,
            invalid_chunks,
            {ar_data_sync, unpacked},
            {ar_chunk_storage, unpacked_padded},
            {ar_data_sync, {replica_2_9, address()}},
            {ar_data_sync, {spora_2_6, address()}},
            {ar_data_sync, {composite, address(), 1}},
            {ar_data_sync_footprints, {composite, address(), 32}}
        ],
    ?assertEqual(
        Ids,
        [
            lib_arweave_sync_record:parse_label(
                lib_arweave_sync_record:label(Id)
            )
        ||
            Id <- Ids
        ]
    ),
    ?assertEqual(
        [
            <<"ar_data_sync">>,
            <<"ar_chunk_storage">>,
            <<"ar_chunk_storage_replica_2_9_1_unpacked">>,
            <<"ar_chunk_storage_replica_2_9_5_entropy">>,
            <<"ar_chunk_storage_replica_2_9_1_entropy">>,
            <<"ar_data_sync_footprints">>,
            <<"invalid_chunks">>,
            <<"ar_data_sync.unpacked">>,
            <<"ar_chunk_storage.unpacked_padded">>,
            << "ar_data_sync.replica_2_9_", Encoded/binary >>,
            << "ar_data_sync.spora_2_6_", Encoded/binary >>,
            << "ar_data_sync.composite_", Encoded/binary, ".1" >>,
            << "ar_data_sync_footprints.composite_", Encoded/binary, ".32" >>
        ],
        [lib_arweave_sync_record:label(Id) || Id <- Ids]
    ),
    % A label naming a packing this node cannot read is refused rather than
    % guessed at.
    ?assertEqual(
        not_found,
        lib_arweave_sync_record:parse_label(<<"ar_data_sync.spora_2_5">>)
    ),
    ?assertEqual(
        not_found,
        lib_arweave_sync_record:parse_label(<<"ar_data_sync.replica_2_9_xyz">>)
    ).

%% @doc A stored value that is not a whole number of intervals is refused,
%% rather than read as a range this node would then claim to hold.
malformed_record_refused_test() ->
    Opts = opts(<<"malformed">>),
    Module = module(),
    ok = written(Module, <<"ar_data_sync">>, << 0:120 >>, Opts),
    ?assertMatch(
        {error, #{ <<"message">> := <<"sync-record-malformed">> }},
        lib_arweave_sync_record:load(Module, Opts)
    ),
    % An interval with no bytes in it is malformed too.
    ok = written(Module, <<"ar_data_sync">>, << 5:64, 5:64 >>, Opts),
    ?assertMatch(
        {error, #{ <<"message">> := <<"sync-record-malformed">> }},
        lib_arweave_sync_record:load(Module, Opts)
    ).

%% @doc A key that holds children rather than bytes is refused. The store
%% answers such a key with its own shape rather than a value, which the detail
%% names by label alone while the raw answer goes to the event stream.
non_binary_record_refused_test() ->
    Opts = opts(<<"non-binary">>),
    Module = module(),
    Store = lib_arweave_storage:store(Opts),
    ok = hb_store:group(Store, group(Module), Opts),
    ok =
        hb_store:write(
            Store,
            #{ << (key(Module, <<"ar_data_sync">>))/binary, "/x" >> => <<"1">> },
            Opts
        ),
    ?assertMatch(
        {error,
            #{
                <<"message">> := <<"sync-record-unreadable">>,
                <<"detail">> := <<"ar_data_sync">>
            }
        },
        lib_arweave_sync_record:load(Module, Opts)
    ).

%% @doc A record whose label names an id this node has no name for is refused:
%% it covers bytes this node cannot answer for.
unknown_id_refused_test() ->
    Opts = opts(<<"unknown-id">>),
    Module = module(),
    Random = hb_util:encode(crypto:strong_rand_bytes(12)),
    Label = << "unnamed-", Random/binary >>,
    ok = written(Module, Label, << 10:64, 0:64 >>, Opts),
    ?assertMatch(
        {error, #{ <<"message">> := <<"sync-record-unknown-id">> }},
        lib_arweave_sync_record:load(Module, Opts)
    ).

%%% Test helpers.

%% @doc The record every query vector is checked against: bytes 3 to 5, 8 to 10
%% and 16 to 20 of the weave.
query_records() ->
    lists:foldl(
        fun({End, Start}, Records) ->
            lib_arweave_sync_record:add(Records, ar_data_sync, End, Start)
        end,
        #{},
        [{5, 2}, {10, 7}, {20, 15}]
    ).

%% @doc `is_recorded/3' at every boundary of the table: the left bound of an
%% interval is excluded and the right bound included.
recorded_vectors() ->
    [
        {0, false}, {1, false}, {2, false}, {3, true}, {4, true}, {5, true},
        {6, false}, {7, false}, {8, true}, {9, true}, {10, true},
        {11, false}, {15, false}, {16, true}, {19, true}, {20, true},
        {21, false}
    ].

%% @doc `interval/3' at every boundary of the table.
interval_vectors() ->
    [
        {0, not_found}, {2, not_found}, {3, {5, 2}}, {5, {5, 2}},
        {6, not_found}, {7, not_found}, {8, {10, 7}}, {10, {10, 7}},
        {11, not_found}, {15, not_found}, {16, {20, 15}}, {20, {20, 15}},
        {21, not_found}
    ].

%% @doc `next_synced/4': the lowest interval whose end offset is strictly above
%% the offset, clipped to the right bound, or `not_found' when its start is at
%% or above that bound.
next_synced_vectors() ->
    [
        {{0, infinity}, {5, 2}},
        {{4, infinity}, {5, 2}},
        {{5, infinity}, {10, 7}},
        {{10, infinity}, {20, 15}},
        {{20, infinity}, not_found},
        {{25, infinity}, not_found},
        {{0, 4}, {4, 2}},
        {{0, 3}, {3, 2}},
        {{0, 2}, not_found},
        {{5, 8}, {8, 7}},
        {{5, 7}, not_found},
        {{7, 7}, not_found},
        {{12, 30}, {20, 15}}
    ].

%% @doc `next_unsynced/4': the lowest gap at or above the offset, clipped to
%% the right bound. An offset inside a recorded interval moves to that
%% interval's end, where the bound is checked again.
next_unsynced_vectors() ->
    [
        {{0, infinity}, {2, 0}},
        {{2, infinity}, {7, 5}},
        {{3, infinity}, {7, 5}},
        {{5, infinity}, {7, 5}},
        {{10, infinity}, {15, 10}},
        {{20, infinity}, {infinity, 20}},
        {{25, infinity}, {infinity, 25}},
        {{0, 1}, {1, 0}},
        {{2, 6}, {6, 5}},
        {{2, 5}, not_found},
        {{7, 7}, not_found},
        {{16, 20}, not_found}
    ].

%% @doc Compare two record sets by what they hold rather than by the shape of
%% the trees holding it: a set built by adding intervals in one order is the
%% same record as one built by adding them in another.
assert_records(Expected, Actual) ->
    ?assertEqual(contents(Expected), contents(Actual)).

contents(Records) ->
    [
        {Id, listed(Records, Id)}
    ||
        Id <- lib_arweave_sync_record:ids(Records)
    ].

listed(Records, Id) ->
    ar_intervals:to_list(lib_arweave_sync_record:intervals(Records, Id)).

%% @doc Read one record's stored bytes at the documented key, so that the
%% on-disk format is asserted rather than assumed.
stored(Module, Id, Opts) ->
    {ok, Value} =
        hb_store:read(
            lib_arweave_storage:store(Opts),
            key(Module, lib_arweave_sync_record:label(Id)),
            Opts
        ),
    Value.

%% @doc Write one record's bytes under a label of the caller's choosing, for
%% the vectors that check what a reader does with a value it did not write.
written(Module, Label, Value, Opts) ->
    Store = lib_arweave_storage:store(Opts),
    ok = hb_store:group(Store, group(Module), Opts),
    hb_store:write(Store, #{ key(Module, Label) => Value }, Opts).

%% @doc The store key one record is written at, which carries the module the
%% record belongs to.
key(Module, Label) ->
    hb_path:to_binary([group(Module), Label]).

%% @doc The group one module's records are filed under.
group(Module) ->
    hb_path:to_binary(
        [
            <<"~arweave@2.9/storage">>,
            hb_util:bin(lib_arweave_storage:id(Module)),
            <<"sync-record">>
        ]
    ).

%% @doc The storage module every test names. One bucket of the weave, packed
%% for this node's address.
module() ->
    {ar_block:partition_size(), 3, {replica_2_9, address()}}.

address() ->
    crypto:hash(sha256, <<"sync-record-address">>).

%% @doc Point a test at a data directory of its own under the system temporary
%% directory, so that each one gets a fresh store with a name no other test
%% shares.
opts(Tag) ->
    #{
        <<"arweave-data-dir">> =>
            hb_util:bin(
                filename:join(
                    os:getenv("TMPDIR", "/tmp"),
                    hb_util:list(
                        <<
                            "hb-sync-record-",
                            Tag/binary,
                            "-",
                            (hb_util:bin(
                                erlang:system_time(microsecond)))/binary,
                            "-",
                            (hb_util:encode(crypto:strong_rand_bytes(6)))/binary
                        >>
                    )
                )
            )
    }.
