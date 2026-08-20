%%% @doc Test vectors for the Arweave block index.
-module(dev_arweave_block_index_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(RUN_SIZE, 4096).
-define(ENTRY_SIZE, 89).
-define(RUN_INDEX_SIZE, 56).

%%% Tests. The tests use a small run size so that a handful of entries spans
%%% several runs; `default_run_size_test' covers the shipped one.
%% @doc A store key carrying `..' is refused before it reaches the cache.
%%
%% `run-index' and the `runs' map arrive on a caller-supplied base -- both this
%% device's keys are reachable over HTTP in their own right -- so the base is
%% checked before it names anything. `/' is ordinary here: this device's own
%% keys are path-namespaced.
store_key_cannot_escape_the_store_test() ->
    ?assertThrow(
        {unsafe_store_key, _},
        dev_arweave_block_index:safe_key(<<"../../secret">>)
    ),
    ?assertThrow({unsafe_store_key, _}, dev_arweave_block_index:safe_key(<<"runs/../../secret">>)),
    ?assertEqual(
        <<"~arweave-block-index@2.9/runs/7">>,
        dev_arweave_block_index:safe_key(<<"~arweave-block-index@2.9/runs/7">>)
    ),
    % Not a binary: passed through to fail where it would have failed before.
    ?assertEqual(not_found, dev_arweave_block_index:safe_key(not_found)).


-define(TEST_RUN_SIZE, 64).

test_opts() ->
    #{ <<"store">> => [hb_test_utils:test_store()] }.

test_base() ->
    #{
        <<"device">> => <<"arweave-block-index@2.9">>,
        <<"run-size">> => ?TEST_RUN_SIZE
    }.

%% @doc Build an index by handing the wire form to `from-binary', exactly as a
%% bootstrap fetch would.
test_index(Entries, Opts) ->
    test_index(test_base(), Entries, Opts).
test_index(Base, Entries, Opts) ->
    hb_util:ok(
        hb_ao:resolve(
            Base,
            #{ <<"path">> => <<"from-binary">>, <<"body">> => test_wire(Entries) },
            Opts
        )
    ).

%% @doc Resolve a key and drop the private hashpath the resolver records, so
%% that a test can assert on the whole result.
test_resolve(Base, Req, Opts) ->
    case hb_ao:resolve(Base, Req, Opts) of
        {ok, Result} -> {ok, hb_maps:without([<<"priv">>], Result, Opts)};
        Other -> Other
    end.

%% @doc Encode triplets in the form `/block_index2' serves.
test_wire(Entries) ->
    << << (test_wire_entry(Entry))/binary >> || Entry <- Entries >>.

test_wire_entry({Hash, WeaveSize, TXRoot}) ->
    Encoded = binary:encode_unsigned(WeaveSize),
    << Hash:48/binary, (byte_size(Encoded)):16, Encoded/binary,
        (byte_size(TXRoot)):8, TXRoot/binary >>.

%% @doc A run of synthetic entries with strictly growing weave sizes.
test_entries(Count) ->
    [
        {crypto:strong_rand_bytes(48), N * 262144, crypto:strong_rand_bytes(32)}
    ||
        N <- lists:seq(1, Count)
    ].

test_root(Entries) ->
    hb_util:encode(
        ar_unbalanced_merkle:block_index_to_merkle_root(lists:reverse(Entries))
    ).

%% @doc Every entry the index ingested comes back out, across run boundaries.
at_test() ->
    Opts = test_opts(),
    Entries = test_entries((?TEST_RUN_SIZE * 2) + 7),
    Index = test_index(Entries, Opts),
    ?assertEqual(
        (?TEST_RUN_SIZE * 2) + 7,
        hb_maps:get(<<"length">>, Index, not_found, Opts)
    ),
    lists:foreach(
        fun({Height, {Hash, WeaveSize, TXRoot}}) ->
            ?assertEqual(
                {ok,
                    #{
                        <<"indep-hash">> => hb_util:encode(Hash),
                        <<"weave-size">> => WeaveSize,
                        <<"tx-root">> => hb_util:encode(TXRoot)
                    }
                },
                test_resolve(
                    Index,
                    #{ <<"path">> => <<"at">>, <<"height">> => Height },
                    Opts
                )
            )
        end,
        lists:zip(lists:seq(0, length(Entries) - 1), Entries)
    ).

%% @doc A height the index does not cover is an error, not a wrapped read.
at_out_of_range_test() ->
    Opts = test_opts(),
    Index = test_index(test_entries(4), Opts),
    {error, Error} =
        hb_ao:resolve(
            Index,
            #{ <<"path">> => <<"at">>, <<"height">> => 4 },
            Opts
        ),
    ?assertEqual(
        <<"height-out-of-range">>,
        hb_maps:get(<<"message">>, Error, not_found, Opts)
    ).

%% @doc Every byte a block wrote maps back to that block's range and tx root,
%% at the first byte, the second byte and the last byte of each block.
bounds_test() ->
    Opts = test_opts(),
    Entries = test_entries(?TEST_RUN_SIZE + 3),
    Index = test_index(Entries, Opts),
    lists:foreach(
        fun({Height, {_, WeaveSize, TXRoot}}) ->
            Start = Height * 262144,
            Expected =
                {ok,
                    #{
                        <<"block-start">> => Start,
                        <<"block-end">> => WeaveSize,
                        <<"tx-root">> => hb_util:encode(TXRoot)
                    }
                },
            lists:foreach(
                fun(Offset) ->
                    ?assertEqual(
                        Expected,
                        test_resolve(
                            Index,
                            #{ <<"path">> => <<"bounds">>, <<"offset">> => Offset },
                            Opts
                        )
                    )
                end,
                [Start, Start + 1, WeaveSize - 1]
            )
        end,
        lists:zip(lists:seq(0, length(Entries) - 1), Entries)
    ).

%% @doc A block that added no data shares its predecessor's weave size, so
%% several heights carry the same weave size. Every byte must still resolve to
%% the block that wrote it -- the *lowest* height above the offset -- not to an
%% empty block that follows it. Getting this wrong returns the wrong tx root
%% for every byte of the preceding block, and silently breaks proof-of-access
%% validation rather than failing loudly.
bounds_with_empty_blocks_test() ->
    Opts = test_opts(),
    First = crypto:strong_rand_bytes(32),
    Second = crypto:strong_rand_bytes(32),
    Entries =
        [
            {crypto:strong_rand_bytes(48), 262144, First},
            {crypto:strong_rand_bytes(48), 262144, <<>>},
            {crypto:strong_rand_bytes(48), 262144, <<>>},
            {crypto:strong_rand_bytes(48), 524288, Second}
        ],
    Index = test_index(Entries, Opts),
    lists:foreach(
        fun({Offset, Start, End, TXRoot}) ->
            ?assertEqual(
                {ok,
                    #{
                        <<"block-start">> => Start,
                        <<"block-end">> => End,
                        <<"tx-root">> => hb_util:encode(TXRoot)
                    }
                },
                test_resolve(
                    Index,
                    #{ <<"path">> => <<"bounds">>, <<"offset">> => Offset },
                    Opts
                )
            )
        end,
        [
            {0, 0, 262144, First},
            {262143, 0, 262144, First},
            {262144, 262144, 524288, Second},
            {524287, 262144, 524288, Second}
        ]
    ).

%% @doc `bounds/3' answers exactly what the vendored arithmetic answers over
%% the same index, for every byte around every block boundary. The device
%% keeps its own search so that a lookup reads one run rather than two, and
%% this is what holds the two implementations to the same result.
bounds_matches_vendored_test() ->
    Opts = test_opts(),
    Entries = test_entries(?TEST_RUN_SIZE + 3),
    Index = test_index(Entries, Opts),
    Seek = dev_arweave_block_index:seek(Index, Opts),
    lists:foreach(
        fun(Offset) ->
            {BlockStart, BlockEnd, TXRoot} =
                ar_block_index:get_block_bounds(Offset, Seek),
            ?assertEqual(
                {ok,
                    #{
                        <<"block-start">> => BlockStart,
                        <<"block-end">> => BlockEnd,
                        <<"tx-root">> => hb_util:encode(TXRoot)
                    }
                },
                test_resolve(
                    Index,
                    #{ <<"path">> => <<"bounds">>, <<"offset">> => Offset },
                    Opts
                )
            )
        end,
        lists:flatten(
            [
                [(N * 262144) - 1, N * 262144, (N * 262144) + 1]
            ||
                N <- lists:seq(1, ?TEST_RUN_SIZE + 2)
            ]
        )
    ).

%% @doc The seek answers the vendored module's three queries.
seek_test() ->
    Opts = test_opts(),
    Entries = test_entries(?TEST_RUN_SIZE + 3),
    Index = test_index(Entries, Opts),
    Seek = dev_arweave_block_index:seek(Index, Opts),
    {Hash, WeaveSize, TXRoot} = lists:nth(?TEST_RUN_SIZE + 1, Entries),
    ?assertEqual({Hash, WeaveSize, TXRoot}, Seek({height, ?TEST_RUN_SIZE})),
    ?assertEqual(not_found, Seek({height, ?TEST_RUN_SIZE + 3})),
    ?assertEqual(
        {?TEST_RUN_SIZE, {Hash, WeaveSize, TXRoot}},
        Seek({hash, Hash})
    ),
    ?assertEqual(not_found, Seek({hash, crypto:strong_rand_bytes(48)})),
    ?assertEqual(
        {?TEST_RUN_SIZE, {Hash, WeaveSize, TXRoot}},
        Seek({weave_size_above, WeaveSize - 1})
    ),
    ?assertEqual(
        not_found,
        Seek({weave_size_above, (?TEST_RUN_SIZE + 3) * 262144})
    ).

%% @doc An offset past the end of the weave has no block, and says so.
bounds_out_of_range_test() ->
    Opts = test_opts(),
    Index = test_index(test_entries(4), Opts),
    {error, Error} =
        hb_ao:resolve(
            Index,
            #{ <<"path">> => <<"bounds">>, <<"offset">> => 4 * 262144 },
            Opts
        ),
    ?assertEqual(
        <<"offset-out-of-range">>,
        hb_maps:get(<<"message">>, Error, not_found, Opts)
    ).

%% @doc The recomputed root matches both the root carried forward across
%% appends and a fold over the same entries by the vendored module. If the
%% three ever diverge, the index or the incremental update is wrong.
root_test() ->
    Opts = test_opts(),
    Entries = test_entries(?TEST_RUN_SIZE + 5),
    Index =
        lists:foldl(
            fun({Hash, WeaveSize, TXRoot}, Acc) ->
                hb_util:ok(
                    hb_ao:resolve(
                        Acc,
                        #{
                            <<"path">> => <<"append">>,
                            <<"indep-hash">> => hb_util:encode(Hash),
                            <<"weave-size">> => WeaveSize,
                            <<"tx-root">> => hb_util:encode(TXRoot)
                        },
                        Opts
                    )
                )
            end,
            test_base(),
            Entries
        ),
    ?assertEqual(
        {ok, #{ <<"root">> => test_root(Entries) }},
        test_resolve(Index, <<"root">>, Opts)
    ),
    ?assertEqual(test_root(Entries), hb_maps:get(<<"root">>, Index, not_found, Opts)).

%% @doc The shipped run size behaves as the test one does.
default_run_size_test() ->
    Opts = test_opts(),
    Entries = test_entries(24),
    Index =
        test_index(
            #{ <<"device">> => <<"arweave-block-index@2.9">> },
            Entries,
            Opts
        ),
    ?assertEqual(?RUN_SIZE, hb_maps:get(<<"run-size">>, Index, not_found, Opts)),
    ?assertEqual(
        {ok, #{ <<"root">> => test_root(Entries) }},
        test_resolve(Index, <<"root">>, Opts)
    ).

%% @doc An index built page by page is identical to one built in a single
%% call. Bootstrap pages the fetch, so the two must not diverge.
paged_ingest_test() ->
    Opts = test_opts(),
    Entries = test_entries(?TEST_RUN_SIZE + 11),
    {Head, Tail} = lists:split(?TEST_RUN_SIZE - 2, Entries),
    Paged = test_index(test_index(Head, Opts), Tail, Opts),
    ?assertEqual(test_root(Entries), hb_maps:get(<<"root">>, Paged, not_found, Opts)),
    ?assertEqual(
        {ok, #{ <<"root">> => test_root(Entries) }},
        test_resolve(Paged, <<"root">>, Opts)
    ).

%% @doc A page that does not continue where the index ends is rejected. A
%% dropped page would otherwise produce an index that is well-formed and
%% wrong.
non_contiguous_page_test() ->
    Opts = test_opts(),
    Index = test_index(test_entries(4), Opts),
    {error, Error} =
        hb_ao:resolve(
            Index,
            #{
                <<"path">> => <<"from-binary">>,
                <<"body">> => test_wire(test_entries(2)),
                <<"start-height">> => 9
            },
            Opts
        ),
    ?assertEqual(
        <<"non-contiguous-index-range">>,
        hb_maps:get(<<"message">>, Error, not_found, Opts)
    ).

%% @doc Unwinding a reorg leaves exactly the index that would have been built
%% without the unwound blocks.
rollback_test() ->
    Opts = test_opts(),
    Entries = test_entries(?TEST_RUN_SIZE + 6),
    Kept = lists:sublist(Entries, ?TEST_RUN_SIZE + 2),
    Rolled =
        hb_util:ok(
            hb_ao:resolve(
                test_index(Entries, Opts),
                #{ <<"path">> => <<"rollback">>, <<"count">> => 4 },
                Opts
            )
        ),
    ?assertEqual(
        ?TEST_RUN_SIZE + 2,
        hb_maps:get(<<"length">>, Rolled, not_found, Opts)
    ),
    ?assertEqual(test_root(Kept), hb_maps:get(<<"root">>, Rolled, not_found, Opts)),
    ?assertEqual(
        {ok, #{ <<"root">> => test_root(Kept) }},
        test_resolve(Rolled, <<"root">>, Opts)
    ).

%% @doc Unwinding past genesis is an error rather than an empty index.
rollback_too_far_test() ->
    Opts = test_opts(),
    Index = test_index(test_entries(4), Opts),
    {error, Error} =
        hb_ao:resolve(
            Index,
            #{ <<"path">> => <<"rollback">>, <<"count">> => 5 },
            Opts
        ),
    ?assertEqual(
        <<"invalid-rollback-count">>,
        hb_maps:get(<<"message">>, Error, not_found, Opts)
    ).

%% @doc An index hashes to its own root.
verify_test() ->
    Opts = test_opts(),
    Entries = test_entries(?TEST_RUN_SIZE + 2),
    ?assertEqual(
        {ok, #{ <<"valid">> => true }},
        test_resolve(
            test_index(Entries, Opts),
            #{
                <<"path">> => <<"verify">>,
                <<"expected-root">> => test_root(Entries)
            },
            Opts
        )
    ).

%%% Mutation tests. Each mutates exactly the field its check guards and
%%% asserts the error that check produces. A mutant that still verifies means
%%% the check is not doing anything.

%% @doc Swapping two entries changes the root, though every entry is still
%% present. Order is part of what the root commits to. The two swapped entries
%% share a weave size -- as consecutive blocks that added no data do -- so the
%% reordering is invisible to the monotonicity check and only the root
%% catches it.
reordered_entry_test() ->
    Opts = test_opts(),
    [A, {BHash, WeaveSize, BTXRoot}, {CHash, _, CTXRoot} | Rest] =
        test_entries(?TEST_RUN_SIZE + 4),
    B = {BHash, WeaveSize, BTXRoot},
    C = {CHash, WeaveSize, CTXRoot},
    ?assertEqual(
        {error, <<"invalid-block-index-root">>},
        verify_against(
            test_index([A, C, B | Rest], Opts),
            test_root([A, B, C | Rest]),
            Opts
        )
    ).

%% @doc An entry whose weave size moves backwards is rejected on ingest. The
%% binary search in `bounds/3' could not reach the entries behind it.
non_monotonic_entry_test() ->
    Opts = test_opts(),
    [A, B, C | Rest] = test_entries(8),
    {error, Error} =
        hb_ao:resolve(
            test_base(),
            #{
                <<"path">> => <<"from-binary">>,
                <<"body">> => test_wire([A, C, B | Rest])
            },
            Opts
        ),
    ?assertEqual(
        <<"non-monotonic-weave-size">>,
        hb_maps:get(<<"message">>, Error, not_found, Opts)
    ).

%% @doc An entry too wide for the stored form is rejected rather than crashing
%% the ingest. The wire form is wider than the stored one -- a tx root's length
%% is recorded in a byte, so a peer may serve one of up to 255 bytes where the
%% stored entry holds 32 -- and the binary parses cleanly before the mismatch
%% is reached. This is the untrusted network path, so a `function_clause' out
%% of it would be a hostile peer taking the ingest down.
oversized_tx_root_test() ->
    Opts = test_opts(),
    [{Hash, WeaveSize, _TXRoot} | Rest] = test_entries(4),
    ?assertEqual(
        <<"invalid-block-index-entry">>,
        ingest_rejection(
            [{Hash, WeaveSize, crypto:strong_rand_bytes(64)} | Rest],
            Opts
        )
    ).

%% @doc The same for a weave size wider than the 64 bits a stored entry holds.
%% The wire form records its length in two bytes, so a peer may serve one of
%% any width. The block hash is the one field the two forms agree upon: the
%% wire form fixes it at 48 bytes, so no entry that parses can fail on it.
oversized_weave_size_test() ->
    Opts = test_opts(),
    Entries = test_entries(4),
    {Hash, _WeaveSize, TXRoot} = lists:last(Entries),
    ?assertEqual(
        <<"invalid-block-index-entry">>,
        ingest_rejection(
            lists:droplast(Entries) ++ [{Hash, 1 bsl 64, TXRoot}],
            Opts
        )
    ).

%% @doc The message `from-binary' rejects a set of entries with.
ingest_rejection(Entries, Opts) ->
    {error, Error} =
        hb_ao:resolve(
            test_base(),
            #{
                <<"path">> => <<"from-binary">>,
                <<"body">> => test_wire(Entries)
            },
            Opts
        ),
    hb_maps:get(<<"message">>, Error, not_found, Opts).

%% @doc An index missing its last entry does not verify against the full
%% index's root. Truncation is the cheapest attack on a paged fetch.
truncated_index_test() ->
    Opts = test_opts(),
    Entries = test_entries(?TEST_RUN_SIZE + 4),
    ?assertEqual(
        {error, <<"invalid-block-index-root">>},
        verify_against(
            test_index(lists:droplast(Entries), Opts),
            test_root(Entries),
            Opts
        )
    ).

%% @doc A wire binary that ends mid-entry is rejected rather than silently
%% dropping the partial entry. A short read is the normal failure of a paged
%% fetch.
truncated_binary_test() ->
    Opts = test_opts(),
    Wire = test_wire(test_entries(4)),
    {error, Error} =
        hb_ao:resolve(
            test_base(),
            #{
                <<"path">> => <<"from-binary">>,
                <<"body">> => binary:part(Wire, 0, byte_size(Wire) - 5)
            },
            Opts
        ),
    ?assertEqual(
        <<"invalid-block-index-binary">>,
        hb_maps:get(<<"message">>, Error, not_found, Opts)
    ).

%% @doc Changing one entry's tx root changes the root of the whole index.
mutated_tx_root_test() ->
    Opts = test_opts(),
    [{Hash, WeaveSize, _} | Rest] = Entries = test_entries(?TEST_RUN_SIZE + 4),
    Mutated = [{Hash, WeaveSize, crypto:strong_rand_bytes(32)} | Rest],
    ?assertEqual(
        {error, <<"invalid-block-index-root">>},
        verify_against(test_index(Mutated, Opts), test_root(Entries), Opts)
    ).

%% @doc Changing one entry's block hash changes the root of the whole index.
mutated_indep_hash_test() ->
    Opts = test_opts(),
    [{_, WeaveSize, TXRoot} | Rest] = Entries = test_entries(?TEST_RUN_SIZE + 4),
    Mutated = [{crypto:strong_rand_bytes(48), WeaveSize, TXRoot} | Rest],
    ?assertEqual(
        {error, <<"invalid-block-index-root">>},
        verify_against(test_index(Mutated, Opts), test_root(Entries), Opts)
    ).

%% @doc Changing one entry's weave size changes the root of the whole index,
%% even when the change keeps the index monotonic.
mutated_weave_size_test() ->
    Opts = test_opts(),
    [A, {Hash, WeaveSize, TXRoot} | Rest] = Entries = test_entries(?TEST_RUN_SIZE + 4),
    Mutated = [A, {Hash, WeaveSize - 1, TXRoot} | Rest],
    ?assertEqual(
        {error, <<"invalid-block-index-root">>},
        verify_against(test_index(Mutated, Opts), test_root(Entries), Opts)
    ).

%% @doc Resolve `verify' and reduce the result to its error `message', so that
%% a mutation test asserts on the check that fired and nothing else.
verify_against(Index, Root, Opts) ->
    case
        hb_ao:resolve(
            Index,
            #{ <<"path">> => <<"verify">>, <<"expected-root">> => Root },
            Opts
        )
    of
        {ok, Result} ->
            {ok, Result};
        {error, Error} ->
            {error, hb_maps:get(<<"message">>, Error, not_found, Opts)}
    end.
