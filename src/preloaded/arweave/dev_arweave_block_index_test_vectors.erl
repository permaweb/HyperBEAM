%%% @doc Test vectors for the AO-native Arweave block index.
-module(dev_arweave_block_index_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(LEAF_SIZE, 128).

%% @doc Reuse a bounded set of stores within one test VM. Separate VMs get
%% separate paths, so concurrent suites cannot reset one another's stores.
test_opts(Tag) ->
    Run = list_to_binary(os:getpid()),
    Root = <<"cache-TEST/arweave-block-index-", Run/binary>>,
    Store = #{
        <<"store-module">> => hb_store_lmdb,
        <<"name">> =>
            case Tag of
                <<"block-index-isolated">> ->
                    <<Root/binary, "-isolated">>;
                _ ->
                    Root
            end
    },
    ok = hb_store:reset(Store),
    #{ <<"store">> => [Store] }.

test_base() ->
    #{ <<"device">> => <<"arweave-block-index@2.9">> }.

%% @doc Build an index through its semantic AO append interface.
test_index(Entries, Opts) ->
    test_index(test_base(), Entries, Opts).
test_index(Base, Entries, Opts) ->
    Length = hb_util:int(hb_maps:get(<<"length">>, Base, 0, Opts)),
    hb_util:ok(
        hb_ao:resolve(
            Base,
            #{
                <<"path">> => <<"append">>,
                <<"start-height">> => Length,
                <<"entries">> =>
                    hb_util:list_to_numbered_message(
                        [entry_message(Entry) || Entry <- Entries]
                    )
            },
            Opts
        )
    ).

%% @doc Append one entry using the public single-entry form.
append_one(Base, Entry, Opts) ->
    hb_util:ok(
        hb_ao:resolve(
            Base,
            maps:merge(#{ <<"path">> => <<"append">> }, entry_message(Entry)),
            Opts
        )
    ).

entry_message({Hash, WeaveSize, TXRoot}) ->
    #{
        <<"indep-hash">> => hb_util:encode(Hash),
        <<"weave-size">> => WeaveSize,
        <<"tx-root">> => hb_util:encode(TXRoot)
    }.

%% @doc Resolve a key without the resolver's private hashpath in the result.
test_resolve(Base, Req, Opts) ->
    case hb_ao:resolve(Base, Req, Opts) of
        {ok, Result} -> {ok, hb_maps:without([<<"priv">>], Result, Opts)};
        Other -> Other
    end.

%% @doc Reproducible entries with strictly increasing weave sizes.
test_entries(Count) ->
    [test_entry(N) || N <- lists:seq(1, Count)].

test_entry(N) ->
    Seed = integer_to_binary(N),
    {
        crypto:hash(sha384, <<"block-", Seed/binary>>),
        N * 262144,
        crypto:hash(sha256, <<"tx-root-", Seed/binary>>)
    }.

test_root(Entries) ->
    hb_util:encode(
        ar_unbalanced_merkle:block_index_to_merkle_root(lists:reverse(Entries))
    ).

test_link(ID) ->
    {link, ID, #{ <<"type">> => <<"link">>, <<"lazy">> => false }}.

link_id({link, ID, _Opts}) -> ID.

%% @doc Replace one linked topology part while retaining public state fields.
replace_part(Index, Key, Part, Opts) ->
    {ok, PartID} = hb_cache:write(Part, Opts#{ <<"match-index">> => false }),
    Index#{ Key => test_link(PartID) }.

%% @doc Assert verification rejects a malformed internal topology.
assert_invalid_topology(Index, Root, Opts) ->
    {error, Error} = hb_ao:resolve(
        Index,
        #{ <<"path">> => <<"verify">>, <<"expected-root">> => Root },
        Opts
    ),
    ?assertEqual(
        <<"invalid-block-index-topology">>,
        hb_maps:get(<<"message">>, Error, not_found, Opts)
    ).

%% @doc The public state links its topology and survives an LMDB round trip.
linked_tree_survives_lmdb_round_trip_test() ->
    Opts = test_opts(<<"block-index-round-trip">>),
    Entries = test_entries((?LEAF_SIZE * 2) + 7),
    Index = test_index(Entries, Opts),
    ?assertMatch(
        {link, CompletedID, _} when ?IS_ID(CompletedID),
        maps:get(<<"completed">>, Index)
    ),
    ?assertMatch(
        {link, TailID, _} when ?IS_ID(TailID),
        maps:get(<<"tail">>, Index)
    ),
    ?assertEqual(
        [<<"completed">>, <<"device">>, <<"length">>, <<"root">>, <<"tail">>],
        lists:sort(
            maps:keys(hb_maps:without([<<"priv">>], Index, Opts))
        )
    ),
    {ok, ID} = hb_cache:write(Index, Opts),
    {ok, Stored} = hb_cache:read(ID, Opts),
    assert_entry(Stored, 0, hd(Entries), Opts),
    assert_entry(Stored, ?LEAF_SIZE, lists:nth(?LEAF_SIZE + 1, Entries), Opts),
    ?assertEqual(
        {ok, #{ <<"root">> => test_root(Entries) }},
        test_resolve(Stored, <<"root">>, Opts)
    ).

%% @doc Standard commitments and resolver metadata do not alter state shape.
committed_state_resolves_test() ->
    Opts = test_opts(<<"block-index-committed-state">>),
    Entries = test_entries(2),
    Index = test_index(Entries, Opts),
    Committed = hb_message:commit(
        Index,
        Opts#{ <<"priv-wallet">> => hb:wallet() }
    ),
    ?assertEqual(
        {ok, #{ <<"root">> => test_root(Entries) }},
        test_resolve(Committed, <<"root">>, Opts)
    ).

%% @doc Legacy storage shapes are invalid states, not bootstrap inputs.
legacy_state_shape_is_rejected_test() ->
    Opts = test_opts(<<"block-index-legacy-shape">>),
    Legacy = (test_base())#{
        <<"previous">> => <<"legacy">>,
        <<"runs">> => #{},
        <<"run-index">> => <<"legacy">>,
        <<"run-size">> => 5000,
        <<"tree">> => <<"legacy">>
    },
    Requests = [
        #{ <<"path">> => <<"root">> },
        #{
            <<"path">> => <<"verify">>,
            <<"expected-root">> => <<>>
        },
        maps:merge(
            #{ <<"path">> => <<"append">> },
            entry_message(test_entry(1))
        )
    ],
    lists:foreach(
        fun(Req) ->
            {error, Error} = hb_ao:resolve(Legacy, Req, Opts),
            ?assertEqual(
                <<"invalid-block-index-topology">>,
                hb_maps:get(<<"message">>, Error, not_found, Opts)
            )
        end,
        Requests
    ).

%% @doc Linked topology in one LMDB is unavailable in another.
linked_tree_is_store_isolated_test() ->
    SourceOpts = test_opts(<<"block-index-source">>),
    IsolatedOpts = test_opts(<<"block-index-isolated">>),
    Index = test_index(test_entries(2), SourceOpts),
    assert_entry(Index, 0, test_entry(1), SourceOpts),
    {error, Error} = hb_ao:resolve(
        Index,
        #{ <<"path">> => <<"at">>, <<"height">> => 0 },
        IsolatedOpts
    ),
    ?assertEqual(
        <<"invalid-block-index-topology">>,
        hb_maps:get(<<"message">>, Error, not_found, IsolatedOpts)
    ).

%% @doc Scalar paths and named aliases are not topology dependencies.
raw_store_paths_are_not_tree_links_test() ->
    Opts = test_opts(<<"block-index-raw-links">>),
    Index = test_index(test_entries(2), Opts),
    Raw = Index#{ <<"tail">> => <<"data/attacker-selected">> },
    Alias = Index#{ <<"tail">> => test_link(<<"attacker/tree">>) },
    lists:foreach(
        fun(Malformed) ->
            {error, Error} = hb_ao:resolve(
                Malformed,
                #{ <<"path">> => <<"at">>, <<"height">> => 0 },
                Opts
            ),
            ?assertEqual(
                <<"invalid-block-index-topology">>,
                hb_maps:get(<<"message">>, Error, not_found, Opts)
            )
        end,
        [Raw, Alias]
    ).

%% @doc A missing canonical content link fails closed.
missing_content_link_fails_closed_test() ->
    Opts = test_opts(<<"block-index-missing-link">>),
    Index = test_index(test_entries(1), Opts),
    Missing = Index#{
        <<"tail">> => test_link(hb_util:encode(crypto:strong_rand_bytes(32)))
    },
    Requests = [
        #{ <<"path">> => <<"at">>, <<"height">> => 0 },
        #{ <<"path">> => <<"bounds">>, <<"offset">> => 0 },
        maps:merge(
            #{ <<"path">> => <<"append">> },
            entry_message(test_entry(2))
        )
    ],
    lists:foreach(
        fun(Req) ->
            {error, Error} = hb_ao:resolve(Missing, Req, Opts),
            ?assertEqual(
                <<"invalid-block-index-topology">>,
                hb_maps:get(<<"message">>, Error, not_found, Opts)
            )
        end,
        Requests
    ).

%% @doc Missing and malformed verification roots return a stable device error.
malformed_verify_root_test() ->
    Opts = test_opts(<<"block-index-error-classification">>),
    Index = test_index(test_entries(1), Opts),
    lists:foreach(
        fun(Req) ->
            {error, Error} = hb_ao:resolve(Index, Req, Opts),
            ?assertEqual(
                <<"invalid-block-index-root">>,
                hb_maps:get(<<"message">>, Error, not_found, Opts)
            )
        end,
        [
            #{ <<"path">> => <<"verify">> },
            #{ <<"path">> => <<"verify">>, <<"expected-root">> => <<"!">> }
        ]
    ).

%% @doc Every entry is readable across leaf and branch boundaries.
at_test() ->
    Opts = test_opts(<<"block-index-at">>),
    Entries = test_entries((?LEAF_SIZE * 2) + 7),
    Index = test_index(Entries, Opts),
    ?assertEqual(
        length(Entries),
        hb_util:int(hb_maps:get(<<"length">>, Index, not_found, Opts))
    ),
    lists:foreach(
        fun({Height, Entry}) -> assert_entry(Index, Height, Entry, Opts) end,
        lists:zip(lists:seq(0, length(Entries) - 1), Entries)
    ).

assert_entry(Index, Height, {Hash, WeaveSize, TXRoot}, Opts) ->
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
    ).

%% @doc Heights outside the stored prefix are rejected.
at_out_of_range_test() ->
    Opts = test_opts(<<"block-index-at-range">>),
    Index = test_index(test_entries(4), Opts),
    lists:foreach(
        fun(Height) ->
            {error, Error} = hb_ao:resolve(
                Index,
                #{ <<"path">> => <<"at">>, <<"height">> => Height },
                Opts
            ),
            ?assertEqual(
                <<"height-out-of-range">>,
                hb_maps:get(<<"message">>, Error, not_found, Opts)
            )
        end,
        [-1, 4]
    ).

%% @doc Every byte around every leaf boundary maps to the correct block.
bounds_test() ->
    Opts = test_opts(<<"block-index-bounds">>),
    Entries = test_entries(?LEAF_SIZE + 3),
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
                            #{ <<"path">> => <<"bounds">>,
                                <<"offset">> => Offset },
                            Opts
                        )
                    )
                end,
                [Start, Start + 1, WeaveSize - 1]
            )
        end,
        lists:zip(lists:seq(0, length(Entries) - 1), Entries)
    ).

%% @doc Bounds controls belong to the AO request, while the carried index stays
%% an exact canonical state. Putting the same control into the state is rejected
%% before any topology is searched.
bounds_request_is_not_state_test() ->
    Opts = test_opts(<<"block-index-bounds-request">>),
    [Entry = {_Hash, WeaveSize, TXRoot}] = test_entries(1),
    Index = test_index([Entry], Opts),
    ?assertEqual(
        {ok,
            #{
                <<"block-start">> => 0,
                <<"block-end">> => WeaveSize,
                <<"tx-root">> => hb_util:encode(TXRoot)
            }
        },
        test_resolve(
            Index,
            #{ <<"path">> => <<"bounds">>, <<"offset">> => 0 },
            Opts
        )
    ),
    {error, Error} =
        hb_ao:resolve(Index#{ <<"offset">> => 0 }, <<"bounds">>, Opts),
    ?assertEqual(
        <<"invalid-block-index-topology">>,
        hb_maps:get(<<"message">>, Error, not_found, Opts)
    ).

%% @doc Empty blocks do not capture bytes written by their predecessor.
bounds_with_empty_blocks_test() ->
    Opts = test_opts(<<"block-index-empty-blocks">>),
    First = crypto:hash(sha256, <<"first">>),
    Second = crypto:hash(sha256, <<"second">>),
    Entries =
        [
            {crypto:hash(sha384, <<"1">>), 262144, First},
            {crypto:hash(sha384, <<"2">>), 262144, <<>>},
            {crypto:hash(sha384, <<"3">>), 262144, <<>>},
            {crypto:hash(sha384, <<"4">>), 524288, Second}
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

%% @doc Negative and past-end offsets have no covering block.
bounds_out_of_range_test() ->
    Opts = test_opts(<<"block-index-bounds-range">>),
    Index = test_index(test_entries(4), Opts),
    lists:foreach(
        fun(Offset) ->
            {error, Error} = hb_ao:resolve(
                Index,
                #{ <<"path">> => <<"bounds">>, <<"offset">> => Offset },
                Opts
            ),
            ?assertEqual(
                <<"offset-out-of-range">>,
                hb_maps:get(<<"message">>, Error, not_found, Opts)
            )
        end,
        [-1, 4 * 262144]
    ).

%% @doc Malformed request integers return one stable semantic error.
malformed_request_integer_test() ->
    Opts = test_opts(<<"block-index-request-integer">>),
    Index = test_index(test_entries(4), Opts),
    Requests = [
        #{ <<"path">> => <<"at">> },
        #{ <<"path">> => <<"at">>, <<"height">> => <<"not-an-integer">> },
        #{ <<"path">> => <<"bounds">> },
        #{ <<"path">> => <<"bounds">>, <<"offset">> => <<"not-an-integer">> },
        maps:merge(
            #{ <<"path">> => <<"append">>,
                <<"start-height">> => <<"not-an-integer">> },
            entry_message(test_entry(5))
        )
    ],
    lists:foreach(
        fun(Req) ->
            {error, Error} = hb_ao:resolve(Index, Req, Opts),
            ?assertEqual(
                <<"invalid-block-index-integer">>,
                hb_maps:get(<<"message">>, Error, not_found, Opts)
            )
        end,
        Requests
    ).

%% @doc Bulk and one-at-a-time appends preserve Arweave's exact accumulator.
root_and_append_parity_test() ->
    Opts = test_opts(<<"block-index-root">>),
    Entries = test_entries(?LEAF_SIZE + 5),
    OneByOne = lists:foldl(
        fun(Entry, Acc) -> append_one(Acc, Entry, Opts) end,
        test_base(),
        Entries
    ),
    Bulk = test_index(Entries, Opts),
    Expected = test_root(Entries),
    ?assertEqual(Expected, hb_maps:get(<<"root">>, OneByOne, not_found, Opts)),
    ?assertEqual(Expected, hb_maps:get(<<"root">>, Bulk, not_found, Opts)),
    ?assertEqual(
        {ok, #{ <<"root">> => Expected }},
        test_resolve(OneByOne, <<"root">>, Opts)
    ).

%% @doc Semantic bootstrap batches compose exactly and reject dropped pages.
paged_append_test() ->
    Opts = test_opts(<<"block-index-paged">>),
    Entries = test_entries(?LEAF_SIZE + 11),
    {Head, Tail} = lists:split(?LEAF_SIZE - 2, Entries),
    Paged = test_index(test_index(Head, Opts), Tail, Opts),
    ?assertEqual(test_root(Entries), hb_maps:get(<<"root">>, Paged, Opts)),
    {error, Error} = hb_ao:resolve(
        Paged,
        #{
            <<"path">> => <<"append">>,
            <<"start-height">> => 9,
            <<"entries">> =>
                hb_util:list_to_numbered_message(
                    [entry_message(test_entry(1000))]
                )
        },
        Opts
    ),
    ?assertEqual(
        <<"non-contiguous-index-range">>,
        hb_maps:get(<<"message">>, Error, not_found, Opts)
    ).

%% @doc Extending a full 32-leaf branch retains the native root of its unchanged
%% final leaf while the right edge grows into the next branch.
completed_branch_rollover_append_test() ->
    Opts = test_opts(<<"block-index-completed-branch-rollover">>),
    Entries = test_entries(?LEAF_SIZE * 33),
    {InitialEntries, AppendedEntries} =
        lists:split(?LEAF_SIZE * 32, Entries),
    Initial = test_index(InitialEntries, Opts),
    Extended = test_index(Initial, AppendedEntries, Opts),
    ?assertEqual(test_root(Entries), hb_maps:get(<<"root">>, Extended, Opts)),
    assert_entry(Extended, length(InitialEntries) - 1, lists:last(InitialEntries), Opts),
    assert_entry(Extended, length(Entries) - 1, lists:last(Entries), Opts),
    ?assertEqual(
        {ok, #{ <<"root">> => test_root(Entries) }},
        test_resolve(Extended, <<"root">>, Opts)
    ).

%% @doc Verification accepts exactly the vendored accumulator.
verify_and_mutation_test() ->
    Opts = test_opts(<<"block-index-verify">>),
    Entries = test_entries(?LEAF_SIZE + 2),
    Index = test_index(Entries, Opts),
    ?assertEqual(
        {ok, #{ <<"valid">> => true }},
        test_resolve(
            Index,
            #{ <<"path">> => <<"verify">>,
                <<"expected-root">> => test_root(Entries) },
            Opts
        )
    ),
    [{Hash, WeaveSize, _TXRoot} | Rest] = Entries,
    Mutated = [
        {Hash, WeaveSize, crypto:hash(sha256, <<"mutated">>)} | Rest
    ],
    {error, MutationError} = hb_ao:resolve(
        test_index(Mutated, Opts),
        #{ <<"path">> => <<"verify">>,
            <<"expected-root">> => test_root(Entries) },
        Opts
    ),
    ?assertEqual(
        <<"invalid-block-index-root">>,
        hb_maps:get(<<"message">>, MutationError, not_found, Opts)
    ).

%% @doc Verification authenticates routing metadata as well as entry bodies.
topology_metadata_is_verified_test() ->
    Opts = test_opts(<<"block-index-topology-verify">>),
    Entries = test_entries(?LEAF_SIZE + 1),
    Index = test_index(Entries, Opts),
    FirstLeaf = hb_cache:ensure_loaded(maps:get(<<"completed">>, Index), Opts),
    PoisonedLeaf = FirstLeaf#{ <<"start-weave-size">> => 1 },
    {ok, PoisonedLeafID} = hb_cache:write(
        PoisonedLeaf,
        Opts#{ <<"match-index">> => false }
    ),
    assert_invalid_topology(
        Index#{ <<"completed">> => test_link(PoisonedLeafID) },
        test_root(Entries),
        Opts
    ).

%% @doc Verification rejects non-canonical branch levels and unary roots.
topology_shape_is_verified_test() ->
    Opts = test_opts(<<"block-index-topology-shape">>),
    Entries = test_entries(?LEAF_SIZE * 2),
    Index = test_index(Entries, Opts),
    CompletedLink = maps:get(<<"completed">>, Index),
    Completed = hb_cache:ensure_loaded(CompletedLink, Opts),
    SecondLink = maps:get(<<"2">>, Completed),
    Second = hb_cache:ensure_loaded(SecondLink, Opts),
    Unary = #{
        <<"node-type">> => <<"branch">>,
        <<"count">> => maps:get(<<"count">>, Second),
        <<"child-count">> => 1,
        <<"max-weave-size">> => maps:get(<<"max-weave-size">>, Second),
        <<"root">> => maps:get(<<"root">>, Second),
        <<"boundaries">> =>
            <<(maps:get(<<"count">>, Second)):64,
                (maps:get(<<"max-weave-size">>, Second)):64>>,
        <<"1">> => SecondLink
    },
    {ok, UnaryID} = hb_cache:write(
        Unary,
        Opts#{ <<"match-index">> => false }
    ),
    Regrouped = Completed#{ <<"2">> => test_link(UnaryID) },
    assert_invalid_topology(
        replace_part(Index, <<"completed">>, Regrouped, Opts),
        test_root(Entries),
        Opts
    ),
    UnaryRoot = Unary#{
        <<"count">> => maps:get(<<"count">>, Completed),
        <<"max-weave-size">> => maps:get(<<"max-weave-size">>, Completed),
        <<"root">> => maps:get(<<"root">>, Completed),
        <<"boundaries">> =>
            <<(maps:get(<<"count">>, Completed)):64,
                (maps:get(<<"max-weave-size">>, Completed)):64>>,
        <<"1">> => CompletedLink
    },
    assert_invalid_topology(
        replace_part(Index, <<"completed">>, UnaryRoot, Opts),
        test_root(Entries),
        Opts
    ).

%% @doc Read routing fails closed when no child covers the requested value.
exhausted_routing_is_rejected_test() ->
    Opts = test_opts(<<"block-index-exhausted-routing">>),
    Index = test_index(test_entries(?LEAF_SIZE * 2), Opts),
    Completed = hb_cache:ensure_loaded(
        maps:get(<<"completed">>, Index),
        Opts
    ),
    ChildCount = hb_util:int(maps:get(<<"child-count">>, Completed)),
    Poisoned = Completed#{
        <<"boundaries">> =>
            iolist_to_binary(
                [<<0:64, 0:64>> || _ <- lists:seq(1, ChildCount)]
            )
    },
    PoisonedIndex = replace_part(
        Index,
        <<"completed">>,
        Poisoned,
        Opts
    ),
    lists:foreach(
        fun(Req) ->
            {error, Error} = hb_ao:resolve(PoisonedIndex, Req, Opts),
            ?assertEqual(
                <<"invalid-block-index-topology">>,
                hb_maps:get(<<"message">>, Error, not_found, Opts)
            )
        end,
        [
            #{ <<"path">> => <<"at">>, <<"height">> => 0 },
            #{ <<"path">> => <<"bounds">>, <<"offset">> => 0 }
        ]
    ).

%% @doc Append authenticates its path; verify authenticates inherited branches.
append_trust_boundary_test() ->
    Opts = test_opts(<<"block-index-append-trust-boundary">>),
    Existing = test_entries(?LEAF_SIZE * 2),
    Added = lists:nthtail(
        ?LEAF_SIZE * 2,
        test_entries(?LEAF_SIZE * 3)
    ),
    Index = test_index(Existing, Opts),
    Completed = hb_cache:ensure_loaded(
        maps:get(<<"completed">>, Index),
        Opts
    ),
    Left = hb_cache:ensure_loaded(maps:get(<<"1">>, Completed), Opts),
    {ok, PoisonedLeftID} = hb_cache:write(
        Left#{ <<"start-weave-size">> => 1 },
        Opts#{ <<"match-index">> => false }
    ),
    PoisonedLeft = replace_part(
        Index,
        <<"completed">>,
        Completed#{ <<"1">> => test_link(PoisonedLeftID) },
        Opts
    ),
    Extended = test_index(PoisonedLeft, Added, Opts),
    assert_invalid_topology(
        Extended,
        test_root(Existing ++ Added),
        Opts
    ),
    MissingID = hb_util:encode(crypto:strong_rand_bytes(32)),
    PoisonedRight = replace_part(
        Index,
        <<"completed">>,
        Completed#{ <<"2">> => test_link(MissingID) },
        Opts
    ),
    {error, Error} = hb_ao:resolve(
        PoisonedRight,
        #{
            <<"path">> => <<"append">>,
            <<"start-height">> => ?LEAF_SIZE * 2,
            <<"entries">> => hb_util:list_to_numbered_message(
                [entry_message(Entry) || Entry <- Added]
            )
        },
        Opts
    ),
    ?assertEqual(
        <<"invalid-block-index-topology">>,
        hb_maps:get(<<"message">>, Error, not_found, Opts)
    ).

%% @doc Verification authenticates leaf ordering and canonical zero padding.
leaf_encoding_is_verified_test() ->
    Opts = test_opts(<<"block-index-leaf-encoding">>),
    Entries = test_entries(4),
    Index = test_index(Entries, Opts),
    Tail = hb_cache:ensure_loaded(maps:get(<<"tail">>, Index), Opts),
    Body = hb_maps:get(<<"body">>, Tail, not_found, Opts),
    WeavePos = 89 + 48,
    <<BeforeWeave:WeavePos/binary, _WeaveSize:64, AfterWeave/binary>> = Body,
    Decreasing = Tail#{
        <<"body">> => <<BeforeWeave/binary, 1:64, AfterWeave/binary>>
    },
    assert_invalid_topology(
        replace_part(Index, <<"tail">>, Decreasing, Opts),
        test_root(Entries),
        Opts
    ),
    {Hash, WeaveSize, _TXRoot} = test_entry(1),
    EmptyTX = [{Hash, WeaveSize, <<>>}],
    EmptyIndex = test_index(EmptyTX, Opts),
    EmptyTail = hb_cache:ensure_loaded(maps:get(<<"tail">>, EmptyIndex), Opts),
    <<BeforePadding:88/binary, _LastPaddingByte:8>> =
        hb_maps:get(<<"body">>, EmptyTail, not_found, Opts),
    NonCanonical = EmptyTail#{
        <<"body">> => <<BeforePadding/binary, 1:8>>
    },
    assert_invalid_topology(
        replace_part(EmptyIndex, <<"tail">>, NonCanonical, Opts),
        test_root(EmptyTX),
        Opts
    ).

%% @doc Reordering equal-weave entries changes the accumulator.
reordered_entry_test() ->
    Opts = test_opts(<<"block-index-reordered">>),
    [A, {BHash, WeaveSize, BTXRoot}, {CHash, _, CTXRoot} | Rest] =
        test_entries(8),
    B = {BHash, WeaveSize, BTXRoot},
    C = {CHash, WeaveSize, CTXRoot},
    {error, Error} = hb_ao:resolve(
        test_index([A, C, B | Rest], Opts),
        #{ <<"path">> => <<"verify">>,
            <<"expected-root">> => test_root([A, B, C | Rest]) },
        Opts
    ),
    ?assertEqual(
        <<"invalid-block-index-root">>,
        hb_maps:get(<<"message">>, Error, not_found, Opts)
    ).

%% @doc Semantic entries reject non-monotonic values and invalid widths.
invalid_entry_test() ->
    Opts = test_opts(<<"block-index-invalid-entry">>),
    [A, B, C | Rest] = test_entries(8),
    ?assertEqual(
        <<"non-monotonic-weave-size">>,
        append_rejection([A, C, B | Rest], Opts)
    ),
    [{Hash, WeaveSize, _TXRoot} | Tail] = test_entries(4),
    ?assertEqual(
        <<"invalid-block-index-entry">>,
        append_rejection(
            [{Hash, WeaveSize, crypto:strong_rand_bytes(64)} | Tail],
            Opts
        )
    ),
    {LastHash, _LastWeaveSize, LastTXRoot} = lists:last(test_entries(4)),
    ?assertEqual(
        <<"invalid-block-index-entry">>,
        append_rejection(
            lists:droplast(test_entries(4)) ++
                [{LastHash, 1 bsl 64, LastTXRoot}],
            Opts
        )
    ),
    ?assertEqual(
        <<"invalid-block-index-entry">>,
        append_rejection([{Hash, -1, LastTXRoot}], Opts)
    ),
    ?assertEqual(
        <<"invalid-block-index-entry">>,
        append_rejection(
            [{crypto:strong_rand_bytes(47), WeaveSize, LastTXRoot}],
            Opts
        )
    ),
    ?assertEqual(
        <<"invalid-block-index-entry">>,
        append_rejection(
            [{Hash, WeaveSize, crypto:strong_rand_bytes(1)}],
            Opts
        )
    ),
    InvalidRequests = [
        #{ <<"path">> => <<"append">> },
        #{
            <<"path">> => <<"append">>,
            <<"indep-hash">> => <<"!">>,
            <<"weave-size">> => WeaveSize,
            <<"tx-root">> => hb_util:encode(LastTXRoot)
        },
        #{
            <<"path">> => <<"append">>,
            <<"indep-hash">> => hb_util:encode(Hash),
            <<"weave-size">> => <<"not-an-integer">>,
            <<"tx-root">> => hb_util:encode(LastTXRoot)
        },
        #{ <<"path">> => <<"append">>, <<"entries">> => <<"invalid">> },
        #{
            <<"path">> => <<"append">>,
            <<"entries">> => #{
                <<"1">> => entry_message(test_entry(1)),
                <<"3">> => entry_message(test_entry(3))
            }
        }
    ],
    lists:foreach(
        fun(Req) ->
            ?assertEqual(
                <<"invalid-block-index-entry">>,
                append_request_rejection(Req, Opts)
            )
        end,
        InvalidRequests
    ).

append_rejection(Entries, Opts) ->
    append_request_rejection(
        #{
            <<"path">> => <<"append">>,
            <<"entries">> =>
                hb_util:list_to_numbered_message(
                    [entry_message(Entry) || Entry <- Entries]
                )
        },
        Opts
    ).

append_request_rejection(Req, Opts) ->
    {error, Error} = hb_ao:resolve(
        test_base(),
        Req,
        Opts
    ),
    hb_maps:get(<<"message">>, Error, not_found, Opts).

%% @doc Raw peer transport is not a device operation.
transport_binary_is_not_a_device_operation_test() ->
    Opts = test_opts(<<"block-index-no-transport">>),
    ?assertNotMatch(
        {ok, _},
        hb_ao:resolve(
            test_base(),
            #{ <<"path">> => <<"from-binary">>, <<"body">> => <<0:512>> },
            Opts
        )
    ).

%% @doc Internal topology nodes never populate the generic match index.
internal_nodes_skip_match_index_test() ->
    Opts = test_opts(<<"block-index-no-match-index">>),
    _Index = test_index(test_entries((?LEAF_SIZE * 2) + 1), Opts),
    LeafPath = <<"data/", (hb_path:hashpath(<<"leaf">>, Opts))/binary>>,
    BranchPath = <<"data/", (hb_path:hashpath(<<"branch">>, Opts))/binary>>,
    ?assertEqual(
        [],
        hb_cache:list(
            <<"~match@1.0&node-type=", LeafPath/binary>>,
            Opts
        )
    ),
    ?assertEqual(
        [],
        hb_cache:list(
            <<"~match@1.0&node-type=", BranchPath/binary>>,
            Opts
        )
    ).

%% @doc An ordinary append rewrites the tail but shares the completed tree.
append_path_copy_shares_prefix_test() ->
    Opts = test_opts(<<"block-index-path-copy">>),
    Original = test_index(test_entries(600), Opts),
    Extended = append_one(Original, test_entry(601), Opts),
    ?assertEqual(
        link_id(maps:get(<<"completed">>, Original)),
        link_id(maps:get(<<"completed">>, Extended))
    ),
    ?assertNotEqual(
        link_id(maps:get(<<"tail">>, Original)),
        link_id(maps:get(<<"tail">>, Extended))
    ).

%% @doc Filling a tail publishes it once into the immutable completed tree.
tail_rollover_test() ->
    Opts = test_opts(<<"block-index-tail-rollover">>),
    Partial = test_index(test_entries(?LEAF_SIZE - 1), Opts),
    Full = append_one(Partial, test_entry(?LEAF_SIZE), Opts),
    Extended = append_one(Full, test_entry(?LEAF_SIZE + 1), Opts),
    ?assertNot(maps:is_key(<<"tail">>, Full)),
    ?assertEqual(
        link_id(maps:get(<<"completed">>, Full)),
        link_id(maps:get(<<"completed">>, Extended))
    ),
    assert_entry(Extended, ?LEAF_SIZE, test_entry(?LEAF_SIZE + 1), Opts),
    ?assertEqual(
        {ok, #{ <<"root">> => test_root(test_entries(?LEAF_SIZE + 1)) }},
        test_resolve(Extended, <<"root">>, Opts)
    ).

%% @doc The extra branch level beyond 8192 entries preserves all operations.
three_level_tree_test() ->
    Opts = test_opts(<<"block-index-three-level">>),
    Entries = test_entries((?LEAF_SIZE * 33) + 8),
    Index = test_index(Entries, Opts),
    assert_entry(Index, 0, hd(Entries), Opts),
    assert_entry(Index, length(Entries) - 1, lists:last(Entries), Opts),
    {_, End, TXRoot} = lists:last(Entries),
    ?assertEqual(
        {ok,
            #{
                <<"block-start">> => End - 262144,
                <<"block-end">> => End,
                <<"tx-root">> => hb_util:encode(TXRoot)
            }
        },
        test_resolve(
            Index,
            #{ <<"path">> => <<"bounds">>, <<"offset">> => End - 1 },
            Opts
        )
    ),
    ?assertEqual(
        {ok, #{ <<"root">> => test_root(Entries) }},
        test_resolve(Index, <<"root">>, Opts)
    ).
