%%% @doc Deterministic proof-of-access boundary vectors for Arweave 2.9.
-module(dev_arweave_spora_test_vectors).
-include("include/hb.hrl").
-include("include/ar_consensus.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc A rebased data path is legal only after rebase support activates.
select_rebase_ruleset_test() ->
    Opts = opts(),
    Request = rebased_request(?MERKLE_REBASE_SUPPORT_THRESHOLD),
    ?assertMatch({ok, _}, hb_ao:resolve(Request, <<"validate">>, Opts)),
    ?assertEqual(
        <<"invalid-data-path">>,
        rejection(
            rebased_request(
                ?MERKLE_REBASE_SUPPORT_THRESHOLD - ?DATA_CHUNK_SIZE
            ),
            Opts
        )
    ).

%% @doc A non-border chunk is legal only under the pre-rebase strict-borders rules.
select_strict_borders_ruleset_test() ->
    Opts = opts(),
    Start = ?STRICT_DATA_SPLIT_THRESHOLD - 150000,
    ?assertMatch(
        {ok, _},
        hb_ao:resolve(split_request(Start), <<"validate">>, Opts)
    ),
    ?assertEqual(
        <<"invalid-data-path">>,
        rejection(split_request(Start + ?DATA_CHUNK_SIZE), Opts)
    ).

rebased_request(BlockStartOffset) ->
    Leaves =
        [
            [{leaf(1), ?DATA_CHUNK_SIZE}, {leaf(2), 2 * ?DATA_CHUNK_SIZE}],
            {leaf(3), ?DATA_CHUNK_SIZE}
        ],
    synthetic_request(
        BlockStartOffset,
        BlockStartOffset,
        Leaves,
        3 * ?DATA_CHUNK_SIZE,
        0,
        leaf(1)
    ).

split_request(BlockStartOffset) ->
    Leaves = [{leaf(1), 100000}, {leaf(2), 200000}, {leaf(3), 300000}],
    synthetic_request(
        BlockStartOffset,
        ?STRICT_DATA_SPLIT_THRESHOLD +
            (BlockStartOffset - (?STRICT_DATA_SPLIT_THRESHOLD - 150000)),
        Leaves,
        300000,
        150000,
        leaf(2)
    ).

synthetic_request(BlockStartOffset, RecallOffset, Leaves, TXSize, Dest, ChunkID) ->
    {DataRoot, DataTree} = ar_merkle:generate_tree(Leaves),
    {TXRoot, TXTree} = ar_merkle:generate_tree([{DataRoot, TXSize}]),
    #{
        <<"device">> => <<"arweave-spora@2.9">>,
        <<"block-start-offset">> => BlockStartOffset,
        <<"block-size">> => TXSize,
        <<"recall-offset">> => RecallOffset,
        <<"tx-root">> => hb_util:encode(TXRoot),
        <<"sub-chunk-index">> => 0,
        <<"expected-chunk-id">> => hb_util:encode(ChunkID),
        <<"packing">> =>
            #{
                <<"format">> => <<"replica-2-9">>,
                <<"reward-addr">> => hb_util:encode(leaf(0)),
                <<"packing-difficulty">> => 10
            },
        <<"poa">> =>
            #{
                <<"tx-path">> =>
                    hb_util:encode(
                        ar_merkle:generate_path(TXRoot, Dest, TXTree)
                    ),
                <<"data-path">> =>
                    hb_util:encode(
                        ar_merkle:generate_path(DataRoot, Dest, DataTree)
                    ),
                <<"chunk">> => <<>>,
                <<"unpacked-chunk">> => <<>>
            }
    }.

leaf(N) -> crypto:hash(sha256, <<"dev_arweave_spora leaf ", N:8>>).

opts() -> #{ <<"store">> => [hb_test_utils:test_store()] }.

rejection(Request, Opts) ->
    {error, Error} = hb_ao:resolve(Request, <<"validate">>, Opts),
    hb_maps:get(<<"message">>, Error, not_found, Opts).
