%%% @doc Vectors for the indexer over a synthetic unpacked storage module.
%%%
%%% Everything here runs against a real data directory of its own: bundles
%%% are built and signed with `ar_bundles', laid into chunk files in the
%%% exact on-disk layout an Arweave node writes (`lib_arweave_chunks:write'),
%%% and scanned back through the full pipeline -- manifest, reader, scan,
%%% runs, merge -- as `lib_arweave_index:run/1' drives it in production.
%%%
%%% The expectations are computed in this module with independent arithmetic:
%%% row bit-packing by integer maths, IDs and predicate hashes by direct
%%% `crypto:hash/2', item placement by summing the serialized bytes laid
%%% down. The scanner's own encoders are never consulted for an expected
%%% value, so a format drift on either side fails the comparison.
%%%
%%% The module sits above the strict data split threshold, as every unpacked
%%% mainnet partition of the present era does, with a small chunk group size
%%% so that chunk-file boundaries are reachable without gigabytes of sparse
%%% file. The weave it holds exercises: multi-item bundles with unicode,
%%% empty and long tags; field targets; a nested bundle; items whose
%%% headers straddle chunk and chunk-file boundaries; a transaction that is
%%% not a bundle; a manifest entry marked not-a-bundle; a hole where a
%%% chunk was never written; and the exclusion intervals -- one at an
%%% item's exact extent (its end pinning the half-open boundary against the
%%% next item), one strictly inside an item's interior, one covering a
%%% nested item -- with a RedStone-tagged item outside every interval,
%%% which is indexed like any other.
-module(lib_arweave_index_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% A chunk file of eight buckets, so that boundaries are reachable.
-define(SMALL_GROUP_SIZE, (8 * ?DATA_CHUNK_SIZE)).

%%% The synthetic module: 16 MiB buckets, placed far above the strict data
%%% split threshold, unpacked.
-define(BUCKET_SIZE, (16 * 1024 * 1024)).
-define(BUCKET, 2000000).

%% @doc The full pipeline over the synthetic weave, once with one worker and
%% once with three: identical merged indexes, exactly the expected row sets.
end_to_end_test_() ->
    {timeout, 240, fun test_end_to_end/0}.

test_end_to_end() ->
    Opts = test_opts(<<"end-to-end">>),
    {Txs, Expected, Placed} = weave(Opts),
    ok =
        lib_arweave_index_manifest:write(
            hb_opts:get(<<"arweave-index-manifest">>, no_path, Opts),
            Txs
        ),
    {ok, Report} = lib_arweave_index:run(Opts#{ <<"arweave-index-workers">> => 3 }),
    {ok, _MergeReport} = lib_arweave_index:merge(Opts),
    assert_items(Opts, Expected),
    assert_counts(Report, Placed),
    assert_containers(Opts),
    % The same weave scanned by a single worker merges to the same bytes.
    Opts1 = Opts#{
        <<"arweave-index-output">> =>
            << (hb_opts:get(<<"arweave-index-output">>, no_dir, Opts))/binary,
                "-single" >>
    },
    {ok, _Report1} = lib_arweave_index:run(Opts1#{ <<"arweave-index-workers">> => 1 }),
    {ok, _MergeReport1} = lib_arweave_index:merge(Opts1),
    ?assertEqual(items(Opts, <<"offset">>), items(Opts1, <<"offset">>)),
    ?assertEqual(items(Opts, <<"match">>), items(Opts1, <<"match">>)).

%% @doc The gateway enrichment against the live network: boundaries derived
%% one way (each transaction's own `/tx/<id>/offset') are assigned the right
%% txids by the other (block-level reconstruction from ids and sizes). Also
%% the on-chain proof of the scan's lattice: every real transaction start
%% sits at `Threshold + k * 262144'.
enrich_live_test_() ->
    {timeout, 120, fun test_enrich_live/0}.

test_enrich_live() ->
    Gateway = <<"https://arweave.net">>,
    {ok, Info} = live_json(Gateway, <<"/info">>),
    Height = hb_util:int(maps:get(<<"height">>, Info)) - 50,
    % Blocks holding only dataless transactions are common; walk back until
    % the ground-truth sample has substance.
    Known = known_block_offsets(Gateway, Height, Height - 10, 8),
    ?assertNotEqual([], Known),
    % The lattice: every start is Threshold + k * 262144.
    lists:foreach(
        fun({Start, _Size, _ID}) ->
            ?assertEqual(
                ar_block:strict_data_split_threshold() rem ?DATA_CHUNK_SIZE,
                Start rem ?DATA_CHUNK_SIZE
            )
        end,
        Known
    ),
    % Enrichment reproduces every txid from block metadata alone.
    Path =
        filename:join(
            os:getenv("TMPDIR", "/tmp"),
            <<
                "hb-enrich-live-",
                (hb_util:encode(crypto:strong_rand_bytes(6)))/binary
            >>
        ),
    ok =
        lib_arweave_index_manifest:write(
            Path,
            [
                #{ <<"start">> => Start, <<"size">> => Size }
            ||
                {Start, Size, _ID} <- Known
            ]
        ),
    {ok, Report} = lib_arweave_index_manifest:enrich(Path, #{}),
    ?assertEqual(length(Known), maps:get(<<"enriched">>, Report)),
    {ok, After} = lib_arweave_index_manifest:load(Path, 0, 1 bsl 62),
    ByStart =
        maps:from_list([{maps:get(<<"start">>, Tx), Tx} || Tx <- After]),
    lists:foreach(
        fun({Start, _Size, ID}) ->
            ?assertEqual(
                ID,
                maps:get(<<"id">>, maps:get(Start, ByStart))
            )
        end,
        Known
    ).

%% @doc Ground-truth placements from descending blocks until enough of
%% their transactions carry data.
known_block_offsets(_Gateway, Height, Floor, _Wanted) when Height =< Floor ->
    [];
known_block_offsets(Gateway, Height, Floor, Wanted) ->
    {ok, Block} =
        live_json(Gateway, << "/block/height/", (hb_util:bin(Height))/binary >>),
    case known_offsets(Gateway, maps:get(<<"txs">>, Block, []), Wanted, []) of
        [] -> known_block_offsets(Gateway, Height - 1, Floor, Wanted);
        Known -> Known
    end.

%% @doc Ground-truth placements for up to `Wanted' of a block's transactions
%% that carry data, from each transaction's own offset endpoint. Most of a
%% block's transactions carry none.
known_offsets(_Gateway, [], _Wanted, Acc) ->
    Acc;
known_offsets(_Gateway, _IDs, 0, Acc) ->
    Acc;
known_offsets(Gateway, [ID | IDs], Wanted, Acc) ->
    % A transaction the gateway serves no offset for -- dataless, or not yet
    % seeded -- is simply not part of the sample.
    case live_json(Gateway, << "/tx/", ID/binary, "/offset" >>) of
        {ok, Offset} ->
            Size = hb_util:int(maps:get(<<"size">>, Offset, 0)),
            End = hb_util:int(maps:get(<<"offset">>, Offset, 0)),
            case Size > 0 of
                true ->
                    known_offsets(
                        Gateway,
                        IDs,
                        Wanted - 1,
                        [{End - Size, Size, hb_util:native_id(ID)} | Acc]
                    );
                false ->
                    known_offsets(Gateway, IDs, Wanted, Acc)
            end;
        {error, _Reason} ->
            known_offsets(Gateway, IDs, Wanted, Acc)
    end.

%% @doc Fetch one JSON resource from the live gateway.
live_json(Gateway, Path) ->
    case hb_http:get(Gateway, Path, #{}) of
        {ok, Res} -> {ok, hb_json:decode(maps:get(<<"body">>, Res))};
        {error, Reason} -> {error, Reason}
    end.

%%% The synthetic weave.

%% @doc Build and place every transaction, returning the manifest specs, the
%% expected rows, and the placement facts the count assertions check.
weave(Opts) ->
    Wallet = ar_wallet:new(),
    Wallet2 = ar_wallet:new(),
    Recipient = crypto:hash(sha256, <<"the recipient wallet">>),
    TxAID = crypto:hash(sha256, <<"txid of bundle A">>),
    % Transaction A: five items, including a RedStone look-alike the
    % exclusion intervals cover, a multi-chunk item, and an item whose
    % header straddles a chunk boundary.
    I1 =
        item(Wallet,
            <<>>,
            [
                {<<"Content-Type">>, <<"text/html">>},
                {<<"App-Name">>, <<"Test-App">>}
            ],
            <<"<h1>hello, weave</h1>">>
        ),
    I2 =
        item(Wallet2,
            Recipient,
            [
                {<<"Unicode-Тег"/utf8>>, <<"значение-\x{1F680}"/utf8>>},
                {<<"Empty">>, <<>>},
                {<<"Long">>, binary:copy(<<"v">>, 3000)}
            ],
            crypto:strong_rand_bytes(1000)
        ),
    I3 =
        item(Wallet,
            <<>>,
            [
                {<<"dataFeedId">>, <<"BTC">>},
                {<<"dataServiceId">>, <<"redstone-primary-prod">>},
                {<<"signerAddress">>, <<"0x926E370Fd53c23f8B71ad2B3217b227E41A92b12">>},
                {<<"timestamp">>, <<"1756080000000">>},
                {<<"type">>, <<"redstone-oracles">>}
            ],
            crypto:strong_rand_bytes(300)
        ),
    % Item four's data is sized so that item five's header begins thirty
    % bytes before a chunk boundary, three or more chunks in, which the
    % placement assertions verify.
    I4Base =
        item(Wallet, <<>>, [{<<"Big">>, <<"item">>}], <<>>),
    PrefixSizes =
        lists:sum([byte_size(ar_bundles:serialize(I)) || I <- [I1, I2, I3]]),
    I4DataStart = 32 + 64 * 5 + PrefixSizes + byte_size(ar_bundles:serialize(I4Base)),
    I4Data =
        boundary_pad(I4DataStart, 3 * ?DATA_CHUNK_SIZE, ?DATA_CHUNK_SIZE, 30),
    I4 =
        item(Wallet, <<>>, [{<<"Big">>, <<"item">>}],
            crypto:strong_rand_bytes(I4Data)),
    I5 =
        item(Wallet2, <<>>, [{<<"After-Boundary">>, <<"yes">>}],
            crypto:strong_rand_bytes(200)),
    A = [I1, I2, I3, I4, I5],
    % Transaction B: a nested bundle. Sub-items S1 and S2 travel inside item
    % P, whose tags name it a bundle.
    S1 =
        item(Wallet,
            <<>>,
            [{<<"Nested-Level">>, <<"1">>}],
            <<"first nested payload">>
        ),
    S2 =
        item(Wallet2,
            Recipient,
            [{<<"Nested-Level">>, <<"1">>}, {<<"Kind">>, <<"second">>}],
            crypto:strong_rand_bytes(400)
        ),
    P =
        item(Wallet,
            <<>>,
            [
                {<<"Bundle-Format">>, <<"binary">>},
                {<<"Bundle-Version">>, <<"2.0.0">>},
                {<<"App-Name">>, <<"Nested-Carrier">>}
            ],
            payload([S1, S2])
        ),
    B = [P],
    % Transaction D: an item header straddling a chunk-file boundary. The
    % transaction begins exactly at a chunk file's own start, and item two's
    % header begins forty bytes before that file's end. Item three carries
    % the full RedStone tag signature but sits outside every exclusion
    % interval, so it is indexed like any other item.
    D1Base = item(Wallet, <<>>, [{<<"Filler">>, <<"d1">>}], <<>>),
    D1DataStart = 32 + 64 * 3 + byte_size(ar_bundles:serialize(D1Base)),
    D1Data = boundary_pad(D1DataStart, 1, ?SMALL_GROUP_SIZE, 40),
    D1 =
        item(Wallet, <<>>, [{<<"Filler">>, <<"d1">>}],
            crypto:strong_rand_bytes(D1Data)),
    D2 =
        item(Wallet2, <<>>, [{<<"Cross-File">>, <<"yes">>}],
            crypto:strong_rand_bytes(100)),
    D3 =
        item(Wallet,
            <<>>,
            [
                {<<"dataFeedId">>, <<"ETH">>},
                {<<"dataServiceId">>, <<"redstone-primary-prod">>},
                {<<"signerAddress">>, <<"0x0">>},
                {<<"timestamp">>, <<"1756080000000">>},
                {<<"type">>, <<"redstone-oracles">>}
            ],
            crypto:strong_rand_bytes(150)),
    D = [D1, D2, D3],
    % Transaction E: three small items with a hole where the module never
    % wrote the second chunk. The first item survives; the two whose headers
    % sit in the hole are lost to it.
    E1 =
        item(Wallet, <<>>, [{<<"Hole">>, <<"before">>}],
            crypto:strong_rand_bytes(?DATA_CHUNK_SIZE div 2)),
    E2 =
        item(Wallet, <<>>, [{<<"Hole">>, <<"inside">>}],
            crypto:strong_rand_bytes(?DATA_CHUNK_SIZE div 2)),
    E3 =
        item(Wallet2, <<>>, [{<<"Hole">>, <<"inside-too">>}], <<"tail">>),
    E = [E1, E2, E3],
    % Placement: each transaction begins at the bucket after its
    % predecessor's data, except D, which is pinned to the next chunk file's
    % own start so that its second item's header crosses a file boundary.
    % C is not a bundle; F is marked skipped in the manifest.
    APayload = payload(A),
    BPayload = payload(B),
    CPayload = crypto:strong_rand_bytes(100000),
    DPayload = payload(D),
    EPayload = payload(E),
    FPayload = crypto:strong_rand_bytes(?DATA_CHUNK_SIZE),
    AStart = next_lattice(range_start()),
    BStart = next_lattice(AStart + byte_size(APayload)),
    CStart = next_lattice(BStart + byte_size(BPayload)),
    DStart = next_file_lattice(CStart + byte_size(CPayload)),
    EStart = next_lattice(DStart + byte_size(DPayload)),
    FStart = next_lattice(EStart + byte_size(EPayload)),
    place(AStart, APayload, Opts),
    place(BStart, BPayload, Opts),
    place(CStart, CPayload, Opts),
    place(DStart, DPayload, Opts),
    place(EStart, EPayload, Opts),
    % The hole: transaction E's second chunk is unwritten after placement.
    hole(EStart + ?DATA_CHUNK_SIZE, Opts),
    place(FStart, FPayload, Opts),
    % The boundary geometry the paddings were solved for really occurred:
    % boundaries sit on the threshold-anchored lattice, so positions are
    % taken relative to the residue.
    [_, _, {I3Start, I3Size}, {I4Start, _}, {I5Start, _}] = offsets(AStart, A),
    ?assertEqual(
        ?DATA_CHUNK_SIZE - 30,
        (I5Start - residue()) rem ?DATA_CHUNK_SIZE
    ),
    ?assert(I5Start - I4Start >= 3 * ?DATA_CHUNK_SIZE),
    [_, {D2Start, _}, _] = offsets(DStart, D),
    ?assertEqual(
        ?SMALL_GROUP_SIZE - 40,
        (D2Start - residue()) rem ?SMALL_GROUP_SIZE
    ),
    Txs =
        [
            #{ <<"start">> => AStart, <<"size">> => byte_size(APayload),
                <<"id">> => TxAID },
            #{ <<"start">> => BStart, <<"size">> => byte_size(BPayload) },
            #{ <<"start">> => CStart, <<"size">> => byte_size(CPayload) },
            #{ <<"start">> => DStart, <<"size">> => byte_size(DPayload),
                <<"bundle">> => true },
            #{ <<"start">> => EStart, <<"size">> => byte_size(EPayload) },
            #{ <<"start">> => FStart, <<"size">> => byte_size(FPayload),
                <<"bundle">> => false }
        ],
    % Expected rows: every indexed item, with its independently computed
    % placement, parentage and predicates. I3 and S1 sit inside exclusion
    % intervals; E3's header sits in the hole (E2's header is still in the
    % chunk before it, though its data is not); C and F yield nothing. A's
    % manifest txid yields no rows: `parent' marks nested containment only,
    % so only S2 carries one.
    PID = crypto:hash(sha256, P#tx.signature),
    PHeader = byte_size(ar_bundles:serialize(P)) - byte_size(P#tx.data),
    SubStart = BStart + 32 + 64 + PHeader,
    [{S1Start, S1Size}, _] = offsets(SubStart, [S1, S2]),
    % The exclusion intervals: I3's exact extent, whose end -- the very
    % offset I4 begins at -- pins the half-open boundary; a strict interior
    % of I4, which does not catch an item that merely overlaps it; and the
    % nested S1's extent inside P's payload.
    ok =
        file:write_file(
            hb_opts:get(<<"arweave-index-exclusions">>, no_path, Opts),
            <<
                I3Start:64, (I3Start + I3Size):64,
                (I4Start + 100):64, (I4Start + 200):64,
                S1Start:64, (S1Start + S1Size):64
            >>
        ),
    [{E3Start, _}] = lists:nthtail(2, offsets(EStart, E)),
    ?assert(E3Start > EStart + ?DATA_CHUNK_SIZE),
    ?assert(E3Start < EStart + 2 * ?DATA_CHUNK_SIZE),
    Expected =
        lists:append(
            [
                expected(AStart, A, undefined, [3]),
                expected(BStart, B, undefined, []),
                expected(SubStart, [S1, S2], hb_util:encode(PID), [1]),
                expected(DStart, D, undefined, []),
                expected(EStart, [E1, E2, E3], undefined, [3])
            ]
        ),
    Placed =
        #{
            <<"items">> => 11,
            <<"excluded">> => 2,
            <<"bytes-skipped">> => I3Size + S1Size,
            <<"in-holes">> => 1,
            <<"nested">> => 1,
            <<"not-bundle">> => 1,
            <<"skipped">> => 1
        },
    {Txs, Expected, Placed}.

%%% Expectation helpers: independent arithmetic only.

%% @doc The expected rows of a run of items placed from `Start', excluding
%% the 1-based positions in `Excluded' (RedStone or lost to holes).
expected(Start, Items, Parent, Excluded) ->
    Placements = offsets(Start, Items),
    [
        expected_item(Item, Offset, Size, Parent)
    ||
        {N, {Item, {Offset, Size}}} <-
            lists:enumerate(lists:zip(Items, Placements)),
        not lists:member(N, Excluded)
    ].

%% @doc One item's expected offset row and match rows, packed by integer
%% maths on independently hashed inputs. The codec-consumed
%% `bundle-format'/`bundle-version' tags yield no predicates.
expected_item(Item, Offset, Size, Parent) ->
    ID = crypto:hash(sha256, Item#tx.signature),
    OffsetRow =
        <<
            (binary:part(ID, 0, 10))/binary,
            (2 * (1 bsl 84) + Offset * (1 bsl 34) + Size):88
        >>,
    Committer = hb_util:encode(crypto:hash(sha256, Item#tx.owner)),
    Predicates =
        [
            <<
                "~match@1.0/",
                (string:lowercase(Name))/binary,
                "=",
                Value/binary
            >>
        ||
            {Name, Value} <- Item#tx.tags,
            not lists:member(
                string:lowercase(Name),
                [<<"bundle-format">>, <<"bundle-version">>]
            )
        ]
        ++ [<<"~match@1.0/commitment-device=ans104@1.0">>]
        ++ [<<"~match@1.0/committer=", Committer/binary>>]
        ++
            case Item#tx.target of
                <<>> -> [];
                Target ->
                    [<<"~match@1.0/field-target=",
                        (hb_util:encode(Target))/binary>>]
            end
        ++
            case Parent of
                undefined -> [];
                ParentID -> [<<"~match@1.0/parent=", ParentID/binary>>]
            end,
    {
        OffsetRow,
        [
            <<
                (binary:part(crypto:hash(sha256, Predicate), 0, 10))/binary,
                (Offset * (1 bsl 7)):56
            >>
        ||
            Predicate <- Predicates
        ]
    }.

%% @doc Where each of a payload's items lands: absolute offset and size, by
%% summing the serialized bytes.
offsets(Start, Items) ->
    Bins = [ar_bundles:serialize(Item) || Item <- Items],
    First = Start + 32 + 64 * length(Items),
    {_End, Placements} =
        lists:foldl(
            fun(Bin, {Pos, Acc}) ->
                {Pos + byte_size(Bin), [{Pos, byte_size(Bin)} | Acc]}
            end,
            {First, []},
            Bins
        ),
    lists:reverse(Placements).

%% @doc Compare the merged item files to the expected rows exactly.
assert_items(Opts, Expected) ->
    ExpectedOffset = lists:usort([Row || {Row, _Match} <- Expected]),
    ExpectedMatch =
        lists:usort(lists:append([Match || {_Row, Match} <- Expected])),
    ?assertEqual(ExpectedOffset, items(Opts, <<"offset">>)),
    ?assertEqual(ExpectedMatch, items(Opts, <<"match">>)).

%% @doc The merged items of one kind, as a list of fixed-width binaries.
items(Opts, Kind) ->
    Out = hb_opts:get(<<"arweave-index-output">>, no_dir, Opts),
    Width = lib_arweave_index_runs:item_size(Kind),
    {ok, Bin} =
        file:read_file(filename:join(Out, << Kind/binary, ".items" >>)),
    [Item || << Item:Width/binary >> <= Bin].

%% @doc The published containers round-trip: built from the merged item
%% files by sorted appends, they carry the LMDB 1.0 meta page the format
%% specifies and read back as exactly the items that went in. When the
%% linked `elmdb' does not carry the append API, the build must say so
%% rather than produce something else.
assert_containers(Opts) ->
    Out = hb_opts:get(<<"arweave-index-output">>, no_dir, Opts),
    _Loaded = code:ensure_loaded(elmdb),
    case erlang:function_exported(elmdb, put_batch_append, 2) of
        false ->
            ?assertEqual(
                {error, <<"elmdb-append-unavailable">>},
                lib_arweave_index_runs:container(
                    <<"offset">>,
                    filename:join(Out, <<"offset.items">>),
                    filename:join(Out, <<"offset.db">>)
                )
            );
        true ->
            lists:foreach(
                fun(Kind) -> assert_container(Kind, Out) end,
                [<<"offset">>, <<"match">>]
            )
    end.

assert_container(Kind, Out) ->
    ItemsPath = filename:join(Out, << Kind/binary, ".items" >>),
    DBPath = filename:join(Out, << Kind/binary, ".db" >>),
    ok = lib_arweave_index_runs:container(Kind, ItemsPath, DBPath),
    Width = lib_arweave_index_runs:item_size(Kind),
    {ok, Items} = file:read_file(ItemsPath),
    % The container is one published file of whole 64 KiB pages, whose
    % first meta page carries the LMDB magic and data version 3.
    Size = filelib:file_size(DBPath),
    ?assert(Size > 0 andalso Size rem 65536 == 0),
    {ok, DBFile} = file:open(DBPath, [read, raw, binary]),
    {ok, Meta} = file:pread(DBFile, 0, 32),
    ok = file:close(DBFile),
    << _:24/binary, Magic:32/little, Version:32/little >> = Meta,
    ?assertEqual({16#BEEFC0DE, 3}, {Magic, Version}),
    % Every item reads back, ascending, through the positioned dup read.
    {ok, Env} =
        elmdb:env_open(DBPath, [{page_size, 65536}, read_only, no_subdir]),
    {ok, DB} = elmdb:db_open(Env, [dupsort, dupfixed]),
    {ok, Read} = elmdb:read_dups(DB, << 0 >>, [{limit, 0}]),
    ok = elmdb:env_close(Env),
    ?assertEqual(
        [Item || << Item:Width/binary >> <= Items],
        Read
    ).

%% @doc The scan's counters match the weave that was placed.
assert_counts(Report, Placed) ->
    Counts = maps:get(<<"counts">>, Report),
    ?assertEqual(maps:get(<<"items">>, Placed), maps:get(<<"items">>, Counts)),
    ?assertEqual(
        maps:get(<<"excluded">>, Placed),
        maps:get(<<"items-excluded-intervals">>, Counts)
    ),
    ?assertEqual(
        maps:get(<<"bytes-skipped">>, Placed),
        maps:get(<<"bytes-skipped">>, Counts)
    ),
    ?assertEqual(
        maps:get(<<"in-holes">>, Placed),
        maps:get(<<"items-in-holes">>, Counts)
    ),
    ?assertEqual(
        maps:get(<<"nested">>, Placed),
        maps:get(<<"bundles-nested">>, Counts)
    ),
    ?assertEqual(
        maps:get(<<"not-bundle">>, Placed),
        maps:get(<<"txs-not-bundle">>, Counts)
    ),
    ?assertEqual(
        maps:get(<<"skipped">>, Placed),
        maps:get(<<"txs-skipped">>, Counts)
    ),
    ?assertEqual(5, maps:get(<<"txs">>, Counts)).

%%% Weave-building helpers.

%% @doc A signed data item.
item(Wallet, Target, Tags, Data) ->
    ar_bundles:sign_item(ar_bundles:new_item(Target, <<>>, Tags, Data), Wallet).

%% @doc A bundle payload: the item-count table, then the items. Built here
%% by hand -- table arithmetic included -- rather than through the code under
%% test.
payload(Items) ->
    Bins = [ar_bundles:serialize(Item) || Item <- Items],
    iolist_to_binary(
        [
            << (length(Items)):256/little >>,
            [
                <<
                    (byte_size(Bin)):256/little,
                    (crypto:hash(sha256, Item#tx.signature))/binary
                >>
            ||
                {Item, Bin} <- lists:zip(Items, Bins)
            ],
            Bins
        ]
    ).

%% @doc The data size that places the next item's header `Margin' bytes
%% before an `Align'-multiple boundary at least `Floor' bytes past the data's
%% own start at `Used' bytes into the payload.
boundary_pad(Used, Floor, Align, Margin) ->
    hb_util:ceil_int(Used + Floor + Margin, Align) - Margin - Used.

%% @doc Write one payload into the module's chunk files from its lattice-
%% aligned start, zero-padding the final chunk as the layout demands.
place(Start, Payload, Opts) ->
    true = Start rem ?DATA_CHUNK_SIZE == residue(),
    chunks(Start, Payload, Opts).

chunks(_Offset, <<>>, _Opts) ->
    ok;
chunks(Offset, << Chunk:?DATA_CHUNK_SIZE/binary, Rest/binary >>, Opts) ->
    ok =
        lib_arweave_chunks:write(
            module(), Offset + ?DATA_CHUNK_SIZE, Chunk, Opts),
    chunks(Offset + ?DATA_CHUNK_SIZE, Rest, Opts);
chunks(Offset, Tail, Opts) ->
    Padded =
        << Tail/binary, 0:((?DATA_CHUNK_SIZE - byte_size(Tail)) * 8) >>,
    ok =
        lib_arweave_chunks:write(
            module(), Offset + ?DATA_CHUNK_SIZE, Padded, Opts),
    ok.

%% @doc Unwrite the chunk whose bucket begins at the given offset, leaving
%% the zero prefix that means `never written'.
hole(BucketStart, Opts) ->
    {_FileStart, Path, Position, _ChunkOffset} =
        lib_arweave_chunks:locate(module(), BucketStart + ?DATA_CHUNK_SIZE, Opts),
    {ok, File} = file:open(Path, [read, write, raw, binary]),
    ok = file:pwrite(File, Position, << 0:((3 + ?DATA_CHUNK_SIZE) * 8) >>),
    ok = file:close(File).

%% @doc The offset of the padded lattice within the absolute buckets: real
%% transactions above the strict data split threshold begin at
%% `Threshold + k * 262144'.
residue() ->
    ar_block:strict_data_split_threshold() rem ?DATA_CHUNK_SIZE.

%% @doc The first lattice point at or after an offset.
next_lattice(Offset) ->
    residue() + hb_util:ceil_int(Offset - residue(), ?DATA_CHUNK_SIZE).

%% @doc The first lattice point at or after an offset whose slot begins a
%% chunk file of its own.
next_file_lattice(Offset) ->
    residue() + hb_util:ceil_int(Offset - residue(), ?SMALL_GROUP_SIZE).

%% @doc The synthetic module and its geometry.
module() ->
    {?BUCKET_SIZE, ?BUCKET, unpacked}.

range_start() ->
    ?BUCKET_SIZE * ?BUCKET.

%% @doc A test's own data directory, manifest and output paths.
test_opts(Tag) ->
    Base =
        hb_util:bin(
            filename:join([
                os:getenv("TMPDIR", "/tmp"),
                "hb-arweave-index",
                <<
                    Tag/binary,
                    "-",
                    (hb_util:encode(crypto:strong_rand_bytes(6)))/binary
                >>
            ])
        ),
    #{
        <<"arweave-data-dir">> => << Base/binary, "/data" >>,
        <<"arweave-chunk-group-size">> => ?SMALL_GROUP_SIZE,
        <<"arweave-index-module">> => lib_arweave_storage:id(module()),
        <<"arweave-index-manifest">> => << Base/binary, "/manifest.aimf" >>,
        <<"arweave-index-exclusions">> => << Base/binary, "/exclusions.bin" >>,
        <<"arweave-index-output">> => << Base/binary, "/out" >>,
        <<"arweave-index-run-rows">> => 7
    }.
