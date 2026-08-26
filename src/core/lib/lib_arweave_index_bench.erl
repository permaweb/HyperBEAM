%%% @doc Throughput measurement for the index scanner, over synthetic
%%% modules it generates itself.
%%%
%%% `generate/1' lays a weave of structurally valid ANS-104 bundles into a
%%% real unpacked storage module and writes its manifest; `scan/1' runs the
%%% production pipeline over it and reports the throughput figures beside
%%% the run report. Items are fabricated rather than signed -- the scanner
%%% never verifies signatures, and random signature bytes give every item a
%%% distinct ID -- so generating gigabytes takes seconds, not hours.
%%%
%%% Profiles set the item mix, bracketing the parser's envelope:
%%%
%%% <ul>
%%%   <li>`dense': every payload byte is a ~2 KiB RedStone-shaped item --
%%%       maximum headers per byte, the parse-bound worst case.</li>
%%%   <li>`mixed': by bytes, roughly 30% small RedStone-shaped items, 30%
%%%       mid-size tagged items, 40% multi-megabyte items.</li>
%%%   <li>`media': a few large items per bundle -- the read-and-skip best
%%%       case.</li>
%%% </ul>
%%%
%%% For example:
%%%
%%% ```
%%% Opts = lib_arweave_index_bench:generate(#{
%%%     <<"dir">> => <<"/tmp/hb-bench">>,
%%%     <<"bytes">> => 2147483648,
%%%     <<"profile">> => <<"dense">>
%%% }),
%%% lib_arweave_index_bench:scan(Opts#{ <<"arweave-index-workers">> => 4 }).
%%% '''
-module(lib_arweave_index_bench).
-export([generate/1, scan/1]).
-include("include/hb.hrl").

%%% The bucket the synthetic module occupies: above the strict data split
%%% threshold, in mainnet-partition position.
-define(BENCH_BUCKET, 106).

%% @doc Build a module of the requested size and profile, returning the
%% options `scan/1' consumes.
generate(Spec) ->
    Dir = maps:get(<<"dir">>, Spec, <<"/tmp/hb-index-bench">>),
    Bytes = maps:get(<<"bytes">>, Spec, 1073741824),
    Profile = maps:get(<<"profile">>, Spec, <<"mixed">>),
    Opts =
        #{
            <<"arweave-data-dir">> => << Dir/binary, "/data" >>,
            <<"arweave-index-module">> =>
                lib_arweave_storage:id(module()),
            <<"arweave-index-manifest">> => << Dir/binary, "/manifest.aimf" >>,
            <<"arweave-index-output">> => << Dir/binary, "/out" >>
        },
    % The weave's chunk lattice is global and threshold-anchored: a
    % partition's first transaction begins at the first lattice point at or
    % after the partition start.
    {RangeStart, _RangeEnd} = lib_arweave_storage:range(module()),
    From = next_lattice(RangeStart),
    Started = erlang:monotonic_time(millisecond),
    Txs = lay(From, From + Bytes, Profile, Opts, []),
    ok =
        lib_arweave_index_manifest:write(
            hb_opts:get(<<"arweave-index-manifest">>, no_path, Opts),
            Txs
        ),
    ?event(arweave_index,
        {bench_generated,
            {bytes, Bytes},
            {profile, Profile},
            {txs, length(Txs)},
            {wall_ms, erlang:monotonic_time(millisecond) - Started}
        }
    ),
    Last = lists:last(Txs),
    Opts#{
        <<"arweave-index-from">> => From,
        <<"arweave-index-to">> =>
            maps:get(<<"start">>, Last) + maps:get(<<"size">>, Last)
    }.

%% @doc Run the pipeline over a generated module and print the figures.
scan(Opts) ->
    {ok, Report} = lib_arweave_index:run(Opts),
    #{
        <<"wall-ms">> := Wall,
        <<"bytes-read">> := BytesRead,
        <<"weave-bytes">> := Weave,
        <<"rows">> := Rows
    } = Report,
    Summary =
        #{
            <<"wall-ms">> => Wall,
            <<"read-gbps">> => maps:get(<<"read-gbps">>, Report),
            <<"weave-gbps">> => maps:get(<<"weave-gbps">>, Report),
            <<"bytes-read">> => BytesRead,
            <<"weave-bytes">> => Weave,
            <<"rows-per-second">> =>
                case Wall of
                    0 -> 0;
                    _ ->
                        1000 * (maps:get(<<"offset">>, Rows, 0)
                            + maps:get(<<"match">>, Rows, 0)) div Wall
                end,
            <<"counts">> => maps:get(<<"counts">>, Report),
            <<"rows">> => Rows
        },
    ?event(arweave_index, {bench_scan, Summary}),
    {ok, Summary, Report}.

%%% Internal functions.

%% @doc Lay bundles bucket by bucket until the range is filled.
lay(Pos, End, _Profile, _Opts, Txs) when Pos >= End ->
    lists:reverse(Txs);
lay(Pos, End, Profile, Opts, Txs) ->
    Payload = bundle(Profile, min(End - Pos, 134217728)),
    ok = place(Pos, Payload, Opts),
    Tx = #{ <<"start">> => Pos, <<"size">> => byte_size(Payload) },
    lay(next_lattice(Pos + byte_size(Payload)), End, Profile, Opts, [Tx | Txs]).

%% @doc One bundle payload of roughly the target size.
bundle(Profile, Target) ->
    Items = items(Profile, Target, []),
    Table =
        [
            << (byte_size(Item)):256/little,
                (crypto:strong_rand_bytes(32))/binary >>
        ||
            Item <- Items
        ],
    iolist_to_binary(
        [<< (length(Items)):256/little >>, Table, Items]
    ).

%% @doc Fabricated items until the target bytes are covered.
items(_Profile, Target, Items) when Target =< 0 ->
    Items;
items(Profile, Target, Items) ->
    Item = item(Profile),
    items(Profile, Target - byte_size(Item) - 64, [Item | Items]).

%% @doc One fabricated item of the profile's mix. Dense keeps the measured
%% mainnet ratio: 86% RedStone-shaped, the rest small tagged items, so the
%% row-emitting path runs at its real per-byte rate.
item(<<"dense">>) ->
    case rand:uniform(100) of
        Roll when Roll =< 86 -> redstone_item();
        _Roll -> tagged_item(1024 + rand:uniform(4096))
    end;
item(<<"media">>) ->
    tagged_item(2097152 + rand:uniform(6291456));
item(<<"mixed">>) ->
    case rand:uniform(100) of
        Roll when Roll =< 70 -> redstone_item();
        Roll when Roll =< 95 -> tagged_item(2048 + rand:uniform(49152));
        _Roll -> tagged_item(1048576 + rand:uniform(7340032))
    end.

%% @doc A RedStone-shaped item: an ethereum signature envelope, the five
%% marker tags, and a couple of kilobytes of payload.
redstone_item() ->
    fabricate(
        << 3, 0 >>,
        65,
        65,
        [
            {<<"dataFeedId">>, <<"ETH">>},
            {<<"dataServiceId">>, <<"redstone-primary-prod">>},
            {<<"signerAddress">>,
                <<"0x926E370Fd53c23f8B71ad2B3217b227E41A92b12">>},
            {<<"timestamp">>, integer_to_binary(rand:uniform(1 bsl 40))},
            {<<"type">>, <<"redstone-oracles">>}
        ],
        1024 + rand:uniform(2048)
    ).

%% @doc An RSA-enveloped item with a typical tag set.
tagged_item(DataSize) ->
    fabricate(
        << 1, 0 >>,
        512,
        512,
        [
            {<<"Content-Type">>, <<"application/octet-stream">>},
            {<<"App-Name">>, <<"Bench-App">>},
            {<<"App-Version">>, <<"1.0.3">>},
            {<<"Unix-Time">>, integer_to_binary(rand:uniform(1 bsl 31))},
            {<<"Nonce">>, hb_util:encode(crypto:strong_rand_bytes(12))},
            {<<"Data-Protocol">>, <<"bench">>}
        ],
        DataSize
    ).

%% @doc A structurally valid item: real layout, random signature and owner.
fabricate(SigType, SigSize, OwnerSize, Tags, DataSize) ->
    Encoded = ar_bundles:encode_tags(Tags),
    iolist_to_binary(
        [
            SigType,
            crypto:strong_rand_bytes(SigSize),
            crypto:strong_rand_bytes(OwnerSize),
            << 0 >>,
            << 0 >>,
            << (length(Tags)):64/little, (byte_size(Encoded)):64/little >>,
            Encoded,
            filler(DataSize)
        ]
    ).

%% @doc Cheap payload bytes: one random block, repeated.
filler(Size) ->
    Block = binary:copy(crypto:strong_rand_bytes(1024), 64),
    Whole = binary:copy(Block, Size div byte_size(Block)),
    << Whole/binary, (binary:part(Block, 0, Size rem byte_size(Block)))/binary >>.

%% @doc Write one payload's chunks from its bucket-aligned start.
place(Start, Payload, Opts) ->
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
    lib_arweave_chunks:write(module(), Offset + ?DATA_CHUNK_SIZE, Padded, Opts).

%% @doc The first threshold-anchored lattice point at or after an offset.
next_lattice(Offset) ->
    Residue = ar_block:strict_data_split_threshold() rem ?DATA_CHUNK_SIZE,
    Residue + hb_util:ceil_int(Offset - Residue, ?DATA_CHUNK_SIZE).

%% @doc The synthetic module: one mainnet-sized unpacked partition.
module() ->
    {ar_block:partition_size(), ?BENCH_BUCKET, unpacked}.
