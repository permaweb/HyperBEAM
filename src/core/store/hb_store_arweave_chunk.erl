%%% @doc A store of the weave chunks that a node reads from Arweave nodes, so
%%% that a read of bytes in a chunk that the node has read before does not
%%% download the chunk again. `arweave-chunk-store' selects the store: by
%%% default a volatile table that keeps each chunk for five minutes, while
%%% `[]' keeps no chunks. Each chunk is held at `~arweave@2.9/chunk=END', the
%%% path of the request that gives it: its compact hashpath (`hb_hashpath') is
%%% `ao:~arweave@2.9/chunk=END'. `END' is the weave offset of the chunk's last
%%% byte, zero-padded to 20 digits so that the order of the keys is the order
%%% of the offsets. A read of an offset lists the `~arweave@2.9' group from
%%% `chunk=OFFSET' with a limit of one: the first chunk that ends at or after
%%% the offset holds it if the chunk starts at or before it.
-module(hb_store_arweave_chunk).
-export([read/2, write/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc The group of chunks: the `~arweave@2.9' device, whose `chunk' key
%% gives each of them.
-define(CHUNKS, <<"~arweave@2.9">>).

%% @doc Read the chunk that holds the byte at a weave offset, as the offsets
%% of its first and last bytes and its data.
read(Offset, Opts) ->
    Store = hb_opts:get(arweave_chunk_store, [], Opts),
    maybe
        {ok, [Key = <<"chunk=", Digits/binary>>]} ?=
            hb_store:list(
                Store,
                #{
                    <<"list">> => ?CHUNKS,
                    <<"from">> => key(Offset),
                    <<"limit">> => 1
                },
                Opts
            ),
        {ok, Chunk} ?=
            hb_store:read(Store, <<?CHUNKS/binary, "/", Key/binary>>, Opts),
        End = hb_util:int(Digits),
        Start = End - byte_size(Chunk) + 1,
        true ?= Start =< Offset orelse {error, not_found},
        {ok, {Start, End, Chunk}}
    else
        {ok, _} -> {error, not_found};
        {error, _} = Error -> Error
    end.

%% @doc Write a chunk at the weave offset of its last byte.
write(End, Chunk, Opts) ->
    hb_store:write(
        hb_opts:get(arweave_chunk_store, [], Opts),
        #{ <<?CHUNKS/binary, "/", (key(End))/binary>> => Chunk },
        Opts
    ).

%% @doc The `chunk' key of a weave offset, zero-padded to 20 digits as block
%% index and match index offsets are.
key(Offset) ->
    iolist_to_binary(io_lib:format("chunk=~20..0B", [Offset])).

%%% Tests

%% @doc Keys sort as their offsets do, including across a change in the
%% number of digits.
key_test() ->
    ?assertEqual(<<"chunk=00000000000000000001">>, key(1)),
    ?assertEqual(<<"chunk=00000378092137521399">>, key(378092137521399)),
    ?assert(key(9) < key(10)).

%% @doc A read finds the chunk that holds an offset at the chunk's last byte,
%% inside it, and at its first byte, and finds no chunk for an offset between
%% the chunks that the store holds or after them.
read_test_() ->
    [
        {hb_util:list(Module), fun() -> read_offsets(Module) end}
    ||
        Module <- [hb_store_volatile, hb_store_lmdb]
    ].

read_offsets(Module) ->
    Opts = #{ <<"arweave-chunk-store">> => hb_test_utils:test_store(Module) },
    A = binary:copy(<<"a">>, 100),
    B = binary:copy(<<"b">>, 150),
    C = binary:copy(<<"c">>, 100),
    ok = write(100, A, Opts),
    ok = write(250, B, Opts),
    ok = write(600, C, Opts),
    % The last byte of a chunk, two adjacent bytes inside it, and the byte
    % after its last, which is the first byte of the next chunk.
    ?assertEqual({ok, {1, 100, A}}, read(100, Opts)),
    ?assertEqual({ok, {1, 100, A}}, read(50, Opts)),
    ?assertEqual({ok, {1, 100, A}}, read(51, Opts)),
    ?assertEqual({ok, {101, 250, B}}, read(101, Opts)),
    % A byte between the chunks that the store holds, and one after them.
    ?assertEqual({error, not_found}, read(300, Opts)),
    ?assertEqual({error, not_found}, read(601, Opts)),
    % A node with no chunk store holds no chunks.
    ?assertEqual(
        {error, not_found},
        read(100, #{ <<"arweave-chunk-store">> => [] })
    ).

%% @doc An empty store misses an offset inside a chunk, downloads the chunk
%% once and keeps it at the chunk's last byte. The next offset, and both
%% offsets together, then read from that entry with no download and no new
%% entry. A read of the whole transaction downloads only the four chunks that
%% the store lacks, leaving one entry per chunk at its last byte, after which
%% no read inside it downloads anything. A node without the store downloads
%% the chunk for each read.
stored_range_test_() ->
    [
        {hb_util:list(Module), fun() -> stored_range(Module) end}
    ||
        Module <- [hb_store_volatile, hb_store_lmdb]
    ].

stored_range(Module) ->
    %% https://arweave.net/tx/QL7_EnmrFtx-0wVgPr2IwaGWQT8vmPcF3R20CKMO3D4/offset
    Offset = 378092137521399,
    Length = 1264430,
    DataRoot = <<"aHoisEr0IJ5xV2i0NjnE-vbtZOzZqI01ecyn15tojbE">>,
    Store = hb_test_utils:test_store(Module),
    Opts = (receipt_opts())#{ <<"arweave-chunk-store">> => Store },
    Read =
        fun(From, Size, ReadOpts) ->
            hb_ao:resolve(
                #{ <<"device">> => <<"arweave@2.9">> },
                #{
                    <<"path">> => <<"chunk">>,
                    <<"offset">> => From,
                    <<"length">> => Size
                },
                ReadOpts
            )
        end,
    Ends =
        fun() ->
            {ok, Keys} = hb_store:list(Store, ?CHUNKS, #{}),
            lists:sort(Keys)
        end,
    FirstEnd = key(Offset + ?DATA_CHUNK_SIZE - 1),
    % Offset X misses, and its chunk is downloaded once and kept at the
    % chunk's last byte.
    {ok, <<X>>} = Read(Offset + 1000, 1, Opts),
    ?assertEqual(1, downloads()),
    ?assertEqual([FirstEnd], Ends()),
    % X+1, then X and X+1 together, read from that entry.
    {ok, <<Y>>} = Read(Offset + 1001, 1, Opts),
    ?assertEqual({ok, <<X, Y>>}, Read(Offset + 1000, 2, Opts)),
    ?assertEqual(0, downloads()),
    ?assertEqual([FirstEnd], Ends()),
    % Without the store, a read downloads its chunk.
    ?assertEqual(
        {ok, <<Y>>},
        Read(Offset + 1001, 1, Opts#{ <<"arweave-chunk-store">> => [] })
    ),
    ?assertEqual(1, downloads()),
    % The whole transaction downloads its other four chunks, three of 256 KiB
    % and one of 215,854 bytes, and keeps each at its last byte.
    {ok, Data} = Read(Offset, Length, Opts),
    ?assertEqual(4, downloads()),
    ?assertEqual(DataRoot, hb_util:encode(ar_tx:data_root(Data))),
    ?assertEqual(<<X, Y>>, binary:part(Data, 1000, 2)),
    ?assertEqual(
        [ key(Offset + N * ?DATA_CHUNK_SIZE - 1) || N <- lists:seq(1, 4) ] ++
            [key(Offset + Length - 1)],
        Ends()
    ),
    % Every byte after the first, and the bytes either side of the first
    % chunk's end, read from the store alone.
    ?assertEqual(
        {ok, binary:part(Data, 1, Length - 1)},
        Read(Offset + 1, Length - 1, Opts)
    ),
    ?assertEqual(
        {ok, binary:part(Data, ?DATA_CHUNK_SIZE - 1, 2)},
        Read(Offset + ?DATA_CHUNK_SIZE - 1, 2, Opts)
    ),
    ?assertEqual(0, downloads()).

%% @doc The chunk that `~arweave@2.9/chunk=END' gives is held in the store at
%% that path, which is the request's compact hashpath without its `ao:'
%% scheme. The path, and a range read of the same bytes, then read from that
%% entry with no download.
chunk_path_test_() ->
    {timeout, 120, fun chunk_path/0}.

chunk_path() ->
    % The first chunk of the transaction that `stored_range/1' reads.
    Start = 378092137521399,
    Path = <<"~arweave@2.9/chunk=00000378092137783542">>,
    Store = hb_test_utils:test_store(hb_store_volatile),
    Opts = (receipt_opts())#{ <<"arweave-chunk-store">> => Store },
    [Base, Req] = hb_singleton:from(#{ <<"path">> => Path }, Opts),
    ?assertEqual(
        <<"ao:", Path/binary>>,
        hb_hashpath:format(#{ <<"base">> => Base, <<"request">> => Req }, Opts)
    ),
    {ok, Chunk} = hb_ao:resolve(Path, Opts),
    ?assertEqual(1, downloads()),
    ?assertEqual(?DATA_CHUNK_SIZE, byte_size(Chunk)),
    ?assertEqual({ok, Chunk}, hb_store:read(Store, Path, Opts)),
    ?assertEqual({ok, Chunk}, hb_ao:resolve(Path, Opts)),
    ?assertEqual(
        {ok, Chunk},
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{
                <<"path">> => <<"chunk">>,
                <<"offset">> => Start,
                <<"length">> => ?DATA_CHUNK_SIZE
            },
            Opts
        )
    ),
    ?assertEqual(0, downloads()).

%% @doc The offsets of two IDs that are adjacent in the published offset
%% index, looked up through its default definition: with the default
%% options, the leaf chunk that the first lookup downloads answers the
%% second with no download. With no chunk store, the second lookup downloads
%% again.
offset_index_test_() ->
    [
        {Desc, {timeout, 120, fun() -> offset_index(NodeOpts) end}}
    ||
        {Desc, NodeOpts} <-
            [
                {"default options", #{}},
                {"no chunk store", #{ <<"arweave-chunk-store">> => [] }}
            ]
    ].

offset_index(NodeOpts) ->
    Index = published_offset_index(),
    Opts = maps:merge(receipt_opts(), NodeOpts),
    empty_chunk_store(Opts),
    Lookup =
        fun(ID) ->
            hb_store:read([Index], <<"~arweave@2.9/offset=", ID/binary>>, Opts)
        end,
    ?assertMatch(
        {ok, #{ <<"start">> := 381852134215637, <<"length">> := 3947 }},
        Lookup(<<"AAAAhyV8_NwududSxuraAj7DLWiZHDTqVKWrZglpNok">>)
    ),
    First = downloads(),
    ?assertMatch(
        {ok, #{ <<"start">> := 189811668003858, <<"length">> := 22938 }},
        Lookup(<<"AAAAh8mf_LHy_pkEoPL0pedAkw0S6PDbPZ3Nu2WBLc0">>)
    ),
    Second = downloads(),
    ?assert(First > 0),
    case NodeOpts of
        #{ <<"arweave-chunk-store">> := [] } -> ?assert(Second > 0);
        _ -> ?assertEqual(0, Second)
    end.

%% @doc Two items next to each other in the weave, read by ID through the
%% published offset index, as GraphQL reads the IDs it matches, and then at
%% the first item's offset, with the default options: once the chunk that
%% holds the first item is downloaded, the second item, and the first again,
%% read with no download.
adjacent_items_test_() ->
    {timeout, 120, fun adjacent_items/0}.

adjacent_items() ->
    First = <<"AAAAhyV8_NwududSxuraAj7DLWiZHDTqVKWrZglpNok">>,
    Next = <<"jjlrZfiS_WGVxvp-AhpypEhc1o7zpFr3rfBhgse9zkE">>,
    Index = published_offset_index(),
    Arweave =
        #{
            <<"store-module">> => hb_store_arweave,
            <<"name">> => <<"chunk-cache-adjacent-items">>,
            <<"index-store">> => [Index]
        },
    Opts = receipt_opts(),
    empty_chunk_store(Opts),
    Offset =
        fun(ID) ->
            hb_store:read([Index], <<"~arweave@2.9/offset=", ID/binary>>, Opts)
        end,
    % The second item begins where the first ends. The offsets are looked up
    % first, so that the downloads after them are the items' own.
    {ok, #{ <<"start">> := Start, <<"length">> := 3947 }} = Offset(First),
    NextStart = Start + 3947,
    ?assertMatch({ok, #{ <<"start">> := NextStart }}, Offset(Next)),
    downloads(),
    ?assertMatch({ok, _}, hb_store:read([Arweave], First, Opts)),
    ?assertEqual(1, downloads()),
    ?assertMatch({ok, _}, hb_store:read([Arweave], Next, Opts)),
    ?assertMatch({ok, _}, hb_store:read([Arweave], First, Opts)),
    ?assertEqual(0, downloads()),
    % The first item at its offset, with its data length: 3,947 bytes less
    % its 1,120-byte header.
    {ok, Item} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            <<(hb_util:bin(Start))/binary, "-2827">>,
            Opts
        ),
    ?assertEqual(First, hb_message:id(Item, signed, Opts)),
    ?assertEqual(0, downloads()).

%% @doc The default definition of the published offset index, under a fresh
%% name, so that its start is the calling test's own.
published_offset_index() ->
    #{ <<"index-store">> := IndexStores } =
        hb_store_arweave:store_from_opts(#{}),
    [Default] =
        [
            Store
        ||
            Store = #{ <<"name">> := <<"published-offset-index">> } <-
                IndexStores
        ],
    Default#{
        <<"name">> =>
            <<
                "published-offset-index-",
                (hb_util:encode(crypto:strong_rand_bytes(4)))/binary
            >>
    }.

%% @doc Stop the node's chunk store, so that a test's reads begin with an
%% empty table.
empty_chunk_store(Opts) ->
    ok = hb_store:stop(hb_opts:get(arweave_chunk_store, [], Opts), Opts).

%% @doc Node options whose `http-client-receipt' hook sends the path and
%% status class of each outbound request attempt to the calling process.
receipt_opts() ->
    Self = self(),
    Receipt =
        fun(_, Req, _) ->
            Self !
                {
                    receipt,
                    hb_ao:get(<<"body/request-path">>, Req, #{}),
                    hb_ao:get(<<"body/status-class">>, Req, #{})
                },
            {ok, Req}
        end,
    #{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"on">> =>
            #{
                <<"http-client-receipt">> =>
                    #{ <<"device">> => #{ http_client_receipt => Receipt } }
            }
    }.

%% @doc The chunks downloaded since the last call: `GET /chunk' requests that
%% succeeded, counted from their receipts, which arrive after the reads that
%% made them return. A chunk request goes on from node to node until one
%% answers 200, and the attempts that other nodes refused or failed
%% downloaded nothing.
downloads() -> downloads(0).
downloads(N) ->
    receive
        {receipt, <<"/chunk/", _/binary>>, <<"success">>} -> downloads(N + 1);
        {receipt, _, _} -> downloads(N)
    after 500 -> N
    end.
