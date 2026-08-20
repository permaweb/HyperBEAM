%%% @doc Deterministic mining search vectors for Arweave 2.9, and a live probe
%%% that requires this node to rediscover the solution of a block the network
%%% already accepted.
%%%
%%% Every deterministic vector mines over one synthetic weave: a single
%%% transaction of two chunks in the first block, served through the same
%%% `chunk-proof' key a gateway answers `GET /chunk/<offset>' with, and
%%% reporting the true offset of each chunk it serves. The source is told which
%%% of the two chunks it holds, which is how a node's partial copy of the weave
%%% is expressed: a byte of any other chunk is answered as a hole.
%%%
%%% The mining inputs are fixed, so every outcome asserted below is fixed with
%%% them. The VDF step of each vector was chosen for the search it produces --
%%% which nonce solves it, and which kind of solution -- so nothing here is
%%% sampled: the same inputs yield the same nonce on every run.
-module(dev_arweave_mining_test_vectors).
-export([live_mines_a_mainnet_solution_/0]).
-include("include/hb.hrl").
-include("include/ar_consensus.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The difficulty the vectors mine at. The two-chunk threshold is met by about
%%% a quarter of nonces and the one-chunk threshold, a hundredfold harder, by
%%% about one in two hundred -- so which kind of solution a search finds is
%%% decided by which range it can read, exactly as it is on the network.
-define(EASY_DIFF, (?MAX_DIFF * 3 div 5)).
%%% The difficulty no solution can meet: every threshold it scales to is the
%%% largest a 32-byte hash can be, and the protocol's check is a strict
%%% inequality.
-define(IMPOSSIBLE_DIFF, (?MAX_DIFF - 1)).
%%% A height above every fork whose rules a post-2.9 solution is bound by,
%%% the 2.9 fork included: below it, `ar_block:validate_replica_format/3'
%%% admits no replication format a solution here could be mined under.
-define(HEIGHT, 1_700_000).
%%% The VDF step number a solution is found at, which it carries into the block.
-define(STEP_NUMBER, 4_400_000).
%%% The weave the vectors mine over: one transaction of two chunks.
-define(CHUNKS, 2).
-define(WEAVE_SIZE, (?CHUNKS * ?DATA_CHUNK_SIZE)).

%% @doc A search whose second range it cannot read finds a one-chunk solution:
%% the hash of the first range's sub-chunk alone, at the hundredfold difficulty
%% the protocol charges for leaving the second chunk out. It declares no second
%% recall byte and carries no second proof, because the block it goes into
%% has neither.
%%
%% The step recalls its first range from the chunk this node holds and its
%% second from the chunk it does not, which is the position of a miner storing
%% one partition of a larger weave.
one_chunk_solution_test() ->
    Opts = opts(),
    {ok, Solution} = solve(session(278, ?EASY_DIFF, [1]), Opts),
    ?assertEqual(true, field(<<"solution">>, Solution, Opts)),
    ?assertEqual(2, field(<<"nonce">>, Solution, Opts)),
    ?assertEqual(3, field(<<"nonces-searched">>, Solution, Opts)),
    ?assertEqual(491482, field(<<"recall-byte">>, Solution, Opts)),
    ?assertEqual(
        not_found,
        hb_maps:get(<<"recall-byte2">>, Solution, not_found, Opts)
    ),
    ?assertEqual(not_found, hb_maps:get(<<"poa2">>, Solution, not_found, Opts)),
    ?assert(
        ar_node_utils:h1_passes_diff_check(
            hb_util:decode(field(<<"solution-hash">>, Solution, Opts)),
            diff_pair(?EASY_DIFF),
            ?REPLICA_2_9_PACKING_DIFFICULTY
        )
    ),
    ?assertEqual(
        {ok, {
            field(<<"solution-hash">>, Solution, Opts),
            field(<<"hash-preimage">>, Solution, Opts)
        }},
        hash(<<"h1">>,
            #{
                <<"h0">> => field(<<"h0">>, Solution, Opts),
                <<"nonce">> => field(<<"nonce">>, Solution, Opts),
                <<"chunk">> => proof(<<"poa">>, Solution, Opts)
            },
            Opts
        )
    ),
    ?assertMatch(
        {ok, #{ <<"valid">> := true, <<"chunk-verified">> := true }},
        proved(<<"poa">>, <<"recall-byte">>, Solution, Opts)
    ).

%% @doc A search that can read both ranges finds a two-chunk solution: the hash
%% of the second range's sub-chunk over the first range's own hash, at the
%% difficulty the block declares. Both recall bytes are declared and both proofs
%% are carried, and each proves a different chunk of the weave.
%%
%% The solution also carries the session it was found in, which is what a block
%% producer fills the rest of its header from.
two_chunk_solution_test() ->
    Opts = opts(),
    {ok, Solution} = solve(session(9, ?EASY_DIFF, [0, 1]), Opts),
    ?assertEqual(true, field(<<"solution">>, Solution, Opts)),
    ?assertEqual(0, field(<<"nonce">>, Solution, Opts)),
    ?assertEqual(1, field(<<"nonces-searched">>, Solution, Opts)),
    ?assertEqual(407865, field(<<"recall-byte">>, Solution, Opts)),
    ?assertEqual(176842, field(<<"recall-byte2">>, Solution, Opts)),
    ?assert(
        ar_node_utils:h2_passes_diff_check(
            hb_util:decode(field(<<"solution-hash">>, Solution, Opts)),
            diff_pair(?EASY_DIFF),
            ?REPLICA_2_9_PACKING_DIFFICULTY
        )
    ),
    % The hash is the one the validator recomputes from the two proofs: the
    % first range's sub-chunk hashed into H1, and the second's into H2 over it.
    {ok, {H1, _}} =
        hash(<<"h1">>,
            #{
                <<"h0">> => field(<<"h0">>, Solution, Opts),
                <<"nonce">> => field(<<"nonce">>, Solution, Opts),
                <<"chunk">> => proof(<<"poa">>, Solution, Opts)
            },
            Opts
        ),
    ?assertEqual(
        {ok, {
            field(<<"solution-hash">>, Solution, Opts),
            field(<<"hash-preimage">>, Solution, Opts)
        }},
        hash(<<"h2">>,
            #{
                <<"h0">> => field(<<"h0">>, Solution, Opts),
                <<"h1">> => H1,
                <<"chunk">> => proof(<<"poa2">>, Solution, Opts)
            },
            Opts
        )
    ),
    ?assertMatch(
        {ok, #{ <<"valid">> := true, <<"chunk-verified">> := true }},
        proved(<<"poa">>, <<"recall-byte">>, Solution, Opts)
    ),
    ?assertMatch(
        {ok, #{ <<"valid">> := true, <<"chunk-verified">> := true }},
        proved(<<"poa2">>, <<"recall-byte2">>, Solution, Opts)
    ),
    ?assertEqual(0, field(<<"partition-number">>, Solution, Opts)),
    ?assertEqual(
        ?REPLICA_2_9_PACKING_DIFFICULTY,
        field(<<"packing-difficulty">>, Solution, Opts)
    ),
    ?assertEqual(1, field(<<"replica-format">>, Solution, Opts)),
    ?assertEqual(
        hb_util:encode(reward_addr()),
        field(<<"reward-addr">>, Solution, Opts)
    ),
    ?assertEqual(
        hb_util:encode(output(9)),
        field(<<"nonce-limiter-output">>, Solution, Opts)
    ),
    ?assertEqual(
        ?STEP_NUMBER,
        field(<<"global-step-number">>, Solution, Opts)
    ).

%% @doc A difficulty nothing reaches is not an error. The pass hashes every
%% nonce it was given, against both thresholds, and reports how many it examined
%% -- which is what tells its caller where the next pass begins.
no_solution_test() ->
    Opts = opts(),
    {ok, Result} =
        bounded(session(9, ?IMPOSSIBLE_DIFF, [0, 1]), 4, Opts),
    ?assertEqual(false, field(<<"solution">>, Result, Opts)),
    ?assertEqual(4, field(<<"nonces-searched">>, Result, Opts)),
    ?assertEqual(
        not_found,
        hb_maps:get(<<"solution-hash">>, Result, not_found, Opts)
    ).

%% @doc A hole in this node's copy of the weave removes the nonces it covers and
%% nothing else. The step recalls its first range from a chunk this node does
%% not hold and, thirty-two nonces later, from one it does: the first group is
%% skipped without a hash being taken, and the solution comes from the second.
hole_in_the_weave_test() ->
    Opts = opts(),
    {ok, Solution} = solve(session(33, ?EASY_DIFF, [1]), Opts),
    ?assertEqual(true, field(<<"solution">>, Solution, Opts)),
    ?assertEqual(32, field(<<"nonce">>, Solution, Opts)),
    % The thirty-two nonces of the missing chunk cost nothing: only the nonce
    % that solved the step was ever hashed.
    ?assertEqual(1, field(<<"nonces-searched">>, Solution, Opts)),
    ?assertEqual(491002, field(<<"recall-byte">>, Solution, Opts)),
    ?assertEqual(283852, field(<<"recall-byte2">>, Solution, Opts)),
    ?assertMatch(
        {ok, #{ <<"valid">> := true, <<"chunk-verified">> := true }},
        proved(<<"poa">>, <<"recall-byte">>, Solution, Opts)
    ).

%% @doc A recall byte at or beyond the end of the weave is skipped whatever the
%% source holds there. The weave a session names is the one a solution would be
%% proved against, and a byte past its end has no block to prove against at all
%% -- so the source's own copy running longer, as a fork's would, cannot draw
%% the search into a solution nothing can validate.
beyond_the_weave_test() ->
    Opts = opts(),
    Session = session(278, ?EASY_DIFF, [1]),
    Shorter = Session#{ <<"weave-size">> => ?DATA_CHUNK_SIZE },
    ?assertMatch(
        {ok, #{ <<"solution">> := true, <<"nonce">> := 2 }},
        solve(Session, Opts)
    ),
    {ok, Result} = solve(Shorter, Opts),
    ?assertEqual(false, field(<<"solution">>, Result, Opts)),
    ?assertEqual(0, field(<<"nonces-searched">>, Result, Opts)),
    {ok, Range} = hb_ao:resolve(Shorter, <<"range">>, Opts),
    ?assertEqual([], field(<<"chunks">>, Range, Opts)).

%% @doc `max-nonces' bounds a pass to the nonces it was asked for. The step
%% solves at its third nonce, so a pass given two examines both and finds
%% nothing, and one given the whole range finds it -- the bound, and only the
%% bound, is what separates them.
max_nonces_bounds_the_pass_test() ->
    Opts = opts(),
    Session = session(278, ?EASY_DIFF, [1]),
    {ok, Bounded} = bounded(Session, 2, Opts),
    ?assertEqual(false, field(<<"solution">>, Bounded, Opts)),
    ?assertEqual(2, field(<<"nonces-searched">>, Bounded, Opts)),
    % The pass a bound describes is the one the range key reports.
    {ok, Range} = hb_ao:resolve(Session, bound(<<"range">>, 2), Opts),
    ?assertEqual(2, field(<<"nonces">>, Range, Opts)).

%% @doc The byte and sub-chunk a solution rests on are the ones the protocol
%% determines for its nonce: the recall byte the range key lists for the group,
%% and the sub-chunk of that chunk `~arweave-spora@2.9' indexes with the nonce.
%% A solution that packed any other sub-chunk would hash to something no
%% validator recomputes.
recall_byte_of_the_solution_test() ->
    Opts = opts(),
    Session = session(278, ?EASY_DIFF, [1]),
    {ok, Solution} = solve(Session, Opts),
    {ok, Range} = hb_ao:resolve(Session, <<"range">>, Opts),
    {Byte, Index} =
        recall(
            field(<<"range1-start">>, Range, Opts),
            field(<<"nonce">>, Solution, Opts),
            Opts
        ),
    ?assertEqual(Byte, field(<<"recall-byte">>, Solution, Opts)),
    ?assertEqual(
        hb_util:encode(pack(1, Index, Opts)),
        proof(<<"poa">>, Solution, Opts)
    ),
    % Only one group of either range lies within this weave, and it is the one
    % the search read.
    ?assertEqual(
        [
            #{
                <<"nonce">> => 0,
                <<"recall-byte">> => Byte,
                <<"recall-byte2">> => 3620
            }
        ],
        field(<<"chunks">>, Range, Opts)
    ).

%% @doc A source answering in any format but the one the partition is packed in
%% is refused by name, where the range enters the search and before a byte of it
%% is hashed.
%%
%% Nothing downstream could tell. A nonce slices the sub-chunk it addresses out
%% of whatever bytes it is given, so an unpacked answer yields hashes that meet
%% no difficulty and a partition that reads as holding nothing -- which looks
%% like bad luck rather than like a misconfigured source.
unsupported_packing_test() ->
    Opts = opts(),
    lists:foreach(
        fun(Format) ->
            {error, Error} =
                solve(session(278, ?EASY_DIFF, [1], Format), Opts),
            ?assertEqual(
                <<"unsupported-packing">>,
                field(<<"message">>, Error, Opts)
            )
        end,
        [<<"unpacked">>, <<"composite">>, <<"spora-2-6">>]
    ).

%% @doc A source answering a range with encoded chunks is refused by name,
%% where the range enters the search and before a byte of it is hashed.
%%
%% `range' carries its chunks as bytes and `chunk-proof' carries them as
%% base64url, because a pass hashes a range and drops it while a proof's fields
%% go into a block header. That makes the encoding part of the contract, and a
%% source reached over a codec that cannot carry bytes -- JSON, say -- would
%% answer with text. Nothing downstream could tell: a nonce would slice a
%% sub-chunk out of the text, hash it, and report a partition that holds
%% nothing. The size is what gives it away, and is checked rather than assumed.
encoded_range_test() ->
    Opts = opts(),
    {error, Error} = solve(encoded_session(278, ?EASY_DIFF, [1, 2]), Opts),
    ?assertEqual(<<"malformed-range">>, field(<<"message">>, Error, Opts)),
    ?assertNotEqual(
        nomatch,
        binary:match(field(<<"detail">>, Error, Opts), <<"262144">>)
    ).

%% @doc A pass over a real storage module on disk finds exactly the solution a
%% pass over a source answering from memory finds.
%%
%% This is the whole port in one assertion. The module is built the way a node
%% builds one: `prepare' writes the replica-2.9 entropy into the chunk file
%% slots, `store' walks each chunk's Merkle proof and enciphers it into the slot
%% its offset owns, and the pass then reads the packed bytes back out of the
%% file and hashes them. Nothing about the search knows it is reading a file.
%%
%% Equality is the right assertion and not a strong one by accident: the two
%% sources agree on the nonce, both recall bytes, both solution hashes, both
%% preimages, and both proofs of access -- the packed sub-chunks, the unpacked
%% chunks and the two Merkle paths. A storage module that placed a chunk one
%% bucket out, enciphered it with another bucket's entropy, or indexed it at an
%% offset the file does not hold would differ in at least one of them.
storage_module_source_test_() ->
    {timeout, 300, fun test_storage_module_source/0}.
test_storage_module_source() ->
    Opts = storage_opts(),
    ok = build_storage_module(Opts),
    {ok, FromMemory} = solve(session(9, ?EASY_DIFF, [0, 1]), Opts),
    {ok, FromDisk} =
        solve(
            (session(9, ?EASY_DIFF, [0, 1]))#{
                <<"weave">> => #{ <<"device">> => <<"arweave-storage@2.9">> }
            },
            Opts
        ),
    ?assertEqual(true, field(<<"solution">>, FromDisk, Opts)),
    % Key by key, so that a difference names itself. Two whole solutions
    % compared at once would say only that they differ.
    %
    % `priv' is excluded and only `priv'. It carries the hashpath of the
    % resolution that produced the answer, and the two resolutions differ by
    % construction: one names a storage device as its weave and the other names
    % a source answering from memory. Everything a block carries is compared.
    ?assertEqual(
        lists:sort(public(FromMemory, Opts)),
        lists:sort(public(FromDisk, Opts))
    ),
    lists:foreach(
        fun(Key) ->
            ?assertEqual(
                {Key, field(Key, FromMemory, Opts)},
                {Key, field(Key, FromDisk, Opts)}
            )
        end,
        public(FromMemory, Opts) -- [<<"poa">>, <<"poa2">>]
    ),
    lists:foreach(
        fun(Which) ->
            Memory = field(Which, FromMemory, Opts),
            Disk = field(Which, FromDisk, Opts),
            ?assertEqual(
                lists:sort(public(Memory, Opts)),
                lists:sort(public(Disk, Opts))
            ),
            lists:foreach(
                fun(Key) ->
                    ?assertEqual(
                        {Which, Key, field(Key, Memory, Opts)},
                        {Which, Key, field(Key, Disk, Opts)}
                    )
                end,
                public(Memory, Opts)
            )
        end,
        [<<"poa">>, <<"poa2">>]
    ),
    ?assertMatch(
        {ok, #{ <<"valid">> := true, <<"chunk-verified">> := true }},
        proved(<<"poa">>, <<"recall-byte">>, FromDisk, Opts)
    ),
    ?assertMatch(
        {ok, #{ <<"valid">> := true, <<"chunk-verified">> := true }},
        proved(<<"poa2">>, <<"recall-byte2">>, FromDisk, Opts)
    ).

%% @doc A storage module holding only one of the weave's two chunks yields the
%% one-chunk solution a node missing the other reaches, and the module reports
%% holding exactly one chunk's worth of bytes.
partial_storage_module_test_() ->
    {timeout, 300, fun test_partial_storage_module/0}.
test_partial_storage_module() ->
    Opts = storage_opts(),
    ok = prepare_storage_module(Opts),
    ok = store_chunk(1, Opts),
    {ok, Records} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-storage@2.9">> },
            <<"sync-record">>,
            Opts
        ),
    ?assertEqual(
        ?DATA_CHUNK_SIZE,
        hb_ao:get(<<"1/records/ar_chunk_storage/size">>, Records, Opts)
    ),
    {ok, FromDisk} =
        solve(
            (session(278, ?EASY_DIFF, [1]))#{
                <<"weave">> => #{ <<"device">> => <<"arweave-storage@2.9">> }
            },
            Opts
        ),
    ?assertEqual(true, field(<<"solution">>, FromDisk, Opts)),
    ?assertEqual(2, field(<<"nonce">>, FromDisk, Opts)),
    ?assertEqual(491482, field(<<"recall-byte">>, FromDisk, Opts)),
    ?assertEqual(
        not_found,
        hb_maps:get(<<"recall-byte2">>, FromDisk, not_found, Opts)
    ).

%% @doc A session at a height whose rules admit no such replication format is
%% refused rather than searched. Every solution it could produce would go into
%% a block `ar_block:validate_replica_format/3' rejects, so the search is work
%% that cannot pay -- and at packing difficulty 0 its nonces address no
%% sub-chunk at all.
unreplicable_session_test() ->
    Opts = opts(),
    Session = session(278, ?EASY_DIFF, [1]),
    {error, Error} =
        solve(Session#{ <<"height">> => ar_fork:height_2_9() - 1 }, Opts),
    ?assertEqual(
        <<"invalid-replica-format">>,
        field(<<"message">>, Error, Opts)
    ),
    {error, Zero} = solve(Session#{ <<"packing-difficulty">> => 0 }, Opts),
    ?assertEqual(
        <<"invalid-replica-format">>,
        field(<<"message">>, Zero, Opts)
    ).

%% @doc A weave with no partition upper bound is refused rather than divided
%% by. Both range starts are taken modulo that bound, so a weave of nothing has
%% no range to search.
empty_weave_test() ->
    Opts = opts(),
    Session = session(278, ?EASY_DIFF, [1]),
    {error, Error} =
        solve(Session#{ <<"partition-upper-bound">> => 0 }, Opts),
    ?assertEqual(<<"empty-weave">>, field(<<"message">>, Error, Opts)).

%%% The storage module the disk-backed vectors mine from: one bucket covering
%%% the whole of the two-chunk weave, packed for the address the weave is packed
%%% to. The bucket size is the weave itself, so every recall range a session can
%%% draw starts inside it.
-define(MODULE_BUCKET_SIZE, ?WEAVE_SIZE).

%% @doc Node options naming a fresh data directory and the one storage module
%% the disk-backed vectors build in it.
storage_opts() ->
    (opts())#{
        <<"arweave-data-dir">> => hb_util:bin(data_directory()),
        <<"arweave-storage-modules">> =>
            [
                #{
                    <<"bucket">> => 0,
                    <<"bucket-size">> => ?MODULE_BUCKET_SIZE,
                    <<"packing">> => <<"replica-2-9">>,
                    <<"address">> => hb_util:encode(reward_addr())
                }
            ]
    }.

%% @doc A data directory of this run's own, under the system temporary
%% directory.
data_directory() ->
    filename:join(
        [
            hb_util:list(hb_opts:get(<<"tmp-dir">>, <<"/tmp">>, #{})),
            "hb-arweave-mining",
            hb_util:list(hb_util:encode(crypto:strong_rand_bytes(8)))
        ]
    ).

%% @doc Build the storage module the disk-backed vectors mine from: entropy for
%% every bucket of it, then both chunks of the weave stored through their own
%% Merkle proofs.
build_storage_module(Opts) ->
    ok = prepare_storage_module(Opts),
    lists:foreach(fun(Chunk) -> ok = store_chunk(Chunk, Opts) end, [0, 1]),
    ok.

%% @doc Write the replica-2.9 entropy across the module. One footprint covers
%% one bucket of a module this short, so the pass is given as many footprints as
%% the module has buckets and then asked to confirm it is done.
prepare_storage_module(Opts) ->
    lists:foreach(
        fun(_Pass) ->
            {ok, _Result} =
                hb_ao:resolve(
                    #{ <<"device">> => <<"arweave-storage@2.9">> },
                    #{ <<"path">> => <<"prepare">>, <<"footprints">> => 1 },
                    Opts
                )
        end,
        lists:seq(1, ?CHUNKS)
    ),
    ok.

%% @doc Store one chunk of the weave, with the proof that places it there and
%% the bounds of the block that wrote it.
store_chunk(Chunk, Opts) ->
    {DataRoot, DataTree} = data_tree(),
    {TXRoot, TXTree} = tx_tree(),
    Offset = Chunk * ?DATA_CHUNK_SIZE,
    {ok, Stored} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-storage@2.9">> },
            #{
                <<"path">> => <<"store">>,
                <<"offset">> => Offset,
                <<"chunk">> => hb_util:encode(chunk(Chunk)),
                <<"tx-path">> =>
                    hb_util:encode(
                        ar_merkle:generate_path(TXRoot, Offset, TXTree)),
                <<"data-path">> =>
                    hb_util:encode(
                        ar_merkle:generate_path(DataRoot, Offset, DataTree)),
                <<"tx-root">> => hb_util:encode(TXRoot),
                <<"block-start-offset">> => 0,
                <<"block-size">> => ?WEAVE_SIZE,
                <<"packing">> => <<"replica-2-9">>,
                <<"address">> => hb_util:encode(reward_addr())
            },
            Opts
        ),
    ?assertEqual(
        (Chunk + 1) * ?DATA_CHUNK_SIZE,
        hb_util:int(field(<<"absolute-end-offset">>, Stored, Opts))
    ),
    ok.

%% @doc Search a session for a solution.
solve(Session, Opts) ->
    hb_ao:resolve(Session, <<"solve">>, Opts).

%% @doc Search a session for a solution, examining at most `Nonces' nonces.
bounded(Session, Nonces, Opts) ->
    hb_ao:resolve(Session, bound(<<"solve">>, Nonces), Opts).

%% @doc A request for one of the device's keys, bounded to a number of nonces.
bound(Path, Nonces) ->
    #{ <<"path">> => Path, <<"max-nonces">> => Nonces }.

%% @doc The mining session the vectors search: one partition holding the whole
%% of a two-chunk weave, at the VDF step and difficulty given, over a source
%% holding the chunks named in the format named.
session(Step, Diff, Held) ->
    session(Step, Diff, Held, <<"replica-2-9">>).
session(Step, Diff, Held, Format) ->
    #{
        <<"device">> => <<"arweave-mining@2.9">>,
        <<"nonce-limiter-output">> => hb_util:encode(output(Step)),
        <<"global-step-number">> => ?STEP_NUMBER,
        <<"seed">> => hb_util:encode(seed()),
        <<"reward-addr">> => hb_util:encode(reward_addr()),
        <<"partition-number">> => 0,
        <<"partition-upper-bound">> => ?WEAVE_SIZE,
        <<"weave-size">> => ?WEAVE_SIZE,
        <<"packing-difficulty">> => ?REPLICA_2_9_PACKING_DIFFICULTY,
        <<"replica-format">> => 1,
        <<"diff">> => Diff,
        <<"height">> => ?HEIGHT,
        <<"weave">> => weave(Held, Format)
    }.

%% @doc The same session over a source that base64urls the chunks of a range,
%% which is what any codec that cannot carry bytes makes of them.
encoded_session(Step, Diff, Held) ->
    Session = session(Step, Diff, Held),
    Weave = maps:get(<<"weave">>, Session),
    Device = maps:get(<<"device">>, Weave),
    Range = maps:get(range, Device),
    Session#{
        <<"weave">> =>
            Weave#{
                <<"device">> =>
                    Device#{
                        range =>
                            fun(Base, Req, Opts) ->
                                encoded(Range(Base, Req, Opts), Opts)
                            end
                    }
            }
    }.

%% @doc Re-spell every chunk of a range's answer as base64url.
encoded({ok, Result}, Opts) ->
    Chunks = hb_maps:get(<<"chunks">>, Result, #{}, Opts),
    {ok,
        Result#{
            <<"chunks">> =>
                hb_util:list_to_numbered_message(
                    [
                        Chunk#{
                            <<"chunk">> =>
                                hb_util:encode(
                                    hb_maps:get(<<"chunk">>, Chunk, <<>>, Opts))
                        }
                    ||
                        Chunk <- hb_util:message_to_ordered_list(Chunks, Opts)
                    ]
                )
        }
    }.

%% @doc A weave source: one transaction of two chunks in the first block of the
%% weave, answering the two keys a pass reads a weave through. `range' returns
%% the chunks of a span in the format the source holds them in, which is what a
%% pass hashes; `chunk-proof' returns one chunk with the two Merkle paths that
%% place it, which is what a solution carries. The source is told which chunks
%% it holds, so a byte of any other chunk falls in no range and is proved by
%% nothing -- which is what a node that has not synced that part of the weave
%% answers.
weave(Held, Format) ->
    #{
        <<"device">> =>
            #{
                range =>
                    fun(_Base, Req, Opts) ->
                        range(Held, Format, Req, Opts)
                    end,
                chunk_proof =>
                    fun(_Base, Req, Opts) ->
                        chunk_proof(
                            Held,
                            Format,
                            hb_util:int(
                                hb_maps:get(<<"offset">>, Req, 0, Opts)),
                            Opts
                        )
                    end
            }
    }.

%% @doc Answer with the chunks of a span of the weave this source holds, in the
%% form it holds them in. The span is the recall range the protocol defines at
%% the request's packing difficulty.
range(Held, Format, Req, Opts) ->
    Start = hb_util:int(hb_maps:get(<<"range-start">>, Req, 0, Opts)),
    Size =
        ar_block:get_recall_range_size(
            hb_util:int(
                hb_maps:get(<<"packing-difficulty">>, Req, 0, Opts))),
    {ok,
        #{
            <<"range-start">> => Start,
            <<"size">> => Size,
            <<"packing">> => Format,
            <<"chunks">> =>
                hb_util:list_to_numbered_message(
                    [
                        #{
                            <<"absolute-end-offset">> => end_offset(Chunk),
                            <<"chunk">> => held(Format, Chunk, Opts)
                        }
                    ||
                        Chunk <- Held, intersects(Chunk, Start, Size)
                    ]
                )
        }
    }.

%% @doc Hold for a chunk any of whose bytes lie inside a span.
intersects(Chunk, Start, Size) ->
    end_offset(Chunk) > Start
        andalso end_offset(Chunk) - ?DATA_CHUNK_SIZE < Start + Size.

%% @doc The absolute end offset of one chunk of this weave.
end_offset(Chunk) ->
    (Chunk + 1) * ?DATA_CHUNK_SIZE.

%% @doc Answer for the chunk of the weave holding a byte, with the two Merkle
%% paths that prove it lies where the source says it does.
chunk_proof(Held, Format, Offset, Opts) ->
    Chunk = Offset div ?DATA_CHUNK_SIZE,
    chunk_proof(lists:member(Chunk, Held), Format, Chunk, Offset, Opts).
chunk_proof(false, _Format, _Chunk, _Offset, _Opts) ->
    {error,
        #{
            <<"status">> => 404,
            <<"message">> => <<"chunk-not-found">>,
            <<"detail">> => <<"This node holds no chunk at that offset.">>
        }
    };
chunk_proof(true, Format, Chunk, Offset, Opts) ->
    {DataRoot, DataTree} = data_tree(),
    {TXRoot, TXTree} = tx_tree(),
    {ok,
        #{
            <<"chunk">> => hb_util:encode(held(Format, Chunk, Opts)),
            <<"unpacked-chunk">> => hb_util:encode(chunk(Chunk)),
            <<"chunk-size">> => ?DATA_CHUNK_SIZE,
            <<"absolute-end-offset">> => end_offset(Chunk),
            <<"packing">> => Format,
            <<"tx-path">> =>
                hb_util:encode(ar_merkle:generate_path(TXRoot, Offset, TXTree)),
            <<"data-path">> =>
                hb_util:encode(
                    ar_merkle:generate_path(DataRoot, Offset, DataTree))
        }
    }.

%% @doc The bytes a source holds for a chunk: the chunk itself where it holds
%% the weave unpacked, and every sub-chunk of it packed for the mining address
%% where it holds the weave as a miner does. A source naming any other format
%% serves the plain chunk, because what a search does with such an answer is
%% refuse it.
held(<<"replica-2-9">>, Chunk, Opts) ->
    <<
        << (pack(Chunk, Index, Opts))/binary >>
    ||
        Index <- lists:seq(0, ?COMPOSITE_PACKING_SUB_CHUNK_COUNT - 1)
    >>;
held(_Format, Chunk, _Opts) ->
    chunk(Chunk).

%% @doc The Merkle tree over the chunks of the weave's only transaction.
data_tree() ->
    ar_merkle:generate_tree(
        [
            {
                ar_tx:generate_chunk_id(chunk(Chunk)),
                (Chunk + 1) * ?DATA_CHUNK_SIZE
            }
        ||
            Chunk <- lists:seq(0, ?CHUNKS - 1)
        ]
    ).

%% @doc The Merkle tree over the transactions of the weave's only block.
tx_tree() ->
    {DataRoot, _DataTree} = data_tree(),
    ar_merkle:generate_tree([{DataRoot, ?WEAVE_SIZE}]).

%% @doc Check a proof a solution carries against the weave it was drawn from,
%% the way the block validator checks the proofs of a block header. The
%% sub-chunk a nonce points at is a property of the nonce alone, so the range it
%% is asked for does not enter the answer.
proved(Which, ByteKey, Solution, Opts) ->
    {TXRoot, _TXTree} = tx_tree(),
    {_Byte, Index} = recall(0, field(<<"nonce">>, Solution, Opts), Opts),
    hb_ao:resolve(
        #{
            <<"device">> => <<"arweave-spora@2.9">>,
            <<"block-start-offset">> => 0,
            <<"block-size">> => ?WEAVE_SIZE,
            <<"recall-offset">> => field(ByteKey, Solution, Opts),
            <<"tx-root">> => hb_util:encode(TXRoot),
            <<"sub-chunk-index">> => Index,
            <<"packing">> => packing(),
            <<"poa">> => hb_maps:get(Which, Solution, not_found, Opts)
        },
        <<"validate">>,
        Opts
    ).

%% @doc The byte a nonce recalls from a range and the sub-chunk of the chunk
%% holding it, as the protocol determines them.
recall(RangeStart, Nonce, Opts) ->
    {ok, Result} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-spora@2.9">>,
                <<"range-start">> => RangeStart,
                <<"nonce">> => Nonce,
                <<"packing-difficulty">> => ?REPLICA_2_9_PACKING_DIFFICULTY
            },
            <<"recall-byte">>,
            Opts
        ),
    {
        hb_util:int(hb_maps:get(<<"recall-byte">>, Result, not_found, Opts)),
        hb_util:int(hb_maps:get(<<"sub-chunk-index">>, Result, not_found, Opts))
    }.

%% @doc Compute one of the two solution hashes, with the preimage it was taken
%% over.
hash(Key, Request, Opts) ->
    {ok, Result} =
        hb_ao:resolve(
            Request#{ <<"device">> => <<"arweave-spora@2.9">> },
            Key,
            Opts
        ),
    {ok, {
        hb_maps:get(<<"hash">>, Result, not_found, Opts),
        hb_maps:get(<<"preimage">>, Result, not_found, Opts)
    }}.

%% @doc Pack one sub-chunk of one chunk of the weave for the mining address.
pack(Chunk, Index, Opts) ->
    {ok, Result} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-spora@2.9">>,
                <<"chunk">> => hb_util:encode(chunk(Chunk)),
                <<"sub-chunk-index">> => Index,
                <<"absolute-end-offset">> => (Chunk + 1) * ?DATA_CHUNK_SIZE,
                <<"packing">> => packing()
            },
            <<"pack-sub-chunk">>,
            Opts
        ),
    hb_maps:get(<<"chunk">>, Result, not_found, Opts).

%% @doc The packing the partition being mined is held in.
packing() ->
    #{
        <<"format">> => <<"replica-2-9">>,
        <<"reward-addr">> => hb_util:encode(reward_addr()),
        <<"packing-difficulty">> => ?REPLICA_2_9_PACKING_DIFFICULTY
    }.

%% @doc The difficulty pair a solution of either kind is checked against.
diff_pair(Diff) ->
    {ar_difficulty:poa1_diff(Diff, ?HEIGHT), Diff}.

%% @doc The packed sub-chunk one of a solution's proofs carries.
proof(Which, Solution, Opts) ->
    field(<<"chunk">>, hb_maps:get(Which, Solution, not_found, Opts), Opts).

%% @doc The bytes of one chunk of the weave.
chunk(Chunk) ->
    <<
        <<
            (crypto:hash(sha256,
                <<"dev_arweave_mining chunk ", Chunk:8, Part:16>>))/binary
        >>
    ||
        Part <- lists:seq(0, (?DATA_CHUNK_SIZE div 32) - 1)
    >>.

%% @doc The VDF output of a step.
output(Step) ->
    crypto:hash(sha256, <<"dev_arweave_mining step ", Step:16>>).

%% @doc The seed of the block below the one being mined, which is 48 bytes on
%% the network and truncated to 32 by the entropy preimage.
seed() ->
    crypto:hash(sha384, <<"dev_arweave_mining seed">>).

%% @doc The address the weave is packed to and the reward would be paid to.
reward_addr() ->
    crypto:hash(sha256, <<"dev_arweave_mining address">>).

%% @doc The keys of a message a caller can see, which is every key but the
%% private one the resolver carries its own bookkeeping in.
public(Message, Opts) ->
    hb_maps:keys(Message, Opts) -- [<<"priv">>].

%% @doc Read a field of a result, so that a key a device did not answer with is
%% `not_found' in the assertion rather than a badkey in the test.
field(Key, Message, Opts) ->
    hb_maps:get(Key, Message, not_found, Opts).

opts() -> #{ <<"store">> => [hb_test_utils:test_store()] }.

%%% Live probe.

%%% The block this node is required to rediscover the solution of. Its solution
%%% was found from two chunks, and its two recall bytes fall on opposite sides
%%% of the strict data split threshold -- so one chunk is stored under the
%%% bucket-aligned layout above it and the other under the free layout below,
%%% and the search has to read both.
-define(LIVE_HEIGHT, 1_982_900).

%%% The footprints of entropy each of the two modules is prepared with.
%%%
%%% A footprint is thirty-two 8 MiB blobs and the thousand and twenty-four
%%% buckets their slices are spread across, one every 3.27 GiB of the partition.
%%% A module one bucket wide holds at most one of those buckets, so a footprint
%%% writes one slice into it and none of the rest.
%%%
%%% Two of them, because the first writes nothing: the cursor of an unprepared
%%% module starts one byte into its range, a bucket is named by the offset it
%%% ends at, and so the first footprint is anchored on the bucket below the
%%% module's own. The second reaches the module's own bucket. Each is
%%% thirty-two RandomX runs whether it writes or not.
-define(LIVE_FOOTPRINTS, 2).

%%% How long the probe is given. Four footprints of entropy and two chunks
%%% deciphered for their proofs are a hundred and ninety-two 8 MiB RandomX runs,
%%% and the forty-odd blocks it reads of the network are read one at a time.
-define(LIVE_TIMEOUT, 1800).

%% @doc The probe, under a bound generous enough for the entropy and the
%% network. Nothing discovers it: it carries no name EUnit collects, and is
%% reached by asking for it --
%% `--devices dev_arweave_mining --test all:live_mines_a_mainnet_solution'.
live_mines_a_mainnet_solution_() ->
    {timeout, ?LIVE_TIMEOUT, fun live_mines_a_mainnet_solution/0}.

%% @doc A partition this node packed itself, holding the chunks a real mainnet
%% block was mined from, searched by this node's own pass at the difficulty the
%% network set that day, yields the solution the network accepted.
%%
%% Nothing here is synthetic. The chunks are the block's own, carried by the
%% proofs of access in its header; the packing is this node's, written into
%% chunk files on a fresh directory; the difficulty, the step, the seed and the
%% partition are the block's own fields; and the search is the device's.
%%
%% This is the only vector that closes the loop. Every other one checks this
%% node against itself or against a fixture, and a packing off by one byte, an
%% entropy keyed on a derived offset, a chunk placed one bucket out or an index
%% that disagrees with the file would pass all of them: each of those produces a
%% self-consistent node that mines blocks nothing accepts. Here the answer is
%% fixed by the network, in a header 1.98 million blocks deep, and the node has
%% to arrive at it.
live_mines_a_mainnet_solution() ->
    Base = opts(),
    Block = live_block(?LIVE_HEIGHT, Base),
    Parent = live_block(?LIVE_HEIGHT - 1, Base),
    Address = field(<<"reward_addr">>, Block, Base),
    Recalls =
        [
            live_recall(<<"recall_byte">>, <<"poa">>, Block, Base),
            live_recall(<<"recall_byte2">>, <<"poa2">>, Block, Base)
        ],
    Opts = live_storage_opts(Address, Recalls, Base),
    lists:foreach(fun(Id) -> live_prepare(Id, Opts) end, live_modules(Opts)),
    lists:foreach(
        fun(Recall) -> live_store(Recall, Address, Opts) end,
        Recalls
    ),
    {ok, Solution} = solve(live_session(Block, Parent, Opts), Opts),
    ?assertEqual(true, field(<<"solution">>, Solution, Opts)),
    ?assertEqual(live_nonce(Block, Opts), field(<<"nonce">>, Solution, Opts)),
    ?assertEqual(
        live_int(<<"recall_byte">>, Block, Opts),
        field(<<"recall-byte">>, Solution, Opts)
    ),
    ?assertEqual(
        live_int(<<"recall_byte2">>, Block, Opts),
        field(<<"recall-byte2">>, Solution, Opts)
    ),
    % The hash a block header commits to, and the bytes it was taken over.
    ?assertEqual(
        field(<<"hash">>, Block, Opts),
        field(<<"solution-hash">>, Solution, Opts)
    ),
    ?assertEqual(
        field(<<"hash_preimage">>, Block, Opts),
        field(<<"hash-preimage">>, Solution, Opts)
    ),
    ?assert(live_passes(Solution, Block, Opts)),
    % Each proof carries the packed sub-chunk the block declares for it, and the
    % chunk that sub-chunk deciphers to. The packed bytes are the statement: a
    % module packed for another address, or enciphered with another bucket's
    % entropy, hashes to something no validator recomputes.
    lists:foreach(
        fun(Which) ->
            Declared = field(Which, Block, Opts),
            Proved = field(Which, Solution, Opts),
            ?assertEqual(
                {Which, field(<<"chunk">>, Declared, Opts)},
                {Which, field(<<"chunk">>, Proved, Opts)}
            ),
            ?assertEqual(
                {Which, field(<<"unpacked_chunk">>, Declared, Opts)},
                {Which, field(<<"unpacked-chunk">>, Proved, Opts)}
            )
        end,
        [<<"poa">>, <<"poa2">>]
    ).

%% @doc What this node has to hold to search one of a block's two recall ranges:
%% the byte the block recalled, the proof of access its header carries for that
%% byte, the bounds of the block that wrote the chunk the byte falls in, and
%% where in the weave those bounds place that chunk.
live_recall(ByteKey, PoAKey, Block, Opts) ->
    Byte = live_int(ByteKey, Block, Opts),
    PoA = field(PoAKey, Block, Opts),
    Bounds = live_bounds(live_seek(Byte), Opts),
    #{
        <<"byte">> => Byte,
        <<"poa">> => PoA,
        <<"bounds">> => Bounds,
        <<"end-offset">> => live_placement(Byte, PoA, Bounds, Opts)
    }.

%% @doc The byte of a chunk a recall byte is proved through: one this node knows
%% lies in the unpadded part of the chunk, which is where both the block index
%% and every Merkle walk are asked about it.
live_seek(Byte) ->
    ar_chunk_storage:get_chunk_seek_offset(Byte + 1) - 1.

%% @doc Where a block's own proof of access places the chunk it proves: the
%% absolute end offset its two Merkle paths walk to under the tx root of the
%% block that wrote it.
%%
%% The recalled byte does not say. A chunk ends where the Merkle layout of the
%% transaction holding it put it, so below the strict data split threshold the
%% bucket it occupies can be the one above the bucket that byte's own 256 KiB
%% would end in -- and a module built around the wrong bucket is prepared with
%% another bucket's entropy and holds the chunk in a slot no recall range reads.
%% `~arweave-storage@2.9/store' reads the offset out of the same walk, for the
%% same reason.
live_placement(Byte, PoA, Bounds, Opts) ->
    {ok, Placement} =
        hb_ao:resolve(
            Bounds#{
                <<"device">> => <<"arweave-spora@2.9">>,
                <<"poa">> =>
                    #{
                        <<"tx-path">> => field(<<"tx_path">>, PoA, Opts),
                        <<"data-path">> => field(<<"data_path">>, PoA, Opts),
                        <<"chunk">> => <<>>
                    },
                <<"recall-offset">> => live_seek(Byte),
                <<"sub-chunk-index">> => 0,
                <<"expected-chunk-id">> =>
                    hb_util:encode(
                        ar_tx:generate_chunk_id(
                            hb_util:decode(
                                field(<<"unpacked_chunk">>, PoA, Opts)
                            )
                        )
                    )
            },
            <<"validate">>,
            Opts
        ),
    ?assertEqual(true, field(<<"valid">>, Placement, Opts)),
    ?assertEqual(?DATA_CHUNK_SIZE, field(<<"chunk-size">>, Placement, Opts)),
    hb_util:int(field(<<"absolute-end-offset">>, Placement, Opts)).

%% @doc The bounds of the block that wrote the chunk holding a byte: the
%% transaction root it committed to, where it begins in the weave, and how many
%% bytes it added.
%%
%% `~arweave-storage@2.9/store' takes these from the block index of this node's
%% own validated tip unless the caller supplies them, and a node mining from
%% storage modules is one that has bootstrapped a chain. This vector has not, so
%% it supplies them -- which is a property of the harness rather than a shortcut
%% the device offers. See `decisions/block-index-for-stored-chunks.md'.
%%
%% The block that wrote a byte is the first whose weave size exceeds it, and the
%% weave only grows, so the search holds a block below the byte and a block
%% above it and halves the gap between them: twenty-one blocks read of the 1.98
%% million the chain holds.
live_bounds(Byte, Opts) ->
    live_bounds(Byte, 0, ?LIVE_HEIGHT, Opts).
live_bounds(_Byte, Below, Above, Opts) when Below + 1 >= Above ->
    Block = live_block(Above, Opts),
    Start = live_int(<<"weave_size">>, live_block(Below, Opts), Opts),
    #{
        <<"tx-root">> => field(<<"tx_root">>, Block, Opts),
        <<"block-start-offset">> => Start,
        <<"block-size">> => live_int(<<"weave_size">>, Block, Opts) - Start
    };
live_bounds(Byte, Below, Above, Opts) ->
    Middle = (Below + Above) div 2,
    case live_int(<<"weave_size">>, live_block(Middle, Opts), Opts) > Byte of
        true -> live_bounds(Byte, Below, Middle, Opts);
        false -> live_bounds(Byte, Middle, Above, Opts)
    end.

%% @doc Node options holding one storage module per recall byte, on a data
%% directory of this run's own, each packed replica-2.9 for the address the
%% block was mined to.
%%
%% Each module is one bucket wide: the bucket the chunk the block proved sits
%% in, which is the bucket that chunk's own end offset names. That is the whole
%% of what a pass reads for the byte, and the ten-chunk overlap every
%% replica-2.9 module carries takes the module past the end of the 2 621 440
%% byte recall range whatever the byte's position within it. A module any wider
%% would cost a footprint of entropy for every bucket between its start and this
%% one before the search could read a byte.
%%
%% A module reaching only part of a range is not a module reaching part of an
%% answer. A span is read from every module that shares a byte with it and is
%% answered with the chunks those modules hold, so the recall range that begins
%% 1.25 MiB below this module is answered with the one chunk of it that any
%% nonce of the block's own group reads.
live_storage_opts(Address, Recalls, Opts) ->
    Opts#{
        <<"arweave-data-dir">> => hb_util:bin(data_directory()),
        <<"arweave-storage-modules">> =>
            [
                #{
                    <<"bucket">> =>
                        ar_chunk_storage:get_chunk_bucket_start(
                            field(<<"end-offset">>, Recall, Opts)
                        ) div ?DATA_CHUNK_SIZE,
                    <<"bucket-size">> => ?DATA_CHUNK_SIZE,
                    <<"packing">> => <<"replica-2-9">>,
                    <<"address">> => Address
                }
            ||
                Recall <- Recalls
            ]
    }.

%% @doc The identifier of every storage module this node is configured with, in
%% the order they were configured.
live_modules(Opts) ->
    {ok, Modules} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-storage@2.9">> },
            <<"modules">>,
            Opts
        ),
    [
        field(<<"id">>, Module, Opts)
    ||
        Module <- hb_util:message_to_ordered_list(Modules, Opts)
    ].

%% @doc Write the entropy of the bucket one module holds, and require the module
%% to report holding exactly that one bucket's worth of it.
live_prepare(Id, Opts) ->
    {ok, Result} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-storage@2.9">> },
            #{
                <<"path">> => <<"prepare">>,
                <<"module">> => Id,
                <<"footprints">> => ?LIVE_FOOTPRINTS
            },
            Opts
        ),
    ?assertEqual(?LIVE_FOOTPRINTS, field(<<"footprints">>, Result, Opts)),
    ?assertEqual(
        ?DATA_CHUNK_SIZE,
        live_record(Id, <<"ar_chunk_storage_replica_2_9_5_entropy">>, Opts)
    ).

%% @doc Store the chunk a block proved for one of its recall bytes, through the
%% two Merkle paths that proof carries and the bounds of the block that wrote
%% it, in whichever module covers the offset the walk puts the chunk at.
%%
%% Where it lands is the assertion. The offset comes out of the proof, so a
%% chunk stored one bucket out of the one its recall byte falls in is a chunk
%% the pass reading that byte's range never sees.
live_store(Recall, Address, Opts) ->
    Byte = field(<<"byte">>, Recall, Opts),
    PoA = field(<<"poa">>, Recall, Opts),
    {ok, Stored} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-storage@2.9">> },
            (field(<<"bounds">>, Recall, Opts))#{
                <<"path">> => <<"store">>,
                <<"offset">> => Byte,
                <<"chunk">> => field(<<"unpacked_chunk">>, PoA, Opts),
                <<"tx-path">> => field(<<"tx_path">>, PoA, Opts),
                <<"data-path">> => field(<<"data_path">>, PoA, Opts),
                <<"packing">> => <<"replica-2-9">>,
                <<"address">> => Address
            },
            Opts
        ),
    EndOffset = hb_util:int(field(<<"absolute-end-offset">>, Stored, Opts)),
    Padded = hb_util:int(field(<<"padded-end-offset">>, Stored, Opts)),
    ?assertEqual(true, field(<<"stored">>, Stored, Opts)),
    ?assertEqual(field(<<"end-offset">>, Recall, Opts), EndOffset),
    ?assertEqual(ar_block:get_chunk_padded_offset(EndOffset), Padded),
    % The slot the chunk took is the slot the recalled byte reads from.
    ?assert(Byte >= Padded - ?DATA_CHUNK_SIZE andalso Byte < Padded),
    ?assertEqual(
        ?DATA_CHUNK_SIZE,
        live_record(field(<<"module">>, Stored, Opts), <<"ar_chunk_storage">>,
            Opts)
    ).

%% @doc How many bytes of one module one of its sync records holds.
live_record(Id, Record, Opts) ->
    {ok, Records} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-storage@2.9">> },
            #{ <<"path">> => <<"sync-record">>, <<"module">> => Id },
            Opts
        ),
    hb_ao:get(<<"1/records/", Record/binary, "/size">>, Records, Opts).

%% @doc The mining session a block declares, restated as the keys a pass is
%% searched under: the step it was found at with the seed of the block below it,
%% the partition it searched and the weave that partition stood in, the address
%% it is packed to, and the difficulty and height its solution is held to. The
%% weave is the storage modules this node built.
live_session(Block, Parent, Opts) ->
    Info = field(<<"nonce_limiter_info">>, Block, Opts),
    #{
        <<"device">> => <<"arweave-mining@2.9">>,
        <<"nonce-limiter-output">> => field(<<"output">>, Info, Opts),
        <<"global-step-number">> =>
            live_int(<<"global_step_number">>, Info, Opts),
        <<"seed">> =>
            field(
                <<"seed">>,
                field(<<"nonce_limiter_info">>, Parent, Opts),
                Opts
            ),
        <<"reward-addr">> => field(<<"reward_addr">>, Block, Opts),
        <<"partition-number">> => live_int(<<"partition_number">>, Block, Opts),
        <<"partition-upper-bound">> =>
            live_int(<<"zone_upper_bound">>, Info, Opts),
        <<"weave-size">> => live_int(<<"weave_size">>, Block, Opts),
        <<"packing-difficulty">> => ?REPLICA_2_9_PACKING_DIFFICULTY,
        <<"replica-format">> => 1,
        <<"diff">> => live_int(<<"diff">>, Block, Opts),
        <<"height">> => live_int(<<"height">>, Block, Opts),
        <<"weave">> => #{ <<"device">> => <<"arweave-storage@2.9">> }
    }.

%% @doc Whether a two-chunk solution hash meets the difficulty its block
%% declares. The pair the check is given carries both thresholds -- the block's
%% own, and the hundredfold a solution drawn from one range alone is held to --
%% and a two-chunk solution answers to the first of them.
live_passes(Solution, Block, Opts) ->
    ar_node_utils:h2_passes_diff_check(
        hb_util:decode(field(<<"solution-hash">>, Solution, Opts)),
        {
            ar_difficulty:poa1_diff(
                live_int(<<"diff">>, Block, Opts),
                live_int(<<"height">>, Block, Opts)
            ),
            live_int(<<"diff">>, Block, Opts)
        },
        ?REPLICA_2_9_PACKING_DIFFICULTY
    ).

%% @doc The nonce a block was found at, which its header carries as the bytes of
%% the number.
live_nonce(Block, Opts) ->
    binary:decode_unsigned(hb_util:decode(field(<<"nonce">>, Block, Opts))).

%% @doc Read an integer field of a block message.
live_int(Key, Message, Opts) ->
    hb_util:int(field(Key, Message, Opts)).

%% @doc Read a block from the network by height.
live_block(Height, Opts) ->
    {ok, Block} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{ <<"path">> => <<"block">>, <<"block">> => Height },
            Opts
        ),
    Block.
