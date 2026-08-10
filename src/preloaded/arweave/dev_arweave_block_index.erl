%%% @doc An AO-Core interface to Arweave's block index: the
%%% `{indep-hash, weave-size, tx-root}' triplet of every block from genesis.
%%%
%%% The index exists for one reason above all others. A proof of access names a
%%% recall byte, and that byte may fall anywhere in the weave, so validating a
%%% single block requires mapping an arbitrary offset back onto the block that
%%% wrote it. `bounds/3' is that mapping, and it must stay cheap over an index
%%% that is ~2M entries today and only grows.
%%%
%%% Entries therefore live in the `hb_store', chunked into fixed-size runs at
%%% `~arweave-block-index@2.9/runs/<n>'. A `run-index' -- one
%%% `{weave-size, root}' pair per run -- lets a lookup binary search the runs
%%% without reading them, then binary search inside the single run it does
%%% read. Each entry is a fixed 89-byte record, so the search inside a run is
%%% offset arithmetic rather than a scan. The variable-width form Arweave
%%% serves is parsed once, on ingest, by `from-binary/3'.
%%%
%%% What a lookup follows is the content identifier the state message holds,
%%% never the `runs/<n>' path. Two index states -- a fork, or a bootstrap
%%% half-finished -- disagree about the contents of run `n' while agreeing
%%% about every run before it, so the name is a handle for enumerating the
%%% device's own store namespace, and the identifier is what makes a state
%%% immutable.
%%%
%%% The index is also what makes a node self-authenticating relative to its
%%% trusted checkpoint. A block's
%%% `hash-list-merkle' is the unbalanced Merkle root over every index entry
%%% preceding it, so `root/3' recomputed from stored entries and compared
%%% against that one committed hash proves every one of those entries at once.
%%% For that reason `root/3' always recomputes and never returns the
%%% maintained `root' field: a memoised answer would verify nothing. `append/3'
%%% does carry the field forward with a single hash, which is what makes the
%%% per-block `hash-list-merkle' check affordable, and the two agree by
%%% construction.
-module(dev_arweave_block_index).
-implements(<<"arweave-block-index@2.9">>).
-export([info/1, bounds/3, at/3, root/3, verify/3, append/3, rollback/3]).
-export([from_binary/3]).
%%% Helper functions.
-export([seek/2]).
-ifdef(TEST).
-export([safe_key/1]).
-endif.
-include("include/hb.hrl").

%%% The number of entries in a run. Sized so that the run a lookup reads stays
%%% a few hundred kilobytes while the run-index stays small enough to carry in
%%% the state message.
-define(RUN_SIZE, 4096).
%%% A stored entry: the 48-byte block hash, the weave size, and the tx root
%%% padded to its full width with its true length recorded alongside. The
%%% fixed width is the point -- it is what makes the search inside a run
%%% logarithmic rather than a scan.
-define(ENTRY_SIZE, 89).
%%% A run-index element: the weave size of the run's last entry, and the
%%% unbalanced Merkle root over every entry up to and including it.
-define(RUN_INDEX_SIZE, 56).

%% @doc Export only the index operations, leaving message manipulation to
%% `message@1.0'. `seek' is excluded because it hands a `fun' to the vendored
%% arithmetic rather than a message to a caller, so it is not a key.
info(_Base) ->
    #{
        excludes =>
            [<<"keys">>, <<"set">>, <<"set-path">>, <<"remove">>, <<"seek">>]
    }.

%% @doc Return the block that wrote the byte at `offset': the weave range it
%% covers and the tx root it committed. This is the key the index exists for,
%% so it reads exactly two values from the store -- the run-index, then the
%% one run that can hold the answer -- and binary searches both.
bounds(Base, Req, Opts) ->
    Offset = hb_util:int(required(<<"offset">>, Base, Req, Opts)),
    RunIndex = run_index(Base, Opts),
    case first_run_above(RunIndex, Offset) of
        not_found ->
            {error, error_message(<<"offset-out-of-range">>,
                <<"The offset lies beyond the end of the weave.">>)};
        RunNumber ->
            Run = run(Base, RunNumber, Opts),
            Index = first_entry_above(Run, Offset),
            {_, BlockEnd, TXRoot} = decode_entry(entry_at(Run, Index)),
            {ok,
                #{
                    <<"block-start">> =>
                        block_start(RunIndex, RunNumber, Run, Index),
                    <<"block-end">> => BlockEnd,
                    <<"tx-root">> => hb_util:encode(TXRoot)
                }
            }
    end.

%% @doc Return the triplet recorded for the block at `height'.
at(Base, Req, Opts) ->
    Height = hb_util:int(required(<<"height">>, Base, Req, Opts)),
    case Height >= 0 andalso Height < entries(Base, Opts) of
        false ->
            {error, error_message(<<"height-out-of-range">>,
                <<"The index does not cover the given height.">>)};
        true ->
            RunSize = run_size(Base, Opts),
            Run = run(Base, Height div RunSize, Opts),
            {Hash, WeaveSize, TXRoot} =
                decode_entry(entry_at(Run, Height rem RunSize)),
            {ok,
                #{
                    <<"indep-hash">> => hb_util:encode(Hash),
                    <<"weave-size">> => WeaveSize,
                    <<"tx-root">> => hb_util:encode(TXRoot)
                }
            }
    end.

%% @doc Recompute the unbalanced Merkle root over every entry in the index.
%% Always a recomputation from the stored entries: this is the check a
%% bootstrapping node's whole trust model rests on, so returning the
%% maintained `root' field would make it vacuous.
root(Base, _Req, Opts) ->
    {ok, #{ <<"root">> => hb_util:encode(recompute_root(Base, Opts)) }}.

%% @doc Verify the index against a block's `hash-list-merkle'. Checking ~2M
%% entries against a single committed hash is what lets a node accept an index
%% fetched from an untrusted peer.
verify(Base, Req, Opts) ->
    Expected = decoded(<<"expected-root">>, Base, Req, Opts),
    case recompute_root(Base, Opts) of
        Expected ->
            {ok, #{ <<"valid">> => true }};
        _ ->
            {error, error_message(<<"invalid-block-index-root">>,
                <<"The index does not hash to the expected root.">>)}
    end.

%% @doc Extend the index by one block.
append(Base, Req, Opts) ->
    Hash = decoded(<<"indep-hash">>, Base, Req, Opts),
    WeaveSize = hb_util:int(required(<<"weave-size">>, Base, Req, Opts)),
    TXRoot = decoded(<<"tx-root">>, Base, Req, Opts),
    extend(Base, [{Hash, WeaveSize, TXRoot}], Opts).

%% @doc Unwind the last `count' blocks, as a reorg requires. The root of the
%% shortened index is rebuilt from the last run boundary at or below the new
%% length, so the cost is bounded by the run size rather than by the length of
%% the index.
rollback(Base, Req, Opts) ->
    Count = hb_util:int(required(<<"count">>, Base, Req, Opts)),
    Entries = entries(Base, Opts),
    case Count >= 0 andalso Count =< Entries of
        false ->
            {error, error_message(<<"invalid-rollback-count">>,
                <<"Cannot unwind more blocks than the index holds.">>)};
        true ->
            {ok, truncate(Base, Entries - Count, Opts)}
    end.

%% @doc Ingest a range of entries in the binary form `/block_index2' serves,
%% appending them to the index. The wire order is oldest-first, so successive
%% pages of a bootstrap fetch can be handed straight to this key in the order
%% they arrive. When `start-height' is given it must name the next height the
%% index expects, which turns a dropped or repeated page into an error rather
%% than a silently wrong index.
from_binary(Base, Req, Opts) ->
    Body = required(<<"body">>, Base, Req, Opts),
    Entries = entries(Base, Opts),
    case hb_util:int(get_first(<<"start-height">>, Base, Req, Entries, Opts)) of
        Entries ->
            case decode_wire(Body, []) of
                {error, Message, Detail} ->
                    {error, error_message(Message, Detail)};
                Parsed ->
                    extend(Base, Parsed, Opts)
            end;
        _ ->
            {error, error_message(<<"non-contiguous-index-range">>,
                <<"The range does not start where the index ends.">>)}
    end.

%%% Helper functions.

%% @doc Return the lookup function the vendored `ar_block_index' arithmetic
%% takes, backed by this index. `~arweave-block@2.9' hands it to
%% `ar_block:get_block_bounds/3', which needs a lookup rather than a message.
%%
%% The run-index is read once and closed over, so a seek used repeatedly --
%% which is how the vendored code uses it -- reads one run per query and no
%% more. `{hash, _}' is the exception: the index is ordered by height, not by
%% hash, so a hash is found by scanning back from the tip. Its callers
%% (`member/2', `get_list_by_hash/2') look for recent blocks, which are in the
%% last run.
seek(Base, Opts) ->
    RunSize = run_size(Base, Opts),
    Entries = entries(Base, Opts),
    RunIndex = run_index(Base, Opts),
    fun
        ({height, Height}) when Height >= 0, Height < Entries ->
            decode_entry(
                entry_at(run(Base, Height div RunSize, Opts), Height rem RunSize)
            );
        ({height, _Height}) ->
            not_found;
        ({weave_size_above, Offset}) ->
            case first_run_above(RunIndex, Offset) of
                not_found ->
                    not_found;
                RunNumber ->
                    Run = run(Base, RunNumber, Opts),
                    Index = first_entry_above(Run, Offset),
                    {(RunNumber * RunSize) + Index, decode_entry(entry_at(Run, Index))}
            end;
        ({hash, Hash}) ->
            find_hash(Base, Hash, run_count(Entries, RunSize) - 1, RunSize, Opts)
    end.

%% @doc Scan runs from the newest backwards for the entry with the given block
%% hash. A hash occurs at a multiple of the entry size, which is what
%% distinguishes it from the same bytes appearing inside a tx root.
find_hash(_Base, _Hash, RunNumber, _RunSize, _Opts) when RunNumber < 0 ->
    not_found;
find_hash(Base, Hash, RunNumber, RunSize, Opts) ->
    Run = run(Base, RunNumber, Opts),
    case [ Pos || {Pos, _} <- binary:matches(Run, Hash), Pos rem ?ENTRY_SIZE == 0 ] of
        [] ->
            find_hash(Base, Hash, RunNumber - 1, RunSize, Opts);
        [Pos | _] ->
            {
                (RunNumber * RunSize) + (Pos div ?ENTRY_SIZE),
                decode_entry(binary:part(Run, Pos, ?ENTRY_SIZE))
            }
    end.

%%% Internal functions.

%% @doc Append entries to the index, rebuilding the runs they fall in and
%% carrying the root forward one entry at a time. Rejects a weave size that
%% moves backwards: the search in `bounds/3' relies on the weave size being
%% non-decreasing, so an out-of-order entry would not merely be wrong, it
%% would make correct entries unreachable. Rejects an entry too wide for the
%% stored form first, because that one is a question about the bytes rather
%% than about the sequence.
extend(Base, Parsed, Opts) ->
    maybe
        ok ?= storable(Parsed),
        ok ?= monotonic(Parsed, last_weave_size(Base, Opts)),
        RunSize = run_size(Base, Opts),
        Entries = entries(Base, Opts),
        Complete = Entries div RunSize,
        Tail = tail_run(Base, Complete, Entries rem RunSize, Opts),
        {Runs, RunIndex, Root} =
            write_runs(
                Complete,
                << Tail/binary, (encode_entries(Parsed))/binary >>,
                run_root(Base, Complete - 1, Opts),
                RunSize,
                Opts
            ),
        {ok,
            Base#{
                <<"device">> => <<"arweave-block-index@2.9">>,
                <<"length">> => Entries + length(Parsed),
                <<"run-size">> => RunSize,
                <<"root">> => hb_util:encode(Root),
                <<"run-index">> => splice_run_index(Base, RunIndex, Complete, Opts),
                <<"runs">> => hb_maps:merge(runs(Base, Opts), Runs, Opts)
            }
        }
    end.

%% @doc Shorten the index to `entries' entries, rebuilding the root from the
%% last complete run below the new length.
truncate(Base, Entries, Opts) ->
    RunSize = run_size(Base, Opts),
    Complete = Entries div RunSize,
    {Runs, RunIndex, Root} =
        write_runs(
            Complete,
            tail_run(Base, Complete, Entries rem RunSize, Opts),
            run_root(Base, Complete - 1, Opts),
            RunSize,
            Opts
        ),
    Base#{
        <<"length">> => Entries,
        <<"root">> => hb_util:encode(Root),
        <<"run-index">> => splice_run_index(Base, RunIndex, Complete, Opts),
        <<"runs">> =>
            hb_maps:merge(
                hb_maps:with(run_keys(Complete), runs(Base, Opts), Opts),
                Runs,
                Opts
            )
    }.

%% @doc Write a stretch of entries out as whole runs, returning the new run
%% links, the run-index elements describing them, and the root after the last
%% entry. A trailing partial run is written too -- it is rewritten as the
%% index grows into it.
write_runs(RunNumber, Entries, Root, RunSize, Opts) ->
    write_runs(RunNumber, Entries, Root, RunSize, #{}, <<>>, Opts).
write_runs(_RunNumber, <<>>, Root, _RunSize, Runs, RunIndex, _Opts) ->
    {Runs, RunIndex, Root};
write_runs(RunNumber, Entries, Root, RunSize, Runs, RunIndex, Opts) ->
    Size = min(byte_size(Entries), RunSize * ?ENTRY_SIZE),
    << Run:Size/binary, Rest/binary >> = Entries,
    Count = Size div ?ENTRY_SIZE,
    RunRoot = fold_root(Run, 0, Count, Root),
    {ok, ID} =
        hb_cache:write_binary(
            << "~arweave-block-index@2.9/runs/", (hb_util:bin(RunNumber))/binary >>,
            Run,
            Opts
        ),
    write_runs(
        RunNumber + 1,
        Rest,
        RunRoot,
        RunSize,
        Runs#{ run_key(RunNumber) => ID },
        << RunIndex/binary, (weave_size_at(Run, Count - 1)):64, RunRoot:48/binary >>,
        Opts
    ).

%% @doc Splice new run-index elements over the ones they replace, keeping the
%% elements of the runs below `complete' verbatim.
splice_run_index(Base, RunIndex, Complete, Opts) ->
    Kept = binary:part(run_index(Base, Opts), 0, Complete * ?RUN_INDEX_SIZE),
    {ok, ID} = hb_cache:write(<< Kept/binary, RunIndex/binary >>, Opts),
    ID.

%% @doc Recompute the root over every entry, run by run. The runs are read one
%% at a time rather than concatenated, so verifying the whole weave never
%% holds more than a single run in memory.
recompute_root(Base, Opts) ->
    RunSize = run_size(Base, Opts),
    lists:foldl(
        fun(RunNumber, Root) ->
            Run = run(Base, RunNumber, Opts),
            fold_root(Run, 0, byte_size(Run) div ?ENTRY_SIZE, Root)
        end,
        <<>>,
        lists:seq(0, run_count(entries(Base, Opts), RunSize) - 1)
    ).

%% @doc Fold `ar_unbalanced_merkle' over a run's entries, oldest first.
fold_root(_Run, Count, Count, Root) ->
    Root;
fold_root(Run, Index, Count, Root) ->
    fold_root(
        Run,
        Index + 1,
        Count,
        ar_unbalanced_merkle:root(
            Root,
            decode_entry(entry_at(Run, Index)),
            fun ar_unbalanced_merkle:hash_block_index_entry/1
        )
    ).

%% @doc Find the lowest-numbered run whose last entry lies beyond `offset',
%% which is the only run that can hold the block covering it.
first_run_above(RunIndex, Offset) ->
    Count = byte_size(RunIndex) div ?RUN_INDEX_SIZE,
    case Count > 0 andalso run_index_weave_size(RunIndex, Count - 1) > Offset of
        false ->
            not_found;
        true ->
            search(
                fun(Index) -> run_index_weave_size(RunIndex, Index) end,
                Offset,
                0,
                Count - 1
            )
    end.

%% @doc Find the lowest-indexed entry in a run whose weave size lies beyond
%% `offset'. The caller has already established that the run holds one.
first_entry_above(Run, Offset) ->
    search(
        fun(Index) -> weave_size_at(Run, Index) end,
        Offset,
        0,
        (byte_size(Run) div ?ENTRY_SIZE) - 1
    ).

%% @doc Binary search a non-decreasing sequence for the lowest index whose
%% value exceeds `offset'. `Hi' is known to satisfy the predicate.
search(_Value, _Offset, Index, Index) ->
    Index;
search(Value, Offset, Lo, Hi) ->
    Mid = Lo + ((Hi - Lo) div 2),
    case Value(Mid) > Offset of
        true -> search(Value, Offset, Lo, Mid);
        false -> search(Value, Offset, Mid + 1, Hi)
    end.

%% @doc The weave size the block covering an offset starts at: its
%% predecessor's. When the block is the first of its run the predecessor's
%% weave size is already in the run-index, so the neighbouring run is never
%% read.
block_start(_RunIndex, 0, _Run, 0) ->
    0;
block_start(RunIndex, RunNumber, _Run, 0) ->
    run_index_weave_size(RunIndex, RunNumber - 1);
block_start(_RunIndex, _RunNumber, Run, Index) ->
    weave_size_at(Run, Index - 1).

%% @doc Parse the block index binary Arweave serves, oldest entry first.
decode_wire(<<>>, Parsed) ->
    lists:reverse(Parsed);
decode_wire(<< Hash:48/binary, WeaveSizeSize:16, WeaveSize:(WeaveSizeSize * 8),
        TXRootSize:8, TXRoot:TXRootSize/binary, Rest/binary >>, Parsed) ->
    decode_wire(Rest, [{Hash, WeaveSize, TXRoot} | Parsed]);
decode_wire(_Rest, _Parsed) ->
    {error, <<"invalid-block-index-binary">>,
        <<"The binary is not a whole number of block index entries.">>}.

%% @doc Encode entries in the fixed-width form the runs hold. The guards are
%% the format's limits: exceeding either would silently truncate.
encode_entries(Parsed) ->
    << << (encode_entry(Entry))/binary >> || Entry <- Parsed >>.

encode_entry({Hash, WeaveSize, TXRoot})
        when byte_size(Hash) == 48, WeaveSize < (1 bsl 64), byte_size(TXRoot) =< 32 ->
    Padding = (32 - byte_size(TXRoot)) * 8,
    << Hash:48/binary, WeaveSize:64, (byte_size(TXRoot)):8, TXRoot/binary, 0:Padding >>.

decode_entry(<< Hash:48/binary, WeaveSize:64, TXRootSize:8, Padded:32/binary >>) ->
    << TXRoot:TXRootSize/binary, _/binary >> = Padded,
    {Hash, WeaveSize, TXRoot}.

%% @doc The `index'th entry of a run, as a sub-binary.
entry_at(Run, Index) ->
    binary:part(Run, Index * ?ENTRY_SIZE, ?ENTRY_SIZE).

%% @doc The weave size of a run's `index'th entry, without decoding the rest.
weave_size_at(Run, Index) ->
    << WeaveSize:64 >> = binary:part(Run, (Index * ?ENTRY_SIZE) + 48, 8),
    WeaveSize.

run_index_weave_size(RunIndex, RunNumber) ->
    << WeaveSize:64 >> = binary:part(RunIndex, RunNumber * ?RUN_INDEX_SIZE, 8),
    WeaveSize.

%% @doc Confirm the entries continue a non-decreasing weave size from `last'.
monotonic([], _Last) ->
    ok;
monotonic([{_, WeaveSize, _} | _], Last) when WeaveSize < Last ->
    {error, error_message(<<"non-monotonic-weave-size">>,
        <<"An entry's weave size is below its predecessor's.">>)};
monotonic([{_, WeaveSize, _} | Parsed], _Last) ->
    monotonic(Parsed, WeaveSize).

%% @doc Confirm every entry fits the fixed-width form the runs hold. The wire
%% form is wider than the stored one -- a tx root's length is recorded in a
%% byte, so a peer may serve one of up to 255 bytes, and a weave size in as
%% many bytes as the peer likes -- so an entry that parses is not yet an entry
%% that can be stored. `encode_entry/1' guards the same three widths and has
%% no other clause, so without this an entry from an untrusted peer would
%% raise inside the ingest rather than be refused by it.
storable([]) ->
    ok;
storable([{Hash, WeaveSize, TXRoot} | Parsed])
        when byte_size(Hash) == 48, WeaveSize < (1 bsl 64),
                byte_size(TXRoot) =< 32 ->
    storable(Parsed);
storable(_Parsed) ->
    {error, error_message(<<"invalid-block-index-entry">>,
        <<"An entry is wider than the fixed-width form the index holds.">>)}.

%%% Index state accessors. Every one reads a field of the state message with
%%% `hb_maps', never `hb_ao': `root' is both a field of the state and a key of
%%% this device, so resolving it would dispatch back into the device.

entries(Base, Opts) ->
    hb_util:int(hb_maps:get(<<"length">>, Base, 0, Opts)).

run_size(Base, Opts) ->
    hb_util:int(hb_maps:get(<<"run-size">>, Base, ?RUN_SIZE, Opts)).

runs(Base, Opts) ->
    hb_maps:get(<<"runs">>, Base, #{}, Opts).

run_index(Base, Opts) ->
    case hb_maps:get(<<"run-index">>, Base, <<>>, Opts) of
        <<>> -> <<>>;
        ID -> hb_util:ok(hb_cache:read(safe_key(ID), Opts))
    end.

%% @doc Refuse a store key that could be resolved as a path.
%%
%% These keys arrive on a caller-supplied base -- both this device's keys are
%% reachable over HTTP in their own right -- and are passed to `hb_cache:read/2'
%% as store keys, so they are checked before they name anything.
%%
%% `/' is deliberately allowed: this device's own store keys are path-namespaced
%% (`~arweave-block-index@2.9/runs/<n>'), so a separator is ordinary here. A
%% base64url cache id carries neither `..' nor a NUL.
safe_key(Key) when is_binary(Key) ->
    case binary:match(Key, [<<"..">>, <<0>>]) of
        nomatch -> Key;
        _ -> throw({unsafe_store_key, Key})
    end;
safe_key(Key) ->
    % Not a binary, so not a path. Passed through to fail where it would have
    % failed before rather than being turned into a different error here.
    Key.

%% @doc Read one run out of the store.
run(Base, RunNumber, Opts) ->
    hb_util:ok(
        hb_cache:read(
            safe_key(
                hb_maps:get(
                    run_key(RunNumber),
                    runs(Base, Opts),
                    not_found,
                    Opts
                )
            ),
            Opts
        )
    ).

%% @doc The leading `count' entries of a run, which is the part an append or a
%% rollback keeps. Never reads the run when it keeps nothing of it.
tail_run(_Base, _RunNumber, 0, _Opts) ->
    <<>>;
tail_run(Base, RunNumber, Count, Opts) ->
    binary:part(run(Base, RunNumber, Opts), 0, Count * ?ENTRY_SIZE).

%% @doc The root recorded at the end of a run, or the empty root that precedes
%% the first one.
run_root(_Base, RunNumber, _Opts) when RunNumber < 0 ->
    <<>>;
run_root(Base, RunNumber, Opts) ->
    binary:part(run_index(Base, Opts), (RunNumber * ?RUN_INDEX_SIZE) + 8, 48).

%% @doc The weave size of the last entry, which every appended entry must
%% equal or exceed.
last_weave_size(Base, Opts) ->
    case entries(Base, Opts) of
        0 ->
            0;
        Entries ->
            run_index_weave_size(
                run_index(Base, Opts),
                run_count(Entries, run_size(Base, Opts)) - 1
            )
    end.

run_count(Entries, RunSize) ->
    ((Entries + RunSize) - 1) div RunSize.

%% @doc Runs are held in a `structured@1.0' numbered message, so they are
%% one-indexed while heights and run numbers are zero-indexed.
run_key(RunNumber) ->
    hb_util:bin(RunNumber + 1).

run_keys(Count) ->
    [ run_key(RunNumber) || RunNumber <- lists:seq(0, Count - 1) ].

%% @doc Read a key from the request, falling back to the base message.
get_first(Key, Base, Req, Default, Opts) ->
    hb_ao:get_first([{Req, Key}, {Base, Key}], Default, Opts).

%% @doc Read a key that has no meaningful default.
required(Key, Base, Req, Opts) ->
    case get_first(Key, Base, Req, not_found, Opts) of
        not_found -> throw({missing_key, Key});
        Value -> Value
    end.

%% @doc Read a required base64URL key, decoded. Every one of them arrives from
%% a peer, so the checked decoder is used: `hb_util:decode/1' would turn a
%% malformed value into plausible-looking bytes.
decoded(Key, Base, Req, Opts) ->
    case hb_util:safe_decode(required(Key, Base, Req, Opts)) of
        {ok, Value} -> Value;
        {error, _} -> throw({invalid_base64, Key})
    end.

%% @doc Build the standard error body.
error_message(Message, Detail) ->
    #{
        <<"status">> => 422,
        <<"message">> => Message,
        <<"detail">> => Detail
    }.
