%%% @doc Filling a storage module with the weave: the bounded pass that fetches
%%% chunks from peers and stores what they answer with.
%%%
%%% A storage module is a range of the weave, and syncing is the work of
%%% finding the bytes of that range this node does not hold and asking a peer
%%% for them. One pass fetches a bounded number of chunks and stops, so it is
%%% driven by `~cron@1.0/every' exactly as preparation is: an operator sets how
%%% many chunks a pass may take and the passes add up.
%%%
%%% Nothing here writes a chunk. A chunk arrives from a peer as bytes and two
%%% Merkle paths, and where it belongs in the weave is read out of those paths
%%% by `~arweave-storage@2.9/store', against the block index this node
%%% validated itself. This module chooses which byte to ask about and what to
%%% do with the answer; the placement, the validation and every write but the
%%% cursor belong to that key. So a peer cannot name the offset its bytes land
%%% at, which is the whole reason the two are separate: a caller that could
%%% would be able to put any bytes in any slot of a partition this node then
%%% mines.
%%%
%%% The pass keeps its place in a file in the module's own directory, so a node
%%% that restarts does not begin the range again. That place is not the record of
%%% what is held -- the sync record is -- and the two are deliberately
%%% different things. The cursor moves past every byte the pass ASKED about,
%%% including the ones no peer answered for, so a hole in the network's copy of
%%% the weave costs one request per pass rather than every request of every
%%% pass. The record still shows those bytes unsynced, so the pass that wraps
%%% round the end of the range asks again.
%%%
%%% Which peer answers is not decided here. `~arweave@2.9/chunk-proof' routes
%%% by offset through the routes this node is configured with, so a pass asks
%%% the network rather than a peer it chose.
-module(lib_arweave_data_sync).
-export([sync/3, cursor/2, advance/3, missing/3]).
-include("include/hb.hrl").
-include("include/ar.hrl").

%%% The file this node records a module's syncing progress in. The name is this
%%% node's own, and is spelled unlike anything an Arweave node writes into a
%%% storage module.
-define(CURSOR, "hyperbeam_sync_cursor").

%% @doc Fetch and store up to `Chunks' more of a module's range, starting from
%% the first byte at or after the cursor that the module does not hold.
%%
%% Bounded and idempotent. A pass takes no lock of its own: each chunk is
%% written by one `~arweave-storage@2.9/store' resolution, which is where a
%% module's writes are serialised, so two passes over one module interleave
%% chunks rather than corrupting each other. Nothing read before a resolution
%% is assumed to still hold after it -- the record is loaded again for each
%% byte the pass considers. For the same reason a caller must not wrap this in
%% `lib_arweave_storage:exclusive/3': the store resolution queues behind the
%% module's runner, and a pass already running as that runner would be waiting
%% on itself.
%%
%% Answers with what it did, where it stopped and whether it reached the end of
%% the range, which is what tells a caller driving this from `~cron@1.0'
%% whether there is more to do.
sync(Module, Chunks, Opts) ->
    {Start, End} = lib_arweave_storage:range(Module),
    State =
        #{
            <<"module">> => Module,
            <<"range-start">> => Start,
            <<"range-end">> => End,
            <<"stored">> => 0,
            <<"attempted">> => 0
        },
    chunks(Chunks, cursor(Module, Opts), State, Opts).

%% @doc The byte a module's syncing has reached. A module that has never been
%% synced starts at the first byte of its own range.
cursor(Module, Opts) ->
    {Start, _End} = lib_arweave_storage:range(Module),
    case file:read_file(path(Module, Opts)) of
        {ok, Bin} -> stored_cursor(catch binary_to_term(Bin, [safe]), Start);
        {error, _} -> Start
    end.

%% @doc Record that syncing has reached a byte. The encoding is the one an
%% Arweave node writes its own cursors in, so the two files read alike.
%%
%% A pass that ends where the file already stands writes nothing. A module that
%% holds the whole of its range wraps to the start of it on every pass, and a
%% pass is scheduled every second: without this the place a pass never moves
%% from would be rewritten into the operator's data directory for as long as
%% the node runs.
advance(Module, Cursor, Opts) ->
    advance(Module, Cursor, cursor(Module, Opts), Opts).
advance(_Module, Cursor, Cursor, _Opts) ->
    ok;
advance(Module, Cursor, _Reached, Opts) ->
    maybe
        ok ?= lib_arweave_chunks:ensure_dir(Module, Opts),
        saved(file:write_file(path(Module, Opts), term_to_binary(Cursor)))
    end.

%% @doc The first `Limit' spans of a module's range that it does not hold,
%% oldest first, for an operator asking what is left to sync. Each span is the
%% record's own interval: the start is excluded and the end included.
missing(Module, Limit, Opts) ->
    {Start, End} = lib_arweave_storage:range(Module),
    maybe
        {ok, Records} ?= lib_arweave_sync_record:load(Module, Opts),
        {ok, gaps(Records, Limit, Start, End, [])}
    end.

%%% Internal functions.

%% @doc Attempt one byte at a time until the pass has attempted the number of
%% chunks it was given, or the module's range holds nothing more to ask for.
chunks(0, Cursor, State, Opts) ->
    done(Cursor, false, State, Opts);
chunks(Chunks, Cursor, State, Opts) ->
    maybe
        {ok, Records} ?=
            lib_arweave_sync_record:load(field(<<"module">>, State), Opts),
        unsynced(Chunks, gap(Records, Cursor, State), State, Opts)
    end.

%% @doc The next span of a module's range that it does not hold, at or above
%% the cursor and below the end of the range.
gap(Records, Cursor, State) ->
    lib_arweave_sync_record:next_unsynced(
        Records, ar_data_sync, Cursor, field(<<"range-end">>, State)).

%% @doc Attempt the first byte of the next gap in the module's range, or wrap
%% to the start of the range when there is none below its end.
%%
%% The gap's start is the last byte the record holds, which is the first byte
%% it does not: the record's intervals exclude their start and include their
%% end, so a gap of `{End, Start}' is the bytes `Start + 1 .. End' in the
%% record's own numbering and the byte `Start' in the numbering a chunk is
%% asked for by.
unsynced(_Chunks, not_found, State, Opts) ->
    done(field(<<"range-start">>, State), true, State, Opts);
unsynced(Chunks, {_GapEnd, Byte}, State, Opts) ->
    attempt(Chunks, Byte, State, Opts).

%% @doc Ask about one byte and carry on from beyond whatever it turned out to
%% cover.
attempt(Chunks, Byte, State, Opts) ->
    maybe
        {ok, Placed} ?= fetched(Byte, State, Opts),
        chunks(Chunks - 1, next(Byte, Placed), counted(Placed, State), Opts)
    end.

%% @doc Fetch the chunk of the weave holding a byte and store it, answering
%% with the offset it was written at or `not_found' when nothing was written.
%%
%% A peer that does not answer for a byte is not a failure of the pass: it is a
%% byte the network could not serve, which is the ordinary state of a weave
%% being synced. A peer that answers in a packing this node did not ask for is
%% a different thing and is refused by name, because bytes in an unknown
%% packing written into a module's slots are bytes nothing on the weave
%% accepts, and there is no offset to step over that would make that safe.
fetched(Byte, State, Opts) ->
    Request = #{ <<"path">> => <<"chunk-proof">>, <<"offset">> => Byte },
    case hb_ao:resolve(weave(Opts), Request, Opts) of
        {ok, Answer} -> proven(Byte, Answer, State, Opts);
        {error, Error} -> missed(Byte, Error, Opts)
    end.

%% @doc Store a chunk a peer answered with, once its packing is one this node
%% takes from a peer at all.
proven(Byte, Answer, State, Opts) ->
    maybe
        ok ?= accepted(hb_maps:get(<<"packing">>, Answer, <<>>, Opts), Byte),
        stored(Byte, Answer, State, Opts)
    end.

%% @doc Hold only for the packing a peer serves the weave in. This node asks
%% for no packing, so a peer answers `unpacked' or refuses; anything else is a
%% peer answering a question it was not asked.
accepted(<<"unpacked">>, _Byte) ->
    ok;
accepted(Packing, Byte) ->
    {error, error_message(422, <<"unsupported-packing">>,
        <<"A peer answered for byte ", (hb_util:bin(Byte))/binary,
            " with a `", (hb_util:bin(Packing))/binary,
            "' chunk; this node syncs `unpacked' chunks.">>)}.

%% @doc Hand a peer's answer to the key that places and writes it. Only the
%% bytes, the two paths and the byte they were asked for are passed on: every
%% offset the chunk is written at is derived from the paths there, against this
%% node's own block index.
stored(Byte, Answer, State, Opts) ->
    Request =
        #{
            <<"device">> => <<"arweave-storage@2.9">>,
            <<"chunk">> => hb_maps:get(<<"chunk">>, Answer, <<>>, Opts),
            <<"tx-path">> => hb_maps:get(<<"tx-path">>, Answer, <<>>, Opts),
            <<"data-path">> => hb_maps:get(<<"data-path">>, Answer, <<>>, Opts),
            <<"offset">> => Byte
        },
    case hb_ao:resolve(Request, <<"store">>, Opts) of
        {ok, Result} -> written(Byte, Result, State, Opts);
        {error, Error} -> refused(Byte, Error, Opts)
    end.

%% @doc Read where a stored chunk landed. A chunk whose offset another module
%% of this node covers is written into that module, and is not this pass's to
%% count: the pass steps over the byte and this module's record still shows it
%% unsynced, which is the truth about this module.
written(Byte, Result, State, Opts) ->
    Id = hb_util:bin(lib_arweave_storage:id(field(<<"module">>, State))),
    PaddedEndOffset =
        hb_util:int(hb_maps:get(<<"padded-end-offset">>, Result, 0, Opts)),
    case hb_maps:get(<<"module">>, Result, not_found, Opts) of
        Id ->
            ?event(arweave_data_sync,
                {chunk_stored,
                    {module, {string, Id}},
                    {byte, Byte},
                    {padded_end_offset, PaddedEndOffset}
                },
                Opts
            ),
            {ok, PaddedEndOffset};
        Other ->
            ?event(arweave_data_sync,
                {chunk_stored_elsewhere,
                    {module, {string, Id}},
                    {byte, Byte},
                    {stored_in, {string, hb_util:bin(Other)}}
                },
                Opts
            ),
            {ok, not_found}
    end.

%% @doc Note a byte no peer answered for. The pass steps over it.
missed(Byte, Error, Opts) ->
    ?event(arweave_data_sync,
        {chunk_unavailable, {byte, Byte}, {error, Error}},
        Opts
    ),
    {ok, not_found}.

%% @doc Note a chunk this node would not write. A proof that does not place the
%% chunk where it was asked for, or an offset no module of this node covers,
%% leaves the byte unsynced exactly as an unanswered request does.
refused(Byte, Error, Opts) ->
    ?event(arweave_data_sync,
        {chunk_refused, {byte, Byte}, {error, Error}},
        Opts
    ),
    {ok, not_found}.

%% @doc The byte a pass carries on from after attempting one.
%%
%% A chunk that was written names the offset its slot ends at, which is the
%% byte the next chunk of the weave begins at. A byte nothing was written for
%% steps one chunk's width: asking for it again would spend the whole pass on
%% it, and the record shows it unsynced for the pass that comes back round.
next(Byte, not_found) -> Byte + ?DATA_CHUNK_SIZE;
next(_Byte, PaddedEndOffset) -> PaddedEndOffset.

%% @doc Count what an attempt did: every byte the pass asked about, and every
%% chunk it wrote.
counted(not_found, State) ->
    State#{ <<"attempted">> => field(<<"attempted">>, State) + 1 };
counted(_PaddedEndOffset, State) ->
    (counted(not_found, State))#{
        <<"stored">> => field(<<"stored">>, State) + 1
    }.

%% @doc Save the pass's place and answer with what it did.
%%
%% The place is saved last, and it is the only thing this module writes. A
%% crash before it repeats the pass, which is a repeat of requests the store
%% already answers idempotently; a place saved for chunks that were never
%% fetched would be a range this node walked past without ever asking for.
done(Cursor, Wrapped, State, Opts) ->
    Module = field(<<"module">>, State),
    maybe
        ok ?= advance(Module, Cursor, Opts),
        {ok, Records} ?= lib_arweave_sync_record:load(Module, Opts),
        ?event(arweave_data_sync,
            {pass_complete,
                {module, {string, hb_util:bin(lib_arweave_storage:id(Module))}},
                {chunks, field(<<"stored">>, State)},
                {attempted, field(<<"attempted">>, State)},
                {cursor, Cursor},
                {wrapped, Wrapped}
            },
            Opts
        ),
        {ok,
            #{
                <<"chunks">> => field(<<"stored">>, State),
                <<"attempted">> => field(<<"attempted">>, State),
                <<"cursor">> => Cursor,
                <<"wrapped">> => Wrapped,
                <<"synced">> =>
                    lib_arweave_sync_record:size(Records, ar_data_sync)
            }
        }
    end.

%% @doc Walk the gaps of a module's range from its start, taking the first
%% `Limit' of them. Every gap ends strictly above where it began, so the walk
%% reaches the end of the range whatever the record holds.
gaps(_Records, 0, _Offset, _End, Found) ->
    lists:reverse(Found);
gaps(Records, Limit, Offset, End, Found) ->
    case
        lib_arweave_sync_record:next_unsynced(
            Records, ar_data_sync, Offset, End)
    of
        not_found ->
            lists:reverse(Found);
        {GapEnd, GapStart} ->
            gaps(
                Records,
                Limit - 1,
                GapEnd,
                End,
                [#{ <<"start">> => GapStart, <<"end">> => GapEnd } | Found]
            )
    end.

%% @doc The source a pass fetches chunks from. `~arweave@2.9' asks the peers
%% this node routes to; `arweave-weave' names another message answering the
%% same `chunk-proof' key, which is how a pass is pointed at one weave rather
%% than at the network's.
weave(Opts) ->
    hb_opts:get(
        <<"arweave-weave">>, #{ <<"device">> => <<"arweave@2.9">> }, Opts).

%% @doc The file a module's cursor is kept in: the module's own directory,
%% beside `chunk_storage' rather than inside it. The cursor is this node's
%% alone -- unlike the preparation cursor, which is the Arweave node's file in
%% the Arweave node's format -- and a directory an Arweave node fills should
%% carry nothing of ours among the chunk files it wrote.
path(Module, Opts) ->
    filename:join(lib_arweave_storage:module_path(Module, Opts), ?CURSOR).

%% @doc Read a stored cursor, falling back to the start of the module for a
%% file that holds anything else.
stored_cursor(Cursor, _Default) when is_integer(Cursor) -> Cursor;
stored_cursor(_Other, Default) -> Default.

%% @doc Report a cursor that could not be written. The pass did its work; what
%% failed is this node's own disk, and a caller that was told the pass
%% succeeded would run it again from the same byte.
saved(ok) ->
    ok;
saved({error, Reason}) ->
    {error,
        error_message(
            500,
            <<"sync-cursor-unwritable">>,
            hb_util:bin(io_lib:format("~p", [Reason]))
        )
    }.

%% @doc Read a field of the pass state.
field(Key, State) ->
    maps:get(Key, State).

%% @doc Build the standard error body.
error_message(Status, Message, Detail) ->
    #{
        <<"status">> => Status,
        <<"message">> => Message,
        <<"detail">> => Detail
    }.
