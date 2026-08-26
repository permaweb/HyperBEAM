%%% @doc A streaming walk of the ANS-104 bundles in a span of the weave,
%%% touching header bytes and skipping data.
%%%
%%% The scan is given each L1 transaction's absolute start offset and data
%%% size (`lib_arweave_index_manifest') and a reader over the module's chunk
%%% files (`lib_arweave_index_read'). For each transaction it reads the
%%% bundle's item-count table, then each item's header -- signature, owner,
%%% target, anchor, tags -- and emits the published-index rows for it through
%%% the caller's sink. The bytes between one item's header and the next
%%% item's start are never requested, so a module dense with large items
%%% costs a fraction of its size in reads, while a module dense with small
%%% items is read whole, sequentially.
%%%
%%% Parsing is `ar_bundles:deserialize_header/1' over a window of the item:
%%% the scanner indexes exactly what the AO-Core write path would have
%%% parsed, by construction. A window that proves too small for the tags is
%%% regrown geometrically and reparsed; an item whose header does not parse
%%% at its full extent is malformed on chain and is counted and skipped, as
%%% are items lost to holes in the module. Item IDs are recomputed as
%%% `sha256(signature)' -- the table's claimed IDs are not believed.
%%%
%%% An item whose tags name it a bundle (`bundle-format: binary',
%%% `bundle-version: 2.0.0', names case-insensitive, values lower-cased, as
%%% `ar_tx:type/1' reads them) and whose data passes the same structural
%%% table checks is recursed into, its children's `bundled-in' being the
%%% enclosing item's ID. RedStone oracle items are dropped before their
%%% signature is ever hashed. The item-count table is believed only within
%%% bounds: a count that does not fit its transaction, or a table whose item
%%% sizes overrun it, marks the transaction as not a bundle.
-module(lib_arweave_index_scan).
-export([open/3, tx/2, finish/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The window sizes an item header is attempted at, in bytes. Nearly every
%%% real item's header -- signature, owner, flags and tags -- fits the first;
%%% the tail of the ladder exists for tag sections of pathological size. An
%%% item whose header outgrows the last rung is counted malformed.
-define(WINDOWS, [4096, 65536, 1048576, 16777216]).

%%% The largest item count a table is believed to hold. 4M items of 64 bytes
%%% is a 256 MiB table, held in memory while its transaction is walked.
-define(MAX_ITEMS, 4194304).

%%% How deep nested bundles are followed.
-define(MAX_DEPTH, 8).

%% @doc A scan state over a reader and a row sink. The sink is
%% `fun(OffsetItem, MatchItems, SinkState) -> SinkState', receiving one
%% encoded 21-byte offset item (or `excluded') and the item's 17-byte match
%% items per data item scanned.
open(Reader, Sink, SinkState) ->
    #{
        <<"reader">> => Reader,
        <<"sink">> => Sink,
        <<"sink-state">> => SinkState,
        <<"counts">> => #{}
    }.

%% @doc Scan one L1 transaction: `start' and `size' place its data in the
%% weave; `id' (when known) becomes its top-level items' `bundled-in';
%% `bundle' set false skips it without a read. Only reader failures are
%% errors -- a transaction that is not a bundle, a malformed item, a hole in
%% the module are all counted and stepped over.
tx(#{ <<"bundle">> := false }, State) ->
    {ok, counted(<<"txs-skipped">>, State)};
tx(Spec = #{ <<"start">> := Start, <<"size">> := Size },
        State = #{ <<"reader">> := Reader }) ->
    Parent =
        case maps:get(<<"id">>, Spec, undefined) of
            undefined -> undefined;
            ID -> hb_util:human_id(ID)
        end,
    % The transaction's extent bounds every read it can cause, so the
    % reader clips its batches and read-ahead to it.
    Limited =
        State#{
            <<"reader">> =>
                lib_arweave_index_read:limit(Start + Size, Reader)
        },
    maybe
        {ok, State2} ?= bundle(Start, Size, Parent, 0, Limited),
        {ok, counted(<<"txs">>, State2)}
    end.

%% @doc The final sink state, the reader as the scan left it, and the scan's
%% counters.
finish(State) ->
    #{
        <<"sink-state">> := SinkState,
        <<"reader">> := Reader,
        <<"counts">> := Counts
    } = State,
    {SinkState, Reader, Counts}.

%%% Internal functions.

%% @doc Walk one bundle: the item-count table, then every item. Depth names
%% how many bundles enclose this one; the L1 transaction itself is depth 0.
bundle(Start, Size, Parent, Depth, State) ->
    case table(Start, Size, State) of
        {ok, Sizes, State2} ->
            ItemsStart = Start + 32 + 64 * length(Sizes),
            items(ItemsStart, Sizes, Parent, Depth, State2);
        {skip, Reason, State2} ->
            {ok, counted(skip_count(Depth, Reason), State2)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Read and validate a bundle's item-count table, returning the item
%% sizes in order. A table that does not fit, a count out of bounds, or item
%% sizes overrunning the payload mark the payload as not a bundle.
table(_Start, Size, State) when Size < 32 + 64 ->
    {skip, <<"no-table">>, State};
table(Start, Size, State) ->
    case bytes(Start, 32, State) of
        {ok, << Count:256/little >>, State2} ->
            table_entries(Start, Size, Count, State2);
        {short, State2} ->
            {skip, <<"table-hole">>, State2};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Read the table's entries once its count is known to fit.
table_entries(Start, Size, Count, State)
        when Count > 0 andalso Count =< ?MAX_ITEMS
        andalso 32 + 64 * Count =< Size ->
    case bytes(Start + 32, 64 * Count, State) of
        {ok, Table, State2} ->
            Sizes =
                [ItemSize || << ItemSize:256/little, _ID:32/binary >> <= Table],
            Fit =
                lists:all(fun(ItemSize) -> ItemSize > 0 end, Sizes)
                    andalso 32 + 64 * Count + lists:sum(Sizes) =< Size,
            case Fit of
                true -> {ok, Sizes, State2};
                false -> {skip, <<"sizes-invalid">>, State2}
            end;
        {short, State2} ->
            {skip, <<"table-hole">>, State2};
        {error, Reason} ->
            {error, Reason}
    end;
table_entries(_Start, _Size, _Count, State) ->
    {skip, <<"count-invalid">>, State}.

%% @doc Walk a bundle's items in payload order.
items(_Pos, [], _Parent, _Depth, State) ->
    {ok, State};
items(Pos, [Size | Sizes], Parent, Depth, State) ->
    maybe
        {ok, State2} ?= item(Pos, Size, Parent, Depth, State),
        items(Pos + Size, Sizes, Parent, Depth, State2)
    end.

%% @doc Parse one item's header and emit its rows, growing the parse window
%% as the tag section demands.
item(Pos, Size, Parent, Depth, State) ->
    windowed(Pos, Size, Parent, Depth, ?WINDOWS, State).

windowed(Pos, Size, Parent, Depth, [Window | Windows], State) ->
    Take = min(Size, Window),
    case bytes(Pos, Take, State) of
        {ok, Bin, State2} ->
            case header(Bin) of
                {ok, HeaderSize, TX} ->
                    emit(TX, HeaderSize, Pos, Size, Parent, Depth, State2);
                failed when Take < Size andalso Windows /= [] ->
                    windowed(Pos, Size, Parent, Depth, Windows, State2);
                failed ->
                    {ok, counted(<<"items-malformed">>, State2)}
            end;
        {short, State2} ->
            {ok, counted(<<"items-in-holes">>, State2)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Deserialize an item header through `ar_bundles', reporting any parse
%% failure -- a window ending mid-field, an unsupported signature type, a tag
%% section that does not decode -- as `failed' for the caller to size up or
%% count. The try is confined to the vendored parser, whose interface for
%% foreign bytes is to throw.
header(Bin) ->
    try ar_bundles:deserialize_header(Bin)
    catch
        throw:{invalid_ans104_tags, _} -> failed;
        error:_ -> failed
    end.

%% @doc Emit one parsed item's rows and recurse into it if it is itself a
%% bundle. RedStone items produce no rows and are never bundles.
emit(TX, HeaderSize, Pos, Size, Parent, Depth, State) ->
    #tx{ signature = Signature, tags = Tags } = TX,
    case lib_arweave_index_rows:redstone(Tags) of
        true ->
            {ok, counted(<<"items-redstone">>, State)};
        false ->
            ID = crypto:hash(sha256, Signature),
            State2 = sunk(TX, ID, Pos, Size, Parent, State),
            nested(TX, ID, Pos + HeaderSize, Size - HeaderSize, Depth,
                counted(<<"items">>, State2))
    end.

%% @doc Push one item's rows through the sink.
sunk(TX, ID, Pos, Size, Parent, State) ->
    #{ <<"sink">> := Sink, <<"sink-state">> := SinkState } = State,
    OffsetItem =
        lib_arweave_index_rows:offset_item(ID, <<"ans104@1.0">>, Pos, Size),
    MatchItems = lib_arweave_index_rows:match_rows(header_map(TX, ID, Parent), Pos),
    State#{ <<"sink-state">> => Sink(OffsetItem, MatchItems, SinkState) }.

%% @doc The parsed fields the row builder draws predicates from.
header_map(TX, _ID, Parent) ->
    #tx{
        owner = Owner,
        signature_type = SigType,
        target = Target,
        tags = Tags
    } = TX,
    Base =
        #{
            <<"tags">> => Tags,
            <<"owner-address">> =>
                hb_util:human_id(ar_wallet:to_address(Owner, SigType))
        },
    WithTarget =
        case Target of
            <<>> -> Base;
            _ -> Base#{ <<"recipient">> => hb_util:human_id(Target) }
        end,
    case Parent of
        undefined -> WithTarget;
        _ -> WithTarget#{ <<"bundled-in">> => Parent }
    end.

%% @doc Recurse into an item that is itself a bundle. The tag reading is
%% `ar_tx:type/1''s: both bundle tags present, names case-insensitive,
%% values lower-cased -- except that the data here is raw weave bytes, so
%% the structural table check stands in for `decode_bundle_header'.
nested(TX, ID, DataPos, DataSize, Depth, State) ->
    Format = ar_tx:tagfind(<<"bundle-format">>, TX#tx.tags, <<>>),
    Version = ar_tx:tagfind(<<"bundle-version">>, TX#tx.tags, <<>>),
    IsBundle =
        {hb_util:to_lower(Format), hb_util:to_lower(Version)}
            == {<<"binary">>, <<"2.0.0">>},
    case IsBundle andalso Depth < ?MAX_DEPTH of
        false when IsBundle ->
            {ok, counted(<<"bundles-too-deep">>, State)};
        false ->
            {ok, State};
        true ->
            bundle(
                DataPos,
                DataSize,
                hb_util:human_id(ID),
                Depth + 1,
                counted(<<"bundles-nested">>, State)
            )
    end.

%% @doc Read a range of the weave through the scan's reader.
bytes(Offset, Len, State = #{ <<"reader">> := Reader }) ->
    case lib_arweave_index_read:read(Offset, Len, Reader) of
        {ok, Bin, Reader2} -> {ok, Bin, State#{ <<"reader">> => Reader2 }};
        {short, Reader2} -> {short, State#{ <<"reader">> => Reader2 }};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Step one of the scan's counters.
counted(Key, State = #{ <<"counts">> := Counts }) ->
    State#{ <<"counts">> => maps:update_with(Key, fun(N) -> N + 1 end, 1, Counts) }.

%% @doc The counter a skipped payload steps: an L1 transaction that is not a
%% bundle is expected; a nested one that fails its table is a malformed item.
skip_count(0, _Reason) -> <<"txs-not-bundle">>;
skip_count(_Depth, _Reason) -> <<"bundles-nested-invalid">>.
