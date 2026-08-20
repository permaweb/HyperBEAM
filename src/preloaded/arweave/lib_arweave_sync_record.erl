%%% @doc The record of which bytes of the weave a storage module holds, and in
%%% which packing.
%%%
%%% A record is a set of non-overlapping intervals of absolute weave offsets.
%%% The left bound of an interval is excluded and the right bound included, so
%%% a record holding `{1, 0}' holds the first byte of the weave:
%%% `is_recorded/3' is false at 0 and true at 1. The interval algebra is the
%%% vendored `ar_intervals', which is the algebra an Arweave node uses, so a
%%% range recorded here means what the same range means there.
%%%
%%% Records are named by an id, which is either an atom -- `ar_data_sync' for
%%% the bytes that are synced, `ar_chunk_storage' for the buckets a chunk file
%%% holds -- or an `{Atom, Packing}' pair naming the same bytes in one packing.
%%% `add/5' writes both, which is how a reader asks "is this offset synced, and
%%% in the packing this node can mine from". The id is data: it is read from
%%% the store rather than chosen from a list this module knows.
%%%
%%% An Arweave node keeps this in a process fronted by a write-ahead log. Here
%%% a bounded pass loads the records once, changes them in memory, writes the
%%% chunk bytes, and saves the records last. Losing a record for bytes that are
%%% on disk costs a re-sync, which is idempotent; gaining one for bytes that
%%% are not on disk is a claim to serve data this node does not have. So every
%%% function here but `load/2' and `save/3' is pure, and the save is the last
%%% thing a pass does.
%%%
%%% One store key per record, under the storage module's own store:
%%%
%%%     `~arweave@2.9/storage/<StoreID>/sync-record/<Label>'
%%%
%%% `<Label>' is `label/1' of the id: for an atom, the atom's name; for an
%%% `{Atom, Packing}' pair, the atom's name, a `.', and the packing in the
%%% spelling `ar_serialize:encode_packing/2' gives it -- `unpacked',
%%% `unpacked_padded', `replica_2_9_<address>', `spora_2_6_<address>' or
%%% `composite_<address>.<difficulty>', with the address base64url. Labels
%%% carry no separator, so the one group lists its records and nothing else.
%%%
%%% The value is the intervals as fixed-width big-endian pairs, ascending by
%%% start offset:
%%%
%%%     `<< <<End:64, Start:64>> || {End, Start} <- ar_intervals:to_list(I) >>'
%%%
%%% Sixteen bytes an interval, so a value of any other length is one this node
%%% did not write, and it is refused rather than read as a range this node
%%% would then claim to hold.
-module(lib_arweave_sync_record).
-export([load/2, save/3, ids/1]).
-export([add/4, add/5, delete/4, cut/3]).
-export([is_recorded/3, is_recorded/4, interval/3, next_synced/4, next_unsynced/4,
    intervals/2, count/2, size/2]).
-export([label/1, parse_label/1]).
-include("include/hb.hrl").

%%% The group every record of one storage module is filed under.
-define(STORAGE, <<"~arweave@2.9/storage">>).
-define(RECORDS, <<"sync-record">>).
%%% The width on disk of one interval: a 64-bit end offset and a 64-bit start.
-define(ENTRY_SIZE, 16).

%% @doc Read every record the given storage module has.
%%
%% A module that has never had a record written to it has no group to list, so
%% it loads as `{ok, #{}}' rather than as a failure. A record that is present
%% but cannot be read is an error: a pass that saved without it would erase
%% ranges that are really on disk. The record set is wrapped so that a caller's
%% `?=' propagates that error rather than binding it as a set.
load(Module, Opts) ->
    Store = lib_arweave_storage:store(Opts),
    load(Store, group(Module), labels(Store, group(Module), Opts), #{}, Opts).

%% @doc Write every record of the given set back to the storage module's store.
save(Module, Records, Opts) ->
    Store = lib_arweave_storage:store(Opts),
    maybe
        ok ?= hb_store:group(Store, group(Module), Opts),
        ok ?= hb_store:write(Store, writes(group(Module), Records), Opts),
        ?event(arweave_storage,
            {sync_records_saved,
                {module, lib_arweave_storage:id(Module)},
                {ids, ids(Records)}
            },
            Opts
        ),
        ok
    end.

%% @doc The ids of the records in the given set, in a stable order.
ids(Records) ->
    lists:sort(maps:keys(Records)).

%% @doc Record the bytes `Start + 1 .. End' against one id.
%%
%% A range that begins before the weave does is refused rather than recorded.
%% `ar_intervals' asks only that the end exceed the start, so a negative start
%% is added happily, written to disk as the unsigned value it wraps to, and
%% refused on every load afterwards -- a caller that miscomputed one offset
%% would leave the module unreadable rather than wrong by one chunk.
add(_Records, Id, End, Start) when Start < 0 ->
    throw({'invalid-sync-record-interval', Id, End, Start});
add(Records, Id, End, Start) ->
    Records#{ Id => ar_intervals:add(intervals(Records, Id), End, Start) }.

%% @doc Record the bytes against both the id and the id in the given packing.
%% A reader asking what is synced reads the first; a reader asking what is
%% synced in a packing it can use reads the second.
add(Records, Id, Packing, End, Start) ->
    add(add(Records, Id, End, Start), {Id, Packing}, End, Start).

%% @doc Remove the bytes from an id and from every packing of it. The bytes are
%% gone whatever they were packed as, so a packing-tagged record that kept them
%% would answer for data that is no longer there.
delete(Records, Id, End, Start) ->
    each_tagged(
        Records#{ Id => ar_intervals:delete(intervals(Records, Id), End, Start) },
        Id,
        fun(Intervals) -> ar_intervals:delete(Intervals, End, Start) end
    ).

%% @doc Remove everything strictly above the given offset from an id and from
%% every packing of it. This is what a reorganisation shortening the weave
%% leaves behind.
cut(Records, Id, Offset) ->
    each_tagged(
        Records#{ Id => ar_intervals:cut(intervals(Records, Id), Offset) },
        Id,
        fun(Intervals) -> ar_intervals:cut(Intervals, Offset) end
    ).

%% @doc Is the given offset held by the record with the given id? The offset is
%% 1-based: the first byte of the weave is offset 1.
is_recorded(Records, Id, Offset) ->
    ar_intervals:is_inside(intervals(Records, Id), Offset).

%% @doc Is the given offset held by the record with the given id, in the given
%% packing?
is_recorded(Records, Id, Packing, Offset) ->
    is_recorded(Records, {Id, Packing}, Offset).

%% @doc The interval holding the given offset, or `not_found'. The right bound
%% is included and the left bound excluded, so an offset equal to an interval's
%% start belongs to whatever precedes it, not to that interval.
interval(Records, Id, Offset) ->
    case above(intervals(Records, Id), Offset - 1) of
        none -> not_found;
        {End, Start} when Offset > Start -> {End, Start};
        _Interval -> not_found
    end.

%% @doc The lowest recorded interval whose end offset is strictly above the
%% given offset and whose start is below `RightBound', clipped to that bound.
%% `not_found' if there is none. `RightBound' may be `infinity'.
next_synced(_Records, _Id, Offset, RightBound) when Offset >= RightBound ->
    not_found;
next_synced(Records, Id, Offset, RightBound) ->
    case above(intervals(Records, Id), Offset) of
        none -> not_found;
        {_End, Start} when Start >= RightBound -> not_found;
        {End, Start} -> {min(End, RightBound), Start}
    end.

%% @doc The lowest gap between the recorded intervals starting at or above the
%% given offset, clipped to `RightBound'. `not_found' when the offset is
%% already at or above the bound; the whole span when nothing above the offset
%% is recorded. `RightBound' may be `infinity'.
next_unsynced(Records, Id, Offset, RightBound) ->
    unsynced(intervals(Records, Id), Offset, RightBound).

%% @doc The intervals recorded against one id. An id with nothing recorded has
%% an empty set, which every query here answers from without a special case.
intervals(Records, Id) ->
    maps:get(Id, Records, ar_intervals:new()).

%% @doc How many intervals one record holds.
count(Records, Id) ->
    ar_intervals:count(intervals(Records, Id)).

%% @doc How many bytes one record holds.
size(Records, Id) ->
    ar_intervals:sum(intervals(Records, Id)).

%% @doc The name one record is filed under.
label(Id) when is_atom(Id) ->
    hb_util:bin(Id);
label({Id, Packing}) ->
    << (hb_util:bin(Id))/binary, ".", (packing_label(Packing))/binary >>.

%% @doc Read the id a label names, or `not_found' if it names none.
parse_label(Label) ->
    case binary:split(Label, <<".">>) of
        [Name] -> name(Name);
        [Name, Packing] -> tagged(name(Name), parse_packing(Packing))
    end.

%%% Internal functions.

%% @doc The labels of the records one store holds. The group is listed rather
%% than each key guessed, because the ids are data.
%%
%% Every shape but `{ok, Labels}' is a store holding no records: `hb_store'
%% ends a listing no store answered with `{error, not_found}', `{error, _}' or
%% `{failure, _}' depending on how each declined, and a store that has had
%% nothing written to it has no group to list at all. `hb_util:ok_or/2' is
%% total over all of them.
labels(Store, Group, Opts) ->
    hb_util:ok_or(hb_store:list(Store, Group, Opts), []).

%% @doc Read each listed record into the set, stopping at the first that cannot
%% be read.
load(_Store, _Group, [], Records, _Opts) ->
    {ok, Records};
load(Store, Group, [Label | Rest], Records, Opts) ->
    maybe
        {ok, Id, Intervals} ?= record(Store, Group, Label, Opts),
        load(Store, Group, Rest, Records#{ Id => Intervals }, Opts)
    end.

%% @doc Read one record: the id its label names and the intervals it holds.
record(Store, Group, Label, Opts) ->
    maybe
        {ok, Id} ?= id(Label),
        {ok, Value} ?= read(Store, Group, Label, Opts),
        {ok, Intervals} ?= decode(Label, Value),
        {ok, Id, Intervals}
    end.

%% @doc The id a label names. An id is written by the pass that records it, so
%% a label naming an id this node has no name for came from other software and
%% covers bytes this node cannot answer for.
id(Label) ->
    case parse_label(Label) of
        not_found ->
            {error, error_message(<<"sync-record-unknown-id">>, Label)};
        Id ->
            {ok, Id}
    end.

%% @doc Read the stored bytes of one record. The store's own answer is a term
%% of the store layer's shape rather than anything a caller can act on, so it
%% goes to the event stream and the label alone names the record that failed.
read(Store, Group, Label, Opts) ->
    case hb_store:read(Store, key(Group, Label), Opts) of
        {ok, Value} when is_binary(Value) ->
            {ok, Value};
        Other ->
            ?event(warning,
                {sync_record_unreadable, {label, Label}, {result, Other}},
                Opts
            ),
            {error, error_message(<<"sync-record-unreadable">>, Label)}
    end.

%% @doc The store key one record is written at.
key(Group, Label) ->
    hb_path:to_binary([Group, Label]).

%% @doc The group one module's records are filed under. The module names itself
%% in the key rather than only in the store it is written to, because
%% `arweave-storage-index' lets an operator keep every module's records in one
%% store -- and two modules record the same offsets wherever their ranges
%% overlap.
group(Module) ->
    hb_path:to_binary(
        [?STORAGE, hb_util:bin(lib_arweave_storage:id(Module)), ?RECORDS]).

%% @doc The stored bytes of every record, as one write batch.
writes(Group, Records) ->
    maps:from_list(
        [
            {key(Group, label(Id)), encode(Intervals)}
        ||
            {Id, Intervals} <- maps:to_list(Records)
        ]
    ).

%% @doc One record's intervals as fixed-width offset pairs, ascending.
encode(Intervals) ->
    << <<End:64, Start:64>> || {End, Start} <- ar_intervals:to_list(Intervals) >>.

%% @doc Read one record's intervals back. A value that is not a whole number of
%% pairs, or that carries an interval with no bytes in it, was not written by
%% this node.
decode(Label, Value) when byte_size(Value) rem ?ENTRY_SIZE =/= 0 ->
    {error, error_message(<<"sync-record-malformed">>, Label)};
decode(Label, Value) ->
    decode(Label, Value, ar_intervals:new()).

decode(_Label, <<>>, Intervals) ->
    {ok, Intervals};
decode(Label, << End:64, Start:64, Rest/binary >>, Intervals) when End > Start ->
    decode(Label, Rest, ar_intervals:add(Intervals, End, Start));
decode(Label, _Value, _Intervals) ->
    {error, error_message(<<"sync-record-malformed">>, Label)}.

%% @doc Apply a change to every packing-tagged record of one id.
each_tagged(Records, Id, Change) ->
    maps:map(
        fun
            ({Tagged, _Packing}, Intervals) when Tagged == Id ->
                Change(Intervals);
            (_Key, Intervals) ->
                Intervals
        end,
        Records
    ).

%% @doc The lowest interval whose end offset is strictly above the given one,
%% or `none'. Every query below is built from this: the intervals are ordered
%% by end offset and do not overlap, so the first one past a point is the only
%% one that can hold it or bound the gap before it.
above(Intervals, Offset) ->
    % The pair `{Offset, Offset}' is below every interval whose end offset is
    % above `Offset' and above every one whose end offset is at or below it,
    % because an interval's start is always below its end.
    Iterator = ar_intervals:iterator_from({Offset, Offset}, Intervals),
    case ar_intervals:next(Iterator) of
        none -> none;
        {Interval, _Iterator} -> Interval
    end.

%% @doc Walk to the first gap at or above the given offset. An offset inside a
%% recorded interval moves to that interval's end and looks again, which is
%% where the bound is re-checked -- a gap that begins at or beyond the bound is
%% not a gap this caller asked about.
unsynced(_Intervals, Offset, RightBound) when Offset >= RightBound ->
    not_found;
unsynced(Intervals, Offset, RightBound) ->
    case above(Intervals, Offset) of
        none -> {RightBound, Offset};
        {_End, Start} when Start > Offset -> {min(RightBound, Start), Offset};
        {End, _Start} -> unsynced(Intervals, End, RightBound)
    end.

%% @doc Name a packing as it is spelled in a label. This is the spelling
%% `ar_serialize:encode_packing(Packing, true)' gives, so a label reads the
%% same here as it does in the Arweave node's own logs and paths.
packing_label(unpacked) ->
    <<"unpacked">>;
packing_label(unpacked_padded) ->
    <<"unpacked_padded">>;
packing_label({spora_2_6, Address}) ->
    << "spora_2_6_", (hb_util:encode(Address))/binary >>;
packing_label({replica_2_9, Address}) ->
    << "replica_2_9_", (hb_util:encode(Address))/binary >>;
packing_label({composite, Address, Difficulty}) ->
    <<
        "composite_",
        (hb_util:encode(Address))/binary,
        ".",
        (hb_util:bin(Difficulty))/binary
    >>.

%% @doc Read the packing a label's second component names, or `not_found'.
parse_packing(<<"unpacked">>) ->
    unpacked;
parse_packing(<<"unpacked_padded">>) ->
    unpacked_padded;
parse_packing(<< "spora_2_6_", Address/binary >>) ->
    packed(spora_2_6, Address);
parse_packing(<< "replica_2_9_", Address/binary >>) ->
    packed(replica_2_9, Address);
parse_packing(<< "composite_", Rest/binary >>) ->
    composite(binary:split(Rest, <<".">>));
parse_packing(_Label) ->
    not_found.

%% @doc Pair a packing format with the address it is bound to.
packed(Format, Encoded) ->
    case address(Encoded) of
        not_found -> not_found;
        Address -> {Format, Address}
    end.

composite([Encoded, Difficulty]) ->
    composite(address(Encoded), catch hb_util:int(Difficulty));
composite(_Parts) ->
    not_found.

composite(not_found, _Difficulty) ->
    not_found;
composite(Address, Difficulty) when is_integer(Difficulty) ->
    {composite, Address, Difficulty};
composite(_Address, _Difficulty) ->
    not_found.

%% @doc Read the address a label spells in base64url. Every Arweave address is
%% 32 bytes, so anything else is a label this node did not write.
address(Encoded) ->
    case catch hb_util:native_id(Encoded) of
        Address when is_binary(Address), byte_size(Address) == 32 -> Address;
        _Other -> not_found
    end.

%% @doc Read the atom a label's first component names. The atom must already
%% exist: the ids are written by the passes that record them, so a label naming
%% an atom this node has never used names a record it cannot act on.
name(Encoded) ->
    case catch hb_util:atom(Encoded) of
        Id when is_atom(Id) -> Id;
        _Other -> not_found
    end.

%% @doc Pair an id with the packing its label names, refusing a label either
%% half of which could not be read.
tagged(not_found, _Packing) -> not_found;
tagged(_Id, not_found) -> not_found;
tagged(Id, Packing) -> {Id, Packing}.

%% @doc Build the standard error body. A record that cannot be read is a
%% failure of this node's own store rather than of a request.
error_message(Message, Detail) ->
    #{ <<"status">> => 500, <<"message">> => Message, <<"detail">> => Detail }.
