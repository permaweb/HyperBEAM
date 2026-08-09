%%% @doc The persistent linked list Arweave's two carried histories are held
%%% in, and the rules that bound their length.
%%%
%%% A block header commits to a `reward-history-hash' and a
%%% `block-time-history-hash' over histories the header does not carry and no
%%% peer gossips. Both shift by one element per block, and the rules that
%%% consume them read a fixed window back, so the representation has to make
%%% extending a history cheap without making reading one expensive.
%%%
%%% Each element is one immutable message whose `previous' key links to the
%%% element before it. Extending a history therefore writes exactly one
%%% message, and the whole tail is shared -- as the same messages, under the
%%% same identifiers -- with every state that already reached it. Two branches
%%% of a reorg that agree about the blocks below the fork hold the identical
%%% entries for them, and switching between the branches rewrites none of them.
%%%
%%% Reading follows one link per entry returned and stops when the caller has
%%% what it asked for, so `entries/3' and `values/3' cost what they return
%%% rather than the length of the chain.
%%%
%%% Each entry records the `length' of the history ending at it, capped by
%%% `cap/2' at the number of entries the consensus rules read back. An entry
%%% older than the cap is still linked -- an entry is immutable, so a tail
%%% cannot be cut without rewriting it, and rewriting it is what would break the
%%% sharing a reorg relies on -- but it is out of the history, is never
%%% followed, and is never counted.
%%%
%%% `values/2' -- the whole history, which is the read every consensus rule
%%% performs -- is memoised against the identifier of the entry it ends at.
%%% Extending a history conses one value onto the window its parent left
%%% behind, so a run of blocks costs one entry message per block rather than a
%%% walk of the window per block. What the window is keyed on is the entry's
%%% identifier rather than the term, so a chain state read back from the cache
%%% is answered from it just as one still held in memory is -- which is the
%%% case that matters, because that is how a block reaches its parent.
%%%
%%% The window lives in the process dictionary of whichever process is applying
%%% blocks, which for a node is the single long-lived runner
%%% `~arweave@2.9/sync' serialises every pass on. A process that has neither
%%% built nor read a history walks the whole of it once, which is why the walk
%%% and the window are both worth having.
-module(lib_arweave_history).
-export([length/2, entries/3, values/2, values/3]).
-export([append/5, from_binary/4, cap/2]).
-include("include/hb.hrl").

-define(DEVICE, <<"arweave-history@2.9">>).

%%% The process-dictionary key the memoised window of each history is held
%%% under. One window per kind, which is the one the next block needs: a reorg
%%% asks for the window of a head on the other branch, misses, and walks it
%%% once.
-define(WINDOW(Kind), {?MODULE, Kind}).

%% @doc Return the number of values a history holds: the length recorded on its
%% newest entry, which `cap/2' has already bounded. An absent history holds
%% none.
length([], _Opts) ->
    0;
length(Head, Opts) ->
    hb_util:int(hb_maps:get(<<"length">>, Head, 0, Opts)).

%% @doc Return the newest `Count' entries of a history, newest first. Exactly
%% one link is followed per entry returned, so a caller asking for the tip of a
%% history never reads the rest of it.
entries(_Head, Count, _Opts) when Count =< 0 ->
    [];
entries([], _Count, _Opts) ->
    [];
entries(Head, 1, _Opts) ->
    % The last entry the walk returns. Its link is left alone, so a read of the
    % newest entry of a history touches that entry and nothing under it.
    [Head];
entries(Head, Count, Opts) ->
    [
        Head
    |
        entries(hb_maps:get(<<"previous">>, Head, [], Opts), Count - 1, Opts)
    ].

%% @doc Return every value a history holds, newest first, as the tuples the
%% vendored rules work on.
%%
%% This is the read every consensus rule performs, and the only one memoised: a
%% caller taking the newest few values is inspecting the history rather than
%% validating against it, and must not displace the window a block needs.
values([], _Opts) ->
    [];
values(Head, Opts) ->
    case window(Head, Opts) of
        not_found ->
            Values = values(Head, length(Head, Opts), Opts),
            with_window(Head, Values, Opts),
            Values;
        Values ->
            Values
    end.

%% @doc Return the newest `Count' values of a history, newest first.
values(Head, Count, Opts) ->
    [ value(Entry, Opts) || Entry <- entries(Head, Count, Opts) ].

%% @doc Extend a history with one value and return its new head.
%%
%% One message is written. Every entry below the new one is shared with the
%% history that was extended, so the cost of a block is the entry it adds and
%% nothing else.
%%
%% What the window gains is the value read back off the entry rather than the
%% one handed in, so that a window and a walk of the same history cannot answer
%% differently: were the two to disagree, a state would validate in the process
%% that wrote it and fail in the one that read it back.
append(Kind, Value, Height, Head, Opts) ->
    Cap = cap(Kind, Height),
    Entry = push(Kind, Value, Cap, Head, Opts),
    with_window(
        Entry,
        [value(Entry, Opts) | lists:sublist(values(Head, Opts), Cap - 1)],
        Opts
    ).

%% @doc Build a history from the binary form a peer serves it in, which is
%% oldest entry first.
%%
%% Values older than the cap are dropped rather than written: they are out of
%% the history the consensus rules read, so an entry holding one would never be
%% followed.
from_binary(Kind, Body, Height, Opts) ->
    Cap = cap(Kind, Height),
    case decode(Kind, Body) of
        {ok, Values} ->
            {ok, from_values(Kind, lists:sublist(Values, Cap), Cap, Opts)};
        {error, _Reason} ->
            {error, << "invalid-", Kind/binary >>}
    end.

%% @doc Return the number of entries the consensus rules read back from a
%% history.
%%
%% Both lengths come from the vendored module that owns the rule, so a stored
%% history is exactly as long as what reads it: the reward history's is the
%% length `ar_rewards:add_element/2' trims to, and the block-time history's is
%% the length every `ar_block_time_history' consumer takes.
cap(<<"reward-history">>, Height) ->
    ar_rewards:buffered_reward_history_length(Height);
cap(<<"block-time-history">>, _Height) ->
    ar_block_time_history:history_length().

%%% Internal functions.

%% @doc Write one entry onto a head and return it, leaving the window alone.
%%
%% Every entry is written as it is created, which is what lets the entry above
%% it link to it by identifier without writing it again, and what lets a bulk
%% build memoise once at the end rather than at every entry it passes.
push(Kind, Value, Cap, Head, Opts) ->
    Entry =
        maps:merge(
            (fields(Kind, Value))#{
                <<"device">> => ?DEVICE,
                <<"kind">> => Kind,
                <<"length">> => min(length(Head, Opts) + 1, Cap)
            },
            previous(Head, Opts)
        ),
    {ok, _ID} = hb_cache:write(Entry, Opts),
    Entry.

%% @doc Build a history from a newest-first list of values, oldest entry first
%% so that each links the one before it. The window is accumulated as the
%% entries are written, and out of them, so a bulk build costs one pass and
%% leaves behind the same window a walk of the result would produce.
from_values(Kind, Values, Cap, Opts) ->
    {Head, Window} =
        lists:foldl(
            fun(Value, {Onto, Decoded}) ->
                Entry = push(Kind, Value, Cap, Onto, Opts),
                {Entry, [value(Entry, Opts) | Decoded]}
            end,
            {[], []},
            lists:reverse(Values)
        ),
    with_window(Head, Window, Opts).

%% @doc Link a new entry to the head it extends. The oldest entry carries no
%% such key, which is what ends a walk.
previous([], _Opts) ->
    #{};
previous(Head, Opts) ->
    #{
        <<"previous">> =>
            {link,
                id(Head, Opts),
                #{ <<"type">> => <<"link">>, <<"lazy">> => false }
            }
    }.

%% @doc Memoise a materialised window against the entry it ends at, and return
%% that entry. An empty history has no entry to key a window on and needs none.
with_window([], _Values, _Opts) ->
    [];
with_window(Entry, Values, Opts) ->
    erlang:put(
        ?WINDOW(hb_maps:get(<<"kind">>, Entry, <<>>, Opts)),
        {id(Entry, Opts), Values}
    ),
    Entry.

%% @doc Return the window memoised for a history, or `not_found'. The window is
%% keyed on the head's own identifier, so a head read back from the cache is
%% answered from the window built when it was written.
window(Head, Opts) ->
    ID = id(Head, Opts),
    case erlang:get(?WINDOW(hb_maps:get(<<"kind">>, Head, <<>>, Opts))) of
        {ID, Values} -> Values;
        _ -> not_found
    end.

%% @doc The identifier an entry is known by, which is the one the entry above it
%% links to and the one its window is memoised against.
id(Entry, Opts) ->
    hb_message:id(Entry, all, Opts).

%% @doc Represent one value as the keys its entry carries. The kind is carried
%% on every entry rather than only on the head, so an entry read on its own
%% decodes on its own.
fields(<<"reward-history">>, {Address, HashRate, Reward, Denomination}) ->
    #{
        <<"address">> => hb_util:encode(Address),
        <<"hash-rate">> => HashRate,
        <<"reward">> => Reward,
        <<"denomination">> => Denomination
    };
fields(<<"block-time-history">>, {BlockInterval, VDFInterval, ChunkCount}) ->
    #{
        <<"block-interval">> => BlockInterval,
        <<"vdf-interval">> => VDFInterval,
        <<"chunk-count">> => ChunkCount
    }.

%% @doc Decode one entry into the tuple the vendored rules work on.
value(Entry, Opts) ->
    value(hb_maps:get(<<"kind">>, Entry, <<>>, Opts), Entry, Opts).

value(<<"reward-history">>, Entry, Opts) ->
    {
        hb_util:decode(hb_maps:get(<<"address">>, Entry, <<>>, Opts)),
        hb_util:int(hb_maps:get(<<"hash-rate">>, Entry, 0, Opts)),
        hb_util:int(hb_maps:get(<<"reward">>, Entry, 0, Opts)),
        hb_util:int(hb_maps:get(<<"denomination">>, Entry, 0, Opts))
    };
value(<<"block-time-history">>, Entry, Opts) ->
    {
        hb_util:int(hb_maps:get(<<"block-interval">>, Entry, 0, Opts)),
        hb_util:int(hb_maps:get(<<"vdf-interval">>, Entry, 0, Opts)),
        hb_util:int(hb_maps:get(<<"chunk-count">>, Entry, 0, Opts))
    }.

%% @doc Parse the wire form of a history, newest value first. The decoders are
%% the vendored ones, so what a peer serves and what the entries hold are the
%% same values by construction rather than by a second reading of the format.
decode(<<"reward-history">>, Body) ->
    ar_serialize:binary_to_reward_history(Body);
decode(<<"block-time-history">>, Body) ->
    ar_serialize:binary_to_block_time_history(Body).
