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
%%% A materialized window is carried in the head message's private section,
%%% scoped to the store configuration that supplied it. Private state is not
%%% part of the message identity and never reaches persistent storage. A cold
%%% state therefore walks its history once, while sequential extensions cons
%%% one value onto the window they already carry. Moving a head to another
%%% store cannot borrow values that store cannot resolve.
%%%
-module(lib_arweave_history).
-export([length/2, entries/3, values/2, values/3]).
-export([append/5, materialize/2, from_values/4]).
-export([from_message/3, cap/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE, <<"arweave-history@2.9">>).

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
values([], _Opts) ->
    [];
values(Head, Opts) ->
    case materialized(Head, Opts) of
        not_found -> values(Head, length(Head, Opts), Opts);
        Window -> Window
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
append(Kind, Value, Height, Head, Opts) ->
    Cap = cap(Kind, Height),
    Entry = push(Kind, Value, Cap, Head, Opts),
    with_values(
        Entry,
        [value(Entry, Opts) | lists:sublist(values(Head, Opts), Cap - 1)],
        Opts
    ).

%% @doc Attach a store-scoped materialized window to a history head.
materialize([], _Opts) ->
    [];
materialize(Head, Opts) ->
    case materialized(Head, Opts) of
        not_found -> with_values(Head, values(Head, Opts), Opts);
        _Window -> Head
    end.

%% @doc Build a history from its newest-first consensus values.
%% Values older than the cap are out of the history the rules read, so they are
%% dropped rather than written.
from_values(Kind, Values, Height, Opts) ->
    Cap = cap(Kind, Height),
    from_capped_values(Kind, lists:sublist(Values, Cap), Cap, Opts).

%% @doc Read one value out of the message that carries its fields, which is how
%% a caller supplies the element it is appending. A kind this module does not
%% hold is named rather than raised on: it arrives from a request.
from_message(Kind, Msg, Opts)
        when Kind == <<"reward-history">>;
                Kind == <<"block-time-history">> ->
    {ok, value(Kind, Msg, Opts)};
from_message(_Kind, _Msg, _Opts) ->
    {error, <<"unknown-history-kind">>}.

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

%% @doc Write one entry onto a head and return it. Every entry is written as it
%% is created, which lets the entry above link to it by identifier without
%% writing it again.
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
    {ok, ID} =
        hb_cache:write(
            Entry,
            Opts#{ <<"match-index">> => false }
        ),
    {ok, _Stored} = hb_cache:read(ID, Opts),
    Entry.

%% @doc Build a history from a newest-first list of values, oldest entry first
%% so that each links the one before it, and carry the decoded window privately.
from_capped_values(Kind, Values, Cap, Opts) ->
    {Head, Window} =
        lists:foldl(
            fun(Value, {Onto, Decoded}) ->
                Entry = push(Kind, Value, Cap, Onto, Opts),
                {Entry, [value(Entry, Opts) | Decoded]}
            end,
            {[], []},
            lists:reverse(Values)
        ),
    with_values(Head, Window, Opts).

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

%% @doc Carry a materialized window in the private part of its head message.
with_values([], _Values, _Opts) ->
    [];
with_values(Head, Values, Opts) ->
    hb_private:set(
        Head,
        <<"values">>,
        {hb_opts:get(store, [], Opts), Values},
        Opts
    ).

%% @doc Read a materialized window only in the store context that supplied it.
materialized(Head, Opts) ->
    Store = hb_opts:get(store, [], Opts),
    case hb_private:get(<<"values">>, Head, not_found, Opts) of
        {Store, Values} -> Values;
        _ -> not_found
    end.

%% @doc The identifier an entry is known by, which is the one the entry above it
%% links to.
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

%%% Tests.

%% @doc Materialising a history is confined to the store in the active AO-Core
%% resolution, even when the same process has already read it from another.
store_isolation_test() ->
    Complete = #{ <<"store">> => [hb_test_utils:test_store()] },
    Partial = #{ <<"store">> => [hb_test_utils:test_store()] },
    Values = [{1, 1, 1}, {2, 2, 1}],
    Head = from_values(<<"block-time-history">>, Values, 1275480, Complete),
    {ok, ID} = hb_cache:write(Head, Complete),
    {ok, Reread} = hb_cache:read(ID, Complete),
    ?assertEqual(Values, values(Reread, Complete)),
    {ok, _} = hb_cache:write(Head, Partial),
    ?assertThrow(
        {necessary_message_not_found, _, _},
        values(Reread, Partial)
    ).
