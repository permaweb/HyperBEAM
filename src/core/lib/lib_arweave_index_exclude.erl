%%% @doc The weave-offset intervals a scan excludes items by.
%%%
%%% The exclusion policy of the published indexes names byte ranges of the
%%% weave, not item content: an item whose absolute start offset falls
%%% inside an interval receives no rows of either kind, and the scan skips
%%% it before its header is read or parsed. The intervals of the current
%%% policy are the data extents of the L1 transactions that carry RedStone
%%% oracle items, so a covered transaction costs its item-count table and
%%% nothing further.
%%%
%%% The interval file is flat, sorted by start, non-overlapping:
%%%
%%% ```
%%% << Start:64, End:64 >>*    % big-endian; covers [Start, End)
%%% '''
%%%
%%% Intervals are half-open: `Start' is covered, `End' is not. On chain the
%%% boundaries are unobservable either way -- `Start' is the transaction's
%%% own start, where its item-count table sits, and `End' is followed by
%%% chunk padding, so no item begins at either -- and the half-open reading
%%% is the one under which `End - Start' is the transaction's data size.
%%%
%%% The whole file is held as one flat binary -- 16 bytes per interval,
%%% shared by reference across every scan worker -- and membership is a
%%% binary search over it. The empty binary excludes nothing.
-module(lib_arweave_index_exclude).
-export([load/1, excluded/2, count/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The width of one encoded interval, in bytes.
-define(INTERVAL_SIZE, 16).

%% @doc The exclusion intervals named by `arweave-index-exclusions', as
%% `{ok, Intervals, Sha256}' with the file's raw bytes and their hash, or
%% `{ok, <<>>, none}' when no file is configured.
load(Opts) ->
    case hb_opts:get(<<"arweave-index-exclusions">>, not_found, Opts) of
        not_found ->
            {ok, <<>>, none};
        Path ->
            maybe
                {ok, Intervals} ?= file:read_file(Path),
                true ?=
                    byte_size(Intervals) rem ?INTERVAL_SIZE == 0
                        orelse {error, <<"exclusions-misaligned">>},
                {ok, Intervals, crypto:hash(sha256, Intervals)}
            end
    end.

%% @doc Whether an absolute weave offset falls inside an exclusion interval.
excluded(_Offset, <<>>) ->
    false;
excluded(Offset, Intervals) ->
    case search(Offset, Intervals, 0, byte_size(Intervals) div ?INTERVAL_SIZE) of
        none -> false;
        Index ->
            << _Start:64, End:64 >> =
                binary:part(Intervals, Index * ?INTERVAL_SIZE, ?INTERVAL_SIZE),
            Offset < End
    end.

%% @doc How many intervals a loaded binary holds.
count(Intervals) ->
    byte_size(Intervals) div ?INTERVAL_SIZE.

%%% Internal functions.

%% @doc The index of the last interval whose start is at or below the
%% offset, or `none' when the offset precedes them all.
search(Offset, Intervals, Lo, Hi) when Lo < Hi ->
    Mid = (Lo + Hi) div 2,
    << Start:64, _End:64 >> =
        binary:part(Intervals, Mid * ?INTERVAL_SIZE, ?INTERVAL_SIZE),
    case Start =< Offset of
        true -> search(Offset, Intervals, Mid + 1, Hi);
        false -> search(Offset, Intervals, Lo, Mid)
    end;
search(_Offset, _Intervals, 0, _Hi) ->
    none;
search(_Offset, _Intervals, Lo, _Hi) ->
    Lo - 1.

%%% Tests.

%% @doc Membership at and around every boundary of a small interval set:
%% starts are covered, ends are not, gaps and the far outsides are clear.
excluded_test() ->
    Intervals =
        <<
            100:64, 200:64,
            200:64, 250:64,
            1000:64, 1821:64,
            (1 bsl 60):64, ((1 bsl 60) + 5):64
        >>,
    ?assertNot(excluded(0, Intervals)),
    ?assertNot(excluded(99, Intervals)),
    ?assert(excluded(100, Intervals)),
    ?assert(excluded(199, Intervals)),
    % Adjacent intervals leave no seam: 200 ends one and starts the next.
    ?assert(excluded(200, Intervals)),
    ?assert(excluded(249, Intervals)),
    ?assertNot(excluded(250, Intervals)),
    ?assertNot(excluded(999, Intervals)),
    ?assert(excluded(1000, Intervals)),
    ?assert(excluded(1820, Intervals)),
    ?assertNot(excluded(1821, Intervals)),
    ?assert(excluded(1 bsl 60, Intervals)),
    ?assertNot(excluded((1 bsl 60) + 5, Intervals)),
    ?assertNot(excluded(1 bsl 62, Intervals)),
    ?assertNot(excluded(150, <<>>)).

%% @doc The binary search agrees with a linear scan across every offset of
%% a dense little universe.
search_parity_test() ->
    Intervals = << 3:64, 5:64, 8:64, 9:64, 12:64, 20:64 >>,
    Linear =
        fun(Offset) ->
            lists:any(
                fun({S, E}) -> Offset >= S andalso Offset < E end,
                [{3, 5}, {8, 9}, {12, 20}]
            )
        end,
    [
        ?assertEqual(Linear(Offset), excluded(Offset, Intervals))
    ||
        Offset <- lists:seq(0, 25)
    ],
    ok.
