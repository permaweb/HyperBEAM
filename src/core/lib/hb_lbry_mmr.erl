%%% @doc LBRY header-commitment primitives: SHA256d hashing, the Merkle Mountain
%%% Range (MMR) over block hashes, and the Electrum transaction-merkle fold.
%%%
%%% The MMR is the 32-byte commitment from which Odysee header trust bootstraps
%%% (see `aidocs/003_header_commitment_design.md'). Leaves are block hashes
%%% (`sha256d(header)'), interior nodes are `sha256d(L || R)', peaks are bagged
%%% right-to-left, and a chunk is the height-10 perfect subtree over exactly
%%% 1024 block hashes. These constructions are byte-validated against mainnet.
%%%
%%% This module is a pure library: every function is deterministic over its
%%% arguments and performs no I/O. Hashes are handled as raw 32-byte binaries
%%% internally; callers convert display hex at the edge.
-module(hb_lbry_mmr).
-export([sha256/1, sha256d/1]).
-export([mmr_peaks/1, bag_peaks/1, insert_at/3, fold_to_peak/4]).
-export([mmr_append/2, mmr_root/1, chunk_subtree_root/1]).
-export([verify_membership/5, verify_consistency/4]).
-export([merkle_fold/3]).
-include_lib("eunit/include/eunit.hrl").

-define(CHUNK_HEADERS, 1024).

%%% --------------------------------------------------------------------------
%%% Hashing primitives
%%% --------------------------------------------------------------------------

sha256(Bin) -> crypto:hash(sha256, Bin).

sha256d(Bin) -> sha256(sha256(Bin)).

%%% --------------------------------------------------------------------------
%%% MMR shape helpers
%%% --------------------------------------------------------------------------

%% @doc Peak sizes of the size-`N' MMR, largest-first. Each set bit of `N' is a
%% perfect subtree; e.g. `mmr_peaks(7) = [4, 2, 1]'.
mmr_peaks(N) ->
    mmr_peaks(N, bit_length(N) - 1, []).

mmr_peaks(_N, Bit, Acc) when Bit < 0 -> lists:reverse(Acc);
mmr_peaks(N, Bit, Acc) ->
    Size = 1 bsl Bit,
    case N band Size of
        0 -> mmr_peaks(N, Bit - 1, Acc);
        _ -> mmr_peaks(N, Bit - 1, [Size | Acc])
    end.

bit_length(0) -> 0;
bit_length(N) -> bit_length(N, 0).

bit_length(0, Acc) -> Acc;
bit_length(N, Acc) -> bit_length(N bsr 1, Acc + 1).

%% @doc Bag peaks right-to-left: `root = peak_k'; for `i = k-1..0',
%% `root = sha256d(peak_i || root)'.
bag_peaks([]) -> <<0:256>>;
bag_peaks([Single]) -> Single;
bag_peaks(Peaks) ->
    [Last | RevRest] = lists:reverse(Peaks),
    lists:foldl(fun(P, Acc) -> sha256d(<<P/binary, Acc/binary>>) end, Last, RevRest).

%% @doc Insert `Elem' at 0-based `Index' of `List'.
insert_at(List, Index, Elem) ->
    {Before, After} = lists:split(Index, List),
    Before ++ [Elem | After].

find_peak(_Height, [], _Start) -> not_found;
find_peak(Height, [Size | Rest], Start) ->
    case Height >= Start andalso Height < Start + Size of
        true  -> {Size, Start};
        false -> find_peak(Height, Rest, Start + Size)
    end.

%%% --------------------------------------------------------------------------
%%% MMR construction (append / root)
%%% --------------------------------------------------------------------------

%% @doc Append one leaf hash to a peaks list of `{Height, Hash}' (strictly
%% decreasing height), merging equal-height peaks. Matches the Python reference
%% (`aidocs/007_roll_forward_headers.py').
mmr_append(Peaks, LeafHash) ->
    mmr_append(lists:reverse(Peaks), LeafHash, 0).

mmr_append([{H, Peak} | RevRest], Node, H) ->
    mmr_append(RevRest, sha256d(<<Peak/binary, Node/binary>>), H + 1);
mmr_append(RevPeaks, Node, H) ->
    lists:reverse([{H, Node} | RevPeaks]).

%% @doc MMR root over an ordered list of leaf hashes.
mmr_root(LeafHashes) ->
    Peaks = lists:foldl(fun(L, Acc) -> mmr_append(Acc, L) end, [], LeafHashes),
    bag_peaks([P || {_H, P} <- Peaks]).

%% @doc Height-10 perfect binary tree over exactly 1024 block hashes (chunk root).
chunk_subtree_root(Hashes) when length(Hashes) =:= ?CHUNK_HEADERS ->
    perfect_subtree_root(Hashes).

perfect_subtree_root([H]) -> H;
perfect_subtree_root(Leaves) ->
    Half = length(Leaves) div 2,
    {Left, Right} = lists:split(Half, Leaves),
    sha256d(<<(perfect_subtree_root(Left))/binary,
              (perfect_subtree_root(Right))/binary>>).

%%% --------------------------------------------------------------------------
%%% Membership proof
%%% --------------------------------------------------------------------------

%% @doc Fold proof siblings from the leaf at 0-based `Height' up to its peak in
%% the size-`N' MMR. `LocalIdx' within the peak's subtree drives left/right.
fold_to_peak(LeafHash, Height, N, Siblings) ->
    PeakSizes = mmr_peaks(N),
    case find_peak(Height, PeakSizes, 0) of
        not_found -> <<0:256>>;
        {_PeakSize, PeakStart} ->
            fold_siblings(LeafHash, Height - PeakStart, Siblings)
    end.

fold_siblings(Hash, _LocalIdx, []) -> Hash;
fold_siblings(Hash, LocalIdx, [Sib | Rest]) ->
    NewHash =
        case LocalIdx band 1 of
            0 -> sha256d(<<Hash/binary, Sib/binary>>);
            1 -> sha256d(<<Sib/binary, Hash/binary>>)
        end,
    fold_siblings(NewHash, LocalIdx bsr 1, Rest).

%% @doc Verify that `LeafHash' sits at 0-based `Height' of the size-`N' MMR
%% committed to by `Root'. The proof folds the leaf to its peak, splices that
%% peak into `OtherPeaks' at `PeakIndex', and re-bags. All hashes are raw
%% 32-byte binaries. Returns a boolean.
verify_membership(LeafHash, Height, {Siblings, OtherPeaks, PeakIndex}, N, Root) ->
    ComputedPeak = fold_to_peak(LeafHash, Height, N, Siblings),
    AllPeaks = insert_at(OtherPeaks, PeakIndex, ComputedPeak),
    bag_peaks(AllPeaks) =:= Root.

%%% --------------------------------------------------------------------------
%%% Consistency proof (roll-forward)
%%% --------------------------------------------------------------------------

%% @doc Verify an MMR roll-forward from `FromRoot' to `ToRoot'. `OldPeaks' is the
%% list of `{Height, Hash}' peaks of the size-from MMR (the consistency proof);
%% `DeltaLeaves' are the (independently validated) new leaf hashes appended.
%% Binds the old root, appends the delta, and confirms the new root.
%% Per `aidocs/007_roll_forward_headers.py'. Returns a boolean.
verify_consistency(FromRoot, OldPeaks, DeltaLeaves, ToRoot) ->
    case bag_peaks([P || {_H, P} <- OldPeaks]) =:= FromRoot of
        false -> false;
        true ->
            NewPeaks =
                lists:foldl(
                    fun(Leaf, Acc) -> mmr_append(Acc, Leaf) end,
                    OldPeaks,
                    DeltaLeaves
                ),
            bag_peaks([P || {_H, P} <- NewPeaks]) =:= ToRoot
    end.

%%% --------------------------------------------------------------------------
%%% Transaction merkle fold (Electrum)
%%% --------------------------------------------------------------------------

%% @doc Fold `TxId' (raw 32-byte internal order) up a merkle `Branch' of raw
%% 32-byte siblings, with `Pos' selecting left/right at each level. Returns the
%% merkle root (raw 32 bytes).
merkle_fold(TxId, [], _Pos) ->
    TxId;
merkle_fold(TxId, [Sibling | Rest], Pos) ->
    Working =
        case Pos band 1 of
            1 -> sha256d(<<Sibling/binary, TxId/binary>>);
            0 -> sha256d(<<TxId/binary, Sibling/binary>>)
        end,
    merkle_fold(Working, Rest, Pos bsr 1).

%%% --------------------------------------------------------------------------
%%% Tests
%%% --------------------------------------------------------------------------

sha256d_test() ->
    Expected =
        binary:decode_hex(
            <<"9595c9df90075148eb06860365df33584b75bff782a510c6cd4883a419833d50">>),
    ?assertEqual(Expected, sha256d(<<"hello">>)).

mmr_peaks_test() ->
    ?assertEqual([4, 2, 1], mmr_peaks(7)),
    ?assertEqual([8], mmr_peaks(8)),
    ?assertEqual([1], mmr_peaks(1)).

mmr_root_single_peak_test() ->
    L = [sha256d(<<"leaf", (integer_to_binary(I))/binary>>) || I <- lists:seq(0, 3)],
    [L0, L1, L2, L3] = L,
    N01 = sha256d(<<L0/binary, L1/binary>>),
    N23 = sha256d(<<L2/binary, L3/binary>>),
    Expected = sha256d(<<N01/binary, N23/binary>>),
    ?assertEqual(Expected, mmr_root(L)).

mmr_append_matches_root_test() ->
    L = [sha256d(<<"x", (integer_to_binary(I))/binary>>) || I <- lists:seq(0, 6)],
    Peaks = lists:foldl(fun(X, Acc) -> mmr_append(Acc, X) end, [], L),
    Heights = [H || {H, _} <- Peaks],
    ?assertEqual([2, 1, 0], Heights),
    ?assertEqual(bag_peaks([P || {_, P} <- Peaks]), mmr_root(L)).

verify_membership_test() ->
    L = [sha256d(<<"blk", (integer_to_binary(I))/binary>>) || I <- lists:seq(0, 3)],
    [L0, L1, L2, L3] = L,
    N01 = sha256d(<<L0/binary, L1/binary>>),
    N23 = sha256d(<<L2/binary, L3/binary>>),
    Root = sha256d(<<N01/binary, N23/binary>>),
    ?assert(verify_membership(L0, 0, {[L1, N23], [], 0}, 4, Root)),
    ?assertNot(verify_membership(sha256d(<<"wrong">>), 0, {[L1, N23], [], 0}, 4, Root)).

verify_consistency_test() ->
    All = [sha256d(<<"h", (integer_to_binary(I))/binary>>) || I <- lists:seq(0, 6)],
    {Old, Delta} = lists:split(4, All),
    OldPeaks = lists:foldl(fun(X, Acc) -> mmr_append(Acc, X) end, [], Old),
    FromRoot = bag_peaks([P || {_, P} <- OldPeaks]),
    ToRoot = mmr_root(All),
    ?assert(verify_consistency(FromRoot, OldPeaks, Delta, ToRoot)),
    ?assertNot(verify_consistency(FromRoot, OldPeaks, Delta, sha256d(<<"bad">>))),
    ?assertNot(
        verify_consistency(sha256d(<<"badfrom">>), OldPeaks, Delta, ToRoot)).

merkle_fold_test() ->
    Tx = [sha256d(<<"tx", (integer_to_binary(I))/binary>>) || I <- lists:seq(0, 3)],
    [_, _, T2, T3] = Tx,
    [T0, T1, _, _] = Tx,
    N0 = sha256d(<<T0/binary, T1/binary>>),
    N1 = sha256d(<<T2/binary, T3/binary>>),
    Root = sha256d(<<N0/binary, N1/binary>>),
    ?assertEqual(Root, merkle_fold(T2, [T3, N0], 2)).

%%% Fixture-backed tests (network-free) against mainnet-validated vectors.

-define(FIXTURE, "test/fixtures/lbry/").

read_fixture(Name) ->
    {ok, Bin} = file:read_file(?FIXTURE ++ Name),
    Bin.

read_eterm(Name) ->
    {ok, [Term]} = file:consult(?FIXTURE ++ Name),
    Term.

hx(H) -> binary:decode_hex(H).

%% Height-10 subtree root of header chunk 0 (heights 0..1023), mainnet-validated.
chunk_subtree_root_fixture_test() ->
    Chunk0 = read_fixture("chunk0.bin"),
    Hashes = [sha256d(binary:part(Chunk0, I * 112, 112)) || I <- lists:seq(0, 1023)],
    Expected =
        hx(<<"7621d56d4aec31d0c874008dec0e12b04d0b863546ccbf21c47e872f43a519e4">>),
    ?assertEqual(Expected, chunk_subtree_root(Hashes)).

%% Real mainnet MMR membership proof (height 2058011, N=2058045).
membership_real_fixture_test() ->
    P = read_eterm("mmr_proof_2058011.eterm"),
    Proof =
        {
            [hx(S) || S <- maps:get(siblings, P)],
            [hx(O) || O <- maps:get(other_peaks, P)],
            maps:get(peak_index, P)
        },
    ?assert(
        verify_membership(
            hx(maps:get(leaf_hash, P)),
            maps:get(height, P),
            Proof,
            maps:get(n, P),
            hx(maps:get(root, P))
        )).

membership_real_fixture_tampered_test() ->
    P = read_eterm("mmr_proof_2058011.eterm"),
    [First | Rest] = maps:get(siblings, P),
    <<Byte, Tail/binary>> = hx(First),
    Proof =
        {
            [<<(Byte bxor 1), Tail/binary>> | [hx(S) || S <- Rest]],
            [hx(O) || O <- maps:get(other_peaks, P)],
            maps:get(peak_index, P)
        },
    ?assertNot(
        verify_membership(
            hx(maps:get(leaf_hash, P)),
            maps:get(height, P),
            Proof,
            maps:get(n, P),
            hx(maps:get(root, P)))).
