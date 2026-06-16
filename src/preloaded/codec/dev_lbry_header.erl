%%% @doc The `lbry-header@1.0' codec: verifies LBRY block-header commitments
%%% against the MMR header commitment (see `aidocs/003_header_commitment_design.md').
%%%
%%% `verify/3' dispatches on the request's `type' key over the TRUSTLESS
%%% commitment classes:
%%%
%%%   `mmr-chunk'       - recompute the 1024 block hashes of a header chunk plus
%%%                       their internal prev-hash linkage, and check that the
%%%                       height-10 subtree root equals the committed chunk id.
%%%   `mmr-membership'  - fold a `(height, block-hash)' proof to the trusted MMR
%%%                       root (the snapshot pinned in node opts).
%%%   `mmr-consistency' - bag the old peaks to the old root, append the provided
%%%                       (independently validated) delta leaves, and re-bag to
%%%                       the new root (per `aidocs/007_roll_forward_headers.py').
%%%
%%% The TEE/snp-anchored commitment classes (`tee-tail', `mmr-genesis') are
%%% deliberately not implemented here: they require an attestation device that
%%% is not present on this branch.
%%%
%%% Trust root: `mmr-membership' and `mmr-consistency' read the pinned root from
%%% node opts (`lbry-header-root' / `lbry-header-snapshot-n'). A codec reading
%%% node opts is normally flagged, but the commitment's trust anchor is the
%%% permitted exception: the verifier-pinned root is exactly the configuration
%%% that defines what "valid" means, and must come from the node, not the
%%% (untrusted) message under verification.
-module(dev_lbry_header).
-implements(<<"lbry-header@1.0">>).
-export([info/0, verify/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(HEADER_SIZE, 112).
-define(CHUNK_HEADERS, 1024).
-define(CHUNK_SIZE, (?CHUNK_HEADERS * ?HEADER_SIZE)).

%% @doc Codec device: `verify' is the only resolved key.
info() ->
    #{ excludes => [<<"keys">>, <<"set">>, <<"set-path">>, <<"remove">>] }.

%% @doc Verify a header commitment, dispatching on the request `type'.
verify(Msg, Req, Opts) ->
    case hb_maps:get(<<"type">>, Msg, undefined, Opts) of
        <<"mmr-chunk">>       -> verify_chunk(Msg, Req, Opts);
        <<"mmr-membership">>  -> verify_membership(Msg, Req, Opts);
        <<"mmr-consistency">> -> verify_consistency(Msg, Req, Opts);
        _                     -> {error, unknown_commitment_type}
    end.

%%% --------------------------------------------------------------------------
%%% mmr-chunk
%%% --------------------------------------------------------------------------

verify_chunk(Msg, _Req, Opts) ->
    ChunkData = hb_maps:get(<<"chunk-data">>, Msg, undefined, Opts),
    ChunkRoot = hb_maps:get(<<"chunk-root">>, Msg, undefined, Opts),
    case lists:member(undefined, [ChunkData, ChunkRoot]) of
        true -> {error, missing_fields};
        false ->
            case byte_size(ChunkData) of
                ?CHUNK_SIZE ->
                    Headers = split_headers(ChunkData),
                    BlockHashes = [hb_lbry_mmr:sha256d(H) || H <- Headers],
                    case check_prevhash_linkage(Headers, BlockHashes) of
                        ok ->
                            Computed = hb_lbry_mmr:chunk_subtree_root(BlockHashes),
                            {ok, Computed =:= normalize_hash(ChunkRoot)};
                        {error, _} = Err ->
                            Err
                    end;
                _ ->
                    {error, invalid_chunk_size}
            end
    end.

split_headers(Bin) ->
    [binary:part(Bin, I * ?HEADER_SIZE, ?HEADER_SIZE)
        || I <- lists:seq(0, ?CHUNK_HEADERS - 1)].

%% Check that header[i+1]'s prev_hash (bytes 4..35) == sha256d(header[i]).
%% Header[0]'s prev_hash is not checked (it connects to the prior chunk).
check_prevhash_linkage(Headers, BlockHashes) ->
    Pairs = lists:zip(
        lists:sublist(Headers, 2, ?CHUNK_HEADERS - 1),
        lists:sublist(BlockHashes, 1, ?CHUNK_HEADERS - 1)
    ),
    check_pairs(Pairs, 1).

check_pairs([], _Idx) -> ok;
check_pairs([{Header, PrevHash} | Rest], Idx) ->
    <<_Version:4/binary, ActualPrev:32/binary, _/binary>> = Header,
    case ActualPrev =:= PrevHash of
        true  -> check_pairs(Rest, Idx + 1);
        false -> {error, {prevhash_mismatch, Idx}}
    end.

%%% --------------------------------------------------------------------------
%%% mmr-membership
%%% --------------------------------------------------------------------------

verify_membership(Msg, _Req, Opts) ->
    Height     = hb_maps:get(<<"height">>,                Msg, undefined, Opts),
    BlockHash  = hb_maps:get(<<"block-hash">>,            Msg, undefined, Opts),
    Siblings   = hb_maps:get(<<"mmr-proof">>,             Msg, undefined, Opts),
    OtherPeaks = hb_maps:get(<<"mmr-proof-peaks">>,       Msg, undefined, Opts),
    PeakIndex  = hb_maps:get(<<"mmr-proof-peak-index">>,  Msg, undefined, Opts),
    TrustedRoot = hb_maps:get(<<"lbry-header-root">>,        Opts, undefined, Opts),
    N           = hb_maps:get(<<"lbry-header-snapshot-n">>,  Opts, undefined, Opts),
    Fields = [Height, BlockHash, Siblings, OtherPeaks, PeakIndex, TrustedRoot, N],
    case lists:member(undefined, Fields) of
        true -> {error, missing_fields};
        false ->
            Proof =
                {
                    [normalize_hash(H) || H <- Siblings],
                    [normalize_hash(P) || P <- OtherPeaks],
                    hb_util:int(PeakIndex)
                },
            {ok,
                hb_lbry_mmr:verify_membership(
                    normalize_hash(BlockHash),
                    hb_util:int(Height),
                    Proof,
                    hb_util:int(N),
                    normalize_hash(TrustedRoot)
                )
            }
    end.

%%% --------------------------------------------------------------------------
%%% mmr-consistency
%%% --------------------------------------------------------------------------

verify_consistency(Msg, _Req, Opts) ->
    OldPeaks    = hb_maps:get(<<"old-peaks">>,    Msg, undefined, Opts),
    DeltaLeaves = hb_maps:get(<<"delta-leaves">>, Msg, undefined, Opts),
    ToRoot      = hb_maps:get(<<"to-root">>,      Msg, undefined, Opts),
    FromRoot    = hb_maps:get(<<"lbry-header-root">>, Opts, undefined, Opts),
    case lists:member(undefined, [OldPeaks, DeltaLeaves, ToRoot, FromRoot]) of
        true -> {error, missing_fields};
        false ->
            {ok,
                hb_lbry_mmr:verify_consistency(
                    normalize_hash(FromRoot),
                    [normalize_peak(P) || P <- OldPeaks],
                    [normalize_hash(L) || L <- DeltaLeaves],
                    normalize_hash(ToRoot)
                )
            }
    end.

%% A peak is a `{Height, Hash}' pair; the height stays as an integer.
normalize_peak({H, Hash}) -> {hb_util:int(H), normalize_hash(Hash)};
normalize_peak([H, Hash]) -> {hb_util:int(H), normalize_hash(Hash)}.

%%% --------------------------------------------------------------------------
%%% Helpers
%%% --------------------------------------------------------------------------

%% Accept either raw 32-byte hashes or 64-char display hex.
normalize_hash(H) when is_binary(H), byte_size(H) =:= 32 -> H;
normalize_hash(H) when is_binary(H), byte_size(H) =:= 64 -> binary:decode_hex(H).

%%% --------------------------------------------------------------------------
%%% Tests (network-free)
%%% --------------------------------------------------------------------------
-ifdef(TEST).

-define(FIXTURE, "test/fixtures/lbry/").

read_fixture(Name) ->
    {ok, Bin} = file:read_file(?FIXTURE ++ Name),
    Bin.

read_eterm(Name) ->
    {ok, [Term]} = file:consult(?FIXTURE ++ Name),
    Term.

verify_chunk_test() ->
    Msg = #{
        <<"type">>       => <<"mmr-chunk">>,
        <<"chunk-data">> => read_fixture("chunk0.bin"),
        <<"chunk-root">> =>
            <<"7621d56d4aec31d0c874008dec0e12b04d0b863546ccbf21c47e872f43a519e4">>
    },
    ?assertEqual({ok, true}, verify(Msg, #{}, #{})).

verify_chunk_wrong_root_test() ->
    Msg = #{
        <<"type">>       => <<"mmr-chunk">>,
        <<"chunk-data">> => read_fixture("chunk0.bin"),
        <<"chunk-root">> => binary:copy(<<$0>>, 64)
    },
    ?assertEqual({ok, false}, verify(Msg, #{}, #{})).

verify_chunk_invalid_size_test() ->
    Msg = #{
        <<"type">>       => <<"mmr-chunk">>,
        <<"chunk-data">> => <<"tooshort">>,
        <<"chunk-root">> => binary:copy(<<$0>>, 64)
    },
    ?assertEqual({error, invalid_chunk_size}, verify(Msg, #{}, #{})).

membership_fixture() ->
    P = read_eterm("mmr_proof_2058011.eterm"),
    Msg = #{
        <<"type">>                 => <<"mmr-membership">>,
        <<"height">>               => maps:get(height, P),
        <<"block-hash">>           => maps:get(leaf_hash, P),
        <<"mmr-proof">>            => maps:get(siblings, P),
        <<"mmr-proof-peaks">>      => maps:get(other_peaks, P),
        <<"mmr-proof-peak-index">> => maps:get(peak_index, P)
    },
    Opts = #{
        <<"lbry-header-root">>       => maps:get(root, P),
        <<"lbry-header-snapshot-n">> => maps:get(n, P)
    },
    {P, Msg, Opts}.

verify_membership_real_test() ->
    {_P, Msg, Opts} = membership_fixture(),
    ?assertEqual({ok, true}, verify(Msg, #{}, Opts)).

verify_membership_tampered_test() ->
    {P, Msg, Opts} = membership_fixture(),
    [First | Rest] = maps:get(siblings, P),
    <<Byte, Tail/binary>> = binary:decode_hex(First),
    Flipped = binary:encode_hex(<<(Byte bxor 1), Tail/binary>>, lowercase),
    ?assertEqual(
        {ok, false},
        verify(Msg#{<<"mmr-proof">> => [Flipped | Rest]}, #{}, Opts)).

consistency_fixture() ->
    Chunk0 = read_fixture("chunk0.bin"),
    Leaves = [hb_lbry_mmr:sha256d(binary:part(Chunk0, I * 112, 112))
                || I <- lists:seq(0, 6)],
    {Old, Delta} = lists:split(4, Leaves),
    OldPeaks = lists:foldl(fun(L, A) -> hb_lbry_mmr:mmr_append(A, L) end, [], Old),
    FromRoot = hb_lbry_mmr:bag_peaks([Pk || {_, Pk} <- OldPeaks]),
    ToRoot = hb_lbry_mmr:mmr_root(Leaves),
    {FromRoot, OldPeaks, Delta, ToRoot}.

verify_consistency_test() ->
    {FromRoot, OldPeaks, Delta, ToRoot} = consistency_fixture(),
    Msg = #{
        <<"type">>         => <<"mmr-consistency">>,
        <<"old-peaks">>    => OldPeaks,
        <<"delta-leaves">> => Delta,
        <<"to-root">>      => ToRoot
    },
    ?assertEqual({ok, true}, verify(Msg, #{}, #{<<"lbry-header-root">> => FromRoot})).

verify_consistency_wrong_to_root_test() ->
    {FromRoot, OldPeaks, Delta, _ToRoot} = consistency_fixture(),
    Msg = #{
        <<"type">>         => <<"mmr-consistency">>,
        <<"old-peaks">>    => OldPeaks,
        <<"delta-leaves">> => Delta,
        <<"to-root">>      => hb_lbry_mmr:sha256d(<<"bad">>)
    },
    ?assertEqual({ok, false}, verify(Msg, #{}, #{<<"lbry-header-root">> => FromRoot})).

unknown_type_test() ->
    ?assertEqual(
        {error, unknown_commitment_type},
        verify(#{<<"type">> => <<"tee-tail">>}, #{}, #{})).

-endif.
