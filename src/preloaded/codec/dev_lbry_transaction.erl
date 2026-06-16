-module(dev_lbry_transaction).
-implements(<<"lbry-transaction@1.0">>).
-device_libraries([lib_lbry_codec]).
-export([from/3, to/3, to_hint/3, verify/3, content_type/1]).
-include("include/hb.hrl").

content_type(_) ->
    {ok, <<"application/vnd.lbry.transaction">>}.

from(Map, Req, Opts) when is_map(Map) ->
    lib_lbry_codec:from_structured(ensure_device(Map), Req, Opts);
from(Raw, Req, Opts) when is_binary(Raw) ->
    case parse_input(Raw, Req, Opts) of
        {ok, Tx} ->
            lib_lbry_codec:from_structured(ensure_device(Tx), Req, Opts);
        Error ->
            Error
    end.

to(Bin, _Req, _Opts) when is_binary(Bin) ->
    {ok, Bin};
to(TABM, Req, Opts) ->
    {ok, Structured} = lib_lbry_codec:to_structured(TABM, Req, Opts),
    lib_lbry_codec:raw_hex_or_structured(ensure_device(Structured), Req, Opts).

to_hint(_Msg, Req, _Opts) ->
    lib_lbry_codec:to_hint(Req).

%% @doc Verify a transaction commitment. Two commitment types are supported,
%% dispatched on the `type' key (read from the commitment `Req', falling back
%% to `Base'):
%%   `sha-256d'            - the raw bytes recompute to the native txid and
%%                           parse as a valid transaction (content integrity);
%%   `mmr-block-inclusion' - the transaction is included in a block whose header
%%                           is committed to the trusted MMR header root (block
%%                           inclusion / SPV anchoring).
verify(Base, Req, Opts) ->
    Type =
        case hb_maps:get(<<"type">>, Req, undefined, Opts) of
            undefined -> hb_maps:get(<<"type">>, Base, undefined, Opts);
            T -> T
        end,
    case Type of
        <<"sha-256d">> -> verify_sha256d(Base, Req, Opts);
        <<"mmr-block-inclusion">> -> verify_block_inclusion(Base, Opts);
        _ -> {ok, false}
    end.

%% @doc The raw transaction bytes must recompute to the commitment's native
%% display-order txid and parse as a valid transaction. The message's `txid'
%% and `device' keys must agree with the commitment. Any missing or mismatching
%% input fails closed.
verify_sha256d(Base, Req, Opts) ->
    Valid =
        maybe
            <<"lbry-transaction@1.0">> ?=
                hb_maps:get(<<"device">>, Base, undefined, Opts),
            ok ?=
                hb_lbry_commitment:committed_subset(
                    Req,
                    [<<"device">>, <<"raw">>, <<"txid">>],
                    Opts
                ),
            {ok, Hex, Bytes} ?= hb_lbry_commitment:native_id(Req, Opts),
            32 ?= byte_size(Bytes),
            Raw = hb_maps:get(<<"raw">>, Base, undefined, Opts),
            true ?= is_binary(Raw),
            Hex ?= hb_lbry_tx:txid(Raw),
            {ok, _Tx} ?= hb_lbry_tx:parse(Raw),
            Hex == txid_field(Base, Opts)
        else
            _ -> false
        end,
    ?event(lbry_commitment, {transaction_verify, {valid, Valid}}),
    {ok, Valid}.

%% @doc Prove block inclusion: (a) the transaction's txid folds up the supplied
%% Electrum merkle branch to the carrying header's merkle root, and (b) the
%% header is committed to the trusted MMR header root (read from node opts) via
%% the same `(height, block-hash)' membership check the header codec performs.
%% The trusted root in `Opts' is the trust anchor (the SPV snapshot), not a
%% content input, so reading it from options is intended.
verify_block_inclusion(Base, Opts) ->
    Raw    = hb_maps:get(<<"raw">>,           Base, undefined, Opts),
    Branch = hb_maps:get(<<"merkle-branch">>, Base, undefined, Opts),
    Pos    = hb_maps:get(<<"position">>,      Base, undefined, Opts),
    Header = hb_maps:get(<<"header">>,        Base, undefined, Opts),
    case lists:member(undefined, [Raw, Branch, Pos, Header]) of
        true -> {error, missing_fields};
        false ->
            Valid =
                maybe
                    true ?= verify_merkle(Raw, Branch, Pos, Header),
                    verify_header_membership(Header, Base, Opts)
                else
                    _ -> false
                end,
            ?event(lbry_commitment, {block_inclusion_verify, {valid, Valid}}),
            {ok, Valid}
    end.

%% (a) Fold the txid (internal byte order) up the branch and compare against the
%% header's merkle root. The txid and the Electrum branch siblings arrive in
%% display byte order and are reversed to internal order before folding, while
%% the header's stored merkle root is already internal order.
verify_merkle(Raw, Branch, Pos, Header) when byte_size(Header) =:= 112 ->
    <<_:36/binary, MerkleRoot:32/binary, _/binary>> = Header,
    TxIdInternal = display_to_internal(hb_lbry_tx:txid(Raw)),
    Computed =
        hb_lbry_mmr:merkle_fold(
            TxIdInternal,
            [display_to_internal(S) || S <- Branch],
            hb_util:int(Pos)
        ),
    Computed =:= MerkleRoot;
verify_merkle(_Raw, _Branch, _Pos, _Header) ->
    false.

%% (b) The header's block hash must be an MMR member of the trusted root. The
%% MMR proof nodes are our own internal-order construction, used as-is.
verify_header_membership(Header, Base, Opts) ->
    BlockHash   = sha256d(Header),
    Height      = hb_maps:get(<<"height">>,               Base, undefined, Opts),
    Siblings    = hb_maps:get(<<"mmr-proof">>,            Base, undefined, Opts),
    OtherPeaks  = hb_maps:get(<<"mmr-proof-peaks">>,      Base, undefined, Opts),
    PeakIndex   = hb_maps:get(<<"mmr-proof-peak-index">>, Base, undefined, Opts),
    TrustedRoot = hb_maps:get(<<"lbry-header-root">>,       Opts, undefined, Opts),
    N           = hb_maps:get(<<"lbry-header-snapshot-n">>, Opts, undefined, Opts),
    case lists:member(undefined,
            [Height, Siblings, OtherPeaks, PeakIndex, TrustedRoot, N]) of
        true -> false;
        false ->
            Proof =
                {
                    [normalize_hash(S) || S <- Siblings],
                    [normalize_hash(P) || P <- OtherPeaks],
                    hb_util:int(PeakIndex)
                },
            hb_lbry_mmr:verify_membership(
                BlockHash,
                hb_util:int(Height),
                Proof,
                hb_util:int(N),
                normalize_hash(TrustedRoot)
            )
    end.

sha256d(Bin) -> crypto:hash(sha256, crypto:hash(sha256, Bin)).

%% A 32-byte binary is already raw; a 64-char binary is display-order hex.
normalize_hash(H) when is_binary(H), byte_size(H) =:= 32 -> H;
normalize_hash(H) when is_binary(H), byte_size(H) =:= 64 -> binary:decode_hex(H).

%% Display (txid/Electrum) order -> internal (consensus) order.
display_to_internal(H) ->
    binary_reverse(normalize_hash(H)).

binary_reverse(Bin) ->
    binary:list_to_bin(lists:reverse(binary:bin_to_list(Bin))).

parse_input(Raw, Req, Opts) ->
    Decoded =
        case hb_maps:get(<<"encoding">>, Req, undefined, Opts) of
            <<"hex">> ->
                decode_hex(Raw);
            _ ->
                {ok, Raw}
        end,
    case Decoded of
        {ok, Bytes} ->
            case hb_lbry_commitment:transaction_message(Bytes) of
                {ok, _} = Ok -> Ok;
                Error when Raw == Bytes -> retry_as_hex(Raw, Error);
                Error -> Error
            end;
        Error ->
            Error
    end.

%% Bare binary inputs may be raw bytes or hex without an `encoding' hint;
%% retry the hex interpretation before failing, matching the previous
%% auto-detection behavior.
retry_as_hex(Raw, ParseError) ->
    case decode_hex(Raw) of
        {ok, Bytes} ->
            case hb_lbry_commitment:transaction_message(Bytes) of
                {ok, _} = Ok -> Ok;
                _ -> ParseError
            end;
        _ ->
            ParseError
    end.

decode_hex(Raw) ->
    case lib_lbry_codec:hex_to_binary(Raw) of
        {ok, Bytes} -> {ok, Bytes};
        _ -> {error, invalid_tx_hex}
    end.

txid_field(Base, Opts) ->
    case hb_maps:get(<<"txid">>, Base, undefined, Opts) of
        TxID when is_binary(TxID) -> hb_util:to_lower(TxID);
        _ -> undefined
    end.

ensure_device(Msg) ->
    Msg#{ <<"device">> => <<"lbry-transaction@1.0">> }.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(FIXTURE, "test/fixtures/lbry/").

read_eterm(Name) ->
    {ok, [Term]} = file:consult(?FIXTURE ++ Name),
    Term.

block_inclusion_msg(Fx) ->
    #{
        <<"type">>                 => <<"mmr-block-inclusion">>,
        <<"raw">>                  => binary:decode_hex(maps:get(raw, Fx)),
        <<"merkle-branch">>        => maps:get(branch, Fx),
        <<"position">>             => maps:get(position, Fx),
        <<"header">>               => binary:decode_hex(maps:get(header, Fx)),
        <<"height">>               => maps:get(height, Fx),
        <<"mmr-proof">>            => maps:get(siblings, Fx),
        <<"mmr-proof-peaks">>      => maps:get(other_peaks, Fx),
        <<"mmr-proof-peak-index">> => maps:get(peak_index, Fx)
    }.

block_inclusion_opts(Fx) ->
    #{
        <<"lbry-header-root">>       => maps:get(root, Fx),
        <<"lbry-header-snapshot-n">> => maps:get(n, Fx)
    }.

%% Real mainnet vector: tx 27628d98..80cb included in block 2058011, anchored
%% to MMR root dcb2769a.. at n=2058045. Exercises the display->internal reversal
%% of the Electrum branch (siblings stored in display order).
verify_block_inclusion_real_test() ->
    Fx = read_eterm("block_inclusion.eterm"),
    ?assertEqual(
        {ok, true},
        verify(block_inclusion_msg(Fx), #{}, block_inclusion_opts(Fx))
    ).

verify_block_inclusion_rejects_tampered_branch_test() ->
    Fx = read_eterm("block_inclusion.eterm"),
    [S0 | Rest] = maps:get(branch, Fx),
    Tampered = Fx#{ branch => [flip_first_hex(S0) | Rest] },
    ?assertEqual(
        {ok, false},
        verify(block_inclusion_msg(Tampered), #{}, block_inclusion_opts(Tampered))
    ).

verify_block_inclusion_rejects_wrong_root_test() ->
    Fx = read_eterm("block_inclusion.eterm"),
    Opts = (block_inclusion_opts(Fx))#{
        <<"lbry-header-root">> => binary:copy(<<$0>>, 64)
    },
    ?assertEqual({ok, false}, verify(block_inclusion_msg(Fx), #{}, Opts)).

flip_first_hex(<<C, Rest/binary>>) ->
    Flipped = case C of $0 -> $1; _ -> $0 end,
    <<Flipped, Rest/binary>>.

-endif.
