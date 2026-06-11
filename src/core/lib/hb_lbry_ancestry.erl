%%% @doc Create-ancestry walking for LBRY update claim outputs. An update
%%% output only asserts its claim id in-script; this module proves that the
%%% asserted claim id descends from the `OP_CLAIM_NAME' create output that
%%% derives it, by following the spends of prior claim outputs back to the
%%% create. Every hop must be signature-authorized: pointer lineage alone is
%%% forgeable by an untrusted transaction source, so each update input's
%%% spend signature is verified against the parent output's payment script
%%% over the legacy `SIGHASH_ALL' digest.
%%%
%%% Build mode (`build/4') may fetch missing parent transactions through a
%%% caller-supplied fetch function and produces embedded ancestry entries.
%%% Verify mode (`verify_walk/4') replays committed entries only and never
%%% fetches. Build-mode conditions that merely prevent the upgrade (missing
%%% or unsupported evidence, ambiguity, depth) degrade to the existing
%%% `asserted' label; evidence that is internally inconsistent fails hard in
%%% both modes.
-module(hb_lbry_ancestry).
-export([verify_walk/4, build/4, verify_spend/3]).
-export([default_depth_limit/0, depth_limit/1]).
-ifdef(TEST).
-export([test_key/0, test_create_tx/1, test_create_tx/2]).
-export([test_update_tx/4, test_chain/1]).
-endif.
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_DEPTH_LIMIT, 128).
-define(SIGHASH_ALL, 16#01).

default_depth_limit() -> ?DEFAULT_DEPTH_LIMIT.

%% @doc Normalize a configured depth limit, accepting the JSON-encoded
%% binary form alongside plain integers. Anything else falls back to the
%% default: the limit is a resource bound, so a malformed value can only
%% widen back to the default, never accept an invalid proof.
depth_limit(Limit) when is_integer(Limit), Limit > 0 ->
    Limit;
depth_limit(Limit) when is_binary(Limit) ->
    try binary_to_integer(Limit) of
        Int when Int > 0 -> Int;
        _ -> ?DEFAULT_DEPTH_LIMIT
    catch
        _:_ -> ?DEFAULT_DEPTH_LIMIT
    end;
depth_limit(_) ->
    ?DEFAULT_DEPTH_LIMIT.

%% @doc Verify a committed ancestry chain for an update claim output. The
%% entries run from the immediate parent toward the create; each entry
%% embeds the spent parent raw transaction plus, under `input-parents', the
%% previous transactions of the child's other inputs, so the build-mode
%% candidate-uniqueness rule replays from committed material alone: every
%% input's previous output is inspected and exactly one input -- the pinned
%% one -- may spend a claim output carrying the child's claim id. Returns
%% the create's display txid on success. Every failure is hard: verify mode
%% runs at the commitment trust boundary, where incomplete ancestry on an
%% upgraded commitment means the proof does not hold.
verify_walk(ChildRaw, ChildNout, Entries, DepthLimit) ->
    maybe
        {ok, Tx} ?= hb_lbry_tx:parse(ChildRaw),
        {ok, Output} ?= claim_output(Tx, ChildNout),
        <<"update">> ?= maps:get(<<"claim-op">>, Output),
        ClaimID = maps:get(<<"claim-id">>, Output),
        Visited = [{maps:get(<<"txid">>, Tx), ChildNout}],
        walk_entries(Tx, ClaimID, Entries, Visited, DepthLimit)
    else
        {error, _} = Error -> Error;
        <<"create">> -> {error, not_an_update};
        _ -> {error, invalid_ancestry_input}
    end.

walk_entries(_ChildTx, _ClaimID, [], _Visited, _Remaining) ->
    {error, incomplete_ancestry};
walk_entries(_ChildTx, _ClaimID, _Entries, _Visited, Remaining)
        when Remaining =< 0 ->
    {error, ancestry_depth_exceeded};
walk_entries(ChildTx, ClaimID, [Entry | Rest], Visited, Remaining) ->
    maybe
        {ok, ParentTx, ParentOutput, Nout, Siblings} ?= entry_evidence(Entry),
        ParentTxID = maps:get(<<"txid">>, ParentTx),
        true ?= (not lists:member({ParentTxID, Nout}, Visited))
            orelse {error, ancestry_cycle},
        ClaimID ?= maps:get(<<"claim-id">>, ParentOutput),
        ClaimID ?= lower_entry_field(Entry, <<"claim-id">>),
        ClaimOp = maps:get(<<"claim-op">>, ParentOutput),
        ClaimOp ?= maps:get(<<"claim-op">>, Entry, undefined),
        {ok, InputIndex} ?= spending_input(ChildTx, ParentTxID, Nout),
        ok ?=
            unique_pinned_candidate(
                ChildTx,
                ClaimID,
                [ParentTx | Siblings],
                InputIndex
            ),
        ok ?= verify_spend(ChildTx, InputIndex, ParentOutput),
        case {ClaimOp, Rest} of
            {<<"create">>, []} ->
                {ok, ParentTxID};
            {<<"create">>, _} ->
                {error, trailing_ancestry};
            {<<"update">>, _} ->
                walk_entries(
                    ParentTx,
                    ClaimID,
                    Rest,
                    [{ParentTxID, Nout} | Visited],
                    Remaining - 1
                )
        end
    else
        {error, _} = Error -> Error;
        _ -> {error, ancestry_mismatch}
    end.

entry_evidence(Entry) when is_map(Entry) ->
    maybe
        Raw = maps:get(<<"raw-transaction">>, Entry, undefined),
        true ?= is_binary(Raw) orelse {error, missing_ancestor_transaction},
        Nout = maps:get(<<"nout">>, Entry, undefined),
        true ?= is_integer(Nout) orelse {error, invalid_ancestor_nout},
        {ok, ParentTx} ?= hb_lbry_tx:parse(Raw),
        TxID = maps:get(<<"txid">>, ParentTx),
        TxID ?= lower_entry_field(Entry, <<"txid">>),
        {ok, ParentOutput} ?= claim_output(ParentTx, Nout),
        {ok, Siblings} ?=
            sibling_transactions(maps:get(<<"input-parents">>, Entry, [])),
        {ok, ParentTx, ParentOutput, Nout, Siblings}
    else
        {error, _} = Error -> Error;
        _ -> {error, invalid_ancestor_entry}
    end;
entry_evidence(_) ->
    {error, invalid_ancestor_entry}.

sibling_transactions(Raws) when is_list(Raws) ->
    sibling_transactions(Raws, []);
sibling_transactions(_) ->
    {error, invalid_input_parents}.

sibling_transactions([], Acc) ->
    {ok, lists:reverse(Acc)};
sibling_transactions([Raw | Rest], Acc) when is_binary(Raw) ->
    case hb_lbry_tx:parse(Raw) of
        {ok, Tx} -> sibling_transactions(Rest, [Tx | Acc]);
        _ -> {error, malformed_input_parent}
    end;
sibling_transactions(_, _) ->
    {error, invalid_input_parents}.

%% @doc Replay the build-mode candidate-uniqueness rule from committed
%% evidence: every input of the child transaction must have its previous
%% transaction embedded (the pinned parent or one of the `input-parents'
%% siblings), and exactly one input -- the pinned one -- may spend a claim
%% output carrying the child's claim id. Sibling evidence that no input
%% references is rejected, keeping proofs canonical.
unique_pinned_candidate(ChildTx, ClaimID, ParentTxs, PinnedIndex) ->
    Index =
        maps:from_list(
            [{maps:get(<<"txid">>, ParentTx), ParentTx} || ParentTx <- ParentTxs]
        ),
    Inputs = maps:get(<<"inputs">>, ChildTx),
    InputTxIDs = [maps:get(<<"prev-txid">>, Input) || Input <- Inputs],
    Unreferenced =
        [TxID || TxID <- maps:keys(Index), not lists:member(TxID, InputTxIDs)],
    case {Unreferenced, claim_candidates(Inputs, Index, ClaimID)} of
        {[], {ok, [PinnedIndex]}} -> ok;
        {[], {ok, Candidates}} when length(Candidates) > 1 ->
            {error, ambiguous_ancestry};
        {[], {ok, _Other}} -> {error, pinned_parent_not_candidate};
        {[], {error, _} = Error} -> Error;
        {_, _} -> {error, unreferenced_input_parent}
    end.

claim_candidates(Inputs, Index, ClaimID) ->
    claim_candidates(Inputs, Index, ClaimID, 0, []).

claim_candidates([], _Index, _ClaimID, _Position, Acc) ->
    {ok, lists:reverse(Acc)};
claim_candidates([Input | Rest], Index, ClaimID, Position, Acc) ->
    case maps:get(maps:get(<<"prev-txid">>, Input), Index, undefined) of
        undefined ->
            {error, missing_input_parent};
        ParentTx ->
            NextAcc =
                case claim_output(ParentTx, maps:get(<<"prev-nout">>, Input)) of
                    {ok, #{ <<"claim-id">> := ClaimID }} -> [Position | Acc];
                    _ -> Acc
                end,
            claim_candidates(Rest, Index, ClaimID, Position + 1, NextAcc)
    end.

%% @doc Build the ancestry entries for an update claim output, fetching
%% parent raw transactions with `FetchFun(TxIDHex) -> {ok, Raw} | error'.
%% Every input's previous transaction is fetched so candidate parents can be
%% selected unambiguously: exactly one input must spend a claim output that
%% carries the child's claim id. Returns `{ok, Entries}', `{degrade,
%% Reason}' when no complete supported proof can be built (the output keeps
%% its `asserted' label), or `{error, Reason}' when the evidence itself is
%% inconsistent.
build(ChildRaw, ChildNout, FetchFun, DepthLimit) ->
    maybe
        {ok, Tx} ?= hb_lbry_tx:parse(ChildRaw),
        {ok, Output} ?= claim_output(Tx, ChildNout),
        <<"update">> ?= maps:get(<<"claim-op">>, Output),
        ClaimID = maps:get(<<"claim-id">>, Output),
        Visited = [{maps:get(<<"txid">>, Tx), ChildNout}],
        build_hops(Tx, ClaimID, FetchFun, Visited, DepthLimit, [])
    else
        {error, _} = Error -> Error;
        <<"create">> -> {error, not_an_update};
        _ -> {error, invalid_ancestry_input}
    end.

build_hops(_ChildTx, _ClaimID, _FetchFun, _Visited, Remaining, _Acc)
        when Remaining =< 0 ->
    {degrade, ancestry_depth_exceeded};
build_hops(ChildTx, ClaimID, FetchFun, Visited, Remaining, Acc) ->
    maybe
        {ok, Parents} ?= fetch_parents(ChildTx, FetchFun),
        {ok, InputIndex, ParentTx, ParentOutput, Nout} ?=
            unique_claim_parent(Parents, ClaimID),
        ParentTxID = maps:get(<<"txid">>, ParentTx),
        true ?= (not lists:member({ParentTxID, Nout}, Visited))
            orelse {error, ancestry_cycle},
        ok ?= degradable_spend(verify_spend(ChildTx, InputIndex, ParentOutput)),
        Entry = with_input_parents(
            #{
                <<"txid">> => ParentTxID,
                <<"nout">> => Nout,
                <<"claim-op">> => maps:get(<<"claim-op">>, ParentOutput),
                <<"claim-id">> => ClaimID,
                <<"raw-transaction">> => maps:get(<<"raw">>, ParentTx)
            },
            Parents,
            ParentTxID
        ),
        case maps:get(<<"claim-op">>, ParentOutput) of
            <<"create">> ->
                {ok, lists:reverse([Entry | Acc])};
            <<"update">> ->
                build_hops(
                    ParentTx,
                    ClaimID,
                    FetchFun,
                    [{ParentTxID, Nout} | Visited],
                    Remaining - 1,
                    [Entry | Acc]
                )
        end
    else
        {error, _} = Error -> Error;
        {degrade, _} = Degrade -> Degrade;
        _ -> {error, ancestry_build_failed}
    end.

%% Embed the previous transactions of the child's other inputs into the
%% entry, so verify mode can replay the candidate-uniqueness rule without
%% fetching. The pinned parent already travels as `raw-transaction'; the
%% key is omitted entirely for single-parent spends.
with_input_parents(Entry, Parents, PinnedTxID) ->
    {Raws, _Seen} =
        lists:foldl(
            fun({_Index, _Input, ParentTx}, {Acc, Seen}) ->
                TxID = maps:get(<<"txid">>, ParentTx),
                case TxID == PinnedTxID orelse lists:member(TxID, Seen) of
                    true -> {Acc, Seen};
                    false ->
                        {[maps:get(<<"raw">>, ParentTx) | Acc], [TxID | Seen]}
                end
            end,
            {[], []},
            Parents
        ),
    case lists:reverse(Raws) of
        [] -> Entry;
        InputParents -> Entry#{ <<"input-parents">> => InputParents }
    end.

%% Unsupported script shapes prevent the upgrade but are not evidence of
%% tampering; an invalid signature or mismatched public key on a supported
%% shape is, since no confirmed transaction can carry one.
degradable_spend(ok) -> ok;
degradable_spend({error, unsupported_payment_script}) ->
    {degrade, unsupported_payment_script};
degradable_spend({error, unsupported_sighash_type}) ->
    {degrade, unsupported_sighash_type};
degradable_spend({error, unsupported_script_sig}) ->
    {degrade, unsupported_script_sig};
degradable_spend({error, _} = Error) ->
    Error.

fetch_parents(ChildTx, FetchFun) ->
    Inputs = maps:get(<<"inputs">>, ChildTx),
    fetch_parents(
        lists:zip(Inputs, lists:seq(0, length(Inputs) - 1)),
        FetchFun,
        #{},
        []
    ).

fetch_parents([], _FetchFun, _Cache, Acc) ->
    {ok, lists:reverse(Acc)};
fetch_parents([{Input, Index} | Rest], FetchFun, Cache, Acc) ->
    PrevTxID = maps:get(<<"prev-txid">>, Input),
    case parent_transaction(PrevTxID, FetchFun, Cache) of
        {ok, ParentTx, NextCache} ->
            fetch_parents(
                Rest,
                FetchFun,
                NextCache,
                [{Index, Input, ParentTx} | Acc]
            );
        {degrade, _} = Degrade ->
            Degrade;
        {error, _} = Error ->
            Error
    end.

parent_transaction(PrevTxID, FetchFun, Cache) ->
    case maps:get(PrevTxID, Cache, undefined) of
        undefined ->
            maybe
                {ok, Raw} ?= fetch_result(FetchFun(PrevTxID), PrevTxID),
                {ok, ParentTx} ?= parse_parent(Raw),
                PrevTxID ?= maps:get(<<"txid">>, ParentTx),
                {ok, ParentTx, Cache#{ PrevTxID => ParentTx }}
            else
                {degrade, _} = Degrade -> Degrade;
                {error, _} = Error -> Error;
                _ -> {error, parent_txid_mismatch}
            end;
        ParentTx ->
            {ok, ParentTx, Cache}
    end.

fetch_result({ok, Raw}, _PrevTxID) when is_binary(Raw) -> {ok, Raw};
fetch_result(_, PrevTxID) -> {degrade, {missing_parent, PrevTxID}}.

parse_parent(Raw) ->
    case hb_lbry_tx:parse(Raw) of
        {ok, ParentTx} -> {ok, ParentTx};
        _ -> {error, malformed_parent}
    end.

unique_claim_parent(Parents, ClaimID) ->
    Candidates =
        [
            {Index, ParentTx, Output, maps:get(<<"prev-nout">>, Input)}
         ||
            {Index, Input, ParentTx} <- Parents,
            {ok, Output} <-
                [claim_output(ParentTx, maps:get(<<"prev-nout">>, Input))],
            maps:get(<<"claim-id">>, Output) == ClaimID
        ],
    case Candidates of
        [{Index, ParentTx, Output, Nout}] -> {ok, Index, ParentTx, Output, Nout};
        [] -> {degrade, no_claim_parent};
        _ -> {degrade, ambiguous_ancestry}
    end.

%% @doc Verify the spend signature of one input of a parsed transaction
%% against the parent claim output it spends. The scriptSig must be the
%% standard pay-to-public-key-hash shape (`<DER signature || hash type>
%% <public key>'), the hash type must be exactly `SIGHASH_ALL', the public
%% key must hash to the parent payment script's recipient, and the ECDSA
%% signature must verify over the legacy digest with the parent's full
%% scriptPubKey (claim prefix included) as the scriptCode.
verify_spend(ChildTx, InputIndex, ParentOutput) ->
    maybe
        Inputs = maps:get(<<"inputs">>, ChildTx),
        true ?= InputIndex < length(Inputs) orelse {error, missing_input},
        Input = lists:nth(InputIndex + 1, Inputs),
        {ok, DERSignature, PublicKey} ?=
            spend_script_sig(maps:get(<<"script">>, Input)),
        {ok, PubKeyHash} ?=
            payment_pubkey_hash(
                maps:get(<<"payment-script">>, ParentOutput, undefined)
            ),
        true ?=
            hb_lbry_tx:hash160(PublicKey) == PubKeyHash
                orelse {error, spend_public_key_mismatch},
        {ok, Uncompressed} ?=
            hb_lbry_attestation:public_key_to_uncompressed(PublicKey),
        Digest =
            hb_lbry_tx:signature_hash(
                ChildTx,
                InputIndex,
                maps:get(<<"script">>, ParentOutput)
            ),
        true ?=
            ecdsa_verify(Digest, DERSignature, Uncompressed)
                orelse {error, invalid_spend_signature},
        ok
    else
        {error, _} = Error -> Error;
        _ -> {error, invalid_spend}
    end.

spend_script_sig(
    <<SigLen, SigWithType:SigLen/binary, PubLen, PublicKey:PubLen/binary>>
) when SigLen > 9, SigLen < 16#4c, PubLen < 16#4c ->
    DERSize = SigLen - 1,
    case SigWithType of
        <<DERSignature:DERSize/binary, ?SIGHASH_ALL>> ->
            {ok, DERSignature, PublicKey};
        _ ->
            {error, unsupported_sighash_type}
    end;
spend_script_sig(_) ->
    {error, unsupported_script_sig}.

payment_pubkey_hash(<<16#76, 16#a9, 16#14, Hash:20/binary, 16#88, 16#ac>>) ->
    {ok, Hash};
payment_pubkey_hash(_) ->
    {error, unsupported_payment_script}.

ecdsa_verify(Digest, DERSignature, PublicKey) ->
    try crypto:verify(
        ecdsa,
        sha256,
        {digest, Digest},
        DERSignature,
        [PublicKey, secp256k1]
    ) of
        Result -> Result
    catch
        _:_ -> false
    end.

spending_input(ChildTx, ParentTxID, Nout) ->
    Inputs = maps:get(<<"inputs">>, ChildTx),
    Matches =
        [
            Index
         ||
            {Input, Index} <-
                lists:zip(Inputs, lists:seq(0, length(Inputs) - 1)),
            maps:get(<<"prev-txid">>, Input) == ParentTxID,
            maps:get(<<"prev-nout">>, Input) == Nout
        ],
    case Matches of
        [Index] -> {ok, Index};
        [] -> {error, missing_parent_reference};
        _ -> {error, duplicate_parent_reference}
    end.

claim_output(Tx, Nout) ->
    Outputs =
        [
            Output
         ||
            Output <- maps:get(<<"outputs">>, Tx, []),
            maps:get(<<"nout">>, Output, undefined) == Nout,
            maps:is_key(<<"claim">>, Output)
        ],
    case Outputs of
        [Output | _] -> {ok, Output};
        [] -> {error, missing_claim_output}
    end.

lower_entry_field(Entry, Key) ->
    case maps:get(Key, Entry, undefined) of
        Value when is_binary(Value) -> hb_util:to_lower(Value);
        _ -> undefined
    end.

%%% Test fixtures: a deterministic P2PKH key chain. The update transactions
%%% carry real `SIGHASH_ALL' spend signatures so the verifier's digest and
%%% signature checks run against the same construction rules as on-chain
%%% transactions.
-ifdef(TEST).

test_key() ->
    PrivateKey = <<1:256>>,
    {Uncompressed, _} = crypto:generate_key(ecdh, secp256k1, PrivateKey),
    Compressed = ar_wallet:compress_ecdsa_pubkey(Uncompressed),
    {PrivateKey, Compressed, hb_lbry_tx:hash160(Compressed)}.

test_p2pkh(PubKeyHash) ->
    <<16#76, 16#a9, 16#14, PubKeyHash/binary, 16#88, 16#ac>>.

%% A create transaction with the claim output at nout 0 and a plain payment
%% output at nout 1, both paying the test key.
test_create_tx(Name) ->
    test_create_tx(Name, <<0, "ancestry fixture claim">>).

test_create_tx(Name, Claim) ->
    {_, _, PubKeyHash} = test_key(),
    ClaimScript = <<
        16#b5,
        (script_push(Name))/binary,
        (script_push(Claim))/binary,
        16#6d, 16#75,
        (test_p2pkh(PubKeyHash))/binary
    >>,
    Inputs = [serialized_input(<<0:256>>, 0, <<>>)],
    Outputs = [
        serialized_output(100000, ClaimScript),
        serialized_output(50000, test_p2pkh(PubKeyHash))
    ],
    serialized_tx(Inputs, Outputs).

%% An update transaction asserting `ClaimID' (display hex), spending the
%% given parent outpoints. The first spend must be the claim parent and is
%% signed over the parent's full output script; later spends carry empty
%% script sigs (the walker only authorizes the claim-parent spend).
%% `SignOpts' may override the signing key (`sign-key') to produce an
%% invalid signature, the hash type byte (`hashtype'), or the claim envelope
%% bytes (`claim').
test_update_tx(Name, ClaimID, Spends, SignOpts) ->
    {PrivateKey, PublicKey, PubKeyHash} = test_key(),
    Claim = maps:get(<<"claim">>, SignOpts, <<0, "ancestry fixture update">>),
    ClaimHash = reverse(binary:decode_hex(ClaimID)),
    ClaimScript = <<
        16#b7,
        (script_push(Name))/binary,
        (script_push(ClaimHash))/binary,
        (script_push(Claim))/binary,
        16#6d, 16#6d,
        (test_p2pkh(PubKeyHash))/binary
    >>,
    UnsignedInputs =
        [
            serialized_input(
                hb_lbry_tx:double_sha256(ParentRaw),
                ParentNout,
                <<>>
            )
         ||
            {ParentRaw, ParentNout} <- Spends
        ],
    Outputs = [serialized_output(100000, ClaimScript)],
    Unsigned = serialized_tx(UnsignedInputs, Outputs),
    {ok, UnsignedTx} = hb_lbry_tx:parse(Unsigned),
    [{ClaimParentRaw, ClaimParentNout} | RestSpends] = Spends,
    {ok, ParentTx} = hb_lbry_tx:parse(ClaimParentRaw),
    ParentOutput =
        lists:nth(ClaimParentNout + 1, maps:get(<<"outputs">>, ParentTx)),
    Digest =
        hb_lbry_tx:signature_hash(
            UnsignedTx,
            0,
            maps:get(<<"script">>, ParentOutput)
        ),
    SigningKey = maps:get(<<"sign-key">>, SignOpts, PrivateKey),
    HashType = maps:get(<<"hashtype">>, SignOpts, ?SIGHASH_ALL),
    DERSignature =
        crypto:sign(ecdsa, sha256, {digest, Digest}, [SigningKey, secp256k1]),
    ScriptSig = <<
        (script_push(<<DERSignature/binary, HashType>>))/binary,
        (script_push(PublicKey))/binary
    >>,
    SignedInputs = [
        serialized_input(
            hb_lbry_tx:double_sha256(ClaimParentRaw),
            ClaimParentNout,
            ScriptSig
        )
    |
        [
            serialized_input(hb_lbry_tx:double_sha256(ParentRaw), ParentNout, <<>>)
         ||
            {ParentRaw, ParentNout} <- RestSpends
        ]
    ],
    serialized_tx(SignedInputs, Outputs).

%% A chain of `Updates' update transactions on top of one create. Returns
%% `{CreateRaw, UpdateRaws, ClaimID}' with the updates ordered oldest first.
test_chain(Updates) ->
    CreateRaw = test_create_tx(<<"chain">>),
    {ok, CreateTx} = hb_lbry_tx:parse(CreateRaw),
    [CreateOutput | _] = maps:get(<<"outputs">>, CreateTx),
    ClaimID = maps:get(<<"claim-id">>, CreateOutput),
    UpdateRaws =
        lists:foldl(
            fun(_, Raws) ->
                Parent = hd(Raws),
                [test_update_tx(<<"chain">>, ClaimID, [{Parent, 0}], #{}) | Raws]
            end,
            [CreateRaw],
            lists:seq(1, Updates)
        ),
    {CreateRaw, lists:reverse(lists:droplast(UpdateRaws)), ClaimID}.

serialized_input(PrevHash, Nout, ScriptSig) ->
    <<PrevHash/binary, Nout:32/little,
        (byte_size(ScriptSig)), ScriptSig/binary,
        16#ffffffff:32/little>>.

serialized_output(Amount, Script) ->
    <<Amount:64/little, (varint_bytes(byte_size(Script)))/binary, Script/binary>>.

serialized_tx(Inputs, Outputs) ->
    <<1:32/little-signed,
        (length(Inputs)),
        (iolist_to_binary(Inputs))/binary,
        (length(Outputs)),
        (iolist_to_binary(Outputs))/binary,
        0:32/little>>.

varint_bytes(Value) when Value < 16#fd ->
    <<Value>>;
varint_bytes(Value) ->
    <<16#fd, Value:16/little>>.

script_push(Value) when byte_size(Value) < 16#4c ->
    <<(byte_size(Value)), Value/binary>>;
script_push(Value) when byte_size(Value) =< 16#ff ->
    <<16#4c, (byte_size(Value)), Value/binary>>.

reverse(Bin) ->
    list_to_binary(lists:reverse(binary_to_list(Bin))).

fixture_fetch(Raws) ->
    Index =
        maps:from_list(
            [{hb_lbry_tx:txid(Raw), Raw} || Raw <- Raws]
        ),
    fun(TxID) ->
        case maps:get(TxID, Index, undefined) of
            undefined -> {error, not_found};
            Raw -> {ok, Raw}
        end
    end.

%%% Tests

%% The frozen pair is a real on-chain channel update and the prior claim
%% output it spends; the digest constant was cross-computed independently.
%% This anchors the digest construction (full scriptPubKey as scriptCode,
%% claim prefix included) to consensus reality rather than to fixtures that
%% were signed with the same code under test.
real_claim_spend_signature_verifies_test() ->
    Update = binary:decode_hex(channel_update_tx_hex()),
    Parent = binary:decode_hex(channel_update_parent_tx_hex()),
    {ok, Tx} = hb_lbry_tx:parse(Update),
    {ok, ParentTx} = hb_lbry_tx:parse(Parent),
    ?assertEqual(
        <<"fd8f4b898288c6e42f0652dd05d0829200cda1d9526d950104ddc8ba187a8fad">>,
        maps:get(<<"txid">>, ParentTx)
    ),
    {ok, ParentOutput} = claim_output(ParentTx, 0),
    ?assertEqual(<<"update">>, maps:get(<<"claim-op">>, ParentOutput)),
    ?assertEqual(
        <<"585d54c7b82fd92043ed583c5aea18a9547028aa">>,
        maps:get(<<"claim-id">>, ParentOutput)
    ),
    Digest =
        hb_lbry_tx:signature_hash(
            Tx,
            0,
            maps:get(<<"script">>, ParentOutput)
        ),
    ?assertEqual(
        <<"193a66a1750e6720e8978174b0f66c60a5b479e017c5a76b02ff9721acbd9c2f">>,
        hb_util:to_hex(Digest)
    ),
    ?assertEqual(ok, verify_spend(Tx, 0, ParentOutput)),
    % The payment-script tail alone is the wrong scriptCode: the signature
    % covers the full claim script.
    TailOnly = ParentOutput#{
        <<"script">> => maps:get(<<"payment-script">>, ParentOutput)
    },
    ?assertEqual(
        {error, invalid_spend_signature},
        verify_spend(Tx, 0, TailOnly)
    ).

real_claim_spend_rejects_tampered_signature_test() ->
    Update = binary:decode_hex(channel_update_tx_hex()),
    {ok, Tx} = hb_lbry_tx:parse(Update),
    Parent = binary:decode_hex(channel_update_parent_tx_hex()),
    {ok, ParentTx} = hb_lbry_tx:parse(Parent),
    {ok, ParentOutput} = claim_output(ParentTx, 0),
    [Input | RestInputs] = maps:get(<<"inputs">>, Tx),
    % Flip a byte inside the DER signature body.
    <<Len, SigHead:10/binary, SigByte, SigTail/binary>> =
        maps:get(<<"script">>, Input),
    Forged = Tx#{
        <<"inputs">> =>
            [
                Input#{
                    <<"script">> =>
                        <<Len, SigHead/binary, (SigByte bxor 1), SigTail/binary>>
                }
            |
                RestInputs
            ]
    },
    ?assertEqual(
        {error, invalid_spend_signature},
        verify_spend(Forged, 0, ParentOutput)
    ).

build_and_verify_single_hop_test() ->
    {CreateRaw, [UpdateRaw], ClaimID} = test_chain(1),
    Fetch = fixture_fetch([CreateRaw]),
    {ok, Entries} = build(UpdateRaw, 0, Fetch, ?DEFAULT_DEPTH_LIMIT),
    [Entry] = Entries,
    ?assertEqual(<<"create">>, maps:get(<<"claim-op">>, Entry)),
    ?assertEqual(ClaimID, maps:get(<<"claim-id">>, Entry)),
    ?assertEqual(CreateRaw, maps:get(<<"raw-transaction">>, Entry)),
    ?assertEqual(
        {ok, hb_lbry_tx:txid(CreateRaw)},
        verify_walk(UpdateRaw, 0, Entries, ?DEFAULT_DEPTH_LIMIT)
    ).

build_and_verify_multi_hop_test() ->
    {CreateRaw, [Update1, Update2, Update3], _ClaimID} = test_chain(3),
    Fetch = fixture_fetch([CreateRaw, Update1, Update2]),
    {ok, Entries} = build(Update3, 0, Fetch, ?DEFAULT_DEPTH_LIMIT),
    ?assertEqual(3, length(Entries)),
    ?assertEqual(
        [<<"update">>, <<"update">>, <<"create">>],
        [maps:get(<<"claim-op">>, Entry) || Entry <- Entries]
    ),
    ?assertEqual(
        {ok, hb_lbry_tx:txid(CreateRaw)},
        verify_walk(Update3, 0, Entries, ?DEFAULT_DEPTH_LIMIT)
    ).

build_rejects_create_output_test() ->
    CreateRaw = test_create_tx(<<"solo">>),
    ?assertEqual(
        {error, not_an_update},
        build(CreateRaw, 0, fixture_fetch([]), ?DEFAULT_DEPTH_LIMIT)
    ).

build_degrades_on_missing_parent_test() ->
    {_CreateRaw, [UpdateRaw], _ClaimID} = test_chain(1),
    ?assertMatch(
        {degrade, {missing_parent, _}},
        build(UpdateRaw, 0, fixture_fetch([]), ?DEFAULT_DEPTH_LIMIT)
    ).

build_degrades_without_claim_parent_test() ->
    % The update asserts the claim id but spends only the create's payment
    % output, not its claim output: no candidate parent exists.
    {CreateRaw, _Updates, ClaimID} = test_chain(1),
    Detached = test_update_tx(<<"chain">>, ClaimID, [{CreateRaw, 1}], #{}),
    ?assertEqual(
        {degrade, no_claim_parent},
        build(Detached, 0, fixture_fetch([CreateRaw]), ?DEFAULT_DEPTH_LIMIT)
    ).

build_degrades_on_ambiguous_parents_test() ->
    % Two inputs both spend claim outputs carrying the same claim id; the
    % parent selection cannot be made deterministically.
    {CreateRaw, _Updates, ClaimID} = test_chain(1),
    Sibling = test_update_tx(<<"chain">>, ClaimID, [{CreateRaw, 1}], #{}),
    Ambiguous =
        test_update_tx(
            <<"chain">>,
            ClaimID,
            [{CreateRaw, 0}, {Sibling, 0}],
            #{}
        ),
    ?assertEqual(
        {degrade, ambiguous_ancestry},
        build(
            Ambiguous,
            0,
            fixture_fetch([CreateRaw, Sibling]),
            ?DEFAULT_DEPTH_LIMIT
        )
    ).

build_embeds_input_parents_for_verify_replay_test() ->
    % The update spends the claim parent plus a payment output of an
    % unrelated transaction: the unrelated parent must travel in the entry
    % so verify mode can replay the candidate-uniqueness rule.
    {CreateRaw, _Updates, ClaimID} = test_chain(1),
    OtherCreate = test_create_tx(<<"other">>),
    Child =
        test_update_tx(
            <<"chain">>,
            ClaimID,
            [{CreateRaw, 0}, {OtherCreate, 1}],
            #{}
        ),
    Fetch = fixture_fetch([CreateRaw, OtherCreate]),
    {ok, [Entry]} = build(Child, 0, Fetch, ?DEFAULT_DEPTH_LIMIT),
    ?assertEqual([OtherCreate], maps:get(<<"input-parents">>, Entry)),
    ?assertEqual(
        {ok, hb_lbry_tx:txid(CreateRaw)},
        verify_walk(Child, 0, [Entry], ?DEFAULT_DEPTH_LIMIT)
    ),
    % Stripping the sibling evidence leaves an input whose previous output
    % cannot be inspected: the replayed uniqueness rule must fail closed.
    Stripped = maps:remove(<<"input-parents">>, Entry),
    ?assertEqual(
        {error, missing_input_parent},
        verify_walk(Child, 0, [Stripped], ?DEFAULT_DEPTH_LIMIT)
    ).

verify_walk_fails_on_committed_ambiguity_test() ->
    % Two inputs spend claim outputs carrying the same claim id. Build mode
    % degrades; a hand-crafted proof pinning one of them must not verify
    % either, replaying the same fail-closed rule.
    {CreateRaw, _Updates, ClaimID} = test_chain(1),
    Sibling = test_update_tx(<<"chain">>, ClaimID, [{CreateRaw, 1}], #{}),
    Ambiguous =
        test_update_tx(
            <<"chain">>,
            ClaimID,
            [{CreateRaw, 0}, {Sibling, 0}],
            #{}
        ),
    Entry = #{
        <<"txid">> => hb_lbry_tx:txid(CreateRaw),
        <<"nout">> => 0,
        <<"claim-op">> => <<"create">>,
        <<"claim-id">> => ClaimID,
        <<"raw-transaction">> => CreateRaw,
        <<"input-parents">> => [Sibling]
    },
    ?assertEqual(
        {error, ambiguous_ancestry},
        verify_walk(Ambiguous, 0, [Entry], ?DEFAULT_DEPTH_LIMIT)
    ).

verify_walk_rejects_unreferenced_input_parents_test() ->
    {CreateRaw, [UpdateRaw], _ClaimID} = test_chain(1),
    {ok, [Entry]} =
        build(UpdateRaw, 0, fixture_fetch([CreateRaw]), ?DEFAULT_DEPTH_LIMIT),
    Padded = Entry#{ <<"input-parents">> => [test_create_tx(<<"other">>)] },
    ?assertEqual(
        {error, unreferenced_input_parent},
        verify_walk(UpdateRaw, 0, [Padded], ?DEFAULT_DEPTH_LIMIT)
    ).

depth_limit_normalizes_configured_values_test() ->
    ?assertEqual(2, depth_limit(2)),
    ?assertEqual(2, depth_limit(<<"2">>)),
    ?assertEqual(?DEFAULT_DEPTH_LIMIT, depth_limit(0)),
    ?assertEqual(?DEFAULT_DEPTH_LIMIT, depth_limit(<<"zero">>)),
    ?assertEqual(?DEFAULT_DEPTH_LIMIT, depth_limit(undefined)).

build_fails_on_invalid_spend_signature_test() ->
    {CreateRaw, _Updates, ClaimID} = test_chain(1),
    Forged =
        test_update_tx(
            <<"chain">>,
            ClaimID,
            [{CreateRaw, 0}],
            #{ <<"sign-key">> => <<2:256>> }
        ),
    ?assertEqual(
        {error, invalid_spend_signature},
        build(Forged, 0, fixture_fetch([CreateRaw]), ?DEFAULT_DEPTH_LIMIT)
    ).

build_degrades_on_non_sighash_all_test() ->
    {CreateRaw, _Updates, ClaimID} = test_chain(1),
    AnyoneCanPay =
        test_update_tx(
            <<"chain">>,
            ClaimID,
            [{CreateRaw, 0}],
            #{ <<"hashtype">> => 16#81 }
        ),
    ?assertEqual(
        {degrade, unsupported_sighash_type},
        build(AnyoneCanPay, 0, fixture_fetch([CreateRaw]), ?DEFAULT_DEPTH_LIMIT)
    ).

build_degrades_on_depth_limit_test() ->
    {CreateRaw, [Update1, Update2, Update3], _ClaimID} = test_chain(3),
    Fetch = fixture_fetch([CreateRaw, Update1, Update2]),
    ?assertEqual(
        {degrade, ancestry_depth_exceeded},
        build(Update3, 0, Fetch, 2)
    ),
    ?assertMatch({ok, _}, build(Update3, 0, Fetch, 3)).

verify_walk_fails_on_incomplete_ancestry_test() ->
    {CreateRaw, [Update1, Update2], _ClaimID} = test_chain(2),
    Fetch = fixture_fetch([CreateRaw, Update1]),
    {ok, Entries} = build(Update2, 0, Fetch, ?DEFAULT_DEPTH_LIMIT),
    ?assertEqual(
        {error, incomplete_ancestry},
        verify_walk(Update2, 0, lists:droplast(Entries), ?DEFAULT_DEPTH_LIMIT)
    ).

verify_walk_fails_on_trailing_entries_test() ->
    {CreateRaw, [UpdateRaw], ClaimID} = test_chain(1),
    {ok, Entries} =
        build(UpdateRaw, 0, fixture_fetch([CreateRaw]), ?DEFAULT_DEPTH_LIMIT),
    Trailing = Entries ++ [#{
        <<"txid">> => hb_lbry_tx:txid(CreateRaw),
        <<"nout">> => 0,
        <<"claim-op">> => <<"create">>,
        <<"claim-id">> => ClaimID,
        <<"raw-transaction">> => CreateRaw
    }],
    ?assertEqual(
        {error, trailing_ancestry},
        verify_walk(UpdateRaw, 0, Trailing, ?DEFAULT_DEPTH_LIMIT)
    ).

verify_walk_fails_on_tampered_ancestor_test() ->
    {CreateRaw, [UpdateRaw], _ClaimID} = test_chain(1),
    {ok, [Entry]} =
        build(UpdateRaw, 0, fixture_fetch([CreateRaw]), ?DEFAULT_DEPTH_LIMIT),
    <<First, Rest/binary>> = CreateRaw,
    Tampered = Entry#{ <<"raw-transaction">> => <<(First bxor 1), Rest/binary>> },
    ?assertMatch(
        {error, _},
        verify_walk(UpdateRaw, 0, [Tampered], ?DEFAULT_DEPTH_LIMIT)
    ).

verify_walk_fails_on_unrelated_parent_test() ->
    % A valid create for a different claim id cannot terminate the walk.
    {CreateRaw, [UpdateRaw], _ClaimID} = test_chain(1),
    OtherCreate = test_create_tx(<<"other">>),
    {ok, OtherTx} = hb_lbry_tx:parse(OtherCreate),
    {ok, OtherOutput} = claim_output(OtherTx, 0),
    Unrelated = #{
        <<"txid">> => maps:get(<<"txid">>, OtherTx),
        <<"nout">> => 0,
        <<"claim-op">> => <<"create">>,
        <<"claim-id">> => maps:get(<<"claim-id">>, OtherOutput),
        <<"raw-transaction">> => OtherCreate
    },
    ?assertMatch(
        {error, _},
        verify_walk(UpdateRaw, 0, [Unrelated], ?DEFAULT_DEPTH_LIMIT)
    ),
    % Relabeling the unrelated entry with the child's claim id fails on the
    % freshly parsed parent output instead.
    {ok, Tx} = hb_lbry_tx:parse(UpdateRaw),
    {ok, Output} = claim_output(Tx, 0),
    Relabeled = Unrelated#{ <<"claim-id">> => maps:get(<<"claim-id">>, Output) },
    ?assertMatch(
        {error, _},
        verify_walk(UpdateRaw, 0, [Relabeled], ?DEFAULT_DEPTH_LIMIT)
    ).

verify_walk_fails_on_depth_limit_test() ->
    {CreateRaw, [Update1, Update2, Update3], _ClaimID} = test_chain(3),
    Fetch = fixture_fetch([CreateRaw, Update1, Update2]),
    {ok, Entries} = build(Update3, 0, Fetch, ?DEFAULT_DEPTH_LIMIT),
    ?assertEqual(
        {error, ancestry_depth_exceeded},
        verify_walk(Update3, 0, Entries, 2)
    ),
    ?assertMatch({ok, _}, verify_walk(Update3, 0, Entries, 3)).

verify_walk_detects_repeated_outpoint_test() ->
    % Entries that revisit an already-walked outpoint must fail closed even
    % before the structural spend checks reject them.
    {CreateRaw, [Update1, Update2], _ClaimID} = test_chain(2),
    Fetch = fixture_fetch([CreateRaw, Update1]),
    {ok, [Entry1, Entry2]} = build(Update2, 0, Fetch, ?DEFAULT_DEPTH_LIMIT),
    ?assertMatch(
        {error, _},
        verify_walk(Update2, 0, [Entry1, Entry1, Entry2], ?DEFAULT_DEPTH_LIMIT)
    ).

channel_update_tx_hex() ->
    <<
        "0100000002ad8f7a18bac8dd0401956d52d9a1cd009282d005dd52062fe4c68882894b8ffd000000006a47304402201c579c665a5e4e8c78645e3ee8420ab80c1d7645125ecb118e5fa200562309cb02207fde2d077027a734f5d938d4513253ec1ac0083e38b44836fd7fc138bbe7b86701210378ff344cc1f8a5451e7b8f348670b20c44ae44704ac05c59fb936ac1a4f26769ffffffffc16daf5fb34006534963044d5c5337b5a54ecee985bf4ef2e34bb61cb43d4437000000006b483045022100c6af24f806bf3ffaab2190cd309cbde58f1889d4611cde91acae8a23e16dfbe7022019217f2137bae29baab2add1de31afff7798c091cfd2255cc511092adb93b52001210378ff344cc1f8a5451e7b8f348670b20c44ae44704ac05c59fb936ac1a4f26769ffffffff02a086010000000000fd1f03b7134053454d494e4552494f53616c7661746f726514aa287054a918ea5a3c58ed4320d92fb8c7545d584dd7020012650a2103fa4e5fe9f02f2f1a8c34ec150b91f762d8b07b7be942f26aa80c40902d5dbd1122402a3e68747470733a2f2f7468756d62732e6f647963646e2e636f6d2f38303466376661363666363665666233343262393364646332653166333565322e6a706742187777772e6c69627265696e666f726d6174696f6e732e62654a8f044c27696e74c3a96772616c6974c3a920646573207075626c69636174696f6e7320737572203a20200a312e2053697465203a202020202068747470733a2f2f7777772e6c69627265696e666f726d6174696f6e732e62652f0a322e20436861696e65203a2068747470733a2f2f63726f776462756e6b65722e636f6d2f406c69627265696e666f726d6174696f6e730a332e20436861696e65203a2068747470733a2f2f6f64797365652e636f6d2f4053454d494e4552494f53616c7661746f72653a350a342e205768617473417070203a2068747470733a2f2f636861742e77686174736170702e636f6d2f42746b4a697268705443374c6d6f746a507366475a6a0a352e2054c3a96cc3a96772616d203a2068747470733a2f2f742e6d652f6c69627265696e666f730a0a44276176616e6365206a6520766f75732072656d657263696520646520736f7574656e6972206d6f6e207472617661696c20656e206d652066616973616e7420756e20646f6e20706172203a0a20566972656d656e742062616e636169726520656e20636f6d6d756e69636174696f6e2022204e6f6d2f5072c3a96e6f6d2f4164726573736520652d6d61696c220a2d204ec2b020424531392030303133203235373820373531320a2d204ec2b020424534322030363337203534323920333335340a4f75207669612050617950616c2e4d652f6c69627265696e666f0a202d2d2052412a3f68747470733a2f2f7468756d62732e6f647963646e2e636f6d2f32353662656232363563393632656535326535333436316534316430386164342e776562706d6d76a914b462dfca8f203323f9c4375e4160e257f61aca7888acec770000000000001976a914b462dfca8f203323f9c4375e4160e257f61aca7888ac00000000"
    >>.

channel_update_parent_tx_hex() ->
    <<
        "0100000002f20737f76d906508928485986321fc24610039e4ba1a98208f12f01b9a56cedc000000006b483045022100b0ebfbfce75ef2621cdbc81f61b566c418a38cecbdc5f0bba346657b25ec2d05022004ddaba7c3efc4e5a9b4f17d65dc24385a3880f4cdb4f40b82fd3d2c68fcbbef01210378ff344cc1f8a5451e7b8f348670b20c44ae44704ac05c59fb936ac1a4f26769ffffffff5429bf56dea2cf686c5627ed4b24cd4b1c47f32cb58469329bc140cb67b186f3010000006b483045022100f24e35ab1b1b658b1ef90754ac09234f768dbcdd491c47baaedce21057522ed70220465ebe383b5e9a09a290d48ec49213ed9d9cb9f83176b0d20db9011ff94a821a01210378ff344cc1f8a5451e7b8f348670b20c44ae44704ac05c59fb936ac1a4f26769ffffffff01a086010000000000fd9101b7134053454d494e4552494f53616c7661746f726514aa287054a918ea5a3c58ed4320d92fb8c7545d584d49010012650a2103fa4e5fe9f02f2f1a8c34ec150b91f762d8b07b7be942f26aa80c40902d5dbd1122402a3e68747470733a2f2f7468756d62732e6f647963646e2e636f6d2f38303466376661363666363665666233343262393364646332653166333565322e6a706742187777772e6c69627265696e666f726d6174696f6e732e62654a8101526574726f7576657a20746f757465206c657320766964c3a96f7320737572203a207777772e6c69627265696e666f726d6174696f6e732e6265202d2046616365626f6f6b2073656d696e6572696f0a73616c7661746f7265202d2020536f7574656e657a206e6f75733a2050617950616c2e4d652f6c69627265696e666f200a52412a3f68747470733a2f2f7468756d62732e6f647963646e2e636f6d2f32353662656232363563393632656535326535333436316534316430386164342e776562706d6d76a914b462dfca8f203323f9c4375e4160e257f61aca7888ac00000000"
    >>.

-endif.
