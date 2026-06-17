-module(hb_odysee_corpus_test).
-include_lib("eunit/include/eunit.hrl").

corpus_shape_test() ->
    Corpus = corpus(),
    Entries = maps:get(entries, Corpus),
    ?assert(length(Entries) >= 10),
    ?assert(length(Entries) =< 20),
    Labels = [maps:get(label, Entry) || Entry <- Entries],
    ?assertEqual(length(Labels), length(lists:usort(Labels))),
    lists:foreach(fun validate_entry/1, Entries).

corpus_coverage_matrix_test() ->
    Corpus = corpus(),
    Entries = maps:get(entries, Corpus),
    Coverage = lists:usort(lists:append([
        maps:get(coverage, Entry) || Entry <- Entries
    ])),
    Required = [
        channel_signed,
        anonymous_stream,
        stream_descriptor,
        descriptor_head_204,
        large_multi_blob,
        small_file,
        canonical_name,
        unicode_metadata,
        name_takeover,
        verified_stream_attestation,
        signed_claim_sd_hash,
        exact_stream_size,
        raw_tx_hex,
        sdk_signature_valid
    ],
    lists:foreach(
        fun(RequiredCoverage) ->
            ?assert(lists:member(RequiredCoverage, Coverage))
        end,
        Required
    ),
    ?assert(lists:member(
        legacy_v0_v1_claim,
        maps:get(remaining_matrix_gaps, Corpus, [])
    )).

corpus() ->
    {ok, [Corpus]} = file:consult("test/odysee_bridge_corpus.eterm"),
    Corpus.

validate_entry(Entry) ->
    ?assert(is_atom(maps:get(label, Entry))),
    Coverage = maps:get(coverage, Entry),
    ?assert(is_list(Coverage)),
    ?assert(Coverage =/= []),
    ?assert(lists:all(fun is_atom/1, Coverage)),
    validate_hex(maps:get(claim_id, Entry), 20),
    validate_optional_hex(txid, Entry, 32),
    validate_optional_non_negative_integer(nout, Entry),
    validate_hex(maps:get(sd_hash, Entry), 48),
    validate_channel_claim_id(maps:get(channel_claim_id, Entry, undefined)),
    validate_descriptor_status(Entry, Coverage),
    validate_optional_positive_integer(byte_size, Entry),
    validate_optional_binary(name, Entry),
    validate_optional_binary(canonical_url, Entry),
    validate_optional_binary(media_type, Entry).

validate_hex(Hex, ByteSize) when is_binary(Hex) ->
    ?assertEqual(ByteSize * 2, byte_size(Hex)),
    ?assertEqual(ByteSize, byte_size(binary:decode_hex(Hex))).

validate_optional_hex(Key, Entry, ByteSize) ->
    case maps:get(Key, Entry, undefined) of
        undefined -> ok;
        Hex -> validate_hex(Hex, ByteSize)
    end.

validate_optional_non_negative_integer(Key, Entry) ->
    case maps:get(Key, Entry, undefined) of
        undefined -> ok;
        Value ->
            ?assert(is_integer(Value)),
            ?assert(Value >= 0)
    end.

validate_channel_claim_id(undefined) ->
    ok;
validate_channel_claim_id(ClaimID) ->
    validate_hex(ClaimID, 20).

validate_descriptor_status(Entry, Coverage) ->
    case lists:member(descriptor_head_204, Coverage) of
        true -> ?assertEqual(204, maps:get(descriptor_head_status, Entry));
        false -> ?assertNot(maps:is_key(descriptor_head_status, Entry))
    end.

validate_optional_positive_integer(Key, Entry) ->
    case maps:get(Key, Entry, undefined) of
        undefined -> ok;
        Value ->
            ?assert(is_integer(Value)),
            ?assert(Value > 0)
    end.

validate_optional_binary(Key, Entry) ->
    case maps:get(Key, Entry, undefined) of
        undefined -> ok;
        Value -> ?assert(is_binary(Value))
    end.
