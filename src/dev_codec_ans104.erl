%%% @doc Codec for managing transformations from `ar_bundles'-style Arweave TX
%%% records to and from TABMs.
-module(dev_codec_ans104).
-export([to/3, from/3, commit/3, verify/3, content_type/1]).
-export([serialize/3, deserialize/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(BASE_FIELDS, [<<"anchor">>, <<"target">>]).

%% @doc Return the content type for the codec.
content_type(_) -> {ok, <<"application/ans104">>}.

%% @doc Serialize a message or TX to a binary.
serialize(Msg, Req, Opts) when is_map(Msg) ->
    serialize(to(Msg, Req, Opts), Req, Opts);
serialize(TX, _Req, _Opts) when is_record(TX, tx) ->
    {ok, ar_bundles:serialize(TX)}.

%% @doc Deserialize a binary ans104 message to a TABM.
deserialize(#{ <<"body">> := Binary }, Req, Opts) ->
    deserialize(Binary, Req, Opts);
deserialize(Binary, Req, Opts) when is_binary(Binary) ->
    deserialize(ar_bundles:deserialize(Binary), Req, Opts);
deserialize(TX, Req, Opts) when is_record(TX, tx) ->
    from(TX, Req, Opts).

%% @doc Sign a message using the `priv_wallet' key in the options. Supports both
%% the `hmac-sha256' and `rsa-pss-sha256' algorithms, offering unsigned and
%% signed commitments.
commit(Msg, Req = #{ <<"type">> := <<"unsigned">> }, Opts) ->
    commit(Msg, Req#{ <<"type">> => <<"unsigned-sha256">> }, Opts);
commit(Msg, Req = #{ <<"type">> := <<"signed">> }, Opts) ->
    commit(Msg, Req#{ <<"type">> => <<"rsa-pss-sha256">> }, Opts);
commit(Msg, Req = #{ <<"type">> := <<"rsa-pss-sha256">> }, Opts) ->
    % Convert the given message to an ANS-104 TX record, sign it, and convert
    % it back to a structured message.
    {ok, TX} = to(hb_private:reset(Msg), Req, Opts),
    Wallet = hb_opts:get(priv_wallet, no_viable_wallet, Opts),
    Signed = ar_bundles:sign_item(TX, Wallet),
    SignedStructured =
        hb_message:convert(
            Signed,
            <<"structured@1.0">>,
            <<"ans104@1.0">>,
            Opts
        ),
    {ok, SignedStructured};
commit(Msg, #{ <<"type">> := <<"unsigned-sha256">> }, Opts) ->
    % Remove the commitments from the message, convert it to ANS-104, then back.
    % This forces the message to be normalized and the unsigned ID to be
    % recalculated.
    {
        ok,
        hb_message:convert(
            hb_maps:without([<<"commitments">>], Msg, Opts),
            <<"ans104@1.0">>,
            <<"structured@1.0">>,
            Opts
        )
    }.

%% @doc Verify an ANS-104 commitment.
verify(Msg, Req, Opts) ->
    ?event({verify, {base, Msg}, {req, Req}}),
    OnlyWithCommitment =
        hb_private:reset(
            hb_message:with_commitments(
                Req,
                Msg,
                Opts
            )
        ),
    ?event({verify, {only_with_commitment, OnlyWithCommitment}}),
    {ok, TX} = to(OnlyWithCommitment, Req, Opts),
    ?event({verify, {encoded, TX}}),
    Res = ar_bundles:verify_item(TX),
    {ok, Res}.

%% @doc Convert a #tx record into a message map recursively.
from(Binary, _Req, _Opts) when is_binary(Binary) -> {ok, Binary};
from(TX, Req, Opts) when is_record(TX, tx) ->
    case lists:keyfind(<<"ao-type">>, 1, TX#tx.tags) of
        false ->
            do_from(TX, Req, Opts);
        {<<"ao-type">>, <<"binary">>} ->
            {ok, TX#tx.data}
    end.
do_from(RawTX, Req, Opts) ->
    % Ensure the TX is fully deserialized.
    TX = ar_bundles:deserialize(dev_arweave_common:normalize(RawTX)),
    ?event({from, {parsed_tx, TX}}),
    % Get the fields, tags, and data from the TX.
    Fields = dev_codec_ans104_from:fields(TX, <<>>, Opts),
    Tags = dev_codec_ans104_from:tags(TX, Opts),
    Data = dev_codec_ans104_from:data(TX, Req, Tags, Opts),
    ?event({from,
        {parsed_components, {fields, Fields}, {tags, Tags}, {data, Data}}}),
    % Calculate the committed keys on from the TX.
    Keys = dev_codec_ans104_from:committed(
        ?BASE_FIELDS, TX, Fields, Tags, Data, Opts),
    ?event({from, {determined_committed_keys, Keys}}),
    % Create the base message from the fields, tags, and data, filtering to
    % include only the keys that are committed. Will throw if a key is missing.
    Base = dev_codec_ans104_from:base(Keys, Fields, Tags, Data, Opts),
    ?event({from, {calculated_base_message, Base}}),
    % Add the commitments to the message if the TX has a signature.
    FieldCommitments = dev_codec_ans104_from:fields(TX, ?FIELD_PREFIX, Opts),
    WithCommitments = dev_codec_ans104_from:with_commitments(
        TX, <<"ans104@1.0">>, FieldCommitments, Tags, Base, Keys, Opts),
    ?event({from, {parsed_message, WithCommitments}}),
    {ok, WithCommitments}.

%% @doc Internal helper to translate a message to its #tx record representation,
%% which can then be used by ar_bundles to serialize the message. We call the 
%% message's device in order to get the keys that we will be checkpointing. We 
%% do this recursively to handle nested messages. The base case is that we hit
%% a binary, which we return as is.
to(Binary, _Req, _Opts) when is_binary(Binary) ->
    % ar_bundles cannot serialize just a simple binary or get an ID for it, so
    % we turn it into a TX record with a special tag, tx_to_message will
    % identify this tag and extract just the binary.
    {ok,
        #tx{
            tags = [{<<"ao-type">>, <<"binary">>}],
            data = Binary
        }
    };
to(TX, _Req, _Opts) when is_record(TX, tx) -> {ok, TX};
to(RawTABM, Req, Opts) when is_map(RawTABM) ->
    % Ensure that the TABM is fully loaded if the `bundle` key is set to true.
    ?event({to, {inbound, RawTABM}, {req, Req}, {opts, Opts}}),
    MaybeCommitment = hb_message:commitment(
        #{ <<"commitment-device">> => <<"ans104@1.0">> },
        RawTABM,
        Opts
    ),
    IsBundle = dev_codec_ans104_to:is_bundle(MaybeCommitment, Req, Opts),
    MaybeBundle = dev_codec_ans104_to:maybe_load(RawTABM, IsBundle, Opts),
    ?event({to, {maybe_bundle, MaybeBundle}}),

    % Calculate and normalize the `data', if applicable.
    Data = dev_codec_ans104_to:data(MaybeBundle, Req, Opts),
    ?event({to, {calculated_data, Data}}),
    TX0 = dev_codec_ans104_to:siginfo(
        MaybeBundle, MaybeCommitment,
        fun dev_codec_ans104_to:fields_to_tx/4, Opts
    ),
    ?event({to, {found_siginfo, TX0}}),
    TX1 = TX0#tx { data = Data },
    % Calculate the tags for the TX.
    Tags = dev_codec_ans104_to:tags(
        TX1, MaybeCommitment, MaybeBundle,
        dev_codec_ans104_to:excluded_tags(TX1, MaybeBundle, Opts), Opts),
    ?event({to, {calculated_tags, Tags}}),
    TX2 = TX1#tx { tags = Tags },
    Res =
        try dev_arweave_common:normalize(TX2)
        catch
            Type:Error:Stacktrace ->
                ?event({
                    {reset_ids_error, Error},
                    {tx_without_data, {explicit, TX2}}}),
                ?event({prepared_tx_before_ids,
                    {tags, {explicit, TX2#tx.tags}},
                    {data, TX2#tx.data}
                }),
                erlang:raise(Type, Error, Stacktrace)
        end,
    ?event({to, {result, Res}}),
    {ok, Res};
to(Other, _Req, _Opts) ->
    throw({invalid_tx, Other}).

%%% ANS-104-specific testing cases.

normal_tags_test() ->
    Msg = #{
        <<"first-tag">> => <<"first-value">>,
        <<"second-tag">> => <<"second-value">>
    },
    {ok, Encoded} = to(Msg, #{}, #{}),
    ?event({encoded, Encoded}),
    {ok, Decoded} = from(Encoded, #{}, #{}),
    ?event({decoded, Decoded}),
    ?assert(hb_message:match(Msg, Decoded)).

from_maintains_tag_name_case_test() ->
    TX = #tx {
        tags = [
            {<<"Test-Tag">>, <<"test-value">>}
        ]
    },
    SignedTX = ar_bundles:sign_item(TX, hb:wallet()),
    ?event({signed_tx, SignedTX}),
    ?assert(ar_bundles:verify_item(SignedTX)),
    TABM = hb_util:ok(from(SignedTX, #{}, #{})),
    ?event({tabm, TABM}),
    ConvertedTX = hb_util:ok(to(TABM, #{}, #{})),
    ?event({converted_tx, ConvertedTX}),
    ?assert(ar_bundles:verify_item(ConvertedTX)),
    ?assertEqual(ConvertedTX, dev_arweave_common:normalize(SignedTX)).

restore_tag_name_case_from_cache_test() ->
    Opts = #{ store => hb_test_utils:test_store() },
    TX = #tx {
        tags = [
            {<<"Test-Tag">>, <<"test-value">>},
            {<<"test-tag-2">>, <<"test-value-2">>}
        ]
    },
    SignedTX = ar_bundles:sign_item(TX, ar_wallet:new()),
    SignedMsg =
        hb_message:convert(
            SignedTX,
            <<"structured@1.0">>,
            <<"ans104@1.0">>,
            Opts
        ),
    SignedID = hb_message:id(SignedMsg, all),
    ?event({signed_msg, SignedMsg}),
    OnlyCommitted = hb_message:with_only_committed(SignedMsg, Opts),
    ?event({only_committed, OnlyCommitted}),
    {ok, ID} = hb_cache:write(SignedMsg, Opts),
    ?event({id, ID}),
    {ok, ReadMsg} = hb_cache:read(SignedID, Opts),
    ?event({restored_msg, ReadMsg}),
    {ok, ReadTX} = to(ReadMsg, #{}, Opts),
    ?event({restored_tx, ReadTX}),
    ?assert(hb_message:match(ReadMsg, SignedMsg)),
    ?assert(ar_bundles:verify_item(ReadTX)).

unsigned_duplicated_tag_name_test() ->
    TX = dev_arweave_common:normalize(#tx {
        tags = [
            {<<"Test-Tag">>, <<"test-value">>},
            {<<"test-tag">>, <<"test-value-2">>}
        ]
    }),
    Msg = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event({msg, Msg}),
    TX2 = hb_message:convert(Msg, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
    ?event({tx2, TX2}),
    ?assertEqual(TX, TX2).

signed_duplicated_tag_name_test() ->
    TX = ar_bundles:sign_item(#tx {
        tags = [
            {<<"Test-Tag">>, <<"test-value">>},
            {<<"test-tag">>, <<"test-value-2">>}
        ]
    }, ar_wallet:new()),
    Msg = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event({msg, Msg}),
    TX2 = hb_message:convert(Msg, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
    ?event({tx2, TX2}),
    ?assertEqual(TX, TX2),
    ?assert(ar_bundles:verify_item(TX2)).
    
simple_to_conversion_test() ->
    Msg = #{
        <<"first-tag">> => <<"first-value">>,
        <<"second-tag">> => <<"second-value">>
    },
    {ok, Encoded} = to(Msg, #{}, #{}),
    ?event({encoded, Encoded}),
    {ok, Decoded} = from(Encoded, #{}, #{}),
    ?event({decoded, Decoded}),
    ?assert(hb_message:match(Msg, hb_message:uncommitted(Decoded, #{}))).

% @doc Ensure that items with an explicitly defined target field lead to:
% 1. A target being set in the `target' field of the TX record on inbound.
% 2. The parsed message having a `target' field which is committed.
% 3. The target field being placed back into the record, rather than the `tags',
%    on re-encoding.
external_item_with_target_field_test() ->
    TX =
        ar_bundles:sign_item(
            #tx {
                target = crypto:strong_rand_bytes(32),
                anchor = crypto:strong_rand_bytes(32),
                tags = [
                    {<<"test-tag">>, <<"test-value">>},
                    {<<"test-tag-2">>, <<"test-value-2">>}
                ],
                data = <<"test-data">>
            },
            ar_wallet:new()
        ),
    EncodedTarget = hb_util:encode(TX#tx.target),
    EncodedAnchor = hb_util:encode(TX#tx.anchor),
    ?event({tx, TX}),
    Decoded = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event({decoded, Decoded}),
    ?assertEqual(EncodedTarget, hb_maps:get(<<"target">>, Decoded, undefined, #{})),
    ?assertEqual(EncodedAnchor, hb_maps:get(<<"anchor">>, Decoded, undefined, #{})),
    {ok, OnlyCommitted} = hb_message:with_only_committed(Decoded, #{}),
    ?event({only_committed, OnlyCommitted}),
    ?assertEqual(EncodedTarget, hb_maps:get(<<"target">>, OnlyCommitted, undefined, #{})),
    ?assertEqual(EncodedAnchor, hb_maps:get(<<"anchor">>, OnlyCommitted, undefined, #{})),
    Encoded = hb_message:convert(OnlyCommitted, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
    ?assertEqual(TX#tx.target, Encoded#tx.target),
    ?assertEqual(TX#tx.anchor, Encoded#tx.anchor),
    ?event({result, {initial, TX}, {result, Encoded}}),
    ?assertEqual(TX, Encoded).

% @doc Ensure that items made inside HyperBEAM use the tags to encode `target'
% values, rather than the `target' field.
generate_item_with_target_tag_test() ->
    Msg =
        #{
            <<"target">> => Target = <<"NON-ID-TARGET">>,
            <<"anchor">> => Anchor = <<"NON-ID-ANCHOR">>,
            <<"other-key">> => <<"other-value">>
        },
    {ok, TX} = to(Msg, #{}, #{}),
    ?event({encoded_tx, TX}),
    % The encoded TX should have ignored the `target' field, setting a tag instead.
    ?assertEqual(?DEFAULT_TARGET, TX#tx.target),
    ?assertEqual(?DEFAULT_ANCHOR, TX#tx.anchor),
    Decoded = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event({decoded, Decoded}),
    % The decoded message should have the `target' key set to the tag value.
    ?assertEqual(Target, hb_maps:get(<<"target">>, Decoded, undefined, #{})),
    ?assertEqual(Anchor, hb_maps:get(<<"anchor">>, Decoded, undefined, #{})),
    {ok, OnlyCommitted} = hb_message:with_only_committed(Decoded, #{}),
    ?event({only_committed, OnlyCommitted}),
    % The target key should have been committed.
    ?assertEqual(Target, hb_maps:get(<<"target">>, OnlyCommitted, undefined, #{})),
    ?assertEqual(Anchor, hb_maps:get(<<"anchor">>, OnlyCommitted, undefined, #{})),
    Encoded = hb_message:convert(OnlyCommitted, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
    ?event({result, {initial, TX}, {result, Encoded}}),
    ?assertEqual(TX, Encoded).

generate_item_with_target_field_test() ->
    Msg =
        hb_message:commit(
            #{
                <<"target">> => Target = hb_util:encode(crypto:strong_rand_bytes(32)),
                <<"anchor">> => Anchor = hb_util:encode(crypto:strong_rand_bytes(32)),
                <<"other-key">> => <<"other-value">>
            },
            #{ priv_wallet => hb:wallet() },
            <<"ans104@1.0">>
        ),
    {ok, TX} = to(Msg, #{}, #{}),
    ?event({encoded_tx, TX}),
    ?assertEqual(Target, hb_util:encode(TX#tx.target)),
    ?assertEqual(Anchor, hb_util:encode(TX#tx.anchor)),
    Decoded = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event({decoded, Decoded}),
    ?assertEqual(Target, hb_maps:get(<<"target">>, Decoded, undefined, #{})),
    ?assertEqual(Anchor, hb_maps:get(<<"anchor">>, Decoded, undefined, #{})),
    {ok, OnlyCommitted} = hb_message:with_only_committed(Decoded, #{}),
    ?event({only_committed, OnlyCommitted}),
    ?assertEqual(Target, hb_maps:get(<<"target">>, OnlyCommitted, undefined, #{})),
    ?assertEqual(Anchor, hb_maps:get(<<"anchor">>, OnlyCommitted, undefined, #{})),
    Encoded = hb_message:convert(OnlyCommitted, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
    ?event({result, {initial, TX}, {result, Encoded}}),
    ?assertEqual(TX, Encoded).

type_tag_test() ->
    TX =
        ar_bundles:sign_item(
            #tx {
                tags = [{<<"type">>, <<"test-value">>}]
            },
            ar_wallet:new()
        ),
    ?event({tx, TX}),
    Structured = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event({structured, Structured}),
    TX2 = hb_message:convert(Structured, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
    ?event({after_conversion, TX2}),
    ?assertEqual(TX, TX2).

ao_data_key_test() ->
    Msg =
        hb_message:commit(
            #{
                <<"other-key">> => <<"Normal value">>,
                <<"body">> => <<"Body value">>
            },
            #{ priv_wallet => hb:wallet() },
            <<"ans104@1.0">>
        ),
    ?event({msg, Msg}),
    Enc = hb_message:convert(Msg, <<"ans104@1.0">>, #{}),
    ?event({enc, Enc}),
    ?assertEqual(<<"Body value">>, Enc#tx.data),
    Dec = hb_message:convert(Enc, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event({dec, Dec}),
    ?assert(hb_message:verify(Dec, all, #{})).
        
simple_signed_to_httpsig_test() ->
    Structured =
        hb_message:commit(
            #{ <<"test-tag">> => <<"test-value">> },
            #{ priv_wallet => ar_wallet:new() },
            #{
                <<"commitment-device">> => <<"ans104@1.0">>
            }
        ),
    ?event({msg, Structured}),
    HTTPSig =
        hb_message:convert(
            Structured,
            <<"httpsig@1.0">>,
            <<"structured@1.0">>,
            #{}
        ),
    ?event({httpsig, HTTPSig}),
    Structured2 =
        hb_message:convert(
            HTTPSig,
            <<"structured@1.0">>,
            <<"httpsig@1.0">>,
            #{}
        ),
    ?event({decoded, Structured2}),
	Match = hb_message:match(Structured, Structured2, #{}),
    ?assert(Match),
    ?assert(hb_message:verify(Structured2, all, #{})),
    HTTPSig2 = hb_message:convert(Structured2, <<"httpsig@1.0">>, <<"structured@1.0">>, #{}),
    ?event({httpsig2, HTTPSig2}),
    ?assert(hb_message:verify(HTTPSig2, all, #{})),
    ?assert(hb_message:match(HTTPSig, HTTPSig2)).

unsorted_tag_map_test() ->
    TX =
        ar_bundles:sign_item(
            #tx{
                format = ans104,
                tags = [
                    {<<"z">>, <<"position-1">>},
                    {<<"a">>, <<"position-2">>}
                ],
                data = <<"data">>
            },
            ar_wallet:new()
        ),
    ?assert(ar_bundles:verify_item(TX)),
    ?event({tx, TX}),
    {ok, TABM} = dev_codec_ans104:from(TX, #{}, #{}),
    ?event({tabm, TABM}),
    {ok, Decoded} = dev_codec_ans104:to(TABM, #{}, #{}),
    ?event({decoded, Decoded}),
    ?assert(ar_bundles:verify_item(Decoded)).

field_and_tag_ordering_test() ->
    UnsignedTABM = #{
        <<"a">> => <<"value1">>,
        <<"z">> => <<"value2">>,
        <<"target">> => <<"NON-ID-TARGET">>
    },
    Wallet = hb:wallet(),
    SignedTABM = hb_message:commit(
        UnsignedTABM, #{priv_wallet => Wallet}, <<"ans104@1.0">>),
    ?assert(hb_message:verify(SignedTABM)).

unsigned_lowercase_bundle_map_tags_test() ->
    UnsignedTABM = #{
        <<"a1">> => <<"value1">>,
        <<"c1">> => <<"value2">>,
        <<"data">> => #{
            <<"data">> => <<"testdata">>,
            <<"a2">> => <<"value2">>,
            <<"c2">> => <<"value3">>
        }
    },
    {ok, UnsignedTX} = dev_codec_ans104:to(UnsignedTABM, #{}, #{}),
    ?event({tx, UnsignedTX}),
    ?assertEqual([
        {<<"bundle-format">>, <<"binary">>},
        {<<"bundle-version">>, <<"2.0.0">>},
        {<<"bundle-map">>, <<"JmtD0fwFqJTK4P_XexVqBQdnDc0-C7FFIOge6GEOJE8">>},
        {<<"a1">>, <<"value1">>},
        {<<"c1">>, <<"value2">>}
    ], UnsignedTX#tx.tags),
    ?assert(UnsignedTX#tx.manifest =/= undefined),
    {ok, TABM} = dev_codec_ans104:from(UnsignedTX, #{}, #{}),
    ?event({tabm, TABM}),
    ?assertEqual(UnsignedTABM, TABM).

unsigned_mixedcase_bundle_list_tags_1_test() ->
    UnsignedTX = dev_arweave_common:normalize(#tx{
        tags = [
            {<<"TagA1">>, <<"value1">>},
            {<<"TagA2">>, <<"value2">>},
            {<<"Bundle-Format">>, <<"binary">>},
            {<<"Bundle-Version">>, <<"2.0.0">>}
        ],
        data = [ 
            #tx{
                tags = [
                    {<<"TagB1">>, <<"value2">>},
                    {<<"TagB2">>, <<"value3">>}
                ],
                data = <<"item1_data">>
            }
        ]
    }),
    ?event(debug_test, {unsigned_tx, UnsignedTX}),
    ?assertEqual([
        {<<"TagA1">>, <<"value1">>},
        {<<"TagA2">>, <<"value2">>},
        {<<"Bundle-Format">>, <<"binary">>},
        {<<"Bundle-Version">>, <<"2.0.0">>}
    ], UnsignedTX#tx.tags),
    {ok, UnsignedTABM} = dev_codec_ans104:from(UnsignedTX, #{}, #{}),
    ?event(debug_test, {tabm, UnsignedTABM}),
    Commitment = hb_message:commitment(
        hb_util:human_id(UnsignedTX#tx.unsigned_id), UnsignedTABM),
    ?event(debug_test, {commitment, Commitment}),
    ExpectedCommitment = #{
        <<"committed">> => [<<"1">>, <<"taga1">>, <<"taga2">>],
        <<"original-tags">> => #{
            <<"1">> => #{ <<"name">> => <<"TagA1">>, <<"value">> => <<"value1">> },
            <<"2">> => #{ <<"name">> => <<"TagA2">>, <<"value">> => <<"value2">> },
            <<"3">> => #{ <<"name">> => <<"Bundle-Format">>, <<"value">> => <<"binary">> },
            <<"4">> => #{ <<"name">> => <<"Bundle-Version">>, <<"value">> => <<"2.0.0">> }
        }
    },
    ?assertEqual(
        ExpectedCommitment,
        hb_maps:with([<<"committed">>, <<"original-tags">>], Commitment, #{})),
    {ok, TX} = dev_codec_ans104:to(UnsignedTABM, #{}, #{}),
    ?event(debug_test, {tx, TX}),
    ?assertEqual(UnsignedTX, TX),
    ok.

unsigned_mixedcase_bundle_list_tags_2_test() ->
    UnsignedTX = dev_arweave_common:normalize(#tx{
        tags = [
            {<<"TagA1">>, <<"value1">>},
            {<<"TagA2">>, <<"value2">>},
            {<<"Bundle-Format">>, <<"binary">>},
            {<<"Bundle-Version">>, <<"2.0.0">>}
        ],
        data = #{
            <<"1">> => #tx{
                tags = [
                    {<<"TagB1">>, <<"value2">>},
                    {<<"TagB2">>, <<"value3">>}
                ],
                data = <<"item1_data">>
            }
        }
    }),
    ?event(debug_test, {unsigned_tx, UnsignedTX}),
    ?assertEqual([
        {<<"TagA1">>, <<"value1">>},
        {<<"TagA2">>, <<"value2">>},
        {<<"Bundle-Format">>, <<"binary">>},
        {<<"Bundle-Version">>, <<"2.0.0">>}
    ], UnsignedTX#tx.tags),
    {ok, UnsignedTABM} = dev_codec_ans104:from(UnsignedTX, #{}, #{}),
    ?event(debug_test, {tabm, UnsignedTABM}),
    Commitment = hb_message:commitment(
        hb_util:human_id(UnsignedTX#tx.unsigned_id), UnsignedTABM),
    ?event(debug_test, {commitment, Commitment}),
    ExpectedCommitment = #{
        <<"committed">> => [<<"1">>, <<"taga1">>, <<"taga2">>],
        <<"original-tags">> => #{
            <<"1">> => #{ <<"name">> => <<"TagA1">>, <<"value">> => <<"value1">> },
            <<"2">> => #{ <<"name">> => <<"TagA2">>, <<"value">> => <<"value2">> },
            <<"3">> => #{ <<"name">> => <<"Bundle-Format">>, <<"value">> => <<"binary">> },
            <<"4">> => #{ <<"name">> => <<"Bundle-Version">>, <<"value">> => <<"2.0.0">> }
        }
    },
    ?assertEqual(
        ExpectedCommitment,
        hb_maps:with([<<"committed">>, <<"original-tags">>], Commitment, #{})),
    {ok, TX} = dev_codec_ans104:to(UnsignedTABM, #{}, #{}),
    ?event(debug_test, {tx, TX}),
    ?assertEqual(UnsignedTX, TX),
    ok.

unsigned_mixedcase_bundle_map_tags_test() ->
    UnsignedTX = dev_arweave_common:normalize(#tx{
        tags = [
            {<<"bundle-map">>, <<"IJ9HnMqGT4qNc8_O_wZ5-3qTPHC2ZVXxsK03kDRoQw0">>},
            {<<"TagA1">>, <<"value1">>},
            {<<"TagA2">>, <<"value2">>},
            {<<"Bundle-Format">>, <<"binary">>},
            {<<"Bundle-Version">>, <<"2.0.0">>}
        ],
        data = #{
            <<"data">> => #tx{
                tags = [
                    {<<"TagB1">>, <<"value2">>},
                    {<<"TagB2">>, <<"value3">>}
                ],
                data = <<"item1_data">>
            }
        }
    }),
    ?event(debug_test, {unsigned_tx, UnsignedTX}),
    ?assertEqual([
        {<<"bundle-map">>, <<"IJ9HnMqGT4qNc8_O_wZ5-3qTPHC2ZVXxsK03kDRoQw0">>},
        {<<"TagA1">>, <<"value1">>},
        {<<"TagA2">>, <<"value2">>},
        {<<"Bundle-Format">>, <<"binary">>},
        {<<"Bundle-Version">>, <<"2.0.0">>}
    ], UnsignedTX#tx.tags),
    {ok, UnsignedTABM} = dev_codec_ans104:from(UnsignedTX, #{}, #{}),
    ?event(debug_test, {tabm, UnsignedTABM}),
    Commitment = hb_message:commitment(
        hb_util:human_id(UnsignedTX#tx.unsigned_id), UnsignedTABM),
    ?event(debug_test, {commitment, Commitment}),
    ExpectedCommitment = #{
        <<"committed">> => [<<"data">>, <<"taga1">>, <<"taga2">>],
        <<"original-tags">> => #{
            <<"1">> => #{ <<"name">> => <<"bundle-map">>, <<"value">> => <<"IJ9HnMqGT4qNc8_O_wZ5-3qTPHC2ZVXxsK03kDRoQw0">> },
            <<"2">> => #{ <<"name">> => <<"TagA1">>, <<"value">> => <<"value1">> },
            <<"3">> => #{ <<"name">> => <<"TagA2">>, <<"value">> => <<"value2">> },
            <<"4">> => #{ <<"name">> => <<"Bundle-Format">>, <<"value">> => <<"binary">> },
            <<"5">> => #{ <<"name">> => <<"Bundle-Version">>, <<"value">> => <<"2.0.0">> }
        }
    },
    ?assertEqual(
        ExpectedCommitment,
        hb_maps:with([<<"committed">>, <<"original-tags">>], Commitment, #{})),
    {ok, TX} = dev_codec_ans104:to(UnsignedTABM, #{}, #{}),
    ?event(debug_test, {tx, TX}),
    ?assertEqual(UnsignedTX, TX),
    ok.

signed_lowercase_bundle_map_tags_test() ->
    Wallet = ar_wallet:new(),
    UnsignedTABM = #{
        <<"a1">> => <<"value1">>,
        <<"c1">> => <<"value2">>,
        <<"data">> => #{
            <<"data">> => <<"testdata">>,
            <<"a2">> => <<"value2">>,
            <<"c2">> => <<"value3">>
        }
    },
    {ok, UnsignedTX} = dev_codec_ans104:to(UnsignedTABM, #{}, #{}),
    SignedTX = ar_bundles:sign_item(UnsignedTX, Wallet),
    ?event({tx, SignedTX}),
    ?assertEqual([
        {<<"bundle-format">>, <<"binary">>},
        {<<"bundle-version">>, <<"2.0.0">>},
        {<<"bundle-map">>, <<"JmtD0fwFqJTK4P_XexVqBQdnDc0-C7FFIOge6GEOJE8">>},
        {<<"a1">>, <<"value1">>},
        {<<"c1">>, <<"value2">>}
    ], SignedTX#tx.tags),
    ?assert(SignedTX#tx.manifest =/= undefined),
    {ok, SignedTABM} = dev_codec_ans104:from(SignedTX, #{}, #{}),
    ?event({signed_tabm, SignedTABM}),
    ?assertEqual(UnsignedTABM, hb_maps:without([<<"commitments">>], SignedTABM)),
    Commitment = hb_message:commitment(
        hb_util:human_id(SignedTX#tx.id), SignedTABM),
    ?event({commitment, Commitment}),
    ExpectedCommitment = #{
        <<"committed">> => [<<"data">>, <<"a1">>, <<"c1">>],
        <<"bundle-format">> => <<"binary">>,
        <<"bundle-version">> => <<"2.0.0">>,
        <<"bundle-map">> => <<"JmtD0fwFqJTK4P_XexVqBQdnDc0-C7FFIOge6GEOJE8">>
    },
    ?assertEqual(
        ExpectedCommitment, 
        hb_maps:with([
            <<"committed">>,
            <<"bundle-format">>,
            <<"bundle-version">>,
            <<"bundle-map">>], Commitment, #{})),

    {ok, TX} = dev_codec_ans104:to(SignedTABM, #{}, #{}),
    ?event({tx, TX}),
    ?assert(ar_bundles:verify_item(TX)),
    ?assertEqual(SignedTX, TX).

signed_mixedcase_bundle_map_tags_test() ->
    Wallet = ar_wallet:new(),
    UnsignedTABM = #{
        <<"taga1">> => <<"value1">>,
        <<"taga2">> => <<"value2">>,
        <<"data">> => #{
            <<"data">> => <<"testdata">>,
            <<"tagb1">> => <<"value1">>,
            <<"tagb2">> => <<"value2">>
        }
    },
    {ok, UnsignedTX0} = dev_codec_ans104:to(UnsignedTABM, #{}, #{}),
    % Force some of the bundle tags to be out of order and mixed case. Once
    % we sign this version of the transaction, the ordering and casing should
    % be locked in and preserved across future conversions.
    UnsignedTX = UnsignedTX0#tx{ tags = [
        {<<"bundle-map">>, <<"mlOQnRTom7Jlg_UdXk6n_dMMc5h-bUvoTo_QguH7AOE">>},
        {<<"TagA1">>, <<"value1">>},
        {<<"TagA2">>, <<"value2">>},
        {<<"Bundle-Format">>, <<"binary">>},
        {<<"Bundle-Version">>, <<"2.0.0">>}
    ]},
    ?event(debug_test, {unsigned_tx, UnsignedTX}),
    SignedTX = ar_bundles:sign_item(UnsignedTX, Wallet),
    ?event(debug_test, {signed_tx, SignedTX}),
    ?assertEqual([
        {<<"bundle-map">>, <<"mlOQnRTom7Jlg_UdXk6n_dMMc5h-bUvoTo_QguH7AOE">>},
        {<<"TagA1">>, <<"value1">>},
        {<<"TagA2">>, <<"value2">>},
        {<<"Bundle-Format">>, <<"binary">>},
        {<<"Bundle-Version">>, <<"2.0.0">>}
    ], SignedTX#tx.tags),
    ?assert(SignedTX#tx.manifest =/= undefined),
    {ok, SignedTABM} = dev_codec_ans104:from(SignedTX, #{}, #{}),
    ?event(debug_test, {signed_tabm, SignedTABM}),
    ?assertEqual(UnsignedTABM, hb_maps:without([<<"commitments">>], SignedTABM)),
    Commitment = hb_message:commitment(
        hb_util:human_id(SignedTX#tx.id), SignedTABM),
    ?event(debug_test, {commitment, Commitment}),
    ExpectedCommitment = #{
        <<"committed">> => [<<"data">>, <<"taga1">>, <<"taga2">>],
        <<"bundle-format">> => <<"binary">>,
        <<"bundle-version">> => <<"2.0.0">>,
        <<"bundle-map">> => <<"mlOQnRTom7Jlg_UdXk6n_dMMc5h-bUvoTo_QguH7AOE">>,
        <<"original-tags">> => #{
            <<"1">> => #{ <<"name">> => <<"bundle-map">>, <<"value">> => <<"mlOQnRTom7Jlg_UdXk6n_dMMc5h-bUvoTo_QguH7AOE">> },
            <<"2">> => #{ <<"name">> => <<"TagA1">>, <<"value">> => <<"value1">> },
            <<"3">> => #{ <<"name">> => <<"TagA2">>, <<"value">> => <<"value2">> },
            <<"4">> => #{ <<"name">> => <<"Bundle-Format">>, <<"value">> => <<"binary">> },
            <<"5">> => #{ <<"name">> => <<"Bundle-Version">>, <<"value">> => <<"2.0.0">> }
        }
    },
    ?assertEqual(
        ExpectedCommitment, 
        hb_maps:with([
            <<"committed">>,
            <<"bundle-format">>,
            <<"bundle-version">>,
            <<"bundle-map">>,
            <<"original-tags">>], Commitment, #{})),
    {ok, TX} = dev_codec_ans104:to(SignedTABM, #{}, #{}),
    ?event(debug_test, {tx, TX}),
    ?assert(ar_bundles:verify_item(TX)),
    ?assertEqual(SignedTX, TX).

bundle_commitment_test() ->
    test_bundle_commitment(unbundled, unbundled, unbundled),
    test_bundle_commitment(unbundled, bundled, unbundled),
    test_bundle_commitment(unbundled, unbundled, bundled),
    test_bundle_commitment(unbundled, bundled, bundled),
    test_bundle_commitment(bundled, unbundled, unbundled),
    test_bundle_commitment(bundled, bundled, unbundled),
    test_bundle_commitment(bundled, unbundled, bundled),
    test_bundle_commitment(bundled, bundled, bundled),
    ok.

test_bundle_commitment(Commit, Encode, Decode) ->
    Opts = #{ priv_wallet => hb:wallet() },
    Structured = #{ <<"list">> => [1, 2, 3] },
    ToBool = fun(unbundled) -> false; (bundled) -> true end,
    Label = lists:flatten(io_lib:format("~p -> ~p -> ~p",
        [Commit, Encode, Decode])),

    Committed = hb_message:commit(
        Structured,
        Opts,
        #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => ToBool(Commit) }),
    ?event(debug_test, {committed, Label, {explicit, Committed}}),
    ?assert(hb_message:verify(Committed, all, Opts), Label),
    {ok, _, CommittedCommitment} = hb_message:commitment(#{}, Committed, Opts),
    ?assertEqual(
        [<<"list">>], hb_maps:get(<<"committed">>, CommittedCommitment, Opts),
        Label),
    ?assertEqual(ToBool(Commit),
        hb_util:atom(hb_ao:get(<<"bundle">>, CommittedCommitment, false, Opts)),
        Label),
    
    Encoded = hb_message:convert(Committed, 
        #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => ToBool(Encode) },
        <<"structured@1.0">>, Opts),
    ?event(debug_test, {encoded, Label, {explicit, Encoded}}),
    ?assert(ar_bundles:verify_item(Encoded), Label),
    %% IF the input message is unbundled, #tx.data should be empty.
    ?assertEqual(ToBool(Commit), Encoded#tx.data /= <<>>, Label),

    Decoded = hb_message:convert(Encoded, 
        #{ <<"device">> => <<"structured@1.0">>, <<"bundle">> => ToBool(Decode) },
        #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => ToBool(Encode) },
        Opts),
    ?event(debug_test, {decoded, Label, {explicit, Decoded}}),
    ?assert(hb_message:verify(Decoded, all, Opts), Label),
    {ok, _, DecodedCommitment} = hb_message:commitment(#{}, Decoded, Opts),
    ?assertEqual(
        [<<"list">>], hb_maps:get(<<"committed">>, DecodedCommitment, Opts),
        Label),
    ?assertEqual(ToBool(Commit),
        hb_util:atom(hb_ao:get(<<"bundle">>, DecodedCommitment, false, Opts)),
        Label),
    case Commit of
        unbundled ->
            ?assertNotEqual([1, 2, 3], maps:get(<<"list">>, Decoded, Opts), Label);
        bundled ->
            ?assertEqual([1, 2, 3], maps:get(<<"list">>, Decoded, Opts), Label)
    end,
    ok.

bundle_uncommitted_test() ->
    test_bundle_uncommitted(unbundled, unbundled),
    test_bundle_uncommitted(unbundled, bundled),
    test_bundle_uncommitted(bundled, unbundled),
    test_bundle_uncommitted(bundled, bundled),
    ok.

test_bundle_uncommitted(Encode, Decode) ->
    Opts = #{},
    Structured = #{ <<"list">> => [1, 2, 3] },
    ToBool = fun(unbundled) -> false; (bundled) -> true end,
    Label = lists:flatten(io_lib:format("~p -> ~p", [Encode, Decode])),

    Encoded = hb_message:convert(Structured, 
        #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => ToBool(Encode) },
        <<"structured@1.0">>, Opts),
    ?event(debug_test, {encoded, Label, {explicit, Encoded}}),
    %% IF the input message is unbundled, #tx.data should be empty.
    ?assertEqual(ToBool(Encode), Encoded#tx.data /= <<>>, Label),

    Decoded = hb_message:convert(Encoded, 
        #{ <<"device">> => <<"structured@1.0">>, <<"bundle">> => ToBool(Decode) },
        #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => ToBool(Encode) },
        Opts),
    ?event(debug_test, {decoded, Label, {explicit, Decoded}}),
    case Encode of
        unbundled ->
            ?assertNotEqual([1, 2, 3], maps:get(<<"list">>, Decoded, Opts), Label);
        bundled ->
            ?assertEqual([1, 2, 3], maps:get(<<"list">>, Decoded, Opts), Label)
    end,
    ok.