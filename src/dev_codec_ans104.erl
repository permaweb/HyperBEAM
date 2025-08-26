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
    TX = ar_bundles:deserialize(ar_bundles:normalize(RawTX)),
    ?event({parsed_tx, TX}),
    % Get the fields, tags, and data from the TX.
    Fields = dev_codec_ans104_from:fields(TX, <<>>, Opts),
    Tags = dev_codec_ans104_from:tags(TX, Opts),
    Data = dev_codec_ans104_from:data(TX, Req, Tags, Opts),
    ?event({parsed_components, {fields, Fields}, {tags, Tags}, {data, Data}}),
    % Calculate the committed keys on from the TX.
    Keys = dev_codec_ans104_from:committed(
        ?BASE_FIELDS, TX, Fields, Tags, Data, Opts),
    ?event({determined_committed_keys, Keys}),
    % Create the base message from the fields, tags, and data, filtering to
    % include only the keys that are committed. Will throw if a key is missing.
    Base = dev_codec_ans104_from:base(Keys, Fields, Tags, Data, Opts),
    ?event({calculated_base_message, Base}),
    % Add the commitments to the message if the TX has a signature.
    CommittedFields = dev_codec_ans104_from:fields(TX, ?FIELD_PREFIX, Opts),
    WithCommitments = dev_codec_ans104_from:with_commitments(
        TX, <<"ans104@1.0">>, CommittedFields, Tags, Base, Keys, Opts),
    ?event({parsed_message, WithCommitments}),
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
    ?event({to, {inbound, RawTABM}, {req, Req}}),
    MaybeBundle = dev_codec_ans104_to:maybe_load(RawTABM, Req, Opts),
    TX0 = dev_codec_ans104_to:siginfo(
        MaybeBundle, <<"ans104@1.0">>,
        fun dev_codec_ans104_to:fields_to_tx/4,
        Opts
    ),
    ?event({found_siginfo, TX0}),
    % Calculate and normalize the `data', if applicable.
    Data = dev_codec_ans104_to:data(MaybeBundle, Req, Opts),
    ?event({calculated_data, Data}),
    TX1 = TX0#tx { data = Data },
    % Calculate the tags for the TX.
    Tags = dev_codec_ans104_to:tags(
        TX1, <<"ans104@1.0">>, MaybeBundle, Data,
        fun dev_codec_ans104_to:excluded_tags/3, Opts),
    ?event({calculated_tags, Tags}),
    TX2 = TX1#tx { tags = Tags },
    ?event({tx_before_id_gen, TX2}),
    Res =
        try ar_bundles:reset_ids(ar_bundles:normalize(TX2))
        catch
            Type:Error:Stacktrace ->
                ?event({{reset_ids_error, Error}, {tx_without_data, TX2}}),
                ?event({prepared_tx_before_ids,
                    {tags, {explicit, TX2#tx.tags}},
                    {data, TX2#tx.data}
                }),
                erlang:raise(Type, Error, Stacktrace)
        end,
    ?event({to_result, Res}),
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
    ?assertEqual(ConvertedTX, ar_bundles:normalize(SignedTX)).

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
    TX = ar_bundles:reset_ids(ar_bundles:normalize(#tx {
        tags = [
            {<<"Test-Tag">>, <<"test-value">>},
            {<<"test-tag">>, <<"test-value-2">>}
        ]
    })),
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
                tags = [
                    {<<"test-tag">>, <<"test-value">>},
                    {<<"test-tag-2">>, <<"test-value-2">>}
                ],
                data = <<"test-data">>
            },
            ar_wallet:new()
        ),
    EncodedTarget = hb_util:encode(TX#tx.target),
    ?event({tx, TX}),
    Decoded = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event({decoded, Decoded}),
    ?assertEqual(EncodedTarget, hb_maps:get(<<"target">>, Decoded, undefined, #{})),
    {ok, OnlyCommitted} = hb_message:with_only_committed(Decoded, #{}),
    ?event({only_committed, OnlyCommitted}),
    ?assertEqual(EncodedTarget, hb_maps:get(<<"target">>, OnlyCommitted, undefined, #{})),
    Encoded = hb_message:convert(OnlyCommitted, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
    ?assertEqual(TX#tx.target, Encoded#tx.target),
    ?event({result, {initial, TX}, {result, Encoded}}),
    ?assertEqual(TX, Encoded).

% @doc Ensure that items made inside HyperBEAM use the tags to encode `target'
% values, rather than the `target' field.
generate_item_with_target_tag_test() ->
    Msg =
        #{
            <<"target">> => Target = <<"NON-ID-TARGET">>,
            <<"other-key">> => <<"other-value">>
        },
    {ok, TX} = to(Msg, #{}, #{}),
    ?event({encoded_tx, TX}),
    % The encoded TX should have ignored the `target' field, setting a tag instead.
    ?assertEqual(?DEFAULT_TARGET, TX#tx.target),
    Decoded = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event({decoded, Decoded}),
    % The decoded message should have the `target' key set to the tag value.
    ?assertEqual(Target, hb_maps:get(<<"target">>, Decoded, undefined, #{})),
    {ok, OnlyCommitted} = hb_message:with_only_committed(Decoded, #{}),
    ?event({only_committed, OnlyCommitted}),
    % The target key should have been committed.
    ?assertEqual(Target, hb_maps:get(<<"target">>, OnlyCommitted, undefined, #{})),
    Encoded = hb_message:convert(OnlyCommitted, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
    ?event({result, {initial, TX}, {result, Encoded}}),
    ?assertEqual(TX, Encoded).

generate_item_with_target_field_test() ->
    Msg =
        hb_message:commit(
            #{
                <<"target">> => Target = hb_util:encode(crypto:strong_rand_bytes(32)),
                <<"other-key">> => <<"other-value">>
            },
            #{ priv_wallet => hb:wallet() },
            <<"ans104@1.0">>
        ),
    {ok, TX} = to(Msg, #{}, #{}),
    ?event({encoded_tx, TX}),
    ?assertEqual(Target, hb_util:encode(TX#tx.target)),
    Decoded = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event({decoded, Decoded}),
    ?assertEqual(Target, hb_maps:get(<<"target">>, Decoded, undefined, #{})),
    {ok, OnlyCommitted} = hb_message:with_only_committed(Decoded, #{}),
    ?event({only_committed, OnlyCommitted}),
    ?assertEqual(Target, hb_maps:get(<<"target">>, OnlyCommitted, undefined, #{})),
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
    ?event(debug_test, {msg, Structured}),
    HTTPSig =
        hb_message:convert(
            Structured,
            <<"httpsig@1.0">>,
            <<"structured@1.0">>,
            #{}
        ),
    ?event(debug_test, {httpsig, HTTPSig}),
    Structured2 =
        hb_message:convert(
            HTTPSig,
            <<"structured@1.0">>,
            <<"httpsig@1.0">>,
            #{}
        ),
    ?event(debug_test, {decoded, Structured2}),
	Match = hb_message:match(Structured, Structured2, #{}),
    ?assert(Match),
    ?assert(hb_message:verify(Structured2, all, #{})),
    HTTPSig2 = hb_message:convert(Structured2, <<"httpsig@1.0">>, <<"structured@1.0">>, #{}),
    ?event(debug_test, {httpsig2, HTTPSig2}),
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
    ?event(debug_test, {tx, TX}),
    {ok, TABM} = dev_codec_ans104:from(TX, #{}, #{}),
    ?event(debug_test, {tabm, TABM}),
    {ok, Decoded} = dev_codec_ans104:to(TABM, #{}, #{}),
    ?event(debug_test, {decoded, Decoded}),
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