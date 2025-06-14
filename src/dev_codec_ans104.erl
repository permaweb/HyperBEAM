%%% @doc Codec for managing transformations from `ar_bundles'-style Arweave TX
%%% records to and from TABMs.
-module(dev_codec_ans104).
-export([to/3, from/3, commit/3, verify/3, content_type/1]).
-export([serialize/3, deserialize/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% The size at which a value should be made into a body item, instead of a
%% tag.
-define(MAX_TAG_VAL, 4096).
%% The list of TX fields that users can set directly. Data is excluded because
%% it may be set by the codec in order to support nested messages.
-define(TX_KEYS,
    [
        <<"last_tx">>,
        <<"owner">>,
        <<"target">>,
        <<"signature">>
    ]
).

%%% The list of tags that a user is explicitly committing to when they sign an
%%% ANS-104 message.
-define(BASE_COMMITTED_TAGS, ?TX_KEYS ++ [<<"data">>]).
%% List of tags that should be removed during `to'. These relate to the nested
%% ar_bundles format that is used by the `ans104@1.0' codec.
-define(FILTERED_TAGS,
    [
        <<"bundle-format">>,
        <<"bundle-map">>,
        <<"bundle-version">>
    ]
).

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
    ?event({committing, {input, {explicit, Msg}}}),
    Signed =
        ar_bundles:sign_item(
            hb_util:ok(to(hb_private:reset(Msg), Req, Opts)),
            hb_opts:get(priv_wallet, no_viable_wallet, Opts)
        ),
    ?event({committing, {signed, {explicit, Signed}}}),
    SignedTABM =
        hb_message:convert(
            Signed,
            tabm,
            <<"ans104@1.0">>,
            Opts
        ),
    {ok, SignedTABM};
commit(Msg, #{ <<"type">> := <<"unsigned-sha256">> }, Opts) ->
    % Remove the commitments from the message, convert it to ANS-104, then back.
    % This forces the message to be normalized and the unsigned ID to be
    % recalculated.
    TX = hb_message:convert(
        hb_maps:without([<<"commitments">>], Msg, Opts),
        <<"ans104@1.0">>,
        tabm,
        Opts
    ),
    {
        ok,
        hb_message:convert(
            TX,
            tabm,
            <<"ans104@1.0">>,
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
from(#tx{ format = ans104 } = TX, Req, Opts) ->
    case lists:keyfind(<<"ao-type">>, 1, TX#tx.tags) of
        false ->
            do_from(TX, Req, Opts);
        {<<"ao-type">>, <<"binary">>} ->
            {ok, TX#tx.data}
    end.

do_from(RawTX, Req, Opts) ->
    % Ensure the TX is fully deserialized.
    TX = ar_bundles:deserialize(ar_bundles:normalize(RawTX)),

    TABM = hb_tx:tx_to_tabm(TX, <<"ans104@1.0">>, ?BASE_COMMITTED_TAGS, Req, Opts),

    Res = hb_maps:without(?FILTERED_TAGS, TABM, Opts),
    {ok, Res}.

%% @doc Translate a message to its #tx record representation,
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
to(InputTABM, Req, Opts) when is_map(InputTABM) ->
    TX = hb_tx:tabm_to_tx(InputTABM, Req, Opts),
    ?event({to, {result, TX}}),
    {ok, TX};
to(_Other, _Req, _Opts) ->
    throw(invalid_tx).

%%% ------------------------------------------------------------------------------------------
%%% ANS-104-specific testing cases.
%%% ------------------------------------------------------------------------------------------

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
    ?event({restored_msg, {explicit, ReadMsg}}),
    {ok, ReadTX} = to(ReadMsg, #{}, Opts),
    ?event({restored_tx, {explicit, ReadTX}}),
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

only_committed_maintains_target_test() ->
    TX = ar_bundles:sign_item(#tx {
        target = crypto:strong_rand_bytes(32),
        tags = [
            {<<"test-tag">>, <<"test-value">>},
            {<<"test-tag-2">>, <<"test-value-2">>}
        ],
        data = <<"test-data">>
    }, ar_wallet:new()),
    ?event({tx, {explicit, TX}}),
    Decoded = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event({decoded, {explicit, Decoded}}),
    {ok, OnlyCommitted} = hb_message:with_only_committed(Decoded, #{}),
    ?event({only_committed, {explicit, OnlyCommitted}}),
    Encoded = hb_message:convert(OnlyCommitted, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
    ?event({encoded, {explicit, Encoded}}),
    ?event({tx, {explicit, TX}}),
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
        
simple_signed_to_httpsig_test_disabled() ->
    TX =
        ar_bundles:sign_item(
            #tx {
                tags = [
                    {<<"test-tag">>, <<"test-value">>},
                    {<<"test-tag-2">>, <<"test-value-2">>},
                    {<<"Capitalized-Tag">>, <<"test-value-3">>}
                ]
            },
            ar_wallet:new()
        ),
    Structured1 = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event(debug, {tx, TX}),
    TABM = hb_message:convert(TX, tabm, <<"ans104@1.0">>, #{}),
    ?event(debug, {tabm, TABM}),
    HTTPSig = hb_message:convert(TABM, <<"httpsig@1.0">>, tabm, #{}),
    ?event(debug, {httpsig, HTTPSig}),
    Structured2 = hb_message:convert(HTTPSig, <<"structured@1.0">>, <<"httpsig@1.0">>, #{}),
	Match = hb_message:match(Structured1, Structured2, #{}),
    ?event(debug, {match, Match}),
    ?assert(Match),
    ?assert(hb_message:verify(Structured2, all, #{})),
    HTTPSig2 = hb_message:convert(Structured2, <<"httpsig@1.0">>, <<"structured@1.0">>, #{}),
    ?event(debug, {httpsig2, HTTPSig2}),
    ?assert(hb_message:verify(HTTPSig2, all, #{})),
    ?assert(hb_message:match(HTTPSig, HTTPSig2)).


% aotypes_test() ->
%     Msg = #{
%         <<"binary-tag">> => <<"binary-value">>,
%         <<"atom-tag">> => atom_value,
%         <<"integer-tag">> => 123,
%         <<"float-tag">> => 123.456,
%         <<"boolean-tag">> => true,
%         <<"list-tag">> => [1, 2, 3],
%         <<"map-tag">> => #{<<"key">> => <<"value">>}
%     },
%     TABM0 = hb_message:convert(Msg, tabm, <<"structured@1.0">>, #{}),
%     Dataitem = hb_message:convert(TABM0, <<"ans104@1.0">>, tabm, #{}),
%     TABM1 = hb_message:convert(Dataitem, tabm, <<"ans104@1.0">>, #{}),
%     Structured = hb_message:convert(TABM1, <<"structured@1.0">>, tabm, #{}),
%     ?event({tabm, {explicit, TABM0}}),
%     ?event({dataitem, {explicit, Dataitem}}),
%     ?event({tabm, {explicit, TABM1}}),
%     ?event({structured, {explicit, Structured}}),
%     ?event({id, {explicit, Dataitem#tx.unsigned_id}}),
%     ExpectedTX = #tx{
%         unsigned_id = hb_util:decode(<<"MSWJEQCbH_mCmyEuPT45liJ4JSXAXAltYj7ZFGtypPY">>),
%         tags = [
%             {<<"ao-types">>, <<"atom-tag=\"atom\", boolean-tag=\"atom\", float-tag=\"float\", integer-tag=\"integer\"">>},
%             {<<"atom-tag">>, <<"atom_value">>},
%             {<<"binary-tag">>, <<"binary-value">>},
%             {<<"boolean-tag">>, <<"true">>},
%             {<<"float-tag">>, <<"1.23456000000000003070e+02">>},
%             {<<"integer-tag">>, <<"123">>},
%             {<<"list-tag+link">>, <<"LJNSyAg3udG_pxDcNGB0fdNZJ1GT49t7cydlGTRmZLc">>},
%             {<<"map-tag+link">>, <<"C2QtFNMLl1EqNMzRuenooVz-vpXuVDDOdCkiVjIiwSE">>}
%         ]
%     },
%     ?assertEqual(ExpectedTX, Dataitem),
%     ?assert(hb_message:match(Msg, Structured)),
%     ?assert(hb_message:match(TABM0, TABM1)),
%     ok.


set_defaults_test() ->
    UnsignedStructured = #{
        <<"format">> => ans104,
        <<"last_tx">> => <<>>,
        <<"target">> => <<>>,
        <<"quantity">> => 0,
        <<"data">> => ?DEFAULT_DATA,
        <<"manifest">> => undefined,
        <<"data_root">> => <<>>,
        <<"reward">> => 0,
        <<"denomination">> => 0
    },
    UnsignedTX = #tx{
        unsigned_id = hb_util:decode(<<"3eMto8z7IlnQgKPrHjmkrI2ohnrJhnCsss6wc4L86QQ">>),
        tags = [
            {<<"ao-types">>,
                <<
                    "denomination=\"integer\", ",
                    "format=\"atom\", ",
                    "manifest=\"atom\", ",
                    "quantity=\"integer\", ",
                    "reward=\"integer\""
                >>},
            {<<"data">>,?DEFAULT_DATA},
            {<<"data_root">>, <<>>},
            {<<"denomination">>,<<"0">>},
            {<<"format">>,<<"ans104">>},
            {<<"last_tx">>,<<>>},
            {<<"manifest">>,<<"undefined">>},
            {<<"quantity">>,<<"0">>},
            {<<"reward">>,<<"0">>},
            {<<"target">>,<<>>}
        ]
    },
    do_unsigned_roundtrip(UnsignedStructured, UnsignedTX),
    do_signed_roundtrip(UnsignedStructured, UnsignedTX).

invalid_fields_test() ->
    TestCases = [
        { <<"id">>, #{ <<"id">> => hb_util:encode(crypto:strong_rand_bytes(32)) } },
        { <<"unsigned_id">>, #{ <<"unsigned_id">> => hb_util:encode(crypto:strong_rand_bytes(32)) } },
        { <<"owner">>, #{ <<"owner">> => hb_util:encode(crypto:strong_rand_bytes(512)) } },
        { <<"owner_address">>, #{ <<"owner_address">> => hb_util:encode(crypto:strong_rand_bytes(32)) } },
        { <<"tags">>, #{ <<"tags">> => <<"tags">> } },
        { <<"data_size">>, #{ <<"data_size">> => <<"100">> } },
        { <<"data_tree">>, #{ <<"data_tree">> => hb_util:encode(crypto:strong_rand_bytes(32)) } },
        { <<"signature">>, #{ <<"signature">> => hb_util:encode(crypto:strong_rand_bytes(512)) } }
    ],

    lists:foreach(
        fun({InvalidField, TestCase}) ->
            hb_test_utils:assert_throws(
                fun dev_codec_ans104:to/3,
                [TestCase, #{}, #{}],
                {invalid_fields, [InvalidField]},
                InvalidField
            )
        end,
        TestCases
    ).

do_unsigned_roundtrip(UnsignedStructured, UnsignedTX) ->
    StructuredCodec = #{<<"device">> => <<"structured@1.0">>, <<"bundle">> => true},
    TABM0 = hb_message:convert(UnsignedStructured, tabm, StructuredCodec, #{}),
    {ok, CommittedTABM0} =
        dev_codec_ans104:commit(TABM0, #{ <<"type">> => <<"unsigned">> }, #{}),
    {ok, DataItem} = dev_codec_ans104:to(TABM0, #{}, #{}),
    {ok, TABM1} = dev_codec_ans104:from(DataItem, #{}, #{}),
    Structured = hb_message:convert(TABM1, StructuredCodec, tabm, #{}),
    ?assertEqual(UnsignedTX, DataItem),
    ?assert(hb_message:match(UnsignedStructured, Structured)),
    ?assert(hb_message:match(TABM0, TABM1)),
    ?assert(hb_message:match(TABM0, CommittedTABM0)),
    ok.

do_signed_roundtrip(UnsignedStructured, UnsignedTX) ->
    {_, {_, Owner}} = Wallet = ar_wallet:new(),
    Opts = #{ priv_wallet => Wallet },
    StructuredCodec = #{<<"device">> => <<"structured@1.0">>, <<"bundle">> => true},

    TABM0 = hb_message:convert(UnsignedStructured, tabm, StructuredCodec, Opts),
    {ok, SignedTABM0} = 
        dev_codec_ans104:commit(TABM0, #{ <<"type">> => <<"signed">> }, Opts),
    ?assert(hb_util:ok(dev_codec_ans104:verify(SignedTABM0, #{}, Opts))),
    {ok, ID, Commitment} = hb_message:commitment(
        #{ <<"commitment-device">> => <<"ans104@1.0">> }, SignedTABM0, Opts),
    Signature = hb_util:decode(hb_ao:get(<<"signature">>, Commitment, <<>>, Opts)),
    SignedTX = UnsignedTX#tx{ id = hb_util:decode(ID), owner = Owner, signature = Signature },
    ?event({signed_id, {explicit, ID}}),
    {ok, DataItem} = dev_codec_ans104:to(SignedTABM0, #{}, Opts),
    {ok, SignedTABM1} = dev_codec_ans104:from(DataItem, #{}, Opts),

    {ok, UnsignedTABM0} =
        dev_codec_ans104:commit(SignedTABM0, #{ <<"type">> => <<"unsigned">> }, #{}),

    ?assert(hb_message:match(SignedTABM0, SignedTABM1)),
    ?assert(hb_message:match(TABM0, UnsignedTABM0)),
    ?assertEqual(SignedTX, DataItem),
    ok.