%%% @doc Codec for managing transformations from `ar_bundles'-style Arweave TX
%%% records to and from TABMs.
-module(dev_codec_ans104).
-export([to/3, from/3, commit/3, verify/3, content_type/1]).
-export([serialize/3, deserialize/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

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

%% The list of tags that a user is explicitly committing to when they sign an
%% ANS-104 message.
-define(BASE_COMMITTED_TAGS, ?TX_KEYS ++ [<<"data">>]).

%% @doc Return the content type for the codec.
content_type(_) -> {ok, <<"application/ans104">>}.

%% @doc Serialize a message or TX to a binary.
serialize(Msg, Req, Opts) when is_map(Msg) ->
    {ok, TX} = to(Msg, Req, Opts),
    serialize(TX, Req, Opts);
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
commit(Msg, Req, Opts) ->
    hb_tx:commit_message(<<"ans104@1.0">>, Msg, Req, Opts).

%% @doc Verify an ANS-104 commitment.
verify(Msg, Req, Opts) ->
    hb_tx:verify_message(<<"ans104@1.0">>, Msg, Req, Opts).

%% @doc Convert a #tx record into a message map recursively.
from(Binary, _Req, _Opts) when is_binary(Binary) -> {ok, Binary};
from(TX = #tx{ format = ans104 }, Req, Opts) ->
    TABM = hb_tx:tx_to_tabm(TX, ?BASE_COMMITTED_TAGS, Req, Opts),
    {ok, TABM};
from(TX, _Req, _Opts) when is_record(TX, tx) ->
    ?event({invalid_ans104_tx_format, {format, TX#tx.format}, {tx, TX}}),
    throw(invalid_tx).

%% @doc Translate a message to its #tx record representation,
%% which can then be used by ar_bundles to serialize the message. We call the 
%% message's device in order to get the keys that we will be checkpointing. We 
%% do this recursively to handle nested messages. The base case is that we hit
%% a binary, which we return as is.
to(Binary, _Req, _Opts) when is_binary(Binary) ->
    {ok, hb_tx:binary_to_tx(Binary)};
to(TX, _Req, _Opts) when is_record(TX, tx) -> {ok, TX};
to(InputTABM, Req, Opts) when is_map(InputTABM) ->
    {ok, hb_tx:tabm_to_tx(#tx{ format = ans104 }, InputTABM, Req, Opts)};
to(_Other, _Req, _Opts) ->
    throw(invalid_tx).

%%% ------------------------------------------------------------------------------------------
%%% ANS-104-specific testing cases.
%%% ------------------------------------------------------------------------------------------

from_maintains_tag_name_case_test() ->
    TX = #tx {
        tags = [
            {<<"Test-Tag">>, <<"test-value">>}
        ]
    },
    SignedTX = ar_bundles:sign_item(TX, hb:wallet()),
    ?assert(ar_bundles:verify_item(SignedTX)),

    TABM = hb_util:ok(from(SignedTX, #{}, #{})),

    % Straight conversion
    ConvertedTX0 = hb_util:ok(to(TABM, #{}, #{})),
    ?assert(ar_bundles:verify_item(ConvertedTX0)),
    ?assertEqual(ConvertedTX0, hb_tx:normalize(SignedTX)),

    % Serialize/deserialize then convert
    Binary = hb_util:ok(serialize(TABM, #{}, #{})),
    DeserializedTABM = hb_util:ok(deserialize(Binary, #{}, #{})),
    ConvertedTX1 = hb_util:ok(to(DeserializedTABM, #{}, #{})),
    ?assert(ar_bundles:verify_item(ConvertedTX1)),
    ?assertEqual(ConvertedTX1, hb_tx:normalize(SignedTX)),

    ?assertEqual(ConvertedTX0, ConvertedTX1).

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
    OnlyCommitted = hb_message:with_only_committed(SignedMsg, Opts),
    {ok, ID} = hb_cache:write(SignedMsg, Opts),
    {ok, ReadMsg} = hb_cache:read(SignedID, Opts),
    {ok, ReadTX} = to(ReadMsg, #{}, Opts),
    ?assert(hb_message:match(ReadMsg, SignedMsg)),
    ?assert(ar_bundles:verify_item(ReadTX)).

unsigned_duplicated_tag_name_test() ->
    InputTX = hb_tx:reset_ids(hb_tx:normalize(#tx {
        tags = [
            {<<"Test-Tag">>, <<"test-value">>},
            {<<"test-tag">>, <<"test-value-2">>}
        ]
    })),

    % ans104 -> structured -> ans104
    Structured = hb_message:convert(InputTX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ConvertedTX0 = hb_message:convert(Structured, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
    ?assertEqual(InputTX, ConvertedTX0),

    % serialize/deserialize
    TABM = hb_util:ok(from(InputTX, #{}, #{})),
    Binary = hb_util:ok(serialize(TABM, #{}, #{})),
    DeserializedTABM = hb_util:ok(deserialize(Binary, #{}, #{})),
    ConvertedTX1 = hb_util:ok(to(DeserializedTABM, #{}, #{})),
    ?assertEqual(InputTX, ConvertedTX1).

signed_duplicated_tag_name_test() ->
    InputTX = ar_bundles:sign_item(#tx {
        tags = [
            {<<"Test-Tag">>, <<"test-value">>},
            {<<"test-tag">>, <<"test-value-2">>}
        ]
    }, ar_wallet:new()),

    % ans104 -> structured -> ans104
    Structure = hb_message:convert(InputTX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ConvertedTX0 = hb_message:convert(Structure, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
    ?assertEqual(InputTX, ConvertedTX0),
    ?assert(ar_bundles:verify_item(ConvertedTX0)),

    % serialize/deserialize
    TABM = hb_util:ok(from(InputTX, #{}, #{})),
    Binary = hb_util:ok(serialize(TABM, #{}, #{})),
    DeserializedTABM = hb_util:ok(deserialize(Binary, #{}, #{})),
    ConvertedTX1 = hb_util:ok(to(DeserializedTABM, #{}, #{})),
    ?assertEqual(InputTX, ConvertedTX1),
    ?assert(ar_bundles:verify_item(ConvertedTX1)).

only_committed_maintains_target_test() ->
    TX = ar_bundles:sign_item(#tx {
        target = crypto:strong_rand_bytes(32),
        tags = [
            {<<"test-tag">>, <<"test-value">>},
            {<<"test-tag-2">>, <<"test-value-2">>}
        ],
        data = <<"test-data">>
    }, ar_wallet:new()),
    Decoded = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    {ok, OnlyCommitted} = hb_message:with_only_committed(Decoded, #{}),
    Encoded = hb_message:convert(OnlyCommitted, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
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
    InputStructured =
        hb_message:commit(
            #{
                <<"other-key">> => <<"Normal value">>,
                <<"body">> => <<"Body value">>
            },
            #{ priv_wallet => hb:wallet() },
            <<"ans104@1.0">>
        ),

    % Straight conversion
    ConvertedTX0 = hb_message:convert(InputStructured, <<"ans104@1.0">>, #{}),
    ?assertEqual(<<"Body value">>, ConvertedTX0#tx.data),
    Structured0 = hb_message:convert(ConvertedTX0, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?assert(hb_message:verify(Structured0, all, #{})),
    ?assertEqual(InputStructured, Structured0),

    % Serialize/deserialize
    TABM = hb_message:convert(InputStructured, tabm, <<"structured@1.0">>, #{}),
    Binary = hb_util:ok(serialize(TABM, #{}, #{})),
    DeserializedTABM = hb_util:ok(deserialize(Binary, #{}, #{})),
    ConvertedTX1 = hb_util:ok(to(DeserializedTABM, #{}, #{})),
    ?assertEqual(<<"Body value">>, ConvertedTX1#tx.data),
    Structured1 = hb_message:convert(ConvertedTX1, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?assert(hb_message:verify(Structured1, all, #{})),
    ?assertEqual(InputStructured, Structured1),
    ?assertEqual(ConvertedTX0, ConvertedTX1).

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

bundle_list_test_disabled() ->
    % The targets and anchors are just snapshots of random data so that the test data is
    % consistent from run to run.
    Item1 = ar_bundles:new_item(
        Target1 = hb_util:decode(<<"2zgCyu_zlyfnygXm2kjiDUlFJ89FCy3mB2FeG02yE3w">>),
        Anchor1 = hb_util:decode(<<"GduWy-O8vVnefm6ESMt3thz8jo8jGJpOZk92ek_Wi6A">>),
        [],
        Data1 = <<"test-data1">>
    ),
    Item2 = ar_bundles:new_item(
        Target2 = hb_util:decode(<<"TZNaMfvTa86OAAamlrKzvyieg1W9gl6IlH0Hkc0272w">>),
        Anchor2 = hb_util:decode(<<"mWJQNN6TBSoT8cc1sNIegxiv5HHA8bKT7rVphYL7NkY">>),
        [{<<"tag1">>, <<"value1">>}, {<<"tag2">>, <<"value2">>}],
        Data2 = <<"test-data2">>
    ),
    Items = [Item1, Item2],

    ExpectedTABM = #{
        <<"data">> => #{
            <<"ao-types">> => <<".=\"list\"">>,
            <<"1">> => #{
                <<"data">> => Data1,
                <<"target">> => Target1,
                <<"last_tx">> => Anchor1
            },
            <<"2">> => #{
                <<"data">> => Data2,
                <<"target">> => Target2,
                <<"last_tx">> => Anchor2,
                <<"tag1">> => <<"value1">>,
                <<"tag2">> => <<"value2">>
            }
        }
    },

    % :to will normalize the #tx
    ExpectedTX = hb_tx:normalize(#tx{ data = Items }),
    ExpectedDataHash = hb_util:decode(<<"N0yukFcLK7HFFWgftYgDcnmYcfGwt1ZvS3UBwJU6n0E">>),

    Opts = #{},

    % 1. Roundtrip with bundle as list
    InputTX0 = hb_tx:reset_ids(#tx{ data = Items }),
    {ok, TABM0} = dev_codec_ans104:from(InputTX0, #{}, Opts),
    {ok, OutputTX0} = dev_codec_ans104:to(TABM0, #{}, Opts),

    ?assertEqual(ExpectedTABM, TABM0),
    ?assertEqual(ExpectedDataHash, crypto:hash(sha256, OutputTX0#tx.data)),
    ?assertEqual(ExpectedTX, OutputTX0),    

    % 2. Roundtrip with bundle as serialized list
    TX1 = ar_bundles:serialize_bundle_data(Items, #tx{}),
    {ok, TABM1} = dev_codec_ans104:from(TX1, #{}, Opts),
    {ok, OutputTX1} = dev_codec_ans104:to(TABM1, #{}, Opts),

    ?assertEqual(ExpectedTABM, TABM1),
    ?assertEqual(ExpectedDataHash, crypto:hash(sha256, OutputTX1#tx.data)),
    ?assertEqual(ExpectedTX, OutputTX1),    

    % 3. Roundtrip with bundle as serialized list without bundle tags
    TX2 = TX1#tx{ tags = [] },
    {ok, TABM2} = dev_codec_ans104:from(TX2, #{}, Opts),
    {ok, OutputTX2} = dev_codec_ans104:to(TABM2, #{}, Opts),

    ?assertEqual(#{ <<"data">> => TX2#tx.data }, TABM2),
    ?assertEqual(ExpectedDataHash, crypto:hash(sha256, OutputTX2#tx.data)),
    ?assertEqual(hb_tx:normalize(TX2), OutputTX2),    
    ok.

% XXX test map as well as just a sing #tx as the data

roundtrip_test() ->
    LastTX = hb_util:decode(<<"UJW0lZZV4F1HmAXz5uUyIGG4VCwBGsaBp9P5LX7NbnY">>),
    Target = hb_util:decode(<<"YxU84G7_N29RNC2WvWs2xY1Felml35Pug8mglh21REc">>),
    DataRoot = hb_util:decode(<<"EU5KVrF-Vm8WKIXyNGSH2VdXl9RelyTh9lck0AoknAA">>),
    BinaryTag = hb_util:decode(<<"FxrvGdV-V0Quj1aAsnDUfO6nk8IaWHguRmjNEmka_ec">>),
    Data = <<"test-data">>,
    TestCases = [
        {defaults_typed,
            #{
                <<"format">> => ans104,
                <<"last_tx">> => <<>>,
                <<"target">> => <<>>,
                <<"quantity">> => 0,
                <<"data">> => ?DEFAULT_DATA,
                <<"manifest">> => undefined,
                <<"data_root">> => <<>>,
                <<"reward">> => 0,
                <<"denomination">> => 0,
                <<"signature_type">> => ?RSA_KEY_TYPE
            },
            #tx{
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
            }
        },
        {defaults_binary,
            #{
                <<"format">> => <<"ans104">>,
                <<"last_tx">> => <<>>,
                <<"target">> => <<>>,
                <<"quantity">> => <<"0">>,
                <<"data">> => ?DEFAULT_DATA,
                <<"manifest">> => <<"undefined">>,
                <<"data_root">> => <<>>,
                <<"reward">> => <<"0">>,
                <<"denomination">> => <<"0">>
            },
            #tx{
                unsigned_id = hb_util:decode(<<"EYZkeF9dbMD3mAkaNN2-oLgqwzsswq7_He6TLo6TtWU">>),
                tags = [
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
            }
        },
        {non_defaults_typed,
            #{
                <<"first-tag">> => <<"First-Value">>,
                <<"second-tag">> => <<"second-value">>,
                <<"third-tag">> => 1,
                <<"fourth-tag">> => BinaryTag,
                <<"last_tx">> => LastTX,
                <<"target">> => Target,
                <<"quantity">> => 2,
                <<"data">> => Data,
                <<"manifest">> => <<"test-manifest">>,
                <<"data_root">> => DataRoot,
                <<"reward">> => 3,
                <<"denomination">> => 4
            },
            #tx{
                unsigned_id = hb_util:decode(<<"uL1YvOHq-7w-sLHqEyItW53BaKRWNx7yUcXI75inM-c">>),
                target = Target,
                last_tx = LastTX,
                data = Data,
                data_size = byte_size(Data),
                tags = [
                    {<<"ao-types">>, <<"denomination=\"integer\", quantity=\"integer\", reward=\"integer\", third-tag=\"integer\"">>},
                    {<<"data_root">>, DataRoot},
                    {<<"denomination">>,<<"4">>},
                    {<<"first-tag">>,<<"First-Value">>},
                    {<<"fourth-tag">>, BinaryTag},
                    {<<"manifest">>,<<"test-manifest">>},
                    {<<"quantity">>,<<"2">>},
                    {<<"reward">>,<<"3">>},
                    {<<"second-tag">>,<<"second-value">>},
                    {<<"third-tag">>,<<"1">>}
                ]
            }
        },
        {non_defaults_binary,
            #{
                <<"first-tag">> => <<"First-Value">>,
                <<"second-tag">> => <<"second-value">>,
                <<"third-tag">> => <<"1">>,
                <<"fourth-tag">> => BinaryTag,
                <<"last_tx">> => LastTX,
                <<"target">> => Target,
                <<"quantity">> => <<"2">>,
                <<"data">> => Data,
                <<"manifest">> => <<"test-manifest">>,
                <<"data_root">> => DataRoot,
                <<"reward">> => <<"3">>,
                <<"denomination">> => <<"4">>
            },
            #tx{
                unsigned_id = hb_util:decode(<<"y_UCz2-eUvlSjV86Zb6w8lMj5sjOW8i4orWShbVGXig">>),
                target = Target,
                last_tx = LastTX,
                data = Data,
                data_size = byte_size(Data),
                tags = [
                    {<<"data_root">>, DataRoot},
                    {<<"denomination">>,<<"4">>},
                    {<<"first-tag">>,<<"First-Value">>},
                    {<<"fourth-tag">>, BinaryTag},
                    {<<"manifest">>,<<"test-manifest">>},
                    {<<"quantity">>,<<"2">>},
                    {<<"reward">>,<<"3">>},
                    {<<"second-tag">>,<<"second-value">>},
                    {<<"third-tag">>,<<"1">>}
                ]
            }
        },
        {body_key,
            #{
                <<"body">> => Data
            },
            #tx{
                unsigned_id = hb_util:decode(<<"8LoJYC6Qh1SpE0LiY-9QrY0zQkv5DeOYKdiqk2dyW4Y">>),
                data = Data,
                data_size = byte_size(Data),
                tags = [{<<"ao-data-key">>,<<"body">>}]
            }
        }
    ],
    lists:foreach(
        fun({Label, UnsignedStructured, UnsignedTX}) ->
            do_unsigned_roundtrip(
                lists:flatten(io_lib:format("~p unsigned", [Label])),
                UnsignedStructured, UnsignedTX),
            do_signed_roundtrip(
                lists:flatten(io_lib:format("~p signed", [Label])),
                UnsignedStructured, UnsignedTX)
        end,
        TestCases
    ).

invalid_fields_test() ->
    TestCases = [
        { <<"id">>, #{ <<"id">> => hb_util:encode(crypto:strong_rand_bytes(32)) } },
        { <<"unsigned_id">>, #{ <<"unsigned_id">> => hb_util:encode(crypto:strong_rand_bytes(32)) } },
        { <<"owner">>, #{ <<"owner">> => hb_util:encode(crypto:strong_rand_bytes(512)) } },
        { <<"owner_address">>, #{ <<"owner_address">> => hb_util:encode(crypto:strong_rand_bytes(32)) } },
        { <<"tags">>, #{ <<"tags">> => <<"tags">> } },
        { <<"data_size">>, #{ <<"data_size">> => <<"100">> } },
        { <<"data_tree">>, #{ <<"data_tree">> => hb_util:encode(crypto:strong_rand_bytes(32)) } }
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

invalid_field_test() ->
    Signature = hb_util:encode(crypto:strong_rand_bytes(512)),
    TestCases = [
        { <<"signature">>, #{ <<"signature">> => Signature }, {invalid_field, signature, Signature} }
    ],

    lists:foreach(
        fun({InvalidField, TestCase, ExpectedError}) ->
            hb_test_utils:assert_throws(
                fun dev_codec_ans104:to/3,
                [TestCase, #{}, #{}],
                ExpectedError,
                InvalidField
            )
        end,
        TestCases
    ).


do_unsigned_roundtrip(Label, InputStructured, InputTX) ->
    StructuredCodec = #{<<"device">> => <<"structured@1.0">>, <<"bundle">> => true},
    InputTABM = hb_message:convert(InputStructured, tabm, StructuredCodec, #{}),
    ?event(dev_codec_ans104_tests, {Label, input_tabm, {explicit, InputTABM}}),
    {ok, CommittedTABM} =
        dev_codec_ans104:commit(InputTABM, #{ <<"type">> => <<"unsigned">> }, #{}),
    {ok, Binary} = serialize(InputTABM, #{}, #{}),
    {ok, TABM0} = deserialize(Binary, #{}, #{}),
    {ok, DataItem} = dev_codec_ans104:to(TABM0, #{}, #{}),
    {ok, TABM1} = dev_codec_ans104:from(DataItem, #{}, #{}),

    OutputStructured = hb_message:convert(TABM1, StructuredCodec, tabm, #{}),

    ?event(dev_codec_ans104_tests, {Label, tabm0, {explicit, TABM0}}),
    ?event(dev_codec_ans104_tests, {Label, id, {explicit, hb_util:encode(DataItem#tx.unsigned_id)}}),
    ?event(dev_codec_ans104_tests, {Label, output_structured, {explicit, OutputStructured}}),

    ?event(dev_codec_ans104_tests, {Label, input_tx, {explicit, InputTX}}),
    ?event(dev_codec_ans104_tests, {Label, dataitem, {explicit, DataItem}}),
    ?assertEqual(InputTX, DataItem, Label),

    ?event(dev_codec_ans104_tests, {Label, input_structured, {explicit, InputStructured}}),
    ?event(dev_codec_ans104_tests, {Label, output_structured, {explicit, OutputStructured}}),
    ?assert(hb_message:match(InputStructured, OutputStructured), Label),
    ?assert(hb_message:match(InputTABM, TABM0), Label),
    ?assert(hb_message:match(InputTABM, TABM1), Label),
    ?assert(hb_message:match(InputTABM, CommittedTABM), Label),
    ok.

do_signed_roundtrip(Label, UnsignedStructured, UnsignedTX) ->
    {_, {_, Owner}} = Wallet = ar_wallet:new(),
    Opts = #{ priv_wallet => Wallet },
    StructuredCodec = #{<<"device">> => <<"structured@1.0">>, <<"bundle">> => true},

    TABM0 = hb_message:convert(UnsignedStructured, tabm, StructuredCodec, Opts),
    {ok, SignedTABM0} = 
        dev_codec_ans104:commit(TABM0, #{ <<"type">> => <<"signed">> }, Opts),
    ?assert(hb_util:ok(dev_codec_ans104:verify(SignedTABM0, #{}, Opts)), Label),

    {ok, ID, Commitment} = hb_message:commitment(
        #{ <<"commitment-device">> => <<"ans104@1.0">> }, SignedTABM0, Opts),
    Signature = hb_util:decode(hb_ao:get(<<"signature">>, Commitment, <<>>, Opts)),
    SignedTX = UnsignedTX#tx{ id = hb_util:decode(ID), owner = Owner, signature = Signature },

    ?event(dev_codec_ans104_tests, {Label, signed_id, {explicit, ID}}),

    {ok, Binary} = serialize(SignedTABM0, #{}, #{}),
    {ok, SignedTABM1} = deserialize(Binary, #{}, #{}),
    
    {ok, DataItem} = dev_codec_ans104:to(SignedTABM1, #{}, Opts),
    {ok, SignedTABM2} = dev_codec_ans104:from(DataItem, #{}, Opts),

    {ok, UnsignedTABM0} =
        dev_codec_ans104:commit(SignedTABM0, #{ <<"type">> => <<"unsigned">> }, #{}),

    ?assert(hb_message:match(SignedTABM0, SignedTABM1), Label),
    ?assert(hb_message:match(SignedTABM0, SignedTABM2), Label),
    ?assert(hb_message:match(TABM0, UnsignedTABM0), Label),
    ?assertEqual(SignedTX, DataItem, Label),
    ok.

codec_insensitive_get_test() ->
    TX = ar_bundles:sign_item(
        #tx {
            tags = [{<<"Hello">>, <<"World">>}]
        },
        ar_wallet:new()
    ),
    Structured = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?assertEqual(hb_ao:get(<<"Hello">>, Structured, #{}), <<"World">>),
    ?assertEqual(hb_ao:get(<<"hello">>, Structured, #{}), <<"World">>).