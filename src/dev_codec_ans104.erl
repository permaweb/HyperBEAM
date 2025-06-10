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
commit(Msg, Req, Opts) ->
    hb_tx:commit_message(<<"ans104@1.0">>, Msg, Req, Opts).

%% @doc Verify an ANS-104 commitment.
verify(Msg, Req, Opts) ->
    hb_tx:verify_message(<<"ans104@1.0">>, Msg, Req, Opts).

%% @doc Convert a #tx record into a message map recursively.
from(Binary, _Req, _Opts) when is_binary(Binary) -> {ok, Binary};
from(TX = #tx{ format = ans104 }, Req, Opts) ->
    TABM = hb_tx:tx_to_tabm(TX, ?TX_KEYS, ?BASE_COMMITTED_TAGS, Req, Opts),
    {ok, TABM};
from(TX, _Req, _Opts) when is_record(TX, tx) ->
    ?event({invalid_ans104_tx_format, {format, TX#tx.format}, {tx, TX}}),
    throw(invalid_tx).

%% @doc Internal helper to translate a message to its #tx record representation,
%% which can then be used by ar_bundles to serialize the message. We call the 
%% message's device in order to get the keys that we will be checkpointing. We 
%% do this recursively to handle nested messages. The base case is that we hit
%% a binary, which we return as is.
to(Binary, _Req, _Opts) when is_binary(Binary) ->
    {ok, hb_tx:binary_to_tx(Binary)};
to(TX, _Req, _Opts) when is_record(TX, tx) -> {ok, TX};
to(NormTABM, Req, Opts) when is_map(NormTABM) ->
    {ok, hb_tx:tabm_to_tx(NormTABM, Req, Opts)};
to(_Other, _Req, _Opts) ->
    throw(invalid_tx).

%%% ANS-104-specific testing cases.

% normal_tags_test() ->
%     Msg = #{
%         <<"first-tag">> => <<"first-value">>,
%         <<"second-tag">> => <<"second-value">>
%     },
%     {ok, Encoded} = to(Msg, #{}, #{}),
%     ?event({encoded, Encoded}),
%     {ok, Decoded} = from(Encoded, #{}, #{}),
%     ?event({decoded, Decoded}),
%     ?assert(hb_message:match(Msg, Decoded)).

% from_maintains_tag_name_case_test() ->
%     TX = #tx {
%         tags = [
%             {<<"Test-Tag">>, <<"test-value">>}
%         ]
%     },
%     SignedTX = ar_bundles:sign_item(TX, hb:wallet()),
%     ?event({signed_tx, SignedTX}),
%     ?assert(ar_bundles:verify_item(SignedTX)),
%     TABM = hb_util:ok(from(SignedTX, #{}, #{})),
%     ?event({tabm, TABM}),
%     ConvertedTX = hb_util:ok(to(TABM, #{}, #{})),
%     ?event({converted_tx, ConvertedTX}),
%     ?assert(ar_bundles:verify_item(ConvertedTX)),
%     ?assertEqual(ConvertedTX, hb_tx:normalize(SignedTX)).

% restore_tag_name_case_from_cache_test() ->
%     Opts = #{ store => hb_test_utils:test_store() },
%     TX = #tx {
%         tags = [
%             {<<"Test-Tag">>, <<"test-value">>},
%             {<<"test-tag-2">>, <<"test-value-2">>}
%         ]
%     },
%     SignedTX = ar_bundles:sign_item(TX, ar_wallet:new()),
%     SignedMsg =
%         hb_message:convert(
%             SignedTX,
%             <<"structured@1.0">>,
%             <<"ans104@1.0">>,
%             Opts
%         ),
%     SignedID = hb_message:id(SignedMsg, all),
%     ?event({signed_msg, SignedMsg}),
%     OnlyCommitted = hb_message:with_only_committed(SignedMsg, Opts),
%     ?event({only_committed, OnlyCommitted}),
%     {ok, ID} = hb_cache:write(SignedMsg, Opts),
%     ?event({id, ID}),
%     {ok, ReadMsg} = hb_cache:read(SignedID, Opts),
%     ?event({restored_msg, ReadMsg}),
%     {ok, ReadTX} = to(ReadMsg, #{}, Opts),
%     ?event({restored_tx, ReadTX}),
%     ?assert(hb_message:match(ReadMsg, SignedMsg)),
%     ?assert(ar_bundles:verify_item(ReadTX)).

% unsigned_duplicated_tag_name_test() ->
%     TX = hb_tx:reset_ids(hb_tx:normalize(#tx {
%         tags = [
%             {<<"Test-Tag">>, <<"test-value">>},
%             {<<"test-tag">>, <<"test-value-2">>}
%         ]
%     })),
%     Msg = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
%     ?event({msg, Msg}),
%     TX2 = hb_message:convert(Msg, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
%     ?event({tx2, TX2}),
%     ?assertEqual(TX, TX2).

% signed_duplicated_tag_name_test() ->
%     TX = ar_bundles:sign_item(#tx {
%         tags = [
%             {<<"Test-Tag">>, <<"test-value">>},
%             {<<"test-tag">>, <<"test-value-2">>}
%         ]
%     }, ar_wallet:new()),
%     Msg = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
%     ?event({msg, Msg}),
%     TX2 = hb_message:convert(Msg, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
%     ?event({tx2, TX2}),
%     ?assertEqual(TX, TX2),
%     ?assert(ar_bundles:verify_item(TX2)).
    
% simple_to_conversion_test() ->
%     Msg = #{
%         <<"first-tag">> => <<"first-value">>,
%         <<"second-tag">> => <<"second-value">>
%     },
%     {ok, Encoded} = to(Msg, #{}, #{}),
%     ?event({encoded, Encoded}),
%     {ok, Decoded} = from(Encoded, #{}, #{}),
%     ?event({decoded, Decoded}),
%     ?assert(hb_message:match(Msg, hb_message:uncommitted(Decoded, #{}))).

% only_committed_maintains_target_test() ->
%     TX = ar_bundles:sign_item(#tx {
%         target = crypto:strong_rand_bytes(32),
%         tags = [
%             {<<"test-tag">>, <<"test-value">>},
%             {<<"test-tag-2">>, <<"test-value-2">>}
%         ],
%         data = <<"test-data">>
%     }, ar_wallet:new()),
%     ?event({tx, TX}),
%     Decoded = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
%     ?event({decoded, Decoded}),
%     {ok, OnlyCommitted} = hb_message:with_only_committed(Decoded, #{}),
%     ?event({only_committed, OnlyCommitted}),
%     Encoded = hb_message:convert(OnlyCommitted, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
%     ?event({encoded, Encoded}),
%     ?assertEqual(TX, Encoded).

% type_tag_test() ->
%     TX =
%         ar_bundles:sign_item(
%             #tx {
%                 tags = [{<<"type">>, <<"test-value">>}]
%             },
%             ar_wallet:new()
%         ),
%     ?event({tx, TX}),
%     Structured = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
%     ?event({structured, Structured}),
%     TX2 = hb_message:convert(Structured, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
%     ?event({after_conversion, TX2}),
%     ?assertEqual(TX, TX2).
    
% simple_signed_to_httpsig_test_disabled() ->
%     TX =
%         ar_bundles:sign_item(
%             #tx {
%                 tags = [
%                     {<<"test-tag">>, <<"test-value">>},
%                     {<<"test-tag-2">>, <<"test-value-2">>},
%                     {<<"Capitalized-Tag">>, <<"test-value-3">>}
%                 ]
%             },
%             ar_wallet:new()
%         ),
%     Structured1 = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
%     ?event(debug, {tx, TX}),
%     TABM = hb_message:convert(TX, tabm, <<"ans104@1.0">>, #{}),
%     ?event(debug, {tabm, TABM}),
%     HTTPSig = hb_message:convert(TABM, <<"httpsig@1.0">>, tabm, #{}),
%     ?event(debug, {httpsig, HTTPSig}),
%     Structured2 = hb_message:convert(HTTPSig, <<"structured@1.0">>, <<"httpsig@1.0">>, #{}),
% 	Match = hb_message:match(Structured1, Structured2, #{}),
%     ?event(debug, {match, Match}),
%     ?assert(Match),
%     ?assert(hb_message:verify(Structured2, all, #{})),
%     HTTPSig2 = hb_message:convert(Structured2, <<"httpsig@1.0">>, <<"structured@1.0">>, #{}),
%     ?event(debug, {httpsig2, HTTPSig2}),
%     ?assert(hb_message:verify(HTTPSig2, all, #{})),
%     ?assert(hb_message:match(HTTPSig, HTTPSig2)).


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
    Msg = #{
        <<"format">> => ans104,
        <<"id">> => ?DEFAULT_ID,
        <<"unsigned_id">> => ?DEFAULT_ID,
        <<"last_tx">> => <<>>,
        <<"owner">> => ?DEFAULT_OWNER,
        <<"owner_address">> => not_set,
        <<"tags">> => [],
        <<"target">> => <<>>,
        <<"quantity">> => 0,
        <<"data">> => ?DEFAULT_DATA,
        <<"manifest">> => undefined,
        <<"data_size">> => 0,
        <<"data_tree">> => [],
        <<"data_root">> => <<>>,
        <<"signature">> => ?DEFAULT_SIG,
        <<"reward">> => 0,
        <<"denomination">> => 0,
        <<"signature_type">> => ?RSA_KEY_TYPE
    },
    TABM0 = hb_message:convert(Msg, tabm, <<"structured@1.0">>, #{}),
    Dataitem = hb_message:convert(TABM0, <<"ans104@1.0">>, tabm, #{}),
    TABM1 = hb_message:convert(Dataitem, tabm, <<"ans104@1.0">>, #{}),
    Structured = hb_message:convert(TABM1, <<"structured@1.0">>, tabm, #{}),
    ?event({tabm0, {explicit, TABM0}}),
    ?event({dataitem, {explicit, Dataitem}}),
    % ?event({tabm1, {explicit, TABM1}}),
    % ?event({structured, {explicit, Structured}}),
    ?event({id, {explicit, hb_util:encode(Dataitem#tx.unsigned_id)}}),
    ExpectedTX = #tx{
        unsigned_id = hb_util:decode(<<"z6NIKgLIZUd9gShmcIzc70kRmL655kUsA0js4TW1nW4">>),
        tags = [
            {<<"ao-types">>,
                <<
                    "data_size=\"integer\", ",
                    "denomination=\"integer\", ",
                    "format=\"atom\", ",
                    "manifest=\"atom\", ",
                    "owner_address=\"atom\", ",
                    "quantity=\"integer\", ",
                    "reward=\"integer\""
                >>},
            {<<"data_root">>, <<>>},
            {<<"data_size">>,<<"0">>},
            {<<"data_tree+link">>, <<"24Xx7HqIQkRzm3CtNQxfht5RbHTz12N1Ihwm1B0IIFE">>},
            {<<"denomination">>,<<"0">>},
            {<<"format">>,<<"ans104">>},
            {<<"manifest">>,<<"undefined">>},
            {<<"owner_address">>,<<"not_set">>},
            {<<"quantity">>,<<"0">>},
            {<<"reward">>,<<"0">>},
            {<<"signature_type">>, ?RSA_KEY_TYPE},
            {<<"tags+link">>, <<"24Xx7HqIQkRzm3CtNQxfht5RbHTz12N1Ihwm1B0IIFE">>}
        ]
    },
    ?assertEqual(ExpectedTX, Dataitem),
    % ?assert(hb_message:match(Msg, Structured)),
    % ?assert(hb_message:match(TABM0, TABM1)),
    ok.