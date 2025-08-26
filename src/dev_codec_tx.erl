%%% @doc Codec for managing transformations from `ar_tx'-style Arweave TX
%%% records to and from TABMs.
-module(dev_codec_tx).
-export([from/3, to/3, commit/3, verify/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(BASE_FIELDS, [
    <<"anchor">>, <<"data_root">>, <<"format">>,
    <<"quantity">>, <<"reward">>, <<"target">> ]).

%% @doc Sign a message using the `priv_wallet' key in the options. Supports both
%% the `hmac-sha256' and `rsa-pss-sha256' algorithms, offering unsigned and
%% signed commitments.
commit(Msg, Req = #{ <<"type">> := <<"unsigned">> }, Opts) ->
    commit(Msg, Req#{ <<"type">> => <<"unsigned-sha256">> }, Opts);
commit(Msg, Req = #{ <<"type">> := <<"signed">> }, Opts) ->
    commit(Msg, Req#{ <<"type">> => <<"rsa-pss-sha256">> }, Opts);
commit(Msg, Req = #{ <<"type">> := <<"rsa-pss-sha256">> }, Opts) ->
    % Convert the given message to an L1 TX record, sign it, and convert
    % it back to a structured message.
    {ok, TX} = to(hb_private:reset(Msg), Req, Opts),
    Wallet = hb_opts:get(priv_wallet, no_viable_wallet, Opts),
    Signed = ar_tx:sign(TX, Wallet),
    SignedStructured =
        hb_message:convert(
            Signed,
            <<"structured@1.0">>,
            <<"tx@1.0">>,
            Opts
        ),
    {ok, SignedStructured};
commit(Msg, #{ <<"type">> := <<"unsigned-sha256">> }, Opts) ->
    % Remove the commitments from the message, convert it to an L1 TX, 
    % then back. This forces the message to be normalized and the unsigned ID
    % to be recalculated.
    {
        ok,
        hb_message:convert(
            hb_maps:without([<<"commitments">>], Msg, Opts),
            <<"tx@1.0">>,
            <<"structured@1.0">>,
            Opts
        )
    }.

%% @doc Verify an L1 TX commitment.
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
    ?event(debug_test, {verify, {only_with_commitment, {explicit, OnlyWithCommitment}}}),
    {ok, TX} = to(OnlyWithCommitment, Req, Opts),
    ?event(debug_test, {verify, {encoded, {explicit, TX}}}),
    Res = ar_tx:verify(TX),
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
    TX = normalize(RawTX),
    ?event({parsed_tx, TX}),
    % Get the fields, tags, and data from the TX.
    Fields = dev_codec_tx_from:fields(TX, <<>>, Opts),
    Tags = dev_codec_ans104_from:tags(TX, Opts),
    Data = dev_codec_ans104_from:data(TX, Req, Tags, Opts),
    ?event({parsed_components, {fields, Fields}, {tags, Tags}, {data, Data}}),
    % Calculate the committed keys on from the TX.
    Keys = dev_codec_ans104_from:committed(?BASE_FIELDS, TX, Fields, Tags, Data, Opts),
    ?event({determined_committed_keys, Keys}),
    % Create the base message from the fields, tags, and data, filtering to
    % include only the keys that are committed. Will throw if a key is missing.
    Base = dev_codec_ans104_from:base(Keys, Fields, Tags, Data, Opts),
    ?event({calculated_base_message, Base}),
    % Add the commitments to the message if the TX has a signature.
    CommittedFields = dev_codec_tx_from:fields(TX, ?FIELD_PREFIX, Opts),
    WithCommitments = dev_codec_ans104_from:with_commitments(
        TX, <<"tx@1.0">>, CommittedFields, Tags, Base, Keys, Opts),
    ?event({parsed_message, WithCommitments}),
    {ok, WithCommitments}.

%% @doc Internal helper to translate a message to its #tx record representation,
%% which can then be used by ar_tx to serialize the message. We call the 
%% message's device in order to get the keys that we will be checkpointing. We 
%% do this recursively to handle nested messages. The base case is that we hit
%% a binary, which we return as is.
to(Binary, _Req, _Opts) when is_binary(Binary) ->
    % ar_tx cannot serialize just a simple binary or get an ID for it, so
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
        MaybeBundle, <<"tx@1.0">>, fun dev_codec_tx_to:fields_to_tx/4, Opts),
    ?event({found_siginfo, TX0}),
    % Calculate and normalize the `data', if applicable.
    Data = dev_codec_ans104_to:data(MaybeBundle, Req, Opts),
    ?event({calculated_data, Data}),
    TX1 = TX0#tx { data = Data },
    % Calculate the tags for the TX.
    Tags = dev_codec_ans104_to:tags(
        TX1, <<"tx@1.0">>, MaybeBundle, Data,
        fun dev_codec_tx_to:excluded_tags/3, Opts),
    ?event({calculated_tags, Tags}),
    TX2 = TX1#tx { tags = Tags },
    ?event({tx_before_id_gen, TX2}),
    Res = normalize(TX2),
    ?event({to_result, Res}),
    {ok, Res};
to(Other, _Req, _Opts) ->
    throw({invalid_tx, Other}).

normalize(TX) ->
    reset_ids(
        normalize_data_root(
            ar_bundles:normalize_data(
                TX#tx{ owner_address = ar_tx:get_owner_address(TX) })
        )
    ).
    
normalize_data_root(Item = #tx{data = Bin, format = 2})
        when is_binary(Bin) andalso Bin =/= ?DEFAULT_DATA ->
    Item#tx{data_root = ar_tx:data_root(Bin)};
normalize_data_root(Item) -> Item.

reset_ids(TX) ->
    update_ids(TX#tx{unsigned_id = ?DEFAULT_ID, id = ?DEFAULT_ID}).

update_ids(TX = #tx{ unsigned_id = ?DEFAULT_ID }) ->
    update_ids(TX#tx{unsigned_id = ar_tx:generate_id(TX, unsigned)});
update_ids(TX = #tx{ id = ?DEFAULT_ID, signature = ?DEFAULT_SIG }) ->
    TX;
update_ids(TX = #tx{ signature = ?DEFAULT_SIG }) ->
    TX#tx{ id = ?DEFAULT_ID };
update_ids(TX = #tx{ signature = Sig }) when Sig =/= ?DEFAULT_SIG ->
    TX#tx{ id = ar_tx:generate_id(TX, signed) };
update_ids(TX) -> TX.

%%%===================================================================
%%% Tests.
%%%===================================================================

happy_tx_test() ->
    Anchor = crypto:strong_rand_bytes(32),
    Target = crypto:strong_rand_bytes(32),
    Data = <<"data">>,
    TX = #tx{
        format = 2,
        anchor = Anchor,
        tags = [
            {<<"tag1">>, <<"value1">>},
            {<<"tag2">>, <<"value2">>}
        ],
        target = Target,
        quantity = 1000,
        data = Data,
        data_size = byte_size(Data),
        data_root = ar_tx:data_root(Data),
        reward = 2000
    },
    UnsignedTABM = #{
        <<"anchor">> => hb_util:encode(Anchor),
        <<"target">> => hb_util:encode(Target),
        <<"quantity">> => <<"1000">>,
        <<"reward">> => <<"2000">>,
        <<"data_root">> => hb_util:encode(ar_tx:data_root(Data)),
        <<"data">> => Data,
        <<"tag1">> => <<"value1">>,
        <<"tag2">> => <<"value2">>
    },
    SignedCommitment = #{
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"committed">> => [
            <<"anchor">>, <<"data_root">>,
            <<"quantity">>, <<"reward">>, <<"target">>,
            <<"data">>, <<"tag1">>, <<"tag2">>],
        <<"type">> => <<"rsa-pss-sha256">>,
        <<"bundle">> => <<"false">>,
        <<"field-target">> => hb_util:encode(Target),
        <<"field-anchor">> => hb_util:encode(Anchor),
        <<"field-data_root">> => hb_util:encode(ar_tx:data_root(Data)),
        <<"field-quantity">> => <<"1000">>,
        <<"field-reward">> => <<"2000">>
    },
    do_tx_roundtrips(TX, UnsignedTABM, SignedCommitment).

tag_name_case_test() ->
    TX = #tx{
        format = 2,
        tags = [
            {<<"Test-Tag">>, <<"test-value">>}
        ]
    },
    UnsignedID = ar_tx:generate_id(TX, unsigned),
    UnsignedTABM = #{
        <<"test-tag">> => <<"test-value">>,
        <<"commitments">> => #{
            hb_util:encode(UnsignedID) => #{
                <<"commitment-device">> => <<"tx@1.0">>,
                <<"committed">> => [<<"test-tag">>],
                <<"original-tags">> =>#{
                    <<"1">> => #{
                        <<"name">> => <<"Test-Tag">>,
                        <<"value">> => <<"test-value">>
                    }
                },
                <<"type">> => <<"unsigned-sha256">>,
                <<"bundle">> => <<"false">>
            }
        }
    },
    SignedCommitment = #{
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"committed">> => [<<"test-tag">>],
        <<"original-tags">> =>#{
            <<"1">> => #{
                <<"name">> => <<"Test-Tag">>,
                <<"value">> => <<"test-value">>
            }
        },
        <<"type">> => <<"rsa-pss-sha256">>,
        <<"bundle">> => <<"false">>
    },
    do_tx_roundtrips(TX, UnsignedTABM, SignedCommitment).

duplicated_tag_name_test() ->
    TX = #tx{
        format = 2,
        tags = [
            {<<"Test-Tag">>, <<"test-value">>},
            {<<"test-tag">>, <<"test-value-2">>}
        ]
    },
    UnsignedID = ar_tx:generate_id(TX, unsigned),
    UnsignedTABM = #{
        <<"test-tag">> => <<"\"test-value\", \"test-value-2\"">>,
        <<"commitments">> => #{
            hb_util:encode(UnsignedID) => #{
                <<"commitment-device">> => <<"tx@1.0">>,
                <<"committed">> => [<<"test-tag">>],
                <<"original-tags">> =>#{
                    <<"1">> => #{
                        <<"name">> => <<"Test-Tag">>,
                        <<"value">> => <<"test-value">>
                    },
                    <<"2">> => #{
                        <<"name">> => <<"test-tag">>,
                        <<"value">> => <<"test-value-2">>
                    }
                },
                <<"type">> => <<"unsigned-sha256">>,
                <<"bundle">> => <<"false">>
            }
        }
    },
    SignedCommitment = #{
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"committed">> => [<<"test-tag">>],
        <<"original-tags">> =>#{
            <<"1">> => #{
                <<"name">> => <<"Test-Tag">>,
                <<"value">> => <<"test-value">>
            },
            <<"2">> => #{
                <<"name">> => <<"test-tag">>,
                <<"value">> => <<"test-value-2">>
            }
        },
        <<"type">> => <<"rsa-pss-sha256">>,
        <<"bundle">> => <<"false">>
    },
    do_tx_roundtrips(TX, UnsignedTABM, SignedCommitment).

%% @doc Test that when a TABM has base field keys set to values that are not
%% valid on a #tx record, they are preserved as tags instead.
non_conforming_fields_test() ->
    UnsignedTABM = #{
        <<"anchor">> => Anchor = <<"NON-ID-ANCHOR">>,
        <<"target">> => Target = <<"NON-ID-TARGET">>,
        <<"quantity">> => Quantity = <<"NON-INT-QUANTITY">>,
        <<"reward">> => Reward = <<"NON-INT-REWARD">>,
        <<"data_root">> => DataRoot = <<"NON-ID-DATA-ROOT">>,
        <<"tag1">> => <<"value1">>,
        <<"tag2">> => <<"value2">>
    },
    UnsignedTX = #tx{
        format = 2,
        tags = [
            {<<"anchor">>, Anchor},
            {<<"data_root">>, DataRoot},
            {<<"quantity">>, Quantity},
            {<<"reward">>, Reward},
            {<<"tag1">>, <<"value1">>},
            {<<"tag2">>, <<"value2">>},
            {<<"target">>, Target}
        ]
    },
    SignedCommitment = #{
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"committed">> => [<<"anchor">>, <<"data_root">>, <<"quantity">>,
            <<"reward">>, <<"target">>, <<"tag1">>, <<"tag2">>],
        <<"type">> => <<"rsa-pss-sha256">>,
        <<"bundle">> => <<"false">>
    },
    do_tabm_roundtrips(UnsignedTX, UnsignedTABM, SignedCommitment).

%% @doc Run a series of roundtrip tests that start and end with a #tx record
do_tx_roundtrips(UnsignedTX, UnsignedTABM, Commitment) ->
   do_unsigned_tx_roundtrip(UnsignedTX, UnsignedTABM),
   do_signed_tx_roundtrip(UnsignedTX, UnsignedTABM, Commitment).

do_unsigned_tx_roundtrip(UnsignedTX, UnsignedTABM) ->
    JSON = ar_tx:tx_to_json_struct(UnsignedTX),
    DeserializedTX = ar_tx:json_struct_to_tx(JSON),

    TABM = hb_util:ok(from(DeserializedTX, #{}, #{})),
    ?event(debug_test, {unsigned_tx_roundtrip,{expected_tabm, UnsignedTABM}, {actual_tabm, TABM}}),
    ?assertEqual(UnsignedTABM, TABM, unsigned_tx_roundtrip),

    TX = hb_util:ok(to(TABM, #{}, #{})),
    ExpectedTX = UnsignedTX#tx{ unsigned_id = ar_tx:id(UnsignedTX, unsigned) },
    ?event(debug_test, {unsigned_tx_roundtrip, {expected_tx, ExpectedTX}, {actual_tx, TX}}),
    ?assertEqual(ExpectedTX, TX, unsigned_tx_roundtrip).

do_signed_tx_roundtrip(UnsignedTX, UnsignedTABM, Commitment) ->
    SignedTX = ar_tx:sign(UnsignedTX, hb:wallet()),
    ?assert(ar_tx:verify(SignedTX), signed_tx_roundtrip),

    JSON = ar_tx:tx_to_json_struct(SignedTX),
    DeserializedTX = ar_tx:json_struct_to_tx(JSON),

    TABM = hb_util:ok(from(DeserializedTX, #{}, #{})),

    SignedCommitment = Commitment#{
        <<"committer">> => hb_util:human_id(SignedTX#tx.owner_address),
        <<"signature">> => hb_util:encode(SignedTX#tx.signature),
        <<"keyid">> =>
            <<"publickey:", (hb_util:encode(SignedTX#tx.owner))/binary>>
    },
    SignedTABM = UnsignedTABM#{
        <<"commitments">> => #{ hb_util:human_id(SignedTX#tx.id) => SignedCommitment }},
    ?event(debug_test, {signed_tx_roundtrip, {expected_tabm, SignedTABM}, {actual_tabm, TABM}}),
    ?assertEqual(SignedTABM, TABM, signed_tx_roundtrip),

    TX = hb_util:ok(to(TABM, #{}, #{})),
    ExpectedTX = SignedTX,
    ?event(debug_test, {signed_tx_roundtrip, {expected_tx, ExpectedTX}, {actual_tx, TX}}),
    ?assertEqual(ExpectedTX, TX, signed_tx_roundtrip).

%% @doc Run a series of roundtrip tests that start and end with a TABM.
do_tabm_roundtrips(UnsignedTX, UnsignedTABM, Commitment) ->
    % do_unsigned_tabm_roundtrip(UnsignedTX, UnsignedTABM),
    do_signed_tabm_roundtrip(UnsignedTX, UnsignedTABM, Commitment).
    
do_unsigned_tabm_roundtrip(UnsignedTX, UnsignedTABM) ->
    TX = hb_util:ok(to(UnsignedTABM, #{}, #{})),
    JSON = ar_tx:tx_to_json_struct(TX),
    DeserializedTX = ar_tx:json_struct_to_tx(JSON),
    ExpectedTX = UnsignedTX#tx{ unsigned_id = ar_tx:id(UnsignedTX, unsigned) },
    ?event(debug_test, {unsigned_tabm_roundtrip, 
        {expected_tx, {explicit, UnsignedTX}}, {actual_tx, {explicit, DeserializedTX}}}),
    ?assertEqual(UnsignedTX, DeserializedTX, unsigned_tabm_roundtrip),

    TABM = hb_util:ok(from(DeserializedTX, #{}, #{})),
    ?event(debug_test, {unsigned_tabm_roundtrip,
        {expected_tabm, UnsignedTABM}, {actual_tabm, TABM}}),
    ?assertEqual(UnsignedTABM, TABM, unsigned_tabm_roundtrip).

do_signed_tabm_roundtrip(UnsignedTX, UnsignedTABM, Commitment) ->
    Wallet = hb:wallet(),
    TX = hb_util:ok(to(UnsignedTABM, #{}, #{})),
    ?event(debug_test, {unsigned_tabm_roundtrip, {unsigned_tx, {explicit, UnsignedTX}}, {tx, {explicit, TX}}}),
    ?assertEqual(UnsignedTX#tx{ unsigned_id = ar_tx:id(UnsignedTX, unsigned) }, TX, unsigned_tabm_roundtrip),
    SignedTX = ar_tx:sign(UnsignedTX, Wallet),
    ?event(debug_test, {signed_tabm_roundtrip, {signed_tx, {explicit, SignedTX}}}),
    ?assert(ar_tx:verify(SignedTX), signed_tabm_roundtrip),
    
    SignedTABM = hb_message:commit(
        UnsignedTABM, #{priv_wallet => Wallet}, <<"tx@1.0">>),
    ?event(debug_test, {signed_tabm_roundtrip, {signed_tabm, SignedTABM}}),
    ?assert(hb_message:verify(SignedTABM), signed_tabm_roundtrip),

    {ok, _, SignedCommitment} = hb_message:commitment(
        #{ <<"commitment-device">> => <<"tx@1.0">> },
        SignedTABM,
        #{}
    ),
    ExpectedCommitment = Commitment#{
        <<"committer">> => hb_util:human_id(ar_wallet:to_address(Wallet)),
        <<"signature">> => maps:get(<<"signature">>, SignedCommitment),
        <<"keyid">> =>
            <<"publickey:", (hb_util:encode(ar_wallet:to_pubkey(Wallet)))/binary>>
    },
    ?assertEqual(ExpectedCommitment, SignedCommitment, signed_tabm_roundtrip),
    
    TX = hb_util:ok(to(SignedTABM, #{}, #{})),
    ExpectedTX = ar_tx:sign(UnsignedTX, Wallet),
    ?event(debug_test, {signed_tabm_roundtrip, 
        {expected_tx, {explicit, ExpectedTX}}, {actual_tx, {explicit, TX}}}),
    ?assertEqual(ExpectedTX, TX, signed_tabm_roundtrip).

    % SignedTX = ar_tx:sign(UnsignedTX, hb:wallet()),
    % ?assert(ar_tx:verify(SignedTX), signed_tx_roundtrip),

    % JSON = ar_tx:tx_to_json_struct(SignedTX),
    % DeserializedTX = ar_tx:json_struct_to_tx(JSON),

    % TABM = hb_util:ok(from(DeserializedTX, #{}, #{})),

    % SignedCommitment = Commitment#{
    %     <<"committer">> => hb_util:human_id(SignedTX#tx.owner_address),
    %     <<"signature">> => hb_util:encode(SignedTX#tx.signature),
    %     <<"keyid">> =>
    %         <<"publickey:", (hb_util:encode(SignedTX#tx.owner))/binary>>
    % },
    % SignedTABM = UnsignedTABM#{
    %     <<"commitments">> => #{ hb_util:human_id(SignedTX#tx.id) => SignedCommitment }},
    % ?event(debug_test, {signed_tx_roundtrip, {expected_tabm, SignedTABM}, {actual_tabm, TABM}}),
    % ?assertEqual(SignedTABM, TABM, signed_tx_roundtrip),

    % TX = hb_util:ok(to(TABM, #{}, #{})),
    % ExpectedTX = SignedTX#tx{ unsigned_id = ar_tx:id(SignedTX, unsigned) },
    % ?event(debug_test, {signed_tx_roundtrip, {expected_tx, ExpectedTX}, {actual_tx, TX}}),
    % ?assertEqual(ExpectedTX, TX, signed_tx_roundtrip).