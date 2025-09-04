%%% @doc Codec for managing transformations from `ar_tx'-style Arweave TX
%%% records to and from TABMs.
-module(dev_codec_tx).
-export([from/3, to/3, commit/3, verify/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(BASE_FIELDS, [
    <<"anchor">>, <<"format">>, <<"quantity">>, <<"reward">>, <<"target">> ]).

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
    ?event({verify, {only_with_commitment, {explicit, OnlyWithCommitment}}}),
    {ok, TX} = to(OnlyWithCommitment, Req, Opts),
    ?event({verify, {encoded, {explicit, TX}}}),
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
    % Assert a minimally valid TX record so we can avoid a lot of edge case
    % handling in the rest of the code.
    enforce_valid_tx(RawTX),
    TX = ar_bundles:deserialize(normalize(RawTX)),
    ?event({parsed_tx, TX}),
    % Get the fields, tags, and data from the TX.
    Fields = dev_codec_tx_from:fields(TX, <<>>, Opts),
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
        normalize(#tx{
            format = 2,
            tags = [{<<"ao-type">>, <<"binary">>}],
            data = Binary
        })
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
    FinalTX = normalize(TX2),
    enforce_valid_tx(FinalTX),
    ?event({to_result, FinalTX}),
    {ok, FinalTX};
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


%% @doc Verifies that the given transaction is a minimally valid signed or
%% unsigned transaction.
%% 
%% In particular:
%% 1. Values are of the correct type and size.
%% 2. In some cases where a limited number of values are allowed for a field, 
%%    those are checked as well (e.g. format is 1 or 2).
%% 3. Unsupported fields are set to their default values.
%% 
%% Of note: for now we require that the `data` field be set on an L1 TX if 
%% there is data. In other words we do not allow `data_root` and `data_size` to
%% be set if `data` is *not* set. This differs from the Arweave protocol which
%% explicitly allows TX headers to be validated in the absence of data.
%% 
%% When support is added for new fields (e.g. when we add support for ECDSA signatures),
%% this function will have to be updated.
enforce_valid_tx(TX) ->
    hb_util:ok_or_throw(TX,
        hb_util:check_type(TX, tx),
        {invalid_tx, TX}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_value(TX#tx.format, [1, 2]),
        {invalid_field, format, TX#tx.format}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.id, [32]),
        {invalid_field, id, TX#tx.id}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.unsigned_id, [32]),
        {invalid_field, unsigned_id, TX#tx.unsigned_id}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.anchor, [0, 32, 48]),
        {invalid_field, anchor, TX#tx.anchor}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.owner, [byte_size(?DEFAULT_OWNER)]),
        {invalid_field, owner, TX#tx.owner}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.target, [0, 32]),
        {invalid_field, target, TX#tx.target}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_type(TX#tx.quantity, integer),
        {invalid_field, quantity, TX#tx.quantity}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_type(TX#tx.data_size, integer),
        {invalid_field, data_size, TX#tx.data_size}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.data_root, [0, 32]),
        {invalid_field, data_root, TX#tx.data_root}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.signature, [65, byte_size(?DEFAULT_SIG)]),
        {invalid_field, signature, TX#tx.signature}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_type(TX#tx.reward, integer),
        {invalid_field, reward, TX#tx.reward}
    ),
    % Arweave L1 #tx doesn't support denomination changes yet.
    % Refresh from arweave source to add support.
    hb_util:ok_or_throw(TX,
        hb_util:check_value(TX#tx.denomination, [0]),
        {invalid_field, denomination, TX#tx.denomination}
    ),
    % Arweave L1 #tx only supports RSA signatures for now
    hb_util:ok_or_throw(TX,
        hb_util:check_value(TX#tx.signature_type, [?RSA_KEY_TYPE]),
        {invalid_field, signature_type, TX#tx.signature_type}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_type(TX#tx.tags, list),
        {invalid_field, tags, TX#tx.tags}
    ),
    lists:foreach(
        fun({Name, Value}) ->
            hb_util:ok_or_throw(TX,
                hb_util:check_type(Name, binary),
                {invalid_field, tag_name, Name}
            ),
            hb_util:ok_or_throw(TX,
                hb_util:check_size(Name, {range, 0, ?MAX_TAG_NAME_SIZE}),
                {invalid_field, tag_name, Name}
            ),
            hb_util:ok_or_throw(TX,
                hb_util:check_type(Value, binary),
                {invalid_field, tag_value, Value}
            ),
            hb_util:ok_or_throw(TX,
                hb_util:check_size(Value, {range, 0, ?MAX_TAG_VALUE_SIZE}),
                {invalid_field, tag_value, Value}
            );
            (InvalidTagForm) ->
                throw({invalid_field, tag, InvalidTagForm})
        end,
        TX#tx.tags
    ),
    enforce_valid_tx_data(TX).

%% @doc For now we require that the `data` field be set on an L1 TX if 
%% there is data. In other words we do not allow `data_root` and `data_size` to
%% be set if `data` is *not* set. This differs from the Arweave protocol which
%% explicitly allows TX headers to be validated in the absence of data.
enforce_valid_tx_data(TX) when TX#tx.data == ?DEFAULT_DATA ->
    case TX#tx.data_root =/= ?DEFAULT_DATA_ROOT of
        true ->
            throw({invalid_field, data_root, TX#tx.data_root});
        false ->
            ok
    end,
    case TX#tx.data_size > 0 of
        true ->
            throw({invalid_field, data_size, TX#tx.data_size});
        false ->
            ok
    end;
enforce_valid_tx_data(TX) ->
    ok.

%%%===================================================================
%%% Tests.
%%%===================================================================

enforce_valid_tx_test() ->
    BaseTX = #tx{ format = 2 },

    InvalidUnsignedID = crypto:strong_rand_bytes(1),
    GoodID = crypto:strong_rand_bytes(32),
    BadID31 = crypto:strong_rand_bytes(31),
    BadID33 = crypto:strong_rand_bytes(33),
    BadOwnerSize = crypto:strong_rand_bytes(byte_size(?DEFAULT_OWNER) - 1),
    TooLongTagName = crypto:strong_rand_bytes(?MAX_TAG_NAME_SIZE + 1),
    TooLongTagValue = crypto:strong_rand_bytes(?MAX_TAG_VALUE_SIZE + 1),

    SigInvalidSize1 = crypto:strong_rand_bytes(1),
    SigInvalidSize64 = crypto:strong_rand_bytes(64),
    SigInvalidSize66 = crypto:strong_rand_bytes(66),
    SigInvalidSize511 = crypto:strong_rand_bytes(511),
    SigTooLong513 = crypto:strong_rand_bytes(byte_size(?DEFAULT_SIG)+1),
    

    FailureCases = [
        {not_a_tx_record, not_a_tx_record_atom, {invalid_tx, not_a_tx_record_atom}},
        {invalid_format_0, BaseTX#tx{format = 0}, {invalid_field, format, 0}},
        {invalid_format_3, BaseTX#tx{format = 3}, {invalid_field, format, 3}},
        {invalid_format_atom, BaseTX#tx{format = an_atom}, {invalid_field, format, an_atom}},
        {id_too_short_31, BaseTX#tx{id = BadID31}, {invalid_field, id, BadID31}},
        {id_too_long_33, BaseTX#tx{id = BadID33}, {invalid_field, id, BadID33}},
        {unsigned_id_invalid_val, BaseTX#tx{unsigned_id = InvalidUnsignedID}, {invalid_field, unsigned_id, InvalidUnsignedID}},
        {anchor_too_short_31, BaseTX#tx{anchor = BadID31}, {invalid_field, anchor, BadID31}},
        {anchor_too_long_33, BaseTX#tx{anchor = BadID33}, {invalid_field, anchor, BadID33}},
        {owner_wrong_size, BaseTX#tx{owner = BadOwnerSize}, {invalid_field, owner, BadOwnerSize}},
        {owner_empty, BaseTX#tx{owner = <<>>}, {invalid_field, owner, <<>>}},
        {target_too_short_31, BaseTX#tx{target = BadID31}, {invalid_field, target, BadID31}},
        {target_too_long_33, BaseTX#tx{target = BadID33}, {invalid_field, target, BadID33}},
        {quantity_not_integer, BaseTX#tx{quantity = <<"100">>}, {invalid_field, quantity, <<"100">>}},
        {data_size_not_integer, BaseTX#tx{data_size = an_atom}, {invalid_field, data_size, an_atom}},
        {data_root_too_short_31, BaseTX#tx{data_root = BadID31}, {invalid_field, data_root, BadID31}},
        {data_root_too_long_33, BaseTX#tx{data_root = BadID33}, {invalid_field, data_root, BadID33}},
        {signature_invalid_size_1, BaseTX#tx{signature = SigInvalidSize1}, {invalid_field, signature, SigInvalidSize1}},
        {signature_invalid_size_64, BaseTX#tx{signature = SigInvalidSize64}, {invalid_field, signature, SigInvalidSize64}},
        {signature_invalid_size_66, BaseTX#tx{signature = SigInvalidSize66}, {invalid_field, signature, SigInvalidSize66}},
        {signature_invalid_size_511, BaseTX#tx{signature = SigInvalidSize511}, {invalid_field, signature, SigInvalidSize511}},
        {signature_too_long_513, BaseTX#tx{signature = SigTooLong513}, {invalid_field, signature, SigTooLong513}},
        {signature_empty, BaseTX#tx{signature = <<>>}, {invalid_field, signature, <<>>}},
        {reward_not_integer, BaseTX#tx{reward = 1.0}, {invalid_field, reward, 1.0}},
        {denomination_not_zero, BaseTX#tx{denomination = 1}, {invalid_field, denomination, 1}},
        {signature_type_not_rsa, BaseTX#tx{signature_type = ?ECDSA_KEY_TYPE}, {invalid_field, signature_type, ?ECDSA_KEY_TYPE}},
        {tags_not_list, BaseTX#tx{tags = #{}}, {invalid_field, tags, #{}}},
        {tag_name_not_binary, BaseTX#tx{tags = [{not_binary, <<"val">>}]}, {invalid_field, tag_name, not_binary}},
        {tag_name_too_long, BaseTX#tx{tags = [{TooLongTagName, <<"val">>}]}, {invalid_field, tag_name, TooLongTagName}},
        {tag_value_not_binary, BaseTX#tx{tags = [{<<"key">>, not_binary}]}, {invalid_field, tag_value, not_binary}},
        {tag_value_too_long, BaseTX#tx{tags = [{<<"key">>, TooLongTagValue}]}, {invalid_field, tag_value, TooLongTagValue}},
        {invalid_tag_form_atom, BaseTX#tx{tags = [not_a_tuple]}, {invalid_field, tag, not_a_tuple}},
        {invalid_tag_form_list, BaseTX#tx{tags = [[<<"name">>, <<"value">>]]}, {invalid_field, tag, [<<"name">>, <<"value">>]} },
        {data_root_without_data, BaseTX#tx{data_root = GoodID}, {invalid_field, data_root, GoodID}},
        {data_size_without_data, BaseTX#tx{data_size = 1}, {invalid_field, data_size, 1}}
    ],

    lists:foreach(
        fun({Label, BadTX, ExpectedThrow}) ->
            ?assertThrow(ExpectedThrow, enforce_valid_tx(BadTX), Label)
        end,
        FailureCases
    ).

happy_tx_test() ->
    Anchor = crypto:strong_rand_bytes(32),
    Target = crypto:strong_rand_bytes(32),
    Data = <<"data">>,
    TX = #tx{
        format = 2,
        anchor = Anchor,
        tags = [
            {<<"tag1">>, <<"value1">>},
            {<<"tag2">>, <<"value2">>},
            {<<"type">>, <<"test-type">>}
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
        <<"data">> => Data,
        <<"tag1">> => <<"value1">>,
        <<"tag2">> => <<"value2">>,
        <<"type">> => <<"test-type">>
    },
    SignedCommitment = #{
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"committed">> => [
            <<"data">>, <<"tag1">>, <<"tag2">>, <<"type">>,
            <<"anchor">>,
            <<"quantity">>, <<"reward">>, <<"target">>],
        <<"type">> => <<"rsa-pss-sha256">>,
        <<"bundle">> => <<"false">>,
        <<"field-target">> => hb_util:encode(Target),
        <<"field-anchor">> => hb_util:encode(Anchor),
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
            <<"reward">>, <<"tag1">>, <<"tag2">>, <<"target">>],
        <<"type">> => <<"rsa-pss-sha256">>,
        <<"bundle">> => <<"false">>
    },
    do_tabm_roundtrips(UnsignedTX, UnsignedTABM, SignedCommitment).

ao_data_key_test() ->
    Data = <<"Body value">>,
    UnsignedTABM = #{
        <<"body">> => Data,
        <<"tag1">> => <<"value1">>
    },
    UnsignedTX = #tx{
        format = 2,
        tags = [
            {<<"ao-data-key">>, <<"body">>},
            {<<"tag1">>, <<"value1">>}
        ],
        data = Data,
        data_size = byte_size(Data),
        data_root = ar_tx:data_root(Data)
    },
    SignedCommitment = #{
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"committed">> => [<<"body">>, <<"tag1">>],
        <<"type">> => <<"rsa-pss-sha256">>,
        <<"bundle">> => <<"false">>
    },
    do_tabm_roundtrips(UnsignedTX, UnsignedTABM, SignedCommitment).

unsorted_tags_test() ->
    TX = #tx{
        format = 2,
        tags = [
            {<<"z">>, <<"position-1">>},
            {<<"a">>, <<"position-2">>}
        ]
    },
    UnsignedTABM = #{
        <<"z">> => <<"position-1">>,
        <<"a">> => <<"position-2">>
    },
    SignedCommitment = #{
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"committed">> => [<<"z">>, <<"a">>],
        <<"type">> => <<"rsa-pss-sha256">>,
        <<"bundle">> => <<"false">>
    },
    % Only do a signed test since we don't need the tag order to be preserved
    % for messages without a commitment. And since this test case doesn't
    % require an original-tags commitment, no unsigned commitment will be
    % generated.
    do_signed_tx_roundtrip(TX, UnsignedTABM, SignedCommitment, false).

nested_data_tabm_test() ->
    UnsignedTABM = #{
        <<"data">> => #{
            <<"data">> => #{
                <<"data">> => <<"nested-data">>,
                <<"tag">> => <<"level-3">>
            },
            <<"tag">> => <<"level-2">>
        },
        <<"tag">> => <<"level-1">>
    },

    TX = #tx{
        format = 2,
        tags = [
            {<<"tag">>, <<"level-1">>}
        ],
        data = #{ 
            <<"data">> => #tx{
                format = ans104,
                tags = [
                    {<<"tag">>, <<"level-2">>}
                ],
                data = #{
                    <<"data">> => #tx{
                        format = ans104,
                        tags = [
                            {<<"tag">>, <<"level-3">>}
                        ],
                        data = <<"nested-data">>
                    }
                }
            }
        }
    },
    UnsignedTX = normalize(TX),
    NoLinksCommitment = #{
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"committed">> => [<<"data">>, <<"tag">>],
        <<"type">> => <<"rsa-pss-sha256">>,
        <<"bundle">> => <<"true">>
    },
    % only bundle true is supported
    do_tabm_roundtrips(UnsignedTX, UnsignedTABM, NoLinksCommitment, true).

nested_non_data_key_tabm_test() ->
    UnsignedTABM = #{
        <<"a1">> => #{
            <<"a2">> => #{
                <<"a3">> => <<"nested-data">>,
                <<"tag3">> => <<"level-3">>
            },
            <<"tag2">> => <<"level-2">>
        },
        <<"tag1">> => <<"level-1">>
    },

    TX = #tx{
        format = 2,
        tags = [
            {<<"tag1">>, <<"level-1">>}
        ],
        data = #{ 
            <<"a1">> => #tx{
                format = ans104,
                tags = [
                    {<<"tag2">>, <<"level-2">>}
                ],
                data = #{
                    <<"a2">> => #tx{
                        format = ans104,
                        tags = [
                            {<<"a3">>, <<"nested-data">>},
                            {<<"tag3">>, <<"level-3">>}
                        ]
                    }
                }
            }
        }
    },
    UnsignedTX = normalize(TX),
    NoLinksCommitment = #{
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"committed">> => [<<"a1">>, <<"tag1">>],
        <<"type">> => <<"rsa-pss-sha256">>,
        <<"bundle">> => <<"true">>
    },
    % only bundle true is supported
    do_tabm_roundtrips(UnsignedTX, UnsignedTABM, NoLinksCommitment, true).

nested_multiple_tabm_test() ->
    UnsignedTABM = #{
        <<"a1">> => #{
            <<"a2">> => #{
                <<"a3">> => <<"nested-data">>,
                <<"tag3">> => <<"level-3">>
            },
            <<"data">> => #{
                <<"other-tag3">> => <<"other-level-3">>
            },
            <<"tag2">> => <<"level-2">>
        },
        <<"data">> => #{
            <<"other-tag2">> => <<"other-level-2">>
        },
        <<"tag1">> => <<"level-1">>
    },

    TX = #tx{
        format = 2,
        tags = [
            {<<"tag1">>, <<"level-1">>}
        ],
        data = #{ 
            <<"a1">> => #tx{
                format = ans104,
                tags = [
                    {<<"tag2">>, <<"level-2">>}
                ],
                data = #{
                    <<"a2">> => #tx{
                        format = ans104,
                        tags = [
                            {<<"a3">>, <<"nested-data">>},
                            {<<"tag3">>, <<"level-3">>}
                        ]
                    },
                    <<"data">> => #tx{
                        format = ans104,
                        tags = [
                            {<<"other-tag3">>, <<"other-level-3">>}
                        ]
                    }
                }
            },
            <<"data">> => #tx{
                format = ans104,
                tags = [
                    {<<"other-tag2">>, <<"other-level-2">>}
                ]
            }
        }
    },
    UnsignedTX = normalize(TX),
    NoLinksCommitment = #{
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"committed">> => [<<"a1">>, <<"data">>, <<"tag1">>],
        <<"type">> => <<"rsa-pss-sha256">>,
        <<"bundle">> => <<"true">>
    },
    % only bundle true is supported
    do_tabm_roundtrips(UnsignedTX, UnsignedTABM, NoLinksCommitment, true).

%% @doc This test is disabled for now. Unclear whether we should deserialize
%% data by default. If we don't need to deserialize data, then this test
%% is no different from earlier tests which have `data` set to a binary.
serialized_data_item_tx_test_disabled() ->
    Anchor = crypto:strong_rand_bytes(32),
    Target = crypto:strong_rand_bytes(32),
    Data = <<"data">>,

    DataItem = #tx{
        format = ans104,
        tags = [
            {<<"ans104-tag1">>, <<"value1">>},
            {<<"ans104-tag2">>, <<"value2">>}
        ],
        anchor = Anchor,
        target = Target,
        data = Data
    },

    SerializedDataItem = ar_bundles:serialize(DataItem),
    TX = #tx{
        format = 2,
        tags = [
            {<<"tx-tag1">>, <<"value1">>},
            {<<"tx-tag2">>, <<"value2">>}
        ],
        data = SerializedDataItem,
        data_size = byte_size(SerializedDataItem),
        data_root = ar_tx:data_root(SerializedDataItem)
    },
    UnsignedTABM = #{ },
    SignedCommitment = #{ },
    do_tx_roundtrips(TX, UnsignedTABM, SignedCommitment, false).

%% @doc Run a series of roundtrip tests that start and end with a #tx record
do_tx_roundtrips(UnsignedTX, UnsignedTABM, Commitment) ->
    % For tests which don't care about bundling, just use false.
    do_tx_roundtrips(UnsignedTX, UnsignedTABM, Commitment, false).
do_tx_roundtrips(UnsignedTX, UnsignedTABM, Commitment, Bundle) ->
    Req = #{ <<"bundle">> => Bundle },
    do_unsigned_tx_roundtrip(UnsignedTX, UnsignedTABM, Req),
    do_signed_tx_roundtrip(UnsignedTX, UnsignedTABM, Commitment, Req).

do_unsigned_tx_roundtrip(UnsignedTX, UnsignedTABM, Req) ->
    % Serialize -> Deserialize
    JSON = ar_tx:tx_to_json_struct(UnsignedTX),
    DeserializedTX = ar_tx:json_struct_to_tx(JSON),
    ?event(debug_test, {unsigned_tx_roundtrip,
        {expected_tx, UnsignedTX}, {deserialized_tx, DeserializedTX}}),
    ?assertEqual(UnsignedTX, DeserializedTX, unsigned_tx_roundtrip),
    % TX -> TABM
    TABM = hb_util:ok(from(DeserializedTX, Req, #{})),
    ?event(debug_test, {unsigned_tx_roundtrip,
        {expected_tabm, UnsignedTABM}, {actual_tabm, TABM}}),
    ?assertEqual(UnsignedTABM, TABM, unsigned_tx_roundtrip),
    % TABM -> TX
    TX = hb_util:ok(to(TABM, Req, #{})),
    ExpectedTX = UnsignedTX#tx{ unsigned_id = ar_tx:id(UnsignedTX, unsigned) },
    ?event(debug_test, {unsigned_tx_roundtrip,
        {expected_tx, ExpectedTX}, {actual_tx, TX}}),
    ?assertEqual(ExpectedTX, TX, unsigned_tx_roundtrip).

do_signed_tx_roundtrip(UnsignedTX, UnsignedTABM, Commitment, Req) ->
    % Sign TX
    SignedTX = ar_tx:sign(UnsignedTX, hb:wallet()),
    ?assert(ar_tx:verify(SignedTX), signed_tx_roundtrip),
    ?event(debug_test, {signed_tx_roundtrip, {signed_tx, SignedTX}}),
    % Serialize -> Deserialize
    JSON = ar_tx:tx_to_json_struct(SignedTX),
    DeserializedTX = ar_tx:json_struct_to_tx(JSON),
    ?event(debug_test, {signed_tx_roundtrip, {deserialized_tx, DeserializedTX}}),
    % TX -> TABM
    TABM = hb_util:ok(from(DeserializedTX, Req, #{})),
    SignedCommitment = Commitment#{
        <<"committer">> => hb_util:human_id(SignedTX#tx.owner_address),
        <<"signature">> => hb_util:encode(SignedTX#tx.signature),
        <<"keyid">> =>
            <<"publickey:", (hb_util:encode(SignedTX#tx.owner))/binary>>
    },
    SignedTABM = UnsignedTABM#{
        <<"commitments">> => 
            #{ hb_util:human_id(SignedTX#tx.id) => SignedCommitment }},
    ?event(debug_test, {signed_tx_roundtrip,
        {expected_tabm, SignedTABM}, {actual_tabm, TABM}}),
    ?assertEqual(SignedTABM, TABM, signed_tx_roundtrip),
    % TABM -> TX
    TX = hb_util:ok(to(TABM, Req, #{})),
    ExpectedTX = SignedTX#tx{ 
        unsigned_id = ar_tx:generate_id(SignedTX, unsigned) },
    ?event(debug_test, {signed_tx_roundtrip,
        {expected_tx, ExpectedTX}, {actual_tx, TX}}),
    ?assertEqual(ExpectedTX, TX, signed_tx_roundtrip).

%% @doc Run a series of roundtrip tests that start and end with a TABM.
do_tabm_roundtrips(UnsignedTX, UnsignedTABM, Commitment) ->
    % For tests which don't care about bundling, just use false.
    do_tabm_roundtrips(UnsignedTX, UnsignedTABM, Commitment, false).
do_tabm_roundtrips(UnsignedTX, UnsignedTABM, Commitment, Bundle) ->
    Req = #{ <<"bundle">> => Bundle },
    Device = #{ <<"device">> => <<"tx@1.0">>, <<"bundle">> => Bundle },
    do_unsigned_tabm_roundtrip(UnsignedTX, UnsignedTABM, Req),
    do_signed_tabm_roundtrip(UnsignedTX, UnsignedTABM, Commitment, Device, Req).
    
do_unsigned_tabm_roundtrip(UnsignedTX0, UnsignedTABM, Req) ->
    UnsignedTX = UnsignedTX0#tx{ 
        unsigned_id = ar_tx:generate_id(UnsignedTX0, unsigned) },
    % TABM -> TX
    TX = hb_util:ok(to(UnsignedTABM, Req, #{})),
    ?event(debug_test, {unsigned_tabm_roundtrip, 
        {expected_tx, UnsignedTX}, {actual_tx, TX}}),
    ?assertEqual(UnsignedTX, TX, unsigned_tabm_roundtrip),
    % Serialize -> Deserialize
    JSON = ar_tx:tx_to_json_struct(TX),
    DeserializedTX = ar_tx:json_struct_to_tx(JSON),
    % TX -> TABM
    TABM = hb_util:ok(from(DeserializedTX, Req, #{})),
    ?event(debug_test, {unsigned_tabm_roundtrip,
        {expected_tabm, UnsignedTABM}, {actual_tabm, TABM}}),
    ?assertEqual(UnsignedTABM, TABM, unsigned_tabm_roundtrip).

do_signed_tabm_roundtrip(UnsignedTX, UnsignedTABM, Commitment, Device, Req) ->
    % Commit TABM
    Wallet = hb:wallet(),
    SignedTABM = hb_message:commit(
        UnsignedTABM, #{priv_wallet => Wallet}, Device),
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
    ?event(debug_test, {signed_tabm_roundtrip,
        {expected_commitment, ExpectedCommitment},
        {signed_commitment, SignedCommitment}}),
    ?assertEqual(ExpectedCommitment, SignedCommitment, signed_tabm_roundtrip),
    % TABM -> TX
    SignedTX = hb_util:ok(to(SignedTABM, Req, #{})),
    ?assert(ar_tx:verify(SignedTX), signed_tabm_roundtrip),
    ExpectedTX = ar_tx:sign(UnsignedTX, Wallet),
    ?assert(ar_tx:verify(ExpectedTX), signed_tabm_roundtrip),
    % Copy the SignedTX signature data over to the ExpectedTX since we expect
    % a different signature each time we sign.
    ?assertEqual(
        ExpectedTX#tx{ 
            unsigned_id = ar_tx:generate_id(ExpectedTX, unsigned),
            id = SignedTX#tx.id,
            signature = SignedTX#tx.signature
        }, SignedTX, signed_tabm_roundtrip),
    % TX -> TABM
    FinalTABM = hb_util:ok(from(SignedTX, Req, #{})),
    ?assertEqual(SignedTABM, FinalTABM, signed_tabm_roundtrip).