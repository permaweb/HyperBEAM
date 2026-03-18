%%% @doc Codec for managing transformations from `ar_tx'-style Arweave TX
%%% records to and from TABMs.
-module(dev_codec_tx).
-export([from/3, to/3, commit/3, verify/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(BASE_FIELDS, [
    <<"anchor">>, <<"format">>, <<"quantity">>, <<"reward">>, <<"target">>,
    <<"data_root">>, <<"data_size">> ]).

%% @doc Sign a message using the `priv_wallet' key in the options. Supports both
%% the `hmac-sha256' and `rsa-pss-sha256' algorithms, offering unsigned and
%% signed commitments.
commit(Msg, Req = #{ <<"type">> := <<"unsigned">> }, Opts) ->
    commit(Msg, Req#{ <<"type">> => <<"unsigned-sha256">> }, Opts);
commit(Msg, Req = #{ <<"type">> := <<"signed">> }, Opts) ->
    commit(Msg, Req#{ <<"type">> => ?RSA_SIGN_TYPE }, Opts);
commit(Msg, Req = #{ <<"type">> := ?RSA_SIGN_TYPE }, Opts) ->
    ?event({committing, {msg, Msg}, {req, Req}}),
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
    ?event({from, {raw_tx, hb_util:human_id(RawTX#tx.id)}}),
    % Assert a minimally valid TX record so we can avoid a lot of edge case
    % handling in the rest of the code.
    enforce_valid_tx(RawTX),
    TX = ar_bundles:deserialize(dev_arweave_common:normalize(RawTX)),
    ?event({from, {parsed_tx, hb_util:human_id(TX#tx.id)}}),
    % Get the fields, tags, and data from the TX.
    Fields = dev_codec_tx_from:fields(TX, <<>>, Opts),
    Tags = dev_codec_ans104_from:tags(TX, Opts),
    Data = dev_codec_ans104_from:data(TX, Req, Tags, Opts),
    ?event({from, {parsed_components, {fields, Fields}, {tags, Tags}, {data, Data}}}),
    % Calculate the committed keys on from the TX.
    Keys = dev_codec_ans104_from:committed(
        ?BASE_FIELDS, TX, Fields, Tags, Data, Opts),
    ?event({from, {determined_committed_keys, Keys}}),
    % Create the base message from the fields, tags, and data, filtering to
    % include only the keys that are committed. Will throw if a key is missing.
    Base = dev_codec_ans104_from:base(Keys, Fields, Tags, Data, Opts),
    ?event({from, {calculated_base_message, Base}}),
    % Add the commitments to the message if the TX has a signature.
    FieldCommitments = dev_codec_tx_from:fields(TX, ?FIELD_PREFIX, Opts),
    WithCommitments = dev_codec_ans104_from:with_commitments(
        ?BASE_FIELDS, TX, <<"tx@1.0">>, FieldCommitments,
        Tags, Base, Keys, Opts),
    ?event({from, {parsed_message, hb_util:human_id(TX#tx.id)}}),
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
        dev_arweave_common:normalize(#tx{
            format = 2,
            tags = [{<<"ao-type">>, <<"binary">>}],
            data = Binary
        })
    };
to(TX, _Req, _Opts) when is_record(TX, tx) -> {ok, TX};
to(RawTABM, Req, Opts) when is_map(RawTABM) ->
    % Ensure that the TABM is fully loaded if the `bundle` key is set to true.
    ?event({to, {inbound, RawTABM}, {req, Req}}),
    MaybeCommitment = hb_message:commitment(
        #{ <<"commitment-device">> => <<"tx@1.0">> },
        RawTABM,
        Opts
    ),
    IsBundle = dev_codec_ans104_to:is_bundle(MaybeCommitment, Req, Opts),
    MaybeBundle = dev_codec_ans104_to:maybe_load(RawTABM, IsBundle, Opts),
    ?event({to, {raw_tabm, RawTABM}, {is_bundle, IsBundle}, {maybe_bundle, MaybeBundle}, {req, Req}, {opts, Opts}}),
    % Calculate and normalize the `data', if applicable.
    Data = dev_codec_ans104_to:data(MaybeBundle, Req, Opts),
    ?event({calculated_data, Data}),
    TX0 = dev_codec_ans104_to:siginfo(
        MaybeBundle, MaybeCommitment,
        fun dev_codec_tx_to:fields_to_tx/4, Opts),
    ?event({found_siginfo, TX0}),
    TX1 = TX0#tx { data = Data },
    % Calculate the tags for the TX.
    Tags = dev_codec_ans104_to:tags(
        TX1, MaybeCommitment, MaybeBundle,
        dev_codec_tx_to:excluded_tags(TX1, MaybeBundle, Opts),
        Opts),
    ?event({calculated_tags, Tags}),
    TX2 = TX1#tx { tags = Tags },
    ?event({tx_before_id_gen, TX2}),
    FinalTX = dev_arweave_common:normalize(TX2),
    enforce_valid_tx(FinalTX),
    ?event({to_result, FinalTX}),
    {ok, FinalTX};
%% @doc List of ans104 items is bundled into a single L1 transaction.
to(RawList, Req, Opts) when is_list(RawList) ->
    List = lists:map(
        fun(Item) -> hb_util:ok(dev_codec_ans104:to(Item, Req, Opts)) end,
        RawList),
    TX = #tx{
        format = 2,
        data = List
    },
    Bundle = dev_arweave_common:normalize(TX),
    ?event({to_result, Bundle}),
    {ok, Bundle};
to(Other, _Req, _Opts) ->
    throw({invalid_tx, Other}).
    
%% @doc Verifies that the given transaction is a minimally valid signed or
%% unsigned transaction.
%% 
%% In particular:
%% 1. Values are of the correct type and size.
%% 2. In some cases where a limited number of values are allowed for a field, 
%%    those are checked as well (e.g. format is 1 or 2).
%% 3. Unsupported fields are set to their default values.
%% 
%% Of note: `data_root`/`data_size` are optional for value transfers and should
%% be preserved when present on the header, even if `data` is missing.
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
        hb_util:check_type(TX#tx.owner, binary),
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
        hb_util:check_type(TX#tx.signature, binary),
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
    % Arweave L1 #tx supports RSA and ECDSA signatures
    hb_util:ok_or_throw(TX,
        hb_util:check_value(TX#tx.signature_type, [?RSA_KEY_TYPE, ?ECDSA_KEY_TYPE]),
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
    ).

%%%===================================================================
%%% Tests.
%%%===================================================================

enforce_valid_tx_test() ->
    BaseTX = #tx{ format = 2 },
    InvalidUnsignedID = crypto:strong_rand_bytes(1),
    BadID31 = crypto:strong_rand_bytes(31),
    BadID33 = crypto:strong_rand_bytes(33),
    TooLongTagName = crypto:strong_rand_bytes(?MAX_TAG_NAME_SIZE + 1),
    TooLongTagValue = crypto:strong_rand_bytes(?MAX_TAG_VALUE_SIZE + 1),
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
        {owner_wrong_type, BaseTX#tx{owner = "hello"}, {invalid_field, owner, "hello"}},
        {target_too_short_31, BaseTX#tx{target = BadID31}, {invalid_field, target, BadID31}},
        {target_too_long_33, BaseTX#tx{target = BadID33}, {invalid_field, target, BadID33}},
        {quantity_not_integer, BaseTX#tx{quantity = <<"100">>}, {invalid_field, quantity, <<"100">>}},
        {data_size_not_integer, BaseTX#tx{data_size = an_atom}, {invalid_field, data_size, an_atom}},
        {data_root_too_short_31, BaseTX#tx{data_root = BadID31}, {invalid_field, data_root, BadID31}},
        {data_root_too_long_33, BaseTX#tx{data_root = BadID33}, {invalid_field, data_root, BadID33}},
        {signature_invalid_type, BaseTX#tx{signature = "hello"}, {invalid_field, signature, "hello"}},
        {reward_not_integer, BaseTX#tx{reward = 1.0}, {invalid_field, reward, 1.0}},
        {denomination_not_zero, BaseTX#tx{denomination = 1}, {invalid_field, denomination, 1}},
        %% ECDSA signature type is now supported, removed signature_type_not_rsa test
        {tags_not_list, BaseTX#tx{tags = #{}}, {invalid_field, tags, #{}}},
        {tag_name_not_binary, BaseTX#tx{tags = [{not_binary, <<"val">>}]}, {invalid_field, tag_name, not_binary}},
        {tag_name_too_long, BaseTX#tx{tags = [{TooLongTagName, <<"val">>}]}, {invalid_field, tag_name, TooLongTagName}},
        {tag_value_not_binary, BaseTX#tx{tags = [{<<"key">>, not_binary}]}, {invalid_field, tag_value, not_binary}},
        {tag_value_too_long, BaseTX#tx{tags = [{<<"key">>, TooLongTagValue}]}, {invalid_field, tag_value, TooLongTagValue}},
        {invalid_tag_form_atom, BaseTX#tx{tags = [not_a_tuple]}, {invalid_field, tag, not_a_tuple}},
        {invalid_tag_form_list, BaseTX#tx{tags = [[<<"name">>, <<"value">>]]}, {invalid_field, tag, [<<"name">>, <<"value">>]} }
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
        data_root = ar_tx:data_root(arweavejs, Data),
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
        <<"type">> => ?RSA_SIGN_TYPE,
        <<"bundle">> => <<"false">>,
        <<"field-target">> => hb_util:encode(Target),
        <<"field-anchor">> => hb_util:encode(Anchor),
        <<"field-quantity">> => <<"1000">>,
        <<"field-reward">> => <<"2000">>
    },
    do_tx_roundtrips(TX, UnsignedTABM, SignedCommitment).

data_header_but_no_data_test() ->
    Anchor = crypto:strong_rand_bytes(32),
    Target = crypto:strong_rand_bytes(32),
    Data = <<"test-data">>,
    DataRoot = ar_tx:data_root(arweavejs, Data),
    DataSize = byte_size(Data),
    UnsignedTX = #tx{
        format = 2,
        anchor = Anchor,
        tags = [
            {<<"tag1">>, <<"value1">>}
        ],
        target = Target,
        quantity = 1000,
        data_size = DataSize,
        data_root = DataRoot,
        reward = 2000
    },
    UnsignedTABM = #{
        <<"anchor">> => hb_util:encode(Anchor),
        <<"target">> => hb_util:encode(Target),
        <<"quantity">> => <<"1000">>,
        <<"reward">> => <<"2000">>,
        <<"data_root">> => hb_util:encode(DataRoot),
        <<"data_size">> => integer_to_binary(DataSize),
        <<"tag1">> => <<"value1">>
    },
    SignedCommitment = #{
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"committed">> => [
            <<"tag1">>, <<"anchor">>, <<"quantity">>, <<"reward">>,
            <<"target">>, <<"data_root">>, <<"data_size">>],
        <<"type">> => ?RSA_SIGN_TYPE,
        <<"bundle">> => <<"false">>,
        <<"field-target">> => hb_util:encode(Target),
        <<"field-anchor">> => hb_util:encode(Anchor),
        <<"field-quantity">> => <<"1000">>,
        <<"field-reward">> => <<"2000">>,
        <<"field-data_root">> => hb_util:encode(DataRoot),
        <<"field-data_size">> => integer_to_binary(DataSize)
    },
    do_tx_roundtrips(
        UnsignedTX,
        UnsignedTABM,
        SignedCommitment,
        #{
            <<"bundle">> => false,
            <<"exclude-data">> => true
        }
    ).

data_tag_with_data_test() ->
    Data = <<"myrealdata">>,
    TX = #tx{
        format = 2,
        tags = [
            {<<"data">>, <<"tagdata">>}
        ],
        data = Data,
        data_size = byte_size(Data),
        data_root = ar_tx:data_root(arweavejs, Data)
    },
    UnsignedID = dev_arweave_common:generate_id(TX, unsigned),
    UnsignedTABM = #{
        <<"commitments">> => #{
            hb_util:encode(UnsignedID) => #{
                <<"commitment-device">> => <<"tx@1.0">>,
                <<"committed">> => [<<"data">>],
                <<"original-tags">> =>#{
                    <<"1">> => #{
                        <<"name">> => <<"data">>,
                        <<"value">> => <<"tagdata">>
                    }
                },
                <<"type">> => <<"unsigned-sha256">>,
                <<"bundle">> => <<"false">>
            }
        },
        <<"data">> => Data
    },
    SignedCommitment = #{
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"committed">> => [<<"data">>],
        <<"original-tags">> =>#{
            <<"1">> => #{
                <<"name">> => <<"data">>,
                <<"value">> => <<"tagdata">>
            }
        },
        <<"type">> => ?RSA_SIGN_TYPE,
        <<"bundle">> => <<"false">>
    },
    do_tx_roundtrips(TX, UnsignedTABM, SignedCommitment).

data_tag_no_data_test() ->
    Data = <<"myrealdata">>,
    DataRoot = ar_tx:data_root(arweavejs, Data),
    DataSize = byte_size(Data),
    TX = #tx{
        format = 2,
        tags = [
            {<<"data">>, <<"tagdata">>}
        ],
        data_size = DataSize,
        data_root = DataRoot
    },
    UnsignedID = dev_arweave_common:generate_id(TX, unsigned),
    UnsignedTABM = #{
        <<"commitments">> => #{
            hb_util:encode(UnsignedID) => #{
                <<"commitment-device">> => <<"tx@1.0">>,
                <<"committed">> => [<<"data_root">>, <<"data_size">>],
                <<"field-data_root">> => 
                    hb_util:encode(DataRoot),
                <<"field-data_size">> =>
                    integer_to_binary(DataSize),
                <<"original-tags">> =>#{
                    <<"1">> => #{
                        <<"name">> => <<"data">>,
                        <<"value">> => <<"tagdata">>
                    }
                },
                <<"type">> => <<"unsigned-sha256">>,
                <<"bundle">> => <<"false">>
            }
        },
        <<"data_root">> => hb_util:encode(DataRoot),
        <<"data_size">> => integer_to_binary(DataSize)
    },
    SignedCommitment = #{
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"committed">> => [<<"data_root">>, <<"data_size">>],
        <<"field-data_root">> => 
            hb_util:encode(DataRoot),
        <<"field-data_size">> =>
            integer_to_binary(DataSize),
        <<"original-tags">> =>#{
            <<"1">> => #{
                <<"name">> => <<"data">>,
                <<"value">> => <<"tagdata">>
            }
        },
        <<"type">> => ?RSA_SIGN_TYPE,
        <<"bundle">> => <<"false">>
    },
    do_tx_roundtrips(TX, UnsignedTABM, SignedCommitment).

tag_name_case_test() ->
    TX = #tx{
        format = 2,
        tags = [
            {<<"Test-Tag">>, <<"test-value">>}
        ]
    },
    UnsignedID = dev_arweave_common:generate_id(TX, unsigned),
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
        <<"type">> => ?RSA_SIGN_TYPE,
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
    UnsignedID = dev_arweave_common:generate_id(TX, unsigned),
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
        <<"type">> => ?RSA_SIGN_TYPE,
        <<"bundle">> => <<"false">>
    },
    do_tx_roundtrips(TX, UnsignedTABM, SignedCommitment).

tags_and_fields_test() ->
    AnchorTag = crypto:strong_rand_bytes(32),
    TargetTag = crypto:strong_rand_bytes(32),
    DataRootTag = crypto:strong_rand_bytes(32),
    AnchorField = crypto:strong_rand_bytes(32),
    TargetField = crypto:strong_rand_bytes(32),
    DataRootField = crypto:strong_rand_bytes(32),
    TX = #tx{
        
        tags = [
            {<<"anchor">>, hb_util:encode(AnchorTag)},
            {<<"format">>, <<"1">>},
            {<<"quantity">>, <<"1000">>},
            {<<"reward">>, <<"2000">>},
            {<<"target">>, hb_util:encode(TargetTag)},
            {<<"data_root">>, hb_util:encode(DataRootTag)},
            {<<"data_size">>, <<"100">>}
        ],
        anchor = AnchorField,
        format = 2,
        quantity = 5,
        reward = 6,
        target = TargetField,
        data_root = DataRootField,
        data_size = 7
    },
    UnsignedID = dev_arweave_common:generate_id(TX, unsigned),
    UnsignedTABM = #{
        <<"anchor">> => hb_util:encode(AnchorField),
        <<"commitments">> => #{
            hb_util:encode(UnsignedID) => #{
                <<"commitment-device">> => <<"tx@1.0">>,
                <<"committed">> => [<<"anchor">>, <<"format">>, 
                    <<"quantity">>, <<"reward">>, <<"target">>,
                    <<"data_root">>, <<"data_size">>],
                <<"field-anchor">> => hb_util:encode(AnchorField),
                <<"field-quantity">> => <<"5">>,
                <<"field-reward">> => <<"6">>,
                <<"field-target">> => hb_util:encode(TargetField),
                <<"field-data_root">> => hb_util:encode(DataRootField),
                <<"field-data_size">> => <<"7">>,
                <<"original-tags">> =>#{
                    <<"1">> => #{
                        <<"name">> => <<"anchor">>,
                        <<"value">> => hb_util:encode(AnchorTag)
                    },
                    <<"2">> => #{
                        <<"name">> => <<"format">>,
                        <<"value">> => <<"1">>
                    },
                    <<"3">> => #{
                        <<"name">> => <<"quantity">>,
                        <<"value">> => <<"1000">>
                    },
                    <<"4">> => #{
                        <<"name">> => <<"reward">>,
                        <<"value">> => <<"2000">>
                    },
                    <<"5">> => #{
                        <<"name">> => <<"target">>,
                        <<"value">> => hb_util:encode(TargetTag)
                    },
                    <<"6">> => #{
                        <<"name">> => <<"data_root">>,
                        <<"value">> => hb_util:encode(DataRootTag)
                    },
                    <<"7">> => #{
                        <<"name">> => <<"data_size">>,
                        <<"value">> => <<"100">>
                    }
                },
                <<"type">> => <<"unsigned-sha256">>,
                <<"bundle">> => <<"false">>
            }
        },
        <<"quantity">> => <<"5">>,
        <<"reward">> => <<"6">>,
        <<"target">> => hb_util:encode(TargetField),
        <<"data_root">> => hb_util:encode(DataRootField),
        <<"data_size">> => <<"7">>,
        <<"format">> => <<"1">>
    },
    SignedCommitment = #{
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"committed">> => [<<"anchor">>, <<"format">>, 
            <<"quantity">>, <<"reward">>, <<"target">>,
            <<"data_root">>, <<"data_size">>],
        <<"field-anchor">> => hb_util:encode(AnchorField),
        <<"field-quantity">> => <<"5">>,
        <<"field-reward">> => <<"6">>,
        <<"field-target">> => hb_util:encode(TargetField),
        <<"field-data_root">> => hb_util:encode(DataRootField),
        <<"field-data_size">> => <<"7">>,
        <<"original-tags">> =>#{
            <<"1">> => #{
                <<"name">> => <<"anchor">>,
                <<"value">> => hb_util:encode(AnchorTag)
            },
            <<"2">> => #{
                <<"name">> => <<"format">>,
                <<"value">> => <<"1">>
            },
            <<"3">> => #{
                <<"name">> => <<"quantity">>,
                <<"value">> => <<"1000">>
            },
            <<"4">> => #{
                <<"name">> => <<"reward">>,
                <<"value">> => <<"2000">>
            },
            <<"5">> => #{
                <<"name">> => <<"target">>,
                <<"value">> => hb_util:encode(TargetTag)
            },
            <<"6">> => #{
                <<"name">> => <<"data_root">>,
                <<"value">> => hb_util:encode(DataRootTag)
            },
            <<"7">> => #{
                <<"name">> => <<"data_size">>,
                <<"value">> => <<"100">>
            }
        },
        <<"type">> => ?RSA_SIGN_TYPE,
        <<"bundle">> => <<"false">>
    },
    do_tx_roundtrips(TX, UnsignedTABM, SignedCommitment).

tags_no_fields_test() ->
    AnchorTag = crypto:strong_rand_bytes(32),
    TargetTag = crypto:strong_rand_bytes(32),
    DataRootTag = crypto:strong_rand_bytes(32),
    TX = #tx{
        tags = [
            {<<"anchor">>, hb_util:encode(AnchorTag)},
            {<<"format">>, <<"1">>},
            {<<"quantity">>, <<"1000">>},
            {<<"reward">>, <<"2000">>},
            {<<"target">>, hb_util:encode(TargetTag)},
            {<<"data_root">>, hb_util:encode(DataRootTag)},
            {<<"data_size">>, <<"100">>}
        ],
        format = 2
    },
    UnsignedID = dev_arweave_common:generate_id(TX, unsigned),
    UnsignedTABM = #{
        <<"commitments">> => #{
            hb_util:encode(UnsignedID) => #{
                <<"commitment-device">> => <<"tx@1.0">>,
                <<"committed">> => [<<"anchor">>, <<"format">>, 
                    <<"quantity">>, <<"reward">>, <<"target">>,
                    <<"data_root">>, <<"data_size">>],
                <<"original-tags">> =>#{
                    <<"1">> => #{
                        <<"name">> => <<"anchor">>,
                        <<"value">> => hb_util:encode(AnchorTag)
                    },
                    <<"2">> => #{
                        <<"name">> => <<"format">>,
                        <<"value">> => <<"1">>
                    },
                    <<"3">> => #{
                        <<"name">> => <<"quantity">>,
                        <<"value">> => <<"1000">>
                    },
                    <<"4">> => #{
                        <<"name">> => <<"reward">>,
                        <<"value">> => <<"2000">>
                    },
                    <<"5">> => #{
                        <<"name">> => <<"target">>,
                        <<"value">> => hb_util:encode(TargetTag)
                    },
                    <<"6">> => #{
                        <<"name">> => <<"data_root">>,
                        <<"value">> => hb_util:encode(DataRootTag)
                    },
                    <<"7">> => #{
                        <<"name">> => <<"data_size">>,
                        <<"value">> => <<"100">>
                    }
                },
                <<"type">> => <<"unsigned-sha256">>,
                <<"bundle">> => <<"false">>
            }
        },
        <<"anchor">> => hb_util:encode(AnchorTag),
        <<"quantity">> => <<"1000">>,
        <<"reward">> => <<"2000">>,
        <<"target">> => hb_util:encode(TargetTag),
        <<"data_root">> => hb_util:encode(DataRootTag),
        <<"data_size">> => <<"100">>,
        <<"format">> => <<"1">>
    },
    SignedCommitment = #{
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"committed">> => [<<"anchor">>, <<"format">>, 
            <<"quantity">>, <<"reward">>, <<"target">>,
            <<"data_root">>, <<"data_size">>],
        <<"original-tags">> =>#{
            <<"1">> => #{
                <<"name">> => <<"anchor">>,
                <<"value">> => hb_util:encode(AnchorTag)
            },
            <<"2">> => #{
                <<"name">> => <<"format">>,
                <<"value">> => <<"1">>
            },
            <<"3">> => #{
                <<"name">> => <<"quantity">>,
                <<"value">> => <<"1000">>
            },
            <<"4">> => #{
                <<"name">> => <<"reward">>,
                <<"value">> => <<"2000">>
            },
            <<"5">> => #{
                <<"name">> => <<"target">>,
                <<"value">> => hb_util:encode(TargetTag)
            },
            <<"6">> => #{
                <<"name">> => <<"data_root">>,
                <<"value">> => hb_util:encode(DataRootTag)
            },
            <<"7">> => #{
                <<"name">> => <<"data_size">>,
                <<"value">> => <<"100">>
            }
        },
        <<"type">> => ?RSA_SIGN_TYPE,
        <<"bundle">> => <<"false">>
    },
    do_tx_roundtrips(TX, UnsignedTABM, SignedCommitment).

%% @doc Test that when a TABM has base field keys set to values that are not
%% valid on a #tx record, they are preserved as tags instead.
non_conforming_fields_test() ->
    Anchor = <<"NON-ID-ANCHOR">>,
    DataRoot = <<"NON-ID-DATA-ROOT">>,
    Quantity = <<"NON-INT-QUANTITY">>,
    Reward = <<"NON-INT-REWARD">>,
    Target = <<"NON-ID-TARGET">>,
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
    UnsignedID = dev_arweave_common:generate_id(UnsignedTX, unsigned),
    UnsignedTABM = #{
        <<"commitments">> => #{
            hb_util:encode(UnsignedID) => #{
                <<"commitment-device">> => <<"tx@1.0">>,
                <<"committed">> => [<<"anchor">>, <<"data_root">>, 
                    <<"quantity">>, <<"reward">>, <<"tag1">>,
                    <<"tag2">>, <<"target">>],
                <<"original-tags">> =>#{
                    <<"1">> => #{
                        <<"name">> => <<"anchor">>,
                        <<"value">> => Anchor
                    },
                    <<"2">> => #{
                        <<"name">> => <<"data_root">>,
                        <<"value">> => DataRoot
                    },
                    <<"3">> => #{
                        <<"name">> => <<"quantity">>,
                        <<"value">> => Quantity
                    },
                    <<"4">> => #{
                        <<"name">> => <<"reward">>,
                        <<"value">> => Reward
                    },
                    <<"5">> => #{
                        <<"name">> => <<"tag1">>,
                        <<"value">> => <<"value1">>
                    },
                    <<"6">> => #{
                        <<"name">> => <<"tag2">>,
                        <<"value">> => <<"value2">>
                    },
                    <<"7">> => #{
                        <<"name">> => <<"target">>,
                        <<"value">> => Target
                    }
                },
                <<"type">> => <<"unsigned-sha256">>,
                <<"bundle">> => <<"false">>
            }
        },
        <<"anchor">> => Anchor,
        <<"target">> => Target,
        <<"quantity">> => Quantity,
        <<"reward">> => Reward,
        <<"data_root">> => DataRoot,
        <<"tag1">> => <<"value1">>,
        <<"tag2">> => <<"value2">>
    },

    SignedCommitment = #{
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"committed">> => [<<"anchor">>, <<"data_root">>, 
            <<"quantity">>, <<"reward">>, <<"tag1">>,
            <<"tag2">>, <<"target">>],
        <<"original-tags">> =>#{
            <<"1">> => #{
                <<"name">> => <<"anchor">>,
                <<"value">> => Anchor
            },
            <<"2">> => #{
                <<"name">> => <<"data_root">>,
                <<"value">> => DataRoot
            },
            <<"3">> => #{
                <<"name">> => <<"quantity">>,
                <<"value">> => Quantity
            },
            <<"4">> => #{
                <<"name">> => <<"reward">>,
                <<"value">> => Reward
            },
            <<"5">> => #{
                <<"name">> => <<"tag1">>,
                <<"value">> => <<"value1">>
            },
            <<"6">> => #{
                <<"name">> => <<"tag2">>,
                <<"value">> => <<"value2">>
            },
            <<"7">> => #{
                <<"name">> => <<"target">>,
                <<"value">> => Target
            }
        },
        <<"type">> => ?RSA_SIGN_TYPE,
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
        data_root = ar_tx:data_root(arweavejs, Data)
    },
    SignedCommitment = #{
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"committed">> => [<<"body">>, <<"tag1">>],
        <<"type">> => ?RSA_SIGN_TYPE,
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
        <<"type">> => ?RSA_SIGN_TYPE,
        <<"bundle">> => <<"false">>
    },
    % Only do a signed test since we don't need the tag order to be preserved
    % for messages without a commitment. And since this test case doesn't
    % require an original-tags commitment, no unsigned commitment will be
    % generated.
    do_signed_tx_roundtrip(TX, UnsignedTABM, SignedCommitment, #{}).

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
    UnsignedTX = dev_arweave_common:normalize(TX),
    NoLinksCommitment = #{
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"committed">> => [<<"data">>, <<"tag">>],
        <<"type">> => ?RSA_SIGN_TYPE,
        <<"bundle">> => <<"true">>,
        <<"bundle-format">> => <<"binary">>,
        <<"bundle-version">> => <<"2.0.0">>,
        <<"bundle-map">> => <<"ucPqsShS_YNxyPdPbcDpZzxBvpu_eIppvaFM_nzB-CA">>
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
    UnsignedTX = dev_arweave_common:normalize(TX),
    NoLinksCommitment = #{
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"committed">> => [<<"a1">>, <<"tag1">>],
        <<"type">> => ?RSA_SIGN_TYPE,
        <<"bundle">> => <<"true">>,
        <<"bundle-format">> => <<"binary">>,
        <<"bundle-version">> => <<"2.0.0">>,
        <<"bundle-map">> => <<"dO5bHNSlNCDS-kOv435QJ7Z_z--TJGa3avQog0f0DDw">>
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
    UnsignedTX = dev_arweave_common:normalize(TX),
    NoLinksCommitment = #{
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"committed">> => [<<"a1">>, <<"data">>, <<"tag1">>],
        <<"type">> => ?RSA_SIGN_TYPE,
        <<"bundle">> => <<"true">>,
        <<"bundle-format">> => <<"binary">>,
        <<"bundle-version">> => <<"2.0.0">>,
        <<"bundle-map">> => <<"8dP-rTKhUiDOnDf1BNGFl0yYpRCrhtfVcbSgImZ4bJI">>
    },
    % only bundle true is supported
    do_tabm_roundtrips(UnsignedTX, UnsignedTABM, NoLinksCommitment, true).

real_basic_data_tx_test() ->
    do_real_tx_verify(
        <<"ptBC0UwDmrUTBQX3MqZ1lB57ex20ygwzkjjCrQjIx3o">>,
        [<<"ptBC0UwDmrUTBQX3MqZ1lB57ex20ygwzkjjCrQjIx3o">>]
    ).

real_rsa_nested_bundle_tx_test() ->
    do_real_tx_verify(
        <<"bndIwac23-s0K11TLC1N7z472sLGAkiOdhds87ZywoE">>,
        [
            <<"bndIwac23-s0K11TLC1N7z472sLGAkiOdhds87ZywoE">>,
            <<"8_YOiWq-vc7bErBIef0J-AJ5AOq0ik_GoqBsw2rxmH0">>,
            <<"3MyW4IFKB4ZqBog7N31wKwun__AnGseuZNP0GuRdo7c">>,
            <<"swN9cX9-vwB1eCn8OygZ1J13Aibs1K7m2dkpoygYpkA">>,
            <<"LDcC_5NM9J9kMLry5RAUKGo3QoSkNDeAm_kLPCo83_k">>,
            <<"34r40QBNWF2sSE2FjXD44AnJVgEFtK3cOxk5RSNbd8A">>,
            <<"ephwZY1QMLNNup2uKl_q9avkph8nr3oRY-QFOKOE6wk">>
        ]
    ).

%% @doc Disabled until we support ECDSA signatures.
real_ecdsa_bundle_tx_test_disabled() ->
    % 12 items, no mint
    do_real_tx_verify(
        <<"EOARN0wNp4qttWgd15k6IeylsZ88vI2ZeaW2b-mJRkg">>,
        []
    ).

real_ecdsa_single_item_bundle_tx_test_disabled() ->
    do_real_tx_verify(
        <<"5CHMPU1oDCiqwrjGG5PEh7mht9VdVFnnF9yGfjPehno">>,
        []
    ).

real_ecdsa_no_data_tx_test() ->
    do_real_tx_verify(
        <<"p42_hnfcQd2ESry-WY8x9RcMbaw6piapVM1CpErzf8Y">>,
        [<<"p42_hnfcQd2ESry-WY8x9RcMbaw6piapVM1CpErzf8Y">>]
    ).

real_ecdsa_data_tx_test() ->
    do_real_tx_verify(
        <<"LomQXlcWSeJdhg26wXQwiE_spvAfEq4nULkUMV_1Fqo">>,
        [<<"LomQXlcWSeJdhg26wXQwiE_spvAfEq4nULkUMV_1Fqo">>]
    ).

real_2048_bit_rsa_tx_test() ->
    do_real_tx_verify(
        <<"tj76flZk936u0S2owyEzUFBvBAYle9Al5LH8zJ7icNc">>,
        [<<"tj76flZk936u0S2owyEzUFBvBAYle9Al5LH8zJ7icNc">>]
    ).

real_no_data_tx_test() ->
    do_real_tx_verify(
        <<"N1Cyu67lQtmZMQlIZVFpNfy3xz6k9wEZ8LLeDbOebbk">>,
        [<<"N1Cyu67lQtmZMQlIZVFpNfy3xz6k9wEZ8LLeDbOebbk">>]
    ).

do_real_tx_verify(TXID, ExpectedIDs) ->
    Opts = #{},
    {ok, #{ <<"body">> := TXJSON }} = hb_http:request(
        #{
            <<"path">> => <<"/arweave/tx/", TXID/binary>>,
            <<"method">> => <<"GET">>
        },
        Opts
    ),
    TXHeader = ar_tx:json_struct_to_tx(hb_json:decode(TXJSON)),
    TX = case hb_http:request(
        #{
            <<"path">> => <<"/arweave/raw/", TXID/binary>>,
            <<"method">> => <<"GET">>
        },
        Opts
    ) of
        {ok, #{ <<"body">> := Data }} ->
            ?event(debug_test, {
                {tx_id, TXID},
                {size, byte_size(Data)},
                {data, Data}
            }),
            TXHeader#tx{ data = Data };
        {ok, _} ->
            TXHeader#tx{ data = ?DEFAULT_DATA };
        {error, #{ <<"status">> := 404 }} ->
            TXHeader#tx{ data = ?DEFAULT_DATA };
        {error, Error} ->
            throw({http_request_error, Error})
    end,
    ?event(debug_test, {tx, TX}),
    ?assert(ar_tx:verify(TX)),
    StructuredTX = hb_message:convert(
        TX,
        <<"structured@1.0">>,
        <<"tx@1.0">>,
        Opts
    ),
    ?event(debug_test, {structured_tx, StructuredTX}),
    ?assert(hb_message:verify(StructuredTX, all, #{})),
    Deserialized = ar_bundles:deserialize(TX),
    verify_items(Deserialized, ExpectedIDs, Opts).

verify_items(RootItem, ExpectedIDs, Opts) ->
    AllItems = flatten_items(RootItem),
    ?assertEqual(length(ExpectedIDs), length(AllItems)),
    [RootItem | NestedItems] = AllItems,
    [RootID | NestedIDs] = ExpectedIDs,
    NormalizedRootItem = dev_arweave_common:normalize(RootItem),
    ?assert(
        ar_tx:verify(NormalizedRootItem),
        hb_util:encode(RootItem#tx.id)),
    StructuredRootItem = hb_message:convert(
        NormalizedRootItem,
        <<"structured@1.0">>,
        <<"tx@1.0">>,
        Opts
    ),
    ?assert(hb_message:verify(StructuredRootItem, all, Opts),
        hb_util:encode(RootItem#tx.id)),
    ?assertEqual(RootID, hb_util:encode(RootItem#tx.id)),
    lists:zipwith(
        fun(Item, ExpectedID) ->
            ?assert(ar_bundles:verify_item(Item), hb_util:encode(Item#tx.id)),
            StructuredItem = hb_message:convert(
                Item,
                <<"structured@1.0">>,
                <<"ans104@1.0">>,
                Opts
            ),
            ?assert(hb_message:verify(StructuredItem, all, Opts),
                hb_util:encode(Item#tx.id)),
            ?assertEqual(ExpectedID, hb_util:encode(Item#tx.id))
        end,
        NestedItems,
        NestedIDs
    ).

flatten_items(Item) when is_record(Item, tx) ->
    NestedItems = case Item#tx.data of
        Data when is_map(Data) ->
            SortedKeys = lists:sort(maps:keys(Data)),
            lists:flatmap(
                fun(Key) ->
                    flatten_items(maps:get(Key, Data))
                end,
                SortedKeys
            );
        _ ->
            []
    end,
    [Item | NestedItems];
flatten_items(_) ->
    [].

%% @doc Run a series of roundtrip tests that start and end with a #tx record
do_tx_roundtrips(UnsignedTX, UnsignedTABM, Commitment) ->
    % For tests which don't care about bundling, just use false.
    do_tx_roundtrips(UnsignedTX, UnsignedTABM, Commitment, false).
do_tx_roundtrips(UnsignedTX, UnsignedTABM, Commitment, Req) when is_map(Req) ->
    do_unsigned_tx_roundtrip(UnsignedTX, UnsignedTABM, Req),
    do_signed_tx_roundtrip(UnsignedTX, UnsignedTABM, Commitment, Req);
do_tx_roundtrips(UnsignedTX, UnsignedTABM, Commitment, Bundle)
        when is_boolean(Bundle) ->
    Req = #{ <<"bundle">> => Bundle },
    do_tx_roundtrips(UnsignedTX, UnsignedTABM, Commitment, Req).

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
        unsigned_id = dev_arweave_common:generate_id(SignedTX, unsigned) },
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
        unsigned_id = dev_arweave_common:generate_id(UnsignedTX0, unsigned) },
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
            unsigned_id = dev_arweave_common:generate_id(ExpectedTX, unsigned),
            id = SignedTX#tx.id,
            signature = SignedTX#tx.signature
        }, SignedTX, signed_tabm_roundtrip),
    % TX -> TABM
    FinalTABM = hb_util:ok(from(SignedTX, Req, #{})),
    ?assertEqual(SignedTABM, FinalTABM, signed_tabm_roundtrip).

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
        #{ <<"device">> => <<"tx@1.0">>, <<"bundle">> => ToBool(Commit) }),
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
        #{ <<"device">> => <<"tx@1.0">>, <<"bundle">> => ToBool(Encode) },
        <<"structured@1.0">>, Opts),
    ?event(debug_test, {encoded, Label, {explicit, Encoded}}),
    ?assert(ar_tx:verify(Encoded), Label),
    %% IF the input message is unbundled, #tx.data should be empty.
    ?assertEqual(ToBool(Commit), Encoded#tx.data /= <<>>, Label),

    Decoded = hb_message:convert(Encoded, 
        #{ <<"device">> => <<"structured@1.0">>, <<"bundle">> => ToBool(Decode) },
        #{ <<"device">> => <<"tx@1.0">>, <<"bundle">> => ToBool(Encode) },
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
        #{ <<"device">> => <<"tx@1.0">>, <<"bundle">> => ToBool(Encode) },
        <<"structured@1.0">>, Opts),
    ?event(debug_test, {encoded, Label, {explicit, Encoded}}),
    %% IF the input message is unbundled, #tx.data should be empty.
    ?assertEqual(ToBool(Encode), Encoded#tx.data /= <<>>, Label),

    Decoded = hb_message:convert(Encoded, 
        #{ <<"device">> => <<"structured@1.0">>, <<"bundle">> => ToBool(Decode) },
        #{ <<"device">> => <<"tx@1.0">>, <<"bundle">> => ToBool(Encode) },
        Opts),
    ?event(debug_test, {decoded, Label, {explicit, Decoded}}),
    case Encode of
        unbundled ->
            ?assertNotEqual([1, 2, 3], maps:get(<<"list">>, Decoded, Opts), Label);
        bundled ->
            ?assertEqual([1, 2, 3], maps:get(<<"list">>, Decoded, Opts), Label)
    end,
    ok.

bundle_list_test() ->
    % Load an arweave.js-created dataitem
    Item = ar_bundles:deserialize(
        hb_util:ok(
            file:read_file(<<"test/arbundles.js/ans104-item.bundle">>)
        )
    ),
    ?event(debug_test, {item, Item}),
    ?assert(ar_bundles:verify_item(Item)),
    % Load an arweave.js-created list bundle
    {ok, Bin} = file:read_file(<<"test/arbundles.js/ans104-list-bundle.bundle">>),
    BundledItem = ar_bundles:sign_item(#tx{ 
        format = ans104,
        data = Bin,
        data_size = byte_size(Bin),
        tags = [
            {<<"Bundle-Format">>, <<"binary">>},
            {<<"Bundle-Version">>, <<"2.0.0">>}
        ]
    }, hb:wallet()),
    ?event(debug_test, {bundled_item, BundledItem}),
    ?assert(ar_bundles:verify_item(BundledItem)),
    % Convert both dataitems to structured messages
    ItemStructured = hb_message:convert(Item,
        #{ <<"device">> => <<"structured@1.0">>, <<"bundle">> => true },
        #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => true },
        #{}),
    ?event(debug_test, {item_structured, ItemStructured}),
    ?assert(hb_message:verify(ItemStructured, all, #{})),
    BundledItemStructured = hb_message:convert(BundledItem,
        #{ <<"device">> => <<"structured@1.0">>, <<"bundle">> => true },
        #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => true },
        #{}),
    ?event(debug_test, {bundled_item_structured, BundledItemStructured}),
    ?assert(hb_message:verify(BundledItemStructured, all, #{})),
    % Use dev_codec_tx:to(List) to create a L1 TX bundle. We use this
    % interface to mimic the logic used in dev_bundler
    {ok, BundledTX} = dev_codec_tx:to(
        [ItemStructured, BundledItemStructured], #{}, #{}),
    SignedTX = ar_tx:sign(BundledTX, hb:wallet()),
    ?event(debug_test, {signed_tx, SignedTX}),
    ?assert(ar_tx:verify(SignedTX)),
    % Convert the signed TX to a structured message
    StructuredTX = hb_message:convert(SignedTX,
        #{ <<"device">> => <<"structured@1.0">>, <<"bundle">> => true },
        #{ <<"device">> => <<"tx@1.0">>, <<"bundle">> => true },
        #{}),
    ?event(debug_test, {structured_tx, StructuredTX}),
    ?assert(hb_message:verify(StructuredTX, all, #{})),
    % Convert back to an L1 TX
    SignedTXRoundtrip = hb_message:convert(StructuredTX,
        #{ <<"device">> => <<"tx@1.0">>, <<"bundle">> => true },
        #{ <<"device">> => <<"structured@1.0">>, <<"bundle">> => true },
        #{}),
    ?event(debug_test, {signed_tx_roundtrip, SignedTXRoundtrip}),
    ?assert(ar_tx:verify(SignedTXRoundtrip)),
    ?assertEqual(SignedTX, SignedTXRoundtrip),
    ok.