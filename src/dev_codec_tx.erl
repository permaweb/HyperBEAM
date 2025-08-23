%%% @doc Codec for managing transformations from `ar_tx'-style Arweave TX
%%% records to and from TABMs.
-module(dev_codec_tx).
-export([from/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(BASE_FIELDS, [
    <<"anchor">>, <<"data_root">>, <<"format">>,
    <<"quantity">>, <<"reward">>, <<"target">> ]).

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
    TX = RawTX, %ar_bundles:deserialize(ar_bundles:normalize(RawTX)),
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
    CommittedFields = dev_codec_tx_from:fields(TX, <<"field-">>, Opts),
    WithCommitments = dev_codec_ans104_from:with_commitments(
        TX, CommittedFields, Tags, Base, Keys, Opts),
    ?event({parsed_message, WithCommitments}),
    {ok, WithCommitments}.

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
    Commitment = #{
        <<"commitment-device">> => <<"ans104@1.0">>,
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
    do_roundtrips(TX, UnsignedTABM, Commitment).


do_roundtrips(UnsignedTX, UnsignedTABM, Commitment) ->
   do_unsigned_tx_roundtrip(UnsignedTX, UnsignedTABM),
   do_signed_tx_roundtrip(UnsignedTX, UnsignedTABM, Commitment).

do_unsigned_tx_roundtrip(UnsignedTX, UnsignedTABM) ->
    TABM = hb_util:ok(from(UnsignedTX, #{}, #{})),
    ?event(debug_test, {unsigned_tx_roundtrip,{expected, UnsignedTABM}, {actual, TABM}}),
    ?assertEqual(UnsignedTABM, TABM, unsigned_tx_roundtrip).

do_signed_tx_roundtrip(UnsignedTX, UnsignedTABM, Commitment) ->
    SignedTX = ar_tx:sign(UnsignedTX, hb:wallet()),
    ?assert(ar_tx:verify(SignedTX), signed_tx_roundtrip),
    TABM = hb_util:ok(from(SignedTX, #{}, #{})),

    SignedCommitment = Commitment#{
        <<"committer">> => hb_util:human_id(SignedTX#tx.owner_address),
        <<"signature">> => hb_util:encode(SignedTX#tx.signature),
        <<"keyid">> =>
            <<"publickey:", (hb_util:encode(SignedTX#tx.owner))/binary>>
    },
    SignedTABM = UnsignedTABM#{
        <<"commitments">> => #{ hb_util:human_id(SignedTX#tx.id) => SignedCommitment }},
    ?event(debug_test, {signed_tx_roundtrip, {expected, SignedTABM}, {actual, TABM}}),
    ?assertEqual(SignedTABM, TABM, signed_tx_roundtrip).