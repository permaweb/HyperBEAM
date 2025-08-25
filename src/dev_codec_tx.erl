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
    CommittedFields = dev_codec_tx_from:fields(TX, ?FIELD_PREFIX, Opts),
    WithCommitments = dev_codec_ans104_from:with_commitments(
        TX, CommittedFields, Tags, Base, Keys, Opts),
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
        MaybeBundle, fun dev_codec_tx_to:fields_to_tx/4, Opts),
    ?event({found_siginfo, TX0}),
    % Calculate and normalize the `data', if applicable.
    Data = dev_codec_ans104_to:data(MaybeBundle, Req, Opts),
    ?event({calculated_data, Data}),
    TX1 = TX0#tx { data = Data },
    % Calculate the tags for the TX.
    Tags = dev_codec_ans104_to:tags(
        TX1, MaybeBundle, Data, fun dev_codec_tx_to:excluded_tags/3, Opts),
    ?event({calculated_tags, Tags}),
    TX2 = TX1#tx { tags = Tags },
    ?event({tx_before_id_gen, TX2}),
    Res = normalize(TX2),
    ?event({to_result, Res}),
    {ok, Res};
to(Other, _Req, _Opts) ->
    throw({invalid_tx, Other}).

normalize(TX) ->
    reset_ids(normalize_data_root(ar_bundles:normalize_data(TX))).
    
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
    ?event(debug_test, {unsigned_tx_roundtrip,{expected_tabm, UnsignedTABM}, {actual_tabm, TABM}}),
    ?assertEqual(UnsignedTABM, TABM, unsigned_tx_roundtrip),

    TX = hb_util:ok(to(TABM, #{}, #{})),
    ExpectedTX = UnsignedTX#tx{ unsigned_id = ar_tx:id(UnsignedTX, unsigned) },
    ?event(debug_test, {unsigned_tx_roundtrip, {expected_tx, ExpectedTX}, {actual_tx, TX}}),
    ?assertEqual(ExpectedTX, TX, unsigned_tx_roundtrip).

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
    ?event(debug_test, {signed_tx_roundtrip, {expected_tabm, SignedTABM}, {actual_tabm, TABM}}),
    ?assertEqual(SignedTABM, TABM, signed_tx_roundtrip).