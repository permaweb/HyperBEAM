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
%%% The list of keys that should be forced into the tag list, rather than being
%%% encoded as fields in the TX record.
-define(FORCED_TAG_FIELDS,
    [
        <<"quantity">>,
        <<"manifest">>,
        <<"data_size">>,
        <<"data_tree">>,
        <<"data_root">>,
        <<"reward">>,
        <<"denomination">>,
        <<"signature_type">>
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
    ?event({committing, {input, Msg}}),
    Signed =
        ar_bundles:sign_item(
            hb_util:ok(to(hb_private:reset(Msg), Req, Opts)),
            hb_opts:get(priv_wallet, no_viable_wallet, Opts)
        ),
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
    TABM = hb_tx:tx_to_tabm(TX, ?TX_KEYS, <<"ans104@1.0">>, ?BASE_COMMITTED_TAGS, Req, Opts),
    Res = hb_maps:without(?FILTERED_TAGS, TABM, Opts),
    {ok, Res}.

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
to(NormTABM, Req, Opts) when is_map(NormTABM) ->
    TX = hb_tx:tabm_to_tx(NormTABM, Req, Opts),
    TXWithIDs =
        try ar_bundles:reset_ids(ar_bundles:normalize(TX))
        catch
            _:Error ->
                ?event({prepared_tx_before_ids,
                    {tags, {explicit, TX#tx.tags}},
                    {data, TX#tx.data}
                }),
                throw(Error)
        end,
    {ok, TXWithIDs};
to(_Other, _Req, _Opts) ->
    throw(invalid_tx).

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
