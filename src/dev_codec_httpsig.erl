%%% @doc This module implements HTTP Message Signatures as described in RFC-9421
%%% (https://datatracker.ietf.org/doc/html/rfc9421), as an AO-Core device.
%%% It implements the codec standard (from/1, to/1), as well as the optional
%%% commitment functions (id/3, sign/3, verify/3). The commitment functions
%%% are found in this module, while the codec functions are relayed to the
%%% `dev_codec_httpsig_conv' module.
-module(dev_codec_httpsig).
%%% Codec API functions
-export([to/3, from/3]).
%%% Uni-directional codec support (_to_ binary/header+body components), but not
%%% back.
-export([serialize/2, serialize/3]).
%%% Commitment API functions
-export([commit/3, verify/3]).
%%% Public API functions
-export([add_content_digest/2, normalize_for_encoding/3]).
-include("include/hb.hrl").

-include_lib("eunit/include/eunit.hrl").


%%% Routing functions for the `dev_codec_httpsig_conv' module
to(Msg, Req, Opts) -> dev_codec_httpsig_conv:to(Msg, Req, Opts).


from(Msg, Req, Opts) -> dev_codec_httpsig_conv:from(Msg, Req, Opts).


%% @doc Generate the `Opts' to use during AO-Core operations in the codec.
opts(RawOpts) ->
    RawOpts#{
      hashpath => ignore,
      cache_control => [<<"no-cache">>, <<"no-store">>],
      force_message => false
     }.


%% @doc A helper utility for creating a direct encoding of a HTTPSig message.
%%
%% This function supports two modes of operation:
%% 1. `format: binary`, yielding a raw binary HTTP/1.1-style response that can
%%    either be stored or emitted raw accross a transport medium.
%% 2. `format: components`, yielding a message containing `headers` and `body`
%%    keys, suitable for use in connecting to HTTP-response flows implemented
%%    by other servers.
%%
%% Optionally, the `index` key can be set to override resolution of the default
%% index page into HTTP responses that do not contain their own `body` field.
serialize(Msg, Opts) -> serialize(Msg, #{}, Opts).


serialize(Msg, #{<<"format">> := <<"components">>}, Opts) ->
    % Convert to HTTPSig via TABM through calling `hb_message:convert` rather
    % than executing `to/3` directly. This ensures that our responses are
    % normalized.
    {ok, EncMsg} = hb_message:convert(Msg, <<"httpsig@1.0">>, Opts),
    {ok,
     #{
       <<"body">> => hb_maps:get(<<"body">>, EncMsg, <<>>),
       <<"headers">> => hb_maps:without([<<"body">>], EncMsg)
      }};
serialize(Msg, _Req, Opts) ->
    % We assume the default format of `binary` if none of the prior clauses
    % match.
    HTTPSig = hb_message:convert(Msg, <<"httpsig@1.0">>, Opts),
    {ok, dev_codec_httpsig_conv:encode_http_msg(HTTPSig, Opts)}.


verify(Base, Req, RawOpts) ->
    % A rsa-pss-sha512 commitment is verified by regenerating the signature
    % base and validating against the signature.
    Opts = opts(RawOpts),
    {ok, EncMsg, EncComm, _} = normalize_for_encoding(Base, Req, Opts),
    SigBase = signature_base(EncMsg, EncComm, Opts),
    ?event({signature_base_verify, {string, SigBase}}),
    KeyID = maps:get(<<"keyid">>, Req),
    RawSignature = hb_util:decode(Signature = maps:get(<<"signature">>, Req)),
    case maps:get(<<"type">>, Req) of
        <<"rsa-pss-sha512">> ->
            ?event(httpsig_verify, {verify, {rsa_pss_sha512, {sig_base, SigBase}}}),
            {ok,
             ar_wallet:verify(
               {{rsa, 65537}, base64:decode(KeyID)},
               SigBase,
               RawSignature,
               sha512)};
        <<"hmac-sha256">> ->
            ActualHMac =
                hb_util:human_id(
                  crypto:mac(hmac, sha256, KeyID, SigBase)),
            ?event(httpsig_verify,
                   {verify,
                    {hmac_sha256,
                     {sig_base, SigBase},
                     {actual_hmac, {string, ActualHMac}},
                     {signature, {string, Signature}},
                     {matches, Signature =:= ActualHMac}}}),
            {ok, Signature =:= ActualHMac};
        UnsupportedType ->
            {error, <<"Unsupported commitment type: ", UnsupportedType/binary>>}
    end.


%% @doc Commit to a message using the HTTP-Signature format. We use the `type'
%% parameter to determine the type of commitment to use. If the `type' parameter
%% is `signed', we default to the rsa-pss-sha512 algorithm. If the `type'
%% parameter is `unsigned', we default to the hmac-sha256 algorithm.
commit(Msg, Req = #{<<"type">> := <<"unsigned">>}, Opts) ->
    commit(Msg, Req#{<<"type">> => <<"hmac-sha256">>}, Opts);
commit(Msg, Req = #{<<"type">> := <<"signed">>}, Opts) ->
    commit(Msg, Req#{<<"type">> => <<"rsa-pss-sha512">>}, Opts);
commit(MsgToSign, Req = #{<<"type">> := <<"rsa-pss-sha512">>}, RawOpts) ->
    ?event(
      {generating_rsa_pss_sha512_commitment, {msg, MsgToSign}, {req, Req}}),
    Opts = opts(RawOpts),
    Wallet = hb_opts:get(priv_wallet, no_viable_wallet, Opts),
    if
        Wallet =:= no_viable_wallet ->
            throw({cannot_commit, no_viable_wallet, MsgToSign});
        true ->
            ok
    end,
    % Utilize the hashpath, if present, as the tag for the commitment.
    MaybeTagMap =
        case MsgToSign of
            #{<<"priv">> := #{<<"hashpath">> := HP}} -> #{<<"tag">> => HP};
            _ -> #{}
        end,
    % Generate the unsigned commitment and signature base.
    ToCommit = hb_ao:normalize_keys(keys_to_commit(MsgToSign, Req, Opts)),
    ?event({to_commit, ToCommit}),
    UnsignedCommitment =
        maybe_bundle_tag_commitment(
          MaybeTagMap#{
            <<"commitment-device">> => <<"httpsig@1.0">>,
            <<"type">> => <<"rsa-pss-sha512">>,
            <<"keyid">> => base64:encode(ar_wallet:to_pubkey(Wallet)),
            <<"committer">> =>
                hb_util:human_id(ar_wallet:to_address(Wallet)),
            <<"committed">> => ToCommit
           },
          Req,
          Opts),
    {ok, EncMsg, EncComm, ModCommittedKeys} =
        normalize_for_encoding(MsgToSign, UnsignedCommitment, Opts),
    ?event({encoded_to_httpsig_for_commitment, MsgToSign}),
    % Generate the signature base
    SignatureBase = signature_base(EncMsg, EncComm, Opts),
    ?event({rsa_signature_base, {string, SignatureBase}}),
    ?event({mod_committed_keys, ModCommittedKeys}),
    % Sign the signature base
    Signature = ar_wallet:sign(Wallet, SignatureBase, sha512),
    % Generate the ID of the signature
    ID = hb_util:human_id(crypto:hash(sha256, Signature)),
    ?event({rsa_commit, {committed, ToCommit}}),
    % Calculate the ID and place the signature into the `commitments' key of the
    % message. After, we call `commit' again to add the hmac to the new
    % message.
    commit(
      MsgToSign#{
        <<"commitments">> =>
            (maps:get(<<"commitments">>, MsgToSign, #{}))#{
              ID =>
                  UnsignedCommitment#{
                    <<"signature">> => hb_util:encode(Signature),
                    <<"committed">> => ModCommittedKeys
                   }
             }
       },
      Req#{<<"type">> => <<"hmac-sha256">>},
      Opts);
commit(Msg, Req = #{<<"type">> := <<"hmac-sha256">>}, RawOpts) ->
    % Find the ID of the message without hmac commitments, then add the hmac
    % using the set of all presently committed keys as the input. If no (other)
    % commitments are present, then use all keys from the encoded message.
    Opts = opts(RawOpts),
    WithoutHmac =
        hb_message:without_commitments(
          #{
            <<"commitment-device">> => <<"httpsig@1.0">>,
            <<"type">> => <<"hmac-sha256">>
           },
          Msg,
          Opts),
    % Merge together the bundle and tag maps.
    % Extract the base commitments from the message.
    Commitments = maps:get(<<"commitments">>, WithoutHmac, #{}),
    CommittedKeys = keys_to_commit(Msg, Req, Opts),
    UnauthedCommitment =
        maybe_bundle_tag_commitment(
          #{
            <<"commitment-device">> => <<"httpsig@1.0">>,
            <<"type">> => <<"hmac-sha256">>,
            <<"keyid">> => <<"ao">>,
            <<"committed">> => hb_ao:normalize_keys(CommittedKeys)
           },
          Req,
          Opts),
    {ok, EncMsg, EncComm, ModCommittedKeys} =
        normalize_for_encoding(Msg, UnauthedCommitment, Opts),
    SigBase = signature_base(EncMsg, EncComm, Opts),
    HMac = hb_util:human_id(crypto:mac(hmac, sha256, <<"ao">>, SigBase)),
    ?event(httpsig_commit,
           {hmac_commit,
            {type, <<"hmac-sha256">>},
            {committed, CommittedKeys},
            {sig_base, SigBase},
            {hmac, HMac}}),
    Res =
        {ok,
         Msg#{
           <<"commitments">> =>
               Commitments#{
                 HMac =>
                     UnauthedCommitment#{
                       <<"signature">> => HMac,
                       <<"committed">> => ModCommittedKeys
                      }
                }
          }},
    ?event({hmac_generation_complete, Res}),
    Res.


%% @doc Annotate the commitment with the `bundle' key if the request contains
%% it.
maybe_bundle_tag_commitment(Commitment, Req, _Opts) ->
    case hb_util:atom(maps:get(<<"bundle">>, Req, false)) of
        true -> Commitment#{<<"bundle">> => <<"true">>};
        false -> Commitment
    end.


%% @doc Derive the set of keys to commit to from a `commit` request and a
%% base message.
keys_to_commit(_Base, #{<<"committed">> := Explicit}, _Opts) ->
    % Case 1: Explicitly provided keys to commit.
    % Add `+link` specifiers to the user given list as necessary, in order for
    % their given keys to match the HTTPSig encoded TABM form.
    hb_util:list_to_numbered_message(Explicit);
keys_to_commit(Base, _Req, Opts) ->
    % Extract the set of committed keys from the message.
    case hb_message:committed(Base, #{<<"committers">> => <<"all">>}, opts(Opts)) of
        [] ->
            % Case 3: Default to all keys in the TABM-encoded message, aside
            % metadata.
            hb_util:list_to_numbered_message(
              lists:map(
                fun hb_link:remove_link_specifier/1,
                hb_util:to_sorted_keys(Base, Opts) --
                [<<"commitments">>, <<"priv">>]));
        Keys ->
            % Case 2: Replicate the raw keys that the existing commitments have
            % used. This leads to a message whose commitments can be 'stacked'
            % and represented together in HTTPSig format.
            hb_util:list_to_numbered_message(Keys)
    end.


%% @doc If the `body' key is present and a binary, replace it with a
%% content-digest.
add_content_digest(Msg, _Opts) ->
    case maps:get(<<"body">>, Msg, not_found) of
        Body when is_binary(Body) ->
            % Remove the body from the message and add the content-digest,
            % encoded as a structured field.
            (maps:without([<<"body">>], Msg))#{
              <<"content-digest">> =>
                  hb_util:bin(hb_structured_fields:dictionary(
                                #{
                                  <<"sha-256">> =>
                                      {item, {binary, hb_crypto:sha256(Body)}, []}
                                 }))
             };
        _ -> Msg
    end.


%% @doc Given a base message and a commitment, derive the message and commitment
%% normalized for encoding.
normalize_for_encoding(Msg, Commitment, Opts) ->
    % Extract the requested keys to include in the signature base.
    RawInputs =
        hb_util:message_to_ordered_list(
          maps:get(<<"committed">>, Commitment, []),
          Opts),
    % Normalize the keys to their maybe-linked form, adding `+link` if necessary.
    Inputs =
        lists:map(
          fun(Key) ->
                  NormalizedKey = hb_ao:normalize_key(Key),
                  case maps:is_key(NormalizedKey, Msg) of
                      true -> NormalizedKey;
                      false ->
                          case maps:is_key(<<NormalizedKey/binary, "+link">>, Msg) of
                              true -> <<NormalizedKey/binary, "+link">>;
                              false -> NormalizedKey
                          end
                  end
          end,
          RawInputs),
    ?event({inputs, {list, Inputs}}),
    % Filter the message down to only the requested keys, then encode it.
    MsgWithOnlyInputs = maps:with(Inputs, Msg),
    ?event(find_lru, {msg_with_only_inputs, maps:without([<<"commitments">>], MsgWithOnlyInputs)}),
    {ok, EncodedWithSigInfo} =
        to(
          maps:without([<<"commitments">>], MsgWithOnlyInputs),
          #{
            <<"bundle">> =>
                hb_util:atom(maps:get(<<"bundle">>, Commitment, false))
           },
          Opts),
    % Remove the signature and signature-input keys from the encoded message,
    % convert the `body' key to a `content-digest' key, if present.
    Encoded = add_content_digest(EncodedWithSigInfo, Opts),
    % Transform the list of requested keys to their `httpsig@1.0' equivalents.
    EncodedKeys = maps:keys(Encoded),
    EncodedKeysWithBodyKey =
        case hb_maps:get(<<"ao-body-key">>, EncodedWithSigInfo, not_found) of
            not_found ->
                EncodedKeys;
            AOBodyKey ->
                hb_util:list_replace(
                  EncodedKeys,
                  AOBodyKey,
                  [<<"body">>, <<"ao-body-key">>])
        end,
    % The keys to be used in encodings of the message:
    KeysForEncoding =
        hb_util:list_replace(
          EncodedKeysWithBodyKey,
          <<"body">>,
          <<"content-digest">>),
    % Calculate the keys that have been removed from the message, as a result
    % of being added to the body. These keys will need to be removed from the
    % `committed' list and re-added where the `content-digest' was.
    BodyKeys =
        lists:filter(
          fun(Key) -> not key_present(Key, Encoded) end,
          RawInputs),
    KeysForCommitment =
        dev_codec_httpsig_siginfo:from_siginfo_keys(
          EncodedWithSigInfo,
          BodyKeys,
          KeysForEncoding),
    ?event(debug_httpsig,
           {normalized_for_encoding,
            {raw_inputs, Inputs},
            {inputs_for_encoding, KeysForEncoding},
            {final_for_commitment_message, KeysForCommitment},
            {encoded_message, Encoded}}),
    {ok,
     Encoded,
     Commitment#{<<"committed">> => KeysForEncoding},
     KeysForCommitment}.


%% @doc Calculate if a key or its `+link' TABM variant is present in a message.
key_present(Key, Msg) ->
    NormalizedKey = hb_ao:normalize_key(Key),
    maps:is_key(NormalizedKey, Msg) orelse
    maps:is_key(<<NormalizedKey/binary, "+link">>, Msg).


%% @doc create the signature base that will be signed in order to create the
%% Signature and SignatureInput.
%%
%% This implements a portion of RFC-9421 see:
%% https://datatracker.ietf.org/doc/html/rfc9421#name-creating-the-signature-base
signature_base(EncodedMsg, Commitment, Opts) ->
    ComponentsLines =
        signature_components_line(
          EncodedMsg,
          Commitment,
          Opts),
    ?event({component_identifiers_for_sig_base, ComponentsLines}),
    ParamsLine = signature_params_line(Commitment, Opts),
    SignatureBase =
        <<ComponentsLines/binary, "\n",
          "\"@signature-params\": ", ParamsLine/binary>>,
    ?event(signature_base, {signature_base, {string, SignatureBase}}),
    SignatureBase.


%% @doc Given a list of Component Identifiers and a Request/Response Message
%% context, create the "signature-base-line" portion of the signature base
%% TODO: catch duplicate identifier:
%% https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.2.5.2.1
%%
%% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.1
signature_components_line(Req, Commitment, _Opts) ->
    ComponentsLines =
        lists:map(
          fun(Name) ->
                  case maps:get(Name, Req, not_found) of
                      not_found ->
                          throw(
                            {missing_key_for_signature_component_line,
                             Name,
                             {message, Req},
                             {commitment, Commitment}});
                      Value ->
                          <<<<"\"">>/binary, Name/binary, <<"\"">>/binary, <<": ">>/binary, Value/binary>>
                  end
          end,
          maps:get(<<"committed">>, Commitment)),
    iolist_to_binary(lists:join(<<"\n">>, ComponentsLines)).


%% @doc construct the "signature-params-line" part of the signature base.
%%
%% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.3.2.4
signature_params_line(RawCommitment, Opts) ->
    Commitment =
        maps:without(
          [<<"signature">>, <<"signature-input">>],
          RawCommitment),
    ?event(debug_enc, {signature_params_line, {commitment, Commitment}}),
    hb_util:bin(
      hb_structured_fields:list(
        [{list,
          lists:map(
            fun(Key) -> {item, {string, Key}, []} end,
            dev_codec_httpsig_siginfo:add_derived_specifiers(
              hb_util:message_to_ordered_list(
                maps:get(<<"committed">>, Commitment),
                Opts))),
          lists:map(
            fun({<<"alg">>, Param}) when is_binary(Param) ->
                    {<<"alg">>, {string, Param}};
               ({Name, Param}) when is_binary(Param) ->
                    {Name, {string, Param}};
               ({Name, Param}) when is_integer(Param) ->
                    {Name, Param}
            end,
            lists:sort(maps:to_list(
                         maps:with(
                           [<<"created">>,
                            <<"expires">>,
                            <<"nonce">>,
                            <<"alg">>,
                            <<"keyid">>,
                            <<"tag">>,
                            <<"bundle">>],
                           Commitment#{<<"alg">> => maps:get(<<"type">>, Commitment)}))))}])).


%%%
%%% TESTS
%%%

%%% Integration Tests


%% @doc Ensure that we can validate a signature on an extremely large and complex
%% message that is sent over HTTP, signed with the codec.
validate_large_message_from_http_test() ->
    Node = hb_http_server:start_node(Opts = #{
                                              force_signed => true,
                                              commitment_device => <<"httpsig@1.0">>,
                                              extra =>
                                                  [ [ [ #{
                                                          <<"n">> => N,
                                                          <<"m">> => M,
                                                          <<"o">> => O
                                                         }
                                                        || O <- lists:seq(1, 3) ]
                                                      || M <- lists:seq(1, 3) ]
                                                    || N <- lists:seq(1, 3) ]
                                             }),
    {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, Opts),
    Signers = hb_message:signers(Res, Opts),
    ?event({received, {signers, Signers}, {res, Res}}),
    ?assert(length(Signers) == 1),
    ?assert(hb_message:verify(Res, Signers, Opts)),
    ?event({sig_verifies, Signers}),
    ?assert(hb_message:verify(Res, all, Opts)),
    ?event({hmac_verifies, <<"hmac-sha256">>}),
    {ok, OnlyCommitted} = hb_message:with_only_committed(Res, Opts),
    ?event({msg_with_only_committed, OnlyCommitted}),
    ?assert(hb_message:verify(OnlyCommitted, Signers, Opts)),
    ?event({msg_with_only_committed_verifies, Signers}),
    ?assert(hb_message:verify(OnlyCommitted, all, Opts)),
    ?event({msg_with_only_committed_verifies_hmac, <<"hmac-sha256">>}).


committed_id_test() ->
    Msg = #{<<"basic">> => <<"value">>},
    Signed = hb_message:commit(Msg, hb:wallet()),
    ?event({signed_msg, Signed}),
    UnsignedID = hb_message:id(Signed, none),
    SignedID = hb_message:id(Signed, all),
    ?event({ids, {unsigned_id, UnsignedID}, {signed_id, SignedID}}),
    ?assertNotEqual(UnsignedID, SignedID).


multicommitted_id_test() ->
    Msg = #{<<"basic">> => <<"value">>},
    Signed1 = hb_message:commit(Msg, Wallet1 = ar_wallet:new()),
    Signed2 = hb_message:commit(Signed1, Wallet2 = ar_wallet:new()),
    Addr1 = hb_util:human_id(ar_wallet:to_address(Wallet1)),
    Addr2 = hb_util:human_id(ar_wallet:to_address(Wallet2)),
    ?event({signed_msg, Signed2}),
    UnsignedID = hb_message:id(Signed2, none),
    SignedID = hb_message:id(Signed2, all),
    ?event({ids, {unsigned_id, UnsignedID}, {signed_id, SignedID}}),
    ?assertNotEqual(UnsignedID, SignedID),
    ?assert(hb_message:verify(Signed2, [])),
    ?assert(hb_message:verify(Signed2, [Addr1])),
    ?assert(hb_message:verify(Signed2, [Addr2])),
    ?assert(hb_message:verify(Signed2, [Addr1, Addr2])),
    ?assert(hb_message:verify(Signed2, [Addr2, Addr1])),
    ?assert(hb_message:verify(Signed2, all)).


%% @doc Test that we can sign and verify a message with a link. We use
sign_and_verify_link_test() ->
    Msg = #{
            <<"normal">> => <<"typical-value">>,
            <<"untyped">> => #{<<"inner-untyped">> => <<"inner-value">>},
            <<"typed">> => #{<<"inner-typed">> => 123}
           },
    NormMsg = hb_message:convert(Msg, <<"structured@1.0">>, #{}),
    ?event({msg, NormMsg}),
    Signed = hb_message:commit(NormMsg, hb:wallet()),
    ?event({signed_msg, Signed}),
    ?assert(hb_message:verify(Signed)).
