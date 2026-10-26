%%% @doc This module implements HTTP Message Signatures as described in RFC-9421
%%% (https://datatracker.ietf.org/doc/html/rfc9421), as an AO-Core device.
%%% It implements the codec standard (from/1, to/1), as well as the optional
%%% commitment functions (id/3, sign/3, verify/3). The commitment functions
%%% are found in this module, while the codec functions are relayed to the 
%%% `dev_httpsig_conv' module.
-module(dev_httpsig).
-specification("../../../specs/httpsig@1.0.md").
-device_libraries([lib_arweave_common, lib_secret]).
%%% Codec API functions
-export([to/3, to_hint/3, from/3]).
%%% Uni-directional codec support (_to_ binary/header+body components), but not 
%%% back.
-export([serialize/2, serialize/3]).
%%% Commitment API functions
-export([commit/3, verify/3]).
%%% Public API functions
-export([add_content_digest/2, normalize_for_encoding/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Routing functions for the `dev_httpsig_conv' module
-spec to(#{ _ => _ }, #{ _ => _ }, map()) -> term().
to(Msg, Req, Opts) -> dev_httpsig_conv:to(Msg, Req, Opts).
-spec from(#{ _ => _ }, #{ _ => _ }, map()) -> term().
from(Msg, Req, Opts) -> dev_httpsig_conv:from(Msg, Req, Opts).

%% @doc Generate the `Opts' to use during AO-Core operations in the codec.
opts(RawOpts) ->
    RawOpts#{
        <<"hashpath">> => ignore,
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
        <<"force-message">> => false
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
-spec serialize(
    #{ _ => _ },
    #{ format => binary(), index => binary(), _ => _ },
    #{ _ => _ }
) -> {ok, binary() | #{ headers := #{ _ => _ }, body := _, _ => _ }}.
serialize(Msg, #{ <<"format">> := <<"components">> }, Opts) ->
    % Convert to HTTPSig via TABM through calling `hb_message:convert` rather
    % than executing `to/3` directly. This ensures that our responses are 
    % normalized.
    EncMsg = hb_message:convert(Msg, <<"httpsig@1.0">>, Opts),
    {ok,
        #{
            <<"body">> => hb_maps:get(<<"body">>, EncMsg, <<>>),
            <<"headers">> => hb_maps:without([<<"body">>], EncMsg)
        }
    };
serialize(Msg, _Req, Opts) ->
    % We assume the default format of `binary` if none of the prior clauses
    % match.
    {ok, dev_httpsig_conv:encode_http_msg(Msg, Opts) }.

-spec verify(
    #{ _ => _ },
    #{ signature := binary(), type := binary(), _ => _ },
    #{ _ => _ }
) -> {ok, boolean()} | {failure, _}.
verify(Base, Req, RawOpts) ->
    Opts = opts(RawOpts),
    % Verify the commitment's ID as well as the presence of its committed keys.
    maybe
        [] ?= missing_keys(Base, Req, Opts),
        ID = dev_httpsig_siginfo:derived_commitment_id(
            hb_util:decode(maps:get(<<"signature">>, Req))),
        true ?= maps:is_key(ID, maps:get(<<"commitments">>, Base, #{})),
        do_verify(Base, Req, Opts)
    else
        Failure ->
            ?event(httpsig_verify, {verify, {invalid_commitment, Failure}}),
            {ok, false}
    end.

%% @doc Verify a commitment whose committed keys the base carries. We
%% regenerate the signature base, and the committer that the `keyid' gives
%% verifies the signature against it.
do_verify(Base, Req, Opts) ->
    {ok, EncMsg, EncComm, _} = normalize_for_encoding(Base, Req, Opts),
    SigBase = signature_base(EncMsg, EncComm, Opts),
    KeyID = maps:get(<<"keyid">>, Req, ?UNSIGNED_COMMITTER),
    ?event_debug(debug_httpsig,
        {
            httpsig_verifying,
            {keyid, KeyID},
            {req, Req},
            {signature_base, {string, SigBase}}
        }
    ),
    {
        ok,
        maps:get(<<"committer">>, Req, undefined) =:=
            dev_httpsig_siginfo:keyid_to_committer(KeyID)
        andalso strong_key(Req)
        andalso committer_verifies(KeyID, SigBase, Req, Opts)
    }.

%% @doc Check that a commitment uses one of this device's algorithms. An
%% `rsa-pss-sha512' commitment must also have a `public-key' of at least 2048
%% bits.
strong_key(#{ <<"type">> := <<"rsa-pss-sha512">>, <<"public-key">> := Key }) ->
    binary:decode_unsigned(hb_util:decode(Key)) >= (1 bsl 2047);
strong_key(#{ <<"type">> := <<"hmac-sha256">> }) ->
    true;
strong_key(_Req) ->
    false.

%% @doc Load the committer from a `keyid' and ask it to verify the signature of
%% a signature base. The request is the commitment. Its `priv' message is given
%% to the committer, as it contains the verifier's key material, such as a
%% secret. The `keyid' must be the URI of the committer. A `keyid' in any
%% other form does not verify.
committer_verifies(KeyID, SigBase, Req, Opts) ->
    case committer(#{ <<"committer">> => KeyID }, Opts) of
        {ok, Committer, KeyID} ->
            % The signature and the public key are base64url encoded in a
            % commitment, so we decode them before verification. The
            % committer checks the public key as the `owner' of the signature.
            Raw =
                case Req of
                    #{ <<"public-key">> := Key } ->
                        #{ <<"owner">> => hb_util:decode(Key) };
                    _ -> #{}
                end,
            % A committer refuses a commitment that keeps its device.
            Commitment = maps:remove(<<"commitment-device">>, Req),
            {ok, Verified} =
                hb_ao:raw(
                    hb_private:set_priv(Committer, hb_private:from_message(Req)),
                    (maps:merge(Commitment, Raw))#{
                        <<"path">> => <<"verify">>,
                        <<"body">> => SigBase,
                        <<"signature">> =>
                            hb_util:decode(maps:get(<<"signature">>, Req))
                    },
                    Opts
                ),
            Verified;
        _ -> false
    end.

%% @doc Find the committer of a request, returning it as a message along with
%% its URI. The request's `committer' may be a message or a URI. We load a
%% committer that is given as a URI, and return the committer's own URI, so
%% that each committer has one URI.
%% We never load the committer of unsigned commitments by its URI, because
%% loading a URI makes an unsigned commitment to its inputs, which would call
%% this function again.
committer(#{ <<"committer">> := ?UNSIGNED_COMMITTER }, _Opts) ->
    {ok, Scheme, Rest} = hb_uri:scheme(?UNSIGNED_COMMITTER),
    {
        ok,
        #{ <<"device">> => hb_uri:device(Scheme), <<"body">> => Rest },
        ?UNSIGNED_COMMITTER
    };
committer(#{ <<"committer">> := URI }, Opts) when is_binary(URI) ->
    maybe
        {ok, Committer} ?= hb_uri:from(URI, Opts),
        committer(#{ <<"committer">> => Committer }, Opts)
    end;
committer(#{ <<"committer">> := Committer }, Opts) ->
    case hb_committer:uri(Committer, Opts) of
        URI when is_binary(URI) -> {ok, Committer, URI};
        _ -> {error, not_found}
    end;
committer(_Req, _Opts) ->
    {error, not_found}.

%% @doc Commit to a message using the HTTP-Signature format. We use the `type'
%% parameter to determine the type of commitment to use. If the `type' parameter
%% is `signed', we default to the rsa-pss-sha512 algorithm. If the `type'
%% parameter is `unsigned', we default to the hmac-sha256 algorithm.
-spec commit(
    #{ _ => _ },
    #{
        type := binary(),
        bundle => boolean(),
        committed => [_] | #{ _ => _ },
        _ => _
    },
    #{ _ => _ }
) -> {ok, #{ _ => _ }}.
commit(Msg, Req = #{ <<"type">> := <<"unsigned">> }, Opts) ->
    commit(Msg, Req#{ <<"type">> => <<"hmac-sha256">> }, Opts);
commit(Msg, Req = #{ <<"type">> := <<"signed">> }, Opts) ->
    commit(Msg, Req#{ <<"type">> => <<"rsa-pss-sha512">> }, Opts);
commit(MsgToSign, Req = #{ <<"type">> := <<"rsa-pss-sha512">> }, RawOpts) ->
    ?event(
        {generating_rsa_pss_sha512_commitment, {msg, MsgToSign}, {req, Req}}
    ),
    Opts = opts(RawOpts),
    % Find the committer of the request. Its public key is the `public-key'
    % parameter of the signature.
    {CommitterMsg, KeyID} =
        case committer(Req, Opts) of
            {ok, Committer, URI} -> {Committer, URI};
            Error -> throw({cannot_commit, Error, MsgToSign})
        end,
    Owner = hb_maps:get(<<"owner">>, CommitterMsg, <<>>, Opts),
    % Utilize the hashpath, if present, as the tag for the commitment.
    MaybeTagMap =
        case MsgToSign of
            #{ <<"priv">> := #{ <<"hashpath">> := HP }} -> #{ <<"tag">> => HP };
            _ -> #{}
        end,
    % Generate the unsigned commitment and signature base.
    ToCommit = hb_ao:normalize_keys(keys_to_commit(MsgToSign, Req, Opts)),
    ?event_debug({to_commit, ToCommit}),
    UnsignedCommitment =
        maybe_bundle_tag_commitment(
            MaybeTagMap#{
                <<"commitment-device">> => <<"httpsig@1.0">>,
                <<"type">> => <<"rsa-pss-sha512">>,
                <<"keyid">> => KeyID,
                <<"committer">> => KeyID,
                <<"public-key">> => hb_util:encode(Owner),
                <<"committed">> => ToCommit
            },
            MsgToSign,
            Req,
            Opts
        ),
    {ok, EncMsg, EncComm, ModCommittedKeys} =
        normalize_for_encoding(
            MsgToSign,
            UnsignedCommitment,
            is_map_key(<<"committed">>, Req),
            Opts
        ),
    ?event_debug({encoded_to_httpsig_for_commitment, MsgToSign}),
    % Generate the signature base
    SignatureBase = signature_base(EncMsg, EncComm, Opts),
    ?event_debug({rsa_signature_base, {string, SignatureBase}}),
    ?event_debug({mod_committed_keys, ModCommittedKeys}),
    % Sign the signature base with the committer.
    CommitRes =
        hb_ao:raw(
            CommitterMsg,
            #{
                <<"path">> => <<"commit">>,
                <<"body">> => SignatureBase,
                <<"type">> => <<"rsa-pss-sha512">>
            },
            Opts
        ),
    Signature =
        case CommitRes of
            {ok, #{ <<"signature">> := Sig, <<"owner">> := Owner }} -> Sig;
            CommitError -> throw({cannot_commit, CommitError, MsgToSign})
        end,
    % Generate the ID of the signature
    ID = hb_util:human_id(crypto:hash(sha256, Signature)),
    ?event_debug({rsa_commit, {committed, ToCommit}}),
    % Calculate the ID and place the signature into the `commitments' key of the
    % message. After, we call `commit' again to add the hmac to the new
    % message.
    {
        ok,
        MsgToSign#{
            <<"commitments">> =>
                (maps:get(<<"commitments">>, MsgToSign, #{}))#{
                    ID =>
                        UnsignedCommitment#{
                            <<"signature">> => hb_util:encode(Signature),
                            <<"committed">> => ModCommittedKeys
                        }
                }
        }
    };
commit(BaseMsg, Req = #{ <<"type">> := <<"hmac-sha256">> }, RawOpts) ->
    % Find the committer of the request and its URI. If the
    % request gives no committer, we use the committer of unsigned commitments.
    Opts = opts(RawOpts),
    {ok, CommitterMsg, KeyID} =
        committer(
            maps:merge(#{ <<"committer">> => ?UNSIGNED_COMMITTER }, Req),
            Opts
        ),
    Committer = dev_httpsig_siginfo:keyid_to_committer(KeyID),
    % Remove any existing hmac commitments with the given keyid before adding
    % the new one.
    Msg =
        hb_message:without_commitments(
            #{
                <<"commitment-device">> => <<"httpsig@1.0">>,
                <<"type">> => <<"hmac-sha256">>,
                <<"keyid">> => KeyID
            },
            BaseMsg,
            Opts
        ),
    % Extract the base commitments from the message.
    Commitments = maps:get(<<"commitments">>, Msg, #{}),
    CommittedKeys = keys_to_commit(Msg, Req, Opts),
    % Create the commitment with the appropriate keyid, committed keys, and 
    % bundle specifier.
    CommitmentWithoutCommitter = #{
        <<"commitment-device">> => <<"httpsig@1.0">>,
        <<"type">> => <<"hmac-sha256">>,
        <<"keyid">> => KeyID,
        <<"committed">> => hb_ao:normalize_keys(CommittedKeys)
    },
    % If the committer is undefined, we do not need to add the `committer' key.
    BaseCommitment =
        if Committer =:= undefined -> CommitmentWithoutCommitter;
        true -> CommitmentWithoutCommitter#{ <<"committer">> => Committer }
        end,
    UnauthedCommitment =
        maybe_bundle_tag_commitment(
            BaseCommitment,
            BaseMsg,
            Req,
            Opts
        ),
    {ok, EncMsg, EncComm, ModCommittedKeys} =
        normalize_for_encoding(
            Msg,
            UnauthedCommitment,
            is_map_key(<<"committed">>, Req),
            Opts
        ),
    SigBase = signature_base(EncMsg, EncComm, Opts),    
    {ok, #{ <<"signature">> := Signature }} =
        hb_ao:raw(
            CommitterMsg,
            #{ <<"path">> => <<"commit">>, <<"body">> => SigBase },
            Opts
        ),
    HMac = hb_util:human_id(Signature),
    ?event_debug(
        debug_commitments,
        {hmac_commit,
            {type, <<"hmac-sha256">>},
            {keyid, KeyID},
            {committer, Committer},
            {committed, CommittedKeys},
            {mod_committed_keys, ModCommittedKeys},
            {sig_base, SigBase},
            {hmac, HMac}
        }
    ),
    Res =
        {
            ok,
            Msg#{
                <<"commitments">> =>
                    Commitments#{
                        HMac =>
                            UnauthedCommitment#{
                                <<"signature">> => HMac,
                                <<"committed">> => ModCommittedKeys
                            }
                    }
            }
        },
    ?event_debug(debug_commitments, {hmac_generation_complete, Res}),
    Res.

%% @doc Annotate the commitment with the `bundle' key if the message is bundled.
%% `to_hint/3' gives its bundle state, from the request or the message's
%% `httpsig@1.0' commitment, as it does for the encoding of the message.
maybe_bundle_tag_commitment(Commitment, Msg, Req, Opts) ->
    {ok, HintedReq} = to_hint(Msg, Req, Opts),
    case hb_util:atom(maps:get(<<"bundle">>, HintedReq, false)) of
        true -> Commitment#{ <<"bundle">> => <<"true">> };
        false -> Commitment
    end.

%% @doc Apply the bundle state of an HTTPSig commitment to a conversion.
to_hint(Msg, Req, Opts) ->
    case lib_arweave_common:bundle_hint(<<"httpsig@1.0">>, Msg, Req, Opts) of
        not_found -> {ok, Req};
        Hint -> Hint
    end.

%% @doc Derive the set of keys to commit to from a `commit` request and a 
%% base message: the keys that the request lists, or every key of the message.
keys_to_commit(_Base, #{ <<"committed">> := Explicit}, _Opts) ->
    hb_util:list_to_numbered_message(Explicit);
keys_to_commit(Base, _Req, Opts) ->
    default_keys_to_commit(Base, Opts).

default_keys_to_commit(Base, Opts) ->
    hb_util:list_to_numbered_message(
        hb_util:to_sorted_keys(Base, Opts) -- [<<"commitments">>, <<"priv">>]
    ).

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
                        }
                    ))
            };
        _ -> Msg
    end.

%% @doc Given a base message and a commitment, derive the message and commitment
%% normalized for encoding. The components of the signature follow the order
%% of the keys that the commitment lists. When `Ordered' is false, as for a
%% commitment made without a `committed' list in its request, they follow the
%% order of the keys of the encoded message. Every key of the message is
%% encoded: the message contains only the keys of the commitment.
-spec normalize_for_encoding(
    #{ _ => _ },
    #{ committed => [_], _ => _ },
    #{ _ => _ }
) -> {ok, #{ _ => _ }, #{ committed := [_], _ => _ }, [_]}.
normalize_for_encoding(Msg, Commitment, Opts) ->
    normalize_for_encoding(Msg, Commitment, true, Opts).
normalize_for_encoding(Msg, Commitment, Ordered, Opts) ->
    % Extract the requested keys to include in the signature base.
    RawInputs = committed_keys(Commitment, Opts),
    % A commitment is over every key it lists. A committed key that the
    % message lacks cannot be left out of the signature base silently: the
    % commitment would then be encoded over a message that its signature does
    % not verify.
    case missing_keys(Msg, Commitment, Opts) of
        [] -> ok;
        Missing ->
            throw(
                {committed_key_missing,
                    {keys, Missing},
                    {commitment, Commitment},
                    {msg, Msg}
                }
            )
    end,
    % Encode the message without its commitments.
    {ok, EncodedWithSigInfo} =
        to(
            maps:without([<<"commitments">>], Msg),
            #{
                <<"bundle">> =>
                    hb_util:atom(maps:get(<<"bundle">>, Commitment, false))
            },
            Opts
        ),
    % `to/3' adds the `content-digest' key of a binary body, so remove the
    % `body' key from the encoded message.
    Encoded = maps:without([<<"body">>], EncodedWithSigInfo),
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
                    [<<"body">>, <<"ao-body-key">>]
                )
        end,
    % The keys to be used in encodings of the message. A key that is an RFC
    % 9421 derived component, such as `path', is encoded as that component,
    % `@path':
    KeysForEncoding =
        dev_httpsig_siginfo:add_derived_specifiers(
            hb_util:list_replace(
                EncodedKeysWithBodyKey,
                <<"body">>,
                <<"content-digest">>
            )
        ),
    % Calculate the keys that have been removed from the message, as a result
    % of being added to the body. These keys will need to be removed from the
    % `committed' list and re-added where the `content-digest' was in the
    % `from_siginfo_keys' call. A `content-digest' or `ao-body-key' key of the
    % message is always in the body: the headers `content-digest' and
    % `ao-body-key' give the digest and the key of the body.
    BodyKeys =
        lists:filter(
            fun(Key) ->
                Key =:= <<"content-digest">> orelse
                    Key =:= <<"ao-body-key">> orelse
                    not key_present(Key, Encoded) orelse
                    (Key =:= <<"content-type">> andalso
                        maps:get(Key, Msg, undefined) =/=
                            maps:get(Key, Encoded, undefined))
            end,
            RawInputs
        ),
    Components =
        case Ordered of
            true -> order_components(KeysForEncoding, RawInputs, BodyKeys);
            false -> KeysForEncoding
        end,
    KeysForCommitment =
        dev_httpsig_siginfo:from_siginfo_keys(
            EncodedWithSigInfo,
            BodyKeys,
            Components
        ),
    ?event_debug(debug_httpsig,
        {normalized_for_encoding,
            {raw_inputs, RawInputs},
            {inputs_for_encoding, Components},
            {final_for_commitment_message, KeysForCommitment},
            {encoded_message, Encoded}
        }
    ),
    {
        ok,
        Encoded,
        Commitment#{ <<"committed">> => Components },
        KeysForCommitment
    }.

%% @doc Order the components of a signature as a commitment lists its keys. The
%% keys of the body are covered together by `content-digest', at the place of
%% the first of them. A component that the commitment does not list, such as
%% the `ao-types' of its values, keeps its place.
order_components(Components, Keys, BodyKeys) ->
    Listed =
        hb_util:unique(
            dev_httpsig_siginfo:add_derived_specifiers(
                [
                    case lists:member(Key, BodyKeys) of
                        true -> <<"content-digest">>;
                        false -> dev_httpsig_conv:encode_key(Key)
                    end
                ||
                    Key <- Keys
                ]
            )
        ),
    {Ordered, []} =
        lists:mapfoldl(
            fun(Component, Rest) ->
                case lists:member(Component, Listed) of
                    true -> {hd(Rest), tl(Rest)};
                    false -> {Component, Rest}
                end
            end,
            [Key || Key <- Listed, lists:member(Key, Components)],
            Components
        ),
    Ordered.

%% @doc The keys a commitment lists as committed, in order.
committed_keys(Commitment, Opts) ->
    hb_util:message_to_ordered_list(
        maps:get(<<"committed">>, Commitment, []),
        Opts
    ).

%% @doc Normalize the keys that a commitment lists, so that they can be matched
%% against the keys of the message.
input_keys(RawInputs) ->
    lists:map(fun hb_ao:normalize_key/1, RawInputs).

%% @doc The keys a commitment lists that the message does not carry.
missing_keys(Msg, Commitment, Opts) ->
    [
        Key
    ||
        Key <- input_keys(committed_keys(Commitment, Opts)),
        not key_present(Key, Msg)
    ].

%% @doc Calculate if a key is present in a message, either as it is or
%% percent-encoded as it is on the wire.
key_present(Key, Msg) ->
    is_map_key(Key, Msg)
        orelse is_map_key(dev_httpsig_conv:encode_key(Key), Msg).

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
            Opts
        ),
    ?event_debug({component_identifiers_for_sig_base, {priv_components, ComponentsLines}}),
	ParamsLine = signature_params_line(Commitment, Opts),
    SignatureBase = 
        <<
            ComponentsLines/binary, "\n",
            "\"@signature-params\": ", ParamsLine/binary
        >>,
    ?event_debug(signature_base, {signature_base, {string, SignatureBase}}),
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
                [Key] = dev_httpsig_siginfo:remove_derived_specifiers([Name]),
                case maps:get(Key, Req, not_found) of
                    not_found ->
                        throw(
                            {
                                missing_key_for_signature_component_line,
                                Name,
                                {message, Req},
                                {commitment, Commitment}
                            }
                        );
                    Value ->
                        <<"\"", Name/binary, "\": ", Value/binary>>
                end
            end,
            maps:get(<<"committed">>, Commitment)
        ),
	iolist_to_binary(lists:join(<<"\n">>, ComponentsLines)).

%% @doc construct the "signature-params-line" part of the signature base.
%%
%% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.3.2.4
signature_params_line(RawCommitment, Opts) ->
    ?event_debug(debug_enc, {signature_params_line, {commitment, RawCommitment}}),
	hb_util:bin(
        hb_structured_fields:list(
            [
                % We build the line from the parameters that `httpsig@1.0'
                % sends, even if the commitment is of another device that
                % signs in this format, such as `json@1.0'.
                dev_httpsig_siginfo:signature_input(
                    hb_util:message_to_ordered_list(
                        maps:get(<<"committed">>, RawCommitment),
                        Opts
                    ),
                    RawCommitment#{
                        <<"commitment-device">> => <<"httpsig@1.0">>
                    },
                    Opts
                )
            ]
        )
    ).

%%%
%%% TESTS
%%%

%%% Integration Tests

%% @doc Commitment identities must agree with the verified key and signature.
commitment_identity_test() ->
    Opts = #{ <<"priv-wallet">> => ar_wallet:new() },
    Msg = #{ <<"body">> => <<"authenticated">> },
    Victim = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    Signed = hb_message:commit(Msg, Opts),
    % The unsigned commitment hashes the body once.
    erlang:trace_pattern({hb_crypto, sha256, 1}, true, [call_time]),
    erlang:trace(self(), true, [call]),
    Unsigned = hb_message:commit(Msg, Opts, #{ <<"type">> => <<"unsigned">> }),
    {call_time, [{_, Hashes, _, _}]} =
        erlang:trace_info({hb_crypto, sha256, 1}, call_time),
    erlang:trace_pattern({hb_crypto, sha256, 1}, false, [call_time]),
    ?assertEqual(1, Hashes),
    % `commitment_devices/2' does not run the `device' of a commitment.
    Device =
        #{ <<"commitment-device">> => fun(_, _, _) -> {ok, <<"ran">>} end },
    ?assertEqual(
        [<<"httpsig@1.0">>],
        hb_message:commitment_devices(
            Signed#{
                <<"commitments">> =>
                    maps:map(
                        fun(_, C) -> C#{ <<"device">> => Device } end,
                        maps:get(<<"commitments">>, Signed)
                    )
            },
            Opts
        )
    ),
    % The `keyid' of an unsigned commitment is `?UNSIGNED_COMMITTER'.
    ?assertMatch(
        [#{ <<"keyid">> := ?UNSIGNED_COMMITTER }],
        maps:values(maps:get(<<"commitments">>, Unsigned))
    ),
    % The committer of unsigned commitments, given as `HMAC:constant:ao',
    % makes the same unsigned commitment.
    ?assertEqual(
        Unsigned,
        hb_message:commit(
            Msg,
            Opts,
            #{
                <<"type">> => <<"unsigned">>,
                <<"committer">> => <<"HMAC:constant:ao">>
            }
        )
    ),
    % The `keyid' of a signed commitment is the URI of the committer of its
    % key. The commitment carries the key as its `public-key', and has no
    % `owner'.
    Committer = hb_test_utils:committer(maps:get(<<"priv-wallet">>, Opts)),
    [SignedCommitment] = maps:values(maps:get(<<"commitments">>, Signed)),
    ?assertMatch(
        #{
            <<"keyid">> := Committer,
            <<"committer">> := Committer,
            <<"public-key">> := _
        },
        SignedCommitment
    ),
    ?assertNot(maps:is_key(<<"owner">>, SignedCommitment)),
    % A signed commitment whose `public-key' is not the key of the committer in
    % its `keyid' does not verify.
    #{ <<"owner">> := OtherKey } = ar_wallet:new(),
    ?assertNot(
        hb_message:verify(
            Signed#{
                <<"commitments">> =>
                    maps:map(
                        fun(_, C) ->
                            C#{ <<"public-key">> => hb_util:encode(OtherKey) }
                        end,
                        maps:get(<<"commitments">>, Signed)
                    )
            },
            all,
            Opts
        )
    ),
    lists:foreach(
        fun(Committed) ->
            ?assert(hb_message:verify(
                Committed, #{ <<"ids">> => <<"all">> }, Opts)),
            Forged = Committed#{ <<"commitments">> => maps:map(
                fun(_, C) -> C#{ <<"committer">> => Victim } end,
                maps:get(<<"commitments">>, Committed)
            ) },
            ?assertNot(hb_message:verify(Forged, all, Opts)),
            Renamed = Committed#{ <<"commitments">> => #{
                Victim => hd(maps:values(maps:get(<<"commitments">>, Committed)))
            } },
            ?assertNot(hb_message:verify(
                Renamed, #{ <<"ids">> => <<"all">> }, Opts))
        end,
        [Signed, Unsigned]
    ),
    Wire = hb_message:convert(Signed, <<"httpsig@1.0">>, Opts),
    Roundtrip = hb_message:convert(
        Wire, <<"structured@1.0">>, <<"httpsig@1.0">>, Opts
    ),
    ?assert(hb_message:verify(Roundtrip, all, Opts)),
    % A key that is an RFC 9421 derived component, such as `path', is signed as
    % that component, `@path'.
    PathWire =
        hb_message:convert(
            hb_message:commit(
                #{ <<"path">> => <<"/x">>, <<"a">> => <<"1">> },
                Opts
            ),
            <<"httpsig@1.0">>,
            Opts
        ),
    ?assertNotEqual(
        nomatch,
        binary:match(
            maps:get(<<"signature-input">>, PathWire),
            <<"\"@path\"">>
        )
    ),
    % A key that no commitment covers is not sent in the body, which the
    % commitment covers in full, so the message still verifies.
    Extended = Signed#{ <<"big">> => binary:copy(<<"x">>, 5000) },
    ?assert(
        hb_message:verify(
            hb_message:convert(
                hb_message:convert(Extended, <<"httpsig@1.0">>, Opts),
                <<"structured@1.0">>,
                <<"httpsig@1.0">>,
                Opts
            ),
            all,
            Opts
        )
    ),
    % A message that the RFC 9421 library `http-message-signatures' signed,
    % with the key as its `public-key' parameter, verifies.
    {ok, External} =
        file:read_file(<<"test/http-message-signatures.js/signed.json">>),
    ?assert(
        hb_message:verify(
            hb_message:convert(
                hb_json:decode(External),
                <<"structured@1.0">>,
                <<"httpsig@1.0">>,
                Opts
            ),
            all,
            Opts
        )
    ),
    % A request that the library signed over `@path', the path of its URL,
    % verifies with that path as its `path' key.
    {ok, ExternalPath} =
        file:read_file(<<"test/http-message-signatures.js/signed-path.json">>),
    ?assert(
        hb_message:verify(
            hb_message:convert(
                (hb_json:decode(ExternalPath))#{ <<"path">> => <<"/x">> },
                <<"structured@1.0">>,
                <<"httpsig@1.0">>,
                Opts
            ),
            all,
            Opts
        )
    ).

%% @doc Ensure that a publicly known RSA key cannot authenticate an HMAC
%% signer. A commitment that uses the key as its secret is made by its own
%% `hmac:secret:' committer, not by the key's address.
public_key_hmac_is_not_authority_test() ->
    #{ <<"owner">> := Pub } = ar_wallet:new(),
    Opts = #{ <<"priv-wallet">> => ar_wallet:new() },
    ID = lib_secret:to_committer(Pub),
    Committed = hb_message:commit(
        #{ <<"body">> => <<"public-key-hmac">> }, Opts,
        #{
            <<"type">> => <<"hmac-sha256">>,
            <<"committer">> =>
                #{
                    <<"device">> => <<"hmac@1.0">>,
                    <<"body">> => <<"secret:", ID/binary>>,
                    <<"priv">> => #{ <<"secret">> => Pub }
                }
        }
    ),
    ?assertEqual(
        [<<"hmac:secret:", ID/binary>>],
        hb_message:signers(Committed, Opts)
    ).

%% @doc HTTPSig rejects weak moduli, including zero-padded keys.
rsa_minimum_modulus_test() ->
    lists:foreach(
        fun(Bits) ->
            {[_, Pub], [_, Pub, Priv | _]} = crypto:generate_key(rsa, {Bits, 65537}),
            lists:foreach(
                fun(Key) ->
                    Wallet = ar_wallet:new({rsa, 65537}, Priv, Key),
                    Opts = #{ <<"priv-wallet">> => Wallet },
                    Data = <<"rsa-size">>,
                    Signature = ar_wallet:sign(Wallet, Data),
                    ?assert(ar_wallet:verify(Wallet, Data, Signature)),
                    Signed = hb_message:commit(#{ <<"body">> => Data }, Opts),
                    ?assertEqual(Bits >= 2048, hb_message:verify(Signed, all, Opts))
                end,
                [Pub, <<0:4096, Pub/binary>>]
            )
        end,
        [1536, 2048]
    ).

%% @doc Ensure that we can validate a signature on an extremely large and complex
%% message that is sent over HTTP, signed with the codec.
validate_large_message_from_http_test() ->
    Node = hb_http_server:start_node(Opts = #{
        <<"force-signed">> => true,
        <<"commitment-device">> => <<"httpsig@1.0">>,
        extra =>
            [
                [
                    [
                        #{
                            <<"n">> => N,
                            <<"m">> => M,
                            <<"o">> => O
                        }
                    ||
                        O <- lists:seq(1, 3)
                    ]
                ||
                    M <- lists:seq(1, 3)
                ]
            ||
                N <- lists:seq(1, 3)
            ]
    }),
    {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, Opts),
    Signers = hb_message:signers(Res, Opts),
    ?event_debug({received, {signers, Signers}, {res, Res}}),
    ?assert(length(Signers) == 1),
    ?assert(hb_message:verify(Res, Signers, Opts)),
    ?event_debug({sig_verifies, Signers}),
    ?assert(hb_message:verify(Res, all, Opts)),
    ?event_debug({hmac_verifies, <<"hmac-sha256">>}),
    {ok, _, #{ <<"committed">> := HashpathCommitted }} =
        hb_message:commitment(
            #{ <<"committer">> => hd(Signers) },
            Res,
            Opts
        ),
    ?assertEqual([<<"hashpath">>], HashpathCommitted).

committed_id_test() ->
    Msg = #{ <<"basic">> => <<"value">> },
    Opts = #{ <<"priv-wallet">> => hb:wallet() },
    Signed = hb_message:commit(Msg, Opts),
    ?assert(hb_message:verify(Signed, all, Opts)),
    % The type of `basic' is committed: as an atom, it does not verify.
    ?assertNot(hb_message:verify(Signed#{ <<"basic">> => value }, all, Opts)),
    ?event_debug({signed_msg, Signed}),
    UnsignedID = hb_message:id(Signed, none),
    SignedID = hb_message:id(Signed, all),
    ?event_debug({ids, {unsigned_id, UnsignedID}, {signed_id, SignedID}}),
    ?assertNotEqual(UnsignedID, SignedID),
    % A commitment covers only the keys that it is asked to cover, and not a
    % key that is the escaped form of one of them.
    Committed =
        hb_message:commit(
            #{ <<"a b">> => 1, <<"a%20b">> => <<"2">> },
            Opts,
            #{ <<"committed">> => [<<"a b">>] }
        ),
    ?assert(hb_message:verify(Committed#{ <<"a%20b">> => <<"3">> }, all, Opts)),
    % The type of `a b' is committed: as a binary, it does not verify.
    ?assertNot(
        hb_message:verify(Committed#{ <<"a b">> => <<"1">> }, all, Opts)
    ),
    % Both forms of a signed message contain its signature as a header.
    HTTPSigSigned =
        hb_message:commit(Msg#{ <<"device">> => <<"httpsig@1.0">> }, Opts),
    ?assertMatch(
        {ok, <<"basic: value\r\ndevice: httpsig@1.0\r\nsignature: ", _/binary>>},
        hb_ao:resolve(HTTPSigSigned, <<"serialize">>, Opts)
    ),
    ?assertMatch(
        {ok, #{ <<"headers">> := #{ <<"signature">> := _ } }},
        hb_ao:resolve(
            HTTPSigSigned,
            #{ <<"path">> => <<"serialize">>, <<"format">> => <<"components">> },
            Opts
        )
    ).

commit_secret_key_test() ->
    Msg = #{ <<"basic">> => <<"value">> },
    Opts = #{ <<"priv-wallet">> => hb:wallet() },
    ID = lib_secret:to_committer(<<"test-secret">>),
    CommittedMsg =
        hb_message:commit(
            Msg,
            Opts,
            #{
                <<"type">> => <<"hmac-sha256">>,
                <<"commitment-device">> => <<"httpsig@1.0">>,
                <<"committer">> =>
                    #{
                        <<"device">> => <<"hmac@1.0">>,
                        <<"body">> => <<"secret:", ID/binary>>,
                        <<"priv">> => #{ <<"secret">> => <<"test-secret">> }
                    }
            }
        ),
    ?event_debug({committed_msg, CommittedMsg}),
    Committers = hb_message:signers(CommittedMsg, #{}),
    % The commitment of a secret has its `hmac:secret:' committer as its signer.
    ?assertEqual([<<"hmac:secret:", ID/binary>>], Committers),
    ?event_debug({committers, Committers}),
    ?assert(
        hb_message:verify(
            CommittedMsg,
            #{
                <<"committers">> => Committers,
                <<"priv">> => #{ <<"secret">> => <<"test-secret">> }
            },
            #{}
        )
    ),
    ?assertNot(
        hb_message:verify(
            CommittedMsg,
            #{
                <<"committers">> => Committers,
                <<"priv">> => #{ <<"secret">> => <<"bad-secret">> }
            },
            #{}
        )
    ).

multicommitted_id_test() ->
    Msg = #{ <<"basic">> => <<"value">> },
    Signed1 = hb_message:commit(Msg, #{ <<"priv-wallet">> => Wallet1 = ar_wallet:new() }),
    Signed2 = hb_message:commit(Signed1, #{ <<"priv-wallet">> => Wallet2 = ar_wallet:new() }),
    Addr1 = hb_test_utils:committer(Wallet1),
    Addr2 = hb_test_utils:committer(Wallet2),
    ?event_debug({signed_msg, Signed2}),
    UnsignedID = hb_message:id(Signed2, none),
    SignedID = hb_message:id(Signed2, all),
    ?event_debug({ids, {unsigned_id, UnsignedID}, {signed_id, SignedID}}),
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
        <<"untyped">> => #{ <<"inner-untyped">> => <<"inner-value">> },
        <<"typed">> => #{ <<"inner-typed">> => 123 }
    },
    Opts = #{ <<"priv-wallet">> => hb:wallet() },
    NormMsg = hb_message:convert(Msg, <<"structured@1.0">>, #{}),
    ?event_debug({msg, NormMsg}),
    Signed = hb_message:commit(NormMsg, Opts),
    ?event_debug({signed_msg, Signed}),
    ?assert(hb_message:verify(Signed, Opts)).
