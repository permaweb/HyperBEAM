%%% @doc This module implements HTTP Message Signatures as described in RFC-9421
%%% (https://datatracker.ietf.org/doc/html/rfc9421), as an AO-Core device.
%%% It implements the codec standard (from/1, to/1), as well as the optional
%%% commitment functions (id/3, sign/3, verify/3). The commitment functions
%%% are found in this module, while the codec functions are relayed to the 
%%% `dev_codec_httpsig_conv' module.
-module(dev_codec_httpsig).
%%% Device API
-export([id/3, commit/3, committed/3, verify/3, reset_hmac/1, public_keys/1]).
%%% Codec API functions
-export([to/1, from/1]).
%%% Public API functions
-export([add_content_digest/1]).
-export([add_derived_specifiers/1, remove_derived_specifiers/1]).
% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.7-14
-define(EMPTY_QUERY_PARAMS, $?).
% https://datatracker.ietf.org/doc/html/rfc9421#name-signature-parameters
-define(SIGNATURE_PARAMS, [created, expired, nonce, alg, keyid, tag]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-type fields() :: #{
	binary() | atom() | string() => binary() | atom() | string()
}.
-type request_message() :: #{
	url => binary(),
	method => binary(),
	headers => fields(),
	trailers => fields(),
	is_absolute_form => boolean()
}.
-type response_message() :: #{
	status => integer(),
	headers => fields(),
	trailers => fields()
}.
-type component_identifier() :: {
	item,
	{string, binary()},
	{binary(), integer() | boolean() | {string | token | binary, binary()}}
}.

%%% A list of components that are `derived' in the context of RFC-9421 from the
%%% request message.
-define(DERIVED_COMPONENTS, [
    <<"method">>,
    <<"target-uri">>,
    <<"authority">>,
    <<"scheme">>,
    <<"request-target">>,
    <<"path">>,
    <<"query">>,
    <<"query-param">>,
    <<"status">>
]).

%%% Routing functions for the `dev_codec_httpsig_conv' module
to(Msg) -> dev_codec_httpsig_conv:to(Msg).
from(Msg) -> dev_codec_httpsig_conv:from(Msg).

%%% A map that contains signature parameters metadata as described
%%% in https://datatracker.ietf.org/doc/html/rfc9421#name-signature-parameters
%%%
%%% All values are optional, but in our use-case "alg" and "keyid" will
%%% almost always be provided.
%%%
%%% #{
%%% 	created => 1733165109, % a unix timestamp
%%% 	expires => 1733165209, % a unix timestamp
%%% 	nonce => <<"foobar">,
%%% 	alg => <<"rsa-pss-sha512">>,
%%% 	keyid => <<"6eVuWgpNgv3bxfNgFrIiTkzE8Yb0V2omShxS4uKyzpw">>
%%% 	tag => <<"HyperBEAM">>
%%% }
-type signature_params() ::
    #{atom() | binary() | string() => binary() | integer()}.

%%% The state encapsulated as the "Authority".
%%% It includes an ordered list of parsed component identifiers, used for 
%%% extracting values from the Request/Response Message Context, as well as 
%%% the signature parameters used when creating the signature and encode in 
%%% the signature base.
%%%
%%% This is effectively the State of an Authority, used to sign a Request/Response 
%%% Message Context.
%%%
%%% #{
%%% 	component_identifiers => [{item, {string, <<"@method">>}, []}]
%%% 	sig_params => #{
%%% 		created => 1733165109, % a unix timestamp
%%% 		expires => 1733165209, % a unix timestamp
%%% 		nonce => <<"foobar">,
%%% 		alg => <<"rsa-pss-sha512">>,
%%% 		keyid => <<"6eVuWgpNgv3bxfNgFrIiTkzE8Yb0V2omShxS4uKyzpw">>
%%% 		tag => <<"HyperBEAM">>
%%% 	}
%%% }
-type authority_state() :: #{
	component_identifiers => [component_identifier()],
	% TODO: maybe refine this to be more explicit w.r.t valid signature params
	sig_params => signature_params(),
	key => binary()
}.

id(Msg, _Params, _Opts) ->
    ?event({calculating_id, {msg, Msg}}),
    case find_id(Msg) of
        {ok, ID} -> {ok, hb_util:human_id(ID)};
        {not_found, MsgToID} ->
            {ok, MsgAfterReset} = reset_hmac(MsgToID),
            {ok, ID} = find_id(MsgAfterReset),
            {ok, hb_util:human_id(ID)}
    end.

%% @doc Find the ID of the message, which is the hmac of the fields referenced in
%% the signature and signature input. If the message already has a signature-input,
%% directly, it is treated differently: We relabel it as `x-signature-input' to
%% avoid key collisions.
find_id(Msg = #{ <<"commitments">> := Comms }) when map_size(Comms) > 1 ->
    #{ <<"commitments">> := CommsWithoutHmac } =
        hb_message:without_commitments(
            #{ <<"alg">> => <<"hmac-sha256">> },
            Msg
        ),
    IDs = maps:keys(CommsWithoutHmac),
    case IDs of
        [] -> throw({could_not_find_ids, CommsWithoutHmac});
        [ID] ->
            ?event({returning_single_id, ID}),
            {ok, hb_util:human_id(ID)};
        _ ->
            ?event({multiple_ids, IDs}),
            SortedIDs =
                [
                    {item, {string, hb_util:human_id(ID)}, []}
                ||
                    ID <- lists:sort(IDs)
                ],
            SFList = iolist_to_binary(hb_structured_fields:list(SortedIDs)),
            ?event({sorted_ids, SortedIDs, {sf_list, SFList}}),
            {ok, hb_util:human_id(crypto:hash(sha256, SFList))}
    end;
find_id(#{ <<"commitments">> := CommitmentMap }) ->
    {ok, hd(maps:keys(CommitmentMap))};
find_id(AttMsg = #{ <<"signature-input">> := UserSigInput }) ->
    {not_found, (maps:without([<<"signature-input">>], AttMsg))#{
        <<"x-signature-input">> => UserSigInput
    }};
find_id(Msg) ->
    ?event({no_id, Msg}),
    {not_found, Msg}.

%% @doc Main entrypoint for signing a HTTP Message, using the standardized format.
commit(MsgToSign, _Req, Opts) ->
    Wallet = hb_opts:get(priv_wallet, no_viable_wallet, Opts),
    NormMsg = hb_ao:normalize_keys(MsgToSign),
    % The hashpath, if present, is encoded as a HTTP Sig tag,
    % added as a field on the commitment, and then the field is removed from the Msg,
    % so that it is not included in the actual signature matierial.
    %
    % In this sense, the hashpath is a property of the commitment
    % and the signature metadata, not the message itself, being signed
    % See https://datatracker.ietf.org/doc/html/rfc9421#section-2.3-4.12
    {SigParams, MsgWithoutHP} =
        case NormMsg of
            #{ <<"priv">> := #{ <<"hashpath">> := HP }} ->
                {#{ tag => HP }, NormMsg};
            _ -> {#{}, NormMsg}
        end,
    EncWithoutBodyKeys =
        maps:without(
            [<<"signature">>, <<"signature-input">>, <<"body-keys">>, <<"priv">>],
            hb_message:convert(MsgWithoutHP, <<"httpsig@1.0">>, Opts)
        ),
    Enc = add_content_digest(EncWithoutBodyKeys),
    ?event({encoded_to_httpsig_for_commitment, Enc}),
    Authority = authority(lists:sort(maps:keys(Enc)), SigParams, Wallet),
    {ok, {SignatureInput, Signature}} = sign_auth(Authority, #{}, Enc),
    [ParsedSignatureInput] = hb_structured_fields:parse_list(SignatureInput),
    % Set the name as `http-sig-[hex encoding of the first 8 bytes of the public key]'
    ID = hb_util:human_id(crypto:hash(sha256, Signature)),
    Address = ar_wallet:to_address(Wallet),
    SigName = address_to_sig_name(Address),
    % Calculate the id and place the signature into the `commitments' key of the message.
    Commitment =
        #{
            <<"commitment-device">> => <<"httpsig@1.0">>,
            <<"alg">> => <<"rsa-pss-sha512">>,
            <<"committer">> => hb_util:human_id(Address),
            % https://datatracker.ietf.org/doc/html/rfc9421#section-4.2-1
            <<"signature">> =>
                bin(hb_structured_fields:dictionary(
                    #{ SigName => {item, {binary, Signature}, []} }
                )),
            <<"signature-input">> =>
                bin(hb_structured_fields:dictionary(
                    #{ SigName => ParsedSignatureInput }
                ))
        },
    OldCommitments = maps:get(<<"commitments">>, NormMsg, #{}),
    reset_hmac(MsgWithoutHP#{<<"commitments">> =>
        OldCommitments#{ ID => Commitment }
    }).

%% @doc Return the list of committed keys from a message. The message will have
%% had the `commitments' key removed and the signature inputs added to the
%% root. Subsequently, we can parse that to get the list of committed keys.
committed(RawMsg, Req, Opts) ->
    Msg = to(RawMsg),
    case maps:get(<<"signature-input">>, Msg, none) of
        none -> {ok, []};
        SigInput ->
            do_committed(SigInput, Msg, Req, Opts)
    end.

do_committed(SigInputStr, Msg, _Req, _Opts) ->
    [{_SigInputName, SigInput} | _] = hb_structured_fields:parse_dictionary(
        SigInputStr
    ),
    {list, ComponentIdentifiers, _SigParams} = SigInput,
    BinComponentIdentifiers = lists:map(
        fun({item, {_Kind, ID}, _Params}) -> ID end,
        ComponentIdentifiers    
    ),
    Signed =
        [<<"signature">>, <<"signature-input">>] ++
            remove_derived_specifiers(BinComponentIdentifiers),
    % Extract the implicit keys from the `ao-types' of the encoded message if
    % the types key itself was signed.
    SignedWithImplicit = Signed ++
        case lists:member(<<"ao-types">>, Signed) of
            true -> dev_codec_structured:implicit_keys(Msg);
            false -> []
        end,
    case lists:member(<<"content-digest">>, SignedWithImplicit) of
        false -> {ok, SignedWithImplicit};
        true -> {ok, SignedWithImplicit ++ committed_from_body(Msg)}
    end.

%% @doc Return the list of committed keys from a message that are derived from
%% the body components.
committed_from_body(Msg) ->
    % Body and inline-body-key are always committed if the
    % content-digest is present.
    [<<"body">>, <<"inline-body-key">>] ++
        % If the inline-body-key is present, add it to the list of
        % committed keys.
        case maps:get(<<"inline-body-key">>, Msg, []) of
            [] -> [];
            InlineBodyKey -> [InlineBodyKey]
        end
        % If the body-keys are present, add them to the list of
        % committed keys.
        ++ case maps:get(<<"body-keys">>, Msg, []) of
            [] -> [];
            BodyKeys ->
                ParsedList = case BodyKeys of
                    List when is_list(List) -> List;
                    RawBodyKeys when is_binary(RawBodyKeys) ->
                        hb_structured_fields:parse_list(RawBodyKeys) 
                end,
                % Ensure a list of binaries, extracting the binary
                % from the structured item if necessary
                ParsedBodyKeys = lists:map(
                    fun
                        (BK) when is_binary(BK) -> BK;
                        ({ item, {_, BK }, _}) -> BK
                    end,
                    ParsedList   
                ),
                % Grab the top most field on the body key
                % because the top most being committed means all subsequent
                % fields are also committed
                Tops = lists:map(
                    fun(BodyKey) ->
                        hd(hb_path:term_to_path_parts(BodyKey, #{}))
                    end,
                    ParsedBodyKeys
                ),
                lists:sort(lists:uniq(Tops))
        end.

%% @doc If the `body' key is present, replace it with a content-digest.
add_content_digest(Msg) ->
    case maps:get(<<"body">>, Msg, not_found) of
        not_found -> Msg;
        Body ->
            % Remove the body from the message and add the content-digest,
            % encoded as a structured field.
            ?event({add_content_digest, {string, Body}}),
            (maps:without([<<"body">>], Msg))#{
                <<"content-digest">> =>
                    iolist_to_binary(hb_structured_fields:dictionary(
                        #{
                            <<"sha-256">> =>
                                {item, {binary, hb_crypto:sha256(Body)}, []}
                        }
                    ))
            }
    end.

%% @doc Convert an address to a signature name that is short, unique to the
%% address, and lowercase.
-spec address_to_sig_name(binary()) -> binary().
address_to_sig_name(Address) when ?IS_ID(Address) ->
    <<"http-sig-", (hb_util:to_hex(binary:part(hb_util:native_id(Address), 1, 8)))/binary>>;
address_to_sig_name(OtherRef) ->
    OtherRef.

%%@doc Ensure that the commitments and hmac are properly encoded
reset_hmac(RawMsg) ->
    Msg = hb_message:convert(RawMsg, tabm, #{}),
    WithoutHmac =
        hb_message:without_commitments(
            #{
                <<"commitment-device">> => <<"httpsig@1.0">>,
                <<"alg">> => <<"hmac-sha256">>
            },
            Msg
        ),
    Commitments = maps:get(<<"commitments">>, WithoutHmac, #{}),
    AllSigs =
        maps:from_list(lists:map(
            fun ({Committer, #{ <<"signature">> := Signature }}) ->
                SigNameFromDict = sig_name_from_dict(Signature),
                ?event({name_options,
                    {committer, Committer},
                    {sig_name_from_dict, SigNameFromDict}}
                ),
                SigBin =
                    maps:get(SigNameFromDict,
                        maps:from_list(
                            hb_structured_fields:parse_dictionary(Signature)
                        )
                    ),
                {SigNameFromDict, SigBin}
            end,
            maps:to_list(Commitments)
        )),
    AllInputs =
        maps:from_list(lists:map(
            fun ({_Committer, #{ <<"signature-input">> := Inputs }}) ->
                SigNameFromDict = sig_name_from_dict(Inputs),
                Res = hb_structured_fields:parse_dictionary(Inputs),
                SingleSigInput = maps:get(SigNameFromDict, maps:from_list(Res)),
                {SigNameFromDict, SingleSigInput}
            end,
            maps:to_list(Commitments)
        )),
    FlatInputs = lists:flatten(maps:values(AllInputs)),
    HMacSigInfo =
        case FlatInputs of
            [] -> #{};
            _ -> #{
                <<"signature">> =>
                    bin(hb_structured_fields:dictionary(AllSigs)),
                <<"signature-input">> =>
                    bin(hb_structured_fields:dictionary(AllInputs))
            }
        end,
    ?event({pre_hmac_sig_input,
        {string, maps:get(<<"signature-input">>, HMacSigInfo, none)}}),
    HMacInputMsg = maps:merge(Msg, HMacSigInfo),
    {ok, RawID} = hmac(HMacInputMsg),
    ID = hb_util:human_id(RawID),
    Res = {
        ok,
        maps:put(
            <<"commitments">>,
            Commitments#{
                ID =>
                    HMacSigInfo#{
                        <<"commitment-device">> => <<"httpsig@1.0">>,
                        <<"alg">> => <<"hmac-sha256">>
                    }
            },
            Msg
        )
    },
    ?event({reset_hmac_complete, Res}),
    Res.

sig_name_from_dict(DictBin) ->
    [{SigNameFromDict, _}] = hb_structured_fields:parse_dictionary(DictBin),
    SigNameFromDict.

%% @doc Generate the ID of the message, with the current signature and signature
%% input as the components for the hmac.
hmac(Msg) ->
    % The message already has a signature and signature input, so we can use
    % just those as the components for the hmac
    EncodedMsg =
        maps:without(
            [<<"body-keys">>],
            to(maps:without([<<"commitments">>, <<"body-keys">>], Msg))
        ),
    % Remove the body and set the content-digest as a field
    MsgWithContentDigest = add_content_digest(EncodedMsg),
    % Find the keys to use for the hmac. These should be set by the signature
    % input, but if that is not present, then use all the keys from the encoded
    % message.
    HMacKeys =
        case maps:get(<<"signature-input">>, Msg, none) of
            none -> 
                ?event(no_sig_input_found),
                maps:keys(MsgWithContentDigest);
            SigInput ->
                ?event(sig_input_found),
                [{_, {list, Items, _}}|_]
                    = hb_structured_fields:parse_dictionary(SigInput),
                ?event({parsed_sig_input_dict, {explicit, Items}}),
                [ Name || {item, {_, Name}, _} <- Items ]
        end,
    HMACSpecifiers = normalize_component_identifiers(HMacKeys),
    % Generate the signature base
    {_, SignatureBase} = signature_base(
        #{
            component_identifiers => HMACSpecifiers,
            sig_params => #{
                keyid => <<"ao">>,
                alg => <<"hmac-sha256">>
            }
        },
        #{},
        MsgWithContentDigest
    ),
    ?event({hmac_keys, {explicit, HMacKeys}}),
    ?event({hmac_base, {string, SignatureBase}}),
    HMacValue = crypto:mac(hmac, sha256, <<"ao">>, SignatureBase),
    ?event({hmac_result, {string, hb_util:human_id(HMacValue)}}),
    {ok, HMacValue}.

%% @doc Verify different forms of httpsig committed messages. `dev_message:verify'
%% already places the keys from the commitment message into the root of the
%% message.
verify(MsgToVerify, #{ <<"commitment">> := ExpectedID, <<"alg">> := <<"hmac-sha256">> }, _Opts) ->
    % Verify a hmac on the message
    ?event({verify_hmac, {target, MsgToVerify}, {expected_id, ExpectedID}}),
    {ok, ResetMsg} = reset_hmac(maps:without([<<"id">>], MsgToVerify)),
    case maps:get(<<"commitments">>, ResetMsg, no_commitments) of
        no_commitments -> {error, could_not_calculate_id};
        #{ ExpectedID := #{ <<"alg">> := <<"hmac-sha256">> } } ->
            ?event({hmac_verified, {id, ExpectedID}}),
            {ok, true};
        _ ->
            ?event({hmac_failed_verification,
                {recalculated_commitments,
                    maps:keys(maps:get(<<"commitments">>, ResetMsg, #{}))
                },
                {expected_id, ExpectedID}}),
            {ok, false}
    end;
verify(MsgToVerify, Req, _Opts) ->
    % Validate a signed commitment.
    ?event({verify, {target, MsgToVerify}, {req, Req}}),
    % Parse the signature parameters into a map.
    CommitmentID = maps:get(<<"commitment">>, Req),
    Commitment =
        maps:get(
            CommitmentID,
            maps:get(<<"commitments">>, MsgToVerify, #{})
        ),
    SigName = address_to_sig_name(maps:get(<<"committer">>, Commitment)),
    {list, _SigInputs, ParamsKVList} =
        maps:get(
            SigName,
            maps:from_list(
                hb_structured_fields:parse_dictionary(
                    maps:get(<<"signature-input">>, MsgToVerify)
                )
            )
        ),
    {string, Alg} = maps:get(<<"alg">>, Params = maps:from_list(ParamsKVList)),
    AlgFromCommitment = maps:get(<<"alg">>, Commitment),
    case Alg of
        _ when AlgFromCommitment =/= Alg ->
            {error, {commitment_alg_mismatch,
                {from_commitment_message, AlgFromCommitment},
                {from_signature_params, Alg}
            }};
        <<"rsa-pss-sha512">> when AlgFromCommitment =:= Alg ->
            {string, KeyID} = maps:get(<<"keyid">>, Params),
            PubKey = hb_util:decode(KeyID),
            Address = hb_util:human_id(ar_wallet:to_address(PubKey)),
            % Re-run the same conversion that was done when creating the signature.
            Enc = hb_message:convert(MsgToVerify, <<"httpsig@1.0">>, #{}),
            EncWithoutBodyKeys = maps:without([<<"body-keys">>], Enc),
            % Add the signature data back into the encoded message.
            EncWithSig =
                EncWithoutBodyKeys#{
                    <<"signature-input">> =>
                        maps:get(<<"signature-input">>, MsgToVerify),
                    <<"signature">> =>
                        maps:get(<<"signature">>, MsgToVerify)
                },
            % If the content-digest is already present, we override it with a
            % regenerated value. If those values match, then the signature will
            % verify correctly. If they do not match, then the signature will
            % fail to verify, as the signature bases will not be the same.
            EncWithDigest = add_content_digest(EncWithSig),
            ?event({encoded_msg_for_verification, EncWithDigest}),
            Res = verify_auth(
                #{
                    key => {{rsa, 65537}, PubKey},
                    sig_name => address_to_sig_name(Address)
                },
                EncWithDigest
            ),
            ?event({rsa_verify_res, Res}),
            {ok, Res};
        _ ->
            {error, {unsupported_alg, Alg}}
    end.

public_keys(Commitment) ->
    SigInputs = maps:get(<<"signature-input">>, Commitment),
    lists:filtermap(
        fun ({_SigName, {list, _, ParamsKVList}}) ->
            case maps:get(<<"alg">>, Params = maps:from_list(ParamsKVList)) of
                {string, <<"rsa-pss-sha512">>} ->
                    {string, KeyID} = maps:get(<<"keyid">>, Params),
                    PubKey = hb_util:decode(KeyID),
                    {true, PubKey};
                _ ->
                    false
            end
        end,
        hb_structured_fields:parse_dictionary(SigInputs)
    ).

%%% @doc A helper to validate and produce an "Authority" State
-spec authority(
	[binary() | component_identifier()],
	#{binary() => binary() | integer()},
	{} %TODO: type out a key_pair
) -> authority_state().
authority(ComponentIdentifiers, SigParams, PubKey = {KeyType = {ALG, _}, _Pub})
        when is_atom(ALG) ->
    % Only the public key is provided, so use an stub binary for private
    % which will trigger errors downstream if it's needed, which is what we want
    authority(ComponentIdentifiers, SigParams, {{KeyType, <<>>, PubKey}, PubKey});
authority(ComponentIdentifiers, SigParams, PrivKey = {KeyType = {ALG, _}, _, Pub})
        when is_atom(ALG) ->
    % Only the private key was provided, so derive the public from private
    authority(ComponentIdentifiers, SigParams, {PrivKey, {KeyType, Pub}});
authority(ComponentIdentifiers, SigParams, KeyPair = {{_, _, _}, {_, _}}) ->
    #{
		component_identifiers => add_derived_specifiers(ComponentIdentifiers),
		% TODO: add checks to allow only valid signature parameters
		% https://datatracker.ietf.org/doc/html/rfc9421#name-signature-parameters
		sig_params => SigParams,
		key_pair => KeyPair
	}.

%% @doc Takes a list of keys that will be used in the signature inputs and 
%% ensures that they have deterministic sorting, as well as the coorect
%% component identifiers if applicable.
normalize_component_identifiers(ComponentIdentifiers) ->
    Stripped =
        lists:map(
            fun(<<"@", Key/binary>>) -> Key; (Key) -> Key end,
            ComponentIdentifiers
        ),
    lists:sort(add_derived_specifiers(Stripped)).

%% @doc Normalize key parameters to ensure their names are correct.
add_derived_specifiers(ComponentIdentifiers) ->
    lists:flatten(
        lists:map(
            fun(Key) ->
                case lists:member(Key, ?DERIVED_COMPONENTS) of
                    true -> << "@", Key/binary >>;
                    false -> Key
                end
            end,
            ComponentIdentifiers
        )
    ).

%% @doc Remove derived specifiers from a list of component identifiers.
remove_derived_specifiers(ComponentIdentifiers) ->
    lists:map(
        fun(<<"@", Key/binary>>) -> Key; (Key) -> Key end,
        ComponentIdentifiers
    ).

%% @doc using the provided Authority and Request/Response Messages Context,
%% create a Name, Signature and SignatureInput that can be used to additional
%% signatures to a corresponding HTTP Message
-spec sign_auth(authority_state(), request_message(), response_message()) ->
    {ok, {binary(), binary(), binary()}}.
sign_auth(Authority, Req, Res) ->
    {Priv, Pub} = maps:get(key_pair, Authority),
    % Create the signature base and signature-input values
    AuthorityWithSigParams = add_sig_params(Authority, Pub),
	{SignatureInput, SignatureBase} =
        signature_base(AuthorityWithSigParams, Req, Res),
    % Now perform the actual signing
    ?event(signature_base,
        {signature_base_for_signing, {string, SignatureBase}}),
	Signature = ar_wallet:sign(Priv, SignatureBase, sha512),
	{ok, {SignatureInput, Signature}}.

%% @doc Add the signature parameters to the authority state
add_sig_params(Authority, {_KeyType, PubKey}) ->
    maps:put(
        sig_params,
        maps:merge(
            maps:get(sig_params, Authority),
            #{
                alg => <<"rsa-pss-sha512">>,
                keyid => hb_util:encode(PubKey)
            }
        ),
        Authority
    ).

%%% @doc same verify/3, but with an empty Request Message Context
verify_auth(Verifier, Msg) ->
    % Assume that the Msg is a response message, and use an empty Request 
    % message context
    %
    % A corollary is that a signature containing any components from the request
    % will produce an error. It is the caller's responsibility to provide the
    % required Message Context in order to verify the signature
    verify_auth(Verifier, #{}, Msg).

%%% @doc Given the signature name, and the Request/Response Message Context
%%% verify the named signature by constructing the signature base and comparing
verify_auth(#{ sig_name := SigName, key := Key }, Req, Res) ->
    % Signature and Signature-Input fields are each themself a dictionary 
    % structured field.
    % Ergo, we can use our same utilities to extract the value at the desired key,
    % in this case, the signature name. Because our utilities already implement
    % the relevant portions of RFC-9421, we get the error handling here as well.
    % 
    %  See https://datatracker.ietf.org/doc/html/rfc9421#section-3.2-3.2
    SigNameParams = [{<<"key">>, {string, bin(SigName)}}],
    SignatureIdentifier = {item, {string, <<"signature">>}, SigNameParams},
    SignatureInputIdentifier =
        {item, {string, <<"signature-input">>}, SigNameParams},
    % extract signature and signature params
    SigIdentifier = extract_field(SignatureIdentifier, Req, Res),
    SigInputIdentifier = extract_field(SignatureInputIdentifier, Req, Res),
    case {SigIdentifier, SigInputIdentifier} of
        {{ok, {_, EncodedSignature}}, {ok, {_, SignatureInput}}} ->
            % The signature may be encoded ie. as binary, so we need to parse it
            % further as a structured field
            {item, {_, Signature}, _} =
                hb_structured_fields:parse_item(EncodedSignature),
            % The value encoded within signature input is also a structured field,
            % specifically an inner list that encodes the ComponentIdentifiers
            % and the Signature Params.
            % 
            % So we must parse this value, and then use it to construct the 
            % signature base
            [{list, ComponentIdentifiers, SigParams}] =
                hb_structured_fields:parse_list(SignatureInput),
            SigParamsMap = lists:foldl(
                % TODO: does not support SF decimal params
                fun
                    ({Name, {_Kind, Value}}, Map) -> maps:put(Name, Value, Map);
                    ({Name, Value}, Map) -> maps:put(Name, Value, Map)
                end,
                #{},
                SigParams
            ),
            ?event({sig_params_map, ComponentIdentifiers}),
            % Construct the signature base using the parsed parameters
            Authority = authority(ComponentIdentifiers, SigParamsMap, Key),
            {_, SignatureBase} = signature_base(Authority, Req, Res),
            ?event(signature_base,
                {signature_base_for_verification, {string, SignatureBase}}),
            {_Priv, Pub} = maps:get(key_pair, Authority),
            % Now verify the signature base signed with the provided key matches
            % the signature
            ar_wallet:verify(Pub, SignatureBase, Signature, sha512);
        % An issue with parsing the signature
        {SignatureErr, {ok, _}} -> SignatureErr;
        % An issue with parsing the signature input
        {{ok, _}, SignatureInputErr} -> SignatureInputErr;
        % An issue with parsing both, so just return the first one from the
        % signature parsing
        % TODO: maybe could merge the errors?
        {SignatureErr, _} -> SignatureErr
    end.

%%% @doc create the signature base that will be signed in order to create the
%%% Signature and SignatureInput.
%%%
%%% This implements a portion of RFC-9421 see:
%%% https://datatracker.ietf.org/doc/html/rfc9421#name-creating-the-signature-base
signature_base(Authority, Req, Res) when is_map(Authority) ->
    ComponentIdentifiers = maps:get(component_identifiers, Authority),
    ?event({component_identifiers_for_sig_base, ComponentIdentifiers}),
	ComponentsLine = signature_components_line(ComponentIdentifiers, Req, Res),
	ParamsLine =
        signature_params_line(
            ComponentIdentifiers,
            maps:get(sig_params, Authority)),
    SignatureBase = join_signature_base(ComponentsLine, ParamsLine),
    ?event(signature_base, {signature_base, {string, SignatureBase}}),
	{ParamsLine, SignatureBase}.

join_signature_base(ComponentsLine, ParamsLine) ->
    <<
        ComponentsLine/binary, "\n",
        "\"@signature-params\": ", ParamsLine/binary
    >>.

%%% @doc Given a list of Component Identifiers and a Request/Response Message
%%% context, create the "signature-base-line" portion of the signature base
%%% TODO: catch duplicate identifier:
%%% https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.2.5.2.1
%%%
%%% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.1
signature_components_line(ComponentIdentifiers, Req, Res) ->
	ComponentsLines = lists:map(
		fun({Name, DirectBinary}) when is_binary(DirectBinary) andalso is_binary(Name) ->
			<<Name/binary, <<": ">>/binary, DirectBinary/binary>>;
			(ComponentIdentifier) ->
				% TODO: handle errors?
				{ok, {I, V}} = identifier_to_component(ComponentIdentifier, Req, Res),
				<<I/binary, <<": ">>/binary, V/binary>>
		end,
		ComponentIdentifiers
	),
	ComponentsLine = lists:join(<<"\n">>, ComponentsLines),
	bin(ComponentsLine).

%%% @doc construct the "signature-params-line" part of the signature base.
%%%
%%% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.3.2.4
signature_params_line(ComponentIdentifiers, SigParams) ->
	SfList = sf_signature_params(ComponentIdentifiers, SigParams),
	Res = hb_structured_fields:list(SfList),
	bin(Res).

%%% @doc Given a Component Identifier and a Request/Response Messages Context
%%% extract the value represented by the Component Identifier, from the Messages
%%% Context, and return the normalized form of the identifier, along with the
%%% extracted encoded value.
%%%
%%% Generally speaking, a Component Identifier may reference a "Derived" Component,
%%% a Message Field, or a sub-component of a Message Field.
%%%
%%% Since a Component Identifier is itself a Structured Field, it may also specify
%%% parameters, which are used to describe behavior such as which Message to
%%% derive a field or sub-component of the field, and how to encode the value as
%%% part of the signature base.
identifier_to_component(Identifier, Req, Res) when is_list(Identifier) ->
	identifier_to_component(list_to_binary(Identifier), Req, Res);
identifier_to_component(Identifier, Req, Res) when is_atom(Identifier) ->
	identifier_to_component(atom_to_binary(Identifier), Req, Res);
identifier_to_component(Identifier, Req, Res) when is_binary(Identifier) ->
	identifier_to_component(
        {item, {string, Identifier}, []},
        Req,
        Res
    );
identifier_to_component(ParsedIdentifier = {item, {X, Value}, Params}, Req, Res) ->
	case Value of
		<<$@, Rest/bits>> -> 
            extract_field({item, {X, Rest}, Params}, Req, Res);
		_ -> extract_field(ParsedIdentifier, Req, Res)
	end.

%%% @doc Given a Component Identifier and a Request/Response Messages Context
%%% extract the value represented by the Component Identifier, from the Messages
%%% Context, specifically a field on a Message within the Messages Context,
%%% and return the normalized form of the identifier, along with the extracted
%%% encoded value.
%%%
%%% This implements a portion of RFC-9421
%%% See https://datatracker.ietf.org/doc/html/rfc9421#name-http-fields
extract_field({item, {_Kind, IParsed}, IParams}, Req, Res) ->
	IsStrictFormat = find_strict_format_param(IParams),
	IsByteSequenceEncoded = find_byte_sequence_param(IParams),
	DictKey = find_key_param(IParams),
	case (IsStrictFormat orelse DictKey =/= false) andalso IsByteSequenceEncoded of
		true ->
			% https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.2.5.2.2
			{
                conflicting_params_error,
                <<
                    "Component Identifier parameter 'bs' MUST not ",
                    "be used with 'sf' or 'key'"
                >>
            };
		_ ->
			NormParsed = hb_ao:normalize_key(IParsed),
			NormalizedItem =
                hb_structured_fields:item(
                    {item, {string, NormParsed}, IParams}
                ),
            IsRequestIdentifier = find_request_param(IParams),
			% There may be multiple fields that match the identifier on the Msg,
			% so we filter, instead of find
            %?event({extracting_field, {identifier, Lowered}, {req, Req}, {res, Res}}),
			case maps:get(NormParsed, if IsRequestIdentifier -> Req; true -> Res end, not_found) of
				not_found ->
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.2.5.2.6
					{
                        field_not_found_error,
                        <<"Component Identifier for a field MUST be ",
                            "present on the message">>,
                        {key, NormParsed},
                        {req, Req},
                        {res, Res}
                    };
				FieldValue ->
					% The Field was found, but we still need to potentially
                    % parse it (it could be a Structured Field) and potentially
                    % extract subsequent values ie. specific dictionary key and
                    % its parameters, or further encode it
					case
						extract_field_value(
                            [FieldValue],
                            [DictKey, IsStrictFormat, IsByteSequenceEncoded])
					of
						{ok, Extracted} ->
                            {ok, {bin(NormalizedItem), bin(Extracted)}};
						E -> E
					end
			end
	end.

%%% @doc Extract values from the field and return the normalized field,
%%% along with encoded value
extract_field_value(RawFields, [Key, IsStrictFormat, IsByteSequenceEncoded]) ->
	% TODO: (maybe this already works?) empty string for empty header
	HasKey = case Key of false -> false; _ -> true end,
	case not (HasKey orelse IsStrictFormat orelse IsByteSequenceEncoded) of
        % No RFC-9421 parameterized encodings ie. "sf", "bs", "key"
        % (see https://datatracker.ietf.org/doc/html/rfc9421#section-2.1-17)
        % So simply normalize and return the field values.
        % 
        % This takes into account the list-based fields serialization
        % described in https://datatracker.ietf.org/doc/html/rfc9421#section-2.1-5
		true ->
			Normalized = [trim_and_normalize(Field) || Field <- RawFields],
			{ok, bin(lists:join(<<", ">>, Normalized))};
		_ ->
			case IsByteSequenceEncoded of
				% https://datatracker.ietf.org/doc/html/rfc9421#section-2.1.3-2
				true ->
					SfList = [
						{item, {binary, trim_and_normalize(Field)}, []}
					 || Field <- RawFields
					],
					sf_encode(SfList);
				_ ->
                    % In all cases, multiple fields MUST be combined
                    % into a single data structure, before serialization,
                    %
                    % See https://datatracker.ietf.org/doc/html/rfc9421#section-2.1.1-2
                    % And https://datatracker.ietf.org/doc/html/rfc9421#section-2.1.2-2
					Combined = bin(lists:join(<<", ">>, RawFields)),
					case sf_parse(Combined) of
						% https://datatracker.ietf.org/doc/html/rfc9421#section-2.1.1-3
						{error, _} ->
							{
                                sf_parsing_error,
                                <<"Component Identifier value could not ",
                                    "be parsed as a structured field">>
                            };
						{ok, SF} ->
							case HasKey of
                                % https://datatracker.ietf.org/doc/html/rfc9421#section-2.1.1
								false -> case IsStrictFormat of
                                    % just re-serialize, which should properly
                                    % format the data in Strict-Formatting style
                                    true -> sf_encode(SF);
                                    _ -> Combined
                                end;
								_ -> extract_dictionary_field_value(SF, Key)
							end
					end
			end
	end.

%%% @doc Extract a value from a Structured Field, and return the normalized field,
%%% along with the encoded value
extract_dictionary_field_value(StructuredField = [Elem | _Rest], Key) ->
	case Elem of
		{Name, _} when is_binary(Name) ->
			case lists:keyfind(Key, 1, StructuredField) of
				% https://datatracker.ietf.org/doc/html/rfc9421#section-2.1.2-5
				false ->
					{
                        sf_dicionary_key_not_found_error,
                        <<"Component Identifier references key not ",
                            "found in dictionary structured field">>,
                        {key, Key},
                        {structured_field, StructuredField}
                    };
				{_, Value} ->
					sf_encode(Value)
			end;
		_ ->
			{
                sf_not_dictionary_error,
                <<"Component Identifier cannot reference key on a ",
                    "non-dictionary structured field">>
            }
	end.

%%% @doc Given a Component Identifier and a Request/Response Messages Context
%%% extract the value represented by the Component Identifier, from the Messages
%%% Context, specifically a "Derived" Component within the Messages Context,
%%% and return the normalized form of the identifier, along with the extracted
%%% encoded value.
%%%
%%% This implements a portion of RFC-9421
%%% See https://datatracker.ietf.org/doc/html/rfc9421#name-derived-components
derive_component(Identifier, Req, Res) when map_size(Res) == 0 ->
	derive_component(Identifier, Req, Res, req);
derive_component(Identifier, Req, Res) ->
	derive_component(Identifier, Req, Res, res).
derive_component({item, {_Kind, IParsed}, IParams}, Req, Res, Subject) ->
	case find_request_param(IParams) andalso Subject =:= req of
		% https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.2.5.2.3
		true ->
			{
                req_identifier_error,
                <<"A Component Identifier may not contain a req parameter ",
                    "if the target is a request message">>
            };
		_ ->
			Lowered = lower_bin(IParsed),
			NormalizedItem =
                hb_structured_fields:item(
                    {item, {string, Lowered}, IParams}
                ),
			Result =
				case Lowered of
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.2.1
					<<"@method">> ->
						{ok, upper_bin(maps:get(<<"method">>, Req, <<>>))};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.4.1
					<<"@target-uri">> ->
						{ok, bin(maps:get(<<"path">>, Req, <<>>))};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.6.1
					<<"@authority">> ->
						URI = uri_string:parse(maps:get(<<"path">>, Req, <<>>)),
						Authority = maps:get(host, URI, <<>>),
						{ok, lower_bin(Authority)};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.8.1
					<<"@scheme">> ->
						URI = uri_string:parse(maps:get(<<"path">>, Req)),
						Scheme = maps:get(scheme, URI, <<>>),
						{ok, lower_bin(Scheme)};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.10.1
					<<"@request-target">> ->
						URI = uri_string:parse(maps:get(<<"path">>, Req)),
						% If message contains the absolute form value, then
						% the value must be the absolut url
						%
						% TODO: maybe better way to distinguish besides a flag
						% on the request?
						%
						% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.5-10
						RequestTarget =
							case maps:get(is_absolute_form, Req, false) of
								true -> maps:get(url, Req);
								_ ->
                                    lists:join($?,
                                        [
                                            maps:get(path, URI, <<>>),
                                            maps:get(query, URI, ?EMPTY_QUERY_PARAMS)
                                        ]
                                    )
							end,
						{ok, bin(RequestTarget)};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.12.1
					<<"@path">> ->
						URI = uri_string:parse(maps:get(<<"path">>, Req)),
						Path = maps:get(path, URI),
						{ok, bin(Path)};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.14.1
					<<"@query">> ->
						URI = uri_string:parse(maps:get(<<"path">>, Req)),
						% No query params results in a "?" value
						% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.7-14
						Query =
							case maps:get(query, URI, <<>>) of
								<<>> -> ?EMPTY_QUERY_PARAMS;
								Q -> Q
							end,
						{ok, bin(Query)};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.16.1
					<<"@query-param">> ->
						case find_name_param(IParams) of
							% The name parameter MUST be provided when specifiying a @query-param
							% Derived Component. See https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.8-1
							false ->
								{
                                    req_identifier_error,
                                    <<"@query_param Derived Component Identifier ",
                                    "must specify a name parameter">>
                                };
							Name ->
								URI = uri_string:parse(maps:get(<<"path">>, Req)),
								QueryParams =
                                    uri_string:dissect_query(maps:get(query, URI, "")),
								QueryParam =
									case lists:keyfind(Name, 1, QueryParams) of
										{_, QP} -> QP;
										% An missing or empty query param value results in
										% an empty string value in the signature base
										% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.8-4
										_ -> ""
									end,
								{ok, bin(QueryParam)}
						end;
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.18.1
					<<"@status">> ->
						case Subject =:= req of
							% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.9-8
							true ->
								{
                                    res_identifier_error,
                                    <<"@status Derived Component must not be ",
                                    "used if target is a request message">>
                                };
							_ ->
								Status = maps:get(<<"status">>, Res, <<"200">>),
								{ok, Status}
						end
				end,
            ?event({derive_component, IParsed, Result}),
			case Result of
				{ok, V} ->
                    ?event({derive_component, IParsed, {ok, V}}),
                    {ok, {bin(NormalizedItem), V}};
				E -> E
			end
	end.

%%%
%%% Strucutured Field Utilities
%%%

%%% @doc construct the structured field Parameter for the signature parameter,
%%% checking whether the parameter name is valid according RFC-9421
%%% 
%%% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.3-3
sf_signature_param({Name, Param}) ->
    NormalizedName = bin(Name),
    NormalizedNames = lists:map(fun bin/1, ?SIGNATURE_PARAMS),
    case lists:member(NormalizedName, NormalizedNames) of
        false -> {unknown_signature_param, NormalizedName};
        % all signature params are either integer or string values
        true -> case Param of
            I when is_integer(I) -> {ok, {NormalizedName, Param}};
            P when is_atom(P) orelse is_list(P) orelse is_binary(P) ->
                {ok, {NormalizedName, {string, bin(P)}}};
            P -> {invalid_signature_param_value, P}
        end
    end.

%%% @doc construct the structured field List for the
%%% "signature-params-line" part of the signature base.
%%% 
%%% Can be parsed into a binary by simply passing to hb_structured_fields:list/1
%%%
%%% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.3.2.4
sf_signature_params(ComponentIdentifiers, SigParams) when is_map(SigParams) ->
    AsList = maps:to_list(SigParams),
    Sorted = lists:sort(fun({Key1, _}, {Key2, _}) -> Key1 < Key2 end, AsList),
    sf_signature_params(ComponentIdentifiers, Sorted);
sf_signature_params(ComponentIdentifiers, SigParams) when is_list(SigParams) ->
    [
		{
			list,
			lists:map(
                fun(ComponentIdentifier) ->
					{item, {_Kind, Value}, Params} = sf_item(ComponentIdentifier),
					{item, {string, lower_bin(Value)}, Params}
				end,
                ComponentIdentifiers
            ),
			lists:foldl(
                fun (RawParam, Params) ->
                    case sf_signature_param(RawParam) of
                        {ok, Param} -> Params ++ [Param];
                        % Ignore unknown signature parameters
                        {unknown_signature_param, _} -> Params
                        % TODO: what to do about invalid_signature_param_value?
                        % For now will cause badmatch
                    end
                end,
                [],
                SigParams  
            )
		}
	].

%%% @doc Attempt to parse the binary into a data structure that represents
%%% an HTTP Structured Field.
%%%
%%% Lacking some sort of "hint", there isn't a way to know which "kind" of
%%% Structured Field the binary is, apriori. So we simply try each parser,
%%% and return the first invocation that doesn't result in an error.
%%%
%%% If no parser is successful, then we return an error tuple
sf_parse(Raw) when is_list(Raw) -> sf_parse(list_to_binary(Raw));
sf_parse(Raw) when is_binary(Raw) ->
	Parsers = [
		fun hb_structured_fields:parse_list/1,
		fun hb_structured_fields:parse_dictionary/1,
		fun hb_structured_fields:parse_item/1
	],
	sf_parse(Parsers, Raw).

sf_parse([], _Raw) ->
    {error, undefined};
sf_parse([Parser | Rest], Raw) ->
    case catch Parser(Raw) of
        % skip parsers that fail
        {'EXIT', _} -> sf_parse(Rest, Raw);
        Parsed -> {ok, Parsed}
    end.

%%% @doc Attempt to encode the data structure into an HTTP Structured Field.
%%% This is the inverse of sf_parse.
sf_encode(StructuredField = {list, _, _}) ->
	% The value is an inner_list, and so needs to be wrapped with an outer list
	% before being serialized
	sf_encode(fun hb_structured_fields:list/1, [StructuredField]);
sf_encode(StructuredField = {item, _, _}) ->
	sf_encode(fun hb_structured_fields:item/1, StructuredField);
sf_encode(StructuredField = [Elem | _Rest]) ->
	sf_encode(
		% Both an sf list and dictionary is represented in Erlang as a List of
        % pairs but a dictionary's members will always be a pair whose first value
		% is a binary, so we can match on that to determine which serializer to use
		case Elem of
			{Name, _} when is_binary(Name) ->
                fun hb_structured_fields:dictionary/1;
			_ ->
                fun hb_structured_fields:list/1
		end,
		StructuredField
	).
sf_encode(Serializer, StructuredField) ->
	case catch Serializer(StructuredField) of
		{'EXIT', _} -> {error, <<"Could not serialize into structured field">>};
		Parsed -> {ok, Parsed}
	end.

%%% @doc Attempt to parse the provided value into an HTTP Structured Field Item
sf_item(SfItem = {item, {_Kind, _Parsed}, _Params}) ->
	SfItem;
sf_item(ComponentIdentifier) when is_list(ComponentIdentifier) ->
	sf_item(list_to_binary(ComponentIdentifier));
sf_item(ComponentIdentifier) when is_binary(ComponentIdentifier) ->
    {item, {string, ComponentIdentifier}, []}.

%%% @doc Given a parameter Name, extract the Parameter value from the HTTP
%%% Structured Field data structure.
%%%
%%% If no value is found, then false is returned
find_sf_param(Name, Params, Default) when is_list(Name) ->
    find_sf_param(list_to_binary(Name), Params, Default);
find_sf_param(Name, Params, Default) ->
	% [{<<"name">>,{string,<<"baz">>}}]
	case lists:keyfind(Name, 1, Params) of
		false -> Default;
		{_, {string, Value}} -> Value;
		{_, {token, Value}} -> Value;
		{_, {binary, Value}} -> Value;
		{_, Value} -> Value
	end.

%%%
%%% https://datatracker.ietf.org/doc/html/rfc9421#section-6.5.2-1
%%% using functions allows encapsulating default values
%%%
find_strict_format_param(Params) -> find_sf_param(<<"sf">>, Params, false).
find_key_param(Params) -> find_sf_param(<<"key">>, Params, false).
find_byte_sequence_param(Params) -> find_sf_param(<<"bs">>, Params, false).
find_trailer_param(Params) -> find_sf_param(<<"tr">>, Params, false).
find_request_param(Params) -> find_sf_param(<<"req">>, Params, false).
find_name_param(Params) -> find_sf_param(<<"name">>, Params, false).

%%%
%%% Data Utilities
%%%

% https://datatracker.ietf.org/doc/html/rfc9421#section-2.1-5
trim_and_normalize(Bin) ->
	binary:replace(trim_ws(Bin), <<$\n>>, <<" ">>, [global]).

upper_bin(Item) when is_atom(Item) -> upper_bin(atom_to_list(Item));
upper_bin(Item) when is_binary(Item) -> upper_bin(binary_to_list(Item));
upper_bin(Item) when is_list(Item) -> bin(string:uppercase(Item)).

lower_bin(Item) when is_atom(Item) -> lower_bin(atom_to_list(Item));
lower_bin(Item) when is_binary(Item) -> lower_bin(binary_to_list(Item));
lower_bin(Item) when is_list(Item) -> bin(string:lowercase(Item)).

bin(Item) when is_atom(Item) -> atom_to_binary(Item, utf8);
bin(Item) when is_integer(Item) ->
    case Item of
        % Treat integer as an ASCII code
        N when N > 0 andalso N < 256 -> <<N>>;
        N -> integer_to_binary(N)
    end;
bin(Item) ->
    iolist_to_binary(Item).

%%% @doc Recursively trim space characters from the beginning of the binary
trim_ws(<<$\s, Bin/bits>>) -> trim_ws(Bin);
%%% @doc No space characters at the beginning so now trim them from the end
%%% recrusively
trim_ws(Bin) -> trim_ws_end(Bin, byte_size(Bin) - 1).

trim_ws_end(_, -1) ->
	<<>>;
trim_ws_end(Value, N) ->
	case binary:at(Value, N) of
		$\s ->
			trim_ws_end(Value, N - 1);
		% No more space characters matches on the end
		% So extract the bytes up to N, and this is our trimmed value
		_ ->
			S = N + 1,
			<<Trimmed:S/binary, _/bits>> = Value,
			Trimmed
	end.

%%%
%%% TESTS
%%%

%%% Integration Tests

%% @doc Ensure that we can validate a signature on an extremely large and complex
%% message that is sent over HTTP, signed with the codec.
validate_large_message_from_http_test() ->
    Node = hb_http_server:start_node(#{
        force_signed => true,
        commitment_device => <<"httpsig@1.0">>,
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
    {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
    Signers = hb_message:signers(Res),
    ?event({received, {signers, Signers}, {res, Res}}),
    ?assert(length(Signers) == 1),
    ?assert(hb_message:verify(Res, Signers)),
    ?event({sig_verifies, Signers}),
    ?assert(hb_message:verify(Res)),
    ?event({hmac_verifies, <<"hmac-sha256">>}),
    {ok, OnlyCommitted} = hb_message:with_only_committed(Res),
    ?event({msg_with_only_committed, OnlyCommitted}),
    ?assert(hb_message:verify(OnlyCommitted, Signers)),
    ?event({msg_with_only_committed_verifies, Signers}),
    ?assert(hb_message:verify(OnlyCommitted)),
    ?event({msg_with_only_committed_verifies_hmac, <<"hmac-sha256">>}).

committed_id_test() ->
    Msg = #{ <<"basic">> => <<"value">> },
    Signed = hb_message:commit(Msg, hb:wallet()),
    ?event({signed_msg, Signed}),
    UnsignedID = hb_message:id(Signed, none),
    SignedID = hb_message:id(Signed, all),
    ?event({ids, {unsigned_id, UnsignedID}, {signed_id, SignedID}}),
    ?assertNotEqual(UnsignedID, SignedID).

multicommitted_id_test() ->
    Msg = #{ <<"basic">> => <<"value">> },
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

%%% Unit Tests
trim_ws_test() ->
	?assertEqual(<<"hello world">>, trim_ws(<<"      hello world      ">>)),
	?assertEqual(<<>>, trim_ws(<<"">>)),
	?assertEqual(<<>>, trim_ws(<<"         ">>)),
	ok.

join_signature_base_test() ->
	ParamsLine =
		<<"(\"@method\" \"@path\" \"foo\";req \"foo\";key=\"a\");"
		    "created=1733165109501;nonce=\"foobar\";keyid=\"key1\"">>,
	ComponentsLine = <<"\"@method\": GET\n\"@path\": /id-123/Data\n\"foo\";"
        "req: req-b-bar\n\"foo\";key=\"a\": 1">>,
	?assertEqual(
		<<
            ComponentsLine/binary,
            <<"\n">>/binary,
            <<"\"@signature-params\": ">>/binary,
            ParamsLine/binary
        >>,
		join_signature_base(ComponentsLine, ParamsLine)
	).

signature_params_line_test() ->
	Params = #{created => 1733165109501, nonce => "foobar", keyid => "key1"},
	ContentIdentifiers = [
		<<"Content-Length">>,
        <<"@method">>,
        <<"@Path">>,
        <<"content-type">>,
        <<"example-dict">>
	],
	Result = signature_params_line(ContentIdentifiers, Params),
	?assertEqual(
        <<
            "(\"content-length\" \"@method\" \"@path\" \"content-type\" \"example-dict\")"
            ";created=1733165109501;keyid=\"key1\";nonce=\"foobar\""
        >>,
		Result
	).

derive_component_error_req_param_on_request_target_test() ->
	Result =
        derive_component(
            {item, {string, <<"@query-param">>}, [{<<"req">>, true}]},
            #{}, #{}, req),
	?assertMatch(
		{req_identifier_error, _},
		Result
	).

derive_component_error_query_param_no_name_test() ->
	Result =
        derive_component(
            {item,
                {string, <<"@query-param">>},
                [{<<"noname">>, {string, <<"foo">>}}]
            }, #{}, #{}, req),
	?assertMatch(
		{req_identifier_error, _},
		Result
	).

derive_component_error_status_req_target_test() ->
	Result = derive_component({item, {string, <<"@status">>}, []}, #{}, #{}, req),
	{E, _M} = Result,
	?assertEqual(res_identifier_error, E).
