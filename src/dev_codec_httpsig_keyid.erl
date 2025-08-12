%%% @doc A library for extracting and validating key material for `httpsig@1.0'
%%% requests. Offers support for the following keyid schemes:
%%% - `publickey': The keyid is an encoded public key with the `publickey:' prefix.
%%% - `constant': The key is simply the keyid itself, including the `public:'
%%%   prefix if given.
%%% - `secret': The key is hashed and the `secret:' prefix is added to the
%%%   result in order to generate a keyid.
%%% 
%%% These functions are abstracted in order to allow for the addition of new
%%% schemes in the future.
-module(dev_codec_httpsig_keyid).
-export([req_to_key_material/2, keyid_to_committer/1, keyid_to_committer/2]).
-export([secret_key_to_committer/1, remove_scheme_prefix/1]).
-include_lib("include/hb.hrl").

%%% The supported schemes for HMAC keys.
-define(KEYID_SCHEMES, [constant, publickey, secret]).
%%% The default schemes for each request type.
-define(DEFAULT_SCHEMES_BY_TYPE, #{
    <<"rsa-pss-sha512">> => publickey,
    <<"hmac-sha256">> => constant
}).
%%% Default key to use for HMAC commitments.
-define(HMAC_DEFAULT_KEY, <<"constant:ao">>).

%% @doc Extract the key and keyid from a request, returning
%% `{ok, Scheme, Key, KeyID}' or `{error, Reason}'.
req_to_key_material(Req, Opts) ->
    ?event({req_to_key_material, {req, Req}}),
    KeyID = maps:get(<<"keyid">>, Req, undefined),
    ?event({keyid_to_key_material, {keyid, KeyID}}),
    case find_scheme(KeyID, Req, Opts) of
        {ok, Scheme} ->
            ?event({scheme_found, {scheme, Scheme}}),
            ApplyRes = apply_scheme(Scheme, KeyID, Req),
            ?event({apply_scheme_result, {apply_res, ApplyRes}}),
            case ApplyRes of
                {ok, _, CalcKeyID} when KeyID /= undefined, CalcKeyID /= KeyID ->
                    {error, key_mismatch};
                {ok, Key, CalcKeyID} ->
                    {ok, Scheme, Key, CalcKeyID};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, undefined_scheme} ->
            {ok, DefaultScheme} = req_to_default_scheme(Req, Opts),
            req_to_key_material(Req#{ <<"scheme">> => DefaultScheme }, Opts);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Find the scheme from a keyid or request. Returns `{ok, Scheme}' or
%% `{error, Reason}'. If no scheme is provided in either the request message
%% or the keyid (as a `scheme:' prefix), we default to the scheme specified in
%% the request type. If a scheme is provided in the request, it must match the
%% scheme in the keyid if also present.
find_scheme(KeyID, Req = #{ <<"scheme">> := RawScheme }, Opts) ->
    Scheme = hb_util:atom(RawScheme),
    % Validate that the scheme in the request matches the scheme in the keyid.
    case find_scheme(KeyID, maps:without([<<"scheme">>], Req), Opts) of
        {ok, Scheme} -> {ok, Scheme};
        {error, undefined_scheme} -> {ok, Scheme};
        _OtherScheme -> {error, scheme_mismatch}
    end;
find_scheme(undefined, _Req, _Opts) ->
    {error, undefined_scheme};
find_scheme(KeyID, Req, Opts) ->
    SchemeRes =
        case binary:split(KeyID, <<":">>) of
            [SchemeBin, _KeyID] -> {ok, SchemeBin};
            [_NoSchemeKeyID] ->
                % Determine the default scheme based on the `type' of the request.
                req_to_default_scheme(Req, Opts)
        end,
    case SchemeRes of
        {ok, Scheme} ->
            case lists:member(SchemeAtom = hb_util:atom(Scheme), ?KEYID_SCHEMES) of
                true -> {ok, SchemeAtom};
                false -> {error, unknown_scheme}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Determine the default scheme based on the `type' of the request.
req_to_default_scheme(Req, _Opts) ->
    case maps:find(<<"type">>, Req) of
        {ok, Type} ->
            case maps:find(Type, ?DEFAULT_SCHEMES_BY_TYPE) of
                {ok, Scheme} -> {ok, Scheme};
                error -> {error, unsupported_scheme}
            end;
        error ->
            {error, no_request_type}
    end.

%% @doc Apply the requested scheme to generate the key material (key and keyid).
apply_scheme(publickey, KeyID, _Req) ->
    % Remove the `publickey:' prefix from the keyid and return the key.
    PubKey = base64:decode(remove_scheme_prefix(KeyID)),
    {ok, PubKey, << "publickey:", (base64:encode(PubKey))/binary >>};
apply_scheme(constant, RawKeyID, _Req) ->
    % In the `constant' scheme, the key is simply the key itself, including the
    % `constant:' prefix if given.
    KeyID =
        if RawKeyID == undefined -> ?HMAC_DEFAULT_KEY;
        true -> RawKeyID
        end,
    {ok, KeyID, KeyID};
apply_scheme(secret, _KeyID, Req) ->
    % In the `secret' scheme, the key is hashed to generate a keyid.
    Secret = maps:get(<<"secret">>, Req, undefined),
    Committer = secret_key_to_committer(Secret),
    {ok, Secret, << "secret:", Committer/binary >>};
apply_scheme(_Scheme, _Key, _KeyID) ->
    {error, unsupported_scheme}.

%% @doc Given a keyid and a scheme, generate the committer value for a commitment.
%% Returns `BinaryAddress' or `undefined' if the keyid implies no committer.
keyid_to_committer(KeyID) ->
    case find_scheme(KeyID, #{}, #{}) of
        {ok, Scheme} -> keyid_to_committer(Scheme, KeyID);
        {error, _} -> undefined
    end.
keyid_to_committer(publickey, KeyID) ->
    % Note: There is a subtlety here. The `KeyID' is decoded with the 
    % `hb_util:decode' function rather than `base64:decode'. The reason for this
    % is that certain codecs (e.g. `ans104@1.0') encode the public key with
    % `base64url' encoding, rather than the standard `base64' encoding in 
    % HTTPSig. Our `hb_util:decode' function handles both cases returning the
    % same raw bytes, and is subsequently safe.
    hb_util:human_id(
        ar_wallet:to_address(
            hb_util:decode(remove_scheme_prefix(KeyID))
        )
    );
keyid_to_committer(secret, KeyID) ->
    remove_scheme_prefix(KeyID);
keyid_to_committer(constant, _KeyID) ->
    undefined.

%% @doc Given a secret key, generate the committer value for a commitment.
secret_key_to_committer(Key) ->
    hb_util:human_id(hb_crypto:sha256(Key)).

%% @doc Remove the `scheme:' prefix from a keyid.
remove_scheme_prefix(KeyID) ->
    case binary:split(KeyID, <<":">>) of
        [_Scheme, Key] -> Key;
        [Key] -> Key
    end.
