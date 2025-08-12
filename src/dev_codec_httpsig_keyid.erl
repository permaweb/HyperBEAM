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
-export([find_scheme/3, apply_scheme/3]). %% Export for testing
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
    %% Validate that the scheme in the request matches the scheme in the keyid.
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
                %% Determine the default scheme based on the `type' of the request.
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
    %% Remove the `publickey:' prefix from the keyid and return the key.
    %% Use hb_util:decode to handle both base64 and base64url encodings
    PubKey = hb_util:decode(remove_scheme_prefix(KeyID)),
    %% Return the original KeyID to preserve the encoding format
    {ok, PubKey, KeyID};
apply_scheme(constant, RawKeyID, _Req) ->
    %% In the `constant' scheme, the key is simply the key itself, including the
    %% `constant:' prefix if given.
    KeyID =
        if RawKeyID == undefined -> ?HMAC_DEFAULT_KEY;
        true -> RawKeyID
        end,
    {ok, KeyID, KeyID};
apply_scheme(secret, _KeyID, Req) ->
    %% In the `secret' scheme, the key is hashed to generate a keyid.
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
    %% Note: There is a subtlety here. The `KeyID' is decoded with the
    %% `hb_util:decode' function rather than `base64:decode'. The reason for this
    %% is that certain codecs (e.g. `ans104@1.0') encode the public key with
    %% `base64url' encoding, rather than the standard `base64' encoding in
    %% HTTPSig. Our `hb_util:decode' function handles both cases returning the
    %% same raw bytes, and is subsequently safe.
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

%%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test that demonstrates the base64url issue in apply_scheme
req_to_key_material_base64url_test() ->
    %% Use a real public key that contains base64url characters (- and _)
    Base64UrlKey = <<"sdJIA2uM5b7huNjUHfiq3J1yEhi82uVC2rnKAUMhWkRJNkwXU9gGn25wEcGvH4ibuG-GRdPLHySv_Jcfg3bWCsAwD7O7zudMQwNtHclS5XTaVRk6WoQZkF4sF5Kvf-Pm4puSsyevgQYRXx9SNgAEFMjEgFk3i9gAHevIYBg70ZRfuPx0Dj3It2qYWTLU8cdNRfmklr0SoXkdYBBVy50vNMBQmOe4ys2OeCiKS7jQkiRbf3UApxVOFJimP8kEUjqwbxI1zQLK_BrUG_K-RFolYd26_WnvvntRytPMGbYCjoPIxBn0BhGKAU0x9yN1774wbv-xsh0db6LfQAVMswo5opCmjsig5r0EuVbFmP_UXmYiu2YkobvJ_hmhPUrXYwGQI2IYHawYVlyfJLpqsEycM3CZQf3Ecxbp5HGWg6a3JMh0sBqRuY6CC9FXopkh32NxmWsE3sbjZDNuWkKEuQzxAva44mvUQ-vzlEkoZhVq0Uh5m0eQSe1GXqUBEMoEQFr2JMV52zzsawXBlA_qVxyRqP9ULWudYYb7NImLTHnFTqePh6_WaJ8mS1zKwh0G171dHunVZbu97zZo1lJLs2Gd5oRZ-V1K10rmik7mSAFKYg4SFfWmzzxssLfVfIAxGCtUO_33NQ9s5lH9fZLfqSbEimDk7VOwhrhr_p80wg7-8Ls">>,
    
    KeyID = <<"publickey:", Base64UrlKey/binary>>,
    
    %% Create a request with RSA-PSS signature type
    Req = #{
        <<"keyid">> => KeyID,
        <<"type">> => <<"rsa-pss-sha512">>
    },
    
    %% With the fix using hb_util:decode, this should now work with base64url
    Result = req_to_key_material(Req, #{}),
    ?assertMatch({ok, publickey, _, _}, Result),
    {ok, publickey, DecodedKey, ReturnedKeyID} = Result,
    %% Should successfully decode the base64url key
    ?assert(is_binary(DecodedKey)),
    ?assertEqual(KeyID, ReturnedKeyID).

%% Test that keyid_to_committer handles base64url correctly
keyid_to_committer_base64url_test() ->
    %% Use a real public key that contains base64url characters (- and _)
    Base64UrlKey = <<"sdJIA2uM5b7huNjUHfiq3J1yEhi82uVC2rnKAUMhWkRJNkwXU9gGn25wEcGvH4ibuG-GRdPLHySv_Jcfg3bWCsAwD7O7zudMQwNtHclS5XTaVRk6WoQZkF4sF5Kvf-Pm4puSsyevgQYRXx9SNgAEFMjEgFk3i9gAHevIYBg70ZRfuPx0Dj3It2qYWTLU8cdNRfmklr0SoXkdYBBVy50vNMBQmOe4ys2OeCiKS7jQkiRbf3UApxVOFJimP8kEUjqwbxI1zQLK_BrUG_K-RFolYd26_WnvvntRytPMGbYCjoPIxBn0BhGKAU0x9yN1774wbv-xsh0db6LfQAVMswo5opCmjsig5r0EuVbFmP_UXmYiu2YkobvJ_hmhPUrXYwGQI2IYHawYVlyfJLpqsEycM3CZQf3Ecxbp5HGWg6a3JMh0sBqRuY6CC9FXopkh32NxmWsE3sbjZDNuWkKEuQzxAva44mvUQ-vzlEkoZhVq0Uh5m0eQSe1GXqUBEMoEQFr2JMV52zzsawXBlA_qVxyRqP9ULWudYYb7NImLTHnFTqePh6_WaJ8mS1zKwh0G171dHunVZbu97zZo1lJLs2Gd5oRZ-V1K10rmik7mSAFKYg4SFfWmzzxssLfVfIAxGCtUO_33NQ9s5lH9fZLfqSbEimDk7VOwhrhr_p80wg7-8Ls">>,
    
    KeyID = <<"publickey:", Base64UrlKey/binary>>,
    
    %% keyid_to_committer uses hb_util:decode which handles base64url
    %% This should work without errors
    Result = keyid_to_committer(KeyID),
    
    %% Should return an Arweave address
    ?assert(is_binary(Result)),
    ?assertEqual(43, byte_size(Result)). %% Arweave addresses are 43 bytes

%% Test standard base64 encoding works
req_to_key_material_base64_test() ->
    TestKey = crypto:strong_rand_bytes(32),
    Base64Key = base64:encode(TestKey),
    KeyID = <<"publickey:", Base64Key/binary>>,
    
    Req = #{
        <<"keyid">> => KeyID,
        <<"type">> => <<"rsa-pss-sha512">>
    },
    
    %% This should work with standard base64
    Result = req_to_key_material(Req, #{}),
    
    ?assertMatch({ok, publickey, _, _}, Result),
    {ok, publickey, DecodedKey, ReturnedKeyID} = Result,
    ?assertEqual(TestKey, DecodedKey),
    ?assertEqual(KeyID, ReturnedKeyID).

-endif.
