%%% @doc ACME cryptography module.
%%%
%%% This module provides cryptographic operations for ACME (Automatic Certificate
%%% Management Environment) protocol implementation. It handles RSA key generation,
%%% JWK (JSON Web Key) operations, JWS (JSON Web Signature) creation, and various
%%% encoding/decoding utilities required for secure ACME communication.
-module(hb_acme_crypto).

-include_lib("public_key/include/public_key.hrl").

%% Public API
-export([
    private_key_to_jwk/1,
    get_jwk_thumbprint/1,
    generate_key_authorization/2,
    generate_dns_txt_value/1,
    base64url_encode/1,
    base64url_decode/1,
    create_jws_header/4,
    create_jws_signature/3,
    sign_data/3
]).

%% Type specifications
-spec private_key_to_jwk(public_key:private_key()) -> map().
-spec get_jwk_thumbprint(public_key:private_key()) -> string().
-spec generate_key_authorization(string(), public_key:private_key()) -> string().
-spec generate_dns_txt_value(string()) -> string().
-spec base64url_encode(binary() | string()) -> string().
-spec base64url_decode(string()) -> binary().
-spec create_jws_header(string(), public_key:private_key(), string() | undefined, string()) -> map().
-spec create_jws_signature(string(), string(), public_key:private_key()) -> string().
-spec sign_data(binary() | string(), atom(), public_key:private_key()) -> binary().

%% @doc Converts an RSA private key to JWK (JSON Web Key) format.
%%
%% This function extracts the public key components (modulus and exponent)
%% from an RSA private key and formats them according to RFC 7517 JWK
%% specification for use in ACME protocol communication.
%%
%% @param PrivateKey The RSA private key record
%% @returns A map representing the JWK with required fields
private_key_to_jwk(#'RSAPrivateKey'{modulus = N, publicExponent = E}) ->
    #{
        <<"kty">> => <<"RSA">>,
        <<"n">> => hb_util:bin(base64url_encode(binary:encode_unsigned(N))),
        <<"e">> => hb_util:bin(base64url_encode(binary:encode_unsigned(E)))
    }.

%% @doc Computes the JWK thumbprint for an RSA private key.
%%
%% This function creates a JWK thumbprint according to RFC 7638, which is
%% used in ACME protocol for key identification and challenge generation.
%% The thumbprint is computed by hashing the canonical JSON representation
%% of the JWK.
%%
%% @param PrivateKey The RSA private key
%% @returns The base64url-encoded JWK thumbprint as string
get_jwk_thumbprint(PrivateKey) ->
    Jwk = private_key_to_jwk(PrivateKey),
    JwkJson = hb_json:encode(Jwk),
    Hash = crypto:hash(sha256, JwkJson),
    base64url_encode(Hash).

%% @doc Generates the key authorization string for a challenge.
%%
%% This function creates the key authorization string required for ACME
%% challenges by concatenating the challenge token with the JWK thumbprint.
%% This is used in DNS-01 and other challenge types.
%%
%% @param Token The challenge token from the ACME server
%% @param PrivateKey The account's private key
%% @returns The key authorization string (Token.JWK_Thumbprint)
generate_key_authorization(Token, PrivateKey) ->
    Thumbprint = get_jwk_thumbprint(PrivateKey),
    Token ++ "." ++ Thumbprint.

%% @doc Generates the DNS TXT record value from key authorization.
%%
%% This function creates the value that should be placed in a DNS TXT record
%% for DNS-01 challenge validation. It computes the SHA-256 hash of the
%% key authorization string and encodes it using base64url.
%%
%% @param KeyAuthorization The key authorization string
%% @returns The base64url-encoded SHA-256 hash for the DNS TXT record
generate_dns_txt_value(KeyAuthorization) ->
    Hash = crypto:hash(sha256, KeyAuthorization),
    base64url_encode(Hash).

%% @doc Encodes data using base64url encoding.
%%
%% This function implements base64url encoding as specified in RFC 4648,
%% which is required for JWS and other ACME protocol components. It differs
%% from standard base64 by using URL-safe characters and omitting padding.
%%
%% @param Data The data to encode (binary or string)
%% @returns The base64url-encoded string
base64url_encode(Data) when is_binary(Data) ->
    base64url_encode(binary_to_list(Data));
base64url_encode(Data) when is_list(Data) ->
    Encoded = base64:encode(Data),
    % Convert to URL-safe base64
    NoPlus = string:replace(Encoded, "+", "-", all),
    NoSlash = string:replace(NoPlus, "/", "_", all),
    string:replace(NoSlash, "=", "", all).

%% @doc Decodes base64url encoded data.
%%
%% This function decodes base64url encoded strings back to binary data.
%% It handles the URL-safe character set and adds padding if necessary.
%%
%% @param Data The base64url-encoded string
%% @returns The decoded binary data
base64url_decode(Data) when is_list(Data) ->
    % Convert from URL-safe base64
    WithPlus = string:replace(Data, "-", "+", all),
    WithSlash = string:replace(WithPlus, "_", "/", all),
    % Add padding if necessary
    PaddedLength = 4 * ((length(WithSlash) + 3) div 4),
    Padding = lists:duplicate(PaddedLength - length(WithSlash), $=),
    Padded = WithSlash ++ Padding,
    base64:decode(Padded).

%% @doc Creates a JWS header for ACME requests.
%%
%% This function creates the protected header for JWS (JSON Web Signature)
%% requests as required by the ACME protocol. It handles both new account
%% creation (using JWK) and existing account requests (using KID).
%%
%% @param Url The target URL for the request
%% @param PrivateKey The account's private key
%% @param Kid The account's key identifier (undefined for new accounts)
%% @param Nonce The fresh nonce from the ACME server
%% @returns A map representing the JWS header
create_jws_header(Url, PrivateKey, Kid, Nonce) ->
    BaseHeader = #{
        <<"alg">> => <<"RS256">>,
        <<"nonce">> => hb_util:bin(Nonce),
        <<"url">> => hb_util:bin(Url)
    },
    case Kid of
        undefined ->
            BaseHeader#{<<"jwk">> => private_key_to_jwk(PrivateKey)};
        _ ->
            BaseHeader#{<<"kid">> => hb_util:bin(Kid)}
    end.

%% @doc Creates a JWS signature for the given header and payload.
%%
%% This function creates a JWS signature by signing the concatenated
%% base64url-encoded header and payload with the private key using
%% RS256 (RSA with SHA-256).
%%
%% @param HeaderB64 The base64url-encoded header
%% @param PayloadB64 The base64url-encoded payload
%% @param PrivateKey The private key for signing
%% @returns The base64url-encoded signature
create_jws_signature(HeaderB64, PayloadB64, PrivateKey) ->
    SigningInput = HeaderB64 ++ "." ++ PayloadB64,
    Signature = public_key:sign(SigningInput, sha256, PrivateKey),
    base64url_encode(Signature).

%% @doc Signs data with the specified algorithm and private key.
%%
%% This function provides a general-purpose signing interface for
%% various cryptographic operations needed in ACME protocol.
%%
%% @param Data The data to sign (binary or string)
%% @param Algorithm The signing algorithm (e.g., sha256)
%% @param PrivateKey The private key for signing
%% @returns The signature as binary
sign_data(Data, Algorithm, PrivateKey) when is_list(Data) ->
    sign_data(list_to_binary(Data), Algorithm, PrivateKey);
sign_data(Data, Algorithm, PrivateKey) when is_binary(Data) ->
    public_key:sign(Data, Algorithm, PrivateKey).
