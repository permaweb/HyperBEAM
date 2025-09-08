%%% @doc ACME client module for Let's Encrypt certificate management.
%%%
%%% This module implements the ACME v2 protocol for automated certificate
%%% issuance and management with Let's Encrypt. It handles account creation,
%%% certificate orders, DNS-01 challenges, and certificate finalization.
%%%
%%% The module supports both staging and production Let's Encrypt environments
%%% and provides comprehensive logging through HyperBEAM's event system.
-module(hb_acme_client).
-export([create_account/1, request_certificate/2, get_dns_challenge/2]).
-export([validate_challenge/2, finalize_order/2]).
-export([download_certificate/2, base64url_encode/1]).
-export([get_nonce/0, get_fresh_nonce/1]).
-export([determine_directory_from_url/1, extract_host_from_url/1]).
-export([extract_base_url/1, extract_path_from_url/1]).

-include_lib("public_key/include/public_key.hrl").
-include("include/hb.hrl").

%% ACME server URLs
-define(LETS_ENCRYPT_STAGING, 
    "https://acme-staging-v02.api.letsencrypt.org/directory").
-define(LETS_ENCRYPT_PROD, 
    "https://acme-v02.api.letsencrypt.org/directory").

%% Record definitions
-record(acme_account, {
    key :: public_key:private_key(),
    url :: string(),
    kid :: string()
}).

-record(acme_order, {
    url :: string(),
    status :: string(),
    expires :: string(),
    identifiers :: list(),
    authorizations :: list(),
    finalize :: string(),
    certificate :: string()
}).

-record(dns_challenge, {
    domain :: string(),
    token :: string(),
    key_authorization :: string(),
    dns_value :: string(),
    url :: string()
}).

%% @doc Creates a new ACME account with Let's Encrypt.
%%
%% This function performs the following operations:
%% 1. Determines the ACME directory URL based on environment (staging/prod)
%% 2. Generates a new RSA key pair for the ACME account
%% 3. Retrieves the ACME directory to get service endpoints
%% 4. Creates a new account by agreeing to terms of service
%% 5. Returns an account record with key, URL, and key identifier
%%
%% Required configuration in Config map:
%% - environment: 'staging' or 'production' 
%% - email: Contact email for the account
%% - key_size: RSA key size (typically 2048 or 4096)
%%
%% @param Config A map containing account creation parameters
%% @returns {ok, Account} on success with account details, or
%% {error, Reason} on failure with error information
create_account(Config) ->
    #{
        environment := Environment,
        email := Email,
        key_size := KeySize
    } = Config,
    ?event({acme_account_creation_started, Environment, Email}),
    DirectoryUrl = case Environment of
        staging -> ?LETS_ENCRYPT_STAGING;
        production -> ?LETS_ENCRYPT_PROD
    end,
    try
        % Generate account key pair
        ?event({acme_generating_keypair, KeySize}),
        PrivateKey = generate_rsa_key(KeySize),
        % Get directory
        ?event({acme_fetching_directory, DirectoryUrl}),
        Directory = get_directory(DirectoryUrl),
        NewAccountUrl = maps:get(<<"newAccount">>, Directory),
        % Create account
        Payload = #{
            <<"termsOfServiceAgreed">> => true,
            <<"contact">> => [<<"mailto:", (hb_util:bin(Email))/binary>>]
        },
        ?event({acme_creating_account, NewAccountUrl}),
        case make_jws_request(NewAccountUrl, Payload, PrivateKey, 
                             undefined) of
            {ok, _Response, Headers} ->
                Location = proplists:get_value("location", Headers),
                Account = #acme_account{
                    key = PrivateKey,
                    url = Location,
                    kid = Location
                },
                ?event({acme_account_created, Location}),
                {ok, Account};
            {error, Reason} ->
                ?event({
                    acme_account_creation_failed,
                    {reason, Reason},
                    {directory_url, DirectoryUrl},
                    {email, Email},
                    {environment, Environment}
                }),
                {error, {account_creation_failed, Reason}}
        end
    catch
        Error:CreateReason:Stacktrace ->
            ?event({
                acme_account_creation_error,
                {error_type, Error},
                {reason, CreateReason},
                {config, Config},
                {stacktrace, Stacktrace}
            }),
            {error, {account_creation_failed, Error, CreateReason}}
    end.

%% @doc Requests a certificate for the specified domains.
%%
%% This function initiates the certificate issuance process:
%% 1. Determines the ACME directory URL from the account
%% 2. Creates domain identifiers for the certificate request
%% 3. Submits a new order request to the ACME server
%% 4. Returns an order record with authorization URLs and status
%%
%% The returned order contains authorization URLs that must be completed
%% before the certificate can be finalized.
%%
%% @param Account The ACME account record from create_account/1
%% @param Domains A list of domain names for the certificate
%% @returns {ok, Order} on success with order details, or
%% {error, Reason} on failure with error information
request_certificate(Account, Domains) ->
    ?event({acme_certificate_request_started, Domains}),
    DirectoryUrl = determine_directory_from_account(Account),
    try
        Directory = get_directory(DirectoryUrl),
        NewOrderUrl = maps:get(<<"newOrder">>, Directory),
        % Create identifiers for domains
        Identifiers = [#{<<"type">> => <<"dns">>, 
                        <<"value">> => hb_util:bin(Domain)} 
                      || Domain <- Domains],
        Payload = #{<<"identifiers">> => Identifiers},
        ?event({acme_submitting_order, NewOrderUrl, length(Domains)}),
        case make_jws_request(NewOrderUrl, Payload, Account#acme_account.key,
                             Account#acme_account.kid) of
            {ok, Response, Headers} ->
                Location = proplists:get_value("location", Headers),
                Order = #acme_order{
                    url = Location,
                    status = hb_util:list(maps:get(<<"status">>, Response)),
                    expires = hb_util:list(maps:get(<<"expires">>, Response)),
                    identifiers = maps:get(<<"identifiers">>, Response),
                    authorizations = maps:get(<<"authorizations">>, Response),
                    finalize = hb_util:list(maps:get(<<"finalize">>, Response))
                },
                ?event({acme_order_created, Location, Order#acme_order.status}),
                {ok, Order};
            {error, Reason} ->
                ?event({acme_order_creation_failed, Reason}),
                {error, Reason}
        end
    catch
        Error:OrderReason:Stacktrace ->
            ?event({acme_order_error, Error, OrderReason, Stacktrace}),
            {error, {unexpected_error, Error, OrderReason}}
    end.

%% @doc Retrieves DNS-01 challenges for all domains in an order.
%%
%% This function processes each authorization in the order:
%% 1. Fetches authorization details from each authorization URL
%% 2. Locates the DNS-01 challenge within each authorization
%% 3. Generates the key authorization string for each challenge
%% 4. Computes the DNS TXT record value using SHA-256 hash
%% 5. Returns a list of DNS challenge records with all required information
%%
%% The returned challenges contain the exact values needed to create
%% DNS TXT records for domain validation.
%%
%% @param Account The ACME account record
%% @param Order The certificate order from request_certificate/2
%% @returns {ok, [DNSChallenge]} on success with challenge list, or
%% {error, Reason} on failure
get_dns_challenge(Account, Order) ->
    ?event({acme_dns_challenges_started, length(Order#acme_order.authorizations)}),
    Authorizations = Order#acme_order.authorizations,
    try
        % Process each authorization to get DNS challenges
        Challenges = lists:foldl(fun(AuthzUrl, Acc) ->
            AuthzUrlStr = hb_util:list(AuthzUrl),
            ?event({acme_processing_authorization, AuthzUrlStr}),
            case get_authorization(AuthzUrlStr) of
                {ok, Authz} ->
                    Domain = hb_util:list(maps:get(<<"value">>, 
                                         maps:get(<<"identifier">>, Authz))),
                    case find_dns_challenge(maps:get(<<"challenges">>, Authz)) of
                        {ok, Challenge} ->
                            Token = hb_util:list(maps:get(<<"token">>, Challenge)),
                            Url = hb_util:list(maps:get(<<"url">>, Challenge)),
                            % Generate key authorization
                            KeyAuth = generate_key_authorization(Token, 
                                                               Account#acme_account.key),
                            % Generate DNS TXT record value
                            DnsValue = generate_dns_txt_value(KeyAuth),
                            DnsChallenge = #dns_challenge{
                                domain = Domain,
                                token = Token,
                                key_authorization = KeyAuth,
                                dns_value = DnsValue,
                                url = Url
                            },
                            ?event({acme_dns_challenge_generated, Domain, DnsValue}),
                            [DnsChallenge | Acc];
                        {error, Reason} ->
                            ?event({acme_dns_challenge_not_found, Domain, Reason}),
                            Acc
                    end;
                {error, Reason} ->
                    ?event({acme_authorization_fetch_failed, AuthzUrlStr, Reason}),
                    Acc
            end
        end, [], Authorizations),
        case Challenges of
            [] -> 
                ?event({acme_no_dns_challenges_found}),
                {error, no_dns_challenges_found};
            _ -> 
                ?event({acme_dns_challenges_completed, length(Challenges)}),
                {ok, lists:reverse(Challenges)}
        end
    catch
        Error:DnsReason:Stacktrace ->
            ?event({acme_dns_challenge_error, Error, DnsReason, Stacktrace}),
            {error, {unexpected_error, Error, DnsReason}}
    end.

%% @doc Validates a DNS challenge with the ACME server.
%%
%% This function notifies the ACME server that the DNS TXT record has been
%% created and requests validation:
%% 1. Sends an empty payload POST request to the challenge URL
%% 2. The server will then check the DNS TXT record
%% 3. Returns the challenge status (usually 'pending' initially)
%%
%% After calling this function, the challenge status should be polled
%% until it becomes 'valid' or 'invalid'.
%%
%% @param Account The ACME account record
%% @param Challenge The DNS challenge record from get_dns_challenge/2
%% @returns {ok, Status} on success with challenge status, or
%% {error, Reason} on failure
validate_challenge(Account, Challenge) ->
    ?event({acme_challenge_validation_started, Challenge#dns_challenge.domain}),
    try
        Payload = #{},
        case make_jws_request(Challenge#dns_challenge.url, Payload, 
                             Account#acme_account.key, Account#acme_account.kid) of
            {ok, Response, _Headers} ->
                Status = hb_util:list(maps:get(<<"status">>, Response)),
                ?event({acme_challenge_validation_response, 
                       Challenge#dns_challenge.domain, Status}),
                {ok, Status};
            {error, Reason} ->
                ?event({acme_challenge_validation_failed, 
                       Challenge#dns_challenge.domain, Reason}),
                {error, Reason}
        end
    catch
        Error:ValidateReason:Stacktrace ->
            ?event({acme_challenge_validation_error, 
                   Challenge#dns_challenge.domain, Error, ValidateReason, Stacktrace}),
            {error, {unexpected_error, Error, ValidateReason}}
    end.

%% @doc Finalizes a certificate order after all challenges are validated.
%%
%% This function completes the certificate issuance process:
%% 1. Generates a Certificate Signing Request (CSR) for the domains
%% 2. Creates a new RSA key pair for the certificate
%% 3. Submits the CSR to the ACME server's finalize endpoint
%% 4. Returns the updated order and the certificate private key
%%
%% The order status will change to 'processing' and then 'valid' when
%% the certificate is ready for download.
%%
%% @param Account The ACME account record
%% @param Order The certificate order with validated challenges
%% @returns {ok, UpdatedOrder, CertificateKey} on success, or
%% {error, Reason} on failure
finalize_order(Account, Order) ->
    ?event({acme_order_finalization_started, Order#acme_order.url}),
    try
        % Generate certificate signing request
        Domains = [hb_util:list(maps:get(<<"value">>, Id)) 
                  || Id <- Order#acme_order.identifiers],
        ?event({acme_generating_csr, Domains}),
        case generate_csr_internal(Domains) of
            {ok, CsrDer, CertKey} ->
                CsrB64 = base64url_encode(CsrDer),
                Payload = #{<<"csr">> => hb_util:bin(CsrB64)},
                ?event({acme_submitting_csr, Order#acme_order.finalize}),
                case make_jws_request(Order#acme_order.finalize, Payload,
                                     Account#acme_account.key, 
                                     Account#acme_account.kid) of
                    {ok, Response, _Headers} ->
                        UpdatedOrder = Order#acme_order{
                            status = hb_util:list(maps:get(<<"status">>, Response)),
                            certificate = case maps:get(<<"certificate">>, 
                                                       Response, undefined) of
                                undefined -> undefined;
                                CertUrl -> hb_util:list(CertUrl)
                            end
                        },
                        ?event({acme_order_finalized, UpdatedOrder#acme_order.status}),
                        {ok, UpdatedOrder, CertKey};
                    {error, Reason} ->
                        ?event({acme_order_finalization_failed, Reason}),
                        {error, Reason}
                end;
            {error, Reason} ->
                ?event({acme_csr_generation_failed, Reason}),
                {error, Reason}
        end
    catch
        Error:FinalizeReason:Stacktrace ->
            ?event({acme_finalization_error, Error, FinalizeReason, Stacktrace}),
            {error, {unexpected_error, Error, FinalizeReason}}
    end.

%% @doc Downloads the certificate from the ACME server.
%%
%% This function retrieves the issued certificate:
%% 1. Verifies that the order has a certificate URL
%% 2. Makes a GET request to the certificate URL
%% 3. Returns the certificate chain in PEM format
%%
%% The certificate URL is only available when the order status is 'valid'.
%% The returned PEM typically contains the end-entity certificate followed
%% by intermediate certificates.
%%
%% @param Account The ACME account record (used for authentication)
%% @param Order The finalized certificate order
%% @returns {ok, CertificatePEM} on success with certificate chain, or
%% {error, Reason} on failure
download_certificate(_Account, Order) 
    when Order#acme_order.certificate =/= undefined ->
    ?event({acme_certificate_download_started, Order#acme_order.certificate}),
    try
        case make_get_request(Order#acme_order.certificate) of
            {ok, CertPem} ->
                ?event({acme_certificate_downloaded, 
                       Order#acme_order.certificate, byte_size(CertPem)}),
                {ok, hb_util:list(CertPem)};
            {error, Reason} ->
                ?event({acme_certificate_download_failed, Reason}),
                {error, Reason}
        end
    catch
        Error:DownloadReason:Stacktrace ->
            ?event({acme_certificate_download_error, Error, DownloadReason, Stacktrace}),
            {error, {unexpected_error, Error, DownloadReason}}
    end;
download_certificate(_Account, _Order) ->
    ?event({acme_certificate_not_ready}),
    {error, certificate_not_ready}.

%%%--------------------------------------------------------------------
%%% Internal Functions
%%%--------------------------------------------------------------------

%% @doc Generates an RSA private key of the specified size.
%%
%% @param KeySize The size of the RSA key in bits
%% @returns An RSA private key record
generate_rsa_key(KeySize) ->
    ?event({acme_generating_rsa_key, KeySize}),
    public_key:generate_key({rsa, KeySize, 65537}).

%% @doc Retrieves the ACME directory from the specified URL.
%%
%% @param DirectoryUrl The ACME directory URL
%% @returns A map containing the directory endpoints
get_directory(DirectoryUrl) ->
    ?event({acme_fetching_directory, DirectoryUrl}),
    case make_get_request(DirectoryUrl) of
        {ok, Response} ->
            hb_json:decode(Response);
        {error, Reason} ->
            ?event({acme_directory_fetch_failed, DirectoryUrl, Reason}),
            throw({directory_fetch_failed, Reason})
    end.

%% @doc Determines the ACME directory URL from an account record.
%%
%% @param Account The ACME account record
%% @returns The directory URL string
determine_directory_from_account(Account) ->
    case string:find(Account#acme_account.url, "staging") of
        nomatch -> ?LETS_ENCRYPT_PROD;
        _ -> ?LETS_ENCRYPT_STAGING
    end.

%% @doc Retrieves authorization details from the ACME server.
%%
%% @param AuthzUrl The authorization URL
%% @returns {ok, Authorization} on success, {error, Reason} on failure
get_authorization(AuthzUrl) ->
    case make_get_request(AuthzUrl) of
        {ok, Response} ->
            {ok, hb_json:decode(Response)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Finds the DNS-01 challenge in a list of challenges.
%%
%% @param Challenges A list of challenge maps
%% @returns {ok, Challenge} if found, {error, not_found} otherwise
find_dns_challenge(Challenges) ->
    DnsChallenges = lists:filter(fun(C) -> 
        maps:get(<<"type">>, C) == <<"dns-01">> 
    end, Challenges),
    case DnsChallenges of
        [Challenge | _] -> {ok, Challenge};
        [] -> {error, dns_challenge_not_found}
    end.

%% @doc Generates the key authorization string for a challenge.
%%
%% @param Token The challenge token from the ACME server
%% @param PrivateKey The account's private key
%% @returns The key authorization string
generate_key_authorization(Token, PrivateKey) ->
    Thumbprint = get_jwk_thumbprint(PrivateKey),
    Token ++ "." ++ Thumbprint.

%% @doc Generates the DNS TXT record value from key authorization.
%%
%% @param KeyAuthorization The key authorization string
%% @returns The base64url-encoded SHA-256 hash for the DNS TXT record
generate_dns_txt_value(KeyAuthorization) ->
    Hash = crypto:hash(sha256, KeyAuthorization),
    base64url_encode(Hash).

%% @doc Computes the JWK thumbprint for an RSA private key.
%%
%% @param PrivateKey The RSA private key
%% @returns The base64url-encoded JWK thumbprint
get_jwk_thumbprint(PrivateKey) ->
    Jwk = private_key_to_jwk(PrivateKey),
    JwkJson = hb_json:encode(Jwk),
    Hash = crypto:hash(sha256, JwkJson),
    base64url_encode(Hash).

%% @doc Converts an RSA private key to JWK format.
%%
%% @param PrivateKey The RSA private key record
%% @returns A map representing the JWK
private_key_to_jwk(#'RSAPrivateKey'{modulus = N, publicExponent = E}) ->
    #{
        <<"kty">> => <<"RSA">>,
        <<"n">> => hb_util:bin(base64url_encode(binary:encode_unsigned(N))),
        <<"e">> => hb_util:bin(base64url_encode(binary:encode_unsigned(E)))
    }.

%% @doc Generates a Certificate Signing Request for the domains.
%%
%% @param Domains A list of domain names for the certificate
%% @returns {ok, CSR_DER, PrivateKey} on success, {error, Reason} on failure
generate_csr_internal(Domains) ->
    try
        % Generate certificate key pair
        CertKey = generate_rsa_key(2048),
        % Create subject with first domain as CN
        Subject = [{?'id-at-commonName', hd(Domains)}],
        % Create SAN extension for multiple domains
        SANs = [{dNSName, Domain} || Domain <- Domains],
        Extensions = [#'Extension'{
            extnID = ?'id-ce-subjectAltName',
            critical = false,
            extnValue = SANs
        }],
        % Get public key info
        {_, PubKey} = CertKey,
        PubKeyInfo = #'SubjectPublicKeyInfo'{
            algorithm = #'AlgorithmIdentifier'{
                algorithm = ?'rsaEncryption',
                parameters = 'NULL'
            },
            subjectPublicKey = PubKey
        },
        % Create CSR info
        CsrInfo = #'CertificationRequestInfo'{
            version = v1,
            subject = {rdnSequence, [
                [{#'AttributeTypeAndValue'{
                    type = Type,
                    value = {utf8String, Value}
                }} || {Type, Value} <- Subject]
            ]},
            subjectPKInfo = PubKeyInfo,
            attributes = [#'Attribute'{
                type = ?'pkcs-9-at-extensionRequest',
                values = [Extensions]
            }]
        },
        % Sign CSR
        CsrInfoDer = public_key:der_encode('CertificationRequestInfo', CsrInfo),
        Signature = public_key:sign(CsrInfoDer, sha256, CertKey),
        Csr = #'CertificationRequest'{
            certificationRequestInfo = CsrInfo,
            signatureAlgorithm = #'AlgorithmIdentifier'{
                algorithm = ?'sha256WithRSAEncryption'
            },
            signature = Signature
        },
        CsrDer = public_key:der_encode('CertificationRequest', Csr),
        {ok, CsrDer, CertKey}
    catch
        Error:CsrGenReason:Stacktrace ->
            ?event({acme_csr_generation_error, Error, CsrGenReason, Stacktrace}),
            {error, {csr_generation_failed, Error, CsrGenReason}}
    end.

%% @doc Creates and sends a JWS-signed request to the ACME server.
%%
%% @param Url The target URL
%% @param Payload The request payload
%% @param PrivateKey The account's private key
%% @param Kid The account's key identifier (undefined for new accounts)
%% @returns {ok, Response, Headers} on success, {error, Reason} on failure
make_jws_request(Url, Payload, PrivateKey, Kid) ->
    try
        % Get fresh nonce from ACME server
        DirectoryUrl = determine_directory_from_url(Url),
        FreshNonce = get_fresh_nonce(DirectoryUrl),
        % Create JWS header
        Header = case Kid of
            undefined ->
                #{
                    <<"alg">> => <<"RS256">>,
                    <<"jwk">> => private_key_to_jwk(PrivateKey),
                    <<"nonce">> => hb_util:bin(FreshNonce),
                    <<"url">> => hb_util:bin(Url)
                };
            _ ->
                #{
                    <<"alg">> => <<"RS256">>,
                    <<"kid">> => hb_util:bin(Kid),
                    <<"nonce">> => hb_util:bin(FreshNonce),
                    <<"url">> => hb_util:bin(Url)
                }
        end,
        % Encode components
        HeaderB64 = base64url_encode(hb_json:encode(Header)),
        PayloadB64 = base64url_encode(hb_json:encode(Payload)),
        % Create signature
        SigningInput = HeaderB64 ++ "." ++ PayloadB64,
        Signature = public_key:sign(SigningInput, sha256, PrivateKey),
        SignatureB64 = base64url_encode(Signature),
        % Create JWS
        Jws = #{
            <<"protected">> => hb_util:bin(HeaderB64),
            <<"payload">> => hb_util:bin(PayloadB64),
            <<"signature">> => hb_util:bin(SignatureB64)
        },
        % Make HTTP request
        Body = hb_json:encode(Jws),
        Headers = [
            {"Content-Type", "application/jose+json"},
            {"User-Agent", "HyperBEAM-ACME-Client/1.0"}
        ],
        case hb_http_client:req(#{
            peer => hb_util:bin(extract_base_url(Url)),
            path => hb_util:bin(extract_path_from_url(Url)),
            method => <<"POST">>,
            headers => headers_to_map(Headers),
            body => Body
        }, #{}) of
            {ok, {{Version, StatusCode, ReasonPhrase}, ResponseHeaders, 
                  ResponseBody}} ->
                ?event({
                    acme_http_response_received,
                    {status_code, StatusCode},
                    {reason_phrase, ReasonPhrase},
                    {version, Version},
                    {body_size, byte_size(ResponseBody)}
                }),
                case StatusCode of
                    Code when Code >= 200, Code < 300 ->
                        Response = case ResponseBody of
                            <<>> -> #{};
                            _ -> 
                                try
                                    hb_json:decode(ResponseBody)
                                catch
                                    JsonError:JsonReason ->
                                        ?event({
                                            acme_json_decode_failed,
                                            {error, JsonError},
                                            {reason, JsonReason},
                                            {body, ResponseBody}
                                        }),
                                        #{}
                                end
                        end,
                        ?event({acme_http_request_successful, {response_keys, maps:keys(Response)}}),
                        {ok, Response, ResponseHeaders};
                    _ ->
                        % Enhanced error reporting for HTTP failures
                        ErrorDetails = try
                            case ResponseBody of
                                <<>> -> 
                                    #{<<"error">> => <<"Empty response body">>};
                                _ ->
                                    hb_json:decode(ResponseBody)
                            end
                        catch
                            _:_ ->
                                #{<<"error">> => ResponseBody}
                        end,
                        ?event({
                            acme_http_error_detailed,
                            {status_code, StatusCode},
                            {reason_phrase, ReasonPhrase},
                            {error_details, ErrorDetails},
                            {headers, ResponseHeaders}
                        }),
                        {error, {http_error, StatusCode, ErrorDetails}}
                end;
            {error, Reason} ->
                ?event({
                    acme_http_request_failed,
                    {error_type, connection_failed},
                    {reason, Reason},
                    {url, Url}
                }),
                {error, {connection_failed, Reason}}
        end
    catch
        Error:JwsReason:Stacktrace ->
            ?event({acme_jws_request_error, Url, Error, JwsReason, Stacktrace}),
            {error, {jws_request_failed, Error, JwsReason}}
    end.

%% @doc Makes a GET request to the specified URL.
%%
%% @param Url The target URL
%% @returns {ok, ResponseBody} on success, {error, Reason} on failure
make_get_request(Url) ->
    Headers = [{"User-Agent", "HyperBEAM-ACME-Client/1.0"}],
    case hb_http_client:req(#{
        peer => hb_util:bin(extract_base_url(Url)),
        path => hb_util:bin(extract_path_from_url(Url)),
        method => <<"GET">>,
        headers => headers_to_map(Headers),
        body => <<>>
    }, #{}) of
        {ok, {{Version, StatusCode, ReasonPhrase}, ResponseHeaders, 
              ResponseBody}} ->
            ?event({
                acme_get_response_received,
                {status_code, StatusCode},
                {reason_phrase, ReasonPhrase},
                {version, Version},
                {body_size, byte_size(ResponseBody)},
                {url, Url}
            }),
            case StatusCode of
                Code when Code >= 200, Code < 300 ->
                    ?event({acme_get_request_successful, {url, Url}}),
                    {ok, ResponseBody};
                _ ->
                    % Enhanced error reporting for GET failures
                    ErrorBody = case ResponseBody of
                        <<>> -> <<"Empty response">>;
                        _ -> ResponseBody
                    end,
                    ?event({
                        acme_get_error_detailed,
                        {status_code, StatusCode},
                        {reason_phrase, ReasonPhrase},
                        {error_body, ErrorBody},
                        {url, Url},
                        {headers, ResponseHeaders}
                    }),
                    {error, {http_get_error, StatusCode, ErrorBody}}
            end;
        {error, Reason} ->
            ?event({
                acme_get_request_failed,
                {error_type, connection_failed},
                {reason, Reason},
                {url, Url}
            }),
            {error, {connection_failed, Reason}}
    end.

%% @doc Gets a fresh nonce from the ACME server.
%%
%% This function retrieves a fresh nonce from Let's Encrypt's newNonce
%% endpoint as required by the ACME v2 protocol. Each JWS request must
%% use a unique nonce to prevent replay attacks.
%%
%% @param DirectoryUrl The ACME directory URL to get newNonce endpoint
%% @returns A base64url-encoded nonce string
get_fresh_nonce(DirectoryUrl) ->
    try
        Directory = get_directory(DirectoryUrl),
        NewNonceUrl = hb_util:list(maps:get(<<"newNonce">>, Directory)),
        ?event({acme_getting_fresh_nonce, NewNonceUrl}),
        case hb_http_client:req(#{
            peer => hb_util:bin(extract_base_url(NewNonceUrl)),
            path => hb_util:bin(extract_path_from_url(NewNonceUrl)),
            method => <<"HEAD">>,
            headers => #{<<"User-Agent">> => <<"HyperBEAM-ACME-Client/1.0">>},
            body => <<>>
        }, #{}) of
            {ok, {{Version, StatusCode, ReasonPhrase}, ResponseHeaders, _ResponseBody}} 
                when StatusCode >= 200, StatusCode < 300 ->
                ?event({
                    acme_nonce_response_received,
                    {status_code, StatusCode},
                    {reason_phrase, ReasonPhrase},
                    {version, Version},
                    {headers_count, length(ResponseHeaders)}
                }),
                case proplists:get_value("replay-nonce", ResponseHeaders) of
                    undefined ->
                        ?event({
                            acme_nonce_not_found_in_headers,
                            {available_headers, [K || {K, _V} <- ResponseHeaders]},
                            {url, NewNonceUrl}
                        }),
                        % Fallback to random nonce
                        RandomNonce = base64url_encode(crypto:strong_rand_bytes(16)),
                        ?event({acme_using_fallback_nonce, {nonce_length, length(RandomNonce)}}),
                        RandomNonce;
                    Nonce ->
                        ?event({
                            acme_fresh_nonce_received,
                            {nonce, Nonce},
                            {nonce_length, length(Nonce)},
                            {url, NewNonceUrl}
                        }),
                        Nonce
                end;
            {ok, {{Version, StatusCode, ReasonPhrase}, ResponseHeaders, ResponseBody}} ->
                ?event({
                    acme_nonce_request_failed_with_response,
                    {status_code, StatusCode},
                    {reason_phrase, ReasonPhrase},
                    {version, Version},
                    {body, ResponseBody},
                    {headers, ResponseHeaders}
                }),
                % Fallback to random nonce
                RandomNonce = base64url_encode(crypto:strong_rand_bytes(16)),
                ?event({acme_using_fallback_nonce_after_error, {nonce_length, length(RandomNonce)}}),
                RandomNonce;
            {error, Reason} ->
                ?event({
                    acme_nonce_request_failed,
                    {reason, Reason},
                    {url, NewNonceUrl},
                    {directory_url, DirectoryUrl}
                }),
                % Fallback to random nonce
                RandomNonce = base64url_encode(crypto:strong_rand_bytes(16)),
                ?event({acme_using_fallback_nonce_after_connection_error, {nonce_length, length(RandomNonce)}}),
                RandomNonce
        end
    catch
        _:_ ->
            ?event({acme_nonce_fallback_to_random}),
            base64url_encode(crypto:strong_rand_bytes(16))
    end.

%% @doc Generates a random nonce for JWS requests (fallback).
%%
%% @returns A base64url-encoded nonce string
get_nonce() ->
    base64url_encode(crypto:strong_rand_bytes(16)).

%% @doc Encodes data using base64url encoding.
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

%% @doc Extracts the base URL (scheme + host) from a complete URL.
%%
%% @param Url The complete URL string
%% @returns The base URL (e.g., "https://example.com") as string
extract_base_url(Url) ->
    case string:split(Url, "://") of
        [Scheme, Rest] ->
            case string:split(Rest, "/") of
                [Host | _] -> Scheme ++ "://" ++ Host
            end;
        [_] ->
            % No scheme, assume https
            case string:split(Url, "/") of
                [Host | _] -> "https://" ++ Host
            end
    end.

%% @doc Extracts the host from a URL.
%%
%% @param Url The complete URL string
%% @returns The host portion as binary
extract_host_from_url(Url) ->
    % Parse URL to extract host
    case string:split(Url, "://") of
        [_Scheme, Rest] ->
            case string:split(Rest, "/") of
                [Host | _] -> hb_util:bin(Host)
            end;
        [Host] ->
            case string:split(Host, "/") of
                [HostOnly | _] -> hb_util:bin(HostOnly)
            end
    end.

%% @doc Extracts the path from a URL.
%%
%% @param Url The complete URL string
%% @returns The path portion as string
extract_path_from_url(Url) ->
    % Parse URL to extract path
    case string:split(Url, "://") of
        [_Scheme, Rest] ->
            case string:split(Rest, "/") of
                [_Host | PathParts] -> "/" ++ string:join(PathParts, "/")
            end;
        [Rest] ->
            case string:split(Rest, "/") of
                [_Host | PathParts] -> "/" ++ string:join(PathParts, "/")
            end
    end.

%% @doc Converts header list to map format.
%%
%% @param Headers List of {Key, Value} header tuples
%% @returns Map of headers
headers_to_map(Headers) ->
    maps:from_list([{hb_util:bin(K), hb_util:bin(V)} || {K, V} <- Headers]).

%% @doc Determines the ACME directory URL from any ACME endpoint URL.
%%
%% @param Url Any ACME endpoint URL
%% @returns The directory URL string
determine_directory_from_url(Url) ->
    case string:find(Url, "staging") of
        nomatch -> ?LETS_ENCRYPT_PROD;
        _ -> ?LETS_ENCRYPT_STAGING
    end.
