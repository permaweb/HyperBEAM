%%% @doc ACME protocol implementation module.
%%%
%%% This module implements the core ACME (Automatic Certificate Management
%%% Environment) v2 protocol operations for automated certificate issuance
%%% and management. It handles account creation, certificate orders, challenge
%%% processing, order finalization, and certificate download according to RFC 8555.
%%%
%%% The module provides high-level protocol operations that orchestrate the
%%% lower-level HTTP, cryptographic, and CSR generation operations.
-module(hb_acme_protocol).

-include("include/ssl_cert_records.hrl").
-include("include/hb.hrl").

%% Public API
-export([
    create_account/2,
    request_certificate/2,
    get_dns_challenge/2,
    validate_challenge/2,
    get_challenge_status/2,
    finalize_order/3,
    download_certificate/2,
    get_order/2,
    get_authorization/1,
    find_dns_challenge/1
]).

%% Type specifications
-spec create_account(map(), map()) -> {ok, acme_account()} | {error, term()}.
-spec request_certificate(acme_account(), [string()]) -> {ok, acme_order()} | {error, term()}.
-spec get_dns_challenge(acme_account(), acme_order()) -> {ok, [dns_challenge()]} | {error, term()}.
-spec validate_challenge(acme_account(), dns_challenge()) -> {ok, string()} | {error, term()}.
-spec get_challenge_status(acme_account(), dns_challenge()) -> {ok, string()} | {error, term()}.
-spec finalize_order(acme_account(), acme_order(), map()) -> {ok, acme_order(), public_key:private_key(), string()} | {error, term()}.
-spec download_certificate(acme_account(), acme_order()) -> {ok, string()} | {error, term()}.
-spec get_order(acme_account(), string()) -> {ok, map()} | {error, term()}.

%% @doc Creates a new ACME account with Let's Encrypt.
%%
%% This function performs the complete account creation process:
%% 1. Determines the ACME directory URL based on environment
%% 2. Generates a proper RSA key pair for the ACME account
%% 3. Retrieves the ACME directory to get service endpoints
%% 4. Creates a new account by agreeing to terms of service
%% 5. Returns an account record with key, URL, and key identifier
%%
%% Required configuration in Config map:
%% - environment: 'staging' or 'production' 
%% - email: Contact email for the account
%%
%% Note: The account uses a generated RSA key, while CSR generation uses
%% the wallet key. This ensures proper key serialization for account management.
%%
%% @param Config A map containing account creation parameters
%% @returns {ok, Account} on success with account details, or
%% {error, Reason} on failure with error information
create_account(Config, Opts) ->
    #{
        environment := Environment,
        email := Email
    } = Config,
    ?event(acme, {acme_account_creation_started, Environment, Email}),
    DirectoryUrl = case Environment of
        staging -> ?LETS_ENCRYPT_STAGING;
        production -> ?LETS_ENCRYPT_PROD
    end,
    try
        % Extract RSA key from wallet and save for CSR/certificate generation
        ?event(acme, {acme_extracting_wallet_key}),
        {{_KT = {rsa, E}, PrivBin, PubBin}, _} = hb_opts:get(priv_wallet, hb:wallet(), Opts),
        Modulus = crypto:bytes_to_integer(iolist_to_binary(PubBin)),
        D = crypto:bytes_to_integer(iolist_to_binary(PrivBin)),
        CertificateKey = hb_acme_csr:create_complete_rsa_key_from_wallet(Modulus, E, D),
        % Save the wallet-derived RSA key for CSR generation
        ok = hb_http_server:set_opts(Opts#{ <<"ssl_cert_rsa_key">> => CertificateKey }),
        % Generate separate RSA key for ACME account (must be different from certificate key)
        ?event(acme, {acme_generating_account_keypair}),
        AccountKey = public_key:generate_key({rsa, ?SSL_CERT_KEY_SIZE, 65537}),
        % Get directory
        ?event(acme, {acme_fetching_directory, DirectoryUrl}),
        Directory = hb_acme_http:get_directory(DirectoryUrl),
        NewAccountUrl = maps:get(<<"newAccount">>, Directory),
        % Create account
        Payload = #{
            <<"termsOfServiceAgreed">> => true,
            <<"contact">> => [<<"mailto:", (hb_util:bin(Email))/binary>>]
        },
        ?event(acme, {acme_creating_account, NewAccountUrl}),
        case hb_acme_http:make_jws_request(NewAccountUrl, Payload, AccountKey, undefined) of
            {ok, _Response, Headers} ->
                Location = hb_acme_http:extract_location_header(Headers),
                LocationStr = case Location of
                    undefined -> undefined;
                    L -> hb_util:list(L)
                end,
                Account = #acme_account{
                    key = AccountKey,
                    url = LocationStr,
                    kid = LocationStr
                },
                ?event(acme, {acme_account_created, LocationStr}),
                {ok, Account};
            {error, Reason} ->
                ?event(acme, {
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
            ?event(acme, {
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
%% @param Account The ACME account record from create_account/1
%% @param Domains A list of domain names for the certificate
%% @returns {ok, Order} on success with order details, or {error, Reason} on failure
request_certificate(Account, Domains) ->
    ?event(acme, {acme_certificate_request_started, Domains}),
    DirectoryUrl = hb_acme_url:determine_directory_from_account(Account),
    try
        Directory = hb_acme_http:get_directory(DirectoryUrl),
        NewOrderUrl = maps:get(<<"newOrder">>, Directory),
        % Create identifiers for domains
        Identifiers = [#{<<"type">> => <<"dns">>, 
                        <<"value">> => hb_util:bin(Domain)} 
                      || Domain <- Domains],
        Payload = #{<<"identifiers">> => Identifiers},
        ?event(acme, {acme_submitting_order, NewOrderUrl, length(Domains)}),
        case hb_acme_http:make_jws_request(NewOrderUrl, Payload, 
                                          Account#acme_account.key,
                                          Account#acme_account.kid) of
            {ok, Response, Headers} ->
                Location = hb_acme_http:extract_location_header(Headers),
                LocationStr = case Location of
                    undefined -> undefined;
                    L -> hb_util:list(L)
                end,
                Order = #acme_order{
                    url = LocationStr,
                    status = hb_util:list(maps:get(<<"status">>, Response)),
                    expires = hb_util:list(maps:get(<<"expires">>, Response)),
                    identifiers = maps:get(<<"identifiers">>, Response),
                    authorizations = maps:get(<<"authorizations">>, Response),
                    finalize = hb_util:list(maps:get(<<"finalize">>, Response))
                },
                ?event(acme, {acme_order_created, Location, Order#acme_order.status}),
                {ok, Order};
            {error, Reason} ->
                ?event(acme, {acme_order_creation_failed, Reason}),
                {error, Reason}
        end
    catch
        Error:OrderReason:Stacktrace ->
            ?event(acme, {acme_order_error, Error, OrderReason, Stacktrace}),
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
%% @param Account The ACME account record
%% @param Order The certificate order from request_certificate/2
%% @returns {ok, [DNSChallenge]} on success with challenge list, or {error, Reason} on failure
get_dns_challenge(Account, Order) ->
    ?event(acme, {acme_dns_challenges_started, length(Order#acme_order.authorizations)}),
    Authorizations = Order#acme_order.authorizations,
    try
        % Process each authorization to get DNS challenges
        Challenges = lists:foldl(fun(AuthzUrl, Acc) ->
            AuthzUrlStr = hb_util:list(AuthzUrl),
            ?event(acme, {acme_processing_authorization, AuthzUrlStr}),
            case get_authorization(AuthzUrlStr) of
                {ok, Authz} ->
                    Domain = hb_util:list(maps:get(<<"value">>, 
                                         maps:get(<<"identifier">>, Authz))),
                    case find_dns_challenge(maps:get(<<"challenges">>, Authz)) of
                        {ok, Challenge} ->
                            Token = hb_util:list(maps:get(<<"token">>, Challenge)),
                            Url = hb_util:list(maps:get(<<"url">>, Challenge)),
                            % Generate key authorization
                            KeyAuth = hb_acme_crypto:generate_key_authorization(Token, 
                                                               Account#acme_account.key),
                            % Generate DNS TXT record value
                            DnsValue = hb_acme_crypto:generate_dns_txt_value(KeyAuth),
                            DnsChallenge = #dns_challenge{
                                domain = Domain,
                                token = Token,
                                key_authorization = KeyAuth,
                                dns_value = DnsValue,
                                url = Url
                            },
                            ?event(acme, {acme_dns_challenge_generated, Domain, DnsValue}),
                            [DnsChallenge | Acc];
                        {error, Reason} ->
                            ?event(acme, {acme_dns_challenge_not_found, Domain, Reason}),
                            Acc
                    end;
                {error, Reason} ->
                    ?event(acme, {acme_authorization_fetch_failed, AuthzUrlStr, Reason}),
                    Acc
            end
        end, [], Authorizations),
        case Challenges of
            [] -> 
                ?event(acme, {acme_no_dns_challenges_found}),
                {error, no_dns_challenges_found};
            _ -> 
                ?event(acme, {acme_dns_challenges_completed, length(Challenges)}),
                {ok, lists:reverse(Challenges)}
        end
    catch
        Error:DnsReason:Stacktrace ->
            ?event(acme, {acme_dns_challenge_error, Error, DnsReason, Stacktrace}),
            {error, {unexpected_error, Error, DnsReason}}
    end.

%% @doc Validates a DNS challenge with the ACME server.
%%
%% This function notifies the ACME server that the DNS TXT record has been
%% created and requests validation. After calling this function, the challenge
%% status should be polled until it becomes 'valid' or 'invalid'.
%%
%% @param Account The ACME account record
%% @param Challenge The DNS challenge record from get_dns_challenge/2
%% @returns {ok, Status} on success with challenge status, or {error, Reason} on failure
validate_challenge(Account, Challenge) ->
    ?event(acme, {acme_challenge_validation_started, Challenge#dns_challenge.domain}),
    try
        Payload = #{},
        case hb_acme_http:make_jws_request(Challenge#dns_challenge.url, Payload, 
                             Account#acme_account.key, Account#acme_account.kid) of
            {ok, Response, _Headers} ->
                Status = hb_util:list(maps:get(<<"status">>, Response)),
                ?event(acme, {acme_challenge_validation_response, 
                       Challenge#dns_challenge.domain, Status}),
                {ok, Status};
            {error, Reason} ->
                ?event(acme, {acme_challenge_validation_failed, 
                       Challenge#dns_challenge.domain, Reason}),
                {error, Reason}
        end
    catch
        Error:ValidateReason:Stacktrace ->
            ?event(acme, {acme_challenge_validation_error, 
                   Challenge#dns_challenge.domain, Error, ValidateReason, Stacktrace}),
            {error, {unexpected_error, Error, ValidateReason}}
    end.

%% @doc Retrieves current challenge status using POST-as-GET (does not trigger).
%%
%% @param Account The ACME account
%% @param Challenge The challenge record
%% @returns {ok, Status} on success, {error, Reason} on failure
get_challenge_status(Account, Challenge) ->
    Url = Challenge#dns_challenge.url,
    ?event(acme, {acme_challenge_status_check_started, Challenge#dns_challenge.domain}),
    try
        case hb_acme_http:make_jws_post_as_get_request(Url, Account#acme_account.key, Account#acme_account.kid) of
            {ok, Response, _Headers} ->
                Status = hb_util:list(maps:get(<<"status">>, Response)),
                ?event(acme, {acme_challenge_status_response, Challenge#dns_challenge.domain, Status}),
                {ok, Status};
            {error, Reason} ->
                ?event(acme, {acme_challenge_status_failed, Challenge#dns_challenge.domain, Reason}),
                {error, Reason}
        end
    catch
        Error:GetStatusReason:Stacktrace ->
            ?event(acme, {acme_challenge_status_error, Challenge#dns_challenge.domain, Error, GetStatusReason, Stacktrace}),
            {error, {unexpected_error, Error, GetStatusReason}}
    end.

%% @doc Finalizes a certificate order after all challenges are validated.
%%
%% This function completes the certificate issuance process:
%% 1. Generates a Certificate Signing Request (CSR) for the domains
%% 2. Uses the RSA key pair from wallet for the certificate
%% 3. Submits the CSR to the ACME server's finalize endpoint
%% 4. Returns the updated order and the certificate private key for nginx
%%
%% @param Account The ACME account record
%% @param Order The certificate order with validated challenges
%% @param Opts Configuration options for CSR generation
%% @returns {ok, UpdatedOrder, CertificateKey} on success, or {error, Reason} on failure
finalize_order(Account, Order, Opts) ->
    ?event(acme, {acme_order_finalization_started, Order#acme_order.url}),
    try
        % Generate certificate signing request
        Domains = [hb_util:list(maps:get(<<"value">>, Id)) 
                  || Id <- Order#acme_order.identifiers],
        ?event(acme, {acme_generating_csr, Domains}),
        case hb_acme_csr:generate_csr(Domains, Opts) of
            {ok, CsrDer} ->
                CsrB64 = hb_acme_crypto:base64url_encode(CsrDer),
                Payload = #{<<"csr">> => hb_util:bin(CsrB64)},
                ?event(acme, {acme_submitting_csr, Order#acme_order.finalize}),
                case hb_acme_http:make_jws_request(Order#acme_order.finalize, Payload,
                                     Account#acme_account.key, 
                                     Account#acme_account.kid) of
                    {ok, Response, _Headers} ->
                        ?event(acme, {acme_order_finalization_response, Response}),
                        UpdatedOrder = Order#acme_order{
                            status = hb_util:list(maps:get(<<"status">>, Response)),
                            certificate = case maps:get(<<"certificate">>, 
                                                       Response, undefined) of
                                undefined -> undefined;
                                CertUrl -> hb_util:list(CertUrl)
                            end
                        },
                        ?event(acme, {acme_order_finalized, UpdatedOrder#acme_order.status}),
                        {ok, UpdatedOrder};
                    {error, Reason} ->
                        ?event(acme, {acme_order_finalization_failed, Reason}),
                        {error, Reason}
                end;
            {error, Reason} ->
                ?event(acme, {acme_csr_generation_failed, Reason}),
                {error, Reason}
        end
    catch
        Error:FinalizeReason:Stacktrace ->
            ?event(acme, {acme_finalization_error, Error, FinalizeReason, Stacktrace}),
            {error, {unexpected_error, Error, FinalizeReason}}
    end.

%% @doc Downloads the certificate from the ACME server.
%%
%% This function retrieves the issued certificate when the order status is 'valid'.
%% The returned PEM typically contains the end-entity certificate followed
%% by intermediate certificates.
%%
%% @param _Account The ACME account record (used for authentication)
%% @param Order The finalized certificate order
%% @returns {ok, CertificatePEM} on success with certificate chain, or {error, Reason} on failure
download_certificate(_Account, Order) 
    when Order#acme_order.certificate =/= undefined ->
    ?event(acme, {acme_certificate_download_started, Order#acme_order.certificate}),
    try
        case hb_acme_http:make_get_request(Order#acme_order.certificate) of
            {ok, CertPem} ->
                ?event(acme, {acme_certificate_downloaded, 
                       Order#acme_order.certificate, byte_size(CertPem)}),
                {ok, hb_util:list(CertPem)};
            {error, Reason} ->
                ?event(acme, {acme_certificate_download_failed, Reason}),
                {error, Reason}
        end
    catch
        Error:DownloadReason:Stacktrace ->
            ?event(acme, {acme_certificate_download_error, Error, DownloadReason, Stacktrace}),
            {error, {unexpected_error, Error, DownloadReason}}
    end;
download_certificate(_Account, _Order) ->
    ?event(acme, {acme_certificate_not_ready}),
    {error, certificate_not_ready}.

%% @doc Fetches the latest state of an order (POST-as-GET).
%%
%% @param Account The ACME account
%% @param OrderUrl The order URL
%% @returns {ok, OrderMap} with at least status and optional certificate, or {error, Reason}
get_order(Account, OrderUrl) ->
    ?event(acme, {acme_get_order_started, OrderUrl}),
    try
        case hb_acme_http:make_jws_post_as_get_request(OrderUrl, Account#acme_account.key, Account#acme_account.kid) of
            {ok, Response, _Headers} ->
                ?event(acme, {acme_get_order_response, Response}),
                {ok, Response};
            {error, Reason} ->
                ?event(acme, {acme_get_order_failed, Reason}),
                {error, Reason}
        end
    catch
        Error:GetOrderReason:Stacktrace ->
            ?event(acme, {acme_get_order_error, Error, GetOrderReason, Stacktrace}),
            {error, {unexpected_error, Error, GetOrderReason}}
    end.

%% @doc Retrieves authorization details from the ACME server.
%%
%% @param AuthzUrl The authorization URL
%% @returns {ok, Authorization} on success, {error, Reason} on failure
get_authorization(AuthzUrl) ->
    case hb_acme_http:make_get_request(AuthzUrl) of
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

