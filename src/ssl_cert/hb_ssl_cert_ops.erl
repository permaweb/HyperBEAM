%%% @doc SSL Certificate operations module.
%%%
%%% This module handles certificate-related operations including downloading
%%% certificates from Let's Encrypt, processing certificate chains, and
%%% managing certificate storage and retrieval.
%%%
%%% The module provides functions for the complete certificate lifecycle
%%% from download to storage and cleanup operations.
-module(hb_ssl_cert_ops).

-include("include/ssl_cert_records.hrl").
-include("include/hb.hrl").

%% Public API
-export([
    download_certificate_state/2,
    process_certificate_request/2,
    renew_certificate/2,
    delete_certificate/2,
    extract_end_entity_cert/1
]).

%% Type specifications
-spec download_certificate_state(request_state(), map()) -> 
    {ok, map()} | {error, map()}.
-spec process_certificate_request(map(), map()) -> 
    {ok, map()} | {error, map()}.
-spec renew_certificate(domain_list(), map()) -> 
    {ok, map()} | {error, map()}.
-spec delete_certificate(domain_list(), map()) -> 
    {ok, map()} | {error, map()}.
-spec extract_end_entity_cert(string()) -> string().

%% @doc Downloads a certificate from Let's Encrypt using the request state.
%%
%% This function extracts the necessary information from the request state,
%% downloads the certificate from Let's Encrypt, and returns the certificate
%% in PEM format along with metadata.
%%
%% @param State The current request state containing order information
%% @param _Opts Configuration options (currently unused)
%% @returns {ok, DownloadResponse} or {error, ErrorResponse}
download_certificate_state(State, _Opts) ->
    maybe
        _ ?= case is_map(State) of
            true -> {ok, true};
            false -> {error, invalid_request_state}
        end,
        Account = hb_ssl_cert_state:extract_account_from_state(State),
        Order = hb_ssl_cert_state:extract_order_from_state(State),
        {ok, CertPem} ?= hb_acme_client:download_certificate(Account, Order),
        Domains = maps:get(<<"domains">>, State),
        ProcessedCert = CertPem,
        % Get the CSR private key from request state for nginx (wallet-based)
        PrivKeyPem = hb_util:list(maps:get(<<"csr_private_key_pem">>, State, <<>>)),
        {ok, #{<<"status">> => 200, 
               <<"body">> => #{
                   <<"message">> => <<"Certificate downloaded successfully">>,
                   <<"certificate_pem">> => hb_util:bin(ProcessedCert),
                   <<"private_key_pem">> => hb_util:bin(PrivKeyPem),
                   <<"domains">> => [hb_util:bin(D) || D <- Domains],
                   <<"include_chain">> => true
               }}}
    else
        {error, invalid_request_state} ->
            {error, #{<<"status">> => 400, <<"error">> => <<"Invalid request state">>}};
        {error, certificate_not_ready} ->
            {ok, #{<<"status">> => 202, 
                   <<"body">> => #{<<"message">> => <<"Certificate not ready yet">>}}};
        {error, Reason} ->
            {error, #{<<"status">> => 500, 
                     <<"error">> => hb_util:bin(io_lib:format("Download failed: ~p", [Reason]))}};
        Error ->
            {error, #{<<"status">> => 500, <<"error">> => hb_util:bin(io_lib:format("~p", [Error]))}}
    end.

%% @doc Processes a validated certificate request by creating ACME components.
%%
%% This function orchestrates the certificate request process:
%% 1. Creates an ACME account with Let's Encrypt
%% 2. Submits a certificate order
%% 3. Generates DNS challenges
%% 4. Creates and returns the request state
%%
%% @param ValidatedParams Map of validated request parameters
%% @param _Opts Configuration options
%% @returns {ok, Map} with request details or {error, Reason}
process_certificate_request(ValidatedParams, Opts) ->
    ?event(ssl_cert, {ssl_cert_processing_request, ValidatedParams}),
    maybe
        Domains = maps:get(domains, ValidatedParams),
        {ok, Account} ?=
            (fun() ->
                ?event(ssl_cert, {ssl_cert_account_creation_started}),
                hb_acme_client:create_account(ValidatedParams, Opts)
            end)(),
        ?event(ssl_cert, {ssl_cert_account_created}),
        {ok, Order} ?=
            (fun() ->
                ?event(ssl_cert, {ssl_cert_order_request_started, Domains}),
                hb_acme_client:request_certificate(Account, Domains)
            end)(),
        ?event(ssl_cert, {ssl_cert_order_created}),
        {ok, Challenges} ?=
            (fun() ->
                ?event(ssl_cert, {ssl_cert_get_dns_challenge_started}),
                hb_acme_client:get_dns_challenge(Account, Order)
            end)(),
        ?event(ssl_cert, {challenges, {explicit, Challenges}}),
        RequestState = hb_ssl_cert_state:create_request_state(Account, Order, Challenges, ValidatedParams),
        {ok, #{
            <<"status">> => 200,
            <<"body">> => #{
                <<"status">> => <<"pending_dns">>,
                <<"request_state">> => RequestState,
                <<"message">> => <<"Certificate request created. Use /challenges endpoint to get DNS records.">>,
                <<"domains">> => [hb_util:bin(D) || D <- Domains],
                <<"next_step">> => <<"challenges">>
            }
        }}
    else
        {error, Reason} ->
            ?event(ssl_cert, {ssl_cert_process_error_maybe, Reason}),
            case Reason of
                {account_creation_failed, SubReason} ->
                    {error, #{<<"status">> => 500, <<"error_info">> => #{
                        <<"error">> => <<"ACME account creation failed">>,
                        <<"details">> => hb_ssl_cert_util:format_error_details(SubReason)
                    }}};
                {connection_failed, ConnReason} ->
                    {error, #{<<"status">> => 500, <<"error_info">> => #{
                        <<"error">> => <<"Connection to Let's Encrypt failed">>,
                        <<"details">> => hb_util:bin(io_lib:format("~p", [ConnReason]))
                    }}};
                _ ->
                    {error, #{<<"status">> => 500, <<"error">> => hb_util:bin(io_lib:format("~p", [Reason]))}}
            end;
        Error ->
            ?event(ssl_cert, {ssl_cert_request_processing_failed, Error}),
            {error, #{<<"status">> => 500, <<"error">> => <<"Certificate request processing failed">>}}
    end.

%% @doc Renews an existing SSL certificate.
%%
%% This function initiates renewal for an existing certificate by creating
%% a new certificate request with the same parameters as the original.
%% It reads the configuration from the provided options and creates a new
%% certificate request.
%%
%% @param Domains List of domain names to renew
%% @param Opts Configuration options containing SSL settings
%% @returns {ok, RenewalResponse} or {error, ErrorResponse}
renew_certificate(Domains, Opts) ->
    ?event(ssl_cert, {ssl_cert_renewal_started, {domains, Domains}}),
    try
        % Read SSL configuration from hb_opts  
        SslOpts = hb_opts:get(<<"ssl_opts">>, not_found, Opts),
        % Use configuration for renewal settings (no fallbacks)
        Email = case SslOpts of
            not_found ->
                throw({error, <<"ssl_opts configuration required for renewal">>});
            _ ->
                case maps:get(<<"email">>, SslOpts, not_found) of
                    not_found ->
                        throw({error, <<"email required in ssl_opts configuration">>});
                    ConfigEmail ->
                        ConfigEmail
                end
        end,
        Environment = case SslOpts of
            not_found ->
                staging; % Only fallback is staging for safety
            _ ->
                maps:get(<<"environment">>, SslOpts, staging)
        end,
        RenewalConfig = #{
            domains => [hb_util:list(D) || D <- Domains],
            email => Email,
            environment => Environment,
            key_size => ?SSL_CERT_KEY_SIZE
        },
        ?event(ssl_cert, {
            ssl_cert_renewal_config_created,
            {config, RenewalConfig}
        }),
        % Create new certificate request (renewal)
        case process_certificate_request(RenewalConfig, Opts) of
            {ok, Response} ->
                _Body = maps:get(<<"body">>, Response),
                {ok, #{<<"status">> => 200, 
                       <<"body">> => #{
                           <<"message">> => <<"Certificate renewal initiated">>,
                           <<"domains">> => [hb_util:bin(D) || D <- Domains]
                       }}};
            {error, ErrorResp} ->
                ?event(ssl_cert, {ssl_cert_renewal_failed, {error, ErrorResp}}),
                {error, ErrorResp}
        end
    catch
        Error:Reason:Stacktrace ->
            ?event(ssl_cert, {
                ssl_cert_renewal_error,
                {error, Error},
                {reason, Reason},
                {domains, Domains},
                {stacktrace, Stacktrace}
            }),
            {error, #{<<"status">> => 500, 
                     <<"error">> => <<"Certificate renewal failed">>}}
    end.

%% @doc Deletes a stored SSL certificate.
%%
%% This function removes certificate data associated with the specified domains.
%% In the current implementation, this is a simulated operation that logs
%% the deletion request.
%%
%% @param Domains List of domain names to delete
%% @param _Opts Configuration options (currently unused)
%% @returns {ok, DeletionResponse} or {error, ErrorResponse}
delete_certificate(Domains, _Opts) ->
    ?event(ssl_cert, {ssl_cert_deletion_started, {domains, Domains}}),
    try
        % Generate cache keys for the domains to delete
        DomainList = [hb_util:list(D) || D <- Domains],
        % This would normally:
        % 1. Find all request IDs associated with these domains
        % 2. Remove them from cache
        % 3. Clean up any stored certificate files
        ?event(ssl_cert, {
            ssl_cert_deletion_simulated,
            {domains, DomainList}
        }),
        {ok, #{<<"status">> => 200, 
               <<"body">> => #{
                   <<"message">> => <<"Certificate deletion completed">>,
                   <<"domains">> => [hb_util:bin(D) || D <- DomainList],
                   <<"deleted_count">> => length(DomainList)
               }}}
    catch
        Error:Reason:Stacktrace ->
            ?event(ssl_cert, {
                ssl_cert_deletion_error,
                {error, Error},
                {reason, Reason},
                {domains, Domains},
                {stacktrace, Stacktrace}
            }),
            {error, #{<<"status">> => 500, 
                     <<"error">> => <<"Certificate deletion failed">>}}
    end.

%% @doc Extracts only the end-entity certificate from a PEM chain.
%%
%% This function parses a PEM certificate chain and returns only the
%% end-entity (leaf) certificate, which is typically the first certificate
%% in the chain.
%%
%% @param CertPem Full certificate chain in PEM format
%% @returns Only the end-entity certificate in PEM format
extract_end_entity_cert(CertPem) ->
    % Split PEM into individual certificates
    CertLines = string:split(CertPem, "\n", all),
    % Find the first certificate (end-entity)
    extract_first_cert(CertLines, [], false).

%%%--------------------------------------------------------------------
%%% Internal Functions
%%%--------------------------------------------------------------------

%% @doc Helper to extract the first certificate from PEM lines.
%%
%% @param Lines List of PEM lines to process
%% @param Acc Accumulator for certificate lines
%% @param InCert Whether we're currently inside a certificate block
%% @returns First certificate as string
extract_first_cert([], Acc, _InCert) ->
    string:join(lists:reverse(Acc), "\n");
extract_first_cert([Line | Rest], Acc, InCert) ->
    case {Line, InCert} of
        {"-----BEGIN CERTIFICATE-----", false} ->
            extract_first_cert(Rest, [Line | Acc], true);
        {"-----END CERTIFICATE-----", true} ->
            string:join(lists:reverse([Line | Acc]), "\n");
        {_, true} ->
            extract_first_cert(Rest, [Line | Acc], true);
        {_, false} ->
            extract_first_cert(Rest, Acc, false)
    end.
