%%% @doc SSL Certificate device for automated Let's Encrypt certificate 
%%% management using DNS-01 challenges.
%%%
%%% This device provides HTTP endpoints for requesting, managing, and renewing
%%% SSL certificates through Let's Encrypt's ACME v2 protocol. It supports
%%% both staging and production environments and handles the complete 
%%% certificate lifecycle including DNS challenge generation and validation.
%%%
%%% The device generates DNS TXT records that users must manually add to their
%%% DNS providers, making it suitable for environments where automated DNS
%%% API access is not available.
%%%
%%% This module serves as the main device interface, orchestrating calls to
%%% specialized modules for validation, state management, challenge handling,
%%% and certificate operations.
-module(dev_ssl_cert).

-include("include/hb.hrl").
-include_lib("ssl_cert/include/ssl_cert.hrl").

%% Device API exports
-export([info/1, info/3, request/3, finalize/3]).
-export([renew/3, delete/3]).

%% @doc Controls which functions are exposed via the device API.
%%
%% This function defines the security boundary for the SSL certificate device
%% by explicitly listing which functions are available through HTTP endpoints.
%%
%% @param _ Ignored parameter
%% @returns A map with the `exports' key containing a list of allowed functions
info(_) -> 
    #{ 
        exports => [
            info,
            request,
            finalize,
            renew,
            delete
        ] 
    }.

%% @doc Provides information about the SSL certificate device and its API.
%%
%% This function returns detailed documentation about the device, including:
%% 1. A high-level description of the device's purpose
%% 2. Version information  
%% 3. Available API endpoints with their parameters and descriptions
%% 4. Configuration requirements and examples
%%
%% @param _Msg1 Ignored parameter
%% @param _Msg2 Ignored parameter  
%% @param _Opts A map of configuration options
%% @returns {ok, Map} containing the device information and documentation
info(_Msg1, _Msg2, _Opts) ->
    InfoBody = #{
        <<"description">> => 
            <<"SSL Certificate management with Let's Encrypt DNS-01 challenges">>,
        <<"version">> => <<"1.0">>,
        <<"api">> => #{
            <<"info">> => #{
                <<"description">> => <<"Get device info and API documentation">>
            },
            <<"request">> => #{
                <<"description">> => <<"Request a new SSL certificate">>,
                <<"configuration_required">> => #{
                    <<"ssl_opts">> => #{
                        <<"domains">> => <<"List of domain names for certificate">>,
                        <<"email">> => <<"Contact email for Let's Encrypt account">>,
                        <<"environment">> => <<"'staging' or 'production'">>
                    }
                },
                <<"example_config">> => #{
                    <<"ssl_opts">> => #{
                        <<"domains">> => [<<"example.com">>, <<"www.example.com">>],
                        <<"email">> => <<"admin@example.com">>,
                        <<"environment">> => <<"staging">>
                    }
                },
                <<"usage">> => <<"POST /ssl-cert@1.0/request (returns challenges; state saved internally)">>
            },
            <<"finalize">> => #{
                <<"description">> => <<"Finalize certificate issuance after DNS TXT records are set">>,
                <<"usage">> => <<"POST /ssl-cert@1.0/finalize (validates and returns certificate)">>,
                <<"auto_https">> => <<"Automatically starts HTTPS server and redirects HTTP traffic (default: true)">>,
                <<"https_port">> => <<"Configurable HTTPS port (default: 8443 for development, set to 443 for production)">>
            },
            <<"renew">> => #{
                <<"description">> => <<"Renew an existing certificate">>,
                <<"required_params">> => #{
                    <<"domains">> => <<"List of domain names to renew">>
                }
            },
            <<"delete">> => #{
                <<"description">> => <<"Delete a stored certificate">>,
                <<"required_params">> => #{
                    <<"domains">> => <<"List of domain names to delete">>
                }
            }
        }
    },
    ssl_utils:build_success_response(200, InfoBody).

%% @doc Requests a new SSL certificate for the specified domains.
%%
%% This function initiates the certificate request process:
%% 1. Validates the input parameters (domains, email, environment)
%% 2. Creates or retrieves an ACME account with Let's Encrypt
%% 3. Submits a certificate order for the specified domains
%% 4. Generates DNS-01 challenges for domain validation
%% 5. Stores the request state for subsequent operations
%% 6. Returns a request ID and initial status
%%
%% Required parameters in ssl_opts configuration:
%% - domains: List of domain names for the certificate
%% - email: Contact email for Let's Encrypt account registration
%% - environment: 'staging' or 'production' (use staging for testing)
%%
%% @param _M1 Ignored parameter
%% @param _M2 Request message containing certificate parameters
%% @param Opts A map of configuration options
%% @returns {ok, Map} with request ID and status, or {error, Reason}
request(_M1, _M2, Opts) ->
    ?event({ssl_cert_request_started}),
    maybe
        LoadedOpts = hb_cache:ensure_all_loaded(Opts, Opts),
        StrippedOpts = maps:without([<<"ssl_cert_rsa_key">>, <<"ssl_cert_opts">>], LoadedOpts),
        ?event({ssl_cert_request_started_with_opts, StrippedOpts}),
        % Extract SSL options from configuration
        {ok, SslOpts} ?= extract_ssl_opts(StrippedOpts),
        % Extract and validate parameters
                Domains = maps:get(<<"domains">>, SslOpts, not_found),
                Email = maps:get(<<"email">>, SslOpts, not_found),
                Environment = maps:get(<<"environment">>, SslOpts, staging),
                ?event({
                    ssl_cert_request_params_from_config,
                    {domains, Domains},
                    {email, Email},
                    {environment, Environment}
                }),
        % Validate all parameters
        {ok, ValidatedParams} ?= 
            ssl_cert_validation:validate_request_params(Domains, Email, Environment),
                        EnhancedParams = ValidatedParams#{
            key_size => ?SSL_CERT_KEY_SIZE,
            storage_path => ?SSL_CERT_STORAGE_PATH
        },
        % Process the certificate request
        Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
        {ok, ProcResp} ?= 
            ssl_cert_ops:process_certificate_request(EnhancedParams, Wallet),
        NewOpts = hb_http_server:get_opts(Opts),
        ProcBody = maps:get(<<"body">>, ProcResp, #{}),
        RequestState0 = maps:get(<<"request_state">>, ProcBody, #{}),
        CertificateKey = maps:get(<<"certificate_key">>, ProcBody, not_found),
        ?event({ssl_cert_orchestration_created_request}),
        % Persist request state in node opts (overwrites previous)
        ok = hb_http_server:set_opts(
            NewOpts#{ <<"ssl_cert_request">> => RequestState0, <<"ssl_cert_rsa_key">> => CertificateKey }
        ),
        % Format challenges for response
        Challenges = maps:get(<<"challenges">>, RequestState0, []),
        FormattedChallenges = ssl_cert_challenge:format_challenges_for_response(Challenges),
        % Return challenges and request_state to the caller
        {ok, #{<<"status">> => 200,
               <<"body">> => #{
                   <<"message">> => 
                        <<"Create DNS TXT records for the following challenges, then call finalize">>,
                   <<"challenges">> => FormattedChallenges,
                   <<"next_step">> => <<"finalize">>
               }}}
    else
        {error, <<"ssl_opts configuration required">>} ->
            ssl_utils:build_error_response(400, <<"ssl_opts configuration required">>);
        {error, ReasonBin} when is_binary(ReasonBin) ->
            ssl_utils:format_validation_error(ReasonBin);
                    {error, Reason} ->
            ?event({ssl_cert_request_error_maybe, Reason}),
            FormattedError = ssl_utils:format_error_details(Reason),
            ssl_utils:build_error_response(500, FormattedError);
        Error ->
            ?event({ssl_cert_request_unexpected_error, Error}),
            ssl_utils:build_error_response(500, <<"Internal server error">>)
    end.

%% @doc Finalizes a certificate request: validates challenges and downloads the certificate.
%%
%% This function:
%% 1. Retrieves the stored request state
%% 2. Validates DNS challenges with Let's Encrypt
%% 3. Finalizes the order if challenges are valid
%% 4. Downloads the certificate if available
%% 5. Automatically starts HTTPS server on port 443 (if auto_https is enabled)
%% 6. Configures HTTP server to redirect to HTTPS
%% 7. Returns the certificate and HTTPS server status
%%
%% The auto_https feature (enabled by default) will:
%% - Start a new HTTPS listener on port 443 using the issued certificate
%% - Reconfigure the existing HTTP server to send 301 redirects to HTTPS
%% - Preserve all existing server configuration and functionality
%%
%% @param _M1 Ignored
%% @param _M2 Message containing request_state
%% @param Opts Options (supports auto_https: true/false)
%% @returns {ok, Map} result of validation and optionally certificate
finalize(_M1, _M2, Opts) ->
    ?event({ssl_cert_finalize_started}),
    maybe
        % Load single saved request state from node opts
        RequestState = hb_opts:get(<<"ssl_cert_request">>, not_found, Opts),
        _ ?= case RequestState of
            not_found -> {error, request_state_not_found};
            _ when is_map(RequestState) -> {ok, true};
            _ -> {error, invalid_request_state}
        end,
        PrivKeyRecord = hb_opts:get(<<"ssl_cert_rsa_key">>, not_found, Opts),
        % Validate DNS challenges
        {ok, ValResp} ?= ssl_cert_challenge:validate_dns_challenges_state(RequestState, PrivKeyRecord),
        ValBody = maps:get(<<"body">>, ValResp, #{}),
        OrderStatus = maps:get(<<"order_status">>, ValBody, <<"unknown">>),
        Results = maps:get(<<"results">>, ValBody, []),
        RequestState1 = maps:get(<<"request_state">>, ValBody, RequestState),
        % Handle different order statuses
        case OrderStatus of
            ?ACME_STATUS_VALID ->
                % Try to download the certificate
                case ssl_cert_ops:download_certificate_state(RequestState1, Opts) of
                    {ok, DownResp} ->
                        ?event(ssl_cert, {ssl_cert_certificate_downloaded, DownResp}),
                        DownBody = maps:get(<<"body">>, DownResp, #{}),
                        CertPem = maps:get(<<"certificate_pem">>, DownBody, <<>>),
                        DomainsOut = maps:get(<<"domains">>, DownBody, []),
                        % Get the CSR private key from saved opts and serialize to PEM
                        PrivKeyPem = case PrivKeyRecord of
                            not_found -> <<"">>;
                            Key -> ssl_cert_state:serialize_private_key(Key)
                        end,
                        ?event(ssl_cert, {ssl_cert_certificate_and_key_ready_for_nginx, {domains, DomainsOut}}),
                        
                        % Start HTTPS server with the new certificate and build response
                        case hb_opts:get(<<"auto_https">>, true, Opts) of
                            true ->
                                ?event(ssl_cert, {starting_https_server_with_certificate, {domains, DomainsOut}}),
                                HttpsPortFromOpts = hb_opts:get(https_port, not_found, Opts),
                                ?event(ssl_cert, {https_port_config_check, {https_port_in_opts, HttpsPortFromOpts}, {opts_keys, maps:keys(Opts)}}),
                                StrippedOpts = maps:without([port], Opts),
                                try hb_http_server:start_https_node(CertPem, hb_util:bin(PrivKeyPem), StrippedOpts#{ priv_wallet => ar_wallet:new(), port => HttpsPortFromOpts}) of
                                    ServerUrl when is_binary(ServerUrl) ->
                                        ?event(ssl_cert, {https_server_started_successfully, {server_url, ServerUrl}, {domains, DomainsOut}}),
                                        ResponseBody = #{
                                            <<"message">> => <<"Certificate issued successfully">>,
                                            <<"domains">> => DomainsOut,
                                            <<"results">> => Results,
                                            % TODO: Remove Keys from response
                                            <<"certificate_pem">> => CertPem,
                                            <<"key_pem">> => hb_util:bin(PrivKeyPem),
                                            <<"https_server">> => #{
                                                <<"status">> => <<"started">>,
                                                <<"server_url">> => ServerUrl,
                                                <<"message">> => iolist_to_binary([
                                                    <<"HTTPS server started at ">>, 
                                                    ServerUrl, 
                                                    <<", HTTP traffic will be redirected">>
                                                ])
                                            }
                                        },
                                        {ok, #{<<"status">> => 200, <<"body">> => ResponseBody}}
                                catch
                                    Error:Reason:Stacktrace ->
                                        ?event(ssl_cert, {https_server_start_failed, {error, Error}, {reason, Reason}, {stacktrace, Stacktrace}, {domains, DomainsOut}}),
                                        ResponseBody = #{
                                            <<"message">> => <<"Certificate issued successfully">>,
                                            <<"domains">> => DomainsOut,
                                            <<"results">> => Results,
                                            % TODO: Remove Keys from response
                                            <<"certificate_pem">> => CertPem,
                                            <<"key_pem">> => hb_util:bin(PrivKeyPem),
                                            <<"https_server">> => #{
                                                <<"status">> => <<"failed">>,
                                                <<"error">> => hb_util:bin(hb_format:term({Error, Reason})),
                                                <<"message">> => <<"Certificate issued but HTTPS server failed to start">>
                                            }
                                        },
                                        {ok, #{<<"status">> => 200, <<"body">> => ResponseBody}}
                                end;
                            false ->
                                ?event(ssl_cert, {auto_https_disabled, {domains, DomainsOut}}),
                                ResponseBody = #{
                                    <<"message">> => <<"Certificate issued successfully">>,
                                    <<"domains">> => DomainsOut,
                                    <<"results">> => Results,
                                    % TODO: Remove Keys from response
                                    <<"certificate_pem">> => CertPem,
                                    <<"key_pem">> => hb_util:bin(PrivKeyPem),
                                    <<"https_server">> => #{
                                        <<"status">> => <<"skipped">>,
                                        <<"reason">> => <<"auto_https_disabled">>,
                                        <<"message">> => <<"Certificate issued, HTTPS server not started (auto_https disabled)">>
                                    }
                                },
                                {ok, #{<<"status">> => 200, <<"body">> => ResponseBody}}
                        end;
                    {error, _} ->
                        {ok, #{<<"status">> => 200,
                               <<"body">> => #{
                                   <<"message">> => <<"Order finalized; certificate not ready for download yet">>,
                                   <<"order_status">> => ?ACME_STATUS_PROCESSING,
                                   <<"results">> => Results
                               }}}
                end;
            _ ->
                {ok, #{<<"status">> => 200,
                       <<"body">> => #{
                           <<"message">> => <<"Validation not complete">>,
                           <<"order_status">> => OrderStatus,
                           <<"results">> => Results,
                           <<"request_state">> => RequestState1
                       }}}
        end
    else
        {error, request_state_not_found} ->
            ssl_utils:build_error_response(404, <<"request state not found">>);
        {error, invalid_request_state} ->
            ssl_utils:build_error_response(400, <<"request_state must be a map">>);
        {error, FinalReason} ->
            FormattedError = ssl_utils:format_error_details(FinalReason),
            ssl_utils:build_error_response(500, FormattedError)
    end.


%% @doc Renews an existing SSL certificate.
%%
%% This function initiates renewal for an existing certificate:
%% 1. Validates the domains parameter
%% 2. Retrieves the existing certificate configuration
%% 3. Initiates a new certificate request with the same parameters
%% 4. Returns a new request ID for the renewal process
%%
%% Required parameters in ssl_opts configuration:
%% - domains: List of domain names to renew
%% - email: Contact email for Let's Encrypt account
%% - environment: ACME environment setting
%%
%% @param _M1 Ignored parameter
%% @param M2 Request message containing domains to renew
%% @param Opts A map of configuration options
%% @returns {ok, Map} with renewal request ID, or {error, Reason}
renew(_M1, _M2, Opts) ->
    ?event({ssl_cert_renewal_started}),
    try
        % Extract SSL options and validate
        case extract_ssl_opts(Opts) of
            {error, ErrorReason} ->
                ssl_utils:build_error_response(400, ErrorReason);
            {ok, SslOpts} ->
                Domains = maps:get(<<"domains">>, SslOpts, not_found),
                case Domains of
                    not_found ->
                        ?event({ssl_cert_renewal_domains_missing}),
                        ssl_utils:build_error_response(400, 
                            <<"domains required in ssl_opts configuration">>);
                    _ ->
                        DomainList = ssl_utils:normalize_domains(Domains),
                        ssl_cert_ops:renew_certificate(DomainList, Opts)
                end
        end
    catch
        Error:CatchReason:Stacktrace ->
            ?event({ssl_cert_renewal_error, Error, CatchReason, Stacktrace}),
            ssl_utils:build_error_response(500, <<"Internal server error">>)
    end.

%% @doc Deletes a stored SSL certificate.
%%
%% This function removes a certificate from storage:
%% 1. Validates the domains parameter
%% 2. Locates the certificate in storage
%% 3. Removes the certificate files and metadata
%% 4. Returns confirmation of deletion
%%
%% Required parameters in ssl_opts configuration:
%% - domains: List of domain names to delete
%%
%% @param _M1 Ignored parameter
%% @param M2 Request message containing domains to delete
%% @param Opts A map of configuration options
%% @returns {ok, Map} with deletion confirmation, or {error, Reason}
delete(_M1, _M2, Opts) ->
    ?event({ssl_cert_deletion_started}),
    try
        % Extract SSL options and validate
        case extract_ssl_opts(Opts) of
            {error, ErrorReason} ->
                ssl_utils:build_error_response(400, ErrorReason);
            {ok, SslOpts} ->
                Domains = maps:get(<<"domains">>, SslOpts, not_found),
                case Domains of
                    not_found ->
                        ?event({ssl_cert_deletion_domains_missing}),
                        ssl_utils:build_error_response(400, 
                            <<"domains required in ssl_opts configuration">>);
                    _ ->
                        DomainList = ssl_utils:normalize_domains(Domains),
                        ssl_cert_ops:delete_certificate(DomainList, Opts)
                end
        end
    catch
        Error:CatchReason:Stacktrace ->
            ?event({ssl_cert_deletion_error, Error, CatchReason, Stacktrace}),
            ssl_utils:build_error_response(500, <<"Internal server error">>)
    end.

%% @doc Extracts SSL options from configuration with validation.
%%
%% This function extracts and validates the ssl_opts configuration from
%% the provided options map, ensuring all required fields are present.
%%
%% @param Opts Configuration options map
%% @returns {ok, SslOpts} or {error, Reason}
extract_ssl_opts(Opts) when is_map(Opts) ->
    case hb_opts:get(<<"ssl_opts">>, not_found, Opts) of
        not_found ->
            {error, <<"ssl_opts configuration required">>};
        SslOpts when is_map(SslOpts) ->
            {ok, SslOpts};
        _ ->
            {error, <<"ssl_opts must be a map">>}
    end.
