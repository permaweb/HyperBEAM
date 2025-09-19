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
-export([get_cert/3, request_cert/3]).

-define(CERT_DIR, filename:join([element(2, file:get_cwd()), "certs"])).
-define(CERT_PEM_FILE, 
    filename:join(
        [?CERT_DIR, <<"hyperbeam_cert.pem">>]
    )
).
-define(KEY_PEM_FILE,
    filename:join(
        [?CERT_DIR, <<"hyperbeam_key.pem">>]
    )
).
-define(DEFAULT_HTTPS_PORT, 443).

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
            <<"info">>,
            <<"request">>,
            <<"finalize">>,
            <<"renew">>,
            <<"delete">>,
            <<"get_cert">>,
            <<"request_cert">>
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
            <<
                "SSL Certificate management with", 
                "Let's Encrypt DNS-01 challenges"
            >>,
        <<"version">> => <<"1.0">>,
        <<"api">> => #{
            <<"info">> => #{
                <<"description">> => 
                    <<"Get device info and API documentation">>
            },
            <<"request">> => #{
                <<"description">> => <<"Request a new SSL certificate">>,
                <<"configuration_required">> => #{
                    <<"ssl_opts">> => #{
                        <<"domains">> => 
                            <<"List of domain names for certificate">>,
                        <<"email">> => 
                            <<"Contact email for Let's Encrypt account">>,
                        <<"environment">> => 
                            <<"'staging' or 'production'">>,
                        <<"auto_https">> =>
                            <<
                                "Automatically start HTTPS server and",
                                "redirect HTTP traffic (default: true)"
                            >>,
                        <<"https_port">> => <<"HTTPS port (default: 443)">>
                    }
                },
                <<"example_config">> => #{
                    <<"ssl_opts">> => #{
                        <<"domains">> => 
                            [<<"example.com">>, <<"www.example.com">>],
                        <<"email">> => <<"admin@example.com">>,
                        <<"environment">> => <<"staging">>,
                        <<"auto_https">> => <<"true">>,
                        <<"https_port">> => <<"443">>
                    }
                },
                <<"usage">> =>
                    <<
                        "POST /ssl-cert@1.0/request",
                        " (returns challenges; state saved internally)"
                    >>
            },
            <<"finalize">> => #{
                <<"description">> =>
                    <<
                        "Finalize certificate issuance",
                        "after DNS TXT records are set"
                    >>,
                <<"usage">> =>
                    <<
                        "POST /ssl-cert@1.0/finalize",
                        " (validates and returns certificate)"
                    >>,
                <<"auto_https">> =>
                    <<
                        "Automatically starts HTTPS server and redirects",
                        "HTTP traffic (default: true)"
                    >>,
                <<"https_port">> =>
                    <<
                        "Configurable HTTPS port (default: 8443 for",
                        "development, set to 443 for production)"
                    >>
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
            },
            <<"get_cert">> => #{
                <<"description">> => 
                    <<"Get encrypted certificate and private key for sharing">>,
                <<"usage">> => <<"POST /ssl-cert@1.0/get_cert">>,
                <<"note">> => 
                    <<
                        "Returns encrypted certificate data that can be used by",
                        "another node with the same green zone AES key"
                    >>
            },
            <<"request_cert">> => #{
                <<"description">> => 
                    <<"Request and use certificate from another node">>,
                <<"required_params">> => #{
                    <<"peer_location">> => <<"URL of the peer node">>,
                    <<"peer_id">> => <<"ID of the peer node">>
                },
                <<"usage">> => <<"POST /ssl-cert@1.0/request_cert">>,
                <<"note">> =>
                    <<
                        "Automatically starts HTTPS server with the retrieved",
                        "certificate"
                    >>
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
        {ok, ValidatedParams} ?= 
            extract_and_validate_ssl_params(Opts),
        {ok, {RequestState, ChallengeData}} ?= 
            process_certificate_request_workflow(ValidatedParams, Opts),
        build_request_response(RequestState, ChallengeData)
    else
        {error, <<"ssl_opts configuration required">>} ->
            ssl_utils:build_error_response(
                400, 
                <<"ssl_opts configuration required">>
            );
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

%% @doc Finalizes a certificate request: validates challenges and downloads 
%% the certificate.
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
        {ok, {RequestState, PrivKeyRecord}} ?= 
            load_certificate_state(Opts),
        {ok, {OrderStatus, Results, RequestState1}} ?= 
            validate_challenges(RequestState, PrivKeyRecord),
        case OrderStatus of
            ?ACME_STATUS_VALID ->
                handle_valid_certificate(
                    RequestState1, 
                    PrivKeyRecord, 
                    Results, 
                    Opts
                );
            _ ->
                build_pending_response(OrderStatus, Results, RequestState1)
        end
    else
        {error, request_state_not_found} ->
            ssl_utils:build_error_response(
                404, 
                <<"request state not found">>
            );
        {error, invalid_request_state} ->
            ssl_utils:build_error_response(
                400, 
                <<"request_state must be a map">>
            );
        {error, Reason} ->
            FormattedError = ssl_utils:format_error_details(Reason),
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
                        ssl_utils:build_error_response(
                            400,
                            <<"domains required in ssl_opts configuration">>
                        );
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
                        ssl_utils:build_error_response(
                            400,
                            <<"domains required in ssl_opts configuration">>
                        );
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

%% @doc Get encrypted certificate and private key for sharing with other nodes.
%%
%% This function encrypts the current certificate and private key using the
%% shared green zone AES key, similar to how the green zone shares wallet keys.
%% The encrypted data can be requested by another node that has the same
%% green zone AES key.
%%
%% @param _M1 Ignored parameter
%% @param _M2 Ignored parameter  
%% @param Opts Server configuration options
%% @returns {ok, Map} with encrypted certificate data, or {error, Reason}
get_cert(_M1, _M2, Opts) ->
    ?event(ssl_cert, {get_cert, start}),
    maybe
        {ok, CertPem} ?= file:read_file(?CERT_PEM_FILE),
        {ok, KeyPem} ?= file:read_file(?KEY_PEM_FILE),
        % Create combined certificate data
        CertData = #{
            cert_pem => CertPem,
            key_pem => KeyPem,
            timestamp => erlang:system_time(second)
        },
        % Encrypt using green zone helper function
        {ok, {EncryptedData, IV}} ?= 
            dev_green_zone:encrypt_data(CertData, Opts),
        ?event(ssl_cert, {get_cert, encrypt, complete}),
        ssl_utils:build_success_response(200, #{
            <<"encrypted_cert">> => base64:encode(EncryptedData),
            <<"iv">> => base64:encode(IV),
            <<"message">> => 
                <<"Certificate encrypted and ready for sharing">>
        })
    else
        {error, enoent} ->
            ?event(ssl_cert, {get_cert, file_not_found}),
            ssl_utils:build_error_response(
                404, 
                <<"Certificate or key file not found">>
            );
        {error, no_green_zone_aes_key} ->
            ?event(ssl_cert, {get_cert, error, <<"no aes key">>}),
            ssl_utils:build_error_response(
                400, 
                <<"Node not part of a green zone - no shared AES key">>
            );
        {error, EncryptError} ->
            ?event(ssl_cert, {get_cert, encrypt_error, EncryptError}),
            ssl_utils:build_error_response(500, <<"Encryption failed">>);
        Error ->
            ?event(ssl_cert, {get_cert, unexpected_error, Error}),
            ssl_utils:build_error_response(500, <<"Internal server error">>)
    end.

%% @doc Request certificate from another node and start HTTPS server.
%%
%% This function requests encrypted certificate data from another node,
%% decrypts it using the shared green zone AES key, and automatically
%% starts an HTTPS server with the retrieved certificate.
%%
%% Required parameters:
%% - peer_location: URL of the peer node
%% - peer_id: ID of the peer node for verification
%%
%% @param _M1 Ignored parameter
%% @param _M2 Request message containing peer information
%% @param Opts Server configuration options
%% @returns {ok, Map} with certificate status and HTTPS server info, or 
%%          {error, Reason}
request_cert(_M1, _M2, Opts) ->
    ?event(ssl_cert, {request_cert, start}),
    % Extract peer information
    PeerLocation = hb_opts:get(<<"peer_location">>, undefined, Opts),
    PeerID = hb_opts:get(<<"peer_id">>, undefined, Opts),
    case {PeerLocation, PeerID} of
        {undefined, _} ->
            ssl_utils:build_error_response(
                400, 
                <<"peer_location required">>
            );
        {_, undefined} ->
            ssl_utils:build_error_response(
                400, 
                <<"peer_id required">>
            );
        {_, _} ->
            try_request_cert_from_peer(PeerLocation, PeerID, Opts)
    end.

%%% ===================================================================
%%% Internal Helper Functions
%%% ===================================================================

%% @doc Try to request certificate from peer node.
%%
%% This function makes an HTTP request to the peer node's get_cert endpoint,
%% verifies the response signature, decrypts the certificate data, and
%% starts an HTTPS server with the retrieved certificate.
%%
%% @param PeerLocation URL of the peer node
%% @param PeerID Expected signer ID for verification
%% @param Opts Server configuration options
%% @returns {ok, Map} with certificate status, or {error, Reason}
try_request_cert_from_peer(PeerLocation, PeerID, Opts) ->
    maybe
        ?event(ssl_cert, {request_cert, getting_cert, PeerLocation, PeerID}),
        % Request encrypted certificate from peer
        {ok, CertResp} ?= hb_http:get(PeerLocation, 
                                     <<"/~ssl-cert@1.0/get_cert">>, Opts),
        % Verify response signature
        Signers = hb_message:signers(CertResp, Opts),
        true ?= (hb_message:verify(CertResp, Signers, Opts) and 
                 lists:member(PeerID, Signers)),
        finalize_cert_request(CertResp, Opts)
    else
        false ->
            ?event(ssl_cert, {request_cert, invalid_signature}),
            ssl_utils:build_error_response(
                400, 
                <<"Invalid response signature from peer">>
            );
        Error ->
            ?event(ssl_cert, {request_cert, error, Error}),
            ssl_utils:build_error_response(
                500, 
                <<"Failed to request certificate from peer">>
            )
    end.

%% @doc Finalize certificate request by decrypting and using the certificate.
%%
%% This function decrypts the certificate data received from the peer,
%% writes it to local files, and starts an HTTPS server.
%%
%% @param CertResp Response from peer containing encrypted certificate
%% @param Opts Server configuration options
%% @returns {ok, Map} with HTTPS server status
finalize_cert_request(CertResp, Opts) ->
    maybe
        % Extract encrypted data from response
        Body = hb_ao:get(<<"body">>, CertResp, Opts),
        Combined = 
            base64:decode(hb_ao:get(<<"encrypted_cert">>, Body, Opts)),
        IV = base64:decode(hb_ao:get(<<"iv">>, Body, Opts)),
        % Decrypt using green zone helper function
        {ok, DecryptedBin} ?= dev_green_zone:decrypt_data(Combined, IV, Opts),
        % Extract certificate components
        #{cert_pem := CertPem, key_pem := KeyPem, timestamp := Timestamp} = 
            binary_to_term(DecryptedBin),
        ?event(
            ssl_cert, 
            {request_cert, decrypted_cert, {timestamp, Timestamp}}
        ),
        % Write certificate files
        {ok, {CertFile, KeyFile}} ?= write_certificate_files(CertPem, KeyPem),
        ?event(ssl_cert, {request_cert, files_written, {CertFile, KeyFile}}),
        % Start HTTPS server with the certificate
        HttpsPort = hb_opts:get(<<"https_port">>, ?DEFAULT_HTTPS_PORT, Opts),
        RedirectTo = get_redirect_server_id(Opts),
        HttpsResult = try hb_http_server:start_https_node(
            CertFile, 
            KeyFile, 
            Opts, 
            RedirectTo,
            HttpsPort
        ) of
            ServerUrl when is_binary(ServerUrl) ->
                ?event(ssl_cert, {request_cert, https_started, ServerUrl}),
                {started, ServerUrl}
        catch
            StartError:StartReason:StartStacktrace ->
                ?event(ssl_cert, 
                    {
                        request_cert, https_failed, 
                        {error, StartError},
                        {reason, StartReason},
                        {stacktrace, StartStacktrace}
                    }
                ),
                {failed, {StartError, StartReason}}
        end,
        % Build response
        ssl_utils:build_success_response(200, #{
            <<"message">> => 
                <<"Certificate retrieved and HTTPS server started">>,
            <<"https_server">> => format_https_server_status(HttpsResult),
            <<"certificate_timestamp">> => Timestamp
        })
    else
        {error, no_green_zone_aes_key} ->
            ?event(ssl_cert, {request_cert, error, <<"no aes key">>}),
            ssl_utils:build_error_response(
                400, 
                <<"Node not part of a green zone - no shared AES key">>
            );
        {error, DecryptError} ->
            ?event(ssl_cert, {request_cert, decrypt_error, DecryptError}),
            ssl_utils:build_error_response(
                400, 
                <<"Failed to decrypt certificate data">>
            );
        Error ->
            ?event(ssl_cert, {request_cert, general_error, Error}),
            ssl_utils:build_error_response(
                500, 
                <<"Internal server error">>
            )
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

%% @doc Load and validate certificate state from options.
%%
%% This function retrieves the stored certificate request state and private key
%% from the server options, validating that the request state exists and is
%% properly formatted as a map.
%%
%% @param Opts Server configuration options containing ssl_cert_request 
%%             and ssl_cert_rsa_key
%% @returns {ok, {RequestState, PrivKeyRecord}} or {error, Reason}
load_certificate_state(Opts) ->
    RequestState = hb_opts:get(<<"priv_ssl_cert_request">>, not_found, Opts),
    case RequestState of
        not_found ->
            {error, request_state_not_found};
        _ when is_map(RequestState) ->
            PrivKeyRecord = 
                hb_opts:get(<<"priv_ssl_cert_rsa_key">>, not_found, Opts),
            {ok, {RequestState, PrivKeyRecord}};
        _ ->
            {error, invalid_request_state}
    end.

%% @doc Validate DNS challenges and return order status.
%%
%% This function validates the DNS-01 challenges with Let's Encrypt's 
%% ACME server
%% to verify domain ownership. It extracts the order status, validation 
%% results,
%% and updated request state from the validation response.
%%
%% @param RequestState Current certificate request state
%% @param PrivKeyRecord Private key record for challenge validation
%% @returns {ok, {OrderStatus, Results, RequestState1}} or {error, Reason}
validate_challenges(RequestState, PrivKeyRecord) ->
    case ssl_cert_challenge:validate_dns_challenges_state(
        RequestState, 
        PrivKeyRecord
    ) of
        {ok, ValResp} ->
            ValBody = maps:get(<<"body">>, ValResp, #{}),
            OrderStatus = maps:get(<<"order_status">>, ValBody, <<"unknown">>),
            Results = maps:get(<<"results">>, ValBody, []),
            RequestState1 = 
                maps:get(<<"request_state">>, ValBody, RequestState),
            {ok, {OrderStatus, Results, RequestState1}};
        Error ->
            Error
    end.

%% @doc Handle valid certificate: download and optionally start HTTPS server.
%%
%% This function processes a validated certificate order by downloading the
%% certificate from Let's Encrypt, extracting the certificate data, and
%% optionally starting an HTTPS server with the new certificate.
%%
%% @param RequestState Validated certificate request state
%% @param PrivKeyRecord Private key record for the certificate
%% @param Results Validation results from challenge verification
%% @param Opts Server configuration options
%% @returns {ok, Response} with certificate and optional HTTPS server 
%%          status
handle_valid_certificate(RequestState, PrivKeyRecord, Results, Opts) ->
    case ssl_cert_ops:download_certificate_state(RequestState, Opts) of
        {ok, DownResp} ->
            ?event(ssl_cert, {ssl_cert_certificate_downloaded, DownResp}),
            maybe
                {ok, {CertPem, DomainsOut, PrivKeyPem}} ?= 
                    extract_certificate_data(DownResp, PrivKeyRecord),
                ?event(
                    ssl_cert, 
                    {
                        ssl_cert_certificate_and_key_ready_for_nginx, 
                        {domains, DomainsOut}
                    }
                ),
                HttpsResult = 
                    maybe_start_https_server(
                        CertPem, 
                        PrivKeyPem, 
                        DomainsOut, 
                        Opts
                    ),
                build_success_response(
                    DomainsOut, 
                    Results, 
                    HttpsResult
                )
            end;
        {error, _} ->
            build_processing_response(Results)
    end.

%% @doc Extract certificate data from download response.
%%
%% This function extracts the certificate PEM, domain list, and serialized
%% private key from the certificate download response. It handles the case
%% where no private key record is available.
%%
%% @param DownResp Certificate download response from Let's Encrypt
%% @param PrivKeyRecord Private key record (may be not_found)
%% @returns {ok, {CertPem, DomainsOut, PrivKeyPem}}
extract_certificate_data(DownResp, PrivKeyRecord) ->
    DownBody = maps:get(<<"body">>, DownResp, #{}),
    CertPem = maps:get(<<"certificate_pem">>, DownBody, <<>>),
    DomainsOut = maps:get(<<"domains">>, DownBody, []),
    PrivKeyPem =
        case PrivKeyRecord of
            not_found -> <<"">>;
            Key -> ssl_cert_state:serialize_private_key(Key)
        end,
    {ok, {CertPem, DomainsOut, PrivKeyPem}}.

%% @doc Optionally start HTTPS server with certificate.
%%
%% This function checks the auto_https configuration setting and conditionally
%% starts an HTTPS server with the provided certificate. If auto_https is
%% disabled, it skips the server startup.
%%
%% @param CertPem PEM-encoded certificate chain
%% @param PrivKeyPem PEM-encoded private key
%% @param DomainsOut List of domains for the certificate
%% @param Opts Server configuration options (checks auto_https setting)
%% @returns {started, ServerUrl} | {skipped, Reason} | {failed, Error}
maybe_start_https_server(CertPem, PrivKeyPem, DomainsOut, Opts) ->
    {ok, SSLOpts} = extract_and_validate_ssl_params(Opts),
    ?event(ssl_cert, {sslopts, {explicit, SSLOpts}}),
    case hb_opts:get(<<"auto_https">>, true, SSLOpts) of
        true ->
            ?event(
                ssl_cert, 
                {
                    starting_https_server_with_certificate, 
                    {domains, DomainsOut}
                }
            ),
            HttpsPort = hb_opts:get(<<"https_port">>, ?DEFAULT_HTTPS_PORT, SSLOpts),
            start_https_server_with_certificate(
                CertPem, 
                PrivKeyPem, 
                DomainsOut, 
                Opts,
                HttpsPort
            );
        false ->
            ?event(ssl_cert, {auto_https_disabled, {domains, DomainsOut}}),
            {skipped, auto_https_disabled}
    end.

%% @doc Start HTTPS server with certificate files.
%%
%% This function writes the certificate and key to temporary files, determines
%% the HTTP server to redirect from, and starts a new HTTPS server. It handles
%% all aspects of HTTPS server startup including redirect configuration.
%%
%% @param CertPem PEM-encoded certificate chain
%% @param PrivKeyPem PEM-encoded private key
%% @param DomainsOut List of domains for logging and tracking
%% @param Opts Server configuration options
%% @param HttpsPort HTTPS port number for the server
%% @returns {started, ServerUrl} or {failed, {Error, Reason}}
start_https_server_with_certificate(
    CertPem,PrivKeyPem, DomainsOut, Opts, HttpsPort
) ->
    maybe
        {ok, {CertFile, KeyFile}} ?= 
            write_certificate_files(CertPem, PrivKeyPem),
        RedirectTo = get_redirect_server_id(Opts),
        ?event(
            ssl_cert, 
            {
                https_server_config, 
                {cert_file, CertFile}, 
                {key_file, KeyFile}, 
                {redirect_to, RedirectTo},
                {https_port, HttpsPort}
            }
        ),
        try hb_http_server:start_https_node(
            CertFile, 
            KeyFile, 
            Opts, 
            RedirectTo,
            HttpsPort
        ) of
            ServerUrl when is_binary(ServerUrl) ->
                ?event(
                    ssl_cert, 
                    {
                        https_server_started_successfully, 
                        {server_url, ServerUrl}, 
                        {domains, DomainsOut}
                    }
                ),
                {started, ServerUrl}
        catch
            Error:Reason:Stacktrace ->
                ?event(ssl_cert, 
                    {
                        https_server_start_failed, 
                        {error, Error}, 
                        {reason, Reason}, 
                        {stacktrace, Stacktrace}, 
                        {domains, DomainsOut}
                    }
                ),
                {failed, {Error, Reason}}
        end
    end.

%% @doc Write certificate and key to files.
%%
%% This function writes the PEM-encoded certificate and private key to
%% files that can be used by Cowboy for TLS configuration. It ensures
%% the target directory exists before writing files.
%% Both files must be written successfully for the operation to succeed.
%%
%% @param CertPem PEM-encoded certificate chain
%% @param PrivKeyPem PEM-encoded private key
%% @returns {ok, {CertFile, KeyFile}} or {error, Reason}
write_certificate_files(CertPem, PrivKeyPem) ->
    CertFile = ?CERT_PEM_FILE,
    KeyFile = ?KEY_PEM_FILE,
    % Ensure the directory exists
    case filelib:ensure_dir(filename:join(?CERT_DIR, "dummy")) of
        ok ->
            case {
                file:write_file(CertFile, CertPem), 
                file:write_file(KeyFile, ssl_utils:bin(PrivKeyPem))
            } of
                {ok, ok} -> {ok, {CertFile, KeyFile}};
                {Error, ok} -> Error;
                {ok, Error} -> Error;
                {Error1, _Error2} -> Error1  % Return first error if both fail
            end;
        {error, Reason} ->
            {error, {failed_to_create_cert_directory, Reason}}
    end.

%% @doc Get the server ID for HTTP redirect setup.
%%
%% This function determines which HTTP server should be configured to 
%% redirect
%% traffic to HTTPS. It first checks for an explicit http_server setting,
%% then falls back to using the current server's wallet address.
%%
%% @param Opts Server configuration options
%% @returns ServerID binary for the HTTP server to configure
get_redirect_server_id(Opts) ->
    case hb_opts:get(http_server, no_server, Opts) of
        no_server ->
            % Fallback to current server wallet
            hb_util:human_id(
                ar_wallet:to_address(
                    hb_opts:get(priv_wallet, hb:wallet(), Opts)
                )
            );
        ServerId ->
            ServerId
    end.

%% @doc Build success response with certificate and HTTPS server info.
%%
%% This function constructs the final success response containing the 
%% issued
%% certificate, private key, validation results, and HTTPS server status.
%% The response format is standardized for API consumers.
%%
%% @param DomainsOut List of domains the certificate covers
%% @param Results Validation results from challenge verification
%% @param HttpsResult HTTPS server startup result
%% @returns {ok, #{status => 200, body => ResponseMap}}
build_success_response(DomainsOut, Results, HttpsResult) ->
    ResponseBody = #{
        <<"message">> => <<"Certificate issued successfully">>,
        <<"domains">> => DomainsOut,
        <<"results">> => Results,
        <<"https_server">> => format_https_server_status(HttpsResult)
    },
    ssl_utils:build_success_response(200, ResponseBody).

%% @doc Format HTTPS server status for response.
%%
%% This function formats the HTTPS server startup result into a 
%% standardized
%% response structure with status, URL, and descriptive message. It handles
%% success, failure, and skipped cases.
%%
%% @param HttpsResult Server startup result: {started, Url} | {failed, Error} 
%%                    | {skipped, Reason}
%% @returns Map with status, server_url/error/reason, and message fields
format_https_server_status({started, ServerUrl}) ->
    #{
        <<"status">> => <<"started">>,
        <<"server_url">> => ServerUrl,
        <<"message">> => iolist_to_binary([
            <<"HTTPS server started at ">>,
            ServerUrl,
            <<", HTTP traffic will be redirected">>
        ])
    };
format_https_server_status({failed, {Error, Reason}}) ->
    #{
        <<"status">> => <<"failed">>,
        <<"error">> => ssl_utils:bin(hb_format:term({Error, Reason})),
        <<"message">> => 
            <<"Certificate issued but HTTPS server failed to start">>
    };
format_https_server_status({skipped, Reason}) ->
    #{
        <<"status">> => <<"skipped">>,
        <<"reason">> => ssl_utils:bin(Reason),
        <<"message">> => 
            <<"Certificate issued, HTTPS server not started ",
              "(auto_https disabled)">>
    }.

%% @doc Build response for pending certificate orders.
%%
%% This function creates a response for certificate orders that are not yet
%% valid, indicating that DNS challenge validation is still in progress or
%% incomplete.
%%
%% @param OrderStatus Current ACME order status (e.g., pending, 
%%                   processing)
%% @param Results Validation results from challenge attempts
%% @param RequestState1 Updated request state for potential retry
%% @returns {ok, #{status => 200, body => ResponseMap}}
build_pending_response(OrderStatus, Results, RequestState1) ->
    ResponseBody = #{
        <<"message">> => <<"Validation not complete">>,
        <<"order_status">> => OrderStatus,
        <<"results">> => Results,
        <<"request_state">> => RequestState1
    },
    ssl_utils:build_success_response(200, ResponseBody).

%% @doc Build response when certificate is still processing.
%%
%% This function creates a response for orders that have been finalized 
%% but
%% where the certificate is not yet ready for download from Let's 
%% Encrypt.
%% This typically happens when there's a delay in certificate issuance.
%%
%% @param Results Validation results from challenge verification
%% @returns {ok, #{status => 200, body => ResponseMap}}
build_processing_response(Results) ->
    ResponseBody = #{
        <<"message">> => 
            <<"Order finalized; certificate not ready for download yet">>,
        <<"order_status">> => ?ACME_STATUS_PROCESSING,
        <<"results">> => Results
    },
    ssl_utils:build_success_response(200, ResponseBody).

%% @doc Extract and validate SSL parameters from options.
%%
%% This function loads server options, extracts SSL configuration, and
%% validates all required parameters using the ssl_cert_validation 
%% module.
%% It leverages the library's comprehensive validation functions.
%%
%% @param Opts Server configuration options
%% @returns {ok, ValidatedParams} or {error, Reason}
extract_and_validate_ssl_params(Opts) ->
    maybe
        LoadedOpts = hb_cache:ensure_all_loaded(Opts, Opts),
        StrippedOpts = 
            maps:without(
                [<<"ssl_cert_rsa_key">>, <<"ssl_cert_opts">>], 
                LoadedOpts
            ),
        ?event({ssl_cert_request_started_with_opts, StrippedOpts}),
        % Extract SSL options from configuration
        {ok, SslOpts} ?= extract_ssl_opts(StrippedOpts),
        % Extract parameters
        Domains = maps:get(<<"domains">>, SslOpts, not_found),
        Email = maps:get(<<"email">>, SslOpts, not_found),
        Environment = maps:get(<<"environment">>, SslOpts, staging),
        ?event({
            ssl_cert_request_params_from_config,
            {domains, Domains},
            {email, Email},
            {environment, Environment}
        }),
        % Use library validation function - this does all the heavy lifting!
        {ok, ValidatedParams} ?= 
            ssl_cert_validation:validate_request_params(
                Domains, 
                Email, 
                Environment
            ),
        % Enhance with system defaults (library already includes key_size)
        EnhancedParams = ValidatedParams#{
            storage_path => ?SSL_CERT_STORAGE_PATH
        },
        {ok, EnhancedParams}
    end.

%% @doc Process the complete certificate request workflow.
%%
%% This function handles the ACME certificate request processing and
%% state persistence using the ssl_cert_ops module. It orchestrates
%% the request submission and state management.
%%
%% @param ValidatedParams Validated certificate request parameters
%% @param Opts Server configuration options
%% @returns {ok, {RequestState, ChallengeData}} or {error, Reason}
process_certificate_request_workflow(ValidatedParams, Opts) ->
    maybe
        % Process the certificate request using library function
        Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
        {ok, ProcResp} ?= 
            ssl_cert_ops:process_certificate_request(ValidatedParams, Wallet),
        {ok, {RequestState, ChallengeData}} ?= 
            persist_request_state(ProcResp, Opts),
        {ok, {RequestState, ChallengeData}}
    end.

%% @doc Build the certificate request response.
%%
%% This function constructs the response for a successful certificate 
%% request
%% using the ssl_utils response building functions. It includes DNS challenges
%% and instructions for the next step.
%%
%% @param RequestState Certificate request state data (unused but kept 
%%                     for consistency)
%% @param FormattedChallenges Formatted DNS challenges for the response
%% @returns {ok, #{status => 200, body => ResponseMap}}
build_request_response(_RequestState, FormattedChallenges) ->
    ResponseBody = #{
        <<"message">> => 
            <<
                "Create DNS TXT records for the following",
                " challenges, then call finalize"
            >>,
        <<"challenges">> => FormattedChallenges,
        <<"next_step">> => <<"finalize">>
    },
    ssl_utils:build_success_response(200, ResponseBody).

%% @doc Persist certificate request state in server options.
%%
%% This function extracts the request state and certificate key from 
%% the
%% processing response and persists them in the server options for later
%% retrieval during finalization. It uses ssl_cert_challenge library
%% functions for formatting challenges.
%%
%% @param ProcResp Processing response from certificate request
%% @param Opts Server configuration options
%% @returns {ok, {RequestState, ChallengeData}} or {error, Reason}
persist_request_state(ProcResp, Opts) ->
    maybe
        NewOpts = hb_http_server:get_opts(Opts),
        ProcBody = maps:get(<<"body">>, ProcResp, #{}),
        RequestState0 = maps:get(<<"request_state">>, ProcBody, #{}),
        CertificateKey = maps:get(<<"certificate_key">>, ProcBody, not_found),
        ?event({ssl_cert_orchestration_created_request}),
        % Persist request state in node opts (overwrites previous)
        ok = hb_http_server:set_opts(
            NewOpts#{ 
                <<"priv_ssl_cert_request">> => RequestState0, 
                <<"priv_ssl_cert_rsa_key">> => CertificateKey 
            }
        ),
        % Format challenges using library function
        Challenges = maps:get(<<"challenges">>, RequestState0, []),
        FormattedChallenges = 
            ssl_cert_challenge:format_challenges_for_response(Challenges),
        {ok, {RequestState0, FormattedChallenges}}
    end.

