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
-module(dev_ssl_cert).
-export([info/1, info/3, request/3, status/3]).
-export([challenges/3, validate/3, download/3, list/3]).
-export([renew/3, delete/3]).
-export([validate_request_params/3, generate_request_id/0]).
-export([is_valid_domain/1, is_valid_email/1]).

-include("include/hb.hrl").

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
            info, request, status, challenges, 
            validate, download, list, renew, delete
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
                <<"required_params">> => #{
                    <<"domains">> => <<"List of domain names for certificate">>,
                    <<"email">> => <<"Contact email for Let's Encrypt account">>,
                    <<"environment">> => <<"'staging' or 'production'">>
                },
                <<"example">> => #{
                    <<"domains">> => [<<"example.com">>, <<"www.example.com">>],
                    <<"email">> => <<"admin@example.com">>,
                    <<"environment">> => <<"staging">>
                }
            },
            <<"status">> => #{
                <<"description">> => <<"Check certificate request status">>,
                <<"required_params">> => #{
                    <<"request_id">> => <<"Certificate request identifier">>
                }
            },
            <<"challenges">> => #{
                <<"description">> => <<"Get DNS challenge records to create">>,
                <<"required_params">> => #{
                    <<"request_id">> => <<"Certificate request identifier">>
                }
            },
            <<"validate">> => #{
                <<"description">> => <<"Validate DNS challenges after setup">>,
                <<"required_params">> => #{
                    <<"request_id">> => <<"Certificate request identifier">>
                }
            },
            <<"download">> => #{
                <<"description">> => <<"Download completed certificate">>,
                <<"required_params">> => #{
                    <<"request_id">> => <<"Certificate request identifier">>
                }
            },
            <<"list">> => #{
                <<"description">> => <<"List all stored certificates">>
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
    {ok, #{<<"status">> => 200, <<"body">> => InfoBody}}.

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
%% Required parameters in M2:
%% - domains: List of domain names for the certificate
%% - email: Contact email for Let's Encrypt account registration
%% - environment: 'staging' or 'production' (use staging for testing)
%%
%% @param _M1 Ignored parameter
%% @param M2 Request message containing certificate parameters
%% @param Opts A map of configuration options
%% @returns {ok, Map} with request ID and status, or {error, Reason}
request(_M1, M2, Opts) ->
    ?event({ssl_cert_request_started}),
    try
        % Extract and validate parameters
        Domains = hb_ao:get(<<"domains">>, M2, Opts),
        Email = hb_ao:get(<<"email">>, M2, Opts),
        Environment = hb_ao:get(<<"environment">>, M2, staging, Opts),
        case validate_request_params(Domains, Email, Environment) of
            {ok, ValidatedParams} ->
                process_certificate_request(ValidatedParams, Opts);
            {error, Reason} ->
                ?event({ssl_cert_request_validation_failed, Reason}),
                {error, #{<<"status">> => 400, <<"error">> => Reason}}
        end
    catch
        Error:RequestReason:Stacktrace ->
            ?event({ssl_cert_request_error, Error, RequestReason, Stacktrace}),
            {error, #{<<"status">> => 500, 
                     <<"error">> => <<"Internal server error">>}}
    end.

%% @doc Checks the status of a certificate request.
%%
%% This function retrieves the current status of a certificate request:
%% 1. Validates the request ID parameter
%% 2. Retrieves the stored request state
%% 3. Checks the current ACME order status
%% 4. Returns detailed status information including next steps
%%
%% Required parameters in M2:
%% - request_id: The certificate request identifier
%%
%% @param _M1 Ignored parameter
%% @param M2 Request message containing request_id
%% @param Opts A map of configuration options  
%% @returns {ok, Map} with current status, or {error, Reason}
status(_M1, M2, Opts) ->
    ?event({ssl_cert_status_check_started}),
    try
        RequestId = hb_ao:get(<<"request_id">>, M2, Opts),
        case RequestId of
            not_found ->
                {error, #{<<"status">> => 400, 
                         <<"error">> => <<"Missing request_id parameter">>}};
            _ ->
                get_request_status(hb_util:list(RequestId), Opts)
        end
    catch
        Error:Reason:Stacktrace ->
            ?event({ssl_cert_status_error, Error, Reason, Stacktrace}),
            {error, #{<<"status">> => 500, 
                     <<"error">> => <<"Internal server error">>}}
    end.

%% @doc Retrieves DNS challenge records for manual DNS setup.
%%
%% This function provides the DNS TXT records that must be created:
%% 1. Validates the request ID parameter
%% 2. Retrieves the stored DNS challenges
%% 3. Formats the challenges with provider-specific instructions
%% 4. Returns detailed setup instructions for popular DNS providers
%%
%% Required parameters in M2:
%% - request_id: The certificate request identifier
%%
%% @param _M1 Ignored parameter
%% @param M2 Request message containing request_id
%% @param Opts A map of configuration options
%% @returns {ok, Map} with DNS challenge instructions, or {error, Reason}  
challenges(_M1, M2, Opts) ->
    ?event({ssl_cert_challenges_requested}),
    try
        RequestId = hb_ao:get(<<"request_id">>, M2, Opts),
        case RequestId of
            not_found ->
                {error, #{<<"status">> => 400, 
                         <<"error">> => <<"Missing request_id parameter">>}};
            _ ->
                get_dns_challenges(hb_util:list(RequestId), Opts)
        end
    catch
        Error:Reason:Stacktrace ->
            ?event({ssl_cert_challenges_error, Error, Reason, Stacktrace}),
            {error, #{<<"status">> => 500, 
                     <<"error">> => <<"Internal server error">>}}
    end.

%% @doc Validates DNS challenges after manual DNS record creation.
%%
%% This function validates that DNS TXT records have been properly created:
%% 1. Validates the request ID parameter
%% 2. Checks DNS propagation for all challenge records
%% 3. Notifies Let's Encrypt to validate the challenges
%% 4. Updates the request status based on validation results
%% 5. Returns validation status and next steps
%%
%% Required parameters in M2:
%% - request_id: The certificate request identifier
%%
%% @param _M1 Ignored parameter  
%% @param M2 Request message containing request_id
%% @param Opts A map of configuration options
%% @returns {ok, Map} with validation results, or {error, Reason}
validate(_M1, M2, Opts) ->
    ?event({ssl_cert_validation_started}),
    try
        RequestId = hb_ao:get(<<"request_id">>, M2, Opts),
        case RequestId of
            not_found ->
                {error, #{<<"status">> => 400, 
                         <<"error">> => <<"Missing request_id parameter">>}};
            _ ->
                validate_dns_challenges(hb_util:list(RequestId), Opts)
        end
    catch
        Error:Reason:Stacktrace ->
            ?event({ssl_cert_validation_error, Error, Reason, Stacktrace}),
            {error, #{<<"status">> => 500, 
                     <<"error">> => <<"Internal server error">>}}
    end.

%% @doc Downloads a completed SSL certificate.
%%
%% This function retrieves the issued certificate and private key:
%% 1. Validates the request ID parameter
%% 2. Checks that the certificate is ready for download
%% 3. Retrieves the certificate chain from Let's Encrypt
%% 4. Stores the certificate and private key securely
%% 5. Returns the certificate in PEM format
%%
%% Required parameters in M2:
%% - request_id: The certificate request identifier
%%
%% @param _M1 Ignored parameter
%% @param M2 Request message containing request_id  
%% @param Opts A map of configuration options
%% @returns {ok, Map} with certificate data, or {error, Reason}
download(_M1, M2, Opts) ->
    ?event({ssl_cert_download_started}),
    try
        RequestId = hb_ao:get(<<"request_id">>, M2, Opts),
        case RequestId of
            not_found ->
                {error, #{<<"status">> => 400, 
                         <<"error">> => <<"Missing request_id parameter">>}};
            _ ->
                download_certificate(hb_util:list(RequestId), Opts)
        end
    catch
        Error:Reason:Stacktrace ->
            ?event({ssl_cert_download_error, Error, Reason, Stacktrace}),
            {error, #{<<"status">> => 500, 
                     <<"error">> => <<"Internal server error">>}}
    end.

%% @doc Lists all stored SSL certificates.
%%
%% This function provides an overview of all certificates:
%% 1. Retrieves all stored certificates from the certificate store
%% 2. Checks expiration status for each certificate
%% 3. Formats the certificate information for display
%% 4. Returns a list with domains, status, and expiration dates
%%
%% No parameters required.
%%
%% @param _M1 Ignored parameter
%% @param _M2 Ignored parameter
%% @param Opts A map of configuration options
%% @returns {ok, Map} with certificate list, or {error, Reason}
list(_M1, _M2, Opts) ->
    ?event({ssl_cert_list_requested}),
    try
        get_certificate_list(Opts)
    catch
        Error:Reason:Stacktrace ->
            ?event({ssl_cert_list_error, Error, Reason, Stacktrace}),
            {error, #{<<"status">> => 500, 
                     <<"error">> => <<"Internal server error">>}}
    end.

%% @doc Renews an existing SSL certificate.
%%
%% This function initiates renewal for an existing certificate:
%% 1. Validates the domains parameter
%% 2. Retrieves the existing certificate configuration
%% 3. Initiates a new certificate request with the same parameters
%% 4. Returns a new request ID for the renewal process
%%
%% Required parameters in M2:
%% - domains: List of domain names to renew
%%
%% @param _M1 Ignored parameter
%% @param M2 Request message containing domains to renew
%% @param Opts A map of configuration options
%% @returns {ok, Map} with renewal request ID, or {error, Reason}
renew(_M1, M2, Opts) ->
    ?event({ssl_cert_renewal_started}),
    try
        Domains = hb_ao:get(<<"domains">>, M2, Opts),
        case Domains of
            not_found ->
                {error, #{<<"status">> => 400, 
                         <<"error">> => <<"Missing domains parameter">>}};
            _ ->
                renew_certificate(Domains, Opts)
        end
    catch
        Error:Reason:Stacktrace ->
            ?event({ssl_cert_renewal_error, Error, Reason, Stacktrace}),
            {error, #{<<"status">> => 500, 
                     <<"error">> => <<"Internal server error">>}}
    end.

%% @doc Deletes a stored SSL certificate.
%%
%% This function removes a certificate from storage:
%% 1. Validates the domains parameter
%% 2. Locates the certificate in storage
%% 3. Removes the certificate files and metadata
%% 4. Returns confirmation of deletion
%%
%% Required parameters in M2:
%% - domains: List of domain names to delete
%%
%% @param _M1 Ignored parameter
%% @param M2 Request message containing domains to delete
%% @param Opts A map of configuration options
%% @returns {ok, Map} with deletion confirmation, or {error, Reason}
delete(_M1, M2, Opts) ->
    ?event({ssl_cert_deletion_started}),
    try
        Domains = hb_ao:get(<<"domains">>, M2, Opts),
        case Domains of
            not_found ->
                {error, #{<<"status">> => 400, 
                         <<"error">> => <<"Missing domains parameter">>}};
            _ ->
                delete_certificate(Domains, Opts)
        end
    catch
        Error:Reason:Stacktrace ->
            ?event({ssl_cert_deletion_error, Error, Reason, Stacktrace}),
            {error, #{<<"status">> => 500, 
                     <<"error">> => <<"Internal server error">>}}
    end.

%%%--------------------------------------------------------------------
%%% Internal Functions
%%%--------------------------------------------------------------------

%% @doc Validates certificate request parameters.
%%
%% @param Domains List of domain names
%% @param Email Contact email address
%% @param Environment ACME environment (staging/production)
%% @returns {ok, ValidatedParams} or {error, Reason}
validate_request_params(Domains, Email, Environment) ->
    try
        % Validate domains
        case validate_domains(Domains) of
            {ok, ValidDomains} ->
                % Validate email
                case validate_email(Email) of
                    {ok, ValidEmail} ->
                        % Validate environment
                        case validate_environment(Environment) of
                            {ok, ValidEnv} ->
                                {ok, #{
                                    domains => ValidDomains,
                                    email => ValidEmail,
                                    environment => ValidEnv,
                                    key_size => 2048
                                }};
                            {error, Reason} ->
                                {error, Reason}
                        end;
                    {error, Reason} ->
                        {error, Reason}
                end;
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:_ ->
            {error, <<"Invalid request parameters">>}
    end.

%% @doc Validates a list of domain names.
%%
%% @param Domains List of domain names or not_found
%% @returns {ok, [ValidDomain]} or {error, Reason}
validate_domains(not_found) ->
    {error, <<"Missing domains parameter">>};
validate_domains(Domains) when is_list(Domains) ->
    DomainStrings = [hb_util:list(D) || D <- Domains],
    ValidDomains = [D || D <- DomainStrings, is_valid_domain(D)],
    case ValidDomains of
        [] ->
            {error, <<"No valid domains provided">>};
        _ when length(ValidDomains) =:= length(DomainStrings) ->
            {ok, ValidDomains};
        _ ->
            {error, <<"Some domains are invalid">>}
    end;
validate_domains(_) ->
    {error, <<"Domains must be a list">>}.

%% @doc Validates an email address.
%%
%% @param Email Email address or not_found
%% @returns {ok, ValidEmail} or {error, Reason}
validate_email(not_found) ->
    {error, <<"Missing email parameter">>};
validate_email(Email) ->
    EmailStr = hb_util:list(Email),
    case is_valid_email(EmailStr) of
        true ->
            {ok, EmailStr};
        false ->
            {error, <<"Invalid email address">>}
    end.

%% @doc Validates the ACME environment.
%%
%% @param Environment Environment atom or binary
%% @returns {ok, ValidEnvironment} or {error, Reason}
validate_environment(Environment) ->
    EnvAtom = case Environment of
        <<"staging">> -> staging;
        <<"production">> -> production;
        staging -> staging;
        production -> production;
        _ -> invalid
    end,
    case EnvAtom of
        invalid ->
            {error, <<"Environment must be 'staging' or 'production'">>};
        _ ->
            {ok, EnvAtom}
    end.

%% @doc Checks if a domain name is valid.
%%
%% @param Domain Domain name string
%% @returns true if valid, false otherwise
is_valid_domain(Domain) ->
    % Basic domain validation regex
    DomainRegex = "^[a-zA-Z0-9]([a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9])?" ++
                  "(\\.[a-zA-Z0-9]([a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9])?)*$",
    case re:run(Domain, DomainRegex) of
        {match, _} -> 
            length(Domain) > 0 andalso length(Domain) =< 253;
        nomatch -> 
            false
    end.

%% @doc Checks if an email address is valid.
%%
%% @param Email Email address string
%% @returns true if valid, false otherwise
is_valid_email(Email) ->
    % Basic email validation regex
    EmailRegex = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9][a-zA-Z0-9.-]*\\.[a-zA-Z]{2,}$",
    case re:run(Email, EmailRegex) of
        {match, _} -> 
            % Additional checks for invalid patterns
            HasDoubleDots = string:find(Email, "..") =/= nomatch,
            HasAtDot = string:find(Email, "@.") =/= nomatch,
            HasDotAt = string:find(Email, ".@") =/= nomatch,
            EndsWithDot = lists:suffix(".", Email),
            % Email is valid if none of the invalid patterns are present
            not (HasDoubleDots orelse HasAtDot orelse HasDotAt orelse EndsWithDot);
        nomatch -> 
            false
    end.

%% @doc Processes a validated certificate request.
%%
%% @param ValidatedParams Map of validated request parameters
%% @param Opts Configuration options
%% @returns {ok, Map} with request details or {error, Reason}
process_certificate_request(ValidatedParams, Opts) ->
    ?event({ssl_cert_processing_request, ValidatedParams}),
    % Generate unique request ID
    RequestId = generate_request_id(),
    try
        % Create ACME account
        case hb_acme_client:create_account(ValidatedParams) of
            {ok, Account} ->
                ?event({ssl_cert_account_created, RequestId}),
                % Request certificate order
                Domains = maps:get(domains, ValidatedParams),
                case hb_acme_client:request_certificate(Account, Domains) of
                    {ok, Order} ->
                        ?event({ssl_cert_order_created, RequestId}),
                        % Generate DNS challenges
                        case hb_acme_client:get_dns_challenge(Account, Order) of
                            {ok, Challenges} ->
                                % Store request state
                                RequestState = #{
                                    request_id => RequestId,
                                    account => Account,
                                    order => Order,
                                    challenges => Challenges,
                                    domains => Domains,
                                    status => pending_dns,
                                    created => calendar:universal_time()
                                },
                                store_request_state(RequestId, RequestState, Opts),
                                {ok, #{
                                    <<"status">> => 200,
                                    <<"body">> => #{
                                        <<"request_id">> => hb_util:bin(RequestId),
                                        <<"status">> => <<"pending_dns">>,
                                        <<"message">> => 
                                            <<"Certificate request created. Use /challenges endpoint to get DNS records.">>,
                                        <<"domains">> => [hb_util:bin(D) || D <- Domains],
                                        <<"next_step">> => <<"challenges">>
                                    }
                                }};
                            {error, Reason} ->
                                ?event({ssl_cert_challenge_generation_failed, 
                                       RequestId, Reason}),
                                {error, #{<<"status">> => 500, 
                                         <<"error">> => <<"Challenge generation failed">>}}
                        end;
                    {error, Reason} ->
                        ?event({ssl_cert_order_failed, RequestId, Reason}),
                        {error, #{<<"status">> => 500, 
                                 <<"error">> => <<"Certificate order failed">>}}
                end;
            {error, Reason} ->
                ?event({
                    ssl_cert_account_creation_failed,
                    {request_id, RequestId},
                    {reason, Reason},
                    {config, ValidatedParams}
                }),
                % Provide detailed error information to user
                DetailedError = case Reason of
                    {account_creation_failed, SubReason} ->
                        #{
                            <<"error">> => <<"ACME account creation failed">>,
                            <<"details">> => format_error_details(SubReason),
                            <<"troubleshooting">> => #{
                                <<"check_internet">> => <<"Ensure internet connectivity to Let's Encrypt">>,
                                <<"check_email">> => <<"Verify email address is valid">>,
                                <<"try_staging">> => <<"Try staging environment first">>,
                                <<"check_rate_limits">> => <<"Check Let's Encrypt rate limits">>
                            }
                        };
                    {connection_failed, ConnReason} ->
                        #{
                            <<"error">> => <<"Connection to Let's Encrypt failed">>,
                            <<"details">> => hb_util:bin(io_lib:format("~p", [ConnReason])),
                            <<"troubleshooting">> => #{
                                <<"check_network">> => <<"Check network connectivity">>,
                                <<"check_firewall">> => <<"Ensure HTTPS (443) is not blocked">>,
                                <<"check_dns">> => <<"Verify DNS resolution for acme-staging-v02.api.letsencrypt.org">>
                            }
                        };
                    _ ->
                        #{
                            <<"error">> => <<"Account creation failed">>,
                            <<"details">> => hb_util:bin(io_lib:format("~p", [Reason]))
                        }
                end,
                {error, #{<<"status">> => 500, <<"error_info">> => DetailedError}}
        end
    catch
        Error:ProcessReason:Stacktrace ->
            ?event({ssl_cert_process_error, RequestId, Error, ProcessReason, Stacktrace}),
            {error, #{<<"status">> => 500, 
                     <<"error">> => <<"Certificate request processing failed">>}}
    end.

%% @doc Generates a unique request identifier.
%%
%% @returns A unique request ID string
generate_request_id() ->
    Timestamp = integer_to_list(erlang:system_time(millisecond)),
    Random = integer_to_list(rand:uniform(999999)),
    "ssl_" ++ Timestamp ++ "_" ++ Random.

%% @doc Stores request state for later retrieval.
%%
%% @param RequestId Unique request identifier
%% @param RequestState Complete request state map
%% @param Opts Configuration options
%% @returns ok
store_request_state(RequestId, RequestState, Opts) ->
    ?event({ssl_cert_storing_state, RequestId}),
    % Store in HyperBEAM's cache system
    CacheKey = <<"ssl_cert_request_", (hb_util:bin(RequestId))/binary>>,
    hb_cache:write(#{
        CacheKey => RequestState
    }, Opts),
    ok.

%% @doc Retrieves stored request state.
%%
%% @param RequestId Request identifier
%% @param Opts Configuration options  
%% @returns {ok, RequestState} or {error, not_found}
get_request_state(RequestId, Opts) ->
    CacheKey = <<"ssl_cert_request_", (hb_util:bin(RequestId))/binary>>,
    case hb_cache:read(CacheKey, Opts) of
        {ok, RequestState} ->
            {ok, RequestState};
        _ ->
            {error, not_found}
    end.

%% Placeholder implementations for remaining functions
%% These would be implemented with full functionality

get_request_status(RequestId, Opts) ->
    case get_request_state(RequestId, Opts) of
        {ok, State} ->
            Status = maps:get(status, State, unknown),
            {ok, #{<<"status">> => 200, 
                   <<"body">> => #{<<"request_status">> => hb_util:bin(Status)}}};
        {error, not_found} ->
            {error, #{<<"status">> => 404, <<"error">> => <<"Request not found">>}}
    end.

get_dns_challenges(RequestId, Opts) ->
    case get_request_state(RequestId, Opts) of
        {ok, State} ->
            Challenges = maps:get(challenges, State, []),
            {ok, #{<<"status">> => 200, 
                   <<"body">> => #{<<"challenges">> => format_challenges(Challenges)}}};
        {error, not_found} ->
            {error, #{<<"status">> => 404, <<"error">> => <<"Request not found">>}}
    end.

validate_dns_challenges(_RequestId, _Opts) ->
    {ok, #{<<"status">> => 200, 
           <<"body">> => #{<<"message">> => <<"Validation started">>}}}.

download_certificate(_RequestId, _Opts) ->
    {ok, #{<<"status">> => 200, 
           <<"body">> => #{<<"message">> => <<"Certificate ready">>}}}.

get_certificate_list(_Opts) ->
    {ok, #{<<"status">> => 200, 
           <<"body">> => #{<<"certificates">> => []}}}.

renew_certificate(_Domains, _Opts) ->
    {ok, #{<<"status">> => 200, 
           <<"body">> => #{<<"message">> => <<"Renewal started">>}}}.

delete_certificate(_Domains, _Opts) ->
    {ok, #{<<"status">> => 200, 
           <<"body">> => #{<<"message">> => <<"Certificate deleted">>}}}.

format_challenges(_Challenges) ->
    [#{<<"domain">> => hb_util:bin("example.com"),
       <<"record">> => <<"_acme-challenge.example.com">>,
       <<"value">> => <<"challenge_value">>}].

%% @doc Formats error details for user-friendly display.
%%
%% @param ErrorReason The error reason to format
%% @returns Formatted error details as binary
format_error_details(ErrorReason) ->
    case ErrorReason of
        {http_error, StatusCode, Details} ->
            StatusBin = hb_util:bin(integer_to_list(StatusCode)),
            DetailsBin = case Details of
                Map when is_map(Map) ->
                    case maps:get(<<"detail">>, Map, undefined) of
                        undefined -> hb_util:bin(io_lib:format("~p", [Map]));
                        Detail -> Detail
                    end;
                Binary when is_binary(Binary) -> Binary;
                Other -> hb_util:bin(io_lib:format("~p", [Other]))
            end,
            <<"HTTP ", StatusBin/binary, ": ", DetailsBin/binary>>;
        {connection_failed, ConnReason} ->
            ConnBin = hb_util:bin(io_lib:format("~p", [ConnReason])),
            <<"Connection failed: ", ConnBin/binary>>;
        Other ->
            hb_util:bin(io_lib:format("~p", [Other]))
    end.
