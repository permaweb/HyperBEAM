%%% @doc SSL Certificate validation module.
%%%
%%% This module provides comprehensive validation functions for SSL certificate
%%% request parameters including domain names, email addresses, and ACME
%%% environment settings. It ensures all inputs meet the requirements for
%%% Let's Encrypt certificate issuance.
%%%
%%% The module includes detailed error reporting to help users correct
%%% invalid parameters quickly.
-module(hb_ssl_cert_validation).

-include("include/ssl_cert_records.hrl").

%% Public API
-export([
    validate_request_params/3,
    validate_domains/1,
    validate_email/1,
    validate_environment/1,
    is_valid_domain/1,
    is_valid_email/1
]).

%% Type specifications
-spec validate_request_params(term(), term(), term()) -> 
    {ok, map()} | {error, binary()}.
-spec validate_domains(term()) -> 
    {ok, domain_list()} | {error, binary()}.
-spec validate_email(term()) -> 
    {ok, email_address()} | {error, binary()}.
-spec validate_environment(term()) -> 
    {ok, acme_environment()} | {error, binary()}.
-spec is_valid_domain(string()) -> boolean().
-spec is_valid_email(string()) -> boolean().

%% @doc Validates certificate request parameters.
%%
%% This function performs comprehensive validation of all required parameters
%% for a certificate request including domains, email, and environment.
%% It returns a validated parameter map or detailed error information.
%%
%% @param Domains List of domain names or not_found
%% @param Email Contact email address or not_found  
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
                                    key_size => ?SSL_CERT_KEY_SIZE
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
%% This function validates that:
%% - Domains parameter is provided and is a list
%% - All domains are valid according to DNS naming rules
%% - At least one domain is provided
%% - All domains pass individual validation checks
%%
%% @param Domains List of domain names or not_found
%% @returns {ok, [ValidDomain]} or {error, Reason}
validate_domains(not_found) ->
    {error, <<"Missing domains parameter">>};
validate_domains(Domains) when is_list(Domains) ->
    case Domains of
        [] ->
            {error, <<"At least one domain must be provided">>};
        _ ->
            DomainStrings = [hb_util:list(D) || D <- Domains],
            % Check for duplicates
            UniqueDomains = lists:usort(DomainStrings),
            case length(UniqueDomains) =:= length(DomainStrings) of
                false ->
                    {error, <<"Duplicate domains are not allowed">>};
                true ->
                    % Validate each domain
                    ValidationResults = [
                        case is_valid_domain(D) of
                            true -> {ok, D};
                            false -> {error, D}
                        end || D <- DomainStrings
                    ],
                    InvalidDomains = [D || {error, D} <- ValidationResults],
                    case InvalidDomains of
                        [] ->
                            {ok, DomainStrings};
                        _ ->
                            InvalidList = string:join(InvalidDomains, ", "),
                            {error, hb_util:bin(io_lib:format("Invalid domains: ~s", [InvalidList]))}
                    end
            end
    end;
validate_domains(_) ->
    {error, <<"Domains must be a list">>}.

%% @doc Validates an email address.
%%
%% This function validates that:
%% - Email parameter is provided
%% - Email format follows basic RFC standards
%% - Email doesn't contain invalid patterns
%%
%% @param Email Email address or not_found
%% @returns {ok, ValidEmail} or {error, Reason}
validate_email(not_found) ->
    {error, <<"Missing email parameter">>};
validate_email(Email) ->
    EmailStr = hb_util:list(Email),
    case EmailStr of
        "" ->
            {error, <<"Email address cannot be empty">>};
        _ ->
            case is_valid_email(EmailStr) of
                true ->
                    {ok, EmailStr};
                false ->
                    {error, <<"Invalid email address format">>}
            end
    end.

%% @doc Validates the ACME environment.
%%
%% This function validates that the environment is either 'staging' or 'production'.
%% It accepts both atom and binary formats and normalizes to atom format.
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

%% @doc Checks if a domain name is valid according to DNS standards.
%%
%% This function validates domain names according to RFC 1123 and RFC 952:
%% - Labels can contain letters, numbers, and hyphens
%% - Labels cannot start or end with hyphens
%% - Labels cannot exceed 63 characters
%% - Total domain length cannot exceed 253 characters
%% - Domain must have at least one dot (except for localhost-style names)
%%
%% @param Domain Domain name string
%% @returns true if valid, false otherwise
is_valid_domain(Domain) when is_list(Domain) ->
    case Domain of
        "" -> false;
        _ ->
            % Check total length
            case length(Domain) =< 253 of
                false -> false;
                true ->
                    % Basic domain validation regex
                    DomainRegex = "^[a-zA-Z0-9]([a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9])?" ++
                                  "(\\.[a-zA-Z0-9]([a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9])?)*$",
                    case re:run(Domain, DomainRegex) of
                        {match, _} -> 
                            % Additional checks for edge cases
                            validate_domain_labels(Domain);
                        nomatch -> 
                            false
                    end
            end
    end;
is_valid_domain(_) ->
    false.

%% @doc Checks if an email address is valid according to basic RFC standards.
%%
%% This function performs basic email validation:
%% - Must contain exactly one @ symbol
%% - Local part (before @) must be valid
%% - Domain part (after @) must be valid
%% - No consecutive dots
%% - No dots adjacent to @ symbol
%%
%% @param Email Email address string
%% @returns true if valid, false otherwise
is_valid_email(Email) when is_list(Email) ->
    case Email of
        "" -> false;
        _ ->
            % Basic email validation regex
            EmailRegex = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9][a-zA-Z0-9.-]*\\.[a-zA-Z]{2,}$",
            case re:run(Email, EmailRegex) of
                {match, _} -> 
                    % Additional checks for invalid patterns
                    HasDoubleDots = string:find(Email, "..") =/= nomatch,
                    HasAtDot = string:find(Email, "@.") =/= nomatch,
                    HasDotAt = string:find(Email, ".@") =/= nomatch,
                    EndsWithDot = lists:suffix(".", Email),
                    StartsWithDot = lists:prefix(".", Email),
                    % Check @ symbol count
                    AtCount = length([C || C <- Email, C =:= $@]),
                    % Email is valid if none of the invalid patterns are present
                    AtCount =:= 1 andalso
                    not (HasDoubleDots orelse HasAtDot orelse HasDotAt orelse 
                         EndsWithDot orelse StartsWithDot);
                nomatch -> 
                    false
            end
    end;
is_valid_email(_) ->
    false.

%%%--------------------------------------------------------------------
%%% Internal Functions
%%%--------------------------------------------------------------------

%% @doc Validates individual domain labels for additional edge cases.
%%
%% @param Domain The domain to validate
%% @returns true if all labels are valid, false otherwise
validate_domain_labels(Domain) ->
    Labels = string:split(Domain, ".", all),
    lists:all(fun validate_single_label/1, Labels).

%% @doc Validates a single domain label.
%%
%% @param Label The domain label to validate
%% @returns true if valid, false otherwise
validate_single_label(Label) ->
    case Label of
        "" -> false;  % Empty labels not allowed
        _ ->
            Length = length(Label),
            % Check length (1-63 characters)
            Length >= 1 andalso Length =< 63 andalso
            % Cannot start or end with hyphen
            not lists:prefix("-", Label) andalso
            not lists:suffix("-", Label) andalso
            % Must contain only valid characters
            lists:all(fun(C) ->
                (C >= $a andalso C =< $z) orelse
                (C >= $A andalso C =< $Z) orelse  
                (C >= $0 andalso C =< $9) orelse
                C =:= $-
            end, Label)
    end.
