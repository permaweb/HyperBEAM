%%% @doc Shared record definitions and constants for SSL certificate management.
%%%
%%% This header file contains all the common record definitions, type specifications,
%%% and constants used by the SSL certificate management modules including the
%%% device interface, ACME client, validation, and state management modules.

%% ACME server URLs
-define(LETS_ENCRYPT_STAGING, 
    "https://acme-staging-v02.api.letsencrypt.org/directory").
-define(LETS_ENCRYPT_PROD, 
    "https://acme-v02.api.letsencrypt.org/directory").

%% Challenge validation polling configuration
-define(CHALLENGE_POLL_DELAY_SECONDS, 5).
-define(CHALLENGE_DEFAULT_TIMEOUT_SECONDS, 300).

%% Request defaults
-define(SSL_CERT_KEY_SIZE, 4096).
-define(SSL_CERT_STORAGE_PATH, "certificates").

%% Order polling after finalization
-define(ORDER_POLL_DELAY_SECONDS, 5).
-define(ORDER_POLL_TIMEOUT_SECONDS, 60).

%% ACME challenge status constants
-define(ACME_STATUS_VALID, <<"valid">>).
-define(ACME_STATUS_INVALID, <<"invalid">>).
-define(ACME_STATUS_PENDING, <<"pending">>).
-define(ACME_STATUS_PROCESSING, <<"processing">>).

%% ACME Account Record
%% Represents an ACME account with Let's Encrypt
-record(acme_account, {
    key :: public_key:private_key(),  % Private key for account
    url :: string(),                  % Account URL from ACME server
    kid :: string()                   % Key ID for account
}).

%% ACME Order Record  
%% Represents a certificate order with Let's Encrypt
-record(acme_order, {
    url :: string(),           % Order URL
    status :: string(),        % Order status (pending, valid, invalid, etc.)
    expires :: string(),       % Expiration timestamp
    identifiers :: list(),     % List of domain identifiers
    authorizations :: list(),  % List of authorization URLs
    finalize :: string(),      % Finalization URL
    certificate :: string()    % Certificate download URL (when ready)
}).

%% DNS Challenge Record
%% Represents a DNS-01 challenge for domain validation
-record(dns_challenge, {
    domain :: string(),              % Domain name being validated
    token :: string(),               % Challenge token
    key_authorization :: string(),   % Key authorization string
    dns_value :: string(),          % DNS TXT record value to set
    url :: string()                 % Challenge URL for validation
}).

%% Type definitions for better documentation and dialyzer support
-type acme_account() :: #acme_account{}.
-type acme_order() :: #acme_order{}.
-type dns_challenge() :: #dns_challenge{}.
-type acme_environment() :: staging | production.
-type domain_list() :: [string()].
-type email_address() :: string().
-type validation_result() :: #{binary() => binary()}.
-type request_state() :: #{binary() => term()}.

%% Export types for use in other modules
-export_type([
    acme_account/0,
    acme_order/0, 
    dns_challenge/0,
    acme_environment/0,
    domain_list/0,
    email_address/0,
    validation_result/0,
    request_state/0
]).
