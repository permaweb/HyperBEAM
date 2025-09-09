%%% @doc ACME client module for Let's Encrypt certificate management.
%%%
%%% This module provides the main API for ACME (Automatic Certificate Management
%%% Environment) v2 protocol operations. It serves as a facade that orchestrates
%%% calls to specialized modules for HTTP communication, cryptographic operations,
%%% CSR generation, and protocol implementation.
%%%
%%% The module supports both staging and production Let's Encrypt environments
%%% and provides comprehensive logging through HyperBEAM's event system.
%%%
%%% This refactored version delegates complex operations to specialized modules:
%%% - hb_acme_protocol: Core ACME protocol operations
%%% - hb_acme_http: HTTP client and communication
%%% - hb_acme_crypto: Cryptographic operations and JWS
%%% - hb_acme_csr: Certificate Signing Request generation
%%% - hb_acme_url: URL parsing and manipulation utilities
-module(hb_acme_client).

%% Main ACME API
-export([
    create_account/2, 
    request_certificate/2, 
    get_dns_challenge/2,
    validate_challenge/2, 
    get_challenge_status/2, 
    finalize_order/3, 
    download_certificate/2,
    get_order/2
]).

%% Utility exports for backward compatibility
-export([
    base64url_encode/1,
    get_nonce/0, 
    get_fresh_nonce/1,
    determine_directory_from_url/1, 
    extract_host_from_url/1,
    extract_base_url/1, 
    extract_path_from_url/1,
    make_jws_post_as_get_request/3
]).

%% @doc Creates a new ACME account with Let's Encrypt.
create_account(Config, Opts) ->
    hb_acme_protocol:create_account(Config, Opts).

%% @doc Requests a certificate for the specified domains.
request_certificate(Account, Domains) ->
    hb_acme_protocol:request_certificate(Account, Domains).

%% @doc Retrieves DNS-01 challenges for all domains in an order.
get_dns_challenge(Account, Order) ->
    hb_acme_protocol:get_dns_challenge(Account, Order).

%% @doc Validates a DNS challenge with the ACME server.
validate_challenge(Account, Challenge) ->
    hb_acme_protocol:validate_challenge(Account, Challenge).

%% @doc Retrieves current challenge status using POST-as-GET.
get_challenge_status(Account, Challenge) ->
    hb_acme_protocol:get_challenge_status(Account, Challenge).

%% @doc Finalizes a certificate order after all challenges are validated.
finalize_order(Account, Order, Opts) ->
    hb_acme_protocol:finalize_order(Account, Order, Opts).

%% @doc Downloads the certificate from the ACME server.
download_certificate(Account, Order) ->
    hb_acme_protocol:download_certificate(Account, Order).

%% @doc Fetches the latest state of an order (POST-as-GET).
get_order(Account, OrderUrl) ->
    hb_acme_protocol:get_order(Account, OrderUrl).

%%%--------------------------------------------------------------------
%%% Utility Functions for Backward Compatibility
%%%--------------------------------------------------------------------

%% @doc Encodes data using base64url encoding.
base64url_encode(Data) ->
    hb_acme_crypto:base64url_encode(Data).

%% @doc Generates a random nonce for JWS requests (fallback).
get_nonce() ->
    hb_acme_http:get_nonce().

%% @doc Gets a fresh nonce from the ACME server.
get_fresh_nonce(DirectoryUrl) ->
    hb_acme_http:get_fresh_nonce(DirectoryUrl).

%% @doc Determines the ACME directory URL from any ACME endpoint URL.
determine_directory_from_url(Url) ->
    hb_acme_url:determine_directory_from_url(Url).

%% @doc Extracts the host from a URL.
extract_host_from_url(Url) ->
    hb_acme_url:extract_host_from_url(Url).

%% @doc Extracts the base URL (scheme + host) from a complete URL.
extract_base_url(Url) ->
    hb_acme_url:extract_base_url(Url).

%% @doc Extracts the path from a URL.
extract_path_from_url(Url) ->
    hb_acme_url:extract_path_from_url(Url).

%% @doc Creates and sends a JWS POST-as-GET request.
make_jws_post_as_get_request(Url, PrivateKey, Kid) ->
    hb_acme_http:make_jws_post_as_get_request(Url, PrivateKey, Kid).
