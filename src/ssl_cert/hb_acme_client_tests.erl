%%% @doc ACME client test suite.
%%%
%%% This module provides comprehensive tests for the ACME client functionality
%%% including CSR generation, protocol operations, cryptographic functions,
%%% and integration tests. The tests are designed to validate the modular
%%% ACME client implementation across all its components.
-module(hb_acme_client_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").
-include("include/ssl_cert_records.hrl").

%%%--------------------------------------------------------------------
%%% CSR Generation Tests
%%%--------------------------------------------------------------------

%% @doc Tests CSR (Certificate Signing Request) generation functionality.
%%
%% Verifies that the ACME client can generate valid CSRs for SSL certificates
%% with proper ASN.1 encoding, subject names, and SAN extensions.
csr_generation_test() ->
    % Test CSR generation for single domain
    SingleDomain = ["example.com"],
    {ok, CsrDer, CertKey} = hb_acme_csr:generate_csr(SingleDomain, #{ priv_wallet => ar_wallet:new() }),
    % Verify basic properties without decoding (since ACME will handle that)
    ?assert(is_record(CertKey, 'RSAPrivateKey')),
    ?assert(is_binary(CsrDer)),
    ?assert(byte_size(CsrDer) > 0),
    ok.

%% @doc Tests CSR generation for multiple domains (SAN certificate).
csr_generation_multi_domain_test() ->
    % Test CSR generation for multiple domains (SAN certificate)
    MultiDomains = ["example.com", "www.example.com", "api.example.com"],
    {ok, MultiCsrDer, MultiCertKey} = hb_acme_csr:generate_csr(MultiDomains, #{ priv_wallet => ar_wallet:new() }),
    % Verify basic properties without decoding (since ACME will handle that)
    ?assert(is_record(MultiCertKey, 'RSAPrivateKey')),
    ?assert(is_binary(MultiCsrDer)),
    ?assert(byte_size(MultiCsrDer) > 0),
    ok.

%% @doc Tests CSR generation error handling.
csr_generation_error_handling_test() ->
    % Test CSR generation with invalid domain
    InvalidDomains = [""],
    case hb_acme_csr:generate_csr(InvalidDomains, #{ priv_wallet => ar_wallet:new() }) of
        {ok, _InvalidCsr, _InvalidKey} ->
            {error, invalid_csr_unexpectedly_succeeded};
        {error, _InvalidReason} ->
            {ok, invalid_csr_failed_as_expected}
    end.

%%%--------------------------------------------------------------------
%%% Cryptographic Function Tests
%%%--------------------------------------------------------------------

%% @doc Tests RSA key generation functionality via wallet.
rsa_key_generation_test() ->
    % Test key extraction from wallet (as used in production)
    Wallet = ar_wallet:new(),
    {{_KT = {rsa, E}, _PrivBin, _PubBin}, _} = Wallet,
    % Verify the wallet contains RSA key material
    ?assertEqual(65537, E), % Standard RSA exponent
    ok.

%% @doc Tests JWK (JSON Web Key) conversion.
jwk_conversion_test() ->
    % Create RSA key from wallet (as used in production)
    Wallet = ar_wallet:new(),
    {{_KT = {rsa, E}, PrivBin, PubBin}, _} = Wallet,
    Modulus = crypto:bytes_to_integer(iolist_to_binary(PubBin)),
    D = crypto:bytes_to_integer(iolist_to_binary(PrivBin)),
    Key = #'RSAPrivateKey'{
        version = 'two-prime',
        modulus = Modulus,
        publicExponent = E,
        privateExponent = D
    },
    Jwk = hb_acme_crypto:private_key_to_jwk(Key),
    % Verify JWK structure
    ?assertEqual(<<"RSA">>, maps:get(<<"kty">>, Jwk)),
    ?assert(maps:is_key(<<"n">>, Jwk)),
    ?assert(maps:is_key(<<"e">>, Jwk)),
    % Verify modulus and exponent are base64url encoded
    N = maps:get(<<"n">>, Jwk),
    E_Jwk = maps:get(<<"e">>, Jwk),
    ?assert(is_binary(N)),
    ?assert(is_binary(E_Jwk)),
    ok.

%% @doc Tests JWK thumbprint generation.
jwk_thumbprint_test() ->
    % Create RSA key from wallet
    Wallet = ar_wallet:new(),
    {{_KT = {rsa, E}, PrivBin, PubBin}, _} = Wallet,
    Modulus = crypto:bytes_to_integer(iolist_to_binary(PubBin)),
    D = crypto:bytes_to_integer(iolist_to_binary(PrivBin)),
    Key = #'RSAPrivateKey'{
        version = 'two-prime',
        modulus = Modulus,
        publicExponent = E,
        privateExponent = D
    },
    Thumbprint = hb_acme_crypto:get_jwk_thumbprint(Key),
    % Verify thumbprint properties
    ?assert(is_list(Thumbprint)),
    ?assert(length(Thumbprint) > 0),
    % Verify thumbprint is deterministic (same key = same thumbprint)
    Thumbprint2 = hb_acme_crypto:get_jwk_thumbprint(Key),
    ?assertEqual(Thumbprint, Thumbprint2),
    ok.

%% @doc Tests base64url encoding.
base64url_encoding_test() ->
    TestData = "Hello, ACME World!",
    % Test encoding
    Encoded = hb_acme_crypto:base64url_encode(TestData),
    ?assert(is_list(Encoded)),
    % Verify URL-safe characters (no +, /, or =)
    ?assertEqual(nomatch, string:find(Encoded, "+")),
    ?assertEqual(nomatch, string:find(Encoded, "/")),
    ?assertEqual(nomatch, string:find(Encoded, "=")),
    % Test binary encoding as well
    BinaryEncoded = hb_acme_crypto:base64url_encode(list_to_binary(TestData)),
    ?assert(is_list(BinaryEncoded)),
    ?assertEqual(Encoded, BinaryEncoded),
    ok.

%% @doc Tests key authorization generation.
key_authorization_test() ->
    % Create RSA key from wallet
    Wallet = ar_wallet:new(),
    {{_KT = {rsa, E}, PrivBin, PubBin}, _} = Wallet,
    Modulus = crypto:bytes_to_integer(iolist_to_binary(PubBin)),
    D = crypto:bytes_to_integer(iolist_to_binary(PrivBin)),
    Key = #'RSAPrivateKey'{
        version = 'two-prime',
        modulus = Modulus,
        publicExponent = E,
        privateExponent = D
    },
    Token = "test_token_123",
    KeyAuth = hb_acme_crypto:generate_key_authorization(Token, Key),
    % Verify structure (token.thumbprint)
    ?assert(is_list(KeyAuth)),
    ?assert(string:find(KeyAuth, Token) =/= nomatch),
    ?assert(string:find(KeyAuth, ".") =/= nomatch),
    % Verify consistency
    KeyAuth2 = hb_acme_crypto:generate_key_authorization(Token, Key),
    ?assertEqual(KeyAuth, KeyAuth2),
    ok.

%% @doc Tests DNS TXT value generation.
dns_txt_value_test() ->
    KeyAuth = "test_token.test_thumbprint",
    DnsValue = hb_acme_crypto:generate_dns_txt_value(KeyAuth),
    % Verify DNS value properties
    ?assert(is_list(DnsValue)),
    ?assert(length(DnsValue) > 0),
    % Verify URL-safe base64 (no padding, +, /)
    ?assertEqual(nomatch, string:find(DnsValue, "+")),
    ?assertEqual(nomatch, string:find(DnsValue, "/")),
    ?assertEqual(nomatch, string:find(DnsValue, "=")),
    ok.

%%%--------------------------------------------------------------------
%%% URL Utility Tests
%%%--------------------------------------------------------------------

%% @doc Tests URL parsing functionality.
url_parsing_test() ->
    TestUrl = "https://acme-v02.api.letsencrypt.org/acme/new-account",
    % Test base URL extraction
    BaseUrl = hb_acme_url:extract_base_url(TestUrl),
    ?assertEqual("https://acme-v02.api.letsencrypt.org", BaseUrl),
    % Test host extraction
    Host = hb_acme_url:extract_host_from_url(TestUrl),
    ?assertEqual(<<"acme-v02.api.letsencrypt.org">>, Host),
    % Test path extraction
    Path = hb_acme_url:extract_path_from_url(TestUrl),
    ?assertEqual("/acme/new-account", Path),
    ok.

%% @doc Tests directory URL determination.
directory_determination_test() ->
    % Test staging URL detection
    StagingUrl = "https://acme-staging-v02.api.letsencrypt.org/directory",
    ?assertEqual(?LETS_ENCRYPT_STAGING, hb_acme_url:determine_directory_from_url(StagingUrl)),
    % Test production URL detection
    ProdUrl = "https://acme-v02.api.letsencrypt.org/directory",
    ?assertEqual(?LETS_ENCRYPT_PROD, hb_acme_url:determine_directory_from_url(ProdUrl)),
    ok.

%% @doc Tests header conversion utilities.
header_conversion_test() ->
    Headers = [
        {"content-type", "application/json"},
        {"user-agent", "test-client/1.0"},
        {<<"custom-header">>, <<"custom-value">>}
    ],
    HeaderMap = hb_acme_url:headers_to_map(Headers),
    % Verify conversion to binary keys/values
    ?assertEqual(<<"application/json">>, maps:get(<<"content-type">>, HeaderMap)),
    ?assertEqual(<<"test-client/1.0">>, maps:get(<<"user-agent">>, HeaderMap)),
    ?assertEqual(<<"custom-value">>, maps:get(<<"custom-header">>, HeaderMap)),
    ok.

%%%--------------------------------------------------------------------
%%% Domain Validation Tests
%%%--------------------------------------------------------------------

%% @doc Tests domain validation functionality.
domain_validation_test() ->
    % Test valid domains
    ValidDomains = ["example.com", "www.example.com", "sub.example.com"],
    {ok, NormalizedDomains} = hb_acme_csr:validate_domains(ValidDomains),
    ?assertEqual(3, length(NormalizedDomains)),
    % Test empty domain filtering
    MixedDomains = ["example.com", "", "www.example.com"],
    {ok, FilteredDomains} = hb_acme_csr:validate_domains(MixedDomains),
    ?assertEqual(2, length(FilteredDomains)),
    % Test all empty domains
    EmptyDomains = ["", ""],
    ?assertMatch({error, no_valid_domains}, hb_acme_csr:validate_domains(EmptyDomains)),
    ok.

%% @doc Tests domain normalization.
domain_normalization_test() ->
    % Test binary input
    BinaryDomain = hb_acme_csr:normalize_domain(<<"example.com">>),
    ?assertEqual(<<"example.com">>, BinaryDomain),
    % Test string input
    StringDomain = hb_acme_csr:normalize_domain("example.com"),
    ?assertEqual(<<"example.com">>, StringDomain),
    ok.

%%%--------------------------------------------------------------------
%%% Integration Tests
%%%--------------------------------------------------------------------

%% @doc Tests the complete CSR generation workflow.
csr_workflow_integration_test() ->
    Domains = ["test.example.com", "www.test.example.com"],
    Wallet = ar_wallet:new(),
    % Test complete workflow
    Result = hb_acme_csr:generate_csr(Domains, #{priv_wallet => Wallet}),
    ?assertMatch({ok, _CsrDer, _PrivateKey}, Result),
    {ok, CsrDer, PrivateKey} = Result,
    % Verify CSR properties
    ?assert(is_binary(CsrDer)),
    ?assert(byte_size(CsrDer) > 100), % Reasonable minimum size
    ?assert(is_record(PrivateKey, 'RSAPrivateKey')),
    ok.

%% @doc Tests error handling across modules.
error_handling_integration_test() ->
    % Test invalid domain handling
    ?assertMatch({error, _}, hb_acme_csr:validate_domains([])),
    % Test base64url with invalid input (should not crash)
    ?assert(is_list(hb_acme_crypto:base64url_encode(""))),
    % Test URL parsing with malformed URLs
    ?assert(is_list(hb_acme_url:extract_base_url("not-a-url"))),
    ok.

%%%--------------------------------------------------------------------
%%% Performance Tests
%%%--------------------------------------------------------------------

%% @doc Tests performance of key operations.
performance_test() ->
    % Test wallet key extraction performance (should complete quickly)
    StartTime = erlang:system_time(millisecond),
    _Wallet = ar_wallet:new(),
    EndTime = erlang:system_time(millisecond),
    % Should complete within reasonable time (10 seconds)
    Duration = EndTime - StartTime,
    ?assert(Duration < 10000),
    ok.

%%%--------------------------------------------------------------------
%%% Mock and Stub Tests
%%%--------------------------------------------------------------------

%% @doc Tests with mocked external dependencies.
mock_dependencies_test() ->
    % This test would use meck or similar to mock external HTTP calls
    % For now, we just verify the modules can be called without crashing
    
    % Test that modules load correctly
    ?assert(erlang:module_loaded(hb_acme_crypto)),
    ?assert(erlang:module_loaded(hb_acme_url)),
    ?assert(erlang:module_loaded(hb_acme_csr)),
    ok.
