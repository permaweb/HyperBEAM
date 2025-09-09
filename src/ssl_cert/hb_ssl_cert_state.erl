%%% @doc SSL Certificate state management module.
%%%
%%% This module handles all state management operations for SSL certificate
%%% requests including serialization, deserialization, persistence, and
%%% state transformations between internal records and external map formats.
%%%
%%% The module provides a clean interface for storing and retrieving certificate
%%% request state while hiding the complexity of format conversions.
-module(hb_ssl_cert_state).

-include("include/ssl_cert_records.hrl").
-include_lib("public_key/include/public_key.hrl").

%% Public API
-export([
    create_request_state/4,
    serialize_account/1,
    deserialize_account/1,
    serialize_order/1,
    deserialize_order/1,
    serialize_challenges/1,
    deserialize_challenges/1,
    serialize_private_key/1,
    deserialize_private_key/1,
    serialize_wallet_private_key/1,
    update_order_in_state/2,
    extract_account_from_state/1,
    extract_order_from_state/1,
    extract_challenges_from_state/1
]).

%% Type specifications
-spec create_request_state(acme_account(), acme_order(), [dns_challenge()], map()) -> 
    request_state().
-spec serialize_account(acme_account()) -> map().
-spec deserialize_account(map()) -> acme_account().
-spec serialize_order(acme_order()) -> map().
-spec deserialize_order(map()) -> acme_order().
-spec serialize_challenges([dns_challenge()]) -> [map()].
-spec deserialize_challenges([map()]) -> [dns_challenge()].
-spec serialize_private_key(public_key:private_key()) -> string().
-spec deserialize_private_key(string()) -> public_key:private_key().

%% @doc Creates a complete request state map from ACME components.
%%
%% This function takes the core ACME components (account, order, challenges)
%% and additional parameters to create a comprehensive state map that can
%% be stored and later used to continue the certificate request process.
%%
%% @param Account The ACME account record
%% @param Order The ACME order record
%% @param Challenges List of DNS challenge records
%% @param ValidatedParams The validated request parameters
%% @returns Complete request state map
create_request_state(Account, Order, Challenges, ValidatedParams) ->
    ChallengesMaps = serialize_challenges(Challenges),
    Domains = maps:get(domains, ValidatedParams, []),
    #{
        <<"account">> => serialize_account(Account),
        <<"order">> => serialize_order(Order),
        <<"challenges">> => ChallengesMaps,
        <<"domains">> => [hb_util:bin(D) || D <- Domains],
        <<"status">> => <<"pending_dns">>,
        <<"created">> => calendar:universal_time(),
        <<"config">> => serialize_config(ValidatedParams)
    }.


%% @doc Serializes an ACME account record to a map.
%%
%% @param Account The ACME account record
%% @returns Serialized account map
serialize_account(Account) when is_record(Account, acme_account) ->
    #{
        <<"key_pem">> => hb_util:bin(serialize_private_key(Account#acme_account.key)),
        <<"url">> => hb_util:bin(Account#acme_account.url),
        <<"kid">> => hb_util:bin(Account#acme_account.kid)
    }.

%% @doc Deserializes an account map back to an ACME account record.
%%
%% @param AccountMap The serialized account map
%% @returns ACME account record
deserialize_account(AccountMap) when is_map(AccountMap) ->
    #acme_account{
        key = deserialize_private_key(hb_util:list(maps:get(<<"key_pem">>, AccountMap))),
        url = hb_util:list(maps:get(<<"url">>, AccountMap)),
        kid = hb_util:list(maps:get(<<"kid">>, AccountMap))
    }.

%% @doc Serializes an ACME order record to a map.
%%
%% @param Order The ACME order record
%% @returns Serialized order map
serialize_order(Order) when is_record(Order, acme_order) ->
    #{
        <<"url">> => hb_util:bin(Order#acme_order.url),
        <<"status">> => hb_util:bin(Order#acme_order.status),
        <<"expires">> => hb_util:bin(Order#acme_order.expires),
        <<"identifiers">> => Order#acme_order.identifiers,
        <<"authorizations">> => Order#acme_order.authorizations,
        <<"finalize">> => hb_util:bin(Order#acme_order.finalize),
        <<"certificate">> => hb_util:bin(Order#acme_order.certificate)
    }.

%% @doc Deserializes an order map back to an ACME order record.
%%
%% @param OrderMap The serialized order map
%% @returns ACME order record
deserialize_order(OrderMap) when is_map(OrderMap) ->
    #acme_order{
        url = hb_util:list(maps:get(<<"url">>, OrderMap)),
        status = hb_util:list(maps:get(<<"status">>, OrderMap)),
        expires = hb_util:list(maps:get(<<"expires">>, OrderMap)),
        identifiers = maps:get(<<"identifiers">>, OrderMap),
        authorizations = maps:get(<<"authorizations">>, OrderMap),
        finalize = hb_util:list(maps:get(<<"finalize">>, OrderMap)),
        certificate = hb_util:list(maps:get(<<"certificate">>, OrderMap, ""))
    }.

%% @doc Serializes a list of DNS challenge records to maps.
%%
%% @param Challenges List of DNS challenge records
%% @returns List of serialized challenge maps
serialize_challenges(Challenges) when is_list(Challenges) ->
    [serialize_challenge(C) || C <- Challenges].

%% @doc Deserializes a list of challenge maps back to DNS challenge records.
%%
%% @param ChallengeMaps List of serialized challenge maps
%% @returns List of DNS challenge records
deserialize_challenges(ChallengeMaps) when is_list(ChallengeMaps) ->
    [deserialize_challenge(C) || C <- ChallengeMaps].

%% @doc Serializes an RSA private key to PEM format for storage.
%%
%% @param PrivateKey The RSA private key record
%% @returns PEM-encoded private key as string
serialize_private_key(PrivateKey) ->
    DerKey = public_key:der_encode('RSAPrivateKey', PrivateKey),
    PemBinary = public_key:pem_encode([{'RSAPrivateKey', DerKey, not_encrypted}]),
    binary_to_list(PemBinary).

%% @doc Deserializes a PEM-encoded private key back to RSA record.
%%
%% @param PemKey The PEM-encoded private key string
%% @returns RSA private key record
deserialize_private_key(PemKey) ->
    % Clean up the PEM string (remove extra whitespace) and convert to binary
    CleanPem = hb_util:bin(string:trim(PemKey)),
    [{'RSAPrivateKey', DerKey, not_encrypted}] = public_key:pem_decode(CleanPem),
    public_key:der_decode('RSAPrivateKey', DerKey).

%% @doc Serializes wallet private key components to PEM format for nginx.
%%
%% This function extracts the RSA components from the wallet and creates
%% a proper nginx-compatible private key. The key will match the one used
%% in CSR generation to ensure certificate compatibility.
%%
%% @param WalletTuple The complete wallet tuple containing RSA components
%% @returns PEM-encoded private key as string
serialize_wallet_private_key(WalletTuple) ->
    % Extract the same RSA key that's used in CSR generation
    {{_KT = {rsa, E}, PrivBin, PubBin}, _} = WalletTuple,
    Modulus = crypto:bytes_to_integer(iolist_to_binary(PubBin)),
    D = crypto:bytes_to_integer(iolist_to_binary(PrivBin)),
    
    % Create the same RSA private key structure as used in CSR generation
    % This ensures the private key matches the certificate
    RSAPrivKey = #'RSAPrivateKey'{
        version = 'two-prime',
        modulus = Modulus,
        publicExponent = E,
        privateExponent = D
    },
    
    % Serialize to PEM format for nginx
    serialize_private_key(RSAPrivKey).

%% @doc Updates the order information in a request state.
%%
%% @param State The current request state
%% @param UpdatedOrder The updated ACME order record
%% @returns Updated request state
update_order_in_state(State, UpdatedOrder) when is_map(State), is_record(UpdatedOrder, acme_order) ->
    UpdatedOrderMap = serialize_order(UpdatedOrder),
    OrderStatusBin = hb_util:bin(UpdatedOrder#acme_order.status),
    State#{
        <<"order">> => UpdatedOrderMap,
        <<"status">> => OrderStatusBin
    }.

%% @doc Extracts and deserializes the account from request state.
%%
%% @param State The request state map
%% @returns ACME account record
extract_account_from_state(State) when is_map(State) ->
    AccountMap = maps:get(<<"account">>, State),
    deserialize_account(AccountMap).

%% @doc Extracts and deserializes the order from request state.
%%
%% @param State The request state map
%% @returns ACME order record
extract_order_from_state(State) when is_map(State) ->
    OrderMap = maps:get(<<"order">>, State),
    deserialize_order(OrderMap).

%% @doc Extracts and deserializes the challenges from request state.
%%
%% @param State The request state map
%% @returns List of DNS challenge records
extract_challenges_from_state(State) when is_map(State) ->
    ChallengeMaps = maps:get(<<"challenges">>, State, []),
    deserialize_challenges(ChallengeMaps).

%%%--------------------------------------------------------------------
%%% Internal Functions
%%%--------------------------------------------------------------------

%% @doc Serializes a single DNS challenge record to a map.
%%
%% @param Challenge The DNS challenge record
%% @returns Serialized challenge map
serialize_challenge(Challenge) when is_record(Challenge, dns_challenge) ->
    #{
        <<"domain">> => hb_util:bin(Challenge#dns_challenge.domain),
        <<"token">> => hb_util:bin(Challenge#dns_challenge.token),
        <<"key_authorization">> => hb_util:bin(Challenge#dns_challenge.key_authorization),
        <<"dns_value">> => hb_util:bin(Challenge#dns_challenge.dns_value),
        <<"url">> => hb_util:bin(Challenge#dns_challenge.url)
    }.

%% @doc Deserializes a single challenge map back to a DNS challenge record.
%%
%% @param ChallengeMap The serialized challenge map
%% @returns DNS challenge record
deserialize_challenge(ChallengeMap) when is_map(ChallengeMap) ->
    #dns_challenge{
        domain = hb_util:list(maps:get(<<"domain">>, ChallengeMap)),
        token = hb_util:list(maps:get(<<"token">>, ChallengeMap)),
        key_authorization = hb_util:list(maps:get(<<"key_authorization">>, ChallengeMap)),
        dns_value = hb_util:list(maps:get(<<"dns_value">>, ChallengeMap)),
        url = hb_util:list(maps:get(<<"url">>, ChallengeMap))
    }.

%% @doc Serializes configuration parameters for storage in state.
%%
%% @param ValidatedParams The validated parameters map
%% @returns Serialized configuration map
serialize_config(ValidatedParams) ->
    maps:map(fun(K, V) -> 
        case {K, V} of
            {dns_propagation_wait, _} when is_integer(V) -> V;
            {validation_timeout, _} when is_integer(V) -> V;
            {include_chain, _} when is_boolean(V) -> V;
            {key_size, _} when is_integer(V) -> V;
            {_, _} when is_atom(V) -> V;
            {_, _} -> hb_util:bin(V)
        end
    end, ValidatedParams).
