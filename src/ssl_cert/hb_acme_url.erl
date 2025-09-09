%%% @doc ACME URL utilities module.
%%%
%%% This module provides URL parsing, validation, and manipulation utilities
%%% for ACME (Automatic Certificate Management Environment) operations.
%%% It handles URL decomposition, directory URL determination, and header
%%% format conversions needed for ACME protocol communication.
-module(hb_acme_url).

-include("include/ssl_cert_records.hrl").

%% Public API
-export([
    extract_base_url/1,
    extract_host_from_url/1, 
    extract_path_from_url/1,
    determine_directory_from_url/1,
    determine_directory_from_account/1,
    headers_to_map/1,
    normalize_url/1
]).

%% Type specifications
-spec extract_base_url(string() | binary()) -> string().
-spec extract_host_from_url(string() | binary()) -> binary().
-spec extract_path_from_url(string() | binary()) -> string().
-spec determine_directory_from_url(string() | binary()) -> string().
-spec determine_directory_from_account(acme_account()) -> string().
-spec headers_to_map([{string() | binary(), string() | binary()}]) -> map().
-spec normalize_url(string() | binary()) -> string().

%% @doc Extracts the base URL (scheme + host) from a complete URL.
%%
%% This function parses a URL and returns only the scheme and host portion,
%% which is useful for creating HTTP client connections.
%%
%% Examples:
%%   extract_base_url("https://acme-v02.api.letsencrypt.org/directory") 
%%   -> "https://acme-v02.api.letsencrypt.org"
%%
%% @param Url The complete URL string or binary
%% @returns The base URL (e.g., "https://example.com") as string
extract_base_url(Url) ->
    UrlStr = hb_util:list(Url),
    case string:split(UrlStr, "://") of
        [Scheme, Rest] ->
            case string:split(Rest, "/") of
                [Host | _] -> hb_util:list(Scheme) ++ "://" ++ hb_util:list(Host)
            end;
        [_] ->
            % No scheme, assume https
            case string:split(UrlStr, "/") of
                [Host | _] -> "https://" ++ hb_util:list(Host)
            end
    end.

%% @doc Extracts the host from a URL.
%%
%% This function parses a URL and returns only the host portion as a binary,
%% which is useful for host-based routing or validation.
%%
%% Examples:
%%   extract_host_from_url("https://acme-v02.api.letsencrypt.org/directory")
%%   -> <<"acme-v02.api.letsencrypt.org">>
%%
%% @param Url The complete URL string or binary
%% @returns The host portion as binary
extract_host_from_url(Url) ->
    % Parse URL to extract host
    UrlStr = hb_util:list(Url),
    case string:split(UrlStr, "://") of
        [_Scheme, Rest] ->
            case string:split(Rest, "/") of
                [Host | _] -> hb_util:bin(hb_util:list(Host))
            end;
        [Host] ->
            case string:split(Host, "/") of
                [HostOnly | _] -> hb_util:bin(hb_util:list(HostOnly))
            end
    end.

%% @doc Extracts the path from a URL.
%%
%% This function parses a URL and returns only the path portion,
%% which is needed for HTTP request routing.
%%
%% Examples:
%%   extract_path_from_url("https://acme-v02.api.letsencrypt.org/directory")
%%   -> "/directory"
%%
%% @param Url The complete URL string or binary
%% @returns The path portion as string (always starts with "/")
extract_path_from_url(Url) ->
    % Parse URL to extract path
    UrlStr = hb_util:list(Url),
    case string:split(UrlStr, "://") of
        [_Scheme, Rest] ->
            case string:split(Rest, "/") of
                [_Host | PathParts] -> "/" ++ string:join([hb_util:list(P) || P <- PathParts], "/")
            end;
        [Rest] ->
            case string:split(Rest, "/") of
                [_Host | PathParts] -> "/" ++ string:join([hb_util:list(P) || P <- PathParts], "/")
            end
    end.

%% @doc Determines the ACME directory URL from any ACME endpoint URL.
%%
%% This function examines a URL to determine whether it belongs to the
%% Let's Encrypt staging or production environment and returns the
%% appropriate directory URL.
%%
%% @param Url Any ACME endpoint URL
%% @returns The directory URL string (staging or production)
determine_directory_from_url(Url) ->
    case string:find(Url, "staging") of
        nomatch -> ?LETS_ENCRYPT_PROD;
        _ -> ?LETS_ENCRYPT_STAGING
    end.

%% @doc Determines the ACME directory URL from an account record.
%%
%% This function examines an ACME account's URL to determine whether
%% it was created in the staging or production environment.
%%
%% @param Account The ACME account record
%% @returns The directory URL string (staging or production)
determine_directory_from_account(Account) ->
    case string:find(Account#acme_account.url, "staging") of
        nomatch -> ?LETS_ENCRYPT_PROD;
        _ -> ?LETS_ENCRYPT_STAGING
    end.

%% @doc Converts header list to map format.
%%
%% This function converts HTTP headers from the proplist format 
%% [{Key, Value}, ...] to a map format for easier manipulation.
%% It handles both string and binary keys/values.
%%
%% @param Headers List of {Key, Value} header tuples
%% @returns Map of headers with binary keys and values
headers_to_map(Headers) ->
    maps:from_list([{hb_util:bin(K), hb_util:bin(V)} || {K, V} <- Headers]).

%% @doc Normalizes a URL to a consistent string format.
%%
%% This function ensures URLs are in a consistent format for processing,
%% handling both string and binary inputs and ensuring proper encoding.
%%
%% @param Url The URL to normalize
%% @returns Normalized URL as string
normalize_url(Url) ->
    UrlStr = hb_util:list(Url),
    % Basic normalization - ensure it starts with http:// or https://
    case string:prefix(UrlStr, "http://") orelse string:prefix(UrlStr, "https://") of
        nomatch ->
            % No scheme provided, assume https
            "https://" ++ UrlStr;
        _ ->
            % Already has scheme
            UrlStr
    end.
