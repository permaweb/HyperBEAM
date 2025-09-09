%%% @doc SSL Certificate utility module.
%%%
%%% This module provides utility functions for SSL certificate management
%%% including error formatting, response building, and common helper functions
%%% used across the SSL certificate system.
%%%
%%% The module centralizes formatting logic and provides consistent error
%%% handling and response generation for the SSL certificate system.
-module(hb_ssl_cert_util).

%% No includes needed for basic utility functions

%% Public API
-export([
    format_error_details/1,
    build_error_response/2,
    build_success_response/2,
    format_validation_error/1,
    extract_ssl_opts/1,
    normalize_domains/1,
    normalize_email/1
]).

%% Type specifications
-spec format_error_details(term()) -> binary().
-spec build_error_response(integer(), binary()) -> {error, map()}.
-spec build_success_response(integer(), map()) -> {ok, map()}.
-spec format_validation_error(binary()) -> {error, map()}.
-spec extract_ssl_opts(map()) -> {ok, map()} | {error, binary()}.
-spec normalize_domains(term()) -> [string()].
-spec normalize_email(term()) -> string().

%% @doc Formats error details for user-friendly display.
%%
%% This function takes various error reason formats and converts them
%% to user-friendly binary strings suitable for API responses.
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
        {validation_failed, ValidationErrors} when is_list(ValidationErrors) ->
            ErrorList = [hb_util:bin(io_lib:format("~s", [E])) || E <- ValidationErrors],
            ErrorsBin = hb_util:bin(string:join([binary_to_list(E) || E <- ErrorList], ", ")),
            <<"Validation failed: ", ErrorsBin/binary>>;
        {acme_error, AcmeDetails} ->
            AcmeBin = hb_util:bin(io_lib:format("~p", [AcmeDetails])),
            <<"ACME error: ", AcmeBin/binary>>;
        Binary when is_binary(Binary) ->
            Binary;
        List when is_list(List) ->
            hb_util:bin(List);
        Atom when is_atom(Atom) ->
            hb_util:bin(atom_to_list(Atom));
        Other ->
            hb_util:bin(io_lib:format("~p", [Other]))
    end.

%% @doc Builds a standardized error response.
%%
%% @param StatusCode HTTP status code
%% @param ErrorMessage Error message as binary
%% @returns Standardized error response tuple
build_error_response(StatusCode, ErrorMessage) when is_integer(StatusCode), is_binary(ErrorMessage) ->
    {error, #{<<"status">> => StatusCode, <<"error">> => ErrorMessage}}.

%% @doc Builds a standardized success response.
%%
%% @param StatusCode HTTP status code
%% @param Body Response body map
%% @returns Standardized success response tuple
build_success_response(StatusCode, Body) when is_integer(StatusCode), is_map(Body) ->
    {ok, #{<<"status">> => StatusCode, <<"body">> => Body}}.


%% @doc Formats validation errors for consistent API responses.
%%
%% @param ValidationError Validation error message
%% @returns Formatted validation error response
format_validation_error(ValidationError) when is_binary(ValidationError) ->
    build_error_response(400, ValidationError).

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

%% @doc Normalizes domain input to a list of strings.
%%
%% This function handles various input formats for domains and converts
%% them to a consistent list of strings format.
%%
%% @param Domains Domain input in various formats
%% @returns List of domain strings
normalize_domains(Domains) when is_list(Domains) ->
    try
        [hb_util:list(D) || D <- Domains, is_binary(D) orelse is_list(D)]
    catch
        _:_ -> []
    end;
normalize_domains(Domain) when is_binary(Domain) ->
    [hb_util:list(Domain)];
normalize_domains(Domain) when is_list(Domain) ->
    try
        [hb_util:list(Domain)]
    catch
        _:_ -> []
    end;
normalize_domains(_) ->
    [].

%% @doc Normalizes email input to a string.
%%
%% This function handles various input formats for email addresses and
%% converts them to a consistent string format.
%%
%% @param Email Email input in various formats
%% @returns Email as string
normalize_email(Email) when is_binary(Email) ->
    hb_util:list(Email);
normalize_email(Email) when is_list(Email) ->
    try
        hb_util:list(Email)
    catch
        _:_ -> ""
    end;
normalize_email(_) ->
    "".
