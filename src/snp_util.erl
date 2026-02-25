%%% @doc Shared utility functions for SNP commitment reports.
%%%
%%% This module provides common utility functions used across SNP modules to
%%% eliminate code duplication and ensure consistent behavior.
-module(snp_util).
-export([hex_to_binary/1, binary_to_hex_string/1, hex_char_to_int/1]).
-export([get_type_name/1]).
-export([build_validation_error/4, build_field_error/3]).
-export([is_pem_binary/1, is_json_binary/1]).
-export([safe_bin/1, safe_json_decode/1]).
-export([wrap_error/3, wrap_error/4]).
-include("include/hb.hrl").
-include("include/snp_constants.hrl").

%% Standard error reason types
-type error_reason() ::
    {validation_failed, FieldName :: binary(), Reason :: term(), Context :: map()} |
    {conversion_failed, From :: term(), To :: binary(), Reason :: term()} |
    {missing_field, FieldName :: binary()} |
    {invalid_type, FieldName :: binary(), Expected :: binary(), Actual :: term()} |
    {network_error, Operation :: binary(), Reason :: term()} |
    {system_error, Operation :: binary(), Reason :: term()} |
    {operation_failed, Step :: atom(), Reason :: term(), Context :: map()}.

%% Common result types
-type result(T) :: {ok, T} | {error, error_reason()}.
-type maybe_result(T) :: T | {error, error_reason()}.

%% @doc Convert hex string to binary.
%% @param Hex binary() - Hex string (must have even number of bytes, valid hex chars)
%% @returns {ok, binary()} on success, {error, invalid_hex} on invalid or odd-length input
%% @example
%%   hex_to_binary(<<"48656c6c6f">>) =:= {ok, <<"Hello">>}
-spec hex_to_binary(Hex :: binary()) -> {ok, binary()} | {error, invalid_hex}.
hex_to_binary(Hex) when is_binary(Hex), byte_size(Hex) rem 2 =:= 0 ->
    ?event(snp, {hex_to_binary_start, #{hex_size => byte_size(Hex)}}),
    try
        Result = << <<(hex_char_to_int(H) bsl 4 + hex_char_to_int(L))>> || <<H, L>> <= Hex >>,
        ?event(snp, {hex_to_binary_success, #{result_size => byte_size(Result)}}),
        {ok, Result}
    catch
        _:_ ->
            ?event(snp_error, {hex_to_binary_error, #{hex_size => byte_size(Hex)}}),
            {error, invalid_hex}
    end;
hex_to_binary(Hex) ->
    ?event(snp_error, {hex_to_binary_invalid_input, #{hex_size => case is_binary(Hex) of true -> byte_size(Hex); false -> undefined end}}),
    {error, invalid_hex}.

%% @doc Convert binary to hex string for logging.
%% @param Binary binary() - Binary to convert
%% @returns string() - Hex string representation
%% @example
%%   binary_to_hex_string(<<"Hello">>) =:= "48656c6c6f"  % true
-spec binary_to_hex_string(Binary :: binary()) -> string().
binary_to_hex_string(Binary) ->
    hb_util:list(hb_util:to_hex(Binary)).

%% @doc Convert hex character to integer.
%% @param Char char() - Hex character ('0'-'9', 'a'-'f', 'A'-'F')
%% @returns 0..15 - Integer value of hex character
%% @example
%%   hex_char_to_int($A) =:= 10  % true
-spec hex_char_to_int(Char :: char()) -> 0..15.
hex_char_to_int($0) -> 0;
hex_char_to_int($1) -> 1;
hex_char_to_int($2) -> 2;
hex_char_to_int($3) -> 3;
hex_char_to_int($4) -> 4;
hex_char_to_int($5) -> 5;
hex_char_to_int($6) -> 6;
hex_char_to_int($7) -> 7;
hex_char_to_int($8) -> 8;
hex_char_to_int($9) -> 9;
hex_char_to_int($a) -> 10;
hex_char_to_int($A) -> 10;
hex_char_to_int($b) -> 11;
hex_char_to_int($B) -> 11;
hex_char_to_int($c) -> 12;
hex_char_to_int($C) -> 12;
hex_char_to_int($d) -> 13;
hex_char_to_int($D) -> 13;
hex_char_to_int($e) -> 14;
hex_char_to_int($E) -> 14;
hex_char_to_int($f) -> 15;
hex_char_to_int($F) -> 15.

%% @doc Get type name of a term for error messages.
%% @param T term() - Term to get type name for
%% @returns binary() - Type name as binary
%% @example
%%   get_type_name(<<"test">>) =:= <<"binary">>  % true
%%   get_type_name([1,2,3]) =:= <<"list">>  % true
-spec get_type_name(term()) -> binary().
get_type_name(T) when is_binary(T) -> <<"binary">>;
get_type_name(T) when is_list(T) -> <<"list">>;
get_type_name(T) when is_map(T) -> <<"map">>;
get_type_name(T) when is_integer(T) -> <<"integer">>;
get_type_name(T) when is_atom(T) -> <<"atom">>;
get_type_name(_) -> <<"other">>.

%% @doc Build a validation error message.
%% @param FieldName binary() - Name of the field being validated
%% @param ExpectedType binary() - Expected type description
%% @param ActualValue term() - Actual value that failed validation
%% @param Suggestion binary() - Suggestion for fixing the error
%% @returns binary() - Formatted error message
-spec build_validation_error(FieldName :: binary(), ExpectedType :: binary(), 
                             ActualValue :: term(), Suggestion :: binary()) -> binary().
build_validation_error(FieldName, ExpectedType, ActualValue, Suggestion) ->
    <<FieldName/binary, " validation failed: expected ", ExpectedType/binary,
      ", got ", (get_type_name(ActualValue))/binary, ". ", Suggestion/binary>>.

%% @doc Build a field error message.
%% @param FieldName binary() - Name of the field
%% @param ExpectedType binary() - Expected type description
%% @param ActualValue term() - Actual value that failed validation
%% @returns binary() - Formatted error message
-spec build_field_error(FieldName :: binary(), ExpectedType :: binary(), 
                        ActualValue :: term()) -> binary().
build_field_error(FieldName, ExpectedType, ActualValue) ->
    <<"Invalid ", FieldName/binary, " type: expected ", ExpectedType/binary,
      ", got ", (get_type_name(ActualValue))/binary, ".">>.

%% @doc Check if binary is PEM format.
%% @param PemBinary binary() - Binary to check
%% @returns boolean() - true if binary appears to be PEM format
-spec is_pem_binary(binary()) -> boolean().
is_pem_binary(<<"-----BEGIN", _/binary>>) -> true;
is_pem_binary(_) -> false.

%% @doc Check if binary is JSON format (basic check).
%% @param JsonBinary binary() - Binary to check
%% @returns boolean() - true if binary appears to be JSON format
-spec is_json_binary(binary()) -> boolean().
is_json_binary(<<"{", _/binary>>) -> true;
is_json_binary(<<"[", _/binary>>) -> true;
is_json_binary(_) -> false.

%% @doc Safely convert a value to binary, handling errors.
%% @param Value term() - Value to convert
%% @returns {ok, binary()} | {error, error_reason()}
-spec safe_bin(term()) -> {ok, binary()} | {error, error_reason()}.
safe_bin(Value) ->
    try
        Binary = hb_util:bin(Value),
        case is_binary(Binary) of
            true -> {ok, Binary};
            false -> {error, {conversion_failed, Value, <<"binary">>, <<"hb_util:bin returned non-binary">>}}
        end
    catch
        Error:Reason ->
            {error, {conversion_failed, Value, <<"binary">>, {Error, Reason}}}
    end.

%% @doc Safely decode JSON, handling errors.
%% @param JsonBinary binary() - JSON string to decode
%% @returns {ok, map()} | {error, error_reason()}
-spec safe_json_decode(binary()) -> {ok, map()} | {error, error_reason()}.
safe_json_decode(JsonBinary) when is_binary(JsonBinary) ->
    try
        Decoded = hb_json:decode(JsonBinary),
        case Decoded of
            Map when is_map(Map) -> {ok, Map};
            Other ->
                {error, {conversion_failed, JsonBinary, <<"map">>, 
                    {invalid_format, <<"JSON decoded to ", (get_type_name(Other))/binary>>}}}
        end
    catch
        Error:Reason ->
            {error, {conversion_failed, JsonBinary, <<"map">>, {Error, Reason}}}
    end;
safe_json_decode(Invalid) ->
    {error, {invalid_type, <<"json">>, <<"binary">>, Invalid}}.

%% @doc Wrap an error with operation context.
%% @param Step atom() - The step/operation that failed
%% @param Reason term() - The original error reason
%% @param Context map() - Additional context about the operation
%% @returns {error, error_reason()}
-spec wrap_error(Step :: atom(), Reason :: term(), Context :: map()) -> 
    {error, error_reason()}.
wrap_error(Step, Reason, Context) ->
    {error, {operation_failed, Step, Reason, Context}}.

%% @doc Wrap an error with operation context and field name.
%% @param Step atom() - The step/operation that failed
%% @param FieldName binary() - The field that caused the error
%% @param Reason term() - The original error reason
%% @param Context map() - Additional context about the operation
%% @returns {error, error_reason()}
-spec wrap_error(Step :: atom(), FieldName :: binary(), Reason :: term(), Context :: map()) -> 
    {error, error_reason()}.
wrap_error(Step, FieldName, Reason, Context) ->
    {error, {validation_failed, FieldName, Reason, Context#{step => Step}}}.

