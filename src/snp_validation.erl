%%% @doc Centralized input validation for SNP commitment reports.
%%%
%%% This module provides consistent validation functions for common input types
%%% used across SNP modules, including ChipId, SPL values, report binaries,
%%% and PEM certificates.
-module(snp_validation).
-export([validate_chip_id/1, validate_spl_value/2, validate_spl_values/4,
         validate_report_binary/1, validate_pem_binary/1]).
-export([validate_size/3, validate_type/3, validate_range/4]).
-include("include/hb.hrl").
-include("include/snp_constants.hrl").
-include("include/snp_guids.hrl").

%% Type definitions for validation results
-type validation_result(T) :: {ok, T} | {error, binary()}.
-type spl_name() :: atom() | binary().
-type spl_value() :: 0..255.

%% @doc Validate ChipId is exactly 64 bytes.
%% @param ChipId The chip ID to validate (can be binary or list)
%% @returns {ok, ChipIdBinary} if valid, {error, Reason} if invalid
-spec validate_chip_id(ChipId :: binary() | list()) -> 
    validation_result(binary()).
validate_chip_id(ChipId) when is_binary(ChipId) ->
    case byte_size(ChipId) of
        ?CHIP_ID_SIZE ->
            {ok, ChipId};
        ActualSize ->
            ErrorMsg = <<"ChipId validation failed: expected exactly ", 
                (hb_util:bin(integer_to_list(?CHIP_ID_SIZE)))/binary, 
                " bytes, got ", (hb_util:bin(integer_to_list(ActualSize)))/binary, 
                ". Ensure ChipId is a ", (hb_util:bin(integer_to_list(?CHIP_ID_SIZE)))/binary, 
                "-byte binary from the SNP report.">>,
            ?event(snp_error, {validate_chip_id_failed, #{
                operation => <<"validate_chip_id">>,
                expected_size => ?CHIP_ID_SIZE,
                actual_size => ActualSize,
                chip_id_type => <<"binary">>,
                suggestion => <<"Ensure ChipId is exactly ", 
                    (hb_util:bin(integer_to_list(?CHIP_ID_SIZE)))/binary, 
                    " bytes. Extract it from the 'chip_id' field in the SNP report.">>
            }}),
            {error, ErrorMsg}
    end;
validate_chip_id(ChipId) when is_list(ChipId) ->
    case length(ChipId) of
        ?CHIP_ID_SIZE ->
            ChipIdBinary = hb_util:bin(ChipId),
            {ok, ChipIdBinary};
        ActualLength ->
            ErrorMsg = <<"ChipId validation failed: expected list of exactly ", 
                (hb_util:bin(integer_to_list(?CHIP_ID_SIZE)))/binary, 
                " bytes, got ", (hb_util:bin(integer_to_list(ActualLength)))/binary, 
                ". Ensure ChipId is a list containing ", 
                (hb_util:bin(integer_to_list(?CHIP_ID_SIZE)))/binary, 
                " bytes from the SNP report.">>,
            ?event(snp_error, {validate_chip_id_failed, #{
                operation => <<"validate_chip_id">>,
                expected_size => ?CHIP_ID_SIZE,
                actual_size => ActualLength,
                chip_id_type => <<"list">>,
                suggestion => <<"Ensure ChipId is a list containing exactly ", 
                    (hb_util:bin(integer_to_list(?CHIP_ID_SIZE)))/binary, 
                    " bytes. Extract it from the 'chip_id' field in the SNP report.">>
            }}),
            {error, ErrorMsg}
    end;
validate_chip_id(Invalid) ->
    TypeName = snp_util:get_type_name(Invalid),
    ErrorMsg = <<"ChipId validation failed: expected binary or list, got ", TypeName/binary, ".">>,
    ?event(snp_error, {validate_chip_id_failed, #{
        operation => <<"validate_chip_id">>,
        expected_type => <<"binary or list">>,
        actual_type => TypeName,
        suggestion => <<"ChipId must be a binary or list containing exactly ", 
            (hb_util:bin(integer_to_list(?CHIP_ID_SIZE)))/binary, 
            " bytes. Extract it from the 'chip_id' field in the SNP report.">>
    }}),
    {error, ErrorMsg}.

%% @doc Validate a single SPL value is in valid range (0-255).
%% @param SPLValue The SPL value to validate
%% @param SPLName The name of the SPL field (for error messages)
%% @returns {ok, SPLValue} if valid, {error, Reason} if invalid
-spec validate_spl_value(SPLValue :: term(), SPLName :: spl_name()) -> 
    validation_result(spl_value()).
validate_spl_value(SPLValue, _SPLName) when is_integer(SPLValue), 
                                           SPLValue >= 0, 
                                           SPLValue =< ?MAX_SPL_VALUE ->
    {ok, SPLValue};
validate_spl_value(SPLValue, SPLName) when is_integer(SPLValue) ->
    SPLNameBin = case SPLName of
        A when is_atom(A) -> hb_util:bin(atom_to_list(A));
        B when is_binary(B) -> B;
        _ -> <<"spl">>
    end,
    ErrorMsg = <<"SPL validation failed: ", SPLNameBin/binary, 
        " expected integer in range 0-", (hb_util:bin(integer_to_list(?MAX_SPL_VALUE)))/binary, 
        ", got ", (hb_util:bin(integer_to_list(SPLValue)))/binary, ".">>,
    ?event(snp_error, {validate_spl_value_failed, #{
        operation => <<"validate_spl_value">>,
        spl_name => SPLNameBin,
        expected_range => <<"0-", (hb_util:bin(integer_to_list(?MAX_SPL_VALUE)))/binary>>,
        actual_value => SPLValue,
        suggestion => <<"Ensure ", SPLNameBin/binary, 
            " is an integer in the range 0-", (hb_util:bin(integer_to_list(?MAX_SPL_VALUE)))/binary, 
            ". Check the TCB structure in the SNP report.">>
    }}),
    {error, ErrorMsg};
validate_spl_value(Invalid, SPLName) ->
    SPLNameBin = case SPLName of
        A when is_atom(A) -> hb_util:bin(atom_to_list(A));
        B when is_binary(B) -> B;
        _ -> <<"spl">>
    end,
    TypeName = snp_util:get_type_name(Invalid),
    ErrorMsg = <<"SPL validation failed: ", SPLNameBin/binary, 
        " expected integer, got ", (hb_util:bin(TypeName))/binary, ".">>,
    ?event(snp_error, {validate_spl_value_failed, #{
        operation => <<"validate_spl_value">>,
        spl_name => SPLNameBin,
        expected_type => <<"integer">>,
        actual_type => TypeName,
        suggestion => <<"Ensure ", SPLNameBin/binary, 
            " is an integer in the range 0-", (hb_util:bin(integer_to_list(?MAX_SPL_VALUE)))/binary, 
            ". Check the TCB structure in the SNP report.">>
    }}),
    {error, ErrorMsg}.

%% @doc Validate all four SPL values are in valid range (0-255).
%% @param BootloaderSPL Bootloader SPL value
%% @param TeeSPL TEE SPL value
%% @param SnpSPL SNP SPL value
%% @param UcodeSPL Microcode SPL value
%% @returns ok if all valid, {error, Reason} if any invalid
-spec validate_spl_values(BootloaderSPL :: integer(), TeeSPL :: integer(),
    SnpSPL :: integer(), UcodeSPL :: integer()) -> 
    ok | {error, binary()}.
validate_spl_values(BootloaderSPL, TeeSPL, SnpSPL, UcodeSPL) ->
    SPLValues = [
        {bootloader, BootloaderSPL},
        {tee, TeeSPL},
        {snp, SnpSPL},
        {ucode, UcodeSPL}
    ],
    ValidationResults = lists:map(
        fun({Name, Value}) -> 
            {Name, validate_spl_value(Value, Name)}
        end,
        SPLValues
    ),
    InvalidResults = lists:filter(
        fun({_Name, Result}) -> 
            case Result of
                {error, _} -> true;
                _ -> false
            end
        end,
        ValidationResults
    ),
    case InvalidResults of
        [] ->
            ok;
        _ ->
            InvalidDetails = lists:map(
                fun({Name, {error, ErrorMsg}}) ->
                    <<(hb_util:bin(atom_to_list(Name)))/binary, ": ", ErrorMsg/binary>>
                end,
                InvalidResults
            ),
            ErrorMsg = <<"SPL validation failed: ", 
                (hb_util:bin(string:join([hb_util:list(D) || D <- InvalidDetails], "; ")))/binary, 
                ". All SPL values must be integers in range 0-", 
                (hb_util:bin(integer_to_list(?MAX_SPL_VALUE)))/binary, ".">>,
            ?event(snp_error, {validate_spl_values_failed, #{
                operation => <<"validate_spl_values">>,
                invalid_count => length(InvalidResults),
                invalid_values => InvalidResults,
                expected_range => <<"0-", (hb_util:bin(integer_to_list(?MAX_SPL_VALUE)))/binary>>,
                suggestion => <<"Ensure all SPL values (bootloader, tee, snp, ucode) are integers in the range 0-", 
                    (hb_util:bin(integer_to_list(?MAX_SPL_VALUE)))/binary, 
                    ". Check the TCB structure in the SNP report.">>
            }}),
            {error, ErrorMsg}
    end.

%% @doc Validate report binary is exactly 1184 bytes.
%% @param ReportBinary The report binary to validate
%% @returns {ok, ReportBinary} if valid, {error, Reason} if invalid
-spec validate_report_binary(ReportBinary :: binary()) -> 
    validation_result(binary()).
validate_report_binary(ReportBinary) when is_binary(ReportBinary) ->
    case byte_size(ReportBinary) of
        ?REPORT_SIZE ->
            {ok, ReportBinary};
        ActualSize ->
            ErrorMsg = <<"Report binary validation failed: expected exactly ", 
                (hb_util:bin(integer_to_list(?REPORT_SIZE)))/binary, 
                " bytes, got ", (hb_util:bin(integer_to_list(ActualSize)))/binary, 
                ". Ensure the report is a complete ", 
                (hb_util:bin(integer_to_list(?REPORT_SIZE)))/binary, 
                "-byte binary as specified in the SNP report format.">>,
            ?event(snp_error, {validate_report_binary_failed, #{
                operation => <<"validate_report_binary">>,
                expected_size => ?REPORT_SIZE,
                actual_size => ActualSize,
                suggestion => <<"Ensure the report binary is exactly ", 
                    (hb_util:bin(integer_to_list(?REPORT_SIZE)))/binary, 
                    " bytes. The SNP report format requires a fixed-size binary structure.">>
            }}),
            {error, ErrorMsg}
    end;
validate_report_binary(Invalid) ->
    TypeName = snp_util:get_type_name(Invalid),
    ErrorMsg = <<"Report binary validation failed: expected binary, got ", (hb_util:bin(TypeName))/binary, ".">>,
    ?event(snp_error, {validate_report_binary_failed, #{
        operation => <<"validate_report_binary">>,
        expected_type => <<"binary">>,
        actual_type => TypeName,
        suggestion => <<"Ensure the report is a binary containing exactly ", 
            (hb_util:bin(integer_to_list(?REPORT_SIZE)))/binary, 
            " bytes. Convert JSON to binary using snp_report_format:report_json_to_binary/1 if needed.">>
    }}),
    {error, ErrorMsg}.

%% @doc Validate binary is PEM format.
%% @param PemBinary The binary to validate
%% @returns {ok, PemBinary} if valid, {error, Reason} if invalid
-spec validate_pem_binary(PemBinary :: binary()) -> 
    validation_result(binary()).
validate_pem_binary(PemBinary) when is_binary(PemBinary) ->
    case snp_util:is_pem_binary(PemBinary) of
        true ->
            {ok, PemBinary};
        false ->
            ErrorMsg = <<"PEM validation failed: binary does not appear to be in PEM format. ",
                "PEM format should start with '-----BEGIN'.">>,
            ?event(snp_error, {validate_pem_binary_failed, #{
                operation => <<"validate_pem_binary">>,
                binary_preview => binary:part(PemBinary, 0, min(50, byte_size(PemBinary))),
                suggestion => <<"Ensure the certificate is in PEM format (text-based, starts with '-----BEGIN'). ",
                    "If you have DER format, convert it to PEM first.">>
            }}),
            {error, ErrorMsg}
    end;
validate_pem_binary(Invalid) ->
    TypeName = snp_util:get_type_name(Invalid),
    ErrorMsg = <<"PEM validation failed: expected binary, got ", (hb_util:bin(TypeName))/binary, ".">>,
    ?event(snp_error, {validate_pem_binary_failed, #{
        operation => <<"validate_pem_binary">>,
        expected_type => <<"binary">>,
        actual_type => TypeName,
        suggestion => <<"Ensure the certificate is a binary in PEM format (text-based, starts with '-----BEGIN').">>
    }}),
    {error, ErrorMsg}.

%% @doc Generic validation helper: validate size of binary or list.
%% @param Value binary() | list() - Value to validate
%% @param ExpectedSize non_neg_integer() - Expected size
%% @param FieldName binary() - Field name for error messages
%% @returns {ok, binary()} if valid, {error, binary()} if invalid
-spec validate_size(Value :: binary() | list(), ExpectedSize :: non_neg_integer(), 
                   FieldName :: binary()) -> validation_result(binary()).
validate_size(Value, ExpectedSize, FieldName) when is_binary(Value) ->
    ActualSize = byte_size(Value),
    case ActualSize =:= ExpectedSize of
        true -> {ok, Value};
        false ->
            ErrorMsg = <<FieldName/binary, " validation failed: expected exactly ", 
                (hb_util:bin(integer_to_list(ExpectedSize)))/binary, 
                " bytes, got ", (hb_util:bin(integer_to_list(ActualSize)))/binary, ".">>,
            ?event(snp_error, {validate_size_failed, #{
                operation => <<"validate_size">>,
                field_name => FieldName,
                expected_size => ExpectedSize,
                actual_size => ActualSize,
                suggestion => <<"Ensure ", FieldName/binary, " is exactly ", 
                    (hb_util:bin(integer_to_list(ExpectedSize)))/binary, " bytes.">>
            }}),
            {error, ErrorMsg}
    end;
validate_size(Value, ExpectedSize, FieldName) when is_list(Value) ->
    ActualSize = length(Value),
    case ActualSize =:= ExpectedSize of
        true -> 
            ValueBinary = hb_util:bin(Value),
            {ok, ValueBinary};
        false ->
            ErrorMsg = <<FieldName/binary, " validation failed: expected list of exactly ", 
                (hb_util:bin(integer_to_list(ExpectedSize)))/binary, 
                " bytes, got ", (hb_util:bin(integer_to_list(ActualSize)))/binary, ".">>,
            ?event(snp_error, {validate_size_failed, #{
                operation => <<"validate_size">>,
                field_name => FieldName,
                expected_size => ExpectedSize,
                actual_size => ActualSize,
                suggestion => <<"Ensure ", FieldName/binary, " is a list containing exactly ", 
                    (hb_util:bin(integer_to_list(ExpectedSize)))/binary, " bytes.">>
            }}),
            {error, ErrorMsg}
    end;
validate_size(Invalid, _ExpectedSize, FieldName) ->
    TypeName = snp_util:get_type_name(Invalid),
    ErrorMsg = <<FieldName/binary, " validation failed: expected binary or list, got ", 
        TypeName/binary, ".">>,
    ?event(snp_error, {validate_size_failed, #{
        operation => <<"validate_size">>,
        field_name => FieldName,
        expected_type => <<"binary or list">>,
        actual_type => TypeName,
        suggestion => <<"Ensure ", FieldName/binary, " is a binary or list.">>
    }}),
    {error, ErrorMsg}.

%% @doc Generic validation helper: validate type of a value.
%% @param Value term() - Value to validate
%% @param TypeCheck fun((term()) -> boolean()) - Function to check if value is correct type
%% @param FieldName binary() - Field name for error messages
%% @returns ok if valid, {error, binary()} if invalid
-spec validate_type(Value :: term(), TypeCheck :: fun((term()) -> boolean()), 
                   FieldName :: binary()) -> ok | {error, binary()}.
validate_type(Value, TypeCheck, FieldName) when is_function(TypeCheck, 1) ->
    case TypeCheck(Value) of
        true -> ok;
        false ->
            TypeName = snp_util:get_type_name(Value),
            ErrorMsg = <<FieldName/binary, " validation failed: invalid type, got ", 
                TypeName/binary, ".">>,
            ?event(snp_error, {validate_type_failed, #{
                operation => <<"validate_type">>,
                field_name => FieldName,
                actual_type => TypeName,
                suggestion => <<"Ensure ", FieldName/binary, " has the correct type.">>
            }}),
            {error, ErrorMsg}
    end.

%% @doc Generic validation helper: validate integer is in valid range.
%% @param Value integer() - Value to validate
%% @param Min integer() - Minimum allowed value (inclusive)
%% @param Max integer() - Maximum allowed value (inclusive)
%% @param FieldName binary() - Field name for error messages
%% @returns {ok, integer()} if valid, {error, binary()} if invalid
-spec validate_range(Value :: integer(), Min :: integer(), Max :: integer(), 
                    FieldName :: binary()) -> validation_result(integer()).
validate_range(Value, Min, Max, FieldName) when is_integer(Value) ->
    case Value >= Min andalso Value =< Max of
        true -> {ok, Value};
        false ->
            ErrorMsg = <<FieldName/binary, " validation failed: expected integer in range ", 
                (hb_util:bin(integer_to_list(Min)))/binary, "-", 
                (hb_util:bin(integer_to_list(Max)))/binary, 
                ", got ", (hb_util:bin(integer_to_list(Value)))/binary, ".">>,
            ?event(snp_error, {validate_range_failed, #{
                operation => <<"validate_range">>,
                field_name => FieldName,
                expected_range => <<(hb_util:bin(integer_to_list(Min)))/binary, "-", 
                    (hb_util:bin(integer_to_list(Max)))/binary>>,
                actual_value => Value,
                suggestion => <<"Ensure ", FieldName/binary, 
                    " is an integer in the range ", 
                    (hb_util:bin(integer_to_list(Min)))/binary, "-", 
                    (hb_util:bin(integer_to_list(Max)))/binary, ".">>
            }}),
            {error, ErrorMsg}
    end;
validate_range(Invalid, _Min, _Max, FieldName) ->
    TypeName = snp_util:get_type_name(Invalid),
    ErrorMsg = <<FieldName/binary, " validation failed: expected integer, got ", 
        (hb_util:bin(TypeName))/binary, ".">>,
    ?event(snp_error, {validate_range_failed, #{
        operation => <<"validate_range">>,
        field_name => FieldName,
        expected_type => <<"integer">>,
        actual_type => TypeName,
        suggestion => <<"Ensure ", FieldName/binary, " is an integer.">>
    }}),
    {error, ErrorMsg}.

