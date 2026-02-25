%%% @doc Report format conversion for SNP commitment reports.
%%%
%%% This module handles conversion between binary (1184-byte) and JSON formats
%%% for AMD SEV-SNP attestation reports.
-module(snp_report_format).
-export([report_binary_to_json/1, report_json_to_binary/1, validate_report_schema/1]).
-include("include/hb.hrl").
-include("include/snp_constants.hrl").
-include("include/snp_guids.hrl").

%% Type definitions
-type report_binary() :: binary().  % Exactly ?REPORT_SIZE bytes
-type report_json() :: binary() | map().  % JSON string or decoded map

%% Helper function to construct TCB binary from map
%% Optimized to avoid repeated pattern matching
-spec build_tcb_binary(TCBMap :: map()) -> binary().
build_tcb_binary(TCBMap) ->
    <<(maps:get(<<"bootloader">>, TCBMap, 0)):8,
      (maps:get(<<"tee">>, TCBMap, 0)):8,
      0:(?TCB_RESERVED_BYTES * 8),  % ?TCB_RESERVED_BYTES reserved bytes (bytes 2-5)
      (maps:get(<<"snp">>, TCBMap, 0)):8,
      (maps:get(<<"microcode">>, TCBMap, 0)):8>>.

%% Helper function to normalize binary to exact size (pad or truncate)
%% Optimized to avoid multiple pattern matches
-spec normalize_binary_size(Binary :: binary(), TargetSize :: non_neg_integer()) -> binary().
normalize_binary_size(Binary, TargetSize) when is_binary(Binary) ->
    case byte_size(Binary) of
        TargetSize -> Binary;
        Size when Size > TargetSize -> binary:part(Binary, 0, TargetSize);
        Size when Size < TargetSize -> 
            PaddingSize = TargetSize - Size,
            <<Binary/binary, 0:(PaddingSize * 8)>>
    end;
normalize_binary_size(_, TargetSize) ->
    <<0:(TargetSize * 8)>>.

%% @doc Convert binary report structure (1184 bytes) to JSON map.
%% This replaces the C JSON serialization for better error handling.
%% @param ReportBinary 1184-byte binary containing the raw report structure
%% @returns Map containing the report fields as Erlang terms
-spec report_binary_to_json(ReportBinary :: report_binary()) -> {ok, map()} | {error, binary()}.
report_binary_to_json(ReportBinary) when byte_size(ReportBinary) =:= ?REPORT_SIZE ->
    <<Version:32/little-unsigned-integer,
      GuestSvn:32/little-unsigned-integer,
      Policy:64/little-unsigned-integer,
      FamilyId:?FAMILY_ID_SIZE/binary,
      ImageId:?IMAGE_ID_SIZE/binary,
      Vmpl:32/little-unsigned-integer,
      SigAlgo:32/little-unsigned-integer,
      CurrentTcb:?TCB_SIZE/binary,
      PlatInfo:64/little-unsigned-integer,
      AuthorKeyEn:32/little-unsigned-integer,
      Reserved0:32/little-unsigned-integer,
      ReportData:?CHIP_ID_SIZE/binary,
      Measurement:?LAUNCH_DIGEST_SIZE/binary,
      HostData:?HOST_DATA_SIZE/binary,
      IdKeyDigest:?LAUNCH_DIGEST_SIZE/binary,
      AuthorKeyDigest:?LAUNCH_DIGEST_SIZE/binary,
      ReportId:?REPORT_ID_SIZE/binary,
      ReportIdMa:?REPORT_ID_SIZE/binary,
      ReportedTcb:?TCB_SIZE/binary,
      _Reserved1:?RESERVED1_SIZE/binary,
      ChipId:?CHIP_ID_SIZE/binary,
      CommittedTcb:?TCB_SIZE/binary,
      CurrentBuild:8,
      CurrentMinor:8,
      CurrentMajor:8,
      Reserved2:8,
      CommittedBuild:8,
      CommittedMinor:8,
      CommittedMajor:8,
      Reserved3:8,
      LaunchTcb:?TCB_SIZE/binary,
      _Reserved4:?REPORT_SIGNATURE_SIZE/binary,
      SignatureR:?SIGNATURE_R_SIZE/binary,
      SignatureS:?SIGNATURE_S_SIZE/binary,
      _SignatureReserved:?SIGNATURE_RESERVED_TOTAL_SIZE/binary>> = ReportBinary,
    
    #{
        <<"version">> => Version,
        <<"guest_svn">> => GuestSvn,
        <<"policy">> => Policy,
        <<"family_id">> => hb_util:list(FamilyId),
        <<"image_id">> => hb_util:list(ImageId),
        <<"vmpl">> => Vmpl,
        <<"sig_algo">> => SigAlgo,
        <<"current_tcb">> => begin
            % TcbVersion structure: bootloader(?TCB_OFFSET_BOOTLOADER), tee(?TCB_OFFSET_TEE), _reserved(2-5), snp(?TCB_OFFSET_SNP), microcode(?TCB_OFFSET_MICROCODE)
            Bootloader = binary:at(CurrentTcb, ?TCB_OFFSET_BOOTLOADER),
            Tee = binary:at(CurrentTcb, ?TCB_OFFSET_TEE),
            Snp = binary:at(CurrentTcb, ?TCB_OFFSET_SNP),  % Skip ?TCB_RESERVED_BYTES reserved bytes (2-5)
            Microcode = binary:at(CurrentTcb, ?TCB_OFFSET_MICROCODE),
            ?event(snp, {binary_to_json_current_tcb, #{
                raw_binary_hex => snp_util:binary_to_hex_string(CurrentTcb),
                bootloader => Bootloader,
                tee => Tee,
                snp => Snp,
                microcode => Microcode
            }}),
            #{
                <<"bootloader">> => Bootloader,
                <<"tee">> => Tee,
                <<"snp">> => Snp,
                <<"microcode">> => Microcode
            }
        end,
        <<"plat_info">> => PlatInfo,
        <<"_author_key_en">> => AuthorKeyEn,
        <<"_reserved_0">> => Reserved0,
        <<"report_data">> => hb_util:list(ReportData),
        <<"measurement">> => hb_util:list(Measurement),
        <<"host_data">> => hb_util:list(HostData),
        <<"id_key_digest">> => hb_util:list(IdKeyDigest),
        <<"author_key_digest">> => hb_util:list(AuthorKeyDigest),
        <<"report_id">> => hb_util:list(ReportId),
        <<"report_id_ma">> => hb_util:list(ReportIdMa),
        <<"reported_tcb">> => #{
            <<"bootloader">> => binary:at(ReportedTcb, ?TCB_OFFSET_BOOTLOADER),
            <<"tee">> => binary:at(ReportedTcb, ?TCB_OFFSET_TEE),
            <<"snp">> => binary:at(ReportedTcb, ?TCB_OFFSET_SNP),  % Skip ?TCB_RESERVED_BYTES reserved bytes (2-5)
            <<"microcode">> => binary:at(ReportedTcb, ?TCB_OFFSET_MICROCODE)
        },
        <<"chip_id">> => hb_util:list(ChipId),
        <<"committed_tcb">> => #{
            <<"bootloader">> => binary:at(CommittedTcb, ?TCB_OFFSET_BOOTLOADER),
            <<"tee">> => binary:at(CommittedTcb, ?TCB_OFFSET_TEE),
            <<"snp">> => binary:at(CommittedTcb, ?TCB_OFFSET_SNP),  % Skip ?TCB_RESERVED_BYTES reserved bytes (2-5)
            <<"microcode">> => binary:at(CommittedTcb, ?TCB_OFFSET_MICROCODE)
        },
        <<"current_build">> => CurrentBuild,
        <<"current_minor">> => CurrentMinor,
        <<"current_major">> => CurrentMajor,
        <<"_reserved_2">> => Reserved2,
        <<"committed_build">> => CommittedBuild,
        <<"committed_minor">> => CommittedMinor,
        <<"committed_major">> => CommittedMajor,
        <<"_reserved_3">> => Reserved3,
        <<"launch_tcb">> => #{
            <<"bootloader">> => binary:at(LaunchTcb, ?TCB_OFFSET_BOOTLOADER),
            <<"tee">> => binary:at(LaunchTcb, ?TCB_OFFSET_TEE),
            <<"snp">> => binary:at(LaunchTcb, ?TCB_OFFSET_SNP),  % Skip ?TCB_RESERVED_BYTES reserved bytes (2-5)
            <<"microcode">> => binary:at(LaunchTcb, ?TCB_OFFSET_MICROCODE)
        },
        <<"signature">> => #{
            <<"r">> => hb_util:list(SignatureR),
            <<"s">> => hb_util:list(SignatureS)
        }
    };
report_binary_to_json(InvalidBinary) ->
    ActualSize = case is_binary(InvalidBinary) of
        true -> byte_size(InvalidBinary);
        false -> <<"not_a_binary">>
    end,
    ?event(snp_error, {report_binary_to_json_invalid_size, #{
        operation => <<"report_binary_to_json">>,
        actual_size => ActualSize,
        expected_size => ?REPORT_SIZE,
        actual_type => case is_binary(InvalidBinary) of true -> <<"binary">>; false -> <<"not_binary">> end,
        suggestion => <<"Ensure the report binary is exactly ", (hb_util:bin(integer_to_list(?REPORT_SIZE)))/binary, " bytes as specified in the SNP report format.">>
    }}),
    {error, <<"Report binary validation failed: expected exactly ", 
        (hb_util:bin(integer_to_list(?REPORT_SIZE)))/binary, 
        " bytes, got ", 
        (hb_util:bin(case is_binary(InvalidBinary) of true -> integer_to_list(byte_size(InvalidBinary)); false -> "not a binary" end))/binary,
        ". Ensure the report is a complete ", (hb_util:bin(integer_to_list(?REPORT_SIZE)))/binary, "-byte binary.">>}.

%% @doc Convert JSON report map to binary report structure (?REPORT_SIZE bytes).
%% This reconstructs the binary structure from parsed JSON for signature verification.
%% @param ReportJSON Binary containing JSON report OR map
%% @returns 1184-byte binary containing the raw report structure
-spec report_json_to_binary(ReportJSON :: report_json()) -> report_binary() | {error, term()}.
report_json_to_binary(ReportJSON) when is_binary(ReportJSON) ->
    ?event(snp_short, {json_input_size, byte_size(ReportJSON)}),
    case snp_util:safe_json_decode(ReportJSON) of
        {ok, ReportMap} ->
            ?event(snp_short, {json_decoded_to_map, #{
                has_current_tcb => maps:is_key(<<"current_tcb">>, ReportMap),
                map_size => map_size(ReportMap)
            }}),
            report_json_to_binary(ReportMap);
        {error, {conversion_failed, _, _, {invalid_format, TypeMsg}}} ->
            ?event(snp_error, {report_json_to_binary_invalid_json, #{
                operation => <<"report_json_to_binary">>,
                actual_type => TypeMsg,
                expected => <<"valid JSON that decodes to a map">>,
                suggestion => <<"Ensure the input is valid JSON that decodes to a map/object containing all required SNP report fields.">>
            }}),
            {error, <<"JSON format validation failed: expected valid JSON that decodes to a map, got invalid format. Ensure the JSON is properly formatted and contains all required fields.">>};
        {error, {conversion_failed, _, _, {Error, Reason}}} ->
            ?event(snp_error, {report_json_to_binary_decode_error, #{
                operation => <<"report_json_to_binary">>,
                error => Error,
                reason => Reason,
                suggestion => <<"JSON decode failed. Ensure the input is valid JSON format.">>
            }}),
            {error, <<"JSON decode failed: ", (hb_util:bin(io_lib:format("~p", [Reason])))/binary, ". Ensure the input is valid JSON format.">>};
        {error, Reason} ->
            ?event(snp_error, {report_json_to_binary_error, #{
                operation => <<"report_json_to_binary">>,
                reason => Reason,
                suggestion => <<"JSON processing failed. Check the input format.">>
            }}),
            {error, Reason}
    end;
report_json_to_binary(ReportMap) when is_map(ReportMap) ->
    % Validate report schema and field values before conversion
    case validate_report_schema(ReportMap) of
        ok ->
            report_json_to_binary_validated(ReportMap);
        {error, ValidationErrors} ->
            ?event(snp_error, {report_json_to_binary_validation_failed, #{
                operation => <<"report_json_to_binary">>,
                validation_errors => ValidationErrors,
                error_count => length(ValidationErrors),
                suggestion => <<"Fix the validation errors before converting the report. Check that all fields are present, have correct types, and values are within valid ranges.">>
            }}),
            {error, {validation_failed, ValidationErrors}}
    end;
report_json_to_binary(InvalidInput) ->
    ActualType = case InvalidInput of
        B when is_binary(B) -> <<"binary">>;
        M when is_map(M) -> <<"map">>;
        L when is_list(L) -> <<"list">>;
        _ -> <<"other">>
    end,
    ?event(snp_error, {report_json_to_binary_invalid_input, #{
        operation => <<"report_json_to_binary">>,
        actual_type => ActualType,
        expected => <<"binary (JSON string) or map">>,
        suggestion => <<"Provide either a JSON-encoded binary string or a map containing the SNP report fields.">>
    }}),
    {error, <<"Report format validation failed: expected binary (JSON) or map, got ", 
        ActualType/binary, ". Provide a valid JSON string or map containing the SNP report data.">>}.

%% Internal function to perform conversion after validation
-spec report_json_to_binary_validated(ReportMap :: map()) -> binary() | {error, term()}.
report_json_to_binary_validated(ReportMap) ->
    try
        Version = maps:get(<<"version">>, ReportMap),
        GuestSvn = maps:get(<<"guest_svn">>, ReportMap),
        Policy = maps:get(<<"policy">>, ReportMap),
        FamilyId = hb_util:bin(maps:get(<<"family_id">>, ReportMap)),
        ImageId = hb_util:bin(maps:get(<<"image_id">>, ReportMap)),
        Vmpl = maps:get(<<"vmpl">>, ReportMap),
        SigAlgo = maps:get(<<"sig_algo">>, ReportMap),
        CurrentTcbMap = maps:get(<<"current_tcb">>, ReportMap),
        ?event(snp, {current_tcb_map_raw, #{
            map_keys => maps:keys(CurrentTcbMap),
            map_size => maps:size(CurrentTcbMap),
            bootloader_value => maps:get(<<"bootloader">>, CurrentTcbMap, not_found),
            tee_value => maps:get(<<"tee">>, CurrentTcbMap, not_found),
            snp_value => maps:get(<<"snp">>, CurrentTcbMap, not_found),
            microcode_value => maps:get(<<"microcode">>, CurrentTcbMap, not_found),
            all_entries => maps:to_list(CurrentTcbMap)
        }}),
        CurrentTcbBootloader = maps:get(<<"bootloader">>, CurrentTcbMap, 0),
        CurrentTcbTee = maps:get(<<"tee">>, CurrentTcbMap, 0),
        CurrentTcbSnp = maps:get(<<"snp">>, CurrentTcbMap, 0),
        CurrentTcbMicrocode = maps:get(<<"microcode">>, CurrentTcbMap, 0),
        ?event(snp, {current_tcb_values, #{
            bootloader => CurrentTcbBootloader,
            tee => CurrentTcbTee,
            snp => CurrentTcbSnp,
            microcode => CurrentTcbMicrocode
        }}),
        % TcbVersion structure: bootloader(?TCB_OFFSET_BOOTLOADER), tee(?TCB_OFFSET_TEE), _reserved(2-5), snp(?TCB_OFFSET_SNP), microcode(?TCB_OFFSET_MICROCODE)
        CurrentTcb = build_tcb_binary(CurrentTcbMap),
        PlatInfo = maps:get(<<"plat_info">>, ReportMap),
        AuthorKeyEn = maps:get(<<"_author_key_en">>, ReportMap, 0),
        Reserved0 = maps:get(<<"_reserved_0">>, ReportMap, 0),
        ReportData = hb_util:bin(maps:get(<<"report_data">>, ReportMap)),
        Measurement = hb_util:bin(maps:get(<<"measurement">>, ReportMap)),
        HostData = hb_util:bin(maps:get(<<"host_data">>, ReportMap)),
        IdKeyDigest = hb_util:bin(maps:get(<<"id_key_digest">>, ReportMap)),
        AuthorKeyDigest = hb_util:bin(maps:get(<<"author_key_digest">>, ReportMap)),
        ReportId = hb_util:bin(maps:get(<<"report_id">>, ReportMap)),
        ReportIdMa = hb_util:bin(maps:get(<<"report_id_ma">>, ReportMap)),
        ReportedTcbMap = maps:get(<<"reported_tcb">>, ReportMap),
        % TcbVersion structure: bootloader(?TCB_OFFSET_BOOTLOADER), tee(?TCB_OFFSET_TEE), _reserved(2-5), snp(?TCB_OFFSET_SNP), microcode(?TCB_OFFSET_MICROCODE)
        ReportedTcb = build_tcb_binary(ReportedTcbMap),
        ChipId = hb_util:bin(maps:get(<<"chip_id">>, ReportMap)),
        CommittedTcbMap = maps:get(<<"committed_tcb">>, ReportMap),
        % TcbVersion structure: bootloader(?TCB_OFFSET_BOOTLOADER), tee(?TCB_OFFSET_TEE), _reserved(2-5), snp(?TCB_OFFSET_SNP), microcode(?TCB_OFFSET_MICROCODE)
        CommittedTcb = build_tcb_binary(CommittedTcbMap),
        CurrentBuild = maps:get(<<"current_build">>, ReportMap, 0),
        CurrentMinor = maps:get(<<"current_minor">>, ReportMap, 0),
        CurrentMajor = maps:get(<<"current_major">>, ReportMap, 0),
        Reserved2 = maps:get(<<"_reserved_2">>, ReportMap, 0),
        CommittedBuild = maps:get(<<"committed_build">>, ReportMap, 0),
        CommittedMinor = maps:get(<<"committed_minor">>, ReportMap, 0),
        CommittedMajor = maps:get(<<"committed_major">>, ReportMap, 0),
        Reserved3 = maps:get(<<"_reserved_3">>, ReportMap, 0),
        LaunchTcbMap = maps:get(<<"launch_tcb">>, ReportMap),
        % TcbVersion structure: bootloader(?TCB_OFFSET_BOOTLOADER), tee(?TCB_OFFSET_TEE), _reserved(2-5), snp(?TCB_OFFSET_SNP), microcode(?TCB_OFFSET_MICROCODE)
        LaunchTcb = build_tcb_binary(LaunchTcbMap),
        SignatureMap = maps:get(<<"signature">>, ReportMap),
        SignatureRList = maps:get(<<"r">>, SignatureMap),
        SignatureSList = maps:get(<<"s">>, SignatureMap),
        ?event(snp, {signature_from_json, #{
            r_list_length => length(SignatureRList),
            s_list_length => length(SignatureSList),
            r_first_8 => lists:sublist(SignatureRList, 1, min(8, length(SignatureRList))),
            s_first_8 => lists:sublist(SignatureSList, 1, min(8, length(SignatureSList)))
        }}),
        SignatureR = hb_util:bin(SignatureRList),
        SignatureS = hb_util:bin(SignatureSList),
        ?event(snp, {signature_converted_to_binary, #{
            r_size => byte_size(SignatureR),
            s_size => byte_size(SignatureS),
            r_first_8_bytes_hex => snp_util:binary_to_hex_string(binary:part(SignatureR, 0, min(8, byte_size(SignatureR)))),
            s_first_8_bytes_hex => snp_util:binary_to_hex_string(binary:part(SignatureS, 0, min(8, byte_size(SignatureS))))
        }}),
        
        % Reconstruct binary report structure
        ?event(snp, {before_binary_construction, #{
            signature_r_size => byte_size(SignatureR),
            signature_s_size => byte_size(SignatureS),
            signature_r_first_8_hex => snp_util:binary_to_hex_string(binary:part(SignatureR, 0, min(8, byte_size(SignatureR)))),
            signature_s_first_8_hex => snp_util:binary_to_hex_string(binary:part(SignatureS, 0, min(8, byte_size(SignatureS))))
        }}),
        % Construct main portion (everything before signature)
        % Calculate expected size: 4+4+8+16+16+4+4+8+8+4+4+64+48+32+48+48+32+32+8+24+64+8+1+1+1+1+1+1+1+1+8+168 = 672 bytes
        % But signature should start at 1016, so there might be padding or the structure is different
        ?event(snp, {before_main_portion_construction, #{
            expected_main_portion_size => ?REPORT_MAIN_PORTION_SIZE,
            calculated_field_sizes => 672,
            current_tcb_binary_hex => snp_util:binary_to_hex_string(CurrentTcb)
        }}),
        MainPortion = <<
            Version:32/little-unsigned-integer,
            GuestSvn:32/little-unsigned-integer,
            Policy:64/little-unsigned-integer,
            FamilyId:?FAMILY_ID_SIZE/binary,
            ImageId:?IMAGE_ID_SIZE/binary,
            Vmpl:32/little-unsigned-integer,
            SigAlgo:32/little-unsigned-integer,
            CurrentTcb:?TCB_SIZE/binary,
            PlatInfo:64/little-unsigned-integer,
            AuthorKeyEn:32/little-unsigned-integer,
            Reserved0:32/little-unsigned-integer,
            ReportData:?CHIP_ID_SIZE/binary,
            Measurement:?LAUNCH_DIGEST_SIZE/binary,
            HostData:?HOST_DATA_SIZE/binary,
            IdKeyDigest:?LAUNCH_DIGEST_SIZE/binary,
            AuthorKeyDigest:?LAUNCH_DIGEST_SIZE/binary,
            ReportId:?REPORT_ID_SIZE/binary,
            ReportIdMa:?REPORT_ID_SIZE/binary,
            ReportedTcb:?TCB_SIZE/binary,
            0:?RESERVED1_BITS,  % Reserved1 (?RESERVED1_SIZE bytes)
            ChipId:?CHIP_ID_SIZE/binary,
            CommittedTcb:?TCB_SIZE/binary,
            CurrentBuild:8,
            CurrentMinor:8,
            CurrentMajor:8,
            Reserved2:8,
            CommittedBuild:8,
            CommittedMinor:8,
            CommittedMajor:8,
            Reserved3:8,
            LaunchTcb:?TCB_SIZE/binary,
            0:?RESERVED4_BITS  % Reserved4 (?REPORT_SIGNATURE_SIZE bytes)
        >>,
        MainPortionSize = byte_size(MainPortion),
        ?event(snp, {main_portion_constructed, #{
            main_portion_size => MainPortionSize,
            expected_size => ?REPORT_MAIN_PORTION_SIZE,
            padding_needed => ?REPORT_MAIN_PORTION_SIZE - MainPortionSize
        }}),
        % Pad MainPortion to exactly ?REPORT_MAIN_PORTION_SIZE bytes to match the actual binary format
        % The Rust struct may have padding for alignment, but the binary format requires ?REPORT_MAIN_PORTION_SIZE bytes before signature
        MainPortionPadded = normalize_binary_size(MainPortion, ?REPORT_MAIN_PORTION_SIZE),
        ?event(snp, {main_portion_padded, #{
            padded_size => byte_size(MainPortionPadded),
            expected_size => ?REPORT_MAIN_PORTION_SIZE
        }}),
        % Construct the signature portion separately to ensure correct insertion
        % Signature reserved is ?SIGNATURE_RESERVED_SIZE bytes (?SIGNATURE_RESERVED_BITS bits)
        SignaturePortion = <<SignatureR/binary, SignatureS/binary, 0:?SIGNATURE_RESERVED_BITS>>,
        % Verify signature portion before concatenation
        ?event(snp, {signature_portion_constructed, #{
            sig_portion_size => byte_size(SignaturePortion),
            expected_size => ?SIGNATURE_R_SIZE + ?SIGNATURE_S_SIZE + ?SIGNATURE_RESERVED_SIZE,
            sig_r_first_8_hex => snp_util:binary_to_hex_string(binary:part(SignatureR, 0, min(8, byte_size(SignatureR)))),
            sig_s_first_8_hex => snp_util:binary_to_hex_string(binary:part(SignatureS, 0, min(8, byte_size(SignatureS)))),
                    portion_r_first_8_hex => snp_util:binary_to_hex_string(binary:part(SignaturePortion, 0, min(8, byte_size(SignaturePortion)))),
            portion_r_at_offset_1016 => case byte_size(SignaturePortion) >= 8 of
                true -> snp_util:binary_to_hex_string(binary:part(SignaturePortion, 0, 8));
                false -> <<"too_small">>
            end
        }}),
        % Concatenate the main portion with the signature portion
        ReportBinary = <<MainPortionPadded/binary, SignaturePortion/binary>>,
        % Verify signature was correctly placed in binary
        ?event(snp, {after_binary_construction, #{
            report_binary_size => byte_size(ReportBinary),
            expected_size => ?REPORT_SIZE
        }}),
        % Extract signature from constructed binary to verify
        case byte_size(ReportBinary) >= ?REPORT_MAIN_PORTION_SIZE + ?SIGNATURE_PORTION_SIZE of
            true ->
                <<_:(?REPORT_MAIN_PORTION_SIZE)/binary, SigRFromBinary:?SIGNATURE_R_SIZE/binary, SigSFromBinary:?SIGNATURE_S_SIZE/binary, _/binary>> = ReportBinary,
                ?event(snp, {signature_in_constructed_binary, #{
                    r_first_8_hex => snp_util:binary_to_hex_string(binary:part(SigRFromBinary, 0, min(8, byte_size(SigRFromBinary)))),
                    s_first_8_hex => snp_util:binary_to_hex_string(binary:part(SigSFromBinary, 0, min(8, byte_size(SigSFromBinary)))),
                    r_all_zeros => (SigRFromBinary =:= <<0:?SIGNATURE_R_BITS>>),
                    s_all_zeros => (SigSFromBinary =:= <<0:?SIGNATURE_S_BITS>>)
                }});
            false ->
                ?event(snp, {binary_too_small_for_signature, #{
                    actual_size => byte_size(ReportBinary),
                    required_size => ?REPORT_MAIN_PORTION_SIZE + ?SIGNATURE_PORTION_SIZE
                }})
        end,
        ReportBinary
    catch
        Error:Reason ->
            ?event(snp_error, {report_json_to_binary_conversion_error, #{
                operation => <<"report_json_to_binary">>,
                error => Error,
                reason => Reason,
                suggestion => <<"Check that all required fields are present and have the correct types. Required fields include: version, guest_svn, policy, current_tcb, chip_id, measurement, and signature components.">>
            }}),
            {error, {conversion_error, Error, Reason}}
    end.


%% @doc Validate report schema and field values
%% @param ReportMap map() - Report map to validate
%% @returns ok | {error, ValidationErrors} where ValidationErrors is a list of detailed error messages
-spec validate_report_schema(ReportMap :: map()) -> ok | {error, [binary()]}.
validate_report_schema(ReportMap) when is_map(ReportMap) ->
    ValidationErrors = [],
    ValidationErrors1 = validate_required_fields(ReportMap, ValidationErrors),
    ValidationErrors2 = validate_version(ReportMap, ValidationErrors1),
    ValidationErrors3 = validate_guest_svn(ReportMap, ValidationErrors2),
    ValidationErrors4 = validate_policy(ReportMap, ValidationErrors3),
    ValidationErrors5 = validate_vmpl(ReportMap, ValidationErrors4),
    ValidationErrors6 = validate_sig_algo(ReportMap, ValidationErrors5),
    ValidationErrors7 = validate_tcb_fields(ReportMap, ValidationErrors6),
    ValidationErrors8 = validate_version_numbers(ReportMap, ValidationErrors7),
    ValidationErrors9 = validate_binary_fields(ReportMap, ValidationErrors8),
    ValidationErrors10 = validate_signature(ReportMap, ValidationErrors9),
    case ValidationErrors10 of
        [] -> ok;
        Errors -> {error, Errors}
    end;
validate_report_schema(InvalidInput) ->
    {error, [<<"Report schema validation failed: expected map, got ", 
        (hb_util:bin(case InvalidInput of
            B when is_binary(B) -> "binary";
            L when is_list(L) -> "list";
            _ -> "other"
        end))/binary, ".">>]}.

%% Validate required fields are present
-spec validate_required_fields(ReportMap :: map(), Errors :: [binary()]) -> [binary()].
validate_required_fields(ReportMap, Errors) ->
    RequiredFields = [
        <<"version">>, <<"guest_svn">>, <<"policy">>, <<"family_id">>, <<"image_id">>,
        <<"vmpl">>, <<"sig_algo">>, <<"current_tcb">>, <<"plat_info">>,
        <<"report_data">>, <<"measurement">>, <<"host_data">>, <<"id_key_digest">>,
        <<"author_key_digest">>, <<"report_id">>, <<"report_id_ma">>, <<"reported_tcb">>,
        <<"chip_id">>, <<"committed_tcb">>, <<"launch_tcb">>, <<"signature">>
    ],
    MissingFields = lists:filter(fun(Field) -> not maps:is_key(Field, ReportMap) end, RequiredFields),
    case MissingFields of
        [] -> Errors;
        _ ->
            MissingFieldsStr = string:join([hb_util:list(F) || F <- MissingFields], ", "),
            ErrorMsg = <<"Missing required fields: ", (hb_util:bin(MissingFieldsStr))/binary, 
                ". All SNP report fields must be present.">>,
            [ErrorMsg | Errors]
    end.

%% Validate version field
-spec validate_version(ReportMap :: map(), Errors :: [binary()]) -> [binary()].
validate_version(ReportMap, Errors) ->
    case maps:get(<<"version">>, ReportMap, undefined) of
        undefined -> Errors;
        Version when is_integer(Version), Version >= 0, Version =< 16#FFFFFFFF ->
            Errors;
        Version when is_integer(Version) ->
            ErrorMsg = <<"Invalid version: expected unsigned 32-bit integer (0-4294967295), got ", 
                (hb_util:bin(integer_to_list(Version)))/binary, ".">>,
            [ErrorMsg | Errors];
        Invalid ->
            ErrorMsg = <<"Invalid version type: expected integer, got ", 
                (hb_util:bin(case Invalid of
                    B when is_binary(B) -> "binary";
                    L when is_list(L) -> "list";
                    _ -> "other"
                end))/binary, ".">>,
            [ErrorMsg | Errors]
    end.

%% Validate guest_svn field
-spec validate_guest_svn(ReportMap :: map(), Errors :: [binary()]) -> [binary()].
validate_guest_svn(ReportMap, Errors) ->
    case maps:get(<<"guest_svn">>, ReportMap, undefined) of
        undefined -> Errors;
        GuestSvn when is_integer(GuestSvn), GuestSvn >= 0, GuestSvn =< 16#FFFFFFFF ->
            Errors;
        GuestSvn when is_integer(GuestSvn) ->
            ErrorMsg = <<"Invalid guest_svn: expected unsigned 32-bit integer (0-4294967295), got ", 
                (hb_util:bin(integer_to_list(GuestSvn)))/binary, ".">>,
            [ErrorMsg | Errors];
        Invalid ->
            ErrorMsg = <<"Invalid guest_svn type: expected integer, got ", 
                (hb_util:bin(case Invalid of
                    B when is_binary(B) -> "binary";
                    L when is_list(L) -> "list";
                    _ -> "other"
                end))/binary, ".">>,
            [ErrorMsg | Errors]
    end.

%% Validate policy field
-spec validate_policy(ReportMap :: map(), Errors :: [binary()]) -> [binary()].
validate_policy(ReportMap, Errors) ->
    case maps:get(<<"policy">>, ReportMap, undefined) of
        undefined -> Errors;
        Policy when is_integer(Policy), Policy >= 0, Policy =< 16#FFFFFFFFFFFFFFFF ->
            Errors;
        Policy when is_integer(Policy) ->
            ErrorMsg = <<"Invalid policy: expected unsigned 64-bit integer (0-18446744073709551615), got ", 
                (hb_util:bin(integer_to_list(Policy)))/binary, ".">>,
            [ErrorMsg | Errors];
        Invalid ->
            ErrorMsg = <<"Invalid policy type: expected integer, got ", 
                (hb_util:bin(case Invalid of
                    B when is_binary(B) -> "binary";
                    L when is_list(L) -> "list";
                    _ -> "other"
                end))/binary, ".">>,
            [ErrorMsg | Errors]
    end.

%% Validate VMPL field (0-3)
-spec validate_vmpl(ReportMap :: map(), Errors :: [binary()]) -> [binary()].
validate_vmpl(ReportMap, Errors) ->
    case maps:get(<<"vmpl">>, ReportMap, undefined) of
        undefined -> Errors;
        Vmpl when is_integer(Vmpl), Vmpl >= 0, Vmpl =< 3 ->
            Errors;
        Vmpl when is_integer(Vmpl) ->
            ErrorMsg = <<"Invalid vmpl: expected integer in range 0-3, got ", 
                (hb_util:bin(integer_to_list(Vmpl)))/binary, 
                ". VMPL (Virtual Machine Privilege Level) must be between 0 and 3.">>,
            [ErrorMsg | Errors];
        Invalid ->
            ErrorMsg = <<"Invalid vmpl type: expected integer, got ", 
                (hb_util:bin(case Invalid of
                    B when is_binary(B) -> "binary";
                    L when is_list(L) -> "list";
                    _ -> "other"
                end))/binary, ".">>,
            [ErrorMsg | Errors]
    end.

%% Validate signature algorithm field
-spec validate_sig_algo(ReportMap :: map(), Errors :: [binary()]) -> [binary()].
validate_sig_algo(ReportMap, Errors) ->
    case maps:get(<<"sig_algo">>, ReportMap, undefined) of
        undefined -> Errors;
        SigAlgo when is_integer(SigAlgo), SigAlgo =:= 1 ->
            Errors;  % ECDSA-P384_SHA384 = 1
        SigAlgo when is_integer(SigAlgo) ->
            ErrorMsg = <<"Invalid sig_algo: expected 1 (ECDSA-P384_SHA384), got ", 
                (hb_util:bin(integer_to_list(SigAlgo)))/binary, ".">>,
            [ErrorMsg | Errors];
        Invalid ->
            ErrorMsg = <<"Invalid sig_algo type: expected integer, got ", 
                (hb_util:bin(case Invalid of
                    B when is_binary(B) -> "binary";
                    L when is_list(L) -> "list";
                    _ -> "other"
                end))/binary, ".">>,
            [ErrorMsg | Errors]
    end.

%% Validate TCB fields (SPL values must be 0-255)
-spec validate_tcb_fields(ReportMap :: map(), Errors :: [binary()]) -> [binary()].
validate_tcb_fields(ReportMap, Errors) ->
    TcbFields = [
        {<<"current_tcb">>, <<"current_tcb">>},
        {<<"reported_tcb">>, <<"reported_tcb">>},
        {<<"committed_tcb">>, <<"committed_tcb">>},
        {<<"launch_tcb">>, <<"launch_tcb">>}
    ],
    lists:foldl(
        fun({FieldName, FieldLabel}, AccErrors) ->
            case maps:get(FieldName, ReportMap, undefined) of
                undefined -> AccErrors;
                TcbMap when is_map(TcbMap) ->
                    validate_tcb_map(TcbMap, FieldLabel, AccErrors);
                Invalid ->
                    ErrorMsg = <<"Invalid ", FieldLabel/binary, " type: expected map, got ", 
                        (hb_util:bin(case Invalid of
                            B when is_binary(B) -> "binary";
                            L when is_list(L) -> "list";
                            _ -> "other"
                        end))/binary, ".">>,
                    [ErrorMsg | AccErrors]
            end
        end,
        Errors,
        TcbFields
    ).

%% Validate a single TCB map
-spec validate_tcb_map(TCBMap :: map(), FieldLabel :: binary(), Errors :: [binary()]) -> [binary()].
validate_tcb_map(TCBMap, FieldLabel, Errors) ->
    SPLFields = [
        {<<"bootloader">>, <<"bootloader">>},
        {<<"tee">>, <<"tee">>},
        {<<"snp">>, <<"snp">>},
        {<<"microcode">>, <<"microcode">>}
    ],
    lists:foldl(
        fun({FieldName, SPLName}, AccErrors) ->
            case maps:get(FieldName, TCBMap, undefined) of
                undefined ->
                    ErrorMsg = <<"Missing ", FieldLabel/binary, ".", SPLName/binary, 
                        ": required SPL field must be present.">>,
                    [ErrorMsg | AccErrors];
                SPLValue when is_integer(SPLValue), SPLValue >= 0, SPLValue =< ?MAX_SPL_VALUE ->
                    AccErrors;
                SPLValue when is_integer(SPLValue) ->
                    ErrorMsg = <<"Invalid ", FieldLabel/binary, ".", SPLName/binary, 
                        ": expected integer in range 0-", (hb_util:bin(integer_to_list(?MAX_SPL_VALUE)))/binary, 
                        ", got ", (hb_util:bin(integer_to_list(SPLValue)))/binary, ".">>,
                    [ErrorMsg | AccErrors];
                Invalid ->
                    ErrorMsg = <<"Invalid ", FieldLabel/binary, ".", SPLName/binary, 
                        " type: expected integer, got ", 
                        (hb_util:bin(case Invalid of
                            B when is_binary(B) -> "binary";
                            L when is_list(L) -> "list";
                            _ -> "other"
                        end))/binary, ".">>,
                    [ErrorMsg | AccErrors]
            end
        end,
        Errors,
        SPLFields
    ).

%% Validate version numbers (current/committed build/minor/major)
-spec validate_version_numbers(ReportMap :: map(), Errors :: [binary()]) -> [binary()].
validate_version_numbers(ReportMap, Errors) ->
    VersionFields = [
        {<<"current_build">>, <<"current_build">>},
        {<<"current_minor">>, <<"current_minor">>},
        {<<"current_major">>, <<"current_major">>},
        {<<"committed_build">>, <<"committed_build">>},
        {<<"committed_minor">>, <<"committed_minor">>},
        {<<"committed_major">>, <<"committed_major">>}
    ],
    lists:foldl(
        fun({FieldName, FieldLabel}, AccErrors) ->
            case maps:get(FieldName, ReportMap, undefined) of
                undefined -> AccErrors;
                Version when is_integer(Version), Version >= 0, Version =< 255 ->
                    AccErrors;
                Version when is_integer(Version) ->
                    ErrorMsg = <<"Invalid ", FieldLabel/binary, 
                        ": expected unsigned 8-bit integer (0-255), got ", 
                        (hb_util:bin(integer_to_list(Version)))/binary, ".">>,
                    [ErrorMsg | AccErrors];
                Invalid ->
                    ErrorMsg = <<"Invalid ", FieldLabel/binary, " type: expected integer, got ", 
                        (hb_util:bin(case Invalid of
                            B when is_binary(B) -> "binary";
                            L when is_list(L) -> "list";
                            _ -> "other"
                        end))/binary, ".">>,
                    [ErrorMsg | AccErrors]
            end
        end,
        Errors,
        VersionFields
    ).

%% Validate binary field sizes
-spec validate_binary_fields(ReportMap :: map(), Errors :: [binary()]) -> [binary()].
validate_binary_fields(ReportMap, Errors) ->
    BinaryFields = [
        {<<"family_id">>, ?FAMILY_ID_SIZE, <<"family_id">>},
        {<<"image_id">>, ?IMAGE_ID_SIZE, <<"image_id">>},
        {<<"report_data">>, ?CHIP_ID_SIZE, <<"report_data">>},
        {<<"measurement">>, ?LAUNCH_DIGEST_SIZE, <<"measurement">>},
        {<<"host_data">>, ?HOST_DATA_SIZE, <<"host_data">>},
        {<<"id_key_digest">>, ?LAUNCH_DIGEST_SIZE, <<"id_key_digest">>},
        {<<"author_key_digest">>, ?LAUNCH_DIGEST_SIZE, <<"author_key_digest">>},
        {<<"report_id">>, ?REPORT_ID_SIZE, <<"report_id">>},
        {<<"report_id_ma">>, ?REPORT_ID_SIZE, <<"report_id_ma">>},
        {<<"chip_id">>, ?CHIP_ID_SIZE, <<"chip_id">>}
    ],
    lists:foldl(
        fun({FieldName, ExpectedSize, FieldLabel}, AccErrors) ->
            case maps:get(FieldName, ReportMap, undefined) of
                undefined -> AccErrors;
                FieldValue when is_binary(FieldValue) ->
                    FieldSize = byte_size(FieldValue),
                    if
                        FieldSize =:= ExpectedSize -> AccErrors;
                        true ->
                            ErrorMsg = <<"Invalid ", FieldLabel/binary, " size: expected ", 
                                (hb_util:bin(integer_to_list(ExpectedSize)))/binary, 
                                " bytes, got ", (hb_util:bin(integer_to_list(FieldSize)))/binary, ".">>,
                            [ErrorMsg | AccErrors]
                    end;
                FieldValue when is_list(FieldValue) ->
                    % Convert list to binary to check size
                    FieldBinary = hb_util:bin(FieldValue),
                    FieldBinarySize = byte_size(FieldBinary),
                    if
                        FieldBinarySize =:= ExpectedSize -> AccErrors;
                        true ->
                            ErrorMsg = <<"Invalid ", FieldLabel/binary, " size: expected ", 
                                (hb_util:bin(integer_to_list(ExpectedSize)))/binary, 
                                " bytes, got ", (hb_util:bin(integer_to_list(FieldBinarySize)))/binary, 
                                " (after converting from list).">>,
                            [ErrorMsg | AccErrors]
                    end;
                Invalid ->
                    InvalidType = case Invalid of
                        I when is_integer(I) -> "integer";
                        M when is_map(M) -> "map";
                        _ -> "other"
                    end,
                    ErrorMsg = <<"Invalid ", FieldLabel/binary, " type: expected binary or list, got ", 
                        (hb_util:bin(InvalidType))/binary, ".">>,
                    [ErrorMsg | AccErrors]
            end
        end,
        Errors,
        BinaryFields
    ).

%% Validate signature field
-spec validate_signature(ReportMap :: map(), Errors :: [binary()]) -> [binary()].
validate_signature(ReportMap, Errors) ->
    case maps:get(<<"signature">>, ReportMap, undefined) of
        undefined -> Errors;
        SignatureMap when is_map(SignatureMap) ->
            Errors1 = case maps:get(<<"r">>, SignatureMap, undefined) of
                undefined ->
                    [<<"Missing signature.r: required signature component must be present.">> | Errors];
                SignatureR when is_binary(SignatureR) ->
                    case byte_size(SignatureR) of
                        ?SIGNATURE_R_SIZE -> Errors;
                        ActualSize ->
                            ErrorMsg = <<"Invalid signature.r size: expected ", 
                                (hb_util:bin(integer_to_list(?SIGNATURE_R_SIZE)))/binary, 
                                " bytes, got ", (hb_util:bin(integer_to_list(ActualSize)))/binary, ".">>,
                            [ErrorMsg | Errors]
                    end;
                SignatureR when is_list(SignatureR) ->
                    SignatureRBin = hb_util:bin(SignatureR),
                    case byte_size(SignatureRBin) of
                        ?SIGNATURE_R_SIZE -> Errors;
                        ActualSize ->
                            ErrorMsg = <<"Invalid signature.r size: expected ", 
                                (hb_util:bin(integer_to_list(?SIGNATURE_R_SIZE)))/binary, 
                                " bytes, got ", (hb_util:bin(integer_to_list(ActualSize)))/binary, 
                                " (after converting from list).">>,
                            [ErrorMsg | Errors]
                    end;
                Invalid ->
                    ErrorMsg = <<"Invalid signature.r type: expected binary or list, got ", 
                        (hb_util:bin(case Invalid of
                            I when is_integer(I) -> "integer";
                            M when is_map(M) -> "map";
                            _ -> "other"
                        end))/binary, ".">>,
                    [ErrorMsg | Errors]
            end,
            Errors2 = case maps:get(<<"s">>, SignatureMap, undefined) of
                undefined ->
                    [<<"Missing signature.s: required signature component must be present.">> | Errors1];
                SignatureS when is_binary(SignatureS) ->
                    case byte_size(SignatureS) of
                        ?SIGNATURE_S_SIZE -> Errors1;
                        ActualSizeS ->
                            ErrorMsgS = <<"Invalid signature.s size: expected ", 
                                (hb_util:bin(integer_to_list(?SIGNATURE_S_SIZE)))/binary, 
                                " bytes, got ", (hb_util:bin(integer_to_list(ActualSizeS)))/binary, ".">>,
                            [ErrorMsgS | Errors1]
                    end;
                SignatureS when is_list(SignatureS) ->
                    SignatureSBin = hb_util:bin(SignatureS),
                    case byte_size(SignatureSBin) of
                        ?SIGNATURE_S_SIZE -> Errors1;
                        ActualSizeSList ->
                            ErrorMsgSList = <<"Invalid signature.s size: expected ", 
                                (hb_util:bin(integer_to_list(?SIGNATURE_S_SIZE)))/binary, 
                                " bytes, got ", (hb_util:bin(integer_to_list(ActualSizeSList)))/binary, 
                                " (after converting from list).">>,
                            [ErrorMsgSList | Errors1]
                    end;
                InvalidS ->
                    ErrorMsgS = <<"Invalid signature.s type: expected binary or list, got ", 
                        (hb_util:bin(case InvalidS of
                            IS when is_integer(IS) -> "integer";
                            MS when is_map(MS) -> "map";
                            _ -> "other"
                        end))/binary, ".">>,
                    [ErrorMsgS | Errors1]
            end,
            Errors2;
        Invalid ->
            ErrorMsg = <<"Invalid signature type: expected map, got ", 
                (hb_util:bin(case Invalid of
                    B when is_binary(B) -> "binary";
                    L when is_list(L) -> "list";
                    I when is_integer(I) -> "integer";
                    _ -> "other"
                end))/binary, ".">>,
            [ErrorMsg | Errors]
    end.

