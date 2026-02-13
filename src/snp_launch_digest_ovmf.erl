%%% @doc OVMF parsing and metadata processing for SNP commitment reports.
%%%
%%% This module handles parsing of OVMF firmware files, extracting metadata
%%% sections, and updating the launch digest context with OVMF-related data.
-module(snp_launch_digest_ovmf).
-export([parse_and_update_ovmf_metadata_erlang/6]).
-include("include/hb.hrl").
-include("include/snp_constants.hrl").
-include("include/snp_launch_digest.hrl").
-include("include/snp_guids.hrl").

%% @doc Parse and update OVMF metadata
%% @param GCTX #gctx{} record with current launch digest
%% @param VMMType integer() - VMM type (1=QEMU, 2=EC2)
%% @param KernelHash undefined | binary() - Kernel hash (optional)
%% @param InitrdHash undefined | binary() - Initrd hash (optional)
%% @param AppendHash undefined | binary() - Append hash (optional)
%% @param SevHashesGPA non_neg_integer() - SEV hashes table GPA
%% @returns {#gctx{}, ResetEIP} where ResetEIP is the reset EIP value from OVMF
-spec parse_and_update_ovmf_metadata_erlang(GCTX :: #gctx{}, VMMType :: integer(), 
    KernelHash :: undefined | binary(), InitrdHash :: undefined | binary(), 
    AppendHash :: undefined | binary(), SevHashesGPA :: non_neg_integer()) -> 
    {#gctx{}, non_neg_integer()}.
parse_and_update_ovmf_metadata_erlang(GCTX, VMMType, KernelHash, InitrdHash, AppendHash, SevHashesGPA) ->
    ?event(snp, {parse_and_update_ovmf_metadata_start, #{
        vmm_type => VMMType,
        sev_hashes_gpa => SevHashesGPA,
        has_kernel => is_binary(KernelHash),
        has_initrd => is_binary(InitrdHash),
        has_append => is_binary(AppendHash)
    }}),
    % Try to find OVMF file
    CwdPath = case file:get_cwd() of
        {ok, Cwd} -> filename:join([Cwd, "test", "OVMF-1.55.fd"]);
        {error, _} -> filename:join(["test", "OVMF-1.55.fd"])
    end,
    OvmfPaths = [
        CwdPath,
        "/root/hb-release/test/OVMF-1.55.fd"
    ],
    ?event(snp, {ovmf_paths_to_try, OvmfPaths}),
    
    case find_ovmf_file(OvmfPaths) of
        {ok, OvmfPath} ->
            ?event(snp_short, {ovmf_file_found, #{path => OvmfPath}}),
            parse_ovmf_and_update(GCTX, OvmfPath, VMMType, KernelHash, InitrdHash, AppendHash, SevHashesGPA);
        {error, Reason} ->
            ?event(snp_error, {ovmf_file_not_found, #{reason => Reason}}),
            % Fallback: use default reset EIP (0x0) if OVMF not found, matching Rust
            DefaultResetEIP = ?DEFAULT_RESET_EIP,
            ?event(snp, {using_default_reset_eip, #{reset_eip => DefaultResetEIP}}),
            % If OVMF parsing failed but we have SEV hashes GPA, try to update just the hashes table
            GCTX1 = case {KernelHash, InitrdHash, AppendHash, SevHashesGPA} of
                {K, I, A, GPA} when is_binary(K), is_binary(I), is_binary(A), GPA =/= 0 ->
                    ?event(snp, {updating_sev_hashes_table_fallback, #{gpa => GPA}}),
                    case snp_launch_digest_sev_hashes:update_sev_hashes_table(GCTX, K, I, A, GPA) of
                        {ok, G} -> G;
                        {error, invalid_hex} -> erlang:error(invalid_hex)
                    end;
                _ ->
                    ?event(snp, no_sev_hashes_update_possible),
                    GCTX
            end,
            {GCTX1, DefaultResetEIP}
    end.

%% Find OVMF file in list of paths
-spec find_ovmf_file([string()]) -> {ok, string()} | {error, term()}.
find_ovmf_file([]) -> 
    ?event(snp, ovmf_file_search_exhausted),
    {error, not_found};
find_ovmf_file([Path | Rest]) ->
    ?event(snp, {trying_ovmf_path, #{path => Path}}),
    case file:read_file_info(Path) of
        {ok, FileInfo} -> 
            FileSize = case is_tuple(FileInfo) andalso tuple_size(FileInfo) >= 2 of
                true -> element(2, FileInfo);
                false -> 0
            end,
            ?event(snp_short, {ovmf_file_found_at_path, #{path => Path, size => FileSize}}),
            {ok, Path};
        {error, Reason} -> 
            ?event(snp, {ovmf_path_failed, #{path => Path, reason => Reason}}),
            find_ovmf_file(Rest)
    end.

%% Parse OVMF and update GCTX with all metadata sections
%% Returns {GCTX, ResetEIP} where ResetEIP is read from OVMF footer table (matching Rust)
-spec parse_ovmf_and_update(GCTX :: #gctx{}, OvmfPath :: string(), VMMType :: integer(),
    KernelHash :: undefined | binary(), InitrdHash :: undefined | binary(),
    AppendHash :: undefined | binary(), SevHashesGPA :: non_neg_integer()) ->
    {#gctx{}, non_neg_integer()}.
parse_ovmf_and_update(GCTX, OvmfPath, VMMType, KernelHash, InitrdHash, AppendHash, SevHashesGPA) ->
    ?event(snp, {parse_ovmf_and_update_start, #{
        path => OvmfPath,
        vmm_type => VMMType,
        has_kernel => is_binary(KernelHash),
        has_initrd => is_binary(InitrdHash),
        has_append => is_binary(AppendHash),
        sev_hashes_gpa_arg => SevHashesGPA
    }}),
    % Get SEV hashes table GPA from footer table if not provided (matches Rust ovmf.sev_hashes_table_gpa())
    FinalSevHashesGPA = case SevHashesGPA of
        0 -> 
            case snp_ovmf:parse_ovmf_sev_hashes_gpa(OvmfPath) of
                {ok, GPA} -> 
                    ?event(snp_short, {sev_hashes_gpa_from_footer_table, #{gpa => GPA}}),
                    GPA;
                _ -> 
                    ?event(snp, sev_hashes_gpa_not_found_in_footer_table),
                    0
            end;
        _ -> SevHashesGPA
    end,
    case file:read_file(OvmfPath) of
        {ok, OvmfData} ->
            % If GCTX was initialized with zeros (no firmware hash provided),
            % update it with full OVMF data first (matching Rust behavior)
            % Rust: gctx.update_page(PageType::Normal, ovmf.gpa(), Some(ovmf.data()), None)?
            OvmfSize = byte_size(OvmfData),
            OvmfGPA = ?FOUR_GB - OvmfSize,
            GCTX1 = case GCTX#gctx.ld of
                <<0:?LAUNCH_DIGEST_BITS>> ->  % If LD is all zeros, we need to update with OVMF data
                    ?event(snp, {updating_gctx_with_ovmf_data, #{
                        ovmf_size => OvmfSize,
                        ovmf_gpa => OvmfGPA,
                        ovmf_gpa_hex => integer_to_list(OvmfGPA, 16),
                        ld_before_hex => snp_util:binary_to_hex_string(GCTX#gctx.ld)
                    }}),
                    % Update GCTX with full OVMF data as Normal page
                    % This processes the OVMF in ?PAGE_SIZE chunks, hashing each page
                    UpdatedGCTX = snp_launch_digest_gctx:gctx_update_page(GCTX, ?PAGE_TYPE_NORMAL, OvmfGPA, OvmfData),
                    ?event(snp, {ovmf_data_update_complete, #{
                        ld_after_hex => snp_util:binary_to_hex_string(UpdatedGCTX#gctx.ld)
                    }}),
                    UpdatedGCTX;
                _ -> 
                    ?event(snp, {gctx_already_initialized_with_hash, #{
                        ld_hex => snp_util:binary_to_hex_string(GCTX#gctx.ld)
                    }}),
                    GCTX  % Already initialized with firmware hash, skip OVMF update
            end,
            ?event(snp, {after_ovmf_data_update, #{
                ld_hex => snp_util:binary_to_hex_string(GCTX1#gctx.ld)
            }}),
            % Read reset EIP from OVMF footer table (matching Rust ovmf.sev_es_reset_eip())
            ResetEIP = case snp_ovmf:parse_ovmf_reset_eip(OvmfPath) of
                {ok, EIP} -> 
                    ?event(snp_short, {reset_eip_from_ovmf, #{eip => EIP}}),
                    EIP;
                {error, Reason} -> 
                    ?event(snp, {reset_eip_not_found_using_default, #{default => ?DEFAULT_RESET_EIP, reason => Reason}}),
                    ?DEFAULT_RESET_EIP  % Default to 0 if not found (Rust would error, but we continue)
            end,
            case parse_ovmf_metadata_sections(OvmfData) of
                {ok, Sections} ->
                    ?event(snp_short, {ovmf_metadata_sections_parsed, #{count => length(Sections)}}),
                    % Process all sections (starting from GCTX1 which may have been updated with OVMF data)
                    GCTX2 = lists:foldl(
                        fun(Section, AccGCTX) ->
                            SectionNum = maps:get(section_type, Section),
                            SectionGPA = maps:get(gpa, Section),
                            SectionSize = maps:get(size, Section),
                            LD_BeforeSection = snp_util:binary_to_hex_string(AccGCTX#gctx.ld),
                            ?event(snp, {metadata_section_before, #{
                                section_type => SectionNum,
                                section_gpa => SectionGPA,
                                section_size => SectionSize,
                                ld_before_hex => LD_BeforeSection
                            }}),
                            ?event(snp, {processing_metadata_section, #{
                                section_type => SectionNum,
                                gpa => SectionGPA,
                                size => SectionSize,
                                ld_before_hex => LD_BeforeSection
                            }}),
                            NewGCTX = process_ovmf_section(AccGCTX, Section, VMMType, KernelHash, InitrdHash, AppendHash, OvmfData, FinalSevHashesGPA),
                            LD_AfterSection = snp_util:binary_to_hex_string(NewGCTX#gctx.ld),
                            ?event(snp, {metadata_section_after, #{
                                section_type => SectionNum,
                                section_gpa => SectionGPA,
                                section_size => SectionSize,
                                ld_before_hex => LD_BeforeSection,
                                ld_after_hex => LD_AfterSection
                            }}),
                            ?event(snp, {metadata_section_processed, #{
                                section_type => SectionNum,
                                section_gpa => SectionGPA,
                                section_size => SectionSize,
                                ld_before_hex => LD_BeforeSection,
                                ld_after_hex => LD_AfterSection
                            }}),
                            NewGCTX
                        end,
                        GCTX,
                        Sections
                    ),
                    % Special handling for EC2 VMM type: process CPUID sections again
                    GCTX3 = case VMMType of
                        ?VMM_TYPE_EC2 -> % EC2
                            ?event(snp, {processing_cpuid_sections_for_ec2, #{
                                ld_before_hex => snp_util:binary_to_hex_string(GCTX2#gctx.ld)
                            }}),
                            Result = lists:foldl(
                                fun(Section, AccGCTX) ->
                                    case Section of
                                        #{section_type := ?OVMF_SECTION_CPUID} -> % Cpuid
                                            SectionGPA = maps:get(gpa, Section),
                                            ?event(snp, {processing_cpuid_section_ec2, #{
                                                gpa => SectionGPA,
                                                ld_before_hex => snp_util:binary_to_hex_string(AccGCTX#gctx.ld)
                                            }}),
                                            NewGCTX = snp_launch_digest_gctx:gctx_update_page(AccGCTX, ?PAGE_TYPE_CPUID, SectionGPA, undefined),
                                            ?event(snp, {cpuid_section_ec2_processed, #{
                                                gpa => SectionGPA,
                                                ld_after_hex => snp_util:binary_to_hex_string(NewGCTX#gctx.ld)
                                            }}),
                                            NewGCTX;
                                        _ -> AccGCTX
                                    end
                                end,
                                GCTX2,
                                Sections
                            ),
                            ?event(snp_short, {cpuid_sections_ec2_complete, #{
                                ld_hex => snp_util:binary_to_hex_string(Result#gctx.ld)
                            }}),
                            Result;
                        _ -> GCTX2
                    end,
                    % Verify SEV hashes section exists if we have hashes
                    case {KernelHash, InitrdHash, AppendHash} of
                        {K, I, A} when is_binary(K), is_binary(I), is_binary(A) ->
                            HasSevHashes = lists:any(
                                fun(S) -> maps:get(section_type, S) =:= ?OVMF_SECTION_SNP_KERNEL_HASHES end, % SnpKernelHashes = 0x10
                                Sections
                            ),
                            case HasSevHashes of
                                true -> {GCTX3, ResetEIP};
                                false -> 
                                    ?event(snp, missing_snp_kernel_hashes_section),
                                    {GCTX3, ResetEIP}  % Continue anyway, but log the issue
                            end;
                        _ -> {GCTX3, ResetEIP}
                    end;
                {error, MetadataReason} ->
                    ?event(snp_error, {ovmf_metadata_parse_failed, #{reason => MetadataReason}}),
                    % Fallback: try to use SEV hashes GPA if available
                    GCTX1 = case {KernelHash, InitrdHash, AppendHash} of
                        {K, I, A} when is_binary(K), is_binary(I), is_binary(A) ->
                            case snp_ovmf:parse_ovmf_sev_hashes_gpa(OvmfPath) of
                                {ok, FallbackGPA} ->
                                    ?event(snp, {fallback_to_sev_hashes_gpa, #{gpa => FallbackGPA}}),
                                    case snp_launch_digest_sev_hashes:update_sev_hashes_table(GCTX, K, I, A, FallbackGPA) of
                                        {ok, G} -> G;
                                        {error, invalid_hex} -> erlang:error(invalid_hex)
                                    end;
                                _ -> GCTX
                            end;
                        _ -> GCTX
                    end,
                    {GCTX1, ResetEIP}
            end;
        {error, Reason} ->
            ?event(snp_error, {ovmf_file_read_failed, #{reason => Reason}}),
            {GCTX, ?DEFAULT_RESET_EIP}  % Return default reset EIP if file read fails
    end.

%% Parse OVMF metadata sections from OVMF data
-spec parse_ovmf_metadata_sections(binary()) -> {ok, [map()]} | {error, term()}.
parse_ovmf_metadata_sections(OvmfData) ->
    % OVMF_SEV_METADATA_GUID: dc886566-984a-4798-a75e-5585a7bf67cc
    % UUID to_bytes_le() converts to: 666588dc4a989847a75e5585a7bf67cc
    % Which is: [102, 101, 136, 220, 74, 152, 152, 71, 167, 94, 85, 133, 167, 191, 103, 204]
    OvmfSevMetadataGuid = <<102, 101, 136, 220, 74, 152, 152, 71, 167, 94, 85, 133, 167, 191, 103, 204>>,
    
    % First, parse footer table to find the metadata GUID entry
    case parse_ovmf_footer_table_for_guid(OvmfData, OvmfSevMetadataGuid) of
        {ok, MetadataEntry} ->
            % Metadata entry contains offset_from_end (i32, little-endian)
            <<OffsetFromEnd:32/little-signed>> = binary:part(MetadataEntry, 0, 4),
            DataSize = byte_size(OvmfData),
            HeaderStart = DataSize - OffsetFromEnd,
            
            % Parse metadata header: signature (4 bytes) + size (u32) + version (u32) + num_items (u32)
            % Signature should be "ASEV"
            ExpectedSignature = <<"ASEV">>,
            case binary:part(OvmfData, HeaderStart, 4) of
                ExpectedSignature ->
                    <<_:4/binary, HeaderSize:32/little, Version:32/little, NumItems:32/little>> = 
                        binary:part(OvmfData, HeaderStart, 16),
                    
                    if
                        Version =/= ?OVMF_METADATA_VERSION -> {error, {invalid_metadata_version, Version}};
                        HeaderSize < ?OVMF_METADATA_HEADER_SIZE -> {error, {invalid_header_size, HeaderSize}};
                        true ->
                            % Parse section descriptors
                            ItemsStart = HeaderStart + ?OVMF_METADATA_HEADER_SIZE,
                            ItemsSize = HeaderSize - ?OVMF_METADATA_HEADER_SIZE,
                            parse_metadata_section_descriptors(OvmfData, ItemsStart, ItemsSize, NumItems, [])
                    end;
                OtherSignature ->
                    {error, {invalid_signature, OtherSignature}}
            end;
        {error, Reason} ->
            {error, {metadata_guid_not_found, Reason}}
    end.

%% Parse metadata section descriptors
-spec parse_metadata_section_descriptors(binary(), integer(), integer(), integer(), [map()]) -> 
    {ok, [map()]} | {error, term()}.
parse_metadata_section_descriptors(_OvmfData, _ItemsStart, _ItemsSize, 0, Acc) ->
    {ok, lists:reverse(Acc)};
parse_metadata_section_descriptors(OvmfData, ItemsStart, ItemsSize, NumItems, Acc) when NumItems > 0 ->
    % OvmfSevMetadataSectionDesc: GPA (u32, 4 bytes) + Size (u32, 4 bytes) + SectionType (u8, 1 byte) + padding (3 bytes) = 12 bytes
    % With #[repr(C)], the struct is padded to 12 bytes for alignment
    DescriptorSize = ?OVMF_DESCRIPTOR_SIZE,
    Index = length(Acc),
    Offset = ItemsStart + (Index * DescriptorSize),
    
    if
        Offset + DescriptorSize > byte_size(OvmfData) ->
            {error, {descriptor_out_of_bounds, Index}};
        true ->
            <<GPA:32/little, Size:32/little, SectionType:8, _Padding:24>> = 
                binary:part(OvmfData, Offset, DescriptorSize),
            
            Section = #{
                gpa => GPA,
                size => Size,
                section_type => SectionType
            },
            ?event(snp, {parsed_metadata_section, Section}),
            parse_metadata_section_descriptors(OvmfData, ItemsStart, ItemsSize, NumItems - 1, [Section | Acc])
    end.

%% Process a single OVMF metadata section
-spec process_ovmf_section(GCTX :: #gctx{}, Section :: map(), VMMType :: integer(),
    KernelHash :: undefined | binary(), InitrdHash :: undefined | binary(),
    AppendHash :: undefined | binary(), OvmfData :: binary(), SevHashesTableGPA :: non_neg_integer()) ->
    #gctx{}.
process_ovmf_section(GCTX, Section, VMMType, KernelHash, InitrdHash, AppendHash, _OvmfData, SevHashesTableGPA) ->
    SectionType = maps:get(section_type, Section),
    GPA = maps:get(gpa, Section),
    Size = maps:get(size, Section),
    
    LD_Before = snp_util:binary_to_hex_string(GCTX#gctx.ld),
    ?event(snp, {processing_section_start, #{
        section_type => SectionType,
        gpa => GPA,
        size => Size,
        ld_before_hex => LD_Before
    }}),
    
    Result = case SectionType of
        ?OVMF_SECTION_SNP_SEC_MEMORY -> % SnpSecMemory
            ?event(snp, {processing_section_snp_sec_memory, #{gpa => GPA, size => Size}}),
            % Process as zero pages (multiple ?PAGE_SIZE pages)
            process_zero_pages(GCTX, GPA, Size);
        ?OVMF_SECTION_SNP_SECRETS -> % SnpSecrets
            ?event(snp, {processing_section_snp_secrets, #{gpa => GPA}}),
            snp_launch_digest_gctx:gctx_update_page(GCTX, ?PAGE_TYPE_SECRETS, GPA, undefined);
        ?OVMF_SECTION_CPUID -> % Cpuid
            if
                VMMType =/= ?VMM_TYPE_EC2 -> % Not EC2
                    ?event(snp, {processing_section_cpuid, #{gpa => GPA}}),
                    snp_launch_digest_gctx:gctx_update_page(GCTX, ?PAGE_TYPE_CPUID, GPA, undefined);
                true ->
                    ?event(snp, {skipping_cpuid_section_for_ec2, #{gpa => GPA}}),
                    GCTX
            end;
        ?OVMF_SECTION_SNP_KERNEL_HASHES -> % SnpKernelHashes (0x10)
            case {KernelHash, InitrdHash, AppendHash} of
                {K, I, A} when is_binary(K), is_binary(I), is_binary(A) ->
                    ?event(snp_short, {processing_section_snp_kernel_hashes, #{
                        section_gpa => GPA, 
                        size => Size,
                        sev_hashes_table_gpa => SevHashesTableGPA
                    }}),
                    % Use footer table GPA for page offset (matches Rust: sev_hashes_table_gpa & _PAGE_MASK)
                    % But use section GPA directly for update_page call (matches Rust: gpa parameter)
                    PageOffset = case SevHashesTableGPA of
                        0 -> GPA band ?PAGE_MASK;  % Fallback to section GPA if footer table GPA not available
                        _ -> SevHashesTableGPA band ?PAGE_MASK
                    end,
                    % Use section GPA directly (not page-aligned) to match Rust implementation
                    ?event(snp, {sev_hashes_page_offset_calc, #{
                        page_offset => PageOffset,
                        section_gpa => GPA,
                        using_footer_table_gpa => SevHashesTableGPA =/= 0
                    }}),
                    case snp_launch_digest_sev_hashes:construct_sev_hashes_page_erlang(K, I, A, PageOffset) of
                        {ok, SevHashesPage} ->
                    SevHashesPageHex = snp_util:binary_to_hex_string(SevHashesPage),
                    SevHashesPageHash = crypto:hash(sha384, SevHashesPage),
                    SevHashesPageHashHex = snp_util:binary_to_hex_string(SevHashesPageHash),
                    ?event(snp, {sev_hashes_page_ready, #{
                        page_offset => PageOffset,
                        page_size => byte_size(SevHashesPage),
                        page_hex => SevHashesPageHex,
                        page_sha384 => SevHashesPageHashHex
                    }}),
                    snp_launch_digest_gctx:gctx_update_page(GCTX, ?PAGE_TYPE_NORMAL, GPA, SevHashesPage); % use GPA directly
                        {error, invalid_hex} ->
                            erlang:error(invalid_hex)
                    end;
                _ ->
                    ?event(snp, {skipping_snp_kernel_hashes_no_hashes, #{gpa => GPA}}),
                    % Process as zero pages if no hashes provided
                    process_zero_pages(GCTX, GPA, Size)
            end;
        ?OVMF_SECTION_SVSM_CAA -> % SvsmCaa
            ?event(snp, {processing_section_svsm_caa, #{gpa => GPA, size => Size}}),
            process_zero_pages(GCTX, GPA, Size);
        _ ->
            ?event(snp_error, {unknown_section_type, #{type => SectionType, gpa => GPA}}),
            GCTX
    end,
    LD_After = snp_util:binary_to_hex_string(Result#gctx.ld),
    ?event(snp, {processing_section_complete, #{
        section_type => SectionType,
        gpa => GPA,
        ld_before_hex => LD_Before,
        ld_after_hex => LD_After
    }}),
    Result.

%% Process zero pages (multiple 4KB pages)
-spec process_zero_pages(GCTX :: #gctx{}, GPA :: non_neg_integer(), Size :: non_neg_integer()) -> #gctx{}.
process_zero_pages(GCTX, _GPA, Size) when Size =< 0 ->
    GCTX;
process_zero_pages(GCTX, GPA, Size) ->
    % Process in ?PAGE_SIZE chunks
    Pages = Size div ?PAGE_SIZE,
    ?event(snp, {process_zero_pages_start, #{
        gpa => GPA,
        size => Size,
        pages => Pages
    }}),
    Result = lists:foldl(
        fun(PageNum, AccGCTX) ->
            PageGPA = GPA + (PageNum * ?PAGE_SIZE),
            ?event(snp, {processing_zero_page, #{
                page_num => PageNum,
                page_gpa => PageGPA,
                total_pages => Pages
            }}),
            snp_launch_digest_gctx:gctx_update_page(AccGCTX, ?PAGE_TYPE_ZERO, PageGPA, undefined)
        end,
        GCTX,
        lists:seq(0, Pages - 1)
    ),
    ?event(snp, {process_zero_pages_complete, #{
        pages_processed => Pages,
        final_ld_hex => snp_util:binary_to_hex_string(Result#gctx.ld)
    }}),
    Result.

%% Parse OVMF footer table to find a specific GUID entry
-spec parse_ovmf_footer_table_for_guid(binary(), binary()) -> {ok, binary()} | {error, term()}.
parse_ovmf_footer_table_for_guid(OvmfData, TargetGuid) ->
    DataSize = byte_size(OvmfData),
    ENTRY_HEADER_SIZE = ?OVMF_ENTRY_HEADER_SIZE,
    % Footer table ends ?OVMF_FOOTER_OFFSET bytes before end, last entry is at start_of_footer_table
    StartOfFooterTable = DataSize - ?OVMF_FOOTER_OFFSET - ENTRY_HEADER_SIZE,
    ?event(snp, {parsing_footer_table, #{
        data_size => DataSize,
        start_of_footer_table => StartOfFooterTable
    }}),
    
    % Read the footer entry
    FooterEntry = binary:part(OvmfData, StartOfFooterTable, ENTRY_HEADER_SIZE),
    <<FooterSize:16/little, FooterGuid:16/binary>> = FooterEntry,
    
    % OVMF_TABLE_FOOTER_GUID (from snp_guids.hrl)
    ExpectedFooterGuid = ?OVMF_TABLE_FOOTER_GUID,
    
    FooterGuidHex = hb_util:to_hex(FooterGuid),
    ExpectedGuidHex = hb_util:to_hex(ExpectedFooterGuid),
    ?event(snp, {footer_entry_read, #{
        footer_size => FooterSize,
        footer_guid_hex => FooterGuidHex,
        expected_guid_hex => ExpectedGuidHex,
        match => FooterGuid =:= ExpectedFooterGuid
    }}),
    
    if
        FooterGuid =/= ExpectedFooterGuid -> 
            ?event(snp_error, {footer_guid_mismatch, #{
                read => FooterGuidHex,
                expected => ExpectedGuidHex
            }}),
            {error, invalid_footer_guid};
        FooterSize < ENTRY_HEADER_SIZE -> {error, invalid_footer_size};
        true ->
            % Calculate table size and start
            TableSize = FooterSize - ENTRY_HEADER_SIZE,
            TableStart = StartOfFooterTable - TableSize,
            ?event(snp, {footer_table_calculated, #{
                table_size => TableSize,
                table_start => TableStart
            }}),
            
            if
                TableStart < 0 -> {error, invalid_table_offset};
                true ->
                    % Read the table and search backwards for the target GUID
                    TableData = binary:part(OvmfData, TableStart, TableSize),
                    TargetGuidHex = hb_util:to_hex(TargetGuid),
                    ?event(snp, {searching_for_guid_in_table, #{
                        target_guid_hex => TargetGuidHex,
                        table_size => TableSize
                    }}),
                    find_guid_in_table(TableData, TargetGuid, TableSize)
            end
    end.

%% Find a GUID entry in the footer table (searching backwards)
-spec find_guid_in_table(binary(), binary(), integer()) -> {ok, binary()} | {error, term()}.
find_guid_in_table(_TableData, _TargetGuid, Offset) when Offset < ?OVMF_ENTRY_HEADER_SIZE ->
    {error, guid_not_found};
find_guid_in_table(TableData, TargetGuid, Offset) ->
    ENTRY_HEADER_SIZE = ?OVMF_ENTRY_HEADER_SIZE,
    EntryHeaderOffset = Offset - ENTRY_HEADER_SIZE,
    <<EntrySize:16/little, EntryGuid:16/binary>> = 
        binary:part(TableData, EntryHeaderOffset, ENTRY_HEADER_SIZE),
    
    if
        EntrySize < ENTRY_HEADER_SIZE -> {error, invalid_entry_size};
        Offset < EntrySize -> {error, invalid_entry_offset};
        EntryGuid =:= TargetGuid ->
            % Found it! Entry data is before the header
            DataOffset = Offset - EntrySize,
            if
                DataOffset + ?OVMF_METADATA_OFFSET_SIZE > byte_size(TableData) -> {error, invalid_data_offset};
                true ->
                    % Return the entry data (first 4 bytes are the offset/data we need)
                    EntryData = binary:part(TableData, DataOffset, EntrySize - ENTRY_HEADER_SIZE),
                    {ok, EntryData}
            end;
        true ->
            find_guid_in_table(TableData, TargetGuid, Offset - EntrySize)
    end.

