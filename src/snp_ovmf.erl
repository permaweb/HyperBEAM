%%% @doc OVMF file parsing for SNP commitment reports.
%%%
%%% This module handles parsing of OVMF (Open Virtual Machine Firmware) files
%%% to extract SEV-related metadata, including SEV hashes table GPA and reset EIP.
-module(snp_ovmf).
-export([read_ovmf_gpa/0, parse_ovmf_sev_hashes_gpa/1, parse_ovmf_reset_eip/1]).
-include("include/hb.hrl").
-include("include/snp_constants.hrl").
-include("include/snp_guids.hrl").

%% @doc Read OVMF file and extract SEV hashes table GPA.
%% OVMF is copied to priv/ovmf/ at build time (rebar); same layout as snp_launch_digest_ovmf.
%% @returns {ok, GPA} or {error, Reason}
-spec read_ovmf_gpa() -> {ok, non_neg_integer()} | {error, term()}.
read_ovmf_gpa() ->
    {ok, Cwd} = file:get_cwd(),
    OvmfPaths = [
        % Canonical path: priv/ovmf/ (build-time copy)
        filename:join([code:priv_dir(hb), "ovmf", "OVMF-1.55.fd"]),
        % Fallback: repo root (dev, before compile)
        filename:join([Cwd, "OVMF-1.55.fd"])
    ],
    ?event(snp, {ovmf_search_paths, OvmfPaths}),
    read_ovmf_gpa(OvmfPaths).

%% Internal helper to try multiple OVMF paths
%% @param Paths [string()] - List of paths to try
%% @returns {ok, non_neg_integer()} or {error, ovmf_file_not_found}
-spec read_ovmf_gpa(Paths :: [string()]) -> {ok, non_neg_integer()} | {error, ovmf_file_not_found}.
read_ovmf_gpa([]) ->
    {error, ovmf_file_not_found};
read_ovmf_gpa([Path | Rest]) ->
    case parse_ovmf_sev_hashes_gpa(Path) of
        {ok, Gpa} -> {ok, Gpa};
        {error, _Reason} -> read_ovmf_gpa(Rest)
    end.

%% @doc Parse OVMF file to extract SEV hashes table GPA.
%% This reads the OVMF footer table and finds the SEV_HASH_TABLE_RV_GUID entry.
%% @param OvmfPath Path to the OVMF file (e.g. priv/ovmf/OVMF-1.55.fd)
%% @returns {ok, GPA} where GPA is a 64-bit integer, or {error, Reason} on failure
-spec parse_ovmf_sev_hashes_gpa(OvmfPath :: string() | binary()) -> {ok, non_neg_integer()} | {error, term()}.
parse_ovmf_sev_hashes_gpa(OvmfPath) when is_binary(OvmfPath) ->
    parse_ovmf_sev_hashes_gpa(hb_util:list(OvmfPath));
parse_ovmf_sev_hashes_gpa(OvmfPath) when is_list(OvmfPath) ->
    % Print current working directory for debugging
    {ok, Cwd} = file:get_cwd(),
    ?event(snp, {parse_ovmf_sev_hashes_gpa_start, #{cwd => Cwd, ovmf_path => OvmfPath}}),
    case file:read_file(OvmfPath) of
        {ok, OvmfData} ->
            parse_ovmf_footer_table(OvmfData);
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end;
parse_ovmf_sev_hashes_gpa(_) ->
    {error, invalid_path}.

%% Internal function to parse OVMF footer table
%% Internal helper to parse OVMF footer table
%% @param OvmfData binary() - OVMF file contents
%% @returns {ok, non_neg_integer()} or {error, term()}
-spec parse_ovmf_footer_table(OvmfData :: binary()) -> {ok, non_neg_integer()} | {error, term()}.
parse_ovmf_footer_table(OvmfData) ->
    Size = byte_size(OvmfData),
    if
        Size < ?OVMF_MIN_FILE_SIZE -> {error, file_too_small};
        true ->
            % Footer entry is at offset: Size - ?OVMF_FOOTER_OFFSET - ?OVMF_ENTRY_HEADER_SIZE
            ENTRY_HEADER_SIZE = ?OVMF_ENTRY_HEADER_SIZE,  % 2 bytes size + 16 bytes GUID
            FooterEntryOffset = Size - ?OVMF_FOOTER_OFFSET - ENTRY_HEADER_SIZE,
            if
                FooterEntryOffset < 0 -> {error, invalid_file_format};
                true ->
                    % Read footer entry
                    FooterEntry = binary:part(OvmfData, FooterEntryOffset, ENTRY_HEADER_SIZE),
                    <<FooterSize:16/little, FooterGuid:16/binary>> = FooterEntry,
                    
                    % Check if this is the OVMF_TABLE_FOOTER_GUID
                    % GUID: 96b582de-1fb2-45f7-baea-a366c55a082d (little-endian)
                    % Python: uuid.UUID('96b582de-1fb2-45f7-baea-a366c55a082d').bytes_le
                    % = de 82 b5 96 b2 1f f7 45 ba ea a3 66 c5 5a 08 2d
                    ExpectedGuid = <<222, 130, 181, 150, 178, 31, 247, 69, 186, 234, 163, 102, 197, 90, 8, 45>>,
                    if
                        FooterGuid =/= ExpectedGuid -> {error, invalid_footer_guid};
                        FooterSize < ENTRY_HEADER_SIZE -> {error, invalid_footer_size};
                        true ->
                            % Calculate table size and start
                            TableSize = FooterSize - ENTRY_HEADER_SIZE,
                            TableStart = FooterEntryOffset - TableSize,
                            if
                                TableStart < 0 -> {error, invalid_table_offset};
                                true ->
                                    % Read the table
                                    TableData = binary:part(OvmfData, TableStart, TableSize),
                                    % Parse entries backwards to find SEV_HASH_TABLE_RV_GUID (from snp_guids.hrl)
                                    SevHashTableGuid = ?SEV_HASH_TABLE_RV_GUID,
                                    find_sev_hashes_gpa(TableData, SevHashTableGuid, TableSize)
                            end
                    end
            end
    end.

%% Find SEV hashes table GPA in the table data
find_sev_hashes_gpa(TableData, TargetGuid, TableSize) ->
    find_sev_hashes_gpa(TableData, TargetGuid, TableSize, TableSize).

find_sev_hashes_gpa(_TableData, _TargetGuid, _TableSize, Offset) when Offset < ?OVMF_ENTRY_HEADER_SIZE ->
    {error, guid_not_found};
find_sev_hashes_gpa(TableData, TargetGuid, TableSize, Offset) ->
    ENTRY_HEADER_SIZE = ?OVMF_ENTRY_HEADER_SIZE,
    EntryHeaderOffset = Offset - ENTRY_HEADER_SIZE,
    <<EntrySize:16/little, EntryGuid:16/binary>> = binary:part(TableData, EntryHeaderOffset, ENTRY_HEADER_SIZE),
    
    % Debug: log the GUID we're checking (first call only to avoid spam)
    case Offset =:= TableSize of
        true ->
            EntryGuidHex = hb_util:to_hex(EntryGuid),
            TargetGuidHex = hb_util:to_hex(TargetGuid),
            ?event(snp, {searching_sev_hashes_guid, {explicit, #{
                target_guid_hex => TargetGuidHex,
                first_entry_guid_hex => EntryGuidHex,
                entry_size => EntrySize,
                table_size => TableSize
            }}});
        false -> ok
    end,
    
    if
        EntrySize < ENTRY_HEADER_SIZE -> {error, invalid_entry_size};
        Offset < EntrySize -> {error, invalid_entry_offset};
        EntryGuid =:= TargetGuid ->
            % Found it! Entry data is before the header
            DataOffset = Offset - EntrySize,
            if
                DataOffset + ?OVMF_METADATA_OFFSET_SIZE > TableSize -> {error, invalid_data_offset};
                true ->
                    % First ?OVMF_GPA_EIP_SIZE bytes are the GPA (little-endian u32)
                    <<GpaU32:32/little>> = binary:part(TableData, DataOffset, ?OVMF_GPA_EIP_SIZE),
                    ?event(snp_short, {sev_hashes_gpa_found, #{gpa => GpaU32}}),
                    {ok, GpaU32}
            end;
        true ->
            % Continue searching backwards
            find_sev_hashes_gpa(TableData, TargetGuid, TableSize, Offset - EntrySize)
    end.

%% Parse reset EIP from OVMF footer table (matching Rust ovmf.sev_es_reset_eip())
%% GUID: 00f771de-1a7e-4fcb-890e-68c77e2fb44e
-spec parse_ovmf_reset_eip(OvmfPath :: string() | binary()) -> {ok, non_neg_integer()} | {error, term()}.
parse_ovmf_reset_eip(OvmfPath) when is_binary(OvmfPath) ->
    parse_ovmf_reset_eip(hb_util:list(OvmfPath));
parse_ovmf_reset_eip(OvmfPath) when is_list(OvmfPath) ->
    case file:read_file(OvmfPath) of
        {ok, OvmfData} ->
            DataSize = byte_size(OvmfData),
            if
                DataSize < ?OVMF_MIN_FILE_SIZE -> {error, file_too_small};
                true ->
                    ENTRY_HEADER_SIZE = ?OVMF_ENTRY_HEADER_SIZE,
                    FooterEntryOffset = DataSize - ?OVMF_FOOTER_OFFSET - ENTRY_HEADER_SIZE,
                    if
                        FooterEntryOffset < 0 -> {error, invalid_file_format};
                        true ->
                            FooterEntry = binary:part(OvmfData, FooterEntryOffset, ENTRY_HEADER_SIZE),
                            <<FooterSize:16/little, FooterGuid:16/binary>> = FooterEntry,
                            
                            % Check if this is the OVMF_TABLE_FOOTER_GUID
                            % GUID: 96b582de-1fb2-45f7-baea-a366c55a082d (little-endian)
                            % Python: uuid.UUID('96b582de-1fb2-45f7-baea-a366c55a082d').bytes_le
                            % = de 82 b5 96 b2 1f f7 45 ba ea a3 66 c5 5a 08 2d
                            ExpectedGuid = <<222, 130, 181, 150, 178, 31, 247, 69, 186, 234, 163, 102, 197, 90, 8, 45>>,
                            if
                                FooterGuid =/= ExpectedGuid -> {error, invalid_footer_guid};
                                FooterSize < ENTRY_HEADER_SIZE -> {error, invalid_footer_size};
                                true ->
                                    TableSize = FooterSize - ENTRY_HEADER_SIZE,
                                    TableStart = FooterEntryOffset - TableSize,
                                    if
                                        TableStart < 0 -> {error, invalid_table_offset};
                                        true ->
                                            TableData = binary:part(OvmfData, TableStart, TableSize),
                                            % SEV_ES_RESET_BLOCK_GUID: 00f771de-1a7e-4fcb-890e-68c77e2fb44e (little-endian)
                                            % Python: uuid.UUID('00f771de-1a7e-4fcb-890e-68c77e2fb44e').bytes_le
                                            % = de 71 f7 00 7e 1a cb 4f 89 0e 68 c7 7e 2f b4 4e
                                            ResetBlockGuid = ?SEV_ES_RESET_BLOCK_GUID,
                                            find_reset_eip(TableData, ResetBlockGuid, TableSize)
                                    end
                            end
                    end
            end;
        {error, Reason} -> {error, Reason}
    end;
parse_ovmf_reset_eip(_) ->
    {error, invalid_path}.

%% Find reset EIP in the footer table
find_reset_eip(TableData, TargetGuid, TableSize) ->
    find_reset_eip(TableData, TargetGuid, TableSize, TableSize).

find_reset_eip(_TableData, _TargetGuid, _TableSize, Offset) when Offset < ?OVMF_ENTRY_HEADER_SIZE ->
    {error, guid_not_found};
find_reset_eip(TableData, TargetGuid, TableSize, Offset) ->
    ENTRY_HEADER_SIZE = ?OVMF_ENTRY_HEADER_SIZE,
    EntryHeaderOffset = Offset - ENTRY_HEADER_SIZE,
    <<EntrySize:16/little, EntryGuid:16/binary>> = binary:part(TableData, EntryHeaderOffset, ENTRY_HEADER_SIZE),
    
    % Debug: log the GUID we're checking (first call only to avoid spam)
    case Offset =:= TableSize of
        true ->
            EntryGuidHex = hb_util:to_hex(EntryGuid),
            TargetGuidHex = hb_util:to_hex(TargetGuid),
            ?event(snp, {searching_reset_eip_guid, {explicit, #{
                target_guid_hex => TargetGuidHex,
                first_entry_guid_hex => EntryGuidHex,
                entry_size => EntrySize,
                table_size => TableSize
            }}});
        false -> ok
    end,
    
    if
        EntrySize < ENTRY_HEADER_SIZE -> {error, invalid_entry_size};
        Offset < EntrySize -> {error, invalid_entry_offset};
        EntryGuid =:= TargetGuid ->
            % Found it! Entry data is before the header
            DataOffset = Offset - EntrySize,
            if
                DataOffset + ?OVMF_METADATA_OFFSET_SIZE > TableSize -> {error, invalid_data_offset};
                true ->
                    % First ?OVMF_GPA_EIP_SIZE bytes are the EIP (little-endian u32)
                    <<EIP:32/little>> = binary:part(TableData, DataOffset, ?OVMF_GPA_EIP_SIZE),
                    ?event(snp_short, {reset_eip_found, #{eip => EIP}}),
                    {ok, EIP}
            end;
        true ->
            % Continue searching backwards
            find_reset_eip(TableData, TargetGuid, TableSize, Offset - EntrySize)
    end.

