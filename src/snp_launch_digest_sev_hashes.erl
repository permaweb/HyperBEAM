%%% @doc SEV hashes table construction for SNP commitment reports.
%%%
%%% This module handles the construction of SEV hashes pages, which contain
%%% kernel, initrd, and append hashes in a structured format.
-module(snp_launch_digest_sev_hashes).
-export([construct_sev_hashes_page_erlang/4, update_sev_hashes_table/5]).
-include("include/hb.hrl").
-include("include/snp_constants.hrl").
-include("include/snp_launch_digest.hrl").
-include("include/snp_guids.hrl").

%% @doc Construct SEV hashes page
%% @param KernelHash binary() - Kernel hash (SHA-256, ?SEV_HASH_BINARY_SIZE bytes or hex string)
%% @param InitrdHash binary() - Initrd hash (SHA-256, ?SEV_HASH_BINARY_SIZE bytes or hex string)
%% @param AppendHash binary() - Append hash (SHA-256, ?SEV_HASH_BINARY_SIZE bytes or hex string)
%% @param PageOffset non_neg_integer() - Page offset for hash table placement
%% @returns {ok, binary()} - Complete SEV hashes page (?PAGE_SIZE bytes), or {error, invalid_hex}
-spec construct_sev_hashes_page_erlang(KernelHash :: binary(), InitrdHash :: binary(),
    AppendHash :: binary(), PageOffset :: non_neg_integer()) ->
    {ok, binary()} | {error, invalid_hex}.
construct_sev_hashes_page_erlang(KernelHash, InitrdHash, AppendHash, PageOffset) ->
    ?event(snp, {construct_sev_hashes_page_start, #{
        page_offset => PageOffset,
        kernel_size => byte_size(KernelHash),
        initrd_size => byte_size(InitrdHash),
        append_size => byte_size(AppendHash)
    }}),
    % Convert hex strings to binary if needed (hashes come in as hex strings, need ?SEV_HASH_BINARY_SIZE-byte binaries)
    case hash_to_binary(KernelHash) of
        {error, invalid_hex} -> {error, invalid_hex};
        {ok, KernelHashBin} ->
            case hash_to_binary(InitrdHash) of
                {error, invalid_hex} -> {error, invalid_hex};
                {ok, InitrdHashBin} ->
                    case hash_to_binary(AppendHash) of
                        {error, invalid_hex} -> {error, invalid_hex};
                        {ok, AppendHashBin} ->
                            build_sev_hashes_page(KernelHashBin, InitrdHashBin, AppendHashBin, PageOffset)
                    end
            end
    end.

%% @doc Convert hash (binary or hex string) to ?SEV_HASH_BINARY_SIZE binary.
-spec hash_to_binary(binary()) -> {ok, binary()} | {error, invalid_hex}.
hash_to_binary(Hash) when byte_size(Hash) =:= ?SEV_HASH_BINARY_SIZE ->
    {ok, Hash};
hash_to_binary(Hash) when byte_size(Hash) =:= ?SEV_HASH_HEX_SIZE ->
    snp_util:hex_to_binary(Hash);
hash_to_binary(Hash) ->
    {ok, Hash}.

-spec build_sev_hashes_page(binary(), binary(), binary(), non_neg_integer()) -> {ok, binary()}.
build_sev_hashes_page(KernelHashBin, InitrdHashBin, AppendHashBin, PageOffset) ->
    ?event(snp, {hashes_converted, #{
        kernel_size => byte_size(KernelHashBin),
        initrd_size => byte_size(InitrdHashBin),
        append_size => byte_size(AppendHashBin)
    }}),
    
    % SEV Hash Table GUIDs (from snp_guids.hrl)
    SevHashTableHeaderGuid = ?SEV_HASH_TABLE_HEADER_GUID,
    SevCmdlineEntryGuid = ?SEV_CMDLINE_ENTRY_GUID,
    SevInitrdEntryGuid = ?SEV_INITRD_ENTRY_GUID,
    SevKernelEntryGuid = ?SEV_KERNEL_ENTRY_GUID,
    
    % Each entry is: GUID (16 bytes) + Length (2 bytes LE) + Hash (?SEV_HASH_BINARY_SIZE bytes SHA-256)
    % According to Rust code, length = size_of::<SevHashTableEntry>() = ?SEV_HASH_TABLE_ENTRY_LENGTH bytes
    EntryLength = ?SEV_HASH_TABLE_ENTRY_LENGTH,  % Total entry size including GUID
    
    % Build entries (cmdline/append, initrd, kernel)
    % Entry format: GUID (16) + Length (2, LE) + Hash (?SEV_HASH_BINARY_SIZE)
    % Note: Rust uses EntryLength = 50 (total entry size) in the length field
    AppendEntry = <<SevCmdlineEntryGuid:16/binary, EntryLength:16/little, AppendHashBin:?SEV_HASH_BINARY_SIZE/binary>>,
    InitrdEntry = <<SevInitrdEntryGuid:16/binary, EntryLength:16/little, InitrdHashBin:?SEV_HASH_BINARY_SIZE/binary>>,
    KernelEntry = <<SevKernelEntryGuid:16/binary, EntryLength:16/little, KernelHashBin:?SEV_HASH_BINARY_SIZE/binary>>,
    
    % Build the SevHashTable structure (matches Rust PaddedSevHashTable)
    % Header: GUID (16) + Length (2) = 18 bytes
    % Table length = size_of::<SevHashTable>() = 16 (guid) + 2 (length) + 3*?SEV_HASH_TABLE_ENTRY_LENGTH (entries) = ?SEV_HASH_TABLE_SIZE
    TableLength = ?SEV_HASH_TABLE_SIZE,
    Header = <<SevHashTableHeaderGuid:16/binary, TableLength:16/little>>,
    
    % Build complete table: Header + Cmdline + Initrd + Kernel
    % Order matches Rust: cmdline, initrd, kernel
    HashTable = <<Header/binary, AppendEntry/binary, InitrdEntry/binary, KernelEntry/binary>>,
    
    % The Rust code uses bincode serialization which may add padding
    % PaddedSevHashTable adds padding to align to 16 bytes
    % Padding size = ((size_of::<SevHashTable>() + 15) & !15) - size_of::<SevHashTable>()
    % SevHashTable size = ?SEV_HASH_TABLE_SIZE, so padding = ?SEV_HASH_TABLE_PADDING
    PaddingSize = ?SEV_HASH_TABLE_PADDING,
    Padding = <<0:(PaddingSize*8)>>,
    PaddedHashTable = <<HashTable/binary, Padding/binary>>,
    
    ?event(snp, {hash_table_built, #{
        header_size => byte_size(Header),
        table_length => TableLength,
        hash_table_size => byte_size(HashTable),
        padded_size => byte_size(PaddedHashTable)
    }}),
    
    % Build the page: zeros up to offset, then hash table, then zeros to fill page
    PagePrefix = <<0:(PageOffset*8)>>,
    HashTableSize = byte_size(PaddedHashTable),
    PageSuffixSize = ?PAGE_SIZE - PageOffset - HashTableSize,
    PageSuffix = case PageSuffixSize > 0 of
        true -> <<0:(PageSuffixSize*8)>>;
        false -> <<>>
    end,
    Result = <<PagePrefix/binary, PaddedHashTable/binary, PageSuffix/binary>>,
    ?event(snp_short, {construct_sev_hashes_page_complete, #{
        result_size => byte_size(Result),
        page_offset => PageOffset,
        hash_table_size => HashTableSize
    }}),
    {ok, Result}.

%% @doc Update SEV hashes table in GCTX
%% @param GCTX #gctx{} record with current launch digest
%% @param KernelHash binary() - Kernel hash
%% @param InitrdHash binary() - Initrd hash
%% @param AppendHash binary() - Append hash
%% @param SevHashesGPA non_neg_integer() - SEV hashes table GPA
%% @returns {ok, #gctx{}} with updated launch digest, or {error, invalid_hex}
-spec update_sev_hashes_table(GCTX :: #gctx{}, KernelHash :: binary(), InitrdHash :: binary(),
    AppendHash :: binary(), SevHashesGPA :: non_neg_integer()) -> {ok, #gctx{}} | {error, invalid_hex}.
update_sev_hashes_table(GCTX, KernelHash, InitrdHash, AppendHash, SevHashesGPA) ->
    ?event(snp, {update_sev_hashes_table_start, #{
        sev_hashes_gpa => SevHashesGPA,
        kernel_size => byte_size(KernelHash),
        initrd_size => byte_size(InitrdHash),
        append_size => byte_size(AppendHash)
    }}),
    PageOffset = SevHashesGPA band ?PAGE_MASK,
    PageAlignedGPA = SevHashesGPA band (bnot ?PAGE_MASK),
    ?event(snp, {sev_hashes_page_calc, #{page_offset => PageOffset, page_aligned_gpa => PageAlignedGPA}}),
    case construct_sev_hashes_page_erlang(KernelHash, InitrdHash, AppendHash, PageOffset) of
        {ok, SevHashesPage} ->
            ?event(snp_short, {sev_hashes_page_constructed, #{page_size => byte_size(SevHashesPage)}}),
            {ok, snp_launch_digest_gctx:gctx_update_page(GCTX, ?PAGE_TYPE_NORMAL, PageAlignedGPA, SevHashesPage)};
        {error, invalid_hex} ->
            {error, invalid_hex}
    end.

