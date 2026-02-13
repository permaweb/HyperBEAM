%%% @doc Launch digest computation for SNP commitment reports.
%%%
%%% This module orchestrates the computation of launch digests for AMD SEV-SNP
%%% attestation reports, delegating to specialized sub-modules for OVMF parsing,
%%% VMSA page creation, and launch digest calculation.
-module(snp_launch_digest).
-export([compute_launch_digest/1]).
-include("include/hb.hrl").
-include("include/snp_constants.hrl").
-include("include/snp_launch_digest.hrl").
-include("include/snp_guids.hrl").

%% Type definitions
-type gctx() :: #gctx{}.
-type vmm_type() :: ?VMM_TYPE_QEMU | ?VMM_TYPE_EC2.
-type vcpu_type() :: integer().  % VCPU type identifier (0=Epyc, 1=EpycV1, etc.)
-type guest_features() :: non_neg_integer().  % Guest features flags
-type launch_digest_args() :: #{
    vcpus => integer(),
    vcpu_type => integer(),
    vmm_type => ?VMM_TYPE_QEMU | ?VMM_TYPE_EC2,
    guest_features => non_neg_integer(),
    firmware => undefined | binary() | list(),
    kernel => undefined | binary(),
    initrd => undefined | binary(),
    append => undefined | binary(),
    sev_hashes_gpa => non_neg_integer()
}.

%% @doc Compute launch digest - pure Erlang implementation
%% @param Args Map containing: vcpus, vcpu_type, vmm_type, guest_features, firmware, kernel, initrd, append, sev_hashes_gpa
%% @returns {ok, Digest} where Digest is ?LAUNCH_DIGEST_SIZE-byte binary, or {error, invalid_args} if Args is not a map
-spec compute_launch_digest(Args :: map() | term()) -> 
    {ok, binary()} | {error, invalid_args}.
compute_launch_digest(Args) when is_map(Args) ->
    compute_launch_digest_erlang(Args);
compute_launch_digest(_Args) ->
    {error, invalid_args}.

%% @doc Compute launch digest - pure Erlang implementation
%% @param Args Map containing launch digest parameters:
%%   - vcpus: non_neg_integer() - Number of VCPUs
%%   - vcpu_type: integer() - VCPU type identifier
%%   - vmm_type: integer() - VMM type (1=QEMU, 2=EC2)
%%   - guest_features: non_neg_integer() - Guest features flags
%%   - firmware: undefined | binary() | list() - Firmware hash (optional)
%%   - kernel: undefined | binary() - Kernel hash (optional)
%%   - initrd: undefined | binary() - Initrd hash (optional)
%%   - append: undefined | binary() - Append hash (optional)
%%   - sev_hashes_gpa: non_neg_integer() - SEV hashes table GPA (optional, defaults to 0)
%% @returns {ok, Digest} where Digest is ?LAUNCH_DIGEST_SIZE-byte binary, or {error, {computation_failed, Error, Reason}} on failure
-spec compute_launch_digest_erlang(Args :: map()) -> 
    {ok, binary()} | {error, {computation_failed, term(), term()}}.
compute_launch_digest_erlang(Args) ->
    ?event(snp_short, {compute_launch_digest_erlang_start, Args}),
    {TimeMicros, Result} = timer:tc(fun() ->
        try
            compute_launch_digest_steps(Args)
        catch
            Error:Reason -> 
                ?event(snp_error, {compute_launch_digest_erlang_error, #{error => Error, reason => Reason}}),
                {error, {computation_failed, Error, Reason}}
        end
    end),
    TimeMs = TimeMicros / 1000,
    ?event(snp_short, {compute_launch_digest_time_ms, TimeMs}),
    Result.

%% Helper function to execute launch digest computation steps
-spec compute_launch_digest_steps(Args :: map()) -> {ok, binary()} | {error, term()}.
compute_launch_digest_steps(Args) ->
    % Extract parameters
    {VCPUs, VCPUType, VMMType, GuestFeatures, FirmwareHash, KernelHash, InitrdHash, AppendHash, SevHashesGPA} = 
        extract_launch_digest_params(Args),
    
    % Initialize GCTX with OVMF hash
    GCTX = initialize_gctx_from_firmware(FirmwareHash),
    
    % Parse and update OVMF metadata (also get reset EIP for VMSA)
    {GCTX1, ResetEIP} = process_ovmf_metadata(GCTX, VMMType, KernelHash, InitrdHash, AppendHash, SevHashesGPA),
    
    % Create VMSA pages and update GCTX
    GCTX2 = create_and_update_vmsa_pages(GCTX1, VCPUs, VCPUType, VMMType, GuestFeatures, ResetEIP),
    
    % Return final digest
    FinalLDHex = snp_util:binary_to_hex_string(GCTX2#gctx.ld),
    ?event(snp_short, {compute_launch_digest_erlang_success, #{
        digest_size => byte_size(GCTX2#gctx.ld),
        digest_hex => FinalLDHex
    }}),
    {ok, GCTX2#gctx.ld}.

%% Helper function to process OVMF metadata
-spec process_ovmf_metadata(GCTX :: #gctx{}, VMMType :: integer(), 
                          KernelHash :: undefined | binary(), InitrdHash :: undefined | binary(),
                          AppendHash :: undefined | binary(), SevHashesGPA :: non_neg_integer()) ->
    {#gctx{}, ResetEIP :: non_neg_integer()}.
process_ovmf_metadata(GCTX, VMMType, KernelHash, InitrdHash, AppendHash, SevHashesGPA) ->
    ?event(snp_short, {parsing_ovmf_metadata, #{vmm_type => VMMType, sev_hashes_gpa => SevHashesGPA}}),
    {GCTX1, ResetEIP} = snp_launch_digest_ovmf:parse_and_update_ovmf_metadata_erlang(
        GCTX, VMMType, KernelHash, InitrdHash, AppendHash, SevHashesGPA),
    AfterMetadataLDHex = snp_util:binary_to_hex_string(GCTX1#gctx.ld),
    ?event(snp_short, {ovmf_metadata_parsed, #{
        ld_size => byte_size(GCTX1#gctx.ld),
        ld_hex => AfterMetadataLDHex,
        reset_eip => ResetEIP
    }}),
    {GCTX1, ResetEIP}.

%% Helper function to create VMSA pages and update GCTX
-spec create_and_update_vmsa_pages(GCTX :: gctx(), VCPUs :: integer(), VCPUType :: vcpu_type(),
                                  VMMType :: vmm_type(), GuestFeatures :: guest_features(), ResetEIP :: non_neg_integer()) -> gctx().
create_and_update_vmsa_pages(GCTX, VCPUs, VCPUType, VMMType, GuestFeatures, ResetEIP) ->
    % Create VMSA pages (use reset EIP from OVMF, matching Rust)
    ?event(snp_short, {creating_vmsa_pages, #{vcpu_type => VCPUType, vmm_type => VMMType, guest_features => GuestFeatures, reset_eip => ResetEIP}}),
    {BSPVMSA, APVMSA} = snp_launch_digest_vmsa:create_vmsa_pages_erlang(
        ResetEIP, VCPUType, VMMType, GuestFeatures),
    ?event(snp_short, {vmsa_pages_created, #{bsp_size => byte_size(BSPVMSA), ap_size => byte_size(APVMSA)}}),
    
    % Update GCTX with VMSA pages
    ?event(snp_short, {updating_with_vmsa_pages, #{vcpus => VCPUs}}),
    GCTX2 = snp_launch_digest_gctx:update_with_vmsa_pages(GCTX, VCPUs, BSPVMSA, APVMSA),
    AfterVMSALDHex = snp_util:binary_to_hex_string(GCTX2#gctx.ld),
    ?event(snp_short, {vmsa_pages_updated, #{
        ld_size => byte_size(GCTX2#gctx.ld),
        ld_hex => AfterVMSALDHex
    }}),
    GCTX2.

%% Helper function to extract launch digest parameters from Args map
-spec extract_launch_digest_params(Args :: launch_digest_args()) -> 
    {integer(), vcpu_type(), vmm_type(), guest_features(), undefined | binary() | list(), 
     undefined | binary(), undefined | binary(), undefined | binary(), non_neg_integer()}.
extract_launch_digest_params(Args) ->
    VCPUs = maps:get(vcpus, Args),
    VCPUType = maps:get(vcpu_type, Args),
    VMMType = maps:get(vmm_type, Args),
    GuestFeatures = maps:get(guest_features, Args, 0),
    FirmwareHash = maps:get(firmware, Args, undefined),
    KernelHash = maps:get(kernel, Args, undefined),
    InitrdHash = maps:get(initrd, Args, undefined),
    AppendHash = maps:get(append, Args, undefined),
    SevHashesGPA = maps:get(sev_hashes_gpa, Args, 0),
    ?event(snp, {extracted_params, #{vcpus => VCPUs, vcpu_type => VCPUType, vmm_type => VMMType, guest_features => GuestFeatures}}),
    FirmwareHashInfo = case FirmwareHash of 
        undefined -> undefined; 
        FH when is_binary(FH) -> {size, byte_size(FH)}; 
        _ -> FirmwareHash 
    end,
    KernelHashInfo = case KernelHash of 
        undefined -> undefined; 
        KH when is_binary(KH) -> {size, byte_size(KH)}; 
        _ -> KernelHash 
    end,
    InitrdHashInfo = case InitrdHash of 
        undefined -> undefined; 
        IH when is_binary(IH) -> {size, byte_size(IH)}; 
        _ -> InitrdHash 
    end,
    AppendHashInfo = case AppendHash of 
        undefined -> undefined; 
        AH when is_binary(AH) -> {size, byte_size(AH)}; 
        _ -> AppendHash 
    end,
    ?event(snp_short, {extracted_hashes, #{
        firmware => FirmwareHashInfo,
        kernel => KernelHashInfo,
        initrd => InitrdHashInfo,
        append => AppendHashInfo,
        sev_hashes_gpa => SevHashesGPA
    }}),
    {VCPUs, VCPUType, VMMType, GuestFeatures, FirmwareHash, KernelHash, InitrdHash, AppendHash, SevHashesGPA}.

%% Helper function to initialize GCTX from firmware hash
-spec initialize_gctx_from_firmware(FirmwareHash :: undefined | binary() | list()) -> gctx().
initialize_gctx_from_firmware(FirmwareHash) ->
    FirmwareHashInfo = case FirmwareHash of 
        undefined -> undefined; 
        FH when is_binary(FH) -> {size, byte_size(FH)}; 
        _ -> FirmwareHash 
    end,
    ?event(snp_short, {initializing_gctx, #{firmware_hash => FirmwareHashInfo}}),
    GCTX = case FirmwareHash of
        undefined -> 
            ?event(snp_short, gctx_init_with_zeros),
            % When firmware hash is not provided, initialize with zeros
            % Then we'll update with full OVMF data in parse_ovmf_and_update
            % (matching Rust: gctx.update_page(PageType::Normal, ovmf.gpa(), Some(ovmf.data()), None)?)
            snp_launch_digest_gctx:init_gctx();
        Hash when is_binary(Hash) ->
            HashSize = byte_size(Hash),
            ?event(snp_short, {gctx_init_with_binary, #{size => HashSize}}),
            case HashSize of
                ?HEX_STRING_48_BYTES ->
                    ?event(snp_short, gctx_init_from_hex_96),
                    case snp_util:hex_to_binary(Hash) of
                        {ok, B} -> snp_launch_digest_gctx:init_gctx_with_seed(B);
                        {error, invalid_hex} -> erlang:error(invalid_hex)
                    end;
                ?LAUNCH_DIGEST_SIZE ->
                    ?event(snp_short, gctx_init_from_binary_48),
                    snp_launch_digest_gctx:init_gctx_with_seed(Hash);
                _ ->
                    ?event(snp_short, {gctx_init_fallback_to_zeros, #{size => HashSize}}),
                    snp_launch_digest_gctx:init_gctx()
            end;
        Hash when is_list(Hash) ->
            HashBin = hb_util:bin(Hash),
            HashSize = byte_size(HashBin),
            ?event(snp_short, {gctx_init_with_list, #{size => HashSize}}),
            case HashSize of
                ?HEX_STRING_48_BYTES ->
                    ?event(snp, gctx_init_from_hex_96_list),
                    case snp_util:hex_to_binary(HashBin) of
                        {ok, B} -> snp_launch_digest_gctx:init_gctx_with_seed(B);
                        {error, invalid_hex} -> erlang:error(invalid_hex)
                    end;
                ?LAUNCH_DIGEST_SIZE ->
                    ?event(snp, gctx_init_from_binary_48_list),
                    snp_launch_digest_gctx:init_gctx_with_seed(HashBin);
                _ ->
                    ?event(snp, {gctx_init_fallback_to_zeros_list, #{size => HashSize}}),
                    snp_launch_digest_gctx:init_gctx()
            end
    end,
    InitialLDHex = snp_util:binary_to_hex_string(GCTX#gctx.ld),
    ?event(snp, {gctx_initialized, #{
        ld_size => byte_size(GCTX#gctx.ld),
        ld_hex => InitialLDHex
    }}),
    GCTX.

