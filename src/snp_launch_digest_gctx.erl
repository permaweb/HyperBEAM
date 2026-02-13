%%% @doc GCTX (Launch Digest Context) management for SNP commitment reports.
%%%
%%% This module handles the initialization and updating of the launch digest
%%% context (GCTX), which tracks the current state of the launch digest
%%% computation.
-module(snp_launch_digest_gctx).
-export([init_gctx/0, init_gctx_with_seed/1, gctx_update_page/4, build_page_info/9, update_with_vmsa_pages/4]).
-include("include/hb.hrl").
-include("include/snp_constants.hrl").
-include("include/snp_launch_digest.hrl").
-include("include/snp_guids.hrl").

%% Type definitions
-type gctx() :: #gctx{}.
-type page_type() :: ?PAGE_TYPE_NORMAL | ?PAGE_TYPE_VMSA | ?PAGE_TYPE_ZERO | 
                     ?PAGE_TYPE_SVSM_CAA | ?PAGE_TYPE_SECRETS | ?PAGE_TYPE_CPUID.
-type gpa() :: non_neg_integer().  % Guest Physical Address

%% Helper function to normalize binary to exact size (pad or truncate)
%% Optimized to avoid multiple pattern matches and improve performance
-spec normalize_binary_to_size(Binary :: binary() | term(), TargetSize :: non_neg_integer()) -> binary().
normalize_binary_to_size(Binary, TargetSize) when is_binary(Binary) ->
    case byte_size(Binary) of
        TargetSize -> Binary;
        Size when Size > TargetSize -> binary:part(Binary, 0, TargetSize);
        Size when Size < TargetSize -> 
            PaddingSize = TargetSize - Size,
            <<Binary/binary, 0:(PaddingSize * 8)>>
    end;
normalize_binary_to_size(_, TargetSize) ->
    <<0:(TargetSize * 8)>>.

%% @doc Initialize GCTX with zeros
%% @returns #gctx{} record with launch digest initialized to zeros
-spec init_gctx() -> gctx().
init_gctx() ->
    ?event(snp_short, init_gctx_called),
    GCTX = #gctx{ld = <<0:?LAUNCH_DIGEST_BITS>>},  % ?LAUNCH_DIGEST_SIZE bytes of zeros
    ?event(snp_short, {init_gctx_result, #{ld_size => byte_size(GCTX#gctx.ld)}}),
    GCTX.

%% @doc Initialize GCTX with seed (OVMF hash)
%% @param Seed ?LAUNCH_DIGEST_SIZE-byte binary seed value
%% @returns #gctx{} record with launch digest initialized to seed
-spec init_gctx_with_seed(Seed :: binary()) -> gctx().
init_gctx_with_seed(Seed) when byte_size(Seed) =:= ?LAUNCH_DIGEST_SIZE ->
    ?event(snp_short, {init_gctx_with_seed, #{seed_size => byte_size(Seed)}}),
    GCTX = #gctx{ld = Seed},
    ?event(snp_short, {init_gctx_with_seed_result, #{ld_size => byte_size(GCTX#gctx.ld)}}),
    GCTX.

%% @doc Update launch digest with page data
%% @param GCTX #gctx{} record with current launch digest
%% @param PageType integer() - Page type (1=Normal, 2=VMSA, 3=Zero, etc.)
%% @param GPA non_neg_integer() - Guest physical address
%% @param Contents undefined | binary() - Page contents (undefined for zero pages)
%% @returns #gctx{} record with updated launch digest
-spec gctx_update_page(GCTX :: gctx(), PageType :: page_type(), GPA :: gpa(), Contents :: undefined | binary()) -> 
    gctx().
gctx_update_page(GCTX, PageType, GPA, Contents) ->
    CurrentLD = GCTX#gctx.ld,
    CurrentLDHex = snp_util:binary_to_hex_string(CurrentLD),
    ?event(snp_short, {gctx_update_page_start, #{
        page_type => PageType, 
        gpa => GPA, 
        contents_size => case Contents of undefined -> undefined; Cont when is_binary(Cont) -> byte_size(Cont); _ -> Contents end,
        current_ld_size => byte_size(CurrentLD),
        current_ld_hex => CurrentLDHex
    }}),
    PageInfoLen = ?PAGE_INFO_LEN,
    IsIMI = 0,
    VMPL3Perms = 0,
    VMPL2Perms = 0,
    VMPL1Perms = 0,
    
    % Build page_info structure
    PageInfo = build_page_info(
        CurrentLD, PageType, GPA, Contents,
        IsIMI, VMPL3Perms, VMPL2Perms, VMPL1Perms, PageInfoLen),
    PageInfoHex = snp_util:binary_to_hex_string(PageInfo),
    ?event(snp_short, {page_info_built, #{
        page_info_size => byte_size(PageInfo),
        page_info_hex => PageInfoHex
    }}),
    
    % Hash page_info to get new launch digest
    NewLD = crypto:hash(sha384, PageInfo),
    ?event(snp_short, {gctx_update_page_complete, #{
        page_type => PageType,
        gpa => GPA,
        new_ld_size => byte_size(NewLD)
    }}),
    
    GCTX#gctx{ld = NewLD}.

%% @doc Build page_info structure
%% @param CurrentLD binary() - Current launch digest (?LAUNCH_DIGEST_SIZE bytes)
%% @param PageType integer() - Page type (1=Normal, 2=VMSA, 3=Zero, etc.)
%% @param GPA non_neg_integer() - Guest physical address
%% @param Contents undefined | binary() - Page contents (undefined for zero pages)
%% @param IsIMI integer() - IMI flag (0 or 1)
%% @param VMPL3 integer() - VMPL3 permissions
%% @param VMPL2 integer() - VMPL2 permissions
%% @param VMPL1 integer() - VMPL1 permissions
%% @param PageInfoLen integer() - Page info structure length (?PAGE_INFO_LEN)
%% @returns binary() - Page info structure (?PAGE_INFO_LEN bytes)
-spec build_page_info(CurrentLD :: binary(), PageType :: integer(), GPA :: non_neg_integer(), 
    Contents :: undefined | binary(), IsIMI :: integer(), VMPL3 :: integer(), 
    VMPL2 :: integer(), VMPL1 :: integer(), PageInfoLen :: integer()) -> binary().
build_page_info(CurrentLD, PageType, GPA, Contents, IsIMI, VMPL3, VMPL2, VMPL1, PageInfoLen) ->
    CurrentLDSizeInfo = case CurrentLD of CLD when is_binary(CLD) -> byte_size(CLD); _ -> undefined end,
    ContentsSizeInfo = case Contents of undefined -> undefined; Cont when is_binary(Cont) -> byte_size(Cont); _ -> Contents end,
    ?event(snp_short, {build_page_info_start, #{
        current_ld_size => CurrentLDSizeInfo,
        page_type => PageType,
        gpa => GPA,
        contents_size => ContentsSizeInfo
    }}),
    % Ensure CurrentLD is exactly ?LAUNCH_DIGEST_SIZE bytes
    CurrentLDOriginalSize = case is_binary(CurrentLD) of true -> byte_size(CurrentLD); false -> undefined end,
    CurrentLD48 = normalize_binary_to_size(CurrentLD, ?LAUNCH_DIGEST_SIZE),
    case CurrentLDOriginalSize of
        undefined -> 
            ?event(snp_short, current_ld_not_binary_using_zeros);
        Size when Size > ?LAUNCH_DIGEST_SIZE -> 
            ?event(snp_short, {current_ld_truncated, #{from => Size, to => ?LAUNCH_DIGEST_SIZE}});
        Size when Size < ?LAUNCH_DIGEST_SIZE -> 
            ?event(snp_short, {current_ld_padded, #{from => Size, to => ?LAUNCH_DIGEST_SIZE}});
        _ -> ok
    end,
    
    % Copy current launch digest (?LAUNCH_DIGEST_SIZE bytes)
    % Copy page contents or hash
    % For zero pages, secrets, and CPUID pages, Rust uses ZEROS = [0; ?LAUNCH_DIGEST_SIZE] (?LAUNCH_DIGEST_SIZE bytes of zeros)
    % This matches the Rust implementation: const ZEROS: [u8; LD_BYTES] = [0; LD_BYTES];
    PageContentsHash = case {PageType, Contents} of
        {?PAGE_TYPE_ZERO, _} -> 
            ?event(snp_short, page_contents_zero_page),
            <<0:?LAUNCH_DIGEST_BITS>>;  % PAGE_TYPE_ZERO - ?LAUNCH_DIGEST_SIZE bytes of zeros (matching Rust ZEROS)
        {?PAGE_TYPE_SECRETS, _} -> 
            ?event(snp_short, page_contents_secrets),
            <<0:?LAUNCH_DIGEST_BITS>>;  % PAGE_TYPE_SECRETS - ?LAUNCH_DIGEST_SIZE bytes of zeros (matching Rust ZEROS)
        {?PAGE_TYPE_CPUID, _} -> 
            ?event(snp, page_contents_cpuid),
            <<0:?LAUNCH_DIGEST_BITS>>;  % PAGE_TYPE_CPUID - ?LAUNCH_DIGEST_SIZE bytes of zeros (matching Rust ZEROS)
        {?PAGE_TYPE_NORMAL, C} when is_binary(C), byte_size(C) =:= ?PAGE_SIZE -> 
            ?event(snp_short, {page_contents_normal_hashing, #{size => byte_size(C)}}),
            crypto:hash(sha384, C);  % PAGE_TYPE_NORMAL
        {?PAGE_TYPE_VMSA, C} when is_binary(C), byte_size(C) =:= ?PAGE_SIZE -> 
            ?event(snp_short, {page_contents_vmsa_hashing, #{size => byte_size(C)}}),
            crypto:hash(sha384, C);  % PAGE_TYPE_VMSA
        {_, C} when is_binary(C), byte_size(C) =:= ?LAUNCH_DIGEST_SIZE -> 
            ?event(snp_short, {page_contents_already_hash, #{size => byte_size(C)}}),
            C;  % Already a ?LAUNCH_DIGEST_SIZE-byte hash
        {_, _} -> 
            ?event(snp, {page_contents_default_zeros, #{page_type => PageType}}),
            <<0:?LAUNCH_DIGEST_BITS>>  % Default to ?LAUNCH_DIGEST_SIZE bytes of zeros
    end,
    
    % Ensure PageContentsHash is exactly ?LAUNCH_DIGEST_SIZE bytes
    PageContentsHashOriginalSize = byte_size(PageContentsHash),
    PageContentsHash48 = normalize_binary_to_size(PageContentsHash, ?LAUNCH_DIGEST_SIZE),
    if PageContentsHashOriginalSize > ?LAUNCH_DIGEST_SIZE -> 
        ?event(snp, {page_contents_hash_truncated, #{from => PageContentsHashOriginalSize, to => ?LAUNCH_DIGEST_SIZE}});
    PageContentsHashOriginalSize < ?LAUNCH_DIGEST_SIZE -> 
        ?event(snp, {page_contents_hash_padded, #{from => PageContentsHashOriginalSize, to => ?LAUNCH_DIGEST_SIZE}});
    true -> ok
    end,
    
    % Build complete page_info (?PAGE_INFO_LEN bytes)
    PageInfo = <<CurrentLD48:?LAUNCH_DIGEST_SIZE/binary,
      PageContentsHash48:?LAUNCH_DIGEST_SIZE/binary,
      PageInfoLen:16/little,
      PageType:8,
      IsIMI:8,
      VMPL3:8,
      VMPL2:8,
      VMPL1:8,
      0:8,  % Reserved
      GPA:64/little>>,
    CurrentLDHex = snp_util:binary_to_hex_string(CurrentLD48),
    PageContentsHashHex = snp_util:binary_to_hex_string(PageContentsHash48),
    ?event(snp, {build_page_info_complete, #{
        page_info_size => byte_size(PageInfo),
        current_ld_hex => CurrentLDHex,
        page_contents_hash_hex => PageContentsHashHex,
        page_info_len => PageInfoLen,
        page_type => PageType,
        gpa => GPA,
        gpa_hex => integer_to_list(GPA, 16)
    }}),
    PageInfo.

%% @doc Update GCTX with VMSA pages
%% @param GCTX #gctx{} record with current launch digest
%% @param VCPUs non_neg_integer() - Number of VCPUs
%% @param BSPVMSA binary() - BSP VMSA page (?PAGE_SIZE bytes)
%% @param APVMSA binary() - AP VMSA page (?PAGE_SIZE bytes)
%% @returns #gctx{} record with updated launch digest
-spec update_with_vmsa_pages(GCTX :: #gctx{}, VCPUs :: non_neg_integer(), BSPVMSA :: binary(), APVMSA :: binary()) -> 
    #gctx{}.
update_with_vmsa_pages(GCTX, VCPUs, BSPVMSA, APVMSA) ->
    % DoS safeguard: reject out-of-range VCPUs before building lists:seq(0, VCPUs - 1)
    case is_integer(VCPUs) andalso VCPUs >= 1 andalso VCPUs =< ?MAX_VCPUS of
        true -> ok;
        false -> erlang:error({invalid_vcpus, VCPUs}, [GCTX, VCPUs, BSPVMSA, APVMSA])
    end,
    ?event(snp, {update_with_vmsa_pages_start, #{
        vcpus => VCPUs,
        bsp_vmsa_size => byte_size(BSPVMSA),
        ap_vmsa_size => byte_size(APVMSA),
        current_ld_size => byte_size(GCTX#gctx.ld)
    }}),
    VMSAGPA = ?VMSA_GPA,
    Result = lists:foldl(
        fun(I, AccGCTX) ->
            VMSAToUse = case I of
                0 -> 
                    ?event(snp, {updating_vmsa_for_vcpu, #{vcpu => I, type => bsp}}),
                    BSPVMSA;
                _ -> 
                    ?event(snp, {updating_vmsa_for_vcpu, #{vcpu => I, type => ap}}),
                    APVMSA
            end,
            VMSAHash = crypto:hash(sha384, VMSAToUse),
            ?event(snp, {vmsa_before_update, #{
                vcpu => I,
                vmsa_type => case I of 0 -> bsp; _ -> ap end,
                vmsa_hash_hex => snp_util:binary_to_hex_string(VMSAHash),
                current_ld_hex => snp_util:binary_to_hex_string(AccGCTX#gctx.ld),
                vmsa_gpa => VMSAGPA
            }}),
            NewGCTX = gctx_update_page(AccGCTX, ?PAGE_TYPE_VMSA, VMSAGPA, VMSAToUse),
            ?event(snp, {vmsa_updated_for_vcpu, #{
                vcpu => I,
                vmsa_type => case I of 0 -> bsp; _ -> ap end,
                new_ld_size => byte_size(NewGCTX#gctx.ld),
                new_ld_hex => snp_util:binary_to_hex_string(NewGCTX#gctx.ld),
                old_ld_hex => snp_util:binary_to_hex_string(AccGCTX#gctx.ld)
            }}),
            NewGCTX
        end,
        GCTX,
        lists:seq(0, VCPUs - 1)
    ),
    ?event(snp_short, {update_with_vmsa_pages_complete, #{
        vcpus => VCPUs,
        final_ld_size => byte_size(Result#gctx.ld)
    }}),
    Result.

