%%% @doc VMSA (Virtual Machine Save Area) page creation for SNP commitment reports.
%%%
%%% This module handles the creation of VMSA pages for BSP and AP VCPUs,
%%% including segment registers, control registers, and other CPU state fields.
-module(snp_launch_digest_vmsa).
-export([create_vmsa_pages_erlang/4]).
-include("include/hb.hrl").
-include("include/snp_constants.hrl").
-include("include/snp_guids.hrl").

%% @doc Create VMSA pages for BSP and AP
%% ResetEIP should be read from OVMF footer table (matching Rust ovmf.sev_es_reset_eip())
%% BSP uses BSP_EIP = 0xffff_fff0, AP uses ResetEIP from OVMF (matching Rust)
%% @param ResetEIP non_neg_integer() - Reset EIP value from OVMF
%% @param VCPUType integer() - VCPU type identifier
%% @param VMMType integer() - VMM type (1=QEMU, 2=EC2)
%% @param GuestFeatures non_neg_integer() - Guest features flags
%% @returns {BSPVMSA, APVMSA} where both are ?PAGE_SIZE-byte binaries
-spec create_vmsa_pages_erlang(ResetEIP :: non_neg_integer(), VCPUType :: integer(), 
    VMMType :: integer(), GuestFeatures :: non_neg_integer()) -> 
    {binary(), binary()}.
create_vmsa_pages_erlang(ResetEIP, VCPUType, VMMType, GuestFeatures) ->
    ?event(snp, {create_vmsa_pages_start, #{reset_eip => ResetEIP, vcpu_type => VCPUType, vmm_type => VMMType, guest_features => GuestFeatures}}),
    % BSP uses BSP_EIP (matching Rust const BSP_EIP: u64 = 0xffff_fff0;)
    BSP_EIP = ?BSP_EIP,
    BSPVMSA = create_vmsa_page_erlang(BSP_EIP, VCPUType, VMMType, GuestFeatures),
    ?event(snp_short, {bsp_vmsa_created, #{size => byte_size(BSPVMSA), eip => BSP_EIP}}),
    % AP uses ResetEIP from OVMF (matching Rust: ap_eip parameter)
    APVMSA = create_vmsa_page_erlang(ResetEIP, VCPUType, VMMType, GuestFeatures),
    ?event(snp_short, {ap_vmsa_created, #{size => byte_size(APVMSA), eip => ResetEIP}}),
    ?event(snp_short, {vmsa_pages_created, #{bsp_size => byte_size(BSPVMSA), ap_size => byte_size(APVMSA)}}),
    {BSPVMSA, APVMSA}.

%% Create a single VMSA page
%% Matching Rust build_save_area() function exactly
%%
%% Rust sets the following fields (all others remain at default/zero):
%% - Segment registers (all VmcbSeg: selector, attrib, limit, base):
%%   - es: (0, 0x93, 0xffff, 0)
%%   - cs: (0xf000, cs_flags, 0xffff, eip & 0xffff0000)
%%   - ss: (0, ss_flags, 0xffff, 0)
%%   - ds: (0, 0x93, 0xffff, 0)
%%   - fs: (0, 0x93, 0xffff, 0)
%%   - gs: (0, 0x93, 0xffff, 0)
%%   - gdtr: (0, 0, 0xffff, 0)
%%   - idtr: (0, 0, 0xffff, 0)
%%   - ldtr: (0, 0x82, 0xffff, 0)
%%   - tr: (0, tr_flags, 0xffff, 0)
%% - Control registers:
%%   - efer: 0x1000
%%   - cr4: 0x40
%%   - cr0: 0x10
%%   - dr7: 0x400
%%   - dr6: 0xffff0ff0
%%   - rflags: 0x2
%%   - rip: eip & 0xffff
%% - Other fields:
%%   - g_pat: 0x7040600070406
%%   - rdx: rdx (from vcpu_type.sig() or 0)
%%   - sev_features: guest_features.0
%%   - xcr0: 0x1
%%   - mxcsr: mxcsr (from vmm_type)
%%   - x87_fcw: fcw (from vmm_type)
%%
%% Note: All other fields remain at their default values (zeros).
%% The struct is initialized with SevEsSaveArea::default() which zeros everything.
-spec create_vmsa_page_erlang(EIP :: non_neg_integer(), VCPUType :: integer(), 
    VMMType :: integer(), GuestFeatures :: non_neg_integer()) -> binary().
create_vmsa_page_erlang(EIP, VCPUType, VMMType, GuestFeatures) ->
    % Determine if this is BSP or AP based on EIP
    VMSAType = if EIP =:= ?BSP_EIP -> <<"BSP">>; true -> <<"AP">> end,
    ?event(snp, {create_vmsa_page_start, #{eip => EIP, vmsa_type => VMSAType, guest_features => GuestFeatures, vcpu_type => VCPUType, vmm_type => VMMType}}),
    % Initialize VMSA page with all zeros (?PAGE_SIZE bytes)
    VMSA = <<0:(?PAGE_SIZE * 8)>>,
    ?event(snp, {vmsa_initialized, #{size => byte_size(VMSA)}}),
    
    % Determine flags and values based on VMMType (matching Rust)
    {CSFlags, SSFlags, TRFlags, RDXValue, MXCSRValue, FCWValue} = determine_vmm_flags(EIP, VCPUType, VMMType),
    
    % Log all field values we're setting (matching Rust build_save_area)
    ?event(snp, {vmsa_field_values_set, {explicit, #{
        % Segment registers (VmcbSeg: selector, attrib, limit, base)
        es => #{selector => 0, attrib => ?VMSA_SEGMENT_ATTRIB_ES, limit => ?VMSA_SEGMENT_LIMIT, base => 0},
        cs => #{selector => ?VMSA_CS_SELECTOR, attrib => CSFlags, limit => ?VMSA_SEGMENT_LIMIT, base => (EIP band ?EIP_UPPER_16_MASK)},
        ss => #{selector => 0, attrib => SSFlags, limit => ?VMSA_SEGMENT_LIMIT, base => 0},
        ds => #{selector => 0, attrib => ?VMSA_SEGMENT_ATTRIB_DS, limit => ?VMSA_SEGMENT_LIMIT, base => 0},
        fs => #{selector => 0, attrib => ?VMSA_SEGMENT_ATTRIB_FS, limit => ?VMSA_SEGMENT_LIMIT, base => 0},
        gs => #{selector => 0, attrib => ?VMSA_SEGMENT_ATTRIB_GS, limit => ?VMSA_SEGMENT_LIMIT, base => 0},
        gdtr => #{selector => 0, attrib => 0, limit => ?VMSA_SEGMENT_LIMIT, base => 0},
        idtr => #{selector => 0, attrib => 0, limit => ?VMSA_SEGMENT_LIMIT, base => 0},
        ldtr => #{selector => 0, attrib => ?VMSA_SEGMENT_ATTRIB_LDTR, limit => ?VMSA_SEGMENT_LIMIT, base => 0},
        tr => #{selector => 0, attrib => TRFlags, limit => ?VMSA_SEGMENT_LIMIT, base => 0},
        % Control registers
        efer => ?VMSA_EFER_VALUE,
        cr4 => ?VMSA_CR4_VALUE,
        cr0 => ?VMSA_CR0_VALUE,
        dr7 => ?VMSA_DR7_VALUE,
        dr6 => ?VMSA_DR6_VALUE,
        rflags => ?VMSA_RFLAGS_VALUE,
        rip => (EIP band ?EIP_LOWER_16_MASK),
        % Other fields
        g_pat => ?VMSA_G_PAT_VALUE,
        rdx => RDXValue,
        sev_features => GuestFeatures,
        xcr0 => ?VMSA_XCR0_VALUE,
        mxcsr => MXCSRValue,
        x87_fcw => FCWValue
    }}}),
    
    % Match Rust: area.rip = eip & 0xffff (lower 16 bits only)
    RIPValue = EIP band ?EIP_LOWER_16_MASK,
    % Match Rust: area.cs.base = eip & 0xffff0000 (upper 16 bits to CS base)
    CSBaseValue = EIP band ?EIP_UPPER_16_MASK,
    
    % Set all segment registers
    VMSA10 = set_all_vmsa_segments(VMSA, EIP, CSFlags, SSFlags, TRFlags),
    
    % Set all control registers
    VMSA17 = set_all_vmsa_control_registers(VMSA10, RIPValue),
    
    % Set all other fields
    VMSA23 = set_all_vmsa_other_fields(VMSA17, RDXValue, GuestFeatures, MXCSRValue, FCWValue),
    
    % Verify and log all critical field values
    % Read back key fields to verify they were set correctly
    <<_BeforeRIP:(?VMSA_OFFSET_RIP)/binary, RIPReadBack:64/little, _AfterRIP/binary>> = VMSA23,
    <<_BeforeRDX:(?VMSA_OFFSET_RDX)/binary, RDXReadBack:64/little, _AfterRDX/binary>> = VMSA23,
    <<_BeforeSEV:(?VMSA_OFFSET_SEV_FEATURES)/binary, SEVReadBack:64/little, _AfterSEV/binary>> = VMSA23,
    <<_BeforeXCR0:(?VMSA_OFFSET_XCR0)/binary, XCR0ReadBack:64/little, _AfterXCR0/binary>> = VMSA23,
    <<_BeforeMXCSR:(?VMSA_OFFSET_MXCSR)/binary, MXCSRReadBack:32/little, _AfterMXCSR/binary>> = VMSA23,
    <<_BeforeFCW:(?VMSA_OFFSET_X87_FCW)/binary, FCWReadBack:16/little, _AfterFCW/binary>> = VMSA23,
    <<_BeforeG_PAT:(?VMSA_OFFSET_G_PAT)/binary, G_PATReadBack:64/little, _AfterG_PAT/binary>> = VMSA23,
    <<_BeforeEFER:(?VMSA_OFFSET_EFER)/binary, EFERReadBack:64/little, _AfterEFER/binary>> = VMSA23,
    <<_BeforeCR4:(?VMSA_OFFSET_CR4)/binary, CR4ReadBack:64/little, _AfterCR4/binary>> = VMSA23,
    <<_BeforeCR0:(?VMSA_OFFSET_CR0)/binary, CR0ReadBack:64/little, _AfterCR0/binary>> = VMSA23,
    <<_BeforeRFLAGS:(?VMSA_OFFSET_RFLAGS)/binary, RFLAGSReadBack:64/little, _AfterRFLAGS/binary>> = VMSA23,
    
    % Read CS segment to verify CS base
    <<_BeforeCS:(?VMSA_OFFSET_CS)/binary, CSSelector:16/little, CSAttrib:16/little, CSLimit:32/little, CSBase:64/little, _AfterCS/binary>> = VMSA23,
    
    ?event(snp, {vmsa_field_verification, {explicit, #{
        % Segment registers
        cs_selector => CSSelector,
        cs_attrib => CSAttrib,
        cs_limit => CSLimit,
        cs_base_expected => CSBaseValue,
        cs_base_read_back => CSBase,
        cs_base_match => CSBaseValue =:= CSBase,
        % Control registers
        efer_expected => ?VMSA_EFER_VALUE,
        efer_read_back => EFERReadBack,
        efer_match => ?VMSA_EFER_VALUE =:= EFERReadBack,
        cr4_expected => ?VMSA_CR4_VALUE,
        cr4_read_back => CR4ReadBack,
        cr4_match => ?VMSA_CR4_VALUE =:= CR4ReadBack,
        cr0_expected => ?VMSA_CR0_VALUE,
        cr0_read_back => CR0ReadBack,
        cr0_match => ?VMSA_CR0_VALUE =:= CR0ReadBack,
        rflags_expected => ?VMSA_RFLAGS_VALUE,
        rflags_read_back => RFLAGSReadBack,
        rflags_match => ?VMSA_RFLAGS_VALUE =:= RFLAGSReadBack,
        % RIP
        eip_expected => EIP,
        rip_expected => RIPValue,
        rip_read_back => RIPReadBack,
        rip_match => RIPValue =:= RIPReadBack,
        % Other fields
        g_pat_expected => ?VMSA_G_PAT_VALUE,
        g_pat_read_back => G_PATReadBack,
        g_pat_match => ?VMSA_G_PAT_VALUE =:= G_PATReadBack,
        rdx_expected => RDXValue,
        rdx_read_back => RDXReadBack,
        rdx_match => RDXValue =:= RDXReadBack,
        sev_features_expected => GuestFeatures,
        sev_features_read_back => SEVReadBack,
        sev_features_match => GuestFeatures =:= SEVReadBack,
        xcr0_expected => ?VMSA_XCR0_VALUE,
        xcr0_read_back => XCR0ReadBack,
        xcr0_match => ?VMSA_XCR0_VALUE =:= XCR0ReadBack,
        mxcsr_expected => MXCSRValue,
        mxcsr_read_back => MXCSRReadBack,
        mxcsr_match => MXCSRValue =:= MXCSRReadBack,
        x87_fcw_expected => FCWValue,
        x87_fcw_read_back => FCWReadBack,
        x87_fcw_match => FCWValue =:= FCWReadBack
    }}}),
    
    % Log key byte ranges for comparison with Rust
    % CS base (offset 0x18-0x1F, which is CS base field within CS segment)
    CSBaseOffset = ?VMSA_OFFSET_CS + 8,  % CS base is at offset 8 within CS segment (16 bytes total)
    <<_BeforeCSBase:CSBaseOffset/binary, CSBaseBytes:8/binary, _AfterCSBase/binary>> = VMSA23,
    % EFER (offset ?VMSA_OFFSET_EFER)
    <<_BeforeEFERBytes:(?VMSA_OFFSET_EFER)/binary, EFERBytes:8/binary, _AfterEFERBytes/binary>> = VMSA23,
    % CR4 (offset ?VMSA_OFFSET_CR4)
    <<_BeforeCR4Bytes:(?VMSA_OFFSET_CR4)/binary, CR4Bytes:8/binary, _AfterCR4Bytes/binary>> = VMSA23,
    % RIP (offset ?VMSA_OFFSET_RIP)
    <<_BeforeRIPBytes:(?VMSA_OFFSET_RIP)/binary, RIPBytes:8/binary, _AfterRIPBytes/binary>> = VMSA23,
    % RDX (offset ?VMSA_OFFSET_RDX) - matching Rust comparison output
    <<_BeforeRDXBytes:(?VMSA_OFFSET_RDX)/binary, RDXBytes:8/binary, _AfterRDXBytes/binary>> = VMSA23,
    % SEV Features (offset ?VMSA_OFFSET_SEV_FEATURES) - matching Rust struct
    <<_BeforeSEVBytes:(?VMSA_OFFSET_SEV_FEATURES)/binary, SEVBytes:8/binary, _AfterSEVBytes/binary>> = VMSA23,
    % MXCSR (offset ?VMSA_OFFSET_MXCSR) - matching Rust comparison output
    <<_BeforeMXCSRBytes:(?VMSA_OFFSET_MXCSR)/binary, MXCSRBytes:4/binary, _AfterMXCSRBytes/binary>> = VMSA23,
    % X87 FCW (offset ?VMSA_OFFSET_X87_FCW) - matching Rust comparison output
    <<_BeforeFCWBytes:(?VMSA_OFFSET_X87_FCW)/binary, FCWBytes:2/binary, _AfterFCWBytes/binary>> = VMSA23,
    
    % Compute hash of VMSA page for verification (don't log full binary dumps)
    VMSAHash = crypto:hash(sha384, VMSA23),
    
    % Log key field hashes instead of full values for security
    KeyFieldsHash = crypto:hash(sha256, <<CSBaseBytes/binary, EFERBytes/binary, CR4Bytes/binary, 
                                         RIPBytes/binary, RDXBytes/binary, SEVBytes/binary,
                                         MXCSRBytes/binary, FCWBytes/binary>>),
    ?event(snp, {vmsa_key_fields_summary, #{
        key_fields_hash => snp_util:binary_to_hex_string(KeyFieldsHash),
        eip => EIP
    }}),
    
    % Determine if this is BSP or AP based on EIP
    VMSAType = if EIP =:= ?BSP_EIP -> <<"BSP">>; true -> <<"AP">> end,
    ?event(snp_short, {create_vmsa_page_complete, #{
        vmsa_type => VMSAType,
        size => byte_size(VMSA23),
        vmsa_hash_hex => snp_util:binary_to_hex_string(VMSAHash),
        eip => EIP,
        % Log all field values we set for comparison
        field_values => #{
            cs_flags => CSFlags,
            ss_flags => SSFlags,
            tr_flags => TRFlags,
            rdx_value => RDXValue,
            mxcsr_value => MXCSRValue,
            fcw_value => FCWValue,
            rip_value => RIPValue,
            cs_base_value => CSBaseValue,
            guest_features => GuestFeatures
        },
        % Dump the full VMSA page for byte-by-byte comparison with Rust
        full_vmsa_page_hex => snp_util:binary_to_hex_string(VMSA23)
    }}),
    VMSA23.

%% Helper function to determine VMM flags and values
-spec determine_vmm_flags(EIP :: non_neg_integer(), VCPUType :: integer(), VMMType :: integer()) ->
    {integer(), integer(), integer(), integer(), integer(), integer()}.
determine_vmm_flags(EIP, VCPUType, VMMType) ->
    case VMMType of
        ?VMM_TYPE_QEMU -> % VMMType::QEMU
            % For QEMU: (?VMM_QEMU_CS_FLAGS, ?VMM_QEMU_SS_FLAGS, ?VMM_QEMU_TR_FLAGS, vcpu_type.sig(), ?VMM_QEMU_MXCSR, ?VMM_QEMU_FCW)
            VCPUSig = get_vcpu_sig(VCPUType),
            {?VMM_QEMU_CS_FLAGS, ?VMM_QEMU_SS_FLAGS, ?VMM_QEMU_TR_FLAGS, VCPUSig, ?VMM_QEMU_MXCSR, ?VMM_QEMU_FCW};
        ?VMM_TYPE_EC2 -> % VMMType::EC2
            % For EC2: depends on EIP
            if EIP =:= ?BSP_EIP ->
                {?VMM_EC2_BSP_CS_FLAGS, ?VMM_EC2_BSP_SS_FLAGS, ?VMM_EC2_BSP_TR_FLAGS, 0, 0, 0};
            true ->
                {?VMM_EC2_AP_CS_FLAGS, ?VMM_EC2_AP_SS_FLAGS, ?VMM_EC2_AP_TR_FLAGS, 0, 0, 0}
            end;
        _ -> % Default/other
            {?VMM_QEMU_CS_FLAGS, ?VMM_QEMU_SS_FLAGS, ?VMM_QEMU_TR_FLAGS, 0, ?VMM_QEMU_MXCSR, ?VMM_QEMU_FCW}
    end.

%% Helper function to set all VMSA segment registers
-spec set_all_vmsa_segments(VMSA :: binary(), EIP :: non_neg_integer(), CSFlags :: integer(), 
    SSFlags :: integer(), TRFlags :: integer()) -> binary().
set_all_vmsa_segments(VMSA, EIP, CSFlags, SSFlags, TRFlags) ->
    CSBaseValue = EIP band ?EIP_UPPER_16_MASK,
    VMSA1 = set_vmsa_segment(VMSA, ?VMSA_OFFSET_ES, 0, ?VMSA_SEGMENT_ATTRIB_ES, ?VMSA_SEGMENT_LIMIT, 0),
    VMSA2 = set_vmsa_segment(VMSA1, ?VMSA_OFFSET_CS, ?VMSA_CS_SELECTOR, CSFlags, ?VMSA_SEGMENT_LIMIT, CSBaseValue),
    VMSA3 = set_vmsa_segment(VMSA2, ?VMSA_OFFSET_SS, 0, SSFlags, ?VMSA_SEGMENT_LIMIT, 0),
    VMSA4 = set_vmsa_segment(VMSA3, ?VMSA_OFFSET_DS, 0, ?VMSA_SEGMENT_ATTRIB_DS, ?VMSA_SEGMENT_LIMIT, 0),
    VMSA5 = set_vmsa_segment(VMSA4, ?VMSA_OFFSET_FS, 0, ?VMSA_SEGMENT_ATTRIB_FS, ?VMSA_SEGMENT_LIMIT, 0),
    VMSA6 = set_vmsa_segment(VMSA5, ?VMSA_OFFSET_GS, 0, ?VMSA_SEGMENT_ATTRIB_GS, ?VMSA_SEGMENT_LIMIT, 0),
    VMSA7 = set_vmsa_segment(VMSA6, ?VMSA_OFFSET_GDTR, 0, 0, ?VMSA_SEGMENT_LIMIT, 0),
    VMSA8 = set_vmsa_segment(VMSA7, ?VMSA_OFFSET_LDTR, 0, ?VMSA_SEGMENT_ATTRIB_LDTR, ?VMSA_SEGMENT_LIMIT, 0),
    VMSA9 = set_vmsa_segment(VMSA8, ?VMSA_OFFSET_IDTR, 0, 0, ?VMSA_SEGMENT_LIMIT, 0),
    set_vmsa_segment(VMSA9, ?VMSA_OFFSET_TR, 0, TRFlags, ?VMSA_SEGMENT_LIMIT, 0).

%% Helper function to set all VMSA control registers
-spec set_all_vmsa_control_registers(VMSA :: binary(), RIPValue :: non_neg_integer()) -> binary().
set_all_vmsa_control_registers(VMSA, RIPValue) ->
    VMSA1 = set_vmsa_field(VMSA, ?VMSA_OFFSET_EFER, ?VMSA_EFER_VALUE, 8),
    VMSA2 = set_vmsa_field(VMSA1, ?VMSA_OFFSET_CR4, ?VMSA_CR4_VALUE, 8),
    VMSA3 = set_vmsa_field(VMSA2, ?VMSA_OFFSET_CR0, ?VMSA_CR0_VALUE, 8),
    VMSA4 = set_vmsa_field(VMSA3, ?VMSA_OFFSET_DR7, ?VMSA_DR7_VALUE, 8),
    VMSA5 = set_vmsa_field(VMSA4, ?VMSA_OFFSET_DR6, ?VMSA_DR6_VALUE, 8),
    VMSA6 = set_vmsa_field(VMSA5, ?VMSA_OFFSET_RFLAGS, ?VMSA_RFLAGS_VALUE, 8),
    set_vmsa_field(VMSA6, ?VMSA_OFFSET_RIP, RIPValue, 8).

%% Helper function to set all VMSA other fields
-spec set_all_vmsa_other_fields(VMSA :: binary(), RDXValue :: integer(), GuestFeatures :: integer(),
    MXCSRValue :: integer(), FCWValue :: integer()) -> binary().
set_all_vmsa_other_fields(VMSA, RDXValue, GuestFeatures, MXCSRValue, FCWValue) ->
    VMSA1 = set_vmsa_field(VMSA, ?VMSA_OFFSET_G_PAT, ?VMSA_G_PAT_VALUE, 8),
    VMSA2 = set_vmsa_field(VMSA1, ?VMSA_OFFSET_RDX, RDXValue, 8),
    VMSA3 = set_vmsa_field(VMSA2, ?VMSA_OFFSET_SEV_FEATURES, GuestFeatures, 8),
    VMSA4 = set_vmsa_field(VMSA3, ?VMSA_OFFSET_XCR0, ?VMSA_XCR0_VALUE, 8),
    VMSA5 = set_vmsa_field(VMSA4, ?VMSA_OFFSET_MXCSR, MXCSRValue, 4),
    set_vmsa_field(VMSA5, ?VMSA_OFFSET_X87_FCW, FCWValue, 2).

%% Set a VmcbSeg segment register (16 bytes: selector:u16, attrib:u16, limit:u32, base:u64)
-spec set_vmsa_segment(VMSA :: binary(), Offset :: non_neg_integer(), Selector :: integer(), 
    Attrib :: integer(), Limit :: integer(), Base :: non_neg_integer()) -> binary().
set_vmsa_segment(VMSA, Offset, Selector, Attrib, Limit, Base) ->
    % VmcbSeg structure: selector (2 bytes), attrib (2 bytes), limit (4 bytes), base (8 bytes)
    <<Before:Offset/binary, _:16/binary, After/binary>> = VMSA,
    Segment = <<Selector:16/little, Attrib:16/little, Limit:32/little, Base:64/little>>,
    % Log the segment bytes we're creating for debugging
    ?event(snp, {set_vmsa_segment_bytes, #{
        offset => Offset,
        selector => Selector,
        attrib => Attrib,
        limit => Limit,
        base => Base,
        segment_bytes_hex => snp_util:binary_to_hex_string(Segment)
    }}),
    Result = <<Before/binary, Segment/binary, After/binary>>,
    % Verify the segment was set correctly by reading it back
    <<_BeforeRead:Offset/binary, ReadSelector:16/little, ReadAttrib:16/little, ReadLimit:32/little, ReadBase:64/little, _AfterRead/binary>> = Result,
    ?event(snp, {set_vmsa_segment_verification, #{
        offset => Offset,
        selector_match => Selector =:= ReadSelector,
        attrib_match => Attrib =:= ReadAttrib,
        limit_match => Limit =:= ReadLimit,
        base_match => Base =:= ReadBase
    }}),
    Result.

%% Get CPU signature for VCPU type (matching Rust cpu_sig function exactly)
%% Rust: cpu_sig(family, model, stepping) = 
%%   if family > 0xf:
%%     family_low = 0xf, family_high = (family - 0x0f) & 0xff
%%   else:
%%     family_low = family, family_high = 0
%%   model_low = model & 0xf, model_high = (model >> 4) & 0xf
%%   stepping_low = stepping & 0xf
%%   result = (family_high << 20) | (model_high << 16) | (family_low << 8) | (model_low << 4) | stepping_low
-spec get_vcpu_sig(VCPUType :: integer()) -> integer().
get_vcpu_sig(VCPUType) ->
    case VCPUType of
        0 -> % Epyc = cpu_sig(23, 1, 2)
            cpu_sig(23, 1, 2);
        1 -> % EpycV1 = cpu_sig(23, 1, 2)
            cpu_sig(23, 1, 2);
        3 -> % EpycIBPB = cpu_sig(23, 1, 2)
            cpu_sig(23, 1, 2);
        4 -> % EpycV3 = cpu_sig(23, 1, 2)
            cpu_sig(23, 1, 2);
        5 -> % EpycV4 = cpu_sig(23, 1, 2)
            cpu_sig(23, 1, 2);
        6 -> % EpycRome = cpu_sig(23, 49, 0)
            cpu_sig(23, 49, 0);
        7 -> % EpycRomeV1 = cpu_sig(23, 49, 0)
            cpu_sig(23, 49, 0);
        8 -> % EpycRomeV2 = cpu_sig(23, 49, 0)
            cpu_sig(23, 49, 0);
        9 -> % EpycRomeV3 = cpu_sig(23, 49, 0)
            cpu_sig(23, 49, 0);
        10 -> % EpycMilan = cpu_sig(25, 1, 1)
            cpu_sig(25, 1, 1);
        11 -> % EpycMilanV1 = cpu_sig(25, 1, 1)
            cpu_sig(25, 1, 1);
        12 -> % EpycMilanV2 = cpu_sig(25, 1, 1)
            cpu_sig(25, 1, 1);
        13 -> % EpycGenoa = cpu_sig(25, 17, 0)
            cpu_sig(25, 17, 0);
        14 -> % EpycGenoaV1 = cpu_sig(25, 17, 0)
            cpu_sig(25, 17, 0);
        _ -> % Default to Epyc signature
            cpu_sig(23, 1, 2)
    end.

%% Calculate CPU signature (matching Rust cpu_sig function exactly)
-spec cpu_sig(Family :: integer(), Model :: integer(), Stepping :: integer()) -> integer().
cpu_sig(Family, Model, Stepping) ->
    {FamilyLow, FamilyHigh} = if
        Family > 16#F ->
            {16#F, (Family - 16#F) band 16#FF};
        true ->
            {Family, 0}
    end,
    ModelLow = Model band 16#F,
    ModelHigh = (Model bsr 4) band 16#F,
    SteppingLow = Stepping band 16#F,
    (FamilyHigh bsl 20) bor (ModelHigh bsl 16) bor (FamilyLow bsl 8) bor (ModelLow bsl 4) bor SteppingLow.

%% Set a field in VMSA page
-spec set_vmsa_field(VMSA :: binary(), Offset :: non_neg_integer(), Value :: integer(), Size :: non_neg_integer()) -> binary().
set_vmsa_field(VMSA, Offset, Value, Size) when 
    is_binary(VMSA), 
    Offset >= 0, 
    Size > 0, 
    Offset + Size =< byte_size(VMSA) ->
    ?event(snp, {set_vmsa_field_valid, #{offset => Offset, size => Size, value => Value, vmsa_size => byte_size(VMSA)}}),
    <<Before:Offset/binary, _:Size/binary, After/binary>> = VMSA,
    Result = <<Before/binary, Value:(Size*8)/little, After/binary>>,
    % Verify the value was set correctly by reading it back
    <<_BeforeRead:Offset/binary, ReadValue:(Size*8)/little, _AfterRead/binary>> = Result,
    ?event(snp, {set_vmsa_field_complete, #{
        result_size => byte_size(Result),
        value_written => Value,
        value_read_back => ReadValue,
        match => Value =:= ReadValue
    }}),
    Result;
set_vmsa_field(VMSA, Offset, Value, Size) ->
    % Return original VMSA if offset/size is invalid
    ?event(snp_error, {set_vmsa_field_invalid, #{
        offset => Offset, 
        size => Size, 
        value => Value, 
        vmsa_size => case is_binary(VMSA) of true -> byte_size(VMSA); false -> undefined end
    }}),
    VMSA.

