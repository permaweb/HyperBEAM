%%% @doc Constants for SNP commitment reports.
%%%
%%% This file contains all numeric constants used across SNP modules to avoid
%%% magic numbers and improve maintainability.

%% Report structure sizes
-define(REPORT_SIZE, 1184).              % Total SNP report size in bytes
-define(REPORT_MAIN_PORTION_SIZE, 1016).  % Size of main portion before signature
-define(REPORT_SIGNATURE_SIZE, 168).      % Signature portion size (72 + 72 + 24)

%% Page and memory sizes
-define(PAGE_SIZE, 4096).                 % Standard page size in bytes (4KB)
-define(LAUNCH_DIGEST_SIZE, 48).          % Launch digest size in bytes (SHA-384)
-define(LAUNCH_DIGEST_BITS, 384).         % Launch digest size in bits (48 * 8)
-define(CHIP_ID_SIZE, 64).                 % Chip ID size in bytes

%% Hash sizes
-define(SHA256_SIZE, 32).                 % SHA-256 hash size in bytes
-define(SHA384_SIZE, 48).                 % SHA-384 hash size in bytes
-define(HEX_STRING_48_BYTES, 96).          % Hex string length for 48-byte hash

%% Page info structure
-define(PAGE_INFO_LEN, 112).              % Page info structure size (0x70 bytes)

%% Memory addresses and masks
-define(FOUR_GB, 16#100000000).           % 4GB address (0x100000000)
-define(PAGE_MASK, 16#FFF).               % Page offset mask (4KB alignment)
-define(BSP_EIP, 16#FFFFFFFFF0).         % BSP EIP value (0xffff_fff0)

%% VMSA page structure offsets (in hex for clarity)
-define(VMSA_OFFSET_ES, 16#0).            % ES segment register offset
-define(VMSA_OFFSET_CS, 16#10).           % CS segment register offset
-define(VMSA_OFFSET_SS, 16#20).           % SS segment register offset
-define(VMSA_OFFSET_DS, 16#30).           % DS segment register offset
-define(VMSA_OFFSET_FS, 16#40).           % FS segment register offset
-define(VMSA_OFFSET_GS, 16#50).           % GS segment register offset
-define(VMSA_OFFSET_GDTR, 16#60).        % GDTR segment register offset
-define(VMSA_OFFSET_LDTR, 16#70).        % LDTR segment register offset
-define(VMSA_OFFSET_IDTR, 16#80).        % IDTR segment register offset
-define(VMSA_OFFSET_TR, 16#90).           % TR segment register offset
-define(VMSA_OFFSET_EFER, 16#D0).        % EFER control register offset
-define(VMSA_OFFSET_CR4, 16#148).        % CR4 control register offset
-define(VMSA_OFFSET_CR0, 16#158).        % CR0 control register offset
-define(VMSA_OFFSET_DR7, 16#160).        % DR7 control register offset
-define(VMSA_OFFSET_DR6, 16#168).        % DR6 control register offset
-define(VMSA_OFFSET_RFLAGS, 16#170).     % RFLAGS control register offset
-define(VMSA_OFFSET_RIP, 16#178).        % RIP control register offset
-define(VMSA_OFFSET_G_PAT, 16#268).      % G_PAT register offset
-define(VMSA_OFFSET_RDX, 16#310).        % RDX register offset
-define(VMSA_OFFSET_SEV_FEATURES, 16#3B0). % SEV features register offset
-define(VMSA_OFFSET_XCR0, 16#3E8).       % XCR0 register offset
-define(VMSA_OFFSET_MXCSR, 16#408).      % MXCSR register offset
-define(VMSA_OFFSET_X87_FCW, 16#410).    % X87 FCW register offset

%% VMSA register values
-define(VMSA_EFER_VALUE, 16#1000).       % EFER register value
-define(VMSA_CR4_VALUE, 16#40).          % CR4 register value
-define(VMSA_CR0_VALUE, 16#10).          % CR0 register value
-define(VMSA_DR7_VALUE, 16#400).        % DR7 register value
-define(VMSA_DR6_VALUE, 16#FFFF0FF0).    % DR6 register value
-define(VMSA_RFLAGS_VALUE, 16#2).        % RFLAGS register value
-define(VMSA_G_PAT_VALUE, 16#7040600070406). % G_PAT register value
-define(VMSA_XCR0_VALUE, 16#1).         % XCR0 register value
-define(VMSA_CS_SELECTOR, 16#F000).      % CS selector value
-define(VMSA_SEGMENT_LIMIT, 16#FFFF).    % Standard segment limit value
-define(VMSA_SEGMENT_ATTRIB_ES, 16#93).  % ES segment attribute
-define(VMSA_SEGMENT_ATTRIB_DS, 16#93).  % DS segment attribute
-define(VMSA_SEGMENT_ATTRIB_FS, 16#93).  % FS segment attribute
-define(VMSA_SEGMENT_ATTRIB_GS, 16#93).  % GS segment attribute
-define(VMSA_SEGMENT_ATTRIB_LDTR, 16#82). % LDTR segment attribute

%% VMSA GPA
-define(VMSA_GPA, 16#FFFFFFFFF000).      % VMSA page GPA

%% Page type constants
-define(PAGE_TYPE_NORMAL, 1).            % Normal page type
-define(PAGE_TYPE_VMSA, 2).              % VMSA page type
-define(PAGE_TYPE_ZERO, 3).              % Zero page type
-define(PAGE_TYPE_SVSM_CAA, 4).          % SVSM CAA page type
-define(PAGE_TYPE_SECRETS, 5).           % Secrets page type
-define(PAGE_TYPE_CPUID, 6).             % CPUID page type

%% SEV hash table constants
-define(SEV_HASH_TABLE_ENTRY_LENGTH, 50). % SEV hash table entry length
-define(SEV_HASH_TABLE_SIZE, 168).        % SEV hash table total size
-define(SEV_HASH_TABLE_PADDING, 8).       % SEV hash table padding size

%% SPL value limits
-define(MAX_SPL_VALUE, 255).              % Maximum SPL value (u8)

%% Report data version
-define(REPORT_DATA_VERSION, 1).          % Report data version

%% Signature component sizes
-define(SIGNATURE_R_SIZE, 72).            % Signature R component size in bytes
-define(SIGNATURE_S_SIZE, 72).            % Signature S component size in bytes
-define(SIGNATURE_RESERVED_SIZE, 24).     % Signature reserved area size in bytes
-define(SIGNATURE_RESERVED_BITS, 192).    % Signature reserved area size in bits (24 * 8)
-define(RESERVED1_SIZE, 24).              % Reserved1 field size in bytes
-define(RESERVED1_BITS, 192).            % Reserved1 field size in bits (24 * 8)
-define(RESERVED4_BITS, 1344).            % Reserved4 field size in bits (168 * 8)

%% OVMF footer table constants
-define(OVMF_ENTRY_HEADER_SIZE, 18).      % OVMF entry header size (2 bytes size + 16 bytes GUID)
-define(OVMF_DESCRIPTOR_SIZE, 12).        % OVMF metadata section descriptor size
-define(OVMF_FOOTER_OFFSET, 32).          % OVMF footer table offset from end of file

%% Configuration constants
-define(COMMITTED_PARAMETERS, [vcpus, vcpu_type, vmm_type, guest_features,
    firmware, kernel, initrd, append]).  % Parameters committed in SNP reports
%% Guest policy DEBUG bit (AMD SEV-SNP): policy.DEBUG=1 => debug VM, 0 => production.
%% Use this bit only; do not infer debug from TCB/SVN. Report must be verified (signature + chain) first.
-define(DEBUG_FLAG_BIT, 19).              % Bit position of DEBUG in SNP guest policy (u64)
-define(SNP_GUEST_POLICY_DEBUG, (1 bsl ?DEBUG_FLAG_BIT)).  % Mask for C-style (report.policy & SNP_GUEST_POLICY_DEBUG)

%% TCB structure offsets
-define(TCB_OFFSET_BOOTLOADER, 0).        % Bootloader SPL offset in TCB structure
-define(TCB_OFFSET_TEE, 1).                % TEE SPL offset in TCB structure
-define(TCB_OFFSET_SNP, 6).                % SNP SPL offset in TCB structure (skips reserved bytes 2-5)
-define(TCB_OFFSET_MICROCODE, 7).          % Microcode SPL offset in TCB structure
-define(TCB_RESERVED_BYTES, 4).            % Reserved bytes in TCB structure (bytes 2-5)
-define(TCB_SIZE, 8).                      % Total TCB structure size in bytes

%% Report field sizes
-define(FAMILY_ID_SIZE, 16).              % Family ID size in bytes
-define(IMAGE_ID_SIZE, 16).                % Image ID size in bytes
-define(HOST_DATA_SIZE, 32).               % Host data size in bytes
-define(REPORT_ID_SIZE, 32).               % Report ID size in bytes

%% Signature reserved area
-define(SIGNATURE_RESERVED_TOTAL_SIZE, 368). % Total signature reserved area (includes padding after R+S)

%% Signature verification constants
-define(SIGNATURE_PORTION_SIZE, 144).     % Signature portion size (72 + 72 bytes)
-define(SIGNATURE_R_BITS, 576).            % Signature R size in bits (72 * 8)
-define(SIGNATURE_S_BITS, 576).            % Signature S size in bits (72 * 8)

%% HTTP constants
-define(HTTP_PORT_HTTPS, 443).            % HTTPS default port
-define(HTTP_PORT_HTTP, 80).             % HTTP default port
-define(HTTP_STATUS_OK, 200).             % HTTP success status code

%% Certificate constants
-define(CERT_CHAIN_MIN_SIZE, 2).          % Minimum certificates in chain (ASK + ARK)
-define(CERT_SINGLE, 1).                  % Single certificate

%% OVMF parsing constants
-define(OVMF_MIN_FILE_SIZE, 50).          % Minimum OVMF file size for parsing
-define(OVMF_GPA_EIP_SIZE, 4).            % Size of GPA/EIP fields in bytes (u32)

%% OVMF section type constants
-define(OVMF_SECTION_SNP_SEC_MEMORY, 1).  % SnpSecMemory section type
-define(OVMF_SECTION_SNP_SECRETS, 2).     % SnpSecrets section type
-define(OVMF_SECTION_CPUID, 3).           % Cpuid section type
-define(OVMF_SECTION_SVSM_CAA, 4).        % SvsmCaa section type
-define(OVMF_SECTION_SNP_KERNEL_HASHES, 16). % SnpKernelHashes section type (0x10)

%% VMM type constants
-define(VMM_TYPE_QEMU, 1).                % QEMU VMM type
-define(VMM_TYPE_EC2, 2).                  % EC2 VMM type

%% VMM-specific VMSA flags (QEMU)
-define(VMM_QEMU_CS_FLAGS, 16#9B).        % QEMU CS segment flags
-define(VMM_QEMU_SS_FLAGS, 16#93).        % QEMU SS segment flags
-define(VMM_QEMU_TR_FLAGS, 16#8B).        % QEMU TR segment flags
-define(VMM_QEMU_MXCSR, 16#1F80).         % QEMU MXCSR value
-define(VMM_QEMU_FCW, 16#37F).             % QEMU X87 FCW value

%% VMM-specific VMSA flags (EC2)
-define(VMM_EC2_BSP_CS_FLAGS, 16#9A).     % EC2 BSP CS segment flags
-define(VMM_EC2_BSP_SS_FLAGS, 16#92).     % EC2 BSP SS segment flags
-define(VMM_EC2_BSP_TR_FLAGS, 16#83).     % EC2 BSP TR segment flags
-define(VMM_EC2_AP_CS_FLAGS, 16#9B).      % EC2 AP CS segment flags
-define(VMM_EC2_AP_SS_FLAGS, 16#92).       % EC2 AP SS segment flags
-define(VMM_EC2_AP_TR_FLAGS, 16#83).      % EC2 AP TR segment flags

%% EIP bit masks
-define(EIP_LOWER_16_MASK, 16#FFFF).       % Mask for lower 16 bits of EIP
-define(EIP_UPPER_16_MASK, 16#FFFF0000).   % Mask for upper 16 bits of EIP (CS base)

%% Hash size constants for SEV hashes
-define(SEV_HASH_BINARY_SIZE, 32).        % SEV hash binary size (SHA-256)
-define(SEV_HASH_HEX_SIZE, 64).            % SEV hash hex string size (32 bytes * 2)

%% JSON preview size
-define(JSON_PREVIEW_SIZE, 1000).          % Size for JSON preview in logging

%% AMD KDS (Key Distribution Service) constants
-define(KDS_CERT_SITE, "https://kdsintf.amd.com").  % AMD KDS certificate site URL
-define(KDS_VCEK_PATH, "/vcek/v1").                 % AMD KDS VCEK certificate path
-define(DEFAULT_SEV_PRODUCT, "Milan").              % Default SEV product name

%% OVMF metadata constants
-define(OVMF_METADATA_VERSION, 1).                  % OVMF metadata version
-define(OVMF_METADATA_HEADER_SIZE, 16).            % OVMF metadata header size (4 bytes signature + 4 bytes size + 4 bytes version + 4 bytes num_items)
-define(OVMF_METADATA_OFFSET_SIZE, 4).             % OVMF metadata offset field size (u32)

%% Default reset EIP
-define(DEFAULT_RESET_EIP, 0).                      % Default reset EIP value when OVMF parsing fails

%% VMSA area sizes (for debugging/logging)
-define(VMSA_SEGMENT_REGS_AREA_SIZE, 160).         % Segment registers area size (0x0-0x9F)
-define(VMSA_CONTROL_REGS_AREA_SIZE, 304).        % Control registers area size (from EFER offset)
-define(VMSA_GENERAL_REGS_AREA_OFFSET, 16#300).   % General registers area offset
-define(VMSA_GENERAL_REGS_AREA_SIZE, 256).         % General registers area size (0x300-0x3FF)

