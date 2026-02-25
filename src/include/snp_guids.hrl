%%% @doc GUID definitions for SNP commitment reports.
%%%
%%% This file contains all GUID (Globally Unique Identifier) definitions used
%%% across SNP modules. GUIDs are defined in little-endian byte order to match
%%% the Rust implementation.

%% SEV Hash Table GUIDs (from Rust sev_hashes.rs)
%% SEV_HASH_TABLE_HEADER_GUID: 9438d606-4f22-4cc9-b479-a793d411fd21
-define(SEV_HASH_TABLE_HEADER_GUID, <<6, 214, 56, 148, 34, 79, 201, 76,
                                      180, 121, 167, 147, 212, 17, 253, 33>>).

%% SEV_CMDLINE_ENTRY_GUID: 97d02dd8-bd20-4c94-aa78-e7714d36ab2a
-define(SEV_CMDLINE_ENTRY_GUID, <<216, 45, 208, 151, 32, 189, 148, 76,
                                  170, 120, 231, 113, 77, 54, 171, 42>>).

%% SEV_INITRD_ENTRY_GUID: 44baf731-3a2f-4bd7-9af1-41e29169781d
%% Note: Bytes 8-9 swapped to match Rust (9a f1)
-define(SEV_INITRD_ENTRY_GUID, <<49, 247, 186, 68, 47, 58, 215, 75,
                                 154, 241, 65, 226, 145, 105, 120, 29>>).

%% SEV_KERNEL_ENTRY_GUID: 4de79437-abd2-427f-b835-d5b172d2045b
%% Note: Bytes 8-9 swapped to match Rust (b8 35)
-define(SEV_KERNEL_ENTRY_GUID, <<55, 148, 231, 77, 210, 171, 127, 66,
                                 184, 53, 213, 177, 114, 210, 4, 91>>).

%% OVMF GUIDs
%% OVMF_TABLE_FOOTER_GUID: 96b582de-1fb2-45f7-baea-a366c55a082d
-define(OVMF_TABLE_FOOTER_GUID, <<222, 130, 181, 150, 178, 31, 247, 69,
                                  186, 234, 163, 102, 197, 90, 8, 45>>).

%% OVMF_SEV_METADATA_GUID: dc886566-984a-4798-a75e-5585a7bf67cc
-define(OVMF_SEV_METADATA_GUID, <<102, 101, 136, 220, 74, 152, 152, 71,
                                  167, 94, 85, 133, 167, 191, 103, 204>>).

%% SEV_HASH_TABLE_RV_GUID: 7237551f-3a3b-4b04-927b-1da6efa8d454
-define(SEV_HASH_TABLE_RV_GUID, <<31, 55, 85, 114, 59, 58, 4, 75,
                                  146, 123, 29, 166, 239, 168, 212, 84>>).

%% SEV_ES_RESET_BLOCK_GUID: 00f771de-1a7e-4fcb-890e-68c77e2fb44e
-define(SEV_ES_RESET_BLOCK_GUID, <<222, 113, 247, 0, 126, 26, 203, 79,
                                   137, 14, 104, 199, 126, 47, 180, 78>>).

