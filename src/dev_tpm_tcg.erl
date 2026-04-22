%%% @doc TCG event log parser -- pure Erlang, no external deps.
%%%
%%% Parses the binary TCG PC Client event log format
%%% (TCG PC Client Platform Firmware Profile Specification, rev 1.05+)
%%% into AO-Core-native messages. Modern firmware emits the
%%% crypto-agile format, which starts with one legacy TCG_PCR_EVENT
%%% (SpecID) followed by TCG_PCR_EVENT2 records carrying one digest
%%% per algorithm the firmware is using.
%%%
%%% Binary layouts (all little-endian, no padding):
%%%
%%%   TCG_PCR_EVENT (legacy; one per log as the first record):
%%%     pcrIndex    uint32
%%%     eventType   uint32
%%%     digest      20 bytes (SHA-1)
%%%     eventSize   uint32
%%%     event       eventSize bytes
%%%
%%%   TCG_PCR_EVENT2 (crypto-agile):
%%%     pcrIndex    uint32
%%%     eventType   uint32
%%%     digestsCount uint32
%%%     digests:
%%%       hashAlg    uint16
%%%       digest     sizeof(hashAlg)  -- see hash_alg_size/1
%%%     eventSize   uint32
%%%     event       eventSize bytes
%%%
%%%   First record's event bytes hold a TCG_EfiSpecIdEventStruct:
%%%     signature         16 bytes  "Spec ID Event03\0"
%%%     platformClass     uint32
%%%     specVersionMinor  uint8
%%%     specVersionMajor  uint8
%%%     specErrata        uint8
%%%     uintnSize         uint8
%%%     numberOfAlgorithms uint32
%%%     for each:
%%%       algorithmId     uint16
%%%       digestSize      uint16
%%%     vendorInfoSize    uint8
%%%     vendorInfo        vendorInfoSize bytes
%%%
%%% Output shape (for consumers): a map keyed by 1-based sequence
%%% number (binary "1" / "2" / ...), each value an AO-Core message:
%%%
%%%   #{
%%%     <<"seq">>             => integer 1..N
%%%     <<"pcr">>             => integer 0..23
%%%     <<"event-type-code">> => integer (raw TCG code, e.g. 2147483649)
%%%     <<"event-type">>      => binary (human name, e.g.
%%%                              <<"EV_EFI_VARIABLE_DRIVER_CONFIG">>;
%%%                              looked up from priv/event-types.json)
%%%     <<"digests">>         => #{ <<"sha256">> => <<32 bytes>>,
%%%                                 <<"sha1">>   => <<20 bytes>>, ... }
%%%     <<"event-data">>      => raw binary
%%%   }
%%%
%%% Errors: this module never crashes on malformed input. If the log
%%% is truncated or a record can't be parsed, parse/1 returns the
%%% events it was able to decode plus a `#{error => ...}' map at the
%%% end, so callers see "this many events were fine, then something
%%% went wrong."
-module(dev_tpm_tcg).
-export([parse/1, parse/2, event_type_name/1, event_type_name/2,
         decode_event/1, decode_events/1,
         %% UEFI structure helpers (also useful to callers):
         parse_device_path/1, parse_smbios/1, parse_smbios_structure/1,
         parse_acpi_table/1, parse_acpi_rsdp/1,
         %% systemd-stub UKI section awareness:
         systemd_stub_pe_section_pcr/1, is_systemd_stub_pe_section/1,
         %% Linux kernel cmdline tokeniser (paper section Architecture):
         parse_kernel_cmdline/1]).
-include_lib("public_key/include/public_key.hrl").

%%%============================================================================
%%% Public API
%%%============================================================================

%% @doc Parse a TCG event log into a map of 1-indexed AO-Core messages.
%% `Opts' (optional) can carry the event-types registry; otherwise we
%% use the built-in `static_event_types/0' fallback for basic names.
-spec parse(binary()) -> map().
parse(Bin) -> parse(Bin, #{}).

-spec parse(binary(), map()) -> map().
parse(Bin, Opts) when is_binary(Bin) ->
    Registry = event_types_registry(Opts),
    case parse_first_record(Bin) of
        {ok, FirstEv, AlgList, Rest} ->
            {Events, _} = parse_crypto_agile(Rest, AlgList, 2, [FirstEv]),
            Named = [attach_type_name(E, Registry) || E <- Events],
            index_map(Named);
        {error, _} = E ->
            %% Log isn't crypto-agile -- try legacy all-SHA1. Rare in
            %% modern firmware but some embedded setups emit this.
            case parse_all_legacy(Bin, 1, []) of
                {ok, LegacyEvents} ->
                    Named = [attach_type_name(Ev, Registry)
                             || Ev <- LegacyEvents],
                    index_map(Named);
                _ -> #{<<"error">> => fmt_parse_error(E)}
            end
    end;
parse(_, _) -> #{<<"error">> => <<"input is not a binary">>}.

%% @doc Human name for a raw TCG event type code. Returns an
%% "EV_UNKNOWN_0x..." binary if unregistered.
-spec event_type_name(integer()) -> binary().
event_type_name(Code) -> event_type_name(Code, #{}).

event_type_name(Code, Opts) ->
    Registry = event_types_registry(Opts),
    case maps:get(integer_to_binary(Code), Registry, undefined) of
        #{<<"name">> := Name} -> Name;
        _ -> iolist_to_binary(io_lib:format("EV_UNKNOWN_0x~.16B", [Code]))
    end.

%%%============================================================================
%%% Per-event-type decoders
%%%============================================================================
%%%
%%% `decode_event/1' takes a parsed event message (as produced by
%%% `parse/1,2') and returns it with a `parsed' sub-map of
%%% structured fields when the event type is recognised. Unknown
%%% event types are returned unchanged.
%%%
%%% The decoders are defensive: a malformed event body (truncated,
%%% wrong shape for its type) produces `parsed => #{error => ...}'
%%% rather than a crash or a misleading value.
%%%
%%% `decode_events/1' maps decode_event across the map form
%%% produced by `parse/1,2' -- gives callers a one-shot "parse +
%%% decode" pipeline.

-spec decode_events(map()) -> map().
decode_events(Events) when is_map(Events) ->
    maps:map(fun(_K, V) when is_map(V) -> decode_event(V);
                (_K, V) -> V
             end, Events);
decode_events(Other) -> Other.

-spec decode_event(map()) -> map().
decode_event(Event) when is_map(Event) ->
    case maps:get(<<"event-type-code">>, Event, undefined) of
        undefined -> Event;
        Code -> Event#{<<"parsed">> => do_decode(Code, Event)}
    end;
decode_event(E) -> E.

%%%---- M4: Secure Boot variables + firmware CRTM + POST code -----------

%% EV_EFI_VARIABLE_DRIVER_CONFIG (0x80000001)
%% EV_EFI_VARIABLE_BOOT          (0x80000002) -- adds BootOrder /
%%   Boot#### parsing on top of the generic UEFI_VARIABLE_DATA shape
%% EV_EFI_VARIABLE_AUTHORITY     (0x800000E0)
do_decode(16#80000001, Event) -> decode_uefi_variable(Event);
do_decode(16#80000002, Event) -> decode_uefi_variable_boot(Event);
do_decode(16#800000E0, Event) -> decode_uefi_variable(Event);

%% EV_EFI_GPT_EVENT (0x80000006) -- UEFI_GPT_DATA (header + entries).
do_decode(16#80000006, Event) -> decode_uefi_gpt(Event);

%% EV_EFI_HANDOFF_TABLES2 (0x8000000B) -- named handoff table
%% measurement (ACPI / SMBIOS / ...). Extract the descriptive name.
do_decode(16#8000000B, Event) -> decode_handoff_tables2(Event);

%% EV_EVENT_TAG (0x06) -- 128-bit GUID + variable-length data.
%% Expose the GUID; categorise common tags (firmware boot phases,
%% Intel TXT init markers, QEMU-specific tags).
do_decode(16#6, Event) -> decode_event_tag(Event);

%% EV_S_CRTM_VERSION (0x08) -- firmware/CRTM version string.
%% Typically UTF-16LE; occasionally ASCII. Best-effort decode.
do_decode(16#8, Event) -> decode_crtm_version(Event);

%% EV_POST_CODE (0x01) -- firmware POST code; usually short ASCII
%% or manufacturer-defined bytes.
do_decode(16#1, Event) -> decode_post_code(Event);

%%%---- M5: bootloader + UKI + systemd-stub -----------------------------

%% EV_EFI_BOOT_SERVICES_APPLICATION (0x80000003)
%% EV_EFI_BOOT_SERVICES_DRIVER      (0x80000004)
%% EV_EFI_RUNTIME_SERVICES_DRIVER   (0x80000005)
do_decode(16#80000003, Event) -> decode_uefi_image_load(Event);
do_decode(16#80000004, Event) -> decode_uefi_image_load(Event);
do_decode(16#80000005, Event) -> decode_uefi_image_load(Event);

%% EV_IPL (0x0D) -- generic OS-loader event. systemd-stub encodes
%% "key=value" ASCII on PCR 11/12/13; other users encode opaque
%% data. Try key=value, fall back to raw.
do_decode(16#D, Event) -> decode_ev_ipl(Event);

%% EV_EFI_PLATFORM_FIRMWARE_BLOB  (0x80000008)
%% EV_EFI_PLATFORM_FIRMWARE_BLOB2 (0x8000000A) -- with blob description
do_decode(16#80000008, Event) -> decode_firmware_blob(Event);
do_decode(16#8000000A, Event) -> decode_firmware_blob2(Event);

%%%---- M6: remaining TCG codes -----------------------------------------

%% EV_CPU_MICROCODE (0x09) -- microcode update header.
do_decode(16#9, Event) -> decode_cpu_microcode(Event);

%% EV_SEPARATOR (0x04) -- typically 0x00000000 (normal) or
%% 0xFFFFFFFF (firmware reports an error).
do_decode(16#4, Event) -> decode_separator(Event);

%% EV_ACTION (0x05) + EV_EFI_ACTION (0x80000007) -- ASCII action markers.
do_decode(16#5, Event) -> decode_ascii_action(Event);
do_decode(16#80000007, Event) -> decode_ascii_action(Event);

%% EV_EFI_HCRTM_EVENT (0x80000010) -- fixed "HCRTM" ASCII.
do_decode(16#80000010, Event) -> decode_ascii_action(Event);

%% EV_NO_ACTION (0x03) -- first record carries SpecID; others may
%% carry StartupLocality or similar markers.
do_decode(16#3, Event) -> decode_no_action(Event);

%% EV_OMIT_BOOT_DEVICE_EVENTS (0x12) -- ASCII.
do_decode(16#12, Event) -> decode_ascii_action(Event);

%% EV_EFI_HANDOFF_TABLES v1 (0x80000009) -- deprecated in favour of
%% v2, but still emitted by older firmware. Layout:
%%   NumberOfTables  u64 LE
%%   TableEntry[]    {VendorGuid:16B, VendorTable:u64 LE}
do_decode(16#80000009, Event) -> decode_handoff_tables_v1(Event);

%% EV_S_CRTM_CONTENTS (0x07) -- measurement of the firmware blob that
%% bootstrapped the CRTM. Vendor-specific format; most commonly
%% the same UEFI_PLATFORM_FIRMWARE_BLOB shape as 0x80000008 (addr +
%% length), so we try that first and fall back to opaque.
do_decode(16#7, Event) -> decode_crtm_contents(Event);

%% EV_PLATFORM_CONFIG_FLAGS (0x0A) -- vendor-defined flag bits.
%% We surface byte length + SHA-256 of the raw bytes so a policy
%% engine can at least pin the value.
do_decode(16#A, Event) -> decode_platform_config_flags(Event);

%% EV_TABLE_OF_DEVICES (0x0B) -- array of UEFI_DEVICE_PATH.
%% Walk each path with our existing walker.
do_decode(16#B, Event) -> decode_table_of_devices(Event);

%% EV_COMPACT_HASH (0x0C) -- rarely seen compact hash of external
%% data. Pure opaque; surface length only.
do_decode(16#C, Event) -> decode_opaque_with_length(Event);

%% EV_IPL_PARTITION_DATA (0x0E) -- GRUB legacy. Data is typically
%% a NUL-terminated ASCII path like "/boot/grub/grub.cfg" followed
%% by the file's content up to measurement.
do_decode(16#E, Event) -> decode_ipl_partition_data(Event);

%% EV_NONHOST_CODE (0x0F) / _CONFIG (0x10) / _INFO (0x11) -- code /
%% config / info for non-host processors (AMD PSP, Intel ME, etc.).
%% Format is firmware-proprietary; surface the raw SHA-256 so a
%% verifier can pin the value.
do_decode(16#F, Event)  -> decode_nonhost(Event, <<"code">>);
do_decode(16#10, Event) -> decode_nonhost(Event, <<"config">>);
do_decode(16#11, Event) -> decode_nonhost(Event, <<"info">>);

%% TCG PC Client PFP 1.06 additions.
%% EV_POST_CODE2          (0x13) -- UEFI_PLATFORM_FIRMWARE_BLOB2 shape
%% EV_EFI_VARIABLE_BOOT2  (0x8000000C) -- like EV_EFI_VARIABLE_BOOT
%%                          but digest is over a PII-normalised view
%% EV_EFI_GPT_EVENT2      (0x8000000D) -- like EV_EFI_GPT_EVENT but
%%                          digest is GUID/CRC-normalised
do_decode(16#13, Event) -> decode_firmware_blob2(Event);
do_decode(16#8000000C, Event) -> decode_uefi_variable_boot(Event);
do_decode(16#8000000D, Event) -> decode_uefi_gpt(Event);

%% UEFI 2.10 section 32 / PFP 1.06 section 10.5.6 SPDM device-firmware attestation.
%% EV_EFI_SPDM_FIRMWARE_BLOB    (0x800000E1)
%% EV_EFI_SPDM_FIRMWARE_CONFIG  (0x800000E2)
%% EV_EFI_SPDM_DEVICE_POLICY    (0x800000E3)
%% EV_EFI_SPDM_DEVICE_AUTHORITY (0x800000E4)
%% EV_EFI_SPDM_DEVICE_BLOB      (0x800000E5) -- post-1.06 draft
%%
%% SPDM event data is `TCG_DEVICE_SECURITY_EVENT_DATA2`: a header
%% followed by an EFI_DEVICE_PATH (variable-length) followed by
%% SPDM protocol data. We parse the embedded device path (if
%% parseable) + surface the trailing payload length + SHA-256.
do_decode(16#800000E1, Event) -> decode_spdm_event(Event, <<"firmware-blob">>);
do_decode(16#800000E2, Event) -> decode_spdm_event(Event, <<"firmware-config">>);
do_decode(16#800000E3, Event) -> decode_spdm_event(Event, <<"device-policy">>);
do_decode(16#800000E4, Event) -> decode_spdm_event(Event, <<"device-authority">>);
do_decode(16#800000E5, Event) -> decode_spdm_event(Event, <<"device-blob">>);

%% Windows SIPA / WBCL event types (0x10000000 + *).
%% These are used by Microsoft BitLocker / Measured Boot to pin
%% Windows-specific state (secure-boot config, BitLocker PCRs,
%% code integrity). 60+ types. We decode the common shape
%% (each SIPA event has a fixed SIPA_EVENT_HEADER inside the
%% TCG event data).
do_decode(Code, Event) when Code >= 16#10000000, Code =< 16#10FFFFFF ->
    decode_sipa_event(Code, Event);

%% Anything else: no structured decode.
do_decode(_Code, _Event) -> #{}.

%%%---- Decoders (bodies) -----------------------------------------------

%% UEFI_VARIABLE_DATA:
%%   variableName        EFI_GUID (16B)
%%   unicodeNameLength   uint64 LE  (count of UTF-16 chars)
%%   variableDataLength  uint64 LE
%%   unicodeName         [unicodeNameLength] UTF-16LE chars
%%                         (2 * unicodeNameLength bytes)
%%   variableData        [variableDataLength] bytes
decode_uefi_variable(#{<<"event-data">> := Data}) ->
    case Data of
        <<GuidBin:16/binary,
          NameLen:64/unsigned-little,
          DataLen:64/unsigned-little,
          Rest0/binary>> ->
            NameBytes = NameLen * 2,
            case Rest0 of
                <<NameUtf16:NameBytes/binary, VarData:DataLen/binary,
                  _Tail/binary>> ->
                    Name = utf16le_to_utf8(NameUtf16),
                    #{
                        <<"variable-guid">> => format_guid(GuidBin),
                        <<"variable-name">> => Name,
                        <<"variable-data">> => VarData,
                        <<"variable-data-length">> => DataLen,
                        <<"semantic">> =>
                            decode_uefi_variable_semantic(Name, VarData)
                    };
                _ ->
                    #{<<"error">> => <<"truncated UEFI_VARIABLE_DATA">>}
            end;
        _ ->
            #{<<"error">> => <<"event_data too short for UEFI_VARIABLE_DATA "
                               "header">>}
    end;
decode_uefi_variable(_) -> #{}.

%% EV_EFI_VARIABLE_BOOT: delegates to the generic UEFI_VARIABLE_DATA
%% parser, then layers on BootOrder / Boot#### decoding on top of the
%% `variable-data` binary. `BootOrder' is an array of u16 (little-
%% endian) referencing the Boot#### entries to try in order; each
%% Boot#### is an EFI_LOAD_OPTION: {attributes u32, file-path-list-
%% length u16, description UTF-16 NUL-terminated, file-path-list
%% [file-path-list-length], optional-data ...}.
decode_uefi_variable_boot(Event) ->
    Base = decode_uefi_variable(Event),
    Name = maps:get(<<"variable-name">>, Base, <<>>),
    Data = maps:get(<<"variable-data">>, Base, <<>>),
    case {Name, Data} of
        {<<"BootOrder">>, D} when is_binary(D), byte_size(D) > 0,
                                  byte_size(D) rem 2 =:= 0 ->
            Order = [N || <<N:16/little>> <= D],
            Boot = #{
                <<"boot-order">> =>
                    [iolist_to_binary(
                        io_lib:format("Boot~4.16.0B", [N])) || N <- Order],
                <<"boot-order-count">> => length(Order)
            },
            Sem = maps:get(<<"semantic">>, Base, #{}),
            Base#{<<"semantic">> => maps:merge(Sem, Boot)};
        {<<"Boot", _/binary>>, D} when is_binary(D), byte_size(D) > 6 ->
            case parse_efi_load_option(D) of
                undefined -> Base;
                LoadOpt ->
                    Sem = maps:get(<<"semantic">>, Base, #{}),
                    Base#{<<"semantic">> => maps:merge(Sem, LoadOpt)}
            end;
        _ -> Base
    end.

parse_efi_load_option(
  <<Attributes:32/little,
    FilePathListLength:16/little,
    Rest/binary>>) ->
    %% Read UTF-16LE NUL-terminated description.
    case read_utf16_nul(Rest) of
        {ok, DescBin, After} when byte_size(After) >= FilePathListLength ->
            <<_FilePath:FilePathListLength/binary, _/binary>> = After,
            #{
                <<"load-option-attributes">> => Attributes,
                <<"load-option-active">> =>
                    (Attributes band 16#0001) =/= 0,
                <<"load-option-hidden">> =>
                    (Attributes band 16#0008) =/= 0,
                <<"load-option-description">> => DescBin,
                <<"load-option-file-path-length">> => FilePathListLength
            };
        _ -> undefined
    end;
parse_efi_load_option(_) -> undefined.

read_utf16_nul(Bin) -> read_utf16_nul(Bin, []).
read_utf16_nul(<<0:16, Rest/binary>>, Acc) ->
    Raw = list_to_binary(lists:reverse(Acc)),
    case unicode:characters_to_binary(Raw, {utf16, little}, utf8) of
        U when is_binary(U) -> {ok, U, Rest};
        _ -> {ok, Raw, Rest}
    end;
read_utf16_nul(<<C:2/binary, Rest/binary>>, Acc) ->
    read_utf16_nul(Rest, [C | Acc]);
read_utf16_nul(_, _) -> undefined.

%% UEFI_GPT_DATA: EFI_PARTITION_TABLE_HEADER (92B) + NumberOfPartitions
%% u64 + PARTITION_ENTRIES. The PARTITION_TABLE_HEADER fields we surface
%% are the disk GUID + first/last LBA + number of entries; partition
%% entries carry partition GUID + type GUID + name + span.
decode_uefi_gpt(#{<<"event-data">> := Data})
  when byte_size(Data) >= 92 + 8 ->
    <<_Sig:8/binary,
      _Rev:32/little,
      HdrSize:32/little,
      _HdrCrc:32/little,
      _Reserved:32,
      MyLba:64/little,
      AltLba:64/little,
      FirstUsable:64/little,
      LastUsable:64/little,
      DiskGuid:16/binary,
      PartEntryLba:64/little,
      NumEntries:32/little,
      EntrySize:32/little,
      _PartArrCrc:32/little,
      _Rest0/binary>> = Data,
    %% GPT header u64 for total-number-of-partition-entries may be
    %% overridden by an EV_EFI_GPT_DATA-specific field after the header.
    PartCount =
        case byte_size(Data) of
            N when N >= HdrSize + 8 ->
                <<_:HdrSize/binary, PC:64/little, _/binary>> = Data,
                PC;
            _ -> NumEntries
        end,
    #{
        <<"disk-guid">>               => format_guid(DiskGuid),
        <<"my-lba">>                  => MyLba,
        <<"alternate-lba">>           => AltLba,
        <<"first-usable-lba">>        => FirstUsable,
        <<"last-usable-lba">>         => LastUsable,
        <<"partition-entry-lba">>     => PartEntryLba,
        <<"number-of-partition-entries">> => NumEntries,
        <<"size-of-partition-entry">> => EntrySize,
        <<"measured-partition-count">> => PartCount
    };
decode_uefi_gpt(_) -> #{}.

%% EV_EFI_HANDOFF_TABLES2 layout: tableDescription is a uint8-length-
%% prefixed UTF-8 string (ACPI / SMBIOS / ...), followed by the table
%% array. We only surface the description; the tables themselves are
%% vendor-specific blobs.
decode_handoff_tables2(#{<<"event-data">> := Data})
  when byte_size(Data) >= 1 ->
    <<DescLen:8, Rest/binary>> = Data,
    case DescLen of
        0 -> #{<<"table-description">> => <<>>};
        _ when byte_size(Rest) >= DescLen ->
            <<Desc:DescLen/binary, _/binary>> = Rest,
            #{<<"table-description">> => Desc,
              <<"table-description-length">> => DescLen};
        _ -> #{<<"error">> => <<"truncated handoff-tables2 description">>}
    end;
decode_handoff_tables2(_) -> #{}.

%%%============================================================================
%%% SMBIOS + ACPI table metadata decoders
%%%============================================================================
%%%
%%% Some firmware stacks measure the actual SMBIOS / ACPI table
%%% content on PCR 1 (rather than just pointers via HANDOFF_TABLES).
%%% These helpers recognise the table's shape so a caller can make
%%% sense of `measurement-content-type: smbios' or `acpi' without
%%% having to re-parse the bytes themselves.
%%%
%%% We don't invoke them from any specific event-type decoder
%%% automatically -- firmware stacks vary too much. Callers that
%%% suspect a blob to be SMBIOS or ACPI can invoke `parse_smbios/1'
%%% / `parse_acpi_table/1' directly; the results are a structured
%%% message just like the event parsers above.

%% SMBIOS entry point: anchor string "_SM_" (v2.x) or "_SM3_" (v3.x)
%% at the start of the blob. Layout per DMTF DSP0134.
%%
%% v2.x entry (31 bytes):
%%   Anchor                "_SM_"        (4B)
%%   Checksum              u8
%%   EntryPointLength      u8            (must be 31)
%%   MajorVersion          u8
%%   MinorVersion          u8
%%   MaxStructureSize      u16 LE
%%   EPRevision            u8
%%   FormattedArea         [5 bytes]
%%   IntermediateAnchor    "_DMI_"       (5B)
%%   IntermediateChecksum  u8
%%   StructureTableLength  u16 LE
%%   StructureTableAddress u32 LE
%%   NumberOfStructures    u16 LE
%%   BCDRevision           u8
%%
%% v3.x entry (24 bytes):
%%   Anchor                "_SM3_"       (5B)
%%   Checksum              u8
%%   EntryPointLength      u8
%%   MajorVersion          u8
%%   MinorVersion          u8
%%   DocRev                u8
%%   EPRevision            u8
%%   Reserved              u8
%%   StructureTableMaxSize u32 LE
%%   StructureTableAddress u64 LE
parse_smbios(<<"_SM_", Checksum:8, EPL:8, Major:8, Minor:8,
               MaxStructSize:16/little, Rev:8, _Fmt:5/binary,
               "_DMI_", _ImmCheck:8, TableLen:16/little,
               TableAddr:32/little, NumStructs:16/little,
               _Bcd:8, _/binary>>) ->
    #{<<"anchor">>             => <<"_SM_">>,
      <<"version">>            =>
        iolist_to_binary(io_lib:format("~B.~B", [Major, Minor])),
      <<"entry-point-length">> => EPL,
      <<"entry-point-revision">> => Rev,
      <<"entry-point-checksum">> => Checksum,
      <<"table-length">>       => TableLen,
      <<"table-address">>      => TableAddr,
      <<"max-structure-size">> => MaxStructSize,
      <<"number-of-structures">> => NumStructs};
parse_smbios(<<"_SM3_", Checksum:8, EPL:8, Major:8, Minor:8,
               DocRev:8, Rev:8, _Reserved:8,
               MaxSize:32/little, TableAddr:64/little, _/binary>>) ->
    #{<<"anchor">>                 => <<"_SM3_">>,
      <<"version">>                =>
        iolist_to_binary(io_lib:format("~B.~B", [Major, Minor])),
      <<"doc-revision">>           => DocRev,
      <<"entry-point-length">>     => EPL,
      <<"entry-point-revision">>   => Rev,
      <<"entry-point-checksum">>   => Checksum,
      <<"structure-table-max-size">> => MaxSize,
      <<"table-address">>          => TableAddr};
parse_smbios(_) -> #{<<"error">> => <<"not an SMBIOS entry point">>}.

%% SMBIOS structure decoder -- decodes a single SMBIOS structure
%% (the byte-shaped variant within a structure table). Covers the
%% handful of types a verifier actually cares about; others report
%% {type, length}.
%%
%% Common header (always present):
%%   Type      u8
%%   Length    u8        (bytes in the formatted fixed-size area)
%%   Handle    u16 LE
%% followed by type-specific fields then a double-NUL-terminated
%% string table.
parse_smbios_structure(<<Type:8, Length:8, Handle:16/little,
                           Rest/binary>>) when byte_size(Rest) >= Length - 4 ->
    FormattedLen = Length - 4,  %% subtract the 4 header bytes
    <<FormattedArea:FormattedLen/binary, StringTable/binary>> = Rest,
    Strings = smbios_strings(StringTable),
    Fields = decode_smbios_fields(Type, FormattedArea, Strings),
    maps:merge(
        #{<<"smbios-type">>         => Type,
          <<"smbios-type-name">>    => smbios_type_name(Type),
          <<"smbios-length">>       => Length,
          <<"smbios-handle">>       => Handle,
          <<"smbios-strings">>      => Strings},
        Fields);
parse_smbios_structure(_) ->
    #{<<"error">> => <<"malformed SMBIOS structure header">>}.

smbios_strings(Bin) -> smbios_strings(Bin, []).
smbios_strings(<<0, _/binary>>, Acc) -> lists:reverse(Acc);
smbios_strings(<<>>, Acc) -> lists:reverse(Acc);
smbios_strings(Bin, Acc) ->
    case binary:split(Bin, <<0>>) of
        [S, Rest] when byte_size(S) > 0 ->
            smbios_strings(Rest, [S | Acc]);
        _ -> lists:reverse(Acc)
    end.

%% Type 0: BIOS Information (fields at fixed offsets). Reference
%% strings by 1-based index into the string table.
decode_smbios_fields(0, <<VendorIdx:8, VersionIdx:8, _StartAddr:16/little,
                            ReleaseDateIdx:8, RomSize:8, _/binary>>,
                      Strings) ->
    #{<<"bios-vendor">>       => smbios_str(Strings, VendorIdx),
      <<"bios-version">>      => smbios_str(Strings, VersionIdx),
      <<"bios-release-date">> => smbios_str(Strings, ReleaseDateIdx),
      <<"bios-rom-size-code">> => RomSize};
decode_smbios_fields(1, <<ManuIdx:8, ProdIdx:8, VersIdx:8, SerialIdx:8,
                            Uuid:16/binary, WakeUp:8, SkuIdx:8,
                            FamilyIdx:8, _/binary>>,
                      Strings) ->
    #{<<"system-manufacturer">> => smbios_str(Strings, ManuIdx),
      <<"system-product-name">> => smbios_str(Strings, ProdIdx),
      <<"system-version">>      => smbios_str(Strings, VersIdx),
      <<"system-serial">>       => smbios_str(Strings, SerialIdx),
      <<"system-uuid">>         => format_smbios_uuid(Uuid),
      <<"system-wake-up-type">> => WakeUp,
      <<"system-sku">>          => smbios_str(Strings, SkuIdx),
      <<"system-family">>       => smbios_str(Strings, FamilyIdx)};
decode_smbios_fields(2, <<ManuIdx:8, ProdIdx:8, VersIdx:8, SerialIdx:8,
                            AssetIdx:8, _FeatFlags:8, LocIdx:8,
                            _/binary>>, Strings) ->
    #{<<"baseboard-manufacturer">> => smbios_str(Strings, ManuIdx),
      <<"baseboard-product">>      => smbios_str(Strings, ProdIdx),
      <<"baseboard-version">>      => smbios_str(Strings, VersIdx),
      <<"baseboard-serial">>       => smbios_str(Strings, SerialIdx),
      <<"baseboard-asset-tag">>    => smbios_str(Strings, AssetIdx),
      <<"baseboard-location">>     => smbios_str(Strings, LocIdx)};
decode_smbios_fields(3, <<ManuIdx:8, ChassisType:8, VersIdx:8,
                            SerialIdx:8, AssetIdx:8, _/binary>>,
                      Strings) ->
    #{<<"chassis-manufacturer">> => smbios_str(Strings, ManuIdx),
      <<"chassis-type">>         => ChassisType,
      <<"chassis-type-name">>    => smbios_chassis_name(ChassisType),
      <<"chassis-version">>      => smbios_str(Strings, VersIdx),
      <<"chassis-serial">>       => smbios_str(Strings, SerialIdx),
      <<"chassis-asset-tag">>    => smbios_str(Strings, AssetIdx)};
decode_smbios_fields(_, _, _) -> #{}.

smbios_str(Strings, Idx) when Idx >= 1, Idx =< length(Strings) ->
    lists:nth(Idx, Strings);
smbios_str(_, _) -> <<"">>.

smbios_type_name(0)   -> <<"BIOS Information">>;
smbios_type_name(1)   -> <<"System Information">>;
smbios_type_name(2)   -> <<"Baseboard (or Module) Information">>;
smbios_type_name(3)   -> <<"System Enclosure or Chassis">>;
smbios_type_name(4)   -> <<"Processor Information">>;
smbios_type_name(7)   -> <<"Cache Information">>;
smbios_type_name(11)  -> <<"OEM Strings">>;
smbios_type_name(16)  -> <<"Physical Memory Array">>;
smbios_type_name(17)  -> <<"Memory Device">>;
smbios_type_name(19)  -> <<"Memory Array Mapped Address">>;
smbios_type_name(21)  -> <<"Built-in Pointing Device">>;
smbios_type_name(32)  -> <<"System Boot Information">>;
smbios_type_name(38)  -> <<"IPMI Device Information">>;
smbios_type_name(41)  -> <<"Onboard Devices Extended Information">>;
smbios_type_name(42)  -> <<"Management Controller Host Interface">>;
smbios_type_name(43)  -> <<"TPM Device">>;
smbios_type_name(44)  -> <<"Processor Additional Information">>;
smbios_type_name(127) -> <<"End-of-Table">>;
smbios_type_name(_)   -> <<"Other/OEM">>.

smbios_chassis_name(1)  -> <<"Other">>;
smbios_chassis_name(2)  -> <<"Unknown">>;
smbios_chassis_name(3)  -> <<"Desktop">>;
smbios_chassis_name(4)  -> <<"Low Profile Desktop">>;
smbios_chassis_name(5)  -> <<"Pizza Box">>;
smbios_chassis_name(6)  -> <<"Mini Tower">>;
smbios_chassis_name(7)  -> <<"Tower">>;
smbios_chassis_name(8)  -> <<"Portable">>;
smbios_chassis_name(9)  -> <<"Laptop">>;
smbios_chassis_name(10) -> <<"Notebook">>;
smbios_chassis_name(11) -> <<"Hand Held">>;
smbios_chassis_name(12) -> <<"Docking Station">>;
smbios_chassis_name(13) -> <<"All in One">>;
smbios_chassis_name(14) -> <<"Sub Notebook">>;
smbios_chassis_name(15) -> <<"Space-saving">>;
smbios_chassis_name(16) -> <<"Lunch Box">>;
smbios_chassis_name(17) -> <<"Main Server Chassis">>;
smbios_chassis_name(18) -> <<"Expansion Chassis">>;
smbios_chassis_name(19) -> <<"SubChassis">>;
smbios_chassis_name(20) -> <<"Bus Expansion Chassis">>;
smbios_chassis_name(21) -> <<"Peripheral Chassis">>;
smbios_chassis_name(22) -> <<"RAID Chassis">>;
smbios_chassis_name(23) -> <<"Rack Mount Chassis">>;
smbios_chassis_name(24) -> <<"Sealed-case PC">>;
smbios_chassis_name(25) -> <<"Multi-system Chassis">>;
smbios_chassis_name(26) -> <<"Compact PCI">>;
smbios_chassis_name(27) -> <<"Advanced TCA">>;
smbios_chassis_name(28) -> <<"Blade">>;
smbios_chassis_name(29) -> <<"Blade Enclosure">>;
smbios_chassis_name(30) -> <<"Tablet">>;
smbios_chassis_name(31) -> <<"Convertible">>;
smbios_chassis_name(32) -> <<"Detachable">>;
smbios_chassis_name(33) -> <<"IoT Gateway">>;
smbios_chassis_name(34) -> <<"Embedded PC">>;
smbios_chassis_name(35) -> <<"Mini PC">>;
smbios_chassis_name(36) -> <<"Stick PC">>;
smbios_chassis_name(_)  -> <<"Unknown/OEM">>.

%% SMBIOS stores UUIDs in the same mixed-endian pattern as EFI_GUID,
%% EXCEPT SMBIOS pre-2.6 uses the reverse byte order. DMTF DSP0134
%% section 7.2.1 specifies the 2.6+ "proper" layout: first 3 fields are
%% little-endian, last 8 bytes are byte-preserved.
format_smbios_uuid(Uuid) -> format_guid(Uuid).

%%%---- ACPI table header ---------------------------------------------
%%% Every ACPI table (except the RSDP itself) starts with a 36-byte
%%% header per ACPI 6.5 section 5.2.6:
%%%
%%%   Signature        4 chars
%%%   Length           u32 LE
%%%   Revision         u8
%%%   Checksum         u8
%%%   OEMID            6 bytes ASCII
%%%   OEMTableID       8 bytes ASCII
%%%   OEMRevision      u32 LE
%%%   CreatorID        4 bytes ASCII
%%%   CreatorRevision  u32 LE
parse_acpi_table(<<Sig:4/binary, Length:32/little, Rev:8, Checksum:8,
                     OemId:6/binary, OemTableId:8/binary,
                     OemRev:32/little, CreatorId:4/binary,
                     CreatorRev:32/little, _Rest/binary>> = Bin)
  when byte_size(Bin) >= 36 ->
    #{<<"signature">>          => Sig,
      <<"signature-name">>     => acpi_signature_name(Sig),
      <<"length">>             => Length,
      <<"revision">>           => Rev,
      <<"checksum">>           => Checksum,
      <<"oem-id">>             => strip_trailing_nulls(OemId),
      <<"oem-table-id">>       => strip_trailing_nulls(OemTableId),
      <<"oem-revision">>       => OemRev,
      <<"creator-id">>         => strip_trailing_nulls(CreatorId),
      <<"creator-revision">>   => CreatorRev};
parse_acpi_table(_) ->
    #{<<"error">> => <<"not an ACPI table header">>}.

%% RSDP has a different layout (no common header).
%% v1 (20 bytes):
%%   Signature  "RSD PTR "  (8B, trailing space)
%%   Checksum   u8
%%   OEMID      6 bytes
%%   Revision   u8
%%   RsdtAddr   u32 LE
%% v2 (36 bytes) adds:
%%   Length     u32 LE
%%   XsdtAddr   u64 LE
%%   ExtChecksum u8
%%   Reserved   3 bytes
parse_acpi_rsdp(<<"RSD PTR ", Checksum:8, OemId:6/binary,
                    Rev:8, RsdtAddr:32/little, Rest/binary>>) ->
    Base = #{<<"signature">>  => <<"RSD PTR ">>,
             <<"checksum">>   => Checksum,
             <<"oem-id">>     => strip_trailing_nulls(OemId),
             <<"revision">>   => Rev,
             <<"rsdt-address">> => RsdtAddr},
    case {Rev, Rest} of
        {V, <<Length:32/little, XsdtAddr:64/little,
              ExtChecksum:8, _Reserved:3/binary, _/binary>>}
          when V >= 2 ->
            Base#{<<"length">>         => Length,
                  <<"xsdt-address">>   => XsdtAddr,
                  <<"extended-checksum">> => ExtChecksum};
        _ -> Base
    end;
parse_acpi_rsdp(_) ->
    #{<<"error">> => <<"not an ACPI RSDP">>}.

%% Known ACPI signatures a verifier is likely to care about.
%% Reference: ACPI 6.5 Table 5.4 + TCG DICE spec section 5.5.
acpi_signature_name(<<"RSDT">>) -> <<"Root System Description Table">>;
acpi_signature_name(<<"XSDT">>) -> <<"Extended System Description Table">>;
acpi_signature_name(<<"FACP">>) -> <<"Fixed ACPI Description Table (FADT)">>;
acpi_signature_name(<<"FACS">>) -> <<"Firmware ACPI Control Structure">>;
acpi_signature_name(<<"DSDT">>) -> <<"Differentiated System Description Table">>;
acpi_signature_name(<<"SSDT">>) -> <<"Secondary System Description Table">>;
acpi_signature_name(<<"PSDT">>) -> <<"Persistent System Description Table">>;
acpi_signature_name(<<"APIC">>) -> <<"Multiple APIC Description Table (MADT)">>;
acpi_signature_name(<<"SBST">>) -> <<"Smart Battery Specification Table">>;
acpi_signature_name(<<"ECDT">>) -> <<"Embedded Controller Boot Resources Table">>;
acpi_signature_name(<<"SRAT">>) -> <<"System Resource Affinity Table">>;
acpi_signature_name(<<"SLIT">>) -> <<"System Locality Distance Info Table">>;
acpi_signature_name(<<"MCFG">>) -> <<"PCI Express memory-mapped config space">>;
acpi_signature_name(<<"HPET">>) -> <<"High Precision Event Timer">>;
acpi_signature_name(<<"BGRT">>) -> <<"Boot Graphics Resource Table">>;
acpi_signature_name(<<"BERT">>) -> <<"Boot Error Record Table">>;
acpi_signature_name(<<"EINJ">>) -> <<"Error Injection Table">>;
acpi_signature_name(<<"ERST">>) -> <<"Error Record Serialization Table">>;
acpi_signature_name(<<"HEST">>) -> <<"Hardware Error Source Table">>;
acpi_signature_name(<<"TPM2">>) -> <<"Trusted Platform Module 2.0">>;
acpi_signature_name(<<"TCPA">>) -> <<"Trusted Computing Platform Alliance (legacy TPM 1.2)">>;
acpi_signature_name(<<"DMAR">>) -> <<"DMA Remapping Table (Intel VT-d)">>;
acpi_signature_name(<<"IVRS">>) -> <<"I/O Virtualization Reporting Structure (AMD-Vi)">>;
acpi_signature_name(<<"GTDT">>) -> <<"Generic Timer Description Table (ARM)">>;
acpi_signature_name(<<"NFIT">>) -> <<"NVDIMM Firmware Interface Table">>;
acpi_signature_name(<<"WSMT">>) -> <<"Windows SMM Security Mitigation Table">>;
acpi_signature_name(<<"BATB">>) -> <<"Battery Bios Table">>;
acpi_signature_name(<<"PCCT">>) -> <<"Platform Communications Channel Table">>;
acpi_signature_name(<<"PMTT">>) -> <<"Platform Memory Topology Table">>;
acpi_signature_name(<<"SLIC">>) -> <<"Software Licensing Description Table (Microsoft)">>;
acpi_signature_name(<<"MSDM">>) -> <<"Microsoft Data Management Table">>;
acpi_signature_name(<<"SPCR">>) -> <<"Serial Port Console Redirection Table">>;
acpi_signature_name(<<"DBG2">>) -> <<"Debug Port Table 2">>;
acpi_signature_name(<<"WAET">>) -> <<"Windows ACPI Emulated devices Table">>;
acpi_signature_name(<<"WPBT">>) -> <<"Windows Platform Binary Table">>;
acpi_signature_name(<<"CCEL">>) -> <<"Confidential Computing Event Log (TCG, Intel TDX)">>;
acpi_signature_name(<<"SVKL">>) -> <<"Storage Volume Key Location Table">>;
acpi_signature_name(_) -> <<"unknown-or-oem">>.

%%%============================================================================
%%% systemd-stub PE section awareness
%%%============================================================================
%%%
%%% systemd-stub / systemd-boot assembles a Unified Kernel Image (UKI)
%%% as a PE binary with well-known named sections, each of which is
%%% measured into a specific TPM PCR at boot (src/boot/stub.c
%%% `sections[]` table).
%%%
%%% Section              PCR   Notes
%%%   .linux              11   kernel PE image
%%%   .osrel              11   os-release identifier
%%%   .cmdline            12   kernel cmdline (variable)
%%%   .initrd             11   initrd image
%%%   .ucode              11   microcode early-load blob
%%%   .splash             11   splash image
%%%   .dtb                11   device tree (ARM)
%%%   .uname              11   kernel version string
%%%   .sbat               11   SBAT / shim revocation metadata
%%%   .pcrsig             (not measured -- contains the TPM2_Sign of .pcrpkey)
%%%   .pcrpkey            11   public signing key committed across PCRs
%%%   .profile            12   active profile name
%%%   .dtbauto            11   auto-selected device tree
%%%   .hwids              11   hardware ID hints for matching DTs
%%%   .efifw              11   in-UKI firmware update blob
%%%
%%% systemd-stub emits an EV_IPL event per measured section; the
%%% event-data is `<section-name>=<measured-data-description>\0'
%%% which our EV_IPL decoder splits into {key, value}. These helpers
%%% tell a caller (a) whether a given key is a known systemd-stub
%%% section, and (b) which PCR it's expected to be measured into.
is_systemd_stub_pe_section(Key) ->
    maps:is_key(Key, systemd_stub_pe_sections()).

systemd_stub_pe_section_pcr(Key) ->
    maps:get(Key, systemd_stub_pe_sections(), undefined).

systemd_stub_pe_sections() ->
    #{<<".linux">>    => 11,
      <<"linux">>     => 11,  %% sd-stub post-v255 drops the leading "."
      <<".osrel">>    => 11,
      <<"osrel">>     => 11,
      <<".cmdline">>  => 12,
      <<"cmdline">>   => 12,
      <<".initrd">>   => 11,
      <<"initrd">>    => 11,
      <<".ucode">>    => 11,
      <<"ucode">>     => 11,
      <<".splash">>   => 11,
      <<"splash">>    => 11,
      <<".dtb">>      => 11,
      <<"dtb">>       => 11,
      <<".uname">>    => 11,
      <<"uname">>     => 11,
      <<".sbat">>     => 11,
      <<"sbat">>      => 11,
      <<".pcrpkey">>  => 11,
      <<"pcrpkey">>   => 11,
      <<".profile">>  => 12,
      <<"profile">>   => 12,
      <<".dtbauto">>  => 11,
      <<"dtbauto">>   => 11,
      <<".hwids">>    => 11,
      <<"hwids">>     => 11,
      <<".efifw">>    => 11,
      <<"efifw">>     => 11,
      %% sd-stub also emits these for the kernel identity chain:
      <<"kernel-name">>    => 11,
      <<"kernel-version">> => 11,
      <<"kernel-image">>   => 11,
      <<"kernel-cmdline">> => 12,
      %% sd-stub "initrd measurement" legacy key:
      <<"initrd-image">>   => 11}.

%% EV_EFI_HANDOFF_TABLES v1 -- deprecated but still seen on older
%% firmware. Layout (UEFI section 8):
%%   NumberOfTables  u64 LE
%%   TableEntry[N]   {VendorGuid 16B, VendorTable u64 LE}
%%
%% VendorGuid is the well-known GUID for the table (ACPI 2.0 RSDP,
%% SMBIOS 2.x entry point, HOB list, etc.); VendorTable is the
%% physical pointer. We categorise the GUIDs so a caller can say
%% "this boot's ACPI RSDP was at 0x7EFDE014".
decode_handoff_tables_v1(#{<<"event-data">> := Data})
  when byte_size(Data) >= 8 ->
    <<N:64/little, Rest/binary>> = Data,
    Entries = decode_handoff_v1_entries(Rest, N, []),
    #{<<"number-of-tables">> => N,
      <<"tables">>           => Entries};
decode_handoff_tables_v1(_) -> #{}.

decode_handoff_v1_entries(_, 0, Acc) -> lists:reverse(Acc);
decode_handoff_v1_entries(<<GuidBin:16/binary, Addr:64/little,
                             Rest/binary>>, N, Acc) ->
    Guid = format_guid(GuidBin),
    Entry = #{
        <<"vendor-guid">>      => Guid,
        <<"vendor-guid-name">> => handoff_table_guid_name(Guid),
        <<"vendor-table-address">> => Addr
    },
    decode_handoff_v1_entries(Rest, N - 1, [Entry | Acc]);
decode_handoff_v1_entries(_, _, Acc) -> lists:reverse(Acc).

%% Well-known vendor table GUIDs (UEFI spec Appendix A + ACPI 6.5).
handoff_table_guid_name(<<"eb9d2d30-2d88-11d3-9a16-0090273fc14d">>) ->
    <<"ACPI 1.0 RSDP">>;
handoff_table_guid_name(<<"8868e871-e4f1-11d3-bc22-0080c73c8881">>) ->
    <<"ACPI 2.0 RSDP">>;
handoff_table_guid_name(<<"eb9d2d31-2d88-11d3-9a16-0090273fc14d">>) ->
    <<"SMBIOS 2.x entry point">>;
handoff_table_guid_name(<<"f2fd1544-9794-4a2c-992e-e5bbcf20e394">>) ->
    <<"SMBIOS 3.x entry point">>;
handoff_table_guid_name(<<"eb9d2d32-2d88-11d3-9a16-0090273fc14d">>) ->
    <<"SAL System Table">>;
handoff_table_guid_name(<<"eb9d2d2f-2d88-11d3-9a16-0090273fc14d">>) ->
    <<"MPS Table">>;
handoff_table_guid_name(<<"7739f24c-93d7-11d4-9a3a-0090273fc14d">>) ->
    <<"HOB List">>;
handoff_table_guid_name(<<"4c19049f-4137-4dd3-9c10-8b97a83ffdfa">>) ->
    <<"Memory Type Information">>;
handoff_table_guid_name(<<"49152e77-1ada-4764-b7a2-7afefed95e8b">>) ->
    <<"Debug Image Info Table">>;
handoff_table_guid_name(<<"060cc026-4c0d-4dda-8f41-595fef00a502">>) ->
    <<"Memory Status Code Record">>;
handoff_table_guid_name(_) -> <<"unknown">>.

%% EV_S_CRTM_CONTENTS -- most commonly a UEFI_PLATFORM_FIRMWARE_BLOB
%% (v1 shape: 16 bytes). Fall back to opaque if shorter/longer.
decode_crtm_contents(#{<<"event-data">> := <<Addr:64/little,
                                                Len:64/little>>}) ->
    #{<<"format">>               => <<"firmware-blob-v1">>,
      <<"blob-physical-address">> => Addr,
      <<"blob-length">>           => Len};
decode_crtm_contents(#{<<"event-data">> := Data}) ->
    #{<<"format">>     => <<"opaque">>,
      <<"data-length">> => byte_size(Data),
      <<"sha256">>     => hb_util:encode(crypto:hash(sha256, Data))}.

%% EV_PLATFORM_CONFIG_FLAGS -- vendor-specific flag bytes.
decode_platform_config_flags(#{<<"event-data">> := Data}) ->
    #{<<"data-length">> => byte_size(Data),
      <<"sha256">>     => hb_util:encode(crypto:hash(sha256, Data))}.

%% EV_TABLE_OF_DEVICES -- array of UEFI_DEVICE_PATH instances.
%% The array is NUL-terminated (0xFF end-entire) per the TCG spec.
%% We split on the outermost end-entire terminator (0x7F 0xFF 04 00)
%% and walk each path.
decode_table_of_devices(#{<<"event-data">> := Data}) ->
    Paths = split_on_end_entire(Data, <<>>, []),
    Parsed = [begin
                  {Nodes, Text} = parse_device_path(P),
                  #{<<"nodes">> => Nodes,
                    <<"text">>  => Text}
              end || P <- Paths, P =/= <<>>],
    #{<<"device-path-count">> => length(Parsed),
      <<"device-paths">>      => Parsed}.

split_on_end_entire(<<>>, Curr, Acc) ->
    lists:reverse([Curr | Acc]);
split_on_end_entire(<<16#7F, 16#FF, 16#04, 16#00, Rest/binary>>, Curr, Acc) ->
    Completed = <<Curr/binary, 16#7F, 16#FF, 16#04, 16#00>>,
    split_on_end_entire(Rest, <<>>, [Completed | Acc]);
split_on_end_entire(<<B:1/binary, Rest/binary>>, Curr, Acc) ->
    split_on_end_entire(Rest, <<Curr/binary, B/binary>>, Acc).

%% Generic "just surface length + sha256" decoder for events whose
%% internal structure we can't decode at this layer.
decode_opaque_with_length(#{<<"event-data">> := Data}) ->
    #{<<"data-length">> => byte_size(Data),
      <<"sha256">>     => hb_util:encode(crypto:hash(sha256, Data))}.

%% EV_IPL_PARTITION_DATA -- GRUB legacy. Event data is typically
%% an ASCII path string (e.g. "/boot/grub/grub.cfg") followed by
%% the file content. We extract the path prefix + length.
decode_ipl_partition_data(#{<<"event-data">> := Data}) ->
    case binary:split(Data, <<0>>) of
        [Path, Content] ->
            case ascii_only(Path) of
                true ->
                    #{<<"format">>         => <<"grub-legacy">>,
                      <<"path">>           => Path,
                      <<"content-length">> => byte_size(Content),
                      <<"content-sha256">> =>
                          hb_util:encode(crypto:hash(sha256, Content))};
                false ->
                    decode_opaque_with_length(#{<<"event-data">> => Data})
            end;
        _ -> decode_opaque_with_length(#{<<"event-data">> => Data})
    end.

%% EV_NONHOST_* -- AMD PSP / Intel ME / other co-processor firmware.
%% Completely vendor-specific; we surface what every verifier wants:
%% the SHA-256 so they can pin it against a known-good baseline.
decode_nonhost(#{<<"event-data">> := Data}, Kind) ->
    #{<<"nonhost-kind">>  => Kind,
      <<"data-length">>   => byte_size(Data),
      <<"sha256">>        => hb_util:encode(crypto:hash(sha256, Data)),
      <<"note">>          =>
          <<"EV_NONHOST_* event data is firmware-proprietary "
            "(AMD PSP / Intel ME / similar). Verifiers should "
            "compare the SHA-256 against a known-good baseline "
            "from the silicon vendor.">>};
decode_nonhost(_, _) -> #{}.

%% EV_EFI_SPDM_* -- UEFI 2.10 section 32.5.
%%
%% The event data is either:
%%
%%   TCG_DEVICE_SECURITY_EVENT_DATA   (v1, "SPDM Device Sec\0")
%%   TCG_DEVICE_SECURITY_EVENT_DATA2  (v2, "SPDM Device Sec2")
%%
%% Both start with a 16-byte signature which we match on. When
%% recognised we unpack the full structure: header fields +
%% SPDM measurement block (SubHeaderType 0) or SPDM cert chain
%% (SubHeaderType 1) + trailing UEFI_DEVICE_PATH.
%%
%% Legacy / malformed data falls back to a path-first heuristic
%% (find the End-entire terminator) for backward compatibility
%% with older firmware that emitted a non-canonical layout.
decode_spdm_event(#{<<"event-data">> := Data}, Kind) ->
    case Data of
        <<"SPDM Device Sec2", _/binary>> ->
            decode_spdm_v2(Data, Kind);
        <<"SPDM Device Sec", 0, _/binary>> ->
            decode_spdm_v1(Data, Kind);
        _ ->
            decode_spdm_legacy(Data, Kind)
    end;
decode_spdm_event(_, _) -> #{}.

%% TCG_DEVICE_SECURITY_EVENT_DATA2 (UEFI 2.10 section 32.5.1):
%%   Signature[16] "SPDM Device Sec2"
%%   Version           u16 LE (0x0002)
%%   AuthState         u8  (0=Success 1=NoAuthSig 2=NoAuth
%%                          3=NoBinding 4=Fail 0xFF=NoSpdm)
%%   Reserved          u8
%%   Length            u32 LE (total event data length)
%%   DeviceType        u32 LE (0=NONE 1=PCI 2=USB)
%%   SubHeaderType     u32 LE (0=SPDM_MEAS_BLOCK 1=SPDM_CERT_CHAIN)
%%   SubHeaderLength   u32 LE
%%   SubHeaderUid      u64 LE (SPDM session UID)
%%   SubHeader         [SubHeaderLength] -- SPDM meas or cert chain
%%   DevicePathLength  u64 LE
%%   DevicePath        [DevicePathLength]
decode_spdm_v2(Data, Kind) ->
    try
        <<_Sig:16/binary, Version:16/little, AuthState:8,
          _Reserved:8, Length:32/little, DeviceType:32/little,
          SubHeaderType:32/little, SubHeaderLength:32/little,
          SubHeaderUid:64/little, Rest0/binary>> = Data,
        <<SubHeader:SubHeaderLength/binary, Rest1/binary>> = Rest0,
        {DpLen, DevicePath, Tail} =
            case Rest1 of
                <<DL:64/little, DP:DL/binary, T/binary>> ->
                    {DL, DP, T};
                _ -> {0, <<>>, Rest1}
            end,
        {Nodes, Text} = parse_device_path(DevicePath),
        SubMap = decode_spdm_subheader(SubHeaderType, SubHeader),
        Base = #{
            <<"spdm-kind">>              => Kind,
            <<"spdm-data-version">>      => 2,
            <<"spdm-version">>           =>
                iolist_to_binary(io_lib:format(
                    "0x~4.16.0B", [Version])),
            <<"auth-state">>             => AuthState,
            <<"auth-state-name">>        => spdm_auth_state_name(AuthState),
            <<"declared-length">>        => Length,
            <<"device-type">>            => DeviceType,
            <<"device-type-name">>       => spdm_device_type_name(DeviceType),
            <<"sub-header-type">>        => SubHeaderType,
            <<"sub-header-type-name">>   => spdm_sub_header_type_name(SubHeaderType),
            <<"sub-header-length">>      => SubHeaderLength,
            <<"sub-header-uid">>         => SubHeaderUid,
            <<"sub-header-uid-hex">>     =>
                iolist_to_binary(io_lib:format(
                    "0x~16.16.0B", [SubHeaderUid])),
            <<"device-path-length">>     => DpLen,
            <<"device-path-nodes">>      => Nodes,
            <<"device-path-text">>       => Text,
            <<"tail-length">>            => byte_size(Tail)
        },
        maps:merge(Base, SubMap)
    catch _:_ ->
        decode_spdm_legacy(Data, Kind)
    end.

%% TCG_DEVICE_SECURITY_EVENT_DATA (v1, UEFI 2.7-2.9):
%%   Signature[16] "SPDM Device Sec\0"
%%   Version           u16 LE
%%   Length            u16 LE
%%   SpdmHashAlg       u32 LE
%%   DeviceType        u32 LE
%%   (then a fixed SPDM_MEASUREMENT_BLOCK)
%%   DevicePathLength  u64 LE
%%   DevicePath        [DevicePathLength]
%%
%% v1 has no SubHeader union; the measurement block is positional.
decode_spdm_v1(Data, Kind) ->
    try
        <<_Sig:16/binary, Version:16/little, Length:16/little,
          SpdmHashAlg:32/little, DeviceType:32/little,
          Rest0/binary>> = Data,
        %% SPDM_MEASUREMENT_BLOCK starts at Rest0. The block itself
        %% is DMTF-structured; we let decode_spdm_measurement_block/1
        %% consume what it can.
        {BlockMap, _Rest1} = decode_spdm_measurement_block(Rest0),
        %% The device path comes after the measurement block; we
        %% can't predict its offset without tracking the block
        %% size carefully. Best-effort: find the last u64 followed
        %% by a valid device-path terminator.
        {DpLen, DevicePath} = find_trailing_device_path(Rest0),
        {Nodes, Text} = parse_device_path(DevicePath),
        Base = #{
            <<"spdm-kind">>              => Kind,
            <<"spdm-data-version">>      => 1,
            <<"spdm-version">>           =>
                iolist_to_binary(io_lib:format(
                    "0x~4.16.0B", [Version])),
            <<"declared-length">>        => Length,
            <<"spdm-hash-alg-code">>     => SpdmHashAlg,
            <<"spdm-hash-alg-name">>     => spdm_hash_alg_name(SpdmHashAlg),
            <<"device-type">>            => DeviceType,
            <<"device-type-name">>       => spdm_device_type_name(DeviceType),
            <<"device-path-length">>     => DpLen,
            <<"device-path-nodes">>      => Nodes,
            <<"device-path-text">>       => Text
        },
        maps:merge(Base, BlockMap)
    catch _:_ ->
        decode_spdm_legacy(Data, Kind)
    end.

%% Legacy path-first heuristic: find an End-entire terminator and
%% treat bytes before it as device-path, after as payload. Kept
%% for backward compatibility with firmware that didn't emit a
%% canonical v1/v2 signature.
decode_spdm_legacy(Data, Kind) ->
    case binary:match(Data, <<16#7F, 16#FF, 16#04, 16#00>>) of
        {Offset, 4} ->
            PrefixLen = Offset + 4,
            <<PathBin:PrefixLen/binary, Payload/binary>> = Data,
            {Nodes, Text} = parse_device_path(PathBin),
            #{<<"spdm-kind">>        => Kind,
              <<"spdm-data-version">> => 0,
              <<"device-path-nodes">> => Nodes,
              <<"device-path-text">>  => Text,
              <<"payload-length">>   => byte_size(Payload),
              <<"payload-sha256">>   =>
                  hb_util:encode(crypto:hash(sha256, Payload))};
        _ ->
            #{<<"spdm-kind">>     => Kind,
              <<"spdm-data-version">> => 0,
              <<"data-length">>   => byte_size(Data),
              <<"sha256">>        =>
                  hb_util:encode(crypto:hash(sha256, Data))}
    end.

%% Dispatch SPDM SubHeader decode based on type.
%%   0 = SPDM Measurement Block
%%   1 = SPDM Cert Chain
decode_spdm_subheader(0, Data) ->
    %% TCG_DEVICE_SECURITY_EVENT_DATA_SUB_HEADER_SPDM_MEASUREMENT_BLOCK:
    %%   SpdmVersion      u16 LE
    %%   (then the SPDM measurement block itself)
    case Data of
        <<SpdmVersion:16/little, Rest/binary>> ->
            {BlockMap, _} = decode_spdm_measurement_block(Rest),
            maps:merge(
              #{<<"spdm-sub-version">> =>
                    iolist_to_binary(io_lib:format(
                        "0x~4.16.0B", [SpdmVersion]))},
              BlockMap);
        _ ->
            #{<<"spdm-sub-error">> =>
                  <<"truncated SPDM measurement sub-header">>}
    end;
decode_spdm_subheader(1, Data) ->
    %% TCG_DEVICE_SECURITY_EVENT_DATA_SUB_HEADER_SPDM_CERT_CHAIN:
    %%   SpdmVersion      u16 LE
    %%   SpdmSlotId       u8
    %%   Reserved         u8
    %%   SpdmHashAlgo     u32 LE
    %%   SpdmCertChain    [remainder]
    case Data of
        <<SpdmVersion:16/little, SlotId:8, _Res:8,
          HashAlg:32/little, CertChain/binary>> ->
            #{<<"spdm-sub-version">> =>
                  iolist_to_binary(io_lib:format(
                      "0x~4.16.0B", [SpdmVersion])),
              <<"spdm-slot-id">>     => SlotId,
              <<"spdm-cert-hash-alg-code">> => HashAlg,
              <<"spdm-cert-hash-alg-name">> =>
                  spdm_hash_alg_name(HashAlg),
              <<"spdm-cert-chain-length">>  => byte_size(CertChain),
              <<"spdm-cert-chain-sha256">>  =>
                  hb_util:encode(crypto:hash(sha256, CertChain))};
        _ ->
            #{<<"spdm-sub-error">> =>
                  <<"truncated SPDM cert-chain sub-header">>}
    end;
decode_spdm_subheader(Other, Data) ->
    #{<<"spdm-sub-unknown-type">> => Other,
      <<"spdm-sub-length">>       => byte_size(Data),
      <<"spdm-sub-sha256">>       =>
          hb_util:encode(crypto:hash(sha256, Data))}.

%% SPDM_MEASUREMENT_BLOCK (DMTF DSP0274 section 10.11.3):
%%   Index                          u8
%%   MeasurementSpecification       u8 (0x01 = DMTF)
%%   MeasurementSize                u16 LE
%%   Measurement                    bytes[MeasurementSize]
%%     (DMTF-spec measurement value):
%%     DMTFSpecMeasurementValueType u8
%%       bit 7 = raw bit stream
%%       bits 0-6 = type (0=immutable ROM, 1=mutable firmware,
%%                        2=hardware config, 3=firmware config,
%%                        4=firmware-measurement manifest,
%%                        5=device mode, 6=version info,
%%                        7=secure version number)
%%     DMTFSpecMeasurementValueSize u16 LE
%%     DMTFSpecMeasurementValue     bytes[size]
decode_spdm_measurement_block(<<Index:8, MeasSpec:8, MeasSize:16/little,
                                  Rest/binary>>)
    when byte_size(Rest) >= MeasSize ->
    <<MeasurementRaw:MeasSize/binary, Tail/binary>> = Rest,
    InnerMap = decode_dmtf_measurement(MeasurementRaw),
    BaseMap = #{
        <<"meas-block-index">>         => Index,
        <<"meas-block-spec-code">>     => MeasSpec,
        <<"meas-block-spec-name">>     => spdm_meas_spec_name(MeasSpec),
        <<"meas-block-size">>          => MeasSize,
        <<"meas-block-sha256">>        =>
            hb_util:encode(crypto:hash(sha256, MeasurementRaw))
    },
    {maps:merge(BaseMap, InnerMap), Tail};
decode_spdm_measurement_block(Data) ->
    {#{<<"meas-block-error">> =>
           <<"truncated SPDM measurement block">>}, Data}.

decode_dmtf_measurement(<<ValType:8, ValSize:16/little,
                            Value:ValSize/binary, _/binary>>) ->
    Raw = (ValType bsr 7) band 1,
    TypeLow = ValType band 16#7F,
    #{
        <<"dmtf-value-type-code">>   => ValType,
        <<"dmtf-value-type-low">>    => TypeLow,
        <<"dmtf-value-type-name">>   => dmtf_meas_type_name(TypeLow),
        <<"dmtf-value-is-raw">>      => Raw =:= 1,
        <<"dmtf-value-size">>        => ValSize,
        <<"dmtf-value-sha256">>      =>
            hb_util:encode(crypto:hash(sha256, Value))
    };
decode_dmtf_measurement(_) ->
    #{<<"dmtf-value-error">> =>
          <<"truncated DMTF measurement value">>}.

%% When we can't cleanly track the measurement-block size (v1
%% path), scan from the end: a valid device path ends with End-
%% entire (7F FF 04 00) and is preceded by a u64 length.
find_trailing_device_path(Data) ->
    %% Walk from right: last 4 bytes must be end-entire.
    case binary:matches(Data, <<16#7F, 16#FF, 16#04, 16#00>>) of
        [] -> {0, <<>>};
        Matches ->
            {Off, _} = lists:last(Matches),
            PathEnd = Off + 4,
            %% Best-effort: pull the u64 LE right before a
            %% plausible device-path beginning by scanning
            %% backwards a few kilobytes.
            DpLen = PathEnd - 8,
            case DpLen of
                L when L > 0, L =< byte_size(Data) ->
                    <<_:(byte_size(Data) - PathEnd - 8)/binary,
                      Len:64/little, _/binary>> = Data,
                    case Len =< byte_size(Data) andalso
                         (PathEnd - 8 - Len) >= 0 of
                        true ->
                            <<_:(byte_size(Data) - PathEnd)/binary,
                              DP:Len/binary, _/binary>> =
                                binary:part(Data, byte_size(Data) - PathEnd - Len,
                                             Len + PathEnd),
                            {Len, DP};
                        false -> {0, <<>>}
                    end;
                _ -> {0, <<>>}
            end
    end.

%% SPDM AuthState enumeration (TCG-defined).
spdm_auth_state_name(0)    -> <<"Success">>;
spdm_auth_state_name(1)    -> <<"NoAuthNoSig">>;
spdm_auth_state_name(2)    -> <<"NoAuth">>;
spdm_auth_state_name(3)    -> <<"NoBinding">>;
spdm_auth_state_name(4)    -> <<"Fail">>;
spdm_auth_state_name(16#FF)-> <<"NoSpdm">>;
spdm_auth_state_name(_)    -> <<"unknown">>.

%% UEFI 2.10 section 32.5.2 DeviceType enumeration.
spdm_device_type_name(0) -> <<"NONE">>;
spdm_device_type_name(1) -> <<"PCI">>;
spdm_device_type_name(2) -> <<"USB">>;
spdm_device_type_name(_) -> <<"unknown">>.

%% UEFI 2.10 section 32.5.3 SubHeaderType enumeration.
spdm_sub_header_type_name(0) -> <<"SPDM_MEAS_BLOCK">>;
spdm_sub_header_type_name(1) -> <<"SPDM_CERT_CHAIN">>;
spdm_sub_header_type_name(_) -> <<"unknown">>.

%% DMTF DSP0274 section 10.11.3 Table "MeasurementSpecification".
spdm_meas_spec_name(16#01) -> <<"DMTF">>;
spdm_meas_spec_name(_)     -> <<"unknown">>.

%% DMTF DSP0274 section 10.11.3 Table "DMTFSpecMeasurementValueType".
dmtf_meas_type_name(0) -> <<"immutable-rom">>;
dmtf_meas_type_name(1) -> <<"mutable-firmware">>;
dmtf_meas_type_name(2) -> <<"hardware-config">>;
dmtf_meas_type_name(3) -> <<"firmware-config">>;
dmtf_meas_type_name(4) -> <<"firmware-measurement-manifest">>;
dmtf_meas_type_name(5) -> <<"device-mode">>;
dmtf_meas_type_name(6) -> <<"version-info">>;
dmtf_meas_type_name(7) -> <<"secure-version-number">>;
dmtf_meas_type_name(_) -> <<"unknown">>.

%% SPDM BaseHashAlgo per DMTF DSP0274 section 10.6.2 bitmap.
spdm_hash_alg_name(16#00000001) -> <<"spdm-sha-256">>;
spdm_hash_alg_name(16#00000002) -> <<"spdm-sha-384">>;
spdm_hash_alg_name(16#00000004) -> <<"spdm-sha-512">>;
spdm_hash_alg_name(16#00000008) -> <<"spdm-sha3-256">>;
spdm_hash_alg_name(16#00000010) -> <<"spdm-sha3-384">>;
spdm_hash_alg_name(16#00000020) -> <<"spdm-sha3-512">>;
spdm_hash_alg_name(16#00000040) -> <<"spdm-sm3-256">>;
spdm_hash_alg_name(_)           -> <<"spdm-unknown">>.

%% Windows SIPA / WBCL events (0x10000000 + subtype). Structure per
%% `windows-ic-sipa.h` header in the Windows SDK and Microsoft's
%% published TCGLogTools notes:
%%   SIPA_EVENT_HEADER:
%%     EventType  u32 LE
%%     EventSize  u32 LE
%%     EventData  [EventSize]
%% The outer TCG event type uniquely identifies the SIPA category
%% (e.g. 0x10000004 = SIPA_EVENTTYPE_TRUSTBOUNDARY). We surface a
%% category name + SIPA sub-event-type + raw data length.
decode_sipa_event(Code, #{<<"event-data">> := Data}) ->
    Category = sipa_category(Code),
    case Data of
        <<SubType:32/little, SubSize:32/little, Rest/binary>>
          when SubSize =< byte_size(Rest) + 0 ->  %% loose bounds
            Payload = case Rest of
                <<P:SubSize/binary, _/binary>> -> P;
                _ -> Rest
            end,
            Base = #{<<"sipa-category">>       => Category,
                     <<"sipa-category-code">>  => Code,
                     <<"sipa-subtype">>        => SubType,
                     <<"sipa-subtype-name">>   => sipa_subtype_name(SubType),
                     <<"sipa-data-length">>    => SubSize,
                     <<"sipa-sha256">>         =>
                         hb_util:encode(crypto:hash(sha256, Data))},
            PayloadMap = decode_sipa_payload(SubType, Payload),
            maps:merge(Base, PayloadMap);
        _ ->
            #{<<"sipa-category">>       => Category,
              <<"sipa-category-code">>  => Code,
              <<"data-length">>         => byte_size(Data),
              <<"sha256">>              =>
                  hb_util:encode(crypto:hash(sha256, Data))}
    end;
decode_sipa_event(_, _) -> #{}.

%% @doc Decode the per-subtype SIPA payload. Returns a map that
%% the caller merges into the SIPA base record.
%%
%% Each subtype has a fixed payload shape per Microsoft's SIPA
%% specification (see `windows-ic-sipa.h' + TCGLogTools source).
%% We classify by `sipa_subtype_payload_type/1` and decode
%% structurally; unclassified / malformed payloads return a
%% base64url hex dump so the information is preserved.
decode_sipa_payload(SubType, Payload) ->
    Type = sipa_subtype_payload_type(SubType),
    Base = #{<<"sipa-payload-type">> => Type},
    maps:merge(Base, decode_sipa_payload_body(Type, Payload)).

decode_sipa_payload_body(<<"bool">>, <<B:8, _/binary>>) ->
    #{<<"sipa-value-bool">> => B =/= 0};
decode_sipa_payload_body(<<"u32">>, <<V:32/little, _/binary>>) ->
    #{<<"sipa-value-u32">> => V,
      <<"sipa-value-u32-hex">> =>
          iolist_to_binary(io_lib:format("0x~8.16.0B", [V]))};
decode_sipa_payload_body(<<"u64">>, <<V:64/little, _/binary>>) ->
    #{<<"sipa-value-u64">> => V,
      <<"sipa-value-u64-hex">> =>
          iolist_to_binary(io_lib:format("0x~16.16.0B", [V]))};
decode_sipa_payload_body(<<"digest">>, Payload)
    when byte_size(Payload) >= 20 ->
    Size = byte_size(Payload),
    Alg = case Size of
        20 -> <<"sha1">>;
        32 -> <<"sha256">>;
        48 -> <<"sha384">>;
        64 -> <<"sha512">>;
        _  -> <<"unknown">>
    end,
    #{<<"sipa-value-digest">>      => hb_util:encode(Payload),
      <<"sipa-value-digest-alg">>  => Alg,
      <<"sipa-value-digest-size">> => Size};
decode_sipa_payload_body(<<"utf16-string">>, Payload) ->
    Trimmed = strip_trailing_utf16_nulls(Payload),
    Decoded = try
        unicode:characters_to_binary(Trimmed, {utf16, little}, utf8)
    catch _:_ -> Trimmed end,
    case is_binary(Decoded) of
        true  -> #{<<"sipa-value-string">> => Decoded};
        false -> #{<<"sipa-value-string">> => Trimmed,
                   <<"sipa-value-string-error">> =>
                       <<"utf16-decode-failed">>}
    end;
decode_sipa_payload_body(<<"aggregation">>, Payload) ->
    %% Aggregations carry nested SIPA events. Surface the
    %% payload length + sha256 so a verifier can pin the whole
    %% blob; a future iteration can recurse in.
    #{<<"sipa-aggregation-length">>  => byte_size(Payload),
      <<"sipa-aggregation-sha256">>  =>
          hb_util:encode(crypto:hash(sha256, Payload))};
decode_sipa_payload_body(_, Payload) ->
    #{<<"sipa-payload-bytes">> => byte_size(Payload),
      <<"sipa-payload-base64url">> => hb_util:encode(Payload)}.

%% Strip trailing UTF-16LE NUL pairs (0x00 0x00).
strip_trailing_utf16_nulls(Bin) ->
    strip_trailing_utf16_nulls_(Bin).

strip_trailing_utf16_nulls_(B) when byte_size(B) >= 2 ->
    Sz = byte_size(B),
    case binary:part(B, Sz - 2, 2) of
        <<0, 0>> -> strip_trailing_utf16_nulls_(
                      binary:part(B, 0, Sz - 2));
        _        -> B
    end;
strip_trailing_utf16_nulls_(B) -> B.

%% @doc Classify a SIPA sub-event-type by its expected payload
%% shape. Authoritative source: Microsoft's `windows-ic-sipa.h'
%% (Windows SDK) + TCGLogTools' per-subtype decode tables.
%% Any subtype not explicitly listed falls back to "opaque".
sipa_subtype_payload_type(16#00010000) -> <<"bool">>;      % FirmwareDebug
sipa_subtype_payload_type(16#00010001) -> <<"bool">>;      % OsKernelDebug
sipa_subtype_payload_type(16#00010002) -> <<"bool">>;      % CodeIntegrity
sipa_subtype_payload_type(16#00010003) -> <<"bool">>;      % TestSigning
sipa_subtype_payload_type(16#00010004) -> <<"bool">>;      % DataExecutionPrevention
sipa_subtype_payload_type(16#00010005) -> <<"bool">>;      % SafeMode
sipa_subtype_payload_type(16#00010006) -> <<"bool">>;      % WinPE
sipa_subtype_payload_type(16#00010007) -> <<"bool">>;      % PhysicalPresence
sipa_subtype_payload_type(16#00010008) -> <<"digest">>;    % DevicePIDHash
sipa_subtype_payload_type(16#00010009) -> <<"u64">>;       % DevicePIDValue
sipa_subtype_payload_type(16#0001000A) -> <<"u32">>;       % BootCounter
sipa_subtype_payload_type(16#0001000B) -> <<"aggregation">>;% BootRevocationList
sipa_subtype_payload_type(16#0001000C) -> <<"bool">>;      % OsKernelDebugPolicy
sipa_subtype_payload_type(16#0001000D) -> <<"u32">>;       % DriverLoadPolicy
sipa_subtype_payload_type(16#0001000E) -> <<"bool">>;      % BitLockerUnlock
sipa_subtype_payload_type(16#0001000F) -> <<"bool">>;      % LastBootSucceeded
sipa_subtype_payload_type(16#00010010) -> <<"bool">>;      % LastShutdownSucceeded
sipa_subtype_payload_type(16#00010011) -> <<"aggregation">>;% EvAggregationKsr
sipa_subtype_payload_type(16#00010012) -> <<"bool">>;      % ImageValidated
sipa_subtype_payload_type(16#00010020) -> <<"bool">>;      % BitLockerDataVolumes
sipa_subtype_payload_type(16#00010021) -> <<"bool">>;      % BootDebugging
sipa_subtype_payload_type(16#00010022) -> <<"bool">>;      % BootRevocationsPublished
sipa_subtype_payload_type(16#00010023) -> <<"bool">>;      % BootRevocationsDisabled
sipa_subtype_payload_type(16#00010024) -> <<"bool">>;      % Vbs
sipa_subtype_payload_type(16#00010025) -> <<"bool">>;      % VbsVsmRequired
sipa_subtype_payload_type(16#00010026) -> <<"bool">>;      % VbsSecurebootRequired
sipa_subtype_payload_type(16#00010027) -> <<"bool">>;      % VbsIumEnabled
sipa_subtype_payload_type(16#00010028) -> <<"bool">>;      % VbsMmioNxSupported
sipa_subtype_payload_type(16#00010029) -> <<"bool">>;      % VbsApicVirtSupported
sipa_subtype_payload_type(16#0001002A) -> <<"bool">>;      % VbsTpmRequired
sipa_subtype_payload_type(16#0001002B) -> <<"bool">>;      % VbsHvciStrictMode
sipa_subtype_payload_type(16#0001002C) -> <<"bool">>;      % VbsDepLaunch
sipa_subtype_payload_type(16#0001002D) -> <<"u32">>;       % VbsMinProcessorVersion
sipa_subtype_payload_type(16#0001002E) -> <<"bool">>;      % HibrBoot
sipa_subtype_payload_type(16#0001002F) -> <<"bool">>;      % BitLocker
sipa_subtype_payload_type(16#00010030) -> <<"u32">>;       % CodeIntegrityBehaviour
sipa_subtype_payload_type(16#00010031) -> <<"bool">>;      % ElamEnabled
sipa_subtype_payload_type(16#00010032) -> <<"bool">>;      % ElamBootClean
sipa_subtype_payload_type(16#00010033) -> <<"bool">>;      % ElamInitialized
sipa_subtype_payload_type(16#00010034) -> <<"bool">>;      % DmaProtection
sipa_subtype_payload_type(16#00010035) -> <<"bool">>;      % SystemGuardBootEnabled
sipa_subtype_payload_type(16#00020001) -> <<"utf16-string">>;% ElamKeyname
sipa_subtype_payload_type(16#00020002) -> <<"digest">>;    % ElamConfigurationHash
sipa_subtype_payload_type(16#00020003) -> <<"digest">>;    % ElamPolicyHash
sipa_subtype_payload_type(16#00020004) -> <<"digest">>;    % ElamMeasuredSignatureHash
sipa_subtype_payload_type(16#00030001) -> <<"aggregation">>;% LoadedModuleAggregation
sipa_subtype_payload_type(16#00030002) -> <<"utf16-string">>;% LoadedModuleName
sipa_subtype_payload_type(16#00030003) -> <<"digest">>;    % LoadedModuleHash
sipa_subtype_payload_type(16#00030004) -> <<"utf16-string">>;% LoadedModuleVersion
sipa_subtype_payload_type(16#00030005) -> <<"bool">>;      % LoadedModuleNtOsBoot
sipa_subtype_payload_type(16#00040001) -> <<"aggregation">>;% TrustBoundaryAggregation
sipa_subtype_payload_type(16#00040002) -> <<"digest">>;    % SbcpHash
sipa_subtype_payload_type(16#00040003) -> <<"digest">>;    % CertSignerHash
sipa_subtype_payload_type(16#00040004) -> <<"digest">>;    % CertRootHash
sipa_subtype_payload_type(16#00040005) -> <<"utf16-string">>;% CertPolicy
sipa_subtype_payload_type(16#00040006) -> <<"u32">>;       % CertKeyAttributes
sipa_subtype_payload_type(16#00050001) -> <<"digest">>;    % KsrSignature
sipa_subtype_payload_type(16#00050002) -> <<"aggregation">>;% KsrAggregation
sipa_subtype_payload_type(_) -> <<"opaque">>.

%% SIPA outer category codes (Microsoft Windows boot-log schema).
%% Source: TCGLogTools PowerShell module, Windows SDK headers.
sipa_category(16#10000001) -> <<"SIPA_EVENTTYPE_TRUSTPOINT">>;
sipa_category(16#10000002) -> <<"SIPA_EVENTTYPE_ERROR">>;
sipa_category(16#10000003) -> <<"SIPA_EVENTTYPE_PREOSPARAMETER">>;
sipa_category(16#10000004) -> <<"SIPA_EVENTTYPE_OSPARAMETER">>;
sipa_category(16#10000005) -> <<"SIPA_EVENTTYPE_AUTHORITY">>;
sipa_category(16#10000006) -> <<"SIPA_EVENTTYPE_LOADEDMODULE">>;
sipa_category(16#10000007) -> <<"SIPA_EVENTTYPE_TRUSTBOUNDARY">>;
sipa_category(16#10000008) -> <<"SIPA_EVENTTYPE_ELAMAGGREGATION">>;
sipa_category(16#10000009) -> <<"SIPA_EVENTTYPE_LOADEDMODULEAGGREGATION">>;
sipa_category(16#1000000A) -> <<"SIPA_EVENTTYPE_TRUSTPOINT_AGGREGATION">>;
sipa_category(16#1000000B) -> <<"SIPA_EVENTTYPE_ELAM_CERTIFICATE">>;
sipa_category(16#1000000C) -> <<"SIPA_EVENTTYPE_VBS_MEASUREMENTS">>;
sipa_category(16#1000000D) -> <<"SIPA_EVENTTYPE_KSR_SIGNATURE">>;
sipa_category(16#1000000E) -> <<"SIPA_EVENTTYPE_KSR_AGGREGATION">>;
sipa_category(_) -> <<"SIPA_EVENTTYPE_UNKNOWN">>.

%% SIPA sub-event-types (inner 32-bit header). Windows defines
%% 60+ of these across PCR 11-14. Names per TCGLogTools.
%% We cover the most-commonly-attacked / policy-relevant ones.
sipa_subtype_name(16#00010000) -> <<"FirmwareDebug">>;
sipa_subtype_name(16#00010001) -> <<"OsKernelDebug">>;
sipa_subtype_name(16#00010002) -> <<"CodeIntegrity">>;
sipa_subtype_name(16#00010003) -> <<"TestSigning">>;
sipa_subtype_name(16#00010004) -> <<"DataExecutionPrevention">>;
sipa_subtype_name(16#00010005) -> <<"SafeMode">>;
sipa_subtype_name(16#00010006) -> <<"WinPE">>;
sipa_subtype_name(16#00010007) -> <<"PhysicalPresence">>;
sipa_subtype_name(16#00010008) -> <<"DevicePIDHash">>;
sipa_subtype_name(16#00010009) -> <<"DevicePIDValue">>;
sipa_subtype_name(16#0001000A) -> <<"BootCounter">>;
sipa_subtype_name(16#0001000B) -> <<"BootRevocationList">>;
sipa_subtype_name(16#0001000C) -> <<"OsKernelDebugPolicy">>;
sipa_subtype_name(16#0001000D) -> <<"DriverLoadPolicy">>;
sipa_subtype_name(16#0001000E) -> <<"BitLockerUnlock">>;
sipa_subtype_name(16#0001000F) -> <<"LastBootSucceeded">>;
sipa_subtype_name(16#00010010) -> <<"LastShutdownSucceeded">>;
sipa_subtype_name(16#00010011) -> <<"EvAggregationKsr">>;
sipa_subtype_name(16#00010012) -> <<"ImageValidated">>;
sipa_subtype_name(16#00010020) -> <<"BitLockerDataVolumes">>;
sipa_subtype_name(16#00010021) -> <<"BootDebugging">>;
sipa_subtype_name(16#00010022) -> <<"BootRevocationsPublished">>;
sipa_subtype_name(16#00010023) -> <<"BootRevocationsDisabled">>;
sipa_subtype_name(16#00010024) -> <<"Vbs">>;
sipa_subtype_name(16#00010025) -> <<"VbsVsmRequired">>;
sipa_subtype_name(16#00010026) -> <<"VbsSecurebootRequired">>;
sipa_subtype_name(16#00010027) -> <<"VbsIumEnabled">>;
sipa_subtype_name(16#00010028) -> <<"VbsMmioNxSupported">>;
sipa_subtype_name(16#00010029) -> <<"VbsApicVirtSupported">>;
sipa_subtype_name(16#0001002A) -> <<"VbsTpmRequired">>;
sipa_subtype_name(16#0001002B) -> <<"VbsHvciStrictMode">>;
sipa_subtype_name(16#0001002C) -> <<"VbsDepLaunch">>;
sipa_subtype_name(16#0001002D) -> <<"VbsMinProcessorVersion">>;
sipa_subtype_name(16#0001002E) -> <<"HibrBoot">>;
sipa_subtype_name(16#0001002F) -> <<"BitLocker">>;
sipa_subtype_name(16#00010030) -> <<"CodeIntegrityBehaviour">>;
sipa_subtype_name(16#00010031) -> <<"ElamEnabled">>;
sipa_subtype_name(16#00010032) -> <<"ElamBootClean">>;
sipa_subtype_name(16#00010033) -> <<"ElamInitialized">>;
sipa_subtype_name(16#00010034) -> <<"DmaProtection">>;
sipa_subtype_name(16#00010035) -> <<"SystemGuardBootEnabled">>;
sipa_subtype_name(16#00020001) -> <<"ElamKeyname">>;
sipa_subtype_name(16#00020002) -> <<"ElamConfigurationHash">>;
sipa_subtype_name(16#00020003) -> <<"ElamPolicyHash">>;
sipa_subtype_name(16#00020004) -> <<"ElamMeasuredSignatureHash">>;
sipa_subtype_name(16#00030001) -> <<"LoadedModuleAggregation">>;
sipa_subtype_name(16#00030002) -> <<"LoadedModuleName">>;
sipa_subtype_name(16#00030003) -> <<"LoadedModuleHash">>;
sipa_subtype_name(16#00030004) -> <<"LoadedModuleVersion">>;
sipa_subtype_name(16#00030005) -> <<"LoadedModuleNtOsBoot">>;
sipa_subtype_name(16#00040001) -> <<"TrustBoundaryAggregation">>;
sipa_subtype_name(16#00040002) -> <<"SbcpHash">>;
sipa_subtype_name(16#00040003) -> <<"CertSignerHash">>;
sipa_subtype_name(16#00040004) -> <<"CertRootHash">>;
sipa_subtype_name(16#00040005) -> <<"CertPolicy">>;
sipa_subtype_name(16#00040006) -> <<"CertKeyAttributes">>;
sipa_subtype_name(16#00050001) -> <<"KsrSignature">>;
sipa_subtype_name(16#00050002) -> <<"KsrAggregation">>;
sipa_subtype_name(_) -> <<"unknown-sipa-subtype">>.

%% EV_EVENT_TAG -- `TCG_PCClientTaggedEvent` per TCG PC Client PFP section 5.
%% Layout:
%%   taggedEventID        u32 LE
%%   taggedEventDataSize  u32 LE
%%   taggedEventData      [taggedEventDataSize]
%%
%% systemd-stub (sd-stub) reserves 5 well-known tag IDs for its UKI
%% measurement annotations on PCR 11/12; we recognise them inline so
%% the derived surface is sd-stub-aware. Vendor firmware uses this
%% mechanism for QEMU / Intel TXT / SMM init markers as well.
decode_event_tag(#{<<"event-data">> := Data})
  when byte_size(Data) >= 8 ->
    <<TagId:32/little, TagSize:32/little, Rest/binary>> = Data,
    Actual = case Rest of
        <<Payload:TagSize/binary, _/binary>> -> Payload;
        _ -> Rest
    end,
    Name = tagged_event_id_name(TagId),
    Base = #{
        <<"tag-id">>            => TagId,
        <<"tag-id-hex">>        =>
            iolist_to_binary(io_lib:format("0x~8.16.0B", [TagId])),
        <<"tag-id-name">>       => Name,
        <<"tag-data-length">>   => byte_size(Actual)
    },
    %% Decode systemd-stub tag payloads -- they're UTF-16LE human-
    %% readable strings describing what was measured.
    case is_systemd_stub_tag(TagId) of
        true ->
            %% Strip trailing NULs and try UTF-16LE->UTF-8.
            Descr = try
                unicode:characters_to_binary(Actual,
                                              {utf16, little}, utf8)
            catch _:_ -> Actual end,
            Base#{<<"tag-description">> =>
                      case is_binary(Descr) of
                          true -> strip_trailing_nulls(Descr);
                          false -> Actual
                      end};
        false -> Base
    end;
decode_event_tag(_) -> #{}.

%% systemd-stub TCG_PCClientTaggedEvent IDs (src/boot/measure.h).
is_systemd_stub_tag(16#f5bc582a) -> true;  % LOADER_CONF
is_systemd_stub_tag(16#6c46f751) -> true;  % DEVICETREE_ADDON
is_systemd_stub_tag(16#49dffe0f) -> true;  % INITRD_ADDON
is_systemd_stub_tag(16#dac08e1a) -> true;  % UCODE_ADDON
is_systemd_stub_tag(16#13aed6db) -> true;  % UKI_PROFILE
is_systemd_stub_tag(_) -> false.

tagged_event_id_name(16#f5bc582a) ->
    <<"SYSTEMD_STUB_LOADER_CONF">>;
tagged_event_id_name(16#6c46f751) ->
    <<"SYSTEMD_STUB_DEVICETREE_ADDON">>;
tagged_event_id_name(16#49dffe0f) ->
    <<"SYSTEMD_STUB_INITRD_ADDON">>;
tagged_event_id_name(16#dac08e1a) ->
    <<"SYSTEMD_STUB_UCODE_ADDON">>;
tagged_event_id_name(16#13aed6db) ->
    <<"SYSTEMD_STUB_UKI_PROFILE">>;
tagged_event_id_name(_) -> <<"unknown-tag-id">>.

%% Format a 16-byte EFI_GUID as the canonical
%% `XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX' lowercase string.
%% EFI_GUID is mixed-endian: {u32-LE, u16-LE, u16-LE, byte[2], byte[6]}.
format_guid(<<A:32/little, B:16/little, C:16/little,
              D1:8, D2:8, E:6/binary>>) ->
    iolist_to_binary(io_lib:format(
        "~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b",
        [A, B, C, D1, D2, binary:decode_unsigned(E)]));
format_guid(_) -> <<"malformed-guid">>.

%%%============================================================================
%%% UEFI device path walker (UEFI spec section 10)
%%%============================================================================
%%%
%%% A UEFI device path is a linked list of typed variable-length
%%% nodes. Each node is:
%%%     Type     u8
%%%     SubType  u8
%%%     Length   u16 LE  (total node length incl. header)
%%%     Data     [Length-4]
%%% Terminated by an End node: Type=0x7F, SubType=0x01 (end this
%%% instance) or 0xFF (end entire path), Length=0x0004.
%%%
%%% parse_device_path/1 returns {Nodes, Text} -- Nodes is the
%%% structured parse (a list of maps, one per node), Text is the
%%% canonical UEFI textual rendering (e.g.
%%% "PciRoot(0x0)/Pci(0x1F,0x2)/Sata(0,0,0)/HD(1,GPT,<guid>,0x...,0x...)/\\EFI\\BOOT\\BOOTX64.EFI").
%%%
%%% Types per UEFI section 10:
%%%   0x01  Hardware           (PCI, PCCARD, memmap, vendor, controller, BMC)
%%%   0x02  ACPI               (ACPI, expanded ACPI, ADR)
%%%   0x03  Messaging          (SATA, SCSI, USB, MAC, IPv4/6, UART, NVMe, ...)
%%%   0x04  Media              (HardDrive, CD-ROM, file path, PIWG FV/FF, ...)
%%%   0x05  BIOS Boot Spec     (legacy option)
%%%   0x7F  End
parse_device_path(Bin) when is_binary(Bin) ->
    parse_device_path_nodes(Bin, []);
parse_device_path(_) -> {[], <<>>}.

parse_device_path_nodes(<<Type:8, SubType:8, Len:16/little, Rest/binary>>,
                        Acc) when Len >= 4 ->
    DataLen = Len - 4,
    case Rest of
        <<Data:DataLen/binary, Rest2/binary>> ->
            Node = decode_dp_node(Type, SubType, Data),
            case {Type, SubType} of
                {16#7F, _} ->
                    Nodes = lists:reverse([Node | Acc]),
                    {Nodes, render_device_path(Nodes)};
                _ ->
                    parse_device_path_nodes(Rest2, [Node | Acc])
            end;
        _ ->
            Nodes = lists:reverse(
                [#{<<"type">> => Type, <<"subtype">> => SubType,
                   <<"error">> => <<"truncated">>} | Acc]),
            {Nodes, render_device_path(Nodes)}
    end;
parse_device_path_nodes(_, Acc) ->
    Nodes = lists:reverse(Acc),
    {Nodes, render_device_path(Nodes)}.

%% Per-node decode. Returns a map with `type`, `subtype`,
%% `type-name`, `subtype-name`, plus sub-type-specific fields.

%%---- Hardware (Type 0x01) -----------------------------------------
decode_dp_node(16#01, 16#01, <<Function:8, Device:8>>) ->
    dp_node(16#01, 16#01, <<"pci">>,
        #{<<"function">> => Function, <<"device">> => Device});
decode_dp_node(16#01, 16#02, <<FuncNum:8>>) ->
    dp_node(16#01, 16#02, <<"pccard">>, #{<<"function">> => FuncNum});
decode_dp_node(16#01, 16#03,
               <<MemType:32/little, Start:64/little, End:64/little>>) ->
    dp_node(16#01, 16#03, <<"memory-mapped">>,
        #{<<"memory-type">> => MemType, <<"start-address">> => Start,
          <<"end-address">> => End});
decode_dp_node(16#01, 16#04, <<Guid:16/binary, Data/binary>>) ->
    dp_node(16#01, 16#04, <<"hw-vendor">>,
        #{<<"vendor-guid">> => format_guid(Guid),
          <<"vendor-data-length">> => byte_size(Data)});
decode_dp_node(16#01, 16#05, <<Controller:32/little>>) ->
    dp_node(16#01, 16#05, <<"controller">>,
        #{<<"controller-number">> => Controller});
decode_dp_node(16#01, 16#06, <<IfaceType:8, BaseAddr:64/little>>) ->
    dp_node(16#01, 16#06, <<"bmc">>,
        #{<<"interface-type">> => IfaceType,
          <<"base-address">> => BaseAddr});

%%---- ACPI (Type 0x02) ---------------------------------------------
decode_dp_node(16#02, 16#01, <<HID:32/little, UID:32/little>>) ->
    dp_node(16#02, 16#01, <<"acpi">>,
        #{<<"hid">> => HID, <<"uid">> => UID,
          <<"hid-string">> => acpi_hid_to_string(HID)});
decode_dp_node(16#02, 16#02, <<HID:32/little, UID:32/little,
                               CID:32/little, Rest/binary>>) ->
    %% HIDSTR, UIDSTR, CIDSTR -- three NUL-terminated ASCII strings.
    {HidStr, Rest1} = read_nul_ascii(Rest),
    {UidStr, Rest2} = read_nul_ascii(Rest1),
    {CidStr, _}     = read_nul_ascii(Rest2),
    dp_node(16#02, 16#02, <<"acpi-expanded">>,
        #{<<"hid">> => HID, <<"uid">> => UID, <<"cid">> => CID,
          <<"hid-string">> => HidStr,
          <<"uid-string">> => UidStr,
          <<"cid-string">> => CidStr});
decode_dp_node(16#02, 16#03, Data) ->
    %% ADR -- array of u32 ADR values.
    ADRs = [X || <<X:32/little>> <= Data],
    dp_node(16#02, 16#03, <<"acpi-adr">>, #{<<"adrs">> => ADRs});

%%---- Messaging (Type 0x03) ----------------------------------------
decode_dp_node(16#03, 16#01, <<Primary:8, Slave:8, LUN:16/little>>) ->
    dp_node(16#03, 16#01, <<"atapi">>,
        #{<<"primary">> => Primary == 1,
          <<"slave">> => Slave == 1, <<"lun">> => LUN});
decode_dp_node(16#03, 16#02, <<TargetId:16/little, LUN:16/little>>) ->
    dp_node(16#03, 16#02, <<"scsi">>,
        #{<<"target-id">> => TargetId, <<"lun">> => LUN});
decode_dp_node(16#03, 16#03, <<_Reserved:32/little, WWN:64/little,
                               LUN:64/little>>) ->
    dp_node(16#03, 16#03, <<"fibre-channel">>,
        #{<<"wwn">> => WWN, <<"lun">> => LUN});
decode_dp_node(16#03, 16#05, <<Parent:8, Iface:8>>) ->
    dp_node(16#03, 16#05, <<"usb">>,
        #{<<"parent-port">> => Parent,
          <<"interface">> => Iface});
decode_dp_node(16#03, 16#0A, <<Guid:16/binary, Data/binary>>) ->
    dp_node(16#03, 16#0A, <<"msg-vendor">>,
        #{<<"vendor-guid">> => format_guid(Guid),
          <<"vendor-data-length">> => byte_size(Data)});
decode_dp_node(16#03, 16#0B, <<Mac:32/binary, IfType:8>>) ->
    %% 32 bytes fixed even though MAC is 6 -- padded.
    MacBin = binary:part(Mac, 0, 6),
    dp_node(16#03, 16#0B, <<"mac-addr">>,
        #{<<"mac">> => format_mac(MacBin),
          <<"if-type">> => IfType});
decode_dp_node(16#03, 16#0C,
               <<LocalIP:4/binary, RemoteIP:4/binary,
                 LocalPort:16/little, RemotePort:16/little,
                 Protocol:16/little, Static:8, GatewayIP:4/binary,
                 SubnetMask:4/binary>>) ->
    dp_node(16#03, 16#0C, <<"ipv4">>,
        #{<<"local-ip">> => format_ipv4(LocalIP),
          <<"remote-ip">> => format_ipv4(RemoteIP),
          <<"local-port">> => LocalPort,
          <<"remote-port">> => RemotePort,
          <<"protocol">> => Protocol,
          <<"static">> => Static == 0,
          <<"gateway-ip">> => format_ipv4(GatewayIP),
          <<"subnet-mask">> => format_ipv4(SubnetMask)});
decode_dp_node(16#03, 16#0D, Data) ->
    %% IPv6: LocalIP 16B, RemoteIP 16B, LocalPort u16, RemotePort u16,
    %% Protocol u16, IPAddrOrigin u8, PrefixLen u8, GatewayIP 16B.
    case Data of
        <<LocalIP:16/binary, RemoteIP:16/binary,
          LocalPort:16/little, RemotePort:16/little,
          Protocol:16/little, Origin:8, Prefix:8,
          Gateway:16/binary>> ->
            dp_node(16#03, 16#0D, <<"ipv6">>,
                #{<<"local-ip">> => format_ipv6(LocalIP),
                  <<"remote-ip">> => format_ipv6(RemoteIP),
                  <<"local-port">> => LocalPort,
                  <<"remote-port">> => RemotePort,
                  <<"protocol">> => Protocol,
                  <<"ip-addr-origin">> => Origin,
                  <<"prefix-length">> => Prefix,
                  <<"gateway-ip">> => format_ipv6(Gateway)});
        _ -> dp_node(16#03, 16#0D, <<"ipv6">>, #{})
    end;
decode_dp_node(16#03, 16#0E, <<_Reserved:32/little, Baud:64/little,
                               Data:8, Parity:8, Stop:8>>) ->
    dp_node(16#03, 16#0E, <<"uart">>,
        #{<<"baud-rate">> => Baud, <<"data-bits">> => Data,
          <<"parity">> => Parity, <<"stop-bits">> => Stop});
decode_dp_node(16#03, 16#12, <<HbaPort:16/little, PmpPort:16/little,
                               LUN:16/little>>) ->
    dp_node(16#03, 16#12, <<"sata">>,
        #{<<"hba-port">> => HbaPort,
          <<"pmp-port">> => PmpPort, <<"lun">> => LUN});
decode_dp_node(16#03, 16#13, Data) ->
    %% iSCSI -- variable; capture fixed prefix.
    case Data of
        <<Protocol:16/little, Options:16/little, LUN:64/little,
          Tpgt:16/little, Rest/binary>> ->
            dp_node(16#03, 16#13, <<"iscsi">>,
                #{<<"protocol">> => Protocol,
                  <<"login-options">> => Options,
                  <<"lun">> => LUN,
                  <<"target-portal-group">> => Tpgt,
                  <<"target-name">> => Rest});
        _ -> dp_node(16#03, 16#13, <<"iscsi">>, #{})
    end;
decode_dp_node(16#03, 16#17, <<NsId:32/little, EUI:64/little>>) ->
    dp_node(16#03, 16#17, <<"nvme-ns">>,
        #{<<"namespace-id">> => NsId,
          <<"ieee-eui-64">> => EUI});
decode_dp_node(16#03, 16#18, Data) ->
    %% URI path -- variable UTF-8 string.
    dp_node(16#03, 16#18, <<"uri">>, #{<<"uri">> => Data});
decode_dp_node(16#03, 16#1F, <<IsIpv6:8, Rest/binary>>) ->
    dp_node(16#03, 16#1F, <<"dns">>,
        #{<<"is-ipv6">> => IsIpv6 == 1,
          <<"dns-data-length">> => byte_size(Rest)});
decode_dp_node(16#03, 16#04,
               <<_Reserved:32/little, Guid:8/binary>>) ->
    %% IEEE 1394 (Firewire). Reserved u32 + GUID u64.
    dp_node(16#03, 16#04, <<"firewire-1394">>,
        #{<<"guid">> => format_hex(Guid)});
decode_dp_node(16#03, 16#06, <<TargetId:32/little>>) ->
    %% I2O.
    dp_node(16#03, 16#06, <<"i2o">>,
        #{<<"target-id">> => TargetId});
decode_dp_node(16#03, 16#09,
               <<ResFlags:32/little, PortGid:16/binary,
                 IocGuid:8/binary, TargetPortIdGuid:8/binary,
                 DeviceId:8/binary>>) ->
    %% Infiniband.
    dp_node(16#03, 16#09, <<"infiniband">>,
        #{<<"resource-flags">> => ResFlags,
          <<"port-gid">> => format_hex(PortGid),
          <<"ioc-guid">> => format_hex(IocGuid),
          <<"target-port-id-guid">> => format_hex(TargetPortIdGuid),
          <<"device-id">> => format_hex(DeviceId)});
decode_dp_node(16#03, 16#0F, <<VID:16/little, PID:16/little,
                               Class:8, SubClass:8, Protocol:8>>) ->
    %% USB Class.
    dp_node(16#03, 16#0F, <<"usb-class">>,
        #{<<"vendor-id">> => VID,
          <<"product-id">> => PID,
          <<"class">> => Class,
          <<"subclass">> => SubClass,
          <<"protocol">> => Protocol});
decode_dp_node(16#03, 16#10, Data) ->
    %% USB WWID -- Interface u16, VID u16, PID u16, SerialNumber[].
    case Data of
        <<Iface:16/little, VID:16/little, PID:16/little,
          SerialBin/binary>> ->
            dp_node(16#03, 16#10, <<"usb-wwid">>,
                #{<<"interface-number">> => Iface,
                  <<"vendor-id">> => VID,
                  <<"product-id">> => PID,
                  <<"serial-number">> => ucs2_to_utf8(SerialBin)});
        _ -> dp_node(16#03, 16#10, <<"usb-wwid">>, #{})
    end;
decode_dp_node(16#03, 16#11, <<LUN:8>>) ->
    %% Logical Unit (a SCSI subchild).
    dp_node(16#03, 16#11, <<"logical-unit">>,
        #{<<"lun">> => LUN});
decode_dp_node(16#03, 16#14, <<VlanId:16/little>>) ->
    %% VLAN tag (UEFI section 10.5.12).
    dp_node(16#03, 16#14, <<"vlan">>,
        #{<<"vlan-id">> => VlanId});
decode_dp_node(16#03, 16#15,
               <<_Reserved:32/little, WWN:8/binary, LUN:8/binary>>) ->
    %% Fibre Channel Ex.
    dp_node(16#03, 16#15, <<"fibre-channel-ex">>,
        #{<<"wwn">> => format_hex(WWN),
          <<"lun">> => format_hex(LUN)});
decode_dp_node(16#03, 16#16,
               <<SasAddr:8/binary, Lun:8/binary, DeviceTopology:16/little,
                 RelativeTargetPort:16/little>>) ->
    %% SAS Ex.
    dp_node(16#03, 16#16, <<"sas-ex">>,
        #{<<"sas-address">> => format_hex(SasAddr),
          <<"lun">> => format_hex(Lun),
          <<"device-topology">> => DeviceTopology,
          <<"relative-target-port">> => RelativeTargetPort});
decode_dp_node(16#03, 16#19, <<Target:8, Lun:8>>) ->
    %% UFS.
    dp_node(16#03, 16#19, <<"ufs">>,
        #{<<"target-id">> => Target, <<"lun">> => Lun});
decode_dp_node(16#03, 16#1A, <<Slot:8>>) ->
    %% SD.
    dp_node(16#03, 16#1A, <<"sd">>, #{<<"slot-number">> => Slot});
decode_dp_node(16#03, 16#1B, BD) ->
    %% Bluetooth BR/EDR -- BD_ADDR 6 bytes.
    dp_node(16#03, 16#1B, <<"bluetooth">>,
        #{<<"bd-addr">> =>
              case BD of
                  <<B:6/binary, _/binary>> -> format_mac(B);
                  _ -> <<"">>
              end});
decode_dp_node(16#03, 16#1C, Ssid) ->
    %% WiFi -- SSID 32 bytes.
    Ssid0 = binary_part(Ssid, 0, min(32, byte_size(Ssid))),
    dp_node(16#03, 16#1C, <<"wifi">>,
        #{<<"ssid">> => strip_trailing_nulls(Ssid0)});
decode_dp_node(16#03, 16#1D, <<Slot:8>>) ->
    dp_node(16#03, 16#1D, <<"emmc">>, #{<<"slot-number">> => Slot});
decode_dp_node(16#03, 16#1E, <<BdAddr:6/binary, AddrType:8>>) ->
    dp_node(16#03, 16#1E, <<"bluetooth-le">>,
        #{<<"bd-addr">> => format_mac(BdAddr),
          <<"address-type">> => AddrType});
decode_dp_node(16#03, 16#20, <<Uuid:16/binary>>) ->
    dp_node(16#03, 16#20, <<"nvdimm-namespace">>,
        #{<<"uuid">> => format_guid(Uuid)});
decode_dp_node(16#03, 16#21, <<SvcType:8, AccessMode:8, VendorGuid:16/binary,
                               Rest/binary>>) ->
    dp_node(16#03, 16#21, <<"rest-service">>,
        #{<<"service-type">> => SvcType,
          <<"access-mode">> => AccessMode,
          <<"vendor-guid">> => format_guid(VendorGuid),
          <<"vendor-data-length">> => byte_size(Rest)});

%%---- Media (Type 0x04) --------------------------------------------
decode_dp_node(16#04, 16#01,
               <<PartNum:32/little, PartStart:64/little,
                 PartSize:64/little, Sig:16/binary,
                 PartFormat:8, SigType:8>>) ->
    %% Hard Drive. Sig interpretation depends on SigType:
    %%   0 = no signature, 1 = MBR (first 4 bytes = MBR serial),
    %%   2 = GPT (full 16-byte disk GUID).
    SigValue = case SigType of
        16#02 -> format_guid(Sig);                 %% GPT
        16#01 -> format_hex(binary:part(Sig, 0, 4));  %% MBR
        _     -> format_hex(Sig)
    end,
    dp_node(16#04, 16#01, <<"hard-drive">>,
        #{<<"partition-number">> => PartNum,
          <<"partition-start-lba">> => PartStart,
          <<"partition-size-lba">> => PartSize,
          <<"partition-signature">> => SigValue,
          <<"partition-format">> => hd_format(PartFormat),
          <<"signature-type">> => hd_sig_type(SigType)});
decode_dp_node(16#04, 16#02,
               <<BootEntry:32/little, PartStart:64/little,
                 PartSize:64/little>>) ->
    dp_node(16#04, 16#02, <<"cdrom">>,
        #{<<"boot-entry">> => BootEntry,
          <<"partition-start-lba">> => PartStart,
          <<"partition-size-lba">> => PartSize});
decode_dp_node(16#04, 16#03, <<Guid:16/binary, Data/binary>>) ->
    dp_node(16#04, 16#03, <<"media-vendor">>,
        #{<<"vendor-guid">> => format_guid(Guid),
          <<"vendor-data-length">> => byte_size(Data)});
decode_dp_node(16#04, 16#04, Data) ->
    %% File Path. UCS-2 NUL-terminated.
    PathBin = strip_ucs2_nul(Data),
    Utf8 = ucs2_to_utf8(PathBin),
    dp_node(16#04, 16#04, <<"file-path">>,
        #{<<"path">> => Utf8});
decode_dp_node(16#04, 16#05, <<Guid:16/binary>>) ->
    dp_node(16#04, 16#05, <<"media-protocol">>,
        #{<<"protocol-guid">> => format_guid(Guid)});
decode_dp_node(16#04, 16#06, <<Guid:16/binary>>) ->
    dp_node(16#04, 16#06, <<"piwg-fw-file">>,
        #{<<"fv-file-name">> => format_guid(Guid)});
decode_dp_node(16#04, 16#07, <<Guid:16/binary>>) ->
    dp_node(16#04, 16#07, <<"piwg-fw-volume">>,
        #{<<"fv-name">> => format_guid(Guid)});
decode_dp_node(16#04, 16#08, <<_Reserved:32/little,
                               StartOffset:64/little,
                               EndOffset:64/little>>) ->
    dp_node(16#04, 16#08, <<"relative-offset-range">>,
        #{<<"start-offset">> => StartOffset,
          <<"end-offset">> => EndOffset});
decode_dp_node(16#04, 16#09,
               <<Start:64/little, End:64/little,
                 DiskTypeGuid:16/binary, Instance:16/little>>) ->
    dp_node(16#04, 16#09, <<"ram-disk">>,
        #{<<"start-address">> => Start,
          <<"end-address">> => End,
          <<"disk-type-guid">> => format_guid(DiskTypeGuid),
          <<"instance">> => Instance});

%%---- BIOS Boot Spec (Type 0x05) -----------------------------------
decode_dp_node(16#05, 16#01, <<DevType:16/little, Status:16/little,
                               Description/binary>>) ->
    dp_node(16#05, 16#01, <<"bios-boot-spec">>,
        #{<<"device-type">> => DevType,
          <<"status-flag">> => Status,
          <<"description">> => strip_nul(Description)});

%%---- End (Type 0x7F) ----------------------------------------------
decode_dp_node(16#7F, 16#01, _) ->
    dp_node(16#7F, 16#01, <<"end-instance">>, #{});
decode_dp_node(16#7F, 16#FF, _) ->
    dp_node(16#7F, 16#FF, <<"end-entire">>, #{});
decode_dp_node(16#7F, SubType, _) ->
    dp_node(16#7F, SubType, <<"end-unknown">>, #{});

%% Catch-all -- unknown type+subtype, preserve raw data length.
decode_dp_node(Type, SubType, Data) ->
    dp_node(Type, SubType, <<"unknown">>,
        #{<<"data-length">> => byte_size(Data)}).

dp_node(Type, SubType, SubTypeName, Fields) ->
    maps:merge(
      #{<<"type">> => Type,
        <<"subtype">> => SubType,
        <<"type-name">> => dp_type_name(Type),
        <<"subtype-name">> => SubTypeName},
      Fields).

dp_type_name(16#01) -> <<"hardware">>;
dp_type_name(16#02) -> <<"acpi">>;
dp_type_name(16#03) -> <<"messaging">>;
dp_type_name(16#04) -> <<"media">>;
dp_type_name(16#05) -> <<"bios-boot-spec">>;
dp_type_name(16#7F) -> <<"end">>;
dp_type_name(_) -> <<"unknown">>.

hd_format(16#01) -> <<"mbr">>;
hd_format(16#02) -> <<"gpt">>;
hd_format(_) -> <<"unknown">>.

hd_sig_type(16#00) -> <<"none">>;
hd_sig_type(16#01) -> <<"mbr-serial">>;
hd_sig_type(16#02) -> <<"gpt-guid">>;
hd_sig_type(_)     -> <<"unknown">>.

%% Canonical ACPI _HID encoding: first 3 bytes are ASCII vendor code
%% (EISA ID compressed), last 4 hex digits are product/serial.
%% https://uefi.org/specs/ACPI/6.5/05_ACPI_Software_Programming_Model/ACPI_Software_Programming_Model.html#hardware-id
acpi_hid_to_string(HID) when is_integer(HID) ->
    %% EISA-compressed vendor + 16-bit product per ACPI section 5.6.1.2.
    %% HID is stored little-endian in the binary; as an integer the
    %% high 16 bits are the vendor and the low 16 bits are the
    %% product. The 3 vendor letters pack into 15 bits:
    %%     byte0 bits 6:2 = char1 - 'A' + 1
    %%     byte0 bits 1:0 + byte1 bits 7:5 = char2 - 'A' + 1
    %%     byte1 bits 4:0 = char3 - 'A' + 1
    Upper = (HID bsr 16) band 16#FFFF,
    Lower = HID band 16#FFFF,
    <<B0:8, B1:8>> = <<Upper:16/big>>,
    V1 = (B0 bsr 2) band 16#1F,
    V2 = ((B0 band 16#03) bsl 3) bor ((B1 bsr 5) band 16#07),
    V3 = B1 band 16#1F,
    C1 = case V1 of 0 -> $? ; _ -> V1 + $A - 1 end,
    C2 = case V2 of 0 -> $? ; _ -> V2 + $A - 1 end,
    C3 = case V3 of 0 -> $? ; _ -> V3 + $A - 1 end,
    iolist_to_binary(io_lib:format("~c~c~c~4.16.0B",
                                     [C1, C2, C3, Lower])).

format_mac(<<A,B,C,D,E,F>>) ->
    iolist_to_binary(io_lib:format(
        "~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b",
        [A,B,C,D,E,F])).

format_ipv4(<<A,B,C,D>>) ->
    iolist_to_binary(io_lib:format("~B.~B.~B.~B", [A,B,C,D])).

format_ipv6(Bin) when byte_size(Bin) =:= 16 ->
    Groups = [binary:part(Bin, I*2, 2) || I <- lists:seq(0, 7)],
    iolist_to_binary(
        lists:join(<<":">>,
            [io_lib:format("~4.16.0b",
                [binary:decode_unsigned(G)]) || G <- Groups])).

format_hex(Bin) when is_binary(Bin) ->
    iolist_to_binary([io_lib:format("~2.16.0b", [B]) || <<B:8>> <= Bin]).

strip_nul(B) ->
    case binary:split(B, <<0>>) of
        [Pre | _] -> Pre;
        _ -> B
    end.

strip_ucs2_nul(B) ->
    %% Strip trailing UCS-2 NUL (two zero bytes).
    case B of
        <<>> -> B;
        _ ->
            case binary:last(B) of
                0 when byte_size(B) >= 2 ->
                    case binary:at(B, byte_size(B) - 2) of
                        0 -> binary:part(B, 0, byte_size(B) - 2);
                        _ -> B
                    end;
                _ -> B
            end
    end.

ucs2_to_utf8(Bin) ->
    case unicode:characters_to_binary(Bin, {utf16, little}, utf8) of
        U when is_binary(U) -> U;
        _ -> Bin
    end.

read_nul_ascii(<<>>) -> {<<>>, <<>>};
read_nul_ascii(Bin) ->
    case binary:split(Bin, <<0>>) of
        [Pre, Post] -> {Pre, Post};
        [Only]      -> {Only, <<>>}
    end.

%% Render a list of device-path nodes as the canonical UEFI text form.
render_device_path([]) -> <<>>;
render_device_path(Nodes) ->
    Segments = lists:filtermap(fun render_dp_node/1, Nodes),
    iolist_to_binary(lists:join(<<"/">>, Segments)).

render_dp_node(#{<<"type">> := 16#7F}) -> false;
render_dp_node(#{<<"subtype-name">> := <<"pci">>, <<"function">> := F,
                 <<"device">> := D}) ->
    {true, iolist_to_binary(io_lib:format("Pci(0x~.16B,0x~.16B)", [D, F]))};
render_dp_node(#{<<"subtype-name">> := <<"acpi">>, <<"hid-string">> := H,
                 <<"uid">> := U}) ->
    {true, iolist_to_binary(io_lib:format("Acpi(~s,0x~.16B)", [H, U]))};
render_dp_node(#{<<"subtype-name">> := <<"sata">>,
                 <<"hba-port">> := P, <<"pmp-port">> := PMP,
                 <<"lun">> := L}) ->
    {true, iolist_to_binary(io_lib:format("Sata(0x~.16B,0x~.16B,0x~.16B)",
                                            [P, PMP, L]))};
render_dp_node(#{<<"subtype-name">> := <<"nvme-ns">>,
                 <<"namespace-id">> := N,
                 <<"ieee-eui-64">> := E}) ->
    {true, iolist_to_binary(io_lib:format("NVMe(0x~.16B,0x~.16B)", [N, E]))};
render_dp_node(#{<<"subtype-name">> := <<"usb">>,
                 <<"parent-port">> := P, <<"interface">> := I}) ->
    {true, iolist_to_binary(io_lib:format("USB(0x~.16B,0x~.16B)", [P, I]))};
render_dp_node(#{<<"subtype-name">> := <<"mac-addr">>,
                 <<"mac">> := M, <<"if-type">> := T}) ->
    {true, iolist_to_binary(io_lib:format("MAC(~s,0x~.16B)", [M, T]))};
render_dp_node(#{<<"subtype-name">> := <<"ipv4">>,
                 <<"local-ip">> := L, <<"remote-ip">> := R}) ->
    {true, iolist_to_binary(io_lib:format("IPv4(~s,~s)", [L, R]))};
render_dp_node(#{<<"subtype-name">> := <<"ipv6">>,
                 <<"local-ip">> := L, <<"remote-ip">> := R}) ->
    {true, iolist_to_binary(io_lib:format("IPv6(~s,~s)", [L, R]))};
render_dp_node(#{<<"subtype-name">> := <<"hard-drive">>,
                 <<"partition-number">> := N,
                 <<"partition-format">> := F,
                 <<"partition-signature">> := Sig}) ->
    {true, iolist_to_binary(io_lib:format("HD(~B,~s,~s)", [N, F, Sig]))};
render_dp_node(#{<<"subtype-name">> := <<"cdrom">>,
                 <<"boot-entry">> := B}) ->
    {true, iolist_to_binary(io_lib:format("CDROM(~B)", [B]))};
render_dp_node(#{<<"subtype-name">> := <<"file-path">>,
                 <<"path">> := P}) ->
    {true, P};
render_dp_node(#{<<"subtype-name">> := <<"piwg-fw-file">>,
                 <<"fv-file-name">> := G}) ->
    {true, iolist_to_binary(io_lib:format("FvFile(~s)", [G]))};
render_dp_node(#{<<"subtype-name">> := <<"piwg-fw-volume">>,
                 <<"fv-name">> := G}) ->
    {true, iolist_to_binary(io_lib:format("Fv(~s)", [G]))};
render_dp_node(#{<<"subtype-name">> := <<"hw-vendor">>,
                 <<"vendor-guid">> := G}) ->
    {true, iolist_to_binary(io_lib:format("VenHw(~s)", [G]))};
render_dp_node(#{<<"subtype-name">> := <<"media-vendor">>,
                 <<"vendor-guid">> := G}) ->
    {true, iolist_to_binary(io_lib:format("VenMedia(~s)", [G]))};
render_dp_node(#{<<"subtype-name">> := <<"msg-vendor">>,
                 <<"vendor-guid">> := G}) ->
    {true, iolist_to_binary(io_lib:format("VenMsg(~s)", [G]))};
render_dp_node(#{<<"type-name">> := T, <<"subtype-name">> := S}) ->
    {true, iolist_to_binary(io_lib:format("~s/~s", [T, S]))};
render_dp_node(_) -> false.

%% Extract the one thing a policy engine actually cares about per
%% UEFI variable: for `SecureBoot' the single enabled/disabled
%% byte; for `PK`/`KEK`/`db`/`dbx` the signature-list summary.
decode_uefi_variable_semantic(<<"SecureBoot">>, <<1>>) ->
    #{<<"secure-boot-enabled">> => true};
decode_uefi_variable_semantic(<<"SecureBoot">>, <<0>>) ->
    #{<<"secure-boot-enabled">> => false};
decode_uefi_variable_semantic(<<"SecureBoot">>, _) ->
    #{<<"secure-boot-enabled">> => <<"malformed">>};
decode_uefi_variable_semantic(<<"SetupMode">>, <<B:8>>) ->
    #{<<"setup-mode">> => B == 1};
decode_uefi_variable_semantic(<<"AuditMode">>, <<B:8>>) ->
    #{<<"audit-mode">> => B == 1};
decode_uefi_variable_semantic(<<"DeployedMode">>, <<B:8>>) ->
    #{<<"deployed-mode">> => B == 1};
%% MokListTrusted is a SINGLE-BYTE bool in the shim source of
%% truth -- must match BEFORE the signature-list catch-all below.
decode_uefi_variable_semantic(<<"MokListTrusted">>, <<1>>) ->
    #{<<"moklist-trusted">> => true};
decode_uefi_variable_semantic(<<"MokListTrusted">>, <<0>>) ->
    #{<<"moklist-trusted">> => false};
decode_uefi_variable_semantic(Name, Data)
  when Name =:= <<"PK">>; Name =:= <<"KEK">>;
       Name =:= <<"db">>; Name =:= <<"dbx">>;
       Name =:= <<"dbr">>; Name =:= <<"dbt">>;
       Name =:= <<"MokList">>; Name =:= <<"MokListX">>;
       Name =:= <<"MokListRT">>; Name =:= <<"MokListXRT">>;
       Name =:= <<"SbatLevelRT">> ->
    #{<<"signature-list">> => summarise_signature_list(Data)};
decode_uefi_variable_semantic(<<"SbatLevel">>, Data) ->
    %% Shim's SBAT revocation policy. Data is ASCII lines:
    %%   "sbat,<version>,<date-stamp>\n<component>,<revision>\n..."
    %% The first line is the SBAT self-revision + a YYYYMMDDHH
    %% date stamp (used as the revocation cutoff).
    Lines = binary:split(strip_trailing_nulls(Data),
                           <<"\n">>, [global, trim_all]),
    Entries =
        [case binary:split(L, <<",">>, [global]) of
             [Component, Rev | _] -> #{<<"component">> => Component,
                                       <<"revision">>  => Rev};
             [Single]             -> #{<<"component">> => Single,
                                       <<"revision">>  => <<"">>}
         end || L <- Lines],
    #{<<"sbat-entries">>       => Entries,
      <<"sbat-entry-count">>   => length(Entries)};
decode_uefi_variable_semantic(<<"Shim", _/binary>>, Data) ->
    %% Shim's own variables (Shim, ShimRT, ShimGuid, ...). The
    %% content is shim-specific; surface SHA-256 + length.
    #{<<"shim-variable-sha256">> =>
          hb_util:encode(crypto:hash(sha256, Data)),
      <<"shim-variable-length">> => byte_size(Data)};
%% BootCurrent / BootNext must match BEFORE the generic Boot####
%% catch-all below, since BootCurrent/BootNext start with "Boot".
decode_uefi_variable_semantic(<<"BootCurrent">>, <<Curr:16/little>>) ->
    #{<<"boot-current">> => iolist_to_binary(
        io_lib:format("Boot~4.16.0B", [Curr]))};
decode_uefi_variable_semantic(<<"BootNext">>, <<Next:16/little>>) ->
    #{<<"boot-next">> => iolist_to_binary(
        io_lib:format("Boot~4.16.0B", [Next]))};
decode_uefi_variable_semantic(<<"BootOrder">>, _) -> #{};
decode_uefi_variable_semantic(<<"Boot", _/binary>>, _) -> #{};
decode_uefi_variable_semantic(<<"Timeout">>, <<T:16/little>>) ->
    #{<<"boot-menu-timeout-seconds">> => T};
decode_uefi_variable_semantic(<<"OsIndications">>, <<V:64/little>>) ->
    #{<<"os-indications">>       => V,
      <<"os-indications-flags">> => os_indications_flags(V)};
decode_uefi_variable_semantic(<<"OsIndicationsSupported">>,
                                <<V:64/little>>) ->
    #{<<"os-indications-supported">>       => V,
      <<"os-indications-supported-flags">> => os_indications_flags(V)};
decode_uefi_variable_semantic(_, _) -> #{}.

%% UEFI section 8 Table 8-1: OsIndications bit flags.
os_indications_flags(V) ->
    [Name || {Bit, Name} <- [
        {16#01, <<"BOOT_TO_FW_UI">>},
        {16#02, <<"TIMESTAMP_REVOCATION">>},
        {16#04, <<"FILE_CAPSULE_DELIVERY_SUPPORTED">>},
        {16#08, <<"FMP_CAPSULE_SUPPORTED">>},
        {16#10, <<"CAPSULE_RESULT_VAR_SUPPORTED">>},
        {16#20, <<"START_OS_RECOVERY">>},
        {16#40, <<"START_PLATFORM_RECOVERY">>},
        {16#80, <<"JSON_CONFIG_DATA_REFRESH">>}
    ], (V band Bit) =/= 0].

%% EFI_SIGNATURE_LIST header (UEFI section 32.4.1):
%%   signatureType     EFI_GUID (16B)
%%   signatureListSize u32 LE
%%   signatureHeaderSize u32 LE
%%   signatureSize      u32 LE
%%   signatureHeader   [signatureHeaderSize]
%%   signatures         [...] -- each is {signatureOwner: EFI_GUID (16B),
%%                                         signatureData: [signatureSize-16]}
%%
%% Known signatureType GUIDs (UEFI section 32.4.1 + signed.efi spec):
%%   a5c059a1-94e4-4aa7-87b5-ab155c2bf072  EFI_CERT_X509_GUID -- the
%%                                          common case for db/dbx/KEK/PK
%%   c1c41626-504c-4092-aca9-41f936934328  EFI_CERT_SHA256_GUID
%%   3bd2a492-96c0-4079-b420-fcf98ef103ed  EFI_CERT_SHA384_GUID
%%   46dad11e-2b7a-4a3e-aaeb-f5fe0f0bc20e  EFI_CERT_SHA512_GUID
%%   826ca512-cf10-4ac9-b187-be01496631bd  EFI_CERT_SHA1_GUID
%%   3c5766e8-269c-4e34-aa14-ed776e85b3b6  EFI_CERT_RSA2048_GUID
%%   e8665b96-b6bb-4bdf-ba9b-3a3bbecb6f99  EFI_CERT_X509_SHA256_GUID
%%   a7717414-c616-4977-9420-844712a735bf  EFI_CERT_X509_SHA384_GUID
%%   64e0d72c-9e7a-4dc7-8ae5-a6c06c7b9fe0  EFI_CERT_X509_SHA512_GUID
%%   4aafd29d-68df-49ee-8aa9-347d375665a7  EFI_CERT_TYPE_PKCS7_GUID
%%
%% summarise_signature_list returns a list of lists, one per outer
%% EFI_SIGNATURE_LIST, each with per-entry decoded data. For X.509
%% entries we run a full `public_key:pkix_decode_cert/2' + extract
%% issuer DN, subject DN, SHA-256 fingerprint, NotBefore/NotAfter,
%% key algorithm + size. For hash entries we report the digest.
summarise_signature_list(Bin) -> summarise_signature_list(Bin, []).

summarise_signature_list(<<>>, Acc) -> lists:reverse(Acc);
summarise_signature_list(<<GuidBin:16/binary,
                           ListSize:32/unsigned-little,
                           HdrSize:32/unsigned-little,
                           SigSize:32/unsigned-little,
                           Rest/binary>>, Acc)
  when ListSize >= 28 + HdrSize ->
    SignaturesBytes = ListSize - 28 - HdrSize,
    case Rest of
        <<_Header:HdrSize/binary,
          SigsData:SignaturesBytes/binary,
          Tail/binary>> when SigSize > 0 ->
            N = SignaturesBytes div SigSize,
            TypeGuid = format_guid(GuidBin),
            TypeName = efi_sig_type_name(TypeGuid),
            Entries = decode_sig_entries(SigsData, SigSize, TypeName, N, []),
            Entry = #{
                <<"type-guid">>      => TypeGuid,
                <<"type-guid-name">> => TypeName,
                <<"entry-count">>    => N,
                <<"entry-size">>     => SigSize,
                <<"entries">>        => Entries
            },
            summarise_signature_list(Tail, [Entry | Acc]);
        _ ->
            lists:reverse([#{<<"error">> =>
                                 <<"malformed signature list">>} | Acc])
    end;
summarise_signature_list(_, Acc) ->
    lists:reverse([#{<<"error">> =>
                         <<"truncated signature list">>} | Acc]).

efi_sig_type_name(<<"a5c059a1-94e4-4aa7-87b5-ab155c2bf072">>) ->
    <<"EFI_CERT_X509_GUID">>;
efi_sig_type_name(<<"c1c41626-504c-4092-aca9-41f936934328">>) ->
    <<"EFI_CERT_SHA256_GUID">>;
efi_sig_type_name(<<"3bd2a492-96c0-4079-b420-fcf98ef103ed">>) ->
    <<"EFI_CERT_SHA384_GUID">>;
efi_sig_type_name(<<"46dad11e-2b7a-4a3e-aaeb-f5fe0f0bc20e">>) ->
    <<"EFI_CERT_SHA512_GUID">>;
efi_sig_type_name(<<"826ca512-cf10-4ac9-b187-be01496631bd">>) ->
    <<"EFI_CERT_SHA1_GUID">>;
efi_sig_type_name(<<"3c5766e8-269c-4e34-aa14-ed776e85b3b6">>) ->
    <<"EFI_CERT_RSA2048_GUID">>;
efi_sig_type_name(<<"e8665b96-b6bb-4bdf-ba9b-3a3bbecb6f99">>) ->
    <<"EFI_CERT_X509_SHA256_GUID">>;
efi_sig_type_name(<<"a7717414-c616-4977-9420-844712a735bf">>) ->
    <<"EFI_CERT_X509_SHA384_GUID">>;
efi_sig_type_name(<<"64e0d72c-9e7a-4dc7-8ae5-a6c06c7b9fe0">>) ->
    <<"EFI_CERT_X509_SHA512_GUID">>;
efi_sig_type_name(<<"4aafd29d-68df-49ee-8aa9-347d375665a7">>) ->
    <<"EFI_CERT_TYPE_PKCS7_GUID">>;
efi_sig_type_name(_) -> <<"unknown-cert-type">>.

decode_sig_entries(_, _, _, 0, Acc) -> lists:reverse(Acc);
decode_sig_entries(Bin, SigSize, TypeName, N, Acc)
  when byte_size(Bin) >= SigSize ->
    <<EntryBin:SigSize/binary, Rest/binary>> = Bin,
    <<OwnerGuid:16/binary, Payload/binary>> = EntryBin,
    Entry0 = #{<<"owner-guid">> => format_guid(OwnerGuid)},
    Entry = maps:merge(Entry0,
                        decode_sig_entry_payload(TypeName, Payload)),
    decode_sig_entries(Rest, SigSize, TypeName, N - 1, [Entry | Acc]);
decode_sig_entries(_, _, _, _, Acc) -> lists:reverse(Acc).

decode_sig_entry_payload(<<"EFI_CERT_X509_GUID">>, CertDer) ->
    decode_x509_cert(CertDer);
decode_sig_entry_payload(<<"EFI_CERT_SHA256_GUID">>, Digest)
  when byte_size(Digest) =:= 32 ->
    #{<<"sha256">> => hb_util:encode(Digest)};
decode_sig_entry_payload(<<"EFI_CERT_SHA384_GUID">>, Digest)
  when byte_size(Digest) =:= 48 ->
    #{<<"sha384">> => hb_util:encode(Digest)};
decode_sig_entry_payload(<<"EFI_CERT_SHA512_GUID">>, Digest)
  when byte_size(Digest) =:= 64 ->
    #{<<"sha512">> => hb_util:encode(Digest)};
decode_sig_entry_payload(<<"EFI_CERT_SHA1_GUID">>, Digest)
  when byte_size(Digest) =:= 20 ->
    #{<<"sha1">> => hb_util:encode(Digest)};
decode_sig_entry_payload(<<"EFI_CERT_RSA2048_GUID">>, Data) ->
    %% "Signature Data contains the concatenation of the RSA Public
    %% Exponent (fixed width 256 bytes, big-endian) and the RSA Public
    %% Modulus (256 bytes, big-endian)." (UEFI section 32.4.1)
    case Data of
        <<Exp:256/binary, Mod:256/binary>> ->
            #{<<"rsa-exponent-b64url">> => hb_util:encode(Exp),
              <<"rsa-modulus-b64url">> => hb_util:encode(Mod),
              <<"rsa-key-size-bits">> => 2048};
        _ -> #{<<"error">> => <<"malformed RSA2048 signature">>}
    end;
decode_sig_entry_payload(<<"EFI_CERT_X509_SHA256_GUID">>, Data) ->
    case Data of
        <<ToBe:32/binary, HashAlgGuid:16/binary, Sha:32/binary>> ->
            #{<<"to-be-signed-length">> => byte_size(ToBe),
              <<"hash-algorithm-guid">> => format_guid(HashAlgGuid),
              <<"sha256">> => hb_util:encode(Sha)};
        _ -> #{<<"error">> => <<"malformed X509_SHA256">>}
    end;
decode_sig_entry_payload(<<"EFI_CERT_TYPE_PKCS7_GUID">>, Data) ->
    #{<<"pkcs7-data-length">> => byte_size(Data)};
decode_sig_entry_payload(_, Data) ->
    %% Unknown cert type -- opaque.
    #{<<"data-length">> => byte_size(Data),
      <<"sha256">> => hb_util:encode(crypto:hash(sha256, Data))}.

%% Decode a DER-encoded X.509 certificate using OTP's public_key
%% module. Extract the fields a policy engine cares about:
%% issuer DN + subject DN (canonical string form), SHA-256
%% fingerprint, serial number, NotBefore/NotAfter, public-key
%% algorithm + key size. Graceful on malformed DER -- never
%% raises.
decode_x509_cert(Der) when is_binary(Der) ->
    try
        Cert = public_key:pkix_decode_cert(Der, otp),
        #'OTPCertificate'{
            tbsCertificate = Tbs,
            signatureAlgorithm = SigAlg
        } = Cert,
        #'OTPTBSCertificate'{
            serialNumber = Serial,
            issuer = Issuer,
            subject = Subject,
            validity = Validity,
            subjectPublicKeyInfo = Spki
        } = Tbs,
        #'Validity'{notBefore = NotBefore, notAfter = NotAfter} = Validity,
        #'OTPSubjectPublicKeyInfo'{
            algorithm = KeyAlg,
            subjectPublicKey = PubKey
        } = Spki,
        {KeyAlgName, KeySizeBits} = summarise_public_key(KeyAlg, PubKey),
        #{
            <<"x509-cert-der-length">>  => byte_size(Der),
            <<"x509-sha256-fingerprint">> =>
                hb_util:encode(crypto:hash(sha256, Der)),
            <<"x509-serial">>            =>
                iolist_to_binary(io_lib:format("~.16B", [Serial])),
            <<"x509-issuer">>            => dn_to_string(Issuer),
            <<"x509-subject">>           => dn_to_string(Subject),
            <<"x509-not-before">>        => x509_time(NotBefore),
            <<"x509-not-after">>         => x509_time(NotAfter),
            <<"x509-public-key-alg">>    => KeyAlgName,
            <<"x509-public-key-size-bits">> => KeySizeBits,
            <<"x509-signature-alg">>     =>
                sig_alg_name(SigAlg#'SignatureAlgorithm'.algorithm)
        }
    catch Class:Reason ->
        #{
            <<"x509-cert-der-length">> => byte_size(Der),
            <<"x509-decode-error">> =>
                iolist_to_binary(io_lib:format("~p:~p",
                                                [Class, Reason])),
            <<"x509-sha256-fingerprint">> =>
                hb_util:encode(crypto:hash(sha256, Der))
        }
    end.

%% Flatten an X.509 DN (rdnSequence) into a human-readable string:
%%   "CN=Microsoft UEFI CA 2011, O=Microsoft, C=US"
dn_to_string({rdnSequence, RDNs}) ->
    Parts = lists:map(fun rdn_to_string/1, RDNs),
    Joined = lists:filter(fun(X) -> X =/= <<>> end,
                            lists:flatten(Parts)),
    iolist_to_binary(lists:join(<<", ">>, Joined));
dn_to_string(_) -> <<"">>.

rdn_to_string(AttrsList) when is_list(AttrsList) ->
    [attr_to_string(A) || A <- AttrsList].

attr_to_string(#'AttributeTypeAndValue'{type = Type, value = Value}) ->
    Short = attr_short_name(Type),
    ValBin = attr_value_to_binary(Value),
    <<Short/binary, "=", ValBin/binary>>;
attr_to_string(_) -> <<>>.

%% Standard short names for common RDN components.
attr_short_name(?'id-at-commonName')             -> <<"CN">>;
attr_short_name(?'id-at-organizationName')       -> <<"O">>;
attr_short_name(?'id-at-organizationalUnitName') -> <<"OU">>;
attr_short_name(?'id-at-countryName')            -> <<"C">>;
attr_short_name(?'id-at-stateOrProvinceName')    -> <<"ST">>;
attr_short_name(?'id-at-localityName')           -> <<"L">>;
attr_short_name(?'id-at-serialNumber')           -> <<"SERIALNUMBER">>;
attr_short_name(?'id-emailAddress')              -> <<"emailAddress">>;
attr_short_name(OID) when is_tuple(OID) ->
    iolist_to_binary(
        lists:join(<<".">>, [integer_to_binary(I) || I <- tuple_to_list(OID)]));
attr_short_name(_) -> <<"?">>.

%% DER attribute value -> UTF-8 binary.
attr_value_to_binary({printableString, S})  -> to_binary(S);
attr_value_to_binary({utf8String, B})       when is_binary(B) -> B;
attr_value_to_binary({utf8String, L})       when is_list(L) -> to_binary(L);
attr_value_to_binary({bmpString, B})        -> ucs2_to_utf8_maybe(B);
attr_value_to_binary({teletexString, S})    -> to_binary(S);
attr_value_to_binary({ia5String, S})        -> to_binary(S);
attr_value_to_binary({universalString, S})  -> to_binary(S);
attr_value_to_binary(B) when is_binary(B)   -> B;
attr_value_to_binary(L) when is_list(L)     -> to_binary(L);
attr_value_to_binary(_) -> <<"?">>.

to_binary(S) when is_list(S) -> unicode:characters_to_binary(S, unicode);
to_binary(S) when is_binary(S) -> S.

ucs2_to_utf8_maybe(B) when is_binary(B) ->
    case unicode:characters_to_binary(B, {utf16, big}, utf8) of
        U when is_binary(U) -> U;
        _ -> B
    end.

x509_time({utcTime, S})         -> to_binary(S);
x509_time({generalTime, S})     -> to_binary(S);
x509_time(T)                    -> iolist_to_binary(io_lib:format("~p", [T])).

summarise_public_key(#'PublicKeyAlgorithm'{algorithm = Alg},
                      #'RSAPublicKey'{modulus = N}) when is_integer(N) ->
    Bits = byte_size(binary:encode_unsigned(N)) * 8,
    {pk_alg_name(Alg), Bits};
summarise_public_key(#'PublicKeyAlgorithm'{algorithm = Alg},
                      PubKey) when is_binary(PubKey) ->
    {pk_alg_name(Alg), byte_size(PubKey) * 8};
summarise_public_key(#'PublicKeyAlgorithm'{algorithm = Alg}, _) ->
    {pk_alg_name(Alg), 0};
summarise_public_key(_, _) ->
    {<<"unknown">>, 0}.

pk_alg_name(?rsaEncryption)      -> <<"rsa">>;
pk_alg_name(?'id-ecPublicKey')   -> <<"ecdsa">>;
pk_alg_name(?'id-dsa')           -> <<"dsa">>;
pk_alg_name(?'id-Ed25519')       -> <<"ed25519">>;
pk_alg_name(?'id-Ed448')         -> <<"ed448">>;
pk_alg_name(OID) when is_tuple(OID) ->
    iolist_to_binary(lists:join(<<".">>,
        [integer_to_binary(I) || I <- tuple_to_list(OID)]));
pk_alg_name(_) -> <<"unknown">>.

sig_alg_name(?sha256WithRSAEncryption)     -> <<"sha256WithRSA">>;
sig_alg_name(?sha384WithRSAEncryption)     -> <<"sha384WithRSA">>;
sig_alg_name(?sha512WithRSAEncryption)     -> <<"sha512WithRSA">>;
sig_alg_name(?sha1WithRSAEncryption)       -> <<"sha1WithRSA">>;
sig_alg_name(?md5WithRSAEncryption)        -> <<"md5WithRSA">>;
sig_alg_name(?'id-RSASSA-PSS')             -> <<"rsaPSS">>;
sig_alg_name(?'ecdsa-with-SHA256')         -> <<"ecdsaWithSHA256">>;
sig_alg_name(?'ecdsa-with-SHA384')         -> <<"ecdsaWithSHA384">>;
sig_alg_name(?'ecdsa-with-SHA512')         -> <<"ecdsaWithSHA512">>;
sig_alg_name(OID) when is_tuple(OID) ->
    iolist_to_binary(lists:join(<<".">>,
        [integer_to_binary(I) || I <- tuple_to_list(OID)]));
sig_alg_name(_) -> <<"unknown">>.

%% EV_S_CRTM_VERSION -- event data is the version string.
%% Heuristic: if it's an even length and looks like UTF-16LE
%% (every odd byte is 0x00 for ASCII range), decode as UTF-16LE.
%% Otherwise return as ASCII best-effort.
decode_crtm_version(#{<<"event-data">> := Data}) ->
    Decoded = case looks_like_utf16le(Data) of
        true  -> utf16le_to_utf8(Data);
        false -> ascii_trim(Data)
    end,
    #{<<"crtm-version">> => Decoded};
decode_crtm_version(_) -> #{}.

decode_post_code(#{<<"event-data">> := Data}) ->
    case ascii_only(Data) of
        true  -> #{<<"post-code">> => ascii_trim(Data)};
        false -> #{<<"post-code-bytes">> => Data}
    end;
decode_post_code(_) -> #{}.

%% UEFI_IMAGE_LOAD_EVENT:
%%   imageLocationInMemory  uint64 LE
%%   imageLengthInMemory    uint64 LE
%%   imageLinkTimeAddress   uint64 LE
%%   lengthOfDevicePath     uint64 LE
%%   devicePath             [lengthOfDevicePath] EFI_DEVICE_PATH_PROTOCOL
decode_uefi_image_load(#{<<"event-data">> := Data}) ->
    case Data of
        <<LocInMem:64/unsigned-little,
          LenInMem:64/unsigned-little,
          LinkAddr:64/unsigned-little,
          DpLen:64/unsigned-little,
          DevicePath:DpLen/binary,
          _Tail/binary>> ->
            {Nodes, Text} = parse_device_path(DevicePath),
            #{
                <<"image-location-in-memory">> => LocInMem,
                <<"image-length-in-memory">>   => LenInMem,
                <<"image-link-time-address">>  => LinkAddr,
                <<"device-path-length">>       => DpLen,
                <<"device-path">>              => DevicePath,
                <<"device-path-nodes">>        => Nodes,
                <<"device-path-text">>         => Text
            };
        _ ->
            #{<<"error">> => <<"malformed UEFI_IMAGE_LOAD_EVENT">>}
    end;
decode_uefi_image_load(_) -> #{}.

%% EV_IPL -- systemd-stub encodes "key=value\0" ASCII on PCR
%% 11/12/13 for UKI measurements (kernel_cmdline, kernel,
%% initrd, etc.). Other users encode opaque data.
decode_ev_ipl(#{<<"event-data">> := Data}) ->
    %% systemd-stub records are NUL-terminated UTF-8 strings
    %% with a single `=' separator.
    TrimmedData = case binary:last(Data) of
        0  -> binary:part(Data, 0, byte_size(Data) - 1);
        _  -> Data
    end,
    case ascii_only(TrimmedData) of
        true ->
            case binary:split(TrimmedData, <<"=">>) of
                [Key, Value] ->
                    KebabKey = binary:replace(Key, <<"_">>, <<"-">>,
                                               [global]),
                    Base = #{
                        <<"key">>   => KebabKey,
                        <<"value">> => Value,
                        <<"format">> => <<"key-value-ascii">>
                    },
                    %% When the key identifies the kernel cmdline
                    %% (systemd-stub `cmdline' / `kernel-cmdline' +
                    %% legacy aliases), tokenise the value and
                    %% extract the security-relevant flags per
                    %% paper section Architecture line 219-230.
                    case is_cmdline_key(KebabKey) of
                        true ->
                            Base#{
                                <<"cmdline-flags">> =>
                                    parse_kernel_cmdline(Value),
                                <<"cmdline-raw">> => Value
                            };
                        false -> Base
                    end;
                _ ->
                    #{<<"text">> => TrimmedData,
                      <<"format">> => <<"ascii">>}
            end;
        false ->
            #{<<"format">> => <<"opaque">>,
              <<"length">> => byte_size(Data)}
    end;
decode_ev_ipl(_) -> #{}.

%% Whether an EV_IPL `key' names the Linux kernel cmdline.
is_cmdline_key(<<"cmdline">>)         -> true;
is_cmdline_key(<<"kernel-cmdline">>)  -> true;
is_cmdline_key(<<"kernel.cmdline">>)  -> true;
is_cmdline_key(_)                     -> false.

%%%============================================================================
%%% Linux kernel command-line tokeniser (paper section Architecture l.223-229)
%%%============================================================================
%%%
%%% Given a Linux kernel cmdline binary, tokenise into
%%%   #{
%%%     <<"flag-name">> => Value,
%%%     ...
%%%     <<"-flags-seen">> => [<<"flag-name">>, ...],  %% stable-sorted
%%%     <<"-boolean">>    => [<<"flag-name">>, ...]   %% present-as-bool
%%%   }
%%%
%%% A cmdline token is either:
%%%   * `flag'          -- present-as-bool (added to -boolean list)
%%%   * `flag=value'    -- value is the binary after first `='
%%%   * `"quoted value"' -- literal between " is one token (systemd-boot
%%%                        quoting convention)
%%%
%%% Flag names are normalised: kernel `.'-separated flags
%%% (`kvm_intel.nested', `module.sig_enforce') are preserved with
%%% their dots; underscores in a flag name stay as underscores (they
%%% are part of kernel-space naming convention) -- we deliberately do
%%% NOT kebab-normalise flag names because `init_on_alloc' is a
%%% distinct symbol from `init-on-alloc' in the kernel namespace.
parse_kernel_cmdline(Bin) when is_binary(Bin) ->
    Tokens = cmdline_tokens(Bin, [], <<>>, false),
    {Map, Bools, Flags} = lists:foldl(
        fun(Tok, {M, Bs, Fs}) ->
            case binary:split(Tok, <<"=">>) of
                [Name, Val] when Name =/= <<>> ->
                    {M#{Name => cmdline_value(Val)}, Bs, [Name | Fs]};
                [Name] when Name =/= <<>> ->
                    {M#{Name => true}, [Name | Bs], [Name | Fs]};
                _ -> {M, Bs, Fs}
            end
        end, {#{}, [], []}, Tokens),
    Map#{
        <<"-flags-seen">>  => lists:usort(Flags),
        <<"-boolean">>     => lists:usort(Bools),
        <<"-token-count">> => length(Tokens)
    };
parse_kernel_cmdline(_) -> #{}.

%% Tokenise with "quoted values" handled -- `foo="a b c" bar' -> ["foo=a b c", "bar"].
cmdline_tokens(<<>>, Acc, Cur, _InQuote) ->
    case Cur of
        <<>> -> lists:reverse(Acc);
        _    -> lists:reverse([Cur | Acc])
    end;
cmdline_tokens(<<$", Rest/binary>>, Acc, Cur, InQuote) ->
    cmdline_tokens(Rest, Acc, Cur, not InQuote);
cmdline_tokens(<<C, Rest/binary>>, Acc, Cur, true) ->
    cmdline_tokens(Rest, Acc, <<Cur/binary, C>>, true);
cmdline_tokens(<<C, Rest/binary>>, Acc, Cur, false)
  when C =:= $ ; C =:= $\t; C =:= $\n; C =:= $\r ->
    case Cur of
        <<>> -> cmdline_tokens(Rest, Acc, <<>>, false);
        _    -> cmdline_tokens(Rest, [Cur | Acc], <<>>, false)
    end;
cmdline_tokens(<<C, Rest/binary>>, Acc, Cur, false) ->
    cmdline_tokens(Rest, Acc, <<Cur/binary, C>>, false).

%% Interpret a cmdline value: boolean "on"/"off"/"1"/"0" -> bool;
%% integer-looking decimals -> int; everything else -> binary.
cmdline_value(<<"on">>)    -> true;
cmdline_value(<<"ON">>)    -> true;
cmdline_value(<<"yes">>)   -> true;
cmdline_value(<<"Y">>)     -> true;
cmdline_value(<<"y">>)     -> true;
cmdline_value(<<"true">>)  -> true;
cmdline_value(<<"1">>)     -> true;
cmdline_value(<<"off">>)   -> false;
cmdline_value(<<"OFF">>)   -> false;
cmdline_value(<<"no">>)    -> false;
cmdline_value(<<"N">>)     -> false;
cmdline_value(<<"n">>)     -> false;
cmdline_value(<<"false">>) -> false;
cmdline_value(<<"0">>)     -> false;
cmdline_value(V) ->
    %% Multi-value comma list? `iommu=pt,strict' -> split.
    case binary:match(V, <<",">>) of
        nomatch -> V;
        _       -> binary:split(V, <<",">>, [global, trim_all])
    end.

%% UEFI_PLATFORM_FIRMWARE_BLOB:
%%   blobBase   uint64 LE
%%   blobLength uint64 LE
decode_firmware_blob(#{<<"event-data">> := Data}) ->
    case Data of
        <<Base:64/unsigned-little, Len:64/unsigned-little, _Tail/binary>> ->
            #{
                <<"blob-physical-address">> => Base,
                <<"blob-length">>           => Len
            };
        _ ->
            #{<<"error">> => <<"malformed UEFI_PLATFORM_FIRMWARE_BLOB">>}
    end;
decode_firmware_blob(_) -> #{}.

%% UEFI_PLATFORM_FIRMWARE_BLOB2:
%%   blobDescSize u8
%%   blobDesc     [blobDescSize] ASCII
%%   blobBase     uint64 LE
%%   blobLength   uint64 LE
decode_firmware_blob2(#{<<"event-data">> := Data}) ->
    case Data of
        <<DescSize:8, Rest0/binary>> ->
            case Rest0 of
                <<Desc:DescSize/binary, Base:64/unsigned-little,
                  Len:64/unsigned-little, _Tail/binary>> ->
                    #{
                        <<"blob-description">>      => Desc,
                        <<"blob-physical-address">> => Base,
                        <<"blob-length">>           => Len
                    };
                _ ->
                    #{<<"error">> => <<"malformed UEFI_PLATFORM_FIRMWARE_"
                                       "BLOB2">>}
            end;
        _ ->
            #{<<"error">> => <<"UEFI_PLATFORM_FIRMWARE_BLOB2 too short">>}
    end;
decode_firmware_blob2(_) -> #{}.

%% CPU microcode update header (Intel):
%%   headerVersion       uint32 LE
%%   updateRevision      uint32 LE
%%   date                uint32 LE   (yyyymmdd BCD)
%%   processorSignature  uint32 LE   (CPUID leaf 1 EAX)
%%   checksum            uint32 LE
%%   loaderRevision      uint32 LE
%%   processorFlags      uint32 LE
%%   dataSize            uint32 LE
%%   totalSize           uint32 LE
%%   ...reserved 12 bytes
%%   data...
%%
%% EV_CPU_MICROCODE -- data is the signed microcode update header.
%% Two shapes in the wild:
%%   Intel: 48-byte header starting with HeaderVersion=1 and an
%%          UpdateRevision / Date / ProcessorSignature (IA-32 IA
%%          manuals, Intel SDM Vol 3A section 9.11.1).
%%   AMD:   64-byte `microcode_header_amd' from the Linux kernel
%%          (arch/x86/kernel/cpu/microcode/amd.c). Starts with
%%          data_code (u32 BCD date), patch_id (u32), then
%%          mc_patch_data_id (u16), mc_patch_data_len (u8),
%%          init_flag (u8), patch_data_checksum (u32),
%%          nb_dev_id (u32), sb_dev_id (u32),
%%          processor_rev_id (u16), nb_rev_id (u8), sb_rev_id (u8),
%%          bios_api_rev (u8), 3 reserved bytes, 8×u32 match_reg.
%%
%% Both formats start with a 4-byte little-endian u32. Intel's
%% HeaderVersion is always 0x00000001; AMD's data_code is a BCD
%% date (e.g. 0x20250512 for 2025-05-12). We discriminate on that
%% byte pattern.
decode_cpu_microcode(#{<<"event-data">> := Data}) ->
    case classify_microcode(Data) of
        intel  -> decode_microcode_intel(Data);
        amd    -> decode_microcode_amd(Data);
        partial -> decode_microcode_partial(Data);
        too_short -> #{<<"error">> =>
                         <<"EV_CPU_MICROCODE too short for header">>}
    end;
decode_cpu_microcode(_) -> #{}.

%% Discriminator: Intel microcode headers always have HeaderVersion=1
%% in bytes 0-3 (u32 LE). AMD's first 4 bytes are a BCD date
%% 0x20YYMMDD, which is orders of magnitude larger than 1. So the
%% first u32 LE cleanly picks one side when the byte count permits.
classify_microcode(<<1:32/little, _:24/binary, _/binary>>) -> intel;
classify_microcode(<<V:32/little, _/binary>>)
  when V >= 16#20000101, V =< 16#20991231 ->
    case bcd_date_ok(V) of
        true  -> amd;
        false -> partial
    end;
classify_microcode(Data) when byte_size(Data) >= 28 -> partial;
classify_microcode(_) -> too_short.

bcd_date_ok(V) when is_integer(V) ->
    MM = (V bsr 8) band 16#FF,
    DD = V band 16#FF,
    bcd_ok(V bsr 24)
        andalso bcd_ok((V bsr 16) band 16#FF)
        andalso bcd_ok(MM) andalso bcd_ok(DD)
        andalso MM >= 16#01 andalso MM =< 16#12
        andalso DD >= 16#01 andalso DD =< 16#31.

bcd_ok(Byte) ->
    (Byte bsr 4) =< 9 andalso (Byte band 16#F) =< 9.

decode_microcode_intel(<<1:32/little, UR:32/little, Date:32/little,
                           ProcSig:32/little, Checksum:32/little,
                           LoaderRev:32/little, ProcFlags:32/little,
                           _Rest/binary>>) ->
    #{
        <<"format">>              => <<"intel">>,
        <<"header-version">>      => 1,
        <<"update-revision">>     => UR,
        <<"date-bcd">>            => Date,
        <<"date">>                => bcd_date(Date),
        <<"processor-signature">> => ProcSig,
        <<"cpu-family-model-stepping">> => format_intel_sig(ProcSig),
        <<"checksum">>            => Checksum,
        <<"loader-revision">>     => LoaderRev,
        <<"processor-flags">>     => ProcFlags
    }.

decode_microcode_amd(<<DataCode:32/little, PatchId:32/little,
                         McPatchDataId:16/little, McPatchDataLen:8,
                         InitFlag:8,
                         PatchDataChecksum:32/little, NbDevId:32/little,
                         SbDevId:32/little, ProcessorRevId:16/little,
                         NbRevId:8, SbRevId:8, BiosApiRev:8,
                         _Reserved:3/binary, _MatchRegBin:32/binary,
                         _Rest/binary>>) ->
    #{
        <<"format">>                    => <<"amd">>,
        <<"data-code">>                 => DataCode,
        <<"date">>                      => bcd_date(DataCode),
        <<"patch-id">>                  => PatchId,
        <<"mc-patch-data-id">>          => McPatchDataId,
        <<"mc-patch-data-length">>      => McPatchDataLen,
        <<"init-flag">>                 => InitFlag,
        <<"mc-patch-data-checksum">>    => PatchDataChecksum,
        <<"nb-dev-id">>                 => NbDevId,
        <<"sb-dev-id">>                 => SbDevId,
        <<"processor-rev-id">>          => ProcessorRevId,
        <<"processor-rev-id-hex">>      =>
            iolist_to_binary(io_lib:format("0x~4.16.0B",
                                             [ProcessorRevId])),
        <<"nb-rev-id">>                 => NbRevId,
        <<"sb-rev-id">>                 => SbRevId,
        <<"bios-api-rev">>              => BiosApiRev
    };
decode_microcode_amd(Data) ->
    %% Shorter than expected AMD header; fall back to best-effort.
    decode_microcode_partial(Data).

decode_microcode_partial(<<HV:32/little, UR:32/little, Date:32/little,
                             ProcSig:32/little, Checksum:32/little,
                             LoaderRev:32/little, ProcFlags:32/little,
                             _/binary>>) ->
    #{
        <<"format">>              => <<"unknown">>,
        <<"header-version">>      => HV,
        <<"update-revision">>     => UR,
        <<"date-bcd">>            => Date,
        <<"processor-signature">> => ProcSig,
        <<"checksum">>            => Checksum,
        <<"loader-revision">>     => LoaderRev,
        <<"processor-flags">>     => ProcFlags
    }.

%% (is_bcd_date + bcd_ok helpers live earlier in the file, near
%%  classify_microcode/1; they're not duplicated here.)

bcd_date(V) when is_integer(V) ->
    %% V is e.g. 0x20250512 -> "2025-05-12".
    YYYY = (V bsr 16) band 16#FFFF,
    MM = (V bsr 8) band 16#FF,
    DD = V band 16#FF,
    iolist_to_binary(io_lib:format(
        "~4.16.0B-~2.16.0B-~2.16.0B", [YYYY, MM, DD]));
bcd_date(_) -> <<"">>.

%% Format Intel's processor signature u32 (family/model/stepping).
%% Layout per Intel SDM section 9.11.1:
%%   bits 0-3 Stepping
%%   bits 4-7 Model
%%   bits 8-11 Family
%%   bits 12-13 Type
%%   bits 16-19 ExtModel
%%   bits 20-27 ExtFamily
format_intel_sig(Sig) when is_integer(Sig) ->
    Stepping   = Sig band 16#F,
    Model      = (Sig bsr 4) band 16#F,
    Family     = (Sig bsr 8) band 16#F,
    ExtModel   = (Sig bsr 16) band 16#F,
    ExtFamily  = (Sig bsr 20) band 16#FF,
    FullFamily = case Family of
        16#F -> Family + ExtFamily;
        _    -> Family
    end,
    FullModel = case Family of
        F when F =:= 16#6 orelse F =:= 16#F ->
            (ExtModel bsl 4) bor Model;
        _ -> Model
    end,
    iolist_to_binary(io_lib:format(
        "family=~.10B model=~.10B stepping=~.10B",
        [FullFamily, FullModel, Stepping])).

decode_separator(#{<<"event-data">> := <<16#FF, 16#FF, 16#FF, 16#FF>>}) ->
    #{<<"separator">> => <<"firmware_error">>};
decode_separator(#{<<"event-data">> := <<0, 0, 0, 0>>}) ->
    #{<<"separator">> => <<"normal">>};
decode_separator(#{<<"event-data">> := Data}) ->
    #{<<"separator">> => <<"other">>,
      <<"bytes">> => Data};
decode_separator(_) -> #{}.

decode_ascii_action(#{<<"event-data">> := Data}) ->
    case ascii_only(Data) of
        true -> #{<<"action">> => ascii_trim(Data)};
        false -> #{<<"action-bytes">> => Data}
    end;
decode_ascii_action(_) -> #{}.

%% EV_NO_ACTION -- first record carries TCG_EfiSpecIdEvent; others
%% may carry StartupLocality ("StartupLocality" + 1 byte) or
%% other markers.
decode_no_action(#{<<"event-data">> := <<"Spec ID Event03", 0, _/binary>>
                   = Data}) ->
    case parse_spec_id(Data) of
        {ok, AlgList} ->
            #{<<"spec-id">> => <<"Event03">>,
              <<"algorithms">> =>
                [#{<<"hash-alg-id">> => AlgId,
                   <<"hash-alg-name">> => hash_alg_name(AlgId),
                   <<"digest-size">> => Sz}
                 || {AlgId, Sz} <- AlgList]};
        _ -> #{<<"error">> => <<"malformed SpecID">>}
    end;
decode_no_action(#{<<"event-data">> := <<"StartupLocality", 0, Locality:8,
                                           _/binary>>}) ->
    #{<<"marker">> => <<"StartupLocality">>,
      <<"locality">> => Locality};
decode_no_action(#{<<"event-data">> := Data}) ->
    #{<<"marker">> => <<"other">>,
      <<"length">> => byte_size(Data)};
decode_no_action(_) -> #{}.

%%%---- Small text helpers ----------------------------------------------

utf16le_to_utf8(Bin) ->
    case unicode:characters_to_binary(Bin, {utf16, little}, utf8) of
        B when is_binary(B) -> ascii_trim(B);
        _ -> ascii_trim(Bin)
    end.

looks_like_utf16le(Bin) when is_binary(Bin), byte_size(Bin) >= 2 ->
    byte_size(Bin) rem 2 =:= 0 andalso
        lists:all(fun(<<_:8, 0:8>>) -> true;
                    (_) -> false
                 end,
                 [binary:part(Bin, I, 2)
                  || I <- lists:seq(0, byte_size(Bin) - 2, 2)]);
looks_like_utf16le(_) -> false.

ascii_only(Bin) when is_binary(Bin) ->
    lists:all(
        fun(B) -> (B =:= 9) orelse (B =:= 10) orelse (B =:= 13)
                   orelse (B >= 16#20 andalso B =< 16#7E)
                   orelse (B =:= 0) end,
        binary_to_list(Bin));
ascii_only(_) -> false.

ascii_trim(Bin) when is_binary(Bin) ->
    %% Strip trailing NUL bytes (common in UEFI strings + a
    %% byproduct of UTF-16LE -> UTF-8 conversion when the source
    %% had a trailing null terminator).
    strip_trailing_nulls(Bin);
ascii_trim(Other) -> Other.

strip_trailing_nulls(<<>>) -> <<>>;
strip_trailing_nulls(Bin) ->
    case binary:last(Bin) of
        0 -> strip_trailing_nulls(binary:part(Bin, 0, byte_size(Bin) - 1));
        _ -> Bin
    end.

%% (legacy fmt_efi_guid + fmt_guid_tail removed; format_guid/1
%%  earlier in the file is the single canonical lowercase-hex
%%  implementation per UEFI section 22 GUID canonical form.)

%%%============================================================================
%%% Legacy first record (TCG_PCR_EVENT + TCG_EfiSpecIdEvent)
%%%============================================================================

%% First record in a crypto-agile log is legacy TCG_PCR_EVENT on
%% PCR 0 with an EV_NO_ACTION event whose data is a
%% TCG_EfiSpecIdEventStruct declaring which digest algorithms are
%% in use in subsequent records.
parse_first_record(
    <<Pcr:32/unsigned-little,
      EventType:32/unsigned-little,
      Sha1:20/binary,
      EventSize:32/unsigned-little,
      Event:EventSize/binary,
      Rest/binary>>) ->
    case parse_spec_id(Event) of
        {ok, AlgList} ->
            FirstEv = #{
                <<"seq">>             => 1,
                <<"pcr">>             => Pcr,
                <<"event-type-code">> => EventType,
                <<"digests">>         => #{<<"sha1">> => Sha1},
                <<"event-data">>      => Event
            },
            {ok, FirstEv, AlgList, Rest};
        _ ->
            {error, {no_spec_id_header,
                     byte_size(Event),
                     case Event of
                         <<Head:16/binary, _/binary>> -> Head;
                         _ -> Event
                     end}}
    end;
parse_first_record(Bin) ->
    {error, {first_record_truncated, byte_size(Bin)}}.

%% TCG_EfiSpecIdEventStruct: the event data inside the first record.
parse_spec_id(<<"Spec ID Event03", 0,
                _PlatformClass:32/unsigned-little,
                _SpecMinor:8, _SpecMajor:8, _SpecErrata:8,
                _UintnSize:8,
                NumAlgs:32/unsigned-little,
                AlgRest/binary>>) ->
    case parse_alg_list(AlgRest, NumAlgs, []) of
        {ok, AlgList, _Tail} -> {ok, AlgList};
        _ -> error
    end;
parse_spec_id(_) -> error.

parse_alg_list(Rest, 0, Acc) ->
    {ok, lists:reverse(Acc), Rest};
parse_alg_list(<<AlgId:16/unsigned-little,
                 DigestSize:16/unsigned-little,
                 Rest/binary>>, N, Acc) ->
    parse_alg_list(Rest, N - 1, [{AlgId, DigestSize} | Acc]);
parse_alg_list(_, _, _) -> error.

%%%============================================================================
%%% Crypto-agile records (TCG_PCR_EVENT2)
%%%============================================================================

parse_crypto_agile(<<>>, _AlgList, _Seq, Acc) ->
    {lists:reverse(Acc), <<>>};
parse_crypto_agile(<<Pcr:32/unsigned-little,
                     EventType:32/unsigned-little,
                     NumDigests:32/unsigned-little,
                     Rest0/binary>>, AlgList, Seq, Acc) ->
    case parse_digests(Rest0, NumDigests, AlgList, #{}) of
        {ok, Digests, Rest1} ->
            case Rest1 of
                <<EventSize:32/unsigned-little,
                  Event:EventSize/binary,
                  Rest2/binary>> ->
                    Ev = #{
                        <<"seq">>             => Seq,
                        <<"pcr">>             => Pcr,
                        <<"event-type-code">> => EventType,
                        <<"digests">>         => Digests,
                        <<"event-data">>      => Event
                    },
                    parse_crypto_agile(Rest2, AlgList, Seq + 1, [Ev | Acc]);
                _ ->
                    %% Truncated -- return what we have.
                    TruncErr = #{
                        <<"error">> => <<"truncated event (bad eventSize)">>,
                        <<"at-seq">> => Seq
                    },
                    {lists:reverse([TruncErr | Acc]), <<>>}
            end;
        error ->
            TruncErr = #{
                <<"error">> => <<"truncated digests">>,
                <<"at-seq">> => Seq
            },
            {lists:reverse([TruncErr | Acc]), <<>>}
    end;
parse_crypto_agile(_Bin, _AlgList, _Seq, Acc) ->
    %% Trailing bytes that don't match a record header. Could be
    %% noise at end of log. Stop cleanly.
    {lists:reverse(Acc), <<>>}.

%% Parse N digests. Digest sizes MUST match the SpecID's declared
%% algorithms (in order). Some logs use different algorithms per
%% record, so we look up the size by algId if not in the SpecID
%% list (but only the SpecID-declared algs are truly crypto-agile).
parse_digests(Rest, 0, _AlgList, Acc) ->
    {ok, Acc, Rest};
parse_digests(<<AlgId:16/unsigned-little, Rest0/binary>>,
              N, AlgList, Acc) ->
    Size = digest_size_for(AlgId, AlgList),
    case Rest0 of
        <<Digest:Size/binary, Rest1/binary>> ->
            Name = hash_alg_name(AlgId),
            parse_digests(Rest1, N - 1, AlgList, Acc#{Name => Digest});
        _ -> error
    end;
parse_digests(_, _, _, _) -> error.

digest_size_for(AlgId, AlgList) ->
    case lists:keyfind(AlgId, 1, AlgList) of
        {AlgId, Size} -> Size;
        _ -> hash_alg_size(AlgId)
    end.

%% TCG algorithm registry (partial -- the common ones).
hash_alg_size(16#04) -> 20;   %% TPM_ALG_SHA1
hash_alg_size(16#0B) -> 32;   %% TPM_ALG_SHA256
hash_alg_size(16#0C) -> 48;   %% TPM_ALG_SHA384
hash_alg_size(16#0D) -> 64;   %% TPM_ALG_SHA512
hash_alg_size(16#12) -> 32;   %% TPM_ALG_SM3_256
hash_alg_size(16#15) -> 32;   %% TPM_ALG_SHA3_256
hash_alg_size(16#16) -> 48;   %% TPM_ALG_SHA3_384
hash_alg_size(16#17) -> 64;   %% TPM_ALG_SHA3_512
hash_alg_size(_)     -> 0.    %% unknown -> parser will fail record

hash_alg_name(16#04) -> <<"sha1">>;
hash_alg_name(16#0B) -> <<"sha256">>;
hash_alg_name(16#0C) -> <<"sha384">>;
hash_alg_name(16#0D) -> <<"sha512">>;
hash_alg_name(16#12) -> <<"sm3-256">>;
hash_alg_name(16#15) -> <<"sha3-256">>;
hash_alg_name(16#16) -> <<"sha3-384">>;
hash_alg_name(16#17) -> <<"sha3-512">>;
hash_alg_name(Alg)   -> iolist_to_binary(
                            io_lib:format("alg_0x~.16B", [Alg])).

%%%============================================================================
%%% All-legacy fallback (old firmware that never emitted a SpecID)
%%%============================================================================

parse_all_legacy(<<>>, _Seq, Acc) -> {ok, lists:reverse(Acc)};
parse_all_legacy(<<Pcr:32/unsigned-little,
                   EventType:32/unsigned-little,
                   Sha1:20/binary,
                   EventSize:32/unsigned-little,
                   Event:EventSize/binary,
                   Rest/binary>>, Seq, Acc) ->
    Ev = #{
        <<"seq">>             => Seq,
        <<"pcr">>             => Pcr,
        <<"event-type-code">> => EventType,
        <<"digests">>         => #{<<"sha1">> => Sha1},
        <<"event-data">>      => Event
    },
    parse_all_legacy(Rest, Seq + 1, [Ev | Acc]);
parse_all_legacy(_Bin, _Seq, _Acc) ->
    %% Failed partway -- signal caller to report parse error.
    error.

%%%============================================================================
%%% Naming + indexing
%%%============================================================================

attach_type_name(Ev = #{<<"event-type-code">> := Code}, Registry) ->
    Ev#{<<"event-type">> => lookup_name(Code, Registry)};
attach_type_name(Ev, _) -> Ev.

lookup_name(Code, Registry) ->
    case maps:get(integer_to_binary(Code), Registry, undefined) of
        #{<<"name">> := Name} when is_binary(Name) -> Name;
        _ ->
            %% Built-in fallback for the common core codes --
            %% handles dev environments where priv/ isn't loadable.
            static_event_type_name(Code)
    end.

static_event_type_name(16#0) -> <<"EV_PREBOOT_CERT">>;
static_event_type_name(16#1) -> <<"EV_POST_CODE">>;
static_event_type_name(16#3) -> <<"EV_NO_ACTION">>;
static_event_type_name(16#4) -> <<"EV_SEPARATOR">>;
static_event_type_name(16#5) -> <<"EV_ACTION">>;
static_event_type_name(16#6) -> <<"EV_EVENT_TAG">>;
static_event_type_name(16#7) -> <<"EV_S_CRTM_CONTENTS">>;
static_event_type_name(16#8) -> <<"EV_S_CRTM_VERSION">>;
static_event_type_name(16#9) -> <<"EV_CPU_MICROCODE">>;
static_event_type_name(16#A) -> <<"EV_PLATFORM_CONFIG_FLAGS">>;
static_event_type_name(16#B) -> <<"EV_TABLE_OF_DEVICES">>;
static_event_type_name(16#C) -> <<"EV_COMPACT_HASH">>;
static_event_type_name(16#D) -> <<"EV_IPL">>;
static_event_type_name(16#E) -> <<"EV_IPL_PARTITION_DATA">>;
static_event_type_name(16#F) -> <<"EV_NONHOST_CODE">>;
static_event_type_name(16#10) -> <<"EV_NONHOST_CONFIG">>;
static_event_type_name(16#11) -> <<"EV_NONHOST_INFO">>;
static_event_type_name(16#12) -> <<"EV_OMIT_BOOT_DEVICE_EVENTS">>;
static_event_type_name(16#80000001) -> <<"EV_EFI_VARIABLE_DRIVER_CONFIG">>;
static_event_type_name(16#80000002) -> <<"EV_EFI_VARIABLE_BOOT">>;
static_event_type_name(16#80000003) -> <<"EV_EFI_BOOT_SERVICES_APPLICATION">>;
static_event_type_name(16#80000004) -> <<"EV_EFI_BOOT_SERVICES_DRIVER">>;
static_event_type_name(16#80000005) -> <<"EV_EFI_RUNTIME_SERVICES_DRIVER">>;
static_event_type_name(16#80000006) -> <<"EV_EFI_GPT_EVENT">>;
static_event_type_name(16#80000007) -> <<"EV_EFI_ACTION">>;
static_event_type_name(16#80000008) -> <<"EV_EFI_PLATFORM_FIRMWARE_BLOB">>;
static_event_type_name(16#80000009) -> <<"EV_EFI_HANDOFF_TABLES">>;
static_event_type_name(16#8000000A) -> <<"EV_EFI_PLATFORM_FIRMWARE_BLOB2">>;
static_event_type_name(16#8000000B) -> <<"EV_EFI_HANDOFF_TABLES2">>;
static_event_type_name(16#80000010) -> <<"EV_EFI_HCRTM_EVENT">>;
static_event_type_name(16#800000E0) -> <<"EV_EFI_VARIABLE_AUTHORITY">>;
static_event_type_name(16#800000E1) -> <<"EV_EFI_SPDM_FIRMWARE_BLOB">>;
static_event_type_name(16#800000E2) -> <<"EV_EFI_SPDM_FIRMWARE_CONFIG">>;
static_event_type_name(16#800000E3) -> <<"EV_EFI_SPDM_DEVICE_POLICY">>;
static_event_type_name(16#800000E4) -> <<"EV_EFI_SPDM_DEVICE_AUTHORITY">>;
static_event_type_name(Code) ->
    iolist_to_binary(io_lib:format("EV_UNKNOWN_0x~.16B", [Code])).

%% Convert a list of events into a 1-indexed binary-keyed map
%% (AO-Core natural collection form -- individual events
%% addressable by path traversal).
index_map(Events) ->
    maps:from_list(
        [{integer_to_binary(maps:get(<<"seq">>, Ev, I)), Ev}
         || {I, Ev} <- lists:zip(lists:seq(1, length(Events)), Events)]).

fmt_parse_error({no_spec_id_header, Sz, Head}) ->
    iolist_to_binary(io_lib:format(
        "first record has no TCG_EfiSpecIdEvent signature "
        "(eventSize=~B, head=~p)", [Sz, Head]));
fmt_parse_error({first_record_truncated, Sz}) ->
    iolist_to_binary(io_lib:format(
        "first TCG_PCR_EVENT truncated at ~B bytes", [Sz]));
fmt_parse_error(Other) ->
    iolist_to_binary(io_lib:format("~p", [Other])).

%%%============================================================================
%%% Event-types registry lookup
%%%============================================================================

%% Prefer a caller-supplied registry in Opts for testability; fall
%% back to the one loaded by hb_db_tpm at startup.
event_types_registry(#{event_types := R}) when is_map(R) -> R;
event_types_registry(_Opts) ->
    try hb_db_tpm:load(#{}) of
        #{<<"event-types">> := R} when is_map(R) -> R;
        _ -> #{}
    catch _:_ -> #{}
    end.

%%%============================================================================
%%% Tests
%%%============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Synthetic crypto-agile event log with:
%%   - first record: SpecID declaring sha1+sha256
%%   - one EV_S_CRTM_VERSION on PCR 0 with "TEST FW v1" ASCII event
%%   - one EV_EFI_VARIABLE_DRIVER_CONFIG on PCR 7 with a SecureBoot=1
%%     variable inside (minimal UEFI_VARIABLE_DATA shape)
build_fixture() ->
    %% --- First record: TCG_PCR_EVENT (legacy header) ---
    %%
    %% SpecID event data.
    AlgPairs = <<16#04:16/little, 20:16/little,       %% SHA-1, 20B
                 16#0B:16/little, 32:16/little>>,     %% SHA-256, 32B
    SpecId = <<"Spec ID Event03", 0,
               0:32/little,                           %% platform class
               0:8, 2:8, 0:8, 8:8,                    %% v2.0, 8-byte uintn
               2:32/little,                           %% 2 algs
               AlgPairs/binary,
               0:8>>,                                 %% no vendorInfo
    SpecIdSize = byte_size(SpecId),
    FirstRec = <<0:32/little,                         %% PCR 0
                 3:32/little,                         %% EV_NO_ACTION
                 0:(20*8),                            %% SHA-1 zero
                 SpecIdSize:32/little,
                 SpecId/binary>>,
    %% --- Record 2: EV_S_CRTM_VERSION on PCR 0 ---
    Data2 = <<"TEST FW v1">>,
    Data2Size = byte_size(Data2),
    Sha1_2 = crypto:hash(sha, Data2),
    Sha256_2 = crypto:hash(sha256, Data2),
    Rec2 = <<0:32/little,                             %% PCR 0
             16#8:32/little,                          %% EV_S_CRTM_VERSION
             2:32/little,                             %% 2 digests
             16#04:16/little, Sha1_2/binary,
             16#0B:16/little, Sha256_2/binary,
             Data2Size:32/little,
             Data2/binary>>,
    %% --- Record 3: EV_EFI_VARIABLE_DRIVER_CONFIG (SecureBoot) on PCR 7 ---
    %% Minimal UEFI_VARIABLE_DATA:
    %%   variableName GUID (16B, using zeros -- content doesn't matter here)
    %%   unicodeNameLength u64 = 10 (SecureBoot = 10 UTF-16 chars)
    %%   variableDataLength u64 = 1 (single byte 0x01)
    %%   unicodeName UTF-16LE of "SecureBoot"
    %%   variableData = <<1>>
    Uname = unicode:characters_to_binary(<<"SecureBoot">>, utf8, {utf16, little}),
    UvData = <<0:(16*8),                              %% guid
               10:64/little,                          %% unicodeNameLength
               1:64/little,                           %% variableDataLength
               Uname/binary,
               1>>,                                   %% SecureBoot = 1
    UvSize = byte_size(UvData),
    Sha1_3 = crypto:hash(sha, UvData),
    Sha256_3 = crypto:hash(sha256, UvData),
    Rec3 = <<7:32/little,                             %% PCR 7
             16#80000001:32/little,                   %% EV_EFI_VAR_DRV_CFG
             2:32/little,                             %% 2 digests
             16#04:16/little, Sha1_3/binary,
             16#0B:16/little, Sha256_3/binary,
             UvSize:32/little,
             UvData/binary>>,
    <<FirstRec/binary, Rec2/binary, Rec3/binary>>.

parses_crypto_agile_three_records_test() ->
    Events = parse(build_fixture()),
    %% Keyed by binary sequence numbers "1", "2", "3".
    ?assertEqual(3, maps:size(Events)),
    ?assert(maps:is_key(<<"1">>, Events)),
    ?assert(maps:is_key(<<"2">>, Events)),
    ?assert(maps:is_key(<<"3">>, Events)),
    ok.

first_record_is_spec_id_no_action_test() ->
    Events = parse(build_fixture()),
    E1 = maps:get(<<"1">>, Events),
    ?assertEqual(0, maps:get(<<"pcr">>, E1)),
    ?assertEqual(3, maps:get(<<"event-type-code">>, E1)),
    ?assertEqual(<<"EV_NO_ACTION">>, maps:get(<<"event-type">>, E1)),
    %% Only SHA-1 on the first record (legacy shape).
    D = maps:get(<<"digests">>, E1),
    ?assert(maps:is_key(<<"sha1">>, D)).

second_record_has_both_digest_algs_test() ->
    Events = parse(build_fixture()),
    E2 = maps:get(<<"2">>, Events),
    ?assertEqual(<<"EV_S_CRTM_VERSION">>, maps:get(<<"event-type">>, E2)),
    D = maps:get(<<"digests">>, E2),
    ?assert(maps:is_key(<<"sha1">>, D)),
    ?assert(maps:is_key(<<"sha256">>, D)),
    ?assertEqual(20, byte_size(maps:get(<<"sha1">>, D))),
    ?assertEqual(32, byte_size(maps:get(<<"sha256">>, D))),
    %% Event data is the raw ASCII string.
    ?assertEqual(<<"TEST FW v1">>, maps:get(<<"event-data">>, E2)).

secure_boot_variable_record_parses_test() ->
    Events = parse(build_fixture()),
    E3 = maps:get(<<"3">>, Events),
    ?assertEqual(7, maps:get(<<"pcr">>, E3)),
    ?assertEqual(16#80000001,
                 maps:get(<<"event-type-code">>, E3)),
    ?assertEqual(<<"EV_EFI_VARIABLE_DRIVER_CONFIG">>,
                 maps:get(<<"event-type">>, E3)),
    %% Event data begins with the 16-byte GUID, length fields, then
    %% the UTF-16LE "SecureBoot" string, then a single 0x01 byte.
    Data = maps:get(<<"event-data">>, E3),
    ?assert(byte_size(Data) > 40).

event_type_name_standalone_test() ->
    %% With no Opts, falls back to the static table.
    ?assertEqual(<<"EV_S_CRTM_VERSION">>, event_type_name(16#8)),
    ?assertEqual(<<"EV_EFI_VARIABLE_DRIVER_CONFIG">>,
                 event_type_name(16#80000001)),
    ?assert(binary:match(event_type_name(16#DEADBEEF),
                         <<"EV_UNKNOWN_">>) =/= nomatch).

parse_handles_truncated_second_record_test() ->
    Full = build_fixture(),
    %% Cut off mid-way through record 2's digests -- parser should
    %% return the first record plus an error entry, not crash.
    Truncated = binary:part(Full, 0, byte_size(Full) - 40),
    Events = parse(Truncated),
    ?assert(maps:size(Events) >= 1),
    ok.

parse_empty_input_test() ->
    %% Empty binary -> empty map (no spec-id, no legacy -- just nothing).
    R = parse(<<>>),
    ?assert(is_map(R)).

parse_non_binary_input_test() ->
    R = parse(not_a_binary),
    ?assertMatch(#{<<"error">> := _}, R).

%%%---- Decoder tests ---------------------------------------------------

%% The Secure Boot UEFI variable encodes `enabled' as a single byte
%% (0x01/0x00). Surface as `semantic.secure_boot_enabled: bool'.
decode_secure_boot_variable_enabled_test() ->
    Data = build_uefi_variable(<<0:128>>, <<"SecureBoot">>, <<1>>),
    Ev = #{<<"event-type-code">> => 16#80000001,
           <<"event-data">> => Data},
    Parsed = (decode_event(Ev))#{<<"parsed">> => _P = maps:get(<<"parsed">>,
                                          decode_event(Ev), #{})},
    P = maps:get(<<"parsed">>, Parsed),
    ?assertEqual(<<"SecureBoot">>, maps:get(<<"variable-name">>, P)),
    ?assertEqual(#{<<"secure-boot-enabled">> => true},
                 maps:get(<<"semantic">>, P)).

decode_secure_boot_variable_disabled_test() ->
    Data = build_uefi_variable(<<0:128>>, <<"SecureBoot">>, <<0>>),
    Ev = #{<<"event-type-code">> => 16#80000001,
           <<"event-data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(#{<<"secure-boot-enabled">> => false},
                 maps:get(<<"semantic">>, P)).

%% SetupMode / AuditMode / DeployedMode single-byte bool semantic.
decode_setup_mode_test() ->
    Data = build_uefi_variable(<<0:128>>, <<"SetupMode">>, <<1>>),
    Ev = #{<<"event-type-code">> => 16#80000001,
           <<"event-data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(true, maps:get(<<"setup-mode">>,
                                 maps:get(<<"semantic">>, P))).

decode_audit_mode_test() ->
    Data = build_uefi_variable(<<0:128>>, <<"AuditMode">>, <<0>>),
    Ev = #{<<"event-type-code">> => 16#80000001,
           <<"event-data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(false, maps:get(<<"audit-mode">>,
                                  maps:get(<<"semantic">>, P))).

%% MokListTrusted single-byte form (not signature-list form).
decode_moklisttrusted_test() ->
    Data = build_uefi_variable(<<0:128>>, <<"MokListTrusted">>, <<1>>),
    Ev = #{<<"event-type-code">> => 16#800000E0,
           <<"event-data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(true, maps:get(<<"moklist-trusted">>,
                                 maps:get(<<"semantic">>, P))).

%% SbatLevel -- parse ASCII SBAT revocation policy.
decode_sbatlevel_test() ->
    %% Minimal SBAT policy: self-revision header + 2 components.
    Sbat = <<"sbat,1,2024030100\n"
             "shim,4\n"
             "grub,3\n">>,
    Data = build_uefi_variable(<<0:128>>, <<"SbatLevel">>, Sbat),
    Ev = #{<<"event-type-code">> => 16#800000E0,
           <<"event-data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    Sem = maps:get(<<"semantic">>, P),
    Entries = maps:get(<<"sbat-entries">>, Sem),
    ?assertEqual(3, length(Entries)),
    ?assertEqual(3, maps:get(<<"sbat-entry-count">>, Sem)),
    [E1 | _] = Entries,
    ?assertEqual(<<"sbat">>, maps:get(<<"component">>, E1)).

%% OsIndications u64 bit-flag decomposition.
decode_os_indications_test() ->
    %% Bits: 0x01 BOOT_TO_FW_UI + 0x04 FILE_CAPSULE + 0x20 START_OS_RECOVERY
    Flags = 16#25,
    Data = build_uefi_variable(<<0:128>>, <<"OsIndications">>,
                                 <<Flags:64/little>>),
    Ev = #{<<"event-type-code">> => 16#80000001,
           <<"event-data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    Sem = maps:get(<<"semantic">>, P),
    ?assertEqual(Flags, maps:get(<<"os-indications">>, Sem)),
    FlagNames = maps:get(<<"os-indications-flags">>, Sem),
    ?assert(lists:member(<<"BOOT_TO_FW_UI">>, FlagNames)),
    ?assert(lists:member(<<"FILE_CAPSULE_DELIVERY_SUPPORTED">>,
                         FlagNames)),
    ?assert(lists:member(<<"START_OS_RECOVERY">>, FlagNames)).

%% BootOrder / BootCurrent semantic handling.
decode_boot_current_test() ->
    Data = build_uefi_variable(<<0:128>>, <<"BootCurrent">>,
                                 <<16#0002:16/little>>),
    Ev = #{<<"event-type-code">> => 16#80000002,
           <<"event-data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    Sem = maps:get(<<"semantic">>, P),
    ?assertEqual(<<"Boot0002">>, maps:get(<<"boot-current">>, Sem)).

decode_crtm_version_utf16le_test() ->
    Utf16 = unicode:characters_to_binary(<<"BIOS 1.23">>, utf8,
                                           {utf16, little}),
    Ev = #{<<"event-type-code">> => 16#8, <<"event-data">> => Utf16},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(<<"BIOS 1.23">>, maps:get(<<"crtm-version">>, P)).

decode_crtm_version_ascii_test() ->
    Ev = #{<<"event-type-code">> => 16#8,
           <<"event-data">> => <<"AMI v5.19">>},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(<<"AMI v5.19">>, maps:get(<<"crtm-version">>, P)).

decode_separator_normal_vs_error_test() ->
    EvNormal = #{<<"event-type-code">> => 16#4,
                 <<"event-data">> => <<0,0,0,0>>},
    EvError = #{<<"event-type-code">> => 16#4,
                <<"event-data">> => <<16#FF,16#FF,16#FF,16#FF>>},
    ?assertEqual(<<"normal">>,
                 maps:get(<<"separator">>,
                          maps:get(<<"parsed">>,
                                   decode_event(EvNormal)))),
    ?assertEqual(<<"firmware_error">>,
                 maps:get(<<"separator">>,
                          maps:get(<<"parsed">>,
                                   decode_event(EvError)))).

decode_no_action_spec_id_test() ->
    AlgPairs = <<16#04:16/little, 20:16/little,
                 16#0B:16/little, 32:16/little>>,
    SpecId = <<"Spec ID Event03", 0,
               0:32/little, 0:8, 2:8, 0:8, 8:8, 2:32/little,
               AlgPairs/binary, 0:8>>,
    Ev = #{<<"event-type-code">> => 16#3, <<"event-data">> => SpecId},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(<<"Event03">>, maps:get(<<"spec-id">>, P)),
    ?assertEqual(2, length(maps:get(<<"algorithms">>, P))).

decode_uefi_image_load_test() ->
    DevicePath = <<16#01,16#02,16#03,16#04,16#05>>,  %% arbitrary bytes
    DpLen = byte_size(DevicePath),
    Data = <<16#1000:64/little, 16#20000:64/little,
             16#FFFFFFFF00000000:64/little, DpLen:64/little,
             DevicePath/binary>>,
    Ev = #{<<"event-type-code">> => 16#80000003,
           <<"event-data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(16#1000, maps:get(<<"image-location-in-memory">>, P)),
    ?assertEqual(16#20000, maps:get(<<"image-length-in-memory">>, P)),
    ?assertEqual(DpLen, maps:get(<<"device-path-length">>, P)),
    ?assertEqual(DevicePath, maps:get(<<"device-path">>, P)).

decode_ev_ipl_systemd_stub_kernel_cmdline_test() ->
    Ev = #{<<"event-type-code">> => 16#D,
           <<"event-data">> => <<"kernel_cmdline=ro quiet",0>>},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(<<"kernel-cmdline">>, maps:get(<<"key">>, P)),
    ?assertEqual(<<"ro quiet">>, maps:get(<<"value">>, P)),
    ?assertEqual(<<"key-value-ascii">>, maps:get(<<"format">>, P)),
    %% Now also: cmdline-flags submap with tokenised flags.
    Flags = maps:get(<<"cmdline-flags">>, P),
    ?assert(maps:is_key(<<"ro">>, Flags)),
    ?assert(maps:is_key(<<"quiet">>, Flags)),
    ?assertEqual(true, maps:get(<<"ro">>, Flags)),
    ?assertEqual(true, maps:get(<<"quiet">>, Flags)),
    ?assertEqual(2, maps:get(<<"-token-count">>, Flags)).

%% Full paper-strength cmdline: mem_encrypt / iommu / lockdown /
%% init_on_alloc / init_on_free / module.sig_enforce / verity
%% roothash. Derived from LapEE's own recommended cmdline
%% (paper section Architecture l.219-230).
parse_kernel_cmdline_security_flags_test() ->
    Raw = <<"ro quiet mem_encrypt=on intel_iommu=on iommu=pt "
            "iommu.strict=1 lockdown=confidentiality "
            "init_on_alloc=1 init_on_free=1 module.sig_enforce=1 "
            "roothash=deadbeef slab_nomerge page_poison=1 "
            "kvm_intel.nested=0">>,
    Flags = parse_kernel_cmdline(Raw),
    ?assertEqual(true,   maps:get(<<"ro">>, Flags)),
    ?assertEqual(true,   maps:get(<<"quiet">>, Flags)),
    ?assertEqual(true,   maps:get(<<"mem_encrypt">>, Flags)),
    ?assertEqual(true,   maps:get(<<"intel_iommu">>, Flags)),
    ?assertEqual(<<"pt">>, maps:get(<<"iommu">>, Flags)),
    ?assertEqual(true,   maps:get(<<"iommu.strict">>, Flags)),
    ?assertEqual(<<"confidentiality">>,
                 maps:get(<<"lockdown">>, Flags)),
    ?assertEqual(true,   maps:get(<<"init_on_alloc">>, Flags)),
    ?assertEqual(true,   maps:get(<<"init_on_free">>, Flags)),
    ?assertEqual(true,   maps:get(<<"module.sig_enforce">>, Flags)),
    ?assertEqual(<<"deadbeef">>, maps:get(<<"roothash">>, Flags)),
    ?assertEqual(true,   maps:get(<<"slab_nomerge">>, Flags)),
    ?assertEqual(true,   maps:get(<<"page_poison">>, Flags)),
    ?assertEqual(false,  maps:get(<<"kvm_intel.nested">>, Flags)),
    %% Token count is the full flag list.
    ?assertEqual(14, maps:get(<<"-token-count">>, Flags)),
    %% -boolean list contains the flags that appeared without "=".
    Bools = maps:get(<<"-boolean">>, Flags),
    ?assert(lists:member(<<"ro">>, Bools)),
    ?assert(lists:member(<<"quiet">>, Bools)),
    ?assert(lists:member(<<"slab_nomerge">>, Bools)),
    ?assertNot(lists:member(<<"mem_encrypt">>, Bools)).

%% Quoted-value tokenisation: `foo="a b c" bar' -> foo has the
%% three-word string, bar is a bool.
parse_kernel_cmdline_quoted_value_test() ->
    Raw = <<"foo=\"a b c\" bar baz=42">>,
    Flags = parse_kernel_cmdline(Raw),
    ?assertEqual(<<"a b c">>, maps:get(<<"foo">>, Flags)),
    ?assertEqual(true,        maps:get(<<"bar">>, Flags)),
    ?assertEqual(<<"42">>,    maps:get(<<"baz">>, Flags)),
    ?assertEqual(3, maps:get(<<"-token-count">>, Flags)).

%% Multi-value comma-list splits into a list.
parse_kernel_cmdline_multivalue_test() ->
    Raw = <<"iommu=pt,strict audit=1,2,3">>,
    Flags = parse_kernel_cmdline(Raw),
    ?assertEqual([<<"pt">>, <<"strict">>],
                 maps:get(<<"iommu">>, Flags)),
    ?assertEqual([<<"1">>, <<"2">>, <<"3">>],
                 maps:get(<<"audit">>, Flags)).

decode_ev_ipl_opaque_test() ->
    Ev = #{<<"event-type-code">> => 16#D,
           <<"event-data">> => <<0,1,2,3,4,5>>},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    %% Not key=value ASCII -> format=opaque
    ?assertEqual(<<"opaque">>, maps:get(<<"format">>, P)),
    ?assertEqual(6, maps:get(<<"length">>, P)).

%% Hour-4: SIPA per-subtype payload decode.
%% Build a SIPA event for `Vbs` (0x00010024 = bool) whose
%% SIPA_EVENT_HEADER carries a single 0x01 payload byte.
decode_sipa_bool_vbs_test() ->
    %% Outer category: SIPA_EVENTTYPE_OSPARAMETER = 0x10000004
    Payload = <<1:8>>,
    SubType = 16#00010024,
    SubSize = byte_size(Payload),
    Data = <<SubType:32/little, SubSize:32/little, Payload/binary>>,
    Ev = #{<<"event-type-code">> => 16#10000004,
           <<"event-data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(<<"SIPA_EVENTTYPE_OSPARAMETER">>,
                 maps:get(<<"sipa-category">>, P)),
    ?assertEqual(<<"Vbs">>, maps:get(<<"sipa-subtype-name">>, P)),
    ?assertEqual(<<"bool">>, maps:get(<<"sipa-payload-type">>, P)),
    ?assertEqual(true, maps:get(<<"sipa-value-bool">>, P)).

%% SIPA u32 payload: `BootCounter' (0x0001000A) is a u32.
decode_sipa_u32_boot_counter_test() ->
    Payload = <<16#A1B2C3D4:32/little>>,
    SubType = 16#0001000A,
    SubSize = byte_size(Payload),
    Data = <<SubType:32/little, SubSize:32/little, Payload/binary>>,
    Ev = #{<<"event-type-code">> => 16#10000004,
           <<"event-data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(<<"BootCounter">>,
                 maps:get(<<"sipa-subtype-name">>, P)),
    ?assertEqual(<<"u32">>, maps:get(<<"sipa-payload-type">>, P)),
    ?assertEqual(16#A1B2C3D4, maps:get(<<"sipa-value-u32">>, P)).

%% SIPA digest payload: `CertRootHash' (0x00040004) is a digest.
%% Feed a 32-byte SHA-256 digest and assert the decoded alg.
decode_sipa_digest_cert_root_hash_test() ->
    Digest = crypto:hash(sha256, <<"test cert">>),
    SubType = 16#00040004,
    SubSize = byte_size(Digest),
    Data = <<SubType:32/little, SubSize:32/little, Digest/binary>>,
    Ev = #{<<"event-type-code">> => 16#10000005,
           <<"event-data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(<<"CertRootHash">>,
                 maps:get(<<"sipa-subtype-name">>, P)),
    ?assertEqual(<<"digest">>,
                 maps:get(<<"sipa-payload-type">>, P)),
    ?assertEqual(<<"sha256">>,
                 maps:get(<<"sipa-value-digest-alg">>, P)),
    ?assertEqual(32, maps:get(<<"sipa-value-digest-size">>, P)),
    ?assertEqual(hb_util:encode(Digest),
                 maps:get(<<"sipa-value-digest">>, P)).

%% SIPA UTF-16LE string payload: `LoadedModuleName' (0x00030002).
decode_sipa_utf16_module_name_test() ->
    Name16 = unicode:characters_to_binary(
               <<"ntoskrnl.exe">>, utf8, {utf16, little}),
    Payload = <<Name16/binary, 0, 0>>, % NUL-terminated UTF-16
    SubType = 16#00030002,
    SubSize = byte_size(Payload),
    Data = <<SubType:32/little, SubSize:32/little, Payload/binary>>,
    Ev = #{<<"event-type-code">> => 16#10000006,
           <<"event-data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(<<"LoadedModuleName">>,
                 maps:get(<<"sipa-subtype-name">>, P)),
    ?assertEqual(<<"utf16-string">>,
                 maps:get(<<"sipa-payload-type">>, P)),
    ?assertEqual(<<"ntoskrnl.exe">>,
                 maps:get(<<"sipa-value-string">>, P)).

%% Aggregation subtypes surface length + SHA-256; the recursion
%% into nested events is a future iteration's work.
decode_sipa_aggregation_test() ->
    Payload = <<0:128>>,  %% 16 bytes of sub-event noise
    SubType = 16#00030001,
    SubSize = byte_size(Payload),
    Data = <<SubType:32/little, SubSize:32/little, Payload/binary>>,
    Ev = #{<<"event-type-code">> => 16#10000006,
           <<"event-data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(<<"LoadedModuleAggregation">>,
                 maps:get(<<"sipa-subtype-name">>, P)),
    ?assertEqual(<<"aggregation">>,
                 maps:get(<<"sipa-payload-type">>, P)),
    ?assertEqual(16,
                 maps:get(<<"sipa-aggregation-length">>, P)).

decode_firmware_blob_test() ->
    Ev = #{<<"event-type-code">> => 16#80000008,
           <<"event-data">> => <<16#FF000000:64/little,
                                 16#100000:64/little>>},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(16#FF000000, maps:get(<<"blob-physical-address">>, P)),
    ?assertEqual(16#100000, maps:get(<<"blob-length">>, P)).

%% Hour-10: TCG_DEVICE_SECURITY_EVENT_DATA2 round-trip. Build a
%% synthetic v2 SPDM event with a PCI device type + SPDM
%% measurement block (DMTF mutable-firmware type) + simple
%% device path, feed through decode_event, assert every field.
decode_spdm_event_data2_test() ->
    Sig = <<"SPDM Device Sec2">>,
    DmtfValue = <<0:256>>,          %% 32 zero bytes
    DmtfMeas = <<1:8,               %% type = mutable-firmware
                 32:16/little, DmtfValue/binary>>,
    MeasBlock = <<1:8,               %% meas-block-index
                  16#01:8,           %% DMTF spec
                  (byte_size(DmtfMeas)):16/little,
                  DmtfMeas/binary>>,
    SubHeaderBody = <<16#0012:16/little, MeasBlock/binary>>,
    %% Device path: minimal "ACPI(HID=0, UID=0)" + End-entire.
    AcpiNode = <<16#02, 16#01, 12:16/little, 0:32, 0:32>>,
    EndEntire = <<16#7F, 16#FF, 16#04, 16#00>>,
    DevicePath = <<AcpiNode/binary, EndEntire/binary>>,
    Data = <<Sig/binary,
             16#0002:16/little,                %% Version
             0:8,                              %% AuthState = Success
             0:8,                              %% Reserved
             0:32/little,                      %% Length
             1:32/little,                      %% DeviceType = PCI
             0:32/little,                      %% SubHeaderType = MEAS
             (byte_size(SubHeaderBody)):32/little,
             16#DEADBEEFCAFEBABE:64/little,    %% SubHeaderUid
             SubHeaderBody/binary,
             (byte_size(DevicePath)):64/little,
             DevicePath/binary>>,
    Ev = #{<<"event-type-code">> => 16#800000E1,
           <<"event-data">>      => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(2, maps:get(<<"spdm-data-version">>, P)),
    ?assertEqual(<<"Success">>,
                 maps:get(<<"auth-state-name">>, P)),
    ?assertEqual(<<"PCI">>,
                 maps:get(<<"device-type-name">>, P)),
    ?assertEqual(<<"SPDM_MEAS_BLOCK">>,
                 maps:get(<<"sub-header-type-name">>, P)),
    ?assertEqual(1, maps:get(<<"meas-block-index">>, P)),
    ?assertEqual(<<"DMTF">>,
                 maps:get(<<"meas-block-spec-name">>, P)),
    ?assertEqual(<<"mutable-firmware">>,
                 maps:get(<<"dmtf-value-type-name">>, P)),
    ?assertEqual(false, maps:get(<<"dmtf-value-is-raw">>, P)),
    ?assertEqual(32, maps:get(<<"dmtf-value-size">>, P)),
    ?assertEqual(<<"0xDEADBEEFCAFEBABE">>,
                 maps:get(<<"sub-header-uid-hex">>, P)),
    ok.

%% Hour-10: SPDM cert-chain sub-header decoded correctly.
decode_spdm_cert_chain_test() ->
    Sig = <<"SPDM Device Sec2">>,
    CertChain = <<"FAKE-PKCS7-CERT-CHAIN-PAYLOAD">>,
    SubHeaderBody = <<16#0012:16/little, 0:8, 0:8,
                       16#00000002:32/little,   %% SPDM_HASH_SHA_384
                       CertChain/binary>>,
    Data = <<Sig/binary,
             16#0002:16/little, 0:8, 0:8,
             0:32/little, 2:32/little,           %% DeviceType = USB
             1:32/little,                        %% SubHeaderType=CERT
             (byte_size(SubHeaderBody)):32/little,
             0:64/little,
             SubHeaderBody/binary,
             0:64/little>>,                      %% no device path
    Ev = #{<<"event-type-code">> => 16#800000E4,
           <<"event-data">>      => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(<<"SPDM_CERT_CHAIN">>,
                 maps:get(<<"sub-header-type-name">>, P)),
    ?assertEqual(<<"USB">>,
                 maps:get(<<"device-type-name">>, P)),
    ?assertEqual(<<"spdm-sha-384">>,
                 maps:get(<<"spdm-cert-hash-alg-name">>, P)),
    ?assertEqual(byte_size(CertChain),
                 maps:get(<<"spdm-cert-chain-length">>, P)),
    ok.

%% Hour-10: unsigned / non-canonical SPDM data falls through to
%% the legacy path-first heuristic.
decode_spdm_event_legacy_fallback_test() ->
    Data = <<"some-non-canonical-spdm-blob",
             16#7F, 16#FF, 16#04, 16#00,    %% end-entire terminator
             "payload-after-terminator">>,
    Ev = #{<<"event-type-code">> => 16#800000E1,
           <<"event-data">>      => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(0, maps:get(<<"spdm-data-version">>, P)),
    ?assertEqual(<<"firmware-blob">>, maps:get(<<"spdm-kind">>, P)),
    ok.

decode_firmware_blob2_with_description_test() ->
    Desc = <<"main-fw">>,
    DescLen = byte_size(Desc),
    Ev = #{<<"event-type-code">> => 16#8000000A,
           <<"event-data">> =>
               <<DescLen:8, Desc/binary,
                 16#FF000000:64/little,
                 16#100000:64/little>>},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(<<"main-fw">>, maps:get(<<"blob-description">>, P)).

decode_cpu_microcode_header_test() ->
    %% 28-byte header prefix is enough for our parser.
    Data = <<1:32/little, 16#12345:32/little, 16#20240101:32/little,
             16#806EA:32/little, 0:32/little, 1:32/little,
             1:32/little, 100:32/little, 200:32/little,
             0:96>>,
    Ev = #{<<"event-type-code">> => 16#9,
           <<"event-data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(<<"intel">>, maps:get(<<"format">>, P)),
    ?assertEqual(16#12345, maps:get(<<"update-revision">>, P)),
    ?assertEqual(16#20240101, maps:get(<<"date-bcd">>, P)),
    ?assertEqual(<<"2024-01-01">>, maps:get(<<"date">>, P)).

%% AMD microcode header -- 64-byte `microcode_header_amd` layout per
%% arch/x86/kernel/cpu/microcode/amd.c. Discriminator: first 4 bytes
%% are a BCD date 0x20YYMMDD, not HeaderVersion=1.
decode_amd_cpu_microcode_test() ->
    %% AMD Ryzen 7040 (Phoenix): patch 0x0AA00212 from 2024-04-15.
    DataCode          = 16#20240415,
    PatchId           = 16#0AA00212,
    McPatchDataId     = 16#0050,
    McPatchDataLen    = 16,
    InitFlag          = 0,
    PatchDataChecksum = 16#DEADBEEF,
    NbDevId           = 0,
    SbDevId           = 0,
    ProcessorRevId    = 16#AA50,  %% family/model indicator
    NbRevId           = 0,
    SbRevId           = 0,
    BiosApiRev        = 1,
    Reserved          = <<0, 0, 0>>,
    MatchReg          = <<0:256>>,
    Data = <<DataCode:32/little, PatchId:32/little,
             McPatchDataId:16/little, McPatchDataLen:8, InitFlag:8,
             PatchDataChecksum:32/little, NbDevId:32/little,
             SbDevId:32/little, ProcessorRevId:16/little,
             NbRevId:8, SbRevId:8, BiosApiRev:8,
             Reserved/binary, MatchReg/binary>>,
    Ev = #{<<"event-type-code">> => 16#9, <<"event-data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(<<"amd">>, maps:get(<<"format">>, P)),
    ?assertEqual(DataCode, maps:get(<<"data-code">>, P)),
    ?assertEqual(<<"2024-04-15">>, maps:get(<<"date">>, P)),
    ?assertEqual(PatchId, maps:get(<<"patch-id">>, P)),
    ?assertEqual(ProcessorRevId, maps:get(<<"processor-rev-id">>, P)),
    ?assertEqual(<<"0xAA50">>,
                 maps:get(<<"processor-rev-id-hex">>, P)).

decode_malformed_uefi_variable_returns_error_test() ->
    Ev = #{<<"event-type-code">> => 16#80000001,
           <<"event-data">> => <<1,2,3>>},  %% way too short
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertMatch(#{<<"error">> := _}, P).

decode_unknown_event_type_is_no_op_test() ->
    %% Unregistered code -> empty `parsed'.
    Ev = #{<<"event-type-code">> => 16#DEADBEEF,
           <<"event-data">> => <<>>},
    ?assertEqual(#{}, maps:get(<<"parsed">>, decode_event(Ev))).

%% EV_EFI_VARIABLE_BOOT -- BootOrder is an array of u16 little-endian.
decode_uefi_variable_boot_order_test() ->
    %% UEFI_VARIABLE_DATA for BootOrder = [0001, 0002, 0000] (3 u16).
    Guid = <<0:(16*8)>>,
    Name = unicode:characters_to_binary(<<"BootOrder">>, utf8,
                                          {utf16, little}),
    NameLen = byte_size(Name) div 2,
    DataBin = <<1:16/little, 2:16/little, 0:16/little>>,
    DataLen = byte_size(DataBin),
    Uv = <<Guid/binary, NameLen:64/little, DataLen:64/little,
           Name/binary, DataBin/binary>>,
    Ev = #{<<"event-type-code">> => 16#80000002,
           <<"event-data">> => Uv},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    Sem = maps:get(<<"semantic">>, P),
    ?assertEqual([<<"Boot0001">>, <<"Boot0002">>, <<"Boot0000">>],
                 maps:get(<<"boot-order">>, Sem)),
    ?assertEqual(3, maps:get(<<"boot-order-count">>, Sem)).

%% EV_EFI_GPT_EVENT -- UEFI_GPT_DATA with a minimal EFI_PARTITION_TABLE_
%% HEADER. We check disk-guid parsing + header fields.
decode_gpt_event_test() ->
    DiskGuid = <<16#01,16#02,16#03,16#04,     %% u32-LE = 04030201
                 16#05,16#06,                 %% u16-LE = 0605
                 16#07,16#08,                 %% u16-LE = 0807
                 16#09,16#0A,                 %% byte[2]
                 16#0B,16#0C,16#0D,16#0E,16#0F,16#10>>,
    Hdr = <<
        "EFI PART",                           %% 8B sig
        16#00010000:32/little,                %% rev
        92:32/little,                         %% hdrSize
        16#DEADBEEF:32/little,                %% hdrCrc
        0:32,                                 %% reserved
        1:64/little,                          %% myLba
        2:64/little,                          %% altLba
        34:64/little,                         %% firstUsable
        100:64/little,                        %% lastUsable
        DiskGuid/binary,                      %% diskGuid
        2:64/little,                          %% partEntryLba
        128:32/little,                        %% numEntries
        128:32/little,                        %% entrySize
        16#CAFEBEEF:32/little>>,              %% partArrCrc
    %% UEFI_GPT_DATA adds a u64 "number-of-partition-entries" after the
    %% header. Claim 4 measured entries.
    Data = <<Hdr/binary, 4:64/little>>,
    Ev = #{<<"event-type-code">> => 16#80000006,
           <<"event-data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(<<"04030201-0605-0807-090a-0b0c0d0e0f10">>,
                 maps:get(<<"disk-guid">>, P)),
    ?assertEqual(1, maps:get(<<"my-lba">>, P)),
    ?assertEqual(128, maps:get(<<"size-of-partition-entry">>, P)),
    ?assertEqual(4, maps:get(<<"measured-partition-count">>, P)).

%% EV_EFI_HANDOFF_TABLES2 -- 1-byte-length-prefixed UTF-8 description.
decode_handoff_tables2_test() ->
    Desc = <<"ACPI 2.0">>,
    Len = byte_size(Desc),
    Data = <<Len:8, Desc/binary, 0,0,0,0>>,  %% trailing table bytes
    Ev = #{<<"event-type-code">> => 16#8000000B,
           <<"event-data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(<<"ACPI 2.0">>,
                 maps:get(<<"table-description">>, P)),
    ?assertEqual(Len,
                 maps:get(<<"table-description-length">>, P)).

%% EV_EVENT_TAG -- TCG_PCClientTaggedEvent {taggedEventID u32,
%% taggedEventDataSize u32, taggedEventData [size]}.
decode_event_tag_test() ->
    Payload = <<"some-tag-payload">>,
    PayloadLen = byte_size(Payload),
    %% Use a QEMU-style arbitrary TagID.
    TagId = 16#d9dfa6d8,
    Data = <<TagId:32/little, PayloadLen:32/little, Payload/binary>>,
    Ev = #{<<"event-type-code">> => 16#6,
           <<"event-data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(TagId, maps:get(<<"tag-id">>, P)),
    ?assertEqual(<<"0xD9DFA6D8">>, maps:get(<<"tag-id-hex">>, P)),
    ?assertEqual(PayloadLen, maps:get(<<"tag-data-length">>, P)).

%% EV_EVENT_TAG -- the systemd-stub UKI measurement annotations.
%% sd-stub uses TagIDs 0xf5bc582a / 0x6c46f751 / 0x49dffe0f /
%% 0xdac08e1a / 0x13aed6db (from src/boot/measure.h) with a
%% UTF-16LE description of the measured blob.
decode_event_tag_systemd_stub_test() ->
    %% Kernel profile name -- UKI_PROFILE_EVENT_TAG_ID = 0x13aed6db.
    ProfileText = <<"default">>,
    ProfileUtf16 = unicode:characters_to_binary(ProfileText, utf8,
                                                  {utf16, little}),
    Payload = <<ProfileUtf16/binary, 0, 0>>,  %% UTF-16 NUL-terminated
    PayloadLen = byte_size(Payload),
    Data = <<16#13aed6db:32/little, PayloadLen:32/little,
             Payload/binary>>,
    Ev = #{<<"event-type-code">> => 16#6, <<"event-data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(<<"SYSTEMD_STUB_UKI_PROFILE">>,
                 maps:get(<<"tag-id-name">>, P)),
    ?assertEqual(<<"default">>,
                 maps:get(<<"tag-description">>, P)).

%% UEFI device path walker -- basic path, three nodes +
%% end: PciRoot ACPI, PCI(0x0,0x1F), FilePath \EFI\BOOT\BOOTX64.EFI
parse_device_path_basic_test() ->
    %% PciRoot -- ACPI HID PNP0A03 (EISA 0x030AD041), UID 0.
    %% ACPI _HID encoding for PNP0A03:
    %%    'P'=16, 'N'=14, 'P'=16 -> V1=0x10, V2=0x0E, V3=0x10
    %%    packed byte0 = 0x10<<2 | 0x0E>>3 = 0x41; byte1 = (0x0E<<5)|0x10 = 0xD0
    %%    HID = 0x0A03 << 16 | (0xD041 as low 16 swapped -> 0x41D0)
    %% Actually for our test we just verify the walker produces
    %% the right 3 structured nodes + a non-empty text. The exact
    %% HID decode is separately exercised below.
    AcpiNode = <<16#02, 16#01, 16#0C, 16#00,
                 16#41, 16#D0, 16#0A, 16#03,      %% HID little-endian
                 16#00, 16#00, 16#00, 16#00>>,    %% UID
    PciNode  = <<16#01, 16#01, 16#06, 16#00, 16#00, 16#1F>>,
    %% File path node for "\EFI" (UCS-2, NUL-terminated, 10 bytes).
    FilePath = <<"\\", 0, "E", 0, "F", 0, "I", 0, 0, 0>>,
    FpLen = byte_size(FilePath) + 4,
    FpNode = <<16#04, 16#04, FpLen:16/little, FilePath/binary>>,
    End    = <<16#7F, 16#FF, 16#04, 16#00>>,
    DP = <<AcpiNode/binary, PciNode/binary, FpNode/binary, End/binary>>,
    {Nodes, Text} = parse_device_path(DP),
    ?assertEqual(4, length(Nodes)),  %% 3 content + 1 end
    ?assertNotEqual(nomatch,
                     binary:match(Text, <<"\\EFI">>)),
    Acpi = lists:nth(1, Nodes),
    ?assertEqual(<<"acpi">>, maps:get(<<"type-name">>, Acpi)),
    ?assertEqual(<<"acpi">>, maps:get(<<"subtype-name">>, Acpi)),
    Pci = lists:nth(2, Nodes),
    ?assertEqual(<<"pci">>, maps:get(<<"subtype-name">>, Pci)),
    ?assertEqual(16#1F, maps:get(<<"device">>, Pci)),
    Fp = lists:nth(3, Nodes),
    ?assertEqual(<<"file-path">>, maps:get(<<"subtype-name">>, Fp)),
    ?assertEqual(<<"\\EFI">>, maps:get(<<"path">>, Fp)),
    EndN = lists:nth(4, Nodes),
    ?assertEqual(<<"end">>, maps:get(<<"type-name">>, EndN)).

%% UEFI device path -- SATA + Hard Drive (GPT) + File Path typical
%% boot device shape.
parse_device_path_sata_gpt_test() ->
    SataNode = <<16#03, 16#12, 16#0A, 16#00,
                 16#00, 16#00,        %% HBA port 0
                 16#FF, 16#FF,        %% PMP port 0xFFFF (direct)
                 16#00, 16#00>>,      %% LUN 0
    GptGuid = <<1:32/little, 2:16/little, 3:16/little,
                4, 5, 6, 7, 8, 9, 10, 11>>,
    HdNode = <<16#04, 16#01, 16#2A, 16#00,
               1:32/little,
               2048:64/little,
               204800:64/little,
               GptGuid/binary,
               16#02,            %% GPT format
               16#02>>,           %% signature type: GPT GUID
    FpRaw = <<"\\EFI\\BOOT\\BOOTX64.EFI">>,
    FpUcs = unicode:characters_to_binary(FpRaw, utf8, {utf16, little}),
    FpBin = <<FpUcs/binary, 0, 0>>,
    FpLen = byte_size(FpBin) + 4,
    FpNode = <<16#04, 16#04, FpLen:16/little, FpBin/binary>>,
    End    = <<16#7F, 16#FF, 16#04, 16#00>>,
    DP = <<SataNode/binary, HdNode/binary, FpNode/binary, End/binary>>,
    {Nodes, Text} = parse_device_path(DP),
    ?assertEqual(4, length(Nodes)),
    %% SATA node.
    Sata = lists:nth(1, Nodes),
    ?assertEqual(<<"sata">>, maps:get(<<"subtype-name">>, Sata)),
    ?assertEqual(0, maps:get(<<"hba-port">>, Sata)),
    %% HD node with GPT signature.
    Hd = lists:nth(2, Nodes),
    ?assertEqual(<<"hard-drive">>, maps:get(<<"subtype-name">>, Hd)),
    ?assertEqual(<<"gpt">>, maps:get(<<"partition-format">>, Hd)),
    ?assertEqual(<<"gpt-guid">>, maps:get(<<"signature-type">>, Hd)),
    ?assertEqual(1, maps:get(<<"partition-number">>, Hd)),
    %% File path.
    Fp = lists:nth(3, Nodes),
    ?assertEqual(<<"\\EFI\\BOOT\\BOOTX64.EFI">>,
                 maps:get(<<"path">>, Fp)),
    %% Textual rendering contains the key parts.
    ?assertNotEqual(nomatch, binary:match(Text, <<"Sata(">>)),
    ?assertNotEqual(nomatch, binary:match(Text, <<"HD(">>)),
    ?assertNotEqual(nomatch, binary:match(Text, <<"BOOTX64.EFI">>)).

%% EV_EFI_BOOT_SERVICES_APPLICATION full parse -- image load event
%% should now carry a structured device path + text.
decode_uefi_image_load_walks_device_path_test() ->
    FpRaw = <<"\\EFI\\BOOT\\SHIMX64.EFI">>,
    FpUcs = unicode:characters_to_binary(FpRaw, utf8, {utf16, little}),
    FpBin = <<FpUcs/binary, 0, 0>>,
    FpLen = byte_size(FpBin) + 4,
    FpNode = <<16#04, 16#04, FpLen:16/little, FpBin/binary>>,
    End    = <<16#7F, 16#FF, 16#04, 16#00>>,
    DP = <<FpNode/binary, End/binary>>,
    DpLen = byte_size(DP),
    Data = <<16#1000:64/little, 16#20000:64/little,
             16#FFFFFFFF00000000:64/little, DpLen:64/little,
             DP/binary>>,
    Ev = #{<<"event-type-code">> => 16#80000003,
           <<"event-data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    Nodes = maps:get(<<"device-path-nodes">>, P),
    ?assertEqual(2, length(Nodes)),
    Text = maps:get(<<"device-path-text">>, P),
    ?assertNotEqual(nomatch, binary:match(Text, <<"SHIMX64.EFI">>)).

%% X.509 signature list -- a valid self-signed cert ends up fully
%% decoded (issuer DN + subject + fingerprint + key algorithm).
decode_x509_signature_list_test() ->
    %% Generate a self-signed RSA cert in-test so we have a
    %% deterministic ASN.1-valid sample without shipping a .pem.
    {Cert, _Key} = generate_test_self_signed_cert(),
    Der = public_key:pkix_encode('OTPCertificate', Cert, otp),
    %% Build one EFI_SIGNATURE_LIST containing one EFI_CERT_X509
    %% entry with owner GUID = zeros + cert DER.
    X509TypeGuid =
        %% a5c059a1-94e4-4aa7-87b5-ab155c2bf072 in mixed-endian
        <<16#a1, 16#59, 16#c0, 16#a5,
          16#e4, 16#94, 16#a7, 16#4a,
          16#87, 16#b5, 16#ab, 16#15, 16#5c, 16#2b, 16#f0, 16#72>>,
    Owner = <<0:(16*8)>>,
    SigSize = 16 + byte_size(Der),
    HdrSize = 0,
    ListSize = 28 + HdrSize + SigSize,
    EFI_SIG_LIST = <<
        X509TypeGuid/binary,
        ListSize:32/little,
        HdrSize:32/little,
        SigSize:32/little,
        Owner/binary,
        Der/binary
    >>,
    [Entry] = summarise_signature_list(EFI_SIG_LIST),
    ?assertEqual(<<"EFI_CERT_X509_GUID">>,
                 maps:get(<<"type-guid-name">>, Entry)),
    ?assertEqual(1, maps:get(<<"entry-count">>, Entry)),
    [Cert1] = maps:get(<<"entries">>, Entry),
    ?assert(maps:is_key(<<"x509-sha256-fingerprint">>, Cert1)),
    ?assert(maps:is_key(<<"x509-issuer">>, Cert1)),
    ?assert(maps:is_key(<<"x509-subject">>, Cert1)),
    ?assert(maps:is_key(<<"x509-not-before">>, Cert1)),
    ?assert(maps:is_key(<<"x509-not-after">>, Cert1)),
    ?assert(maps:is_key(<<"x509-public-key-alg">>, Cert1)),
    ?assert(maps:is_key(<<"x509-public-key-size-bits">>, Cert1)),
    ?assertEqual(<<"rsa">>, maps:get(<<"x509-public-key-alg">>, Cert1)).

%% Malformed X.509 in a signature list: we report a decode error
%% but still provide a SHA-256 fingerprint of the raw bytes so a
%% policy engine can at least pin the opaque entry.
decode_malformed_x509_returns_error_test() ->
    GarbageCert = <<"not-a-cert">>,
    X509TypeGuid = <<16#a1, 16#59, 16#c0, 16#a5,
                     16#e4, 16#94, 16#a7, 16#4a,
                     16#87, 16#b5, 16#ab, 16#15,
                     16#5c, 16#2b, 16#f0, 16#72>>,
    Owner = <<0:(16*8)>>,
    SigSize = 16 + byte_size(GarbageCert),
    HdrSize = 0,
    ListSize = 28 + HdrSize + SigSize,
    Bin = <<X509TypeGuid/binary, ListSize:32/little,
            HdrSize:32/little, SigSize:32/little,
            Owner/binary, GarbageCert/binary>>,
    [Entry] = summarise_signature_list(Bin),
    [Cert1] = maps:get(<<"entries">>, Entry),
    ?assert(maps:is_key(<<"x509-decode-error">>, Cert1)),
    ?assert(maps:is_key(<<"x509-sha256-fingerprint">>, Cert1)).

%% Helper: generate a self-signed RSA cert using OTP public_key.
generate_test_self_signed_cert() ->
    Key = public_key:generate_key({rsa, 2048, 65537}),
    PubKey = #'RSAPublicKey'{
        modulus = Key#'RSAPrivateKey'.modulus,
        publicExponent = Key#'RSAPrivateKey'.publicExponent
    },
    Subject = {rdnSequence, [
        [#'AttributeTypeAndValue'{
            type = ?'id-at-commonName',
            value = {utf8String, <<"LapEE Test Cert">>}
        }],
        [#'AttributeTypeAndValue'{
            type = ?'id-at-organizationName',
            value = {utf8String, <<"Test">>}
        }]
    ]},
    Validity = #'Validity'{
        notBefore = {utcTime, "250101000000Z"},
        notAfter  = {utcTime, "350101000000Z"}
    },
    TbsCert = #'OTPTBSCertificate'{
        version = v3,
        serialNumber = 1,
        signature = #'SignatureAlgorithm'{
            algorithm = ?sha256WithRSAEncryption,
            parameters = 'NULL'
        },
        issuer = Subject,
        validity = Validity,
        subject = Subject,
        subjectPublicKeyInfo = #'OTPSubjectPublicKeyInfo'{
            algorithm = #'PublicKeyAlgorithm'{
                algorithm = ?rsaEncryption,
                parameters = 'NULL'
            },
            subjectPublicKey = PubKey
        }
    },
    Signed = public_key:pkix_sign(TbsCert, Key),
    %% pkix_sign returns a DER binary; re-decode to OTPCertificate
    %% for our caller.
    Cert = public_key:pkix_decode_cert(Signed, otp),
    {Cert, Key}.

%% SMBIOS v2.x entry point -- 31 bytes anchored at "_SM_".
parse_smbios_v2_entry_point_test() ->
    EP = <<"_SM_", 16#CC:8, 31:8, 3:8, 5:8, 16#1000:16/little,
           0:8, 0:40,
           "_DMI_", 16#DD:8, 16#1234:16/little, 16#80000000:32/little,
           8:16/little, 16#25:8>>,
    P = parse_smbios(EP),
    ?assertEqual(<<"_SM_">>,    maps:get(<<"anchor">>, P)),
    ?assertEqual(<<"3.5">>,     maps:get(<<"version">>, P)),
    ?assertEqual(31,            maps:get(<<"entry-point-length">>, P)),
    ?assertEqual(16#1234,       maps:get(<<"table-length">>, P)),
    ?assertEqual(16#80000000,   maps:get(<<"table-address">>, P)),
    ?assertEqual(8,             maps:get(<<"number-of-structures">>, P)).

%% SMBIOS v3 entry point -- 24 bytes anchored at "_SM3_".
parse_smbios_v3_entry_point_test() ->
    EP = <<"_SM3_", 16#AA:8, 24:8, 3:8, 6:8, 0:8, 16#01:8, 0:8,
           16#20000:32/little, 16#F0000000:64/little>>,
    P = parse_smbios(EP),
    ?assertEqual(<<"_SM3_">>,    maps:get(<<"anchor">>, P)),
    ?assertEqual(<<"3.6">>,      maps:get(<<"version">>, P)),
    ?assertEqual(16#F0000000,    maps:get(<<"table-address">>, P)).

%% SMBIOS structure -- Type 1 (System Information) with UUID +
%% manufacturer + product.
parse_smbios_type1_test() ->
    Fields = <<1:8, 2:8, 3:8, 4:8,
               16#01020304050607080910111213141516:128,
               1:8, 0:8, 0:8>>,
    Strings = <<"Lenovo", 0,
                "ThinkPad X1 Carbon Gen 11", 0,
                "21HM006VUS", 0,
                "PC12345", 0,
                0>>,
    Type1 = <<1:8, 27:8, 16#0100:16/little,
              Fields/binary,
              Strings/binary>>,
    P = parse_smbios_structure(Type1),
    ?assertEqual(1, maps:get(<<"smbios-type">>, P)),
    ?assertEqual(<<"System Information">>,
                 maps:get(<<"smbios-type-name">>, P)),
    ?assertEqual(<<"Lenovo">>,
                 maps:get(<<"system-manufacturer">>, P)),
    ?assertEqual(<<"ThinkPad X1 Carbon Gen 11">>,
                 maps:get(<<"system-product-name">>, P)),
    ?assertEqual(<<"21HM006VUS">>,
                 maps:get(<<"system-version">>, P)),
    ?assertEqual(<<"PC12345">>,
                 maps:get(<<"system-serial">>, P)).

%% ACPI table header -- pick the TPM2 ACPI table.
parse_acpi_tpm2_header_test() ->
    Hdr = <<"TPM2",
            76:32/little,
            4:8, 16#AB:8,
            "LENOVO",
            "TP-TPM2_",
            16#00010000:32/little,
            "LNVO",
            16#0001:32/little,
            0:4/unit:8>>,   %% 4 bytes padding after the 36-byte header
    P = parse_acpi_table(Hdr),
    ?assertEqual(<<"TPM2">>,                  maps:get(<<"signature">>, P)),
    ?assertEqual(<<"Trusted Platform Module 2.0">>,
                 maps:get(<<"signature-name">>, P)),
    ?assertEqual(76,                          maps:get(<<"length">>, P)),
    ?assertEqual(<<"LENOVO">>,                maps:get(<<"oem-id">>, P)),
    ?assertEqual(<<"TP-TPM2_">>,              maps:get(<<"oem-table-id">>, P)).

%% ACPI RSDP v2 (36 bytes).
parse_acpi_rsdp_v2_test() ->
    Rsdp = <<"RSD PTR ",
             16#BB:8, "INTEL ", 2:8, 16#7FF00000:32/little,
             36:32/little, 16#0000000080000000:64/little,
             16#CC:8, 0, 0, 0>>,
    P = parse_acpi_rsdp(Rsdp),
    ?assertEqual(<<"RSD PTR ">>, maps:get(<<"signature">>, P)),
    ?assertEqual(2,              maps:get(<<"revision">>, P)),
    ?assertEqual(16#7FF00000,    maps:get(<<"rsdt-address">>, P)),
    ?assertEqual(16#0000000080000000,
                 maps:get(<<"xsdt-address">>, P)).

%% systemd-stub PE section -> PCR mapping.
systemd_stub_pe_section_pcr_test() ->
    ?assertEqual(11, systemd_stub_pe_section_pcr(<<".linux">>)),
    ?assertEqual(11, systemd_stub_pe_section_pcr(<<"linux">>)),
    ?assertEqual(12, systemd_stub_pe_section_pcr(<<"cmdline">>)),
    ?assertEqual(12, systemd_stub_pe_section_pcr(<<"kernel-cmdline">>)),
    ?assertEqual(11, systemd_stub_pe_section_pcr(<<".osrel">>)),
    ?assertEqual(11, systemd_stub_pe_section_pcr(<<"initrd">>)),
    ?assertEqual(undefined,
                 systemd_stub_pe_section_pcr(<<"not-a-section">>)),
    ?assert(is_systemd_stub_pe_section(<<".linux">>)),
    ?assert(is_systemd_stub_pe_section(<<"kernel-cmdline">>)),
    ?assertNot(is_systemd_stub_pe_section(<<"not-a-section">>)).

%% Pipeline: parse a log, then decode_events to get the
%% per-event `parsed' enrichment on every entry.
decode_events_on_full_fixture_test() ->
    Raw = build_fixture(),
    Parsed = parse(Raw),
    Decoded = decode_events(Parsed),
    %% Event 3 is the SecureBoot variable -- should be
    %% semantically decoded.
    E3 = maps:get(<<"3">>, Decoded),
    P3 = maps:get(<<"parsed">>, E3),
    ?assertEqual(<<"SecureBoot">>, maps:get(<<"variable-name">>, P3)),
    Sem = maps:get(<<"semantic">>, P3),
    ?assertEqual(#{<<"secure-boot-enabled">> => true}, Sem),
    %% Event 2 is CRTM_VERSION -- should have decoded string.
    E2 = maps:get(<<"2">>, Decoded),
    P2 = maps:get(<<"parsed">>, E2),
    ?assertEqual(<<"TEST FW v1">>, maps:get(<<"crtm-version">>, P2)).

%%%---- Helper used by decoder tests ------------------------------------

build_uefi_variable(GuidBin, NameUtf8, VarData) ->
    Name = unicode:characters_to_binary(NameUtf8, utf8,
                                          {utf16, little}),
    NameLen = byte_size(Name) div 2,
    DataLen = byte_size(VarData),
    <<GuidBin/binary,
      NameLen:64/little,
      DataLen:64/little,
      Name/binary,
      VarData/binary>>.

%%%---- Real-world fixture harness -------------------------------------
%%%
%%% Parses every file under `priv/tpm-interpret/fixtures/` captured
%%% from real hardware (Lenovo ThinkPad, Dell notebook, Intel NUC,
%%% Supermicro, Inspur, AWS EBS, Google Compute Engine, Intel TDX
%%% CCEL, QEMU/OVMF, fwupd test fixtures, tpm2-tools canonical set)
%%% plus deliberate edge cases (empty, bogus, truncated, duplicate
%%% separator). Each file must:
%%%   * parse without raising,
%%%   * produce a map keyed by 1-based sequence numbers,
%%%   * every event must have pcr + event-type-code + digests +
%%%     event-data keys,
%%%   * when fed through `decode_events/1', every decoder must not
%%%     raise.
%%%
%%% We don't assert specific field values per fixture (that would
%%% require a known-good oracle); we assert structural invariants.
%%% Regression value: if a decoder ever crashes on a real vector
%%% that the Erlang parser has seen before, the test fails.

real_fixture_corpus_parses_without_crashes_test_() ->
    Dir = fixtures_dir(),
    Files =
        case file:list_dir(Dir) of
            {ok, L} ->
                KeepExts = [".bin", ""],
                [filename:join(Dir, F)
                 || F <- L,
                    lists:member(filename:extension(F), KeepExts)];
            _ -> []
        end,
    [{"fixture " ++ filename:basename(F),
      fun() -> check_fixture(F) end} || F <- Files].

fixtures_dir() ->
    case code:priv_dir(hb) of
        {error, _} ->
            filename:join([filename:dirname(
                filename:dirname(code:which(?MODULE))),
                          "priv", "tpm-interpret", "fixtures"]);
        P -> filename:join(P, "tpm-interpret/fixtures")
    end.

check_fixture(Path) ->
    {ok, Bin} = file:read_file(Path),
    case byte_size(Bin) of
        0 ->
            %% Empty file -> parse returns empty map OR `#{}'.
            Result = parse(Bin),
            ?assert(is_map(Result)),
            ?assertEqual(0, maps:size(Result));
        _ ->
            try
                Parsed = parse(Bin),
                ?assert(is_map(Parsed)),
                %% Every entry: has the core keys.
                maps:foreach(
                    fun(K, V) when is_binary(K), is_map(V) ->
                        %% "error" entries from truncated records are
                        %% allowed; they carry an `error' key.
                        case maps:is_key(<<"error">>, V) of
                            true -> ok;
                            false ->
                                check_event_shape(V)
                        end;
                       (_, _) -> ok
                    end,
                    Parsed),
                %% `decode_events/1' enriches every non-error event
                %% with a `parsed' submap. Must not raise for any
                %% event type we've ever seen in the wild.
                Decoded = decode_events(Parsed),
                ?assert(is_map(Decoded))
            catch Class:Reason:Stack ->
                erlang:error({fixture_failed,
                              [{path, Path},
                               {class, Class},
                               {reason, Reason},
                               {stack, Stack}]})
            end
    end.

check_event_shape(Ev) ->
    ?assert(maps:is_key(<<"seq">>, Ev)),
    ?assert(maps:is_key(<<"pcr">>, Ev)),
    ?assert(maps:is_key(<<"event-type-code">>, Ev)),
    ?assert(maps:is_key(<<"digests">>, Ev)),
    ?assert(maps:is_key(<<"event-data">>, Ev)),
    ?assert(is_integer(maps:get(<<"pcr">>, Ev))),
    ?assert(is_integer(maps:get(<<"seq">>, Ev))),
    ?assert(is_integer(maps:get(<<"event-type-code">>, Ev))),
    ?assert(is_map(maps:get(<<"digests">>, Ev))),
    ?assert(is_binary(maps:get(<<"event-data">>, Ev))).

%%%---- Integration tests: assert specific content from real fixtures

%% tpm2tools-bootorder.bin -- this fixture from the tpm2-tools test
%% suite contains EV_EFI_VARIABLE_BOOT events for BootOrder + Boot0000
%% + Boot0001 + Boot0002. Verify our decoder produces a BootOrder
%% list + at least one Boot#### with load-option-description.
integration_tpm2tools_bootorder_test() ->
    case fixture_exists("tpm2tools-bootorder.bin") of
        false -> ok;
        Path ->
            {ok, Raw} = file:read_file(Path),
            Events = decode_events(parse(Raw)),
            BootEvs = [E || {_, E} <- maps:to_list(Events),
                            is_map(E),
                            16#80000002 =:= maps:get(<<"event-type-code">>,
                                                      E, 0)],
            %% At least one EV_EFI_VARIABLE_BOOT event must exist.
            ?assert(length(BootEvs) >= 1),
            %% Expect at least one to be BootOrder with a boot-order
            %% list in its parsed.semantic.
            BootOrders = [
                maps:get(<<"semantic">>, maps:get(<<"parsed">>, E, #{}), #{})
                || E <- BootEvs,
                   <<"BootOrder">> =:=
                       maps:get(<<"variable-name">>,
                                 maps:get(<<"parsed">>, E, #{}), <<>>)
            ],
            ?assert(length(BootOrders) >= 1),
            [FirstBO | _] = BootOrders,
            Order = maps:get(<<"boot-order">>, FirstBO, []),
            ?assert(length(Order) >= 1),
            %% The boot order entries are of the form "Boot####".
            lists:foreach(
                fun(B) -> ?assertMatch(<<"Boot", _/binary>>, B) end,
                Order)
    end.

%% tpm2tools-uefivar.bin -- should contain at least one EV_EFI_
%% VARIABLE_DRIVER_CONFIG event; likely SecureBoot.
integration_tpm2tools_uefivar_test() ->
    case fixture_exists("tpm2tools-uefivar.bin") of
        false -> ok;
        Path ->
            {ok, Raw} = file:read_file(Path),
            Events = decode_events(parse(Raw)),
            VarEvs = [E || {_, E} <- maps:to_list(Events),
                           is_map(E),
                           16#80000001 =:= maps:get(<<"event-type-code">>,
                                                     E, 0)],
            ?assert(length(VarEvs) >= 1),
            %% Each event must have parsed.variable-name present.
            lists:foreach(
                fun(E) ->
                    P = maps:get(<<"parsed">>, E, #{}),
                    ?assert(maps:is_key(<<"variable-name">>, P))
                end, VarEvs)
    end.

%% fedora37-sd-boot.bin -- Fedora 37 systemd-boot. Expected to
%% contain EV_IPL events with systemd-stub keys on PCR 11/12.
integration_fedora_sdboot_test() ->
    case fixture_exists("fedora37-sd-boot.bin") of
        false -> ok;
        Path ->
            {ok, Raw} = file:read_file(Path),
            Events = decode_events(parse(Raw)),
            %% Find all EV_IPL events.
            Ipls = [E || {_, E} <- maps:to_list(Events),
                         is_map(E),
                         16#D =:= maps:get(<<"event-type-code">>,
                                           E, 0)],
            %% Fedora 37 sd-boot should have at least one EV_IPL.
            ?assert(length(Ipls) >= 1)
    end.

%% Lenovo ThinkPad P51 fixture -- expected to emit an EV_S_CRTM_
%% VERSION on PCR 0 with a "N1M"-prefix UTF-16LE string (Lenovo
%% ThinkPad P51 CRTM convention per firmware-versions/
%% lenovo-thinkpad.json).
integration_lenovo_thinkpad_crtm_test() ->
    case fixture_exists("lenovo-thinkpad-p51.bin") of
        false -> ok;
        Path ->
            {ok, Raw} = file:read_file(Path),
            Events = decode_events(parse(Raw)),
            %% Find EV_S_CRTM_VERSION events (code 0x08).
            CrtmEvs = [E || {_, E} <- maps:to_list(Events),
                            is_map(E),
                            16#8 =:= maps:get(<<"event-type-code">>,
                                              E, 0)],
            case CrtmEvs of
                [] ->
                    %% Some legacy logs don't have it; skip the
                    %% content assertion but note.
                    ok;
                [E | _] ->
                    P = maps:get(<<"parsed">>, E, #{}),
                    V = maps:get(<<"crtm-version">>, P, <<>>),
                    ?assert(is_binary(V)),
                    ?assert(byte_size(V) > 0)
            end
    end.

%% Canonical Ubuntu fixture -- should produce a rich event log
%% with many standard UEFI events.
integration_canonical_ubuntu_test() ->
    case fixture_exists("canonical-ubuntu.bin") of
        false -> ok;
        Path ->
            {ok, Raw} = file:read_file(Path),
            Events = decode_events(parse(Raw)),
            ?assert(maps:size(Events) > 10),
            %% At least one EV_EFI_BOOT_SERVICES_APPLICATION event
            %% (the bootloader PE image).
            Apps = [E || {_, E} <- maps:to_list(Events),
                         is_map(E),
                         16#80000003 =:= maps:get(<<"event-type-code">>,
                                                   E, 0)],
            ?assert(length(Apps) >= 1),
            %% Every image-load event must carry a device-path-text
            %% field from our walker.
            lists:foreach(
                fun(E) ->
                    P = maps:get(<<"parsed">>, E, #{}),
                    ?assert(maps:is_key(<<"device-path-text">>, P))
                end, Apps)
    end.

%% Helper: resolves a fixture path, returning false if missing so
%% the integration tests skip gracefully on minimal checkouts.
fixture_exists(FileName) ->
    Path = filename:join(fixtures_dir(), FileName),
    case filelib:is_file(Path) of
        true -> Path;
        false -> false
    end.

%% lenovo-thinkpad-p51.bin has 10 EV_EVENT_TAG events per scan;
%% assert each decoded event exposes a tag-id + tag-id-name.
integration_thinkpad_p51_tagged_events_test() ->
    case fixture_exists("lenovo-thinkpad-p51.bin") of
        false -> ok;
        Path ->
            {ok, Raw} = file:read_file(Path),
            Events = decode_events(parse(Raw)),
            Tags = [E || {_, E} <- maps:to_list(Events),
                         is_map(E),
                         16#6 =:= maps:get(<<"event-type-code">>,
                                           E, 0)],
            ?assert(length(Tags) >= 1),
            lists:foreach(
                fun(E) ->
                    P = maps:get(<<"parsed">>, E, #{}),
                    ?assert(maps:is_key(<<"tag-id">>, P)),
                    ?assert(maps:is_key(<<"tag-id-name">>, P))
                end, Tags)
    end.

%% canonical-ubuntu.bin has EV_EFI_HANDOFF_TABLES v1 events;
%% assert each decoded event has `tables` with named vendor GUIDs.
integration_canonical_ubuntu_handoff_tables_test() ->
    case fixture_exists("canonical-ubuntu.bin") of
        false -> ok;
        Path ->
            {ok, Raw} = file:read_file(Path),
            Events = decode_events(parse(Raw)),
            Handoffs = [E || {_, E} <- maps:to_list(Events),
                             is_map(E),
                             16#80000009 =:= maps:get(<<"event-type-code">>,
                                                       E, 0)],
            ?assert(length(Handoffs) >= 1),
            lists:foreach(
                fun(E) ->
                    P = maps:get(<<"parsed">>, E, #{}),
                    ?assert(maps:is_key(<<"tables">>, P)),
                    ?assert(maps:is_key(<<"number-of-tables">>, P)),
                    Tables = maps:get(<<"tables">>, P),
                    lists:foreach(
                        fun(T) ->
                            ?assert(maps:is_key(<<"vendor-guid">>, T)),
                            ?assert(maps:is_key(<<"vendor-guid-name">>, T)),
                            ?assert(maps:is_key(
                                <<"vendor-table-address">>, T))
                        end, Tables)
                end, Handoffs)
    end.

%% dell-notebook-wbcl.bin has 3 EV_NONHOST_CODE/CONFIG/INFO events;
%% assert each decoded event has nonhost-kind + sha256 pin.
integration_dell_nonhost_events_test() ->
    case fixture_exists("dell-notebook-wbcl.bin") of
        false -> ok;
        Path ->
            {ok, Raw} = file:read_file(Path),
            Events = decode_events(parse(Raw)),
            Nonhost = [E || {_, E} <- maps:to_list(Events),
                            is_map(E),
                            begin
                                C = maps:get(<<"event-type-code">>, E, 0),
                                C =:= 16#F orelse C =:= 16#10
                                    orelse C =:= 16#11
                            end],
            case Nonhost of
                [] -> ok;
                _ ->
                    lists:foreach(
                        fun(E) ->
                            P = maps:get(<<"parsed">>, E, #{}),
                            ?assert(maps:is_key(<<"nonhost-kind">>, P)),
                            ?assert(maps:is_key(<<"sha256">>, P))
                        end, Nonhost)
            end
    end.

%% tpm2tools-moklisttrusted.bin has a MokListTrusted authority event.
%% Verify our shim-aware decoder picks it out.
integration_tpm2tools_moklisttrusted_test() ->
    case fixture_exists("tpm2tools-moklisttrusted.bin") of
        false -> ok;
        Path ->
            {ok, Raw} = file:read_file(Path),
            Events = decode_events(parse(Raw)),
            AuthEvs = [E || {_, E} <- maps:to_list(Events),
                            is_map(E),
                            16#800000E0 =:= maps:get(<<"event-type-code">>,
                                                      E, 0)],
            ?assert(length(AuthEvs) >= 1),
            %% At least one should be for a shim/MokList-style
            %% variable. Find any non-empty variable-name.
            NonEmpty = [E || E <- AuthEvs,
                             byte_size(maps:get(<<"variable-name">>,
                                 maps:get(<<"parsed">>, E, #{}),
                                 <<>>)) > 0],
            ?assert(length(NonEmpty) >= 1)
    end.

%% intel-tdx-ccel.bin has EV_EFI_HANDOFF_TABLES2 events. Verify
%% our v2 decoder extracts the table-description.
integration_intel_tdx_handoff_v2_test() ->
    case fixture_exists("intel-tdx-ccel.bin") of
        false -> ok;
        Path ->
            {ok, Raw} = file:read_file(Path),
            Events = decode_events(parse(Raw)),
            V2Evs = [E || {_, E} <- maps:to_list(Events),
                          is_map(E),
                          16#8000000B =:= maps:get(<<"event-type-code">>,
                                                    E, 0)],
            case V2Evs of
                [] -> ok;
                _ ->
                    lists:foreach(
                        fun(E) ->
                            P = maps:get(<<"parsed">>, E, #{}),
                            ?assert(maps:is_key(
                                <<"table-description">>, P))
                        end, V2Evs)
            end
    end.

-endif.
