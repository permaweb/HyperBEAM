%%% @doc TCG event log parser — pure Erlang, no external deps.
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
%%%       digest     sizeof(hashAlg)  — see hash_alg_size/1
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
%%%     <<"event_type_code">> => integer (raw TCG code, e.g. 2147483649)
%%%     <<"event_type">>      => binary (human name, e.g.
%%%                              <<"EV_EFI_VARIABLE_DRIVER_CONFIG">>;
%%%                              looked up from priv/event-types.json)
%%%     <<"digests">>         => #{ <<"sha256">> => <<32 bytes>>,
%%%                                 <<"sha1">>   => <<20 bytes>>, ... }
%%%     <<"event_data">>      => raw binary
%%%   }
%%%
%%% Errors: this module never crashes on malformed input. If the log
%%% is truncated or a record can't be parsed, parse/1 returns the
%%% events it was able to decode plus a `#{error => …}' map at the
%%% end, so callers see "this many events were fine, then something
%%% went wrong."
-module(dev_tpm_tcg).
-export([parse/1, parse/2, event_type_name/1, event_type_name/2,
         decode_event/1, decode_events/1]).

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
            %% Log isn't crypto-agile — try legacy all-SHA1. Rare in
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
%%% wrong shape for its type) produces `parsed => #{error => …}'
%%% rather than a crash or a misleading value.
%%%
%%% `decode_events/1' maps decode_event across the map form
%%% produced by `parse/1,2' — gives callers a one-shot "parse +
%%% decode" pipeline.

-spec decode_events(map()) -> map().
decode_events(Events) when is_map(Events) ->
    maps:map(fun(_K, V) when is_map(V) -> decode_event(V);
                (_K, V) -> V
             end, Events);
decode_events(Other) -> Other.

-spec decode_event(map()) -> map().
decode_event(Event) when is_map(Event) ->
    case maps:get(<<"event_type_code">>, Event, undefined) of
        undefined -> Event;
        Code -> Event#{<<"parsed">> => do_decode(Code, Event)}
    end;
decode_event(E) -> E.

%%%---- M4: Secure Boot variables + firmware CRTM + POST code -----------

%% EV_EFI_VARIABLE_DRIVER_CONFIG (0x80000001)
%% EV_EFI_VARIABLE_BOOT          (0x80000002)
%% EV_EFI_VARIABLE_AUTHORITY     (0x800000E0)
do_decode(16#80000001, Event) -> decode_uefi_variable(Event);
do_decode(16#80000002, Event) -> decode_uefi_variable(Event);
do_decode(16#800000E0, Event) -> decode_uefi_variable(Event);

%% EV_S_CRTM_VERSION (0x08) — firmware/CRTM version string.
%% Typically UTF-16LE; occasionally ASCII. Best-effort decode.
do_decode(16#8, Event) -> decode_crtm_version(Event);

%% EV_POST_CODE (0x01) — firmware POST code; usually short ASCII
%% or manufacturer-defined bytes.
do_decode(16#1, Event) -> decode_post_code(Event);

%%%---- M5: bootloader + UKI + systemd-stub -----------------------------

%% EV_EFI_BOOT_SERVICES_APPLICATION (0x80000003)
%% EV_EFI_BOOT_SERVICES_DRIVER      (0x80000004)
%% EV_EFI_RUNTIME_SERVICES_DRIVER   (0x80000005)
do_decode(16#80000003, Event) -> decode_uefi_image_load(Event);
do_decode(16#80000004, Event) -> decode_uefi_image_load(Event);
do_decode(16#80000005, Event) -> decode_uefi_image_load(Event);

%% EV_IPL (0x0D) — generic OS-loader event. systemd-stub encodes
%% "key=value" ASCII on PCR 11/12/13; other users encode opaque
%% data. Try key=value, fall back to raw.
do_decode(16#D, Event) -> decode_ev_ipl(Event);

%% EV_EFI_PLATFORM_FIRMWARE_BLOB  (0x80000008)
%% EV_EFI_PLATFORM_FIRMWARE_BLOB2 (0x8000000A) — with blob description
do_decode(16#80000008, Event) -> decode_firmware_blob(Event);
do_decode(16#8000000A, Event) -> decode_firmware_blob2(Event);

%%%---- M6: remaining TCG codes -----------------------------------------

%% EV_CPU_MICROCODE (0x09) — microcode update header.
do_decode(16#9, Event) -> decode_cpu_microcode(Event);

%% EV_SEPARATOR (0x04) — typically 0x00000000 (normal) or
%% 0xFFFFFFFF (firmware reports an error).
do_decode(16#4, Event) -> decode_separator(Event);

%% EV_ACTION (0x05) + EV_EFI_ACTION (0x80000007) — ASCII action markers.
do_decode(16#5, Event) -> decode_ascii_action(Event);
do_decode(16#80000007, Event) -> decode_ascii_action(Event);

%% EV_EFI_HCRTM_EVENT (0x80000010) — fixed "HCRTM" ASCII.
do_decode(16#80000010, Event) -> decode_ascii_action(Event);

%% EV_NO_ACTION (0x03) — first record carries SpecID; others may
%% carry StartupLocality or similar markers.
do_decode(16#3, Event) -> decode_no_action(Event);

%% EV_OMIT_BOOT_DEVICE_EVENTS (0x12) — ASCII.
do_decode(16#12, Event) -> decode_ascii_action(Event);

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
decode_uefi_variable(#{<<"event_data">> := Data}) ->
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
                        <<"variable_guid">> => fmt_efi_guid(GuidBin),
                        <<"variable_name">> => Name,
                        <<"variable_data">> => VarData,
                        <<"variable_data_length">> => DataLen,
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

%% Extract the one thing a policy engine actually cares about per
%% UEFI variable: for `SecureBoot' the single enabled/disabled
%% byte; for `PK`/`KEK`/`db`/`dbx` the signature-list summary.
decode_uefi_variable_semantic(<<"SecureBoot">>, <<1>>) ->
    #{<<"secure_boot_enabled">> => true};
decode_uefi_variable_semantic(<<"SecureBoot">>, <<0>>) ->
    #{<<"secure_boot_enabled">> => false};
decode_uefi_variable_semantic(<<"SecureBoot">>, _) ->
    #{<<"secure_boot_enabled">> => <<"malformed">>};
decode_uefi_variable_semantic(<<"SetupMode">>, <<B:8>>) ->
    #{<<"setup_mode">> => B == 1};
decode_uefi_variable_semantic(<<"AuditMode">>, <<B:8>>) ->
    #{<<"audit_mode">> => B == 1};
decode_uefi_variable_semantic(<<"DeployedMode">>, <<B:8>>) ->
    #{<<"deployed_mode">> => B == 1};
decode_uefi_variable_semantic(Name, Data)
  when Name =:= <<"PK">>; Name =:= <<"KEK">>;
       Name =:= <<"db">>; Name =:= <<"dbx">> ->
    #{<<"signature_list">> => summarise_signature_list(Data)};
decode_uefi_variable_semantic(_, _) -> #{}.

%% EFI_SIGNATURE_LIST header:
%%   signatureType     EFI_GUID (16B)
%%   signatureListSize uint32 LE
%%   signatureHeaderSize uint32 LE
%%   signatureSize      uint32 LE
%%   signatureHeader   [signatureHeaderSize]
%%   signatures         [...] — each is {owner: EFI_GUID, data: ...}
%%
%% We don't try to decode the cert bytes fully; we just report
%% per-list {type, n_entries, total_bytes}.
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
          Sigs:SignaturesBytes/binary,
          Tail/binary>> when SigSize > 0 ->
            N = SignaturesBytes div SigSize,
            Entry = #{
                <<"type_guid">>   => fmt_efi_guid(GuidBin),
                <<"entry_count">> => N,
                <<"entry_size">>  => SigSize
            },
            summarise_signature_list(Tail, [Entry | Acc]);
        _ ->
            lists:reverse([#{<<"error">> =>
                                 <<"malformed signature list">>} | Acc])
    end;
summarise_signature_list(_, Acc) ->
    lists:reverse([#{<<"error">> =>
                         <<"truncated signature list">>} | Acc]).

%% EV_S_CRTM_VERSION — event data is the version string.
%% Heuristic: if it's an even length and looks like UTF-16LE
%% (every odd byte is 0x00 for ASCII range), decode as UTF-16LE.
%% Otherwise return as ASCII best-effort.
decode_crtm_version(#{<<"event_data">> := Data}) ->
    Decoded = case looks_like_utf16le(Data) of
        true  -> utf16le_to_utf8(Data);
        false -> ascii_trim(Data)
    end,
    #{<<"crtm_version">> => Decoded};
decode_crtm_version(_) -> #{}.

decode_post_code(#{<<"event_data">> := Data}) ->
    case ascii_only(Data) of
        true  -> #{<<"post_code">> => ascii_trim(Data)};
        false -> #{<<"post_code_bytes">> => Data}
    end;
decode_post_code(_) -> #{}.

%% UEFI_IMAGE_LOAD_EVENT:
%%   imageLocationInMemory  uint64 LE
%%   imageLengthInMemory    uint64 LE
%%   imageLinkTimeAddress   uint64 LE
%%   lengthOfDevicePath     uint64 LE
%%   devicePath             [lengthOfDevicePath] EFI_DEVICE_PATH_PROTOCOL
decode_uefi_image_load(#{<<"event_data">> := Data}) ->
    case Data of
        <<LocInMem:64/unsigned-little,
          LenInMem:64/unsigned-little,
          LinkAddr:64/unsigned-little,
          DpLen:64/unsigned-little,
          DevicePath:DpLen/binary,
          _Tail/binary>> ->
            #{
                <<"image_location_in_memory">> => LocInMem,
                <<"image_length_in_memory">>   => LenInMem,
                <<"image_link_time_address">>  => LinkAddr,
                <<"device_path_length">>       => DpLen,
                <<"device_path">>              => DevicePath
            };
        _ ->
            #{<<"error">> => <<"malformed UEFI_IMAGE_LOAD_EVENT">>}
    end;
decode_uefi_image_load(_) -> #{}.

%% EV_IPL — systemd-stub encodes "key=value\0" ASCII on PCR
%% 11/12/13 for UKI measurements (kernel_cmdline, kernel,
%% initrd, etc.). Other users encode opaque data.
decode_ev_ipl(#{<<"event_data">> := Data}) ->
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
                    #{
                        <<"key">>   => Key,
                        <<"value">> => Value,
                        <<"format">> => <<"key_value_ascii">>
                    };
                _ ->
                    #{<<"text">> => TrimmedData,
                      <<"format">> => <<"ascii">>}
            end;
        false ->
            #{<<"format">> => <<"opaque">>,
              <<"length">> => byte_size(Data)}
    end;
decode_ev_ipl(_) -> #{}.

%% UEFI_PLATFORM_FIRMWARE_BLOB:
%%   blobBase   uint64 LE
%%   blobLength uint64 LE
decode_firmware_blob(#{<<"event_data">> := Data}) ->
    case Data of
        <<Base:64/unsigned-little, Len:64/unsigned-little, _Tail/binary>> ->
            #{
                <<"blob_physical_address">> => Base,
                <<"blob_length">>           => Len
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
decode_firmware_blob2(#{<<"event_data">> := Data}) ->
    case Data of
        <<DescSize:8, Rest0/binary>> ->
            case Rest0 of
                <<Desc:DescSize/binary, Base:64/unsigned-little,
                  Len:64/unsigned-little, _Tail/binary>> ->
                    #{
                        <<"blob_description">>      => Desc,
                        <<"blob_physical_address">> => Base,
                        <<"blob_length">>           => Len
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
%%   …reserved 12 bytes
%%   data…
%%
%% AMD microcode layout differs — we return what we can read
%% with a `format' tag so callers can try vendor-specific decoding.
decode_cpu_microcode(#{<<"event_data">> := Data}) ->
    case Data of
        <<HV:32/little, UR:32/little, Date:32/little,
          ProcSig:32/little, Checksum:32/little, LoaderRev:32/little,
          ProcFlags:32/little, _/binary>> ->
            #{
                <<"format">>             => <<"intel_or_compatible">>,
                <<"header_version">>     => HV,
                <<"update_revision">>    => UR,
                <<"date_bcd">>           => Date,
                <<"processor_signature">> => ProcSig,
                <<"checksum">>           => Checksum,
                <<"loader_revision">>    => LoaderRev,
                <<"processor_flags">>    => ProcFlags
            };
        _ ->
            #{<<"error">> => <<"EV_CPU_MICROCODE too short for header">>}
    end;
decode_cpu_microcode(_) -> #{}.

decode_separator(#{<<"event_data">> := <<16#FF, 16#FF, 16#FF, 16#FF>>}) ->
    #{<<"separator">> => <<"firmware_error">>};
decode_separator(#{<<"event_data">> := <<0, 0, 0, 0>>}) ->
    #{<<"separator">> => <<"normal">>};
decode_separator(#{<<"event_data">> := Data}) ->
    #{<<"separator">> => <<"other">>,
      <<"bytes">> => Data};
decode_separator(_) -> #{}.

decode_ascii_action(#{<<"event_data">> := Data}) ->
    case ascii_only(Data) of
        true -> #{<<"action">> => ascii_trim(Data)};
        false -> #{<<"action_bytes">> => Data}
    end;
decode_ascii_action(_) -> #{}.

%% EV_NO_ACTION — first record carries TCG_EfiSpecIdEvent; others
%% may carry StartupLocality ("StartupLocality" + 1 byte) or
%% other markers.
decode_no_action(#{<<"event_data">> := <<"Spec ID Event03", 0, _/binary>>
                   = Data}) ->
    case parse_spec_id(Data) of
        {ok, AlgList} ->
            #{<<"spec_id">> => <<"Event03">>,
              <<"algorithms">> =>
                [#{<<"hash_alg_id">> => AlgId,
                   <<"hash_alg_name">> => hash_alg_name(AlgId),
                   <<"digest_size">> => Sz}
                 || {AlgId, Sz} <- AlgList]};
        _ -> #{<<"error">> => <<"malformed SpecID">>}
    end;
decode_no_action(#{<<"event_data">> := <<"StartupLocality", 0, Locality:8,
                                           _/binary>>}) ->
    #{<<"marker">> => <<"StartupLocality">>,
      <<"locality">> => Locality};
decode_no_action(#{<<"event_data">> := Data}) ->
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
    %% byproduct of UTF-16LE → UTF-8 conversion when the source
    %% had a trailing null terminator).
    strip_trailing_nulls(Bin);
ascii_trim(Other) -> Other.

strip_trailing_nulls(<<>>) -> <<>>;
strip_trailing_nulls(Bin) ->
    case binary:last(Bin) of
        0 -> strip_trailing_nulls(binary:part(Bin, 0, byte_size(Bin) - 1));
        _ -> Bin
    end.

fmt_efi_guid(<<A:32/little, B:16/little, C:16/little, D:8/binary>>) ->
    iolist_to_binary(
        io_lib:format("~8.16.0B-~4.16.0B-~4.16.0B-~s",
                      [A, B, C, fmt_guid_tail(D)])).

fmt_guid_tail(<<D0:8, D1:8, D2:8, D3:8, D4:8, D5:8, D6:8, D7:8>>) ->
    io_lib:format("~2.16.0B~2.16.0B-~2.16.0B~2.16.0B~2.16.0B~2.16.0B"
                  "~2.16.0B~2.16.0B",
                  [D0, D1, D2, D3, D4, D5, D6, D7]).

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
                <<"event_type_code">> => EventType,
                <<"digests">>         => #{<<"sha1">> => Sha1},
                <<"event_data">>      => Event
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
                        <<"event_type_code">> => EventType,
                        <<"digests">>         => Digests,
                        <<"event_data">>      => Event
                    },
                    parse_crypto_agile(Rest2, AlgList, Seq + 1, [Ev | Acc]);
                _ ->
                    %% Truncated — return what we have.
                    TruncErr = #{
                        <<"error">> => <<"truncated event (bad eventSize)">>,
                        <<"at_seq">> => Seq
                    },
                    {lists:reverse([TruncErr | Acc]), <<>>}
            end;
        error ->
            TruncErr = #{
                <<"error">> => <<"truncated digests">>,
                <<"at_seq">> => Seq
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

%% TCG algorithm registry (partial — the common ones).
hash_alg_size(16#04) -> 20;   %% TPM_ALG_SHA1
hash_alg_size(16#0B) -> 32;   %% TPM_ALG_SHA256
hash_alg_size(16#0C) -> 48;   %% TPM_ALG_SHA384
hash_alg_size(16#0D) -> 64;   %% TPM_ALG_SHA512
hash_alg_size(16#12) -> 32;   %% TPM_ALG_SM3_256
hash_alg_size(16#15) -> 32;   %% TPM_ALG_SHA3_256
hash_alg_size(16#16) -> 48;   %% TPM_ALG_SHA3_384
hash_alg_size(16#17) -> 64;   %% TPM_ALG_SHA3_512
hash_alg_size(_)     -> 0.    %% unknown → parser will fail record

hash_alg_name(16#04) -> <<"sha1">>;
hash_alg_name(16#0B) -> <<"sha256">>;
hash_alg_name(16#0C) -> <<"sha384">>;
hash_alg_name(16#0D) -> <<"sha512">>;
hash_alg_name(16#12) -> <<"sm3_256">>;
hash_alg_name(16#15) -> <<"sha3_256">>;
hash_alg_name(16#16) -> <<"sha3_384">>;
hash_alg_name(16#17) -> <<"sha3_512">>;
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
        <<"event_type_code">> => EventType,
        <<"digests">>         => #{<<"sha1">> => Sha1},
        <<"event_data">>      => Event
    },
    parse_all_legacy(Rest, Seq + 1, [Ev | Acc]);
parse_all_legacy(_Bin, _Seq, _Acc) ->
    %% Failed partway — signal caller to report parse error.
    error.

%%%============================================================================
%%% Naming + indexing
%%%============================================================================

attach_type_name(Ev = #{<<"event_type_code">> := Code}, Registry) ->
    Ev#{<<"event_type">> => lookup_name(Code, Registry)};
attach_type_name(Ev, _) -> Ev.

lookup_name(Code, Registry) ->
    case maps:get(integer_to_binary(Code), Registry, undefined) of
        #{<<"name">> := Name} when is_binary(Name) -> Name;
        _ ->
            %% Built-in fallback for the common core codes —
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
%% (AO-Core natural collection form — individual events
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
        #{<<"event_types">> := R} when is_map(R) -> R;
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
    %%   variableName GUID (16B, using zeros — content doesn't matter here)
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
    ?assertEqual(3, maps:get(<<"event_type_code">>, E1)),
    ?assertEqual(<<"EV_NO_ACTION">>, maps:get(<<"event_type">>, E1)),
    %% Only SHA-1 on the first record (legacy shape).
    D = maps:get(<<"digests">>, E1),
    ?assert(maps:is_key(<<"sha1">>, D)).

second_record_has_both_digest_algs_test() ->
    Events = parse(build_fixture()),
    E2 = maps:get(<<"2">>, Events),
    ?assertEqual(<<"EV_S_CRTM_VERSION">>, maps:get(<<"event_type">>, E2)),
    D = maps:get(<<"digests">>, E2),
    ?assert(maps:is_key(<<"sha1">>, D)),
    ?assert(maps:is_key(<<"sha256">>, D)),
    ?assertEqual(20, byte_size(maps:get(<<"sha1">>, D))),
    ?assertEqual(32, byte_size(maps:get(<<"sha256">>, D))),
    %% Event data is the raw ASCII string.
    ?assertEqual(<<"TEST FW v1">>, maps:get(<<"event_data">>, E2)).

secure_boot_variable_record_parses_test() ->
    Events = parse(build_fixture()),
    E3 = maps:get(<<"3">>, Events),
    ?assertEqual(7, maps:get(<<"pcr">>, E3)),
    ?assertEqual(16#80000001,
                 maps:get(<<"event_type_code">>, E3)),
    ?assertEqual(<<"EV_EFI_VARIABLE_DRIVER_CONFIG">>,
                 maps:get(<<"event_type">>, E3)),
    %% Event data begins with the 16-byte GUID, length fields, then
    %% the UTF-16LE "SecureBoot" string, then a single 0x01 byte.
    Data = maps:get(<<"event_data">>, E3),
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
    %% Cut off mid-way through record 2's digests — parser should
    %% return the first record plus an error entry, not crash.
    Truncated = binary:part(Full, 0, byte_size(Full) - 40),
    Events = parse(Truncated),
    ?assert(maps:size(Events) >= 1),
    ok.

parse_empty_input_test() ->
    %% Empty binary → empty map (no spec-id, no legacy — just nothing).
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
    Ev = #{<<"event_type_code">> => 16#80000001,
           <<"event_data">> => Data},
    Parsed = (decode_event(Ev))#{<<"parsed">> => _P = maps:get(<<"parsed">>,
                                          decode_event(Ev), #{})},
    P = maps:get(<<"parsed">>, Parsed),
    ?assertEqual(<<"SecureBoot">>, maps:get(<<"variable_name">>, P)),
    ?assertEqual(#{<<"secure_boot_enabled">> => true},
                 maps:get(<<"semantic">>, P)).

decode_secure_boot_variable_disabled_test() ->
    Data = build_uefi_variable(<<0:128>>, <<"SecureBoot">>, <<0>>),
    Ev = #{<<"event_type_code">> => 16#80000001,
           <<"event_data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(#{<<"secure_boot_enabled">> => false},
                 maps:get(<<"semantic">>, P)).

decode_crtm_version_utf16le_test() ->
    Utf16 = unicode:characters_to_binary(<<"BIOS 1.23">>, utf8,
                                           {utf16, little}),
    Ev = #{<<"event_type_code">> => 16#8, <<"event_data">> => Utf16},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(<<"BIOS 1.23">>, maps:get(<<"crtm_version">>, P)).

decode_crtm_version_ascii_test() ->
    Ev = #{<<"event_type_code">> => 16#8,
           <<"event_data">> => <<"AMI v5.19">>},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(<<"AMI v5.19">>, maps:get(<<"crtm_version">>, P)).

decode_separator_normal_vs_error_test() ->
    EvNormal = #{<<"event_type_code">> => 16#4,
                 <<"event_data">> => <<0,0,0,0>>},
    EvError = #{<<"event_type_code">> => 16#4,
                <<"event_data">> => <<16#FF,16#FF,16#FF,16#FF>>},
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
    Ev = #{<<"event_type_code">> => 16#3, <<"event_data">> => SpecId},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(<<"Event03">>, maps:get(<<"spec_id">>, P)),
    ?assertEqual(2, length(maps:get(<<"algorithms">>, P))).

decode_uefi_image_load_test() ->
    DevicePath = <<16#01,16#02,16#03,16#04,16#05>>,  %% arbitrary bytes
    DpLen = byte_size(DevicePath),
    Data = <<16#1000:64/little, 16#20000:64/little,
             16#FFFFFFFF00000000:64/little, DpLen:64/little,
             DevicePath/binary>>,
    Ev = #{<<"event_type_code">> => 16#80000003,
           <<"event_data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(16#1000, maps:get(<<"image_location_in_memory">>, P)),
    ?assertEqual(16#20000, maps:get(<<"image_length_in_memory">>, P)),
    ?assertEqual(DpLen, maps:get(<<"device_path_length">>, P)),
    ?assertEqual(DevicePath, maps:get(<<"device_path">>, P)).

decode_ev_ipl_systemd_stub_kernel_cmdline_test() ->
    Ev = #{<<"event_type_code">> => 16#D,
           <<"event_data">> => <<"kernel_cmdline=ro quiet",0>>},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(<<"kernel_cmdline">>, maps:get(<<"key">>, P)),
    ?assertEqual(<<"ro quiet">>, maps:get(<<"value">>, P)),
    ?assertEqual(<<"key_value_ascii">>, maps:get(<<"format">>, P)).

decode_ev_ipl_opaque_test() ->
    Ev = #{<<"event_type_code">> => 16#D,
           <<"event_data">> => <<0,1,2,3,4,5>>},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    %% Not key=value ASCII → format=opaque
    ?assertEqual(<<"opaque">>, maps:get(<<"format">>, P)),
    ?assertEqual(6, maps:get(<<"length">>, P)).

decode_firmware_blob_test() ->
    Ev = #{<<"event_type_code">> => 16#80000008,
           <<"event_data">> => <<16#FF000000:64/little,
                                 16#100000:64/little>>},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(16#FF000000, maps:get(<<"blob_physical_address">>, P)),
    ?assertEqual(16#100000, maps:get(<<"blob_length">>, P)).

decode_firmware_blob2_with_description_test() ->
    Desc = <<"main_fw">>,
    DescLen = byte_size(Desc),
    Ev = #{<<"event_type_code">> => 16#8000000A,
           <<"event_data">> =>
               <<DescLen:8, Desc/binary,
                 16#FF000000:64/little,
                 16#100000:64/little>>},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(<<"main_fw">>, maps:get(<<"blob_description">>, P)).

decode_cpu_microcode_header_test() ->
    %% 28-byte header prefix is enough for our parser.
    Data = <<1:32/little, 16#12345:32/little, 16#20240101:32/little,
             16#806EA:32/little, 0:32/little, 1:32/little,
             1:32/little, 100:32/little, 200:32/little,
             0:96>>,
    Ev = #{<<"event_type_code">> => 16#9,
           <<"event_data">> => Data},
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertEqual(<<"intel_or_compatible">>, maps:get(<<"format">>, P)),
    ?assertEqual(16#12345, maps:get(<<"update_revision">>, P)),
    ?assertEqual(16#20240101, maps:get(<<"date_bcd">>, P)).

decode_malformed_uefi_variable_returns_error_test() ->
    Ev = #{<<"event_type_code">> => 16#80000001,
           <<"event_data">> => <<1,2,3>>},  %% way too short
    P = maps:get(<<"parsed">>, decode_event(Ev)),
    ?assertMatch(#{<<"error">> := _}, P).

decode_unknown_event_type_is_no_op_test() ->
    %% Unregistered code → empty `parsed'.
    Ev = #{<<"event_type_code">> => 16#DEADBEEF,
           <<"event_data">> => <<>>},
    ?assertEqual(#{}, maps:get(<<"parsed">>, decode_event(Ev))).

%% Pipeline: parse a log, then decode_events to get the
%% per-event `parsed' enrichment on every entry.
decode_events_on_full_fixture_test() ->
    Raw = build_fixture(),
    Parsed = parse(Raw),
    Decoded = decode_events(Parsed),
    %% Event 3 is the SecureBoot variable — should be
    %% semantically decoded.
    E3 = maps:get(<<"3">>, Decoded),
    P3 = maps:get(<<"parsed">>, E3),
    ?assertEqual(<<"SecureBoot">>, maps:get(<<"variable_name">>, P3)),
    Sem = maps:get(<<"semantic">>, P3),
    ?assertEqual(#{<<"secure_boot_enabled">> => true}, Sem),
    %% Event 2 is CRTM_VERSION — should have decoded string.
    E2 = maps:get(<<"2">>, Decoded),
    P2 = maps:get(<<"parsed">>, E2),
    ?assertEqual(<<"TEST FW v1">>, maps:get(<<"crtm_version">>, P2)).

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

-endif.
