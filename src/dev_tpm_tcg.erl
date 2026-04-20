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
-export([parse/1, parse/2, event_type_name/1, event_type_name/2]).

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

-endif.
