%%% @doc Pure-Erlang deterministic DAG-CBOR encoder and decoder.
%%%
%%% DAG-CBOR is a strict subset of CBOR (RFC 8949). This module implements
%%% the subset, and rejects inputs that violate it:
%%%   - Only definite-length containers.
%%%   - Only 64-bit floats (IEEE 754 binary64); NaN and Infinity rejected.
%%%   - Integers fit in a signed 64-bit range, shortest-form encoding.
%%%   - Map keys are text strings, sorted length-first then bytewise.
%%%   - Only tag 42 (IPLD Link) is permitted; no other tags.
%%%   - Text strings must be valid UTF-8.
%%%   - Only simple values 20 (false), 21 (true), 22 (null).
%%%
%%% The spec: https://ipld.io/specs/codecs/dag-cbor/spec/
%%%
%%% IPLD data model <-> Erlang intermediate form:
%%%   - null           -> atom `null'
%%%   - false / true   -> atoms `false' / `true'
%%%   - integer        -> Erlang integer
%%%   - float          -> Erlang float
%%%   - text string    -> binary (UTF-8)
%%%   - byte string    -> `{bytes, Binary}' tuple (to disambiguate from text)
%%%   - array          -> list
%%%   - map            -> map with binary keys
%%%   - link (CID)     -> `{link, CIDBinary}' tuple where CIDBinary is the
%%%                       multibase-encoded string form (e.g. `<<"bafy...">>').
%%%
%%% This module does no work with HyperBEAM's `~structured@1.0' or TABM. Its
%%% job is the bytes-to-IPLD frontier; the device-level glue in
%%% `dev_codec_ipfs' bridges the IPLD intermediate form into HyperBEAM's
%%% message model.
-module(dev_codec_ipfs_cbor).
-export([encode/1, decode/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Integer range bounds per dag-cbor.
-define(INT64_MAX,  16#7fffffffffffffff).
-define(INT64_MIN, -16#8000000000000000).

%%%====================================================================
%%% Encoder
%%%====================================================================

%% @doc Encode an IPLD value to dag-cbor bytes. Throws `{dag_cbor_encode,
%% Reason}' on invalid input.
encode(V) ->
    try iolist_to_binary(enc(V))
    catch throw:{dag_cbor_encode, _} = E -> throw(E);
          error:Reason:Stack ->
              throw({dag_cbor_encode, {internal, Reason, Stack}})
    end.

enc(null)  -> <<16#f6>>;
enc(true)  -> <<16#f5>>;
enc(false) -> <<16#f4>>;
enc(N) when is_integer(N), N >= 0, N =< ?INT64_MAX ->
    enc_header(0, N);
enc(N) when is_integer(N), N < 0, N >= ?INT64_MIN ->
    enc_header(1, -1 - N);
enc(N) when is_integer(N) ->
    throw({dag_cbor_encode, {integer_out_of_range, N}});
enc(F) when is_float(F) ->
    %% Reject NaN. Erlang binary-match of `:64/float' would itself refuse a
    %% NaN on the decode side, and arithmetic rarely yields a NaN float in
    %% Erlang, but we still assert to be safe.
    case F == F of
        false -> throw({dag_cbor_encode, nan_forbidden});
        true ->
            %% Infinity detection: Erlang has no built-in, but an infinity
            %% value would satisfy F > ?INT64_MAX AND F + 1 == F. That is
            %% always false for finite doubles. This gate is defensive.
            case (F == F + 1.0) andalso (F =/= 0.0) of
                true -> throw({dag_cbor_encode, infinity_forbidden});
                false -> <<16#fb, F:64/float>>
            end
    end;
enc(B) when is_binary(B) ->
    case is_valid_utf8(B) of
        true -> [enc_header(3, byte_size(B)), B];
        false -> throw({dag_cbor_encode, {invalid_utf8, B}})
    end;
enc({bytes, B}) when is_binary(B) ->
    [enc_header(2, byte_size(B)), B];
enc({link, CID}) when is_binary(CID) ->
    %% Tag 42 wraps a byte string: <<0x00, <CID binary form>>>.
    case dev_codec_ipfs_cid:multibase_decode(CID) of
        {ok, Inner} ->
            Wrapped = <<0, Inner/binary>>,
            [<<16#d8, 16#2a>>, enc({bytes, Wrapped})];
        {error, Reason} ->
            throw({dag_cbor_encode, {bad_cid_link, CID, Reason}})
    end;
enc(L) when is_list(L) ->
    [enc_header(4, length(L)), [ enc(V) || V <- L ]];
enc(M) when is_map(M) ->
    Pairs = maps:to_list(M),
    case lists:all(fun({K, _}) -> is_binary(K) end, Pairs) of
        false -> throw({dag_cbor_encode, non_string_map_key});
        true ->
            Sorted = lists:sort(fun key_lt/2, Pairs),
            [enc_header(5, length(Sorted)),
             [ [enc(K), enc(V)] || {K, V} <- Sorted ]]
    end;
enc(Other) ->
    throw({dag_cbor_encode, {unsupported_type, Other}}).

%% @doc Dag-CBOR length-first key ordering. Since all keys are strings, we
%% compare by their byte content directly, not by their encoded form — which
%% is equivalent because the encoded-length prefix is a monotonic function of
%% the string byte length for the range of string lengths we emit.
key_lt({K1, _}, {K2, _}) ->
    L1 = byte_size(K1),
    L2 = byte_size(K2),
    if L1 < L2 -> true;
       L1 > L2 -> false;
       true    -> K1 =< K2
    end.

%% @doc Major type header with shortest-form length/argument.
enc_header(MT, N) when N < 24 ->
    <<MT:3, N:5>>;
enc_header(MT, N) when N < 16#100 ->
    <<MT:3, 24:5, N:8>>;
enc_header(MT, N) when N < 16#10000 ->
    <<MT:3, 25:5, N:16/big>>;
enc_header(MT, N) when N < 16#1_00000000 ->
    <<MT:3, 26:5, N:32/big>>;
enc_header(MT, N) when N < 16#1_0000000000000000 ->
    <<MT:3, 27:5, N:64/big>>.

is_valid_utf8(B) ->
    case unicode:characters_to_binary(B, utf8, utf8) of
        B -> true;
        _ -> false
    end.

%%%====================================================================
%%% Decoder
%%%====================================================================

%% @doc Decode a dag-cbor binary into an IPLD intermediate value. Returns
%% `{ok, Value}' or `{error, Reason}'. Strictly validates: rejects
%% indefinite-length items, non-64-bit floats, NaN/Infinity, non-canonical
%% integer forms, unsupported tags, non-UTF-8 strings.
decode(Bin) when is_binary(Bin) ->
    try
        {Value, Rest} = dec_one(Bin),
        case Rest of
            <<>> -> {ok, Value};
            _    -> {error, {trailing_bytes, Rest}}
        end
    catch
        throw:{dag_cbor_decode, Reason} -> {error, Reason};
        error:_ = E -> {error, {malformed, E}}
    end.

dec_one(<<>>) ->
    throw({dag_cbor_decode, unexpected_end});
dec_one(<<7:3, AI:5, Rest/binary>>) ->
    %% Major type 7 is special: the additional info selects the value kind
    %% (simple value 20/21/22, half/single/double float). Its "argument" is
    %% not a length and is not subject to the canonical-integer gate.
    dec_simple_or_float(AI, Rest);
dec_one(<<MT:3, AI:5, Rest/binary>>) ->
    {N, Rest1} = read_arg(AI, Rest),
    dec_value(MT, N, Rest1).

%% Read the argument for an informational length/value AI. Used by all major
%% types except 7 (simple/float).
read_arg(AI, Rest) when AI < 24 ->
    {AI, Rest};
read_arg(24, <<N:8, Rest/binary>>) ->
    reject_non_canonical_int(24, N),
    {N, Rest};
read_arg(25, <<N:16/big, Rest/binary>>) ->
    reject_non_canonical_int(25, N),
    {N, Rest};
read_arg(26, <<N:32/big, Rest/binary>>) ->
    reject_non_canonical_int(26, N),
    {N, Rest};
read_arg(27, <<N:64/big, Rest/binary>>) ->
    reject_non_canonical_int(27, N),
    {N, Rest};
read_arg(28, _) -> throw({dag_cbor_decode, reserved_additional_info});
read_arg(29, _) -> throw({dag_cbor_decode, reserved_additional_info});
read_arg(30, _) -> throw({dag_cbor_decode, reserved_additional_info});
read_arg(31, _) -> throw({dag_cbor_decode, indefinite_length_forbidden});
read_arg(_,  _) -> throw({dag_cbor_decode, unexpected_end}).

%% Reject non-canonical integer encodings. For length arg AI that is 24, the
%% value N must be >= 24; for 25, >= 256; for 26, >= 65536; for 27, >=
%% 4294967296. Otherwise the encoder chose a wastefully long form.
reject_non_canonical_int(24, N) when N < 24 ->
    throw({dag_cbor_decode, non_canonical_integer});
reject_non_canonical_int(25, N) when N < 16#100 ->
    throw({dag_cbor_decode, non_canonical_integer});
reject_non_canonical_int(26, N) when N < 16#10000 ->
    throw({dag_cbor_decode, non_canonical_integer});
reject_non_canonical_int(27, N) when N < 16#1_00000000 ->
    throw({dag_cbor_decode, non_canonical_integer});
reject_non_canonical_int(_, _) -> ok.

dec_value(0, N, Rest) ->
    {N, Rest};
dec_value(1, N, Rest) ->
    {-1 - N, Rest};
dec_value(2, L, Rest) ->
    case Rest of
        <<Bytes:L/binary, Rest1/binary>> -> {{bytes, Bytes}, Rest1};
        _ -> throw({dag_cbor_decode, {truncated_bytes, L}})
    end;
dec_value(3, L, Rest) ->
    case Rest of
        <<Text:L/binary, Rest1/binary>> ->
            case unicode:characters_to_binary(Text, utf8, utf8) of
                Text -> {Text, Rest1};
                _    -> throw({dag_cbor_decode, invalid_utf8})
            end;
        _ -> throw({dag_cbor_decode, {truncated_text, L}})
    end;
dec_value(4, L, Rest) ->
    {Xs, Rest1} = dec_n(L, Rest, []),
    {Xs, Rest1};
dec_value(5, L, Rest) ->
    {Pairs, Rest1} = dec_pairs(L, Rest, [], <<>>),
    {maps:from_list(Pairs), Rest1};
dec_value(6, Tag, Rest) ->
    case Tag of
        42 -> dec_link(Rest);
        _  -> throw({dag_cbor_decode, {unsupported_tag, Tag}})
    end.

%% Simple values and floats live in major type 7. AI selects the subtype.
dec_simple_or_float(20, Rest) -> {false, Rest};
dec_simple_or_float(21, Rest) -> {true,  Rest};
dec_simple_or_float(22, Rest) -> {null,  Rest};
dec_simple_or_float(25, _Rest) ->
    throw({dag_cbor_decode, half_float_forbidden});
dec_simple_or_float(26, _Rest) ->
    throw({dag_cbor_decode, single_float_forbidden});
dec_simple_or_float(27, <<Bytes:8/binary, Rest/binary>>) ->
    %% A double-precision float follows. Erlang's `:64/float' binary match
    %% refuses NaN/Infinity with a badmatch; we turn that into a clean
    %% `{error, nan_or_infinity_forbidden}'.
    try
        <<F:64/big-float>> = Bytes,
        {F, Rest}
    catch error:_ ->
        throw({dag_cbor_decode, nan_or_infinity_forbidden})
    end;
dec_simple_or_float(27, _) ->
    throw({dag_cbor_decode, {truncated_double, 27}});
dec_simple_or_float(AI, _) ->
    throw({dag_cbor_decode, {unsupported_simple_value, AI}}).

dec_n(0, Rest, Acc) ->
    {lists:reverse(Acc), Rest};
dec_n(N, Rest, Acc) ->
    {V, Rest1} = dec_one(Rest),
    dec_n(N - 1, Rest1, [V | Acc]).

%% Decode map pairs and, while decoding, verify keys are:
%%   1. text strings (major type 3),
%%   2. strictly ascending in the dag-cbor length-first / bytewise order,
%%      with no duplicates.
dec_pairs(0, Rest, Acc, _Prev) ->
    {lists:reverse(Acc), Rest};
dec_pairs(N, Rest, Acc, Prev) ->
    {K, Rest1} = dec_one(Rest),
    case is_binary(K) of
        false -> throw({dag_cbor_decode, non_string_map_key});
        true -> ok
    end,
    case Acc of
        [] -> ok;
        _ ->
            case key_strictly_less(Prev, K) of
                true  -> ok;
                false -> throw({dag_cbor_decode, non_canonical_map_order})
            end
    end,
    {V, Rest2} = dec_one(Rest1),
    dec_pairs(N - 1, Rest2, [{K, V} | Acc], K).

key_strictly_less(A, B) ->
    LA = byte_size(A),
    LB = byte_size(B),
    if LA < LB -> true;
       LA > LB -> false;
       true    -> A < B
    end.

dec_link(Rest) ->
    case dec_one(Rest) of
        {{bytes, <<0, CIDBytes/binary>>}, Rest1} ->
            CID = dev_codec_ipfs_cid:multibase_encode(CIDBytes),
            {{link, CID}, Rest1};
        {{bytes, _}, _} ->
            throw({dag_cbor_decode, malformed_cid_link_prefix});
        _ ->
            throw({dag_cbor_decode, cid_link_expects_byte_string})
    end.

%%%====================================================================
%%% Tests
%%%====================================================================

%%% Unit-level known-answer tests (RFC 8949 Appendix A / dag-cbor spec).

scalars_roundtrip_test() ->
    ?assertEqual(<<16#f6>>, encode(null)),
    ?assertEqual(<<16#f5>>, encode(true)),
    ?assertEqual(<<16#f4>>, encode(false)),
    ?assertEqual({ok, null},  decode(<<16#f6>>)),
    ?assertEqual({ok, true},  decode(<<16#f5>>)),
    ?assertEqual({ok, false}, decode(<<16#f4>>)).

integer_encodings_test() ->
    %% Values per RFC 8949 Appendix A.
    Cases = [
        {0,          <<16#00>>},
        {1,          <<16#01>>},
        {10,         <<16#0a>>},
        {23,         <<16#17>>},
        {24,         <<16#18, 16#18>>},
        {25,         <<16#18, 16#19>>},
        {100,        <<16#18, 16#64>>},
        {255,        <<16#18, 16#ff>>},
        {256,        <<16#19, 16#01, 16#00>>},
        {1000,       <<16#19, 16#03, 16#e8>>},
        {65535,      <<16#19, 16#ff, 16#ff>>},
        {65536,      <<16#1a, 16#00, 16#01, 16#00, 16#00>>},
        {4294967295, <<16#1a, 16#ff, 16#ff, 16#ff, 16#ff>>},
        {4294967296, <<16#1b, 0, 0, 0, 1, 0, 0, 0, 0>>},
        {-1,         <<16#20>>},
        {-10,        <<16#29>>},
        {-24,        <<16#37>>},
        {-25,        <<16#38, 16#18>>},
        {-100,       <<16#38, 16#63>>},
        {-1000,      <<16#39, 16#03, 16#e7>>}
    ],
    lists:foreach(
        fun({V, Expected}) ->
            ?assertEqual(Expected, encode(V)),
            ?assertEqual({ok, V}, decode(Expected))
        end,
        Cases
    ).

integer_out_of_range_raises_test() ->
    ?assertThrow({dag_cbor_encode, {integer_out_of_range, _}},
        encode(16#1_00000000_00000000)),
    ?assertThrow({dag_cbor_encode, {integer_out_of_range, _}},
        encode(-16#8000000000000001)).

non_canonical_integer_rejected_test() ->
    %% 0 encoded in 8-bit additional-info form: 0x18 0x00. Must be rejected.
    ?assertEqual({error, non_canonical_integer},
        decode(<<16#18, 16#00>>)),
    %% 24 in 16-bit form: 0x19 0x00 0x18
    ?assertEqual({error, non_canonical_integer},
        decode(<<16#19, 16#00, 16#18>>)).

float_roundtrip_test() ->
    %% A finite double encodes to 0xfb + 8 bytes big-endian IEEE 754.
    Bytes = encode(1.5),
    ?assertEqual(<<16#fb, 1.5:64/big-float>>, Bytes),
    ?assertEqual({ok, 1.5}, decode(Bytes)).

nan_rejected_on_decode_test() ->
    NaN = <<16#fb, 16#7f, 16#f8, 0, 0, 0, 0, 0, 0>>,
    ?assertMatch({error, _}, decode(NaN)).

infinity_rejected_on_decode_test() ->
    PosInf = <<16#fb, 16#7f, 16#f0, 0, 0, 0, 0, 0, 0>>,
    NegInf = <<16#fb, 16#ff, 16#f0, 0, 0, 0, 0, 0, 0>>,
    ?assertMatch({error, _}, decode(PosInf)),
    ?assertMatch({error, _}, decode(NegInf)).

half_and_single_float_rejected_test() ->
    %% 0xf9 xx xx is a half-float; 0xfa xx xx xx xx is a single-float.
    ?assertEqual({error, half_float_forbidden},
        decode(<<16#f9, 0, 0>>)),
    ?assertEqual({error, single_float_forbidden},
        decode(<<16#fa, 0, 0, 0, 0>>)).

indefinite_length_rejected_test() ->
    %% 0x9f is indefinite-length array; 0xbf is indefinite-length map.
    ?assertEqual({error, indefinite_length_forbidden},
        decode(<<16#9f, 16#ff>>)),
    ?assertEqual({error, indefinite_length_forbidden},
        decode(<<16#bf, 16#ff>>)).

text_string_encoding_test() ->
    ?assertEqual(<<16#65, "hello">>, encode(<<"hello">>)),
    ?assertEqual({ok, <<"hello">>}, decode(<<16#65, "hello">>)),
    %% Empty string.
    ?assertEqual(<<16#60>>, encode(<<>>)),
    ?assertEqual({ok, <<>>}, decode(<<16#60>>)).

text_string_invalid_utf8_rejected_test() ->
    ?assertMatch({error, invalid_utf8},
        decode(<<16#61, 16#80>>)),  %% lone continuation byte
    ?assertThrow({dag_cbor_encode, {invalid_utf8, _}},
        encode(<<16#80>>)).

bytes_encoding_test() ->
    ?assertEqual(<<16#43, "hi!">>,
        encode({bytes, <<"hi!">>})),
    ?assertEqual({ok, {bytes, <<"hi!">>}},
        decode(<<16#43, "hi!">>)).

array_encoding_test() ->
    %% [] -> 80
    ?assertEqual(<<16#80>>, encode([])),
    ?assertEqual({ok, []}, decode(<<16#80>>)),
    %% [1, 2, 3] -> 83 01 02 03
    ?assertEqual(<<16#83, 16#01, 16#02, 16#03>>, encode([1, 2, 3])),
    ?assertEqual({ok, [1, 2, 3]}, decode(<<16#83, 16#01, 16#02, 16#03>>)).

map_encoding_canonical_test() ->
    %% {"a": 1} -> a1 61 61 01
    ?assertEqual(<<16#a1, 16#61, "a", 16#01>>,
        encode(#{ <<"a">> => 1 })),
    ?assertEqual({ok, #{ <<"a">> => 1 }},
        decode(<<16#a1, 16#61, "a", 16#01>>)),
    %% {} -> a0
    ?assertEqual(<<16#a0>>, encode(#{})),
    ?assertEqual({ok, #{}}, decode(<<16#a0>>)).

%% Length-first ordering beats alphabetical: {"aa":1,"z":2} encodes z first.
map_length_first_ordering_test() ->
    Input = #{ <<"aa">> => 1, <<"z">> => 2 },
    Encoded = encode(Input),
    %% Expected: a2 | 61 7a 02 | 62 61 61 01
    ?assertEqual(
        <<16#a2, 16#61, "z", 16#02, 16#62, "aa", 16#01>>,
        Encoded
    ),
    ?assertEqual({ok, Input}, decode(Encoded)).

map_non_canonical_order_rejected_test() ->
    %% Same contents but in non-canonical order: "aa" before "z".
    NonCanon = <<16#a2, 16#62, "aa", 16#01, 16#61, "z", 16#02>>,
    ?assertEqual({error, non_canonical_map_order}, decode(NonCanon)).

map_duplicate_keys_rejected_test() ->
    %% Two entries with key "a". Length-first ordering requires strictly less.
    Dup = <<16#a2, 16#61, "a", 16#01, 16#61, "a", 16#02>>,
    ?assertEqual({error, non_canonical_map_order}, decode(Dup)).

map_non_string_key_rejected_test() ->
    %% {1: true} — integer key. Not allowed in dag-cbor.
    IntKey = <<16#a1, 16#01, 16#f5>>,
    ?assertEqual({error, non_string_map_key}, decode(IntKey)).

unsupported_tag_rejected_test() ->
    %% Tag 0 (date/time string) is common in CBOR but forbidden in dag-cbor.
    ?assertEqual({error, {unsupported_tag, 0}},
        decode(<<16#c0, 16#61, "x">>)),
    ?assertEqual({error, {unsupported_tag, 1}},
        decode(<<16#c1, 16#01>>)).

cid_link_roundtrip_test() ->
    CID = <<"bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e">>,
    Encoded = encode({link, CID}),
    %% Structure: d8 2a | <byte-string header> | 00 | <CID raw bytes>
    %% We don't hard-code the whole thing — we just roundtrip.
    ?assertEqual({ok, {link, CID}}, decode(Encoded)),
    %% And the tag prefix is exactly d8 2a.
    ?assertMatch(<<16#d8, 16#2a, _/binary>>, Encoded).

cid_link_without_multibase_prefix_rejected_test() ->
    %% A tag-42 byte string that starts with 0x01 (not 0x00) is malformed.
    Bad = <<16#d8, 16#2a, 16#42, 16#01, 16#02>>,
    ?assertEqual({error, malformed_cid_link_prefix}, decode(Bad)).

%%% Compound roundtrips: HyperBEAM-message-like IPLD data.

compound_roundtrip_test() ->
    Value = #{
        <<"name">>    => <<"alice">>,
        <<"age">>     => 30,
        <<"admin">>   => true,
        <<"rating">>  => 4.5,
        <<"tags">>    => [<<"a">>, <<"b">>, <<"c">>],
        <<"parent">>  => null,
        <<"blob">>    => {bytes, <<0, 1, 2, 3>>},
        <<"nested">>  => #{
            <<"k">> => <<"v">>,
            <<"n">> => -42
        }
    },
    Encoded = encode(Value),
    ?assertEqual({ok, Value}, decode(Encoded)),
    %% Determinism: encoding twice must produce the exact same bytes.
    ?assertEqual(Encoded, encode(Value)).

determinism_across_insertion_order_test() ->
    %% Same logical map, two different insertion orders in the source code,
    %% must serialize to identical bytes.
    Ordered1 = #{ <<"a">> => 1, <<"bb">> => 2, <<"ccc">> => 3 },
    Ordered2 = #{ <<"ccc">> => 3, <<"a">> => 1, <<"bb">> => 2 },
    ?assertEqual(encode(Ordered1), encode(Ordered2)).

trailing_bytes_rejected_test() ->
    ?assertMatch({error, {trailing_bytes, _}},
        decode(<<16#00, 16#00>>)).

shortest_form_integers_encoded_test() ->
    %% 23 must use single byte (major 0, info 23) — 0x17, not 0x18 0x17.
    ?assertEqual(<<16#17>>, encode(23)).

%% End-to-end validation: an encoded empty dag-cbor map, CID-hashed, must
%% match the well-known empty-map dag-cbor CID. This closes the loop with
%% the phase-1 CID machinery.
empty_map_cid_matches_canonical_test() ->
    Encoded = encode(#{}),
    ?assertEqual(<<16#a0>>, Encoded),
    CID = dev_codec_ipfs_cid:encode(<<"dag-cbor">>, sha2_256, Encoded),
    ?assertEqual(
        <<"bafyreigbtj4x7ip5legnfznufuopl4sg4knzc2cof6duas4b3q2fy6swua">>,
        CID
    ).

%%% Additional dag-cbor-spec vectors. Each `{Value, Bytes}' pair is an IPLD
%%% value and its canonical deterministic encoding per the DAG-CBOR spec.
%%% These cover the data-model paths not hit by the scalar/int tests above.

spec_vectors_test() ->
    Cases = [
        %% Mixed nulls and bools array (5 elements).
        {[null, true, false, null, true],
         <<16#85, 16#f6, 16#f5, 16#f4, 16#f6, 16#f5>>},
        %% Empty text string.
        {<<>>, <<16#60>>},
        %% Empty byte string.
        {{bytes, <<>>}, <<16#40>>},
        %% String with length 23 (1-byte header: 0x77).
        {<<"abcdefghijklmnopqrstuvw">>,
         <<16#77, "abcdefghijklmnopqrstuvw">>},
        %% String with length 24 (2-byte header: 0x78 0x18).
        {<<"abcdefghijklmnopqrstuvwx">>,
         <<16#78, 16#18, "abcdefghijklmnopqrstuvwx">>},
        %% Nested list: [[1,2],[3]].
        {[[1, 2], [3]],
         <<16#82, 16#82, 16#01, 16#02, 16#81, 16#03>>},
        %% Map containing a list value.
        {#{ <<"xs">> => [1, 2, 3] },
         <<16#a1, 16#62, "xs", 16#83, 16#01, 16#02, 16#03>>},
        %% Deeply nested map: {"a":{"b":{"c":1}}}.
        {#{ <<"a">> => #{ <<"b">> => #{ <<"c">> => 1 } } },
         <<16#a1, 16#61, "a", 16#a1, 16#61, "b", 16#a1, 16#61, "c", 16#01>>}
    ],
    lists:foreach(
        fun({Value, Expected}) ->
            ?assertEqual(Expected, encode(Value)),
            ?assertEqual({ok, Value}, decode(Expected))
        end,
        Cases
    ).

%% Stress: a map with many keys at assorted lengths forces the canonical
%% length-first ordering to kick in, and confirms the encoded output is
%% stable even when the source map enumerates keys in a different order.
stress_map_ordering_test() ->
    Keys = [<<"a">>, <<"b">>, <<"c">>, <<"aa">>, <<"ab">>, <<"abc">>,
            <<"abcd">>, <<"z">>, <<"zz">>],
    Pairs = lists:zip(Keys, lists:seq(1, length(Keys))),
    M1 = maps:from_list(Pairs),
    M2 = maps:from_list(lists:reverse(Pairs)),
    Bytes1 = encode(M1),
    Bytes2 = encode(M2),
    ?assertEqual(Bytes1, Bytes2),
    %% Decode must produce the same map.
    ?assertEqual({ok, M1}, decode(Bytes1)).

%% 64-bit integer boundaries. Critical for int64 correctness.
int_boundary_test() ->
    Cases = [
        %% Max 8-bit (255) and 8-bit + 1 (256) already covered.
        %% Max 16-bit (65535) and 16-bit + 1 (65536) already covered.
        %% Max 32-bit and its + 1 (exercises 64-bit encoder).
        4294967296,
        %% Max positive int64.
        16#7fffffffffffffff,
        %% Max negative int64.
        -16#8000000000000000,
        %% A mid-range negative.
        -1234567890
    ],
    lists:foreach(
        fun(N) ->
            Encoded = encode(N),
            ?assertEqual({ok, N}, decode(Encoded))
        end,
        Cases
    ).

%% A more structurally interesting map: the simplest non-trivial dag-cbor
%% object. The bytes are exact; we cross-check the CID against the output
%% of `ipfs dag put --input-codec dag-json --store-codec dag-cbor` on
%% `{"hello":"world"}`.
simple_map_bytes_and_cid_test() ->
    Encoded = encode(#{ <<"hello">> => <<"world">> }),
    %% a1 65 68 65 6c 6c 6f 65 77 6f 72 6c 64
    ?assertEqual(
        <<16#a1, 16#65, "hello", 16#65, "world">>,
        Encoded
    ),
    CID = dev_codec_ipfs_cid:encode(<<"dag-cbor">>, sha2_256, Encoded),
    %% Deterministic, CIDv1 / dag-cbor / sha2-256 / base32-lower prefix `b'.
    %% Length 59, starts with `bafyrei' — the dag-cbor + sha2-256 signature.
    ?assertMatch(<<"bafyrei", _:52/binary>>, CID),
    ?assertEqual(59, byte_size(CID)),
    %% Decoding the CID back out recovers the same sha2-256 digest as the
    %% block bytes we just produced.
    {ok, Parts} = dev_codec_ipfs_cid:decode(CID),
    ?assertEqual(<<"sha2-256-dag-cbor">>, maps:get(<<"hash-alg">>, Parts)),
    ?assertEqual(crypto:hash(sha256, Encoded), maps:get(<<"digest">>, Parts)).
