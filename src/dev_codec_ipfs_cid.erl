%%% @doc Pure functions for the thin slice of the IPFS/IPLD spec that this
%%% device needs: unsigned varints, sha2-256 multihashes, base32-lowercase
%%% multibase, and CIDv1 encode/decode.
%%%
%%% Intentionally narrow. This module covers only what `dev_codec_ipfs' uses
%%% to turn a `body' blob into a content identifier. It is not a general IPFS
%%% library: no CIDv0, no hash functions besides sha2-256, no multibases
%%% besides base32-lower, and no resolution of IPLD paths. See
%%% `docs/devices/ipfs-at-1-0.md' for the device-level rationale.
%%%
%%% References:
%%%   - CIDv1 spec:      https://github.com/multiformats/cid
%%%   - Multihash spec:  https://github.com/multiformats/multihash
%%%   - Multibase spec:  https://github.com/multiformats/multibase
%%%   - unsigned-varint: https://github.com/multiformats/unsigned-varint
-module(dev_codec_ipfs_cid).
-export([encode/3, decode/1]).
-export([codec_code/1, codec_name/1]).
-export([multihash/2, multibase_encode/1, multibase_decode/1]).
-export([varint_encode/1, varint_decode/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Multicodec codes we care about. Full registry:
%% https://github.com/multiformats/multicodec/blob/master/table.csv
-define(CODEC_RAW,      16#55).
-define(CODEC_DAG_CBOR, 16#71).

%% Multihash function codes.
-define(HASH_SHA2_256, 16#12).
-define(SHA2_256_LEN,  32).

%% Multibase prefix for base32 lowercase (RFC4648, no padding).
-define(MB_BASE32_LOWER, $b).

%% @doc Encode a `body' blob as a CIDv1 string, using the given codec name
%% (`<<"raw">>' | `<<"dag-cbor">>') and hash algorithm (`sha2_256' atom, or
%% `<<"sha2-256">>' binary).
encode(Codec, HashAlg, Body) when is_binary(Codec) ->
    encode(codec_code(Codec), HashAlg, Body);
encode(CodecCode, <<"sha2-256">>, Body) ->
    encode(CodecCode, sha2_256, Body);
encode(CodecCode, sha2_256, Body)
        when is_integer(CodecCode), is_binary(Body) ->
    MH = multihash(sha2_256, Body),
    CIDBin =
        <<
            (varint_encode(1))/binary,
            (varint_encode(CodecCode))/binary,
            MH/binary
        >>,
    multibase_encode(CIDBin).

%% @doc Decode a CIDv1 string into its components. Returns `{error, Reason}'
%% if the string is not a CIDv1 that this module knows how to parse.
decode(Bin) when is_binary(Bin) ->
    case multibase_decode(Bin) of
        {ok, Raw} -> decode_bytes(Raw);
        Err -> Err
    end.

decode_bytes(Bin) ->
    try
        {Version, Rest1} = varint_decode(Bin),
        case Version of
            1 ->
                {CodecCode, Rest2} = varint_decode(Rest1),
                {HashCode, Rest3} = varint_decode(Rest2),
                {DigestLen, Digest} = varint_decode(Rest3),
                case {HashCode, byte_size(Digest)} of
                    {?HASH_SHA2_256, DigestLen} when DigestLen =:= ?SHA2_256_LEN ->
                        %% Combine the multihash function and the
                        %% multicodec into a single `hash-alg' string, the
                        %% way IPFS tooling names a CID's construction.
                        Multicodec = codec_name(CodecCode),
                        HashAlg = <<"sha2-256-", Multicodec/binary>>,
                        {ok, #{
                            <<"version">>  => 1,
                            <<"hash-alg">> => HashAlg,
                            <<"digest">>   => Digest
                        }};
                    {_, L} when L =/= DigestLen ->
                        {error, {truncated_digest, {declared, DigestLen}, {actual, L}}};
                    {Other, _} ->
                        {error, {unsupported_hash, Other}}
                end;
            V ->
                {error, {unsupported_cid_version, V}}
        end
    catch
        _:_ -> {error, malformed_cid}
    end.

%% @doc Resolve a codec name to its multicodec code.
codec_code(<<"raw">>)      -> ?CODEC_RAW;
codec_code(<<"dag-cbor">>) -> ?CODEC_DAG_CBOR;
codec_code(Other) -> throw({unsupported_codec, Other}).

%% @doc Inverse of `codec_code/1'. Unknown codes round-trip as a `<<"codec-0xHEX">>'
%% binary so that decode never throws on a stranger's CID.
codec_name(?CODEC_RAW)      -> <<"raw">>;
codec_name(?CODEC_DAG_CBOR) -> <<"dag-cbor">>;
codec_name(N) when is_integer(N) ->
    iolist_to_binary(io_lib:format("codec-0x~.16b", [N])).

%% @doc Wrap a digest as a multihash binary: <<code, len, digest...>>.
multihash(sha2_256, Body) when is_binary(Body) ->
    Digest = crypto:hash(sha256, Body),
    <<
        (varint_encode(?HASH_SHA2_256))/binary,
        (varint_encode(?SHA2_256_LEN))/binary,
        Digest/binary
    >>.

%% @doc Multibase-encode a binary as base32-lowercase, no padding, prefix `b'.
multibase_encode(Bin) when is_binary(Bin) ->
    Encoded = base32:encode(Bin, [lower, nopad]),
    <<?MB_BASE32_LOWER, Encoded/binary>>.

%% @doc Multibase-decode. Accepts base32 lowercase (`b'), base32 upper (`B'),
%% and base16 lowercase (`f') defensively. Anything else is `{error, _}'.
multibase_decode(<<?MB_BASE32_LOWER, Rest/binary>>) ->
    try {ok, base32:decode(pad_base32(string:uppercase(Rest)))}
    catch _:_ -> {error, invalid_base32} end;
multibase_decode(<<$B, Rest/binary>>) ->
    try {ok, base32:decode(pad_base32(Rest))}
    catch _:_ -> {error, invalid_base32} end;
multibase_decode(<<$f, Rest/binary>>) ->
    try {ok, binary:decode_hex(Rest)}
    catch _:_ -> {error, invalid_base16} end;
multibase_decode(<<Prefix, _/binary>>) ->
    {error, {unsupported_multibase, <<Prefix>>}};
multibase_decode(_) ->
    {error, empty_cid}.

pad_base32(Bin) ->
    %% RFC4648 base32 groups are 40 bits (8 chars). Pad with `=' to a multiple of 8.
    case (8 - (byte_size(Bin) rem 8)) rem 8 of
        0 -> Bin;
        N -> <<Bin/binary, (binary:copy(<<"=">>, N))/binary>>
    end.

%% @doc Encode a non-negative integer as an unsigned-varint.
varint_encode(N) when is_integer(N), N >= 0, N < 16#80 ->
    <<N>>;
varint_encode(N) when is_integer(N), N >= 0 ->
    <<1:1, (N band 16#7f):7, (varint_encode(N bsr 7))/binary>>.

%% @doc Decode an unsigned-varint from the start of a binary. Returns
%% `{Value, Rest}'. Throws on truncated input.
varint_decode(Bin) ->
    varint_decode(Bin, 0, 0).

varint_decode(<<0:1, B:7, Rest/binary>>, Acc, Shift) ->
    {Acc bor (B bsl Shift), Rest};
varint_decode(<<1:1, B:7, Rest/binary>>, Acc, Shift) when Shift < 63 ->
    varint_decode(Rest, Acc bor (B bsl Shift), Shift + 7);
varint_decode(_, _, _) ->
    throw({malformed_varint, truncated_or_too_long}).

%%%====================================================================
%%% Tests
%%%====================================================================

%% IPFS canonical reference: `ipfs add --raw-leaves -Q <"hello world"` returns
%% the below CID. We use this as an immovable ground-truth across all our
%% varint / multihash / multibase / CID glue.
hello_world_raw_cid_test() ->
    CID = encode(<<"raw">>, sha2_256, <<"hello world">>),
    ?assertEqual(
        <<"bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e">>,
        CID
    ).

%% Empty body under the `raw' codec. Cross-checked against
%% `ipfs add --raw-leaves -Q /dev/null'.
empty_raw_cid_test() ->
    CID = encode(<<"raw">>, sha2_256, <<>>),
    ?assertEqual(
        <<"bafkreihdwdcefgh4dqkjv67uzcmw7ojee6xedzdetojuzjevtenxquvyku">>,
        CID
    ).

%% Known DAG-CBOR CID for the canonical empty-map block (`0xa0`), cross-checked
%% against `ipfs dag put <<<"{}"` with input-codec dag-cbor.
empty_dag_cbor_cid_test() ->
    CID = encode(<<"dag-cbor">>, sha2_256, <<16#a0>>),
    ?assertEqual(
        <<"bafyreigbtj4x7ip5legnfznufuopl4sg4knzc2cof6duas4b3q2fy6swua">>,
        CID
    ).

roundtrip_decode_raw_test() ->
    CID = encode(<<"raw">>, sha2_256, <<"hello world">>),
    {ok, Parts} = decode(CID),
    ?assertEqual(<<"sha2-256-raw">>, maps:get(<<"hash-alg">>, Parts)),
    ?assertEqual(1, maps:get(<<"version">>, Parts)),
    ?assertEqual(32, byte_size(maps:get(<<"digest">>, Parts))),
    ?assertEqual(
        crypto:hash(sha256, <<"hello world">>),
        maps:get(<<"digest">>, Parts)
    ).

roundtrip_decode_dag_cbor_test() ->
    CID = encode(<<"dag-cbor">>, sha2_256, <<"body bytes">>),
    {ok, Parts} = decode(CID),
    ?assertEqual(<<"sha2-256-dag-cbor">>, maps:get(<<"hash-alg">>, Parts)).

bad_multibase_prefix_test() ->
    ?assertMatch({error, {unsupported_multibase, _}},
        decode(<<"Qmfoobar">>)).

malformed_cid_test() ->
    %% A `b' prefix with a valid base32 body that decodes to nonsense.
    ?assertMatch({error, _}, decode(<<"baaa">>)).

varint_roundtrip_test() ->
    lists:foreach(
        fun(N) ->
            Enc = varint_encode(N),
            ?assertEqual({N, <<>>}, varint_decode(Enc))
        end,
        [0, 1, 127, 128, 255, 16#55, 16#71, 1234, 16#ff_ff, 16#ff_ff_ff_ff]).

varint_truncated_raises_test() ->
    %% Continuation bit set but no following byte.
    ?assertThrow({malformed_varint, _}, varint_decode(<<16#ff>>)).

multihash_shape_test() ->
    MH = multihash(sha2_256, <<"x">>),
    %% code(0x12) + len(32) + 32-byte digest = 34 bytes
    ?assertEqual(34, byte_size(MH)),
    <<16#12, 32, Digest:32/binary>> = MH,
    ?assertEqual(crypto:hash(sha256, <<"x">>), Digest).

multibase_roundtrip_test() ->
    Bytes = <<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20>>,
    Encoded = multibase_encode(Bytes),
    ?assertMatch(<<?MB_BASE32_LOWER, _/binary>>, Encoded),
    ?assertEqual({ok, Bytes}, multibase_decode(Encoded)).
