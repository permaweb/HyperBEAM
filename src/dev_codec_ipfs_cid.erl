%%% @doc Pure functions for the thin slice of the IPFS/IPLD spec that
%%% `~ipfs@1.0' needs: unsigned varints, sha2-256 multihashes, base32-lower
%%% multibase, and CIDv1 encode/decode. Not a general IPFS library — CIDv0,
%%% non-sha2 hashes, multibases other than `b', and IPLD path resolution
%%% are all out of scope.
%%%
%%% References:
%%%   - CIDv1:           https://github.com/multiformats/cid
%%%   - Multihash:       https://github.com/multiformats/multihash
%%%   - Multibase:       https://github.com/multiformats/multibase
%%%   - unsigned-varint: https://github.com/multiformats/unsigned-varint
-module(dev_codec_ipfs_cid).
-export([encode/3, decode/1]).
-export([codec_code/1, codec_name/1]).
-export([multihash/2, multibase_encode/1, multibase_decode/1]).
-export([varint_encode/1, varint_decode/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Multicodec codes. Full registry:
%% https://github.com/multiformats/multicodec/blob/master/table.csv
-define(CODEC_RAW,      16#55).
-define(CODEC_DAG_CBOR, 16#71).

%% Multihash function code and sha2-256 digest length.
-define(HASH_SHA2_256, 16#12).
-define(SHA2_256_LEN,  32).

%% Multibase prefix for base32 lowercase (RFC4648, no padding).
-define(MB_BASE32_LOWER, $b).

%% @doc Encode `Body' as a CIDv1 string under `Codec' (`<<"raw">>' or
%% `<<"dag-cbor">>') and hash algorithm `sha2_256' (atom) or
%% `<<"sha2-256">>' (binary).
encode(Codec, HashAlg, Body) when is_binary(Codec) ->
    encode(codec_code(Codec), HashAlg, Body);
encode(CodecCode, <<"sha2-256">>, Body) ->
    encode(CodecCode, sha2_256, Body);
encode(CodecCode, sha2_256, Body)
        when is_integer(CodecCode), is_binary(Body) ->
    multibase_encode(
        <<(varint_encode(1))/binary,
          (varint_encode(CodecCode))/binary,
          (multihash(sha2_256, Body))/binary>>).

%% @doc Decode a CIDv1 string into its component parts, or `{error, _}'.
decode(Bin) when is_binary(Bin) ->
    case multibase_decode(Bin) of
        {ok, Raw} -> decode_bytes(Raw);
        Err -> Err
    end.

decode_bytes(Bin) ->
    try
        {1,         Rest1} = varint_decode(Bin),
        {CodecCode, Rest2} = varint_decode(Rest1),
        {HashCode,  Rest3} = varint_decode(Rest2),
        {DigestLen, Rest4} = varint_decode(Rest3),
        case {HashCode, DigestLen, Rest4} of
            {?HASH_SHA2_256, ?SHA2_256_LEN, <<Digest:?SHA2_256_LEN/binary>>} ->
                Multicodec = codec_name(CodecCode),
                {ok, #{
                    <<"version">>  => 1,
                    <<"hash-alg">> => <<"sha2-256-", Multicodec/binary>>,
                    <<"digest">>   => Digest
                }};
            {?HASH_SHA2_256, ?SHA2_256_LEN, _} ->
                {error, truncated_digest};
            {Other, _, _} ->
                {error, {unsupported_hash, Other}}
        end
    catch _:_ -> {error, malformed_cid}
    end.

%% @doc Resolve a codec name to its multicodec code.
codec_code(<<"raw">>)      -> ?CODEC_RAW;
codec_code(<<"dag-cbor">>) -> ?CODEC_DAG_CBOR;
codec_code(Other) -> throw({unsupported_codec, Other}).

%% @doc Inverse of `codec_code/1'. Unknown codes round-trip as
%% `<<"codec-0xHEX">>' so `decode/1' never throws on a stranger's CID.
codec_name(?CODEC_RAW)      -> <<"raw">>;
codec_name(?CODEC_DAG_CBOR) -> <<"dag-cbor">>;
codec_name(N) when is_integer(N) ->
    iolist_to_binary(io_lib:format("codec-0x~.16b", [N])).

%% @doc Wrap a sha2-256 digest of `Body' as a multihash binary.
multihash(sha2_256, Body) when is_binary(Body) ->
    <<(varint_encode(?HASH_SHA2_256))/binary,
      (varint_encode(?SHA2_256_LEN))/binary,
      (crypto:hash(sha256, Body))/binary>>.

%% @doc Multibase-encode a binary as base32-lowercase, no padding, prefix `b'.
multibase_encode(Bin) when is_binary(Bin) ->
    <<?MB_BASE32_LOWER, (base32:encode(Bin, [lower, nopad]))/binary>>.

%% @doc Multibase-decode. Accepts base32-lower (`b'), base32-upper (`B'),
%% and base16-lower (`f') defensively; anything else is `{error, _}'.
multibase_decode(<<?MB_BASE32_LOWER, Rest/binary>>) ->
    safe(fun() -> base32:decode(pad_base32(string:uppercase(Rest))) end,
         invalid_base32);
multibase_decode(<<$B, Rest/binary>>) ->
    safe(fun() -> base32:decode(pad_base32(Rest)) end, invalid_base32);
multibase_decode(<<$f, Rest/binary>>) ->
    safe(fun() -> binary:decode_hex(Rest) end, invalid_base16);
multibase_decode(<<Prefix, _/binary>>) ->
    {error, {unsupported_multibase, <<Prefix>>}};
multibase_decode(_) ->
    {error, empty_cid}.

safe(Fun, ErrorTag) ->
    try {ok, Fun()} catch _:_ -> {error, ErrorTag} end.

pad_base32(Bin) ->
    %% RFC4648 base32 groups are 40 bits (8 chars). Pad with `=' to a
    %% multiple of 8.
    case (8 - (byte_size(Bin) rem 8)) rem 8 of
        0 -> Bin;
        N -> <<Bin/binary, (binary:copy(<<"=">>, N))/binary>>
    end.

%% @doc Encode a non-negative integer as an unsigned-varint.
varint_encode(N) when is_integer(N), N >= 0, N < 16#80 ->
    <<N>>;
varint_encode(N) when is_integer(N), N >= 0 ->
    <<1:1, (N band 16#7f):7, (varint_encode(N bsr 7))/binary>>.

%% @doc Decode an unsigned-varint from the start of `Bin'. Returns
%% `{Value, Rest}'; throws `{malformed_varint, _}' on truncated input.
varint_decode(Bin) -> varint_decode(Bin, 0, 0).

varint_decode(<<0:1, B:7, Rest/binary>>, Acc, Shift) ->
    {Acc bor (B bsl Shift), Rest};
varint_decode(<<1:1, B:7, Rest/binary>>, Acc, Shift) when Shift < 63 ->
    varint_decode(Rest, Acc bor (B bsl Shift), Shift + 7);
varint_decode(_, _, _) ->
    throw({malformed_varint, truncated_or_too_long}).

%%% Tests

%% @doc IPFS canonical ground truth: `ipfs add --raw-leaves -Q <"hello world"'
%% returns this CID. The only immovable cross-check for our varint /
%% multihash / multibase / CIDv1 glue.
hello_world_raw_cid_test() ->
    ?assertEqual(
        <<"bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e">>,
        encode(<<"raw">>, sha2_256, <<"hello world">>)).

empty_raw_cid_test() ->
    ?assertEqual(
        <<"bafkreihdwdcefgh4dqkjv67uzcmw7ojee6xedzdetojuzjevtenxquvyku">>,
        encode(<<"raw">>, sha2_256, <<>>)).

empty_dag_cbor_cid_test() ->
    ?assertEqual(
        <<"bafyreigbtj4x7ip5legnfznufuopl4sg4knzc2cof6duas4b3q2fy6swua">>,
        encode(<<"dag-cbor">>, sha2_256, <<16#a0>>)).

roundtrip_decode_raw_test() ->
    CID = encode(<<"raw">>, sha2_256, <<"hello world">>),
    {ok, Parts} = decode(CID),
    ?assertEqual(<<"sha2-256-raw">>, maps:get(<<"hash-alg">>, Parts)),
    ?assertEqual(1, maps:get(<<"version">>, Parts)),
    ?assertEqual(crypto:hash(sha256, <<"hello world">>),
        maps:get(<<"digest">>, Parts)).

roundtrip_decode_dag_cbor_test() ->
    {ok, Parts} = decode(encode(<<"dag-cbor">>, sha2_256, <<"body bytes">>)),
    ?assertEqual(<<"sha2-256-dag-cbor">>, maps:get(<<"hash-alg">>, Parts)).

bad_multibase_prefix_test() ->
    ?assertMatch({error, {unsupported_multibase, _}}, decode(<<"Qmfoobar">>)).

malformed_cid_test() ->
    ?assertMatch({error, _}, decode(<<"baaa">>)).

varint_roundtrip_test() ->
    [ ?assertEqual({N, <<>>}, varint_decode(varint_encode(N)))
      || N <- [0, 1, 127, 128, 255, 16#55, 16#71, 1234, 16#ffff, 16#ffffffff] ].

varint_truncated_raises_test() ->
    ?assertThrow({malformed_varint, _}, varint_decode(<<16#ff>>)).

multihash_shape_test() ->
    MH = multihash(sha2_256, <<"x">>),
    <<16#12, 32, Digest:32/binary>> = MH,
    ?assertEqual(34, byte_size(MH)),
    ?assertEqual(crypto:hash(sha256, <<"x">>), Digest).

multibase_roundtrip_test() ->
    Bytes = <<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20>>,
    Encoded = multibase_encode(Bytes),
    ?assertMatch(<<?MB_BASE32_LOWER, _/binary>>, Encoded),
    ?assertEqual({ok, Bytes}, multibase_decode(Encoded)).
