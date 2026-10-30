%%% @doc A stateless device for verifiable randomness: it checks a BLS
%%% randomness-beacon signature and derives bytes from a seed. Every key is a
%%% pure function of its inputs, so a process may call it from inside its
%%% computation (for example `ao.resolve' from Lua) and every node that
%%% replays the process reaches the same result.
%%% RNG Labs, a CipherPlay Inc. product, maintains the canonical protocol,
%%% reference network and threat model. CipherPlay Inc. owns the originating IP.
%%%
%%% Keys:
%%% <pre>
%%%     verify-bls  Inputs: `public-key' (hex, 48-byte compressed G1 point),
%%%                 `message' (binary), `signature' (hex, 96-byte compressed
%%%                 G2 point). Returns `valid' (boolean). When the signature
%%%                 is valid, `value' is the hex of
%%%                 SHA-256("hyperbeam/rng/v1/round|" ++ message ++ "|" ++
%%%                 hex(signature)), the beacon's round value; otherwise
%%%                 `reason' names the failed check.
%%%     derive      Inputs: `seed' (binary), `counter' (integer >= 0), `bytes'
%%%                 (integer, 1 to 1024). Returns `valid' and `value', the hex
%%%                 of the first `bytes' bytes of
%%%                 consecutive domain-separated, length-framed SHA-256 blocks.
%%% </pre>
%%%
%%% The BLS scheme is the IETF "minimal-pubkey-size" variant over BLS12-381
%%% (`blst_nif'): keys in G1, signatures in G2, ciphersuite
%%% `BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_'. A t-of-n threshold group
%%% signature verifies as an ordinary signature under the group key. Both
%%% points are subgroup-checked. Inputs are read from the request, then from
%%% the base message. Malformed inputs give `valid: false', never an error.
-module(dev_rng).
-export([verify_bls/3, derive/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(MAX_DERIVE_BYTES, 1024).

%% @doc Verify a BLS signature over a message under a public key.
-spec verify_bls(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ valid := boolean(), _ => _ }}.
verify_bls(Base, Req, Opts) ->
    Result =
        maybe
            {ok, PubKey} ?= hex_input(<<"public-key">>, Base, Req, Opts),
            {ok, Message} ?= binary_input(<<"message">>, Base, Req, Opts),
            {ok, Sig} ?= hex_input(<<"signature">>, Base, Req, Opts),
            true ?= blst_nif:verify(PubKey, Message, Sig),
            {ok, {Message, Sig}}
        end,
    case Result of
        {ok, {ValidMessage, ValidSig}} ->
            {ok,
                #{
                    <<"valid">> => true,
                    <<"value">> => round_value(ValidMessage, ValidSig)
                }};
        Failure ->
            ?event(rng, {verify_bls_failed, Failure}),
            {ok, invalid(Failure)}
    end.

%% @doc Derive up to `?MAX_DERIVE_BYTES' bytes from a seed and a counter.
-spec derive(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ valid := boolean(), _ => _ }}.
derive(Base, Req, Opts) ->
    Result =
        maybe
            {ok, Seed} ?= binary_input(<<"seed">>, Base, Req, Opts),
            {ok, Counter} ?= int_input(<<"counter">>, Base, Req, Opts),
            {ok, Bytes} ?= int_input(<<"bytes">>, Base, Req, Opts),
            true ?= Counter >= 0 orelse {error, <<"counter-negative">>},
            true ?=
                (Bytes >= 1 andalso Bytes =< ?MAX_DERIVE_BYTES)
                    orelse {error, <<"bytes-out-of-range">>},
            {ok, derive_bytes(Seed, Counter, Bytes)}
        end,
    case Result of
        {ok, Derived} ->
            {ok, #{ <<"valid">> => true, <<"value">> => hb_util:to_hex(Derived) }};
        Failure ->
            {ok, invalid(Failure)}
    end.

%% @doc The beacon's value for a round: the hex of SHA-256 over `round|'
%% followed by the lowercase hex of the round's group signature.
round_value(Message, Sig) ->
    SigHex = hb_util:to_hex(Sig),
    hb_util:to_hex(
        crypto:hash(
            sha256,
            <<"hyperbeam/rng/v1/round|", Message/binary, "|", SigHex/binary>>
        )
    ).

%% @doc Concatenate domain-separated, length-framed SHA-256 blocks for
%% counters `Counter + K', then keep the first `Bytes' bytes.
derive_bytes(Seed, Counter, Bytes) ->
    SeedLength = integer_to_binary(byte_size(Seed)),
    Stream =
        <<
            <<(crypto:hash(
                sha256,
                <<
                    "hyperbeam/rng/v1/derive|", SeedLength/binary, "|", Seed/binary,
                    "|", (integer_to_binary(Counter + K))/binary
                >>
            ))/binary>>
        ||
            K <- lists:seq(0, (Bytes + 31) div 32 - 1)
        >>,
    binary:part(Stream, 0, Bytes).

%% @doc The result message for a failed verification or derivation.
invalid({error, Reason}) when is_atom(Reason) ->
    Dashed = binary:replace(hb_util:bin(Reason), <<"_">>, <<"-">>, [global]),
    invalid({error, Dashed});
invalid({error, Reason}) when is_binary(Reason) ->
    #{ <<"valid">> => false, <<"reason">> => Reason };
invalid(false) ->
    #{ <<"valid">> => false, <<"reason">> => <<"verify-failed">> }.

%% @doc Find an input key in the request, then in the base message.
input(Key, Base, Req, Opts) ->
    case hb_maps:find(Key, Req, Opts) of
        {ok, Value} -> {ok, Value};
        error ->
            case hb_maps:find(Key, Base, Opts) of
                {ok, Value} -> {ok, Value};
                error -> {error, <<"missing-", Key/binary>>}
            end
    end.

%% @doc Find a binary input.
binary_input(Key, Base, Req, Opts) ->
    case input(Key, Base, Req, Opts) of
        {ok, Value} when is_binary(Value) -> {ok, Value};
        {ok, _} -> {error, <<"invalid-", Key/binary>>};
        Error -> Error
    end.

%% @doc Find a hex input and decode it.
hex_input(Key, Base, Req, Opts) ->
    maybe
        {ok, Hex} ?= binary_input(Key, Base, Req, Opts),
        try {ok, binary:decode_hex(Hex)}
        catch _:_ -> {error, <<"invalid-", Key/binary>>}
        end
    end.

%% @doc Find an integer input, given as an integer or in decimal.
int_input(Key, Base, Req, Opts) ->
    maybe
        {ok, Value} ?= input(Key, Base, Req, Opts),
        {ok, _Int} ?= hb_util:safe_int(Value)
    else
        {error, invalid} -> {error, <<"invalid-", Key/binary>>};
        Error -> Error
    end.

%%% Tests

%% @doc Resolve a key of the device with the given inputs.
resolve_rng(Key, Inputs) ->
    hb_ao:resolve(
        Inputs#{ <<"path">> => <<"~rng@1.0/", Key/binary>> },
        #{}
    ).

%% @doc Every interop vector from the beacon's JavaScript (noble-curves)
%% verifies, or fails, exactly as it does there.
interop_vectors_test_() ->
    [
        {
            Name,
            fun() ->
                {ok, Res} = resolve_rng(<<"verify-bls">>, Inputs),
                ?assertEqual(Valid, hb_maps:get(<<"valid">>, Res, undefined, #{})),
                case Valid of
                    true ->
                        ?assertEqual(
                            round_value(
                                maps:get(<<"message">>, Inputs),
                                binary:decode_hex(
                                    maps:get(<<"signature">>, Inputs)
                                )
                            ),
                            hb_maps:get(<<"value">>, Res, undefined, #{})
                        );
                    false ->
                        ?assertNot(hb_maps:is_key(<<"value">>, Res, #{}))
                end
            end
        }
    ||
        {Name, Inputs, Valid} <- dev_rng_test_vectors:bls_vectors()
    ].

%% @doc A round signature of the live beacon process verifies under its group
%% key, and the device gives the value the beacon published for it.
live_beacon_round_test() ->
    {Inputs, Value} = dev_rng_test_vectors:live_round(),
    {ok, Res} = resolve_rng(<<"verify-bls">>, Inputs),
    ?assertEqual(true, hb_maps:get(<<"valid">>, Res, undefined, #{})),
    ?assertEqual(Value, hb_maps:get(<<"value">>, Res, undefined, #{})).

%% @doc On-curve points outside the prime-order subgroups are rejected by the
%% subgroup checks, before any pairing.
subgroup_check_test() ->
    Vectors = maps:from_list(
        [{N, I} || {N, I, _} <- dev_rng_test_vectors:bls_vectors()]
    ),
    Raw =
        fun(Name) ->
            I = maps:get(Name, Vectors),
            blst_nif:verify(
                binary:decode_hex(maps:get(<<"public-key">>, I)),
                maps:get(<<"message">>, I),
                binary:decode_hex(maps:get(<<"signature">>, I))
            )
        end,
    ?assertEqual({error, signature_not_in_group}, Raw(<<"non-subgroup-sig">>)),
    ?assertEqual({error, public_key_not_in_group}, Raw(<<"non-subgroup-key">>)),
    ?assertEqual({error, public_key_is_infinity}, Raw(<<"infinity-key">>)).

%% @doc Missing and mistyped inputs give `valid: false' with a reason.
malformed_inputs_test() ->
    {_Name, Good, true} = hd(dev_rng_test_vectors:bls_vectors()),
    Check =
        fun(Inputs, Reason) ->
            {ok, Res} = resolve_rng(<<"verify-bls">>, Inputs),
            ?assertEqual(false, hb_maps:get(<<"valid">>, Res, undefined, #{})),
            ?assertEqual(Reason, hb_maps:get(<<"reason">>, Res, undefined, #{}))
        end,
    Check(maps:remove(<<"signature">>, Good), <<"missing-signature">>),
    Check(Good#{ <<"public-key">> => <<"0x12">> }, <<"invalid-public-key">>),
    Check(Good#{ <<"message">> => 7 }, <<"invalid-message">>),
    Check(
        Good#{ <<"signature">> => <<"00">> },
        <<"bad-signature-size">>
    ).

%% @doc `derive' gives domain-separated, length-framed SHA-256 blocks, cut to
%% the requested length, and refuses out-of-range requests.
derive_test() ->
    Seed = <<"seed">>,
    Expected =
        binary:part(
            <<
                (crypto:hash(sha256, <<"hyperbeam/rng/v1/derive|4|seed|7">>))/binary,
                (crypto:hash(sha256, <<"hyperbeam/rng/v1/derive|4|seed|8">>))/binary,
                (crypto:hash(sha256, <<"hyperbeam/rng/v1/derive|4|seed|9">>))/binary
            >>,
            0,
            70
        ),
    {ok, Res} =
        resolve_rng(
            <<"derive">>,
            #{ <<"seed">> => Seed, <<"counter">> => 7, <<"bytes">> => 70 }
        ),
    ExpectedHex = hb_util:to_hex(Expected),
    ?assertEqual(ExpectedHex, hb_maps:get(<<"value">>, Res, undefined, #{})),
    % Integers given in decimal, as they arrive over HTTP, are the same.
    {ok, FromDecimal} =
        resolve_rng(
            <<"derive">>,
            #{
                <<"seed">> => Seed,
                <<"counter">> => <<"7">>,
                <<"bytes">> => <<"70">>
            }
        ),
    ?assertEqual(
        ExpectedHex,
        hb_maps:get(<<"value">>, FromDecimal, undefined, #{})
    ),
    {ok, Max} =
        resolve_rng(
            <<"derive">>,
            #{ <<"seed">> => Seed, <<"counter">> => 0, <<"bytes">> => 1024 }
        ),
    ?assertEqual(2048, byte_size(hb_maps:get(<<"value">>, Max, undefined, #{}))),
    {ok, FramedA} =
        resolve_rng(
            <<"derive">>,
            #{ <<"seed">> => <<"a1">>, <<"counter">> => 2, <<"bytes">> => 32 }
        ),
    {ok, FramedB} =
        resolve_rng(
            <<"derive">>,
            #{ <<"seed">> => <<"a">>, <<"counter">> => 12, <<"bytes">> => 32 }
        ),
    ?assertNotEqual(
        hb_maps:get(<<"value">>, FramedA, undefined, #{}),
        hb_maps:get(<<"value">>, FramedB, undefined, #{})
    ),
    lists:foreach(
        fun({Inputs, Reason}) ->
            {ok, Bad} = resolve_rng(<<"derive">>, Inputs),
            ?assertEqual(false, hb_maps:get(<<"valid">>, Bad, undefined, #{})),
            ?assertEqual(Reason, hb_maps:get(<<"reason">>, Bad, undefined, #{}))
        end,
        [
            {#{ <<"seed">> => Seed, <<"counter">> => 0, <<"bytes">> => 1025 },
                <<"bytes-out-of-range">>},
            {#{ <<"seed">> => Seed, <<"counter">> => 0, <<"bytes">> => 0 },
                <<"bytes-out-of-range">>},
            {#{ <<"seed">> => Seed, <<"counter">> => -1, <<"bytes">> => 8 },
                <<"counter-negative">>},
            {#{ <<"seed">> => Seed, <<"counter">> => <<"x">>, <<"bytes">> => 8 },
                <<"invalid-counter">>},
            {#{ <<"counter">> => 0, <<"bytes">> => 8 }, <<"missing-seed">>}
        ]
    ).

%% @doc The Lua module used by the `lua@5.3a' tests: its functions call the
%% device through `ao.resolve', as a beacon process does.
lua_module() ->
    <<"""
    function check_round(base, req)
        local status, res = ao.resolve({
            path = "/~rng@1.0/verify-bls",
            ["public-key"] = req["public-key"],
            message = req.message,
            signature = req.signature
        })
        return res
    end
    function check_round_device_key(base, req)
        local status, res = ao.resolve(
            {
                device = "rng@1.0",
                ["public-key"] = req["public-key"],
                message = req.message,
                signature = req.signature
            },
            { path = "verify-bls" }
        )
        return res
    end
    function derive_seed(base, req)
        local status, res = ao.resolve({
            path = "/~rng@1.0/derive",
            seed = req.seed,
            counter = 7,
            bytes = 70
        })
        return res
    end
    """>>.

%% @doc A `lua@5.3a' process verifies a beacon round, and derives bytes,
%% by calling the device from Lua.
lua_resolve_test() ->
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"module">> => #{
            <<"content-type">> => <<"application/lua">>,
            <<"body">> => lua_module()
        }
    },
    {Live, Value} = dev_rng_test_vectors:live_round(),
    lists:foreach(
        fun(Func) ->
            {ok, Res} = hb_ao:resolve(Base, Live#{ <<"path">> => Func }, #{}),
            ?assertEqual(true, hb_ao:get(<<"valid">>, Res, #{})),
            ?assertEqual(Value, hb_ao:get(<<"value">>, Res, #{})),
            {ok, Bad} =
                hb_ao:resolve(
                    Base,
                    Live#{ <<"path">> => Func, <<"message">> => <<"forged|1">> },
                    #{}
                ),
            ?assertEqual(false, hb_ao:get(<<"valid">>, Bad, #{}))
        end,
        [<<"check_round">>, <<"check_round_device_key">>]
    ),
    {ok, Derived} =
        hb_ao:resolve(
            Base,
            #{ <<"path">> => <<"derive_seed">>, <<"seed">> => <<"seed">> },
            #{}
        ),
    {ok, Direct} =
        resolve_rng(
            <<"derive">>,
            #{ <<"seed">> => <<"seed">>, <<"counter">> => 7, <<"bytes">> => 70 }
        ),
    ?assertEqual(
        hb_maps:get(<<"value">>, Direct, undefined, #{}),
        hb_ao:get(<<"value">>, Derived, #{})
    ).
