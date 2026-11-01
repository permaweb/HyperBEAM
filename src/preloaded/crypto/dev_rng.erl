%%% @doc A stateless device for verifiable randomness: it checks BLS beacon
%%% signatures and Any-Honest time-lock reveals and cracks, and derives bytes.
%%% Every key is a
%%% pure function of its inputs, so a process may call it from inside its
%%% computation (for example `ao.resolve' from Lua) and every node that
%%% replays the process reaches the same result.
%%% RNG Labs, a CipherPlay Inc. product, maintains the canonical protocol,
%%% reference network and threat model. CipherPlay Inc. owns the originating IP.
%%%
%%% Keys:
%%% <pre>
%%%     generator   Returns the fixed unknown-order group as `g' and `n'.
%%%     verify-reveal Inputs: `c', `alpha', `h'. Checks that the secret opens
%%%                 the commitment and returns canonical `y'.
%%%     verify-crack Inputs: `x', `t', `y', `proof'. Checks a Wesolowski proof
%%%                 for a withheld time-lock output and returns canonical `y'.
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
-export([generator/3, verify_reveal/3, verify_crack/3]).
-export([verify_bls/3, derive/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(MAX_DERIVE_BYTES, 1024).
-define(MAX_ELEMENT_HEX, 512).
-define(MAX_ALPHA_HEX, 128).
-define(MAX_T, (1 bsl 32)).
-define(N_HEX,
    <<
        "c7970ceedcc3b0754490201a7aa613cd73911081c790f5f1a8726f463550bb5b"
        "7ff0db8e1ea1189ec72f93d1650011bd721aeeacc2acde32a04107f0648c2813"
        "a31f5b0b7765ff8b44b4b6ffc93384b646eb09c7cf5e8592d40ea33c80039f35"
        "b4f14a04b51f7bfd781be4d1673164ba8eb991c2c4d730bbbe35f592bdef524a"
        "f7e8daefd26c66fc02c479af89d64d373f442709439de66ceb955f3ea37d5159"
        "f6135809f85334b5cb1813addc80cd05609f10ac6a95ad65872c909525bdad32"
        "bc729592642920f24c61dc5b3c3b7923e56b16a4d9d373d8721f24a3fc0f1b31"
        "31f55615172866bccc30f95054c824e733a5eb6817f7bc16399d48c6361cc7e5"
    >>
).
-define(G_HEX,
    <<
        "af837d388770d250471f03e561cef2f1d2793a166c01b582123ab1d418a26533"
        "1ffbe5bcf204f7d4d1a9d880feac19f16f45e7d03a6df66a763cc524106e1b88"
        "1afbb78b92994e69f0bcaee1ceb87688447e19e643cad5de61dcb6039d0f84a8"
        "1ebe1372e718fdd42447b48b0df11e6a7d704279a01b2eb7083769b38fea7b1f"
        "b35c161137e9b570ce49d6eb9185c424ad8e79b06a9e1edfc185982efc755c9e"
        "4cdf4961ddea042df9aa94383fb8aa720df097a738e3b6b6f5a331077a8d878a"
        "e424892a911fb0cb8030f29fb3edd3115af0fc88e1c00fafca3d6f10b0a28cf3"
        "dbcecff52398f6b53f00ac1622a0d72696aeba05ad8d817ebc31b8e3e159a2e8"
    >>
).

%% @doc The built-in group: the generator `g' and the modulus `n', in hex.
-spec generator(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ valid := true, _ => _ }}.
generator(_Base, _Req, _Opts) ->
    #{ g := G } = params(),
    {ok, #{ <<"valid">> => true, <<"g">> => to_hex(G), <<"n">> => ?N_HEX }}.

%% @doc Check a validator's reveal: its secret `alpha' opens the commit `c',
%% and its output is h^alpha.
-spec verify_reveal(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ valid := boolean(), _ => _ }}.
verify_reveal(Base, Req, Opts) ->
    safe(
        fun() ->
            maybe
                {ok, {_, C}} ?= element_input(<<"c">>, Base, Req, Opts),
                {ok, {_, H}} ?= element_input(<<"h">>, Base, Req, Opts),
                {ok, {_, Alpha}} ?=
                    hex_int_input(<<"alpha">>, ?MAX_ALPHA_HEX, Base, Req, Opts),
                check_reveal(C, Alpha, H)
            end
        end
    ).

%% @doc Check a crack: the answer `y' found by `t' squarings of `x' and its
%% Wesolowski proof.
-spec verify_crack(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ valid := boolean(), _ => _ }}.
verify_crack(Base, Req, Opts) ->
    safe(
        fun() ->
            maybe
                {ok, {XHex, X}} ?= element_input(<<"x">>, Base, Req, Opts),
                {ok, {YHex, Y}} ?= element_input(<<"y">>, Base, Req, Opts),
                {ok, {_, Proof}} ?= element_input(<<"proof">>, Base, Req, Opts),
                {ok, T} ?= t_input(Base, Req, Opts),
                check_crack(XHex, X, T, YHex, Y, Proof)
            end
        end
    ).

%% @doc SPEC 3: g^alpha = c (mod N); the output is canonical(h^alpha mod N).
check_reveal(C, Alpha, H) ->
    #{ n := N, g := G } = params(),
    case mod_pow(G, Alpha, N) =:= C of
        true -> {ok, canonical(mod_pow(H, Alpha, N), N)};
        false -> {error, <<"verify-failed">>}
    end.

%% @doc SPEC 2.5: proof^l * x^(2^t mod l) = y (mod N).
check_crack(XHex, X, T, YHex, Y, Proof) ->
    #{ n := N } = params(),
    L = challenge_prime(XHex, YHex, T),
    R = mod_pow(2, T, L),
    case (mod_pow(Proof, L, N) * mod_pow(X, R, N)) rem N =:= Y of
        true -> {ok, canonical(Y, N)};
        false -> {error, <<"verify-failed">>}
    end.

%% @doc y* = min(y, N - y): y and N - y pass the same checks, so they count
%% as one output.
canonical(Y, N) -> min(Y, N - Y).

%% @doc The fixed unknown-order group and primes used for proof verification.
params() ->
    #{
        n => binary_to_integer(?N_HEX, 16),
        g => binary_to_integer(?G_HEX, 16),
        small => small_primes()
    }.

%% @doc The Wesolowski challenge: the first prime at or above the first 128
%% bits of SHA-256("rng/wesolowski|x|y|t"), with bits 127 and 0 set.
challenge_prime(XHex, YHex, T) ->
    <<C:128, _/binary>> =
        crypto:hash(
            sha256,
            <<
                "rng/wesolowski|", XHex/binary, "|", YHex/binary, "|",
                (integer_to_binary(T))/binary
            >>
        ),
    #{ small := Small } = params(),
    next_prime(C bor (1 bsl 127) bor 1, Small).

next_prime(C, Small) ->
    case is_probable_prime(C, Small) of
        true -> C;
        false -> next_prime(C + 2, Small)
    end.

%% @doc Trial division by the primes below 1000, then Miller-Rabin with the
%% first 20 primes as bases, exactly as the reference does it.
is_probable_prime(N, _Small) when N < 2 -> false;
is_probable_prime(N, Small) ->
    case trial_division(N, Small) of
        prime -> true;
        composite -> false;
        unknown -> miller_rabin(N, lists:sublist(Small, 20))
    end.

trial_division(_N, []) -> unknown;
trial_division(N, [N | _]) -> prime;
trial_division(N, [P | _]) when N rem P =:= 0 -> composite;
trial_division(N, [_ | Rest]) -> trial_division(N, Rest).

miller_rabin(N, Bases) ->
    {D, S} = split_twos(N - 1, 0),
    lists:all(fun(A) -> mr_round(mod_pow(A, D, N), N, S) end, Bases).

split_twos(D, S) when D band 1 =:= 0 -> split_twos(D bsr 1, S + 1);
split_twos(D, S) -> {D, S}.

mr_round(1, _N, _S) -> true;
mr_round(X, N, _S) when X =:= N - 1 -> true;
mr_round(X, N, S) -> mr_square(X, N, S - 1).

mr_square(_X, _N, 0) -> false;
mr_square(X, N, Left) ->
    case (X * X) rem N of
        Sq when Sq =:= N - 1 -> true;
        Sq -> mr_square(Sq, N, Left - 1)
    end.

%% @doc The primes below 1000.
small_primes() ->
    lists:foldl(
        fun(K, Acc) ->
            case lists:any(fun(P) -> K rem P =:= 0 end, Acc) of
                true -> Acc;
                false -> Acc ++ [K]
            end
        end,
        [],
        lists:seq(2, 999)
    ).

%% @doc B^E mod M on non-negative integers.
mod_pow(_B, _E, 1) -> 0;
mod_pow(B, E, M) -> binary:decode_unsigned(crypto:mod_pow(B, E, M)).

%% @doc The number of bits of a positive integer.
bit_length(N) ->
    <<First, Rest/binary>> = binary:encode_unsigned(N),
    byte_size(Rest) * 8 + length(integer_to_list(First, 2)).

%% @doc Lowercase hex without leading zeros, as `BigInt.toString(16)'.
to_hex(N) -> string:lowercase(integer_to_binary(N, 16)).

%% @doc Run a time-lock key, turning its outcome (or any crash) into a result
%% message: `{ok, Y}' gives `valid' and the canonical output `y'.
safe(Fun) ->
    Result =
        try Fun()
        catch Class:Reason ->
            ?event(timelock, {crashed, Class, Reason}),
            {error, <<"internal-error">>}
        end,
    case Result of
        {ok, Y} -> {ok, #{ <<"valid">> => true, <<"y">> => to_hex(Y) }};
        Failure -> {ok, invalid(Failure)}
    end.
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

%% @doc Find a hex integer, lowercase without leading zeros, of at most
%% `Max' digits. Returns the hex as given and its value.
hex_int_input(Key, Max, Base, Req, Opts) ->
    maybe
        {ok, Hex} ?= binary_input(Key, Base, Req, Opts),
        true ?=
            byte_size(Hex) =< Max
                orelse {error, <<Key/binary, "-too-large">>},
        true ?= is_hex(Hex) orelse {error, <<"invalid-", Key/binary>>},
        true ?= is_canonical_hex(Hex)
            orelse {error, <<"non-canonical-", Key/binary>>},
        {ok, {Hex, binary_to_integer(Hex, 16)}}
    end.

is_hex(<<>>) -> false;
is_hex(Hex) ->
    lists:all(
        fun(C) ->
            (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f)
                orelse (C >= $A andalso C =< $F)
        end,
        binary_to_list(Hex)
    ).

is_canonical_hex(<<"0">>) -> true;
is_canonical_hex(<<"0", _/binary>>) -> false;
is_canonical_hex(Hex) -> string:lowercase(Hex) =:= Hex.

%% @doc A group element: a hex integer with 0 < value < N.
element_input(Key, Base, Req, Opts) ->
    maybe
        {ok, {_, V}} = Ok ?= hex_int_input(Key, ?MAX_ELEMENT_HEX, Base, Req, Opts),
        #{ n := N } = params(),
        true ?= (V > 0 andalso V < N) orelse {error, <<Key/binary, "-out-of-range">>},
        Ok
    end.

%% @doc The squaring count: an integer from 0 to 2^32, or its decimal
%% without leading zeros.
t_input(Base, Req, Opts) ->
    maybe
        {ok, Value} ?= input(<<"t">>, Base, Req, Opts),
        {ok, T} ?= parse_t(Value),
        true ?= (T >= 0 andalso T =< ?MAX_T) orelse {error, <<"t-out-of-range">>},
        {ok, T}
    end.

parse_t(T) when is_integer(T) -> {ok, T};
parse_t(<<"0">>) -> {ok, 0};
parse_t(<<First, _/binary>> = Dec)
        when First >= $1, First =< $9, byte_size(Dec) =< 11 ->
    case lists:all(fun(C) -> C >= $0 andalso C =< $9 end, binary_to_list(Dec)) of
        true -> {ok, binary_to_integer(Dec)};
        false -> {error, <<"invalid-t">>}
    end;
parse_t(<<First, _/binary>> = Dec) when First >= $1, First =< $9 ->
    case lists:all(fun(C) -> C >= $0 andalso C =< $9 end, binary_to_list(Dec)) of
        true -> {error, <<"t-out-of-range">>};
        false -> {error, <<"invalid-t">>}
    end;
parse_t(_) -> {error, <<"invalid-t">>}.

%%% Tests

%% @doc Resolve a key of the device with the given inputs.
resolve_rng(Key, Inputs) ->
    hb_ao:resolve(
        Inputs#{ <<"path">> => <<"~rng@1.0/", Key/binary>> },
        #{}
    ).

%% @doc Every time-lock vector from the reference implementation gives the
%% same `valid' and outputs (`y', `g', `n') as it does there.
timelock_vectors_test_() ->
    [
        {
            Name,
            fun() ->
                {ok, Res} = resolve_rng(Key, Inputs),
                maps:foreach(
                    fun(K, V) ->
                        ?assertEqual({K, V}, {K, hb_maps:get(K, Res, undefined, #{})})
                    end,
                    maps:remove(<<"l">>, Expected)
                ),
                case maps:get(<<"valid">>, Expected) of
                    true -> ok;
                    false ->
                        ?assert(is_binary(hb_maps:get(<<"reason">>, Res, undefined, #{})))
                end
            end
        }
    ||
        {Name, Key, Inputs, Expected} <- dev_timelock_test_vectors:timelock_vectors()
    ].

%% @doc The challenge primes of the valid cracks, and of the reference's own
%% fixed case, are the reference's.
challenge_prime_test() ->
    Cracks =
        [
            {I, E}
        ||
            {_, <<"verify-crack">>, I, #{ <<"l">> := _ } = E}
                <- dev_timelock_test_vectors:timelock_vectors()
        ],
    ?assert(length(Cracks) >= 3),
    lists:foreach(
        fun({I, E}) ->
            ?assertEqual(
                binary_to_integer(maps:get(<<"l">>, E), 16),
                challenge_prime(
                    maps:get(<<"x">>, I),
                    maps:get(<<"y">>, I),
                    binary_to_integer(maps:get(<<"t">>, I))
                )
            )
        end,
        Cracks
    ),
    {{X, Y, T}, L} = dev_timelock_test_vectors:fixed_challenge_prime(),
    Prime = challenge_prime(X, Y, T),
    ?assertEqual(binary_to_integer(L, 16), Prime),
    ?assertEqual(128, bit_length(Prime)).

%% @doc The built-in N is the 2048-bit RSA challenge number and g is a
%% square below it.
params_test() ->
    #{ n := N, g := G, small := Small } = params(),
    ?assertEqual(2048, bit_length(N)),
    ?assertEqual(?N_HEX, to_hex(N)),
    ?assert(G > 1 andalso G < N),
    ?assertEqual(168, length(Small)).

%% @doc The primality test refuses Carmichael numbers and small composites,
%% and accepts small and large primes.
primality_test() ->
    #{ small := Small } = params(),
    ?assertNot(is_probable_prime(561, Small)),
    ?assertNot(is_probable_prime(1, Small)),
    ?assertNot(is_probable_prime(1009 * 1013, Small)),
    ?assert(is_probable_prime(2, Small)),
    ?assert(is_probable_prime(997, Small)),
    ?assert(is_probable_prime((1 bsl 127) - 1, Small)),
    ?assertNot(is_probable_prime((1 bsl 128) + 1, Small)).

%% @doc Malformed and oversized inputs give `valid: false' with a reason, and
%% `t' may also be given as an integer.
timelock_malformed_inputs_test() ->
    Vectors =
        maps:from_list(
            [
                {Name, Inputs}
            ||
                {Name, _, Inputs, _} <- dev_timelock_test_vectors:timelock_vectors()
            ]
        ),
    Reveal = maps:get(<<"t1024-reveal-valid">>, Vectors),
    Crack = maps:get(<<"t1024-crack-valid">>, Vectors),
    Run =
        fun(Key, Inputs) ->
            {ok, Res} = resolve_rng(Key, Inputs),
            {
                hb_maps:get(<<"valid">>, Res, undefined, #{}),
                hb_maps:get(<<"reason">>, Res, undefined, #{})
            }
        end,
    ?assertEqual({true, undefined}, Run(<<"verify-reveal">>, Reveal)),
    ?assertEqual({true, undefined}, Run(<<"verify-crack">>, Crack)),
    ?assertEqual(
        {true, undefined},
        Run(
            <<"verify-crack">>,
            Crack#{ <<"t">> => binary_to_integer(maps:get(<<"t">>, Crack)) }
        )
    ),
    Upper = string:uppercase(maps:get(<<"c">>, Reveal)),
    Huge = binary:copy(<<"f">>, ?MAX_ELEMENT_HEX + 1),
    lists:foreach(
        fun({Key, Inputs, Reason}) ->
            ?assertEqual({false, Reason}, Run(Key, Inputs))
        end,
        [
            {<<"verify-reveal">>, maps:remove(<<"alpha">>, Reveal), <<"missing-alpha">>},
            {<<"verify-reveal">>, maps:remove(<<"h">>, Reveal), <<"missing-h">>},
            {<<"verify-reveal">>, Reveal#{ <<"c">> => Upper }, <<"non-canonical-c">>},
            {<<"verify-reveal">>,
                Reveal#{ <<"c">> => <<"0", (maps:get(<<"c">>, Reveal))/binary>> },
                <<"non-canonical-c">>},
            {<<"verify-reveal">>, Reveal#{ <<"c">> => <<>> }, <<"invalid-c">>},
            {<<"verify-reveal">>, Reveal#{ <<"c">> => <<"0x12">> }, <<"invalid-c">>},
            {<<"verify-reveal">>, Reveal#{ <<"c">> => 12 }, <<"invalid-c">>},
            {<<"verify-reveal">>, Reveal#{ <<"c">> => Huge }, <<"c-too-large">>},
            {<<"verify-reveal">>, Reveal#{ <<"c">> => <<"0">> }, <<"c-out-of-range">>},
            {<<"verify-reveal">>, Reveal#{ <<"h">> => ?N_HEX }, <<"h-out-of-range">>},
            {<<"verify-reveal">>,
                Reveal#{ <<"alpha">> => binary:copy(<<"f">>, ?MAX_ALPHA_HEX + 1) },
                <<"alpha-too-large">>},
            {<<"verify-reveal">>, Reveal#{ <<"alpha">> => <<"1">> }, <<"verify-failed">>},
            {<<"verify-crack">>, maps:remove(<<"proof">>, Crack), <<"missing-proof">>},
            {<<"verify-crack">>, Crack#{ <<"x">> => ?N_HEX }, <<"x-out-of-range">>},
            {<<"verify-crack">>, Crack#{ <<"y">> => <<"0">> }, <<"y-out-of-range">>},
            {<<"verify-crack">>, Crack#{ <<"proof">> => Huge }, <<"proof-too-large">>},
            {<<"verify-crack">>, Crack#{ <<"t">> => <<"4294967297">> }, <<"t-out-of-range">>},
            {<<"verify-crack">>, Crack#{ <<"t">> => <<"99999999999999999999">> },
                <<"t-out-of-range">>},
            {<<"verify-crack">>, Crack#{ <<"t">> => -1 }, <<"t-out-of-range">>},
            {<<"verify-crack">>, Crack#{ <<"t">> => <<"01024">> }, <<"invalid-t">>},
            {<<"verify-crack">>, Crack#{ <<"t">> => <<"-1">> }, <<"invalid-t">>},
            {<<"verify-crack">>, Crack#{ <<"t">> => <<"1e3">> }, <<"invalid-t">>},
            {<<"verify-crack">>, Crack#{ <<"t">> => 1.5 }, <<"invalid-t">>},
            {<<"verify-crack">>, Crack#{ <<"t">> => <<"1023">> }, <<"verify-failed">>}
        ]
    ),
    % A reveal and a crack of the same commit give the same canonical output.
    {ok, R} = resolve_rng(<<"verify-reveal">>, Reveal),
    {ok, K} = resolve_rng(<<"verify-crack">>, Crack),
    ?assertEqual(maps:get(<<"c">>, Reveal), maps:get(<<"x">>, Crack)),
    ?assertEqual(
        hb_maps:get(<<"y">>, R, undefined, #{}),
        hb_maps:get(<<"y">>, K, undefined, #{})
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
