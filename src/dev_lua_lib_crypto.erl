%%% @doc Crypto library for Lua environment.
%%% Exposes Erlang's crypto module to luerl sandbox.
%%%
%%% Pattern follows dev_lua_lib.erl - each exported function (except install)
%%% is automatically added to the Lua `crypto` table.
%%%
%%% DETERMINISM WARNING:
%%% - hash, verify, to_hex, from_hex are DETERMINISTIC (replay-safe)
%%% - random_bytes is NON-DETERMINISTIC (gated by allow_nondeterministic option)
%%%
%%% @see dev_lua_lib
-module(dev_lua_lib_crypto).
%%% Library functions. Each exported function is _automatically_ added to the
%%% Lua environment, except for the `install/3' function, which is used to
%%% install the library in the first place.
-export([install/3]).
-export([hash/3, verify/3, to_hex/3, from_hex/3, random_bytes/3]).
%% sign/3 excluded - security risk (see Security section in plan)
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% OTP version compile-time check for OTP 22+ (needed for binary:encode_hex/1)
-ifdef(OTP_RELEASE).
    -if(?OTP_RELEASE < 22).
        -error("dev_lua_lib_crypto requires OTP 22+ for binary:encode_hex/1").
    -endif.
-endif.

%% @doc Install crypto functions into Lua's `crypto` table.
%% Pattern matches dev_lua_lib.erl:install/3
install(_Base, State, Opts) ->
    %% Create base crypto table first (like ao table in dev_lua_lib.erl)
    {ok, State2} =
        luerl:set_table_keys_dec(
            [crypto],
            dev_lua:encode(#{}, Opts),
            State
        ),
    %% Add each exported function (auto-discovered from module exports)
    {ok, lists:foldl(
        fun(FuncName, StateIn) ->
            {ok, StateOut} =
                luerl:set_table_keys_dec(
                    [crypto, FuncName],
                    fun(RawArgs, ImportState) ->
                        ?event(lua_crypto, {calling, {func, FuncName}}),
                        %% Decode arguments from Lua environment
                        Args = lists:map(
                            fun(Arg) ->
                                dev_lua:decode(
                                    luerl:decode(Arg, ImportState),
                                    Opts
                                )
                            end,
                            RawArgs
                        ),
                        %% Call the function with decoded arguments
                        {Res, ResState} = ?MODULE:FuncName(Args, ImportState, Opts),
                        %% Encode response for return to Lua
                        return(Res, ResState, Opts)
                    end,
                    StateIn
                ),
            StateOut
        end,
        State2,
        [FuncName || {FuncName, _} <- ?MODULE:module_info(exports),
                     FuncName /= module_info,
                     FuncName /= install]
    )}.

%% @doc Helper function for returning a result from a Lua function.
%% Pattern matches dev_lua_lib.erl:return/2
return(Result, ExecState, Opts) ->
    ?event(lua_crypto, {returning, {result, Result}}),
    TableEncoded = dev_lua:encode(Result, Opts),
    {ReturnParams, ResultingState} =
        lists:foldr(
            fun(LuaEncoded, {Params, StateIn}) ->
                {NewParam, NewState} = luerl:encode(LuaEncoded, StateIn),
                {[NewParam | Params], NewState}
            end,
            {[], ExecState},
            TableEncoded
        ),
    {ReturnParams, ResultingState}.

%% @doc crypto.hash(algorithm, data) -> binary
%% Supported: sha256, sha512, sha3_256, sha3_512, blake2b, blake2s, md5
%% DETERMINISTIC: Same input always produces same output (replay-safe)
hash([Algo, Data], State, _Opts) when is_binary(Algo), is_binary(Data) ->
    try
        Algorithm = algo_to_atom(Algo),
        Result = crypto:hash(Algorithm, Data),
        {[Result], State}
    catch
        throw:{unknown_algorithm, _} ->
            {[<<"error">>, <<"Unknown algorithm">>], State}
    end;
hash(_Args, State, _Opts) ->
    {[<<"error">>, <<"Invalid arguments: hash(algorithm, data) - both must be strings">>], State}.

%% @doc crypto.verify(signature, message, pubkey, algorithm) -> boolean
%% For Ed25519: crypto.verify(sig, msg, pubkey, "ed25519")
%% For RSA: crypto.verify(sig, msg, pubkey, "rsa_sha256")
%% DETERMINISTIC: Same input always produces same output (replay-safe)
verify([Sig, Msg, PubKey, <<"ed25519">>], State, _Opts)
        when is_binary(Sig), is_binary(Msg), is_binary(PubKey) ->
    try
        Result = crypto:verify(eddsa, none, Msg, Sig, [PubKey, ed25519]),
        {[Result], State}
    catch
        _:_ -> {[false], State}
    end;
verify([Sig, Msg, PubKey, <<"rsa_sha256">>], State, _Opts)
        when is_binary(Sig), is_binary(Msg), is_binary(PubKey) ->
    try
        Result = crypto:verify(rsa, sha256, Msg, Sig, PubKey),
        {[Result], State}
    catch
        _:_ -> {[false], State}
    end;
verify(_Args, State, _Opts) ->
    {[<<"error">>, <<"Invalid arguments: verify(sig, msg, pubkey, algo)">>], State}.

%% @doc crypto.to_hex(binary) -> hex_string (lowercase)
%% DETERMINISTIC: Same input always produces same output (replay-safe)
to_hex([Binary], State, _Opts) when is_binary(Binary) ->
    Hex = string:lowercase(binary:encode_hex(Binary)),
    {[Hex], State};
to_hex(_Args, State, _Opts) ->
    {[<<"error">>, <<"Invalid argument: to_hex(binary) - must be a string/binary">>], State}.

%% @doc crypto.from_hex(hex_string) -> binary
%% DETERMINISTIC: Same input always produces same output (replay-safe)
from_hex([HexString], State, _Opts) when is_binary(HexString) ->
    try
        Binary = binary:decode_hex(HexString),
        {[Binary], State}
    catch
        _:_ -> {[<<"error">>, <<"Invalid hex string">>], State}
    end;
from_hex(_Args, State, _Opts) ->
    {[<<"error">>, <<"Invalid argument: from_hex(hex_string)">>], State}.

%% @doc crypto.random_bytes(n) -> binary of n random bytes
%% NON-DETERMINISTIC: Returns different values each call
%% REQUIRED: Gated by allow_nondeterministic option (AO Panel requirement)
%%
%% WARNING: Using this in AO processes breaks replay determinism!
%% Different CUs will compute different states if this affects state.
%% Safe for: nonces in external calls, test data generation
%% Unsafe for: anything stored in process state
random_bytes([N], State, Opts) when is_integer(N), N > 0, N =< 1024 ->
    case hb_opts:get(allow_nondeterministic, false, Opts) of
        false ->
            {[<<"error">>, <<"random_bytes disabled: breaks replay determinism. Set allow_nondeterministic=true to enable.">>], State};
        true ->
            Bytes = crypto:strong_rand_bytes(N),
            {[Bytes], State}
    end;
random_bytes([N], State, _Opts) when is_integer(N) ->
    {[<<"error">>, <<"Invalid size: must be 1-1024">>], State};
random_bytes(_Args, State, _Opts) ->
    {[<<"error">>, <<"Invalid argument: random_bytes(n)">>], State}.

%% Internal helpers
algo_to_atom(<<"sha256">>) -> sha256;
algo_to_atom(<<"sha512">>) -> sha512;
algo_to_atom(<<"sha3_256">>) -> sha3_256;
algo_to_atom(<<"sha3_512">>) -> sha3_512;
algo_to_atom(<<"blake2b">>) -> {blake2b, 32};
algo_to_atom(<<"blake2s">>) -> {blake2s, 32};
algo_to_atom(<<"md5">>) -> md5;  %% Legacy, not recommended
algo_to_atom(Algo) -> throw({unknown_algorithm, Algo}).

%%% EUnit Tests

hash_sha256_test() ->
    Input = <<"hello">>,
    Expected = crypto:hash(sha256, Input),
    {[Result], _} = hash([<<"sha256">>, Input], #{}, #{}),
    ?assertEqual(Expected, Result).

hash_sha256_known_vector_test() ->
    %% SHA256("test") = 9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08
    Input = <<"test">>,
    {[Result], _} = hash([<<"sha256">>, Input], #{}, #{}),
    {[Hex], _} = to_hex([Result], #{}, #{}),
    ?assertEqual(<<"9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08">>, Hex).

hash_unknown_algo_test() ->
    {[<<"error">>, _], _} = hash([<<"unknown_algo">>, <<"data">>], #{}, #{}).

hash_invalid_args_test() ->
    %% Non-binary arguments should return error
    {[<<"error">>, _], _} = hash([123, <<"data">>], #{}, #{}),
    {[<<"error">>, _], _} = hash([<<"sha256">>], #{}, #{}).

to_hex_test() ->
    {[Hex], _} = to_hex([<<"Hello">>], #{}, #{}),
    ?assertEqual(<<"48656c6c6f">>, Hex).

to_hex_invalid_test() ->
    {[<<"error">>, _], _} = to_hex([123], #{}, #{}).

from_hex_test() ->
    {[Binary], _} = from_hex([<<"48656c6c6f">>], #{}, #{}),
    ?assertEqual(<<"Hello">>, Binary).

from_hex_invalid_test() ->
    {[<<"error">>, _], _} = from_hex([<<"not_hex_ZZZ">>], #{}, #{}).

hex_roundtrip_test() ->
    Original = <<"Test data 123">>,
    {[Hex], _} = to_hex([Original], #{}, #{}),
    {[Decoded], _} = from_hex([Hex], #{}, #{}),
    ?assertEqual(Original, Decoded).

random_bytes_disabled_by_default_test() ->
    %% REQUIRED: random_bytes should be disabled by default
    {[<<"error">>, Msg], _} = random_bytes([32], #{}, #{}),
    ?assert(binary:match(Msg, <<"disabled">>) =/= nomatch).

random_bytes_enabled_test() ->
    %% When explicitly enabled, should work
    Opts = #{allow_nondeterministic => true},
    {[Bytes], _} = random_bytes([32], #{}, Opts),
    ?assertEqual(32, byte_size(Bytes)).

random_bytes_invalid_size_test() ->
    Opts = #{allow_nondeterministic => true},
    {[<<"error">>, _], _} = random_bytes([0], #{}, Opts),
    {[<<"error">>, _], _} = random_bytes([2000], #{}, Opts).

verify_ed25519_test() ->
    %% Generate test keypair
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    Msg = <<"test message">>,
    Sig = crypto:sign(eddsa, none, Msg, [Priv, ed25519]),
    {[Result], _} = verify([Sig, Msg, Pub, <<"ed25519">>], #{}, #{}),
    ?assert(Result).

verify_ed25519_invalid_test() ->
    {Pub, _Priv} = crypto:generate_key(eddsa, ed25519),
    Msg = <<"test message">>,
    FakeSig = crypto:strong_rand_bytes(64),
    {[Result], _} = verify([FakeSig, Msg, Pub, <<"ed25519">>], #{}, #{}),
    ?assertNot(Result).

verify_invalid_args_test() ->
    {[<<"error">>, _], _} = verify([<<"sig">>, <<"msg">>], #{}, #{}).
