%%% @doc BLS12-381 signature verification over the `blst' library
%%% (supranational/blst, `native/lib/blst'), through `native/blst_nif'.
%%%
%%% The scheme is the IETF BLS signature "minimal-pubkey-size" variant:
%%% public keys are compressed G1 points (48 bytes), signatures are compressed
%%% G2 points (96 bytes), and messages hash to G2 with the ciphersuite
%%% `BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_'. A threshold group signature
%%% verifies the same way under the group public key.
-module(blst_nif).
-export([verify/3]).

-on_load(init/0).

init() ->
	PrivDir = code:priv_dir(hb),
	ok = erlang:load_nif(filename:join([PrivDir, "blst_nif"]), 0).

%% @doc Verify `Signature' over `Message' under `PublicKey'. Both points must
%% decode, lie on the curve and in their prime-order subgroup, and the key
%% must not be the identity; otherwise `{error, Reason}' names the failed
%% check. Runs on a dirty CPU scheduler.
-spec verify(binary(), binary(), binary()) -> boolean() | {error, atom()}.
verify(_PublicKey, _Message, _Signature) ->
	erlang:nif_error(nif_not_loaded).
