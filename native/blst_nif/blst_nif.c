#include <string.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include <erl_nif.h>
#include <blst.h>

// BLS12-381 signature verification over supranational/blst, in the
// "minimal-pubkey-size" variant: public keys in G1 (48 bytes compressed),
// signatures in G2 (96 bytes compressed), messages hashed to G2.

#define BLS_PUBKEY_SIZE 48
#define BLS_SIGNATURE_SIZE 96

static const char BLS_DST[] = "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_";

static int blst_nif_load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info) {
	return 0;
}

static ERL_NIF_TERM error_atom(ErlNifEnv* env, const char* reason) {
	return enif_make_tuple2(
		env,
		enif_make_atom(env, "error"),
		enif_make_atom(env, reason)
	);
}

// verify(PublicKey :: <<_:384>>, Message :: binary(), Signature :: <<_:768>>)
//     -> true | false | {error, Reason}
// Both points are decompressed, checked to be on the curve and in their
// prime-order subgroup, and the public key must not be the identity.
static ERL_NIF_TERM verify_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	ErlNifBinary pk_bin, msg_bin, sig_bin;
	blst_p1_affine pk;
	blst_p2_affine sig;
	BLST_ERROR err;
	if (argc != 3
			|| !enif_inspect_binary(env, argv[0], &pk_bin)
			|| !enif_inspect_binary(env, argv[1], &msg_bin)
			|| !enif_inspect_binary(env, argv[2], &sig_bin)) {
		return enif_make_badarg(env);
	}
	if (pk_bin.size != BLS_PUBKEY_SIZE) {
		return error_atom(env, "bad_public_key_size");
	}
	if (sig_bin.size != BLS_SIGNATURE_SIZE) {
		return error_atom(env, "bad_signature_size");
	}
	// Public key: valid encoding, on the curve, in G1, not the identity.
	if (blst_p1_uncompress(&pk, pk_bin.data) != BLST_SUCCESS) {
		return error_atom(env, "bad_public_key");
	}
	if (blst_p1_affine_is_inf(&pk)) {
		return error_atom(env, "public_key_is_infinity");
	}
	if (!blst_p1_affine_in_g1(&pk)) {
		return error_atom(env, "public_key_not_in_group");
	}
	// Signature: valid encoding, on the curve, in G2.
	if (blst_p2_uncompress(&sig, sig_bin.data) != BLST_SUCCESS) {
		return error_atom(env, "bad_signature");
	}
	if (!blst_p2_affine_in_g2(&sig)) {
		return error_atom(env, "signature_not_in_group");
	}
	err =
		blst_core_verify_pk_in_g1(
			&pk,
			&sig,
			true,
			msg_bin.data, msg_bin.size,
			(const byte*)BLS_DST, sizeof(BLS_DST) - 1,
			NULL, 0
		);
	return enif_make_atom(env, err == BLST_SUCCESS ? "true" : "false");
}

static ErlNifFunc nif_funcs[] = {
	{"verify", 3, verify_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(blst_nif, nif_funcs, blst_nif_load, NULL, NULL, NULL);
