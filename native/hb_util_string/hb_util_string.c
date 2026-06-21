/*
 * hb_util_string: dependency-free byte-transform NIFs backing the ASCII
 * string operations in `hb_util'. Each maps input bytes in a single scalar
 * pass (the compiler auto-vectorizes the compare-and-select at -O3, so no
 * `-march=native' is needed):
 *
 *   lowercase/1   `A'..`Z' -> `a'..`z'                  (to_lower)
 *   key_chars/1   `A'..`Z' -> `a'..`z', and `-' -> `_'  (key_to_atom)
 *   canon_chars/1 `A'..`Z' -> `a'..`z', and `_' -> `-'  (hb_opts canonical_key)
 *   dash_chars/1  `_' -> `-'                            (atom_to_dashed_binary)
 *
 * UTF-8 safety: `lowercase' and `key_chars' fold ASCII only and do NOT
 * validate UTF-8, whereas `string:lowercase' folds full Unicode and *throws*
 * on invalid UTF-8 (a contract `ar_tx' tag parsing relies on to reject
 * non-string tags). To stay both fast and exact, those two functions return
 * the atom `non_ascii' the moment they see a byte >= 0x80, and the Erlang
 * caller falls back to the original `string:lowercase'-based expression. Pure
 * ASCII -- the overwhelming common case for HB keys -- takes the fast path and
 * is provably identical to `string:lowercase' (only `A'..`Z' has an ASCII case
 * mapping, and ASCII is always valid UTF-8, so it never throws). `dash_chars'
 * only swaps the ASCII byte `_'<->`-' and never folds, so it is exact for all
 * input and needs no fallback.
 *
 * A fresh binary is allocated; the caller's input memory is never mutated. The
 * module carries no mutable static state; its `upgrade' callback is a no-op so
 * it reloads cleanly under the device-test preloader's code upgrade ("Upgrade
 * not supported" otherwise).
 */
#include <stddef.h>

#include "erl_nif.h"

/* `lowercase/1' is general purpose and may be handed an arbitrarily large
 * binary, so inputs at or above this size run on a dirty CPU scheduler. The
 * key/atom transforms only ever see bounded keys (atoms are <= 255 bytes), so
 * they always run inline. */
#ifndef HB_UTIL_STRING_DIRTY_THRESHOLD
#define HB_UTIL_STRING_DIRTY_THRESHOLD (256U * 1024U)
#endif

static inline unsigned char
to_lower_byte(unsigned char c)
{
    return (c >= 'A' && c <= 'Z') ? (unsigned char)(c + ('a' - 'A')) : c;
}

static inline unsigned char
key_byte(unsigned char c)
{
    if (c >= 'A' && c <= 'Z') {
        return (unsigned char)(c + ('a' - 'A'));
    }
    return (c == '-') ? (unsigned char)'_' : c;
}

static inline unsigned char
canon_byte(unsigned char c)
{
    if (c >= 'A' && c <= 'Z') {
        return (unsigned char)(c + ('a' - 'A'));
    }
    return (c == '_') ? (unsigned char)'-' : c;
}

static inline unsigned char
dash_byte(unsigned char c)
{
    return (c == '_') ? (unsigned char)'-' : c;
}

/* ASCII-only transform that bails to the atom `non_ascii' on the first byte
 * >= 0x80, so the Erlang caller can delegate to `string:lowercase'. FN is
 * `static inline', so each expansion keeps its loop vectorizable. */
#define ASCII_TRANSFORM_BODY(FN)                                              \
    ErlNifBinary in;                                                          \
    ERL_NIF_TERM out_term;                                                    \
    unsigned char* out;                                                       \
    size_t i;                                                                 \
    if (!enif_inspect_binary(env, argv[0], &in)) {                           \
        return enif_make_badarg(env);                                         \
    }                                                                         \
    if (in.size == 0) {                                                       \
        return argv[0];                                                       \
    }                                                                         \
    out = enif_make_new_binary(env, in.size, &out_term);                      \
    if (out == NULL) {                                                        \
        return enif_raise_exception(env, enif_make_atom(env, "enomem"));      \
    }                                                                         \
    for (i = 0; i < in.size; i++) {                                           \
        unsigned char c = in.data[i];                                         \
        if (c >= 0x80) {                                                      \
            return enif_make_atom(env, "non_ascii");                          \
        }                                                                     \
        out[i] = FN(c);                                                       \
    }                                                                         \
    return out_term;

static ERL_NIF_TERM
lowercase_do(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    ASCII_TRANSFORM_BODY(to_lower_byte)
}

#ifdef ERL_NIF_DIRTY_JOB_CPU_BOUND
static ERL_NIF_TERM
lowercase(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;

    if (argc != 1 || !enif_inspect_binary(env, argv[0], &in)) {
        return enif_make_badarg(env);
    }
    if (in.size >= HB_UTIL_STRING_DIRTY_THRESHOLD) {
        return enif_schedule_nif(
            env, "lowercase_dirty", ERL_NIF_DIRTY_JOB_CPU_BOUND,
            lowercase_do, argc, argv);
    }
    return lowercase_do(env, argc, argv);
}
#else
#define lowercase lowercase_do
#endif

static ERL_NIF_TERM
key_chars(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    ASCII_TRANSFORM_BODY(key_byte)
}

static ERL_NIF_TERM
canon_chars(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    ASCII_TRANSFORM_BODY(canon_byte)
}

/* `dash_chars' is exact for all bytes (`_'<->`-' swap, never folds), so it has
 * no non-ASCII fallback. */
static ERL_NIF_TERM
dash_chars(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;
    ERL_NIF_TERM out_term;
    unsigned char* out;
    size_t i;

    (void)argc;
    if (!enif_inspect_binary(env, argv[0], &in)) {
        return enif_make_badarg(env);
    }
    if (in.size == 0) {
        return argv[0];
    }
    out = enif_make_new_binary(env, in.size, &out_term);
    if (out == NULL) {
        return enif_raise_exception(env, enif_make_atom(env, "enomem"));
    }
    for (i = 0; i < in.size; i++) {
        out[i] = dash_byte(in.data[i]);
    }
    return out_term;
}

/* Stateless: nothing to migrate or initialize, so accepting the upgrade is
 * sufficient. Without this, reloading the module fails with "Upgrade not
 * supported by this NIF library." */
static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    (void)env;
    (void)priv;
    (void)old_priv;
    (void)info;
    return 0;
}

static ErlNifFunc funcs[] = {
    {"lowercase", 1, lowercase, 0},
    {"key_chars", 1, key_chars, 0},
    {"canon_chars", 1, canon_chars, 0},
    {"dash_chars", 1, dash_chars, 0}
};

ERL_NIF_INIT(hb_util_string, funcs, NULL, NULL, upgrade, NULL)
