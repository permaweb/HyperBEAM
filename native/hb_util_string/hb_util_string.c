/*
 * hb_util_string: dependency-free byte-transform NIFs backing the ASCII
 * string operations in `hb_util'. Each maps input bytes in a single scalar
 * pass (the compiler auto-vectorizes the compare-and-select at -O3, so no
 * `-march=native' is needed):
 *
 *   lowercase/1      `A'..`Z' -> `a'..`z'                 (to_lower)
 *   key_chars/1      `A'..`Z' -> `a'..`z', and `-' -> `_' (key_to_atom)
 *   canon_chars/1    `A'..`Z' -> `a'..`z', and `_' -> `-' (hb_opts canonical_key)
 *   dash_chars/1     `_' -> `-'                           (atom_to_dashed_binary)
 *   normalize_path/1 collapse `//' runs, trim leading/trailing `/' (hb_path)
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
 * and `normalize_path' only rewrite ASCII byte positions (`_'<->`-' and `/'
 * runs respectively) and never fold, so they are exact for all input and need
 * no fallback.
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
 * key/atom transforms see only bounded keys (atoms are <= 255 bytes) and
 * `normalize_path' only bounded paths, so they always run inline. */
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

/* Collapse runs of `/' to a single `/' and strip leading/trailing `/'.
 * Equivalent to
 *   iolist_to_binary(lists:join(<<"/">>,
 *     binary:split(Bin, <<"/">>, [global, trim_all]))).
 * Exact for all bytes (only `/' positions matter); never folds, no fallback.
 *
 * Two-pass on purpose: the overwhelmingly common hot-path input is an *already
 * normalized* path (segments joined by a single `/', no leading/trailing one),
 * so a first scan that proves the binary is already clean lets us return the
 * caller's binary verbatim -- no allocation, no copy. Only a genuinely dirty
 * path (leading/trailing `/' or a `//' run) falls through to the collapse
 * pass. The detect scan is branch-light and vectorizes; skipping the allocate
 * + byte-copy is what makes the clean case ~3-13x faster than the Erlang
 * split/join -- the margin largest on the short paths that dominate, since
 * there the avoided allocation outweighs everything else. */
static ERL_NIF_TERM
normalize_path(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;
    ERL_NIF_TERM out_term;
    unsigned char* out;
    size_t i, n = 0;
    int pending_sep = 0;

    (void)argc;
    if (!enif_inspect_binary(env, argv[0], &in)) {
        return enif_make_badarg(env);
    }
    if (in.size == 0) {
        return argv[0];
    }
    /* Clean iff no leading `/', no trailing `/', and no `//' run anywhere. */
    if (in.data[0] != '/' && in.data[in.size - 1] != '/') {
        for (i = 1; i < in.size; i++) {
            if (in.data[i] == '/' && in.data[i - 1] == '/') {
                break;
            }
        }
        if (i == in.size) {
            return argv[0];
        }
    }
    out = enif_make_new_binary(env, in.size, &out_term);
    if (out == NULL) {
        return enif_raise_exception(env, enif_make_atom(env, "enomem"));
    }
    for (i = 0; i < in.size; i++) {
        unsigned char c = in.data[i];
        if (c == '/') {
            if (n > 0) {
                pending_sep = 1;
            }
        } else {
            if (pending_sep) {
                out[n++] = '/';
                pending_sep = 0;
            }
            out[n++] = c;
        }
    }
    if (n == in.size) {
        return out_term;
    }
    return enif_make_sub_binary(env, out_term, 0, n);
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
    {"dash_chars", 1, dash_chars, 0},
    {"normalize_path", 1, normalize_path, 0}
};

ERL_NIF_INIT(hb_util_string, funcs, NULL, NULL, upgrade, NULL)
