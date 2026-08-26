/*
 * lib_arweave_index_item: one ANS-104 data item's published-index rows,
 * computed in a single native pass over the item's header window.
 *
 * The scanner (`lib_arweave_index_scan') hands over exactly what its pure
 * Erlang path would parse -- a window binary, the item's absolute weave
 * offset and full size, and the enclosing bundle's ID -- and receives the
 * finished artifacts: the 21-byte offset-index item, the 17-byte match-index
 * items, and whether the item is itself a bundle to recurse into. One call
 * replaces the per-item `ar_bundles:deserialize_header' walk, the RedStone
 * tag check, the sha256 calls for the item ID, owner address and predicate
 * hashes, and the row encoding of `lib_arweave_index_rows'.
 *
 * Parity contract: the byte semantics mirror the Erlang reference
 * (`lib_arweave_index_item:reference/4') clause for clause -- including the
 * deliberate quirks of the vendored parser: a varint hitting end-of-input
 * yields its partial value silently; a zero name-size terminates a tag run
 * early with the declared count unchecked; Avro block form is bounded by its
 * own size and refuses residue; an unsupported signature type fails the
 * parse. Anything this code cannot reproduce byte-exactly -- tag names with
 * bytes >= 0x80 (Unicode case folding), bundle-format/bundle-version values
 * with such bytes, varints in Erlang-bignum territory, solana addresses
 * whose base58 form leaves `hb_util:human_id''s passthrough widths -- comes
 * back as the atom `fallback', and the Erlang side runs the reference for
 * that one item. RedStone items are detected before any of that applies,
 * byte-exactly, and cost no hashing on either path.
 *
 * The module carries no mutable static state; its `upgrade' callback is a
 * no-op so it reloads cleanly under the device-test preloader's code
 * upgrade.
 */
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "erl_nif.h"

#ifdef __APPLE__
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#include <CommonCrypto/CommonDigest.h>
typedef CC_SHA256_CTX sha256_ctx_t;
#define sha256_init(ctx) CC_SHA256_Init(ctx)
#define sha256_update(ctx, data, len) CC_SHA256_Update(ctx, data, (CC_LONG)(len))
#define sha256_final(out, ctx) CC_SHA256_Final(out, ctx)
#else
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#include <openssl/sha.h>
typedef SHA256_CTX sha256_ctx_t;
#define sha256_init(ctx) SHA256_Init(ctx)
#define sha256_update(ctx, data, len) SHA256_Update(ctx, data, len)
#define sha256_final(out, ctx) SHA256_Final(out, ctx)
#endif

/* The ethereum owner addresses hash with the same keccak the `hb_keccak'
 * NIF carries. The source compiles into this port directly -- one copy of
 * the code in the tree, but port-scoped compile flags -- rather than sharing
 * object files between ports. */
#include "../hb_keccak/hb_keccak.c"

/* The row widths and field bounds of `lib_arweave_index_rows'. */
#define OFFSET_ITEM_SIZE 21
#define MATCH_ITEM_SIZE 17
#define OFFSET_BOUND (1ULL << 50)
#define LENGTH_BOUND (1ULL << 34)
#define MATCH_OFFSET_BOUND (1ULL << 49)

/* Parse outcomes. */
#define PARSE_OK 0
#define PARSE_FAILED (-1)
#define PARSE_FALLBACK (-2)

static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_bundle;
static ERL_NIF_TERM am_redstone;
static ERL_NIF_TERM am_failed;
static ERL_NIF_TERM am_fallback;
static ERL_NIF_TERM am_excluded;

/* One decoded tag: name and value slices into the window. */
typedef struct {
    const uint8_t *name;
    size_t name_len;
    const uint8_t *value;
    size_t value_len;
} tag_t;

/* A growable tag vector with inline storage for the common case. */
#define TAGS_INLINE 32
typedef struct {
    tag_t inline_tags[TAGS_INLINE];
    tag_t *tags;
    size_t count;
    size_t capacity;
} tagvec_t;

/* A parsed item header: field slices into the window. */
typedef struct {
    unsigned sig_type;
    const uint8_t *signature;
    size_t signature_len;
    const uint8_t *owner;
    size_t owner_len;
    const uint8_t *target;
    size_t header_size;
} header_t;

static void
tagvec_init(tagvec_t *vec)
{
    vec->tags = vec->inline_tags;
    vec->count = 0;
    vec->capacity = TAGS_INLINE;
}

static void
tagvec_free(tagvec_t *vec)
{
    if (vec->tags != vec->inline_tags) {
        enif_free(vec->tags);
    }
}

static int
tagvec_push(tagvec_t *vec, const uint8_t *name, size_t name_len,
    const uint8_t *value, size_t value_len)
{
    if (vec->count == vec->capacity) {
        size_t grown = vec->capacity * 2;
        tag_t *tags = enif_alloc(grown * sizeof(tag_t));
        if (tags == NULL) {
            return 0;
        }
        memcpy(tags, vec->tags, vec->count * sizeof(tag_t));
        if (vec->tags != vec->inline_tags) {
            enif_free(vec->tags);
        }
        vec->tags = tags;
        vec->capacity = grown;
    }
    vec->tags[vec->count].name = name;
    vec->tags[vec->count].name_len = name_len;
    vec->tags[vec->count].value = value;
    vec->tags[vec->count].value_len = value_len;
    vec->count++;
    return 1;
}

/* Decode one zigzag varint, mirroring `ar_bundles:decode_zigzag/1': a varint
 * cut off by end-of-input yields its partial value silently. Values needing
 * a shift of 63 or more live in Erlang-bignum territory and bail to the
 * reference implementation. */
static int
zigzag(const uint8_t **pp, const uint8_t *end, int64_t *out)
{
    uint64_t result = 0;
    unsigned shift = 0;
    const uint8_t *p = *pp;
    while (p < end) {
        uint8_t byte = *p++;
        if (shift >= 63) {
            return PARSE_FALLBACK;
        }
        result |= (uint64_t)(byte & 0x7F) << shift;
        if ((byte & 0x80) == 0) {
            break;
        }
        shift += 7;
    }
    *pp = p;
    *out = (result & 1) ? -((int64_t)(result >> 1)) - 1
                        : (int64_t)(result >> 1);
    return PARSE_OK;
}

/* Decode up to `count' tags, mirroring `ar_bundles:decode_avro_tags/2': a
 * zero name-size stops the run early with the remaining count unchecked;
 * negative sizes and truncation fail. */
static int
avro_tags(const uint8_t **pp, const uint8_t *end, int64_t count,
    tagvec_t *vec)
{
    while (count > 0) {
        int64_t name_size, value_size;
        const uint8_t *name, *value;
        int rc;
        if (*pp >= end) {
            return PARSE_FAILED;
        }
        if ((rc = zigzag(pp, end, &name_size)) != PARSE_OK) {
            return rc;
        }
        if (name_size == 0) {
            return PARSE_OK;
        }
        if (name_size < 0 || (int64_t)(end - *pp) < name_size) {
            return PARSE_FAILED;
        }
        name = *pp;
        *pp += name_size;
        if ((rc = zigzag(pp, end, &value_size)) != PARSE_OK) {
            return rc;
        }
        if (value_size < 0) {
            return PARSE_FAILED;
        }
        value = *pp;
        if (value_size > 0) {
            if ((int64_t)(end - *pp) < value_size) {
                return PARSE_FAILED;
            }
            *pp += value_size;
        }
        if (!tagvec_push(vec, name, (size_t)name_size,
                value, (size_t)value_size)) {
            /* Allocation failure is not a verdict on the bytes: let the
             * Erlang path decide. */
            return PARSE_FALLBACK;
        }
        count--;
    }
    return PARSE_OK;
}

/* Decode a whole Avro tag section, mirroring
 * `ar_bundles:decode_avro_tag_section/1': blocks until the zero terminator,
 * each either a plain run or a size-bounded block that must consume exactly
 * its own bytes, and the section must be consumed whole. */
static int
avro_section(const uint8_t *section, size_t section_len, tagvec_t *vec)
{
    const uint8_t *p = section;
    const uint8_t *end = section + section_len;
    for (;;) {
        int64_t count;
        int rc;
        if (p >= end) {
            return PARSE_FAILED;
        }
        if ((rc = zigzag(&p, end, &count)) != PARSE_OK) {
            return rc;
        }
        if (count == 0) {
            return (p == end) ? PARSE_OK : PARSE_FAILED;
        } else if (count > 0) {
            if ((rc = avro_tags(&p, end, count, vec)) != PARSE_OK) {
                return rc;
            }
        } else {
            int64_t block_size;
            const uint8_t *block_end;
            if ((rc = zigzag(&p, end, &block_size)) != PARSE_OK) {
                return rc;
            }
            if (block_size < 0 || (int64_t)(end - p) < block_size) {
                return PARSE_FAILED;
            }
            block_end = p + block_size;
            if ((rc = avro_tags(&p, block_end, -count, vec)) != PARSE_OK) {
                return rc;
            }
            if (p != block_end) {
                return PARSE_FAILED;
            }
        }
    }
}

/* Parse one item header from a window, mirroring
 * `ar_bundles:deserialize_header/1' over the same bytes: signature envelope
 * by type, optional target and anchor, then the tag section with its
 * declared count checked against the tags actually decoded. */
static int
parse_header(const uint8_t *win, size_t win_len, header_t *h, tagvec_t *vec)
{
    const uint8_t *p = win;
    const uint8_t *end = win + win_len;
    uint64_t tag_count, tag_size;
    unsigned i;
    int rc;
    if (end - p < 2) {
        return PARSE_FAILED;
    }
    h->sig_type = (unsigned)p[0] | ((unsigned)p[1] << 8);
    switch (h->sig_type) {
        case 1: h->signature_len = 512; h->owner_len = 512; break;
        case 2: h->signature_len = 64; h->owner_len = 32; break;
        case 3: h->signature_len = 65; h->owner_len = 65; break;
        case 4: h->signature_len = 64; h->owner_len = 32; break;
        case 7: h->signature_len = 65; h->owner_len = 42; break;
        default: return PARSE_FAILED;
    }
    p += 2;
    if ((size_t)(end - p) < h->signature_len + h->owner_len) {
        return PARSE_FAILED;
    }
    h->signature = p;
    p += h->signature_len;
    h->owner = p;
    p += h->owner_len;
    /* The two optional fields: target, then anchor. Only target is kept. */
    h->target = NULL;
    for (i = 0; i < 2; i++) {
        if (p >= end) {
            return PARSE_FAILED;
        }
        if (*p == 0) {
            p += 1;
        } else if (*p == 1) {
            if (end - p < 33) {
                return PARSE_FAILED;
            }
            if (i == 0) {
                h->target = p + 1;
            }
            p += 33;
        } else {
            return PARSE_FAILED;
        }
    }
    if (end - p < 16) {
        return PARSE_FAILED;
    }
    tag_count = 0;
    tag_size = 0;
    for (i = 0; i < 8; i++) {
        tag_count |= (uint64_t)p[i] << (8 * i);
        tag_size |= (uint64_t)p[8 + i] << (8 * i);
    }
    p += 16;
    if (tag_count == 0 && tag_size == 0) {
        h->header_size = (size_t)(p - win);
        return PARSE_OK;
    }
    if ((uint64_t)(end - p) < tag_size) {
        return PARSE_FAILED;
    }
    if ((rc = avro_section(p, (size_t)tag_size, vec)) != PARSE_OK) {
        return rc;
    }
    if ((uint64_t)vec->count != tag_count) {
        return PARSE_FAILED;
    }
    p += tag_size;
    h->header_size = (size_t)(p - win);
    return PARSE_OK;
}

/* Whether the tags carry the RedStone oracle signature: all five marker
 * names present, byte-exact (`lib_arweave_index_rows:redstone/1'). */
static int
is_redstone(const tagvec_t *vec)
{
    static const char *markers[] =
        {"dataFeedId", "dataServiceId", "signerAddress", "timestamp", "type"};
    unsigned found = 0;
    size_t i, m;
    for (i = 0; i < vec->count; i++) {
        for (m = 0; m < 5; m++) {
            if ((found & (1U << m)) == 0
                    && vec->tags[i].name_len == strlen(markers[m])
                    && memcmp(vec->tags[i].name, markers[m],
                        vec->tags[i].name_len) == 0) {
                found |= 1U << m;
            }
        }
    }
    return found == 0x1F;
}

static inline uint8_t
lower_byte(uint8_t c)
{
    return (c >= 'A' && c <= 'Z') ? (uint8_t)(c + ('a' - 'A')) : c;
}

/* Case-insensitive ASCII compare of a tag name against a lower-case
 * needle. Callers have established the name is pure ASCII. */
static int
name_is(const tag_t *tag, const char *needle)
{
    size_t len = strlen(needle);
    size_t i;
    if (tag->name_len != len) {
        return 0;
    }
    for (i = 0; i < len; i++) {
        if (lower_byte(tag->name[i]) != (uint8_t)needle[i]) {
            return 0;
        }
    }
    return 1;
}

/* Whether an ASCII-lowered slice equals a lower-case needle. */
static int
lowered_is(const uint8_t *bytes, size_t len, const char *needle)
{
    size_t nlen = strlen(needle);
    size_t i;
    if (len != nlen) {
        return 0;
    }
    for (i = 0; i < len; i++) {
        if (lower_byte(bytes[i]) != (uint8_t)needle[i]) {
            return 0;
        }
    }
    return 1;
}

static int
all_ascii(const uint8_t *bytes, size_t len)
{
    size_t i;
    for (i = 0; i < len; i++) {
        if (bytes[i] >= 0x80) {
            return 0;
        }
    }
    return 1;
}

static void
sha256(const uint8_t *in, size_t len, uint8_t out[32])
{
    sha256_ctx_t ctx;
    sha256_init(&ctx);
    sha256_update(&ctx, in, len);
    sha256_final(out, &ctx);
}

/* base64url without padding, the `hb_util:encode/1' alphabet. */
static size_t
b64url(const uint8_t *in, size_t len, uint8_t *out)
{
    static const char alphabet[] =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
    size_t i = 0, o = 0;
    while (i + 3 <= len) {
        uint32_t v = ((uint32_t)in[i] << 16)
            | ((uint32_t)in[i + 1] << 8) | in[i + 2];
        out[o++] = alphabet[(v >> 18) & 0x3F];
        out[o++] = alphabet[(v >> 12) & 0x3F];
        out[o++] = alphabet[(v >> 6) & 0x3F];
        out[o++] = alphabet[v & 0x3F];
        i += 3;
    }
    if (len - i == 1) {
        uint32_t v = (uint32_t)in[i] << 16;
        out[o++] = alphabet[(v >> 18) & 0x3F];
        out[o++] = alphabet[(v >> 12) & 0x3F];
    } else if (len - i == 2) {
        uint32_t v = ((uint32_t)in[i] << 16) | ((uint32_t)in[i + 1] << 8);
        out[o++] = alphabet[(v >> 18) & 0x3F];
        out[o++] = alphabet[(v >> 12) & 0x3F];
        out[o++] = alphabet[(v >> 6) & 0x3F];
    }
    return o;
}

/* base58, mirroring `hb_util:base58_encode/1': leading zero bytes become
 * `1's, the rest encodes as one big-endian integer. */
static size_t
base58(const uint8_t *in, size_t len, uint8_t *out)
{
    static const char alphabet[] =
        "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";
    uint8_t num[64];
    char reversed[96];
    size_t zeros = 0, o = 0, r = 0, start, i;
    while (zeros < len && in[zeros] == 0) {
        zeros++;
    }
    memcpy(num, in + zeros, len - zeros);
    start = 0;
    while (start < len - zeros) {
        uint32_t rem = 0;
        for (i = start; i < len - zeros; i++) {
            uint32_t acc = (rem << 8) | num[i];
            num[i] = (uint8_t)(acc / 58);
            rem = acc % 58;
        }
        reversed[r++] = alphabet[rem];
        while (start < len - zeros && num[start] == 0) {
            start++;
        }
    }
    for (i = 0; i < zeros; i++) {
        out[o++] = '1';
    }
    while (r > 0) {
        out[o++] = (uint8_t)reversed[--r];
    }
    return o;
}

/* The checksummed ethereum address of a key, mirroring
 * `hb_keccak:key_to_ethereum_address/1': keccak of the key minus its first
 * byte, the last 20 bytes as lower-case hex, and the EIP-55-style checksum
 * from the keccak of that hex string. */
static void
eth_address(const uint8_t *key, size_t key_len, uint8_t out[42])
{
    static const char hex[] = "0123456789abcdef";
    uint8_t key_hash[32], check_hash[32];
    uint8_t addr_hex[40];
    size_t i;
    keccak_256(key_hash, 32, key + 1, key_len - 1);
    for (i = 0; i < 20; i++) {
        addr_hex[2 * i] = (uint8_t)hex[key_hash[12 + i] >> 4];
        addr_hex[2 * i + 1] = (uint8_t)hex[key_hash[12 + i] & 0x0F];
    }
    keccak_256(check_hash, 32, addr_hex, 40);
    out[0] = '0';
    out[1] = 'x';
    for (i = 0; i < 40; i++) {
        uint8_t c = addr_hex[i];
        uint8_t h = (uint8_t)hex[(check_hash[i / 2] >> (i % 2 ? 0 : 4)) & 0x0F];
        if (h >= '8' && c >= 'a') {
            c -= 'a' - 'A';
        }
        out[2 + i] = c;
    }
}

/* Write one match item: the predicate hash prefix over the offset. */
static void
match_item(const uint8_t hash[32], uint64_t offset, uint8_t *row)
{
    uint64_t packed = offset << 7;
    int i;
    memcpy(row, hash, 10);
    for (i = 0; i < 7; i++) {
        row[10 + i] = (uint8_t)(packed >> (8 * (6 - i)));
    }
}

/* Hash `~match@1.0/<lowered key>=<value>' and write the row. The key is
 * ASCII-lowered in bounded chunks, so no scratch allocation scales with
 * the tag. */
static void
predicate_row(const uint8_t *key, size_t key_len, const uint8_t *value,
    size_t value_len, uint64_t offset, uint8_t *row)
{
    sha256_ctx_t ctx;
    uint8_t hash[32];
    uint8_t chunk[256];
    size_t done = 0;
    sha256_init(&ctx);
    sha256_update(&ctx, "~match@1.0/", 11);
    while (done < key_len) {
        size_t take = key_len - done;
        size_t i;
        if (take > sizeof(chunk)) {
            take = sizeof(chunk);
        }
        for (i = 0; i < take; i++) {
            chunk[i] = lower_byte(key[done + i]);
        }
        sha256_update(&ctx, chunk, take);
        done += take;
    }
    sha256_update(&ctx, "=", 1);
    sha256_update(&ctx, value, value_len);
    sha256_final(hash, &ctx);
    match_item(hash, offset, row);
}

/* @doc Compute one item's rows. Arguments: the window binary, the item's
 * absolute weave offset, its full size, and the enclosing bundle's ID as a
 * human-readable binary (empty for none). */
static ERL_NIF_TERM
rows_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary win, parent;
    ErlNifUInt64 offset, size;
    header_t h;
    tagvec_t tags;
    const uint8_t *format_value = NULL, *version_value = NULL;
    size_t format_len = 0, version_len = 0;
    int format_found = 0, version_found = 0, is_bundle;
    uint8_t id[32], owner_hash[32], address[45];
    size_t address_len, i;
    size_t match_count;
    ERL_NIF_TERM offset_term, blob_term, match_list, result;
    uint8_t *blob;
    int rc;
    if (argc != 4
            || !enif_inspect_binary(env, argv[0], &win)
            || !enif_get_uint64(env, argv[1], &offset)
            || !enif_get_uint64(env, argv[2], &size)
            || !enif_inspect_binary(env, argv[3], &parent)) {
        return enif_make_badarg(env);
    }
    tagvec_init(&tags);
    rc = parse_header(win.data, win.size, &h, &tags);
    if (rc != PARSE_OK) {
        tagvec_free(&tags);
        return rc == PARSE_FAILED ? am_failed : am_fallback;
    }
    /* RedStone drops before any lowering or hashing, byte-exactly, so it
     * needs no fallback checks. */
    if (is_redstone(&tags)) {
        tagvec_free(&tags);
        return am_redstone;
    }
    /* Tag names with bytes >= 0x80 reach Unicode case folding on the Erlang
     * path, in the predicate keys and in the bundle-tag search alike. */
    for (i = 0; i < tags.count; i++) {
        if (!all_ascii(tags.tags[i].name, tags.tags[i].name_len)) {
            tagvec_free(&tags);
            return am_fallback;
        }
    }
    /* The first bundle-format and bundle-version tags, matched
     * case-insensitively as `ar_tx:tagfind/3' matches. Their values are
     * case-folded on the Erlang path, so non-ASCII values fall back too. */
    for (i = 0; i < tags.count; i++) {
        if (!format_found && name_is(&tags.tags[i], "bundle-format")) {
            format_found = 1;
            format_value = tags.tags[i].value;
            format_len = tags.tags[i].value_len;
        }
        if (!version_found && name_is(&tags.tags[i], "bundle-version")) {
            version_found = 1;
            version_value = tags.tags[i].value;
            version_len = tags.tags[i].value_len;
        }
    }
    if ((format_found && !all_ascii(format_value, format_len))
            || (version_found && !all_ascii(version_value, version_len))) {
        tagvec_free(&tags);
        return am_fallback;
    }
    is_bundle = format_found && version_found
        && lowered_is(format_value, format_len, "binary")
        && lowered_is(version_value, version_len, "2.0.0");
    /* The owner address, by signature type. */
    switch (h.sig_type) {
        case 1:
        case 2:
            sha256(h.owner, h.owner_len, owner_hash);
            address_len = b64url(owner_hash, 32, address);
            break;
        case 3:
        case 7:
            eth_address(h.owner, h.owner_len, address);
            address_len = 42;
            break;
        default:
            address_len = base58(h.owner, h.owner_len, address);
            if (address_len < 42 || address_len > 44) {
                /* Outside `hb_util:human_id''s passthrough widths; the
                 * Erlang path's behavior is its own. */
                tagvec_free(&tags);
                return am_fallback;
            }
            break;
    }
    sha256(h.signature, h.signature_len, id);
    /* The offset item, or `excluded' when a field overflows. */
    if (offset < OFFSET_BOUND && size < LENGTH_BOUND) {
        uint8_t *item = enif_make_new_binary(env, OFFSET_ITEM_SIZE,
            &offset_term);
        uint64_t high = (2ULL << 20) | (uint64_t)(offset >> 30);
        uint64_t low = ((uint64_t)(offset & ((1ULL << 30) - 1)) << 34)
            | (uint64_t)size;
        memcpy(item, id, 10);
        /* The trailing 88 bits are type:4, offset:50, length:34 packed
         * big-endian: 24 bits of type-and-high-offset, then 64. */
        for (i = 0; i < 3; i++) {
            item[10 + i] = (uint8_t)(high >> (8 * (2 - i)));
        }
        for (i = 0; i < 8; i++) {
            item[13 + i] = (uint8_t)(low >> (8 * (7 - i)));
        }
    } else {
        offset_term = am_excluded;
    }
    /* The match rows: every tag, the owner, the recipient when a target is
     * present, and the enclosing bundle. All share the item's offset, so one
     * bound check drops them together. */
    match_count = 0;
    if (offset < MATCH_OFFSET_BOUND) {
        match_count = tags.count + 1 + (h.target != NULL ? 1 : 0)
            + (parent.size > 0 ? 1 : 0);
    }
    blob = enif_make_new_binary(env, match_count * MATCH_ITEM_SIZE,
        &blob_term);
    if (match_count > 0) {
        uint8_t *row = blob;
        sha256_ctx_t ctx;
        uint8_t hash[32];
        for (i = 0; i < tags.count; i++) {
            predicate_row(tags.tags[i].name, tags.tags[i].name_len,
                tags.tags[i].value, tags.tags[i].value_len, offset, row);
            row += MATCH_ITEM_SIZE;
        }
        sha256_init(&ctx);
        sha256_update(&ctx, "~match@1.0/owner=", 17);
        sha256_update(&ctx, address, address_len);
        sha256_final(hash, &ctx);
        match_item(hash, offset, row);
        row += MATCH_ITEM_SIZE;
        if (h.target != NULL) {
            uint8_t recipient[43];
            b64url(h.target, 32, recipient);
            sha256_init(&ctx);
            sha256_update(&ctx, "~match@1.0/recipient=", 21);
            sha256_update(&ctx, recipient, 43);
            sha256_final(hash, &ctx);
            match_item(hash, offset, row);
            row += MATCH_ITEM_SIZE;
        }
        if (parent.size > 0) {
            sha256_init(&ctx);
            sha256_update(&ctx, "~match@1.0/bundled-in=", 22);
            sha256_update(&ctx, parent.data, parent.size);
            sha256_final(hash, &ctx);
            match_item(hash, offset, row);
        }
    }
    tagvec_free(&tags);
    /* The match rows surface as the sink's list of 17-byte binaries: cheap
     * sub-binaries of the one blob, built back to front. */
    match_list = enif_make_list(env, 0);
    for (i = match_count; i > 0; i--) {
        match_list = enif_make_list_cell(env,
            enif_make_sub_binary(env, blob_term,
                (i - 1) * MATCH_ITEM_SIZE, MATCH_ITEM_SIZE),
            match_list);
    }
    if (is_bundle) {
        ERL_NIF_TERM id_term;
        uint8_t *id_out = enif_make_new_binary(env, 43, &id_term);
        b64url(id, 32, id_out);
        result = enif_make_tuple5(env, am_bundle, offset_term, match_list,
            enif_make_uint64(env, (ErlNifUInt64)h.header_size), id_term);
    } else {
        result = enif_make_tuple3(env, am_ok, offset_term, match_list);
    }
    return result;
}

static int
load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    am_ok = enif_make_atom(env, "ok");
    am_bundle = enif_make_atom(env, "bundle");
    am_redstone = enif_make_atom(env, "redstone");
    am_failed = enif_make_atom(env, "failed");
    am_fallback = enif_make_atom(env, "fallback");
    am_excluded = enif_make_atom(env, "excluded");
    return 0;
}

static int
upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data,
    ERL_NIF_TERM load_info)
{
    return load(env, priv_data, load_info);
}

static ErlNifFunc nif_funcs[] = {
    {"rows_nif", 4, rows_nif, 0},
    {"rows_dirty_nif", 4, rows_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(lib_arweave_index_item, nif_funcs, load, NULL, upgrade, NULL)
