#include "../include/hb_convert.h"

#include "hb_logging.h"
#include "wasm_c_api.h"
#include <ei.h>
#include <stdint.h>

extern ErlDrvTermData atom_execution_result;

// Unions used for bit casting from unsigned to signed ints
union sign_i32 {
    uint32_t u;
    int32_t s;
};
union sign_i64 {
    uint64_t u;
    int64_t s;
};

enum erl_port_buffer_to_wasm_vals_rc erl_port_buffer_to_wasm_vals(const char* buff, int* index, wasm_valkind_t *val_kinds, uint32_t val_count, wasm_val_t **out_vals) {
    int rc = ERL_PORT_BUFFER_TO_WASM_VALS_UNSPECIFIED_ERROR; // General failure

    if (!buff || !index || !val_kinds || !val_count || !out_vals) {
        DRV_DEBUG("Invalid arguments");
        rc = ERL_PORT_BUFFER_TO_WASM_VALS_INVALID_ARGUMENTS;
        goto fail0;
    }

    int arity, type, res;

    if(ei_get_type(buff, index, &type, &arity) == -1) {
        DRV_DEBUG("Failed to get type");
        rc = ERL_PORT_BUFFER_TO_WASM_VALS_MALFORMED_BUFFER;
        goto fail0;
    }
    DRV_DEBUG("Decoded buffer header. Arity: %d", arity);
    
    // Strings are lists of I32s, so we need to handle them separately
    if(type != ERL_LIST_EXT && type != ERL_STRING_EXT) {
        DRV_DEBUG("Port buffer is not a list or string, type: %c", type);
        rc = ERL_PORT_BUFFER_TO_WASM_VALS_MALFORMED_BUFFER;
        goto fail0;
    }

    // In either case, the arity must match the val_count
    if (arity != val_count) {
        DRV_DEBUG("Arity mismatch. Port buffer arity: %d, Wasm val count: %d", arity, val_count);
        rc = ERL_PORT_BUFFER_TO_WASM_VALS_MALFORMED_BUFFER;
        goto fail0;
    }

    wasm_val_t* vals = NULL;

    if (type == ERL_LIST_EXT) {
        DRV_DEBUG("Decoding list");

        /* Consume the list header so that the index now points at the
            * first element. */
        if(ei_decode_list_header(buff, index, &arity) != 0) {
            DRV_DEBUG("Failed to decode list header");
            rc = ERL_PORT_BUFFER_TO_WASM_VALS_MALFORMED_BUFFER;
            goto fail0;
        }

        vals = driver_alloc(sizeof(wasm_val_t) * val_count);

        for(int i = 0; i < arity; i++) {
            int elem_type, elem_size;
            ei_get_type(buff, index, &elem_type, &elem_size);
            DRV_DEBUG("Decoded elem[%d] type: %c, size: %d", i, elem_type, elem_size);

            wasm_valkind_t val_kind = val_kinds[i];
            if (val_kind != WASM_I32 && val_kind != WASM_I64 && val_kind != WASM_F32 && val_kind != WASM_F64) {
                DRV_DEBUG("Unsupported wasm type: %d", val_kind);
                rc = ERL_PORT_BUFFER_TO_WASM_VALS_UNSUPPORTED_WASM_TYPE;
                goto fail1;
            }
            vals[i].kind = val_kind;
        
            // Convert based on the erl term
            switch (elem_type) {
                DRV_DEBUG("Decoding elem %d: %d", i, elem_type);

                /* 32-bit (or smaller) integers */
                case ERL_SMALL_INTEGER_EXT:
                case ERL_INTEGER_EXT: {
                    // Allowed for all supported wasm types
                    if (val_kind != WASM_I32 && val_kind != WASM_I64 && val_kind != WASM_F32 && val_kind != WASM_F64) {
                        DRV_DEBUG("Unsupported wasm type for ERL_SMALL_INTEGER_EXT/ERL_INTEGER_EXT: %d", val_kind);
                        rc = ERL_PORT_BUFFER_TO_WASM_VALS_UNSUPPORTED_WASM_TYPE;
                        goto fail1;
                    }

                    // Decode the erl term
                    long val_l;
                    unsigned long val_ul;
                    if (ei_decode_long(buff, index, &val_l) == 0){
                        switch (val_kind) {
                            case WASM_I32:
                                vals[i].of.i32 = (int32_t)val_l;
                                break;
                            case WASM_I64:
                                vals[i].of.i64 = (int64_t)val_l;
                                break;
                            case WASM_F32:
                                vals[i].of.f32 = (float)val_l;
                                break;
                            case WASM_F64:
                                vals[i].of.f64 = (double)val_l;
                                break;
                            default:
                                DRV_DEBUG("Unexpected wasm type for ERL_SMALL_INTEGER_EXT/ERL_INTEGER_EXT: %d", val_kind);
                                rc = ERL_PORT_BUFFER_TO_WASM_VALS_UNSUPPORTED_WASM_TYPE;
                                goto fail1;
                        }
                        break;
                    } else if (ei_decode_ulong(buff, index, &val_ul) == 0) {
                        switch (val_kind) {
                            case WASM_I32:
                                // This is outside the range of a signed int32,
                                // but inside the range of an unsigned uint32, so 
                                // we bitcast it
                                union sign_i32 sign_val;
                                sign_val.u = val_ul;
                                vals[i].of.i32 = sign_val.s;
                                break;
                            case WASM_I64:
                                vals[i].of.i64 = (int64_t)val_ul;
                                break;
                            case WASM_F32:
                                vals[i].of.f32 = (float)val_ul;
                                break;
                            case WASM_F64:
                                vals[i].of.f64 = (double)val_ul;
                                break;
                            default:
                                DRV_DEBUG("Unexpected wasm type for ERL_SMALL_INTEGER_EXT/ERL_INTEGER_EXT: %d", val_kind);
                                rc = ERL_PORT_BUFFER_TO_WASM_VALS_UNSUPPORTED_WASM_TYPE;
                                goto fail1;
                        }
                        break;
                    } else {
                        DRV_DEBUG("Failed to decode ERL_SMALL_INTEGER_EXT/ERL_INTEGER_EXT at pos %d", i);
                        rc = ERL_PORT_BUFFER_TO_WASM_VALS_MALFORMED_BUFFER;
                        goto fail1;
                    }
                }

                /* 64-bit integers are encoded as SMALL BIGs */
                case ERL_SMALL_BIG_EXT:
                case ERL_LARGE_BIG_EXT: {
                    // Allowed for all supported wasm types, however, I32 should be bounds checked
                    if (val_kind != WASM_I32 && val_kind != WASM_I64 && val_kind != WASM_F32 && val_kind != WASM_F64) {
                        DRV_DEBUG("Unsupported wasm type for ERL_SMALL_INTEGER_EXT/ERL_INTEGER_EXT: %d", val_kind);
                        rc = ERL_PORT_BUFFER_TO_WASM_VALS_UNSUPPORTED_WASM_TYPE;
                        goto fail1;
                    }

                    EI_LONGLONG val_ll;
                    EI_ULONGLONG val_ull;
                    if (ei_decode_longlong(buff, index, &val_ll) == 0) {
                        switch (val_kind) {
                            case WASM_I32:
                                if (val_ll <= INT32_MAX && val_ll >= INT32_MIN) {
                                    // Valid int32, set directly
                                    vals[i].of.i32 = (int32_t)val_ll;
                                } else if (val_ll >= 0 && val_ll <= UINT32_MAX) {
                                    // Valid uint, bitcast it
                                    union sign_i32 sign_val;
                                    sign_val.u = (uint32_t)val_ll;
                                    vals[i].of.i32 = sign_val.s;
                                } else {
                                    DRV_DEBUG("I32 out of range: %lld", val_ll);
                                    rc = ERL_PORT_BUFFER_TO_WASM_VALS_VALUE_OUT_OF_RANGE;
                                    goto fail1;
                                }
                                break;
                            case WASM_I64:
                                vals[i].of.i64 = (int64_t)val_ll;
                                break;
                            case WASM_F32:
                                vals[i].of.f32 = (float)val_ll;
                                break;
                            case WASM_F64:
                                vals[i].of.f64 = (double)val_ll;
                                break;
                            default:
                                DRV_DEBUG("Unexpected wasm type for ERL_SMALL_BIG_EXT/ERL_LARGE_BIG_EXT: %d", val_kind);
                                goto fail1;
                        }
                        DRV_DEBUG("Decoded BIG int %d: %lld", i, (long long)val_ll);
                    } else if (ei_decode_ulonglong(buff, index, &val_ull) == 0) {
                        switch (val_kind) {
                            case WASM_I32:
                                // If it decodes as a ulonglong, that means it's definitely out of bounds.
                                DRV_DEBUG("I32 out of range: %llu", val_ull);
                                rc = ERL_PORT_BUFFER_TO_WASM_VALS_VALUE_OUT_OF_RANGE;
                                goto fail1;
                                break;
                            case WASM_I64:
                                // This is outside the range of a signed int64,
                                // but inside the range of an unsigned uint64, so 
                                // we bitcast it
                                union sign_i64 sign_val;
                                sign_val.u = val_ull;
                                vals[i].of.i64 = sign_val.s;
                                break;
                            case WASM_F32:
                                vals[i].of.f32 = (float)val_ull;
                                break;
                            case WASM_F64:
                                vals[i].of.f64 = (double)val_ull;
                                break;
                            default:
                                DRV_DEBUG("Unexpected wasm type for ERL_SMALL_BIG_EXT/ERL_LARGE_BIG_EXT: %d", val_kind);
                                rc = ERL_PORT_BUFFER_TO_WASM_VALS_UNSUPPORTED_WASM_TYPE;
                                goto fail1;
                        }
                        DRV_DEBUG("Decoded ULONGBIG int %d: %llu", i, val_ull);
                    } else {
                        DRV_DEBUG("Failed to decode ERL_SMALL_BIG_EXT/ERL_LARGE_BIG_EXT at pos %d", i);
                        // This can happen if the value is too large to fit in a (u)longlong
                        rc = ERL_PORT_BUFFER_TO_WASM_VALS_VALUE_OUT_OF_RANGE;
                        goto fail1;
                    }
                    break;
                }

                /* Floats */
                case ERL_FLOAT_EXT:
                case NEW_FLOAT_EXT: {
                    // Here we only support f32 and f64
                    // We could do some conversion, but better to coerce it on the erlang side
                    if (val_kind != WASM_F32 && val_kind != WASM_F64) {
                        DRV_DEBUG("Unsupported wasm type for ERL_FLOAT_EXT/NEW_FLOAT_EXT: %d", val_kind);
                        rc = ERL_PORT_BUFFER_TO_WASM_VALS_UNSUPPORTED_WASM_TYPE;
                        goto fail1;
                    }
                    
                    double dval;
                    if (ei_decode_double(buff, index, &dval) == 0) {
                        switch (val_kind) {
                            case WASM_F32:
                                // Rely on system downcasting.
                                // TODO: Confirm this is 100% portable?
                                vals[i].of.f32 = (float)dval;
                                break;
                            case WASM_F64:
                                vals[i].of.f64 = dval;
                                break;
                            default:
                                DRV_DEBUG("Unexpected wasm type for ERL_FLOAT_EXT/NEW_FLOAT_EXT: %d", val_kind);
                                rc = ERL_PORT_BUFFER_TO_WASM_VALS_UNSUPPORTED_WASM_TYPE;
                                goto fail1;
                        }
                    } else {
                        DRV_DEBUG("Failed to decode ERL_FLOAT_EXT/NEW_FLOAT_EXT at pos %d", i);
                        rc = ERL_PORT_BUFFER_TO_WASM_VALS_MALFORMED_BUFFER;
                        goto fail1;
                    }
                    break;
                }

                default:
                    DRV_DEBUG("Unsupported erl term type: %d", elem_type);
                    rc = ERL_PORT_BUFFER_TO_WASM_VALS_MALFORMED_BUFFER;
                    goto fail1;
            }
        }

        /* Skip list tail (usually NIL) so that the caller's index points
            past the list. We don't care about its value, only that it is
            a valid term. */
        ei_skip_term(buff, index);
    } else if (type == ERL_STRING_EXT) {
        DRV_DEBUG("Decoding string");

        char* str = driver_alloc(arity * sizeof(char) + 1);
        if (!str) {
            DRV_DEBUG("Failed to allocate string");
            // Use generic error code
            goto fail1;
        }
        
        if (ei_decode_string(buff, index, str) != 0) {
            DRV_DEBUG("Failed to decode string");
            driver_free(str);
            rc = ERL_PORT_BUFFER_TO_WASM_VALS_MALFORMED_BUFFER;
            goto fail1;
        }

        vals = driver_alloc(sizeof(wasm_val_t) * arity);

        for(int i = 0; i < arity; i++) {
            wasm_valkind_t val_kind = val_kinds[i];
            if (val_kind != WASM_I32 && val_kind != WASM_I64 && val_kind != WASM_F32 && val_kind != WASM_F64) {
                DRV_DEBUG("Unsupported wasm type: %d", val_kind);
                rc = ERL_PORT_BUFFER_TO_WASM_VALS_UNSUPPORTED_WASM_TYPE;
                goto fail1;
            }
            vals[i].kind = val_kind;

            // We can convert to any type
            switch (val_kind) {
                case WASM_I32:
                    // Bitcast to i32
                    union sign_i32 sign_val;
                    sign_val.u = (uint32_t)str[i];
                    vals[i].of.i32 = sign_val.s;
                    break;
                case WASM_I64:
                    vals[i].of.i64 = (int64_t)str[i];
                    break;
                case WASM_F32:
                    vals[i].of.f32 = (float)str[i];
                    break;
                case WASM_F64:
                    vals[i].of.f64 = (double)str[i];
                    break;
                default:
                    DRV_DEBUG("Unsupported wasm type: %d", val_kind);
                    rc = ERL_PORT_BUFFER_TO_WASM_VALS_UNSUPPORTED_WASM_TYPE;
                    goto fail1;
            }
        }
        driver_free(str);
    }

    *out_vals = vals;
    rc = ERL_PORT_BUFFER_TO_WASM_VALS_SUCCESS;
    return rc;

fail1:
    driver_free(vals);

fail0:
    DRV_DEBUG("Failed to convert erl port buffer to wasm vals");
    return rc;
}

int wasm_val_to_erl_term(ErlDrvTermData* term, const wasm_val_t* val) {
    DRV_DEBUG("Adding wasm val to erl term");
    switch (val->kind) {
        case WASM_I32:
            term[0] = ERL_DRV_INT;
            term[1] = val->of.i32;
            return 2;
        case WASM_I64:
            term[0] = ERL_DRV_INT64;
            term[1] = (ErlDrvTermData) &val->of.i64;
            return 2;
        case WASM_F32:
            term[0] = ERL_DRV_FLOAT;
            term[1] = (ErlDrvTermData) &val->of.f32;
            return 2;
        case WASM_F64:
            term[0] = ERL_DRV_FLOAT;
            term[1] = (ErlDrvTermData) &val->of.f64;
            return 2;
        default:
            DRV_DEBUG("Unsupported result type: %d", val->kind);
            return 0;
    }
}

int wasm_vals_to_erl_msg(const wasm_val_t* vals, const int val_count, ErlDrvTermData** msg, int* msg_i, int msg_base_size) {
    if (!vals || !val_count || !msg || !msg_i) {
        goto fail0;
    }
    
    // Send the results back to Erlang
    DRV_DEBUG("Results size: %d", val_count);
    DRV_DEBUG("Reallocating msg");
    *msg = driver_realloc(*msg, sizeof(ErlDrvTermData) * (
        msg_base_size +
        (val_count * 2) + // Each term is 2 ErlDrvTermData
        3
    ));
    for (size_t i = 0; i < val_count; i++) {
        int res_size = wasm_val_to_erl_term(&(*msg)[*msg_i], &vals[i]);
        assert(res_size == 2);
        *msg_i += res_size;
    }

    (*msg)[(*msg_i)++] = ERL_DRV_NIL;
    (*msg)[(*msg_i)++] = ERL_DRV_LIST;
    (*msg)[(*msg_i)++] = val_count + 1;

    DRV_DEBUG("msg: %p", *msg);
    DRV_DEBUG("msg_i: %d", *msg_i);
    DRV_DEBUG("val_count: %d", val_count);

    return 0;

fail1:
    driver_free(msg);

fail0:
    DRV_DEBUG("Failed to convert wasm vals to erl port buffer");
    return -1;
}

int wasm_val_kinds_to_erl_msg(const wasm_valkind_t* val_kinds, const int val_count, ErlDrvTermData** msg, int* msg_i, int msg_base_size) {

}
