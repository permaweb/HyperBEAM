#include "../include/hb_helpers.h"
#include "../include/hb_logging.h"
#include "wasm_c_api.h"
#include "wasm_export.h"
#include <stdint.h>

// Returns the string name corresponding to the wasm type
const char* get_wasm_type_name(wasm_valkind_t kind) {
    switch (kind) {
        case WASM_I32: return "i32";
        case WASM_I64: return "i64";
        case WASM_F32: return "f32";
        case WASM_F64: return "f64";
        default: return "unknown";
    }
}

const char* wasm_import_export_kind_to_string(wasm_import_export_kind_t kind) {
    switch (kind) {
        case WASM_EXTERN_FUNC: return "func";
        case WASM_EXTERN_GLOBAL: return "global";
        case WASM_EXTERN_TABLE: return "table";
        case WASM_EXTERN_MEMORY: return "memory";
        default: return "unknown";
    }
}

// Helper function to convert wasm_valtype_t to char
char wasm_valkind_to_char(enum wasm_valkind_enum valkind_enum) {
    switch (valkind_enum) {
        case WASM_I32: return 'i';
        case WASM_I64: return 'I';
        case WASM_F32: return 'f';
        case WASM_F64: return 'F';
        case WASM_EXTERNREF: return 'e';
        case WASM_V128: return 'v';
        case WASM_FUNCREF: return 'f';
        default: return 'u';
    }
}

int wasm_val_to_erl_term(ErlDrvTermData* term, const wasm_val_t* val) {
    DRV_DEBUG("Adding wasm val to erl term");
    DRV_DEBUG("Val of: %d", val->of.i32);
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

int import_arg_to_erl_term(ErlDrvTermData* term, enum wasm_valkind_enum kind, uint64_t* arg_ptr) {
    DRV_DEBUG("Adding wasm val to erl term");
    DRV_DEBUG("Val of type %d: %d", kind, *arg_ptr);
    switch (kind) {
        case WASM_I32:
            term[0] = ERL_DRV_INT;
            term[1] = (ErlDrvTermData)(int32_t)(*arg_ptr);
            return 2;
        case WASM_I64:
            term[0] = ERL_DRV_INT64;
            term[1] = (ErlDrvTermData)(int64_t *)(arg_ptr);
            return 2;
        case WASM_F32:
            term[0] = ERL_DRV_FLOAT;
            term[1] = (ErlDrvTermData)(float *)(arg_ptr);
            return 2;
        case WASM_F64:
            term[0] = ERL_DRV_FLOAT;
            term[1] = (ErlDrvTermData)(double *)(arg_ptr);
            return 2;
        default:
            DRV_DEBUG("Unsupported result type: %d", kind);
            return 0;
    }
}


int erl_term_to_wasm_val(wasm_val_t* val, ei_term* term) {
    switch (val->kind) {
        case WASM_I32:
            DRV_DEBUG("Converting erl term to wasm i32. Term: %d. Size: %d", term->value.i_val, term->size);
            val->of.i32 = (int32_t) term->value.i_val;
            break;
        case WASM_I64:
            DRV_DEBUG("Converting erl term to wasm i64. Term: %lld. Size: %d", term->value.i_val, term->size);
            val->of.i64 = (int64_t) term->value.i_val;
            break;
        case WASM_F32:
            DRV_DEBUG("Converting erl term to wasm f32. Term: %f. Size: %d", term->value.d_val, term->size);
            val->of.f32 = term->value.d_val;
            break;
        case WASM_F64:
            DRV_DEBUG("Converting erl term to wasm f64. Term: %f. Size: %d", term->value.d_val, term->size);
            val->of.f64 = term->value.d_val;
            break;
        default:
            DRV_DEBUG("Unsupported parameter type: %d", val->kind);
            return -1;
    }
    return 0;
}

int erl_terms_to_wasm_vals(wasm_val_vec_t* vals, ei_term* terms) {
    DRV_DEBUG("Converting erl terms to wasm vals");
    DRV_DEBUG("Vals: %d", vals->size);
    for(int i = 0; i < vals->size; i++) {
        DRV_DEBUG("Converting term %d: %p", i, &vals->data[i]);
        int res = erl_term_to_wasm_val(&vals->data[i], &terms[i]);
        if(res == -1) {
            DRV_DEBUG("Failed to convert term to wasm val");
            return -1;
        }
    }
    return 0;
}

int erl_term_to_indirect_arg(uint32_t* val, enum wasm_valkind_enum val_kind, ei_term* term) {
    DRV_DEBUG("Converting erl term to indirect arg. Kind: %d. Term: %d. Size: %d", val_kind, term->value.i_val, term->size);
    switch (val_kind) {
        case WASM_I32:
            *val = (uint32_t)(term->value.i_val);
            return 1;
        case WASM_I64:
            *(val) = (uint32_t)((int64_t)term->value.i_val & 0xFFFFFFFF);
            *(val + 1) = (uint32_t)(((int64_t)term->value.i_val) >> 32);
            return 2;
        case WASM_F32:
            *val = (uint32_t)(term->value.i_val);
            return 1;
        case WASM_F64:
            *val = (uint32_t)(term->value.i_val & 0xFFFFFFFF);
            *(val + 1) = (uint32_t)(term->value.i_val >> 32);
            return 2;
        default:
            DRV_DEBUG("Unsupported parameter type: %d", val_kind);
            return -1;
    }
}

int erl_terms_to_indirect_args(uint32_t* vals, wasm_valkind_t *val_kinds, uint32_t val_count, ei_term* terms) {
    DRV_DEBUG("Converting erl terms to indirect args");
    DRV_DEBUG("Vals: %d", val_count);
    int i = 0, pos = 0;
    for (i = 0; i < val_count; i++) {
        DRV_DEBUG("Converting term %d: %p", i, &vals[i]);
        int res = erl_term_to_indirect_arg(&vals[pos], val_kinds[i], &terms[i]);
        if(res == -1) {
            DRV_DEBUG("Failed to convert term to indirect arg");
            return -1;
        }
        pos += res;
    }
    return 0;
}

int erl_term_to_import_result(enum wasm_valkind_enum val_kind, uint64_t* val, ei_term* term) {
    DRV_DEBUG("Converting erl term to wasm val. Term: %d. Size: %d", term->value.i_val, term->size);
    switch (val_kind) {
        case WASM_I32:
            *val = (int) term->value.i_val;
            break;
        case WASM_I64:
            *val = (int64_t) term->value.i_val;
            break;
        case WASM_F32:
            *val = (float) term->value.d_val;
            break;
        case WASM_F64:
            *val = term->value.d_val;
            break;
        default:
            DRV_DEBUG("Unsupported parameter type: %d", val_kind);
            return -1;
    }
    return 0;
}

int erl_terms_to_import_results(uint32_t val_count, wasm_valkind_t *val_kinds, uint64_t* vals, ei_term* terms) {
    DRV_DEBUG("Converting erl terms to wasm vals");
    DRV_DEBUG("Vals: %d", val_count);
    for(int i = 0; i < val_count; i++) {
        DRV_DEBUG("Converting term %d: %p", i, &vals[i]);
        int res = erl_term_to_import_result((enum wasm_valkind_enum)val_kinds[i], &vals[i], &terms[i]);
        if(res == -1) {
            DRV_DEBUG("Failed to convert term to wasm val");
            return -1;
        }
    }
    return 0;
}

int kind_size(enum wasm_valkind_enum kind) {
    switch (kind) {
        case WASM_I32: return 1;
        case WASM_I64: return 2;
        case WASM_F32: return 1;
        case WASM_F64: return 2;
        default: {
            DRV_DEBUG("kind_size: unsupported kind: %d", kind);
            return 0;
        }
    }
}

int kinds_size(wasm_valkind_t *kinds, int count) {
    int size = 0;
    for(int i = 0; i < count; i++) {
        size += kind_size(kinds[i]);
    }
    return size;
}

int decode_list(char* buff, int* index, ei_term** out_terms) {
    int arity, type, res;

    if(ei_get_type(buff, index, &type, &arity) == -1) {
        DRV_DEBUG("Failed to get type");
        return -1;
    }
    DRV_DEBUG("Decoded header. Arity: %d", arity);

    *out_terms = driver_alloc(sizeof(ei_term) * arity);

    if(type == ERL_LIST_EXT) {
        /* Consume the list header so that the index now points at the
         * first element. */
        ei_decode_list_header(buff, index, &arity);

        for(int i = 0; i < arity; i++) {
            int elem_type, elem_size;
            ei_get_type(buff, index, &elem_type, &elem_size);

            switch (elem_type) {
                DRV_DEBUG("Decoding elem %d: %d", i, elem_type);

                /* 32-bit (or smaller) integers */
                case ERL_SMALL_INTEGER_EXT:
                case ERL_INTEGER_EXT: {
                    long val;
                    unsigned long val_ull;
                    if (ei_decode_long(buff, index, &val) == 0){
                        (*out_terms)[i].ei_type = ERL_INTEGER_EXT;
                        (*out_terms)[i].arity = 0;
                        (*out_terms)[i].size = 0;
                        (*out_terms)[i].value.i_val = val;
                        DRV_DEBUG("Decoded SMALL/LONG int %d: %ld", i, val);
                        break;
                    } else if (ei_decode_ulong(buff, index, &val_ull) == 0) {
                        (*out_terms)[i].ei_type = ERL_INTEGER_EXT;
                        (*out_terms)[i].arity = 0;
                        (*out_terms)[i].size = 0;
                        (*out_terms)[i].value.i_val = val_ull;
                        DRV_DEBUG("Decoded ULONG int %d: %lu", i, val_ull);
                        break;
                    } else {
                        DRV_DEBUG("Failed to decode ERL_SMALL_INTEGER_EXT/ERL_INTEGER_EXT at pos %d", i);
                        return -1;
                    }
                }

                /* 64-bit integers are encoded as SMALL BIGs */
                case ERL_SMALL_BIG_EXT:
                case ERL_LARGE_BIG_EXT: {
                    EI_LONGLONG val_ll;
                    EI_ULONGLONG val_ull;
                    if (ei_decode_longlong(buff, index, &val_ll) == 0) {
                        (*out_terms)[i].ei_type = ERL_INTEGER_EXT;
                        (*out_terms)[i].arity = 0;
                        (*out_terms)[i].size = 0;
                        (*out_terms)[i].value.i_val = val_ll; /* long is 64-bit on modern 64-bit platforms */
                        DRV_DEBUG("Decoded BIG int %d: %lld", i, (long long)val_ll);
                    } else if (ei_decode_ulonglong(buff, index, &val_ull) == 0) {
                        /* This is outside of Erlang's 64-bit signed integer range. */
                        /* Instead, we need to decode it as ulonglong */
                        (*out_terms)[i].ei_type = ERL_INTEGER_EXT;
                        (*out_terms)[i].arity = 0;
                        (*out_terms)[i].size = 0;
                        (*out_terms)[i].value.i_val = val_ull;
                        DRV_DEBUG("Decoded ULONGBIG int %d: %llu", i, val_ull);
                    } else {
                        DRV_DEBUG("Failed to decode ERL_SMALL_BIG_EXT/ERL_LARGE_BIG_EXT at pos %d", i);
                        return -1;
                    }
                    break;
                }

                /* Floats */
                case ERL_FLOAT_EXT:
                case NEW_FLOAT_EXT: {
                    double dval;
                    ei_decode_double(buff, index, &dval);
                    (*out_terms)[i].ei_type = ERL_FLOAT_EXT;
                    (*out_terms)[i].arity = 0;
                    (*out_terms)[i].size = 0;
                    (*out_terms)[i].value.d_val = dval;
                    DRV_DEBUG("Decoded float as double %d: %f", i, dval);
                    break;
                }

                /* Strings come over as lists encoded as string ext when args
                   are binaries – keep previous behaviour. */
                case ERL_STRING_EXT: {
                    unsigned char* str = driver_alloc(elem_size * sizeof(char) + 1);
                    ei_decode_string(buff, index, (char*)str);
                    (*out_terms)[i].ei_type = ERL_STRING_EXT;
                    (*out_terms)[i].arity = elem_size;
                    (*out_terms)[i].size = elem_size;
                    /* Store pointer value temporarily as i_val (not used for strings currently) */
                    (*out_terms)[i].value.i_val = 0;
                    driver_free(str);
                    DRV_DEBUG("Decoded string ext of size %d at pos %d", elem_size, i);
                    break;
                }

                /* Fallback to existing generic decoder for any other term */
                default:
                    if (ei_decode_ei_term(buff, index, &(*out_terms)[i]) != 1) {
                        /* If it wasn't decoded we need to skip it manually to
                           keep the index in sync. */
                        DRV_DEBUG("Falling back to ei_skip_term for unknown type %d", elem_type);
                        ei_skip_term(buff, index);
                    }
                    break;
            }
        }

        /* Skip list tail (usually NIL) so that the caller's index points
           past the list. We don't care about its value, only that it is
           a valid term. */
        ei_skip_term(buff, index);
    }
    else if(type == ERL_STRING_EXT) {
        /* Existing behaviour for lists encoded as strings */
        unsigned char* str = driver_alloc(arity * sizeof(char) + 1);
        ei_decode_string(buff, index, (char*)str);
        for(int i = 0; i < arity; i++) {
            (*out_terms)[i].ei_type = ERL_INTEGER_EXT;
            (*out_terms)[i].value.i_val = (long) str[i];
            DRV_DEBUG("Decoded term %d from string ext: %d", i, (*out_terms)[i].value.i_val);
        }
        driver_free(str);
    }
    else if (type == ERL_NIL_EXT) {
        DRV_DEBUG("Decoding nil");
        (*out_terms)[0].ei_type = ERL_NIL_EXT;
        (*out_terms)[0].value.i_val = 0;
    }
    else {
        DRV_DEBUG("Unknown type in decode_list: %d", type);
        return -1;
    }

    return res;
}

int get_function_sig(uint32_t param_count, wasm_valkind_t *param_kinds, 
                       uint32_t result_count, wasm_valkind_t *result_kinds, 
                       char* type_str) {
    int current_pos = 0;
    type_str[current_pos++] = '(';
    for(uint32_t i = 0; i < param_count; i++) {
        type_str[current_pos++] = wasm_valkind_to_char((enum wasm_valkind_enum) param_kinds[i]);
    }
    type_str[current_pos++] = ')';
    for(uint32_t i = 0; i < result_count; i++) {
        type_str[current_pos++] = wasm_valkind_to_char((enum wasm_valkind_enum) result_kinds[i]);
    }
    type_str[current_pos] = '\0'; // Null-terminate the string
    return 1; // Assuming success always for now
}

wasm_func_t* get_exported_function(Proc* proc, const char* target_name) {
    wasm_extern_vec_t exports;
    wasm_instance_exports(proc->instance, &exports);
    wasm_exporttype_vec_t export_types;
    wasm_module_exports(proc->module, &export_types);
    wasm_func_t* func = NULL;

    for (size_t i = 0; i < exports.size; ++i) {
        wasm_extern_t* ext = exports.data[i];
        if (wasm_extern_kind(ext) == WASM_EXTERN_FUNC) {
            const wasm_name_t* exp_name = wasm_exporttype_name(export_types.data[i]);
            if (exp_name && exp_name->size == strlen(target_name) + 1 && 
                strncmp(exp_name->data, target_name, exp_name->size - 1) == 0) {
                func = wasm_extern_as_func(ext);
                break;
            }
        }
    }

    return func;
}

wasm_memory_inst_t get_memory(Proc* proc) {
    DRV_DEBUG("Getting memory for instance: %p", proc->instance);

    wasm_memory_inst_t default_memory = wasm_runtime_get_default_memory(proc->instance);
    if (default_memory) {
        DRV_DEBUG("Default memory: %p", default_memory);
        return default_memory;
    }

    DRV_DEBUG("Failed to get memory");
    return NULL;
}

long get_memory_size(Proc* proc) {
    DRV_DEBUG("Getting memory size");

    wasm_memory_inst_t memory = get_memory(proc);
    if (!memory) return 0;

    DRV_DEBUG("Memory: %p", memory);
    uint64_t page_count = wasm_memory_get_cur_page_count(memory);
    uint64_t page_size = wasm_memory_get_bytes_per_page(memory);
    uint64_t size = page_count * page_size;
    DRV_DEBUG("Memory size: %d pages * %dB = %dB", page_count, page_size, size);
    return size;
}
