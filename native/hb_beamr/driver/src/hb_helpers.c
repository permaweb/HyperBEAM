#include "../include/hb_helpers.h"
#include "../include/hb_logging.h"
#include "wasm_export.h"

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
char wasm_valkind_to_char(enum wasm_valkind_enum* valkind) {
    switch (*valkind) {
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
            term[1] = (ErlDrvTermData) *arg_ptr;
            return 2;
        case WASM_I64:
            term[0] = ERL_DRV_INT64;
            term[1] = (ErlDrvTermData) *arg_ptr;
            return 2;
        case WASM_F32:
            term[0] = ERL_DRV_FLOAT;
            term[1] = (ErlDrvTermData) *arg_ptr;
            return 2;
        case WASM_F64:
            term[0] = ERL_DRV_FLOAT;
            term[1] = (ErlDrvTermData) *arg_ptr;
            return 2;
        default:
            DRV_DEBUG("Unsupported result type: %d", kind);
            return 0;
    }
}


int erl_term_to_wasm_val(wasm_val_t* val, ei_term* term) {
    DRV_DEBUG("Converting erl term to wasm val. Term: %d. Size: %d", term->value.i_val, term->size);
    switch (val->kind) {
        case WASM_I32:
            val->of.i32 = (int) term->value.i_val;
            break;
        case WASM_I64:
            val->of.i64 = (long) term->value.i_val;
            break;
        case WASM_F32:
            val->of.f32 = (float) term->value.d_val;
            break;
        case WASM_F64:
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

int erl_term_to_import_result(enum wasm_valkind_enum* val_kind, uint64_t* val, ei_term* term) {
    DRV_DEBUG("Converting erl term to wasm val. Term: %d. Size: %d", term->value.i_val, term->size);
    switch (*val_kind) {
        case WASM_I32:
            *val = (int) term->value.i_val;
            break;
        case WASM_I64:
            *val = (long) term->value.i_val;
            break;
        case WASM_F32:
            *val = (float) term->value.d_val;
            break;
        case WASM_F64:
            *val = term->value.d_val;
            break;
        default:
            DRV_DEBUG("Unsupported parameter type: %d", *val_kind);
            return -1;
    }
    return 0;
}

int erl_terms_to_import_results(uint32_t val_count, enum wasm_valkind_enum* val_kinds, uint64_t* vals, ei_term* terms) {
    DRV_DEBUG("Converting erl terms to wasm vals");
    DRV_DEBUG("Vals: %d", val_count);
    for(int i = 0; i < val_count; i++) {
        DRV_DEBUG("Converting term %d: %p", i, &vals[i]);
        int res = erl_term_to_import_result(&val_kinds[i], &vals[i], &terms[i]);
        if(res == -1) {
            DRV_DEBUG("Failed to convert term to wasm val");
            return -1;
        }
    }
    return 0;
}

ei_term* decode_list(char* buff, int* index) {
    int arity, type;

    if(ei_get_type(buff, index, &type, &arity) == -1) {
        DRV_DEBUG("Failed to get type");
        return NULL;
    }
    DRV_DEBUG("Decoded header. Arity: %d", arity);

    ei_term* res = driver_alloc(sizeof(ei_term) * arity);

    if(type == ERL_LIST_EXT) {
        //DRV_DEBUG("Decoding list");
        ei_decode_list_header(buff, index, &arity);
        //DRV_DEBUG("Decoded list header. Arity: %d", arity);
        for(int i = 0; i < arity; i++) {
            ei_decode_ei_term(buff, index, &res[i]);
            DRV_DEBUG("Decoded term (assuming int) %d: %d", i, res[i].value.i_val);
        }
    }
    else if(type == ERL_STRING_EXT) {
        //DRV_DEBUG("Decoding list encoded as string");
        unsigned char* str = driver_alloc(arity * sizeof(char) + 1);
        ei_decode_string(buff, index, str);
        for(int i = 0; i < arity; i++) {
            res[i].ei_type = ERL_INTEGER_EXT;
            res[i].value.i_val = (long) str[i];
            DRV_DEBUG("Decoded term %d: %d", i, res[i].value.i_val);
        }
        driver_free(str);
    }
    else {
        DRV_DEBUG("Unknown type: %d", type);
        return NULL;
    }

    return res;
}

int get_function_sig(uint32_t param_count, enum wasm_valkind_enum* param_kinds, 
                       uint32_t result_count, enum wasm_valkind_enum* result_kinds, 
                       char* type_str) {
    int current_pos = 0;
    type_str[current_pos++] = '(';
    for(uint32_t i = 0; i < param_count; i++) {
        type_str[current_pos++] = wasm_valkind_to_char(&param_kinds[i]);
    }
    type_str[current_pos++] = ')';
    for(uint32_t i = 0; i < result_count; i++) {
        type_str[current_pos++] = wasm_valkind_to_char(&result_kinds[i]);
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
    return wasm_runtime_get_default_memory(proc->instance);
}

long get_memory_size(Proc* proc) {
    wasm_memory_inst_t memory = get_memory(proc);
    return wasm_memory_get_cur_page_count(memory) * wasm_memory_get_bytes_per_page(memory);
}
