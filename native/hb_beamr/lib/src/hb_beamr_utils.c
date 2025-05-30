#include "hb_beamr_utils.h"
#include "wasm_export.h"
#include <stdio.h>

void hb_beamr_utils_print_wasm_vals(const wasm_val_t* vals, const int val_count) {
    fprintf(stderr, "Printing %d wasm vals\n", val_count);
    for (int i = 0; i < val_count; i++) {
        switch(vals[i].kind) {
            case WASM_I32:
                fprintf(stderr, "- [%d]: I32 %d\n", i, vals[i].of.i32);
                break;
            case WASM_I64:
                fprintf(stderr, "- [%d]: I64 %lld\n", i, vals[i].of.i64);
                break;
            case WASM_F32:
                fprintf(stderr, "- [%d]: F32 %f\n", i, vals[i].of.f32);
                break;
            case WASM_F64:
                fprintf(stderr, "- [%d]: F64 %lf\n", i, vals[i].of.f64);
                break;
            default:
                fprintf(stderr, "- [%d]: Unknown result type: %d\n", i, vals[i].kind);
                break;
        }
    }
    fflush(stderr);
}

char* wasm_valkind_to_string(const wasm_valkind_t val_kind) {
    switch(val_kind) {
        case WASM_I32:
            return "I32";
        case WASM_I64:
            return "I64";
        case WASM_F32:
            return "F32";
        case WASM_F64:
            return "F64";
        case WASM_V128:
            return "V128";
        case WASM_EXTERNREF:
            return "EXTERNREF";
        case WASM_FUNCREF:
            return "FUNCREF";
        default:
            return "UNKNOWN";
    }
}

void hb_beamr_utils_print_wasm_val_kinds(const wasm_valkind_t* vals, const int val_count) {
    fprintf(stderr, "Printing %d wasm val kinds\n", val_count);
    for (int i = 0; i < val_count; i++) {
        fprintf(stderr, "- [%d]: %s (%d)\n", i, wasm_valkind_to_string(vals[i]), vals[i]);
    }
    fflush(stderr);
}

void hb_beamr_utils_print_module_info(wasm_module_t wasm_module) {
    fprintf(stderr, "[DEBUG hb_beamr_lib_instantiate] Post-instantiation module info (from wasm_module: %p)\n", (void*)wasm_module);
    if (wasm_module) {
        int32_t import_count = wasm_runtime_get_import_count(wasm_module);
        fprintf(stderr, "  Import count: %d\n", import_count);
        for (int32_t i = 0; i < import_count; ++i) {
            wasm_import_t import_info;
            wasm_runtime_get_import_type(wasm_module, i, &import_info);
            const char* kind_str = "unknown_import_kind";
            switch (import_info.kind) {
                case WASM_IMPORT_EXPORT_KIND_FUNC: kind_str = "func"; break;
                case WASM_IMPORT_EXPORT_KIND_TABLE: kind_str = "table"; break;
                case WASM_IMPORT_EXPORT_KIND_MEMORY: kind_str = "memory"; break;
                case WASM_IMPORT_EXPORT_KIND_GLOBAL: kind_str = "global"; break;
            }
            fprintf(stderr, "    Import [%d]: Module='%s', Name='%s', Kind=%s (%d)\n", 
                    i, import_info.module_name ? import_info.module_name : "NULL", 
                       import_info.name ? import_info.name : "NULL", 
                       kind_str, import_info.kind);
        }

        int32_t export_count = wasm_runtime_get_export_count(wasm_module);
        fprintf(stderr, "  Export count: %d\n", export_count);
        for (int32_t i = 0; i < export_count; ++i) {
            wasm_export_t export_info;
            wasm_runtime_get_export_type(wasm_module, i, &export_info);
            const char* kind_str = "unknown_export_kind";
            switch (export_info.kind) {
                case WASM_IMPORT_EXPORT_KIND_FUNC: kind_str = "func"; break;
                case WASM_IMPORT_EXPORT_KIND_TABLE: kind_str = "table"; break;
                case WASM_IMPORT_EXPORT_KIND_MEMORY: kind_str = "memory"; break;
                case WASM_IMPORT_EXPORT_KIND_GLOBAL: kind_str = "global"; break;
            }
            fprintf(stderr, "    Export [%d]: Name='%s', Kind=%s (%d)\n", 
                    i, export_info.name ? export_info.name : "NULL", 
                       kind_str, export_info.kind);
        }
    }
    fflush(stderr);
}