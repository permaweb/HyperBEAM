/*
 * Copyright (C) 2019 Intel Corporation. All rights reserved.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 */

#include "../include/compile.h"
#include "../include/stub.h"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "wasm_export.h"
#include "aot_export.h"

// #include <llvm-c/Support.h>

static bool
can_enable_tiny_frame(const AOTCompOption *opt)
{
    return !opt->call_stack_features.values && !opt->enable_gc
           && !opt->enable_perf_profiling;
}

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

int wasm_func_type_to_signature(CompileOpts *compile_opts, wasm_func_type_t func_type, char **out_signature) {
    int res = -1;
    wasm_valkind_t *param_kinds = NULL;
    wasm_valkind_t *result_kinds = NULL;
    char *type_str = NULL;
    size_t signature_len;

    void *(*malloc_func)(size_t) = compile_opts->mem_alloc_option.allocator.malloc_func;
    // void *(*realloc_func)(void *, size_t) = compile_opts->mem_alloc_option.allocator.realloc_func;
    void (*free_func)(void *) = compile_opts->mem_alloc_option.allocator.free_func;

    uint32_t param_count = wasm_func_type_get_param_count(func_type);
    DEBUG("Param count: %d", param_count);

    if (param_count > 0) {
        param_kinds = malloc_func(sizeof(wasm_valkind_t) * param_count);
        if (!param_kinds) {
            DEBUG("Failed to allocate memory for param_kinds");
            goto fail;
        }
        for (uint32_t p_idx = 0; p_idx < param_count; ++p_idx) {
            param_kinds[p_idx] = wasm_func_type_get_param_valkind(func_type, p_idx);
            DEBUG("Param %d kind: %d", p_idx, param_kinds[p_idx]);
        }
    }

    uint32_t result_count = wasm_func_type_get_result_count(func_type);
    DEBUG("Result count: %d", result_count);

    if (result_count > 0) {
        result_kinds = malloc_func(sizeof(wasm_valkind_t) * result_count);
        if (!result_kinds) {
            DEBUG("Failed to allocate memory for result_kinds");
            goto fail;
        }
        for (uint32_t r_idx = 0; r_idx < result_count; ++r_idx) {
            result_kinds[r_idx] = wasm_func_type_get_result_valkind(func_type, r_idx);
            DEBUG("Result %d kind: %d", r_idx, result_kinds[r_idx]);
        }
    }

    // Calculate required length: '(' + params + ')' + results + '\0'
    signature_len = 1 + param_count + 1 + result_count + 1;
    type_str = malloc_func(signature_len);
    if (!type_str) {
        DEBUG("Failed to allocate memory for type_str");
        goto fail;
    }

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

    *out_signature = type_str;
    res = 0; // Success

fail:
    if (param_kinds) free_func(param_kinds);
    if (result_kinds) free_func(result_kinds);
    // If successful (res == 0), type_str is transferred to the caller via out_signature
    // If failed (res == -1), free type_str if it was allocated
    if (res != 0 && type_str) free_func(type_str);

    return res;
}

int extract_interface_functions(CompileOpts *compile_opts, wasm_module_t wasm_module, WasmInterfaceFunction **out_imports, size_t *out_import_count, WasmInterfaceFunction **out_exports, size_t *out_export_count)
{
    int res = -1;

    // Extract malloc/realloc/free functions from compile_opts
    void *(*malloc_func)(size_t) = compile_opts->mem_alloc_option.allocator.malloc_func;
    void *(*realloc_func)(void *, size_t) = compile_opts->mem_alloc_option.allocator.realloc_func;
    void (*free_func)(void *) = compile_opts->mem_alloc_option.allocator.free_func;

    int32_t import_count = wasm_runtime_get_import_count(wasm_module);
    DEBUG("Import count: %d", import_count);
    wasm_import_t *imports = malloc_func(sizeof(wasm_import_t) * import_count);

    for (int i = 0; i < import_count; i++) {
        wasm_runtime_get_import_type(wasm_module, i, &imports[i]);
        DEBUG("imports[%d]: %s.%s [%d]", i, imports[i].module_name, imports[i].name, imports[i].kind);
    }

    int32_t export_count = wasm_runtime_get_export_count(wasm_module);
    DEBUG("Export count: %d", export_count);
    wasm_export_t *exports = malloc_func(sizeof(wasm_export_t) * export_count);

    for (int i = 0; i < export_count; i++) {
        wasm_runtime_get_export_type(wasm_module, i, &exports[i]);
        DEBUG("exports[%d]: %s [%d]", i, exports[i].name, exports[i].kind);
    }

    WasmInterfaceFunction* imports_meta = malloc_func(sizeof(WasmInterfaceFunction) * import_count);
    WasmInterfaceFunction* exports_meta = malloc_func(sizeof(WasmInterfaceFunction) * export_count);

    for (int i = 0; i < import_count; i++) {
        imports_meta[i].module_name = malloc_func(strlen(imports[i].module_name) + 1);
        strcpy(imports_meta[i].module_name, imports[i].module_name);
        imports_meta[i].field_name = malloc_func(strlen(imports[i].name) + 1);
        strcpy(imports_meta[i].field_name, imports[i].name);
        imports_meta[i].kind = imports[i].kind;
        if (imports[i].kind == WASM_IMPORT_EXPORT_KIND_FUNC) {
            char *signature = NULL;
            if (wasm_func_type_to_signature(compile_opts, imports[i].u.func_type, &signature) != 0) {
                DEBUG("Failed to get signature for import %s.%s", imports[i].module_name, imports[i].name);
                goto fail0;
            }
            imports_meta[i].signature = signature;
        } else {
            imports_meta[i].signature = NULL;
        }
        DEBUG("Import %d: [%d] %s.%s %s", i, imports_meta[i].kind, imports_meta[i].module_name, imports_meta[i].field_name, imports_meta[i].signature);
    }
    
    for (int i = 0; i < export_count; i++) {
        // Exports do not have a module name
        exports_meta[i].module_name = NULL;
        exports_meta[i].field_name = malloc_func(strlen(exports[i].name) + 1);
        strcpy(exports_meta[i].field_name, exports[i].name);
        exports_meta[i].kind = exports[i].kind;
        if (exports[i].kind == WASM_IMPORT_EXPORT_KIND_FUNC) {
            char *signature = NULL;
            if (wasm_func_type_to_signature(compile_opts, exports[i].u.func_type, &signature) != 0) {
                DEBUG("Failed to get signature for export %s.%s", exports[i].name, exports[i].name);
                goto fail0;
            }
            exports_meta[i].signature = signature;
        } else {
            exports_meta[i].signature = NULL;
        }
        DEBUG("Export %d: [%d] %s %s", i, exports_meta[i].kind, exports_meta[i].field_name, exports_meta[i].signature);
    }

    *out_imports = imports_meta;
    *out_import_count = import_count;
    *out_exports = exports_meta;
    *out_export_count = export_count;

    res = 0;

fail1:
    if (res != 0) {
        free_func(imports_meta);
        free_func(exports_meta);
    }

fail0:
    free_func(imports);
    free_func(exports);

    return res;
}

__attribute__((visibility("default")))
int hb_wasm_aot_compile(CompileOpts *compile_opts, uint8_t *wasm_module_data, size_t wasm_module_size, WasmInterfaceFunction **out_imports, size_t *out_import_count, WasmInterfaceFunction **out_exports, size_t *out_export_count, uint8_t **out_wasm_aot_data, size_t *out_wasm_aot_size)
{
    *out_imports = NULL;
    *out_import_count = 0;
    *out_exports = NULL;
    *out_export_count = 0;
    *out_wasm_aot_data = NULL;
    *out_wasm_aot_size = 0;

    char **llvm_options = NULL;
    size_t llvm_options_count = 0;
    wasm_module_t wasm_module = NULL;
    aot_comp_data_t comp_data = NULL;
    aot_comp_context_t comp_ctx = NULL;
    RuntimeInitArgs init_args;
    AOTCompOption option = { 0 };
    char error_buf[128];
    int log_verbose_level = 2;
    bool sgx_mode = false, size_level_set = false, use_dummy_wasm = false;
    int exit_status = EXIT_FAILURE;

    /* Default options */
    option.opt_level = 3;
    option.size_level = 3;
    option.output_format = AOT_FORMAT_FILE;
    /* default value, enable or disable depends on the platform */
    option.bounds_checks = 2;
    /* default value, enable or disable depends on the platform */
    option.stack_bounds_checks = 2;
    option.enable_simd = true;
    option.enable_aux_stack_check = true;
    option.enable_bulk_memory = true;
    option.enable_ref_types = true;
    option.enable_gc = false;
    aot_call_stack_features_init_default(&option.call_stack_features);

    /* Configured options */
    // for (argc--, argv++; argc > 0 && argv[0][0] == '-'; argc--, argv++) {
    // if (!strcmp(argv[0], "-o")) {
    //     argc--, argv++;
    //     if (argc < 2)
    //         PRINT_HELP_AND_EXIT();
    //     out_file_name = argv[0];
    // }
    // else if (!strncmp(argv[0], "--target=", 9)) {
    //     if (argv[0][9] == '\0')
    //         PRINT_HELP_AND_EXIT();
    // option.target_arch = "aarch64";
    //     if (!strcmp(option.target_arch, "help")) {
    //         use_dummy_wasm = true;
    //     }
    // }
    // else if (!strncmp(argv[0], "--target-abi=", 13)) {
    //     if (argv[0][13] == '\0')
    //         PRINT_HELP_AND_EXIT();
    //     option.target_abi = argv[0] + 13;
    //     if (!strcmp(option.target_abi, "help")) {
    //         use_dummy_wasm = true;
    //     }
    // }
    // else if (!strncmp(argv[0], "--cpu=", 6)) {
    //     if (argv[0][6] == '\0')
    //         PRINT_HELP_AND_EXIT();
    //     option.target_cpu = argv[0] + 6;
    //     if (!strcmp(option.target_cpu, "help")) {
    //         use_dummy_wasm = true;
    //     }
    // }
    // else if (!strncmp(argv[0], "--cpu-features=", 15)) {
    //     if (argv[0][15] == '\0')
    //         PRINT_HELP_AND_EXIT();
    //     option.cpu_features = argv[0] + 15;
    //     if (!strcmp(option.cpu_features, "+help")) {
    //         use_dummy_wasm = true;
    //     }
    // }
    // else if (!strncmp(argv[0], "--opt-level=", 12)) {
    //     if (argv[0][12] == '\0')
    //         PRINT_HELP_AND_EXIT();
    //     option.opt_level = (uint32)atoi(argv[0] + 12);
    //     if (option.opt_level > 3)
    //         option.opt_level = 3;
    // }
    // else if (!strncmp(argv[0], "--size-level=", 13)) {
    //     if (argv[0][13] == '\0')
    //         PRINT_HELP_AND_EXIT();
    //     option.size_level = (uint32)atoi(argv[0] + 13);
    //     if (option.size_level > 3)
    //         option.size_level = 3;
    //     size_level_set = true;
    // }
    // else if (!strcmp(argv[0], "-sgx")) {
    //     sgx_mode = true;
    // }
    // else if (!strncmp(argv[0], "--bounds-checks=", 16)) {
    option.bounds_checks = 1;
    // }
    // else if (!strncmp(argv[0], "--stack-bounds-checks=", 22)) {
    option.stack_bounds_checks = 1;
    // }
    // else if (!strncmp(argv[0], "--stack-usage=", 14)) {
    //     option.stack_usage_file = argv[0] + 14;
    // }
    // else if (!strncmp(argv[0], "--format=", 9)) {
    //     if (argv[0][9] == '\0')
    //         PRINT_HELP_AND_EXIT();
    //     if (!strcmp(argv[0] + 9, "aot"))
    //         option.output_format = AOT_FORMAT_FILE;
    //     else if (!strcmp(argv[0] + 9, "object"))
    //         option.output_format = AOT_OBJECT_FILE;
    //     else if (!strcmp(argv[0] + 9, "llvmir-unopt"))
    //         option.output_format = AOT_LLVMIR_UNOPT_FILE;
    //     else if (!strcmp(argv[0] + 9, "llvmir-opt"))
    //         option.output_format = AOT_LLVMIR_OPT_FILE;
    //     else {
    //         printf("Invalid format %s.\n", argv[0] + 9);
    //         PRINT_HELP_AND_EXIT();
    //     }
    // }
    // else if (!strncmp(argv[0], "-v=", 3)) {
    //     log_verbose_level = atoi(argv[0] + 3);
    //     if (log_verbose_level < 0 || log_verbose_level > 5)
    //         PRINT_HELP_AND_EXIT();
    // }
    // else if (!strcmp(argv[0], "--disable-bulk-memory")) {
    //     option.enable_bulk_memory = false;
    // }
    // else if (!strcmp(argv[0], "--enable-multi-thread")) {
    //     option.enable_bulk_memory = true;
    //     option.enable_thread_mgr = true;
    // }
    // else if (!strcmp(argv[0], "--enable-tail-call")) {
    option.enable_tail_call = true;
    // }
    // else if (!strcmp(argv[0], "--enable-simd")) {
    //     /* obsolete option, kept for compatibility */
    option.enable_simd = true;
    // }
    // else if (!strcmp(argv[0], "--disable-simd")) {
    //     option.enable_simd = false;
    // }
    // else if (!strcmp(argv[0], "--disable-ref-types")) {
    // option.enable_ref_types = false;
    // }
    // else if (!strcmp(argv[0], "--disable-aux-stack-check")) {
    //     option.enable_aux_stack_check = false;
    // }
    // else if (!strcmp(argv[0], "--enable-dump-call-stack")) {
    option.aux_stack_frame_type = AOT_STACK_FRAME_TYPE_STANDARD;
    // }
    // else if (!strncmp(argv[0], "--call-stack-features=", 22)) {
    //     /* Reset all the features, only enable the user-defined ones */
    // memset(&option.call_stack_features, 0,
    //         sizeof(AOTCallStackFeatures));
    //     if (argv[0][22] != '\0') {
    //         if (!parse_call_stack_features(argv[0] + 22,
    //                                         &option.call_stack_features)) {
    // option.call_stack_features.bounds_checks = true;
    //             printf("Failed to parse call-stack-features\n");
    //             PRINT_HELP_AND_EXIT();
    //         }
    //     }
    // }
    // else if (!strcmp(argv[0], "--enable-perf-profiling")) {
    //     option.aux_stack_frame_type = AOT_STACK_FRAME_TYPE_STANDARD;
    //     option.enable_perf_profiling = true;
    // }
    // else if (!strcmp(argv[0], "--enable-memory-profiling")) {
    option.enable_memory_profiling = true;
    option.enable_stack_estimation = true;
    // }
    // else if (!strcmp(argv[0], "--xip")) {
    //     option.is_indirect_mode = true;
    //     option.disable_llvm_intrinsics = true;
    // }
    // else if (!strcmp(argv[0], "--enable-indirect-mode")) {
    //     option.is_indirect_mode = true;
    // }
    // else if (!strcmp(argv[0], "--enable-gc")) {
    //     option.aux_stack_frame_type = AOT_STACK_FRAME_TYPE_STANDARD;
    //     option.enable_gc = true;
    // }
    // else if (!strcmp(argv[0], "--disable-llvm-intrinsics")) {
    //     option.disable_llvm_intrinsics = true;
    // }
    // else if (!strncmp(argv[0], "--enable-builtin-intrinsics=", 28)) {
    //     if (argv[0][28] == '\0')
    //         PRINT_HELP_AND_EXIT();
    //     option.builtin_intrinsics = argv[0] + 28;
    // }
    // else if (!strcmp(argv[0], "--disable-llvm-lto")) {
    //     option.disable_llvm_lto = true;
    // }
    // else if (!strcmp(argv[0], "--enable-llvm-pgo")) {
    //     option.enable_llvm_pgo = true;
    // }
    // else if (!strncmp(argv[0], "--enable-llvm-passes=", 21)) {
    //     if (argv[0][21] == '\0')
    //         PRINT_HELP_AND_EXIT();
    //     option.llvm_passes = argv[0] + 21;
    // }
    // else if (!strncmp(argv[0], "--use-prof-file=", 16)) {
    //     if (argv[0][16] == '\0')
    //         PRINT_HELP_AND_EXIT();
    //     option.use_prof_file = argv[0] + 16;
    // }
    // else if (!strcmp(argv[0], "--enable-segue")) {
    //     /* all flags are enabled */
    //     option.segue_flags = 0x1F1F;
    // }
    // else if (!strncmp(argv[0], "--enable-segue=", 15)) {
    //     option.segue_flags = resolve_segue_flags(argv[0] + 15);
    //     if (option.segue_flags == (uint32)-1)
    //         PRINT_HELP_AND_EXIT();
    // }
    // else if (!strncmp(argv[0], "--emit-custom-sections=", 23)) {
    //     int len = 0;
    //     if (option.custom_sections) {
    //         free(option.custom_sections);
    //     }

    //     option.custom_sections = split_string(argv[0] + 23, &len, ",");
    //     if (!option.custom_sections) {
    //         printf("Failed to process emit-custom-sections: alloc "
    //                 "memory failed\n");
    //         PRINT_HELP_AND_EXIT();
    //     }

    //     option.custom_sections_count = len;
    // }
    // else if (!strncmp(argv[0], "--mllvm=", 8)) {
    //     void *np;
    //     if (argv[0][8] == '\0')
    //         PRINT_HELP_AND_EXIT();
    //     if (llvm_options_count == 0)
    //         llvm_options_count += 2;
    //     else
    //         llvm_options_count++;
    //     np = realloc(llvm_options, llvm_options_count * sizeof(char *));
    //     if (np == NULL) {
    //         printf("Memory allocation failure\n");
    //         goto fail0;
    //     }
    //     llvm_options = np;
    //     if (llvm_options_count == 2)
    //         llvm_options[llvm_options_count - 2] = "wamrc";
    //     llvm_options[llvm_options_count - 1] = argv[0] + 8;
    // }
    // else if (!strcmp(argv[0], "--enable-shared-heap")) {
    //     option.enable_shared_heap = true;
    // }
    // else if (!strcmp(argv[0], "--version")) {
    //     uint32 major, minor, patch;
    //     wasm_runtime_get_version(&major, &minor, &patch);
    //     printf("wamrc %u.%u.%u\n", major, minor, patch);
    //     return 0;
    // }
    // else
    //     PRINT_HELP_AND_EXIT();
    // }

    /* Additional options */
    option.enable_nan_canonicalization = true;
    option.nan_canonicalization_sign_bit = 0;

    if (option.aux_stack_frame_type == AOT_STACK_FRAME_TYPE_STANDARD
        && can_enable_tiny_frame(&option)) {
        DEBUG("Use tiny frame mode for stack frames");
        option.aux_stack_frame_type = AOT_STACK_FRAME_TYPE_TINY;
        /* for now we only enable frame per function for a TINY frame mode */
        option.call_stack_features.frame_per_function = true;
    }
    if (!option.call_stack_features.func_idx
        && (option.enable_gc || option.enable_perf_profiling)) {
        DEBUG("'func-idx' call stack feature will be automatically "
                    "enabled for GC and perf profiling mode");
        option.call_stack_features.func_idx = true;
    }

    if (!size_level_set) {
        /**
         * Set opt level to 1 by default for Windows and MacOS as
         * they can not memory map out 0-2GB memory and might not
         * be able to meet the requirements of some AOT relocation
         * operations.
         */
        if (option.target_abi && !strcmp(option.target_abi, "msvc")) {
            DEBUG("Set size level to 1 for Windows AOT file");
            option.size_level = 1;
        }
#if defined(_WIN32) || defined(_WIN32_) \
    || ((defined(__APPLE__) || defined(__MACH__)) && !defined(__arm64__))
        if (!option.target_arch && !option.target_abi) {
            DEBUG("Set size level to 1 for Windows or MacOS AOT file");
            option.size_level = 1;
        }
#endif
    }

    if (option.enable_gc && !option.call_stack_features.values) {
        DEBUG("Call stack feature 'values' must be enabled for GC. The "
                    "feature will be enabled automatically.");
        option.call_stack_features.values = true;
    }

    if (sgx_mode) {
        option.size_level = 1;
        option.is_sgx_platform = true;
    }

    if (option.enable_gc) {
        option.enable_ref_types = false;
    }

    memset(&init_args, 0, sizeof(RuntimeInitArgs));

    init_args.mem_alloc_type = Alloc_With_Allocator;
    init_args.mem_alloc_option.allocator.malloc_func = compile_opts->mem_alloc_option.allocator.malloc_func;
    init_args.mem_alloc_option.allocator.realloc_func = compile_opts->mem_alloc_option.allocator.realloc_func;
    init_args.mem_alloc_option.allocator.free_func = compile_opts->mem_alloc_option.allocator.free_func;

    /* initialize runtime environment */
    if (!wasm_runtime_full_init(&init_args)) {
        printf("Init runtime environment failed.\n");
        return -1;
    }

    // if (llvm_options_count > 0)
    //     LLVMParseCommandLineOptions(llvm_options_count,
    //                                 (const char **)llvm_options, "wamrc");

    DEBUG("Begin to load wasm file");

    if (wasm_module_size >= 4 /* length of MAGIC NUMBER */
        && get_package_type(wasm_module_data, wasm_module_size)
               != Wasm_Module_Bytecode) {
        DEBUG("Invalid wasm file: magic header not detected\n");
        goto fail2;
    }

    /* load WASM module */
    if (!(wasm_module = wasm_runtime_load(wasm_module_data, wasm_module_size, error_buf,
                                          sizeof(error_buf)))) {
        DEBUG("%s\n", error_buf);
        goto fail2;
    }

    if (extract_interface_functions(compile_opts, wasm_module, out_imports, out_import_count, out_exports, out_export_count) != 0) {
        DEBUG("Failed to extract interface functions");
        goto fail3;
    }

    if (!(comp_data = aot_create_comp_data(wasm_module, option.target_arch,
                                           option.enable_gc))) {
        DEBUG("%s\n", aot_get_last_error());
        goto fail3;
    }

#if WASM_ENABLE_DEBUG_AOT != 0
    if (!create_dwarf_extractor(comp_data, wasm_file_name)) {
        printf("%s:create dwarf extractor failed\n", wasm_file_name);
    }
#endif

    DEBUG("Begin to create compile context");

    if (!(comp_ctx = aot_create_comp_context(comp_data, &option))) {
        DEBUG("%s\n", aot_get_last_error());
        goto fail4;
    }

    DEBUG("Begin to compile");

    if (!aot_compile_wasm(comp_ctx)) {
        DEBUG("%s\n", aot_get_last_error());
        goto fail5;
    }

    uint32_t size_l;
    switch (option.output_format) {
        case AOT_FORMAT_FILE:
            *out_wasm_aot_data = aot_emit_aot_file_buf(comp_ctx, comp_data, &size_l);
            *out_wasm_aot_size = size_l;
            break;
        default:
            DEBUG("Unsupported output format: %d", option.output_format);
            exit_status = EXIT_FAILURE;
            goto fail5;
            break;
    }

    DEBUG("Compile end");

    DEBUG("Compile success, AOT buffer of size %zu was generated.\n", *out_wasm_aot_size);
    exit_status = EXIT_SUCCESS;

fail5:
    TRACE("fail5");
    /* Destroy compiler context */
    aot_destroy_comp_context(comp_ctx);

fail4:
    TRACE("fail4");
    /* Destroy compile data */
    aot_destroy_comp_data(comp_data);

fail3:
    TRACE("fail3");
    /* Unload WASM module */
    wasm_runtime_unload(wasm_module);

fail2:
    TRACE("fail2");
    /* free the file buffer */
    // wasm_runtime_free(wasm_module_data);

fail1:
    TRACE("fail1");
    /* Destroy runtime environment */
    wasm_runtime_destroy();

fail0:
    TRACE("fail0");
    /* free option.custom_sections */
    if (option.custom_sections) {
        free(option.custom_sections);
    }
    free(llvm_options);

    DEBUG("hb_wasm_aot_compile return");
    return exit_status;
}
