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

#if BH_HAS_DLFCN
#include <dlfcn.h>

typedef uint32 (*get_native_lib_func)(char **p_module_name,
                                      NativeSymbol **p_native_symbols);

static uint32
load_and_register_native_libs(const char **native_lib_list,
                              uint32 native_lib_count,
                              void **native_handle_list)
{
    uint32 i, native_handle_count = 0, n_native_symbols;
    NativeSymbol *native_symbols;
    char *module_name;
    void *handle;

    for (i = 0; i < native_lib_count; i++) {
        /* open the native library */
        if (!(handle = dlopen(native_lib_list[i], RTLD_NOW | RTLD_GLOBAL))
            && !(handle = dlopen(native_lib_list[i], RTLD_LAZY))) {
            LOG_WARNING("warning: failed to load native library %s",
                        native_lib_list[i]);
            continue;
        }

        /* lookup get_native_lib func */
        get_native_lib_func get_native_lib = dlsym(handle, "get_native_lib");
        if (!get_native_lib) {
            LOG_WARNING("warning: failed to lookup `get_native_lib` function "
                        "from native lib %s",
                        native_lib_list[i]);
            dlclose(handle);
            continue;
        }

        n_native_symbols = get_native_lib(&module_name, &native_symbols);

        /* register native symbols */
        if (!(n_native_symbols > 0 && module_name && native_symbols
              && wasm_runtime_register_natives(module_name, native_symbols,
                                               n_native_symbols))) {
            LOG_WARNING("warning: failed to register native lib %s",
                        native_lib_list[i]);
            dlclose(handle);
            continue;
        }

        native_handle_list[native_handle_count++] = handle;
    }

    return native_handle_count;
}

static void
unregister_and_unload_native_libs(uint32 native_lib_count,
                                  void **native_handle_list)
{
    uint32 i, n_native_symbols;
    NativeSymbol *native_symbols;
    char *module_name;
    void *handle;

    for (i = 0; i < native_lib_count; i++) {
        handle = native_handle_list[i];

        /* lookup get_native_lib func */
        get_native_lib_func get_native_lib = dlsym(handle, "get_native_lib");
        if (!get_native_lib) {
            LOG_WARNING("warning: failed to lookup `get_native_lib` function "
                        "from native lib %p",
                        handle);
            continue;
        }

        n_native_symbols = get_native_lib(&module_name, &native_symbols);
        if (n_native_symbols == 0 || module_name == NULL
            || native_symbols == NULL) {
            LOG_WARNING("warning: get_native_lib returned different values for "
                        "native lib %p",
                        handle);
            continue;
        }

        /* unregister native symbols */
        if (!wasm_runtime_unregister_natives(module_name, native_symbols)) {
            LOG_WARNING("warning: failed to unregister native lib %p", handle);
            continue;
        }

        dlclose(handle);
    }
}
#endif

// #define HANDLE_FAIL() \
//     do {                      \
//         goto fail0;           \
//     } while (0)

static bool
can_enable_tiny_frame(const AOTCompOption *opt)
{
    return !opt->call_stack_features.values && !opt->enable_gc
           && !opt->enable_perf_profiling;
}

/* When print help info for target/cpu/target-abi/cpu-features, load this dummy
 * wasm file content rather than from an input file, the dummy wasm file content
 * is: magic header + version number */
static unsigned char dummy_wasm_file[8] = { 0x00, 0x61, 0x73, 0x6D,
                                            0x01, 0x00, 0x00, 0x00 };

int hb_wasm_aot_compile(uint8_t *wasm_module_data, size_t wasm_module_size, uint8_t **out_wasm_aot_data, size_t *out_wasm_aot_size)
{
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
#if BH_HAS_DLFCN
    const char *native_lib_list[8] = { NULL };
    uint32 native_lib_count = 0;
    void *native_handle_list[8] = { NULL };
    uint32 native_handle_count = 0;
#endif
#if WASM_ENABLE_LINUX_PERF != 0
    bool enable_linux_perf = false;
#endif

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
    init_args.mem_alloc_option.allocator.malloc_func = malloc;
    init_args.mem_alloc_option.allocator.realloc_func = realloc;
    init_args.mem_alloc_option.allocator.free_func = free;
#if WASM_ENABLE_LINUX_PERF != 0
    init_args.enable_linux_perf = enable_linux_perf;
#endif

    /* initialize runtime environment */
    if (!wasm_runtime_full_init(&init_args)) {
        printf("Init runtime environment failed.\n");
        return -1;
    }

#if BH_HAS_DLFCN
    bh_print_time("Begin to load native libs");
    native_handle_count = load_and_register_native_libs(
        native_lib_list, native_lib_count, native_handle_list);
#endif

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
    // if (!use_dummy_wasm) {
    //     wasm_runtime_free(wasm_module_data);
    // }

fail1:
    TRACE("fail1");
#if BH_HAS_DLFCN
    unregister_and_unload_native_libs(native_handle_count, native_handle_list);
#endif
    /* Destroy runtime environment */
    wasm_runtime_destroy();

fail0:
    TRACE("fail0");
    /* free option.custom_sections */
    if (option.custom_sections) {
        free(option.custom_sections);
    }
    free(llvm_options);

    DEBUG("wamrc return");
    return exit_status;
}
