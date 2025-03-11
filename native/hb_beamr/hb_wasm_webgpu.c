#include "include/hb_wasm_webgpu.h"
#include "include/hb_core.h"
#include "include/hb_helpers.h"
#include "include/hb_logging.h"
#include "include/hb_wasm.h"
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <wasm_c_api.h>
#include <webgpu.h>
#include <wgpu.h>

#define LOG_MACROS
#define LOG_TRACE(...) DRV_DEBUG(__VA_ARGS__)
#define LOG_DEBUG(...) DRV_DEBUG(__VA_ARGS__)
#define LOG_WARN(...) DRV_DEBUG(__VA_ARGS__)
#define LOG_ERROR(...) DRV_DEBUG(__VA_ARGS__)
#define FATAL(...)                                                             \
    do {                                                                       \
        DRV_DEBUG(__VA_ARGS__);                                                \
        abort();                                                               \
    } while (0)

#include "wasm_webgpu_c_api.h"

#ifdef WASM_64
#define WASM_INT_KIND WASM_I64
#define WASM_INT_C_TYPE int64_t
#define WASM_POINTER_C_TYPE int64_t
#define WASM_POINTER_KIND WASM_I64
#define WASM_VAL_INT_PROP i64
#else
#define WASM_INT_KIND WASM_I32
#define WASM_INT_C_TYPE int32_t
#define WASM_POINTER_C_TYPE int32_t
#define WASM_POINTER_KIND WASM_I32
#define WASM_VAL_INT_PROP i32
#endif

#define WGPU_LOG_LEVEL WGPULogLevel_Trace

// typedef void (*WGPULogCallback)(WGPULogLevel level, char const * message,
// void * userdata);
void webgpu_log_callback(WGPULogLevel level, const char *message,
                         void *userdata) {
    DRV_DEBUG("WGPU[%d]: %s", level, message);
}

// Placeholder macro for error handling (can be overridden by user if needed).
#ifndef WASM_MEMORY_ERROR
#define WASM_MEMORY_ERROR(msg, ...)                                            \
    do {                                                                       \
        DRV_DEBUG("WASM Memory Error: %s\n", msg, __VA_ARGS__);                \
    } while (0)
#endif

/**
 * Safely copy a flat struct from a WASM instance's linear memory into native
 * memory.
 *
 * \param memory    Pointer to the WASM memory (obtained from wasm_instance).
 * \param offset    Offset (index) within WASM memory where the struct starts
 * (i.e., the struct pointer as i32 from WASM).
 * \param out_struct Pointer to a native struct where data will be copied.
 * \param struct_size Size (in bytes) of the struct to copy.
 * \param align     Required alignment of the struct (in bytes, e.g., 4 for
 * 32-bit alignment). Use 1 if no specific alignment needed.
 * \return 0 on success, 1 on failure (with an error reported via
 * WASM_MEMORY_ERROR macro).
 *
 * This function performs bounds checking to ensure the entire struct lies
 * within the WASM memory, and also checks alignment if `align` > 1. It uses
 * `wasm_memory_data` to get a direct pointer to the WASM memory buffer and then
 * copies `struct_size` bytes into the native struct.
 */
int wasm_copy_struct_safe(wasm_memory_t *memory, uint32_t offset,
                          void *out_struct, size_t struct_size, size_t align) {
    assert(memory != NULL);
    assert(out_struct != NULL);

    // Get the start of the memory and its size.
    byte_t *mem_base = wasm_memory_data(memory);
    size_t mem_size = wasm_memory_data_size(memory);

    // Bounds checking: ensure [offset, offset+struct_size) is within memory.
    // Also protect against overflow in pointer arithmetic.
    if (offset > mem_size || struct_size > mem_size ||
        offset > mem_size - struct_size) {
        WASM_MEMORY_ERROR("Attempt to read struct out of WASM memory bounds");
        return 1;
    }

    // Alignment check (if required).
    // If the struct in WASM memory is expected to be aligned (e.g., naturally
    // aligned), verify the offset meets the alignment requirement. This can
    // catch misaligned pointers.
    if (align > 1 && (offset % align) != 0) {
        return 1;
    }

    // Copy bytes from WASM memory into the native struct.
    memcpy(out_struct, mem_base + offset, struct_size);
    return 0;
}

int wasm_copy_array_safe(wasm_memory_t *memory, uint32_t offset,
                         void *out_array, size_t array_size, size_t align) {
    assert(memory != NULL);
    assert(out_array != NULL);

    // Get the start of the memory and its size.
    byte_t *mem_base = wasm_memory_data(memory);
    size_t mem_size = wasm_memory_data_size(memory);

    // Bounds check the initial pointer
    if (offset >= mem_size || offset + array_size > mem_size) {
        WASM_MEMORY_ERROR("Array out of bounds");
        return 1;
    }

    // Copy bytes from WASM memory into the native struct.
    memcpy(out_array, mem_base + offset, array_size);
    return 0;
}

char *wasm_copy_string_len_safe(wasm_memory_t *memory, uint32_t str_ptr,
                                size_t str_len) {
    if (str_ptr == 0)
        return NULL;

    byte_t *mem_base = wasm_memory_data(memory);
    size_t mem_size = wasm_memory_data_size(memory);

    // Bounds check the initial pointer
    if (str_ptr >= mem_size) {
        WASM_MEMORY_ERROR("String pointer out of bounds");
        return NULL;
    }

    if (str_len > mem_size - str_ptr) {
        WASM_MEMORY_ERROR("String length out of bounds");
        return NULL;
    }

    // Get string length safely
    const char *wasm_str = (const char *)(mem_base + str_ptr);

    // Allocate and copy
    char *native_str = malloc(str_len + 1);
    if (!native_str) {
        WASM_MEMORY_ERROR("Failed to allocate memory for string");
        return NULL;
    }

    memcpy(native_str, wasm_str, str_len);
    native_str[str_len] = '\0';
    return native_str;
}

// Boilerplate for handlers
#define HANDLER_INIT(name, args_expected)                                      \
    DRV_DEBUG(#name " called");                                                \
    ImportHook *import_hook = (ImportHook *)env;                               \
    Proc *proc = import_hook->proc;                                            \
    BindWGPUObjectMappingRegistry *registry = &proc->registry;                 \
    wasm_store_t *store = proc->store;                                         \
    wasm_memory_t *wasm_memory = get_memory(proc);                             \
    byte_t *wasm_base_ptr = wasm_memory_data(wasm_memory);                     \
    DRV_DEBUG("Proc: %p. Args size: %d", (void *)proc, (int)args->size);       \
    DRV_DEBUG("Import name: %s.%s [%s]", import_hook->module_name,             \
              import_hook->field_name, import_hook->signature);                \
    if (args->size != args_expected) {                                         \
        DRV_DEBUG("Incorrect number of arguments: expected " #args_expected    \
                  ", got %d",                                                  \
                  (int)args->size);                                            \
        const char *errMsg = "Not enough arguments provided";                  \
        wasm_byte_vec_t message;                                               \
        wasm_byte_vec_new(&message, strlen(errMsg), errMsg);                   \
        return wasm_trap_new(proc->store, &message);                           \
    }

typedef struct {
    WASM_INT_C_TYPE queue;
    uint64_t submissionIndex;
} WASMWrappedSubmissionIndex;

WGPUWrappedSubmissionIndex *
extract_wrapped_submission_index(BindWGPUObjectMappingRegistry *registry,
                                 wasm_memory_t *memory,
                                 uint32_t descriptor_ptr) {
    if (descriptor_ptr == 0) {
        return NULL;
    }

    WASMWrappedSubmissionIndex *descriptor =
        (WASMWrappedSubmissionIndex *)malloc(
            sizeof(WASMWrappedSubmissionIndex));
    memset(descriptor, 0, sizeof(WASMWrappedSubmissionIndex));

    WASMWrappedSubmissionIndex wasm_wrapped_submission_index;
    if (!wasm_copy_struct_safe(memory, descriptor_ptr,
                               &wasm_wrapped_submission_index,
                               sizeof(WASMWrappedSubmissionIndex), 4)) {
        DRV_DEBUG("Failed to copy submission index");
        return NULL;
    }

    WGPUWrappedSubmissionIndex *wrapped_submission_index =
        (WGPUWrappedSubmissionIndex *)malloc(
            sizeof(WGPUWrappedSubmissionIndex));
    memset(wrapped_submission_index, 0, sizeof(WGPUWrappedSubmissionIndex));

    wrapped_submission_index->queue = (WGPUQueue)registry_item_get_mapping(
        &registry->queues, wasm_wrapped_submission_index.queue);
    wrapped_submission_index->submissionIndex =
        wasm_wrapped_submission_index.submissionIndex;

    return wrapped_submission_index;
}

// WGPUBool wgpuDevicePoll(WGPUDevice device, WGPUBool wait, WGPU_NULLABLE WGPUWrappedSubmissionIndex const * wrappedSubmissionIndex);
wasm_trap_t *wgpuDevicePollImport(void *env, const wasm_val_vec_t *args,
                                  wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuDevicePoll, 3);

    // Get the device from registry
    WGPUDevice device = (WGPUDevice)registry_item_get_mapping(
        &registry->devices, wasm_val_to_native_int(args->data[0]));

    // Get wait
    WGPUBool wait = (WGPUBool)wasm_val_to_native_int(args->data[1]);

    // Get wrappedSubmissionIndex
    WGPUWrappedSubmissionIndex *wrappedSubmissionIndex =
        extract_wrapped_submission_index(registry, wasm_memory,
                                         wasm_val_to_native_int(args->data[2]));

    // Print arguments
    DRV_DEBUG("device = %p, wait = %d, wrappedSubmissionIndex = %p", device,
              wait, wrappedSubmissionIndex);

    // Call the native function
    WGPUBool result = wgpuDevicePoll(device, wait, wrappedSubmissionIndex);

    results->data[0].kind = WASM_INT_KIND;
    results->data[0].of.i32 = result;

    return NULL;
}

// WGPU_EXPORT WGPUInstance wgpuCreateInstance(WGPU_NULLABLE WGPUInstanceDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuCreateInstanceImport(void *env, const wasm_val_vec_t *args,
                                      wasm_val_vec_t *results) {
    LOG_TRACE(
        "wgpuCreateInstanceImport: params: env (%p), args (%p), results (%p)",
        env, args, results);
    HANDLER_INIT(wgpuCreateInstanceImport, 1);

    wgpuSetLogLevel(WGPULogLevel_Trace);
    wgpuSetLogCallback(webgpu_log_callback, NULL);

    WGPUInstanceDescriptor *descriptor = NULL;
    extract_instance_descriptor(registry, wasm_memory,
                                (byte_t *)wasm_val_to_native_int(args->data[0]),
                                &descriptor);

    DRV_DEBUG("Parameters: descriptor=%p", &descriptor);
    WGPUInstance instance = wgpuCreateInstance(descriptor);
    DRV_DEBUG("Result: WGPUInstance: %p", (uintptr_t)instance);

    size_t instance_index =
        registry_item_add_mapping(&registry->instances, (void *)instance);

    DRV_DEBUG("Setting result to WGPUInstance WASM_INT_KIND: %d",
              instance_index);
    results->size = 1;
    results->data[0].kind = WASM_INT_KIND;
    results->data[0].of.i32 = instance_index;

    return NULL;
}

static const struct {
    const char *name;
    void *func;
} webgpu_import_funcs_custom[] = {
    // WGPUBool wgpuDevicePoll(WGPUDevice device, WGPUBool wait, WGPU_NULLABLE WGPUWrappedSubmissionIndex const * wrappedSubmissionIndex);
    {.name = "wgpuDevicePoll", .func = wgpuDevicePollImport},
    // WGPU_EXPORT WGPUInstance wgpuCreateInstance(WGPU_NULLABLE WGPUInstanceDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
    {.name = "wgpuCreateInstance", .func = wgpuCreateInstanceImport},
};

wasm_trap_t *environ_sizes_get_import(void *env, const wasm_val_vec_t *args,
                                      wasm_val_vec_t *results) {
    results->size = 2;
    results->data[0].kind = WASM_INT_KIND;
    results->data[0].of.i32 = 0;
    results->data[1].kind = WASM_INT_KIND;
    results->data[1].of.i32 = 0;
    return NULL;
}

wasm_trap_t *environ_get_import(void *env, const wasm_val_vec_t *args,
                                wasm_val_vec_t *results) {
    results->size = 2;
    results->data[0].kind = WASM_INT_KIND;
    results->data[0].of.i32 = 0;
    results->data[1].kind = WASM_INT_KIND;
    results->data[1].of.i32 = 0;
    return NULL;
}

typedef struct {
    WASM_POINTER_C_TYPE buf_ptr;
    WASM_INT_C_TYPE buf_len;
} WASMIOVec;

wasm_trap_t *fd_write_import(void *env, const wasm_val_vec_t *args,
                             wasm_val_vec_t *results) {
    HANDLER_INIT(fd_write, 4)

    // extract parameters
    WASM_INT_C_TYPE fd = wasm_val_to_native_int(args->data[0]);
    WASM_POINTER_C_TYPE iovs = wasm_val_to_native_int(args->data[1]);
    WASM_INT_C_TYPE iovs_len = wasm_val_to_native_int(args->data[2]);
    WASM_POINTER_C_TYPE pnum = wasm_val_to_native_int(args->data[3]);

    // DRV_DEBUG("fd=%d, iovs=%p, iovs_len=%d, pnum=%p", fd, iovs, iovs_len, pnum);

    WASM_INT_C_TYPE written = 0;
    for (int i = 0; i < iovs_len; i++) {
        WASM_POINTER_C_TYPE offset = sizeof(WASMIOVec) * i;

        WASMIOVec iovec;
        memset(&iovec, 0, sizeof(WASMIOVec));
        if (wasm_copy_struct_safe(wasm_memory, iovs + offset, &iovec,
                                  sizeof(WASMIOVec), 4)) {
            DRV_DEBUG("fd_write_import: failed to copy iov");
            return NULL;
        }
        // DRV_DEBUG("fd_write_import: iovec1.buf_ptr=%p, iovec1.buf_len=%d", iovec.buf_ptr, iovec.buf_len);

        char *str = wasm_copy_string_len_safe(wasm_memory, iovec.buf_ptr,
                                              iovec.buf_len);
        DRV_DEBUG("fd_write[%d]: %s", fd, str);

        written += iovec.buf_len;
    }

    // TODO, safely :)
    // DRV_DEBUG("fd_write_import: written=%d", written);
    memcpy(wasm_memory_data(wasm_memory) + pnum, (void *)&written, 4);

    // set error output as 0 (success)
    results->data[0].kind = WASM_INT_KIND;
    results->data[0].of.i32 = 0;

    return NULL;
}

static int MEMCPY_CALL_COUNT = 0;

wasm_trap_t *emscripten_memcpy_js_import(void *env, const wasm_val_vec_t *args,
                                         wasm_val_vec_t *results) {
    // HANDLER_INIT(emscripten_memcpy_js, 3);

    ImportHook *import_hook = (ImportHook *)env;
    Proc *proc = import_hook->proc;
    wasm_store_t *store = proc->store;
    wasm_memory_t *wasm_memory = get_memory(proc);
    byte_t *wasm_base_ptr = wasm_memory_data(wasm_memory);

    // __emscripten_memcpy_js = (dest, src, num)

    WASM_POINTER_C_TYPE wasm_dest = wasm_val_to_native_int(args->data[0]);
    WASM_POINTER_C_TYPE wasm_src = wasm_val_to_native_int(args->data[1]);
    WASM_INT_C_TYPE num = wasm_val_to_native_int(args->data[2]);

    byte_t *dest = wasm_base_ptr + wasm_dest;
    byte_t *src = wasm_base_ptr + wasm_src;

    // printf("memcpy: #%d @ %dB, (%p) -> (%p)", ++MEMCPY_CALL_COUNT, num, wasm_src, wasm_dest);

    // DRV_DEBUG("emscripten_memcpy_js_import: dest=%p, src=%p, num=%d", dest, src, num);
    // TODO: !!! Make this safe :)
    memcpy(dest, src, num);

    // printf(" ...done!\n");

    return 0;
}

wasm_func_callback_with_env_t webgpu_wasm_callback(wasm_byte_t *module_name,
                                                   wasm_byte_t *name) {
    // wasi fns
    if (strcmp(module_name, "wasi_snapshot_preview1") == 0) {
        if (strcmp(name, "environ_sizes_get") == 0) {
            return environ_sizes_get_import;
        } else if (strcmp(name, "environ_get") == 0) {
            return environ_get_import;
        } else if (strcmp(name, "fd_write") == 0) {
            return fd_write_import;
        }
    }

    // emscripten fns
    if (strcmp(module_name, "env") == 0) {
        if (strcmp(name, "_emscripten_memcpy_js") == 0) {
            return emscripten_memcpy_js_import;
        }
    }

    // webgpu fns
    if (strcmp(module_name, "env") == 0) {
        // custom
        for (size_t i = 0; i < sizeof(webgpu_import_funcs_custom) /
                                   sizeof(webgpu_import_funcs_custom[0]);
             i++) {
            if (strcmp(webgpu_import_funcs_custom[i].name, name) == 0) {
                return webgpu_import_funcs_custom[i].func;
            }
        }

        // generated
        for (size_t i = 0;
             i < sizeof(webgpu_import_funcs) / sizeof(webgpu_import_funcs[0]);
             i++) {
            if (strcmp(webgpu_import_funcs[i].name, name) == 0) {
                return webgpu_import_funcs[i].func;
            }
        }
    }

    return NULL;
}
