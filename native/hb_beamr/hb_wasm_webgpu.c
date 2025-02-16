#include "include/hb_wasm_webgpu.h"
#include "include/hb_helpers.h"
#include "include/hb_logging.h"
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
#define FATAL(...) do { DRV_DEBUG(__VA_ARGS__); abort(); } while (0)

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

BindWGPUObjectMappingRegistry registry;

#define WGPU_LOG_LEVEL WGPULogLevel_Trace

// typedef void (*WGPULogCallback)(WGPULogLevel level, char const * message,
// void * userdata);
void webgpu_log_callback(WGPULogLevel level, const char *message,
                         void *userdata) {
    DRV_DEBUG("WGPU[%d]: %s", level, message);
}

// Placeholder macro for error handling (can be overridden by user if needed).
#ifndef WASM_MEMORY_ERROR
#define WASM_MEMORY_ERROR(msg, ...)                                                 \
    do {                                                                       \
        DRV_DEBUG("WASM Memory Error: %s\n", msg, __VA_ARGS__);                             \
    } while (0)
#endif

int wasm_copy_int_safe(wasm_memory_t *memory, uint32_t offset, int *out_int) {
    assert(memory != NULL);
    assert(out_int != NULL);

    // Get the start of the memory and its size.
    byte_t *mem_base = wasm_memory_data(memory);
    size_t mem_size = wasm_memory_data_size(memory);

    // Bounds checking: ensure [offset, offset+struct_size) is within memory.
    // Also protect against overflow in pointer arithmetic.
    if (offset > mem_size || sizeof(int) > mem_size ||
        offset > mem_size - sizeof(int)) {
        WASM_MEMORY_ERROR("Attempt to read int out of WASM memory bounds");
        return 1;
    }

    *out_int = *(int *)(mem_base + offset);
    return 0;
}

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

/**
 * Safely copy a string from WASM memory to native memory.
 * Returns NULL if the string pointer is invalid or memory allocation fails.
 */
char *wasm_copy_string_null_safe(wasm_memory_t *memory, uint32_t str_ptr) {
    if (str_ptr == 0)
        return NULL;

    byte_t *mem_base = wasm_memory_data(memory);
    size_t mem_size = wasm_memory_data_size(memory);

    // Bounds check the initial pointer
    if (str_ptr >= mem_size) {
        WASM_MEMORY_ERROR("String pointer out of bounds");
        return NULL;
    }

    // Get string length safely
    const char *wasm_str = (const char *)(mem_base + str_ptr);
    size_t max_len = mem_size - str_ptr;
    size_t str_len = strnlen(wasm_str, max_len);

    if (str_len == max_len) {
        WASM_MEMORY_ERROR(
            "String not null-terminated or extends beyond memory");
        return NULL;
    }

    // Allocate and copy
    char *native_str = malloc(str_len + 1);
    if (!native_str) {
        WASM_MEMORY_ERROR("Failed to allocate memory for string");
        return NULL;
    }

    memcpy(native_str, wasm_str, str_len + 1);
    return native_str;
}

// 0: Out pointer safely set to memory location
// 1: Out pointer set to null
// 2: Wasm pointer out of bounds, out pointer unset
int wasm_get_buffer_pointer_safe(wasm_memory_t *memory,
                                 uint32_t wasm_buffer_ptr, size_t buffer_size,
                                 uintptr_t *out_native_ptr) {
    if (wasm_buffer_ptr == 0) {
        *out_native_ptr = 0;
        DRV_DEBUG("Setting out_native_ptr to 0");
        return 1;
    };

    byte_t *wasm_mem_base = wasm_memory_data(memory);
    size_t wasm_mem_size = wasm_memory_data_size(memory);
    if (wasm_buffer_ptr >= wasm_mem_size || buffer_size > wasm_mem_size ||
        wasm_buffer_ptr + buffer_size > wasm_mem_size) {
        DRV_DEBUG("wasm_buffer_ptr out of bounds: wasm_buffer_ptr = %zu, "
                  "buffer_size = %zu, wasm_mem_size = %zu",
                  wasm_buffer_ptr, buffer_size, wasm_mem_size);
        return 2;
    };

    *out_native_ptr = (uintptr_t)wasm_mem_base + wasm_buffer_ptr;

    DRV_DEBUG(
        "wasm_get_buffer_pointer_safe: wasm_mem_base = %p, wasm_buffer_ptr = "
        "%zu, buffer_size = %zu, native_buffer_ptr = %p",
        (void *)wasm_mem_base, wasm_buffer_ptr, buffer_size,
        (void *)*out_native_ptr);
    return 0;
}

#define GET_WASM_SYS_INT(data) (data).of.WASM_VAL_INT_PROP

static inline WASM_INT_C_TYPE wasm_val_to_native_int(wasm_val_t wasm_val) {
    if (wasm_val.kind != WASM_INT_KIND) {
        DRV_DEBUG("wasm_val_to_native_int: expected %s, got kind: %d",
                  WASM_INT_KIND, wasm_val.kind);
    } else {
        // DRV_DEBUG("wasm_val_to_native_int: got kind: %d", WASM_INT_KIND);
    }
    return GET_WASM_SYS_INT(wasm_val);
}

// Boilerplate for handlers
#define HANDLER_INIT(name, args_expected)                                      \
    DRV_DEBUG(#name " called");                                                \
    ImportHook *import_hook = (ImportHook *)env;                               \
    Proc *proc = import_hook->proc;                                            \
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
    Proc *proc;
    WASM_INT_C_TYPE func_index;
} CallbackContext;

typedef struct {
    WASM_INT_C_TYPE queue;
    uint64_t submissionIndex;
} WASMWrappedSubmissionIndex;

WGPUWrappedSubmissionIndex *
extract_wrapped_submission_index(wasm_memory_t *memory,
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

    wrapped_submission_index->queue =
        (WGPUQueue)registry_item_get_mapping(&registry.queues, wasm_wrapped_submission_index.queue);
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
        &registry.devices, wasm_val_to_native_int(args->data[0]));

    // Get wait
    WGPUBool wait = (WGPUBool)wasm_val_to_native_int(args->data[1]);

    // Get wrappedSubmissionIndex
    WGPUWrappedSubmissionIndex *wrappedSubmissionIndex =
        extract_wrapped_submission_index(wasm_memory,
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

// WASM-friendly struct definitions to match memory layout
typedef struct {
    WASM_POINTER_C_TYPE next; // 4 bytes for WASM pointer
    WASM_INT_C_TYPE sType;    // 4 bytes for enum
} WASMChainedStruct;

typedef struct {
    WASMChainedStruct chain;
    uint32_t code; // 4 bytes for WASM pointer
} WASMShaderModuleWGSLDescriptor;

int MAX_CHAIN_DEPTH = 8;

// WGPU_EXPORT WGPUInstance wgpuCreateInstance(WGPU_NULLABLE WGPUInstanceDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuCreateInstanceImport(void *env, const wasm_val_vec_t *args,
                                      wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuCreateInstanceImport, 1);


    WGPUInstanceDescriptor descriptor;
    memset(&descriptor, 0, sizeof(WGPUInstanceDescriptor));
    extract_instance_descriptor(
        &registry, 
        wasm_memory, 
        (WasmWGPUInstanceDescriptor *)wasm_val_to_native_int(args->data[0]), 
        &descriptor
    );

    DRV_DEBUG("Parameters: descriptor=%p", &descriptor);
    WGPUInstance instance = wgpuCreateInstance(&descriptor);
    DRV_DEBUG("Result: WGPUInstance: %p", (uintptr_t)instance);

    size_t instance_index = registry_item_add_mapping(&registry.instances, (void *)instance);

    DRV_DEBUG("Setting result to WGPUInstance WASM_EXTERNREF");
    results->size = 1;
    results->data[0].kind = WASM_INT_KIND;
    results->data[0].of.i32 = instance_index;

    return NULL;
}

// Function pointer for callback inside WASM
CallbackContext wgpuInstanceRequestAdapterCallbackContext = {};
void wgpuInstanceRequestAdapterCallbackNative(WGPURequestAdapterStatus status,
                                              WGPUAdapter adapter,
                                              char const *message,
                                              void *userdata) {
    DRV_DEBUG("wgpuInstanceRequestAdapterCallbackNative called");
    DRV_DEBUG("Parameters: status=%d, adapter=%p, message=%s, userdata=%p",
              status, (void *)adapter, message, userdata);

    // Map the adapter
    size_t adapter_index = registry_item_add_mapping(&registry.adapters, (void*)adapter);

    DRV_DEBUG("Getting the callback context");
    Proc *proc = wgpuInstanceRequestAdapterCallbackContext.proc;
    WASM_INT_C_TYPE callback_index =
        wgpuInstanceRequestAdapterCallbackContext.func_index;

    DRV_DEBUG("Getting __indirect_function_table");
    wasm_table_t *indirect_functions_table = proc->indirect_func_table;

    DRV_DEBUG("Looking up function index: %d", callback_index);
    wasm_ref_t *callback_ref =
        wasm_table_get(indirect_functions_table, callback_index);
    DRV_DEBUG("Callback ref: %p", callback_ref);
    wasm_func_t *callback = wasm_ref_as_func(callback_ref);
    DRV_DEBUG("Callback: %p", callback);

    DRV_DEBUG(
        "Creating args/results for wgpuInstanceRequestAdapterCallbackWasm");
    wasm_val_vec_t args;
    wasm_val_vec_new_uninitialized(&args, 4);
    args.data[0].kind = WASM_INT_KIND;
    args.data[0].of.i32 = status;
    args.data[1].kind = WASM_INT_KIND;
    args.data[1].of.i32 = adapter_index;
    args.data[2].kind = WASM_INT_KIND;
    // TODO: Allocate memory for the message, copy data, and set the ref
    args.data[2].of.i32 = 0;
    args.data[3].kind = WASM_INT_KIND;
    args.data[3].of.i32 = (WASM_INT_C_TYPE)(uintptr_t)userdata;
    args.num_elems = 4;

    // create results (void)
    wasm_val_vec_t results;
    wasm_val_vec_new_uninitialized(&results, 0);

    // call the WASM function
    DRV_DEBUG("Calling wgpuInstanceRequestAdapterCallbackWasm");
    wasm_trap_t *trap = wasm_func_call(callback, &args, &results);
    if (trap != NULL) {
        wasm_message_t msg;
        wasm_trap_message(trap, &msg);
        DRV_DEBUG("Error calling wgpuInstanceRequestAdapterCallbackWasm: %.*s",
                  msg.size, msg.data);
        return;
    }

    DRV_DEBUG("Successfully called wgpuInstanceRequestAdapterCallbackWasm");
}

// WGPU_NULLABLE WGPURequestAdapterOptions const * options, WGPUInstanceRequestAdapterCallback callback, WGPU_NULLABLE void * userdata) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuInstanceRequestAdapterImport(void *env,
                                              const wasm_val_vec_t *args,
                                              wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuInstanceRequestAdapterImport, 4);

    size_t instance_mapping_index = wasm_val_to_native_int(args->data[0]);
    DRV_DEBUG("instance_mapping_index = %zu", instance_mapping_index);

    if (registry.instances.count == 0 && instance_mapping_index == 1) {
        // This might be emscripten, which has an implicit global instance at
        size_t stored = registry_item_add_mapping(&registry.instances, (void*)wgpuCreateInstance(NULL));
        DRV_DEBUG("Stored instance at index 1: %zu", stored);
    }
    DRV_DEBUG("Getting instance id: %zu", instance_mapping_index);
    WGPUInstance instance =
        (WGPUInstance)registry_item_get_mapping(&registry.instances, instance_mapping_index);
    DRV_DEBUG("instance = %p", (void *)instance);

    WGPURequestAdapterOptions *options = NULL;
    extract_request_adapter_options(&registry, wasm_memory, (byte_t *)wasm_val_to_native_int(args->data[1]), &options);

    // an index into the module’s function table
    WASM_INT_C_TYPE callback_index = wasm_val_to_native_int(args->data[2]);
    WGPUInstanceRequestAdapterCallback callback = NULL;
    if (callback_index == 0) {
        DRV_DEBUG("wgpuInstanceRequestAdapterCallbackWasmFuncTableRef is 0, "
                  "using NULL native callback");
    } else {
        DRV_DEBUG("wgpuInstanceRequestAdapterCallbackWasmFuncTableRef is %d",
                  callback_index);
        callback = wgpuInstanceRequestAdapterCallbackNative;
        // Store the callback context in a global variable
        wgpuInstanceRequestAdapterCallbackContext.proc = proc;
        wgpuInstanceRequestAdapterCallbackContext.func_index = callback_index;
    }
    // Keep this as a WASM pointer since it's not used in the native code
    WASM_INT_C_TYPE userdata_wasm_ptr = wasm_val_to_native_int(args->data[3]);

    // Call the actual function
    DRV_DEBUG("Parameters: instance=%p, options=%p, callback=%p, userdata=%p",
              (void *)instance, (void *)options, (void *)callback,
              (void *)(uintptr_t)userdata_wasm_ptr);
    wgpuInstanceRequestAdapter(instance, options, callback,
                               (void *)(uintptr_t)userdata_wasm_ptr);

    return NULL;
}

// Function pointer for callback inside WASM
CallbackContext wgpuInstanceRequestDeviceCallbackContext = {};
void wgpuInstanceRequestDeviceCallbackNative(WGPURequestDeviceStatus status,
                                             WGPUDevice device,
                                             char const *message,
                                             void *userdata) {
    DRV_DEBUG("wgpuInstanceRequestDeviceCallbackNative called");
    DRV_DEBUG("Parameters: status=%d, device=%p, message=%s, userdata=%p",
              status, (void *)device, message, userdata);

    // Map the device
    DRV_DEBUG("Mapping device");
    size_t device_index = registry_item_add_mapping(&registry.devices, (void *)device);
    DRV_DEBUG("Device index: %d", device_index);

    DRV_DEBUG("Getting the callback context");
    Proc *proc = wgpuInstanceRequestDeviceCallbackContext.proc;
    WASM_INT_C_TYPE callback_index =
        wgpuInstanceRequestDeviceCallbackContext.func_index;

    DRV_DEBUG("Getting __indirect_function_table");
    wasm_table_t *indirect_function_table = proc->indirect_func_table;

    DRV_DEBUG("Looking up function index: %d", callback_index);
    wasm_ref_t *callback_ref =
        wasm_table_get(indirect_function_table, callback_index);
    DRV_DEBUG("Callback ref: %p", callback_ref);
    wasm_func_t *callback = wasm_ref_as_func(callback_ref);
    DRV_DEBUG("Callback: %p", callback);

    DRV_DEBUG(
        "Creating args/results for wgpuInstanceRequestDeviceCallbackNative");
    wasm_val_vec_t args;
    wasm_val_vec_new_uninitialized(&args, 4);
    args.data[0].kind = WASM_INT_KIND;
    args.data[0].of.i32 = status;
    args.data[1].kind = WASM_INT_KIND;
    args.data[1].of.i32 = device_index;
    args.data[2].kind = WASM_INT_KIND;
    // TODO: Allocate memory for the message, copy data, and set the ref
    args.data[2].of.i32 = 0;
    args.data[3].kind = WASM_INT_KIND;
    args.data[3].of.i32 = (WASM_INT_C_TYPE)(uintptr_t)userdata;
    args.num_elems = 4;

    // create results (void)
    wasm_val_vec_t results;
    wasm_val_vec_new_uninitialized(&results, 0);

    // call the WASM function
    DRV_DEBUG("Calling wgpuInstanceRequestDeviceCallbackWasm");
    wasm_trap_t *trap = wasm_func_call(callback, &args, &results);
    if (trap != NULL) {
        wasm_message_t msg;
        wasm_trap_message(trap, &msg);
        DRV_DEBUG("Error calling wgpuInstanceRequestDeviceCallbackWasm: %.*s",
                  msg.size, msg.data);
        return;
    }

    DRV_DEBUG("Successfully called wgpuInstanceRequestDeviceCallbackWasm");
}

// WGPU_EXPORT void wgpuAdapterRequestDevice(WGPUAdapter adapter, WGPU_NULLABLE WGPUDeviceDescriptor const * descriptor, WGPUAdapterRequestDeviceCallback callback, WGPU_NULLABLE void * userdata) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuAdapterRequestDeviceImport(void *env,
                                            const wasm_val_vec_t *args,
                                            wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuAdapterRequestDeviceImport, 4);

    wgpuSetLogLevel(WGPULogLevel_Trace);
    wgpuSetLogCallback(webgpu_log_callback, NULL);

    size_t adapter_mapping_index = wasm_val_to_native_int(args->data[0]);
    DRV_DEBUG("adapter_mapping_index = %zu", adapter_mapping_index);
    WGPUAdapter adapter =
        (WGPUAdapter)registry_item_get_mapping(&registry.adapters, adapter_mapping_index);
    DRV_DEBUG("adapter = %p", adapter);

    WGPUDeviceDescriptor *descriptor = NULL;
    extract_device_descriptor(&registry, wasm_memory, (WasmWGPUDeviceDescriptor *)args->data[1].of.WASM_VAL_INT_PROP, &descriptor);
    DRV_DEBUG("descriptor = %p", descriptor);

    // an index into the module’s function table
    WASM_INT_C_TYPE callback_index = wasm_val_to_native_int(args->data[2]);
    WGPUAdapterRequestDeviceCallback callback = NULL;
    if (callback_index == 0) {
        DRV_DEBUG("wgpuInstanceRequestDeviceCallbackWasmFuncTableRef is 0, "
                  "using NULL native callback");
    } else {
        DRV_DEBUG("wgpuInstanceRequestDeviceCallbackWasmFuncTableRef is %d",
                  callback_index);
        callback = wgpuInstanceRequestDeviceCallbackNative;
        // Store the callback context in a global variable
        wgpuInstanceRequestDeviceCallbackContext.proc = proc;
        wgpuInstanceRequestDeviceCallbackContext.func_index = callback_index;
    }

    // Keep this as a WASM pointer since it's not used in the native code
    WASM_INT_C_TYPE userdata_wasm_ptr = wasm_val_to_native_int(args->data[3]);

    // Call the actual function
    DRV_DEBUG("Parameters: adapter=%p, descriptor=%p, callback=%p, userdata=%p",
              (void *)adapter, (void *)descriptor, (void *)callback,
              (void *)(uintptr_t)userdata_wasm_ptr);
    wgpuAdapterRequestDevice(adapter, descriptor, callback,
                             (void *)(uintptr_t)userdata_wasm_ptr);

    return NULL;
}

// WGPU_EXPORT WGPUQueue wgpuDeviceGetQueue(WGPUDevice device) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuDeviceGetQueueImport(void *env, const wasm_val_vec_t *args,
                                      wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuDeviceGetQueue, 1);

    size_t device_mapping_index = wasm_val_to_native_int(args->data[0]);
    WGPUDevice device = (WGPUDevice)registry_item_get_mapping(&registry.devices, device_mapping_index);

    // Call the actual function
    DRV_DEBUG("Parameters: device=%p", (void *)device);
    WGPUQueue queue = wgpuDeviceGetQueue(device);

    size_t queue_mapping_index = registry_item_add_mapping(&registry.queues, (void *)queue);

    // Return the queue handle
    results->size = 1;
    results->data[0].kind = WASM_INT_KIND;
    results->data[0].of.i32 = (uint64_t)queue_mapping_index;

    return NULL;
}

// WGPU_EXPORT WGPUShaderModule wgpuDeviceCreateShaderModule(WGPUDevice device, WGPUShaderModuleDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuDeviceCreateShaderModuleImport(void *env,
                                                const wasm_val_vec_t *args,
                                                wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuDeviceCreateShaderModule, 2);

    // Get device parameter from mapping
    size_t device_mapping_index = wasm_val_to_native_int(args->data[0]);
    DRV_DEBUG("Device mapping index: %zu", device_mapping_index);
    WGPUDevice device = (WGPUDevice)registry_item_get_mapping(&registry.devices, device_mapping_index);
    if (!device) {
        DRV_DEBUG("Invalid device handle");
        results->size = 1;
        results->data[0].kind = WASM_INT_KIND;
        results->data[0].of.i32 = 0;
        return NULL;
    }

    // Get shader module descriptor from WASM memory
    uint32_t descriptor_offset =
        (uint32_t)wasm_val_to_native_int(args->data[1]);
    DRV_DEBUG("Descriptor offset: 0x%x", descriptor_offset);
    WGPUShaderModuleDescriptor *descriptor = NULL;
    if (extract_shader_module_descriptor(&registry, wasm_memory, (byte_t *)descriptor_offset, &descriptor)) {
        DRV_DEBUG("Failed to extract shader module descriptor");
        results->size = 1;
        results->data[0].kind = WASM_INT_KIND;
        results->data[0].of.i32 = 0;
        return NULL;
    }

    // Debug print shader module descriptor
    DRV_DEBUG("smd (%p)", &descriptor);
    DRV_DEBUG("smd label: %s", descriptor->label);
    DRV_DEBUG("smd hint count: %d", descriptor->hintCount);
    for (size_t i = 0; i < descriptor->hintCount; i++) {
        DRV_DEBUG("smd hint: %p", &descriptor->hints[i]);
    }
    DRV_DEBUG("smd next ptr: %p", descriptor->nextInChain);
    WGPUShaderModuleWGSLDescriptor *next = (WGPUShaderModuleWGSLDescriptor *)descriptor->nextInChain;
    DRV_DEBUG("smd next (%p)", &next);
    DRV_DEBUG("smd next code: %s", next->code);
    DRV_DEBUG("smd next sType: %d", next->chain.sType);
    DRV_DEBUG("smd next next: %p", next->chain.next);

    // Call the actual function
    DRV_DEBUG("Creating shader module with device=%p, descriptor=%p",
              (void *)device, (void *)descriptor);

    WGPUShaderModule shader_module =
        wgpuDeviceCreateShaderModule(device, descriptor);
    DRV_DEBUG("Created shader module: %p", (void *)shader_module);
    DRV_DEBUG("shader module: %p", shader_module);

    // Clean up the descriptor
    if (descriptor->label)
        free((void *)descriptor->label);
    if (descriptor->nextInChain) {
        if (((WGPUShaderModuleWGSLDescriptor *)descriptor->nextInChain)->code) {
            free((void *)((WGPUShaderModuleWGSLDescriptor *)
                              descriptor->nextInChain)
                     ->code);
        }
        free((void *)descriptor->nextInChain);
    }

    // Store the shader module in our registry
    size_t shader_module_mapping_index =
        registry_item_add_mapping(&registry.shaderModules, (void *)shader_module);
    DRV_DEBUG("Stored shader module at index: %zu",
              shader_module_mapping_index);

    // Return the shader module handle
    results->size = 1;
    results->data[0].kind = WASM_INT_KIND;
    results->data[0].of.WASM_VAL_INT_PROP =
        (uint64_t)shader_module_mapping_index;

    return NULL;
}

// WGPU_EXPORT WGPUBuffer wgpuDeviceCreateBuffer(WGPUDevice device, WGPUBufferDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuDeviceCreateBufferImport(void *env, const wasm_val_vec_t *args,
                                          wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuDeviceCreateBuffer, 2);

    // Get device parameter from mapping
    size_t device_mapping_index = wasm_val_to_native_int(args->data[0]);
    DRV_DEBUG("Device mapping index: %zu", device_mapping_index);
    WGPUDevice device = (WGPUDevice)registry_item_get_mapping(&registry.devices, device_mapping_index);
    if (!device) {
        DRV_DEBUG("Invalid device handle");
        results->size = 1;
        results->data[0].kind = WASM_INT_KIND;
        results->data[0].of.WASM_VAL_INT_PROP = 0;
        return NULL;
    }

    // Get buffer descriptor from WASM memory
    uint32_t descriptor_offset =
        (uint32_t)wasm_val_to_native_int(args->data[1]);
    DRV_DEBUG("Descriptor offset: 0x%x", descriptor_offset);
    WGPUBufferDescriptor *descriptor = NULL;
    if (extract_buffer_descriptor(&registry, wasm_memory, (byte_t *)descriptor_offset, &descriptor)) {
        DRV_DEBUG("Failed to extract buffer descriptor");
        results->size = 1;
        results->data[0].kind = WASM_INT_KIND;
        results->data[0].of.WASM_VAL_INT_PROP = 0;
        return NULL;
    }

    // Call the actual function
    DRV_DEBUG("Creating buffer with device=%p, descriptor=%p", (void *)device,
              (void *)descriptor);

    DRV_DEBUG("debug print descriptor");
    DRV_DEBUG("descriptor label: %s", descriptor->label);
    DRV_DEBUG("descriptor usage: %d", descriptor->usage);
    DRV_DEBUG("descriptor size: %d", descriptor->size);
    DRV_DEBUG("descriptor mappedAtCreation: %d", descriptor->mappedAtCreation);
    
    WGPUBuffer buffer = wgpuDeviceCreateBuffer(device, descriptor);
    DRV_DEBUG("Created buffer: %p", (void *)buffer);

    // Store the buffer in our registry
    size_t buffer_mapping_index = registry_item_add_mapping(&registry.buffers, (void *)buffer);
    DRV_DEBUG("Stored buffer at index: %zu", buffer_mapping_index);

    // Return the buffer handle
    results->size = 1;
    results->data[0].kind = WASM_INT_KIND;
    results->data[0].of.WASM_VAL_INT_PROP = (uint64_t)buffer_mapping_index;

    return NULL;
}

// WGPU_EXPORT WGPUComputePipeline wgpuDeviceCreateComputePipeline(WGPUDevice device, WGPUComputePipelineDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuDeviceCreateComputePipelineImport(void *env,
                                                   const wasm_val_vec_t *args,
                                                   wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuDeviceCreateComputePipeline, 2);

    // Get device from registry
    WGPUDevice device = (WGPUDevice)registry_item_get_mapping(
        &registry.devices, wasm_val_to_native_int(args->data[0]));

    // Extract the descriptor
    WGPUComputePipelineDescriptor *descriptor = NULL;
    if (extract_compute_pipeline_descriptor(
        &registry, wasm_memory, (byte_t *)wasm_val_to_native_int(args->data[1]), &descriptor)) {
        DRV_DEBUG("Failed to extract compute pipeline descriptor");
        results->data[0].kind = WASM_INT_KIND;
        results->data[0].of.WASM_VAL_INT_PROP = 0;
        return NULL;
    }

    // Debug log the descriptor before creation
    DRV_DEBUG("Descriptor - nextInChain: %p, label: %s, layout: %p",
              (void *)descriptor->nextInChain,
              descriptor->label ? descriptor->label : "(null)",
              (void *)descriptor->layout);
    // Log the compute
    DRV_DEBUG(
        "  - compute - nextInChain: %p, module handle: %p, entryPoint: %s",
        (void *)descriptor->compute.nextInChain,
        (void *)descriptor->compute.module,
        descriptor->compute.entryPoint ? descriptor->compute.entryPoint
                                       : "(null)");

    // Create the pipeline
    WGPUComputePipeline pipeline =
        wgpuDeviceCreateComputePipeline(device, descriptor);
    DRV_DEBUG("Created compute pipeline: %p", (void *)pipeline);

    // Add pipeline to registry and return its index
    size_t pipeline_index =
        registry_item_add_mapping(&registry.computePipelines, (void*)pipeline);
    results->data[0].kind = WASM_INT_KIND;
    results->data[0].of.WASM_VAL_INT_PROP = pipeline_index;

    return NULL;
}

// WGPU_EXPORT WGPURenderPipeline wgpuDeviceCreateRenderPipeline(WGPUDevice device, WGPURenderPipelineDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuDeviceCreateRenderPipelineImport(void *env,
                                                  const wasm_val_vec_t *args,
                                                  wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuDeviceCreateRenderPipeline, 2);

    WGPUDevice device = (WGPUDevice)registry_item_get_mapping(
        &registry.devices, wasm_val_to_native_int(args->data[0]));
    DRV_DEBUG("Device: %p", (void *)device);

    DRV_DEBUG("Creating render pipeline descriptor...");
    WGPURenderPipelineDescriptor *descriptor = NULL;
    extract_render_pipeline_descriptor(
            &registry, wasm_memory, (byte_t *)wasm_val_to_native_int(args->data[1]), &descriptor);

    WGPURenderPipeline pipeline = wgpuDeviceCreateRenderPipeline(device, descriptor);
    if (!pipeline) {
        DRV_DEBUG("Failed to create render pipeline");
        return NULL;
    }

    // Add pipeline to registry and return its index
    size_t pipeline_index = registry_item_add_mapping(&registry.renderPipelines, (void *)pipeline);
    DRV_DEBUG("Pipeline (%p) index: %d", pipeline, pipeline_index);
    
    results->data[0].kind = WASM_INT_KIND;
    results->data[0].of.WASM_VAL_INT_PROP = pipeline_index;

    return NULL;
}

// WGPU_EXPORT WGPUBindGroupLayout wgpuComputePipelineGetBindGroupLayout(WGPUComputePipeline computePipeline, uint32_t groupIndex) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuComputePipelineGetBindGroupLayoutImport(
    void *env, const wasm_val_vec_t *args, wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuComputePipelineGetBindGroupLayout, 2);

    // Get compute pipeline from registry
    WGPUComputePipeline pipeline = (WGPUComputePipeline)registry_item_get_mapping(
        &registry.computePipelines, wasm_val_to_native_int(args->data[0]));

    // Get group index
    uint32_t group_index = (uint32_t)wasm_val_to_native_int(args->data[1]);
    DRV_DEBUG("Group index: %u", group_index);

    // Get the bind group layout
    WGPUBindGroupLayout layout =
        wgpuComputePipelineGetBindGroupLayout(pipeline, group_index);
    DRV_DEBUG("Retrieved bind group layout: %p", (void *)layout);

    // Add layout to registry and return its index
    size_t layout_index = registry_item_add_mapping(&registry.bindGroupLayouts, (void *)layout);
    results->data[0].kind = WASM_INT_KIND;
    results->data[0].of.WASM_VAL_INT_PROP = layout_index;

    return NULL;
}

// WGPU_EXPORT WGPUBindGroup wgpuDeviceCreateBindGroup(WGPUDevice device, WGPUBindGroupDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuDeviceCreateBindGroupImport(void *env,
                                             const wasm_val_vec_t *args,
                                             wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuDeviceCreateBindGroup, 2);

    WASM_INT_C_TYPE device_index = wasm_val_to_native_int(args->data[0]);
    DRV_DEBUG("Retrieved device index: %u", device_index);

    // Get device from registry
    WGPUDevice device = (WGPUDevice)registry_item_get_mapping(&registry.devices, device_index);
    DRV_DEBUG("Retrieved device: %p", (void *)device);

    // Extract the descriptor
    WGPUBindGroupDescriptor *descriptor = NULL;
    if (extract_bind_group_descriptor(
        &registry, wasm_memory, (byte_t *)wasm_val_to_native_int(args->data[1]), &descriptor)) {
        DRV_DEBUG("Failed to extract bind group descriptor");
        results->data[0].kind = WASM_INT_KIND;
        results->data[0].of.WASM_VAL_INT_PROP = 0;
        return NULL;
    }

    // Debug log the descriptor
    DRV_DEBUG("WGPUBindGroupDescriptor - nextInChain: %p, label: %s, layout: "
              "%p, entryCount: %zu",
              (void *)descriptor->nextInChain,
              descriptor->label ? descriptor->label : "(null)",
              (void *)descriptor->layout, descriptor->entryCount);
    // Print out all entries
    for (size_t i = 0; i < descriptor->entryCount; i++) {
        DRV_DEBUG("WGPUBindGroupEntry - entry[%zu] - binding: %u, buffer: %p, "
                  "offset: %lu, size: %lu, sampler: %p, textureView: %p",
                  i, descriptor->entries[i].binding,
                  (void *)descriptor->entries[i].buffer,
                  descriptor->entries[i].offset, descriptor->entries[i].size,
                  (void *)descriptor->entries[i].sampler,
                  (void *)descriptor->entries[i].textureView);
    }

    // Create the bind group
    WGPUBindGroup bind_group = wgpuDeviceCreateBindGroup(device, descriptor);
    DRV_DEBUG("Created bind group: %p", (void *)bind_group);

    // Add bind group to registry and return its index
    size_t bind_group_index = registry_item_add_mapping(&registry.bindGroups, (void*)bind_group);
    results->data[0].kind = WASM_INT_KIND;
    results->data[0].of.WASM_VAL_INT_PROP = bind_group_index;

    return NULL;
}

// WGPU_EXPORT WGPUCommandEncoder wgpuDeviceCreateCommandEncoder(WGPUDevice device, WGPU_NULLABLE WGPUCommandEncoderDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuDeviceCreateCommandEncoderImport(void *env,
                                                  const wasm_val_vec_t *args,
                                                  wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuDeviceCreateCommandEncoder, 2);

    // Get device from registry
    WASM_INT_C_TYPE device_index = wasm_val_to_native_int(args->data[0]);
    DRV_DEBUG("Retrieved device index: %u", device_index);

    WGPUDevice device = (WGPUDevice)registry_item_get_mapping(&registry.devices, device_index);
    DRV_DEBUG("Retrieved device: %p", (void *)device);

    // Extract the descriptor if provided
    WGPUCommandEncoderDescriptor *descriptor = NULL;
    uint32_t descriptor_ptr = wasm_val_to_native_int(args->data[1]);
    if (descriptor_ptr != 0) {
        if (extract_command_encoder_descriptor(&registry, wasm_memory, (byte_t *)descriptor_ptr, &descriptor)) {
            DRV_DEBUG("Failed to extract command encoder descriptor");
            results->data[0].kind = WASM_INT_KIND;
            results->data[0].of.WASM_VAL_INT_PROP = 0;
            return NULL;
        }
    }

    // Create the command encoder
    WGPUCommandEncoder encoder =
        wgpuDeviceCreateCommandEncoder(device, &descriptor);
    DRV_DEBUG("Created command encoder: %p", (void *)encoder);

    // Add command encoder to registry and return its index
    size_t encoder_index = registry_item_add_mapping(&registry.commandEncoders, (void *)encoder);
    results->data[0].kind = WASM_INT_KIND;
    results->data[0].of.WASM_VAL_INT_PROP = encoder_index;

    return NULL;
}

// WGPU_EXPORT WGPUComputePassEncoder wgpuCommandEncoderBeginComputePass(WGPUCommandEncoder commandEncoder, WGPU_NULLABLE WGPUComputePassDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *
wgpuCommandEncoderBeginComputePassImport(void *env, const wasm_val_vec_t *args,
                                         wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuCommandEncoderBeginComputePass, 2);

    // Get the command encoder from the first argument
    WGPUCommandEncoder commandEncoder = (WGPUCommandEncoder)registry_item_get_mapping(
        &registry.commandEncoders, wasm_val_to_native_int(args->data[0]));

    // Extract the descriptor from WASM memory if provided
    WGPUComputePassDescriptor descriptor;
    memset(&descriptor, 0, sizeof(descriptor));
    extract_compute_pass_descriptor(
        &registry, wasm_memory, (WasmWGPUComputePassDescriptor *)wasm_val_to_native_int(args->data[1]), &descriptor);

    // Call the native function
    WGPUComputePassEncoder encoder =
        wgpuCommandEncoderBeginComputePass(commandEncoder, &descriptor);

    // Store the result in the extern ref registry
    results->data[0].of.i32 =
        registry_item_add_mapping(&registry.computePassEncoders, (void *)encoder);

    return NULL;
}

// WGPU_EXPORT void wgpuComputePassEncoderSetPipeline(WGPUComputePassEncoder computePassEncoder, WGPUComputePipeline pipeline) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuComputePassEncoderSetPipelineImport(void *env,
                                                     const wasm_val_vec_t *args,
                                                     wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuComputePassEncoderSetPipeline, 2);

    // Get the compute pass encoder from the first argument
    WGPUComputePassEncoder encoder = (WGPUComputePassEncoder)registry_item_get_mapping(
        &registry.computePassEncoders, wasm_val_to_native_int(args->data[0]));

    // Get the compute pipeline from the second argument
    WGPUComputePipeline pipeline = (WGPUComputePipeline)registry_item_get_mapping(
        &registry.computePipelines, wasm_val_to_native_int(args->data[1]));

    // Call the native function
    wgpuComputePassEncoderSetPipeline(encoder, pipeline);

    return NULL;
}

// WGPU_EXPORT void wgpuComputePassEncoderSetBindGroup(WGPUComputePassEncoder computePassEncoder, uint32_t groupIndex, WGPU_NULLABLE WGPUBindGroup group, size_t dynamicOffsetCount, uint32_t const * dynamicOffsets) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *
wgpuComputePassEncoderSetBindGroupImport(void *env, const wasm_val_vec_t *args,
                                         wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuComputePassEncoderSetBindGroup, 5);

    // Get the compute pass encoder from the first argument
    WGPUComputePassEncoder encoder = (WGPUComputePassEncoder)registry_item_get_mapping(
        &registry.computePassEncoders, wasm_val_to_native_int(args->data[0]));

    // Get the group index from the second argument
    uint32_t groupIndex = wasm_val_to_native_int(args->data[1]);

    // Get the bind group from the third argument
    WGPUBindGroup group = NULL;
    if (wasm_val_to_native_int(args->data[2]) != 0) {
        group = (WGPUBindGroup)registry_item_get_mapping(
            &registry.bindGroups, wasm_val_to_native_int(args->data[2]));
    }

    // Get dynamic offset count and offsets array from the fourth and fifth
    // arguments
    size_t dynamicOffsetCount = wasm_val_to_native_int(args->data[3]);
    const uint32_t *dynamicOffsets = NULL;
    if (dynamicOffsetCount > 0 && wasm_val_to_native_int(args->data[4]) != 0) {
        // Get pointer to dynamic offsets array in WASM memory
        dynamicOffsets =
            (const uint32_t *)(wasm_memory_data(wasm_memory) +
                               wasm_val_to_native_int(args->data[4]));

        // Validate that the offsets array is within bounds
        size_t required_size = dynamicOffsetCount * sizeof(uint32_t);
        if (wasm_val_to_native_int(args->data[4]) + required_size >
            wasm_memory_data_size(wasm_memory)) {
            DRV_DEBUG("Dynamic offsets array out of bounds");
            return NULL;
        }
    }

    // Call the native function
    wgpuComputePassEncoderSetBindGroup(encoder, groupIndex, group,
                                       dynamicOffsetCount, dynamicOffsets);

    return NULL;
}

// WGPU_EXPORT void wgpuComputePassEncoderDispatchWorkgroups(WGPUComputePassEncoder computePassEncoder, uint32_t workgroupCountX, uint32_t workgroupCountY, uint32_t workgroupCountZ) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuComputePassEncoderDispatchWorkgroupsImport(
    void *env, const wasm_val_vec_t *args, wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuComputePassEncoderDispatchWorkgroups, 4);

    // Get the compute pass encoder from the first argument
    WGPUComputePassEncoder encoder = (WGPUComputePassEncoder)registry_item_get_mapping(
        &registry.computePassEncoders, wasm_val_to_native_int(args->data[0]));

    // Get workgroup counts from the remaining arguments
    uint32_t workgroupCountX = wasm_val_to_native_int(args->data[1]);
    uint32_t workgroupCountY = wasm_val_to_native_int(args->data[2]);
    uint32_t workgroupCountZ = wasm_val_to_native_int(args->data[3]);

    // Call the native function
    wgpuComputePassEncoderDispatchWorkgroups(encoder, workgroupCountX,
                                             workgroupCountY, workgroupCountZ);

    return NULL;
}

// WGPU_EXPORT void wgpuComputePassEncoderEnd(WGPUComputePassEncoder computePassEncoder) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuComputePassEncoderEndImport(void *env,
                                             const wasm_val_vec_t *args,
                                             wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuComputePassEncoderEnd, 1);

    // Get the compute pass encoder from the argument
    WGPUComputePassEncoder encoder = (WGPUComputePassEncoder)registry_item_get_mapping(
        &registry.computePassEncoders, wasm_val_to_native_int(args->data[0]));

    // Call the native function
    wgpuComputePassEncoderEnd(encoder);

    return NULL;
}

// Must be implemented: https://github.com/gfx-rs/wgpu-native/issues/412 WGPU_EXPORT void wgpuComputePassEncoderRelease(WGPUComputePassEncoder computePassEncoder) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuComputePassEncoderReleaseImport(void *env,
                                                 const wasm_val_vec_t *args,
                                                 wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuComputePassEncoderRelease, 1);

    // Get the compute pass encoder from the argument
    WGPUComputePassEncoder encoder = (WGPUComputePassEncoder)registry_item_get_mapping(
        &registry.computePassEncoders, wasm_val_to_native_int(args->data[0]));

    // Call the native function
    wgpuComputePassEncoderRelease(encoder);

    return NULL;
}

// Must be implemented: https://github.com/gfx-rs/wgpu-native/issues/412 WGPU_EXPORT void wgpuRenderPassEncoderRelease(WGPUComputePassEncoder computePassEncoder) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuRenderPassEncoderReleaseImport(void *env,
                                                 const wasm_val_vec_t *args,
                                                 wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuRenderPassEncoderRelease, 1);

    // Get the compute pass encoder from the argument
    WGPURenderPassEncoder encoder = (WGPURenderPassEncoder)registry_item_get_mapping(
        &registry.renderPassEncoders, wasm_val_to_native_int(args->data[0]));

    // Call the native function
    wgpuRenderPassEncoderRelease(encoder);

    return NULL;
}

// WGPU_EXPORT void wgpuCommandEncoderCopyBufferToBuffer(WGPUCommandEncoder commandEncoder, WGPUBuffer source, uint64_t sourceOffset, WGPUBuffer destination, uint64_t destinationOffset, uint64_t size) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuCommandEncoderCopyBufferToBufferImport(
    void *env, const wasm_val_vec_t *args, wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuCommandEncoderCopyBufferToBuffer, 9);

    // Get the command encoder from the first argument
    WGPUCommandEncoder encoder = (WGPUCommandEncoder)registry_item_get_mapping(
        &registry.commandEncoders, wasm_val_to_native_int(args->data[0]));

    // Get source buffer from the second argument
    WGPUBuffer source = (WGPUBuffer)registry_item_get_mapping(
        &registry.buffers, wasm_val_to_native_int(args->data[1]));

    // Get source offset from the third and fourth arguments (low and high 32
    // bits)
    uint64_t sourceOffset =
        ((uint64_t)wasm_val_to_native_int(args->data[3]) << 32) |
        (uint64_t)wasm_val_to_native_int(args->data[2]);

    // Get destination buffer from the fifth argument
    WGPUBuffer destination = (WGPUBuffer)registry_item_get_mapping(
        &registry.buffers, wasm_val_to_native_int(args->data[4]));

    // Get destination offset from the sixth and seventh arguments (low and high
    // 32 bits)
    uint64_t destinationOffset =
        ((uint64_t)wasm_val_to_native_int(args->data[6]) << 32) |
        (uint64_t)wasm_val_to_native_int(args->data[5]);

    // Get size from the eighth and ninth arguments (low and high 32 bits)
    uint64_t size = ((uint64_t)wasm_val_to_native_int(args->data[8]) << 32) |
                    (uint64_t)wasm_val_to_native_int(args->data[7]);

    // Call the native function
    wgpuCommandEncoderCopyBufferToBuffer(encoder, source, sourceOffset,
                                         destination, destinationOffset, size);

    return NULL;
}

// WGPU_EXPORT WGPUCommandBuffer wgpuCommandEncoderFinish(WGPUCommandEncoder commandEncoder, WGPU_NULLABLE WGPUCommandBufferDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuCommandEncoderFinishImport(void *env,
                                            const wasm_val_vec_t *args,
                                            wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuCommandEncoderFinish, 2);

    // Get the command encoder from the first argument
    WGPUCommandEncoder encoder = (WGPUCommandEncoder)registry_item_get_mapping(
        &registry.commandEncoders, wasm_val_to_native_int(args->data[0]));

    WGPUCommandBufferDescriptor *descriptor = NULL;
    extract_command_buffer_descriptor(
        &registry, wasm_memory, (WasmWGPUCommandBufferDescriptor *)wasm_val_to_native_int(args->data[1]), &descriptor);

    // Call the native function
    WGPUCommandBuffer command_buffer =
        wgpuCommandEncoderFinish(encoder, descriptor);

    // Add the command buffer to our registry and return the index
    size_t command_buffer_index =
        registry_item_add_mapping(&registry.commandBuffers, (void *)command_buffer);
    DRV_DEBUG("created command_buffer_index = %zu", command_buffer_index);
    results->data[0].kind = WASM_INT_KIND;
    results->data[0].of.i32 = command_buffer_index;

    return NULL;
}

// WGPU_EXPORT void wgpuQueueWriteBuffer(WGPUQueue queue, WGPUBuffer buffer, uint64_t bufferOffset, void const * data, size_t size) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuQueueWriteBufferImport(void *env, const wasm_val_vec_t *args,
                                        wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuQueueWriteBuffer, 6);

    WGPUQueue queue =
        (WGPUQueue)registry_item_get_mapping(&registry.queues, wasm_val_to_native_int(args->data[0]));

    WGPUBuffer buffer = (WGPUBuffer)registry_item_get_mapping(
        &registry.buffers, wasm_val_to_native_int(args->data[1]));

    uint64_t bufferOffset =
        ((uint64_t)wasm_val_to_native_int(args->data[3]) << 32) |
        (uint64_t)wasm_val_to_native_int(args->data[2]);

    size_t size = wasm_val_to_native_int(args->data[5]);

    uintptr_t native_buffer_ptr;
    int data_ptr_status = wasm_get_buffer_pointer_safe(
        wasm_memory, wasm_val_to_native_int(args->data[4]), size,
        &native_buffer_ptr);

    DRV_DEBUG("queue = %p, buffer = %p, bufferOffset = %llu, size = %zu, "
              "native_buffer_ptr = %p",
              queue, buffer, bufferOffset, size, native_buffer_ptr);

    // Debug print the entire buffer in hex
    for (int i = 0; i < size; i++) {
        DRV_DEBUG("%02x", ((byte_t *)native_buffer_ptr)[i]);
    }

    wgpuQueueWriteBuffer(queue, buffer, bufferOffset, (void *)native_buffer_ptr,
                         size);

    return NULL;
}

// WGPU_EXPORT void wgpuQueueSubmit(WGPUQueue queue, size_t commandCount, WGPUCommandBuffer const * commands) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuQueueSubmitImport(void *env, const wasm_val_vec_t *args,
                                   wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuQueueSubmit, 3);

    // Get the queue from registry
    WGPUQueue queue =
        (WGPUQueue)registry_item_get_mapping(&registry.queues, wasm_val_to_native_int(args->data[0]));

    // Get command count
    size_t commandCount = wasm_val_to_native_int(args->data[1]);

    WASM_INT_C_TYPE command_buffer_index_ptr =
        wasm_val_to_native_int(args->data[2]);
    // Get wasm memory at `command_buffer_index_ptr`
    WASM_INT_C_TYPE command_buffer_index =
        *(WASM_INT_C_TYPE *)(wasm_base_ptr + command_buffer_index_ptr);
    DRV_DEBUG("command_buffer_index_ptr = %p, command_buffer_index = %u",
              command_buffer_index_ptr, command_buffer_index);
    WGPUCommandBuffer command_buffer =
        (WGPUCommandBuffer)registry_item_get_mapping(&registry.commandBuffers, command_buffer_index);

    // Print arguments
    DRV_DEBUG("queue = %p, commandCount = %zu, command_buffer = %p", queue,
              commandCount, command_buffer);

    // Submit the commands
    wgpuQueueSubmit(queue, commandCount, &command_buffer);

    return NULL;
}

CallbackContext wgpuBufferMapAsyncCallbackContext = {};
void wgpuBufferMapAsyncCallbackNative(WGPUBufferMapAsyncStatus status,
                                      WGPU_NULLABLE void *userdata) {
    DRV_DEBUG("wgpuBufferMapAsyncImportCallbackNative called");
    DRV_DEBUG("Parameters: status=%d, userdata=%p", status, userdata);

    DRV_DEBUG("Getting the callback context");
    Proc *proc = wgpuBufferMapAsyncCallbackContext.proc;
    WASM_INT_C_TYPE callback_index =
        wgpuBufferMapAsyncCallbackContext.func_index;

    DRV_DEBUG("Getting __indirect_function_table");
    wasm_table_t *indirect_functions_table = proc->indirect_func_table;

    DRV_DEBUG("Looking up function index: %d", callback_index);
    wasm_ref_t *callback_ref =
        wasm_table_get(indirect_functions_table, callback_index);
    DRV_DEBUG("Callback ref: %p", callback_ref);
    wasm_func_t *callback = wasm_ref_as_func(callback_ref);
    DRV_DEBUG("Callback: %p", callback);

    DRV_DEBUG("Creating args/results for wgpuBufferMapAsyncImportCallbackWasm");
    wasm_val_vec_t args;
    wasm_val_vec_new_uninitialized(&args, 2);
    args.data[0].kind = WASM_INT_KIND;
    args.data[0].of.i32 = status;
    args.data[1].kind = WASM_INT_KIND;
    args.data[1].of.i32 = (WASM_INT_C_TYPE)(uintptr_t)userdata;
    args.num_elems = 2;

    // create results (void)
    wasm_val_vec_t results;
    wasm_val_vec_new_uninitialized(&results, 0);

    // call the WASM function
    DRV_DEBUG("Calling wgpuBufferMapAsyncImportCallbackWasm");
    wasm_trap_t *trap = wasm_func_call(callback, &args, &results);
    if (trap != NULL) {
        wasm_message_t msg;
        wasm_trap_message(trap, &msg);
        DRV_DEBUG("Error calling wgpuBufferMapAsyncImportCallbackWasm: %.*s",
                  msg.size, msg.data);
        return;
    }

    DRV_DEBUG("Successfully called wgpuBufferMapAsyncImportCallbackWasm");
}

// WGPU_EXPORT void wgpuBufferMapAsync(WGPUBuffer buffer, WGPUMapModeFlags mode, size_t offset, size_t size, WGPUBufferMapAsyncCallback callback, WGPU_NULLABLE void * userdata) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuBufferMapAsyncImport(void *env, const wasm_val_vec_t *args,
                                      wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuBufferMapAsync, 6);

    WGPUBuffer buffer = (WGPUBuffer)registry_item_get_mapping(
        &registry.buffers, wasm_val_to_native_int(args->data[0]));
    WGPUMapModeFlags mode =
        (WGPUMapModeFlags)wasm_val_to_native_int(args->data[1]);
    size_t offset = (size_t)wasm_val_to_native_int(args->data[2]);
    size_t size = (size_t)wasm_val_to_native_int(args->data[3]);

    // an index into the module’s function table
    WASM_INT_C_TYPE callback_index = wasm_val_to_native_int(args->data[4]);
    WGPUBufferMapAsyncCallback callback = NULL;
    if (callback_index == 0) {
        DRV_DEBUG("wgpuBufferMapAsyncCallbackWasmFuncTableRef is 0, using NULL "
                  "native callback");
    } else {
        DRV_DEBUG("wgpuBufferMapAsyncCallbackWasmFuncTableRef is %d",
                  callback_index);
        callback = wgpuBufferMapAsyncCallbackNative;
        // Store the callback context in a global variable
        wgpuBufferMapAsyncCallbackContext.proc = proc;
        wgpuBufferMapAsyncCallbackContext.func_index = callback_index;
    }
    // Keep this as a WASM pointer since it's not used in the native code
    WASM_INT_C_TYPE userdata_wasm_ptr = wasm_val_to_native_int(args->data[5]);

    // Call the actual function
    DRV_DEBUG("buffer = %p, mode = %d, offset = %zu, size = %zu, callback = "
              "%p, userdata = %p",
              buffer, mode, offset, size, callback,
              (void *)(uintptr_t)userdata_wasm_ptr);
    wgpuBufferMapAsync(buffer, mode, offset, size, callback,
                       (void *)(uintptr_t)userdata_wasm_ptr);

    return NULL;
}

// WGPU_EXPORT void * wgpuBufferGetMappedRange(WGPUBuffer buffer, size_t offset, size_t size) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuBufferGetMappedRangeImport(void *env,
                                            const wasm_val_vec_t *args,
                                            wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuBufferGetMappedRange, 3);

    WGPUBuffer buffer = (WGPUBuffer)registry_item_get_mapping(
        &registry.buffers, wasm_val_to_native_int(args->data[0]));
    size_t offset = (size_t)wasm_val_to_native_int(args->data[1]);
    size_t size = (size_t)wasm_val_to_native_int(args->data[2]);

    void *mapped = wgpuBufferGetMappedRange(buffer, offset, size);

    // // Debug print out memory
    // DRV_DEBUG("mapped = %p, size = %zu", mapped, size);
    // for (int i = 0; i < size; i++) {
    //     DRV_DEBUG("%02x", ((byte_t *)mapped)[i]);
    // }

    wasm_func_t *wasm_malloc = get_exported_function(proc, "malloc");

    // set up arguments
    wasm_val_vec_t wm_args = {};
    wasm_val_vec_new_uninitialized(&wm_args, 1);
    wm_args.size = 1;
    wm_args.num_elems = 1;
    wm_args.data[0].kind = WASM_INT_KIND;
    wm_args.data[0].of.i32 = size;

    // create results (void)
    wasm_val_vec_t wm_results = {};
    wasm_val_vec_new_uninitialized(&wm_results, 1);

    wasm_trap_t *trap = wasm_func_call(wasm_malloc, &wm_args, &wm_results);
    if (trap != NULL) {
        wasm_message_t msg;
        wasm_trap_message(trap, &msg);
        DRV_DEBUG("Error calling wgpuBufferMapAsyncImportCallbackWasm: %.*s",
                  msg.size, msg.data);
        return NULL;
    }

    WASM_INT_C_TYPE wasm_malloc_res =
        wasm_val_to_native_int(wm_results.data[0]);
    DRV_DEBUG("Called wasm_malloc with result %p", wasm_malloc_res);

    if (wasm_malloc_res == 0) {
        DRV_DEBUG("wasm_malloc failed");
        return NULL;
    }

    uintptr_t wasm_buffer_write_native_ptr;
    wasm_get_buffer_pointer_safe(wasm_memory, wasm_malloc_res, size,
                                 &wasm_buffer_write_native_ptr);

    if (wasm_buffer_write_native_ptr) {
        memcpy((void *)wasm_buffer_write_native_ptr, mapped, size);
    }

    results->data[0].kind = WASM_POINTER_KIND;
    results->data[0].of.i32 = wasm_malloc_res;

    return NULL;
}

// WGPU_EXPORT void const * wgpuBufferGetConstMappedRange(WGPUBuffer buffer, size_t offset, size_t size) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuBufferGetConstMappedRangeImport(void *env,
                                                const wasm_val_vec_t *args,
                                                wasm_val_vec_t *results) {
    DRV_DEBUG("wgpuBufferGetConstMappedRangeImport called, resolving to wgpuBufferGetMappedRangeImport");

    return wgpuBufferGetMappedRangeImport(env, args, results);
}

// WGPU_EXPORT WGPUTexture wgpuDeviceCreateTexture(WGPUDevice device, WGPUTextureDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuDeviceCreateTextureImport(void *env, const wasm_val_vec_t *args,
                                          wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuDeviceCreateTexture, 2);

    WGPUDevice device = (WGPUDevice)registry_item_get_mapping(
        &registry.devices, wasm_val_to_native_int(args->data[0]));
    if (!device) {
        DRV_DEBUG("Failed to get device");
        results->data[0].kind = WASM_INT_KIND;
        results->data[0].of.i32 = 0;
        return NULL;
    }

    WGPUTextureDescriptor *descriptor = NULL;
    extract_texture_descriptor(
        &registry, wasm_memory, (byte_t *)wasm_val_to_native_int(args->data[1]), &descriptor);
    if (!descriptor) {
        DRV_DEBUG("Failed to extract texture descriptor");
        results->data[0].kind = WASM_INT_KIND;
        results->data[0].of.i32 = 0;
        return NULL;
    }

    WGPUTexture texture = wgpuDeviceCreateTexture(device, descriptor);
    if (!texture) {
        DRV_DEBUG("Failed to create texture");
        results->data[0].kind = WASM_INT_KIND;
        results->data[0].of.i32 = 0;
        return NULL;
    }

    WASM_INT_C_TYPE texture_mapping_index = registry_item_add_mapping(&registry.textures, (void *)texture);

    results->data[0].kind = WASM_INT_KIND;
    results->data[0].of.i32 = texture_mapping_index;
    return NULL;
}

// WGPU_EXPORT WGPUTextureView wgpuTextureCreateView(WGPUTexture texture, WGPU_NULLABLE WGPUTextureViewDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuTextureCreateViewImport(void *env, const wasm_val_vec_t *args,
                                         wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuTextureCreateView, 2);

    WGPUTexture texture = (WGPUTexture)registry_item_get_mapping(
        &registry.textures, wasm_val_to_native_int(args->data[0]));

    if (!texture) {
        DRV_DEBUG("Failed to get texture");
        results->data[0].kind = WASM_INT_KIND;
        results->data[0].of.i32 = 0;
        return NULL;
    }

    WGPUTextureViewDescriptor *descriptor = NULL;
    extract_texture_view_descriptor(
        &registry, wasm_memory, (byte_t *)wasm_val_to_native_int(args->data[1]), &descriptor);
    if (!descriptor) {
        DRV_DEBUG("Failed to extract texture view descriptor");
        results->data[0].kind = WASM_INT_KIND;
        results->data[0].of.i32 = 0;
        return NULL;
    }

    WGPUTextureView texture_view = wgpuTextureCreateView(texture, descriptor);
    if (!texture_view) {
        DRV_DEBUG("Failed to create texture view");
        results->data[0].kind = WASM_INT_KIND;
        results->data[0].of.i32 = 0;
        return NULL;
    }

    WASM_INT_C_TYPE texture_view_mapping_index = registry_item_add_mapping(&registry.textureViews,
                                                           (void *)texture_view);

    results->data[0].kind = WASM_INT_KIND;
    results->data[0].of.i32 = texture_view_mapping_index;
    return NULL;
}

// WGPU_EXPORT WGPURenderPassEncoder wgpuCommandEncoderBeginRenderPass(WGPUCommandEncoder commandEncoder, WGPURenderPassDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuCommandEncoderBeginRenderPassImport(void *env, const wasm_val_vec_t *args,
                                                     wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuCommandEncoderBeginRenderPass, 2);

    WGPUCommandEncoder commandEncoder = (WGPUCommandEncoder)registry_item_get_mapping(
        &registry.commandEncoders, wasm_val_to_native_int(args->data[0]));
    if (!commandEncoder) {
        DRV_DEBUG("Failed to get command encoder");
        return NULL;
    }

    WGPURenderPassDescriptor *descriptor = NULL;
    extract_render_pass_descriptor(
        &registry, wasm_memory, (byte_t *)wasm_val_to_native_int(args->data[1]), &descriptor);

    WGPURenderPassEncoder encoder = wgpuCommandEncoderBeginRenderPass(commandEncoder, descriptor);

    if (!encoder) {
        DRV_DEBUG("Failed to create render pass encoder");
        return NULL;
    }

    WASM_INT_C_TYPE encoder_mapping_index = registry_item_add_mapping(&registry.renderPassEncoders,
                                                       (void *)encoder);

    results->data[0].kind = WASM_INT_KIND;
    results->data[0].of.i32 = encoder_mapping_index;

    return NULL;
}

// WGPU_EXPORT void wgpuRenderPassEncoderSetPipeline(WGPURenderPassEncoder renderPassEncoder, WGPURenderPipeline pipeline) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuRenderPassEncoderSetPipelineImport(void *env, const wasm_val_vec_t *args,
                                                    wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuRenderPassEncoderSetPipeline, 2);

    WGPURenderPassEncoder renderPassEncoder = (WGPURenderPassEncoder)registry_item_get_mapping(
        &registry.renderPassEncoders, wasm_val_to_native_int(args->data[0]));
    if (!renderPassEncoder) {
        DRV_DEBUG("Failed to get render pass encoder");
        return NULL;
    }

    WGPURenderPipeline pipeline = (WGPURenderPipeline)registry_item_get_mapping(
        &registry.renderPipelines, wasm_val_to_native_int(args->data[1]));
    if (!pipeline) {
        DRV_DEBUG("Failed to get render pipeline");
        return NULL;
    }

    wgpuRenderPassEncoderSetPipeline(renderPassEncoder, pipeline);

    return NULL;
}

// WGPU_EXPORT void wgpuRenderPassEncoderDraw(WGPURenderPassEncoder renderPassEncoder, uint32_t vertexCount, uint32_t instanceCount, uint32_t firstVertex, uint32_t firstInstance) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuRenderPassEncoderDrawImport(void *env, const wasm_val_vec_t *args,
                                             wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuRenderPassEncoderDraw, 5);

    WGPURenderPassEncoder renderPassEncoder = (WGPURenderPassEncoder)registry_item_get_mapping(
        &registry.renderPassEncoders, wasm_val_to_native_int(args->data[0]));
    if (!renderPassEncoder) {
        DRV_DEBUG("Failed to get render pass encoder");
        return NULL;
    }

    // Get other params
    uint32_t vertexCount = wasm_val_to_native_int(args->data[1]);
    uint32_t instanceCount = wasm_val_to_native_int(args->data[2]);
    uint32_t firstVertex = wasm_val_to_native_int(args->data[3]);
    uint32_t firstInstance = wasm_val_to_native_int(args->data[4]);

    wgpuRenderPassEncoderDraw(renderPassEncoder, vertexCount, instanceCount, firstVertex, firstInstance);

    return NULL;
}

// WGPU_EXPORT void wgpuRenderPassEncoderEnd(WGPURenderPassEncoder renderPassEncoder) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuRenderPassEncoderEndImport(void *env, const wasm_val_vec_t *args,
                                            wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuRenderPassEncoderEnd, 1);

    WGPURenderPassEncoder renderPassEncoder = (WGPURenderPassEncoder)registry_item_get_mapping(
        &registry.renderPassEncoders, wasm_val_to_native_int(args->data[0]));
    if (!renderPassEncoder) {
        DRV_DEBUG("Failed to get render pass encoder");
        return NULL;
    }

    wgpuRenderPassEncoderEnd(renderPassEncoder);

    return NULL;
}

wasm_trap_t *wgpuTextureGetHeightImport(void *env, const wasm_val_vec_t *args,
                                        wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuTextureGetHeight, 1);

    WGPUTexture texture = (WGPUTexture)registry_item_get_mapping(
        &registry.textures, wasm_val_to_native_int(args->data[0]));
    if (!texture) {
        DRV_DEBUG("Failed to get texture");
        return NULL;
    }

    uint32_t height = wgpuTextureGetHeight(texture);

    results->size = 1;
    results->data[0].kind = WASM_INT_KIND;
    results->data[0].of.i32 = height;
    return NULL;
}

wasm_trap_t *wgpuTextureGetWidthImport(void *env, const wasm_val_vec_t *args,
                                        wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuTextureGetWidth, 1);

    WGPUTexture texture = (WGPUTexture)registry_item_get_mapping(
        &registry.textures, wasm_val_to_native_int(args->data[0]));
    if (!texture) {
        DRV_DEBUG("Failed to get texture");
        return NULL;
    }

    uint32_t width = wgpuTextureGetWidth(texture);

    results->size = 1;
    results->data[0].kind = WASM_INT_KIND;
    results->data[0].of.i32 = width;
    return NULL;
}

// WGPU_EXPORT void wgpuBufferUnmap(WGPUBuffer buffer) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuBufferUnmapImport(void *env, const wasm_val_vec_t *args,
                                   wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuBufferUnmap, 1);

    WGPUBuffer buffer = (WGPUBuffer)registry_item_get_mapping(
        &registry.buffers, wasm_val_to_native_int(args->data[0]));
    if (!buffer) {
        DRV_DEBUG("Failed to get buffer");
        return NULL;
    }

    wgpuBufferUnmap(buffer);

    return NULL;
}

// WGPU_EXPORT void wgpuQueueRelease(WGPUQueue queue) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuQueueReleaseImport(void *env, const wasm_val_vec_t *args,
                              wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuQueueRelease, 1);

    WGPUQueue queue = (WGPUQueue)registry_item_get_mapping(
        &registry.queues, wasm_val_to_native_int(args->data[0]));
    if (!queue) {
        DRV_DEBUG("Failed to get queue");
        return NULL;
    }

    wgpuQueueRelease(queue);

    return NULL;
}

// WGPU_EXPORT void wgpuCommandEncoderCopyTextureToBuffer(WGPUCommandEncoder commandEncoder, WGPUImageCopyTexture const * source, WGPUImageCopyBuffer const * destination, WGPUExtent3D const * copySize) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuCommandEncoderCopyTextureToBufferImport(void *env, const wasm_val_vec_t *args,
                                                         wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuCommandEncoderCopyTextureToBuffer, 4);

    WGPUCommandEncoder commandEncoder = (WGPUCommandEncoder)registry_item_get_mapping(
        &registry.commandEncoders, wasm_val_to_native_int(args->data[0]));
    if (!commandEncoder) {
        DRV_DEBUG("Failed to get command encoder");
        return NULL;
    }

    WASM_POINTER_OBJECT_C_TYPE command_encoder_id = wasm_val_to_native_int(args->data[0]);
    DRV_DEBUG("Retrieved command encoder id: %u", command_encoder_id);
    WGPUCommandEncoder command_encoder = (WGPUCommandEncoder)registry_item_get_mapping(
        &registry.commandEncoders, command_encoder_id);
    if (!command_encoder) {
        DRV_DEBUG("Failed to get command encoder");
        return NULL;
    }

    WGPUImageCopyTexture *texture = NULL;
    extract_image_copy_texture(
        &registry, wasm_memory, (byte_t *)wasm_val_to_native_int(args->data[1]), &texture);
    if (!texture) {
        DRV_DEBUG("Failed to extract image copy texture");
        return NULL;
    }

    WGPUImageCopyBuffer *buffer = NULL;
    extract_image_copy_buffer(
        &registry, wasm_memory, (byte_t *)wasm_val_to_native_int(args->data[2]), &buffer);
    if (!buffer) {
        DRV_DEBUG("Failed to extract image copy buffer");
        return NULL;
    }

    WGPUExtent3D *extent = NULL;
    extract_extent_3D(
        &registry, wasm_memory, (byte_t *)wasm_val_to_native_int(args->data[3]), &extent);
    if (!extent) {
        DRV_DEBUG("Failed to extract extent");
        return NULL;
    }

    wgpuCommandEncoderCopyTextureToBuffer(command_encoder, texture, buffer, extent);

    return NULL;
}

static const struct {
    const char *name;
    void *func;
} webgpu_functions[] = {
    // WGPUBool wgpuDevicePoll(WGPUDevice device, WGPUBool wait, WGPU_NULLABLE WGPUWrappedSubmissionIndex const * wrappedSubmissionIndex);
    {"wgpuDevicePoll", wgpuDevicePollImport},
    // WGPU_EXPORT WGPUInstance wgpuCreateInstance(WGPU_NULLABLE WGPUInstanceDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
	{"wgpuCreateInstance", wgpuCreateInstanceImport},
    // WGPU_EXPORT void wgpuInstanceRequestAdapter(WGPUInstance instance, WGPU_NULLABLE WGPURequestAdapterOptions const * options, WGPUInstanceRequestAdapterCallback callback, WGPU_NULLABLE void * userdata) WGPU_FUNCTION_ATTRIBUTE;
	{"wgpuInstanceRequestAdapter", wgpuInstanceRequestAdapterImport},
    // WGPU_EXPORT void wgpuAdapterRequestDevice(WGPUAdapter adapter, WGPU_NULLABLE WGPUDeviceDescriptor const * descriptor, WGPUAdapterRequestDeviceCallback callback, WGPU_NULLABLE void * userdata) WGPU_FUNCTION_ATTRIBUTE;
	{"wgpuAdapterRequestDevice", wgpuAdapterRequestDeviceImport},
    // WGPU_EXPORT WGPUQueue wgpuDeviceGetQueue(WGPUDevice device) WGPU_FUNCTION_ATTRIBUTE;
	{"wgpuDeviceGetQueue", wgpuDeviceGetQueueImport},
    // WGPU_EXPORT WGPUShaderModule wgpuDeviceCreateShaderModule(WGPUDevice device, WGPUShaderModuleDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuDeviceCreateShaderModule", wgpuDeviceCreateShaderModuleImport},
    // WGPU_EXPORT WGPUBuffer wgpuDeviceCreateBuffer(WGPUDevice device, WGPUBufferDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuDeviceCreateBuffer", wgpuDeviceCreateBufferImport},
    // WGPU_EXPORT WGPUComputePipeline wgpuDeviceCreateComputePipeline(WGPUDevice device, WGPUComputePipelineDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuDeviceCreateComputePipeline", wgpuDeviceCreateComputePipelineImport},
    // WGPU_EXPORT WGPURenderPipeline wgpuDeviceCreateRenderPipeline(WGPUDevice device, WGPURenderPipelineDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuDeviceCreateRenderPipeline", wgpuDeviceCreateRenderPipelineImport},
    // WGPU_EXPORT WGPUBindGroupLayout wgpuComputePipelineGetBindGroupLayout(WGPUComputePipeline computePipeline, uint32_t groupIndex) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuComputePipelineGetBindGroupLayout", wgpuComputePipelineGetBindGroupLayoutImport},
    // WGPU_EXPORT WGPUBindGroup wgpuDeviceCreateBindGroup(WGPUDevice device, WGPUBindGroupDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuDeviceCreateBindGroup", wgpuDeviceCreateBindGroupImport},
    // WGPU_EXPORT WGPUCommandEncoder wgpuDeviceCreateCommandEncoder(WGPUDevice device, WGPU_NULLABLE WGPUCommandEncoderDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuDeviceCreateCommandEncoder", wgpuDeviceCreateCommandEncoderImport},
    // WGPU_EXPORT WGPUComputePassEncoder wgpuCommandEncoderBeginComputePass(WGPUCommandEncoder commandEncoder, WGPU_NULLABLE WGPUComputePassDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuCommandEncoderBeginComputePass", wgpuCommandEncoderBeginComputePassImport},
    // WGPU_EXPORT void wgpuComputePassEncoderSetPipeline(WGPUComputePassEncoder computePassEncoder, WGPUComputePipeline pipeline) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuComputePassEncoderSetPipeline", wgpuComputePassEncoderSetPipelineImport},
    // WGPU_EXPORT void wgpuComputePassEncoderSetBindGroup(WGPUComputePassEncoder computePassEncoder, uint32_t groupIndex, WGPU_NULLABLE WGPUBindGroup group, size_t dynamicOffsetCount, uint32_t const * dynamicOffsets) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuComputePassEncoderSetBindGroup", wgpuComputePassEncoderSetBindGroupImport},
    // WGPU_EXPORT void wgpuComputePassEncoderDispatchWorkgroups(WGPUComputePassEncoder computePassEncoder, uint32_t workgroupCountX, uint32_t workgroupCountY, uint32_t workgroupCountZ) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuComputePassEncoderDispatchWorkgroups", wgpuComputePassEncoderDispatchWorkgroupsImport},
    // WGPU_EXPORT void wgpuComputePassEncoderEnd(WGPUComputePassEncoder computePassEncoder) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuComputePassEncoderEnd", wgpuComputePassEncoderEndImport},
    // WGPU_EXPORT void wgpuComputePassEncoderRelease(WGPUComputePassEncoder computePassEncoder) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuComputePassEncoderRelease", wgpuComputePassEncoderReleaseImport},
    // WGPU_EXPORT void wgpuRenderPassEncoderRelease(WGPURenderPassEncoder renderPassEncoder) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuRenderPassEncoderRelease", wgpuRenderPassEncoderReleaseImport},
    // WGPU_EXPORT void wgpuCommandEncoderCopyBufferToBuffer(WGPUCommandEncoder commandEncoder, WGPUBuffer source, uint64_t sourceOffset, WGPUBuffer destination, uint64_t destinationOffset, uint64_t size) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuCommandEncoderCopyBufferToBuffer", wgpuCommandEncoderCopyBufferToBufferImport},
    // WGPU_EXPORT WGPUCommandBuffer wgpuCommandEncoderFinish(WGPUCommandEncoder commandEncoder, WGPU_NULLABLE WGPUCommandBufferDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuCommandEncoderFinish", wgpuCommandEncoderFinishImport},
    // WGPU_EXPORT void wgpuQueueWriteBuffer(WGPUQueue queue, WGPUBuffer buffer, uint64_t bufferOffset, void const * data, size_t size) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuQueueWriteBuffer", wgpuQueueWriteBufferImport},
    // WGPU_EXPORT void wgpuQueueSubmit(WGPUQueue queue, size_t commandCount, WGPUCommandBuffer const * commands) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuQueueSubmit", wgpuQueueSubmitImport},
    // WGPU_EXPORT void wgpuBufferMapAsync(WGPUBuffer buffer, WGPUMapModeFlags mode, size_t offset, size_t size, WGPUBufferMapAsyncCallback callback, WGPU_NULLABLE void * userdata) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuBufferMapAsync", wgpuBufferMapAsyncImport},
    // WGPU_EXPORT void * wgpuBufferGetMappedRange(WGPUBuffer buffer, size_t offset, size_t size) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuBufferGetMappedRange", wgpuBufferGetMappedRangeImport},
    // WGPU_EXPORT void const * wgpuBufferGetConstMappedRange(WGPUBuffer buffer, size_t offset, size_t size) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuBufferGetConstMappedRange", wgpuBufferGetConstMappedRangeImport},
    // WGPU_EXPORT WGPUTexture wgpuDeviceCreateTexture(WGPUDevice device, WGPUTextureDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuDeviceCreateTexture", wgpuDeviceCreateTextureImport},
    // WGPU_EXPORT WGPUTextureView wgpuTextureCreateView(WGPUTexture texture, WGPU_NULLABLE WGPUTextureViewDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuTextureCreateView", wgpuTextureCreateViewImport},
    // WGPU_EXPORT WGPURenderPassEncoder wgpuCommandEncoderBeginRenderPass(WGPUCommandEncoder commandEncoder, WGPURenderPassDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuCommandEncoderBeginRenderPass", wgpuCommandEncoderBeginRenderPassImport},
    // WGPU_EXPORT void wgpuRenderPassEncoderSetPipeline(WGPURenderPassEncoder renderPassEncoder, WGPURenderPipeline pipeline) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuRenderPassEncoderSetPipeline", wgpuRenderPassEncoderSetPipelineImport},
    // WGPU_EXPORT void wgpuRenderPassEncoderDraw(WGPURenderPassEncoder renderPassEncoder, uint32_t vertexCount, uint32_t instanceCount, uint32_t firstVertex, uint32_t firstInstance) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuRenderPassEncoderDraw", wgpuRenderPassEncoderDrawImport},
    // WGPU_EXPORT void wgpuRenderPassEncoderEnd(WGPURenderPassEncoder renderPassEncoder) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuRenderPassEncoderEnd", wgpuRenderPassEncoderEndImport},
    // WGPU_EXPORT uint32_t wgpuTextureGetHeight(WGPUTexture texture) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuTextureGetHeight", wgpuTextureGetHeightImport},
    // WGPU_EXPORT uint32_t wgpuTextureGetWidth(WGPUTexture texture) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuTextureGetWidth", wgpuTextureGetWidthImport},
    // WGPU_EXPORT void wgpuBufferUnmap(WGPUBuffer buffer) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuBufferUnmap", wgpuBufferUnmapImport},
    // WGPU_EXPORT void wgpuQueueRelease(WGPUQueue queue) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuQueueRelease", wgpuQueueReleaseImport},
    // WGPU_EXPORT void wgpuCommandEncoderCopyTextureToBuffer(WGPUCommandEncoder commandEncoder, WGPUImageCopyTexture const * source, WGPUImageCopyBuffer const * destination, WGPUExtent3D const * copySize) WGPU_FUNCTION_ATTRIBUTE;
    {"wgpuCommandEncoderCopyTextureToBuffer", wgpuCommandEncoderCopyTextureToBufferImport},
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
        if (wasm_copy_struct_safe(wasm_memory, iovs + offset, &iovec, sizeof(WASMIOVec), 4)) {
            DRV_DEBUG("fd_write_import: failed to copy iov");
            return NULL;
        }
        // DRV_DEBUG("fd_write_import: iovec1.buf_ptr=%p, iovec1.buf_len=%d", iovec.buf_ptr, iovec.buf_len);
        
        char * str = wasm_copy_string_len_safe(wasm_memory, iovec.buf_ptr, iovec.buf_len);
        DRV_DEBUG("fd_write[%d]: %s", fd, str);

        written += iovec.buf_len;
    }

    // TODO, safely :)
    // DRV_DEBUG("fd_write_import: written=%d", written);
    memcpy(wasm_memory_data(wasm_memory) + pnum, (void*)&written, 4);

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

int set_callback_webgpu(wasm_byte_t *module_name, wasm_byte_t *name,
                        wasm_func_callback_with_env_t *callback_out) {
    // wasi fns
    if (strcmp(module_name, "wasi_snapshot_preview1") == 0) {
        if (strcmp(name, "environ_sizes_get") == 0) {
            *callback_out = environ_sizes_get_import;
            return 1;
        } else if (strcmp(name, "environ_get") == 0) {
            *callback_out = environ_get_import;
            return 1;
        } else if (strcmp(name, "fd_write") == 0) {
            *callback_out = fd_write_import;
            return 1;
        }
    }

    // emscripten fns
    if (strcmp(module_name, "env") == 0) {
        if (strcmp(name, "_emscripten_memcpy_js") == 0) {
            *callback_out = emscripten_memcpy_js_import;
            return 1;
        }
    }

    // webgpu fns
    if (strcmp(module_name, "env") == 0) {
        for (size_t i = 0;
             i < sizeof(webgpu_functions) / sizeof(webgpu_functions[0]); i++) {
            if (strcmp(webgpu_functions[i].name, name) == 0) {
                void *const func = webgpu_functions[i].func;
                *callback_out = func;
                return 1;
            }
        }
    }

    return 0;
}
