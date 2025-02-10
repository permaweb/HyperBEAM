#include "include/hb_wasm_webgpu.h"
#include "include/hb_helpers.h"
#include "include/hb_logging.h"
#include <stdint.h>
#include <string.h>
#include <wasm_c_api.h>
#include <webgpu.h>
#include <wgpu.h>

#ifdef WASM_64
#define WASM_INT_TYPE WASM_I64
#define WASM_INT_NATIVE_TYPE int64_t
#define WASM_POINTER_TYPE WASM_I64
#define WASM_POINTER_NAME i64
#else
#define WASM_INT_TYPE WASM_I32
#define WASM_INT_NATIVE_TYPE int32_t
#define WASM_POINTER_TYPE WASM_I32
#define WASM_POINTER_NAME i32
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
#define WASM_MEMORY_ERROR(msg)                                                 \
    do {                                                                       \
        fprintf(stderr, "WASM Memory Error: %s\n", msg);                       \
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
        WASM_MEMORY_ERROR("Unaligned struct pointer in WASM memory");
        return 1;
    }

    // Copy bytes from WASM memory into the native struct.
    memcpy(out_struct, mem_base + offset, struct_size);
    return 0;
}

/**
 * Safely copy a string from WASM memory to native memory.
 * Returns NULL if the string pointer is invalid or memory allocation fails.
 */
char *wasm_copy_string_safe(wasm_memory_t *memory, uint32_t str_ptr) {
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

#define GET_WASM_SYS_INT(data) (data).of.WASM_POINTER_NAME

static inline WASM_INT_NATIVE_TYPE wasm_val_to_native_int(wasm_val_t wasm_val) {
    if (wasm_val.kind != WASM_INT_TYPE) {
        DRV_DEBUG("wasm_val_to_native_int: expected %s, got kind: %d",
                  WASM_INT_TYPE, wasm_val.kind);
    } else {
        DRV_DEBUG("wasm_val_to_native_int: got kind: %d", WASM_INT_TYPE);
    }
    return GET_WASM_SYS_INT(wasm_val);
}

static inline uintptr_t wasm_val_to_native_ptr(byte_t *wasm_base_ptr,
                                               wasm_val_t wasm_val) {
    if (wasm_val.kind != WASM_POINTER_TYPE) {
        DRV_DEBUG("wasm_val_to_native_ptr: expected %s, got kind: %d",
                  WASM_POINTER_TYPE, wasm_val.kind);
        return 0;
    }
    uintptr_t wasm_ptr = GET_WASM_SYS_INT(wasm_val);
    if (wasm_ptr == 0) {
        DRV_DEBUG("wasm_val_to_native_ptr: wasm pointer is NULL");
        return 0;
    }
    uintptr_t native_ptr = (uintptr_t)(wasm_base_ptr + wasm_ptr);
    DRV_DEBUG("Converted wasm pointer (%p) to native pointer (%p)",
              (void *)wasm_ptr, (void *)native_ptr);
    return native_ptr;
}
#define WASM_TO_NATIVE_STRUCT(type, wasm_val)                                  \
    ((type *)wasm_val_to_native_ptr(wasm_base_ptr, wasm_val));
#define WASM_EXTERN_TO_NATIVE_STRUCT(type, wasm_val)                           \
    ((type *)GET_WASM_SYS_INT(wasm_val));

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

#define MAX_MAPPINGS 1024

typedef struct {
    size_t index;
    uintptr_t native_ptr;
} ExternRefMapping;

typedef struct {
    size_t count;
    ExternRefMapping *mappings[MAX_MAPPINGS];
} ExternRefRegistry;

size_t add_mapping(ExternRefRegistry *registry, uintptr_t native_ptr) {
    if (!registry || registry->count >= MAX_MAPPINGS) {
        DRV_DEBUG("Error: Registry is full or NULL");
        return 0;
    }

    ExternRefMapping *mapping = driver_alloc(sizeof(ExternRefMapping));
    if (!mapping) {
        DRV_DEBUG("Error: Failed to allocate mapping");
        return 0;
    }

    registry->count++;

    mapping->index = registry->count;
    mapping->native_ptr = native_ptr;

    registry->mappings[mapping->index] = mapping;

    DRV_DEBUG("add_mapping: index=%zu, native_ptr=%p", mapping->index,
              (void *)native_ptr);

    return mapping->index;
}

uintptr_t get_mapping(ExternRefRegistry *registry, size_t index) {
    if (!registry) {
        DRV_DEBUG("Error: NULL registry");
        return 0;
    }
    if (index == 0 || index > registry->count) {
        DRV_DEBUG("Error: Invalid index %zu", index);
        return 0;
    }
    if (index > MAX_MAPPINGS) {
        DRV_DEBUG("Error: Index %zu exceeds MAX_MAPPINGS", index);
        return 0;
    }

    ExternRefMapping *mapping = registry->mappings[index];
    if (!mapping) {
        DRV_DEBUG("Error: Mapping not found");
        return 0;
    }

    return mapping->native_ptr;
}

typedef struct {
    Proc *proc;
    WASM_INT_NATIVE_TYPE func_index;
} CallbackContext;

ExternRefRegistry DEVICES = {.count = 0, .mappings = {}};

typedef struct {
    WASM_INT_NATIVE_TYPE queue;
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
        (WGPUQueue)wasm_wrapped_submission_index.queue;
    wrapped_submission_index->submissionIndex =
        wasm_wrapped_submission_index.submissionIndex;

    return wrapped_submission_index;
}

// WGPUBool wgpuDevicePoll(WGPUDevice device, WGPUBool wait, WGPU_NULLABLE
// WGPUWrappedSubmissionIndex const * wrappedSubmissionIndex);
wasm_trap_t *wgpuDevicePollImport(void *env, const wasm_val_vec_t *args,
                                  wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuDevicePoll, 3);

    // Get the device from registry
    WGPUDevice device = (WGPUDevice)get_mapping(
        &DEVICES, wasm_val_to_native_int(args->data[0]));

    // Get wait
    WGPUBool wait = args->data[1].of.i32 != 0;

    // Get wrappedSubmissionIndex
    WGPUWrappedSubmissionIndex *wrappedSubmissionIndex =
        extract_wrapped_submission_index(wasm_memory, args->data[2].of.i32);

    // Print arguments
    DRV_DEBUG("device = %p, wait = %d, wrappedSubmissionIndex = %p", device,
              wait, wrappedSubmissionIndex);

    // Call the native function
    WGPUBool result = wgpuDevicePoll(device, wait, wrappedSubmissionIndex);

    results->data[0].kind = WASM_INT_TYPE;
    results->data[0].of.i32 = result;

    return NULL;
}

ExternRefRegistry INSTANCES = {.count = 0, .mappings = {}};

// WGPU_EXPORT WGPUInstance wgpuCreateInstance(WGPU_NULLABLE
// WGPUInstanceDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuCreateInstanceImport(void *env, const wasm_val_vec_t *args,
                                      wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuCreateInstanceImport, 1);

    WGPUInstanceDescriptor *descriptor =
        WASM_TO_NATIVE_STRUCT(WGPUInstanceDescriptor, args->data[0]);

    DRV_DEBUG("Parameters: descriptor=%p", (uintptr_t)descriptor);
    WGPUInstance instance = wgpuCreateInstance(descriptor);
    DRV_DEBUG("Result: WGPUInstance: %p", (uintptr_t)instance);

    size_t instance_index = add_mapping(&INSTANCES, (uintptr_t)instance);

    DRV_DEBUG("Setting result to WGPUInstance WASM_EXTERNREF");
    results->size = 1;
    results->data[0].kind = WASM_INT_TYPE;
    results->data[0].of.i32 = instance_index;

    return NULL;
}

ExternRefRegistry ADAPTERS = {.count = 0, .mappings = {}};

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
    size_t adapter_index = add_mapping(&ADAPTERS, (uintptr_t)adapter);

    DRV_DEBUG("Getting the callback context");
    Proc *proc = wgpuInstanceRequestAdapterCallbackContext.proc;
    WASM_INT_NATIVE_TYPE callback_index =
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
    args.data[0].kind = WASM_INT_TYPE;
    args.data[0].of.i32 = status;
    args.data[1].kind = WASM_INT_TYPE;
    args.data[1].of.i32 = adapter_index;
    args.data[2].kind = WASM_INT_TYPE;
    // TODO: Allocate memory for the message, copy data, and set the ref
    args.data[2].of.i32 = 0;
    args.data[3].kind = WASM_INT_TYPE;
    args.data[3].of.i32 = (WASM_INT_NATIVE_TYPE)userdata;
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

// WGPU_EXPORT void wgpuInstanceRequestAdapter(WGPUInstance instance,
// WGPU_NULLABLE WGPURequestAdapterOptions const * options,
// WGPUInstanceRequestAdapterCallback callback, WGPU_NULLABLE void * userdata)
// WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuInstanceRequestAdapterImport(void *env,
                                              const wasm_val_vec_t *args,
                                              wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuInstanceRequestAdapterImport, 4);

    size_t instance_mapping_index = wasm_val_to_native_int(args->data[0]);
    DRV_DEBUG("instance_mapping_index = %zu", instance_mapping_index);

    if (INSTANCES.count == 0 && instance_mapping_index == 1) {
        // This might be emscripten, which has an implicit global instance at
        // index 1
        add_mapping(&INSTANCES, (uintptr_t)wgpuCreateInstance(NULL));
    }
    DRV_DEBUG("Getting instance id: %zu", instance_mapping_index);
    WGPUInstance instance =
        (WGPUInstance)get_mapping(&INSTANCES, instance_mapping_index);

    WGPURequestAdapterOptions *options =
        WASM_TO_NATIVE_STRUCT(WGPURequestAdapterOptions, args->data[1]);

    // an index into the module’s function table
    WASM_INT_NATIVE_TYPE callback_index = wasm_val_to_native_int(args->data[2]);
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
    WASM_INT_NATIVE_TYPE userdata_wasm_ptr =
        wasm_val_to_native_int(args->data[3]);

    // Call the actual function
    DRV_DEBUG("Parameters: instance=%p, options=%p, callback=%p, userdata=%p",
              (void *)instance, (void *)options, (void *)callback,
              (void *)userdata_wasm_ptr);
    wgpuInstanceRequestAdapter(instance, options, callback,
                               (void *)userdata_wasm_ptr);

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
    size_t device_index = add_mapping(&DEVICES, (uintptr_t)device);
    DRV_DEBUG("Device index: %d", device_index);

    DRV_DEBUG("Getting the callback context");
    Proc *proc = wgpuInstanceRequestDeviceCallbackContext.proc;
    WASM_INT_NATIVE_TYPE callback_index =
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
    args.data[0].kind = WASM_INT_TYPE;
    args.data[0].of.i32 = status;
    args.data[1].kind = WASM_INT_TYPE;
    args.data[1].of.i32 = device_index;
    args.data[2].kind = WASM_INT_TYPE;
    // TODO: Allocate memory for the message, copy data, and set the ref
    args.data[2].of.i32 = 0;
    args.data[3].kind = WASM_INT_TYPE;
    args.data[3].of.i32 = (WASM_INT_NATIVE_TYPE)userdata;
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

// WGPU_EXPORT void wgpuAdapterRequestDevice(WGPUAdapter adapter, WGPU_NULLABLE
// WGPUDeviceDescriptor const * descriptor, WGPUAdapterRequestDeviceCallback
// callback, WGPU_NULLABLE void * userdata) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuAdapterRequestDeviceImport(void *env,
                                            const wasm_val_vec_t *args,
                                            wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuAdapterRequestDeviceImport, 4);

    size_t adapter_mapping_index = wasm_val_to_native_int(args->data[0]);
    DRV_DEBUG("adapter_mapping_index = %zu", adapter_mapping_index);
    WGPUAdapter *adapter =
        (WGPUAdapter *)get_mapping(&ADAPTERS, adapter_mapping_index);
    DRV_DEBUG("adapter = %p", adapter);

    WGPUDeviceDescriptor *descriptor =
        WASM_TO_NATIVE_STRUCT(WGPUDeviceDescriptor, args->data[1]);
    DRV_DEBUG("descriptor = %p", descriptor);

    // an index into the module’s function table
    WASM_INT_NATIVE_TYPE callback_index = wasm_val_to_native_int(args->data[2]);
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
    WASM_INT_NATIVE_TYPE userdata_wasm_ptr =
        wasm_val_to_native_int(args->data[3]);

    // Call the actual function
    DRV_DEBUG("Parameters: adapter=%p, descriptor=%p, callback=%p, userdata=%p",
              (void *)adapter, (void *)descriptor, (void *)callback,
              (void *)userdata_wasm_ptr);
    wgpuAdapterRequestDevice(adapter, descriptor, callback,
                             (void *)userdata_wasm_ptr);

    return NULL;
}

ExternRefRegistry QUEUES = {.count = 0, .mappings = {}};

// WGPU_EXPORT WGPUQueue wgpuDeviceGetQueue(WGPUDevice device)
// WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuDeviceGetQueueImport(void *env, const wasm_val_vec_t *args,
                                      wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuDeviceGetQueue, 1);

    size_t device_mapping_index = wasm_val_to_native_int(args->data[0]);
    WGPUDevice device = (WGPUDevice)get_mapping(&DEVICES, device_mapping_index);

    // Call the actual function
    DRV_DEBUG("Parameters: device=%p", (void *)device);
    WGPUQueue queue = wgpuDeviceGetQueue(device);

    size_t queue_mapping_index = add_mapping(&QUEUES, (uintptr_t)queue);

    // Return the queue handle
    results->size = 1;
    results->data[0].kind = WASM_INT_TYPE;
    results->data[0].of.i32 = (uint64_t)queue_mapping_index;

    return NULL;
}

ExternRefRegistry SHADER_MODULES = {.count = 0, .mappings = {}};

ExternRefRegistry CHAIN_STRUCTS = {.count = 0, .mappings = {}};

// WASM-friendly struct definitions to match memory layout
typedef struct {
    uint32_t next;  // 4 bytes for WASM pointer
    uint32_t sType; // 4 bytes for enum
} WASMChainedStruct;

typedef struct {
    WASMChainedStruct chain;
    uint32_t code; // 4 bytes for WASM pointer
} WASMShaderModuleWGSLDescriptor;

typedef struct {
    uint32_t nextInChain; // 4 bytes for WASM pointer
    uint32_t label;       // 4 bytes for WASM pointer
    // ... any other fields
} WASMShaderModuleDescriptor;

/**
 * Safely extract a shader module descriptor from WASM memory.
 */
WGPUShaderModuleDescriptor *
extract_shader_module_descriptor(wasm_memory_t *memory,
                                 uint32_t descriptor_ptr) {
    DRV_DEBUG("Extracting shader module descriptor from offset: 0x%x",
              descriptor_ptr);
    if (descriptor_ptr == 0) {
        DRV_DEBUG("Null descriptor pointer");
        return NULL;
    }

    // First copy the base descriptor structure using our WASM-friendly struct
    WASMShaderModuleDescriptor wasm_descriptor = {0};
    if (wasm_copy_struct_safe(memory, descriptor_ptr, &wasm_descriptor,
                              sizeof(WASMShaderModuleDescriptor), 4)) {
        DRV_DEBUG("Failed to copy shader module descriptor");
        return NULL;
    }

    DRV_DEBUG("WASM descriptor - chain: 0x%x, label: 0x%x",
              wasm_descriptor.nextInChain, wasm_descriptor.label);

    // Allocate the result structure
    WGPUShaderModuleDescriptor *result =
        malloc(sizeof(WGPUShaderModuleDescriptor));
    if (!result) {
        DRV_DEBUG("Failed to allocate shader module descriptor");
        return NULL;
    }
    memset(result, 0, sizeof(WGPUShaderModuleDescriptor));

    // Copy the label if present
    if (wasm_descriptor.label != 0) {
        result->label = wasm_copy_string_safe(memory, wasm_descriptor.label);
        DRV_DEBUG("Copied label: %s", result->label ? result->label : "(null)");
    }

    // Handle the chain if present
    if (wasm_descriptor.nextInChain != 0) {
        DRV_DEBUG("Chain pointer from WASM: 0x%x", wasm_descriptor.nextInChain);

        // First copy just the chain header to check the type
        WASMChainedStruct wasm_chain = {0};
        if (wasm_copy_struct_safe(memory, wasm_descriptor.nextInChain,
                                  &wasm_chain, sizeof(WASMChainedStruct), 4)) {
            DRV_DEBUG("Failed to copy chain header");
            goto error;
        }

        DRV_DEBUG("Chain header - next: 0x%x, sType: %u", wasm_chain.next,
                  wasm_chain.sType);

        switch (wasm_chain.sType) {
        case WGPUSType_ShaderModuleWGSLDescriptor: {
            WASMShaderModuleWGSLDescriptor wasm_wgsl = {0};
            if (wasm_copy_struct_safe(
                    memory, wasm_descriptor.nextInChain, &wasm_wgsl,
                    sizeof(WASMShaderModuleWGSLDescriptor), 4)) {
                DRV_DEBUG("Failed to copy WGSL descriptor");
                goto error;
            }

            DRV_DEBUG("WGSL descriptor - code: 0x%x", wasm_wgsl.code);

            WGPUShaderModuleWGSLDescriptor *native_wgsl =
                malloc(sizeof(WGPUShaderModuleWGSLDescriptor));
            if (!native_wgsl) {
                DRV_DEBUG("Failed to allocate WGSL descriptor");
                goto error;
            }
            memset(native_wgsl, 0, sizeof(WGPUShaderModuleWGSLDescriptor));

            // Set up the chain
            native_wgsl->chain.next = NULL;
            native_wgsl->chain.sType = WGPUSType_ShaderModuleWGSLDescriptor;

            // Copy the WGSL code if present
            if (wasm_wgsl.code != 0) {
                native_wgsl->code =
                    wasm_copy_string_safe(memory, wasm_wgsl.code);
                if (!native_wgsl->code) {
                    DRV_DEBUG("Failed to copy WGSL code");
                    free(native_wgsl);
                    goto error;
                }
                DRV_DEBUG("Copied WGSL code, length: %zu",
                          strlen(native_wgsl->code));
            }

            result->nextInChain = (const WGPUChainedStruct *)native_wgsl;
            break;
        }
        default:
            DRV_DEBUG("Unhandled sType: %u", wasm_chain.sType);
            break;
        }
    }

    return result;

error:
    if (result->label)
        free((void *)result->label);
    if (result->nextInChain) {
        if (((WGPUShaderModuleWGSLDescriptor *)result->nextInChain)->code) {
            free((void *)((WGPUShaderModuleWGSLDescriptor *)result->nextInChain)
                     ->code);
        }
        free((void *)result->nextInChain);
    }
    free(result);
    return NULL;
}

// WGPU_EXPORT WGPUShaderModule wgpuDeviceCreateShaderModule(WGPUDevice device,
// WGPUShaderModuleDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuDeviceCreateShaderModuleImport(void *env,
                                                const wasm_val_vec_t *args,
                                                wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuDeviceCreateShaderModule, 2);

    // Get device parameter from mapping
    size_t device_mapping_index = wasm_val_to_native_int(args->data[0]);
    DRV_DEBUG("Device mapping index: %zu", device_mapping_index);
    WGPUDevice device = (WGPUDevice)get_mapping(&DEVICES, device_mapping_index);
    if (!device) {
        DRV_DEBUG("Invalid device handle");
        results->size = 1;
        results->data[0].kind = WASM_INT_TYPE;
        results->data[0].of.i32 = 0;
        return NULL;
    }

    // Get shader module descriptor from WASM memory
    uint32_t descriptor_offset =
        (uint32_t)wasm_val_to_native_int(args->data[1]);
    DRV_DEBUG("Descriptor offset: 0x%x", descriptor_offset);
    WGPUShaderModuleDescriptor *descriptor =
        extract_shader_module_descriptor(wasm_memory, descriptor_offset);
    if (!descriptor) {
        DRV_DEBUG("Failed to extract shader module descriptor");
        results->size = 1;
        results->data[0].kind = WASM_INT_TYPE;
        results->data[0].of.i32 = 0;
        return NULL;
    }

    // Call the actual function
    DRV_DEBUG("Creating shader module with device=%p, descriptor=%p",
              (void *)device, (void *)descriptor);
    wgpuSetLogLevel(WGPULogLevel_Trace);
    wgpuSetLogCallback(webgpu_log_callback, NULL);
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
    free(descriptor);

    // Store the shader module in our registry
    size_t shader_module_mapping_index =
        add_mapping(&SHADER_MODULES, (uintptr_t)shader_module);
    DRV_DEBUG("Stored shader module at index: %zu",
              shader_module_mapping_index);

    // Return the shader module handle
    results->size = 1;
    results->data[0].kind = WASM_INT_TYPE;
    results->data[0].of.WASM_POINTER_NAME =
        (uint64_t)shader_module_mapping_index;

    return NULL;
}

ExternRefRegistry BUFFERS = {.count = 0, .mappings = {}};

// WASM-friendly buffer descriptor
typedef struct {
    uint32_t nextInChain;      // 4 bytes for WASM pointer
    uint32_t label;            // 4 bytes for WASM pointer
    uint32_t usage;            // 4 bytes for flags
    uint32_t size_high;        // Upper 32 bits of size (comes first in memory)
    uint32_t size_low;         // Lower 32 bits of size
    uint32_t mappedAtCreation; // 4 bytes for bool
} WASMBufferDescriptor;

WGPUBufferDescriptor *extract_buffer_descriptor(wasm_memory_t *memory,
                                                uint32_t descriptor_ptr) {
    DRV_DEBUG("Extracting buffer descriptor from offset: 0x%x", descriptor_ptr);
    if (descriptor_ptr == 0) {
        DRV_DEBUG("Null descriptor pointer");
        return NULL;
    }

    // Copy the WASM descriptor
    WASMBufferDescriptor wasm_descriptor = {0};
    if (wasm_copy_struct_safe(memory, descriptor_ptr, &wasm_descriptor,
                              sizeof(WASMBufferDescriptor), 4)) {
        DRV_DEBUG("Failed to copy buffer descriptor");
        return NULL;
    }

    DRV_DEBUG("WASM buffer descriptor - chain: 0x%x, label: 0x%x, usage: 0x%x, "
              "size: 0x%x%08x, mapped: %u",
              wasm_descriptor.nextInChain, wasm_descriptor.label,
              wasm_descriptor.usage, wasm_descriptor.size_high,
              wasm_descriptor.size_low, wasm_descriptor.mappedAtCreation);

    // Allocate and initialize the native descriptor
    WGPUBufferDescriptor *result = malloc(sizeof(WGPUBufferDescriptor));
    if (!result) {
        DRV_DEBUG("Failed to allocate buffer descriptor");
        return NULL;
    }
    memset(result, 0, sizeof(WGPUBufferDescriptor));

    // Copy the label if present
    if (wasm_descriptor.label != 0) {
        result->label = wasm_copy_string_safe(memory, wasm_descriptor.label);
        DRV_DEBUG("Copied label: %s", result->label ? result->label : "(null)");
    }

    // Handle the chain if present (similar to shader module)
    if (wasm_descriptor.nextInChain != 0) {
        DRV_DEBUG("Chain handling not implemented for buffer descriptor");
        // TODO: Implement chain handling if needed
    }

    // Copy the simple fields
    result->usage = wasm_descriptor.usage;
    // In WASM memory, the size is stored as two 32-bit values
    result->size =
        (uint64_t)wasm_descriptor.size_high << 32 | wasm_descriptor.size_low;
    result->mappedAtCreation = wasm_descriptor.mappedAtCreation;

    return result;
}

// WGPU_EXPORT WGPUBuffer wgpuDeviceCreateBuffer(WGPUDevice device,
// WGPUBufferDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuDeviceCreateBufferImport(void *env, const wasm_val_vec_t *args,
                                          wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuDeviceCreateBuffer, 2);

    // Get device parameter from mapping
    size_t device_mapping_index = wasm_val_to_native_int(args->data[0]);
    DRV_DEBUG("Device mapping index: %zu", device_mapping_index);
    WGPUDevice device = (WGPUDevice)get_mapping(&DEVICES, device_mapping_index);
    if (!device) {
        DRV_DEBUG("Invalid device handle");
        results->size = 1;
        results->data[0].kind = WASM_INT_TYPE;
        results->data[0].of.WASM_POINTER_NAME = 0;
        return NULL;
    }

    // Get buffer descriptor from WASM memory
    uint32_t descriptor_offset =
        (uint32_t)wasm_val_to_native_int(args->data[1]);
    DRV_DEBUG("Descriptor offset: 0x%x", descriptor_offset);
    WGPUBufferDescriptor *descriptor =
        extract_buffer_descriptor(wasm_memory, descriptor_offset);
    if (!descriptor) {
        DRV_DEBUG("Failed to extract buffer descriptor");
        results->size = 1;
        results->data[0].kind = WASM_INT_TYPE;
        results->data[0].of.WASM_POINTER_NAME = 0;
        return NULL;
    }

    // Call the actual function
    DRV_DEBUG("Creating buffer with device=%p, descriptor=%p", (void *)device,
              (void *)descriptor);
    WGPUBuffer buffer = wgpuDeviceCreateBuffer(device, descriptor);
    DRV_DEBUG("Created buffer: %p", (void *)buffer);

    // Clean up the descriptor
    if (descriptor->label)
        free((void *)descriptor->label);
    if (descriptor->nextInChain) {
        // TODO: Clean up chain if implemented
        free((void *)descriptor->nextInChain);
    }
    free(descriptor);

    // Store the buffer in our registry
    size_t buffer_mapping_index = add_mapping(&BUFFERS, (uintptr_t)buffer);
    DRV_DEBUG("Stored buffer at index: %zu", buffer_mapping_index);

    // Return the buffer handle
    results->size = 1;
    results->data[0].kind = WASM_INT_TYPE;
    results->data[0].of.WASM_POINTER_NAME = (uint64_t)buffer_mapping_index;

    return NULL;
}

ExternRefRegistry PIPELINE_LAYOUTS = {.count = 0, .mappings = {}};

ExternRefRegistry COMPUTE_PIPELINES = {.count = 0, .mappings = {}};

// WASM-friendly compute pipeline descriptor structures
typedef struct {
    uint32_t nextInChain; // 4 bytes for WASM pointer
    uint32_t module;      // 4 bytes for shader module handle
    uint32_t entryPoint;  // 4 bytes for entry point string pointer
} WASMProgrammableStageDescriptor;

typedef struct {
    uint32_t nextInChain; // 4 bytes for WASM pointer
    uint32_t label;       // 4 bytes for WASM pointer
    uint32_t layout;      // 4 bytes for pipeline layout handle
    WASMProgrammableStageDescriptor compute; // Compute stage descriptor
} WASMComputePipelineDescriptor;

WGPUComputePipelineDescriptor *
extract_compute_pipeline_descriptor(wasm_memory_t *memory,
                                    uint32_t descriptor_ptr) {
    DRV_DEBUG("Extracting compute pipeline descriptor from offset: 0x%x",
              descriptor_ptr);

    if (descriptor_ptr == 0) {
        DRV_DEBUG("Null descriptor pointer");
        return NULL;
    }

    // Copy the WASM descriptor
    WASMComputePipelineDescriptor wasm_descriptor = {0};
    if (wasm_copy_struct_safe(memory, descriptor_ptr, &wasm_descriptor,
                              sizeof(WASMComputePipelineDescriptor), 4)) {
        DRV_DEBUG("Failed to copy compute pipeline descriptor");
        return NULL;
    }

    DRV_DEBUG("WASM compute pipeline descriptor - chain: 0x%x, label: 0x%x, "
              "layout: 0x%x",
              wasm_descriptor.nextInChain, wasm_descriptor.label,
              wasm_descriptor.layout);

    // Allocate and initialize the native descriptor
    WGPUComputePipelineDescriptor *result =
        malloc(sizeof(WGPUComputePipelineDescriptor));
    if (!result) {
        DRV_DEBUG("Failed to allocate compute pipeline descriptor");
        return NULL;
    }
    memset(result, 0, sizeof(WGPUComputePipelineDescriptor));

    // Copy the label if present
    if (wasm_descriptor.label != 0) {
        result->label = wasm_copy_string_safe(memory, wasm_descriptor.label);
        DRV_DEBUG("Copied label: %s", result->label ? result->label : "(null)");
    }

    // Handle the chain if present
    if (wasm_descriptor.nextInChain != 0) {
        DRV_DEBUG(
            "Chain handling not implemented for compute pipeline descriptor");
        // TODO: Implement chain handling if needed
    }

    if (wasm_descriptor.layout) {
        // Get the pipeline layout from registry
        result->layout = (WGPUPipelineLayout)get_mapping(
            &PIPELINE_LAYOUTS, wasm_descriptor.layout);
        DRV_DEBUG("Retrieved pipeline layout: %p", (void *)result->layout);
    } else {
        DRV_DEBUG("No pipeline layout specified");
    }

    // Set up compute stage directly in the result struct
    result->compute.nextInChain = NULL;
    WGPUShaderModule shader_module = (WGPUShaderModule)get_mapping(
        &SHADER_MODULES, wasm_descriptor.compute.module);
    DRV_DEBUG(
        "Retrieved shader module from registry - handle: %p, wasm handle: 0x%x",
        (void *)shader_module, wasm_descriptor.compute.module);
    result->compute.module = shader_module;
    DRV_DEBUG("Stored shader module in descriptor: %p",
              (void *)result->compute.module);

    // Copy entry point
    if (wasm_descriptor.compute.entryPoint != 0) {
        result->compute.entryPoint =
            wasm_copy_string_safe(memory, wasm_descriptor.compute.entryPoint);
        DRV_DEBUG("Copied entry point: %s", result->compute.entryPoint
                                                ? result->compute.entryPoint
                                                : "(null)");
    }

    return result;
}

// WGPU_EXPORT WGPUComputePipeline wgpuDeviceCreateComputePipeline(WGPUDevice
// device, WGPUComputePipelineDescriptor const * descriptor)
// WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuDeviceCreateComputePipelineImport(void *env,
                                                   const wasm_val_vec_t *args,
                                                   wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuDeviceCreateComputePipeline, 2);

    // Get device from registry
    WGPUDevice device = (WGPUDevice)get_mapping(
        &DEVICES, wasm_val_to_native_int(args->data[0]));
    DRV_DEBUG("Retrieved device: %p", (void *)device);

    // Extract the descriptor
    WGPUComputePipelineDescriptor *descriptor =
        extract_compute_pipeline_descriptor(
            wasm_memory, wasm_val_to_native_int(args->data[1]));
    if (!descriptor) {
        DRV_DEBUG("Failed to extract compute pipeline descriptor");
        results->data[0].kind = WASM_INT_TYPE;
        results->data[0].of.WASM_POINTER_NAME = 0;
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

    // Clean up the descriptor
    if (descriptor->label)
        free((void *)descriptor->label);
    if (descriptor->compute.entryPoint)
        free((void *)descriptor->compute.entryPoint);
    free(descriptor);

    // Add pipeline to registry and return its index
    size_t pipeline_index =
        add_mapping(&COMPUTE_PIPELINES, (uintptr_t)pipeline);
    results->data[0].kind = WASM_INT_TYPE;
    results->data[0].of.WASM_POINTER_NAME = pipeline_index;

    return NULL;
}

ExternRefRegistry BIND_GROUP_LAYOUTS = {.count = 0, .mappings = {}};

// WGPU_EXPORT WGPUBindGroupLayout
// wgpuComputePipelineGetBindGroupLayout(WGPUComputePipeline computePipeline,
// uint32_t groupIndex) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuComputePipelineGetBindGroupLayoutImport(
    void *env, const wasm_val_vec_t *args, wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuComputePipelineGetBindGroupLayout, 2);

    // Get compute pipeline from registry
    WGPUComputePipeline pipeline = (WGPUComputePipeline)get_mapping(
        &COMPUTE_PIPELINES, wasm_val_to_native_int(args->data[0]));

    // Get group index
    uint32_t group_index = (uint32_t)wasm_val_to_native_int(args->data[1]);
    DRV_DEBUG("Group index: %u", group_index);

    // Get the bind group layout
    WGPUBindGroupLayout layout =
        wgpuComputePipelineGetBindGroupLayout(pipeline, group_index);
    DRV_DEBUG("Retrieved bind group layout: %p", (void *)layout);

    // Add layout to registry and return its index
    size_t layout_index = add_mapping(&BIND_GROUP_LAYOUTS, (uintptr_t)layout);
    results->data[0].kind = WASM_INT_TYPE;
    results->data[0].of.WASM_POINTER_NAME = layout_index;

    return NULL;
}

ExternRefRegistry TEXTURE_VIEWS = {.count = 0, .mappings = {}};

ExternRefRegistry SAMPLERS = {.count = 0, .mappings = {}};

ExternRefRegistry BIND_GROUPS = {.count = 0, .mappings = {}};

typedef struct {
    uint32_t nextInChain; // wasm32 pointer
    uint32_t label;       // wasm32 pointer
    uint32_t layout;      // wasm32 pointer
    uint32_t entryCount;
    uint32_t entries; // wasm32 pointer
} WASMBindGroupDescriptor;

typedef struct {
    uint32_t nextInChain; // wasm32 pointer
    uint32_t binding;
    uint32_t buffer; // wasm32 pointer
    uint64_t offset;
    uint64_t size;
    uint32_t sampler;     // wasm32 pointer
    uint32_t textureView; // wasm32 pointer
} WASMBindGroupEntry;

// Helper function to extract bind group entries from WASM memory
WGPUBindGroupDescriptor *
extract_bind_group_descriptor(wasm_memory_t *memory, uint32_t descriptor_ptr) {
    DRV_DEBUG("Extracting bind group descriptor at ptr: %u", descriptor_ptr);

    if (!memory) {
        DRV_DEBUG("Null memory pointer");
        return NULL;
    }

    // Verify memory bounds before copying
    size_t memory_size = wasm_memory_data_size(memory);
    if (descriptor_ptr >= memory_size ||
        descriptor_ptr + sizeof(WASMBindGroupDescriptor) > memory_size) {
        DRV_DEBUG("Descriptor pointer out of bounds: ptr=%u, size=%zu",
                  descriptor_ptr, memory_size);
        return NULL;
    }

    // Copy the WASM descriptor struct
    WASMBindGroupDescriptor wasm_desc = {0}; // Initialize to zero
    if (wasm_copy_struct_safe(memory, descriptor_ptr, &wasm_desc,
                              sizeof(WASMBindGroupDescriptor), 8)) {
        DRV_DEBUG("Failed to copy bind group descriptor");
        return NULL;
    }

    // Validate descriptor fields
    if (wasm_desc.entryCount >
        1000) { // Sanity check for reasonable entry count
        DRV_DEBUG("Entry count too large: %u", wasm_desc.entryCount);
        return NULL;
    }

    // Debug log the descriptor before creation
    DRV_DEBUG("  - descriptor - nextInChain: %u, label: %u, layout: %u, "
              "entryCount: %u, entries: %u",
              wasm_desc.nextInChain, wasm_desc.label, wasm_desc.layout,
              wasm_desc.entryCount, wasm_desc.entries);

    // Verify label pointer if present
    if (wasm_desc.label) {
        if (wasm_desc.label >= memory_size) {
            DRV_DEBUG("Label pointer out of bounds: %u", wasm_desc.label);
            return NULL;
        }
    }

    // Verify entries pointer if present
    if (wasm_desc.entryCount > 0) {
        if (wasm_desc.entries == 0 || wasm_desc.entries >= memory_size ||
            wasm_desc.entries +
                    (wasm_desc.entryCount * sizeof(WASMBindGroupEntry)) >
                memory_size) {
            DRV_DEBUG("Entries pointer out of bounds: ptr=%u, count=%u",
                      wasm_desc.entries, wasm_desc.entryCount);
            return NULL;
        }
    }

    // Allocate native descriptor
    WGPUBindGroupDescriptor *descriptor =
        malloc(sizeof(WGPUBindGroupDescriptor));
    if (!descriptor) {
        DRV_DEBUG("Failed to allocate bind group descriptor");
        return NULL;
    }

    // Copy basic fields
    descriptor->nextInChain = wasm_desc.nextInChain
                                  ? (const WGPUChainedStruct *)get_mapping(
                                        &CHAIN_STRUCTS, wasm_desc.nextInChain)
                                  : NULL;
    descriptor->label =
        wasm_desc.label ? wasm_copy_string_safe(memory, wasm_desc.label) : NULL;

    // Verify layout exists in registry
    WGPUBindGroupLayout layout =
        (WGPUBindGroupLayout)get_mapping(&BIND_GROUP_LAYOUTS, wasm_desc.layout);
    if (!layout && wasm_desc.layout != 0) {
        DRV_DEBUG("Failed to find bind group layout: %u", wasm_desc.layout);
        if (descriptor->label)
            free((void *)descriptor->label);
        free(descriptor);
        return NULL;
    }
    descriptor->layout = layout;
    descriptor->entryCount = wasm_desc.entryCount;

    // Copy entries array if present
    if (wasm_desc.entryCount > 0 && wasm_desc.entries) {
        DRV_DEBUG("Copying %u entries from address %u", wasm_desc.entryCount,
                  wasm_desc.entries);

        size_t entries_size = sizeof(WGPUBindGroupEntry) * wasm_desc.entryCount;
        WGPUBindGroupEntry *entries =
            calloc(wasm_desc.entryCount,
                   sizeof(WGPUBindGroupEntry)); // Use calloc to zero-initialize
        if (!entries) {
            DRV_DEBUG("Failed to allocate bind group entries");
            if (descriptor->label)
                free((void *)descriptor->label);
            free(descriptor);
            return NULL;
        }

        // Allocate temporary array for WASM entries
        WASMBindGroupEntry *wasm_entries =
            calloc(wasm_desc.entryCount,
                   sizeof(WASMBindGroupEntry)); // Use calloc to zero-initialize
        if (!wasm_entries) {
            DRV_DEBUG("Failed to allocate WASM bind group entries");
            free(entries);
            if (descriptor->label)
                free((void *)descriptor->label);
            free(descriptor);
            return NULL;
        }

        // Copy the WASM entries array from memory
        if (wasm_copy_struct_safe(
                memory, wasm_desc.entries, wasm_entries,
                sizeof(WASMBindGroupEntry) * wasm_desc.entryCount, 8)) {
            DRV_DEBUG("Failed to copy WASM bind group entries");
            free(wasm_entries);
            free(entries);
            if (descriptor->label)
                free((void *)descriptor->label);
            free(descriptor);
            return NULL;
        }

        DRV_DEBUG("WASMBindGroupEntry\n\
    - entries - nextInChain: %u, binding: %u, buffer: %u, offset: %u, size: %u",
                  wasm_entries[0].nextInChain, wasm_entries[0].binding,
                  wasm_entries[0].buffer, wasm_entries[0].offset,
                  wasm_entries[0].size);
        // Process each entry to convert WASM indices to native pointers
        for (size_t i = 0; i < wasm_desc.entryCount; i++) {
            DRV_DEBUG("Processing entry %zu", i);

            entries[i].nextInChain =
                wasm_entries[i].nextInChain
                    ? (const WGPUChainedStruct *)get_mapping(
                          &CHAIN_STRUCTS, wasm_entries[i].nextInChain)
                    : NULL;
            entries[i].binding = wasm_entries[i].binding;

            // Get buffer if present
            if (wasm_entries[i].buffer) {
                entries[i].buffer =
                    (WGPUBuffer)get_mapping(&BUFFERS, wasm_entries[i].buffer);
                if (!entries[i].buffer) {
                    DRV_DEBUG("Failed to find buffer: %u",
                              wasm_entries[i].buffer);
                    free(wasm_entries);
                    free(entries);
                    if (descriptor->label)
                        free((void *)descriptor->label);
                    free(descriptor);
                    return NULL;
                }
            } else {
                entries[i].buffer = NULL;
            }

            entries[i].offset = wasm_entries[i].offset;
            entries[i].size = wasm_entries[i].size;

            // Get sampler if present
            if (wasm_entries[i].sampler) {
                entries[i].sampler = (WGPUSampler)get_mapping(
                    &SAMPLERS, wasm_entries[i].sampler);
                if (!entries[i].sampler) {
                    DRV_DEBUG("Failed to find sampler: %u",
                              wasm_entries[i].sampler);
                    free(wasm_entries);
                    free(entries);
                    if (descriptor->label)
                        free((void *)descriptor->label);
                    free(descriptor);
                    return NULL;
                }
            } else {
                entries[i].sampler = NULL;
            }

            // Get texture view if present
            if (wasm_entries[i].textureView) {
                entries[i].textureView = (WGPUTextureView)get_mapping(
                    &TEXTURE_VIEWS, wasm_entries[i].textureView);
                if (!entries[i].textureView) {
                    DRV_DEBUG("Failed to find texture view: %u",
                              wasm_entries[i].textureView);
                    free(wasm_entries);
                    free(entries);
                    if (descriptor->label)
                        free((void *)descriptor->label);
                    free(descriptor);
                    return NULL;
                }
            } else {
                entries[i].textureView = NULL;
            }

            DRV_DEBUG("  - entry[%zu] - binding: %u, buffer: %p, offset: %lu, "
                      "size: %lu, sampler: %p, textureView: %p",
                      i, entries[i].binding, (void *)entries[i].buffer,
                      entries[i].offset, entries[i].size,
                      (void *)entries[i].sampler,
                      (void *)entries[i].textureView);
        }

        free(wasm_entries);
        descriptor->entries = entries;
    } else {
        descriptor->entries = NULL;
        descriptor->entryCount = 0;
    }

    DRV_DEBUG("Successfully created bind group descriptor with %u entries",
              descriptor->entryCount);
    return descriptor;
}

// WGPU_EXPORT WGPUBindGroup wgpuDeviceCreateBindGroup(WGPUDevice device,
// WGPUBindGroupDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuDeviceCreateBindGroupImport(void *env,
                                             const wasm_val_vec_t *args,
                                             wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuDeviceCreateBindGroup, 2);

    WASM_INT_NATIVE_TYPE device_index = wasm_val_to_native_int(args->data[0]);
    DRV_DEBUG("Retrieved device index: %u", device_index);

    // Get device from registry
    WGPUDevice device = (WGPUDevice)get_mapping(&DEVICES, device_index);
    DRV_DEBUG("Retrieved device: %p", (void *)device);

    // Extract the descriptor
    WGPUBindGroupDescriptor *descriptor = extract_bind_group_descriptor(
        wasm_memory, wasm_val_to_native_int(args->data[1]));
    if (!descriptor) {
        DRV_DEBUG("Failed to extract bind group descriptor");
        results->data[0].kind = WASM_INT_TYPE;
        results->data[0].of.WASM_POINTER_NAME = 0;
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

    // Clean up the descriptor
    if (descriptor->label)
        free((void *)descriptor->label);
    if (descriptor->entries)
        free((void *)descriptor->entries);
    free(descriptor);

    // Add bind group to registry and return its index
    size_t bind_group_index = add_mapping(&BIND_GROUPS, (uintptr_t)bind_group);
    results->data[0].kind = WASM_INT_TYPE;
    results->data[0].of.WASM_POINTER_NAME = bind_group_index;

    return NULL;
}

ExternRefRegistry COMMAND_ENCODERS = {.count = 0, .mappings = {}};

typedef struct {
    uint32_t nextInChain; // wasm32 pointer
    uint32_t label;       // wasm32 pointer
} WASMCommandEncoderDescriptor;

// Helper function to extract command encoder descriptor from WASM memory
WGPUCommandEncoderDescriptor *
extract_command_encoder_descriptor(wasm_memory_t *memory,
                                   uint32_t descriptor_ptr) {
    DRV_DEBUG("Extracting command encoder descriptor at ptr: %u",
              descriptor_ptr);

    if (!memory) {
        DRV_DEBUG("Null memory pointer");
        return NULL;
    }

    // Verify memory bounds before copying
    size_t memory_size = wasm_memory_data_size(memory);
    if (descriptor_ptr >= memory_size ||
        descriptor_ptr + sizeof(WASMCommandEncoderDescriptor) > memory_size) {
        DRV_DEBUG("Descriptor pointer out of bounds: ptr=%u, size=%zu",
                  descriptor_ptr, memory_size);
        return NULL;
    }

    // Copy the WASM descriptor struct
    WASMCommandEncoderDescriptor wasm_desc = {0}; // Initialize to zero
    if (wasm_copy_struct_safe(memory, descriptor_ptr, &wasm_desc,
                              sizeof(WASMCommandEncoderDescriptor), 4)) {
        DRV_DEBUG("Failed to copy command encoder descriptor");
        return NULL;
    }

    // Debug log the descriptor before creation
    DRV_DEBUG("  - descriptor - nextInChain: %u, label: %u",
              wasm_desc.nextInChain, wasm_desc.label);

    // Verify label pointer if present
    if (wasm_desc.label) {
        if (wasm_desc.label >= memory_size) {
            DRV_DEBUG("Label pointer out of bounds: %u", wasm_desc.label);
            return NULL;
        }
    }

    // Allocate native descriptor
    WGPUCommandEncoderDescriptor *descriptor =
        malloc(sizeof(WGPUCommandEncoderDescriptor));
    if (!descriptor) {
        DRV_DEBUG("Failed to allocate command encoder descriptor");
        return NULL;
    }

    // Copy basic fields
    descriptor->nextInChain = wasm_desc.nextInChain
                                  ? (const WGPUChainedStruct *)get_mapping(
                                        &CHAIN_STRUCTS, wasm_desc.nextInChain)
                                  : NULL;
    descriptor->label =
        wasm_desc.label ? wasm_copy_string_safe(memory, wasm_desc.label) : NULL;

    DRV_DEBUG("Successfully created command encoder descriptor");
    return descriptor;
}

// WGPU_EXPORT WGPUCommandEncoder wgpuDeviceCreateCommandEncoder(WGPUDevice
// device, WGPU_NULLABLE WGPUCommandEncoderDescriptor const * descriptor)
// WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuDeviceCreateCommandEncoderImport(void *env,
                                                  const wasm_val_vec_t *args,
                                                  wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuDeviceCreateCommandEncoder, 2);

    // Get device from registry
    WASM_INT_NATIVE_TYPE device_index = wasm_val_to_native_int(args->data[0]);
    DRV_DEBUG("Retrieved device index: %u", device_index);

    WGPUDevice device = (WGPUDevice)get_mapping(&DEVICES, device_index);
    DRV_DEBUG("Retrieved device: %p", (void *)device);

    // Extract the descriptor if provided
    WGPUCommandEncoderDescriptor *descriptor = NULL;
    uint32_t descriptor_ptr = wasm_val_to_native_int(args->data[1]);
    if (descriptor_ptr != 0) {
        descriptor =
            extract_command_encoder_descriptor(wasm_memory, descriptor_ptr);
        if (!descriptor) {
            DRV_DEBUG("Failed to extract command encoder descriptor");
            results->data[0].kind = WASM_INT_TYPE;
            results->data[0].of.WASM_POINTER_NAME = 0;
            return NULL;
        }
    }

    // Create the command encoder
    WGPUCommandEncoder encoder =
        wgpuDeviceCreateCommandEncoder(device, descriptor);
    DRV_DEBUG("Created command encoder: %p", (void *)encoder);

    // Clean up the descriptor
    if (descriptor) {
        if (descriptor->label)
            free((void *)descriptor->label);
        free(descriptor);
    }

    // Add command encoder to registry and return its index
    size_t encoder_index = add_mapping(&COMMAND_ENCODERS, (uintptr_t)encoder);
    results->data[0].kind = WASM_INT_TYPE;
    results->data[0].of.WASM_POINTER_NAME = encoder_index;

    return NULL;
}

ExternRefRegistry COMPUTE_PASS_ENCODERS = {.count = 0, .mappings = {}};

typedef struct {
    uint32_t nextInChain;
    uint32_t label;
    uint32_t timestampWrites;
} WASMComputePassDescriptor;

// Helper function to extract compute pass descriptor from WASM memory
WGPUComputePassDescriptor *
extract_compute_pass_descriptor(wasm_memory_t *memory,
                                uint32_t descriptor_ptr) {
    WGPUComputePassDescriptor *descriptor =
        (WGPUComputePassDescriptor *)malloc(sizeof(WGPUComputePassDescriptor));

    if (descriptor_ptr == 0) {
        return descriptor;
    }

    size_t memory_size = wasm_memory_data_size(memory);
    byte_t *memory_data = wasm_memory_data(memory);

    // Check if the descriptor pointer is within bounds
    if (descriptor_ptr >= memory_size ||
        descriptor_ptr + sizeof(WASMComputePassDescriptor) > memory_size) {
        DRV_DEBUG("Invalid descriptor pointer");
        return descriptor;
    }

    WASMComputePassDescriptor wasm_desc = {0};
    if (wasm_copy_struct_safe(memory, descriptor_ptr, &wasm_desc,
                              sizeof(WASMComputePassDescriptor), 4)) {
        DRV_DEBUG("Failed to copy compute pass descriptor");
        return descriptor;
    }

    // Copy fields from WASM descriptor to native descriptor
    descriptor->nextInChain = NULL; // We don't support chain extensions yet
    if (wasm_desc.label != 0) {
        descriptor->label = (char *)memory_data + wasm_desc.label;
    }

    // Handle timestamp writes if present
    if (wasm_desc.timestampWrites != 0) {
        WGPUComputePassTimestampWrites *timestampWrites =
            (WGPUComputePassTimestampWrites *)(memory_data +
                                               wasm_desc.timestampWrites);
        descriptor->timestampWrites = timestampWrites;
    }

    return descriptor;
}

// WGPU_EXPORT WGPUComputePassEncoder
// wgpuCommandEncoderBeginComputePass(WGPUCommandEncoder commandEncoder,
// WGPU_NULLABLE WGPUComputePassDescriptor const * descriptor)
// WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *
wgpuCommandEncoderBeginComputePassImport(void *env, const wasm_val_vec_t *args,
                                         wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuCommandEncoderBeginComputePass, 2);

    // Get the command encoder from the first argument
    WGPUCommandEncoder commandEncoder = (WGPUCommandEncoder)get_mapping(
        &COMMAND_ENCODERS, wasm_val_to_native_int(args->data[0]));

    // Extract the descriptor from WASM memory if provided
    WGPUComputePassDescriptor *descriptor =
        extract_compute_pass_descriptor(wasm_memory, args->data[1].of.i32);

    // Call the native function
    WGPUComputePassEncoder encoder =
        wgpuCommandEncoderBeginComputePass(commandEncoder, descriptor);

    // Store the result in the extern ref registry
    results->data[0].kind = WASM_INT_TYPE;
    results->data[0].of.i32 =
        add_mapping(&COMPUTE_PASS_ENCODERS, (uintptr_t)encoder);

    return NULL;
}

// WGPU_EXPORT void wgpuComputePassEncoderSetPipeline(WGPUComputePassEncoder
// computePassEncoder, WGPUComputePipeline pipeline) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuComputePassEncoderSetPipelineImport(void *env,
                                                     const wasm_val_vec_t *args,
                                                     wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuComputePassEncoderSetPipeline, 2);

    // Get the compute pass encoder from the first argument
    WGPUComputePassEncoder encoder = (WGPUComputePassEncoder)get_mapping(
        &COMPUTE_PASS_ENCODERS, wasm_val_to_native_int(args->data[0]));

    // Get the compute pipeline from the second argument
    WGPUComputePipeline pipeline = (WGPUComputePipeline)get_mapping(
        &COMPUTE_PIPELINES, wasm_val_to_native_int(args->data[1]));

    // Call the native function
    wgpuComputePassEncoderSetPipeline(encoder, pipeline);

    return NULL;
}

// WGPU_EXPORT void wgpuComputePassEncoderSetBindGroup(WGPUComputePassEncoder
// computePassEncoder, uint32_t groupIndex, WGPU_NULLABLE WGPUBindGroup group,
// size_t dynamicOffsetCount, uint32_t const * dynamicOffsets)
// WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *
wgpuComputePassEncoderSetBindGroupImport(void *env, const wasm_val_vec_t *args,
                                         wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuComputePassEncoderSetBindGroup, 5);

    // Get the compute pass encoder from the first argument
    WGPUComputePassEncoder encoder = (WGPUComputePassEncoder)get_mapping(
        &COMPUTE_PASS_ENCODERS, wasm_val_to_native_int(args->data[0]));

    // Get the group index from the second argument
    uint32_t groupIndex = wasm_val_to_native_int(args->data[1]);

    // Get the bind group from the third argument
    WGPUBindGroup group = NULL;
    if (args->data[2].of.i32 != 0) {
        group = (WGPUBindGroup)get_mapping(
            &BIND_GROUPS, wasm_val_to_native_int(args->data[2]));
    }

    // Get dynamic offset count and offsets array from the fourth and fifth
    // arguments
    size_t dynamicOffsetCount = wasm_val_to_native_int(args->data[3]);
    const uint32_t *dynamicOffsets = NULL;
    if (dynamicOffsetCount > 0 && args->data[4].of.i32 != 0) {
        // Get pointer to dynamic offsets array in WASM memory
        dynamicOffsets = (const uint32_t *)(wasm_memory_data(wasm_memory) +
                                            args->data[4].of.i32);

        // Validate that the offsets array is within bounds
        size_t required_size = dynamicOffsetCount * sizeof(uint32_t);
        if (args->data[4].of.i32 + required_size >
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

// WGPU_EXPORT void
// wgpuComputePassEncoderDispatchWorkgroups(WGPUComputePassEncoder
// computePassEncoder, uint32_t workgroupCountX, uint32_t workgroupCountY,
// uint32_t workgroupCountZ) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuComputePassEncoderDispatchWorkgroupsImport(
    void *env, const wasm_val_vec_t *args, wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuComputePassEncoderDispatchWorkgroups, 4);

    // Get the compute pass encoder from the first argument
    WGPUComputePassEncoder encoder = (WGPUComputePassEncoder)get_mapping(
        &COMPUTE_PASS_ENCODERS, wasm_val_to_native_int(args->data[0]));

    // Get workgroup counts from the remaining arguments
    uint32_t workgroupCountX = wasm_val_to_native_int(args->data[1]);
    uint32_t workgroupCountY = wasm_val_to_native_int(args->data[2]);
    uint32_t workgroupCountZ = wasm_val_to_native_int(args->data[3]);

    // Call the native function
    wgpuComputePassEncoderDispatchWorkgroups(encoder, workgroupCountX,
                                             workgroupCountY, workgroupCountZ);

    return NULL;
}

// WGPU_EXPORT void wgpuComputePassEncoderEnd(WGPUComputePassEncoder
// computePassEncoder) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuComputePassEncoderEndImport(void *env,
                                             const wasm_val_vec_t *args,
                                             wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuComputePassEncoderEnd, 1);

    // Get the compute pass encoder from the argument
    WGPUComputePassEncoder encoder = (WGPUComputePassEncoder)get_mapping(
        &COMPUTE_PASS_ENCODERS, wasm_val_to_native_int(args->data[0]));

    // Call the native function
    wgpuComputePassEncoderEnd(encoder);

    return NULL;
}

// Must be implemented: https://github.com/gfx-rs/wgpu-native/issues/412
// WGPU_EXPORT void wgpuComputePassEncoderRelease(WGPUComputePassEncoder
// computePassEncoder) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuComputePassEncoderReleaseImport(void *env,
                                                 const wasm_val_vec_t *args,
                                                 wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuComputePassEncoderRelease, 1);

    // Get the compute pass encoder from the argument
    WGPUComputePassEncoder encoder = (WGPUComputePassEncoder)get_mapping(
        &COMPUTE_PASS_ENCODERS, wasm_val_to_native_int(args->data[0]));

    // Call the native function
    wgpuComputePassEncoderRelease(encoder);

    return NULL;
}

// WGPU_EXPORT void wgpuCommandEncoderCopyBufferToBuffer(WGPUCommandEncoder
// commandEncoder, WGPUBuffer source, uint64_t sourceOffset, WGPUBuffer
// destination, uint64_t destinationOffset, uint64_t size)
// WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuCommandEncoderCopyBufferToBufferImport(
    void *env, const wasm_val_vec_t *args, wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuCommandEncoderCopyBufferToBuffer, 9);

    // Get the command encoder from the first argument
    WGPUCommandEncoder encoder = (WGPUCommandEncoder)get_mapping(
        &COMMAND_ENCODERS, wasm_val_to_native_int(args->data[0]));

    // Get source buffer from the second argument
    WGPUBuffer source = (WGPUBuffer)get_mapping(
        &BUFFERS, wasm_val_to_native_int(args->data[1]));

    // Get source offset from the third and fourth arguments (low and high 32
    // bits)
    uint64_t sourceOffset =
        ((uint64_t)wasm_val_to_native_int(args->data[3]) << 32) |
        (uint64_t)wasm_val_to_native_int(args->data[2]);

    // Get destination buffer from the fifth argument
    WGPUBuffer destination = (WGPUBuffer)get_mapping(
        &BUFFERS, wasm_val_to_native_int(args->data[4]));

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

ExternRefRegistry COMMAND_BUFFERS = {.count = 0, .mappings = {}};

typedef struct {
    uint32_t next_in_chain;
    uint32_t label;
} WASMCommandBufferDescriptor;

WGPUCommandBufferDescriptor *
extract_command_buffer_descriptor(wasm_memory_t *memory,
                                  uint32_t descriptor_ptr) {
    WGPUCommandBufferDescriptor *descriptor =
        (WGPUCommandBufferDescriptor *)malloc(
            sizeof(WGPUCommandBufferDescriptor));
    memset(descriptor, 0, sizeof(WGPUCommandBufferDescriptor));

    if (descriptor_ptr == 0) {
        return descriptor;
    }

    size_t memory_size = wasm_memory_data_size(memory);
    byte_t *memory_data = wasm_memory_data(memory);

    // Check if the descriptor pointer is within bounds
    if (descriptor_ptr >= memory_size ||
        descriptor_ptr + sizeof(WASMCommandBufferDescriptor) > memory_size) {
        DRV_DEBUG("Invalid descriptor pointer");
        return descriptor;
    }

    // Copy the WASM descriptor
    WASMCommandBufferDescriptor wasm_descriptor;
    wasm_copy_struct_safe(memory, descriptor_ptr, &wasm_descriptor,
                          sizeof(WASMCommandBufferDescriptor), 4);

    // Convert the label if present
    if (wasm_descriptor.label != 0) {
        descriptor->label = (const char *)(memory_data + wasm_descriptor.label);
    }

    // Handle next_in_chain if present (currently we don't support any chain
    // types)
    descriptor->nextInChain = NULL;

    return descriptor;
}

// WGPU_EXPORT WGPUCommandBuffer wgpuCommandEncoderFinish(WGPUCommandEncoder
// commandEncoder, WGPU_NULLABLE WGPUCommandBufferDescriptor const * descriptor)
// WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuCommandEncoderFinishImport(void *env,
                                            const wasm_val_vec_t *args,
                                            wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuCommandEncoderFinish, 2);

    // Get the command encoder from the first argument
    WGPUCommandEncoder encoder = (WGPUCommandEncoder)get_mapping(
        &COMMAND_ENCODERS, wasm_val_to_native_int(args->data[0]));

    WGPUCommandBufferDescriptor *descriptor =
        extract_command_buffer_descriptor(wasm_memory, args->data[1].of.i32);

    // Call the native function
    WGPUCommandBuffer command_buffer =
        wgpuCommandEncoderFinish(encoder, descriptor);

    // Add the command buffer to our registry and return the index
    size_t command_buffer_index =
        add_mapping(&COMMAND_BUFFERS, (uintptr_t)command_buffer);
    DRV_DEBUG("created command_buffer_index = %zu", command_buffer_index);
    results->data[0].kind = WASM_INT_TYPE;
    results->data[0].of.i32 = command_buffer_index;

    return NULL;
}

// WGPU_EXPORT void wgpuQueueWriteBuffer(WGPUQueue queue, WGPUBuffer buffer,
// uint64_t bufferOffset, void const * data, size_t size)
// WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuQueueWriteBufferImport(void *env, const wasm_val_vec_t *args,
                                        wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuQueueWriteBuffer, 6);

    WGPUQueue queue =
        (WGPUQueue)get_mapping(&QUEUES, wasm_val_to_native_int(args->data[0]));

    WGPUBuffer buffer = (WGPUBuffer)get_mapping(
        &BUFFERS, wasm_val_to_native_int(args->data[1]));

    uint64_t bufferOffset =
        ((uint64_t)wasm_val_to_native_int(args->data[3]) << 32) |
        (uint64_t)wasm_val_to_native_int(args->data[2]);

    size_t size = wasm_val_to_native_int(args->data[5]);

    uintptr_t native_buffer_ptr;
    int data_ptr_status = wasm_get_buffer_pointer_safe(
        wasm_memory, args->data[4].of.i32, size, &native_buffer_ptr);

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

// WGPU_EXPORT void wgpuQueueSubmit(WGPUQueue queue, size_t commandCount,
// WGPUCommandBuffer const * commands) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuQueueSubmitImport(void *env, const wasm_val_vec_t *args,
                                   wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuQueueSubmit, 3);

    // Get the queue from registry
    WGPUQueue queue =
        (WGPUQueue)get_mapping(&QUEUES, wasm_val_to_native_int(args->data[0]));

    // Get command count
    size_t commandCount = wasm_val_to_native_int(args->data[1]);

    WASM_INT_NATIVE_TYPE command_buffer_index_ptr =
        wasm_val_to_native_int(args->data[2]);
    // Get wasm memory at `command_buffer_index_ptr`
    WASM_INT_NATIVE_TYPE command_buffer_index =
        *(WASM_INT_NATIVE_TYPE *)(wasm_base_ptr + command_buffer_index_ptr);
    DRV_DEBUG("command_buffer_index_ptr = %p, command_buffer_index = %u",
              command_buffer_index_ptr, command_buffer_index);
    WGPUCommandBuffer command_buffer =
        (WGPUCommandBuffer)get_mapping(&COMMAND_BUFFERS, command_buffer_index);

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
    WASM_INT_NATIVE_TYPE callback_index =
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
    wasm_val_vec_new_uninitialized(&args, 4);
    args.data[0].kind = WASM_INT_TYPE;
    args.data[0].of.i32 = status;
    args.data[1].kind = WASM_INT_TYPE;
    args.data[1].of.i32 = (WASM_INT_NATIVE_TYPE)userdata;
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

// WGPU_EXPORT void wgpuBufferMapAsync(WGPUBuffer buffer, WGPUMapModeFlags mode,
// size_t offset, size_t size, WGPUBufferMapAsyncCallback callback,
// WGPU_NULLABLE void * userdata) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuBufferMapAsyncImport(void *env, const wasm_val_vec_t *args,
                                      wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuBufferMapAsync, 6);

    WGPUBuffer buffer = (WGPUBuffer)get_mapping(
        &BUFFERS, wasm_val_to_native_int(args->data[0]));
    WGPUMapModeFlags mode =
        (WGPUMapModeFlags)wasm_val_to_native_int(args->data[1]);
    size_t offset = (size_t)wasm_val_to_native_int(args->data[2]);
    size_t size = (size_t)wasm_val_to_native_int(args->data[3]);

    // an index into the module’s function table
    WASM_INT_NATIVE_TYPE callback_index = wasm_val_to_native_int(args->data[4]);
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
    WASM_INT_NATIVE_TYPE userdata_wasm_ptr =
        wasm_val_to_native_int(args->data[5]);

    DRV_DEBUG("buffer = %p, mode = %d, offset = %zu, size = %zu, callback = "
              "%p, userdata = %p",
              buffer, mode, offset, size, callback, (void *)userdata_wasm_ptr);
    wgpuBufferMapAsync(buffer, mode, offset, size, callback,
                       (void *)userdata_wasm_ptr);

    return NULL;
}

// WGPU_EXPORT void * wgpuBufferGetMappedRange(WGPUBuffer buffer, size_t offset,
// size_t size) WGPU_FUNCTION_ATTRIBUTE;
wasm_trap_t *wgpuBufferGetMappedRangeImport(void *env,
                                            const wasm_val_vec_t *args,
                                            wasm_val_vec_t *results) {
    HANDLER_INIT(wgpuBufferGetMappedRange, 3);

    WGPUBuffer buffer = (WGPUBuffer)get_mapping(
        &BUFFERS, wasm_val_to_native_int(args->data[0]));
    size_t offset = (size_t)wasm_val_to_native_int(args->data[1]);
    size_t size = (size_t)wasm_val_to_native_int(args->data[2]);

    void *mapped = wgpuBufferGetMappedRange(buffer, offset, size);

    // Debug print out memory
    DRV_DEBUG("mapped = %p, size = %zu", mapped, size);
    for (int i = 0; i < size; i++) {
        DRV_DEBUG("%02x", ((byte_t *)mapped)[i]);
    }

    wasm_func_t *wasm_malloc = get_exported_function(proc, "malloc");

    // set up arguments
    wasm_val_vec_t wm_args = {};
    wasm_val_vec_new_uninitialized(&wm_args, 1);
    wm_args.size = 1;
    wm_args.num_elems = 1;
    wm_args.data[0].kind = WASM_INT_TYPE;
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

    WASM_INT_NATIVE_TYPE wasm_malloc_res = wm_results.data[0].of.i32;
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

    results->data[0].kind = WASM_POINTER_TYPE;
    results->data[0].of.i32 = wasm_malloc_res;

    return NULL;
}

static const struct
{
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
};

int set_callback_webgpu(wasm_byte_t *module_name, wasm_byte_t *name,
                        wasm_func_callback_with_env_t *callback_out) {
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
