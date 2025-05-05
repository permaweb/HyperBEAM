#include "../include/hb_core.h"
#include "../lib/include/compile.h"
#include "hb_logging.h"
#include "hb_driver.h"
#include <stddef.h>

// Declare the atoms used in Erlang driver communication
ErlDrvTermData atom_ok;
ErlDrvTermData atom_error;
ErlDrvTermData atom_compilation_result;

static ErlDrvData compiler_driver_start(ErlDrvPort port, char *buff) {
    ErlDrvSysInfo info;
    driver_system_info(&info, sizeof(info));

    DRV_DEBUG("Starting AOT Compiler driver");
    DRV_DEBUG("Port: %p", port);
    DRV_DEBUG("Buff: %s", buff);
    DRV_DEBUG("Caller PID: %d", driver_caller(port));
    DRV_DEBUG("ERL_DRV_EXTENDED_MAJOR_VERSION: %d", ERL_DRV_EXTENDED_MAJOR_VERSION);
    DRV_DEBUG("ERL_DRV_EXTENDED_MINOR_VERSION: %d", ERL_DRV_EXTENDED_MINOR_VERSION);
    DRV_DEBUG("ERL_DRV_FLAG_USE_PORT_LOCKING: %d", ERL_DRV_FLAG_USE_PORT_LOCKING);
    DRV_DEBUG("info.major_version: %d", info.driver_major_version);
    DRV_DEBUG("info.minor_version: %d", info.driver_major_version);
    DRV_DEBUG("info.thread_support: %d", info.thread_support);
    DRV_DEBUG("info.smp_support: %d", info.smp_support);
    DRV_DEBUG("info.async_threads: %d", info.async_threads);
    DRV_DEBUG("info.scheduler_threads: %d", info.scheduler_threads);
    DRV_DEBUG("info.nif_major_version: %d", info.nif_major_version);
    DRV_DEBUG("info.nif_minor_version: %d", info.nif_minor_version);
    DRV_DEBUG("info.dirty_scheduler_support: %d", info.dirty_scheduler_support);
    DRV_DEBUG("info.erts_version: %s", info.erts_version);
    DRV_DEBUG("info.otp_release: %s", info.otp_release);

    Proc* proc = driver_alloc(sizeof(Proc));
    memset(proc, 0, sizeof(Proc));

    proc->is_running = erl_drv_mutex_create("aot_compiler_mutex");

    proc->port = port;
    DRV_DEBUG("Port: %p", proc->port);

    // Set a base port key, which will be incremented for each async call to ensure
    // that the calls are on different threads so cannot deadlock each other.
    // TODO: This is not a good solution due to wasted overhead of threads.
    proc->port_key = driver_async_port_key(port);
    DRV_DEBUG("Port key: %d", proc->port_key);

    proc->port_term = driver_mk_port(proc->port);
    DRV_DEBUG("Port term: %p", proc->port_term);

    proc->start_time = time(NULL);
    DRV_DEBUG("Start time: %ld", proc->start_time);

    return (ErlDrvData)proc;
}

static void compiler_driver_stop(ErlDrvData raw) {
    // DRV_DEBUG("IGNORING STOP");
    Proc* proc = (Proc*)raw;
    DRV_DEBUG("Stopping AOT Compiler driver");

    // We need to first grab the lock, then unlock it and destroy it. Must be a better way...
    DRV_DEBUG("Grabbing is_running mutex to shutdown...");
    drv_lock(proc->is_running);
    drv_unlock(proc->is_running);
    DRV_DEBUG("Destroying is_running mutex");
    erl_drv_mutex_destroy(proc->is_running);
    // Cleanup WASM resources
    DRV_DEBUG("Cleaning up AOT Compiler resources");
    DRV_DEBUG("Freeing proc");
    driver_free(proc);
    DRV_DEBUG("Freed proc");
}

static void compiler_driver_output(ErlDrvData raw, char *buff, ErlDrvSizeT bufflen) {
    DRV_DEBUG("AOT Compiler driver output received");
    Proc* proc = (Proc*)raw;
    //DRV_DEBUG("Port: %p", proc->port);
    //DRV_DEBUG("Port term: %p", proc->port_term);

    int index = 0;
    int version;
    if(ei_decode_version(buff, &index, &version) != 0) {
        send_error(proc->port_term, "Failed to decode message header (version).");
        return;
    }
    //DRV_DEBUG("Received term has version: %d", version);
    //DRV_DEBUG("Index: %d. buff_len: %d. buff: %p", index, bufflen, buff);
    int arity;
    ei_decode_tuple_header(buff, &index, &arity);
    //DRV_DEBUG("Term arity: %d", arity);

    char command[MAXATOMLEN];
    ei_decode_atom(buff, &index, command);
    DRV_DEBUG("Port %p received command: %s, arity: %d", proc->port, command, arity);
    
    if (strcmp(command, "compile") == 0) {
        DRV_DEBUG("Received compile command");
        // Start async initialization
        proc->pid = driver_caller(proc->port);
        //DRV_DEBUG("Caller PID: %d", proc->pid);
        int size, type, mode_size;
        char* mode;
        ei_get_type(buff, &index, &type, &size);
        //DRV_DEBUG("WASM binary size: %d bytes. Type: %c", size, type);
        void* wasm_binary = driver_alloc(size);
        long size_l = (long)size;
        ei_decode_binary(buff, &index, wasm_binary, &size_l);
        ei_get_type(buff, &index, &type, &mode_size);
        // the init message size + '\0' character
        mode = driver_alloc(mode_size + 1);
        ei_decode_atom(buff, &index, mode);
        CompileWasmReq* mod_bin = driver_alloc(sizeof(CompileWasmReq));
        mod_bin->proc = proc;
        mod_bin->binary = wasm_binary;
        mod_bin->size = size;
        //DRV_DEBUG("Calling for async thread to init");
        proc->port_key++;
        driver_async(proc->port, &proc->port_key, invoke_compile, mod_bin, NULL);
    }
    else {
        DRV_DEBUG("Unknown command: %s", command);
        send_error(proc->port_term, "Unknown command");
    }
}

void invoke_compile(void *raw) {
    CompileWasmReq* mod_bin = (CompileWasmReq*)raw;
    Proc* proc = mod_bin->proc;
    DRV_DEBUG("Invoking compile");
    DRV_DEBUG("WASM binary size: %d bytes", mod_bin->size);

    CompileOpts compile_opts = { 0 };
    compile_opts.mem_alloc_option.allocator.malloc_func = driver_alloc;
    compile_opts.mem_alloc_option.allocator.realloc_func = driver_realloc;
    compile_opts.mem_alloc_option.allocator.free_func = driver_free;

    drv_lock(proc->is_running);

    WasmInterfaceFunction* imports;
    size_t import_count;
    WasmInterfaceFunction* exports;
    size_t export_count;

    size_t aot_module_size;
    uint8_t* aot_module;
    int res = hb_wasm_aot_compile(&compile_opts, mod_bin->binary, mod_bin->size, &imports, &import_count, &exports, &export_count, &aot_module, &aot_module_size);
    DRV_DEBUG("AOT module size: %d bytes", aot_module_size);
    
    if (res == 0) {
        DRV_DEBUG("Compilation successful");
        send_compilation_result(proc, imports, import_count, exports, export_count, aot_module, aot_module_size);
    }
    else {
        DRV_DEBUG("Compilation failed");
        send_error(proc->port_term, "Compilation failed");
    }

    drv_unlock(proc->is_running);
}

const char* wasm_import_export_kind_to_string(wasm_import_export_kind_t kind) {
    switch (kind) {
        case WASM_IMPORT_EXPORT_KIND_FUNC: return "func";
        case WASM_IMPORT_EXPORT_KIND_GLOBAL: return "global";
        case WASM_IMPORT_EXPORT_KIND_TABLE: return "table";
        case WASM_IMPORT_EXPORT_KIND_MEMORY: return "memory";
        default: return "unknown";
    }
}

void send_compilation_result(Proc* proc, WasmInterfaceFunction* imports, size_t import_count, WasmInterfaceFunction* exports, size_t export_count, uint8_t* aot_module, uint32_t aot_module_size) {
    DRV_DEBUG("Sending compilation result");
    DRV_DEBUG("Imports: %d (%p)", import_count, imports);
    DRV_DEBUG("Exports: %d (%p)", export_count, exports);
    DRV_DEBUG("AOT module: %p", aot_module);
    DRV_DEBUG("AOT module size: %d bytes", aot_module_size);

    // Send the compilation result to the caller
    int msg_size = sizeof(ErlDrvTermData) * (2 + 3 + 5 + (13 * import_count) + (11 * export_count) + 4);
    ErlDrvTermData* msg = driver_alloc(msg_size);
    int msg_i = 0;

    msg[msg_i++] = ERL_DRV_ATOM;
    msg[msg_i++] = atom_compilation_result;
    
    DRV_DEBUG("Sending %d imports", import_count);
    for (size_t i = 0; i < import_count; i++) {
		WasmInterfaceFunction import = imports[i];
        DRV_DEBUG("import %d pointers: %p %p %p", i, import.module_name, import.field_name, import.signature);
        DRV_DEBUG("Sending import[%d]: %s.%s %s", i, import.module_name, import.field_name, import.signature == NULL ? "(null)" : import.signature);

        msg[msg_i++] = ERL_DRV_ATOM;
        msg[msg_i++] = driver_mk_atom((char*)wasm_import_export_kind_to_string(import.kind));
        msg[msg_i++] = ERL_DRV_STRING;
        msg[msg_i++] = (ErlDrvTermData)import.module_name;
        msg[msg_i++] = strlen(import.module_name);
        msg[msg_i++] = ERL_DRV_STRING;
        msg[msg_i++] = (ErlDrvTermData)import.field_name;
        msg[msg_i++] = strlen(import.field_name);
        msg[msg_i++] = ERL_DRV_STRING;
        msg[msg_i++] = (ErlDrvTermData)import.signature;
        msg[msg_i++] = strlen(import.signature);
        msg[msg_i++] = ERL_DRV_TUPLE;
        msg[msg_i++] = 4;
    }

    msg[msg_i++] = ERL_DRV_NIL;
    msg[msg_i++] = ERL_DRV_LIST;
    msg[msg_i++] = import_count + 1;

    DRV_DEBUG("Sending %d exports", export_count);
    for (size_t i = 0; i < export_count; i++) {
		WasmInterfaceFunction export = exports[i];
        DRV_DEBUG("Sending export[%d]: %s %s", i, export.field_name, export.signature == NULL ? "(null)" : export.signature);
        
        // Signature should be an empty string if it's NULL
        const char *send_signature = export.signature == NULL ? "" : export.signature;

        msg[msg_i++] = ERL_DRV_ATOM;
        msg[msg_i++] = driver_mk_atom((char *)wasm_import_export_kind_to_string(export.kind));
        msg[msg_i++] = ERL_DRV_STRING;
        msg[msg_i++] = (ErlDrvTermData)export.field_name;
        msg[msg_i++] = strlen(export.field_name);
        msg[msg_i++] = ERL_DRV_STRING;
        msg[msg_i++] = (ErlDrvTermData)send_signature;
        msg[msg_i++] = strlen(send_signature);
        msg[msg_i++] = ERL_DRV_TUPLE;
        msg[msg_i++] = 3;
    }

    msg[msg_i++] = ERL_DRV_NIL;
    msg[msg_i++] = ERL_DRV_LIST;
    msg[msg_i++] = export_count + 1;

    msg[msg_i++] = ERL_DRV_BUF2BINARY;
    msg[msg_i++] = (ErlDrvTermData)aot_module;
    msg[msg_i++] = aot_module_size;
    msg[msg_i++] = ERL_DRV_TUPLE;
    msg[msg_i++] = 4;
    
    DRV_DEBUG("Sending message (%p) of size %d to port (%p)", msg, msg_i, proc->port_term);
    int msg_res = erl_drv_output_term(proc->port_term, msg, msg_i);
    DRV_DEBUG("Read response sent: %d", msg_res);
}

static ErlDrvEntry compiler_driver_entry = {
    NULL,
    compiler_driver_start,
    compiler_driver_stop,
    compiler_driver_output,
    NULL,
    NULL,
    "hb_beamrc",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING,
    NULL,
    NULL,
    NULL
};

DRIVER_INIT(compiler_driver) {
    atom_ok = driver_mk_atom("ok");
    atom_error = driver_mk_atom("error");
    atom_compilation_result = driver_mk_atom("compilation_result");
    return &compiler_driver_entry;
}