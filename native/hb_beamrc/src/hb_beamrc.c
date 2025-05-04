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
        send_error(proc, "Failed to decode message header (version).");
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
        send_error(proc, "Unknown command");
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

    size_t aot_module_size;
    uint8_t* aot_module;
    int res = hb_wasm_aot_compile(&compile_opts, mod_bin->binary, mod_bin->size, &aot_module, &aot_module_size);
    DRV_DEBUG("AOT module size: %d bytes", aot_module_size);
    
    if (res == 0) {
        DRV_DEBUG("Compilation successful");
        send_compilation_result(proc, aot_module, aot_module_size);
    }
    else {
        DRV_DEBUG("Compilation failed");
        send_error(proc, "Compilation failed");
    }

    drv_unlock(proc->is_running);
}

void send_compilation_result(Proc* proc, uint8_t* aot_module, uint32_t aot_module_size) {
    DRV_DEBUG("Sending compilation result");
    DRV_DEBUG("AOT module: %p", aot_module);
    DRV_DEBUG("AOT module size: %d bytes", aot_module_size);

    // Send the compilation result to the caller
    ErlDrvTermData* msg = driver_alloc(sizeof(ErlDrvTermData) * 7);
    int msg_index = 0;
    msg[msg_index++] = ERL_DRV_ATOM;
    msg[msg_index++] = atom_compilation_result;
    msg[msg_index++] = ERL_DRV_BUF2BINARY;
    msg[msg_index++] = (ErlDrvTermData)aot_module;
    msg[msg_index++] = aot_module_size;
    msg[msg_index++] = ERL_DRV_TUPLE;
    msg[msg_index++] = 2;
    
    int msg_res = erl_drv_output_term(proc->port_term, msg, msg_index);
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