#include "include/hb_helpers.h"
#include "include/hb_logging.h"
#include "include/hb_driver.h"
#include "include/hb_wasm.h"

// Declare the atoms used in Erlang driver communication
ErlDrvTermData atom_ok;
ErlDrvTermData atom_error;
ErlDrvTermData atom_import;
ErlDrvTermData atom_execution_result;

static ErlDrvData wasm_driver_start(ErlDrvPort port, char *buff) {
    ErlDrvSysInfo info;
    driver_system_info(&info, sizeof(info));
    DRV_DEBUG("Starting WASM driver");
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
    proc->port = port;
    DRV_DEBUG("Port: %p", proc->port);
    proc->port_term = driver_mk_port(proc->port);
    DRV_DEBUG("Port term: %p", proc->port_term);
    proc->is_running = erl_drv_mutex_create("wasm_instance_mutex");
    proc->is_initialized = 0;
    proc->start_time = time(NULL);
    return (ErlDrvData)proc;
}

static void wasm_driver_stop(ErlDrvData raw) {
    Proc* proc = (Proc*)raw;
    DRV_DEBUG("Stopping WASM driver");

    if(proc->current_import) {
        DRV_DEBUG("Shutting down during import response...");
        proc->current_import->error_message = "WASM driver unloaded during import response";
        proc->current_import->ready = 1;
        DRV_DEBUG("Signalling import_response with error");
        drv_signal(proc->current_import->response_ready, proc->current_import->cond, &proc->current_import->ready);
        DRV_DEBUG("Signalled worker to fail. Locking is_running mutex to shutdown");
    }

    // We need to first grab the lock, then unlock it and destroy it. Must be a better way...
    DRV_DEBUG("Grabbing is_running mutex to shutdown...");
    drv_lock(proc->is_running);
    drv_unlock(proc->is_running);
    DRV_DEBUG("Destroying is_running mutex");
    erl_drv_mutex_destroy(proc->is_running);
    // Cleanup WASM resources
    DRV_DEBUG("Cleaning up WASM resources");
    if (proc->is_initialized) {
        DRV_DEBUG("Deleting WASM instance");
        wasm_instance_delete(proc->instance);
        DRV_DEBUG("Deleted WASM instance");
        wasm_module_delete(proc->module);
        DRV_DEBUG("Deleted WASM module");
        wasm_store_delete(proc->store);
        DRV_DEBUG("Deleted WASM store");
    }
    DRV_DEBUG("Freeing proc");
    driver_free(proc);
    DRV_DEBUG("Freed proc");
}

static void wasm_driver_output(ErlDrvData raw, char *buff, ErlDrvSizeT bufflen) {
    DRV_DEBUG("WASM driver output received");
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
    
    if (strcmp(command, "init") == 0) {
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
        LoadWasmReq* mod_bin = driver_alloc(sizeof(LoadWasmReq));
        mod_bin->proc = proc;
        mod_bin->binary = wasm_binary;
        mod_bin->size = size;
        mod_bin->mode = mode;
        //DRV_DEBUG("Calling for async thread to init");
        driver_async(proc->port, NULL, wasm_initialize_runtime, mod_bin, NULL);
    } else if (strcmp(command, "call") == 0) {
        if (!proc->is_initialized) {
            send_error(proc, "Cannot run WASM function as module not initialized.");
            return;
        }
        // Extract the function name and the args from the Erlang term and generate the wasm_val_vec_t
        char* function_name = driver_alloc(MAXATOMLEN);
        ei_decode_string(buff, &index, function_name);
        DRV_DEBUG("Function name: %s", function_name);
        proc->current_function = function_name;

        DRV_DEBUG("Decoding args. Buff: %p. Index: %d", buff, index);
        proc->current_args = decode_list(buff, &index);

        driver_async(proc->port, NULL, wasm_execute_function, proc, NULL);
    } 
    // else if (strcmp(command, "indirect_call") == 0) {
    //     if (!proc->is_initialized) {
    //         send_error(proc, "Cannot run WASM indirect function as module not initialized.");
    //         return;
    //     }
    //     DRV_DEBUG("Decoding indirect call");
    //     ei_decode_long(buff, &index, &proc->current_function_ix);
    //     proc->current_args = decode_list(buff, &index);
	//     DRV_DEBUG("Calling indirect call invoker");
    //      driver_async(proc->port, NULL, async_indirect_call, proc, NULL);
    // } 
    else if (strcmp(command, "import_response") == 0) {
        // Handle import response
        // TODO: We should probably start a mutex on the current_import object here.
        // At the moment current_import->response_ready must not be locked so that signalling can happen.
        DRV_DEBUG("Import response received. Providing...");
        if (proc->current_import) {
            DRV_DEBUG("Decoding import response from Erlang...");
            proc->current_import->result_terms = decode_list(buff, &index);
            proc->current_import->error_message = NULL;

            // Signal that the response is ready
            drv_signal(
                proc->current_import->response_ready,
                proc->current_import->cond,
                &proc->current_import->ready);
        } else {
            DRV_DEBUG("[error] No pending import response waiting");
            send_error(proc, "No pending import response waiting");
        }
    } else if (strcmp(command, "write") == 0) {
        DRV_DEBUG("Write received");
        long ptr, size;
        int type;
        ei_decode_tuple_header(buff, &index, &arity);
        ei_decode_long(buff, &index, &ptr);
        ei_get_type(buff, &index, &type, &size);
        long size_l = (long)size;
        char* wasm_binary;
        int res = ei_decode_bitstring(buff, &index, &wasm_binary, NULL, &size_l);
        DRV_DEBUG("Decoded binary. Res: %d. Size (bits): %ld", res, size_l);
        long size_bytes = size_l / 8;
        DRV_DEBUG("Write received. Ptr: %ld. Bytes: %ld", ptr, size_bytes);
        long memory_size = get_memory_size(proc);
        if(ptr + size_bytes > memory_size) {
            DRV_DEBUG("Write request out of bounds.");
            send_error(proc, "Write request out of bounds");
            return;
        }
        byte_t* memory_data = wasm_memory_data(get_memory(proc));
        DRV_DEBUG("Memory location to write to: %p", ptr+memory_data);

        memcpy(memory_data + ptr, wasm_binary, size_bytes);
        DRV_DEBUG("Write complete");

        ErlDrvTermData* msg = driver_alloc(sizeof(ErlDrvTermData) * 2);
        msg[0] = ERL_DRV_ATOM;
        msg[1] = atom_ok;
        erl_drv_output_term(proc->port_term, msg, 2);
    }
    else if (strcmp(command, "read") == 0) {
        DRV_DEBUG("Read received");
        long ptr, size;
        ei_decode_tuple_header(buff, &index, &arity);
        ei_decode_long(buff, &index, &ptr);
        ei_decode_long(buff, &index, &size);
        long size_l = (long)size;
        long memory_size = get_memory_size(proc);
        DRV_DEBUG("Read received. Ptr: %ld. Size: %ld. Memory size: %ld", ptr, size_l, memory_size);
        if(ptr + size_l > memory_size) {
            DRV_DEBUG("Read request out of bounds.");
            send_error(proc, "Read request out of bounds");
            return;
        }
        byte_t* memory_data = wasm_memory_data(get_memory(proc));
        DRV_DEBUG("Memory location to read from: %p", memory_data + ptr);
        
        char* out_binary = driver_alloc(size_l);
        memcpy(out_binary, memory_data + ptr, size_l);

        DRV_DEBUG("Read complete. Binary: %p", out_binary);

        ErlDrvTermData* msg = driver_alloc(sizeof(ErlDrvTermData) * 7);
        int msg_index = 0;
        msg[msg_index++] = ERL_DRV_ATOM;
        msg[msg_index++] = atom_execution_result;
        msg[msg_index++] = ERL_DRV_BUF2BINARY;
        msg[msg_index++] = (ErlDrvTermData)out_binary;
        msg[msg_index++] = size_l;
        msg[msg_index++] = ERL_DRV_TUPLE;
        msg[msg_index++] = 2;
        
        int msg_res = erl_drv_output_term(proc->port_term, msg, msg_index);
        DRV_DEBUG("Read response sent: %d", msg_res);
    }
    else if (strcmp(command, "size") == 0) {
        DRV_DEBUG("Size received");
        long size = get_memory_size(proc);
        DRV_DEBUG("Size: %ld", size);

        ErlDrvTermData* msg = driver_alloc(sizeof(ErlDrvTermData) * 6);
        int msg_index = 0;
        msg[msg_index++] = ERL_DRV_ATOM;
        msg[msg_index++] = atom_execution_result;
        msg[msg_index++] = ERL_DRV_INT;
        msg[msg_index++] = size;
        msg[msg_index++] = ERL_DRV_TUPLE;
        msg[msg_index++] = 2;
        erl_drv_output_term(proc->port_term, msg, msg_index);
    }
    else {
        DRV_DEBUG("Unknown command: %s", command);
        send_error(proc, "Unknown command");
    }
}

static ErlDrvEntry wasm_driver_entry = {
    NULL,
    wasm_driver_start,
    wasm_driver_stop,
    wasm_driver_output,
    NULL,
    NULL,
    "hb_beamr",
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

DRIVER_INIT(wasm_driver) {
    atom_ok = driver_mk_atom("ok");
    atom_error = driver_mk_atom("error");
    atom_import = driver_mk_atom("import");
    atom_execution_result = driver_mk_atom("execution_result");
    return &wasm_driver_entry;
}