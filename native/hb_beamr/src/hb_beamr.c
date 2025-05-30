#include "../include/hb_core.h"
#include "../include/hb_async.h"
#include "../include/hb_convert.h"
#include "hb_beamr_lib.h"
#include "hb_logging.h"
#include "hb_driver.h"

// Declare the atoms used in Erlang driver communication
ErlDrvTermData atom_ok;
ErlDrvTermData atom_error;
ErlDrvTermData atom_import;
ErlDrvTermData atom_execution_result;
ErlDrvTermData atom_pos_inf;
ErlDrvTermData atom_neg_inf;
ErlDrvTermData atom_nan;

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
    memset(proc, 0, sizeof(Proc));

    proc->is_running = erl_drv_mutex_create("wasm_instance_mutex");

    proc->port = port;
    DRV_DEBUG("Port: %p", proc->port);

    proc->port_key = driver_async_port_key(port);
    DRV_DEBUG("Port key: %d", proc->port_key);

    proc->port_term = driver_mk_port(proc->port);
    DRV_DEBUG("Port term: %p", proc->port_term);

    proc->start_time = time(NULL);
    DRV_DEBUG("Start time: %ld", proc->start_time);

    return (ErlDrvData)proc;
}

static void wasm_driver_stop(ErlDrvData raw) {
    // DRV_DEBUG("IGNORING STOP");
    Proc* proc = (Proc*)raw;
    DRV_DEBUG("Stopping WASM driver");

    // if(proc->current_import) {
    //     DRV_DEBUG("Shutting down during import response...");
    //     proc->current_import->error_message = "WASM driver unloaded during import response";
    //     proc->current_import->ready = 1;
    //     DRV_DEBUG("Signalling import_response with error");
    //     drv_signal(proc->current_import->response_ready, proc->current_import->cond, &proc->current_import->ready);
    //     DRV_DEBUG("Signalled worker to fail. Locking is_running mutex to shutdown");
    // }

    // We need to first grab the lock, then unlock it and destroy it. Must be a better way...
    DRV_DEBUG("Grabbing is_running mutex to shutdown...");
    drv_lock(proc->is_running);
    drv_unlock(proc->is_running);
    DRV_DEBUG("Destroying is_running mutex");
    erl_drv_mutex_destroy(proc->is_running);
    // Cleanup WASM resources
    DRV_DEBUG("Cleaning up WASM resources");
    if (proc->is_initialized) {
        // DRV_DEBUG("Deleting WASM instance");
        // wasm_instance_delete(proc->instance);
        // DRV_DEBUG("Deleted WASM instance");
        // wasm_module_delete(proc->module);
        // DRV_DEBUG("Deleted WASM module");
        // wasm_store_delete(proc->store);
        // DRV_DEBUG("Deleted WASM store");
    }
    DRV_DEBUG("Freeing proc");
    driver_free(proc);
    DRV_DEBUG("Freed proc");
}

// Declare helper functions
static void handle_init(Proc* proc, char* buff, ErlDrvSizeT bufflen, int start_index);
static void handle_call(Proc* proc, char* buff, ErlDrvSizeT bufflen, int start_index, enum call_type call_type);
static void handle_import_response(Proc* proc, char* buff, ErlDrvSizeT bufflen, int start_index);
static void handle_write(Proc* proc, char* buff, ErlDrvSizeT bufflen, int start_index);
static void handle_read(Proc* proc, char* buff, ErlDrvSizeT bufflen, int start_index);
static void handle_size(Proc* proc, char* buff, ErlDrvSizeT bufflen, int start_index);

static void wasm_driver_output(ErlDrvData raw, char *buff, ErlDrvSizeT bufflen) {
    DRV_DEBUG("WASM driver output received");
    Proc* proc = (Proc*)raw;
    DRV_TRACE("Port: %p", proc->port);
    DRV_TRACE("Port term: %p", proc->port_term);

    int index = 0;
    int version;
    if(ei_decode_version(buff, &index, &version) != 0) {
        send_error(proc->port_term, "Failed to decode message header (version).");
        return;
    }
    DRV_TRACE("Received term has version: %d", version);
    DRV_TRACE("Index: %d. buff_len: %d. buff: %p", index, bufflen, buff);
    int arity;
    ei_decode_tuple_header(buff, &index, &arity);
    DRV_TRACE("Term arity: %d", arity);

    char command[MAXATOMLEN];
    ei_decode_atom(buff, &index, command);
    DRV_TRACE("Port %p received command: %s, arity: %d", proc->port, command, arity);
    
    if (strcmp(command, "init") == 0) {
        handle_init(proc, buff, bufflen, index);
    } else if (strcmp(command, "call_export") == 0) {
        handle_call(proc, buff, bufflen, index, CALL_EXPORT);
    } else if (strcmp(command, "call_indirect") == 0) {
        handle_call(proc, buff, bufflen, index, CALL_INDIRECT);
    } else if (strcmp(command, "import_response") == 0) {
        handle_import_response(proc, buff, bufflen, index);
    } else if (strcmp(command, "write") == 0) {
        handle_write(proc, buff, bufflen, index);
    } else if (strcmp(command, "read") == 0) {
        handle_read(proc, buff, bufflen, index);
    } else if (strcmp(command, "size") == 0) {
        handle_size(proc, buff, bufflen, index);
    } else {
        DRV_DEBUG("Unknown command: %s", command);
        send_error(proc->port_term, "Unknown command");
    }
}

static void handle_init(Proc* proc, char* buff, ErlDrvSizeT bufflen, int index) {
// Start async initialization
    proc->pid = driver_caller(proc->port);
    DRV_TRACE("Caller PID: %d", proc->pid);
    int size, type, mode_size;
    char* mode;
    ei_get_type(buff, &index, &type, &size);
    DRV_TRACE("WASM binary size: %d bytes. Type: %c", size, type);
    void* wasm_binary = driver_alloc(size);
    long size_l = (long)size;
    ei_decode_binary(buff, &index, wasm_binary, &size_l);
    ei_get_type(buff, &index, &type, &mode_size);
    // the init message size + '\0' character
    mode = driver_alloc(mode_size + 1);
    ei_decode_atom(buff, &index, mode);

    InitHandlerReq* init_req = driver_alloc(sizeof(InitHandlerReq));
    init_req->proc = proc;
    init_req->mod_bin = wasm_binary;
    init_req->mod_size = size;
    init_req->run_mode = mode;
    
    driver_async(proc->port, NULL, wasm_initialize_runtime, init_req, NULL);
}

const char* DEFAULT_INDIRECT_TABLE_NAME = "__indirect_function_table";

static void handle_call(Proc* proc, char* buff, ErlDrvSizeT bufflen, int index, enum call_type call_type) {
    hb_beamr_lib_rc_t rc = HB_BEAMR_LIB_SUCCESS;

    if (!proc->is_initialized) {
        send_error(proc->port_term, "Cannot run WASM function as module not initialized.");
        return;
    }

    if (proc->call_stack_height >= MAX_CALL_STACK_DEPTH) {
        send_error(proc->port_term, "Call stack overflow. Max depth: %d", MAX_CALL_STACK_DEPTH);
        return;
    }

    CallRequest* cr = driver_alloc(sizeof(CallRequest));
    memset(cr, 0, sizeof(CallRequest));
    cr->call_type = call_type;

    hb_beamr_meta_func_t *func_meta = NULL;

    if (call_type == CALL_EXPORT) {
        // Extract the function name and the args from the Erlang term and generate the wasm_val_vec_t
        cr->call_export.function_name = driver_alloc(MAXATOMLEN);
        ei_decode_string(buff, &index, cr->call_export.function_name);
        DRV_DEBUG("Function name: %s", cr->call_export.function_name);

        rc = hb_beamr_lib_meta_export_func(proc->wasm_meta, cr->call_export.function_name, &func_meta);
    } else if (call_type == CALL_INDIRECT) {
        DRV_DEBUG("Decoding indirect call");
        long table_index;
        ei_decode_long(buff, &index, &table_index);
        DRV_DEBUG("Indirect function table index: %ld", table_index);

        cr->call_indirect.table_name = (char*)DEFAULT_INDIRECT_TABLE_NAME;

        rc = hb_beamr_lib_meta_indirect_func(proc->wasm_ctx, cr->call_indirect.table_name, table_index, &func_meta);
    }

    if (rc != HB_BEAMR_LIB_SUCCESS) {
        send_error(proc->port_term, "Failed to get function meta data: %d. %s", rc, hb_beamr_lib_get_last_error(proc->wasm_ctx));
        driver_free(cr);
        return;
    }

    DRV_DEBUG("Decoding args. Buff: %p. Index: %d", buff, index);
    wasm_val_t* args = NULL;
    enum erl_port_buffer_to_wasm_vals_rc erc = erl_port_buffer_to_wasm_vals(buff, &index, func_meta->param_types, func_meta->param_count, &args);
    if (erc != ERL_PORT_BUFFER_TO_WASM_VALS_SUCCESS) {
        if (erc == ERL_PORT_BUFFER_TO_WASM_VALS_VALUE_OUT_OF_RANGE) {
            send_error(proc->port_term, "Argument value out of range for wasm type");
        } else {
            send_error(proc->port_term, "Failed to decode arguments: %d", erc);
        }
        // driver_free(func_meta);
        driver_free(cr);
        return;
    }

    cr->args = args;
    cr->arg_count = func_meta->param_count;
    cr->result_types = func_meta->result_types;
    cr->result_count = func_meta->result_count;

    CallContext *cc = &proc->call_stack[proc->call_stack_height++];
    memcpy(&cc->call_request, cr, sizeof(CallRequest));
    driver_free(cr);

    // Initialize import response
    ImportState *is = &cc->import_state;
    memset(is, 0, sizeof(ImportState));
    char* response_mutex_name = driver_alloc(128);
    sprintf(response_mutex_name, "response_mutex_%d", proc->call_stack_height);
    char* response_cond_name = driver_alloc(128);
    sprintf(response_cond_name, "response_cond_%d", proc->call_stack_height);
    is->response_ready = erl_drv_mutex_create(response_mutex_name);
    is->cond = erl_drv_cond_create(response_cond_name);
    is->meta = NULL;
    is->ready = 0;
    driver_free(response_mutex_name);
    driver_free(response_cond_name);

    if (call_type == CALL_EXPORT) {
        driver_async(proc->port, NULL, wasm_execute_exported_function, proc, NULL);
    } else if (call_type == CALL_INDIRECT) {
        driver_async(proc->port, NULL, wasm_execute_indirect_function, proc, NULL);
    }
}

static void handle_import_response(Proc* proc, char* buff, ErlDrvSizeT bufflen, int index) {
    DRV_DEBUG("Import response received. Providing...");

    if (proc->call_stack_height == 0) {
        DRV_DEBUG("[error] No pending import response waiting");
        send_error(proc->port_term, "No pending import response waiting");
        return;
    }

    CallContext *cc = &proc->call_stack[proc->call_stack_height - 1];
    ImportState *is = &cc->import_state;

    if (is->meta == NULL) {
        DRV_DEBUG("Import response received but no import meta data available");
        send_error(proc->port_term, "Import response received but no import meta data available");
        return;
    }

    DRV_DEBUG("Decoding import response from Erlang...");
    wasm_val_t* results = NULL;
    enum erl_port_buffer_to_wasm_vals_rc erc = erl_port_buffer_to_wasm_vals(buff, &index, is->meta->func.result_types, is->meta->func.result_count, &results);
    if (erc != ERL_PORT_BUFFER_TO_WASM_VALS_SUCCESS) {
        send_error(proc->port_term, "Failed to decode import response");
        return;
    }

    is->results = results;
    is->result_count = is->meta->func.result_count;
    is->error_message = NULL;

    // Signal that the response is ready
    drv_signal(is->response_ready, is->cond, &is->ready);
}

static void handle_write(Proc* proc, char* buff, ErlDrvSizeT bufflen, int index) {
    DRV_DEBUG("Write received");
    long ptr;
    int type, size, arity;
    ei_decode_tuple_header(buff, &index, &arity);
    ei_decode_long(buff, &index, &ptr);
    ei_get_type(buff, &index, &type, &size);
    size_t size_l = (long)size;
    const char* write_data;
    int res = ei_decode_bitstring(buff, &index, &write_data, NULL, &size_l);
    DRV_DEBUG("Decoded binary. Res: %d. Size (bits): %ld", res, size_l);
    size_t size_bytes = size_l / 8;
    DRV_DEBUG("Write received. Ptr: %ld. Bytes: %ld", ptr, size_bytes);
    hb_beamr_lib_rc_t rc = hb_beamr_lib_direct_write_memory(proc->wasm_ctx, ptr, (uint8_t*)write_data, size_bytes);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        send_error(proc->port_term, hb_beamr_lib_get_last_error(proc->wasm_ctx));
        return;
    }

    ErlDrvTermData* msg = driver_alloc(sizeof(ErlDrvTermData) * 2);
    msg[0] = ERL_DRV_ATOM;
    msg[1] = atom_ok;
    int msg_res = erl_drv_output_term(proc->port_term, msg, 2);
    DRV_DEBUG("Write response sent: %d", msg_res);
}

static void handle_read(Proc* proc, char* buff, ErlDrvSizeT bufflen, int index) {
    DRV_DEBUG("Read received");
    int arity;
    long ptr, size;
    ei_decode_tuple_header(buff, &index, &arity);
    ei_decode_long(buff, &index, &ptr);
    ei_decode_long(buff, &index, &size);
    long size_l = (long)size;
    
    uint8_t* out_binary = driver_alloc(size_l);
    hb_beamr_lib_rc_t rc = hb_beamr_lib_direct_read_memory(proc->wasm_ctx, ptr, (uint8_t*)out_binary, size_l);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        send_error(proc->port_term, hb_beamr_lib_get_last_error(proc->wasm_ctx));
        return;
    }

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

static void handle_size(Proc* proc, char* buff, ErlDrvSizeT bufflen, int start_index) {
    DRV_DEBUG("Size received");
    uint8_t* memory_data;
    size_t memory_size;
    wasm_memory_inst_t memory;
    hb_beamr_lib_rc_t rc = hb_beamr_lib_get_memory_info(proc->wasm_ctx, NULL, &memory_data, &memory_size, &memory);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        send_error(proc->port_term, hb_beamr_lib_get_last_error(proc->wasm_ctx));
        return;
    }
    DRV_DEBUG("Size: %ld", memory_size);

    ErlDrvTermData* msg = driver_alloc(sizeof(ErlDrvTermData) * 6);
    int msg_index = 0;
    msg[msg_index++] = ERL_DRV_ATOM;
    msg[msg_index++] = atom_execution_result;
    msg[msg_index++] = ERL_DRV_INT;
    msg[msg_index++] = (long)memory_size;
    msg[msg_index++] = ERL_DRV_TUPLE;
    msg[msg_index++] = 2;
    int msg_res = erl_drv_output_term(proc->port_term, msg, msg_index);
    DRV_DEBUG("Size response sent: %d", msg_res);
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
    atom_pos_inf = driver_mk_atom("+inf");
    atom_neg_inf = driver_mk_atom("-inf");
    atom_nan = driver_mk_atom("nan");
    return &wasm_driver_entry;
}