#include "../include/hb_core.h"
#include "../include/hb_async.h"
#include "../include/hb_convert.h"
#include "hb_logging.h"
#include "hb_driver.h"
#include "hb_beamr_lib.h"
#include "hb_beamr_utils.h"
#include "wasm_export.h"

extern ErlDrvTermData atom_ok;
extern ErlDrvTermData atom_import;
extern ErlDrvTermData atom_execution_result;

static void generic_import_native_symbol_func(wasm_exec_env_t exec_env, uint64_t *args) {
    DRV_DEBUG("generic_import_native_symbol_func called, exec_env: %p, args: %p", exec_env, args);

    wasm_module_inst_t module_inst = wasm_runtime_get_module_inst(exec_env);
    DRV_DEBUG("Module instance: %p", module_inst);

    Proc *proc = wasm_runtime_get_custom_data(module_inst);
    DRV_DEBUG("Proc: %p", proc);

    // We already have the lock from the execute export/indirect function call
    // drv_lock(proc->is_running);

    hb_beamr_meta_import_t *attachment = (hb_beamr_meta_import_t *)wasm_runtime_get_function_attachment(exec_env);
    DRV_DEBUG("Attachment (%p)", attachment);
    if (!attachment) {
        DRV_DEBUG("Skipping import: No attachment found");
        return;
    }

    const char *module_name = attachment->module_name;
    const char *func_name = attachment->field_name;
    const char *signature = attachment->func.signature;
    DRV_DEBUG("Calling import %s.%s [%s]", module_name, func_name, signature);

    uint32_t param_count = attachment->func.param_count;
    wasm_valkind_t *param_kinds = attachment->func.param_types;
    uint32_t result_count = attachment->func.result_count;
    wasm_valkind_t *result_kinds = attachment->func.result_types;

#ifdef HB_DEBUG
    DRV_DEBUG("Import param types:");
    hb_beamr_utils_print_wasm_val_kinds(param_kinds, param_count);
    DRV_DEBUG("Import result types:");
    hb_beamr_utils_print_wasm_val_kinds(result_kinds, result_count);
#endif

    wasm_val_t* wasm_args = NULL;
    hb_beamr_lib_rc_t rc = hb_beamr_lib_convert_raw_args_to_wasm_vals(args, param_kinds, param_count, &wasm_args);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        DRV_DEBUG("Failed to convert raw args to wasm vals");
        return;
    }

#ifdef HB_DEBUG
    DRV_DEBUG("Wasm vals:");
    hb_beamr_utils_print_wasm_vals(wasm_args, param_count);
#endif

    // Initialize the message object
    int base_msg_size = sizeof(ErlDrvTermData) * (
        2 + // atom_import
        3 + // module_name
        3 + // func_name
        // params will be added by wasm_vals_to_erl_msg
        3 + // signature
        2 // tuple
        + 10 // good luck!
    );
    ErlDrvTermData* msg = driver_alloc(base_msg_size);
    int msg_i = 0;
    msg[msg_i++] = ERL_DRV_ATOM;
    msg[msg_i++] = atom_import;
    msg[msg_i++] = ERL_DRV_STRING;
    msg[msg_i++] = (ErlDrvTermData) module_name;
    msg[msg_i++] = strlen(module_name);
    msg[msg_i++] = ERL_DRV_STRING;
    msg[msg_i++] = (ErlDrvTermData) func_name;
    msg[msg_i++] = strlen(func_name);

    // Encode args
    int erc = wasm_vals_to_erl_msg(wasm_args, param_count, &msg, &msg_i, base_msg_size);
    if (erc != 0) {
        DRV_DEBUG("Failed to encode args to message");
        return;
    }

    // Encode function signature
    msg[msg_i++] = ERL_DRV_STRING;
    msg[msg_i++] = (ErlDrvTermData) signature;
    msg[msg_i++] = strlen(signature);

    // Prepare the message to send to the Erlang side
    msg[msg_i++] = ERL_DRV_TUPLE;
    msg[msg_i++] = 5;

    // Initialize the result vector and set the required result types
    CallContext *cc = &proc->call_stack[proc->call_stack_height - 1];
    ImportState *is = &cc->import_state;

    is->meta = attachment;
    is->ready = 0;

    // Send the message to the caller process
    int msg_res = erl_drv_output_term(proc->port_term, msg, msg_i);
    DRV_DEBUG("Call to erl_drv_output_term completed. Res: %d", msg_res);

    // Wait for the response (we set this directly after the message was sent
    // so we have the lock, before Erlang sends us data back)
    DRV_DEBUG("Waiting for import response");
    drv_unlock(proc->is_running);
    drv_wait(is->response_ready, is->cond, &is->ready);
    drv_lock(proc->is_running);
    DRV_DEBUG("Import response ready");

    // Handle error in the response
    if (is->error_message) {
        DRV_DEBUG("Import execution failed. Error message: %s", is->error_message);
        send_error(proc->port_term, "Import execution failed: %s", is->error_message);
        driver_free(is->error_message);
        return;
    }

#ifdef HB_DEBUG
    DRV_DEBUG("Import results:");
    hb_beamr_utils_print_wasm_vals(is->results, is->result_count);
#endif

    // assert(is->result_count == result_count);
    rc = hb_beamr_lib_convert_wasm_vals_to_raw_args(is->results, is->result_count, args);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        DRV_DEBUG("Failed to convert wasm vals to raw args: %d", rc);
        send_error(proc->port_term, "Failed to convert wasm vals to raw args: %d", rc);
        return;
    }

    DRV_DEBUG("Converted import results to raw args");

    // Cleanup any variables we allocated
    if (wasm_args) {
        free(wasm_args);
    }
    if (is->error_message) {
        driver_free(is->error_message);
    }
    if (is->results) {
        driver_free(is->results);
    }

    // Reset the import state
    is->ready = 0;
    is->error_message = NULL;
    is->results = NULL;
    is->result_count = 0;
    // These ones we reuse
    // is->response_ready = NULL;
    // is->cond = NULL;

    DRV_DEBUG("generic_import_native_symbol_func completed (%s.%s)", module_name, func_name);
}

void ensure_thread_init() {
    if (!wasm_runtime_thread_env_inited()) {
        DRV_DEBUG("Thread environment: initializing...");
        wasm_runtime_init_thread_env();
        DRV_DEBUG("Thread environment: initialized!");
    } else {
        DRV_DEBUG("Thread environment: already initialized");
    }
}

void wasm_initialize_runtime(void* raw) {
    DRV_DEBUG("wasm_initialize_runtime called, raw: %p", raw);

    InitHandlerReq* init_req = (InitHandlerReq*)raw;

    Proc* proc = init_req->proc;
    drv_lock(proc->is_running);

    ensure_thread_init();

#if HB_DEBUG==1
    wasm_runtime_set_log_level(WASM_LOG_LEVEL_VERBOSE);
#else
    wasm_runtime_set_log_level(WASM_LOG_LEVEL_ERROR);
#endif

    DRV_DEBUG("Mode: %s", init_req->run_mode);

    // if(strcmp(init_req->mode, "wasm") == 0) {
    //     DRV_DEBUG("Using WASM mode.");
    //     wasm_runtime_set_default_running_mode(Mode_Interp);
    // } else {
    //     DRV_DEBUG("Using AOT mode.");
    // }

    // create a temp module, just for meta data
    hb_beamr_lib_rc_t rc;
    char *error_buffer = driver_alloc(1024);

    rc = hb_beamr_lib_init_runtime_global(NULL);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        send_error(proc->port_term, "Failed to initialize runtime global");
        drv_unlock(proc->is_running);
        return;
    }

    uint8_t* wasm_copy = driver_alloc(init_req->mod_size);
    memcpy(wasm_copy, init_req->mod_bin, init_req->mod_size);

    DRV_DEBUG("Loading WASM module %p @ %d from %p @ %d", wasm_copy, init_req->mod_size, init_req->mod_bin, init_req->mod_size);
    wasm_module_t tmp_module = wasm_runtime_load(wasm_copy, init_req->mod_size, error_buffer, 1024);
    if (tmp_module == NULL) {
        send_error(proc->port_term, "Failed to load temp WASM module: %s", error_buffer);
        drv_unlock(proc->is_running);
        return;
    }
    
    hb_beamr_meta_module_t *meta = driver_alloc(sizeof(hb_beamr_meta_module_t));
    rc = hb_beamr_lib_meta_module(tmp_module, meta);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        send_error(proc->port_term, "Failed to get meta module: %d", rc);
        drv_unlock(proc->is_running);
        return;
    }
    wasm_runtime_unload(tmp_module);

    // generate natives
    hb_beamr_native_symbols_structured_t structured_natives;
    memset(&structured_natives,0,sizeof(structured_natives));
    rc = hb_beamr_lib_generate_natives(meta, generic_import_native_symbol_func,&structured_natives);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        send_error(proc->port_term, "Failed to generate natives");
        drv_unlock(proc->is_running);
        return;
    }

    rc = hb_beamr_lib_register_global_natives(&structured_natives);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        send_error(proc->port_term, "Failed to register natives");
        drv_unlock(proc->is_running);
        return;
    }

    // create a context
    hb_beamr_lib_context_t *wasm_ctx = hb_beamr_lib_create_context();
    rc = hb_beamr_lib_load_wasm_module(wasm_ctx, init_req->mod_bin, init_req->mod_size);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        send_error(proc->port_term, "Failed to load WASM module: %s", hb_beamr_lib_get_last_error(wasm_ctx));
        drv_unlock(proc->is_running);
        return;
    }

    rc = hb_beamr_lib_instantiate(wasm_ctx, 256 * 1024, 1 * 1024 * 1024, proc);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        send_error(proc->port_term, "Failed to instantiate WASM module");
        drv_unlock(proc->is_running);
        return;
    }

    // Generate the erlang message
    int init_msg_size = sizeof(ErlDrvTermData) * 2;
    ErlDrvTermData* msg = driver_alloc(init_msg_size);
    int msg_i = 0;

    msg[msg_i++] = ERL_DRV_ATOM;
    msg[msg_i++] = atom_execution_result;
    msg[msg_i++] = ERL_DRV_TUPLE;
    msg[msg_i++] = 1;

    int send_res = erl_drv_output_term(proc->port_term, msg, msg_i);
    DRV_DEBUG("Send result: %d", send_res);

    // Set proc
    proc->wasm_ctx = wasm_ctx;
    proc->wasm_meta = meta;
    proc->is_initialized = true;

    drv_unlock(proc->is_running);
}

void wasm_execute_exported_function(void* raw) {
    DRV_DEBUG("wasm_execute_exported_function called, raw: %p", raw);

    Proc* proc = (Proc*)raw;
    drv_lock(proc->is_running);

    ensure_thread_init();

    DRV_DEBUG("wasm_execute_exported_function: proc: %p, call_stack_height: %d", proc, proc->call_stack_height);

    CallContext *cc = &proc->call_stack[proc->call_stack_height - 1];
    // assert(cc->call_request.call_type == CALL_EXPORT);
    
    hb_beamr_lib_context_t *wasm_ctx = proc->wasm_ctx;
    hb_beamr_meta_module_t *wasm_meta = proc->wasm_meta;

    wasm_val_t* results = driver_alloc(cc->call_request.result_count * sizeof(wasm_val_t));

#ifdef HB_DEBUG
    DRV_DEBUG("Calling export: %s", cc->call_request.call_export.function_name);
    hb_beamr_utils_print_wasm_vals(cc->call_request.args, cc->call_request.arg_count);
    hb_beamr_utils_print_wasm_val_kinds(cc->call_request.result_types, cc->call_request.result_count);
#endif

    hb_beamr_lib_rc_t rc = hb_beamr_lib_call_export(wasm_ctx, cc->call_request.call_export.function_name, cc->call_request.arg_count, cc->call_request.args, cc->call_request.result_count, results);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        send_error(proc->port_term, "Failed to call exported function: %s", hb_beamr_lib_get_last_error(wasm_ctx));
        drv_unlock(proc->is_running);
        return;
    }
    
    ErlDrvTermData* msg = driver_alloc(sizeof(ErlDrvTermData) * 4);
    int msg_i = 0;
    msg[msg_i++] = ERL_DRV_ATOM;
    msg[msg_i++] = atom_execution_result;
    if (wasm_vals_to_erl_msg(results, cc->call_request.result_count, &msg, &msg_i, 4) != 0) {
        send_error(proc->port_term, "Failed to convert results to erl msg");
        drv_unlock(proc->is_running);
        return;
    };
    msg[msg_i++] = ERL_DRV_TUPLE;
    msg[msg_i++] = 2;

    int send_res = erl_drv_output_term(proc->port_term, msg, msg_i);
    DRV_DEBUG("Send result: %d", send_res);

fail1:
    memset(&proc->call_stack[--proc->call_stack_height], 0, sizeof(CallContext));

fail0:
	DRV_DEBUG("Unlocking is_running mutex: %p", proc->is_running);
    drv_unlock(proc->is_running);
}

void wasm_execute_indirect_function(void *raw) {
    DRV_DEBUG("wasm_execute_indirect_function called, raw: %p", raw);

    Proc* proc = (Proc*)raw;
    drv_lock(proc->is_running);

    ensure_thread_init();

    CallContext *cc = &proc->call_stack[proc->call_stack_height - 1];
    // assert(cc->call_request.call_type == CALL_INDIRECT);

    DRV_DEBUG("Indirect function table name: %s", cc->call_request.call_indirect.table_name);
    DRV_DEBUG("Indirect function table index: %ld", cc->call_request.call_indirect.table_index);
    DRV_DEBUG("Indirect function args: %p", cc->call_request.args);

    hb_beamr_lib_context_t *wasm_ctx = proc->wasm_ctx;
    hb_beamr_meta_module_t *wasm_meta = proc->wasm_meta;

    hb_beamr_lib_rc_t rc;

    wasm_val_t* results = driver_alloc(cc->call_request.result_count * sizeof(wasm_val_t));

#ifdef HB_DEBUG
    DRV_DEBUG("Calling indirect table: %s, index: %ld", cc->call_request.call_indirect.table_name, cc->call_request.call_indirect.table_index);
    hb_beamr_utils_print_wasm_vals(cc->call_request.args, cc->call_request.arg_count);
    hb_beamr_utils_print_wasm_val_kinds(cc->call_request.result_types, cc->call_request.result_count);
#endif

    rc = hb_beamr_lib_call_indirect(wasm_ctx, cc->call_request.call_indirect.table_name, cc->call_request.call_indirect.table_index, cc->call_request.arg_count, cc->call_request.args, cc->call_request.result_count, results);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        send_error(proc->port_term, "Failed to call indirect function");
        drv_unlock(proc->is_running);
        return;
    }

fail1:
    memset(&proc->call_stack[--proc->call_stack_height], 0, sizeof(CallContext));

fail0:
    DRV_DEBUG("Indirect function call complete");
    drv_unlock(proc->is_running);
    return;
}
