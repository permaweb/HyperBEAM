#include "../include/hb_core.h"
#include "../include/hb_async.h"
#include "../include/hb_convert.h"
#include "hb_logging.h"
#include "hb_driver.h"
#include "hb_beamr_lib.h"
#include "hb_beamr_utils.h"

extern ErlDrvTermData atom_ok;
extern ErlDrvTermData atom_import;
extern ErlDrvTermData atom_execution_result;

static void generic_import_native_symbol_func(wasm_exec_env_t exec_env, uint64_t *args) {
//     DRV_DEBUG("generic_import_native_symbol_func called, exec_env: %p, args: %p", exec_env, args);
//     wasm_module_inst_t module_inst = wasm_runtime_get_module_inst(exec_env);
//     DRV_DEBUG("Module instance: %p", module_inst);

//     NativeSymbolAttachment *attachment = (NativeSymbolAttachment *)wasm_runtime_get_function_attachment(exec_env);
//     DRV_DEBUG("Attachment (%p)", attachment);
//     if (!attachment) {
//         DRV_DEBUG("Skipping import: No attachment found");
//         return;
//     }

//     if (attachment->proc->import_stack_depth >= MAX_IMPORT_STACK_DEPTH) {
//         DRV_DEBUG("Skipping import: Import stack depth exceeded");
//         return;
//     }

//     Proc *proc = attachment->proc;
//     const char *module_name = attachment->module_name;
//     const char *func_name = attachment->field_name;
//     const char *signature = attachment->signature;
//     DRV_DEBUG("Calling import %s.%s [%s]", module_name, func_name, signature);

//     uint32_t param_count = attachment->param_count;
//     wasm_valkind_t *param_kinds = attachment->param_kinds;
//     uint32_t result_count = attachment->result_count;
//     wasm_valkind_t *result_kinds = attachment->result_kinds;
//     DRV_DEBUG("Param count: %d", param_count);
//     DRV_DEBUG("Result count: %d", result_count);
//     // int param_size = kinds_size(param_kinds, param_count);
//     // int result_size = kinds_size(result_kinds, result_count);
//     // DRV_DEBUG("Param size: %d", param_size);
//     // DRV_DEBUG("Result size: %d", result_size);

//     // Initialize the message object
//     int msg_size = sizeof(ErlDrvTermData) * ((2+(2*3)) + ((param_count + 1) * 2) + ((result_count + 1) * 2) + 2);
//     DRV_DEBUG("Message size: %d", msg_size);
//     ErlDrvTermData* msg = driver_alloc(msg_size);
//     int msg_index = 0;
//     msg[msg_index++] = ERL_DRV_ATOM;
//     msg[msg_index++] = atom_import;
//     msg[msg_index++] = ERL_DRV_STRING;
//     msg[msg_index++] = (ErlDrvTermData) module_name;
//     msg[msg_index++] = strlen(module_name);
//     msg[msg_index++] = ERL_DRV_STRING;
//     msg[msg_index++] = (ErlDrvTermData) func_name;
//     msg[msg_index++] = strlen(func_name);

//     // Encode args
//     for (size_t i = 0; i < param_count; i++) {
//         msg_index += import_arg_to_erl_term(&msg[msg_index], param_kinds[i], &args[i]);
//     }
//     msg[msg_index++] = ERL_DRV_NIL;
//     msg[msg_index++] = ERL_DRV_LIST;
//     msg[msg_index++] = param_count + 1;

//     // Encode function signature
//     msg[msg_index++] = ERL_DRV_STRING;
//     msg[msg_index++] = (ErlDrvTermData) signature;
//     msg[msg_index++] = strlen(signature);

//     // Prepare the message to send to the Erlang side
//     msg[msg_index++] = ERL_DRV_TUPLE;
//     msg[msg_index++] = 5;

//     // Initialize the result vector and set the required result types
//     ImportResponse* this_import = driver_alloc(sizeof(ImportResponse));
//     proc->current_import = this_import;
//     proc->import_stack[proc->import_stack_depth++] = this_import;
//     DRV_DEBUG("import_stack: Import stack depth: %d", proc->import_stack_depth);

//     // Create and initialize a is_running and condition variable for the response
//     char* response_mutex_name = driver_alloc(128);
//     sprintf(response_mutex_name, "response_mutex_%d", proc->import_stack_depth);
//     char* response_cond_name = driver_alloc(128);
//     sprintf(response_cond_name, "response_cond_%d", proc->import_stack_depth);
    
//     this_import->response_ready = erl_drv_mutex_create(response_mutex_name);
//     this_import->cond = erl_drv_cond_create(response_cond_name);
//     this_import->ready = 0;

//     DRV_DEBUG("Sending %d terms...", msg_index);
//     DRV_DEBUG("Pre-send state: proc=%p, port=%lu, msg=%p, msg_index=%d", proc, proc->port_term, msg, msg_index);
//     DRV_DEBUG("  module_name (%p): '%s'", module_name, module_name ? module_name : "(null)");
//     DRV_DEBUG("  func_name (%p): '%s'", func_name, func_name ? func_name : "(null)");
//     DRV_DEBUG("  signature (%p): '%s' (len=%zu)", signature, signature ? signature : "(null)", signature ? strlen(signature) : 0);
//     DRV_DEBUG("About to call erl_drv_output_term");
//     // Send the message to the caller process
//     int msg_res = erl_drv_output_term(proc->port_term, msg, msg_index);
//     DRV_DEBUG("Call to erl_drv_output_term completed. Res: %d", msg_res);
//     DRV_DEBUG("Message sent. Res: %d", msg_res);

//     // Wait for the response (we set this directly after the message was sent
//     // so we have the lock, before Erlang sends us data back)
//     DRV_DEBUG("Waiting for response");
//     drv_unlock(proc->is_running);
//     drv_wait(this_import->response_ready, this_import->cond, &this_import->ready);
//     drv_lock(proc->is_running);
//     DRV_DEBUG("Response ready");

//     // Handle error in the response
//     if (this_import->error_message) {
//         DRV_DEBUG("Import execution failed. Error message: %s", proc->current_import->error_message);
//         send_error(proc->port_term, "Import execution failed: %s", proc->current_import->error_message);
//         return;
//     }

//     // Convert the response back to the function result, writing back to the args pointer
//     int res = erl_terms_to_import_results(result_count, result_kinds, args, this_import->result_terms);
//     if(res == -1) {
//         DRV_DEBUG("Failed to convert terms to wasm vals");
//         send_error(proc->port_term, "Failed to convert terms to wasm vals");
//         return;
//     }

//     // Clean up
//     DRV_DEBUG("Destroying %s", erl_drv_cond_name(this_import->cond));
//     erl_drv_cond_destroy(this_import->cond);
//     DRV_DEBUG("Destroying %s", erl_drv_mutex_name(this_import->response_ready));
//     erl_drv_mutex_destroy(this_import->response_ready);
//     DRV_DEBUG("Cond and mutex destroyed");

//     // DRV_DEBUG("Cleaning up this_import (%p)", this_import);
//     // driver_free(this_import);

//     DRV_DEBUG("generic_import_native_symbol_func completed (%s.%s)", module_name, func_name);
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

    DRV_DEBUG("Mode: %s", init_req->mode);

    // if(strcmp(init_req->mode, "wasm") == 0) {
    //     DRV_DEBUG("Using WASM mode.");
    //     wasm_runtime_set_default_running_mode(Mode_Interp);
    // } else {
    //     DRV_DEBUG("Using AOT mode.");
    // }

    // create a temp module, just for meta data
    hb_beamr_lib_rc_t rc;
    char *error_buffer = malloc(1024);

    rc = hb_beamr_lib_init_runtime_global(NULL);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        send_error(proc->port_term, "Failed to initialize runtime global");
        drv_unlock(proc->is_running);
        return;
    }

    uint8_t* wasm_copy = malloc(init_req->size);
    memcpy(wasm_copy, init_req->binary, init_req->size);

    DRV_DEBUG("Loading WASM module %p @ %d from %p @ %d", wasm_copy, init_req->size, init_req->binary, init_req->size);
    wasm_module_t tmp_module = wasm_runtime_load(wasm_copy, init_req->size, error_buffer, 1024);
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
    rc = hb_beamr_lib_load_wasm_module(wasm_ctx, init_req->binary, init_req->size);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        send_error(proc->port_term, "Failed to load WASM module: %s", hb_beamr_lib_get_last_error(wasm_ctx));
        drv_unlock(proc->is_running);
        return;
    }

    rc = hb_beamr_lib_instantiate(wasm_ctx, 256 * 1024, 1 * 1024 * 1024);
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

    CallExportHandlerReq* call_export_req = (CallExportHandlerReq*)raw;

    Proc* proc = call_export_req->proc;
    drv_lock(proc->is_running);

    ensure_thread_init();
    
    hb_beamr_lib_context_t *wasm_ctx = proc->wasm_ctx;
    hb_beamr_meta_module_t *wasm_meta = proc->wasm_meta;

    wasm_val_t* results = malloc(call_export_req->result_count * sizeof(wasm_val_t));

#ifdef HB_DEBUG
    DRV_DEBUG("Calling export: %s", call_export_req->function_name);
    hb_beamr_utils_print_wasm_vals(call_export_req->args, call_export_req->arg_count);
    hb_beamr_utils_print_wasm_val_kinds(call_export_req->result_types, call_export_req->result_count);
#endif

    hb_beamr_lib_rc_t rc = hb_beamr_lib_call_export(wasm_ctx, call_export_req->function_name, call_export_req->arg_count, call_export_req->args, call_export_req->result_count, results);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        send_error(proc->port_term, "Failed to call exported function: %s", hb_beamr_lib_get_last_error(wasm_ctx));
        drv_unlock(proc->is_running);
        return;
    }

    ErlDrvTermData* msg = driver_alloc(sizeof(ErlDrvTermData) * 4);
    int msg_i = 0;
    msg[msg_i++] = ERL_DRV_ATOM;
    msg[msg_i++] = atom_execution_result;
    wasm_vals_to_erl_msg(results, call_export_req->result_count, &msg, &msg_i, 4);
    msg[msg_i++] = ERL_DRV_TUPLE;
    msg[msg_i++] = 2;

    int send_res = erl_drv_output_term(proc->port_term, msg, msg_i);
    DRV_DEBUG("Send result: %d", send_res);

	DRV_DEBUG("Unlocking is_running mutex: %p", proc->is_running);
    drv_unlock(proc->is_running);
}

void wasm_execute_indirect_function(void *raw) {
    DRV_DEBUG("wasm_execute_indirect_function called, raw: %p", raw);

    CallIndirectHandlerReq* call_indirect_req = (CallIndirectHandlerReq*)raw;
    
    Proc* proc = call_indirect_req->proc;
    drv_lock(proc->is_running);

    ensure_thread_init();

    DRV_DEBUG("Indirect function table name: %s", call_indirect_req->table_name);
    DRV_DEBUG("Indirect function table index: %ld", call_indirect_req->table_index);
    DRV_DEBUG("Indirect function args: %p", call_indirect_req->args);

    hb_beamr_lib_context_t *wasm_ctx = proc->wasm_ctx;
    hb_beamr_meta_module_t *wasm_meta = proc->wasm_meta;

    hb_beamr_lib_rc_t rc;

    wasm_val_t* results = malloc(call_indirect_req->result_count * sizeof(wasm_val_t));

#ifdef HB_DEBUG
    DRV_DEBUG("Calling indirect table: %s, index: %ld", call_indirect_req->table_name, call_indirect_req->table_index);
    hb_beamr_utils_print_wasm_vals(call_indirect_req->args, call_indirect_req->arg_count);
    hb_beamr_utils_print_wasm_val_kinds(call_indirect_req->result_types, call_indirect_req->result_count);
#endif

    rc = hb_beamr_lib_call_indirect(wasm_ctx, call_indirect_req->table_name, call_indirect_req->table_index, call_indirect_req->arg_count, call_indirect_req->args, call_indirect_req->result_count, results);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        send_error(proc->port_term, "Failed to call indirect function");
        drv_unlock(proc->is_running);
        return;
    }

    drv_unlock(proc->is_running);
    DRV_DEBUG("Indirect function call completed successfully");
}
