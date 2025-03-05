#include "include/hb_wasm.h"
#include "include/hb_logging.h"
#include "include/hb_helpers.h"
#include "include/hb_driver.h"

extern ErlDrvTermData atom_ok;
extern ErlDrvTermData atom_import;
extern ErlDrvTermData atom_execution_result;

void wasm_func_new_with_env_finalizer(void* env){
    ImportHook* hook = (ImportHook*) env;
    DRV_DEBUG("Free %d bytes (allocated for imported function stub)", sizeof(*hook));

    // Also clean up the signature, that is a type_str.
    if(hook->signature){
        driver_free(hook->signature);
    }
    driver_free(hook);

}

wasm_trap_t* wasm_handle_import(void* env, const wasm_val_vec_t* args, wasm_val_vec_t* results) {
    DRV_DEBUG("generic_import_handler called");
    ImportHook* import_hook = (ImportHook*)env;
    Proc* proc = import_hook->proc;

    // Check if the field name is "invoke"; if not, exit early
    if (strncmp(import_hook->field_name, "invoke", 6) == 0) {
        wasm_execute_indirect_function(proc, import_hook->field_name, args, results);
        return NULL;
    }

    DRV_DEBUG("Proc: %p. Args size: %d", proc, args->size);
    DRV_DEBUG("Import name: %s.%s [%s]", import_hook->module_name, import_hook->field_name, import_hook->signature);

    // Initialize the message object
    ErlDrvTermData* msg = driver_alloc(sizeof(ErlDrvTermData) * ((2+(2*3)) + ((args->size + 1) * 2) + ((results->size + 1) * 2) + 2));
    int msg_index = 0;
    msg[msg_index++] = ERL_DRV_ATOM;
    msg[msg_index++] = atom_import;
    msg[msg_index++] = ERL_DRV_STRING;
    msg[msg_index++] = (ErlDrvTermData) import_hook->module_name;
    msg[msg_index++] = strlen(import_hook->module_name);
    msg[msg_index++] = ERL_DRV_STRING;
    msg[msg_index++] = (ErlDrvTermData) import_hook->field_name;
    msg[msg_index++] = strlen(import_hook->field_name);

    // Encode args
    for (size_t i = 0; i < args->size; i++) {
        msg_index += wasm_val_to_erl_term(&msg[msg_index], &args->data[i]);
    }
    msg[msg_index++] = ERL_DRV_NIL;
    msg[msg_index++] = ERL_DRV_LIST;
    msg[msg_index++] = args->size + 1;

    // Encode function signature
    msg[msg_index++] = ERL_DRV_STRING;
    msg[msg_index++] = (ErlDrvTermData) import_hook->signature;
    msg[msg_index++] = strlen(import_hook->signature) - 1;

    // Prepare the message to send to the Erlang side
    msg[msg_index++] = ERL_DRV_TUPLE;
    msg[msg_index++] = 5;

    // Initialize the result vector and set the required result types
    proc->current_import = driver_alloc(sizeof(ImportResponse));

    // Create and initialize a is_running and condition variable for the response
    proc->current_import->response_ready = erl_drv_mutex_create("response_mutex");
    proc->current_import->cond = erl_drv_cond_create("response_cond");
    proc->current_import->ready = 0;

    DRV_DEBUG("Sending %d terms...", msg_index);
    // Send the message to the caller process
    int msg_res = erl_drv_output_term(proc->port_term, msg, msg_index);
    // Wait for the response (we set this directly after the message was sent
    // so we have the lock, before Erlang sends us data back)
    drv_wait(proc->current_import->response_ready, proc->current_import->cond, &proc->current_import->ready);

    DRV_DEBUG("Response ready");

    // Handle error in the response
    if (proc->current_import->error_message) {
        DRV_DEBUG("Import execution failed. Error message: %s", proc->current_import->error_message);
        wasm_name_t message;
        wasm_name_new_from_string_nt(&message, proc->current_import->error_message);
        wasm_trap_t* trap = wasm_trap_new(proc->store, &message);
        driver_free(proc->current_import);
        proc->current_import = NULL;
        return trap;
    }

    // Convert the response back to WASM values
    const wasm_valtype_vec_t* result_types = wasm_functype_results(wasm_func_type(import_hook->stub_func));
    for(int i = 0; i < proc->current_import->result_length; i++) {
        results->data[i].kind = wasm_valtype_kind(result_types->data[i]);
    }
    int res = erl_terms_to_wasm_vals(results, proc->current_import->result_terms);
    if(res == -1) {
        DRV_DEBUG("Failed to convert terms to wasm vals");
        return NULL;
    }

    results->num_elems = result_types->num_elems;

    // Clean up
    DRV_DEBUG("Cleaning up import response");
    driver_free(msg);
    erl_drv_cond_destroy(proc->current_import->cond);
    erl_drv_mutex_destroy(proc->current_import->response_ready);

    // Cleanup result terms
    driver_free(proc->current_import->result_terms);
    proc->current_import->result_terms = NULL;

    driver_free(proc->current_import);
    proc->current_import = NULL;
    return NULL;
}

void wasm_initialize_runtime(void* raw) {
    DRV_DEBUG("Initializing WASM module");
    Proc* proc = (Proc*)raw;
    // Proc* proc = mod_bin->proc;
    drv_lock(proc->is_running);
    // Initialize WASM engine, store, etc.

#if HB_DEBUG==1
    wasm_runtime_set_log_level(WASM_LOG_LEVEL_VERBOSE);
#else
    wasm_runtime_set_log_level(WASM_LOG_LEVEL_ERROR);
#endif

    DRV_DEBUG("Mode: %s", proc->mod_bin->mode);

    // if(strcmp(mod_bin->mode, "wasm") == 0) {
    //     DRV_DEBUG("Using WASM mode.");
    //     wasm_runtime_set_default_running_mode(Mode_Interp);
    // } else {
    //     DRV_DEBUG("Using AOT mode.");
    // }

    proc->engine = wasm_engine_new();
    DRV_DEBUG("Created engine");
    proc->store = wasm_store_new(proc->engine);
    DRV_DEBUG("Created store");


    // Load WASM module
    wasm_byte_vec_t binary;
    wasm_byte_vec_new(&binary, proc->mod_bin->size, (const wasm_byte_t*)proc->mod_bin->binary);

    proc->module = wasm_module_new(proc->store, &binary);
    DRV_DEBUG("Module created: %p", proc->module);
    if (!proc->module) {
        DRV_DEBUG("Failed to create module");
        send_error(proc, "Failed to create module.");
        wasm_byte_vec_delete(&binary);
        wasm_store_delete(proc->store);
        wasm_engine_delete(proc->engine);
        drv_unlock(proc->is_running);
        return;
    }
    //wasm_byte_vec_delete(&binary);
    DRV_DEBUG("Created module");

    // Get imports
    wasm_importtype_vec_t imports;
    wasm_module_imports(proc->module, &imports);
    DRV_DEBUG("Imports size: %d", imports.size);
    wasm_extern_t *stubs[imports.size];
    proc->mod_bin->stubs = driver_alloc(imports.size * sizeof(wasm_func_t*));
    proc->mod_bin->stubs_size = imports.size;

    // Get exports
    wasm_exporttype_vec_t exports;
    wasm_module_exports(proc->module, &exports);

    // Create Erlang lists for imports
    int init_msg_size = sizeof(ErlDrvTermData) * (2 + 3 + 5 + (13 * imports.size) + (11 * exports.size));
    DRV_DEBUG("Allocate %d bytes for wasm init message to erlang", init_msg_size);
    ErlDrvTermData* init_msg = driver_alloc(init_msg_size);
    int msg_i = 0;

    // 2 in the init_msg_size
    init_msg[msg_i++] = ERL_DRV_ATOM;
    init_msg[msg_i++] = atom_execution_result;
    // Process imports
    for (int i = 0; i < imports.size; ++i) {
        const wasm_importtype_t* import = imports.data[i];
        const wasm_name_t* module_name = wasm_importtype_module(import);
        const wasm_name_t* name = wasm_importtype_name(import);
        const wasm_externtype_t* type = wasm_importtype_type(import);

        //DRV_DEBUG("Import: %s.%s", module_name->data, name->data);

        // cleaned by the wasm_func_new_with_env_finalizer
        char* import_type_str = driver_alloc(256);
        // TODO: What happpens here?
        if(!get_function_sig(type, import_type_str)) {
            // TODO: Handle other types of imports?
            continue;
        }
        // 13 items in the each import message
        init_msg[msg_i++] = ERL_DRV_ATOM;
        init_msg[msg_i++] = driver_mk_atom((char*)wasm_externtype_to_kind_string(type));
        init_msg[msg_i++] = ERL_DRV_STRING;
        init_msg[msg_i++] = (ErlDrvTermData)module_name->data;
        init_msg[msg_i++] = module_name->size - 1;
        init_msg[msg_i++] = ERL_DRV_STRING;
        init_msg[msg_i++] = (ErlDrvTermData)name->data;
        init_msg[msg_i++] = name->size - 1;
        init_msg[msg_i++] = ERL_DRV_STRING;
        init_msg[msg_i++] = (ErlDrvTermData)import_type_str;
        init_msg[msg_i++] = strlen(import_type_str);
        init_msg[msg_i++] = ERL_DRV_TUPLE;
        init_msg[msg_i++] = 4;

        DRV_DEBUG("Creating callback for %s.%s [%s]", module_name->data, name->data, import_type_str);
        // the hook is cleared by wasm_func_new_with_env_finalizer
        ImportHook* hook = driver_alloc(sizeof(ImportHook));
        hook->module_name = module_name->data;
        hook->field_name = name->data;
        hook->proc = proc;
        hook->signature = import_type_str;

        hook->stub_func =
            wasm_func_new_with_env(
                proc->store,
                wasm_externtype_as_functype_const(type),
                wasm_handle_import,
                hook,
                wasm_func_new_with_env_finalizer
            );
        stubs[i] = wasm_func_as_extern(hook->stub_func);
        proc->mod_bin->stubs[i] = hook->stub_func ;
    }

    init_msg[msg_i++] = ERL_DRV_NIL;
    init_msg[msg_i++] = ERL_DRV_LIST;
    init_msg[msg_i++] = imports.size + 1;

    // Create proc!
    wasm_extern_vec_t externs;
    wasm_extern_vec_new(&externs, imports.size, stubs);
    wasm_trap_t* trap = NULL;
    proc->instance = wasm_instance_new_with_args(proc->store, proc->module, &externs, &trap, 0x10000, 0x10000);
    if (!proc->instance) {
        DRV_DEBUG("Failed to create WASM instance");
        send_error(proc, "Failed to create WASM instance (although module was created).");
        drv_unlock(proc->is_running);
        return;
    }

    wasm_extern_vec_t exported_externs;
    wasm_instance_exports(proc->instance, &exported_externs);

    // Refresh the exports now that we have an instance
    wasm_module_exports(proc->module, &exports);

    char** export_type_str = driver_alloc(sizeof(char*) * exports.size);
    for (size_t i = 0; i < exports.size; i++) {
        const wasm_exporttype_t* export = exports.data[i];
        const wasm_name_t* name = wasm_exporttype_name(export);
        const wasm_externtype_t* type = wasm_exporttype_type(export);
        char* kind_str = (char*) wasm_externtype_to_kind_string(type);

        // Check if the export is the indirect function table
        if (strcmp(name->data, "__indirect_function_table") == 0) {
            DRV_DEBUG("Found indirect function table: %s. Index: %d", name->data, i);
            proc->indirect_func_table_ix = i;
            const wasm_tabletype_t* table_type = wasm_externtype_as_tabletype_const(type);
            const wasm_limits_t* table_limits = wasm_tabletype_limits(table_type);
            // Retrieve the indirect function table
            proc->indirect_func_table = wasm_extern_as_table(exported_externs.data[i]);

        }

        // cleared below in the same function
        char* type_str = driver_alloc(256);
        export_type_str[i] = type_str;
        if ( !get_function_sig(type, type_str) ){
            DRV_DEBUG("Could not find function sig %s", type_str);

        }

        DRV_DEBUG("Export: %s [%s] -> %s", name->data, kind_str, type_str);

        // 10 elements for each exported function
        init_msg[msg_i++] = ERL_DRV_ATOM;
        init_msg[msg_i++] = driver_mk_atom(kind_str);
        init_msg[msg_i++] = ERL_DRV_STRING;
        init_msg[msg_i++] = (ErlDrvTermData)name->data;
        init_msg[msg_i++] = name->size - 1;
        init_msg[msg_i++] = ERL_DRV_STRING;
        init_msg[msg_i++] = (ErlDrvTermData)type_str;
        init_msg[msg_i++] = strlen(type_str);
        init_msg[msg_i++] = ERL_DRV_TUPLE;
        init_msg[msg_i++] = 3;
    }

    // 5 closing elements
    init_msg[msg_i++] = ERL_DRV_NIL;
    init_msg[msg_i++] = ERL_DRV_LIST;
    init_msg[msg_i++] = (exports.size) + 1;
    init_msg[msg_i++] = ERL_DRV_TUPLE;
    init_msg[msg_i++] = 3;

    DRV_DEBUG("Sending init message to Erlang. Elements: %d", msg_i);
    int send_res = erl_drv_output_term(proc->port_term, init_msg, msg_i);
    DRV_DEBUG("Send result: %d", send_res);
    // clean up export type strings
    for (int i = 0; i < exports.size; i++) {
        driver_free(export_type_str[i]);
    }
    driver_free(export_type_str);
    proc->current_import = NULL;
    proc->is_initialized = 1;
    drv_unlock(proc->is_running);
}

void wasm_execute_function(void* raw) {
    Proc* proc = (Proc*)raw;
    DRV_DEBUG("Calling function: %s", proc->current_function);
    drv_lock(proc->is_running);
    char* function_name = proc->current_function;

    wasm_extern_vec_t exports;
    wasm_instance_exports(proc->instance, &exports);
    wasm_exporttype_vec_t export_types;
    wasm_module_exports(proc->module, &export_types);

    wasm_func_t* func = get_exported_function(exports, export_types, function_name);
    if (!func) {
        send_error(proc, "Function not found: %s", function_name);
        drv_unlock(proc->is_running);
        return;
    }
    DRV_DEBUG("Func: %p", func);

    const wasm_functype_t* func_type = wasm_func_type(func); // cleaned below
    const wasm_valtype_vec_t* param_types = wasm_functype_params(func_type);
    const wasm_valtype_vec_t* result_types = wasm_functype_results(func_type);

    wasm_val_vec_t args, results;

    // Also cleared below
    wasm_val_vec_new_uninitialized(&args, param_types->size);
    args.num_elems = param_types->num_elems;
    // CONV: ei_term* -> wasm_val_vec_t
    for(int i = 0; i < param_types->size; i++) {
        args.data[i].kind = wasm_valtype_kind(param_types->data[i]);
    }

    int res = erl_terms_to_wasm_vals(&args, proc->current_args);

    for(int i = 0; i < args.size; i++) {
        DRV_DEBUG("Arg %d: %d", i, args.data[i].of.i64);
        DRV_DEBUG("Source term: %d", proc->current_args[i].value.i_val);
    }

    driver_free(proc->current_args);
    proc->current_args = NULL;

    if(res == -1) {
        send_error(proc, "Failed to convert terms to wasm vals");
        drv_unlock(proc->is_running);
        return;
    }

    // cleared below
    wasm_val_vec_new_uninitialized(&results, result_types->size);
    results.num_elems = result_types->num_elems;
    for (size_t i = 0; i < result_types->size; i++) {
        results.data[i].kind = wasm_valtype_kind(result_types->data[i]);
    }

    proc->exec_env = wasm_runtime_get_exec_env_singleton(func->inst_comm_rt);

    // Call the function
    DRV_DEBUG("Calling function: %s", function_name);
    wasm_trap_t* trap = wasm_func_call(func, &args, &results);

    if (trap) {
        wasm_message_t trap_msg;
        wasm_trap_message(trap, &trap_msg);
        // wasm_frame_t* origin = wasm_trap_origin(trap);
        // int32_t func_index = wasm_frame_func_index(origin);
        // int32_t func_offset = wasm_frame_func_offset(origin);
        // char* func_name;
        // DRV_DEBUG("WASM Exception: [func_index: %d, func_offset: %d] %.*s", func_index, func_offset, trap_msg.size, trap_msg.data);
        send_error(proc, "%.*s", trap_msg.size, trap_msg.data);
        drv_unlock(proc->is_running);
        return;
    }

    // Send the results back to Erlang
    DRV_DEBUG("Results size: %d", results.size);
    ErlDrvTermData* msg = driver_alloc(sizeof(ErlDrvTermData) * (7 + (results.size * 2)));
    DRV_DEBUG("Allocated msg");
    int msg_index = 0;
    msg[msg_index++] = ERL_DRV_ATOM;
    msg[msg_index++] = atom_execution_result;
    for (size_t i = 0; i < results.size; i++) {
        DRV_DEBUG("Processing result %d", i);
        DRV_DEBUG("Result type: %d", results.data[i].kind);
        switch(results.data[i].kind) {
            case WASM_I32:
                DRV_DEBUG("Value: %d", results.data[i].of.i32);
                break;
            case WASM_I64:
                DRV_DEBUG("Value: %ld", results.data[i].of.i64);
                break;
            case WASM_F32:
                DRV_DEBUG("Value: %f", results.data[i].of.f32);
                break;
            case WASM_F64:
                DRV_DEBUG("Value: %f", results.data[i].of.f64);
                break;
            default:
                DRV_DEBUG("Unknown result type.", results.data[i].kind);
                break;
        }
        int res_size = wasm_val_to_erl_term(&msg[msg_index], &results.data[i]);
        msg_index += res_size;
    }
    msg[msg_index++] = ERL_DRV_NIL;
    msg[msg_index++] = ERL_DRV_LIST;
    msg[msg_index++] = results.size + 1;
    msg[msg_index++] = ERL_DRV_TUPLE;
    msg[msg_index++] = 2;
    DRV_DEBUG("Sending %d terms", msg_index);
    int response_msg_res = erl_drv_output_term(proc->port_term, msg, msg_index);
    driver_free(msg);
    DRV_DEBUG("Msg: %d", response_msg_res);
    wasm_val_vec_delete(&results);
    wasm_val_vec_delete(&args);
    wasm_functype_delete(func_type);
    driver_free(proc->current_import);
    proc->current_import = NULL;
    wasm_extern_vec_delete(&exports);
    wasm_exporttype_vec_delete(&export_types);
    wasm_trap_delete(trap);
    DRV_DEBUG("Unlocking is_running mutex: %p", proc->is_running);
    drv_unlock(proc->is_running);
}

int wasm_execute_indirect_function(Proc* proc, const char *field_name, const wasm_val_vec_t* input_args, wasm_val_vec_t* output_results) {


    DRV_DEBUG("=================================================");
    DRV_DEBUG("Starting function invocation");
    DRV_DEBUG("=================================================");

    wasm_table_t* indirect_function_table = proc->indirect_func_table;


    int result = 0;
    DRV_DEBUG("Function name: %s", field_name);

// Extract the function index from the input arguments
    int function_index = input_args->data[0].of.i32;
    DRV_DEBUG("Function index retrieved from input_args: %d", function_index);

    // Get the function reference from the table and cast it to a function
    wasm_ref_t* function_ref = wasm_table_get(indirect_function_table, function_index);
    const wasm_func_t* func = wasm_ref_as_func(function_ref);
    DRV_DEBUG("Function pointer: %p", func);

    // Retrieve the function type and log its parameters and results
    const wasm_functype_t* function_type = wasm_func_type(func);
    if (!function_type) {
        DRV_DEBUG("Failed to retrieve function type for function at index %d", function_index);
    }

    // Log the function's parameter types
    const wasm_valtype_vec_t* param_types = wasm_functype_params(function_type);
    DRV_DEBUG("Function at index %d has %zu parameters", function_index, param_types->size);
    for (size_t j = 0; j < param_types->size; ++j) {
        const wasm_valtype_t* param_type = param_types->data[j];
        wasm_valkind_t param_kind = wasm_valtype_kind(param_type);
        DRV_DEBUG("Param %zu: %s", j, get_wasm_type_name(param_kind));
    }


    // Log the function's result types
    const wasm_valtype_vec_t* result_types = wasm_functype_results(function_type);
    DRV_DEBUG("Function at index %d has %zu results", function_index, result_types->size);
    for (size_t k = 0; k < result_types->size; ++k) {
        const wasm_valtype_t* result_type = result_types->data[k];
        wasm_valkind_t result_kind = wasm_valtype_kind(result_type);
        DRV_DEBUG("Result %zu: %s", k, get_wasm_type_name(result_kind));
    }

    // Prepare the arguments for the function call
    wasm_val_vec_t prepared_args;
        // If there are no arguments or only one argument (function index), no preparation is needed
    if (input_args->size <= 1) {
        DRV_DEBUG("Not enough arguments to create new wasm_val_vec_t");
        return 0;
    }

    // Allocate memory for the prepared arguments
    wasm_val_t* prepared_data = driver_alloc(sizeof(wasm_val_t) * (input_args->size - 1));

    // Copy the arguments starting from the second element (skip function index)
    for (size_t i = 1; i < input_args->size; ++i) {
        prepared_data[i - 1] = input_args->data[i];
    }

    // Create a new wasm_val_vec_t with the prepared arguments
    wasm_val_vec_new(&prepared_args, input_args->size - 1, prepared_data);
    DRV_DEBUG("Prepared %zu arguments for function call", prepared_args.size);

    uint64_t argc = prepared_args.size;
    uint64_t* argv = driver_alloc(sizeof(uint64_t) * argc);

    // Convert prepared arguments to an array of 64-bit integers
    for (uint64_t i = 0; i < argc; ++i) {
        argv[i] = prepared_args.data[i].of.i64;
    }


    /* ---------------- STACK SAVE -----------------*/

    // const char* stack_save_name = "emscripten_stack_get_current";
    // wasm_val_t *stack_save_params = NULL;
    // wasm_val_t stack_save_results[1];
    // if (call_exported_function_runtime(proc, stack_save_name, stack_save_params, stack_save_results) != 0) {
    //     DRV_DEBUG("Failed to call stack save function");
    // }

    /* ---------------- STACK SAVE -----------------*/

    // Attempt to call the function and check for any exceptions
    if (!wasm_runtime_call_indirect(proc->exec_env, function_index, argc, argv)) {
        if (wasm_runtime_get_exception(proc->exec_env)) {
            DRV_DEBUG("%s", wasm_runtime_get_exception(proc->exec_env));
        }
        DRV_DEBUG("WASM function call failed");
        result = -1;
    }

    if(result != 0) {



    }


    // driver_free allocated memory
    driver_free(argv);
    driver_free(prepared_args.data);
    DRV_DEBUG("Function call completed successfully");
    return result;
}

int wasm_execute_exported_function(Proc* proc, const *function_name, wasm_val_t* params, wasm_val_t * results) {
    DRV_DEBUG("=== Calling Runtime Export Function ===");
    DRV_DEBUG("=   Function name: %s", function_name);

    // Get exported wasm_func_t pointer by function name
    wasm_extern_vec_t exports;
    wasm_instance_exports(proc->instance, &exports);
    wasm_exporttype_vec_t export_types;
    wasm_module_exports(proc->module, &export_types);
    // TODO: Free these exports and export_types
    wasm_func_t* func = get_exported_function(exports, export_types, function_name);
    if(!func) {
        DRV_DEBUG("=   Failed to get exported function");
        return -1;
    }

    // Get the function type
    const wasm_functype_t* function_type = wasm_func_type(func);
    if (!function_type) {
        DRV_DEBUG("=   Failed to get function type");
        return -1;
    }

    // Get the function's parameter types and set the argument types for args
    const wasm_valtype_vec_t* param_types = wasm_functype_params(function_type);
    if(!param_types) {
        DRV_DEBUG("=   Failed to get function parameters");
        return -1;
    }

    DRV_DEBUG("=   Function has %zu parameters", param_types->size);
    for (size_t j = 0; j < param_types->size; ++j) {
        const wasm_valtype_t* param_type = param_types->data[j];
        wasm_valkind_t param_kind = wasm_valtype_kind(param_type);
        params[j].kind = param_kind;
        DRV_DEBUG("=      Param %zu: %s, %i", j, get_wasm_type_name(param_kind), params[j].of.i64);
    }


    // Get the function's result types and set the result types for results
    const wasm_valtype_vec_t* result_types = wasm_functype_results(function_type);
    if (!result_types) {
        DRV_DEBUG("=   Failed to get function results");
        return -1;
    }
    DRV_DEBUG("=   Function has %zu results", result_types->size);

    for (size_t k = 0; k < result_types->size; ++k) {
        const wasm_valtype_t* result_type = result_types->data[k];
        wasm_valkind_t result_kind = wasm_valtype_kind(result_type);
        results[k].kind = result_kind;
        results[k].of.i64 = 0;  // Initialize result value
        DRV_DEBUG("=      Result %zu: %s, %i", k, get_wasm_type_name(result_kind), results[k].of.i64);
    }


    // Call the exported function
    if (wasm_runtime_call_wasm_a(proc->exec_env, func->func_comm_rt, result_types->size, results, param_types->size, params)) {
        DRV_DEBUG("=   Function call successful");
    } else {
        const char* exception = wasm_runtime_get_exception(proc->exec_env);
        DRV_DEBUG("=   Function call failed: %s", exception);
        return -1;
    }

    // Retrieve the stack pointer result (as i64)
    int64_t stack_pointer = results[0].of.i64;  // Assuming the result is i64
    DRV_DEBUG("Stack pointer: %lld", stack_pointer);

    return 0;
}

