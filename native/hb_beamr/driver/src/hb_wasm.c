#include "../include/hb_wasm.h"
#include "../include/hb_logging.h"
#include "../include/hb_helpers.h"
#include "../include/hb_driver.h"

extern ErlDrvTermData atom_ok;
extern ErlDrvTermData atom_import;
extern ErlDrvTermData atom_execution_result;

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
    erl_drv_cond_destroy(proc->current_import->cond);
    erl_drv_mutex_destroy(proc->current_import->response_ready);
    driver_free(proc->current_import);

    proc->current_import = NULL;
    return NULL;
}

static void generic_import_native_symbol_func(wasm_exec_env_t exec_env, uint64_t *args) {
    NativeSymbolAttachment *attachment = (NativeSymbolAttachment *)wasm_runtime_get_function_attachment(exec_env);
    const char *func_name = attachment->field_name;
    DRV_DEBUG("generic_import_native_symbol_func called with %s\n", func_name);
}

void wasm_initialize_runtime(void* raw) {
    DRV_DEBUG("Initializing WASM module");
    LoadWasmReq* mod_bin = (LoadWasmReq*)raw;
    Proc* proc = mod_bin->proc;
    drv_lock(proc->is_running);
    // Initialize WASM engine, store, etc.

#if HB_DEBUG==1
    wasm_runtime_set_log_level(WASM_LOG_LEVEL_VERBOSE);
#else
    wasm_runtime_set_log_level(WASM_LOG_LEVEL_ERROR);
#endif

    DRV_DEBUG("Mode: %s", mod_bin->mode);

    // if(strcmp(mod_bin->mode, "wasm") == 0) {
    //     DRV_DEBUG("Using WASM mode.");
    //     wasm_runtime_set_default_running_mode(Mode_Interp);
    // } else {
    //     DRV_DEBUG("Using AOT mode.");
    // }

    // Initialize WAMR runtime
    RuntimeInitArgs init_args;
    memset(&init_args, 0, sizeof(RuntimeInitArgs));
    init_args.mem_alloc_type = Alloc_With_Allocator;
    init_args.mem_alloc_option.allocator.malloc_func = (void *)malloc;
    init_args.mem_alloc_option.allocator.realloc_func = (void *)realloc;
    init_args.mem_alloc_option.allocator.free_func = (void *)free;

    if (!wasm_runtime_full_init(&init_args)) {
        DRV_DEBUG("Failed to initialize WAMR runtime");
        return;
    }
    DRV_DEBUG("Created engine");

    char *error_buf = driver_alloc(1024);
    uint8_t *binary_copy = driver_alloc(mod_bin->size);
    memcpy(binary_copy, mod_bin->binary, mod_bin->size);
    wasm_module_t module_proto = wasm_runtime_load(binary_copy, mod_bin->size, error_buf, 1024);
    if (!module_proto) {
        DRV_DEBUG("Failed to load WASM proto-module: %s", error_buf);
        send_error(proc, "Failed to load WASM proto-module: %s", error_buf);
        drv_unlock(proc->is_running);
        return;
    }
    DRV_DEBUG("Created proto-module: %p", module_proto);

    int32_t import_count = wasm_runtime_get_import_count(module_proto);
    DRV_DEBUG("Import count: %d", import_count);
    wasm_import_t *imports = driver_alloc(sizeof(wasm_import_t) * import_count);

    for (int i = 0; i < import_count; i++) {
        wasm_runtime_get_import_type(module_proto, i, &imports[i]);
        DRV_DEBUG("imports[%d]: %s.%s [%d]", i, imports[i].module_name, imports[i].name, imports[i].kind);
    }

    int32_t export_count = wasm_runtime_get_export_count(module_proto);
    DRV_DEBUG("Export count: %d", export_count);
    wasm_export_t *exports = driver_alloc(sizeof(wasm_export_t) * export_count);

    for (int i = 0; i < export_count; i++) {
        wasm_runtime_get_export_type(module_proto, i, &exports[i]);
        DRV_DEBUG("exports[%d]: %s [%d]", i, exports[i].name, exports[i].kind);
    }

    // Create Erlang lists for imports
    int init_msg_size = sizeof(ErlDrvTermData) * (2 + 3 + 5 + (13 * import_count) + (11 * export_count));
    ErlDrvTermData* init_msg = driver_alloc(init_msg_size);
    int msg_i = 0;

    // 2 in the init_msg_size
    init_msg[msg_i++] = ERL_DRV_ATOM;
    init_msg[msg_i++] = atom_execution_result;

    // Scaffold the index of modules and their symbols
    ImportModuleSymbols *import_modules_symbols = driver_alloc(sizeof(ImportModuleSymbols) * import_count);
    int found_import_modules_count = 0;
    for (int i = 0; i < import_count; ++i) {
        const char* module_name = imports[i].module_name;
        bool found = false;
        for (int j = 0; j < found_import_modules_count; ++j) {
            if (strcmp(import_modules_symbols[j].module_name, module_name) == 0) {
                found = true;
                import_modules_symbols[j].count++;
                break;
            }
        }
        if (!found) {
            import_modules_symbols[found_import_modules_count].module_name = module_name;
            import_modules_symbols[found_import_modules_count].count = 1;
            found_import_modules_count++;
        }
    }

    DRV_DEBUG("Found %d import modules", found_import_modules_count);
    for (int i = 0; i < found_import_modules_count; ++i) {
        DRV_DEBUG("Import module %d: %s (%d symbols)", i, import_modules_symbols[i].module_name, import_modules_symbols[i].count);
    }

    // Build the native symbols
    for (int i = 0; i < found_import_modules_count; ++i) {
        // Allocate symbols array directly into the struct in the main array
        import_modules_symbols[i].symbols = driver_alloc(sizeof(NativeSymbol) * import_modules_symbols[i].count);
        if (!import_modules_symbols[i].symbols) {
            DRV_DEBUG("Failed to allocate memory for symbols array for module %s", import_modules_symbols[i].module_name);
            // TODO: Handle allocation failure (e.g., cleanup previously allocated memory)
            continue;
        }

        int current_symbol_index = 0; // Index for the current module's symbols array

        // Iterate through all original imports to find ones matching the current module
        for (int k = 0; k < import_count; ++k) {
            if (strcmp(imports[k].module_name, import_modules_symbols[i].module_name) == 0) {
                // Found an import belonging to the current module
                if (current_symbol_index >= import_modules_symbols[i].count) {
                     DRV_DEBUG("Error: Found more symbols for module %s than initially counted.", import_modules_symbols[i].module_name);
                     // This indicates a potential logic error in the counting phase
                     break; // Stop processing this module
                }

                wasm_import_t import = imports[k]; // Use the correct import
                const char* module_name = import.module_name;
                const char* name = import.name;
                const wasm_import_export_kind_t kind = import.kind;
                const wasm_func_type_t type = import.u.func_type;
                DRV_DEBUG("Building symbol %d for module %s: %s.%s", current_symbol_index, module_name, module_name, name);

                uint32_t param_count = wasm_func_type_get_param_count(type);
                //DRV_DEBUG("Param count: %d", param_count);

                enum wasm_valkind_enum *param_kinds = driver_alloc(sizeof(enum wasm_valkind_enum) * param_count);
                 if (!param_kinds && param_count > 0) {
                    DRV_DEBUG("Failed to allocate memory for param_kinds for %s.%s", module_name, name);
                    // TODO: Handle allocation failure
                    continue; // Skip this symbol
                }
                for (uint32_t p_idx = 0; p_idx < param_count; ++p_idx) {
                    param_kinds[p_idx] = wasm_func_type_get_param_valkind(type, p_idx);
                    //DRV_DEBUG("Param %d kind: %d", p_idx, param_kinds[p_idx]);
                }

                char* type_str = driver_alloc(256);
                 if (!type_str) {
                    DRV_DEBUG("Failed to allocate memory for type_str for %s.%s", module_name, name);
                    if (param_kinds) driver_free(param_kinds);
                    // TODO: Handle allocation failure
                    continue; // Skip this symbol
                }

                if(!get_function_sig(param_kinds, param_count, type_str)) {
                    DRV_DEBUG("Failed to get function signature for %s.%s", module_name, name);
                     if (param_kinds) driver_free(param_kinds);
                     driver_free(type_str);
                    // TODO: Handle other types of imports?
                    continue; // Skip this symbol
                } else {
                    DRV_DEBUG("Got function signature for %s.%s: %s", module_name, name, type_str);
                }
                if (param_kinds) driver_free(param_kinds); // Free param_kinds after use

                // Add import details to the init_msg for Erlang
                init_msg[msg_i++] = ERL_DRV_ATOM;
                init_msg[msg_i++] = driver_mk_atom((char*)wasm_import_export_kind_to_string(kind));
                init_msg[msg_i++] = ERL_DRV_STRING;
                init_msg[msg_i++] = (ErlDrvTermData)module_name;
                init_msg[msg_i++] = strlen(module_name);
                init_msg[msg_i++] = ERL_DRV_STRING;
                init_msg[msg_i++] = (ErlDrvTermData)name;
                init_msg[msg_i++] = strlen(name);
                init_msg[msg_i++] = ERL_DRV_STRING;
                init_msg[msg_i++] = (ErlDrvTermData)type_str;
                init_msg[msg_i++] = strlen(type_str);
                init_msg[msg_i++] = ERL_DRV_TUPLE;
                init_msg[msg_i++] = 4;

                DRV_DEBUG("Generating native symbol metadata for %s.%s [%s]", module_name, name, type_str);

                NativeSymbolAttachment *attachment = driver_alloc(sizeof(NativeSymbolAttachment));
                if (!attachment) {
                    DRV_DEBUG("Failed to allocate memory for NativeSymbolAttachment");
                    driver_free(type_str); // Free type_str allocated earlier
                    continue;
                }
                attachment->module_name = module_name; // Potential dangling pointer if module_proto is freed
                attachment->field_name = name;       // Potential dangling pointer if module_proto is freed
                attachment->signature = type_str;      // Keep allocated type_str, free later

                NativeSymbol *native_symbol = driver_alloc(sizeof(NativeSymbol));
                if (!native_symbol) {
                    DRV_DEBUG("Failed to allocate memory for NativeSymbol");
                    driver_free(attachment); // Free attachment allocated above
                    driver_free(type_str); // Free type_str allocated earlier
                    continue;
                }
                native_symbol->symbol = name;         // Potential dangling pointer if module_proto is freed
                native_symbol->func_ptr = generic_import_native_symbol_func;
                native_symbol->signature = NULL; // wasm_runtime_register_natives_raw ignores signature
                native_symbol->attachment = attachment;

                // Copy the struct content into the allocated array
                import_modules_symbols[i].symbols[current_symbol_index] = *native_symbol;

                // TODO: Memory leak: 'native_symbol' and 'attachment' allocated above are lost here.
                // We copied the *content* of native_symbol, but not the pointer itself.
                // The 'attachment' pointer inside the copied struct now points to the allocated attachment.
                // Need to decide how to manage the lifetime of 'native_symbol', 'attachment', and 'type_str'.
                // If wasm_runtime_register_natives_raw copies data, we might need to free them after registration.
                // If it stores pointers, they need to persist.

                current_symbol_index++; // Move to the next slot in the symbols array

                // NOTE: type_str is intentionally NOT freed here because its pointer is stored in 'attachment'.
                // It needs to be freed later, likely alongside 'attachment' and 'native_symbol'.
            }
        }
         // Optional: Check if we filled the expected number of symbols
        if (current_symbol_index != import_modules_symbols[i].count) {
            DRV_DEBUG("Warning: Expected %d symbols for module %s, but found %d.",
                      import_modules_symbols[i].count, import_modules_symbols[i].module_name, current_symbol_index);
            // This might indicate an issue with the counting logic or the processing loop
        }
    }

    DRV_DEBUG("Cleaning up proto-module");
    wasm_module_delete(&module_proto);
    wasm_runtime_destroy();

    DRV_DEBUG("Reinitializing runtime");
    wasm_runtime_full_init(&init_args);

    init_msg[msg_i++] = ERL_DRV_NIL;
    init_msg[msg_i++] = ERL_DRV_LIST;
    init_msg[msg_i++] = import_count + 1;

    DRV_DEBUG("Registering native symbols");
    for (int i = 0; i < found_import_modules_count; ++i) {
        ImportModuleSymbols import_module_symbols = import_modules_symbols[i];
        DRV_DEBUG("Registering native symbols for import module[%d]: %s", i, import_module_symbols.module_name);
        wasm_runtime_register_natives_raw(import_module_symbols.module_name, import_module_symbols.symbols, import_module_symbols.count);
    }

    DRV_DEBUG("Initializing runtime module");
    wasm_module_t module_runtime = wasm_runtime_load(mod_bin->binary, mod_bin->size, error_buf, 1024);
    if (!module_runtime) {
        DRV_DEBUG("Failed to load WASM runtime-module: %s", error_buf);
        send_error(proc, "Failed to load WASM runtime-module: %s", error_buf);
        drv_unlock(proc->is_running);
        return;
    }
    DRV_DEBUG("Created runtime-module: %p", module_runtime);
    
    wasm_module_inst_t module_inst = wasm_runtime_instantiate(module_runtime, 0x10000, 0x10000, error_buf, 1024);
    if (!module_inst) {
        DRV_DEBUG("Failed to instantiate WASM runtime-module: %s", error_buf);
        send_error(proc, "Failed to instantiate WASM runtime-module: %s", error_buf);
        drv_unlock(proc->is_running);
        return;
    }
    DRV_DEBUG("Created runtime-module instance: %p", module_inst);
    // proc->instance = module_inst;

    DRV_DEBUG("Processing exports");
    wasm_table_inst_t indirect_func_table;
    if (!wasm_runtime_get_export_table_inst(module_inst, "__indirect_function_table", &indirect_func_table)) {
        DRV_DEBUG("Failed to find __indirect_function_table");
        // send_error(proc, "Failed to get indirect function table");
        // drv_unlock(proc->is_running);
        // return;
    }
    // proc->indirect_func_table = indirect_func_table;

    for (size_t i = 0; i < export_count; i++) {
        wasm_export_t export = exports[i];
        const char* name = export.name;
        const wasm_import_export_kind_t kind = export.kind;
        char* kind_str = (char*) wasm_import_export_kind_to_string(kind);
        DRV_DEBUG("Processing export: %s [%s]", name, kind_str);

        const wasm_func_type_t type = export.u.func_type;

        char* type_str = driver_alloc(256);
            if (!type_str) {
            DRV_DEBUG("Failed to allocate memory for type_str for %s", name);
            continue; // Skip this symbol
        }
        
        if (kind == WASM_IMPORT_EXPORT_KIND_FUNC) {
            uint32_t param_count = wasm_func_type_get_param_count(type);
            DRV_DEBUG("Param count: %d", param_count);

            enum wasm_valkind_enum *param_kinds = driver_alloc(sizeof(enum wasm_valkind_enum) * param_count);
                if (!param_kinds && param_count > 0) {
                DRV_DEBUG("Failed to allocate memory for param_kinds for %s", name);
                // TODO: Handle allocation failure
                continue; // Skip this symbol
            }
            for (uint32_t p_idx = 0; p_idx < param_count; ++p_idx) {
                param_kinds[p_idx] = wasm_func_type_get_param_valkind(type, p_idx);
                DRV_DEBUG("Param %d kind: %d", p_idx, param_kinds[p_idx]);
            }

            DRV_DEBUG("get_function_sig(%p, %d, %p)", param_kinds, param_count, type_str);

            if(!get_function_sig(param_kinds, param_count, type_str)) {
                DRV_DEBUG("Failed to get function signature for %s", name);
                if (param_kinds) driver_free(param_kinds);
                driver_free(type_str);
                // TODO: Handle other types of imports?
                continue; // Skip this symbol
            } else {
                DRV_DEBUG("Got function signature for %s: %s", name, type_str);
            }
        } else {
            type_str[0] = '\0';
        }

        DRV_DEBUG("Export: %s [%s] -> %s", name, kind_str, type_str);

        // 10 elements for each exported function
        init_msg[msg_i++] = ERL_DRV_ATOM;
        init_msg[msg_i++] = driver_mk_atom(kind_str);
        init_msg[msg_i++] = ERL_DRV_STRING;
        init_msg[msg_i++] = (ErlDrvTermData)name;
        init_msg[msg_i++] = strlen(name);
        init_msg[msg_i++] = ERL_DRV_STRING;
        init_msg[msg_i++] = (ErlDrvTermData)type_str;
        init_msg[msg_i++] = strlen(type_str);
        init_msg[msg_i++] = ERL_DRV_TUPLE;
        init_msg[msg_i++] = 3;
    }

    // 5 closing elements
    init_msg[msg_i++] = ERL_DRV_NIL;
    init_msg[msg_i++] = ERL_DRV_LIST;
    init_msg[msg_i++] = export_count + 1;
    init_msg[msg_i++] = ERL_DRV_TUPLE;
    init_msg[msg_i++] = 3;

    DRV_DEBUG("Sending init message to Erlang. Elements: %d", msg_i);

    int send_res = erl_drv_output_term(proc->port_term, init_msg, msg_i);
    DRV_DEBUG("Send result: %d", send_res);

    proc->current_import = NULL;
    proc->is_initialized = 1;
    drv_unlock(proc->is_running);
}

void wasm_execute_function(void* raw) {
    Proc* proc = (Proc*)raw;
    DRV_DEBUG("Calling function: %s", proc->current_function);
    drv_lock(proc->is_running);
    char* function_name = proc->current_function;

    // Find the function in the exports
    wasm_func_t* func = get_exported_function(proc, function_name);
    if (!func) {
        send_error(proc, "Function not found: %s", function_name);
        drv_unlock(proc->is_running);
        return;
    }
    DRV_DEBUG("Func: %p", func);

    const wasm_functype_t* func_type = wasm_func_type(func);
    const wasm_valtype_vec_t* param_types = wasm_functype_params(func_type);
    const wasm_valtype_vec_t* result_types = wasm_functype_results(func_type);

    wasm_val_vec_t args, results;
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

    if(res == -1) {
        send_error(proc, "Failed to convert terms to wasm vals");
        drv_unlock(proc->is_running);
        return;
    }

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
    proc->current_import = NULL;

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
    wasm_val_t* prepared_data = malloc(sizeof(wasm_val_t) * (input_args->size - 1));

    // Copy the arguments starting from the second element (skip function index)
    for (size_t i = 1; i < input_args->size; ++i) {
        prepared_data[i - 1] = input_args->data[i];
    }

    // Create a new wasm_val_vec_t with the prepared arguments
    wasm_val_vec_new(&prepared_args, input_args->size - 1, prepared_data);
    DRV_DEBUG("Prepared %zu arguments for function call", prepared_args.size);

    uint64_t argc = prepared_args.size;
    uint64_t* argv = malloc(sizeof(uint64_t) * argc);
    
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


    // Free allocated memory
    free(argv);
    free(prepared_args.data);
    DRV_DEBUG("Function call completed successfully");
    return result;
}

int wasm_execute_exported_function(Proc* proc, const char *function_name, wasm_val_t* params, wasm_val_t * results) {
    DRV_DEBUG("=== Calling Runtime Export Function ===");
    DRV_DEBUG("=   Function name: %s", function_name);


    // Get exported wasm_func_t pointer by function name
    wasm_func_t* func = get_exported_function(proc, function_name);
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

