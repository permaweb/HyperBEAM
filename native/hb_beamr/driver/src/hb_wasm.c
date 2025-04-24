#include "../include/hb_wasm.h"
#include "../include/hb_logging.h"
#include "../include/hb_helpers.h"
#include "../include/hb_driver.h"
#include "wasm_export.h"

extern ErlDrvTermData atom_ok;
extern ErlDrvTermData atom_import;
extern ErlDrvTermData atom_execution_result;

static void generic_import_native_symbol_func(wasm_exec_env_t exec_env, uint64_t *args) {
    DRV_DEBUG("generic_import_native_symbol_func called, exec_env: %p, args: %p", exec_env, args);
    wasm_module_inst_t module_inst = wasm_runtime_get_module_inst(exec_env);
    DRV_DEBUG("Module instance: %p", module_inst);

    NativeSymbolAttachment *attachment = (NativeSymbolAttachment *)wasm_runtime_get_function_attachment(exec_env);
    DRV_DEBUG("Attachment (%p)", attachment);
    if (!attachment) {
        DRV_DEBUG("Skipping import: No attachment found");
        return;
    }

    if (attachment->proc->import_stack_depth >= MAX_IMPORT_STACK_DEPTH) {
        DRV_DEBUG("Skipping import: Import stack depth exceeded");
        return;
    }

    Proc *proc = attachment->proc;
    const char *module_name = attachment->module_name;
    const char *func_name = attachment->field_name;
    const char *signature = attachment->signature;
    DRV_DEBUG("Calling import %s.%s [%s]", module_name, func_name, signature);

    uint32_t param_count = attachment->param_count;
    wasm_valkind_t *param_kinds = attachment->param_kinds;
    uint32_t result_count = attachment->result_count;
    wasm_valkind_t *result_kinds = attachment->result_kinds;
    DRV_DEBUG("Param count: %d", param_count);
    DRV_DEBUG("Result count: %d", result_count);
    // int param_size = kinds_size(param_kinds, param_count);
    // int result_size = kinds_size(result_kinds, result_count);
    // DRV_DEBUG("Param size: %d", param_size);
    // DRV_DEBUG("Result size: %d", result_size);

    // Initialize the message object
    int msg_size = sizeof(ErlDrvTermData) * ((2+(2*3)) + ((param_count + 1) * 2) + ((result_count + 1) * 2) + 2);
    DRV_DEBUG("Message size: %d", msg_size);
    ErlDrvTermData* msg = driver_alloc(msg_size);
    int msg_index = 0;
    msg[msg_index++] = ERL_DRV_ATOM;
    msg[msg_index++] = atom_import;
    msg[msg_index++] = ERL_DRV_STRING;
    msg[msg_index++] = (ErlDrvTermData) module_name;
    msg[msg_index++] = strlen(module_name);
    msg[msg_index++] = ERL_DRV_STRING;
    msg[msg_index++] = (ErlDrvTermData) func_name;
    msg[msg_index++] = strlen(func_name);

    // Encode args
    for (size_t i = 0; i < param_count; i++) {
        msg_index += import_arg_to_erl_term(&msg[msg_index], param_kinds[i], &args[i]);
    }
    msg[msg_index++] = ERL_DRV_NIL;
    msg[msg_index++] = ERL_DRV_LIST;
    msg[msg_index++] = param_count + 1;

    // Encode function signature
    msg[msg_index++] = ERL_DRV_STRING;
    msg[msg_index++] = (ErlDrvTermData) signature;
    msg[msg_index++] = strlen(signature) - 1;

    // Prepare the message to send to the Erlang side
    msg[msg_index++] = ERL_DRV_TUPLE;
    msg[msg_index++] = 5;

    // Initialize the result vector and set the required result types
    ImportResponse* this_import = driver_alloc(sizeof(ImportResponse));
    proc->import_stack[proc->import_stack_depth++] = this_import;
    DRV_DEBUG("import_stack: Import stack depth: %d", proc->import_stack_depth);
    proc->current_import = this_import;

    // Create and initialize a is_running and condition variable for the response
    char* response_mutex_name = driver_alloc(128);
    sprintf(response_mutex_name, "response_mutex_%d", proc->import_stack_depth);
    char* response_cond_name = driver_alloc(128);
    sprintf(response_cond_name, "response_cond_%d", proc->import_stack_depth);
    
    this_import->response_ready = erl_drv_mutex_create(response_mutex_name);
    this_import->cond = erl_drv_cond_create(response_cond_name);
    this_import->ready = 0;

    DRV_DEBUG("Sending %d terms...", msg_index);
    DRV_DEBUG("Pre-send state: proc=%p, port=%lu, msg=%p, msg_index=%d", proc, proc->port_term, msg, msg_index);
    DRV_DEBUG("  module_name (%p): '%s'", module_name, module_name ? module_name : "(null)");
    DRV_DEBUG("  func_name (%p): '%s'", func_name, func_name ? func_name : "(null)");
    DRV_DEBUG("  signature (%p): '%s' (len=%zu)", signature, signature ? signature : "(null)", signature ? strlen(signature) : 0);
    DRV_DEBUG("About to call erl_drv_output_term");
    // Send the message to the caller process
    int msg_res = erl_drv_output_term(proc->port_term, msg, msg_index);
    DRV_DEBUG("Call to erl_drv_output_term completed. Res: %d", msg_res);
    DRV_DEBUG("Message sent. Res: %d", msg_res);

    // Wait for the response (we set this directly after the message was sent
    // so we have the lock, before Erlang sends us data back)
    DRV_DEBUG("Waiting for response");
    drv_wait(this_import->response_ready, this_import->cond, &this_import->ready);
    DRV_DEBUG("Response ready");

    // Handle error in the response
    if (this_import->error_message) {
        DRV_DEBUG("Import execution failed. Error message: %s", proc->current_import->error_message);
        send_error(proc, "Import execution failed: %s", proc->current_import->error_message);
        return;
    }

    // Convert the response back to the function result, writing back to the args pointer
    int res = erl_terms_to_import_results(result_count, result_kinds, args, this_import->result_terms);
    if(res == -1) {
        DRV_DEBUG("Failed to convert terms to wasm vals");
        send_error(proc, "Failed to convert terms to wasm vals");
        return;
    }

    // Clean up
    DRV_DEBUG("Destroying %s", erl_drv_cond_name(this_import->cond));
    erl_drv_cond_destroy(this_import->cond);
    DRV_DEBUG("Destroying %s", erl_drv_mutex_name(this_import->response_ready));
    erl_drv_mutex_destroy(this_import->response_ready);
    DRV_DEBUG("Cond and mutex destroyed");
    DRV_DEBUG("Cleaning up import response");
    driver_free(this_import);
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
    DRV_DEBUG("Loading proto-module");
    DRV_DEBUG("wasm_runtime_load(%p, %zu, %p, %zu)", binary_copy, mod_bin->size, error_buf, 1024);
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

                wasm_valkind_t *param_kinds = driver_alloc(sizeof(wasm_valkind_t) * param_count);
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

                uint32_t result_count = wasm_func_type_get_result_count(type);
                DRV_DEBUG("Result count: %d", result_count);

                wasm_valkind_t *result_kinds = driver_alloc(sizeof(wasm_valkind_t) * result_count);
                if (!result_kinds && result_count > 0) {
                    DRV_DEBUG("Failed to allocate memory for result_kinds for %s.%s", module_name, name);
                    // TODO: Handle allocation failure
                    continue; // Skip this symbol
                }
                for (uint32_t r_idx = 0; r_idx < result_count; ++r_idx) {
                    result_kinds[r_idx] = wasm_func_type_get_result_valkind(type, r_idx);
                    DRV_DEBUG("Result %d kind: %d", r_idx, result_kinds[r_idx]);
                }

                if(!get_function_sig(param_count, param_kinds, result_count, result_kinds, type_str)) {
                    DRV_DEBUG("Failed to get function signature for %s.%s", module_name, name);
                     if (param_kinds) driver_free(param_kinds);
                     driver_free(type_str);
                    // TODO: Handle other types of imports?
                    continue; // Skip this symbol
                } else {
                    DRV_DEBUG("Got function signature for %s.%s: %s", module_name, name, type_str);
                }

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
                attachment->proc = proc;
                attachment->module_name = module_name; // Potential dangling pointer if module_proto is freed
                attachment->field_name = name;       // Potential dangling pointer if module_proto is freed
                attachment->signature = type_str;      // Keep allocated type_str, free later
                attachment->param_kinds = param_kinds;
                attachment->result_kinds = result_kinds;
                attachment->param_count = param_count;
                attachment->result_count = result_count;

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

    init_msg[msg_i++] = ERL_DRV_NIL;
    init_msg[msg_i++] = ERL_DRV_LIST;
    init_msg[msg_i++] = import_count + 1;

    DRV_DEBUG("Processing exports");
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

            uint32_t result_count = wasm_func_type_get_result_count(type);
            DRV_DEBUG("Result count: %d", result_count);

            enum wasm_valkind_enum *result_kinds = driver_alloc(sizeof(enum wasm_valkind_enum) * result_count);
            if (!result_kinds && result_count > 0) {
                DRV_DEBUG("Failed to allocate memory for result_kinds for %s", name);
                continue; // Skip this symbol
            }

            if(!get_function_sig(param_count, param_kinds, result_count, result_kinds, type_str)) {
                DRV_DEBUG("Failed to get function signature for %s", name);
                if (param_kinds) driver_free(param_kinds);
                if (result_kinds) driver_free(result_kinds);
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

    DRV_DEBUG("Cleaning up proto-module");
    wasm_module_delete(&module_proto);
    wasm_runtime_destroy();

    DRV_DEBUG("Reinitializing runtime");
    if (!wasm_runtime_full_init(&init_args)) {
        DRV_DEBUG("Failed to reinitialize runtime");
        send_error(proc, "Failed to reinitialize runtime");
        drv_unlock(proc->is_running);
        return;
    }

    DRV_DEBUG("Registering native symbols");
    for (int i = 0; i < found_import_modules_count; ++i) {
        ImportModuleSymbols import_module_symbols = import_modules_symbols[i];
        DRV_DEBUG("Registering native symbols for import module[%d]: (%d symbols) %s", i, import_module_symbols.count, import_module_symbols.module_name);
        for (int j = 0; j < import_module_symbols.count; ++j) {
            DRV_DEBUG("  - %d: %s %s.%s", j, ((NativeSymbolAttachment *)(import_module_symbols.symbols[j].attachment))->signature, import_module_symbols.module_name, import_module_symbols.symbols[j].symbol);
        }

        wasm_runtime_register_natives_raw(import_module_symbols.module_name, import_module_symbols.symbols, import_module_symbols.count);
    }

    DRV_DEBUG("Loading runtime-module");
    wasm_module_t module_runtime = wasm_runtime_load(mod_bin->binary, mod_bin->size, error_buf, 1024);
    if (!module_runtime) {
        DRV_DEBUG("Failed to load WASM runtime-module: %s", error_buf);
        send_error(proc, "Failed to load WASM runtime-module: %s", error_buf);
        drv_unlock(proc->is_running);
        return;
    }
    DRV_DEBUG("Created runtime-module: %p", module_runtime);
    proc->module = module_runtime;

    wasm_module_inst_t module_inst = wasm_runtime_instantiate(module_runtime, 0x10000, 0x10000, error_buf, 1024);
    if (!module_inst) {
        DRV_DEBUG("Failed to instantiate WASM runtime-module: %s", error_buf);
        send_error(proc, "Failed to instantiate WASM runtime-module: %s", error_buf);
        drv_unlock(proc->is_running);
        return;
    }
    DRV_DEBUG("Created runtime-module instance: %p", module_inst);
    proc->instance = module_inst;

    wasm_table_inst_t indirect_func_table;
    if (!wasm_runtime_get_export_table_inst(module_inst, "__indirect_function_table", &indirect_func_table)) {
        DRV_DEBUG("Failed to find __indirect_function_table");
        // send_error(proc, "Failed to get indirect function table");
        // drv_unlock(proc->is_running);
        // return;
    } else {
        DRV_DEBUG("Found __indirect_function_table");
        proc->indirect_func_table = indirect_func_table;
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

void wasm_execute_exported_function(void* raw) {
    Proc* proc = (Proc*)raw;
    DRV_DEBUG("Calling exported function: %s", proc->current_function);
    drv_lock(proc->is_running);
    char* function_name = proc->current_function;

    // Find the function in the exports
    wasm_function_inst_t* func = wasm_runtime_lookup_function(proc->instance, function_name);
    if (!func) {
        send_error(proc, "Function not found: %s", function_name);
        drv_unlock(proc->is_running);
        return;
    }
    DRV_DEBUG("Func: %p", func);

    const uint32_t param_count = wasm_func_get_param_count(func, proc->instance);
    wasm_valkind_t *param_types = driver_alloc(sizeof(wasm_valkind_t) * param_count);
    wasm_func_get_param_types(func, proc->instance, param_types);
    DRV_DEBUG("Param types: %p", param_types);

    const uint32_t result_count = wasm_func_get_result_count(func, proc->instance);
    wasm_valkind_t *result_types = driver_alloc(sizeof(wasm_valkind_t) * result_count);
    wasm_func_get_result_types(func, proc->instance, result_types);
    DRV_DEBUG("Result types: %p", result_types);

    wasm_val_vec_t args, results;
    wasm_val_vec_new_uninitialized(&args, param_count);
    wasm_val_vec_new_uninitialized(&results, result_count);
    
    for(int i = 0; i < param_count; i++) {
        args.data[i].kind = param_types[i];
    }

    for(int i = 0; i < result_count; i++) {
        results.data[i].kind = result_types[i];
    }
    
    if (!(erl_terms_to_wasm_vals(&args, proc->current_args) == 0)) {
        send_error(proc, "Failed to convert terms to wasm vals");
        drv_unlock(proc->is_running);
        return;
    }

    proc->exec_env = wasm_runtime_create_exec_env(proc->instance, 0x10000);

    // Call the function
    DRV_DEBUG("wasm_runtime_call_wasm_a: %s", function_name);
    bool call_success = wasm_runtime_call_wasm_a(proc->exec_env, func, results.size, results.data, args.size, args.data);

    if (!call_success) {
        const char *exception = wasm_runtime_get_exception(proc->instance);
        send_error(proc, "Call to %s failed with exception: %s", function_name, exception);
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
    
    // This should already be set to NULL by the import handler
    proc->current_import = NULL;

	DRV_DEBUG("Unlocking is_running mutex: %p", proc->is_running);
    drv_unlock(proc->is_running);
}

void wasm_execute_indirect_function(void *raw) {
    DRV_DEBUG("=================================================");
    DRV_DEBUG("Starting indirect function invocation");
    DRV_DEBUG("=================================================");
    // Proc* proc, const char *field_name, const wasm_val_vec_t* input_args, wasm_val_vec_t* output_results
    
    int result = 0;

    Proc* proc = (Proc*)raw;
    long function_ix = proc->current_function_ix;

    DRV_DEBUG("Function index: %ld", function_ix);
    DRV_DEBUG("Current args: %p", proc->current_args);

    // Look up the indirect function table
    wasm_table_inst_t indirect_function_table = proc->indirect_func_table;
    wasm_function_inst_t* function_ref = wasm_table_get_func_inst(proc->instance, &indirect_function_table, function_ix);
    DRV_DEBUG("Function ref: %p", function_ref);

    uint32_t param_count = wasm_func_get_param_count(function_ref, proc->instance);
    DRV_DEBUG("Param count: %d", param_count);
    wasm_valkind_t *param_types = driver_alloc(sizeof(wasm_valkind_t) * param_count);
    wasm_func_get_param_types(function_ref, proc->instance, param_types);
    DRV_DEBUG("Param types: %p", param_types);
    for (int i = 0; i < param_count; i++) {
        DRV_DEBUG("  - Param type %d: %d", i, param_types[i]);
    }

    uint32_t result_count = wasm_func_get_result_count(function_ref, proc->instance);
    DRV_DEBUG("Result count: %d", result_count);
    wasm_valkind_t *result_types = driver_alloc(sizeof(wasm_valkind_t) * result_count);    
    wasm_func_get_result_types(function_ref, proc->instance, result_types);
    DRV_DEBUG("Result types: %p", result_types);
    for (int i = 0; i < result_count; i++) {
        DRV_DEBUG("  - Result type %d: %d", i, result_types[i]);
    }

    uint32_t param_size = kinds_size(param_types, param_count);
    uint32_t result_size = kinds_size(result_types, result_count);
    DRV_DEBUG("Param size: %d", param_size);
    DRV_DEBUG("Result size: %d", result_size);
    // argc should be the max of param_size and result_size
    uint32_t argc = param_size > result_size ? param_size : result_size;
    uint32_t* argv = driver_alloc(sizeof(uint32_t) * argc);
    DRV_DEBUG("Converting terms to indirect args");
    if (erl_terms_to_indirect_args(argv, param_types, param_count, proc->current_args) == -1) {
        DRV_DEBUG("Failed to convert terms to indirect args");
        send_error(proc, "Failed to convert terms to indirect args");
        drv_unlock(proc->is_running);
        return;
    }

    bool res = wasm_runtime_call_indirect(proc->exec_env, function_ix, argc, argv);

    // // Prepare the arguments for the function call
    // wasm_val_vec_t prepared_args;
    //     // If there are no arguments or only one argument (function index), no preparation is needed
    // if (input_args->size <= 1) {
    //     DRV_DEBUG("Not enough arguments to create new wasm_val_vec_t");
    //     send_error(proc, "Not enough arguments to create new wasm_val_vec_t");
    //     drv_unlock(proc->is_running);
    //     return;
    // }

    // // Allocate memory for the prepared arguments
    // wasm_val_t* prepared_data = malloc(sizeof(wasm_val_t) * (input_args->size - 1));

    // // Copy the arguments starting from the second element (skip function index)
    // for (size_t i = 1; i < input_args->size; ++i) {
    //     prepared_data[i - 1] = input_args->data[i];
    // }

    // // Create a new wasm_val_vec_t with the prepared arguments
    // wasm_val_vec_new(&prepared_args, input_args->size - 1, prepared_data);
    // DRV_DEBUG("Prepared %zu arguments for function call", prepared_args.size);

    // uint64_t argc = prepared_args.size;
    // uint64_t* argv = malloc(sizeof(uint64_t) * argc);
    
    // // Convert prepared arguments to an array of 64-bit integers
    // for (uint64_t i = 0; i < argc; ++i) {
    //     argv[i] = prepared_args.data[i].of.i64;
    // }

    // // Attempt to call the function and check for any exceptions
    // if (!wasm_runtime_call_indirect(proc->exec_env, function_index, argc, argv)) {
    //     if (wasm_runtime_get_exception(proc->exec_env)) {
    //         DRV_DEBUG("%s", wasm_runtime_get_exception(proc->exec_env));
    //     }
    //     DRV_DEBUG("WASM function call failed");
    //     send_error(proc, "WASM function call failed");
    //     drv_unlock(proc->is_running);
    //     return;
    // }

        // Send the results back to Erlang
    DRV_DEBUG("Results size: %d", result_count);
    ErlDrvTermData* msg = driver_alloc(sizeof(ErlDrvTermData) * (7 + (result_count * 2)));
    DRV_DEBUG("Allocated msg");
    int msg_index = 0;
    msg[msg_index++] = ERL_DRV_ATOM;
    msg[msg_index++] = atom_execution_result;
    for (size_t i = 0; i < result_count; i++) {
        DRV_DEBUG("Processing result %d", i);
        DRV_DEBUG("Result type: %d", result_types[i]);
        int res_size = import_arg_to_erl_term(&msg[msg_index], result_types[i], (uint64_t *)&(argv[i]));
        msg_index += res_size;
    }
    msg[msg_index++] = ERL_DRV_NIL;
    msg[msg_index++] = ERL_DRV_LIST;
    msg[msg_index++] = result_count + 1;
    msg[msg_index++] = ERL_DRV_TUPLE;
    msg[msg_index++] = 2;
    DRV_DEBUG("Sending %d terms", msg_index);
    int response_msg_res = erl_drv_output_term(proc->port_term, msg, msg_index);
    driver_free(msg);
    DRV_DEBUG("Msg: %d", response_msg_res);

    // // Free allocated memory
    // free(argv);
    // free(prepared_args.data);
    DRV_DEBUG("Indirect function call completed successfully");
}
