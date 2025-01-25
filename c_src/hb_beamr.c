#include <erl_driver.h>
#include <ei.h>
#include <wasm_c_api.h>
#include <wasm_export.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <pthread.h>

typedef struct {
    ErlDrvMutex* response_ready;
    ErlDrvCond* cond;
    int ready;
    char* error_message;
    ei_term* result_terms;
    int result_length;
} ImportResponse;

typedef struct {
    wasm_engine_t* engine;
    wasm_instance_t* instance;
    wasm_module_t* module;
    wasm_store_t* store;
    ErlDrvPort port;
    ErlDrvTermData port_term;
    ErlDrvMutex* is_running;
    char* current_function;
    ei_term* current_args;
    int current_args_length;
    ImportResponse* current_import;
    ErlDrvTermData pid;
    int is_initialized;
    time_t start_time;
} Proc;

typedef struct {
    char* module_name;
    char* field_name;
    char* signature;
    Proc* proc;
    wasm_func_t* stub_func;
} ImportHook;

typedef struct {
    void* binary;
    long size;
    Proc* proc;
} LoadWasmReq;

static ErlDrvTermData atom_ok;
static ErlDrvTermData atom_error;
static ErlDrvTermData atom_import;
static ErlDrvTermData atom_execution_result;

#ifndef HB_DEBUG
#define HB_DEBUG 0
#endif

#define DRV_DEBUG(format, ...) beamr_print(HB_DEBUG, __FILE__, __LINE__, format, ##__VA_ARGS__)
#define DRV_PRINT(format, ...) beamr_print(1, __FILE__, __LINE__, format, ##__VA_ARGS__)

void beamr_print(int print, const char* file, int line, const char* format, ...) {
    va_list args;
    va_start(args, format);
    if(print) {
        pthread_t thread_id = pthread_self();
        printf("[DBG#%p @ %s:%d] ", thread_id, file, line);
        vprintf(format, args);
        printf("\r\n");
    }
    va_end(args);
}

const char* wasm_externtype_to_kind_string(const wasm_externtype_t* type) {
    switch (wasm_externtype_kind(type)) {
        case WASM_EXTERN_FUNC: return "func";
        case WASM_EXTERN_GLOBAL: return "global";
        case WASM_EXTERN_TABLE: return "table";
        case WASM_EXTERN_MEMORY: return "memory";
        default: return "unknown";
    }
}

// Helper function to convert wasm_valtype_t to char
char wasm_valtype_kind_to_char(const wasm_valtype_t* valtype) {
    switch (wasm_valtype_kind(valtype)) {
        case WASM_I32: return 'i';
        case WASM_I64: return 'I';
        case WASM_F32: return 'f';
        case WASM_F64: return 'F';
        case WASM_EXTERNREF: return 'e';
        case WASM_V128: return 'v';
        case WASM_FUNCREF: return 'f';
        default: return 'u';
    }
}

int wasm_val_to_erl_term(ErlDrvTermData* term, const wasm_val_t* val) {
    DRV_DEBUG("Adding wasm val to erl term");
    DRV_DEBUG("Val of: %d", val->of.i32);
    switch (val->kind) {
        case WASM_I32:
            term[0] = ERL_DRV_INT;
            term[1] = val->of.i32;
            return 2;
        case WASM_I64:
            term[0] = ERL_DRV_INT64;
            term[1] = (ErlDrvTermData) &val->of.i64;
            return 2;
        case WASM_F32:
            term[0] = ERL_DRV_FLOAT;
            term[1] = (ErlDrvTermData) &val->of.f32;
            return 2;
        case WASM_F64:
            term[0] = ERL_DRV_FLOAT;
            term[1] = (ErlDrvTermData) &val->of.f64;
            return 2;
        default:
            DRV_DEBUG("Unsupported result type: %d", val->kind);
            return 0;
    }
}

int erl_term_to_wasm_val(wasm_val_t* val, ei_term* term) {
    DRV_DEBUG("Converting erl term to wasm val. Term: %d. Size: %d", term->value.i_val, term->size);
    switch (val->kind) {
        case WASM_I32:
            val->of.i32 = (int) term->value.i_val;
            break;
        case WASM_I64:
            val->of.i64 = (long) term->value.i_val;
            break;
        case WASM_F32:
            val->of.f32 = (float) term->value.d_val;
            break;
        case WASM_F64:
            val->of.f64 = term->value.d_val;
            break;
        default:
            DRV_DEBUG("Unsupported parameter type: %d", val->kind);
            return -1;
    }
    return 0;
}

int erl_terms_to_wasm_vals(wasm_val_vec_t* vals, ei_term* terms) {
    DRV_DEBUG("Converting erl terms to wasm vals");
    DRV_DEBUG("Vals: %d", vals->size);
    for(int i = 0; i < vals->size; i++) {
        DRV_DEBUG("Converting term %d: %p", i, &vals->data[i]);
        int res = erl_term_to_wasm_val(&vals->data[i], &terms[i]);
        if(res == -1) {
            DRV_DEBUG("Failed to convert term to wasm val");
            return -1;
        }
    }
    return 0;
}

ei_term* decode_list(char* buff, int* index) {
    int arity, type;

    if(ei_get_type(buff, index, &type, &arity) == -1) {
        DRV_DEBUG("Failed to get type");
        return NULL;
    }
    DRV_DEBUG("Decoded header. Arity: %d", arity);

    ei_term* res = driver_alloc(sizeof(ei_term) * arity);

    if(type == ERL_LIST_EXT) {
        //DRV_DEBUG("Decoding list");
        ei_decode_list_header(buff, index, &arity);
        //DRV_DEBUG("Decoded list header. Arity: %d", arity);
        for(int i = 0; i < arity; i++) {
            ei_decode_ei_term(buff, index, &res[i]);
            DRV_DEBUG("Decoded term (assuming int) %d: %d", i, res[i].value.i_val);
        }
    }
    else if(type == ERL_STRING_EXT) {
        //DRV_DEBUG("Decoding list encoded as string");
        unsigned char* str = driver_alloc(arity * sizeof(char) + 1);
        ei_decode_string(buff, index, str);
        for(int i = 0; i < arity; i++) {
            res[i].ei_type = ERL_INTEGER_EXT;
            res[i].value.i_val = (long) str[i];
            DRV_DEBUG("Decoded term %d: %d", i, res[i].value.i_val);
        }
        driver_free(str);
    }
    else {
        DRV_DEBUG("Unknown type: %d", type);
        return NULL;
    }

    return res;
}

int get_function_sig(const wasm_externtype_t* type, char* type_str) {
    if (wasm_externtype_kind(type) == WASM_EXTERN_FUNC) {
        const wasm_functype_t* functype = wasm_externtype_as_functype_const(type);
        const wasm_valtype_vec_t* params = wasm_functype_params(functype);
        const wasm_valtype_vec_t* results = wasm_functype_results(functype);

        if(!params || !results) {
            DRV_DEBUG("Export function params/results are NULL");
            return 0;
        }

        type_str[0] = '(';
        size_t offset = 1;

        for (size_t i = 0; i < params->size; i++) {
            type_str[offset++] = wasm_valtype_kind_to_char(params->data[i]);
        }
        type_str[offset++] = ')';

        for (size_t i = 0; i < results->size; i++) {
            type_str[offset++] = wasm_valtype_kind_to_char(results->data[i]);
        }
        type_str[offset] = '\0';

        return 1;
    }
    return 0;
}

void drv_lock(ErlDrvMutex* mutex) {
    DRV_DEBUG("Locking: %s", erl_drv_mutex_name(mutex));
    erl_drv_mutex_lock(mutex);
    DRV_DEBUG("Locked: %s", erl_drv_mutex_name(mutex));
}

void drv_unlock(ErlDrvMutex* mutex) {
    DRV_DEBUG("Unlocking: %s", erl_drv_mutex_name(mutex));
    erl_drv_mutex_unlock(mutex);
    DRV_DEBUG("Unlocked: %s", erl_drv_mutex_name(mutex));
}

void drv_signal(ErlDrvMutex* mut, ErlDrvCond* cond, int* ready) {
    DRV_DEBUG("Signaling: %s. Pre-signal ready state: %d", erl_drv_cond_name(cond), *ready);
    drv_lock(mut);
    *ready = 1;
    erl_drv_cond_signal(cond);
    drv_unlock(mut);
    DRV_DEBUG("Signaled: %s. Post-signal ready state: %d", erl_drv_cond_name(cond), *ready);
}

void drv_wait(ErlDrvMutex* mut, ErlDrvCond* cond, int* ready) {
    DRV_DEBUG("Started to wait: %s. Ready: %d", erl_drv_cond_name(cond), *ready);
    DRV_DEBUG("Mutex: %s", erl_drv_mutex_name(mut));
    drv_lock(mut);
    while (!*ready) {
        DRV_DEBUG("Waiting: %s", erl_drv_cond_name(cond));
        erl_drv_cond_wait(cond, mut);
        DRV_DEBUG("Woke up: Ready: %d", *ready);
    }
    drv_unlock(mut);
    DRV_DEBUG("Finish waiting: %s", erl_drv_cond_name(cond));
}

wasm_func_t* get_exported_function(Proc* proc, const char* target_name) {
    wasm_extern_vec_t exports;
    wasm_instance_exports(proc->instance, &exports);
    wasm_exporttype_vec_t export_types;
    wasm_module_exports(proc->module, &export_types);
    wasm_func_t* func = NULL;

    for (size_t i = 0; i < exports.size; ++i) {
        wasm_extern_t* ext = exports.data[i];
        if (wasm_extern_kind(ext) == WASM_EXTERN_FUNC) {
            const wasm_name_t* exp_name = wasm_exporttype_name(export_types.data[i]);
            if (exp_name && exp_name->size == strlen(target_name) + 1 && 
                strncmp(exp_name->data, target_name, exp_name->size - 1) == 0) {
                func = wasm_extern_as_func(ext);
                break;
            }
        }
    }

    return func;
}

wasm_memory_t* get_memory(Proc* proc) {
    wasm_extern_vec_t exports;
    wasm_instance_exports(proc->instance, &exports);
    for (size_t i = 0; i < exports.size; i++) {
        if (wasm_extern_kind(exports.data[i]) == WASM_EXTERN_MEMORY) {
            return wasm_extern_as_memory(exports.data[i]);
        }
    }
    return NULL;
}

long get_memory_size(Proc* proc) {
    wasm_memory_t* memory = get_memory(proc);
    return wasm_memory_size(memory) * 65536;
}

void send_error(Proc* proc, const char* message_fmt, ...) {
    va_list args;
    va_start(args, message_fmt);
    char* message = driver_alloc(256);
    vsnprintf(message, 256, message_fmt, args);
    DRV_DEBUG("Sending error message: %s", message);
    ErlDrvTermData* msg = driver_alloc(sizeof(ErlDrvTermData) * 7);
    int msg_index = 0;
    msg[msg_index++] = ERL_DRV_ATOM;
    msg[msg_index++] = atom_error;
    msg[msg_index++] = ERL_DRV_STRING;
    msg[msg_index++] = (ErlDrvTermData)message;
    msg[msg_index++] = strlen(message);
    msg[msg_index++] = ERL_DRV_TUPLE;
    msg[msg_index++] = 2;

    int msg_res = erl_drv_output_term(proc->port_term, msg, msg_index);
    DRV_DEBUG("Sent error message. Res: %d", msg_res);
    va_end(args);
}

wasm_trap_t* generic_import_handler(void* env, const wasm_val_vec_t* args, wasm_val_vec_t* results) {
    DRV_DEBUG("generic_import_handler called");
    ImportHook* import_hook = (ImportHook*)env;
    Proc* proc = import_hook->proc;
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
    // Clean up the result_terms of current_import as it's not cleaned automatically
    driver_free(proc->current_import->result_terms);
    driver_free(proc->current_import);

    proc->current_import = NULL;
    return NULL;
}

// Async initialization function
static void async_init(void* raw) {
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

    wasm_runtime_set_default_running_mode(Mode_Interp);

    proc->engine = wasm_engine_new();
    DRV_DEBUG("Created engine");
    proc->store = wasm_store_new(proc->engine);
    DRV_DEBUG("Created store");

    // Load WASM module
    wasm_byte_vec_t binary;
    wasm_byte_vec_new(&binary, mod_bin->size, (const wasm_byte_t*)mod_bin->binary);

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

    // Get exports
    wasm_exporttype_vec_t exports;
    wasm_module_exports(proc->module, &exports);

    // Create Erlang lists for imports
    //DRV_DEBUG("Exports size: %d", exports.size);
    ErlDrvTermData* init_msg = driver_alloc(sizeof(ErlDrvTermData) * (2 + (13 * imports.size) + (11 * exports.size)));
    //DRV_DEBUG("Allocated init message");
    int msg_i = 0;
    init_msg[msg_i++] = ERL_DRV_ATOM;
    init_msg[msg_i++] = atom_execution_result;

    // Process imports
    for (int i = 0; i < imports.size; ++i) {
        //DRV_DEBUG("Processing import %d", i);
        const wasm_importtype_t* import = imports.data[i];
        const wasm_name_t* module_name = wasm_importtype_module(import);
        const wasm_name_t* name = wasm_importtype_name(import);
        const wasm_externtype_t* type = wasm_importtype_type(import);

        //DRV_DEBUG("Import: %s.%s", module_name->data, name->data);

        char* type_str = driver_alloc(256);
        // TODO: What happpens here?
        if(!get_function_sig(type, type_str)) {
            // TODO: Handle other types of imports?
            continue;
        }

        init_msg[msg_i++] = ERL_DRV_ATOM;
        init_msg[msg_i++] = driver_mk_atom((char*)wasm_externtype_to_kind_string(type));
        init_msg[msg_i++] = ERL_DRV_STRING;
        init_msg[msg_i++] = (ErlDrvTermData)module_name->data;
        init_msg[msg_i++] = module_name->size - 1;
        init_msg[msg_i++] = ERL_DRV_STRING;
        init_msg[msg_i++] = (ErlDrvTermData)name->data;
        init_msg[msg_i++] = name->size - 1;
        init_msg[msg_i++] = ERL_DRV_STRING;
        init_msg[msg_i++] = (ErlDrvTermData)type_str;
        init_msg[msg_i++] = strlen(type_str);
        init_msg[msg_i++] = ERL_DRV_TUPLE;
        init_msg[msg_i++] = 4;

        DRV_DEBUG("Creating callback for %s.%s [%s]", module_name->data, name->data, type_str);
        ImportHook* hook = driver_alloc(sizeof(ImportHook));
        hook->module_name = module_name->data;
        hook->field_name = name->data;
        hook->proc = proc;
        hook->signature = type_str;

        hook->stub_func =
            wasm_func_new_with_env(
                proc->store,
                wasm_externtype_as_functype_const(type),
                generic_import_handler,
                hook,
                NULL
            );
        stubs[i] = wasm_func_as_extern(hook->stub_func);
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

    // Refresh the exports now that we have an instance
    wasm_module_exports(proc->module, &exports);
    for (size_t i = 0; i < exports.size; i++) {
        //DRV_DEBUG("Processing export %d", i);
        const wasm_exporttype_t* export = exports.data[i];
        const wasm_name_t* name = wasm_exporttype_name(export);
        const wasm_externtype_t* type = wasm_exporttype_type(export);
        char* kind_str = (char*) wasm_externtype_to_kind_string(type);

        char* type_str = driver_alloc(256);
        get_function_sig(type, type_str);
        DRV_DEBUG("Export: %s [%s] -> %s", name->data, kind_str, type_str);

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

    init_msg[msg_i++] = ERL_DRV_NIL;
    init_msg[msg_i++] = ERL_DRV_LIST;
    init_msg[msg_i++] = (exports.size) + 1;
    init_msg[msg_i++] = ERL_DRV_TUPLE;
    init_msg[msg_i++] = 3;

    DRV_DEBUG("Sending init message to Erlang. Elements: %d", msg_i);

    int send_res = erl_drv_output_term(proc->port_term, init_msg, msg_i);
    DRV_DEBUG("Send result: %d", send_res);

    proc->current_import = NULL;
    proc->is_initialized = 1;
    drv_unlock(proc->is_running);
}

static void async_call(void* raw) {
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

    drv_unlock(proc->is_running);
}

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
        int size, type;
        ei_get_type(buff, &index, &type, &size);
        //DRV_DEBUG("WASM binary size: %d bytes. Type: %c", size, type);
        void* wasm_binary = driver_alloc(size);
        long size_l = (long)size;
        ei_decode_binary(buff, &index, wasm_binary, &size_l);
        LoadWasmReq* mod_bin = driver_alloc(sizeof(LoadWasmReq));
        mod_bin->proc = proc;
        mod_bin->binary = wasm_binary;
        mod_bin->size = size;
        //DRV_DEBUG("Calling for async thread to init");
        driver_async(proc->port, NULL, async_init, mod_bin, NULL);
    } else if (strcmp(command, "call") == 0) {
        if (!proc->is_initialized) {
            send_error(proc, "Cannot run WASM function as module not initialized.");
            return;
        }
        // Extract the function name and the args from the Erlang term and generate the wasm_val_vec_t
        char* function_name = driver_alloc(MAXATOMLEN);
        ei_decode_string(buff, &index, function_name);
        //DRV_DEBUG("Function name: %s", function_name);
        proc->current_function = function_name;

        //DRV_DEBUG("Decoding args. Buff: %p. Index: %d", buff, index);
        proc->current_args = decode_list(buff, &index);

        driver_async(proc->port, NULL, async_call, proc, NULL);
    } else if (strcmp(command, "import_response") == 0) {
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