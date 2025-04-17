#include "include/hb_wasm.h"
#include "include/hb_logging.h"
#include "include/hb_helpers.h"
#include "include/hb_driver.h"

extern ErlDrvTermData atom_ok;
extern ErlDrvTermData atom_import;
extern ErlDrvTermData atom_execution_result;

wasm_trap_t* wasm_handle_import(void* env, const wasm_val_vec_t* args, wasm_val_vec_t* results) {
    DRV_DEBUG("generic_import_handler called");
    ImportHook* import_hook = (ImportHook*)env;
    Proc* proc = import_hook->proc;

    DRV_DEBUG("Proc: %p. Args size: %d", proc, args->size);
    DRV_DEBUG("Import name: %s.%s [%s]", import_hook->module_name, import_hook->field_name, import_hook->signature);

    // Check if the import name matches "env.invoke_*", if so, call that instead
    if (strcmp(import_hook->module_name, "env") == 0 && strncmp(import_hook->field_name, "invoke_", 7) == 0) {
		DRV_DEBUG("Import is indirect function: %s", import_hook->field_name);
        wasm_execute_indirect_function(proc, import_hook->field_name, args, results); 
        return NULL;
    }

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

/**
 * Safely copy a flat struct from a WASM instance's linear memory into native
 * memory.
 *
 * \param memory    Pointer to the WASM memory (obtained from wasm_instance).
 * \param offset    Offset (index) within WASM memory where the struct starts
 * (i.e., the struct pointer as i32 from WASM).
 * \param out_struct Pointer to a native struct where data will be copied.
 * \param struct_size Size (in bytes) of the struct to copy.
 * \param align     Required alignment of the struct (in bytes, e.g., 4 for
 * 32-bit alignment). Use 1 if no specific alignment needed.
 * \return 0 on success, 1 on failure (with an error reported via
 * WASM_MEMORY_ERROR macro).
 *
 * This function performs bounds checking to ensure the entire struct lies
 * within the WASM memory, and also checks alignment if `align` > 1. It uses
 * `wasm_memory_data` to get a direct pointer to the WASM memory buffer and then
 * copies `struct_size` bytes into the native struct.
 */
int wasm_copy_struct_safe(wasm_memory_t *memory, uint32_t offset,
						  void *out_struct, size_t struct_size, size_t align)
{
	assert(memory != NULL);
	assert(out_struct != NULL);

	// Get the start of the memory and its size.
	byte_t *mem_base = wasm_memory_data(memory);
	size_t mem_size = wasm_memory_data_size(memory);

	// Bounds checking: ensure [offset, offset+struct_size) is within memory.
	// Also protect against overflow in pointer arithmetic.
	if (offset > mem_size || struct_size > mem_size ||
		offset > mem_size - struct_size)
	{
		DRV_DEBUG("Attempt to read struct out of WASM memory bounds");
		return 1;
	}

	// Alignment check (if required).
	// If the struct in WASM memory is expected to be aligned (e.g., naturally
	// aligned), verify the offset meets the alignment requirement. This can
	// catch misaligned pointers.
	if (align > 1 && (offset % align) != 0)
	{
		return 1;
	}

	// Copy bytes from WASM memory into the native struct.
	memcpy(out_struct, mem_base + offset, struct_size);
	return 0;
}

char *wasm_copy_string_len_safe(wasm_memory_t *memory, uint32_t str_ptr,
								size_t str_len)
{
	if (str_ptr == 0)
		return NULL;

	byte_t *mem_base = wasm_memory_data(memory);
	size_t mem_size = wasm_memory_data_size(memory);

	// Bounds check the initial pointer
	if (str_ptr >= mem_size)
	{
		DRV_DEBUG("String pointer out of bounds");
		return NULL;
	}

	if (str_len > mem_size - str_ptr)
	{
		DRV_DEBUG("String length out of bounds");
		return NULL;
	}

	// Get string length safely
	const char *wasm_str = (const char *)(mem_base + str_ptr);

	// Allocate and copy
	char *native_str = malloc(str_len + 1);
	if (!native_str)
	{
		DRV_DEBUG("Failed to allocate memory for string");
		return NULL;
	}

	memcpy(native_str, wasm_str, str_len);
	native_str[str_len] = '\0';
	return native_str;
}

typedef struct
{
	int32_t buf_ptr;
	int32_t buf_len;
} WASMIOVec;

wasm_trap_t *fd_write_import(void *env, const wasm_val_vec_t *args,
							 wasm_val_vec_t *results)
{
	DRV_DEBUG("fd_write_import called");
	ImportHook *import_hook = (ImportHook *)env;
	Proc *proc = import_hook->proc;
	wasm_store_t *store = proc->store;
	wasm_memory_t *wasm_memory = get_memory(proc);
	byte_t *wasm_base_ptr = wasm_memory_data(wasm_memory);
	int args_expected = 4;
	if (args->size != args_expected)
	{
		DRV_DEBUG("Incorrect number of arguments: expected %d, got %d", args_expected, (int)args->size);
		const char *errMsg = "fd_write: Incorrect number of arguments provided";
		wasm_byte_vec_t message;
		wasm_byte_vec_new(&message, strlen(errMsg), errMsg);
		return wasm_trap_new(proc->store, &message);
	}

	// extract parameters
	int32_t fd = args->data[0].of.i32;
	int32_t iovs = args->data[1].of.i32;
	int32_t iovs_len = args->data[2].of.i32;
	int32_t pnum = args->data[3].of.i32;

	DRV_DEBUG("fd=%d, iovs=%p, iovs_len=%d, pnum=%p", fd, iovs, iovs_len, pnum);

	int32_t written = 0;
	for (int i = 0; i < iovs_len; i++)
	{
		int32_t offset = sizeof(WASMIOVec) * i;

		WASMIOVec iovec;
		memset(&iovec, 0, sizeof(WASMIOVec));
		if (wasm_copy_struct_safe(wasm_memory, iovs + offset, &iovec,
								  sizeof(WASMIOVec), 4))
		{
			DRV_DEBUG("fd_write_import: failed to copy iov");
			return NULL;
		}
		// DRV_DEBUG("fd_write_import: iovec1.buf_ptr=%p, iovec1.buf_len=%d", iovec.buf_ptr, iovec.buf_len);

		char *str = wasm_copy_string_len_safe(wasm_memory, iovec.buf_ptr,
											  iovec.buf_len);
		DRV_DEBUG("fd_write[%d]: %s", fd, str);

		written += iovec.buf_len;
	}

	// TODO, safely :)
	// DRV_DEBUG("fd_write_import: written=%d", written);
	memcpy(wasm_memory_data(wasm_memory) + pnum, (void *)&written, 4);

	// set error output as 0 (success)
	results->data[0].kind = WASM_I32;
	results->data[0].of.i32 = 0;

	return NULL;
}

void do_set_threw(Proc* proc, int32_t arg0, int32_t arg1) {
    DRV_DEBUG("do_set_threw called with values: %d, %d", arg0, arg1);

    wasm_val_vec_t args;
    wasm_val_vec_new_uninitialized(&args, 2);
    args.num_elems = 2;
    args.data[0].kind = WASM_I32;
    args.data[0].of.i32 = arg0;
    args.data[1].kind = WASM_I32;
    args.data[1].of.i32 = arg1;

    wasm_val_vec_t results;
    wasm_val_vec_new_uninitialized(&results, 0);

    int ret = wasm_execute_exported_function(proc, "setThrew", &args, &results);
    if (ret != 0) {
        DRV_DEBUG("Failed to call setThrew");
        return;
    }

    DRV_DEBUG("Successfully called setThrew");
}

// emscripten JS:

// class ExceptionInfo {
//  constructor(excPtr) {
//   this.excPtr = excPtr;
//   this.ptr = excPtr - 24;
//  }
//  set_type(type) {
//   HEAPU32[(((this.ptr) + (4)) >> 2)] = type;
//  }
//  get_type() {
//   return HEAPU32[(((this.ptr) + (4)) >> 2)];
//  }
//  set_destructor(destructor) {
//   HEAPU32[(((this.ptr) + (8)) >> 2)] = destructor;
//  }
//  get_destructor() {
//   return HEAPU32[(((this.ptr) + (8)) >> 2)];
//  }
//  set_caught(caught) {
//   caught = caught ? 1 : 0;
//   HEAP8[(this.ptr) + (12)] = caught;
//  }
//  get_caught() {
//   return HEAP8[(this.ptr) + (12)] != 0;
//  }
//  set_rethrown(rethrown) {
//   rethrown = rethrown ? 1 : 0;
//   HEAP8[(this.ptr) + (13)] = rethrown;
//  }
//  get_rethrown() {
//   return HEAP8[(this.ptr) + (13)] != 0;
//  }
//  init(type, destructor) {
//   this.set_adjusted_ptr(0);
//   this.set_type(type);
//   this.set_destructor(destructor);
//  }
//  set_adjusted_ptr(adjustedPtr) {
//   HEAPU32[(((this.ptr) + (16)) >> 2)] = adjustedPtr;
//  }
//  get_adjusted_ptr() {
//   return HEAPU32[(((this.ptr) + (16)) >> 2)];
//  }
//  get_exception_ptr() {
//   var isPointer = ___cxa_is_pointer_type(this.get_type());
//   if (isPointer) {
//    return HEAPU32[((this.excPtr) >> 2)];
//   }
//   var adjusted = this.get_adjusted_ptr();
//   if (adjusted !== 0) return adjusted;
//   return this.excPtr;
//  }
// }

wasm_trap_t *cxa_throw_import(void *env, const wasm_val_vec_t *args,
							 wasm_val_vec_t *results)
{
	DRV_DEBUG("cxa_throw_import called");
	ImportHook *import_hook = (ImportHook *)env;
	Proc *proc = import_hook->proc;
	wasm_store_t *store = proc->store;
	wasm_memory_t *wasm_memory = get_memory(proc);
	byte_t *wasm_base_ptr = wasm_memory_data(wasm_memory);
	int args_expected = 3;
	if (args->size != args_expected)
	{
		DRV_DEBUG("Incorrect number of arguments: expected %d, got %d", args_expected, (int)args->size);
		const char *errMsg = "cxa_throw: Incorrect number of arguments provided";
		wasm_byte_vec_t message;
		wasm_byte_vec_new(&message, strlen(errMsg), errMsg);
		return wasm_trap_new(proc->store, &message);
	}

    DRV_DEBUG("cxa_throw_import extract parameters");

	// extract parameters
	int32_t ptr_arg = args->data[0].of.i32;
	int32_t type = args->data[1].of.i32;
	int32_t destructor = args->data[2].of.i32;

	DRV_DEBUG("cxa_throw_import extract parameters: ptr_arg: %#x, type: %d, destructor: %d", ptr_arg, type, destructor);

    // var info = new ExceptionInfo(ptr);
    uint32_t exc_ptr = ptr_arg;
    uint32_t ptr = exc_ptr - 24;

    // info.init(type, destructor);
    // set_adjusted_ptr
    memcpy(&wasm_base_ptr[(ptr + 16)], &ptr, 4);
    // set_type
    memcpy(&wasm_base_ptr[(ptr + 4)], &type, 4);
    // set_destructor
    memcpy(&wasm_base_ptr[(ptr + 8)], &destructor, 4);

    char* exception_text = malloc(100);
    sprintf(exception_text, "__cxa_throw at %#x", ptr);

    DRV_DEBUG("Setting exception tip: %s", exception_text);

    bool result = stack_entry_set_exception_tip(proc, exception_text);
    DRV_DEBUG("stack_entry_set_exception_tip result: %d", result);

    // exceptionLast = ptr;
    proc->exception_ptr_last = exc_ptr;

    return NULL;
}

void do_set_temp_ret0(Proc *proc, int32_t value) {
    DRV_DEBUG("do_set_temp_ret0 called with value: %d", value);

    wasm_val_vec_t args;
    wasm_val_vec_new_uninitialized(&args, 1);
    args.num_elems = 1;
    args.data[0].kind = WASM_I32;
    args.data[0].of.i32 = value;

    wasm_val_vec_t results;
    wasm_val_vec_new_uninitialized(&results, 0);

    int ret = wasm_execute_exported_function(proc, "_emscripten_tempret_set", &args, &results);
    if (ret != 0) {
        DRV_DEBUG("Failed to call _emscripten_tempret_set");
        return;
    }

    DRV_DEBUG("Successfully called _emscripten_tempret_set");
}

wasm_trap_t *cxa_find_matching_catch_3_import(void *env, const wasm_val_vec_t *args,
							 wasm_val_vec_t *results)
{
	DRV_DEBUG("cxa_find_matching_catch_3_import called");
	ImportHook *import_hook = (ImportHook *)env;
	Proc *proc = import_hook->proc;
	wasm_store_t *store = proc->store;
	wasm_memory_t *wasm_memory = get_memory(proc);
	byte_t *wasm_base_ptr = wasm_memory_data(wasm_memory);
	int args_expected = 1;
	if (args->size != args_expected)
	{
		DRV_DEBUG("Incorrect number of arguments: expected %d, got %d", args_expected, (int)args->size);
		const char *errMsg = "cxa_find_matching_catch_3: Incorrect number of arguments provided";
		wasm_byte_vec_t message;
		wasm_byte_vec_new(&message, strlen(errMsg), errMsg);
		return wasm_trap_new(proc->store, &message);
	}

    DRV_DEBUG("cxa_find_matching_catch_3_import extract parameters");

	// extract parameters
	int32_t ptr_arg = args->data[0].of.i32;

    //  var thrown = exceptionLast;
    uint32_t thrown = proc->exception_ptr_last;

    //  if (!thrown) {
    //   setTempRet0(0);
    //   return 0;
    //  }

    if (!thrown) {
        DRV_DEBUG("No exception to find matching catch for");
        do_set_temp_ret0(proc, 0);
        results->data[0].kind = WASM_I32;
        results->data[0].of.i32 = 0;
        return NULL;
    }

    //  var info = new ExceptionInfo(thrown);
    uint32_t exc_ptr = thrown;
    uint32_t ptr = exc_ptr - 24;

    //  info.set_adjusted_ptr(thrown);
    memcpy(&wasm_base_ptr[(ptr + 16)], &thrown, 4);

    //  var thrownType = info.get_type();
    uint32_t thrown_type;
    memcpy(&thrown_type, &wasm_base_ptr[(ptr + 4)], 4);

    //  if (!thrownType) {
    //   setTempRet0(0);
    //   return thrown;
    //  }
    if (!thrown_type) {
        DRV_DEBUG("No thrown type found");
        do_set_temp_ret0(proc, 0);
        results->data[0].kind = WASM_I32;
        results->data[0].of.i32 = thrown;
        return NULL;
    }

    //  for (var arg in args) {
    //   var caughtType = args[arg];
    //   if (caughtType === 0 || caughtType === thrownType) {
    //    break;
    //   }
    //   var adjusted_ptr_addr = info.ptr + 16;
    //   if (___cxa_can_catch(caughtType, thrownType, adjusted_ptr_addr)) {
    //    setTempRet0(caughtType);
    //    return thrown;
    //   }
    //  }
    // TODO: Translate

    //  setTempRet0(thrownType);
    do_set_temp_ret0(proc, thrown_type);
    
    //  return thrown;
    results->data[0].kind = WASM_I32;
    results->data[0].of.i32 = thrown;

    return NULL;
}

void do_cxa_increment_exception_refcount(Proc *proc, uint32_t exc_ptr) {
    DRV_DEBUG("do_cxa_increment_exception_refcount called with value: %d", exc_ptr);

    wasm_val_vec_t args;
    wasm_val_vec_new_uninitialized(&args, 1);
    args.num_elems = 1;
    args.data[0].kind = WASM_I32;
    args.data[0].of.i32 = exc_ptr;

    wasm_val_vec_t results;
    wasm_val_vec_new_uninitialized(&results, 0);

    int ret = wasm_execute_exported_function(proc, "__cxa_increment_exception_refcount", &args, &results);
    if (ret != 0) {
        DRV_DEBUG("Failed to call __cxa_increment_exception_refcount");
        return;
    }

    DRV_DEBUG("Incremented exception refcount");
}

void do_cxa_decrement_exception_refcount(Proc *proc, uint32_t exc_ptr) {
    DRV_DEBUG("do_cxa_decrement_exception_refcount called with value: %d", exc_ptr);

    wasm_val_vec_t args;
    wasm_val_vec_new_uninitialized(&args, 1);
    args.num_elems = 1;
    args.data[0].kind = WASM_I32;
    args.data[0].of.i32 = exc_ptr;

    wasm_val_vec_t results;
    wasm_val_vec_new_uninitialized(&results, 0);

    int ret = wasm_execute_exported_function(proc, "__cxa_decrement_exception_refcount", &args, &results);
    if (ret != 0) {
        DRV_DEBUG("Failed to call __cxa_decrement_exception_refcount");
        return;
    }

    DRV_DEBUG("Decremented exception refcount");
}

uint32_t do_cxa_is_pointer_type(Proc *proc, uint32_t type) {
    DRV_DEBUG("do_cxa_is_pointer_type called with value: %d", type);

    wasm_val_vec_t args;
    wasm_val_vec_new_uninitialized(&args, 1);
    args.num_elems = 1;
    args.data[0].kind = WASM_I32;
    args.data[0].of.i32 = type;

    wasm_val_vec_t results;
    wasm_val_vec_new_uninitialized(&results, 1);

    int ret = wasm_execute_exported_function(proc, "__cxa_is_pointer_type", &args, &results);
    if (ret != 0) {
        DRV_DEBUG("Failed to call __cxa_is_pointer_type");
        return 0;
    }

    return results.data[0].of.i32;
}

wasm_trap_t *cxa_begin_catch_import(void *env, const wasm_val_vec_t *args,
							 wasm_val_vec_t *results)
{
	DRV_DEBUG("cxa_begin_catch_import called");
	ImportHook *import_hook = (ImportHook *)env;
	Proc *proc = import_hook->proc;
	wasm_store_t *store = proc->store;
	wasm_memory_t *wasm_memory = get_memory(proc);
	byte_t *wasm_base_ptr = wasm_memory_data(wasm_memory);
	int args_expected = 1;
	if (args->size != args_expected)
	{
		DRV_DEBUG("Incorrect number of arguments: expected %d, got %d", args_expected, (int)args->size);
		const char *errMsg = "cxa_begin_catch: Incorrect number of arguments provided";
		wasm_byte_vec_t message;
		wasm_byte_vec_new(&message, strlen(errMsg), errMsg);
		return wasm_trap_new(proc->store, &message);
	}

    DRV_DEBUG("cxa_begin_catch_import extract parameters");

	// extract parameters
	int32_t ptr_arg = args->data[0].of.i32;

	DRV_DEBUG("cxa_begin_catch_import extract parameters: ptr_arg: %d", ptr_arg);

    //  var info = new ExceptionInfo(ptr);
    uint32_t exc_ptr = ptr_arg;
    uint32_t ptr = exc_ptr - 24;

    //  if (!info.get_caught()) {
    bool is_caught = false;
    memcpy(&is_caught, &wasm_base_ptr[(ptr + 12)], 1);
    if (!is_caught) {
        DRV_DEBUG("Exception already caught");
    
    //   info.set_caught(true);
        bool _true = 1;
        DRV_DEBUG("memcpy(%p, %p, %d)", wasm_base_ptr + (ptr + 12), &_true, 1);
        memcpy(&wasm_base_ptr[(ptr + 12)], &_true, 1);

    //   uncaughtExceptionCount--;
        // No need to decrement, never used

    //  }
    }

    //  info.set_rethrown(false);
    bool _false = 0;
    memcpy(&wasm_base_ptr[(ptr + 13)], &_false, 1);

    //  exceptionCaught.push(info);
    // TODO: Exception Stack

    //  ___cxa_increment_exception_refcount(info.excPtr);
    do_cxa_increment_exception_refcount(proc, exc_ptr);

    //  return info.get_exception_ptr();

    //  get_exception_ptr() {
    //   var isPointer = ___cxa_is_pointer_type(this.get_type());
    uint32_t type;
    memcpy(&type, &wasm_base_ptr[(ptr + 4)], 4);
    uint32_t is_pointer = do_cxa_is_pointer_type(proc, type);

    //   if (isPointer) {
    //    return HEAPU32[((this.excPtr) >> 2)];
    //   }
    if (is_pointer) {
        uint32_t res;
        memcpy(&res, &wasm_base_ptr[(exc_ptr >> 2)], 4);
        results->data[0].kind = WASM_I32;
        results->data[0].of.i32 = res;
        return NULL;
    }

    //   var adjusted = this.get_adjusted_ptr();
    uint32_t adjusted;
    memcpy(&adjusted, &wasm_base_ptr[(exc_ptr + 16)], 4);

    //   if (adjusted !== 0) return adjusted;
    if (adjusted != 0) {
        results->data[0].kind = WASM_I32;
        results->data[0].of.i32 = adjusted;
        return NULL;
    }

    //   return this.excPtr;
    results->data[0].kind = WASM_I32;
    results->data[0].of.i32 = exc_ptr;

    return NULL;
}


wasm_trap_t *cxa_end_catch_import(void *env, const wasm_val_vec_t *args,
							 wasm_val_vec_t *results)
{
	DRV_DEBUG("cxa_end_catch_import called");
	ImportHook *import_hook = (ImportHook *)env;
	Proc *proc = import_hook->proc;
	wasm_store_t *store = proc->store;
	wasm_memory_t *wasm_memory = get_memory(proc);
	byte_t *wasm_base_ptr = wasm_memory_data(wasm_memory);
	int args_expected = 0;
	if (args->size != args_expected)
	{
		DRV_DEBUG("Incorrect number of arguments: expected %d, got %d", args_expected, (int)args->size);
		const char *errMsg = "cxa_end_catch: Incorrect number of arguments provided";
		wasm_byte_vec_t message;
		wasm_byte_vec_new(&message, strlen(errMsg), errMsg);
		return wasm_trap_new(proc->store, &message);
	}

    //  _setThrew(0, 0);
    do_set_threw(proc, 0, 0);

    //  var info = exceptionCaught.pop();
    // TODO: Exception Stack
    uint32_t exc_ptr = 0x1234;
    uint32_t ptr = exc_ptr - 24;

    //  ___cxa_decrement_exception_refcount(info.excPtr);
    do_cxa_decrement_exception_refcount(proc, exc_ptr);
    
    //  exceptionLast = 0;
    proc->exception_ptr_last = 0;

    return NULL;
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
    int init_msg_size = sizeof(ErlDrvTermData) * (2 + 3 + 5 + (13 * imports.size) + (11 * exports.size));
    ErlDrvTermData* init_msg = driver_alloc(init_msg_size);
    int msg_i = 0;

    // 2 in the init_msg_size
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
            DRV_DEBUG("Failed to get function signature for %s.%s", module_name->data, name->data);
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

        wasm_func_callback_with_env_t callback = wasm_handle_import;

		if (strcmp(module_name->data, "wasi_snapshot_preview1") == 0 && strcmp(name->data, "fd_write") == 0) {
			DRV_DEBUG("*** Overriding fd_write_import callback **");
			callback = fd_write_import;
		}

		if (strcmp(module_name->data, "env") == 0) {
            if (strcmp(name->data, "__cxa_throw") == 0) {
                DRV_DEBUG("*** Overriding cxa_throw_import callback **");
                callback = cxa_throw_import;
            } else if (strcmp(name->data, "__cxa_find_matching_catch_3") == 0) {
                DRV_DEBUG("*** Overriding __cxa_find_matching_catch_3 callback **");
                callback = cxa_find_matching_catch_3_import;
            } else if (strcmp(name->data, "__cxa_begin_catch") == 0) {
                DRV_DEBUG("*** Overriding cxa_begin_catch_import callback **");
                callback = cxa_begin_catch_import;
            } else if (strcmp(name->data, "__cxa_end_catch") == 0) {
                DRV_DEBUG("*** Overriding cxa_end_catch_import callback **");
                callback = cxa_end_catch_import;
            }
		}

        hook->stub_func =
            wasm_func_new_with_env(
                proc->store,
                wasm_externtype_as_functype_const(type),
                callback,
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

    wasm_extern_vec_t exported_externs;
    wasm_instance_exports(proc->instance, &exported_externs);

    // Refresh the exports now that we have an instance
    wasm_module_exports(proc->module, &exports);
    for (size_t i = 0; i < exports.size; i++) {
        //DRV_DEBUG("Processing export %d", i);
        const wasm_exporttype_t* export = exports.data[i];
        const wasm_name_t* name = wasm_exporttype_name(export);
        const wasm_externtype_t* type = wasm_exporttype_type(export);
        char* kind_str = (char*) wasm_externtype_to_kind_string(type);

        // Check if the export is the indirect function table
        if (strcmp(name->data, "__indirect_function_table") == 0) {
            DRV_DEBUG("Found indirect function table: %s. Index: %d", name->data, i);
            proc->indirect_func_table_ix = i;
            const wasm_tabletype_t* table_type = wasm_externtype_as_tabletype_const(type);
            // const wasm_limits_t* table_limits = wasm_tabletype_limits(table_type);
            // Retrieve the indirect function table
            proc->indirect_func_table = wasm_extern_as_table(exported_externs.data[i]);

        }

        char* type_str = driver_alloc(256);
        get_function_sig(type, type_str);
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

    proc->current_import = NULL;
    proc->is_initialized = 1;
    drv_unlock(proc->is_running);
}

void wasm_execute_function(void* raw) {
    Proc* proc = (Proc*)raw;
    DRV_DEBUG("Execute function: %s", proc->current_function);
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
        DRV_DEBUG("Arg %d: %d", i, args.data[i].of.i32);
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

    DRV_DEBUG("wasm_func_call(%p, %p, %p)", func, &args, &results);
    wasm_trap_t* trap = wasm_func_call(func, &args, &results);

    if (trap) {
        wasm_message_t trap_msg;
        wasm_trap_message(trap, &trap_msg);
        // wasm_frame_t* origin = wasm_trap_origin(trap);
        // int32_t func_index = wasm_frame_func_index(origin);
        // int32_t func_offset = wasm_frame_func_offset(origin);
        // char* func_name;

        DRV_DEBUG("WASM Exception: %.*s", trap_msg.size, trap_msg.data);
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
    DRV_DEBUG("Starting indirect function invocation");
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

    // print all properties of `param_types`
    DRV_DEBUG("Param types: %p", param_types);
    DRV_DEBUG("Param types size: %zu", param_types->size);
    DRV_DEBUG("Param types num_elems: %zu", param_types->num_elems);
    DRV_DEBUG("Param types data: %p", param_types->data);

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
        // DRV_DEBUG("Not enough arguments to create new wasm_val_vec_t");
        // return 0;
    } else {
		DRV_DEBUG("Preparing %zu arguments for function call", input_args->size - 1);
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

    uint32_t argc = prepared_args.size;
    uint32_t* argv = malloc(sizeof(uint32_t) * argc);
    
    // Convert prepared arguments to an array of 32-bit integers
    for (uint32_t i = 0; i < argc; ++i) {
        DRV_DEBUG("Arg %d: %d", i, prepared_args.data[i].of.i32);
        argv[i] = prepared_args.data[i].of.i32;
    }

    /* ---------------- STACK SAVE -----------------*/

    const char* stack_save_name = "emscripten_stack_get_current";

    wasm_val_vec_t stack_save_args;
    wasm_val_vec_new_uninitialized(&stack_save_args, 0);

    wasm_val_vec_t stack_save_results;
    wasm_val_vec_new_uninitialized(&stack_save_results, 1);

    if (wasm_execute_exported_function(proc, stack_save_name, &stack_save_args, &stack_save_results) != 0) {
        DRV_DEBUG("Failed to call stack save function");
    }
    DRV_DEBUG("Indirect function (%s) stack at pointer: %#x", field_name, stack_save_results.data[0].of.i32);

    size_t stack_ptr_index = stack_entry_push(proc, &stack_save_results.data[0]);
    DRV_DEBUG("Stack pointer index: %zu", stack_ptr_index);

    /* ---------------- STACK SAVE -----------------*/
    
    DRV_DEBUG("wasm_func_call(%p, %p, %p)", func, &prepared_args, output_results);
    wasm_trap_t* trap = wasm_func_call(func, &prepared_args, output_results);
    if (trap) {
		const char* exception = wasm_runtime_get_exception(func->inst_comm_rt);
        if (exception) {
            DRV_DEBUG("wasm_runtime_call_indirect (%s) gave exception: %s", field_name, exception);
        } else {
			DRV_DEBUG("wasm_runtime_call_indirect (%s) non-zero return, but no exception", field_name);
		}

		result = -1;
    }
    
    if(result == 0) {
    	DRV_DEBUG("Indirect function call completed successfully (%s)", field_name);
    } else {
		DRV_DEBUG("Indirect function call failed (%s)", field_name);
	}

    StackEntry* stack_entry = stack_entry_pop(proc);
    DRV_DEBUG("Stack pointer popped: %p", stack_entry);

    // Check for exception
    if (stack_entry->exception) {
        DRV_DEBUG("Stack entry has exception: %s", stack_entry->exception);

        /* ---------------- STACK RESTORE -----------------*/

        const char* stack_restore_name = "_emscripten_stack_restore";

        wasm_val_vec_t stack_restore_args;
        wasm_val_vec_new(&stack_restore_args, 1, stack_entry->ptr);
        stack_restore_args.num_elems = 1;

        wasm_val_vec_t stack_restore_results;
        wasm_val_vec_new_uninitialized(&stack_restore_results, 0);

        if (wasm_execute_exported_function(proc, stack_restore_name, &stack_restore_args, &stack_restore_results) != 0) {
            DRV_DEBUG("Failed to call stack restore function");
        }

        DRV_DEBUG("Indirect function (%s) stack restored to pointer: %#x", field_name, stack_entry->ptr->of.i32);

        /* ---------------- STACK RESTORE -----------------*/

        do_set_threw(proc, 1, 0);
    }

    // Free allocated memory
    free(argv);
    free(prepared_args.data);
    return result;
}

int wasm_execute_exported_function(Proc* proc, const char *function_name, const wasm_val_vec_t* input_args, wasm_val_vec_t* output_results) {
    DRV_DEBUG("=== Calling Runtime Export Function ===");
    DRV_DEBUG("=   Function name: %s", function_name);

    // Find the function in the exports
    wasm_func_t* func = get_exported_function(proc, function_name);
    if (!func) {
        send_error(proc, "Function not found: %s", function_name);
        drv_unlock(proc->is_running);
        return -1;
    }
    DRV_DEBUG("=   Func: %p (%s)", func, func->type);

    // Call the exported function
	DRV_DEBUG("=   Calling function: %s", function_name);

	DRV_DEBUG("=   wasm_func_call(%p, %p, %p)", func, input_args, output_results);
    wasm_trap_t* trap = wasm_func_call(func, input_args, output_results);

    if (trap) {
        wasm_message_t trap_msg;
        wasm_trap_message(trap, &trap_msg);
        DRV_DEBUG("=   %s export function call gave WASM exception: %.*s", function_name, trap_msg.size, trap_msg.data);

        return -1;
    }

    DRV_DEBUG("=   %s export function call successful", function_name);

    return 0;
}
