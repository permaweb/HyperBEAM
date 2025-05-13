use crate::convert::{self, nif_vals_to_wasm_vals};
use crate::types::{NativeFuncDesc, NifWasmVal, WasmVal};
use crate::wasm::HostFuncDesc;
use crate::wasm_fsm::{
    CallOutcome, FsmError, HostFuncResponse, NativeFuncRequest, StateTag, WasmFsm, WasmModuleData,
};
use anyhow;
use rustler::{Atom, Binary, Encoder, Env, NifResult, ResourceArc, Term};
use std::sync::Mutex;
use tracing::{debug, error, trace, trace_span};

pub struct NifRes {
    fsm: Mutex<WasmFsm>,
    #[allow(dead_code)]
    runtime: tokio::runtime::Runtime,
}

fn fsm_error_to_term<'a>(env: Env<'a>, err: FsmError) -> Term<'a> {
    let reason_string = err.to_string();
    (Atom::from_str(env, "error").unwrap(), reason_string).encode(env)
}

#[rustler::nif(schedule = "DirtyCpu")]
fn create<'a>(env: Env<'a>, module_binary: Binary) -> NifResult<Term<'a>> {
    trace!("create");

    let span = trace_span!("create");
    let _enter = span.enter();

    debug!("enter");

    let runtime = match tokio::runtime::Runtime::new() {
        Ok(rt) => rt,
        Err(e) => {
            let fsm_err = FsmError::InitializationFailed(anyhow::anyhow!(
                "Failed to create Tokio runtime: {}",
                e
            ));
            return Ok(fsm_error_to_term(env, fsm_err));
        }
    };

    let binary_data = module_binary.as_slice();
    // SAFETY: Transmuting lifetime to 'static. Assumes `module_binary` (owned by BEAM)
    // lives at least as long as the WasmFsm resource, as WasmModuleData::Binary expects &'static [u8].
    let module_data = WasmModuleData::Binary(unsafe { std::mem::transmute(binary_data) });

    let res = match WasmFsm::new(module_data, &runtime) {
        Ok(fsm_instance) => {
            debug!("WasmFsm created successfully.");
            let resource = ResourceArc::new(NifRes {
                fsm: Mutex::new(fsm_instance),
                runtime,
            });
            Ok((Atom::from_str(env, "ok").unwrap(), resource).encode(env))
        }
        Err(e) => {
            error!("Error during WasmFsm::new: {:?}", e);
            Ok(fsm_error_to_term(env, e))
        }
    };

    trace!("exit");

    res
}

#[rustler::nif(schedule = "DirtyCpu")]
fn call_begin<'a>(
    env: Env<'a>,
    resource: ResourceArc<NifRes>,
    func_desc: NativeFuncDesc,
    params: Vec<NifWasmVal>,
) -> NifResult<Term<'a>> {
    trace!("call_begin");

    let span = trace_span!("call_begin", func = func_desc.to_string(), params = ?params);
    let _enter = span.enter();

    debug!("enter");

    let mut fsm = resource.fsm.lock().unwrap();

    if !matches!(fsm.current_state(), StateTag::Idle) && 
       !matches!(fsm.current_state(), StateTag::AwaitingHost) {
        return Ok(fsm_error_to_term(
            env,
            FsmError::InvalidState {
                operation: "call_start (must be Idle or AwaitingHost for nesting)",
                current_state: fsm.current_state().clone(),
            },
        ));
    }

    let param_types: Vec<wasmtime::ValType> = match fsm.get_native_func_param_types(&func_desc) {
        Ok(types) => types,
        Err(e) => return Ok(fsm_error_to_term(env, e)),
    };

    let wasm_params = match convert::nif_vals_to_wasm_vals(params.as_slice(), &param_types) {
        Ok(p) => p,
        Err(e) => {
            error!("Argument conversion failed: {:?}", e);
            let reason = format!("Argument conversion failed: {:?}", e);
            return Ok((Atom::from_str(env, "error").unwrap(), reason).encode(env));
        }
    };

    let native_request = NativeFuncRequest {
        func_desc,
        params: wasm_params,
    };

    if let Err(e) = fsm.push(native_request) {
        return Ok(fsm_error_to_term(env, e));
    }

    let res = match fsm.step() {
        Ok(CallOutcome::Complete(results)) => {
            trace!("Call started and completed. Results: {:?}", results);
            let ok_atom = Atom::from_str(env, "ok").unwrap();
            let complete_atom = Atom::from_str(env, "complete").unwrap();
            match convert::wasm_vals_to_term_list(env, &results) {
                Ok(results_term) => Ok((ok_atom, complete_atom, results_term).encode(env)),
                Err(e) => {
                    error!("Result conversion failed: {:?}", e);
                    let reason = format!("Result conversion failed: {:?}", e);
                    Ok((Atom::from_str(env, "error").unwrap(), reason).encode(env))
                }
            }
        }
        Ok(CallOutcome::ImportCallNeeded(req)) => {
            trace!("Call started, yielded import call: {:?}", req);
            let ok_atom = Atom::from_str(env, "ok").unwrap();
            let import_atom = Atom::from_str(env, "import").unwrap();
            match convert::wasm_host_func_req_to_term_list(env, req) {
                Ok(meta_term) => Ok((ok_atom, import_atom, meta_term).encode(env)),
                Err(e) => {
                    error!("Import request conversion failed: {:?}", e);
                    let reason = format!("Import request conversion failed: {:?}", e);
                    Ok((Atom::from_str(env, "error").unwrap(), reason).encode(env))
                }
            }
        }
        Err(e) => Ok(fsm_error_to_term(env, e)),
    };

    trace!("exit");

    res
}

#[rustler::nif(schedule = "DirtyCpu")]
fn call_continue<'a>(
    env: Env<'a>,
    resource: ResourceArc<NifRes>,
    module_name: String,
    field_name: String,
    results: Vec<NifWasmVal>,
) -> NifResult<Term<'a>> {
    trace!("call_continue");

    let span = trace_span!("call_continue", func = format!("{}.{}", module_name, field_name), results = ?results);
    let _enter = span.enter();

    debug!("enter");

    let mut fsm = resource.fsm.lock().unwrap();

    if !matches!(fsm.current_state(), StateTag::AwaitingHost) {
        return Ok(fsm_error_to_term(
            env,
            FsmError::InvalidState {
                operation: "call_resume (must be AwaitingHost)",
                current_state: fsm.current_state().clone(),
            },
        ));
    }

    // Inner function to handle fallible conversion, returns Result<HostFuncResponse, String>
    // where String is a formatted error message.
    let decoding_result: Result<HostFuncResponse, String> = (|| {
        let result_types = match fsm.get_last_host_func_result_types() {
            Ok(types) => types,
            Err(fsm_err) => {
                return Err(format!(
                    "Error getting host func result types: {}",
                    fsm_err.to_string()
                ));
            }
        };
        let wasm_results: Vec<WasmVal> =
            match nif_vals_to_wasm_vals(results.as_slice(), &result_types) {
                Ok(vals) => vals,
                Err(rustler_err) => {
                    return Err(format!(
                        "Error converting NIF vals to Wasm vals: {:?}",
                        rustler_err
                    ));
                }
            };
        Ok(HostFuncResponse {
            func_desc: HostFuncDesc {
                module_name,
                field_name,
            },
            results: wasm_results,
        })
    })();

    let host_response = match decoding_result {
        Ok(resp) => resp,
        Err(reason_string) => {
            error!("Host response processing failed: {}", reason_string);
            return Ok((Atom::from_str(env, "error").unwrap(), reason_string).encode(env));
        }
    };

    if let Err(e) = fsm.pop(host_response) {
        return Ok(fsm_error_to_term(env, e));
    }

    let res = match fsm.step() {
        Ok(CallOutcome::Complete(results)) => {
            trace!("Call resumed and completed. Results: {:?}", results);
            let ok_atom = Atom::from_str(env, "ok").unwrap();
            let complete_atom = Atom::from_str(env, "complete").unwrap();
            match convert::wasm_vals_to_term_list(env, &results) {
                Ok(results_term) => Ok((ok_atom, complete_atom, results_term).encode(env)),
                Err(e) => {
                    error!("Result conversion failed: {:?}", e);
                    let reason = format!("Result conversion failed: {:?}", e);
                    Ok((Atom::from_str(env, "error").unwrap(), reason).encode(env))
                }
            }
        }
        Ok(CallOutcome::ImportCallNeeded(req)) => {
            trace!("Call resumed, yielded another import call: {:?}", req);
            let ok_atom = Atom::from_str(env, "ok").unwrap();
            let import_atom = Atom::from_str(env, "import").unwrap();
            match convert::wasm_host_func_req_to_term_list(env, req) {
                Ok(meta_term) => Ok((ok_atom, import_atom, meta_term).encode(env)),
                Err(e) => {
                    error!("Import request conversion failed: {:?}", e);
                    let reason = format!("Import request conversion failed: {:?}", e);
                    Ok((Atom::from_str(env, "error").unwrap(), reason).encode(env))
                }
            }
        }
        Err(e) => Ok(fsm_error_to_term(env, e)),
    };

    trace!("exit");

    res
}

#[rustler::nif(schedule = "DirtyCpu")]
fn mem_size<'a>(
    env: Env<'a>,
    resource: ResourceArc<NifRes>,
) -> NifResult<Term<'a>> {
    trace!("mem_size");

    let span = trace_span!("mem_size");
    let _enter = span.enter();

    debug!("enter");

    let mut fsm = resource.fsm.lock().unwrap();

    // We need access to the instance state within the FSM
    // Assuming WasmFsm has a method to get the instance state mutably 
    // or a dedicated method that calls wasm_memory_size internally.
    // Let's assume a method `get_memory_size()` exists on Fsm for now.
    let res = match fsm.get_memory_size() { // Assuming this method exists
        Ok(size_in_pages) => {
            // Wasm memory size is in pages (64KiB)
            // let size_in_bytes = size_in_pages * 65536; // No longer needed, FSM returns pages
            Ok((Atom::from_str(env, "ok").unwrap(), size_in_pages).encode(env))
        }
        Err(e) => Ok(fsm_error_to_term(env, e)),
    };

    trace!("exit");

    res
}

#[rustler::nif(schedule = "DirtyCpu")]
fn mem_read<'a>(
    env: Env<'a>,
    resource: ResourceArc<NifRes>,
    offset: usize,
    length: usize,
) -> NifResult<Term<'a>> {
    trace!("mem_read");

    let span = trace_span!("mem_read", offset = offset, length = length);
    let _enter = span.enter();

    debug!("enter");

    let mut fsm = resource.fsm.lock().unwrap();

    let mut erl_bin = rustler::NewBinary::new(env, length);

    let res = match fsm.read_memory(offset, erl_bin.as_mut_slice()) {
        Ok(()) => {
            Ok((Atom::from_str(env, "ok").unwrap(), Term::from(erl_bin)).encode(env))
        }
        Err(e) => Ok(fsm_error_to_term(env, e)),
    };

    trace!("exit");

    res
}

#[rustler::nif(schedule = "DirtyCpu")]
fn mem_write<'a>(
    env: Env<'a>,
    resource: ResourceArc<NifRes>,
    offset: usize,
    data: Binary<'a>,
) -> NifResult<Term<'a>> {
    trace!("mem_write");

    let span = trace_span!("mem_write", offset = offset, data_len = data.len());
    let _enter = span.enter();

    debug!("enter");

    let mut fsm = resource.fsm.lock().unwrap();

    let res = match fsm.write_memory(offset, data.as_slice()) {
        Ok(()) => Ok(Atom::from_str(env, "ok").unwrap().encode(env)),
        Err(e) => Ok(fsm_error_to_term(env, e)),
    };

    trace!("exit");

    res
}
