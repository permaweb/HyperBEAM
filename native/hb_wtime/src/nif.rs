use crate::convert::{self, nif_vals_to_wasm_vals};
use crate::types::{NativeFuncDesc, NifWasmVal, WasmVal};
use crate::wasm::{HostFuncDesc, HostFuncResult};
use crate::wasm_fsm::{
    CallOutcome, FsmError, HostFuncResponse, NativeFuncRequest, StateTag, WasmFsm, WasmModuleData,
};
use anyhow;
use rustler::types::atom::{error, ok};
use rustler::{Binary, Encoder, Env, NifResult, OwnedBinary, ResourceArc, Term};
use std::sync::Mutex;
use tracing::{debug, error, trace, debug_span};

pub struct NifRes {
    fsm: Mutex<WasmFsm>,
    #[allow(dead_code)]
    runtime: tokio::runtime::Runtime,
}

mod result_atoms {
    use rustler::atoms;

    atoms! {
        complete,
        import,
        not_found,
    }
}

fn fsm_error_to_term<'a>(env: Env<'a>, err: FsmError) -> Term<'a> {
    let reason_string_raw = err.to_string();
    // Rust strings (i.e. generated from wasmtime errors) can contain null bytes,
    // which could cause issues if passed along.
    let reason_string_escaped = reason_string_raw.replace("\0", "\\0");
    (error(), reason_string_escaped).encode(env)
}

#[rustler::nif(schedule = "DirtyCpu")]
fn create<'a>(env: Env<'a>, module_binary: Binary) -> NifResult<Term<'a>> {
    trace!("create");

    let span = debug_span!("create");
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

    let binary_data_slice = module_binary.as_slice();
    let owned_module_bytes_vec = binary_data_slice.to_vec();
    let leaked_module_bytes: &'static [u8] = Box::leak(owned_module_bytes_vec.into_boxed_slice());
    let module_data = WasmModuleData::Binary(leaked_module_bytes);

    let res = match WasmFsm::new(module_data, &runtime) {
        Ok(fsm_instance) => {
            debug!("WasmFsm created successfully.");
            let resource = ResourceArc::new(NifRes {
                fsm: Mutex::new(fsm_instance),
                runtime,
            });
            debug!("fsm.current_state(): {:?}", resource.fsm.lock().unwrap().current_state());
            Ok((ok(), resource).encode(env))
        }
        Err(e) => {
            error!("Error during WasmFsm::new: {:?}", e);
            Ok(fsm_error_to_term(env, e))
        }
    };

    debug!("res: {:?}", res);
    trace!("exit");
    res
}

#[rustler::nif(schedule = "DirtyCpu")]
fn meta<'a>(env: Env<'a>, resource: ResourceArc<NifRes>) -> NifResult<Term<'a>> {
    trace!("meta");

    let span = debug_span!("meta");
    let _enter = span.enter();

    let fsm = resource.fsm.lock().unwrap();

    let res = match convert::wasm_module_to_meta_term_list(env, &fsm.instance.module) {
        Ok(res) => Ok((ok(), res).encode(env)),
        Err(e) => Ok((error(), format!("Failed to convert metadata: {:?}", e)).encode(env)),
    };

    debug!("res: {:?}", res);
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

    let span = debug_span!("call_begin", wsm_fn = func_desc.to_string());
    let _enter = span.enter();

    debug!("enter");
    debug!("params: {:?}", params);

    let mut fsm = resource.fsm.lock().unwrap();

    debug!("fsm.current_state(): {:?}", fsm.current_state());
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
            return Ok((error(), reason).encode(env));
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
            let ok_atom = ok();
            match convert::wasm_vals_to_term_list(env, &results) {
                Ok(results_term) => Ok((ok_atom, result_atoms::complete(), results_term).encode(env)),
                Err(e) => {
                    error!("Result conversion failed: {:?}", e);
                    let reason = format!("Result conversion failed: {:?}", e);
                    Ok((error(), reason).encode(env))
                }
            }
        }
        Ok(CallOutcome::ImportCallNeeded(req)) => {
            trace!("Call started, yielded import call: {:?}", req);
            let ok_atom = ok();
            match convert::wasm_host_func_req_to_term_list(env, req) {
                Ok(meta_term) => Ok((ok_atom, result_atoms::import(), meta_term).encode(env)),
                Err(e) => {
                    error!("Import request conversion failed: {:?}", e);
                    let reason = format!("Import request conversion failed: {:?}", e);
                    Ok((error(), reason).encode(env))
                }
            }
        }
        Err(e) => Ok(fsm_error_to_term(env, e)),
    };

    debug!("fsm.current_state(): {:?}", fsm.current_state());

    debug!("res: {:?}", res);
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

    let span = debug_span!("call_continue", hst_fn = format!("{}.{}", module_name, field_name));
    let _enter = span.enter();

    debug!("enter");
    debug!("results: {:?}", results);

    let mut fsm = resource.fsm.lock().unwrap();

    debug!("fsm.current_state(): {:?}", fsm.current_state());

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
            results: HostFuncResult::Success(wasm_results),
        })
    })();

    let host_response = match decoding_result {
        Ok(resp) => resp,
        Err(reason_string) => {
            error!("Host response processing failed: {}", reason_string);
            return Ok((error(), reason_string).encode(env));
        }
    };

    if let Err(e) = fsm.pop(host_response) {
        return Ok(fsm_error_to_term(env, e));
    }

    let res = match fsm.step() {
        Ok(CallOutcome::Complete(results)) => {
            trace!("Call resumed and completed. Results: {:?}", results);
            let ok_atom = ok();
            match convert::wasm_vals_to_term_list(env, &results) {
                Ok(results_term) => Ok((ok_atom, result_atoms::complete(), results_term).encode(env)),
                Err(e) => {
                    error!("Result conversion failed: {:?}", e);
                    let reason = format!("Result conversion failed: {:?}", e);
                    Ok((error(), reason).encode(env))
                }
            }
        }
        Ok(CallOutcome::ImportCallNeeded(req)) => {
            trace!("Call resumed, yielded another import call: {:?}", req);
            let ok_atom = ok();
            match convert::wasm_host_func_req_to_term_list(env, req) {
                Ok(meta_term) => Ok((ok_atom, result_atoms::import(), meta_term).encode(env)),
                Err(e) => {
                    error!("Import request conversion failed: {:?}", e);
                    let reason = format!("Import request conversion failed: {:?}", e);
                    Ok((error(), reason).encode(env))
                }
            }
        }
        Err(e) => Ok(fsm_error_to_term(env, e)),
    };

    debug!("fsm.current_state(): {:?}", fsm.current_state());

    debug!("res: {:?}", res);
    trace!("exit");

    res
}

#[rustler::nif(schedule = "DirtyCpu")]
fn mem_size<'a>(
    env: Env<'a>,
    resource: ResourceArc<NifRes>,
) -> NifResult<Term<'a>> {
    trace!("mem_size");

    let span = debug_span!("mem_size");
    let _enter = span.enter();

    debug!("enter");

    let mut fsm = resource.fsm.lock().unwrap();

    let res = match fsm.get_memory_size() {
        Ok(size_option) => {
            match size_option {
                Some(size_in_bytes) => Ok((ok(), size_in_bytes).encode(env)),
                None => Ok((ok(), result_atoms::not_found()).encode(env)),
            }
        }
        Err(e) => Ok(fsm_error_to_term(env, e)),
    };

    debug!("res: {:?}", res);
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

    let span = debug_span!("mem_read", offset = offset, length = length);
    let _enter = span.enter();

    debug!("enter");

    let mut fsm = resource.fsm.lock().unwrap();

    let mut erl_bin = match OwnedBinary::new(length) {
        Some(bin) => bin,
        None => {
            return Ok((error(), format!("Failed to allocate memory (length: {})", length)).encode(env));
        }
    };

    let res = match fsm.read_memory(offset, erl_bin.as_mut_slice()) {
        Ok(()) => {
            Ok((ok(), Binary::from_owned(erl_bin, env)).encode(env))
        }
        Err(e) => Ok(fsm_error_to_term(env, e)),
    };

    debug!("res: {:?}", res);
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

    let span = debug_span!("mem_write", offset = offset, data_len = data.len());
    let _enter = span.enter();

    debug!("enter");

    let mut fsm = resource.fsm.lock().unwrap();

    let res = match fsm.write_memory(offset, data.as_slice()) {
        Ok(()) => Ok(ok().encode(env)),
        Err(e) => Ok(fsm_error_to_term(env, e)),
    };

    debug!("res: {:?}", res);
    trace!("exit");

    res
}
