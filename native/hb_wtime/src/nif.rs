use crate::convert::{self, nif_vals_to_wasm_vals};
use crate::types::{NativeFuncDesc, NifWasmVal, WasmVal};
use crate::wasm::{CallStepResult, HostFuncDesc, HostFuncResult};
use crate::wasm_fsm::{
    FsmError, HostFuncResponse, NativeFuncRequest, StateTag, StepType, WasmFsm, WasmModuleData
};
use anyhow;
use rustler::types::atom::{error, ok};
use rustler::{Binary, Encoder, Env, NifResult, OwnedBinary, ResourceArc, Term};
use std::sync::Mutex;
use tracing::{debug, debug_span, error, trace, warn};

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
    // Make sure it is converted to a `Binary` and owned by Erlang
    let mut reason_binary_owned = OwnedBinary::new(reason_string_escaped.len()).unwrap();
    reason_binary_owned.as_mut_slice().copy_from_slice(reason_string_escaped.as_bytes());
    let reason_binary = Binary::from_owned(reason_binary_owned, env);
    (error(), reason_binary).encode(env)
}

fn fsm_state_meta_to_str(fsm: &WasmFsm) -> String {
    let tag_str = match fsm.current_state() {
        StateTag::Idle => "Idle".to_string(),
        StateTag::AwaitingHost => "Await".to_string(),
        StateTag::PendingStep(st) => match st {
            StepType::Begin => "PendBeg".to_string(),
            StepType::Resume => "PendRes".to_string(),
        },
    };
    let call_stack_len = fsm.call_stack.len();
    let call_has_import_flag = match fsm.call_stack.first().map_or(false, |call| call.pending_import.is_some()) {
        true => "i",
        _ => "",
    };
    format!("{tag_str}({call_stack_len}{call_has_import_flag})")
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
            debug!("fsm: {}", fsm_state_meta_to_str(&resource.fsm.lock().unwrap()));
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

    debug!("enter");

    let fsm = resource.fsm.lock().unwrap();

    debug!("fsm: {}", fsm_state_meta_to_str(&fsm));

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

    debug!("fsm: {}", fsm_state_meta_to_str(&fsm));
    
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
        func_desc: func_desc.clone(),
        params: wasm_params,
    };

    if let Err(e) = fsm.push(native_request) {
        return Ok(fsm_error_to_term(env, e));
    }

    let res = match fsm.step() {
        Ok(CallStepResult::Complete(results)) => {
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
        Ok(CallStepResult::ImportCall(req)) => {
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
        Ok(CallStepResult::Error(reason)) => {
            let msg = format!("Call to {} resulted in error: {}", func_desc, reason);
            warn!(msg);
            Ok((error(), msg).encode(env))
        }
        Err(e) => Ok(fsm_error_to_term(env, e)),
    };

    debug!("fsm: {}", fsm_state_meta_to_str(&fsm));

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

    debug!("fsm: {}", fsm_state_meta_to_str(&fsm));

    if !matches!(fsm.current_state(), StateTag::AwaitingHost) {
        return Ok(fsm_error_to_term(
            env,
            FsmError::InvalidState {
                operation: "call_resume (must be AwaitingHost)",
                current_state: fsm.current_state().clone(),
            },
        ));
    }

    let func_desc = HostFuncDesc {
        module_name,
        field_name,
    };

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
            func_desc: func_desc.clone(),
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
        Ok(CallStepResult::Complete(results)) => {
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
        Ok(CallStepResult::ImportCall(req)) => {
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
        Ok(CallStepResult::Error(reason)) => {
            let msg = format!("Response from {} resulted in error: {}", func_desc, reason);
            warn!(msg);
            Ok((error(), msg).encode(env))
        }
        Err(e) => Ok(fsm_error_to_term(env, e)),
    };

    debug!("fsm: {}", fsm_state_meta_to_str(&fsm));

    debug!("res: {:?}", res);
    trace!("exit");

    res
}

#[rustler::nif(schedule = "DirtyCpu")]
fn call_cancel<'a>(
    env: Env<'a>,
    resource: ResourceArc<NifRes>,
    module_name: String,
    field_name: String,
) -> NifResult<Term<'a>> {
    trace!("call_cancel");

    let span = debug_span!("call_cancel");
    let _enter = span.enter();

    debug!("enter");

    let mut fsm = resource.fsm.lock().unwrap();

    debug!("fsm: {}", fsm_state_meta_to_str(&fsm));

    if !matches!(fsm.current_state(), StateTag::AwaitingHost) {
        return Ok(fsm_error_to_term(
            env,
            FsmError::InvalidState {
                operation: "call_cancel (must be AwaitingHost)",
                current_state: fsm.current_state().clone(),
            },
        ));
    }

    let host_response =
        HostFuncResponse {
            func_desc: HostFuncDesc {
                module_name,
                field_name,
            },
            results: HostFuncResult::Failure("Cancelled by `call_cancel` NIF function".to_string()),
        };

    if let Err(e) = fsm.pop(host_response) {
        return Ok(fsm_error_to_term(env, e));
    }

    let res = match fsm.step() {
        Ok(CallStepResult::Error(reason)) => {
            Ok((ok(), reason).encode(env))
        }
        Ok(CallStepResult::Complete(results)) => {
            let err = format!("Unexpected call complete: {:?}", results);
            Ok((error(), err).encode(env))
        }
        Ok(CallStepResult::ImportCall(req)) => {
            let err = format!("Unexpected call import needed: {:?}", req);
            Ok((error(), err).encode(env))
        }
        Err(e) => Ok(fsm_error_to_term(env, e)),
    };

    debug!("fsm: {}", fsm_state_meta_to_str(&fsm));

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

    debug!("fsm: {}", fsm_state_meta_to_str(&fsm));

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

    debug!("fsm: {}", fsm_state_meta_to_str(&fsm));

    let res = match fsm.write_memory(offset, data.as_slice()) {
        Ok(()) => Ok(ok().encode(env)),
        Err(e) => Ok(fsm_error_to_term(env, e)),
    };

    debug!("res: {:?}", res);
    trace!("exit");

    res
}
