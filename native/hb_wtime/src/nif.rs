use crate::types::{NifWasmVal, WasmVal, WasmValType};
use crate::wasm::*;
use crate::convert;
use rustler::{Atom, Binary, Env, NifResult, ResourceArc, Term, Encoder};
use tracing::{debug, error, trace, warn};
use std::sync::Mutex;
use tokio::sync::{mpsc, oneshot};

#[derive(Debug)]
pub struct NifContext {
    rt: tokio::runtime::Runtime,
    init_state: WasmInstanceState,
    init_extra: WasmInstanceExtra,
}

pub struct NifRes {
    context: Mutex<NifContext>,
}

#[rustler::nif(schedule = "DirtyCpu")]
fn wtime_create_instance(env: Env, module_binary: Binary) -> NifResult<(Atom, ResourceArc<NifRes>)> {
    trace!("wtime_instance_init");

    let rt = tokio::runtime::Runtime::new().map_err(|e| rustler::Error::Term(Box::new(format!("Failed to create Tokio runtime: {}", e))))?;

    let binary_data = module_binary.as_slice();
    let module_data = WasmModuleData::Binary(unsafe { std::mem::transmute(binary_data) });

    let init_res = rt.block_on(instance_init(module_data));

    match init_res {
        Ok(init_val) => {
            let context = NifContext {
                rt,
                init_state: init_val.state,
                init_extra: init_val.extra,
            };
            debug!("Wasm instance context created successfully.");

            let resource = ResourceArc::new(NifRes { context: Mutex::new(context) });
            Ok((Atom::from_str(env, "ok").unwrap(), resource))
        }
        Err(e) => {
            error!("Error during instance_init: {:?}", e);
            Err(rustler::Error::Term(Box::new(e.to_string())))
        }
    }
}

#[rustler::nif(schedule = "DirtyCpu")]
fn wtime_call_start(env: Env, resource: ResourceArc<NifRes>, func: String, params: Vec<NifWasmVal>) -> NifResult<Term> {
    trace!("wtime_call_start - func: {}, params: {:?}", func, params);

    let mut context = resource.context.lock().unwrap();
    let rt_handle = context.rt.handle().clone();

    let (_, dummy_rx) = mpsc::channel::<(HostFuncRequest, oneshot::Sender<HostFuncResponse>)>(1);
    let instance_extra = std::mem::replace(&mut context.init_extra, WasmInstanceExtra { host_req_channel: dummy_rx });

    let func_type = context.init_state.module.get_export(func.as_str()).unwrap();
    let param_def = func_type.unwrap_func().params();
    let param_types = param_def.collect::<Vec<_>>();

    let params = match convert::nif_vals_to_wasm_vals(params.as_slice(), &param_types) {
        Ok(p) => p,
        Err(e) => {
            error!("Argument conversion failed: {:?}", e);
            context.init_extra = instance_extra;
            return Err(e);
        }
    };
    debug!("Converted args to Wasm params: {:?}", params);

    let native_request = NativeFuncRequest {
        func_desc: NativeFuncDesc::Export(func),
        params: params,
    };

    let call_res_tuple: (Result<CallStepResult, anyhow::Error>, Option<WasmInstanceExtra>) = rt_handle.block_on(async {
        let mut call_state = match call_init(instance_extra) {
             Ok(state) => state,
             Err(e) => return (Err(e), None),
        };

        if let Err(e) = call_push_native(&mut context.init_state, &mut call_state, native_request).await {
            return (Err(e), Some(call_state.instance_extra));
        }

        let step_result = call_step(&mut call_state).await;

        (step_result, Some(call_state.instance_extra))
    });

    let (step_result, returned_instance_extra_opt) = call_res_tuple;

    if let Some(returned_instance_extra) = returned_instance_extra_opt {
         context.init_extra = returned_instance_extra;
    } else {
         warn!("call_init failed; instance_extra state might be unexpected.");
         let (_, new_dummy_rx) = mpsc::channel::<(HostFuncRequest, oneshot::Sender<HostFuncResponse>)>(1);
         context.init_extra = WasmInstanceExtra { host_req_channel: new_dummy_rx };
    }

    match step_result {
        Ok(step_result_val) => {
            match step_result_val {
                CallStepResult::ImportCall(import_meta) => {
                    error!("ImportCall occurred, but state persistence for resume is not implemented. Meta: {:?}", import_meta);

                    let ok_atom = Atom::from_str(env, "ok").unwrap();
                    let import_atom = Atom::from_str(env, "import").unwrap();
                    let meta_term = convert::wasm_host_func_req_to_term_list(env, import_meta)?;
                    Ok((ok_atom, import_atom, meta_term).encode(env))
                }
                CallStepResult::Complete(results) => {
                    debug!("Call completed immediately. Results: {:?}", results);
                    let ok_atom = Atom::from_str(env, "ok").unwrap();
                    let complete_atom = Atom::from_str(env, "complete").unwrap();

                    let results_term = convert::wasm_vals_to_term_list(env, &results)?;
                    Ok((ok_atom, complete_atom, results_term).encode(env))
                }
            }
        }
        Err(e) => {
             error!("Error during call execution: {:?}", e);
             let error_atom = Atom::from_str(env, "error").unwrap();
             Ok((error_atom, e.to_string()).encode(env))
        }
    }
}


