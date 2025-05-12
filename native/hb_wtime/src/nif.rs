use crate::wasm::*;
use rustler::{Atom, Binary, Env, NifResult, Resource, ResourceArc, Term, Encoder};
use tracing::{debug, error, trace, warn};
use wasmtime::Val;
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
fn wtime_call_start(env: Env, resource: ResourceArc<NifRes>, func: String, args: Vec<f64>) -> NifResult<Term> {
    trace!("wtime_call_start");

    let mut context = resource.context.lock().unwrap();

    // Get a cloneable handle to the runtime *before* block_on
    // This avoids borrowing `context` within the block_on call itself.
    let rt_handle = context.rt.handle().clone();

    let (dummy_tx, dummy_rx) = mpsc::channel::<(HostFuncRequest, oneshot::Sender<HostFuncResponse>)>(1);
    let mut instance_extra = std::mem::replace(&mut context.init_extra, WasmInstanceExtra { host_req_channel: dummy_rx });

    // Convert f64 argument to WasmVal::F64 using its bit representation
    let params = vec![Val::F64(args[0].to_bits())];
    if !args.is_empty() {
        warn!("Argument conversion from String to WasmVal not implemented yet.");
    }
    let native_request = NativeFuncRequest {
        func_desc: NativeFuncDesc::Export(func),
        params: params,
    };

    let call_res_tuple: (Result<CallStepResult, anyhow::Error>, Option<WasmInstanceExtra>) = rt_handle.block_on(async {
        let mut call_state = match call_init(instance_extra) {
             Ok(state) => state,
             Err(e) => return (Err(e.into()), None),
        };

        let result = call_push_native(&mut context.init_state, &mut call_state, native_request).await;

        (result, Some(call_state.instance_extra))
    });

    let (call_res, returned_instance_extra_opt) = call_res_tuple;

    if let Some(returned_instance_extra) = returned_instance_extra_opt {
         context.init_extra = returned_instance_extra;
    } else {
         warn!("call_init failed; restoring dummy instance_extra.");
         let (_, new_dummy_rx) = mpsc::channel::<(HostFuncRequest, oneshot::Sender<HostFuncResponse>)>(1);
         context.init_extra = WasmInstanceExtra { host_req_channel: new_dummy_rx };
    }

    match call_res {
        Ok(step_result) => {
            match step_result {
                CallStepResult::ImportCall(import_meta) => {
                    error!("ImportCall occurred, but state persistence for resume is not implemented.");

                    let ok_atom = Atom::from_str(env, "ok").unwrap();
                    let import_atom = Atom::from_str(env, "import").unwrap();
                    let meta_term = rustler::types::map::map_new(env);
                    Ok((ok_atom, import_atom, meta_term).encode(env))
                }
                CallStepResult::Complete(results) => {
                    debug!("Call completed immediately. Results: {:?}", results);

                    let ok_atom = Atom::from_str(env, "ok").unwrap();
                    let complete_atom = Atom::from_str(env, "complete").unwrap();
                    // Convert WasmVal results to a Vec<Term>
                    let encoded_results: Result<Vec<Term>, rustler::Error> = results.into_iter().map(|result| {
                        match result {
                            Val::I32(i) => Ok(i.encode(env)),
                            Val::I64(i) => Ok(i.encode(env)),
                            Val::F32(f) => Ok(f32::from_bits(f).encode(env)),
                            Val::F64(f) => Ok(f64::from_bits(f).encode(env)),
                            // TODO: Handle other Val types like V128, FuncRef, ExternRef if needed
                            _ => Err(rustler::Error::Term(Box::new("Unsupported WasmVal result type"))),
                        }
                    }).collect();

                    // Encode the Vec<Term> directly, or propagate the error
                    let results_term = encoded_results?.encode(env);
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
