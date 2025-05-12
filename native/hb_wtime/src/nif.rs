use crate::wasm::*;
use rustler::{Atom, Binary, Env, Resource, ResourceArc};
use tracing::{debug, error, trace};
use std::sync::Mutex;
use tokio::sync::{mpsc, oneshot};

#[derive(Debug)]
pub struct NifContext {
    rt: tokio::runtime::Runtime,
    init_state: WasmInstanceState,
    init_extra: WasmInstanceExtra,
    call: Option<WasmCallState<'static>>,
}

pub struct NifRes {
    context: Mutex<NifContext>,
}

#[rustler::nif(schedule = "DirtyCpu")]
fn wtime_instance_init(env: Env, module_binary: Binary) -> rustler::NifResult<(rustler::Atom, ResourceArc<NifRes>)> {
    trace!("wtime_instance_init");

    let rt = tokio::runtime::Runtime::new().unwrap();

    let binary_data = module_binary.as_slice();
    let module_data = WasmModuleData::Binary(unsafe { std::mem::transmute(binary_data) });

    let init_handle = rt.spawn(async move {
        instance_init(module_data).await
    });

    let init_res = rt.block_on(init_handle).unwrap();

    match init_res {
        Ok(init_val) => {
            let context = NifContext {
                rt,
                init_state: init_val.state,
                init_extra: init_val.extra,
                call: None,
            };
            debug!("context: {:?}", context);

            let resource = ResourceArc::new(NifRes { context: Mutex::new(context) });
            Ok((Atom::from_str(env, "ok").unwrap(), resource))
        }
        Err(e) => {
            error!("error: {:?}", e);
            Err(rustler::Error::Term(Box::new(e.to_string())))
        }
    }
}

#[rustler::nif(schedule = "DirtyCpu")]
fn wtime_call_init(env: Env, resource: ResourceArc<NifRes>, func: String, args: Vec<String>) -> rustler::NifResult<rustler::Atom> {
    trace!("wtime_call_init");

    let mut context = resource.context.lock().unwrap();
    let init_extra = std::mem::replace(&mut context.init_extra, WasmInstanceExtra {
        host_req_channel: mpsc::channel::<(HostFuncRequest, oneshot::Sender<HostFuncResponse>)>(1).1,
    });

    let call_handle = context.rt.spawn(async move {
        call_init(init_extra)
    });

    let call_res = context.rt.block_on(call_handle).unwrap();

    Ok(Atom::from_str(env, "ok").unwrap())
}
