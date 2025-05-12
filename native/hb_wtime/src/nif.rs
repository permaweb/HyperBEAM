use crate::wasm::*;
use rustler::{Atom, Binary, Env, Resource, ResourceArc};
use tracing::{debug, error, trace};
use std::sync::Mutex;

#[derive(Debug)]
pub struct NifContext {
    init_state: Mutex<WasmInstanceState>,
    init_extra: Mutex<WasmInstanceExtra>,
    call: Mutex<Option<WasmCallState<'static>>>,
}

#[rustler::nif(schedule = "DirtyCpu")]
fn wtime_instance_init(env: Env, module_binary: Binary) -> rustler::NifResult<(rustler::Atom, ResourceArc<NifContext>)> {
    trace!("wtime_instance_init");

    let binary_data = module_binary.as_slice();
    let module_data = WasmModuleData::Binary(unsafe { std::mem::transmute(binary_data) });

    let rt = tokio::runtime::Runtime::new().unwrap();
    let init_handle = rt.spawn(async move {
        instance_init(module_data).await
    });

    let init_res = rt.block_on(init_handle).unwrap();

    match init_res {
        Ok(init_success) => {
            let context = NifContext {
                init_state: Mutex::new(init_success.state),
                init_extra: Mutex::new(init_success.extra),
                call: Mutex::new(None),
            };
            println!("context: {:?}", context);

            let resource = ResourceArc::new(context);
            Ok((Atom::from_str(env, "ok").unwrap(), resource))
        }
        Err(e) => {
            error!("error: {:?}", e);
            Err(rustler::Error::Term(Box::new(e.to_string())))
        }
    }
}
