use std::{future::Future, pin::Pin};
use crate::types::{WasmVal, WasmValType};

use anyhow::Result;
use tokio::sync::{mpsc, oneshot};
use tracing::{debug, error, info, trace};
use wasmtime::{Caller, Config, Engine, Instance, Linker, Module, Store};

#[derive(Debug, Clone)]
pub enum WasmModuleData {
    Binary(&'static [u8]),
    Wat(&'static str),
}

type StoreData = ();

#[derive(Debug)]
pub struct WasmInstanceState {
    pub engine: Engine,
    pub linker: Linker<StoreData>,
    pub store: Store<StoreData>,
    pub module: Module,
    pub instance: Instance,
}

type HostFuncRespTx = oneshot::Sender<HostFuncResponse>;

type HostFuncChannel = mpsc::Receiver<(HostFuncRequest, HostFuncRespTx)>;

#[derive(Debug)]
pub struct WasmInstanceExtra {
    pub host_req_channel: HostFuncChannel,
}

#[derive(Debug)]
pub struct InstanceInitResult {
    pub state: WasmInstanceState,
    pub extra: WasmInstanceExtra,
}

// Initialize the wasmtime runtime
pub async fn instance_init(module_binary: WasmModuleData) -> Result<InstanceInitResult> {
    trace!("instance_init");

    let mut cfg = Config::new();
    
    // Necessary for the async import architecture
    cfg.async_support(true);

    // Configurations for deterministic behavior
    cfg.cranelift_nan_canonicalization(true);
    cfg.wasm_threads(false);
    cfg.relaxed_simd_deterministic(true);

    // Additional safe features
    // TODO: Configurable?
    cfg.wasm_simd(true);
    cfg.wasm_relaxed_simd(true); // see `relaxed_simd_deterministic`

    let engine = Engine::new(&cfg)?;

    let module = match module_binary {
        WasmModuleData::Binary(binary) => Module::new(&engine, binary)?,
        WasmModuleData::Wat(wat) => Module::new(&engine, wat)?,
    };

    let mut store = Store::new(&engine, ());
    let mut linker = Linker::new(&engine);

    // Create an mpsc channel for requests from guest to host.
    // Each request will include a oneshot::Sender for the host to send the response.
    let (host_req_tx, host_req_rx) =
        mpsc::channel::<(HostFuncRequest, oneshot::Sender<HostFuncResponse>)>(1);

    module.imports().for_each(|import| {
        let module_name_ref = import.module();
        let field_name_ref = import.name();
        trace!("Creating import function for: {}.{}", module_name_ref, field_name_ref);

        let ty = match import.ty().func() {
            Some(ty) => ty.clone(),
            None => panic!("Import function {} has no type", field_name_ref),
        };

        let module_name_owned = import.module().to_string();
        let field_name_owned = import.name().to_string();

        let current_host_req_tx = host_req_tx.clone();

        let link_res = linker.func_new_async(
            module_name_ref,
            field_name_ref,
            ty,
            move |mut _caller: Caller<'_, ()>, params: &[WasmVal], results: &mut [WasmVal]| {
                let captured_module_name = module_name_owned.clone();
                let captured_field_name = field_name_owned.clone();
                trace!(
                    "Import function: {}.{}",
                    module_name_owned,
                    field_name_owned
                );

                let params_vec = params.to_vec();
                trace!("Params: {:?}", params_vec);

                let captured_req_tx = current_host_req_tx.clone();

                Box::new(async move {
                    trace!(
                        "Import function: {}.{}",
                        captured_module_name,
                        captured_field_name
                    );

                    let (resp_tx, resp_rx) = oneshot::channel::<HostFuncResponse>();

                    let request = HostFuncRequest {
                        func_desc: HostFuncDesc {
                            module_name: captured_module_name,
                            field_name: captured_field_name,
                        },
                        params: params_vec,
                    };

                    trace!("Sending request: {:?}", request);

                    if captured_req_tx.send((request, resp_tx)).await.is_err() {
                        // Host is no longer listening or channel closed
                        return Err(anyhow::anyhow!("Failed to send request to host"));
                    }

                    trace!("Waiting for response");

                    let host_response = resp_rx.await;

                    trace!("Received response: {:?}", host_response);

                    match host_response {
                        Ok(host_response) => {
                            if results.len() == host_response.results.len() {
                                results.copy_from_slice(&host_response.results);
                                Ok(())
                            } else {
                                Err(anyhow::anyhow!("Host response arity mismatch"))
                            }
                        }
                        Err(_) => {
                            // Host dropped the response sender or other error
                            Err(anyhow::anyhow!("Failed to receive response from host"))
                        }
                    }
                })
            },
        );

        match link_res {
            Ok(_) => (),
            Err(e) => panic!("Failed to link function: {}", e),
        };
    });

    let instance = linker.instantiate_async(&mut store, &module).await?;
    let state = WasmInstanceState {
        engine,
        module,
        linker,
        store,
        instance,
    };
    let extra = WasmInstanceExtra {
        host_req_channel: host_req_rx,
    };

    let res = InstanceInitResult {
        state,
        extra,
    };

    trace!("Instance init result: {:?}", res);

    Ok(res)
}

pub struct WasmFunctionSignature {
    pub params: Vec<WasmValType>,
    pub results: Vec<WasmValType>,
}

type WasmParamsVec = Vec<WasmVal>;

type WasmResultsVec = Vec<WasmVal>;

#[derive(Debug, Clone)]
pub struct HostFuncDesc {
    pub module_name: String,
    pub field_name: String,
}

#[derive(Debug, Clone)]
pub enum NativeFuncDesc {
    Export(String),
    Indirect(i64),
}

#[derive(Debug, Clone)]
pub struct NativeFuncRequest {
    pub func_desc: NativeFuncDesc,
    pub params: WasmParamsVec,
}

#[derive(Debug, Clone)]
pub struct NativeFuncResponse {
    pub func_desc: NativeFuncDesc,
    pub results: WasmResultsVec,
}

#[derive(Debug, Clone)]
pub struct HostFuncRequest {
    pub func_desc: HostFuncDesc,
    pub params: WasmParamsVec,
}

#[derive(Debug, Clone)]
pub struct HostFuncResponse {
    pub func_desc: HostFuncDesc,
    pub results: WasmResultsVec,
}

pub struct PendingImportStackItem<'a> {
    pub func_desc: HostFuncDesc,
    pub results_count: usize,
    /// The active Wasm future. The lifetime 'a is tied to the Wasmtime Store
    /// from WasmInstanceState that this future operates on.
    pub wasm_future: Pin<Box<dyn Future<Output = Result<Vec<WasmVal>, anyhow::Error>> + Send + 'a>>,
    pub host_resp_tx: HostFuncRespTx,
}

impl<'a> std::fmt::Debug for PendingImportStackItem<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "PendingImportStackItem {{ func_desc: {:?}, results_count: {}, host_resp_tx: {:?} }}", self.func_desc, self.results_count, self.host_resp_tx)
    }
}

#[derive(Debug)]
/// Represents the state of an ongoing Wasm call.
/// The lifetime 'a is derived from the WasmInstanceState (specifically its Store)
/// with which this call state is associated. This means WasmCallState<'a>
/// must be used in a context where that WasmInstanceState is also live.
pub struct WasmCallState<'a> {
    pub instance_extra: WasmInstanceExtra,
    pub pending_import_stack: Vec<PendingImportStackItem<'a>>,
}

#[derive(Debug, Clone)]
pub enum CallStepResult {
    ImportCall(HostFuncRequest),
    Complete(WasmResultsVec),
}

pub fn call_init<'a>(instance_extra: WasmInstanceExtra) -> Result<WasmCallState<'a>> {
    trace!("call_init");

    let call_state = WasmCallState {
        instance_extra,
        pending_import_stack: vec![],
    };

    Ok(call_state)
}

pub async fn call_push_native<'a>(
    instance_state: &'a mut WasmInstanceState,
    call_state: &mut WasmCallState<'a>,
    native_request: NativeFuncRequest,
) -> Result<CallStepResult> {
    let module = &instance_state.module;

    let (func_inst, results_count) = match native_request.func_desc {
        NativeFuncDesc::Export(ref name) => {
            let func_type = module.get_export(name.as_str()).unwrap();
            let results_count_val = func_type.unwrap_func().results().count();
            let func = instance_state.instance.get_func(&mut instance_state.store, name.as_str()).unwrap();
            (func, results_count_val)
        },
        NativeFuncDesc::Indirect(_index) => unimplemented!(),
    };

    let mut wasm_future = Box::pin(async move {
        let mut results = vec![WasmVal::I32(0); results_count];
        func_inst.call_async(
            &mut instance_state.store,
            native_request.params.as_slice(),
            results.as_mut_slice(),
        ).await.unwrap();
        Ok::<Vec<WasmVal>, anyhow::Error>(results)
    });

    // Re-add Box::pin for host_func_request_future as recv() is not Unpin
    let mut host_func_request_future = Box::pin(call_state.instance_extra.host_req_channel.recv());

    let step_result = tokio::select! {
        call_result = &mut wasm_future => {
            match call_result {
                Ok(results) => {
                    debug!("Wasm future complete, results populated in provided slice");
                    drop(wasm_future);
                    CallStepResult::Complete(results)
                }
                Err(e) => {
                    panic!("Wasm future failed: {:?}", e);
                }
            }
        },
        Some((host_req, host_resp_tx)) = &mut host_func_request_future => {
            info!("Host request: {:?}", host_req);
            call_state.pending_import_stack.push(PendingImportStackItem {
                func_desc: host_req.func_desc.clone(),
                results_count,
                wasm_future,
                host_resp_tx,
            });
            let import_meta = HostFuncRequest {
                func_desc: host_req.func_desc,
                params: host_req.params.clone(),
            };
            CallStepResult::ImportCall(import_meta)
        },
        else => unreachable!(),
    };

    Ok(step_result)
}

pub async fn call_pop_host<'a>(
    call_state: &mut WasmCallState<'a>,
    host_response: HostFuncResponse,
) -> Result<CallStepResult> {
    trace!("call_pop_host");

    // First, pop the pending import stack and send the response
    let pending_import_stack_item = call_state.pending_import_stack.pop().unwrap();

    debug!("Popped pending import stack item: {:?}", pending_import_stack_item);

    let results_count = pending_import_stack_item.results_count;
    // let func_desc = pending_import_stack_item.func_desc;
    let mut wasm_future = pending_import_stack_item.wasm_future;

    // Then, send the response
    pending_import_stack_item.host_resp_tx.send(host_response).unwrap();
    
    let mut host_func_request_future = Box::pin(call_state.instance_extra.host_req_channel.recv());

    let step_result = tokio::select! {
        call_result = &mut wasm_future => {
            match call_result {
                Ok(results) => {
                    debug!("Wasm future complete, results populated in provided slice");
                    drop(wasm_future);
                    CallStepResult::Complete(results)
                }
                Err(e) => {
                    panic!("Wasm future failed: {:?}", e);
                }
            }
        },
        Some((host_req, host_resp_tx)) = &mut host_func_request_future => {
            info!("Host request: {:?}", host_req);
            call_state.pending_import_stack.push(PendingImportStackItem {
                func_desc: host_req.func_desc.clone(),
                results_count,
                wasm_future,
                host_resp_tx,
            });
            let import_meta = HostFuncRequest {
                func_desc: host_req.func_desc,
                params: host_req.params.clone(),
            };
            CallStepResult::ImportCall(import_meta)
        },
        else => unreachable!(),
    };

    Ok(step_result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tracing::info;
    use tracing_test::traced_test;

    #[tokio::test]
    #[traced_test]
    async fn test_init_wat() {
        let wat = r#"
            (module
            (import "host" "sleep_and_resume" (func $sleep (param i32) (result i32)))
            (func (export "run") (result i32)
                i32.const 5000
                call $sleep))
        "#;
        let module_data = WasmModuleData::Wat(wat);
        let res = instance_init(module_data).await.unwrap();
        assert_eq!(res.state.engine.is_async(), true);
    }

    #[tokio::test]
    #[traced_test]
    async fn test_init_binary() {
        let module_binary = include_bytes!("../fixture/test-64.wasm");
        let res = instance_init(WasmModuleData::Binary(module_binary))
            .await
            .unwrap();
        assert_eq!(res.state.engine.is_async(), true);
    }

    #[tokio::test]
    #[traced_test]
    async fn test_import_call() {
        let wat = r#"
            (module
            (import "host" "sleep_and_resume" (func $sleep (param i32) (result i32)))
            (func (export "run") (result i32)
                i32.const 5000
                call $sleep))
        "#;
        let module_data = WasmModuleData::Wat(wat);
        let mut res = instance_init(module_data).await.unwrap();
        let instance = res.state.instance;
        let mut store = res.state.store;
        let run = instance
            .get_typed_func::<(), i32>(&mut store, "run")
            .unwrap();
        // Pin the future as it's used in select! and might not be Unpin
        let mut wasm_future = Box::pin(run.call_async(&mut store, ()));

        info!("Waiting for request");

        loop {
            tokio::select! {
                call_result = &mut wasm_future => {
                    match call_result {
                        Ok(complete) => {
                            info!("Wasm future complete: {:?}", complete);
                            assert_eq!(complete, 9001);
                            break;
                        }
                        Err(e) => {
                            panic!("Wasm future failed: {:?}", e);
                        }
                    }
                },
                Some((host_req, host_resp_tx)) = res.extra.host_req_channel.recv() => {
                    info!("Host request: {:?}", host_req);
                    let host_resp = HostFuncResponse {
                        func_desc: host_req.func_desc,
                        results: vec![WasmVal::I32(9001)],
                    };
                    host_resp_tx.send(host_resp).unwrap();
                },
                else => unreachable!(),
            }
        }
    }

    #[tokio::test]
    #[traced_test]
    async fn test_call_init_push_pop() {
        let wat = r#"
            (module
            (import "host" "sleep_and_resume" (func $sleep (param i32) (result i32)))
            (func (export "run") (result i32)
                i32.const 5000
                call $sleep))
        "#;
        let module_data = WasmModuleData::Wat(wat);
        let init_res = instance_init(module_data).await.unwrap();
        
        let mut instance_state = init_res.state;
        let instance_extra = init_res.extra;
        let mut call_state = call_init(instance_extra).unwrap();

        let native_request = NativeFuncRequest {
            func_desc: NativeFuncDesc::Export("run".to_string()),
            params: vec![],
        };

        let res = call_push_native(
            &mut instance_state,
            &mut call_state,
            native_request
        ).await.unwrap();

        match res {
            CallStepResult::ImportCall(import_meta) => {
                info!("Import call: {:?}", import_meta);
                assert_eq!(import_meta.func_desc.module_name, "host");
                assert_eq!(import_meta.func_desc.field_name, "sleep_and_resume");
            }
            CallStepResult::Complete(_) => assert!(false, "Expected ImportCall, got Complete"),
        }
        
        trace!("Pending import stack: {:?}", call_state.pending_import_stack);
        assert_eq!(call_state.pending_import_stack.len(), 1);

        let host_response = HostFuncResponse {
            func_desc: HostFuncDesc {
                module_name: "host".to_string(),
                field_name: "sleep_and_resume".to_string(),
            },
            results: vec![WasmVal::I32(0)],
        };

        let pop_res = call_pop_host(&mut call_state, host_response).await.unwrap();
        
        
        // assert_eq!(pop_res, CallStepResult::Complete(vec![WasmVal::I32(0); 1]));
        error!("Pop res: {:?}", pop_res);
    }
}
