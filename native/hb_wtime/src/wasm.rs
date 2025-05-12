use crate::types::WasmVal;
use std::{future::Future, pin::Pin};

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

pub type HostFuncChannel = mpsc::Receiver<(HostFuncRequest, HostFuncRespTx)>;

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
pub async fn wasm_instance_create(module_binary: WasmModuleData) -> Result<InstanceInitResult> {
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
    cfg.wasm_relaxed_simd(true); // determinism handled by `relaxed_simd_deterministic`

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

    for import in module.imports() {
        let module_name_ref = import.module();
        let field_name_ref = import.name();
        trace!(
            "Creating import function for: {}.{}",
            module_name_ref,
            field_name_ref
        );

        let ty = match import.ty().func() {
            Some(ty) => ty.clone(),
            None => return Err(anyhow::anyhow!("Import item '{}.{}' is not a function type", module_name_ref, field_name_ref)),
        };

        let module_name_owned = import.module().to_string();
        let field_name_owned = import.name().to_string();

        let current_host_req_tx = host_req_tx.clone();

        linker.func_new_async(
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

                let moved_host_req_tx = current_host_req_tx.clone();

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

                    if moved_host_req_tx.send((request, resp_tx)).await.is_err() {
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
        )
        .map_err(|e| anyhow::anyhow!("Failed to link import function '{}.{}': {}", module_name_ref, field_name_ref, e))?;
    }

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

    let res = InstanceInitResult { state, extra };

    trace!("Instance init result: {:?}", res);

    Ok(res)
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
        write!(
            f,
            "PendingImportStackItem {{ func_desc: {:?}, results_count: {}, host_resp_tx: {:?} }}",
            self.func_desc, self.results_count, self.host_resp_tx
        )
    }
}

// Removed #[derive(Debug)] as Debug is manually implemented
pub struct WasmCallState<'a> {
    pub pending_import_stack: Vec<PendingImportStackItem<'a>>,
    pub current_future:
        Option<Pin<Box<dyn Future<Output = Result<Vec<WasmVal>, anyhow::Error>> + Send + 'a>>>,
    pub current_results_count: Option<usize>,
}

// Manually implement Debug to exclude the non-Debug Future field.
impl<'a> std::fmt::Debug for WasmCallState<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("WasmCallState")
            .field("pending_import_stack", &self.pending_import_stack)
            .field("current_results_count", &self.current_results_count)
            .finish_non_exhaustive()
    }
}

#[derive(Debug, Clone)]
pub enum CallStepResult {
    ImportCall(HostFuncRequest),
    Complete(WasmResultsVec),
}

pub fn wasm_call_init<'a>() -> Result<WasmCallState<'a>> {
    trace!("call_init");

    let call_state = WasmCallState {
        pending_import_stack: vec![],
        current_future: None,
        current_results_count: None,
    };

    Ok(call_state)
}

// Make call_step public
pub async fn wasm_call_step<'a>(
    call_state: &mut WasmCallState<'a>,
    host_req_channel: &mut HostFuncChannel, // Add host_req_channel argument
) -> Result<CallStepResult> {
    // Take the future and count from the state. Error if not present.
    let mut wasm_future = call_state
        .current_future
        .take()
        .ok_or_else(|| anyhow::anyhow!("call_step called but no current_future in state"))?;
    let results_count = call_state
        .current_results_count
        .take()
        .ok_or_else(|| anyhow::anyhow!("call_step called but no current_results_count in state"))?;

    // Use the passed-in host_req_channel
    let mut host_func_request_future = Box::pin(host_req_channel.recv());

    tokio::select! {
        biased;

        call_result = &mut wasm_future => {
            match call_result {
                Ok(results) => {
                    debug!("Wasm future completed successfully.");
                    // Future completed, no need to put it back in state.
                    Ok(CallStepResult::Complete(results))
                }
                Err(e) => {
                    error!("Wasm future failed: {:?}", e);
                    // Future failed, no need to put it back in state.
                    Err(anyhow::anyhow!("Wasm future failed: {:?}", e))
                }
            }
        },
        maybe_host_req = &mut host_func_request_future => {
            match maybe_host_req {
                Some((host_req, host_resp_tx)) => {
                    info!("Host request received by call_step: {:?}, results_count for original call: {}", host_req.func_desc, results_count);
                    // Push the currently executing future onto the stack.
                    call_state.pending_import_stack.push(PendingImportStackItem {
                        func_desc: host_req.func_desc.clone(),
                        results_count, // Expected results count for the original Wasm call that led to this (or a parent) host call.
                        wasm_future, // Move the future here.
                        host_resp_tx,
                    });
                    let import_meta = HostFuncRequest {
                        func_desc: host_req.func_desc,
                        params: host_req.params,
                    };
                    // The future is now on the stack, not active.
                    Ok(CallStepResult::ImportCall(import_meta))
                }
                None => {
                    error!("Host request channel closed while call_step was awaiting a host request. This is unexpected if a Wasm future is pending.");
                    // Even though the future failed, we already took it out of the state.
                    Err(anyhow::anyhow!("Host request channel closed, Wasm future cannot proceed if it requires host calls."))
                }
            }
        },
    }
}

pub async fn wasm_call_push_native<'a>(
    instance_state: &'a mut WasmInstanceState,
    call_state: &mut WasmCallState<'a>, // Takes mutable call_state
    native_request: NativeFuncRequest,
) -> Result<()> {
    // Returns Result<()> instead of the future/count tuple
    trace!("call_push_native: {:?}", native_request.func_desc);
    let module = &instance_state.module;

    let (func_inst, results_count) = match native_request.func_desc {
        NativeFuncDesc::Export(ref name) => {
            let export_extern = module
                .get_export(name.as_str())
                .ok_or_else(|| anyhow::anyhow!("Export '{}' not found", name))?;

            let func_ty = match export_extern {
                wasmtime::ExternType::Func(ft) => ft,
                _ => return Err(anyhow::anyhow!("Export '{}' is not a function", name)),
            };
            let results_count_val = func_ty.results().count();

            let func = instance_state
                .instance
                .get_func(&mut instance_state.store, name.as_str())
                .ok_or_else(|| {
                    anyhow::anyhow!("Function instance not found for export: {}", name)
                })?;
            (func, results_count_val)
        }
        NativeFuncDesc::Indirect(_index) => {
            unimplemented!("Indirect function calls not yet implemented")
        }
    };

    let params_owned = native_request.params.clone();
    let func_desc_clone = native_request.func_desc.clone(); // Clone for use in error message

    let wasm_future: Pin<
        Box<dyn Future<Output = Result<Vec<WasmVal>, anyhow::Error>> + Send + 'a>,
    > = Box::pin(async move {
        let mut temp_results_buffer = vec![WasmVal::I32(0); results_count];

        // Important: Need mutable access to instance_state.store here.
        // The lifetime 'a ensures instance_state outlives the future.
        func_inst
            .call_async(
                &mut instance_state.store,
                &params_owned,
                temp_results_buffer.as_mut_slice(),
            )
            .await
            .map_err(|e| {
                anyhow::anyhow!(
                    "Wasm function call_async failed for {:?}: {:?}",
                    func_desc_clone,
                    e
                )
            })?;

        Ok(temp_results_buffer)
    });

    // Store the future and results_count in the call_state.
    call_state.current_future = Some(wasm_future);
    call_state.current_results_count = Some(results_count);
    Ok(())
}

pub async fn wasm_call_pop_host<'a>(
    call_state: &mut WasmCallState<'a>, // Takes mutable call_state
    host_response: HostFuncResponse,
) -> Result<()> {
    // Returns Result<()> instead of the future/count tuple
    trace!(
        "call_pop_host for {:?}, response: {:?}",
        host_response.func_desc,
        host_response.results
    );

    let pending_item = call_state
        .pending_import_stack
        .pop()
        .ok_or_else(|| anyhow::anyhow!("call_pop_host called with empty pending import stack"))?;

    debug!(
        "Popped pending import item for {:?}. Results count for original call: {}",
        pending_item.func_desc, pending_item.results_count
    );

    if pending_item
        .host_resp_tx
        .send(host_response.clone())
        .is_err()
    {
        error!(
            "Failed to send host response; Wasm receiver was dropped for {:?}.",
            pending_item.func_desc
        );
        return Err(anyhow::anyhow!(
            "Wasm future for {:?} dropped its response receiver. Host response was: {:?}",
            pending_item.func_desc,
            host_response
        ));
    }

    // Store the resumed future and original results count in the call_state
    call_state.current_future = Some(pending_item.wasm_future);
    call_state.current_results_count = Some(pending_item.results_count);
    Ok(())
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
        let res = wasm_instance_create(module_data).await.unwrap();
        assert_eq!(res.state.engine.is_async(), true);
    }

    #[tokio::test]
    #[traced_test]
    async fn test_init_binary() {
        let module_binary = include_bytes!("../fixture/test-64.wasm");
        let res = wasm_instance_create(WasmModuleData::Binary(module_binary))
            .await
            .unwrap();
        assert_eq!(res.state.engine.is_async(), true);
    }

    #[tokio::test]
    #[traced_test]
    async fn test_call_repeatedly() {
        // Wasm that increments a global counter and returns the new value.
        let wat = r#"
            (module
              (global $counter (mut i32) (i32.const 0))
              (func (export "run") (result i32)
                global.get $counter
                i32.const 1
                i32.add
                global.set $counter ;; Set $counter to the new value
                global.get $counter ;; Return the new value of $counter
              )
            )
        "#;
        let module_data = WasmModuleData::Wat(wat);
        let init_result = wasm_instance_create(module_data).await.unwrap();
        let mut init = init_result.state; // WasmInstanceState
        let mut current_instance_extra = init_result.extra; // Re-add initialization of current_instance_extra

        for i in 0..10 {
            let mut call_state = wasm_call_init().unwrap();

            let native_request = NativeFuncRequest {
                func_desc: NativeFuncDesc::Export("run".to_string()),
                params: vec![],
            };

            wasm_call_push_native(&mut init, &mut call_state, native_request).await.unwrap();
            // Correctly use current_instance_extra.host_req_channel
            let step_result = wasm_call_step(&mut call_state, &mut current_instance_extra.host_req_channel).await.unwrap();
            
            match step_result {
                CallStepResult::Complete(results) => {
                    assert_eq!(results.len(), 1);
                    assert_eq!(results[0].i32(), Some((i + 1) as i32));
                    // current_instance_extra is taken and put back implicitly by the loop structure for this test
                    // if it were a more complex scenario, we might need to extract it from call_state
                    // but since WasmCallState no longer holds it, this is managed externally.
                }
                CallStepResult::ImportCall(ic) => {
                    panic!("Expected Complete, got ImportCall: {:?}", ic);
                }
            };
        }
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
        let res = wasm_instance_create(module_data).await.unwrap();
        let mut init = res.state;
        
        let mut instance_extra = res.extra; // Make instance_extra mutable
        let mut call_state = wasm_call_init().unwrap();

        let native_request = NativeFuncRequest {
            func_desc: NativeFuncDesc::Export("run".to_string()),
            params: vec![],
        };

        // call_push_native now mutates call_state and returns Result<()>
        wasm_call_push_native(&mut init, &mut call_state, native_request)
            .await
            .unwrap();

        // Call call_step, which takes the future from call_state
        let res = wasm_call_step(&mut call_state, &mut instance_extra.host_req_channel).await.unwrap();

        let import_meta = match res {
            CallStepResult::ImportCall(ic) => ic,
            CallStepResult::Complete(results) => {
                panic!("Expected ImportCall, got Complete({:?})", results)
            }
        };

        assert_eq!(import_meta.func_desc.module_name, "host");
        assert_eq!(import_meta.func_desc.field_name, "sleep_and_resume");
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
        let init_res = wasm_instance_create(module_data).await.unwrap();

        let mut instance_state = init_res.state;
        let mut instance_extra = init_res.extra; // Make instance_extra mutable
        let mut call_state = wasm_call_init().unwrap();

        let native_request = NativeFuncRequest {
            func_desc: NativeFuncDesc::Export("run".to_string()),
            params: vec![],
        };

        // Push native call, mutates state
        wasm_call_push_native(&mut instance_state, &mut call_state, native_request)
            .await
            .unwrap();

        // Call call_step to get the first result (ImportCall)
        let push_step_res = wasm_call_step(&mut call_state, &mut instance_extra.host_req_channel).await.unwrap();

        // Expect the first step to be an import call
        let import_meta = match push_step_res {
            CallStepResult::ImportCall(ic) => ic,
            CallStepResult::Complete(results) => {
                panic!("Expected ImportCall, got Complete({:?})", results)
            }
        };

        // Prepare and send host response
        let host_response = HostFuncResponse {
            func_desc: import_meta.func_desc.clone(),
            results: vec![wasmtime::Val::I32(12345)],
        };
        info!("Sending host response: {:?}", host_response);

        // Pop host response, mutates state to prepare for next step
        wasm_call_pop_host(&mut call_state, host_response).await.unwrap();

        // Call call_step again with the resumed future now in state
        let pop_res = wasm_call_step(&mut call_state, &mut instance_extra.host_req_channel).await.unwrap();

        // Expect completion
        match pop_res {
            CallStepResult::Complete(results) => {
                info!("Final Wasm execution completed with results: {:?}", results);
                assert_eq!(
                    format!("{:?}", results),
                    format!("{:?}", vec![wasmtime::Val::I32(12345)])
                );
            }
            CallStepResult::ImportCall(ic) => panic!(
                "Expected Complete, got ImportCall({:?}) after host response",
                ic
            ),
        }

        assert_eq!(call_state.pending_import_stack.len(), 0);
        info!("Test test_call_init_push_pop completed successfully.");
    }
}
