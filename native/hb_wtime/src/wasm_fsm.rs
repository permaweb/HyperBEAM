use crate::types::WasmVal;
pub use crate::wasm::{
    self, HostFuncRequest, HostFuncResponse, NativeFuncRequest, WasmCallState, WasmInstanceExtra,
    WasmInstanceState, WasmModuleData,
};
use anyhow::Result;
use strum_macros::Display;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum StepType {
    Begin,
    Resume,
}

// Represents the possible states of the Wasm instance FSM.
#[derive(Debug, Clone, PartialEq)]
pub enum StateTag {
    /// The instance is initialized and ready for a new call.
    Idle,
    /// A call has been started or resumed, and `step()` is expected next.
    PendingStep(StepType),
    /// The Wasm function called a host function (import) and is paused, waiting for the host response.
    AwaitingHost,
}

/// Represents the outcome of stepping the Wasm execution.
#[derive(Debug, Clone)]
pub enum CallOutcome {
    /// The Wasm function call completed successfully. Contains the results.
    Complete(Vec<WasmVal>),
    /// The Wasm function called an imported host function. Contains the request details.
    ImportCallNeeded(HostFuncRequest),
}

/// Errors specific to the FSM operation.
#[derive(Debug, Display)]
pub enum FsmError {
    #[strum(to_string = "Wasm initialization failed: {0}")]
    InitializationFailed(anyhow::Error),

    #[strum(to_string = "Wasm call failed: {0}")]
    CallFailed(anyhow::Error),

    #[strum(to_string = "Invalid state transition: cannot perform operation '{operation}' in state {current_state:?}")]
    InvalidState {
        operation: &'static str,
        current_state: StateTag,
    },

    #[strum(to_string = "Internal FSM Error: {0}")]
    InternalError(String),

    #[strum(to_string = "Export not found: {0}")]
    ExportNotFound(String),

    #[strum(to_string = "Indirect not found: {0}")]
    IndirectNotFound(String),

    #[strum(to_string = "Import not found: {0}")]
    ImportNotFound(String),

    #[strum(to_string = "Export '{0}' is not a function")]
    NotAFunction(String),
}

// Helper to convert wasm::Error into FsmError::CallFailed
impl From<wasm::CallStepResult> for CallOutcome {
    fn from(result: wasm::CallStepResult) -> Self {
        match result {
            wasm::CallStepResult::Complete(results) => CallOutcome::Complete(results),
            wasm::CallStepResult::ImportCall(req) => CallOutcome::ImportCallNeeded(req),
        }
    }
}

/// The Wasm FSM wrapper.
pub struct WasmFsm {
    // active_call_state must be declared before instance to ensure correct drop order.
    // It holds WasmCallState<'static> which contains futures borrowing from instance.store.
    active_call_state: Option<WasmCallState<'static>>,
    instance: WasmInstanceState,

    // Extra state (like the host channel receiver) needed for calls.
    // Held here when Idle, moved into WasmCallState when Running/AwaitingHost.
    idle_extra: Option<WasmInstanceExtra>,

    // Simple tag indicating the logical state of the FSM.
    state_tag: StateTag,
    // Tokio runtime handle needed for async operations within sync FSM methods
    // Cloned from the runtime used during initialization.
    rt_handle: tokio::runtime::Handle,
}

impl WasmFsm {
    // Private helper to safely get mutable access to the active call state
    // with the correct lifetime 'a tied to the borrow of self.instance.
    #[inline]
    fn get_active_call_state_mut<'a>(&'a mut self) -> Result<&'a mut WasmCallState<'a>, FsmError> {
        match self.active_call_state {
            Some(ref mut call_state) => {
                // SAFETY: We are transmuting the 'static lifetime back to 'a.
                // This is safe because:
                // 1. 'a is derived from a borrow of `self`, which owns `self.instance`.
                // 2. `active_call_state` logically borrows `self.instance.store`.
                // 3. The `active_call_state` (and the WasmCallState within it) will only
                //    live as long as the returned borrow `&'a mut WasmCallState<'a>`.
                // 4. The `WasmCallState` is always taken or dropped before `self.instance` is dropped.
                let state_ref_mut: &'a mut WasmCallState<'a> = unsafe {
                    std::mem::transmute::<&mut WasmCallState<'static>, &mut WasmCallState<'a>>(
                        call_state,
                    )
                };
                Ok(state_ref_mut)
            }
            None => Err(FsmError::InternalError(
                "Attempted to get active_call_state when None".to_string(),
            )),
        }
    }

    /// Creates a new Wasm FSM instance by initializing the wasmtime environment
    /// and loading the provided Wasm module data.
    ///
    /// Requires a Tokio runtime to perform async initialization.
    pub fn new(
        module_data: WasmModuleData,
        runtime: &tokio::runtime::Runtime,
    ) -> Result<Self, FsmError> {
        let rt_handle = runtime.handle().clone();
        let init_res = rt_handle
            .block_on(wasm::instance_init(module_data))
            .map_err(|e| FsmError::InitializationFailed(e))?;

        Ok(WasmFsm {
            instance: init_res.state,
            idle_extra: Some(init_res.extra),
            active_call_state: None,
            state_tag: StateTag::Idle,
            rt_handle,
        })
    }

    /// Starts a new Wasm function call.
    ///
    /// Can only be called when the FSM is in the `Idle` state.
    /// Transitions the FSM to the `Running` state.
    /// The caller should subsequently call `step()` to drive the execution.
    pub fn start_call(&mut self, native_request: NativeFuncRequest) -> Result<(), FsmError> {
        if self.state_tag != StateTag::Idle {
            return Err(FsmError::InvalidState {
                operation: "start_call",
                current_state: self.state_tag.clone(),
            });
        }

        let extra = self.idle_extra.take().ok_or_else(|| {
            FsmError::InternalError("idle_extra was None in Idle state".to_string())
        })?;

        // Initialize the call state within the wasm module. `extra` is consumed here.
        let mut call_state = wasm::call_init(extra).map_err(|e| FsmError::CallFailed(e))?; // If call_init fails, `extra` is dropped. self.idle_extra remains None.

        // Push the native call onto the wasm state
        // This needs mutable access to instance and the newly created call_state
        let push_result = self.rt_handle.block_on(wasm::call_push_native(
            &mut self.instance,
            &mut call_state,
            native_request,
        ));

        match push_result {
            Ok(()) => {
                // SAFETY: Storing the WasmCallState with a 'static lifetime.
                // This is safe because we will only access it via `get_active_call_state_mut`,
                // which transmutes it back to a lifetime tied to `self.instance`.
                // It will be `take()`n or dropped before `self.instance` is.
                self.active_call_state = Some(unsafe {
                    std::mem::transmute::<WasmCallState<'_>, WasmCallState<'static>>(call_state)
                });
                // Transition to PendingStep(Begin)
                self.state_tag = StateTag::PendingStep(StepType::Begin);
                Ok(())
            }
            Err(e) => {
                // Push failed, put idle_extra back and return error
                self.idle_extra = Some(call_state.instance_extra); // Retrieve extra from failed call_state
                self.state_tag = StateTag::Idle; // Remain Idle
                Err(FsmError::CallFailed(e))
            }
        }
    }

    /// Drives the Wasm execution forward by one step.
    ///
    /// Can only be called when the FSM is in the `PendingStep` state.
    /// Returns the outcome of the step: `Complete` or `ImportCallNeeded`.
    /// Transitions the FSM to `Idle` on completion, or `AwaitingHost` if an import is called.
    pub fn step(&mut self) -> Result<CallOutcome, FsmError> {
        // Use matches! to check for PendingStep(_) without comparing inner data
        if !matches!(self.state_tag, StateTag::PendingStep(_)) {
            return Err(FsmError::InvalidState {
                operation: "step",
                current_state: self.state_tag.clone(),
            });
        }

        // Clone the handle *before* establishing the mutable borrow for call_state.
        let rt_handle = self.rt_handle.clone();

        // Get the call state with the correct lifetime 'a derived from &'a mut self
        let call_state = self.get_active_call_state_mut()?;

        // Execute the next step of the Wasm computation using the cloned handle
        let step_result = rt_handle
            .block_on(wasm::call_step(call_state))
            .map_err(|e| {
                // Error occurred during step, clear state and go Idle
                let taken_call_state = self.active_call_state.take();
                self.idle_extra = taken_call_state.map(|cs| cs.instance_extra);
                self.state_tag = StateTag::Idle;
                FsmError::CallFailed(e)
            })?;

        match step_result {
            wasm::CallStepResult::Complete(results) => {
                // Call completed, take the call state, retrieve extra, go Idle
                let taken_call_state = self.active_call_state.take().ok_or_else(|| {
                    FsmError::InternalError("active_call_state was None after Complete".to_string())
                })?;
                self.idle_extra = Some(taken_call_state.instance_extra);
                self.state_tag = StateTag::Idle;
                Ok(CallOutcome::Complete(results))
            }
            wasm::CallStepResult::ImportCall(request) => {
                // Waiting for host, transition to AwaitingHost
                self.state_tag = StateTag::AwaitingHost;
                Ok(CallOutcome::ImportCallNeeded(request))
            }
        }
    }

    /// Provides the response from a host function call to resume Wasm execution.
    ///
    /// Can only be called when the FSM is in the `AwaitingHost` state.
    /// Transitions the FSM back to the `Running` state.
    /// The caller should subsequently call `step()` to drive the execution further.
    pub fn provide_host_response(
        &mut self,
        host_response: HostFuncResponse,
    ) -> Result<(), FsmError> {
        if self.state_tag != StateTag::AwaitingHost {
            return Err(FsmError::InvalidState {
                operation: "provide_host_response",
                current_state: self.state_tag.clone(),
            });
        }

        // Clone the handle *before* establishing the mutable borrow for call_state.
        let rt_handle = self.rt_handle.clone();

        // Get the call state with the correct lifetime 'a derived from &'a mut self
        let call_state = self.get_active_call_state_mut()?;

        // Resume the Wasm execution by popping the host response using the cloned handle
        rt_handle
            .block_on(wasm::call_pop_host(call_state, host_response))
            .map_err(|e| {
                // Error occurred during pop, clear state and go Idle
                let taken_call_state = self.active_call_state.take();
                self.idle_extra = taken_call_state.map(|cs| cs.instance_extra);
                self.state_tag = StateTag::Idle;
                FsmError::CallFailed(e)
            })?;

        // State is now ready to be stepped again
        self.state_tag = StateTag::PendingStep(StepType::Resume); // Transition to PendingStep(Resume)
        Ok(())
    }

    /// Returns a reference to the current state tag of the FSM.
    pub fn current_state(&self) -> &StateTag {
        &self.state_tag
    }

    // New method to get export parameter types
    pub fn get_native_func_param_types(
        &self,
        func_name: &str,
    ) -> Result<Vec<wasmtime::ValType>, FsmError> {
        let export_extern = self
            .instance
            .module
            .get_export(func_name)
            .ok_or_else(|| FsmError::ExportNotFound(func_name.to_string()))?;

        match export_extern.func() {
            Some(func) => Ok(func.params().collect()),
            None => Err(FsmError::NotAFunction(func_name.to_string())),
        }
    }

    pub fn get_last_host_func_result_types(&self) -> Result<Vec<wasmtime::ValType>, FsmError> {
        // Borrow active_call_state and the item in pending_import_stack
        let call_state_ref = self.active_call_state.as_ref().ok_or_else(|| {
            FsmError::InternalError("active_call_state is None when getting host func result types".to_string())
        })?;

        let pending_item_ref = call_state_ref.pending_import_stack.last().ok_or_else(|| {
            FsmError::InternalError("pending_import_stack is empty".to_string())
        })?;

        // Clone func_desc to get an owned value if needed, or work with references if possible
        let func_desc = pending_item_ref.func_desc.clone(); // Clone HostFuncDesc

        let import_type = self
            .instance
            .module
            .imports()
            .find(|import| {
                import.module() == func_desc.module_name && import.name() == func_desc.field_name
            })
            .ok_or_else(|| {
                FsmError::ImportNotFound(format!(
                    "{}:{}",
                    func_desc.module_name, func_desc.field_name
                ))
            })?;
        
        match import_type.ty() {
            wasmtime::ExternType::Func(func) => Ok(func.results().collect()),
            _ => Err(FsmError::NotAFunction(format!("{}:{}", func_desc.module_name, func_desc.field_name))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::wasm::WasmModuleData;
    use wasmtime::Val;

    // Helper to create a runtime and FSM instance for tests
    fn setup_fsm(wat: &'static str) -> (tokio::runtime::Runtime, WasmFsm) {
        let runtime = tokio::runtime::Runtime::new().unwrap();
        let module_data = WasmModuleData::Wat(wat);
        let fsm = WasmFsm::new(module_data, &runtime).expect("FSM creation failed");
        (runtime, fsm)
    }

    #[test]
    fn test_fsm_new_and_state() {
        let wat = r#"(module)"#;
        let (_runtime, fsm) = setup_fsm(wat);
        assert_eq!(*fsm.current_state(), StateTag::Idle);
    }

    #[test]
    fn test_fsm_simple_call_complete() {
        let wat = r#"
            (module
              (func (export "run") (result i32) i32.const 42)
            )
        "#;
        let (_runtime, mut fsm) = setup_fsm(wat);

        let request = NativeFuncRequest {
            func_desc: wasm::NativeFuncDesc::Export("run".to_string()),
            params: vec![],
        };

        assert_eq!(*fsm.current_state(), StateTag::Idle);
        fsm.start_call(request).expect("start_call failed");
        assert_eq!(*fsm.current_state(), StateTag::PendingStep(StepType::Begin));

        match fsm.step().expect("step failed") {
            CallOutcome::Complete(results) => {
                assert_eq!(results.len(), 1);
                assert_eq!(results[0].i32(), Some(42));
            }
            CallOutcome::ImportCallNeeded(_) => panic!("Expected Complete, got ImportCallNeeded"),
        }
        assert_eq!(*fsm.current_state(), StateTag::Idle);
    }

    #[test]
    fn test_fsm_import_call_resume_complete() {
        let wat = r#"
            (module
              (import "host" "add" (func $host_add (param i32 i32) (result i32)))
              (func (export "run") (param i32 i32) (result i32)
                local.get 0
                local.get 1
                call $host_add
              )
            )
        "#;
        let (_runtime, mut fsm) = setup_fsm(wat);

        let request = NativeFuncRequest {
            func_desc: wasm::NativeFuncDesc::Export("run".to_string()),
            params: vec![Val::I32(10), Val::I32(20)],
        };

        // Start -> PendingStep
        fsm.start_call(request).expect("start_call failed");
        assert_eq!(*fsm.current_state(), StateTag::PendingStep(StepType::Begin));

        // Step -> AwaitingHost
        let import_request = match fsm.step().expect("step 1 failed") {
            CallOutcome::ImportCallNeeded(req) => req,
            CallOutcome::Complete(_) => panic!("Expected ImportCallNeeded, got Complete"),
        };
        assert_eq!(*fsm.current_state(), StateTag::AwaitingHost);
        assert_eq!(import_request.func_desc.module_name, "host");
        assert_eq!(import_request.func_desc.field_name, "add");
        assert_eq!(import_request.params.len(), 2);
        assert_eq!(import_request.params[0].i32(), Some(10));
        assert_eq!(import_request.params[1].i32(), Some(20));

        // Provide Response -> PendingStep
        let host_response = HostFuncResponse {
            func_desc: import_request.func_desc,
            results: vec![Val::I32(30)], // 10 + 20
        };
        fsm.provide_host_response(host_response)
            .expect("provide_host_response failed");
        assert_eq!(
            *fsm.current_state(),
            StateTag::PendingStep(StepType::Resume)
        );

        // Step -> Idle (Complete)
        match fsm.step().expect("step 2 failed") {
            CallOutcome::Complete(results) => {
                assert_eq!(results.len(), 1);
                assert_eq!(results[0].i32(), Some(30));
            }
            CallOutcome::ImportCallNeeded(_) => panic!("Expected Complete, got ImportCallNeeded"),
        }
        assert_eq!(*fsm.current_state(), StateTag::Idle);
    }

    #[test]
    fn test_fsm_invalid_state_transitions() {
        let wat = r#"(module (func (export "run")))"#;
        let (_runtime, mut fsm) = setup_fsm(wat);

        let request = NativeFuncRequest {
            func_desc: wasm::NativeFuncDesc::Export("run".to_string()),
            params: vec![],
        };
        let response = HostFuncResponse {
            func_desc: wasm::HostFuncDesc {
                module_name: "".into(),
                field_name: "".into(),
            },
            results: vec![],
        };

        // Cannot step or provide response when Idle
        assert!(matches!(
            fsm.step(),
            Err(FsmError::InvalidState {
                operation: "step",
                current_state: StateTag::Idle
            })
        ));
        assert!(matches!(
            fsm.provide_host_response(response.clone()),
            Err(FsmError::InvalidState {
                operation: "provide_host_response",
                current_state: StateTag::Idle
            })
        ));

        // Start call -> PendingStep
        fsm.start_call(request.clone()).unwrap();
        assert_eq!(*fsm.current_state(), StateTag::PendingStep(StepType::Begin));

        // Cannot start call or provide response when PendingStep
        assert!(matches!(
            fsm.start_call(request.clone()),
            Err(FsmError::InvalidState {
                operation: "start_call",
                current_state: StateTag::PendingStep(StepType::Begin)
            })
        ));
        assert!(matches!(
            fsm.provide_host_response(response.clone()),
            Err(FsmError::InvalidState {
                operation: "provide_host_response",
                current_state: StateTag::PendingStep(StepType::Begin)
            })
        ));

        // Step to complete -> Idle
        match fsm.step() {
            Ok(CallOutcome::Complete(_)) => (),
            Err(e) => panic!("Step failed: {:?}", e),
            _ => panic!("Unexpected step outcome"),
        };
        assert_eq!(*fsm.current_state(), StateTag::Idle);
    }

    #[test]
    fn test_fsm_invalid_state_transitions_awaiting() {
        let wat = r#"
            (module
              (import "host" "pause" (func $host_pause))
              (func (export "run") call $host_pause)
            )
        "#;
        let (_runtime, mut fsm) = setup_fsm(wat);

        let request = NativeFuncRequest {
            func_desc: wasm::NativeFuncDesc::Export("run".to_string()),
            params: vec![],
        };
        let response = HostFuncResponse {
            func_desc: wasm::HostFuncDesc {
                module_name: "host".into(),
                field_name: "pause".into(),
            },
            results: vec![],
        };

        fsm.start_call(request.clone()).expect("start_call failed");
        match fsm.step() {
            Ok(CallOutcome::ImportCallNeeded(_)) => (),
            Err(e) => panic!("Step failed: {:?}", e),
            _ => panic!("Unexpected step outcome"),
        };
        assert_eq!(*fsm.current_state(), StateTag::AwaitingHost);

        // Cannot start call or step when AwaitingHost
        assert!(matches!(
            fsm.start_call(request.clone()),
            Err(FsmError::InvalidState {
                operation: "start_call",
                current_state: StateTag::AwaitingHost
            })
        ));
        assert!(matches!(
            fsm.step(),
            Err(FsmError::InvalidState {
                operation: "step",
                current_state: StateTag::AwaitingHost
            })
        ));

        // Provide response -> PendingStep
        fsm.provide_host_response(response).unwrap();
        assert_eq!(
            *fsm.current_state(),
            StateTag::PendingStep(StepType::Resume)
        );
    }
}
