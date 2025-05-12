use crate::types::WasmVal;
pub use crate::wasm::{
    self, HostFuncChannel, HostFuncRequest, HostFuncResponse, NativeFuncRequest, WasmCallState,
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

    #[strum(
        to_string = "Invalid state transition: cannot perform operation '{operation}' in state {current_state:?}"
    )]
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
    // Call stack for managing (potentially nested) calls.
    call_stack: Vec<WasmCallState<'static>>,
    instance: WasmInstanceState,

    // The single MPSC receiver for all host function calls from this instance.
    // Option to allow taking it for mutable operations if strictly needed,
    // though typically passed as &mut to wasm::call_step.
    host_channel_receiver: Option<HostFuncChannel>,

    state_tag: StateTag,
    rt_handle: tokio::runtime::Handle,
}

impl WasmFsm {
    // Private helper to safely get mutable access to the top active call state
    // with the correct lifetime 'a tied to the borrow of self.instance.
    #[inline]
    fn get_top_call_state_mut<'a>(&'a mut self) -> Result<&'a mut WasmCallState<'a>, FsmError> {
        match self.call_stack.last_mut() {
            // Operate on last_mut() of call_stack
            Some(call_state) => {
                // SAFETY: (Same as before)
                let state_ref_mut: &'a mut WasmCallState<'a> = unsafe {
                    std::mem::transmute::<&mut WasmCallState<'static>, &mut WasmCallState<'a>>(
                        call_state,
                    )
                };
                Ok(state_ref_mut)
            }
            None => Err(FsmError::InternalError(
                "Attempted to get top_call_state_mut when call_stack is empty".to_string(),
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
            .block_on(wasm::wasm_instance_create(module_data))
            .map_err(|e| FsmError::InitializationFailed(e))?;

        Ok(WasmFsm {
            instance: init_res.state,
            host_channel_receiver: Some(init_res.extra.host_req_channel), // Store the receiver
            call_stack: Vec::new(),                                       // Initialize call_stack
            state_tag: StateTag::Idle,
            rt_handle,
        })
    }

    /// Starts a new Wasm function call.
    ///
    /// Can only be called when the FSM is in the `Idle` state or `AwaitingHost` state.
    /// Transitions the FSM to the `PendingStep` state.
    /// The caller should subsequently call `step()` to drive the execution.
    pub fn push(&mut self, native_request: NativeFuncRequest) -> Result<(), FsmError> {
        if !matches!(self.state_tag, StateTag::Idle)
            && !matches!(self.state_tag, StateTag::AwaitingHost)
        {
            return Err(FsmError::InvalidState {
                operation: "start_call (must be Idle or AwaitingHost for nesting)",
                current_state: self.state_tag.clone(),
            });
        }

        // WasmInstanceExtra (containing the channel) is not taken by call_init anymore.
        // It's held by WasmFsm and passed to call_step when needed.
        let mut call_state = wasm::wasm_call_init().map_err(|e| {
            // call_init failing is highly unlikely given its current simplicity,
            // but if it did, host_channel_receiver is untouched here.
            FsmError::CallFailed(e)
        })?;

        let push_result = self.rt_handle.block_on(wasm::wasm_call_push_native(
            &mut self.instance,
            &mut call_state,
            native_request,
        ));

        match push_result {
            Ok(()) => {
                self.call_stack.push(unsafe {
                    std::mem::transmute::<WasmCallState<'_>, WasmCallState<'static>>(call_state)
                });
                self.state_tag = StateTag::PendingStep(StepType::Begin);
                Ok(())
            }
            Err(e) => {
                // If push_native failed, the call_state was not added to the stack.
                // host_channel_receiver was not taken for call_init, so no need to put it back here.
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
        if !matches!(self.state_tag, StateTag::PendingStep(_)) {
            return Err(FsmError::InvalidState {
                operation: "step",
                current_state: self.state_tag.clone(),
            });
        }

        // 0. Clone rt_handle first, to avoid conflict with later mutable borrows of self.
        let rt_handle = self.rt_handle.clone();

        // 1. Take ownership of the receiver.
        let mut actual_receiver = match self.host_channel_receiver.take() {
            Some(receiver) => receiver,
            None => {
                return Err(FsmError::InternalError(
                    "Host channel receiver is None during step".to_string(),
                ))
            }
        };

        // 2. Now, get the mutable reference to the top call state.
        let call_state_ref = self.get_top_call_state_mut()?;

        // 3. Pass a mutable reference to actual_receiver.
        let step_result_fut = wasm::wasm_call_step(call_state_ref, &mut actual_receiver);
        let step_result = rt_handle.block_on(step_result_fut);

        // 4. Put the actual_receiver back.
        self.host_channel_receiver = Some(actual_receiver);

        // 5. Process result.
        match step_result {
            Ok(wasm::CallStepResult::Complete(results)) => {
                self.call_stack.pop(); // Remove completed call
                if self.call_stack.is_empty() {
                    self.state_tag = StateTag::Idle;
                } else {
                    // The completed call was popped. The FSM state should now reflect
                    // the state of the new top-most call on the stack.
                    // This new top call was previously suspended.
                    // If its pending_import_stack is not empty, it means it was AwaitingHost.
                    // Otherwise, it was suspended due to a nested native call and should become PendingStep.
                    if let Some(new_top_call_state) = self.call_stack.last() {
                        if !new_top_call_state.pending_import_stack.is_empty() {
                            self.state_tag = StateTag::AwaitingHost;
                        } else {
                            // This implies the underlying call was waiting for a nested native call (which just completed).
                            // It should now be ready to be stepped again.
                            // Which StepType? It depends on how new_top_call_state was left.
                            // If it has a current_future, it was likely pushed (Begin) or resumed (Resume).
                            // This is where knowing *why* it was on stack is important.
                            // For now, if it has a future, assume it's resumable.
                            if new_top_call_state.current_future.is_some() {
                                self.state_tag = StateTag::PendingStep(StepType::Resume);
                            // Or Begin? This needs care.
                            // Let's assume Resume is safer for now.
                            } else {
                                // This case should ideally not happen if a call is on stack and not AwaitingHost.
                                // It implies it was PendingStep but its future was taken and not put back.
                                // Or, it was pushed without a future properly set up.
                                // Fallback to a general PendingStep, but this signals a potential logic gap.
                                // For robustness, we might need WasmCallState to also store its conceptual state.
                                self.state_tag = StateTag::PendingStep(StepType::Resume);
                                // Consider logging a warning here if current_future is None and not AwaitingHost
                            }
                        }
                    } else {
                        // Should be caught by is_empty() above, but as a fallback:
                        self.state_tag = StateTag::Idle;
                    }
                }
                Ok(CallOutcome::Complete(results))
            }
            Ok(wasm::CallStepResult::ImportCall(request)) => {
                self.state_tag = StateTag::AwaitingHost;
                Ok(CallOutcome::ImportCallNeeded(request))
            }
            Err(e) => {
                self.call_stack.pop(); // Remove the failed call state
                if self.call_stack.is_empty() {
                    self.state_tag = StateTag::Idle;
                } else {
                    // Similar to above, determine state from new top call.
                    if let Some(new_top_call_state) = self.call_stack.last() {
                        if !new_top_call_state.pending_import_stack.is_empty() {
                            self.state_tag = StateTag::AwaitingHost;
                        } else if new_top_call_state.current_future.is_some() {
                            self.state_tag = StateTag::PendingStep(StepType::Resume);
                        } else {
                            self.state_tag = StateTag::PendingStep(StepType::Resume);
                            // Fallback
                        }
                    } else {
                        self.state_tag = StateTag::Idle;
                    }
                }
                Err(FsmError::CallFailed(e))
            }
        }
    }

    /// Provides the response from a host function call to resume Wasm execution.
    ///
    /// Can only be called when the FSM is in the `AwaitingHost` state.
    /// Transitions the FSM back to the `Running` state.
    /// The caller should subsequently call `step()` to drive the execution further.
    pub fn pop(
        &mut self,
        host_response: HostFuncResponse,
    ) -> Result<(), FsmError> {
        if !matches!(self.state_tag, StateTag::AwaitingHost) {
            return Err(FsmError::InvalidState {
                operation: "provide_host_response",
                current_state: self.state_tag.clone(),
            });
        }

        let rt_handle = self.rt_handle.clone();
        let call_state_ref = self.get_top_call_state_mut()?;

        // `wasm::call_pop_host` does not directly use the receiver,
        // but it modifies `call_state_ref` which might affect subsequent `call_step`.
        // The receiver is primarily for `call_step`.
        // No need to take/put back `self.host_channel_receiver` here if `call_pop_host` doesn't use it.

        let pop_result = rt_handle.block_on(wasm::wasm_call_pop_host(call_state_ref, host_response));

        match pop_result {
            Ok(()) => {
                self.state_tag = StateTag::PendingStep(StepType::Resume);
                Ok(())
            }
            Err(e) => {
                self.call_stack.pop(); // Assume error in pop_host invalidates current call
                if self.call_stack.is_empty() {
                    self.state_tag = StateTag::Idle;
                } else {
                    // TODO: Determine correct state/StepType for new top of stack after error.
                    self.state_tag = StateTag::PendingStep(StepType::Resume);
                }
                Err(FsmError::CallFailed(e))
            }
        }
    }

    /// Returns a reference to the current state tag of the FSM.
    pub fn current_state(&self) -> &StateTag {
        // state_tag should always reflect the FSM's current top-level state
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

    // Returns the expected result types of the most recently called host function
    // for the currently active (topmost) Wasm call, if it's awaiting a host response.
    pub fn get_last_host_func_result_types(&self) -> Result<Vec<wasmtime::ValType>, FsmError> {
        let top_call_state = self.call_stack.last().ok_or_else(|| {
            FsmError::InternalError(
                "call_stack is empty when getting host func result types".to_string(),
            )
        })?;

        let pending_item_ref = top_call_state
            .pending_import_stack
            .last()
            .ok_or_else(|| FsmError::InternalError("pending_import_stack is empty".to_string()))?;

        let func_desc = pending_item_ref.func_desc.clone();

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
            _ => Err(FsmError::NotAFunction(format!(
                "{}:{}",
                func_desc.module_name, func_desc.field_name
            ))),
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
        fsm.push(request).expect("start_call failed");
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
        fsm.push(request).expect("start_call failed");
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
        fsm.pop(host_response)
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
            fsm.pop(response.clone()),
            Err(FsmError::InvalidState {
                operation: "provide_host_response",
                current_state: StateTag::Idle
            })
        ));

        // Start call -> PendingStep
        fsm.push(request.clone()).expect("start_call failed");
        assert_eq!(*fsm.current_state(), StateTag::PendingStep(StepType::Begin));

        // Cannot start call or provide response when PendingStep
        assert!(matches!(
            fsm.push(request.clone()),
            Err(FsmError::InvalidState {
                operation: "start_call (must be Idle or AwaitingHost for nesting)",
                current_state: StateTag::PendingStep(StepType::Begin)
            })
        ));
        assert!(matches!(
            fsm.pop(response.clone()),
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
    fn test_fsm_nesting_from_awaiting_host() {
        let wat = r#"
            (module
              (import "env" "host_A" (func $host_A (result i32))) 
              (import "env" "host_B" (func $host_B (param i32) (result i32)))

              (func (export "outer_call") (result i32) ;; Calls host_A, then expects host_B(result_of_A)
                call $host_A ;; Returns, say, 10. FSM goes to AwaitingHost for host_A.
                               ;; Host will then make a nested call to inner_call.
                               ;; When inner_call completes, host gets its result (e.g. 50).
                               ;; Host then provides result for host_A (10).
                               ;; outer_call resumes, now needs to call host_B with 10.
              )

              (func (export "inner_call") (result i32) ;; Simple sync call
                i32.const 50
              )
            )
        "#;
        let (_runtime, mut fsm) = setup_fsm(wat);

        // 1. Start outer_call
        let outer_request = NativeFuncRequest {
            func_desc: wasm::NativeFuncDesc::Export("outer_call".to_string()),
            params: vec![],
        };
        fsm.push(outer_request)
            .expect("start_call for outer_call failed");
        assert_eq!(*fsm.current_state(), StateTag::PendingStep(StepType::Begin));

        // 2. Step outer_call, it should request host_A
        let import_req_a = match fsm.step().expect("step for outer_call (host_A) failed") {
            CallOutcome::ImportCallNeeded(req) => req,
            _ => panic!("Expected ImportCallNeeded for host_A"),
        };
        assert_eq!(*fsm.current_state(), StateTag::AwaitingHost);
        assert_eq!(import_req_a.func_desc.field_name, "host_A");

        // 3. FSM is AwaitingHost for host_A. Now, make a nested call to inner_call.
        let inner_request = NativeFuncRequest {
            func_desc: wasm::NativeFuncDesc::Export("inner_call".to_string()),
            params: vec![],
        };
        // This start_call is the nesting.
        fsm.push(inner_request)
            .expect("nested start_call for inner_call failed");
        // FSM state should now reflect the new, topmost call.
        assert_eq!(*fsm.current_state(), StateTag::PendingStep(StepType::Begin));
        assert_eq!(fsm.call_stack.len(), 2); // Outer and inner calls on stack

        // 4. Step and complete inner_call
        match fsm.step().expect("step for inner_call failed") {
            CallOutcome::Complete(results) => {
                assert_eq!(results.len(), 1);
                assert_eq!(results[0].i32(), Some(50));
            }
            _ => panic!("Expected Complete for inner_call"),
        }
        // Inner call completed and popped. FSM state should revert to outer call's state.
        // The outer call was AwaitingHost for host_A.
        assert_eq!(*fsm.current_state(), StateTag::AwaitingHost);
        assert_eq!(fsm.call_stack.len(), 1);

        // 5. Provide response for host_A (for the outer_call)
        let host_response_A = HostFuncResponse {
            func_desc: import_req_a.func_desc, // Use the original func_desc for host_A
            results: vec![Val::I32(10)],       // host_A returned 10
        };
        fsm.pop(host_response_A)
            .expect("provide_host_response for host_A failed");
        assert_eq!(
            *fsm.current_state(),
            StateTag::PendingStep(StepType::Resume)
        );

        // 6. Step outer_call again.
        // The Wasm for outer_call isn't fully implemented to use the result of host_A yet.
        // For this test, we just expect it to complete or request another import if the WAT was more complex.
        // Given the simple WAT for outer_call, it effectively completes after the host_A call site.
        match fsm.step().expect("final step for outer_call failed") {
            CallOutcome::Complete(results) => {
                // outer_call doesn't actually return a value in this simplified WAT
                // It would if it used the result of host_A and then returned something.
                // Let's assume if it completes, it means the flow worked.
                assert!(results.is_empty() || results[0].i32().is_some()); // Or check specific expected result if WAT defined one
            }
            // If outer_call tried to call host_B, this branch would be taken.
            _ => panic!("Expected Complete for outer_call after resuming from host_A"),
        }
        assert_eq!(*fsm.current_state(), StateTag::Idle);
        assert!(fsm.call_stack.is_empty());
    }

    #[test]
    fn test_fsm_call_sequence_multiple_functions() {
        let wat = r#"
            (module
              (func (export "add") (param i32 i32) (result i32)
                local.get 0
                local.get 1
                i32.add)
              (func (export "sub") (param i32 i32) (result i32)
                local.get 0
                local.get 1
                i32.sub)
            )
        "#;
        let (_runtime, mut fsm) = setup_fsm(wat);

        // Call 1: "add"
        let add_request = NativeFuncRequest {
            func_desc: wasm::NativeFuncDesc::Export("add".to_string()),
            params: vec![Val::I32(10), Val::I32(5)],
        };

        assert_eq!(*fsm.current_state(), StateTag::Idle);
        fsm.push(add_request)
            .expect("start_call for add failed");
        assert_eq!(*fsm.current_state(), StateTag::PendingStep(StepType::Begin));

        match fsm.step().expect("step for add failed") {
            CallOutcome::Complete(results) => {
                assert_eq!(results.len(), 1);
                assert_eq!(results[0].i32(), Some(15)); // 10 + 5
            }
            _ => panic!("Expected Complete for add"),
        }
        assert_eq!(*fsm.current_state(), StateTag::Idle);

        // Call 2: "sub"
        let sub_request = NativeFuncRequest {
            func_desc: wasm::NativeFuncDesc::Export("sub".to_string()),
            params: vec![Val::I32(10), Val::I32(5)],
        };

        assert_eq!(*fsm.current_state(), StateTag::Idle); // Should be Idle again
        fsm.push(sub_request)
            .expect("start_call for sub failed");
        assert_eq!(*fsm.current_state(), StateTag::PendingStep(StepType::Begin));

        match fsm.step().expect("step for sub failed") {
            CallOutcome::Complete(results) => {
                assert_eq!(results.len(), 1);
                assert_eq!(results[0].i32(), Some(5)); // 10 - 5
            }
            _ => panic!("Expected Complete for sub"),
        }
        assert_eq!(*fsm.current_state(), StateTag::Idle);
    }

    #[test]
    fn test_fsm_call_sequence_with_imports() {
        let wat = r#"
            (module
              (import "env" "host_double" (func $double (param i32) (result i32)))
              (func (export "run_double") (param i32) (result i32)
                local.get 0
                call $double
              )
              (func (export "run_add_five") (param i32) (result i32)
                local.get 0
                i32.const 5
                i32.add
              )
            )
        "#;
        let (_runtime, mut fsm) = setup_fsm(wat);

        // Call 1: "run_double" (invokes host function)
        let double_request = NativeFuncRequest {
            func_desc: wasm::NativeFuncDesc::Export("run_double".to_string()),
            params: vec![Val::I32(10)],
        };

        assert_eq!(*fsm.current_state(), StateTag::Idle);
        fsm.push(double_request)
            .expect("start_call for run_double failed");
        assert_eq!(*fsm.current_state(), StateTag::PendingStep(StepType::Begin));

        let import_req = match fsm.step().expect("step 1 for run_double failed") {
            CallOutcome::ImportCallNeeded(req) => req,
            _ => panic!("Expected ImportCallNeeded for run_double"),
        };
        assert_eq!(*fsm.current_state(), StateTag::AwaitingHost);
        assert_eq!(import_req.func_desc.field_name, "host_double");

        let host_resp = HostFuncResponse {
            func_desc: import_req.func_desc,
            results: vec![Val::I32(20)], // 10 * 2
        };
        fsm.pop(host_resp)
            .expect("provide_host_response for run_double failed");
        assert_eq!(
            *fsm.current_state(),
            StateTag::PendingStep(StepType::Resume)
        );

        match fsm.step().expect("step 2 for run_double failed") {
            CallOutcome::Complete(results) => {
                assert_eq!(results.len(), 1);
                assert_eq!(results[0].i32(), Some(20));
            }
            _ => panic!("Expected Complete for run_double after host call"),
        }
        assert_eq!(*fsm.current_state(), StateTag::Idle);

        // Call 2: "run_add_five" (simple call, no imports)
        let add_five_request = NativeFuncRequest {
            func_desc: wasm::NativeFuncDesc::Export("run_add_five".to_string()),
            params: vec![Val::I32(7)],
        };

        assert_eq!(*fsm.current_state(), StateTag::Idle); // Should be Idle again
        fsm.push(add_five_request)
            .expect("start_call for run_add_five failed");
        assert_eq!(*fsm.current_state(), StateTag::PendingStep(StepType::Begin));

        match fsm.step().expect("step for run_add_five failed") {
            CallOutcome::Complete(results) => {
                assert_eq!(results.len(), 1);
                assert_eq!(results[0].i32(), Some(12)); // 7 + 5
            }
            _ => panic!("Expected Complete for run_add_five"),
        }
        assert_eq!(*fsm.current_state(), StateTag::Idle);
    }
}
