pub mod core;
pub mod utils;

#[cfg(test)]
pub mod tests;

use crate::core::riscv_machine::evaluate_raw_tx;
use core::state::{deserialize_state, get_base_path, get_state, serialize_state, shallow_merge};
use rustler::NifResult;

mod atoms {
    rustler::atoms! {
        ok,
    }
}

#[rustler::nif]
fn hello() -> NifResult<String> {
    Ok("hello from riscv-em nif".to_string())
}

#[rustler::nif]
fn eval_riscv_bytecode(signed_raw_tx: String, chain_id: String) -> NifResult<String> {
    let state = get_state(&chain_id);

    if state.len() == 0 {
        return Ok(serde_json::json!({"error": "invalid chain_id"}).to_string());
    };

    let deserialized_state = deserialize_state(&state)
        .map_err(|_| rustler::Error::Atom("Error deserializing json state"))?;
    let evaluation_res = evaluate_raw_tx(deserialized_state, &signed_raw_tx);
    let serialized_evaluated_state = serialize_state(evaluation_res.0.accounts)
        .map_err(|_| rustler::Error::Atom("Error evaluating calldata"))?;
    let merged_states = shallow_merge(&state, &serialized_evaluated_state)
        .map_err(|_| rustler::Error::Atom("Error shallow merging states"))?;

    if merged_states.1 {
        let path = format!("{}/{}.json", get_base_path("appchains"), chain_id);
        let _state_update = std::fs::write(path, merged_states.0)
            .map_err(|_| rustler::Error::Atom("Error saving new state"))?;
    }

    Ok(serialized_evaluated_state)
}

#[rustler::nif]
fn get_appchain_state(chain_id: &str) -> NifResult<String> {
    let state = get_state(chain_id);
    Ok(state)
}

rustler::init!(
    "riscv_em_nif",
    [hello, eval_riscv_bytecode, get_appchain_state]
);