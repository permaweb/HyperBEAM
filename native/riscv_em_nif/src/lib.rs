pub mod core;
pub mod utils;

#[cfg(test)]
pub mod tests;

use core::state::{deserialize_state, serialize_state, get_state};

use crate::core::riscv_machine::evaluate_raw_tx;
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
fn eval_riscv_bytecode(signed_raw_tx: String, state: String) -> NifResult<String> {
    let deserialized_state = deserialize_state(&state).map_err(|_| rustler::Error::Atom("Error deserializing json state"))?;
    let evaluation_res = evaluate_raw_tx(deserialized_state, &signed_raw_tx);
    let serialized_evaluated_state = serialize_state(evaluation_res.0.accounts).map_err(|_| rustler::Error::Atom("Error evaluating calldata"))?;
    Ok(serialized_evaluated_state)
}

#[rustler::nif]
fn get_appchain_state(chain_id: &str) -> NifResult<String> {
	let state = get_state(chain_id);
    Ok(state)
}

rustler::init!("riscv_em_nif", [hello, eval_riscv_bytecode, get_appchain_state]);