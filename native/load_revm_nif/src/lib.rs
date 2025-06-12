pub mod core;
#[cfg(test)]
pub mod tests;
pub mod utils;

use core::state::get_state;

use crate::core::interpreter::eval;
use rustler::NifResult;

mod atoms {
    rustler::atoms! {
        ok,
    }
}

#[rustler::nif]
fn hello() -> NifResult<String> {
    Ok("Hello world!".to_string())
}

#[rustler::nif]
fn eval_bytecode(
    signed_raw_tx: String,
    state: String,
    cout_state_path: String,
) -> NifResult<String> {
    let state_option = if state.is_empty() { None } else { Some(state) };
    let evaluated_state: (String, String) = eval(signed_raw_tx, state_option, cout_state_path)?;
    Ok(evaluated_state.0)
}

#[rustler::nif]
fn get_appchain_state(chain_id: &str) -> NifResult<String> {
    let state = get_state(chain_id);
    Ok(state)
}

rustler::init!("load_revm_nif", [hello, eval_bytecode, get_appchain_state]);
