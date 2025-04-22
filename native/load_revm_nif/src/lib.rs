pub mod core;
pub mod utils;
#[cfg(test)]
mod tests;

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
fn eval_bytecode(signed_raw_tx: String, state: String) -> NifResult<String> {
    let state_option = if state.is_empty() { None } else { Some(state) };
    let evaluated_state: (String, String) = eval(signed_raw_tx, state_option)?;
    Ok(evaluated_state.0)
}

rustler::init!("load_revm_nif", [hello, eval_bytecode]);
