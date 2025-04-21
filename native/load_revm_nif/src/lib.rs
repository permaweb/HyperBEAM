pub mod tx_utils;
pub mod utils;
pub mod core;
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
fn interprete(signed_raw_tx: String, state: Option<String>) -> NifResult<String> {
    let evaluated_state: (String, String) = eval(signed_raw_tx, state)?;
    Ok(evaluated_state.0)
}

rustler::init!("load_revm_nif", [hello, interprete]);
