use rustler::{Encoder, Env, Error, Term};
use wasmtime::Val;

use crate::wasm::HostFuncRequest;

/// Converts an Erlang float (f64) argument into a WasmVal::F64.
/// Currently assumes the first argument is the one to convert.
pub fn args_to_wasm_vals(args: &[f64]) -> Result<Vec<Val>, Error> {
    if args.is_empty() {
        Ok(vec![])
    } else {
        // TODO: Add proper signature checking and conversion for multiple args.
        Ok(vec![Val::F64(args[0].to_bits())])
    }
}

/// Converts a WasmVal result into an encodable rustler::Term.
pub fn wasm_val_to_term<'a>(env: Env<'a>, val: &Val) -> Result<Term<'a>, Error> {
    match val {
        Val::I32(i) => Ok(i.encode(env)),
        Val::I64(i) => Ok(i.encode(env)),
        Val::F32(f) => Ok(f32::from_bits(*f).encode(env)),
        Val::F64(f) => Ok(f64::from_bits(*f).encode(env)),
        // TODO: Handle other Val types like V128, FuncRef, ExternRef if needed
        _ => Err(Error::Term(Box::new("Unsupported WasmVal result type"))),
    }
}

/// Converts a slice of WasmVal results into an Erlang list term.
pub fn wasm_vals_to_term_list<'a>(env: Env<'a>, results: &[Val]) -> Result<Term<'a>, Error> {
    let terms: Result<Vec<Term>, Error> = results
        .iter()
        .map(|val| wasm_val_to_term(env, val))
        .collect();
    // Encode the resulting Vec<Term> into a single Term (list).
    Ok(terms?.encode(env))
}

pub fn wasm_host_func_req_to_term_list<'a>(
    env: Env<'a>,
    host_func_req: HostFuncRequest,
) -> Result<Term<'a>, Error> {
    // Create a vector of terms directly.
    let terms: Vec<Term> = vec![
        host_func_req.func_desc.module_name.encode(env),
        host_func_req.func_desc.field_name.encode(env),
        wasm_vals_to_term_list(env, &host_func_req.params)?,
    ];
    // Encode the vector into an Erlang list term.
    Ok(terms.encode(env))
}
