use rustler::{Encoder, Env, Error, Term};
use tracing::{error, trace};
use wasmtime::{Val, ValType};

use crate::{types::NifWasmParams, wasm::HostFuncRequest};

/// Converts an Erlang float (f64) argument into a WasmVal::F64.
/// Currently assumes the first argument is the one to convert.
pub fn nif_params_to_wasm_vals(
    params: &[NifWasmParams],
    param_types: &[ValType],
) -> Result<Vec<Val>, Error> {
    trace!("nif_params_to_wasm_vals - params: {:?}, param_types: {:?}", params, param_types);

    let mut wasm_vals = Vec::new();
    for (arg, arg_type) in params.iter().zip(param_types.iter()) {
        trace!("nif_params_to_wasm_vals - arg: {:?}, arg_type: {:?}", arg, arg_type);

        match (arg, arg_type) {
            (NifWasmParams::I32(arg), ValType::I32) => wasm_vals.push(Val::I32(*arg as i32)),
            (NifWasmParams::I64(arg), ValType::I64) => wasm_vals.push(Val::I64(*arg as i64)),
            (NifWasmParams::F32(arg), ValType::F32) => wasm_vals.push(Val::F32(arg.to_bits())),
            (NifWasmParams::F32(arg), ValType::F64) => wasm_vals.push(Val::F64((*arg as f64).to_bits())),
            (NifWasmParams::F64(arg), ValType::F32) => wasm_vals.push(Val::F32((*arg as f32).to_bits())),
            (NifWasmParams::F64(arg), ValType::F64) => wasm_vals.push(Val::F64(arg.to_bits())),
            _ => {
                error!("nif_params_to_wasm_vals - Unsupported WasmValType");
                return Err(Error::Term(Box::new("Unsupported WasmValType")))
            },
        }
    }
    Ok(wasm_vals)
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
