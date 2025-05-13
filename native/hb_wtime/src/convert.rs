use rustler::{Encoder, Env, Error, Term};
use tracing::{debug, trace, warn};
use wasmtime::V128;

use crate::{
    types::{NifWasmVal, WasmVal, WasmValType},
    wasm::HostFuncRequest,
};

/// Converts an Erlang float (f64) argument into a WasmVal::F64.
/// Currently assumes the first argument is the one to convert.
pub fn nif_vals_to_wasm_vals(
    vals: &[NifWasmVal],
    val_types: &[WasmValType],
) -> Result<Vec<WasmVal>, Error> {
    trace!(
        "nif_vals_to_wasm_vals - vals: {:?}, val_types: {:?}",
        vals,
        val_types
    );

    let mut wasm_vals = Vec::new();
    for (val, val_type) in vals.iter().zip(val_types.iter()) {
        trace!(
            "nif_vals_to_wasm_vals - val: {:?}, val_type: {:?}",
            val,
            val_type
        );

        let wasm_val = match (val, val_type) {
            (NifWasmVal::I32(arg), WasmValType::I32) => WasmVal::I32(*arg as i32),
            (NifWasmVal::I32(arg), WasmValType::I64) => WasmVal::I64(*arg as i64),
            (NifWasmVal::I64(arg), WasmValType::I32) => WasmVal::I32(*arg as i32),
            (NifWasmVal::I64(arg), WasmValType::I64) => WasmVal::I64(*arg as i64),
            (NifWasmVal::F32(arg), WasmValType::F32) => WasmVal::F32(arg.to_bits()),
            (NifWasmVal::F32(arg), WasmValType::F64) => WasmVal::F64((*arg as f64).to_bits()),
            (NifWasmVal::F64(arg), WasmValType::F32) => WasmVal::F32((*arg as f32).to_bits()),
            (NifWasmVal::F64(arg), WasmValType::F64) => WasmVal::F64(arg.to_bits()),
            _ => {
                let msg = format!(
                    "Unsupported combination of NifVal/WasmType: ({:?}, {:?})",
                    val, val_type
                );
                debug!("nif_params_to_wasm_vals - {}", msg);

                warn!("Using default value for unconvertable type: {:?}", val_type);
                match val_type {
                    WasmValType::I32 => WasmVal::I32(0),
                    WasmValType::I64 => WasmVal::I64(0),
                    WasmValType::F32 => WasmVal::F32(0),
                    WasmValType::F64 => WasmVal::F64(0),
                    WasmValType::V128 => WasmVal::V128(V128::from(0)),
                    _ => unimplemented!("Unsupported WasmValType: {:?}", val_type),
                }
            }
        };

        trace!("nif_vals_to_wasm_vals - wasm_val: {:?}", wasm_val);
        wasm_vals.push(wasm_val);
    }
    Ok(wasm_vals)
}

/// Converts a WasmVal result into an encodable rustler::Term.
pub fn wasm_val_to_term<'a>(env: Env<'a>, val: &WasmVal) -> Result<Term<'a>, Error> {
    match val {
        WasmVal::I32(i) => Ok(i.encode(env)),
        WasmVal::I64(i) => Ok(i.encode(env)),
        WasmVal::F32(f) => Ok(f32::from_bits(*f).encode(env)),
        WasmVal::F64(f) => Ok(f64::from_bits(*f).encode(env)),
        // TODO: Handle other Val types like V128, FuncRef, ExternRef if needed
        _ => Err(Error::Term(Box::new("Unsupported WasmVal result type"))),
    }
}

/// Converts a slice of WasmVal results into an Erlang list term.
pub fn wasm_vals_to_term_list<'a>(env: Env<'a>, results: &[WasmVal]) -> Result<Term<'a>, Error> {
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
