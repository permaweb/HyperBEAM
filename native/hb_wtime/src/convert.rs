use rustler::{Atom, Binary, Encoder, Env, Error, OwnedBinary, Term};
use tracing::{debug, trace, warn};
use wasmtime::{
    ExportType, ExternType, ImportType, Module, RefType as WasmtimeRefType,
    ValType as WasmtimeValType, V128,
};
use anyhow::{anyhow, Result};

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

mod extern_type_atoms {
    use rustler::atoms;

    atoms! {
        func,
        global,
        table,
        memory,
        tag,
    }
}

pub fn wasm_extern_type_to_atom(extern_type: &ExternType) -> Atom {
    match extern_type {
        ExternType::Func(_) => extern_type_atoms::func(),
        ExternType::Global(_) => extern_type_atoms::global(),
        ExternType::Table(_) => extern_type_atoms::table(),
        ExternType::Memory(_) => extern_type_atoms::memory(),
        ExternType::Tag(_) => extern_type_atoms::tag(),
    }
}

mod valtype_atoms {
    use rustler::atoms;

    atoms! {
        i32,
        i64,
        f32,
        f64,
        v128,
    }
}

pub fn wasm_ref_type_to_atom<'a>(env: Env<'a>, ref_type: &WasmtimeRefType) -> Result<Atom, Error> {
    Atom::from_str(env, format!("{:?}", ref_type).as_str())
}

fn wasmtime_valtype_to_atom<'a>(env: Env<'a>, val_type: &WasmtimeValType) -> Atom {
    match val_type {
        WasmtimeValType::I32 => valtype_atoms::i32(),
        WasmtimeValType::I64 => valtype_atoms::i64(),
        WasmtimeValType::F32 => valtype_atoms::f32(),
        WasmtimeValType::F64 => valtype_atoms::f64(),
        WasmtimeValType::V128 => valtype_atoms::v128(),
        WasmtimeValType::Ref(ref_type) => wasm_ref_type_to_atom(env, ref_type).unwrap(),
    }
}

pub fn str_to_erl_bin<'a>(env: Env<'a>, s: &str) -> Result<Binary<'a>, Error> {
    match OwnedBinary::new(s.len()) {
        Some(mut owned_bin) => {
            owned_bin.as_mut_slice().copy_from_slice(s.as_bytes());
            Ok(Binary::from_owned(owned_bin, env))
        }
        None => Err(Error::Term(Box::new(format!("Failed to allocate OwnedBinary in str_to_erl_bin for string: {}", s)))),
    }
}

pub fn wasm_import_to_raw<'a>(
    env: Env<'a>,
    import: &ImportType,
) -> Result<(Atom, Binary<'a>, Binary<'a>, Option<Vec<Atom>>, Option<Vec<Atom>>), Error> {
    let extern_type_details = import.ty();
    let type_atom = wasm_extern_type_to_atom(&extern_type_details);
    let module_name_bin = str_to_erl_bin(env, import.module())?;
    let field_name_bin = str_to_erl_bin(env, import.name())?;

    let params_atoms_opt: Option<Vec<Atom>>;
    let results_atoms_opt: Option<Vec<Atom>>;

    if let Some(func_type) = extern_type_details.func() {
        params_atoms_opt = Some(
            func_type
                .params()
                .map(|p_valtype| {
                    wasmtime_valtype_to_atom(env, &p_valtype)
                })
                .collect(),
        );
        results_atoms_opt = Some(
            func_type
                .results()
                .map(|r_valtype| {
                    wasmtime_valtype_to_atom(env, &r_valtype)
                })
                .collect(),
        );
    } else {
        params_atoms_opt = None;
        results_atoms_opt = None;
    }

    Ok((
        type_atom,
        module_name_bin,
        field_name_bin,
        params_atoms_opt,
        results_atoms_opt,
    ))
}

pub fn wasm_export_to_raw<'a>(
    env: Env<'a>,
    export: &ExportType,
) -> Result<(Atom, Binary<'a>, Option<Vec<Atom>>, Option<Vec<Atom>>), Error> {
    let extern_type_details = export.ty();
    let type_atom = wasm_extern_type_to_atom(&extern_type_details);
    let name = str_to_erl_bin(env, export.name())?;

    let params_atoms_opt: Option<Vec<Atom>>;
    let results_atoms_opt: Option<Vec<Atom>>;

    if let Some(func_type) = extern_type_details.func() {
        params_atoms_opt = Some(
            func_type
                .params()
                .map(|p_valtype| {
                    wasmtime_valtype_to_atom(env, &p_valtype)
                })
                .collect(),
        );
        results_atoms_opt = Some(
            func_type
                .results()
                .map(|r_valtype| {
                    wasmtime_valtype_to_atom(env, &r_valtype)
                })
                .collect(),
        );
    } else {
        params_atoms_opt = None;
        results_atoms_opt = None;
    }

    Ok((type_atom, name, params_atoms_opt, results_atoms_opt))
}

mod meta_section_atoms {
    use rustler::atoms;

    atoms! {
        imports,
        exports,
    }
}
pub fn wasm_module_to_meta_term_list<'a>(env: Env<'a>, module: &Module) -> Result<Term<'a>, Error> {
    let raw_imports_data_res: Result<Vec<_>, Error> = module
        .imports()
        .map(|import| wasm_import_to_raw(env, &import))
        .collect();
    let raw_imports_data = raw_imports_data_res?;

    let raw_exports_data_res: Result<Vec<_>, Error> = module
        .exports()
        .map(|export| wasm_export_to_raw(env, &export))
        .collect();
    let raw_exports_data = raw_exports_data_res?;

    let imports_terms: Result<Vec<Term<'a>>, Error> = raw_imports_data
        .into_iter()
        .map(|(type_atom, module_name, field_name, params_opt, results_opt)| {
            if type_atom == extern_type_atoms::func() {
                Ok((type_atom, module_name, field_name, params_opt, results_opt).encode(env))
            } else {
                Ok((type_atom, module_name, field_name).encode(env))
            }
        })
        .collect();

    let exports_terms: Result<Vec<Term<'a>>, Error> = raw_exports_data
        .into_iter()
        .map(|(type_atom, name, params_opt, results_opt)| {
            if type_atom == extern_type_atoms::func() {
                Ok((type_atom, name, params_opt, results_opt).encode(env))
            } else {
                Ok((type_atom, name).encode(env))
            }
        })
        .collect();

    Ok((
        (meta_section_atoms::imports(), imports_terms?),
        (meta_section_atoms::exports(), exports_terms?),
    )
        .encode(env))
}
