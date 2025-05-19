use tracing::trace;
use wasmtime::{Caller, Engine, FuncType};
use super::types::ImportDef;
use crate::types::{WasmVal, WasmValType};

pub fn get_memcpy_js(engine: Engine, is_64: bool) -> ImportDef {
    let arg_types = if is_64 {
        vec![WasmValType::I64, WasmValType::I64, WasmValType::I64]
    } else {
        vec![WasmValType::I32, WasmValType::I32, WasmValType::I32]
    };
    let result_types = vec![WasmValType::I32];
    ImportDef {
        module_name: "env".to_string(),
        field_name: "_emscripten_memcpy_js".to_string(),
        ty: FuncType::new(&engine, arg_types, result_types),
        func: Box::new(move |mut caller: Caller<'_, ()>, params: &[WasmVal], results: &mut [WasmVal]| {
            trace!("calling native env._emscripten_memcpy_js");

            let (dst, src, len) = match (params[0], params[1], params[2]) {
                (WasmVal::I64(dst), WasmVal::I64(src), WasmVal::I64(len)) => {
                    (dst as usize, src as usize, len as usize)
                }
                (WasmVal::I32(dst), WasmVal::I32(src), WasmVal::I32(len)) => {
                    (dst as usize, src as usize, len as usize)
                }
                _ => {
                    return Err(anyhow::anyhow!(
                        "Invalid parameters for env._emscripten_memcpy_js: {:?}",
                        params
                    ));
                }
            };
            let memory = caller.get_export("memory").unwrap().into_memory().unwrap();
            let data = memory.data_mut(&mut caller);

            let mem_len = data.len();

            if len == 0 {
                // Emscripten's memcpy can be called with 0 length.
                return Ok(());
            }

            // Check source bounds: src_offset + copy_len must not exceed mem_len
            if src.checked_add(len).map_or(true, |end| end > mem_len) {
                return Err(anyhow::anyhow!(
                    "_emscripten_memcpy_js: source access out of bounds. \
                    src_offset: {}, copy_len: {}, memory_size: {}",
                    src, len, mem_len
                ));
            }

            // Check destination bounds: dst_offset + copy_len must not exceed mem_len
            if dst.checked_add(len).map_or(true, |end| end > mem_len) {
                return Err(anyhow::anyhow!(
                    "_emscripten_memcpy_js: destination access out of bounds. \
                    dst: {}, len: {}, memory_size: {}",
                    dst, len, mem_len
                ));
            }

            // Perform the copy. `copy_within` handles overlapping regions correctly.
            data.copy_within(src..src + len, dst);

            Ok(())
        }),
    }
}

pub fn get_imports(engine: Engine, is_64: bool) -> Vec<ImportDef> {
    vec![get_memcpy_js(engine, is_64)]
}
