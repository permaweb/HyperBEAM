use std::fmt;

pub use wasmtime::Val as WasmVal;
pub use wasmtime::ValType as WasmValType;

use rustler::NifUntaggedEnum;

#[derive(Debug, Clone, NifUntaggedEnum)]
pub enum NifWasmVal {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

#[derive(Debug, Clone, NifUntaggedEnum)]
pub enum NativeFuncDesc {
    Export(String),
    Indirect(u64),
}

impl fmt::Display for NativeFuncDesc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NativeFuncDesc::Export(name) => 
                match name.parse::<u64>() {
                    Ok(_) => write!(f, "\"{}\"", name),
                    Err(_) => write!(f, "{}", name),
                }
            NativeFuncDesc::Indirect(index) => write!(f, "{}", index),
        }
    }
}
