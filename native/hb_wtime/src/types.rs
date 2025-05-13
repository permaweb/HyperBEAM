use rustler::NifUntaggedEnum;

pub use wasmtime::Val as WasmVal;
pub use wasmtime::ValType as WasmValType;

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
