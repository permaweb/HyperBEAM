use wasmtime::{Caller, FuncType, Val};
use anyhow::Result;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HostModule {
    Emscripten,
}

// StoreData is typically `()` if not specified otherwise, adjust if your Store uses a different type.
pub type ImportFunc = Box<dyn Fn(Caller<'_, ()>, &[Val], &mut [Val]) -> Result<()> + Send + Sync + 'static>;

pub struct ImportDef {
    pub module_name: String,
    pub field_name: String,
    pub ty: FuncType,
    pub func: ImportFunc,
}
