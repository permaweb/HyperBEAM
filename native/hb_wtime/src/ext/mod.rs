mod types;
mod emscripten;

pub use types::{HostModule, ImportDef};
use wasmtime::Engine;

pub fn get_imports(engine: Engine, is_64: bool, enabled_modules: Vec<HostModule>) -> Vec<ImportDef> {
    let mut imports = Vec::new();
    if enabled_modules.contains(&HostModule::Emscripten) {
        imports.extend(emscripten::get_imports(engine, is_64));
    }
    imports
}
