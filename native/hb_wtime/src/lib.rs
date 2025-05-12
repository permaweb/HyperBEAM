use rustler::{Env, Term};
use tracing::trace;

/// Entry point for the Rustler NIF module.
/// This file defines the available NIF functions and organizes them into modules.

pub mod example;
pub mod types;
pub mod wasm;
pub mod convert;
pub mod nif;

rustler::init!(
    "hb_wtime", // Module name as used in Erlang.
    load = load
);

fn load(env: Env, _load_info: Term) -> bool {
    trace!("load");
    rustler::resource!(nif::NifRes, env);
    true
}
