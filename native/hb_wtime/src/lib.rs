// #![feature(type_alias_impl_trait)]

use rustler::{Env, Term};
use tracing::{debug, info, trace};

pub mod convert;
/// Entry point for the Rustler NIF module.
/// This file defines the available NIF functions and organizes them into modules.
pub mod logging;
pub mod nif;
pub mod types;
pub mod wasm;
pub mod wasm_fsm;
pub mod ext;

rustler::init!(
    "hb_wtime", // Module name as used in Erlang.
    load = load
);

fn load(env: Env, _load_info: Term) -> bool {
    logging::init_tracing();
    debug!("loading hb_wtime NIF");

    trace!("loading hb_wtime NIF resources");
    rustler::resource!(nif::NifRes, env).then(|| {
        info!("loaded hb_wtime NIF");
    });

    true
}
