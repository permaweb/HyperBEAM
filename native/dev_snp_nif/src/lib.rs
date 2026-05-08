/// Entry point for the Rustler NIF module.
/// This file defines the available NIF functions and organizes them into modules.

mod logging;
mod snp_support;
mod attestation;
mod digest;
mod verification;
mod helpers;

rustler::init!(
    "hb_snp_nif" // Module name as used in Erlang.
);
