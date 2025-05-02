mod core;
mod utils;
mod tests;

use revm::primitives::db;

use crate::core::riscv_machine::evaluate_raw_tx;
use crate::core::state::{serialize_state, deserialize_state};
fn main() {
    println!("Hello, world!");
}
