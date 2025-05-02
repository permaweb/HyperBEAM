use crate::utils::constants::{GAS_PRICE, TX_GAS_LIMIT};
use r55::eval_utils::{EvalTxResult, LoadEvmConfig};
use r55::exec::eval_tx;

pub type RevmDb = revm::db::CacheDB<revm::db::EmptyDBTyped<std::convert::Infallible>>;

pub fn evaluate_raw_tx(mut state: RevmDb, raw_tx_hex: &str) -> (RevmDb, EvalTxResult) {
    let evm_config = LoadEvmConfig::custom(Some(TX_GAS_LIMIT), Some(GAS_PRICE));
    let tx_result: EvalTxResult = eval_tx(&mut state, raw_tx_hex, Some(evm_config)).unwrap();
    (state, tx_result)
}