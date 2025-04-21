#[cfg(test)]
mod tests {
    use std::fs;

    use crate::core::interpreter::eval;
    use serde_json::Value;

    #[test]
    fn test_eval_raw_transaction_with_state() {
        let raw_tx_hex = "02f87782251881878447868c008447868c008307a12094c69b7ea1931e207bebe89fa32b10435aec234c40893635c9adc5dea0000080c080a07a207f6815ba6a9d92b49f205695514c93fbdb2abe37a1610389958a92f13aa4a0556295a23580307b7f0248784dcb9ec9c9c7aba9234703f902e0b37f227ab9f2";
        let previous_state = fs::read_to_string("./state.json").unwrap();
        let (result, state) = eval(raw_tx_hex.to_string(), Some(previous_state)).unwrap();

        let result_json: Value = serde_json::to_value(&result).unwrap();
        println!("TX response:{:?}", result_json);
        println!("new eval state result: {}", state);
    }
}
