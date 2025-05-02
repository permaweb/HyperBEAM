#[cfg(test)]
mod tests {
    use crate::core::riscv_machine::evaluate_raw_tx;
    use crate::core::state::{serialize_state, deserialize_state};

    #[test]
    fn test_erc20_deploy() {
        let state = revm::db::CacheDB::default();
        let raw_tx_hex = std::fs::read_to_string("src/tests/erc20-bytecode-signed.txt").unwrap();
        let res  = evaluate_raw_tx(state, &raw_tx_hex);
        let tx = res.1;
        println!("{:?}", tx);
        let serialized_state = serialize_state(res.0.accounts).unwrap();
        println!("{:?}", serialized_state);
        assert_eq!(tx.deployed_contract.unwrap(), "0x602a0eF8ccD015ba98eF4E450F6866C05C4154A1");
    }
}