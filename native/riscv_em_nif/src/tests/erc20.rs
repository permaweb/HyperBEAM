#[cfg(test)]
mod tests {
    use crate::core::riscv_machine::evaluate_raw_tx;
    use crate::core::state::{serialize_state, deserialize_state, get_state};
    use revm::primitives::Address;
    use r55::test_utils::{add_balance_to_db, ALICE};
    use serde_json::Value;


    #[test]
    fn test_ser_der_state() {
        let mut state = revm::db::CacheDB::default();
        // fund alice
        add_balance_to_db(&mut state, ALICE, 1e18 as u64);
        // serialize db
        let serialized_state = serialize_state(state.accounts).unwrap();
        // deserialize db
        let mut deserialized_state = deserialize_state(&serialized_state).unwrap();
        let serialized_state: Value = serde_json::from_str(&serialized_state)
        .expect("Failed to parse JSON");

        // retrieve back balances
        let alice_balance_ser = serialized_state
        .pointer(&format!("/accounts/{}/balance", "0x000000000000000000000000000000000000000a")).unwrap().as_str().unwrap();
        let alice_balance_de = deserialized_state.load_account(ALICE).unwrap().info.balance.to_string();
        assert_eq!(alice_balance_de, alice_balance_ser);
    }

    #[test]
    fn test_erc20_deploy() {
        let mut state = revm::db::CacheDB::default();
        let deployer = Address::from_slice(&hex::decode("b76FaBf56a6A9872efeA4EF848605D32eAfF13cE").unwrap());
        add_balance_to_db(&mut state, deployer, 1e18 as u64);
        let raw_tx_hex = std::fs::read_to_string("src/tests/erc20-bytecode-signed.txt").unwrap();
        let tx  = evaluate_raw_tx(state, &raw_tx_hex).1;
        assert_eq!(tx.deployed_contract.unwrap(), "0x602a0eF8ccD015ba98eF4E450F6866C05C4154A1");
    }

    #[test]
    fn test_erc20_mint() {
        let mut state = revm::db::CacheDB::default();
        let deployer = Address::from_slice(&hex::decode("b76FaBf56a6A9872efeA4EF848605D32eAfF13cE").unwrap());
        add_balance_to_db(&mut state, deployer, 1e18 as u64);
        let raw_tx_hex_deploy = std::fs::read_to_string("src/tests/erc20-bytecode-signed.txt").unwrap();
        let raw_tx_hex_mint = std::fs::read_to_string("src/tests/erc20-mint-signed.txt").unwrap();
        let _res_deploy  = evaluate_raw_tx(state.clone(), &raw_tx_hex_deploy);
        let res_mint = evaluate_raw_tx(state, &raw_tx_hex_mint.trim());
        println!("{:?}", res_mint.0);
        assert!(res_mint.1.status);
    }

    #[test]
    fn get_appchain_state() {
        let state = get_state("7777");
        println!("{:?}", state);
    }
}