use alloy_primitives::Address;
use r55::test_utils::add_balance_to_db;
use serde::{Deserialize, Serialize};
use std::str::FromStr;

use crate::core::riscv_machine::RevmDb;
use crate::core::state::{get_base_path, get_state, serialize_state};

pub enum GeneratedState {
    Default(RevmDb),
    Serialized(String),
}

impl GeneratedState {
    pub fn get_default(self) -> Option<RevmDb> {
        match self {
            GeneratedState::Default(state) => Some(state),
            _ => None,
        }
    }

    pub fn get_serialized(self) -> Option<String> {
        match self {
            GeneratedState::Serialized(state) => Some(state),
            _ => None,
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct GenesisConfig {
    pub name: String,
    pub chain_id: u32,
    pub gas_limit: u64,
    pub genesis_address: String,
    pub genesis_address_balance: u64,
}

pub fn get_genesis_config(chain_id: &str) -> String {
    let path = format!("{}/{}.json", get_base_path("genesis_configs"), chain_id);
    let state = std::fs::read_to_string(path).unwrap_or_default();
    state
}
pub fn generate_empty_state(serialized: bool) -> GeneratedState {
    let state = RevmDb::default();
    if serialized {
        let serialized_state = serialize_state(state.accounts).unwrap();
        GeneratedState::Serialized(serialized_state)
    } else {
        GeneratedState::Default(state)
    }
}

pub fn init_new_appchain(chain_id: &str) -> bool {
    let does_exist = get_state(chain_id).len() > 0;
    let genesis_config = get_genesis_config(chain_id);
    if does_exist || genesis_config.len() == 0 {
        false // chain initiated or missing genesis file
    } else {
        let genesis: GenesisConfig = serde_json::from_str(&genesis_config).unwrap();
        let mut generated_state = generate_empty_state(false).get_default().unwrap();

        let genesis_address = Address::from_str(&genesis.genesis_address).unwrap();
        let genesis_balance = genesis.genesis_address_balance; // todo: wei eth
        add_balance_to_db(&mut generated_state, genesis_address, genesis_balance);
        let serialized_state = serialize_state(generated_state.accounts).unwrap();
        let _ = std::fs::write(
            format!("{}/{}.json", get_base_path("appchains"), chain_id),
            serialized_state,
        );
        true
    }
}

#[test]
fn test_chain_init() {
    let init = init_new_appchain("1");
    assert!(init)
}