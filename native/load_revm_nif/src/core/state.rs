use crate::utils::constants::EIP1967_IMPLEMENTATION_SLOT;
use crate::utils::misc::{parse_address, parse_u256};
use revm::state::AccountInfo;
use revm::{
    database::{CacheDB, EmptyDB},
    primitives::{Address, U256},
};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

#[derive(Serialize, Deserialize)]
pub struct AccountState {
    pub nonce: u64,
    pub balance: String,
    pub code: Option<String>,
}

#[derive(Serialize, Deserialize)]
pub struct EvmState {
    pub accounts: HashMap<String, AccountState>,
    pub storage: HashMap<String, HashMap<String, String>>,
}

/// serialize EVM state
pub fn serialize_state(db: HashMap<Address, revm::state::Account>) -> Result<String, String> {
    let mut accounts = HashMap::new();
    let mut storage = HashMap::new();

    // extract accounts from the database
    for (address, account_info) in db.clone() {
        let address_hex = format!("0x{:x}", address);

        // Check if this is a contract
        let is_contract = account_info.info.code.is_some();

        // create account state
        let account_state = AccountState {
            nonce: account_info.info.nonce,
            balance: account_info.info.balance.to_string(),
            code: if let Some(bytecode) = account_info.info.code {
                if !bytecode.is_empty() {
                    // Check if the bytecode is non-empty
                    Some(format!("0x{}", hex::encode(bytecode.bytecode().clone())))
                } else {
                    None
                }
            } else {
                None // EOAs
            },
        };

        accounts.insert(address_hex.clone(), account_state);

        let mut account_storage = HashMap::new();
        for (slot, value) in account_info.storage.iter() {
            let slot_hex = format!("0x{:x}", slot);
            let value_hex = format!("0x{:x}", value.present_value);
            account_storage.insert(slot_hex, value_hex);
        }

        // For contracts, always check the implementation slot even if other storage is empty
        if is_contract {
            // If this slot exists in storage, it's a proxy contract
            if let Some(impl_value) = account_info
                .storage
                .get(&parse_u256(EIP1967_IMPLEMENTATION_SLOT).unwrap())
            {
                let impl_hex = format!("0x{:x}", impl_value.present_value);
                account_storage.insert(EIP1967_IMPLEMENTATION_SLOT.to_string(), impl_hex);
            }
        }

        // Only insert storage if it's not empty or if it's a contract
        if !account_storage.is_empty() || is_contract {
            storage.insert(address_hex, account_storage);
        }
    }

    // final state object
    let state = EvmState { accounts, storage };

    // JSON serialized state
    serde_json::to_string(&state).map_err(|e| format!("Failed to serialize state: {}", e))
}

/// deserialize EVM state
pub fn deserialize_state(state_json: &str) -> Result<CacheDB<EmptyDB>, String> {
    let state: EvmState = serde_json::from_str(state_json)
        .map_err(|e| format!("Failed to parse state JSON: {}", e))?;

    // a new database
    let mut db = CacheDB::new(EmptyDB::default());

    // Populate the database with accounts and storage
    for (address_hex, account_state) in state.accounts {
        let address = match parse_address(&address_hex) {
            Ok(addr) => addr,
            Err(e) => return Err(format!("invalid address {}: {}", address_hex, e)),
        };

        let balance = match account_state.balance.parse::<U256>() {
            Ok(b) => b,
            Err(_) => return Err(format!("invalid balance: {}", account_state.balance)),
        };

        let mut account_info = AccountInfo {
            nonce: account_state.nonce,
            balance,
            code: None,
            code_hash: revm::primitives::KECCAK_EMPTY,
        };

        // Set code if provided (for smart contracts, null for EOAs)
        if let Some(code_hex) = account_state.code {
            let code = match hex::decode(code_hex.trim_start_matches("0x")) {
                Ok(c) => c,
                Err(_) => return Err(format!("Invalid code: {}", code_hex)),
            };

            let bytecode = revm::state::Bytecode::new_raw(code.into());
            account_info.code_hash = ethers_core::utils::keccak256(bytecode.bytes_slice()).into();
            account_info.code = Some(bytecode);
        }

        db.insert_account_info(address, account_info);

        // set storage if available
        if let Some(account_storage) = state.storage.get(&address_hex) {
            for (slot_hex, value_hex) in account_storage {
                let slot = match parse_u256(slot_hex) {
                    Ok(s) => s,
                    Err(e) => return Err(format!("Invalid storage slot {}: {}", slot_hex, e)),
                };

                let value = match parse_u256(value_hex) {
                    Ok(v) => v,
                    Err(e) => return Err(format!("Invalid storage value {}: {}", value_hex, e)),
                };

                let _ = db.insert_account_storage(address, slot, value);
            }
        }
    }

    Ok(db)
}

#[cfg(test)]
fn get_appchain_base_path() -> String {
    "./appchains".to_string()
}

#[cfg(not(test))]
fn get_appchain_base_path() -> String {
    "native/load_revm_nif/appchains".to_string()
}

pub fn get_state(chain_id: &str) -> String {
    let path = format!("{}/{}.json", get_appchain_base_path(), chain_id);
    let state = fs::read_to_string(path).unwrap();
    state
}
