use crate::core::evm::CustomEvm;
use crate::core::state::{deserialize_state, serialize_state};
use crate::core::transaction::{get_tx_kind, get_tx_object, recover_signer};
use crate::utils::constants::{CONTRACT_SIZE_LIMIT, GENESIS_ADDRESS, TX_GAS_LIMIT};
use crate::utils::misc::json_error;
use revm::state::AccountInfo;
use revm::{
    context::TxEnv,
    database::{CacheDB, EmptyDB},
    primitives::{hardfork::SpecId, Address, Bytes, U256},
    Context, ExecuteEvm, MainContext,
};
use rustler::NifResult;
use std::str::FromStr;
use std::{collections::HashMap, fs};

// main interpreter fn -- bytecode eval
pub fn eval(raw_tx_hex: String, previous_state: Option<String>) -> NifResult<(String, String)> {
    let tx = get_tx_object(&raw_tx_hex);

    let input = tx.input.to_vec();
    let chain_id = tx.chain_id.unwrap().to_string().parse::<u64>().unwrap_or(1);
    let value = tx.value;
    let sender = recover_signer(&raw_tx_hex);
    println!("TX SENDER DEBUG: {:?} -- CHAIN ID: {} ", sender, chain_id);

    // Create a transaction environment
    let mut tx_env = TxEnv::default();
    tx_env.gas_limit = TX_GAS_LIMIT; // 1 gigagas
    tx_env.chain_id = Some(chain_id);
    tx_env.data = Bytes::from(input);
    tx_env.value = revm::primitives::U256::from(value.as_u128());
    tx_env.gas_price = 7; // min gas to pass a tx, 7 wei
    tx_env.caller = sender;
    tx_env.nonce = tx.nonce.to_string().parse::<u64>().unwrap_or_default();
    // for manually modified state, debugging purposes
    // tx_env.nonce = revm::primitives::U256::from(0).to_string().parse::<u64>().unwrap();
    tx_env.kind = get_tx_kind(tx);

    println!("TX_ENV: {:?}", tx_env);

    // deserialize previous state or init new state
    let db = if let Some(state_json) = previous_state.clone() {
        match deserialize_state(&state_json) {
            Ok(db) => db,
            Err(e) => {
                return Ok((
                    json_error(&format!("Failed to deserialize state: {}", e)),
                    "{}".to_string(),
                ));
            }
        }
    } else {
        // init new DB
        // dirty genesis setup for now
        CacheDB::new(EmptyDB::default());
        let mut db = CacheDB::new(EmptyDB::default());
        let genesis_address = Address::from_str(GENESIS_ADDRESS).unwrap(); // genesis master address
        let genesis_balance = U256::from(1_000_000) * U256::from(10).pow(U256::from(18)); // 1000000 native gas token

        let account_info = AccountInfo {
            nonce: 0,
            balance: genesis_balance,
            code: None,
            code_hash: revm::primitives::KECCAK_EMPTY,
        };

        db.insert_account_info(genesis_address, account_info);
        db
    };

    // new EVM instance with the provided state
    let ctx: Context<
        revm::context::BlockEnv,
        TxEnv,
        revm::context::CfgEnv,
        CacheDB<revm::database::EmptyDBTyped<std::convert::Infallible>>,
    > = Context::mainnet().with_db(db).modify_cfg_chained(|cfg| {
        cfg.spec = SpecId::CANCUN;
        cfg.chain_id = 9496;
        cfg.disable_block_gas_limit = true; // so we set the gas limit to whatever we set in TX_GAS_LIMIT
        cfg.limit_contract_code_size = Some(CONTRACT_SIZE_LIMIT); // 10 MB
    });

    let mut evm = CustomEvm::new(ctx, ());

    // transaction execution
    match evm.0.transact(tx_env) {
        Ok(result_and_state) => {
            let mut response = serde_json::Map::new();

            response.insert("success".to_string(), serde_json::Value::Bool(true));
            response.insert(
                "sender".to_string(),
                serde_json::Value::String(format!("0x{:x}", sender)),
            );
            let gas_used = result_and_state.result.gas_used();
            response.insert(
                "gas_used".to_string(),
                serde_json::Value::Number(serde_json::Number::from(gas_used)),
            );

            if let Some(output) = result_and_state.result.output() {
                let output_hex = format!("0x{}", hex::encode(output.to_vec()));
                response.insert("output".to_string(), serde_json::Value::String(output_hex));
            }

            if let Some(addr) = result_and_state.result.created_address() {
                let addr_hex = format!("0x{:x}", addr);
                response.insert(
                    "contract_address".to_string(),
                    serde_json::Value::String(addr_hex),
                );
            }

            let result_json = match serde_json::to_string(&response) {
                Ok(json) => json,
                Err(_) => json_error("Error serializing result"),
            };

            let state: HashMap<Address, revm::state::Account> = result_and_state.state;

            let new_state_json = serialize_state(state.into()).unwrap();

            // read existing state
            if let Ok(existing_state) = fs::read_to_string("./state_8009.json") {
                let mut existing: serde_json::Value =
                    serde_json::from_str(&existing_state).unwrap();
                let new: serde_json::Value = serde_json::from_str(&new_state_json).unwrap();

                // merge states
                if let (Some(existing_accounts), Some(new_accounts)) =
                    (existing.get_mut("accounts"), new.get("accounts"))
                {
                    for (k, v) in new_accounts.as_object().unwrap() {
                        existing_accounts
                            .as_object_mut()
                            .unwrap()
                            .insert(k.clone(), v.clone());
                    }
                }

                if let (Some(existing_storage), Some(new_storage)) =
                    (existing.get_mut("storage"), new.get("storage"))
                {
                    for (k, v) in new_storage.as_object().unwrap() {
                        existing_storage
                            .as_object_mut()
                            .unwrap()
                            .insert(k.clone(), v.clone());
                    }
                }

                let merged_state = existing.to_string();
                fs::write("./state_8009.json", merged_state.clone()).unwrap();
                return Ok((result_json, merged_state));
            } else {
                // if no existing state, just write the new state
                fs::write("./state_8009.json", new_state_json.clone()).unwrap();
                return Ok((result_json, new_state_json));
            }
        }
        Err(err) => Ok((
            json_error(&format!("Error executing transaction: {:?}", err)),
            previous_state.unwrap_or_default(),
        )),
    }
}
