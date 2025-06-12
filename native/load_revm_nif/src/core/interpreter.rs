use crate::core::evm::CustomEvm;
use crate::core::state::{deserialize_state, serialize_state};
use crate::core::transaction::{get_tx_kind, get_tx_object, recover_signer};
use crate::utils::constants::{CONTRACT_SIZE_LIMIT, GENESIS_ADDRESS, TX_GAS_LIMIT};
use crate::utils::misc::json_error;
use ethers_core::types::Transaction;
use revm::state::{Account, AccountInfo};
use revm::{
    context::TxEnv,
    database::{CacheDB, EmptyDB},
    primitives::{hardfork::SpecId, Address, Bytes, U256},
    Context, ExecuteEvm, MainContext,
};
use rustler::NifResult;
use serde_json::{json, Value};
use std::str::FromStr;
use std::{collections::HashMap, fs};

type CustomEvmType = CustomEvm<
    Context<
        revm::context::BlockEnv,
        TxEnv,
        revm::context::CfgEnv,
        CacheDB<revm::database::EmptyDBTyped<std::convert::Infallible>>,
    >,
    (),
>;

fn build_tx_env(tx: &Transaction, sender: Address) -> Result<TxEnv, String> {
    let input = tx.input.to_vec();
    let chain_id = tx.chain_id.unwrap().to_string().parse::<u64>().unwrap_or(1);

    let mut env = TxEnv::default();
    env.gas_limit = TX_GAS_LIMIT;
    env.chain_id = Some(chain_id);
    env.data = Bytes::from(input);
    env.value = U256::from(tx.value.as_u128());
    env.gas_price = 7;
    env.caller = sender;
    env.nonce = tx.nonce.to_string().parse::<u64>().unwrap_or_default();
    env.kind = get_tx_kind(tx);

    println!("TX_ENV: {:?}", env);
    Ok(env)
}

fn load_or_init_db(prev: Option<&str>) -> Result<CacheDB<EmptyDB>, String> {
    if let Some(state_json) = prev {
        deserialize_state(state_json).map_err(|e| format!("Failed to deserialize state: {}", e))
    } else {
        // init new DB
        // dirty genesis setup for now
        // todo: @rani, is this needed?
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
        Ok(db)
    }
}

fn build_evm(db: CacheDB<EmptyDB>) -> CustomEvmType {
    let ctx = Context::mainnet().with_db(db).modify_cfg_chained(|cfg| {
        cfg.spec = SpecId::CANCUN;
        cfg.chain_id = 9496;
        cfg.disable_block_gas_limit = true;
        cfg.limit_contract_code_size = Some(CONTRACT_SIZE_LIMIT);
    });

    CustomEvm::new(ctx, ())
}

fn execute_and_persist(
    evm: &mut CustomEvmType,
    tx_env: TxEnv,
    path: &str,
) -> Result<(String, String), String> {
    let caller = tx_env.caller.clone();

    let result = evm.0.transact(tx_env).map_err(|e| e.to_string())?;
    let mut resp = serde_json::Map::new();
    resp.insert("success".into(), Value::Bool(true));
    resp.insert("sender".into(), json!(format!("0x{:x}", caller)));
    resp.insert("gas_used".into(), json!(result.result.gas_used()));

    if let Some(out) = result.result.output() {
        resp.insert("output".into(), json!(format!("0x{}", hex::encode(out))));
    }
    if let Some(addr) = result.result.created_address() {
        resp.insert("contract_address".into(), json!(format!("0x{:x}", addr)));
    }

    let result_json =
        serde_json::to_string(&resp).unwrap_or_else(|_| json_error("Serialization failed"));
    let state_map: HashMap<Address, Account> = result.state;
    let new_state = serialize_state(state_map.into())?;

    // Merge or write state file
    let merged = if let Ok(existing) = fs::read_to_string(path) {
        merge_states(&existing, &new_state)?
    } else {
        new_state.clone()
    };
    fs::write(path, &merged).map_err(|e| e.to_string())?;

    Ok((result_json, merged))
}

fn merge_states(old: &str, new: &str) -> Result<String, String> {
    let mut existing: Value =
        serde_json::from_str(old).map_err(|e| json_error(&(e.to_string())))?;
    let new: Value = serde_json::from_str(new).map_err(|e| json_error(&(e.to_string())))?;
    for key in ["accounts", "storage"] {
        if let (Some(ex_obj), Some(new_obj)) = (
            existing.get_mut(key).and_then(Value::as_object_mut),
            new.get(key).and_then(Value::as_object),
        ) {
            for (k, v) in new_obj {
                ex_obj.insert(k.clone(), v.clone());
            }
        }
    }
    Ok(existing.to_string())
}

pub fn eval(
    raw_tx_hex: String,
    previous_state: Option<String>,
    state_path: String,
) -> NifResult<(String, String)> {
    // Parse transaction and signer
    let tx = get_tx_object(&raw_tx_hex);
    let sender = recover_signer(&raw_tx_hex);
    println!("TX SENDER: {:?}", sender);

    // Build environment and database
    let tx_env = match build_tx_env(&tx, sender).map_err(|e| (json_error(&e), "{}".to_string())) {
        Ok(ok) => ok,
        Err(e) => return Ok(e),
    };
    let mut db = match load_or_init_db(previous_state.as_deref()) {
        Ok(db) => db,
        Err(e) => return Ok((json_error(&e), "{}".to_string())),
    };

    // Create EVM instance
    let mut evm = build_evm(db);

    // Execute and persist
    let (result_json, new_state) = match execute_and_persist(&mut evm, tx_env, &state_path) {
        Ok(res) => res,
        Err(e) => return Ok((json_error(&e), "{}".to_string())),
    };

    Ok((result_json, new_state))
}
