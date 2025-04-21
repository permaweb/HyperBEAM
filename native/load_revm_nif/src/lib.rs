pub mod evm;
pub mod handler;

use crate::{evm::MyEvm as CustomEvm, handler::MyHandler};
use rustler::{Encoder, Env, NifResult, Term};
use std::hash::Hash;
use revm::{
    context::TxEnv,
    database::InMemoryDB,
    handler::{ExecuteCommitEvm, ExecuteEvm, Handler},
    inspector::InspectorHandler,
    Context, MainContext,
};

mod atoms {
    rustler::atoms! {
        ok,
    }
}

#[rustler::nif]
fn hello() -> NifResult<String> {
    Ok("Hello world!".to_string())
}

#[rustler::nif]
fn load_evm() -> NifResult<String> {
	let mut load_evm = CustomEvm::new(Context::mainnet(), ());
	// println!("{:?}", load_evm.0);
	let _res_and_state = load_evm.0.transact(TxEnv::default()).unwrap();
	println!("{:?}", _res_and_state);
    let res = _res_and_state.result.output().unwrap().to_string();

    Ok(res)
}

rustler::init!("load_revm_nif", [hello, load_evm]);
