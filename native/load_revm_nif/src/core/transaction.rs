use ethers::types::Transaction;
use ethers::utils::rlp;
use hex::FromHex;
use revm::primitives::alloy_primitives::Address;
use revm::primitives::TxKind;

pub fn recover_signer(raw_tx_hex: &str) -> Address {
    let raw_tx_bytes: Vec<u8> = Vec::from_hex(raw_tx_hex).unwrap();
    let tx: Transaction = rlp::decode(&raw_tx_bytes).unwrap();
    let signer = tx.recover_from().unwrap();
    Address::from_slice(signer.as_bytes())
}

pub fn get_tx_object(raw_tx_hex: &str) -> Transaction {
    let raw_tx_bytes = Vec::from_hex(raw_tx_hex).unwrap();
    rlp::decode(&raw_tx_bytes).unwrap()
}

pub fn get_tx_kind(tx: Transaction) -> TxKind {
    if tx.to.is_none() {
        TxKind::Create
    } else {
        let target = Address::from_slice(tx.to.unwrap().as_bytes());
        TxKind::Call(target)
    }
}
