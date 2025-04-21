use ethers::types::Transaction;
use ethers::utils::rlp;
use hex::FromHex;
use revm::primitives::alloy_primitives::Address;
use revm::primitives::TxKind;

pub fn recover_signer(raw_tx_hex: &str) -> Address {
    // let raw_tx_hex = "02f87782251881898447868c008447868c008307a12094d5bb24152217bea7a617525ddfa64ea3b41b9c0a89056bc75e2d6310000080c001a03a29fc3c0d9a0e3b4c73ed5976c016be447965b894a5c09f18485ddf693fabf1a07856dcf54d5a5d5deca3b223cdea3a0b0b3a86e025d16e48db15afa4adb2f855"; // hex string
    let raw_tx_bytes: Vec<u8> = Vec::from_hex(raw_tx_hex).unwrap();

    let tx: Transaction = rlp::decode(&raw_tx_bytes).unwrap();
    let signer = tx.recover_from().unwrap();
    println!("From: {:?}", tx.recover_from().unwrap());
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
