use revm::primitives::{Address, U256};

pub fn parse_address(addr: &str) -> Result<Address, String> {
    let addr = addr.trim_start_matches("0x");
    if addr.len() != 40 {
        return Err("Address must be 20 bytes (40 hex characters)".to_string());
    }

    let bytes = hex::decode(addr).map_err(|e| "Invalid hex string".to_string())?;

    let mut address = [0u8; 20];
    address.copy_from_slice(&bytes);
    Ok(address.into())
}

pub fn parse_u256(value: &str) -> Result<U256, String> {
    if value.starts_with("0x") {
        let hex_val = value.trim_start_matches("0x");
        match U256::from_str_radix(hex_val, 16) {
            Ok(v) => Ok(v),
            Err(_) => Err("Invalid hex value".to_string()),
        }
    } else {
        match value.parse::<U256>() {
            Ok(v) => Ok(v),
            Err(_) => Err("Invalid decimal value".to_string()),
        }
    }
}

pub fn json_error(message: &str) -> String {
    let mut response = serde_json::Map::new();
    response.insert("success".to_string(), serde_json::Value::Bool(false));
    response.insert(
        "error".to_string(),
        serde_json::Value::String(message.to_string()),
    );

    serde_json::to_string(&response).unwrap_or_else(|_| {
        format!(
            "{{\"success\":false,\"error\":\"{}\"}}",
            message.replace("\"", "\\\"")
        )
    })
}
