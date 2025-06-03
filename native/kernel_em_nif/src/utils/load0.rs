use crate::core::constants::{LOAD0_ENDPOINT, FAILED_LOAD0_UPLOAD_HASH};
use anyhow::{Error, anyhow};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct Load0UploadResponse {
    pub optimistic_hash: String,
    pub success: bool,
}

pub fn upload_to_load0(data: Vec<u8>, content_type: &str) -> Result<String, Error> {
    let upload_url = format!("{}/upload", LOAD0_ENDPOINT);
    
    let response = ureq::post(&upload_url)
        .header("Content-Type", content_type)
        .header("X-Load-Authorization", "load_acc_0UHyLmhvJwtvpO3XHuf4dHHGhiYJbf1E") // yeah i know, but this is fine for now
        .send(&data)?;
    
    if !response.status().is_success() {
        return Err(anyhow!(
            "Failed to upload to Load0: Status {}, Body: {}",
            response.status(),
            "err sending data to load0"
        ));
    }


    let upload_response: Load0UploadResponse = response.into_body().read_json()?;
    
    if upload_response.success {
        return Ok(upload_response.optimistic_hash);
    }
    
    Ok(String::from(FAILED_LOAD0_UPLOAD_HASH))
}