use crate::core::constants::{FAILED_LOAD0_UPLOAD_HASH, LOAD0_ENDPOINT};
use anyhow::{Error, anyhow};
use serde::{Deserialize, Serialize};
use ureq::{Body, http::response::Response};

#[derive(Debug, Serialize, Deserialize)]
pub struct Load0UploadResponse {
    pub optimistic_hash: String,
    pub success: bool,
}

pub fn upload_to_load0(data: Vec<u8>, content_type: &str) -> Result<String, Error> {
    let upload_url = format!("{}/upload", LOAD0_ENDPOINT);
    let auth_token = "load_acc_0UHyLmhvJwtvpO3XHuf4dHHGhiYJbf1E"; // Ideally move this to config/env

    let response = ureq::post(&upload_url)
        .header("Content-Type", content_type)
        .header("X-Load-Authorization", auth_token)
        .send(&data)?;

    handle_response(response)
}

fn handle_response(response: Response<Body>) -> Result<String, Error> {
    let status = response.status();
    let success = status.is_success();
    let mut extracted_body = response.into_body();

    if !success {
        return Err(anyhow!(
            "Failed to upload to Load0 (status: {}): {}",
            status,
            extracted_body
                .read_to_string()
                .unwrap_or_else(|_| "unable to read body".to_string())
        ));
    } else {
        let upload_response: Load0UploadResponse = extracted_body.read_json()?;

        if upload_response.success {
            Ok(upload_response.optimistic_hash)
        } else {
            Ok(FAILED_LOAD0_UPLOAD_HASH.to_string())
        }
    }
}
