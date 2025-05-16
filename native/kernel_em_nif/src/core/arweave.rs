use anyhow::Error;

pub fn retrieve_kernel_src(kernel_id: &str) -> Result<String, Error> {
    let url = format!("https://arweave.net/{}", kernel_id);
    let response = ureq::get(&url).call().unwrap();
    let kernel_source = response
        .into_body()
        .read_to_string()
        .map_err(|e| e.to_string())
        .unwrap();
    Ok(kernel_source)
}
