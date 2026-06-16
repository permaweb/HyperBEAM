use sev::certs::snp::{ca, Certificate};
use sev::firmware::host::TcbVersion;
use reqwest::blocking::get;
use std::fs;
use std::path::PathBuf;

/// Base URL for AMD's Key Distribution Service (KDS).
const KDS_CERT_SITE: &str = "https://kdsintf.amd.com";
/// Endpoint for the VCEK API.
const KDS_VCEK: &str = "/vcek/v1";
/// Endpoint for the Certificate Chain API.
const KDS_CERT_CHAIN: &str = "cert_chain";

/// On-disk cache for AMD KDS material. The ARK/ASK chain (per product) and the
/// VCEK (per chip-id + reported TCB) are immutable, and AMD rate-limits KDS
/// requests, so caching them keeps verification fast and avoids rate-limit
/// failures on repeated verifies. Best-effort: if the cache directory cannot be
/// created, the fetch path is used directly.
fn cache_dir() -> Option<PathBuf> {
    let dir = std::env::temp_dir().join("hb-snp-cache");
    fs::create_dir_all(&dir).ok().map(|_| dir)
}

fn cache_read(name: &str) -> Option<Vec<u8>> {
    let path = cache_dir()?.join(name);
    match fs::read(&path) {
        Ok(bytes) if !bytes.is_empty() => Some(bytes),
        _ => None,
    }
}

fn cache_write(name: &str, bytes: &[u8]) {
    if let Some(dir) = cache_dir() {
        let _ = fs::write(dir.join(name), bytes);
    }
}

/// Requests the AMD certificate chain (ASK + ARK) for the given SEV product
/// name, reading from the on-disk cache first.
///
/// # Arguments
/// * `sev_prod_name` - The SEV product name (e.g., "Milan").
///
/// # Returns
/// A `ca::Chain` containing the ASK and ARK certificates.
pub fn request_cert_chain(sev_prod_name: &str) -> Result<ca::Chain, Box<dyn std::error::Error>> {
    let cache_name = format!("cert_chain_{sev_prod_name}.pem");
    let body = match cache_read(&cache_name) {
        Some(bytes) => bytes,
        None => {
            let url = format!("{KDS_CERT_SITE}{KDS_VCEK}/{sev_prod_name}/{KDS_CERT_CHAIN}");
            let response = get(&url)?;
            let bytes = response.bytes()?.to_vec();
            // Only cache a parsable chain so a rate-limit/error page is not stored.
            if openssl::x509::X509::stack_from_pem(&bytes)
                .map(|c| c.len() >= 2)
                .unwrap_or(false)
            {
                cache_write(&cache_name, &bytes);
            }
            bytes
        }
    };

    // Parse the response as a PEM-encoded certificate chain
    let chain = openssl::x509::X509::stack_from_pem(&body)?;
    if chain.len() < 2 {
        return Err("Expected at least two certificates (ARK and ASK) in the chain".into());
    }

    // Convert ARK and ASK into the `ca::Chain` structure required by the SEV crate
    let ark = chain[1].to_pem()?;
    let ask = chain[0].to_pem()?;
    let ca_chain = ca::Chain::from_pem(&ark, &ask)?;

    Ok(ca_chain)
}

/// Requests the VCEK for the given chip ID and reported TCB, reading from the
/// on-disk cache first.
///
/// # Arguments
/// * `chip_id` - The unique 64-byte chip ID.
/// * `reported_tcb` - The TCB version of the platform.
///
/// # Returns
/// A `Certificate` representing the VCEK.
pub fn request_vcek(
    chip_id: [u8; 64],
    reported_tcb: TcbVersion,
) -> Result<Certificate, Box<dyn std::error::Error>> {
    let hw_id = chip_id
        .iter()
        .map(|byte| format!("{:02x}", byte))
        .collect::<String>();

    let cache_name = format!(
        "vcek_{hw_id}_{:02}{:02}{:02}{:02}.der",
        reported_tcb.bootloader, reported_tcb.tee, reported_tcb.snp, reported_tcb.microcode
    );

    let rsp_bytes = match cache_read(&cache_name) {
        Some(bytes) => bytes,
        None => {
            let url = format!(
                "{KDS_CERT_SITE}{KDS_VCEK}/Milan/{hw_id}?blSPL={:02}&teeSPL={:02}&snpSPL={:02}&ucodeSPL={:02}",
                reported_tcb.bootloader, reported_tcb.tee, reported_tcb.snp, reported_tcb.microcode
            );
            let response = get(&url)?;
            let bytes = response.bytes()?.to_vec();
            // Only cache a valid DER certificate, never a rate-limit/error page.
            if Certificate::from_der(&bytes).is_ok() {
                cache_write(&cache_name, &bytes);
            }
            bytes
        }
    };

    // Parse the VCEK response as a DER-encoded certificate
    let vcek_cert = Certificate::from_der(&rsp_bytes)?;
    Ok(vcek_cert)
}
