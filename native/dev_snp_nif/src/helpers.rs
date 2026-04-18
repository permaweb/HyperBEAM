use sev::certs::snp::{ca, Certificate};
use sev::firmware::host::TcbVersion;
use reqwest::blocking::Client;
use std::time::Duration;

const REQUEST_TIMEOUT_SECONDS: u64 = 10;
const KDS_TIMEOUT: Duration = Duration::from_secs(REQUEST_TIMEOUT_SECONDS);

fn fetch_bytes(url: &str) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    let response = Client::builder()
        .timeout(KDS_TIMEOUT)
        .build()?
        .get(url)
        .send()?
        .error_for_status()?;

    Ok(response.bytes()?.to_vec())
}

/// Base URL for AMD's Key Distribution Service (KDS).
const KDS_CERT_SITE: &str = "https://kdsintf.amd.com";
/// Endpoint for the VCEK API.
const KDS_VCEK: &str = "/vcek/v1";
/// Endpoint for the Certificate Chain API.
const KDS_CERT_CHAIN: &str = "cert_chain";

/// Requests the AMD certificate chain (ASK + ARK) for the given SEV product name.
///
/// # Arguments
/// * `sev_prod_name` - The SEV product name (e.g., "Milan").
///
/// # Returns
/// A `ca::Chain` containing the ASK and ARK certificates.
///
/// # Errors
/// Returns an error if the request fails, the response is invalid, or the certificate parsing fails.
///
/// # Example
/// ```erlang
/// {ok, CertChain} = dev_snp_nif:request_cert_chain("Milan").
pub fn request_cert_chain(sev_prod_name: &str) -> Result<ca::Chain, Box<dyn std::error::Error>> {
    let url = format!("{KDS_CERT_SITE}{KDS_VCEK}/{sev_prod_name}/{KDS_CERT_CHAIN}");
    // log_message(
    //     "INFO",
    //     file!(),
    //     line!(),
    //     &format!("Requesting AMD certificate chain from: {url}"),
    // );

    // Perform the blocking GET request
    let body = fetch_bytes(&url)?;

    // Parse the response as a PEM-encoded certificate chain
    let chain = openssl::x509::X509::stack_from_pem(&body)?;
    if chain.len() < 2 {
        return Err("Expected at least two certificates (ARK and ASK) in the chain".into());
    }

    // Convert ARK and ASK into the `ca::Chain` structure required by the SEV crate
    let ark = chain[1].to_pem()?;
    let ask = chain[0].to_pem()?;
    let ca_chain = ca::Chain::from_pem(&ark, &ask)?;

    //log_message(
    //    "INFO",
    //    file!(),
    //    line!(),
    //    "Successfully fetched AMD certificate chain.",
    //);

    Ok(ca_chain)
}

/// Requests the VCEK for the given chip ID and reported TCB.
///
/// # Arguments
/// * `chip_id` - The unique 64-byte chip ID.
/// * `reported_tcb` - The TCB version of the platform.
///
/// # Returns
/// A `Certificate` representing the VCEK.
///
/// # Errors
/// Returns an error if the request fails, the response is invalid, or the certificate parsing fails.
///
/// # Example
/// ```erlang
/// {ok, VcekCert} = dev_snp_nif:request_vcek(ChipIdBinary, ReportedTcbMap).
/// ```
pub fn request_vcek(
    chip_id: [u8; 64],
    reported_tcb: TcbVersion,
) -> Result<Certificate, Box<dyn std::error::Error>> {
    let hw_id = chip_id
        .iter()
        .map(|byte| format!("{:02x}", byte))
        .collect::<String>();

    let url = format!(
        "{KDS_CERT_SITE}{KDS_VCEK}/Milan/{hw_id}?blSPL={:02}&teeSPL={:02}&snpSPL={:02}&ucodeSPL={:02}",
        reported_tcb.bootloader, reported_tcb.tee, reported_tcb.snp, reported_tcb.microcode
    );

    // log_message(
    //     "INFO",
    //     file!(),
    //     line!(),
    //     &format!("Requesting VCEK from: {url}"),
    // );

    // Perform the blocking GET request
    let rsp_bytes = fetch_bytes(&url)?;

    // Parse the VCEK response as a DER-encoded certificate
    let vcek_cert = Certificate::from_der(&rsp_bytes)?;

    // log_message("INFO", file!(), line!(), "Successfully fetched VCEK.");
    Ok(vcek_cert)
}
