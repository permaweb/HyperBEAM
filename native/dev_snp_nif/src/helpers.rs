use sev::certs::snp::{ca, Certificate};
use sev::firmware::host::TcbVersion;
use crate::logging::log_message;
use reqwest::blocking::get;

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
// Blocking version of reqwest
    let url = format!("{KDS_CERT_SITE}{KDS_VCEK}/{sev_prod_name}/{KDS_CERT_CHAIN}");
    // log_message(
    //     "INFO",
    //     file!(),
    //     line!(),
    //     &format!("Requesting AMD certificate chain from: {url}"),
    // );

    let response = get(&url)?;
    if !response.status().is_success() {
        return Err(format!("KDS returned HTTP {}: {}", response.status(), url).into());
    }
    let body = response.bytes()?;

    // Parse the response as a PEM-encoded certificate chain
    let chain = openssl::x509::X509::stack_from_pem(&body)
        .map_err(|e| format!("Failed to parse cert chain PEM ({} bytes): {:?}", body.len(), e))?;
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
    use reqwest::blocking::get; // Blocking version of reqwest

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

    let response = get(&url)?;
    if !response.status().is_success() {
        return Err(format!("KDS returned HTTP {}: {}", response.status(), url).into());
    }
    let rsp_bytes = response.bytes()?;

    // Parse the VCEK response as a DER-encoded certificate
    let vcek_cert = Certificate::from_der(&rsp_bytes)
        .map_err(|e| format!("Failed to parse VCEK DER ({} bytes): {:?}", rsp_bytes.len(), e))?;

    // log_message("INFO", file!(), line!(), "Successfully fetched VCEK.");
    Ok(vcek_cert)
}
