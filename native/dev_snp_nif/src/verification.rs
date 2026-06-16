use rustler::{Binary, Encoder, Env, NifResult, Term};
use rustler::types::atom::{self, ok};
use serde_json::Value;
use serde::Deserialize;
use sev::certs::snp::{ecdsa::Signature, Chain, Verifiable};
use sev::firmware::host::TcbVersion;
use sev::firmware::guest::{AttestationReport, GuestPolicy, PlatformInfo};
use crate::helpers::{request_cert_chain, request_vcek};
use crate::logging::log_message;

/// Verifies whether the measurement in the attestation report matches the expected measurement.
///
/// # Arguments
/// * `env` - The Rustler environment, used to encode the return value.
/// * `_report` - A binary containing the serialized attestation report (JSON format).
/// * `_expected_measurement` - A binary containing the expected measurement (as a byte array).
///
/// # Returns
/// A tuple with:
/// - `ok` atom and a success message if the measurements match.
/// - `error` atom and an error message if the measurements do not match.
#[rustler::nif]
fn verify_measurement<'a>(
    env: Env<'a>,
    _report: Binary,
    _expected_measurement: Binary,
) -> NifResult<Term<'a>> {
    //log_message("INFO", file!(), line!(), "Starting measurement verification...");

    // Define a struct for deserializing the attestation report.
    #[derive(Debug, Deserialize)]
    struct AttestationReport {
        measurement: Vec<u8>,
        // Additional fields can be added here if needed.
    }

    // Step 1: Deserialize the JSON report.
    let report: AttestationReport = match serde_json::from_slice(_report.as_slice()) {
        Ok(parsed_report) => {
            //log_message(
            //    "INFO",
            //    file!(),
            //    line!(),
            //    &format!("Successfully parsed report: {:?}", parsed_report),
            //);
            parsed_report
        }
        Err(err) => {
            log_message(
                "ERROR",
                file!(),
                line!(),
                &format!("Failed to deserialize report: {:?}", err),
            );
            return Ok((atom::error(), "Invalid report format").encode(env));
        }
    };

    // Step 2: Extract the actual measurement from the report.
    let actual_measurement = &report.measurement;
    // log_message(
    //     "INFO",
    //     file!(),
    //     line!(),
    //     &format!("Extracted actual measurement: {:?}", actual_measurement),
    // );

    // Step 3: Decode the expected measurement from the input binary.
    let expected_measurement: Vec<u8> = _expected_measurement.as_slice().to_vec();
    // log_message(
    //     "INFO",
    //     file!(),
    //     line!(),
    //     &format!("Decoded expected measurement: {:?}", expected_measurement),
    // );

    // Step 4: Compare the actual and expected measurements.
    if actual_measurement == &expected_measurement {
        //log_message("INFO", file!(), line!(), "Measurements match.");
        Ok((atom::ok(), true).encode(env))
    } else {
        //log_message("ERROR", file!(), line!(), "Measurements do not match.");
        Ok((atom::error(), false).encode(env))
    }
}


/// Verifies the signature of an attestation report.
///
/// # Arguments
/// * `env` - The Rustler environment, used to encode the return value.
/// * `report` - A binary containing the serialized attestation report.
///
/// # Returns
/// A tuple with:
/// - `ok` atom and a success message if the signature is valid.
/// - `error` atom and an error message if the signature verification fails.
#[rustler::nif]
fn verify_signature<'a>(
    env: Env<'a>,
    report: Binary<'a>,
) ->  NifResult<Term<'a>>  {
    // log_message("INFO", file!(), line!(), "Verifying signature...");

    // Step 1: Parse the report JSON into a serde Value object.
    let json_data = match serde_json::from_slice::<Value>(report.as_slice()) {
        Ok(data) => data,
        Err(err) => {
            return Ok((
                rustler::types::atom::error(),
                format!("Failed to parse JSON: {}", err),
            )
                .encode(env));
        }
    };

    // Step 2: Map JSON fields to the AttestationReport struct.
    // Each field is individually parsed to ensure type safety.
    let attestation_report = AttestationReport {
        version: json_data["version"].as_u64().unwrap_or(0) as u32,
        guest_svn: json_data["guest_svn"].as_u64().unwrap_or(0) as u32,
        policy: GuestPolicy(json_data["policy"].as_u64().unwrap_or(0)),
        family_id: json_data["family_id"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .map(|v| v.as_u64().unwrap_or(0) as u8)
            .collect::<Vec<u8>>()
            .try_into()
            .unwrap_or([0; 16]),
        image_id: json_data["image_id"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .map(|v| v.as_u64().unwrap_or(0) as u8)
            .collect::<Vec<u8>>()
            .try_into()
            .unwrap_or([0; 16]),
        vmpl: json_data["vmpl"].as_u64().unwrap_or(0) as u32,
        sig_algo: json_data["sig_algo"].as_u64().unwrap_or(0) as u32,
        current_tcb: TcbVersion {
            bootloader: json_data["current_tcb"]["bootloader"].as_u64().unwrap_or(0) as u8,
            tee: json_data["current_tcb"]["tee"].as_u64().unwrap_or(0) as u8,
            snp: json_data["current_tcb"]["snp"].as_u64().unwrap_or(0) as u8,
            microcode: json_data["current_tcb"]["microcode"].as_u64().unwrap_or(0) as u8,
            _reserved: [0; 4],
        },
        plat_info: PlatformInfo(json_data["plat_info"].as_u64().unwrap_or(0)),
        _author_key_en: json_data["_author_key_en"].as_u64().unwrap_or(0) as u32,
        _reserved_0: json_data["_reserved_0"].as_u64().unwrap_or(0) as u32,
        report_data: json_data["report_data"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .map(|v| v.as_u64().unwrap_or(0) as u8)
            .collect::<Vec<u8>>()
            .try_into()
            .unwrap_or([0; 64]),
        measurement: json_data["measurement"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .map(|v| v.as_u64().unwrap_or(0) as u8)
            .collect::<Vec<u8>>()
            .try_into()
            .unwrap_or([0; 48]),
        host_data: json_data["host_data"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .map(|v| v.as_u64().unwrap_or(0) as u8)
            .collect::<Vec<u8>>()
            .try_into()
            .unwrap_or([0; 32]),
        id_key_digest: json_data["id_key_digest"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .map(|v| v.as_u64().unwrap_or(0) as u8)
            .collect::<Vec<u8>>()
            .try_into()
            .unwrap_or([0; 48]),
        author_key_digest: json_data["author_key_digest"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .map(|v| v.as_u64().unwrap_or(0) as u8)
            .collect::<Vec<u8>>()
            .try_into()
            .unwrap_or([0; 48]),
        report_id: json_data["report_id"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .map(|v| v.as_u64().unwrap_or(0) as u8)
            .collect::<Vec<u8>>()
            .try_into()
            .unwrap_or([0; 32]),
        report_id_ma: json_data["report_id_ma"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .map(|v| v.as_u64().unwrap_or(0) as u8)
            .collect::<Vec<u8>>()
            .try_into()
            .unwrap_or([0; 32]),
        reported_tcb: TcbVersion {
            bootloader: json_data["reported_tcb"]["bootloader"]
                .as_u64()
                .unwrap_or(0) as u8,
            tee: json_data["reported_tcb"]["tee"].as_u64().unwrap_or(0) as u8,
            snp: json_data["reported_tcb"]["snp"].as_u64().unwrap_or(0) as u8,
            microcode: json_data["reported_tcb"]["microcode"].as_u64().unwrap_or(0) as u8,
            _reserved: [0; 4],
        },
        _reserved_1: [0; 24],
        chip_id: json_data["chip_id"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .map(|v| v.as_u64().unwrap_or(0) as u8)
            .collect::<Vec<u8>>()
            .try_into()
            .unwrap_or([0; 64]),
        committed_tcb: TcbVersion {
            bootloader: json_data["committed_tcb"]["bootloader"]
                .as_u64()
                .unwrap_or(0) as u8,
            tee: json_data["committed_tcb"]["tee"].as_u64().unwrap_or(0) as u8,
            snp: json_data["committed_tcb"]["snp"].as_u64().unwrap_or(0) as u8,
            microcode: json_data["committed_tcb"]["microcode"]
                .as_u64()
                .unwrap_or(0) as u8,
            _reserved: [0; 4],
        },
        current_build: json_data["current_build"].as_u64().unwrap_or(0) as u8,
        current_minor: json_data["current_minor"].as_u64().unwrap_or(0) as u8,
        current_major: json_data["current_major"].as_u64().unwrap_or(0) as u8,
        _reserved_2: json_data["_reserved_2"].as_u64().unwrap_or(0) as u8,
        committed_build: json_data["committed_build"].as_u64().unwrap_or(0) as u8,
        committed_minor: json_data["committed_minor"].as_u64().unwrap_or(0) as u8,
        committed_major: json_data["committed_major"].as_u64().unwrap_or(0) as u8,
        _reserved_3: json_data["_reserved_3"].as_u64().unwrap_or(0) as u8,
        launch_tcb: TcbVersion {
            bootloader: json_data["launch_tcb"]["bootloader"].as_u64().unwrap_or(0) as u8,
            tee: json_data["launch_tcb"]["tee"].as_u64().unwrap_or(0) as u8,
            snp: json_data["launch_tcb"]["snp"].as_u64().unwrap_or(0) as u8,
            microcode: json_data["launch_tcb"]["microcode"].as_u64().unwrap_or(0) as u8,
            _reserved: [0; 4],
        },
        _reserved_4: [0; 168],
        signature: Signature {
            r: json_data["signature"]["r"]
                .as_array()
                .unwrap_or(&vec![])
                .iter()
                .map(|v| v.as_u64().unwrap_or(0) as u8)
                .collect::<Vec<u8>>()
                .try_into()
                .unwrap_or([0; 72]),
            s: json_data["signature"]["s"]
                .as_array()
                .unwrap_or(&vec![])
                .iter()
                .map(|v| v.as_u64().unwrap_or(0) as u8)
                .collect::<Vec<u8>>()
                .try_into()
                .unwrap_or([0; 72]),
            _reserved: [0; 368],
        },
    };

    // Step 3: Extract the chip ID and TCB version.
    let chip_id_array: [u8; 64] = attestation_report
        .chip_id
        .try_into()
        .expect("chip_id must be 64 bytes");
    let tcb_version = attestation_report.current_tcb;

    // Step 4: Request the certificate chain and VCEK (cached; AMD KDS is
    // rate-limited and may be unreachable). On failure return a clean
    // `{ok, false}' rather than panicking the BEAM scheduler, so the Erlang
    // verifier treats the attestation as unverified instead of crashing.
    let ca = match request_cert_chain("Milan") {
        Ok(ca) => ca,
        Err(e) => {
            log_message(
                "ERROR",
                file!(),
                line!(),
                &format!("AMD cert chain unavailable: {:?}", e),
            );
            return Ok((ok(), false).encode(env));
        }
    };
    let vcek = match request_vcek(chip_id_array, tcb_version) {
        Ok(vcek) => vcek,
        Err(e) => {
            log_message(
                "ERROR",
                file!(),
                line!(),
                &format!("AMD VCEK unavailable: {:?}", e),
            );
            return Ok((ok(), false).encode(env));
        }
    };

    // Step 5: Verify the certificate chain.
    if let Err(e) = ca.verify() {
        log_message(
            "ERROR",
            file!(),
            line!(),
            &format!("CA chain verification failed: {:?}", e),
        );
        return Ok((atom::error(), format!("CA verification failed: {:?}", e)).encode(env));
    }
    //log_message("INFO", file!(), line!(), "CA chain verification successful.");

    // Step 6: Verify the attestation report.
    let cert_chain = Chain { ca, vek: vcek };
    if let Err(e) = (&cert_chain, &attestation_report).verify() {
        log_message(
            "ERROR",
            file!(),
            line!(),
            &format!("Attestation report verification failed: {:?}", e),
        );
        return Ok((atom::error(), format!("Report verification failed: {:?}", e)).encode(env));
    }

    //log_message("INFO", file!(), line!(), "Signature verification successful.");
    Ok((ok(), true).encode(env))
}
