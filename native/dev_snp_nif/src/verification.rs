use rustler::{Binary, Encoder, Env, NifResult, Term};
use rustler::types::atom::{self, ok};
use sev::certs::snp::{Chain, Verifiable};
use sev::firmware::guest::AttestationReport;
use crate::helpers::{request_cert_chain, request_vcek};
use crate::logging::log_message;
use serde::Deserialize;

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

    // Define a struct for extracting the measurement field.
    #[derive(Debug, Deserialize)]
    struct AttestationReportMeasurement {
        measurement: Vec<u8>,
        // Additional fields can be added here if needed.
    }

    // Step 1: Deserialize the JSON report.
    let report: AttestationReportMeasurement = match serde_json::from_slice(_report.as_slice()) {
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
#[rustler::nif(schedule = "DirtyIo")]
fn verify_signature<'a>(
    env: Env<'a>,
    report: Binary<'a>,
) ->  NifResult<Term<'a>>  {
    // Step 1: Parse the report JSON into an SEV attestation report structure.
    let attestation_report: AttestationReport = match serde_json::from_slice(report.as_slice()) {
        Ok(data) => data,
        Err(err) => {
            return Ok((atom::error(), format!("Invalid report JSON: {}", err)).encode(env));
        }
    };

    // Step 3: Extract the chip ID and TCB version.
    let reported_tcb_version = attestation_report.reported_tcb;
    let chip_id = attestation_report.chip_id;

    // Step 4: Request the certificate chain and VCEK.
    let ca = match request_cert_chain("Milan") {
        Ok(chain) => chain,
        Err(e) => {
            return Ok((atom::error(), format!("Failed to fetch certificate chain: {}", e)).encode(env));
        }
    };
    let vcek = match request_vcek(chip_id, reported_tcb_version) {
        Ok(cert) => cert,
        Err(e) => {
            return Ok((atom::error(), format!("Failed to fetch VCEK certificate: {}", e)).encode(env));
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
