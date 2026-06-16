use rustler::{Binary, Encoder, Env, NifResult, Term};
use rustler::types::atom::{self, ok};
use sev::firmware::guest::{Firmware, AttestationReport};
use serde_json::to_string;
use crate::logging::log_message;

/// Generates an attestation report using the provided unique data and VMPL value.
///
/// # Arguments
/// * `env` - The Rustler environment, used to encode the return value.
/// * `unique_data` - A 64-byte binary input containing unique data for the attestation report.
/// * `vmpl` - The Virtual Machine Privilege Level (VMPL) to be used in the report.
///
/// # Returns
/// A tuple containing an `ok` atom and the serialized attestation report in JSON format.
/// If an error occurs during the generation or serialization process, an error is returned.
///
/// # Example
/// ```erlang
/// {ok, JsonReport} = dev_snp_nif:generate_attestation_report(UniqueDataBinary, VMPL).
/// ```
#[rustler::nif]
pub fn generate_attestation_report<'a>(
    env: Env<'a>,
    unique_data: Binary,
    vmpl: u32,
) -> NifResult<Term<'a>> {
    log_message("INFO", file!(), line!(), "Starting attestation report generation...");

    // Step 1: Convert the binary input to a fixed-size array.
    let unique_data_array: [u8; 64] = match unique_data.as_slice().try_into() {
        Ok(data) => data,
        Err(_) => {
            let msg = "Input binary must be exactly 64 bytes long.";
            log_message("ERROR", file!(), line!(), msg);
            return Err(rustler::Error::BadArg);
        }
    };

    // Step 2: Open the firmware interface.
    let mut firmware = match Firmware::open() {
        Ok(fw) => {
            log_message("INFO", file!(), line!(), "Firmware opened successfully.");
            fw
        }
        Err(err) => {
            let msg = format!("Failed to open firmware: {:?}", err);
            log_message("ERROR", file!(), line!(), &msg);
            return Ok((atom::error(), msg).encode(env));
        }
    };

    // Step 3: Generate the attestation report.
    let report: AttestationReport = match firmware.get_report(None, Some(unique_data_array), Some(vmpl)) {
        Ok(report) => {
            log_message("INFO", file!(), line!(), "Attestation report generated successfully.");
            report
        }
        Err(err) => {
            let msg = format!("Failed to generate attestation report: {:?}", err);
            log_message("ERROR", file!(), line!(), &msg);
            return Ok((atom::error(), msg).encode(env));
        }
    };

    // Step 4: Serialize the report into a JSON string for output.
    let report_json = match to_string(&report) {
        Ok(json) => {
            log_message("INFO", file!(), line!(), "Attestation report serialized to JSON format.");
            json
        }
        Err(err) => {
            let msg = format!("Failed to serialize attestation report: {:?}", err);
            log_message("ERROR", file!(), line!(), &msg);
            return Ok((atom::error(), msg).encode(env));
        }
    };

    // Step 5: Log the serialized JSON for debugging purposes.
    // log_message(
    //     "INFO",
    //     file!(),
    //     line!(),
    //     &format!("Generated report JSON: {:?}", report_json),
    // );

    // Step 6: Return the result as a tuple with the `ok` atom.
    Ok((ok(), report_json).encode(env))
}
