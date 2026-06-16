use rustler::{Encoder, Env, NifResult, Term};
use rustler::types::atom::ok;
use sev::firmware::guest::Firmware;
use crate::logging::log_message;

/// Checks if Secure Nested Paging (SNP) is supported by the system.
///
/// # Arguments
/// * `env` - The Rustler environment, used to encode the return value.
///
/// # Returns
/// A tuple containing an `ok` atom and a boolean value:
/// - `true` if the firmware indicates that SNP is supported.
/// - `false` if SNP is not supported or if the firmware cannot be accessed.
///
/// # Example
/// ```erlang
/// {ok, Supported} = dev_snp_nif:check_snp_support().
/// ```
#[rustler::nif]
pub fn check_snp_support<'a>(env: Env<'a>) -> NifResult<Term<'a>> {
    //log_message("INFO", file!(), line!(), "Checking SNP support...");

    // Step 1: Attempt to open the firmware interface.
    // If the firmware is accessible, SNP is supported; otherwise, it is not.
    let is_supported = match Firmware::open() {
        Ok(_) => {
            //log_message("INFO", file!(), line!(), "SNP is supported.");
            true // SNP is supported.
        }
        Err(_) => {
            // log_message(
            //     "ERROR",
            //     file!(),
            //     line!(),
            //     "Failed to open firmware. SNP is not supported.",
            // );
            false // SNP is not supported.
        }
    };

    // Step 2: Return the result as a tuple with the `ok` atom and the boolean value.
    Ok((ok(), is_supported).encode(env))
}
