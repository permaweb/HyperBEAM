use rustler::{Encoder, Env, MapIterator, NifResult, Term};
use rustler::types::atom::{self, ok};
use sev::measurement::snp::{snp_calc_launch_digest, SnpMeasurementArgs};
use sev::measurement::vcpu_types::CpuType;
use sev::measurement::vmsa::{GuestFeatures, VMMType};
use crate::logging::log_message;
use std::path::PathBuf;
use bincode;

/// Struct to hold launch digest arguments passed from Erlang
#[derive(Debug)]
struct LaunchDigestArgs {
    vcpus: Option<u32>,
    vcpu_type: Option<u8>,
    vmm_type: Option<u8>,
    guest_features: Option<u64>,
    firmware: Option<[u8; 48]>,
    kernel_hash: Option<[u8; 32]>,
    initrd_hash: Option<[u8; 32]>,
    append_hash: Option<[u8; 32]>,
}

/// Computes the launch digest using the input arguments provided as an Erlang map.
///
/// # Arguments
/// * `env` - The Rustler environment, used to encode the return value.
/// * `input_map` - An Erlang map containing the input parameters required for the calculation.
///
/// # Returns
/// A tuple containing an `ok` atom and the calculated and serialized launch digest.
/// If the input is invalid or an error occurs during calculation, an error is returned.
///
/// # Expected Input Map Keys:
/// - `"vcpus"`: Number of virtual CPUs (u32).
/// - `"vcpu_type"`: Type of the virtual CPU (u8).
/// - `"vmm_type"`: Type of the Virtual Machine Monitor (u8).
/// - `"guest_features"`: Features of the guest (u64).
/// - `"ovmf_hash_str"`: Hash of the OVMF firmware (String).
/// - `"kernel_hash"`: Hash of the kernel (String).
/// - `"initrd_hash"`: Hash of the initrd (String).
/// - `"append_hash"`: Hash of the kernel command line arguments (String).
///
/// # Example
/// ```erlang
/// {ok, LaunchDigest} = dev_snp_nif:compute_launch_digest(InputMap).
/// ```
#[rustler::nif]
pub fn compute_launch_digest<'a>(env: Env<'a>, input_map: Term<'a>) -> NifResult<Term<'a>> {
    //log_message("INFO", file!(), line!(), "Starting launch digest calculation...");

    // Step 1: Validate that the input is a map.
    if !input_map.is_map() {
        log_message("ERROR", file!(), line!(), "Provided input is not a map.");
        return Ok((atom::error(), "Input must be a map").encode(env));
    }

    // Step 2: Helper functions to decode and validate input fields.
    fn decode_string(value: Term) -> Result<String, String> {
        match value.get_type() {
            rustler::TermType::List => {
                let list: Vec<u8> = value
                    .decode()
                    .map_err(|err| format!("invalid list-based string: {err:?}"))?;
                String::from_utf8(list).map_err(|err| format!("invalid UTF-8 string: {err:?}"))
            }
            _ => value
                .decode()
                .map_err(|err| format!("invalid string value: {err:?}")),
        }
    }

    fn decode_u8(name: &'static str, value: Term) -> Result<u8, String> {
        let raw = value
            .decode::<u64>()
            .map_err(|err| format!("invalid {name} field: {err:?}"))?;
        u8::try_from(raw).map_err(|_| format!("{name} must be between 0 and {}", u8::MAX))
    }

    fn decode_u32(name: &'static str, value: Term) -> Result<u32, String> {
        let raw = value
            .decode::<u64>()
            .map_err(|err| format!("invalid {name} field: {err:?}"))?;
        u32::try_from(raw).map_err(|_| format!("{name} must be between 0 and {}", u32::MAX))
    }

    fn decode_hex<const N: usize>(name: &'static str, value: Term) -> Result<[u8; N], String> {
        let text = decode_string(value)?;
        let bytes = hex::decode(&text)
            .map_err(|err| format!("invalid hex value in {name}: {err:?}"))?;
        let bytes_len = bytes.len();
        bytes
            .try_into()
            .map_err(|_| format!("{name} must decode to exactly {N} bytes, got {bytes_len}"))
    }

    fn require_field<T>(name: &'static str, value: Option<T>) -> Result<T, String> {
        value.ok_or_else(|| format!("missing required field: {name}"))
    }

    // Step 3: Parse input map into LaunchDigestArgs.
    let mut args = LaunchDigestArgs {
        vcpus: None,
        vcpu_type: None,
        vmm_type: None,
        guest_features: None,
        firmware: None,
        kernel_hash: None,
        initrd_hash: None,
        append_hash: None,
    };

    let map_iter = match MapIterator::new(input_map) {
        Some(iter) => iter,
        None => {
            return Ok((atom::error(), "Input must be a map").encode(env));
        }
    };
    for (key, value) in map_iter {
        let key_str = match key.atom_to_string() {
            Ok(k) => k.to_string(),
            Err(_) => {
                let msg = "Expected atom keys in input map";
                return Ok((atom::error(), msg).encode(env));
            }
        };
        match key_str.as_str() {
            "vcpus" => {
                args.vcpus = match decode_u32("vcpus", value) {
                    Ok(v) => Some(v),
                    Err(msg) => return Ok((atom::error(), msg).encode(env)),
                };
            }
            "vcpu_type" => {
                args.vcpu_type = match decode_u8("vcpu_type", value) {
                    Ok(v) => Some(v),
                    Err(msg) => return Ok((atom::error(), msg).encode(env)),
                };
            }
            "vmm_type" => {
                args.vmm_type = match decode_u8("vmm_type", value) {
                    Ok(v) => Some(v),
                    Err(msg) => return Ok((atom::error(), msg).encode(env)),
                };
            }
            "guest_features" => {
                args.guest_features = match value.decode::<u64>() {
                    Ok(v) => Some(v),
                    Err(err) => {
                        return Ok((atom::error(), format!("invalid guest_features field: {err:?}")).encode(env));
                    }
                };
            }
            "firmware" => {
                args.firmware = match decode_hex("firmware", value) {
                    Ok(v) => Some(v),
                    Err(msg) => return Ok((atom::error(), msg).encode(env)),
                };
            }
            "kernel" => {
                args.kernel_hash = match decode_hex("kernel", value) {
                    Ok(v) => Some(v),
                    Err(msg) => return Ok((atom::error(), msg).encode(env)),
                };
            }
            "initrd" => {
                args.initrd_hash = match decode_hex("initrd", value) {
                    Ok(v) => Some(v),
                    Err(msg) => return Ok((atom::error(), msg).encode(env)),
                };
            }
            "append" => {
                args.append_hash = match decode_hex("append", value) {
                    Ok(v) => Some(v),
                    Err(msg) => return Ok((atom::error(), msg).encode(env)),
                };
            }
            _ => log_message("WARN", file!(), line!(), &format!("Unexpected key: {}", key_str)),
        }
    }

    let vcpus = match require_field("vcpus", args.vcpus) {
        Ok(value) => value,
        Err(msg) => return Ok((atom::error(), msg).encode(env)),
    };
    let vcpu_type = match require_field("vcpu_type", args.vcpu_type) {
        Ok(value) => value,
        Err(msg) => return Ok((atom::error(), msg).encode(env)),
    };
    let vmm_type = match require_field("vmm_type", args.vmm_type) {
        Ok(value) => value,
        Err(msg) => return Ok((atom::error(), msg).encode(env)),
    };
    let guest_features = match require_field("guest_features", args.guest_features) {
        Ok(value) => value,
        Err(msg) => return Ok((atom::error(), msg).encode(env)),
    };
    let firmware = match require_field("firmware", args.firmware) {
        Ok(value) => value,
        Err(msg) => return Ok((atom::error(), msg).encode(env)),
    };
    let kernel_hash = match require_field("kernel", args.kernel_hash) {
        Ok(value) => value,
        Err(msg) => return Ok((atom::error(), msg).encode(env)),
    };
    let initrd_hash = match require_field("initrd", args.initrd_hash) {
        Ok(value) => value,
        Err(msg) => return Ok((atom::error(), msg).encode(env)),
    };
    let append_hash = match require_field("append", args.append_hash) {
        Ok(value) => value,
        Err(msg) => return Ok((atom::error(), msg).encode(env)),
    };

    //log_message("INFO", file!(), line!(), &format!("Parsed arguments: {:?}", args));

    // Step 4: Prepare SnpMeasurementArgs for digest calculation.
    let ovmf_file = "test/OVMF-1.55.fd".to_owned();
    let firmware_hash = hex::encode(firmware);
    let vcpu_type = match CpuType::try_from(vcpu_type) {
        Ok(v) => v,
        Err(_) => return Ok((atom::error(), "Invalid vcpu_type").encode(env)),
    };
    let vmm_type = match VMMType::try_from(vmm_type) {
        Ok(v) => Some(v),
        Err(_) => return Ok((atom::error(), "Invalid vmm_type").encode(env)),
    };

    let measurement_args = SnpMeasurementArgs {
        ovmf_file: Some(PathBuf::from(ovmf_file)),
        kernel_file: None,
        initrd_file: None,
        append: None,

        vcpus,
        vcpu_type,
        vmm_type,
        guest_features: GuestFeatures(guest_features),
        ovmf_hash_str: Some(firmware_hash.as_str()),
        kernel_hash: Some(kernel_hash),
        initrd_hash: Some(initrd_hash),
        append_hash: Some(append_hash),
    };

    // Step 5: Compute the launch digest.
    let digest = match snp_calc_launch_digest(measurement_args) {
        Ok(digest) => digest,
        Err(err) => {
            let msg = format!("Failed to compute launch digest: {:?}", err);
            log_message("ERROR", file!(), line!(), &msg);
            return Ok((atom::error(), msg).encode(env));
        }
    };

    // Step 6: Serialize the digest.
    let serialized_digest = match bincode::serialize(&digest) {
        Ok(serialized) => serialized,
        Err(err) => {
            let msg = format!("Failed to serialize launch digest: {:?}", err);
            log_message("ERROR", file!(), line!(), &msg);
            return Ok((atom::error(), msg).encode(env));
        }
    };

    //log_message(
    //    "INFO",
    //    file!(),
    //    line!(),
    //    "Launch digest successfully computed and serialized.",
    //);

    // Step 7: Return the calculated and serialized digest.
    Ok((ok(), serialized_digest).encode(env))
}
