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
    vcpus: u32,
    vcpu_type: u8,
    vmm_type: u8,
    guest_features: u64,
    ovmf_hash_str: String,
    kernel_hash: String,
    initrd_hash: String,
    append_hash: String,
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
        return Err(rustler::Error::BadArg);
    }

    // Step 2: Helper function to decode string values from the map.
    fn decode_string(value: Term) -> NifResult<String> {
        match value.get_type() {
            rustler::TermType::List => {
                let list: Vec<u8> = value.decode()?;
                String::from_utf8(list).map_err(|_| rustler::Error::BadArg)
            }
            _ => value.decode(),
        }
    }

    // Step 3: Parse input map into LaunchDigestArgs.
    let mut args = LaunchDigestArgs {
        vcpus: 0,
        vcpu_type: 0,
        vmm_type: 0,
        guest_features: 0,
        ovmf_hash_str: String::new(),
        kernel_hash: String::new(),
        initrd_hash: String::new(),
        append_hash: String::new(),
    };

    let map_iter = MapIterator::new(input_map).unwrap();
    for (key, value) in map_iter {
        let key_str = key.atom_to_string()?.to_string();
        match key_str.as_str() {
            "vcpus" => args.vcpus = value.decode()?,
            "vcpu_type" => args.vcpu_type = value.decode()?,
            "vmm_type" => args.vmm_type = value.decode()?,
            "guest_features" => args.guest_features = value.decode()?,
            "firmware" => args.ovmf_hash_str = decode_string(value)?,
            "kernel" => args.kernel_hash = decode_string(value)?,
            "initrd" => args.initrd_hash = decode_string(value)?,
            "append" => args.append_hash = decode_string(value)?,
            _ => log_message("WARN", file!(), line!(), &format!("Unexpected key: {}", key_str)),
        }
    }

    //log_message("INFO", file!(), line!(), &format!("Parsed arguments: {:?}", args));

    // Step 4: Prepare SnpMeasurementArgs for digest calculation.
    let ovmf_file = "test/OVMF-1.55.fd".to_owned();
    let measurement_args = SnpMeasurementArgs {
        ovmf_file: Some(PathBuf::from(ovmf_file)),
        kernel_file: None,
        initrd_file: None,
        append: None,
        // vcpus: args.vcpus,
        // vcpu_type: CpuType::try_from(args.vcpu_type).unwrap(),
        // vmm_type: Some(VMMType::try_from(args.vmm_type).unwrap()),
        // guest_features: GuestFeatures(args.guest_features),
		vcpus: 32,
        vcpu_type: CpuType::EpycV4,
        vmm_type: Some(VMMType::QEMU),
        guest_features: GuestFeatures(0x1),
        ovmf_hash_str: Some(args.ovmf_hash_str.as_str()),
        kernel_hash: Some(hex::decode(args.kernel_hash).unwrap().try_into().unwrap()),
        initrd_hash: Some(hex::decode(args.initrd_hash).unwrap().try_into().unwrap()),
        append_hash: Some(hex::decode(args.append_hash).unwrap().try_into().unwrap()),
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
