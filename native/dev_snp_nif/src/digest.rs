use rustler::{Encoder, Env, MapIterator, NifResult, Term};
use rustler::types::atom::{self, ok};
use sev::measurement::snp::{snp_calc_launch_digest, SnpMeasurementArgs};
use sev::measurement::vcpu_types::CpuType;
use sev::measurement::vmsa::{GuestFeatures, VMMType};
use crate::logging::log_message;
use std::path::PathBuf;
use bincode;
use hex;
use std::panic;

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
    log_message("DEBUG", file!(), line!(), "===== Starting launch digest calculation =====");

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

    // Debug: Log all parsed input arguments
    log_message("DEBUG", file!(), line!(), "===== Parsed Input Arguments =====");
    log_message("DEBUG", file!(), line!(), &format!("vcpus: {}", args.vcpus));
    log_message("DEBUG", file!(), line!(), &format!("vcpu_type (u8): {}", args.vcpu_type));
    log_message("DEBUG", file!(), line!(), &format!("vmm_type (u8): {}", args.vmm_type));
    log_message("DEBUG", file!(), line!(), &format!("guest_features (u64): 0x{:016x}", args.guest_features));
    log_message("DEBUG", file!(), line!(), &format!("ovmf_hash_str: {}", args.ovmf_hash_str));
    log_message("DEBUG", file!(), line!(), &format!("kernel_hash: {}", args.kernel_hash));
    log_message("DEBUG", file!(), line!(), &format!("initrd_hash: {}", args.initrd_hash));
    log_message("DEBUG", file!(), line!(), &format!("append_hash: {}", args.append_hash));

    // Step 4: Prepare SnpMeasurementArgs for digest calculation.
    let ovmf_file = "test/OVMF-1.55.fd".to_owned();
    
    // Decode hash strings to bytes for logging
    let kernel_hash_bytes = hex::decode(&args.kernel_hash).unwrap();
    let initrd_hash_bytes = hex::decode(&args.initrd_hash).unwrap();
    let append_hash_bytes = hex::decode(&args.append_hash).unwrap();
    
    // Debug: Log decoded hash bytes
    log_message("DEBUG", file!(), line!(), "===== Decoded Hash Bytes =====");
    log_message("DEBUG", file!(), line!(), &format!("kernel_hash bytes ({}): {}", kernel_hash_bytes.len(), hex::encode(&kernel_hash_bytes)));
    log_message("DEBUG", file!(), line!(), &format!("initrd_hash bytes ({}): {}", initrd_hash_bytes.len(), hex::encode(&initrd_hash_bytes)));
    log_message("DEBUG", file!(), line!(), &format!("append_hash bytes ({}): {}", append_hash_bytes.len(), hex::encode(&append_hash_bytes)));
    
    // Convert to fixed-size arrays
    let kernel_hash_array: [u8; 32] = kernel_hash_bytes.try_into().unwrap();
    let initrd_hash_array: [u8; 32] = initrd_hash_bytes.try_into().unwrap();
    let append_hash_array: [u8; 32] = append_hash_bytes.try_into().unwrap();
    
    // Convert vcpu_type and vmm_type
    let vcpu_type_enum = CpuType::try_from(args.vcpu_type).unwrap();
    let vmm_type_enum = VMMType::try_from(args.vmm_type).unwrap();
    let guest_features_enum = GuestFeatures(args.guest_features);
    
    // Debug: Log enum conversions
    log_message("DEBUG", file!(), line!(), "===== Enum Conversions =====");
    log_message("DEBUG", file!(), line!(), &format!("CpuType: {:?}", vcpu_type_enum));
    log_message("DEBUG", file!(), line!(), &format!("VMMType: {:?}", vmm_type_enum));
    log_message("DEBUG", file!(), line!(), &format!("GuestFeatures: {:?} (0x{:016x})", guest_features_enum, args.guest_features));
    
    let measurement_args = SnpMeasurementArgs {
        ovmf_file: Some(PathBuf::from(ovmf_file)),
        kernel_file: None,
        initrd_file: None,
        append: None,
        vcpus: args.vcpus,
        vcpu_type: vcpu_type_enum,
        vmm_type: Some(vmm_type_enum),
        guest_features: guest_features_enum,
        ovmf_hash_str: Some(args.ovmf_hash_str.as_str()),
        kernel_hash: Some(kernel_hash_array),
        initrd_hash: Some(initrd_hash_array),
        append_hash: Some(append_hash_array),
    };
    
    // Debug: Log SnpMeasurementArgs summary
    log_message("DEBUG", file!(), line!(), "===== SnpMeasurementArgs Summary =====");
    log_message("DEBUG", file!(), line!(), &format!("vcpus: {}", measurement_args.vcpus));
    log_message("DEBUG", file!(), line!(), &format!("vcpu_type: {:?}", measurement_args.vcpu_type));
    log_message("DEBUG", file!(), line!(), &format!("vmm_type: {:?}", measurement_args.vmm_type));
    log_message("DEBUG", file!(), line!(), &format!("guest_features: {:?}", measurement_args.guest_features));
    log_message("DEBUG", file!(), line!(), &format!("ovmf_hash_str: {:?}", measurement_args.ovmf_hash_str));
    log_message("DEBUG", file!(), line!(), &format!("kernel_hash present: {}", measurement_args.kernel_hash.is_some()));
    log_message("DEBUG", file!(), line!(), &format!("initrd_hash present: {}", measurement_args.initrd_hash.is_some()));
    log_message("DEBUG", file!(), line!(), &format!("append_hash present: {}", measurement_args.append_hash.is_some()));

    // Step 5: Compute the launch digest.
    log_message("DEBUG", file!(), line!(), "===== Calling snp_calc_launch_digest =====");
    
    // Log detailed information about GuestFeatures before the call
    log_message("DEBUG", file!(), line!(), "===== Pre-call GuestFeatures Details =====");
    log_message("DEBUG", file!(), line!(), &format!("GuestFeatures raw value: 0x{:016x}", args.guest_features));
    log_message("DEBUG", file!(), line!(), &format!("GuestFeatures bits: {:064b}", args.guest_features));
    log_message("DEBUG", file!(), line!(), &format!("GuestFeatures struct: {:?}", guest_features_enum));
    
    // Log the full measurement_args one more time before the call
    log_message("DEBUG", file!(), line!(), "===== Final measurement_args before snp_calc_launch_digest =====");
    log_message("DEBUG", file!(), line!(), &format!("vcpus: {}", measurement_args.vcpus));
    log_message("DEBUG", file!(), line!(), &format!("vcpu_type: {:?}", measurement_args.vcpu_type));
    log_message("DEBUG", file!(), line!(), &format!("vmm_type: {:?}", measurement_args.vmm_type));
    log_message("DEBUG", file!(), line!(), &format!("guest_features: {:?}", measurement_args.guest_features));
    log_message("DEBUG", file!(), line!(), &format!("guest_features inner value: 0x{:016x}", args.guest_features));
    
    // Wrap the call in a panic handler to catch any panics from the sev crate
    log_message("DEBUG", file!(), line!(), "===== About to call snp_calc_launch_digest (wrapped in panic handler) =====");
    let digest_result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
        snp_calc_launch_digest(measurement_args)
    }));
    
    let digest = match digest_result {
        Ok(Ok(digest)) => {
            log_message("DEBUG", file!(), line!(), "===== Launch digest computed successfully =====");
            // Debug: Log the digest structure
            log_message("DEBUG", file!(), line!(), &format!("Digest struct: {:?}", digest));
            digest
        },
        Ok(Err(err)) => {
            let msg = format!("Failed to compute launch digest: {:?}", err);
            log_message("ERROR", file!(), line!(), &msg);
            return Ok((atom::error(), msg).encode(env));
        },
        Err(panic_info) => {
            let msg = format!("Panic in snp_calc_launch_digest: {:?}", panic_info);
            log_message("ERROR", file!(), line!(), &msg);
            
            // Try to extract panic message if available
            let panic_msg = if let Some(s) = panic_info.downcast_ref::<&str>() {
                format!("Panic message: {}", s)
            } else if let Some(s) = panic_info.downcast_ref::<String>() {
                format!("Panic message: {}", s)
            } else {
                "Unknown panic type".to_string()
            };
            log_message("ERROR", file!(), line!(), &panic_msg);
            
            return Ok((atom::error(), format!("Panic in launch digest computation: {}", panic_msg)).encode(env));
        }
    };

    // Step 6: Serialize the digest.
    log_message("DEBUG", file!(), line!(), "===== Serializing digest with bincode =====");
    let serialized_digest = match bincode::serialize(&digest) {
        Ok(serialized) => {
            log_message("DEBUG", file!(), line!(), &format!("Serialized digest length: {} bytes", serialized.len()));
            log_message("DEBUG", file!(), line!(), &format!("Serialized digest (hex): {}", hex::encode(&serialized)));
            if serialized.len() >= 64 {
                log_message("DEBUG", file!(), line!(), &format!("Serialized digest (first 64 bytes hex): {}", hex::encode(&serialized[..64])));
            } else {
                log_message("DEBUG", file!(), line!(), &format!("Serialized digest (all {} bytes hex): {}", serialized.len(), hex::encode(&serialized)));
            }
            
            // Log bytes in a format that's easy to compare (16 bytes per line)
            log_message("DEBUG", file!(), line!(), "===== Serialized digest bytes (16 bytes per line) =====");
            for (i, chunk) in serialized.chunks(16).enumerate() {
                let hex_str = chunk.iter().map(|b| format!("{:02x}", b)).collect::<Vec<_>>().join(" ");
                log_message("DEBUG", file!(), line!(), &format!("Offset 0x{:04x}: {}", i * 16, hex_str));
            }
            
            serialized
        },
        Err(err) => {
            let msg = format!("Failed to serialize launch digest: {:?}", err);
            log_message("ERROR", file!(), line!(), &msg);
            return Ok((atom::error(), msg).encode(env));
        }
    };

    log_message("DEBUG", file!(), line!(), "===== Launch digest calculation complete =====");

    // Step 7: Return the calculated and serialized digest.
    Ok((ok(), serialized_digest).encode(env))
}
