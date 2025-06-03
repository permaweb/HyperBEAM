pub mod core;
#[cfg(test)]
pub mod tests;
use crate::core::arweave::retrieve_kernel_src;
use crate::core::execution_machine::KernelExecutor;
use rustler::NifResult;

mod atoms {
    rustler::atoms! {
        ok,
    }
}

#[rustler::nif]
fn hello() -> NifResult<String> {
    Ok("Hello world!".to_string())
}

#[rustler::nif(schedule = "DirtyCpu")]
fn execute_kernel(
    kernel_id: String,
    input_data: rustler::Binary,
    output_size_hint: u64,
) -> NifResult<Vec<u8>> {
    let kernel_src = retrieve_kernel_src(&kernel_id).unwrap();
    let kem = pollster::block_on(KernelExecutor::new());
    let result = kem.execute_kernel_default(&kernel_src, input_data.as_slice(), Some(output_size_hint));
    Ok(result)
}

#[rustler::nif(schedule = "DirtyCpu")]
fn execute_kernel_with_params(
    kernel_id: String,
    input_data: rustler::Binary,
    params: rustler::Binary,
) -> NifResult<Vec<u8>> {
    let kernel_src = retrieve_kernel_src(&kernel_id).unwrap();
    let kem = pollster::block_on(KernelExecutor::new());
    let result = kem.execute_kernel_with_uniform_default(&kernel_src, input_data.as_slice(), params.as_slice());
    Ok(result)
}

#[rustler::nif(schedule = "DirtyCpu")]
fn adapter_info() -> NifResult<String> {
    let adapter = pollster::block_on(KernelExecutor::get_adapter_info());
    Ok(adapter)
}

rustler::init!("kem_nif", [hello, execute_kernel, execute_kernel_with_params, adapter_info]);
