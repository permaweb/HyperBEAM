pub mod core;
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
fn execute_kernel(kernel_id: String, input_data: Vec<u8>, output_size_hint: Option<u64>) -> NifResult<Vec<u8>> {
    let kernel_src = retrieve_kernel_src(&kernel_id).unwrap();
    let kem =  pollster::block_on(KernelExecutor::new());
    let result = kem.execute_kernel_default(&kernel_src, &input_data, output_size_hint);
    Ok(result)
}

rustler::init!("kem_nif", [hello, execute_kernel]);
