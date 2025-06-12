pub mod core;
#[cfg(test)]
pub mod tests;
pub mod utils;
use crate::core::arweave::retrieve_kernel_src;
use crate::core::execution_machine::{KernelExecutor, KernelExecutorOpts};
use crate::utils::load0::upload_to_load0;
use image::ImageBuffer;
use image::ImageFormat;
use image::Rgba;
use image::codecs::png;
use rustler::NifResult;
use std::io::Cursor;

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
    let kem = pollster::block_on(KernelExecutor::new(KernelExecutorOpts::default()));
    let result =
        kem.execute_kernel_default(&kernel_src, input_data.as_slice(), Some(output_size_hint));
    Ok(result)
}

// shortcut hack to upload data to load0
// purpose: berlin demo
#[rustler::nif(schedule = "DirtyCpu")]
fn execute_kernel_with_params(
    kernel_id: String,
    input_data: rustler::Binary,
    params: rustler::Binary,
) -> NifResult<String> {
    let img = image::load_from_memory(&input_data).unwrap();

    let rgba_img = img.to_rgba8();
    let (width, height) = rgba_img.dimensions();
    let pixel_bytes = rgba_img.into_raw();

    let pixel_size = if params.len() >= 12 {
        u32::from_le_bytes([params[8], params[9], params[10], params[11]])
    } else {
        50 // default pixel size
    };
    let kernel_src = retrieve_kernel_src(&kernel_id).unwrap();
    let kem = pollster::block_on(KernelExecutor::new(KernelExecutorOpts::default()));
    let result = kem.execute_kernel_with_uniform_default(
        &kernel_src,
        pixel_bytes.as_slice(),
        params.as_slice(),
    );

    let output_data = &result[0..pixel_bytes.len()];

    let output_img =
        ImageBuffer::<Rgba<u8>, Vec<u8>>::from_raw(width, height, output_data.to_vec()).unwrap();

    let mut png_bytes = Vec::new();
    output_img
        .write_to(&mut Cursor::new(&mut png_bytes), ImageFormat::Png)
        .unwrap();
    let load0_hash = upload_to_load0(png_bytes, "image/png").unwrap();
    Ok(load0_hash)
}

#[rustler::nif(schedule = "DirtyCpu")]
fn adapter_info() -> NifResult<String> {
    let adapter = pollster::block_on(KernelExecutor::get_adapter_info());
    Ok(adapter)
}

rustler::init!(
    "kem_nif",
    [
        hello,
        execute_kernel,
        execute_kernel_with_params,
        adapter_info
    ]
);
