use rustler::NifUntaggedEnum;

#[derive(Debug, NifUntaggedEnum)]
pub enum NifWasmVal {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}
