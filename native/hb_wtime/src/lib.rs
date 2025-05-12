use rustler::{Binary, Env, Error, NifMap, NifResult, NifTaggedEnum, NifTuple, NifUnitEnum, NifUntaggedEnum, OwnedBinary};

/// Entry point for the Rustler NIF module.
/// This file defines the available NIF functions and organizes them into modules.

pub mod wasm;

#[rustler::nif]
fn xor_example<'a>(env: Env<'a>, x: Binary<'a>, y: Binary<'a>) -> NifResult<Binary<'a>> {
    let mut x_owned: OwnedBinary = x.to_owned().ok_or(Error::Term(Box::new("no mem")))?;
    let y_owned: OwnedBinary = y.to_owned().ok_or(Error::Term(Box::new("no mem")))?;
    for (x_byte, y_byte) in x_owned.as_mut_slice().iter_mut().zip(y_owned.as_slice()) {
        *x_byte ^= *y_byte;
    }

    // Ownership of `owned`'s data is transferred to `env` on the
    // following line, so no additional heap allocations are incurred.
    Ok(Binary::from_owned(x_owned, env))
}

#[derive(NifMap)]
struct MyMap {
    lhs: i32,
    rhs: i32,
}

#[derive(NifTuple)]
struct MyTuple {
    lhs: i32,
    rhs: i32,
}

#[derive(NifUnitEnum)]
enum UnitEnum {
    FooBar,
    Baz,
}

#[derive(NifTaggedEnum)]
enum TaggedEnum {
    Foo,
    Bar(String),
    Baz{ a: i32, b: i32 },
}

#[derive(NifUntaggedEnum)]
enum UntaggedEnum {
    Foo(u32),
    Bar(String),
}

#[rustler::nif(name = "add")]
fn add_nif(a: i64, b: i64) -> i64 {
    add(a, b)
}

fn add(a: i64, b: i64) -> i64 {
    a + b
}

#[rustler::nif(name = "my_map")]
fn my_map_nif() -> MyMap {
    my_map()
}

#[rustler::nif]
fn my_maps() -> Vec<MyMap> {
    vec![ my_map(), my_map()]
}

fn my_map() -> MyMap {
    MyMap { lhs: 33, rhs: 21 }
}

#[rustler::nif]
fn my_tuple() -> MyTuple {
    MyTuple { lhs: 33, rhs: 21 }
}

#[rustler::nif]
fn unit_enum_echo(unit_enum: UnitEnum) -> UnitEnum {
    unit_enum
}

#[rustler::nif]
fn tagged_enum_echo(tagged_enum: TaggedEnum) -> TaggedEnum {
    tagged_enum
}

#[rustler::nif]
fn untagged_enum_echo(untagged_enum: UntaggedEnum) -> UntaggedEnum {
    untagged_enum
}

rustler::init!(
    "hb_wtime" // Module name as used in Erlang.
);
