// SPDX-License-Identifier: GPL-3.0
// Simple direct doubling kernel

// input as u32 array
@group(0) @binding(0)
var<storage, read> input_bytes: array<u32>;

// output as u32 array
@group(0) @binding(1)
var<storage, read_write> output_bytes: array<u32>;

// a work group of 256 threads
@compute @workgroup_size(256)
// main compute kernel entry point
fn main(@builtin(global_invocation_id) global_id: vec3<u32>) {
    let index = global_id.x;
    
    let array_length = arrayLength(&input_bytes);
    if (index >= array_length) {
        return;
    }
    
    output_bytes[index] = input_bytes[index] * 2u;
}