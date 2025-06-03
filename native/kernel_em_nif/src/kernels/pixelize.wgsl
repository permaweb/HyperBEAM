// SPDX-License-Identifier: GPL-3.0
// Simple image pixelizing kernel
// deployment id (Arweave): -

@group(0) @binding(0)
var<storage, read> input_pixels: array<u32>;

@group(0) @binding(1)
var<storage, read_write> output_pixels: array<u32>;

@group(0) @binding(2)
var<uniform> params: vec4<u32>; // width, height, pixel_size, unused

// Convert RGBA8 packed u32 to vec4<f32>
fn unpack_rgba(packed: u32) -> vec4<f32> {
    let r = f32((packed >> 0u) & 0xFFu) / 255.0;
    let g = f32((packed >> 8u) & 0xFFu) / 255.0;
    let b = f32((packed >> 16u) & 0xFFu) / 255.0;
    let a = f32((packed >> 24u) & 0xFFu) / 255.0;
    return vec4<f32>(r, g, b, a);
}

// Convert vec4<f32> to RGBA8 packed u32
fn pack_rgba(color: vec4<f32>) -> u32 {
    let r = u32(clamp(color.r * 255.0, 0.0, 255.0));
    let g = u32(clamp(color.g * 255.0, 0.0, 255.0));
    let b = u32(clamp(color.b * 255.0, 0.0, 255.0));
    let a = u32(clamp(color.a * 255.0, 0.0, 255.0));
    return r | (g << 8u) | (b << 16u) | (a << 24u);
}

// Sample pixel with bounds checking
fn sample_pixel(x: i32, y: i32, width: i32, height: i32) -> vec4<f32> {
    if (x < 0 || x >= width || y < 0 || y >= height) {
        return vec4<f32>(0.0, 0.0, 0.0, 0.0);
    }
    
    let idx = u32(y * width + x);
    return unpack_rgba(input_pixels[idx]);
}

@compute @workgroup_size(16, 16)
fn main(@builtin(global_invocation_id) global_id: vec3<u32>) {
    let x = i32(global_id.x);
    let y = i32(global_id.y);
    let width = i32(params.x);
    let height = i32(params.y);
    let pixel_size = i32(params.z);

    if (x >= width || y >= height) {
        return;
    }

    // Calculate which pixel block this thread belongs to
    let block_x = (x / pixel_size) * pixel_size;
    let block_y = (y / pixel_size) * pixel_size;
    
    // Calculate the center of the pixel block for sampling
    let center_x = block_x + pixel_size / 2;
    let center_y = block_y + pixel_size / 2;
    
    
    var total_color = vec4<f32>(0.0, 0.0, 0.0, 0.0);
    var sample_count = 0;
    
    for (var dy = 0; dy < pixel_size; dy++) {
        for (var dx = 0; dx < pixel_size; dx++) {
            let sample_x = block_x + dx;
            let sample_y = block_y + dy;
            
            if (sample_x < width && sample_y < height) {
                total_color += sample_pixel(sample_x, sample_y, width, height);
                sample_count++;
            }
        }
    }
    
    let block_color = total_color / f32(sample_count);
    
    
    // Write the block color to the output
    let idx = u32(y * width + x);
    output_pixels[idx] = pack_rgba(block_color);
}
