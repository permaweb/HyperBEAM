// SPDX-License-Identifier: GPL-3.0
// Full-chaos glitch kernel — now with frequent massive block wreckage -- demo'd on Arweave Day Berlin 2025
// kernel id: GBPZsoX_NugmvImUVUn0loXeJbAb-U6QVgOWRWuYVIE

@group(0) @binding(0)
var<storage, read> input_pixels: array<u32>;

@group(0) @binding(1)
var<storage, read_write> output_pixels: array<u32>;

@group(0) @binding(2)
var<uniform> params: vec4<u32>; // width, height, base_block_size, unused

fn unpack_rgba(packed: u32) -> vec4<f32> {
    let r = f32((packed >> 0u) & 0xFFu) / 255.0;
    let g = f32((packed >> 8u) & 0xFFu) / 255.0;
    let b = f32((packed >> 16u) & 0xFFu) / 255.0;
    let a = f32((packed >> 24u) & 0xFFu) / 255.0;
    return vec4<f32>(r, g, b, a);
}

fn pack_rgba(color: vec4<f32>) -> u32 {
    let r = u32(clamp(color.r * 255.0, 0.0, 255.0));
    let g = u32(clamp(color.g * 255.0, 0.0, 255.0));
    let b = u32(clamp(color.b * 255.0, 0.0, 255.0));
    let a = u32(clamp(color.a * 255.0, 0.0, 255.0));
    return r | (g << 8u) | (b << 16u) | (a << 24u);
}

fn sample_pixel(x: i32, y: i32, width: i32, height: i32) -> vec4<f32> {
    if (x < 0 || x >= width || y < 0 || y >= height) {
        return vec4<f32>(0.0, 0.0, 0.0, 1.0);
    }
    let idx = u32(y * width + x);
    return unpack_rgba(input_pixels[idx]);
}

fn random(x: u32, y: u32) -> f32 {
    let seed = (x * 3266489917u) ^ (y * 668265263u) ^ 0xdeadbeefu;
    let hashed = (seed ^ (seed >> 13u)) * 1274126177u;
    return f32((hashed >> 8u) & 0xFFFFFFu) / f32(0xFFFFFFu);
}

@compute @workgroup_size(16, 16)
fn main(@builtin(global_invocation_id) global_id: vec3<u32>) {
    let x = i32(global_id.x);
    let y = i32(global_id.y);
    let width = i32(params.x);
    let height = i32(params.y);
    let base_block_size = i32(params.z);

    if (x >= width || y >= height) {
        return;
    }

    let col_original = sample_pixel(x, y, width, height);
    var col = col_original;

    // === BIG MOSAIC REARRANGE MODE ===
    let mosaic_chance = random(global_id.x ^ 12121u, global_id.y ^ 34343u);
    if (mosaic_chance < 0.3) {
        let tile_cols = 1 + i32(random(global_id.x ^ 707u, global_id.y ^ 808u) * 4.0);
        let tile_rows = 1 + i32(random(global_id.y ^ 909u, global_id.x ^ 606u) * 4.0);

        let tile_w = max(width / tile_cols, 1);
        let tile_h = max(height / tile_rows, 1);

        let tile_x = (x / tile_w);
        let tile_y = (y / tile_h);

        let new_tx = i32(random(u32(tile_x) ^ 333u, u32(tile_y) ^ 999u) * f32(tile_cols));
        let new_ty = i32(random(u32(tile_y) ^ 222u, u32(tile_x) ^ 888u) * f32(tile_rows));

        let offset_x = x % tile_w;
        let offset_y = y % tile_h;

        let sx = clamp(new_tx * tile_w + offset_x, 0, width - 1);
        let sy = clamp(new_ty * tile_h + offset_y, 0, height - 1);

        col = sample_pixel(sx, sy, width, height);
    } else {
        // === BLOCK-BASED GLITCH ===
        let region_id_x = (x + i32(random(global_id.x, global_id.y) * f32(base_block_size))) / base_block_size;
        let region_id_y = (y + i32(random(global_id.y ^ 420u, global_id.x ^ 1337u) * f32(base_block_size))) / base_block_size;

        let region_seed_x = u32(region_id_x);
        let region_seed_y = u32(region_id_y);

        let block_x = region_id_x * base_block_size;
        let block_y = region_id_y * base_block_size;

        let max_w = max(base_block_size * 12, width / 3);
        let max_h = max(base_block_size * 12, height / 3);

        let region_w = 8 + i32(random(region_seed_x ^ 555u, region_seed_y ^ 999u) * f32(max_w));
        let region_h = 8 + i32(random(region_seed_y ^ 888u, region_seed_x ^ 444u) * f32(max_h));

        let local_x = x - block_x;
        let local_y = y - block_y;

        let offset_x = i32((random(region_seed_x ^ 123u, region_seed_y ^ 321u) - 0.5) * f32(width));
        let offset_y = i32((random(region_seed_y ^ 456u, region_seed_x ^ 654u) - 0.5) * f32(height));

        let sample_x = clamp(block_x + local_x + offset_x, 0, width - 1);
        let sample_y = clamp(block_y + local_y + offset_y, 0, height - 1);

        let glitch_chance = random(region_seed_x ^ 1u, region_seed_y ^ 2u);
        if (glitch_chance > 0.5) {
            var warped = sample_pixel(sample_x, sample_y, width, height);
            let mode = random(region_seed_x ^ 991u, region_seed_y ^ 117u);
            if (mode > 0.95) {
                warped = vec4<f32>(warped.b, warped.r, warped.g, warped.a);
            } else if (mode > 0.8) {
                warped = vec4<f32>(warped.r, warped.b, warped.g, warped.a);
            } else if (mode > 0.6) {
                warped = vec4<f32>(warped.g, warped.r, warped.b, warped.a);
            }
            col = warped;
        }

        let scramble_chance = random(global_id.x * 17u, global_id.y * 31u);
        if (scramble_chance > 0.995) {
            let dx = i32(random(global_id.x ^ 333u, global_id.y ^ 999u) * f32(width));
            let dy = i32(random(global_id.y ^ 444u, global_id.x ^ 888u) * f32(height));
            col = sample_pixel(dx, dy, width, height);

            let m = random(global_id.x ^ 4321u, global_id.y ^ 8765u);
            if (m > 0.9) {
                col = vec4<f32>(col.r, col.a, col.b, col.g);
            } else if (m > 0.7) {
                col = vec4<f32>(col.g, col.r, col.a, col.b);
            }
        }

        // === BIG BLOCKY GLITCH MODE (More frequent and destructive) ===
        let big_block_size = 200;
        let big_block_x = (x / big_block_size) * big_block_size;
        let big_block_y = (y / big_block_size) * big_block_size;

        let big_chance = random(u32(big_block_x) ^ 10001u, u32(big_block_y) ^ 20002u);
        if (big_chance > 0.7) {
            let dx = i32(random(u32(big_block_x) ^ 1111u, u32(big_block_y) ^ 2222u) * f32(width));
            let dy = i32(random(u32(big_block_y) ^ 3333u, u32(big_block_x) ^ 4444u) * f32(height));

            let sx = clamp(dx + (x % big_block_size), 0, width - 1);
            let sy = clamp(dy + (y % big_block_size), 0, height - 1);
            col = sample_pixel(sx, sy, width, height);
        }

        // === MEGA BLOCK WIPE (devastating) ===
        let mega_block_size = 256;
        let mx = (x / mega_block_size) * mega_block_size;
        let my = (y / mega_block_size) * mega_block_size;

        let mega_chance = random(u32(mx) ^ 777u, u32(my) ^ 888u);
        if (mega_chance > 0.6) {
            let src_dx = i32(random(u32(mx) ^ 9999u, u32(my) ^ 1234u) * f32(width));
            let src_dy = i32(random(u32(my) ^ 5678u, u32(mx) ^ 4321u) * f32(height));

            let sx = clamp(src_dx + (x % mega_block_size), 0, width - 1);
            let sy = clamp(src_dy + (y % mega_block_size), 0, height - 1);
            col = sample_pixel(sx, sy, width, height);
        }
    }

    let invert_chance = random(global_id.x ^ 8888u, global_id.y ^ 7777u);
    if (invert_chance > 0.995) {
        col = vec4<f32>(1.0 - col.r, 1.0 - col.g, 1.0 - col.b, col.a);
    }

    let bleed = random(global_id.x ^ 5555u, global_id.y ^ 4444u);
    if (bleed > 0.998) {
        let n1 = sample_pixel(x + 1, y, width, height);
        let n2 = sample_pixel(x, y + 1, width, height);
        let ghost = (n1.rgb + n2.rgb) * 0.5;
        col = vec4<f32>(mix(col.rgb, ghost, 0.3), col.a);
    }

    let idx = u32(y * width + x);
    output_pixels[idx] = pack_rgba(col);
}
