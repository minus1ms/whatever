// SHA-256 implementation in Rust without external libraries.
// FIPS 180-4 compliant implementation.

use core::time;

const K: [u32; 64] = [
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
];

const H0_INIT: [u32; 8] = [
    0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19,
];

#[inline(always)]
fn rotr(x: u32, n: u32) -> u32 {
    x.rotate_right(n)
}

#[inline(always)]
fn shr(x: u32, n: u32) -> u32 {
    x >> n
}

#[inline(always)]
pub fn ch(x: u32, y: u32, z: u32) -> u32 {
    let res = x & y;
    let val = !x & z;
    let res = res ^ val;
    res
}

#[inline(always)]
pub fn maj(x: u32, y: u32, z: u32) -> u32 {
    let res = x & y;
    let a = x & z;
    let res = res ^ a;
    let b = y & z;
    let res = res ^ b;
    res
}

#[inline(always)]
pub fn big_sigma0(x: u32) -> u32 {
    let res = rotr(x, 2);
    let val = rotr(x, 13);
    let res = res ^ val;
    let val = rotr(x, 22);
    let res = res ^ val;
    res
}

#[inline(always)]
pub fn big_sigma1(x: u32) -> u32 {
    rotr(x, 6) ^ rotr(x, 11) ^ rotr(x, 25)
}

#[inline(always)]
pub fn small_sigma0(x: u32) -> u32 {
    rotr(x, 7) ^ rotr(x, 18) ^ shr(x, 3)
}

#[inline(always)]
pub fn small_sigma1(x: u32) -> u32 {
    rotr(x, 17) ^ rotr(x, 19) ^ shr(x, 10)
}

/// each u8 is [48; 57] and [97; 102]
pub fn sha256(message: [u8; 64]) -> [u8; 32] {
    let mut h: [u32; 8] = H0_INIT;
    let schedule = message_schedule(message);
    compression_loop(schedule, &mut h);
    padding_compression_loop(&mut h);
    compute_digest(h)
}

pub fn compute_digest(h: [u32; 8]) -> [u8; 32] {
    let mut digest = [0u8; 32];
    for (i, word) in h.iter().enumerate() {
        digest[i * 4] = (word >> 24) as u8;
        digest[i * 4 + 1] = (word >> 16) as u8;
        digest[i * 4 + 2] = (word >> 8) as u8;
        digest[i * 4 + 3] = *word as u8;
    }
    digest
}

// each u8 is [48; 57] and [97; 102]
fn message_schedule(chunk: [u8; 64]) -> [u32; 64] {
    let mut w = [0u32; 64];
    // Prepare message schedule
    for i in 0..16 {
        w[i] = ((chunk[i * 4] as u32) << 24)
            | ((chunk[i * 4 + 1] as u32) << 16)
            | ((chunk[i * 4 + 2] as u32) << 8)
            | (chunk[i * 4 + 3] as u32);
    }
    for i in 16..64 {
        // possible indexes: [14; 61]
        w[i] = small_sigma1(w[i - 2])
            // possible indexes: [9; 56]
            .wrapping_add(w[i - 7])
            // possible indexes: [1; 48]
            .wrapping_add(small_sigma0(w[i - 15]))
            // possible indexes: [0; 47]
            .wrapping_add(w[i - 16]);
    }
    w
}

fn compression_loop(w: [u32; 64], h: &mut [u32; 8]) {
    // Initialize working variables
    let (mut a, mut b, mut c, mut d, mut e, mut f, mut g, mut h_temp) =
        (h[0], h[1], h[2], h[3], h[4], h[5], h[6], h[7]);

    // Compression loop
    for i in 0..64 {
        let big_sigma = big_sigma1(e);
        let t1 = h_temp.wrapping_add(big_sigma);
        let ch = ch(e, f, g);
        let t1 = t1.wrapping_add(ch);
        let t1 = t1.wrapping_add(K[i]);
        let t1 = t1.wrapping_add(w[i]);
        let t2 = big_sigma0(a);
        let maj = maj(a, b, c);
        let t2 = t2.wrapping_add(maj);
        h_temp = g;
        g = f;
        f = e;
        e = d.wrapping_add(t1);
        d = c;
        c = b;
        b = a;
        a = t1.wrapping_add(t2);
    }

    h[0] = h[0].wrapping_add(a);
    h[1] = h[1].wrapping_add(b);
    h[2] = h[2].wrapping_add(c);
    h[3] = h[3].wrapping_add(d);
    h[4] = h[4].wrapping_add(e);
    h[5] = h[5].wrapping_add(f);
    h[6] = h[6].wrapping_add(g);
    h[7] = h[7].wrapping_add(h_temp);
}

fn padding_compression_loop(h: &mut [u32; 8]) {
    print_hex(h.clone());
    let w = [
        0x80000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000200, 0x80000000, 0x01400000, 0x00205000, 0x00005088, 0x22000800,
        0x22550014, 0x05089742, 0xa0000020, 0x5a880000, 0x005c9400, 0x0016d49d, 0xfa801f00,
        0xd33225d0, 0x11675959, 0xf6e6bfda, 0xb30c1549, 0x08b2b050, 0x9d7c4c27, 0x0ce2a393,
        0x88e6e1ea, 0xa52b4335, 0x67a16f49, 0xd732016f, 0x4eeb2e91, 0x5dbf55e5, 0x8eee2335,
        0xe2bc5ec2, 0xa83f4394, 0x45ad78f7, 0x36f3d0cd, 0xd99c05e8, 0xb0511dc7, 0x69bc7ac4,
        0xbd11375b, 0xe3ba71e5, 0x3b209ff2, 0x18feee17, 0xe25ad9e7, 0x13375046, 0x0515089d,
        0x4f0d0f04, 0x2627484e, 0x310128d2, 0xc668b434, 0x420841cc, 0x62d311b8, 0xe59ba771,
        0x85a7a484,
    ];

    // Initialize working variables
    let (mut a, mut b, mut c, mut d, mut e, mut f, mut g, mut h_temp) =
        (h[0], h[1], h[2], h[3], h[4], h[5], h[6], h[7]);

    // asserts are for constraints
    // Compression loop
    for i in 0usize..64 {
        let big_sigma = big_sigma1(e);
        let t1_1 = h_temp.wrapping_add(big_sigma);
        let ch_ = ch(e, f, g);
        let t1_2 = t1_1.wrapping_add(ch_);
        let t1_3 = t1_2.wrapping_add(K[i]);
        let t1_4 = t1_3.wrapping_add(w[i]);
        let t2_1 = big_sigma0(a);
        let maj_ = maj(a, b, c);
        let t2_2 = t2_1.wrapping_add(maj_);
        h_temp = g;
        // if i == 23 {
        //     let t1 = t1_4;
        //    println!("{} {}", h_temp, {
        //        todo!()
        //    });
        //    todo!();
        // }
        // old_h_temp equation:
        // t1_4
        // .wrapping_sub(w[i])
        // .wrapping_sub(K[i])
        // .wrapping_sub(ch(e, f, h_temp))
        // .wrapping_sub(big_sigma1(e))
        g = f;
        f = e;
        e = d.wrapping_add(t1_4);
        d = c;
        c = b;
        b = a;
        a = t1_4.wrapping_add(t2_2);
        // let t2 = big_sigma0(b).wrapping_add(maj(b, c, d));
        // get t1 from above a = operation
    }

    h[0] = h[0].wrapping_add(a);
    h[1] = h[1].wrapping_add(b);
    h[2] = h[2].wrapping_add(c);
    h[3] = h[3].wrapping_add(d);
    h[4] = h[4].wrapping_add(e);
    h[5] = h[5].wrapping_add(f);
    h[6] = h[6].wrapping_add(g);
    h[7] = h[7].wrapping_add(h_temp);
}

pub fn print_hex<const S: usize>(arr: [u32; S]) {
    print!("    [");
    for (i, &value) in arr.iter().enumerate() {
        if i % 4 == 0 && i > 0 {
            println!();
            print!("    ");
        }
        print!("0x{:08x}", value);
        if i < arr.len() - 1 {
            print!(", ");
        }
    }
    println!();
    println!("    ]");
}
