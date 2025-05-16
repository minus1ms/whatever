#![feature(box_patterns)]

use reverse::RevProc;

mod constraint;
mod evaluated;
mod helper;
mod map_prototype;
mod mapping;
mod rev_fns;
mod rev_val_holder;
mod reverse;
mod sha256;
#[cfg(test)]
mod tests;
mod utils;

#[cfg(feature = "dhat-heap")]
#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

fn main() {
    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::new_heap();

    // each character is [48; 57] and [97; 102]
    let input = b"38e99de07c873a1d4756e6c1e25a61c83a94e4e270bc5f22dab74ecb462c3420";

    let hash = sha256::sha256(input.clone());
    for byte in &hash {
        print!("{:02x}", byte);
    }
    println!();

    let mut rev = RevProc::new(hash);
    let input = rev.single_run();
    println!("{:?}", input)
}
