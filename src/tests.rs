// use std::collections::VecDeque; // replaced with Vec

use rand::random;

use crate::{
    bytecode::{Bytecode, BytecodeProgram},
    reverse::{Context, rev_compute_digest},
    sha256::{compute_digest, maj},
    utils::AbstractVal,
    var::Var,
};

// turn digest back into h
#[test]
fn test_digest() {
    let example_digest: Vec<[u8; 32]> = vec![[
        110, 170, 144, 21, 178, 180, 216, 191, 41, 58, 74, 53, 243, 129, 51, 210, 232, 101, 199,
        41, 125, 108, 224, 153, 246, 128, 76, 65, 253, 249, 246, 15,
    ]];

    for digest in example_digest {
        let reverse = rev_compute_digest(digest);
        let original = compute_digest(reverse);
        assert_eq!(digest, original)
    }
}

// we test bytecode program
#[test]
fn test_maj() {
    let program = BytecodeProgram::from_code(Vec::from([
        Bytecode::Context(Var::A),
        Bytecode::Context(Var::B),
        Bytecode::Context(Var::C),
        Bytecode::Maj,
    ]));

    for _ in 0..10 {
        let a = random();
        let b = random();
        let c = random();
        let ctx = Context::new(vec![
            (Var::A, AbstractVal::U32(a)),
            (Var::B, AbstractVal::U32(b)),
            (Var::C, AbstractVal::U32(c)),
        ]);
        let res = maj(a, b, c);
        assert_eq!(res, program.execute(&ctx).to_u32());
    }
}
