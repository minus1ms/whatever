use std::fmt::Debug;

use hashbrown::HashMap;

use crate::{
    bytecode::{Bytecode, BytecodeProgram},
    constraint::{Constraint, ConstraintId, Constraints, ConstraintsHolder, ContextVal, RevVal},
    rev_fns::{
        ChConstraints, MajConstraints, WrappingAddConstraints, rev_assignment, rev_big_sigma0,
        rev_big_sigma1, rev_ch, rev_maj, rev_wrapping_add,
    },
    utils::{AbstractVal, RevRes},
};

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

// all the possible step kinds
#[derive(Debug, Clone)]
enum PossibleStep {
    Step1 {
        i: usize,
        h: [u32; 8],
        context: Step1Context,
    },
    Step2 {
        i: usize,
        h: [u32; 8],
        a: u32,
        b: u32,
        c: u32,
        d: u32,
        e: u32,
        f: u32,
        g: u32,
        h_temp: u32,
    },
    Step3 {
        h: [u32; 8],
        a: u32,
        b: u32,
        c: u32,
        d: u32,
        e: u32,
        f: u32,
        g: u32,
        h_temp: u32,
    },
}

#[derive(Debug, Default, Clone)]
struct Step1Context {
    h_temp: Option<u32>,
    g: Option<u32>,
    f: Option<u32>,
    e: Option<u32>,
    d: Option<u32>,
    c: Option<u32>,
    b: Option<u32>,
    a: Option<u32>,
}

impl PossibleStep {
    fn back(
        self,
        constraints_holder: &mut ConstraintsHolder,
        id: ConstraintId,
    ) -> RevRes<PossibleStep> {
        match self {
            PossibleStep::Step1 {
                i,
                mut h,
                mut context,
            } => {
                let constraints = constraints_holder.get(&id);

                let (prev_h_val, val) = match rev_wrapping_add(
                    h[i],
                    None,
                    None,
                    constraints_holder,
                    id.next(0),
                    1,
                    Context::new(vec![]),
                ) {
                    RevRes::Normal(x) => x,
                    RevRes::ConstraintsChanged(x) => return RevRes::ConstraintsChanged(x),
                };

                h[i] = prev_h_val;
                *match i {
                    0 => &mut context.a,
                    1 => &mut context.b,
                    2 => &mut context.c,
                    3 => &mut context.d,
                    4 => &mut context.e,
                    5 => &mut context.f,
                    6 => &mut context.g,
                    7 => &mut context.h_temp,
                    _ => unreachable!(),
                } = Some(val);
                if i <= 0 {
                    return PossibleStep::Step2 {
                        i: 63,
                        h,
                        a: context.a.unwrap(),
                        b: context.b.unwrap(),
                        c: context.c.unwrap(),
                        d: context.d.unwrap(),
                        e: context.e.unwrap(),
                        f: context.f.unwrap(),
                        g: context.g.unwrap(),
                        h_temp: context.h_temp.unwrap(),
                    }
                    .back(constraints_holder, id.next(2));
                }
                PossibleStep::Step1 {
                    i: i - 1,
                    h,
                    context,
                }
                .back(constraints_holder, id.next(1))
            }
            PossibleStep::Step2 {
                i,
                h,
                a,
                b,
                c,
                d,
                e,
                f,
                g,
                h_temp,
            } => {
                if i == 63 {
                    println!("{a}");
                }

                let w: [u32; 64] = [
                    0x80000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
                    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
                    0x00000000, 0x00000000, 0x00000000, 0x00000200, 0x80000000, 0x01400000,
                    0x00205000, 0x00005088, 0x22000800, 0x22550014, 0x05089742, 0xa0000020,
                    0x5a880000, 0x005c9400, 0x0016d49d, 0xfa801f00, 0xd33225d0, 0x11675959,
                    0xf6e6bfda, 0xb30c1549, 0x08b2b050, 0x9d7c4c27, 0x0ce2a393, 0x88e6e1ea,
                    0xa52b4335, 0x67a16f49, 0xd732016f, 0x4eeb2e91, 0x5dbf55e5, 0x8eee2335,
                    0xe2bc5ec2, 0xa83f4394, 0x45ad78f7, 0x36f3d0cd, 0xd99c05e8, 0xb0511dc7,
                    0x69bc7ac4, 0xbd11375b, 0xe3ba71e5, 0x3b209ff2, 0x18feee17, 0xe25ad9e7,
                    0x13375046, 0x0515089d, 0x4f0d0f04, 0x2627484e, 0x310128d2, 0xc668b434,
                    0x420841cc, 0x62d311b8, 0xe59ba771, 0x85a7a484,
                ];
                // t1 has to be taken into context if you are after `a` override,
                // because the fact that we have t2 doesnt mean we can calculate t1 without a
                let (t1, t2) = match rev_wrapping_add(
                    a,
                    None,
                    None,
                    constraints_holder,
                    id.next(0),
                    1,
                    Context::new(vec![
                        (
                            "K",
                            AbstractVal::Arr(K.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "w",
                            AbstractVal::Arr(w.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "h",
                            AbstractVal::Arr(h.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        ("a", a.into()),
                        ("b", b.into()),
                        ("c", c.into()),
                        ("d", d.into()),
                        ("e", e.into()),
                        ("f", f.into()),
                        ("g", g.into()),
                        ("h_temp", h_temp.into()),
                        ("i", (i as u32).into()),
                    ]),
                ) {
                    RevRes::Normal(x) => x,
                    RevRes::ConstraintsChanged(x) => return RevRes::ConstraintsChanged(x),
                };
                let a = match rev_assignment(
                    b,
                    None,
                    constraints_holder,
                    id.next(1),
                    1,
                    Context::new(vec![
                        (
                            "h".into(),
                            AbstractVal::Arr(h.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "w".into(),
                            AbstractVal::Arr(w.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "K".into(),
                            AbstractVal::Arr(K.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        // no `a` should be used ever there, should be always mapped out
                        ("b", b.into()),
                        ("c", c.into()),
                        ("d", d.into()),
                        ("e", e.into()),
                        ("f", f.into()),
                        ("g", g.into()),
                        ("i", (i as u32).into()),
                        ("h_temp", h_temp.into()),
                        ("t1", t1.into()),
                    ]),
                ) {
                    RevRes::Normal(x) => x,
                    RevRes::ConstraintsChanged(x) => return RevRes::ConstraintsChanged(x),
                };

                let b = match rev_assignment(
                    c,
                    None,
                    constraints_holder,
                    id.next(2),
                    2,
                    Context::new(vec![
                        (
                            "K".into(),
                            AbstractVal::Arr(K.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "w".into(),
                            AbstractVal::Arr(w.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "h".into(),
                            AbstractVal::Arr(h.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        ("a", a.into()),
                        // no `b` should be used ever there, should be always mapped out
                        ("c", c.into()),
                        ("d", d.into()),
                        ("e", e.into()),
                        ("f", f.into()),
                        ("g", g.into()),
                        ("h_temp", h_temp.into()),
                        ("i", (i as u32).into()),
                        ("t1", t1.into()),
                    ]),
                ) {
                    RevRes::Normal(x) => x,
                    RevRes::ConstraintsChanged(x) => {
                        return RevRes::ConstraintsChanged(x);
                    }
                };
                let c = match rev_assignment(
                    d,
                    None,
                    constraints_holder,
                    id.next(3),
                    3,
                    Context::new(vec![
                        (
                            "K".into(),
                            AbstractVal::Arr(K.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "w".into(),
                            AbstractVal::Arr(w.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "h".into(),
                            AbstractVal::Arr(h.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        ("a", a.into()),
                        ("b", b.into()),
                        // no `c` should be used ever there, should be always mapped out
                        ("d", d.into()),
                        ("e", e.into()),
                        ("f", f.into()),
                        ("g", g.into()),
                        ("h_temp", h_temp.into()),
                        ("i", (i as u32).into()),
                        ("t1", t1.into()),
                    ]),
                ) {
                    RevRes::Normal(x) => x,
                    RevRes::ConstraintsChanged(x) => return RevRes::ConstraintsChanged(x),
                };
                // not the same d as in prev iteration
                let (d, _t1) = match rev_wrapping_add(
                    e,
                    None,
                    Some("t1".into()),
                    constraints_holder,
                    id.next(4),
                    2,
                    Context::new(vec![
                        (
                            "K".into(),
                            AbstractVal::Arr(K.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "w".into(),
                            AbstractVal::Arr(w.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "h".into(),
                            AbstractVal::Arr(h.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        ("a", a.into()),
                        ("b", b.into()),
                        ("c", c.into()),
                        // no `d` should be used ever there, should be always mapped out
                        ("f", f.into()),
                        ("e", e.into()),
                        ("g", g.into()),
                        ("h_temp", h_temp.into()),
                        ("i", (i as u32).into()),
                        ("t1", t1.into()),
                    ]),
                ) {
                    RevRes::Normal(x) => x,
                    RevRes::ConstraintsChanged(x) => return RevRes::ConstraintsChanged(x),
                };
                let e = match rev_assignment(
                    f,
                    None,
                    constraints_holder,
                    id.next(5),
                    4,
                    Context::new(vec![
                        (
                            "K".into(),
                            AbstractVal::Arr(K.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "w".into(),
                            AbstractVal::Arr(w.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "h".into(),
                            AbstractVal::Arr(h.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        ("a", a.into()),
                        ("b", b.into()),
                        ("c", c.into()),
                        ("d", d.into()),
                        // no `e` should be used ever there, should be always mapped out
                        ("f", f.into()),
                        ("g", g.into()),
                        ("h_temp", h_temp.into()),
                        ("i", (i as u32).into()),
                        ("t1", t1.into()),
                    ]),
                ) {
                    RevRes::Normal(x) => x,
                    RevRes::ConstraintsChanged(x) => return RevRes::ConstraintsChanged(x),
                };
                let f = match rev_assignment(
                    g,
                    None,
                    constraints_holder,
                    id.next(6),
                    5,
                    Context::new(vec![
                        (
                            "K".into(),
                            AbstractVal::Arr(K.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "w".into(),
                            AbstractVal::Arr(w.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "h".into(),
                            AbstractVal::Arr(h.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        ("a", a.into()),
                        ("b", b.into()),
                        ("c", c.into()),
                        ("d", d.into()),
                        ("e", e.into()),
                        // no `f` should be used ever there, should be always mapped out
                        ("g", g.into()),
                        ("h_temp", h_temp.into()),
                        ("i", (i as u32).into()),
                        ("t1", t1.into()),
                    ]),
                ) {
                    RevRes::Normal(x) => x,
                    RevRes::ConstraintsChanged(x) => return RevRes::ConstraintsChanged(x),
                };
                let g = match rev_assignment(
                    h_temp,
                    None,
                    constraints_holder,
                    id.next(7),
                    6,
                    Context::new(vec![
                        (
                            "K".into(),
                            AbstractVal::Arr(K.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "w".into(),
                            AbstractVal::Arr(w.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "h".into(),
                            AbstractVal::Arr(h.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        ("a", a.into()),
                        ("b", b.into()),
                        ("c", c.into()),
                        ("d", d.into()),
                        ("e", e.into()),
                        ("f", f.into()),
                        // no `g` should be used ever there, should be always mapped out
                        ("h_temp", h_temp.into()),
                        ("i", (i as u32).into()),
                        ("t1", t1.into()),
                    ]),
                ) {
                    RevRes::Normal(x) => x,
                    RevRes::ConstraintsChanged(x) => return RevRes::ConstraintsChanged(x),
                };
                // h_temp there is invalid, we dont know its value, all the other values are valid
                let (t2, maj) = match rev_wrapping_add(
                    t2,
                    None,
                    None,
                    constraints_holder,
                    id.next(8),
                    3,
                    Context::new(vec![
                        (
                            "K".into(),
                            AbstractVal::Arr(K.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "w".into(),
                            AbstractVal::Arr(w.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "h".into(),
                            AbstractVal::Arr(h.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        ("i", (i as u32).into()),
                        ("a".into(), a.into()),
                        ("b".into(), b.into()),
                        ("c".into(), c.into()),
                        ("d".into(), d.into()),
                        ("e".into(), e.into()),
                        ("f".into(), f.into()),
                        ("g".into(), g.into()),
                        ("h_temp".into(), h_temp.into()),
                    ]),
                ) {
                    RevRes::Normal(x) => x,
                    RevRes::ConstraintsChanged(x) => return RevRes::ConstraintsChanged(x),
                };
                // the same a, b, c that we currently have, we validate it with maj
                let (_a, _b, _c) = match rev_maj(
                    maj,
                    Some(a),
                    Some(b),
                    Some(c),
                    constraints_holder,
                    id.next(9),
                ) {
                    RevRes::Normal(x) => x,
                    RevRes::ConstraintsChanged(x) => return RevRes::ConstraintsChanged(x),
                };
                let _a = match rev_big_sigma0(t2, Some(a), constraints_holder, id.next(10)) {
                    RevRes::Normal(x) => x,
                    RevRes::ConstraintsChanged(x) => return RevRes::ConstraintsChanged(x),
                };
                let (t1, _w_val) = match rev_wrapping_add(
                    t1,
                    None,
                    Some(Bytecode::AvLoad("w", i).into()),
                    constraints_holder,
                    id.next(11),
                    4,
                    Context::new(vec![
                        (
                            "K".into(),
                            AbstractVal::Arr(K.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "w".into(),
                            AbstractVal::Arr(w.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "h".into(),
                            AbstractVal::Arr(h.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        ("i", (i as u32).into()),
                        ("a".into(), a.into()),
                        ("b".into(), b.into()),
                        ("c".into(), c.into()),
                        ("d".into(), d.into()),
                        ("e".into(), e.into()),
                        ("f".into(), f.into()),
                        ("g".into(), g.into()),
                        ("h_temp".into(), h_temp.into()),
                        // use but ALWAYS map when adding constraint there
                        ("t1".into(), t1.into()),
                    ]),
                ) {
                    RevRes::Normal(x) => x,
                    RevRes::ConstraintsChanged(x) => return RevRes::ConstraintsChanged(x),
                };
                let (t1, _k_val) = match rev_wrapping_add(
                    t1,
                    None,
                    Some(Bytecode::AvLoad("K", i).into()),
                    constraints_holder,
                    id.next(12),
                    5,
                    Context::new(vec![
                        (
                            "K".into(),
                            AbstractVal::Arr(K.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "w".into(),
                            AbstractVal::Arr(w.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "h".into(),
                            AbstractVal::Arr(h.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        ("i", (i as u32).into()),
                        ("a".into(), a.into()),
                        ("b".into(), b.into()),
                        ("c".into(), c.into()),
                        ("d".into(), d.into()),
                        ("e".into(), e.into()),
                        ("f".into(), f.into()),
                        ("g".into(), g.into()),
                        ("h_temp".into(), h_temp.into()),
                        // use but ALWAYS map when adding constraint there
                        ("t1".into(), t1.into()),
                    ]),
                ) {
                    RevRes::Normal(x) => x,
                    RevRes::ConstraintsChanged(x) => return RevRes::ConstraintsChanged(x),
                };
                let (t1, ch) = match rev_wrapping_add(
                    t1,
                    None,
                    None,
                    constraints_holder,
                    id.next(13),
                    6,
                    Context::new(vec![
                        (
                            "K".into(),
                            AbstractVal::Arr(K.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "w".into(),
                            AbstractVal::Arr(w.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "h".into(),
                            AbstractVal::Arr(h.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        ("i", (i as u32).into()),
                        ("a".into(), a.into()),
                        ("b".into(), b.into()),
                        ("c".into(), c.into()),
                        ("d".into(), d.into()),
                        ("e".into(), e.into()),
                        ("f".into(), f.into()),
                        ("g".into(), g.into()),
                        ("h_temp".into(), h_temp.into()),
                        // use but ALWAYS map when adding constraint there
                        ("t1".into(), t1.into()),
                    ]),
                ) {
                    RevRes::Normal(x) => x,
                    RevRes::ConstraintsChanged(x) => return RevRes::ConstraintsChanged(x),
                };
                // the same e, f, g
                let (_e, _f, _g) = match rev_ch(
                    ch,
                    Some(e),
                    Some(f),
                    Some(g),
                    constraints_holder,
                    id.next(14),
                ) {
                    RevRes::Normal(x) => x,
                    RevRes::ConstraintsChanged(x) => return RevRes::ConstraintsChanged(x),
                };
                // println!("{i}");
                // not the same h_temp, because prev one is reversed from g
                let (h_temp, big_sigma) = match rev_wrapping_add(
                    t1,
                    None,
                    None,
                    constraints_holder,
                    id.next(15),
                    7,
                    Context::new(vec![
                        ("a".into(), a.into()),
                        ("b".into(), b.into()),
                        ("c".into(), c.into()),
                        ("d".into(), d.into()),
                        ("e".into(), e.into()),
                        ("f".into(), f.into()),
                        ("g".into(), g.into()),
                        ("h_temp".into(), h_temp.into()),
                        (
                            "K".into(),
                            AbstractVal::Arr(K.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "w".into(),
                            AbstractVal::Arr(w.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        (
                            "h".into(),
                            AbstractVal::Arr(h.iter().map(|x| AbstractVal::U32(*x)).collect()),
                        ),
                        ("i", (i as u32).into()),
                        ("t1".into(), t1.into()),
                    ]),
                ) {
                    RevRes::Normal(x) => x,
                    RevRes::ConstraintsChanged(x) => return RevRes::ConstraintsChanged(x),
                };
                let _e = match rev_big_sigma1(big_sigma, Some(e), constraints_holder, id.next(16)) {
                    RevRes::Normal(x) => x,
                    RevRes::ConstraintsChanged(x) => return RevRes::ConstraintsChanged(x),
                };

                if i <= 0 {
                    return PossibleStep::Step3 {
                        h,
                        a,
                        b,
                        c,
                        d,
                        e,
                        f,
                        g,
                        h_temp,
                    }
                    .back(constraints_holder, id.next(18));
                }

                PossibleStep::Step2 {
                    i: i - 1,
                    h,
                    a,
                    b,
                    c,
                    d,
                    e,
                    f,
                    g,
                    h_temp,
                }
                .back(constraints_holder, id.next(17))
            }
            PossibleStep::Step3 {
                h,
                a,
                b,
                c,
                d,
                e,
                f,
                g,
                h_temp,
            } => {
                if h_temp != h[7] {
                    match constraints_holder.get(&id.back()) {
                        Constraints::Step2(x) => {
                            let constraint = Constraint::Equals(
                                BytecodeProgram::from_single_code(Bytecode::Input(0)),
                                BytecodeProgram::from_single_code(Bytecode::AvLoad("h", 7)),
                            );
                            x.w_add7.to_w_add().constraints.push(constraint);
                            return RevRes::ConstraintsChanged(id);
                        }
                        _ => todo!("{:?}", constraints_holder.get(&id.back())),
                    }
                }
                assert!(g == h[6]);
                assert!(f == h[5]);
                assert!(e == h[4]);
                assert!(d == h[3]);
                assert!(c == h[2]);
                assert!(b == h[1]);
                assert!(a == h[0]);

                todo!()
            }
        }
    }
}

// reversal process, contains first step
pub struct RevProc {
    first_step: PossibleStep,
    holder: ConstraintsHolder,
}

impl RevProc {
    pub fn new(out: [u8; 32]) -> Self {
        // h value before digest
        let h = rev_compute_digest(out);
        Self {
            first_step: PossibleStep::Step1 {
                i: 7,
                h,
                context: Step1Context::default(),
            },
            holder: ConstraintsHolder::default(),
        }
    }

    // returns the initial value, takes out as an input
    pub fn single_run(&mut self) -> [u8; 64] {
        loop {
            let res = self
                .first_step
                .clone()
                .back(&mut self.holder, ConstraintId(vec![0]));
            // println!("{:?}", self.first_constraints);
            println!("{:?}", res);
            match res {
                RevRes::Normal(_) => todo!(),
                RevRes::ConstraintsChanged(_) => continue,
            }
        }
    }
}

pub fn rev_compute_digest(digest: [u8; 32]) -> [u32; 8] {
    let mut h = [0u32; 8];
    for i in 0..8 {
        h[i] = ((digest[i * 4] as u32) << 24)
            | ((digest[i * 4 + 1] as u32) << 16)
            | ((digest[i * 4 + 2] as u32) << 8)
            | (digest[i * 4 + 3] as u32);
    }
    h
}

#[derive(Debug, Default)]
pub struct Step1Constraints {
    pub w_add: WrappingAddConstraints,
    pub step1: Option<Constraints>,
    pub step2: Option<Constraints>,
}

#[derive(Debug)]
pub struct Step2Constraints {
    pub w_add1: Constraints,
    pub ass1: Constraints,
    pub ass2: Constraints,
    pub ass3: Constraints,
    pub w_add2: Constraints,
    pub ass4: Constraints,
    pub ass5: Constraints,
    pub ass6: Constraints,
    pub w_add3: Constraints,
    pub maj: Constraints,
    pub big_sigma0: Constraints,
    pub w_add4: Constraints,
    pub w_add5: Constraints,
    pub w_add6: Constraints,
    pub ch: Constraints,
    pub w_add7: Constraints,
    pub big_sigma1: Constraints,
    pub step2: Option<Constraints>,
    pub step3: Option<Constraints>,
}

impl Default for Step2Constraints {
    fn default() -> Self {
        Self {
            step3: Default::default(),
            step2: Default::default(),
            maj: Constraints::Maj(MajConstraints::default().into()),
            ch: Constraints::Ch(ChConstraints::default().into()),
            w_add1: Constraints::WAdd(Default::default()),
            w_add2: Constraints::WAdd(Default::default()),
            w_add3: Constraints::WAdd(Default::default()),
            w_add4: Constraints::WAdd(Default::default()),
            w_add5: Constraints::WAdd(Default::default()),
            w_add6: Constraints::WAdd(Default::default()),
            w_add7: Constraints::WAdd(Default::default()),
            ass1: Constraints::Ass(Default::default()),
            ass2: Constraints::Ass(Default::default()),
            ass3: Constraints::Ass(Default::default()),
            ass4: Constraints::Ass(Default::default()),
            ass5: Constraints::Ass(Default::default()),
            ass6: Constraints::Ass(Default::default()),
            big_sigma0: Constraints::BigSigma0(Default::default()),
            big_sigma1: Constraints::BigSigma1(Default::default()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Context {
    pub values: HashMap<&'static str, AbstractVal>,
}

impl Context {
    pub fn new(values: Vec<(&'static str, AbstractVal)>) -> Self {
        Self {
            values: {
                let mut res = HashMap::new();
                for (name, val) in values {
                    res.insert(name, val);
                }
                res
            },
        }
    }
}
