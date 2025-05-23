// use std::collections::VecDeque; // replaced with Vec

use std::rc::Rc;

use crate::{
    bytecode::{Bytecode, BytecodeProgram, BytecodeSingleMapping, eval},
    constraint::{
        Constraint, ConstraintId, Constraints, ConstraintsHolder, ContextVal, Operation, RevVal,
    },
    custom_opcode::OpcodeInterface,
    rev_val_holder::LazyRevVal,
    reverse::Context,
    sha256::{big_sigma0, big_sigma1, ch, maj},
    utils::{AbstractVal, RevRes},
    var::Var,
};

static mut COUNTER: usize = 0;

pub fn rev_big_sigma0(
    output: u32,
    x_in: Option<u32>,
    constraints_holder: &mut ConstraintsHolder,
    id: ConstraintId,
) -> RevRes<u32> {
    if let Some(x_in) = x_in {
        if big_sigma0(x_in) != output {
            match constraints_holder.get(&id.back()) {
                Constraints::Step2(x) => {
                    let constraint = Constraint::Equals(
                        BytecodeProgram::from_single_code(Bytecode::Input(0)),
                        BytecodeProgram::from_code(Vec::from([
                            Bytecode::Context(Var::A),
                            Bytecode::BigSigma0,
                        ])),
                    );
                    x.w_add3.to_w_add().constraints.push(constraint);
                }
                _ => todo!("{:?}", constraints_holder.get(&id.back())),
            }
            return RevRes::ConstraintsChanged(id);
        }
        return x_in.into();
    }

    let (res, val) = rev_xor(output, 1);
    let (x, _n) = rev_rotr(val, None, Some(22));
    let (res, val) = rev_xor(res, 2);
    let (_x, _n) = rev_rotr(val, Some(x), Some(13));
    let (_x, _n) = rev_rotr(res, Some(x), Some(2));
    x.into()
}

#[derive(Debug, Default)]
pub struct BigSigma0Constraints {}

pub fn rev_big_sigma1(
    output: u32,
    x_in: Option<u32>,
    constraints_holder: &mut ConstraintsHolder,
    id: ConstraintId,
) -> RevRes<u32> {
    if let Some(x_in) = x_in {
        if big_sigma1(x_in) != output {
            match constraints_holder.get(&id.back()) {
                Constraints::Step2(x) => {
                    let constraint = Constraint::Equals(
                        BytecodeProgram::from_single_code(Bytecode::Input(1)),
                        BytecodeProgram::from_code(Vec::from([
                            Bytecode::Context(Var::E),
                            Bytecode::BigSigma1,
                        ])),
                    );
                    x.w_add7.to_w_add().constraints.push(constraint);
                    return RevRes::ConstraintsChanged(id);
                }
                _ => todo!("{:?}", constraints_holder.get(&id.back())),
            }
        }
        return x_in.into();
    }
    todo!()
}

#[derive(Debug, Default)]
pub struct BigSigma1Constraints {}

pub fn rev_rotr(output: u32, x_in: Option<u32>, n_in: Option<u32>) -> (u32, u32) {
    if let Some(x_in) = x_in {
        if let Some(n_in) = n_in {
            assert!(x_in.rotate_right(n_in) == output);
            return (x_in, n_in);
        }
        todo!()
    }
    if let Some(n_in) = n_in {
        return (output.rotate_left(n_in), n_in);
    }
    todo!()
}

pub fn rev_ch(
    output: u32,
    in1: Option<u32>,
    in2: Option<u32>,
    in3: Option<u32>,
    constraints_holder: &mut ConstraintsHolder,
    id: ConstraintId,
) -> RevRes<(u32, u32, u32)> {
    let constraints = match constraints_holder.get(&id.back()) {
        Constraints::Step2(x) => &mut x.ch,
        _ => todo!("{:?}", constraints_holder.get(&id.back())),
    };

    if let Some(in1) = in1 {
        if let Some(in2) = in2 {
            if let Some(in3) = in3 {
                if ch(in1, in2, in3) != output {
                    match constraints_holder.get(&id.back()) {
                        Constraints::Step2(x) => {
                            let constraint = Constraint::Equals(
                                BytecodeProgram::from_single_code(Bytecode::Input(1)),
                                BytecodeProgram::from_code(Vec::from([
                                    Bytecode::Context(Var::E),
                                    Bytecode::Context(Var::F),
                                    Bytecode::Context(Var::G),
                                    Bytecode::Ch,
                                ])),
                            );
                            let t1_1 =
                                BytecodeProgram::from_single_code(Bytecode::Context(Var::HTemp))
                                    .extend(
                                        BytecodeProgram::from_single_code(Bytecode::Context(
                                            Var::E,
                                        ))
                                        .add(Bytecode::BigSigma1),
                                    )
                                    .add(Bytecode::WAdd);
                            let old_t1 =
                                BytecodeProgram::from_single_code(Bytecode::Context(Var::T1))
                                    .extend(t1_1)
                                    .add(Bytecode::WSub);
                            let constraint = constraint.map_context(vec![(Var::T1, old_t1)]);
                            unsafe {
                                static mut COUNTER: usize = 0;
                                if COUNTER == 64 {
                                    println!("{constraint}"); // check t1 and if it
                                    todo!();
                                }
                                COUNTER += 1;
                            }
                            x.w_add6.to_w_add().constraints.push(constraint);
                            return RevRes::ConstraintsChanged(id);
                        }
                        _ => todo!("{:?}", constraints_holder.get(&id.back())),
                    }
                }
                return (in1, in2, in3).into();
            }
            todo!()
        }
        todo!()
    }

    let (res, val) = rev_xor(output, 1);
    let RevRes::Normal((x, z)) = rev_and(
        val,
        None,
        None,
        constraints_holder,
        id.next(1),
        Context::new(vec![]),
        1,
    ) else {
        todo!()
    };
    let x = !x;
    let RevRes::Normal((_x, y)) = rev_and(
        res,
        Some(x),
        None,
        constraints_holder,
        id.next(2),
        Context::new(vec![]),
        2,
    ) else {
        todo!()
    };
    (x, y, z).into()
}

#[derive(Debug, Default)]
pub struct ChConstraints {
    xor: XorConstraints,
    and1: AndConstraints,
    and2: AndConstraints,
}

pub static mut INDICATOR: bool = false;

pub fn rev_assignment(
    output: u32,
    x_in: Option<u32>,
    constraints_holder: &mut ConstraintsHolder,
    id: ConstraintId,
    count: usize,
    context: Context,
) -> RevRes<u32> {
    let Constraints::Ass(constraints) = constraints_holder.get(&id) else {
        todo!()
    };
    if let Some(x_in) = x_in {
        todo!()
    }

    match count {
        1 => match constraints.constraints.len() {
            0 => {}
            1 => {
                let val = {
                    let constraint = &constraints.constraints[0];
                    let Constraint::Equals(val1, val2) = constraint;
                    assert!(val1.is_input(0));
                    val2
                }
                .clone();
                if val.execute(&context).to_u32() != output {
                    match constraints_holder.get(&id.back().back()) {
                        Constraints::Step2(x) => {
                            struct Custom {
                                val: BytecodeProgram,
                            }
                            impl OpcodeInterface for Custom {
                                fn apply(
                                    &self,
                                    ctx: &Context,
                                    acumulated_mapping: &[BytecodeSingleMapping],
                                ) -> AbstractVal {
                                    // an example
                                    // 1. values from initial context and existing mappings, we need to handle mappings in case we are wrapped
                                    let h = ctx.get(Var::H);
                                    let w = ctx.get(Var::W);
                                    let k = ctx.get(Var::K);
                                    let a = eval(acumulated_mapping, ctx, Var::A);
                                    let c = eval(acumulated_mapping, ctx, Var::C);
                                    let d = eval(acumulated_mapping, ctx, Var::D);
                                    let e = eval(acumulated_mapping, ctx, Var::E);
                                    let f = eval(acumulated_mapping, ctx, Var::F);
                                    let g = eval(acumulated_mapping, ctx, Var::G);
                                    let h_temp = eval(acumulated_mapping, ctx, Var::HTemp);
                                    let i = eval(acumulated_mapping, ctx, Var::I).to_u32();
                                    let t1 = eval(acumulated_mapping, ctx, Var::T1).to_u32();

                                    // 2. values from current mapping
                                    let old_d = e.to_u32().wrapping_sub(t1);
                                    let (b, c, d, e, f, g, h_temp, t1, i) = (
                                        c.clone(),
                                        d.clone(),
                                        old_d,
                                        f.clone(),
                                        g.clone(),
                                        h_temp.clone(),
                                        t1.wrapping_sub(w.to_vec()[i as usize].to_u32())
                                            .wrapping_sub(k.to_vec()[i as usize].to_u32())
                                            .wrapping_sub(ch(
                                                f.to_u32(),
                                                g.to_u32(),
                                                h_temp.to_u32(),
                                            ))
                                            .wrapping_sub(big_sigma1(f.to_u32())),
                                        a.to_u32().wrapping_sub(
                                            big_sigma0(c.to_u32()).wrapping_add(maj(
                                                c.to_u32(),
                                                d.to_u32(),
                                                old_d,
                                            )),
                                        ),
                                        i.wrapping_sub(1),
                                    );

                                    // 3. context construction from values, we provide everything that we mapped
                                    let ctx = Context::new(vec![
                                        (Var::H, h),
                                        (Var::W, w),
                                        (Var::K, k),
                                        (Var::B, b),
                                        (Var::C, c),
                                        (Var::D, AbstractVal::U32(d)),
                                        (Var::E, e),
                                        (Var::F, f),
                                        (Var::G, g),
                                        (Var::HTemp, AbstractVal::U32(h_temp)),
                                        (Var::T1, AbstractVal::U32(t1)),
                                        (Var::I, AbstractVal::U32(i)),
                                    ]);

                                    // 4. final program execution using the context
                                    self.val.execute(&ctx)
                                }

                                fn display(&self) -> String {
                                    format!(
                                        "{{\nlet (i, b, c, d, e, f, g, h_temp, t1) = (i.wrapping_sub(1), c, d, e.wrapping_sub(t1), f, g, h_temp, t1.wrapping_sub(w[i]).wrapping_sub(K[i]).wrapping_sub(ch(f, g, h_temp)).wrapping_sub(big_sigma1(f)), a.wrapping_sub(big_sigma0(c).wrapping_add(maj(c, d, e.wrapping_sub(t1)))));\n{}\n}}",
                                        self.val
                                    )
                                }
                            }
                            let val = BytecodeProgram::from_single_code(Bytecode::Custom(
                                Rc::new(Custom { val }),
                                0,
                            ));

                            // let old_d = BytecodeProgram::from_code(Vec::from([
                            //     Var::E.into(),
                            //     Var::T1.into(),
                            //     Bytecode::WSub,
                            // ]));
                            // let ch = BytecodeProgram::from_code(Vec::from([
                            //     Var::F.into(),
                            //     Var::G.into(),
                            //     Var::HTemp.into(),
                            //     Bytecode::Ch,
                            // ]));
                            // let old_h_temp = BytecodeProgram::from_code(Vec::from([
                            //     Var::T1.into(),
                            //     Var::I.into(),
                            //     Bytecode::AvLoadCtx(Var::W),
                            //     Bytecode::WSub,
                            //     Var::I.into(),
                            //     Bytecode::AvLoadCtx(Var::K),
                            //     Bytecode::WSub,
                            // ]))
                            // .extend(ch)
                            // .extend(BytecodeProgram::from_code(Vec::from([
                            //     Bytecode::WSub,
                            //     Bytecode::Context(Var::F),
                            //     Bytecode::BigSigma1,
                            //     Bytecode::WSub,
                            // ])));
                            // let prev_iter_t2 = BytecodeProgram::from_code(Vec::from([
                            //     Var::C.into(),
                            //     Bytecode::BigSigma0,
                            //     Var::C.into(),
                            //     Var::D.into(),
                            // ]))
                            // .extend(old_d.clone())
                            // .extend(BytecodeProgram::from_code(Vec::from([
                            //     Bytecode::Maj,
                            //     Bytecode::WAdd,
                            // ])));
                            // let prev_iter_t1 = BytecodeProgram::from_single_code(Var::A.into())
                            //     .extend(prev_iter_t2)
                            //     .add(Bytecode::WSub);
                            let constraint = Constraint::Equals(
                                BytecodeProgram::from_single_code(Bytecode::Input(0)),
                                val,
                            );
                            // let constraint = constraint.map_context(vec![
                            //     (
                            //         Var::I,
                            //         BytecodeProgram::from_code(Vec::from([
                            //             Var::I.into(),
                            //             Bytecode::Const(1),
                            //             Bytecode::WSub,
                            //         ])),
                            //     ),
                            //     (Var::B, Var::C.into()),
                            //     (Var::C, Var::D.into()),
                            //     (Var::D, old_d),
                            //     (Var::E, Var::F.into()),
                            //     (Var::F, Var::G.into()),
                            //     (Var::G, Var::HTemp.into()),
                            //     (Var::HTemp, old_h_temp),
                            //     (Var::T1, prev_iter_t1),
                            // ]);
                            let Constraints::Ass(ass2) = &mut x.ass2 else {
                                todo!()
                            };
                            ass2.constraints.push(constraint);
                            return RevRes::ConstraintsChanged(id);
                        }
                        _ => todo!("{:?}", constraints_holder.get(&id.back().back())),
                    }
                }
                todo!()
            }
            _ => todo!("{}", constraints.constraints.len()),
        },
        2 => match constraints.constraints.len() {
            0 => {}
            1 => {
                let val = {
                    let constraint = &constraints.constraints[0];
                    let Constraint::Equals(val1, val2) = constraint;
                    assert!(val1.is_input(0));
                    val2
                }
                .clone();

                if val.execute(&context).to_u32() != output {
                    match constraints_holder.get(&id.back().back()) {
                        Constraints::Step2(x) => {
                            let old_d = BytecodeProgram::from_code(Vec::from([
                                Var::E.into(),
                                Var::T1.into(),
                                Bytecode::WSub,
                            ]));
                            let ch = BytecodeProgram::from_code(Vec::from([
                                Var::F.into(),
                                Var::G.into(),
                                Var::HTemp.into(),
                                Bytecode::Ch,
                            ]));
                            let old_h_temp = BytecodeProgram::from_code(Vec::from([
                                Var::T1.into(),
                                Var::I.into(),
                                Bytecode::AvLoadCtx(Var::W),
                                Bytecode::WSub,
                                Var::I.into(),
                                Bytecode::AvLoadCtx(Var::K),
                                Bytecode::WSub,
                            ]))
                            .extend(ch)
                            .extend(BytecodeProgram::from_code(Vec::from([
                                Bytecode::WSub,
                                Bytecode::Context(Var::F),
                                Bytecode::BigSigma1,
                                Bytecode::WSub,
                            ])));
                            let prev_iter_t2 = BytecodeProgram::from_code(Vec::from([
                                Var::B.into(),
                                Bytecode::BigSigma0,
                                Var::B.into(),
                                Var::D.into(),
                            ]))
                            .extend(old_d.clone())
                            .add(Bytecode::Maj)
                            .add(Bytecode::WAdd);
                            let prev_iter_t1 = BytecodeProgram::from_single_code(Var::A.into())
                                .extend(prev_iter_t2)
                                .add(Bytecode::WSub);
                            let constraint = Constraint::Equals(
                                BytecodeProgram::from_single_code(Bytecode::Input(0)),
                                val,
                            );
                            let constraint = constraint.map_context(vec![
                                (
                                    Var::I,
                                    BytecodeProgram::from_code(Vec::from([
                                        Var::I.into(),
                                        Bytecode::Const(1),
                                        Bytecode::WSub,
                                    ])),
                                ),
                                (Var::A, Var::B.into()),
                                (Var::C, Var::D.into()),
                                (Var::D, old_d),
                                (Var::E, Var::F.into()),
                                (Var::F, Var::G.into()),
                                (Var::G, Var::HTemp.into()),
                                (Var::HTemp, old_h_temp),
                                (Var::T1, prev_iter_t1),
                            ]);
                            let Constraints::Ass(ass3) = &mut x.ass3 else {
                                todo!()
                            };
                            ass3.constraints.push(constraint);
                            return RevRes::ConstraintsChanged(id);
                        }
                        _ => todo!("{:?}", constraints_holder.get(&id.back().back())),
                    }
                }
                todo!()
            }
            _ => todo!("{}", constraints.constraints.len()),
        },
        3 => match constraints.constraints.len() {
            0 => {}
            1 => {
                let val = {
                    let constraint = &constraints.constraints[0];
                    let Constraint::Equals(val1, val2) = constraint;
                    assert!(val1.is_input(0));
                    val2
                }
                .clone();

                if val.execute(&context).to_u32() != output {
                    match constraints_holder.get(&id.back().back()) {
                        Constraints::Step2(x) => {
                            let old_d = BytecodeProgram::from_code(Vec::from([
                                Var::E.into(),
                                Var::T1.into(),
                                Bytecode::WSub,
                            ]));
                            let ch = BytecodeProgram::from_code(Vec::from([
                                Var::F.into(),
                                Var::G.into(),
                                Var::HTemp.into(),
                                Bytecode::Ch,
                            ]));
                            let old_h_temp = BytecodeProgram::from_code(Vec::from([
                                Var::T1.into(),
                                Var::I.into(),
                                Bytecode::AvLoadCtx(Var::W),
                                Bytecode::WSub,
                                Var::I.into(),
                                Bytecode::AvLoadCtx(Var::K),
                                Bytecode::WSub,
                            ]))
                            .extend(ch)
                            .extend(BytecodeProgram::from_code(Vec::from([
                                Bytecode::WSub,
                                Bytecode::Context(Var::F),
                                Bytecode::BigSigma1,
                                Bytecode::WSub,
                            ])));
                            let prev_iter_t2 = BytecodeProgram::from_code(Vec::from([
                                Var::B.into(),
                                Bytecode::BigSigma0,
                                Var::B.into(),
                                Var::C.into(),
                            ]))
                            .extend(old_d.clone())
                            .add(Bytecode::Maj)
                            .add(Bytecode::WAdd);
                            let prev_iter_t1 = BytecodeProgram::from_single_code(Var::A.into())
                                .extend(prev_iter_t2)
                                .add(Bytecode::WSub);
                            let constraint = Constraint::Equals(
                                BytecodeProgram::from_single_code(Bytecode::Input(0)),
                                val,
                            );
                            let constraint = constraint.map_context(vec![
                                (
                                    Var::I,
                                    BytecodeProgram::from_code(Vec::from([
                                        Var::I.into(),
                                        Bytecode::Const(1),
                                        Bytecode::WSub,
                                    ])),
                                ),
                                (Var::A, Var::B.into()),
                                (Var::B, Var::C.into()),
                                (Var::D, old_d),
                                (Var::E, Var::F.into()),
                                (Var::F, Var::G.into()),
                                (Var::G, Var::HTemp.into()),
                                (Var::HTemp, old_h_temp),
                                (Var::T1, prev_iter_t1),
                            ]);
                            x.w_add2.to_w_add().constraints.push(constraint);
                            return RevRes::ConstraintsChanged(id);
                        }
                        _ => todo!("{:?}", constraints_holder.get(&id.back().back())),
                    }
                }
                todo!()
            }
            _ => todo!("{}", constraints.constraints.len()),
        },
        4 => match constraints.constraints.len() {
            0 => {}
            1 => {
                let val = {
                    let constraint = &constraints.constraints[0];
                    let Constraint::Equals(val1, val2) = constraint;
                    assert!(val1.is_input(0));
                    val2
                }
                .clone();

                if val.execute(&context).to_u32() != output {
                    match constraints_holder.get(&id.back().back()) {
                        Constraints::Step2(x) => {
                            let prev_iter_t2 = BytecodeProgram::from_code(Vec::from([
                                Var::B.into(),
                                Bytecode::BigSigma0,
                                Var::B.into(),
                                Var::C.into(),
                                Var::D.into(),
                                Bytecode::Maj,
                                Bytecode::WAdd,
                            ]));
                            let prev_iter_t1 = BytecodeProgram::from_single_code(Var::A.into())
                                .extend(prev_iter_t2)
                                .add(Bytecode::WSub);
                            let old_d = BytecodeProgram::from_single_code(Var::E.into())
                                .extend(prev_iter_t1.clone())
                                .add(Bytecode::WSub);
                            let ch = BytecodeProgram::from_code(Vec::from([
                                Var::E.into(),
                                Var::G.into(),
                                Var::HTemp.into(),
                                Bytecode::Ch,
                            ]));
                            let old_h_temp = BytecodeProgram::from_code(Vec::from([
                                Var::T1.into(),
                                Var::I.into(),
                                Bytecode::AvLoadCtx(Var::W),
                                Bytecode::WSub,
                                Var::I.into(),
                                Bytecode::AvLoadCtx(Var::K),
                                Bytecode::WSub,
                            ]))
                            .extend(ch)
                            .extend(BytecodeProgram::from_code(Vec::from([
                                Bytecode::WSub,
                                Bytecode::Context(Var::E),
                                Bytecode::BigSigma1,
                                Bytecode::WSub,
                            ])));
                            let constraint = Constraint::Equals(
                                BytecodeProgram::from_single_code(Bytecode::Input(0)),
                                val,
                            );
                            let constraint = constraint.map_context(vec![
                                (
                                    Var::I,
                                    BytecodeProgram::from_code(Vec::from([
                                        Var::I.into(),
                                        Bytecode::Const(1),
                                        Bytecode::WSub,
                                    ])),
                                ),
                                (Var::A, Var::B.into()),
                                (Var::B, Var::C.into()),
                                (Var::C, Var::D.into()),
                                (Var::D, old_d),
                                (Var::F, Var::G.into()),
                                (Var::G, Var::HTemp.into()),
                                (Var::HTemp, old_h_temp),
                                (Var::T1, prev_iter_t1),
                            ]);
                            let Constraints::Ass(ass5) = &mut x.ass5 else {
                                todo!()
                            };
                            ass5.constraints.push(constraint);
                            return RevRes::ConstraintsChanged(id);
                        }
                        _ => todo!("{:?}", constraints_holder.get(&id.back().back())),
                    }
                }
                todo!()
            }
            _ => todo!("{}", constraints.constraints.len()),
        },
        5 => match constraints.constraints.len() {
            0 => {}
            1 => {
                let val = {
                    let constraint = &constraints.constraints[0];
                    let Constraint::Equals(val1, val2) = constraint;
                    assert!(val1.is_input(0));
                    val2
                }
                .clone();
                unsafe {
                    INDICATOR = true;
                }
                if val.execute(&context).to_u32() != output {
                    match constraints_holder.get(&id.back().back()) {
                        Constraints::Step2(x) => {
                            let prev_iter_t2 = BytecodeProgram::from_code(Vec::from([
                                Var::B.into(),
                                Bytecode::BigSigma0,
                                Var::B.into(),
                                Var::C.into(),
                                Var::D.into(),
                                Bytecode::Maj,
                                Bytecode::WAdd,
                            ]));
                            let prev_iter_t1 = BytecodeProgram::from_single_code(Var::A.into())
                                .extend(prev_iter_t2)
                                .add(Bytecode::WSub);
                            let old_d = BytecodeProgram::from_single_code(Var::E.into())
                                .extend(prev_iter_t1.clone())
                                .add(Bytecode::WSub);
                            let ch = BytecodeProgram::from_code(Vec::from([
                                Var::E.into(),
                                Var::F.into(),
                                Var::HTemp.into(),
                                Bytecode::Ch,
                            ]));
                            let old_h_temp = BytecodeProgram::from_code(Vec::from([
                                Var::T1.into(),
                                Var::I.into(),
                                Bytecode::AvLoadCtx(Var::W),
                                Bytecode::WSub,
                                Var::I.into(),
                                Bytecode::AvLoadCtx(Var::K),
                                Bytecode::WSub,
                            ]))
                            .extend(ch)
                            .extend(BytecodeProgram::from_code(Vec::from([
                                Bytecode::WSub,
                                Bytecode::Context(Var::E),
                                Bytecode::BigSigma1,
                                Bytecode::WSub,
                            ])));
                            let constraint = Constraint::Equals(
                                BytecodeProgram::from_single_code(Bytecode::Input(0)),
                                val,
                            );
                            let constraint = constraint.map_context(vec![
                                (
                                    Var::I,
                                    BytecodeProgram::from_code(Vec::from([
                                        Var::I.into(),
                                        Bytecode::Const(1),
                                        Bytecode::WSub,
                                    ])),
                                ),
                                (Var::A, Var::B.into()),
                                (Var::B, Var::C.into()),
                                (Var::C, Var::D.into()),
                                (Var::D, old_d),
                                (Var::E, Var::F.into()),
                                (Var::G, Var::HTemp.into()),
                                (Var::HTemp, old_h_temp),
                                (Var::T1, prev_iter_t1),
                            ]);
                            let Constraints::Ass(ass6) = &mut x.ass6 else {
                                todo!()
                            };
                            ass6.constraints.push(constraint);
                            return RevRes::ConstraintsChanged(id);
                        }
                        _ => todo!("{:?}", constraints_holder.get(&id.back().back())),
                    }
                }
                todo!()
            }
            _ => todo!("{}", constraints.constraints.len()),
        },
        6 => match constraints.constraints.len() {
            0 => {}
            1 => {
                let val = {
                    let constraint = &constraints.constraints[0];
                    let Constraint::Equals(val1, val2) = constraint;
                    assert!(val1.is_input(0));
                    val2
                }
                .clone();
                if val.execute(&context).to_u32() != output {
                    match constraints_holder.get(&id.back().back()) {
                        Constraints::Step2(x) => {
                            let prev_iter_t2 = BytecodeProgram::from_code(Vec::from([
                                Var::B.into(),
                                Bytecode::BigSigma0,
                                Var::B.into(),
                                Var::C.into(),
                                Var::D.into(),
                                Bytecode::Maj,
                                Bytecode::WAdd,
                            ]));
                            let prev_iter_t1 = BytecodeProgram::from_single_code(Var::A.into())
                                .extend(prev_iter_t2)
                                .add(Bytecode::WSub);
                            let old_d = BytecodeProgram::from_single_code(Var::E.into())
                                .extend(prev_iter_t1.clone())
                                .add(Bytecode::WSub);
                            let big_sigma1 = BytecodeProgram::from_code(Vec::from([
                                Var::E.into(),
                                Bytecode::BigSigma1,
                            ]));
                            let old_h_temp = BytecodeProgram::from_single_code(Var::T1.into())
                                .extend(big_sigma1.clone())
                                .add(Bytecode::WSub);
                            let constraint = Constraint::Equals(
                                BytecodeProgram::from_single_code(Bytecode::Input(0)),
                                val,
                            );
                            let constraint = constraint.map_context(vec![
                                (
                                    Var::I,
                                    BytecodeProgram::from_code(Vec::from([
                                        Var::I.into(),
                                        Bytecode::Const(1),
                                        Bytecode::WSub,
                                    ])),
                                ),
                                (Var::A, Var::B.into()),
                                (Var::B, Var::C.into()),
                                (Var::C, Var::D.into()),
                                (Var::D, old_d),
                                (Var::E, Var::F.into()),
                                (Var::F, Var::G.into()),
                                (Var::HTemp, old_h_temp),
                                (Var::T1, prev_iter_t1),
                            ]);
                            x.w_add7.to_w_add().constraints.push(constraint);
                            return RevRes::ConstraintsChanged(id);
                        }
                        _ => todo!("{:?}", constraints_holder.get(&id.back().back())),
                    }
                }
                todo!()
            }
            _ => todo!("{}", constraints.constraints.len()),
        },
        _ => todo!("{count}"),
    }
    output.into()
}

#[derive(Debug, Default)]
pub struct AssignmentConstraints {
    constraints: Vec<Constraint>,
}

pub fn rev_wrapping_add(
    output: u32,
    first_in: Option<u32>,
    second_in: Option<BytecodeProgram>,
    constraints_holder: &mut ConstraintsHolder,
    id: ConstraintId,
    count: usize,
    context: Context,
) -> RevRes<(u32, u32)> {
    if let Some(first_in) = first_in {
        todo!()
    }
    if let Some(raw_second_in) = second_in {
        let second_in = raw_second_in.execute(&context).to_u32();
        match constraints_holder.get(&id.back()) {
            Constraints::Step2(x) => match count {
                2 => {
                    let constraints = &mut x.w_add2.to_w_add();
                    match constraints.constraints.len() {
                        0 => {}
                        1 => {
                            let val1 = {
                                let constraint = &constraints.constraints[0];
                                let Constraint::Equals(val1, val2) = constraint;
                                assert!(val1.is_input(0));
                                val2
                            }
                            .clone();

                            if val1.execute(&context).to_u32().wrapping_add(second_in) != output {
                                match constraints_holder.get(&id.back().back()) {
                                    Constraints::Step2(x) => {
                                        let constraint = Constraint::Equals(
                                            BytecodeProgram::from_single_code(Bytecode::Input(0)),
                                            val1.clone().extend(raw_second_in).add(Bytecode::WAdd),
                                        );
                                        let prev_iter_t2 = BytecodeProgram::from_code(Vec::from([
                                            Var::B.into(),
                                            Bytecode::BigSigma0,
                                            Var::B.into(),
                                            Var::C.into(),
                                            Var::D.into(),
                                            Bytecode::Maj,
                                            Bytecode::WAdd,
                                        ]));
                                        let prev_iter_t1 =
                                            BytecodeProgram::from_single_code(Var::A.into())
                                                .extend(prev_iter_t2)
                                                .add(Bytecode::WSub);
                                        let old_d =
                                            BytecodeProgram::from_single_code(Var::F.into())
                                                .extend(prev_iter_t1.clone())
                                                .add(Bytecode::WSub);
                                        let ch = BytecodeProgram::from_code(Vec::from([
                                            Var::F.into(),
                                            Var::G.into(),
                                            Var::HTemp.into(),
                                            Bytecode::Ch,
                                        ]));
                                        let old_t1 = BytecodeProgram::from_code(Vec::from([
                                            Var::T1.into(),
                                            Var::I.into(),
                                            Bytecode::AvLoadCtx(Var::W),
                                            Bytecode::WSub,
                                            Var::I.into(),
                                            Bytecode::AvLoadCtx(Var::K),
                                            Bytecode::WSub,
                                        ]))
                                        .extend(ch)
                                        .add(Bytecode::WSub);
                                        let big_sigma1 = BytecodeProgram::from_code(Vec::from([
                                            Var::F.into(),
                                            Bytecode::BigSigma1,
                                        ]));
                                        let old_h_temp =
                                            old_t1.extend(big_sigma1).add(Bytecode::WSub);
                                        let constraint = constraint.map_context(vec![
                                            (Var::A, Var::B.into()),
                                            (Var::B, Var::C.into()),
                                            (Var::C, Var::D.into()),
                                            (Var::D, old_d),
                                            (Var::F, Var::G.into()),
                                            (Var::E, Var::F.into()),
                                            (Var::G, Var::HTemp.into()),
                                            (Var::HTemp, old_h_temp),
                                            (
                                                Var::I,
                                                BytecodeProgram::from_code(Vec::from([
                                                    Var::I.into(),
                                                    Bytecode::Const(1),
                                                    Bytecode::WSub,
                                                ])),
                                            ),
                                            (Var::T1, prev_iter_t1),
                                        ]);
                                        let Constraints::Ass(ass4) = &mut x.ass4 else {
                                            todo!()
                                        };
                                        ass4.constraints.push(constraint);
                                        return RevRes::ConstraintsChanged(id);
                                    }
                                    _ => todo!("{:?}", constraints_holder.get(&id.back().back())),
                                }
                            }
                            todo!()
                        }
                        _ => todo!("{}", constraints.constraints.len()),
                    }
                }
                4 => {
                    let constraints = &mut x.w_add4.to_w_add();
                    match constraints.constraints.len() {
                        0 => {}
                        1 => {
                            let val1 = {
                                let constraint = &constraints.constraints[0];
                                let Constraint::Equals(val1, val2) = constraint;
                                assert!(val1.is_input(0));
                                val2
                            }
                            .clone();
                            if val1.execute(&context).to_u32().wrapping_add(second_in) != output {
                                match constraints_holder.get(&id.back()) {
                                    Constraints::Step2(x) => {
                                        let ch = BytecodeProgram::from_code(Vec::from([
                                            Var::F.into(),
                                            Var::G.into(),
                                            Var::HTemp.into(),
                                            Bytecode::Ch,
                                        ]));
                                        let t2 = BytecodeProgram::from_code(Vec::from([
                                            Var::B.into(),
                                            Bytecode::BigSigma0,
                                            Var::B.into(),
                                            Var::C.into(),
                                            Var::D.into(),
                                            Bytecode::Maj,
                                            Bytecode::WAdd,
                                        ]));
                                        let t1_4 = BytecodeProgram::from_single_code(Var::A.into())
                                            .extend(t2)
                                            .add(Bytecode::WSub);
                                        let t1_1 = t1_4
                                            .clone()
                                            .extend(BytecodeProgram::from_code(Vec::from([
                                                Var::I.into(),
                                                Bytecode::AvLoadCtx(Var::W),
                                                Bytecode::WSub,
                                                Var::I.into(),
                                                Bytecode::AvLoadCtx(Var::K),
                                                Bytecode::WSub,
                                            ])))
                                            .extend(ch)
                                            .add(Bytecode::WSub);
                                        let big_sigma1 = BytecodeProgram::from_code(Vec::from([
                                            Var::F.into(),
                                            Bytecode::BigSigma1,
                                        ]));
                                        let old_h_temp =
                                            t1_1.extend(big_sigma1).add(Bytecode::WSub);
                                        let old_d =
                                            BytecodeProgram::from_single_code(Var::E.into())
                                                .extend(t1_4.clone())
                                                .add(Bytecode::WSub);
                                        let constraint = Constraint::Equals(
                                            BytecodeProgram::from_single_code(Bytecode::Input(0)),
                                            val1.extend(raw_second_in).add(Bytecode::WAdd),
                                        )
                                        .map_context(vec![
                                            (Var::A, Var::B.into()),
                                            (Var::B, Var::C.into()),
                                            (Var::C, Var::D.into()),
                                            (Var::D, old_d),
                                            (Var::E, Var::F.into()),
                                            (Var::F, Var::G.into()),
                                            (Var::G, Var::HTemp.into()),
                                            (Var::HTemp, old_h_temp),
                                            (Var::T1, t1_4),
                                        ]);
                                        x.w_add1.to_w_add().constraints.push(constraint);
                                        return RevRes::ConstraintsChanged(id);
                                    }
                                    _ => todo!("{:?}", constraints_holder.get(&id.back())),
                                }
                            }
                            todo!()
                        }
                        _ => todo!("{}", constraints.constraints.len()),
                    }
                }
                5 => {
                    let constraints = &mut x.w_add5.to_w_add();
                    match constraints.constraints.len() {
                        0 => {}
                        1 => {
                            let val1 = {
                                let constraint = &constraints.constraints[0];
                                let Constraint::Equals(val1, val2) = constraint;
                                assert!(val1.is_input(0));
                                val2
                            }
                            .clone();
                            if val1.execute(&context).to_u32().wrapping_add(second_in) != output {
                                match constraints_holder.get(&id.back()) {
                                    Constraints::Step2(x) => {
                                        let constraint = Constraint::Equals(
                                            BytecodeProgram::from_single_code(Bytecode::Input(0)),
                                            val1.extend(raw_second_in).add(Bytecode::WAdd),
                                        );
                                        let t1_3 = BytecodeProgram::from_code(Vec::from([
                                            Var::T1.into(),
                                            Var::I.into(),
                                            Bytecode::AvLoadCtx(Var::W),
                                            Bytecode::WSub,
                                        ]));
                                        let constraint =
                                            constraint.map_context(vec![(Var::T1, t1_3)]);
                                        x.w_add4.to_w_add().constraints.push(constraint);
                                        return RevRes::ConstraintsChanged(id);
                                    }
                                    _ => todo!("{:?}", constraints_holder.get(&id.back())),
                                }
                            }
                            todo!()
                        }
                        _ => todo!("{}", constraints.constraints.len()),
                    }
                }
                _ => todo!("{count}"),
            },
            _ => todo!("{:?}", constraints_holder.get(&id.back())),
        }
        return (output.wrapping_sub(second_in), second_in).into();
    }
    match constraints_holder.get(&id.back()) {
        Constraints::Step1(x) => assert!(x.w_add.constraints.is_empty()),
        Constraints::Step2(x) => match count {
            1 => {
                let constraints = &mut x.w_add1.to_w_add();
                match constraints.constraints.len() {
                    0 => {}
                    1 => {
                        let val2 = {
                            let constraint = &constraints.constraints[0];
                            let Constraint::Equals(val1, val2) = constraint;
                            assert!(val1.is_input(1));
                            val2.execute(&context).into()
                        };
                        return (output.wrapping_sub(val2), val2).into();
                    }
                    2 => {
                        let val2 = {
                            let constraint = &constraints.constraints[0];
                            let Constraint::Equals(val1, val2) = constraint;
                            assert!(val1.is_input(1));
                            val2
                        }
                        .clone();
                        let val1 = {
                            let constraint = &constraints.constraints[1];
                            let Constraint::Equals(val1, val2) = constraint;
                            assert!(val1.is_input(0));
                            val2
                        }
                        .clone();

                        if val1
                            .execute(&context)
                            .to_u32()
                            .wrapping_add(val2.execute(&context).into())
                            != output
                        {
                            match constraints_holder.get(&id.back().back()) {
                                Constraints::Step2(x) => {
                                    let old_d = BytecodeProgram::from_code(Vec::from([
                                        Var::E.into(),
                                        Var::T1.into(),
                                        Bytecode::WSub,
                                    ]));
                                    let ch = BytecodeProgram::from_code(Vec::from([
                                        Var::F.into(),
                                        Var::G.into(),
                                        Var::HTemp.into(),
                                        Bytecode::Ch,
                                    ]));
                                    let old_t1 = BytecodeProgram::from_code(Vec::from([
                                        Var::T1.into(),
                                        Var::I.into(),
                                        Bytecode::AvLoadCtx(Var::W),
                                        Bytecode::WSub,
                                        Var::I.into(),
                                        Bytecode::AvLoadCtx(Var::K),
                                        Bytecode::WSub,
                                    ]))
                                    .extend(ch)
                                    .add(Bytecode::WSub);
                                    let big_sigma1 = BytecodeProgram::from_code(Vec::from([
                                        Var::F.into(),
                                        Bytecode::BigSigma1,
                                    ]));
                                    let old_h_temp = old_t1.extend(big_sigma1).add(Bytecode::WSub);
                                    let constraint = Constraint::Equals(
                                        BytecodeProgram::from_single_code(Bytecode::Input(0)),
                                        val1.extend(val2).add(Bytecode::WAdd),
                                    );

                                    let constraint = constraint.map_context(vec![
                                        (Var::A, Var::B.into()),
                                        (Var::B, Var::C.into()),
                                        (Var::C, Var::D.into()),
                                        (Var::D, old_d),
                                        (Var::E, Var::F.into()),
                                        (Var::F, Var::G.into()),
                                        (Var::G, Var::HTemp.into()),
                                        (Var::HTemp, old_h_temp),
                                        (
                                            Var::I,
                                            BytecodeProgram::from_code(Vec::from([
                                                Var::I.into(),
                                                Bytecode::Const(1),
                                                Bytecode::WSub,
                                            ])),
                                        ),
                                    ]);
                                    let Constraints::Ass(ass1) = &mut x.ass1 else {
                                        todo!()
                                    };
                                    ass1.constraints.push(constraint);
                                    return RevRes::ConstraintsChanged(id);
                                }
                                _ => todo!("{:?}", constraints_holder.get(&id.back().back())),
                            }
                        }
                        todo!()
                    }
                    _ => todo!("{}", constraints.constraints.len()),
                }
            }
            3 => {
                let constraints = &mut x.w_add3.to_w_add();
                match constraints.constraints.len() {
                    0 => {}
                    1 => {
                        let val2 = {
                            let constraint = &constraints.constraints[0];
                            let Constraint::Equals(val1, val2) = constraint;
                            assert!(val1.is_input(1));
                            val2.execute(&context).into()
                        };
                        return (output.wrapping_sub(val2), val2).into();
                    }
                    2 => {
                        let val2 = {
                            let constraint = &constraints.constraints[0];
                            let Constraint::Equals(val1, val2) = constraint;
                            assert!(val1.is_input(1));
                            val2
                        }
                        .clone();

                        let val1 = {
                            let constraint = &constraints.constraints[1];
                            let Constraint::Equals(val1, val2) = constraint;
                            assert!(val1.is_input(0));
                            val2
                        }
                        .clone();

                        if val1
                            .execute(&context)
                            .to_u32()
                            .wrapping_add(val2.execute(&context).into())
                            != output
                        {
                            match constraints_holder.get(&id.back()) {
                                Constraints::Step2(x) => {
                                    let constraint = Constraint::Equals(
                                        BytecodeProgram::from_single_code(Bytecode::Input(1)),
                                        val1.extend(val2).add(Bytecode::WAdd),
                                    );
                                    let t2 = BytecodeProgram::from_code(Vec::from([
                                        Var::B.into(),
                                        Bytecode::BigSigma0,
                                        Var::B.into(),
                                        Var::C.into(),
                                        Var::D.into(),
                                        Bytecode::Maj,
                                        Bytecode::WAdd,
                                    ]));
                                    let t1_4 = BytecodeProgram::from_single_code(Var::A.into())
                                        .extend(t2)
                                        .add(Bytecode::WSub);
                                    let old_d = BytecodeProgram::from_single_code(Var::E.into())
                                        .extend(t1_4)
                                        .add(Bytecode::WSub);
                                    let constraint = constraint.map_context(vec![
                                        (Var::A, Var::B.into()),
                                        (Var::B, Var::C.into()),
                                        (Var::C, Var::D.into()),
                                        (Var::D, old_d),
                                        (Var::E, Var::F.into()),
                                        (Var::F, Var::G.into()),
                                        (Var::G, Var::HTemp.into()),
                                    ]);
                                    x.w_add1.to_w_add().constraints.push(constraint);
                                    return RevRes::ConstraintsChanged(id);
                                }
                                _ => todo!("{:?}", constraints_holder.get(&id.back())),
                            }
                        }

                        return (
                            val1.execute(&context).to_u32(),
                            val2.execute(&context).to_u32(),
                        )
                            .into();
                    }
                    _ => todo!("{}", constraints.constraints.len()),
                }
            }
            6 => {
                let constraints = &mut x.w_add6.to_w_add();
                match constraints.constraints.len() {
                    0 => {}
                    1 => {
                        let val2: u32 = {
                            let constraint = &constraints.constraints[0];
                            let Constraint::Equals(val1, val2) = constraint;
                            assert!(val1.is_input(1));
                            val2.execute(&context).into()
                        };
                        return (output.wrapping_sub(val2), val2).into();
                    }
                    2 => {
                        let val2 = {
                            let constraint = &constraints.constraints[0];
                            let Constraint::Equals(val1, val2) = constraint;
                            assert!(val1.is_input(1));
                            val2
                        }
                        .clone();
                        let val1 = {
                            let constraint = &constraints.constraints[1];
                            let Constraint::Equals(val1, val2) = constraint;
                            assert!(val1.is_input(0));
                            val2
                        }
                        .clone();
                        if val1
                            .execute(&context)
                            .to_u32()
                            .wrapping_add(val2.execute(&context).into())
                            != output
                        {
                            match constraints_holder.get(&id.back()) {
                                Constraints::Step2(x) => {
                                    let constraint = Constraint::Equals(
                                        BytecodeProgram::from_single_code(Bytecode::Input(0)),
                                        val1.extend(val2).add(Bytecode::WAdd),
                                    );
                                    let t1_2 = BytecodeProgram::from_code(Vec::from([
                                        Var::T1.into(),
                                        Var::I.into(),
                                        Bytecode::AvLoadCtx(Var::K.into()),
                                        Bytecode::WSub,
                                    ]));
                                    let constraint = constraint.map_context(vec![(Var::T1, t1_2)]);
                                    x.w_add5.to_w_add().constraints.push(constraint);
                                    return RevRes::ConstraintsChanged(id);
                                }
                                _ => todo!("{:?}", constraints_holder.get(&id.back())),
                            }
                        }
                        todo!()
                    }
                    _ => todo!("{}", constraints.constraints.len()),
                }
            }
            7 => {
                let constraints = &mut x.w_add7.to_w_add();
                match constraints.constraints.len() {
                    0 => {}
                    1 => {
                        let val2: u32 = {
                            let constraint = &constraints.constraints[0];
                            let Constraint::Equals(val1, val2) = constraint;
                            assert!(val1.is_input(1));
                            val2.execute(&context).into()
                        };
                        return (output.wrapping_sub(val2), val2).into();
                    }
                    2 => {
                        let val2 = {
                            let constraint = &constraints.constraints[0];
                            let Constraint::Equals(val1, val2) = constraint;
                            assert!(val1.is_input(1));
                            val2
                        }
                        .clone();
                        let val1 = {
                            let constraint = &constraints.constraints[1];
                            let Constraint::Equals(val1, val2) = constraint;
                            assert!(val1.is_input(0));
                            val2
                        }
                        .clone();
                        if val1
                            .execute(&context)
                            .to_u32()
                            .wrapping_add(val2.execute(&context).into())
                            != output
                        {
                            match constraints_holder.get(&id.back()) {
                                Constraints::Step2(x) => {
                                    let constraint = Constraint::Equals(
                                        BytecodeProgram::from_single_code(Bytecode::Input(0)),
                                        val1.extend(val2).add(Bytecode::WAdd),
                                    );
                                    let ch = BytecodeProgram::from_code(Vec::from([
                                        Var::E.into(),
                                        Var::F.into(),
                                        Var::G.into(),
                                        Bytecode::Ch,
                                    ]));
                                    let old_t1 = BytecodeProgram::from_single_code(Var::T1.into())
                                        .extend(ch)
                                        .add(Bytecode::WSub);
                                    let constraint =
                                        constraint.map_context(vec![(Var::T1, old_t1)]);
                                    unsafe {
                                        if COUNTER == 3 {
                                            println!("{constraint}");
                                            std::process::exit(0);
                                        }
                                        COUNTER += 1;
                                    }
                                    x.w_add6.to_w_add().constraints.push(constraint);
                                    return RevRes::ConstraintsChanged(id);
                                }
                                _ => todo!("{:?}", constraints_holder.get(&id.back())),
                            }
                        }
                        todo!()
                    }
                    _ => todo!("{}", constraints.constraints.len()),
                }
            }
            _ => todo!("{count}"),
        },
        _ => todo!("{:?}", constraints_holder.get(&id.back())),
    }
    for a in 0_u32..=u32::MAX {
        let b = output.wrapping_sub(a);
        return (a, b).into();
    }
    unreachable!()
}

#[derive(Default, Debug)]
pub struct WrappingAddConstraints {
    pub constraints: Vec<Constraint>,
}

pub fn rev_maj(
    output: u32,
    first_in: Option<u32>,
    second_in: Option<u32>,
    third_in: Option<u32>,
    constraints_holder: &mut ConstraintsHolder,
    id: ConstraintId,
) -> RevRes<(u32, u32, u32)> {
    if let Some(first_in) = first_in {
        if let Some(second_in) = second_in {
            if let Some(third_in) = third_in {
                if maj(first_in, second_in, third_in) != output {
                    match constraints_holder.get(&id.back()) {
                        Constraints::Step2(x) => {
                            let constraint = Constraint::Equals(
                                Bytecode::Input(1).into(),
                                BytecodeProgram::from_code(Vec::from([
                                    Bytecode::Context(Var::A),
                                    Bytecode::Context(Var::B),
                                    Bytecode::Context(Var::C),
                                    Bytecode::Maj,
                                ])),
                            );
                            x.w_add3.to_w_add().constraints.push(constraint);
                            return RevRes::ConstraintsChanged(id);
                        }
                        _ => todo!("{:?}", constraints_holder.get(&id.back())),
                    }
                }
                return (first_in, second_in, third_in).into();
            }
            todo!()
        }
        todo!()
    }

    let (res, b) = rev_xor(output, 1);
    let RevRes::Normal((y, z)) = rev_and(
        b,
        None,
        None,
        constraints_holder,
        id.next(1),
        Context::new(vec![]),
        1,
    ) else {
        return RevRes::ConstraintsChanged(id);
    };
    let (res, a) = rev_xor(res, 2);
    // x == res & y
    let RevRes::Normal((x, _z)) = rev_and(
        a,
        None,
        Some(z),
        constraints_holder,
        id.next(3),
        Context::new(vec![(Var::Y, y.into())]),
        2,
    ) else {
        return RevRes::ConstraintsChanged(id);
    };
    let RevRes::Normal((_x, _y)) = rev_and(
        res,
        Some(x),
        Some(y),
        constraints_holder,
        id.next(4),
        Context::new(vec![]),
        3,
    ) else {
        return RevRes::ConstraintsChanged(id);
    };
    (x, y, z).into()
}

#[derive(Debug, Default)]
pub struct MajConstraints {
    xor1: XorConstraints,
    and1: AndConstraints,
    xor2: XorConstraints,
    and2: AndConstraints,
    and3: AndConstraints,
}

pub fn rev_xor(output: u32, count: usize) -> (u32, u32) {
    for a in 0u32..=u32::MAX {
        let b = a ^ output;
        return (a, b);
    }
    unreachable!()
}

#[derive(Debug, Default)]
pub struct XorConstraints {
    constraints: Vec<Constraint>,
}

pub fn rev_and(
    output: u32,
    first_in: Option<u32>,
    second_in: Option<u32>,
    constraints_holder: &mut ConstraintsHolder,
    id: ConstraintId,
    context: Context,
    count: usize,
) -> RevRes<(u32, u32)> {
    let constraints = match constraints_holder.get(&id.back()) {
        Constraints::Maj(x) => match count {
            1 => &mut x.and1,
            2 => &mut x.and2,
            3 => &mut x.and3,
            _ => todo!(),
        },
        Constraints::Ch(x) => match count {
            1 => &mut x.and1,
            2 => &mut x.and2,
            _ => todo!(),
        },
        _ => todo!("{:?}", constraints_holder.get(&id.back())),
    };
    if let Some(first_in) = first_in {
        if let Some(second_in) = second_in {
            if first_in & second_in != output {
                match constraints_holder.get(&id.back()) {
                    Constraints::Maj(x) => match count {
                        3 => {
                            let constraint = Constraint::Equals(
                                Bytecode::Input(0).into(),
                                BytecodeProgram::from_code(Vec::from([
                                    Bytecode::Context(Var::X),
                                    Bytecode::Context(Var::Y),
                                    Bytecode::And,
                                ])),
                            );
                            todo!("{constraint}");
                            // map it so it doesnt use x, because it doesnt exists
                            x.xor2.constraints.push(constraint);
                            return RevRes::ConstraintsChanged(id);
                        }
                        _ => todo!(),
                    },
                    Constraints::Step2(x) => todo!(),
                    _ => todo!("{:?}", constraints_holder.get(&id.back())),
                }
            }
            return (first_in, second_in).into();
        }

        if output & !first_in != 0 {
            todo!("No solutions");
        }
        let mut sub = !first_in;
        loop {
            let a = first_in;
            let b = output | sub;
            // todo: check constraints
            return (a, b).into();
            if sub == 0 {
                break;
            }
            sub = (sub - 1) & !first_in;
        }
        todo!()
    }
    if let Some(second_in) = second_in {
        if (second_in & output) != output {
            todo!("No solutions");
        }
        let mut sub = !second_in;
        loop {
            let a = output | sub;
            let b = second_in;
            println!("{}", constraints.constraints.len());
            if constraints.constraints.iter().all(|x| todo!()) {
                return (a, b).into();
            }
            if sub == 0 {
                break;
            }
            sub = (sub - 1) & !second_in;
        }
    }
    let not_x = !output;
    let mut t = not_x;
    loop {
        let a = output | t;
        let free = not_x & !t;
        let mut s = free;
        loop {
            let b = output | s;
            // todo check constraints
            return (a, b).into();
            if s == 0 {
                break;
            }
            s = (s - 1) & free;
        }
        if t == 0 {
            break;
        }
        t = (t - 1) & not_x;
    }
    unreachable!()
}

#[derive(Default, Debug)]
pub struct AndConstraints {
    constraints: Vec<Constraint>,
}
