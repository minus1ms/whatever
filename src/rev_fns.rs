use crate::{
    constraint::{
        Constraint, ConstraintId, Constraints, ConstraintsHolder, ContextVal, Operation, RevVal,
    },
    rev_val_holder::LazyRevVal,
    reverse::Context,
    sha256::{big_sigma0, big_sigma1, ch, maj},
    utils::RevRes,
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
                        RevVal::Input(0).into(),
                        Operation::BigSigma0("a".into()).into(),
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
                        RevVal::Input(1).into(),
                        Operation::BigSigma1("e".into()).into(),
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
                                RevVal::Input(1).into(),
                                Operation::Ch("e".into(), "f".into(), "g".into()).into(),
                            );
                            let t1_1 = Operation::WAdd(
                                "h_temp".into(),
                                Operation::BigSigma1("e".into()).into(),
                            )
                            .into();
                            let old_t1 = Operation::WSub("t1".into(), t1_1).into();
                            let constraint = constraint.map_context(vec![("t1", old_t1)]);
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
                if val.evaluate(&context).to_u32() != output {
                    match constraints_holder.get(&id.back().back()) {
                        Constraints::Step2(x) => {
                            let old_d: LazyRevVal = Operation::WSub("e".into(), "t1".into()).into();
                            let ch = Operation::Ch("f".into(), "g".into(), "h_temp".into()).into();
                            let old_h_temp = Operation::WSub(
                                Operation::WSub(
                                    Operation::WSub(
                                        Operation::WSub(
                                            "t1".into(),
                                            ContextVal::ArrayCtx("w", "i".into()).into(),
                                        )
                                        .into(),
                                        ContextVal::ArrayCtx("K", "i".into()).into(),
                                    )
                                    .into(),
                                    ch,
                                )
                                .into(),
                                Operation::BigSigma1("f".into()).into(),
                            )
                            .into();
                            let prev_iter_t2 = Operation::WAdd(
                                Operation::BigSigma0("c".into()).into(),
                                Operation::Maj("c".into(), "d".into(), old_d.clone()).into(),
                            )
                            .into();
                            let prev_iter_t1: LazyRevVal =
                                Operation::WSub("a".into(), prev_iter_t2).into();
                            let constraint = Constraint::Equals(RevVal::Input(0).into(), val);
                            let constraint = constraint.map_context(vec![
                                (
                                    "i",
                                    RevVal::Operation(
                                        Operation::WSub("i".into(), RevVal::Const(1).into()).into(),
                                    )
                                    .into(),
                                ),
                                ("b", "c".into()),
                                ("c", "d".into()),
                                ("d", old_d),
                                ("e", "f".into()),
                                ("f", "g".into()),
                                ("g", "h_temp".into()),
                                ("h_temp", old_h_temp),
                                ("t1", prev_iter_t1),
                            ]);
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

                if val.evaluate(&context).to_u32() != output {
                    match constraints_holder.get(&id.back().back()) {
                        Constraints::Step2(x) => {
                            let old_d: LazyRevVal = Operation::WSub("e".into(), "t1".into()).into();
                            let ch = Operation::Ch("f".into(), "g".into(), "h_temp".into()).into();
                            let old_h_temp: LazyRevVal = Operation::WSub(
                                Operation::WSub(
                                    Operation::WSub(
                                        Operation::WSub(
                                            "t1".into(),
                                            ContextVal::ArrayCtx("w", "i".into()).into(),
                                        )
                                        .into(),
                                        ContextVal::ArrayCtx("K", "i".into()).into(),
                                    )
                                    .into(),
                                    ch,
                                )
                                .into(),
                                Operation::BigSigma1("f".into()).into(),
                            )
                            .into();
                            let prev_iter_t2 = Operation::WAdd(
                                Operation::BigSigma0("b".into()).into(),
                                Operation::Maj("b".into(), "d".into(), old_d.clone()).into(),
                            )
                            .into();
                            let prev_iter_t1: LazyRevVal =
                                Operation::WSub("a".into(), prev_iter_t2).into();
                            let constraint = Constraint::Equals(RevVal::Input(0).into(), val);
                            let constraint = constraint.map_context(vec![
                                (
                                    "i",
                                    RevVal::Operation(
                                        Operation::WSub("i".into(), RevVal::Const(1).into()).into(),
                                    )
                                    .into(),
                                ),
                                ("a", "b".into()),
                                ("c", "d".into()),
                                ("d", old_d),
                                ("e", "f".into()),
                                ("f", "g".into()),
                                ("g", "h_temp".into()),
                                ("h_temp", old_h_temp),
                                ("t1", prev_iter_t1),
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

                if val.evaluate(&context).to_u32() != output {
                    match constraints_holder.get(&id.back().back()) {
                        Constraints::Step2(x) => {
                            let old_d: LazyRevVal = Operation::WSub("e".into(), "t1".into()).into();
                            let ch = Operation::Ch("f".into(), "g".into(), "h_temp".into()).into();
                            let old_h_temp = Operation::WSub(
                                Operation::WSub(
                                    Operation::WSub(
                                        Operation::WSub(
                                            "t1".into(),
                                            ContextVal::ArrayCtx("w", "i".into()).into(),
                                        )
                                        .into(),
                                        ContextVal::ArrayCtx("K", "i".into()).into(),
                                    )
                                    .into(),
                                    ch,
                                )
                                .into(),
                                Operation::BigSigma1("f".into()).into(),
                            )
                            .into();
                            let prev_iter_t2 = Operation::WAdd(
                                Operation::BigSigma0("b".into()).into(),
                                Operation::Maj("b".into(), "c".into(), old_d.clone()).into(),
                            )
                            .into();
                            let prev_iter_t1 = Operation::WSub("a".into(), prev_iter_t2).into();
                            let constraint = Constraint::Equals(RevVal::Input(0).into(), val);
                            let constraint = constraint.map_context(vec![
                                (
                                    "i",
                                    RevVal::Operation(
                                        Operation::WSub("i".into(), RevVal::Const(1).into()).into(),
                                    )
                                    .into(),
                                ),
                                ("a", "b".into()),
                                ("b", "c".into()),
                                ("d", old_d),
                                ("e", "f".into()),
                                ("f", "g".into()),
                                ("g", "h_temp".into()),
                                ("h_temp", old_h_temp),
                                ("t1", prev_iter_t1),
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

                if val.evaluate(&context).to_u32() != output {
                    match constraints_holder.get(&id.back().back()) {
                        Constraints::Step2(x) => {
                            let prev_iter_t2: LazyRevVal = Operation::WAdd(
                                Operation::BigSigma0("b".into()).into(),
                                Operation::Maj("b".into(), "c".into(), "d".into()).into(),
                            )
                            .into();
                            let prev_iter_t1: LazyRevVal =
                                Operation::WSub("a".into(), prev_iter_t2).into();
                            let old_d = Operation::WSub("e".into(), prev_iter_t1.clone()).into();
                            let ch = Operation::Ch("e".into(), "g".into(), "h_temp".into()).into();
                            let old_h_temp: LazyRevVal = Operation::WSub(
                                Operation::WSub(
                                    Operation::WSub(
                                        Operation::WSub(
                                            "t1".into(),
                                            ContextVal::ArrayCtx("w", "i".into()).into(),
                                        )
                                        .into(),
                                        ContextVal::ArrayCtx("K", "i".into()).into(),
                                    )
                                    .into(),
                                    ch,
                                )
                                .into(),
                                Operation::BigSigma1("e".into()).into(),
                            )
                            .into();
                            let constraint = Constraint::Equals(RevVal::Input(0).into(), val);
                            let constraint = constraint.map_context(vec![
                                (
                                    "i",
                                    RevVal::Operation(
                                        Operation::WSub("i".into(), RevVal::Const(1).into()).into(),
                                    )
                                    .into(),
                                ),
                                ("a", "b".into()),
                                ("b", "c".into()),
                                ("c", "d".into()),
                                ("d", old_d),
                                ("f", "g".into()),
                                ("g", "h_temp".into()),
                                ("h_temp", old_h_temp),
                                ("t1", prev_iter_t1),
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
                if val.evaluate(&context).to_u32() != output {
                    match constraints_holder.get(&id.back().back()) {
                        Constraints::Step2(x) => {
                            let prev_iter_t2: LazyRevVal = Operation::WAdd(
                                Operation::BigSigma0("b".into()).into(),
                                Operation::Maj("b".into(), "c".into(), "d".into()).into(),
                            )
                            .into();
                            let prev_iter_t1: LazyRevVal =
                                Operation::WSub("a".into(), prev_iter_t2).into();
                            let old_d = Operation::WSub("e".into(), prev_iter_t1.clone()).into();
                            let ch = Operation::Ch("e".into(), "f".into(), "h_temp".into()).into();
                            let old_h_temp: LazyRevVal = Operation::WSub(
                                Operation::WSub(
                                    Operation::WSub(
                                        Operation::WSub(
                                            "t1".into(),
                                            ContextVal::ArrayCtx("w", "i".into()).into(),
                                        )
                                        .into(),
                                        ContextVal::ArrayCtx("K", "i".into()).into(),
                                    )
                                    .into(),
                                    ch,
                                )
                                .into(),
                                Operation::BigSigma1("e".into()).into(),
                            )
                            .into();
                            let constraint = Constraint::Equals(RevVal::Input(0).into(), val);
                            let constraint = constraint.map_context(vec![
                                (
                                    "i",
                                    RevVal::Operation(
                                        Operation::WSub("i".into(), RevVal::Const(1).into()).into(),
                                    )
                                    .into(),
                                ),
                                ("a", "b".into()),
                                ("b", "c".into()),
                                ("c", "d".into()),
                                ("d", old_d),
                                ("e", "f".into()),
                                ("g", "h_temp".into()),
                                ("h_temp", old_h_temp),
                                ("t1", prev_iter_t1),
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
                if val.evaluate(&context).to_u32() != output {
                    match constraints_holder.get(&id.back().back()) {
                        Constraints::Step2(x) => {
                            let prev_iter_t2: LazyRevVal = Operation::WAdd(
                                Operation::BigSigma0("b".into()).into(),
                                Operation::Maj("b".into(), "c".into(), "d".into()).into(),
                            )
                            .into();
                            let prev_iter_t1: LazyRevVal =
                                Operation::WSub("a".into(), prev_iter_t2).into();
                            let old_d = Operation::WSub("e".into(), prev_iter_t1.clone()).into();
                            let big_sigma1 = Operation::BigSigma1("e".into()).into();
                            let old_h_temp = Operation::WSub("t1".into(), big_sigma1).into();
                            let constraint = Constraint::Equals(RevVal::Input(0).into(), val);
                            let constraint = constraint.map_context(vec![
                                (
                                    "i",
                                    RevVal::Operation(
                                        Operation::WSub("i".into(), RevVal::Const(1).into()).into(),
                                    )
                                    .into(),
                                ),
                                ("a", "b".into()),
                                ("b", "c".into()),
                                ("c", "d".into()),
                                ("d", old_d),
                                ("e", "f".into()),
                                ("f", "g".into()),
                                ("h_temp", old_h_temp),
                                ("t1", prev_iter_t1),
                            ]);
                            unsafe {
                                if COUNTER == 1 {
                                    // println!("{constraint}");
                                    std::process::exit(0);
                                }
                                COUNTER += 1;
                            }
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
    second_in: Option<LazyRevVal>,
    constraints_holder: &mut ConstraintsHolder,
    id: ConstraintId,
    count: usize,
    context: Context,
) -> RevRes<(u32, u32)> {
    if let Some(first_in) = first_in {
        todo!()
    }
    if let Some(raw_second_in) = second_in {
        let second_in = raw_second_in.evaluate(&context).to_u32();
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

                            if val1.evaluate(&context).to_u32().wrapping_add(second_in) != output {
                                match constraints_holder.get(&id.back().back()) {
                                    Constraints::Step2(x) => {
                                        let constraint = Constraint::Equals(
                                            RevVal::Input(0).into(),
                                            Operation::WAdd(val1, raw_second_in).into(),
                                        );
                                        let prev_iter_t2 = Operation::WAdd(
                                            Operation::BigSigma0("b".into()).into(),
                                            Operation::Maj("b".into(), "c".into(), "d".into())
                                                .into(),
                                        )
                                        .into();
                                        let prev_iter_t1: LazyRevVal =
                                            Operation::WSub("a".into(), prev_iter_t2).into();
                                        let old_d: LazyRevVal =
                                            Operation::WSub("f".into(), prev_iter_t1.clone())
                                                .into();
                                        let ch: LazyRevVal =
                                            Operation::Ch("f".into(), "g".into(), "h_temp".into())
                                                .into();
                                        let old_t1: LazyRevVal = Operation::WSub(
                                            Operation::WSub(
                                                Operation::WSub(
                                                    "t1".into(),
                                                    ContextVal::ArrayCtx("w", "i".into()).into(),
                                                )
                                                .into(),
                                                ContextVal::ArrayCtx("K", "i".into()).into(),
                                            )
                                            .into(),
                                            ch,
                                        )
                                        .into();
                                        let big_sigma1 = Operation::BigSigma1("f".into()).into();
                                        let old_h_temp = Operation::WSub(old_t1, big_sigma1).into();
                                        let constraint = constraint.map_context(vec![
                                            ("a", "b".into()),
                                            ("b", "c".into()),
                                            ("c", "d".into()),
                                            ("d", old_d),
                                            ("f", "g".into()),
                                            ("e", "f".into()),
                                            ("g", "h_temp".into()),
                                            ("h_temp", old_h_temp),
                                            (
                                                "i",
                                                RevVal::Operation(
                                                    Operation::WSub(
                                                        "i".into(),
                                                        RevVal::Const(1).into(),
                                                    )
                                                    .into(),
                                                )
                                                .into(),
                                            ),
                                            ("t1", prev_iter_t1),
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
                            if val1.evaluate(&context).to_u32().wrapping_add(second_in) != output {
                                match constraints_holder.get(&id.back()) {
                                    Constraints::Step2(x) => {
                                        let ch: LazyRevVal =
                                            Operation::Ch("f".into(), "g".into(), "h_temp".into())
                                                .into();
                                        let t2 = Operation::WAdd(
                                            Operation::BigSigma0("b".into()).into(),
                                            Operation::Maj("b".into(), "c".into(), "d".into())
                                                .into(),
                                        )
                                        .into();
                                        let t1_4: LazyRevVal =
                                            Operation::WSub("a".into(), t2).into();
                                        let t1_1: LazyRevVal = Operation::WSub(
                                            Operation::WSub(
                                                Operation::WSub(
                                                    t1_4.clone(),
                                                    ContextVal::ArrayCtx("w", "i".into()).into(),
                                                )
                                                .into(),
                                                ContextVal::ArrayCtx("K", "i".into()).into(),
                                            )
                                            .into(),
                                            ch,
                                        )
                                        .into();
                                        let big_sigma1 = Operation::BigSigma1("f".into()).into();
                                        let old_h_temp = Operation::WSub(t1_1, big_sigma1).into();
                                        let old_d =
                                            Operation::WSub("e".into(), t1_4.clone()).into();
                                        let constraint = Constraint::Equals(
                                            RevVal::Input(0).into(),
                                            Operation::WAdd(val1, raw_second_in).into(),
                                        )
                                        .map_context(vec![
                                            ("a", "b".into()),
                                            ("b", "c".into()),
                                            ("c", "d".into()),
                                            ("d", old_d),
                                            ("e", "f".into()),
                                            ("f", "g".into()),
                                            ("g", "h_temp".into()),
                                            ("h_temp", old_h_temp),
                                            ("t1", t1_4),
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
                            if val1.evaluate(&context).to_u32().wrapping_add(second_in) != output {
                                match constraints_holder.get(&id.back()) {
                                    Constraints::Step2(x) => {
                                        let constraint = Constraint::Equals(
                                            RevVal::Input(0).into(),
                                            Operation::WAdd(val1, raw_second_in).into(),
                                        );
                                        let t1_3 = Operation::WSub(
                                            "t1".into(),
                                            ContextVal::ArrayCtx("w", "i".into()).into(),
                                        )
                                        .into();
                                        let constraint = constraint.map_context(vec![("t1", t1_3)]);
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
                            val2.evaluate(&context).into()
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
                            .evaluate(&context)
                            .to_u32()
                            .wrapping_add(val2.evaluate(&context).into())
                            != output
                        {
                            match constraints_holder.get(&id.back().back()) {
                                Constraints::Step2(x) => {
                                    let old_d: LazyRevVal =
                                        Operation::WSub("e".into(), "t1".into()).into();
                                    let ch: LazyRevVal =
                                        Operation::Ch("f".into(), "g".into(), "h_temp".into())
                                            .into();
                                    let old_t1: LazyRevVal = Operation::WSub(
                                        Operation::WSub(
                                            Operation::WSub(
                                                "t1".into(),
                                                ContextVal::ArrayCtx("w", "i".into()).into(),
                                            )
                                            .into(),
                                            ContextVal::ArrayCtx("K", "i".into()).into(),
                                        )
                                        .into(),
                                        ch,
                                    )
                                    .into();
                                    let big_sigma1 = Operation::BigSigma1("f".into()).into();
                                    let old_h_temp = Operation::WSub(old_t1, big_sigma1).into();
                                    let constraint = Constraint::Equals(
                                        RevVal::Input(0).into(),
                                        Operation::WAdd(val1, val2).into(),
                                    );
                                    let constraint = constraint.map_context(vec![
                                        ("a", "b".into()),
                                        ("b", "c".into()),
                                        ("c", "d".into()),
                                        ("d", old_d),
                                        ("e", "f".into()),
                                        ("f", "g".into()),
                                        ("g", "h_temp".into()),
                                        ("h_temp", old_h_temp),
                                        (
                                            "i",
                                            RevVal::Operation(
                                                Operation::WSub(
                                                    "i".into(),
                                                    RevVal::Const(1).into(),
                                                )
                                                .into(),
                                            )
                                            .into(),
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
                            val2.evaluate(&context).into()
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
                            .evaluate(&context)
                            .to_u32()
                            .wrapping_add(val2.evaluate(&context).into())
                            != output
                        {
                            match constraints_holder.get(&id.back()) {
                                Constraints::Step2(x) => {
                                    let constraint = Constraint::Equals(
                                        RevVal::Input(1).into(),
                                        Operation::WAdd(val1, val2).into(),
                                    );
                                    let old_d: LazyRevVal =
                                        Operation::WSub("e".into(), "t1".into()).into();
                                    let constraint = constraint.map_context(vec![
                                        ("a", "b".into()),
                                        ("b", "c".into()),
                                        ("c", "d".into()),
                                        ("d", old_d),
                                        ("e", "f".into()),
                                        ("f", "g".into()),
                                        ("g", "h_temp".into()),
                                    ]);
                                    x.w_add1.to_w_add().constraints.push(constraint);
                                    return RevRes::ConstraintsChanged(id);
                                }
                                _ => todo!("{:?}", constraints_holder.get(&id.back())),
                            }
                        }

                        return (
                            val1.evaluate(&context).to_u32(),
                            val2.evaluate(&context).to_u32(),
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
                            val2.evaluate(&context).into()
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
                            .evaluate(&context)
                            .to_u32()
                            .wrapping_add(val2.evaluate(&context).into())
                            != output
                        {
                            match constraints_holder.get(&id.back()) {
                                Constraints::Step2(x) => {
                                    let constraint = Constraint::Equals(
                                        RevVal::Input(0).into(),
                                        Operation::WAdd(val1, val2).into(),
                                    );
                                    let t1_2 = Operation::WSub(
                                        "t1".into(),
                                        ContextVal::ArrayCtx("K", "i".into()).into(),
                                    )
                                    .into();
                                    let constraint = constraint.map_context(vec![("t1", t1_2)]);
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
                            val2.evaluate(&context).into()
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
                            .evaluate(&context)
                            .to_u32()
                            .wrapping_add(val2.evaluate(&context).into())
                            != output
                        {
                            match constraints_holder.get(&id.back()) {
                                Constraints::Step2(x) => {
                                    let constraint = Constraint::Equals(
                                        RevVal::Input(0).into(),
                                        Operation::WAdd(val1, val2).into(),
                                    );
                                    let ch =
                                        Operation::Ch("e".into(), "f".into(), "g".into()).into();
                                    let old_t1 = Operation::WSub("t1".into(), ch).into();
                                    let constraint = constraint.map_context(vec![("t1", old_t1)]);
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
                                RevVal::Input(1).into(),
                                Operation::Maj("a".into(), "b".into(), "c".into()).into(),
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
        Context::new(vec![("y".into(), y.into())]),
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
pub struct XorConstraints {}

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
                                RevVal::Input(0).into(),
                                Operation::And(RevVal::Output.into(), "y".into()).into(),
                            );
                            todo!("{constraint}");
                            x.and2.constraints.push(constraint);
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
