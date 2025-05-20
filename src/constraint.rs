use std::{
    arch::x86_64,
    backtrace::Backtrace,
    fmt::Display,
    fs::{self, File},
    io::{BufWriter, Write},
    ops::Deref,
    rc::Rc,
    sync::Arc,
};

use hashbrown::HashMap;
use indexmap::IndexMap;
use itertools::Itertools;
use once_cell::sync::Lazy;
use parking_lot::Mutex;

use crate::{
    bytecode::{Bytecode, BytecodeMapping, BytecodeProgram, BytecodeSingleMapping},
    evaluated::factorise,
    mapping::Mapping,
    rev_fns::{
        AndConstraints, AssignmentConstraints, BigSigma0Constraints, BigSigma1Constraints,
        ChConstraints, MajConstraints, WrappingAddConstraints, XorConstraints,
    },
    rev_val_holder::{LazyRevVal, RevValHolder},
    reverse::{Context, Step1Constraints, Step2Constraints},
    sha256::{ch, maj},
    utils::AbstractVal,
};

#[derive(Debug)]
pub enum Constraint {
    Equals(BytecodeProgram, BytecodeProgram),
}

impl Display for Constraint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constraint::Equals(val1, val2) => {
                write!(f, "{} == {}", val1, val2)
            }
        }
    }
}

impl Constraint {
    pub fn map_context(self, mapping: Vec<(&'static str, BytecodeProgram)>) -> Constraint {
        let mapping: BytecodeSingleMapping = Rc::new(mapping);

        match self {
            Constraint::Equals(val1, val2) => Constraint::Equals(
                val1.map_context_lazy(mapping.clone()),
                val2.map_context_lazy(mapping.clone()),
            ),
        }
    }

    pub fn save_display(&self, path: &str) {
        fs::write(path, format!("{self}")).unwrap()
    }
}

// dont use it before making it intern
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum RevVal {
    // point to the output value
    Output,
    // point to one of input values
    Input(usize),
    // point to a value in context
    Context(ContextVal),
    Const(u32),
    Operation(Box<Operation>),
}

impl From<&'static str> for RevVal {
    fn from(value: &'static str) -> Self {
        Self::Context(value.into())
    }
}

impl Display for RevVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RevVal::Output => todo!(),
            RevVal::Input(i) => write!(f, "(input {i})"),
            RevVal::Context(context_val) => write!(f, "{context_val}"),
            RevVal::Const(x) => write!(f, "{x}"),
            RevVal::Operation(operation) => write!(f, "{operation}"),
        }
    }
}

impl RevVal {
    pub fn display_mapped(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        mapping: Mapping,
        cache: &IndexMap<*const RevVal, usize>,
    ) -> std::fmt::Result {
        match self {
            RevVal::Output => todo!(),
            RevVal::Input(i) => write!(f, "(input {i})"),
            RevVal::Context(val) => val.display_with_mapping(f, mapping, cache),
            RevVal::Const(x) => write!(f, "{x}"),
            RevVal::Operation(operation) => operation.display_mapped(f, mapping, cache),
        }
    }

    pub fn append_to_file_mapped(&self, file: &mut BufWriter<File>, mapping: Mapping) {
        match self {
            RevVal::Output => todo!(),
            RevVal::Input(_) => todo!(),
            RevVal::Context(val) => val.append_to_file_mapped(file, mapping),
            RevVal::Const(_) => todo!(),
            RevVal::Operation(operation) => operation.append_to_file_mapped(file, mapping),
        }
    }

    pub fn lookup_mapped(
        &self,
        cache: &mut IndexMap<(*const RevVal, Option<Mapping>), usize>,
        mapping: Mapping,
    ) {
        *cache
            .entry((self as *const RevVal, Some(mapping.clone())))
            .or_insert(0) += 1;
        match self {
            RevVal::Output => {}
            RevVal::Input(i) => {}
            RevVal::Context(val) => val.lookup_mapped(cache, mapping),
            RevVal::Const(x) => {}
            RevVal::Operation(operation) => operation.lookup_mapped(cache, mapping),
        }
    }

    pub fn intern(self) -> &'static Self {
        static REV_VAL_HOLDER: Lazy<Mutex<RevValHolder>> =
            Lazy::new(|| Mutex::new(RevValHolder::new()));

        let mut holder = REV_VAL_HOLDER.lock();
        let stored_ref = holder.intern(self);
        // SAFETY: `REV_VAL_HOLDER` lives for 'static and never deallocates or moves its buckets.
        // Hence the &RevVal reference remains valid for the program’s lifetime.
        unsafe { &*(stored_ref as *const RevVal) }
    }

    pub fn evaluate(&self, context: &Context) -> AbstractVal {
        match self {
            RevVal::Output => todo!(),
            RevVal::Input(_) => todo!(),
            RevVal::Context(val) => match val {
                ContextVal::Name(name) => {
                    if let Some(val) = context.values.get(*name) {
                        val.clone()
                    } else {
                        todo!("{} {}", name, Backtrace::force_capture())
                    }
                }
                ContextVal::Array(name, i) => {
                    if let Some(val) = context.values.get(*name) {
                        val.to_vec()[*i].clone()
                    } else {
                        todo!("{} {}", name, Backtrace::force_capture())
                    }
                }
                ContextVal::ArrayCtx(name, idx) => {
                    if let Some(val) = context.values.get(*name) {
                        val.to_vec()[idx.evaluate(context).to_u32() as usize].clone()
                    } else {
                        todo!("{} {}", name, Backtrace::force_capture())
                    }
                }
            },
            RevVal::Operation(operation) => operation.apply(context),
            RevVal::Const(x) => AbstractVal::U32(*x),
        }
    }

    pub fn evaluate_with_mapping(&self, context: &Context, mapping: Mapping) -> AbstractVal {
        match self {
            RevVal::Output => todo!(),
            RevVal::Input(_) => todo!(),
            RevVal::Context(val) => val.evaluate_with_mapping(context, mapping),
            RevVal::Const(x) => AbstractVal::U32(*x),
            RevVal::Operation(operation) => operation.apply_with_mapping(context, mapping),
        }
    }

    #[must_use]
    fn map_context(&mut self, mapping: &HashMap<&'static str, RevVal>) -> Option<RevVal> {
        todo!(); // will get removed
        // match self {
        //     RevVal::Output => None,
        //     RevVal::Input(_) => None,
        //     RevVal::Context(context_val) => {
        //         // the entire value could be replaced
        //         match context_val {
        //             ContextVal::Name(name) => {
        //                 if let Some(new) = mapping.get(*name) {
        //                     Some(new.clone())
        //                 } else {
        //                     todo!("{name} {}", Backtrace::force_capture())
        //                 }
        //             }
        //             ContextVal::Array(name, i) => {
        //                 if let Some(new) = mapping.get(*name) {
        //                     if let RevVal::Context(ContextVal::Name(name)) = new.deref() {
        //                         return Some(ContextVal::Array(name, *i).into());
        //                     }
        //                     Some(new.clone())
        //                 } else {
        //                     todo!("{name} {}", Backtrace::force_capture())
        //                 }
        //             }
        //             ContextVal::ArrayCtx(name, idx) => {
        //                 if let Some(new) = mapping.get(*name) {
        //                     if let RevVal::Context(ContextVal::Name(name)) = new.deref() {
        //                         return Some(
        //                             ContextVal::ArrayCtx(
        //                                 name,
        //                                 if let Some(idx) = idx.map_context(mapping) {
        //                                     Box::new(idx)
        //                                 } else {
        //                                     idx.clone()
        //                                 },
        //                             )
        //                             .into(),
        //                         );
        //                     }
        //                     Some(new.clone())
        //                 } else {
        //                     todo!("{name} {}", Backtrace::force_capture())
        //                 }
        //             }
        //         }
        //     }
        //     RevVal::Operation(operation) => {
        //         operation.as_mut().map_context(mapping);
        //         None
        //     }
        //     RevVal::Const(_) => None,
        // }
    }
}

impl From<Operation> for RevVal {
    fn from(value: Operation) -> Self {
        RevVal::Operation(Box::new(value))
    }
}

impl From<ContextVal> for RevVal {
    fn from(value: ContextVal) -> Self {
        RevVal::Context(value)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ContextVal {
    Name(&'static str),
    Array(&'static str, usize),
    ArrayCtx(&'static str, LazyRevVal),
}

impl ContextVal {
    // first check in mapping, then in context
    fn evaluate_with_mapping(&self, context: &Context, mapping: Mapping) -> AbstractVal {
        match self {
            ContextVal::Name(name) => {
                if let Some(val) = mapping.apply(name) {
                    val.evaluate(context)
                } else {
                    if let Some(val) = context.values.get(*name) {
                        val.clone()
                    } else {
                        todo!("{} {}", name, Backtrace::force_capture())
                    }
                }
            }
            ContextVal::Array(name, i) => {
                if let Some(val) = mapping.apply(name) {
                    val.evaluate(context).to_vec()[*i].clone()
                } else {
                    if let Some(val) = context.values.get(*name) {
                        val.to_vec()[*i].clone()
                    } else {
                        todo!("{} {}", name, Backtrace::force_capture())
                    }
                }
            }
            ContextVal::ArrayCtx(name, i) => {
                if let Some(val) = mapping.apply(name) {
                    val.evaluate(context).to_vec()
                        [i.evaluate_with_mapping(context, mapping).to_u32() as usize]
                        .clone()
                } else {
                    if let Some(val) = context.values.get(*name) {
                        val.to_vec()[i.evaluate_with_mapping(context, mapping).to_u32() as usize]
                            .clone()
                    } else {
                        todo!("{} {}", name, Backtrace::force_capture())
                    }
                }
            }
        }
    }

    fn display_with_mapping(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        mapping: Mapping,
        cache: &IndexMap<*const RevVal, usize>,
    ) -> std::fmt::Result {
        match self {
            ContextVal::Name(x) => {
                if let Some(x) = mapping.apply(x) {
                    x.display(f, cache)
                } else {
                    write!(f, "{x}")
                }
            }
            ContextVal::Array(x, i) => {
                if let Some(x) = mapping.apply(x) {
                    x.display(f, cache).unwrap();
                } else {
                    write!(f, "{x}").unwrap();
                }
                write!(f, "[{i}]")
            }
            ContextVal::ArrayCtx(x, val) => {
                if let Some(x) = mapping.apply(x) {
                    x.display(f, cache).unwrap();
                } else {
                    write!(f, "{x}").unwrap();
                }
                write!(f, "[").unwrap();
                val.clone().map_context(mapping).display(f, cache).unwrap();
                write!(f, "]")
            }
        }
    }

    fn append_to_file_mapped(&self, file: &mut BufWriter<File>, mapping: Mapping) {
        match self {
            ContextVal::Name(x) => {
                if let Some(x) = mapping.apply(x) {
                    x.append_to_file(file);
                } else {
                    write!(file, "{x}").unwrap()
                }
            }
            ContextVal::Array(x, i) => {
                if let Some(x) = mapping.apply(x) {
                    x.append_to_file(file);
                } else {
                    write!(file, "{x}").unwrap()
                }
                write!(file, "[{i}]").unwrap()
            }
            ContextVal::ArrayCtx(x, val) => {
                if let Some(x) = mapping.apply(x) {
                    x.append_to_file(file);
                } else {
                    write!(file, "{x}").unwrap()
                }
                write!(file, "[").unwrap();
                val.clone().map_context(mapping).append_to_file(file);
                write!(file, "]").unwrap();
            }
        }
    }

    fn lookup_mapped(
        &self,
        cache: &mut IndexMap<(*const RevVal, Option<Mapping>), usize>,
        mapping: Mapping,
    ) {
        match self {
            ContextVal::Name(x) => {
                if let Some(x) = mapping.apply(x) {
                    x.lookup(cache);
                }
            }
            ContextVal::Array(x, _) => {
                if let Some(x) = mapping.apply(x) {
                    x.lookup(cache);
                }
            }
            ContextVal::ArrayCtx(x, val) => {
                if let Some(x) = mapping.apply(x) {
                    x.lookup(cache);
                }
                val.lookup_mapped(cache, mapping);
            }
        }
    }

    fn lookup(&self, cache: &mut IndexMap<(*const RevVal, Option<Mapping>), usize>) {
        match self {
            ContextVal::Name(_) => {}
            ContextVal::Array(_, _) => {}
            ContextVal::ArrayCtx(_, lazy_rev_val) => lazy_rev_val.lookup(cache),
        }
    }
}

impl Display for ContextVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ContextVal::Name(x) => write!(f, "{x}"),
            ContextVal::Array(name, i) => write!(f, "{name}[{i}]"),
            ContextVal::ArrayCtx(name, i) => write!(f, "{name}[{i}]"),
        }
    }
}

impl Into<ContextVal> for &'static str {
    fn into(self) -> ContextVal {
        ContextVal::Name(self)
    }
}
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operation {
    BigSigma0(LazyRevVal),
    BigSigma1(LazyRevVal),
    Xor(LazyRevVal, LazyRevVal),
    And(LazyRevVal, LazyRevVal),
    WAdd(LazyRevVal, LazyRevVal),
    WSub(LazyRevVal, LazyRevVal),
    Maj(LazyRevVal, LazyRevVal, LazyRevVal),
    Ch(LazyRevVal, LazyRevVal, LazyRevVal),
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::BigSigma0(val) => write!(f, "big_sigma0({val})"),
            Operation::BigSigma1(val) => write!(f, "big_sigma1({val})"),
            Operation::Xor(val1, val2) => todo!(),
            Operation::And(val1, val2) => todo!(),
            Operation::WAdd(val1, val2) => write!(f, "{val1}.wrapping_add({val2})"),
            Operation::WSub(val1, val2) => write!(f, "{val1}.wrapping_sub({val2})"),
            Operation::Maj(val1, val2, val3) => write!(f, "maj({val1}, {val2}, {val3})"),
            Operation::Ch(val1, val2, val3) => write!(f, "ch({val1}, {val2}, {val3})"),
        }
    }
}

impl Operation {
    pub fn lookup(&self, cache: &mut IndexMap<(*const RevVal, Option<Mapping>), usize>) {
        match self {
            Operation::BigSigma0(val) | Operation::BigSigma1(val) => val.lookup(cache),
            Operation::Xor(val1, val2)
            | Operation::And(val1, val2)
            | Operation::WAdd(val1, val2)
            | Operation::WSub(val1, val2) => {
                val1.lookup(cache);
                val2.lookup(cache);
            }
            Operation::Maj(val1, val2, val3) | Operation::Ch(val1, val2, val3) => {
                val1.lookup(cache);
                val2.lookup(cache);
                val3.lookup(cache);
            }
        }
    }

    pub fn lookup_mapped(
        &self,
        cache: &mut IndexMap<(*const RevVal, Option<Mapping>), usize>,
        mapping: Mapping,
    ) {
        match self {
            Operation::BigSigma0(val) | Operation::BigSigma1(val) => {
                val.lookup_mapped(cache, mapping)
            }
            Operation::Xor(val1, val2)
            | Operation::And(val1, val2)
            | Operation::WAdd(val1, val2)
            | Operation::WSub(val1, val2) => {
                val1.lookup_mapped(cache, mapping.clone());
                val2.lookup_mapped(cache, mapping);
            }
            Operation::Maj(val1, val2, val3) | Operation::Ch(val1, val2, val3) => {
                val1.lookup_mapped(cache, mapping.clone());
                val2.lookup_mapped(cache, mapping.clone());
                val3.lookup_mapped(cache, mapping);
            }
        }
    }

    pub fn display_mapped(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        mapping: Mapping,
        cache: &IndexMap<*const RevVal, usize>,
    ) -> std::fmt::Result {
        match self {
            Operation::BigSigma0(val) => {
                write!(f, "big_sigma0(")?;
                val.display_mapped(f, mapping, cache)?;
                write!(f, ")")
            }
            Operation::BigSigma1(val) => {
                write!(f, "big_sigma1(")?;
                val.display_mapped(f, mapping, cache)?;
                write!(f, ")")
            }
            Operation::Xor(val1, val2) => todo!(),
            Operation::And(val1, val2) => todo!(),
            Operation::WAdd(val1, val2) => {
                val1.display_mapped(f, mapping.clone(), cache)?;
                write!(f, ".wrapping_add(")?;
                val2.display_mapped(f, mapping, cache)?;
                write!(f, ")")
            }
            Operation::WSub(val1, val2) => {
                val1.display_mapped(f, mapping.clone(), cache)?;
                write!(f, ".wrapping_sub(")?;
                val2.display_mapped(f, mapping, cache)?;
                write!(f, ")")
            }
            Operation::Maj(val1, val2, val3) => {
                write!(f, "maj(")?;
                val1.display_mapped(f, mapping.clone(), cache)?;
                write!(f, ", ")?;
                val2.display_mapped(f, mapping.clone(), cache)?;
                write!(f, ", ")?;
                val3.display_mapped(f, mapping, cache)?;
                write!(f, ")")
            }
            Operation::Ch(val1, val2, val3) => {
                write!(f, "ch(")?;
                val1.display_mapped(f, mapping.clone(), cache)?;
                write!(f, ", ")?;
                val2.display_mapped(f, mapping.clone(), cache)?;
                write!(f, ", ")?;
                val3.display_mapped(f, mapping, cache)?;
                write!(f, ")")
            }
        }
    }

    pub fn append_to_file_mapped(&self, file: &mut BufWriter<File>, mapping: Mapping) {
        match self {
            Operation::BigSigma0(val) => {
                write!(file, "big_sigma0(").unwrap();
                val.append_to_file_mapped(file, mapping);
                write!(file, ")").unwrap();
            }
            Operation::BigSigma1(val) => {
                write!(file, "big_sigma1(").unwrap();
                val.append_to_file_mapped(file, mapping);
                write!(file, ")").unwrap();
            }
            Operation::Xor(val1, val2) => todo!(),
            Operation::And(val1, val2) => todo!(),
            Operation::WAdd(val1, val2) => {
                val1.append_to_file_mapped(file, mapping.clone());
                write!(file, ".wrapping_add(").unwrap();
                val2.append_to_file_mapped(file, mapping);
                write!(file, ")").unwrap();
            }
            Operation::WSub(val1, val2) => {
                val1.append_to_file_mapped(file, mapping.clone());
                write!(file, ".wrapping_sub(").unwrap();
                val2.append_to_file_mapped(file, mapping);
                write!(file, ")").unwrap();
            }
            Operation::Maj(val1, val2, val3) => {
                write!(file, "maj(").unwrap();
                val1.append_to_file_mapped(file, mapping.clone());
                write!(file, ", ").unwrap();
                val2.append_to_file_mapped(file, mapping.clone());
                write!(file, ", ").unwrap();
                val3.append_to_file_mapped(file, mapping);
                write!(file, ")").unwrap()
            }
            Operation::Ch(val1, val2, val3) => {
                write!(file, "ch(").unwrap();
                val1.append_to_file_mapped(file, mapping.clone());
                write!(file, ", ").unwrap();
                val2.append_to_file_mapped(file, mapping.clone());
                write!(file, ", ").unwrap();
                val3.append_to_file_mapped(file, mapping);
                write!(file, ")").unwrap()
            }
        }
    }

    pub fn apply(&self, context: &Context) -> AbstractVal {
        match self {
            Operation::BigSigma0(val) => val.evaluate(context).big_sigma0(),
            Operation::BigSigma1(val) => val.evaluate(context).big_sigma1(),
            Operation::Xor(rev_val, rev_val1) => todo!(),
            Operation::And(rev_val, rev_val1) => todo!(),
            Operation::WAdd(val1, val2) => {
                val1.evaluate(context).wrapping_add(val2.evaluate(context))
            }
            Operation::WSub(val1, val2) => {
                val1.evaluate(context).wrapping_sub(val2.evaluate(context))
            }
            Operation::Maj(val1, val2, val3) => AbstractVal::U32(maj(
                val1.evaluate(context).into(),
                val2.evaluate(context).into(),
                val3.evaluate(context).into(),
            )),
            Operation::Ch(val1, val2, val3) => AbstractVal::U32(ch(
                val1.evaluate(context).into(),
                val2.evaluate(context).into(),
                val3.evaluate(context).into(),
            )),
        }
    }

    pub fn apply_with_mapping(&self, context: &Context, mapping: Mapping) -> AbstractVal {
        match self {
            Operation::BigSigma0(val) => val.evaluate_with_mapping(context, mapping).big_sigma0(),
            Operation::BigSigma1(val) => val.evaluate_with_mapping(context, mapping).big_sigma1(),
            Operation::Xor(val1, val2) => todo!(),
            Operation::And(val1, val2) => todo!(),
            Operation::WAdd(val1, val2) => val1
                .evaluate_with_mapping(context, mapping.clone())
                .wrapping_add(val2.evaluate_with_mapping(context, mapping)),
            Operation::WSub(val1, val2) => val1
                .evaluate_with_mapping(context, mapping.clone())
                .wrapping_sub(val2.evaluate_with_mapping(context, mapping)),
            Operation::Maj(val1, val2, val3) => AbstractVal::U32(maj(
                val1.evaluate_with_mapping(context, mapping.clone()).into(),
                val2.evaluate_with_mapping(context, mapping.clone()).into(),
                val3.evaluate_with_mapping(context, mapping).into(),
            )),
            Operation::Ch(val1, val2, val3) => AbstractVal::U32(ch(
                val1.evaluate_with_mapping(context, mapping.clone()).into(),
                val2.evaluate_with_mapping(context, mapping.clone()).into(),
                val3.evaluate_with_mapping(context, mapping).into(),
            )),
        }
    }

    fn map_context(&mut self, mapping: &HashMap<&'static str, RevVal>) {
        todo!() // will get removed
        // match self {
        //     Operation::BigSigma0(val) => {
        //         if let Some(new) = val.map_context(&mapping) {
        //             *val = new
        //         }
        //     }
        //     Operation::BigSigma1(val) => {
        //         if let Some(new) = val.map_context(&mapping) {
        //             *val = new
        //         }
        //     }
        //     Operation::Xor(rev_val, rev_val1) => todo!(),
        //     Operation::And(rev_val, rev_val1) => todo!(),
        //     Operation::WAdd(val1, val2) => {
        //         if let Some(new) = val1.map_context(&mapping) {
        //             *val1 = new
        //         }
        //         if let Some(new) = val2.map_context(&mapping) {
        //             *val2 = new
        //         }
        //     }
        //     Operation::WSub(val1, val2) => {
        //         if let Some(new) = val1.map_context(&mapping) {
        //             *val1 = new
        //         }
        //         if let Some(new) = val2.map_context(&mapping) {
        //             *val2 = new
        //         }
        //     }
        //     Operation::Maj(val1, val2, val3) => {
        //         if let Some(new) = val1.map_context(&mapping) {
        //             *val1 = new
        //         }
        //         if let Some(new) = val2.map_context(&mapping) {
        //             *val2 = new
        //         }
        //         if let Some(new) = val3.map_context(&mapping) {
        //             *val3 = new
        //         }
        //     }
        //     Operation::Ch(val1, val2, val3) => {
        //         if let Some(new) = val1.map_context(&mapping) {
        //             *val1 = new
        //         }
        //         if let Some(new) = val2.map_context(&mapping) {
        //             *val2 = new
        //         }
        //         if let Some(new) = val3.map_context(&mapping) {
        //             *val3 = new
        //         }
        //     }
        // }
    }
}

#[derive(Clone, Debug)]
pub struct ConstraintId(pub Vec<usize>);

impl ConstraintId {
    pub fn back(&self) -> ConstraintId {
        ConstraintId(self.0[..self.0.len() - 1].to_vec())
    }

    pub fn next(&self, index: usize) -> ConstraintId {
        ConstraintId({
            let mut id = self.0.clone();
            id.push(index);
            id
        })
    }
}

pub struct ConstraintsHolder {
    first: Constraints,
}

impl Default for ConstraintsHolder {
    fn default() -> Self {
        Self {
            first: Constraints::Start(
                Constraints::Step1(Step1Constraints::default().into()).into(),
            ),
        }
    }
}

impl ConstraintsHolder {
    pub fn get(&mut self, id: &ConstraintId) -> &mut Constraints {
        let mut res = &mut self.first;
        for index in id.0.clone() {
            res = match res {
                Constraints::Start(x) => x,
                Constraints::Step1(x) => match index {
                    1 => x
                        .step1
                        .get_or_insert(Constraints::Step1(Step1Constraints::default().into())),
                    2 => x
                        .step2
                        .get_or_insert(Constraints::Step2(Step2Constraints::default().into())),
                    _ => todo!("{index}"),
                },
                Constraints::Step2(x) => match index {
                    0 => &mut x.w_add1,
                    1 => &mut x.ass1,
                    2 => &mut x.ass2,
                    3 => &mut x.ass3,
                    5 => &mut x.ass4,
                    6 => &mut x.ass5,
                    7 => &mut x.ass6,
                    9 => &mut x.w_add3,
                    17 => x
                        .step2
                        .get_or_insert(Constraints::Step2(Step2Constraints::default().into())),
                    _ => todo!("{index}"),
                },
                Constraints::Xor(xor_constraints) => todo!(),
                Constraints::And(and_constraints) => todo!(),
                Constraints::Maj(maj_constraints) => todo!(),
                Constraints::Ch(ch_constraints) => todo!(),
                Constraints::WAdd(wrapping_add_constraints) => todo!(),
                Constraints::Ass(assignment_constraints) => todo!(),
                Constraints::BigSigma0(big_sigma0_constraints) => todo!(),
                Constraints::BigSigma1(big_sigma1_constraints) => todo!(),
            }
        }
        res
    }
}

#[derive(Debug)]
pub enum Constraints {
    // start is different, holds directly
    Start(Box<Constraints>),
    Step1(Box<Step1Constraints>),
    Step2(Box<Step2Constraints>),
    Xor(Box<XorConstraints>),
    And(Box<AndConstraints>),
    Maj(Box<MajConstraints>),
    Ch(Box<ChConstraints>),
    WAdd(Box<WrappingAddConstraints>),
    Ass(Box<AssignmentConstraints>),
    BigSigma0(Box<BigSigma0Constraints>),
    BigSigma1(Box<BigSigma1Constraints>),
}

impl Constraints {
    pub fn to_xor(&self) -> &XorConstraints {
        let Constraints::Xor(x) = self else {
            unreachable!()
        };
        x
    }
    pub fn to_and(&self) -> &AndConstraints {
        let Constraints::And(x) = self else {
            unreachable!()
        };
        x
    }
    pub fn to_maj(&self) -> &MajConstraints {
        let Constraints::Maj(x) = self else {
            unreachable!()
        };
        x
    }
    pub fn to_ch(&self) -> &ChConstraints {
        let Constraints::Ch(x) = self else {
            unreachable!()
        };
        x
    }
    pub fn to_w_add(&mut self) -> &mut WrappingAddConstraints {
        let Constraints::WAdd(x) = self else {
            unreachable!()
        };
        x
    }
}
