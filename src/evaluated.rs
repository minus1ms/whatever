use std::{
    backtrace::Backtrace,
    fmt::{self, Display, Formatter},
    rc::Rc,
};

use hashbrown::HashMap;

use crate::{
    constraint::{ContextVal, Operation, RevVal},
    mapping::Mapping,
    rev_val_holder::LazyRevVal,
    reverse::Context,
    sha256::{ch, maj},
    utils::AbstractVal,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EvaluatedRevVal {
    Context(EvaluatedContextVal),
    Const(u32),
    Operation(EvaluatedOperation),

    /// New: reference into the variable map produced by `factorise`.
    VarRef(String),
}

// impl EvaluatedRevVal {
//     pub fn evaluate(
//         &self,
//         cache: &mut HashMap<EvaluatedRevVal, AbstractVal>,
//         context: &Context,
//     ) -> AbstractVal {
//         // if let Some(cached) = cache.get(self) {
//         //     return cached.clone();
//         // }
//         let res = match self {
//             EvaluatedRevVal::Context(context_val) => context_val.evaluate(cache, context),
//             EvaluatedRevVal::Const(x) => AbstractVal::U32(*x),
//             EvaluatedRevVal::Operation(operation) => operation.evaluate(cache, context),
//             EvaluatedRevVal::VarRef(_) => todo!(),
//         };
//         // cache.insert(self.clone(), res.clone());
//         res
//     }

//     pub fn create_lazy(arg: &LazyRevVal) -> Rc<Self> {
//         match arg {
//             LazyRevVal::Ref(rev_val) => Self::create(rev_val),
//             LazyRevVal::Mapping { inner, mapping } => Self::create_lazy_mapped(inner, mapping),
//         }
//     }

//     fn create_lazy_mapped(val: &LazyRevVal, mapping: &Mapping) -> Rc<Self> {
//         match val {
//             LazyRevVal::Ref(rev_val) => Self::create_mapped(rev_val, mapping),
//             LazyRevVal::Mapping {
//                 inner,
//                 mapping: inner_mapping,
//             } => Self::create_lazy_mapped(&inner, &inner_mapping.merge(mapping.clone())),
//         }
//     }

//     pub fn create(arg: &RevVal) -> Rc<Self> {
//         match arg {
//             RevVal::Output => todo!(),
//             RevVal::Input(_) => todo!(),
//             RevVal::Context(context_val) => EvaluatedContextVal::create(context_val),
//             RevVal::Const(x) => Self::Const(*x).into(),
//             RevVal::Operation(operation) => {
//                 Self::Operation(EvaluatedOperation::create(operation).into()).into()
//             }
//         }
//     }

//     fn create_mapped(val: &RevVal, mapping: &Mapping) -> Rc<Self> {
//         match val {
//             RevVal::Output => todo!(),
//             RevVal::Input(_) => todo!(),
//             RevVal::Context(context_val) => {
//                 EvaluatedContextVal::create_mapped(context_val, mapping)
//             }
//             RevVal::Const(x) => Self::Const(*x).into(),
//             RevVal::Operation(operation) => {
//                 Self::Operation(EvaluatedOperation::create_mapped(operation, mapping).into()).into()
//             }
//         }
//     }
// }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EvaluatedOperation {
    BigSigma0(Rc<EvaluatedRevVal>),
    BigSigma1(Rc<EvaluatedRevVal>),
    Xor(Rc<EvaluatedRevVal>, Rc<EvaluatedRevVal>),
    And(Rc<EvaluatedRevVal>, Rc<EvaluatedRevVal>),
    WAdd(Rc<EvaluatedRevVal>, Rc<EvaluatedRevVal>),
    WSub(Rc<EvaluatedRevVal>, Rc<EvaluatedRevVal>),
    Maj(
        Rc<EvaluatedRevVal>,
        Rc<EvaluatedRevVal>,
        Rc<EvaluatedRevVal>,
    ),
    Ch(
        Rc<EvaluatedRevVal>,
        Rc<EvaluatedRevVal>,
        Rc<EvaluatedRevVal>,
    ),
}

// impl EvaluatedOperation {
//     pub fn evaluate(
//         &self,
//         cache: &mut HashMap<EvaluatedRevVal, AbstractVal>,
//         context: &Context,
//     ) -> AbstractVal {
//         match self {
//             EvaluatedOperation::BigSigma0(val) => val.evaluate(cache, context).big_sigma0(),
//             EvaluatedOperation::BigSigma1(val) => val.evaluate(cache, context).big_sigma1(),
//             EvaluatedOperation::Xor(val1, val2) => todo!(),
//             EvaluatedOperation::And(val1, val2) => todo!(),
//             EvaluatedOperation::WAdd(val1, val2) => val1
//                 .evaluate(cache, context)
//                 .wrapping_add(val2.evaluate(cache, context)),
//             EvaluatedOperation::WSub(val1, val2) => val1
//                 .evaluate(cache, context)
//                 .wrapping_sub(val2.evaluate(cache, context)),
//             EvaluatedOperation::Maj(val1, val2, val3) => AbstractVal::U32(maj(
//                 val1.evaluate(cache, context).into(),
//                 val2.evaluate(cache, context).into(),
//                 val3.evaluate(cache, context).into(),
//             )),
//             EvaluatedOperation::Ch(val1, val2, val3) => AbstractVal::U32(ch(
//                 val1.evaluate(cache, context).into(),
//                 val2.evaluate(cache, context).into(),
//                 val3.evaluate(cache, context).into(),
//             )),
//         }
//     }

//     fn create_mapped(operation: &Operation, mapping: &Mapping) -> Self {
//         match operation {
//             Operation::BigSigma0(val) => {
//                 Self::BigSigma0(EvaluatedRevVal::create_lazy_mapped(val, mapping))
//             }
//             Operation::BigSigma1(val) => {
//                 Self::BigSigma1(EvaluatedRevVal::create_lazy_mapped(val, mapping))
//             }
//             Operation::Xor(val1, val2) => Self::Xor(
//                 EvaluatedRevVal::create_lazy_mapped(val1, mapping),
//                 EvaluatedRevVal::create_lazy_mapped(val2, mapping),
//             ),
//             Operation::And(val1, val2) => Self::And(
//                 EvaluatedRevVal::create_lazy_mapped(val1, mapping),
//                 EvaluatedRevVal::create_lazy_mapped(val2, mapping),
//             ),
//             Operation::WAdd(val1, val2) => Self::WAdd(
//                 EvaluatedRevVal::create_lazy_mapped(val1, mapping),
//                 EvaluatedRevVal::create_lazy_mapped(val2, mapping),
//             ),
//             Operation::WSub(val1, val2) => Self::WSub(
//                 EvaluatedRevVal::create_lazy_mapped(val1, mapping),
//                 EvaluatedRevVal::create_lazy_mapped(val2, mapping),
//             ),
//             Operation::Maj(val1, val2, val3) => Self::Maj(
//                 EvaluatedRevVal::create_lazy_mapped(val1, mapping),
//                 EvaluatedRevVal::create_lazy_mapped(val2, mapping),
//                 EvaluatedRevVal::create_lazy_mapped(val3, mapping),
//             ),
//             Operation::Ch(val1, val2, val3) => Self::Ch(
//                 EvaluatedRevVal::create_lazy_mapped(val1, mapping),
//                 EvaluatedRevVal::create_lazy_mapped(val2, mapping),
//                 EvaluatedRevVal::create_lazy_mapped(val3, mapping),
//             ),
//         }
//     }

//     fn create(operation: &Operation) -> Self {
//         match operation {
//             Operation::BigSigma0(val) => Self::BigSigma0(EvaluatedRevVal::create_lazy(val)),
//             Operation::BigSigma1(val) => Self::BigSigma1(EvaluatedRevVal::create_lazy(val)),
//             Operation::Xor(val1, val2) => Self::Xor(
//                 EvaluatedRevVal::create_lazy(val1),
//                 EvaluatedRevVal::create_lazy(val2),
//             ),
//             Operation::And(val1, val2) => Self::And(
//                 EvaluatedRevVal::create_lazy(val1),
//                 EvaluatedRevVal::create_lazy(val2),
//             ),
//             Operation::WAdd(val1, val2) => Self::WAdd(
//                 EvaluatedRevVal::create_lazy(val1),
//                 EvaluatedRevVal::create_lazy(val2),
//             ),
//             Operation::WSub(val1, val2) => Self::WSub(
//                 EvaluatedRevVal::create_lazy(val1),
//                 EvaluatedRevVal::create_lazy(val2),
//             ),
//             Operation::Maj(val1, val2, val3) => Self::Maj(
//                 EvaluatedRevVal::create_lazy(val1),
//                 EvaluatedRevVal::create_lazy(val2),
//                 EvaluatedRevVal::create_lazy(val3),
//             ),
//             Operation::Ch(val1, val2, val3) => Self::Ch(
//                 EvaluatedRevVal::create_lazy(val1),
//                 EvaluatedRevVal::create_lazy(val2),
//                 EvaluatedRevVal::create_lazy(val3),
//             ),
//         }
//     }
// }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EvaluatedContextVal {
    Name(&'static str),
    Array(&'static str, usize),
    ArrayCtx(&'static str, Rc<EvaluatedRevVal>),
}

// impl EvaluatedContextVal {
//     fn evaluate(
//         &self,
//         cache: &mut HashMap<EvaluatedRevVal, AbstractVal>,
//         ctx: &Context,
//     ) -> AbstractVal {
//         match self {
//             EvaluatedContextVal::Name(x) => {
//                 if let Some(val) = ctx.values.get(*x) {
//                     val.clone()
//                 } else {
//                     todo!("{} {}", x, Backtrace::force_capture())
//                 }
//             }
//             EvaluatedContextVal::Array(x, i) => {
//                 if let Some(val) = ctx.values.get(*x) {
//                     val.to_vec()[*i].clone()
//                 } else {
//                     todo!("{} {}", x, Backtrace::force_capture())
//                 }
//             }
//             EvaluatedContextVal::ArrayCtx(x, i) => {
//                 if let Some(val) = ctx.values.get(*x) {
//                     val.to_vec()[i.evaluate(cache, ctx).to_u32() as usize].clone()
//                 } else {
//                     todo!("{} {}", x, Backtrace::force_capture())
//                 }
//             }
//         }
//     }

//     // we return that because we can map out the entire value
//     fn create_mapped(context_val: &ContextVal, mapping: &Mapping) -> Rc<EvaluatedRevVal> {
//         match context_val {
//             ContextVal::Name(x) => {
//                 if let Some(val) = mapping.apply(x) {
//                     EvaluatedRevVal::create_lazy(&val)
//                 } else {
//                     EvaluatedRevVal::Context(Self::Name(x).into()).into()
//                 }
//             }
//             ContextVal::Array(x, i) => {
//                 if let Some(val) = mapping.apply(x) {
//                     todo!()
//                 } else {
//                     EvaluatedRevVal::Context(Self::Array(x, *i).into()).into()
//                 }
//             }
//             ContextVal::ArrayCtx(x, lazy_rev_val) => {
//                 if let Some(val) = mapping.apply(x) {
//                     todo!()
//                 } else {
//                     EvaluatedRevVal::Context(
//                         Self::ArrayCtx(
//                             x,
//                             EvaluatedRevVal::create_lazy_mapped(lazy_rev_val, mapping),
//                         )
//                         .into(),
//                     )
//                     .into()
//                 }
//             }
//         }
//     }

//     fn create(context_val: &ContextVal) -> Rc<EvaluatedRevVal> {
//         match context_val {
//             ContextVal::Name(x) => EvaluatedRevVal::Context(Self::Name(x).into()).into(),
//             ContextVal::Array(x, i) => EvaluatedRevVal::Context(Self::Array(x, *i).into()).into(),
//             ContextVal::ArrayCtx(x, val) => {
//                 EvaluatedRevVal::Context(Self::ArrayCtx(x, val.into_evaluated()).into()).into()
//             }
//         }
//     }
// }

/// Public helper -----------------------------------------------------------
///
/// Returns the rewritten expression and a Vec of `(var_name, definition)`.
pub fn factorise(
    root: Rc<EvaluatedRevVal>,
) -> (Rc<EvaluatedRevVal>, Vec<(String, Rc<EvaluatedRevVal>)>) {
    let mut counts = HashMap::<Rc<EvaluatedRevVal>, usize>::new();
    collect_counts(root.clone(), &mut counts);

    let mut mapping = HashMap::<Rc<EvaluatedRevVal>, String>::new();
    let mut defs = Vec::<(String, Rc<EvaluatedRevVal>)>::new();
    let mut counter = 0_usize;

    let rewritten = replace_repeats(root, &counts, &mut mapping, &mut defs, &mut counter);

    (rewritten, defs)
}

/// Pass-1: tally every subtree. --------------------------------------------
fn collect_counts(val: Rc<EvaluatedRevVal>, acc: &mut HashMap<Rc<EvaluatedRevVal>, usize>) {
    *acc.entry(val.clone()).or_insert(0) += 1;

    match val.as_ref() {
        EvaluatedRevVal::Context(EvaluatedContextVal::ArrayCtx(_, inner)) => {
            collect_counts(inner.clone(), acc);
        }
        EvaluatedRevVal::Operation(op) => match op {
            EvaluatedOperation::BigSigma0(a) | EvaluatedOperation::BigSigma1(a) => {
                collect_counts(a.clone(), acc);
            }
            EvaluatedOperation::Xor(a, b)
            | EvaluatedOperation::And(a, b)
            | EvaluatedOperation::WAdd(a, b)
            | EvaluatedOperation::WSub(a, b) => {
                collect_counts(a.clone(), acc);
                collect_counts(b.clone(), acc);
            }
            EvaluatedOperation::Maj(a, b, c) | EvaluatedOperation::Ch(a, b, c) => {
                collect_counts(a.clone(), acc);
                collect_counts(b.clone(), acc);
                collect_counts(c.clone(), acc);
            }
        },
        _ => {}
    }
}

/// True if this node **may** be hoisted into a variable. --------------------
fn candidate(val: &EvaluatedRevVal) -> bool {
    match val {
        // Plain context name is atomic – leave it inline.
        EvaluatedRevVal::Context(EvaluatedContextVal::Name(_)) | EvaluatedRevVal::Const(_) => false,
        // Everything else is fair game.
        _ => true,
    }
}

/// Pass-2: build new tree, hoisting repeats into `VarRef`. ------------------
fn replace_repeats(
    val: Rc<EvaluatedRevVal>,
    counts: &HashMap<Rc<EvaluatedRevVal>, usize>,
    mapping: &mut HashMap<Rc<EvaluatedRevVal>, String>,
    defs: &mut Vec<(String, Rc<EvaluatedRevVal>)>,
    counter: &mut usize,
) -> Rc<EvaluatedRevVal> {
    // Do we *want* a variable for this exact subtree?
    let need_var = candidate(&val) && counts.get(&val).copied().unwrap_or(0) > 1;
    // if need_var {
    //     if let EvaluatedRevVal::Operation(box EvaluatedOperation::BigSigma0(_)) = val {
    //         println!("{}", counts.get(&val).copied().unwrap_or(0));
    //         println!("{val}");
    //         todo!()
    //     }
    // }

    if need_var {
        // Already have one?
        if let Some(name) = mapping.get(&val) {
            return EvaluatedRevVal::VarRef(name.clone()).into();
        }

        // Create a fresh variable and *then* recurse so that the
        // definition itself is as compact as possible.
        let var_name = format!("var_{}", *counter);
        *counter += 1;

        // Insert placeholder first to break potential self-recursion loops.
        mapping.insert(val.clone(), var_name.clone());

        // Recurse into a *clone* so we keep the original around as map key.
        let transformed_def = box_children(val.clone(), |child| {
            replace_repeats(child, counts, mapping, defs, counter)
        });

        defs.push((var_name.clone(), transformed_def.clone()));
        EvaluatedRevVal::VarRef(var_name).into()
    } else {
        // Not hoisted – just rebuild with transformed children.
        box_children(val, |child| {
            replace_repeats(child, counts, mapping, defs, counter)
        })
    }
}

/// Helper: rebuild an expression after transforming each child. ------------
fn box_children<F>(val: Rc<EvaluatedRevVal>, mut f: F) -> Rc<EvaluatedRevVal>
where
    F: FnMut(Rc<EvaluatedRevVal>) -> Rc<EvaluatedRevVal>,
{
    match val.as_ref() {
        EvaluatedRevVal::Operation(op) => {
            let new_op = match op {
                EvaluatedOperation::BigSigma0(a) => EvaluatedOperation::BigSigma0(f(a.clone())),
                EvaluatedOperation::BigSigma1(a) => EvaluatedOperation::BigSigma1(f(a.clone())),
                EvaluatedOperation::Xor(a, b) => {
                    EvaluatedOperation::Xor(f(a.clone()), f(b.clone()))
                }
                EvaluatedOperation::And(a, b) => {
                    EvaluatedOperation::And(f(a.clone()), f(b.clone()))
                }
                EvaluatedOperation::WAdd(a, b) => {
                    EvaluatedOperation::WAdd(f(a.clone()), f(b.clone()))
                }
                EvaluatedOperation::WSub(a, b) => {
                    EvaluatedOperation::WSub(f(a.clone()), f(b.clone()))
                }
                EvaluatedOperation::Maj(a, b, c) => {
                    EvaluatedOperation::Maj(f(a.clone()), f(b.clone()), f(c.clone()))
                }
                EvaluatedOperation::Ch(a, b, c) => {
                    EvaluatedOperation::Ch(f(a.clone()), f(b.clone()), f(c.clone()))
                }
            };
            EvaluatedRevVal::Operation(new_op).into()
        }
        EvaluatedRevVal::Context(EvaluatedContextVal::ArrayCtx(name, inner)) => {
            EvaluatedRevVal::Context(EvaluatedContextVal::ArrayCtx(name, f(inner.clone()))).into()
        }
        // Leaf nodes or VarRef – nothing to transform.
        _ => val,
    }
}

impl Display for EvaluatedContextVal {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            EvaluatedContextVal::Name(name) => write!(f, "{}", name),
            EvaluatedContextVal::Array(name, idx) => write!(f, "{}[{}]", name, idx),
            EvaluatedContextVal::ArrayCtx(name, inner) => write!(f, "{}[{}]", name, inner),
        }
    }
}

impl Display for EvaluatedRevVal {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            EvaluatedRevVal::Context(ctx) => write!(f, "{}", ctx),
            EvaluatedRevVal::Const(c) => write!(f, "{}", c),
            EvaluatedRevVal::Operation(op) => write!(f, "{}", op),
            EvaluatedRevVal::VarRef(s) => write!(f, "{}", s),
        }
    }
}

impl Display for EvaluatedOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            EvaluatedOperation::BigSigma0(x) => write!(f, "big_sigma0({})", x),
            EvaluatedOperation::BigSigma1(x) => write!(f, "big_sigma1({})", x),
            EvaluatedOperation::Xor(a, b) => write!(f, "{} ^ {}", a, b),
            EvaluatedOperation::And(a, b) => write!(f, "{} & {}", a, b),
            EvaluatedOperation::WAdd(a, b) => write!(f, "{}.wrapping_add({})", a, b),
            EvaluatedOperation::WSub(a, b) => write!(f, "{}.wrapping_sub({})", a, b),
            EvaluatedOperation::Maj(a, b, c) => write!(f, "maj({}, {}, {})", a, b, c),
            EvaluatedOperation::Ch(x, y, z) => write!(f, "ch({}, {}, {})", x, y, z),
        }
    }
}
