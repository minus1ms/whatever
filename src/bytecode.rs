use std::{fmt::Display, iter, ops::Deref, rc::Rc};

use hashbrown::HashMap;

use crate::{
    constraint::{ContextVal, Operation, RevVal},
    custom_opcode::OpcodeInterface,
    mapping::Mapping,
    rev_val_holder::LazyRevVal,
    reverse::Context,
    sha256::{big_sigma0, big_sigma1, ch, maj},
    stack::Stack,
    utils::AbstractVal,
    var::Var,
};

#[derive(Clone)]
pub enum Bytecode {
    StartMapping(BytecodeSingleMapping),
    EndMapping,
    // get one of the input values on the stack
    Input(usize),
    Context(Var),
    AvLoad(Var, usize),
    AvLoadCtx(Var),
    Const(u32),
    BigSigma0,
    BigSigma1,
    Xor,
    And,
    WAdd,
    WSub,
    Maj,
    Ch,
    // id as the second one
    Custom(Rc<dyn OpcodeInterface>, usize),
}

impl std::fmt::Debug for Bytecode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Bytecode::*;
        match self {
            StartMapping(_) => write!(f, "StartMapping(...)"),
            EndMapping => write!(f, "EndMapping"),
            Input(i) => f.debug_tuple("Input").field(i).finish(),
            Context(v) => f.debug_tuple("Context").field(v).finish(),
            AvLoad(v, i) => f.debug_tuple("AvLoad").field(v).field(i).finish(),
            AvLoadCtx(v) => f.debug_tuple("AvLoadCtx").field(v).finish(),
            Const(c) => f.debug_tuple("Const").field(c).finish(),
            BigSigma0 => write!(f, "BigSigma0"),
            BigSigma1 => write!(f, "BigSigma1"),
            Xor => write!(f, "Xor"),
            And => write!(f, "And"),
            WAdd => write!(f, "WAdd"),
            WSub => write!(f, "WSub"),
            Maj => write!(f, "Maj"),
            Ch => write!(f, "Ch"),
            Custom(_, id) => write!(f, "Custom({id})"),
        }
    }
}

impl From<Var> for Bytecode {
    fn from(value: Var) -> Self {
        Self::Context(value)
    }
}

#[derive(Debug, Clone)]
pub struct BytecodeProgram {
    pub code: Vec<Bytecode>,
}

impl From<Var> for BytecodeProgram {
    fn from(value: Var) -> Self {
        Bytecode::from(value).into()
    }
}

impl From<Bytecode> for BytecodeProgram {
    fn from(value: Bytecode) -> Self {
        BytecodeProgram::from_single_code(value)
    }
}

impl Display for BytecodeProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(i) = self.is_any_input() {
            write!(f, "(input {i})")
        } else {
            let mut stack = vec![];
            fn handle_code(
                f: &mut std::fmt::Formatter<'_>,
                stack: &mut Vec<String>,
                code: &Bytecode,
            ) {
                match code {
                    Bytecode::StartMapping(mapping) => {
                        let mut res = String::from("{\nlet (");
                        for (name, _) in &mapping[0..mapping.len() - 1] {
                            res.push_str(&format!("{name}, "));
                        }
                        res.push_str(&format!("{}) = (", mapping.last().unwrap().0));
                        for (_, val) in &mapping[0..mapping.len() - 1] {
                            res.push_str(&format!("{val}, "));
                        }
                        res.push_str(&format!("{});\n", mapping.last().unwrap().1));
                        stack.push(res)
                    }
                    Bytecode::EndMapping => {
                        let val = stack.pop().unwrap();
                        let mapping = stack.pop().unwrap();
                        stack.push(format!("{mapping}{val}\n}}"));
                    }
                    Bytecode::Context(x) => {
                        stack.push(x.to_string());
                    }
                    Bytecode::AvLoad(x, i) => {
                        stack.push(format!("{x}[{i}]"));
                    }
                    Bytecode::AvLoadCtx(x) => {
                        let i = stack.pop().unwrap();
                        stack.push(format!("{x}[{i}]"));
                    }
                    Bytecode::Const(x) => {
                        stack.push(format!("{x}"));
                    }
                    Bytecode::BigSigma0 => {
                        assert!(stack.len() >= 1);
                        let x = stack.pop().unwrap();
                        stack.push(format!("big_sigma0({x})"));
                    }
                    Bytecode::BigSigma1 => {
                        assert!(stack.len() >= 1);
                        let x = stack.pop().unwrap();
                        stack.push(format!("big_sigma1({x})"));
                    }
                    Bytecode::Xor => todo!(),
                    Bytecode::And => todo!(),
                    Bytecode::WAdd => {
                        assert!(stack.len() >= 2);
                        let y = stack.pop().unwrap();
                        let x = stack.pop().unwrap();
                        stack.push(format!("{x}.wrapping_add({y})"));
                    }
                    Bytecode::WSub => {
                        assert!(stack.len() >= 2);
                        let y = stack.pop().unwrap();
                        let x = stack.pop().unwrap();
                        stack.push(format!("{x}.wrapping_sub({y})"));
                    }
                    Bytecode::Maj => {
                        assert!(stack.len() >= 3);
                        let z = stack.pop().unwrap();
                        let y = stack.pop().unwrap();
                        let x = stack.pop().unwrap();
                        stack.push(format!("maj({x}, {y}, {z})"));
                    }
                    Bytecode::Ch => {
                        assert!(stack.len() >= 3);
                        let z = stack.pop().unwrap();
                        let y = stack.pop().unwrap();
                        let x = stack.pop().unwrap();
                        stack.push(format!("ch({x}, {y}, {z})"));
                    }
                    Bytecode::Input(_) => todo!(),
                    Bytecode::Custom(_, id) => stack.push(format!("Custom({id})")),
                }
            }

            for code in &self.code {
                handle_code(f, &mut stack, code);
            }

            // do final print
            write!(f, "{}", stack.pop().unwrap())
        }
    }
}

pub static mut DEBUG_EXECUTE: bool = false;

impl BytecodeProgram {
    // pub fn create(tree: &LazyRevVal) -> Self {
    //     let mut code = Vec::new();
    //     Self::from_lazy_tree(tree, &mut code);
    //     Self { code }
    // }

    pub fn execute(&self, context: &Context) -> AbstractVal {
        unsafe {
            if DEBUG_EXECUTE {
                println!("executing {}", self.code.len());
                println!("{:?}", context);
            }
        }
        let mut stack = Stack::new();
        let mut base_mapping = Vec::new();

        fn handle_code<'a>(
            context: &Context,
            stack: &mut Stack,
            acumulated_mapping: &mut Vec<BytecodeSingleMapping>,
            code: &Bytecode,
        ) {
            match code {
                Bytecode::StartMapping(mapping) => {
                    acumulated_mapping.push(mapping.clone());
                }
                Bytecode::EndMapping => {
                    acumulated_mapping.pop().unwrap();
                }
                Bytecode::Context(x) => {
                    if let Some((i, prog)) =
                        acumulated_mapping
                            .iter()
                            .enumerate()
                            .rev()
                            .find_map(|(i, mapping)| {
                                mapping
                                    .iter()
                                    .find(|(name, _)| name == x)
                                    .map(|(_, x)| (i, x))
                            })
                    {
                        let mut acumulated_mapping = acumulated_mapping[..i].to_vec();
                        for ele in &prog.code {
                            handle_code(context, stack, &mut acumulated_mapping, ele);
                        }
                    } else {
                        stack.push(context.get(*x));
                    }
                }
                Bytecode::AvLoad(x, i) => {
                    stack.push(context.get(*x).to_vec()[*i].clone());
                }
                Bytecode::AvLoadCtx(x) => {
                    let i = stack.pop().to_u32();
                    stack.push(context.get(*x).to_vec()[i as usize].clone());
                }
                Bytecode::Const(x) => {
                    stack.push(AbstractVal::U32(*x));
                }
                Bytecode::BigSigma0 => {
                    assert!(stack.len() >= 1);
                    let x = stack.pop().to_u32();
                    stack.push(AbstractVal::U32(big_sigma0(x)));
                }
                Bytecode::BigSigma1 => {
                    assert!(stack.len() >= 1);
                    let x = stack.pop().to_u32();
                    stack.push(AbstractVal::U32(big_sigma1(x)));
                }
                Bytecode::Xor => todo!(),
                Bytecode::And => todo!(),
                Bytecode::WAdd => {
                    assert!(stack.len() >= 2);
                    let y = stack.pop().to_u32();
                    let x = stack.pop().to_u32();
                    stack.push(AbstractVal::U32(x.wrapping_add(y)));
                }
                Bytecode::WSub => {
                    assert!(stack.len() >= 2);
                    let y = stack.pop().to_u32();
                    let x = stack.pop().to_u32();
                    stack.push(AbstractVal::U32(x.wrapping_sub(y)));
                }
                Bytecode::Maj => {
                    assert!(stack.len() >= 3);
                    let z = stack.pop().to_u32();
                    let y = stack.pop().to_u32();
                    let x = stack.pop().to_u32();
                    stack.push(AbstractVal::U32(maj(x, y, z)));
                }
                Bytecode::Ch => {
                    assert!(stack.len() >= 3);
                    let z = stack.pop().to_u32();
                    let y = stack.pop().to_u32();
                    let x = stack.pop().to_u32();
                    stack.push(AbstractVal::U32(ch(x, y, z)));
                }
                Bytecode::Input(_) => todo!(),
                Bytecode::Custom(opcode_interface, _) => {
                    stack.push(opcode_interface.apply(context, acumulated_mapping))
                }
            }
        }
        for ele in &self.code {
            handle_code(context, &mut stack, &mut base_mapping, ele);
        }
        let res = stack.pop();
        unsafe {
            if DEBUG_EXECUTE {
                println!("res: {:?}", res);
            }
        }
        res
    }

    // pub fn from_lazy_tree(tree: &LazyRevVal, code: &mut Vec<Bytecode>) {
    //     match tree {
    //         LazyRevVal::Ref(rev_val) => Self::from_tree(rev_val, code),
    //         LazyRevVal::Mapping { inner, mapping } => {
    //             Self::from_lazy_tree_mapped(inner, mapping, code)
    //         }
    //     }
    // }

    // pub fn from_lazy_tree_mapped(tree: &LazyRevVal, mapping: &Mapping, code: &mut Vec<Bytecode>) {
    //     match tree {
    //         LazyRevVal::Ref(rev_val) => Self::from_tree_mapped(rev_val, mapping, code),
    //         LazyRevVal::Mapping {
    //             inner,
    //             mapping: inner_mapping,
    //         } => Self::from_lazy_tree_mapped(inner, &inner_mapping.merge(mapping.clone()), code),
    //     }
    // }

    // pub fn from_tree(tree: &RevVal, code: &mut Vec<Bytecode>) {
    //     match tree {
    //         RevVal::Output => todo!(),
    //         RevVal::Input(_) => todo!(),
    //         RevVal::Context(ctx_val) => {
    //             Self::from_ctx_val(ctx_val, code);
    //         }
    //         RevVal::Const(x) => code.push(Bytecode::Const(*x)),
    //         RevVal::Operation(operation) => {
    //             Self::from_operation(operation, code);
    //         }
    //     }
    // }

    // fn from_tree_mapped(tree: &RevVal, mapping: &Mapping, code: &mut Vec<Bytecode>) {
    //     match tree {
    //         RevVal::Output => todo!(),
    //         RevVal::Input(_) => todo!(),
    //         RevVal::Context(context_val) => Self::from_ctx_val_mapped(context_val, mapping, code),
    //         RevVal::Const(x) => code.push(Bytecode::Const(*x)),
    //         RevVal::Operation(operation) => Self::from_operation_mapped(operation, mapping, code),
    //     }
    // }

    // pub fn from_ctx_val(ctx: &ContextVal, code: &mut Vec<Bytecode>) {
    //     match ctx {
    //         ContextVal::Name(x) => code.push(Bytecode::Context(x)),
    //         ContextVal::Array(x, i) => {
    //             code.push(Bytecode::AvLoad(x, *i));
    //         }
    //         ContextVal::ArrayCtx(x, lazy_rev_val) => {
    //             Self::from_lazy_tree(lazy_rev_val, code);
    //             code.push(Bytecode::AvLoadCtx(x));
    //         }
    //     }
    // }

    // fn from_ctx_val_mapped(context_val: &ContextVal, mapping: &Mapping, code: &mut Vec<Bytecode>) {
    //     match context_val {
    //         ContextVal::Name(x) => {
    //             if let Some(val) = mapping.apply(x) {
    //                 Self::from_lazy_tree(&val, code);
    //             } else {
    //                 code.push(Bytecode::Context(x))
    //             }
    //         }
    //         ContextVal::Array(x, i) => {
    //             if let Some(val) = mapping.apply(x) {
    //                 // never mapped now
    //                 todo!()
    //             } else {
    //                 code.push(Bytecode::AvLoad(x, *i))
    //             }
    //         }
    //         ContextVal::ArrayCtx(x, lazy_rev_val) => {
    //             if let Some(val) = mapping.apply(x) {
    //                 // never mapped now
    //                 todo!()
    //             }
    //             Self::from_lazy_tree_mapped(lazy_rev_val, mapping, code);
    //             code.push(Bytecode::AvLoadCtx(x));
    //         }
    //     }
    // }

    // fn from_operation(operation: &Operation, code: &mut Vec<Bytecode>) {
    //     match operation {
    //         Operation::BigSigma0(lazy_rev_val) => {
    //             Self::from_lazy_tree(lazy_rev_val, code);
    //             code.push(Bytecode::BigSigma0);
    //         }
    //         Operation::BigSigma1(lazy_rev_val) => {
    //             Self::from_lazy_tree(lazy_rev_val, code);
    //             code.push(Bytecode::BigSigma1);
    //         }
    //         Operation::Xor(lazy_rev_val, lazy_rev_val1) => todo!(),
    //         Operation::And(lazy_rev_val, lazy_rev_val1) => todo!(),
    //         Operation::WAdd(lazy_rev_val, lazy_rev_val1) => {
    //             Self::from_lazy_tree(lazy_rev_val, code);
    //             Self::from_lazy_tree(lazy_rev_val1, code);
    //             code.push(Bytecode::WAdd);
    //         }
    //         Operation::WSub(lazy_rev_val, lazy_rev_val1) => {
    //             Self::from_lazy_tree(lazy_rev_val, code);
    //             Self::from_lazy_tree(lazy_rev_val1, code);
    //             code.push(Bytecode::WSub);
    //         }
    //         Operation::Maj(lazy_rev_val, lazy_rev_val1, lazy_rev_val2) => {
    //             Self::from_lazy_tree(lazy_rev_val, code);
    //             Self::from_lazy_tree(lazy_rev_val1, code);
    //             Self::from_lazy_tree(lazy_rev_val2, code);
    //             code.push(Bytecode::Maj);
    //         }
    //         Operation::Ch(lazy_rev_val, lazy_rev_val1, lazy_rev_val2) => {
    //             Self::from_lazy_tree(lazy_rev_val, code);
    //             Self::from_lazy_tree(lazy_rev_val1, code);
    //             Self::from_lazy_tree(lazy_rev_val2, code);
    //             code.push(Bytecode::Ch);
    //         }
    //     }
    // }

    // fn from_operation_mapped(operation: &Operation, mapping: &Mapping, code: &mut Vec<Bytecode>) {
    //     match operation {
    //         Operation::BigSigma0(lazy_rev_val) => {
    //             Self::from_lazy_tree_mapped(lazy_rev_val, mapping, code);
    //             code.push(Bytecode::BigSigma0);
    //         }
    //         Operation::BigSigma1(lazy_rev_val) => {
    //             Self::from_lazy_tree_mapped(lazy_rev_val, mapping, code);
    //             code.push(Bytecode::BigSigma1);
    //         }
    //         Operation::Xor(lazy_rev_val, lazy_rev_val1) => todo!(),
    //         Operation::And(lazy_rev_val, lazy_rev_val1) => todo!(),
    //         Operation::WAdd(lazy_rev_val, lazy_rev_val1) => {
    //             Self::from_lazy_tree_mapped(lazy_rev_val, mapping, code);
    //             Self::from_lazy_tree_mapped(lazy_rev_val1, mapping, code);
    //             code.push(Bytecode::WAdd);
    //         }
    //         Operation::WSub(lazy_rev_val, lazy_rev_val1) => {
    //             Self::from_lazy_tree_mapped(lazy_rev_val, mapping, code);
    //             Self::from_lazy_tree_mapped(lazy_rev_val1, mapping, code);
    //             code.push(Bytecode::WSub);
    //         }
    //         Operation::Maj(lazy_rev_val, lazy_rev_val1, lazy_rev_val2) => {
    //             Self::from_lazy_tree_mapped(lazy_rev_val, mapping, code);
    //             Self::from_lazy_tree_mapped(lazy_rev_val1, mapping, code);
    //             Self::from_lazy_tree_mapped(lazy_rev_val2, mapping, code);
    //             code.push(Bytecode::Maj);
    //         }
    //         Operation::Ch(lazy_rev_val, lazy_rev_val1, lazy_rev_val2) => {
    //             Self::from_lazy_tree_mapped(lazy_rev_val, mapping, code);
    //             Self::from_lazy_tree_mapped(lazy_rev_val1, mapping, code);
    //             Self::from_lazy_tree_mapped(lazy_rev_val2, mapping, code);
    //             code.push(Bytecode::Ch);
    //         }
    //     }
    // }

    pub fn from_code(code: Vec<Bytecode>) -> BytecodeProgram {
        Self { code }
    }

    pub fn from_single_code(code: Bytecode) -> BytecodeProgram {
        Self { code: vec![code] }
    }

    pub fn is_input(&self, i: usize) -> bool {
        for ele in self.code.iter().rev() {
            match ele {
                Bytecode::Input(x) => {
                    if *x == i {
                        return true;
                    }
                }
                Bytecode::EndMapping => {}
                _ => {
                    todo!()
                }
            }
        }
        false
    }

    pub fn is_any_input(&self) -> Option<usize> {
        for ele in self.code.iter().rev() {
            match ele {
                Bytecode::Input(x) => {
                    return Some(*x);
                }
                Bytecode::EndMapping => {}
                _ => {
                    // can happen in strange places, keep an eye
                    return None;
                }
            }
        }
        None
    }

    // might optimize by removing values from mapping not used in code
    pub fn map_context_lazy(mut self, mapping: BytecodeSingleMapping) -> BytecodeProgram {
        let mut code = Vec::with_capacity(self.code.len() + 2);
        code.push(Bytecode::StartMapping(mapping));
        code.extend(self.code);
        code.push(Bytecode::EndMapping);
        self.code = code;
        self
    }

    pub fn extend(mut self, other: BytecodeProgram) -> Self {
        self.code.extend(other.code);
        self
    }

    pub fn add(mut self, code: Bytecode) -> BytecodeProgram {
        self.code.push(code);
        self
    }
}

pub type BytecodeSingleMapping = Rc<Vec<(Var, BytecodeProgram)>>;

pub fn eval(mappings: &[BytecodeSingleMapping], ctx: &Context, name: Var) -> AbstractVal {
    let mut found: Option<BytecodeProgram> = None;
    for mapping in mappings.iter().rev() {
        if let Some(fnd) = found {
            found = Some(fnd.map_context_lazy(mapping.clone()));
        } else {
            if let Some((_, got)) = mapping.iter().find(|(x, _)| *x == name) {
                found = Some(got.clone())
            }
        }
    }
    if let Some(found) = found {
        println!("mappings: {}", mappings.len());
        // for mapping in mappings {
        //     println!("mapping: {:?}", mapping);
        // }
        println!("name: {name}");
        println!("ctx: {:?}", ctx);
        println!("{:?}", found);
        found.execute(ctx)
    } else {
        ctx.get(name)
    }
}

#[derive(Debug, Clone)]
pub struct BytecodeMapping {
    content: Vec<BytecodeSingleMapping>,
}

impl BytecodeMapping {
    pub fn create(mapping: BytecodeSingleMapping) -> BytecodeMapping {
        Self {
            content: vec![mapping],
        }
    }

    pub fn merge(&self, mapping: BytecodeMapping) -> Self {
        Self {
            content: [self.content.clone(), mapping.content].concat(),
        }
    }

    fn remove(&mut self) {
        self.content.remove(0);
    }

    pub fn apply(&self, name: Var) -> Option<BytecodeProgram> {
        let mut found: Option<BytecodeProgram> = None;
        for mapping in &self.content {
            if let Some(fnd) = found {
                found = Some(fnd.map_context_lazy(mapping.clone()));
            } else {
                if let Some((_, got)) = mapping.iter().find(|(x, _)| *x == name) {
                    found = Some(got.clone())
                }
            }
        }
        found
    }

    fn push_front(&mut self, item: BytecodeSingleMapping) {
        self.content.insert(0, item);
    }
}
