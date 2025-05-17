use std::fmt::Display;

use crate::{
    constraint::ConstraintId,
    sha256::{big_sigma0, big_sigma1},
};

#[derive(Debug)]
pub enum RevRes<T> {
    Normal(T),
    ConstraintsChanged(ConstraintId),
}

impl<T> From<T> for RevRes<T> {
    fn from(value: T) -> Self {
        RevRes::Normal(value)
    }
}

#[derive(Clone)]
pub enum AbstractVal {
    U32(u32),
    Arr(Vec<AbstractVal>),
}

impl Display for AbstractVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AbstractVal::U32(x) => write!(f, "{x}u32"),
            AbstractVal::Arr(vals) => {
                write!(f, "[")?;
                for val in &vals[0..vals.len() - 1] {
                    write!(f, "{val}, ")?;
                }
                write!(f, "{}", vals.last().unwrap())?;
                write!(f, "]")
            }
        }
    }
}

impl AbstractVal {
    pub fn wrapping_add(self, other: AbstractVal) -> AbstractVal {
        AbstractVal::U32(self.to_u32().wrapping_add(other.to_u32()))
    }

    pub fn wrapping_sub(self, other: AbstractVal) -> AbstractVal {
        AbstractVal::U32(self.to_u32().wrapping_sub(other.to_u32()))
    }

    pub fn big_sigma0(self) -> AbstractVal {
        AbstractVal::U32(big_sigma0(self.to_u32()))
    }

    pub fn big_sigma1(self) -> AbstractVal {
        AbstractVal::U32(big_sigma1(self.to_u32()))
    }
}

impl AbstractVal {
    pub fn to_u32(&self) -> u32 {
        match self {
            AbstractVal::U32(x) => *x,
            _ => unreachable!(),
        }
    }

    pub fn to_vec(&self) -> &[AbstractVal] {
        match self {
            AbstractVal::Arr(items) => items,
            _ => unreachable!(),
        }
    }
}

impl Into<u32> for AbstractVal {
    fn into(self) -> u32 {
        match self {
            AbstractVal::U32(x) => x,
            _ => unreachable!(),
        }
    }
}

impl Into<AbstractVal> for u32 {
    fn into(self) -> AbstractVal {
        AbstractVal::U32(self)
    }
}

pub fn usize_to_var_name(idx: usize) -> String {
    format!("var_{}", idx)
}
