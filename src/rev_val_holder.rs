use std::{
    fmt::Display,
    fs::{self, File, OpenOptions},
    io::{BufWriter, Write},
    process::Command,
    rc::Rc,
};

use hashbrown::{HashMap, HashSet};
use indexmap::IndexMap;

use crate::{
    bytecode::BytecodeProgram,
    constraint::{ContextVal, Operation, RevVal},
    evaluated::EvaluatedRevVal,
    mapping::Mapping,
    reverse::Context,
    utils::{AbstractVal, usize_to_var_name},
};

pub struct RevValHolder {
    storage: HashSet<&'static RevVal>,
}

impl RevValHolder {
    pub fn new() -> Self {
        Self {
            storage: HashSet::new(),
        }
    }

    /// Inserts `item` or returns a reference to the canonical `Self` in the set.
    pub fn intern(&mut self, val: RevVal) -> &'static RevVal {
        if let Some(interned) = self.storage.get(&val) {
            return interned;
        }
        let leaked = Box::leak(Box::new(val));
        self.storage.insert(leaked);
        leaked
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum LazyRevVal {
    Ref(&'static RevVal),
    Mapping {
        inner: Box<LazyRevVal>,
        mapping: Mapping,
    },
}

impl Display for LazyRevVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display(f, &IndexMap::new())
    }
}

impl From<RevVal> for LazyRevVal {
    fn from(value: RevVal) -> Self {
        Self::Ref(value.intern())
    }
}

impl From<&'static str> for LazyRevVal {
    fn from(value: &'static str) -> Self {
        RevVal::from(value).into()
    }
}

impl From<Operation> for LazyRevVal {
    fn from(value: Operation) -> Self {
        RevVal::from(value).into()
    }
}

impl From<ContextVal> for LazyRevVal {
    fn from(value: ContextVal) -> Self {
        RevVal::from(value).into()
    }
}

impl LazyRevVal {
    pub fn display(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        cache: &IndexMap<*const RevVal, usize>,
    ) -> std::fmt::Result {
        match self {
            LazyRevVal::Ref(rev_val) => {
                if let Some(i) = cache.get_index_of(&(*rev_val as *const RevVal)) {
                    write!(f, "{}", usize_to_var_name(i))
                } else {
                    write!(f, "{rev_val}")
                }
            }
            LazyRevVal::Mapping { inner, mapping } => {
                inner.display_mapped(f, mapping.clone(), cache)
            }
        }
    }

    pub fn append_to_file(&self, file: &mut BufWriter<File>) {
        match self {
            LazyRevVal::Ref(rev_val) => write!(file, "{rev_val}").unwrap(),
            LazyRevVal::Mapping { inner, mapping } => {
                inner.append_to_file_mapped(file, mapping.clone())
            }
        }
    }

    pub fn into_evaluated(&self) -> Rc<EvaluatedRevVal> {
        EvaluatedRevVal::create_lazy(self)
    }

    pub fn display_mapped(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        mapping: Mapping,
        cache: &IndexMap<*const RevVal, usize>,
    ) -> std::fmt::Result {
        match self {
            LazyRevVal::Ref(rev_val) => rev_val.display_mapped(f, mapping, cache),
            LazyRevVal::Mapping {
                inner,
                mapping: inner_mapping,
            } => {
                // we have to map the mappings
                inner.display_mapped(f, inner_mapping.merge(mapping), cache)
            }
        }
    }

    pub fn append_to_file_mapped(&self, file: &mut BufWriter<File>, mapping: Mapping) {
        match self {
            LazyRevVal::Ref(rev_val) => rev_val.append_to_file_mapped(file, mapping),
            LazyRevVal::Mapping {
                inner,
                mapping: inner_mapping,
            } => {
                // we have to map the mappings
                inner.append_to_file_mapped(file, inner_mapping.merge(mapping))
            }
        }
    }

    pub fn lookup_mapped(
        &self,
        cache: &mut IndexMap<(*const RevVal, Option<Mapping>), usize>,
        mapping: Mapping,
    ) {
        match self {
            LazyRevVal::Ref(rev_val) => rev_val.lookup_mapped(cache, mapping),
            LazyRevVal::Mapping {
                inner,
                mapping: inner_mapping,
            } => inner.lookup_mapped(cache, inner_mapping.merge(mapping)),
        }
    }

    pub fn lookup(&self, cache: &mut IndexMap<(*const RevVal, Option<Mapping>), usize>) {
        match self {
            LazyRevVal::Ref(rev_val) => {
                *cache.entry((*rev_val as *const RevVal, None)).or_insert(0) += 1
            }
            LazyRevVal::Mapping { inner, mapping } => {
                inner.lookup_mapped(cache, mapping.clone());
            }
        }
    }

    pub fn evaluate(&self, context: &Context) -> AbstractVal {
        // convert to bytecode
        let program = BytecodeProgram::create(self);
        program.execute(context)
    }

    // we keep mapping Vec in there in order to apply this mapping to another LazyRevVal in mapping field
    pub fn evaluate_with_mapping(&self, context: &Context, mapping: Mapping) -> AbstractVal {
        match self {
            LazyRevVal::Ref(rev_val) => rev_val.evaluate_with_mapping(context, mapping),
            LazyRevVal::Mapping {
                inner,
                mapping: inner_mapping,
            } => {
                // we have to map the mappings
                inner.evaluate_with_mapping(context, inner_mapping.merge(mapping))
            }
        }
    }

    pub fn get_inner(&self) -> &'static RevVal {
        match self {
            LazyRevVal::Ref(rev_val) => rev_val,
            LazyRevVal::Mapping { inner, mapping: _ } => inner.get_inner(),
        }
    }

    pub fn is_input(&self, index: usize) -> bool {
        if let RevVal::Input(i) = self.get_inner()
            && index == *i
        {
            true
        } else {
            false
        }
    }

    #[must_use]
    pub fn map_context(self, mapping: Mapping) -> Self {
        Self::Mapping {
            inner: Box::new(self),
            mapping,
        }
    }
}
