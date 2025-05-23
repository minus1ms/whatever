use std::{ops::Deref, sync::Arc};

use crate::rev_val_holder::LazyRevVal;

// we use Vec to keep Hash working
type SingleMapping = Arc<Vec<(&'static str, Box<LazyRevVal>)>>;

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Mapping {
    content: Vec<SingleMapping>,
}

impl Mapping {
    pub fn create(mapping: SingleMapping) -> Self {
        Self {
            content: vec![mapping],
        }
    }

    pub fn merge(&self, mapping: Mapping) -> Self {
        Self {
            content: [self.content.clone(), mapping.content].concat(),
        }
    }

    // pub fn apply(&self, name: &'static str) -> Option<LazyRevVal> {
    //     let mut found: Option<LazyRevVal> = None;
    //     for mapping in &self.content {
    //         if let Some(fnd) = found {
    //             found = Some(fnd.map_context(Mapping::create(mapping.clone())));
    //         } else {
    //             if let Some((_, got)) = mapping.iter().find(|(x, _)| *x == name) {
    //                 found = Some(got.deref().clone())
    //             }
    //         }
    //     }
    //     found
    // }
}
