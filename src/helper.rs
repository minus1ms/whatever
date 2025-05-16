use crate::constraint::RevVal;

pub struct HelperContext {
    pub a: Option<u32>,
    pub b: Option<u32>,
    pub c: Option<u32>,
    pub d: Option<u32>,
    pub e: Option<u32>,
    pub f: Option<u32>,
    pub g: Option<u32>,
    pub h_temp: Option<u32>,
}

pub struct ToValue {
    order: Option<Order>,
    desired: Desired,
}

pub enum Current {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    Htemp,
    T1,
    T2,
    I,
}

// where in a single interation
pub enum Order {
    Former,
    Latter,
}

// iteration
pub enum Desired {
    Before,
    After,
    Current,
}

pub fn get_val(current: Current, order: Order, ctx: HelperContext) -> u32 {
    todo!()
}

pub fn get_rev_val(current: Current, order: Order) -> RevVal {
    todo!()
}
