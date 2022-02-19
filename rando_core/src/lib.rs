mod algebra;
mod database;
pub mod descriptor;
mod shuffles;

// Todo: Find a good place for these
pub enum Oolean {
    False,
    Ool,
    True,
}

pub enum Ntgr {
    Num(u32),
    Infinity,
}

pub struct Relation<'a> {
    _a: &'a (),
}
