use crate::{NodeRef};

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Ident(pub String);

impl From<String> for Ident {
    fn from(value: String) -> Self {
        Ident(value)
    }
}

impl From<&str> for Ident {
    fn from(value: &str) -> Self {
        Ident(value.to_owned())
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum EntryPointType {
    Field,
    Op,
    Prim,
    Alge,
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct EntryPoint {
    pub root_node: NodeRef,
    pub ty: EntryPointType,
}
