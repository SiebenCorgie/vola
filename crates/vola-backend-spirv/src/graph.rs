use rvsdg::{
    edge::LangEdge,
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    smallvec::SmallVec,
};

#[derive(LangNode)]
pub struct SpvOp {
    #[inputs]
    inputs: SmallVec<[Input; 3]>,
    #[outputs]
    outputs: SmallVec<[Output; 3]>,
}

pub enum SpvEdg {
    Value,
    State,
}
impl LangEdge for SpvEdg {
    fn value_edge() -> Self {
        Self::Value
    }
    fn state_edge() -> Self {
        Self::State
    }

    fn is_value_edge(&self) -> bool {
        if let Self::Value = self {
            true
        } else {
            false
        }
    }

    fn is_state_edge(&self) -> bool {
        if let Self::State = self {
            true
        } else {
            false
        }
    }
}
