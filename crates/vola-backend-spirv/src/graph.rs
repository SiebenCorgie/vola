use rvsdg::{edge::LangEdge, nodes::LangNode};

pub struct SpvOp;

impl LangNode for SpvOp {
    fn inputs(&self) -> &[rvsdg::region::Input] {
        &[]
    }

    fn inputs_mut(&mut self) -> &mut [rvsdg::region::Input] {
        &mut []
    }

    fn outputs(&self) -> &[rvsdg::region::Output] {
        &[]
    }

    fn outputs_mut(&mut self) -> &mut [rvsdg::region::Output] {
        &mut []
    }
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
