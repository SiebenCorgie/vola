use crate::{edge::LangEdge, NodeRef, Rvsdg};

pub enum VSEdge {
    ///Value Edge, representing data dependency of one node to another
    Value,
    ///State Edge, representing a state dependency. Meaning *This node needs to be executed before this one*.
    ///
    /// Can for instance be used to organise ordering of functions with side-effects.
    State,
}

impl LangEdge for VSEdge {
    fn is_state_edge(&self) -> bool {
        if let VSEdge::State { .. } = &self {
            true
        } else {
            false
        }
    }
    fn is_value_edge(&self) -> bool {
        if let VSEdge::Value { .. } = &self {
            true
        } else {
            false
        }
    }

    fn state_edge() -> Self {
        Self::State
    }

    fn value_edge() -> Self {
        Self::Value
    }
}

///Most common RVSDG representation where each edge is either a data, or state dependency.
pub type CommonRvsdg<N> = Rvsdg<N, VSEdge>;
