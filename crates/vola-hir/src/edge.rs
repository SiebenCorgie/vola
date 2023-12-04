use rvsdg::edge::LangEdge;

pub enum HirEdge {
    State,
    Value,
}

impl LangEdge for HirEdge {
    fn state_edge() -> Self {
        Self::State
    }
    fn value_edge() -> Self {
        Self::Value
    }
    fn is_state_edge(&self) -> bool {
        if let HirEdge::State = self {
            true
        } else {
            false
        }
    }
    fn is_value_edge(&self) -> bool {
        if let HirEdge::Value = self {
            true
        } else {
            false
        }
    }
}
