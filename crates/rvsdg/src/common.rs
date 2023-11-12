use crate::{NodeRef, Rvsdg};

pub enum VSEdge {
    ///Value Edge, representing data dependency of one node to another
    Value { src: NodeRef, dst: NodeRef },
    ///State Edge, representing a state dependency. Meaning *This node needs to be executed before this one*.
    ///
    /// Can for instance be used to organise ordering of functions with side-effects.
    State { src: NodeRef, dst: NodeRef },
}

///Most common RVSDG representation where each edge is either a data, or state dependency.
pub type CommonRvsdg<N> = Rvsdg<N, VSEdge>;
