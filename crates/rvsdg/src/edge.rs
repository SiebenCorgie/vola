use crate::NodeRef;

///Port index references a certain type of port on a node. Note that
/// [Node](crate::nodes::Node) implements `Index`/`IndexMut` for that type, as well as `get()`, `get_mut`, `try_get()`, `try_get_mut()`.
///
/// On a lower level we copy the exact addressing scheme of the source paper. Therefor some port indices can overlab, for instance
/// on a loop node `Input(1)` might also be `EntryVar(var_index = 0, tuple_index=None)`.
pub enum PortIndex {
    Input(usize),
    Output(usize),
    ///Represents the context_variable `var_index`, and the `tuple_index`-th element of the ContexVar tuple defined in _Definition 4, 6, 7_
    /// of the source paper.
    ContextVar {
        var_index: usize,
        ///Must be either 0 or 1
        tuple_index: usize,
    },

    ///Represents the recursion_variable `var_index`, and the `tuple_index`-th element of the RV tuple defined in _Definition 8_
    /// of the source paper.
    RecursionVar {
        var_index: usize,
        ///Must be $<=2$
        tuple_index: usize,
    },
    ///Represents the entry-variable of a [γ-node](crate::nodes::GammaNode) as defined in _Definition 1_ of the source paper.
    EntryVar {
        ///Denotes the n-th entry variable
        var_index: usize,
        ///is the input port of EV (tuple element 0 in the definition), or the ev-argument of the `region-index`-th inner-region of the node.
        ///
        /// Must be None, or < region count of the GammaNode.
        region_index: Option<usize>,
    },

    ///Represents the exit-variable of a [γ-node](crate::nodes::GammaNode) as defined in _Definition 2_ of the source paper.
    ExitVar {
        ///Denotes the n-th exit variable
        var_index: usize,
        ///is the output port of EV (last tuple element in the definition), or the ex-result of the `region-index`-th inner-region of the node.
        ///
        /// Must be None, or < region count of the GammaNode.
        region_index: Option<usize>,
    },

    ///Represents the LoopVariable of a [θ-Node](crate:node::ThetaNode) as described in _Definition 3_ of the source paper.
    ///
    /// We adress the `var_index`-th loop variable, where `tuple_index` corresponds to the tuple
    /// (input=0, argument=1, result=2, output=3) described in the paper.
    LoopVar {
        var_index: usize,
        ///Must be $< 4$.
        tuple_index: usize,
    },
}

///An edge in a RVSDG is always a directed edge from `src` to `dst`. It contains a language specific
/// edge-type `E`, that represent the dependency type. In general there are at least `State` and `Value` edges, but a
/// language is free to represent more sophisticated dependencies through those types.
///
/// # Rational behind the src-index / dst-index.
///
/// On a surface level it's kinda shaky to store some _port-index_ on a node that can, potentially change its output configuration while being
/// build.
/// However, in practice the only type of port that frequently changes are context-, loop-, and recursion-variables, those variables are, however only ever
/// appended, never inserted. The input/output configuration is stable.
/// For instance a loop only has a single _real_ input, the loop-criteria. Or a hypothetical _simple_ `LoadOp` will always have
/// one input (the load address), and two outputs (load_value, and load_state). So when we reference the loaded value via index 0, this will
/// stay stable. Similarly, if we use input 0 of a loop and assume that it is the loop-criteria, this ought to be right.
pub struct Edge<E: LangEdge + 'static> {
    pub src: NodeRef,
    ///Index into the `src`-nodes output array.
    pub src_index: PortIndex,
    pub dst: NodeRef,
    ///Index into the `dst`-node input array.
    pub dst_index: PortIndex,
    pub ty: E,
}

pub trait LangEdge {
    ///Should create a
    fn value_edge() -> Self;
    fn state_edge() -> Self;
    fn is_value_edge(&self) -> bool;
    fn is_state_edge(&self) -> bool;
}
