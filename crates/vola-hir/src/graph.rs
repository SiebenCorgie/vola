use slotmap::new_key_type;
use tinyvec::ArrayVec;

pub(crate) mod region;

new_key_type! {pub struct NodeRef;}

pub type NodeRefs = ArrayVec<[NodeRef; 3]>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NodeTy {
    Region,
    AlgeNode,
    CombNode,
    Error,
}

///All Node types we can express. This is intentionally kept really small.
pub enum Node {
    ///Similarly defined as MLIR's *Region*. This is a node with a defined entry block and signature,
    /// as well as a defined out-block and signature. The internal structure of that region can
    /// be any graph of nodes that addhers to the entry and outro specification.
    ///
    /// Always has an arithmetical `@` input, and a single `primitive` output.
    Region(Region),

    ///Algebraic Node, defined by a set of arguments and the output.
    /// Per definition the node can have `N` inputs and '1` output, it self.
    AlgeNode(AlgeNode),

    ///Combinatorical (Atomic?) Node, characterised by `N` arithmetical inputs, 'M' combinatorical inputs and `1`
    /// combinatorical output.
    ///
    /// Always has an arithmetical `@` input, and a single `primitive` output.
    CombNode(CombNode),

    ///The error node, used whenever a error was found
    Error,
}

impl Node {
    pub fn is_ty(&self, ty: NodeTy) -> bool {
        self.ty() == ty
    }

    pub fn ty(&self) -> NodeTy {
        match self {
            Node::Error => NodeTy::Error,
            Node::AlgeNode(_) => NodeTy::AlgeNode,
            Node::Region(_) => NodeTy::Region,
            Node::CombNode(_) => NodeTy::CombNode,
        }
    }
}

impl From<Region> for Node {
    fn from(value: Region) -> Self {
        Node::Region(value)
    }
}

impl From<AlgeNode> for Node {
    fn from(value: AlgeNode) -> Self {
        Node::AlgeNode(value)
    }
}

impl From<CombNode> for Node {
    fn from(value: CombNode) -> Self {
        Node::CombNode(value)
    }
}

pub struct Region {
    ///Reference to the input `at` node of this region.
    pub in_at_node: NodeRef,
    ///The defined out node, aka. the lowest combinatorical node in the region.
    pub out: NodeRef,
    ///all inputs (plus the at node) of that region. Per definition always a algebraic node.
    pub in_args: NodeRefs,
}

pub enum AlgeOp {
    None,
    At,
}

pub struct AlgeNode {
    pub in_args: NodeRefs,
    pub op: AlgeOp,
}

impl AlgeNode {
    pub fn new(op: AlgeOp) -> Self {
        AlgeNode {
            in_args: NodeRefs::new(),
            op,
        }
    }
    pub fn with_arg(mut self, nref: NodeRef) -> Self {
        self.in_args.push(nref);
        self
    }
}

pub enum CombOp {
    None,
}

pub struct CombNode {
    pub in_args: NodeRefs,
    pub in_children: NodeRefs,
    pub op: CombOp,
}
