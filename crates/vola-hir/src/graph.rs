use std::fmt::Display;

use slotmap::new_key_type;
use tinyvec::ArrayVec;
pub use vola_ast::alge::{BinOp, UnOp};
pub use vola_ast::common::Imm;

use crate::Ident;

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
#[derive(Clone, Debug)]
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

    pub fn unwrap_comb_node(&mut self) -> &mut CombNode {
        if let Node::CombNode(c) = self {
            c
        } else {
            panic!("Was not a CombNode");
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

#[derive(Clone, Debug)]
pub struct Region {
    ///Reference to the input `at` node of this region.
    pub in_at_node: NodeRef,
    ///The defined out node, aka. the lowest combinatorical node in the region.
    pub out: NodeRef,
    ///all inputs (plus the at node) of that region. Per definition always a algebraic node.
    pub in_args: NodeRefs,
    ///all input primitives into that region.
    pub in_prims: NodeRefs,
}

#[derive(Clone, Debug)]
pub enum AlgeOp {
    None,
    At,
    Call(Ident),
    Arg(Ident),
    Ref(Ident),
    BinOp(BinOp),
    UnOp(UnOp),
    Imm(Imm),
    List,
    FieldAccess(Ident),
}

impl Display for AlgeOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AlgeOp::At => write!(f, "At"),
            AlgeOp::None => write!(f, "None"),
            AlgeOp::Arg(i) => write!(f, "Arg_{}", i.0),
            AlgeOp::Call(c) => write!(f, "Call_{}", c.0),
            AlgeOp::Ref(i) => write!(f, "Ref_{}", i.0),
            AlgeOp::List => write!(f, "List"),
            AlgeOp::BinOp(bo) => write!(f, "BinOp_{:?}", bo),
            AlgeOp::UnOp(uo) => write!(f, "UnaryOp_{:?}", uo),
            AlgeOp::FieldAccess(accessed) => {
                write!(f, "FieldAccess_{}", accessed.0)
            }
            AlgeOp::Imm(_im) => write!(f, "Imm"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct AlgeNode {
    pub in_args: NodeRefs,
    pub op: AlgeOp,
    ///In the case of a FieldAccess methode we actually need a reference to a comb node.
    pub child_prim: Option<NodeRef>,
}

impl AlgeNode {
    pub fn new(op: AlgeOp) -> Self {
        AlgeNode {
            in_args: NodeRefs::new(),
            op,
            child_prim: None,
        }
    }
    pub fn with_arg(mut self, nref: NodeRef) -> Self {
        self.in_args.push(nref);
        self
    }
    pub fn with_child(mut self, child: NodeRef) -> Self {
        self.child_prim = Some(child);
        self
    }
}

#[derive(Clone, Debug)]
pub enum CombOp {
    None,
    ///Defines that this calls some else defined op
    OpCall(Ident),
    PrimArg(Ident),
    ///Calls to some primitive definition
    PrimCall(Ident),
    //Creates a new primitive template
    PrimDef(Ident),
    ///Evaluates a primitive template to an actual value.
    PrimEval(Ident),
    ///Mutates the given field with an argument
    PrimFieldMutate {
        ident: Ident,
        op: Option<BinOp>,
    },
}

impl Display for CombOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CombOp::None => write!(f, "None"),
            CombOp::OpCall(c) => write!(f, "CombOp_{}", c.0),
            CombOp::PrimArg(c) => write!(f, "PrimArg_{}", c.0),
            CombOp::PrimCall(c) => write!(f, "PrimCall_{}", c.0),
            CombOp::PrimDef(ident) => write!(f, "PrimDef_{}", ident.0),
            CombOp::PrimEval(ident) => write!(f, "PrimEval_{}", ident.0),
            CombOp::PrimFieldMutate { ident, op: _ } => write!(f, "PrimFieldMutate_{}", ident.0),
        }
    }
}

#[derive(Clone, Debug)]
pub struct CombNode {
    ///The "At" value must always be given to a comb node, since that is implicitly traversed
    pub in_at: NodeRef,
    pub in_args: NodeRefs,
    pub in_children: NodeRefs,
    pub op: CombOp,
}

impl CombNode {
    pub fn new(op: CombOp, at_ref: NodeRef) -> Self {
        CombNode {
            in_at: at_ref,
            in_args: NodeRefs::new(),
            in_children: NodeRefs::new(),
            op,
        }
    }
    pub fn with_arg(mut self, nref: NodeRef) -> Self {
        self.in_args.push(nref);
        self
    }
    pub fn with_child(mut self, nref: NodeRef) -> Self {
        self.in_children.push(nref);
        self
    }
}
