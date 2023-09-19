use graph::NodeTy;
use slotmap::SlotMap;

mod graph;
pub use graph::{region::RegionBuilder, AlgeOp, CombNode, CombOp, Node, NodeRef, Region};
mod common;
pub use common::Ident;
mod symbol_table;
pub use symbol_table::SymbolTable;
mod debug;
use debug::Span;

pub struct Module;

impl Module {
    pub fn builder() -> ModuleBuilder {
        let mut nodes = SlotMap::with_key();
        let error_node = nodes.insert(Node::Error);

        ModuleBuilder {
            error_node,
            nodes,
            symbols: SymbolTable::new(),
        }
    }
}

pub struct ModuleBuilder {
    //Error node
    pub error_node: NodeRef,
    pub nodes: SlotMap<NodeRef, Node>,

    ///Safes the current symbol table state
    symbols: SymbolTable,
}

impl ModuleBuilder {
    pub fn new_region<'a>(&'a mut self, builder: impl FnOnce(RegionBuilder<'a>)) {}

    pub fn new_node(&mut self, node: impl Into<Node>) -> NodeRef {
        self.nodes.insert(node.into())
    }

    ///Returns if the node exists and is of `ty`. Otherwise returns false
    pub fn is_ty(&self, ty: NodeTy, node: NodeRef) -> bool {
        if let Some(n) = self.nodes.get(node) {
            n.is_ty(ty)
        } else {
            false
        }
    }
}
