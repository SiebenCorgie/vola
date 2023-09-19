use slotmap::SlotMap;

mod graph;
pub use graph::{AlgeOp, CombNode, CombOp, Node, NodeRef, Region};
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

        ModuleBuilder { error_node, nodes }
    }
}

pub struct ModuleBuilder {
    //Error node
    pub error_node: NodeRef,
    pub nodes: SlotMap<NodeRef, Node>,
}
