use vola_ast::comb::OpNode;

use crate::{graph::NodeTy, Ident, ModuleBuilder, Node, NodeRef, Region};

use super::{AlgeNode, NodeRefs};

pub struct RegionBuilder<'a> {
    module: &'a mut ModuleBuilder,

    //Preregistered at_ref
    at_ref: NodeRef,
    args: NodeRefs,
    out: Option<NodeRef>,
}

impl<'a> RegionBuilder<'a> {
    pub fn register_arg(
        &mut self,
        ident: Option<impl Into<Ident>>,
        node: impl Into<Node>,
    ) -> NodeRef {
        let node_key = self.module.new_node(node);

        if let Some(ident) = ident {
            self.module.symbols.push_ref(ident, node_key);
        }

        node_key
    }

    ///Sets the out primitive
    pub fn set_out_node(&mut self, node: NodeRef) {
        assert!(
            self.module.is_ty(NodeTy::CombNode, node),
            "out node must be combinatorical"
        );

        self.out = Some(node);
    }

    pub fn build(mut self) -> Region {
        assert!(
            self.out.is_some(),
            "Out node must be set before building region!"
        );

        Region {
            in_at_node: self.at_ref,
            in_args: self.args,
            out: self.out.take().unwrap(),
        }
    }
}
