use vola_ast::comb::OpNode;

use crate::{graph::NodeTy, AlgeOp, Ident, ModuleBuilder, Node, NodeRef, Region};

use super::{AlgeNode, NodeRefs};

pub struct RegionBuilder<'a> {
    pub(crate) module: &'a mut ModuleBuilder,

    //Preregistered at_ref
    pub(crate) at_ref: NodeRef,
    pub(crate) args: NodeRefs,
    pub(crate) out: Option<NodeRef>,
}

impl<'a> RegionBuilder<'a> {
    pub fn register_arg(&mut self, ident: impl Into<Ident>) -> NodeRef {
        let ident = ident.into();
        let node = AlgeNode::new(AlgeOp::Arg(ident.clone()));
        let node_key = self.module.new_node(node);
        self.module.symbols.push_ref(ident, node_key);

        self.args.push(node_key);

        node_key
    }

    ///Sets the out primitive
    pub fn set_out_node(&mut self, node: NodeRef) {
        self.out = Some(node);
    }

    pub fn module(&mut self) -> &mut ModuleBuilder {
        self.module
    }

    pub fn get_at(&self) -> NodeRef {
        self.at_ref
    }

    ///Registers the node in that context
    pub fn register_node(
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

    pub fn get_ref(&self, ident: &Ident) -> Option<NodeRef> {
        self.module.symbols.resolve(ident)
    }

    pub fn build(mut self) -> NodeRef {
        assert!(
            self.out.is_some(),
            "Out node must be set before building region!"
        );

        let region = Region {
            in_at_node: self.at_ref,
            in_args: self.args,
            out: self.out.take().unwrap(),
        };

        //Close the builders scope
        self.module.symbols.close_scope();
        self.module.new_node(region)
    }
}
