use vola_ast::comb::OpNode;

use crate::{
    graph::NodeTy, AlgeOp, CombNode, Ident, ModuleBuilder, Node, NodeRef, Region, SymbolTable,
};

use super::{AlgeNode, NodeRefs};

pub struct RegionBuilder<'a> {
    pub(crate) module: &'a mut ModuleBuilder,

    //Preregistered at_ref
    pub(crate) at_ref: NodeRef,
    pub(crate) args: NodeRefs,
    pub(crate) prim_args: NodeRefs,
    pub(crate) out: Option<NodeRef>,
    ///Safes the current symbol table state
    pub symbols: SymbolTable,
}

impl<'a> RegionBuilder<'a> {
    pub fn register_arg(&mut self, ident: impl Into<Ident>) -> NodeRef {
        let ident = ident.into();
        let node = AlgeNode::new(AlgeOp::Arg(ident.clone()));
        let node_key = self.module.new_node(node);
        self.symbols.push_ref(ident, node_key);

        self.args.push(node_key);

        node_key
    }

    pub fn register_arg_prim(&mut self, ident: impl Into<Ident>) -> NodeRef {
        let ident = ident.into();
        let node = CombNode::new(crate::CombOp::PrimArg(ident.clone()), self.get_at());
        let node_key = self.module.new_node(node);
        self.symbols.push_ref(ident, node_key);

        self.prim_args.push(node_key);

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
            self.symbols.push_ref(ident, node_key);
        }

        node_key
    }

    pub fn get_ref(&self, ident: &Ident) -> Option<NodeRef> {
        self.symbols.resolve(ident)
    }

    ///Creates a unique copy of nrfe which inherits all children and arguments
    /// from its source.
    pub fn copy(&mut self, nref: NodeRef) -> NodeRef {
        let new_node: Node = self.module.nodes.get(nref).cloned().unwrap();
        self.module().new_node(new_node)
    }

    pub fn build(mut self, config: RegionConfig) -> NodeRef {
        assert!(
            self.out.is_some(),
            "Out node must be set before building region!"
        );

        let region = Region {
            in_at_node: self.at_ref,
            in_args: self.args,
            in_prims: self.prim_args,
            out: self.out.take().unwrap(),
        };

        //Close the builders scope
        self.module.new_node(region)
    }
}

pub struct RegionConfig {
    ///Scheduls a pass that tries to find a fitting callsite for any primitive arg.
    /// only valid on Operation regions.
    pub resolve_prim_callsite: bool,
}

impl RegionConfig {
    pub fn none() -> Self {
        RegionConfig {
            resolve_prim_callsite: false,
        }
    }
}

impl Default for RegionConfig {
    fn default() -> Self {
        RegionConfig {
            resolve_prim_callsite: false,
        }
    }
}
