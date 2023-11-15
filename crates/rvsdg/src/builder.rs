//! RVSDG building utilies. If your source is based on structured-control flow, the standart RVSDG builder should give
//! you everything needed to setup a a RVSDG representation.
//!
//! For non-SCF based source the paper outlines a construction strategy in 5.1, however this is not (yet) implemented.
//!
//!
//! In general the builder pattern usually represents some region that is currently worked on.
//! Those regions _usually_ mostly consist of [_Simple_ nodes](crate::nodes::Node). Whenever one of the more complex constructs is needed,
//! like a loop, if-then-else, function calls etc. the builder provides special methodes to build those.
//!
//! Nesting is represented by the closures used to build such special methodes.
//!

use tinyvec::ArrayVec;

use crate::{
    nodes::{LambdaNode, LanguageNode, Node},
    region::{Port, Region},
    NodeRef, RegionRef, Rvsdg,
};

///Probably the most used builder. Represents a simple [Region](crate::region::Region) within one of the higher level nodes.
pub struct RegionBuilder<'a, N: LanguageNode + 'static, E: 'static> {
    ctx: &'a mut Rvsdg<N, E>,
    region: Region,
}

impl<'a, N: LanguageNode + 'static, E: 'static> RegionBuilder<'a, N, E> {
    pub fn new(ctx: &'a mut Rvsdg<N, E>) -> Self {
        RegionBuilder {
            ctx,
            region: Region::new(),
        }
    }

    pub fn build(self) -> RegionRef {
        self.ctx.regions.insert(self.region)
    }
}

///[λ-region](crate::nodes::LambdaNode) builder.
pub struct LambdaBuilder<'a, N: LanguageNode + 'static, E: 'static> {
    ctx: &'a mut Rvsdg<N, E>,
    ///The node that is being build
    node: LambdaNode,
}

impl<'a, N: LanguageNode + 'static, E: 'static> LambdaBuilder<'a, N, E> {
    pub fn new(ctx: &'a mut Rvsdg<N, E>) -> Self {
        let body = ctx.new_region();

        LambdaBuilder {
            ctx,
            node: LambdaNode {
                inputs: ArrayVec::default(),
                output: Port::default(),
                body,
                context_variables: ArrayVec::default(),
            },
        }
    }

    ///Builds the Lambda node for the borrowed context.
    pub fn build(self) -> NodeRef {
        //TODO: do some legalization already, or wait for a legalization pass?

        self.ctx.new_node(Node::Lambda(self.node))
    }
}
