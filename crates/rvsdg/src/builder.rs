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
    nodes::{LanguageNode, OmegaNode},
    NodeRef, RegionRef, Rvsdg,
};

///Top level ω-region builder.
pub struct RvsdgBuilder<'a, N: LanguageNode + 'static, E: 'static> {
    ctx: &'a mut Rvsdg<N, E>,
    region: RegionRef,
    omega: NodeRef,
}

impl<'a, N: LanguageNode + 'static, E: 'static> RvsdgBuilder<'a, N, E> {
    pub fn on_rvsd(rvsdg: &'a mut Rvsdg<N, E>) -> Self {
        let region = rvsdg.new_region();
        let omega = rvsdg.new_node(crate::nodes::Node::Omega(OmegaNode {
            body: region,
            outputs: ArrayVec::default(),
            inputs: ArrayVec::default(),
        }));
        RvsdgBuilder {
            ctx: rvsdg,
            region,
            omega,
        }
    }

    pub fn build(self) -> NodeRef {
        debug_assert!(
            self.region == self.ctx.node(self.omega).regions()[0],
            "OmegaNode regions don't match!"
        );
        self.omega
    }
}
