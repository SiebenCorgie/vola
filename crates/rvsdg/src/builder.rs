//! RVSDG building utilities. If your source is based on structured-control flow, the standart RVSDG builder should give
//! you everything needed to setup a RVSDG representation.
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

mod inter_proc;
mod intra_proc;
use crate::{edge::LangEdge, nodes::LangNode, region::Region, RegionRef, Rvsdg};
pub use inter_proc::LambdaBuilder;
pub use intra_proc::{GammaBuilder, ThetaBuilder};

///Probably the most used builder. Represents a simple [Region](crate::region::Region) within one of the higher level nodes.
pub struct RegionBuilder<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    ctx: &'a mut Rvsdg<N, E>,
    region: Region,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> RegionBuilder<'a, N, E> {
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
