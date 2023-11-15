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

use crate::{nodes::LanguageNode, NodeRef, RegionRef, Rvsdg};

///[λ-region](crate::nodes::LambdaNode) builder.
pub struct LambdaBuilder<'a, N: LanguageNode + 'static, E: 'static> {
    ctx: &'a mut Rvsdg<N, E>,
    region: RegionRef,
    omega: NodeRef,
}

impl<'a, N: LanguageNode + 'static, E: 'static> LambdaBuilder<'a, N, E> {}
