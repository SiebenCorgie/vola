//! RVSDG building utilies. If your source is based on structured-control flow, the standart RVSDG builder should give
//! you everything needed to setup a a RVSDG representation.
//!
//! For non-SCF based source the paper outlines a construction strategy in 5.1, however this is not (yet) implemented.

use crate::{nodes::LanguageNode, Rvsdg};

pub struct RvsdgBuilder<'a, N: LanguageNode + 'static, E: 'static> {
    ctx: &'a mut Rvsdg<N, E>,
}

impl<'a, N: LanguageNode + 'static, E: 'static> RvsdgBuilder<'a, N, E> {
    pub fn on_rvsd(rvsdg: &'a mut Rvsdg<N, E>) -> Self {
        RvsdgBuilder { ctx: rvsdg }
    }
}
