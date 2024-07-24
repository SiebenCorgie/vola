use thiserror::Error;

use crate::{edge::LangEdge, nodes::LangNode, Rvsdg};

#[derive(Debug, Error, Clone, Copy)]
pub enum CnfError {}

pub trait ConstantFoldable<N: LangNode + 'static, E: LangEdge + 'static> {
    ///Tries to fold `self` into a new `Self::Node` based on the given inputs.
    /// `src_nodes` is guaranteed to be the amount of input-ports declared by `self`.
    ///
    /// If `None` is returned, the node is consider _not constant foldable_.
    fn try_fold(&self, src_nodes: &[Option<(&N, &E)>]) -> Option<N> {
        None
    }
}

impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E>
where
    N: ConstantFoldable<N, E>,
{
    ///Applies _constant-node-folding_ to the whole graph.
    pub fn constant_fold(&mut self) -> Result<(), CnfError> {}
}
