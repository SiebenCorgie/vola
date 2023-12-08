//! Implements some helper function when working with the RVSDG.

use crate::{
    edge::{LangEdge, OutportLocation},
    err::GraphError,
    nodes::{LangNode, Node, NodeType},
    NodeRef, Rvsdg,
};

impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    /// Lets you import the output of `src` as an argument into `dst`'s region.
    ///
    /// Searches for `src` in all parent regions of `dst` (including `dst`). If found, routes the output of `src` into `dst`
    /// and returns the port in `dst`'s region that represents the value of `src`.
    ///
    /// # Errors
    /// - Returns `Err` if `src` is not part of any parent region.
    /// - Returns `Err` immediately, if `dst` has no region (which is only true for Apply and Simple nodes).
    ///
    /// # Alternatives
    ///
    /// If you want to import the result of some non-parent node. Lets say, you defined a output in a predecessor of `src`,
    /// consider routing that output _by hand_ into the region of `dst` (or a known parent), and then using this function to facilitate
    /// the import.
    pub fn import_output(
        &mut self,
        src: OutportLocation,
        dst: NodeRef,
    ) -> Result<OutportLocation, GraphError> {
        //make sure we are operating on a node with internal region.
        if let NodeType::Simple(_) | NodeType::Apply(_) = self.node(dst).node_type {
            return Err(GraphError::UnexpectedNodeType);
        }

        let mut parent = Some(dst);
        while let Some(parent) = parent.take() {}

        Err(GraphError::UnexpectedNodeType)
    }
}
