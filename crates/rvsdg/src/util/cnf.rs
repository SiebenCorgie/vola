/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Implements _Constant-Node-Folding_. This allows propagating constant calculations through the graph. Node that both related
//! functions are implemented directly on the graph. See
//!
//! - [constant_fold](crate::Rvsdg::constant_fold)
//! - [cnf_region](crate::Rvsdg::cnf_region)
//!
//! For the function to become available a node type `N` of
//! `Rvsdg<N, E>` needs to implement [ConstantFoldable] for the chosen `N, E` of the graph.

use thiserror::Error;

use crate::{
    edge::LangEdge,
    nodes::{LangNode, Node},
    region::RegionLocation,
    Rvsdg,
};

#[derive(Debug, Error, Clone, Copy)]
pub enum CnfError {}

pub trait ConstantFoldable<N: LangNode + 'static, E: LangEdge + 'static> {
    ///Tries to fold `self` into a new `Self::Node` based on the given inputs.
    /// `src_nodes` is guaranteed to be the amount of input-ports declared by `self`.
    ///
    /// If `None` is returned, the node is consider _not constant foldable_.
    fn try_constant_fold(&self, src_nodes: &[Option<(&N, &E)>]) -> Option<N> {
        None
    }
}

impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E>
where
    N: ConstantFoldable<N, E>,
{
    ///Applies _constant-node-folding_ to the whole graph. If successful, returns all
    /// folded nodes. Note that those are only the folded nodes, not all nodes rendered dead by this pass.
    ///
    /// For instance, given a constant-foldable operation `X`, that depends on two constants `C1`,`C2`: If `X` is constant folded to a value `X'`, `C1`,`C2` will possibly be
    /// dead, but not folded.
    pub fn constant_fold(&mut self) -> Result<Vec<Node<N>>, CnfError> {
        let mut nodes = Vec::new();
        self.cnf_region(self.toplevel_region(), &mut nodes)?;
        Ok(nodes)
    }

    ///Applies cnf to this `region` and all sub regions. See [Self::constant_fold] and [ConstantFoldable] for more information.
    pub fn cnf_region(
        &mut self,
        region: RegionLocation,
        folded_nodes: &mut Vec<Node<N>>,
    ) -> Result<(), CnfError> {
        //NOTE: The idea is similar to how the common-node-elemination works. We traverse the region in topological order.
        //      For any node that _could_ be folded, we call the `try_constant_fold` implementation with all dependencies.
    }
}
