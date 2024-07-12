/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! This module implements the generic dead-node-elimination (DNE).

#[derive(Clone, Debug, Error)]
pub enum DneError {}

use thiserror::Error;

use crate::{
    attrib::FlagStore,
    edge::{LangEdge, OutportLocation},
    err::GraphError,
    nodes::{LangNode, Node},
    region::RegionLocation,
    NodeRef, Rvsdg,
};
impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    ///Applies dead-node-elimination to the whole graph. Returns all nodes that
    /// where deleted.
    pub fn dead_node_elimination(&mut self) -> Result<Vec<Node<N>>, GraphError> {
        self.dne_region(self.toplevel_region())
    }

    ///Applies the dead-node-elimination on `region` and all its children (in topological order).
    pub fn dne_region(&mut self, region: RegionLocation) -> Result<Vec<Node<N>>, GraphError> {
        //We basically coppy the algorithm VI: DeadNodeElimination of the [source paper](http://arxiv.org/abs/1912.05036v2)
        //We express the `marks` via the attribute store and do traversal via recursion.
        //
        //The _mark_ phase is done by our per-port liveness analysis. The sweep only iterates all nodes of all regions
        // and deletes any node, where all output ports are marked dead.

        let liveness = self.liveness_region(region);
        let mut deleted_buffer = Vec::with_capacity(10);
        //we now do a top-down traversal, where we first delete all dead nodes of a region,
        //and then recurse all sub regions of nodes that are still theren. This effectively prevents us from traversing
        //dead structural nodes, since those are deleted at that point already.
        self.dne_dfs_sweep(&liveness, region, &mut deleted_buffer)?;

        Ok(deleted_buffer)
    }

    fn node_is_live(&self, liveness: &FlagStore<bool>, node: NodeRef) -> bool {
        //the node is live, if any output is live
        for outty in self.node(node).outport_types() {
            let port = OutportLocation {
                node,
                output: outty,
            };
            //found an alive port
            if let Some(true) = liveness.get(&port.into()) {
                return true;
            }
        }

        false
    }

    fn dne_dfs_sweep(
        &mut self,
        liveness: &FlagStore<bool>,
        region: RegionLocation,
        deleted_buffer: &mut Vec<Node<N>>,
    ) -> Result<(), GraphError> {
        let all_nodes = self.region(&region).unwrap().nodes.clone();
        let initial_deleted = deleted_buffer.len();
        for node in &all_nodes {
            if !self.node_is_live(liveness, *node) {
                deleted_buffer.push(self.remove_node(*node)?);
            }
        }

        //now recurse into all sub regions
        //NOTE: save one clone if possible
        let left_nodes = if initial_deleted == deleted_buffer.len() {
            all_nodes
        } else {
            self.region(&region).unwrap().nodes.clone()
        };
        for node in left_nodes {
            let regcount = self.node(node).regions().len();
            if regcount > 0 {
                for region_index in 0..regcount {
                    self.dne_dfs_sweep(
                        liveness,
                        RegionLocation { node, region_index },
                        deleted_buffer,
                    )?;
                }
            }
        }
        Ok(())
    }
}
