/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use core::panic;

use ahash::{AHashMap, AHashSet};
use smallvec::SmallVec;

use crate::{
    attrib::AttribLocation,
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    err::GraphError,
    nodes::{LangNode, NodeType, StructuralNode},
    region::RegionLocation,
    util::abstract_node_type::AbstractNodeType,
    NodeRef, Rvsdg, SmallColl,
};

impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    //NOTE: At first I wanted to implement things like `remove_result` and `remove_argument` here,
    //to make life easier. But this would make it easy to produce invalid graph. For instanec if you'd
    //remove a result of a λ-Node's body, you'd have to update all apply node outputs as well.

    ///Removes all unused context variables of the lambda or Phi `node`.
    ///
    /// This will remove all cv-nodes that are not used within the region of `node`, regardless if
    /// they are connected from the outside.
    pub fn remove_unused_context_variables(&mut self, node: NodeRef) {
        let cvcount = match &self.node(node).node_type {
            NodeType::Lambda(l) => l.context_variable_count(),
            NodeType::Phi(p) => p.context_variable_count(),
            //We do not do anything on those
            _ => return,
        };

        //now we build a new cv connection table, and reconnect everything based on that.
        //NOTE: This is not the _fastest_ way, but the most correct. I tried using a
        //      _iterate, collect and rewire dst/src-ports_, but that was always buggy,
        //      so we go the _ez_ road now.

        //Remaps an _old_ cv-index to a new cv-index
        let mut remapping_table: AHashMap<usize, usize> = AHashMap::default();
        let mut next_unused_cv_idx = 0;
        //for each cv, check if it is connected internally, if not,
        //call the remover 🧹, otherwise, add to the mapping
        for cvidx in 0..cvcount {
            let is_connected = if let Some(cv) = self
                .node(node)
                .outport(&OutputType::ContextVariableArgument(cvidx))
            {
                cv.edges.len() > 0
            } else {
                #[cfg(feature = "log")]
                log::warn!("Found invalid cv_idx {cvidx}, ignoring");
                continue;
            };

            //if not connected internally, remove any outside connection if there is any.
            if is_connected {
                remapping_table.insert(cvidx, next_unused_cv_idx);
                next_unused_cv_idx += 1;
            } else {
                if let Some(inport) = self
                    .node(node)
                    .inport(&InputType::ContextVariableInput(cvidx))
                {
                    if let Some(edg) = inport.edge {
                        self.disconnect(edg).unwrap();
                    }
                } else {
                    #[cfg(feature = "log")]
                    log::warn!("Encountered invalid cv-port");
                    continue;
                }
            }
        }

        if remapping_table.len() == cvcount {
            //in this case we can early return, since we ain't remapping anything.
            return;
        }

        //now use the remapping, to disconnect all ports, and
        //save where they need to be connected to _in the next loop_.
        //we can't do that at once, since there might still be connection for any given
        //cv port.
        let mut remap_targets: SmallVec<[(InportLocation, OutportLocation, E); 3]> =
            SmallVec::new();
        for src_cv_idx in remapping_table.keys() {
            if let Some(edgref) = self
                .node(node)
                .inport(&InputType::ContextVariableInput(*src_cv_idx))
                .unwrap()
                .edge
            {
                let original_src = self.edge(edgref).src().clone();
                let ty = self.disconnect(edgref).unwrap();

                let remapped_cv = remapping_table.get(src_cv_idx).unwrap();
                remap_targets.push((
                    InportLocation {
                        node,
                        input: InputType::ContextVariableInput(*remapped_cv),
                    },
                    original_src,
                    ty,
                ));
            }

            //now do the same for all dst. NOTE that we clone the edge-array, since we are changing it in thae
            // self.disconnect() call.
            for edgref in self
                .node(node)
                .outport(&OutputType::ContextVariableArgument(*src_cv_idx))
                .unwrap()
                .edges
                .clone()
            {
                let original_dst = self.edge(edgref).dst().clone();
                let ty = self.disconnect(edgref).unwrap();

                let remapped_cv = remapping_table.get(src_cv_idx).unwrap();
                remap_targets.push((
                    original_dst,
                    OutportLocation {
                        node,
                        output: OutputType::ContextVariableArgument(*remapped_cv),
                    },
                    ty,
                ));
            }
        }
        //finally reconnect all edges and remove all unused cvs
        for remap_target in remap_targets {
            self.connect(remap_target.1, remap_target.0, remap_target.2)
                .unwrap();
        }

        assert!(remapping_table.len() <= cvcount);

        match &mut self.node_mut(node).node_type {
            NodeType::Lambda(l) => {
                //remove all the cvs that are unused
                for _ in remapping_table.len()..cvcount {
                    l.inputs.remove(remapping_table.len());
                    l.body.arguments.remove(remapping_table.len());
                }
                //now decrement the cv-count
                l.cv_count = remapping_table.len();
            }
            NodeType::Phi(p) => {
                //remove the input-argument pair
                for _ in remapping_table.len()..cvcount {
                    p.inputs.remove(remapping_table.len());
                    p.body.arguments.remove(remapping_table.len());
                }
                //now decrement the cv-count
                p.cv_count = remapping_table.len();
            }
            _ => panic!("unexpected non λ- or ϕ-Node"),
        }
    }

    ///Shortcut to replace one node with another, which includes hooking up the replacee the same way `to_be_repled` was.
    ///
    /// Replaces `to_be_replaced` with `replacee` and returns `replacee`'s NodeRef.
    ///
    /// - Assumes that `replacee` has at least the input and output
    /// amount as `to_be_replaced` has.
    /// - Assumes `to_be_replaced` to be a `SimpleNode`.
    /// - Assumes `to_be_replaced` to have a parent region.
    ///
    /// Note that `to_be_replaced` is not deleted from the graph, but compleatly unconnected in its region.
    ///
    /// Returns the Id under which `replacee` is inserted.
    pub fn replace_node(
        &mut self,
        to_be_replaced: NodeRef,
        replacee: N,
    ) -> Result<NodeRef, GraphError> {
        if !self.node(to_be_replaced).node_type.is_simple() {
            return Err(GraphError::InvalidNode(to_be_replaced));
        }

        let parent_region = self.node(to_be_replaced).parent.unwrap();
        let replacee_ref = self
            .on_region(&parent_region, |r| r.insert_node(replacee))
            .unwrap();

        //collect all connected edges, then iterate them, disconnect the node,
        //recover its value, and reconnect in on `replacee`
        for input in self.node(to_be_replaced).input_edges() {
            if let Some(input_edge) = input {
                //disconnect, and reconnect
                let src = self.edge(input_edge).src().clone();
                let mut dst = self.edge(input_edge).dst().clone();
                dst.node = replacee_ref;
                let ty = self.disconnect(input_edge)?;
                self.connect(src, dst, ty)?;
            }
        }
        //similarly reconnect all outputs
        for output in self.node(to_be_replaced).output_edges() {
            for outedge in output {
                let mut src = self.edge(outedge).src().clone();
                let dst = self.edge(outedge).dst().clone();
                src.node = replacee_ref;
                let ty = self.disconnect(outedge)?;
                self.connect(src, dst, ty)?;
            }
        }

        //reconnected everything, so we can return
        Ok(replacee_ref)
    }

    ///Replaces all connections from `replaced` to any input with connections from `replacee`.
    ///
    ///Also takes care of carrying over the edge-types
    pub fn replace_outport_uses(
        &mut self,
        replaced: OutportLocation,
        replacee: OutportLocation,
    ) -> Result<(), GraphError> {
        for edge in self[replaced].edges.clone() {
            let dst = *self[edge].dst();
            let ty = self.disconnect(edge)?;
            self.connect(replacee, dst, ty)?;
        }

        Ok(())
    }

    ///Replaces all uses of `replaced` with `replacee`. Contrary to [replace_node](Rvsdg::replace_node) this does not change inputs to `replacee`.
    /// Instead all _output-connected-edges_ of `replaced` are routed to `replacee`.
    ///
    /// Assumes that:
    /// - both have same amount of outputs
    /// - both are in the same region
    /// - both are simple nodes
    pub fn replace_node_uses(
        &mut self,
        replaced: NodeRef,
        replacee: NodeRef,
    ) -> Result<(), GraphError> {
        if !self.node(replaced).node_type.is_simple() {
            return Err(GraphError::UnexpectedNodeType(
                AbstractNodeType::Simple,
                self[replaced].into_abstract(),
            ));
        }
        if !self.node(replacee).node_type.is_simple() {
            return Err(GraphError::UnexpectedNodeType(
                AbstractNodeType::Simple,
                self[replacee].into_abstract(),
            ));
        }

        let reg_a = self.node(replaced).parent.clone();
        let reg_b = self.node(replacee).parent.clone();
        if reg_a != reg_b {
            return Err(GraphError::NodesNotInSameRegion {
                src: reg_a.expect("Cannot replace in toplevel region and fail"),
                dst: reg_b.expect("Cannot replace in toplevel region and fail"),
            });
        }

        assert!(self.node(replaced).outputs().len() == self.node(replacee).outputs().len());

        for out_ty in self.node(replaced).outport_types() {
            let mut edge_dst_pack = SmallColl::new();
            for connection in self.node(replaced).outport(&out_ty).unwrap().edges.clone() {
                let dst = self.edge(connection).dst;
                let val = self.disconnect(connection)?;
                edge_dst_pack.push((val, dst));
            }

            for (val, dst) in edge_dst_pack {
                self.connect(
                    OutportLocation {
                        node: replacee,
                        output: out_ty,
                    },
                    dst,
                    val,
                )?;
            }
        }

        Ok(())
    }

    ///Utility that collects all node, that are `live`, so connected to any result,
    ///in that region.
    ///
    /// Note that there is also a [liveness](Rvsdg::liveness) analysis that builds a liveness lookuptable
    /// for all ports, which is more precise an recursively traverses sub regions.
    ///
    /// For the recursive version see [live_nodes](Rvsdg::live_nodes).
    pub fn live_nodes_in_region(&self, region: RegionLocation) -> Vec<NodeRef> {
        let mut live_variables = AHashSet::default();

        let region_content = self.region(&region).unwrap();
        for resultidx in 0..region_content.results.len() {
            if let Some(src) = region_content.result_src(self, resultidx) {
                //ignore if the result is connected directly to a argument
                if src.node == region.node {
                    continue;
                }

                //insert the seeding node. If this wasn't already in the map, walk the predecessors as well
                if live_variables.insert(src.node) {
                    for pred in self.walk_predecessors_in_region(src.node) {
                        //ignore the src region
                        if pred.node == region.node {
                            continue;
                        }
                        live_variables.insert(pred.node);
                    }
                }
            }
        }

        live_variables.into_iter().collect()
    }

    ///Finds all nodes that are connected to any result of `region`. Recursively traverses any
    ///sub-regions of nodes within that region.
    ///
    /// If you are only interested of live nodes _in this region_ see [live_nodes_in_region](Rvsdg::live_nodes_in_region).
    /// For a port-wise analysis see [liveness](Rvsdg::liveness).
    pub fn live_nodes(&self, region: RegionLocation) -> Vec<NodeRef> {
        //NOTE: we can't take the nice shortcut of live_nodes_in_region, since we might _over-add_ nodes that are in a subregion
        //      if we annotate all output ports _always_ as live. So we just use the standard liveness analysis, and filter for unique
        //      nodes instead.
        let live_ports = self.liveness_region(region);
        let mut live_nodes = AHashSet::new();
        for (attr, is_live) in live_ports.flags {
            if is_live {
                //NOTE: we only care about ports here
                if let AttribLocation::InPort(InportLocation { node, .. })
                | AttribLocation::OutPort(OutportLocation { node, .. }) = attr
                {
                    live_nodes.insert(node);
                }
            }
        }

        live_nodes.into_iter().collect()
    }
}

#[cfg(test)]
mod test {
    use rvsdg_derive_lang::LangNode;

    use crate::{self as rvsdg, EdgeRef, NodeRef};
    use crate::{
        common::VSEdge,
        region::{Input, Output},
        Rvsdg,
    };

    #[derive(LangNode)]
    struct TestNode {
        #[input]
        inp: Input,
        #[output]
        out: Output,
    }

    impl Default for TestNode {
        fn default() -> Self {
            TestNode {
                inp: Input::default(),
                out: Output::default(),
            }
        }
    }

    fn setup_test_rvsdg() -> (Rvsdg<TestNode, VSEdge>, NodeRef, EdgeRef) {
        let mut rvsdg: Rvsdg<TestNode, VSEdge> = Rvsdg::new();

        let (testnode, test_edge) = rvsdg
            .on_region(&rvsdg.toplevel_region(), |reg| {
                let test_node = reg.insert_node(TestNode::default());
                let test_node2 = reg.insert_node(TestNode::default());

                let edg = reg
                    .ctx_mut()
                    .connect(test_node.output(0), test_node2.input(0), VSEdge::State)
                    .unwrap();

                (test_node, edg)
            })
            .unwrap();

        (rvsdg, testnode, test_edge)
    }
    #[test]
    fn index_valid() {
        let (mut rvsdg, testnode, test_edge) = setup_test_rvsdg();

        let t: &_ = &rvsdg[testnode];
        let t: &mut _ = &mut rvsdg[testnode];

        let e: &_ = &rvsdg[test_edge];
        let e: &mut _ = &mut rvsdg[test_edge];

        let tlreg = rvsdg.toplevel_region();
        let r: &_ = &rvsdg[tlreg];
        let r: &mut _ = &mut rvsdg[tlreg];

        let ip: &_ = &rvsdg[testnode.input(0)];
        let ip: &mut _ = &mut rvsdg[testnode.input(0)];

        let op: &_ = &rvsdg[testnode.output(0)];
        let op: &mut _ = &mut rvsdg[testnode.output(0)];
    }

    #[test]
    #[should_panic]
    fn index_invalid_node_imm() {
        let (mut rvsdg, testnode, test_edge) = setup_test_rvsdg();
        rvsdg.remove_node(testnode).unwrap();
        let t: &_ = &rvsdg[testnode];
    }
    #[test]
    #[should_panic]
    fn index_invalid_node_mut() {
        let (mut rvsdg, testnode, test_edge) = setup_test_rvsdg();
        rvsdg.remove_node(testnode).unwrap();
        let t: &mut _ = &mut rvsdg[testnode];
    }

    #[test]
    #[should_panic]
    fn index_invalid_edge_imm() {
        let (mut rvsdg, testnode, test_edge) = setup_test_rvsdg();
        rvsdg.remove_node(testnode).unwrap();
        let t: &_ = &rvsdg[test_edge];
    }
    #[test]
    #[should_panic]
    fn index_invalid_edge_mut() {
        let (mut rvsdg, testnode, test_edge) = setup_test_rvsdg();
        rvsdg.remove_node(testnode).unwrap();
        let t: &mut _ = &mut rvsdg[test_edge];
    }

    #[test]
    #[should_panic]
    fn index_invalid_inport_imm() {
        let (mut rvsdg, testnode, test_edge) = setup_test_rvsdg();
        rvsdg.remove_node(testnode).unwrap();
        let t: &_ = &rvsdg[testnode.input(0)];
    }
    #[test]
    #[should_panic]
    fn index_invalid_inport_mut() {
        let (mut rvsdg, testnode, test_edge) = setup_test_rvsdg();
        rvsdg.remove_node(testnode).unwrap();
        let t: &mut _ = &mut rvsdg[testnode.input(0)];
    }

    #[test]
    #[should_panic]
    fn index_invalid_outport_imm() {
        let (mut rvsdg, testnode, test_edge) = setup_test_rvsdg();
        rvsdg.remove_node(testnode).unwrap();
        let t: &_ = &rvsdg[testnode.output(0)];
    }
    #[test]
    #[should_panic]
    fn index_invalid_outport_mut() {
        let (mut rvsdg, testnode, test_edge) = setup_test_rvsdg();
        rvsdg.remove_node(testnode).unwrap();
        let t: &mut _ = &mut rvsdg[testnode.output(0)];
    }
}
