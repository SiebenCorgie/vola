use core::panic;

use ahash::{AHashMap, AHashSet};
use smallvec::SmallVec;

use crate::{
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    err::GraphError,
    nodes::{LangNode, NodeType, StructuralNode},
    region::{Input, Output, RegionLocation},
    NodeRef, Rvsdg,
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
        let mut cvcount = match &self.node(node).node_type {
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
        //call the remover 🧹, otherwise, add to the mappin
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

    ///Utility that connects all node, that are `live`, so connected to any result
    ///in some way.
    pub fn live_variables(&self, region: RegionLocation) -> Vec<NodeRef> {
        let mut live_variables = AHashSet::default();

        let region = self.region(&region).unwrap();
        for resultidx in 0..region.results.len() {
            if let Some(src) = region.result_src(self, resultidx) {
                for pred in self.walk_predecessors(src.node) {
                    live_variables.insert(pred.node);
                }
            }
        }

        live_variables.into_iter().collect()
    }
}
