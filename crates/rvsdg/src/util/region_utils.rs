use core::panic;

use crate::{
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    err::GraphError,
    nodes::{LangNode, NodeType, StructuralNode},
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

        //for each cv, check if it is connected internally, if not,
        //call the remover 🧹
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
            if !is_connected {
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

        //now, in reverse order, remove CV-Ports that have nothing connected anymore,
        // and decrement the port_idx of any cv_edge that is connected to any of the
        // cvs _following_ that... yeah its convoluted
        //NOTE: Since all arguments are _after_ the CVs, we can just decrement the CVcounter
        //      And remove the ports from the node and its inner region.
        for cvidx in (0..cvcount).rev() {
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

            if is_connected {
                continue;
            }

            match &mut self.node_mut(node).node_type {
                NodeType::Lambda(lmd) => {
                    lmd.cv_count -= 1;
                    lmd.inputs.remove(cvidx);
                    lmd.body.arguments.remove(cvidx);
                }
                NodeType::Phi(p) => {
                    p.cv_count -= 1;
                    p.inputs.remove(cvidx);
                    p.body.arguments.remove(cvidx);
                }
                _ => panic!(),
            }

            //Update cvount, since we just removed one
            cvcount -= 1;
            //now decrement all following
            for dec_cv_idx in (1 + cvidx)..cvcount {
                if let Some(connected_inport) = self
                    .node(node)
                    .inport(&InputType::ContextVariableInput(dec_cv_idx))
                {
                    if let Some(edg) = connected_inport.edge {
                        match &mut self.edge_mut(edg).dst.input {
                            InputType::ContextVariableInput(deccv) => {
                                assert!(*deccv == dec_cv_idx + 1);
                                *deccv -= 1;
                            }
                            _ => panic!("Expected Context Variable"),
                        }
                    }
                } else {
                    panic!("expected dec_cv_idx to be existent");
                }

                if let Some(outp) = self
                    .node(node)
                    .outport(&OutputType::ContextVariableArgument(dec_cv_idx))
                {
                    for edg in outp.edges.clone() {
                        match &mut self.edge_mut(edg).src.output {
                            OutputType::ContextVariableArgument(deccv) => {
                                assert!(*deccv == dec_cv_idx + 1);
                                *deccv -= 1;
                            }
                            _ => panic!("expected CV"),
                        }
                    }
                } else {
                    panic!("Expected dec_cv_idx arg to be existent");
                }
            }
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
}
