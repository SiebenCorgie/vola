use core::panic;

use crate::{
    edge::{InportLocation, InputType, LangEdge, OutputType},
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
}
