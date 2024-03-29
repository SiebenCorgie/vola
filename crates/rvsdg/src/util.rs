//! Implements some helper function when working with the RVSDG.

use std::collections::VecDeque;

use crate::{
    edge::{InputType, LangEdge, OutportLocation, OutputType},
    err::GraphError,
    nodes::{LangNode, NodeType},
    region::RegionLocation,
    Rvsdg,
};

impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    ///Builds the import path from `src` down to `path`'s last region.
    ///
    /// Panics if the `path` does not describe a valid chain of children, where each successor is a child to its predecessor in the list.
    ///
    /// Panics if `path` contains a Omega, Simple or Apply node.
    ///
    /// Note that the connections will use context-variables whenever needed, so this should only be used
    /// with callable-defs for `src` (function deceleration or phi-arguments). Otherwise please use [Self::build_import_path_arg].
    ///
    ///
    /// # Example
    /// Lets say `src` is a node on your ω-node, and you have a λ-Node, with an internal γ-node with an internal node that needs to call
    /// `src`. In that case you can make `src` available to your node by calling this function with `path = [ω-node-region, λ-node-region, γ-node-region-of-target-branch]`.
    ///
    /// This will import `src` as cv into λ-node's region, then from there into the γ-node's region and return the entry-variable-argument under which
    /// you can access `src` in that branch.
    fn build_import_path_cv(
        &mut self,
        src_region: RegionLocation,
        src: OutportLocation,
        path: &[RegionLocation],
    ) -> OutportLocation {
        let mut current_region = src_region;
        let mut next_out_port = src;
        for reg in path {
            assert!(
                self.region(&current_region)
                    .unwrap()
                    .nodes
                    .contains(&reg.node),
                "Expected next reg in path to be a child to its successor, successor: {}",
                self.node(reg.node).node_type
            );

            //child property holds, advance with new connection into child
            let (new_in_port, new_out_port) = match &mut self.node_mut(reg.node).node_type {
                NodeType::Delta(d) => {
                    let cv_idx = d.add_context_variable();
                    (
                        reg.node
                            .as_inport_location(InputType::ContextVariableInput(cv_idx)),
                        reg.node
                            .as_outport_location(OutputType::ContextVariableArgument(cv_idx)),
                    )
                }
                NodeType::Gamma(g) => {
                    let ev_idx = g.add_entry_var();
                    (
                        reg.node
                            .as_inport_location(InputType::EntryVariableInput(ev_idx)),
                        reg.node
                            .as_outport_location(OutputType::EntryVariableArgument {
                                branch: reg.region_index,
                                entry_variable: ev_idx,
                            }),
                    )
                }
                NodeType::Lambda(l) => {
                    let cv_idx = l.add_context_variable();
                    (
                        reg.node
                            .as_inport_location(InputType::ContextVariableInput(cv_idx)),
                        reg.node
                            .as_outport_location(OutputType::ContextVariableArgument(cv_idx)),
                    )
                }
                NodeType::Phi(p) => {
                    let cv_idx = p.add_context_variable();
                    (
                        reg.node
                            .as_inport_location(InputType::ContextVariableInput(cv_idx)),
                        reg.node
                            .as_outport_location(OutputType::ContextVariableArgument(cv_idx)),
                    )
                }
                NodeType::Theta(t) => {
                    //TODO: Is that correct, or do we want _normal_ arguments to a loop as well?
                    let lv_idx = t.add_loop_variable();
                    (
                        reg.node.as_inport_location(InputType::Input(lv_idx)),
                        reg.node.as_outport_location(OutputType::Argument(lv_idx)),
                    )
                }
                _ => panic!("Unexpected node type in path!"),
            };

            //connect ports from next_out to new_port
            self.connect(next_out_port, new_in_port, E::value_edge())
                .unwrap();

            //advance region and port
            next_out_port = new_out_port;
            current_region = reg.clone();
        }

        next_out_port
    }

    ///Similarly to [Self::build_import_path_cv], but imports values via normal arguments to inner regions.
    pub fn build_import_path_arg(
        &mut self,
        _src: OutportLocation,
        _path: &[RegionLocation],
    ) -> OutportLocation {
        //TODO implement similar to the CV version. Find out how we could merge the code though
        todo!("Implement")
    }

    /// Lets you import the output of `src` as context into `dst` region.
    /// This lets you efficiently import state into a node's region.
    ///
    /// Searches for `src` in all parent regions of `dst` (including `dst`). If found, routes the output of `src` into `dst`
    /// and returns the port in `dst`'s region that represents the value of `src`.
    ///
    /// # Errors
    /// Returns `Err` if `src` is not part of any parent region.
    ///
    /// # Alternatives
    ///
    /// If you want to import the result of some non-parent node. Lets say, you defined a output in a predecessor of `src`,
    /// consider routing that output _by hand_ into the region of `dst` (or a known parent), and then using this function to facilitate
    /// the import.
    pub fn import_context(
        &mut self,
        src: OutportLocation,
        dst: RegionLocation,
    ) -> Result<OutportLocation, GraphError> {
        //The stack will keep track of region boundaries we crossed. We'll use that
        // to get _free_ routing to dst, once we've found src.
        let mut parent_stack: VecDeque<RegionLocation> = VecDeque::new();

        //Find out what our dst region should be. Similarly to Rvsdg::connect we need to figure
        // out if `src` is a argument-like port, or a output. If its an output, we search for the node's parent,
        // otherwise we search for the argumet-port's region
        let searched_for_region = if let Some(reg_idx) = src.output.argument_region_index() {
            RegionLocation {
                node: src.node,
                region_index: reg_idx,
            }
        } else {
            self.node(src.node).parent.as_ref().unwrap().clone()
        };

        let mut parent = Some(dst.clone());
        let mut found_region = false;
        while let Some(regloc) = parent.take() {
            if regloc == searched_for_region {
                //reached the searched for region, therefore break.
                found_region = true;
                break;
            } else {
                //push regloc on stack and go to parent
                parent_stack.push_front(regloc.clone());
                parent = self.node(regloc.node).parent.clone();
            }
        }

        //Check if we've found the parent region.
        //
        // there is one edgecase, which is when src is within dst's region. So src.parent == dst.parent. In that case we
        // can immediately return the src.
        if self.node(src.node).parent == Some(dst.clone()) {
            return Ok(src);
        }

        if !found_region {
            return Err(GraphError::NodeNotInParentRegion(src.node, dst));
        };

        //otherwise, iterate the parent stack from dst's region, down to src's region, always adding a new
        // input-arg pair to the next, inner region.
        Ok(self.build_import_path_cv(searched_for_region, src, parent_stack.make_contiguous()))
    }
}
