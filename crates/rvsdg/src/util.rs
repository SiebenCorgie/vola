//! Implements some helper function when working with the RVSDG.
//!
//! Usually those helpers are implemented on the [RVSDG](crate::Rvsdg) directly. For instance
//! the [copy] module only defines a [StructuralClone](copy::StructuralClone) trait, and then implements
//! [shallow_copy_node](crate::Rvsdg::shallow_copy_node), [deep_copy_node](crate::Rvsdg::deep_copy_node) etc.
use std::collections::VecDeque;

use crate::{
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    err::GraphError,
    nodes::{LangNode, NodeType},
    region::RegionLocation,
    EdgeRef, NodeRef, Rvsdg, SmallColl,
};

pub mod cfg;
pub mod copy;
pub mod dead_node_elimination;
pub mod graph_type_transform;
pub mod inline;
pub mod liveness;
pub mod region_utils;

///Path of multiple edges from `start` to `end`.
pub struct Path {
    pub start: OutportLocation,
    pub end: InportLocation,
    pub edges: SmallColl<EdgeRef>,
}

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
    ) -> (OutportLocation, Option<Path>) {
        let mut current_region = src_region;
        let mut next_out_port = src;
        let mut created_edges = SmallColl::new();
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
            let edg = self
                .connect(next_out_port, new_in_port, E::value_edge())
                .unwrap();
            created_edges.push(edg);
            //advance region and port
            next_out_port = new_out_port;
            current_region = reg.clone();
        }

        //reverse the edge-order, since we are going from _inside_ to outside
        let path = if created_edges.len() > 0 {
            created_edges.reverse();
            let start = self.edge(*created_edges.first().unwrap()).src().clone();
            let end = self.edge(*created_edges.last().unwrap()).dst().clone();
            let path = Path {
                start,
                end,
                edges: created_edges,
            };
            Some(path)
        } else {
            None
        };

        (next_out_port, path)
    }

    ///Similarly to [Self::build_import_path_cv], but imports values via normal arguments to inner regions.
    fn build_import_path_arg(
        &mut self,
        src_region: RegionLocation,
        src: OutportLocation,
        path: &[RegionLocation],
    ) -> (OutportLocation, Option<Path>) {
        //Again, pretty similar to the cv version. BUT:
        // We check for each _next_out_port_ if its already connected to
        // current_region.node.
        // if so, instead of setting up a new connection, we use the already existing one.
        let mut used_edges = SmallColl::new();

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

            //checkout, if the next_out_port is already connected to reg.node,
            //if so use the already existing connection, and just map that port _into_
            //the region.
            let mut existing_connection = None;
            for input_ty in self.node(reg.node).inport_types() {
                let port = self.node(reg.node).inport(&input_ty).unwrap();
                if let Some(src) = port.edge {
                    if self.edge(src).src() == &next_out_port {
                        //use the existing outport and inport
                        let out_port = OutportLocation {
                            node: reg.node,
                            output: input_ty.map_to_in_region(reg.region_index).unwrap(),
                        };
                        let in_port = InportLocation {
                            node: reg.node,
                            input: input_ty,
                        };

                        existing_connection = Some((in_port, out_port, src));
                        break;
                    }
                }
            }

            //child property holds, advance with new connection into child
            let (_new_in_port, new_out_port) = if let Some((
                existing_in,
                existing_out,
                reuse_edge,
            )) = existing_connection
            {
                used_edges.push(reuse_edge);
                (existing_in, existing_out)
            } else {
                let (new_in_port, new_out_port) = match &mut self.node_mut(reg.node).node_type {
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
                    NodeType::Theta(t) => {
                        //TODO: Is that correct, or do we want _normal_ arguments to a loop as well?
                        let lv_idx = t.add_loop_variable();
                        (
                            reg.node.as_inport_location(InputType::Input(lv_idx)),
                            reg.node.as_outport_location(OutputType::Argument(lv_idx)),
                        )
                    }
                    any => panic!("Unexpected node type {any} in argument-path! Note that argument paths cannot import over λ & ϕ boundarie. So only θ & γ nodes can be bridged"),
                };

                //connect ports from next_out to new_port
                let edg = self
                    .connect(next_out_port, new_in_port, E::value_edge())
                    .unwrap();
                used_edges.push(edg);
                (new_in_port, new_out_port)
            };
            //advance region and port
            next_out_port = new_out_port;
            current_region = reg.clone();
        }

        //reverse the edge-order, since we are going from _inside_ to outside
        let path = if used_edges.len() > 0 {
            used_edges.reverse();
            let start = self.edge(*used_edges.first().unwrap()).src().clone();
            let end = self.edge(*used_edges.last().unwrap()).dst().clone();
            let path = Path {
                start,
                end,
                edges: used_edges,
            };
            Some(path)
        } else {
            None
        };

        (next_out_port, path)
    }

    ///Lets you import `src` as an argument into `dst`. Contrary to [import_context] it'll use arguments, loop variables etc.
    ///to create the path.
    ///
    /// Is able to resolve import collissions. For instance if a GammaNode branch imports a
    /// value that is already registered (by another branch) on a entry-variable.
    ///
    /// Fails if `src` is not within a dominating region of `dst`.
    ///
    ///
    /// Returns the (possibly) new `OutportLocation` where `src` is exposed, as well as all created edges throughout the process.
    pub fn import_argument(
        &mut self,
        src: OutportLocation,
        dst: RegionLocation,
    ) -> Result<(OutportLocation, Option<Path>), GraphError> {
        //similar to the cv version, first map out of dst_region
        //an check for _src_ in that region till we either found the node,
        //or we find the omega-node, in which case we failed to find the node.

        // there is one edgecase, which is when src is within dst's region. So src.parent == dst.parent. In that case we
        // can immediately return the src.
        if self.node(src.node).parent == Some(dst.clone()) {
            return Ok((src, None));
        }

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

        let parent_stack = self.build_region_path(src, dst, searched_for_region)?;

        //otherwise, iterate the parent stack from dst's region, down to src's region, always adding a new
        // input-arg pair to the next, inner region.
        Ok(self.build_import_path_arg(searched_for_region, src, &parent_stack))
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
    ) -> Result<(OutportLocation, Option<Path>), GraphError> {
        // there is one edgecase, which is when src is within dst's region. So src.parent == dst.parent. In that case we
        // can immediately return the src.
        if self.node(src.node).parent == Some(dst.clone()) {
            return Ok((src, None));
        }

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

        let parent_stack = self.build_region_path(src, dst, searched_for_region)?;

        //otherwise, iterate the parent stack from dst's region, down to src's region, always adding a new
        // input-arg pair to the next, inner region.
        Ok(self.build_import_path_cv(searched_for_region, src, &parent_stack))
    }

    fn build_region_path(
        &self,
        src: OutportLocation,
        dst: RegionLocation,
        searched_for_region: RegionLocation,
    ) -> Result<Vec<RegionLocation>, GraphError> {
        //The stack will keep track of region boundaries we crossed. We'll use that
        // to get _free_ routing to dst, once we've found src.
        let mut parent_stack: VecDeque<RegionLocation> = VecDeque::new();

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
        if !found_region {
            return Err(GraphError::NodeNotInParentRegion(src.node, dst));
        } else {
            Ok(parent_stack.into_iter().collect())
        }
    }

    ///Tries to the closest lambda or phi node that _surounds_ `node`.
    pub fn find_parent_lambda_or_phi(&self, node: NodeRef) -> Option<NodeRef> {
        let mut parent = self.node(node).parent.clone();

        while let Some(p) = parent.take() {
            if self.node(p.node).node_type.is_lambda() || self.node(p.node).node_type.is_phi() {
                return Some(p.node);
            } else {
                //move to next parent
                parent = self.node(p.node).parent.clone();
            }
        }

        None
    }
}
