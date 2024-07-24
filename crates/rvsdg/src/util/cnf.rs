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
    edge::{InportLocation, LangEdge},
    err::GraphError,
    nodes::{LangNode, Node, NodeType},
    region::RegionLocation,
    Rvsdg, SmallColl,
};

use super::abstract_node_type::AbstractNodeType;

#[derive(Debug, Error, Clone)]
pub enum CnfError {
    #[error(transparent)]
    GraphError(#[from] GraphError),
}

pub trait ConstantFoldable<N: LangNode + 'static, E: LangEdge + 'static> {
    ///Tries to fold `self` into a new constant `Self::Node` based on the given inputs.
    /// `src_nodes` is guaranteed to be the amount of input-ports declared by `self`.
    /// The new node won't be hooked up to any inputs. So be sure that it doesn't need any.
    ///
    /// If `None` is returned, the node is consider _not constant foldable_.
    fn try_constant_fold(
        &self,
        #[allow(unused_variables)] src_nodes: &[Option<&Node<N>>],
    ) -> Option<N> {
        None
    }

    ///Must implement the constant `usize` value this node describes. This is used to fold
    /// Gamma nodes by statically chosing a branch, or unrolling and compacting theta nodes if the bound
    /// is constant.
    fn constant_value(&self) -> Option<usize> {
        None
    }
}

///Small context for the pass that allows us to track _foldability_ through multiple recursions
struct CnfCtx<N: LangNode + 'static> {
    ///Flaggs all encountered ports as foldable or none-foldable
    folded_nodes: Vec<NodeType<N>>,
}

impl<N: LangNode + 'static> CnfCtx<N> {
    pub fn cnf_region<E: LangEdge + 'static>(
        &mut self,
        graph: &mut Rvsdg<N, E>,
        region: RegionLocation,
    ) -> Result<(), CnfError>
    where
        N: ConstantFoldable<N, E>,
    {
        //NOTE: The idea is similar to how the common-node-elemination works. We traverse the region in topological order.
        //      For any node that _could_ be folded, we call the `try_constant_fold` implementation with all dependencies.

        let topord = graph.topological_order_region(region);

        for node in topord {
            match graph.node(node).into_abstract() {
                AbstractNodeType::Simple => {
                    //NOTE: We move the _try_constant_fold_ part in a scope, so we can drop the borrows of all
                    //      input nodes before acting on the result.
                    let new_node = {
                        //Collect all inputs. Then try to fold the node
                        let mut src_collector = SmallColl::default();
                        for input in graph.node(node).inport_types() {
                            if let Some(src) = graph.inport_src(InportLocation { node, input }) {
                                src_collector.push(Some(graph.node(src.node)));
                            } else {
                                src_collector.push(None);
                            }
                        }

                        graph
                            .node(node)
                            .node_type
                            .unwrap_simple_ref()
                            .try_constant_fold(src_collector.as_slice())
                    };

                    //Now try to build a new node from those inputs
                    if let Some(mut new_node) = new_node {
                        //if it worked, disconnect all inputs, and replace the node with the just created one.
                        assert!(
                            new_node.inputs().len() == 0,
                            "Expected a constant value to be emitted, got one with {} inputs",
                            new_node.inputs().len()
                        );

                        assert!(
                            new_node.outputs().len() == graph.node(node).outputs().len(),
                            "Expected replacement node to have same amount of outputs"
                        );

                        //Needed so we can mutate the graph below :(
                        let edges: SmallColl<_> = graph
                            .node(node)
                            .inputs()
                            .iter()
                            .filter_map(|inp| inp.edge.clone())
                            .collect();
                        for edge in edges {
                            graph.disconnect(edge)?;
                        }
                        //swap out actual node
                        std::mem::swap(
                            graph.node_mut(node).node_type.unwrap_simple_mut(),
                            &mut new_node,
                        );
                        //and add folded node to collector
                        self.folded_nodes.push(NodeType::Simple(new_node));
                    } else {
                    }
                }
                _ => {
                    //TODO: Implement gamma and theta folding
                    //
                    //Recurse into the node's region
                    for region_index in 0..graph.node(node).regions().len() {
                        self.cnf_region(graph, RegionLocation { node, region_index })?;
                    }
                }
            }
        }

        Ok(())
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
    pub fn constant_fold(&mut self) -> Result<Vec<NodeType<N>>, CnfError> {
        let mut cnfctx = CnfCtx {
            folded_nodes: Vec::new(),
            //foldables: FlagStore::new(),
        };
        let region = self.toplevel_region();
        cnfctx.cnf_region(self, region)?;

        Ok(cnfctx.folded_nodes)
    }

    pub fn cnf_region(&mut self, region: RegionLocation) -> Result<Vec<NodeType<N>>, CnfError> {
        let mut cnfctx = CnfCtx {
            folded_nodes: Vec::new(),
            //foldables: FlagStore::new(),
        };

        cnfctx.cnf_region(self, region)?;

        Ok(cnfctx.folded_nodes)
    }
}
