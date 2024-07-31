/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! Utilities that help transform a graph `Rvsdg<A, B>` into a graph `RVSDG<C, D>`.

use ahash::AHashMap;
use thiserror::Error;

use crate::{
    edge::LangEdge,
    nodes::{LangNode, NodeType},
    region::RegionLocation,
    EdgeRef, NodeRef, Rvsdg,
};

use super::copy::StructuralClone;

///Transformer definition. Allows you to express how a node or edge in a graph `Rvsdg<SrcNode, SrcEdge>` is transformed to a
/// node or edge in a graph `Rvsdg<DstNode, DstEdge>`. Conceptually such a rewrite will always be a 1:1 mapping from one graph
/// to another. If you need something more involved, consider introducing a meta nodes that are transformed in a following pass.
///
///
/// If you don't need to store any intermediate transformer state its usually enough to implement that trait on an
/// empty struct like this:
///
/// ```ignore
///struct MyGraphTransformer;
///impl GraphTypeTransformer for MyGraphTransformer{
/// //...
///}
/// ```
///
///
/// By default the, transformer will generate a 1:1 rewritten graph, where only the 1:1 mapping of simple-nodes
/// and edges have to be redefined.
pub trait GraphTypeTransformer {
    type SrcNode: LangNode + 'static;
    type SrcEdge: LangEdge + 'static;
    type DstNode: LangNode + 'static;
    type DstEdge: LangEdge + 'static;

    ///Defines how a `src_edge` maps to a `Self::DST_EDGE`.
    fn transform_edge(&mut self, src_edge: &Self::SrcEdge) -> Self::DstEdge;
    fn transform_simple_node(&mut self, src_node: &Self::SrcNode) -> Self::DstNode;
    ///called on each newly added mapping from a `src_node` in the `src_graph` to the `dst_node` in the `dst_graph`.
    ///
    /// Is called on _all_ nodes, not just the `SimpleNode`s.
    #[allow(unused)]
    fn on_mapping(
        &mut self,
        src_graph: &Rvsdg<Self::SrcNode, Self::SrcEdge>,
        src_node: NodeRef,
        dst_graph: &mut Rvsdg<Self::DstNode, Self::DstEdge>,
        dst_node: NodeRef,
    ) {
    }
    //TODO: do we want to allow defining how other node types are transformed? That makes stuff way _involved_... but might
    //      be nice.
}

#[derive(Debug, Error, Clone)]
pub enum GraphTypeTransformerError {
    ///Happens, if the toplevel regions of the src has more arguments than the dst graph.
    #[error("The Toplevel region of the src graph has {src} arguments, but the dst graph has only {dst}")]
    TopLevelArgumentMissmatch { src: usize, dst: usize },
    ///Happens, if the toplevel regions of the src has more results than the dst graph.
    #[error(
        "The Toplevel region of the src graph has {src} results, but the dst graph has only {dst}"
    )]
    TopLevelResultMissmatch { src: usize, dst: usize },
}

///Collects the mapping information of one graph into another while doing the [Rvsdg::transform_into] pass.
///
/// # Warning
///
/// Take extra care, that you only use the keys with the source graph, and the values with the destination graph!.
pub struct GraphMapping {
    ///How a node from the src graph mapps to the dst graph
    pub node_mapping: AHashMap<NodeRef, NodeRef>,
    ///How a edge mapps from the src graph to the dst graph.
    pub edge_mapping: AHashMap<EdgeRef, EdgeRef>,
}

impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    ///Appends the rewritten `src_graph` to the `dst_graph` based on the given `transformer`.
    ///
    ///Returns the mapping from a node `A` in `self` to a node `B` in `dst_graph`.
    pub fn transform_into<DN: LangNode + 'static, DE: LangEdge + 'static>(
        &self,
        dst_graph: &mut Rvsdg<DN, DE>,
        transformer: &mut dyn GraphTypeTransformer<
            SrcNode = N,
            SrcEdge = E,
            DstNode = DN,
            DstEdge = DE,
        >,
    ) -> Result<GraphMapping, GraphTypeTransformerError> {
        //tracks the node-remapping which lets us connect the right edges
        let mut node_remapping = AHashMap::new();
        let mut edge_remapping = AHashMap::new();
        //check that we can hook-up all results/args in the toplevel.
        //all others will be taken care of by the structural-clone process.

        let src_tl_args = self
            .region(&self.toplevel_region())
            .unwrap()
            .arguments
            .len();
        let src_tl_res = self.region(&self.toplevel_region()).unwrap().results.len();
        let dst_tl_args = dst_graph
            .region(&dst_graph.toplevel_region())
            .unwrap()
            .arguments
            .len();
        let dst_tl_res = dst_graph
            .region(&dst_graph.toplevel_region())
            .unwrap()
            .results
            .len();

        if src_tl_args > dst_tl_args {
            return Err(GraphTypeTransformerError::TopLevelArgumentMissmatch {
                src: src_tl_args,
                dst: dst_tl_args,
            });
        }

        if src_tl_res > dst_tl_res {
            return Err(GraphTypeTransformerError::TopLevelResultMissmatch {
                src: src_tl_res,
                dst: dst_tl_res,
            });
        }

        //seed with both top-level nodes
        assert!(node_remapping
            .insert(
                self.toplevel_region().node,
                dst_graph.toplevel_region().node,
            )
            .is_none());

        for regloc in self.walk_region_locations() {
            //map to the dst-region
            let dst_region_node = node_remapping
                .get(&regloc.node)
                .expect("Could not find node in remapping");
            let dstreg = RegionLocation {
                node: *dst_region_node,
                region_index: regloc.region_index,
            };

            //per region, first rewrite all nodes, then hook up the edges based on the `node_remapping`.
            //this is similar to a deep copy actually, but instead of using the StructuralClone trait we use the transformer
            // for simple nodes

            for node in self.region(&regloc).unwrap().nodes.iter() {
                //Use the transformer for simple nodes, all others are shallow copied.
                //NOTE: we need to fully match the whole thing, cause the (rust)compiler needs to known
                //      know the specific type for each node :(
                let tbi = match &self.node(*node).node_type {
                    NodeType::Simple(s) => NodeType::Simple(transformer.transform_simple_node(s)),
                    NodeType::Gamma(g) => NodeType::Gamma(g.structural_copy()),
                    NodeType::Theta(t) => NodeType::Theta(t.structural_copy()),
                    NodeType::Lambda(t) => NodeType::Lambda(t.structural_copy()),
                    NodeType::Apply(t) => NodeType::Apply(t.structural_copy()),
                    NodeType::Delta(t) => NodeType::Delta(t.structural_copy()),
                    NodeType::Phi(t) => NodeType::Phi(t.structural_copy()),
                    NodeType::Omega(t) => NodeType::Omega(t.structural_copy()),
                };

                let inserted_as = dst_graph
                    .on_region(&dstreg, |r| r.add_node_type(tbi))
                    .unwrap();
                //now add to mapping
                node_remapping.insert(*node, inserted_as);
                //finally call post_mapping hook
                transformer.on_mapping(self, *node, dst_graph, inserted_as);
            }

            //now hook-up all edges via the transformator
            for edg in self.region(&regloc).unwrap().edges.iter() {
                let tbi = transformer.transform_edge(&self.edge(*edg).ty);
                let mut new_src = self.edge(*edg).src().clone();
                let mut new_dst = self.edge(*edg).dst().clone();

                new_src.node = *node_remapping.get(&new_src.node).unwrap();
                new_dst.node = *node_remapping.get(&new_dst.node).unwrap();
                //now insert accordingly

                let new_edge = dst_graph.connect(new_src, new_dst, tbi).unwrap();
                edge_remapping.insert(*edg, new_edge);
            }
        }

        Ok(GraphMapping {
            node_mapping: node_remapping,
            edge_mapping: edge_remapping,
        })
    }

    ///Helper that tranforms the whole `src_graph` into a new graph based on the transformator.
    ///
    ///Returns the new graph `B` and the mapping from a node `A` in `self` to a node `B` in `dst_graph`.
    pub fn transform_new<DN: LangNode + 'static, DE: LangEdge + 'static>(
        &self,
        tranformer: &mut dyn GraphTypeTransformer<
            SrcNode = N,
            SrcEdge = E,
            DstNode = DN,
            DstEdge = DE,
        >,
    ) -> Result<(Rvsdg<DN, DE>, GraphMapping), GraphTypeTransformerError> {
        let mut new_graph = Rvsdg::new();
        //take care of argument / result count
        let argcount = self
            .region(&self.toplevel_region())
            .unwrap()
            .arguments
            .len();
        let rescount = self.region(&self.toplevel_region()).unwrap().results.len();

        new_graph.on_omega_node(|omg| {
            for _ in 0..argcount {
                let _ = omg.import();
            }

            for _ in 0..rescount {
                let _ = omg.export();
            }
        });

        let remapping = self.transform_into(&mut new_graph, tranformer)?;
        Ok((new_graph, remapping))
    }
}
