/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use small_map::SmallMap;

use crate::{
    edge::LangEdge,
    nodes::{LangNode, Node, NodeType},
    region::RegionLocation,
    NodeRef, Rvsdg,
};

///Enables the passes to generically check node-type equalness. This is its own trait (intead of
///`PartialEq` or `Eq`), because this should only compare the _operation_ of this node.
/// So for instance a node X of type T could be depend on a & b. And a node Y of type T could
/// depend on c & d.
/// Those nodes are obviously not equal (since the depend on different nodes, and therefore
/// produce different results). However, the _type_ of those nodes (T) is indeed equal.
pub trait NodeTypeEq {
    ///Should return true, if self and `other` are CneTypeEqual. So if the _operation_ this node
    /// represents are equal.
    fn type_equal(&self, other: &Self) -> bool;
}

///Recursively tracks equal nodes on a per-region basis.
///
/// This allows you to track type equal nodes over parenting region borders.
/// For instance to import already existing values into loops (instead of recreating them).
///
/// Right now this is used by the CNE pass to identify unifiable nodes in the _mark_ stage
/// of the algorithm.
pub struct RegionEqNodeTracker {
    pub region: RegionLocation,
    ///Collects all nodes that are _equal_ in a bucket
    pub equality_buckets: Vec<Vec<NodeRef>>,
    ///Maps any node in `region` to a bucket in `equality_bucket`.
    pub equality_map: SmallMap<64, NodeRef, usize>,
}

impl RegionEqNodeTracker {
    const APPLY_IDX: usize = 0;
    const LAMBDA_IDX: usize = 1;
    const PHI_IDX: usize = 2;
    const OMEGA_IDX: usize = 3;
    const THETA_IDX: usize = 4;
    const GAMMA_IDX: usize = 5;
    const DELTA_IDX: usize = 6;
    const SIMPLE_IDX_OFFSET: usize = 7;

    ///Returns all candidates for this node that are of equal type. Assumes that `node` was inserted before, otherwise panics.
    pub fn get_candidates(&self, node: NodeRef) -> &[NodeRef] {
        &self.equality_buckets[*self.equality_map.get(&node).unwrap()]
    }

    pub fn build_for_region<N: LangNode + NodeTypeEq + 'static, E: LangEdge + 'static>(
        graph: &Rvsdg<N, E>,
        region: RegionLocation,
    ) -> Self {
        let mut tracker = RegionEqNodeTracker {
            region,
            equality_map: SmallMap::new(),
            equality_buckets: vec![Vec::new(); Self::SIMPLE_IDX_OFFSET],
        };
        for node in &graph.region(&region).unwrap().nodes {
            tracker.insert_node(graph, graph.node(*node), *node);
        }

        tracker
    }

    fn insert_node<N: LangNode + NodeTypeEq + 'static, E: LangEdge + 'static>(
        &mut self,
        graph: &Rvsdg<N, E>,
        node: &Node<N>,
        id: NodeRef,
    ) {
        //NOTE: we always prepare a offset for all _standard_ nodes, like λ/τ etc.
        //      only the _simple_ nodes really need more space
        //      This function _expects_ the preparation (in `build_for_region`) so we don't / shouldn't
        //      expose it.

        //TODO: In theory we could use `N: Hash` to _sometime_ accellerate this, since the insert would then become a
        //      hashtabel lookup, instead of iterating _all_. However, a region is _mostly_ small, so you'd really have to profile that
        //      I guess.
        match &node.node_type {
            NodeType::Apply(_) => {
                assert!(self.equality_map.insert(id, Self::APPLY_IDX).is_none());
                self.equality_buckets[Self::APPLY_IDX].push(id);
            }
            NodeType::Lambda(_) => {
                assert!(self.equality_map.insert(id, Self::LAMBDA_IDX).is_none());
                self.equality_buckets[Self::LAMBDA_IDX].push(id);
            }
            NodeType::Phi(_) => {
                assert!(self.equality_map.insert(id, Self::PHI_IDX).is_none());
                self.equality_buckets[Self::PHI_IDX].push(id);
            }
            NodeType::Omega(_) => {
                assert!(self.equality_map.insert(id, Self::OMEGA_IDX).is_none());
                self.equality_buckets[Self::OMEGA_IDX].push(id);
            }
            NodeType::Theta(_) => {
                assert!(self.equality_map.insert(id, Self::THETA_IDX).is_none());
                self.equality_buckets[Self::THETA_IDX].push(id);
            }
            NodeType::Gamma(_) => {
                assert!(self.equality_map.insert(id, Self::GAMMA_IDX).is_none());
                self.equality_buckets[Self::GAMMA_IDX].push(id);
            }
            NodeType::Delta(_) => {
                assert!(self.equality_map.insert(id, Self::DELTA_IDX).is_none());
                self.equality_buckets[Self::DELTA_IDX].push(id);
            }
            NodeType::Simple(s) => {
                //try to find a candidate with the same type
                let found: Option<(NodeRef, usize)> = self
                    .equality_map
                    .iter()
                    .find(|(candidate_id, _bucket_index)| {
                        if let NodeType::Simple(candidate_s) = &graph.node(**candidate_id).node_type
                        {
                            s.type_equal(candidate_s)
                        } else {
                            false
                        }
                    })
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .clone();
                if let Some((_candidate_id, bucket_index)) = found {
                    assert!(self.equality_map.insert(id, bucket_index).is_none());
                    self.equality_buckets[bucket_index].push(id);
                } else {
                    //found no equal, therefore building a new bucket
                    let bucket_index = self.equality_buckets.len();
                    self.equality_buckets.push(vec![id]);
                    assert!(self.equality_map.insert(id, bucket_index).is_none());
                }
            }
        }
    }
}
