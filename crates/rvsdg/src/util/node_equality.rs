/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use ahash::AHashMap;

use crate::{
    edge::{InportLocation, LangEdge, OutportLocation},
    nodes::{LangNode, Node, NodeType},
    region::RegionLocation,
    NodeRef, Rvsdg, SmallColl,
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

#[derive(Clone)]
pub struct BucketEntry {
    pub node: NodeRef,
    ///The ports the inputs of this node are connected to
    pub srcs: SmallColl<Option<OutportLocation>>,
    ///The ports this node's outputs are connected to
    pub dsts: SmallColl<SmallColl<InportLocation>>,
}

struct EqMapEntry {
    ///Which bucket this node uses
    bucket_index: usize,
    ///Which entry in that bucket it uses.
    bucket_entry: usize,
}

///Recursively tracks equal nodes on a per-region basis.
///
/// This allows you to track type equal nodes over parenting region borders.
/// For instance to import already existing values into loops (instead of recreating them).
///
/// Right now this is used by the CNE pass to identify unifiable nodes in the _mark_ stage
/// of the algorithm.
pub struct RegionEqNodeTracker {
    #[allow(dead_code)]
    region: RegionLocation,
    ///Collects all nodes that are _equal_ in a bucket
    equality_buckets: Vec<Vec<BucketEntry>>,
    ///Maps any node in `region` to a bucket in `equality_bucket`.
    equality_map: AHashMap<NodeRef, EqMapEntry>,
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
    pub fn get_candidates(&self, node: NodeRef) -> &[BucketEntry] {
        &self.equality_buckets[self.equality_map.get(&node).unwrap().bucket_index]
    }

    pub fn get_input_srcs(&self, node: NodeRef) -> &SmallColl<Option<OutportLocation>> {
        let entry = self.equality_map.get(&node).unwrap();
        &self.equality_buckets[entry.bucket_index][entry.bucket_entry].srcs
    }

    pub fn get_output_dsts(&self, node: NodeRef) -> &SmallColl<SmallColl<InportLocation>> {
        let entry = self.equality_map.get(&node).unwrap();
        &self.equality_buckets[entry.bucket_index][entry.bucket_entry].dsts
    }

    pub fn build_for_region<N: LangNode + NodeTypeEq + 'static, E: LangEdge + 'static>(
        graph: &Rvsdg<N, E>,
        region: RegionLocation,
    ) -> Self {
        let mut tracker = RegionEqNodeTracker {
            region,
            equality_map: AHashMap::new(),
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

        let srcs = graph.build_src_map(id);
        let dsts = graph.build_dst_map(id);
        let bucket_entry = BucketEntry {
            node: id,
            srcs,
            dsts,
        };

        match &node.node_type {
            NodeType::Apply(_) => {
                let map_entry = EqMapEntry {
                    bucket_index: Self::APPLY_IDX,
                    bucket_entry: self.equality_buckets[Self::APPLY_IDX].len(),
                };
                assert!(self.equality_map.insert(id, map_entry).is_none());
                self.equality_buckets[Self::APPLY_IDX].push(bucket_entry);
            }
            NodeType::Lambda(_) => {
                let map_entry = EqMapEntry {
                    bucket_index: Self::LAMBDA_IDX,
                    bucket_entry: self.equality_buckets[Self::LAMBDA_IDX].len(),
                };
                assert!(self.equality_map.insert(id, map_entry).is_none());
                self.equality_buckets[Self::LAMBDA_IDX].push(bucket_entry);
            }
            NodeType::Phi(_) => {
                let map_entry = EqMapEntry {
                    bucket_index: Self::PHI_IDX,
                    bucket_entry: self.equality_buckets[Self::PHI_IDX].len(),
                };
                assert!(self.equality_map.insert(id, map_entry).is_none());
                self.equality_buckets[Self::PHI_IDX].push(bucket_entry);
            }
            NodeType::Omega(_) => {
                let map_entry = EqMapEntry {
                    bucket_index: Self::OMEGA_IDX,
                    bucket_entry: self.equality_buckets[Self::OMEGA_IDX].len(),
                };
                assert!(self.equality_map.insert(id, map_entry).is_none());
                self.equality_buckets[Self::OMEGA_IDX].push(bucket_entry);
            }
            NodeType::Theta(_) => {
                let map_entry = EqMapEntry {
                    bucket_index: Self::THETA_IDX,
                    bucket_entry: self.equality_buckets[Self::THETA_IDX].len(),
                };
                assert!(self.equality_map.insert(id, map_entry).is_none());
                self.equality_buckets[Self::THETA_IDX].push(bucket_entry);
            }
            NodeType::Gamma(_) => {
                let map_entry = EqMapEntry {
                    bucket_index: Self::GAMMA_IDX,
                    bucket_entry: self.equality_buckets[Self::GAMMA_IDX].len(),
                };
                assert!(self.equality_map.insert(id, map_entry).is_none());
                self.equality_buckets[Self::GAMMA_IDX].push(bucket_entry);
            }
            NodeType::Delta(_) => {
                let map_entry = EqMapEntry {
                    bucket_index: Self::DELTA_IDX,
                    bucket_entry: self.equality_buckets[Self::DELTA_IDX].len(),
                };
                assert!(self.equality_map.insert(id, map_entry).is_none());
                self.equality_buckets[Self::DELTA_IDX].push(bucket_entry);
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
                    .map(|(k, v)| (k.clone(), v.bucket_index.clone()))
                    .clone();
                if let Some((_candidate_id, bucket_index)) = found {
                    let map_entry = EqMapEntry {
                        bucket_index,
                        bucket_entry: self.equality_buckets[bucket_index].len(),
                    };
                    assert!(self.equality_map.insert(id, map_entry).is_none());
                    self.equality_buckets[bucket_index].push(bucket_entry);
                } else {
                    //found no equal, therefore building a new bucket
                    let bucket_index = self.equality_buckets.len();
                    let map_entry = EqMapEntry {
                        bucket_index,
                        //NOTE: init new bucket
                        bucket_entry: 0,
                    };
                    self.equality_buckets.push(vec![bucket_entry]);
                    assert!(self.equality_map.insert(id, map_entry).is_none());
                }
            }
        }
    }
}
