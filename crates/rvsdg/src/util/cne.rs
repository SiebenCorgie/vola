/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use std::fmt::Debug;

use ahash::AHashMap;
use smallvec::smallvec;
use thiserror::Error;

use crate::{
    attrib::{AttribLocation, AttribStore, FlagStore},
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    err::GraphError,
    nodes::{LangNode, Node, NodeType, StructuralNode},
    region::RegionLocation,
    EdgeRef, NodeRef, Rvsdg, SmallColl, SmallMap,
};

///Enables the CnePass to generically check node-type equalness. This is its own trait (intead of
///`PartialEq` or `Eq`), because this should only compare the _operation_ of this node.
/// So for instance a node X of type T could be depend on a & b. And a node Y of type T could
/// depend on c & d.
/// Those nodes are obviously not equal (since the depend on different nodes, and therefore
/// produce different results). However, the _type_ of those nodes (T) is indeed equal.
pub trait CneTypeEq {
    ///Should return true, if self and `other` are CneTypeEqual. So if the _operation_ this node
    /// represents are equal.
    fn type_equal(&self, other: &Self) -> bool;
}

//Small helper that collects nodes of the same type into buckets.
struct EqNodeBucket<'a, N: LangNode + CneTypeEq + 'static>(Vec<(&'a N, SmallColl<NodeRef>)>);

impl<'a, N: LangNode + CneTypeEq + 'static> EqNodeBucket<'a, N> {
    fn new() -> Self {
        Self(Vec::new())
    }
    fn insert(&mut self, node: &'a N, id: NodeRef) {
        for (ty, others) in &mut self.0 {
            if ty.type_equal(&node) {
                others.push(id);
                return;
            }
        }

        //if we came till herer, there are no others of that type, so create a new bucket
        self.0.push((node, smallvec![id]));
    }

    fn oder_by_use(&mut self) {
        self.0
            .sort_unstable_by(|a, b| a.1.len().cmp(&b.1.len()).reverse())
    }

    ///Returns all candidates this node is equal to, or none, if there are none.
    fn get_candidates(&self, node: &N) -> Option<&[NodeRef]> {
        println!("Ping");
        for (ty, bucket) in self.0.iter() {
            if ty.type_equal(node) {
                return Some(&bucket);
            }
        }

        None
    }
}

#[derive(Error, Debug, Clone)]
pub enum CneError {
    #[error(transparent)]
    GraphErr(#[from] GraphError),
}

impl<N: LangNode + CneTypeEq + Debug + 'static, E: LangEdge + Eq + 'static> Rvsdg<N, E> {
    ///Performs common-node-elemination on the whole graph. If successful, returns all eleminated nodes and edges.
    ///
    /// This implements a 2-phase _mark & divert_ pass described in section 6.1 in the [source paper](https://www.sjalander.com/research/pdf/sjalander-tecs2020.pdf).
    pub fn common_node_elemination(&mut self) -> Result<(), CneError> {
        println!("Enter CNE!");
        self.cne_region(self.toplevel_region())
    }

    ///Shortcut to execute [CNE](Self::common_node_elemination) only on this region (and its sub-regions).
    pub fn cne_region(&mut self, region: RegionLocation) -> Result<(), CneError> {
        let mut marks = FlagStore::new();
        //Keeps track of all seen nodes (in the key), and saves what sink-port they where mapped to.
        //NOTE: See the
        let mut seen_map = AHashMap::default();
        self.cne_mark_region(region, &mut marks, &mut seen_map)?;
        //fold the marks into a lookup table
        let eq_lookup = Self::mark_fold(marks, &mut seen_map);
        //now divert all marked ports to the source port
        self.cne_divert_ports(eq_lookup)
    }

    ///The mark pass. Updates the `marks` map with concurrent outputs.
    ///
    /// Intuitively the flags stores says
    ///```ignore
    /// if let Some(c) = marks.get(&a){
    ///  // a concurrent to c
    ///}
    ///```
    fn cne_mark_region(
        &self,
        region: RegionLocation,
        marks: &mut FlagStore<OutportLocation>,
        seen_map: &mut AHashMap<OutportLocation, OutportLocation>,
    ) -> Result<(), CneError> {
        println!("START: {region:?}");
        let mut simple_node_equality_bucket = EqNodeBucket::new();
        let mut apply_nodes = Vec::default();
        for node in &self.region(&region).unwrap().nodes {
            match &self.node(*node).node_type {
                NodeType::Simple(s) => simple_node_equality_bucket.insert(s, *node),
                NodeType::Apply(_) => apply_nodes.push(*node),
                _ => {}
            }
        }
        //NOTE: this'll make sure that we check the type of node that appears _more often_ first.
        simple_node_equality_bucket.oder_by_use();

        //process all nodes in region, possibly recursing.
        //
        // we chose the topological order, in order to be sure, that any port we check for concurrency by
        // equivalent-mark-chaseing (see [chase_fold]) is already _fully seen_. So that we can be sure that any
        // port-sink, is in fact a sink, and won't be marked afterwards.
        let topord = self.topological_order_region(region);
        println!("{:?} has {} nodes", region, topord.len());
        assert!(topord.len() == self.region(&region).unwrap().nodes.len());

        for node in topord {
            println!("Node: {}", node);
            let noderef = self.node(node);

            //NOTE sanity check, that this node wasn't touched in the seen-map already
            //     _somehow_
            for output in noderef.outport_types() {
                assert!(!seen_map.contains_key(&OutportLocation { node, output }));
            }

            //This catches all _into-region_ mappings for the mark process. That is true for
            // the gamma-theta node's entry / loop variables, as well as the phi/lambda/delta context variables.
            match noderef.node_type {
                NodeType::Gamma(_)
                | NodeType::Delta(_)
                | NodeType::Phi(_)
                | NodeType::Lambda(_) => {
                    //map all _into region_ mappable ports equivalent
                    let mut seen_srcs: SmallMap<(OutportLocation, OutputType), OutportLocation> =
                        SmallMap::default();

                    let regcount = noderef.regions().len();
                    for inty in noderef.inport_types() {
                        let input_port = InportLocation { node, input: inty };
                        for regidx in 0..regcount {
                            if let Some(in_region) = inty.map_to_in_region(regidx) {
                                let in_region_port = OutportLocation {
                                    node,
                                    output: in_region,
                                };
                                //If we have already seen this src port, mark the mapped outports as equivalent
                                if let Some(src) = self.inport_src(input_port) {
                                    if let Some(equivalent_port) = seen_srcs.get(&(src, in_region))
                                    {
                                        //Is the same port type, with the same src, so mark as
                                        // common
                                        marks.set(in_region_port.into(), *equivalent_port);
                                    } else {
                                        //note that we have seen this src already
                                        //NOTE: we add the _in_region_ Output type, so we can distinguish a
                                        //      ContextVariable that is connected to node N from a entry_variable that
                                        //      is connected to node N.
                                        //
                                        //      PS: this shouldn't happen in a correct graph (there are either CVs, or)
                                        //          arguments, but you know, defensive programming and stuff.
                                        seen_srcs.insert((src, in_region), in_region_port);
                                    }
                                }
                            }
                        }
                    }
                }
                //TODO: implement the theta-node test
                NodeType::Theta(_) => {}
                _ => assert!(noderef.regions().len() == 0),
            }

            println!("Post initial mark on {} ", noderef.node_type);
            // if region != self.toplevel_region() {
            //     return Ok(());
            // }
            match &noderef.node_type {
                NodeType::Simple(s) => {
                    //only try if it makes sense ^^
                    println!(
                        "{} has {} candidates",
                        noderef.node_type,
                        simple_node_equality_bucket
                            .get_candidates(s)
                            .map(|n| n.len())
                            .unwrap_or(0),
                    );
                    if let Some(candidates) = simple_node_equality_bucket.get_candidates(s) {
                        println!("Build source map for {}", noderef.node_type);
                        let own_sig = self.build_src_map(node, noderef, marks, seen_map);

                        println!(
                            "Sig[{}]: {:?} with {} candidates",
                            node,
                            own_sig,
                            candidates.len()
                        );
                        for candidate in candidates.iter() {
                            //Ignore self
                            if *candidate == node {
                                continue;
                            }

                            let candidate_ref = self.node(*candidate);
                            let sig =
                                self.build_src_map(*candidate, candidate_ref, marks, seen_map);

                            if sig == own_sig {
                                //mark equivalent to _apply_node_ by marking all apply outports
                                //
                                // This will effectively render the node _unconnected_ after diverting.
                                assert!(candidate_ref.outputs().len() == noderef.outputs().len());

                                println!("found concurrent simple node!");

                                for (selfout, candout) in noderef
                                    .outport_types()
                                    .iter()
                                    .zip(candidate_ref.outport_types().iter())
                                {
                                    assert!(selfout == candout);
                                    marks.set(
                                        OutportLocation {
                                            node,
                                            output: *selfout,
                                        }
                                        .into(),
                                        OutportLocation {
                                            node: *candidate,
                                            output: *candout,
                                        },
                                    );
                                }
                            }
                        }
                    }
                }
                NodeType::Apply(_a) => {
                    //Apply node functions similar to the simple node.
                    // the caller is a CV, so for instance if the same λ is imported multiple time, the topological
                    // order of the pass + the fact that λ-nodes annotate equivalent cv-ports
                    // makes sure, that equivalent apply-nodes are fused the same way as any other operation would.
                    let own_sig = self.build_src_map(node, noderef, marks, seen_map);
                    //now try to find a node of the same type with the same output-sink signature.
                    //TODO: cache the signatures as well, since its expensive to recalculate them each time
                    //      (I think)
                    for applynode in apply_nodes.iter() {
                        //ignore self
                        if *applynode == node {
                            continue;
                        }
                        let candidate_ref = self.node(*applynode);
                        let sig = self.build_src_map(*applynode, candidate_ref, marks, seen_map);
                        if sig == own_sig {
                            //mark equivalent to _apply_node_ by marking all apply outports
                            //
                            // This will effectively render the node _unconnected_ after diverting.
                            assert!(candidate_ref.outputs().len() == noderef.outputs().len());

                            for (selfout, candout) in noderef
                                .outport_types()
                                .iter()
                                .zip(candidate_ref.outport_types().iter())
                            {
                                assert!(selfout == candout);
                                marks.set(
                                    OutportLocation {
                                        node,
                                        output: *selfout,
                                    }
                                    .into(),
                                    OutportLocation {
                                        node: *applynode,
                                        output: *candout,
                                    },
                                );
                            }
                        }
                    }
                }
                //All these nodes need to recurse their inner region(s)
                NodeType::Gamma(_)
                | NodeType::Theta(_)
                | NodeType::Delta(_)
                | NodeType::Lambda(_)
                | NodeType::Phi(_) => {
                    for regidx in 0..noderef.regions().len() {
                        println!("Recurse {}", node);
                        self.cne_mark_region(
                            RegionLocation {
                                node,
                                region_index: regidx,
                            },
                            marks,
                            seen_map,
                        )?;
                    }
                }
                //Shouldn't happen, but here we go
                NodeType::Omega(_o) => {
                    println!("Recurse Omega!");
                    self.cne_mark_region(
                        RegionLocation {
                            node,
                            region_index: 0,
                        },
                        marks,
                        seen_map,
                    )?;
                }
            }

            //Finally, for gamma nodes, we need to check if any of the outputs are equivalent.
            //we do this basically the same way we did it for the inputs.
            match noderef.node_type {
                NodeType::Gamma(_) => {
                    //TODO: implement.
                    //NOTE: I'm pretty sure that this is wrong, since by definition
                    //      two exit-variable-arguments of branch a & b can't be connected to the same node.
                    todo!("")
                }
                _ => {}
            }
        }

        Ok(())
    }

    ///Builds the input-src map, that is already unified to the currently known port sinks. So any port in the output
    /// collection is in fact a sink
    fn build_src_map(
        &self,
        node: NodeRef,
        noderef: &Node<N>,
        marks: &FlagStore<OutportLocation>,
        seen_map: &mut AHashMap<OutportLocation, OutportLocation>,
    ) -> SmallColl<Option<OutportLocation>> {
        let mut srcs = SmallColl::default();
        for input in noderef.inport_types() {
            if let Some(src) = self.inport_src(InportLocation { node, input }) {
                let actual_src = Self::chase_equivalent(src, &marks, seen_map);
                srcs.push(Some(actual_src))
            } else {
                srcs.push(None)
            }
        }

        srcs
    }

    ///Folds the flag store into a lookup table where a common port is mapped to all equivalent ports.
    /// This lets us trivially divert all ports in the _value_ part to the _key_ port.
    fn mark_fold(
        marks: FlagStore<OutportLocation>,
        seen_map: &mut AHashMap<OutportLocation, OutportLocation>,
    ) -> AHashMap<OutportLocation, SmallColl<OutportLocation>> {
        let mut cmap = AHashMap::default();

        //we build the map by _chasing_ concurrency flags.
        //so for any node n, we check if its flagged to n', then check if n' is concurrent to n'' etc. till we find a
        // nx, that is not concurrent to anything.
        // This is our _concurrency-sink_ (or whatever), that we use as a key. We then add all nodes on that path to the
        // sink's collection (and flag all n as _seen_). This lets us skip all nodes on the path whenever we rediscover this path
        // from any other origin.

        for (port, _equivalent) in marks.flags.iter() {
            let port = if let AttribLocation::OutPort(o) = port {
                *o
            } else {
                panic!("Expected mark-key to be outport!");
            };
            //was already chased
            if seen_map.contains_key(&port) {
                continue;
            } else {
                //setup the chase chain. This will also add `port` to the _seen_ map in consequence
                let _ = Self::chase_fold(port, &marks, &mut cmap, seen_map);
            }
        }

        assert!(seen_map.len() == marks.flags.len());

        cmap
    }

    fn chase_equivalent(
        port: OutportLocation,
        marks: &FlagStore<OutportLocation>,
        seen_map: &mut AHashMap<OutportLocation, OutportLocation>,
    ) -> OutportLocation {
        if let Some(known_target) = seen_map.get(&port) {
            return *known_target;
        }

        if let Some(eq) = marks.get(&port.into()) {
            println!("Chase {port:?} -> {eq:?}");
            let target = Self::chase_equivalent(*eq, marks, seen_map);
            seen_map.insert(port, target);
            target
        } else {
            //has no equivalent, so we are a sink
            port
        }
    }

    //See [Self::mark::fold] for details on what we do here.
    fn chase_fold(
        port: OutportLocation,
        marks: &FlagStore<OutportLocation>,
        cmap: &mut AHashMap<OutportLocation, SmallColl<OutportLocation>>,
        seen_map: &mut AHashMap<OutportLocation, OutportLocation>,
    ) -> OutportLocation {
        //We can early out the chasing, if we know the target of this chain already, so if this
        // path was already chased before.
        if let Some(known_target) = seen_map.get(&port) {
            return *known_target;
        }

        if let Some(equivalent) = marks.get(&port.into()) {
            //chase the equivalent, then add our selfs to the chain.
            let target_port = Self::chase_fold(*equivalent, marks, cmap, seen_map);
            //add our-selft to the cmap
            cmap.get_mut(&target_port)
                .expect("Expected target-port to exist after chaseing")
                .push(port);
            //for later shortcuts, add self to the shortcut map
            seen_map.insert(port, target_port);

            target_port
        } else {
            //this is the last port in this chain, since it has no equivalent.
            //setup a collection if there is none
            if !cmap.contains_key(&port) {
                let old = cmap.insert(port, SmallColl::default());
                assert!(old.is_none());
            }
            return port;
        }
    }

    fn cne_divert_ports(
        &mut self,
        cmap: AHashMap<OutportLocation, SmallColl<OutportLocation>>,
    ) -> Result<(), CneError> {
        //Diverting is pretty simple now. We already have a lookup map, so we just call [divert] for each port combination.
        for (sink_port, ports) in cmap {
            for p in ports {
                self.divert(sink_port, p)?;
            }
        }

        Ok(())
    }

    fn divert(
        &mut self,
        to_port: OutportLocation,
        from_port: OutportLocation,
    ) -> Result<(), CneError> {
        //Diverting works by disconnecting all edges of `from_port`,
        // and reconnecting them to `to_port`.
        for edge in self
            .node(from_port.node)
            .outport(&from_port.output)
            .unwrap()
            .edges
            .clone()
        {
            let dst = self.edge(edge).dst;
            let val = self.disconnect(edge)?;
            self.connect(to_port, dst, val)?;
        }

        Ok(())
    }
}
