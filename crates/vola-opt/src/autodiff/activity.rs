/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use core::panic;

use ahash::AHashMap;
use rvsdg::{
    attrib::FlagStore,
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    region::RegionLocation,
    smallvec::smallvec,
    util::abstract_node_type::AbstractNodeType,
    NodeRef, SmallColl,
};
use vola_common::Span;

use crate::{
    common::{DataType, Shape, Ty},
    imm::{ImmScalar, ImmVector},
    typelevel::{ConstantIndex, UniformConstruct},
    OptError, OptNode, Optimizer,
};

use super::AutoDiff;

type WrtProdMap = AHashMap<OutportLocation, SmallColl<usize>>;
pub(crate) struct Activity {
    ///All nodes or ports from which we _know_ the activity state.
    pub active: FlagStore<bool>,
    ///All ports that are equivalent to the wrt-input of the
    ///original AutoDiff node.
    ///
    /// Example: If the wrt argument is `a.x.y`, then this are all
    /// nodes that produce `a.x.y` in this region.
    ///
    /// For each case, we also track the actual chain of indices
    /// So if OutportLocation is a vector, then the vec will hold one index (which element of the vector is the active one).
    ///    if OutportLocation is a matrix, then the vec will hold the row-index (0) and the column-element (1) for the active element.
    ///    same goes for tensors
    pub wrt_producer: WrtProdMap,
}

impl Activity {
    ///Builds the init value for a wrt-producer.
    ///
    /// Lets something is differentiating for x, and this wrt-producer is a vector (a, x, c, d). Then,
    /// the init value would be (0, 1, 0, 0)
    ///
    /// Panics if not actually wrt producer, or is the port has no type set.
    pub fn build_diff_init_value_for_wrt(
        &self,
        opt: &mut Optimizer,
        region: RegionLocation,
        port: OutportLocation,
    ) -> OutportLocation {
        let wrt_to_scalar_chain = self
            .wrt_producer
            .get(&port.into())
            .expect("Expected scalar chain");
        let ty = opt
            .find_type(&port.into())
            .expect("expected type information");

        match ty {
            Ty::SCALAR_REAL => {
                assert!(wrt_to_scalar_chain.len() == 0);
                opt.graph
                    .on_region(&region, |g| {
                        g.insert_node(OptNode::new(ImmScalar::new(1.0), Span::empty()))
                            .output(0)
                    })
                    .unwrap()
            }
            Ty::Shaped {
                shape: Shape::Vec { width },
                ty: DataType::Real,
            } => {
                assert!(
                    wrt_to_scalar_chain.len() == 1,
                    "expected 1 == {}",
                    wrt_to_scalar_chain.len()
                );
                assert!(wrt_to_scalar_chain[0] < width);

                //build a vector that has the 1 set at the given index
                opt.graph
                    .on_region(&region, |g| {
                        let mut vector: SmallColl<f64> = smallvec![0.0f64; width];
                        vector[wrt_to_scalar_chain[0]] = 1.0;
                        g.insert_node(OptNode::new(ImmVector::new(&vector), Span::empty()))
                            .output(0)
                    })
                    .unwrap()
            }
            _ => todo!(),
        }
    }

    ///Returns true, if this `port` produces the specific wrt-value, or an aggregate type that contains
    /// the wrt value
    pub fn is_wrt_producer(&self, port: &OutportLocation) -> bool {
        self.wrt_producer.contains_key(port)
    }

    //Tries to recover the activity state of this port, and if it can't, traces it
    pub fn is_active_port(&mut self, opt: &Optimizer, port: OutportLocation) -> bool {
        if let Some(state) = self.active.get(&port.into()) {
            *state
        } else {
            //If the port is an argument to a region, assume that the state already exists.
            //otherwise either the `trace_activity_seed()` is buggy, or one of the
            // sub-region handlers in `trace_node_activity()` for non simple nodes.
            if port.output.is_argument() {
                panic!("Argument-like port must already be set!");
            }

            //NOTE: Trace the port's node, and overwrite the querried node afterwards
            let _port_active = self.is_node_active(opt, port.node);
            *self
                .active
                .get(&port.into())
                .expect("Expected port activity to be set after tracing node!")
        }
    }

    pub fn is_node_active(&mut self, opt: &Optimizer, node: NodeRef) -> bool {
        if let Some(state) = self.active.get(&node.into()) {
            *state
        } else {
            self.trace_node_activity(opt, node)
        }
    }

    //traces all predecessors of this node. Each trace ends at
    // 1. Finding cached value for node.
    // 2. at wrt-producer (and recursively taggs predecessors all as _active_)
    // 3. at region argument
    pub(crate) fn trace_node_activity(&mut self, opt: &Optimizer, node: NodeRef) -> bool {
        //TODO: Handle nodes with sub regions, Gamma&Theta nodes mostly...

        //handle the activity by node type.
        //
        //For simple nodes, we can just mark all outputs
        //for apply nodes, we can tag all active argument ports of the apply node's active inputs
        //in the λ-region.
        //
        //for Gamma/Theta nodes, we do this: https://arxiv.org/abs/1810.07951
        let nodetype = opt.graph[node].into_abstract();
        match nodetype {
            AbstractNodeType::Simple => {
                //by definition, this node is active, if any of its predecessors is active
                let mut any_active = false;
                for pred in opt.graph[node].pred(&opt.graph) {
                    if self.is_active_port(opt, pred) {
                        any_active = true;
                    }
                }

                //Overwrite this node, and all ports
                self.active.set(node.into(), any_active);
                for output in opt.graph[node].outport_types() {
                    self.active
                        .set(OutportLocation { node, output }.into(), any_active);
                }

                any_active
            }
            AbstractNodeType::Gamma | AbstractNodeType::Theta => {
                //recurse all entry / input-variables, and map their state into the region's branches.
                //after that, recurse.
                for input in opt.graph[node].inport_types() {
                    let src = if let Some(src) =
                        opt.graph.inport_src(InportLocation { node, input }.into())
                    {
                        src
                    } else {
                        continue;
                    };

                    let is_active = self.is_active_port(opt, src);

                    let wrt_index_chain = self.wrt_producer.get(&src).cloned();

                    for regidx in 0..opt.graph[node].regions().len() {
                        //Try to map that active or inactive port into the region
                        if let Some(in_region_outport) = input.map_to_in_region(regidx) {
                            let in_region_port = OutportLocation {
                                node,
                                output: in_region_outport,
                            };
                            //if that port hat a wrt_index chain, push that forward into the region
                            if let Some(wrt_indices) = wrt_index_chain.clone() {
                                let (seeds, new_prod_map) =
                                    opt.explore_producer_trace(in_region_port, wrt_indices);
                                //and appply to this activity map
                                for (k, v) in seeds.flags {
                                    self.active.flags.insert(k, v);
                                }
                                for (k, v) in new_prod_map {
                                    self.wrt_producer.insert(k, v);
                                }
                            } else {
                                //set active
                                self.active.set(in_region_port.into(), is_active);
                            }
                        }
                    }
                }

                //since all in-gamma ports are mapped now, trace all result ports of each gamma region
                let mut any_output_active = false;
                for subreg in 0..opt.graph[node].regions().len() {
                    for result in opt.graph[node].result_types(subreg) {
                        let result_port = InportLocation {
                            node,
                            input: result,
                        };
                        let src = opt.graph.inport_src(result_port).unwrap();
                        let is_src_active = self.is_active_port(opt, src);
                        //now set the result and map that _out_of_region_
                        //NOTE: we prefer the _is_active_ state. So if one region outputs an inactive result, and the other outputs
                        //      a active, we set the port as active
                        self.active.set(result_port.into(), is_src_active);
                        let output = if let Some(output) = result.map_out_of_region() {
                            output
                        } else {
                            continue;
                        };
                        let output_port = OutportLocation { node, output };
                        if let Some(activity) = self.active.get_mut(&output_port.into()) {
                            //only overwrite if currently not active, and we are active
                            if !*activity && is_src_active {
                                any_output_active = true;
                                *activity = true;
                            }
                        } else {
                            //no state set, always overwrite
                            if is_src_active {
                                any_output_active = true;
                            }
                            self.active.set(output_port.into(), is_src_active);
                        }
                    }
                }

                //Set the node activ, if any output is active and return.
                self.active.set(node.into(), any_output_active);
                any_output_active
            }
            AbstractNodeType::Apply => {
                //For apply nodes, find the called λ, and map the activity state of the inputs to the args in that λ.
                //We then recurse, and finally map the activity state of the results to the outputs.
                //NOTE:
                //      The canonicalization pass makes sure that there is one λ per call. So we can really just tag the activity.
                //      Otherwise that would have to be _call-site_ sensitive.
                let apply_node_call_port = node.input(0);
                let src = opt.graph.inport_src(apply_node_call_port).unwrap();
                let prod = opt.graph.find_callabel_def(src).unwrap();

                for input in opt.graph[node].inport_types() {
                    //Ignore the λ activity
                    let argument_index = match input {
                        //Ignore λ call-def
                        InputType::Input(0) => continue,
                        //normal args
                        InputType::Input(n) => n - 1,
                        _ => panic!("Malformed λ"),
                    };

                    let input_src = opt
                        .graph
                        .inport_src(node.as_inport_location(input))
                        .unwrap();
                    let wrt_index_chain = self.wrt_producer.get(&input_src).cloned();
                    let is_input_active = self.is_active_port(opt, input_src);

                    let arg = prod
                        .node
                        .as_outport_location(OutputType::Argument(argument_index));
                    //if that port hat a wrt_index chain, push that forward into the region
                    if let Some(wrt_indices) = wrt_index_chain.clone() {
                        let (seeds, new_prod_map) =
                            opt.explore_producer_trace(arg.into(), wrt_indices);
                        //and appply to this activity map
                        for (k, v) in seeds.flags {
                            self.active.flags.insert(k, v);
                        }
                        for (k, v) in new_prod_map {
                            self.wrt_producer.insert(k, v);
                        }
                    } else {
                        //set active
                        self.active.set(arg.into(), is_input_active);
                    }
                }
                let mut any_active = false;
                //now trace all results of the λ node, and if any is active, set ourself's active as well
                for result in opt.graph[prod.node].result_types(0) {
                    let result_index = if let InputType::Result(i) = result {
                        i
                    } else {
                        panic!("Malformed λ");
                    };
                    let src = opt
                        .graph
                        .inport_src(prod.node.as_inport_location(result))
                        .unwrap();
                    let is_src_active = self.is_active_port(opt, src);
                    let output_port = node.output(result_index);
                    if let Some(activity) = self.active.get_mut(&output_port.into()) {
                        //only overwrite if currently not active, and we are active
                        if !*activity && is_src_active {
                            any_active = true;
                            *activity = true;
                        }
                    } else {
                        //no state set, always overwrite
                        if is_src_active {
                            any_active = true;
                        }
                        self.active.set(output_port.into(), is_src_active);
                    }
                }
                //Set the node activ, if any output is active and return.
                self.active.set(node.into(), any_active);
                any_active
            }
            ty => panic!("unhandled activity for node type {ty:?}"),
        }
    }
}

impl Optimizer {
    ///Explores the `entrypoint`'s expression in relation to its wrt-argument.
    /// Taggs all nodes in the expression that are influenced by the wrt-argument.
    ///
    /// Also enters sub regions and called functions.
    pub(crate) fn activity_explorer(&self, entrypoint: NodeRef) -> Result<Activity, OptError> {
        // This is a three-stage process.
        // 1. Find the actual value producer that is _active_. The dialects permit none-transforming nodes like _index_

        let region = self.graph[entrypoint].parent.unwrap();
        let wrt_src = self
            .graph
            .inport_src(InportLocation {
                node: entrypoint,
                input: AutoDiff::wrt_input(),
            })
            .unwrap();

        let expr_src = self
            .graph
            .inport_src(InportLocation {
                node: entrypoint,
                input: AutoDiff::expr_input(),
            })
            .unwrap();

        let (activity_seed_nodes, wrt_producer) = self.trace_activity_seed(wrt_src);

        //NOTE: right now we just build the activity for all nodes that are predecessors to the expression
        //
        //TODO: We _could_ also build that thing lazyly. At least the Activity _thingy_ would permit that. But
        //      In that case we'd have to update all passes that _assume_ all active nodes set to querry instead.

        let mut predecerssors = self
            .graph
            .walk_predecessors_in_region(expr_src.node)
            .collect::<Vec<_>>();
        //Push ourselfs into that node as well
        predecerssors.push(expr_src);

        let mut activity = Activity {
            active: activity_seed_nodes,
            wrt_producer,
        };

        //Set all argument ports to this region, that are not already active as inactive. This is needed
        //later on in the `Activity::is_active_port()` to handle inactive-arguments correctly
        for argument in self.graph[region.node].argument_types(region.region_index) {
            let port_loc = OutportLocation {
                node: region.node,
                output: argument,
            };
            if activity.active.get(&port_loc.into()).is_none() {
                activity.active.set(port_loc.into(), false);
            }
        }

        //Now just querry the _activity_ state once for each output, which lets the helper
        //build that state.
        for pred in self
            .graph
            .walk_predecessors_in_region(expr_src.node)
            .chain([expr_src].into_iter())
        {
            let _val = activity.is_active_port(self, pred);
        }

        Ok(activity)
    }

    fn trace_activity_seed(&self, wrt: OutportLocation) -> (FlagStore<bool>, WrtProdMap) {
        //First build a path of indexes, that let us access the wrt-port's scalar value.

        let mut path = SmallColl::new();
        self.build_prime_path(wrt, &mut path, 0);

        //Pop the last element, which is the actual producer at  this point
        let start = path.pop().unwrap();

        let access_index_list = if path.len() > 0 {
            //If there ist still a rest, unify that to indices
            path.reverse();
            self.unify_index_path(path)
        } else {
            //otherwise the start is already the scalar value wrt-arg,
            //so no index list
            SmallColl::new()
        };

        //Now explore all start-connected non-value-producer chains with regards to the access-index-list
        //and tag all producers in the prod map, as well as all ports and nodes we see as active
        self.explore_producer_trace(start, access_index_list)
    }

    //Small helper that iterates the path from the back to find the last ConstantIndex on that path.
    fn last_n_index_on_path(&self, path: &[OutportLocation], n: usize) -> Option<usize> {
        let mut n = n;
        for port in path.iter().rev() {
            if let Some(idx) = self.try_unwrap_node::<ConstantIndex>(port.node) {
                if n == 0 {
                    return Some(idx.access);
                } else {
                    n = n - 1;
                }
            }
        }

        None
    }

    //Recursive prime path construction. Builds the `path` to the initial value producing path
    fn build_prime_path(
        &self,
        port: OutportLocation,
        path: &mut SmallColl<OutportLocation>,
        consecutive_construct_chain: usize,
    ) {
        path.push(port);
        //Break on argument-port, or value producer.
        if port.output.is_argument() || self.is_value_producer(port.node) {
            return;
        } else {
            //if this is a consruction node, take the last known index
            //else just recurse the producer
            if self.is_node_type::<ConstantIndex>(port.node) {
                let src = self.graph.inport_src(port.node.input(0)).unwrap();
                //NOTE: remove one from construct chain (if needed)
                self.build_prime_path(
                    src,
                    path,
                    consecutive_construct_chain.checked_sub(1).unwrap_or(0),
                )
            } else {
                if self.is_node_type::<UniformConstruct>(port.node) {
                    //Find the n-last-index-element...
                    //NOTE: lets say we have a chain `START->UniformConstruct[A]-UniformConstruct[B]-Index[C]-Index[D]`
                    //      then CONSTRUCT[A] is indexed by Index[D], and UniformConstruct[B] is indexed by Index[C].
                    //      so we keep track of _how-far-back_ we'd need to go for our index
                    let last_index = self
                        .last_n_index_on_path(path, consecutive_construct_chain)
                        .expect("Expected any index on path!");
                    //... so we know which construct to follow
                    let index_src = self.graph.inport_src(port.node.input(last_index)).unwrap();
                    self.build_prime_path(index_src, path, consecutive_construct_chain + 1)
                } else {
                    //Note should not happen, since self.is_value_producer() should be mutual
                    //exclusive to the else branch...
                    panic!("Unreachable reached!");
                }
            }
        }
    }

    //Unify the index path by merging all `UniformConstruct->Index[N]` chains.
    //So `UniformConstruct->Index[N]->UniformConstruct->Index[M]->Index[I]` becomes `Index[I]`.
    fn unify_index_path(&self, mut prime_path: SmallColl<OutportLocation>) -> SmallColl<usize> {
        //NOTE use a restart loop that searches UniformConstruct-Index pairs from the front till it can't find any
        'restart: loop {
            let mut construct_index = None;
            for i in 0..prime_path.len() {
                if let Some(construct_index) = construct_index {
                    //searching for a index
                    if self.is_node_type::<ConstantIndex>(prime_path[i].node) {
                        //remove both from the prime path and restart
                        assert!(construct_index < i);
                        prime_path.remove(i);
                        prime_path.remove(construct_index);
                        continue 'restart;
                    }
                } else {
                    //Searching for a construct
                    if self.is_node_type::<UniformConstruct>(prime_path[i].node) {
                        construct_index = Some(i);
                    }
                }
            }
            //If we came till here, the search didn't find anything, so break out of the restart loop
            break;
        }

        //Now unify the filtered prime path to a list of indices
        prime_path
            .into_iter()
            .map(|port| {
                self.try_unwrap_node::<ConstantIndex>(port.node)
                    .expect("Expected only index-nodes!")
                    .access
            })
            .collect()
    }

    fn explore_producer_trace(
        &self,
        prod: OutportLocation,
        index_path: SmallColl<usize>,
    ) -> (FlagStore<bool>, WrtProdMap) {
        let mut activity = FlagStore::new();
        let mut prodmap = WrtProdMap::new();

        //Pre insert the producer
        prodmap.insert(prod, index_path.clone());
        activity.set(prod.into(), true);

        //Now explore all succ to the producer, stopping at value-producing nodes (and result-ports)
        for succ in self.graph[prod].edges.clone() {
            let dst = *self.graph[succ].dst();
            self.explore_inport(&mut activity, &mut prodmap, dst, index_path.clone());
        }

        (activity, prodmap)
    }

    fn explore_inport(
        &self,
        activity: &mut FlagStore<bool>,
        prodmap: &mut WrtProdMap,
        port: InportLocation,
        mut index_path: SmallColl<usize>,
    ) {
        //Only recurse for non producers
        if !self.is_value_producer(port.node) {
            //for constructs, checkout _from where_ we come, and add that index
            //to the list, for ConstantIndex, check if we are indexing _the correct_
            //value.

            if let Some(constant_index) = self.try_unwrap_node::<ConstantIndex>(port.node) {
                assert!(
                    index_path.len() > 0,
                    "Expected non empty index path at ConstantIndex, this is probably a bug!"
                );
                let expected = index_path.remove(0);
                if expected == constant_index.access {
                    //is the correct one, add this node to the producer list, and to the active list
                    prodmap.insert(port.node.output(0), index_path.clone());
                    activity.set(port.node.into(), true);
                    activity.set(port.node.output(0).into(), true);
                    //and finally recurse
                    for dst in self.graph.outport_dsts(port.node.output(0)) {
                        self.explore_inport(activity, prodmap, dst, index_path.clone());
                    }
                } else {
                    activity.set(port.node.into(), false);
                    activity.set(port.node.output(0).into(), false);
                    //is not the correct index, break recursion
                    return;
                }
            } else {
                if self.is_node_type::<UniformConstruct>(port.node) {
                    let const_index = if let InputType::Input(n) = port.input {
                        n
                    } else {
                        panic!("Unexpected inport-type on construct: {:?}", port);
                    };
                    //Push to front, since we need to index that, to arrive back at the old path
                    index_path.insert(0, const_index);

                    //add node to active and producer list
                    activity.set(port.node.into(), true);
                    activity.set(port.node.output(0).into(), true);
                    //NOTE: Add to prod map with _new_ index_path
                    prodmap.insert(port.node.output(0), index_path.clone());

                    //now recurs
                    for dst in self.graph.outport_dsts(port.node.output(0)) {
                        self.explore_inport(activity, prodmap, dst, index_path.clone());
                    }
                }
            }
        } else {
            //In case of a non producer, checkout what case we have.
            match self.graph[port.node].into_abstract() {
                AbstractNodeType::Simple => {
                    //For simple nodes, this declares a _use_ of the active node,
                    //therefore set node itself as active, and all outputs as well
                    activity.set(port.node.into(), true);
                    for output in self.graph[port.node].outport_types() {
                        activity.set(
                            OutportLocation {
                                node: port.node,
                                output,
                            }
                            .into(),
                            true,
                        );
                    }
                }
                AbstractNodeType::Apply => {
                    //For apply nodes, this declares a _use_ of the active node,
                    //therefore set node itself as active, and all outputs as well
                    activity.set(port.node.into(), true);
                    for output in self.graph[port.node].outport_types() {
                        activity.set(
                            OutportLocation {
                                node: port.node,
                                output,
                            }
                            .into(),
                            true,
                        );
                    }
                }
                AbstractNodeType::Gamma => {
                    //For gamma node, bridge into regions if its an entry argument
                    if let InputType::EntryVariableInput(_) = port.input {
                        for subreg in 0..self.graph[port.node].regions().len() {
                            let output = port.input.map_to_in_region(subreg).unwrap();
                            let cvarg = OutportLocation {
                                node: port.node,
                                output,
                            };
                            for inport in self.graph.outport_dsts(cvarg) {
                                self.explore_inport(activity, prodmap, inport, index_path.clone());
                            }
                        }
                    } else {
                        panic!("Unhandeled Gamma Input {:?}", port.input);
                    }
                }
                AbstractNodeType::Lambda => {
                    if let InputType::ContextVariableInput(_cv) = port.input {
                        //For context variables, just bridge into the region and continue
                        let output = port.input.map_to_in_region(0).unwrap();
                        let cvarg = OutportLocation {
                            node: port.node,
                            output,
                        };
                        for inport in self.graph.outport_dsts(cvarg) {
                            self.explore_inport(activity, prodmap, inport, index_path.clone());
                        }
                    } else {
                        panic!("Unhandeled λ-none-context-variable input")
                    }
                }
                AbstractNodeType::Theta => {
                    //NOTE: Handling theta-node activity is a little bit hard, since people can do strange stuff
                    //      with activity indexing, which makes it hard to build the producer map correctly
                    //      So for now we do the same thing as for the gamma-node, and just ignore that there are multiply iterations.
                    //
                    //      In practice the forward-pass (and backward probably too) will canonicalize the ThetaNodes by unrolling them.
                    // TODO: Fix that by handling multiple per-iteration occurances of index / construct operations.
                    if let InputType::Input(_) = port.input {
                        let output = port.input.map_to_in_region(0).unwrap();
                        let cvarg = OutportLocation {
                            node: port.node,
                            output,
                        };
                        for inport in self.graph.outport_dsts(cvarg) {
                            self.explore_inport(activity, prodmap, inport, index_path.clone());
                        }
                    }
                }
                other => panic!(
                    "encountered unhhandeled node type in producer exploration: {:?}",
                    other
                ),
            }
        }
    }

    fn is_value_producer(&self, node: NodeRef) -> bool {
        //FIXME: While currently correct, this is a _really_ bad way of finding that out :((
        if self.is_node_type::<ConstantIndex>(node) || self.is_node_type::<UniformConstruct>(node) {
            false
        } else {
            true
        }
    }
}
