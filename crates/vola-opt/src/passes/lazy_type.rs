/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

//! Implements lazy type discovery on the graph.
//!
//! The whole algorithm operates under the following key-assumption:
//!
//! 1. Any argument of a λ/ω-Node has a valid type-tag
//! 2. Any result of a λ/ω-Node has a valid type-tag
//! 3. Simple-Nodes with no inputs always successfuly output a type on [DialectNode::try_derive_type]
//! 4. All inputs of a simple/apply node are in fact connected
//! 5. All results of a lambda/phi node are in fact connected
//!
//! If those assumptions are holding true, the type-discovery algorithm always returns a type for any given out- or input.

use ahash::AHashMap;
use rvsdg::{
    attrib::AttribLocation,
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    util::abstract_node_type::AbstractNodeType,
    SmallColl,
};
use thiserror::Error;
use vola_common::{Span, VolaError};

use crate::{common::Ty, OptEdge, Optimizer, TypeState};

///Implements the immutable type resolver
mod immutable;
mod initial;
///Implements the mutable type resolver
mod mutable;

#[derive(Debug, Error, Clone)]
pub enum TypeError {
    ///On type collisions in the graph. Generally indicates a bug in some compiler pass.
    #[error("Type collision on {location}:\nset:        {was}\nderived to: {derived}")]
    Collision {
        location: AttribLocation,
        was: Ty,
        derived: Ty,
    },
    #[error("A key assumption was violated at {0}")]
    AssumptionViolation(AttribLocation),

    ///Violation of 1.
    #[error("Argument {0} of function has no type")]
    LambdaArgumentUnset(usize),
    #[error("Context {0} for function has no type set")]
    LambdaContextUnset(usize),
    ///Violation of 2
    #[error("Result {0} of function has no type")]
    LambdaResultUnset(usize),

    #[error("{0} is part of a state-edge")]
    IsState(AttribLocation),
    #[error("Mixed state and value types")]
    MixedEdgeType,
    #[error("No edge was type-set")]
    NoEdgeTypeSet,
    ///Note, you can usually recover from this, if you are operating on dead-code.
    /// In that case just ignore the untyped-port since its dead anyways.
    #[error("Encountered some unexpectedly unconnected port {0}")]
    UnconnectedPort(AttribLocation),
    #[error("Type state derive got stuck at {0}")]
    Stuck(AttribLocation),
    #[error("Shape Error: {0}")]
    ShapeError(String),
    #[error("{0}")]
    Other(String),
}

enum ResolveState {
    ///Needs type info from a specific, recorded, but not yet resolved port
    WaitingFor(OutportLocation),
    ///Succesfuly resolved to this type
    ResolvedTo(Ty),
}

impl Optimizer {
    ///Returns the type of `outport` without setting any edge-types in the process. If you have mutable
    /// access to the [Optimizer], prefer [get_out_type_mut].
    pub fn get_out_type(&self, outport: OutportLocation) -> Result<Ty, VolaError<TypeError>> {
        self.type_discover(outport)
    }

    ///Returns the type of `inport` without setting any edge-types in the process. If you have mutable
    /// access to the [Optimizer], prefer [get_in_type_mut].
    pub fn get_in_type(&self, inport: InportLocation) -> Result<Ty, VolaError<TypeError>> {
        if let Some(src) = self.graph.inport_src(inport) {
            self.get_out_type(src)
        } else {
            let span = self.find_span(inport).unwrap_or(Span::empty());
            Err(VolaError::error_here(
                TypeError::AssumptionViolation(inport.into()),
                span,
                "undefined value",
            ))
        }
    }

    ///Returns the type of `outport`. Sets all known type state on unset-edges along the way.
    pub fn get_out_type_mut(
        &mut self,
        outport: OutportLocation,
    ) -> Result<Ty, VolaError<TypeError>> {
        self.type_discover_and_set(outport)
    }

    ///Returns the type of `outport`. Sets all known type state on unset-edges along the way.
    pub fn get_in_type_mut(&mut self, inport: InportLocation) -> Result<Ty, VolaError<TypeError>> {
        if let Some(src) = self.graph.inport_src(inport) {
            self.get_out_type_mut(src)
        } else {
            let span = self.find_span(inport).unwrap_or(Span::empty());
            Err(VolaError::error_here(
                TypeError::AssumptionViolation(inport.into()),
                span,
                "undefined value",
            ))
        }
    }

    ///Tries to build a unified type from all connected edges of `outport`.
    /// Unset value-edges are ignored/assumed to be the same.
    /// Fails on type collisions, and edge-type collision (i.e. Value/State edge mixed on the same port).
    pub fn unified_outport_type(&self, outport: OutportLocation) -> Result<OptEdge, TypeError> {
        let mut unified = None;
        for edge in &self.graph[outport].edges {
            match (&unified, &self.graph[*edge].ty) {
                //All right
                (Some(OptEdge::State), OptEdge::State) => {}
                //Test if the type-info is the same / could-be refined.
                (Some(OptEdge::Value { ty: a }), OptEdge::Value { ty: b }) => {
                    match (a.get_type(), b.get_type()) {
                        (Some(aty), Some(bty)) => {
                            if aty != bty {
                                return Err(TypeError::Collision {
                                    location: outport.into(),
                                    was: bty,
                                    derived: aty,
                                });
                            }
                            //otherwise its okay
                        }
                        //In this case we _refined_ from just _Value_ to _TypeValue_
                        (None, Some(ty)) | (Some(ty), None) => {
                            unified = Some(OptEdge::Value {
                                ty: TypeState::Derived(ty),
                            });
                        }
                        //Also okay, both are unset type-state
                        (None, None) => {}
                    }
                }
                //First discovery of any state info
                (None, any) => unified = Some(any.clone()),
                //Mixed edge types are invalid
                (Some(OptEdge::State), OptEdge::Value { .. })
                | (Some(OptEdge::Value { .. }), OptEdge::State) => {
                    return Err(TypeError::MixedEdgeType)
                }
            }
        }

        if let Some(ty) = unified {
            Ok(ty)
        } else {
            Err(TypeError::NoEdgeTypeSet)
        }
    }

    ///Explores `outport`'s predecesors until type information is known for each leaf of the dependency-dag.
    ///
    /// Returns the (reverse topological) order of the dependencies (i.e. if you reverse that, you get
    /// the topo-order of the dependency-dag), as well as a localy build type-map. This typemap safes whether a outport was
    /// discovered (exists as key), and what type information is known (the value).
    fn type_exploration(
        &self,
        report_span: Span,
        outport: OutportLocation,
    ) -> Result<(Vec<OutportLocation>, AHashMap<OutportLocation, Option<Ty>>), VolaError<TypeError>>
    {
        // This iterate the ancestors of nodes until we
        // a.) Find type information on a edge
        // b.) Arrive at a λ-argument that is already type-set
        //
        // Once we found a type for any ancestor-seeding point, we iterate the queue backwards
        // call the type-derive for that node, and record the edge-type mapping.
        //
        // In the mutable case we could record that directly to the edge. But we are in the immutable version,
        // so we can't :(
        let mut lookat_queue = Vec::default();
        lookat_queue.push(outport);
        let mut local_type_lookup: AHashMap<OutportLocation, Option<Ty>> = AHashMap::default();
        local_type_lookup.insert(outport, None);

        let mut qidx = 0;
        'explore: loop {
            let Some(port) = lookat_queue.get(qidx).cloned() else {
                //No more ports in the queue, therfore end
                break 'explore;
            };
            let node_type = self.graph[port.node].into_abstract();
            //For all of those we can assume that the ports are indeed type-set
            if node_type == AbstractNodeType::Lambda
                || node_type == AbstractNodeType::Omega
                || node_type == AbstractNodeType::Phi
            {
                let Some(ty) = self.typemap.get(&port.into()) else {
                    match port.output {
                        OutputType::Argument(index) => {
                            let span = self.find_span(port.node).unwrap_or(report_span);
                            return Err(VolaError::error_here(
                                TypeError::LambdaArgumentUnset(index),
                                span,
                                "argument should be type-set",
                            ));
                        }
                        OutputType::LambdaDeclaration => {
                            let span = self.find_span(port.node).unwrap_or(report_span);
                            return Err(VolaError::error_here(
                                TypeError::AssumptionViolation(port.into()),
                                span,
                                "Function was not properly declared",
                            ));
                        }
                        OutputType::ContextVariableArgument(_) => {
                            //NOTE: Context-variables can-not be assumed to exist, therefore push all
                            //      connected producer...
                            if let Some(src) = self.graph.inport_src(
                                port.output
                                    .map_out_of_region()
                                    .unwrap()
                                    .to_location(port.node),
                            ) {
                                if !local_type_lookup.contains_key(&src) {
                                    local_type_lookup.insert(src, None);
                                    lookat_queue.push(src);
                                }
                            } else {
                                return Err(VolaError::error_here(
                                    TypeError::UnconnectedPort(port.into()),
                                    report_span,
                                    "here",
                                ));
                            }
                            //update the index and shortcut the loop
                            qidx = qidx + 1;
                            continue 'explore;
                        }
                        _ => panic!("Unexpected output type: {}", port.output),
                    }
                };
                //Use the type info and skip to next port
                local_type_lookup.insert(port, Some(ty.clone()));
                qidx = qidx + 1;
                continue 'explore;
            }

            //TODO: This does not handle state-edges yet
            match self.unified_outport_type(port) {
                Ok(OptEdge::Value { ty }) => {
                    if let Some(actual_type) = ty.get_type() {
                        //The port is indeed typed, therefore we can bail
                        local_type_lookup.insert(port, Some(actual_type));
                        qidx = qidx + 1;
                        continue;
                    }
                    //NOTE: we consider value-edges without a type _untyped_... duh
                }
                Ok(OptEdge::State) => {
                    //This _should not_ fire till we actually introduce type edges. In that
                    // case you'll need to handle that :)
                    panic!("Encounterd state edge");
                }
                //This can indeed happen and is okay.
                Err(TypeError::NoEdgeTypeSet) => {}
                Err(other_error) => {
                    return Err(VolaError::error_here(
                        other_error,
                        report_span,
                        "while trying to calculate this values's type",
                    ))
                }
            }

            //Port can not derive type. Therfore enque all ancestors of this node
            match self.graph[port.node].into_abstract() {
                AbstractNodeType::Simple => {
                    //Push all unique source ports to give the simple-node a change
                    // to type-derive later on
                    for src in self.graph.unique_src_ports(port.node) {
                        if !local_type_lookup.contains_key(&src) {
                            local_type_lookup.insert(src, None);
                            lookat_queue.push(src);
                        }
                    }
                }
                AbstractNodeType::Apply => {
                    //For apply-nodes, push the associated result in the λ node that is called
                    let lmd = self.graph.find_called(port.node).unwrap();
                    //now, assuming that the result exists, read-out the output index, and use that
                    // to build the result port
                    let OutputType::Output(idx) = port.output else {
                        unreachable!()
                    };
                    let result_port = InputType::Result(idx).to_location(lmd.node);
                    //Now we can assume that the result is indeed already type-set
                    // (per definition)
                    if let Some(ty) = self.typemap.get(&result_port.into()) {
                        let _ = local_type_lookup.insert(port, Some(ty.clone()));
                    } else {
                        let span = self.find_span(lmd.node).unwrap_or(report_span);
                        let base_error = VolaError::error_here(
                            TypeError::LambdaResultUnset(idx),
                            span,
                            "called function's result undefined",
                        );
                        if let Some(call_span) = self.find_span(port.node) {
                            return Err(base_error.with_label(call_span, "called here"));
                        } else {
                            return Err(base_error);
                        }
                    }

                    //also push all call-args
                    for src in self.graph.unique_src_ports(port.node) {
                        if !local_type_lookup.contains_key(&src) {
                            local_type_lookup.insert(src, None);
                            lookat_queue.push(src);
                        }
                    }
                }

                AbstractNodeType::Gamma => {
                    //There are two cases we have to handle: For an exit-var output, we map _into_ all branches and register
                    // the sources.
                    // For Entry-Var-Arg we map _out_ of the gamma-node and register all sources.
                    match port.output {
                        OutputType::ExitVariableOutput(_) => {
                            for branch in 0..self.graph[port.node].regions().len() {
                                if let Some(src) = self.graph.inport_src(
                                    port.output
                                        .map_to_in_region(branch)
                                        .unwrap()
                                        .to_location(port.node),
                                ) {
                                    if !local_type_lookup.contains_key(&src) {
                                        local_type_lookup.insert(src, None);
                                        lookat_queue.push(src);
                                    }
                                } else {
                                    //NOTE: this can happen, if some immutable ex-variale was not
                                    //      properly disconnected.
                                    return Err(VolaError::error_here(
                                        TypeError::UnconnectedPort(port.into()),
                                        report_span,
                                        "here",
                                    ));
                                }
                            }
                        }
                        OutputType::EntryVariableArgument { .. } => {
                            if let Some(src) = self.graph.inport_src(
                                port.output
                                    .map_out_of_region()
                                    .unwrap()
                                    .to_location(port.node),
                            ) {
                                if !local_type_lookup.contains_key(&src) {
                                    local_type_lookup.insert(src, None);
                                    lookat_queue.push(src);
                                }
                            } else {
                                //NOTE: this can happen, if some ev was not
                                //      properly disconnected.
                                return Err(VolaError::error_here(
                                    TypeError::UnconnectedPort(port.into()),
                                    report_span,
                                    "here",
                                ));
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                AbstractNodeType::Theta => {
                    //Theta works similar to gamma, map outputs _into_ the loop body, and
                    // and arguments _out_
                    match port.output {
                        OutputType::Output(_) => {
                            if let Some(src) = self.graph.inport_src(
                                port.output
                                    .map_to_in_region(0)
                                    .unwrap()
                                    .to_location(port.node),
                            ) {
                                if !local_type_lookup.contains_key(&src) {
                                    local_type_lookup.insert(src, None);
                                    lookat_queue.push(src);
                                }
                            } else {
                                //NOTE: this can happen, if some immutable loop-variale was not
                                //      properly disconnected.
                                return Err(VolaError::error_here(
                                    TypeError::UnconnectedPort(port.into()),
                                    report_span,
                                    "here",
                                ));
                            }
                        }
                        OutputType::Argument(_) => {
                            if let Some(src) = self.graph.inport_src(
                                port.output
                                    .map_out_of_region()
                                    .unwrap()
                                    .to_location(port.node),
                            ) {
                                if !local_type_lookup.contains_key(&src) {
                                    local_type_lookup.insert(src, None);
                                    lookat_queue.push(src);
                                }
                            } else {
                                //NOTE: this can happen, if some immutable loop-variale was not
                                //      properly disconnected.
                                return Err(VolaError::error_here(
                                    TypeError::UnconnectedPort(port.into()),
                                    report_span,
                                    "here",
                                ));
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                //We don't use that at all I think...
                AbstractNodeType::Delta => unimplemented!(),
                //If you ended up here, the assumptions didn't hold true somewhere (but probably not here).
                AbstractNodeType::Omega | AbstractNodeType::Lambda | AbstractNodeType::Phi => {
                    unreachable!()
                }
            }
            //Finally goto next exploration port
            qidx = qidx + 1;
        }

        Ok((lookat_queue, local_type_lookup))
    }

    ///Tries to derive a type from the known state in `lookup` and the graph's cached type-state.
    fn try_local_port_resolve(
        &self,
        port: OutportLocation,
        lookup: &AHashMap<OutportLocation, Option<Ty>>,
    ) -> Result<ResolveState, TypeError> {
        match self.graph[port.node].into_abstract() {
            AbstractNodeType::Simple => {
                assert_eq!(self.graph[port.node].outputs().len(), 1);

                let mut input_types = SmallColl::default();
                for port in self.graph[port.node].input_srcs(&self.graph) {
                    //NOTE: The port must be in the map (but maybe unset), which is why we unwrap.
                    //      If this panics, there is something wrong in the explore stage...
                    if let Some(ty) = lookup
                        .get(port.as_ref().unwrap())
                        .as_ref()
                        .expect("Used port should be recorded")
                    {
                        input_types.push(ty.clone());
                    } else {
                        //Signal that we must postpone till the wait-for port was seen.
                        return Ok(ResolveState::WaitingFor(port.unwrap()));
                    }
                }

                match self.graph[port.node]
                    .node_type
                    .unwrap_simple_ref()
                    .node
                    .try_derive_type(&input_types, &self.concepts, &self.csg_node_defs)
                {
                    Ok(ty) => Ok(ResolveState::ResolvedTo(ty)),
                    Err(e) => Err(e),
                }
            }
            AbstractNodeType::Apply => {
                //For the apply node, go into the λ's region and read out the result's
                // type
                let lmd = self.graph.find_called(port.node).unwrap();
                let OutputType::Output(index) = port.output else {
                    panic!("Must be output!")
                };
                let Some(result_src) = self
                    .graph
                    .inport_src(InputType::Result(index).to_location(lmd.node))
                else {
                    return Err(TypeError::LambdaResultUnset(index));
                };
                if let Some(src_type) = lookup.get(&result_src).as_ref().unwrap() {
                    Ok(ResolveState::ResolvedTo(src_type.clone()))
                } else {
                    Ok(ResolveState::WaitingFor(result_src))
                }
            }
            AbstractNodeType::Gamma => {
                //disect the two cases of entry or exit variable.
                // for both we mearly read the predecesor and return its looked-up value
                match port.output {
                    OutputType::EntryVariableArgument { .. } => {
                        let source_port = self
                            .graph
                            .inport_src(
                                port.output
                                    .map_out_of_region()
                                    .unwrap()
                                    .to_location(port.node),
                            )
                            .unwrap();

                        if let Some(ty) = lookup
                            .get(&source_port)
                            .expect("EV-Source should be set already")
                        {
                            Ok(ResolveState::ResolvedTo(ty.clone()))
                        } else {
                            Ok(ResolveState::WaitingFor(source_port))
                        }
                    }
                    OutputType::ExitVariableOutput(_) => {
                        //We map into the first branch and _get_ the type.
                        // For sanity we then check that all other branches produce the same
                        // type.
                        // NOTE: this should never be called for unconnected ex-vars, since the lazy_type_resolver
                        //       only operates on _live_ nodes.
                        //       If you fail here, you are probably operating on a broken graph, or dead nodes.
                        let candidate_src = self
                            .graph
                            .inport_src(
                                port.output
                                    .map_to_in_region(0)
                                    .unwrap()
                                    .to_location(port.node),
                            )
                            .unwrap();
                        let candidate_type = lookup.get(&candidate_src).unwrap().clone();
                        for rest_region in 1..self.graph[port.node].regions().len() {
                            let other_src = self
                                .graph
                                .inport_src(
                                    port.output
                                        .map_to_in_region(rest_region)
                                        .unwrap()
                                        .to_location(port.node),
                                )
                                .unwrap();
                            let other_type = lookup.get(&other_src).unwrap().clone();
                            if candidate_type != other_type {
                                //Oh no, there is a type conflict on the branch results
                                return Err(TypeError::Other(format!(
                                    "Returns {} in first branch, but {} in {}-th",
                                    candidate_type.unwrap_or(Ty::VOID),
                                    other_type.unwrap_or(Ty::VOID),
                                    rest_region + 1
                                )));
                            }
                        }
                        if let Some(ty) = candidate_type {
                            Ok(ResolveState::ResolvedTo(ty))
                        } else {
                            Ok(ResolveState::WaitingFor(candidate_src))
                        }
                    }
                    _ => unreachable!(),
                }
            }
            AbstractNodeType::Theta => {
                //similar to gamma, but here we don't have to check any output collisions
                match port.output {
                    OutputType::Argument(_) => {
                        let source_port = self
                            .graph
                            .inport_src(
                                port.output
                                    .map_out_of_region()
                                    .unwrap()
                                    .to_location(port.node),
                            )
                            .unwrap();
                        if let Some(ty) = lookup.get(&source_port).as_ref().unwrap() {
                            Ok(ResolveState::ResolvedTo(ty.clone()))
                        } else {
                            Ok(ResolveState::WaitingFor(source_port))
                        }
                    }
                    OutputType::Output(_) => {
                        let candidate_src = self
                            .graph
                            .inport_src(
                                port.output
                                    .map_to_in_region(0)
                                    .unwrap()
                                    .to_location(port.node),
                            )
                            .unwrap();
                        if let Some(ty) = lookup.get(&candidate_src).as_ref().unwrap() {
                            Ok(ResolveState::ResolvedTo(ty.clone()))
                        } else {
                            Ok(ResolveState::WaitingFor(candidate_src))
                        }
                    }
                    _ => unreachable!(),
                }
            }
            AbstractNodeType::Lambda => {
                if let OutputType::ContextVariableArgument(_cv) = port.output {
                    //Try get the src's type of this cv in the parent region. Otherwise wait for it.
                    let produce = self
                        .graph
                        .inport_src(
                            port.output
                                .map_out_of_region()
                                .unwrap()
                                .to_location(port.node),
                        )
                        .unwrap();
                    if let Some(ty) = lookup.get(&produce).as_ref().unwrap() {
                        Ok(ResolveState::ResolvedTo(ty.clone()))
                    } else {
                        Ok(ResolveState::WaitingFor(produce))
                    }
                } else {
                    unreachable!("any none-cv variable should have been caught before");
                }
            }
            AbstractNodeType::Delta => unimplemented!(),
            AbstractNodeType::Omega | AbstractNodeType::Phi => {
                unreachable!("Should be caught before")
            }
        }
    }
}
