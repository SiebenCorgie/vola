/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

use rvsdg::{
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    region::RegionLocation,
    NodeRef,
};
use vola_common::{ariadne::Label, report, warning_reporter, Span, VolaError};

use crate::{
    common::Ty,
    interval::{lower_intervals::LowerIntervals, IntervalError},
    typelevel::{ConstantIndex, IntervalConstruct, NonUniformConstruct},
    OptEdge, OptError, OptNode,
};

impl<'opt> LowerIntervals<'opt> {
    ///Checks that the apply-node was handled before
    pub(crate) fn lower_apply(&mut self, node: NodeRef) -> Result<(), VolaError<OptError>> {
        //For sanity, make sure the λ's results/arguments where lowered already
        // This manly means that there is in-fact no interval-typed input or output.
        //
        // Any interval-related apply-node should have been handled before in the λ-lowering of the called λ-node.
        let has_any_interval_ports = self.has_interval_in_or_out(node);

        if has_any_interval_ports {
            Err(VolaError::new(OptError::Internal(format!("There is a bug in the interval lowering, the function-call still operates on intervals!"))))
        } else {
            Ok(())
        }
    }

    ///Returns true if any of the `node`'s inputs or outputs are interval-typed.
    pub(crate) fn has_interval_in_or_out(&self, node: NodeRef) -> bool {
        self.optimizer
            .graph
            .inports(node)
            .into_iter()
            .filter_map(|port| {
                if self
                    .optimizer
                    .find_type(port)
                    .map_or(false, |t| t.is_interval())
                {
                    Some(())
                } else {
                    None
                }
            })
            .chain(
                self.optimizer
                    .graph
                    .outports(node)
                    .into_iter()
                    .filter_map(|port| {
                        if self
                            .optimizer
                            .find_type(port)
                            .map_or(false, |t| t.is_interval())
                        {
                            Some(())
                        } else {
                            None
                        }
                    }),
            )
            .next()
            .is_some()
    }

    ///Returns true if any of `region`'s arguments or results contain a interval-typed value.
    pub(crate) fn has_interval_in_region_interface(&self, region: RegionLocation) -> bool {
        //Note this basically just maps each result to its port, and then checks whether there
        // is a interval type
        self.optimizer
            .graph
            .result_ports(region)
            .into_iter()
            .filter_map(|port| {
                if self
                    .optimizer
                    .find_type(port)
                    .map_or(false, |t| t.is_interval())
                {
                    Some(())
                } else {
                    None
                }
            })
            //This does the same for all arguments
            .chain(
                self.optimizer
                    .graph
                    .argument_ports(region)
                    .into_iter()
                    .filter_map(|port| {
                        if self
                            .optimizer
                            .find_type(port)
                            .map_or(false, |t| t.is_interval())
                        {
                            Some(())
                        } else {
                            None
                        }
                    }),
            )
            .next()
            .is_some()
    }

    //Handles the λ-node by lowering the interface to tuples for any interval-typed argument/result.
    // returns true if any nodes where changed.
    pub(crate) fn lower_lambda(&mut self, node: NodeRef) -> Result<bool, VolaError<OptError>> {
        //Checkout whether any of the arguments is an interval. If so, mutate them to a tuple,
        //For outputs, if they are a interval, mutate them to construct a tuple too, by indexing into the interval.
        // We need to do that, in order for a later lowering of the output-connected node to succeed

        //NOTE: there is a special-case: If a interval is in the interface description, we issue a warning that the interface
        //      will be lowered to a tuple.
        let is_exported = self
            .optimizer
            .exported_functions()
            .into_iter()
            .filter(|reg| reg.node == node)
            .next()
            .is_some();
        let has_interval_in_interface = self.has_interval_in_region_interface(RegionLocation {
            node,
            region_index: 0,
        });
        if is_exported && has_interval_in_interface {
            //warn that this uses an interval in the export
            let span = self.optimizer.find_span(node).unwrap_or(Span::empty());
            report(
                warning_reporter(IntervalError::InExport.to_string(), span.clone())
                    .with_label(Label::new(span).with_message("here"))
                    .finish(),
            )
        }

        let call_sites = self.optimizer.graph.find_caller(node).unwrap();
        /* TODO: Reactivate if the _has-interval-in-interface_ function can handle it...
        //NOTE: If this λ is not exported, but also not called, we can safely bail
        if call_sites.len() == 0 && !is_exported {
            #[cfg(feature = "log")]
            log::trace!("Not lowering λ {}, since its not exported or called", node);
            return Ok(false);
        }
        */

        //now, if there are any intervals in the interface, handle them
        if has_interval_in_interface {
            let region = RegionLocation {
                node,
                region_index: 0,
            };
            for argument in self.optimizer.graph.argument_ports(region) {
                if self.optimizer.find_type(argument).unwrap().is_interval() {
                    self.handle_arg(region, argument, &call_sites);
                }
            }

            for result in self.optimizer.graph.result_ports(region) {
                if self.optimizer.find_type(result).unwrap().is_interval() {
                    self.handle_result(region, result, &call_sites);
                }
            }
        }

        //Regardless, enque this λ's region, since we where touched anyways
        self.region_queue.push_front(RegionLocation {
            node,
            region_index: 0,
        });

        Ok(true)
    }

    fn handle_arg(&mut self, region: RegionLocation, arg: OutportLocation, callsites: &[NodeRef]) {
        //First of all, handle the arg port
        self.itt_outport(region, arg);
        //now go to all call-sites and make sure the in-port is handled too
        for cs in callsites.iter() {
            assert!(self.optimizer.graph[*cs].node_type.is_apply());
            //for the argument at the call-site we just have to take the argument-index and _increase_ it once, since
            // the first port of the apply node is the λ it is calling.
            let OutputType::Argument(i) = arg.output else {
                panic!("must be arg")
            };
            let input = cs.input(i + 1);
            let csregion = self.optimizer.graph[*cs].parent.unwrap();
            self.itt_inport(csregion, input);
        }
    }

    fn handle_result(
        &mut self,
        region: RegionLocation,
        res: InportLocation,
        callsites: &[NodeRef],
    ) {
        //for results we basically do the the same as for arguments, but in reverse.
        // I.e. we first transform the result port, and then all outputs of any
        // call-site
        self.itt_inport(region, res);
        for cs in callsites.iter() {
            let InputType::Result(r) = res.input else {
                panic!("must have been a result!");
            };
            let output = cs.output(r);
            let csregion = self.optimizer.graph[*cs].parent.unwrap();
            self.itt_outport(csregion, output);
        }
    }

    ///Turns a interval typed Outport into a tuple typed port that constructs
    /// an interval from that tuple. Records the interval-mapping nevertheless
    pub(crate) fn itt_outport(&mut self, region: RegionLocation, port: OutportLocation) {
        let base_type = match self.optimizer.find_type(port) {
            Some(Ty::Interval(i)) => *i,
            other => panic!("Expected interval type on {port}, got {other:?}"),
        };
        //Set the span to the function's call-site for now.
        let span = self.optimizer.find_span(port.node).unwrap_or(Span::empty());

        //setup the new_type
        let tuple_type = Ty::Tuple(vec![base_type.clone(), base_type.clone()]);
        //setup the interval_from_tuple, then disconnect all arg-connected edges, and reconnect them to the just created
        let (start, end) = self
            .optimizer
            .graph
            .on_region(&region, |reg| {
                let istart = reg.insert_node(OptNode::new(ConstantIndex::new(0), span.clone()));
                let iend = reg.insert_node(OptNode::new(ConstantIndex::new(1), span.clone()));

                //This creates the interval substitution
                let (i_creation, _) = reg
                    .connect_node(
                        OptNode::new(IntervalConstruct::default(), span.clone()),
                        [istart.output(0), iend.output(0)],
                    )
                    .unwrap();
                //now disconnecy all arg-connected _things_, and connect them to the interval
                for dst in reg.ctx().outport_dsts(port) {
                    let Some(edg) = reg.ctx()[dst].edge.clone() else {
                        continue;
                    };

                    let ty = reg.ctx_mut().disconnect(edg).unwrap();
                    assert!(ty.get_type().unwrap().is_interval());
                    //now reconnect it to the interval-creation node
                    reg.ctx_mut()
                        .connect(i_creation.output(0), dst, ty)
                        .unwrap();
                }

                //finally connect the i_start / i_end indexing to the arg.
                reg.ctx_mut()
                    .connect(
                        port,
                        istart.input(0),
                        OptEdge::value_edge_unset().with_type(tuple_type.clone()),
                    )
                    .unwrap();
                reg.ctx_mut()
                    .connect(
                        port,
                        iend.input(0),
                        OptEdge::value_edge_unset().with_type(tuple_type.clone()),
                    )
                    .unwrap();

                (istart.output(0), iend.output(0))
            })
            .unwrap();
        //now last, overwrite the output's type, if the port was set before, it must have been
        // an interval
        if let Some(old) = self.optimizer.typemap.set(port.into(), tuple_type) {
            assert!(old.is_interval())
        }
        assert!(self.mapping.insert(port, (start, end)).is_none())
    }

    ///Turns a interval typed Inport into a tuple typed port by constructing
    /// a tuple from the indexed interval
    pub(crate) fn itt_inport(&mut self, region: RegionLocation, port: InportLocation) {
        //We do this by building a tuple via indexing into the original interval. Then reconnecting the
        // `port` to to that just build tuple

        let Ty::Interval(base_type) = self.optimizer.find_type(port).unwrap() else {
            panic!("Port must be interval typed!")
        };

        //Set the span to the function's call-site for now.
        let span = self.optimizer.find_span(port.node).unwrap_or(Span::empty());
        let tuple_type = Ty::Tuple(vec![base_type.as_ref().clone(), base_type.as_ref().clone()]);

        //setup the indexing, then disconnect the port, and reconnect it with the tuple
        self.optimizer.graph.on_region(&region, |reg| {
            let src = reg.ctx().inport_src(port).unwrap();
            let (istart, _) = reg
                .connect_node(OptNode::new(ConstantIndex::new(0), span.clone()), [src])
                .unwrap();
            let (iend, _) = reg
                .connect_node(OptNode::new(ConstantIndex::new(1), span.clone()), [src])
                .unwrap();

            //setup the tuple
            let (tuple_const, _) = reg
                .connect_node(
                    OptNode::new(NonUniformConstruct::new(2), span.clone()),
                    [istart.output(0), iend.output(0)],
                )
                .unwrap();
            //now disconnect the old interval producer and connect our fresh tuple
            let edg = reg.ctx()[port].edge.unwrap();
            let old = reg.ctx_mut().disconnect(edg).unwrap();
            assert!(old.get_type().unwrap().is_interval());
            reg.ctx_mut()
                .connect(
                    tuple_const.output(0),
                    port,
                    OptEdge::value_edge_unset().with_type(tuple_type.clone()),
                )
                .unwrap();
        });

        //now last, overwrite the input's type, if the port was set before, it must have been
        // an interval
        if let Some(old) = self.optimizer.typemap.set(port.into(), tuple_type) {
            assert!(old.is_interval())
        }
    }
}
