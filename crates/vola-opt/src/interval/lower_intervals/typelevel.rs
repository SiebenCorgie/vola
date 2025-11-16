/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

use rvsdg::{region::RegionLocation, NodeRef, SmallColl};
use vola_common::{Span, VolaError};

use crate::{
    interval::{lower_intervals::LowerIntervals, IntervalError},
    typelevel::UniformConstruct,
    OptError, OptNode,
};

impl<'opt> LowerIntervals<'opt> {
    pub(crate) fn lower_interval_contruct(
        &mut self,
        _region: RegionLocation,
        _span: Span,
        node: NodeRef,
    ) -> Result<(), VolaError<OptError>> {
        //For intervalontructors its pretty easy, we can just use the
        // first and second input as associations

        let start = self.optimizer.graph.inport_src(node.input(0)).unwrap();
        let end = self.optimizer.graph.inport_src(node.input(1)).unwrap();
        assert!(self.mapping.insert(node.output(0), (start, end)).is_none());
        Ok(())
    }

    ///Lowers the indexing of a constant_index operation into a interval by replacing it with the actually indexed
    /// bound-value
    pub(crate) fn lower_constant_index(
        &mut self,
        node: NodeRef,
        index: usize,
    ) -> Result<(), VolaError<OptError>> {
        //In this case, select either the lower, or upper bound, and feed that to all consumers directly.

        //Read the input's lower and upper bound.
        let (lower, upper) = self
            .mapping
            .get(&self.optimizer.graph.inport_src(node.input(0)).unwrap())
            .unwrap();
        let selected = match index {
            0 => *lower,
            1 => *upper,
            other => {
                let span = self.optimizer.find_span(node).unwrap_or(Span::empty());
                return Err(VolaError::error_here(
                    IntervalError::InvalidIntervalIndex(other).into(),
                    span,
                    "here",
                ));
            }
        };

        let selected_type = self.optimizer.get_or_derive_type(selected, false);
        for consumer in self.optimizer.graph.find_consumer_out(node.output(0)) {
            let consumers_type = self.optimizer.find_type(consumer);
            assert_eq!(consumers_type, Some(selected_type.clone()));
            //Types match, therefore replace
            let edge = self.optimizer.graph[consumer].edge.unwrap();
            let ty = self.optimizer.graph.disconnect(edge).unwrap();
            self.optimizer
                .graph
                .connect(selected, consumer, ty)
                .unwrap();
        }

        Ok(())
    }

    pub(crate) fn lower_uniform_construct(
        &mut self,
        region: RegionLocation,
        span: Span,
        node: NodeRef,
    ) -> Result<(), VolaError<OptError>> {
        //Build two new UCs, one connects the lower bounds for any input that is an interval, the
        // other all the upper bounds

        assert_eq!(self.optimizer.graph[node].outputs().len(), 1);

        let (lower_srcs, upper_srcs) = self.optimizer.graph[node]
            .input_srcs(&self.optimizer.graph)
            .into_iter()
            .map(|src| {
                let src = src.expect("UniformConstant source value must be set");
                //If the src is an interval, map it once to lower and upper bound, otherwise just pass through
                if self.optimizer.get_or_derive_type(src, false).is_interval() {
                    //Note: Must exist, since we assume a TopoOrd traversal
                    self.mapping.get(&src).unwrap().clone()
                } else {
                    (src, src)
                }
            })
            .collect::<(SmallColl<_>, SmallColl<_>)>();

        let (lower, upper) = self
            .optimizer
            .graph
            .on_region(&region, |reg| {
                let (lower, _) = reg
                    .connect_node(
                        OptNode::new(
                            UniformConstruct::new().with_inputs(lower_srcs.len()),
                            span.clone(),
                        ),
                        lower_srcs,
                    )
                    .unwrap();

                let (upper, _) = reg
                    .connect_node(
                        OptNode::new(UniformConstruct::new().with_inputs(upper_srcs.len()), span),
                        upper_srcs,
                    )
                    .unwrap();

                (lower.output(0), upper.output(0))
            })
            .unwrap();

        //Register the new lower/upper uc.
        assert!(self
            .mapping
            .insert(node.output(0), (lower, upper))
            .is_none());
        Ok(())
    }
}
