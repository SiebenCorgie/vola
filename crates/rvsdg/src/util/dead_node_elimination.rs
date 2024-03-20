//! This module implements the generic dead-node-elimination (DNE).

#[derive(Clone, Debug, Error)]
pub enum DneError {}

use ahash::AHashSet;
use thiserror::Error;

use crate::{
    attrib::{AttribLocation, AttribStore},
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    nodes::LangNode,
    region::RegionLocation,
    Rvsdg,
};
impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    ///Applies dead-node-elimination to the whole graph.
    pub fn dead_node_elimination(&mut self) -> Result<(), DneError> {
        Ok(())
    }

    fn is_inport_alive(marks: &AttribStore<bool>, port: InportLocation) -> bool {
        if let Some(mark) = marks.attrib(&port.into()) {
            assert!(mark.len() <= 1);
            match mark {
                [false, ..] => false,
                [true, ..] => true,
                [] => false,
            }
        } else {
            #[cfg(feature = "log")]
            log::warn!("Found invalid liveness check!");

            false
        }
    }

    fn is_outport_alive(marks: &AttribStore<bool>, port: OutportLocation) -> bool {
        if let Some(mark) = marks.attrib(&port.into()) {
            assert!(mark.len() <= 1);
            match mark {
                [false, ..] => false,
                [true, ..] => true,
                [] => false,
            }
        } else {
            #[cfg(feature = "log")]
            log::warn!("Found invalid liveness check!");

            false
        }
    }

    fn mark_outport(marks: &mut AttribStore<bool>, port: OutportLocation, is_alive: bool) {
        if let Some(marks) = marks.attrib_mut(&port.into()) {
            //overwrite
            marks.clear();
            marks.push(is_alive);
        } else {
            marks.push_attrib(&port.into(), is_alive)
        }
    }

    fn mark_inport(marks: &mut AttribStore<bool>, port: InportLocation, is_alive: bool) {
        if let Some(marks) = marks.attrib_mut(&port.into()) {
            //overwrite
            marks.clear();
            marks.push(is_alive);
        } else {
            marks.push_attrib(&port.into(), is_alive)
        }
    }

    ///Applies the dead-node-elimination on `region` and all its children (in topological order).
    pub fn dne_region(
        &mut self,
        region: RegionLocation,
        marks: &mut AttribStore<bool>,
    ) -> Result<(), DneError> {
        //We basically coppy the algorithm VI: DeadNodeElimination of the [source paper](http://arxiv.org/abs/1912.05036v2)
        //We express the `marks` via the attribute store and do traversal via recursion.
        //
        // So the _mark-phase_ is before the recursion down to the sub-regions.
        // once the recursion comes back, all child-regions are marked acordingly, so we can use that information to do
        // our own elimination/sweep pass.

        //IMPORTANT NOTE: We don't use the relative addressing of attributes, since we don't care for the difference of
        //                the different argument-like ports. We just use In/output and result/argument.

        //PREPARE: by copying over all result-annotations of the region's parent node
        let rescount = self.region(&region).unwrap().results.len();
        for residx in 0..rescount {
            if Self::is_outport_alive(
                &marks,
                OutportLocation {
                    node: region.node,
                    output: OutputType::Output(residx),
                },
            ) {
                //mark ours as alive
                Self::mark_inport(
                    marks,
                    InportLocation {
                        node: region.node,
                        input: InputType::Result(residx),
                    },
                    true,
                );
            } else {
                //mark dead as well
                Self::mark_inport(
                    marks,
                    InportLocation {
                        node: region.node,
                        input: InputType::Result(residx),
                    },
                    false,
                );
            }
        }

        //MARK:
        let mut residx = 0;
        //Seed a traversal by result-connected nodes. Then use the predecessor iterator to go over all.
        //
        for res in 0..rescount {
            let seed_node = if let Some(seed) = self.region(&region).unwrap().result_src(&self, res)
            {
                seed
            } else {
                continue;
            };
        }

        Ok(())
    }
}
