/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use crate::{
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    nodes::LangNode,
    region::RegionLocation,
    Rvsdg,
};

impl<N: LangNode, E: LangEdge> Rvsdg<N, E> {
    ///Returns the region this `port` resides in.
    pub fn inport_region(&self, port: InportLocation) -> RegionLocation {
        match port.input {
            //All input-like arguments reside in the parent region
            InputType::Input(_)
            | InputType::ContextVariableInput(_)
            | InputType::EntryVariableInput(_)
            | InputType::GammaPredicate => self[port.node].parent.unwrap(),
            //All _simple_ results reside in the first (and only) region
            InputType::RecursionVariableResult(_)
            | InputType::ThetaPredicate
            | InputType::Result(_) => RegionLocation {
                node: port.node,
                region_index: 0,
            },
            InputType::ExitVariableResult {
                branch,
                exit_variable: _,
            } => RegionLocation {
                node: port.node,
                region_index: branch,
            },
        }
    }

    ///Returns the region this `port` resides in.
    pub fn outport_region(&self, port: OutportLocation) -> RegionLocation {
        match port.output {
            //Those simply map to the first (and only) region of the port's node.
            // NOTE: Only Gamma-nodes have none-one region-count. In that case, it must be a Entry/Exit variable though
            OutputType::Argument(_)
            | OutputType::ContextVariableArgument(_)
            | OutputType::RecursionVariableArgument(_) => RegionLocation {
                node: port.node,
                region_index: 0,
            },
            //Is in the given branch of the port's node
            OutputType::EntryVariableArgument {
                branch,
                entry_variable: _,
            } => RegionLocation {
                node: port.node,
                region_index: branch,
            },
            //All of the _output-likes_ reside in the parent region of that node
            OutputType::Output(_)
            | OutputType::DeltaDeclaration
            | OutputType::ExitVariableOutput(_)
            | OutputType::LambdaDeclaration
            | OutputType::RecursionVariableOutput(_) => self[port.node].parent.unwrap(),
        }
    }
}
