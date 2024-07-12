/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use crate::nodes::{LangNode, NodeType};

///A contentless [crate::NodeType]. Helps you match nodes based on
///a note-type, without having to borrow the actual node
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AbstractNodeType {
    Apply,
    Delta,
    Gamma,
    Lambda,
    Omega,
    Phi,
    Simple,
    Theta,
}

impl<'a, N: LangNode + 'static> From<&'a NodeType<N>> for AbstractNodeType {
    fn from(value: &'a NodeType<N>) -> Self {
        match value {
            NodeType::Apply(_) => Self::Apply,
            NodeType::Delta(_) => Self::Delta,
            NodeType::Gamma(_) => Self::Gamma,
            NodeType::Lambda(_) => Self::Lambda,
            NodeType::Omega(_) => Self::Omega,
            NodeType::Phi(_) => Self::Phi,
            NodeType::Simple(_) => Self::Simple,
            NodeType::Theta(_) => Self::Theta,
        }
    }
}
