/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use crate::{
    edge::{InportLocation, OutportLocation},
    EdgeRef, NodeRef,
};

///Defines all possible locations for a label.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum LabelLoc {
    InPort(InportLocation),
    OutPort(OutportLocation),
    Node(NodeRef),
    Edge(EdgeRef),
    Custom(String),
}
