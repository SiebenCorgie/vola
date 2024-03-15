/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use rvsdg::{
    edge::{InportLocation, OutportLocation, OutputType},
    nodes::NodeType,
    smallvec::SmallVec,
    NodeRef,
};

use crate::{common::Ty, Optimizer};

impl Optimizer {
    pub fn get_type_for_inport(&self, port: InportLocation) -> Option<Ty> {
        if let Some(port) = self.graph.node(port.node).inport(&port.input) {
            if let Some(edgref) = port.edge {
                if let Some(ty) = self.graph.edge(edgref).ty.get_type() {
                    //Actually found a type on the edge, so return that
                    return Some(ty.clone());
                }
            }
        }

        //edge seems to have no type, so try to use the port tag
        if let Some([ty]) = self.typemap.attrib(&port.into()) {
            Some(ty.clone())
        } else {
            None
        }
    }

    ///Builds the type signature for a lambda node, based on either the port's type tag, or the type of an connected edge.
    ///
    /// Panics if `node` is not in fact a lambda node.
    pub fn get_lambda_arg_signature(&self, node: NodeRef) -> SmallVec<[Option<Ty>; 3]> {
        let lmd = self.graph.node(node).node_type.unwrap_lambda_ref();
        let mut argidx = 0;
        let mut signature = SmallVec::default();
        while let Some(argport) = lmd.argument(argidx) {
            argidx += 1;

            let mut found_ty = None;
            //First try to get a type for an connected edge, if possible. If not, we try to find a type_tag
            for edge in argport.edges.iter() {
                //try to get a type for that edge, if possible we push that and continue;
                if let Some(ty) = self.graph.edge(*edge).ty.get_type() {
                    if let Some(already_known_type) = found_ty {
                        assert!(
                            ty == &already_known_type,
                            "expected all outgoing edges of a lambda node to be of same type"
                        );
                    }
                    found_ty = Some(ty.clone());
                }
            }

            if let Some(ty) = found_ty {
                signature.push(Some(ty));
                continue;
            }

            //if we didn't find a type yet, try typemap
            match self.typemap.attrib(
                &OutportLocation {
                    node,
                    output: OutputType::Argument(argidx),
                }
                .into(),
            ) {
                Some([ty]) => {
                    //Use that type then
                    signature.push(Some(ty.clone()));
                }
                Some(multiple) => {
                    #[cfg(feature = "log")]
                    log::warn!("outport had multiple types register, using first one.");

                    //Push the first one then
                    signature.push(Some(multiple.get(0).unwrap().clone()));
                }
                None => {
                    //Found none no where
                    signature.push(None);
                }
            }
        }

        signature
    }
}
