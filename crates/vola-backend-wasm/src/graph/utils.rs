/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use rvsdg::{
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    NodeRef, SmallColl,
};

use crate::{WasmBackend, WasmError};

use super::{WasmEdge, WasmTy};

impl WasmBackend {
    //If the port has a value edge connected, returns the type
    pub fn inport_type(&self, port: InportLocation) -> Option<WasmTy> {
        if let Some(edg) = self.graph[port].edge {
            if let WasmEdge::Value(v) = &self.graph[edg].ty {
                Some(v.clone())
            } else {
                None
            }
        } else {
            None
        }
    }

    ///If any value edge is connected (and all connection have the same type), returns the
    /// unified type
    pub fn outport_type(&self, port: OutportLocation) -> Option<WasmTy> {
        let mut unified_type = None;
        for edg in &self.graph[port].edges {
            if let WasmEdge::Value(v) = &self.graph[*edg].ty {
                if let Some(unified) = &unified_type {
                    //The none unifed case
                    if unified != v {
                        return None;
                    }
                    //otherwise, its the same type, so we can use that
                } else {
                    //not yet unified, but has set edge, use that type as unification type
                    unified_type = Some(v.clone());
                }
            } else {
                //Always return none if any state edge is connected!
                return None;
            }
        }

        unified_type
    }

    ///Returns the output signature of this λ-Node. Whenever a result is not connected, `None` is inserted. If a edge is not typed, or a state edeg, the `Undefined` type is used.
    pub fn output_signature(
        &self,
        lambda: NodeRef,
    ) -> Result<SmallColl<Option<WasmTy>>, WasmError> {
        if !self.graph[lambda].node_type.is_lambda() {
            return Err(WasmError::UnsupportedNode(format!("expected λ")));
        }

        let result_count = self.graph[lambda]
            .node_type
            .unwrap_lambda_ref()
            .result_count();

        let sig = (0..result_count)
            .map(|residx| {
                self.graph[lambda.as_inport_location(InputType::Result(residx))]
                    .edge
                    .clone()
            })
            .map(|edg| {
                if let Some(edg) = edg {
                    Some(self.graph[edg].ty.type_or_undefined())
                } else {
                    None
                }
            })
            .collect::<SmallColl<_>>();

        Ok(sig)
    }

    ///Returns the input signature of this λ-Node. Whenever a result is not connected, `None` is inserted. If a edge is not typed, or a state edeg, the `Undefined` type is used.
    ///
    /// If there are multiple, differently typed edges, `Undefined` is used as well.
    pub fn input_signature(&self, lambda: NodeRef) -> Result<SmallColl<Option<WasmTy>>, WasmError> {
        if !self.graph[lambda].node_type.is_lambda() {
            return Err(WasmError::UnsupportedNode(format!("expected λ")));
        }

        let arg_count = self.graph[lambda]
            .node_type
            .unwrap_lambda_ref()
            .argument_count();

        let sig = (0..arg_count)
            .map(|argidx| {
                self.graph[lambda.as_outport_location(OutputType::Argument(argidx))]
                    .edges
                    .clone()
            })
            .map(|edg| {
                let unified_type = edg
                    .into_iter()
                    .map(|edg| self.graph[edg].ty.type_or_undefined())
                    .fold(None, |tystate, this| {
                        if let Some(tystate) = tystate {
                            //one set, try to unify
                            if tystate == this {
                                Some(this)
                            } else {
                                Some(WasmTy::Undefined)
                            }
                        } else {
                            //None set yet, just use this one
                            Some(this)
                        }
                    });

                unified_type
            })
            .collect::<SmallColl<_>>();

        Ok(sig)
    }
}
