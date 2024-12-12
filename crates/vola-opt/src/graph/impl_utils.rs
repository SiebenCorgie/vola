/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use rvsdg::{
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    region::RegionLocation,
    util::unroll::UnrollError,
    NodeRef, SmallColl,
};
use vola_ast::csg::{CsgDef, CsgTy};
use vola_common::Span;

use crate::{
    alge::EvalNode, common::Ty, graph::auxiliary::Impl, imm::ImmNat, OptEdge, OptError, OptNode,
    Optimizer,
};

use super::auxiliary::ImplKey;

impl Optimizer {
    ///Builds a single-operand identity implementation for the given concept-operation compination.
    ///
    /// Fails if either the combination is already implemented, or the key contains a entity definition instead of a
    /// operation definition.
    ///
    /// Fails if the operation definition is tagged `#[no_identity]`
    pub fn implement_identity_for_concept(&mut self, key: ImplKey) -> Result<(), OptError> {
        if self.concept_impl.contains_key(&key) {
            return Err(OptError::AIIFailed(format!(
                "There was already a implementation present for {}::{}",
                key.concept, key.node,
            )));
        }

        match self.csg_node_defs.get(&key.node) {
            Some(CsgDef {
                span: _,
                ty: CsgTy::Entity,
                name,
                args: _,
            }) => {
                return Err(OptError::AIIFailed(format!(
                    "{} is defined as an entity, not an operation!",
                    name.0
                )))
            }
            Some(CsgDef {
                span: _,
                ty: CsgTy::Operation,
                name: _,
                args: _,
            }) => {}
            None => {
                return Err(OptError::AIIFailed(format!(
                    "Could not find operation definition named \"{}\", consider adding it first!",
                    key.node
                )))
            }
        }

        let concept_return_type = if let Some(concept) = self.concepts.get(&key.concept) {
            concept.dst_ty.clone()
        } else {
            return Err(OptError::AIIFailed(format!(
                "Concept \"{}\" doesn't exist",
                key.concept
            )));
        };

        //at this point we are sure that there is no implementation yet, and the csg-node-def is in fact an operation definition.
        //So lets add this ultra-simple λ to the graph. The definition is, that we add one CV, that is mapped to the
        // single eval in the λ's body, we then append all the arguments of the operation, after that all the args of the
        // concept and finally we route the arguments of the concept directly to the eval.

        //Create the lambda node. NOTE that we currently alway export. For debug purposes.
        let (lmd, lmd_region) = self.graph.on_omega_node(|omg| {
            let export_impl_block = std::env::var("VOLA_EXPORT_ALL").is_ok();
            omg.new_function(export_impl_block, |lmd_builder| {
                lmd_builder.on_region(|reg| reg.parent_location())
            })
        });

        let csg_operand = self
            .graph
            .node_mut(lmd)
            .node_type
            .unwrap_lambda_mut()
            .add_context_variable();
        assert!(csg_operand == 0);
        //Add the CSG-Arg
        self.typemap.set(
            OutportLocation {
                node: lmd,
                output: OutputType::ContextVariableArgument(csg_operand),
            }
            .into(),
            Ty::CSG,
        );

        let mut args = SmallColl::new();
        //Add the _unused_ csg-args
        //NOTE: Not really needed, but its the convention, and we don't know at this point if any
        //      pass depends on the convention.
        for csg_aty in self.csg_node_defs.get(&key.node).unwrap().args.iter() {
            //add all the ports and type them
            let argidx = self
                .graph
                .node_mut(lmd)
                .node_type
                .unwrap_lambda_mut()
                .add_argument();
            self.typemap.set(
                OutportLocation {
                    node: lmd,
                    output: OutputType::Argument(argidx),
                }
                .into(),
                csg_aty.ty.clone().into(),
            );
            args.push((csg_aty.ident.0.clone(), csg_aty.ty.clone().into()));
        }

        //Add all the concept arg
        let mut eval_arg_ports = SmallColl::new();
        let src_ty = self.concepts.get(&key.concept).unwrap().src_ty.clone();
        args.push(("unnamed-concept-arg".to_owned(), src_ty.clone().into()));

        let argidx = self
            .graph
            .node_mut(lmd)
            .node_type
            .unwrap_lambda_mut()
            .add_argument();
        let port = OutportLocation {
            node: lmd,
            output: OutputType::Argument(argidx),
        };
        self.typemap.set(port.clone().into(), src_ty.clone().into());
        eval_arg_ports.push((port, src_ty.clone().into()));

        //add the result port
        let res = self
            .graph
            .node_mut(lmd)
            .node_type
            .unwrap_lambda_mut()
            .add_result();
        self.typemap.set(
            InportLocation {
                node: lmd,
                input: InputType::Result(res),
            }
            .into(),
            concept_return_type.clone().into(),
        );
        //add the eval node to the body, then connect all arguments
        self.graph.on_region(&lmd_region, |reg| {
            let eval_node = reg.insert_node(OptNode::new(
                EvalNode::new(eval_arg_ports.len() + 1, key.concept.clone()),
                Span::empty(),
            ));
            //connect subtree to first arg
            let _csg_edge = reg
                .ctx_mut()
                .connect(
                    OutportLocation {
                        node: lmd,
                        output: OutputType::ContextVariableArgument(0),
                    },
                    InportLocation {
                        node: eval_node,
                        input: InputType::Input(0),
                    },
                    OptEdge::value_edge().with_type(Ty::CSG),
                )
                .unwrap();
            //connect the arg-edges and type them
            for (idx, (argport, ty)) in eval_arg_ports.into_iter().enumerate() {
                let _edg = reg
                    .ctx_mut()
                    .connect(
                        argport,
                        InportLocation {
                            node: eval_node,
                            input: InputType::Input(idx + 1),
                        },
                        OptEdge::value_edge().with_type(ty),
                    )
                    .unwrap();
            }

            //connect to result and type it
            let _csg_result = reg
                .ctx_mut()
                .connect(
                    OutportLocation {
                        node: eval_node,
                        output: OutputType::Output(0),
                    },
                    InportLocation {
                        node: lmd,
                        input: InputType::Result(0),
                    },
                    OptEdge::value_edge().with_type(concept_return_type.clone().into()),
                )
                .unwrap();
        });

        //last part is tagging it with a name and adding it to the collection of impl blocks
        self.names.set(
            lmd.into(),
            format!("AII impl {} for {}", key.node, key.concept),
        );

        let mut operands = SmallColl::default();
        operands.push("unnamed".to_string());

        let concept_impl = Impl {
            region_span: Span::empty(),
            def_span: Span::empty(),
            concept: key.concept.clone(),
            subtrees: operands,
            lambda: lmd,
            args,
            return_type: concept_return_type.into(),
        };

        let old = self.concept_impl.insert(key, concept_impl);
        assert!(old.is_none());

        Ok(())
    }

    ///Copies all input connections of `src` to `dst`. This means
    /// for any input `I` of node `src` with an edge `E` from `src`'s output `O` an new connection from `src`'s output `O` to `dst`'s input `I` of the same type as `E` is made.
    ///
    /// Panics if the input signature of `src` is not a subset of `dst`.
    ///
    /// Panics if the node-type of both nodes does not match
    ///
    /// Meaning if src is a SimpleNode with 3 inputs, and dst a SimpleNode with 4 inputs, this works, but not the other way around.
    pub fn copy_input_connections(&mut self, src: NodeRef, dst: NodeRef) {
        assert!(self.graph[src].into_abstract() == self.graph[dst].into_abstract());

        for input in self.graph[src].inport_types() {
            if let Some(edg) = self.graph[InportLocation { node: src, input }].edge.clone() {
                let ty = self.graph[edg].ty.clone();
                let src = self.graph[edg].src().clone();

                self.graph
                    .connect(src, InportLocation { node: dst, input }, ty)
                    .unwrap();
            }
        }
    }

    ///Analyses the `theta` node loop bound. Returns an error if this is not a theta node.
    pub fn loop_count(&self, theta: NodeRef) -> Result<usize, UnrollError> {
        if !self.graph[theta].node_type.is_theta() {
            return Err(UnrollError::NotThetaNode);
        }

        //NOTE: This used to be a analysis from the criterion port
        //      However, by definition we have the first input to the theta-node
        //      as lower bound, and the second as higher bound, so we use those now.

        let lower_src = self
            .graph
            .find_producer_inp(theta.as_inport_location(InputType::Input(0)));
        let higher_src = self
            .graph
            .find_producer_inp(theta.as_inport_location(InputType::Input(1)));

        if lower_src.is_none() || higher_src.is_none() {
            return Err(UnrollError::NonStaticBounds);
        }

        //println!("Loopcount for {theta} ended at: \n    {low_src:?}\n    {high_src:?}");

        //unwrap both bounds and check for natural numbers
        let lower = if let Some(nat) = self.try_unwrap_node::<ImmNat>(lower_src.unwrap().node) {
            nat.lit
        } else {
            return Err(UnrollError::NonStaticBounds);
        };

        let higher = if let Some(nat) = self.try_unwrap_node::<ImmNat>(higher_src.unwrap().node) {
            nat.lit
        } else {
            return Err(UnrollError::NonStaticBounds);
        };

        assert!(lower <= higher);

        Ok((higher - lower) as usize)
    }

    ///Tries to build a single unified type for all branches of `gamma` that make the `exit_variable`'s type.
    ///
    /// Returns None on missmatching types.
    pub(crate) fn gamma_unified_type(&self, gamma: NodeRef, exit_variable: usize) -> Option<Ty> {
        let mut candidate = None;
        for branch in 0..self.graph[gamma].regions().len() {
            if let Some(ty) = self.find_type(
                &gamma
                    .as_inport_location(InputType::ExitVariableResult {
                        branch,
                        exit_variable,
                    })
                    .into(),
            ) {
                if let Some(candidate) = &candidate {
                    if *candidate != ty {
                        //missmatch
                        return None;
                    }
                } else {
                    //no candidate yet, set it
                    candidate = Some(ty);
                }
            }
        }

        candidate
    }

    ///Returns a list of all currently exported λ-bodies
    pub(crate) fn exported_functions(&self) -> SmallColl<RegionLocation> {
        let mut export_lambdas = SmallColl::default();
        for result in self
            .graph
            .result_ports(self.graph.toplevel_region().node, 0)
        {
            if let Some(src) = self.graph.inport_src(result) {
                if self.graph[src.node].node_type.is_lambda() {
                    export_lambdas.push(RegionLocation {
                        node: src.node,
                        region_index: 0,
                    });
                }
            }
        }

        export_lambdas
    }
}
