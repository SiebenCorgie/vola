/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use ahash::AHashMap;
use rvsdg::{
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    util::unroll::UnrollError,
    NodeRef, SmallColl,
};
use vola_ast::{
    common::Ident,
    csg::{CSGNodeDef, CSGNodeTy},
};
use vola_common::Span;

use crate::{
    alge::{
        implblock::{ConceptImpl, ConceptImplKey},
        relational::{BinaryRel, BinaryRelOp},
        EvalNode,
    },
    common::Ty,
    imm::ImmNat,
    OptEdge, OptError, OptNode, Optimizer,
};

impl Optimizer {
    ///Builds a single-operand identity implementation for the given concept-operation compination.
    ///
    /// Fails if either the combination is already implemented, or the key contains a entity definition instead of a
    /// operation definition.
    ///
    /// Fails if the operation definition is tagged `#[no_identity]`
    pub fn implement_identity_for_concept(&mut self, key: ConceptImplKey) -> Result<(), OptError> {
        if self.concept_impl.contains_key(&key) {
            return Err(OptError::AIIFailed(format!(
                "There was already a implementation present for {}::{}",
                key.concept_name, key.node_name,
            )));
        }

        match self.csg_node_defs.get(&key.node_name) {
            Some(CSGNodeDef {
                span: _,
                ty: CSGNodeTy::Entity,
                name,
                args: _,
            }) => {
                return Err(OptError::AIIFailed(format!(
                    "{} is defined as an entity, not an operation!",
                    name.0
                )))
            }
            Some(CSGNodeDef {
                span: _,
                ty: CSGNodeTy::Operation,
                name: _,
                args: _,
            }) => {}
            None => {
                return Err(OptError::AIIFailed(format!(
                    "Could not find operation definition named \"{}\", consider adding it first!",
                    key.node_name
                )))
            }
        }

        let concept_return_type = if let Some(concept) = self.concepts.get(&key.concept_name) {
            concept.dst_ty.clone()
        } else {
            return Err(OptError::AIIFailed(format!(
                "Concept \"{}\" doesn't exist",
                key.concept_name
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
            Ty::CSGTree,
        );

        //Add the _unused_ csg-args
        //NOTE: Not really needed, but its the convention, and we don't know at this point if any
        //      pass depends on the convention.
        for csg_aty in self.csg_node_defs.get(&key.node_name).unwrap().args.iter() {
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
        }

        //Add all the concepts args
        let mut eval_arg_ports = SmallColl::new();
        for con_aty in self.concepts.get(&key.concept_name).unwrap().src_ty.iter() {
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
            self.typemap
                .set(port.clone().into(), con_aty.clone().into());
            eval_arg_ports.push((port, con_aty.clone().into()));
        }

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
                EvalNode::new(eval_arg_ports.len(), key.concept_name.clone()),
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
                    OptEdge::value_edge().with_type(Ty::CSGTree),
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
                    OptEdge::value_edge().with_type(concept_return_type.into()),
                )
                .unwrap();
        });

        //last part is tagging it with a name and adding it to the collection of impl blocks
        self.names.set(
            lmd.into(),
            format!("AII impl {} for {}", key.node_name, key.concept_name),
        );

        let mut operands = AHashMap::default();
        operands.insert("unnamed".to_string(), 0);

        let concept_impl = ConceptImpl {
            span: Span::empty(),
            concept: Ident(key.concept_name.clone()),
            node_type: CSGNodeTy::Operation,
            operands,
            lambda: lmd,
            lambda_region: lmd_region,
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
        let criteria_src = self
            .graph
            .inport_src(theta.as_inport_location(InputType::ThetaPredicate))
            .unwrap();

        let (low_src, high_src) =
            if let Some(lt_arg) = self.try_unwrap_node::<BinaryRel>(criteria_src.node) {
                assert!(lt_arg.op == BinaryRelOp::Lt, "Expected lt as criterion!");
                let low_src = self
                    .graph
                    .find_producer_inp(criteria_src.node.input(0))
                    .expect("Expectede connected src");
                let high_src = self
                    .graph
                    .find_producer_inp(criteria_src.node.input(1))
                    .expect("Expectede connected src");
                (low_src, high_src)
            } else {
                panic!("Theta predicate must be BinaryRel node!");
            };

        //println!("Loopcount for {theta} ended at: \n    {low_src:?}\n    {high_src:?}");
        //now _assume_ both are nats
        let lower = self.try_unwrap_node::<ImmNat>(low_src.node).unwrap().lit as usize;
        let higher = self.try_unwrap_node::<ImmNat>(high_src.node).unwrap().lit as usize;
        assert!(lower <= higher);

        Ok(higher - lower)
    }
}
