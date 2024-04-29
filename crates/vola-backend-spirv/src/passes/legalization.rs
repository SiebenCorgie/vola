/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Walks the current graph, and checks if the nodes and their connection statisfy the SPIR-V spec.
//!
//! Tries to repair any _recoverable_ errors, like forgetting to store some value in a var, before mutating.
//! Might error out if no known _repair_ strategy is known for some error in the graph.

use rvsdg::{nodes::NodeType, NodeRef, SmallColl};
use spirv_grammar_rules::GrammarRules;

use crate::{
    graph::{BackendEdge, BackendOp},
    spv::{ArithTy, CoreOp, SpvNode, SpvOp, SpvType, TyShape},
    BackendSpirvError, SpirvBackend,
};

impl SpirvBackend {
    pub fn legalize(&mut self) -> Result<(), BackendSpirvError> {
        //NOTE: In practice _legalize_ is a set of passes.
        //      We start out by legalizing all nodes based on the SPIR-V grammar. That catches
        //      simple misstakes like operand-count-missmatch, wrong operands etc.
        //
        //      We then use increasingly specialized passes to check for things like type-matching,
        //      and common repairable patterns.
        self.legalize_grammar()?;

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("VOLA_SPIRV_LEGALIZE").is_ok() {
            self.push_debug_state("SPIR-V Legalized");
        }
        Ok(())
    }

    ///Takes care of mapping FMul operations to the actual instruction in case the two
    ///inputs are not in fact the same type.
    fn transmute_multiply(
        &mut self,
        node: NodeRef,
        inputs: &[SpvType],
        output: &SpvType,
    ) -> Result<(), BackendSpirvError> {
        assert!(
            self.graph
                .node(node)
                .node_type
                .unwrap_simple_ref()
                .op
                .unwrap_spv_ref()
                .op
                == SpvOp::CoreOp(CoreOp::FMul)
        );

        //Now inspect the inputs, and possibly mutate self, and the output type to match
        //the actual instruction

        if inputs.len() != 2 {
            return Err(BackendSpirvError::SpvLegalizationMalformed {
                inst: "FMul".to_owned(),
                text: format!("Expected Mul to have 2 operands, but had {}", inputs.len()),
            });
        }

        let input0_is_matrix = if let SpvType::Arith(aty) = &inputs[0] {
            aty.shape.is_matrix()
        } else {
            false
        };

        //NOTE: matrix check to rewrite Matrix-Matrix op accordingly.
        if inputs[0] == inputs[1] && !input0_is_matrix {
            //for sanity
            assert!(output == &inputs[0]);
            //already _okay_
            return Ok(());
        }

        match (&inputs[0], &inputs[1], output) {
            (
                SpvType::Arith(ArithTy {
                    base: base_a,
                    shape: shape_a,
                    resolution: res_a,
                }),
                SpvType::Arith(ArithTy {
                    base: base_b,
                    shape: shape_b,
                    resolution: res_b,
                }),
                SpvType::Arith(ArithTy {
                    base: res_base,
                    shape: res_shape,
                    resolution: res_res,
                }),
            ) => {
                if res_a != res_b || res_a != res_res {
                    return Err(BackendSpirvError::SpvLegalizationMalformed {
                        inst: "FMul".to_owned(),
                        text: format!("base type resolution does not match: {res_a} != {res_b}"),
                    });
                }

                if base_a != base_b || base_a != res_base {
                    return Err(BackendSpirvError::SpvLegalizationMalformed {
                        inst: "FMul".to_owned(),
                        text: format!("base type does not match: {base_a} != {base_b}"),
                    });
                }

                match (shape_a, shape_b, res_shape){
                    //Morph into Vec x Scalar, iff resolution matches and base type is the same.
                    (TyShape::Vector { width }, TyShape::Scalar, TyShape::Vector { width: res_width }) => {
                        if width != res_width{
                            return Err(BackendSpirvError::SpvLegalizationMalformed {
                                inst: "VectorTimesScalar".to_owned(), 
                                text: format!("input/output vector width doesn't match: {width} != {res_width}") 
                            });
                        }
                        self.graph.node_mut(node).node_type.unwrap_simple_mut().op = BackendOp::SpirvOp(SpvNode { op: SpvOp::CoreOp(CoreOp::VectorTimesScalar) });
                        Ok(())
                    },
                    (TyShape::Matrix { width, height }, TyShape::Scalar, TyShape::Matrix { width: res_width, height: res_height }) => {
                        if width != res_width{
                            return Err(BackendSpirvError::SpvLegalizationMalformed {
                                inst: "MatrixTimesScalar".to_owned(),
                                text: format!("input/output matrix width doesn't match: {width} != {res_width}")
                            });
                        }
                        if height != res_height{
                            return Err(BackendSpirvError::SpvLegalizationMalformed {
                                inst: "MatrixTimesScalar".to_owned(),
                                text: format!("input/output matrix width doesn't match: {height} != {res_height}")
                            });
                        }
                        self.graph.node_mut(node).node_type.unwrap_simple_mut().op = BackendOp::SpirvOp(SpvNode { op: SpvOp::CoreOp(CoreOp::MatrixTimesScalar) });
                        Ok(())
                    },
                    (TyShape::Vector { width }, TyShape::Matrix { width: _, height }, TyShape::Vector { width: res_width }) =>{
                        if width != height{
                            return Err(BackendSpirvError::SpvLegalizationMalformed {
                                inst: "FMul".to_owned(),
                                text: format!("Vector-Matrix multiplication assumes that the vector width is the same as the vector height. But vec-width={width} != mat-height={height}")
                            });
                        }
                        if width != res_width{
                            return Err(BackendSpirvError::SpvLegalizationMalformed {
                                inst: "VectorTimesMatrix".to_owned(), 
                                text: format!("input/output vector width doesn't match: {width} != {res_width}")
                            });
                        }
                        self.graph.node_mut(node).node_type.unwrap_simple_mut().op = BackendOp::SpirvOp(SpvNode { op: SpvOp::CoreOp(CoreOp::VectorTimesMatrix) });
                        Ok(())
                    },
                    (TyShape::Matrix { width: _, height }, TyShape::Vector { width }, TyShape::Vector { width: res_width }) =>{
                        if width != height{
                            return Err(BackendSpirvError::SpvLegalizationMalformed {
                                inst: "FMul".to_owned(),
                                text: format!("Matrix-Vector multiplication assumes that the vector width is the same as the vector height. But vec-width={width} != mat-height={height}")
                            });
                        }
                        if width != res_width{
                            return Err(BackendSpirvError::SpvLegalizationMalformed {
                                inst: "MatrixTimesVector".to_owned(),
                                text: format!("input/output vector width doesn't match: {width} != {res_width}")
                            });
                        }
                        self.graph.node_mut(node).node_type.unwrap_simple_mut().op = BackendOp::SpirvOp(SpvNode { op: SpvOp::CoreOp(CoreOp::MatrixTimesVector) });
                        Ok(())
                    }
                    (TyShape::Matrix { width, height }, TyShape::Matrix { width: wb, height: hb }, TyShape::Matrix { width: res_width, height: res_height }) => {
                        if width != res_width{
                            return Err(BackendSpirvError::SpvLegalizationMalformed {
                                inst: "MatrixTimesMatrix".to_owned(),
                                text: format!("input/output matrix width doesn't match: {width} != {res_width}")
                            });
                        }
                        if height != res_height{
                            return Err(BackendSpirvError::SpvLegalizationMalformed {
                                inst: "MatrixTimesMatrix".to_owned(),
                                text: format!("input/output matrix height doesn't match: {height} != {res_height}")
                            });
                        }

                        if width != wb{
                            return Err(BackendSpirvError::SpvLegalizationMalformed {
                                inst: "MatrixTimesMatrix".to_owned(),
                                text: format!("matrix width doesn't match: {width} != {wb}")
                            });
                        }
                        if height != hb{
                            return Err(BackendSpirvError::SpvLegalizationMalformed {
                                inst: "MatrixTimesMatrix".to_owned(),
                                text: format!("matrix height doesn't match: {height} != {hb}")
                            });
                        }
                        self.graph.node_mut(node).node_type.unwrap_simple_mut().op = BackendOp::SpirvOp(SpvNode { op: SpvOp::CoreOp(CoreOp::MatrixTimesMatrix) });
                        Ok(())
                    },
                    _ => Err(BackendSpirvError::SpvLegalizationMalformed { inst: "FMul".to_owned(), text: format!("Encountered multiplication of two arithmetic operands, but could not derive the instruction because of unknown pattern:\na = {:#?}\nb = {:#?}", inputs[0], inputs[1]) })
                }
            }
            _ => Err(BackendSpirvError::SpvLegalizationMalformed {
                inst: "FMul".to_owned(),
                text: format!("Mul had none aritmethic operands."),
            }),
        }
    }

    ///Uses the grammar to legalize all operands of a node.
    fn legalize_grammar(&mut self) -> Result<(), BackendSpirvError> {
        //load the grammar-rules of both used grammar (core and glsl).
        //TODO: Use the bundeled ones, but right now we load from file cause thats faster
        let mut core_rules = GrammarRules::load_core_grammar();
        let mut glsl_rules = GrammarRules::load_glsl_std_450_grammar();

        //NOTE: need to collect upfront, so we can modify the graph in the loop :/
        let reachable = self.graph.walk_reachable().collect::<Vec<_>>();
        for node in reachable {
            //Ignore non-simple nodes atm.
            if !self.graph.node(node).node_type.is_simple() {
                continue;
            }

            //collect the input/output signature for the legalize_pattern call.
            let mut inputs = SmallColl::new();

            for inp in self.graph.node(node).inputs().iter() {
                if let Some(inpedg) = inp.edge {
                    let ty = if let BackendEdge::Value(ty) = &self.graph.edge(inpedg).ty {
                        ty.clone()
                    } else {
                        SpvType::State
                    };
                    inputs.push(ty);
                } else {
                    return Err(BackendSpirvError::Any {
                        text: format!(
                            "Found unconnected port on SPIR-V Node {:?}",
                            self.graph.node(node).node_type.unwrap_simple_ref()
                        ),
                    });
                }
            }

            assert!(self.graph.node(node).outputs().len() == 1);
            let mut output = if self.graph.node(node).outputs()[0].edges.len() == 0 {
                SpvType::undefined()
            } else {
                if let BackendEdge::Value(t) = &self
                    .graph
                    .edge(self.graph.node(node).outputs()[0].edges[0])
                    .ty
                {
                    t.clone()
                } else {
                    SpvType::State
                }
            };

            //Apply all rewrites we know of
            if self.is_core_op(node, CoreOp::FMul) {
                self.transmute_multiply(node, &inputs, &mut output)?;
            }

            //now call the legalizer for the spv op
            if let NodeType::Simple(sn) = &mut self.graph.node_mut(node).node_type {
                if let BackendOp::SpirvOp(op) = &mut sn.op {
                    op.legalize_for_pattern(&mut core_rules, &mut glsl_rules, &inputs, &output)?
                }
            }
        }

        Ok(())
    }
}
