/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use rvsdg::{NodeRef, SmallColl};
use vola_common::{error_reporter, report, Span};

use crate::{
    graph::BackendOp,
    hl::HlOp,
    spv::{ArithBaseTy, ArithTy, CoreOp, GlOp, SpvOp, SpvType},
    BackendSpirvError, SpirvBackend,
};

impl SpirvBackend {
    pub fn hl_to_spv_nodes(&mut self) -> Result<(), BackendSpirvError> {
        //collect all hl-nodes
        let hl_nodes = self
            .graph
            .nodes()
            .filter(|n| {
                if self.graph.node(*n).node_type.is_simple() {
                    if self
                        .graph
                        .node(*n)
                        .node_type
                        .unwrap_simple_ref()
                        .op
                        .is_hlop()
                    {
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            })
            .collect::<Vec<NodeRef>>();

        //now rewrite each hl-node, based on their input/output requirements
        for hlnode in hl_nodes {
            let return_type = self.get_single_node_result_type(hlnode).unwrap();
            let input_types = self.get_node_input_types(hlnode);

            //Now let the node dispatch itself based on the types
            self.graph
                .node_mut(hlnode)
                .node_type
                .unwrap_simple_mut()
                .op
                .dispatch(input_types, return_type)?;
        }

        Ok(())
    }
}

impl BackendOp {
    pub fn dispatch(
        &mut self,
        input_types: SmallColl<Option<SpvType>>,
        return_type: SpvType,
    ) -> Result<(), BackendSpirvError> {
        let hlop = if let Self::HlOp(hl) = self {
            hl.clone()
        } else {
            return Err(BackendSpirvError::Any {
                text: format!("non-high-level op found while dispatching!"),
            });
        };

        let mut basetype_signature: SmallColl<ArithBaseTy> = SmallColl::new();
        for t in input_types.clone() {
            if let Some(t) = t {
                match t {
                    SpvType::Arith(a) => basetype_signature.push(a.base),
                    e => {
                        return Err(BackendSpirvError::Any {
                            text: format!("HL-Op {hlop:?} had non-arithmetic input type {e:?}"),
                        });
                    }
                }
            } else {
                return Err(BackendSpirvError::Any {
                    text: format!("HL-Op {hlop:?} had un-type input"),
                });
            }
        }

        let spvop = match (hlop.clone(), basetype_signature.as_slice()) {
            (HlOp::Negate, [ArithBaseTy::Float]) => SpvOp::CoreOp(CoreOp::FNegate),
            (HlOp::Negate, [ArithBaseTy::Integer { signed: true }]) => {
                SpvOp::CoreOp(CoreOp::SNegate)
            }

            (HlOp::Add, [ArithBaseTy::Float, ArithBaseTy::Float]) => SpvOp::CoreOp(CoreOp::FAdd),
            (HlOp::Add, [ArithBaseTy::Integer { .. }, ArithBaseTy::Integer { .. }]) => {
                SpvOp::CoreOp(CoreOp::IAdd)
            }

            (HlOp::Sub, [ArithBaseTy::Float, ArithBaseTy::Float]) => SpvOp::CoreOp(CoreOp::FSub),
            (HlOp::Sub, [ArithBaseTy::Integer { .. }, ArithBaseTy::Integer { .. }]) => {
                SpvOp::CoreOp(CoreOp::ISub)
            }

            (HlOp::Mul, [ArithBaseTy::Float, ArithBaseTy::Float]) => SpvOp::CoreOp(CoreOp::FMul),
            (HlOp::Mul, [ArithBaseTy::Integer { .. }, ArithBaseTy::Integer { .. }]) => {
                SpvOp::CoreOp(CoreOp::IMul)
            }

            (HlOp::Div, [ArithBaseTy::Float, ArithBaseTy::Float]) => SpvOp::CoreOp(CoreOp::FDiv),
            (
                HlOp::Div,
                [ArithBaseTy::Integer { signed: true }, ArithBaseTy::Integer { signed: true }],
            ) => SpvOp::CoreOp(CoreOp::SDiv),
            (
                HlOp::Div,
                [ArithBaseTy::Integer { signed: false }, ArithBaseTy::Integer { signed: false }],
            ) => SpvOp::CoreOp(CoreOp::UDiv),

            (HlOp::Mod, [ArithBaseTy::Float, ArithBaseTy::Float]) => SpvOp::CoreOp(CoreOp::FMod),
            (
                HlOp::Mod,
                [ArithBaseTy::Integer { signed: true }, ArithBaseTy::Integer { signed: true }],
            ) => SpvOp::CoreOp(CoreOp::SMod),
            (
                HlOp::Mod,
                [ArithBaseTy::Integer { signed: false }, ArithBaseTy::Integer { signed: false }],
            ) => SpvOp::CoreOp(CoreOp::UMod),

            //Comparators
            (HlOp::Lt, [ArithBaseTy::Float, ArithBaseTy::Float]) => {
                SpvOp::CoreOp(CoreOp::FOrdLessThan)
            }
            (
                HlOp::Lt,
                [ArithBaseTy::Integer { signed: true }, ArithBaseTy::Integer { signed: true }],
            ) => SpvOp::CoreOp(CoreOp::SLessThan),
            (
                HlOp::Lt,
                [ArithBaseTy::Integer { signed: false }, ArithBaseTy::Integer { signed: false }],
            ) => SpvOp::CoreOp(CoreOp::ULessThan),

            (HlOp::Gt, [ArithBaseTy::Float, ArithBaseTy::Float]) => {
                SpvOp::CoreOp(CoreOp::FOrdGreaterThan)
            }
            (
                HlOp::Gt,
                [ArithBaseTy::Integer { signed: true }, ArithBaseTy::Integer { signed: true }],
            ) => SpvOp::CoreOp(CoreOp::SGreaterThan),
            (
                HlOp::Gt,
                [ArithBaseTy::Integer { signed: false }, ArithBaseTy::Integer { signed: false }],
            ) => SpvOp::CoreOp(CoreOp::UGreaterThan),

            (HlOp::Lte, [ArithBaseTy::Float, ArithBaseTy::Float]) => {
                SpvOp::CoreOp(CoreOp::FOrdLessThanEqual)
            }
            (
                HlOp::Lte,
                [ArithBaseTy::Integer { signed: true }, ArithBaseTy::Integer { signed: true }],
            ) => SpvOp::CoreOp(CoreOp::SLessThanEqual),
            (
                HlOp::Lte,
                [ArithBaseTy::Integer { signed: false }, ArithBaseTy::Integer { signed: false }],
            ) => SpvOp::CoreOp(CoreOp::ULessThanEqual),

            (HlOp::Gte, [ArithBaseTy::Float, ArithBaseTy::Float]) => {
                SpvOp::CoreOp(CoreOp::FOrdGreaterThanEqual)
            }
            (
                HlOp::Gte,
                [ArithBaseTy::Integer { signed: true }, ArithBaseTy::Integer { signed: true }],
            ) => SpvOp::CoreOp(CoreOp::SGreaterThanEqual),
            (
                HlOp::Gte,
                [ArithBaseTy::Integer { signed: false }, ArithBaseTy::Integer { signed: false }],
            ) => SpvOp::CoreOp(CoreOp::UGreaterThanEqual),

            (HlOp::Eq, [ArithBaseTy::Float, ArithBaseTy::Float]) => {
                SpvOp::CoreOp(CoreOp::FOrdEqual)
            }
            (HlOp::Eq, [ArithBaseTy::Integer { .. }, ArithBaseTy::Integer { .. }]) => {
                SpvOp::CoreOp(CoreOp::IEqual)
            }

            (HlOp::Neq, [ArithBaseTy::Float, ArithBaseTy::Float]) => {
                SpvOp::CoreOp(CoreOp::FOrdNotEqual)
            }
            (
                HlOp::Neq,
                [ArithBaseTy::Integer { signed: true }, ArithBaseTy::Integer { signed: true }],
            ) => SpvOp::CoreOp(CoreOp::INotEqual),

            //Glsl dialect operations
            (HlOp::Min, [ArithBaseTy::Float, ArithBaseTy::Float]) => SpvOp::GlslOp(GlOp::FMin),
            (
                HlOp::Min,
                [ArithBaseTy::Integer { signed: true }, ArithBaseTy::Integer { signed: true }],
            ) => SpvOp::GlslOp(GlOp::SMin),
            (
                HlOp::Min,
                [ArithBaseTy::Integer { signed: false }, ArithBaseTy::Integer { signed: false }],
            ) => SpvOp::GlslOp(GlOp::UMin),

            (HlOp::Max, [ArithBaseTy::Float, ArithBaseTy::Float]) => SpvOp::GlslOp(GlOp::FMax),
            (
                HlOp::Max,
                [ArithBaseTy::Integer { signed: true }, ArithBaseTy::Integer { signed: true }],
            ) => SpvOp::GlslOp(GlOp::SMax),
            (
                HlOp::Max,
                [ArithBaseTy::Integer { signed: false }, ArithBaseTy::Integer { signed: false }],
            ) => SpvOp::GlslOp(GlOp::UMax),

            (HlOp::Mix, [ArithBaseTy::Float, ArithBaseTy::Float, ArithBaseTy::Float]) => {
                SpvOp::GlslOp(GlOp::FMix)
            }
            (
                HlOp::Mix,
                [ArithBaseTy::Integer { .. }, ArithBaseTy::Integer { .. }, ArithBaseTy::Integer { .. }],
            ) => SpvOp::GlslOp(GlOp::IMix),

            (HlOp::Clamp, [ArithBaseTy::Float, ArithBaseTy::Float, ArithBaseTy::Float]) => {
                SpvOp::GlslOp(GlOp::FClamp)
            }
            (
                HlOp::Clamp,
                [ArithBaseTy::Integer { signed: true }, ArithBaseTy::Integer { signed: true }, ArithBaseTy::Integer { signed: true }],
            ) => SpvOp::GlslOp(GlOp::SClamp),
            (
                HlOp::Clamp,
                [ArithBaseTy::Integer { signed: false }, ArithBaseTy::Integer { signed: false }, ArithBaseTy::Integer { signed: false }],
            ) => SpvOp::GlslOp(GlOp::UClamp),

            (HlOp::Abs, [ArithBaseTy::Float]) => SpvOp::GlslOp(GlOp::FAbs),
            (HlOp::Abs, [ArithBaseTy::Integer { signed: true }]) => SpvOp::GlslOp(GlOp::SAbs),
            (HlOp::TypeCast, [_source_type]) => {
                //For casts, make sure we are on a scalar or vector

                match (input_types[0].as_ref().cloned().unwrap(), return_type) {
                    (
                        SpvType::Arith(ArithTy {
                            base: src_base,
                            shape: src_shape,
                            resolution: src_resolution,
                        }),
                        SpvType::Arith(ArithTy {
                            base: dst_base,
                            shape: dst_shape,
                            resolution: dst_resolution,
                        }),
                    ) => {
                        if src_resolution != dst_resolution {
                            let err = BackendSpirvError::Any {
                                text: format!("Can not cast types with different resolutions: {src_resolution} != {dst_resolution}"),
                            };

                            report(
                                error_reporter(err.clone(), Span::empty())
                                    .with_note("This is probably a compiler bug!")
                                    .finish(),
                            );
                            return Err(err);
                        }

                        if src_shape != dst_shape {
                            let err = BackendSpirvError::Any {
                                text: format!("Can not cast types with different shapes: {src_shape} != {dst_shape}"),
                            };

                            report(
                                error_reporter(err.clone(), Span::empty())
                                    .with_note("This is probably a compiler bug!")
                                    .finish(),
                            );
                            return Err(err);
                        }

                        if !src_shape.is_vector() && !src_shape.is_scalar() {
                            let err = BackendSpirvError::Any {
                                text: format!(
                                    "Can only cast vectors and scalars in SPIR-V, is: {src_shape}"
                                ),
                            };

                            report(
                                error_reporter(err.clone(), Span::empty())
                                    .with_note("This is probably a compiler bug!")
                                    .finish(),
                            );
                            return Err(err);
                        }

                        //shapes and resolution match, emit the convert op based on base types
                        match (src_base, dst_base) {
                            //Float <- -> Int
                            (ArithBaseTy::Float, ArithBaseTy::Integer { signed: true }) => {
                                SpvOp::CoreOp(CoreOp::ConvertFToS)
                            }
                            (ArithBaseTy::Float, ArithBaseTy::Integer { signed: false }) => {
                                SpvOp::CoreOp(CoreOp::ConvertFToU)
                            }
                            (ArithBaseTy::Integer { signed: true }, ArithBaseTy::Float) => {
                                SpvOp::CoreOp(CoreOp::ConvertSToF)
                            }
                            (ArithBaseTy::Integer { signed: false }, ArithBaseTy::Float) => {
                                SpvOp::CoreOp(CoreOp::ConvertUToF)
                            }

                            //Int <- -> Uint
                            (
                                ArithBaseTy::Integer { signed: true },
                                ArithBaseTy::Integer { signed: false },
                            ) => SpvOp::CoreOp(CoreOp::SatConvertSToU),
                            (
                                ArithBaseTy::Integer { signed: false },
                                ArithBaseTy::Integer { signed: true },
                            ) => SpvOp::CoreOp(CoreOp::SatConvertUToS),
                            (l, r) => {
                                let err = BackendSpirvError::Any {
                                    text: format!("Can not convert base-type {l} to {r} in SPIR-V"),
                                };

                                report(
                                    error_reporter(err.clone(), Span::empty())
                                        .with_note("This is probably a compiler bug!")
                                        .finish(),
                                );
                                return Err(err);
                            }
                        }
                    }
                    (errin, errreturn) => {
                        let err = BackendSpirvError::Any {
                            text: format!("Can not convert {errin} to {errreturn} in SPRIV "),
                        };

                        report(
                            error_reporter(err.clone(), Span::empty())
                                .with_note("This is probably a compiler bug!")
                                .finish(),
                        );
                        return Err(err);
                    }
                }
            }
            _ => {
                let err = BackendSpirvError::Any {
                    text: format!(
                        "Could not dispatch highlevel op {hlop:?} with base-type signature {:?}",
                        basetype_signature
                    ),
                };

                report(
                    error_reporter(err.clone(), Span::empty())
                        .with_note("This is probably a compiler bug!")
                        .finish(),
                );
                return Err(err);
            }
        };

        //Now mutate our selfs
        *self = Self::SpirvOp(spvop);
        Ok(())
    }
}
