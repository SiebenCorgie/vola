/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Defines the SPIR-V dialect nodes for the backend. We use the [rspirv crate's](https://docs.rs/rspirv/latest/rspirv/grammar) grammar specification to
//! work with spir-v ops in the graph.

use std::fmt::Display;

use ahash::AHashMap;
use rspirv::{
    dr::{Instruction, Operand},
    grammar::{LogicalOperand, OperandKind},
    spirv::Word,
};
use rvsdg::smallvec::SmallVec;
use spirv_grammar_rules::{GrammarRules, Rule};
use vola_opt::common::{DataType, Shape};

use crate::{passes::EmitCtx, BackendSpirvError};

pub type CoreOp = rspirv::spirv::Op;

enum OperatorSrc {
    Result,
    Input(usize),
}

fn type_pattern_check_core_op(
    crules: &mut GrammarRules,
    coreop: &CoreOp,
    input_types: &[SpvType],
    output: &SpvType,
) -> Result<(), BackendSpirvError> {
    //TODO: implement all the _case-dependent checks, or emit an warning if we don't know them_
    //TODO: Or find a way to generate those checks from the spec, cause this stuff is pain. but
    //      Looking at [this](https://github.com/KhronosGroup/SPIRV-Tools/blob/6761288d39e2af51d73a5d8edb328dafc2054b1c/source/val/validate_constants.cpp#L47)
    //      thought it'll probably stay like that :((.

    //TODO: I'm rolling my on thing over on [github](https://github.com/SiebenCorgie/spirv-grammar-rules)

    //try to get a rule that matchen the opcode
    //NOTE: Right now the enum matches the one in the spec, if that ever breaks, someone did
    //      something wrong there.
    let opcode = *coreop as u32;

    if let Some(instruction_rules) = crules.lookup_opcode(opcode) {
        if instruction_rules.rules.len() == 0 {
            #[cfg(feature = "log")]
            log::error!("{:?} had no rules associated in grammar-rules!", coreop);
        }

        //NOTE: The core instructionset always starts with [ResultTypeId, ResultId, operands ..],
        //      which is what we'll express here
        let mut operator_mapping = AHashMap::with_capacity(input_types.len() + 1);
        //result mapping
        let _ = operator_mapping.insert(
            instruction_rules.operand_mapping[1].as_str(),
            OperatorSrc::Result,
        );

        //operands
        for (opidx, op) in instruction_rules.operand_mapping[2..].iter().enumerate() {
            let _ = operator_mapping.insert(op, OperatorSrc::Input(opidx));
        }
        //now test all rules with the given mapping
        for rule in &instruction_rules.rules {
            test_rule(&operator_mapping, input_types, output, rule).map_err(|mut e| {
                e.set_opname(format!("{:?}", coreop));
                e
            })?;
        }

        Ok(())
    } else {
        #[cfg(feature = "log")]
        log::error!(
            "{:?} did not exist in grammar-rules {}!",
            coreop,
            crules.source_grammar
        );
        Ok(())
    }
}

pub type GlOp = rspirv::spirv::GLOp;
fn type_pattern_check_gl_op(
    grules: &mut GrammarRules,
    glop: &GlOp,
    input_types: &[SpvType],
    output: &SpvType,
) -> Result<(), BackendSpirvError> {
    let opcode = *glop as u32;

    if let Some(instruction_rules) = grules.lookup_opcode(opcode) {
        if instruction_rules.rules.len() == 0 {
            #[cfg(feature = "log")]
            log::error!("{:?} had no rules associated in grammar-rules!", glop);
        }

        let mut operator_mapping = AHashMap::with_capacity(input_types.len() + 1);
        //in the case of the GLOperator we add _Result_ for the output, since the actual
        //IdResult_1 is set by the OpExtsInst.
        let _ = operator_mapping.insert("Result", OperatorSrc::Result);

        for (opidx, op) in instruction_rules.operand_mapping.iter().enumerate() {
            let _ = operator_mapping.insert(op, OperatorSrc::Input(opidx));
        }

        //now test all rules with the given mapping
        for rule in &instruction_rules.rules {
            test_rule(&operator_mapping, input_types, output, rule).map_err(|mut e| {
                e.set_opname(format!("{:?}", glop));
                e
            })?;
        }

        Ok(())
    } else {
        #[cfg(feature = "log")]
        log::error!(
            "{:?} did not exist in grammar-rules {}!",
            glop,
            grules.source_grammar
        );
        Ok(())
    }
}

///Tests the `rule` for a given operand_mapping on `input_types` and `output`.
fn test_rule<'a>(
    operand_mapping: &AHashMap<&'a str, OperatorSrc>,
    input_types: &[SpvType],
    output: &SpvType,
    rule: &Rule,
) -> Result<(), BackendSpirvError> {
    match rule {
        Rule::BaseType {
            operand,
            base_types,
        } => {
            let ty_src = match operand_mapping
                .get((*operand).as_str())
                .expect("Operand was not in mapping")
            {
                OperatorSrc::Result => output,
                OperatorSrc::Input(idx) => &input_types[*idx],
            };

            if let Some(basetype) = ty_src.base_type() {
                if base_types.contains(&basetype) {
                    Ok(())
                } else {
                    Err(BackendSpirvError::SpvLegalizationRuleFailed {
                        inst: String::with_capacity(0),
                        rule: rule.clone(),
                    })
                }
            } else {
                Err(BackendSpirvError::SpvLegalizationMalformed {
                    inst: String::with_capacity(0),
                    text: format!(
                        "Rule checks for base type, but operand {} of type {:?} had no base-type",
                        operand, ty_src
                    ),
                })
            }
        }
        Rule::TypeConstraint { operand, ty } => {
            let ty_src = match operand_mapping
                .get((*operand).as_str())
                .expect("Operand was not in mapping")
            {
                OperatorSrc::Result => output,
                OperatorSrc::Input(idx) => &input_types[*idx],
            };

            if let Some(rule_ty_str) = ty_src.as_rule_ty() {
                for t in ty.iter() {
                    if *t == rule_ty_str {
                        return Ok(());
                    }
                }

                Err(BackendSpirvError::SpvLegalizationRuleFailed {
                    inst: String::with_capacity(0),
                    rule: rule.clone(),
                })
            } else {
                Err(BackendSpirvError::SpvLegalizationMalformed { inst: String::with_capacity(0), text: format!("Operand {} of type {:?} has no type, that could be expressed as a SPIR-V TypeConstrain rule.", operand, ty_src) })
            }
        }
        Rule::ResultEqualType(src) => {
            let ty_src = match operand_mapping
                .get((*src).as_str())
                .expect("Operand was not in mapping")
            {
                OperatorSrc::Result => output,
                OperatorSrc::Input(idx) => &input_types[*idx],
            };

            if output != ty_src {
                #[cfg(feature = "log")]
                log::error!(
                    "output is of type {:?}, src is of type {:?}",
                    output,
                    ty_src
                );

                Err(BackendSpirvError::SpvLegalizationRuleFailed {
                    inst: String::with_capacity(0),
                    rule: rule.clone(),
                })
            } else {
                Ok(())
            }
        }
        Rule::ComponentCountEqual { a, b } => {
            let tya = match operand_mapping
                .get((*a).as_str())
                .expect("Operand was not in mapping")
            {
                OperatorSrc::Result => output,
                OperatorSrc::Input(idx) => &input_types[*idx],
            };
            let tyb = match operand_mapping
                .get((*b).as_str())
                .expect("Operand was not in mapping")
            {
                OperatorSrc::Result => output,
                OperatorSrc::Input(idx) => &input_types[*idx],
            };

            let count_a = if let SpvType::Arith(a) = tya {
                a.shape.component_count()
            } else {
                return Err(BackendSpirvError::SpvLegalizationMalformed {
                    inst: String::with_capacity(0),
                    text: format!(
                        "Rule tests component count, but Operand {} of type {:?} has no components",
                        a, tya
                    ),
                });
            };

            let count_b = if let SpvType::Arith(b) = tyb {
                b.shape.component_count()
            } else {
                return Err(BackendSpirvError::SpvLegalizationMalformed {
                    inst: String::with_capacity(0),
                    text: format!(
                        "Rule tests component count, but Operand {} of type {:?} has no components",
                        b, tyb
                    ),
                });
            };

            if count_a != count_b {
                Err(BackendSpirvError::SpvLegalizationRuleFailed {
                    inst: String::with_capacity(0),
                    rule: rule.clone(),
                })
            } else {
                Ok(())
            }
        }
        Rule::ComponentWidthEqual { a, b } => {
            let tya = match operand_mapping
                .get((*a).as_str())
                .expect("Operand was not in mapping")
            {
                OperatorSrc::Result => output,
                OperatorSrc::Input(idx) => &input_types[*idx],
            };
            let tyb = match operand_mapping
                .get((*b).as_str())
                .expect("Operand was not in mapping")
            {
                OperatorSrc::Result => output,
                OperatorSrc::Input(idx) => &input_types[*idx],
            };

            let count_a = if let SpvType::Arith(a) = tya {
                a.resolution
            } else {
                return Err(BackendSpirvError::SpvLegalizationMalformed {
                    inst: String::with_capacity(0),
                    text: format!(
                        "Rule tests component width, but Operand {} of type {:?} has no components",
                        a, tya
                    ),
                });
            };

            let count_b = if let SpvType::Arith(b) = tyb {
                b.resolution
            } else {
                return Err(BackendSpirvError::SpvLegalizationMalformed {
                    inst: String::with_capacity(0),
                    text: format!(
                        "Rule tests component width, but Operand {} of type {:?} has no components",
                        b, tyb
                    ),
                });
            };

            if count_a != count_b {
                Err(BackendSpirvError::SpvLegalizationRuleFailed {
                    inst: String::with_capacity(0),
                    rule: rule.clone(),
                })
            } else {
                Ok(())
            }
        }
        Rule::ComponentTypeEqual { a, b } => {
            let tya = match operand_mapping
                .get((*a).as_str())
                .expect("Operand was not in mapping")
            {
                OperatorSrc::Result => output,
                OperatorSrc::Input(idx) => &input_types[*idx],
            };
            let tyb = match operand_mapping
                .get((*b).as_str())
                .expect("Operand was not in mapping")
            {
                OperatorSrc::Result => output,
                OperatorSrc::Input(idx) => &input_types[*idx],
            };

            if tya.base_type() != tyb.base_type() {
                Err(BackendSpirvError::SpvLegalizationRuleFailed {
                    inst: String::with_capacity(0),
                    rule: rule.clone(),
                })
            } else {
                Ok(())
            }
        }
        Rule::IsSigned { operand, is_signed } => {
            let ty = match operand_mapping
                .get((*operand).as_str())
                .expect("Operand was not in mapping")
            {
                OperatorSrc::Result => output,
                OperatorSrc::Input(idx) => &input_types[*idx],
            };
            match ty {
                SpvType::Arith(a) => match a.base {
                    ArithBaseTy::Integer { signed } => if signed == *is_signed{
                        Ok(())
                    }else{
                         Err(BackendSpirvError::SpvLegalizationRuleFailed { inst: String::with_capacity(0), rule: rule.clone() })
                    },
                    ArithBaseTy::Float | ArithBaseTy::Bool => Err(BackendSpirvError::SpvLegalizationRuleFailed { inst: String::with_capacity(0), rule: rule.clone() }),
                },
                _ => Err(BackendSpirvError::SpvLegalizationMalformed { inst: String::with_capacity(0), text: format!("Rule checks component signedness, but operand {} of type {:?} has no arithmetic type!", operand, ty) }),
            }
        }
        Rule::ComponentWidth { operand, allowed } => {
            let ty = match operand_mapping
                .get((*operand).as_str())
                .expect("Operand was not in mapping")
            {
                OperatorSrc::Result => output,
                OperatorSrc::Input(idx) => &input_types[*idx],
            };
            match ty {
                SpvType::Arith(a) => if !allowed.contains(&a.resolution){
                        Err(BackendSpirvError::SpvLegalizationRuleFailed {
                            inst: String::with_capacity(0),
                            rule: rule.clone(),
                        })
                }else{
                    Ok(())
                },
                _ => Err(BackendSpirvError::SpvLegalizationMalformed { inst: String::with_capacity(0), text: format!("Rule checks component width, but operand {} of type {:?} has no components!", operand, ty) }),
            }
        }
        Rule::ComponentCount { operand, allowed } => {
            let ty = match operand_mapping
                .get((*operand).as_str())
                .expect("Operand was not in mapping")
            {
                OperatorSrc::Result => output,
                OperatorSrc::Input(idx) => &input_types[*idx],
            };
            match ty {
                SpvType::Arith(a) => {
                    if !allowed.contains(&a.shape.component_count()) {
                        Err(BackendSpirvError::SpvLegalizationRuleFailed {
                            inst: String::with_capacity(0),
                            rule: rule.clone(),
                        })
                    } else {
                        Ok(())
                    }
                }
                _ => Err(BackendSpirvError::SpvLegalizationMalformed { inst: String::with_capacity(0), text: format!("Rule checks component count, but operand {} of type {:?} has no components!", operand, ty) }),
            }
        }
        Rule::Unknown(other) => {
            #[cfg(feature = "log")]
            log::error!("Unknown Rule: {:#?}!", other);
            Ok(())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ArithBaseTy {
    Integer { signed: bool },
    Float,
    Bool,
}

impl Display for ArithBaseTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer { signed } => {
                if *signed {
                    write!(f, "Int")
                } else {
                    write!(f, "Uint")
                }
            }
            Self::Float => write!(f, "Float"),
            Self::Bool => write!(f, "Bool"),
        }
    }
}

impl ArithBaseTy {
    #[allow(unused)]
    fn is_of_typestring(&self, string: &str) -> bool {
        match (self, string) {
            (Self::Integer { .. }, "Integer") => true,
            (Self::Float, "FloatingPoint") => true,
            (Self::Bool, "Bool") => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArithTy {
    pub base: ArithBaseTy,
    pub shape: TyShape,
    pub resolution: u32,
}

impl Display for ArithTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}<{} @ {}>", self.shape, self.base, self.resolution)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyShape {
    Scalar,
    Vector { width: u32 },
    Matrix { width: u32, height: u32 },
    Tensor { dim: SmallVec<[u32; 3]> },
}

impl TyShape {
    fn component_count(&self) -> u32 {
        match self {
            Self::Scalar => 1,
            Self::Vector { width } => *width,
            Self::Matrix { width, height } => *width * *height,
            Self::Tensor { dim } => dim.iter().fold(1u32, |a, b| a * *b),
        }
    }

    pub fn is_matrix(&self) -> bool {
        if let Self::Matrix { .. } = self {
            true
        } else {
            false
        }
    }
}

impl Display for TyShape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TyShape::Scalar => write!(f, "Scalar"),
            TyShape::Vector { width } => write!(f, "Vec{width}"),
            TyShape::Matrix { width, height } => write!(f, "Mat{width}x{height}"),
            TyShape::Tensor { dim } => write!(f, "Tensor[{dim:?}]"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PointerType {}

///At this point we _need_ to move into the strange SPIR-V type system.
/// AlgebraicTypes are the same as the ones in the optimizer.
///
/// However, we need to incoporate the whole _pointer-like_ structures etc.
///
///Right now we don't support images at all, so there is that. However, we need to obey
/// the _xy is pointer-like_ rules of the spec. We express that by having _wrapping_ types
/// that express _y is pointer of type x_.
///
/// Sadly we don't get away with just _typed pointers_, cause there are different _types_ of pointer. So we
/// have to make that explicit atm.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SpvType {
    Void,
    Undefined,
    State,
    Arith(ArithTy),
    RuntimeArray(ArithTy),
    //TODO: This would be, where we add TypeImage, buffers etc.
}

impl SpvType {
    pub fn undefined() -> Self {
        SpvType::Undefined
    }

    pub fn check_logical_operand_input(&self, operand: &LogicalOperand) -> bool {
        //Currently pretty simple, since we are just wireing up ~data~ at the moment.
        operand.kind == OperandKind::IdRef
    }

    ///True if the base type is a float. So for instance for a Matrix of floats
    pub fn is_float_base(&self) -> bool {
        if let Self::Arith(a) = self {
            a.base == ArithBaseTy::Float
        } else {
            false
        }
    }

    pub fn is_int_base(&self) -> bool {
        if let Self::Arith(a) = self {
            if let ArithBaseTy::Integer { .. } = a.base {
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn as_rule_ty(&self) -> Option<spirv_grammar_rules::Type> {
        match self {
            Self::Arith(a) => match a.shape {
                TyShape::Scalar => Some(spirv_grammar_rules::Type::Scalar),
                TyShape::Vector { .. } => Some(spirv_grammar_rules::Type::Vector),
                TyShape::Matrix { .. } => Some(spirv_grammar_rules::Type::Matrix),
                TyShape::Tensor { .. } => None,
            },
            _ => None,
        }
    }

    pub fn base_type(&self) -> Option<spirv_grammar_rules::Type> {
        match self {
            Self::Arith(a) => match a.base {
                ArithBaseTy::Integer { .. } => Some(spirv_grammar_rules::Type::Integer),
                ArithBaseTy::Float => Some(spirv_grammar_rules::Type::FloatingPoint),
                ArithBaseTy::Bool => Some(spirv_grammar_rules::Type::Boolean),
            },
            _ => None,
        }
    }
}

impl TryFrom<vola_opt::common::Ty> for SpvType {
    type Error = BackendSpirvError;
    fn try_from(value: vola_opt::common::Ty) -> Result<Self, Self::Error> {
        //FIXME: Currently those are all hard coded, since the optimizer currently does not track resolution.
        let res = match value {
            vola_opt::common::Ty::VOID => Self::Void,
            vola_opt::common::Ty::SCALAR_INT => Self::Arith(ArithTy {
                base: ArithBaseTy::Integer { signed: false },
                shape: TyShape::Scalar,
                resolution: 32,
            }),

            vola_opt::common::Ty::SCALAR_REAL => Self::Arith(ArithTy {
                base: ArithBaseTy::Float,
                shape: TyShape::Scalar,
                resolution: 32,
            }),
            vola_opt::common::Ty::Shaped {
                ty: DataType::Real,
                shape: Shape::Vec { width },
            } => Self::Arith(ArithTy {
                base: ArithBaseTy::Float,
                shape: TyShape::Vector {
                    width: width as u32,
                },
                resolution: 32,
            }),
            vola_opt::common::Ty::Shaped {
                ty: DataType::Real,
                shape: Shape::Matrix { width, height },
            } => Self::Arith(ArithTy {
                base: ArithBaseTy::Float,
                shape: TyShape::Matrix {
                    width: width as u32,
                    height: height as u32,
                },
                resolution: 32,
            }),

            vola_opt::common::Ty::Shaped {
                ty: DataType::Real,
                shape: Shape::Tensor { sizes },
            } => Self::Arith(ArithTy {
                base: ArithBaseTy::Float,
                shape: TyShape::Tensor {
                    dim: sizes.into_iter().map(|d| d as u32).collect(),
                },
                resolution: 32,
            }),
            vola_opt::common::Ty::SCALAR_BOOL => Self::Arith(ArithTy {
                base: ArithBaseTy::Bool,
                shape: TyShape::Scalar,
                resolution: 1,
            }),
            any => return Err(BackendSpirvError::TypeConversionError(any)),
        };

        Ok(res)
    }
}

impl Display for SpvType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SpvType::Void => write!(f, "Void"),
            SpvType::Undefined => write!(f, "Undefined"),
            SpvType::State => write!(f, "State"),
            SpvType::Arith(a) => write!(f, "{a}"),
            SpvType::RuntimeArray(a) => write!(f, "RA<{a}>"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum SpvOp {
    CoreOp(CoreOp),
    GlslOp(GlOp),
    //NOTE: Its not enought to just safe the op in that case.
    ConstantFloat {
        resolution: u32,
        bits: u32,
    },
    ConstantInt {
        resolution: u32,
        bits: u32,
    },
    ConstantBool(bool),
    ///_some_ kind of constant extract. The exact instruction depends on _what_ is
    // being used, but we _know_ that the access indices are _constant_.
    Extract(SmallVec<[u32; 3]>),
    UniformConstruct,
}

impl SpvOp {
    pub fn name(&self) -> String {
        match &self {
            SpvOp::CoreOp(op) => rspirv::grammar::CoreInstructionTable::get(op.clone())
                .opname
                .to_owned(),
            SpvOp::GlslOp(op) => rspirv::grammar::GlslStd450InstructionTable::get(op.clone())
                .opname
                .to_owned(),
            SpvOp::ConstantFloat { resolution, bits } => {
                format!("f{}: {}", resolution, f32::from_bits(*bits))
            }
            SpvOp::ConstantInt { resolution, bits } => format!("i{resolution}: {bits}"),
            SpvOp::ConstantBool(b) => format!("{b}"),
            SpvOp::Extract(ex) => format!("Extract: {:?}", ex),
            SpvOp::UniformConstruct => "UniformConstruct".to_owned(),
        }
    }

    pub fn legalize_for_pattern(
        &mut self,
        core_grammar: &mut GrammarRules,
        glsl_grammar: &mut GrammarRules,
        inputs: &[SpvType],
        output: &SpvType,
    ) -> Result<(), BackendSpirvError> {
        //right now we mostly check that the input and output type are the same as
        //found in the grammar.
        match &self {
            SpvOp::CoreOp(coreop) => {
                let core_instruction = rspirv::grammar::CoreInstructionTable::get(*coreop);
                //NOTE: result operand is _usally_ the first followed by its type-id.
                if core_instruction.operands.len() != (inputs.len() + 2) {
                    return Err(BackendSpirvError::Any {
                        text: format!(
                            "Operation {} expected {} operands, but got {}",
                            core_instruction.opname,
                            core_instruction.operands.len(),
                            inputs.len() + 2
                        ),
                    });
                }

                //now check that the operands are in fact the right ones

                //For results we just _expect_ to have a result id and result-type-id. The others are only checked to be
                //IdRefs, since we are currently not supporting anything else.
                if core_instruction.operands[0].kind != OperandKind::IdResultType
                    || core_instruction.operands[1].kind != OperandKind::IdResult
                {
                    return Err(BackendSpirvError::Any { text: format!("Output of node {} did not match grammar. Got type {:?}, but expected {:?}", core_instruction.opname, output, core_instruction.operands[1]) });
                }
                for input_idx in 0..inputs.len() {
                    if !inputs[input_idx]
                        .check_logical_operand_input(&core_instruction.operands[input_idx + 2])
                    {
                        #[cfg(feature = "log")]
                        log::warn!("Whole Instruction: {:#?}", core_instruction);
                        return Err(BackendSpirvError::Any { text: format!("Input of node {} did not match grammar. Got type {:?}, but expected {:?}", core_instruction.opname, inputs[input_idx], core_instruction.operands[input_idx + 2]) });
                    }
                }

                type_pattern_check_core_op(core_grammar, coreop, inputs, output)
            }
            SpvOp::GlslOp(glslop) => {
                let glinst = rspirv::grammar::GlslStd450InstructionTable::get(*glslop);
                //NOTE: glsl instructions are called by the OpExtinst, so the instruction-table does not encode the result-type / result-id
                if glinst.operands.len() != inputs.len() {
                    return Err(BackendSpirvError::Any {
                        text: format!(
                            "Operation {} expected {} operands, but got {}",
                            glinst.opname,
                            glinst.operands.len(),
                            inputs.len()
                        ),
                    });
                }

                //now check that the operands are in fact the right ones
                for input_idx in 0..inputs.len() {
                    if !inputs[input_idx].check_logical_operand_input(&glinst.operands[input_idx]) {
                        #[cfg(feature = "log")]
                        log::warn!(
                            "Failed glop:\nop = {:#?}, operands={:#?}",
                            glinst.opname,
                            glinst.operands
                        );
                        return Err(BackendSpirvError::Any { text: format!("Input of node {} did not match grammar. Got type {:?}, but expected {:?}", glinst.opname, inputs[input_idx], glinst.operands[input_idx]) });
                    }
                }

                type_pattern_check_gl_op(glsl_grammar, glslop, inputs, output)
            }
            SpvOp::ConstantFloat { .. } | SpvOp::ConstantInt { .. } | SpvOp::ConstantBool(_) => {
                //thats always cool I think :eyes:
                Ok(())
            }
            SpvOp::Extract(_idx) => {
                //TODO: implement
                #[cfg(feature = "log")]
                log::warn!("Extract bound checking not yet implemented!");
                Ok(())
            }
            SpvOp::UniformConstruct => {
                //TODO: implement
                #[cfg(feature = "log")]
                log::warn!("UniformConstruct checking not yet implemented!");
                Ok(())
            }
        }
    }

    pub fn instruction_is_type_or_constant(&self) -> bool {
        match self {
            SpvOp::ConstantFloat { .. } | SpvOp::ConstantInt { .. } | SpvOp::ConstantBool(_) => {
                true
            }
            _ => false,
        }
    }

    pub fn build_instruction(
        &self,
        ctx: &EmitCtx,
        input_ids: &[Word],
        result_type_id: Word,
        result_id: Word,
    ) -> Instruction {
        match &self {
            SpvOp::CoreOp(coreop) => Instruction::new(
                *coreop,
                Some(result_type_id),
                Some(result_id),
                input_ids.iter().map(|id| Operand::IdRef(*id)).collect(),
            ),
            SpvOp::GlslOp(glslop) => {
                //use the extinst to call glslop
                let glsl_inst_set_id = ctx.extinst_ids.get("GLSL.std.450").unwrap();
                let mut operands = Vec::with_capacity(2 + input_ids.len());

                //first push the instset
                operands.push(Operand::IdRef(*glsl_inst_set_id));
                //now push the instruction literal as defined by the spec
                operands.push(Operand::LiteralBit32(*glslop as u32));
                //finally append the inputs
                for iid in input_ids {
                    operands.push(Operand::IdRef(*iid));
                }

                Instruction::new(
                    CoreOp::ExtInst,
                    Some(result_type_id),
                    Some(result_id),
                    operands,
                )
            }
            SpvOp::ConstantFloat { resolution, bits } | SpvOp::ConstantInt { resolution, bits } => {
                let operand = if *resolution <= 32 {
                    Operand::LiteralBit32(*bits)
                } else {
                    if *resolution <= 64 {
                        Operand::LiteralBit64((*bits).into())
                    } else {
                        panic!("Can only create constants up to 64bit, but got {resolution}");
                    }
                };

                Instruction::new(
                    CoreOp::Constant,
                    Some(result_type_id),
                    Some(result_id),
                    vec![operand],
                )
            }
            SpvOp::ConstantBool(b) => {
                //Similar hack, since constant bool has to be defined _outside_ as well
                let core_op = match b {
                    true => CoreOp::ConstantTrue,
                    false => CoreOp::ConstantFalse,
                };
                Instruction::new(core_op, Some(result_type_id), Some(result_id), vec![])
            }
            SpvOp::Extract(chain) => {
                //right now this always translates to OpCompositeExtract
                let mut operands = Vec::with_capacity(1 + chain.len());

                assert!(
                    input_ids.len() == 1,
                    "Expected a single ref to the composite thats being extracted!"
                );
                operands.push(Operand::IdRef(input_ids[0]));
                for offset in chain {
                    operands.push(Operand::LiteralBit32(*offset));
                }

                Instruction::new(
                    CoreOp::CompositeExtract,
                    Some(result_type_id),
                    Some(result_id),
                    operands,
                )
            }
            SpvOp::UniformConstruct => Instruction::new(
                CoreOp::CompositeConstruct,
                Some(result_type_id),
                Some(result_id),
                input_ids.iter().map(|id| Operand::IdRef(*id)).collect(),
            ),
        }
    }
}
