/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Defines the SPIR-V dialect nodes for the backend. We use the [rspirv crate's](https://docs.rs/rspirv/latest/rspirv/grammar) grammar specification to
//! work with spir-v ops in the graph.

use ahash::AHashMap;
use rspirv::grammar::{LogicalOperand, OperandKind};
use rvsdg::smallvec::{smallvec, SmallVec};
use spirv_grammar_rules::{GrammarRules, Rule};
use vola_common::dot::graphviz_rust::attributes::width;
use vola_opt::{
    alge::{CallOp, ConstantIndex, Construct, ImmNat, ImmScalar, WkOp},
    OptNode,
};

use crate::BackendSpirvError;

type CoreOp = rspirv::spirv::Op;

enum OperatorSrc {
    Result,
    Input(usize),
}

fn type_pattern_check_core_op(
    crules: &mut GrammarRules,
    coreop: &CoreOp,
    input_types: &[SpvType],
    output: &SpvType,
) -> bool {
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
            if !test_rule(&operator_mapping, input_types, output, rule) {
                #[cfg(feature = "log")]
                log::error!("{:?} failed rule {:?}", coreop, rule);
                return false;
            }
        }

        true
    } else {
        #[cfg(feature = "log")]
        log::error!(
            "{:?} did not exist in grammar-rules {}!",
            coreop,
            crules.source_grammar
        );
        true
    }
}

type GlOp = rspirv::spirv::GLOp;
fn type_pattern_check_gl_op(
    grules: &mut GrammarRules,
    glop: &GlOp,
    input_types: &[SpvType],
    output: &SpvType,
) -> bool {
    let opcode = *glop as u32;

    if let Some(instruction_rules) = grules.lookup_opcode(opcode) {
        if instruction_rules.rules.len() == 0 {
            #[cfg(feature = "log")]
            log::error!("{:?} had no rules associated in grammar-rules!", glop);
        }

        let mut operator_mapping = AHashMap::with_capacity(input_types.len() + 1);
        //in the case of the GLOperator we add _Result_ for the output, since the actual
        //IdResult_1 is set by the OpExtsInst.
        let _ = operator_mapping.insert("Output", OperatorSrc::Result);

        for (opidx, op) in instruction_rules.operand_mapping.iter().enumerate() {
            let _ = operator_mapping.insert(op, OperatorSrc::Input(opidx));
        }

        //now test all rules with the given mapping
        for rule in &instruction_rules.rules {
            if !test_rule(&operator_mapping, input_types, output, rule) {
                #[cfg(feature = "log")]
                log::error!("{:?} failed rule {:?}", glop, rule);
                return false;
            }
        }

        true
    } else {
        #[cfg(feature = "log")]
        log::error!(
            "{:?} did not exist in grammar-rules {}!",
            glop,
            grules.source_grammar
        );
        true
    }
}

///Tests the `rule` for a given operand_mapping on `input_types` and `output`.
fn test_rule<'a>(
    operand_mapping: &AHashMap<&'a str, OperatorSrc>,
    input_types: &[SpvType],
    output: &SpvType,
    rule: &Rule,
) -> bool {
    match rule {
        Rule::BaseType {
            operand,
            base_types,
        } => {
            #[cfg(feature = "log")]
            log::warn!("Rule unimplemented!");
            true
        }
        Rule::TypeConstraint { operand, ty } => {
            #[cfg(feature = "log")]
            log::warn!("Rule unimplemented!");
            true
        }
        Rule::ResultEqualType(ty) => {
            #[cfg(feature = "log")]
            log::warn!("Rule unimplemented!");
            true
        }
        Rule::ComponentCountEqual { a, b } => {
            #[cfg(feature = "log")]
            log::warn!("Rule unimplemented!");
            true
        }
        Rule::ComponentWidthEqual { a, b } => {
            #[cfg(feature = "log")]
            log::warn!("Rule unimplemented!");

            true
        }
        Rule::ComponentTypeEqual { a, b } => {
            #[cfg(feature = "log")]
            log::warn!("Rule unimplemented!");
            true
        }
        Rule::IsSigned { operand, is_signed } => {
            #[cfg(feature = "log")]
            log::warn!("Rule unimplemented!");
            true
        }
        Rule::ComponentWidth { operand, allowed } => {
            let ty = match operand_mapping.get((*operand).as_str()).unwrap() {
                OperatorSrc::Result => output,
                OperatorSrc::Input(idx) => &input_types[*idx],
            };
            match ty {
                SpvType::Arith(a) => allowed.contains(&(a.resolution as usize)),
                _ => false,
            }
        }
        Rule::ComponentCount { operand, allowed } => {
            let ty = match operand_mapping.get((*operand).as_str()).unwrap() {
                OperatorSrc::Result => output,
                OperatorSrc::Input(idx) => &input_types[*idx],
            };
            match ty {
                SpvType::Arith(a) => allowed.contains(&a.shape.component_count()),
                _ => false,
            }
        }
        Rule::Unknown(other) => {
            #[cfg(feature = "log")]
            log::error!("Rule {:?} not implemented!", other);
            true
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArithBaseTy {
    Integer { signed: bool },
    Float,
}

impl ArithBaseTy {
    fn is_of_typestring(&self, string: &str) -> bool {
        match (self, string) {
            (Self::Integer { .. }, "Integer") => true,
            (Self::Float, "FloatingPoint") => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArithTy {
    base: ArithBaseTy,
    shape: TyShape,
    resolution: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyShape {
    Scalar,
    Vector { width: u32 },
    Matrix { width: u32, height: u32 },
    Tensor { dim: SmallVec<[u32; 3]> },
}

impl TyShape {
    fn component_count(&self) -> usize {
        match self {
            Self::Scalar => 1,
            Self::Vector { width } => *width as usize,
            Self::Matrix { width, height } => (*width as usize) * (*height as usize),
            Self::Tensor { dim } => dim.iter().fold(1usize, |a, b| a * (*b as usize)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpvType {
    Void,
    Undefined,
    State,
    Arith(ArithTy),
    RuntimeArray,
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
}

impl TryFrom<vola_opt::common::Ty> for SpvType {
    type Error = BackendSpirvError;
    fn try_from(value: vola_opt::common::Ty) -> Result<Self, Self::Error> {
        //FIXME: Currently those are all hard coded, since the optimizer currently does not track resolution.
        let res = match value {
            vola_opt::common::Ty::Void => Self::Void,
            vola_opt::common::Ty::Nat => Self::Arith(ArithTy {
                base: ArithBaseTy::Integer { signed: false },
                shape: TyShape::Scalar,
                resolution: 32,
            }),

            vola_opt::common::Ty::Scalar => Self::Arith(ArithTy {
                base: ArithBaseTy::Float,
                shape: TyShape::Scalar,
                resolution: 32,
            }),
            vola_opt::common::Ty::Vector { width } => Self::Arith(ArithTy {
                base: ArithBaseTy::Float,
                shape: TyShape::Vector {
                    width: width as u32,
                },
                resolution: 32,
            }),
            vola_opt::common::Ty::Matrix { width, height } => Self::Arith(ArithTy {
                base: ArithBaseTy::Float,
                shape: TyShape::Matrix {
                    width: width as u32,
                    height: height as u32,
                },
                resolution: 32,
            }),

            vola_opt::common::Ty::Tensor { dim } => Self::Arith(ArithTy {
                base: ArithBaseTy::Float,
                shape: TyShape::Tensor {
                    dim: dim.into_iter().map(|d| d as u32).collect(),
                },
                resolution: 32,
            }),
            any => return Err(BackendSpirvError::TypeConversionError(any)),
        };

        Ok(res)
    }
}

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
    ///_some_ kind of constant extract. The exact instruction depends on _what_ is
    // being used, but we _know_ that the access indices are _constant_.
    Extract(SmallVec<[u32; 3]>),
    Construct,
}

///A single SPIR-V dialect node.
pub struct SpvNode {
    ///The Op represented by this node.
    op: SpvOp,
}

impl SpvNode {
    ///Tries to build a SpvNode from some optimizer node.
    ///Returns None, if no SPIR-V equivalent exists
    pub fn try_from_opt_node(optnode: &OptNode) -> Option<Self> {
        //in practice we try to cast to the different alge-dialect nodes for now.

        if let Some(imm) = optnode.try_downcast_ref::<ImmScalar>() {
            return Some(Self::from_imm_scalar(imm));
        }

        if let Some(imm) = optnode.try_downcast_ref::<ImmNat>() {
            return Some(Self::from_imm_nat(imm));
        }

        if let Some(callop) = optnode.try_downcast_ref::<CallOp>() {
            return Some(Self::from_wk(&callop.op));
        }

        if let Some(facc) = optnode.try_downcast_ref::<ConstantIndex>() {
            return Some(Self::from_const_index(facc));
        }

        if let Some(lconst) = optnode.try_downcast_ref::<Construct>() {
            return Some(Self::from_construct(lconst));
        }

        None
    }

    fn from_imm_scalar(imm: &ImmScalar) -> Self {
        //FIXME: kinda dirty atm. At some point we might want to
        //       track resolutions and stuff. Right now we just cast :D
        Self {
            op: SpvOp::ConstantFloat {
                resolution: 32,
                bits: (imm.lit as f32).to_bits(),
            },
        }
    }

    fn from_imm_nat(imm: &ImmNat) -> Self {
        Self {
            op: SpvOp::ConstantInt {
                resolution: 32,
                bits: (imm.lit as u32).to_be(),
            },
        }
    }

    fn from_wk(wk: &WkOp) -> Self {
        let spvop = match wk {
            WkOp::Not => SpvOp::CoreOp(CoreOp::Not),
            //NOTE: Since we just have floats, we can just use FNegate
            WkOp::Neg => SpvOp::CoreOp(CoreOp::FNegate),
            WkOp::Add => SpvOp::CoreOp(CoreOp::FAdd),
            WkOp::Sub => SpvOp::CoreOp(CoreOp::FSub),
            WkOp::Mul => SpvOp::CoreOp(CoreOp::FMul),
            WkOp::Div => SpvOp::CoreOp(CoreOp::FDiv),

            WkOp::Mod => SpvOp::CoreOp(CoreOp::FMod),

            WkOp::Dot => SpvOp::CoreOp(CoreOp::Dot),
            WkOp::Cross => SpvOp::GlslOp(GlOp::Cross),
            WkOp::Length => SpvOp::GlslOp(GlOp::Length),
            WkOp::SquareRoot => SpvOp::GlslOp(GlOp::Sqrt),
            WkOp::Exp => SpvOp::GlslOp(GlOp::Exp),
            WkOp::Min => SpvOp::GlslOp(GlOp::FMin),
            WkOp::Max => SpvOp::GlslOp(GlOp::FMax),
            WkOp::Mix => SpvOp::GlslOp(GlOp::FMix),
            WkOp::Clamp => SpvOp::GlslOp(GlOp::FClamp),
            WkOp::Abs => SpvOp::GlslOp(GlOp::FAbs),
            WkOp::Frac => SpvOp::GlslOp(GlOp::Fract),
        };

        Self { op: spvop }
    }

    fn from_const_index(fac: &ConstantIndex) -> Self {
        Self {
            op: SpvOp::Extract(smallvec![fac.access as u32]),
        }
    }

    fn from_construct(_lc: &Construct) -> Self {
        Self {
            op: SpvOp::Construct,
        }
    }

    pub fn name(&self) -> String {
        match &self.op {
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
            SpvOp::Extract(ex) => format!("Extract: {:?}", ex),
            SpvOp::Construct => "ConstantConstruct".to_owned(),
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
        match &self.op {
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

                if !type_pattern_check_core_op(core_grammar, coreop, inputs, output) {
                    return Err(BackendSpirvError::Any {
                        text: format!(
                            "{} did not pass type pattern check for input={:?} output={:?}",
                            core_instruction.opname, inputs, output
                        ),
                    });
                }
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

                if !type_pattern_check_gl_op(glsl_grammar, glslop, inputs, output) {
                    return Err(BackendSpirvError::Any {
                        text: format!(
                            "{} did not pass type pattern check for input={:?} output={:?}",
                            glinst.opname, inputs, output
                        ),
                    });
                }
            }
            SpvOp::ConstantFloat { .. } | SpvOp::ConstantInt { .. } => {
                //thats always cool I think :eyes:
            }
            SpvOp::Extract(idx) => {
                //todo!("do bound check")
            }
            SpvOp::Construct => {
                //todo!("Do ... things?")
            }
        }

        Ok(())
    }
}
