/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Defines the SPIR-V dialect nodes for the backend. We use the [rspirv crate's](https://docs.rs/rspirv/latest/rspirv/grammar) grammar specification to
//! work with spir-v ops in the graph.

use rvsdg::smallvec::{smallvec, SmallVec};
use vola_opt::{
    alge::{CallOp, ConstantIndex, Construct, ImmNat, ImmScalar, WkOp},
    OptNode,
};

type CoreOp = rspirv::spirv::Op;
type GlOp = rspirv::spirv::GLOp;

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
}
