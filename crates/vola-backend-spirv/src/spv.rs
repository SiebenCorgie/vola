/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Defines the SPIR-V dialect nodes for the backend. We use the [rspirv crate's](https://docs.rs/rspirv/latest/rspirv/grammar) grammar specification to
//! work with spir-v ops in the graph.

use rvsdg::smallvec::SmallVec;
use vola_opt::{
    alge::{CallOp, ConstantIndex, Construct, ImmNat, ImmScalar, Literal, WkOp},
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
    ///_some_ kind of constant extract. The exact instruction depends on _what_ is
    // being used, but we _know_ that the access indices are _constant_.
    ConstantExtract(SmallVec<[u32; 3]>),
    ConstantConstruct,
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
        todo!()
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
        //for the field access we have to construct the OpCompositeExtract
        //
        // Right now the source language does not allow things like shuffeling etc. So in the end we always get a
        // float.
        //TODO: At some point it might be possible to do shuffeling and extracting based on a variable, in that case
        //      We would have to handle that here :(

        let accessor_list = fac
            .access_list
            .iter()
            .map(|f| {
                f.try_to_index()
                    .expect(&format!("cannot convert {:?} to accessor index!", f))
                    .try_into()
                    .expect("Failed to convert index to u32")
            })
            .collect::<SmallVec<[u32; 3]>>();

        Self {
            op: SpvOp::ConstantExtract(accessor_list),
        }
    }

    fn from_construct(_lc: &Construct) -> Self {
        Self {
            op: SpvOp::ConstantConstruct,
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
            SpvOp::ConstantFloat { resolution, bits } => format!("f{}: 0x{:x}", resolution, bits),
            SpvOp::ConstantExtract(ex) => format!("Extract: {:?}", ex),
            SpvOp::ConstantConstruct => "ConstantConstruct".to_owned(),
        }
    }
}
