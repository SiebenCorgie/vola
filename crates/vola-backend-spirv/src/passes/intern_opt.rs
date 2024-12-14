/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Simple `GraphTypeTransformer` pass that transforms an optimizer-graph to the spirv-backend graph.

use rvsdg::{
    attrib::AttribLocation,
    region::{Input, Output},
    smallvec::smallvec,
    util::graph_type_transform::{GraphMapping, GraphTypeTransformer, GraphTypeTransformerError},
};
use vola_common::{error::error_reporter, report, Span};
// use vola_common::{error::error_reporter, report, Span};
use vola_opt::{
    alge::{
        arithmetic::{BinaryArith, UnaryArith, UnaryArithOp},
        buildin::{Buildin, BuildinOp},
        logical::{BinaryBool, BinaryBoolOp, UnaryBool, UnaryBoolOp},
        matrix::{UnaryMatrix, UnaryMatrixOp},
        relational::{BinaryRel, BinaryRelOp},
        trigonometric::{Trig, TrigOp},
    },
    imm::{ImmBool, ImmNat, ImmScalar},
    typelevel::{ConstantIndex, NonUniformConstruct, UniformConstruct},
    OptEdge, OptNode, Optimizer,
};

use crate::{
    graph::{BackendEdge, BackendNode, BackendOp},
    hl::HlOp,
    spv::{CoreOp, GlOp, SpvOp, SpvType},
    BackendSpirvError, SpirvBackend,
};

pub struct InterningTransformer;

impl GraphTypeTransformer for InterningTransformer {
    type SrcNode = OptNode;
    type SrcEdge = OptEdge;
    type DstNode = BackendNode;
    type DstEdge = BackendEdge;

    fn transform_edge(&mut self, src_edge: &Self::SrcEdge) -> Self::DstEdge {
        //This basically just erases the type information atm.
        match src_edge {
            OptEdge::State => BackendEdge::State,
            OptEdge::Value { ty } => {
                BackendEdge::Value(
                    ty.get_type()
                        .map(|t| {
                            let tc = t.clone();
                            if let Ok(converted) = t.try_into() {
                                converted
                            } else {
                                report(
                                    error_reporter(
                                        BackendSpirvError::Any {
                                            text: format!(
                                                "could not convert type {tc} to SPIR-V Type",
                                            ),
                                        },
                                        Span::empty(),
                                    )
                                    .finish(),
                                );
                                SpvType::Undefined
                            }
                        })
                        .unwrap_or(SpvType::undefined()),
                )
            }
        }
    }
    fn transform_simple_node(&mut self, src_node: &Self::SrcNode) -> Self::DstNode {
        let op = if let Some(sop) = BackendOp::try_from_opt_node(src_node) {
            sop
        } else {
            #[cfg(feature = "log")]
            log::error!(
                "Failed to transform opt node {} into SPIR-V",
                src_node.node.name()
            );
            BackendOp::Dummy
        };

        let node = BackendNode {
            inputs: smallvec![Input::default(); src_node.node.inputs().len()],
            output: Output::default(),
            op,
        };

        node
    }
    /*
    fn on_mapping(
        &mut self,
        src_graph: &rvsdg::Rvsdg<Self::SrcNode, Self::SrcEdge>,
        src_node: rvsdg::NodeRef,
        dst_graph: &mut rvsdg::Rvsdg<Self::DstNode, Self::DstEdge>,
        dst_node: rvsdg::NodeRef,
    ) {
        //for each mapping, check if there is a port-type, if so we add the mapped-type to
        //the node in `dst_graph`.
        for outty in src_graph.node(src_node).outport_types() {
            let edge_count = src_graph
                .node(src_node)
                .outport(&outty)
                .unwrap()
                .edges
                .len();
            for edg_idx in 0..edge_count {
                let src_edge_id = src_graph.node(src_node).outport(&outty).unwrap().edges[edg_idx];
                let dst_edg_id = dst_graph.node(dst_node).outport(&outty).unwrap().edges[edg_idx];

                if let Some(ty) = src_graph.edge(src_edge_id).ty.get_type() {
                    if let BackendEdge::Value(dst_ty) = &mut dst_graph.edge_mut(dst_edg_id).ty {
                        *dst_ty = ty.clone().into();
                    } else {
                        #[cfg(feature = "log")]
                        log::warn!("expected typed value-edge");
                    }
                }
            }
        }
    }
    */
}

impl SpirvBackend {
    pub fn intern_opt_graph(&mut self, opt: &Optimizer) -> Result<(), GraphTypeTransformerError> {
        //right now we do expect the module to have no imports at all. Since we don't expect to link anything
        assert!(
            opt.graph
                .region(&opt.graph.toplevel_region())
                .unwrap()
                .arguments
                .len()
                == 0,
            "Unexpected import on optimizer graph!"
        );

        let mut transformer = InterningTransformer;
        //to be sure that we carry over all exports, first transform into a local graph, then merge with the existing one.
        let (new_graph, remapping) = opt.graph.transform_new(&mut transformer)?;

        #[cfg(feature = "log")]
        {
            //emit error if the current graph is not empty
            //TODO: implement graph merging instead, which should append the exports.
            if self
                .graph
                .region(&self.graph.toplevel_region())
                .unwrap()
                .nodes
                .len()
                > 0
            {
                log::error!("Merging of backend-graphs not yet supported, overwriting!")
            }
        }

        //As part of the interning progress we try to recover all known type information from the opt-graph
        //and move that into the backend-graph.
        self.graph = new_graph;

        //now use the remapping to transfer identifiers and source spans
        self.transfer_debug_info(&remapping, opt);
        self.transfer_type_info(&remapping, opt);

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("VOLA_OPT_INTERN").is_ok() {
            self.push_debug_state("Post opt interning");
        }

        Ok(())
    }

    fn transfer_type_info(&mut self, remapping: &GraphMapping, opt: &Optimizer) {
        for (location, ty) in opt.typemap.flags.iter() {
            let remapped_attrib_location = match location {
                AttribLocation::InPort(port) => {
                    let mut remapped_port = port.clone();
                    if let Some(remapped_node) = remapping.node_mapping.get(&port.node) {
                        remapped_port.node = *remapped_node;
                        AttribLocation::InPort(remapped_port)
                    } else {
                        //Could not remap, therfore ignore
                        continue;
                    }
                }
                AttribLocation::OutPort(port) => {
                    let mut remapped_port = port.clone();
                    if let Some(remapped_node) = remapping.node_mapping.get(&port.node) {
                        remapped_port.node = *remapped_node;
                        AttribLocation::OutPort(remapped_port)
                    } else {
                        //Could not remap, therfore ignore
                        continue;
                    }
                }
                AttribLocation::Region(reg) => {
                    if let Some(remapped_reg_node) = remapping.node_mapping.get(&reg.node) {
                        let mut regloc = reg.clone();
                        regloc.node = *remapped_reg_node;
                        AttribLocation::Region(regloc)
                    } else {
                        continue;
                    }
                }
                AttribLocation::Node(node) => {
                    if let Some(rmn) = remapping.node_mapping.get(node) {
                        AttribLocation::Node(*rmn)
                    } else {
                        continue;
                    }
                }
                AttribLocation::Edge(_edg) => {
                    //Those are taken care of later
                    continue;
                }
            };

            //now try to get the type info, and if there is some, transfore it to the new attribloc
            if let Ok(newty) = ty.clone().try_into() {
                self.typemap.set(remapped_attrib_location, newty);
            }
        }
        //now do all the edges explicitly
        for (src_edg, dst_edg) in remapping.edge_mapping.iter() {
            if let Some(ty) = opt.graph.edge(*src_edg).ty.get_type() {
                if let Ok(converted_type) = ty.clone().try_into() {
                    //replace type info, make sure we only overide untyped edges
                    assert!(self
                        .graph
                        .edge_mut(*dst_edg)
                        .ty
                        .set_type(converted_type)
                        .is_some());
                }
            }
        }
    }

    fn transfer_debug_info(&mut self, remapping: &GraphMapping, opt: &Optimizer) {
        for (src_node, dst_node) in remapping.node_mapping.iter() {
            if let Some(name) = opt.names.get(&src_node.into()) {
                self.idents.set(dst_node.into(), name.clone());
            }
            if let Some(span) = opt.span_tags.get(&src_node.into()) {
                self.spans.set(dst_node.into(), span.clone());
            }
        }
    }
}

impl BackendOp {
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

        if let Some(imm) = optnode.try_downcast_ref::<ImmBool>() {
            return Some(Self::SpirvOp(SpvOp::ConstantBool(imm.lit)));
        }

        if let Some(binary_arith) = optnode.try_downcast_ref::<BinaryArith>() {
            return Some(Self::HlOp(binary_arith.op.into()));
        }

        if let Some(unary_arith) = optnode.try_downcast_ref::<UnaryArith>() {
            return Some(Self::from_unary_arith(unary_arith.op));
        }

        if let Some(unary_arith) = optnode.try_downcast_ref::<Trig>() {
            return Some(Self::from_trig(unary_arith.op));
        }

        if let Some(buildin) = optnode.try_downcast_ref::<Buildin>() {
            return Some(Self::from_buildin(buildin.op));
        }

        if let Some(facc) = optnode.try_downcast_ref::<ConstantIndex>() {
            return Some(Self::from_const_index(facc));
        }

        if let Some(lconst) = optnode.try_downcast_ref::<UniformConstruct>() {
            return Some(Self::from_uniform_construct(lconst));
        }

        if let Some(lconst) = optnode.try_downcast_ref::<NonUniformConstruct>() {
            return Some(Self::from_non_uniform_construct(lconst));
        }

        if let Some(binray_rel) = optnode.try_downcast_ref::<BinaryRel>() {
            return Some(Self::from_binary_rel(binray_rel.op));
        }

        if let Some(binarybool) = optnode.try_downcast_ref::<BinaryBool>() {
            match binarybool.op {
                BinaryBoolOp::And => {
                    return Some(BackendOp::SpirvOp(SpvOp::CoreOp(CoreOp::LogicalAnd)))
                }
                BinaryBoolOp::Or => {
                    return Some(BackendOp::SpirvOp(SpvOp::CoreOp(CoreOp::LogicalOr)))
                }
            }
        }
        if let Some(unarybool) = optnode.try_downcast_ref::<UnaryBool>() {
            match unarybool.op {
                UnaryBoolOp::Not => return Some(BackendOp::SpirvOp(SpvOp::CoreOp(CoreOp::Not))),
            }
        }

        if let Some(matrix_op) = optnode.try_downcast_ref::<UnaryMatrix>() {
            match matrix_op.op {
                UnaryMatrixOp::Invert => {
                    return Some(BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::MatrixInverse)))
                }
            }
        }

        None
    }

    fn from_binary_rel(rel: BinaryRelOp) -> Self {
        match rel {
            BinaryRelOp::Lt => BackendOp::HlOp(HlOp::Lt),
            BinaryRelOp::Gt => BackendOp::HlOp(HlOp::Gt),
            BinaryRelOp::Lte => BackendOp::HlOp(HlOp::Lte),
            BinaryRelOp::Gte => BackendOp::HlOp(HlOp::Gte),
            BinaryRelOp::Eq => BackendOp::HlOp(HlOp::Eq),
            BinaryRelOp::NotEq => BackendOp::HlOp(HlOp::Neq),
        }
    }

    fn from_imm_scalar(imm: &ImmScalar) -> Self {
        //FIXME: kinda dirty atm. At some point we might want to
        //       track resolutions and stuff. Right now we just cast :D
        BackendOp::SpirvOp(SpvOp::ConstantFloat {
            resolution: 32,
            bits: (imm.lit as f32).to_bits(),
        })
    }

    fn from_imm_nat(imm: &ImmNat) -> Self {
        BackendOp::SpirvOp(SpvOp::ConstantInt {
            resolution: 32,
            bits: (imm.lit as u32).to_le(),
        })
    }

    fn from_unary_arith(op: UnaryArithOp) -> Self {
        match op {
            UnaryArithOp::Abs => BackendOp::HlOp(HlOp::Abs),
            UnaryArithOp::Ceil => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Ceil)),
            UnaryArithOp::Floor => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Floor)),
            UnaryArithOp::Round => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Round)),
            UnaryArithOp::Fract => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Fract)),
            //NOTE: Since we just have floats, we can just use FNegate
            UnaryArithOp::Neg => BackendOp::HlOp(HlOp::Negate),
        }
    }

    fn from_trig(op: TrigOp) -> Self {
        match op {
            TrigOp::Sin => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Sin)),
            TrigOp::Cos => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Cos)),
            TrigOp::Tan => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Tan)),
            TrigOp::ASin => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Asin)),
            TrigOp::ACos => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Acos)),
            TrigOp::ATan => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Atan)),
        }
    }

    fn from_buildin(wk: BuildinOp) -> Self {
        match wk {
            BuildinOp::Dot => BackendOp::SpirvOp(SpvOp::CoreOp(CoreOp::Dot)),
            BuildinOp::Cross => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Cross)),
            BuildinOp::Length => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Length)),
            BuildinOp::SquareRoot => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Sqrt)),
            BuildinOp::Exp => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Exp)),
            BuildinOp::Pow => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Pow)),
            BuildinOp::Min => BackendOp::HlOp(HlOp::Min),
            BuildinOp::Max => BackendOp::HlOp(HlOp::Max),
            BuildinOp::Mix => BackendOp::HlOp(HlOp::Mix),
            BuildinOp::Clamp => BackendOp::HlOp(HlOp::Clamp),
        }
    }

    fn from_const_index(fac: &ConstantIndex) -> Self {
        Self::SpirvOp(SpvOp::Extract(smallvec![fac.access as u32]))
    }

    fn from_uniform_construct(_lc: &UniformConstruct) -> Self {
        Self::SpirvOp(SpvOp::UniformConstruct)
    }

    fn from_non_uniform_construct(_lc: &NonUniformConstruct) -> Self {
        Self::SpirvOp(SpvOp::NonUniformConstruct)
    }
}
