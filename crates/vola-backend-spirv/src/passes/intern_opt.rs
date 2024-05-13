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
use vola_opt::{
    alge::{CallOp, ConstantIndex, Construct, ImmNat, ImmScalar, WkOp},
    OptEdge, OptNode, Optimizer,
};

use crate::{
    graph::{BackendEdge, BackendNode, BackendOp},
    hl::HlOp,
    spv::{CoreOp, GlOp, SpvOp, SpvType},
    SpirvBackend,
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
            OptEdge::Value { ty } => BackendEdge::Value(
                ty.get_type()
                    .map(|t| {
                        t.try_into().expect(
                            "Failed to convert opt-edge type to SPIR-V type. This indicates a bug!",
                        )
                    })
                    .unwrap_or(SpvType::undefined()),
            ),
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
        BackendOp::SpirvOp(SpvOp::ConstantFloat {
            resolution: 32,
            bits: (imm.lit as f32).to_bits(),
        })
    }

    fn from_imm_nat(imm: &ImmNat) -> Self {
        BackendOp::SpirvOp(SpvOp::ConstantInt {
            resolution: 32,
            bits: (imm.lit as u32).to_be(),
        })
    }

    fn from_wk(wk: &WkOp) -> Self {
        match wk {
            WkOp::Not => BackendOp::SpirvOp(SpvOp::CoreOp(CoreOp::Not)),
            //NOTE: Since we just have floats, we can just use FNegate
            WkOp::Neg => BackendOp::HlOp(HlOp::Negate),
            WkOp::Add => BackendOp::HlOp(HlOp::Add),
            WkOp::Sub => BackendOp::HlOp(HlOp::Sub),
            WkOp::Mul => BackendOp::HlOp(HlOp::Mul),
            WkOp::Div => BackendOp::HlOp(HlOp::Div),
            WkOp::Mod => BackendOp::HlOp(HlOp::Mod),

            WkOp::Lt => BackendOp::HlOp(HlOp::Lt),
            WkOp::Gt => BackendOp::HlOp(HlOp::Gt),
            WkOp::Lte => BackendOp::HlOp(HlOp::Lte),
            WkOp::Gte => BackendOp::HlOp(HlOp::Gte),
            WkOp::Eq => BackendOp::HlOp(HlOp::Eq),
            WkOp::NotEq => BackendOp::HlOp(HlOp::Neq),

            WkOp::And => BackendOp::SpirvOp(SpvOp::CoreOp(CoreOp::LogicalAnd)),
            WkOp::Or => BackendOp::SpirvOp(SpvOp::CoreOp(CoreOp::LogicalOr)),

            WkOp::Dot => BackendOp::SpirvOp(SpvOp::CoreOp(CoreOp::Dot)),
            WkOp::Cross => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Cross)),
            WkOp::Length => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Length)),
            WkOp::SquareRoot => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Sqrt)),
            WkOp::Exp => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Exp)),
            WkOp::Min => BackendOp::HlOp(HlOp::Min),
            WkOp::Max => BackendOp::HlOp(HlOp::Max),
            WkOp::Mix => BackendOp::HlOp(HlOp::Mix),
            WkOp::Clamp => BackendOp::HlOp(HlOp::Clamp),
            WkOp::Abs => BackendOp::HlOp(HlOp::Abs),
            WkOp::Fract => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Fract)),
            WkOp::Round => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Round)),
            WkOp::Ceil => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Ceil)),
            WkOp::Floor => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Floor)),
            WkOp::Sin => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Sin)),
            WkOp::Cos => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Cos)),
            WkOp::Tan => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Tan)),
            WkOp::ASin => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Asin)),
            WkOp::ACos => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Acos)),
            WkOp::ATan => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::Atan)),
            WkOp::Inverse => BackendOp::SpirvOp(SpvOp::GlslOp(GlOp::MatrixInverse)),
        }
    }

    fn from_const_index(fac: &ConstantIndex) -> Self {
        Self::SpirvOp(SpvOp::Extract(smallvec![fac.access as u32]))
    }

    fn from_construct(_lc: &Construct) -> Self {
        Self::SpirvOp(SpvOp::Construct)
    }
}
