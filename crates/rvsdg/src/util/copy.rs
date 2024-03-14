//! Some copy utilities when working with the graph
use ahash::AHashMap;
use smallvec::smallvec;

use crate::{
    edge::LangEdge,
    nodes::{
        ApplyNode, DeltaNode, GammaNode, LambdaNode, LangNode, Node, NodeType, OmegaNode, PhiNode,
        ThetaNode,
    },
    region::{Input, Output, RegionLocation},
    NodeRef, Rvsdg,
};

///Similar to standard clone, but expects the implementer to not hook-up any connections, and do not _fill_
/// sub regions.
/// Used in the _copy_ implementations.
pub trait StructuralClone {
    fn structural_copy(&self) -> Self;
}

impl StructuralClone for ApplyNode {
    fn structural_copy(&self) -> Self {
        ApplyNode {
            inputs: smallvec![Input::default(); self.inputs.len()],
            outputs: smallvec![Output::default(); self.outputs.len()],
        }
    }
}

impl StructuralClone for DeltaNode {
    fn structural_copy(&self) -> Self {
        DeltaNode {
            cv_count: self.cv_count,
            body: self.body.copy_config(),
            inputs: smallvec![Input::default(); self.inputs.len()],
            output: Output::default(),
        }
    }
}

impl StructuralClone for GammaNode {
    fn structural_copy(&self) -> Self {
        GammaNode {
            entry_var_count: self.entry_var_count,
            exit_var_count: self.exit_var_count,
            regions: self.regions.iter().map(|reg| reg.copy_config()).collect(),
            inputs: smallvec![Input::default(); self.inputs.len()],
            outputs: smallvec![Output::default(); self.outputs.len()],
        }
    }
}

impl StructuralClone for ThetaNode {
    fn structural_copy(&self) -> Self {
        ThetaNode {
            lv_count: self.lv_count,
            loop_body: self.loop_body.copy_config(),
            inputs: smallvec![Input::default(); self.inputs.len()],
            outputs: smallvec![Output::default(); self.outputs.len()],
        }
    }
}

impl StructuralClone for LambdaNode {
    fn structural_copy(&self) -> Self {
        LambdaNode {
            cv_count: self.cv_count,
            inputs: smallvec![Input::default(); self.inputs.len()],
            output: Output::default(),
            body: self.body.copy_config(),
        }
    }
}

impl StructuralClone for PhiNode {
    fn structural_copy(&self) -> Self {
        PhiNode {
            cv_count: self.cv_count,
            rv_count: self.rv_count,
            body: self.body.copy_config(),
            inputs: smallvec![Input::default(); self.inputs.len()],
            outputs: smallvec![Output::default(); self.outputs.len()],
        }
    }
}

impl StructuralClone for OmegaNode {
    fn structural_copy(&self) -> Self {
        OmegaNode {
            body: self.body.copy_config(),
        }
    }
}

impl<N: LangNode + StructuralClone + 'static> StructuralClone for Node<N> {
    fn structural_copy(&self) -> Self {
        Node {
            node_type: self.node_type.structural_copy(),
            parent: None,
        }
    }
}

impl<N: LangNode + StructuralClone + 'static> StructuralClone for NodeType<N> {
    fn structural_copy(&self) -> Self {
        match self {
            Self::Apply(a) => Self::Apply(a.structural_copy()),
            Self::Delta(a) => Self::Delta(a.structural_copy()),
            Self::Gamma(a) => Self::Gamma(a.structural_copy()),
            Self::Theta(a) => Self::Theta(a.structural_copy()),
            Self::Lambda(a) => Self::Lambda(a.structural_copy()),
            Self::Phi(a) => Self::Phi(a.structural_copy()),
            Self::Omega(a) => Self::Omega(a.structural_copy()),
            Self::Simple(a) => Self::Simple(a.structural_copy()),
        }
    }
}

impl<N: LangNode + StructuralClone + 'static, E: LangEdge + StructuralClone + 'static> Rvsdg<N, E> {
    ///Copies `src_node` into a new node. If the `src_node` had any sub regions, copies the region's
    /// configuration, but not the content.
    pub fn shallow_copy_node(&mut self, src_node: NodeRef, dst_region: RegionLocation) -> NodeRef {
        let new_node = self.node(src_node).structural_copy();
        //now unwrap the inner type, and add that to a new registered node in dst_region
        let node = self
            .on_region(&dst_region, |reg| reg.add_node_type(new_node.node_type))
            .unwrap();
        node
    }

    ///_Deep copies_ `node` to `dst_region`.
    /// The result will be a node that is not connected itself, but carries an exact copy of all internal
    /// regions (if there are any). So if you call `deep_copy_node` on a λ-Node, you'll end up with an copy of an already existing
    /// λ-Node.
    ///
    /// Panics if `node` has no parent region.
    ///
    /// NOTE: The copy won't detect any unconnected nodes in a region that is copied.
    pub fn deep_copy_node(&mut self, node: NodeRef, dst_region: RegionLocation) -> NodeRef {
        //first shallow copy node, then, if there are any sub_regions, deep_copy regions

        let shallow_copy = self.shallow_copy_node(node, dst_region);
        let subreg_count = self.node(node).regions().len();
        //Just to be sure
        assert!(self.node(shallow_copy).regions().len() == subreg_count);

        for subreg_idx in 0..subreg_count {
            let src_reg = RegionLocation {
                node,
                region_index: subreg_idx,
            };
            let dst_reg = RegionLocation {
                node: shallow_copy,
                region_index: subreg_idx,
            };
            self.deep_copy_region(src_reg, dst_reg);
        }
        //finished covering all subregions, so we can return actually _as deep copy_
        shallow_copy
    }

    ///Deep copies all nodes of `region` into a new region, and hooks them up exactly the same way.
    ///
    /// NOTE that we expected `src_region` to have the exact same argument/result conifguration as `dst_region`,
    /// otherwise this panics.
    pub fn deep_copy_region(&mut self, src_region: RegionLocation, dst_region: RegionLocation) {
        assert!(
            self.region(&src_region).unwrap().arguments.len()
                == self.region(&dst_region).unwrap().arguments.len(),
            "Argument count does not match"
        );
        assert!(
            self.region(&src_region).unwrap().results.len()
                == self.region(&dst_region).unwrap().results.len(),
            "Result count does not match"
        );

        let mut node_mapping = AHashMap::default();
        //pre-insert the region_parents
        node_mapping.insert(src_region.node, dst_region.node);

        let src_nodes = self.region(&src_region).unwrap().nodes.clone();

        //first task is deep-copying all nodes over to the new region
        for srcnode in src_nodes {
            //copy the node, clear inputs and outputs, and possibly recurse if needed.
            let dstnode = self.deep_copy_node(srcnode, dst_region);
            //now add to the mapping
            node_mapping.insert(srcnode, dstnode);
        }

        //After copying all nodes, hook up the edges as well
        let src_edges = self.region(&src_region).unwrap().edges.clone();
        for src_edge in src_edges {
            //Rebuild all edges by replacing the src/dst node as described by the node mapping.
            //
            let (new_src, new_dst, tycpy) = {
                let (mut original_src, mut original_dst, original_ty) = {
                    let edg = self.edge(src_edge);
                    (edg.src, edg.dst, edg.ty.structural_copy())
                };
                //replace src and dst
                original_src.node = *node_mapping.get(&original_src.node).unwrap();
                original_dst.node = *node_mapping.get(&original_dst.node).unwrap();
                (original_src, original_dst, original_ty)
            };

            self.connect(new_src, new_dst, tycpy).unwrap();
        }
    }
}
