/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Pass that destructs any immediate value that is not `ImmScalar` or `ImmNat`
//! into a tree of just those two and `UniformConstruct` nodes.

use rvsdg::{nodes::NodeType, region::RegionLocation, NodeRef, SmallColl};
use vola_common::Span;

use crate::{
    common::{DataType, Ty},
    imm::{ImmMatrix, ImmScalar, ImmVector},
    typelevel::UniformConstruct,
    OptError, OptNode, Optimizer,
};

impl Optimizer {
    pub fn imm_scalarize(&mut self) -> Result<(), OptError> {
        //The idea is, that we traverse the graph in reverse-topological order whenever we encounter a
        //ImmVector or ImmMatrix, we call the scalarizer on that subtree. They'll take care of replacing the nodes.

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_BEFORE_SCALARIZE").is_ok()
        {
            self.push_debug_state("pre imm scalarize");
        }

        //First identify any reachable node that needs to be transformed
        let to_be_transformed_nodes = self
            .graph
            .walk_reachable()
            .filter_map(|node| {
                if let NodeType::Simple(s) = &self.graph.node(node).node_type {
                    if s.try_downcast_ref::<ImmMatrix>().is_some()
                        || s.try_downcast_ref::<ImmVector>().is_some()
                    {
                        Some(node)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        //now call the destructor on each
        for n in to_be_transformed_nodes {
            self.scalarize_node(n)?;
        }

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_AFTER_SCALARIZE").is_ok() {
            self.push_debug_state("pre post scalarize");
        }

        Ok(())
    }

    fn scalarize_node(&mut self, node: NodeRef) -> Result<(), OptError> {
        enum ImmTy {
            Vec(SmallColl<f64>),
            Matrix(SmallColl<SmallColl<f64>>),
        }

        let immval = if let NodeType::Simple(s) = &self.graph.node(node).node_type {
            if let Some(imm) = s.try_downcast_ref::<ImmVector>() {
                ImmTy::Vec(imm.lit.clone())
            } else {
                if let Some(imm) = s.try_downcast_ref::<ImmMatrix>() {
                    ImmTy::Matrix(imm.lit.clone())
                } else {
                    return Err(OptError::Internal(format!(
                        "Scalarizing {node} failed, was no ImmVector nor ImmMatrix!"
                    )));
                }
            }
        } else {
            return Err(OptError::Internal(format!(
                "Scalarizing {node} failed, was no SimpleNode!"
            )));
        };

        //now build the subgraph
        let region = self
            .graph
            .node(node)
            .parent
            .ok_or(OptError::Internal(format!("Node was in parenless region!")))?;

        //NOTE: we don't need to type the edges, since replace_node_uses takes care of copying over the actual type.
        let replacement_node = match immval {
            ImmTy::Matrix(m) => self.build_construct_imm_mat(region, m),
            ImmTy::Vec(v) => self.build_construct_imm_vec(region, v),
        };

        //finally replace the node and return
        self.graph.replace_node_uses(node, replacement_node)?;

        Ok(())
    }

    fn build_construct_imm_vec(
        &mut self,
        region: RegionLocation,
        immvalues: SmallColl<f64>,
    ) -> NodeRef {
        self.graph
            .on_region(&region, |reg| {
                let mut const_srcs = SmallColl::with_capacity(immvalues.len());
                let input_count = immvalues.len();
                for element in immvalues {
                    let node_port = reg
                        .insert_node(OptNode::new(ImmScalar::new(element), Span::empty()))
                        .output(0);
                    const_srcs.push(node_port);
                }

                //now add the construct node and bail
                let (node, connections) = reg
                    .connect_node(
                        OptNode::new(
                            UniformConstruct::new().with_inputs(input_count),
                            Span::empty(),
                        ),
                        const_srcs,
                    )
                    .unwrap();
                for connection in connections {
                    reg.ctx_mut()
                        .edge_mut(connection)
                        .ty
                        .set_type(Ty::SCALAR_REAL);
                }

                node
            })
            .unwrap()
    }

    fn build_construct_imm_mat(
        &mut self,
        region: RegionLocation,
        immvalue: SmallColl<SmallColl<f64>>,
    ) -> NodeRef {
        let mut vec_construct_srcs = SmallColl::with_capacity(immvalue.len());
        let vec_width = immvalue[0].len();
        let column_count = immvalue.len();

        for column in immvalue {
            let node = self.build_construct_imm_vec(region, column);
            vec_construct_srcs.push(node.output(0));
        }

        let (mat_construct_node, edges) = self
            .graph
            .on_region(&region, |reg| {
                reg.connect_node(
                    OptNode::new(
                        UniformConstruct::new().with_inputs(column_count),
                        Span::empty(),
                    ),
                    vec_construct_srcs,
                )
                .expect("Failed to create mat-construct")
            })
            .unwrap();

        for edge in edges {
            self.graph
                .edge_mut(edge)
                .ty
                .set_type(Ty::vector_type(DataType::Real, vec_width))
                .unwrap();
        }

        mat_construct_node
    }
}
