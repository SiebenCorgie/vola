/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//!Helper for matrix-* operations

use rvsdg::{edge::OutportLocation, region::RegionLocation, NodeRef, SmallColl};
use vola_common::Span;

use crate::{
    alge::{
        buildin::{Buildin, BuildinOp},
        ConstantIndex, Construct,
    },
    common::Ty,
    OptError, OptNode, Optimizer,
};

impl Optimizer {
    pub(super) fn handle_canon_mul(
        &mut self,
        region: &RegionLocation,
        node: NodeRef,
    ) -> Result<(), OptError> {
        //self.push_debug_state(&format!("WhallaBrudi_{node}"));

        //checkout what kind of inputs / outputs we have
        let left_src = self.graph.inport_src(node.input(0)).unwrap();
        let right_src = self.graph.inport_src(node.input(1)).unwrap();

        let left_type = self.get_or_derive_type(left_src);
        let right_type = self.get_or_derive_type(right_src);

        match (left_type.clone(), right_type.clone()) {
            (Ty::Matrix { .. }, Ty::Vector { .. }) => {
                //this is canonicalized into a unrolled multiplication
                let _canon = self
                    .unroll_matrix_vector(region, node, left_type, right_type, left_src, right_src);

                Ok(())
            }
            (Ty::Vector { .. }, Ty::Matrix { .. }) => {
                //this is canonicalized into a unrolled multiplication
                let _canonicalized = self
                    .unroll_vector_matrix(region, node, left_type, right_type, left_src, right_src);

                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn unroll_vector_matrix(
        &mut self,
        region: &RegionLocation,
        mul_node: NodeRef,
        vector_ty: Ty,
        matrix_ty: Ty,
        vector_src: OutportLocation,
        matrix_src: OutportLocation,
    ) -> NodeRef {
        //The Vector-Matrix multiplication implicitly treats the vector as a row-vector, as opposed to a
        //column vector.
        //we now just iterate all columns of the matrix, and multiply each element in the _row-vector_ with the corresbonding element in the
        //column-vector of the matrix.

        //Cause we cool, we prefetch each column of the matrix, and then we fetch the right_collumn's source in the double loop.

        assert!(matrix_ty.height().unwrap() == vector_ty.width().unwrap());

        let span = self.find_span(mul_node.into()).unwrap_or(Span::empty());

        let new_result = self
            .graph
            .on_region(region, |reg| {
                let column_sources: SmallColl<OutportLocation> = (0..matrix_ty.width().unwrap())
                    .map(|col_index| {
                        let (column_src, _) = reg
                            .connect_node(
                                OptNode::new(ConstantIndex::new(col_index), span.clone()),
                                &[matrix_src],
                            )
                            .unwrap();
                        column_src.output(0)
                    })
                    .collect();

                //Now we use the dot product to actually fold the values for each vector element
                let vector_elements: SmallColl<_> = column_sources
                    .iter()
                    .map(|column_src| {
                        //Add a dot product of the vector_dot_column
                        let (dot_res, _) = reg
                            .connect_node(
                                OptNode::new(Buildin::new(BuildinOp::Dot), span.clone()),
                                &[vector_src, *column_src],
                            )
                            .unwrap();
                        dot_res.output(0)
                    })
                    .collect();

                //Finally assemble the vector from those elements
                let (result_vec, _) = reg
                    .connect_node(
                        OptNode::new(
                            Construct::new().with_inputs(vector_elements.len()),
                            span.clone(),
                        ),
                        &vector_elements,
                    )
                    .unwrap();

                result_vec
            })
            .unwrap();

        //now replace the uses of this mul with the newly dot-based vector
        self.graph.replace_node_uses(mul_node, new_result).unwrap();
        new_result
    }

    fn unroll_matrix_vector(
        &mut self,
        region: &RegionLocation,
        mul_node: NodeRef,
        matrix_ty: Ty,
        vector_ty: Ty,
        matrix_src: OutportLocation,
        vector_src: OutportLocation,
    ) -> NodeRef {
        //Similar idea to the vector-matrix multiplication. But this time the the indexing of
        //the matrix is a little harder :/

        assert!(matrix_ty.width().unwrap() == vector_ty.width().unwrap());

        let span = self.find_span(mul_node.into()).unwrap_or(Span::empty());

        let new_result = self
            .graph
            .on_region(region, |reg| {
                let column_sources: SmallColl<OutportLocation> = (0..matrix_ty.width().unwrap())
                    .map(|col_index| {
                        let (column_src, _) = reg
                            .connect_node(
                                OptNode::new(ConstantIndex::new(col_index), span.clone()),
                                &[matrix_src],
                            )
                            .unwrap();
                        column_src.output(0)
                    })
                    .collect();

                let row_vectors: SmallColl<OutportLocation> = (0..matrix_ty.height().unwrap())
                    .map(|row_idx| {
                        //Load all indices of that row into a new vector
                        let row_indices: SmallColl<OutportLocation> =
                            (0..matrix_ty.width().unwrap())
                                .map(|col_idx| {
                                    let (element, _) = reg
                                        .connect_node(
                                            OptNode::new(ConstantIndex::new(row_idx), span.clone()),
                                            &[column_sources[col_idx]],
                                        )
                                        .unwrap();
                                    element.output(0)
                                })
                                .collect();

                        let (row_vector, _) = reg
                            .connect_node(
                                OptNode::new(
                                    Construct::new().with_inputs(row_indices.len()),
                                    span.clone(),
                                ),
                                &row_indices,
                            )
                            .unwrap();
                        row_vector.output(0)
                    })
                    .collect();

                //Now we use the dot product to actually fold the values for each vector element
                let vector_elements: SmallColl<_> = row_vectors
                    .into_iter()
                    .map(|row_src| {
                        //Add a dot product of the vector_dot_column
                        let (dot_res, _) = reg
                            .connect_node(
                                OptNode::new(Buildin::new(BuildinOp::Dot), span.clone()),
                                &[row_src, vector_src],
                            )
                            .unwrap();
                        dot_res.output(0)
                    })
                    .collect();

                //Finally assemble the vector from those elements
                let (result_vec, _) = reg
                    .connect_node(
                        OptNode::new(
                            Construct::new().with_inputs(vector_elements.len()),
                            span.clone(),
                        ),
                        &vector_elements,
                    )
                    .unwrap();

                result_vec
            })
            .unwrap();

        //now replace the uses of this mul with the newly dot-based vector
        self.graph.replace_node_uses(mul_node, new_result).unwrap();
        new_result
    }
}
