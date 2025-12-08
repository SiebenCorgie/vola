/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

//! Implements the multiplication unrolling for vector/matrix multiplication

use rvsdg::{edge::OutportLocation, NodeRef, SmallColl};
use vola_common::Span;

use crate::{
    alge::buildin::{Buildin, BuildinOp},
    common::Ty,
    typelevel::{ConstantIndex, UniformConstruct},
    OptNode,
};

use crate::util::simplify::Simplify;
impl<'a> Simplify<'a> {
    pub(super) fn unroll_vector_matrix(
        self,
        vector_ty: Ty,
        matrix_ty: Ty,
        vector_src: OutportLocation,
        matrix_src: OutportLocation,
    ) -> Option<Vec<NodeRef>> {
        //The Vector-Matrix multiplication implicitly treats the vector as a row-vector, as opposed to a
        //column vector.
        //we now just iterate all columns of the matrix, and multiply each element in the _row-vector_ with the corresbonding element in the
        //column-vector of the matrix.

        //In the vector-matrix case we can omit the row-vector element pre fetching that is used in the matrix-matrix case. Instead
        //we can directly _dot_ the matrix

        let region = self.opt.graph[self.node].parent.unwrap();
        let mul_node = self.node;

        assert!(matrix_ty.height().unwrap() == vector_ty.width().unwrap());

        let span = self.opt.find_span(mul_node).unwrap_or(Span::empty());
        let mut node_collector = Vec::new();

        let new_result = self
            .opt
            .graph
            .on_region(&region, |reg| {
                let column_sources: SmallColl<OutportLocation> = (0..matrix_ty.width().unwrap())
                    .map(|col_index| {
                        let (column_src, _) = reg
                            .connect_node(
                                OptNode::new(ConstantIndex::new(col_index), span.clone()),
                                [matrix_src],
                            )
                            .unwrap();
                        node_collector.push(column_src);
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
                                [vector_src, *column_src],
                            )
                            .unwrap();
                        node_collector.push(dot_res);
                        dot_res.output(0)
                    })
                    .collect();

                //Finally assemble the vector from those elements
                let (result_vec, _) = reg
                    .connect_node(
                        OptNode::new(
                            UniformConstruct::new().with_inputs(vector_elements.len()),
                            span.clone(),
                        ),
                        vector_elements,
                    )
                    .unwrap();
                node_collector.push(result_vec);

                result_vec
            })
            .unwrap();

        //now replace the uses of this mul with the newly dot-based vector
        self.opt
            .graph
            .replace_node_uses(mul_node, new_result)
            .unwrap();
        Some(node_collector)
    }
    ///Unrolls a matrix-vector multiplication into multiple dot product operations.
    pub(super) fn unroll_matrix_vector(
        self,
        left_ty: Ty,
        right_ty: Ty,
        left_src: OutportLocation,
        right_src: OutportLocation,
    ) -> Option<Vec<NodeRef>> {
        let region = self.opt.graph[self.node].parent.unwrap();
        let mul_node = self.node;
        assert!(left_ty.width().unwrap() == right_ty.width().unwrap());

        let span = self.opt.find_span(mul_node).unwrap_or(Span::empty());
        let mut node_collector = Vec::new();

        let new_result = self
            .opt
            .graph
            .on_region(&region, |reg| {
                let column_sources: SmallColl<OutportLocation> = (0..left_ty.width().unwrap())
                    .map(|col_index| {
                        let (column_src, _) = reg
                            .connect_node(
                                OptNode::new(ConstantIndex::new(col_index), span.clone()),
                                [left_src],
                            )
                            .unwrap();
                        node_collector.push(column_src);
                        column_src.output(0)
                    })
                    .collect();

                let row_vectors: SmallColl<OutportLocation> = (0..left_ty.height().unwrap())
                    .map(|row_idx| {
                        //Load all indices of that row into a new vector
                        let row_indices: SmallColl<OutportLocation> = (0..left_ty.width().unwrap())
                            .map(|col_idx| {
                                let (element, _) = reg
                                    .connect_node(
                                        OptNode::new(ConstantIndex::new(row_idx), span.clone()),
                                        [column_sources[col_idx]],
                                    )
                                    .unwrap();

                                node_collector.push(element);
                                element.output(0)
                            })
                            .collect();

                        let (row_vector, _) = reg
                            .connect_node(
                                OptNode::new(
                                    UniformConstruct::new().with_inputs(row_indices.len()),
                                    span.clone(),
                                ),
                                row_indices,
                            )
                            .unwrap();
                        node_collector.push(row_vector);
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
                                [row_src, right_src],
                            )
                            .unwrap();
                        node_collector.push(dot_res);
                        dot_res.output(0)
                    })
                    .collect();

                //Finally assemble the vector from those elements
                let (result_vec, _) = reg
                    .connect_node(
                        OptNode::new(
                            UniformConstruct::new().with_inputs(vector_elements.len()),
                            span.clone(),
                        ),
                        vector_elements,
                    )
                    .unwrap();
                node_collector.push(result_vec);

                result_vec
            })
            .unwrap();

        //now replace the uses of this mul with the newly dot-based vector
        self.opt
            .graph
            .replace_node_uses(mul_node, new_result)
            .unwrap();
        Some(node_collector)
    }

    ///Unrolls a matrix-matrix multiplication into multiple dot product operations.
    ///
    ///NOTE: This also works for Matrix-Vector and Vector-Matrix operations, since vectors can be treated as
    ///      single-column/single-row matrixes.
    pub(super) fn unroll_matrix_matrix(
        self,
        left_ty: Ty,
        right_ty: Ty,
        left_src: OutportLocation,
        right_src: OutportLocation,
    ) -> Option<Vec<NodeRef>> {
        let region = self.opt.graph[self.node].parent.unwrap();
        let mul_node = self.node;
        assert!(left_ty.width().unwrap() == right_ty.height().unwrap());

        let span = self.opt.find_span(mul_node).unwrap_or(Span::empty());
        let mut node_collector = Vec::new();

        let new_result = self
            .opt
            .graph
            .on_region(&region, |reg| {
                let column_sources: SmallColl<OutportLocation> = (0..left_ty.width().unwrap())
                    .map(|col_index| {
                        let (column_src, _) = reg
                            .connect_node(
                                OptNode::new(ConstantIndex::new(col_index), span.clone()),
                                [left_src],
                            )
                            .unwrap();
                        node_collector.push(column_src);
                        column_src.output(0)
                    })
                    .collect();

                let row_vectors: SmallColl<OutportLocation> = (0..left_ty.height().unwrap())
                    .map(|row_idx| {
                        //Load all indices of that row into a new vector
                        let row_indices: SmallColl<OutportLocation> = (0..left_ty.width().unwrap())
                            .map(|col_idx| {
                                let (element, _) = reg
                                    .connect_node(
                                        OptNode::new(ConstantIndex::new(row_idx), span.clone()),
                                        [column_sources[col_idx]],
                                    )
                                    .unwrap();
                                node_collector.push(element);
                                element.output(0)
                            })
                            .collect();

                        let (row_vector, _) = reg
                            .connect_node(
                                OptNode::new(
                                    UniformConstruct::new().with_inputs(row_indices.len()),
                                    span.clone(),
                                ),
                                row_indices,
                            )
                            .unwrap();
                        node_collector.push(row_vector);
                        row_vector.output(0)
                    })
                    .collect();

                let right_colmuns: SmallColl<OutportLocation> = (0..right_ty.width().unwrap())
                    .map(|col_idx| {
                        //add a index into the right value's columns
                        let (element, _) = reg
                            .connect_node(
                                OptNode::new(ConstantIndex::new(col_idx), span.clone()),
                                [right_src],
                            )
                            .unwrap();
                        node_collector.push(element);
                        element.output(0)
                    })
                    .collect();

                //Now we do the double loop that uses dot to calculate each new element in the result matrix
                let mut result_columns = SmallColl::new();
                for column_idx in 0..column_sources.len() {
                    let mut result_column_elements: SmallColl<OutportLocation> = SmallColl::new();
                    for row_idx in 0..row_vectors.len() {
                        let (element, _) = reg
                            .connect_node(
                                OptNode::new(Buildin::new(BuildinOp::Dot), span.clone()),
                                [row_vectors[row_idx], right_colmuns[column_idx]],
                            )
                            .unwrap();
                        node_collector.push(element);
                        result_column_elements.push(element.output(0));
                    }
                    //after finishing this column's vector elements, construct the actual vector that make the column
                    let (col_vec, _) = reg
                        .connect_node(
                            OptNode::new(
                                UniformConstruct::new().with_inputs(result_column_elements.len()),
                                span.clone(),
                            ),
                            result_column_elements,
                        )
                        .unwrap();
                    node_collector.push(col_vec);
                    result_columns.push(col_vec.output(0));
                }

                //Finally assemble the matrix from those columns
                let (result_vec, _) = reg
                    .connect_node(
                        OptNode::new(
                            UniformConstruct::new().with_inputs(result_columns.len()),
                            span.clone(),
                        ),
                        result_columns,
                    )
                    .unwrap();
                node_collector.push(result_vec);

                result_vec
            })
            .unwrap();

        //now replace the uses of this mul with the newly dot-based vector
        self.opt
            .graph
            .replace_node_uses(mul_node, new_result)
            .unwrap();
        Some(node_collector)
    }
}
