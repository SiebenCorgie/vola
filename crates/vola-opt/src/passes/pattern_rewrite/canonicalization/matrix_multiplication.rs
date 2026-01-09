/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2026 Tendsin Mende
 */

//!Helper for matrix-* operations

use rvsdg::{edge::OutportLocation, region::RegionLocation, NodeRef, SmallColl};
use rvsdg_pattern_rewrite::{CodeSize, PatternRewrite};
use vola_common::Span;

use crate::{
    alge::{
        arithmetic::{BinaryArith, BinaryArithOp},
        buildin::{Buildin, BuildinOp},
    },
    common::{DataType, Shape, Ty},
    typelevel::{ConstantIndex, UniformConstruct},
    OptEdge, OptNode, Optimizer,
};

///Pattern that unrolls non trivial multiplications (i.e. vector-matrix, matrix-vector, matrix-matrix etc.)
/// into a set of scalar multiplications and the associated destruction and construction routines to end up
/// at the same value
pub struct UnrollMul;
impl PatternRewrite<OptNode, OptEdge, Optimizer, CodeSize> for UnrollMul {
    fn matches(&self, ctx: &Optimizer, node: NodeRef) -> bool {
        //Early bail if this is no multiplication
        if let Some(binary) = ctx.try_unwrap_node::<BinaryArith>(node) {
            if binary.op != BinaryArithOp::Mul {
                return false;
            }
        } else {
            return false;
        }

        //checkout what kind of inputs / outputs we have
        let left_src = ctx.graph.inport_src(node.input(0)).unwrap();
        let right_src = ctx.graph.inport_src(node.input(1)).unwrap();

        let left_type = ctx.get_out_type(left_src).unwrap();
        let right_type = ctx.get_out_type(right_src).unwrap();

        match (left_type.clone(), right_type.clone()) {
            (
                Ty::Shaped {
                    shape: Shape::Matrix { .. },
                    ty: DataType::Real,
                },
                Ty::Shaped {
                    shape: Shape::Vec { .. },
                    ty: DataType::Real,
                },
            ) => true,
            (
                Ty::Shaped {
                    shape: Shape::Vec { .. },
                    ty: DataType::Real,
                },
                Ty::Shaped {
                    shape: Shape::Matrix { .. },
                    ty: DataType::Real,
                },
            ) => true,
            (
                Ty::Shaped {
                    shape: Shape::Matrix { .. },
                    ty: DataType::Real,
                },
                Ty::Shaped {
                    shape: Shape::Matrix { .. },
                    ty: DataType::Real,
                },
            ) => true,
            _ => false,
        }
    }

    fn apply(&self, ctx: &mut Optimizer, node: NodeRef) {
        #[cfg(feature = "log")]
        log::info!("Unrolling Mul {node}");

        let left_src = ctx.graph.inport_src(node.input(0)).unwrap();
        let right_src = ctx.graph.inport_src(node.input(1)).unwrap();

        let left_type = ctx.get_out_type(left_src).unwrap();
        let right_type = ctx.get_out_type(right_src).unwrap();

        let region = ctx.graph[node].parent.unwrap();

        //checkout which of the three we are and apply the unroller accordingly
        let new_node = if left_type.is_vector() && right_type.is_matrix() {
            Self::unroll_vector_matrix(
                ctx, &region, node, left_type, right_type, left_src, right_src,
            )
        } else if left_type.is_matrix() && right_type.is_vector() {
            Self::unroll_matrix_vector(
                ctx, &region, node, left_type, right_type, left_src, right_src,
            )
        } else if left_type.is_matrix() && right_type.is_matrix() {
            Self::unroll_matrix_matrix(
                ctx, &region, node, left_type, right_type, left_src, right_src,
            )
        } else {
            panic!("Detected node combination that shouldn't trigger the rewrite")
        };

        log::info!("Unrolled multiplication {node} to {new_node}");

        ctx.graph.replace_node_uses(node, new_node).unwrap();
    }

    fn benefit(&self) -> &CodeSize {
        &CodeSize(1)
    }
}

impl UnrollMul {
    fn unroll_vector_matrix(
        ctx: &mut Optimizer,
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

        //In the vector-matrix case we can omit the row-vector element pre fetching that is used in the matrix-matrix case. Instead
        //we can directly _dot_ the matrix

        assert!(matrix_ty.height().unwrap() == vector_ty.width().unwrap());

        let span = ctx.find_span(mul_node).unwrap_or(Span::empty());

        let new_result = ctx
            .graph
            .on_region(region, |reg| {
                let column_sources: SmallColl<OutportLocation> = (0..matrix_ty.width().unwrap())
                    .map(|col_index| {
                        let (column_src, _) = reg
                            .connect_node(
                                OptNode::new(ConstantIndex::new(col_index), span.clone()),
                                [matrix_src],
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
                                [vector_src, *column_src],
                            )
                            .unwrap();
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

                result_vec
            })
            .unwrap();

        new_result
    }
    ///Unrolls a matrix-vector multiplication into multiple dot product operations.
    fn unroll_matrix_vector(
        ctx: &mut Optimizer,
        region: &RegionLocation,
        mul_node: NodeRef,
        left_ty: Ty,
        right_ty: Ty,
        left_src: OutportLocation,
        right_src: OutportLocation,
    ) -> NodeRef {
        assert!(left_ty.width().unwrap() == right_ty.width().unwrap());

        let span = ctx.find_span(mul_node).unwrap_or(Span::empty());

        let new_result = ctx
            .graph
            .on_region(region, |reg| {
                let column_sources: SmallColl<OutportLocation> = (0..left_ty.width().unwrap())
                    .map(|col_index| {
                        let (column_src, _) = reg
                            .connect_node(
                                OptNode::new(ConstantIndex::new(col_index), span.clone()),
                                [left_src],
                            )
                            .unwrap();
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

                result_vec
            })
            .unwrap();

        new_result
    }

    ///Unrolls a matrix-matrix multiplication into multiple dot product operations.
    ///
    ///NOTE: This also works for Matrix-Vector and Vector-Matrix operations, since vectors can be treated as
    ///      single-column/single-row matrixes.
    fn unroll_matrix_matrix(
        ctx: &mut Optimizer,
        region: &RegionLocation,
        mul_node: NodeRef,
        left_ty: Ty,
        right_ty: Ty,
        left_src: OutportLocation,
        right_src: OutportLocation,
    ) -> NodeRef {
        assert!(left_ty.width().unwrap() == right_ty.height().unwrap());

        let span = ctx.find_span(mul_node).unwrap_or(Span::empty());

        let new_result = ctx
            .graph
            .on_region(region, |reg| {
                let column_sources: SmallColl<OutportLocation> = (0..left_ty.width().unwrap())
                    .map(|col_index| {
                        let (column_src, _) = reg
                            .connect_node(
                                OptNode::new(ConstantIndex::new(col_index), span.clone()),
                                [left_src],
                            )
                            .unwrap();
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

                result_vec
            })
            .unwrap();

        new_result
    }
}
