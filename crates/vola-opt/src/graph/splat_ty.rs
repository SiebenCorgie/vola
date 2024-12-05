use rvsdg::{edge::OutportLocation, region::RegionLocation, smallvec::smallvec, SmallColl};
use vola_common::Span;

use crate::{
    alge::Construct,
    common::{DataType, Shape, Ty},
    imm::ImmScalar,
    Optimizer,
};

use super::OptNode;

/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

impl Optimizer {
    ///Splats the value originating from `value` into the shape of `Ty`.
    pub fn splat_scalar(
        &mut self,
        region: RegionLocation,
        value: ImmScalar,
        ty: Ty,
    ) -> OutportLocation {
        match ty {
            Ty::SCALAR_REAL => {
                //already in correct type, add node to graph and return output
                self.graph
                    .on_region(&region, |g| {
                        g.insert_node(OptNode::new(value, Span::empty())).output(0)
                    })
                    .unwrap()
            }
            Ty::Shaped {
                ty: DataType::Real,
                shape: Shape::Vec { width },
            } => {
                //recurse for scalar, and assemble for such a vector
                let scalar_src = self.splat_scalar(region, value, Ty::SCALAR_REAL);

                self.graph
                    .on_region(&region, |g| {
                        let src_array: SmallColl<OutportLocation> = smallvec![scalar_src; width];
                        let (constructor, _) = g
                            .connect_node(
                                OptNode::new(Construct::new().with_inputs(width), Span::empty()),
                                &src_array,
                            )
                            .unwrap();

                        constructor.output(0)
                    })
                    .unwrap()
            }
            Ty::Shaped {
                ty: DataType::Real,
                shape: Shape::Matrix { width, height },
            } => {
                //build a column and splat it for width
                let column_src =
                    self.splat_scalar(region, value, Ty::vector_type(DataType::Real, height));

                self.graph
                    .on_region(&region, |g| {
                        let src_array: SmallColl<_> = smallvec![column_src; width];
                        let (constructor, _) = g
                            .connect_node(
                                OptNode::new(Construct::new().with_inputs(width), Span::empty()),
                                &src_array,
                            )
                            .unwrap();

                        constructor.output(0)
                    })
                    .unwrap()
            }
            Ty::Shaped {
                ty: DataType::Real,
                shape: Shape::Tensor { sizes: mut dim },
            } => match dim.len() {
                0 => self.splat_scalar(region, value, Ty::SCALAR_REAL),
                1 => self.splat_scalar(region, value, Ty::vector_type(DataType::Real, dim[0])),
                2 => self.splat_scalar(
                    region,
                    value,
                    Ty::matrix_type(DataType::Real, dim[1], dim[0]),
                ),
                _other => {
                    let poped_dim = dim.pop().unwrap();
                    let src = self.splat_scalar(
                        region,
                        value,
                        Ty::Shaped {
                            ty: DataType::Real,
                            shape: Shape::Tensor { sizes: dim },
                        },
                    );

                    //build tensor from dim

                    self.graph
                        .on_region(&region, |g| {
                            let src_array: SmallColl<_> = smallvec![src; poped_dim];
                            let (constructor, _) = g
                                .connect_node(
                                    OptNode::new(
                                        Construct::new().with_inputs(poped_dim),
                                        Span::empty(),
                                    ),
                                    &src_array,
                                )
                                .unwrap();

                            constructor.output(0)
                        })
                        .unwrap()
                }
            },
            other => panic!("Wrong type: {other}"),
        }
    }
}
