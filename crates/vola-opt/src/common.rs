/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use ahash::AHashMap;
use rvsdg::{attrib::AttribStore, edge::OutportLocation, smallvec::SmallVec, NodeRef};
use vola_ast::{
    alge::{FieldAccessor, ImplBlock},
    csg::{CSGConcept, CSGNodeDef},
};

use crate::{error::OptError, OptGraph};

///Optimizer types. Those are the AST types, as well as the higher-order-function like types we use to identify
/// CV-Inputs of nodes. They basically make sure that we connect λ-Nodes with the right output type _when called_.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    //Shouldn't be used by the frontend and optimizer. However sometimes we can't (yet?) get around
    //it in the Spirv backend
    Void,
    Scalar,
    Vector {
        width: usize,
    },
    Matrix {
        width: usize,
        height: usize,
    },
    Tensor {
        dim: SmallVec<[usize; 3]>,
    },

    ///A tree of CSG operations in some form.
    CSGTree,

    ///A callable which implements `concept`. Since the namespace of `concepts` is unique, we can be sure
    /// which one it is.
    //TODO: We might want to use some kind of TypeId thing at some point ^^
    Callable {
        concept: String,
    },
}

impl TryFrom<vola_ast::common::Ty> for Ty {
    type Error = OptError;
    fn try_from(value: vola_ast::common::Ty) -> Result<Self, Self::Error> {
        match value {
            vola_ast::common::Ty::Scalar => Ok(Self::Scalar),
            vola_ast::common::Ty::Vec { width } => Ok(Self::Vector { width }),
            vola_ast::common::Ty::Matrix { width, height } => Ok(Self::Matrix { width, height }),
            vola_ast::common::Ty::Tensor { dim } => Ok(Self::Tensor { dim }),
            vola_ast::common::Ty::CSGTree => Err(OptError::TypeConversionError {
                srcty: vola_ast::common::Ty::CSGTree,
            }),
        }
    }
}

impl Ty {
    ///Returns true for scalar, vector, matrix and tensor_type
    pub fn is_algebraic(&self) -> bool {
        match self {
            Self::Scalar | Self::Vector { .. } | Self::Matrix { .. } | Self::Tensor { .. } => true,
            _ => false,
        }
    }

    pub fn is_vector(&self) -> bool {
        if let Self::Vector { .. } = self {
            true
        } else {
            false
        }
    }

    pub(crate) fn try_access_pattern(
        &self,
        access_pattern: &[FieldAccessor],
    ) -> Result<Option<Ty>, OptError> {
        if access_pattern.len() == 0 {
            return Err(OptError::Any {
                text: "empty access pattern is invalid".to_owned(),
            });
        }

        //TODO refactor to recursive sub_slice testting or something.
        match self {
            Ty::Scalar => Err(OptError::Any {
                text: format!("Cannot access field {:?} of a Scalar", access_pattern[0]),
            }),
            Ty::Vector { width } => {
                if access_pattern.len() != 1 {
                    Err(OptError::Any {
                        text: format!(
                            "vector must be accessed with one accessor, like .x or .0, not {}",
                            access_pattern.len()
                        ),
                    })
                } else {
                    //check if we can change to an accessor in vector's width
                    if let Some(idx) = access_pattern[0].try_to_index() {
                        if idx < *width {
                            //yay it worked, in this case the accessed type is always a scalar
                            Ok(Some(Ty::Scalar))
                        } else {
                            //NOTE: yay static bound checking!
                            Err(OptError::Any {
                                text: format!(
                                    "Cannot access element {} on vector with only {} elements",
                                    idx, width
                                ),
                            })
                        }
                    } else {
                        Err(OptError::Any {
                            text: format!(
                                "Could not convert {:?} to access index",
                                access_pattern[0]
                            ),
                        })
                    }
                }
            }
            Ty::Matrix { width, height } => {
                //Connvert the first remaining access to index, if possible, try to propagate to sub-vector type
                if let Some(idx) = access_pattern[0].try_to_index() {
                    //this index selects the _line_ of the matrix, thefore sub-check vector with _width_

                    if idx < *height {
                        let vecty = Ty::Vector { width: *width };
                        if access_pattern[1..].len() > 0 {
                            //subtry
                            vecty.try_access_pattern(&access_pattern[1..])
                        } else {
                            //Was the last access
                            Ok(Some(vecty))
                        }
                    } else {
                        Err(OptError::Any {
                            text: format!(
                                "Cannot access {}-th element of matrix with height {}",
                                idx, height
                            ),
                        })
                    }
                } else {
                    Err(OptError::Any {
                        text: format!("Could not convert {:?} to access index", access_pattern[0]),
                    })
                }
            }
            Ty::Tensor { dim } => {
                //Reduce tensors till either the indices end, or we get into the 2D category, which are
                // matrices.

                //change to matrix check and try again
                match dim.len() {
                    //TODO: handle 0 but currently too tired 😫
                    0 => panic!("encountered zero dimensional tensor"),
                    1 => {
                        //is a vector, try that instead
                        Ty::Vector { width: dim[0] }.try_access_pattern(access_pattern)
                    }
                    2 => {
                        let matrix_type = Ty::Matrix {
                            width: dim[1],
                            height: dim[0],
                        };
                        matrix_type.try_access_pattern(access_pattern)
                    }
                    _x => {
                        //pop of access if valid, check against last dim element. If valid as well, reduce tensor and recurse
                        if let Some(idx) = access_pattern[0].try_to_index() {
                            if idx < dim[0] {
                                //reduce dimension and recurse
                                let new_dim = dim[1..].iter().map(|d| *d).collect();
                                let tensorty = Ty::Tensor { dim: new_dim };
                                tensorty.try_access_pattern(&access_pattern[1..])
                            } else {
                                Err(OptError::Any {
                                    text: format!(
                                        "Cannot access {}-th element of tensor with dimension on axis of {}",
                                        idx, dim[0]
                                    ),
                                })
                            }
                        } else {
                            Err(OptError::Any {
                                text: format!(
                                    "Could not convert {:?} to access index",
                                    access_pattern[0]
                                ),
                            })
                        }
                    }
                }
            }
            _ => Err(OptError::Any {
                text: "FieldAccess must be on scalar, vector, matrix or tensor".to_owned(),
            }),
        }
    }
}

#[derive(Debug)]
pub struct VarDef {
    pub port: OutportLocation,
    pub span: vola_common::Span,
}

///Helper utility that keeps track of defined variables.
#[derive(Debug)]
pub struct LmdContext {
    ///Maps a variable name to an Value outport.
    pub defined_vars: AHashMap<String, VarDef>,
}

impl LmdContext {
    pub fn new_for_impl_block(
        graph: &mut OptGraph,
        type_map: &mut AttribStore<Ty>,
        lmd: NodeRef,
        block: &ImplBlock,
        entity_or_op: &CSGNodeDef,
        concept_def: &CSGConcept,
    ) -> Self {
        let mut defmap = AHashMap::default();

        //first append all _hidden_ operation variables, then all renamed
        // concept defs, (always tagging the types in the optimizer as well)

        for arg in entity_or_op.args.iter() {
            let arg_idx = graph
                .node_mut(lmd)
                .node_type
                .unwrap_lambda_mut()
                .add_argument();
            let argport = OutportLocation {
                node: lmd,
                output: rvsdg::edge::OutputType::Argument(arg_idx),
            };
            defmap.insert(
                arg.ident.0.clone(),
                VarDef {
                    port: argport.clone(),
                    span: arg.span.clone(),
                },
            );
            //tag the type as well
            type_map.push_attrib(
                &argport.into(),
                arg.ty
                    .clone()
                    .try_into()
                    .expect("Could not convert impl block's arg to an opt-type"),
            );
        }

        for (arg_local_idx, renamed) in block.concept_arg_naming.iter().enumerate() {
            //lookup type in the concept definition
            let ty = concept_def.src_ty[arg_local_idx].clone();
            //now add to the node as argument as well

            let arg_idx = graph
                .node_mut(lmd)
                .node_type
                .unwrap_lambda_mut()
                .add_argument();
            let argport = OutportLocation {
                node: lmd,
                output: rvsdg::edge::OutputType::Argument(arg_idx),
            };

            //TODO use the actual correct span.
            defmap.insert(
                renamed.0.clone(),
                VarDef {
                    port: argport.clone(),
                    span: block.span.clone(),
                },
            );
            //tag the type as well
            type_map.push_attrib(
                &argport.into(),
                ty.try_into()
                    .expect("Could not convert impl block's arg to an opt-type"),
            );
        }

        LmdContext {
            defined_vars: defmap,
        }
    }

    pub fn new_for_exportfn(
        graph: &mut OptGraph,
        type_map: &mut AttribStore<Ty>,
        lmd: NodeRef,
        exportfn: &vola_ast::csg::ExportFn,
    ) -> Self {
        //exportfn are basically a normal function call. So we don't have to do any
        // _context_ analysis.

        let mut defined_vars = AHashMap::new();

        for arg in exportfn.inputs.iter() {
            let arg_idx = graph
                .node_mut(lmd)
                .node_type
                .unwrap_lambda_mut()
                .add_argument();
            let argport = OutportLocation {
                node: lmd,
                output: rvsdg::edge::OutputType::Argument(arg_idx),
            };

            //TODO use the actual correct span.
            defined_vars.insert(
                arg.ident.0.clone(),
                VarDef {
                    port: argport.clone(),
                    span: arg.span.clone(),
                },
            );
            //tag the type as well
            type_map.push_attrib(
                &argport.into(),
                arg.ty
                    .clone()
                    .try_into()
                    .expect("Could not convert impl block's arg to an opt-type"),
            );
        }

        LmdContext { defined_vars }
    }

    pub fn new_for_fielddef(
        graph: &mut OptGraph,
        type_map: &mut AttribStore<Ty>,
        lmd: NodeRef,
        fielddef: &vola_ast::csg::FieldDef,
    ) -> Self {
        //fielddef are basically a normal function call. So we don't have to do any
        // _context_ analysis.

        let mut defined_vars = AHashMap::new();

        for arg in fielddef.inputs.iter() {
            let arg_idx = graph
                .node_mut(lmd)
                .node_type
                .unwrap_lambda_mut()
                .add_argument();
            let argport = OutportLocation {
                node: lmd,
                output: rvsdg::edge::OutputType::Argument(arg_idx),
            };

            //TODO use the actual correct span.
            defined_vars.insert(
                arg.ident.0.clone(),
                VarDef {
                    port: argport.clone(),
                    span: arg.span.clone(),
                },
            );
            //tag the type as well
            type_map.push_attrib(
                &argport.into(),
                arg.ty
                    .clone()
                    .try_into()
                    .expect("Could not convert fielddef's arg to an opt-type"),
            );
        }

        LmdContext { defined_vars }
    }

    ///Checks if a variable with "name" already exists.
    pub fn var_exists(&self, name: &str) -> bool {
        self.defined_vars.contains_key(name)
    }

    ///Adds the VarDef. Panics if the var already existed, since we don't support shadowing atm.
    pub fn add_define(&mut self, name: String, def: VarDef) {
        let old = self.defined_vars.insert(name, def);
        assert!(old.is_none(), "Variable with that name already existed!");
    }
}
