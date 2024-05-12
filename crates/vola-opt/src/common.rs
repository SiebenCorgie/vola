use std::fmt::Display;

/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use ahash::AHashMap;
use rvsdg::{
    attrib::FlagStore,
    edge::{OutportLocation, OutputType},
    smallvec::SmallVec,
    NodeRef, SmallColl,
};
use vola_ast::{
    alge::ImplBlock,
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
    //Can only be produced by the optimizer atm.
    Bool,
    //A _Natural_ number. Basically a uint, but with unknown resolution.
    Nat,
    //A _Real_ number. Basically a float, but with unknown resolution.
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

impl From<vola_ast::common::Ty> for Ty {
    fn from(value: vola_ast::common::Ty) -> Self {
        match value {
            vola_ast::common::Ty::Scalar => Self::Scalar,
            vola_ast::common::Ty::Nat => Self::Nat,
            vola_ast::common::Ty::Vec { width } => Self::Vector { width },
            vola_ast::common::Ty::Matrix { width, height } => Self::Matrix { width, height },
            vola_ast::common::Ty::Tensor { dim } => Self::Tensor { dim },
            vola_ast::common::Ty::CSGTree => Self::CSGTree,
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::Void => write!(f, "Void"),
            Ty::Bool => write!(f, "Bool"),
            Ty::Nat => write!(f, "Nat"),
            Ty::Scalar => write!(f, "Scalar"),
            Ty::Vector { width } => write!(f, "Vec{width}"),
            Ty::Matrix { width, height } => write!(f, "Matrix{width}x{height}"),
            Ty::Tensor { dim } => write!(f, "Tensor[{dim:?}]"),
            Ty::CSGTree => write!(f, "CSGTree"),
            Ty::Callable { concept } => write!(f, "Callable(\"{concept}\")"),
        }
    }
}

impl Ty {
    pub fn is_bool(&self) -> bool {
        if let Self::Bool = self {
            true
        } else {
            false
        }
    }
    ///Returns true for scalar, vector, matrix and tensor_type
    pub fn is_algebraic(&self) -> bool {
        match self {
            Self::Scalar
            | Self::Vector { .. }
            | Self::Matrix { .. }
            | Self::Tensor { .. }
            | Self::Nat => true,
            Self::CSGTree | Self::Callable { .. } | Self::Void | Self::Bool => false,
        }
    }

    pub fn is_vector(&self) -> bool {
        if let Self::Vector { .. } = self {
            true
        } else {
            false
        }
    }

    ///Tries to derive a type that would be produced by indexing with `index` into the `Ty`.
    pub(crate) fn try_derive_access_index(&self, index: usize) -> Result<Ty, OptError> {
        match self {
            Ty::Scalar => Err(OptError::Any {
                text: format!("Scalar cannot be indexed with {}", index),
            }),
            Ty::Nat => Err(OptError::Any {
                text: format!("Nat cannot be indexed with {}", index),
            }),
            Ty::Vector { width } => {
                if index >= *width {
                    Err(OptError::Any {
                        text: format!("Vector of width {width} cannot be index with {index}"),
                    })
                } else {
                    //Otherwise always resolves to an scalar
                    Ok(Ty::Scalar)
                }
            }
            Ty::Matrix { width, height } => {
                if index >= *height {
                    Err(OptError::Any {
                        text: format!("Matrix {width}x{height} cannot be index with {index}"),
                    })
                } else {
                    Ok(Ty::Vector { width: *width })
                }
            }
            Ty::Tensor { dim } => match dim.len() {
                0 => Err(OptError::Any {
                    text: "Encountered zero dimensional tensor!".to_owned(),
                }),
                1 => Ty::Vector { width: dim[0] }.try_derive_access_index(index),
                2 => Ty::Matrix {
                    width: dim[1],
                    height: dim[0],
                }
                .try_derive_access_index(index),
                _any => {
                    if index >= dim[0] {
                        Err(OptError::Any {
                            text: format!(
                                "Cannot index tensor dimension of width={} with {index}",
                                dim[0]
                            ),
                        })
                    } else {
                        let new_dim = dim[1..].iter().map(|d| *d).collect();
                        Ok(Ty::Tensor { dim: new_dim })
                    }
                }
            },
            other_ty => Err(OptError::Any {
                text: format!("Cannot index into {:?}", other_ty),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct VarDef {
    pub port: OutportLocation,
    pub span: vola_common::Span,
}

#[derive(Debug, Clone)]
pub struct FnImport {
    pub port: OutportLocation,
    pub span: vola_common::Span,
    pub args: SmallColl<Ty>,
    pub ret: Ty,
}

///Helper utility that keeps track of defined variables.
#[derive(Debug, Clone)]
pub struct LmdContext {
    ///Maps a variable name to an Value outport.
    pub defined_vars: AHashMap<String, VarDef>,
    pub imported_functions: AHashMap<String, FnImport>,
}

impl LmdContext {
    pub(crate) fn merge_inner_ctx(&mut self, ctx: Self) {
        //merge imported functions
        for (key, fn_import) in ctx.imported_functions {
            if !self.imported_functions.contains_key(&key) {
                self.imported_functions.insert(key, fn_import);
            }
        }
    }

    pub fn empty() -> Self {
        LmdContext {
            defined_vars: AHashMap::new(),
            imported_functions: AHashMap::new(),
        }
    }

    pub fn new_for_impl_block(
        graph: &mut OptGraph,
        type_map: &mut FlagStore<Ty>,
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
            type_map.set(argport.into(), arg.ty.clone().into());
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
            type_map.set(argport.into(), ty.into());
        }

        LmdContext {
            defined_vars: defmap,
            imported_functions: AHashMap::with_capacity(0),
        }
    }

    pub fn new_for_exportfn(
        graph: &mut OptGraph,
        type_map: &mut FlagStore<Ty>,
        lmd: NodeRef,
        exportfn: &vola_ast::csg::ExportFn,
    ) -> Self {
        //exportfn are basically a normal function call. So we don't have to do any
        // _context_ analysis.

        let mut defined_vars = AHashMap::new();

        for arg in exportfn.args.iter() {
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
            type_map.set(argport.into(), arg.ty.clone().into());
        }

        LmdContext {
            defined_vars,
            imported_functions: AHashMap::with_capacity(0),
        }
    }

    pub fn new_for_fielddef(
        graph: &mut OptGraph,
        type_map: &mut FlagStore<Ty>,
        lmd: NodeRef,
        fielddef: &vola_ast::csg::FieldDef,
    ) -> Self {
        //fielddef are basically a normal function call. So we don't have to do any
        // _context_ analysis.

        let mut defined_vars = AHashMap::new();

        for arg in fielddef.args.iter() {
            let arg_idx = graph
                .node_mut(lmd)
                .node_type
                .unwrap_lambda_mut()
                .add_argument();
            let argport = OutportLocation {
                node: lmd,
                output: OutputType::Argument(arg_idx),
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
            type_map.set(argport.into(), arg.ty.clone().into());
        }

        LmdContext {
            defined_vars,
            imported_functions: AHashMap::with_capacity(0),
        }
    }

    pub fn new_for_alge_fn(
        graph: &mut OptGraph,
        type_map: &mut FlagStore<Ty>,
        lmd: NodeRef,
        alge_fn: &vola_ast::alge::AlgeFunc,
    ) -> Self {
        let mut defined_vars = AHashMap::new();
        //Build the argument map, and setup the λ-Context
        for arg in &alge_fn.args {
            let arg_port = graph
                .node_mut(lmd)
                .node_type
                .unwrap_lambda_mut()
                .add_argument();

            let argport = OutportLocation {
                node: lmd,
                output: OutputType::Argument(arg_port),
            };
            defined_vars.insert(
                arg.ident.0.clone(),
                VarDef {
                    port: argport.clone(),
                    span: arg.span.clone(),
                },
            );
            type_map.set(argport.clone().into(), Ty::from(arg.clone().ty));
        }

        LmdContext {
            defined_vars,
            imported_functions: AHashMap::with_capacity(0),
        }
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

    pub fn known_function(&self, name: &String) -> Option<&FnImport> {
        self.imported_functions.get(name)
    }
}
