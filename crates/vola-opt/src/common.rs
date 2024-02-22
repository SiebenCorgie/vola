use ahash::AHashMap;
use rvsdg::{
    attrib::AttribStore,
    edge::{InportLocation, OutportLocation},
    nodes::LambdaNode,
    smallvec::SmallVec,
    NodeRef, Rvsdg,
};
use vola_ast::{
    alge::ImplBlock,
    csg::{CSGConcept, CSGNodeDef},
};

use crate::{error::OptError, OptEdge, OptNode, Optimizer};

///Optimizer types. Those are the AST types, as well as the higher-order-function like types we use to identify
/// CV-Inputs of nodes. They basically make sure that we connect λ-Nodes with the right output type _when called_.
pub enum Ty {
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
        graph: &mut Rvsdg<OptNode, OptEdge>,
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

//Macro that implements the "View" trait for an AlgeDialect op
macro_rules! implViewAlgeOp {
    ($opname:ident, $str:expr, $($arg:ident),*) => {
        impl rvsdg_viewer::View for $opname {
            fn color(&self) -> rvsdg_viewer::macroquad::color::Color {
                rvsdg_viewer::macroquad::color::Color::from_rgba(128, 64, 64, 255)
            }

            fn name(&self) -> &str {
                &format!($str, $(self.$arg)*,)
            }

            fn stroke(&self) -> rvsdg_viewer::Stroke {
                rvsdg_viewer::Stroke::Line
            }
        }
    };
}
