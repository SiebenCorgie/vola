//! # Vola's High-level Intermediate Representation
//!
//! This is our main _custom_ compiler layer, where all the ✨magic✨ happens.
//! For now the the layer has the following responsibilities:
//!
//! 1. Transform VolaAST into RVSDG
//! 2. resolve types
//! 3. build the signed-distance + attributes struct for the SDF result
//! 4. function dispatch for aliasing types on the same function: i.e `f(1f32);` and `f(1i32);` dispatch into `f_f32(1.0)` and `f_i32(1)`.
//!
//!

use ahash::AHashMap;
pub use edge::HirEdge;
use err::HirErr;
pub use ops::HirOp;
use rvsdg::{
    attrib::AttribStore,
    edge::{OutportLocation, OutputType},
    NodeRef, Rvsdg,
};
use types::HirTypeState;
use vola_ast::Ast;

mod ast;
mod edge;
pub(crate) mod err;
mod ops;
mod types;

///Stores some information about the HIR module that might be used for transformations.
///
/// All information is stored on a _lazy_ basis.
pub struct Querypool {
    pub type_states: AttribStore<HirTypeState>,
    pub labels: AttribStore<String>,
    ///All known lambda decleration nodes. Do not necessarly have to be _all_.
    pub lambda_decls: AHashMap<String, NodeRef>,
}

impl Querypool {
    pub fn new() -> Self {
        Querypool {
            type_states: AttribStore::new(),
            labels: AttribStore::new(),
            lambda_decls: AHashMap::default(),
        }
    }

    ///Tries to find a lambda node that is labeled with `identifier`. If found, returns the lambda decleration port
    pub fn lookup_lambda_decl(&self, identifier: &str) -> Option<OutportLocation> {
        if let Some(node) = self.lambda_decls.get(identifier) {
            Some(node.as_outport_location(OutputType::LambdaDeclaration))
        } else {
            None
        }
    }
}

pub struct VolaHir {
    rvsdg: Rvsdg<HirOp, HirEdge>,
    querypool: Querypool,
}

impl VolaHir {
    pub fn new() -> Self {
        VolaHir {
            rvsdg: Rvsdg::new(),
            querypool: Querypool::new(),
        }
    }

    ///Interns the ast into the module.
    pub fn intern_ast(&mut self, ast: Ast) -> Result<(), HirErr> {
        ast::tranform_into_ast(ast, self)
    }
}
