/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! # CSG Dialect

use rvsdg::{
    nodes::LangNode,
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    smallvec::{smallvec, SmallVec},
    EdgeRef,
};
use vola_ast::{common::Ident, csg::CSGNodeDef};

use crate::{common::Ty, error::OptError, DialectNode, OptNode};

pub(crate) mod exportfn;
pub(crate) mod fielddef;

//Macro that implements the "View" trait for an AlgeDialect op
macro_rules! implViewCsgOp {
    ($opname:ident, $str:expr, $($arg:ident),*) => {
        impl rvsdg_viewer::View for $opname {
            fn color(&self) -> rvsdg_viewer::macroquad::color::Color {
                rvsdg_viewer::macroquad::color::Color::from_rgba(170, 200, 170, 255)
            }

            fn name(&self) -> String {
                format!($str, $(self.$arg)*,)
            }

            fn stroke(&self) -> rvsdg_viewer::Stroke {
                rvsdg_viewer::Stroke::Line
            }
        }
    };
    ($opname:ident, $str:expr) =>{
        impl rvsdg_viewer::View for $opname {
            fn color(&self) -> rvsdg_viewer::macroquad::color::Color {
                rvsdg_viewer::macroquad::color::Color::from_rgba(170, 200, 170, 255)
            }

            fn name(&self) -> String {
                $str.to_owned()
            }

            fn stroke(&self) -> rvsdg_viewer::Stroke {
                rvsdg_viewer::Stroke::Line
            }
        }
    }
}

///Highlevel CSG Op where the concept identifier is verified, but not yet specialized.
/// Used to build resolved.
#[derive(LangNode, Debug)]
pub struct CsgOp {
    ///The operation or entity that is being called.
    pub op: String,

    pub subtree_count: usize,
    #[inputs]
    pub inputs: SmallVec<[Input; 2]>,
    #[output]
    pub output: Output,
}

impl CsgOp {
    pub fn new(op: Ident, subtree_count: usize, parameter_count: usize) -> Self {
        CsgOp {
            op: op.0,
            subtree_count,
            inputs: smallvec![Input::default(); parameter_count],
            output: Output::default(),
        }
    }
}

implViewCsgOp!(CsgOp, "{:?}", op);
impl DialectNode for CsgOp {
    fn dialect(&self) -> &'static str {
        "csg"
    }

    fn structural_copy(&self, span: vola_common::Span) -> OptNode {
        OptNode {
            span,
            node: Box::new(CsgOp {
                op: self.op.clone(),
                subtree_count: self.subtree_count,
                inputs: smallvec![Input::default(); self.inputs.len()],
                output: Output::default(),
            }),
        }
    }

    fn try_derive_type(
        &self,
        _typemap: &rvsdg::attrib::AttribStore<crate::common::Ty>,
        graph: &crate::OptGraph,
        _concepts: &ahash::AHashMap<String, vola_ast::csg::CSGConcept>,
        csg_defs: &ahash::AHashMap<String, CSGNodeDef>,
    ) -> Result<Option<crate::common::Ty>, crate::error::OptError> {
        //We resole the CSG op by checking, that all inputs adher to the op's specification.
        // Which means the arguments that are connected are equal to the one specified by the
        // implemented operation or entity
        let expected_signature = csg_defs
            .get(&self.op)
            .unwrap()
            .args
            .iter()
            .map(|arg| {
                arg.ty
                    .clone()
                    .try_into()
                    .expect("Could not convert ty opt-type")
            })
            .collect::<SmallVec<[Ty; 3]>>();
        //we always output a _CSGTree_ component.
        let output: Ty = Ty::CSGTree;

        //In practice we now iterate all connected inputs. The first 0..n migth be CSGTrees
        // already, which are our sub_trees. We verify those against the `subtree_count`.
        // All following connected nodes must be part of the `expected_signature`.
        for i in 0..self.subtree_count {
            if let Some(port) = self.inputs.get(i) {
                if let Some(edg) = port.edge {
                    match graph.edge(edg).ty.get_type() {
                        Some(ty) => {
                            //Check that its actually a csg tree
                            if ty != &Ty::CSGTree {
                                return Err(OptError::Any {
                                    text: format!(
                                        "Subtree {i} was not of type CSGTree for CSGOp {}",
                                        self.op
                                    ),
                                });
                            }
                        }
                        None => {
                            //Not set
                            return Ok(None);
                        }
                    }
                }
            } else {
                //edge not yet set
                return Ok(None);
            }
        }

        //NOTE that there is the right amount of args is already checked at the building procedure!
        let mut algearg = 0;
        while let Some(arg) = self.inputs.get(self.subtree_count + algearg) {
            if let Some(edg) = arg.edge {
                match graph.edge(edg).ty.get_type() {
                    Some(ty) => {
                        //Check that its actually a csg tree
                        if ty != &expected_signature[algearg] {
                            return Err(OptError::Any {
                                text: format!(
                                    "expected {algearg}-th argument to be {:?} not {:?} for CSGOp {}",
                                    expected_signature[algearg], ty, self.op
                                ),
                            });
                        }
                    }
                    None => {
                        //Not set
                        return Ok(None);
                    }
                }
            }
            algearg += 1;
        }

        Ok(Some(output))
    }
}

/*
///CsgTree call into a defined sub tree. Akin to a function call, but gets inlined at dispatch-time.
///
/// NOTE: We *don't* use a λ-Node+Apply node pair here, since we don't known _yet_ which concept on this CSGNode (Entity or Operation) is called.
/// So in turn we can't known which λ-Node to import.
#[derive(LangNode, Debug)]
pub struct CsgCall {
    ///The field that thas is called
    pub op: String,

    ///Expected signature for this call.
    pub input_signature: SmallVec<[Ty; 2]>,

    #[inputs]
    pub inputs: SmallVec<[Input; 2]>,
    #[output]
    pub output: Output,
}

impl CsgCall {
    pub fn new(op: Ident, signature: SmallVec<[Ty; 2]>) -> Self {
        CsgCall {
            op: op.0,
            inputs: smallvec![Input::default(); signature.len()],
            input_signature: signature,
            output: Output::default(),
        }
    }
}

implViewCsgOp!(CsgCall, "fieldcall {:?}", op);
impl DialectNode for CsgCall {
    fn dialect(&self) -> &'static str {
        "csg"
    }

    fn structural_copy(&self, span: vola_common::Span) -> OptNode {
        OptNode {
            span,
            node: Box::new(CsgCall {
                op: self.op.clone(),
                input_signature: self.input_signature.clone(),
                inputs: smallvec![Input::default(); self.inputs.len()],
                output: Output::default(),
            }),
        }
    }

    fn try_derive_type(
        &self,
        _typemap: &rvsdg::attrib::AttribStore<Ty>,
        graph: &crate::OptGraph,
        _concepts: &ahash::AHashMap<String, vola_ast::csg::CSGConcept>,
        _csg_defs: &ahash::AHashMap<String, CSGNodeDef>,
    ) -> Result<Option<Ty>, OptError> {
        //We know the expected signature. Just check each input

        for i in 0..self.input_signature.len() {
            if let Some(port) = self.inputs.get(i) {
                if let Some(edg) = port.edge {
                    match graph.edge(edg).ty.get_type() {
                        Some(ty) => {
                            if ty != &self.input_signature[i] {
                                return Err(OptError::Any {
                                    text: format!(
                                        "Field argument {i} was {:?} but expected {:?}",
                                        ty, self.input_signature[i]
                                    ),
                                });
                            }
                        }
                        None => {
                            //Not set
                            return Ok(None);
                        }
                    }
                }
            } else {
                //edge not yet set
                return Ok(None);
            }
        }

        //If we made it till here, we known that the field-call always returns a CSGTree
        Ok(Some(Ty::CSGTree))
    }
}
*/
///Access description for a tree.
#[derive(LangNode, Debug)]
pub struct TreeAccess {
    ///The concept that is being called by the description.
    pub called_concept: String,

    ///Expected signature for this call.
    pub input_signature: SmallVec<[Ty; 2]>,
    pub return_type: Ty,
    #[inputs]
    pub inputs: SmallVec<[Input; 3]>,
    #[output]
    pub output: Output,
}

impl TreeAccess {
    pub fn new(called_concept: Ident, signature: SmallVec<[Ty; 2]>, return_type: Ty) -> Self {
        TreeAccess {
            called_concept: called_concept.0,
            inputs: smallvec![Input::default(); signature.len()],
            return_type,
            input_signature: signature,
            output: Output::default(),
        }
    }

    pub fn get_op_edge(&self) -> Option<EdgeRef> {
        self.inputs[0].edge.clone()
    }

    pub fn get_args(&self) -> SmallVec<[Option<EdgeRef>; 3]> {
        self.inputs[1..]
            .iter()
            .map(|arg| arg.edge.clone())
            .collect()
    }
}

implViewCsgOp!(TreeAccess, "TreeAccess({})", called_concept);
impl DialectNode for TreeAccess {
    fn dialect(&self) -> &'static str {
        "csg"
    }

    fn structural_copy(&self, span: vola_common::Span) -> OptNode {
        OptNode {
            span,
            node: Box::new(TreeAccess {
                called_concept: self.called_concept.clone(),
                input_signature: self.input_signature.clone(),
                return_type: self.return_type.clone(),
                inputs: smallvec![Input::default(); self.inputs.len()],
                output: Output::default(),
            }),
        }
    }

    fn try_derive_type(
        &self,
        _typemap: &rvsdg::attrib::AttribStore<Ty>,
        graph: &crate::OptGraph,
        _concepts: &ahash::AHashMap<String, vola_ast::csg::CSGConcept>,
        _csg_defs: &ahash::AHashMap<String, CSGNodeDef>,
    ) -> Result<Option<Ty>, OptError> {
        //We know the expected signature. Just check each input

        for i in 0..self.input_signature.len() {
            if let Some(port) = self.inputs.get(i) {
                if let Some(edg) = port.edge {
                    match graph.edge(edg).ty.get_type() {
                        Some(ty) => {
                            if ty != &self.input_signature[i] {
                                return Err(OptError::Any {
                                    text: format!(
                                        "Field argument {i} was {:?} but expected {:?}",
                                        ty, self.input_signature[i]
                                    ),
                                });
                            }
                        }
                        None => {
                            //Not set
                            return Ok(None);
                        }
                    }
                }
            } else {
                //edge not yet set
                return Ok(None);
            }
        }

        //If we are here, all went well, return the actual return type
        Ok(Some(self.return_type.clone()))
    }
}
