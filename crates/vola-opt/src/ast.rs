//! Module that handles the opt-graph building based on [AST](vola-ast) nodes.

use rvsdg::NodeRef;
use vola_ast::{AstEntry, TopLevelNode};
use vola_common::report;

use crate::{error::OptError, Optimizer};

impl Optimizer {
    ///Adds the top-level node to the optimizer graph. If it applies, it returns a reference to the created node.
    pub fn add_tl_node(&mut self, tlnode: TopLevelNode) -> Result<Option<NodeRef>, OptError> {
        match tlnode.entry {
            //We ignore those atm
            AstEntry::Comment(_) => Ok(None),
            AstEntry::Concept(csgcon) => {
                if let Some(existing_concept) = self.concepts.get(&csgcon.name.0) {
                    let err = OptError::AnySpannedWithSource {
                        source_span: existing_concept.span.clone().into(),
                        source_text: format!("First occurrence of {}", existing_concept.name.0),
                        text: format!("Concept {} was already defined", existing_concept.name.0),
                        span: csgcon.span.clone().into(),
                        span_text: format!("Tried to redefine it here"),
                    };

                    report(err.clone(), existing_concept.span.get_file());

                    return Err(err);
                } else {
                    //No yet in collection, therefore push
                    self.concepts.insert(csgcon.name.0.clone(), csgcon);
                    Ok(None)
                }
            }
            AstEntry::CSGNodeDef(csgnd) => {
                //similar to the concept case, test if there is already one, if not, push
                if let Some(existing_csg) = self.csg_node_defs.get(&csgnd.name.0) {
                    let err = OptError::AnySpannedWithSource {
                        source_span: existing_csg.span.clone().into(),
                        source_text: format!("First occurrence of {}", existing_csg.name.0),
                        text: format!("Operation or Entity {} was already defined. \nNote that operations and entities share one name space.", existing_csg.name.0),
                        span: csgnd.span.clone().into(),
                        span_text: format!("Tried to redefine it here"),
                    };

                    report(err.clone(), existing_csg.span.get_file());

                    return Err(err);
                } else {
                    //No yet in collection, therefore push
                    self.csg_node_defs.insert(csgnd.name.0.clone(), csgnd);
                    Ok(None)
                }
            }
            AstEntry::ImplBlock(implblock) => self.add_impl_block(implblock).map(|t| Some(t)),
            AstEntry::FieldDefine(fdef) => Ok(None),
            AstEntry::ExportFn(expfn) => Ok(None),
        }
    }
}
