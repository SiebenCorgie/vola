/* * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Module that handles the opt-graph building based on [AST](vola-ast) nodes.

use rvsdg::NodeRef;
use vola_ast::{AstEntry, TopLevelNode};
use vola_common::{ariadne::Label, error::error_reporter, report};

use crate::{error::OptError, Optimizer};

pub(crate) mod block_builder;
pub(crate) mod function_intern;
pub(crate) mod implblock;

impl Optimizer {
    ///Adds the top-level node to the optimizer graph. If it applies, it returns a reference to the created node.
    pub fn add_tl_node(&mut self, tlnode: TopLevelNode) -> Result<Option<NodeRef>, OptError> {
        match tlnode.entry {
            //We ignore those atm
            AstEntry::Comment(_) => Ok(None),
            AstEntry::Module(m) => {
                let err = OptError::Any {
                    text: "Encountered \"module\" while transforming AST to RVSDG. Modules should be resolved beforehand.".to_owned()
                };
                report(
                    error_reporter(err.clone(), m.span.clone())
                        .with_label(
                            Label::new(m.span.clone()).with_message("This module was not resolved"),
                        )
                        .finish(),
                );
                Err(err)
            }
            AstEntry::Concept(csgcon) => {
                if let Some(existing_concept) = self.concepts.get(&csgcon.name.0) {
                    let err = OptError::Any {
                        text: format!("Concept {} was already defined", existing_concept.name.0),
                    };

                    report(
                        error_reporter(err.clone(), csgcon.span.clone())
                            .with_label(
                                Label::new(existing_concept.span.clone())
                                    .with_message("first defined here"),
                            )
                            .with_label(
                                Label::new(csgcon.span.clone()).with_message("redefined here"),
                            )
                            .finish(),
                    );

                    Err(err)
                } else {
                    //No yet in collection, therefore push
                    self.concepts.insert(csgcon.name.0.clone(), csgcon);
                    Ok(None)
                }
            }
            AstEntry::CsgDef(csgnd) => {
                //similar to the concept case, test if there is already one, if not, push
                if let Some(existing_csg) = self.csg_node_defs.get(&csgnd.name.0) {
                    let err = OptError::Any {
                        text: format!("Operation or Entity {} was already defined. \nNote that operations and entities share one name space.", existing_csg.name.0),
                    };

                    report(
                        error_reporter(err.clone(), csgnd.span.clone())
                            .with_label(
                                Label::new(existing_csg.span.clone())
                                    .with_message("first defined here"),
                            )
                            .with_label(
                                Label::new(csgnd.span.clone()).with_message("redefined here"),
                            )
                            .finish(),
                    );

                    Err(err)
                } else {
                    //No yet in collection, therefore push
                    self.csg_node_defs.insert(csgnd.name.0.clone(), csgnd);
                    Ok(None)
                }
            }
            AstEntry::ImplBlock(implblock) => self.add_impl_block(implblock).map(|t| Some(t)),
            AstEntry::Func(algefn) => self.add_fn(algefn).map(|t| Some(t)),
        }
    }
}
