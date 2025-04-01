/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use crate::{OptError, Optimizer};
use vola_ast::{AstEntry, TopLevelNode, VolaAst};
use vola_common::VolaError;
mod ast_to_graph;
mod block_build;
mod expr_build;

impl Optimizer {
    ///Adds a [VolaAst] to the optimizer. Might emit errors if the
    /// semantic analysis fails immediately while adding.
    pub fn add_ast(&mut self, ast: VolaAst) -> Result<(), Vec<VolaError<OptError>>> {
        //Interning the ast works in three steps:
        // 1. Serialize all entry-points into the graph
        //    For any function-like _call_ insert a unresolved node
        // 2. Launche resolve pass, that finds, all inter-procedurale calls. We do this based on the _at some point_
        //    inserted CsgDefs/impl-blocks
        // 3. Launch initial type-resolver.

        #[cfg(feature = "profile")]
        let ast_add_start = std::time::Instant::now();

        #[cfg(feature = "log")]
        log::info!("Adding Ast to Optimizer");

        let mut errors: Vec<VolaError<OptError>> = Vec::with_capacity(0);

        //NOTE: we first inter all defines, then the actual code.
        //      needed so we can already infer some basic stuff at interning time.
        for entry in &ast.entries {
            let TopLevelNode {
                span,
                ct_args,
                entry,
            } = entry;
            let result = match entry {
                AstEntry::Concept(c) => self.add_concept(span.clone(), ct_args.clone(), c.clone()),
                AstEntry::CsgDef(def) => {
                    self.add_csgdef(span.clone(), ct_args.clone(), def.clone())
                }
                AstEntry::Func(f) => self.define_func(f),
                _ => Ok(()),
            };

            if let Err(e) = result {
                errors.push(e);
            }
        }

        //if we already had an error, we can-not continue, since
        //nothing is valid anyways
        if errors.len() > 0 {
            return Err(errors);
        }

        for entry in ast.entries {
            let TopLevelNode {
                span,
                ct_args,
                entry,
            } = entry;
            let result = match entry {
                AstEntry::Func(f) => self.build_func_block(span, ct_args, f),
                AstEntry::ImplBlock(implblock) => self.add_implblock(span, ct_args, implblock),
                AstEntry::Module(_m) => Err(VolaError::new(OptError::Internal(format!(
                    "Found module, those should be resolved already!"
                )))),
                _ => Ok(()),
            };

            if let Err(e) = result {
                errors.push(e);
            }
        }

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_AFTER_INTERN").is_ok() {
            self.push_debug_state("AST interned");
        }

        //try to detect recurisve calls
        self.detect_recursive_calls()?;

        //do initial type resolving
        self.type_derive()?;

        #[cfg(feature = "profile")]
        println!(
            "Adding AST took {}ms / {}ns",
            ast_add_start.elapsed().as_millis(),
            ast_add_start.elapsed().as_nanos()
        );

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("VOLA_DUMP_AST").is_ok() {
            self.push_debug_state("AST to Opt");
        }

        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(())
        }
    }
}
