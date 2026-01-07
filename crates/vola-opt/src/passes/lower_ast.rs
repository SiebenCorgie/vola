/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use crate::{passes::initial_type_check::InitialTypeCheck, OptError, Optimizer};
use vola_ast::{AstEntry, TopLevelNode, VolaAst};
use vola_common::VolaError;
mod ast_to_graph;
mod block_build;
mod expr_build;

///Lowers a [VolaAst] into the optimizer's graph. Takes care of the correct order, checking dependencies of calls and initial type checking.
///
/// Errors are defered until [finish](Self::finish), which allows it to emit multiple error. If this happens, the graph's state is considered _unstable_, since
/// there might be broken code afterwards.
pub struct LowerAst<'opt> {
    opt: &'opt mut Optimizer,
    ///Collects all errors that might have occured
    errors: Vec<VolaError<OptError>>,
    #[cfg(feature = "profile")]
    start: std::time::Instant,
}

impl<'opt> LowerAst<'opt> {
    pub fn setup(opt: &'opt mut Optimizer) -> Self {
        Self {
            opt,
            errors: Vec::with_capacity(0),

            #[cfg(feature = "profile")]
            start: std::time::Instant::now(),
        }
    }

    ///Adds a [VolaAst] to the optimizer. Might emit errors if the
    /// semantic analysis fails immediately while adding. This doesn't mean that you have to stop adding elements
    /// to the graph though. All occured errors, if there where any will be emitted uppon [finish](LowerAst::finish).
    pub fn add_ast(mut self, ast: VolaAst) -> Self {
        //Lowering the ast works in three steps:
        // 1. Serialize all entry-points into the graph
        //    For any function-like _call_ insert a unresolved node
        // 2. Launche resolve pass, that finds, all inter-procedurale calls. We do this based on the _at some point_
        //    inserted CsgDefs/impl-blocks
        // 3. Launch initial type-resolver.

        #[cfg(feature = "log")]
        log::info!(
            "Adding Ast with {} elements to Optimizer",
            ast.entries.len()
        );

        //NOTE: we first inter all defines, then the actual code.
        //      needed so we can already infer some basic stuff at lowering time.

        let mut late_resolve = Vec::new();

        for entry in ast.entries {
            let TopLevelNode {
                span,
                ct_args,
                entry,
            } = entry;
            let result = match entry {
                AstEntry::Concept(c) => self.add_concept(span, ct_args, c),
                AstEntry::CsgDef(def) => self.add_csgdef(span, ct_args, def),
                AstEntry::Func(f) => {
                    //Functions are just defined in this pre-pass, but build later on
                    let res = self.define_func(&f, &ct_args);
                    late_resolve.push(TopLevelNode {
                        span,
                        ct_args,
                        entry: AstEntry::Func(f),
                    });
                    res
                }
                other => {
                    //Resolve all other top-level elements later
                    late_resolve.push(TopLevelNode {
                        span,
                        ct_args,
                        entry: other,
                    });
                    Ok(())
                }
            };

            if let Err(e) = result {
                self.errors.push(e);
            }
        }

        for entry in late_resolve {
            let TopLevelNode {
                span,
                ct_args,
                entry,
            } = entry;
            let result = match entry {
                AstEntry::Func(f) => self.build_func_block(span, ct_args, f),
                AstEntry::ImplBlock(implblock) => self.add_implblock(span, ct_args, implblock),
                AstEntry::Module(_m) => Err(VolaError::new(OptError::Internal(
                    "Found module, those should be resolved already!".to_string(),
                ))),
                _ => Ok(()),
            };

            if let Err(e) = result {
                self.errors.push(e);
            }
        }

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_AFTER_LOWERING").is_ok() {
            self.opt.push_debug_state("AST Lowering");
        }

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("VOLA_DUMP_AST").is_ok() {
            self.opt.push_debug_state("AST to Opt");
        }

        self
    }

    ///Finishes by verifying that no recursion has occured, and the full graph
    /// is type-checked.
    pub fn finish(self) -> Result<(), Vec<VolaError<OptError>>> {
        //NOTE: do not check, if errors ocured before
        if !self.errors.is_empty() {
            return Err(self.errors);
        }

        #[cfg(feature = "profile")]
        println!(
            "Adding AST took {}ms / {}ns",
            self.start.elapsed().as_millis(),
            self.start.elapsed().as_nanos()
        );

        #[cfg(feature = "log")]
        log::info!("Recursion Detection after adding");

        //try to detect recurisve calls
        self.opt.detect_recursive_calls()?;

        #[cfg(feature = "log")]
        log::info!("Type check after adding");
        //do initial type resolving
        //self.initial_type_derive()?;
        InitialTypeCheck::setup(self.opt).execute()?;

        Ok(())
    }
}
