use vola_ast::VolaAst;

use crate::{OptError, Optimizer};

/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

impl Optimizer {
    ///Adds a [VolaAst] to the optimizer. Might emit errors if the
    /// semantic analysis fails immediately while adding.
    pub fn add_ast(&mut self, ast: VolaAst) -> Result<(), OptError> {
        //NOTE we first add all def nodes, since those don't depend on anything else, and without those
        // some of the other nodes might not build, even though they could.
        //
        // After that we add all impl-blocks, since they only depend on the defs,
        // then all field-def, since they need the impl-blocks, and last are all exportfn.

        #[cfg(feature = "profile")]
        let ast_add_start = std::time::Instant::now();

        #[cfg(feature = "log")]
        log::info!("Adding Ast to Optimizer");

        let mut errors = Vec::with_capacity(0);

        //NOTE yes collecting into a big'ol Vec all the time is kinda wasteful, but since
        // we use `self` in the filter, we can't just connect multiple filter_maps :O .

        //concept loop
        let sans_defs = ast
            .entries
            .into_iter()
            .filter_map(|ast_entry| {
                if ast_entry.entry.is_def_node() {
                    //Early add def
                    if let Err(e) = self.add_tl_node(ast_entry) {
                        errors.push(e);
                    }
                    None
                } else {
                    Some(ast_entry)
                }
            })
            .collect::<Vec<_>>();

        //functions
        let sans_alge_fn = sans_defs
            .into_iter()
            .filter_map(|ast_entry| {
                if ast_entry.entry.is_fn() {
                    //Early add def
                    if let Err(e) = self.add_tl_node(ast_entry) {
                        errors.push(e)
                    }
                    None
                } else {
                    Some(ast_entry)
                }
            })
            .collect::<Vec<_>>();

        //implblock loop
        let sans_impl_block = sans_alge_fn
            .into_iter()
            .filter_map(|ast_entry| {
                if ast_entry.entry.is_impl_block() {
                    //Early add def
                    if let Err(e) = self.add_tl_node(ast_entry) {
                        errors.push(e);
                    }
                    None
                } else {
                    Some(ast_entry)
                }
            })
            .collect::<Vec<_>>();

        //Should be empty except for comments and stuff?
        assert!(sans_impl_block.is_empty());

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
            Err(OptError::ErrorsOccurred(errors.len()))
        } else {
            Ok(())
        }
    }
}
