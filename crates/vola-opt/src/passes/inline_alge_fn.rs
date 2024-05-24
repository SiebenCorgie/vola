use rvsdg::SmallColl;
use vola_common::{ariadne::Label, error::error_reporter, report, Span};

use crate::{OptError, Optimizer};

impl Optimizer {
    ///Inlines all callers to a a AlgeFn.
    ///
    ///Can be used at any stage befor dispatching all exports, if needed.
    pub fn inline_alge_fn(&mut self) -> Result<(), OptError> {
        //Thats pretty simple, we find all uses of all alge_fn, inline the using-apply-node
        //and then cleanup the CVs of all regions that we touched.

        #[cfg(feature = "log")]
        log::info!("inline alge functions");

        let mut created_paths = SmallColl::new();
        for algefn in self.alge_fn.values() {
            if let Some(caller) = self.graph.find_caller(algefn.lambda) {
                for c in caller {
                    let mut paths = self.graph.inline_apply_node(c).map_err(|e| {
                        let span = self
                            .span_tags
                            .get(&(c.into()))
                            .cloned()
                            .unwrap_or(Span::empty());

                        let err = OptError::InlineFailed { error: e };

                        report(
                            error_reporter(err.clone(), span.clone())
                                .with_label(
                                    Label::new(span.clone())
                                        .with_message("Failed to inline this function"),
                                )
                                .finish(),
                        );
                        err
                    })?;
                    created_paths.append(&mut paths);
                }
            }
        }

        for p in created_paths {
            if let Err(e) = self.type_path(&p) {
                log::warn!("{e} while inlining all alge-fn");
            }
        }
        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_INLINE_ALGE").is_ok() {
            //self.dump_svg("post_type_derive.svg", true);
            self.push_debug_state("Inline Algebraic");
        }

        Ok(())
    }
}
