use std::fmt::Debug;

use vola_ast::VolaAst;
use vola_common::VolaError;
use vola_opt::{OptError, Optimizer};

pub trait Pass {
    fn execute(self, opt: &mut Optimizer) -> Result<(), PassError>;
}

///Records multiple errors on a pass execution. If printed, reports all errors to
/// stdout
pub struct PassError {
    pub errors: Vec<VolaError<OptError>>,
}

impl PassError {
    ///True if there are any errors
    pub fn any(&self) -> bool {
        !self.errors.is_empty()
    }
}

impl Debug for PassError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for err in &self.errors {
            err.report();
            write!(f, "{err:?}")?
        }

        Ok(())
    }
}

impl From<Vec<VolaError<OptError>>> for PassError {
    fn from(value: Vec<VolaError<OptError>>) -> Self {
        Self { errors: value }
    }
}

impl From<VolaError<OptError>> for PassError {
    fn from(value: VolaError<OptError>) -> Self {
        Self {
            errors: vec![value],
        }
    }
}

///Lowerst the AST into the optimizer
pub struct LowerAst(VolaAst);
impl Pass for LowerAst {
    fn execute(self, opt: &mut Optimizer) -> Result<(), PassError> {
        vola_opt::passes::LowerAst::setup(opt)
            .add_ast(self.0)
            .finish()
            .map_err(|e| e.into())
    }
}

pub struct PatterRewriteAll;
impl Pass for PatterRewriteAll {
    fn execute(self, opt: &mut Optimizer) -> Result<(), PassError> {
        opt.pattern_rewrite_all();
        Ok(())
    }
}

pub struct SpecializeAll;
impl Pass for SpecializeAll {
    fn execute(self, opt: &mut Optimizer) -> Result<(), PassError> {
        opt.specialize_all_exports().map_err(|e| e.into())
    }
}

pub struct Dne;
impl Pass for Dne {
    fn execute(self, opt: &mut Optimizer) -> Result<(), PassError> {
        let _eliminated = opt
            .graph
            .dead_node_elimination()
            .map_err(|e| VolaError::new(e).to_error::<OptError>())?;
        Ok(())
    }
}

///runst common-node-folding on the whole graph
pub struct Cnf;
impl Pass for Cnf {
    fn execute(self, opt: &mut Optimizer) -> Result<(), PassError> {
        opt.full_graph_cnf()
            .map_err(|e| VolaError::new(e.into()).into())
    }
}

pub struct LowerIntervals;
impl Pass for LowerIntervals {
    fn execute(self, opt: &mut Optimizer) -> Result<(), PassError> {
        if opt.config.seen_pass_nodes.interval {
            opt.interval_extension()?;
            //Reset the flag, since we just lowerd them :)
            opt.config.seen_pass_nodes.interval = false;
        }

        opt.interval_to_tuple()?;
        Ok(())
    }
}

pub struct LowerAutodiff;
impl Pass for LowerAutodiff {
    fn execute(self, opt: &mut Optimizer) -> Result<(), PassError> {
        if opt.config.seen_pass_nodes.autodiff {
            vola_opt::passes::AutoDiffPass::setup(opt).autodiff_all()?;
            opt.config.seen_pass_nodes.autodiff = false;
        }
        Ok(())
    }
}

///Cleans the graph from any non-export related data, removes unused context, folds it as good as possible and ensures that the
/// whole things is typed.
pub struct Cleanup;
impl Pass for Cleanup {
    fn execute(self, opt: &mut Optimizer) -> Result<(), PassError> {
        //make the exports _nice_
        vola_opt::passes::Cleanup::setup(opt)
            .remove_unused_toplevel_cvs()
            .cleanup_export_lmd()
            .cne_exports()
            .map_err(|e| VolaError::new(e))?;
        //and type them :)
        vola_opt::passes::TypeEdges::setup(opt).execute()?;

        Ok(())
    }
}
