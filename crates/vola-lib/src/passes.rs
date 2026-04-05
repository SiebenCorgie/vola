use std::{fmt::Debug, path::Path};

use vola_ast::{AstError, VolaAst};
use vola_common::{VolaError, reset_file_cache};
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

impl Into<Vec<VolaError<OptError>>> for PassError {
    fn into(self) -> Vec<VolaError<OptError>> {
        self.errors
    }
}

///Lowerst the AST into the optimizer
pub struct LowerAst(pub VolaAst);

impl LowerAst {
    pub fn from_file(file: &dyn AsRef<Path>) -> Result<Self, Vec<VolaError<AstError>>> {
        //NOTE: Always reset file cache, since the files we are reporting on might have changed.
        reset_file_cache();
        let parser = vola_tree_sitter_parser::VolaTreeSitterParser;
        let ast = VolaAst::new_from_file(file, &parser)?;
        Ok(Self(ast))
    }

    pub fn from_str(
        str: &str,
        workspace: impl AsRef<Path>,
    ) -> Result<Self, Vec<VolaError<AstError>>> {
        Self::from_bytes(str.as_bytes(), workspace)
    }

    pub fn from_bytes(
        bytes: &[u8],
        workspace: impl AsRef<Path>,
    ) -> Result<Self, Vec<VolaError<AstError>>> {
        //NOTE: Always reset file cache, since the files we are reporting on might have changed.
        reset_file_cache();
        let parser = vola_tree_sitter_parser::VolaTreeSitterParser;
        let ast = VolaAst::new_from_bytes(bytes, &parser, workspace)?;

        Ok(Self(ast))
    }
}

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
///runst common-node-elimination on the whole graph
pub struct Cne;
impl Pass for Cne {
    fn execute(self, opt: &mut Optimizer) -> Result<(), PassError> {
        let _ = vola_opt::passes::Cleanup::setup(opt)
            .cne_exports()
            .map_err(|e| VolaError::new(e))?;
        Ok(())
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

///Fully Inlines all exported function. This effectively makes any live function also an exported function.
pub struct InlineExports;
impl Pass for InlineExports {
    fn execute(self, opt: &mut Optimizer) -> Result<(), PassError> {
        //NOTE: we have to cleanup unused edges, otherwise the inliner might confuse stuff
        let _ = vola_opt::passes::Cleanup::setup(opt).remove_unused_edges();
        vola_opt::passes::InlineExports::setup(opt)
            .execute()
            .map_err(|e| e.to_error::<OptError>())?;
        Ok(())
    }
}
